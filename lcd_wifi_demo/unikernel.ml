open Lwt.Infix

external time : unit -> int64 = "caml_get_monotonic_time"


module Hello  
              (Stack: Mirage_stack_lwt.V4)
= struct
 
  type status = {
    width: int;
    height: int;
    depth: int;
    offset: int;
    buffer_offset: int;
    image_data: int;
    x: int;
    y: int;
    bmp_buffer: Cstruct.t;
    video_buffer: Cstruct.t;
  }

  let default_status = {
    width = -1;
    height = -1;
    depth = -1;
    offset = 0;
    x = 20;
    y = 20;
    buffer_offset = 0;
    image_data = 0x10;
    bmp_buffer = Cstruct.create 0;
    video_buffer = Cstruct.create 0;
  }

  type direction = Direction_None | Direction_Up | Direction_Down | Direction_Left | Direction_Right

  let direction = ref Direction_None 

  let rec handle_data buffer status =
    let buf_len = Cstruct.len buffer in
    let status, adv = match status.offset with 
      | 0x0A -> Printf.printf "Image data start = %d\n" (Int32.to_int (Cstruct.LE.get_uint32 buffer 0x0A)); {status with image_data = Int32.to_int (Cstruct.LE.get_uint32 buffer 0x0A)}, 1
      | 0x12 -> Printf.printf "Width = %d\n" (Cstruct.LE.get_uint16 buffer 0x12); {status with width = Cstruct.LE.get_uint16 buffer 0x12}, 1
      | 0x16 -> Printf.printf "Height = %d\n" (Cstruct.LE.get_uint16 buffer 0x16); {status with height = Cstruct.LE.get_uint16 buffer 0x16}, 1
      | 0x18 -> {status with depth = Cstruct.LE.get_uint16 buffer 0x18}, 1
      | 0x19 -> Printf.printf "Allocated buffer!\n"; {status with bmp_buffer = Cstruct.create (3 * status.width * status.height)}, 1
      | i when i >= status.buffer_offset + buf_len -> {status with buffer_offset = status.buffer_offset + buf_len}, 0
      | i when i >= status.image_data -> 
        let cval = Cstruct.get_uint8 buffer (i - status.buffer_offset) in
        Cstruct.set_uint8 status.bmp_buffer (i - status.image_data) cval;
        status, 1
      | _ -> status, 1
    in match adv with 
      | 0 -> {status with offset = status.buffer_offset}
      | n -> handle_data buffer {status with offset = status.offset + n}
  

  let fill_buffer status = 
    let status = {status with video_buffer = Cstruct.of_bigarray (Lcd.alloc_buffer (status.width*status.height))} in 
    for x = 0 to status.width-1 do 
      for y = 0 to status.height-1 do 
        let linear_index = (y*(4*((status.width+3)/4)) + x)*3 in 
        let b = (Cstruct.get_uint8 status.bmp_buffer linear_index) lsr 3
        and g = (Cstruct.get_uint8 status.bmp_buffer (linear_index+1)) lsr 2
        and r = (Cstruct.get_uint8 status.bmp_buffer (linear_index+2)) lsr 3
        in 
        let pixel_int16 = (r lsl 11) + (g lsl 5) + b in
        Cstruct.BE.set_uint16 status.video_buffer ((y*status.width +  status.width-1- x)*2) pixel_int16
      done;
    done;
    status

  let move_picture status = match !direction with 
    | Direction_None -> status
    | Direction_Up -> {status with y = min (Lcd.height - status.height) (status.y + 2)}
    | Direction_Down -> {status with y = max 0 (status.y - 2)}
    | Direction_Left -> {status with x = min (Lcd.width - status.width) (status.x + 2)}
    | Direction_Right -> {status with x = max 0 (status.x - 2)} 

  let refresh_image status = 
    let status = fill_buffer status in
    let rec refresh status =
      let status = move_picture status in
      Lcd.transmit_buffer status.video_buffer status.x status.y status.width status.height; 
      OS.Time.sleep_ns @@ Duration.of_ms 10 >>= (fun _ -> refresh status)
    in
    refresh status
    
    

  let rec read_data flow status = function 
    | Ok `Eof -> Logs.info (fun f -> f "End of file."); refresh_image status
    | Ok (`Data buffer) -> Stack.TCPV4.read flow >>= read_data flow (handle_data buffer status)
    | Error _ -> Logs.info (fun f -> f "Connection halted during data transmission."); Lwt.return_unit

  let rec parse_buffer buf max_index n_read_before = function
    | n when n == max_index -> None
    | n when Cstruct.get_char buf n == '\n' ->
        if n_read_before == 2 then
          Some (Cstruct.sub buf (n+1) (max_index-n-1))
        else
          parse_buffer buf max_index 1 (n+1)
    | n -> parse_buffer buf max_index (n_read_before+1) (n+1)

  let rec parse_header flow = function 
    | Ok `Eof -> Logs.info (fun f -> f "End of file before reading header."); Lwt.return_unit
    | Ok (`Data buffer) -> 
      begin
        match parse_buffer buffer (Cstruct.len buffer) 0 0 with 
          | None -> Stack.TCPV4.read flow >>= parse_header flow
          | Some data_buffer -> read_data flow default_status (Ok (`Data data_buffer))
      end
    | Error _ -> Logs.info (fun f -> f "Connection halted."); Lwt.return_unit

  let check_answer flow = function 
    | Ok () -> Stack.TCPV4.read flow >>= parse_header flow
    | Error _ -> Logs.info (fun f -> f "Connection halted after write."); Lwt.return_unit

  let write_request flow =
    let message = "GET /ocaml.bmp HTTP/1.0\r\n"^
                  "Host: 192.168.43.65:8000\r\n"^
                  "User-Agent: esp-idf/1.0 esp32\r\n\r\n"
    in 
      Stack.TCPV4.write flow (Cstruct.of_string message) >>= check_answer flow

  let handle_connection = function
    | Ok flow -> write_request flow
    | Error _ -> Logs.info (fun f -> f "Connection failed."); Lwt.return_unit


  let rec handle_client_input flow = function 
    | Ok `Eof -> Logs.info (fun f -> f "Closing connection!"); Lwt.return_unit
    | Error e -> Logs.warn (fun f -> f "Error reading data from established connection: %a" Stack.TCPV4.pp_error e); Lwt.return_unit
    | Ok (`Data b) ->
      begin
        for i = 0 to (Cstruct.len b) - 1 do 
          match Cstruct.get_char b i with
          | 'z' ->  direction := Direction_Up
          | 'q' ->  direction := Direction_Left
          | 's' ->  direction := Direction_Down
          | 'd' ->  direction := Direction_Right
          | ' ' ->  direction := Direction_None
          | _ ->    ()
        done;
        Stack.TCPV4.read flow >>= handle_client_input flow
      end
  let this_is_a_server flow = 
    let dst, dst_port = Stack.TCPV4.dst flow in
      Logs.info (fun f -> f "new tcp connection from IP %s on port %d"
                (Ipaddr.V4.to_string dst) dst_port);
      Stack.TCPV4.read flow >>= handle_client_input flow

  let start stack = 
    (* Start wifi driver and initialize station *)
    let _ = Wifi.start () in
    OS.Event.wait_for_event (Wifi.id_of_event Wifi.STA_started) 
    >>= fun _ -> 
    (* Input config and wait for connection. *)
    let _ = Wifi.sta_set_config {
      ssid=Bytes.of_string "not a wifi";
      password=Bytes.of_string "not a password";
    } in
    let _ = Wifi.connect () in
    OS.Time.sleep_ns @@ Duration.of_sec 8 >>= fun _ ->
    let ip = Ipaddr.V4.of_string_exn "192.168.43.65" 
    and port = 8000 
    in
    Stack.listen_tcpv4 stack 8000 this_is_a_server;
    let image_downloader = Stack.TCPV4.create_connection (Stack.tcpv4 stack) (ip, port) >>= handle_connection 
    and command_server = Stack.listen stack in
    Lwt.join [image_downloader; command_server]
  
end 
