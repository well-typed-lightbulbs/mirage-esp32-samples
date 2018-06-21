open Lwt.Infix

module Hello (Time : Mirage_time_lwt.S) = struct

  let start _time =

    let rec loop i = function
      | 0 -> Lwt.return_unit
      | n ->
        Logs.info (fun f -> f "Hello!");
        Time.sleep_ns (Duration.of_sec 1) >>= fun () ->
        loop (i+1) (n-1)
    in
    loop 0 20

end
