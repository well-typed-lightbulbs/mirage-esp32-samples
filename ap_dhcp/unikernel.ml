open Lwt.Infix
open Wifi
open Mirage_types_lwt


module Hello (Netif: NETWORK)(MClock: Mirage_types.MCLOCK)(Time: TIME)
= struct 
    module Eth = Ethif.Make(Netif)
    module Arp = Arpv4.Make(Eth)(MClock)(Time)
    module DC = Dhcp_config

    let string_of_status () =
        let status = Wifi.get_status () in
        let values = [status.inited; status.ap_started; status.sta_started; status.sta_connected] in 
        let values_str = List.map (fun x -> if x then "T" else "F") values in
        "Wifi status: "^String.concat " " values_str 
 
    
    let of_interest dest net =
        Macaddr.compare dest (Netif.mac net) = 0 || not (Macaddr.is_unicast dest)

    let input_dhcp clock net config leases buf =
        match Dhcp_wire.pkt_of_buf buf (Cstruct.len buf) with
        | Error e ->
            Logs.info (fun f -> f "Can't parse packet: %s" e);
            Lwt.return leases
        | Ok pkt ->
            let open Dhcp_server.Input in
            let now = MClock.elapsed_ns clock |> Duration.to_sec |> Int32.of_int in
            match input_pkt config leases pkt now with
        | Silence -> Lwt.return leases
        | Update leases ->
            Logs.info (fun f -> f "Received packet %s - updated lease database" (Dhcp_wire.pkt_to_string pkt));
            Lwt.return leases
        | Warning w ->
            Logs.info (fun f -> f "%s" w);
            Lwt.return leases
        | Dhcp_server.Input.Error e ->
            Logs.info (fun f -> f "%s" e);
            Lwt.return leases
        | Reply (reply, leases) ->
            Logs.info (fun f -> f "Received packet %s" (Dhcp_wire.pkt_to_string pkt));
            Netif.write net (Dhcp_wire.buf_of_pkt reply) >>= fun _ ->
            Logs.info (fun f -> f "Sent reply packet %s" (Dhcp_wire.pkt_to_string reply));
        Lwt.return leases

    let start_dhcp net clock time = 
        Eth.connect net >>= fun e ->
        Arp.connect e clock >>= fun a ->
        Arp.add_ip a DC.ip_address >>= fun () ->

        (* Build a dhcp server *)
        let config = Dhcp_server.Config.make
            ~hostname:DC.hostname
            ~default_lease_time:DC.default_lease_time
            ~max_lease_time:DC.max_lease_time
            ~hosts:DC.hosts
            ~addr_tuple:(DC.ip_address, Netif.mac net)
            ~network:DC.network
            ~range:DC.range
            ~options:DC.options
        in
        let leases = ref (Dhcp_server.Lease.make_db ()) in
        let listener = Netif.listen net (fun buf ->
            match Ethif_packet.Unmarshal.of_cstruct buf with
            | Result.Error s ->
                Logs.info (fun f -> f "Can't parse packet: %s" s); Lwt.return_unit
            | Result.Ok (ethif_header, ethif_payload) ->
                if of_interest ethif_header.Ethif_packet.destination net &&
                    Dhcp_wire.is_dhcp buf (Cstruct.len buf) then begin
                    input_dhcp clock net config !leases buf >>= fun new_leases ->
                    leases := new_leases;
                    Lwt.return_unit
                end else if ethif_header.Ethif_packet.ethertype = Ethif_wire.ARP then
                    Arp.input a ethif_payload
                else Lwt.return_unit
        ) in
        listener

    let start netif_ap clock time =
        Logs.info (fun f -> f "%s" (string_of_status ()));
        let _ = Wifi.start () in
        OS.Event.wait_for_event (Wifi.id_of_event Wifi.AP_started) >>= fun _ ->
        let _ = match Wifi.ap_set_config {
            ssid = Bytes.of_string "oui";
            password = Bytes.of_string "";
            channel = 1;
            auth_mode = Wifi.AUTH_OPEN;
            ssid_hidden = false;
            max_connection = 4;
            beacon_interval = 100;
        } with 
        | Error _ -> Logs.info (fun f -> f "AP_set_config failed")
        | Ok _ -> Logs.info (fun f -> f "AP_set_config succeeded")
        in
        start_dhcp netif_ap clock time
  
end 
