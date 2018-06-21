open Mirage

let main =
  foreign
    ~packages:[package "duration"; package "lcd"; package "wifi"]
    "Unikernel.Hello" (stackv4 @-> job)


let () =
  register "hello" [main $ dyn_dhcp_ipv4_stack (netif "sta")]
