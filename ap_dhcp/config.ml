open Mirage

let main =
  foreign
    ~packages:[
        package ~min:"0.5" ~sublibs:["server"; "wire"] "charrua-core";
        package ~sublibs:["ethif"; "arpv4"] "tcpip"
    ]
    "Unikernel.Hello" (network @-> mclock @-> time @-> job)

let () =
  register "hello" [main $ netif "ap" $ default_monotonic_clock $ default_time ]
