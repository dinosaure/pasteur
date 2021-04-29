open Lwt.Infix

module Make
  (Random : Mirage_random.S)
  (Pclock : Mirage_clock.PCLOCK)
  (Time : Mirage_time.S)
  (Stack : Mirage_stack.V4V6) = struct
  module Certify = Dns_certify_mirage.Make(Random)(Pclock)(Time)(Stack)

  type configuration =
    { key : string
    ; port : int
    ; addr : Ipaddr.t
    ; seed : string option
    ; hostname : [ `host ] Domain_name.t }

  let provision_certificate stack cfg =
    Certify.retrieve_certificate stack ~dns_key:cfg.key
      ~hostname:cfg.hostname ?key_seed:cfg.seed cfg.addr cfg.port
    >|= Rresult.R.open_error_msg
    >|= Rresult.R.map (fun certchain -> `Single certchain)
end
