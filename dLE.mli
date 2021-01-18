module Make
  (Random : Mirage_random.S)
  (Pclock : Mirage_clock.PCLOCK)
  (Time : Mirage_time.S)
  (StackV4 : Mirage_stack.V4) : sig
  type configuration =
    { key : string
    ; port : int
    ; addr : Ipaddr.V4.t
    ; seed : string option
    ; hostname : [ `host ] Domain_name.t }

  val provision_certificate :
    StackV4.t ->
    configuration ->
    (Tls.Config.own_cert, [> `Msg of string ]) result Lwt.t
end
