module Make
  (Random : Mirage_random.S)
  (Pclock : Mirage_clock.PCLOCK)
  (Time : Mirage_time.S)
  (Stack : Mirage_stack.V4V6) : sig
  type configuration =
    { key : string
    ; port : int
    ; addr : Ipaddr.t
    ; seed : string option
    ; hostname : [ `host ] Domain_name.t }

  val provision_certificate :
    Stack.t ->
    configuration ->
    (Tls.Config.own_cert, [> `Msg of string ]) result Lwt.t
end
