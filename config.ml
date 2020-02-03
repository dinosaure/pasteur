(* (c) Romain Calascibetta 2019 *)

open Mirage

let remote =
  let doc = Key.Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
  Key.(create "remote" Arg.(opt string "git://127.0.0.1/pasteur" doc))

let port =
  let doc = Key.Arg.info ~doc:"port of HTTP service" [ "p"; "port" ] in
  Key.(create "port" Arg.(opt int 4343 doc))

let random_len =
  let doc = Key.Arg.info ~doc:"Length of generated URI" [ "length" ] in
  Key.(create "random_length" Arg.(opt int 3 doc))

let pasteur =
  foreign "Unikernel.Make"
    ~keys:[ Key.abstract remote; Key.abstract port; Key.abstract random_len ]
    (random @-> console @-> pclock @-> kv_ro @-> stackv4 @-> resolver @-> conduit @-> job)

let stack = generic_stackv4 default_network
let conduit = conduit_direct stack
let resolver = resolver_dns stack
let console = console
let public = generic_kv_ro "public"

let packages =
  let multipart_form = "git+https://github.com/dinosaure/multipart_form.git" in
  let tuyau = "git+https://github.com/dinosaure/tuyau.git" in

  [ package "httpaf"
  ; package "uuidm"
  ; package "tyxml"
  ; package "irmin-mirage-git"

  ; package ~pin:tuyau ~sublibs:["mirage.tcp"] "tuyau"
  ; package ~pin:multipart_form "multipart_form" ]


let () =
  register "pasteur"
    ~packages
    [ pasteur $ default_random $ default_console $ default_posix_clock $ public $ stack $ resolver $ conduit ]
