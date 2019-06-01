(* (c) Romain Calascibetta 2019 *)

open Mirage

let remote =
  let doc = Key.Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
  Key.(create "remote" Arg.(opt string "git://127.0.0.1/pasteur" doc))

let pasteur =
  foreign "Unikernel.Make"
    ~keys:[ Key.abstract remote ]
    (random @-> console @-> pclock @-> kv_ro @-> resolver @-> conduit @-> http @-> job)

let stack = generic_stackv4 default_network
let conduit = conduit_direct stack
let resolver = resolver_dns stack
let app = httpaf_server conduit
let console = console
let public = generic_kv_ro "public"

let packages =
  let irmin_pin = "git+https://github.com/mirage/irmin.git" in
  let git_pin = "git+https://github.com/mirage/ocaml-git.git" in
  let multipart_form = "git+https://github.com/dinosaure/multipart_form.git" in
  [ package ~pin:"git+https://github.com/mirage/mirage-http.git#wip" "mirage-http"
  ; package ~pin:"git+https://github.com/anmonteiro/httpaf.git#mirage" "httpaf"
  ; package ~pin:"git+https://github.com/anmonteiro/httpaf.git#mirage" "httpaf-mirage"
  ; package ~pin:"git+https://github.com/anmonteiro/httpaf.git#mirage" "httpaf-lwt"
  ; package ~pin:"git+https://github.com/mirage/encore.git" "encore"
  ; package ~pin:"git+https://github.com/mirage/ke.git" "ke"
  ; package ~pin:"git+https://github.com/mirage/pecu.git" "pecu"
  ; package ~pin:git_pin "git"
  ; package ~pin:git_pin "git-http"
  ; package ~pin:git_pin "git-mirage"
  ; package ~pin:irmin_pin "irmin"
  ; package ~pin:irmin_pin "irmin-mem"
  ; package ~pin:irmin_pin "irmin-git"
  ; package ~pin:irmin_pin "irmin-mirage"
  ; package ~pin:irmin_pin "irmin-mirage-git"
  ; package ~pin:multipart_form "multipart_form"
  ; package "uuidm"
  ; package "tyxml"
  ; package ~sublibs:["c"] "checkseum"
  ; package ~sublibs:["c"] "digestif" ]

let () =
  register "pasteur"
    ~packages
    [ pasteur $ default_random $ default_console $ default_posix_clock $ public $ resolver $ conduit $ app ]
