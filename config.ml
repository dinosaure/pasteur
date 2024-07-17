(* (c) Romain Calascibetta 2019 *)

open Mirage

let setup = runtime_arg ~pos:__POS__ "Unikernel.K.setup"

let ssh_key =
  Runtime_arg.create ~pos:__POS__
    {|let open Cmdliner in
      let doc = Arg.info ~doc:"The private SSH key (rsa:<seed> or ed25519:<b64-key>)." ["ssh-key"] in
      Arg.(value & opt (some string) None doc)|}

let ssh_authenticator =
  Runtime_arg.create ~pos:__POS__
    {|let open Cmdliner in
      let doc = Arg.info ~doc:"SSH authenticator." ["ssh-auth"] in
      Arg.(value & opt (some string) None doc)|}

let ssh_password =
  Runtime_arg.create ~pos:__POS__
    {|let open Cmdliner in
      let doc = Arg.info ~doc:"The private SSH password." [ "ssh-password" ] in
      Arg.(value & opt (some string) None doc)|}

let pasteur_js =
  let dune _info =
    [
      Dune.stanzaf
        {dune|(subdir public
 (rule
  (enabled_if (= %%{context_name} "default"))
  (deps ../js/pasteur_js.bc.js)
  (target pasteur.js)
  (action (copy %%{deps} %%{target}))))|dune}
    ]
  in
  impl ~dune "Pasteur_js" job

let pasteur_hljs =
  let dune _info =
    [
      Dune.stanzaf
        {dune|(rule
 (enabled_if (= %%{context_name} "default"))
 (mode promote)
 (deps js/pasteur_hljs.bc.js public/highlight.js)
 (target language.default.ml)
 (action (with-stdout-to
  %%{target} (bash "node js/pasteur_hljs.bc.js"))))|dune}
    ; Dune.stanzaf
        {dune|(rule
 (target language.ml)
 (deps %%{exe:language.default.ml})
 (action (copy %%{exe:language.default.ml} %%{target})))|dune}
    ]
  in
  let files _ = [ Fpath.v "language.ml" ] in
  impl ~files ~dune "Pasteur_hljs" job

let packages =
  [
    package "paf" ~min:"0.5.0"
  ; package "paf" ~sublibs:[ "mirage" ] ~min:"0.5.0"
  ; package "letsencrypt-mirage"
  ; package "uuidm"
  ; package "tyxml"
  ; package "git-kv" ~min:"0.0.3"
  ; package "multipart_form-lwt"
  ; package "json-data-encoding"
  ; package "data-encoding"
  ; package "ezjsonm"
  ; package "brr" ~build:true ~scope:`Switch
  ]

let pasteur =
  main "Unikernel.Make"
    ~packages
    ~runtime_args:[ setup ]
    ~deps:[ dep pasteur_js; dep pasteur_hljs ]
    (random @-> time @-> mclock @-> pclock @-> kv_ro @-> stackv4v6
     @-> alpn_client @-> git_client @-> job)

let stack = generic_stackv4v6 default_network
let tcp = tcpv4v6_of_stackv4v6 stack
let he = generic_happy_eyeballs stack
let dns = generic_dns_client stack he

let git =
  let git = mimic_happy_eyeballs stack he dns in
  git_ssh ~key:ssh_key ~password:ssh_password ~authenticator:ssh_authenticator tcp git

let http_client =
  let dns = mimic_happy_eyeballs stack he dns in
  paf_client tcp dns

let public = docteur ~extra_deps:[ "public/pasteur.js" ] "relativize://public/"

let () =
  register "pasteur"
    [ pasteur
      $ default_random
      $ default_time
      $ default_monotonic_clock
      $ default_posix_clock
      $ public
      $ stack
      $ http_client
      $ git
    ]
