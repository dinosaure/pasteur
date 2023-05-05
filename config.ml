(* (c) Romain Calascibetta 2019 *)

open Mirage

let remote =
  let doc = Key.Arg.info ~doc:"Remote Git repository." [ "r"; "remote" ] in
  Key.(create "remote" Arg.(required string doc))

let port =
  let doc = Key.Arg.info ~doc:"port of HTTP service." [ "p"; "port" ] in
  Key.(create "port" Arg.(opt (some int) None doc))

let email =
  let doc = Key.Arg.info ~doc:"Let's encrypt email." [ "email" ] in
  Key.(create "email" Arg.(opt (some string) None doc))

let hostname =
  let doc = Key.Arg.info ~doc:"Hostname of the unikernel." [ "hostname" ] in
  Key.(create "hostname" Arg.(opt (some string) None doc))

let random_len =
  let doc = Key.Arg.info ~doc:"Length of generated URI." [ "length" ] in
  Key.(create "random-length" Arg.(opt int 3 doc))

let cert_seed =
  let doc = Key.Arg.info ~doc:"Let's encrypt certificate seed." [ "cert-seed" ] in
  Key.(create "cert-seed" Arg.(opt (some string) None doc))

let cert_key_type =
  let doc = Key.Arg.info ~doc:"certificate key type" [ "cert-key-type" ] in
  Key.(create "cert-key-type" Arg.(opt string "RSA" doc))

let cert_bits =
  let doc = Key.Arg.info ~doc:"certificate public key bits" [ "cert-bits" ] in
  Key.(create "cert-bits" Arg.(opt int 4096 doc))

let account_seed =
  let doc = Key.Arg.info ~doc:"Let's encrypt account seed." [ "account-seed" ] in
  Key.(create "account-seed" Arg.(opt (some string) None doc))

let account_key_type =
  let doc = Key.Arg.info ~doc:"account key type" [ "account-key-type" ] in
  Key.(create "account-key-type" Arg.(opt string "RSA" doc))

let account_bits =
  let doc = Key.Arg.info ~doc:"account public key bits" [ "account-bits" ] in
  Key.(create "account-bits" Arg.(opt int 4096 doc))

let production =
  let doc = Key.Arg.info ~doc:"Let's encrypt production environment." [ "production" ] in
  Key.(create "production" Arg.(opt bool false doc))

let https =
  let doc = Key.Arg.info ~doc:"Start an HTTP server with a TLS certificate." [ "https" ] in
  Key.(create "https" Arg.(opt bool false doc))

let ssh_key =
  let doc = Key.Arg.info ~doc:"Private ssh key (rsa:<seed> or ed25519:<b64-key>)." ["ssh-key"] in
  Key.(create "ssh-key" Arg.(opt (some string) None doc))

let ssh_password =
  let doc = Key.Arg.info ~doc:"The private SSH password." [ "ssh-password" ] in
  Key.(create "ssh-password" Arg.(opt (some string) None doc))

let ssh_authenticator =
  let doc = Key.Arg.info ~doc:"SSH host key authenticator." [ "ssh-authenticator" ] in
  Key.(create "ssh_authenticator" Arg.(opt (some string) None doc))

let tls_authenticator =
  let doc = Key.Arg.info ~doc:"TLS host authenticator." [ "tls-authenticator" ] in
  Key.(create "https_authenticator" Arg.(opt (some string) None doc))

let pasteur_js =
  let dune _info =
    [ Dune.stanzaf {dune|(subdir public
 (rule
  (enabled_if (= %%{context_name} "default"))
  (deps ../js/pasteur_js.bc.js)
  (target pasteur.js)
  (action (copy %%{deps} %%{target}))))|dune} ] in
  impl ~dune "Pasteur_js" job

let pasteur_hljs =
  let dune _info =
    [ Dune.stanzaf {dune|(rule
 (enabled_if (= %%{context_name} "default"))
 (mode promote)
 (deps js/pasteur_hljs.bc.js public/highlight.js)
 (target language.default.ml)
 (action (with-stdout-to
  %%{target} (bash "node js/pasteur_hljs.bc.js"))))|dune}
    ; Dune.stanzaf {dune|(rule
 (target language.ml)
 (deps %%{exe:language.default.ml})
 (action (copy %%{exe:language.default.ml} %%{target})))|dune} ] in
  let files _ = [ Fpath.v "language.ml" ] in
  impl ~files ~dune "Pasteur_hljs" job

let pasteur =
  foreign "Unikernel.Make"
    ~deps:[ dep pasteur_js; dep pasteur_hljs ]
    ~packages:[ package "brr" ~build:true ~scope:`Switch ]
    ~keys:[ Key.v remote
          ; Key.v port
          ; Key.v https
          ; Key.v email
          ; Key.v hostname
          ; Key.v random_len
          ; Key.v cert_seed
          ; Key.v cert_key_type
          ; Key.v cert_bits
          ; Key.v account_seed
          ; Key.v account_key_type
          ; Key.v account_bits
          ; Key.v production ]
    (random @-> console @-> time @-> mclock @-> pclock @-> kv_ro @-> stackv4v6 @-> dns_client @-> git_client @-> job)

let stack = generic_stackv4v6 default_network
let dns = generic_dns_client stack

let git =
  let git = mimic_happy_eyeballs stack dns (generic_happy_eyeballs stack dns) in
  let tcp = tcpv4v6_of_stackv4v6 stack in
  merge_git_clients (git_tcp tcp git)
    (merge_git_clients (git_ssh ~key:ssh_key ~password:ssh_password ~authenticator:ssh_authenticator tcp git)
                       (git_http ~authenticator:tls_authenticator tcp git))

let public = docteur ~extra_deps:[ "public/pasteur.js" ] "relativize://public/"

let packages =
  [ package "paf" ~min:"0.0.9"
  ; package "paf" ~sublibs:[ "mirage" ] ~min:"0.0.9"
  ; package "paf-le" ~min:"0.0.9"
  ; package "uuidm"
  ; package "tyxml"
  ; package "irmin-mirage-git"
  ; package "multipart_form-lwt"
  ; package "json-data-encoding"
  ; package "ezjsonm" ]

  let () =
  register "pasteur"
    ~packages
    [ pasteur $ default_random $ default_console $ default_time $ default_monotonic_clock $ default_posix_clock $ public
      $ stack $ dns $ git ]
