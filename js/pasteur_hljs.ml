open Js_of_ocaml

let hljs = Js.Unsafe.js_expr {js|require("../public/highlight.js")|js}
let lst = Jv.call hljs "listLanguages" [||] |> Jv.to_list Jv.to_string

let ocamlify s =
  let s = String.capitalize_ascii s in
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> Buffer.add_char b c
      | '-' | '.' -> Buffer.add_char b '_'
      | _ -> ())
    s;
  let s' = Buffer.contents b in
  if String.length s' = 0 || ('0' <= s'.[0] && s'.[0] <= '9') then
    raise (Invalid_argument s);
  s'

let branch_to_string name = Fmt.str {ocaml|%s -> %S|ocaml} (ocamlify name) name

let branch_of_string = function
  | `Valid name -> Fmt.str {ocaml|%S -> %s|ocaml} name (ocamlify name)
  | `Invalid ->
      Fmt.str {ocaml|v -> @[<2>Fmt.failwith "invalid language: %%s" v|ocaml}

let output ppf lst =
  Fmt.pf ppf "type t =@\n @[<v>%a@]@\n"
    Fmt.(list ~sep:(any "@ | ") (using ocamlify string))
    lst;
  Fmt.pf ppf "let all =@\n @[<v>%a@]@\n"
    Fmt.(Dump.list (using ocamlify string))
    lst;
  Fmt.pf ppf "let to_string = function@\n @[<v>%a@]@\n"
    Fmt.(list ~sep:(any "@ | ") (using branch_to_string string))
    lst;
  Fmt.pf ppf "let of_string = function@\n @[<v>%a@]@\n"
    Fmt.(list ~sep:(any "@ | ") (using branch_of_string string))
    (List.map (fun x -> `Valid x) lst @ [ `Invalid ])

let () = output Fmt.stdout lst
