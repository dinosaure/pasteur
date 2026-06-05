(* Usage: ocaml extract_langs.ml highlight.min.js *)

let read_file path =
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let str = really_input_string ic len in
  close_in ic; str

let needle = "registerLanguage(\""

let langs str =
  let nl = String.length needle and sl = String.length str in
  let rec go idx acc =
    if idx > sl - nl then List.rev acc
    else if String.sub str idx nl = needle then
      let start = idx + nl in
      let stop = String.index_from str start '"' in
      let name = String.sub str start (stop - start) in
      go stop (name :: acc)
    else go (idx + 1) acc
  in
  go 0 []

let () =
  let str = read_file Sys.argv.(1) in
  let names = langs str in
  print_string "let languages = [ ";
  print_string (String.concat "; " (List.map (Printf.sprintf "%S") names));
  print_endline " ]"
