open Tyxml.Html

let checkbox ~name ?label:(contents= [ txt name ]) ?(value= "on") ?(checked= false) () =
  let checked = if checked then [ a_checked () ] else [] in
  label
    (input ~a:([ a_input_type `Checkbox
               ; a_name name
               ; a_value value ] @ checked) ()
     :: contents)

let post_href = Xml.uri_of_string "/"
let css_href = Xml.uri_of_string "/pastisserie.css"
let sjcl_js_href = Xml.uri_of_string "/sjcl.js"
let pasteur_js_href = Xml.uri_of_string "/pasteur.js"

let options =
  let ln = checkbox ~name:"ln" ~label:[ txt "Line numbers" ] () in
  let raw = checkbox ~name:"raw" ~label:[ txt "Raw paste" ] () in
  [ ln; raw; br () ]

let language lst =
  let lang_id_of_lang = function
    | None -> "__no_highlighting__"
    | Some lang -> lang in
  let fn (name, lang) =
    option ~a:[ a_value (lang_id_of_lang lang) ]
      (txt name) in
  [ select ~a:[ a_name "hl" ] (List.map fn lst); br () ]

let name_field =
  [ label ~a:[ a_label_for "user" ] [ txt "User (optional):"; ]
  ; br ()
  ; input ~a:[ a_input_type `Text; a_name "user"; a_id "user" ] ()
  ; br () ]

let comment_field =
  [ label ~a:[ a_label_for "comment" ] [ txt "Comment (optional):"; ]
  ; br ()
  ; input ~a:[ a_input_type `Text; a_name "comment"; a_id "comment" ] ()
  ; br () ]

let form lst =
  form ~a:[ a_id "pasteur" ]
    ([ input ~a:[ a_input_type `Text; a_name "content"; a_style "display: none;" ] ()
     ; textarea ~a:[ a_name "paste"; a_rows 20; a_cols 80 ] (txt "")
     ; br () ]
     @ language lst
     @ options
     @ name_field
     @ comment_field
     @ [ input ~a:[ a_input_type `Button; a_onclick "doEncrypt();"; a_value "Paste!" ] () ])

let html ~title:title_contents ~documentation languages =
  html
    (head (title (txt title_contents))
       [ meta ~a:[ a_http_equiv "Content-Type"; a_content "text/html; charset=utf-8;" ] ()
       ; script ~a:[ a_src sjcl_js_href ] (txt "")
       ; script ~a:[ a_src pasteur_js_href ] (txt "")
       ; link ~rel:[ `Stylesheet ] ~href:css_href () ])
    (body [ h1 [ txt title_contents
               ; space ()
               ; span ~a:[ a_style "font-size: 12px;" ] [ txt documentation ] ]
          ; form languages ])
