open Tyxml.Html

let css_href = Xml.uri_of_string "/pastisserie.css"
let highlight_js_href = Xml.uri_of_string "/highlight.pack.js"
let highlight_css_href = Xml.uri_of_string "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.6/styles/default.min.css"
let sjcl_js_href = Xml.uri_of_string "/sjcl.js"
let pasteur_js_href = Xml.uri_of_string "/pasteur.js"

let html ?code:code_class code_contents =
  let a = match code_class with
    | Some code_class -> [ a_id "output"; a_class [ code_class ] ]
    | None -> [ a_id "output"; a_class [ "nohighlight" ] ] in
  html
    (head (title (txt "Past-isserie"))
       [ meta ~a:[ a_http_equiv "Content-Type"; a_content "text/html; charset=utf-8;" ] ()
       ; link ~rel:[ `Stylesheet ] ~href:css_href ()
       ; link ~rel:[ `Stylesheet ] ~href:highlight_css_href ()
       ; script ~a:[ a_src highlight_js_href ] (txt "")
       ; script ~a:[ a_src sjcl_js_href ] (txt "")
       ; script ~a:[ a_src pasteur_js_href ] (txt "") ])
    (body [ div ~a:[ a_id "raw" ] [ txt code_contents ]
          ; pre [ code ~a [ txt "" ] ]
          ; script (txt "window.onload = function () { doDecrypt(); hljs.configure({ languages: [] }); hljs.initHighlighting(); }") ])
