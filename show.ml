open Tyxml.Html

let css_href = Xml.uri_of_string "/pastisserie.css"
let highlight_js_href = Xml.uri_of_string "/highlight.js"
let pasteur_js_href = Xml.uri_of_string "/script.js"
let highlight_css_href = Xml.uri_of_string "/highlight.css"

let html ~title:title_contents ?code:code_class ?(encrypted = true)
    ?(ln = false) code_contents =
  let a =
    match code_class with
    | Some code_class -> [ a_id "output"; a_class [ code_class ] ]
    | None -> [ a_id "output"; a_class [ "nohighlight" ] ]
  in
  let ln_call = if ln then "doLineNumbers(); " else "" in
  let onload =
    Fmt.str
      "window.onload = function () { doShow().then(function (n) { \
       document.querySelectorAll('pre code').forEach(function (block) { \
       hljs.highlightBlock(block); }); %s}).catch(function (err) { \
       console.error('doShow failed:', err); }); }"
      ln_call
  in
  html
    (head
       (title (txt title_contents))
       [
         meta
           ~a:
             [
               a_http_equiv "Content-Type"
             ; a_content "text/html; charset=utf-8;"
             ]
           (); link ~rel:[ `Stylesheet ] ~href:css_href ()
       ; link ~rel:[ `Stylesheet ] ~href:highlight_css_href ()
       ; script ~a:[ a_src highlight_js_href ] (txt "")
       ; script ~a:[ a_src pasteur_js_href ] (txt "")
       ])
    (body
       ~a:[ a_class [ "show" ] ]
       [
         div ~a:[ a_id "raw" ] [ txt code_contents ]; pre [ code ~a [ txt "" ] ]
       ; div
           ~a:[ a_id "encrypted"; a_style "display: none;" ]
           [ txt (if encrypted then "on" else "off") ]; script (txt onload)
       ])
