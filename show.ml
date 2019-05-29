open Tyxml.Html

let css_href = Xml.uri_of_string "/pastisserie.css"
let highlight_js_href = Xml.uri_of_string "/highlight.pack.js"
let highlight_css_href = Xml.uri_of_string "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.15.6/styles/default.min.css"

let html ~code:code_class code_contents =
  html
    (head (title (txt "Past-isserie"))
       [ meta ~a:[ a_http_equiv "Content-Type"; a_content "text/html; charset=utf-8;" ] ()
       ; link ~rel:[ `Stylesheet ] ~href:css_href ()
       ; link ~rel:[ `Stylesheet ] ~href:highlight_css_href ()
       ; script ~a:[ a_src highlight_js_href ] (txt "")
       ; script (txt "hljs.initHighlightingOnLoad();") ])
    (body [ pre [ code ~a:[ a_class [ code_class] ] [ txt code_contents ] ]])
