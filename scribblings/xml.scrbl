#lang scribble/manual

@(require scribble/eval (for-label racket sugar xml))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar))

@title{XML}
@defmodule[#:multi (sugar/xml (submod sugar/xml safe))]


Making it easier to do the simplest kind of round-trip with XML: convert an XML string to X-expressions, manipulate, and then convert these X-expressions back to an XML string. @bold{This submodule is untyped only.}

@defproc[
(xml-string->xexprs
[xml-string string?])
(values xexpr? xexpr?)]
Take a string containg XML and break it into two X-expressions: one representing the prolog of the document, and the other representing everything under the root node. Your @racket[_xml-string] must have a root node, but it doesn't need a prolog.

@examples[#:eval my-eval
(define str "<?xml encoding=\"utf-8\"?>\n<root>hello</root>")
(xml-string->xexprs str)
(define root-only "<root>hello</root>")
(xml-string->xexprs root-only)
(define prolog-only "<?xml encoding=\"utf-8\"?>")
(xml-string->xexprs prolog-only)
]


@defproc[
(xexprs->xml-string
[prolog-xexpr xexpr?]
[root-xexpr xexpr?])
string?]
Take two X-expressions representing the prolog and root of an XML document and join them back into an XML string. In other words, the inverse of the function above.

@examples[#:eval my-eval
(define str "<?xml encoding=\"utf-8\"?>\n<root>hello</root>")
(define-values (prolog doc) (xml-string->xexprs str))
prolog
doc
(xexprs->xml-string prolog doc)]

