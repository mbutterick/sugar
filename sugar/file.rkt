#lang racket/base
(require sugar/define racket/set sugar/coerce)
(require-via-wormhole "../typed/sugar/file.rkt")

(provide+safe
 [get-enclosing-dir (coerce/path? . -> . path?)]
 [has-ext? (coerce/path? coerce/string? . -> . coerce/boolean?)]
 [get-ext (coerce/path? . -> . (or/c #f string?))]
 binary-extensions
 [has-binary-ext? (coerce/path? . -> . coerce/boolean?)]
 [add-ext (coerce/string? coerce/string? . -> . coerce/path?)]
 [remove-ext (coerce/path? . -> . path?)]
 [remove-ext* (coerce/path? . -> . path?)])
 