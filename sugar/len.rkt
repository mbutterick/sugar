#lang racket/base
(require sugar/define racket/set)
(require-via-wormhole "../typed/sugar/len.rkt")

(provide+safe [len ((or/c list? vector? set? sequence? string? symbol? path? hash?) . -> . integer?)])
