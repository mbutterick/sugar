#lang racket/base
(require sugar/define racket/set sugar/coerce)
(require-via-wormhole "../typed/sugar/misc.rkt")

(provide+safe [bytecount->string (integer? . -> . string?)])