#lang racket/base
(require sugar/define racket/set sugar/coerce)
(require-via-wormhole "../typed/sugar/string.rkt")

(provide+safe [starts-with? (coerce/string? coerce/string? . -> . coerce/boolean?)]
              [ends-with? (coerce/string? coerce/string? . -> . coerce/boolean?)]
              [capitalized? (coerce/string? . -> . coerce/boolean?)]) 