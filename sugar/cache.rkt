#lang racket/base
(require sugar/define)
(require-via-wormhole "../typed/sugar/cache.rkt")

(provide+safe [make-caching-proc (procedure? . -> . procedure?)]
              define/caching)
