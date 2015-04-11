#lang racket/base
(require sugar/define)
(require-via-wormhole "../typed/sugar/debug.rkt")

(provide+safe report report-apply report* repeat time-repeat time-repeat* compare)