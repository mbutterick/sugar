#lang racket/base
(require sugar/define)
(require-via-wormhole "../typed/sugar/debug.rkt")

(provide+safe report report/line report/file
              report* report*/line report*/file
              report-apply repeat time-repeat time-repeat* compare)
