#lang typed/racket/base
;; isolate typed requires in a helper file so the untyped versions can be substituted on the untyped side
(require/typed racket/list [dropf (All (A) (Listof A) (A -> Boolean) -> (Listof A))]
               [dropf-right (All (A) (Listof A) (A -> Boolean) -> (Listof A))])