#lang racket/base
;; isolate typed requires in a helper file so the untyped versions can be substituted on the untyped side
(require net/url)