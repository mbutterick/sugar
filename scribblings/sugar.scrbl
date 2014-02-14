#lang scribble/manual

@(require scribble/eval (for-label racket "../main.rkt"))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar))


@title{Sugar}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

A collection of little functions that help make Racket code more readable.

@section{Installation & updates}

At the command line:
@verbatim{raco pkg install sugar}

After that, you can update the package from the command line:
@verbatim{raco pkg update sugar}


@section{Interface}

@defmodule[sugar]

Hello sugar.


@section{License & source code}

This module is licensed under the LGPL.

Source repository at @link["http://github.com/mbutterick/sugar"]{http://github.com/mbutterick/sugar}. Suggestions & corrections welcome.

