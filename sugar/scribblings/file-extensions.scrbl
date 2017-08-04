#lang scribble/manual

@(require scribble/eval (for-label racket sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require sugar))

@title{File extensions}
@defmodule[#:multi (sugar/file (submod sugar/file safe))]

These functions don't access the filesystem. @bold{Warning}: these functions adopt the simplifying assumption that the paths are encoded as ASCII or UTF-8. A fully precise treatment of paths would need to handle them as byte strings. If you need that, see the functions in @racketmodname[racket/path]. This library will remain naive.

Arguments that are @racket[pathish?] can take either a string or a path. For clarity below, I've used strings.

@defproc[
(get-ext
[file-path pathish?])
(or/c #f string?)]
Return the last file extension of @racket[_file-path] as a string, or @racket[#f] if it has no extension. Omit the intervening @litchar{.} separator. 

@examples[#:eval my-eval
(get-ext "foo.txt")
(get-ext "/path/to/foo.txt")
(get-ext "/path/to/foo.txt.bar")
(get-ext "/path/to/file-without-extension")
(get-ext "/path/to/directory/")]

@defproc[
(has-ext?
[file-path pathish?]
[ext stringish?])
boolean?]
Return @racket[#t] if the last file extension of @racket[_file-path] is @racket[_ext], otherwise @racket[#f]. Not sensitive to case.

@examples[#:eval my-eval
(has-ext? "foo.txt" "txt")
(has-ext? "foo.txt" "TXT")
(has-ext? "foo.txt" "jpg")
(has-ext? "foo.jpg.txt" "jpg")]

@defproc[
(remove-ext
[file-path pathish?])
path?]
Remove the last file extension of @racket[_file-path], and return the path that remains. If @racket[_file-path] has no extension, you just get the same @racket[_file-path]. Does not use the filesystem.

@examples[#:eval my-eval
(remove-ext "foo.txt")
(remove-ext "/path/to/foo.txt")
(remove-ext "/path/to/foo.txt.bar")
(remove-ext (remove-ext "/path/to/foo.txt.bar"))]

@defproc[
(remove-ext*
[file-path pathish?])
path?]
Like @racket[remove-ext], just more. Remove all file extensions from @racket[_file-path], and return the path that remains. If @racket[_file-path] has no extensions, you just get the same @racket[_file-path]. Does not use the filesystem.

@examples[#:eval my-eval
(remove-ext* "foo.txt")
(remove-ext* "/path/to/foo.txt")
(remove-ext* "/path/to/foo.txt.bar")
(remove-ext* (remove-ext* "/path/to/foo.txt.bar"))]

@defproc[
(add-ext
[file-path pathish?]
[ext stringish?])
path?]
Return a new @racket[_file-path] with @racket[_ext] appended. Note that this does not replace an existing file extension. If that's what you want, then do @racket[(add-ext (remove-ext _file-path) _ext)].

@examples[#:eval my-eval
(add-ext "foo" "txt")
(add-ext "foo.txt" "jpg")
(add-ext (remove-ext "foo.txt") "jpg")]
