#lang racket/base

(require ffi/unsafe
         racket/runtime-path
         (for-syntax racket/base
                     racket/syntax))


(define-syntax (define-sdl-library-path stx)
  (with-syntax ([sdl-library-path
                 (format-id stx "sdl-library-path" #:source stx)])
    (case (system-type 'os)
      [(unix)
       (define machine-type (system-type 'machine))
       (cond
	[(regexp-match #px"x86_64" machine-type)
	 #'(begin
	     (define-runtime-path sdl-library-path
	       (build-path "linux64" "libSDL-1.2.so.0.11.4")))]
	[else
	 #'(begin
	     (define-runtime-path sdl-library-path
	       (build-path "linux32" "libSDL-1.2.so.0.11.4")))])]
      [(windows)
       ;; FIXME: decide based on platform
       (define machine-type (system-type 'machine))
       (displayln machine-type)
       #'(define-runtime-path sdl-library-path
           (build-path "win64" "libSDL-1.2.so.0.11.4"))]
      [(macosx)
       ;; FIXME: we must do some extra stuff
       ;; with regards to Mac OS X.  Will probably look something
       ;; like this:
       ;; https://github.com/albertz/PySDL/blob/master/SDL/__init__.py

       #'(begin
           (log-debug "sdl: macosx")
           (define-runtime-path sdl-library-path
             (build-path "macosx" "SDL")))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-sdl-library-path)
(define sdl-lib (ffi-lib sdl-library-path))

(provide sdl-lib)
