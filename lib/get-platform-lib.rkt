#lang racket/base

;; A compatibility library to help get SDL running on the major
;; platforms.

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
             (log-debug "sdl: linux64")
	     (define-runtime-path sdl-library-path
	       (build-path "linux64" "libSDL-1.2.so.0.11.4")))]
	[else
	 #'(begin
             (log-debug "sdl: linux32")
	     (define-runtime-path sdl-library-path
	       (build-path "linux32" "libSDL-1.2.so.0.11.4")))])]
      [(windows)
       ;; FIXME: decide based on platform
       (define machine-type (system-type 'machine))
       (displayln machine-type)
       #'(begin
           (log-debug "sdl: windows")
           (define-runtime-path sdl-library-path
             (build-path "win64" "libSDL-1.2.so.0.11.4")))]

      [(macosx)
       #'(begin
           (require "macosx/wait-for-sdl-init.rkt")
           (log-debug "sdl: macosx")
           (define-runtime-path sdl-library-path
             (build-path "macosx" "SDL")))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-sdl-library-path)
(define sdl-lib (ffi-lib sdl-library-path))

(provide sdl-lib)
