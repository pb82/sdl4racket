#lang racket/base

(require racket/ffi
         racket/runtime-path
         (for-syntax racket/base
                     racket/syntax))

(define-syntax (define-sdl-library-path stx)
  (with-syntax ([sdl-library-path
                 (format-id stx "sdl-library-path" stx)])
    (case (system-type 'os)
      [(unix)
       (define machine-type (system-type 'machine))
       ;; FIXME: decide based on platform
       #'(define-runtime-path sdl-library-path
           (build-path "linux64" "libSDL-1.2.so.0.11.4"))]
      [(windows)
       ;; FIXME: decide based on platform
       (define machine-type (system-type 'machine))
       #'(define-runtime-path sdl-library-path
           (build-path "win64" "libSDL-1.2.so.0.11.4"))]
      [(macosx)
       #'(define-runtime-path sdl-library-path
           (build-path "macosx" "SDL"))])))

(define-sdl-library-path)
