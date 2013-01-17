#lang racket/base

;; A compatibility library to help get SDL running on the major
;; platforms.

(require ffi/unsafe
         racket/runtime-path
         (for-syntax racket/base
                     racket/syntax))

;; We case analyze on the value of system-library-subpath.
;; There are a few known strings (listed as DEFINE 
;;

(define-syntax (define-sdl-library-path stx)
  (with-syntax ([sdl-library-path
                 (format-id stx "sdl-library-path" #:source stx)])

    (case (system-type 'os)
      [(unix)
       (cond
	[(equal? "x86_64-linux" (system-library-subpath #f))
	 #'(begin
             (log-debug "sdl: linux64")
	     (define-runtime-path sdl-library-path
	       (build-path "linux64" "libSDL-1.2.so.0.11.4")))]
	[(equal? "i386-linux" (system-library-subpath #f))
	 #'(begin
             (log-debug "sdl: linux32")
	     (define-runtime-path sdl-library-path
	       (build-path "linux32" "libSDL-1.2.so.0.11.4")))]
        [else
         (error 'define-sdl-library-path 
                (format "Unsupported platform: ~a" 
                        (system-library-subpath #f)))])]
      [(windows)
       (cond
        [(equal? "win32\\x86_64" (system-library-subpath #f))
         #'(begin
             (log-debug "sdl: windows 64 bit")
             (define-runtime-path sdl-library-path
               (build-path "win64" "SDL.dll")))]
        [(equal? "win32\\i386" (system-library-subpath #f))
          #'(begin
             (log-debug "sdl: windows 32 bit")
             (define-runtime-path sdl-library-path
               (build-path "win32" "SDL.dll")))]
        [else
         (error 'define-sdl-library-path 
                (format "Unsupported platform: ~a" 
                        (system-library-subpath #f)))])]

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
