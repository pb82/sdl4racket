#lang racket

(require  
  ffi/unsafe
  ffi/unsafe/define
  "structs.rkt")

(provide img-load)

;; libSDL and libSDL_image initialization
;; ---------------------------------------------------------------------

(define (sdl-image-get-lib)
  (let ((type (system-type 'os)))
    (case type
      ((unix)     "libSDL_image")
      ((windows)  "SDL_image")
      ;; correct? can't test on OS X
      ((macosx)   "libSDL_image")
      (else (error "Platform not supported: " type)))))

;; Try to load the SDL_image lib and provide IMG_load, the generic
;; entry point for all sorts of images. This can fail (if SDL_image
;; is not installed). But that's ok, it's an optional dependency.

;; Dummy img-load. If the library can't be loaded, the usage of
;; the img-load wrapper throws an exception.
(define img-load 
  (lambda (dummy) (error "img-load: SDL_image not available.")))

;; Try to load SDL_image
(with-handlers 
  ((exn:fail? 
    (lambda (ex) 
      (log-debug
        (format "Failed to load optional dependency: SDL_image: ~a" ex)))))
    (begin
      (define-ffi-definer define-img (ffi-lib (sdl-image-get-lib) #f))
      (define-img IMG_Load (_fun _bytes -> _sdl-surface-pointer))
      ;; If loading the library succeeded, replace the dummy function
      ;; with the actual SDL_image export.
      (set! img-load (lambda (path)
        (IMG_Load (string->bytes/locale path))))))
