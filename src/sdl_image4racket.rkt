#lang racket

(require	
	ffi/unsafe
	ffi/unsafe/define
	"./structs.rkt")
					
(provide 	img-load)

(define-ffi-definer define-img (ffi-lib "libSDL_image" #f))

;; IMG_Load (const char *) -> SDL_Surface*
;;
;; This is the generic entry point to load all kinds
;; of image formats.
(define-img IMG_Load (_fun _bytes -> _sdl-surface-pointer))
(define (img-load path)
	(IMG_Load (string->bytes/locale path)))
