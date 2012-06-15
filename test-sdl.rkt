#!/usr/bin/env racket
#lang racket

(require 	
	"src/sdl4racket.rkt"
	"src/sdl_image4racket.rkt"
	"src/structs.rkt")

;; Helper function
(define (nth list index)
	(define (iter l i)
		(if (null? l)
			(error "Index out of bounds: " index)
			(cond ((= i index) (car l))
						(#t (iter (cdr l) (+ i 1))))))
	(iter list 0))
	

(sdl-init '(SDL_INIT_VIDEO))
(let ((screen (sdl-set-video-mode 320 240 32 '(SDL_SWSURFACE SDL_DOUBLEBUF)))
			(image (sdl-display-format (img-load "logo.png")))
			;;(image (sdl-display-format (sdl-load-bmp "test.bmp")))
			(srect (make-sdl-rect 0 0 320 240))
			(drect (make-sdl-rect 0 0 320 240)))
	(begin 	(sdl-wm-set-caption "sdl4racket" "")
					(printf "~a\n" (sdl-video-driver-name))
					(printf "~a\n" (car (sdl-video-mode-ok 320 240 32 '(SDL_DOUBLEBUF SDL_SWSURFACE))))
					(printf "Endianness: ~a\n" (sdl-get-endianness))
					;;(printf "~a\n" (sdl-event-struct-type (sdl-wait-event)))
					(sdl-blit-surface image srect screen drect)
					(sdl-set-clip-rect image srect)
					(let ((gamma-ramp (sdl-get-gamma-ramp)))
						(sdl-set-gamma-ramp (nth gamma-ramp 0) (nth gamma-ramp 1) (nth gamma-ramp 2)))
					(let ((rect (sdl-get-clip-rect image)))
						(printf "~a\n" (sdl-rect-w rect)))
				 	(sdl-flip screen)
					(sdl-pump-events)
					(display (sdl-peep-events (list (sdl-make-event) (sdl-make-event)) SDL_PEEKEVENT SDL_ALLEVENTS))
					(sleep 2)
					(sdl-fill-rect screen srect 27)
					(sdl-flip screen)
					(sdl-pump-events)
				 	(sdl-free-surface image)))
	
(sleep 2)
(sdl-quit)
