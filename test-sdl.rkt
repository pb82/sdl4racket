#!/usr/bin/env racket
#lang racket

(require 	
	"src/sdl4racket.rkt"
	"src/sdl_image4racket.rkt"
	"src/structs.rkt")

;; App State handling
;; -----------------------------------------------------
(define (app-state terminate)
	(lambda (msg)
		(case msg
			((GET) terminate)
			((SET) (lambda (state) (set! terminate state))))))

(define global-state (app-state #f))
;; -----------------------------------------------------

(define (init-sdl)
	(begin
		(sdl-init '(SDL_INIT_VIDEO))))

(define (init-screen)
	(let ((screen (sdl-set-video-mode 320 240 32 '(SDL_SWSURFACE SDL_DOUBLEBUF))))
		screen))

(define (main-loop screen)
	(define (iter event)
		(begin
			(sdl-wait-event (event 'POINTER))

			(let ((type (event 'TYPE)))
				(case type
					((SDL_QUIT) 					((global-state 'SET) #t))
					((SDL_MOUSEMOTION) 		(printf "x: ~a y: ~a \n" ((event 'EVENT) 'X) ((event 'EVENT) 'Y)))
					((SDL_KEYDOWN) 				(printf "key: ~a\n" (((event 'EVENT) 'KEYSYM) 'SYM)))
					((SDL_ACTIVEEVENT) 		(printf "gain: ~a\n" ((event 'EVENT) 'GAIN)))
					((SDL_MOUSEBUTTONUP) 	(printf "button: ~a\n" ((event 'EVENT) 'BUTTON)))
					(else 								(printf "unhandled type: ~a \n" type))))

			(sdl-flip screen)

			(if (eq? #f (global-state 'GET))
				(iter event)
				(sdl-quit))))
	(begin
		(let ((event (sdl-make-event)))
			(iter event))))
		
(init-sdl)
(main-loop (init-screen))
