#!/usr/bin/env racket
#lang racket

(require 
    racket/draw
    "../src/sdl.rkt")

(define (init-sdl) (sdl-init '(SDL_INIT_VIDEO SDL_INIT_TIMER)))

(define (init-screen)
  (let
    ((screen (sdl-set-video-mode 640 480 32 '(SDL_SWSURFACE SDL_DOUBLEBUF))))
    screen))          

;; User-events fired from a timer
(define (timer-callback interval data)
  (let ((userev (sdl-make-event 'SDL_USEREVENT)))
    (begin
      (((userev 'EVENT) 'SET_CODE) 5)
      (sdl-push-event (userev 'POINTER))
      500)))
      
(define (main-loop screen)
  (define timer-id (sdl-add-timer 500 timer-callback #f))
  (define (iter event)
    (begin      
      (sdl-wait-event (event 'POINTER))
      
      (if (eq? (event 'TYPE) 'SDL_USEREVENT)
        (printf "User-event code: ~a\n" ((event 'EVENT) 'CODE))
        '())
      
      (if (eq? (event 'TYPE) 'SDL_QUIT)
        (begin
            (sdl-remove-timer timer-id)
            (sdl-quit))
        (iter event))))
  (iter (sdl-make-event)))
                        
(init-sdl)
(define screen (init-screen))
(main-loop screen)
