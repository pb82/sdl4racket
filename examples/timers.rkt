#!/usr/bin/env racket
#lang racket

(require 
    racket/draw
    data/queue
    "../src/sdl.rkt")

;; Demonstrates how to launch (and stop) a timer.
;; ---------------------------------------------------------------------

(define colors (make-queue))
(enqueue! colors #xFFFF0000) ;; R
(enqueue! colors #xFF00FF00) ;; G
(enqueue! colors #xFF0000FF) ;; B

(define (init-sdl) (sdl-init '(SDL_INIT_VIDEO SDL_INIT_TIMER)))

(define (init-screen)
  (let
    ((screen (sdl-set-video-mode 640 480 32 '(SDL_SWSURFACE SDL_DOUBLEBUF))))
    screen))          

(define (timer-callback interval data)
   (begin
     (let ((color (dequeue! colors)))        
        ;; Since the sdl4racket implementation of sdl-wait-event will 
        ;; cause the callback to be executed in the same thread as the 
        ;; main loop we can call all SDL functions here 
        ;; (and not only SDL_PushEvent).
        (sdl-fill-rect screen (sdl-make-rect 0 0 640 480) color)
        (sdl-flip screen)
        (enqueue! colors color)
        500)))

(define (main-loop screen)
  (define timer-id (sdl-add-timer 500 timer-callback #f))
  (define (iter event)
    (begin      
      (sdl-wait-event (event 'POINTER))
      (if (eq? (event 'TYPE) 'SDL_QUIT)
        (begin
            (sdl-remove-timer timer-id)
            (sdl-quit))
        (iter event))))
  (iter (sdl-make-event)))
                        

(init-sdl)
(define screen (init-screen))
(main-loop screen)
