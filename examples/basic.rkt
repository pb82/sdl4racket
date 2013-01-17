#!/usr/bin/env racket
#lang racket

(require "../src/sdl4racket.rkt")

;; Demonstrates initialization, surfaces and simple event handling.
;; ---------------------------------------------------------------------

;; App state handling
;; ---------------------------------------------------------------------
(define (app-state terminate)
  (lambda (msg)
    (case msg
      ((GET) terminate)
      ((SET) (lambda (state) (set! terminate state))))))

(define global-state (app-state #f))
;; ---------------------------------------------------------------------

(define (init-sdl) (sdl-init '(SDL_INIT_VIDEO)))

(define (init-screen)
  (let
    ((screen (sdl-set-video-mode 640 480 32 '(SDL_SWSURFACE SDL_DOUBLEBUF)))
     #;(logo   (sdl-display-format (img-load "logo.png")))
     #;(srect  (sdl-make-rect 0 0 320 240))
     #;(drect  (sdl-make-rect 160 120 320 240)))
          
    (begin        
      #;(sdl-blit-surface logo srect screen drect)
      (sdl-flip screen)
      screen)))

(define (main-loop screen)
  (define (iter event)
    (begin      
      (printf "Waiting for next event\n")
      (sdl-wait-event (event 'POINTER))      
      
      (let ((type (event 'TYPE)))
        (case type
        
          ((SDL_QUIT) ((global-state 'SET) #t))            
          
          ((SDL_MOUSEMOTION)
            (printf "x: ~a y: ~a \n" 
              ((event 'EVENT) 'X) 
              ((event 'EVENT) 'Y)))
              
          (else
            (printf "unhandled event: ~a\n" type))))
            
      ;; Quit?
      (if (eq? #f (global-state 'GET))
        (iter event)
        (sdl-quit))))
  (begin
    (let ((event (sdl-make-event)))
      (iter event))))

(printf "initializing\n")
(init-sdl)

(printf "creating screen\n")
(define screen (init-screen))

(printf "beginning main loop\n")
(main-loop screen)
