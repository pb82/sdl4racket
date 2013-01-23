#!/usr/bin/env racket
#lang racket

(require "../src/sdl4racket.rkt"
         racket/fixnum
         images/logos)

;; Demonstrates initialization, surfaces and simple event handling.
;; ---------------------------------------------------------------------

;; init-sdl: -> void
(define (init-sdl)
  (void (sdl-init '(SDL_INIT_VIDEO))))

;; init-screen: -> sdl-surface
;; Returns an sdl-surface of the framebuffer.
(define (init-screen)
  (sdl-set-video-mode 640 480 32 '(SDL_SWSURFACE SDL_DOUBLEBUF)))



(define logo (plt-logo))


(define (start-main-loop)
  (define screen (init-screen))
  (define event (sdl-make-event))
  (define sprite (sdl-display-format (bitmap->sdl-surface logo)))
  (define sprite-width (send logo get-width))
  (define sprite-height (send logo get-height))
  (define sprite-width/2 (quotient (send logo get-width) 2))
  (define sprite-height/2 (quotient (send logo get-height) 2))

  ;; Number of milliseconds between ticks to do 60 frames per second.
  (define delay-between-blits (inexact->exact (floor (* 1000.0 (/ 1.0 60.0)))))
  (define last-blit-time 0)

  ;; blit!: number number -> void
  (define (blit! current-x current-y)
    (sdl-fill-rect screen (sdl-make-rect 0 0 640 480) #x00000000)
    (sdl-blit-surface sprite 
                      (sdl-make-rect 0 0 sprite-width sprite-height) 
                      screen
                      (sdl-make-rect (- current-x sprite-width/2)
                                     (- current-y sprite-height/2)
                                     sprite-width
                                     sprite-height))
    (sdl-flip screen)
    (set! last-blit-time (sdl-get-ticks)))

  (define (iter current-x current-y)
    ;; Redraw if necessary:
    (when (fx> (fx- (sdl-get-ticks) last-blit-time)
             delay-between-blits)
      (blit! current-x current-y))

    ;; Watch for mouse movement.
    (cond [(= (sdl-poll-event (event 'POINTER)) 1)
           (let ((type (event 'TYPE)))
             (case type
               ((SDL_QUIT)
                (sdl-quit))
               ((SDL_MOUSEMOTION)
                (define new-x ((event 'EVENT) 'X))
                (define new-y ((event 'EVENT) 'Y))
                (iter new-x new-y))
               (else
                (iter current-x current-y))))]
          [else
           (iter current-x current-y)]))

  (iter 320 240))

(init-sdl)
(start-main-loop)
