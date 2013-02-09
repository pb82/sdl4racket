#!/usr/bin/env racket
#lang racket

(require 
    rackunit
    rackunit/text-ui
    "../src/sdl.rkt")
    
(define sdl-video-tests
    (test-suite
    "Tests for the SDL_Video subsystem"
    
    (test-case
        "Initialization"
        (check-equal? (sdl-init '(SDL_INIT_VIDEO SDL_INIT_CDROM)) 0 "Initialization failed")
        (check-not-equal? (sdl-was-init '(SDL_INIT_VIDEO)) 0 "Initialization failed")
        (check-equal? (sdl-get-error) #"" "Error condition after initialization")
        (check-not-exn (lambda () (sdl-quit-subsystem '(SDL_INIT_VIDEO)))))))
                
(run-tests sdl-video-tests)
