#lang racket/base

;; A small library for setting up a proper evaluation context for SDL
;; and Allegro.


;; Huh.  Just requiring racket/gui/base appears to be enough
;; to set up the event loop.
(require racket/gui/base)






;; The commented code below almost works, but not quite, as
;; mouse move events aren't being sent over for some reason.
;; I'll have to investigate further when I have more time.


;; (require ffi/unsafe
;;          ffi/unsafe/objc
;;          racket/gui/dynamic)


;; ;; Most of this comes from:
;; ;; https://github.com/albertz/PySDL/blob/master/SDL/__init__.py
;; ;;
;; ;; This should pauses until the Cocoa context has been initialized.
;; ;; In particular, we need this when dynamically loading SDL, and
;; ;; probably need it for Allegro as well.
;; ;;
;; ;; WARNING: Do not run this in a racket/gui/base context.  It will fight with
;; ;; the initialization code here!
;; (define (wait-for-ns-initialization)
;;   (call/cc 
;;    (lambda (resume)
;;   ;; Note: because we're accessing Cocoa library functions, something must provoke loading
;;   ;; that library.  So we load the library here:
;;   (define cocoa-lib (ffi-lib (format "/System/Library/Frameworks/Cocoa.framework/Cocoa")))

;;   (import-class NSAutoreleasePool 
;;                 NSNotificationCenter
;;                 NSApplication
;;                 NSApp
;;                 NSString
;;                 NSObject)
  
;;   ;; When do we tell the pool to close down?
;;   (define pool (tell (tell NSAutoreleasePool alloc) init))

;;   (displayln pool)
;;   ;; Need to add a little more for creating the dynamic class and the
;;   ;; application activation...
  
  
;;   (define-objc-class MyApplicationActivator NSObject
;;     [] ;; No fields
;;     [- _id (init)
;;        (super-tell init)]
;;     [- _void (activateNow: [_id notification])
;;        (resume)])
  
;;   (define activator (tell (tell MyApplicationActivator alloc) init))
;;   (define center (tell NSNotificationCenter defaultCenter))
;;   (tellv center
;;          addObserver: activator
;;          selector: #:type _SEL (selector activateNow:)
;;          name: #:type _id (tell (tell NSString alloc)
;;                                 initWithUTF8String: #:type _string
;;                                 "NSApplicationDidFinishLaunchingNotification")
;;          object: #f)
  
;;   (define app (tell NSApplication sharedApplication))
;;   (displayln app)
;;   ;(tellv NSApp finishLaunching)
;;   ;(tellv NSApp updateWindows)
;;   ;(tellv NSApp activateIgnoringOtherApps: #:type _bool #t)
;;   (tell app run))))

;; (unless (gui-available?)
;;   (wait-for-ns-initialization)
;;   (void))
