#lang racket/base

;; A small library for setting up a proper evaluation context for SDL and Allegro.

(provide initialize-ns-app)


(require ffi/unsafe
         ffi/unsafe/objc)


;; Most of this comes from: https://github.com/albertz/PySDL/blob/master/SDL/__init__.py
  
;;
;; initialize-app: (-> void) -> void
;; Runs after-init in a Cocoa context.  In particular, we need this when dynamically
;; loading SDL, and probably need it for Allegro as well.
;;
;; WARNING: Do not run this in a racket/gui/base context.  It will fight with
;; the initialization code here!
(define (initialize-ns-app after-init)
  
  ;; Note: because we're accessing Cocoa library functions, something must provoke loading
  ;; that library.  So we load the library here:
  (define cocoa-lib (ffi-lib (format "/System/Library/Frameworks/Cocoa.framework/Cocoa")))

  (import-class NSAutoreleasePool 
                NSNotificationCenter
                NSApplication
                NSApp
                NSString
                NSObject)
  
  ;; When do we tell the pool to close down?
  (define pool (tell (tell NSAutoreleasePool alloc) init))

  (displayln pool)
  ;; Need to add a little more for creating the dynamic class and the
  ;; application activation...
  
  
  (define-objc-class MyApplicationActivator NSObject
    [] ;; No fields
    [- _id (init)
       (displayln "In init")
       (super-tell init)]
    [- _void (activateNow: [_id notification])
       (with-handlers ((void (lambda (exn)
                               ;; FIXME: write out the exception.
                               (void))))
         (after-init))
       (exit 0)])
  
  (define activator (tell (tell MyApplicationActivator alloc) init))
  (displayln "activator constructed")
  (displayln activator)

  (define center (tell NSNotificationCenter defaultCenter))
  (displayln "Got center")
  (displayln center)

  (tellv center
         addObserver: activator
         selector: #:type _SEL (selector activateNow:)
         name: #:type _id (tell (tell NSString alloc)
                                initWithUTF8String: #:type _string
                                "NSApplicationDidFinishLaunchingNotification")
         object: #f)
         
  (define app (tell NSApplication sharedApplication))
  (displayln app)
  (tellv NSApp finishLaunching)
  (tellv NSApp updateWindows)
  (tellv NSApp activateIgnoringOtherApps: #:type _bool #t)
  (tell app run))



#;(initialize-ns-app (lambda () 
                     (printf "Hello, I am here\n")
                     (void)))







#|
        init_SDL_dll("/Library/Frameworks/SDL.framework/SDL", "/Library/Frameworks/SDL.framework/Headers")
        init_SDLImage_dll("/Library/Frameworks/SDL_image.framework/SDL_image", "/Library/Frameworks/SDL_image.framework/Headers")
        print('Done loading SDL')

        pool = cp.send_message('NSAutoreleasePool', 'alloc')
        pool = cp.send_message(pool, 'init')

        class MyApplicationActivator_Impl(object):
                MyApplicationActivator = cp.ObjCSubclass('NSObject', 'MyApplicationActivator')

                @MyApplicationActivator.method('@')
                def init(self):
                        self = cp.ObjCInstance(cp.send_super(self, 'init'))
                        return self

                @MyApplicationActivator.method('v@')
                def activateNow(self, aNotification):
                        try:
                                app_main()
                        except:
                                sys.excepthook(*sys.exc_info())
                        os._exit(0)

        MyApplicationActivator = cp.ObjCClass('MyApplicationActivator')
        activator = MyApplicationActivator.alloc().init()
        center = cp.send_message('NSNotificationCenter', 'defaultCenter')
        cp.send_message(center, 'addObserver:selector:name:object:',
                activator,
                cp.get_selector("activateNow"),
                cp.get_NSString("NSApplicationDidFinishLaunchingNotification"),
                None)

        app = cp.send_message('NSApplication', 'sharedApplication')
        cp.send_message('NSApp', 'finishLaunching')
        cp.send_message('NSApp', 'updateWindows')
        cp.send_message('NSApp', 'activateIgnoringOtherApps', True)
        if app_main is not None:
                cp.send_message(app, 'run')		
|#