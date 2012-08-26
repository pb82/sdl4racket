#lang racket

(require  
  ffi/unsafe
  ffi/unsafe/define
  ffi/cvector
  ffi/unsafe/cvector
  "./structs.rkt")

(provide (all-defined-out))
(provide (struct-out sdl-surface))
(provide (struct-out sdl-rect))
(provide (struct-out sdl-color))
(provide (struct-out sdl-palette))
(provide (struct-out sdl-pixel-format))
(provide (struct-out sdl-video-info))
(provide (struct-out sdl-joystick))
(provide (struct-out sdl-cd))
(provide (struct-out sdl-cdtrack))

(define *flags* 
 '((SDL_INIT_TIMER        #x00000001)
   (SDL_INIT_AUDIO        #x00000010)
   (SDL_INIT_VIDEO        #x00000020)
   (SDL_INIT_CDROM        #x00000100)
   (SDL_INIT_JOYSTICK     #x00000200)
   (SDL_INIT_NOPARACHUTE  #x00100000)
   (SDL_INIT_EVENTTHREAD  #x01000000)
   (SDL_INIT_EVERYTHING   #x0000FFFF)
   ;; Available for SDL_CreateRGBSurface or
   ;; SDL_SetVideoMode
   (SDL_SWSURFACE         #x00000000)
   (SDL_HWSURFACE         #x00000001)
   (SDL_ASYNCBLIT         #x00000004)
   ;; Avaibalbe for SDL_SetVideoMode
   (SDL_ANYFORMAT         #x10000000)
   (SDL_HWPALETTE         #x20000000)
   (SDL_DOUBLEBUF         #x40000000)
   (SDL_FULLSCREEN        #x80000000)
   (SDL_OPENGL            #x00000002)
   (SDL_OPENGLBLIT        #x0000000A)
   (SDL_RESIZABLE         #x00000010)
   (SDL_NOFRAME           #x00000020)
   ;; Used internally (read-only)
   (SDL_HWACCEL           #x00000100)
   (SDL_SRCCOLORKEY       #x00001000)
   (SDL_RLEACCELOK        #x00002000)
   (SDL_SRCALPHA          #x00010000)
   (SDL_PREALLOC          #x01000000)))

;; SDL_GrabMode
(define SDL_GRAB_QUERY    -1)
(define SDL_GRAB_OFF      0)
(define SDL_GRAB_ON       1)

(define SDL_ADDEVENT      0)
(define SDL_PEEKEVENT     1)
(define SDL_GETEVENT      2)
(define SDL_ALLEVENTS     #xFFFFFFFF)

;; Event states. For use with sdl-event-state
(define *event-states*
  '((SDL_QUERY	         -1)
    (SDL_IGNORE	          0)
    (SDL_DISABLE	        0)
    (SDL_ENABLE	          1)))

;; Predefined error messages
(define *sdl-error*
  '((SDL_ENOMEM           0)
    (SDL_EFREAD           1)
    (SDL_EFWRITE          2)
    (SDL_EFSEEK           3)
    (SDL_UNSUPPORTED      4)
    (SDL_LASTERROR        5)))

;; libSDL and libSDL_image initialization
;; ---------------------------------------------------------------------

(define (sdl-get-lib)
  (let ((type (system-type 'os)))
    (case type
      ((unix)     "libSDL")
      ((windows)  "SDL")
      ;; correct? can't test on OS X
      ((macosx)   "libSDL")
      (else (error "Platform not supported: " type)))))

(define (sdl-image-get-lib)
  (let ((type (system-type 'os)))
    (case type
      ((unix)     "libSDL_image")
      ((windows)  "SDL_image")
      ;; correct? can't test on OS X
      ((macosx)   "libSDL_image")
      (else (error "Platform not supported: " type)))))

(define-ffi-definer define-sdl (ffi-lib (sdl-get-lib) #f))

;; Try to load the SDL_image lib and provide IMG_load, the generic
;; entry point for all sorts of images. This can fail (if SDL_image
;; is not installed). But that's ok, it's an optional dependency.

;; Dummy img-load. If the library can't be loaded, the usage of
;; the img-load wrapper throws an exception.
(define img-load 
  (lambda (dummy) (error "img-load: SDL_image not available.")))

;; Try to load SDL_image
(with-handlers 
  ((exn:fail? 
    (lambda (ex) 
      (printf "Failed to load optional dependency: SDL_image: ~a" ex))))      
    (begin
      (define-ffi-definer define-img (ffi-lib (sdl-image-get-lib) #f))
      (define-img IMG_Load (_fun _bytes -> _sdl-surface-pointer))
      ;; If loading the library succeeded, replace the dummy function
      ;; with the actual SDL_image export.
      (set! img-load (lambda (path)
      (IMG_Load (string->bytes/locale path))))))
;; ---------------------------------------------------------------------


;; Helper functions
;; ---------------------------------------------------------------------

;; merge-flags: bitwise-or a list
(define (merge-flags flags flag-map)
  (let ((vals (map (lambda (flag) (cadr (assoc flag flag-map))) flags)))
    (foldl (lambda (a b) (bitwise-ior a b)) 0 vals)))

(define (assert condition value who)
  (if condition
    value
    (error who "failed with " value)))

(define-syntax-rule (handle-msg-error msg)
  (error "Unknown message: " msg))

;; Determine sytem byteorder
;;
;; Returns 'LITTLE on little endian systems (e.g. Linux on x86)
;; or 'BIG on big endian systems (e.g. Linux on PowerPC) Throws 
;; if the endianness could not be determined.
;;
;; Thanks to Jens Axel Søgaard (Racket Mailing List) for pointing me
;; to the system-big-endian? function.
(define (sdl-get-endianness)
  (if (system-big-endian?)
    'BIG
    'LITTLE))


;; Just to have a uniform API. All sdl related functions should start
;; with sdl-... This includes also structure creation.
(define sdl-make-rect   make-sdl-rect)
(define sdl-make-color  make-sdl-color)

;; Get a valid (in terms of endianness) bitmask for use with SDL_Surface
;; creation.
(define (sdl-default-mask type)
  (if (eqv? 'BIG (sdl-get-endianness))
    (cond ((eqv? type 'R) #xFF000000)
          ((eqv? type 'G) #x00FF0000)
          ((eqv? type 'B) #x0000FF00)
          ((eqv? type 'A) #x000000FF)
          (else (error "Not a valid mask descriptor: " type)))
    (cond ((eqv? type 'R) #x000000FF)
          ((eqv? type 'G) #x0000FF00)
          ((eqv? type 'B) #x00FF0000)
          ((eqv? type 'A) #xFF000000)
          (else (error "Not a valid mask descriptor: " type)))))


;; SDL initialization and cleanup functions
;; ---------------------------------------------------------------------

;; sdl-init

(define-sdl SDL_Init 
  (_fun _uint32 
    -> (r : _int) 
    -> (assert (= r 0) r 'sdl-init)))
    
(define (sdl-init flags)
  (SDL_Init (merge-flags flags *flags*)))

;; sdl-init-subsystem
(define-sdl SDL_InitSubSystem
  (_fun _uint32
    -> (r : _int) 
    -> (assert (= r 0) r 'sdl-init-subsystem)))
    
(define (sdl-init-subsystem flags)
  (SDL_InitSubSystem (merge-flags flags *flags*)))

;; sdl-quit-subsystem
(define-sdl SDL_QuitSubSystem
  (_fun _uint32
    -> _void))
    
(define (sdl-quit-subsystem flags)
  (SDL_QuitSubSystem (merge-flags flags *flags*)))

;; sdl-quit
(define-sdl SDL_Quit 
  (_fun 
    -> _void))
  
(define sdl-quit SDL_Quit)

;; sdl-was-init
(define-sdl SDL_WasInit
  (_fun _uint32
    -> _int))
    
(define (sdl-was-init flags)
  (SDL_WasInit (merge-flags flags *flags*)))

;; sdl-get-error
(define-sdl SDL_GetError 
  (_fun 
    -> _bytes))
    
(define sdl-get-error SDL_GetError)

;; sdl-set-error
(define-sdl SDL_SetError
  (_fun _bytes
    -> _void))
    
(define (sdl-set-error message)
  (SDL_SetError (string->bytes/locale message)))

;; sdl-error
(define-sdl SDL_Error
  (_fun _uint8
    -> _void))

(define (sdl-error error-flag)
  (SDL_Error (cadr (assoc error-flag *sdl-error*))))

;; sdl-clear-error
(define-sdl SDL_ClearError
  (_fun
    -> _void))
    
(define sdl-clear-error SDL_ClearError)

;; TODO:
;; MISSING:
;; SDL_LoadObject
;; SDL_LoadFunction
;; SDL_UnloadObject
;; SDL_envvars
;; SDL_VERISON
;; SDL_Linked_Version
;; SDL_version

;; ---------------------------------------------------------------------


;; SDL video subsystem
;; ---------------------------------------------------------------------

;; sdl-get-video-surface
(define-sdl SDL_GetVideoSurface 
  (_fun 
    -> _sdl-surface-pointer))

(define sdl-get-video-surface SDL_GetVideoSurface)
    
;; sdl-get-video-info
(define-sdl SDL_GetVideoInfo 
  (_fun 
    -> _sdl-video-info-pointer))
    
(define sdl-get-video-info SDL_GetVideoInfo)

;; sdl-video-driver-name
(define-sdl SDL_VideoDriverName 
  (_fun _bytes _int 
    -> _pointer))
    
(define (sdl-video-driver-name)
  (let* ((buffer (make-bytes 12))
         (p (SDL_VideoDriverName buffer 12)))
      (if (not (ptr-equal? p #f))
        (make-sized-byte-string buffer 12)
        (error "Failed to get video driver name. sdl initialized?"))))


;; TODO: 
;; MISSING:
;; SDL_ListModes

;; sdl-video-mode-ok
(define-sdl SDL_VideoModeOK 
  (_fun _int _int _int _uint32 
    -> _int))
    
(define (sdl-video-mode-ok width height bpp flags)
  (let ((bpp (SDL_VideoModeOK width height bpp (merge-flags flags *flags*))))
    (cons (> bpp 0) bpp)))

;; sdl-update-rects
(define (sdl-update-rects screen rects)
  (define (iter item list)
    (if (null? list)
      (SDL_UpdateRect screen 
        (sdl-rect-x item) 
        (sdl-rect-y item) 
        (sdl-rect-w item) 
        (sdl-rect-h item))
      (iter (car list) (cdr list))))
  (iter (car rects) (cdr rects)))
        
;; sdl-set-colors
(define-sdl SDL_SetColors 
  (_fun _sdl-surface-pointer _pointer _int _int 
    -> _int))
    
(define (sdl-set-colors surface colors)
  (let ((vector (list->cvector colors _sdl-color-pointer)))
    (SDL_SetColors surface (cvector-ptr vector) 0 (length colors))))

;; sdl-set-palette
(define-sdl SDL_SetPalette 
  (_fun _sdl-surface-pointer _int _pointer _int _int 
    -> _int))
    
(define (sdl-set-palette surface flags colors)
  (let ((flags-value (merge-flags flags *flags*))
        (vector (list->cvector colors _sdl-color-pointer)))
    (SDL_SetPalette 
      surface 
      flags-value 
      (cvector-ptr vector) 
      0 
      (length colors))))

;; sdl-set-gamma
(define-sdl SDL_SetGamma 
  (_fun _float _float _float 
    -> (r : _int) 
    -> (assert (>= r 0) r 'sdl-set-gamma)))
    
(define (sdl-set-gamma r g b)
  (SDL_SetGamma r g b))

;; sdl-set-gamma-ramp
;; returns a list of three lists. Each of the nested lists has a length
;; of 256 and contains the tables for red, green and blue.
;; Throws exception on error.
(define-sdl SDL_GetGammaRamp 
  (_fun _pointer _pointer _pointer 
    -> (r : _int) 
    -> (assert (>= r 0) r 'sdl-get-gamma-ramp)))
    
(define (sdl-get-gamma-ramp)
  (let ((r (make-cvector _uint16 256))
        (g (make-cvector _uint16 256))
        (b (make-cvector _uint16 256)))
    (begin
      (assert 
        (>= 0 
          (SDL_GetGammaRamp 
            (cvector-ptr r) 
            (cvector-ptr g) 
            (cvector-ptr b))) 0 'sdl-get-gamma-ramp)
      (list (cvector->list r) (cvector->list g) (cvector->list b)))))

;; set-gamma-ramp
(define-sdl SDL_SetGammaRamp 
  (_fun _pointer _pointer _pointer 
    -> (r : _int) 
    -> (assert (>= r 0) r 'sdl-set-gamma-ramp)))
    
(define (sdl-set-gamma-ramp r g b)
  (let ((rvector (list->cvector r _uint16))
        (gvector (list->cvector g _uint16))
        (bvector (list->cvector b _uint16)))
    (SDL_SetGammaRamp 
      (cvector-ptr rvector) 
      (cvector-ptr gvector) 
      (cvector-ptr bvector))))

;; sdl-map-rgb
(define-sdl SDL_MapRGB
  (_fun _sdl-pixel-format-pointer _uint8 _uint8 _uint8
    -> _uint32))

(define sdl-map-rgb SDL_MapRGB)

;; sdl-map-rgba
(define-sdl SDL_MapRGBA
  (_fun _sdl-pixel-format-pointer _uint8 _uint8 _uint8 _uint8
    -> _uint32))

(define sdl-map-rgba SDL_MapRGBA)

;; sdl-get-rgb
(define SDL_GetRGB 
  (_fun _uint32 _sdl-pixel-format-pointer _uint8 _uint8 _uint8
    -> _void))
    
(define sdl-get-rgb SDL_GetRGB)

;; TODO:
;; MISSING:
;; SDL_GetRGB
;; SDL_GetRGBA

;; sdl-create-rgb-surface
(define-sdl SDL_CreateRGBSurface 
  (_fun _uint32 _int _int _int _uint32 _uint32 _uint32 _uint32
    -> _sdl-surface-pointer))
    
(define (sdl-create-rgb-surface flags w h bpp rmask gmask bmask amask)
  (SDL_CreateRGBSurface 
    (merge-flags flags *flags*) 
    w 
    h 
    bpp 
    rmask 
    gmask 
    bmask 
    amask))

;; sdl-create-rgb-surface-from
(define-sdl SDL_CreateRGBSurfaceFrom 
  (_fun _pointer _int _int _int _int _uint32 _uint32 _uint32 _uint32 
    -> _sdl-surface-pointer))
    
(define (sdl-create-rgb-surface-from pixels w h depth pitch rmask gmask bmask amask)
  (SDL_CreateRGBSurfaceFrom 
    pixels 
    w 
    h 
    depth 
    pitch 
    rmask 
    gmask 
    bmask 
    amask))

;; sdl-lock-surface
(define-sdl SDL_LockSurface 
  (_fun _sdl-surface-pointer 
    -> (r : _int) 
    -> (assert (= r 0) r 'sdl-lock-surface)))
    
(define sdl-lock-surface SDL_LockSurface)

;; sdl-unlock-surface
(define-sdl SDL_UnlockSurface 
  (_fun _sdl-surface-pointer 
    -> _void))
    
(define sdl-unlock-surface SDL_UnlockSurface)

;; sdl-convert-surface
(define-sdl SDL_ConvertSurface 
  (_fun _sdl-surface-pointer _sdl-pixel-format-pointer _uint32 
    -> _sdl-surface-pointer))
    
(define (sdl-convert-surface source format flags)
  (SDL_ConvertSurface source format (merge-flags flags *flags*)))

;; sdl-rw-from-file
(define-sdl SDL_RWFromFile 
  (_fun _bytes _bytes 
    -> _pointer))
    
(define (sdl-rw-from-file path mode)
  (SDL_RWFromFile 
    (string->bytes/locale path) 
    (string->bytes/locale mode)))

;; sdl-load-bmp
(define-sdl SDL_LoadBMP_RW 
  (_fun _pointer _int 
    -> _sdl-surface-pointer))
    
(define (sdl-load-bmp path)
  (SDL_LoadBMP_RW (sdl-rw-from-file path "r") 1))

;; sdl-save-bmp
(define-sdl SDL_SaveBMP_RW 
  (_fun _sdl-surface-pointer _pointer _int 
    -> (r : _int) 
    -> (assert (= 0 r) r 'sdl-save-bmp)))
    
(define (sdl-save-bmp surface path)
  (SDL_SaveBMP_RW surface (sdl-rw-from-file path "wb") 1))

;; sdl-set-color-key
(define-sdl SDL_SetColorKey 
  (_fun _sdl-surface-pointer _uint32 _uint32 
    -> (r : _int) 
    -> (assert (= 0 r) r 'sdl-set-color-key)))
    
(define (sdl-set-color-key surface flag key)
  (SDL_SetColorKey surface (merge-flags flag *flags*) key))

;; sdl-set-aplhp
(define-sdl SDL_SetAlpha 
  (_fun _sdl-surface-pointer _uint32 _uint8 
    -> (r : _int) 
    -> (assert (= 0 r) r 'sdl-set-alpha)))
    
(define (sdl-set-alpha surface flags alpha)
  (SDL_SetAlpha surface (merge-flags flags *flags*) alpha))

;; sdl-set-clip-rect
(define-sdl SDL_SetClipRect 
  (_fun _sdl-surface-pointer _sdl-rect-pointer 
    -> _void))
    
(define sdl-set-clip-rect SDL_SetClipRect)

;; sdl-get-clip-rect
(define-sdl SDL_GetClipRect 
  (_fun _sdl-surface-pointer _sdl-rect-pointer 
    -> _void))
    
(define (sdl-get-clip-rect surface)
  (let ((rect (make-sdl-rect 0 0 0 0)))
    (begin
      (SDL_GetClipRect surface rect)
      rect)))

;; sdl-fill-rect
(define-sdl SDL_FillRect 
  (_fun _sdl-surface-pointer _sdl-rect-pointer _uint32 
    -> (r : _int) 
    -> (assert (= r 0) r 'sdl-fill-rect)))
    
(define sdl-fill-rect SDL_FillRect)

;; TODO:
;; MISSING:
;; SDL_LoadLibrary (?)
;; SDL_GetProcAddress (?)
;; SDL_GetAttribute
;; SDL_SetAttribute

(define-sdl SDL_GL_SwapBuffers
  (_fun
    -> _void))
    
(define sdl-gl-swap-buffers SDL_GL_SwapBuffers)

;; SDL_GLattr
;; SDL_CreateYUVOverlay
;; SDL_LockYUVOverlay
;; SDL_DisplayYUVOverlay
;; SDL_FreeYUVOverlay

;; sdl-set-video-mode
(define-sdl SDL_SetVideoMode 
  (_fun _int _int _int _uint32 
    -> _sdl-surface-pointer))
    
(define (sdl-set-video-mode width height bpp flags)
  (SDL_SetVideoMode width height bpp (merge-flags flags *flags*)))

;; sdl-blit-surface
(define-sdl SDL_UpperBlit 
  (_fun 
    _sdl-surface-pointer 
    _sdl-rect-pointer 
    _sdl-surface-pointer 
    _sdl-rect-pointer 
    -> (r : _int) 
    -> (assert (= r 0) r 'sdl-blit-surface)))
    
(define sdl-blit-surface SDL_UpperBlit)

;; sdl-update-rect
(define-sdl SDL_UpdateRect 
  (_fun _sdl-surface-pointer _sint32 _sint32 _sint32 _sint32 
    -> _void))
    
(define sdl-update-rect SDL_UpdateRect)

;; sdl-free-surface
(define-sdl SDL_FreeSurface 
  (_fun _sdl-surface-pointer 
    -> _void))
    
(define sdl-free-surface SDL_FreeSurface)

;; sdl-flip
(define-sdl SDL_Flip 
  (_fun _sdl-surface-pointer 
    -> _void))
    
(define sdl-flip SDL_Flip)

;; sdl-display-format
(define-sdl SDL_DisplayFormat 
  (_fun _sdl-surface-pointer 
    -> _sdl-surface-pointer))
    
(define sdl-display-format SDL_DisplayFormat)

;; sdl-display-format-alpha
(define-sdl SDL_DisplayFormatAlpha 
  (_fun _sdl-surface-pointer 
    -> _sdl-surface-pointer))
    
(define sdl-display-format-alpha SDL_DisplayFormatAlpha)

;; ---------------------------------------------------------------------


;; SDL window management
;; ---------------------------------------------------------------------

;; SDL_WM_SetCaption
(define-sdl SDL_WM_SetCaption 
  (_fun _bytes _bytes 
    -> _void))
    
(define (sdl-wm-set-caption title icon)
  (SDL_WM_SetCaption 
    (string->bytes/locale title) 
    (string->bytes/locale icon)))

;; TODO:
;; MISSING:
;; SDL_GetWMCaption
;; SDL_GetWMInfo

;; sdl-wm-set-icon
(define-sdl SDL_WM_SetIcon 
  (_fun _sdl-surface-pointer _uint8 
    -> _void))
    
(define sdl-wm-set-icon SDL_WM_SetIcon)

;; sdl-wm-iconify-window
(define-sdl SDL_WM_IconifyWindow 
  (_fun 
    -> _int))
    
(define sdl-wm-iconify-window SDL_WM_IconifyWindow)

;; sdl-toggle-fullscreen
(define-sdl SDL_WM_ToggleFullScreen 
  (_fun _sdl-surface-pointer 
    -> _int))
    
(define sdl-wm-toggle-fullscreen SDL_WM_ToggleFullScreen)

;; sdl-wm-grab-input
(define-sdl SDL_WM_GrabInput 
  (_fun _int 
    -> _int))
    
(define sdl-wm-grab-input SDL_WM_GrabInput)
;; ---------------------------------------------------------------------


;; SDL timing
;; ---------------------------------------------------------------------

;; sdl-get-ticks
(define-sdl SDL_GetTicks
  (_fun
    -> _uint32))
    
(define sdl-get-ticks SDL_GetTicks)

;; sdl-delay
(define-sdl SDL_Delay
  (_fun _uint32
    -> _void))
    
(define sdl-delay SDL_Delay)

;; TODO:
;; MISSING:
;; SDL_AddTimer
;; SDL_RemoveTimer
;; SDL_SetTimer

;; ---------------------------------------------------------------------


;; SDL mouse
;; ---------------------------------------------------------------------

;; sdl-warp-mouse
(define-sdl SDL_WarpMouse
  (_fun _uint16 _uint16
    -> _void))
    
(define sdl-warp-mouse SDL_WarpMouse)

;; TODO:
;; MISSING:
;; SDL_CreateCursor
;; SDL_FreeCursor
;; SDL_SetCursor
;; SDL_GetCursor

;; sdl-show-cursor
(define-sdl SDL_ShowCursor
  (_fun _int
    -> _int))
    
(define (sdl-show-cursor toggle)
  (SDL_ShowCursor (cadr (assoc toggle *event-states*))))

;; ---------------------------------------------------------------------


;; SDL joystick
;; ---------------------------------------------------------------------

;; sdl-num-joysticks
(define-sdl SDL_NumJoysticks
  (_fun
    -> _int))
    
(define sdl-num-joysticks SDL_NumJoysticks)

;; sdl-joystick-name
(define-sdl SDL_JoystickName
  (_fun _int
    -> _bytes))
    
(define sdl-joystick-name SDL_JoystickName)

;; sdl-joystick-open
(define-sdl SDL_JoystickOpen
  (_fun _int
    -> _sdl-joystick-pointer))
    
(define sdl-joystick-open SDL_JoystickOpen)

;; sdl-joystick-index
(define-sdl SDL_JoystickIndex
  (_fun _sdl-joystick-pointer
    -> _int))
    
(define sdl-joystick-index SDL_JoystickIndex)
  
;; sdl-joystick-num-axes
(define-sdl SDL_JoystickNumAxes
  (_fun _sdl-joystick-pointer
    -> _int))

(define sdl-joystick-num-axes SDL_JoystickNumAxes)
  
;; sdl-joystick-num-balls
(define-sdl SDL_JoystickNumBalls
  (_fun _sdl-joystick-pointer
    -> _int))
    
(define sdl-joystick-num-balls SDL_JoystickNumBalls)
  
;; sdl-joystick-num-buttons
(define-sdl SDL_JoystickNumButtons
  (_fun _sdl-joystick-pointer
    -> _int))
    
(define sdl-joystick-num-buttons SDL_JoystickNumButtons)

;; sdl-joystick-update
(define-sdl SDL_JoystickUpdate
  (_fun
    -> _void))
    
(define sdl-joystick-update SDL_JoystickUpdate)
  
;; sdl-joystick-get-axis
(define-sdl SDL_JoystickGetAxis
  (_fun _sdl-joystick-pointer _int
    -> _sint16))
    
(define sdl-joystick-get-axis SDL_JoystickGetAxis)
  
;; sdl-joystick-get-hat
(define-sdl SDL_JoystickGetHat
  (_fun _sdl-joystick-pointer _int
    -> _uint8))
    
(define sdl-joystick-get-hat SDL_JoystickGetHat)
  
;; sdl-joystick-get-button
(define-sdl SDL_JoystickGetButton
  (_fun _sdl-joystick-pointer _int
    -> _uint8))
    
(define sdl-joystick-get-button SDL_JoystickGetButton)

;; sdl-joystick-get-ball
(define-sdl SDL_JoystickGetBall
  (_fun _sdl-joystick-pointer _int _pointer _pointer
    -> (r : _int)
    -> (assert (= r 0) r 'sdl-joystick-get-ball)))
    
(define (sdl-joystick-get-ball joystick ball)
  (let* ((dx (malloc (ctype-sizeof _int)))
         (dy (malloc (ctype-sizeof _int)))
         (r  (SDL_JoystickGetBall joystick ball dx dy)))
      (list        
        (ptr-ref dx _int)
        (ptr-ref dy _int))))

;; sdl-joystick-close
(define-sdl SDL_JoystickClose
  (_fun _sdl-joystick-pointer
    -> _void))
    
(define sdl-joystick-close SDL_JoystickClose)
    
;; ---------------------------------------------------------------------


;; SDL CD-ROM
;; ---------------------------------------------------------------------

;; sdl-cd-num-drives
(define-sdl SDL_CDNumDrives
  (_fun
    -> _int))

(define sdl-cd-num-drives SDL_CDNumDrives)

;; sdl-cd-name
(define-sdl SDL_CDName 
  (_fun _int
    -> _bytes))
    
(define sdl-cd-name SDL_CDName)

;; sdl-cd-open
(define-sdl SDL_CDOpen
  (_fun _int
    -> _sdl-cd-pointer))
    
(define sdl-cd-open SDL_CDOpen)

;; sdl-cd-status
(define-sdl SDL_CDStatus
  (_fun _sdl-cd-pointer
    -> _CDStatus))

(define sdl-cd-state SDL_CDStatus)

;; sdl-cd-play
(define-sdl SDL_CDPlay
  (_fun _sdl-cd-pointer _int _int
    -> (r : _int) 
    -> (assert (= r 0) r 'sdl-cd-play)))
    
(define sdl-cd-play SDL_CDPlay)
    
;; sdl-cd-play-tracks
(define-sdl SDL_CDPlayTracks
  (_fun _sdl-cd-pointer _int _int _int _int
    -> (r : _int) 
    -> (assert (= r 0) r 'sdl-cd-play-tracks)))
    
(define sdl-cd-play-tracks SDL_CDPlayTracks)

;; sdl-cd-pause
(define-sdl SDL_CDPause
  (_fun _sdl-cd-pointer
    -> (r : _int) 
    -> (assert (= r 0) r 'sdl-cd-pause)))

(define sdl-cd-pause SDL_CDPause)

;; sdl-cd-resume
(define-sdl SDL_CDResume
  (_fun _sdl-cd-pointer
    -> (r : _int) 
    -> (assert (= r 0) r 'sdl-cd-resume)))

(define sdl-cd-resume SDL_CDResume)

;; sdl-cd-stop
(define-sdl SDL_CDStop
  (_fun _sdl-cd-pointer
    -> (r : _int) 
    -> (assert (= r 0) r 'sdl-cd-stop)))

(define sdl-cd-stop SDL_CDStop)

;; sdl-cd-eject
(define-sdl SDL_CDEject
  (_fun _sdl-cd-pointer
    -> (r : _int) 
    -> (assert (= r 0) r 'sdl-cd-eject)))

(define sdl-cd-eject SDL_CDEject)

;; sdl-cd-close
(define-sdl SDL_CDClose
  (_fun _sdl-cd-pointer
    -> _void))
    
(define sdl-cd-close SDL_CDClose)

;; ---------------------------------------------------------------------


;; SDL Audio
;; ---------------------------------------------------------------------

(define sdl-make-audio-spec make-SDL_AudioSpec)

;; ---------------------------------------------------------------------


;; SDL events
;; ---------------------------------------------------------------------

;; sdl-pump-events
(define-sdl SDL_PumpEvents 
  (_fun 
    -> _void))
    
(define (sdl-pump-events)
  (SDL_PumpEvents))

;; sdl-peep-events
(define-sdl SDL_PeepEvents 
  (_fun _pointer _int _uint8 _uint32 
    -> (r : _int)
    -> (assert (>= r 0) r 'sdl-wait-events)))
    
(define (sdl-peep-events events action mask)
  (let ((pointers 
          (list->cvector 
            (map (lambda (event) (event 'POINTER)) events) _pointer)))
      
    (SDL_PeepEvents 
      (cvector-ptr pointers) 
      (length events) 
      action 
      mask)))
      
;; sdl-poll-event
(define-sdl SDL_PollEvent 
  (_fun _sdl-event-pointer 
    -> _int))
    
(define (sdl-poll-event event)
  (SDL_PollEvent event))

;; sdl-wait-event
(define-sdl SDL_WaitEvent 
  (_fun _sdl-event-pointer 
    -> (r : _int) 
    -> (assert (= 1 r) r 'sdl-wait-events)))
    
(define (sdl-wait-event event)
  (SDL_WaitEvent event))

;; TODO:
;; MISSING:
;; SDL_PushEvent
;; SDL_GetEventFilter
;; SDL_SetEventFilter

;; sdl-event-state
(define-sdl SDL_EventState
  (_fun _sdl-event-type _int
    -> _uint8))
    
(define (sdl-event-state type state)
  (SDL_EventState type (cadr (assoc state *event-states*))))

;; sdl-get-key-state
(define-sdl SDL_GetKeyState
  (_fun _pointer
    -> _pointer))

;; TODO:
;; This function returns  a list that must be indexed with the values
;; of the _sdl-key enum. Make this enum accessible to the user.
(define (sdl-get-key-state)
  (let* ((numkeys (malloc (ctype-sizeof _int)))
         (result  (SDL_GetKeyState numkeys))
         (length  (ptr-ref numkeys _int))
         (vector  (make-cvector* result _uint8 length)))
    (cvector->list vector)))

;; sdl-get-mod-state
(define-sdl SDL_GetModState
  (_fun
    -> _sdl-mod))
    
(define (sdl-get-mod-state) (SDL_GetModState))

;; sdl-set-mod-state
(define-sdl SDL_SetModState
  (_fun _sdl-mod
    -> _void))
    
;; TODO:
;; Make the _sdl-mod enum accessible to the user.
(define (sdl-set-mod-state state)
  (SDL_SetModState state))

;; TODO:
;; MISSING:
;; SDL_GetKeyName

;; sdl-enable-unicode
(define-sdl SDL_EnableUNICODE
  (_fun _int
    -> _int))
    
(define (sdl-enable-unicode enable)
  (SDL_EnableUNICODE enable))

;; sdl-enable-key-repeat
(define-sdl SDL_EnableKeyRepeat
  (_fun _int _int
    -> (r : _int)
    -> (assert (= r 0) r 'sdl-enable-key-repeat)))

(define (sdl-enable-key-repeat delay interval)
  (SDL_EnableKeyRepeat delay interval))

;; sdl-get-mouse-state
(define-sdl SDL_GetMouseState
  (_fun _pointer _pointer
    -> _uint8))
    
(define (sdl-get-mouse-state)
  (let* ((x (malloc (ctype-sizeof _int)))
         (y (malloc (ctype-sizeof _int)))
         (r (SDL_GetMouseState x y)))
    (begin
      (list
        r
        (ptr-ref x _int)
        (ptr-ref y _int)))))

;; sdl-get-relative-mouse-state
(define-sdl SDL_GetRelativeMouseState
  (_fun _pointer _pointer
    -> _uint8))
    
(define (sdl-get-relative-mouse-state)
  (let* ((x (malloc (ctype-sizeof _int)))
         (y (malloc (ctype-sizeof _int)))
         (r (SDL_GetRelativeMouseState x y)))
    (begin
      (list
        r
        (ptr-ref x _int)
        (ptr-ref y _int)))))

;; sdl-get-app-state
(define-sdl SDL_GetAppState
  (_fun 
    -> _uint8))
    
;; TODO:
;; Make the result comparable to either of
;; SDL_APPMOUSEFOCUS
;; SDL_APPINPUTFOCUS
;; SDL_APPACTIVE
(define (sdl-get-app-state) (SDL_GetAppState))

;; sdl-joystick-event-state
(define-sdl SDL_JoystickEventState
  (_fun _int
    -> _int))
    
(define (sdl-joystick-event-state state)
  (SDL_JoystickEventState (cadr (assoc state *event-states*))))

;; SDL event structures are converted to function closures.
;; There is a constructor function for every event type, that
;; returns another function which takes a symbol (message) with
;; the name of the desired property.

(define (sdl-active-constructor raw-pointer type)
  (let ((event (ptr-ref raw-pointer _sdl-active-event)))
    (lambda (msg)
      (case msg
        ((TYPE)   type)
        ((GAIN)   (sdl-active-event-gain        event))
        ((STATE)  (sdl-active-event-state       event))        
        (else     (handle-msg-error msg))))))

(define (sdl-mouse-motion-constructor raw-pointer type)
  (let ((event (ptr-ref raw-pointer _sdl-mouse-motion-event)))
    (lambda (msg)
      (case msg
        ((TYPE)   type)
        ((WHICH)  (sdl-mouse-motion-event-which event))
        ((STATE)  (sdl-mouse-motion-event-state event))
        ((X)      (sdl-mouse-motion-event-x     event))
        ((Y)      (sdl-mouse-motion-event-y     event))
        ((XREL)   (sdl-mouse-motion-event-xrel  event))
        ((YREL)   (sdl-mouse-motion-event-yrel  event))
        (else     (handle-msg-error msg))))))

(define (sdl-keyboard-constructor raw-pointer type)
  (let ((event (ptr-ref raw-pointer _sdl-keyboard-event)))
    (lambda (msg)
      (case msg
        ((TYPE)     type)
        ((WHICH)    (sdl-keyboard-event-which   event))
        ((STATE)    (sdl-keyboard-event-state   event))
        ((KEYSYM)   (sdl-keysym-constructor (sdl-keyboard-event-keysym event)))
        (else       (handle-msg-error msg))))))

(define (sdl-keysym-constructor keysym)
  (lambda (msg)
    (case msg
      ((SCANCODE)   (sdl-keysym-scancode keysym))
      ((SYM)        (sdl-keysym-sym keysym))
      ((MOD)        (sdl-keysym-mod keysym))
      ((UNICODE)    (sdl-keysym-unicode keysym))
      (else         (handle-msg-error msg)))))

(define (sdl-mouse-button-constructor raw-pointer type)
  (let ((event (ptr-ref raw-pointer _sdl-mouse-button-event)))
    (lambda (msg)
      (case msg
        ((TYPE)     type)
        ((WHICH)    (sdl-mouse-button-event-which   event))
        ((BUTTON)   (sdl-mouse-button-event-button  event))
        ((STATE)    (sdl-mouse-button-event-state   event))
        ((X)        (sdl-mouse-button-event-x       event))
        ((Y)        (sdl-mouse-button-event-y       event))
        (else       (handle-msg-error msg))))))

(define (sdl-joy-axis-constructor raw-pointer type)
  (let ((event (ptr-ref raw-pointer _sdl-joy-axis-event)))
    (lambda (msg)
      (case msg
        ((TYPE)     type)
        ((WHICH)    (sdl-joy-axis-event-which       event))
        ((AXIS)     (sdl-joy-axis-event-axis        event))
        ((VALUE)    (sdl-joy-axis-event-value       event))
        (else       (handle-msg-error msg))))))

(define (sdl-joy-ball-constructor raw-pointer type)
  (let ((event (ptr-ref raw-pointer _sdl-joy-ball-event)))
    (lambda (msg)
      (case msg
        ((TYPE)     type)
        ((WHICH)    (sdl-joy-ball-event-which       event))
        ((BALL)     (sdl-joy-ball-event-ball        event))
        ((XREL)     (sdl-joy-ball-event-xrel        event))
        ((YREL)     (sdl-joy-ball-event-yrel        event))
        (else       (handle-msg-error msg))))))

(define (sdl-joy-hat-constructor raw-pointer type)
  (let ((event (ptr-ref raw-pointer _sdl-joy-hat-event)))
    (lambda (msg)
      (case msg
        ((TYPE)     type)
        ((WHICH)    (sdl-joy-hat-event-which        event))
        ((HAT)      (sdl-joy-hat-event-hat          event))
        ((VALUE)    (sdl-joy-hat-event-value        event))
        (else       (handle-msg-error msg))))))

(define (sdl-joy-button-constructor raw-pointer type)
  (let ((event (ptr-ref raw-pointer _sdl-joy-button-event)))
    (lambda (msg)
      (case msg
        ((TYPE)     type)
        ((WHICH)    (sdl-joy-button-event-which     event))
        ((BUTTON)   (sdl-joy-button-event-button    event))
        ((STATE)    (sdl-joy-button-event-state     event))
        (else       (handle-msg-error msg))))))

(define (sdl-resize-constructor raw-pointer type)
  (let ((event (ptr-ref raw-pointer _sdl-resize-event)))
    (lambda (msg)
      (case msg
        ((TYPE)     type)
        ((W)        (sdl-resize-event-w             event))
        ((H)        (sdl-resize-event-h             event))
        (else       (handle-msg-error msg))))))

(define (sdl-expose-constructor type)
  (lambda (msg)
    (case msg
      ((TYPE)       type)
      (else         (handle-msg-error msg)))))

(define (sdl-quit-constructor type)
  (lambda (msg)
    (case msg
      ((TYPE)       type)
      (else         (handle-msg-error msg)))))

(define (sdl-make-event)
  (let ((event (malloc 128)))
    (begin
      (cpointer-push-tag! event sdl-event-tag)
      (lambda (msg)
        (case msg         
          ((POINTER) event)
          ((TYPE) (sdl-event-type event))
          ((EVENT) 
            (case (sdl-event-type event)
              ((SDL_ACTIVEEVENT)      (sdl-active-constructor       event 'SDL_ACTIVEEVENT))
              ((SDL_MOUSEMOTION)      (sdl-mouse-motion-constructor event 'SDL_MOUSEMOTION))
              ((SDL_KEYDOWN)          (sdl-keyboard-constructor     event 'SDL_KEYDOWN))
              ((SDL_KEYUP)            (sdl-keyboard-constructor     event 'SDL_KEYUP))
              ((SDL_MOUSEBUTTONDOWN)  (sdl-mouse-button-constructor event 'SDL_MOUSEBUTTONDOWN))
              ((SDL_MOUSEBUTTONUP)    (sdl-mouse-button-constructor event 'SDL_MOUSEBUTTONUP))
              ((SDL_JOYAXISMOTION)    (sdl-joy-axis-constructor     event 'SDL_JOYAXISMOTION))
              ((SDL_JOYBALLMOTION)    (sdl-joy-ball-constructor     event 'SDL_JOYBALLMOTION))
              ((SDL_JOYHATMOTION)     (sdl-joy-hat-constructor      event 'SDL_JOYHATMOTION))
              ((SDL_JOYBUTTONDOWN)    (sdl-joy-button-constructor   event 'SDL_JOYBUTTONDOWN))
              ((SDL_JOYBUTTONUP)      (sdl-joy-button-constructor   event 'SDL_JOYBUTTONUP))
              ((SDL_VIDEORESIZE)      (sdl-resize-constructor       event 'SDL_VIDEORESIZE))
              ((SDL_VIDEOEXPOSE)      (sdl-expose-constructor             'SDL_VIDEOEXPOSE))
              ((SDL_QUIT)             (sdl-quit-constructor               'SDL_QUIT))
              (else                   (error "Unkown event type:" msg)))))))))
