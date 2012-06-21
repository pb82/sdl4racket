#lang racket

(require	
	ffi/unsafe
	ffi/unsafe/define
	ffi/cvector
	ffi/unsafe/cvector
	"./structs.rkt")

;; Provided functions from libSDL
(provide (all-defined-out))

(define *flags* 
 '((SDL_INIT_TIMER #x00000001)
	 (SDL_INIT_AUDIO #x00000010)
	 (SDL_INIT_VIDEO #x00000020)
	 (SDL_INIT_CDROM #x00000100)
	 (SDL_INIT_JOYSTICK #x00000200)
	 (SDL_INIT_NOPARACHUTE #x00100000)
	 (SDL_INIT_EVENTTHREAD #x01000000)
	 (SDL_INIT_EVERYTHING #x0000FFFF)
	 ;; Available for SDL_CreateRGBSurface or
	 ;; SDL_SetVideoMode
	 (SDL_SWSURFACE #x00000000)
	 (SDL_HWSURFACE #x00000001)
	 (SDL_ASYNCBLIT #x00000004)
	 ;; Avaibalbe for SDL_SetVideoMode
	 (SDL_ANYFORMAT #x10000000)
	 (SDL_HWPALETTE #x20000000)
	 (SDL_DOUBLEBUF #x40000000)
	 (SDL_FULLSCREEN #x80000000)
	 (SDL_OPENGL #x00000002)
	 (SDL_OPENGLBLIT #x0000000A)
	 (SDL_RESIZABLE #x00000010)
	 (SDL_NOFRAME #x00000020)
	 ;; Used internally (read-only)
	 (SDL_HWACCEL #x00000100)
	 (SDL_SRCCOLORKEY #x00001000)
	 (SDL_RLEACCELOK #x00002000)
	 (SDL_SRCALPHA #x00010000)
	 (SDL_PREALLOC #x01000000)))

;; SDL_GrabMode
(define SDL_GRAB_QUERY -1)
(define SDL_GRAB_OFF 0)
(define SDL_GRAB_ON 1)

;; TODO: Find out on which OS this is running and load
;; the appropriate lib (libSDL.dll, libSDL.dylib)
(define-ffi-definer define-sdl (ffi-lib "libSDL" #f))

;; merge-flags
;; bitwise-or a list
(define (merge-flags flags)
	(let ((vals (map (lambda (flag) (cadr (assoc flag *flags*))) flags)))
		(foldl (lambda (a b) (bitwise-ior a b)) 0 vals)))

(define (assert condition value who)
	(if condition
		value
		(error who "failed with ~a" value)))

;; Determine sytem byteorder
;; Thanks to http://serverfault.com/questions/163487/linux-how-to-tell-if-system-is-big-endian-or-little-endian
;;
;; Returns 'LITTLE on little endian systems (e.g. Linux on x86)
;; or 'BIG on big endian systems (e.g. Linux on PowerPC)
;;
;; Throws exception if the endianness could not be determined.
(define (sdl-get-endianness)
	(case (system-type 'os)
		((unix)	(let ((out (process "echo -n I | od -to2 | head -n1 | cut -f2 -d' ' | cut -c6 ")))
							(let ((result (read-line (car out))))
       					(begin
         					(close-input-port (car out))
         					(close-output-port (cadr out))
         					(close-input-port (cadddr out)))
       						(cond ((string=? result "1") 'LITTLE)
            				((string=? result "0") 'BIG)
             				(else (error "Error determining system endianness"))))))
		((windows) 'LITTLE)
		;; TODO:
		;; Find out how to check for OS X on ppc.
		;; Currently this assumes OS X running on x86 or x86-64.
		((macosx) 'LITTLE)))

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

;; SDL_Init (flags) -> int
(define-sdl SDL_Init (_fun _uint32 -> (r : _int) -> (assert (= r 0) r 'sdl-init)))
(define (sdl-init flags)
	(SDL_Init (merge-flags flags)))

;; SDL_Quit
(define-sdl SDL_Quit (_fun -> _void))
(define (sdl-quit)
	(SDL_Quit))

;; SDL_GetError -> char*
(define-sdl SDL_GetError (_fun -> _bytes))
(define (sdl-get-error)
	(SDL_GetError))

;; <SDL Video>
;; -----------

;; SDL_GetVideoSurface (void)-> SDL_Surface*
(define-sdl SDL_GetVideoSurface (_fun -> _sdl-surface-pointer))
(define (sdl-get-video-surface)
	(SDL_GetVideoSurface))

;; SDL_GetVideoInfo (void) SDL_VideoInfo*
(define-sdl SDL_GetVideoInfo (_fun -> _sdl-video-info-pointer))
(define (sdl-get-video-info)
	(SDL_GetVideoInfo))

;; SDL_GetVideoDriverName (char *buffer, int maxlen)
(define-sdl SDL_VideoDriverName (_fun _bytes _int -> _pointer))
(define (sdl-video-driver-name)
	(let ((buffer (make-bytes 12)))
		(begin
			(let ((p (SDL_VideoDriverName buffer 12)))
				(if (ptr-equal? p #f)
					(error "Failed to get video driver name. Have you called sdl-init?")
					(make-sized-byte-string buffer 12))))))

;; TODO
;; SDL_ListModes

;; SDL_VideoModeOK (width, height, bpp, flags) -> (ok?, bpp)
(define-sdl SDL_VideoModeOK (_fun _int _int _int _uint32 -> _int))
(define (sdl-video-mode-ok width height bpp flags)
	(let ((bpp (SDL_VideoModeOK width height bpp (merge-flags flags))))
		(cons (> bpp 0) bpp)))

;; SDL_UpdateRects
(define (sdl-update-rects screen rects)
	(define (iter item list)
		(if (null? list)
			(SDL_UpdateRect screen (sdl-rect-x item) (sdl-rect-y item) (sdl-rect-w item) (sdl-rect-h item))
			(iter (car list) (cdr list))))
	(iter (car rects) (cdr rects)))
				
;; SDL_SetColors (SDL_Surface*, SDL_Color *, int firstcolor, intncolors)
(define-sdl SDL_SetColors (_fun _sdl-surface-pointer _pointer _int _int -> _int))
(define (sdl-set-colors surface color-list)
	(let ((vector (list->cvector color-list _sdl-color-pointer)))
		(SDL_SetColors surface (cvector-ptr vector) 0 (length color-list))))

;; SDL_SetPalette (SDL_Surface*, int flags, SDL_Color*, int first, int n)
(define-sdl SDL_SetPalette (_fun _sdl-surface-pointer _int _pointer _int _int -> _int))
(define (sdl-set-palette surface flags color-list)
	(let ((flags-value (merge-flags flags))
				(vector (list->cvector color-list _sdl-color-pointer)))
		(SDL_SetPalette surface flags-value (cvector-ptr vector) 0 (length color-list))))

;; SDL_SetGamma (float,float,float) -> int
(define-sdl SDL_SetGamma (_fun _float _float _float -> (r : _int) -> (assert (>= r 0) r 'sdl-set-gamma)))
(define (sdl-set-gamma r g b)
	(SDL_SetGamma r g b))

;; SDL_GetGammaRamp (Uint16*, Uint16*, Uint16*) -> int
;; 
;; Returns a list of three lists. Each of the nested lists has a length
;; of 256 and contains the tables for red, green and blue.
;;
;; Throws exception on error.
(define-sdl SDL_GetGammaRamp (_fun _pointer _pointer _pointer -> (r : _int) -> (assert (>= r 0) r 'sdl-get-gamma-ramp)))
(define (sdl-get-gamma-ramp)
	(let ((r (make-cvector _uint16 256))
				(g (make-cvector _uint16 256))
				(b (make-cvector _uint16 256)))
		(begin
			(assert (>= 0 (SDL_GetGammaRamp (cvector-ptr r) (cvector-ptr g) (cvector-ptr b))) 0 'sdl-get-gamma-ramp)
			(list (cvector->list r) (cvector->list g) (cvector->list b)))))

;; SDL_SetGammaRamp (uint16*, uint16*, uint16*) -> int
(define-sdl SDL_SetGammaRamp (_fun _pointer _pointer _pointer -> (r : _int) -> (assert (>= 0 r) r 'sdl-set-gamma-ramp)))
(define (sdl-set-gamma-ramp r g b)
	(let ((rvector (list->cvector r _uint16))
				(gvector (list->cvector g _uint16))
				(bvector (list->cvector b _uint16)))
		(SDL_SetGammaRamp (cvector-ptr rvector) (cvector-ptr gvector) (cvector-ptr bvector))))

;; TODO:
;;
;; SDL_MapRGB
;; SDL_MapRGBA
;; SDL_GetRGB
;; SDL_GetRGBA

;; SDL_CreateRGBSurface (flags, width, height, bpp, rmas, gmask, bmask, amask) -> SDL_Surface*
(define-sdl SDL_CreateRGBSurface (_fun _uint32 _int _int _int _uint32 _uint32 _uint32 _uint32 -> _sdl-surface-pointer))
(define (sdl-create-rgb-surface flags width height bpp rmask gmask bmask)
	(SDL_CreateRGBSurface (merge-flags flags) width height bpp rmask gmask bmask))

;; SDL_CreateRGBSurfaceFrom (void*, int, int, int, int, uint32, uint32, uint32, uint32)
(define-sdl SDL_CreateRGBSurfaceFrom (_fun _pointer _int _int _int _int _uint32 _uint32 _uint32 _uint32 -> _sdl-surface-pointer))
(define (sdl-create-rgb-surface-from pixels width height depth pitch rmask gmask bmask amask)
	(SDL_CreateRGBSurfaceFrom pixels width height depth pitch rmask gmask bmask amask))

;; SDL_LockSurface (SDL_Surface*) -> int
(define-sdl SDL_LockSurface (_fun _sdl-surface-pointer -> (r : _int) -> (assert (= r 0) r 'sdl-lock-surface)))
(define (sdl-lock-surface surface)
	(SDL_LockSurface surface))

;; SDL_UnlockSurface (SDL_Surface*) -> void
(define-sdl SDL_UnlockSurface (_fun _sdl-surface-pointer -> _void))
(define (sdl-unlock-surface surface)
	(SDL_UnlockSurface surface))

;; SDL_ConvertSurface (SDL_Surface*, SDL_PixelFormat*, uint32) -> SDL_Surface*
(define-sdl SDL_ConvertSurface (_fun _sdl-surface-pointer _sdl-pixel-format-pointer _uint32 -> _sdl-surface-pointer))
(define (sdl-convert-surface source format flags)
	(SDL_ConvertSurface source format (merge-flags flags)))

(define-sdl SDL_RWFromFile (_fun _bytes _bytes -> _pointer))
(define (sdl-rw-from-file path mode)
	(SDL_RWFromFile (string->bytes/locale path) (string->bytes/locale mode)))

;; SDL_LoadBMP (a macro to SDL_LoadBMP_RW) (const char*) -> SDL_Surface*
(define-sdl SDL_LoadBMP_RW (_fun _pointer _int -> _sdl-surface-pointer))
(define (sdl-load-bmp path)
	(SDL_LoadBMP_RW (sdl-rw-from-file path "r") 1))

;; SDL_SaveBMP (a macro to SDL_SaveBMP_RW) (SDL_Surface*, const char*) -> int
(define-sdl SDL_SaveBMP_RW (_fun _sdl-surface-pointer _pointer _int -> (r : _int) -> (assert (= 0 r) r 'sdl-save-bmp)))
(define (sdl-save-bmp surface path)
	(SDL_SaveBMP_RW surface (sdl-rw-from-file path "wb") 1))

;; SDL_SetColorKey (SDL_Surface*, uint32, uint32) -> int
(define-sdl SDL_SetColorKey (_fun _sdl-surface-pointer _uint32 _uint32 -> (r : _int) -> (assert (= 0 r) r 'sdl-set-color-key)))
(define (sdl-set-color-key surface flag key)
	(SDL_SetColorKey surface (merge-flags flag) key))

;; SDL_SetAlpha (SDL_Surface*, uint32, uint8) -> int
(define-sdl SDL_SetAlpha (_fun _sdl-surface-pointer _uint32 _uint8 -> (r : _int) -> (assert (= 0 r) r 'sdl-set-alpha)))
(define (sdl-set-alpha surface flags alpha)
	(SDL_SetAlpha surface (merge-flags flags) alpha))

;; SDL_SetClipRect (SDL_Surface*, SDL_Rect*) -> void
(define-sdl SDL_SetClipRect (_fun _sdl-surface-pointer _sdl-rect-pointer -> _void))
(define (sdl-set-clip-rect surface rect)
	(SDL_SetClipRect surface rect))

;; SDL_GetClipRect (SDL_Surface*, SDL_Rect*) -> void
(define-sdl SDL_GetClipRect (_fun _sdl-surface-pointer _sdl-rect-pointer -> _void))
(define (sdl-get-clip-rect surface)
	(let ((rect (make-sdl-rect 0 0 0 0)))
		(begin
			(SDL_GetClipRect surface rect)
			rect)))

;; SDL_FillRect (SDL_Surface*, SDL_Rect*) -> int
(define-sdl SDL_FillRect (_fun _sdl-surface-pointer _sdl-rect-pointer _uint32 -> (r : _int) -> (assert (= r 0) r 'sdl-fill-rect)))
(define (sdl-fill-rect surface rect color)
	(SDL_FillRect surface rect color))

;; TODO
;; SDL_LoadLibrary (?)
;; SDL_GetProcAddress (?)
;; SDL_GetAttribute
;; SDL_SetAttribute
;; SDL_SwapBuffers
;; SDL_GLattr
;; SDL_CreateYUVOverlay
;; SDL_LockYUVOverlay
;; SDL_DisplayYUVOverlay
;; SDL_FreeYUVOverlay

;; SDL_SetVideoMode (width, height, bpp, flags) -> SDL_Surface*
(define-sdl SDL_SetVideoMode (_fun _int _int _int _uint32 -> _sdl-surface-pointer))
(define (sdl-set-video-mode width height bpp flags)
	(SDL_SetVideoMode width height bpp (merge-flags flags)))

;; SDL_BlitSurface (source, source_rect, dest, dest_rect) -> int
(define-sdl SDL_UpperBlit (_fun _sdl-surface-pointer _sdl-rect-pointer _sdl-surface-pointer _sdl-rect-pointer -> (r : _int) -> (assert (= r 0) r 'sdl-blit-surface)))
(define (sdl-blit-surface s srect d drect)
	(SDL_UpperBlit s srect d drect))

;; SDL_UpdateRect (screen, x, y, w, h)
(define-sdl SDL_UpdateRect (_fun _sdl-surface-pointer _sint32 _sint32 _sint32 _sint32 -> _void))
(define (sdl-update-rect screen x y w h)
	(SDL_UpdateRect screen x y w h))

;; SDL_FreeSurface (surface)
(define-sdl SDL_FreeSurface (_fun _sdl-surface-pointer -> _void))
(define (sdl-free-surface surface)
	(SDL_FreeSurface surface))

;; SDL_Flip (surface)
(define-sdl SDL_Flip (_fun _sdl-surface-pointer -> _void))
(define (sdl-flip surface)
	(SDL_Flip surface))

;; SDL_DisplayFormat
(define-sdl SDL_DisplayFormat (_fun _sdl-surface-pointer -> _sdl-surface-pointer))
(define (sdl-display-format surface)
	(SDL_DisplayFormat surface))

;; SDL_DisplayFormatAlpha (SDL_Surface*) -> SDL_Surface*
(define-sdl SDL_DisplayFormatAlpha (_fun _sdl-surface-pointer -> _sdl-surface-pointer))
(define (sdl-display-format-alpha surface)
	(SDL_DisplayFormatAlpha surface))

;; </SDL Video>
;; ------------

;; <SDL Window Management>
;; -----------------------

;; SDL_WM_SetCaption
(define-sdl SDL_WM_SetCaption (_fun _bytes _bytes -> _void))
(define (sdl-wm-set-caption title icon)
	(SDL_WM_SetCaption (string->bytes/locale title) (string->bytes/locale icon)))

;; TODO
;; SDL_GetWMCaption
;; SDL_GetWMInfo

;; SDL_WM_SetCaption (SDL_Surface*, uint8) -> void
(define-sdl SDL_WM_SetIcon (_fun _sdl-surface-pointer _uint8 -> _void))
(define (sdl-wm-set-icon surface mask)
	(SDL_WM_SetIcon surface mask))

;; SDL_WM_IconifyWindow (void) -> int
(define-sdl SDL_WM_IconifyWindow (_fun -> _int))
(define (sdl-wm-iconify-window)
	(SDL_WM_IconifyWindow))

;; SDL_WM_ToggleFullScreen (SDL_Surface*) -> int
(define-sdl SDL_WM_ToggleFullScreen (_fun _sdl-surface-pointer -> _int))
(define (sdl-wm-toggle-fullscreen surface)
	(SDL_WM_ToggleFullScreen surface))

(define-sdl SDL_WM_GrabInput (_fun _int -> _int))
(define (sdl-wm-grab-input mode)
	(SDL_WM_GrabInput mode))

;; </SDL Window Management>
;; ------------------------

;; <SDL Events>
;; ------------

;; SDL_PumpEvents(void) -> void
(define-sdl SDL_PumpEvents (_fun -> _void))
(define (sdl-pump-events)
	(SDL_PumpEvents))

(define-sdl SDL_WaitEvent (_fun _sdl-event-pointer -> (r : _int) -> (assert (= 1 r) r 'sdl-wait-events)))
(define (sdl-wait-event event)
	(SDL_WaitEvent event))

(define-sdl SDL_PollEvent (_fun _sdl-event-pointer -> _int))
(define (sdl-poll-event event)
	(SDL_PollEvent event))

(define SDL_ADDEVENT 0)
(define SDL_PEEKEVENT 1)
(define SDL_GETEVENT 2)
(define SDL_ALLEVENTS #xFFFFFFFF)


(define-sdl SDL_PeepEvents (_fun _pointer _int _uint8 _uint32 -> _int))
(define (sdl-peep-events events action mask)
	(let ((pointers (list->cvector (map (lambda (event) (event 'POINTER)) events) _pointer)))
		(SDL_PeepEvents (cvector-ptr pointers) (length events) action mask)))

(define (sdl-mouse-motion-constructor event)
	(let ((motion-event (ptr-ref event _sdl-mouse-motion-event)))
		(lambda (msg)
			(case msg
				((WHICH) 	(sdl-mouse-motion-event-which	motion-event))
				((STATE)	(sdl-mouse-motion-event-state motion-event))
				((X) 			(sdl-mouse-motion-event-x 		motion-event))
				((Y) 			(sdl-mouse-motion-event-y 		motion-event))
				((XREL)		(sdl-mouse-motion-event-xrel	motion-event))
				((YREL)		(sdl-mouse-motion-event-yrel	motion-event))
				(else (error "Unknown message: " msg))))))

(define (sdl-keyboard-constructor event)
	(let ((keyboard-event (ptr-ref event _sdl-keyboard-event)))
		(lambda (msg)
			(case msg
				((WHICH)		(sdl-keyboard-event-which		keyboard-event))
				((STATE)		(sdl-keyboard-event-state		keyboard-event))
				((KEYSYM)		(sdl-keyboard-event-keysym	keyboard-event))
				(else (error "Unknown message: " msg))))))

(define (sdl-make-event)
	(let ((event (malloc 128)))
		(begin
			(cpointer-push-tag! event sdl-event-tag)
			(lambda (msg)
				(case msg					
					((POINTER) event)
					((TYPE) (sdl-event-type event))
					((EVENT) (case (sdl-event-type event)
											((SDL_MOUSEMOTION) 	(sdl-mouse-motion-constructor event))
											((SDL_KEYDOWN) 			(sdl-keyboard-constructor event))
											((SDL_KEYUP) 				(sdl-keyboard-constructor event))))
					(else (error "Unkown message:" msg)))))))
