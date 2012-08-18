#lang racket

(require  
  ffi/unsafe)

(provide  
  (all-defined-out))
          
(define-cstruct _sdl-rect
  ((x _sint16)
   (y _sint16)
   (w _uint16)
   (h _uint16)))

(define-cstruct _sdl-color
  ((r _uint8)
   (g _uint8)
   (b _uint8)
   (unused _uint8)))

(define-cstruct _sdl-palette
  ((ncolors _int)
   (colors _sdl-color-pointer)))
    
(define-cstruct _sdl-pixel-format
  ((palette _sdl-palette-pointer)
   (BitsPerPixel _uint8)
   (BytesPerPixel _uint8)
   (Rloss _uint8)
   (Gloss _uint8)
   (Bloss _uint8)
   (Aloss _uint8)
   (Rshift _uint8)
   (Gshift _uint8)
   (Bshift _uint8)
   (Ashift _uint8)
   (Rmask _uint32)
   (Gmask _uint32)
   (Bmask _uint32)
   (Amask _uint32)
   (colorkey _uint32)
   (alpha _uint8)))

(define-cstruct _sdl-surface
  ((flags _uint32)
   (format _sdl-pixel-format-pointer)
   (w _int)
   (h _int)
   (pitch _uint16)
   (pixels _pointer)
   (clip_rect _sdl-rect)
   (refcount _int)))


(define video-info-mask 
  (_bitmask
  '(hw_available =  #x00000000
    wm_available =  #x00000001
    blit_hw =       #x00000002
    blit_hw_CC =    #x00000003
    blit_hw_A =     #x00000004
    blit_sw =       #x00000005
    blit_sw_CC =    #x00000006
    blit_sw_A =     #x00000007
    blit_fill =     #x00000008)
  _uint32))

(define-cstruct _sdl-video-info
  ((mask video-info-mask)
   (vfmt _sdl-pixel-format-pointer)
   (current_w _int)
   (current_h _int)))

;; SDL Event
;; ---------------------------------------------------------------------

(define _sdl-key
  (_enum
    '(SDLK_UNKNOWN = 0
      SDLK_FIRST = 0
      SDLK_BACKSPACE = 8
      SDLK_TAB = 9
      SDLK_CLEAR = 12
      SDLK_RETURN = 13
      SDLK_PAUSE = 19
      SDLK_ESCAPE = 27
      SDLK_SPACE = 32
      SDLK_EXCLAIM = 33
      SDLK_QUOTEDBL = 34
      SDLK_HASH = 35
      SDLK_DOLLAR = 36
      SDLK_AMPERSAND = 38
      SDLK_QUOTE = 39
      SDLK_LEFTPAREN = 40
      SDLK_RIGHTPAREN = 41
      SDLK_ASTERISK = 42
      SDLK_PLUS = 43
      SDLK_COMMA = 44
      SDLK_MINUS = 45
      SDLK_PERIOD = 46
      SDLK_SLASH = 47
      SDLK_0 = 48
      SDLK_1 = 49
      SDLK_2 = 50
      SDLK_3 = 51
      SDLK_4 = 52
      SDLK_5 = 53
      SDLK_6 = 54
      SDLK_7 = 55
      SDLK_8 = 56
      SDLK_9 = 57
      SDLK_COLON = 58
      SDLK_SEMICOLON = 59
      SDLK_LESS = 60
      SDLK_EQUALS = 61
      SDLK_GREATER = 62
      SDLK_QUESTION = 63
      SDLK_AT = 64
      ;; Skip uppercase letters
      SDLK_LEFTBRACKET = 91
      SDLK_BACKSLASH = 92
      SDLK_RIGHTBRACKET = 93
      SDLK_CARET = 94
      SDLK_UNDERSCORE = 95
      SDLK_BACKQUOTE = 96
      SDLK_a = 97
      SDLK_b = 98
      SDLK_c = 99
      SDLK_d = 100
      SDLK_e = 101
      SDLK_f = 102
      SDLK_g = 103
      SDLK_h = 104
      SDLK_i = 105
      SDLK_j = 106
      SDLK_k = 107
      SDLK_l = 108
      SDLK_m = 109
      SDLK_n = 110
      SDLK_o = 111
      SDLK_p = 112
      SDLK_q = 113
      SDLK_r = 114
      SDLK_s = 115
      SDLK_t = 116
      SDLK_u = 117
      SDLK_v = 118
      SDLK_w = 119
      SDLK_x = 120
      SDLK_y = 121
      SDLK_z = 122
      SDLK_DELETE = 127
      ;; End of ascii mapped keysyms
      ;; International keyboard syms
      SDLK_WORLD_0 = 160
      SDLK_WORLD_1 = 161
      SDLK_WORLD_2 = 162
      SDLK_WORLD_3 = 163
      SDLK_WORLD_4 = 164
      SDLK_WORLD_5 = 165
      SDLK_WORLD_6 = 166
      SDLK_WORLD_7 = 167
      SDLK_WORLD_8 = 168
      SDLK_WORLD_9 = 169
      SDLK_WORLD_10 = 170
      SDLK_WORLD_11 = 171
      SDLK_WORLD_12 = 172
      SDLK_WORLD_13 = 173
      SDLK_WORLD_14 = 174
      SDLK_WORLD_15 = 175
      SDLK_WORLD_16 = 176
      SDLK_WORLD_17 = 177
      SDLK_WORLD_18 = 178
      SDLK_WORLD_19 = 179
      SDLK_WORLD_20 = 180
      SDLK_WORLD_21 = 181
      SDLK_WORLD_22 = 182
      SDLK_WORLD_23 = 183
      SDLK_WORLD_24 = 184
      SDLK_WORLD_25 = 185
      SDLK_WORLD_26 = 186
      SDLK_WORLD_27 = 187
      SDLK_WORLD_28 = 188
      SDLK_WORLD_29 = 189
      SDLK_WORLD_30 = 190
      SDLK_WORLD_31 = 191
      SDLK_WORLD_32 = 192
      SDLK_WORLD_33 = 193
      SDLK_WORLD_34 = 194
      SDLK_WORLD_35 = 195
      SDLK_WORLD_36 = 196
      SDLK_WORLD_37 = 197
      SDLK_WORLD_38 = 198
      SDLK_WORLD_39 = 199
      SDLK_WORLD_40 = 200
      SDLK_WORLD_41 = 201
      SDLK_WORLD_42 = 202
      SDLK_WORLD_43 = 203
      SDLK_WORLD_44 = 204
      SDLK_WORLD_45 = 205
      SDLK_WORLD_46 = 206
      SDLK_WORLD_47 = 207
      SDLK_WORLD_48 = 208
      SDLK_WORLD_49 = 209
      SDLK_WORLD_50 = 210
      SDLK_WORLD_51 = 211
      SDLK_WORLD_52 = 212
      SDLK_WORLD_53 = 213
      SDLK_WORLD_54 = 214
      SDLK_WORLD_55 = 215
      SDLK_WORLD_56 = 216
      SDLK_WORLD_57 = 217
      SDLK_WORLD_58 = 218
      SDLK_WORLD_59 = 219
      SDLK_WORLD_60 = 220
      SDLK_WORLD_61 = 221
      SDLK_WORLD_62 = 222
      SDLK_WORLD_63 = 223
      SDLK_WORLD_64 = 224
      SDLK_WORLD_65 = 225
      SDLK_WORLD_66 = 226
      SDLK_WORLD_67 = 227
      SDLK_WORLD_68 = 228
      SDLK_WORLD_69 = 229
      SDLK_WORLD_70 = 230
      SDLK_WORLD_71 = 231
      SDLK_WORLD_72 = 232
      SDLK_WORLD_73 = 233
      SDLK_WORLD_74 = 234
      SDLK_WORLD_75 = 235
      SDLK_WORLD_76 = 236
      SDLK_WORLD_77 = 237
      SDLK_WORLD_78 = 238
      SDLK_WORLD_79 = 239
      SDLK_WORLD_80 = 240
      SDLK_WORLD_81 = 241
      SDLK_WORLD_82 = 242
      SDLK_WORLD_83 = 243
      SDLK_WORLD_84 = 244
      SDLK_WORLD_85 = 245
      SDLK_WORLD_86 = 246
      SDLK_WORLD_87 = 247
      SDLK_WORLD_88 = 248
      SDLK_WORLD_89 = 249
      SDLK_WORLD_90 = 250
      SDLK_WORLD_91 = 251
      SDLK_WORLD_92 = 252
      SDLK_WORLD_93 = 253
      SDLK_WORLD_94 = 254
      SDLK_WORLD_95 = 255
      ;; Numeric keypad
      SDLK_KP0 = 256
      SDLK_KP1 = 257
      SDLK_KP2 = 258
      SDLK_KP3 = 259
      SDLK_KP4 = 260
      SDLK_KP5 = 261
      SDLK_KP6 = 262
      SDLK_KP7 = 263
      SDLK_KP8 = 264
      SDLK_KP9 = 265
      SDLK_KP_PERIOD = 266
      SDLK_KP_DIVIDE = 267
      SDLK_KP_MULTIPLY = 268
      SDLK_KP_MINUS = 269
      SDLK_KP_PLUS = 270
      SDLK_KP_ENTER = 271
      SDLK_KP_EQUALS = 272
      ;; Arrows + Home/End pad
      SDLK_UP = 273
      SDLK_DOWN = 274
      SDLK_RIGHT = 275
      SDLK_LEFT = 276
      SDLK_INSERT = 277
      SDLK_HOME = 278
      SDLK_END = 279
      SDLK_PAGEUP = 280
      SDLK_PAGEDOWN = 281
      ;; Function keys
      SDLK_F1 = 282
      SDLK_F2 = 283
      SDLK_F3 = 284
      SDLK_F4 = 285
      SDLK_F5 = 286
      SDLK_F6 = 287
      SDLK_F7 = 288
      SDLK_F8 = 289
      SDLK_F9 = 290
      SDLK_F10 = 291
      SDLK_F11 = 292
      SDLK_F12 = 293
      SDLK_F13 = 294
      SDLK_F14 = 295
      SDLK_F15 = 296
      ;; Key state modifier keys
      SDLK_NUMLOCK = 300
      SDLK_CAPSLOCK = 301
      SDLK_SCROLLOCK = 302
      SDLK_RSHIFT = 303
      SDLK_LSHIFT = 304
      SDLK_RCTRL = 305
      SDLK_LCTRL = 306
      SDLK_RALT = 307
      SDLK_LALT = 308
      SDLK_RMETA = 309
      SDLK_LMETA = 310
      SDLK_LSUPER = 311
      SDLK_RSUPER = 312
      SDLK_MODE = 313
      SDLK_COMPOSE = 314
      ;; Miscellaneous function keys
      SDLK_HELP = 315
      SDLK_PRINT = 316
      SDLK_SYSREQ = 317
      SDLK_BREAK = 318
      SDLK_MENU = 319
      SDLK_POWER = 320
      SDLK_EURO = 321
      SDLK_UNDO = 322)))
      
(define _sdl-mod
  (_enum
    '(KMOD_NONE =     #x0000
      KMOD_LSHIFT =   #x0001
      KMOD_RSHIFT =   #x0002
      KMOD_LCTRL =    #x0040
      KMOD_RCTRL =    #x0080
      KMOD_LALT =     #x0100
      KMOD_RALT =     #x0200
      KMOD_LMETA =    #x0400
      KMOD_RMETA =    #x0800
      KMOD_NUM =      #x1000
      KMOD_CAPS =     #x2000
      KMOD_MODE =     #x4000
      KMOD_RESERVED = #x8000)
    _uint8))

(define _sdl-event-type
  (_enum
    '(SDL_NOEVENT = 0
      SDL_ACTIVEEVENT
      SDL_KEYDOWN
      SDL_KEYUP
      SDL_MOUSEMOTION
      SDL_MOUSEBUTTONDOWN
      SDL_MOUSEBUTTONUP
      SDL_JOYAXISMOTION
      SDL_JOYBALLMOTION
      SDL_JOYHATMOTION
      SDL_JOYBUTTONDOWN
      SDL_JOYBUTTONUP
      SDL_QUIT
      SDL_SYSWMEVENT
      SDL_EVENT_RESERVEDA
      SDL_EVENT_RESERVEDB
      SDL_VIDEORESIZE
      SDL_VIDEOEXPOSE
      SDL_EVENT_RESERVED2
      SDL_EVENT_RESERVED3
      SDL_EVENT_RESERVED4
      SDL_EVENT_RESERVED5
      SDL_EVENT_RESERVED6
      SDL_EVENT_RESERVED7
      SDL_USEREVENT = 24
      SDL_NUMEVENTS = 32)
    _uint8))

(define-cstruct _sdl-keysym
  ((scancode _uint8)
   (sym _sdl-key)
   (mod _sdl-mod)
   (unicode _uint16)))

(define-cstruct _sdl-active-event
  ((type _uint8)
   (gain _uint8)
   (state _uint8)))

(define-cstruct _sdl-keyboard-event
  ((type _uint8)
   (which _uint8)
   (state _uint8)
   (keysym _sdl-keysym)))

(define-cstruct _sdl-mouse-motion-event
  ((type _uint8)
   (which _uint8)
   (state _uint8)
   (x _uint16)
   (y _uint16)
   (xrel _sint16)
   (yrel _sint16)))

(define-cstruct _sdl-mouse-button-event
  ((type _uint8)
   (which _uint8)
   (button _uint8)
   (state _uint8)
   (x _uint16)
   (y _uint16)))

(define-cstruct _sdl-joy-axis-event
  ((type _uint8)
   (which _uint8)
   (axis _uint8)
   (value _sint16)))

(define-cstruct _sdl-joy-ball-event
  ((type _uint8)
   (which _uint8)
   (ball _uint8)
   (xrel _sint16)
   (yrel _sint16)))

(define-cstruct _sdl-joy-hat-event
  ((type _uint8)
   (which _uint8)
   (hat _uint8)
   (value _uint8)))

(define-cstruct _sdl-joy-button-event
  ((type _uint8)
   (which _uint8)
   (button _uint8)
   (state _uint8)))

(define-cstruct _sdl-resize-event
  ((type _uint8)
   (w _int)
   (h _int)))

(define-cstruct _sdl-user-event
  ((type _uint8)
   (code _int)
   (data1 _pointer)
   (data2 _pointer)))

(define-cstruct _sdl-sys-wm-event
  ((type _uint8)
   (msg _pointer)))

(define-cstruct _sdl-event
  ((type _sdl-event-type)))
;; ---------------------------------------------------------------------


;; SDL Joystick
;; ---------------------------------------------------------------------

(define-cstruct _sdl-joystick
  ((ptr _pointer)))

;; ---------------------------------------------------------------------


;; SDL CD-ROM
;; ---------------------------------------------------------------------

(define _CDStatus
  (_enum
    '(CD_TRAYEMPTY
      CD_STOPPED
      CD_PLAYING
      CD_PAUSED
      CD_ERROR = -1)))

(define-cstruct _SDL_CDtrack
  ((id _uint8)
   (type _uint8)
   (length _uint32)
   (offset _uint32)))

(define-cstruct _SDL_CD
  ((id _int)
   (status _CDStatus)
   (numtracks _int)
   (cur_track _int)
   (cur_frame _int)
   (track _SDL_CDtrack-pointer)))

;; ---------------------------------------------------------------------


;; SDL Audio
;; ---------------------------------------------------------------------

(define-cstruct _SDL_AudioSpec
      ((freq _int)
       (format _uint16)
       (channels _uint8)
       (silence _uint8)
       (samples _uint16)
       (size _uint32)
       (callback (_fun _pointer _pointer _int -> _void))
       (userdata _pointer)))

;; ---------------------------------------------------------------------
