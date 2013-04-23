sdl4racket
==========

An attempt to create libSDL 1.2 and libSDL_image bindings for the Racket programming language. I've tried to keep the sdl4racket API as close to the C API as possible, so you can use the SDL Wiki and all the tutorials out there. Of course the functions have more lispy names, for example:

**sdl-set-video-mode** instead of **SDL_SetVideoMode**

Integration with racket/draw is possible via the **bitmap->sdl-surface** function.

C Structures (e.g. all the SDL event structures) are converted to function closures. For example, a property like:

```c
event.key.keysym.sym
```

can be accesed in Racket this way:

```scheme
(((event 'EVENT) 'KEYSYM) 'SYM)
```

The main C event structure is actually a union. In sdl4racket a call to

```scheme
(event 'EVENT)
```


will return the correct event closure, according to the current event type.


This binding is not complete, however a reasonable part of SDL_Video
and the event handling is already working.


Variations from libSDL:

  * SDL_CDStatus is not available as sdl-cd-status but as sdl-cd-state in order to prevent a collision with the sdl-cd struct getter for 'status'
  * sdl-wait-event will internally use a polling loop to prevent the main thread from blocking.
    If blocking the main thread is ok for you (in sdl4racket timer callbacks are executed on the main thread)
    then you may use **sdl-wait-event-native** which gives you the original behavior.
