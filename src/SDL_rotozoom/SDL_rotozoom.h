/* SDL_rotozoom.h */

#ifndef _SDL_rotozoom_h
#define _SDL_rotozoom_h

/* Set up for C function definitions, even when using C++ */
#ifdef __cplusplus
extern "C" {
#endif

#include <math.h>
#ifndef M_PI
 #define M_PI	3.141592654
#endif
#include <SDL/SDL.h>


/* ---- Defines */

#define SMOOTHING_OFF		0
#define SMOOTHING_ON		1

/* ---- Structures */

typedef struct tColorRGBA {
	Uint8 r;
	Uint8 g;
	Uint8 b;
	Uint8 a;
} tColorRGBA;

typedef struct tColorY {
	Uint8 y;
} tColorY;


/* ---- Prototypes */


/*:Return a newly created surface made from rotating and zooming the
   @var{src} surface.  @var{angle} is the rotation in degrees.  @var{zoom}
   is a scaling factor.  If @var{smooth} is 1 then the result surface is
   anti-aliased.  If @var{src} is not 8bit or 32bit RGBA/ABGR it will be
   converted into a 32bit RGBA format on the fly.  The result surface is
   32bit.
*/
SDL_Surface * rotozoomSurface (SDL_Surface *src,
                               double angle, double zoom,
                               int smooth);


/*:Return a newly created surface made from zooming the @var{src} surface.
   @var{zoomx} and @var{zoomy} are scaling factors for width and height.
   If @var{smooth} is 1 then the result surface is anti-aliased.  If
   @var{src} is not 8bit or 32bit RGBA/ABGR it will be converted into a
   32bit RGBA format on the fly.  The result surface is 32bit.
*/
SDL_Surface * zoomSurface (SDL_Surface *src,
                           double zoomx, double zoomy,
                           int smooth);


/* Ends C function definitions when using C++ */
#ifdef __cplusplus
};
#endif

#endif /* _SDL_rotozoom_h */

/* SDL_rotozoom.h ends here */
