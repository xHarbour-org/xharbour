/*
 * $Id$
 *
 */

/*
 * xHarbour Project source code:
 * Graphics API header file.
 *
 * Copyright 2004 Mauricio Abre <maurifull@datafull.com>
 * www - http://www.xharbour.org
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
 * WARNING: this file is also included in C code, so don't add xHarbour specific stuff,
 * or protect it under #ifndef's
 *
 */

#ifndef _GFX_CH_
#define _GFX_CH_

/*
 * NOTE: ACQUIRE / RELEASE screen pair must work same way DispBegin()/DispEnd() pair does
 *       (that is, with an internal counter), as lots of function may want to 'acquire/release' it.
 *
 *       However, a GT must properly manage its gfx output if the user didn't requested to acquire the
 *       screen, so this is under user choice.
 *       (the user just needs to know that it is not the same to aquire the screen, draw 100 lines, then
 *        release screen, than simply drawing 100 lines -as the GT will be acquiring/releasing the screen
 *        100 times, which will slow down things a lot-)
 *
 * Mauricio
 *
 */

/* Misc, internals */
#define GFX_ACQUIRESCREEN   1  // Some GTs may require that you 'aqcuire' the screen before doing gfx things
#define GFX_RELEASESCREEN   2  // Release a previouly 'aquired' screen
#define GFX_MAKECOLOR       3  // Calculate gfx color number based on RGBA values
/* Functions that affect drawing mode */
#define GFX_DRAWINGMODE    10
/* Drawing primitives */
#define GFX_GETPIXEL       21
#define GFX_PUTPIXEL       22
#define GFX_LINE           23
#define GFX_RECT           24
#define GFX_FILLEDRECT     25
#define GFX_CIRCLE         26
#define GFX_FILLEDCIRCLE   27
#define GFX_ELLIPSE        28
#define GFX_FILLEDELLIPSE  29
#define GFX_FLOODFILL      30

/* Drawing mode constants */
#define GFX_MODE_SOLID      1  // Solid mode, no translucency, no patterned primitives
#define GFX_MODE_XOR        2  // XOR with current screen contents
#define GFX_MODE_ALPHA      3  // Use alpha for transluced effect (SLOW)
/* TODO: add patterned mode drawings */

#ifndef _NO_GFX_TRANSLATES_

#translate GFXACQUIRESCREEN() => gfxPrimitive(GFX_ACQUIRESCREEN)
#translate GFXRELEASESCREEN() => gfxPrimitive(GFX_RELEASESCREEN)
#translate GFXMAKECOLOR(<nRed>, <nGreen>, <nBlue>[, <nAlpha>]) => gfxPrimitive(GFX_MAKECOLOR, <nRed>, <nGreen>, <nBlue>[, <nAlpha>])
#translate GFXDRAWINGMODE([<nMode>]) => gfxPrimitive(GFX_DRAWINGMODE[, <nMode>])
#translate GFXGETPIXEL(<nY>, <nX>) => gfxPrimitive(GFX_GETPIXEL, <nY>, <nX>)
#translate GFXPUTPIXEL(<nY>, <nX>, <nColor>) => gfxPrimitive(GFX_PUTPIXEL, <nY>, <nX>, <nColor>)
#translate GFXLINE(<nTop>, <nLeft>, <nBottom>, <nRight>, <nColor>) => gfxPrimitive(GFX_LINE, <nTop>, <nLeft>, <nBottom>, <nRight>, <nColor>)
#translate GFXRECT(<nTop>, <nLeft>, <nBottom>, <nRight>, <nColor>) => gfxPrimitive(GFX_RECT, <nTop>, <nLeft>, <nBottom>, <nRight>, <nColor>)
#translate GFXFILLEDRECT(<nTop>, <nLeft>, <nBottom>, <nRight>, <nColor>) => gfxPrimitive(GFX_FILLEDRECT, <nTop>, <nLeft>, <nBottom>, <nRight>, <nColor>)
#translate GFXCIRCLE(<nY>, <nX>, <nRadius>, <nColor>) => gfxPrimitive(GFX_CIRCLE, <nY>, <nX>, <nRadius>, <nColor>)
#translate GFXFILLEDCIRCLE(<nY>, <nX>, <nRadius>, <nColor>) => gfxPrimitive(GFX_FILLEDCIRCLE, <nY>, <nX>, <nRadius>, <nColor>)
#translate GFXELLIPSE(<nY>, <nX>, <nRadY>, <nRadX>, <nColor>) => gfxPrimitive(GFX_ELLIPSE, <nY>, <nX>, <nRadY>, <nRadX>, <nColor>)
#translate GFXFILLEDELLIPSE(<nY>, <nX>, <nRadY>, <nRadX>, <nColor>) => gfxPrimitive(GFX_FILLEDELLIPSE, <nY>, <nX>, <nRadY>, <nRadX>, <nColor>)
#translate GFXFLOODFILL(<nY>, <nX>, <nColor>) => gfxPrimitive(GFX_FLOODFILL, <nY>, <nX>, <nColor>)

#endif

#endif  /* _GFX_CH_ */
