/*
 * $Id: TSplash.prg,v 1.5 2002/11/05 21:39:58 what32 Exp $
 */

/*
 * xHarbour Project source code:
 *
 * Whoo.lib Splash Window CLASS
 *
 * Copyright 2002 Augusto Infante [augusto@2vias.com.ar]
 * www - http://www.xharbour.org
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
 */

#include "hbclass.ch"
#include "windows.ch"
#include "debug.ch"
#include "what32.ch"

CLASS TSplash FROM TForm
   DATA bitmap
   METHOD Create() CONSTRUCTOR
   METHOD OnPaint( hDC )  INLINE DrawBitmap( hDC, ::bitmap ), 0
   METHOD OnDestroy()     INLINE DeleteObject(::bitmap),NIL
   METHOD OnTimer( n )    INLINE IIF( n==1,::Destroy(),)
   METHOD OnLButtonDown() INLINE ::Destroy()
   METHOD OnRButtonDown() INLINE ::Destroy()
ENDCLASS

METHOD Create( oParent, cFile, nTimeOut ) CLASS TSplash
   local aRect,abRect
   DEFAULT nTimeOut TO 2000

   ::style   := WS_POPUP + WS_VISIBLE + WS_BORDER
   ::ExStyle := WS_EX_TOPMOST

   ::bitmap  := LoadImage( NIL, cFile, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE )

   aRect     := GetWindowRect(GetDesktopWindow())
   abRect    := GetBitmapSize( ::bitmap )

   ::FWidth  := abRect[1]
   ::FHeight := abRect[2]
   ::FLeft   := (aRect[3]/2)-(::FWidth/2)
   ::FTop    := (aRect[4]/2)-(::FHeight/2)

   super:Create( oParent )

   SetTimer( ::Handle, 1, nTimeOut )

   ProcessMessages()

return( self )
