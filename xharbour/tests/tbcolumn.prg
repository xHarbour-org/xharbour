/*
 * $Id: tbcolumn.prg,v 1.7 2003/08/08 22:31:50 walito Exp $
 */

/*
 * Harbour Project source code:
 * TBColumn Class
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * (C) 2003 Francesco Saverio Giudice <info@fsgiudice.com>
 *
 * 2004/01/04 - TBColumn Class revised to be full compatible
 *
 */

#define HB_CLS_NOTOBJECT  // this will avoid any default object to be inherited - for binary compatibility

#include "hbclass.ch"
#include "hbsetup.ch"
#include "common.ch"
#include "tbrowse.ch"

CLASS TBColumn

   // LEAVE DATA IN THIS ORDER TO BE BINARY COMPATIBLE!

   DATA  cargo                // User-definable variable
   DATA  width                // Column display width
   DATA  block                // Code block to retrieve data for the column
   DATA  defColor             // Array of numeric indexes into the color table
   DATA  colorBlock           // Code block that determines color of data items
   DATA  heading              // Column heading
   DATA  headSep              // Heading separator character
   DATA  colSep               // Column separator character
   DATA  footSep              // Footing separator character
   DATA  footing              // Column footing
   DATA  picture              // Column picture string

#ifdef HB_COMPAT_C53
   DATA  PreBlock             //
   DATA  PostBlock            //
   DATA  aSetStyle
#endif

   METHOD New(cHeading, bBlock)  CONSTRUCTOR

   //METHOD New(cHeading, bBlock)  INLINE TBColumn():New(cHeading, bBlock) // Constructor

#ifdef HB_COMPAT_C53
   METHOD SetStyle( nMode, lSetting )
#endif

ENDCLASS

METHOD New( cHeading, bBlock ) CLASS TBColumn

   local xRes, cType, nTokenPos := 0, nL

   DEFAULT cHeading TO ""

   // Assign Defaults
   ::block      := bBlock
   ::cargo      := NIL
   ::colorBlock := {|| ::defColor }
   ::colSep     := NIL
   ::defColor   := { 1, 2 }
   ::footing    := ""
   ::footSep    := NIL
   ::heading    := cHeading
   ::headSep    := NIL
   ::picture    := NIL
   ::width      := NIL

#ifdef HB_COMPAT_C53
   ::aSetStyle := ARRAY( 3 )

   ::aSetStyle[ TBC_READWRITE ] := .f.
   ::aSetStyle[ TBC_MOVE ]      := .f.
   ::aSetStyle[ TBC_SIZE ]      := .f.
#endif

RETURN Self


#ifdef HB_COMPAT_C53
METHOD SetStyle( nMode, lSetting ) CLASS TBColumn
  LOCAL lRet := .F.

  IF nMode > LEN( ::aSetStyle )
     RETURN .F.
  ENDIF

  lRet := ::aSetStyle[ nMode ]

  IF ISLOGICAL( lSetting )
     ::aSetStyle[ nMode ] := lSetting
  ENDIF

RETURN lRet
#endif

function TBColumnNew(cHeading, bBlock)
return TBColumn():New(cHeading, bBlock)
