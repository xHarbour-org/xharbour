/*
 * $Id: wvtpaint.prg.prg,v 1.0 2004/07/01 17:40:00 vouchcac Exp $
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2004 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                           WvtPaint.prg
//
//            Routines to manage Wvt*Classes Gui Painting
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

#include 'wvtwin.ch'
#include 'common.ch'

//-------------------------------------------------------------------//

static paint_:= { { '', {} } }

//-------------------------------------------------------------------//
//
//        This function must have to be defined in your appls
//
// function Wvt_Paint()

   //  Call this function from this funtion
   //
   // WvtPaintObjects()

// return nil

//-------------------------------------------------------------------//

function WvtPaintObjects()
LOCAL i, lExe, nLeft, nRight, b, tlbr_, aBlocks, nBlocks

aBlocks := WvtSetPaint()

if ( nBlocks := len( aBlocks ) ) > 0
   tlbr_:= Wvt_GetPaintRect()

   for i := 1 to nBlocks
      lExe := .t.

      if aBlocks[ i,3 ] <> nil .and. !empty( aBlocks[ i,3 ] )
         //  Check parameters against tlbr_ depending upon the
         //  type of object and attributes contained in aAttr
         //
         do case
         case aBlocks[ i,3,1 ] == WVT_BLOCK_GRID_V
            b := aBlocks[ i,3,6 ]
            if len( b:aColumnsSep ) == 0
               lExe := .f.
            else
               nLeft  := b:aColumnsSep[ 1 ]
               nRight := b:aColumnsSep[ len( b:aColumnsSep ) ]
               if !( tlbr_[ 1 ] <= aBlocks[ i,3,4 ] .and. ; // top   < bottom
                     tlbr_[ 3 ] >= aBlocks[ i,3,2 ] .and. ; // bootm > top
                     tlbr_[ 2 ] <= nRight           .and. ; // left  < right
                     tlbr_[ 4 ] >= nLeft                  ) // right > left
                  lExe := .f.
               endif
            endif

         otherwise
            // If refreshing rectangle's top is less than objects' bottom
            // and left is less than objects' right
            //
            if !( tlbr_[ 1 ] <= aBlocks[ i,3,4 ] .and. ; // top   < bottom
                  tlbr_[ 3 ] >= aBlocks[ i,3,2 ] .and. ; // bootm > top
                  tlbr_[ 2 ] <= aBlocks[ i,3,5 ] .and. ; // left  < right
                  tlbr_[ 4 ] >= aBlocks[ i,3,3 ] )       // right > left
               lExe := .f.
            endif

         endcase
      endif

      if lExe
         eval( aBlocks[ i,2 ] )
      endif
   next
endif

return ( 0 )

//-------------------------------------------------------------------//

function WvtSetPaint( a_ )
local o
static s := {}

o := s

if a_ <> nil
   s := a_
endif

return o

//-------------------------------------------------------------------//

function SetPaint( cID, nAction, xData, aAttr )
local n, n1, oldData

if xData <> nil
   if ( n := ascan( paint_, { |e_| e_[ 1 ] == cID } ) ) > 0
      if ( n1 := ascan( paint_[ n,2 ], {|e_| e_[ 1 ] == nAction } ) ) > 0
         oldData := paint_[ n,2,n1,2 ]
         paint_[ n,2,n1,2 ] := xData
         paint_[ n,2,n1,3 ] := aAttr
      else
         aadd( paint_[ n,2 ], { nAction,xData,aAttr } )
      endif
   else
      aadd( paint_, { cID, {} } )
      n := len( paint_ )
      aadd( paint_[ n,2 ], { nAction, xData, aAttr } )
   endif
endif

return oldData

//-------------------------------------------------------------------//

function GetPaint( cID )
local n

if ( n := ascan( paint_, { |e_| e_[ 1 ] == cID } ) ) > 0
   return paint_[ n,2 ]
endif

return {}

//-------------------------------------------------------------------//

function DelPaint( cID, nAction )
local xData, n1, n

if ( n := ascan( paint_, { |e_| e_[ 1 ] == cID } ) ) > 0
   if ( n1 := ascan( paint_[ n,2 ], {|e_| e_[ 1 ] == nAction } ) ) > 0
      xData := paint_[ n,2,n1,2 ]
      paint_[ n,2,n1,2 ] := {|| .t. }
   endif
endif

return xData

//-------------------------------------------------------------------//

function PurgePaint( cID,lDummy )
local n, aPaint

DEFAULT lDummy TO .f.

if ( n := ascan( paint_, { |e_| e_[ 1 ] == cID } ) ) > 0
   aPaint := paint_[ n ]
   ADel( paint_, n )
   aSize( paint_, len( paint_ ) - 1 )
endif

if lDummy
   WvtSetPaint( {} )
endif

return ( aPaint )

//-------------------------------------------------------------------//

function InsertPaint( cID, aPaint, lSet )
local n

DEFAULT lSet TO .f.

if ( n := ascan( paint_, { |e_| e_[ 1 ] == cID } ) ) > 0
   paint_[ n ] := aPaint
else
   aadd( paint_, aPaint )
endif

if lSet
   WvtSetPaint( aPaint )
endif

return nil

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//


