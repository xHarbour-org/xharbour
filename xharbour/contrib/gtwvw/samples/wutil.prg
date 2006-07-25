/*
 * $Id$
 * WUTIL.PRG
 * A simple interface (middle module) between application and GTWVW.
 * This is a greatly simplified version of CSASOFT's WVWUTIL.PRG.
 */

#include "common.ch"
#include "hbclass.ch"
#include "wutil.ch"

STATIC s_aPObjList := {}

/*********************************************************/

CLASS wGUIObj
   DATA nWinNum                      //parent window's number
   DATA lVisible                     //is the object visible
   DATA nType                        //Type
   DATA cId                          //Id
   DATA nRow1, nCol1, nRow2, nCol2   //mouse object region
   DATA aOffTLBR                     //offset in pixels

ENDCLASS //wGUIObj

/*********************************************************/

CLASS wPaintObj FROM wGUIObj
   * image like wvtimage
   DATA cImage
   DATA lTransp

   METHOD New()
   METHOD Draw()
   METHOD Undraw()
   METHOD Hide()
   METHOD Show()
ENDCLASS //wPaintOBJ

METHOD New(nWinNum, nType, cId, nRow1, nCol1, nRow2, nCol2, aOffTLBR, lTransp) CLASS wPaintObj
   default aOffTLBR to {0,0,0,0}
   default lTransp to .f.

   ::nWinNum := nWinNum
   ::lVisible := .t.

   ::nType := nType
   ::cId := cId
   ::nRow1 := nRow1
   ::nCol1 := nCol1
   ::nRow2 := nRow2
   ::nCol2 := nCol2

   if !(valtype(aOffTLBR)=="A")
      aOffTLBR := {0,0,0,0}
   endif

   ::aOffTLBR := aclone(aOffTLBR)

   ::lTransp := lTransp
RETURN Self

METHOD Draw() CLASS wPaintObj
   if !::lVisible
      return NIL
   endif

   do case
      case ::nType==WPAINTOBJ_IMAGE
         if !empty(::cImage)
            WVW_DRAWIMAGE( ::nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ;
                           ::cImage, ::aOffTLBR, ::lTransp )
         endif

      otherwise
         * lBoxErrMessage()
   endcase

RETURN NIL  //DRAW()

METHOD Undraw() CLASS wPaintObj
* undraw the object
* normally this is called with ::lVisible == .f.,
* otherwise the object will be redrawn by WVW_PAINT
local cScreen
local nRow1, nCol1, nRow2, nCol2, nMaxRow, nMaxCol
   * to be safer, the area can be enlarged first
   nMaxRow := maxrow()
   nMaxCol := maxcol()

   do case
      case ::nType==WPAINTOBJ_LABEL
         nRow1 := ::nRow1
         nCol1 := ::nCol1
         nRow2 := ::nRow2
         nCol2 := ::nCol2
      otherwise
         nRow1 := max(::nRow1-1, 0)
         nCol1 := max(::nCol1-1, 0)
         nRow2 := min(::nRow2+1, nMaxRow)
         nCol2 := min(::nCol2+1, nMaxCol)
   endcase

   cScreen := savescreen(nRow1, nCol1, nRow2, nCol2)
   DISPBEGIN()
   restscreen(nRow1, nCol1, nRow2, nCol2, cScreen)
   DISPEND()
RETURN NIL //undraw()

METHOD Hide() CLASS wPaintObj
* temporarily hides the object
   ::lVisible := .f.
   ::Undraw()
RETURN NIL

METHOD Show() CLASS wPaintObj
* show the object
   ::lVisible := .t.
   ::draw()
RETURN NIL

/*********************************************************/

function wg_ResetWPaintObj( nWinNum, nObjNum, lStrict )
* clears all wPaint objects from window nWinNum
* if nObjNum specified, clears object >= nObjNum
local i
   default nObjNum to 0
   default lStrict to .f.

   do while len(s_aPObjList) < nWinNum+1
      aadd( s_aPObjList, {} )
   enddo

      asize(s_aPObjList[ nWinNum+1 ], nObjNum)

return .t.

function wg_AddWPaintObj( nWinNum, oWPaint, lStrict, nOperation )
* adds a WPaint object oWPaint into window nWinNum
* returns ::cId if successful. "" if failed.
local i
local nLen, aRect //20050720
   default lStrict to .f.
   default nOperation to WOBJ_ADD_OVERWRITE

   * simplified:
   nOperation := WOBJ_ADD_OVERWRITE

   * parameter checking...
   * ...

      * exist nType + cId ?
      i := ASCAN(s_aPObjList[ nWinNum+1 ], {|x| x:nType==oWPaint:nType .and. x:cId==oWPaint:cId})

   if i > 0
      * so we are about to overwrite now...
      //::Hide() is ideal, but it can be slow
      //let's do it only of user want strict/perfect operation
      if lStrict
         s_aPObjList[ nWinNum+1 ][i]:Hide()
      else
         s_aPObjList[ nWinNum+1 ][i]:lVisible := .f.
      endif
      s_aPObjList[ nWinNum+1 ][i] := oWPaint

   else
      aadd( s_aPObjList[ nWinNum+1 ], oWPaint )
   endif

   * if it is visible, draw it now!
   if oWPaint:lVisible
      oWPaint:draw()
   endif
return oWPaint:cId //20040811 was .t.

function wg_DelWPaintObj( nWinNum, nType, cId, lStrict )
* deletes a WPaint object oWPaint from window nWinNum
* returns number of object deleted.
*
*NOTE: if cId is NIL, delete all object of type nType
local i
local lDelAll := (cId == NIL)
local nDeleted := 0
local nLen
local cCurId
   default lStrict to .f.

   * is nType set?
   if nType < 1
      return nDeleted
   endif

   * exist nType + cId ?
   i := 1
   nLen := len(s_aPObjList[ nWinNum+1 ])
   do while i <= nLen
      if s_aPObjList[ nWinNum+1 ][i]:nType==nType .and.;
         (lDelAll .or. s_aPObjList[ nWinNum+1 ][i]:cId==cId)
         if lStrict
            s_aPObjList[ nWinNum+1 ][i]:Hide()
         else
            s_aPObjList[ nWinNum+1 ][i]:lVisible := .f.
         endif
         cCurId := s_aPObjList[ nWinNum+1 ][i]:cId
         adel(s_aPObjList[ nWinNum+1 ], i)
         asize(s_aPObjList[ nWinNum+1 ], --nLen)
         nDeleted++
      else
         i++
      endif
   enddo
return nDeleted

***************************************************************
* Supporting functions
***************************************************************

FUNCTION rgb( r,g,b )

RETURN ( r + ( g * 256 ) + ( b * 256 * 256 ) )

FUNCTION WVW_PAINT( nWinNum )
local aPendingRect, aTemp, nrowofs, ncolofs,;
      aOverlappedObj
  if len(s_aPObjList) >= nWinNum+1
     aPendingRect := WVW_GetPaintRect(nWinNum)

     * simple redraw, ignoring wpaint obj dependency with each other:
     aeval( s_aPObjList[ nWinNum+1 ], ;
             {|oWPaint| oWPaint:draw() ;
             };
          )
  endif
return 0
