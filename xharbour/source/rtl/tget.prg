/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Get Class
 *
 * Copyright 1999 Ignacio Ortiz de Z£niga <ignacio@fivetech.com>
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

#include "hbclass.ch"
#include "hblang.ch"

#include "color.ch"
#include "common.ch"
#include "setcurs.ch"
#include "getexit.ch"
#include "inkey.ch"
#include "button.ch"

/* TODO: :posInBuffer( <nRow>, <nCol> ) --> nPos
         Determines a position within the edit buffer based on screen
         coordinates.
         Xbase++ compatible method */

#define GET_CLR_UNSELECTED      0
#define GET_CLR_ENHANCED        1
#define GET_CLR_CAPTION         2
#define GET_CLR_ACCEL           3

//----------------------------------------------------------------------------//

CLASS GET

// Exported

   DATA BadDate
   DATA Block
   DATA Buffer
   DATA Cargo
   DATA Changed
   DATA CLEAR
   DATA Col
   DATA DecPos
   DATA ExitState
   DATA HasFocus
   DATA Minus
   DATA Name
   DATA Original
   DATA Pos
   DATA PostBlock
   DATA PreBlock
   DATA Reader
   DATA Rejected
   DATA Row
   DATA SubScript
   DATA TYPE
   DATA TypeOut

#ifdef HB_COMPAT_C53
   DATA Control
   DATA MESSAGE
   DATA Caption
   DATA nLastExitState
   DATA CapRow
   DATA CapCol
#endif

   METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec )

   METHOD Assign()
#ifdef HB_COMPAT_XPP
   MESSAGE _Assign METHOD Assign()
#endif

   ACCESS ColorSpec                 INLINE ::cColorSpec
   ASSIGN ColorSpec( cColorSpec )   INLINE ::SetColorSpec( cColorSpec )
//ACCESS Picture                   INLINE ::cPicture
   ACCESS PICTURE                   INLINE ::cOrigPicture
   ASSIGN Picture( cPicture )       INLINE ::SetPicture( cPicture )

   METHOD DISPLAY( lForced )
   METHOD ColorDisp( cColorSpec ) INLINE ::ColorSpec := cColorSpec, ::Display(), Self
   METHOD KillFocus()
   METHOD Reset()
   METHOD SetFocus()
   METHOD Undo()
   METHOD UnTransform( cBuffer )
   METHOD UpdateBuffer() INLINE  if( ::hasfocus, ( ::buffer := ::PutMask( ::VarGet() ), ::Display() ), ), Self
   METHOD VarGet()
   METHOD VarPut( xValue, lReFormat )

   METHOD End()
#ifdef HB_COMPAT_XPP
   MESSAGE _End METHOD End()
#endif

   METHOD Home()
   MESSAGE Left( lDisplay ) METHOD _Left( lDisplay )
   MESSAGE Right( lDisplay ) METHOD _Right( lDisplay )
   METHOD ToDecPos()
   METHOD WordLeft()
   METHOD WordRight()

   METHOD BackSpace( lDisplay )
   MESSAGE DELETE( lDisplay ) METHOD _Delete( lDisplay )
   METHOD DelEnd()
   METHOD DelLeft()
   METHOD DelRight()
   METHOD DelWordLeft()
   METHOD DelWordRight()

   METHOD Insert( cChar )
   METHOD OverStrike( cChar )

#ifdef HB_COMPAT_C53
   METHOD HitTest( mrow, mcol )
#endif

   ACCESS NTOL INLINE ::lNumToLeft
   MESSAGE NumToLeft() METHOD _NumToLeft()

   PROTECTED:           /*  P R O T E C T E D  */

   DATA cColorSpec            // Used only for METHOD ColorSpec
   DATA cPicture              // Used only for METHOD Picture
   DATA cOrigPicture          // Original picture supplied by user.

   METHOD SetColorSpec( cColorSpec )
   METHOD SetPicture( cPicture )

   METHOD PutMask( cBuffer, lEdit )
   METHOD HasScroll() INLINE ::nDispLen != ::nMaxLen

   ACCESS PassWord INLINE ::lPassword

   HIDDEN:              /*  H I D D E N  */

   DATA cPicMask, cPicFunc, nMaxLen, lEdit, lDecRev, lPicComplex
   DATA nDispLen, nDispPos, nOldPos, lCleanZero, cDelimit, nMaxEdit
   DATA lMinusPrinted, xVarGet
   DATA lDispLen INIT .F.
   DATA lUndo INIT .F.
   DATA lLeftJust INIT .F.         // to "@B" mask

   DATA lDispLenChanged  INIT .F.  // to "@X", "@C", "(" and ")" pictures.
   DATA nDispLenReduce   INIT 0    // idem.

   DATA lNumToLeft  INIT .F.       // to "@L" mask
   DATA lNeverDeleted INIT .T.

   DATA lPassWord INIT .F.         // true if "@P" picture is used.
   DATA cPassWordChar INIT "*"     // char to display password in buffer display.
   DATA nPassWordLen INIT 0        // password content length

   METHOD _NumToLeft()             // idem.
   METHOD StopMoveH()               // idem

   METHOD ParsePict( cPicture )
   METHOD DeleteAll()
   METHOD IsEditable( nPos )
   METHOD INPUT( cChar )
   METHOD FirstEditable( )
   METHOD LastEditable( )

   DATA lForceCentury                 // to "@2" or "@4" masks.
   DATA lCentury INIT __SetCentury()  // idem
   DATA lDecPos INIT .F.

ENDCLASS

//---------------------------------------------------------------------------//

METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec ) CLASS Get

   DEFAULT nRow       TO Row()
   DEFAULT nCol       TO Col()
   DEFAULT cVarName   TO ""
   DEFAULT bVarBlock  TO iif( HB_ISSTRING( cVarName ), MemVarBlock( cVarName ), NIL )
   DEFAULT cPicture   TO ""
   DEFAULT cColorSpec TO BuildGetColor( cColorSpec ) // SetColor()

   ::HasFocus       := .F.
   ::lEdit          := .F.
   ::BadDate        := .F.
   ::Changed        := .F.
   ::Clear          := .F.
   ::lCleanZero     := .F.
   ::lMinusPrinted  := .F.
   ::Minus          := .F.
   ::Rejected       := .F.
   ::TypeOut        := .F.

    /*
   ::DecPos         := NIL
   ::Pos            := NIL
   ::PostBlock      := NIL
   ::PreBlock       := NIL
   ::Reader         := NIL
   ::SubScript      := NIL
    */

//::ExitState      := 0 // NIL in Clipper
   ::nLastExitState := 0
   ::nOldPos        := 0

   ::nDispPos       := 1
   ::Block          := bVarBlock
   ::Col            := nCol
   ::Row            := nRow
   ::ColorSpec      := cColorSpec
   ::Picture        := cPicture
   ::cOrigPicture   := cPicture

   ::Name           := cVarName
   ::cDelimit       := iif( Set( _SET_DELIMITERS ), Set( _SET_DELIMCHARS ), NIL )

//::Original       := ::VarGet()
//::Type           := ValType( ::VarGet() )   // Must know the type at creation
   ::VarGet()   // Must know the type at creation

#ifdef HB_COMPAT_C53
//::CapCol         := ;
   ::CapRow         := 0
   ::Caption        := ""
#endif

   RETURN Self

//---------------------------------------------------------------------------//

METHOD ParsePict( cPicture ) CLASS Get

   LOCAL cChar, cMask
   LOCAL nAt
   LOCAL nFor
   LOCAL cNum := ""
   LOCAL nPos := At( "@", cPicture )
   LOCAL nLen, nDec
   LOCAL nMaxLen

   /* E.F. 2006/MAY/31 - Search by last occurrence of "@" into picture
    * to verify if picture has a function and avoid any char before it.
    * E.F. 2006/AUG/14 - Replaced RAT by AT to preserve others pictures, if any.
    */

//if Left( cPicture, 1 ) == "@"
//nPos := RAT("@", cPicture )
//nPos := AT("@", cPicture )
   IF nPos > 0

      IF nPos > 1
         // extracts any char before "@"
         cPicture := SubStr( cPicture, nPos )
         ::cPicture := cPicture
      ENDIF

      nAt := At( " ", cPicture )

      IF nAt == 0
         ::cPicFunc := Upper( cPicture )
         ::cPicMask := ""
      ELSE
         /* The first space after the function symbom is obligatory, but it's
            ignored in the mask. */
         ::cPicFunc := Upper( SubStr( cPicture, 1, nAt - 1 ) )

         /* E.F. 2006/JUN/06 Clipper doesn't extract left spaces of the mask
          * of any var type. For example: "@E     999.99", "@R   999.999"
          * ::cPicMask := LTrim( SubStr( cPicture, nAt + 1 ) )
          */
         ::cPicMask := SubStr( cPicture, nAt + 1 )
      ENDIF

      // Password picture.
      nPos := At( "P", ::cPicFunc )
      IF nPos > 0 .AND. ::Type == "C"
         ::lPassWord := .T.
         ::nPassWordLen := Len( RTrim( ::xVarGet ) )
         IF Len( ::cPicFunc ) > nPos .AND. ! Empty( ::cPicFunc[nPos+1] )
            ::cPassWordChar := ::cPicFunc[nPos+1]
         ENDIF
      ENDIF
      //      AnalyzePicture( @::cPicFunc )

      IF "D" IN ::cPicFunc

         ::cPicMask := Upper( Set( _SET_DATEFORMAT ) )
         ::cPicMask := StrTran( ::cPicmask, "Y", "9" )
         ::cPicMask := StrTran( ::cPicmask, "M", "9" )
         ::cPicMask := StrTran( ::cPicmask, "D", "9" )

      ENDIF

      IF "T" IN ::cPicFunc

         ::cPicMask := Upper( Set( _SET_TIMEFORMAT ) )
         ::cPicMask := StrTran( ::cPicmask, "Y", "9" )
         ::cPicMask := StrTran( ::cPicmask, "M", "9" )
         ::cPicMask := StrTran( ::cPicmask, "D", "9" )
         ::cPicMask := StrTran( ::cPicmask, "H", "9" )
         ::cPicMask := StrTran( ::cPicmask, "S", "9" )
         ::cPicMask := StrTran( ::cPicmask, "C", "9" )

      ENDIF

      IF ( nAt := At( "S", ::cPicFunc ) ) > 0

         FOR nFor := nAt + 1 TO Len( ::cPicFunc )
            IF ! IsDigit( SubStr( ::cPicFunc, nFor, 1 ) )
               EXIT
            ELSE
               cNum += SubStr( ::cPicFunc, nFor, 1 )
            ENDIF
         NEXT

         IF Val( cNum ) > 0
            ::nDispLen := Val( cNum )
            ::lDispLen := .T.
         ELSE
            ::lDispLen := .F.
         ENDIF
         ::cPicFunc := SubStr( ::cPicFunc, 1, nAt - 1 ) + SubStr( ::cPicFunc, nFor )
      ELSE
         ::lDispLen := .F.
      ENDIF

      // 2007/JAN/09 - E.F. - Protect ::lCleanzero value before extract "Z" from picture.
      IF !::lCleanZero
         ::lCleanZero := ( "Z" IN ::cPicFunc ) // Display zero as blanks.
      ENDIF

      // 2006/DEC/19 - E.F. - Extracted "Z" from ::cPicture
      if ::lCleanZero
         ::cPicFunc := StrTran( ::cPicFunc, "Z", "" )
         ::cPicture := StrTran( ::cPicture, "Z", "" )
      ENDIF

      // 2007/MAR/31 - E.F. - Set left-justified behaviour.
      IF !::lLeftJust
         ::lLeftJust := ( "B" IN ::cPicFunc )
      ENDIF

      if ::lLeftJust
         ::cPicFunc := StrTran( ::cPicFunc, "B", "" )
         ::cPicture := StrTran( ::cPicture, "B", "" )
      ENDIF

      /* 2008/FEB/15 - E.F. - Set NumToLeft @L behaviour. */
      IF ! ::lNumToLeft
         ::lNumToLeft := ::Type == "N" .AND. ( "L" IN ::cPicFunc )
      ENDIF

      /* 2008/FEB/15 - E.F. - Extract "@L" from mask. */
      if ::lNumToLeft .OR. "L" IN ::cPicFunc
         ::cPicFunc := StrTran( ::cPicFunc, "L", "" )
         ::cPicture := StrTran( ::cPicture, "L", "" )
      ENDIF

      /* 2008/FEB/16 - E.F. - Set force century behaviour. */
      ::lForceCentury := if( "4" IN ::cPicFunc .AND. ! ::lCentury, .T. , ;
         if( "2" IN ::cPicFunc .AND. ::lCentury, .F. , NIL ) )

      IF HB_ISLOGICAL( ::lForceCentury )
         ::cPicFunc := StrTran( ::cPicFunc, "4", "" )
         ::cPicture := StrTran( ::cPicture, "4", "" )
         ::cPicFunc := StrTran( ::cPicFunc, "2", "" )
         ::cPicture := StrTran( ::cPicture, "2", "" )
      ENDIF

      if ::cPicFunc == "@"
         ::cPicFunc := ""
      ENDIF

      // 2006/DEC/19 - E.F. - Extracted "@" from ::cPicture
      if ::cPicture == "@"
         ::cPicture := ""
      ENDIF

   ELSE
      ::cPicFunc   := ""
      ::cPicMask   := cPicture
      ::lCleanZero := .F.
      ::lDispLen   := .F.
   ENDIF

   if ::type == "D" .OR. ::type == "T"
      // ::cPicMask := LTrim( ::cPicMask )
      // avoid user date picture to force default date picture in
      // accordance with set date format. See below.
      ::cPicMask := ""
   ENDIF

// Comprobar si tiene la , y el . cambiado (Solo en Xbase++)

   ::lDecRev := "," IN Transform( 1.1, "9.9" )

// Generate default picture mask if not specified

   IF Empty( ::cPicMask ) .AND. !Empty( ::type )

      Switch ::type
      CASE "D"

         IF HB_ISLOGICAL( ::lForceCentury )
            __SetCentury( if( ::lForceCentury,"ON","OFF" ) )
         ENDIF

         ::cPicMask := Upper( Set( _SET_DATEFORMAT ) )

         IF HB_ISLOGICAL( ::lForceCentury )
            __SetCentury( if( ::lCentury,"ON","OFF" ) )
         ENDIF

         ::cPicMask := StrTran( ::cPicmask, "Y", "9" )
         ::cPicMask := StrTran( ::cPicmask, "M", "9" )
         ::cPicMask := StrTran( ::cPicmask, "D", "9" )

         EXIT

      CASE "T"
         IF HB_ISLOGICAL( ::lForceCentury )
            __SetCentury( if( ::lForceCentury,"ON","OFF" ) )
         ENDIF

         ::cPicMask := Upper( Set( _SET_DATEFORMAT ) + " " + Set( _SET_TIMEFORMAT ) )

         IF HB_ISLOGICAL( ::lForceCentury )
            __SetCentury( if( ::lCentury,"ON","OFF" ) )
         ENDIF

         ::cPicMask := StrTran( ::cPicmask, "Y", "9" )
         ::cPicMask := StrTran( ::cPicmask, "M", "9" )
         ::cPicMask := StrTran( ::cPicmask, "D", "9" )
         ::cPicMask := StrTran( ::cPicmask, "H", "9" )
         ::cPicMask := StrTran( ::cPicmask, "S", "9" )
         ::cPicMask := StrTran( ::cPicmask, "C", "9" )

         EXIT

      CASE "N"

         /* E.F. 2007/FEB/03 - Assign default mask format in accordance with
                               buffer lenght and dec pos. */
         DEFAULT ::xVarGet TO 0

         if ::nMaxLen != NIL .AND. ::DecPos != NIL

            /* 2007/MAY/20 - E.F. */
            nMaxLen := ::nMaxLen - iif( ::lDispLenChanged, ::nDispLenReduce, 0 )

            if ::DecPos < nMaxLen .AND. ::DecPos > 0
               cNum := Str( ::xVarGet, nMaxLen, nMaxLen - ::DecPos )
            ELSE
               cNum := Str( ::xVarGet, nMaxLen, 0 )
            ENDIF

         ELSE
            cNum := Str( ::xVarGet )
         ENDIF

         /* E.F. 2006/APRIL/19 - If ::xVarGet is negative and smaller than
          *  -99999999.99, the Str(::xVarGet) will return a string width
          * of 23, instead it's length, increasing the ::cPicMask length.
          * cNum := Str( ::xVarGet ) is not better way to do it.
          */
         IF ::xVarGet < 0
            nDec := Len( cNum ) - RAt( ".", cNum )
            IF nDec >= Len( cNum )
               nDec := 0
            ENDIF
            // 2006/DEC/24 - E.F. Fixed nLen value for negative numbers.
            //nLen := 10 + iif(nDec>0,1,0) + nDec
            nLen := iif( ::xVarGet < - 99999999.99, 10 + iif( nDec > 0,1,0 ) + nDec, Len( cNum ) )
            cNum := Str( ::xVarGet, nLen, nDec )
         ENDIF

         IF ( nAt := At( iif( ::lDecRev, ",", "." ), cNum ) ) > 0
            ::cPicMask := Replicate( '9', nAt - 1 ) + iif( ::lDecRev, ",", "." )
            ::cPicMask += Replicate( '9', Len( cNum ) - Len( ::cPicMask ) )
         ELSE
            ::cPicMask := Replicate( '9', Len( cNum ) )
         ENDIF
         EXIT

      CASE "C"
         If ::cPicFunc == "@9"
            ::cPicMask := Replicate( "9", Len( ::VarGet() ) )
            ::cPicFunc := ""
         ENDIF
         EXIT

      end
   ENDIF

// Comprobar si tiene caracteres embebidos no modificables en la plantilla

   ::lPicComplex := .F.

   IF ! Empty( ::cPicMask )
      FOR EACH cChar in ::cPicMask
         IF cChar == 'a'
            ::cPicMask[ HB_EnumIndex() ] := 'A'
         ELSEIF cChar == 'n'
            ::cPicMask[ HB_EnumIndex() ] := 'N'
         ELSEIF !( cChar IN "!ANX9#" )
            ::lPicComplex := .T.
            EXIT
         ENDIF
      NEXT
   ENDIF

//   if ::HasFocus  // TODO: Delete this line if the "if ::hasfocus" in ::Picture
//       is correct.
   if ::type == "N"
      ::decpos := At( iif( ::lDecRev .OR. "E" IN ::cPicFunc, ",", "." ), ;
         Transform( 1, if( Empty( ::cPicFunc ), "", ::cPicFunc + " " ) + ::cPicMask ) )
      if ::decpos == 0
         ::decpos := iif( ::buffer == NIL, NIL, Len( ::buffer ) + 1 )
      ENDIF

   ELSE
      ::decpos := NIL

      // 2006/DEC/11 - EF - changed ::cPicMask to uppercase if ::type is character.
      // 2007/NOV/06 - EF - toggle to uppercase only template chars of the mask.
      IF ::type == 'L'
         ::cPicMask := Upper( ::cPicMask )
      ELSEIF ::type == 'C'
         cMask := ::cPicMask
         ::cPicMask := ""
         FOR EACH cChar IN cMask
            IF cChar $ "alnxy"
               ::cPicMask += Upper( cChar )
            ELSE
               ::cPicMask += cChar
            ENDIF
         NEXT
      ENDIF
   ENDIF
//   endif

   return ::cPicFunc + ' ' + ::cPicMask

//---------------------------------------------------------------------------//

METHOD Assign() CLASS Get

   if ::hasfocus
      ::VarPut( ::unTransform(), .F. )
   ENDIF

   RETURN Self

//---------------------------------------------------------------------------//

METHOD DISPLAY( lForced ) CLASS Get

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL xBuffer
   LOCAL xVar
   LOCAL cCaption
   LOCAL cClrCap := hb_ColorIndex( ::ColorSpec, GET_CLR_CAPTION )
   LOCAL cClrAcc := hb_ColorIndex( ::ColorSpec, GET_CLR_ACCEL )
   //LOCAL lIsIntense := Set( _SET_INTENSITY )
   LOCAL nCol, cDisplay
   LOCAL nDispReduce := 0
   LOCAL cChar

   DEFAULT lForced TO .T.

   IF Empty( cClrCap )
      cClrCap := hb_ColorIndex( SetColor(), CLR_STANDARD )
   ENDIF

   IF Empty( cClrAcc )
      cClrAcc := hb_ColorIndex( SetColor(), CLR_BACKGROUND )
   ENDIF

   IF ::Buffer == NIL
      xVar      := ::VarGet() // In VarGet() is setting ::xVarGet needed to
      // ::Picture.
      ::Picture := ::cOrigPicture
      xBuffer   := ::PutMask( xVar, .F. )
   ELSE
      xBuffer   := ::Buffer
   ENDIF

   HBConsoleLock()

// Change display character on password picture.
   IF ::lPassWord .AND. ::Type == "C"
      FOR EACH cChar IN Left( xBuffer, ::nPasswordLen )
         xBuffer[HB_EnumIndex()] := ::cPassWordChar
      NEXT
   ENDIF

   /* E.F. 2006/MAY/23 - Display minus sign in the front of xBuffer value.
    * IF ! ::lMinusPrinted .AND. ! Empty( ::DecPos ) .AND. ::minus .AND. SubStr( xBuffer, ::DecPos - 1, 1 ) == "0"
    *   xBuffer := SubStr( xBuffer, 1, ::DecPos - 2 ) + "-." + SubStr( xBuffer, ::DecPos + 1 )
    */
   IF ::Type == "N" .AND. ! ::lMinusPrinted .AND. ::DecPos != NIL .AND. ::Minus
      xBuffer := PadL( StrTran( xBuffer,'-','' ), Len( xBuffer ) )
      IF ::DecPos > 0 .AND. ::DecPos < Len( xBuffer )
         xBuffer :=  PadL( '-' + LTrim( SubStr( xBuffer, 1, ::DecPos - 1 ) ) + "." + SubStr( xBuffer, ::DecPos + 1 ) , Len( xBuffer ) )
      ELSE
         xBuffer :=  PadL( '-' + LTrim( xBuffer ), Len( xBuffer ) )
      ENDIF
   ENDIF

   IF ::HasScroll() .AND. ::Pos != NIL
      IF ::nDispLen > 8
         ::nDispPos := Max( 1, Min( ::Pos - ::nDispLen + 4, ::nMaxLen - ::nDispLen + 1 ) )
      ELSE
         ::nDispPos := Max( 1, Min( ::Pos - Int( ::nDispLen / 2 ), ::nMaxLen - ::nDispLen + 1 ) )
      ENDIF
   ENDIF

   IF xBuffer != NIL .AND. ( lForced .OR. ( ::nDispPos != ::nOldPos ) )

      /* 2008/FEB/12 - EF - Reset buffer content when "@S" is used, after lose focus. */
      if ::HasScroll() .AND. !::HasFocus
         cDisplay := SubStr( xBuffer, 1, ::nDispLen )
      ELSE
         cDisplay := SubStr( xBuffer, ::nDispPos, ::nDispLen )
      ENDIF

      IF Len( cDisplay ) < ::nDispLen
/* 2007/MAY/18 - E.F. - Adjust display length
         cDisplay := Padr( cDisplay, ::nDispLen ) */
         cDisplay := PadR( cDisplay, Min( ::nDispLen,::nMaxLen ) )
      ENDIF

      // The get lost the focus. We need left-adjust the content buffer
      // if "@B" was used.
      //
      IF ::Type == "N" .AND. ::lLeftJust .AND. !::HasFocus
         cDisplay := PadR( LTrim( cDisplay ), ::nDispLen )
      ENDIF

      /* 2007/MAY/19 - E.F. - Reduce display length at "@X","@C","@(" or "@)" picture and final value conditions */
      IF ::Type == "N" .AND. !::lDispLenChanged .AND. !::HasFocus .AND. ;
            ::Changed .AND. !::Rejected .AND. ;
            ::ExitState != NIL .AND. xBuffer != NIL .AND. ;
            ::ExitState > GE_NOEXIT .AND. ::ExitState != GE_ESCAPE

/*    2007/SEP/24 - EF - Adjust buffer contents.
*         if ( "X" IN ::cPicFunc .and. ::Untransform( xBuffer ) >= 0 ) .or.;
*            ( "C" IN ::cPicFunc .and. ::Untransform( xBuffer ) < 0 )
*/
         IF ( "X" IN ::cPicFunc .AND. ::Untransform( xBuffer ) >= 0 ) .OR. ;
               ( "C" IN ::cPicFunc .AND. ::Untransform( xBuffer ) <= 0 )

            nDispReduce := 3

         ENDIF

         IF ( "(" IN ::cPicFunc .OR. ")" IN ::cPicFunc .AND. ::Untransform( xBuffer ) >= 0 )

            IF ")" IN ::cPicFunc
               cDisplay := StrTran( cDisplay, "(", " " )
               cDisplay := StrTran( cDisplay, ")", " " )
            ENDIF

            nDispReduce := 1

         ENDIF

      ENDIF


      /* 2007/SEP/14 - E.F. - Display buffer content only if not object as
                              ListBox, CheckCob, PushButton or RadioGroup.
      */
#ifdef HB_COMPAT_C53
      IF !HB_ISOBJECT( ::Control )
#endif
         DispOutAt( ::Row, ::Col + if( ::cDelimit == NIL, 0, 1 ), ;
            cDisplay, ;
            hb_ColorIndex( ::ColorSpec, iif( ::HasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ), .T. )

         IF nDispReduce > 0

            DispOutAt( ::Row, ::Col + if( ::cDelimit == NIL, 0, 1 ) + ::nDispLen, ;
               Space( nDispReduce ), hb_ColorIndex( ::ColorSpec, 4 ), .T. )
         ENDIF

         IF ! ( ::cDelimit == NIL )
            DispOutAt( ::Row, ::Col, SubStr( ::cDelimit, 1, 1 ), hb_ColorIndex( ::ColorSpec, iif( ::HasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ), .T. )
            DispOutAt( ::Row, ::Col + ::nDispLen + 1, SubStr( ::cDelimit, 2, 1 ), hb_ColorIndex( ::ColorSpec, iif( ::HasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ), .T. )
         ENDIF

#ifdef HB_COMPAT_C53
      ENDIF
#endif

   ENDIF

#ifdef HB_COMPAT_C53
   /* 2007/SEP/14 - EF - The Caption below is only for plain get caption,
                         no object get. This display it's own ones.
   */
   IF !HB_ISOBJECT( ::Control )
#endif

      IF !Empty( ::Caption )
         cCaption := StrTran( ::Caption, "&", "" )
         DispOutAt( ::Row, ::Col - Len( cCaption ) - 1, cCaption, cClrCap, .T. )
         IF "&" $ ::Caption
            DispOutAt( ::Row, ::Col - Len( cCaption ) - 2 + At( "&", ::Caption ), cCaption[At( "&", ::Caption )], cClrAcc, .T. )
         ENDIF
      ENDIF

#ifdef HB_COMPAT_C53
   ENDIF
#endif

   ::nOldPos := ::nDispPos

   IF ::Pos != NIL

      nCol := ::Col + ::Pos - ::nDispPos + if( ::cDelimit == NIL, 0, 1 )

      /* E.F. 2006/APRIL/13 - We need adjust cursor column position if user
       * has pressed a dot or comma key in the numeric var that haven't
       * decimal part.
       */
      IF ::Type == "N" .AND. ::hasfocus .AND. ( ::DecPos = NIL .OR. ::DecPos > ::nMaxLen ) .AND. ( LastKey() = Asc( ',' ) .OR. LastKey() = Asc( '.' ) )
         nCol := ::Col + ::nMaxLen - 1
         IF ::Col > 0 .AND. nCol < ::nMaxLen
            ::Left( .F. )
         ENDIF
      ENDIF

      /* 2008/FEB/15 - E.F. - Adjust col pos when @L is used. */
      if ::lNumtoLeft
         if ::DecPos != NIL .AND. ::DecPos > 0
            if ::Pos < ::DecPos
               nCol := ::Col + ::DecPos - 2
            ENDIF
         ELSE
            nCol := ::Col + ::nMaxLen - 1
         ENDIF

      ENDIF

      SetPos( ::Row, nCol )

   ENDIF

   SetCursor( nOldCursor )

   HBConsoleUnlock()

   RETURN Self

//---------------------------------------------------------------------------//

METHOD End() CLASS Get

   LOCAL nLastCharPos, nPos, nFor

   if ::HasFocus != nil .AND. ::HasFocus .AND. ! ::StopMoveH()

      /* 2006/JUN/03 - E.F. if cursor is already in last get value entered
       * position +1, then go to the end display position.
       */
      if ::Pos > ::FirstEditable() .AND. ::IsEditable( ::Pos ) .AND. ::Pos < ::nMaxEdit .AND. Empty( ::buffer[::Pos] )
         ::Pos := ::nMaxEdit
         ::TypeOut := .F.
         ::Clear   := .F.
         ::Display( .F. )
         RETURN Self
      ENDIF
      /**/

      nLastCharPos := Min( Len( RTrim( ::buffer ) ) + 1, ::nMaxEdit )

      if ::Pos != nLastCharPos
         nPos := nLastCharPos
      ELSE
         nPos := ::nMaxEdit
      ENDIF

      FOR nFor := nPos to ::FirstEditable() step - 1

         /* 2006/JUN/02 - E.F. Adjust cursor positon to valid end position */
         if ::IsEditable( nFor ) .AND. !Empty( ::buffer[nFor] )
            nFor++
            WHILE !::IsEditable( nFor ) .AND. nFor < ::nMaxEdit
               nFor++
            end
            ::Pos := Min( nFor, ::nMaxEdit )
            EXIT
         ENDIF
         /**/

      NEXT

      /* 2006/JUN/02 - E.F. if cursor reach first editable position, then go to
       * the end display position
       */
      if ::Pos == ::FirstEditable()
         ::Pos := ::nMaxEdit
      ENDIF
      /**/

      ::TypeOut := .F.
      ::Clear := .F.
      ::Display( .F. )
   ENDIF

   RETURN Self

//---------------------------------------------------------------------------//

METHOD Home() CLASS Get

   if ::HasFocus .AND. !::StopMoveH()
      ::Pos := ::FirstEditable()
      ::TypeOut := .F.
      ::Clear := .F.
      ::Display( .F. )
   ENDIF

   RETURN Self

//---------------------------------------------------------------------------//

METHOD Reset() CLASS Get

   if ::hasfocus
      ::buffer := ::PutMask( ::VarGet(), .F. )
      ::Pos := ::FirstEditable()
      ::TypeOut := .F.
      ::lNeverDeleted := .T.
   ENDIF

   RETURN Self

//---------------------------------------------------------------------------//

METHOD Undo() CLASS Get

   if ::hasfocus
      /* E.F. 2006/APRIL/14 - reset ::minus flag if ::xVarGet was
       * negative number but ::original value not.
       */
      IF ::Type == "N" .AND. ::Original != NIL .AND. ::Original >= 0
         ::minus := .F.
      ENDIF
      IF ::lPassword
         ::DeleteAll()
      ENDIF
      ::VarPut( ::Original, .T. )
      ::Pos := ::FirstEditable()
      ::UpdateBuffer() // 7/01/2004 9:44a.m. was ::Display()
      ::lUndo := .T.
   ENDIF

   RETURN Self

//---------------------------------------------------------------------------//

METHOD SetFocus() CLASS Get

   LOCAL xVarGet

   IF !::HasFocus

      xVarGet := ::VarGet() // In VarGet() is setting ::xVarGet

      ::hasfocus   := .T.
      ::rejected   := .F.
      ::typeout    := .F.

      /* FSG - 16/06/2006 - reset ::lUndo to false to avoid clearing of get in case of reuse of it */
      ::lUndo      := .F.

      ::Original   := xVarGet
      ::Type       := ValType( xVarGet )
      ::Picture    := ::cOrigPicture
      ::Buffer     := ::PutMask( xVarGet, .F. )
      ::Changed    := .F.
      ::Clear      := ( "K" IN ::cPicFunc .OR. ::type == "N" )
      //::nMaxLen    := IIF( ::buffer == NIL, 0, Len( ::buffer ) )
      ::lEdit      := .F.
      ::Pos        := ::FirstEditable()

      if ::Pos = 0
         ::TypeOut = .T.
      ENDIF

      if ::type == "N"
         ::decpos := At( iif( ::lDecRev .OR. "E" IN ::cPicFunc, ",", "." ), ::buffer )
         if ::decpos == 0
            ::decpos := iif( ::buffer == NIL, NIL, Len( ::buffer ) + 1 )
         ENDIF
         /* No, tested with clipper, ::minus .F. on ::setFocus of a negative variable get
         ::minus := ( xVarGet < 0 )
         */
         ::lMinusPrinted := ( xVarGet < 0 )
         ::minus := .F.
      ELSE
         ::decpos := NIL
         ::lMinusPrinted := ::minus  := .F.
      ENDIF


      if ::type == "D" .OR. ::type == "T"
         ::BadDate := IsBadDate( ::buffer, ::cPicFunc )
      ELSE
         ::BadDate := .F.
      ENDIF

      if ::buffer != NIL
         if ::nDispLen == NIL .OR. !::lDispLen
            ::nDispLen := ::nMaxLen
         ENDIF

         ::Display( .T. )
      ELSE
         ::Display()
      ENDIF
   ELSE
      ::Display()
   ENDIF

   RETURN Self

//---------------------------------------------------------------------------//

METHOD KillFocus() CLASS Get

   if ::lEdit
      ::Assign()
   ENDIF

   ::hasfocus := .F.
//   ::buffer   := ::PutMask( )
   ::buffer   := NIL
   ::Original := NIL
   ::Pos      := NIL

   ::Display()
   ::xVarGet  := NIL

   ::typeout := .F.  /* Clipper compatible */

   RETURN Self

//---------------------------------------------------------------------------//

METHOD VarPut( xValue, lReFormat ) CLASS Get

   LOCAL nCounter, aGetVar, aIndex, nDim

   DEFAULT lReFormat TO .T.

   if ::Block != nil .AND. xValue != nil

      IF ::SubScript == NIL
         Eval( ::Block, xValue )
      ELSE
         aIndex := ::SubScript
         nDim := Len( aIndex )
         aGetVar := Eval( ::Block )
         FOR nCounter := 1 TO nDim - 1
            aGetVar := aGetVar[ aIndex[ nCounter ] ]
         NEXT
         aGetVar[ aIndex[ nCounter ] ] := xValue
      ENDIF

      IF lReFormat
         IF !::hasfocus
            ::Original := xValue
         ENDIF
         ::Type    := ValType( xValue )
         ::xVarGet := xValue
         ::lEdit   := .F.
         ::Picture := ::cOrigPicture
      ENDIF
   ENDIF

   RETURN xValue

//---------------------------------------------------------------------------//

METHOD VarGet() CLASS Get

   LOCAL xVarGet, aIndex, nDim, aGetVar, nCounter
   LOCAL cVarGet, nDecPos, nLen, nDec, cMask, nRat

   IF ! HB_ISBLOCK( ::Block )
      ::xVarGet := NIL
      ::Type    := 'U'
      RETURN NIL
   ENDIF

   IF ::SubScript == NIL
      xVarGet := Eval( ::Block )
   ELSE
      aIndex := ::SubScript
      nDim := Len( aIndex )
      aGetVar := Eval( ::Block )

      FOR nCounter := 1 TO nDim - 1
         aGetVar := aGetVar[ aIndex[ nCounter ] ]
      NEXT

      xVarGet := aGetVar[ aIndex[ nCounter ] ]
   ENDIF

   ::Type := ValType( xVarGet )

   /* E.F. 2006/APR/12 - We need adjust get value in any circuntancies.
      E.F. 2006/MAY/05 - Added ::hasfocus to maintain ::original value,if
           we exit with ESC before start edit get. */
   IF ::hasfocus .AND. ::Type == "N" .AND. ::nMaxLen != NIL .AND. ;
         ::DecPos != NIL .AND. ::DecPos > 0 .AND. ::DecPos < ::nMaxLen

      nDecPos := ::DecPos

      IF ! Empty( ::cPicMask )
         /* 2008/MAR/04 - E.F. Discard any no number char after last digit of the mask. */

         nRat := RAt( "9", ::cPicMask )

         IF nRat = 0
            nRat := RAt( "N", ::cPicMask )
            IF nRat = 0
               nRat := RAt( "#", ::cPicMask )
            ENDIF
         ENDIF

         cMask :=  SubStr( ::cPicMask, 1, nRat )
         nLen := HowMuchNumeric( cMask ) + 1 + iif( ::minus, 1, 0 )
         nLen := Min( nLen, Len( cMask ) )
         nDec := Len( cMask ) - RAt( ".", cMask )
         IF nDec >= nLen
            nDec := 0
         ENDIF
      ELSEIF ! Empty( ::cPicture )
         /* 2008/MAR/04 - E.F. Discard any no number char after last digit of the pict. */

         nRat := RAt( "9", ::cPicture )

         IF nRat = 0
            nRat := RAt( "N", ::cPicture )
            IF nRat = 0
               nRat := RAt( "#", ::cPicture )
            ENDIF
         ENDIF

         cMask :=  SubStr( ::cPicture, 1, nRat )
         nLen := HowMuchNumeric( cMask ) + 1 + iif( ::minus, 1, 0 )
         nLen := Min( nLen, Len( cMask ) )
         nDec := Len( cMask ) - RAt( ".", cMask )
         IF nDec >= nLen
            nDec := 0
         ENDIF
      ELSE
         cVarGet := Str( xVarGet )
         nDec := Len( cVarGet ) - RAt( ".", cVarGet )
         IF nDec >= Len( cVarGet )
            nDec := 0
         ENDIF
         nLen := 10 + iif( nDec > 0, 1, 0 ) + nDec
      ENDIF
      cVarGet := Str( xVarGet, nLen, nDec )
      // Insert "0" before decimal dot, if empty.
      IF Empty( SubStr( cVarGet, 1, nDecPos - 1 ) )
         cVarGet := Stuff( cVarGet, nDecPos - 1, 1, "0" )
      ENDIF

      // Fill with zeros after decimal dot, if empty.
      IF Len( cVarGet ) > nDecPos .AND. Empty( SubStr( cVarGet, nDecPos + 1 ) )
         cVarGet := Stuff( cVarGet, nDecPos + 1, ::nMaxLen - nDecPos, Replicate( "0",::nMaxLen - nDecPos ) )
      ENDIF

      xVarGet := Val( cVarGet )

      // E.F. 2006/MAY/23 - Avoid minus flag if get value is zero.
      IF xVarGet == 0
         ::minus := .F.
      ENDIF

   ENDIF

   ::xVarGet := xVarGet

   RETURN xVarGet

//---------------------------------------------------------------------------//

METHOD Untransform( cBuffer ) CLASS Get

   LOCAL xValue, lUntransform
   LOCAL cChar
   LOCAL nFor, nPad := 0
   LOCAL cMaskDel := ""

   DEFAULT cBuffer TO ::buffer

   /*
    *if !::lEdit
    *  return ::VarGet()
    * endif
    */
   IF cBuffer == NIL
      RETURN NIL
   ENDIF

   Switch ::type
   CASE "C"

      IF "R" IN ::cPicFunc
         FOR EACH cChar in ::cPicMask
            cMaskDel += if( ( cChar IN "ANX9#!" ), " ", "X" )
         NEXT
         cBuffer := StrDel( cBuffer, cMaskDel )
         cBuffer += SubStr( ::xVarGet, Len( cBuffer ) + 1 )
      ELSE
         cBuffer += SubStr( ::xVarGet, ::nMaxLen + 1 )
      ENDIF

      xValue := cBuffer
      EXIT

   CASE "N"
      IF "X" IN ::cPicFunc
         IF Right( cBuffer, 2 ) == "DB"
            ::minus := .T.
         ENDIF
      ENDIF
      IF ! ::minus
         FOR nFor := 1 to ::nMaxLen
            if ::IsEditable( nFor ) .AND. IsDigit( SubStr( cBuffer, nFor, 1 ) )
               EXIT
            ENDIF
            IF SubStr( cBuffer, nFor, 1 ) IN "-(" .AND. SubStr( cBuffer, nFor, 1 ) != SubStr( ::cPicMask, nFor, 1 )
               ::minus := .T.
               EXIT
            ENDIF
         NEXT
      ENDIF

      /* 14/08/2004 - <maurilio.longo@libero.it>
       *              If there should be a decimal point (or comma) but this is missing
       *              re-add it. Clipper does it. A little sample is inside ChangeLog
       *              for this fix.
       */
      if ::decPos < Len( cBuffer ) .AND. Empty( cBuffer[ ::decPos ] ) .AND. if( ::lDecRev, "," , "." ) IN ::cPicture
         cBuffer[ ::decPos ] := if( "E" IN ::cPicFunc .OR. ::lDecRev, ",", "." )
      ENDIF

      cBuffer := Space( ::FirstEditable() - 1 ) + ;
         SubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 )

      IF "D" IN ::cPicFunc
         FOR nFor := ::FirstEditable( ) to ::LastEditable( )
            cMaskDel += if( ::IsEditable( nFor ), " ", "X" )
         NEXT
      ELSEIF "T" IN ::cPicFunc
         FOR nFor := ::FirstEditable( ) to ::LastEditable( )
            cMaskDel += if( ::IsEditable( nFor ), " ", "X" )
         NEXT
      ELSE
         IF "E" IN ::cPicFunc .OR. ::lDecRev
            cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + ;
               StrTran( SubStr( cBuffer, ::FirstEditable( ), ;
               ::LastEditable( ) - ::FirstEditable( ) + 1 ), ;
               ".", " " ) + ;
               SubStr( cBuffer, ::LastEditable() + 1 )

            cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + ;
               StrTran( SubStr( cBuffer, ::FirstEditable( ), ;
               ::LastEditable( ) - ::FirstEditable( ) + 1 ), ;
               ",", "." ) + ;
               SubStr( cBuffer, ::LastEditable() + 1 )

         ELSE
/*
 * 2005/07/30 - Eduardo Fernandes <modalsist@yahoo.com.br>
 *              The two IFs below was disabled because cause wrong get value
 *              if we type a numeric var greater than 999 in the picture
 *              "@R 9,999.99".
 *              Added: lUntransform := ( "R" IN ::cPicFunc ) instead.
 *
 *            if "R" IN ::cPicFunc
 *               lUntransform := Empty( ::buffer )
 *            endif
 *
 *            if ":" IN ::cPicture
 *               lUntransform := .T.
 *            endif
 */
            lUntransform := ( "R" IN ::cPicFunc )


            IF lUntransform
               cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + ;
                  StrTran( SubStr( cBuffer, ::FirstEditable( ), ;
                  ::LastEditable( ) - ::FirstEditable( ) + 1 ), ;
                  ",", " " ) + ;
                  SubStr( cBuffer, ::LastEditable() + 1 )
            ENDIF

         ENDIF

         /* Tony (ABC)   12/22/2005      3:53PM
          * The mask to delete has to be created according to the whole buffer!!
          * Because it is applied below as:   cBuffer := StrDel( cBuffer, cMaskDel )
          */
         FOR nFor := 1 TO Len( cBuffer )
            // for nFor := ::FirstEditable( ) to ::LastEditable( )
            cMaskDel += iif( ::IsEditable( nFor ) .OR. SubStr( cBuffer, nFor, 1 ) == ".", " ", "X" )
            if ::IsEditable( nFor ) .OR. SubStr( cBuffer, nFor, 1 ) == "."
               nPad ++
            ENDIF
         NEXT
      ENDIF

      /* Tony (ABC)   12/22/2005      3:53PM
       * I found that cMaskDel was shorter than cBuffer. Fixed bug.
       */
      cBuffer := StrDel( cBuffer, cMaskDel )

      cBuffer := StrTran( cBuffer, "$", " " )
      cBuffer := StrTran( cBuffer, "*", " " )
      cBuffer := StrTran( cBuffer, "-", " " )
      cBuffer := StrTran( cBuffer, "(", " " )
      cBuffer := StrTran( cBuffer, ")", " " )

      cBuffer := PadL( StrTran( cBuffer, " ", "" ), nPad )

      /* cBuffer := PadL( StrTran( cBuffer, " ", "" ), Len( cBuffer ) )
       * It replace left, right and medium spaces.
       * Don't replace for Alltrim()
       * xValue  := 0 + Val( cBuffer )    // 0 + ... avoids setting the
       */

/*    13/08/2004 - <maurilio.longo@libero.it>
*                  We're talking about a number and a few lines of code before this
*                  point ::minus is set to .T. if there is a "-(" inside number and
*                  a few lines after we make a Val(cBuffer), so, this buffer has to
*                  contain only numbers (and a "-" or ".", at max);
*                  that said, while there was this loop?
*                  I've replaced it with the iif( ::minus... ) which is shorter and
*                  evaluates correctly a buffer containing "  -.10"
*
*     if ::minus
*        For each cChar in cBuffer
*           if IsDigit( cChar )
*              nFor := HB_EnumIndex()
*              exit
*           endif
*        Next
*        nFor--
*        if nFor > 0
*           cBuffer := Left( cBuffer, nFor-1 ) + "-" + SubStr( cBuffer, nFor+1 )
*        else
*           cBuffer := "-" + cBuffer
*        endif
*     endif
*/

      /* 2006/JUN/07 - E.F. Adjust buffer string in case of var get without
       *  picture and empty buffer. Without this adjust, xValue returned will be
       *  0 instead 0.0, 0.00, 0.000 if decimal point exist.
       */
      IF Empty( ::cPicture ) .AND. Empty( StrTran( cBuffer,".","" ) )
         cBuffer := PadL( "0." +  Replicate( "0",::nMaxLen - ::Decpos ), ::nMaxLen )
      ENDIF

      xValue := iif( ::minus, - Val( cBuffer ), Val( cBuffer ) )
      EXIT

   CASE "L"
      cBuffer := Upper( cBuffer )
      xValue := "T" IN cBuffer .OR. "Y" IN cBuffer .OR. hb_langMessage( HB_LANG_ITEM_BASE_TEXT + 1 ) IN cBuffer
      EXIT

   CASE "D"

      IF HB_ISLOGICAL( ::lForceCentury )
         __SetCentury( if( ::lForceCentury,"ON","OFF" ) )
      ENDIF

      IF "E" IN ::cPicFunc
         //cBuffer := SubStr( cBuffer, 4, 3 ) + SubStr( cBuffer, 1, 3 ) + SubStr( cBuffer, 7 )
         cBuffer := InvertDwM( cBuffer )
      ENDIF

      xValue := CToD( cBuffer )

      IF HB_ISLOGICAL( ::lForceCentury )
         __SetCentury( if( ::lCentury,"ON","OFF" ) )
      ENDIF

      EXIT

   CASE "T"

      IF HB_ISLOGICAL( ::lForceCentury )
         __SetCentury( if( ::lForceCentury,"ON","OFF" ) )
      ENDIF

      IF "E" IN ::cPicFunc
         //cBuffer := SubStr( cBuffer, 4, 3 ) + SubStr( cBuffer, 1, 3 ) + SubStr( cBuffer, 7 )
         cBuffer := InvertDwM( cBuffer )
      ENDIF

      xValue := CToT( cBuffer )

      IF HB_ISLOGICAL( ::lForceCentury )
         __SetCentury( if( ::lCentury,"ON","OFF" ) )
      ENDIF

      EXIT

   end

   RETURN xValue

//---------------------------------------------------------------------------//

METHOD overstrike( cChar ) CLASS Get

   if ::Type == "N" .AND. ! ::lEdit .AND. ::Clear
      ::Pos := ::FirstEditable()
   ENDIF

   if ::Pos > ::nMaxEdit
      ::Rejected := .T.
      RETURN Self
   ENDIF

   cChar := ::Input( cChar )

   IF cChar == ""
      ::Rejected := .T.
      RETURN Self
   ELSE
      ::Rejected := .F.
   ENDIF


   if ::lUndo .OR. ( ::CLEAR .AND. ::Pos == ::FirstEditable() ) .OR. ( ::lPassword .AND. ::nPasswordLen > ::Pos )
      ::DeleteAll()
      ::Clear := .F.
      ::lEdit := .F.
   ENDIF

   IF ! ::lEdit
      ::lEdit  := .T.
      //::buffer := ::PutMask( ::VarGet(), .t. )
   ENDIF

   if ::Pos == 0
      ::Pos = 1
   ENDIF

   DO WHILE ! ::IsEditable( ::Pos ) .AND. ::Pos <= ::nMaxEdit
      ::Pos++
   ENDDO

   if ::Pos > ::nMaxEdit
      ::Pos := ::FirstEditable()
   ENDIF

   ::buffer := SubStr( ::buffer, 1, ::Pos - 1 ) + cChar + SubStr( ::buffer, ::Pos + 1 )

// To conform UPDATED() behaviour with that of Clipper
   ::Changed := .T.

// UPDATED() function previously did not return .T. even if a key press is
// accepted.
//   ::Changed := ValType( ::Original ) != ValType( ::unTransform() ) .or. ;
//                !( ::unTransform() == ::Original )
   ::Right( .F. )

   if ::lPassword
      ::nPasswordLen := Min( ::nPasswordLen + 1, ::nMaxLen )
   ENDIF

   if ::type == "D" .OR. ::type == "T"
      ::BadDate := IsBadDate( ::buffer, ::cPicFunc )
   ELSE
      ::BadDate := .F.
   ENDIF

   ::Display()

   RETURN Self

//---------------------------------------------------------------------------//

METHOD Insert( cChar ) CLASS Get

   LOCAL n
   LOCAL nMaxEdit := ::nMaxEdit

   if ::Type == "N" .AND. ! ::lEdit .AND. ::Clear
      ::Pos := ::FirstEditable()
   ENDIF

   if ::Pos > ::nMaxEdit
      ::Rejected := .T.
      RETURN Self
   ENDIF

   cChar := ::Input( cChar )

   IF cChar == ""
      ::Rejected := .T.
      RETURN Self
   ELSE
      ::Rejected := .F.
   ENDIF

   if ::lUndo .OR. ( ::CLEAR .AND. ::Pos == ::FirstEditable() ) .OR. ( ::lPassword .AND. ::nPasswordLen > ::Pos )
      ::DeleteAll()
      ::Clear := .F.
      ::lEdit := .F.
   ENDIF

   IF ! ::lEdit
      ::lEdit  := .T.
      //::buffer := ::PutMask( ::VarGet(), .t. )
   ENDIF

   if ::Pos == 0
      ::Pos = 1
   ENDIF

   DO WHILE ! ::IsEditable( ::Pos ) .AND. ::Pos <= ::nMaxEdit
      ::Pos++
   ENDDO

   if ::Pos > ::nMaxEdit
      ::Pos := ::FirstEditable()
   ENDIF

   if ::lPicComplex
      // Calculating diferent nMaxEdit for ::lPicComplex

      FOR n := ::Pos TO nMaxEdit
         IF !::IsEditable( n )
            EXIT
         ENDIF
      NEXT
      nMaxEdit := n
      ::buffer := Left( SubStr( ::buffer, 1, ::Pos - 1 ) + cChar + ;
         SubStr( ::buffer, ::Pos, nMaxEdit - 1 - ::Pos ) + ;
         SubStr( ::buffer, nMaxEdit ), ::nMaxLen )
   ELSE
      ::buffer := Left( SubStr( ::buffer, 1, ::Pos - 1 ) + cChar + SubStr( ::buffer, ::Pos ), ::nMaxEdit )
   ENDIF

// To conform UPDATED() behaviour with that of Clipper
   ::Changed := .T.

// UPDATED() function previously did not return .T. even if a key press is
// accepted.
//   ::Changed := ValType( ::Original ) != ValType( ::unTransform() ) .or. ;
//                !( ::unTransform() == ::Original )
   ::Right( .F. )

   if ::lPassword
      ::nPasswordLen := Min( ::nPasswordLen + 1, ::nMaxLen )
   ENDIF

   if ::type == "D" .OR. ::type == "T"
      ::BadDate := IsBadDate( ::buffer, ::cPicFunc )
   ELSE
      ::BadDate := .F.
   ENDIF

   ::Display()

   RETURN Self

//---------------------------------------------------------------------------//

METHOD _Right( lDisplay ) CLASS Get

   LOCAL nPos

   DEFAULT lDisplay TO .T.

   IF ! ::hasfocus
      RETURN Self
   ENDIF

   IF ! ::StopMoveH()

      ::TypeOut := .F.
      ::Clear   := .F.

      if ::Pos == ::nMaxEdit
         ::TypeOut := .T.
         RETURN Self
      ENDIF

      nPos := ::Pos + 1

      DO WHILE ! ::IsEditable( nPos ) .AND. nPos <= ::nMaxEdit
         nPos++
      ENDDO

      IF nPos <= ::nMaxEdit
         ::Pos := nPos
      ELSE
         ::TypeOut := .T.
      ENDIF

      IF lDisplay
         ::Display( .F. )
      ENDIF

      if ::DecPos != NIL .AND. ::Pos > ::DecPos
         ::lDecPos := .T.
      ENDIF

   ENDIF

   RETURN Self

//---------------------------------------------------------------------------//

METHOD _Left( lDisplay ) CLASS Get

   LOCAL nPos

   DEFAULT lDisplay TO .T.

   IF ! ::hasfocus
      RETURN Self
   ENDIF

   IF ! ::StopMoveH()

      ::TypeOut := .F.
      ::Clear   := .F.

      if ::Pos == ::FirstEditable()
         ::TypeOut := .T.
         RETURN Self
      ENDIF

      nPos := ::Pos - 1

      DO WHILE ! ::IsEditable( nPos ) .AND. nPos > 0
         nPos--
      ENDDO

      IF nPos > 0
         ::Pos := nPos
      ELSE
         ::TypeOut := .T.
      ENDIF

      IF lDisplay
         ::Display( .F. )
      ENDIF

   ENDIF

   RETURN Self

//---------------------------------------------------------------------------//

METHOD WordLeft() CLASS Get

   LOCAL nPos, nFirstEditable

   IF ! ::hasfocus
      RETURN Self
   ENDIF

   IF ! ::StopMoveH()

      ::TypeOut := .F.
      ::Clear   := .F.

      nFirstEditable := ::FirstEditable()

      if ::Pos == nFirstEditable
         ::TypeOut := .T.
         RETURN Self
      ENDIF

      nPos := ::Pos

      DO WHILE nPos > nFirstEditable .AND. ( SubStr( ::buffer, nPos - 1, 1 ) == " " .OR. ! ::IsEditable( nPos - 1 ) )
         nPos--
      ENDDO

      DO WHILE nPos > nFirstEditable .AND. ! ( SubStr( ::buffer, nPos - 1, 1 ) == " " .OR. ! ::IsEditable( nPos - 1 ) )
         nPos--
      ENDDO

      ::Pos := nPos

      ::Display( .F. )

   ENDIF

   RETURN Self

//---------------------------------------------------------------------------//

METHOD WordRight() CLASS Get

   LOCAL nPos, nLastEditable

   IF ! ::hasfocus
      RETURN Self
   ENDIF

   IF ! ::StopMoveH()

      ::TypeOut := .F.
      ::Clear   := .F.

      nLastEditable := ::LastEditable()

      if ::Pos == nLastEditable
         ::TypeOut := .T.
         RETURN Self
      ENDIF

      nPos := ::Pos

      DO WHILE nPos < nLastEditable .AND. ! ( SubStr( ::buffer, nPos, 1 ) == " " .OR. ! ::IsEditable( nPos ) )
         nPos++
      ENDDO

      DO WHILE nPos < nLastEditable .AND. ( SubStr( ::buffer, nPos, 1 ) == " " .OR. ! ::IsEditable( nPos ) )
         nPos++
      ENDDO

      ::Pos := nPos

      ::Display( .F. )

   ENDIF

   RETURN Self

//---------------------------------------------------------------------------//

METHOD ToDecPos() CLASS Get

   LOCAL xBuffer

   IF ! ::HasFocus .OR. ::DecPos == NIL
      RETURN Self
   ENDIF

   /* 2006/JUN/05 - E.F. Delete all only if we press dot or comma in a numeric
      var. */
   if ::Pos == ::FirstEditable() .AND. ::Type == 'N' .AND. ;
         ( LastKey() = Asc( '.' ) .OR. LastKey() == Asc( ',' ) )
      ::DeleteAll()
   ENDIF

   ::TypeOut := .F.
   ::Clear   := .F.
   ::lEdit   := .T.
   xBuffer   := ::UnTransform()
   ::Buffer  := ::PutMask( xBuffer, .F. )
   ::Pos     := ::DecPos + 1
   ::lDecPos := .T.

   ::Display( .T. )

   /* E.F. 2006/APRIL/12 - Re-entrance of buffer value to update ::xVarGet
    * into VarGet()
    */
   ::VarPut( xBuffer, .T. )

   RETURN Self

//---------------------------------------------------------------------------//

METHOD IsEditable( nPos ) CLASS Get

   LOCAL cChar

   IF Empty( ::cPicMask )
      RETURN .T.
   ENDIF

   if ::nMaxEdit == NIL .OR. nPos > ::nMaxEdit
      RETURN .F.
   ENDIF

   cChar := Upper( SubStr( ::cPicMask, nPos, 1 ) )

   Switch ::type
   CASE "C"
      RETURN cChar IN "!ANXLY9#"
   CASE "N"
      RETURN cChar IN "9#$*"
   CASE "D"
   CASE "T"
      RETURN ( cChar == "9" )
   CASE "L"
      RETURN cChar IN "LY"
   end

   RETURN .F.

//---------------------------------------------------------------------------//

METHOD INPUT( cChar ) CLASS Get

   LOCAL cPic

   Switch ::type
   CASE "N"

      Switch cChar
      CASE "-"
         /* 2006/JUN/07 - E.F. The minus sign can not be write in any place,
          *               instead, can be write up to 1 position before
          *               decimal point. Example: -.99 not -99 or 9.-9 or
          *               9.9-
          *               Clipper allow it, but it's wrong.
          */
         if ::decpos > 0 .AND. ::Pos > ::decpos
            ::ToDecPos()
            RETURN ""
         ELSE
            ::minus := .T.
         ENDIF
         EXIT

      CASE "."
      CASE ","
         ::ToDecPos()
         RETURN ""

      CASE "0"
      CASE "1"
      CASE "2"
      CASE "3"
      CASE "4"
      CASE "5"
      CASE "6"
      CASE "7"
      CASE "8"
      CASE "9"
         EXIT

         DEFAULT
         RETURN ""

      end
      EXIT

   CASE "D"
   CASE "T"

      IF !( cChar IN "0123456789" )
         RETURN ""
      ENDIF
      EXIT

   CASE "L"

      IF !( Upper( cChar ) IN "YNTF" + hb_langMessage( HB_LANG_ITEM_BASE_TEXT + 1 ) + hb_langMessage( HB_LANG_ITEM_BASE_TEXT + 2 ) )
         RETURN ""
      ENDIF
      DO CASE
      CASE Upper( cChar ) == hb_langMessage( HB_LANG_ITEM_BASE_TEXT + 1 )
         cChar := "Y"
      CASE Upper( cChar ) == hb_langMessage( HB_LANG_ITEM_BASE_TEXT + 2 )
         cChar := "N"
      ENDCASE
      EXIT

   end

   IF ! Empty( ::cPicFunc )
      // Dirty HACK for solve the the "@RE" TRANSFORMation... "N" -> "  /N  /  "
      IF "R" IN ::cPicFunc .AND. "E" IN ::cPicFunc
         cChar := SubStr( Transform( cChar, ::cPicFunc ), 4, 1 ) // Needed for @RE
      ELSE
         cChar := Left( Transform( cChar, ::cPicFunc ), 1 ) // Left needed for @D
      ENDIF
   ENDIF

   IF ! Empty( ::cPicMask )
      cPic  := SubStr( ::cPicMask, ::Pos, 1 )

      //      cChar := Transform( cChar, cPic )
      // Above line eliminated because some get picture template symbols for
      // numeric input not work in text input. eg: $ and *

      Switch cPic
      CASE "A"
         IF ! IsAlpha( cChar )
            cChar := ""
         ENDIF
         EXIT

      CASE "N"
         IF ! IsAlpha( cChar ) .AND. ! IsDigit( cChar )
            cChar := ""
         ENDIF
         EXIT

      CASE "9"
         IF ! IsDigit( cChar )
            IF cChar != "-" .OR. ::Type != "N"
               cChar := ""
            ENDIF
         ENDIF
         EXIT

      CASE "#"
         IF ! IsDigit( cChar ) .AND. !( cChar == " " ) .AND. !( cChar IN ".+-" )
            cChar := ""
         ENDIF
         EXIT

      CASE "L"
         IF !( Upper( cChar ) IN "YNTF" + ;
               hb_langMessage( HB_LANG_ITEM_BASE_TEXT + 1 ) + ;
               hb_langMessage( HB_LANG_ITEM_BASE_TEXT + 2 ) )
            cChar := ""
         ENDIF
         EXIT

      CASE "Y"
         IF !( Upper( cChar ) IN "YN" )
            cChar := ""
         ENDIF
         EXIT

      CASE "$"
      CASE "*"
         if ::type == "N"
            IF ! IsDigit( cChar ) .AND. cChar != "-"
               cChar := ""
            ENDIF
         ELSE
            cChar := Transform( cChar, cPic )
         ENDIF
         EXIT

         DEFAULT
         cChar := Transform( cChar, cPic )
      end
   ENDIF

   RETURN cChar

//---------------------------------------------------------------------------//

METHOD PutMask( xValue, lEdit ) CLASS Get

   LOCAL cChar
   LOCAL cBuffer
   LOCAL cPicFunc
   LOCAL cMask

   DEFAULT lEdit  TO ::HasFocus
   DEFAULT xValue TO ::UnTransform()
   DEFAULT xValue TO ::xVarGet
   DEFAULT xValue TO ::VarGet()

   IF ::Type == NIL
      ::Type := ValType( xValue )
      ::Picture := ::cOrigPicture
   ENDIF

   cPicFunc := ::cPicFunc
   cMask    := ::cPicMask

   DEFAULT cMask  TO ""

   IF xValue == NIL .OR. ValType( xValue ) IN "AB"
      ::nMaxLen := 0
      RETURN NIL
   ENDIF

   if ::HasFocus
      cPicFunc := StrTran( cPicfunc, "B", "" )
      IF cPicFunc == "@"
         cPicFunc := ""
      ENDIF
   ENDIF
   IF lEdit .AND. ::lEdit
      IF ( "*" IN cMask ) .OR. ( "$" IN cMask )
         cMask := StrTran( StrTran( cMask, "*", "9" ), "$", "9" )
      ENDIF
   ENDIF

   IF ( ::Type == "D" .OR. ::Type == "T" ) .AND. HB_ISLOGICAL( ::lForceCentury )
      __SetCentury( if( ::lForceCentury,"ON","OFF" ) )
   ENDIF

   cBuffer := Transform( xValue, if( Empty( cPicFunc ), if( ::lCleanZero .AND. !::HasFocus, "@Z ", "" ), cPicFunc + if( ::lCleanZero .AND. !::HasFocus, "Z", "" ) + " " ) + cMask )

   IF ( ::Type == "D" .OR. ::Type == "T" ) .AND. HB_ISLOGICAL( ::lForceCentury )
      __SetCentury( if( ::lCentury,"ON","OFF" ) )
   ENDIF

   /* 2007/MAY/20 - E.F. */
   ::lDispLenChanged := ( ::Type == "N" .AND. ( "DB" IN cBuffer .OR. "CR" IN cBuffer .OR. "(" IN cBuffer .OR. ")" IN cBuffer ) )

   if ::type == "N"

      if  ::lDispLenChanged

         IF "(" IN cPicFunc .OR. ")" IN cPicFunc

            ::nDispLenReduce := 1

            IF xValue >= 0
               cBuffer := PadR( cBuffer, iif( ::nMaxLen != NIL, ::nMaxLen, iif(cMask != NIL,Len(cMask ),10 ) ) )
            ENDIF

         ELSEIF "C" IN cPicFunc

            ::nDispLenReduce := 3

            IF xValue < 0
               cBuffer := PadR( cBuffer, iif( ::nMaxLen != NIL, ::nMaxLen, iif(cMask != NIL,Len(cMask ),10 ) ) )
            ENDIF

         ELSEIF "X" IN cPicFunc

            ::nDispLenReduce := 3

            IF xValue >= 0
               cBuffer := PadR( cBuffer, iif( ::nMaxLen != NIL, ::nMaxLen, iif(cMask != NIL,Len(cMask ),10 ) ) )
            ENDIF

         ENDIF

      ENDIF

      ::lMinusPrinted := ( xValue < 0 )

   ENDIF

   ::nMaxLen  := Len( cBuffer )
   ::nMaxEdit := ::nMaxLen

   if ::nDispLen == NIL .OR. !::lDispLen
      ::nDispLen := ::nMaxLen
   ENDIF

   IF lEdit .AND. ::type == "N" .AND. ! Empty( cMask )

      IF "E" IN cPicFunc
         cMask := Left( cMask, ::FirstEditable() - 1 ) + StrTran( SubStr( cMask, ::FirstEditable( ), ::LastEditable( ) - ::FirstEditable( ) + 1 ), ",", Chr( 1 ) ) + SubStr( cMask, ::LastEditable() + 1 )
         cMask := Left( cMask, ::FirstEditable() - 1 ) + StrTran( SubStr( cMask, ::FirstEditable( ), ::LastEditable( ) - ::FirstEditable( ) + 1 ), ".",      "," ) + SubStr( cMask, ::LastEditable() + 1 )
         cMask := Left( cMask, ::FirstEditable() - 1 ) + StrTran( SubStr( cMask, ::FirstEditable( ), ::LastEditable( ) - ::FirstEditable( ) + 1 ), Chr( 1 ), "." ) + SubStr( cMask, ::LastEditable() + 1 )
      ENDIF

      FOR EACH cChar in cMask
         IF cChar IN ",." .AND. SubStr( cBuffer, HB_EnumIndex(), 1 ) IN ",."
            cBuffer := SubStr( cBuffer, 1, HB_EnumIndex() - 1 ) + cChar + SubStr( cBuffer, HB_EnumIndex() + 1 )
         ENDIF
      NEXT

      if ::lEdit .AND. Empty( xValue )
         cBuffer := StrTran( cBuffer, "0", " " )
      ENDIF

      if ::lDecRev
         cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + StrTran( SubStr( cBuffer, ::FirstEditable( ), ::LastEditable( ) - ::FirstEditable( ) + 1 ), ",", Chr( 1 ) ) + SubStr( cBuffer, ::LastEditable() + 1 )
         cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + StrTran( SubStr( cBuffer, ::FirstEditable( ), ::LastEditable( ) - ::FirstEditable( ) + 1 ), ".",      "," ) + SubStr( cBuffer, ::LastEditable() + 1 )
         cBuffer := Left( cBuffer, ::FirstEditable() - 1 ) + StrTran( SubStr( cBuffer, ::FirstEditable( ), ::LastEditable( ) - ::FirstEditable( ) + 1 ), Chr( 1 ), "." ) + SubStr( cBuffer, ::LastEditable() + 1 )
      ENDIF

   ENDIF

   /* 2007/MAY/20 - E.F. */
   if ::type == "N"

      if ::lDispLenChanged

         IF ( "(" IN ::cPicFunc .OR. ")" IN ::cPicFunc .OR. ;
               "C" IN ::cPicFunc .OR. "X" IN ::cPicFunc )

            ::nMaxEdit := ::nMaxLen - ::nDispLenReduce

         ENDIF

      ENDIF

   /* 2006/OCT/28 - E.F. - fixed a bug when we set century on after set date
    *                      and use get with picture "@E" in date variable.
    * If ::type == "D" .and. ::BadDate .and. ::Buffer != nil
    *    cBuffer := ::Buffer
    * Endif
    */
   elseif ::type == "D" .OR. ::type == "T"
      If ::BadDate .AND. ::Buffer != nil
         cBuffer := ::Buffer
      Elseif ::Buffer == NIL .AND. "E" IN ::cPicFunc
         IF HB_ISLOGICAL( ::lForceCentury )
            __SetCentury( if( ::lForceCentury,"ON","OFF" ) )
         ENDIF
         cBuffer := InvertDwM( if ( ::type == "D", DToC( xValue ), TtoC( xValue ) ) )
         IF HB_ISLOGICAL( ::lForceCentury )
            __SetCentury( if( ::lCentury,"ON","OFF" ) )
         ENDIF

      ENDIF
   ENDIF

   RETURN cBuffer

//---------------------------------------------------------------------------//

METHOD BackSpace( lDisplay ) CLASS Get

   LOCAL nPos := ::Pos, nMinus

   DEFAULT lDisplay TO .T.

   IF nPos > 1 .AND. nPos == ::FirstEditable() .AND. ::minus
      /* For delete the parenthesis (negative indicator) in a non editable position */

      nMinus := At( "(", SubStr( ::buffer, 1, nPos - 1 ) )

      IF nMinus > 0 .AND. SubStr( ::cPicMask, nMinus, 1 ) != "("

         ::lEdit := .T.

         ::buffer := SubStr( ::buffer, 1, nMinus - 1 ) + " " + ;
            SubStr( ::buffer, nMinus + 1 )

         ::Changed := .T.

         IF lDisplay
            ::Display()
         ENDIF

         RETURN Self

      ENDIF

   ENDIF

   ::Left()

   if ::Pos < nPos
      ::Delete( lDisplay )
   elseif ::lNumToLeft
      //    ::DeleteAll()
      if ::Pos = 1 .AND. nPos = 1
         ::Pos := if( ::DecPos != NIL .AND. ::DecPos > 0, ::DecPos - 2, ::nMaxLen - 1 )
         ::Delete( lDisplay )
      ENDIF
   ENDIF

   RETURN Self

//---------------------------------------------------------------------------//

METHOD _Delete( lDisplay ) CLASS Get

   LOCAL nMaxLen := ::nMaxLen, n, xBuff

   DEFAULT lDisplay TO .T.

   ::Clear := .F.
   ::lEdit := .T.

   if ::lPicComplex
      // Calculating diferent nMaxLen for ::lPicComplex
      FOR n := ::Pos TO nMaxLen
         IF !::IsEditable( n )
            EXIT
         ENDIF
      NEXT
      nMaxLen := n - 1
   ENDIF

   if ::type == "N" .AND. SubStr( ::buffer, ::Pos, 1 ) IN "(-"
      ::minus := .F.
   ENDIF

   /* 2006/OCT/06 - E.F. Added new @K+ functionality to empty
    *               get buffer if first key pressed is DEL key.
    */
   IF ( "K+" IN ::cPicFunc .AND. ::Pos == ::FirstEditable() ) .OR. ;
         ::lNumToLeft .AND. !IsDigit( SubStr( ::buffer, ::Pos, 1 ) )
      ::DeleteAll()
   ELSE
      /* 2007/FEB/24 - E.F. - Adjust ::Pos value under @L picture */

      if ::lNumToLeft

         if ::lDecPos
            if ::Pos < ::DecPos
               ::Pos++
            ENDIF
            if ::Pos - 1 < ::DecPos
               ::lDecPos := .F.
            ENDIF
         elseif ::lNeverDeleted .OR. ( IsDigit( Right(::buffer,1 ) ) .AND. ::Pos + 1 == ::nMaxLen )
            ::Pos++
         ENDIF

      ENDIF
      xBuff := ::buffer
      ::buffer := PadR( SubStr( ::buffer, 1, ::Pos - 1 ) + ;
         SubStr( ::buffer, ::Pos + 1, nMaxLen - ::Pos ) + " " + ;
         SubStr( ::buffer, nMaxLen + 1 ), ::nMaxLen )

      if ::lPassword .AND. ( ! ( xBuff == ::buffer ) .OR. ::nPasswordLen >= ::Pos )
         ::nPasswordLen := Max( 0, ::nPasswordLen - 1 )
      ENDIF
   ENDIF

   if ::type == "D" .OR. ::type == "T"
      ::BadDate := IsBadDate( ::buffer, ::cPicFunc )
   ELSE
      ::BadDate := .F.
   ENDIF

   ::Changed := .T.

   if ::lNeverDeleted
      ::lNeverDeleted := .F.
   ENDIF

   IF lDisplay
      ::Display()
   ENDIF

   RETURN Self

//---------------------------------------------------------------------------//

METHOD DeleteAll() CLASS Get

   LOCAL xValue

   ::lEdit := .T.
   ::lUndo := .F.
   ::lNeverDeleted := .T.

   Switch ::type
   CASE "C"
      xValue := Space( ::nMaxlen )
      EXIT
   CASE "N"
      xValue  := 0
      ::minus := .F.
      EXIT
   CASE "D"
      xValue := SToD()
      ::BadDate := .F.
      EXIT
   CASE "T"
      xValue := CToT( "" )
      ::BadDate := .F.
      EXIT
   CASE "L"
      xValue := .F.
      EXIT
   end

   ::buffer := ::PutMask( xValue, .T. )
   ::Pos    := ::FirstEditable()
   if ::lPassword
      ::nPasswordLen := 0
   ENDIF
   /* E.F. 2006/APRIL/14 - Clipper show all commas and dots of '@E','@R'
    * masks of numeric vars, into display edit buffer after first key number
    * is entered.
    * E.F. 2006/MAY/31 - Idem for numeric mask without '@E' or '@R' as
    * pict '999,999,999.99' also.
    */
   IF ::type == "N" .AND. ::buffer != NIL .AND. !Empty( ::cPicture ) .AND. ;
         ( "," IN ::cPicture .AND. "." IN ::cPicture )
      IF "R" IN ::cPicFunc .AND. !( "E" IN ::cPicFunc ) .OR. Empty( ::cPicFunc )
         ::buffer := StrTran( ::buffer, ".", "" )
         ::buffer := StrTran( ::buffer, ",", "" )
         ::buffer := Transform( ::buffer, ::cPicture )
      ELSEIF "E" IN ::cPicFunc
         IF "R" IN ::cPicFunc
            ::buffer := Transform( ::buffer, ::cPicMask )
         ELSE
            ::buffer := Transform( ::buffer, ::cPicture )
         ENDIF
         ::buffer := StrTran( ::buffer, ".", Chr( 1 ) )
         ::buffer := StrTran( ::buffer, ",", "." )
         ::buffer := StrTran( ::buffer, Chr( 1 ), "," )
      ENDIF
   ENDIF

   RETURN Self

//---------------------------------------------------------------------------//

METHOD DelEnd() CLASS Get

   LOCAL nPos := ::Pos

   IF ! ::hasfocus
      RETURN Self
   ENDIF

   ::Pos := ::nMaxEdit

   ::Delete( .F. )

   DO while ::Pos > nPos
      ::BackSpace( .F. )
   ENDDO

   ::Display()

   RETURN Self

//---------------------------------------------------------------------------//

METHOD DelLeft() CLASS Get

   ::Left( .F. )
   ::Delete( .F. )
   ::Right()

   RETURN Self

//---------------------------------------------------------------------------//

METHOD DelRight() CLASS Get

   ::Right( .F. )
   ::Delete( .F. )
   ::Left()

   RETURN Self

//---------------------------------------------------------------------------//

METHOD DelWordLeft() CLASS Get

   IF ! ::hasfocus
      RETURN Self
   ENDIF

   ::WordLeft()
   ::DelWordRight()

   RETURN Self

//---------------------------------------------------------------------------//

METHOD DelWordRight() CLASS Get

   LOCAL nCount, nPos

   IF ! ::hasfocus
      RETURN Self
   ENDIF

   ::TypeOut := .F.
   ::Clear   := .F.

   if ::Pos == ::nMaxEdit
      ::TypeOut := .T.
      RETURN Self
   ENDIF

// Counts how many characters must be deleted
   nPos := ::Pos
   nCount := 0
   DO WHILE nPos <= ::nMaxEdit .AND. ! SubStr( ::buffer, nPos, 1 ) == " " .AND. ::IsEditable( nPos )
      nPos++
      nCount++
   ENDDO
   DO WHILE nPos <= ::nMaxEdit .AND. SubStr( ::buffer, nPos, 1 ) == " " .AND. ::IsEditable( nPos )
      nPos++
      nCount++
   ENDDO

   DO WHILE nCount > 0
      ::Delete( .F. )
      nCount--
   ENDDO

   ::Display()

   RETURN Self

//---------------------------------------------------------------------------//

/* The METHOD ColorSpec and DATA cColorSpec allow to replace the
 * property ColorSpec for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the function receives a single color and
 * be used for GET_CLR_UNSELECTED and GET_CLR_ENHANCED.
 *
  // QUESTIONS, is realy necessary this method? the ::colorspec generated was
  not clipper 5.x compatible, and also was not respecting SET INTENSITY
*/

METHOD SetColorSpec( cColorSpec ) CLASS Get

   return ::cColorSpec := buildGetColor( cColorSpec )

//---------------------------------------------------------------------------//

/* The METHOD Picture and DATA cPicture allow to replace the
 * property Picture for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the Picture is loaded later on
 * to the creation of the object, being necessary to carry out
 * several tasks to adjust the internal data of the object.
 */

METHOD SetPicture( cPicture ) CLASS Get

   IF cPicture != NIL

      ::nDispLen := NIL

      ::cPicture := cPicture

      ::ParsePict( cPicture )

      if ::nDispLen == NIL .OR. !::lDispLen
         ::nDispLen := ::nMaxLen
      ENDIF

      //if empty( ::cOrigPicture )
      IF !( ::cOrigPicture == cPicture )
         ::cOrigPicture := cPicture
      ENDIF

   ENDIF

   return ::cPicture

//---------------------------------------------------------------------------//

#ifdef HB_COMPAT_C53

METHOD HitTest( mRow, mCol ) CLASS GET

   IF HB_ISOBJECT( ::Control )
      Return ::Control:HitTest( mRow, mCol )
   ENDIF

   IF ::Row != mRow
      RETURN HTNOWHERE
   ENDIF

   IF ::nDispLen == NIL
      ::nDispLen := 1
   ENDIF

   IF mCol >= ::Col .AND. mCol <= ::Col + ::nDispLen + iif( ::cDelimit == NIL, 0, 2 )
      RETURN HTCLIENT
   ENDIF

   RETURN HTNOWHERE

#endif

//---------------------------------------------------------------------------//

METHOD FirstEditable( ) CLASS GET

   LOCAL nFor

   If ::nMaxLen != NIL

      If ::IsEditable( 1 )
         RETURN 1
      ENDIF

      FOR nFor := 2 to ::nMaxLen
         If ::IsEditable( nFor )
            RETURN nFor
         ENDIF
      NEXT

   ENDIF

   ::TypeOut := .T.

   RETURN 0

//---------------------------------------------------------------------------//

METHOD LastEditable( ) CLASS GET

   LOCAL nFor

   If ::nMaxLen != NIL

      FOR nFor := ::nMaxLen TO 1 step - 1
         If ::IsEditable( nFor )
            RETURN nFor
         ENDIF
      NEXT

   ENDIF

   ::TypeOut := .T.

   RETURN 0

//---------------------------------------------------------------------------//

METHOD _NumToLeft() CLASS GET

   LOCAL nRow, nCol
   LOCAL cColor, cBuff, nValue

 /* 2008/FEB/15 - E.F. - Scroll numbers from right to left as calculator.
  *                      Contributed by Julio Cesar Cantillo Molina.
  */

   IF ::Type == "N"

      HBConsoleLock()
      DispBegin()

      nRow := ::Row
      nCol := ::Col

      nValue := ::UnTransform()
      cBuff := Transform( nValue, SubStr( ::picture, At( "9", ::Picture ) ) )

      IF "," IN ::cPicture .OR. "." IN ::cPicture
         IF "R" IN ::cPicFunc .AND. !( "E" IN ::cPicFunc )
            cBuff := StrTran( cBuff, ".", "" )
            cBuff := StrTran( cBuff, ",", "" )
            cBuff := Transform( cBuff, ::cPicture )
         ELSEIF "E" IN ::cPicFunc
            IF "R" IN ::cPicFunc
               cBuff := Transform( cBuff, ::cPicMask )
            ELSE
               cBuff := Transform( cBuff, ::cPicture )
            ENDIF
            cBuff := StrTran( cBuff, ".", Chr( 1 ) )
            cBuff := StrTran( cBuff, ",", "." )
            cBuff := StrTran( cBuff, Chr( 1 ), "," )
         ENDIF
      ENDIF

      cColor := SubStr( ::colorSpec, At( ",",::colorSpec ) + 1 )

      DispOutAt( nRow, nCol, " ", cColor )
      DispOutAt( nRow, nCol, cBuff , cColor )

      IF ( At( "-", ::buffer ) > 0 .AND. nValue = 0 )
         DispOutAt( nRow, nCol, "-", cColor )
      ENDIF

      IF ( ::DecPos > 0 )
         // SetPos( nRow, nCol + IF( ::decpos > ::pos, ::decPos, ::Pos ) - 1 )
         SetPos( nRow, nCol + Min( Len(::buffer ),Max(::decpos - 1,::Pos ) ) - 1 )
      ENDIF

      DispEnd()
      HBConsoleUnLock()

   ENDIF

   RETURN SELF

//--------------------------------------------------------------------------//

METHOD StopMoveH() CLASS Get

// stop horizontal cursor movement under @L picture.

   LOCAL lStop := .F.
   LOCAL nKey  := LastKey()

   if ::lNumToLeft .AND. ! ::lDecPos .AND. ;
      ( nKey == K_LEFT  .OR. ;
        nKey == K_RIGHT .OR. ;
        nKey == K_HOME  .OR. ;
        nKey == K_END   .OR. ;
        nKey == K_CTRL_RIGHT .OR. ;
        nKey == K_CTRL_LEFT )

      if ::DecPos != NIL .AND. ::DecPos > 0
         lStop := ( ::Pos < ::DecPos )
      ELSE
         lStop := .T.
      ENDIF
   ENDIF

   RETURN lStop

//---------------------------------------------------------------------------//
/*
*STATIC FUNCTION IsBadDate( cBuffer, cPicFunc )
*
*   local nFor, nLen
*
*   if "E" IN cPicFunc
*       cBuffer := InvertDwM( cBuffer )
*   endif
*
*   If !Empty( Ctod( cBuffer ) )
*      return .f.
*   Endif
*
*   nLen := len( cBuffer )
*
*   For nFor := 1 to nLen
*      If IsDigit( Substr( cBuffer, nFor, 1 ) )
*         return .t.
*      Endif
*   Next
*
* return .f.
*/

STATIC FUNCTION IsBadDate( cBuffer, cPicFunc )

   LOCAL cBuffer2

   IF Empty( cBuffer )
      RETURN .F.
   ENDIF

   IF "E" IN cPicFunc
      cBuffer := InvertDwM( cBuffer )
   ENDIF

   cBuffer2 := StrTran( cBuffer, "/", "" )
   cBuffer2 := StrTran( cBuffer2, "-", "" )
   cBuffer2 := StrTran( cBuffer2, ".", "" )

   IF Empty( cBuffer2 )
      RETURN .F.
   ENDIF

   IF Empty( CToD( cBuffer ) )
      RETURN .T.
   ENDIF

   RETURN .F.

STATIC FUNCTION InvertDwM( cDate )
/* 2003/03/25 - Eduardo Fernandes <modalsist@yahoo.com.br>
   Invert day with month to date format if "@E" picture is used.*/

   IF SubStr( Set( _SET_DATEFORMAT ), 1, 2 ) == "yy" // set date ANSI and JAPAN
      IF __SetCentury()
         cDate := SubStr( cDate, 1, 5 ) + SubStr( cDate, 9, 2 ) + SubStr( cDate, 8, 1 ) + SubStr( cDate, 6, 2 )
      ELSE
         cDate := SubStr( cDate, 1, 3 ) + SubStr( cDate, 7, 2 ) + SubStr( cDate, 6, 1 ) + SubStr( cDate, 4, 2 )
      ENDIF
   ELSE // all others set date
      cDate := SubStr( cDate, 4, 3 ) + SubStr( cDate, 1, 3 ) + SubStr( cDate, 7 )
   ENDIF

   RETURN cDate

/*
*static procedure AnalyzePicture( cPicture )
*   Local cChar, lS := .f.
*
*   For each cChar in Substr( cPicture, 2 )
*      do case
*      case cChar == "S"
*         lS := .t.
*      case cChar $ "!()L0ABCDEKRXZ"
*         lS := .f.
*      case cChar $ "0123456789" .and. lS
*      other
*         cPicture := Left( cPicture, HB_EnumIndex() )
*      endcase
*   Next
*Return
*/

STATIC FUNCTION BuildGetColor( cColorSpec )

   LOCAL cCur, nClrOth, nClrUns

   IF !HB_ISSTRING( cColorSpec )
      cColorSpec := Nil                          // Clipper compatibility
   ENDIF

   cCur := SetColor()

   IF SET( _SET_INTENSITY )

      DEFAULT cColorSpec TO __GUIColor( cCur, CLR_UNSELECTED + 1 ) + "," + ;
         __GUIColor( cCur, CLR_ENHANCED   + 1 ) + "," + ;
         __GUIColor( cCur, CLR_STANDARD   + 1 ) + "," + ;
         __GUIColor( cCur, CLR_BACKGROUND + 1 )

   ELSE
      DEFAULT cColorSpec TO __GUIColor( cCur, CLR_STANDARD   + 1 )

   ENDIF

#ifdef HB_COMPAT_C53
   cColorSpec := hb_NToColor( nClrUns := Max( hb_ColorToN( hb_ColorIndex( cColorSpec, GET_CLR_UNSELECTED ) ), 0 ) ) + ;
      "," + hb_NToColor( iif( ( nClrOth := hb_ColorToN( hb_ColorIndex( cColorSpec, GET_CLR_ENHANCED ) ) ) != - 1, nClrOth, nClrUns ) ) + ;
      "," + hb_NToColor( iif( ( nClrOth := hb_ColorToN( hb_ColorIndex( cColorSpec, GET_CLR_CAPTION  ) ) ) != - 1, nClrOth, nClrUns ) ) + ;
      "," + hb_NToColor( iif( ( nClrOth := hb_ColorToN( hb_ColorIndex( cColorSpec, GET_CLR_ACCEL    ) ) ) != - 1, nClrOth, nClrUns ) )
#else
   cColorSpec := hb_NToColor( nClrUns := Max( hb_ColorToN( hb_ColorIndex( cColorSpec, GET_CLR_UNSELECTED ) ), 0 ) ) + ;
      "," + hb_NToColor( iif( ( nClrOth := hb_ColorToN( hb_ColorIndex( cColorSpec, GET_CLR_ENHANCED ) ) ) != - 1, nClrOth, nClrUns ) )
#endif

   RETURN cColorSpec

#ifdef HB_COMPAT_C53

FUNCTION __GUIColor( cPair, nPos )

   RETURN hb_ColorIndex( cpair, npos - 1 )

FUNCTION IsDefcolor()

   RETURN Upper( SetColor() ) == "W/N,N/W,N/N,N/N,N/W"

#endif

STATIC FUNCTION HowMuchNumeric( cPict )

   LOCAL c
   LOCAL r := 0

   /* E.F. 2006/MAY/05 - added space and % as part of picture also. */
   FOR EACH c IN cPict
      IF c IN "9#*$ %"
         r++
      ENDIF
   NEXT

   RETURN r
