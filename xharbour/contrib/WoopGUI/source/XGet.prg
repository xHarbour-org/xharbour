/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
*
*-----------------------------------------------------------------------------
* Parts of this project come from:
* "Harbour MiniGUI"
*                   Copyright 2002 Roberto Lopez <roblez@ciudad.com.ar>
*                   http://www.geocities.com/harbour_minigui/
* "Harbour GUI framework for Win32"
*                   Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
*                   Copyright 2001 Antonio Linares <alinares@fivetech.com>
*                   http://www.harbour-project.org
*-----------------------------------------------------------------------------
*
*/

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"
#include "windows.ch"

#include "getexit.ch"
#include "inkey.ch"

#pragma BEGINDUMP

#include "hbapi.h"
#include "hbapigt.h"

static int s_nRow = 0, s_nCol = 0;

HB_FUNC( SETPOS ) /* Sets the screen position */
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
   {
      s_nRow = hb_parni( 1 );
      s_nCol = hb_parni( 2 );
   }

}

HB_FUNC( ROW ) /* Return the current screen row position (zero origin) */
{
   hb_retni( s_nRow );
}

HB_FUNC( COL ) /* Return the current screen column position (zero origin) */
{
   hb_retni( s_nCol );
}

#pragma ENDDUMP

//---------------------------------------------------------------------------//
/*
FUNCTION __GET( bSetGet, cVarName, cPicture, bValid, bWhen )

   LOCAL oGet

   IF bSetGet == NIL
      IF FieldPos( cVarName ) > 0
         // "{|_1| IIF( _1 == NIL, FIELD->&cVarName, FIELD->&cVarName := _1 )"
         bSetGet := &( "{|_1| IIF( _1 == NIL, FIELD->" + cVarName + ", FIELD->" + cVarName + " := _1 ) }" )
      ELSEIF __MVEXIST( cVarName )
         // "{|_1| IIF( _1 == NIL, M->&cVarName, M->&cVarName := _1 )"
         bSetGet := {|_1| iif( _1 == NIL,  __MVGET( cVarName ), __MVPUT( cVarName, _1 ) ) }
      ELSE
         // Force a Run-Time Error!
         bSetGet := &cVarName
      ENDIF
   ENDIF

   oGet := WG_TXGet():New( Row(), Col(), bSetGet, cVarName, cPicture )

   oGet:PreBlock := bWhen
   oGet:PostBlock := bValid

RETURN oGet

FUNCTION __GETA( bGetArray, cVarName, cPicture, bValid, bWhen, aIndex )

   LOCAL oGet
   LOCAL nDim := Len( aIndex )
   LOCAL bSetGet
   LOCAL aGetVar
   LOCAL nCounter

   IF bGetArray == NIL
      IF __MVEXIST( cVarName )
         aGetVar := __MVGET( cVarName )
      ELSE
         aGetVar := &cVarName
      ENDIF
   ELSE
      aGetVar := Eval( bGetArray )
   ENDIF

   FOR nCounter := 1 TO nDim - 1
      aGetVar := aGetVar[ aIndex[ nCounter ] ]
   NEXT
   bSetGet := {|_1| iif( _1 == NIL, aGetVar[ aIndex[ nCounter ] ], aGetVar[ aIndex[ nCounter ] ] := _1 ) }

   oGet := WG_TXGet():New(Row(), Col(), bSetGet, cVarName, cPicture )
   oGet:SubScript := aIndex

   oGet:PreBlock := bWhen
   oGet:PostBlock := bValid

RETURN oGet
*/

FUNCTION _XGET_( cVarName, bSetGet, cType, cCaption, nRow, nCol, nWidth, nHeight, ;
                 oParent, cToolTip, cStatusMsg, cMsg, lPixel, nID, ;
                 nLimit, aVal, xDef, cPicture, bDisplay, bReader, bValid, bWhen, bHelp, bMeaning, cAlias, ;
                 bAction, bReadblock, oMaint, nKey, cLetter, bExit, lReadOnly, lPassword, ;
                 oFont, cFontName, nFontSize, ncFgColor, ncBgColor, nStyle, lNoAutoSize )

   LOCAL oGet

   DEFAULT lNoAutoSize TO TRUE

   IF bSetGet == NIL
      IF FieldPos( cVarName ) > 0
         // "{|_1| IIF( _1 == NIL, FIELD->&cVarName, FIELD->&cVarName := _1 )"
         bSetGet := &( "{|_1| IIF( _1 == NIL, FIELD->" + cVarName + ", FIELD->" + cVarName + " := _1 ) }" )
      ELSEIF __MVEXIST( cVarName )
         // "{|_1| IIF( _1 == NIL, M->&cVarName, M->&cVarName := _1 )"
         bSetGet := {|_1| iif( _1 == NIL,  __MVGET( cVarName ), __MVPUT( cVarName, _1 ) ) }
      ELSE
         // Force a Run-Time Error!
         bSetGet := &cVarName
      ENDIF
   ENDIF

   oGet := WG_TXGet():NewExtended( nRow, nCol, bSetGet, cVarName, cPicture,, ;
                                   nWidth, nHeight, oParent, cToolTip,;
                                   cStatusMsg, lPixel, nID, xDef, nLimit, lReadOnly, lPassword, !lNoAutoSize, ;
                                   oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor )

   IF nRow <> NIL .AND. nCol <> NIL
      SetPos( nRow, nCol )
   ELSE
      SetPos( oGet:nRow, oGet:nCol )
   ENDIF

   oGet:PreBlock := bWhen
   oGet:PostBlock := bValid

RETURN oGet







// Called by @...XGET command -> similar to _GET_()
/*
FUNCTION _XGET_( cVar, bVar, cType, cCaption, nRow, nCol, nWidth, nHeight, ;
                 oParent, cToolTip, cStatusMsg, cMsg, lPixel, nID, ;
                 nLimit, aVal, xDef, cPicture, bDisplay, bReader, bValid, bWhen, bHelp, bMeaning, cAlias, ;
                 bAction, bReadblock, oMaint, nKey, cLetter, bExit, lReadOnly, ;
                 oFont, cFontName, nFontSize, ncFgColor, ncBgColor, nStyle )

   LOCAL xg, t, fbVar, f
   LOCAL bXGValid
   LOCAL cBaseName := StripAlias( cVar )
   LOCAL mt

   //WG_ParamDisplay( , hb_aparams(), "_XGET_" )
   WG_DebugTrace("XGet: _XGET_ - START" )

   DEFAULT cType TO "EDIT"

   cType := Upper( cType )

   // Set codeblock if nil
   IF bVar == NIL
      IF FieldPos( cVar ) > 0
         // "{|_1| IIF( _1 == NIL, FIELD->&cVar, FIELD->&cVar := _1 )"
         bVar := &( "{|_1| IIF( _1 == NIL, FIELD->" + cVar + ", FIELD->" + cVar + " := _1 ) }" )
      ELSEIF __MVEXIST( cVar )
         // "{|_1| IIF( _1 == NIL, M->&cVar, M->&cVar := _1 )"
         bVar := {|_1| iif( _1 == NIL,  __MVGET( cVar ), __MVPUT( cVar, _1 ) ) }
      ELSE
         // Force a Run-Time Error!
         bVar := &cVar
      ENDIF
   ENDIF

   //IF cAlias == NIL
   //   IF m != NIL
   //      mt:= MaintMaster( m )
   //   ELSE
   //      mt:= ReportGetTable( ReportActive() )
   //   ENDIF
   //   f := TableGetFld( mt, cBaseName )
   //ELSE
   //   f := SysGetFld( Upper( cAlias ), cBaseName )
   //ENDIF

   IF bReadBlock == NIL
      IF f == NIL
         fbVar := bVar
      ELSE
         fbVar := f
      ENDIF
   ELSE
      fbVar := bReadBlock
   ENDIF

   WG_DebugTrace("XGet: _XGET_ - Define TXGet():New()" )

   xg := WG_TXGet():New( nRow, nCol, bVar, cVar, cPicture, ;
                         nWidth, nHeight, oParent, cToolTip,;
                         cStatusMsg, lPixel, nID, xDef, nLimit, cMsg, bHelp, bMeaning, bDisplay, ;
                         nKey, cLetter, bExit, ;
                         oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor )

   //IF f <> NIL .AND. FldTrans( f )
   //   xg:preblock := {|| FALSE }
   //ELSE
      xg:preBlock := bWhen
   //ENDIF
   IF f == NIL
      xg:postBlock := {|g| CondEval( bExit, g ), ValidPost( bValid, g ) }
   ELSE
      DO CASE
         CASE bValid <> NIL .AND. xg:postBlock <> NIL
            bXGValid := xg:postBlock
            xg:postBlock := {|g| Eval( bXGValid, g ) .AND. ValidPost( bValid, g ) }
         CASE bValid <> NIL
            xg:postBlock := {|g| CondEval( bExit, g ), ValidPost( bValid, g ) }
      ENDCASE
   ENDIF
   //IF cColor == NIL .AND. f <> NIL
   //   IF FldTrans( f )
   //      xg:colorSpec := "n/w+" // FSG - ColorSayData()
   //   ELSEIF !( FldHelpBlk( f ) == NIL ) .OR. FldType( f ) == "D"
   //      xg:colorSpec := "W+/G" // FSG - ColorGetF2()
   //   ENDIF
   //ENDIF
   xg:bAction       := bAction
   xg:cExtendedType := cType
   xg:cDefault      := xDef

   DO CASE
      CASE cType == "EDIT"
           WG_DebugTrace("XGet: _XGET_ - Type EDIT, Define xg:Control" )
           xg:Control       := WG_TEdit():NewExtended( cVar, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip,;
                                               cStatusMsg, lPixel, nID, xDef, nLimit, lReadOnly, lPassword, ;
                                               bVar, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor )

           WG_DebugTrace("XGet: _XGET_ - Type EDIT", "xg:Control", xg:Control )
           //xg:Control:DisplayData()
           // xg:Reader        := {|| WG_TEditReader(
   ENDCASE


   //DO CASE
   //   CASE cType == "COMBO"
   //        IF aVal == NIL .AND. f <> NIL
   //           aVal := AClone( FldArrMeaning( f ) )
   //        ENDIF
   //        xg:aData( xg, aVal )
   //        // FSG - CoGetInit( xg )
   //
   //   CASE cType == "CHECK"
   //        IF aVal == NIL .AND. f <> NIL
   //           aVal := AClone( FldArrMeaning( f ) )
   //        ENDIF
   //        XGetArray( xg, aVal )
   //
   //   CASE cType == "RADIOBUTTON"
   //        IF aVal[3] == NIL .AND. f <> NIL
   //           aVal[3] := AClone( FldArrMeaning( f ) )
   //           aVal[2] := TRUE
   //        ENDIF
   //        xg:colorSpec := ColorGetData()
   //        XGetArray( xg, aVal )
   //        RaGetInit( xg )
   //
   //   //CASE cType == "NORMAL"
   //   //CASE cType == "BUTTON"
   //   //CASE cType == "SECRET"
   //   OTHERWISE
   //        XGetArray( xg, aVal )
   //ENDCASE
   // To be cleaned if uncomment lines above
   xg:aData := aVal
   IF bReader != NIL
      xg:reader := bReader
   ENDIF

   *xg:display()
   *Eval( XGetMeaning( xg ), xg )
RETURN xg

STATIC FUNCTION ValidPost( bValid, xg )
   LOCAL lOk := TRUE
RETURN lOk
*/

/*
STATIC FUNCTION ValidPost( bValid, xg )
   LOCAL lOk := TRUE
   LOCAL cErrMsg
   LOCAL f, lHelp, lCallHelp := TRUE

   IF ( f:= XGetFld( xg ) ) <> NIL
      DO WHILE TRUE
         lOk := Eval( bValid, xg, @cErrMsg, @lCallHelp )
         IF !lOk .AND. xg:HasFocus .AND. lCallHelp
            lHelp := XGetHelp( xg )
            IF lHelp .AND. !Esc()
               KEYBOARD ""  // Svuoto il buffer dagli ultimi tasti premuti
               LOOP
            ENDIF
         ENDIF
         EXIT
      ENDDO
   ELSEIF bValid != NIL
      lOk := Eval( bValid, xg, @cErrMsg, @lCallHelp )
   ENDIF
   IF !lOk .AND. cErrMsg != NIL
      IF xg:hasFocus .AND. InfoActive()
         InfoMsg( cErrMsg, BOX_CENTER )
      ELSE
         MsgBox( cErrMsg, 0, BOX_CENTER )
      ENDIF
   ENDIF
RETURN lOk
*/                                                                          