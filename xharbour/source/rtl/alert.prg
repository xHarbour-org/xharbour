/*
 * $Id: alert.prg,v 1.16 2005/06/13 02:02:49 peterrees Exp $
 */

/*
 * Harbour Project source code:
 * ALERT() function
 *
 * Released to Public Domain by Vladimir Kazimirchik <v_kazimirchik@yahoo.com>
 * www - http://www.harbour-project.org
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    Changes for higher Clipper compatibility, console mode, extensions
 *    __NONOALERT()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbsetup.ch"

#include "box.ch"
#include "common.ch"
#include "inkey.ch"
#include "setcurs.ch"

/* TOFIX: Clipper defines a clipped window for Alert() [vszakats] */

/* NOTE: Clipper will return NIL if the first parameter is not a string, but
         this is not documented. This implementation converts the first
         parameter to a string if another type was passed. You can switch back
         to Clipper compatible mode by defining constant
         HB_C52_STRICT. [vszakats] */

/* NOTE: Clipper handles these buttons { "Ok", "", "Cancel" } in a buggy way.
         This is fixed. [vszakats] */

/* NOTE: nDelay parameter is a Harbour extension. */

#define INRANGE( xLo, xVal, xHi )       ( xVal >= xLo .AND. xVal <= xHi )

#ifdef HB_C52_UNDOC
STATIC s_lNoAlert
#endif

FUNCTION Alert( xMessage, aOptions, cColorNorm, nDelay )

   LOCAL nChoice
   LOCAL aSay, nPos, nWidth, nOpWidth, nInitRow, nInitCol, nEval
   LOCAL nKey, aPos, nCurrent, aHotkey, aOptionsOK, cEval
   LOCAL cColorHigh

   LOCAL nOldRow
   LOCAL nOldCol
   LOCAL nOldCursor
   LOCAL cOldScreen

   LOCAL nOldDispCount
   LOCAL nCount
   LOCAL nLen, sCopy
   LOCAL lWhile

   LOCAL cColorStr,cColorPair1,cColorPair2,cColor11,cColor12,cColor21,cColor22
   LOCAL nCommaSep,nSlash

#ifdef HB_COMPAT_C53
   LOCAL nMRow, nMCol
#endif

   /* TOFIX: Clipper decides at runtime, whether the GT is linked in,
             if it is not, the console mode is choosen here. [vszakats] */
   LOCAL lConsole := .F.

#ifdef HB_C52_UNDOC

   DEFAULT s_lNoAlert TO hb_argCheck( "NOALERT" )

   IF s_lNoAlert
      RETURN NIL
   ENDIF

#endif

   aSay := {}

#ifdef HB_C52_STRICT

   IF !ISCHARACTER( xMessage )
      RETURN NIL
   ENDIF

   DO WHILE ( nPos := At( ';', xMessage ) ) != 0
      AAdd( aSay, Left( xMessage, nPos - 1 ) )
      xMessage := SubStr( xMessage, nPos + 1 )
   ENDDO
   AAdd( aSay, xMessage )

#else

   IF PCount() == 0
      RETURN NIL
   ENDIF

   IF ISARRAY( xMessage )

      FOR EACH cEval IN xMessage
         IF ISCHARACTER( cEval )
            AAdd( aSay, cEval )
         ENDIF
      NEXT

   ELSE

      SWITCH ValType( xMessage )
         CASE "C"
         CASE "M"
            EXIT

         CASE "N"
            xMessage := LTrim( Str( xMessage ) )
            EXIT

         CASE "D"
            xMessage := DToC( xMessage )
            EXIT

         CASE "L"
            xMessage := iif( xMessage, ".T.", ".F." )
            EXIT

         CASE "O"
            xMessage := xMessage:className + " Object"
            EXIT

         CASE "B"
            xMessage := "{||...}"
            EXIT

         DEFAULT
            xMessage := "NIL"
      END

      DO WHILE ( nPos := At( ';', xMessage ) ) != 0
         AAdd( aSay, Left( xMessage, nPos - 1 ) )
         xMessage := SubStr( xMessage, nPos + 1 )
      ENDDO
      AAdd( aSay, xMessage )

      FOR EACH xMessage IN aSay

         IF ( nLen := Len( xMessage ) ) > 58
            FOR nPos := 58 TO 1 STEP -1
               IF xMessage[nPos] IN ( " " + Chr( 9 ) )
                  EXIT
               ENDIF
            NEXT

            IF nPos == 0
               nPos := 58
            ENDIF

            sCopy := xMessage
            aSay[ HB_EnumIndex() ] := RTrim( Left( xMessage, nPos ) )

            IF Len( aSay ) == HB_EnumIndex()
               aAdd( aSay, SubStr( sCopy, nPos + 1 ) )
            ELSE
               aIns( aSay, HB_EnumIndex() + 1, SubStr( sCopy, nPos + 1 ), .T. )
            ENDIF
        ENDIF
      NEXT

   ENDIF

#endif

   IF !ISARRAY( aOptions )
      aOptions := {}
   ENDIF

   IF !ISCHARACTER( cColorNorm ) .or. EMPTY( cColorNorm )
      cColorNorm := "W+/R" // first pair color (Box line and Text)
      cColorHigh := "W+/B" // second pair color (Options buttons)
   ELSE

      /* NOTE: Clipper Alert does not handle second color pair properly.
               If we inform the second color pair, xHarbour alert will consider it.
               if we not inform the second color pair, then xHarbour alert will behave
               like Clipper.  2004/Sep/16 - Eduardo Fernandes <modalsist> */

      cColor11 := cColor12 := cColor21 := cColor22 := ""

      cColorStr := alltrim( StrTran( cColorNorm," ","") )
      nCommaSep := At(",",cColorStr)

      if nCommaSep > 0 // exist more than one color pair.
         cColorPair1 := SubStr( cColorStr, 1, nCommaSep - 1 )
         cColorPair2 := SubStr( cColorStr, nCommaSep + 1 )
      else
         cColorPair1 := cColorStr
         cColorPair2 := ""
      endif

      nSlash := At("/",cColorPair1)

      if nSlash > 1
         cColor11 := SubStr( cColorPair1,1,nSlash-1)
         cColor12 := SubStr( cColorPair1,nSlash+1)
      else
         cColor11 := cColorPair1
         cColor12 := "R"
      endif

      if ColorValid(cColor11) .and. ColorValid(cColor12)

        // if color pair is passed in numeric format, then we need to convert for
        // letter format to avoid blinking in some circumstances.
        if IsDigit( cColor11 )
           cColor11 := COLORLETTER( cColor11 )
        endif

        cColorNorm := cColor11

        if !empty(cColor12)

            if IsDigit( cColor12 )
               cColor12 := COLORLETTER( cColor12 )
            endif

            cColorNorm := cColor11+"/"+cColor12

        endif

      else
         cColor11 := "W+"
         cColor12 := "R"
         cColorNorm := cColor11+"/"+cColor12
      endif


      // if second color pair exist, then xHarbour alert will handle properly.
      if !empty( cColorPair2 )

         nSlash := At("/",cColorPair2)

         if nSlash > 1
            cColor21 := SubStr( cColorPair2,1,nSlash-1)
            cColor22 := SubStr( cColorPair2,nSlash+1)
         else
            cColor21 := cColorPair2
            cColor22 := "B"
         endif

         if ColorValid(cColor21) .and. ColorValid(cColor22)

            if IsDigit( cColor21 )
               cColor21 := COLORLETTER( cColor21 )
            endif

            cColorHigh := cColor21

            if !empty(cColor22)

                if IsDigit( cColor22 )
                   cColor22 := COLORLETTER( cColor22 )
                endif

                // extracting color attributes from background color.
                cColor22 := StrTran( cColor22, "+", "" )
                cColor22 := StrTran( cColor22, "*", "" )
                cColorHigh := cColor21+"/"+cColor22

            endif

         else
            cColorHigh := "W+/B"
         endif

      else // if does not exist the second color pair, xHarbour alert will behave like Clipper
         if empty(cColor11) .or. empty(cColor12)
            cColor11 := "B"
            cColor12 := "W+"
         else
            cColor11 := StrTran( cColor11, "+", "" )
            cColor11 := StrTran( cColor11, "*", "" )
         endif
         cColorHigh := cColor12+"/"+cColor11
      endif

   ENDIF

   IF nDelay == NIL
      nDelay := 0
   ENDIF

   /* The longest line */
   nWidth := 0
   AEval( aSay, {| x | nWidth := Max( Len( x ), nWidth ) } )

   /* Cleanup the button array */
   aOptionsOK := {}
   FOR EACH cEval IN aOptions
      IF ISCHARACTER( cEval ) .AND. !Empty( cEval )
         AAdd( aOptionsOK, cEval )
      ENDIF
   NEXT

   IF Len( aOptionsOK ) == 0
      aOptionsOK := { 'Ok' }
#ifdef HB_C52_STRICT
   /* NOTE: Clipper allows only four options [vszakats] */
   ELSEIF Len( aOptionsOK ) > 4
      aSize( aOptionsOK, 4 )
#endif
   ENDIF

   /* Total width of the botton line (the one with choices) */
   nOpWidth := 0
   AEval( aOptionsOK, {| x | nOpWidth += Len( x ) + 4 } )

   /* what's wider ? */
   nWidth := Max( nWidth + 2 + iif( Len( aSay ) == 1, 4, 0 ), nOpWidth + 2 )

   /* box coordinates */
   nInitRow := Int( ( ( MaxRow() - ( Len( aSay ) + 4 ) ) / 2 ) + .5 )
   nInitCol := Int( ( ( MaxCol() - ( nWidth + 2 ) ) / 2 ) + .5 )

   /* detect prompts positions */
   aPos := {}
   aHotkey := {}
   nCurrent := nInitCol + Int( ( nWidth - nOpWidth ) / 2 ) + 2
   AEval( aOptionsOK, {| x | AAdd( aPos, nCurrent ), AAdd( aHotKey, Upper( Left( x, 1 ) ) ), nCurrent += Len( x ) + 4 } )

   nChoice := 1

   IF lConsole

      nCount := Len( aSay )
      FOR EACH cEval IN aSay
         OutStd( cEval )
         IF HB_EnumIndex() < nCount
            OutStd( hb_OSNewLine() )
         ENDIF
      NEXT

      OutStd( " (" )
      nCount := Len( aOptionsOK )
      FOR EACH cEval IN aOptionsOK
         OutStd( cEval )
         IF HB_EnumIndex() < nCount
            OutStd( ", " )
         ENDIF
      NEXT
      OutStd( ") " )

      /* choice loop */
      lWhile := .T.
      DO WHILE lWhile

         nKey := Inkey( nDelay, INKEY_ALL )

         SWITCH nKey
            CASE 0
               lWhile := .F.
               EXIT

            CASE K_ESC

               nChoice := 0
               lWhile  := .F.
               EXIT

            DEFAULT
               IF Upper( Chr( nKey ) ) IN aHotkey
                  nChoice := aScan( aHotkey, {| x | x == Upper( Chr( nKey ) ) } )
                  lWhile  := .F.
               ENDIF

         END

      ENDDO

      OutStd( Chr( nKey ) )

   ELSE

      /* PreExt */
      nCount := nOldDispCount := DispCount()

      DO WHILE nCount-- != 0
         DispEnd()
      ENDDO

      /* save status */
      nOldRow := Row()
      nOldCol := Col()
      nOldCursor := SetCursor( SC_NONE )
      cOldScreen := SaveScreen( nInitRow, nInitCol, nInitRow + Len( aSay ) + 3, nInitCol + nWidth + 1 )

      /* draw box */
      DispBox( nInitRow, nInitCol, nInitRow + Len( aSay ) + 3, nInitCol + nWidth + 1, B_SINGLE + ' ', cColorNorm )

      FOR EACH cEval IN aSay
         DispOutAt( nInitRow + HB_EnumIndex(), nInitCol + 1 + Int( ( ( nWidth - Len( cEval ) ) / 2 ) + .5 ), cEval, cColorNorm )
      NEXT

      /* choice loop */
      lWhile := .T.
      DO WHILE lWhile

         nCount := Len( aSay )
         FOR EACH cEval IN aOptionsOK
            DispOutAt( nInitRow + nCount + 2, aPos[ HB_EnumIndex() ], " " + cEval + " ", cColorNorm )
         NEXT
         DispOutAt( nInitRow + nCount + 2, aPos[ nChoice ], " " + aOptionsOK[ nChoice ] + " ", cColorHigh, TRUE )

         nKey := Inkey( nDelay, INKEY_ALL )

         SWITCH nKey
            CASE K_ENTER
            CASE K_SPACE
            CASE 0
               lWhile := .F.
               EXIT

            CASE K_ESC
               nChoice := 0
               lWhile  := .F.
               EXIT

#ifdef HB_COMPAT_C53

            CASE K_LBUTTONDOWN

               nMRow  := WMRow()
               nMCol  := WMCol()
               nPos   := 0
               nCount := Len( aSay )

               FOR EACH cEval IN aOptionsOK
                  IF nMRow == nInitRow + nCount + 2 .AND. ;
                       INRANGE( aPos[ HB_EnumIndex() ], nMCol, aPos[ HB_EnumIndex() ] + Len( cEval ) + 2 - 1 )
                     nPos := HB_EnumIndex()
                     EXIT
                  ENDIF
               NEXT

               IF nPos > 0
                  nChoice := nPos
                  lWhile := .F.
               ENDIF

               EXIT

#endif

            CASE K_LEFT
            CASE K_SH_TAB
               IF Len( aOptionsOK ) > 1

                  nChoice--
                  IF nChoice == 0
                     nChoice := Len( aOptionsOK )
                  ENDIF

                  nDelay := 0
               ENDIF
               EXIT

            CASE K_RIGHT
            CASE K_TAB
               IF Len( aOptionsOK ) > 1

                  nChoice++
                  IF nChoice > Len( aOptionsOK )
                     nChoice := 1
                  ENDIF

                  nDelay := 0
               ENDIF
               EXIT

            DEFAULT
               IF Upper( Chr( nKey ) ) IN aHotkey

                  nChoice := aScan( aHotkey, {| x | x == Upper( Chr( nKey ) ) } )
                  lWhile  := .F.
               ENDIF

         END

      ENDDO

      /* Restore status */
      RestScreen( nInitRow, nInitCol, nInitRow + Len( aSay ) + 3, nInitCol + nWidth + 1, cOldScreen )
      SetCursor( nOldCursor )
      SetPos( nOldRow, nOldCol )

      /* PostExt */
      DO WHILE nOldDispCount-- != 0
         DispBegin()
      ENDDO

   ENDIF

   RETURN nChoice

#ifdef HB_C52_UNDOC

PROCEDURE __NONOALERT()

   s_lNoAlert := .F.

   RETURN

#endif

//-----------------------------------//
// 2004/Setp/15 - Eduardo Fernandes
// Convert number color format to character color format.
STATIC FUNCTION COLORLETTER( cColor )

Local nColor
Local aColor := {"N","B","G","BG","R","RB","GR","W","N+",;
                 "B+","G+","BG+","R+","RB+","GR+","W+"}

  if !IsCharacter( cColor )
     cColor:=""
  endif

  cColor := StrTran( cColor, " ","")
  cColor := StrTran( cColor, "*","")
  cColor := StrTran( cColor, "+","")

  nColor := Min( Abs( Val( cColor ) ), 15 )

RETURN (  cColor := aColor[ nColor + 1 ] )

//-----------------------------------//
// 2004/Setp/15 - Eduardo Fernandes
// Test vality of the color string
STATIC FUNCTION COLORVALID( cColor )

if !IsCharacter( cColor )
   Return .F.
endif

cColor := StrTran( cColor, " ","" )
cColor := StrTran( cColor, "*","" )
cColor := StrTran( cColor, "+","" )
cColor := Upper( cColor )

Return cColor IN { "0","1","2","3","4","5","6","7","8","9","10","11","12",;
                   "13","14","15","B","BG","G","GR","N","R","RB","W"}
