/*
 * $Id: alert.prg,v 1.10 2003/01/27 03:37:23 walito Exp $
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

   IF !ISCHARACTER( cColorNorm )
      cColorNorm := "W+/R"
      cColorHigh := "W+/B"
   ELSE
      cColorHigh := StrTran( StrTran( iif( At( "/", cColorNorm ) == 0, "N", SubStr( cColorNorm, At( "/", cColorNorm ) + 1 ) ) + "/" +;
                                      iif( At( "/", cColorNorm ) == 0, cColorNorm, Left( cColorNorm, At( "/", cColorNorm ) - 1 ) ), "+", "" ), "*", "" )
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
         DispOutAt( nInitRow + nCount + 2, aPos[ nChoice ], " " + aOptionsOK[ nChoice ] + " ", cColorHigh )

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

               nMRow := MRow()
               nMCol := MCol()

               nChoice := 0
               nCount  := Len( aSay )
               FOR EACH cEval IN aOptionsOK
                  IF nMRow == nInitRow + nCount + 2 .AND. ;
                       INRANGE( aPos[ HB_EnumIndex() ], nMCol, aPos[ HB_EnumIndex() ] + Len( cEval ) + 2 - 1 )
                     nChoice := HB_EnumIndex()
                     EXIT
                  ENDIF
               NEXT

               IF nChoice == 0
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

