/*
 * $Id: alert.prg,v 1.27 2008/03/16 19:15:59 likewolf Exp $
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

#include "box.ch"
#include "common.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "hbgtinfo.ch"

/* TOFIX: Clipper defines a clipped window for Alert() [vszakats] */

/* NOTE: Clipper will return NIL if the first parameter is not a string, but
         this is not documented. This implementation converts the first
         parameter to a string if another type was passed. You can switch back
         to Clipper compatible mode by undefining constant HB_EXTENSION. [vszakats] */

/* NOTE: Clipper handles these buttons { "Ok", "", "Cancel" } in a buggy way.
         This is fixed. [vszakats] */

/* NOTE: nDelay parameter is a Harbour extension. */

#ifdef HB_C52_UNDOC
STATIC s_lNoAlert
#endif

FUNCTION Alert( xMessage, aOptions, cColor, nDelay )
   LOCAL cMessage
   LOCAL cColorBox
   LOCAL cColorButton
   LOCAL aOptionsOK
   LOCAL nEval
   LOCAL nAt
#ifdef HB_EXTENSION
   LOCAL lFirst
   LOCAL cLine
#endif

#ifdef HB_C52_UNDOC

   DEFAULT s_lNoAlert TO hb_argCheck( "NOALERT" )

   IF s_lNoAlert
      RETURN NIL
   ENDIF

#endif

#ifdef HB_EXTENSION

   IF PCount() == 0
      RETURN NIL
   ENDIF

   cMessage := ""

   IF ISARRAY( xMessage )

      lFirst := .T.
      FOR nEval := 1 TO Len( xMessage )
         IF ISCHARACTER( cLine := xMessage[ nEval ] )
            cMessage += iif( lFirst, "", Chr( 10 ) ) + cLine
            lFirst := .F.
         ENDIF
      NEXT

   ELSE

      DO CASE
      CASE ValType( xMessage ) $ "CM" ; cMessage := StrTran( xMessage, ";", Chr( 10 ) )
      CASE ValType( xMessage ) == "N" ; cMessage := LTrim( Str( xMessage ) )
      CASE ValType( xMessage ) == "D" ; cMessage := DToC( xMessage )
      CASE ValType( xMessage ) == "L" ; cMessage := iif( xMessage, ".T.", ".F." )
      CASE ValType( xMessage ) == "O" ; cMessage := xMessage:className + " Object"
      CASE ValType( xMessage ) == "S" ; cMessage := "@" + xMessage:Name + "()"
      CASE ValType( xMessage ) == "B" ; cMessage := "{||...}"
      OTHERWISE                       ; cMessage := "NIL"
      ENDCASE

   ENDIF

#else

   IF !ISCHARACTER( xMessage )
      RETURN NIL
   ENDIF

   cMessage := StrTran( xMessage, ";", Chr( 10 ) )

#endif

   IF ! ISARRAY( aOptions )
      aOptions := {}
   ENDIF


   IF ! ISCHARACTER( cColor ) .or. EMPTY( cColor )
      cColorBox    := "W+/R"  // first color pair:  (Box)
      cColorButton := "W+/B"  // second color pair: (Buttons)
   ELSE
/*
      cColorButton := StrTran( StrTran( iif( At( "/", cColor ) == 0, "N", SubStr( cColor, At( "/", cColor ) + 1 ) ) + "/" +;
                                        iif( At( "/", cColor ) == 0, cColor, Left( cColor, At( "/", cColor ) - 1 ) ), "+", "" ), "*", "" )
*/  
      nAt := At(",",cColor)
      if nAt == 0
         cColorBox := cColor
         nAt := At("/",cColor)
         if nAt != 0
            cColorButton := SubStr( cColor, nAt+1 )+"/"+ SubStr( cColor, 1, nAt - 1 ) 
            cColorButton := StrTran( cColorButton, "+","") 
            cColorButton := StrTran( cColorButton, "*","") 
         else
            cColorButton := "W+/B" 
         endif
      else
         cColorBox    := Left( cColor, nAt - 1 )
         cColorButton := SubStr( cColor, nAt + 1 )
      endif

   ENDIF

   IF nDelay == NIL
      nDelay := 0
   ENDIF

   aOptionsOK := {}
   FOR nEval := 1 TO Len( aOptions )
      IF ISCHARACTER( aOptions[ nEval ] ) .AND. !Empty( aOptions[ nEval ] )
         AAdd( aOptionsOK, aOptions[ nEval ] )
      ENDIF
   NEXT

   IF Len( aOptionsOK ) == 0
      aOptionsOK := { 'Ok' }
#ifdef HB_C52_STRICT
   /* NOTE: Clipper allows only four options [vszakats] */
   ELSEIF Len( aOptionsOK ) > 4
      ASize( aOptionsOK, 4 )
#endif
   ENDIF

   RETURN hb_gtAlert( cMessage, aOptionsOK, cColorBox, cColorButton, nDelay );

#ifdef HB_C52_UNDOC

PROCEDURE __NONOALERT()

   s_lNoAlert := .F.

   RETURN

#endif
