/*
 * HWGUI - Harbour Win32 GUI library source code:
 * Windows errorsys replacement
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "common.ch"
#include "error.ch"
//#include "windows.ch"

Static LogInitialPath

PROCEDURE WG_MyErrorSys

   ErrorBlock( { | oError | WG_DefError( oError ) } )
   LogInitialPath := "\" + CURDIR() + IIF( EMPTY( CURDIR() ), "", "\" )
   //MessageBox(0,"Inizializzazione errori" )

   RETURN

STATIC FUNCTION WG_DefError( oError )
   LOCAL cMessage
   LOCAL cDOSError

   LOCAL aOptions
   LOCAL nChoice

   LOCAL n

   //MessageBox(0,"ERRORE ....." )

   // By default, division by zero results in zero
   IF oError:genCode == EG_ZERODIV
      RETURN 0
   ENDIF

   // Set NetErr() of there was a database open error
   IF oError:genCode == EG_OPEN .AND. ;
      oError:osCode == 32 .AND. ;
      oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   // Set NetErr() if there was a lock error on dbAppend()
   IF oError:genCode == EG_APPENDLOCK .AND. ;
      oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   cMessage := WG_ErrorMessage( oError )
   IF ! Empty( oError:osCode )
      cDOSError := "(DOS Error " + LTrim( Str( oError:osCode ) ) + ")"
   ENDIF

   IF ! Empty( oError:osCode )
      cMessage += " " + cDOSError
   ENDIF

   n := 2
   WHILE ! Empty( ProcName( n ) )
      cMessage += Chr(10) + "Called from " + ProcName( n ) + ;
               "(" + AllTrim( Str( ProcLine( n++ ) ) ) + ")"
   ENDDO

   MemoWrit( LogInitialPath+"Error.log", cMessage )
   //SendMessage( GetMainWindow()[1], WM_DESTROY, 0, 0 )
   QUIT

RETURN .F.


FUNCTION WG_ErrorMessage( oError )
   LOCAL cMessage

   // start error message
   cMessage := iif( oError:severity > ES_WARNING, "Error", "Warning" ) + " "

   // add subsystem name if available
   IF ISCHARACTER( oError:subsystem )
      cMessage += oError:subsystem()
   ELSE
      cMessage += "???"
   ENDIF

   // add subsystem's error code if available
   IF ISNUMBER( oError:subCode )
      cMessage += "/" + LTrim( Str( oError:subCode ) )
   ELSE
      cMessage += "/???"
   ENDIF

   // add error description if available
   IF ISCHARACTER( oError:description )
      cMessage += "  " + oError:description
   ENDIF

   // add either filename or operation
   DO CASE
   CASE !Empty( oError:filename )
      cMessage += ": " + oError:filename
   CASE !Empty( oError:operation )
      cMessage += ": " + oError:operation
   ENDCASE

   RETURN cMessage

function WG_WriteLog( cText,fname )
Local nHand

  fname := LogInitialPath + Iif( fname == Nil,"a.log",fname )
  if !File( fname )
     nHand := Fcreate( fname )
  else
     nHand := Fopen( fname,1 )
  endif
  Fseek( nHand,0,2 )
  Fwrite( nHand, cText + chr(10) )
  Fclose( nHand )

return nil
