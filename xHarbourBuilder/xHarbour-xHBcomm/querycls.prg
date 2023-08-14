#define CRLF hb_osNewLine()
#include "tcomm.ch"
#include "common.ch"

PROCEDURE MAIN( cPort )

   LOCAL cDaten
   LOCAL Taste
   LOCAL GetList := {}
   LOCAL nCom
   LOCAL nByte
   LOCAL x       := Fcreate( "modem1.txt" )
   LOCAL oComm
   Local a,b,c,d

   DEFAULT cPort to "COM3"
   CLEAR

   cDaten := Space( 4000 )
   nCom   := 0
   ? "Monitor Telefonswitchboard"
   // Initialize COM1: with 9600 baud, 7 databits, even (3), 1 stopbit (0)
   START COMMDIALOG oComm PORT cPort
   IF !oComm:IsWorking( nCom )
      ? "Can't open COM1:!"
      QUIT
   ENDIF
   ? SETHANDSHAKE( oComm:nHandle,EHANDSHAKEHARDWARE)
   ? "Serial port COM1: opened and initialized"
   ?
   ?
   WRITE COMM oComm DATA "AT" + Chr( 13 )

   GETINBUFFER oComm into nByte

   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)
   IF nByte > 0

      Inkey( .2 )

      IF oComm:readport( nByte, @cDaten ) < 0
         ? 'Error reading COM1:'
      ENDIF

   ENDIF
   // ESC pressed?
   Inkey( .2 )

   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   WRITE COMM oComm DATA "AT &F E0 &C1 &D2 V1 S0=0\V1" + Chr( 13 )
   GETINBUFFER oComm into nByte
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   IF nByte > 0
      
      Inkey( .2 )

      IF oComm:readport( nByte, @cDaten ) < 0
         ? 'Error reading COM1:'
      ENDIF
      ?? cDaten
      //       ?? CompressData (cDaten)
   ENDIF
   
   Inkey( .2 )
   Fwrite( x, "AT" + " RETURNED " + Strtran( Strtran( cDaten, Chr( 13 ), "" ), Chr( 10 ), "" ) + CRLF )
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   WRITE COMM oComm DATA "ATQ0V1E0" + Chr( 13 )
   GETINBUFFER oComm into nByte
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   IF nByte > 0

      Inkey( .2 )

      IF nByte != oComm:readport( nByte, @cDaten )
         ? 'Error reading COM1:'
      ENDIF

   ENDIF
   
   Inkey( .2 )
   Fwrite( x, "ATQ0V1E0" + " RETURNED " + Strtran( Strtran( cDaten, Chr( 13 ), "" ), Chr( 10 ), "" ) + CRLF )
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   WRITE COMM oComm DATA "AT+GMM" + Chr( 13 )
   GETINBUFFER oComm into nByte
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   IF nByte > 0
      
      Inkey( .2 )
      IF oComm:readport( nByte, @cDaten ) < 0
         ? 'Error reading COM1:'
      ENDIF

   ENDIF
   
   Inkey( .2 )
   Fwrite( x, "AT &F E0 &C1 &D2 V1 S0=0\V1" + ;
           " RETURNED " + Strtran( Strtran( cDaten, Chr( 13 ), "" ), Chr( 10 ), "" ) + CRLF )
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   WRITE COMM oComm DATA "AT+FCLASS=?" + Chr( 13 )
   GETINBUFFER oComm into nByte
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   IF nByte > 0
      
      Inkey( .2 )

      IF oComm:readport( nByte, @cDaten ) < 0
         ? 'Error reading COM1:'
      ENDIF

   ENDIF
   
   Inkey( .2 )
   Fwrite( x, "AT+GMM" + " RETURNED " + Strtran( Strtran( cDaten, Chr( 13 ), "" ), Chr( 10 ), "" ) + CRLF )
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   WRITE COMM oComm DATA "AT#CLS=?" + Chr( 13 )
   GETINBUFFER oComm into nByte
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   IF nByte > 0
      
      Inkey( .2 )

      IF oComm:readport( nByte, @cDaten ) < 0
         ? 'Error reading COM1:'
      ENDIF

   ENDIF
   
   Inkey( .2 )

   Fwrite( x, "AT#CLS" + ;
           " RETURNED " + Strtran( Strtran( cDaten, Chr( 13 ), "" ), Chr( 10 ), "" ) + CRLF )
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   WRITE COMM oComm DATA "AT+GCI?" + Chr( 13 )

   GETINBUFFER oComm into nByte
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   IF nByte > 0
      
      Inkey( .2 )

      IF oComm:readport( nByte, @cDaten ) < 0
         ? 'Error reading COM1:'
      ENDIF

   ENDIF
   
   Inkey( .2 )
   Fwrite( x, "AT+FCLASS" + ;
           " RETURNED " + Strtran( Strtran( cDaten, Chr( 13 ), "" ), Chr( 10 ), "" ) + CRLF )

   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   WRITE COMM oComm DATA "AT+GCI=?" + Chr( 13 )

   GETINBUFFER oComm into nByte
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   IF nByte > 0
      
      Inkey( .2 )

      IF oComm:readport( nByte, @cDaten ) < 0
         ? 'Error reading COM1:'
      ENDIF

   ENDIF
   
   Inkey( .2 )
   Fwrite( x, "AT+GCI?" + ;
           " RETURNED " + Strtran( Strtran( cDaten, Chr( 13 ), "" ), Chr( 10 ), "" ) + CRLF )
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   WRITE COMM oComm DATA "ATI1" + Chr( 13 )

   GETINBUFFER oComm into nByte
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   IF nByte > 0
      
      Inkey( .2 )

      IF oComm:readport( nByte, @cDaten ) < 0
         ? 'Error reading COM1:'
      ENDIF

   ENDIF
   
   Inkey( .2 )
   Fwrite( x, "AT+GCI=?" + ;
           " RETURNED " + Strtran( Strtran( cDaten, Chr( 13 ), "" ), Chr( 10 ), "" ) + CRLF )
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   WRITE COMM oComm DATA "ATI2" + Chr( 13 )

   GETINBUFFER oComm into nByte
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   IF nByte > 0
      
      Inkey( .2 )

      IF oComm:readport( nByte, @cDaten ) < 0
         ? 'Error reading COM1:'
      ENDIF

   ENDIF
   
   Inkey( .2 )
   Fwrite( x, "ATI1" + ;
           " RETURNED " + Strtran( Strtran( cDaten, Chr( 13 ), "" ), Chr( 10 ), "" ) + CRLF )
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   WRITE COMM oComm DATA "ATI3" + Chr( 13 )

   GETINBUFFER oComm into nByte
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   IF nByte > 0
      
      Inkey( .2 )

      IF oComm:readport( nByte, @cDaten ) < 0
         ? 'Error reading COM1:'
      ENDIF

   ENDIF
   
   Inkey( .2 )
   Fwrite( x, "ATI2" + ;
           " RETURNED " + Strtran( Strtran( cDaten, Chr( 13 ), "" ), Chr( 10 ), "" ) + CRLF )
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   WRITE COMM oComm DATA "ATI4" + Chr( 13 )

   GETINBUFFER oComm into nByte
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   IF nByte > 0
      
      Inkey( .2 )

      IF oComm:readport( nByte, @cDaten ) < 0
         ? 'Error reading COM1:'
      ENDIF

   ENDIF
   
   Inkey( .2 )
   Fwrite( x, "ATI3" + ;
           " RETURNED " + Strtran( Strtran( cDaten, Chr( 13 ), "" ), Chr( 10 ), "" ) + CRLF )
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   WRITE COMM oComm DATA "ATI5" + Chr( 13 )

   GETINBUFFER oComm into nByte

   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)
   IF nByte > 0
      
      Inkey( .2 )

      IF oComm:readport( nByte, @cDaten ) < 0
         ? 'Error reading COM1:'
      ENDIF

   ENDIF
   
   Inkey( .2 )
   Fwrite( x, "ATI4" + ;
           " RETURNED " + Strtran( Strtran( cDaten, Chr( 13 ), "" ), Chr( 10 ), "" ) + CRLF )

   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   WRITE COMM oComm DATA "ATI6" + Chr( 13 )

   GETINBUFFER oComm into nByte

   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)
   IF nByte > 0
      
      Inkey( .2 )

      IF oComm:readport( nByte, @cDaten ) < 0
         ? 'Error reading COM1:'
      ENDIF

   ENDIF
   
   Inkey( .2 )
   Fwrite( x, "ATI5" + ;
           " RETURNED " + Strtran( Strtran( cDaten, Chr( 13 ), "" ), Chr( 10 ), "" ) + CRLF )
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   WRITE COMM oComm DATA "ATI7" + Chr( 13 )

   GETINBUFFER oComm into nByte
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   IF nByte > 0
      
      Inkey( .2 )

      IF oComm:readport( nByte, @cDaten ) < 0
         ? 'Error reading COM1:'
      ENDIF

   ENDIF
   
   Inkey( .2 )
   Fwrite( x, "ATI6" + ;
           " RETURNED " + Strtran( Strtran( cDaten, Chr( 13 ), "" ), Chr( 10 ), "" ) + CRLF )

   GETINBUFFER oComm into nByte
   GETMODEMstatus(oComm:nHandle,@a,@b,@c,@d)
   tracelog(a,b,c,d)

   IF nByte > 0
      
      Inkey( .2 )

      IF oComm:readport( nByte, @cDaten ) < 0
         ? 'Error reading COM1:'
      ENDIF

   ENDIF
   
   Inkey( .2 )

   Fwrite( x, "ATI7" + ;
           " RETURNED " + Strtran( Strtran( cDaten, Chr( 13 ), "" ), Chr( 10 ), "" ) + CRLF )
   
   CLOSE COMM oComm
   Fclose( x )
   QUIT

RETURN NIL

FUNCTION CompressData( cDaten )

LOCAL cRet
LOCAL i
   cRet := ''
   FOR i := 1 TO Len( cDaten )
      IF ( i % 3 ) = 1
         cRet += Substr( cDaten, i, 1 )
      ENDIF
   NEXT i
RETURN cRet

*+ EOF: QUERYCLS.PRG
