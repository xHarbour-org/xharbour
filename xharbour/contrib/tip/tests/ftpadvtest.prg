/******************************************
* TIP test
* FTP Advanced operations Test
*
* $Id$
*****/

PROCEDURE MAIN( cUrl )
   LOCAL oCon, oUrl

   oUrl := tURL():New( cUrl )
   IF Empty( oUrl )
      ? "Invalid url " + cUrl
      ?
      QUIT
   ENDIF

   IF oUrl:cProto != "ftp"
      ? 'This is a "DELE" test for ftp.'
      ? 'Use an ftp address with a file that you can delete.'
      ?
      QUIT
   END

   oCon := TipClient():New( oUrl )
   oCon:nConnTimeout := 20000
   ? "Connecting with", oUrl:cServer
   IF oCon:Open( cUrl )
      ? "Connection eshtablished"
      ? "Deleting", oUrl:cPath
      IF oCon:Dele( oUrl:cPath + "/" + oUrl:cFile )
         ? "Success"
      ELSE
         ? "Faliure (server reply:", oCon:cReply + ")"
      ENDIF
      oCon:Close()
   ELSE
      ? "Can't connect with", oUrl:cServer
      IF InetErrorCode( oCon:SocketCon ) == 0
         ? "Server sayed:", oCon:cReply
      ELSE
         ? "Error in connection:", InetErrorDesc( oCon:SocketCon )
      ENDIF
   END

   ? "Done"
   ?
RETURN
