***********************************************************
* process.prg
* $Id: process.prg,v 1.1 2003/08/26 15:08:51 jonnymind Exp $
* Test for process oriented functions
* Read from streams and manage sub processes.
*
* Param cProc: the command line to launch
* Param cSend: A string to send as standard input to
* the process
* Giancarlo Niccolai
*

PROCEDURE Main( cProc, cSend )
   LOCAL hIn, hOut, hErr
   LOCAL cData, hProc, nLen

   IF cProc == NIL
      cProc := "xterm"
   ENDIF

   IF cSend == NIL
      cSend := "Hello world"
   ENDIF

   CLEAR SCREEN
   ? " X H A R B O U R - Process management test "

   ? "Launching process", cProc
   hProc := HB_OpenProcess( cProc , @hIn, @hOut, @hErr )
   ? "Process handler:", hProc
   ? "Error: ", FError()

   ? "Sending", cSend
   FWrite( hIn, cSend )

   ? "To kill the application, press a key"
   Inkey(0)
   ? "Result of the kill", HB_CloseProcess( hProc )

   ? "Reading output"
   cData := Space( 1000 )
   nLen := Fread( hOut, @cData, 1000 )
   ? "Received", nLen, "bytes"
   IF nLen > 0 .and. nLen < 200
      ? "Dumping them: ", SubStr( cData, 1, nLen )
   ENDIF

   ? "Reading errors"
   cData := Space( 1000 )
   nLen := Fread( hErr, @cData, 1000 )
   ? "Received", nLen, "bytes"
   IF nLen >0 .and. nLen < 200
      ? "Dumping them: ", SubStr( cData, 1, nLen )
   ENDIF

   ? "Waiting for process termination"
   ? "Return code", HB_ProcessValue( hProc )

   FClose( hProc )
   FClose( hIn )
   FClose( hOut )
   FClose( hErr )

   ? "Done. Press any key to terminate"
   Inkey(0)

   ?
RETURN