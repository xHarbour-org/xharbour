*****************************************************
* TEST of TIP libs (for higher level URI interface)
*
* $Id: tiptest.prg,v 1.2 2003/06/22 04:43:57 jonnymind Exp $
*
* Usage: This file is similar to a wget command
*
* tiptest <URI> [filename]
*
* Without the filename, tiptest will be in demo mode,
* just demostrating it is working
*
* With the filename, data will be stored to the file or
* retreived from the file and sent to internet.
*
* Usage of URI.
* HTTP Protocol
*   http://<sitename>/<path>?<query>
*   - at the moment HTTP URI is not able to send data,
*     (e.g. a form)
*
* POP Protocol
*    pop://<username>:<password>@<popserver>/[-][MsgNum]
*    - Witout MsgNum, you get the list of messages
*    - With MsgNum get Message MsgNum
*    - With -MsgNum deletes message MsgNum
*
* SMTP Protocol
*    smtp://<mail-from>@<server>/RCPT
*    - (You have to provide a filename)
*    - use &at; in mail-from message
*    - Send the letter in filename (that must include
*      headers) to RCPT e.f.
*      stmp://user&at;myprovider.com@smtp.myprovider.com/gian@niccolai.ws
*
*      NOTE: IN UNIX, to use & from command line you have to surround
*      the url with "", eg "smtp://...&at;...@server/dest"
*
* FTP Protocol
*    ftp://user:passwd@<ftpserver>/[+][<path>]
*    - without path, get the list of files (use path/ to get the list of
*      files in a dir.
*    - with path, get a file
*    + with +path, send a file (using the filename command line)
*

#include "hbclass.ch"
#include "tip.ch"

PROCEDURE MAIN( cUrl, cFile )
   LOCAL bWrite := .F.
   LOCAL oUrl, oClient
   LOCAL cData

   Set Color to w+/b
   CLEAR SCREEN
   @1,6 SAY "X H A R B O U R - TIP (class based internet client protocol) test"


   IF Empty( cUrl )
      @4,5 SAY "USAGE: tipclient <URI> [dumpToOrFromFileName]"
      Terminate()
   ENDIF

   oUrl := tURL():New( cUrl )
   IF Empty( oUrl )
      @4,5 SAY "Invalid url " + cUrl
      Terminate()
   ENDIF

   oClient := tIPClient():New( oUrl )
   oClient:nConnTimeout := 20000


   IF Empty( oClient )
      @4,5 SAY "Invalid url " + cUrl
      Terminate()
   ENDIF

   @4,5 SAY "Connecting to " + oUrl:cProto + "://" + oUrl:cServer
   IF oClient:Open( oUrl )
      IF Empty( oClient:cReply )
         @5,5 SAY "Connection status: <connected>"
      ELSE
         @5,5 SAY "Connection status: " + oClient:cReply
      ENDIF

      IF .not. Empty( cFile ) .and. cFile[1] == '+'
         cFile := Substr( cFile, 2 )
         bWrite := .T.
      ENDIF

      IF oClient:nAccessMode == TIP_WO .or. ( oClient:nAccessMode == TIP_RW .and. bWrite )
         IF oClient:WriteFromFile( cFile )
            @7,5 SAY "Data sucessfully sent"
         ELSE
            @7,5 SAY "ERROR: Data not sent"
         ENDIF
      ELSE
         IF Empty( cFile )
            cData := oClient:Read()
            IF .not. Empty( cData )
               @7,5 SAY "First 80 characters:"
               ? Trim(SubStr( cData, 1, 80 ))
            ELSE
               @7,5 SAY "ERROR - file can't be retreived"
            ENDIF
         ELSE
            IF oClient:ReadToFile( cFile )
               @7,5 SAY "File "+ cFile + " written."
            ELSE
               @7,5 SAY "Generic error in writing."  + cFile
            ENDIF
         ENDIF
      ENDIF

      oClient:Close()
      IF Empty( oClient:cReply )
         @22, 5 SAY "Done: (no goodbye message)"
      ELSE
         @22, 5 SAY "Done: " + oClient:cReply
      ENDIF
   ELSE
      @5,5 SAY "Can't open URI " + cUrl
      IF .not. Empty( oClient:cReply )
         @6,5 SAY oClient:cReply
      ENDIF
   ENDIF

   Terminate()
RETURN

PROCEDURE Terminate()
   @23,18 SAY "Program done - Press a key to terminate"
   Inkey( 0 )
   @24,0
   QUIT
RETURN

