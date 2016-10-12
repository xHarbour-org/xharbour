/*
* RunScript
* Utility tool for pushing large script files into SQL databases
* Copyright (c) 2003 - Marcelo Lombardo  marcelo@xharbour.com.br
* All Rights Reserved
*/

#include "sqlrdd.ch"
#include "sqlodbc.ch"

#define CRLF   chr(13)+chr(10)

#define SQL_DBMS_NAME                       17
#define SQL_DBMS_VER                        18

REQUEST SQLRDD
REQUEST SR_ODBC

static cBuffer := ""
static lUnicode

/*------------------------------------------------------------------------*/

Function Main( cScriptFile, cLogFile, nToCommit )

   local oSql, h1, h2, cSql, nCnn, nRet, nIssued, nErrors, j

   ? ""
   ? Replicate( "-", 79 )
   ? ""
   ? "RunScript - tool for pushing large script files into SQL Databases"
   ? ""
   ? "(c) 2003 - Marcelo Lombardo - marcelo@xharbour.com.br"
   ? ""
   ? "About the scrip file:"
   ? ""
   ? "You must have at least one blank line or a semicolon (;) involving each SQL"
   ? "command as a separator. If the database rejects any SQL statement, it will"
   ? "be added to the log file, as well as the error message."
   ? ""
   ? Replicate( "-", 79 )
   ? ""

   Connect()

   ? "Connected to :", SR_GetConnectionInfo(, SQL_DBMS_NAME ), SR_GetConnectionInfo(, SQL_DBMS_VER )

   oSql := SR_GetConnection()

   If empty( cScriptFile )
      ? "Usage: runscript <ScriptFile> [<LogFile>] [<CommitEveryN>]"
      ? ""
      ? "where:"
      ? " CommitEveryN to issue a COMMIT after processing N commands. Default is 1000"
      ? ""
      ? "ex.: runscript demodata.sql logs.txt"
      ? ""
      ? Replicate( "-", 79 )
      ? ""
      Return
   EndIf

   /* Check for files */
   If !file( cScriptFile )
      ? "Script input file not found."
      Return
   EndIf

   h1 = fopen( cScriptFile )

   if h1 < 0
      ? "Error opening the script file " + cScriptFile
      Return
   EndIf

   If nToCommit == NIL
      nToCommit := 1000
   EndIf

   If cLogFile == NIL
      cLogFile := "runscript.log"
   endif

   h2 = fcreate( cLogFile )

   if h2 < 0
      ? "Error creating log file " + cLogFile
      Return
   EndIf

   fwrite( h2, "Runscript 1.0" + CRLF + CRLF + ;
               dtoc(date()) + " " + time() + "*** Log started to run " + cScriptFile + CRLF )

   nIssued  := 0
   nErrors  := 0
   cGira    := "-\|/"
   j        := 1

   ? ""

   While !Empty( cSql := ProxStmt(h1) )

      cSql := alltrim( cSql )

      While (!empty( cSql )) .and. right( cSql, 1 ) $ chr(13) + chr(10) + ";"
         cSql := left( cSql, len(cSql)-1 )
      EndDo

      If upper(right(cSql,3)) == " GO"
         cSql := left( cSql, len(cSql)-3 )
      EndIf

      nRet := oSql:exec( cSql, .F. )

      nIssued ++

      If nRet != SQL_SUCCESS
         nErrors ++
         fwrite( h2, dtoc(date()) + " " + time() + " SQL Statement: " + CRLF + cSql + CRLF + "Error:" + CRLF + oSql:cSQLError + CRLF )
      EndIf

      If ( nIssued % nToCommit ) = 0
         oSql:Commit()
      EndIf

      @row(), 1 say "Processing (ALT+C to cancel)  " + cGira[j] + "  " + alltrim( str(nIssued) ) + " commands executed with " + alltrim( str(nErrors) ) + " error" + if(nErrors != 1,"s", "")
      j++

      if j = 5
         j := 1
      endif

   EndDo

   oSql:commit()

   fwrite( h2, CRLF + CRLF + alltrim( str(nIssued) ) + " command" + if(nIssued>1,"s", "") + " executed" + CRLF + alltrim( str(nErrors) ) + " error" + if(nErrors != 1,"s", "") + CRLF + CRLF )
   fwrite( h2, dtoc(date()) + " " + time() + "*** Log finished." )

   fclose( h1 )
   fclose( h2 )

   oSql:end()

   ? ""
   ? "Finished. Starting log view..."
   ? ""

   Run ("notepad " + cLogFile)

Return

/*------------------------------------------------------------------------*/

Function ProxStmt( h1 )

   local c, i, cOut, lEOF, lOpenQuote, lLF

   cOut := ""
   lEOF := .F.
   lLF  := .F.

   lOpenQuote := .F.

   If empty( cBuffer )
      cBuffer := ReadBlock( h1, @lEOF )
      If empty( cBuffer )
         Return ""
      EndIf
   EndIf

   c := cBuffer

   While !empty(c) .and. c[1] == chr(10)
      lLF := .T.
      c   := subStr(c,2)
   EndDo

   While .T.

      For i = 1 to len( c )
         If c[i] == "'"
            lOpenQuote := !lOpenQuote
            cOut += c[i]
         ElseIf c[i] == chr(13) .and. (!lOpenQuote)
            cOut += " "
         ElseIf c[i] == chr(255) .and. ((!lOpenQuote) .or. lUnicode )
         ElseIf c[i] == chr(254) .and. ((!lOpenQuote) .or. lUnicode )
         ElseIf c[i] == chr(0)   .and. ((!lOpenQuote) .or. lUnicode )
         ElseIf c[i] == chr(10)  .and. (!lOpenQuote) .and. (!lLF)
            cOut += " "
            lLF := .T.
         ElseIf ((c[i] = chr(10) .and. lLF) .or. c[i] == ";") .and. !Empty( alltrim(cOut) ) .and. (!lOpenQuote)
            cBuffer := SubStr( c, i+1 )
            Return cOut
         Else
            cOut += c[i]
            lLF := .F.
         EndIf

      Next

      If lEOF
         cBuffer := ""
         Return alltrim( cOut )
      EndIf

      c := ReadBlock( h1, @lEOF )

      If empty( c )
         cBuffer := ""
         Return alltrim( cOut )
      EndIf

   EndDo

Return ""

/*------------------------------------------------------------------------*/

Function ReadBlock( h1, lEOF )

   local n, c := space(4096)

   lEOF := .f.
   n := fread( h1, @c, 4096 )

   If n < 4096
      lEOF := .T.
   EndIf

   If lUnicode == NIL
      If left( c, 2 ) == chr(255) + chr(254)
         lUnicode := .T.
      Else
         lUnicode := .F.
      EndIf
   EndIf

Return c

/*------------------------------------------------------------------------*/

#include "connect.prg"

/*------------------------------------------------------------------------*/
