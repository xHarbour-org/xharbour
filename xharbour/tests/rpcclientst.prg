************************************************************
* rpcclientst.prg
* $Id$
* Test for tRpcClient class
*
* SINGLE THREAD MODE
*
* This demonstrates how to use broadcasting and remote calling
* capabilities (both in synchronous and asyncrhronous mode)
* in XHarbour.
*
* Call this program using a network broadcast address where to search
* for servers. If you have a local network with C class address (e.g.
* 192.168.x.y) change the last number with 255, and after having
* launched the server.
*
* Example: rpcclientst 192.168.1.255
*
* Giancarlo Niccolai
*

GLOBAL nRow
GLOBAL lComplete

PROCEDURE Main( cNetwork )
   LOCAL oRpc, aElem, cFname
   LOCAL oResult, nPos
   LOCAL cBase, nCount
   LOCAL nSum, nNumber, oElem

   CLEAR SCREEN

   @1,6 SAY "X H A R B O U R - Remote Procedure Call client test (Single Thread)"

   // default scan network to local machine if not given
   IF cNetwork == NIL
      cNetwork := "127.0.0.255"
   ENDIF

   // creating a client
   oRpc := tRpcClient( cNetwork )

   @3,5 SAY "Scanning (sync) for server in the network " + cNetwork
   nRow := 4

   //we'll give 2,5 secs time to the servers to present themselves
   // since asyncrhonous mode is off by default, you HAVE to provide a
   // finite timeout.
   oRpc:ScanServers( ".*", 2500 )

   // display found servers
   FOR EACH aElem in oRpc:aServers
      IF nRow > 7
         @nRow, 10 SAY "... And more"
         EXIT
      ENDIF

      @nRow, 10 SAY "Found server: " + aElem[2] + " at address " + aElem[1]
      nRow ++
   NEXT

   // activating asynchronous mode
   //oRpc:lAsyncMode := .T.

   // events can be caught also by overloading members, but you can also
   // use codeblocks
   oRpc:bOnScanFunctionsProgress := {|x| FoundFunction( x ) }
   oRpc:bOnScanComplete := { || ScanComplete() }
   lComplete := .F.

   nRow ++
   @nRow, 5 SAY "Scanning for functions called Check* (any version, async mode)"
   nRow ++

   // now a timeout is optional, and we could even use a oRpc:StopScan() from
   // a control thread
   oRpc:ScanFunctions( "Check.*", "00000000.0", 2500 )

   nPos := 10
   /*
   DO WHILE .not. lComplete
      @nRow, nPos SAY "."
      nPos++
      //ThreadSleep( 250 )
   ENDDO
   */
   // turning sync mode back on for simplicity reasons
   //oRpc:lAsyncMode := .F.

   // please, note that callback functions are called also in sync mode
   IF Empty( oRpc:aFunctions )
      nRow++
      @nRow, 5 SAY "No server found. Terminating (press a key)"
      Inkey(0)
      QUIT
   ENDIF

   nRow ++
   @nRow, 5 SAY "Connecting with " + oRpc:aFunctions[1][2]
   nRow ++
   // Demo server has a fairly symple authorization scheme ;-)
   // remember to use the address, not the logical rpc name of the server
   oRpc:SetEncryption( "A nice key to be used by servers" )
   IF oRpc:Connect( oRpc:aFunctions[1][1], "Giancarlo", "Niccolai", 2500 )
      @nRow, 10 SAY "Connection established"
      nRow ++
      cFname = oRpc:GetFunctionName( 1 )
      @nRow, 10 SAY "Calling Function " + cFname
      nRow++

      oResult := oRpc:Call( cFname, {"asdfasdfwefasdfawerasdf"}, 2500 )

      IF Empty( oResult )
         @nRow, 10 SAY "Function call failed"
      ELSE
         @nRow, 10 SAY "Result of " + cFname +": " + oResult
      ENDIF
      nRow ++

      @nRow, 5 SAY "Test of Self defined loop call"
      nRow++

      // test of loop

      oResult := oRpc:CallForeachSummary( cFname, { "abc", "123", "xyz", "456"}, { "$." }, 2500 )
      IF .not. Empty ( oResult )
         nPos := 45
         @nRow, 10 SAY "Results for abc, 123, xyz, 456: "
         FOR EACH oElem in oResult
            @nRow, nPos SAY AllTrim( oElem )
            nPos +=5
         NEXT
      ELSE
         @nRow, 10 SAY "Test Failed"
      ENDIF

      nRow++

      // tryng again asyncrhonous mode, and now autmoatic compression feature
      //oRpc:lAsyncMode := .T.
      lComplete := .F.
      // again, notice that this handler are called also if the mode is sync,
      // but they are almost essential in async mode.
      oRpc:bOnFunctionProgress := {|x,y| Progress( x, y ) }
      oRpc:bOnFunctionReturn := { |x| FuncComplete( x ) }
      oRpc:bOnFunctionFail := { |x| FuncHadFailed( x ) }

      // now building a 512 lengt string, that will be compresed
      cBase := ""
      nSum := 0
      FOR nCount := 1 TO 512
         nNumber := Int( asc( "a" ) + HB_Random(4) )
         cBase += chr( nNumber )
         nSum += nNumber
      NEXT
      @nRow, 5 SAY "Async/compressed call test (expecting result: " + AllTrim( Str(nSum))+")"
      nRow ++


      oResult := oRpc:Call( cFname, { cBase }, 150000 )
      nPos := 28
      /*
      @nRow, 20 SAY "Waiting"
      DO WHILE .not. lComplete
         @nRow, nPos SAY "."
         nPos++
         //ThreadSleep( 100 )
         IF nPos > 120
            nRow++
            @nRow,10 say "Async test failed"
            lComplete := .T.
         ENDIF
      ENDDO
      */
   ELSE
      @nRow, 10 SAY "Can't Connect with  " + oRpc:aFunctions[1][2]
      nRow++
   ENDIF

   oRpc:Destroy()
   nRow ++
   @nRow, 5 SAY "DONE - Press any key to terminate"
   Inkey( 0 )
RETURN


/* Callback called when the system intercepts a function available on a server */
FUNCTION FoundFunction( aElem )
   @nRow, 10 SAY "Func " + aElem[3] + " (from "+ aElem[2] + " at " + aElem[1] +")"
   nRow ++
   // returning .F. would terminate the search
RETURN .T.

/* Called when a scan (server or function) has been completed */
FUNCTION ScanComplete()
   lComplete := .T.
   @nRow, 10 SAY "Scan function complete"
   nRow ++
RETURN .T.


/* Called when the Remote Procedures signals a significant progress. Also called when
   server starts the RPC (with oData == NIL and nProgressInd == 0 ) */
FUNCTION Progress( nProgressInd, oData )
   IF Empty( oData )
      @nRow,5 SAY AllTrim( Str( nProgressInd ) ) +"%"
   ELSE
      // oData can be anything, but we know our test function returns just a string
      @nRow, 5 SAY AllTrim( Str( nProgressInd ) ) +"% (" + AllTrim( oData ) + ")"
   ENDIF
RETURN .T.

/* Called when function is complete */
FUNCTION FuncComplete( oResult  )
      // oResult can be anything, but we know our test function returns just a string
      lComplete := .T.
      nRow++
      @nRow, 10 SAY "Function complete, result: " + oResult
      nRow++
RETURN .T.

/* Called when function is complete */
FUNCTION FuncHadFailed( cDesc  )
      lComplete := .T.
      nRow++
      @nRow, 10 SAY "Function FAILED internally! " 
      nRow++
RETURN .T.
