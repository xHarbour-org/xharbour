/* $Id$ */

#include "common.ch"
#include "dbstruct.ch"
#include "hbclass.ch"

#include "vxhdebug.ch"

#define TAB Chr( 9 )
#define EOL Chr( 10 )

CLASS XHDebugger
  DATA socket

  /* UI elements */
  DATA oConsole
  DATA oMonitor
  DATA oCallStack
  DATA oWatch
  DATA oWorkArea
  DATA oSets

  DATA oTimer

  /* Debugger state */
  DATA nLine
  DATA cModule
  DATA cFunction
  DATA lDone      INIT .F.
  DATA lSyncing   INIT .F.
  DATA lListening INIT .F.
  DATA lStopped   INIT .F.

  DATA aBreak INIT {}
  DATA aVars  INIT {}
  DATA aCalls INIT {}

  /* User configurable preferences */
  DATA lShowCalls      INIT .T.
  DATA lShowGlobals    INIT .F.
  DATA lShowLocals     INIT .F.
  DATA lShowPrivates   INIT .F.
  DATA lShowPublics    INIT .F.
  DATA lShowStatics    INIT .F.
  DATA lShowAllGlobals INIT .F.

  METHOD New()
  METHOD AddPoint( lTrace, cExpr )
  METHOD ConnectionError()
  METHOD Continue() INLINE ::Do( "continue" ), ::Listen( .T. )
  METHOD Do( cCommand )
  METHOD DoConsoleCommand( cCommand )
  METHOD Frame( nFrame )
  METHOD GetSourceFiles()
  METHOD Invoke()   INLINE ::Do( "invoke" )
  METHOD IsValidStopLine( cFile, nLine )
  METHOD Listen( lFlag )
  METHOD Next()     INLINE ::Do( "next" ), ::Listen( .T. )
  METHOD ProcessOutput()
  METHOD ReadAreas()
  METHOD ReadBreak()
  METHOD ReadCalls()
  METHOD ReadExpressionValue( cExpr ) INLINE ::Do( "??" + cExpr ), ::ReadValue()
  METHOD ReadHashKeys( cExpr)
  METHOD ReadObjectMessages( cExpr )
  METHOD ReadSets()
  METHOD ReadValue()
  METHOD ReadVars()
  METHOD ReadVarValue( aVar )
  METHOD ReadWatches( lSingle )
  METHOD ReadWorkAreaInfo( n )
  METHOD ReadWorkAreaRecord( n, nFCount )
  METHOD RecvUntil( cEndBlock )
  METHOD Start( cAddress, nPort )
  METHOD Step()     INLINE ::Do( "step" ), ::Listen( .T. )
  METHOD StepOut()  INLINE ::Do( "stepout" ), ::Listen( .T. )
  METHOD Stop()
  METHOD SyncCalls()
  METHOD SyncVars()
  METHOD ToggleBreak()
  METHOD Until( cFile, nLine ) INLINE ::Do( "until " + cFile + ":" + LTrim( Str( nLine ) ) ), ::Listen( .T. )
  METHOD UnWatch( n ) INLINE ::Do( "unwatch " + LTrim( Str( n ) ) ), ::RecvUntil( ">" )
  METHOD WorkAreaInfoText( nAreaNo, hInfo, aStruct, aRecord )
  METHOD OnStart() VIRTUAL
  METHOD OnStop() VIRTUAL
ENDCLASS


METHOD New() CLASS XHDebugger
  InetInit()
  ::oTimer := XHDebugTimer():new( ::oConsole, {|| ::ProcessOutput() } )
RETURN Self


METHOD AddPoint( lTrace, cExpr ) CLASS XHDebugger
  LOCAL nNumber

  ::Do( "wp " + cExpr )
  nNumber := ::ReadWatches( .T. )[ 1 ][ _NUMBER ]
  IF lTrace
    ::Do( "tp " + LTrim( Str( nNumber ) ) )
    ::RecvUntil( ">" )
  ENDIF
RETURN Self


METHOD ConnectionError() CLASS XHDebugger
  IF ::socket != NIL
    ::oConsole:Out( InetErrorDesc( ::socket ) )
  ENDIF
  IF ::lListening
    ::oTimer:Stop()
    ::Listen( .F. )
  ENDIF
  ::socket := NIL
  ::Stop()
RETURN Self


METHOD Do( cCommand ) CLASS XHDebugger
  IF ::socket != NIL
    ::lDone := .F.
    InetSend( ::socket, cCommand + Chr( 13 ) + EOL )

    // This is a patch to prevent ::aTabs[ new ]:ShowUp in vxhdgui.prg line 165 to be fired after continue is clicked
    // this not an actual FIX must be checked by debugger team
    // Augusto

    //IF cCommand == "continue"
    //   ::socket := NIL
    //ENDIF

  ENDIF
RETURN Self


METHOD DoConsoleCommand( cCommand ) CLASS XHDebugger
  IF Left( cCommand, 2 ) == "??"
    ::Inspect( "(" + AllTrim( SubStr( cCommand, 3 ) ) + ")" )
    ::oConsole:Out( ">" )
  ELSE
    ::Do( cCommand )
    ::Listen( .T. )
  ENDIF
RETURN Self


METHOD Frame( nFrame ) CLASS XHDebugger
  LOCAL aFrame := ::aCalls[ -( nFrame + 1 ) ]

  IF ::socket != NIL
    ::Do( "frame " + LTrim( Str( nFrame ) ) )
    ::RecvUntil( ">" )
    ::cFunction := aFrame[ _FUNCTION ]
    ::cModule   := aFrame[ _MODULE ]
    ::nLine     := aFrame[ _LINENO ]
    ::Sync( .F. )
  ENDIF
RETURN Self


METHOD GetSourceFiles() CLASS XHDebugger
  IF ::socket != NIL
    ::Do( ".sources" )
    RETURN &( ::RecvUntil( ">" ) )
  ENDIF
RETURN {}


METHOD IsValidStopLine( cFile, nLine ) CLASS XHDebugger
  IF ::socket != NIL
    ::Do( ".valid " + cFile + ":" + LTrim( Str( nLine ) ) )
    RETURN Left( ::RecvUntil( ">" ), 3 ) == ".T."
  ENDIF
RETURN .F.


METHOD Listen( lFlag ) CLASS XHDebugger
  IF lFlag
    ::oTimer:Start()
  ELSE
    ::oTimer:Stop()
  ENDIF
  ::lListening := lFlag
RETURN Self


METHOD ProcessOutput() CLASS XHDebugger
  LOCAL cString := "", s, n, socket := ::socket
  LOCAL cMessage

  IF socket != NIL .AND. InetErrorCode( socket ) == 0
    s := Space( 32 )
    DO WHILE InetErrorCode( socket ) == 0 .AND. InetDataReady( socket, 100 ) > 0
      n := InetRecv( socket, @s )
      cString += Left( s, n )
    ENDDO
    IF Len( cString ) > 0
      cString += EOL
      DO WHILE ( n := At( EOL, cString ) ) > 0
        cMessage := Left( cString, n - 1 )
        cString := SubStr( cString, n + 1 )
        ::oConsole:Out( StrTran( cMessage, TAB, " " ) )
        DO CASE
          CASE Left( cMessage, 5 ) == "line" + TAB
            ::nLine := Val( SubStr( cMessage, 6 ) )
          CASE Left( cMessage, 7 ) == "module" + TAB
            cMessage := SubStr( cMessage, 8 )
            n := RAt( ':', cMessage )
            ::cModule := Left( cMessage, n - 1 )
            ::cFunction := SubStr( cMessage, n + 1 )
          CASE cMessage == ">"
            IF ::lListening
              ::Listen( .F. )
            ENDIF
            ::lDone := .T.
            ::Sync()
        ENDCASE
      ENDDO
    ENDIF
  ELSE
    ::ConnectionError()
  ENDIF
RETURN Self


METHOD ReadAreas() CLASS XHDebugger
  LOCAL cString := ""
  LOCAL cMessage, n, n1, aAreas := {}

  IF ::socket == NIL
    RETURN Self
  ENDIF
  cString := ::RecvUntil( ">" )
  IF InetErrorCode( ::socket ) != 0
    ::ConnectionError()
    RETURN Self
  ENDIF
  IF Len( cString ) > 0
    DO WHILE ( n := At( EOL, cString ) ) > 0
      cMessage := Left( cString, n - 1 )
      cString := SubStr( cString, n + 1 )
      n := At( TAB, cMessage )
      n1 := At( TAB, cMessage, n + 1 )
      AAdd( aAreas, { /* _NUMBER */ Val( SubStr( cMessage, n + 1 ) ), ;
                      /* _ALIAS */  SubStr( cMessage, n1 + 1 ) } )
    ENDDO
  ENDIF
RETURN aAreas


METHOD ReadBreak() CLASS XHDebugger
  LOCAL cString := "", cBreak, nBreak, n
  LOCAL cMessage

  IF ::socket == NIL
    RETURN Self
  ENDIF
  cString := ::RecvUntil( EOL )
  IF InetErrorCode( ::socket ) != 0
    ::ConnectionError()
    RETURN Self
  ENDIF
  IF Len( cString ) > 0
    cMessage := cString
    DO CASE
      CASE Left( cMessage, 6 ) == "break" + TAB
        nBreak := Val( SubStr( cMessage, 7 ) )
        cBreak := StrTran( SubStr( cMessage, At( TAB, cMessage, 7 ) + 1 ), TAB, ":" )
        IF ( n := AScan( ::aBreak, {|x| x[1] == nBreak } ) ) > 0
          ::aBreak[n][2] := cBreak
        ELSE
          AAdd( ::aBreak, { nBreak, cBreak } )
        ENDIF
    ENDCASE
  ENDIF
RETURN Self


METHOD ReadCalls() CLASS XHDebugger
  LOCAL cString := ""
  LOCAL cMessage, n, n1, aCall

  IF ::socket == NIL
    RETURN Self
  ENDIF
  cString := ::RecvUntil( ">" )
  IF InetErrorCode( ::socket ) != 0
    ::ConnectionError()
    RETURN Self
  ENDIF
  IF Len( cString ) > 0
    DO WHILE ( n := At( EOL, cString ) ) > 0
      cMessage := Left( cString, n - 1 )
      cString := SubStr( cString, n + 1 )
      n := At( TAB, cMessage )
      n1 := At( TAB, cMessage, n + 1 )
      aCall := { /* _MODULE */   Left( cMessage, n - 1 ), ;
                 /* _LINENO */   Val( SubStr( cMessage, n + 1 ) ), ;
                 /* _FUNCTION */ SubStr( cMessage, n1 + 1 ) }
      AAdd( ::aCalls, aCall )
    ENDDO
  ENDIF
RETURN Self


METHOD ReadHashKeys( cExpr ) CLASS XHDebugger
RETURN &( SubStr( ::ReadExpressionValue( "XHD_VAL_TO_STR(HGETKEYS(" + cExpr + "))" ), 2 ) )


METHOD ReadObjectMessages( cExpr ) CLASS XHDebugger
RETURN &( SubStr( ::ReadExpressionValue( "XHD_VAL_TO_STR(__OBJGETMSGLIST(" + cExpr + ",.T.))" ), 2 ) )


METHOD ReadSets() CLASS XHDebugger
  LOCAL cExpr, i
  LOCAL aSets, aOut

  cExpr := "XHD_VAL_TO_STR({"
  FOR i := 1 TO _SET_COUNT
    cExpr += "SET(" + LTrim( Str( i ) ) + "),"
  NEXT
  FOR i := _SET_COUNT + 1 TO HB_SET_BASE - 1
    cExpr += ","
  NEXT
  FOR i := HB_SET_BASE TO HB_SET_BASE + HB_SET_COUNT - 1
    cExpr += "SET(" + LTrim( Str( i ) ) + "),"
  NEXT
  cExpr := Left( cExpr, Len( cExpr ) - 1 ) + "})"

  aSets := &( SubStr( ::ReadExpressionValue( cExpr ), 2 ) )
  aOut := { { _SET_EXACT, "Exact", aSets[ _SET_EXACT ] }, ;
            { _SET_FIXED, "Fixed", aSets[ _SET_FIXED ] }, ;
            { _SET_DECIMALS, "Decimals", aSets[ _SET_DECIMALS ] }, ;
            { _SET_DATEFORMAT, "DateFormat", aSets[ _SET_DATEFORMAT ] }, ;
            { _SET_EPOCH, "Epoch", aSets[ _SET_EPOCH ] }, ;
            { _SET_PATH, "Path", aSets[ _SET_PATH ] }, ;
            { _SET_DEFAULT, "Default", aSets[ _SET_DEFAULT ] }, ;
            { _SET_EXCLUSIVE, "Exclusive", aSets[ _SET_EXCLUSIVE ] }, ;
            { _SET_SOFTSEEK, "SoftSeek", aSets[ _SET_SOFTSEEK ] }, ;
            { _SET_UNIQUE, "Unique", aSets[ _SET_UNIQUE ] }, ;
            { _SET_DELETED, "Deleted", aSets[ _SET_DELETED ] }, ;
            { _SET_CANCEL, "Cancel", aSets[ _SET_CANCEL ] }, ;
            { _SET_DEBUG, "Debug", aSets[ _SET_DEBUG ] }, ;
            { _SET_TYPEAHEAD, "Typeahead", aSets[ _SET_TYPEAHEAD ] }, ;
            { _SET_COLOR, "Color", aSets[ _SET_COLOR ] }, ;
            { _SET_CURSOR, "Cursor", aSets[ _SET_CURSOR ] }, ;
            { _SET_CONSOLE, "Console", aSets[ _SET_CONSOLE ] }, ;
            { _SET_ALTERNATE, "Alternate", aSets[ _SET_ALTERNATE ] }, ;
            { _SET_ALTFILE, "AltFile", aSets[ _SET_ALTFILE ] }, ;
            { _SET_DEVICE, "Device", aSets[ _SET_DEVICE ] }, ;
            { _SET_EXTRA, "Extra", aSets[ _SET_EXTRA ] }, ;
            { _SET_EXTRAFILE, "ExtraFile", aSets[ _SET_EXTRAFILE ] }, ;
            { _SET_PRINTER, "Printer", aSets[ _SET_PRINTER ] }, ;
            { _SET_PRINTFILE, "PrintFile", aSets[ _SET_PRINTFILE ] }, ;
            { _SET_MARGIN, "Margin", aSets[ _SET_MARGIN ] }, ;
            { _SET_BELL, "Bell", aSets[ _SET_BELL ] }, ;
            { _SET_CONFIRM, "Confirm", aSets[ _SET_CONFIRM ] }, ;
            { _SET_ESCAPE, "Escape", aSets[ _SET_ESCAPE ] }, ;
            { _SET_INSERT, "Insert", aSets[ _SET_INSERT ] }, ;
            { _SET_EXIT, "Exit", aSets[ _SET_EXIT ] }, ;
            { _SET_INTENSITY, "Intensity", aSets[ _SET_INTENSITY ] }, ;
            { _SET_SCOREBOARD, "ScoreBoard", aSets[ _SET_SCOREBOARD ] }, ;
            { _SET_DELIMITERS, "Delimiters", aSets[ _SET_DELIMITERS ] }, ;
            { _SET_DELIMCHARS, "DelimChars", aSets[ _SET_DELIMCHARS ] }, ;
            { _SET_WRAP, "Wrap", aSets[ _SET_WRAP ] }, ;
            { _SET_MESSAGE, "Message", aSets[ _SET_MESSAGE ] }, ;
            { _SET_MCENTER, "MCenter", aSets[ _SET_MCENTER ] }, ;
            { _SET_SCROLLBREAK, "ScrollBreak", aSets[ _SET_SCROLLBREAK ] }, ;
            { _SET_EVENTMASK, "EventMask", aSets[ _SET_EVENTMASK ] }, ;
            { _SET_VIDEOMODE, "VideoMode", aSets[ _SET_VIDEOMODE ] }, ;
            { _SET_MBLOCKSIZE, "MBlockSize", aSets[ _SET_MBLOCKSIZE ] }, ;
            { _SET_MFILEEXT, "MFileExt", aSets[ _SET_MFILEEXT ] }, ;
            { _SET_STRICTREAD, "StrictRead", aSets[ _SET_STRICTREAD ] }, ;
            { _SET_OPTIMIZE, "Optimize", aSets[ _SET_OPTIMIZE ] }, ;
            { _SET_AUTOPEN, "AutOpen", aSets[ _SET_AUTOPEN ] }, ;
            { _SET_AUTORDER, "AutOrder", aSets[ _SET_AUTORDER ] }, ;
            { _SET_AUTOSHARE, "AutoShare", aSets[ _SET_AUTOSHARE ] }, ;
            { _SET_LANGUAGE, "Language", aSets[ _SET_LANGUAGE ] }, ;
            { _SET_IDLEREPEAT, "IdleRepeat", aSets[ _SET_IDLEREPEAT ] }, ;
            { _SET_FILECASE, "FileCase", aSets[ _SET_FILECASE ] }, ;
            { _SET_DIRCASE, "DirCase", aSets[ _SET_DIRCASE ] }, ;
            { _SET_DIRSEPARATOR, "DirSeparator", aSets[ _SET_DIRSEPARATOR ] }, ;
            { _SET_DBFLOCKSCHEME, "DBFLockScheme", aSets[ _SET_DBFLOCKSCHEME ] }, ;
            { _SET_TRIMFILENAME, "TrimFileName", aSets[ _SET_TRIMFILENAME ] }, ;
            /*{ _SET_GTMODE, "GTMode", aSets[ _SET_GTMODE ] },*/ ;
            { _SET_HARDCOMMIT, "HardCommit", aSets[ _SET_HARDCOMMIT ] }, ;
            { _SET_FORCEOPT, "ForceOpt", aSets[ _SET_FORCEOPT ] }, ;
            { _SET_EOL, "EOL", aSets[ _SET_EOL ] } }
            
            // Phil: aSets returns an array of 111 elements (up to _SET_TRIMFILENAME) and these values
            // are beyond 200. Augusto
            //{ _SET_BACKGROUNDTASKS, "BackgroundTasks", aSets[ _SET_BACKGROUNDTASKS ] }, ;
            //{ _SET_BACKGROUNDTICK, "BackgroundTick", aSets[ _SET_BACKGROUNDTICK ] }, ;
            //{ _SET_ERRORLOG, "ErrorLog", aSets[ _SET_ERRORLOG ] },;
            //{ _SET_ERRORLOOP, "ErrorLoop", aSets[ _SET_ERRORLOOP ] }, ;
            //{ _SET_OUTPUTSAFETY, "OutputSafety", aSets[ _SET_OUTPUTSAFETY ] }, ;
            //{ _SET_PRINTERJOB, "PrinterJob", aSets[ _SET_PRINTERJOB ] }, ;
            //{ _SET_TRACE, "Trace", aSets[ _SET_TRACE ] }, ;
            //{ _SET_TRACEFILE, "TraceFile", aSets[ _SET_TRACEFILE ] }, ;
            //{ _SET_TRACESTACK, "TraceStack", aSets[ _SET_TRACESTACK ] }, ;
RETURN aOut


METHOD ReadValue() CLASS XHDebugger
  LOCAL nSize := Val( ::RecvUntil( ":" ) )
  LOCAL cString := Space( nSize )

  IF ::socket != NIL .AND. InetErrorCode( ::socket ) == 0
    InetRecvAll( ::socket, @cString, nSize )
    IF InetErrorCode( ::socket ) == 0
      ::RecvUntil( ">" )
      IF InetErrorCode( ::socket ) == 0
        RETURN cString
      ENDIF
    ENDIF
  ENDIF
  ::ConnectionError()
RETURN cString


METHOD ReadVars() CLASS XHDebugger
  LOCAL cString := ""
  LOCAL cMessage, n, n1, aVar

  IF ::socket == NIL
    RETURN Self
  ENDIF
  cString := ::RecvUntil( ">" )
  IF InetErrorCode( ::socket ) != 0
    ::ConnectionError()
    RETURN Self
  ENDIF
  IF Len( cString ) > 0
    cString += EOL
    DO WHILE ( n := At( EOL, cString ) ) > 0
      cMessage := Left( cString, n - 1 )
      cString := SubStr( cString, n + 1 )
      DO CASE
        CASE Left( cMessage, 7 ) == "global" + TAB
          n := At( TAB, cMessage, 8 )
          n1 := At( TAB, cMessage, n + 1 )
          aVar := { /* _SCOPE */ "Global", ;
                    /* _FRAME */ Val( SubStr( cMessage, 8 ) ), ;
                    /* _INDEX */ Val( SubStr( cMessage, n + 1 ) ), ;
                    /* _NAME */  SubStr( cMessage, n1 + 1 ) }
          AAdd( ::aVars, aVar )
        CASE Left( cMessage, 6 ) == "local" + TAB
          n := At( TAB, cMessage, 7 )
          n1 := At( TAB, cMessage, n + 1 )
          aVar := { /* _SCOPE */ "Local", ;
                    /* _FRAME */ Val( SubStr( cMessage, 7 ) ), ;
                    /* _INDEX */ Val( SubStr( cMessage, n + 1 ) ), ;
                    /* _NAME */  SubStr( cMessage, n1 + 1 ) }
          AAdd( ::aVars, aVar )
        CASE Left( cMessage, 7 ) == "static" + TAB
          n := At( TAB, cMessage, 8 )
          n1 := At( TAB, cMessage, n + 1 )
          aVar := { /* _SCOPE */ "Static", ;
                    /* _FRAME */ Val( SubStr( cMessage, 8 ) ), ;
                    /* _INDEX */ Val( SubStr( cMessage, n + 1 ) ), ;
                    /* _NAME */  SubStr( cMessage, n1 + 1 ) }
          AAdd( ::aVars, aVar )
        CASE Left( cMessage, 8 ) == "private" + TAB
          n := At( TAB, cMessage, 9 )
          aVar := { /* _SCOPE */ "Private", ;
                    /* _FRAME */ , ;
                    /* _INDEX */ Val( SubStr( cMessage, 9 ) ), ;
                    /* _NAME */  SubStr( cMessage, n + 1 ) }
          AAdd( ::aVars, aVar )
        CASE Left( cMessage, 7 ) == "public" + TAB
          n := At( TAB, cMessage, 8 )
          aVar := { /* _SCOPE */ "Public", ;
                    /* _FRAME */ , ;
                    /* _INDEX */ Val( SubStr( cMessage, 8 ) ), ;
                    /* _NAME */  SubStr( cMessage, n + 1 ) }
          AAdd( ::aVars, aVar )
      ENDCASE
    ENDDO
  ENDIF
RETURN Self


METHOD ReadVarValue( aVar ) CLASS XHDebugger
  LOCAL cCommand := "." + Lower( Left( aVar[ _SCOPE ], 2 ) ) + " "

  IF aVar[ _SCOPE ][ 1 ] $ "GLS"
    cCommand += LTrim( Str( aVar[ _FRAME ] ) ) + " "
  ENDIF
  ::Do( cCommand + LTrim( Str( aVar[ _INDEX ] ) ) )
RETURN ::ReadValue()


METHOD ReadWatches( lSingle ) CLASS XHDebugger
  LOCAL aWatches := {}
  LOCAL cString := ""
  LOCAL cMessage, n, n1, n2

  DEFAULT lSingle TO .F.

  IF ::socket == NIL
    RETURN {}
  ENDIF
  IF InetErrorCode( ::socket ) == 0
    cString := ::RecvUntil( EOL + ">" ) + If( lSingle, EOL, "" )
    IF InetErrorCode( ::socket ) == 0
      DO WHILE ( n := At( EOL, cString ) ) > 0
        cMessage := Left(cString, n - 1)
        cString := SubStr( cString, n + 1)
        n := At( TAB, cMessage )
        IF Left( cMessage, n - 1 ) == "watch"
          n1 := At( TAB, cMessage, n + 1 )
          n2 := At( TAB, cMessage, n1 + 1 )
          AAdd( aWatches, { /* _NUMBER */ Val( SubStr( cMessage, n + 1 ) ), ;
                            /* _EXPR */   SubStr( cMessage, n2 + 1 ), ;
                            /* _TRACE */  cMessage[ n1 + 1 ] == 't' } )
        ENDIF
      ENDDO
      RETURN aWatches
    ENDIF
  ENDIF
  ::ConnectionError()
RETURN {}


METHOD ReadWorkAreaInfo( n ) CLASS XHDebugger
  LOCAL hInfo

  ::Do( ".info " + LTrim( Str( n ) ) )
  TRY
    hInfo := &( SubStr( ::ReadValue(), 2 ) )
  CATCH
  END
RETURN hInfo


METHOD ReadWorkAreaRecord( n, aStruct ) CLASS XHDebugger
  LOCAL i, a := Array( Len( aStruct ) )

  FOR i := 1 TO Len( a )
    a[ i ] := SubStr( ::ReadExpressionValue( LTrim( Str( n ) ) + "->" + aStruct[ i ][ DBS_NAME ] ), 2 )
  NEXT
RETURN a


METHOD RecvUntil( cEndBlock ) CLASS XHDebugger
  LOCAL n, s := Space( 1 ), cString := ""

  DO WHILE ::socket != NIL .AND. InetErrorCode( ::socket ) == 0
    n := InetRecv( ::socket, @s )
    cString += Left( s, n )
    IF Right( cString, Len( cEndBlock ) ) == cEndBlock
      cString := Left( cString, Len( cString ) - Len( cEndBlock ) )
      EXIT
    ENDIF
  ENDDO
RETURN cString


METHOD Start( cAddress, nPort ) CLASS XHDebugger
  LOCAL nSeconds

  DEFAULT cAddress TO "127.0.0.1", nPort TO 10000

  IF ::socket != NIL
    ::Continue()
    RETURN Self
  ENDIF

  ::OnStart()

  ::oConsole:Out( "Connecting to the debugging server " + cAddress + ":" + LTrim( Str( nPort ) ) + "..." )
  nSeconds := Seconds()
  DO WHILE Seconds() < nSeconds + 15
    ::socket := InetConnect( cAddress, nPort )
    InetErrorCode( ::socket )
    IF InetErrorCode( ::socket ) == 0
      ::oConsole:Out( "Success!" )
      ::lStopped := .F.
      ::Listen( .T. )
      RETURN Self
    ENDIF
  ENDDO
  ::ConnectionError()
RETURN Self


METHOD Stop() CLASS XHDebugger
  ::Listen( .F. )
  ::Do( "quit" )
  ::lStopped := .T.
  ::oConsole:Out( "The program has been terminated." )
  IF ::socket != NIL
    InetClose( ::socket )
    ::socket := NIL
  ENDIF
RETURN Self


METHOD SyncCalls() CLASS XHDebugger
  ::aCalls := {}
  IF ::lShowCalls .AND. ::socket != NIL
    ::Do( ".callstack" )
    ::ReadCalls()
    ::oCallStack:SetCalls( ::aCalls )
  ENDIF
RETURN Self


METHOD SyncVars() CLASS XHDebugger
  IF ::socket != NIL
    ::aVars := {}

    IF ::lShowGlobals
      IF !::lShowAllGlobals
        ::Do( ".globals" )
      ELSE
        ::Do( ".allglobals" )
      ENDIF
      ::ReadVars()
    ENDIF
    IF ::lShowLocals
      ::Do( ".locals" )
      ::ReadVars()
    ENDIF
    IF ::lShowStatics
      ::Do( ".statics" )
      ::ReadVars()
    ENDIF
    IF ::lShowPrivates
      ::Do( ".privates" )
      ::ReadVars()
    ENDIF
    IF ::lShowPublics
      ::Do( ".publics" )
      ::ReadVars()
    ENDIF

    ::oMonitor:SetVars( ::aVars )
  ENDIF
RETURN Self


METHOD ToggleBreak( cFile, nLine ) CLASS XHDebugger
  LOCAL n
  LOCAL cBreak

  cBreak := cFile + ":" + LTrim( Str( nLine ) )

  IF ( n := AScan( ::aBreak, {|x| x[2] == cBreak } ) ) == 0
    ::Do( "break " + cBreak )
    ::ReadBreak()
    ::RecvUntil( ">" )
  ELSE
    ::Do( "del " + Str( ::aBreak[n][1] ) )
    ADel( ::aBreak, n, .T. )
    AEval( ::aBreak, {|x| If( x[1] > n, x[1]--, ) } )
    ::RecvUntil( ">" )
  ENDIF
RETURN Self


METHOD WorkAreaInfoText( nAreaNo, hInfo, aStruct, aRecord ) CLASS XHDebugger
  LOCAL i
  LOCAL ret := { ;
    "Area number: " + LTrim( Str( nAreaNo ) ), ;
    "Driver: " + hInfo[ "RddName" ], ;
    "Record size: " + LTrim( Str( hInfo[ "RecSize" ] ) ), ;
    "Header size: " + LTrim( Str( hInfo[ "Header" ] ) ), ;
    "Field count: " + LTrim( Str( hInfo[ "FCount" ] ) ), ;
    "Record count: " + LTrim( Str( hInfo[ "RecCount" ] ) ), ;
    "Last update: " + DToC( hInfo[ "LUpdate" ] ), ;
    "BOF: " + If( hInfo[ "BOF" ], "Yes", "No" ), ;
    "EOF: " + If( hInfo[ "EOF" ], "Yes", "No" ), ;
    "Deleted: " + If( hInfo[ "Deleted" ], "Yes", "No" ), ;
    "Found: " + If( hInfo[ "Found" ], "Yes", "No" ), ;
    "Filter: " + hInfo[ "DbFilter" ], ;
    "Index order: " + LTrim( Str( hInfo[ "IndexOrd" ] ) ), ;
    "Order key: " + hInfo[ "OrdKey" ], ;
    "Current record: " + LTrim( Str( hInfo[ "RecNo" ] ) ) ;
  }

  FOR i := 1 TO Len( aRecord )
    AAdd( ret, "  " + aStruct[ i ][ DBS_NAME ] + " = " + aRecord[ i ] )
  NEXT
RETURN ret


FUNCTION find_source( cName, aPath )

  LOCAL cDir

#ifdef __PLATFORM__Windows
  LOCAL cSource, cFile

  cFile := "\" + Lower( cName )

  FOR EACH cSource IN __GetApplication():aSources
     IF cFile IN Lower( cSource ) .AND. File( cSource )
        RETURN cSource
     ENDIF
  NEXT
#endif

  FOR EACH cDir IN aPath
    IF File( cDir + hb_OSPathSeparator() + cName )
      RETURN cDir + hb_OSPathSeparator() + cName
    ENDIF
  NEXT

RETURN cName


FUNCTION parse_cmd_line( aArgs )

  LOCAL cArg, cArgs := "", aSources := {}, aPath := {}
  LOCAL cHost, cPort, cStartPath, cXBP, oXBP

  DO WHILE Len( aArgs ) > 0 .AND. aArgs[ 1 ][ 1 ] == '-'
    //Alert( aArgs[ 1 ] )

    IF Lower( Left( aArgs[ 1 ], 6 ) ) == "-host:"
      cHost := SubStr( aArgs[ 1 ], 7 )
      IF ':' $ cHost
        cPort := SubStr( cHost, At( ':', cHost ) + 1 )
        cHost := Left( cHost, At( ':', cHost ) - 1 )
      ENDIF
    ELSEIF Lower( Left( aArgs[ 1 ], 6 ) ) == "-port:"
      cPort := SubStr( aArgs[ 1 ], 7 )
    ELSEIF Lower( Left( aArgs[ 1 ], 12 ) ) == "-sourcepath:"
      aPath := hb_ATokens( SubStr( aArgs[ 1 ], 13 ), ';' )
    ELSEIF Lower( Left( aArgs[ 1 ], 9 ) ) == "-sources:"
      aEval( hb_ATokens( SubStr( aArgs[ 1 ], 10 ), ';' ), {|cSource| aAdd( aSources, cSource ) } )
    ELSEIF Lower( Left( aArgs[ 1 ], 7 ) ) == "-start:"
      cStartPath := SubStr( aArgs[ 1 ], 8 )
    ELSEIF Lower( Left( aArgs[ 1 ], 5 ) ) == "-xbp:"
      cXBP := SubStr( aArgs[ 1 ], 6 )
      oXBP := LoadProject( cXBP )

      //Alert( cXBP )
      //Alert( oXBP:ClassName )

      IF IsObject( oXBP )
         //Alert( oXBP:PrgSources() )
         aEval( hb_ATokens( oXBP:PrgSources(), ';' ), {|cSource| aAdd( aSources, cSource ) } )

         IF Empty( cStartPath )
            cStartPath := oXBP:StartIn
         ENDIF
      ELSE
         Alert( "Sorry could not load XBP: '" + cXBP + "'" )
      ENDIF
    ELSE
      Alert( "Invalid argument: '" + aArgs[1] + "'" )
      //EXIT
    ENDIF

    ADel( aArgs, 1, .T. )
  ENDDO

  FOR EACH cArg IN aArgs
    cArgs += cArg + " "
  NEXT

RETURN { "host" => cHost, ;
         "port" => cPort, ;
         "sourcepath" => aPath, ;
         "sources" => aSources, ;
         "args" => cArgs, ;
         "startpath" => cStartPath }

FUNCTION xBuild_GUI_ONERROR()
RETURN .T.

FUNCTION xBuild_GUI_SETERROR()
RETURN .T.

FUNCTION xBuild_GUI_RESULTSWINDOW()
RETURN -1