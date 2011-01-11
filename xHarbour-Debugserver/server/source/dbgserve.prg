/*
 * $Id$
 */

#include "common.ch"
#include "hbdebug.ch"
#include "hbmemvar.ch"

#pragma BEGINDUMP
#include "hbapidbg.h"
#pragma ENDDUMP


#define DEBUG_PORT "10000"


#define TAB Chr( 9 )
#define EOL Chr( 10 )

/* Protocol definitions */
#define SEND_BREAKPOINT( n, v ) send( "break" + TAB + Str( n ) + TAB + If( !Empty( v[ _FUNCTION ] ), v[ _FUNCTION ], v[ _MODULE ] + TAB + Str( v[ _LINENO ] ) ) + EOL )
#define SEND_GLOBAL( v )      send( "global" + TAB + Str( v[ _FRAME ] ) + TAB + Str( v[ _INDEX ] ) + TAB + v[ _NAME ] + EOL )
#define SEND_LINENO( n )        send( "line" + TAB + Str( n ) + EOL )
#define SEND_LOCAL( v )      send( "local" + TAB + Str( v[ _FRAME ] ) + TAB + Str( v[ _INDEX ] ) + TAB + v[ _NAME ] + EOL )
#define SEND_MODULE( s )        send( "module" + TAB + s + EOL )
#define SEND_PRIVATE( k, v )    send( "private" + TAB + Str( v ) + TAB + k + EOL )
#define SEND_PUBLIC( k, v )     send( "public" + TAB + Str( v ) + TAB + k + EOL )
#define SEND_STATIC( v )     send( "static" + TAB + Str( v[ _FRAME ] ) + TAB + Str( v[ _INDEX ] ) + TAB + v[ _NAME ] + EOL )
#define SEND_WATCHPOINT( n, v ) send( "watch" + TAB + Str( n ) + TAB + If( v[ _TRACE ], "t", "w") + TAB + v[ _EXPR ] + EOL )
#define SEND_WORKAREA( n )      send( "workarea" + TAB + Str( n ) + TAB + ( n )->( Alias() ) + EOL )

#command ? <xx,...> => send( <xx> + EOL )


/* Array indices for s_aBreakPoints, s_aCallStack entries */
#define _MODULE   1
#define _FUNCTION 2
#define _LINENO   3
/* Array indices for s_aCallStack entries */
#define _LEVEL    4
#define _LOCALS   5
#define _STATICS  6

/* Array indices for s_aCallStack[][ _LOCALS | _STATICS ], s_aStaticModules[] values */
#define _NAME  1
#define _INDEX 2
#define _TYPE  3
#define _FRAME 4

/* Array indices for s_aWatchPoints entries */
#define _EXPR  1
#define _TRACE 2


REQUEST HGetKeys


/* The following variables are needed only statically but some of them are
 * used before the STATIC variable support is initialized, so they're GLOBAL */
GLOBAL s_noDebug // .T. if the debugger is disabled

GLOBAL s_Socket // Server socket
GLOBAL s_Accept // Connection socket

/* The operating parameters of the debugger */
GLOBAL s_handle         // Debugger info handle
GLOBAL s_aBreakPoints   // Breakpoints array
GLOBAL s_nUntil         // Index into breakpoints array of a temporary breakpoint
GLOBAL s_aWatchPoints   // Watchpoints array
GLOBAL s_aCallStack     // Not actual callstack but debuginfo one
GLOBAL s_nFrame         // Callstack frame selected for examination
GLOBAL s_aModules       // Modules with static and global variables


STATIC PROCEDURE init_globals()
  s_aBreakPoints := {}
  s_nUntil := 0
  s_aWatchPoints := {}
  s_aCallStack := {}
  s_nFrame := 0
  s_aModules := {}
RETURN


STATIC FUNCTION recv()
  LOCAL s := "", nResponse := 0

  IF s_Accept != NIL
    DO WHILE InetDataReady( s_Accept, 100) == 0
      IF InetErrorCode( s_Accept ) != 0
        QUIT
      ENDIF
    ENDDO
    s := InetRecvLine( s_Accept, @nResponse )
    IF InetErrorCode( s_Accept ) != 0
      QUIT
    ENDIF
  /*ELSE
    ACCEPT TO s*/
  ENDIF
RETURN s


STATIC PROCEDURE send( cString )
  IF s_Accept != NIL
    InetSend( s_Accept, cString )
  /*ELSE
    OutStd( cString )*/
  ENDIF
RETURN


STATIC PROCEDURE open( nPort )
  LOCAL nTime := 0

  OutStd( "Acting as a debugging server on port " + LTrim( Str( nPort ) ) + "... " )
  InetInit()
  s_Socket := InetServer( nPort )
  InetSetTimeOut( s_Socket, 250 )
  DO WHILE nTime < 60
    s_Accept := InetAccept( s_Socket )
    IF InetErrorCode( s_Socket ) == 0
      OutStd( "A connection is ready!" )
      ? "You have connected to the xHarbour debugging server."
      RETURN
    ENDIF
    nTime++
  ENDDO
  OutStd( "No connection in 15 seconds, exiting..." )
  QUIT
RETURN


PROCEDURE __dbgAltDEntry()
  hb_dbg_invokedebug( Set( _SET_DEBUG ) )
RETURN


#pragma BEGINDUMP

static BOOL
__dbgInvoke( void )
{
  static PHB_DYNS pDynSym = NULL;

  if ( !pDynSym )
  {
    pDynSym = hb_dynsymFind( "__DBGINVOKE" );
  }
  hb_vmPushSymbol( pDynSym->pSymbol );
  hb_vmPushNil();
  hb_vmDo( 0 );
  return hb_itemGetL( hb_stackReturnItem() );
}

#pragma ENDDUMP


FUNCTION __dbgInvoke()
RETURN ( InetDataReady( s_Accept ) > 0 .AND. recv() == "invoke" )


PROCEDURE __dbgEntry( nMode, xParam1, xParam2, xParam3, xParam4, xParam5 )
  LOCAL n, aTop, bClassScope

  (xParam2)

  /* Do something only if //DEBUG command line argument is given */
  IF s_noDebug == NIL
    s_noDebug := !HB_ArgCheck( "DEBUG" )
  ENDIF
  IF s_noDebug
    RETURN
  ENDIF

  IF s_Socket == NIL
    IF Empty( n := HB_ArgString( "DEBUGPORT" ) )
      n := DEBUG_PORT
    ENDIF
    open( Val( n ) )
    init_globals()
  ENDIF

  DO CASE
    CASE nMode == HB_DBG_GETENTRY
      hb_dbg_SetEntry()
    CASE nMode == HB_DBG_ACTIVATE
      s_handle := xParam1
      s_aCallStack := xParam3
      s_aModules := xParam4
      s_aBreakPoints := xParam5
      IF s_nUntil > 0
        delete_breakpoint( "0" )
        s_nUntil := 0
      ENDIF
      HB_INLINE( s_handle ) {
        hb_dbgSetInvoke( hb_parptr( 1 ), __dbgInvoke );
      }
      bClassScope := __SetClassScope( .F. )
      aTop := s_aCallStack[ 1 ]
      SEND_MODULE( aTop[ _MODULE ] + ":" + aTop[ _FUNCTION ] )
      SEND_LINENO( aTop[ _LINENO ] )
      get_command()
      __SetClassScope( bClassScope )

  ENDCASE

RETURN


EXIT PROCEDURE __dbgExit()
  IF s_Accept != NIL
    InetClose( s_Accept )
  ENDIF
  IF s_Socket != NIL
    InetClose( s_Socket )
  ENDIF
  InetCleanup()
RETURN


STATIC PROCEDURE get_command()
  LOCAL lExit := .F., cCommand, nResponse := 0

  s_nFrame := 0

  DO WHILE !lExit
    send( ">" )
    IF !Empty( cCommand := recv() )
      do_command( cCommand, @lExit )
    ENDIF
  ENDDO
RETURN


#define STARTS( what, with ) ( Left( what, Len( with ) ) == with )

STATIC PROCEDURE do_command( cCommand, lExit )
  LOCAL n, cArgs := ""

  n := At( ' ', cCommand )
  IF n > 0
    cArgs := SubStr( cCommand, n + 1 )
    cCommand := Left( cCommand, n - 1 )
  ENDIF

  DO CASE
    CASE STARTS( ".allglobals", cCommand )
      show_globals( .T. )
    CASE STARTS( ".areas", cCommand )
      show_workareas()
    CASE STARTS( ".breakpoints", cCommand )
      show_breakpoints()
    CASE STARTS( ".callstack", cCommand )
      show_callstack()
    CASE STARTS( ".global", cCommand )
      show_global( Val( cArgs ), Val( SubStr( cArgs, At( ' ', cArgs ) + 1 ) ) )
    CASE STARTS( ".globals", cCommand )
      show_globals( .F. )
    CASE STARTS( ".info", cCommand )
      show_workarea_info( Val( cArgs ) )
    CASE STARTS( ".local", cCommand )
      show_local( Val( cArgs ), Val( SubStr( cArgs, At( ' ', cArgs ) + 1 ) ) )
    CASE STARTS( ".locals", cCommand )
      show_locals()
    CASE STARTS( ".private", cCommand )
      show_private( Val( cArgs ) )
    CASE STARTS( ".privates", cCommand )
      show_privates()
    CASE STARTS( ".public", cCommand )
      show_public( Val( cArgs ) )
    CASE STARTS( ".publics", cCommand )
      show_publics()
    CASE STARTS( ".sources", cCommand )
      show_sources()
    CASE STARTS( ".static", cCommand )
      show_static( Val( cArgs ), Val( SubStr( cArgs, At( ' ', cArgs ) + 1 ) ) )
    CASE STARTS( ".statics", cCommand )
      show_statics()
    CASE STARTS( ".valid", cCommand )
      show_is_valid_stop_line( cArgs )
    CASE STARTS( ".wp", cCommand )
      IF Val( cArgs ) > 0
        show_watchpoint( Val( cArgs ) )
      ELSE
        show_watchpoints()
      ENDIF
    CASE STARTS( "+cbtrace", cCommand )
      set_cbtrace( .T. )
    CASE STARTS( "-cbtrace", cCommand )
      set_cbtrace( .F. )
    CASE Left( cCommand, 2 ) == "??"
      inspect( evaluate( SubStr( cCommand, 3 ) + " " + cArgs ) )
    CASE cCommand[ 1 ] == '?'
      ? evaluate( SubStr( cCommand, 2 ) + " " + cArgs )
    CASE STARTS( "break", cCommand )
      add_breakpoint( cArgs )
    CASE STARTS( "continue", cCommand )
      hb_dbg_SetGo( s_handle )
      lExit := .T.
    CASE STARTS( "del", cCommand )
      delete_breakpoint( cArgs )
    CASE STARTS( "frame", cCommand )
      s_nFrame := Val( cArgs )
    CASE STARTS( "help", cCommand )
      show_help()
    CASE STARTS( "invoke", cCommand )
      ? "The 'invoke' command is only available when the program is running."
    CASE STARTS( "next", cCommand )
      hb_dbg_SetTrace( s_handle )
      lExit := .T.
    CASE STARTS( "quit", cCommand )
      QUIT
    CASE STARTS( "step", cCommand )
      lExit := .T.
    CASE STARTS( "stepout", cCommand )
      hb_dbg_SetNextRoutine( s_handle )
      lExit := .T.
    CASE STARTS( "tp", cCommand )
      add_tracepoint( Val( cArgs ) )
    CASE STARTS( "unwatch", cCommand )
      delete_watchpoint( cArgs )
    CASE STARTS( "until", cCommand )
      IF Len( AllTrim( cArgs ) ) > 0
        add_breakpoint( cArgs )
        s_nUntil := Len( s_aBreakPoints )
        hb_dbg_SetGo( s_handle )
        lExit := .T.
      ELSE
        ? "'until' what? Try 'help'."
      ENDIF
    CASE STARTS( "untrace", cCommand )
      delete_tracepoint( Val( cArgs ) )
    CASE STARTS( "wp", cCommand )
      add_watchpoint( cArgs )
    OTHERWISE
      ? "Unknown command '" + cCommand + "'. Try 'help'."
  ENDCASE
RETURN


STATIC PROCEDURE add_breakpoint( cArgs )
  LOCAL aBreak := parse_breakpoint( cArgs )

  AAdd( s_aBreakPoints, aBreak )
  HB_INLINE( s_handle, aBreak[ _MODULE ], aBreak[ _LINENO ], aBreak[ _FUNCTION ] ) {
    hb_dbgAddBreak( hb_parptr( 1 ), hb_parc( 2 ), hb_parni( 3 ), hb_parc( 4 ) );
  }
  SEND_BREAKPOINT( Len( s_aBreakPoints ), aBreak )
RETURN


STATIC PROCEDURE add_tracepoint( n )
  LOCAL aWatch

  aWatch := s_aWatchPoints[ n ]
  aWatch[ _TRACE ] := .T.

  hb_dbg_SetWatch( s_handle, n - 1, aWatch[ _EXPR ], .T. )
RETURN


STATIC PROCEDURE add_watchpoint( cArgs )
  LOCAL aWatch, aNames := {}, aVars := {}

  cArgs := AllTrim( cArgs )
  IF !Empty( cArgs )
    aWatch := { cArgs, .F. }
    AAdd( s_aWatchPoints, aWatch )

    hb_dbg_AddWatch( s_handle, cArgs, .F. )

    SEND_WATCHPOINT( Len( s_aWatchPoints ), aWatch )
  ENDIF
RETURN


STATIC PROCEDURE delete_breakpoint( cArgs )
  LOCAL nBreak := Val( cArgs )

  IF Len( s_aBreakPoints ) > 0
    IF nBreak == 0
      nBreak := Len( s_aBreakPoints )
    ENDIF
    IF nBreak >= 1 .AND. nBreak <= Len( s_aBreakPoints )
      ADel( s_aBreakPoints, nBreak, .T. )
      hb_dbg_DelBreak( s_handle, nBreak - 1 )
    ENDIF
  ENDIF
RETURN


STATIC PROCEDURE delete_tracepoint( n )
  LOCAL aWatch := s_aWatchPoints[ n ]

  aWatch[ n ][ _TRACE ] := .F.
  hb_dbg_SetWatch( s_handle, n - 1, aWatch[ n ][ _EXPR ], .F. )
RETURN


STATIC PROCEDURE delete_watchpoint( cArgs )
  LOCAL nWatch := Val( cArgs )

  IF Len( s_aWatchPoints ) > 0
    IF nWatch == 0
      nWatch := Len( s_aWatchPoints )
    ENDIF
    IF nWatch >= 1 .AND. nWatch <= Len( s_aWatchPoints )
      ADel( s_aWatchPoints, nWatch, .T. )
      hb_dbg_DelWatch( s_handle, nWatch - 1 )
    ENDIF
  ENDIF
RETURN


STATIC FUNCTION evaluate( xExpr )
  LOCAL bError := ErrorBlock( {|e| Break(e) } )
  LOCAL lValid := .F.
  LOCAL xResult,cResult,oerror

  BEGIN SEQUENCE
    xResult := hb_dbg_GetExprValue( s_handle, xExpr, @lValid )
    IF !lValid
      cResult := "E" + "Syntax error"
    ELSE
      cResult := serialize( xResult )
    ENDIF
  RECOVER USING oError
    cResult := "E" + oError:operation + ": " + oError:description
  END SEQUENCE

  ErrorBlock( bError )
RETURN cResult


STATIC PROCEDURE inspect( cValue )
  ? LTrim( Str( Len( cValue ) ) ) + ":" + cValue
RETURN


STATIC FUNCTION parse_breakpoint( cArgs )
  LOCAL aTail := s_aCallStack[ 1 ], n, aBreak

  cArgs := AllTrim( cArgs )
  IF Empty( cArgs )
    aBreak := { aTail[ _MODULE ], , aTail[ _LINENO ] }
  ELSEIF ( n := At( ':', cArgs ) ) > 0
    aBreak := { Left( cArgs, n - 1 ), , Val( SubStr( cArgs, n + 1 ) ) }
  ELSEIF IsDigit( cArgs )
    aBreak := { aTail[ _MODULE ], , Val( SubStr( cArgs, n + 1 ) ) }
  ELSE
    aBreak := { , Upper( cArgs ), }
  ENDIF
RETURN aBreak


STATIC FUNCTION serialize( xValue )
RETURN ValType( xValue ) + CStr( xValue )


STATIC PROCEDURE set_cbtrace( lValue )
  hb_dbg_SetCBTrace( s_handle, lValue )
RETURN


STATIC PROCEDURE show_breakpoints()
  AEval( s_aBreakPoints, {|x, n| If( x != NIL, SEND_BREAKPOINT( n, x ), ) } )
RETURN


STATIC PROCEDURE show_callstack()
  LOCAL i, x

  FOR i := Len( s_aCallStack ) TO 1 STEP -1
    x := s_aCallStack[ i ]
    ? x[ _MODULE ] + TAB + Str( x[ _LINENO ] ) + TAB + x[ _FUNCTION ]
  NEXT
RETURN


STATIC PROCEDURE show_global( frame, index )
  LOCAL value := serialize( hb_dbg_vmVarGGet( frame, index ) )

  ? LTrim( Str( Len( value ) ) ) + ":" + value
RETURN


STATIC PROCEDURE show_globals( lAll )
  LOCAL cModule := s_aCallStack[ s_nFrame + 1 ][ _MODULE ]
  LOCAL aModule

  FOR EACH aModule IN s_aModules
    IF !lAll .AND. aModule[ 1 ] != cModule
      LOOP
    ENDIF
    AEval( aModule[ 3 ], {|v| SEND_GLOBAL( v ) } )
    IF !lAll
      AEval( aModule[ 4 ], {|v| SEND_GLOBAL( v ) } )
    ENDIF
  NEXT
RETURN


STATIC PROCEDURE show_help()
  ? ".allglobals         Show all GLOBAL variables including unseen in this module"
  ? ".areas              Show workareas"
  ? ".breakpoints        Show breakpoints"
  ? ".callstack          Show call stack"
  ? ".global <frame> <n> Show a GLOBAL variable value"
  ? ".globals            Show GLOBAL variables"
  ? ".info <n>           Show info for workarea <n>"
  ? ".local <frame> <n>  Show a LOCAL variable value"
  ? ".locals             Show LOCAL variables"
  ? ".private <n>        Show a PRIVATE variable value"
  ? ".privates           Show PRIVATE variables"
  ? ".public <n>         Show a PUBLIC variable value"
  ? ".publics            Show PUBLIC variables"
  ? ".sources            Show list of source files"
  ? ".static <frame> <n> Show a STATIC variable value"
  ? ".statics            Show STATIC variables"
  ? ".valid [file:]<n>   Show if line <n> in <file> is a valid stop line"
  ? ".wp                 Show watchpoint expressions"
  ? ".wp <n>             Show the value of the watchpoint expression [n]"
  ? "+cbtrace            Switch codeblock tracing on"
  ? "-cbtrace            Switch codeblock tracing off"
  ? "? <expression>      Evaluate an expression"
  ? "?? <expression>     Evaluate an expression and inspect its value"
  ? "break               Set a breakpoint on current line"
  ? "break <function>    Set a breakpoint on function"
  ? "break [file:]<line> Set a breakpoint on line"
  ? "continue            Continue program execution"
  ? "delete [n]          Delete breakpoint number [n] or the last one"
  ? "frame <n>           Select stack frame <n> (0 is top)"
  ? "help                Show this information"
  ? "invoke              Stop the program execution and accept commands"
  ? "next                Continue execution until next program line"
  ? "quit                Quit the program"
  ? "step                Continue execution for a single step"
  ? "stepout             Continue execution until exit from current function"
  ? "tp <n>              Change a watchpoint to a tracepoint"
  ? "until [file:]<line> Continue execution until [file:]<line> is reached"
  ? "untrace <n>         Change a tracepoint to a watchpoint"
  ? "unwatch [n]         Delete watchpoint/tracepoint number [n] or the last one"
  ? "wp <expression>     Add a watchpoint expression"
RETURN


STATIC PROCEDURE show_is_valid_stop_line( cArgs )
  LOCAL aParsed := parse_breakpoint( cArgs )
  LOCAL lRet := .F.

  IF !Empty( aParsed[ _MODULE ] ) .AND. !Empty( aParsed[ _LINENO ] )
    lRet := hb_dbg_IsValidStopLine( s_handle, aParsed[ _MODULE ], aParsed[ _LINENO ] )
  ENDIF
  ? If( lRet, ".T.", ".F." )
RETURN


STATIC PROCEDURE show_local( frame, index )
  LOCAL value := serialize( hb_dbg_vmVarLGet( hb_dbg_procLevel() - frame, index ) )

  ? LTrim( Str( Len( value ) ) ) + ":" + value
RETURN


STATIC PROCEDURE show_locals()
  LOCAL frame := s_aCallStack[ s_nFrame + 1 ]
  Local i

  FOR i := 1 TO Len( frame[ _LOCALS ] )
    SEND_LOCAL( frame[ _LOCALS ][ i ] )
  NEXT
RETURN


STATIC PROCEDURE show_private( index )
  LOCAL value := serialize( __MVDbgInfo( HB_MV_PRIVATE, index ) )

  ? LTrim( Str( Len( value ) ) ) + ":" + value
RETURN


STATIC PROCEDURE show_privates()
  LOCAL i, n, name

  n := __MVDbgInfo( HB_MV_PRIVATE )
  FOR i := n TO 1 STEP -1
    __MVDbgInfo( HB_MV_PRIVATE, i, @name )
    SEND_PRIVATE( name, i )
  NEXT
RETURN


STATIC PROCEDURE show_public( index )
  LOCAL value := serialize( __MVDbgInfo( HB_MV_PUBLIC, index ) )

  ? LTrim( Str( Len( value ) ) ) + ":" + value
RETURN


STATIC PROCEDURE show_publics()
  LOCAL i, n, name

  n := __MVDbgInfo( HB_MV_PUBLIC )
  FOR i := n TO 1 STEP -1
    __MVDbgInfo( HB_MV_PUBLIC, i, @name )
    SEND_PUBLIC( name, i )
  NEXT
RETURN


STATIC PROCEDURE show_sources()
  LOCAL aSources

  aSources := hb_dbg_GetSourceFiles( s_handle )
  ? xhd_val_to_str( aSources )
RETURN


FUNCTION xhd_val_to_str(xValue)
  local out:=""
  local xMember
  if valtype(xValue)='A'
    out:="{"
    for each xMember in xValue
      out += xhd_val_to_str(xMember) + ","
    next
    if len(out) > 1
      out[-1] := "}"
    else
      out += "}"
    endif
    return out
  elseif valtype(xValue)='H'
    out := "{" 
    for each xMember in xValue:keys
      out += ValToPrgExp(xMember) + "=>" + xhd_val_to_str(xValue:values[hb_enumindex()]) + ","
    next
    if len(out) > 1
      out[-1] := "}"
    else
      out = "{ => }"
    endif
    return out
  endif
return valtoprgexp(xValue)
    


STATIC PROCEDURE show_static( frame, index )
  LOCAL value := serialize( hb_dbg_vmVarSGet( frame, index ) )

  ? LTrim( Str( Len( value ) ) ) + ":" + value
RETURN


STATIC PROCEDURE show_statics()
  LOCAL aFrame := s_aCallStack[ s_nFrame + 1 ]
  LOCAL cModule := aFrame[ _MODULE ]
  LOCAL nModule

  IF ( nModule := AScan( s_aModules, {|x| x[ 1 ] == cModule } ) ) > 0
    AEval( s_aModules[ nModule ][ 2 ], {|v| SEND_STATIC( v ) } )
  ENDIF
  AEval( aFrame[ _STATICS ], {|v| SEND_STATIC( v ) } )
RETURN


STATIC PROCEDURE show_watchpoint( n )
  LOCAL cValue

  IF n > 0 .AND. n <= Len( s_aWatchPoints ) .AND. s_aWatchPoints[ n ] != NIL
    cValue := evaluate( n )
    ? LTrim( Str( Len( cValue ) ) ) + ":" + cValue
  ENDIF
RETURN


STATIC PROCEDURE show_watchpoints()
  LOCAL i

  FOR i := 1 TO Len( s_aWatchPoints )
    IF s_aWatchPoints[ i ] != NIL
      SEND_WATCHPOINT( i, s_aWatchPoints[ i ] )
    ENDIF
  NEXT
  ? ""
RETURN


STATIC PROCEDURE show_workarea_info( n )
  LOCAL hInfo

  IF n <= 0
    n := Select()
  ENDIF

  IF (n)->( !Used() )
    inspect( "L" + xhd_val_to_str( .F. ) )
    RETURN
  ENDIF
  hInfo := (n)->( { 'RddName' => RddName(), 'RecSize' => RecSize(), 'Header' => Header(), ;
                    'FCount' => FCount(), 'RecCount' => RecCount(), 'LUpdate'=> LUpdate(), ;
                    'BOF' => BOF(), 'EOF' => EOF(), 'Deleted' => Deleted(), 'Found' => Found(), ;
                    'DbFilter' => DbFilter(), 'IndexOrd' => IndexOrd(), 'OrdKey' => OrdKey(), ;
                    'RecNo' => RecNo(), 'DbStruct' => DbStruct() } )
  inspect( "H" + xhd_val_to_str( hInfo ) )
RETURN


#pragma BEGINDUMP

#include <hbapi.h>
#include <hbapirdd.h>

static HB_ERRCODE add_area( AREAP area, void *data )
{
  PHB_ITEM arr = (PHB_ITEM)data;
  PHB_ITEM number = hb_itemPutNL( NULL, area->uiArea );

  hb_arrayAddForward( arr, number );
  return HB_SUCCESS;
}

#pragma ENDDUMP


STATIC PROCEDURE show_workareas()
  LOCAL a := {}

  HB_INLINE( a ) {
    PHB_ITEM a = hb_itemParam( 1 );

    hb_rddIterateWorkAreas( add_area, (void *) a );
  }

  AEval( a, {|n| SEND_WORKAREA( n ) } )
RETURN

