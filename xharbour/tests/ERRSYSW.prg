// Error handler system adapted to FiveWin
// ErrSysW.prg

#include "error.ch"
#include "FiveWin.ch"

external _fwGenError   // Link FiveWin generic Error Objects Generator

#define NTRIM(n)    ( LTrim( Str( n ) ) )

#ifdef __CLIPPER__
   #define DLG_TITLE "FiveWin: The CA-Clipper for Windows Library"
#else
   #ifdef __HARBOUR__
      #define DLG_TITLE "FiveWin for Harbour"
   #else
      #define DLG_TITLE "FiveWin for Xbase++"
   #endif
#endif

/*************
*	ErrorSys()
*
*	Note:  automatically executes at startup
*/
proc ErrorSys()
    ErrorBlock( { | e | ErrorDialog( e ) } )
return

proc ErrorLink()
return


/*************
*   ErrorDialog()
*/
static function ErrorDialog( e ) // -> logical or quits App.

   local oDlg, oLbx, oFont
   local lRet    // if lRet == nil -> default action: QUIT
   local n, j, cMessage, aStack := {}
   local oSay, hLogo
   local nButtons  := 1
   local cErrorLog := ""
   local aVersions := GetVersion()
   local aTasks    := GetTasks()
   local aRDDs, nTarget, uValue
   local oOldError
   local cRelation
   local lIsWinNT := IsWinNT()

   cErrorLog += "Application" + CRLF
   cErrorLog += "===========" + CRLF
   cErrorLog += "   Path and name: " + GetModuleFileName( GetInstance() )

   #ifdef __CLIPPER__
      cErrorLog += " (16 bits)" + CRLF
   #else
      cErrorLog += " (32 bits)" + CRLF
   #endif

   cErrorLog += "   Size: " + Transform( FSize( GetModuleFileName( ;
                GetInstance() ) ), "9,999,999 bytes" ) + CRLF
   #ifdef __CLIPPER__
      cErrorLog += "   Max files handles permited: ( SetHandleCount() ) " + ;
                   Str( SetHandleCount(), 3 ) + CRLF
   #endif

   cErrorLog += "   Error ocurred at: " + ;
                DToC( Date() ) + ", " + Time() + CRLF

   // Error object analysis
   cMessage   = "   Error description: " + ErrorMessage( e ) + CRLF
   cErrorLog += cMessage

   if ValType( e:Args ) == "A"
      cErrorLog += "   Args:" + CRLF
      for n = 1 to Len( e:Args )
         cErrorLog += "     [" + Str( n, 4 ) + "] = " + ValType( e:Args[ n ] ) + ;
                      "   " + cValToChar( e:Args[ 1 ] ) + CRLF
      next
   endif

   cErrorLog += CRLF + "Stack Calls" + CRLF
   cErrorLog += "===========" + CRLF
      n := 2    // we don't disscard any info again !
      while ( n < 74 )
          if ! Empty(ProcName( n ) )
             AAdd( aStack, "   Called from " + Trim( ProcName( n ) ) + ;
                           "(" + NTRIM( ProcLine( n ) ) + ")" )
             cErrorLog += ATail( aStack ) + CRLF
          endif
          n++
    end

   cErrorLog += CRLF + "System" + CRLF
   cErrorLog += "======" + CRLF

   #ifdef __CLIPPER__
      cErrorLog += "   CPU type: " + GetCPU() + CRLF
   #else
      cErrorLog += "   CPU type: " + GetCPU() + " " + ;
                   AllTrim( Str( GetCPUSpeed() ) ) + " Mhz" + CRLF
   #endif

   cErrorLog += "   Hardware memory: " + ;
                cValToChar( Int( nExtMem() / ( 1024 * 1024 ) ) + 1 ) + ;
                " megs" + CRLF + CRLF

   cErrorLog += "   Free System resources: " + AllTrim( Str( GetFreeSystemResources( 0 ) ) ) + " %" + CRLF + ;
                "        GDI    resources: " + AllTrim( Str( GetFreeSystemResources( 1 ) ) ) + " %" + CRLF + ;
                "        User   resources: " + AllTrim( Str( GetFreeSystemResources( 2 ) ) ) + " %" + CRLF + CRLF

   cErrorLog += "   Compiler version: " + Version() + CRLF

   #ifdef __CLIPPER__
      cErrorLog += "   Windows and MsDos versions: " + ;
                   AllTrim( Str( aVersions[ 1 ] ) ) + "." + ;
                   AllTrim( Str( aVersions[ 2 ] ) ) + ", " + ;
                   AllTrim( Str( aVersions[ 3 ] ) ) + "." + ;
                   AllTrim( Str( aVersions[ 4 ] ) ) + CRLF + CRLF
   #else
      cErrorLog += "   Windows version: " + ;
                   AllTrim( Str( aVersions[ 1 ] ) ) + "." + ;
                   AllTrim( Str( aVersions[ 2 ] ) ) + ", Build " + ;
                   AllTrim( Str( aVersions[ 3 ] ) ) + ;
                   " " + aVersions[ 5 ] + CRLF + CRLF
   #endif

   cErrorLog += "   Windows total applications running: " + ;
                AllTrim( Str( Len( aTasks ) ) ) + CRLF
   for n = 1 to Len( aTasks )
      cErrorLog += "    " + Str( n, 3 ) + " " + aTasks[ n ] + CRLF
   next

   // by default, division by zero yields zero
   if ( e:genCode == EG_ZERODIV )
      return 0
   end

   // for network open error, set NETERR() and subsystem default
   if ( e:genCode == EG_OPEN .and. ;
      ( e:osCode == 32 .or. e:osCode == 5 ) .and. ;
        e:canDefault )
      NetErr( .t. )
      return .f.       // Warning: Exiting!
   end

   // for lock error during APPEND BLANK, set NETERR() and subsystem default
   if ( e:genCode == EG_APPENDLOCK .and. e:canDefault )
      NetErr( .t. )
      return .f.       // OJO SALIDA
   endif

   if ProcName( 7 ) == "ERRORDIALO"   // recursive error !!!
      SET RESOURCES TO
      ErrorLevel( 1 )
      QUIT
   endif

   // Warning!!! Keep here this code !!! Or we will be consuming GDI as
   // we don't generate the error but we were generating the bitmap

   hLogo = FWBitMap()

   if e:canRetry
      nButtons++
   endif

   if e:canDefault
      nButtons++
   endif

   cErrorLog += CRLF + "Variables in use" + CRLF + "================" + CRLF
   cErrorLog += "   Procedure     Type   Value" + CRLF
   cErrorLog += "   ==========================" + CRLF

   n := 2    // we don't disscard any info again !
   while ( n < 74 )

       if ! Empty( ProcName( n ) )
          cErrorLog += "   " + Trim( ProcName( n ) ) + CRLF
          for j = 1 to ParamCount( n )
             cErrorLog += "     Param " + Str( j, 3 ) + ":    " + ;
                          ValType( GetParam( n, j ) ) + ;
                          "    " + cGetInfo( GetParam( n, j ) ) + CRLF
          next
          for j = 1 to LocalCount( n )
             cErrorLog += "     Local " + Str( j, 3 ) + ":    " + ;
                          ValType( GetLocal( n, j ) ) + ;
                          "    " + cGetInfo( GetLocal( n, j ) ) + CRLF
          next
       endif

       n++
   end

   cErrorLog += CRLF + "Linked RDDs" + CRLF + "===========" + CRLF
   aRDDs = RddList( 1 )
   for n = 1 to Len( aRDDs )
      cErrorLog += "   " + aRDDs[ n ] + CRLF
   next

   cErrorLog += CRLF + "DataBases in use" + CRLF + "================" + CRLF
   for n = 1 to 255
      if ! Empty( Alias( n ) )
         cErrorLog += CRLF + Str( n, 3 ) + ": " + If( Select() == n,"=> ", "   " ) + ;
                      PadR( Alias( n ), 15 ) + Space( 20 ) + "RddName: " + ;
                      ( Alias( n ) )->( RddName() ) + CRLF
         cErrorLog += "     ==============================" + CRLF
         cErrorLog += "     RecNo    RecCount    BOF   EOF" + CRLF
         cErrorLog += "    " + Transform( ( Alias( n ) )->( RecNo() ), "99999" ) + ;
                      "      " + Transform( ( Alias( n ) )->( RecCount() ), "99999" ) + ;
                      "      " + cValToChar( ( Alias( n ) )->( BoF() ) ) + ;
                      "   " + cValToChar( ( Alias( n ) )->( EoF() ) ) + CRLF + CRLF
         cErrorLog += "     Indexes in use " + Space( 23 ) + "TagName" + CRLF
         for j = 1 to 15
            if ! Empty( ( Alias( n ) )->( IndexKey( j ) ) )
               cErrorLog += Space( 8 ) + ;
                            If( ( Alias( n ) )->( IndexOrd() ) == j, "=> ", "   " ) + ;
                            PadR( ( Alias( n ) )->( IndexKey( j ) ), 35 ) + ;
                            ( Alias( n ) )->( OrdName( j ) ) + ;
                            CRLF
            endif
         next
         cErrorLog += CRLF + "     Relations in use" + CRLF
         for j = 1 to 8
            if ! Empty( ( nTarget := ( Alias( n ) )->( DbRSelect( j ) ) ) )
               cErrorLog += Space( 8 ) + Str( j ) + ": " + ;
                            "TO " + ( Alias( n ) )->( DbRelation( j ) ) + ;
                            " INTO " + Alias( nTarget ) + CRLF
               // uValue = ( Alias( n ) )->( DbRelation( j ) )
               // cErrorLog += cValToChar( &( uValue ) ) + CRLF
            endif
         next
      endif
   next

   n = 1
   cErrorLog += CRLF + "Classes in use:" + CRLF
   cErrorLog += "===============" + CRLF
   while ! Empty( __ClassName( n ) )
      cErrorLog += "   " + Str( n, 3 ) + " " + __ClassName( n++ ) + CRLF
   end

   cErrorLog += CRLF + "Memory Analysis" + CRLF
   cErrorLog +=        "===============" + CRLF

   #ifdef __CLIPPER__
      cErrorLog += "   Static memory:" + CRLF
      cErrorLog += "      data segment: 64k" + CRLF
   #endif

   #ifdef __CLIPPER__
   cErrorLog += "      Initial size:       " + ;
                LTrim( Str( nInitDSSize() ) ) + ;
                " bytes  (SYMP=" + LTrim( Str( nSymPSize() ) ) + ;
                ", Stack=" + LTrim( Str( nStackSize() ) ) + ;
                ", Heap=" + LTrim( Str( nHeapSize() ) ) + ")" + CRLF
   cErrorLog += "      PRG Stack:          " + ;
                LTrim( Str( 65535 - ( nStatics() * 14 ) - nInitDSSize() ) ) + ;
                " bytes" + CRLF
   #endif

   #ifdef __CLIPPER__
      cErrorLog += "      " + LTrim( Str( nStatics() ) ) + " Static variables: " + ;
                   LTrim( Str( nStatics() * 14 ) ) + " bytes" + CRLF + CRLF
   #else
      cErrorLog += "      " + LTrim( Str( nStatics() ) ) + " Static variables" + ;
                   CRLF + CRLF
   #endif

   cErrorLog += "   Dynamic memory consume:" + CRLF
   cErrorLog += "      Actual  Value: " + Str( MemUsed() ) + " bytes" + CRLF
   cErrorLog += "      Highest Value: " + Str( MemMax() ) + " bytes" + CRLF
   // nSymNames() no longer returns a real value! 15/April/97
   /*
   cErrorLog += "   SYMBOLS segment" + CRLF
   cErrorLog += "      " + LTrim( Str( nSymNames() ) ) + " SymbolNames:   " + ;
                LTrim( Str( nSymNames() * 16 ) ) + " bytes"
   */

   // Generates a file with an Error Log

   BEGIN SEQUENCE
      oOldError = ErrorBlock( { || DoBreak() } )
      MemoWrit( "Error.log", cErrorLog )
   END SEQUENCE
   ErrorBlock( oOldError )

   DEFINE FONT oFont NAME "Ms Sans Serif" SIZE 0, -10

   DEFINE DIALOG oDlg ;
      SIZE 300, 200 + If( lIsWinNT, 50, 0 ) ;
      TITLE DLG_TITLE ;
      FONT oFont

   @ 0, 0 SAY oSay PROMPT OemToAnsi( cMessage ) ;
      CENTERED OF oDlg FONT oFont SIZE 149, 20

   oSay:nStyle   = nOR( oSay:nStyle, 128 )   // SS_NOPREFIX
   oSay:nTop     =   3
   oSay:nLeft    =  22
   oSay:nBottom  =  25
   oSay:nRight   = 148

   @ 24,   6 SAY "&Stack List" OF oDlg FONT oFont PIXEL
   @ 24, 106 SAY "See Error.log file" OF oDlg FONT oFont PIXEL

   n = aStack[ 1 ]

   @ 33, 3 LISTBOX oLbx VAR n ITEMS aStack OF oDlg ;
      SIZE 145, 60 + If( lIsWinNT, 18, 0 ) PIXEL

   if nButtons == 1 .or. nButtons == 3
      @ 88 + If( lIsWinNT, 24, 0 ), 60 BUTTON "&Quit" OF oDlg ACTION oDlg:End() ;
         SIZE 30, 11 PIXEL FONT oFont DEFAULT
   else
      @ 88 + If( lIsWinNT, 24, 0 ), 37 BUTTON "&Quit" OF oDlg ACTION oDlg:End() ;
         SIZE 30, 11 PIXEL FONT oFont
   endif

   if e:CanRetry
      @ 88 + If( lIsWinNT, 24, 0 ), If( nButtons == 2, 82, 13 ) BUTTON "&Retry" ;
         OF oDlg ACTION ( lRet  := .t., oDlg:End() ) ;
         SIZE 30, 11 FONT oFont PIXEL
   endif

   if e:CanDefault
      @ 88 + If( lIsWinNT, 24, 0 ), 108 BUTTON "&Default"  OF oDlg ;
         ACTION ( lRet  := .f., oDlg:End() ) ;
         SIZE 30, 11 FONT oFont PIXEL
   endif

   ACTIVATE DIALOG oDlg CENTERED ;
      ON PAINT DrawBitmap( hDC, hLogo, 6, 6 )

   DeleteObject( hLogo )
   oFont:End()

   if lRet == nil .or. ( !LWRunning() .and. lRet )
      SET RESOURCES TO
      ErrorLevel( 1 )
      QUIT              // must be QUIT !!!
   endif

return lRet

//----------------------------------------------------------------------------//

static function DoBreak()

   BREAK

return nil

//----------------------------------------------------------------------------//

static func ErrorMessage( e )

	// start error message
    local cMessage := if( empty( e:OsCode ), ;
                          if( e:severity > ES_WARNING, "Error ", "Warning " ),;
                          "(DOS Error " + NTRIM(e:osCode) + ") " )

	// add subsystem name if available
    cMessage += if( ValType( e:SubSystem ) == "C",;
                    e:SubSystem()                ,;
                    "???" )

	// add subsystem's error code if available
    cMessage += if( ValType( e:SubCode ) == "N",;
                    "/" + NTRIM( e:SubCode )   ,;
                    "/???" )
	// add error description if available
  if ( ValType( e:Description ) == "C" )
        cMessage += "  " + e:Description
	end

	// add either filename or operation
    cMessage += if( ! Empty( e:FileName ),;
                    ": " + e:FileName   ,;
                    if( !Empty( e:Operation ),;
                        ": " + e:Operation   ,;
                        "" ) )
return cMessage

//----------------------------------------------------------------------------//
// returns extended info for a certain variable type

static function cGetInfo( uVal )

   local cType := ValType( uVal )

   do case
      case cType == "C"
           return '"' + cValToChar( uVal ) + '"'

      case cType == "O"
           return "Class: " + uVal:ClassName()

      case cType == "A"
           return "Len: " + Str( Len( uVal ), 4 )

      otherwise
           return cValToChar( uVal )
   endcase

return nil

//----------------------------------------------------------------------------//