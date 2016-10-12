/*
  (c) copyright xHarbour.com Inc. http://www.xHarbour.com
  Author: Ron Pinkas Ron@xHarbour.com

  This source file is an intellectual property of xHarbour.com Inc.
  You may NOT forward or share this file under any conditions!
*/

#ifdef __PLATFORM__Windows
  #define DRIVE_SEPARATOR ':'
  #define DIR_SEPARATOR '\'
  #define EOL Chr(13) + Chr(10)
#else
  #define DRIVE_SEPARATOR ''
  #define DIR_SEPARATOR '/'
  #define EOL Chr(10)
#endif

#define _BUILD_ "4.7"
//#define d_Debug

FUNCTION Main( ... )

   LOCAL aParams := HB_aParams(), oProject, cTarget, nAt, cParam, cArgument, aTokens
   LOCAL BCC, GCC, MINGW, POCC, VC, XCC, xHB_Root, xHB_LibFolder, XHB_Exe, cLibFolders, cIncludeFolders
   LOCAL cDefines, lClean := .F., lLink := .F., C_Compiler, C_Folder, C_Flags, PRG_Flags
   LOCAL GUI_Root, GUI_Flags, GUI_LibFolder
   LOCAL cDefaultFolder, cPRG_OutputFolder, cC_OutputFolder, cRC_OutputFolder
   LOCAL bErrorHandler, bProgress := { |Module| OutStd( DiskName() + DRIVE_SEPARATOR + DIR_SEPARATOR + CurDir() + DIR_SEPARATOR + Module:Project:cFile + "->" + Module:cFile + EOL ) }
   LOCAL lShowErrors := .T., oErr, bDefaultHandler := { |e| Break(e) }, lExpand := .F., lXbp := .T., lNew := .F., lNoAutoFWH := .F.
   LOCAL nIndex, lDebug := .F., cTargetFolder := "", lGUI := .F., lWizard := .F., lMT := .F., lUseDLL := .F.
   LOCAL cFile, cWorkFolder, cCurrentFolder
   LOCAL lPRG_Debug := .F., lPRG_ClassicDebug := .F.
   LOCAL cIni

   ErrorBlock( {|oErr| TraceLog( ValToPrg( oErr ) ), ;
                        Alert( IIF( oProject == NIL, "Error!", "Sorry, could not build (1):" + oProject:cFile ) + ";;Operation: " + oErr:Operation + ";Description: " + oErr:Description ), ;
                        __Quit() } )

   #ifdef __PLATFORM__Windows
      #ifdef GUI
         bProgress := { |Module| GUI_Progress( Module:cFile ) }
         bDefaultHandler := NIL

         /*
         ErrorBlock( {|oErr| TraceLog( ValToPrg( oErr ) ), ;
                             Alert( "Sorry, could not build (2):;" + oProject:cFile + ";;Operation: " + oErr:Operation + ";Description: " + oErr:Description ), ;
                             GUI_OnError(oErr) } )
         */
      #endif
   #endif

   #ifdef DEMO
    #ifndef GUI
     ? "Thank you for evaluating xHarbour xBuilder."
     ?
     ? "Copyright (c) 2003-"+Str(Year(Date()))+" xHarbour.com Inc."
     ? "http://www.xHarbour.com"
     ?
     WAIT
     CLS
     ? "Thank you for evaluating xHarbour xBuilder."
     ?
     ? "Copyright (c) 2003-"+Str(Year(Date()))+" xHarbour.com Inc."
     ? "http://www.xHarbour.com"
     ?
    #endif
   #endif

   IF PCount() == 0
      #ifdef __PLATFORM__Windows
         #ifdef GUI
            REQUEST xEditControlWindowProc

            TRY
               xBuildWizardGUI( NIL, bProgress, bDefaultHandler, /*{|o, p| GUI_OnFWH( o, p ) }*/ )
            CATCH oErr
               Alert( "Sorry, could not build (3):;" + oProject:cFile + ";;Operation: " + oErr:Operation + ";Description: " + oErr:Description )

               SET TRACE ON
               SET( _SET_TRACEFILE, "error.log" )
               SET( _SET_TRACESTACK, 2 )

               TraceLog( ValToPrg( oErr ) )

               RETURN 1
            END
         #else
            xBuildWizard()

            ? "For help with command line syntax, type: 'xbuild -help'"
            ?
         #endif
      #else
         xBuildWizard()

         ? "For help with command line syntax, type: 'xbuild -help'"
         ?
      #endif

      RETURN 0
   ELSEIF PCount() == 1 .AND. aParams[1] LIKE "[/\-]\?" .OR. Lower( aParams[1] ) LIKE "-{1,2}help"
      ?
      ? "xBuild [<target.exe|lib|a|dll>]|[<MainSource>] [<MoreSourceMask>] [switches]"
      ? "or"
      ? "xBuild <ProjectFile.exe|lib|a|dll.xbp> [switches]"
      ?
      WAIT
      CLS
      ? "Switches: -All (Force clean build.)"
      ? "          -B (compile with prg debug information.)"
      ? "          -C<Bcc|Gcc|Mingw|POCC|Vc|xCC>=<RootFolder>[,C_Flags]"
      ? "          -CLASSIC (link classic prg debugger)"
      ? "          -CON (direct error log to console instead of NotePad.)"
      ? "          -D<Define>[=<Value>][;<MoreDefines>]"
      ? "          -Debug (Compile with C low level debug info.)"
      ? "          -Expand (Expand file groups when generating project file.)"
      ? "          -G<GUI_Root>[,GUI_PRG_Flags [,<Optional_GUI_LibFolder>]]"
      ? "          -GUI (build as GUI application.)"
      ? "          -Help (Show this info screen.)"
      ? "          -I<IncludeFolder>[;<MoreFolders>]"
      ? "          -INI:<filename> use custom xbuild settings file."
      ? "          -Link (Force re-link of executable or dll)"
      ? "          -L<LibFolder[;<MoreFolders>] "
      ? "          -MT (build a Multi-Thread application.)"
      ? "          -New (New project from specified sources.)"
      ? "          -NoErr (Don't raise an error on failure.)"
      ? "          -NoLog (Don't display production log on failure.)"
      ? "          -NoXBP (Don't generate project file.)"
      ? "          -NoAutoFWH (Ignore presence of Fivewin.ch.)"
      ? "          -O<OutFolder>[,<PRG_OutFolder>[,<C_OutFolder>[,<RC_OutFolder>]]]"
      ? "          -Quiet (Don't display progress.)"
      ?
      WAIT
      CLS
      ? "Switches: -T<TargetFolder> (Will override target location.)"
      ? "          -UseDLL (create small exeutable which requires dll of the R/T.)"
      ? "          -Wizard (Open Wizard with all supplied data.)"
      ? "          -X<xHarbourRoot>[,PRG_Flags [,<OptionalLibFolder>]]"
      ? "          -V (Show xBuild version)"
      ?
      WAIT

      RETURN 0
   ENDIF

   FOR EACH cParam in aParams
      IF cParam[1] == '-'
         //TraceLog( cParam )

         cArgument := SubStr( cParam, 3 )

         SWITCH cParam[2]
            CASE 'a'
            CASE 'A'
               IF Lower( cArgument ) == "ll"
                  lClean := .T.
               ELSE
                  Throw( ErrorNew( "xBuild", 0, 1001, "Command Processor", "Arguments() - unsupported switch: " + cParam, aParams ) )
               ENDIF
               EXIT

            CASE 'b'
            CASE 'B'
               lPRG_Debug := .T.
               EXIT

            CASE 'c'
            CASE 'C'
               IF Lower( cArgument ) == "on"
                  bErrorHandler := {|e| OutErr( MemoRead( oProject:cFile + ".log" ) ), Break(e) }
                  EXIT
               ELSEIF Lower( cArgument ) == "lassic"
                  lPRG_ClassicDebug := .T.
                  EXIT
               ENDIF

               aTokens := HB_aTokens( cArgument, '=' )
               IF Len( aTokens ) != 2
                  Throw( ErrorNew( "xBuild", 0, 1001, "Command Processor", "invalid number of arguments: " + cArgument, aParams ) )
               ENDIF

               C_Compiler := aTokens[1]

               aTokens := HB_aTokens( aTokens[2], ',' )

               C_Folder := aTokens[1]

               IF Len( aTokens ) >= 2
                  C_Flags := aTokens[2]
               ENDIF

               IF Lower( C_Compiler ) == "bcc"
                  BCC := C_Folder
               ELSEIF Lower( C_Compiler ) == "gcc"
                  GCC := C_Folder
               ELSEIF Lower( C_Compiler ) == "mingw"
                  MINGW := C_Folder
               ELSEIF Lower( C_Compiler ) == "pocc"
                  POCC := C_Folder
               ELSEIF Lower( C_Compiler ) == "vc"
                  VC := C_Folder
               ELSEIF Lower( C_Compiler ) == "xcc"
                  XCC := C_Folder
               ELSE
                  Throw( ErrorNew( "xBuild", 0, 1001, "Command Processor", "unsupported compiler: " + C_Compiler, aParams ) )
               ENDIF
               EXIT

            CASE 'd'
            CASE 'D'
               IF Lower( cArgument ) == "ebug"
                  lDebug := .T.
               ELSE
                  IF cDefines == NIL
                     cDefines := cArgument
                  ELSE
                     cDefines += ';' + cArgument
                  ENDIF
               ENDIF
               EXIT

            CASE 'e'
            CASE 'E'
               IF cArgument[1] == ':'
                  aTokens := HB_aTokens( SubStr( cArgument, 2 ), '=' )
                  AddEnvVar( aTokens[1], aTokens[2] )
               ELSEIF Lower( cArgument ) == "xpand"
                  lExpand := .T.
               ELSE
                  Throw( ErrorNew( "xBuild", 0, 1001, "Command Processor", "Arguments() - unsupported switch: " + cParam, aParams ) )
               ENDIF
               EXIT

            CASE 'g'
            CASE 'G'
               IF Lower( cArgument ) == "ui"
									lGUI := .T.
							 ELSE
               		aTokens := HB_aTokens( cArgument, ',' )
               		IF Len( aTokens ) >= 2
               		   GUI_Root := aTokens[1]
               		   GUI_Flags := aTokens[2]

               		   IF Len( aTokens ) >= 3
                        GUI_LibFolder := aTokens[3]
               		   ENDIF
               		ELSE
               		   GUI_Root := cArgument
               		ENDIF
							 ENDIF
               EXIT

            CASE 'i'
            CASE 'I'
               IF Lower( cArgument ) = "ni:"
                  cIni := SubStr( cArgument, 4 )
               ELSE
                  IF cIncludeFolders == NIL
                     cIncludeFolders := cArgument
                  ELSE
                     cIncludeFolders += ';' + cArgument
                  ENDIF
               ENDIF
               EXIT

            CASE 'l'
            CASE 'L'
               IF Lower( cArgument ) == "ink"
                  lLink := .T.
               ELSE
                  IF cLibFolders == NIL
                     cLibFolders := cArgument
                  ELSE
                     cLibFolders += ';' + cArgument
                  ENDIF
               ENDIF
               EXIT

            CASE 'm'
            CASE 'M'
               IF Lower( cArgument ) == "t"
                  lMT := .T.
               ELSE
                  Throw( ErrorNew( "xBuild", 0, 1001, "Command Processor", "Arguments() - unsupported switch: " + cParam, aParams ) )
               ENDIF
               EXIT

            CASE 'n'
            CASE 'N'
               IF Lower( cArgument ) == "olog"
                  bErrorHandler := bDefaultHandler
               ELSEIF Lower( cArgument ) == "oerr"
                  lShowErrors := .F.
               ELSEIF Lower( cArgument ) == "oxbp"
                  lXbp := .F.
               ELSEIF Lower( cArgument ) == "ew"
                  lNew := .T.
               ELSEIF Lower( cArgument ) == "oautofwh"
                  lNoAutoFWH := .T.
               ELSE
                  Throw( ErrorNew( "xBuild", 0, 1001, "Command Processor", "Arguments() - unsupported switch: " + cParam, aParams ) )
               ENDIF
               EXIT

            CASE 'o'
            CASE 'O'
               aTokens := HB_aTokens( cArgument, ',' )
               IF Len( aTokens ) >= 2
                  cDefaultFolder    := aTokens[1]
                  cPRG_OutputFolder := aTokens[2]

                  IF Len( aTokens ) >= 3
                     cC_OutputFolder := aTokens[3]
                  ENDIF

                  IF Len( aTokens ) >= 4
                     cRC_OutputFolder := aTokens[4]
                  ENDIF
               ELSE
                  cDefaultFolder := cArgument
               ENDIF
               EXIT

            CASE 'q'
            CASE 'Q'
               IF Lower( cArgument ) == "uiet"
                  bProgress := {|| NIL }
               ELSE
                  Throw( ErrorNew( "xBuild", 0, 1001, "Command Processor", "Arguments() - unsupported switch: " + cParam, aParams ) )
               ENDIF
               EXIT

            CASE 't'
            CASE 'T'
               cTargetFolder := cArgument
               EXIT

            CASE 'u'
            CASE 'U'
               IF Lower( cArgument ) == "sedll"
                  lUseDLL := .T.
               ELSE
                  Throw( ErrorNew( "xBuild", 0, 1001, "Command Processor", "Arguments() - unsupported switch: " + cParam, aParams ) )
               ENDIF
               EXIT

            CASE 'v'
            CASE 'V'
               Alert( {"xBuild version " + _BUILD_ + " Dated " + __DATE__ + " " + __TIME__ ,;
                       "" ,;
                       "(c) copyright xHarbour.com Inc. http://www.xHarbour.com"} )
               RETURN 0

            CASE 'w'
            CASE 'W'
               IF Lower( cArgument ) == "izard"
                  lWizard := .T.
               ELSE
                  Throw( ErrorNew( "xBuild", 0, 1001, "Command Processor", "Arguments() - unsupported switch: " + cParam, aParams ) )
               ENDIF
               EXIT

            CASE 'x'
            CASE 'X'
               aTokens := HB_aTokens( cArgument, ',' )
               IF Len( aTokens ) >= 2
                  xHB_Root := aTokens[1]
                  PRG_Flags := aTokens[2]

                  IF Len( aTokens ) >= 3
                     xHB_LibFolder := aTokens[3]
                  ENDIF
               ELSE
                  xHB_Root := cArgument
               ENDIF

              EXIT

            DEFAULT
              Throw( ErrorNew( "xBuild", 0, 1001, "Command Processor", "Arguments() - unsupported switch: " + cParam[2], aParams ) )
         END

         cParam := ""
      ELSEIF cParam[1] == '"' .AND. cParam[-1] == '"'
         cParam := SubStr( cParam, 2, Len( cParam ) - 2 )
      ENDIF
   NEXT

   IF ! Empty( xHB_Root )
      IF xHB_Root[-1] != DIR_SEPARATOR
         xHB_Root += DIR_SEPARATOR
      ENDIF

      #ifdef __PLATFORM__Windows
            xHB_Exe := "xhb.exe"
      #else
          xHB_Exe := "xhb"
      #endif

      IF ! File( xHB_Root + "bin" + DIR_SEPARATOR + xHB_Exe )
         xHB_Exe := NIL
      ENDIF
   ENDIF

   FOR nIndex := 1 TO Len( aParams )
      IF aParams[ nIndex ] == ""
         aDel( aParams, nIndex--, .T. )
      ENDIF
   NEXT

   IF Len( aParams ) == 0
      #ifdef __PLATFORM__Windows
         #ifdef GUI
            REQUEST xEditControlWindowProc

            TRY
               xBuildWizardGUI( NIL, bProgress, bDefaultHandler, /*{|o, p| GUI_OnFWH( o, p ) }*/ )
            CATCH oErr
               Alert( "Sorry, could not build (4):;" + oProject:cFile + ";;Operation: " + oErr:Operation + ";Description: " + oErr:Description )

               SET TRACE ON
               SET( _SET_TRACEFILE, "error.log" )
               SET( _SET_TRACESTACK, 2 )

               TraceLog( ValToPrg( oErr ) )

               RETURN 1
            END
         #else
            xBuildWizard()

            ? "For help with command line syntax, type: 'xbuild -help'"
            ?
         #endif
      #else
         xBuildWizard()

         ? "For help with command line syntax, type: 'xbuild -help'"
         ?
      #endif

      RETURN 0
   ENDIF

   cTarget := lower( aParams[1] )

   IF ( cTarget[-1] == 'e' .AND. cTarget[-2] == 'x' .AND. cTarget[-3] == 'e' .AND. cTarget[-4] == '.' ) .OR. ;
      ( cTarget[-1] == 'b' .AND. cTarget[-2] == 'i' .AND. cTarget[-3] == 'l' .AND. cTarget[-4] == '.' ) .OR. ;
      ( cTarget[-1] == 'l' .AND. cTarget[-2] == 'l' .AND. cTarget[-3] == 'd' .AND. cTarget[-4] == '.' ) .OR. ;
      ( cTarget[-1] == 'b' .AND. cTarget[-2] == 'r' .AND. cTarget[-3] == 'h' .AND. cTarget[-4] == '.' ) .OR. ;
      ( cTarget[-1] == 'a' .AND. cTarget[-2] == '.')

      IF lNew == .F. .AND. File( cTarget + ".xbp" )
         aParams[1] += ".xbp"
         cTarget := lower( aParams[1] )
      ELSE
         IF File( Left( cTarget, Len( cTarget ) - 4 ) + ".prg" )
            aParams[1][-3] := 'p'
            aParams[1][-2] := 'r'
            aParams[1][-1] := 'g'
         ELSE
            aDel( aParams, 1, .T. )
         ENDIF
      ENDIF
   ELSE
      nAt := RAt( ".", cTarget )

      IF lNew == .F. .AND. nAt == 0
         IF File( cTarget + ".exe.xbp" )
            aParams[1] += ".exe.xbp"
            cTarget := lower( aParams[1] )
         ELSEIF File( cTarget + ".lib.xbp" )
            aParams[1] += ".lib.xbp"
            cTarget := lower( aParams[1] )
         ELSEIF File( cTarget + ".dll.xbp" )
            aParams[1] += ".dll.xbp"
            cTarget := lower( aParams[1] )
         ELSEIF File( cTarget + ".hrb.xbp" )
            aParams[1] += ".hrb.xbp"
            cTarget := lower( aParams[1] )
         ELSEIF File( cTarget + ".a.xbp" )
            aParams[1] += ".a.xbp"
            cTarget := lower( aParams[1] )
         ENDIF
      ENDIF
   ENDIF

   IF lNew == .F. .AND. ( cTarget[-1] == 'p' .AND. cTarget[-2] == 'b' .AND. cTarget[-3] == 'x' .AND. cTarget[-4] == '.' )
      TRY
         cFile := aParams[1]

         nAt := RAt( DIR_SEPARATOR, cFile )
         IF nAt > 0
            cCurrentFolder := DIR_SEPARATOR + CurDir()
            cWorkFolder := Left( cFile, nAt )
            cFile := SubStr( cFile, nAt + 1 )

            IF DirChange( cWorkFolder ) != 0
               Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Could not switch to folder: " + cWorkFolder, HB_aParams() ) )
            ENDIF
         ENDIF

         oProject := LoadProject( cFile, NIL, cIni )

         #ifdef d_Debug
          Tracelog( ValToPrg( oProject) )
         #endif

      CATCH oErr
         TraceLog( oErr:Operation, oErr:Description, oErr:ProcName, oErr:ProcLine, ValToPrg( oErr:aaStack ) )
         Alert( "Sorry, could not load your project [" + cFile + "];;Operation: " + oErr:Operation + ";Description: " + oErr:Description )

         SET TRACE ON
         SET( _SET_TRACEFILE, "error.log" )
         SET( _SET_TRACESTACK, 2 )

         TraceLog( ValToPrg( oErr ) )

         RETURN 1
      END

      aDel( aParams, 1, .T. )

      // Target includes a FOLDER
      nAt := RAt( DIR_SEPARATOR, cTarget )
      IF nAt > 0
         cTarget := SubStr( cTarget, nAt + 1 )
      ENDIF
   ELSE
      nAt := RAt( ".", cTarget )

      #ifdef __PLATFORM__Windows
         IF nAt == 0
            cTarget += ".exe"
         ELSE
            IF ! Lower( SubStr( cTarget, nAt ) ) + ';' $ ".exe;.lib;.dll;.hrb;"
               cTarget := Left( cTarget, nAt ) + "exe"
            ENDIF
         ENDIF
      #else
         IF nAt == 0
            // Do nothing
         ELSE
            IF ! ( Right( cTarget, 2 ) == ".a" .OR. Right( cTarget, 3 ) == ".so" )
               cTarget := Left( cTarget, nAt - 1 )
            ENDIF
         ENDIF
      #endif

      // Target includes a FOLDER
      nAt := RAt( DIR_SEPARATOR, cTarget )
      IF nAt > 0
         cTargetFolder := Left( cTarget, nAt )
         cTarget := SubStr( cTarget, nAt + 1 )
      ENDIF
   ENDIF

   // Validate the target folder.
   IF ! Empty( cTargetFolder )
      IF cTargetFolder[-1] != DIR_SEPARATOR
         cTargetFolder += DIR_SEPARATOR
      ENDIF
   ENDIF

   IF ".a" IN cTarget
      cTarget := cTargetFolder + "lib" + cTarget
   ELSE
      cTarget := cTargetFolder + cTarget
   ENDIF

   IF oProject == NIL
      #ifdef d_Debug
         TraceLog(cTarget)
      #endif

      oProject := TMakeProject():New( cTarget )

      #ifdef d_Debug
         TraceLog( ValToPrg( oProject ))
      #endif
   ELSE
      IF ! Empty( cTargetFolder )
         oProject:TargetFolder := cTargetFolder
      ENDIF
   ENDIF

   IF GUI_Root != NIL
      oProject:Set_GUI( GUI_Root, GUI_Flags, GUI_LibFolder )
   ENDIF

   IF cDefines != NIL
      oProject:SetDefines( cDefines ) // standard compiler includes are auto managed.
   ENDIF

   IF cIncludeFolders != NIL
      oProject:SetIncludeFolders( cIncludeFolders ) // standard compiler includes are auto managed.
   ENDIF

   IF cDefaultFolder != NIL
      oProject:OutputFolder     := cDefaultFolder // will be created if needed.
      oProject:PRG_OutputFolder := cPRG_OutputFolder
      oProject:C_OutputFolder   := cC_OutputFolder
      oProject:RC_OutputFolder  := cRC_OutputFolder
   ENDIF

   IF cLibFolders != NIL
      oProject:LibFolders := cLibFolders // standard compiler libs are auto managed.
   ENDIF

   IF ! Empty( xHB_Root )
      oProject:Set_xHB( xHB_Root, PRG_Flags, xHB_LibFolder, xHB_Exe )
   ENDIF

   // Note From Luiz By default Linux only install the shared library version(.so)
   // static library(.a) version are only instaled when you install the
   // -devel-static(some distros is -dev-static) package
   // if the static library is not found( the check is for libc.a from glibc-devel-static package)
   // Assume the dll usage( linux .so libraries)

   #ifndef __PLATFORM__Windows
      IF !File("/usr/lib/libc.a") .and. !File("/lib/libc.a")
         lUseDLL := .T.
      ENDIF
   #endif

   IF lNoAutoFWH
      oProject:lNoAutoFWH := .T.
      TraceLog( HB_ArrayID( oProject ) )
   ENDIF
   IF lUseDLL
      oProject:lUseDLL := .T.
   ENDIF

   IF BCC != NIL
      oProject:Set_BCC( BCC, C_Flags )
   ENDIF

   IF GCC != NIL
      oProject:Set_GCC( GCC, C_Flags )
   ENDIF

   IF MINGW != NIL
      oProject:Set_MINGW( MINGW, C_Flags )
   ENDIF

   IF POCC != NIL
      oProject:Set_POCC( POCC, C_Flags )
   ENDIF

   IF VC != NIL
      oProject:Set_VC( VC, C_Flags )
   ENDIF

   IF XCC != NIL
      oProject:Set_XCC( XCC, C_Flags )
   ENDIF

   IF ! Empty( cIni )
      oProject:cIni := cIni
      TRY
         LoadIni( oProject )
      CATCH
         Alert( "Warning! INI file: " + cIni + " not yet available." )
      END
   ENDIF

   FOR EACH cArgument in aParams
      IF ! '.' IN cArgument
         cArgument += ".prg"
      ENDIF

      oProject:AddFiles( cArgument )
   NEXT

   oProject:lClean := lClean
   oProject:lLink  := lLink
   oProject:lExpand := lExpand
   oProject:lXbp := lXbp

   IF lDebug
      oProject:lDebug := .T.
   ENDIF

	 IF lGUI
      oProject:lGUI := .T.
	 ENDIF

   IF lMT
      oProject:lMT := .T.
   ENDIF

   IF lPRG_Debug
      oProject:lPRG_Debug := .T.
   ENDIF

   IF lPRG_ClassicDebug
      oProject:lPRG_ClassicDebug := .T.
   ENDIF

   IF lWizard
      TRY
         #if defined( __PLATFORM__Windows ) .AND. defined( GUI )
            xBuildWizardGUI( oProject, bProgress, bDefaultHandler, /*{|o, p| GUI_OnFWH( o, p ) }*/ )
         #else
            xBuildWizard( oProject )
         #endif

      CATCH oErr
         Alert( "Sorry, could not build (5):;" + oProject:cFile + ";;Operation: " + oErr:Operation + ";Description: " + oErr:Description )

         SET TRACE ON
         SET( _SET_TRACEFILE, "error.log" )
         SET( _SET_TRACESTACK, 2 )

         TraceLog( ValToPrg( oErr ) )

         RETURN 1
      END
   ELSE
      TRY
         oProject:Make( bErrorHandler, bProgress )

         #ifdef GUI

            Alert( "Project built successfuly." )
         #endif

      CATCH oErr
         IF lShowErrors
            Alert( "Sorry, could not build (6):;" + oProject:cFile + ";;Operation: " + oErr:Operation + ";Description: " + oErr:Description )
         ELSE
            TraceLog( "Sorry, could not build your project (6).;;Operation: " + oErr:Operation + ";Description: " + oErr:Description )
         ENDIF

         SET TRACE ON
         SET( _SET_TRACEFILE, "error.log" )
         SET( _SET_TRACESTACK, 2 )

         IF cCurrentFolder != NIL
            DirChange( cCurrentFolder )
         ENDIF

         TraceLog( ValToPrg( oErr ) )

         ErrorLevel( 1 )
         RETURN 1
      END

      IF cCurrentFolder != NIL
         DirChange( cCurrentFolder )
      ENDIF
   ENDIF

RETURN 0

#ifndef GUI

   PROCEDURE xBuildWizard( oProject )

      LOCAL cTarget, cMain, cTargetType, xHB_Root, xHB_LibFolder, XHB_Exe, cLibFolders, cIncludeFolders, cDefines
      LOCAL C_Compiler, C_Root, C_Flags, PRG_Flags
      LOCAL GUI_Root, GUI_Flags, GUI_LibFolder
      LOCAL cDefaultFolder, cPRG_OutputFolder, cC_OutputFolder, cRC_OutputFolder

      LOCAL lClean := .F., lDebug := .F., bErrorHandler, bProgress := {|Module| SetPos( 21, 13 ), QQOut( Module:cFile ) }
      LOCAL lXbp := .T., lGUI := .F., lMT := .F.

      LOCAL lShowErrors := .T., oErr, bDefaultHandler := {|e| Break(e) }, lExpand := .F.
      LOCAL acMasks[7], cMask
      LOCAL GetList := {}, cScreen, lCancel, lPrevious, lNext, lFinish, nWizard := 0, nDirection := 1

      LOCAL nPresetRow := Row(), nPresetCol := Col()
      LOCAL nMasks
      LOCAL lLink := .F.

      // Question To Ron
      // Should we Enable Mouse Support
      SET( _SET_EVENTMASK, 159 )

      IF oProject == NIL
         cTarget           := Space(128)
         cMain             := Space(128)
         xHB_Root          := Space(128)
         xHB_LibFolder     := Space(128)
         cLibFolders       := Space(128)
         cIncludeFolders   := Space(128)
         cDefines          := Space(128)
         //C_Compiler        := Space(128)
         C_Root            := Space(128)
         C_Flags           := Space(128)
         PRG_Flags         := Space(128)
         GUI_Root          := Space(128)
         GUI_Flags         := Space(128)
         GUI_LibFolder     := Space(128)
         cDefaultFolder    := Space(128)
         cPRG_OutputFolder := Space(128)
         cC_OutputFolder   := Space(128)
         cRC_OutputFolder  := Space(128)

         aFill( acMasks, Space(128) )
      ELSE
         WITH OBJECT oProject
            cTarget           := Pad( :TargetFolder + IIF( Empty( :TargetFolder ) .OR. :TargetFolder[-1] == DIR_SEPARATOR, "", DIR_SEPARATOR ) + :cFile, 128 )
            cMain             := Pad( :aLiteralDependancies[1], 128 )
            xHB_Root          := Pad( :xHB_Root, 128 )
            xHB_LibFolder     := Pad( SubStr( :xHB_LibFolder, Len( RTrim( :xHB_Root ) ) + 1 ), 128 )
            cLibFolders       := Pad( :LibFolders, 128 )
            cIncludeFolders   := Pad( :IncludeFolders, 128 )
            cDefines          := Pad( SubStr( StrTran( :Defines, "-D", ";" ), 2 ), 128 )

            IF ! Empty( :C_Executable )
               DO CASE
                  CASE :C_Executable == "bcc32.exe"
                     C_Compiler := "BCC"

                  CASE :C_Executable == "gcc.exe"
                     #ifdef __PLATFORM__Windows
                        C_Compiler := "GCC"
                     #else
                        C_Compiler := "MingW"
                     #endif

                  CASE :C_Executable == "cl.exe"
                     C_Compiler := "MSVC"

                  CASE :C_Executable == "pocc.exe"
                     C_Compiler := "PellesC"

                  CASE :C_Executable == "xcc.exe"
                     C_Compiler := "xCC"

                  OTHERWISE
                     Alert( "Unsupported C Compiler: " + :C_Executable )
               END
            ELSE
               //C_Compiler        := Pad( :C_Executable, 128 )
            ENDIF

            C_Root            := Pad( :C_Root, 128 )
            C_Flags           := Pad( :C_Flags, 128 )

            PRG_Flags         := Pad( :PRG_Flags, 128 )
            GUI_Root          := Pad( :GUI_Root, 128 )
            GUI_Flags         := Pad( :GUI_PRGFlags, 128 )
            GUI_LibFolder     := Pad( :GUI_LibFolder, 128 )
            cDefaultFolder    := Pad( :OutputFolder, 128 )
            cPRG_OutputFolder := Pad( :PRG_OutputFolder, 128 )
            cC_OutputFolder   := Pad( :C_OutputFolder, 128 )
            cRC_OutputFolder  := Pad( :RC_OutputFolder, 128 )

            nMasks := Len( :aLiteralDependancies )
            acMasks[1]        := IIF( nMasks > 1, :aLiteralDependancies[2], NIL )
            acMasks[2]        := IIF( nMasks > 2, :aLiteralDependancies[3], NIL )
            acMasks[3]        := IIF( nMasks > 3, :aLiteralDependancies[4], NIL )
            acMasks[4]        := IIF( nMasks > 4, :aLiteralDependancies[5], NIL )
            acMasks[5]        := IIF( nMasks > 5, :aLiteralDependancies[6], NIL )
            acMasks[6]        := IIF( nMasks > 6, :aLiteralDependancies[7], NIL )
            acMasks[7]        := IIF( nMasks > 7, :aLiteralDependancies[8], NIL )
         END
      ENDIF

      cTargetType       := ""

      IF Empty( C_Compiler )
         #ifdef __PLATFORM__Windows
            C_Compiler        := "xCC"
         #else
            C_Compiler        := "GCC"
         #endif
      ENDIF

      SET SCOREBOARD OFF
      SAVE SCREEN TO cScreen
      SET COLOR   TO "N/W, W+/B, , , B/W"

      BEGIN SEQUENCE

         DO WHILE LastKey() != 27
            nWizard += nDirection

            DO CASE
               CASE nWizard == 1
                  // Target
                  @  8, 10 TO 22, 69
                  @ 22, 12 SAY '['
                  @ 22, 67 SAY ']'

                  @  9, 11 CLEAR TO 21,68

                  @  9, 32 SAY "xBuild Wizard"

                  @ 11, 12 SAY "Target:" GET cTarget PICTURE "@S36" ;
                                                     VALID ValidateTarget( @cTarget, @cMain, @cTargetType ) ;
                                                     MESSAGE "Optionally may include extension."

                  @ 13, 20, 18, 30 GET cTargetType   LISTBOX { "exe", "lib", "dll", "a" } ;
                                                     /*WHEN cTargetType == "" */;
                                                     CAPTION "&Type:" ;
                                                     MESSAGE "Use [Up], [Down], or [Space] to select." ;
                                                     DROPDOWN ;
                                                     COLOR "B/W, B/W, N/W, W+/B, N/W, N/W, R/W, R/W"

                  @ 16, 12 GET lClean CHECKBOX       CAPTION "Force Clean build." ;
                                                     MESSAGE "Will force recompilation of all modules."

                  @ 17, 12 GET lLink  CHECKBOX       CAPTION "Force Re-Link." ;
                                                     MESSAGE "Will force fresh linking of all modules."

                  @ 18, 12 GET lDebug CHECKBOX       CAPTION "Include Debug information." ;

                  @ 20, 12 GET lCancel PUSHBUTTON ;
                               WHEN LastKey() == 5 .OR. LastKey() == 271;
                               CAPTION "&Cancel"  ;
                               STATE { || BREAK( .T. ) } ;
                               COLOR "B/W, W+/B, R/W, R/W"

                  @ 20, 42 GET lPrevious PUSHBUTTON ;
                               WHEN .F. ;
                               CAPTION "&Previous"  ;
                               STATE { || ReadKill( .T. ), nDirection := -1 } ;
                               COLOR "N+/W, W+/B, R/W, R/W"

                  @ 20, 53 GET lNext PUSHBUTTON ;
                               CAPTION "&Next"  ;
                               STATE { || ReadKill( .T. ), nDirection := 1 } ;
                               COLOR "B/W, W+/B, R/W, R/W"

                  @ 20, 60 GET lFinish PUSHBUTTON ;
                               CAPTION "&Finish"  ;
                               STATE { || Break() } ;
                               COLOR "B/W, W+/B, R/W, R/W"

                  READ MSG AT 22, 13, 66

                  LOOP

               CASE nWizard == 2
                  // Main
                  IF cTargetType == "exe"
                     IF Empty( cMain )
                        cMain := Pad( RTrim( cTarget ) + ".prg", 128 )
                     ENDIF
                  ENDIF

                  @ 10, 12 CLEAR TO 19, 66

                  @ 11, 12 SAY "Main Source:" GET cMain     PICTURE "@S30" ;
                                                            WHEN cTargetType == "Exe" ;
                                                            VALID ValidateMain( @cMain ) ;

                  @ 12, 12 SAY "  PRG Flags:" GET PRG_Flags PICTURE "@S30" ;
                                                            WHEN PRG_Flags_Needed( @cMain ) ;
                                                            MESSAGE "Defaults are '-n -w' only when left empty!"

                  @ 14, 12 SAY "    LIB Folders:" GET cLibFolders     PICTURE "@S29" ;
                                                                      VALID ValidateFolders( @cLibFolders ) ;
                                                                      MESSAGE "Default search folder for library files."

                  @ 15, 12 SAY "Include Folders:" GET cIncludeFolders PICTURE "@S29" ;
                                                                      VALID ValidateFolders( @cIncludeFolders ) ;
                                                                      MESSAGE "Default search folder for include files."

                  @ 16, 12 SAY "        Defines:" GET cDEfines        PICTURE "@S29" ;
                                                                      VALID ValidateDefines( @cDefines ) ;
                                                                      MESSAGE "Will be added to compile flags."


                  @ 20, 12 GET lCancel PUSHBUTTON ;
                               WHEN LastKey() == 5 .OR. LastKey() == 271;
                               CAPTION "&Cancel"  ;
                               STATE { || BREAK( .T. ) } ;
                               COLOR "B/W, W+/B, R/W, R/W"

                  @ 20, 42 GET lPrevious PUSHBUTTON ;
                               WHEN LastKey() == 5 .OR. LastKey() == 271;
                               CAPTION "&Previous"  ;
                               STATE { || ReadKill( .T. ), nDirection := -1 } ;
                               COLOR "B/W, W+/B, R/W, R/W"

                  @ 20, 53 GET lNext PUSHBUTTON ;
                               CAPTION "&Next"  ;
                               STATE { || ReadKill( .T. ), nDirection := 1 } ;
                               COLOR "B/W, W+/B, R/W, R/W"

                  @ 20, 60 GET lFinish PUSHBUTTON ;
                               CAPTION "&Finish"  ;
                               STATE { || Break() } ;
                               COLOR "B/W, W+/B, R/W, R/W"

                  READ MSG AT 21, 13, 66

                  LOOP

               CASE nWizard == 3
                  // Sources
                  @ 10, 12 CLEAR TO 19, 66

                  @ 11, 12 SAY "Modules:" GET acMasks[1] PICTURE "@S30" ;
                                                         VALID ValidateModules( acMasks, 1 ) ;
                                                         MESSAGE "'.prg', '.c', '.rc', '.obj', '.res' '.lib','.a', or '.xbp'."

                  @ 12, 12 SAY "Modules:" GET acMasks[2] PICTURE "@S30" ;
                                                         WHEN ! Empty( acMasks[1] ) ;
                                                         VALID ValidateModules( acMasks, 2 ) ;
                                                         MESSAGE "May be '.prg', '.c', '.rc', '.obj', '.res' '.lib','.a', or '.xbp'."

                  @ 13, 12 SAY "Modules:" GET acMasks[3] PICTURE "@S30" ;
                                                         WHEN ! Empty( acMasks[2] ) ;
                                                         VALID ValidateModules( acMasks, 3 ) ;
                                                         MESSAGE "May be '.prg', '.c', '.rc', '.obj', '.res' '.lib','.a', or '.xbp'."

                  @ 14, 12 SAY "Modules:" GET acMasks[4] PICTURE "@S30" ;
                                                         WHEN ! Empty( acMasks[3] ) ;
                                                         VALID ValidateModules( acMasks, 4 ) ;
                                                         MESSAGE "May be '.prg', '.c', '.rc', '.obj', '.res' '.lib','.a', or '.xbp'."

                  @ 15, 12 SAY "Modules:" GET acMasks[5] PICTURE "@S30" ;
                                                         WHEN ! Empty( acMasks[4] ) ;
                                                         VALID ValidateModules( acMasks, 5 ) ;
                                                         MESSAGE "May be '.prg', '.c', '.rc', '.obj', '.res' '.lib','.a', or '.xbp'."

                  @ 16, 12 SAY "Modules:" GET acMasks[6] PICTURE "@S30" ;
                                                         WHEN ! Empty( acMasks[5] ) ;
                                                         VALID ValidateModules( acMasks, 6 ) ;
                                                         MESSAGE "May be '.prg', '.c', '.rc', '.obj', '.res' '.lib','.a', or '.xbp'."

                  @ 17, 12 SAY "Modules:" GET acMasks[7] PICTURE "@S30" ;
                                                         WHEN ! Empty( acMasks[6] ) ;
                                                         VALID ValidateModules( acMasks, 7 ) ;
                                                         MESSAGE "May be '.prg', '.c', '.rc', '.obj', '.res' '.lib','.a', or '.xbp'."

                  @ 20, 12 GET lCancel PUSHBUTTON ;
                               WHEN LastKey() == 5 .OR. LastKey() == 271;
                               CAPTION "&Cancel"  ;
                               STATE { || BREAK( .T. ) } ;
                               COLOR "B/W, W+/B, R/W, R/W"

                  @ 20, 42 GET lPrevious PUSHBUTTON WHEN LastKey() == 5 .OR. LastKey() == 271;
                               CAPTION "&Previous"  ;
                               STATE { || ReadKill( .T. ), nDirection := -1 } ;
                               COLOR "B/W, W+/B, R/W, R/W"

                  @ 20, 53 GET lNext PUSHBUTTON ;
                               CAPTION "&Next"  ;
                               STATE { || ReadKill( .T. ), nDirection := 1 } ;
                               COLOR "B/W, W+/B, R/W, R/W"

                  @ 20, 60 GET lFinish PUSHBUTTON ;
                               CAPTION "&Finish"  ;
                               STATE { || Break() } ;
                               COLOR "B/W, W+/B, R/W, R/W"

                  READ MSG AT 21, 13, 66

                  LOOP

               CASE nWizard == 4
                  // Settings
                  #ifndef __PLATFORM__Windows
                     IF cTargetType == 'a'
                        xHB_Root:= Pad( "/usr/", 128 )
                        Find_xHarbour( @xHB_Root, @XHB_Exe )
                        Validate_xHB( xHB_Root, xHB_LibFolder )
                     ENDIF
                  #endif
                  @ 10, 12 CLEAR TO 19, 66

                  @ 11, 12 SAY "xHarbour Root:" GET xHB_Root         PICTURE "@S29" ;
                                                                     WHEN Find_xHarbour( @xHB_Root, @XHB_Exe ) ;
                                                                     VALID Validate_xHB( xHB_Root, xHB_LibFolder ) ;
                                                                     MESSAGE "Location of xHarbour on your system."


                  @ 12, 12 SAY "   Lib Folder:" GET xHB_LibFolder    PICTURE "@S29" ;
                                                                     VALID Validate_xHB( xHB_Root, xHB_LibFolder ) ;
                                                                     MESSAGE "Optional location of xHarbour libraries on your system."
#ifdef __PLATFORM__Windows
                  @ 14, 27, 22, 40 GET C_Compiler LISTBOX { "BCC", "MingW", "MSVC", "PellesC", "xCC" };
                                   WHEN IIF( XHB_Exe == "xhb.exe", ( GetActive():VarPut( "xCC" ), C_Root := xHB_Root, GetActive():Display(), .F. ), .T. )  ;
                                   VALID Find_CCompiler( C_Compiler, @C_Root ) ;
                                   CAPTION "&C Compiler:" ;
                                   MESSAGE "Please selected desired C Compiler." ;
                                   DROPDOWN ;
                                   COLOR "B/W, B/W, N/W, W+/B, N/W, N/W, R/W, R/W"

                  @ 15, 12 SAY "       C Root:" GET C_Root           PICTURE "@S29" ;
                                                                     WHEN XHB_Exe != "xhb.exe" ;
                                                                     VALID Validate_CCompiler( C_Compiler, C_Root ) ;
                                                                     MESSAGE "Location of C Compiler on your system."
#else
                  @ 14, 27, 22, 40 GET C_Compiler LISTBOX { "GCC" };
                                   WHEN IIF( XHB_Exe == "xhb" , ( GetActive():VarPut( "GCC" ), C_Root := xHB_Root, GetActive():Display(), .F. ), .T. )  ;
                                   VALID Find_CCompiler( C_Compiler, @C_Root ) ;
                                   CAPTION "&C Compiler:" ;
                                   MESSAGE "Please selected desired C Compiler." ;
                                   DROPDOWN ;
                                   COLOR "B/W, B/W, N/W, W+/B, N/W, N/W, R/W, R/W"

                  @ 15, 12 SAY "       C Root:" GET C_Root           PICTURE "@S29" ;
                                                                     WHEN XHB_Exe != "xhb";
                                                                     VALID Validate_CCompiler( C_Compiler, C_Root ) ;
                                                                     MESSAGE "Location of C Compiler on your system."

#endif

                  @ 16, 12 SAY "    C Flags:" GET C_Flags   PICTURE "@S30" ;
                                                            MESSAGE "Normally you don't need to specify any."

                  @ 20, 12 GET lCancel PUSHBUTTON ;
                               WHEN LastKey() == 5 .OR. LastKey() == 271;
                               CAPTION "&Cancel"  ;
                               STATE { || BREAK( .T. ) } ;
                               COLOR "B/W, W+/B, R/W, R/W"

                  @ 20, 42 GET lPrevious PUSHBUTTON ;
                               WHEN LastKey() == 5 .OR. LastKey() == 271;
                               CAPTION "&Previous"  ;
                               STATE { || ReadKill( .T. ), nDirection := -1 } ;
                               COLOR "B/W, W+/B, R/W, R/W"

                  @ 20, 53 GET lNext PUSHBUTTON ;
                               WHEN .F. ;
                               CAPTION "&Next"  ;
                               STATE { || ReadKill( .T. ), nDirection := 1 } ;
                               COLOR "N+/W, W+/B, R/W, R/W"

                  @ 20, 60 GET lFinish PUSHBUTTON ;
                               CAPTION "&Finish"  ;
                               STATE { || ReadKill( .T. ), nDirection := 1 } ;
                               COLOR "B/W, W+/B, R/W, R/W"

                  READ MSG AT 21, 13, 66

                  LOOP

               OTHERWISE
                  EXIT
            END
         ENDDO
      RECOVER

          IF cTargetType == "" .AND. ! '.' IN cTarget
             cTargetType := "exe"
          ENDIF

          Find_xHarbour( @xHB_Root )
          Find_CCompiler( @C_Compiler, @C_Root )

      END SEQUENCE

      IF LastKey() == 27
         RESTORE SCREEN FROM cScreen

         SET COLOR TO
         @ nPresetRow, nPresetCol
         SET CURSOR ON

         BREAK
      ENDIF

      cTarget := RTrim( cTarget )

      #ifdef __PLATFORM__Windows
         IF ! Right( cTarget, 3 ) == cTargetType
            cTarget += "." + cTargetType
         ENDIF
      #else
         IF ! ( Right( cTarget, 3 ) == cTargetType .OR. Right( cTarget, 1 ) == cTargetType )
            cTarget += "." + cTargetType
         ENDIF
      #endif

      IF oProject == NIL
         IF cTargetType == 'a'
             oProject := TMakeProject():New( "lib" + cTarget )
         ELSE
             #ifdef __PLATFORM__Windows
                TraceLog( cTarget )
                oProject := TMakeProject():New( cTarget )
             #else
                oProject := TMakeProject():New( Left( cTarget, At( ".", cTarget ) - 1 )
             #endif
         ENDIF
      ENDIF

      IF ! Empty( GUI_Root )
         oProject:Set_GUI( GUI_Root, GUI_Flags, GUI_LibFolder )
      ENDIF

      IF ! Empty( cDefines )
         oProject:SetDefines( RTrim( cDefines ) ) // standard compiler includes are auto managed.
      ENDIF

      IF ! Empty( cIncludeFolders )
         oProject:SetIncludeFolders( RTrim( cIncludeFolders ) ) // standard compiler includes are auto managed.
      ENDIF

      IF ! Empty( cDefaultFolder )
         oProject:OutputFolder     := cDefaultFolder // will be created if needed.
         oProject:PRG_OutputFolder := cPRG_OutputFolder
         oProject:C_OutputFolder   := cC_OutputFolder
         oProject:RC_OutputFolder  := cRC_OutputFolder
      ENDIF

      IF ! Empty( cLibFolders )
         oProject:LibFolders := RTrim( cLibFolders ) // standard compiler libs are auto managed.
      ENDIF

      oProject:Set_xHB( RTrim( xHB_Root ), RTrim( PRG_Flags ), RTrim( xHB_LibFolder ), XHB_Exe )

      C_Compiler := RTrim( C_Compiler )

      IF C_Compiler == "BCC"
         oProject:Set_BCC( RTrim( C_Root ), RTrim( C_Flags ) )
      ENDIF

      IF C_Compiler == "GCC"
         oProject:Set_GCC( RTrim( C_Root ), RTrim( C_Flags ) )
      ENDIF

      IF C_Compiler == "MingW"
         oProject:Set_MINGW( RTrim( C_Root ), RTrim( C_Flags ) )
      ENDIF

      IF C_Compiler == "PellesC"
         oProject:Set_POCC( RTrim( C_Root ), RTrim( C_Flags ) )
      ENDIF

      IF C_Compiler == "MSVC"
         oProject:Set_VC( RTrim( C_Root ), RTrim( C_Flags ) )
      ENDIF

      IF C_Compiler == "XCC"
         oProject:Set_XCC( RTrim( xHB_Root ), RTrim( C_Flags ) )
      ENDIF

      IF ! Empty( cMain )
         oProject:AddFiles( RTrim( cMain ) )
      ENDIF

      //TraceLog( ValToPrg( acMasks ) )
      FOR EACH cMask IN acMasks
         IF ! Empty( cMask )
            cMask := AllTrim( cMask )

            IF ".a" IN cMask
               oProject:AddFiles( "lib" + cMask )
            ELSE
               oProject:AddFiles( cMask )
            ENDIF
         ENDIF
      NEXT

      oProject:lClean  := lClean
      oProject:lLink   := lLink
      oProject:lExpand := lExpand
      oProject:lXbp    := lXbp

      IF lDebug
         oProject:lDebug := .T.
      ENDIF

      IF lGUI
         oProject:lGUI := .T.
      ENDIF

      IF lMT
         oProject:lMT := .T.
      ENDIF

      oProject:OnFWH := {|| SetFWH() }

      TRY
         oProject:Make( bErrorHandler, bProgress )
         @ 21, 13 CLEAR TO 21, 66
         @ 21, 13 SAY "Finished: " + cTarget
      CATCH oErr
         //IF lShowErrors
            @ 21, 13 SAY "Failed: " + CStr( oErr:Description )
         //ENDIF

         SET TRACE ON
         SET( _SET_TRACEFILE, "trace.log" )
         SET( _SET_TRACESTACK, 2 )

         TraceLog( ValToPrg( oErr ) )
      END

      Inkey(1)

      RESTORE SCREEN FROM cScreen

      SET COLOR TO
      @ nPresetRow, nPresetCol
      SET CURSOR ON

   RETURN

   PROCEDURE SetFWH()

      LOCAL FWH_Root, FWH_LibFolder, FWH_Flags, GetList := {}

      FWH_Root := FWH_LibFolder := Space(128)

      @ 11, 12 CLEAR TO 13, 66

      @ 11, 12 SAY "  FWH Root:" GET FWH_Root      PICTURE "@S29" ;
                                                   WHEN Find_FWH( @FWH_Root ) ;
                                                   VALID Validate_FWH( FWH_Root, FWH_LibFolder ) ;
                                                   MESSAGE "Location of FWH on your system."


      @ 13, 12 SAY "Lib Folder:" GET FWH_LibFolder PICTURE "@S29" ;
                                                   VALID Validate_FWH( FWH_Root, FWH_LibFolder ) ;
                                                   MESSAGE "Optional location of FWH libraries on your system."

      @ 15, 12 SAY "   C Flags:" GET FWH_Flags     PICTURE "@S30" ;
                                                   MESSAGE "Normally you don't need to specify any."

   RETURN

   FUNCTION ValidateTarget( cTarget, cTargetType )

      LOCAL cTestTarget, hFile, nAt, cExt

      IF '.' IN cTarget
         cTestTarget := RTrim( cTarget )
         nAt := RAt( '.', cTestTarget )
         cExt := Lower( SubStr( cTestTarget, nAt ) )

         IF cExt == ".exe" .OR. cExt == ".lib" .OR. cExt == ".dll" .OR. cExt == ".a"
            cTargetType := SubStr( cExt, 2 )
            KEYBOARD Chr(13)
         ELSE
            Alert( "Ignoring invalid Target extension: '" + cExt + "'" )
            cTarget := Pad( Left( cTarget, nAt - 1 ), 128 )
            cTestTarget := Pad( RTrim( cTarget ) + ".exe", 128 )
         ENDIF
      ELSE
         cTestTarget := RTrim( cTarget ) + IF("LINUX" IN Upper( Os() ) ,"" ,".exe" )
      ENDIF

      IF cTestTarget[1] == '.'
         Alert( "Invalid Target format." )
         RETURN .F.
      ELSEIF File( cTestTarget )
         // Ok.
      ELSE
         hFile := FCreate( cTestTarget )
         IF hFile == -1
            Alert( "Invalid Target Name." )
            RETURN .F.
         ELSE
            FCLose( hFile )
         ENDIF
      ENDIF

   RETURN .T.

   FUNCTION SetTargetExtension( cTargetType, cTarget )

      cTarget := Pad( LTrim( RTrim( cTarget ) ) + "." + cTargetType, 128 )

   RETURN .T.

   FUNCTION ValidateMain( cMain )

      IF ! '.' IN cMain
         cMain := Pad( RTrim( cMain ) + ".prg", 128 )
      ELSE
         IF ! cMain LIKE "[_\a-zA-Z][^.]*\.(c|prg) *"
            Alert( "Main source extension must be '.prg' or '.c'." )
            RETURN .F.
         ENDIF
      ENDIF

      IF ! File( cMain )
         Alert( "Couldn't locate specified Main Source." )
         RETURN .F.
      ENDIF

   RETURN .T.

   FUNCTION ValidateModules( acMasks, nIndex )

      LOCAL cMask := acMasks[ nIndex ]

      // This entry is to avoid the need of user pass /usr/lib/xbuilder when spefify an lib
      #ifndef __PLATFORM__Windows
         LOCAL lLinuxLib
      #endif

      IF Empty( cMask )
         RETURN .T.
      ENDIF


      #ifndef __PLATFORM__Windows
         lLinuxLib := (".so" in cMask) .OR. (".a" in cMask)
      #endif

      IF ! '.' IN cMask
         cMask := Pad( RTrim( cMask ) + ".prg", 128 )
         acMasks[ nIndex ] := cMask
      ELSE
         #ifndef __PLATFORM__Windows
             cMask := Alltrim( cMask )
         #ENDIF

         IF ! cMask LIKE "[_\a-zA-Z][^.]*\.(prg|c|rc|y|sly|obj|res|lib|a|xbp) *"
            Alert( "Module extension must be one of these supported extensions:;; '.prg', '.c', '.rc', '.y', '.sly', '.obj', '.res', '.lib', '.a', '.xbp'." )
            RETURN .F.
         ENDIF
      ENDIF

     #ifdef __PLATFORM__Windows
      IF Len( Directory( cMask) ) == 0
     #else
      IF Len( Directory( IIF( lLinuxLib, "/usr/lib/xbuilder/lib" + cMask , cMask) ) ) == 0
     #endif
         IF '?' IN cMask .OR. '*' IN cMask
            Alert( "Couldn't match any module with: '" + cMask + "'" )
         ELSE
            Alert( "Couldn't locate specified Module: '" + cMask + "'" )
         ENDIF

         RETURN .F.
      ENDIF

   RETURN .T.

   FUNCTION ValidateFolders( cFolders )

       LOCAL aFolders, cFolder

       IF Empty( cFolders )
          RETURN .T.
       ENDIF

       aFolders := HB_aTokens( cFolders, ';' )

       FOR EACH cFolder IN aFolders
          cFolder := LTrim( RTrim( cFolder ) )

          IF Len( Directory( cFolder, 'D' ) ) == 0
             Alert( "Couldn't locate folder: '" + cFolder + "'" )
             RETURN .F.
          ENDIF
       NEXT

   RETURN .T.

   FUNCTION ValidateDefines( cDefines )

       LOCAL aDefines, cDefine

       IF Empty( cDefines )
          RETURN .T.
       ENDIF

       aDefines := HB_aTokens( cDefines, ';' )

       FOR EACH cDefine IN aDefines
          cDefine := LTrim( RTrim( cDefine ) )

          IF ! cDefine LIKE "[_a-zA-Z][_a-zA-Z0-9]* *(=.*)*"
             Alert( "Invalid #define format: '" + cDefine + "'" )
             RETURN .F.
          ENDIF
       NEXT

   RETURN .T.

   FUNCTION PRG_Flags_Needed( cMain )

   RETURN cMain LIKE "[_a-zA-Z][^.]*\.prg *"

#endif

#ifdef __PLATFORM__Windows
  #ifdef GUI
    #pragma BEGINDUMP
       #include "windows.h"
       #include "wincon.h"

       #include "hbapi.h"
       #include "hbfast.h"

       /*
          void GetConsole( void );
          #pragma startup GetConsole
       */

       extern void xBuildWizardGUI( HINSTANCE hInstance, int iCmdShow, PHB_ITEM pProject, PHB_ITEM pProgress, PHB_ITEM pErrorHandler, PHB_ITEM pOnFWH );

       HB_EXTERN_BEGIN

       extern HINSTANCE hb_hInstance;
       extern int hb_iCmdShow;

       HB_EXTERN_END

       /*
       void GetConsole( void )
       {
          if( AllocConsole() )
          {
             HWND hWnd = NULL;
             //char sMessage[256];

             SetConsoleTitle( "xBuild" );

             do
             {
                OutputDebugString( "Find Console!\n" );
                hWnd = FindWindow( "ConsoleWindowClass", "xBuild" );
             }
             while( hWnd == NULL && ( hWnd = FindWindow( "tty", "xBuild" ) ) == NULL );

             if( hWnd )
             {
                OutputDebugString( "Hide Console!\n" );
                ShowWindow( hWnd, SW_HIDE );
             }

             //sprintf( sMessage, "Found: %i\n", hWnd  );
             //OutputDebugString( sMessage );
          }
          else
          {
             MessageBox( 0, "Failed to create Console!", "xBuild", 0 );
             exit( 1 );
          }
       }
       */

       HB_FUNC( XBUILDWIZARDGUI )
       {
          xBuildWizardGUI( hb_hInstance, hb_iCmdShow,
                                         hb_param( 1, HB_IT_ARRAY ),
                                         hb_param( 2, HB_IT_BLOCK ),
                                         hb_param( 3, HB_IT_BLOCK ),
                                         hb_param( 4, HB_IT_BLOCK )
                         );
       }
    #pragma ENDDUMP
  #endif
#endif
