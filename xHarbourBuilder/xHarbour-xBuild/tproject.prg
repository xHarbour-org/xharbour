/*
  (c) copyright xHarbour.com Inc. http://www.xHarbour.com
  Author: Ron Pinkas Ron@xHarbour.com

  This source file is an intellectual property of xHarbour.com Inc.
  You may NOT forward or share this file under any conditions!
*/

#ifndef __XHARBOUR__
   #include "xhb.ch"
#endif

#include "hbclass.ch"

//#define d_Debug
//#define d_Debug_LoadProject

STATIC s_hEnvVars := { => }

STATIC s_sProgramsFolder

STATIC s_sWinSDKBinFolder
STATIC s_sWinSDKIncludeFolders
STATIC s_sWinSDKLibFolder

STATIC s_sHostArch := ""

STATIC s_sUniversalCRT_IncludePath     //$(UniversalCRT_IncludePath)
STATIC s_sUniversalCRT_LibraryPath_x86 //$(UniversalCRT_LibraryPath_x86)

// TODO: $(UniversalCRT_LibraryPath_x64)

STATIC s_sxCCFolder   := ""
STATIC s_sBCCFolder   := ""

STATIC s_sVCFolder    := ""
STATIC s_sVCBinFolder := "bin"
STATIC s_sVCLibFolder := "lib"
STATIC s_sVC_CL       := "cl.exe"
STATIC s_sVC_LIB      := "lib.exe"
STATIC s_sVC_LINK     := "link.exe"

STATIC s_sPOCCFolder  := ""
STATIC s_sLCCFolder   := ""
STATIC s_sGCCFolder   := ""
STATIC s_sCLangFolder := ""
STATIC s_sTCCFolder   := ""

STATIC s_lX            := .F.
STATIC s_sHB_Exe       := ""
STATIC s_sHB_Folder    := ""
STATIC s_sHB_LibFolder := ""
STATIC s_sHB_IncFolder := ""

#ifdef __PLATFORM__Windows
#else
   STATIC s_sLocalFolder       := ""
#endif

STATIC s_sCoreLibs    :=  ""
STATIC s_sCoreMinLibs :=  ""
STATIC s_sCoreDynLibs :=  ""

/*
#xcommand       VIEW  <x> [ <y> ]  =>     sLogDebug( IF(Empty(ProcName( 1 ) ),"",Trim( ProcName( 1 ) ) + "(" + LTrim(Str( ProcLine( 1 ) ) ) + ")->" ) + IF(Empty(ProcName( 0 ) ),"",Trim( ProcName( 0 ) ) + "(" + LTrim(Str( ProcLine( 0 ) ) ) + ")" ) + " : " + <x>   + " -> [" + ValType(<y>) + "] " + ValToPrg(<y>),.T. )
#xcommand       VIEW  <x>          =>     sLogDebug( IF(Empty(ProcName( 1 ) ),"",Trim( ProcName( 1 ) ) + "(" + LTrim(Str( ProcLine( 1 ) ) ) + ")->" ) + IF(Empty(ProcName( 0 ) ),"",Trim( ProcName( 0 ) ) + "(" + LTrim(Str( ProcLine( 0 ) ) ) + ")" ) + " : " + <"x"> + " -> [" + ValType(<x>) + "] " + ValToPrg(<x>),.T. )
#xcommand       VIEW2 <x>          =>     sLogDebug( IF(Empty(ProcName( 1 ) ),"",Trim( ProcName( 1 ) ) + "(" + LTrim(Str( ProcLine( 1 ) ) ) + ")->" ) + IF(Empty(ProcName( 0 ) ),"",Trim( ProcName( 0 ) ) + "(" + LTrim(Str( ProcLine( 0 ) ) ) + ")" ) + " : " + <x>,.T. )
*/

// No Actions.
#define TYPE_SOURCE_LIB -5
#define TYPE_SOURCE_OBJ -4
#define TYPE_SOURCE_RES -3
#define TYPE_RC         -2
#define TYPE_INCLUDE    -1
#define TYPE_NO_ACTION   0

// Actions generating
#define TYPE_FROM_PRG    1
#define TYPE_FROM_C      2
#define TYPE_FROM_SLY    3
#define TYPE_FROM_RC     4

// Targets
#define TYPE_EXE        10
#define TYPE_LIB        11
#define TYPE_DLL        12
#define TYPE_HRB        13

#ifdef __PLATFORM__Windows
  #define DRIVE_SEPARATOR ':'
  #define DIR_SEPARATOR '\'
  #define EOL Chr(13) + Chr(10)
  #define EXACT_CASE .F.
#else
  #define DRIVE_SEPARATOR ''
  #define DIR_SEPARATOR '/'
  #define EOL Chr(10)
  #define EXACT_CASE .T.
#endif

#ifdef GUI
  #define MB_SETFOREGROUND  0x00010000
  #define MB_TOPMOST  0x00040000

  #define SW_SHOW             5

  STATIC FUNCTION Alert( xMessage )

     LOCAL cMessage := ""

     IF ValType( xMessage ) == 'C'
        cMessage := StrTran( xMessage, ";", EOL )
     ELSE
        aEval( xMessage, {|cLine| cMessage += ( cLine + Chr(13) + Chr(10) ) } )
     ENDIF

  RETURN MessageBox( 0, cMessage, "xBuild Wizard", MB_TOPMOST )

  PROCEDURE GUI_OpenLog( oProject )

     TRY
        xBuild_GUI_OnError( NIL, MemoRead( oProject:cFile + ".log" ) )
     CATCH
     END

  RETURN

  PROCEDURE GUI_ViewErrors( oProject )

     TRY
        GUI_ErrorGrid( NIL, MemoRead( oProject:cFile + ".log" ) )
     CATCH
     END

  RETURN

  PROCEDURE GUI_ErrorGrid( oError, cLog )

     LOCAL aaErrors, hWnd, Rect

     TRY
        IF ! Empty( cLog )
           aaErrors := GetLogErrors( cLog )
        ENDIF

        IF Empty( aaErrors )
           xBuild_GUI_OnError( oError, cLog )
        ELSE
           xBuild_GUI_SetError( .T. )

           hWnd := xBuild_GUI_ResultsWindow()

           IF Empty( hWnd )
             PopupEditor( 0, 10, 10, 580, 380, , , cLog )
           ELSE
              GetClientRect( hWnd, @Rect )

              hWnd := xEditListView( hWnd, 0, 0, Rect:right, Rect:bottom, { /*"#",*/ "Source File", "Line", "Type", "Description" }, aaErrors )
              ShowWindow( hWnd, SW_SHOW )
              SetFocus( hWnd )
           ENDIF
        ENDIF
     CATCH oError
        TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine, ValToPrg( oError:Args ) )

        Alert( "Description: " + oError:Description + ";"   +;
               "Operation  : " + oError:Operation + ";"        +;
               "arguments  : " + ValToPrg( oError:Args ) + ";" +;
               oError:ModuleName + "->" + oError:ProcName + "(" + Str( oError:ProcLine, 5 ) + " )" )
     END

  RETURN
#endif


CLASS TDependant //MODULE FRIENDLY //FROM HBPersistent

   //FRIEND FUNCTION xBuildMain

   VAR lCurrent      INIT .F. READONLY
   VAR dDate                  READONLY
   VAR cTime                  READONLY
   VAR aDependancies INIT {}  PROTECTED
   VAR bAction                PROTECTED

   METHOD Reset()             VIRTUAL
   METHOD Refresh()           VIRTUAL

   METHOD CatchUp()
   METHOD CurrentWith( Dependancy ) INLINE /*TraceLog( ::cFile, Dependancy:cFile, ::dDate, Dependancy:dDate, ::cTime, Dependancy:cTime,::dDate > Dependancy:dDate, ::cTime > Dependancy:cTime ),*/ ::dDate > Dependancy:dDate .OR. ( ::dDate == Dependancy:dDate .AND. ::cTime > Dependancy:cTime )
   METHOD DependOn( Dependancy )    INLINE aAdd( ::aDependancies, Dependancy ) // OVERRIDDEN in TMakeObject!!!

ENDCLASS

METHOD CatchUp() CLASS TDependant

   LOCAL Dependancy, cLib, LIB, lCurrent, nAt, cCurrentFolder, cWorkFolder, cFile, oErr, cErrorLog
   LOCAL cDisk

   IF ::bAction == NIL
      ::lCurrent := .T.
   ELSE
      ::lCurrent := ! ::Project:lClean
   ENDIF

   IF ::nType == TYPE_EXE .OR. ::nType == TYPE_DLL
      ::lCurrent := ::lCurrent .AND. ( ! ::lLink )
   ENDIF

   ::Reset()

   FOR EACH Dependancy IN ::aDependancies
       IF Dependancy:ClassName == "TMAKEPROJECT"
          Dependancy:lClean := ::lClean

          IF Dependancy:cFile[2] == DRIVE_SEPARATOR
             cDisk := DiskName()
             DiskChange( Dependancy:cFile[1] )
             Dependancy:cFile := SubStr( Dependancy:cFile, 3 )
          ENDIF

          nAt := RAt( DIR_SEPARATOR, Dependancy:cFile )

          IF nAt > 0
             cCurrentFolder := DIR_SEPARATOR + CurDir()
             cWorkFolder := Left( Dependancy:cFile, nAt )
             cFile := SubStr( Dependancy:cFile, nAt + 1 )

             IF DirChange( cWorkFolder ) != 0
                Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Could not switch to folder: " + cWorkFolder, HB_aParams() ) )
             ENDIF

             Dependancy:cFile := cFile
          ENDIF

          TRY
             ::lCurrent := Dependancy:Make( {|e| Break(e) }, ::bProgress ) .AND. ::lCurrent
          CATCH oErr
             //TraceLog( ValToPrg( oErr ) )
             cErrorLog := MemoRead( Dependancy:cFile + ".log" )
          END

          IF cCurrentFolder == NIL
             IF cDisk != NIL
                Dependancy:cFile := DiskName() + DRIVE_SEPARATOR + cFile
                DiskChange( cDisk )
             ENDIF
          ELSE
             IF DirChange( cCurrentFolder ) != 0
                Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Could not restore to folder: " + cCurrentFolder, HB_aParams() ) )
             ENDIF

             IF Empty( Dependancy:TargetFolder )
                Dependancy:cFile := cWorkFolder + cFile
             ELSE
                IF Dependancy:TargetFolder[1] == DIR_SEPARATOR .OR. Dependancy:TargetFolder[2] == DRIVE_SEPARATOR
                   // Don't restore cWorkFolder because explicit TargetFolder will be used.
                ELSE
                   Dependancy:cFile := cWorkFolder + cFile
                ENDIF
             ENDIF

             IF cDisk != NIL
                IF Dependancy:TargetFolder[1] == DIR_SEPARATOR .OR. Dependancy:TargetFolder[2] == DRIVE_SEPARATOR
                   // Don't restore cDisk because explicit TargetFolder will be used.
                ELSE
                   Dependancy:cFile := DiskName() + DRIVE_SEPARATOR + Dependancy:cFile
                ENDIF

                DiskChange( cDisk )
             ENDIF
          ENDIF

          IF Dependancy:nType == TYPE_DLL
             Dependancy:nType := TYPE_LIB
             Dependancy:cFile[-3] := 'l'
             Dependancy:cFile[-2] := 'i'
             Dependancy:cFile[-1] := 'b'
          ENDIF

          IF oErr != NIL
             cErrorLog := MemoRead( ::cFile + ".log" ) + cErrorLog
             SET( _SET_TRACEFILE, ::cFile + ".log" )
             TraceLog( cErrorLog )
             Throw( oErr )
          ENDIF
       ELSE
          ::lCurrent := Dependancy:CatchUp() .AND. ::lCurrent
       ENDIF

      IF ::lCurrent
         ::lCurrent := ::CurrentWith( Dependancy )
      ENDIF
   NEXT

   IF ( ::nType == TYPE_EXE .OR. ::nType == TYPE_DLL ) .AND. ::lPRG

      #ifdef __PLATFORM__Windows
         IF ::lGUI .AND. ( ! ::lUseDll ) .AND. ( ! ::lDebug ) .AND. ::nType != TYPE_DLL .AND. ::lX
            IF ::lMT
               IF ::C_Executable == "xcc.exe"
                  ::GUI_Libs += "OptGMT.lib "
               ENDIF
            ELSE
               IF ::C_Executable == "xcc.exe"
                  ::GUI_Libs += "OptG.lib "
               ENDIF
            ENDIF
         ENDIF

         IF ::lGUI
            IF ! ( ::C_Executable == "xcc.exe" )
               ::MIN_Libs := StrTran( ::MIN_Libs, "gtwin.lib ", "" )
               ::ST_Libs  := StrTran( ::ST_Libs, "gtwin.lib ", "" )
               ::MT_Libs  := StrTran( ::MT_Libs, "gtwin.lib ", "" )

               ::GUI_Libs += "gtgui.lib "
            ENDIF
         ENDIF
      #endif

      FOR EACH cLib IN HB_aTokens( AllTrim( ::GUI_Libs ), ' ', .T. )
         IF Empty( cLib )
            IF Empty( AllTrim( ::GUI_Libs ) )
               EXIT
            ELSE    
               Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "corrupt GUI_Libs: " + ::GUI_Libs ) )
            ENDIF   
         ENDIF

         //VIEW cLib
         LIB = TMakeObject():New( cLib, TYPE_SOURCE_LIB )
         LIB:Project := Self
         LIB:Reset()
         ::DependOn( LIB )
         ::lCurrent := ::lCurrent .AND. ::CurrentWith( LIB ) // Don't change order!
      NEXT

      FOR EACH cLib IN HB_aTokens( AllTrim( ::Auto_Libs ), ' ', .T. )
         //VIEW cLib
         IF Empty( cLib )
            IF Empty( AllTrim( ::Auto_Libs ) )
               EXIT
            ELSE    
               Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "corrupt Auto_Libs: " + ::Auto_Libs ) )
            ENDIF   
         ENDIF

         LIB = TMakeObject():New( cLib, TYPE_SOURCE_LIB )
         LIB:Project := Self
         LIB:Reset()
         ::DependOn( LIB )
         ::lCurrent := ::lCurrent .AND. ::CurrentWith( LIB ) // Don't change order!
      NEXT

      // Don't optimize with IF ::lCurrent because we MUST add all LIBS to prject for inclusion in Link command.

      FOR EACH cLib IN HB_aTokens( AllTrim( ::PRG_Libs ), ' ', .T. )
         //VIEW cLib
         IF Empty( cLib )
            IF Empty( AllTrim( ::PRG_Libs ) )
               EXIT
            ELSE    
               Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "corrupt PRG_Libs: " + ::PRG_Libs ) )
            ENDIF   
         ENDIF

         LIB = TMakeObject():New( cLib, TYPE_SOURCE_LIB )
         LIB:Project := Self
         LIB:Reset()
         ::DependOn( LIB )
         ::lCurrent := ::lCurrent .AND. ::CurrentWith( LIB ) // Don't change order!
      NEXT

   ENDIF

   lCurrent := ::lCurrent
   IF ! lCurrent
      ::Refresh()
   ENDIF

   #ifdef DEFERRED_RC
      IF ::nType == TYPE_EXE .AND. ::lRC
         FOR EACH Resource IN ::aDeferred
            Resource:Reset()

            // NEW Exe we MUST bind all resources anyway! :-)
            IF ( ! lCurrent ) .OR. ( ! ::CurrentWith( Resource ) )
               TraceLog( ::RC_Executable + " " + Eval( ::RC_Command, Resource ) )

               // Save the current application time stamp into the virtual .res file.
               ::Reset()
               Resource:dDate := ::dDate
               Resource:cTime := ::cTime

               // Bind.
               Eval( Resource:bAction, Resource )

               // Reset the application time stamp.
               ::Reset()

               // Compare new time stamp against the prior time stamp saved into the virtaul .res file.
               IF ! ( ::dDate > Resource:dDate .OR. ( ::dDate == Resource:dDate .AND. ::cTime >= Resource:cTime ) )
                  Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "couldn't bind resource:" + Resource:cFile, HB_aParams() ) )
               ENDIF
            ENDIF
         NEXT
      ENDIF
   #endif

RETURN ::lCurrent

CLASS TMakeObject FROM TDependant //MODULE FRIENDLY

   //FRIEND FUNCTION xBuildMain

   VAR cFile                     READONLY
   VAR nType INIT TYPE_NO_ACTION READONLY
   VAR Project                   READONLY

   VAR MyC_Flags       INIT ""             PERSISTENT
   VAR MyPRG_Flags     INIT ""             PERSISTENT
   VAR MyRC_Flags      INIT ""             PERSISTENT
   VAR MySLY_Flags     INIT ""             PERSISTENT
   VAR MyDefines       INIT ""             PERSISTENT

   VAR C_DebugFlags    INIT ""             READONLY
   VAR Link_DebugFlags INIT ""             READONLY

   #ifdef __PLATFORM__Windows

     #ifdef OLD_PROCESS

          // Having to feed Executable as first token of command line, or argv[0] will be 1st param. (Windows documentation!)
          VAR bC_Compile   INIT {|Self| CreateProcessWait( NIL, ;
                                                           '"' + ::C_Compiler + '" ' + ::C_Command(), ;
                                                           ::Project:cFile + ".log", "PATH=" + ::Project:C_RootX + "..\Common7\Ide" + Chr(0) ) } PROTECTED

          VAR bPRG_Compile INIT {|Self| CreateProcessWait( NIL, ;
                                                           '"' + ::xHB_Compiler + '" ' + ::xHB_Command(), ;
                                                           ::Project:cFile + ".log", "" + Chr(0) ) } PROTECTED

        #ifdef DEFERRED_RC
          VAR bRC_Bind     INIT {|Self| CreateProcessWait( NIL, ;
                                                           '"' + ::RC_Binder + '" ' + Eval( ::RC_Command, Self ), ;
                                                           ::Project:cFile + ".log", "" + Chr(0) ) } PROTECTED
        #else
          VAR bRC_Compile  INIT {|Self| CreateProcessWait( NIL, ;
                                                           '"' + ::RC_Compiler + '" ' + ::RC_Command(), ;
                                                           ::Project:cFile + ".log", "" + Chr(0) ) } PROTECTED
        #endif

          VAR bSLY_Compile INIT {|Self| CreateProcessWait( NIL, ;
                                                           ::Project:SLY_Compiler + ::SLY_Command(), ;
                                                           ::Project:cFile + ".log", ;
                                                           IIF( Empty( GetEnv( "BISON_SIMPLE") ),  ;
                                                                IIF( Empty( GetEnv( "DJGPP" ) ) .AND. File( ::Project:SLY_Root + "share\bison\bison.simple" ), ;
                                                                     "BISON_SIMPLE=" + ::Project:SLY_Root + "share\bison\bison.simple", ;
                                                                     "DJGPP=" + GetEnv( "DJGPP" ) ), ;
                                                                "BISON_SIMPLE=" + GetEnv( "BISON_SIMPLE" ) ) + ;
                                                           Chr(0) ) } PROTECTED
     #else

          // Having to feed Executable as first token of command line, or argv[0] will be 1st param. (Windows documentation!)
          VAR bC_Compile   INIT {|Self| CreateProcessWait( ::C_Compiler, ;
                                                           ::Project:C_Executable + " " + ::C_Command(), ;
                                                           ::Project:cFile + ".log", ;
                                                           HashToEnvVars( ::Project:hC_EnvVars ) ) } PROTECTED

          VAR bPRG_Compile INIT {|Self| CreateProcessWait( ::XHB_Compiler, ;
                                                           ::Project:xHB_Executable + " " + ::xHB_Command(), ;
                                                           ::Project:cFile + ".log", ;
                                                           HashToEnvVars( ::Project:hPRG_EnvVars ) ) } PROTECTED

        #ifdef DEFERRED_RC
          VAR bRC_Bind     INIT {|Self| CreateProcessWait( ::RC_Binder, ;
                                                           ::Project:RC_Executable + " " + Eval( ::RC_Command, Self ), ;
                                                           ::Project:cFile + ".log", Chr(0) ) } PROTECTED
        #else
          VAR bRC_Compile  INIT {|Self| CreateProcessWait( ::RC_Compiler, ;
                                                           ::Project:RC_Executable + " " + ::RC_Command(), ;
                                                           ::Project:cFile + ".log", Chr(0) ) } PROTECTED
        #endif

          VAR bSLY_Compile INIT {|Self| CreateProcessWait( ::SLY_Compiler, ;
                                                           ::Project:SLY_Executable + " " + ::SLY_Command(), ;
                                                           ::Project:cFile + ".log", ;
                                                           HashToEnvVars( ::Project:hSLY_EnvVars ) ) } PROTECTED
     #endif

   #else

     VAR bC_Compile   INIT {|Self| __RUN( ::C_Compiler + " " + ;
                                          ::C_Command() + ;
                                          " >> " + ::Project:cFile + ".log 2>&1" ) } PROTECTED

     VAR bPRG_Compile INIT {|Self| __RUN( ::XHB_Compiler + " " + ;
                                          ::xHB_Command() + ;
                                          " >> " + ::Project:cFile + ".log 2>&1" ) } PROTECTED

   #ifdef DEFERRED_RC
     VAR bRC_Bind     INIT {|Self| __RUN( ::RC_Binder + " " + ;
                                          Eval( ::RC_Command, Self ) + ;
                                          " >> " + ::Project:cFile + ".log 2>&1" ) } PROTECTED
   #else
     VAR bRC_Compile  INIT {|Self| __RUN( ::RC_Compiler + " " + ;
                                          ::RC_Command() + ;
                                          " >> " + ::Project:cFile + ".log 2>&1" ) } PROTECTED
   #endif

     VAR bSLY_Compile INIT {|Self| __RUN( ::SLY_Compiler + " " + ;
                                          ::SLY_Command() + ;
                                          " >> " + ::Project:cFile + ".log 2>&1" ) } PROTECTED
   #endif


   METHOD C_Compiler   INLINE ::Project:C_RootX + ::Project:C_BinFolder + DIR_SEPARATOR + ::Project:C_Executable
      
   METHOD xHB_Compiler INLINE ::Project:xHB_RootX + "bin" + DIR_SEPARATOR + ::Project:xHB_Executable

   #ifdef DEFERRED_RC
      METHOD RC_Binder    INLINE IIF( ::Project:RC_Root == s_sWinSDKBinFolder, s_sWinSDKBinFolder, ::Project:RC_RootX + "bin" ) + DIR_SEPARATOR + ::Project:RC_Executable
   #else
      METHOD RC_Compiler  INLINE IIF( ::Project:RC_Root == s_sWinSDKBinFolder, s_sWinSDKBinFolder, ::Project:RC_RootX + "bin" ) + DIR_SEPARATOR + ::Project:RC_Executable
   #endif

   METHOD SLY_Compiler INLINE ::Project:SLY_Root + "bin" + DIR_SEPARATOR + ::Project:SLY_Executable

   METHOD C_Flags      INLINE IIF( Empty( ::MyC_Flags )  , IIF( Empty( ::Project:MyC_Flags ),   ::Project:C_Flags,   ::Project:MyC_Flags   ), ::MyC_Flags )   + " "
   METHOD PRG_Flags    INLINE IIF( Empty( ::MyPRG_Flags ), IIF( Empty( ::Project:MyPRG_Flags ), ::Project:PRG_Flags, ::Project:MyPRG_Flags ), ::MyPRG_Flags ) + " "
   METHOD RC_Flags     INLINE IIF( Empty( ::MyRC_Flags ) , IIF( Empty( ::Project:MyRC_Flags ),  ::Project:RC_Flags,  ::Project:MyRC_Flags  ), ::MyRC_Flags )  + " "
   METHOD SLY_Flags    INLINE IIF( Empty( ::MySLY_Flags ), IIF( Empty( ::Project:MySLY_Flags ), ::Project:SLY_Flags, ::Project:MySLY_Flags ), ::MySLY_Flags ) + " "
   METHOD Defines      INLINE IIF( Empty( ::MyDefines )  , IIF( Empty( ::Project:MyDefines ),   ::Project:Defines,   ::Project:MyDefines   ), ::MyDefines )   + " "

   METHOD SetDefines( cDefines )

   METHOD SourceFile()

   METHOD C_Command()

   METHOD xHB_Command()

   METHOD RC_Command()

   METHOD SLY_Command()

   METHOD New( cFile, nType ) CONSTRUCTOR
   METHOD OutputFile( lNoQuotes, lFixPrgExtension )
   METHOD Reset()
   METHOD Refresh()

   METHOD DependOn( Dependancy ) INLINE ( IIF( ::ClassName == "TMAKEPROJECT", ::lAddedDependencies := .T.,  ), IIF( Dependancy:nType == TYPE_FROM_C .AND. ;
                                             ! Empty( Dependancy:aDependancies ) .AND. ;
                                             Dependancy:aDependancies[1]:nType == TYPE_FROM_SLY, ;
                                                 aIns( ::aDependancies, 1, Dependancy, .T. ), ;
                                                 aAdd( ::aDependancies, Dependancy ) ) )

ENDCLASS

METHOD New( cFile, nType ) CLASS TMakeObject

   IF nType == NIL
      nType := TYPE_NO_ACTION
   ENDIF

   ::cFile := cFile
   ::nType := nType

RETURN Self

METHOD C_Command() CLASS TMakeObject

   LOCAL cCommand

   cCommand := ::Project:C_OutputFlag + ::OutputFile( .F., .F. ) + " "
   cCommand += ::C_Flags

   IF ::Project:lDebug
      cCommand += ::Project:C_DebugFlags
   ENDIF

   cCommand += ::Defines()
   cCommand += ::Project:Includes( "-I" )

   // Linux only? Assumed same on Windows
   IF ::Project:lX // ( ::Project:C_Executable IN "gcc;clang;tcc" ) .AND. ::Project:xHB_RootX == "/opt/harbour/"
      cCommand += '-I"' + ::Project:xHB_RootX + 'include" '
      cCommand += '-I"' + ::Project:xHB_RootX + 'include/xbuilder" '
   ELSE
      cCommand += '-I"' + ::Project:xHB_RootX + 'include" '
   ENDIF

   IF ::Project:C_Executable == "xcc.exe"
      cCommand += '-I"' + ::Project:C_RootX + 'c_include" '
      cCommand += '-I"' + ::Project:C_RootX + 'c_include\win" '
      cCommand += '-I"' + ::Project:C_RootX + 'c_include\msvc" '

      IF ::Project:lMT
         cCommand += '-MT '
      ENDIF
   ELSEIF ::Project:C_Executable == "bcc32.exe"
      IF ::Project:lMT
         cCommand += '-tWM '
      ENDIF
   ELSEIF ::Project:C_Executable == s_sVC_CL//"cl.exe"
      cCommand += '-I"' + ::Project:C_RootX + 'include" '
      IF Empty( s_sWinSDKIncludeFolders )
         cCommand += '-I"' + ::Project:C_RootX + 'PlatformSdk\Include" '
      ELSE
         cCommand += SplitFoldersWithFlag( s_sWinSDKIncludeFolders, "-I" )
      ENDIF
      IF ! Empty( s_sUniversalCRT_IncludePath )
         cCommand += '-I"' + s_sUniversalCRT_IncludePath + '" '
      ENDIF
      
      cCommand += "-nologo "
   ELSE      
      #IFDEF __PLATFORM__Windows
         cCommand += '-I"' + ::Project:C_RootX + 'include" '
      #ELSE
         //cCommand += '-I"' + ::Project:C_RootX + 'include" '
      #ENDIF      
   ENDIF

   cCommand += ::SourceFile

RETURN cCommand

METHOD xHB_Command() CLASS TMakeObject

   LOCAL cCommand, cFlags, aFlags := {}, cFlag := ""
   LOCAL lInString := .F., cChar

   cCommand := ::Project:PRG_OutputFlag + ::OutputFile( .F., .T. ) + " "

   cFlags := AllTrim( ::PRG_Flags() )

   //aFlags := hb_aTokens( cFlags, ' ', .T. )
   FOR EACH cChar IN cFlags
      IF lInString
         cFlag += cChar

         IF cChar == '"'
           lInString := .F.
         ENDIF

         LOOP
      ENDIF

      SWITCH cChar
         CASE '"'
            lInString := .T.
            EXIT

         CASE ' '
            IF ! Empty( cFlag )
               aAdd( aFlags, cFlag )
               cFlag := ""
            ENDIF
            LOOP

         CASE '/'
            IF ! Empty( cFlag )
               aAdd( aFlags, cFlag )
               cFlag := ""
            ENDIF
            cChar := '-'
            EXIT
     END

     cFlag += cChar
   NEXT
   IF ! Empty( cFlag )
       aAdd( aFlags, cFlag )
   ENDIF

   cFlags := ""
   FOR EACH cFlag IN aFlags
      IF ( Upper( cFlag ) == '-P' .OR. Upper( cFlag ) == '-PT' ) .AND. ( ! Empty( ::Project:PRG_OutputFolder ) )
         cFlag += 'O' + ::Project:PRG_OutputFolder
      ENDIF

      cFlags += cFlag + " "
   NEXT

   cCommand += cFlags

   IF ::Project:lPRG_Debug
      cCommand += "-B "
   ENDIF

   cCommand += ::Defines()
   cCommand += ::Project:Includes( "-I" ) + " "

   //ERVIEW possible diplication in METHOD Reset() which manipulates ::aIncludeGolders
   IF ::Project:lGUI .AND. ! Empty( ::Project:GUI_RootX )
      cCommand += '-I"' + ::Project:GUI_RootX + 'include" '
   ENDIF

   IF ::Project:lFWH .AND. ! Empty( ::Project:FWH_RootX )
      cCommand += '-I"' + ::Project:FWH_RootX + 'include" '
   ENDIF

   #ifdef __PLATFORM__Windows
     cCommand += '-I"' + ::Project:xHB_RootX + 'include" '

      IF ::Project:xHB_Executable == "xhb.exe"
         cCommand += '-I"' + ::Project:xHB_RootX + 'include\w32" '
      ENDIF
   #else
     IF ::Project:lX
        cCommand += '-I"' + ::Project:xHB_RootX + 'include/xbuilder" '
     ELSE
        cCommand += '-I"' + ::Project:xHB_RootX + 'include/harbour" '
        cCommand += '-I"' + ::Project:xHB_RootX + 'contrib/xhb" '
     ENDIF
   #endif

   IF ::Project:lUseDLL .AND. Self == ::Project:aDependancies[1]
      cCommand += "-n2 "
   ENDIF

   cCommand += ::SourceFile

RETURN cCommand

METHOD RC_Command() CLASS TMakeObject

   LOCAL cCommand

   #ifdef DEFERRED_RC
     cCommand := ::Project:RC_OutputFlag + ::Project:cFile + " "
     cCommand += ::RC_Flags
     cCommand += ::Defines()
     cCommand += ::Project:Includes( ::Project:RC_IncludeFlag ) + " "
     cCommand += ::cFile
   #else
     cCommand := ::Project:RC_OutputFlag + ::OutputFile( .F. ) + " "
     cCommand += ::RC_Flags
     cCommand += ::Defines()
     cCommand += ::Project:Includes( ::Project:RC_IncludeFlag ) + " "

     IF ::Project:RC_Executable == "xrc.exe"
        cCommand += '-I"' + ::Project:C_RootX + 'c_include" '
        cCommand += '-I"' + ::Project:C_RootX + 'c_include\win" '
     ELSE
        cCommand += ::Project:RC_IncludeFlag + '"' + ::Project:C_RootX + 'include" '

        IF Empty( s_sWinSDKIncludeFolders )
           cCommand += ::Project:RC_IncludeFlag + '"' + ::Project:C_RootX + 'PlatformSDK\Include" '
        ELSE
           cCommand += SplitFoldersWithFlag( s_sWinSDKIncludeFolders, ::Project:RC_IncludeFlag )
        ENDIF
     ENDIF

     cCommand += ::SourceFile
   #endif

RETURN cCommand

METHOD SLY_Command() CLASS TMakeObject

   LOCAL cCommand

   cCommand := ::Project:SLY_OutputFlag + ::OutputFile( .F. ) + " "
   cCommand += ::SLY_Flags + ::SourceFile

RETURN cCommand

METHOD OutputFile( lNoQuotes, lFixPrgExtension ) CLASS TMakeObject

   LOCAL cOutput

   IF ::nType <= TYPE_NO_ACTION
      cOutput := ::cFile
   ELSE
      SWITCH ::nType
         CASE TYPE_FROM_C
            cOutput := ::Project:C_OutputFolder
            EXIT

         CASE TYPE_FROM_PRG
            cOutput := ::Project:PRG_OutputFolder

            IF lFixPrgExtension .AND. ::Project:xHB_Executable == "xhb.exe"
               cOutput += Left( ::cFile, Len( ::cFile ) - 3 ) + "c"

               IF lNoQuotes
                  RETURN cOutput
               ELSE
                  RETURN '"' + cOutput + '"'
               ENDIF
            ENDIF
            EXIT

         CASE TYPE_FROM_SLY
            cOutput := ::Project:SLY_OutputFolder
            EXIT

         CASE TYPE_FROM_RC
            cOutput := ::Project:RC_OutputFolder
            EXIT

         CASE TYPE_EXE
         CASE TYPE_LIB
         CASE TYPE_DLL
         CASE TYPE_HRB
            cOutput := ::TargetFolder
            EXIT

         DEFAULT
            Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "unsupported type: " + ::cFile, HB_aParams() ) )
      END

      IF ( ! cOutput == "" ) .AND. cOutput[-1] != DIR_SEPARATOR
         cOutput += DIR_SEPARATOR
      ENDIF

      cOutput += ::cFile
   ENDIF

   //? ::cFile, ::nType
   IF lNoQuotes
      RETURN cOutput
   ENDIF

RETURN '"' + cOutput + '"'

METHOD Reset() CLASS TMakeObject

   LOCAL aFileInfo, cIncludeFolder, cLibFolder, cIncludeFile, Include, nAt
   LOCAL oErr
   LOCAL Object, PRG, C

   IF ::nType == TYPE_FROM_PRG
      IF ::Project:xHB_Executable != "xhb.exe"
         /*
            xHB.exe was assumed, but we have a non integrated C compiler!
            We must manipulate the dependancy from:
               prg -> obj
            to:
               prg -> c -> obj
         */
         IF Right( ::cFile, 4 ) == ".obj"
            //TraceLog( ProcName(1), "prg -> c -> obj" )

            OBject := Self
            PRG := Object:aDependancies[1]

            C := TMakeObject():New( StrTran( Object:cFile, ".obj", ".c" ), TYPE_FROM_PRG )

            C:DependOn( PRG )
            C:Project := ::Project
            C:bAction := ::bPRG_Compile

            #if 0
               TraceLog( "!!!",::cFile, ProcName(1), ProcLine(1), ProcName(2), ProcLine(2) )
               IF ProcName(1) == "TMAKEOBJECT:CATCHUP"
                  C:Catchup()
               ELSEIF ProcName(1) == "TMAKEOBJECT:REFRESH"
                  C:Refresh()
               ELSE
                  C:Reset()
               ENDIF
            #endif

            Object:nType := TYPE_FROM_C
            Object:aDependancies[1] := C
            Object:bAction := ::bC_Compile
         ENDIF
      ENDIF
   ENDIF

   #ifdef __PLATFORM__Windows
   #else
      IF ::nType == TYPE_FROM_C
         //IF ( ::Project:C_Executable IN "gcc;clang;tcc")
            ::cFile := StrTran( ::cFile, ".obj", ".o" )
         //ENDIF
      ENDIF
   #endif   

   //VIEW2 "Searching: '" + ::OutputFile( .T., .F. ) + "'"
   aFileInfo := Directory( ::OutputFile( .T., .F. ) )
   //VIEW aFileInfo

   IF ::nType == TYPE_INCLUDE .AND. Len( aFileInfo ) == 0 .AND. ! ( DIR_SEPARATOR IN ::cFile )
      nAt := Len( ::Project:aIncludeFolders )

      aAdd( ::Project:aIncludeFolders, ::Project:xHB_RootX + "include" + DIR_SEPARATOR )

      #ifdef __PLATFORM__Windows
         IF ::Project:xHB_Executable == "xhb.exe"
            aAdd( ::Project:aIncludeFolders, ::Project:xHB_RootX + "include\w32" )
         ENDIF
      #endif

      aAdd( ::Project:aIncludeFolders, ::Project:C_RootX + "include" + DIR_SEPARATOR )

      IF ::Project:lGUI  .AND. ! Empty( ::Project:GUI_RootX )
         aAdd( ::Project:aIncludeFolders, ::Project:GUI_RootX + "include" + DIR_SEPARATOR )
      ENDIF

      IF ::Project:lFWH .AND. ! Empty( ::Project:FWH_RootX )
         aAdd( ::Project:aIncludeFolders, ::Project:FWH_RootX + "include" + DIR_SEPARATOR )
      ENDIF

      FOR EACH cIncludeFolder IN ::Project:aIncludeFolders
         //VIEW cIncludeFolder + ::cFile
         //VIEW HB_EnumIndex()
         //VIEW ::Project:aIncludeFolders

         aFileInfo := Directory( cIncludeFolder + ::cFile )
         IF Len( aFileInfo ) == 1
            IF HB_EnumIndex() == nAt + 2 .OR. cIncludeFolder = ::Project:C_RootX
               ::dDate := aFileInfo[1][3]
               ::cTime := aFileInfo[1][4]
               // Don't scan C compiler #includes.
               nAt := -nAt
               EXIT
            ELSE
               ::cFile := cIncludeFolder + ::cFile
               EXIT
            ENDIF
         ENDIF
      NEXT

      IF nAt < 0
         aSize( ::Project:aIncludeFolders, -nAt )
         RETURN Self
      ENDIF

      // Reset
      aSize( ::Project:aIncludeFolders, nAt )
   ENDIF

   BEGIN SEQUENCE

      IF ::nType == TYPE_SOURCE_LIB .AND. Len( aFileInfo ) == 0 .AND. ! ( DIR_SEPARATOR IN ::cFile )
         FOR EACH cLibFolder IN HB_aTokens( ::Project:LibFoldersX, ';', .T. )
            cLibFolder := AllTrim( cLibFolder )

            IF cLibFolder[ -1 ] != DIR_SEPARATOR
               cLibFolder += DIR_SEPARATOR
            ENDIF

            //TraceLog( "Searching: [" + cLibFolder + ::cFile + "]" )
            aFileInfo := Directory( cLibFolder + ::cFile )

            IF Len( aFileInfo ) == 1
                #ifndef __PLATFORM__Windows
                 ::cFile := SubStr( ::cFile, 4  )
                #else
                 ::cFile := cLibFolder + ::cFile
                #endif
               BREAK
            ENDIF

            #ifndef __PLATFORM__Windows
             /* if not found look for an so lib */
             aFileInfo := Directory( cLibFolder + strtran(::cFile, ".a" , ""  )+ ".so" )
             IF Len( aFileInfo ) == 1
                ::cFile := SubStr( ::cFile, 4  )
                BREAK
             ENDIF
            #endif
         NEXT

         //VIEW ::Project:GUI_LibFolder

         FOR EACH cLibFolder IN HB_aTokens( ::Project:GUI_LibFolderX, ';', .T. )
            cLibFolder := AllTrim( cLibFolder )

            IF cLibFolder[ -1 ] != DIR_SEPARATOR
               cLibFolder += DIR_SEPARATOR
            ENDIF

            //VIEW cLibFolder + ::cFile

            aFileInfo := Directory( cLibFolder + ::cFile )
            IF Len( aFileInfo ) == 1
               //::cFile := cLibFolder + ::cFile
               BREAK
            ENDIF
         NEXT

         FOR EACH cLibFolder IN HB_aTokens( ::Project:FWH_LibFolderX, ';', .T. )
            cLibFolder := AllTrim( cLibFolder )

            IF cLibFolder[ -1 ] != DIR_SEPARATOR
               cLibFolder += DIR_SEPARATOR
            ENDIF

            //VIEW cLibFolder + ::cFile

            aFileInfo := Directory( cLibFolder + ::cFile )
            IF Len( aFileInfo ) == 1
               //::cFile := cLibFolder + ::cFile
               BREAK
            ENDIF
         NEXT

         //TraceLog( ::Project:xHB_LibFolderX + ::cFile )
         aFileInfo := Directory( ::Project:xHB_LibFolderX + ::cFile )

      ENDIF

   END SEQUENCE

   IF Len( aFileInfo ) == 1
      ::dDate := aFileInfo[1][3]
      ::cTime := aFileInfo[1][4]
/*
      VIEW2 "//-------------------------------------------------------------//"
      VIEW "Found"
      VIEW ::OutputFile( .F., .F. )
      VIEW ::cFile
      VIEW ::nType
      VIEW2 "//-------------------------------------------------------------//"
*/
      // No need to scan #includes since we MUST recompile anyway.
      // NOT realy!!!! Because we want to detect FWH, What32, etc...
      IF ::nType <= TYPE_NO_ACTION .AND. ::nType >= TYPE_RC //.AND. ::lCurrent
         cIncludeFile := ::cFile

         IF aScan( ::Project:aIncludedFiles, cIncludeFile, , , EXACT_CASE ) == 0
            aAdd( ::Project:aIncludedFiles, cIncludeFile )

            #ifdef d_Debug
             TraceLog( "Scaning: " + cIncludeFile, ValToPrg( ::Project ) )
            #endif

            FOR EACH cIncludeFile IN FindIncludes( ::cFile )
                //VIEW ::cFile
                //VIEW2 "Found: " + cIncludeFile

                #ifdef __PLATFORM__Windows
                   cIncludeFile := Lower( cIncludeFile ) //Lower() needed?
                #endif

                IF cIncludeFile == "fivewin.ch" .AND. ::Project:lNoAutoFWH == .F.

                   IF ::Project:lFWH == .F.
                      ::Project:lFWH := .T.
                      ::Project:lGUI := .T.

                      IF ( Lower( ::Project:LibList ) HAS "fivehcm{0,1}.lib" )
                         // Explictly specified by user.
                      ELSE
                         IF ::Project:C_Executable == s_sVC_CL/*"cl.exe"*/ .OR. ::Project:C_Executable == "xcc.exe"
                            ::Project:GUI_Libs += "FiveHCM.lib FiveHMX.lib "
                         ELSEIF ::Project:lX
                            ::Project:GUI_Libs += "FiveHC.lib FiveHX.lib "
                         ELSE
                            ::Project:GUI_Libs += "FiveHC.lib FiveH.lib "
                         ENDIF

                         IF ValType( ::Project:OnFWH ) == "B"
                            EVAL( ::Project:OnFWH, Self, Self:Project )
                         ELSEIF ValType( ::Project:OnFWH ) == "N"
                            HB_Exec( ::Project:OnFWH, Self, Self:Project )
                         ENDIF

                         IF Empty( ::Project:FWH_Root )
                            ::Project:FWH_LibFolder := ""

                            IF File( "\fwh\libx\fivehmx.lib" )
                               ::Project:FWH_LibFolder += DiskName() + DRIVE_SEPARATOR + "\fwh\libx;"
                            ELSEIF File( "c:\fwh\libx\fivehmx.lib" )
                               ::Project:FWH_LibFolder += "c:\fwh\libx;"
                            ENDIF

                            IF File( "\fwh\lib\fivehcm.lib" )
                               ::Project:FWH_Root := DiskName() + DRIVE_SEPARATOR + "\fwh\"
                               ::Project:FWH_LibFolder += DiskName() + DRIVE_SEPARATOR + "\fwh\lib;"
                            ELSEIF File( "c:\fwh\lib\fivehcm.lib" )
                               ::Project:FWH_Root := "c:\fwh\"
                               ::Project:FWH_LibFolder += "c:\fwh\lib;"
                            ENDIF

                            IF ! Empty( ::Project:FWH_Root )
                               ::Project:FWH_LibFolder := RTrim( ::Project:FWH_LibFolder )
                            ENDIF

                         ENDIF

                      ENDIF

                   ENDIF

                   IF Empty( ::MyPRG_Flags )
                      ::MyPRG_Flags := ::Project:GUI_PRGFlags
                   ENDIF

                ELSEIF cIncludeFile == "what32.ch"

                   IF ::Project:lW32 == .F.
                      ::Project:lW32 := .T.
                      ::Project:lGUI := .T.

                      IF ( Lower( ::Project:LibList ) HAS "(w32)|(what32).lib" )
                         // Explictly specified by user.
                      ELSE
                         IF ::Project:C_Executable == "xcc.exe"
                            ::Project:GUI_Libs += "W32.lib "
                         ELSE
                            ::Project:GUI_Libs += "What32.lib "
                         ENDIF
                      ENDIF

                   ENDIF

                   IF Empty( ::MyPRG_Flags )
                      ::MyPRG_Flags := ::Project:GUI_PRGFlags
                   ENDIF

                ELSEIF cIncludeFile == "winapi.ch" .AND. ( ! ::Project:lFWH )

                   IF ::Project:lWinAPI == .F.
                      ::Project:lWinAPI := .T.
                      ::Project:lGUI := .T.

                      IF ( Lower( ::Project:LibList ) HAS "(winapi.lib)|(vxh.lib)" ) .OR. ( Lower( ::Project:ObjList ) HAS "(winapi.obj)" )
                         // Explictly specified by user.
                      ELSE
                         ::Project:GUI_Libs += "WinAPI.lib "
                      ENDIF

                   ENDIF

                   IF Empty( ::MyPRG_Flags )
                      ::MyPRG_Flags := ::Project:GUI_PRGFlags
                   ENDIF

                ELSEIF cIncludeFile == "wvtwin.ch"

                   ::Project:lGUI := .T.

                   IF ( Lower( ::Project:LibList ) HAS "(wvg)|(gtwvg).lib" )
                      // Explictly specified by user.
                   ELSE
                      IF ::Project:C_Executable == "xcc.exe"
                         ::Project:GUI_Libs += "wvg.lib "
                      ELSE
                         ::Project:GUI_Libs += "gtwvg.lib "
                      ENDIF
                   ENDIF

                   IF Empty( ::MyPRG_Flags )
                      ::MyPRG_Flags := ::Project:GUI_PRGFlags
                   ENDIF

                ELSEIF cIncludeFile == "apollo.ch"

                   IF ::Project:lSIX == .F.
                      ::Project:lSIX := .T.

                      IF ( Lower( ::Project:LibList ) HAS "six.lib" )
                         // Explictly specified by user.
                      ELSE
                         ::Project:Auto_Libs += "six.lib sde61.lib "
                      ENDIF
                  ENDIF

                ELSEIF cIncludeFile == "ads.ch"

                   IF ::Project:lADS == .F.
                      ::Project:lADS := .T.

                      IF ( Lower( ::Project:LibList ) HAS "ace32.lib" ) .OR. ( Lower(::Project:LibList ) HAS "adsloc.so" )
                         // Explictly specified by user.
                      ELSE
                         IF ::Project:C_Executable == "xcc.exe"
                            ::Project:Auto_Libs += "ads.lib ace32.lib "
                         ELSE
                            #ifdef __PLATFORM__Windows
                               ::Project:Auto_Libs += "rddads.lib ace32.lib "
                            #else
                               ::Project:Auto_Libs += "librddads.a libadsloc.so "
                            #endif
                         ENDIF
                      ENDIF
                   ENDIF

                ELSEIF cIncludeFile == "sqlrdd.ch"

                   IF ::Project:lSQL == .F.
                      ::Project:lSQL := .T.

                      IF ( Lower( ::Project:LibList ) HAS "sql.lib" ) .OR. ( Lower( ::Project:LibList ) HAS "sqlrdd.a" )
                         // Explictly specified by user.
                      ELSE
                         #ifdef __PLATFORM__Windows
                            ::Project:Auto_Libs += "sql.lib "
                         #else
                            ::Project:Auto_Libs += "libsqlrdd.a "
                            IF ::Project:lUseDll
                               ::Project:Auto_Libs += "libodbc.so "
                            ELSE
                               ::Project:Auto_Libs += "libodbc.a "
                            ENDIF
                         #endif
                      ENDIF
                   ENDIF
                /* Native sqlrdd postgres access*/
                ELSEIF cIncludeFile == "pgs.ch"

                   IF ::Project:lPgs == .F.
                      ::Project:lPgs := .T.

                      IF ( Lower( ::Project:LibList ) HAS "libpq.lib" ) .OR. ( Lower( ::Project:LibList ) HAS "pq.so" )
                         // Explictly specified by user.
                      ELSE
                      #ifdef __PLATFORM__Windows
                         ::Project:Auto_Libs += "libpq.lib "
                      #else
                      IF ::Project:lUseDll
                      ::Project:Auto_Libs += "libpq.so "
                      ELSE
                      ::Project:Auto_Libs += "libpq.a libcrypt.a libresolv.a libcrypto.a libnsl.a "
                      ENDIF
                      #endif
                      ENDIF
                   ENDIF

                /* Native sqlrdd mysql access*/
                ELSEIF cIncludeFile == "mysql.ch"

                   IF ::Project:lMySql == .F.
                      ::Project:lMySql := .T.

                      IF ( Lower( ::Project:LibList ) HAS "libmysql.lib" ) .OR. ( Lower( ::Project:LibList ) HAS "mysqlclient.so" )
                         // Explictly specified by user.
                      ELSE
                         #ifdef __PLATFORM__Windows
                            ::Project:Auto_Libs += "libmysql.lib "
                         #else
                            IF ::Project:lUseDll
                               ::Project:Auto_Libs += "libmysqlclient.so "
                            ELSE
                               ::Project:Auto_Libs += "libmysqlclient.a libcrypt.a libz.a libnsl.a "
                            ENDIF
                         #endif
                      ENDIF
                   ENDIF

                ELSEIF cIncludeFile == "oracle.ch"

                   IF ::Project:lOracle == .F.
                      ::Project:lOracle := .T.

                      IF ( Lower( ::Project:LibList ) HAS "oci.lib" ) .OR. ( Lower( ::Project:LibList ) HAS "oci.so" )
                         // Explictly specified by user.
                      ELSE
                         #ifdef __PLATFORM__Windows
                            ::Project:Auto_Libs += "oci.lib "
                         #else
                            IF ::Project:lUseDll
                               ::Project:Auto_Libs += "libosc.so "
                            ELSE
                               ::Project:Auto_Libs += "liboci.a "
                            ENDIF
                         #endif
                      ENDIF
                   ENDIF

                ELSEIF cIncludeFile == "firebird.ch"

                   IF ::Project:lFirebird == .F.
                      ::Project:lFirebird := .T.

                      IF ( Lower( ::Project:LibList ) HAS "fbclient" )
                         // Explictly specified by user.
                      ELSE
                         #ifdef __PLATFORM__Windows
                            IF ::Project:C_Executable == "bcc32.exe"
                               ::Project:Auto_Libs += "fbclient_bc.lib "
                            ELSE
                               ::Project:Auto_Libs += "fbclient_ms.lib "
                            ENDIF
                         #else
                            IF ::Project:lUseDll
                               ::Project:Auto_Libs += "fbclient.so "
                            ELSE
                               ::Project:Auto_Libs += "fbclient.a "
                            ENDIF
                         #endif
                      ENDIF
                   ENDIF

                ELSEIF cIncludeFile == "oleserver.h"

                   IF ::Project:lOle == .F.
                      ::Project:lOle := .T.

                      IF ( "oleserver.lib" IN Lower( ::Project:LibList ) )
                         // Explictly specified by user.
                      ELSE
                         ::Project:Auto_Libs += "oleserver.lib "
                      ENDIF
                   ENDIF

                ELSEIF cIncludeFile == "html.ch"

                   IF ::Project:lHtml == .F.
                      ::Project:lHtml := .T.

                      IF ( Lower( ::Project:LibList ) HAS "html.lib" ) .OR. ( Lower( ::Project:LibList ) HAS "html.a" )
                         // Explictly specified by user.
                      ELSE
                         #ifdef __PLATFORM__Windows
                            ::Project:Auto_Libs += "html.lib "
                         #else
                            ::Project:Auto_Libs += "libhtml.a "
                            ::Project:MyLink_Flags := StrTran( ::Project:MyLink_Flags, "-lgtcrs", "-lgtcgi" )
                         #endif

                      ENDIF
                   ENDIF
                ELSE

                   IF At( DIR_SEPARATOR, cIncludeFile ) == 0
                       nAt := RAt( DIR_SEPARATOR, ::cFile )
                       IF nAt > 0
                          cIncludeFolder := Left( ::cFile, nAt )

                          IF File( cIncludeFolder + cIncludeFile )
                             cIncludeFile := cIncludeFolder + cIncludeFile
                          ENDIF
                       ENDIF
                   ENDIF

                ENDIF

                IF aScan( ::Project:aIncludedFiles, cIncludeFile, , , EXACT_CASE ) == 0
                   Include := TMakeObject():New( cIncludeFile, TYPE_INCLUDE )
                   aAdd( ::Project:aIncludes, Include )

                   Include:Project := ::Project
                   Include:Reset()

                   IF ! ::CurrentWith( Include )
                      ::dDate := Include:dDate
                      ::cTime := Include:cTime
                   ENDIF
                ELSE
                   TRY
                      //VIEW ::cFile
                      //VIEW2 "Searching"
                      //VIEW cInclueFile
                      FOR EACH Include IN ::Project:aIncludes
                         IF Include:cFile == cIncludeFile
                            //VIEW2 "Found"
                            IF ! ::CurrentWith( Include )
                               ::dDate := Include:dDate
                               ::cTime := Include:cTime
                            ENDIF

                            EXIT
                         ENDIF
                      NEXT
                   CATCH oErr
                      //VIEW cIncludeFile
                      Throw( oErr )
                   END
                ENDIF
            NEXT
         ELSE
            //VIEW "Already processed"
            //VIEW ::cFile
         ENDIF
      ENDIF

   ELSE

      ::lCurrent := .F.
/*
      VIEW ::cFile
      VIEW ::nType
      VIEW TYPE_INCLUDE
      VIEW TYPE_FROM_PRG
*/
      #ifdef FULL_LOG
         IF ::nType != TYPE_INCLUDE
            TraceLog( "Missing", ::cFile, ::OutputFile( .F., .F. ), "Type:"  + Str( ::nType, 2 ) )
         ENDIF
      #endif

      IF ::nType == TYPE_INCLUDE .OR. ::nType >= TYPE_FROM_PRG
         ::dDate := ctod( "" )
         ::cTime := "00:00"
      ELSEIF ::nType == TYPE_SOURCE_LIB
         ::dDate := ctod( "" )
         ::cTime := "00:00"

         ::lCurrent := .T.
      ELSE
         Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "couldn't find required file: '" + ::cFile + "'", HB_aParams() ) )
      ENDIF
   ENDIF
/*
   VIEW ::cFile
   VIEW aFileInfo
   VIEW ::dDate
   VIEW ::cTime
   VIEW ::lCurrent
*/
RETURN Self

METHOD Refresh() CLASS TMakeObject

   #ifdef __PLATFORM__Windows
      LOCAL lBuildImpLib := .F.
   #else
      //On Linux Install the Libraries to xBuilder Lib Folder
      LOCAL lExecuteLibInstall := .F.
   #endif

   IF ::Project:bProgress != NIL
      Eval( ::Project:bProgress, Self )
   ENDIF

   SWITCH ::nType
      CASE TYPE_NO_ACTION
         RETURN Self

      CASE TYPE_FROM_C
         TraceLog( ::Project:C_Executable + " " + ::C_Command() )
         EXIT

      CASE TYPE_FROM_PRG
         TraceLog( ::Project:xHB_Executable + " " + ::xHB_Command() )
         EXIT

      CASE TYPE_FROM_SLY
         TraceLog( ::Project:SLY_Compiler + " " + ::SLY_Command() )
         EXIT

      CASE TYPE_FROM_RC
         TraceLog( ::Project:RC_Executable + " " + ::RC_Command() )
         EXIT

      CASE TYPE_EXE
         TraceLog( ::Link_Executable + " " + ::EXE_Command() )
         EXIT

      CASE TYPE_LIB
         TraceLog( ::Lib_Executable + " " + ::LIB_Command() )

         #ifndef __PLATFORM__Windows
            lExecuteLibInstall := ValType( ::bLib_Install ) == 'B'
         #endif

         EXIT

      CASE TYPE_DLL
         TraceLog( ::Link_Executable + " " + ::DLL_Command() )
         IF ::Link_Executable == "xlink.exe" .AND. ( ! ( "-NOIMPLIB" IN ::DLL_Command() ) )
            /* Work aroud for xLink bug when generating import lib for generated dll
               compared to the import lib generated by xLib for same dll.
            lBuildImpLib := .T.
            */
         ENDIF
         EXIT

      CASE TYPE_HRB
         TraceLog( ::Project:xHB_Executable + " " + ::xHB_Command() + " -Gh" )
         EXIT

      //DEFAULT
      //   TraceLog( "Unexpected TYPE!", ::nType )
   END

   IF File( ::OutputFile( .T., .F. ) )
      IF FErase( ::OutputFile( .T., .F. ) ) == -1
         Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "[In use?] couldn't erase: " + ::cFile, HB_aParams() ) )
         RETURN Self
      ENDIF
   ENDIF

   IF ValType( ::bAction ) == 'B'
      Eval( ::bAction, Self )
   ELSE
      TraceLog( ::bAction )   
   ENDIF

   ::lCurrent := .T.

   ::Reset()

   IF ! ::lCurrent
      Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "Couldn't build: " + ::cFile, HB_aParams() ) )
   ENDIF

   #ifdef __PLATFORM__Windows
      IF lBuildImpLib
         TraceLog( ::Lib_Executable + " " + ::IMPLIB_Command() )
         Eval( ::bImpLib_Build, Self )
      ENDIF
   #else
      IF lExecuteLibInstall
         Eval( ::bLib_Install, Self )
      ENDIF
   #endif

RETURN Self

METHOD SetDefines( cDefines ) CLASS TMakeObject

   LOCAL cDefine, aDefines := HB_aTokens( cDefines, ';', .T., .T. )

   IF ::ClassName == "TMAKEPROJECT"
      ::Defines := ""

      FOR EACH cDefine IN aDefines
         ::Defines += "-D" + AllTrim( cDefine ) + " "
      NEXT

      ::MyDefines := ::Defines
   ELSE
      ::MyDefines := ""

      FOR EACH cDefine IN aDefines
         ::MyDefines += "-D" + cDefine + " "
      NEXT
   ENDIF

RETURN Self

METHOD SourceFile() CLASS TMakeObject

   SWITCH ::aDependancies[1]:nType
      CASE TYPE_FROM_PRG
         RETURN '"' + ::Project:PRG_OutputFolder + ::aDependancies[1]:cFile + '"'

      CASE TYPE_FROM_SLY
         RETURN '"' + ::Project:SLY_OutputFolder + ::aDependancies[1]:cFile + '"'

      CASE TYPE_FROM_RC
         RETURN '"' + ::Project:RC_OutputFolder + ::aDependancies[1]:cFile + '"'
   END

RETURN '"' + ::aDependancies[1]:cFile + '"'

CLASS TMakeProject FROM TMakeObject //MODULE FRIENDLY
   
   //FRIEND FUNCTION xBuildMain

   CLASS VAR C_Libs

   VAR LoadFolder      INIT "" READONLY

   VAR C_Root          INIT "" READONLY
   VAR RC_Root         INIT "" READONLY
   VAR xHB_Root        INIT "" READONLY
   VAR xHB_LibFolder   INIT "" READONLY
   VAR xHB_IncFolder   INIT "" READONLY
   VAR SLY_Root        INIT "" READONLY

   VAR C_Executable    READONLY
   VAR xHB_Executable  READONLY
   VAR RC_Executable   READONLY
   
   VAR C_BinFolder     INIT "bin" READONLY
   VAR C_LibFolder     INIT "lib" READONLY   

   #ifdef __PLATFORM__Windows
      VAR SLY_Executable  INIT "bison.exe" READONLY
   #else
      VAR SLY_Executable  INIT "bison" READONLY
   #endif

   VAR hC_EnvVars        INIT Hash()
   VAR hPRG_EnvVars      INIT Hash()
   VAR hRC_EnvVars       INIT Hash()
   VAR hSLY_EnvVars      INIT Hash()

   VAR Lib_Executable  READONLY
   VAR Link_Executable READONLY

   VAR lX                    INIT .T.

   VAR aLoadedProperties
   VAR lAddedDependencies    INIT .F.

   VAR TargetFolder          INIT ""             PERSISTENT

   VAR MapFile INIT ""                           PERSISTENT
   VAR DefFile INIT ""                           PERSISTENT

   VAR lXbp                  INIT .T.
   VAR lIni                  INIT .T.
   VAR lExpand               INIT .F.

   VAR RunArguments          INIT ""             PERSISTENT
   VAR StartIn               INIT ""             PERSISTENT
   VAR lAutoRun                                  PERSISTENT

   VAR bProgress                       READONLY
   //VAR bOnErr                          PROTECTED

   VAR aDeferred             INIT {}   READONLY
   VAR aIncludes             INIT {}   READONLY
   VAR aSources              INIT {}   READONLY
   VAR aLiteralDependancies  INIT {}   READONLY

   VAR OutputFolder          INIT ""             PERSISTENT
   VAR C_OutputFolder                            PERSISTENT
   VAR PRG_OutputFolder                          PERSISTENT
   VAR SLY_OutputFolder                          PERSISTENT
   VAR RC_OutputFolder                           PERSISTENT
   VAR LibFolders            INIT ""             PERSISTENT

   VAR FWH_Root              INIT ""   READONLY
   VAR FWH_LibFolder         INIT ""   READONLY

   VAR GUI_Root              INIT ""   READONLY
   VAR GUI_LibFolder         INIT ""   READONLY
   VAR GUI_PRGFlags          INIT ""   READONLY

   VAR C_OutputFlag          INIT "-o" READONLY
   VAR PRG_OutputFlag        INIT "-o" READONLY
   VAR SLY_OutputFlag        INIT "-o" READONLY

   VAR IncludeFolders        INIT ""   READONLY PERSISTENT
   VAR aIncludeFolders       INIT {}   PROTECTED
   VAR aIncludedFiles                  PROTECTED

   VAR Console_Startup                 PROTECTED
   VAR GUI_Startup                     PROTECTED

   VAR Defines               INIT ""   READONLY
   VAR C_Flags               INIT ""   READONLY
   VAR PRG_Flags             INIT "-m -n -p -q -gc0"   READONLY
   VAR RC_Flags              INIT ""   READONLY

   VAR RC_OutputFlag         INIT ""   READONLY
   VAR RC_IncludeFlag        INIT ""   READONLY
   VAR SLY_Flags        INIT "-v -d"   READONLY

   VAR Lib_Flags                       READONLY
   VAR Lib_OutputFlag                  READONLY
   VAR Lib_AddFlag                     READONLY
   VAR Link_LibFolderFlag              READONLY
   VAR Link_OutputFlag                 READONLY
   VAR DLL_Flags                       READONLY
   VAR DLL_Startup                     READONLY
   VAR Console_Flag                    READONLY
   VAR GUI_Flag                        READONLY

   VAR lClean        INIT .F.

   VAR lLink         INIT .F.

   VAR lDebug            INIT .F.      PERSISTENT
   VAR lMT               INIT .F.      PERSISTENT
   VAR lUseDLL           INIT .F.      PERSISTENT
   VAR lPRG_Debug        INIT .F.      PERSISTENT
   VAR lPRG_ClassicDebug INIT .F.      PERSISTENT

   VAR lMinimal      INIT .F.
   VAR lPRG          INIT .F.          READONLY
   VAR lC            INIT .F.          READONLY

   VAR lFWH          INIT .F.          READONLY
   VAR lNoAutoFWH    INIT .F.          PERSISTENT
   VAR lW32          INIT .F.
   VAR lWinAPI       INIT .F.

   VAR lSIX          INIT .F.
   VAR lADS          INIT .F.

   VAR lSQL          INIT .F.
   VAR lMySql        INIT .F.
   VAR lPgs          INIT .F.
   VAR lOracle       INIT .F.
   VAR lFirebird     INIT .F.

   VAR lOle          INIT .F.
   VAR lHtml         INIT .F.

   VAR lXCL          INIT .F.
   VAR lGUI          INIT .F.          PERSISTENT //PROTECTED
   VAR lSLY          INIT .F.          PROTECTED
   VAR lRC           INIT .F.          PROTECTED

   VAR OnFWH
   VAR OnWhoo
   VAR OnWhat32

   VAR MIN_Libs      INIT "vm.lib rtl.lib macro.lib pp.lib common.lib lang.lib gtwin.lib nulsys.lib debug.lib pcrepos.lib zlib.lib "                                                                                                    READONLY
   VAR ST_Libs       INIT "vm.lib rtl.lib macro.lib pp.lib common.lib lang.lib gtwin.lib rdd.lib dbfntx.lib dbfnsx.lib dbfcdx.lib dbffpt.lib debug.lib pcrepos.lib hsx.lib hbsix.lib ct.lib zlib.lib codepage.lib "                     READONLY
   VAR MT_Libs       INIT "vmmt.lib rtlmt.lib macro.lib ppmt.lib common.lib lang.lib gtwin.lib rddmt.lib dbfnsxmt.lib dbfntxmt.lib dbfcdxmt.lib dbffptmt.lib debug.lib pcrepos.lib hsxmt.lib hbsixmt.lib ct.lib zlib.lib codepage.lib " READONLY

#ifdef __PLATFORM__Windows
   VAR MING_Libs      INIT "-lvm -lrtl -llang -lrdd -lmacro -lpp -lcommon -lcodepage -lgtwin -lnulsys -ldebug"                                       READONLY
   VAR STG_Libs       INIT "-lvm -lrtl -llang -lrdd -lmacro -lpp -lcommon -lcodepage -lgtwin -lrdd -ldbfnsx -ldbfntx -ldbfcdx -ldebug"               READONLY
   VAR MTG_Libs       INIT "-lvmmt -lrtlmt -llang -lrtlmt -lvmmt -lmacromt -lppmt -lcommon -lcodepage -lgtwin -lrddmt -ldbfnsxmt -ldbfntxmt -ldbfcdxmt -ldebug" READONLY
#else
   VAR MING_Libs      INIT "-lvm -lrtl -llang -lrdd -lmacro -lpp -lcommon -lcodepage -lgtcrs -lnulsys -ldebug"                                       READONLY
   VAR STG_Libs       INIT "-lvm -lrtl -llang -lrdd -lmacro -lpp -lcommon -lcodepage -lgtcrs -lrdd -ldbfnsx -ldbfntx -ldbfcdx -ldebug"              READONLY
   VAR MTG_Libs       INIT "-lvmmt -lrtlmt -llang -lrtlmt -lvmmt -lmacromt -lppmt -lcommon -lcodepage -lgtcrs -lrddmt -ldbfnsxmt -ldbfntxmt -ldbfcdxmt -ldebug" READONLY
#endif

   VAR GUI_Libs      INIT ""           READONLY

   VAR Auto_Libs     INIT ""           READONLY

   VAR MyLink_Flags      INIT ""       READONLY
   VAR DefaultLink_Flags INIT ""       READONLY

   VAR cINI                            PERSISTENT

   METHOD C_RootX        INLINE ExpandEnvVars( ::C_Root        )
   METHOD RC_RootX       INLINE ExpandEnvVars( ::RC_Root       )
   METHOD xHB_RootX      INLINE ExpandEnvVars( ::xHB_Root      )
   METHOD xHB_LibFolderX INLINE ExpandEnvVars( ::xHB_LibFolder )
   METHOD xHB_IncFolderX INLINE ExpandEnvVars( ::xHB_IncFolder )
   METHOD FWH_RootX      INLINE ExpandEnvVars( ::FWH_Root      )
   METHOD FWH_LibFolderX INLINE ExpandEnvVars( ::FWH_LibFolder )
   METHOD GUI_RootX      INLINE ExpandEnvVars( ::GUI_Root      )
   METHOD GUI_LibFolderX INLINE ExpandEnvVars( ::GUI_LibFolder )
   METHOD LibFoldersX    INLINE ExpandEnvVars( ::LibFolders    )

   METHOD Link_Flags     INLINE IIF( Empty( ::MyLink_Flags ), ::DefaultLink_Flags, ::MyLink_Flags + " " )

   METHOD LIB_Command()

   METHOD IMPLIB_Command()

   #ifdef __PLATFORM__Windows

      #ifdef OLD_PROCESS

         VAR bExe_Build INIT {|Self| CreateProcessWait( NIL, ;
                                                        ::Linker + " " + ::EXE_Command(), ;
                                                        ::Project:cFile + ".log", "PATH=" + ::C_RootX + "..\Common7\Ide" + Chr(0) ) } PROTECTED

         VAR bLIB_Build INIT {|Self| CreateProcessWait( NIL, ;
                                                        ::Librarian + " " + ::LIB_Command(), ;
                                                        ::Project:cFile + ".log", "PATH=" + ::C_RootX + "..\Common7\Ide" + Chr(0) ) } PROTECTED

         VAR bDLL_Build INIT {|Self| CreateProcessWait( NIL, ;
                                                        ::Linker + " " + ::DLL_Command(), ;
                                                        ::Project:cFile + ".log", "PATH=" + ::C_RootX + "..\Common7\Ide" + Chr(0) ) } PROTECTED

      #else

         VAR bExe_Build INIT {|Self| CreateProcessWait( ::Linker, ;
                                                        ::Link_Executable + " " + ::EXE_Command(), ;
                                                        ::Project:cFile + ".log", ;
                                                        "TMP=" + GetEnv( "TMP" ) + Chr(0) + ;
                                                        "TEMP=" + GetEnv( "TEMP" ) + Chr(0) + ;
                                                        "PATH=" + ::Project:C_RootX + "..\Common7\Ide" + Chr(0) + ;
                                                        "SystemRoot=" + GetEnv( "SystemRoot" ) + Chr(0) ) } PROTECTED

         VAR bLIB_Build INIT {|Self| CreateProcessWait( ::Librarian, ;
                                                        ::Lib_Executable + " " + ::LIB_Command(), ;
                                                        ::Project:cFile + ".log", ;
                                                        "TMP=" + GetEnv( "TMP" ) + Chr(0) + ;
                                                        "TEMP=" + GetEnv( "TEMP" ) + Chr(0) + ;
                                                        "PATH=" + ::Project:C_RootX + "..\Common7\Ide" + Chr(0) + ;
                                                        "SystemRoot=" + GetEnv( "SystemRoot" ) + Chr(0) ) } PROTECTED

         VAR bIMPLIB_Build INIT {|Self| CreateProcessWait( ::Librarian, ;
                                                        ::Lib_Executable + " " + ::IMPLIB_Command(), ;
                                                        ::Project:cFile + ".log", ;
                                                        "TMP=" + GetEnv( "TMP" ) + Chr(0) + ;
                                                        "TEMP=" + GetEnv( "TEMP" ) + Chr(0) + ;
                                                        "PATH=" + ::Project:C_RootX + "..\Common7\Ide" + Chr(0) + ;
                                                        "SystemRoot=" + GetEnv( "SystemRoot" ) + Chr(0) ) } PROTECTED

         VAR bDLL_Build INIT {|Self| CreateProcessWait( ::Linker, ;
                                                        ::Link_Executable + " " + ::DLL_Command(), ;
                                                        ::Project:cFile + ".log", ;
                                                        "TMP=" + GetEnv( "TMP" ) + Chr(0) + ;
                                                        "TEMP=" + GetEnv( "TEMP" ) + Chr(0) + ;
                                                        "PATH=" + ::Project:C_RootX + "..\Common7\Ide" + Chr(0) + ;
                                                        "SystemRoot=" + GetEnv( "SystemRoot" ) + Chr(0) ) } PROTECTED

         VAR bHRB_Build INIT {|Self| CreateProcessWait( ::XHB_Compiler, ;
                                                        ::Project:xHB_Executable + " " + ::xHB_Command() + " -Gh", ;
                                                        ::Project:cFile + ".log", ;
                                                        HashToEnvVars( ::Project:hPRG_EnvVars ) ) } PROTECTED

      #endif

   #else

      VAR bExe_Build INIT {|Self| __RUN( ::Linker + " " + ;
                                         ::EXE_Command() + ;
                                         " >> " + ::Project:cFile + ".log 2>&1" ) } PROTECTED

      VAR bLIB_Build INIT {|Self| __RUN( ::Librarian + " " + ;
                                         ::LIB_Command() + ;
                                         " >> " + ::Project:cFile + ".log 2>&1" ) } PROTECTED

      VAR bDLL_Build INIT {|Self| __RUN( ::Linker + " " + ;
                                         ::DLL_Command() + ;
                                         " >> " + ::Project:cFile + ".log 2>&1" ) } PROTECTED

      VAR bHRB_Build INIT {|Self| __RUN( ::XHB_Compiler + " " + ;
                                          ::xHB_Command() + " -Gh" + ;
                                          " >> " + ::Project:cFile + ".log 2>&1" ) } PROTECTED

      // When sucessfully build a lib on linux, install to xbuilder lib folder
      VAR bLib_Install INIT {|Self| IIF( ::Project:lX, __RUN( "install -m644 " + ::OutputFile( .F. ) + " /usr/local/lib/xbuilder" + " >> " + ::Project:cFile + ".log 2>&1" ), NIL ) }
   #endif

   METHOD SetIncludeFolders( cIncludeFolders )
   #ifdef __PLATFORM__Windows
   METHOD Librarian     INLINE ::C_RootX + ::C_BinFolder + DIR_SEPARATOR + ::Lib_Executable + " "
   METHOD Linker        INLINE ::C_RootX + ::C_BinFolder + DIR_SEPARATOR + ::Link_Executable + " "
   #else
   METHOD Librarian     INLINE ::C_RootX + "bin" + DIR_SEPARATOR + ::Lib_Executable
   METHOD Linker        INLINE ::C_RootX + "bin" + DIR_SEPARATOR + ::Link_Executable
   #endif

   METHOD Includes( cFlag )

   METHOD PRG_Libs()

   METHOD Set_GUI( cRoot, cFlags, cLibFolder ) INLINE cRoot += IIF( cRoot[-1] == DIR_SEPARATOR, "", DIR_SEPARATOR ), ;
                                                      ::GUI_Root := cRoot, ;
                                                      ::GUI_PRGFlags := IIF( Empty( cFlags ), "-n -m ", cFlags + " " ), ;
                                                      ::GUI_LibFolder := IIF( Empty( cLibFolder ), ::GUI_Root + "lib", ;
                                                                              IIF( ( DIR_SEPARATOR IN cLibFolder ), cLibFolder, ::GUI_Root + cLibFolder ) )

   METHOD Set_FWH( cRoot, cLibFolder ) INLINE cRoot += IIF( cRoot[-1] == DIR_SEPARATOR, "", DIR_SEPARATOR ), ;
                                                      ::FWH_Root := cRoot, ;
                                                      ::FWH_LibFolder := IIF( Empty( cLibFolder ), ::FWH_Root + "lib", ;
                                                                              IIF( ( DIR_SEPARATOR IN cLibFolder ), cLibFolder, ::FWH_Root + cLibFolder ) )

   METHOD Set_xHB( cRoot, cFlags, cLibFolder, Exe, cIncFolder ) INLINE  ::xHB_Root       := cRoot + IIF( cRoot[-1] == DIR_SEPARATOR, "", DIR_SEPARATOR ), ;
                                                                        ::xHB_Executable := IIF( Empty( Exe ), s_sHB_Exe, Exe ),;
                                                                        ::PRG_Flags      := IIF( Empty( cFlags ), ::PRG_Flags, cFlags ), ;
                                                                        ::xHB_LibFolder  := IIF( Empty( cLibFolder ), ;
                                                                                                 s_sHB_LibFolder, ;
                                                                                                 IIF( ( DIR_SEPARATOR IN cLibFolder ), ;
                                                                                                      cLibFolder + IIF( cLibFolder[-1] == DIR_SEPARATOR, "", DIR_SEPARATOR ), ;
                                                                                                      ::xHB_Root + cLibFolder + DIR_SEPARATOR ) ), ;
                                                                        ::xHB_IncFolder := IIF( Empty( cIncFolder ), ;
                                                                                                s_sHB_IncFolder, ;
                                                                                                IIF( ( DIR_SEPARATOR IN cIncFolder ), ;
                                                                                                     cIncFolder + IIF( cIncFolder[-1] == DIR_SEPARATOR, "", DIR_SEPARATOR ), ;
                                                                                                     ::xHB_Root + cIncFolder + DIR_SEPARATOR ) ), ;
                                                                        ::lX            := File( ::xHB_IncFolder + "hbfast.h" )


   METHOD Set_BCC( cRoot, C_Flags, LINK_Flags )   INLINE ::C_Root := cRoot + IIF( cRoot[-1] == DIR_SEPARATOR, "", DIR_SEPARATOR ), ;
                                                         ::xHB_Executable := "harbour.exe", ;
                                                         ::C_Executable := "bcc32.exe", ;
                                                         ::Lib_Executable := "tlib.exe", ;
                                                         ::Link_Executable := "ilink32.exe", ;
                                                         ::RC_Root := ::C_Root, ;
                                                         ::RC_Executable := "brc32.exe", ;
                                                         ::RC_Flags := "-32 -r ", ;
                                                         ::RC_OutputFlag := "-fo", ;
                                                         ::RC_IncludeFlag := "-i", ;
                                                         ::Console_Startup := "c0x32.obj ", ;
                                                         ::GUI_Startup := "c0w32.obj ", ;
                                                         ::C_Flags := "-c ", ;
                                                         ::C_DebugFlags := "-v ", ;
                                                         ::MyC_Flags := IIF( Empty( C_Flags ), ::MyC_Flags, C_Flags + " " ), ;
                                                         ::C_OutputFlag := "-o", ;
                                                         ::Link_LibFolderFlag := '-L', ;
                                                         ::Link_OutputFlag := ", ", ;
                                                         ::C_Libs := "cw32.lib import32.lib odbc32.lib ", ;
                                                         ::Lib_Flags := " ", ;
                                                         ::Lib_OutputFlag := " ", ;
                                                         ::Lib_AddFlag := "+", ;
                                                         ::DefaultLINK_Flags := "-Gn -s ", ;
                                                         ::MyLink_Flags := IIF( Empty( Link_Flags ), "", Link_Flags ), ;
                                                         ::LINK_DebugFlags := " ", ;
                                                         ::DLL_Flags := "-Tpd ", ;
                                                         ::DLL_Startup := "c0d32w.obj ", ;
                                                         ::Console_Flag := "-ap ", ;
                                                         ::GUI_Flag := "-aa ", ;
                                                         ::hPRG_EnvVars[ "PATH" ] := ::C_RootX + "bin" // NOT a typo!

   METHOD Set_VC( cRoot, C_Flags, LINK_Flags, cCL, cLIB, cLINK )    INLINE ::C_Root := IIF( cRoot == NIL, s_sVCFolder, cRoot ) + IIF( cRoot[-1] == DIR_SEPARATOR, "", DIR_SEPARATOR ), ;
                                                         ::C_BinFolder := s_sVCBinFolder, ;
                                                         ::C_LibFolder := s_sVCLibFolder, ;
                                                         ::xHB_Executable := "harbour.exe", ;
                                                         ::C_Executable := IIF( cCL == NIL, s_sVC_CL, cCL ), ;
                                                         ::Lib_Executable := IIF( cLIB == NIL, s_sVC_LIB, cLib ), ;
                                                         ::Link_Executable := IIF( cLINK == NIL, s_sVC_LINK, cLink ), ;
                                                         /*::RC_Root := Left( cRoot, RAt( '\', cRoot ) ) + "Common\MSDev98\", */;
                                                         ::RC_Root := IIF( File( s_sWinSDKBinFolder + "\rc.exe" ), ;
                                                                             s_sWinSDKBinFolder /* Do NOT add BACKSLASH! - See :RC_Binfer() / :RC_Compiler() */, ;
                                                                             ::C_Root ), ;
                                                         ::RC_Executable := "rc.exe", ;
                                                         ::RC_Flags := "-x ", ;
                                                         ::RC_OutputFlag := "-fo", ;
                                                         ::RC_IncludeFlag := "-i", ;
                                                         ::Console_Startup := " ", ;
                                                         ::GUI_Startup := " ", ;
                                                         ::C_Flags := "-c -TP -Ox ", ;
                                                         ::C_DebugFlags := "-Zi ", ;
                                                         ::MyC_Flags := IIF( Empty( C_Flags ), ::MyC_Flags, C_Flags + " " ), ;
                                                         ::C_OutputFlag := "-Fo", ;
                                                         ::Link_LibFolderFlag := '-LIBPATH:', ;
                                                         ::Link_OutputFlag := "-OUT:", ;
                                                         ::C_Libs := "user32.lib winspool.lib ole32.lib oleaut32.lib odbc32.lib odbccp32.lib uuid.lib wsock32.lib ws2_32.lib wininet.lib advapi32.lib shlwapi.lib msimg32.lib mpr.lib OleDlg.lib version.lib ", ;
                                                         ::Lib_Flags := " ", ;
                                                         ::Lib_OutputFlag := "-out:", ;
                                                         ::Lib_AddFlag := " ", ;
                                                         ::DefaultLINK_Flags := "-FORCE:MULTIPLE -MAP ", ;
                                                         ::MyLink_Flags := IIF( Empty( Link_Flags ), "", Link_Flags ), ;
                                                         ::LINK_DebugFlags := "-DEBUG -DEBUGTYPE:CV ", ;
                                                         ::DLL_Flags := "-FORCE:MULTIPLE -MAP -DLL ", ;
                                                         ::DLL_Startup := " ", ;
                                                         ::Console_Flag := "-SUBSYSTEM:CONSOLE ", ;
                                                         ::GUI_Flag := "-SUBSYSTEM:WINDOWS ", ;
                                                         ::hC_EnvVars[ "PATH" ] := ::C_RootX + "..\Common7\Ide", ;
                                                         ::hC_EnvVars[ "TMP" ] := GetEnv( "TMP" ), ;
                                                         ::hC_EnvVars[ "SystemRoot" ] := GetEnv( "SystemRoot" )

   METHOD Set_POCC( cRoot, C_Flags, LINK_Flags )  INLINE ::C_Root := cRoot + IIF( cRoot[-1] == DIR_SEPARATOR, "", DIR_SEPARATOR ), ;
                                                         ::xHB_Executable := "harbour.exe", ;
                                                         ::C_Executable := "pocc.exe", ;
                                                         ::Lib_Executable := "polib.exe", ;
                                                         ::Link_Executable := "polink.exe", ;
                                                         ::RC_Root := ::C_Root, ;
                                                         ::RC_Executable := "porc.exe", ;
                                                         ::RC_Flags := " ", ;
                                                         ::RC_OutputFlag := "-fo", ;
                                                         ::RC_IncludeFlag := "-i", ;
                                                         ::Console_Startup := " ", ;
                                                         ::GUI_Startup := " ", ;
                                                         ::C_Flags := "-Ze -Ot ", ;
                                                         ::C_DebugFlags := "-Zi ", ;
                                                         ::MyC_Flags := IIF( Empty( C_Flags ), ::MyC_Flags, C_Flags + " " ), ;
                                                         ::C_OutputFlag := "-Fo", ;
                                                         ::Link_LibFolderFlag := '-LIBPATH:', ;
                                                         ::Link_OutputFlag := "-out:", ;
                                                         ::C_Libs := "crt.lib kernel32.lib user32.lib winspool.lib ole32.lib oleaut32.lib odbc32.lib odbccp32.lib uuid.lib wsock32.lib ws2_32.lib wininet.lib advapi32.lib shlwapi.lib msimg32.lib mpr.lib OleDlg.lib version.lib ", ;
                                                         ::Lib_Flags := " ", ;
                                                         ::DefaultLINK_Flags := "-MAP -FORCE:MULTIPLE ", ;
                                                         ::MyLink_Flags := IIF( Empty( Link_Flags ), "", Link_Flags ), ;
                                                         ::LINK_DebugFlags := "-DEBUG -DEBUGTYPE:CV -FIXED:NO ", ;
                                                         ::Lib_OutputFlag := "-out:", ;
                                                         ::Lib_AddFlag := " ", ;
                                                         ::DLL_Flags := "-DLL -MAP -FORCE:MULTIPLE ", ;
                                                         ::DLL_Startup := " ", ;
                                                         ::Console_Flag := "-subsystem:console ", ;
                                                         ::GUI_Flag := "-subsystem:windows "


   METHOD Set_LCC( cRoot, C_Flags, LINK_Flags )   INLINE ::C_Root := cRoot + IIF( cRoot[-1] == DIR_SEPARATOR, "", DIR_SEPARATOR ), ;
                                                         ::xHB_Executable := "harbour.exe", ;
                                                         ::C_Executable := "lcc.exe", ;
                                                         ::Lib_Executable := "lcclib.exe", ;
                                                         ::Link_Executable := "lcclnk.exe", ;
                                                         ::RC_Root := ::C_Root, ;
                                                         ::RC_Executable := "lrc.exe", ;
                                                         ::RC_Flags := " ", ;
                                                         ::RC_OutputFlag := "/fo", ;
                                                         ::RC_IncludeFlag := "/i", ;
                                                         ::Console_Startup := " ", ;
                                                         ::GUI_Startup := " ", ;
                                                         ::C_Flags := "-c -O ", ;
                                                         ::C_DebugFlags := "-g2 ", ;
                                                         ::MyC_Flags := IIF( Empty( C_Flags ), ::MyC_Flags, C_Flags + " " ), ;
                                                         ::C_OutputFlag := "-Fo", ;
                                                         ::Link_LibFolderFlag := '-L', ;
                                                         ::Link_OutputFlag := "-o ", ;
                                                         ::C_Libs := "libc.lib user32.lib winspool.lib ole32.lib oleaut32.lib odbc32.lib odbccp32.lib uuid.lib wsock32.lib ws2_32.lib wininet.lib advapi32.lib shlwapi.lib msimg32.lib mpr.lib OleDlg.lib version.lib ", ;
                                                         ::Lib_Flags := " ", ;
                                                         ::DefaultLINK_Flags := "-map ", ;
                                                         ::MyLink_Flags := IIF( Empty( Link_Flags ), "", Link_Flags ), ;
                                                         ::LINK_DebugFlags := " ", ;
                                                         ::Lib_OutputFlag := "/out:", ;
                                                         ::Lib_AddFlag := " ", ;
                                                         ::DLL_Flags := "/DLL ", ;
                                                         ::DLL_Startup := " ", ;
                                                         ::Console_Flag := "-subsystem:console ", ;
                                                         ::GUI_Flag := "-subsystem:windows "

   METHOD Set_XCC( cRoot, C_Flags, LINK_Flags )  INLINE  /*TraceLog( ProcLine(), ProcName(1), ProcLine(1), cRoot, C_Flags, LINK_Flags ),*/ ;
                                                         ::C_Root := cRoot + IIF( cRoot[-1] == DIR_SEPARATOR, "", DIR_SEPARATOR ), ;
                                                         ::xHB_Executable := "xhb.exe", ;
                                                         ::C_Executable := "xcc.exe", ;
                                                         ::Lib_Executable := "xlib.exe", ;
                                                         ::Link_Executable := "xlink.exe", ;
                                                         ::RC_Root := ::C_Root, ;
                                                         ::RC_Executable := "xrc.exe", ;
                                                         ::RC_Flags := " ", ;
                                                         ::RC_OutputFlag := "-fo", ;
                                                         ::RC_IncludeFlag := "-i", ;
                                                         ::Console_Startup := " ", ;
                                                         ::GUI_Startup := " ", ;
                                                         ::C_Flags := "-Ot ", ;
                                                         ::C_DebugFlags := "-Zi ", ;
                                                         ::MyC_Flags := IIF( Empty( C_Flags ), ::MyC_Flags, C_Flags + " " ), ;
                                                         ::C_OutputFlag := "-Fo", ;
                                                         ::Link_LibFolderFlag := '-LIBPATH:', ;
                                                         ::Link_OutputFlag := "-out:", ;
                                                         ::C_Libs := "crt.lib kernel32.lib user32.lib winspool.lib ole32.lib oleaut32.lib odbc32.lib odbccp32.lib uuid.lib wsock32.lib ws2_32.lib wininet.lib advapi32.lib shlwapi.lib msimg32.lib mpr.lib OleDlg.lib version.lib ", ;
                                                         ::Lib_Flags := " ", ;
                                                         ::DefaultLINK_Flags := "-NOEXPOBJ -MAP -FORCE:MULTIPLE ", ;
                                                         ::MyLink_Flags := IIF( Empty( Link_Flags ), "", Link_Flags ), ;
                                                         ::LINK_DebugFlags := "-DEBUG -DEBUGTYPE:CV -FIXED:NO ", ;
                                                         ::Lib_OutputFlag := "-out:", ;
                                                         ::Lib_AddFlag := " ", ;
                                                         ::DLL_Flags := "-DLL -MAP -FORCE:MULTIPLE ", ;
                                                         ::DLL_Startup := " ", ;
                                                         ::Console_Flag := "-subsystem:console ", ;
                                                         ::GUI_Flag := "-subsystem:windows "
#ifdef __PLATFORM__Windows
   METHOD Set_GCC( cRoot, C_Flags, LINK_Flags )   INLINE ::C_Root := cRoot + DIR_SEPARATOR, ;
                                                         ::xHB_Executable := "harbour.exe", ;
                                                         ::C_Executable := "gcc.exe", ;
                                                         ::Lib_Executable := "ar.exe", ;
                                                         ::Link_Executable := "ld.exe", ;
                                                         ::RC_Root := ::C_Root, ;
                                                         ::RC_Executable := "???", ;
                                                         ::RC_Flags := " ", ;
                                                         ::RC_OutputFlag := "", ;
                                                         ::RC_IncludeFlag := "", ;
                                                         ::Console_Startup := " ", ;
                                                         ::GUI_Startup := " ", ;
                                                         ::C_Flags :=  "-c ", ;
                                                         ::C_DebugFlags := "??? ", ;
                                                         ::MyC_Flags := IIF( Empty( C_Flags ), ::MyC_Flags, C_Flags + " " ), ;
                                                         ::C_OutputFlag := "-o ", ;
                                                         ::Link_LibFolderFlag := "-L ", ;
                                                         ::Link_OutputFlag := "-o ", ;
                                                         ::C_Libs := " ", ;
                                                         ::Lib_Flags := "a ", ;
                                                         ::Lib_OutputFlag := " ", ;
                                                         ::Lib_AddFlag := " ", ;
                                                         ::DefaultLINK_Flags := " ", ;
                                                         ::MyLink_Flags := IIF( Empty( Link_Flags ), "", Link_Flags ), ;
                                                         ::LINK_DebugFlags := " ", ;
                                                         ::DLL_Flags := " ", ;
                                                         ::DLL_Startup := " ", ;
                                                         ::Console_Flag := " ", ;
                                                         ::GUI_Flag := " "
#else
   METHOD Set_GCC( cRoot, C_Flags, LINK_Flags )   INLINE ::C_Root := cRoot +  IIF( cRoot[-1] == DIR_SEPARATOR, "", DIR_SEPARATOR ), ;
                                                         ::xHB_Executable := "harbour", ;
                                                         ::C_Executable := "gcc", ;
                                                         ::Lib_Executable := "ar", ;
                                                         ::Link_Executable := "gcc", ;
                                                         ::RC_Root := ::C_Root, ;
                                                         ::RC_Executable := "???", ;
                                                         ::RC_Flags := " ", ;
                                                         ::RC_OutputFlag := "", ;
                                                         ::RC_IncludeFlag := "", ;
                                                         ::Console_Startup := " ", ;
                                                         ::GUI_Startup := " ", ;
                                                         ::C_Flags :=  "-c ", ;
                                                         ::C_DebugFlags := "-g ", ;
                                                         ::MyC_Flags := IIF( Empty( C_Flags ), ::MyC_Flags, C_Flags + " " ), ;
                                                         ::C_OutputFlag := "-o ", ;
                                                         ::Link_LibFolderFlag := "-L ", ;
                                                         ::Link_OutputFlag := "-o", ;
                                                         ::C_Libs := " -lpthread -lgpm -lncurses -lm -lslang ", ;
                                                         ::Lib_Flags := "cru ", ;
                                                         ::Lib_OutputFlag := " ", ;
                                                         ::Lib_AddFlag := " ", ;
                                                         ::DefaultLINK_Flags := IIF(::Project:lUseDLL == .F., " --static -lgtcrs", "" ), ;
                                                         ::MyLink_Flags := IIF( Empty( Link_Flags ), "", Link_Flags ), ;
                                                         ::LINK_DebugFlags := " ", ;
                                                         ::DLL_Flags := " ", ;
                                                         ::DLL_Startup := " ", ;
                                                         ::Console_Flag := " ", ;
                                                         ::GUI_Flag := " "
#endif

   METHOD Set_MINGW( cRoot, C_Flags, LINK_Flags ) INLINE ::C_Root := cRoot + DIR_SEPARATOR, ;
                                                         ::xHB_Executable := "harbour.exe", ;
                                                         ::C_Executable := "gcc.exe", ;
                                                         ::Lib_Executable := "ar.exe", ;
                                                         ::Link_Executable := "ld.exe", ;
                                                         ::RC_Root := ::C_Root, ;
                                                         ::RC_Executable := "windres.exe", ;
                                                         ::RC_Flags := " ", ;
                                                         ::RC_OutputFlag := "-o=", ;
                                                         ::RC_IncludeFlag := "--iinclude-dir=", ;
                                                         ::Console_Startup := " ", ;
                                                         ::GUI_Startup := " ", ;
                                                         ::C_Flags := "-c ", ;
                                                         ::C_DebugFlags := "??? ", ;
                                                         ::MyC_Flags := IIF( Empty( C_Flags ), ::MyC_Flags, C_Flags + " " ), ;
                                                         ::C_OutputFlag := "-o ", ;
                                                         ::Link_LibFolderFlag := "-L ", ;
                                                         ::Link_OutputFlag := "-o ", ;
                                                         ::C_Libs := "user32.lib winspool.lib ole32.lib oleaut32.lib ", ;
                                                         ::Lib_Flags := "a ", ;
                                                         ::Lib_OutputFlag := " ", ;
                                                         ::Lib_AddFlag := " ", ;
                                                         ::DefaultLINK_Flags := " ", ;
                                                         ::MyLink_Flags := IIF( Empty( Link_Flags ), "", Link_Flags ), ;
                                                         ::LINK_DebugFlags := " ", ;
                                                         ::DLL_Flags := "--dll ", ;
                                                         ::DLL_Startup := " ", ;
                                                         ::Console_Flag := " ", ;
                                                         ::GUI_Flag := " "

#ifdef __PLATFORM__Windows
   METHOD Set_Clang( cRoot, C_Flags, LINK_Flags )   INLINE ::C_Root := cRoot + DIR_SEPARATOR, ;
                                                         ::xHB_Executable := "harbour.exe", ;
                                                         ::C_Executable := "clang.exe", ;
                                                         ::Lib_Executable := "ar.exe", ;
                                                         ::Link_Executable := "clang.exe", ;
                                                         ::RC_Root := ::C_Root, ;
                                                         ::RC_Executable := "???", ;
                                                         ::RC_Flags := " ", ;
                                                         ::RC_OutputFlag := "", ;
                                                         ::RC_IncludeFlag := "", ;
                                                         ::Console_Startup := " ", ;
                                                         ::GUI_Startup := " ", ;
                                                         ::C_Flags :=  "-c ", ;
                                                         ::C_DebugFlags := "??? ", ;
                                                         ::MyC_Flags := IIF( Empty( C_Flags ), ::MyC_Flags, C_Flags + " " ), ;
                                                         ::C_OutputFlag := "-o ", ;
                                                         ::Link_LibFolderFlag := "-L ", ;
                                                         ::Link_OutputFlag := "-o ", ;
                                                         ::C_Libs := " ", ;
                                                         ::Lib_Flags := "a ", ;
                                                         ::Lib_OutputFlag := " ", ;
                                                         ::Lib_AddFlag := " ", ;
                                                         ::DefaultLINK_Flags := " ", ;
                                                         ::MyLink_Flags := IIF( Empty( Link_Flags ), "", Link_Flags ), ;
                                                         ::LINK_DebugFlags := " ", ;
                                                         ::DLL_Flags := " ", ;
                                                         ::DLL_Startup := " ", ;
                                                         ::Console_Flag := " ", ;
                                                         ::GUI_Flag := " "
#else
   METHOD Set_Clang( cRoot, C_Flags, LINK_Flags )   INLINE ::C_Root := cRoot +  IIF( cRoot[-1] == DIR_SEPARATOR, "", DIR_SEPARATOR ), ;
                                                         ::xHB_Executable := "harbour", ;
                                                         ::C_Executable := "clang", ;
                                                         ::Lib_Executable := "ar", ;
                                                         ::Link_Executable := "clang", ;
                                                         ::RC_Root := ::C_Root, ;
                                                         ::RC_Executable := "???", ;
                                                         ::RC_Flags := " ", ;
                                                         ::RC_OutputFlag := "", ;
                                                         ::RC_IncludeFlag := "", ;
                                                         ::Console_Startup := " ", ;
                                                         ::GUI_Startup := " ", ;
                                                         ::C_Flags :=  "-c ", ;
                                                         ::C_DebugFlags := "-g ", ;
                                                         ::MyC_Flags := IIF( Empty( C_Flags ), ::MyC_Flags, C_Flags + " " ), ;
                                                         ::C_OutputFlag := "-o ", ;
                                                         ::Link_LibFolderFlag := "-L ", ;
                                                         ::Link_OutputFlag := "-o", ;
                                                         ::C_Libs := " -lpthread -lgpm -lncurses -lm -lslang ", ;
                                                         ::Lib_Flags := "cru ", ;
                                                         ::Lib_OutputFlag := " ", ;
                                                         ::Lib_AddFlag := " ", ;
                                                         ::DefaultLINK_Flags := IIF(::Project:lUseDLL == .F., " -fullstatic -lgtcrs", "" ), ;
                                                         ::MyLink_Flags := IIF( Empty( Link_Flags ), "", Link_Flags ), ;
                                                         ::LINK_DebugFlags := " ", ;
                                                         ::DLL_Flags := " ", ;
                                                         ::DLL_Startup := " ", ;
                                                         ::Console_Flag := " ", ;
                                                         ::GUI_Flag := " "
#endif

// Never tested!!!
#ifdef __PLATFORM__Windows
   METHOD Set_TCC( cRoot, C_Flags, LINK_Flags )   INLINE ::C_Root := cRoot + DIR_SEPARATOR, ;
                                                         ::xHB_Executable := "harbour.exe", ;
                                                         ::C_Executable := "gcc.exe", ;
                                                         ::Lib_Executable := "ar.exe", ;
                                                         ::Link_Executable := "ld.exe", ;
                                                         ::RC_Root := ::C_Root, ;
                                                         ::RC_Executable := "???", ;
                                                         ::RC_Flags := " ", ;
                                                         ::RC_OutputFlag := "", ;
                                                         ::RC_IncludeFlag := "", ;
                                                         ::Console_Startup := " ", ;
                                                         ::GUI_Startup := " ", ;
                                                         ::C_Flags :=  "-c ", ;
                                                         ::C_DebugFlags := "??? ", ;
                                                         ::MyC_Flags := IIF( Empty( C_Flags ), ::MyC_Flags, C_Flags + " " ), ;
                                                         ::C_OutputFlag := "-o ", ;
                                                         ::Link_LibFolderFlag := "-L ", ;
                                                         ::Link_OutputFlag := "-o ", ;
                                                         ::C_Libs := " ", ;
                                                         ::Lib_Flags := "a ", ;
                                                         ::Lib_OutputFlag := " ", ;
                                                         ::Lib_AddFlag := " ", ;
                                                         ::DefaultLINK_Flags := " ", ;
                                                         ::MyLink_Flags := IIF( Empty( Link_Flags ), "", Link_Flags ), ;
                                                         ::LINK_DebugFlags := " ", ;
                                                         ::DLL_Flags := " ", ;
                                                         ::DLL_Startup := " ", ;
                                                         ::Console_Flag := " ", ;
                                                         ::GUI_Flag := " "
#else
   METHOD Set_TCC( cRoot, C_Flags, LINK_Flags )   INLINE ::C_Root := cRoot +  IIF( cRoot[-1] == DIR_SEPARATOR, "", DIR_SEPARATOR ), ;
                                                         ::xHB_Executable := "harbour", ;
                                                         ::C_Executable := "tcc", ;
                                                         ::Lib_Executable := "ar", ;
                                                         ::Link_Executable := "tcc", ;
                                                         ::RC_Root := ::C_Root, ;
                                                         ::RC_Executable := "???", ;
                                                         ::RC_Flags := " ", ;
                                                         ::RC_OutputFlag := "", ;
                                                         ::RC_IncludeFlag := "", ;
                                                         ::Console_Startup := " ", ;
                                                         ::GUI_Startup := " ", ;
                                                         ::C_Flags :=  "-c ", ;
                                                         ::C_DebugFlags := "-g ", ;
                                                         ::MyC_Flags := IIF( Empty( C_Flags ), ::MyC_Flags, C_Flags + " " ), ;
                                                         ::C_OutputFlag := "-o ", ;
                                                         ::Link_LibFolderFlag := "-L ", ;
                                                         ::Link_OutputFlag := "-o", ;
                                                         ::C_Libs := " -lpthread -lgpm -lncurses -lm -lslang ", ;
                                                         ::Lib_Flags := "cru ", ;
                                                         ::Lib_OutputFlag := " ", ;
                                                         ::Lib_AddFlag := " ", ;
                                                         ::DefaultLINK_Flags := IIF(::Project:lUseDLL == .F., " -fullstatic -lgtcrs", "" ), ;
                                                         ::MyLink_Flags := IIF( Empty( Link_Flags ), "", Link_Flags ), ;
                                                         ::LINK_DebugFlags := " ", ;
                                                         ::DLL_Flags := " ", ;
                                                         ::DLL_Startup := " ", ;
                                                         ::Console_Flag := " ", ;
                                                         ::GUI_Flag := " "
#endif


   METHOD EXE_Command()

   METHOD DLL_Command()

   METHOD New( cFile ) CONSTRUCTOR
   METHOD AddFiles() 
   METHOD AddObjects() 
   METHOD AddProject( cFile )
   METHOD Make( bOnErr, bProgress )

   METHOD PrgSources()

   METHOD ObjList()
   METHOD LibList()

   #ifndef RC_DEFERRED
      METHOD ResList()
   #endif

   METHOD ValidateSettings()

   METHOD XBPFullPath() INLINE ::LoadFolder + IIF( ::LoadFolder[-1] == DIR_SEPARATOR, "", DIR_SEPARATOR ) + ::cFile + ".xbp"

ENDCLASS

METHOD New( cFile ) CLASS TMakeProject

   LOCAL nAt := RAt( '.', cFile )
   LOCAL cExt

   #ifdef __PLATFORM__Windows
//    cFile := Lower( cFile ) //Lower Needed?
//    cExt  := SubStr( cFile, nAt + 1 )
      cExt  := Lower(SubStr( cFile, nAt + 1 ))
   #else
      cExt  := Lower( SubStr( cFile, nAt + 1 ) )

      if empty(cExt) .or. nAt == 0
         cExt:= "exe"
      endif
   #endif

   DO CASE
      CASE cExt == "exe"
         ::nType := TYPE_EXE
         ::bAction := ::bEXE_Build

      CASE cExt == "lib"
         ::nType := TYPE_LIB
         ::bAction := ::bLIB_Build

      CASE cExt == "a"
         ::nType := TYPE_LIB
         ::bAction := ::bLIB_Build

      CASE cExt == "dll"
         ::nType := TYPE_DLL
         ::bAction := ::bDLL_Build

      CASE cExt == "hrb"
         ::nType := TYPE_HRB
         ::bAction := ::bHrb_Build

      OTHERWISE
         Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "unsupported extension:" + cFile, HB_aParams() ) )
   ENDCASE

   nAt := RAt( DIR_SEPARATOR, cFile )
   IF nAt > 0
      ::TargetFolder := Left( cFile, nAt - 1 )
      ::cFile := SubStr( cFile, nAt + 1 )
   ELSE
      ::cFile := cFile
   ENDIF

   ::Project := Self

   ::LoadFolder := DiskName() + DRIVE_SEPARATOR + DIR_SEPARATOR + CurDir()

   LoadIni( Self )

RETURN Self

METHOD PRG_Libs() CLASS TMakeProject

   IF ::lPRG
      IF ::Project:xHB_Executable == "xhb.exe" .OR. ::Project:xHB_Executable == "xhb"
         IF ::lMT
            IF ::Project:nType == TYPE_EXE
               IF ::Project:lUseDLL
                  IF ::Project:lDebug
                     RETURN IIF( ( "gcc" IN ::C_Executable ), "", "xhbdmtdll.lib rmdbfcdx.lib ct3comm.lib " )
                  ELSE
                     RETURN IIF( ( "gcc" IN ::C_Executable ), "", "xhbmtdll.lib rmdbfcdx.lib ct3comm.lib " )
                  ENDIF
               ELSE
                  RETURN IIF( ( "gcc" IN ::C_Executable ), "libxhbmt.a libdbfmt.a libnsxmt.a libntxmt.a libcdxmt.a ", "xhbmt.lib dbfmt.lib nsxmt.lib ntxmt.lib cdxmt.lib rmdbfcdx.lib ct3comm.lib ")
               ENDIF
            ELSEIF ::Project:nType == TYPE_DLL
               IF ::Project:lDebug
                  RETURN IIF( ( "gcc" IN ::C_Executable ), "", "xhbdmtdll.lib rmdbfcdx.lib ct3comm.lib " )
               ELSE
                  RETURN IIF( ( "gcc" IN ::C_Executable ), "", "xhbmtdll.lib rmdbfcdx.lib ct3comm.lib " )
               ENDIF
            ELSE
               Alert( "Un-Expected case, ", ProcName() )
            ENDIF
         ELSEIF ::lMinimal
            RETURN IIF( ( "gcc" IN ::C_Executable ), "libxh.a libnordd.a", "xhb.lib nordd.lib ")
         ELSE
            IF ::Project:nType == TYPE_EXE
               IF ::Project:lUseDLL
                  IF ::Project:lDebug
                     RETURN IIF( ( "gcc" IN ::C_Executable ), "", "xhbddll.lib rmdbfcdx.lib ct3comm.lib " )
                  ELSE
                     RETURN IIF( ( "gcc" IN ::C_Executable ), "", "xhbdll.lib rmdbfcdx.lib ct3comm.lib " )
                  ENDIF
               ELSE
                  RETURN IIF( ( "gcc" IN ::C_Executable ), "libxhb.a libdbf.a libntx.a libnsx.a libcdx.a ", "xhb.lib dbf.lib nsx.lib ntx.lib cdx.lib rmdbfcdx.lib ct3comm.lib ")
               ENDIF
            ELSEIF ::Project:nType == TYPE_DLL
               IF ::Project:lDebug
                  RETURN IIF( ( "gcc" IN ::C_Executable ), "", "xhbddll.lib rmdbfcdx.lib ct3comm.lib " )
               ELSE
                  RETURN IIF( ( "gcc" IN ::C_Executable ), "", "xhbdll.lib rmdbfcdx.lib ct3comm.lib " )
               ENDIF
            ELSE
               Alert( "Un-Expected case, ", ProcName() )
            ENDIF
         ENDIF
      ELSE
         IF ::lMT
            IF ::Project:nType == TYPE_EXE
               IF ::Project:lUseDLL
                  #ifdef __PLATFORM__Windows                  
                     RETURN IIF( ::lX, /* use_dll.lib see EXE_Command */ "xharbour.lib ", "xharbour.lib " )
                  #else                  
                     // REVIEW! xharbour.so ?
                     RETURN IIF( ::lX, /* use_dll.lib see EXE_Command */ "libxharbour.a ", "libharbour.dylib " )
                  #endif   
               ELSE
                  // "gcc;clang" implied!
                  RETURN IIF( ( ::C_Executable IN "gcc.exe;clang.exe" ), ::MTG_Libs, ::MT_Libs )
               ENDIF
            ELSEIF ::Project:nType == TYPE_DLL
               #ifdef __PLATFORM__Windows                  
                  RETURN IIF( ::lX, "xharbour.lib ", "xharbour.lib " )
               #else                  
                  // REVIEW! xharbour.so ?
                  RETURN IIF( ::lX, "libxharbour.a ", "libharbour.dylib " )
               #endif   
            ELSE
               Alert( "Un-Expected case, ", ProcName() )
            ENDIF
         ELSEIF ::lMinimal
            // "gcc;clang" implied!                    
            RETURN IIF( ( ::C_Executable IN "gcc.exe;clang.exe" ), ::MING_Libs, ::MIN_Libs )
         ELSE
            IF ::Project:nType == TYPE_EXE
               IF ::Project:lUseDLL
                  #ifdef __PLATFORM__Windows                  
                     RETURN IIF( ::lX, /* use_dll.lib see EXE_Command */ "xharbour.lib ", "harbour.lib " )
                  #else                  
                     // REVIEW! xharbour.so ?
                     RETURN IIF( ::lX, /* use_dll.lib see EXE_Command */ "libxharbour.a ", "libharbour.dylib " )
                  #endif   
               ELSE
                  // "gcc;clang" implied!
                  RETURN IIF( ( ::C_Executable IN "gcc.exe;clang.exe" ),::STG_Libs,::ST_Libs )
               ENDIF
            ELSEIF ::Project:nType == TYPE_DLL
               #ifdef __PLATFORM__Windows                  
                  RETURN IIF( ::lX, "xharbour.lib ", "harbour.lib " )
               #else                  
                  // REVIEW! xharbour.so ?
                  RETURN IIF( ::lX, "libxharbour.a ", "libharbour.dylib " )
               #endif   
            ELSE
               Alert( "Un-Expected case, ", ProcName() )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

RETURN " "

METHOD Lib_Command() CLASS TMakeProject

   LOCAL cCommand

   // "gcc;clang" implied!
   IF ( ::C_Executable IN "gcc.exe;clang.exe" )
      cCommand := ::LIB_Flags
      cCommand += ::Lib_OutputFlag + ::OutputFile( .F. ) + " "
      cCommand +=  StrTran( Self:ObjList(), ",", " "  )
   ELSE
      cCommand := ::LIB_Flags
      cCommand += ::Lib_OutputFlag + ::OutputFile( .F. ) + " "
      cCommand += ::Lib_AddFlag + StrTran( Self:ObjList(), ",", " " + ::Lib_AddFlag )
   ENDIF

RETURN cCommand

METHOD ImpLib_Command() CLASS TMakeProject

   LOCAL cCommand, cOutputDll := ::OutputFile( .T. ), cOutputBase := Left( cOutputDll, Len( cOutputDll ) - 4 )

   cCommand := "-OUT:" + '"' + cOutputBase + ".lib" + '"' + " " + ::OutputFile( .F. )

RETURN cCommand

METHOD AddFiles( ... ) CLASS TMakeProject

   LOCAL cMask, aFiles, aFile, cFolder, cFile, cBaseFile, nAt
   LOCAL cExt, bAction, Object, nType, SLY, Dependant

   FOR EACH cMask IN HB_aParams()
      IF aScan( ::aLiteralDependancies, cMask, , , EXACT_CASE ) > 0
         LOOP
      ENDIF

      #ifndef __PLATFORM__Windows
         /* IMHO instead of striping space (why? if they could be
            valid filename part) we should change "\" to DIR_SEPARATOR
            to directly use MS-Windows projects on other platforms.
            In xHarbour internals the "\" is hacked to be translated to
            DIR_SEPARATOR and we cannot use any filenames with "\" so
            we do not lose anything, Przemek.
         */
         //cMask := AllTrim( cMask )
         cMask := strtran( cMask, "\", DIR_SEPARATOR )
      #endif

      aFiles := Directory( cMask )

      IF Len( aFiles ) == 0
         IF ( Lower( Right( cMask, 4 ) ) == ".lib" .AND. ! ( "[?*]" IN cMask ) ) .OR. ;
            ( Lower( Right( cMask, 2 ) ) ==   ".a" .AND. ! ( "[?*]" IN cMask ) ) .OR. ;
            ( Lower( left( cMask, 2 ) )  ==   "-l" .AND. ! ( "[?*]" IN cMask ) ) .OR. ;
            ( Lower( Right( cMask, 3 ) ) ==  ".so" .AND. ! ( "[?*]" IN cMask ) )
            Object := TMakeObject():New( cMask, TYPE_SOURCE_LIB )
            Object:Project := Self
            ::DependOn( Object )
            aAdd( ::aLiteralDependancies, cMask )
            LOOP
         ENDIF

         Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "File not found: " + cMask, HB_aParams() ) )
      ENDIF

      aAdd( ::aLiteralDependancies, cMask )

      nAt := RAt( DIR_SEPARATOR, cMask )
      IF nAt > 0
         cFolder := Left( cMask, nAt )
      ELSE
         cFolder := ""
      ENDIF

      FOR EACH aFile IN aFiles
         cFile := aFile[1]

         IF aScan( ::aSources, cFolder + cFile, , , EXACT_CASE ) > 0
            LOOP
         ENDIF

         nAt   := RAt( '.', cFile )
         cExt  := Lower( SubStr( cFile, nAt + 1 ) )

         DO CASE
            CASE cExt == "c"
               ::lC = .T.
               nType := TYPE_FROM_C
               bAction := ::bC_Compile

            CASE cExt == "cpp"
               ::lC = .T.
               nType := TYPE_FROM_C
               bAction := ::bC_Compile

            CASE cExt == "prg"
               ::lPRG = .T.
               IF ::Project:nType == TYPE_HRB
                  Dependant := TMakeObject():New( cFolder + cFile )
                  Dependant:Project := Self
                  Dependant:nType := TYPE_NO_ACTION
                  ::DependOn( Dependant )
                  LOOP
               ELSE
                  nType := TYPE_FROM_PRG
               ENDIF

            CASE cExt == "rc"
               ::lRC := .T.
               #ifdef DEFERRED_RC
                  Dependant := TMakeObject():New( cFolder + cFile, TYPE_RC )
                  Dependant:bAction := ::bRC_Bind
                  Dependant:Project := Self

                  aAdd( ::aDeferred, Dependant )
                  LOOP
               #else
                  nType := TYPE_FROM_RC
                  bAction := ::bRC_Compile
               #endif

            CASE cExt == "sly"
               ::lSLY := .T.
               nType := TYPE_FROM_SLY

            CASE cExt == "y"
               ::lSLY := .T.
               nType := TYPE_FROM_SLY

            CASE cExt == "xbp"
               Dependant := ::AddProject( cFolder + cFile )

               // By default the folder will be moved into cTargetFolder, which will be overiden with correct Target by the project file. We want to retain the path.
               Dependant:cFile := cFolder + Dependant:cFile
               Object := Dependant
               LOOP

            OTHERWISE
               //Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "unsupported extension:" + cExt, HB_aParams() ) )
               ::AddObjects( cFolder + cFile )
               LOOP
         ENDCASE

         //TraceLog( cFile )

         cBaseFile := Left( cFile, nAt - 1 )

         IF nType == TYPE_FROM_PRG
            //?  cFile + "->" + ::xHB_Executable + ", " + ::C_Executable

            Dependant := TMakeObject():New( cFolder + cFile )
            Dependant:Project := Self

            IF aScan( ::aSources, Dependant:cFile, , , EXACT_CASE ) > 0 .OR. aScan( ::aSources, cFolder + cBaseFile + ".prg", , , EXACT_CASE ) > 0
               // Duplicate (Raise Error ???)
               LOOP
            ENDIF
            aAdd( ::aSources, Dependant:cFile )

            bAction := ::bPRG_Compile
         ELSEIF nType == TYPE_FROM_SLY
            SLY := TMakeObject():New( cFolder + cFile )
            SLY:Project := Self

            aAdd( ::aSources, SLY:cFile )

            cBaseFile += "y"

            Dependant := TMakeObject():New( cBaseFile + ".c", TYPE_FROM_SLY )
            Dependant:DependOn( SLY )
            Dependant:bAction := ::bSLY_Compile
            Dependant:Project := Self

            IF cFile == "macro.y"
              Dependant:MySLY_Flags := "-v -d -p hb_comp"
            ENDIF

            bAction := ::bC_Compile
            nType := TYPE_FROM_C
         ELSEIF nType == TYPE_FROM_RC
            Dependant := TMakeObject():New( cFolder + cFile )
            Dependant:Project := Self

            //aAdd( ::aSources, Dependant:cFile )
         ELSEIF nType == TYPE_FROM_C
            Dependant := TMakeObject():New( cFolder + cFile )
            Dependant:Project := Self

            IF aScan( ::aSources, Dependant:cFile, , , EXACT_CASE ) > 0 .OR. aScan( ::aSources, cFolder + cBaseFile + ".prg", , , EXACT_CASE ) > 0
               // Duplicate (Raise Error ???)
               LOOP
            ENDIF
            aAdd( ::aSources, Dependant:cFile )
         ELSE
            TraceLog( "***Unexpected Type ***", cFile )
            Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "unexpected type:" + cFile, HB_aParams() ) )
         ENDIF

         IF nType == TYPE_FROM_C
            // May later be converted in :Reset() to ".o" - we may not know which C compiler yet!
            Object := TMakeObject():New( cBaseFile + ".obj", nType )
         ELSEIF nType == TYPE_FROM_PRG
            // May later be converted in :Reset() to ".o" - we may not know which C compiler yet!
            Object := TMakeObject():New( cBaseFile + ".obj", nType )
         ELSEIF nType == TYPE_FROM_RC
            Object := TMakeObject():New( cBaseFile + ".res", nType )
         ELSE
            Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "unexpected type:" + cFile, HB_aParams() ) )
         ENDIF

         Object:DependOn( Dependant )
         Object:bAction := bAction
         Object:Project := Self

         ::DependOn( Object )
      NEXT
   NEXT

RETURN Object

METHOD AddObjects( ... ) CLASS TMakeProject

   LOCAL cMask, aFiles, aFile, cFolder, cFile, nAt
   LOCAL cExt, Module, nType

   FOR EACH cMask IN HB_aParams()
      aFiles := Directory( cMask )

      //IF Len( aFiles ) == 0
      //   Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "file not found: " + cMask, HB_aParams() ) )
      //ENDIF

      nAt := RAt( DIR_SEPARATOR, cMask )
      IF nAt > 0
         cFolder := Left( cMask, nAt )
      ELSE
         cFolder := ""
      ENDIF

      FOR EACH aFile IN aFiles
         cFile := aFile[1]
         //aAdd( ::aObjects, cFile )

         nAt := RAt( '.', cFile )
         cExt := Lower( SubStr( cFile, nAt + 1 ) )

         IF cExt == "lib" .or. cExt == "a"
            nType := TYPE_SOURCE_LIB
         ELSEIF cExt == "obj"
            nType := TYPE_SOURCE_OBJ
         ELSEIF cExt == "res"
            nType := TYPE_SOURCE_RES
         ELSE
            //Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "unsupported extension:" + cExt, HB_aParams() ) )
            nType := TYPE_NO_ACTION
         ENDIF

         //TraceLog( cFile )

         Module := TMakeObject():New( cFolder + cFile, nType )

         Module:Project := Self

         ::DependOn( Module )
      NEXT
   NEXT

RETURN Self

METHOD AddProject( cFile ) CLASS TmakeProject

   LOCAL oProject

   oProject := LoadProject( cFile, Self )

   ::DependOn( oProject )

RETURN oProject

FUNCTION LoadIni( oProject )

   LOCAL cIni, cLine, cGroup, nGroup, aLine
   LOCAL xHB_Root, xHB_Flags, xHB_LibFolder, xHB_Executable, xHB_IncFolder
   LOCAL C_Root, C_Flags, Link_Flags, C_Compiler := 0
   LOCAL FWH_Root, FWH_LibFolder
   LOCAL GUI_Root, GUI_LibFolder
   LOCAL nAt
   LOCAL sExeFolder

   IF ! Empty( oProject:cIni )
      cIni := MemoRead( oProject:cIni )

      IF Empty( cIni )
         Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Invalid INI file: '" + oProject:cIni + "'", HB_aParams() ) )
      ENDIF
   ENDIF

   // Recover using generic names.
   IF Empty( cIni )
      #ifdef __PLATFORM__Windows
         cIni := MemoRead( "xbuild.windows.ini" )
      #else
         cIni := MemoRead( "xbuild.linux.ini" )
      #endif
   ENDIF

   // Last resort.
   IF Empty( cIni )
      sExeFolder := GetModuleFileName()
      nAt := RAt( DIR_SEPARATOR, sExeFolder )
      sExeFolder := Left( sExeFolder, nAt )

      #ifdef __PLATFORM__Windows
         cIni := MemoRead( sExeFolder + "xbuild.windows.ini" )
      #else
         cIni := MemoRead( sExeFolder + "xbuild.linux.ini" )
      #endif
   ENDIF

   // No settings!
   IF Empty( cIni )
      RETURN .F.
   ENDIF

   FOR EACH cLine IN HB_aTokens( cIni, Chr(10) )
      #ifdef __PLATFORM__Windows
        IF cLine[-1] == Chr(13)
           cLine[-1] := ' '
        ENDIF
      #endif

      cLine := AllTrim( cLine )

      IF Empty( cLine )
         LOOP
      ENDIF

      IF cLine[1] == '#'
         LOOP
      ENDIF

      IF cLine[1] == '['
         cGroup := Lower( SubStr( cLine, 2, Len( cLine ) - 2 ) )

         DO CASE
            CASE cGroup == "xhb"
               nGroup := 1

            CASE cGroup == "xcc"
               nGroup := 2
               C_Compiler := 1

            CASE cGroup == "bcc"
               nGroup := 2
               C_Compiler := 2

         #ifdef __PLATFORM__Windows
            CASE cGroup == "mingw"
         #else
             CASE cGroup == "gcc"
         #endif
               nGroup := 2
               C_Compiler := 3

            CASE cGroup == "msvc"
               nGroup := 2
               C_Compiler := 4

            CASE cGroup == "pocc"
               nGroup := 2
               C_Compiler := 5

            CASE cGroup == "clang"
               nGroup := 2
               C_Compiler := 6

            CASE cGroup == "tcc"
               nGroup := 2
               C_Compiler := 7

            CASE cGroup == "fwh"
               nGroup := 3

            CASE cGroup == "gui"
               nGroup := 4

            OTHERWISE
               Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Invalid INI section: '" + cGroup + "'", HB_aParams() ) )
         ENDCASE
      ELSE
         #if 0
            aLine := HB_aTokens( cLine, '=' )

            IF Len( aLine ) == 1
               aAdd( aLine, "" )
            ELSEIF Len( aLine ) <> 2
               Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Invalid format: " + cLine, HB_aParams() ) )
            ENDIF
         #else
            nAt := At( '=',  cLine )

            IF nAt == 0
               aLine := { cLine, "" }
            ELSE
               aLine := { Left( cLine, nAt - 1 ), SubStr( cLine, nAt + 1 ) }
            ENDIF
         #endif

         aLine[1] := Lower( RTrim( aLine[1] ) )
         aLine[2] := LTrim( aLine[2] )

         SWITCH nGroup
            CASE 1
               DO CASE
                  CASE aLine[1] == "root"
                     xHB_Root := aLine[2]

                  CASE aLine[1] == "libfolder"
                     xHB_LibFolder := aLine[2]

                  CASE aLine[1] == "flags"
                     xHB_Flags := aLine[2]

                  CASE aLine[1] == "exe"
                     xHB_Executable := aLine[2]

                  CASE aLine[1] == "incfolder"
                     xHB_IncFolder := aLine[2]

                  OTHERWISE
                     Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Invalid xHB setting: " + aLine[1], HB_aParams() ) )
               ENDCASE
               EXIT

            CASE 2
               DO CASE
                  CASE aLine[1] == "root"
                     C_Root := aLine[2]

                  CASE aLine[1] == "compile flags"
                     C_Flags := aLine[2]
                     IF C_Compiler == 1
                        C_Flags := StrTran( C_Flags, "-DHB_STATIC_STARTUP", "" )
                     ENDIF

                  CASE aLine[1] == "link flags"
                     Link_Flags := aLine[2]

                  OTHERWISE
                     Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Invalid C Compiler setting: " + aLine[1], HB_aParams() ) )
               ENDCASE
               EXIT

            CASE 3
               DO CASE
                  CASE aLine[1] == "root"
                     FWH_Root := aLine[2]

                  CASE aLine[1] == "libfolder"
                     FWH_LibFolder := aLine[2]

                  OTHERWISE
                     Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Invalid FWH setting: " + aLine[1], HB_aParams() ) )
               ENDCASE
               EXIT

            CASE 4
               DO CASE
                  CASE aLine[1] == "root"
                     GUI_Root := aLine[2]

                  CASE aLine[1] == "libfolder"
                     GUI_LibFolder := aLine[2]

                  OTHERWISE
                     Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Invalid GUI setting: " + aLine[1], HB_aParams() ) )
               ENDCASE
               EXIT
         END
      ENDIF
   NEXT

   IF ! Empty( xHB_Root )
      IF xHB_Root[-1] != DIR_SEPARATOR
         xHB_Root += DIR_SEPARATOR
      ENDIF

      oProject:Set_xHB( ExpandEnvVars( xHB_Root ), NIL, xHB_LibFolder, xHB_Executable, xHB_IncFolder )

      IF ! Empty( xHB_Flags )
         oProject:PRG_Flags := xHB_Flags
      ENDIF
   ENDIF

   IF ! Empty( C_Root )
      IF C_Root[-1] != DIR_SEPARATOR
         C_Root += DIR_SEPARATOR
      ENDIF
   ENDIF

   SWITCH C_Compiler
      CASE 0
         EXIT

      CASE 1
         oProject:Set_XCC( IIF( Empty( C_Root ), s_sxCCFolder, c_Root ) )
         EXIT

      CASE 2
         oProject:Set_BCC( IIF( Empty( C_Root ), s_sBCCFolder, C_Root ) )
         EXIT

      CASE 3
         oProject:Set_GCC( IIF( Empty( C_Root ), s_sGCCFolder, C_Root ) )
         EXIT

      CASE 4
         oProject:Set_VC( IIF( Empty( C_Root ), s_sVCFolder, C_Root ), , , s_sVC_CL, s_sVC_LIB, s_sVC_LINK )
         EXIT

      CASE 5
         oProject:Set_POCC( IIF( Empty( C_Root ), s_sPOCCFolder, C_Root ) )
         EXIT

      CASE 6
         oProject:Set_CLang( IIF( Empty( C_Root ), s_sCLangFolder, C_Root ) )
         EXIT

      CASE 7
         oProject:Set_TCC( IIF( Empty( C_Root ), s_sTCCFolder, C_Root ) )
         EXIT
   END

   IF ! Empty( C_Flags )
      oProject:C_Flags := C_Flags + " "
   ENDIF

   IF ! Empty( Link_Flags )
      oProject:MyLink_Flags := Link_Flags + " "
   ENDIF

   IF ! Empty( FWH_Root )
      IF FWH_Root[-1] != DIR_SEPARATOR
         FWH_Root += DIR_SEPARATOR
      ENDIF

      oProject:FWH_Root      := FWH_Root
      oProject:FWH_LibFolder := FWH_LibFolder
   ENDIF

   IF ( ! Empty( GUI_Root ) ) .AND. ( ! ( GUI_Root == FWH_Root ) )
      IF GUI_Root[-1] != DIR_SEPARATOR
         GUI_Root += DIR_SEPARATOR
      ENDIF

      oProject:GUI_Root      := GUI_Root
      oProject:GUI_LibFolder := GUI_LibFolder
   ENDIF

RETURN .T.

FUNCTION FileWithPath( cFile )

   IF cFile[1] == '.'
      RETURN RelativeToAbsolutePath( cFile, DiskName() + DRIVE_SEPARATOR + DIR_SEPARATOR + CurDir() )
   ELSEIF ( DIR_SEPARATOR IN cFile )
      RETURN cFile
   ENDIF

RETURN DiskName() + DRIVE_SEPARATOR + DIR_SEPARATOR + CurDir() + DIR_SEPARATOR + cFile

FUNCTION LoadProject( cFile, Parent, cIniFile )

   LOCAL cIni := MemoRead( cFile ), cLine, nAt, cProperty, cValue, oProject, cCurrentFolder, cWorkFolder, oObject, xValue, oErr
   LOCAL aLines, cIncludedLine
   LOCAL cMissingFiles := ""
   LOCAL cDisk
   LOCAL cIncludeFolder, cFolder//, nBaseLine
   LOCAL aProperties

   #ifdef d_Debug_Loadproject
    TraceLog( ProcName(1), cFile, Parent, cIniFile )
   #endif

   IF cIni == ""
      #ifdef GUI
         Alert( "Error reading: " + FileWithPath( cFile ) )
      #endif

      Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Error reading: " + FileWithPath( cFile ), HB_aParams() ) )
   ENDIF

   cFile := Left( cFile, Len( cFile ) - 4 )

   IF cFile[2] == DRIVE_SEPARATOR
      cDisk := DiskName()
      DiskChange( cFile[1] )
      cFile := SubStr( cFile, 3 )
   ENDIF

   IF cFile[1] == '.'
      cFile := RelativeToAbsolutePath( cFile, DiskName() + DRIVE_SEPARATOR + DIR_SEPARATOR + CurDir() )
   ENDIF

   nAt := RAt( DIR_SEPARATOR, cFile )
   IF nAt > 0
      cCurrentFolder := DIR_SEPARATOR + CurDir()
      cWorkFolder := Left( cFile, nAt )
      cFile := SubStr( cFile, nAt + 1 )

      IF DirChange( cWorkFolder ) != 0
         #ifdef GUI
            Alert( "Could not switch to folder: " + cWorkFolder )
         #endif

         Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Could not switch to folder: " + cWorkFolder, HB_aParams() ) )
      ENDIF
   ENDIF

   //? "Project:", cFile
   oProject := TMakeProject():New( cFile )

   aLines := HB_aTokens( cIni, Chr(10) )

   IF Empty( cIniFile )
      // First scan looking for cIni only!!!
      FOR EACH cLine IN aLines
         IF cLine[-1] == Chr( 13 )
            cLine[-1] := ' '
         ENDIF

         cLine := AllTrim( cLine )

         IF Left( cLine, 4 ) == "CINI"
            nAt := At( '=', cLine )
            oProject:cIni := LTrim( SubStr( cLine, nAt + 1 ) )
            LoadIni( oProject )
            //Alert( oProject:xHB_Executable )
         ENDIF
      NEXT
   ELSE
      oProject:cIni := cIniFile
      LoadIni( oProject )
      //Alert( oProject:xHB_Executable + ", " + oProject:C_Executable )
   ENDIF

   // Inherit From outer Project.
   IF Parent != NIL
      oProject:Project := Parent

      WITH OBJECT oProject
         :cINI               := Parent:cINI

         :C_Root             := Parent:C_Root
         :RC_Root            := Parent:RC_Root
         :xHB_Root           := Parent:xHB_Root
         :xHB_LibFolder      := Parent:xHB_LibFolder
         :xHB_IncFolder      := Parent:xHB_IncFolder
         :SLY_Root           := Parent:SLY_Root

         :C_Executable       := Parent:C_Executable
         :xHB_Executable     := Parent:xHB_Executable
         :RC_Executable      := Parent:RC_Executable
         :Lib_Executable     := Parent:Lib_Executable
         :Link_Executable    := Parent:Link_Executable

         :bProgress          := Parent:bProgress

         :TargetFolder       := Parent:TargetFolder

         :OutputFolder       := Parent:OutputFolder
         :C_OutputFolder     := Parent:C_OutputFolder
         :PRG_OutputFolder   := Parent:PRG_OutputFolder
         :SLY_OutputFolder   := Parent:SLY_OutputFolder
         :RC_OutputFolder    := Parent:RC_OutputFolder
         :LibFolders         := Parent:LibFolders

         :FWH_Root           := Parent:FWH_Root
         :FWH_LibFolder      := Parent:FWH_LibFolder

         :GUI_Root           := Parent:GUI_Root
         :GUI_LibFolder      := Parent:GUI_LibFolder
         :GUI_PRGFlags       := Parent:GUI_PRGFlags

         :C_OutputFlag       := Parent:C_OutputFlag
         :PRG_OutputFlag     := Parent:PRG_OutputFlag
         :SLY_OutputFlag     := Parent:SLY_OutputFlag

         :Defines            := Parent:Defines
         :MyDefines          := Parent:MyDefines
         :C_Flags            := Parent:C_Flags
         :PRG_Flags          := Parent:PRG_Flags
         :RC_Flags           := Parent:RC_Flags

         :RC_OutputFlag      := Parent:RC_OutputFlag
         :RC_IncludeFlag     := Parent:RC_IncludeFlag
         :SLY_Flags          := Parent:SLY_Flags

         :Lib_Flags          := Parent:Lib_Flags
         :Lib_OutputFlag     := Parent:Lib_OutputFlag
         :Lib_AddFlag        := Parent:Lib_AddFlag

         :DefaultLink_Flags  := Parent:DefaultLink_Flags
         :Link_LibFolderFlag := Parent:Link_LibFolderFlag
         :Link_OutputFlag    := Parent:Link_OutputFlag

         :DLL_Flags          := Parent:DLL_Flags
         :DLL_Startup        := Parent:DLL_Startup

         :GUI_Flag           := Parent:GUI_Flag

         :hC_EnvVars         := Parent:hC_EnvVars
         :hPRG_EnvVars       := Parent:hPRG_EnvVars
         :hRC_EnvVars        := Parent:hRC_EnvVars
         :hSLY_EnvVars       := Parent:hSLY_EnvVars
      END WITH
   ENDIF

   // Initial object.
   oObject := oProject
   aProperties := {}

   // Now process the project
   FOR EACH cLine IN aLines
      /* I cannot see any reasons to not strip \r from project files
         on other platforms - it will allow us to use the same files
         on each platform, Przemek.
      */
      // #ifdef __PLATFORM__Windows
         IF cLine[-1] == Chr( 13 )
            cLine[-1] := ' '
         ENDIF
      // #endif

      cLine := AllTrim( cLine )

      IF Empty( cLine )
         LOOP
      ENDIF

      IF cLine[1] == '#'
         IF Lower( SubStr( cLine, 2, 7 ) ) == "include"
            cFile := SubStr( cLine, 11 )
            cFile := Left( cFile, Len( cFile ) - 1 )

            nAt := RAt( DIR_SEPARATOR, cFile )

            IF nAt > 0
               cIncludeFolder := Left( cFile, nAt - 1 )

               IF cIncludeFolder[1] == DIR_SEPARATOR
                  cIncludeFolder := DiskName() + DRIVE_SEPARATOR + cIncludeFolder
               ENDIF
            ELSE
               cIncludeFolder := DiskName() + DRIVE_SEPARATOR + DIR_SEPARATOR + CurDir()
            ENDIF

            IF File( cFile )
               //nBaseLine := HB_EnumIndex()

               FOR EACH cIncludedLine IN HB_aTokens( MemoRead( cFile ), Chr(10) )
                  IF cIncludedLine[-1] == Chr(13)
                     cIncludedLine := Left( cIncludedLine, Len( cIncludedLine ) - 1 )
                  ENDIF

                  IF cIncludedLine[1] == '[' .AND. cIncludedLine[2] != DIR_SEPARATOR .AND. cIncludedLine[3] != DRIVE_SEPARATOR
                     IF Lower( Right( cIncludedLine, 5 ) ) == ".lib]"
                        // Keep as is
                        //Alert( "Keep: " + cIncludedLine )
                     ELSE
                        // The file name will include the closing bracket ']'!!!
                        cIncludedLine := '[' + RelativeToAbsolutePath( SubStr( cIncludedLine, 2 ), cIncludeFolder )
                        //Alert( "Relative: " + cIncludedLine )
                     ENDIF
                  ELSEIF Left( cIncludedLine, 14 ) == "INCLUDEFOLDERS" .OR. Left( cIncludedLine, 10 ) == "LIBFOLDERS"
                     nAt := At( '=', cIncludedLine )

                     IF nAt == 0
                        #ifdef GUI
                           Alert( "Invalid format: '" + cIncludedLine + "' in: " + cFile )
                        #endif

                        Throw( ErrorNew( "xBuild", 1001, 0, ProcName(), "Invalid format: '" + cIncludedLine + "' in: " + cFile, HB_aParams() ) )
                     ENDIF

                     //cProperty     := RTrim( Left( cIncludedLine, nAt - 1 ) )
                     cValue        := LTrim( SubStr( cIncludedLine, nAt + 1 ) )
                     cIncludedLine := Left( cIncludedLine, nAt )

                     FOR EACH cFolder IN HB_aTokens( cValue, ';' )
                        cIncludedLine += RelativeToAbsolutePath( cFolder, cIncludeFolder )
                        cIncludedLine += ';'
                     NEXT
                  ENDIF

                  //? nBaseLine, HB_EnumIndex(), nBaseLine + HB_EnumIndex(), cIncludedLine
                  //aIns( aLines, nBaseLine + HB_EnumIndex(), cIncludedLine, .T. )
                  aAdd( aLines, cIncludedLine )
               NEXT
            ELSE
               #ifdef GUI
                  Alert( "Error reading #include: " + cFile )
               #endif

               Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Error reading #include: " + cFile, HB_aParams() ) )
            ENDIF
         ENDIF

         LOOP
      ENDIF

      IF cLine[1] == '['
         // If before adding any files - optionally override xHB Settings.
         IF oObject == oProject
            IF Empty( oProject:xHB_Root ) .OR. Empty( oProject:C_Root )
               LoadIni( oProject )
            ENDIF
         ENDIF

         TRY
            //? CurDir()
            //? "File:", SubStr( cLine, 2, Len( cLine ) - 2 )
            //?
            oObject := oProject:AddFiles( SubStr( cLine, 2, Len( cLine ) - 2 ) )
            aProperties := {}
         CATCH oErr
            IF oErr:Description = "File not found"
               cMissingFiles += oErr:Description + ';'
            ELSE
               //Alert( "Warning, LoadProject() encountered: '" + oErr:Description )
               Throw( oErr )
            ENDIF
         END
      ELSE
         nAt := At( '=', cLine )

         IF nAt == 0
            #ifdef GUI
               Alert( "Invalid format: '" + cLine + "' in: " + cFile )
            #endif

            Throw( ErrorNew( "xBuild", 1001, 0, ProcName(), "Invalid format: '" + cLine + "' in: " + cFile, HB_aParams() ) )
         ENDIF

         cProperty := RTrim( Left( cLine, nAt - 1 ) )
         cValue    := LTrim( SubStr( cLine, nAt + 1 ) )

         #ifdef d_Debug_LoadProject
          TraceLog(cProperty)
         #endif

         IF aScan( aProperties, cProperty, , , .T. ) == 0
            IF ! cValue == ""
               aAdd( aProperties, cProperty )
               #ifdef d_Debug_Loadproject
                TraceLog( oObject, cProperty, cValue )
               #endif
               IF ( "DEFINES" IN cProperty )
                  oObject:SetDefines( cValue )
               ELSEIF cProperty == "INCLUDEFOLDERS"
                  oObject:SetIncludeFolders( cValue )
               ELSEIF cProperty == "LAUTORUN" .AND. ! Empty( cValue )
                  oObject:lAutoRun := CStrToVal( cValue, "L" )
               ELSEIF cProperty == "CINI" .AND. ! Empty( cValue )
                  // Already processed above!
               ELSE
                  xValue := CStrToVal( cValue, ValType( __ObjSendMsg( oObject, cProperty ) ) )
                  __ObjSendMsg( oObject, "_" + cProperty, xValue )
                  #ifdef d_Debug_LoadProject
                   TraceLog( oObject:ClassName, cProperty, xValue )
                  #endif
               ENDIF
            ENDIF
         ELSE
            #ifdef d_Debug_LoadProject
             TraceLog( "Already set: " + cProperty )
            #endif
         ENDIF
      ENDIF
   NEXT

   IF cCurrentFolder != NIL
      IF DirChange( cCurrentFolder ) != 0
         #ifdef GUI
            Alert( "Could not restore to folder: " + cCurrentFolder )
         #endif

         Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Could not restore to folder: " + cCurrentFolder, HB_aParams() ) )
      ENDIF
   ENDIF

   IF cDisk != NIL
      DiskChange( cDisk )
   ENDIF

   IF ! Empty( cMissingFiles )
      #ifdef GUI
         Alert( "Could not load one or more files:;;" + cMissingFiles )
      #endif

      //MemoEdit( 2, 2, MaxRow() - 2, MaxCol() - 2, .F. )
      Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Could not load one or more files:;;" + cMissingFiles ) )
   ENDIF

   oProject:aLoadedProperties := __ClsGetPropertiesAndValues( oProject )

   oProject:lAddedDependencies := .F.

   #ifdef d_Debug_LoadProject
    TraceLog( ValToPrg( oProject ) )
    TraceLog( ValToPrg( oProject:Defines ) )
   #endif

RETURN oProject

METHOD Includes( cFlag ) CLASS TMakeProject

   LOCAL cIncludeFolders

   IF Empty( ::aIncludeFolders )
      RETURN ""
   ELSE
      cIncludeFolders := ""
      aEval( ::aIncludeFolders, {|cIncludeFolder| cIncludeFolder[-1] := '"', cIncludeFolders += cFlag + '"' + cIncludeFolder + " " } )
   ENDIF

RETURN cIncludeFolders

METHOD Make( bOnErr, bProgress ) CLASS TMakeProject

   LOCAL cTraceFile, nTraceLevel
   LOCAL cLogFile := ::cFile + ".log"
   LOCAL aProperty
   //LOCAL bPreviousHandler
   LOCAL oError, bThrow := .F.

   ::ValidateSettings()

   IF bOnErr == NIL
      bThrow := .T.

      #ifdef __PLATFORM__Windows
         #ifdef GUI
            bOnErr := { |oErr| TraceLog( oErr:Description, oErr:Operation, oErr:ProcName, oErr:ProcLine ), ;
                               GUI_ErrorGrid( oErr, MemoRead( cLogFile ) ),;
                               /*ShellExecute( GetActiveWindow() , 'open', FileWithPath( cLogFile ), , , SW_SHOW ),/*;
                               /*xBuild_GUI_OnError( oErr, MemoRead( cLogFile ) ),*/;
                               Break( oErr ) }
         #else
            bOnErr := { |oErr| IIF( oErr:Description = "Couldn't build", ;
                                    TraceLog( oErr:Description ), ;
                                    TraceLog( oErr:Description, oErr:Operation, oErr:ProcName, oErr:ProcLine ) ;
                                  ), ;
                               __Run( "NotePad.exe " + FileWithPath( cLogFile ) ) }
         #endif

      #else
         bOnErr := { |oErr| TraceLog( oErr:Description, oErr:Operation, oErr:ModuleName, oErr:ProcName, oErr:ProcLine ), __Run( "nano " + FileWithPath( cLogFile ) ) }
      #endif
   ENDIF

   TRY
      cTraceFile  := SET( _SET_TRACEFILE, ::cFile + ".log" )
      
      #ifdef d_Debug
         nTraceLevel:= SET( _SET_TRACESTACK, 2 )
      #else
         nTraceLevel:= SET( _SET_TRACESTACK, 0 )
      #endif

      #ifdef AUTO_CLEAN
         aFileInfo := Directory( ::cFile + ".xbp" )

         IF Len( aFileInfo ) == 1
            dXBP_Date := aFileInfo[1][3]
            cXBP_Time := aFileInfo[1][4]

            aFileInfo := Directory( ::OutputFile( .T. ) )
            IF Len( aFileInfo ) == 1
               IF dXBP_Date > aFileInfo[1][3] .OR. ( dXBP_Date == aFileInfo[1][3] .AND. cXBP_Time >= aFileInfo[1][4] )
                  TraceLog( "Modified Project File, forcing clean build." )
                  ::lClean := .T.
               ENDIF
            ENDIF
         ENDIF
      #endif

      IF ::lXbp
         IF Empty( ::aLoadedProperties ) .OR. ::lAddedDependencies .OR. ::lExpand
            GenerateProjectFile( Self )
         ELSE
            FOR EACH aProperty IN __ClsGetPropertiesAndValues( Self )
               /*
               IF aProperty[1] == "LAUTORUN" .OR. aProperty[1] == "RUNARGUMENTS"
                  // Ignore.
                  LOOP
               ENDIF
               */

               IF ( ValType( aProperty[2] ) != ValType( ::aLoadedProperties[ HB_EnumIndex() ][2] ) ) .OR. ! ( aProperty[2] == ::aLoadedProperties[ HB_EnumIndex() ][2] )
                  GenerateProjectFile( Self )
                  EXIT
               ENDIF
            NEXT
         ENDIF
      ENDIF

      ::bProgress := bProgress

      IF Len( ::TargetFolder ) > 0
         IF Len( Directory( ::TargetFolder, 'D' ) ) == 0
            IF ::TargetFolder[-1] == DIR_SEPARATOR
               ::TargetFolder[-1] := 0
            ENDIF

            MakeNestedDir( ::TargetFolder )

            IF ::TargetFolder[-1] == 0
               ::TargetFolder[-1] := DIR_SEPARATOR
            ENDIF
         ENDIF
      ENDIF

      IF Len( ::OutputFolder ) > 0
         IF Len( Directory( ::OutputFolder, 'D' ) ) == 0
            IF ::OutputFolder[-1] == DIR_SEPARATOR
               ::OutputFolder[-1] := 0
            ENDIF

            MakeNestedDir( ::OutputFolder )
         ENDIF

         IF ::OutputFolder[-1] == 0
            ::OutputFolder[-1] := DIR_SEPARATOR
         ELSE
           ::OutputFolder += DIR_SEPARATOR
         ENDIF
      ENDIF

      IF Empty( ::C_OutputFolder )
         ::C_OutputFolder := ::OutputFolder
      ELSE
         IF Len( ::C_OutputFolder ) > 0
            IF Len( Directory( ::C_OutputFolder, 'D' ) ) == 0
               IF ::C_OutputFolder[-1] == DIR_SEPARATOR
                  ::C_OutputFolder[-1] := 0
               ENDIF

               MakeNestedDir( ::C_OutputFolder )
            ENDIF

            IF ::C_OutputFolder[-1] == 0
               ::C_OutputFolder[-1] := DIR_SEPARATOR
            ELSE
              ::C_OutputFolder += DIR_SEPARATOR
            ENDIF
         ENDIF
      ENDIF

      IF Empty( ::PRG_OutputFolder )
         ::PRG_OutputFolder := ::OutputFolder
      ELSE
         IF Len( ::PRG_OutputFolder ) > 0
            IF Len( Directory( ::PRG_OutputFolder, 'D' ) ) == 0
               IF ::PRG_OutputFolder[-1] == DIR_SEPARATOR
                  ::PRG_OutputFolder[-1] := 0
               ENDIF

               MakeNestedDir( ::PRG_OutputFolder )
            ENDIF

            IF ::PRG_OutputFolder[-1] == 0
               ::PRG_OutputFolder[-1] := DIR_SEPARATOR
            ELSE
              ::PRG_OutputFolder += DIR_SEPARATOR
            ENDIF
         ENDIF
      ENDIF

      IF Empty( ::SLY_OutputFolder )
         ::SLY_OutputFolder := ::OutputFolder
      ELSE
         IF Len( ::SLY_OutputFolder ) > 0
            IF Len( Directory( ::SLY_OutputFolder, 'D' ) ) == 0
               IF ::SLY_OutputFolder[-1] == DIR_SEPARATOR
                  ::SLY_OutputFolder[-1] := 0
               ENDIF

               MakeNestedDir( ::SLY_OutputFolder )
            ENDIF

            IF ::SLY_OutputFolder[-1] == 0
               ::SLY_OutputFolder[-1] := DIR_SEPARATOR
            ELSE
              ::SLY_OutputFolder += DIR_SEPARATOR
            ENDIF
         ENDIF
      ENDIF

      IF Empty( ::RC_OutputFolder )
         ::RC_OutputFolder := ::OutputFolder
      ELSE
         IF Len( ::RC_OutputFolder ) > 0
            IF Len( Directory( ::RC_OutputFolder, 'D' ) ) == 0
               IF ::RC_OutputFolder[-1] == DIR_SEPARATOR
                  ::RC_OutputFolder[-1] := 0
               ENDIF

               MakeNestedDir( ::RC_OutputFolder )
            ENDIF

            IF ::RC_OutputFolder[-1] == 0
               ::RC_OutputFolder[-1] := DIR_SEPARATOR
            ELSE
              ::RC_OutputFolder += DIR_SEPARATOR
            ENDIF
         ENDIF
      ENDIF

      IF ( Lower( ::cFile ) HAS "xbuild.*\.exe" )
         ::lMinimal := .T.
      ENDIF

      ::aIncludedFiles := {}

      ::CatchUp()

      SET( _SET_TRACEFILE, cTraceFile, .T. )
      SET( _SET_TRACESTACK, nTraceLevel )
   CATCH oError
      //TraceLog( ValToPrg( oError ) )
      //TraceLog( ValToPrg( Self ) )
      Eval( bOnErr, oError )

      IF bThrow
         Throw( oError )
      ENDIF
   END

   IF ::lIni
      GenerateIni( Self )
   ENDIF

RETURN ::lCurrent

METHOD PrgSources() CLASS TMakeProject

   LOCAL cList := "", Dependancy

   FOR EACH Dependancy IN ::aDependancies
      IF Dependancy:nType == TYPE_FROM_PRG
         cList += RelativeToAbsolutePath( Dependancy:aDependancies[1]:cFile, ::LoadFolder ) + ";"
      ELSEIF Dependancy:ClassName == "TMAKEPROJECT"
         cList += Dependancy:PrgSources() + ";"
      ENDIF
   NEXT

   cList := Left( cList, Len( cList ) - 1 )

RETURN cList

METHOD SetIncludeFolders( cIncludeFolders ) CLASS TMakeProject

   LOCAL cFolder, cRelativeFolder, bRelative, nAt

   ::IncludeFolders  := cIncludeFolders

   ::aIncludeFolders := HB_aTokens( ExpandEnvVars( cIncludeFolders ), ';', .T. )

   FOR nAt := 1 TO Len( ::aIncludeFolders )
      IF Empty( ::aIncludeFolders[ nAt ] )
         aDel( ::aIncludeFolders, nAt, .T. )
         nAt--
      ENDIF
   NEXT

   FOR EACH cFolder IN ::aIncludeFolders
       bRelative := .F.
       cFolder := AllTrim( cFolder )

       #ifndef __PLATFORM__Windows
           cFolder := strtran( cFolder, "\", DIR_SEPARATOR )
           cRelativeFolder := DIR_SEPARATOR + CurDir()
       #else
           cRelativeFolder := DiskName() + DRIVE_SEPARATOR + DIR_SEPARATOR + CurDir()
       #endif

       WHILE cFolder[1] == '.' .AND. cFolder[2] == DIR_SEPARATOR
          bRelative := .T.
          cFolder := LTrim( SubStr( cFolder, 3 ) )
       END

       WHILE cFolder[1] == '.' .AND. cFolder[2] == '.' .AND. cFolder[3] == DIR_SEPARATOR
          bRelative := .T.
          cFolder := LTrim( SubStr( cFolder, 4 ) )

          nAt := RAt( DIR_SEPARATOR, cRelativeFolder )
          IF nAt == 0
             Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "invalid directory specification: " + ::aIncludeFolders[ HB_EnumIndex() ], HB_aParams() ) )
          ELSE
             cRelativeFolder := Left( cRelativeFolder, nAt - 1 )
          ENDIF
       END

       IF cFolder[-1] != DIR_SEPARATOR
          cFolder += DIR_SEPARATOR
       ENDIF

       IF bRelative
          cFolder := cRelativeFolder + DIR_SEPARATOR + cFolder
       ENDIF
   NEXT

RETURN Self

METHOD EXE_Command() CLASS TMakeProject

   LOCAL cCommand

   cCommand := ::Link_Flags()

   IF ::Link_Executable == "ilink32.exe"
      IF ( "-v" IN cCommand ) .OR. ( "/v" IN cCommand )
         ::C_Libs := "cg32.lib " + ::C_Libs

         IF ::Project:lSQL
            ::C_Libs += "odbccp32.lib "
         ENDIF
      ENDIF

      IF ::Project:lDebug
         cCommand += ::Project:Link_DebugFlags
      ENDIF

      IF ::lGUI
         cCommand += ::GUI_Flag
      ELSE
         cCommand += ::Console_Flag
      ENDIF

      cCommand += SplitFoldersWithFlag( ::LibFoldersX, ::Link_LibFolderFlag )

      IF ::lGUI
         cCommand += SplitFoldersWithFlag( ::GUI_LibFolderX, ::Link_LibFolderFlag )
      ENDIF

      IF ::lFWH
         cCommand += SplitFoldersWithFlag( ::FWH_LibFolderX, ::Link_LibFolderFlag )
      ENDIF

      IF ::lPRG
         cCommand += SplitFoldersWithFlag( ::xHB_LibFolderX, ::Link_LibFolderFlag )
      ENDIF

      IF ::lMT
         ::C_Libs := StrTran( ::C_Libs, "cw32.lib", "cw32mt.lib " )
      ENDIF

      cCommand += ::Link_LibFolderFlag + '"' + ::C_RootX + 'lib" '
      cCommand += ::Link_LibFolderFlag + '"' + ::C_RootX + 'lib\psdk" '

      IF ::lGUI
         cCommand += ::GUI_Startup
      ELSE
         cCommand += ::Console_Startup
      ENDIF

      cCommand += StrTran( Self:ObjList(), ",", " " )
      cCommand += ", " + ::OutputFile( .F. )

      cCommand += ", "
      cCommand += ::MapFile

      cCommand += ", "
      IF ::lUseDll .AND. ::lPRG
         cCommand += "use_dll.lib "
      ENDIF
      cCommand += StrTran( Self:LibList(), ",", " " ) + ::C_Libs

      cCommand += ", "
      cCommand += ::DefFile

      cCommand += ", "
      cCommand += StrTran( Self:ResList(), ",", " " )
  ELSEIF ( ::Link_Executable IN "ld.exe;lld.exe;gcc.exe;clang.exe" ) /* "ld;lld;gcc;clang" are implied */
      IF ::Project:lDebug
         cCommand += ::Project:Link_DebugFlags
      ENDIF

      IF ::lGUI
         cCommand += ::GUI_Flag
      ELSE
         cCommand += ::Console_Flag
      ENDIF

      cCommand += ::Link_OutputFlag + ::OutputFile( .F. ) + " "
      cCommand += StrTran( Self:ObjList(), ",", " " )

      cCommand += SplitFoldersWithFlag( ::LibFoldersX, ::Link_LibFolderFlag )

      IF ::lGUI
         cCommand += SplitFoldersWithFlag( ::GUI_LibFolderX, ::Link_LibFolderFlag )
      ENDIF

      IF ::lPRG
         cCommand += SplitFoldersWithFlag( ::xHB_LibFolderX, ::Link_LibFolderFlag )
      ENDIF

      cCommand += ::Console_Startup
      cCommand += StrTran( Self:ResList(), ",", " " )

      IF ::Project:lX
         cCommand += "-Wl, --start-group " + IIF( ::lUseDll, "use_dll.lib ", "" ) + StrTran( Self:LibList(), ",", " " ) + "-Wl, --end-group"
      ELSE
         cCommand += + StrTran( Self:LibList(), ",", " " )
      ENDIF

      cCommand += ::C_Libs
   ELSE
      IF ::Project:lDebug
         cCommand += ::Project:Link_DebugFlags
      ENDIF

      IF ::Link_Executable == "xlink.exe"
         cCommand += "-NOIMPLIB "
      ENDIF

      IF ::lGUI
         cCommand += ::GUI_Flag

         IF ::lFWH .AND. ::Link_Executable == "xlink.exe"
           cCommand += "-UNMANGLE "
         ENDIF
      ELSE
         cCommand += ::Console_Flag
      ENDIF

      cCommand += SplitFoldersWithFlag( ::LibFoldersX, ::Link_LibFolderFlag )

      IF ::lGUI
         cCommand += SplitFoldersWithFlag( ::GUI_LibFolderX, ::Link_LibFolderFlag )
      ENDIF

      IF ::lFWH
         cCommand += SplitFoldersWithFlag( ::FWH_LibFolderX, ::Link_LibFolderFlag )
      ENDIF

      IF ::lPRG
         cCommand += SplitFoldersWithFlag( ::xHB_LibFolderX, ::Link_LibFolderFlag )
      ENDIF

      IF ::Link_Executable == "xlink.exe"
         cCommand += ::Link_LibFolderFlag + '"' + ::C_RootX + 'c_lib" '
         cCommand += ::Link_LibFolderFlag + '"' + ::C_RootX + 'c_lib\win" '

         IF ::lMT
            ::C_Libs := StrTran( ::C_Libs, "crt.lib", "crtmt.lib" )
         ENDIF
      ELSEIF ::Link_Executable == "polink.exe"
         cCommand += ::Link_LibFolderFlag + '"' + ::C_RootX + 'lib" '
         cCommand += ::Link_LibFolderFlag + '"' + ::C_RootX + 'lib\win" '

         IF ::lMT
            ::C_Libs := StrTran( ::C_Libs, "crt.lib", "crtmt.lib" )
         ENDIF
      ELSEIF ::Link_Executable == s_sVC_LINK//"link.exe"
         cCommand += ::Link_LibFolderFlag + '"' + ::C_RootX + ::C_LibFolder + '" '
         
         IF Empty( s_sWinSDKLibFolder )
            cCommand += ::Link_LibFolderFlag + '"' + ::C_RootX + 'PlatformSdk\lib" '
         ELSE
            cCommand += ::Link_LibFolderFlag + '"' + s_sWinSDKLibFolder + '" '
         ENDIF

         IF ! Empty( s_sUniversalCRT_LibraryPath_x86 )
            cCommand += ::Link_LibFolderFlag + '"' + s_sUniversalCRT_LibraryPath_x86 + '" '
         ENDIF
         
         IF ::lMT
            ::C_Libs := StrTran( ::C_Libs, "libc.lib", "libcmt.lib" )
         ENDIF
      ELSE
         cCommand += ::Link_LibFolderFlag + '"' + ::C_RootX + 'lib" '
      ENDIF

      IF ::lGUI
         cCommand += ::GUI_Startup
      ELSE
         cCommand += ::Console_Startup
      ENDIF

      cCommand += StrTran( Self:ObjList(), ",", " " )

      cCommand += StrTran( Self:ResList(), ",", " " )

      IF ::lUseDll .AND. ::lPRG
         IF ::Link_Executable == "xlink.exe"
            cCommand += "usedll.lib "
         ELSE
            cCommand += "use_dll.lib "
         ENDIF
      ENDIF

      IF ::lPRG_Debug .AND. ::lPRG_ClassicDebug == .F.
         cCommand += "DbgServe.lib "
      ENDIF

      cCommand += IIF( ::lSQL .AND. ::lMT, StrTran( StrTran( Self:LibList(), ",", " " ), " sql.lib", " sqlmt.lib" ) , StrTran( Self:LibList(), ",", " " ) )

      cCommand += ::C_Libs

      IF ::lGUI .AND. ( ::Link_Executable == "xlink.exe" .OR. ::Link_Executable == s_sVC_LINK/*"link.exe"*/ )
         cCommand += "comctl32.lib comdlg32.lib gdi32.lib shell32.lib winmm.lib lz32.lib Netapi32.lib "
      ENDIF

      cCommand += ::Link_OutputFlag + ::OutputFile( .F. )
   ENDIF

RETURN cCommand

METHOD DLL_Command() CLASS TMakeProject

   LOCAL cCommand

   IF ! Empty( ::MyLink_Flags )
      cCommand := ::MyLink_Flags + " "
   ELSE
      cCommand := ::DLL_Flags
   ENDIF

   IF ::Link_Executable == "xlink.exe" .OR. ::Link_Executable == s_sVC_LINK//"link.exe"
      cCommand := StrTran( cCommand, "-NOEXPOBJ", "" )

      IF ! ( "-DLL" IN cCommand )
         cCommand += "-DLL "
      ENDIF

      IF ::lPRG .AND. ( ! ::lC )
         cCommand += "-BASE:0x20000000 "
      ELSEIF ( ! ::lPRG ) .AND. ( ! ::lC )
         cCommand += "-machine:ix86 -noentry "
      ENDIF
   ENDIF

   IF ::Project:lDebug
      cCommand += ::Project:Link_DebugFlags
   ENDIF

   IF ::Link_Executable == "ilink32.exe"
      cCommand += SplitFoldersWithFlag( ::LibFoldersX, ::Link_LibFolderFlag )

      IF ::lGUI
         cCommand += SplitFoldersWithFlag( ::GUI_LibFolderX, ::Link_LibFolderFlag )
      ENDIF

      IF ::lFWH
         cCommand += SplitFoldersWithFlag( ::FWH_LibFolderX, ::Link_LibFolderFlag )
      ENDIF

      IF ::lPRG
         cCommand += SplitFoldersWithFlag( ::xHB_LibFolderX, ::Link_LibFolderFlag )
      ENDIF

      cCommand += ::Link_LibFolderFlag + '"' + ::C_RootX + 'lib" '
      cCommand += ::Link_LibFolderFlag + '"' + ::Project:C_RootX + 'lib\psdk" '

      cCommand += ::DLL_Startup

      cCommand += StrTran( Self:ObjList(), ",", " " )

      cCommand += ", "
      cCommand += ::OutputFile( .F. )

      cCommand += ", "
      cCommand += ::MapFile

      cCommand += ", "
      IF ::lPRG
         IF ! ( "oleserver.lib" IN Lower( Self:LibList() ) )
            cCommand += "dllmain.lib "
         ENDIF
      ENDIF
      cCommand += StrTran( Self:LibList(), ",", " " ) + ::C_Libs

      cCommand += ", "
      cCommand += ::DefFile

      cCommand += ", "
      cCommand += StrTran( Self:ResList(), ",", " " )
   ELSE
      cCommand += SplitFoldersWithFlag( ::LibFoldersX, ::Link_LibFolderFlag )

      IF ::lGUI
         cCommand += SplitFoldersWithFlag( ::GUI_LibFolderX, ::Link_LibFolderFlag )
      ENDIF

      IF ::lFWH
         cCommand += SplitFoldersWithFlag( ::FWH_LibFolderX, ::Link_LibFolderFlag )
      ENDIF

      IF ::lPRG
         cCommand += SplitFoldersWithFlag( ::xHB_LibFolderX, ::Link_LibFolderFlag )
      ENDIF

      IF ::Link_Executable == "xlink.exe"
         cCommand += ::Link_LibFolderFlag + '"' + ::C_RootX + 'c_lib" '
         cCommand += ::Link_LibFolderFlag + '"' + ::C_RootX + 'c_lib\win" '

         IF ::lMT
            ::C_Libs := StrTran( ::C_Libs, "crt.lib", "crtmt.lib" )
         ENDIF
      ELSEIF ::Link_Executable == s_sVC_LINK//"link.exe"
         cCommand += ::Link_LibFolderFlag + '"' + ::C_RootX + ::C_LibFolder + '" '
         IF Empty( s_sWinSDKLibFolder )
            cCommand += ::Link_LibFolderFlag + '"' + ::C_RootX + 'PlatformSdk\lib" '
         ELSE
            cCommand += ::Link_LibFolderFlag + '"' + s_sWinSDKLibFolder + '" '
         ENDIF

         IF ! Empty( s_sUniversalCRT_LibraryPath_x86 )
            cCommand += ::Link_LibFolderFlag + '"' + s_sUniversalCRT_LibraryPath_x86 + '" '
         ENDIF
         
         IF ::lMT
            ::C_Libs := StrTran( ::C_Libs, "libc.lib", "libcmt.lib" )
         ENDIF
      ELSE
         cCommand += ::Link_LibFolderFlag + '"' + ::C_RootX + 'lib" '
      ENDIF

      cCommand += ::DLL_Startup
      cCommand += StrTran( Self:ObjList(), ",", " " )

      IF ::lPRG
         IF ! ( "oleserver.lib" IN Lower( Self:LibList() ) )
            IF ::Link_Executable == "xlink.exe"
               cCommand += "dmain.lib "
            ELSE
               cCommand += "dllmain.lib "
            ENDIF
         ENDIF
      ENDIF

      cCommand += StrTran( Self:LibList(), ",", " " )

      IF ::lGUI .AND. ( ::Link_Executable == "xlink.exe" .OR. ::Link_Executable == s_sVC_LINK/*"link.exe"*/ )
         cCommand += "comctl32.lib comdlg32.lib gdi32.lib shell32.lib winmm.lib lz32.lib "
      ENDIF

      cCommand += StrTran( Self:ResList(), ",", " " )
      cCommand += ::C_Libs + ::Link_OutputFlag + ::OutputFile( .F. )
   ENDIF

RETURN cCommand

METHOD ObjList() CLASS TMakeProject

   LOCAL cList := "", Dependancy

   IF ::xHB_Executable == "xhb.exe" .OR. ::xHB_Executable == "xhb"
      FOR EACH Dependancy IN ::aDependancies
         IF Dependancy:nType == TYPE_FROM_C .OR. Dependancy:nType == TYPE_FROM_PRG .OR. Dependancy:nType == TYPE_SOURCE_OBJ
            cList += Dependancy:OutputFile( .F., .F. ) + ','
         ENDIF
      NEXT
   ELSE
      FOR EACH Dependancy IN ::aDependancies
         IF Dependancy:nType == TYPE_FROM_C .OR. Dependancy:nType == TYPE_SOURCE_OBJ
            cList += Dependancy:OutputFile( .F., .F. ) + ','
         ENDIF
      NEXT
   ENDIF

   IF cList == ""
      RETURN ""
   ENDIF

   cList[-1] := ' '

RETURN cList

METHOD LibList() CLASS TMakeProject

   LOCAL cList := "", Dependancy, nAt, cWorkFolder, cFile

   FOR EACH Dependancy IN ::aDependancies
      //TraceLog( Dependancy:nType, Dependancy:cFile )

      IF Dependancy:nType == TYPE_SOURCE_LIB
// cList += Dependancy:OutputFile( .F. ) + ','
         // "gcc;clang" implied
         IF Right( Dependancy:cFile, 6 ) == ".dylib"
            cList += '"' + Dependancy:Project:xHB_LibFolder + Dependancy:cFile + '",'
         ELSEIF Left( Dependancy:cFile, 2 ) == "-l"   
            cList += '"' + Dependancy:cFile + '",'
         ELSE
            cList += '"' + IIF( ( ::C_Executable IN "gcc.exe;clang.exe" ), "-l", "" ) + ;
                           IIF( ( ::C_Executable IN "gcc.exe;clang.exe" ), StrTran( StrTran( StrTran( Dependancy:OutputFile( .T. ), "lib" , "" ), ".a", "" ), ".so", "" ), Dependancy:OutputFile( .T. ) ) + '",'
         ENDIF 
         //TraceLog( IIF( Dependancy:cFile == "", ValToPrg( Dependancy ), "Ok" ), cList, Dependancy:cFile, Dependancy:OutputFile(.F.) )
      ELSEIF Dependancy:nType == TYPE_LIB
         nAt := RAt( DIR_SEPARATOR, Dependancy:cFile )

         IF nAt > 0
            cWorkFolder := Left( Dependancy:cFile, nAt )
            cFile := SubStr( Dependancy:cFile, nAt + 1 )
// cFile := cWorkFolder + Dependancy:TargetFolder + DIR_SEPARATOR + cFile
            // "gcc;clang" implied!
            cFile := IIF( ( ::C_Executable IN "gcc.exe;clang.exe" ), '-l"', '"' ) + cWorkFolder + Dependancy:TargetFolder + ;
                          IIF( Dependancy:TargetFolder[-1] == DIR_SEPARATOR, "", DIR_SEPARATOR ) + cFile + '"'
         ELSE
            cFile := Dependancy:OutputFile( .F. )
         ENDIF

         IF Dependancy:ClassName == "TMAKEPROJECT"
            IF (! Dependancy:lPRG ) .AND. ( ! Dependancy:lC )
               // Resource Dll
               TraceLog( "Skipping Resource Dll: " + Dependancy:cFile, cFile )
               LOOP
            ENDIF
         ENDIF

         cList += cFile + ','
         //TraceLog( IIF( Dependancy:cFile == "", ValToPrg( Dependancy ), "Ok" ), cList, Dependancy:cFile, Dependancy:OutputFile(.F.) )
      ENDIF
   NEXT

   IF cList == ""
      RETURN ""
   ENDIF

   cList[-1] := ' '

RETURN cList

#ifndef RC_DEFERRED
   METHOD ResList() CLASS TMakeProject

      LOCAL cList := "", Dependancy

      FOR EACH Dependancy IN ::aDependancies
         IF Dependancy:nType == TYPE_FROM_RC .OR. Dependancy:nType == TYPE_SOURCE_RES
            cList += Dependancy:OutputFile( .F. ) + ','
         ENDIF
      NEXT

      IF cList == ""
         RETURN ""
      ENDIF

      cList[-1] := ' '

   RETURN cList
#endif

METHOD ValidateSettings() CLASS TMakeProject
      LOCAL nAt

   IF Empty( ::xHB_Executable ) .OR. Empty(::xHB_Root )
      ::Set_xHB( s_sHB_Folder, , s_sHB_LibFolder, s_sHB_Exe, s_sHB_IncFolder )
   ENDIF

   #ifdef __PLATFORM__Windows


      IF Empty( ::C_Executable )
         IF ( ! Empty( ::xHB_Root ) ) .AND. File( ::xHB_RootX + "bin\xcc.exe" )
            ::Set_XCC( ::xHB_RootX )
         ELSEIF File( s_sXCCFolder + "\bin\xcc.exe" )
            ::Set_XCC( s_sxCCFolder )
         ELSEIF File( s_sBCCFolder + "\bin\bcc32.exe" )
            ::Set_BCC( s_sBCCFolder )
         ELSEIF File( s_sVCFolder + "\bin\cl.exe" )
            ::Set_VC( s_sVCFolder, , , s_sVC_CL, s_sVC_LIB, s_sVC_LINK )
         ELSEIF File( s_sPOCCFolder + "\bin\pocc.exe" )
            ::Set_POCC( s_sPOCCFolder )
         ELSEIF File( s_sLCCFolder + "\bin\lcc.exe" )
            ::Set_LCC( s_sLCCFolder )
         ELSEIF File( s_sGCCFolder + "\bin\gcc.exe" )
            ::Set_GCC( s_sGCCFolder )
         ELSEIF File( s_sClangFolder + "\bin\clang.exe" )
            ::Set_CLang( s_sCLangFolder )
         ENDIF
      ENDIF

      IF ::lSLY
         IF File( ::xHB_RootX + "bin" + DIR_SEPARATOR + ::SLY_Executable )
            ::SLY_Root := ::xHB_RootX
         ELSEIF File( s_sProgramsFolder + "\GnuWin32\bin\" + ::SLY_Executable )
            ::SLY_Root := s_sProgramsFolder + "\GnuWin32\"
         ELSEIF File( "\GnuWin32\bin\" + ::SLY_Executable )
            ::SLY_Root := DiskName() + ":\GnuWin32\"
         ELSEIF File( "\djgpp\bin\" + ::SLY_Executable )
            ::SLY_Root := DiskName() + ":\djgpp\"
         ELSEIF File( "c:\djgpp\bin\" + ::SLY_Executable )
            ::SLY_Root := "c:\djgpp\"
         ELSE
            Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "Could not locate Bison: " + ::SLY_Executable, HB_aParams() ) )
         ENDIF

         IF Empty( GetEnv( "BISON_SIMPLE" ) )
            IF Empty( GetEnv( "DJGPP" ) )
               IF File( ::SLY_Root + "share\bison\bison.simple" )
                  ::hSLY_EnvVars[ "BISON_SIMPLE" ] := ::SLY_Root + "share\bison\bison.simple"
               ENDIF
            ELSE
               ::hSLY_EnvVars[ "DJGPP" ] := GetEnv( "DJGPP" )
            ENDIF
         ELSE
            ::hSLY_EnvVars[ "BISON_SIMPLE" ] := GetEnv( "BISON_SIMPLE" )
         ENDIF

         ::hSLY_EnvVars[ "PATH" ] := ::SLY_Root + "bin"

         // TODO - Very ODD we MUST run Bison once by itself or subsequent real command will fail.
         CreateProcessWait( ::SLY_Compiler, ::SLY_Executable + " --version", "NUL", HashToEnvVars( ::hSLY_EnvVars ) )
         //__Run( ::SLY_Compiler + " --version > NUL:" )
      ENDIF

      IF ::nType == TYPE_LIB .AND. ! File( ::Librarian )
         Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "Could not locate Librarian at: " + ::Librarian, HB_aParams() ) )
      ENDIF

   #else

      IF Empty( ::C_Executable )
         IF File( s_sGCCFolder + "/bin/gcc" )
            ::Set_GCC( s_sGCCFolder )
         ELSEIF File( s_sCLangFolder + "/bin/clang" )
            ::Set_CLang( s_sCLangFolder )
         ELSEIF File( s_sTCCFolder + "/bin/tcc" )
            ::Set_TCC( s_sTCCFolder )
         ENDIF
      ENDIF

      IF ::lSLY
         IF File( "/usr/bin/" + ::SLY_Executable )
            ::SLY_Root := "/usr/bin"
         ELSE
            Throw( ErrorNew( "xBuild", 0, 1001, Self:ClassName, "Could not locate Bison at: " + ::SLY_Executable, HB_aParams() ) )
         ENDIF

         IF Empty( GetEnv( "BISON_SIMPLE" ) )
            ::hSLY_EnvVars[ "BISON_SIMPLE" ] := GetEnv( "BISON_SIMPLE" )
         ENDIF

         ::hSLY_EnvVars[ "PATH" ] := ::SLY_Root + "bin"

         // TODO - Very ODD we MUST run Bison once by itself or subsequent real command will fail.
         __Run( ::SLY_Compiler + " --version > NUL:" )
      ENDIF

   #endif

   IF ::lPRG .AND. ! File( ::xHB_Compiler )
      Throw( ErrorNew( "xBuild", 0, 1003, Self:ClassName, "Could not locate xHarbour at: " + ::xHB_Compiler, HB_aParams() ) )
   ENDIF

   IF ! File( ::C_Compiler )
      Throw( ErrorNew( "xBuild", 0, 1003, Self:ClassName, "Could not locate C Compiler at: " + ::C_Compiler, HB_aParams() ) )
   ENDIF

   IF ( ::nType == TYPE_EXE .OR. ::nType == TYPE_DLL ) .AND. ! File( ::Linker )
      Throw( ErrorNew( "xBuild", 0, 1003, Self:ClassName, "Could not locate Linker at: " + ::Linker, HB_aParams() ) )
   ENDIF

#ifdef DEFERRED_RC
   IF ::lRC .AND. ! File( ::RC_Binder )
      Throw( ErrorNew( "xBuild", 0, 1003, Self:ClassName, "Could not locate Resource Binder at: " + ::RC_Binder, HB_aParams() ) )
#else
   IF ::lRC .AND. ! File( ::RC_Compiler )
#endif
      Throw( ErrorNew( "xBuild", 0, 1003, Self:ClassName, "Could not locate Resource Compiler at: " + ::RC_Compiler, HB_aParams() ) )
   ENDIF

   IF ::nType == TYPE_LIB .AND. ! File( ::Librarian )
      Throw( ErrorNew( "xBuild", 0, 1003, Self:ClassName, "Could not locate Librarian at: " + ::Librarian, HB_aParams() ) )
   ENDIF

RETURN Self

FUNCTION HashToEnvVars( hEnvVars )

   LOCAL oHash, cEnv

   IF Empty( hEnvVars )
      RETURN Chr(0)
   ELSE
      cEnv := ""
   ENDIF

   FOR EACH oHash IN hEnvVars
      cEnv += oHash:Key + "=" + oHash:Value + Chr(0)
   NEXT

RETURN cEnv

FUNCTION SplitFoldersWithFlag( cFolders, cFlag )

   LOCAL cSplit := "", cFolder

   IF Empty( cFolders )
      RETURN ""
   ENDIF

   FOR EACH cFolder IN HB_aTokens( cFolders, ';', .T. )
      cFolder := AllTrim( cFolder )
      IF cFolder[-1] == DIR_SEPARATOR
         cFolder := Left( cFolder, Len( cFolder ) - 1 )
      ENDIF

      //TraceLog( cFolder )
      cSplit += ( cFlag + '"' + cFolder + '" ' )
   NEXT

RETURN cSplit

FUNCTION GenerateIni( oProject )

   LOCAL cIni

   cIni := "[xHB]" + EOL
   cIni += "Root      = " + oProject:xHB_Root + EOL
   cIni += "LibFolder = " + oProject:xHB_LibFolder + EOL
   cIni += "Flags     = " + oProject:PRG_Flags + EOL
   cIni += "Exe       = " + oProject:xHB_Executable + EOL
   cIni += "IncFolder = " + oProject:xHB_IncFolder + EOL
   cIni += EOL

   SWITCH oProject:C_Executable[1]
      CASE 'b'
         cIni += "[BCC]" + EOL
         EXIT

      CASE 'g'
         #ifdef __PLATFORM__Windows
            cIni += "[MingW]" + EOL
         #else
            cIni += "[GCC]" + EOL
         #endif
         EXIT

      CASE 'c'
         cIni += "[MSVC]" + EOL
         EXIT

      CASE 'p'
         cIni += "[POCC]" + EOL
         EXIT

      CASE 'x'
         cIni += "[xCC]" + EOL
         EXIT
   END

   cIni += "Root          = " + oProject:C_Root + EOL
   cIni += "Compile Flags = " + oProject:C_Flags + EOL
   cIni += "Link Flags    = " + oProject:DefaultLink_Flags + EOL
   cIni += EOL

   cIni += "[FWH]" + EOL
   cIni += "Root      = " + oProject:FWH_Root + EOL
   cIni += "LibFolder = " + oProject:FWH_LibFolder + EOL
   cIni += EOL

   cIni += "[GUI]" + EOL

   IF oProject:GUI_Root == oProject:FWH_Root
      cIni += "Root      = " + oProject:GUI_Root + EOL
      cIni += "LibFolder = " + oProject:GUI_LibFolder + EOL
   ELSE
      cIni += "Root      = " + EOL
      cIni += "LibFolder = " + EOL
   ENDIF

   cIni += EOL

   IF Empty( oProject:cIni )
      #ifdef __PLATFORM__Windows
         MemoWrit( "xbuild.windows.ini", cIni, .F. ) // Added lAddEOF to .F.
      #else
         MemoWrit( "xbuild.linux.ini", cIni )
      #endif
   ELSE
      MemoWrit( oProject:cIni, cIni , .F. ) // Added lAddEOF to .F.
   ENDIF

RETURN .T.

FUNCTION GenerateProjectFile( oProject )

   LOCAL cIni := "", aPropertyAndValues, aPropertyAndValue, Dependancy, lPRG, lC, lRC, lSLY, cType, cStr
   LOCAL LiteralDependancy, nAt, cExt

   IF oProject:ClassName == "TMAKEPROJECT"

      aPropertyAndValues := __ClsGetPropertiesAndValues( oProject )
      aSort( aPropertyAndValues, , , {|a1, a2| a1[1] < a2[1] } )

      FOR EACH aPropertyAndValue IN aPropertyAndValues

         //Alert( aPropertyAndValue[1] )
         //TraceLog( aPropertyAndValue[1] )

         IF aPropertyAndValue[1] == "MYDEFINES"
            aPropertyAndValue[2] := StrTran( SubStr( aPropertyAndValue[2], 3 ), "-D", ";" )
         ENDIF

         cType := ValType( aPropertyAndValue[2] )

         SWITCH cType
            CASE 'C'
               cIni += aPropertyAndValue[1] + " = " + IIF( aPropertyAndValue[2] == NIL, "", CStr( aPropertyAndValue[2] ) )
               IF cIni[-1] == DIR_SEPARATOR
                  cIni[-1] := ' '
               ENDIF

               cIni += EOL
               EXIT

            CASE 'O'
               EXIT

            CASE 'A'
               cIni += aPropertyAndValue[1] + " = "

               FOR EACH cStr IN aPropertyAndValue[2]
                  IF ValType( cStr ) != 'C'
                     cIni += "*UNDEFINED*,"
                  ELSE  
                     cIni += cStr + ","
                  ENDIF   
               NEXT

               cIni[-1] := ' '
               cIni += EOL
               EXIT

            DEFAULT
               cIni += aPropertyAndValue[1] + " = " + IIF( aPropertyAndValue[2] == NIL, "", CStr( aPropertyAndValue[2] ) ) + EOL
         END
      NEXT

      IF ! oProject:lExpand
         oProject:lExpand := .T.
         FOR EACH LiteralDependancy IN oProject:aLiteralDependancies
            IF ( LiteralDependancy HAS "(\?|\*)" )
               oProject:lExpand := .F.
               EXIT
            ENDIF
         NEXT
      ENDIF

      IF oProject:lExpand
         FOR EACH Dependancy IN oProject:aDependancies
            cIni += EOL

            lPRG := .F.
            lC   := .F.
            lRC  := .F.
            lSLY := .F.

            IF Dependancy:nType == TYPE_FROM_C
               lC := .T.

               IF Dependancy:aDependancies[1]:nType == TYPE_FROM_PRG
                  lPrg := .T.
                  cIni += "[" + Dependancy:aDependancies[1]:aDependancies[1]:cFile + "]" + EOL
               ELSEIF Dependancy:aDependancies[1]:nType == TYPE_FROM_SLY
                  lSLY := .T.
                  cIni += "[" + Dependancy:aDependancies[1]:aDependancies[1]:cFile + "]" + EOL
               ELSE
                  cIni += "[" + Dependancy:aDependancies[1]:cFile + "]" + EOL
               ENDIF
            ELSEIF Dependancy:nType == TYPE_FROM_PRG
               lPrg := .T.
               cIni += "[" + Dependancy:aDependancies[1]:cFile + "]" + EOL
            ELSEIF Dependancy:nType == TYPE_FROM_RC
               cIni += "[" + Dependancy:aDependancies[1]:cFile + "]" + EOL
               lRC := .T.
            ELSE
               IF Dependancy:ClassName == "TMAKEPROJECT"
                  cIni += "[" + Dependancy:cFile + ".xbp" + "]" + EOL
               ELSE
                  cIni += "[" + Dependancy:cFile + "]" + EOL
               ENDIF
               LOOP
            ENDIF

            aPropertyAndValues := __ClsGetPropertiesAndValues( Dependancy )
            aSort( aPropertyAndValues, , , {|a1, a2| a1[1] < a2[1] } )

            FOR EACH aPropertyAndValue IN aPropertyAndValues
               IF ! lRC
                  IF aPropertyAndValue[1] == "MYRC_FLAGS"
                     LOOP
                  ENDIF
               ENDIF

               IF !lSLY
                  IF aPropertyAndValue[1] == "MYSLY_FLAGS"
                     LOOP
                  ENDIF
               ENDIF

               IF ! lC .AND. ! lPRG
                  IF aPropertyAndValue[1] == "MYC_FLAGS"
                     LOOP
                  ENDIF
               ENDIF

               IF ! lPRG
                  IF aPropertyAndValue[1] == "MYPRG_FLAGS"
                     LOOP
                  ENDIF
               ENDIF

               IF aPropertyAndValue[1] == "MYDEFINES"
                  aPropertyAndValue[2] := StrTran( SubStr( aPropertyAndValue[2], 3 ), "-D", ";" )
               ENDIF

               cType := ValType( aPropertyAndValue[2] )

               SWITCH cType
                  CASE 'C'
                     cIni += aPropertyAndValue[1] + " = " + IIF( aPropertyAndValue[2] == NIL, "", CStr( aPropertyAndValue[2] ) )
                     IF cIni[-1] == DIR_SEPARATOR
                        cIni[-1] := ' '
                     ENDIF

                     cIni += EOL
                     EXIT

                  CASE 'O'
                     EXIT

                  CASE 'A'
                     cIni += aPropertyAndValue[1] + " = "

                     FOR EACH cStr IN aPropertyAndValue[2]
                        IF ValType( cStr ) != 'C'
                           cIni += "*UNDEFINED*,"
                        ELSE  
                           cIni += cStr + ","
                        ENDIF   
                     NEXT

                     cIni[-1] := ' '
                     cIni += EOL
                     EXIT

                  DEFAULT
                     cIni += aPropertyAndValue[1] + " = " + IIF( aPropertyAndValue[2] == NIL, "", CStr( aPropertyAndValue[2] ) ) + EOL
               END
            NEXT
         NEXT
      ELSE
         FOR EACH LiteralDependancy IN oProject:aLiteralDependancies
            cIni += EOL
            cIni += "[" + LiteralDependancy + "]" + EOL

            nAt := RAt( '.', LiteralDependancy )
            IF nAt > 0
               cExt := Lower( SubStr( LiteralDependancy, nAt + 1 ) )

               DO CASE
                  CASE cExt == "prg"
                     cIni += "MYC_FLAGS =" + EOL
                     cIni += "MYDEFINES =" + EOL
                     cIni += "MYPRG_FLAGS =" + EOL

                  CASE cExt == "c"
                     cIni += "MYC_FLAGS =" + EOL
                     cIni += "MYDEFINES =" + EOL

                  CASE cExt == "rc"
                     cIni += "MYDEFINES =" + EOL
                     cIni += "MYRC_FLAGS =" + EOL

                  CASE cExt == "sly"
                     cIni += "MYC_FLAGS =" + EOL
                     cIni += "MYDEFINES =" + EOL
                     cIni += "MYSLY_FLAGS =" + EOL

                  CASE cExt == "y"
                     cIni += "MYC_FLAGS =" + EOL
                     cIni += "MYDEFINES =" + EOL
                     cIni += "MYSLY_FLAGS =" + EOL
               ENDCASE
            ENDIF
         NEXT
      ENDIF
   ELSE
      RETURN .F.
   ENDIF

   MemoWrit( oProject:cFile + ".xbp", cIni , .F. ) // Added lAddEOF to .F.

RETURN .T.

FUNCTION Find_xHarbour( xHB_Root, cExe, cLibFolder )
   
  #ifdef __PLATFORM__Windows
   LOCAL cCurrent := DiskName() 
   //LOCAL cDrives := ""
   LOCAL c, sExeFolder, nAt

   sExeFolder := GetModuleFileName()
   nAt        := RAt( '\', sExeFolder )

   IF File( sExeFolder + "xhb.exe" )
      xHB_Root := Pad( Left( sExeFolder, Len( sExeFolder ) - 4 ), 128 )
      cExe     := "xhb.exe"
      RETURN .T.
   ELSEIF File( "\xhb\bin\xhb.exe" )
      xHB_Root := Pad( DiskName() + DRIVE_SEPARATOR + "\xhb\", 128 ) 
      cExe     := "xhb.exe"
      RETURN .T.
   ENDIF

   FOR c := 'C' TO 'Z'
      IF DiskChange( c )
         IF File( c + ":\xhb\bin\xhb.exe" )
            xHB_Root := Pad( c + ":\xhb\", 128 )
            cExe     := "xhb.exe"

            EXIT
         ENDIF
      ENDIF
   NEXT

   IF Empty( xHB_Root )
      IF File( "\xharbour\bin\harbour.exe" )
         xHB_Root := Pad( DiskName() + DRIVE_SEPARATOR + "\xharbour\", 128 ) 
         cExe     := "harbour.exe"

         RETURN .T.
      ENDIF

      FOR c := 'C' TO 'Z'
         IF DiskChange( c )
            IF File( c + ":\xharbour\bin\harbour.exe" )
               xHB_Root := Pad( c + ":\xharbour\", 128 )
               cExe     := "harbour.exe"

               EXIT
            ENDIF
         ENDIF
      NEXT
   ENDIF

   DiskChange( cCurrent )
  #else
   IF File( "/usr/bin/xhb" )
      xHB_Root   := Pad( "/usr/" , 128 )
      cExe       := "xhb"
      RETURN .T.
   ELSEIF File( "/usr/bin/harbour" )
      xHB_Root   := Pad( "/usr/", 128 )
      cExe       := "harbour"
      RETURN .T.
   ELSEIF File( "/opt/harbour/bin/harbour" )
      xHB_Root   := Pad( "/opt/harbour/", 128 )
      cExe       := "harbour"
      cLibFolder := Pad( "lib/harbour/", 128 )
      RETURN .T.
   ENDIF
  #endif

RETURN .F.

FUNCTION Validate_xHB( xHB_Root, xHB_LibFolder, xHB_IncFolder )

   xHB_Root      := ExpandEnvVars( AllTrim( xHB_Root ) )
   xHB_LibFolder := ExpandEnvVars( AllTrim( xHB_LibFolder ) )
   xHB_LibFolder := ExpandEnvVars( AllTrim( xHB_IncFolder ) )

#ifdef __PLATFORM__Windows
   IF ( ! File( xHB_Root + "bin\xhb.exe" ) ) .AND. ( ! File( xHB_Root + "bin\harbour.exe" ) )
      Alert( "Couldn't find PRG Compiler at: '" + xHB_Root + "bin'" )
      
      xHB_Root      := Pad( xHB_Root, 128 )
      xHB_LibFolder := Pad( xHB_LibFolder, 128 )
      xHB_IncFolder := Pad( xHB_IncFolder, 128 )
      RETURN .F.
   ENDIF

   IF ( ! File( xHB_Root + xHB_LibFolder + "xhb.lib" ) ) .AND. ( ! File( xHB_Root + xHB_LibFolder + "rtl.lib" ) )
      Alert( "Couldn't find R/T support' at: '" +  xHB_Root + xHB_LibFolder + "'" )
      
      xHB_Root      := Pad( xHB_Root, 128 )
      xHB_LibFolder := Pad( xHB_LibFolder, 128 )
      xHB_IncFolder := Pad( xHB_IncFolder, 128 )
      RETURN .F.
   ENDIF
#else
   IF ( ! File( xHB_Root + "bin/xhb" ) ) .AND. ( ! File( xHB_Root + "bin/harbour" ) )
      Alert( "Couldn't find PRG Compiler at: '" + xHB_Root + "bin'" )

      xHB_Root      := Pad( xHB_Root, 128 )
      xHB_LibFolder := Pad( xHB_LibFolder, 128 )
      xHB_IncFolder := Pad( xHB_IncFolder, 128 )
      RETURN .F.
   ENDIF

   IF ( ! File( xHB_Root + xHB_LibFolder + "libxhb.a" ) ) .AND. ( ! File( xHB_Root + xHB_LibFolder + "librtl.a" ) )
      Alert( "Couldn't find R/T support' at: '" +  xHB_Root + xHB_LibFolder + "'" )

      xHB_Root      := Pad( xHB_Root, 128 )
      xHB_LibFolder := Pad( xHB_LibFolder, 128 )
      xHB_IncFolder := Pad( xHB_IncFolder, 128 )
      RETURN .F.
   ENDIF
#endif

RETURN .T.

FUNCTION Find_CCompiler( C_Compiler, C_Root )

   LOCAL lFound := .F.
#ifdef __PLATFORM__Windows
   LOCAL cCurrent := DiskName(), c
   //LOCAL cDrives := ""
#endif   

   C_Compiler := Upper( AllTrim( C_Compiler ) )
   C_Root     := ExpandEnvVars( AllTrim( C_Root ) )

   DO CASE
#ifdef __PLATFORM__Windows
      CASE C_Compiler == "BCC"
         IF File( s_sBCCFolder + "\bin\bcc32.exe" )
            C_Root := Pad( s_sBCCFolder, 128 )

            RETURN .T.
         ENDIF

         FOR c := 'C' TO 'Z'
            IF DiskChange( c )
               IF File( c + ":\bcc55\bin\bcc32.exe" )
                  s_sBCCFolder := c + ":\bcc55"
                  C_Root := Pad( s_sBCCFolder, 128 )

                  lFound := .T.
                  EXIT
               ELSEIF File( c + ":\borland\bcc55\bin\bcc32.exe" )
                  s_sBCCFolder := c + ":\borland\bcc55"
                  C_Root := Pad( s_sBCCFolder, 128 ) 

                  lFound := .T.
                  EXIT
               ENDIF
            ENDIF
         NEXT
#endif

      CASE C_Compiler == "CLANG"

         #ifdef __PLATFORM__Windows
            IF File( s_sCLangFolder + "\bin\clang.exe" )
               C_Root := Pad( s_sCLangFolder, 128 )
               RETURN .T.
            ENDIF
         #else
            IF File( s_sCLangFolder + "/bin/clang" )
               C_Root := Pad( s_sCLangFolder, 128 )
               RETURN .T.
            ENDIF
         #endif

      CASE C_Compiler == "GCC"

         #ifdef __PLATFORM__Windows
            IF File( s_sGCCFolder + "\bin\gcc.exe" )
               C_Root := Pad( s_sGCCFolder, 128 )
               RETURN .T.
            ENDIF
         #else
            IF File( s_sGCCFolder + "/bin/gcc" )
               C_Root := Pad( s_sGCCFolder, 128 )
               RETURN .T.
            ENDIF
         #endif


#ifdef __PLATFORM__Windows
      CASE C_Compiler == "LCC"
         IF File( s_sLCCFolder + "\bin\lcc.exe" )
            C_Root := Pad( s_sLCCFolder, 128 )

            RETURN .T.
         ENDIF

         FOR c := 'C' TO 'Z'
            IF DiskChange( c )
               IF File( c + ":\lcc\bin\lcc.exe" )
                  s_sLCCFolder := c + ":\lcc"
                  C_Root := Pad( s_sLCCFolder, 128 )

                  lFound := .T.
                  EXIT
               ENDIF
            ENDIF
         NEXT
#endif

#ifdef __PLATFORM__Windows
      CASE C_Compiler == "MINGW"
         IF File( s_sGCCFolder + "\bin\gcc.exe" )
            C_Root := Pad( DiskName() + DRIVE_SEPARATOR + "\cygwin\", 128 )

            RETURN .T.
         ENDIF

         FOR c := 'C' TO 'Z'
            IF DiskChange( c )
               IF File( c + ":\cygwin\bin\gcc.exe" )
                  s_sGCCFolder := c + ":\cygwin"
                  C_Root := Pad( s_sGCCFolder, 128 )

                  lFound := .T.
                  EXIT
               ENDIF
            ENDIF
         NEXT
#endif

#ifdef __PLATFORM__Windows
      CASE C_Compiler == "MSVC"
         IF File( s_sVCFolder + "\bin\cl.exe" )
            C_Root := Pad( s_sVCFolder, 128 )
           RETURN .T.
         ENDIF
#endif

      CASE C_Compiler == "TCC"

         #ifdef __PLATFORM__Windows
            IF File( s_sTCColder + "\bin\tcc.exe" )
               C_Root := Pad( s_sTCCFolder, 128 )

               RETURN .T.
            ENDIF
         #else
            IF File( s_sTCCFolder + "/bin/tcc" )
               C_Root := Pad( s_sTCCFolder, 128 )

               RETURN .T.
            ENDIF
         #endif

   ENDCASE

#ifdef __PLATFORM__Windows
   DiskChange( cCurrent )
#endif

//Unused var
( lFound )

// Always return .T. so as to free the user to correct the C_Root!
RETURN .T.

FUNCTION Validate_CCompiler( C_Compiler, C_Root )

   LOCAL C_Exe

   C_Compiler := Upper( AllTrim( C_Compiler ) )
   C_Root     := ExpandEnvVars( AllTrim( C_Root ) )

   IF C_Root[-1] != DIR_SEPARATOR
      C_Root += DIR_SEPARATOR
   ENDIF

   DO CASE
      CASE C_Compiler == "BCC"
         C_Exe := "bcc32.exe"

      CASE C_Compiler == "GCC"
         #ifdef __PLATFORM__Windows
            C_Exe := "gcc.exe"
         #else
            C_Exe := "gcc"
         #endif

      CASE C_Compiler == "LCC"
         C_Exe := "lcc.exe"

      CASE C_Compiler == "MINGW "
         C_Exe := "gcc.exe"

      CASE C_Compiler == "MSVC"
         C_Exe := "cl.exe"

      CASE C_Compiler == "CLANG"
         #ifdef __PLATFORM__Windows
            C_Exe := "clang.exe"
         #else
            C_Exe := "clang"
         #endif

      CASE C_Compiler == "TCC"
         #ifdef __PLATFORM__Windows
            C_Exe := "tcc.exe"
         #else
            C_Exe := "tcc"
         #endif

   ENDCASE

   IF ! File( C_Root + "bin" + DIR_SEPARATOR + C_Exe )
      Alert( "Couldn't locate C Compiler '" + C_Exe + "' at: '" + C_Root + "bin'" )
      RETURN .F.
   ENDIF

RETURN .T.

FUNCTION Find_FWH( FWH_Root )

   LOCAL cCurrent := DiskName(), c
   //cDrives := ""

   // TODO Linux!!!

   IF File( "\fwh\include\fivewin.ch" )
      FWH_Root := Pad( DiskName() + DRIVE_SEPARATOR + "\fwh\", Len( FWH_Root ) )

      RETURN .T.
   ENDIF

   FOR c := 'C' TO 'Z'
      IF DiskChange( c )
         IF File( c + ":\fwh\include\fivewin.ch" )
            FWH_Root := Pad( c + ":\fwh\", Len( FWH_Root ) )
            EXIT
         ENDIF
      ENDIF
   NEXT

   DiskChange( cCurrent )

RETURN .T.

FUNCTION Validate_FWH( FWH_Root, FWH_LibFolder )

   FWH_Root := ExpandEnvVars( AllTrim( FWH_Root ) )
   FWH_LibFolder := ExpandEnvVars( AllTrim( FWH_LibFolder ) )

   IF FWH_Root[-1] != DIR_SEPARATOR
      FWH_Root += DIR_SEPARATOR
   ENDIF

   IF Empty( FWH_LibFolder )
      FWH_LibFolder := "lib\"
   ENDIF

   IF ! File( FWH_Root + "include" + DIR_SEPARATOR + "FiveWin.ch" )
      Alert( "Couldn't find 'FiveWin.ch' at: '" +  FWH_Root + "include'" )
      RETURN .F.
   ENDIF

   IF ! File( FWH_Root + FWH_LibFolder + DIR_SEPARATOR + "FiveHC.lib" )
      Alert( "Couldn't find 'FiveHC.lib' at: '" +  FWH_Root + FWH_LibFolder + "'" )
      RETURN .F.
   ENDIF

RETURN .T.

FUNCTION MakeNestedDir( cPath )

   LOCAL nAt, nStart := 1, cCurrentFolder := DIR_SEPARATOR + CurDir(), cNewFolder

   IF cPath[1] = DIR_SEPARATOR
      IF DirChange( DIR_SEPARATOR ) != 0
         IF DirChange( cCurrentFolder ) != 0
            TraceLog( "OOps!", cCurrentFolder, cNewFolder )
         ENDIF
         Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Could not switch to folder: " + cNewFolder, HB_aParams() ) )
      ENDIF

      cPath := Substr( cPath, 2 )
   ENDIF

   WHILE ( nAt := At( DIR_SEPARATOR, cPath, nStart ) ) > 1
      cNewFolder := SubStr( cPath, nStart, nAt - nStart )

      IF Len( Directory( cNewFolder, 'D' ) ) == 1
         IF DirChange( cNewFolder ) != 0
            IF DirChange( cCurrentFolder ) != 0
               TraceLog( "OOps!", cCurrentFolder, cNewFolder )
            ENDIF
            Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Could not switch to folder: " + cNewFolder, HB_aParams() ) )
         ENDIF

         nStart := nAt + 1
         LOOP
      ENDIF

      IF MakeDir( cNewFolder ) == 0
         IF DirChange( cNewFolder ) != 0
            IF DirChange( cCurrentFolder ) != 0
               TraceLog( "OOps!", cCurrentFolder, cNewFolder )
            ENDIF
            Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Could not switch to folder: " + cNewFolder, HB_aParams() ) )
         ENDIF
      ELSE
         IF DirChange( cCurrentFolder ) != 0
            TraceLog( "OOps!", cCurrentFolder, cNewFolder )
         ENDIF

         Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Could not create folder: " + cNewFolder, HB_aParams() ) )
      ENDIF

      nStart := nAt + 1
   ENDDO

   IF nStart < Len( cPath )
      cNewFolder := SubStr( cPath, nStart )

      IF Len( Directory( cNewFolder, 'D' ) ) != 1
         IF  MakeDir( cNewFolder ) != 0
            IF DirChange( cCurrentFolder ) != 0
               TraceLog( "OOps!", cCurrentFolder, cNewFolder )
            ENDIF
            Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "Could not create folder: " + cNewFolder, HB_aParams() ) )
         ENDIF
      ENDIF
   ENDIF

   IF DirChange( cCurrentFolder ) != 0
      TraceLog( "OOps!", cCurrentFolder, cNewFolder )
   ENDIF

RETURN 0

FUNCTION ExpandEnvVars( cString )

   STATIC s_reEnvVar
   LOCAL cMatch, nStart, nLen, cVar

   IF s_reEnvVar == NIL
      s_reEnvVar := HB_RegexComp( "(?<=\$\()" + "\w+" + "(?=\))" )
   ENDIF

   cMatch := HB_AtX( s_reEnvVar, cString, , @nStart, @nLen )

   WHILE ! Empty( cMatch )
      TRY
         cVar := s_hEnvVars[ cMatch ]
      CATCH
         cVar := GetEnv( cMatch )
      END
      //? cMatch, nStart, nLen, cVar

      cString := Left( cString, nStart - 3 ) + cVar + SubStr( cString, nStart + nLen + 1 )

      nStart += Len( cVar ) - 2
      nLen := 0

      cMatch := HB_AtX( s_reEnvVar, cString, , @nStart, @nLen )
   END

RETURN cString

FUNCTION RelativeToAbsolutePath( cPath, cRoot )

    LOCAL bRelative := .F.
    LOCAL cFolder := AllTrim( cPath )
    LOCAL nAt

    WHILE cFolder[1] == '.' .AND. cFolder[2] == DIR_SEPARATOR
       bRelative := .T.
       cFolder := LTrim( SubStr( cFolder, 3 ) )
    END

    IF cRoot[-1] == DIR_SEPARATOR
       cRoot := Left( cRoot, Len( cRoot ) - 1 )
    ENDIF

    WHILE cFolder[1] == '.' .AND. cFolder[2] == '.' .AND. cFolder[3] == DIR_SEPARATOR
       bRelative := .T.
       cFolder := LTrim( SubStr( cFolder, 4 ) )

       nAt := RAt( DIR_SEPARATOR, cRoot )
       IF nAt == 0
          Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "invalid relative specification: " + cPath, HB_aParams() ) )
       ELSE
          cRoot := Left( cRoot, nAt - 1 )
       ENDIF
    END

    cRoot += DIR_SEPARATOR

    IF bRelative
       cFolder := cRoot + cFolder
    ELSE
       IF cFolder[1] != '\' .AND. cFolder[2] != DRIVE_SEPARATOR
          cFolder := cRoot + cFolder
       ENDIF
    ENDIF

    //Alert( cPath + "," + cRoot + "," + cFolder )

RETURN cFolder

INIT PROCEDURE InitTProject
   
   LOCAL sExeFolder, nAt   

   #ifdef __PLATFORM__Windows
      LOCAL sUniversalCRTSdkDir, sUCRTVersion
      LOCAL cVSPAth:="", cVCVer:=""
   #endif

   HSetCaseMatch( s_hEnvVars, .F. )

   #ifdef __PLATFORM__Windows
      sExeFolder := GetModuleFileName()
          
      nAt := RAt( "\", sExeFolder )
      sExeFolder := Left( sExeFolder, nAt )

      IF File( sExeFolder + "xhb.exe" ) .AND. Lower( Right( sExeFolder, 5 ) ) == "\bin\"
         s_lX            := .T.
         s_sHB_Exe       := "xhb.exe"
         s_sHB_Folder    := Left( sExeFolder, Len( sExeFolder ) - 4 )
         s_sHB_LibFolder := s_sHB_Folder + "lib\"
         s_sHB_IncFolder := s_sHB_Folder + "include\"
      ELSEIF File( "\xharbour\bin\xhb.exe" )
         s_lX            := .T.
         s_sHB_Exe       := "xhb.exe"
         s_sHB_Folder    := "\xharbour\"   
         s_sHB_LibFolder := s_sHB_Folder + "lib\"
         s_sHB_IncFolder := s_sHB_Folder + "include\"
      ELSEIF File( "\xharbour\bin\harbour.exe" )
         s_lX            := .T.
         s_sHB_Folder    := "\xharbour\"   
         s_sHB_LibFolder := s_sHB_Folder + "lib\"
         s_sHB_IncFolder := s_sHB_Folder + "include\"
      ELSEIF File( "\harbour\bin\harbour.exe" )
         s_lX            := .F.
         s_sHB_Exe       := "harbour.exe"
         s_sHB_Folder    := "\harbour\"   
         s_sHB_LibFolder := s_sHB_Folder + "lib\harbour\lib\"
         s_sHB_IncFolder := s_sHB_Folder + "include\harbour\include\"
      ENDIF      

      s_sHostArch = GetEnv( "PROCESSOR_ARCHITECTURE" )
   
      s_sProgramsFolder := GetEnv( "ProgramFiles(x86)" )
      IF Empty( s_sProgramsFolder ) .OR. ( ! IsDirectory( s_sProgramsFolder ) )
         s_sProgramsFolder := GetEnv( "ProgramFiles" )
      ENDIF
      
      IF Empty( s_sProgramsFolder ) .OR. ( ! IsDirectory( s_sProgramsFolder ) )
         DO CASE
            CASE IsDirectory( "C:\Program Files (x86)" )
               s_sProgramsFolder := "C:\Program Files (x86)"

            CASE IsDirectory( "C:\Program Files" )
               s_sProgramsFolder := "C:\Program Files"

            CASE IsDirectory( "\Program Files" )
               s_sProgramsFolder := DiskName() + ":\Program Files"

            OTHERWISE
               s_sProgramsFolder := ""
         ENDCASE
      ENDIF

      IF ! Empty( s_sProgramsFolder )
         DO CASE
            CASE File( s_sProgramsFolder + "\Windows Kits\10\Include\10.0.15063.0\um\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Windows Kits\10\Include\10.0.15063.0\um" + ";" +  ;
                                          s_sProgramsFolder + "\Windows Kits\10\Include\10.0.15063.0\shared"
                                          
               IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\bin\10.0.15063.0\" + s_sHostArch )
                  s_sWinSDKBinFolder := s_sProgramsFolder + "\Windows Kits\10\bin\10.0.15063.0\" + s_sHostArch 
               ENDIF
               IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\Lib\10.0.15063.0\um\" + s_sHostArch )
                  s_sWinSDKLibFolder := s_sProgramsFolder + "\Windows Kits\10\Lib\10.0.15063.0\um\" + s_sHostArch 
               ENDIF
               
            CASE File( s_sProgramsFolder + "\Windows Kits\10\Include\10.0.22621.0\um\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Windows Kits\10\Include\10.0.22621.0\um" + ";" +  ;
                                          s_sProgramsFolder + "\Windows Kits\10\Include\10.0.22621.0\shared"
                                          
               IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\bin\10.0.22621.0\" + s_sHostArch )
                  s_sWinSDKBinFolder := s_sProgramsFolder + "\Windows Kits\10\bin\10.0.22621.0\" + s_sHostArch 
               ENDIF
               IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\Lib\10.0.22621.0\um\" + s_sHostArch )
                  s_sWinSDKLibFolder := s_sProgramsFolder + "\Windows Kits\10\Lib\10.0.22621.0\um\" + s_sHostArch 
               ENDIF			
            CASE File( s_sProgramsFolder + "\Windows Kits\10\Include\10.0.22000.0\um\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Windows Kits\10\Include\10.0.22000.0\um" + ";" +  ;
                                          s_sProgramsFolder + "\Windows Kits\10\Include\10.0.22000.0\shared"
                                          
               IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\bin\10.0.22000.0\" + s_sHostArch )
                  s_sWinSDKBinFolder := s_sProgramsFolder + "\Windows Kits\10\bin\10.0.22000.0\" + s_sHostArch 
               ENDIF
               IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\Lib\10.0.22000.0\um\" + s_sHostArch )
                  s_sWinSDKLibFolder := s_sProgramsFolder + "\Windows Kits\10\Lib\10.0.22000.0\um\" + s_sHostArch 
               ENDIF			

            CASE File( s_sProgramsFolder + "\Windows Kits\10\Include\10.0.1904.0\um\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Windows Kits\10\Include\10.0.1904.0\um" + ";" +  ;
                                          s_sProgramsFolder + "\Windows Kits\10\Include\10.0.1904.0\shared"
                                          
               IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\bin\10.0.1904.0\" + s_sHostArch )
                  s_sWinSDKBinFolder := s_sProgramsFolder + "\Windows Kits\10\bin\10.0.1904.0\" + s_sHostArch 
               ENDIF
               IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\Lib\10.0.1904.0\um\" + s_sHostArch )
                  s_sWinSDKLibFolder := s_sProgramsFolder + "\Windows Kits\10\Lib\10.0.1904.0\um\" + s_sHostArch 
               ENDIF			

            CASE File( s_sProgramsFolder + "\Windows Kits\10\Include\10.0.18362.0\um\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Windows Kits\10\Include\10.0.18362.0\um" + ";" +  ;
                                          s_sProgramsFolder + "\Windows Kits\10\Include\10.0.18362.0\shared"
                                          
               IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\bin\10.0.18362.0\" + s_sHostArch )
                  s_sWinSDKBinFolder := s_sProgramsFolder + "\Windows Kits\10\bin\10.0.18362.0\" + s_sHostArch 
               ENDIF
               IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\Lib\10.0.18362.0\um\" + s_sHostArch )
                  s_sWinSDKLibFolder := s_sProgramsFolder + "\Windows Kits\10\Lib\10.0.18362.0\um\" + s_sHostArch 
               ENDIF			
            CASE File( s_sProgramsFolder + "\Windows Kits\10\Include\10.0.17434.0\um\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Windows Kits\10\Include\10.0.17434.0\um" + ";" +  ;
                                          s_sProgramsFolder + "\Windows Kits\10\Include\10.0.17434.0\shared"
                                          
               IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\bin\10.0.17434.0\" + s_sHostArch )
                  s_sWinSDKBinFolder := s_sProgramsFolder + "\Windows Kits\10\bin\10.0.17434.0\" + s_sHostArch 
               ENDIF	
               IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\Lib\10.0.17434.0\um\" + s_sHostArch )
                  s_sWinSDKLibFolder := s_sProgramsFolder + "\Windows Kits\10\Lib\10.0.17434.0\um\" + s_sHostArch 
               ENDIF						   
            CASE File( s_sProgramsFolder + "\Windows Kits\8.1\Include\um\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Windows Kits\8.1\Include\um" + ";" + ;
                                          s_sProgramsFolder + "\Windows Kits\8.1\Include\shared"
               IF IsDirectory( s_sProgramsFolder + "\Windows Kits\8.1\bin\" + s_sHostArch )
                  s_sWinSDKBinFolder := s_sProgramsFolder + "\Windows Kits\8.1\bin\" + s_sHostArch
               ENDIF
               IF IsDirectory( s_sProgramsFolder + "\Windows Kits\8.1\Lib\winv6.3\um\" + s_sHostArch )
                  s_sWinSDKLibFolder := s_sProgramsFolder + "\Windows Kits\8.1\Lib\winv6.3\um\" + s_sHostArch
               ENDIF
               
            CASE File( s_sProgramsFolder + "\Microsoft SDKs\Windows\v10.0A\Include\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Microsoft SDKs\Windows\v10.0A\include"       
               s_sWinSDKLibFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v10.0A\lib"
               s_sWinSDKBinFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v10.0A\bin"
               
            CASE File( s_sProgramsFolder + "\Microsoft SDKs\Windows\v8.1A\Include\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Microsoft SDKs\Windows\v8.1A\include"
               s_sWinSDKLibFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v8.1A\lib"
               s_sWinSDKBinFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v8.1A\bin"

            CASE File( s_sProgramsFolder + "\Microsoft SDKs\Windows\v8.1\Include\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Microsoft SDKs\Windows\v8.1\include"
               s_sWinSDKLibFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v8.1\lib"
               s_sWinSDKBinFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v8.1\bin"

            CASE File( s_sProgramsFolder + "\Microsoft SDKs\Windows\v7.1A\Include\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Microsoft SDKs\Windows\v7.1A\include"
               s_sWinSDKLibFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v7.1A\lib"
               s_sWinSDKBinFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v7.1A\bin"

            CASE File( s_sProgramsFolder + "\Microsoft SDKs\Windows\v7.1\Include\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Microsoft SDKs\Windows\v7.1\include"
               s_sWinSDKLibFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v7.1\lib"
               s_sWinSDKBinFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v7.1\bin"

            CASE File( s_sProgramsFolder + "\Microsoft SDKs\Windows\v7.0A\Include\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Microsoft SDKs\Windows\v7.0A\include"
               s_sWinSDKLibFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v7.0A\lib"
               s_sWinSDKBinFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v7.0A\bin"

            CASE File( s_sProgramsFolder + "\Microsoft SDKs\Windows\v7.0\Include\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Microsoft SDKs\Windows\v7.0\include"
               s_sWinSDKLibFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v7.0\lib"
               s_sWinSDKBinFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v7.0\bin"

            CASE File( s_sProgramsFolder + "\Microsoft SDKs\Windows\v6.1A\Include\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Microsoft SDKs\Windows\v6.1A\include"
               s_sWinSDKLibFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v6.1A\lib"
               s_sWinSDKBinFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v6.1A\bin"

            CASE File( s_sProgramsFolder + "\Microsoft SDKs\Windows\v6.1\Include\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Microsoft SDKs\Windows\v6.1\include"
               s_sWinSDKLibFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v6.1\lib"
               s_sWinSDKBinFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v6.1\bin"

            CASE File( s_sProgramsFolder + "\Microsoft SDKs\Windows\v6.0A\Include\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Microsoft SDKs\Windows\v6.0A\include"
               s_sWinSDKLibFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v6.0A\lib"
               s_sWinSDKBinFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v6.0A\bin"

            CASE File( s_sProgramsFolder + "\Microsoft SDKs\Windows\v6.0\Include\windows.h" )
               s_sWinSDKIncludeFolders := s_sProgramsFolder + "\Microsoft SDKs\Windows\v6.0\include"
               s_sWinSDKLibFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v6.0\lib"
               s_sWinSDKBinFolder      := s_sProgramsFolder + "\Microsoft SDKs\Windows\v6.0\bin"

         ENDCASE
      ENDIF

      IF File( "\xhb\bin\xcc.exe" )
         s_sxCCFolder := DiskName() + DRIVE_SEPARATOR + "\xhb"
      ELSEIF File( "c:\xhb\bin\xcc.exe" )
         s_sxCCFolder := "c:\xhb"
      ENDIF

      IF File( s_sProgramsFolder + "\Borland\BDS\4.0\bin\bcc32.exe" )
         s_sBCCFolder := s_sProgramsFolder + "\Borland\BDS\4.0\"
      ELSEIF File( "\bcc55\bin\bcc32.exe" )
         s_sBCCFolder := DiskName() + DRIVE_SEPARATOR + "\bcc55"
      ELSEIF File( "c:\bcc55\bin\bcc32.exe" )
         s_sBCCFolder := "c:\bcc55"
      ELSEIF File( "\borland\bcc55\bin\bcc32.exe" )
         s_sBCCFolder := DiskName() + DRIVE_SEPARATOR + "\borland\bcc55"
      ELSEIF File( "c:\borland\bcc55\bin\bcc32.exe" )
         s_sBCCFolder := "c:\borland\bcc55"
      ENDIF

/*TODO:
//vs71comntools
//vs80comntools
//vs100comntools
//vs120comntools
//vs140comntools
*/

      sUniversalCRTSdkDir := GetEnv( "UniversalCRTSdkDir" )
      IF Empty( sUniversalCRTSdkDir )
         IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\Include\10.0.15063.0\ucrt" )
            s_sUniversalCRT_IncludePath := s_sProgramsFolder + "\Windows Kits\10\Include\10.0.15063.0\ucrt"
            s_sUniversalCRT_LibraryPath_x86 := s_sProgramsFolder + "\Windows Kits\10\lib\10.0.15063.0\ucrt\x86"
         ENDIF         
         IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\Include\10.0.22621.0\ucrt" )
            s_sUniversalCRT_IncludePath := s_sProgramsFolder + "\Windows Kits\10\Include\10.0.22621.0\ucrt"
            s_sUniversalCRT_LibraryPath_x86 := s_sProgramsFolder + "\Windows Kits\10\lib\10.0.22621.0\ucrt\x86"
         ENDIF     
         IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\Include\10.0.22000.0\ucrt" )
            s_sUniversalCRT_IncludePath := s_sProgramsFolder + "\Windows Kits\10\Include\10.0.22000.0\ucrt"
            s_sUniversalCRT_LibraryPath_x86 := s_sProgramsFolder + "\Windows Kits\10\lib\10.0.22000.0\ucrt\x86"
         ENDIF     

         IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\Include\10.0.1904.0\ucrt" )
            s_sUniversalCRT_IncludePath := s_sProgramsFolder + "\Windows Kits\10\Include\10.0.1904.0\ucrt"
            s_sUniversalCRT_LibraryPath_x86 := s_sProgramsFolder + "\Windows Kits\10\lib\10.0.1904.0\ucrt\x86"
         ENDIF     

         IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\Include\10.0.18362.0\ucrt" )
            s_sUniversalCRT_IncludePath := s_sProgramsFolder + "\Windows Kits\10\Include\10.0.18362.0\ucrt"
            s_sUniversalCRT_LibraryPath_x86 := s_sProgramsFolder + "\Windows Kits\10\lib\10.0.18362.0\ucrt\x86"
         ENDIF     

         IF IsDirectory( s_sProgramsFolder + "\Windows Kits\10\Include\10.0.17434.0\ucrt" )
            s_sUniversalCRT_IncludePath := s_sProgramsFolder + "\Windows Kits\10\Include\10.0.17434.0\ucrt"
            s_sUniversalCRT_LibraryPath_x86 := s_sProgramsFolder + "\Windows Kits\10\lib\10.0.17434.0\ucrt\x86"
         ENDIF     
		 
      ELSEIF IsDirectory( sUniversalCRTSdkDir )
         sUCRTVersion := GetEnv( "UCRTVersion" )
         IF IsDirectory(  sUniversalCRTSdkDir + "include\" + sUCRTVersion + "\ucrt" )
            s_sUniversalCRT_IncludePath := sUniversalCRTSdkDir + "include\" + sUCRTVersion + "\ucrt"
         ENDIF
         IF IsDirectory( sUniversalCRTSdkDir + "lib\" + sUCRTVersion + "\ucrt\x86" )
            s_sUniversalCRT_LibraryPath_x86 := sUniversalCRTSdkDir + "lib\" + sUCRTVersion + "\ucrt\x86"
         ENDIF
      ENDIF
           
      //TraceLog(sUniversalCRTSdkDir, sUCRTVersion, s_sUniversalCRT_LibraryPath_x86)
  
      IF File( s_sProgramsFolder + "\Microsoft Visual Studio\Installer\vswhere.exe" )   
         __Run( '"' + s_sProgramsFolder + '\Microsoft Visual Studio\Installer\vswhere.exe" -latest -legacy -property InstallationPath > $VS-latest-path_.txt' )
      
         cVSPath := MemoRead( "$VS-latest-path_.txt" ) ; FErase( "$VS-latest-path_.txt" )
         cVSPath := Left( cVSPath, Len(cVSPath ) - 2 )
         
         IF File( cVSPath + "\VC\Auxiliary\Build\Microsoft.VCToolsVersion.default.txt" )  
            cVCVer := MemoRead(  cVSPath + "\VC\Auxiliary\Build\Microsoft.VCToolsVersion.default.txt" )         
            cVCVer := Trim( Left( cVCVer, Len( cVCVer ) - 2 ) )
         ENDIF
      ENDIF
      
      IF File( cVSPath +"\VC\Tools\MSVC\" + cVCVer + "\bin\Host" + s_sHostArch + "\" + s_sHostArch + "\cl.exe" )
         s_sVCFolder := cVSPath +"\VC\Tools\MSVC\" + cVCVer 
         s_sVCBinFolder := "bin\Host" + s_sHostArch + "\" + s_sHostArch
         s_sVCLibFolder := "lib\" + s_sHostArch         
      ELSEIF File( s_sProgramsFolder + "\Microsoft Visual Studio 14.0\VC\bin\cl.exe" )
         s_sVCFolder := s_sProgramsFolder + "\Microsoft Visual Studio 14.0\VC"
      ELSEIF File( s_sProgramsFolder + "\Microsoft Visual Studio 13.0\VC\bin\cl.exe" )
         s_sVCFolder := s_sProgramsFolder + "\Microsoft Visual Studio 13.0\VC"
      ELSEIF File( s_sProgramsFolder + "\Microsoft Visual Studio 12.0\VC\bin\cl.exe" )
         s_sVCFolder := s_sProgramsFolder + "\Microsoft Visual Studio 12.0\VC"
      ELSEIF File( s_sProgramsFolder + "\Microsoft Visual Studio 11.0\VC\bin\cl.exe" )
         s_sVCFolder := s_sProgramsFolder + "\Microsoft Visual Studio 11.0\VC"
      ELSEIF File( s_sProgramsFolder + "\Microsoft Visual Studio 10.0\VC\bin\cl.exe" )
         s_sVCFolder := s_sProgramsFolder + "\Microsoft Visual Studio 10.0\VC"
      ELSEIF File( s_sProgramsFolder + "\Microsoft Visual Studio 9.0\VC\bin\cl.exe" )
         s_sVCFolder := s_sProgramsFolder + "\Microsoft Visual Studio 9.0\VC"
      ELSEIF File( s_sProgramsFolder + "\Microsoft Visual Studio 8\VC\bin\cl.exe" )
         s_sVCFolder := s_sProgramsFolder + "\Microsoft Visual Studio 8\VC"
      ELSEIF File( s_sProgramsFolder + "\Microsoft Visual Studio .NET 2003\VC7\bin\cl.exe" )
         s_sVCFolder := s_sProgramsFolder + "\Microsoft Visual Studio .NET 2003\VC7"
      ELSEIF File( s_sProgramsFolder + "\Microsoft Visual Studio\VC98\bin\cl.exe" )
         s_sVCFolder := s_sProgramsFolder + "\Microsoft Visual Studio\VC98"
      ENDIF

#if 0
      IF ( ! Empty( GetEnv( "CLANG-CL" ) ) .AND. File( s_sProgramsFolder + "\llvm\bin\clang-cl.exe" )
         s_sVCFolder := s_sProgramsFolder + "\llvm"
         s_sVC_CL   := "clang-cl.exe"
         s_sVC_LIB  := "llvm-lib.exe"
         s_sVC_LINK := "lld-link.exe"
      ENDIF
#endif

      IF File( s_sProgramsFolder + "\PellesC\bin\pocc.exe" )
         s_sPOCCFolder := s_sProgramsFolder + "\PellesC"
      ELSEIF File( "\PellesC\bin\pocc.exe" )
         s_sPOCCFolder := DiskName() + DRIVE_SEPARATOR + "\PellesC"
      ENDIF

      IF File( "\lcc\bin\lcc.exe" )
         s_sLCCFolder := DiskName() + DRIVE_SEPARATOR + "\lcc"
      ELSEIF File( "c:\lcc\bin\lcc.exe" )
         s_sLCCFolder := "c:\lcc"
      ENDIF

      IF File( "\djgpp\bin\gcc.exe" )
         s_sGCCFolder := DiskName() + DRIVE_SEPARATOR + "\djgpp\bin"
      ELSEIF File( "c:\djgpp\bin\gcc.exe" )
         s_sGCCFolder := "c:\djgpp\bin"
      ENDIF

   #else   

      sExeFolder := GetModuleFileName()
          
      nAt := RAt( "/", sExeFolder )
      sExeFolder := Left( sExeFolder, nAt )

      IF File( sExeFolder + "xhb" ) .AND. Right( sExeFolder, 5 ) == "/bin/"
         s_sHB_Exe       := "xhb"      
         s_sHB_Folder    := Left( sExeFolder, Len( sExeFolder ) - 4 )
         s_sHB_LibFolder := s_sHB_Folder + "lib/"
         s_sHB_IncFolder := s_sHB_Folder + "include/"
         s_lX            := .T.
      ELSEIF File("/usr/local/bin/xhb")
         s_sHB_Exe       := "xhb"      
         s_sHB_Folder    := "/usr/local/"
         s_sHB_LibFolder := s_sHB_Folder + "lib/"
         s_sHB_IncFolder := s_sHB_Folder + "include/"
         s_lX            := .T.
      ELSEIF File("/usr/bin/xhb")
         s_sHB_Exe       := "xhb"      
         s_sHB_Folder    := "/usr/"
         s_sHB_LibFolder := s_sHB_Folder + "lib/"
         s_sHB_IncFolder := s_sHB_Folder + "include/"
         s_lX            := .T.
      ELSEIF File( "/usr/local/bin/harbour" )
         s_sHB_Exe       := "xhb"      
         s_sHB_Folder    := "/usr/local/"
         s_sHB_LibFolder := s_sHB_Folder + "lib/harbour/"
         s_sHB_IncFolder := s_sHB_Folder + "include/harbour/"
      ELSEIF File( "/usr/bin/harbour" )
         s_sHB_Exe       := "harbour"      
         s_sHB_Folder    := "/usr/"
         s_sHB_LibFolder := s_sHB_Folder + "lib/harbour/"
         s_sHB_IncFolder := s_sHB_Folder + "include/harbour/"
         s_lX            := IIF( File( s_sHB_IncFolder + "hbvmpub.h" ), .F., .T. )
      ELSEIF File( "/opt/harbour/bin/harbour" )
         s_sHB_Exe       := "harbour"      
         s_sHB_Folder    := "/opt/harbour/"
         s_sHB_LibFolder := s_sHB_Folder + "lib/harbour/"
         s_sHB_IncFolder := s_sHB_Folder + "include/harbour/"
         s_lX            := IIF( File( s_sHB_IncFolder + "hbvmpub.h" ), .F., .T. )
      ENDIF

      IF File( "/usr/bin/clang" )
         s_sClangFolder := "/usr/"
      ELSEIF File( "/usr/local/bin/clang" )
         s_sCLangFolder := "/usr/local/"
      ELSEIF File( "/opt/gcc/bin/clang" )
         s_sCLangFolder := "/opt/clang/"
      ENDIF   

      IF File( "/usr/bin/gcc" )
         s_sGCCFolder := "/usr/"
      ELSEIF File( "/usr/local/bin/gcc" )
         s_sGCCFolder := "/usr/local/"
      ELSEIF File( "/opt/gcc/bin/gcc" )
         s_sGCCFolder := "/opt/gcc/"
      ENDIF   

      IF File( "/usr/bin/tcc" )
         s_sTCCFolder := "/usr/"
      ELSEIF File( "/usr/local/bin/tcc" )
         s_sTCCFolder := "/usr/local/"
      ELSEIF File( "/opt/gcc/bin/tcc" )
         s_sTCCFolder := "/opt/tcc/"
      ENDIF   

   #endif

RETURN

FUNCTION AddEnvVar( cVar, cValue )
   s_hEnvVars[ cVar ] := cValue
RETURN s_hEnvVars

FUNCTION FoundCompiler( C_Compiler )
   C_Compiler := Lower( C_Compiler )

    DO CASE
       CASE C_Compiler == "bcc"
          RETURN s_sBCCFolder

       CASE C_Compiler == "clang"
          RETURN s_sClangFolder

       CASE C_Compiler == "gcc"
          RETURN s_sGCCFolder

       CASE C_Compiler == "pocc"
          RETURN s_sPOCCFolder

       CASE C_Compiler == "tcc"
          RETURN s_sTCCFolder

       OTHERWISE
          Throw( ErrorNew( "xBuild", 0, 1001, ProcName(), "unknown compiler: " + C_Compiler ) )         
   ENDCASE

RETURN NIL   


 
   