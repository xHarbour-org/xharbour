/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * hbmake.prg xHarbour make utility main file
 *
 * Copyright 2000-2007 Luiz Rafael Culik <culikr@uol.com.br>
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
 /*
  * Modified By Sandro Freire sandrorrfreire at yahoo.com.br
  * added suport to OS Unix
  *
  */

#pragma w2

#include "fileio.ch"
#include "common.ch"
#include "radios.ch"
#include "inkey.ch"
#include 'hbver.ch'


#define EOL Hb_OsNewLine()
#define CRLF Hb_OsNewLine()
#xtranslate TimeToSec(<x>) => ( ( Val( Substr( <x>, 1, 2 ) ) * 3600 ) +( Val( Substr( <x>, 4, 2 ) ) * 60 ) + ( Val( Substr( <x>, 7, 2 ) ) ) )

//REQUEST HB_NOMOUSE

#translate DateDiff(<x>,<y>) => (<x>-<y>)

#Define HBM_USE_DEPENDS    // Set this to have section #DEPENDS parsed like RMake, Ath 2004-06
                           // An extra parameter is added to FileIsNewer() to have it check the INCLUDE paths also
                           // Interactive mode asks whether sources should be scanned for dependencies (#include, set procedure to, set proc to

/*
Beginning Static Variables Table
Default Values for core variables are set here
New Core vars should only be added on this section
*/

STATIC s_cHbMakeVersion  := "1.30"
STATIC s_lPrint          := .F.
STATIC s_lEdit           := .F.
STATIC s_aDefines        := {}
STATIC s_aBuildOrder     := {}
STATIC s_aCommands       := {}
STATIC s_aMacros         := {}
STATIC s_aPrgs           := {}
STATIC s_aExtLibs        := {}
#IfDef HBM_USE_DEPENDS
STATIC s_aDepends        := {}
#Endif
STATIC s_aCFiles         := {}  // array of C source files
STATIC s_aObjs           := {}
STATIC s_aObjsc          := {}
STATIC s_aSrcPaths       := {}
STATIC s_lEof            := .F.
STATIC s_aResources      := {}
STATIC s_cMakeFileName   := "makefile.lnk"
STATIC s_cLinkCommands   := ""
STATIC s_lLinux          := .F.
STATIC s_lUnix           := .F.
STATIC s_lOS2            := .F.
STATIC s_lWin32          := .F.
STATIC s_lBcc            := .F.  // Borland C compiler
STATIC s_lPocc           := .F.  // Pelles C compiler
STATIC s_lMSVcc          := .F.  // MS-Visual C compiler
STATIC s_lGcc            := .F.  // GNU C compiler
STATIC s_lMinGW          := .F.  // MinGW C compiler
STATIC s_lForce          := .F.  // "-f" flag
STATIC s_szProject       := ""
STATIC s_lLibrary        := .F.
STATIC s_lIgnoreErrors   := .F.
STATIC s_lExtended       := .T.
STATIC s_lRecursive      := .F.
STATIC s_lCancelRecursive:= .F.
STATIC s_lEditMake       := .F.
STATIC s_lCompress       := .F.
STATIC s_lExternalLib    := .F.
STATIC s_aDir
STATIC s_aLangMessages   := {}
STATIC s_cAppName        := ""
STATIC s_cDefLang
STATIC s_cLog            := ""   // log file name.
STATIC s_cMap            := ""   // map file name. For borland c
STATIC s_cTds            := ""   // state file name. For borland c
STATIC s_lGenPpo         := .F.
STATIC s_nLang           := 2    // default language is english
STATIC s_lMt             := .F.
STATIC s_cUserDefine     := ""
STATIC s_cUserInclude    := ""
STATIC s_cUserLibs       := ""   // for user libs located other than "xharbour\lib" folder.
STATIC s_lxFwh           := .F.
STATIC s_nFilesToAdd     := 5
STATIC s_nWarningLevel   := 0
STATIC s_lAsDll          := .F.
STATIC s_cAlertMsg       := ""
STATIC s_cHarbourCfg     := "harbour.cfg"   // don't change this file name.
STATIC s_cObjDir         := "obj"
STATIC s_lGui            := .F.
STATIC s_cEditor         := ""
STATIC s_cHarbourDir     := ""
STATIC s_lGenCsource     := .F.  // Generate PCode by default // Ath added 31-05-2006
STATIC s_cInvalidExt     := {".prg",".c",".cpp",".ch",".h",".ppo",".bat",".doc",".txt",".dbf",".ntx",".cdx",".dbt",".fpt",".mem"}

*---------------------------------------------
FUNCTION _APPMAIN( cScriptfile, p1, p2, p3, p4, p5, p6 )
*---------------------------------------------

   LOCAL aFile       := {}
   LOCAL aDef        := {}
   LOCAL cMakeParams := ""
   LOCAL nLang       := GETUSERLANG()
   LOCAL cExt := ""
   LOCAL cExp,cLib
   LOCAL lCreateAndCompile := .F.  // for "-c" param only.

   IF Empty( cScriptfile )
      ShowHelp()
      Return .F.
   endif

   cExt := SubStr( cScriptfile, At(".",cScriptfile) )

   IF ! Empty(cExt) .AND. lower(cExt) IN s_cInvalidExt
      Alert("Invalid extension for hbmake script file <"+cScriptfile+">.")
      Return .F.
   ENDIF

   CLS

   if file( s_cMakeFileName )
      ferase( s_cMakeFileName )
   endif

   SET(39,159)

   cMakeParams := ConvertParams( @cScriptfile, aFile, p1, p2, p3, p4, p5, p6 )

   IF ! empty( cMakeParams )

      cMakeParams := upper( cMakeParams )

      IF "-LPT" IN cMakeParams
         nLang := 1
      ELSEIF "-LEN" IN cMakeParams
         nLang := 2
      ELSEIF "-LES" IN cMakeParams
         nLang := 3
      ENDIF

   ENDIF

   MHIDE()

   s_nLang := nLang
   s_cDefLang := IIF( s_nLang == 1, "PT", IIF( s_nLang == 2, "EN", "ES" ) )
   s_aLangMessages := BuildLangArray( s_cDefLang )


   // What S.O. ?
   //
   s_lOS2   := ( "OS/2" IN OS() )
   s_lLinux := ( "LINUX" IN Upper( OS() ) )
   s_lWin32 := ( "WINDOWS" IN Upper( OS() ) )
   s_lUnix  := IF( ( "UNIX" IN Upper( OS() ) ) .OR. ( "HP-UX" IN Upper( OS() ) ) , .T., .F. )
   s_lLinux := ( "LINUX" IN Upper( OS() ) )

   IF PCount() == 0 .or.;
      "?" IN cMakeParams .or. ;
      "/?" IN cMakeParams .or. ;
      "-?" IN cMakeParams .or. ;
      "/h" IN cMakeParams .or. ;
      "-h" IN cMakeParams
      ShowHelp()
      RETURN NIL
   ENDIF

   IF "credits" IN lower(cMakeParams)
      ShowCredits()
      RETURN NIL
   ENDIF

   s_cHarbourDir := GetHarbourDir()

   IF Empty( s_cHarbourDir )
      IF s_nLang=1
         s_cAlertMsg := "Hbmake necessita do xharbour bin no path."
      ELSEIF s_nLang=3
         s_cAlertMsg := "Hbmake necessita de lo xharbour bin en lo path."
      ELSE
         s_cAlertMsg := "Hbmake need of the xharbour bin in the path."
      ENDIF
      alert( s_cAlertMsg )
      RUN( "PATH" )
      RETURN NIL
   ENDIF

   IF s_nLang=1
      SET DATE BRITISH
   ELSE
      SET DATE ANSI
   ENDIF

   SET SCORE OFF
   SET CENTURY ON
   SET TRACE ON

   DEFAULT p1 TO ""
   DEFAULT p2 TO ""
   DEFAULT p3 TO ""
   DEFAULT p4 TO ""
   DEFAULT p5 TO ""
   DEFAULT p6 TO ""


   s_cEditor := HBMake_Editor()

   IF s_lOS2 .OR. s_lLinux

      s_lGcc   := .T.
      s_lBcc   := .F.
      s_lPocc  := .F.
      s_lMSVcc := .F.
      s_lMinGW := .F.

   ELSEIF s_lWin32

      s_lGcc   := .F.

      if ".bc" IN lower(cScriptfile)
         s_lBcc   := .T.  // Borland C compiler
      elseif ".pc" IN lower(cScriptfile)
         s_lPocc  := .T.  // Pelles C compiler
      elseif ".vc" IN lower(cScriptfile)
         s_lMSVcc := .T.  // MS-VC Compiler
      elseif ".mg" IN lower(cScriptfile) .OR. ".mgw" IN lower(cScriptfile) .OR. ".mingw" IN lower(cScriptfile)
         s_lMinGW := s_lGcc := .T.
      else
         s_lBcc   := .T.  // default
         s_lPocc  := .F.
         s_lMSVcc := .F.
         s_lMinGW := .F.
         s_lGcc   := .F.
      endif

   ENDIF


   IF Len( aFile ) > 1
      IF s_nLang=1
         s_cAlertMsg :="Arquivo definido mais que uma vez."
      ELSEIF s_nLang=3
         s_cAlertMsg:="Fichero definido m†s que una vez."
      ELSE
         s_cAlertMsg:="File defined more than once."
      ENDIF
      alert( s_cAlertMsg )
      RETURN NIL
   ENDIF

   IF Len( aFile ) > 0
      cScriptfile := aFile[ 1 ]
   ELSE
      cScriptfile := ""
   ENDIF

   IF Empty(cScriptfile)
      IF s_nLang=1
         s_cAlertMsg := "Nome de arquivo inv†lido."
      ELSEIF s_nLang=3
         s_cAlertMsg := "Nombre de fichero invalido."
      ELSE
         s_cAlertMsg := "Invalid file name."
      ENDIF
      alert( s_cAlertMsg )
      RETURN NIL
   ENDIF

   if PCount() > 1
      ProcessParameters( cMakeParams )
   endif

   IF s_lForce .and. !File( cScriptfile )
      IF s_nLang=1
         s_cAlertMsg := "Arquivo <"+cScriptfile+"> n∆o encontrado."
      ELSEIF s_nLang=3
         s_cAlertMsg := "Fichero <"+cScriptfile+"> no encontrado."
      ELSE
         s_cAlertMsg := "File <"+cScriptfile+"> not found."
      ENDIF
      alert( s_cAlertMsg )
      RETURN NIL
   ENDIF


   s_cAppName := Substr( cScriptfile,1 , AT(".",cScriptfile) -1)
   s_cLog    := s_cAppName + ".log"

   cExp := s_cAppName + ".exp"
   cLib := s_cAppName + ".lib"

   FErase( (s_cAppName+".out") ) // erase old *.out log filename
   FErase( s_cLog )

   if s_lBcc
      /* if you need of these files, comment the lines below. */
      s_cMap    := s_cAppName + ".map"
      s_cTds    := s_cAppName + ".tds"
      FErase( s_cMap )
      FErase( s_cTds )
   endif

   // Edit or create a new script file.

   if !s_lForce .and. s_lEdit  // PCount() > 1

      if ! File( cScriptfile )
         lCreateAndCompile := ( "-C" IN cMakeParams  )
      else
         lCreateAndCompile := .f.
         cMakeParams := StrTran( cMakeParams, "-C","" )
      endif

      if Hb_IsNil(s_lGenppo) .OR. s_lGenppo == .F.
         Delete_ppo()
      endif

      IF s_lLibrary
         CreateScriptLib( cScriptfile )  
      ELSE
         CreateScript( cScriptfile, lCreateAndCompile  )
      ENDIF

      RETURN NIL

   endif


   // Compile...

   CLS

   // Script file is parsed here

   IF ! ParseMakeFile( cScriptfile )

      IF s_nLang = 1      // brazilian portuguese
         s_cAlertMsg := "<"+cScriptfile+ "> n∆o pode ser aberto. FERROR("+Ltrim(Str(FError()))+"). O HbMake ser† fechado."
      ELSEIF s_nLang = 3  // spanish
         s_cAlertMsg := "<"+cScriptfile + "> no pode ser abierto. FERROR("+Ltrim(Str(FError()))+"). Lo HbMake ser† cerrado."
      ELSE                // english
         s_cAlertMsg := "<"+cScriptfile + "> cannot be openned. FERROR("+Ltrim(Str(FError()))+"). The HbMake will be closed."
      ENDIF

      Alert( s_cAlertMsg )

      RETURN NIL

   ENDIF

   IF s_lPrint
      PrintMacros()
   ENDIF

   set cursor off

   IF !IsDirectory( s_cObjDir )
      MakeDir( s_cObjDir )
   ENDIF

   if Hb_IsNil(s_lGenppo) .OR. s_lGenppo == .F.
      Delete_ppo()
   endif

   IF s_lForce
      CompileFiles()
   ELSE
      CompileUpdatedFiles()
   ENDIF

   SET CURSOR ON
   setpos(9,0)
   Outstd( s_cLinkCommands + CRLF )
   SET CURSOR OFF
   __RUN( (s_cLinkCommands) )

   IF s_lCompress .AND. !s_lLibrary
      SET CURSOR ON
      setpos(9,0)
      __Run( " upx -9 "+ (s_cAppName) )
      SET CURSOR OFF
   ENDIF


   IF s_lasdll .or. lower(right(s_cAppName,3)) == 'dll'
       __Run( ReplaceMacros("implib $(HB_DIR)\lib\" + left(s_cAppName,at(".",s_cAppName)-1)+".lib " +s_cAppName ))
   ENDIF


   if s_lBcc
      /*
      NOTE: The TDS file is always created by borland linker.
      If you need of this file, comment the lines below and
      remove "-x" flag that is created by hbmake in the
      LFLAGS statment for Borland compiler.
      */
      FErase( s_cMap )
      FErase( s_cTds )
   endif

   IF s_lPocc
      FErase( cExp )
      IF !s_lLibrary
         FErase( cLib )
      ENDIF
   ENDIF

//   ferase( s_cMakeFileName )

   SET CURSOR ON

RETURN NIL

*------------------------------
FUNCTION ParseMakeFile( cFile )
*------------------------------

   LOCAL cBuffer     := {}
   LOCAL cMacro      := iif(s_lMSVcc,"#MSVC",iif(s_lPocc,"#POCC",iif(s_lGcc,"#GCC","#BCC")))
   LOCAL cDep        := "#DEPENDS"
   LOCAL cOpt        := "#OPTS"
   LOCAL cCom        := "#COMMANDS"
   LOCAL cBuild      := "#BUILD"
   LOCAL cTemp       := ""
   LOCAL cTemp1      := ""
   LOCAL aTemp       := {}
   LOCAL lMacrosec   := .T.
   LOCAL lBuildSec   := .F.
   LOCAL lComSec     := .F.
#IFDEF HBM_USE_DEPENDS
   LOCAL lDepSec     := .F.
#ENDIF
   LOCAL aTemp1      := {}
   LOCAL cCfg        := ""
   LOCAL lCfgFound   := .F.
   LOCAL aTempCFiles := {}
   LOCAL lLinux      :=  s_lLinux
   LOCAL lUnix       :=  s_lUnix
   LOCAL aLib
   LOCAL aLibx
   LOCAL lDjgpp      := "GNU C" in HB_COMPILER()
   LOCAL x           := 1
   LOCAL nSFhandle   // script file handle
   LOCAL cTrash :=""
   Local n

   nSFhandle := FT_FUSE( cFile, FO_READ )

   IF nSFhandle < 0
      RETURN .F.
   ENDIF

   #IFndef __PLATFORM__Windows
      IF !FILE("hbtemp.c")
         CreateLink()
      ENDIF
   #ENDIF

   cBuffer := Trim( Substr( ReadLN( @s_lEof ), 1 ) )

   AAdd( s_aDefines, { "HARBOUR_DIR", s_cHarbourDir } )

   IF s_lBcc
      AAdd( s_aDefines, { "MAKE_DIR", GetBccDir() } )
   ELSEIF s_lGcc
      AAdd( s_aDefines, { "MAKE_DIR", GetGccDir() } )
   ELSEIF s_lMSVcc
      AAdd( s_aDefines, { "MAKE_DIR", GetVccDir() } )
   ELSEIF s_lPocc
      AAdd( s_aDefines, { "MAKE_DIR", GetPoccDir() } )
   ENDIF

   WHILE ! s_lEof

      IF cMacro IN  cBuffer
         lMacroSec := .T.
         lBuildSec := .F.
         lComSec   := .F.
#IFDEF HBM_USE_DEPENDS
         lDepSec   := .F.
#Endif
      ELSEIF  cBuild IN cBuffer
         lMacroSec := .F.
         lBuildSec := .T.
         lComSec   := .F.
#IfDef HBM_USE_DEPENDS
         lDepSec   := .F.
#Endif
      ELSEIF  cCom IN  cBuffer
         lBuildSec := .F.
         lComSec   := .T.
         lMacroSec := .F.
#IfDef HBM_USE_DEPENDS
         lDepSec   := .F.
      ELSEIF  cDep IN  cBuffer
         lBuildSec := .F.
         lComSec   := .F.
         lMacroSec := .F.
         lDepSec   := .T.
#Endif
      ELSE
         ? "Invalid Make File"
         FClose( nSFhandle )
         RETURN .F.
      ENDIF

      cTemp := Trim( Substr( ReadLN( @s_lEof ), 1 ) )

      IF  "//" IN  cTemp

         WHILE At( "//", cTemp ) > 0

            cTemp := Strtran( cTemp, " //", "" )
            cTemp += Trim( Substr( ReadLN( @s_lEof ), 1 ) )

         ENDDO

         cTemp := Strtran( cTemp, " //", "" )

      ENDIF

      /* Pelles C 5.x and later uses "=" char to assign values on link flag.
       * f.e. /ALTERNATENAME:name=altname.
       * So, we need handle the first "=" different from others. [modalsist]
       */
      if s_lPocc
         n := At( "=", cTemp )
         // preserv the first "=" and replace other by "ı"
         cTemp := SubStr( cTemp,1,n ) + Strtran( SubStr(cTemp,n+1),"=","ı" )
         // get link flags after "=".
         aTemp := ListAsArray2( Alltrim( cTemp ), "=" )
         // restore other "=".
         for n := 1 to len( aTemp)
             aTemp[n] := StrTran( aTemp[n], "ı","=")
         next
      else
         aTemp := ListAsArray2( Alltrim( cTemp ), "=" )
      endif

      IF lmacrosec

         IF Alltrim( Left( cTemp, 7 ) ) <> '!ifndef' .AND. Alltrim( Left( cTemp, 6 ) ) <> "!endif" .AND. Alltrim( Left( cTemp, 7 ) ) <> '!IFfile' .AND. Alltrim( Left( cTemp, 7 ) ) <> '!stdout' .AND. Alltrim( Left( cTemp, 6 ) ) <> '!ifdef'

            IF Len( aTemp ) > 1

                IF  "$" IN aTemp[ 2 ]

                  IF s_lGcc .AND. aTemp[ 1 ] = "CFLAG1" .OR. s_lGcc .AND. aTemp[ 1 ] = "CFLAG2"
                      AAdd( s_aMacros, { aTemp[ 1 ], Strtran( ReplaceMacros( aTemp[ 2 ] ), "\", "/" ) } )

                      x++
                  ELSE

                     IF aTemp[ 1 ] == "MT" .AND. aTemp[ 2 ] == "YES"
                        s_lMt := .T.
                     ENDIF

                     IF aTemp[ 1 ] == "LIBFILES" .AND. ! s_lMt

                        aLib := ListAsArray2( aTemp[ 2 ], ' ' )

                        FOR each aLibx in aLib

                           IF At( 'mt.lib', Lower( aLibx ) ) > 0
                              s_lMt := .T.
                           ENDIF

                           IF "-l" in Lower( aLibx )
                              s_lBcc    := .F.
                              s_lGcc    := .T.
                              s_lMSVcc  := .F.
                              s_lPocc   := .F.
                              s_aDefines[2] := { "MAKE_DIR", GetGccDir() }
                              s_aDefines[3] := { "HARBOUR_DIR", s_cHarbourDir }

                           ENDIF

                        NEXT

                     ENDIF

                     IF aTemp[ 1 ] == "ALLOBJ" .AND. ! s_lMt

                     ENDIF

                     AAdd( s_aMacros, { aTemp[ 1 ], ReplaceMacros( aTemp[ 2 ] ) } )

                  ENDIF

               ELSE

                  IF s_lGcc .AND. aTemp[ 1 ] = "CFLAG1" .OR. s_lGcc .AND. aTemp[ 1 ] = "CFLAG2"
                     AAdd( s_aMacros, { aTemp[ 1 ], Strtran( aTemp[ 2 ], "\", "/" ) } )

                      x++

                  ELSE
                     IF aTemp[ 1 ] == "LIBFILES" .AND. ! s_lMt

                        aLib := ListAsArray2( aTemp[ 2 ], ' ' )

                        FOR each aLibx in aLib

                           IF At( 'mt.lib', Lower( aLibx ) ) > 0
                              s_lMt := .T.
                           ENDIF

                           IF "-l" in Lower( aLibx )
                              s_lBcc    := .F.
                              s_lGcc    := .T.
                              s_lMSVcc  := .F.
                              s_lPocc   := .F.
                              s_aDefines[2] := { "MAKE_DIR", GetGccDir() }
                              s_aMacros[2,2] :=  GetGccDir()
                           ENDIF

                        NEXT
                     elseif  aTemp[ 1 ] == "SHELL"
                             if !empty( Atemp[ 2 ] )
                                __run( (Atemp[ 2 ]) + " > e.txt")
                                Atemp[ 2 ] := alltrim( memoread( "e.txt" ) )
                                aTemp[ 2 ] := strtran( aTemp[ 2 ],chr(13),"")
                                aTemp[ 2 ] := strtran( aTemp[ 2 ],chr(10),"")
                                ferase("e.txt")
                             endif
                     ENDIF
                        AAdd( s_aMacros, { aTemp[ 1 ], aTemp[ 2 ] } )
                  ENDIF

               ENDIF

            ENDIF


            IF aTemp[ 1 ] == "COMPRESS"
               s_lCompress := "YES" IN aTemp[ 2 ]
            ENDIF

            IF aTemp[ 1 ] == "GUI"
               s_lGui := "YES" IN aTemp[ 2 ]
            ENDIF

            IF aTemp[ 1 ] == "EXTERNALLIB"
               s_lExternalLib := "YES" IN aTemp[ 2 ]
            ENDIF

            IF aTemp[ 1 ] == "SRC02"  // obj dir
               s_cObjDir := aTemp[ 2 ]
            ENDIF

            IF aTemp[ 1 ] == "PROJECT"

               IF At( '.lib', aTemp[ 2 ] ) > 0 .OR. At( '.a', aTemp[ 2 ] ) > 0
                  s_lLibrary := .T.
               ENDIF

               s_cAppName := SubStr( aTemp[ 2 ], 1, AT( ' ', aTemp[ 2 ] ) -1 )

            ENDIF

            IF aTemp[ 1 ] == "OBJFILES"
               s_aObjs := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
            ENDIF

            IF aTemp[ 1 ] == "OBJCFILES"

               aTemp1 := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )

               IF Len( aTemp1 ) == 1

                  IF ! Empty( aTemp[ 1 ] )
                      s_aObjsC := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
                   ENDIF

               ELSE
                  s_aObjsC := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
               ENDIF

            ENDIF

            IF aTemp[ 1 ] == "PRGFILES"
               s_aPrgs     := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
               s_lExtended := .T.
               lCfgFound := FindHarbourCfg( @cCfg )
            ENDIF

            IF aTemp[ 1 ] == "PRGFILE"
               s_aPrgs := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
            ENDIF

            IF aTemp[ 1 ] == "EXTLIBFILES"
               s_aExtLibs  := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
            ENDIF

            IF aTemp[ 1 ] == "CFILES"

               IF s_lExtended
                  aTempCFiles := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )

                  IF ( Len( aTempCFiles ) == 1 )

                     IF ! Empty( aTempCFiles[ 1 ] )
                        s_aCFiles := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
                     ENDIF

                  ELSE
                     s_aCFiles := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
                  ENDIF

               ELSE
                  s_aCFiles := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
               ENDIF


            ENDIF

            IF aTemp[ 1 ] == "RESFILES"
               s_aResources := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
            ENDIF

            IF aTemp[ 1 ] == "EDITOR"
               s_cEditor  := Trim( aTemp[ 2 ] )
            ENDIF

         ELSE

            IF '!ifndef' IN cTemp
               CheckDefine( cTemp )
            ELSEIF  '!ifdef' IN cTemp
               CheckIFdef( cTemp )
            ELSEIF '!iffile' IN cTemp
               CheckIFFile( cTemp )
            ELSEIF  '!stdout' IN cTemp
               CheckStdOut( cTemp )
            ENDIF

         ENDIF

      ENDIF
      IF s_lMingw .and. s_lGcc
         x := ascan(s_aMacros,{|x|  X[1] == "HB_DIR"})
         IF x>0
            IF s_aMacros[x,2] != s_cHarbourDir
               s_aMacros[x,2] := s_cHarbourDir
            ENDIF
         ENDIF
      ENDIF

      IF lBuildSec

         s_szProject   := cTemp
         s_aBuildOrder := ListAsArray2( cTemp, ":" )

         IF ! s_lLibrary
            CreateMakefile() // create makefile.lnk
         ELSE
            CreateMakelib()  // idem
         ENDIF

      ENDIF

      IF lComSec

         IF ! Empty( cTemp )
            Setcommands( cTemp )
         ENDIF

      ENDIF

#IfDef HBM_USE_DEPENDS
      IF lDepSec

         IF ! Empty( cTemp )
            SetDependencies( cTemp )
         ENDIF

      ENDIF
#Endif

      IF cTemp = "#BUILD"
         cBuffer := cTemp
      ELSEIF cTemp == "#COMMANDS"
         cbuffer := cTemp
#IfDef HBM_USE_DEPENDS
      ELSEIF cTemp == "#DEPENDS"
         cbuffer := cTemp
#Endif
      ENDIF

   ENDDO

   FT_FUSE()  // Close the opened file & release memory

   IF s_lExtended .AND. (!lCfgFound .or. s_lForce)

      IF s_lBcc
         BuildBccCfgFile()
      ELSEIF s_lMSVcc
         BuildMscCfgFile()
      ELSEIF s_lPocc
         BuildPccCfgFile()
      ELSEIF s_lGcc .AND. !lLinux
         BuildGccCfgFile()
      ELSEIF s_lGcc .AND. lLinux
         BuildGccCfgFileL()
      ENDIF

   ENDIF

RETURN .T.

*----------------------------
FUNCTION Checkdefine( cTemp )
*----------------------------

   LOCAL nPos
   LOCAL cRead
   LOCAL aSet     := {}
   LOCAL nMakePos

   IF cTemp == "!endif"
      RETURN NIL
   ENDIF

   cTemp := Trim( Substr( ReadLN( @s_lEof ), 1 ) )
   cTemp := Strtran( cTemp, "!ifndef ", "" )
   cTemp := Strtran( cTemp, "\..", "" )
   cTemp := Strtran( cTemp, "/..", "" )

   IF  "\.." IN  cTemp
      cTemp := Substr( cTemp, 1, At( "\..", cTemp ) - 1 )
   ELSEIF  "/.." IN  cTemp
      cTemp := Substr( cTemp, 1, At( "/..", cTemp ) - 1 )
   ENDIF

   aSet := ListAsArray2( cTemp, "=" )
   nPos := AScan( s_aDefines, { | x | x[ 1 ] == aSet[ 1 ] } )

   IF nPos = 0
      cRead    := Alltrim( Strtran( aSet[ 2 ], "$(", "" ) )
      cRead    := Strtran( cRead, ")", "" )
      nMakePos := AScan( s_aDefines, { | x | x[ 1 ] == cRead } )

      IF nMakePos > 0
         AAdd( s_aDefines, { aSet[ 1 ], s_aDefines[ nMakePos, 2 ] } )
         AAdd( s_aMacros, { aSet[ 1 ], s_aDefines[ nMakePos, 2 ] } )
      ENDIF

   ENDIF

RETURN NIL

*----------------------------
FUNCTION Setcommands( cTemp )
*----------------------------

   LOCAL cRead        := Alltrim( readln( @s_lEof ) )
   LOCAL nCount       := 0
   LOCAL aTempMacros  := {}
   LOCAL aLocalMacros := {}

   aTempMacros := ListAsArray2( cREad, " " )

   AEval( aTempMacros, { | xMacro | IIF( At( "$", xMacro ) > 0, ;
                         IIF( At( ";", xMacro ) > 0, ( aLocalMacros := ListAsArray2( xMacro, ";" ), ;
                         AEval( aLocalMacros, { | x | Findmacro( x, @cRead ) } ) ), ;
                         Findmacro( xMacro, @cRead ) ), ) } )
   AAdd( s_aCommands, { cTemp, cRead } )

RETURN NIL

#IfDef HBM_USE_DEPENDS

*--------------------------------
FUNCTION SetDependencies( cTemp )
*--------------------------------

   LOCAL nCount       := 0
   LOCAL aTempMacros  := {}
   LOCAL aLocalMacros := {}
   LOCAL cTmp         := ""

   aTempMacros := ListAsArray2( ReplaceMacros(cTemp), " " )

   IF Len( aTempMacros ) > 1
      cTmp := aTempMacros[ 1 ]
      IF Right(cTmp,1) == ":"
         cTmp := Left(cTmp,Len(cTmp) - 1)
      ENDIF
      aTempMacros := ADel( aTempMacros , 1)
      ASize(aTempMacros,Len(aTempMacros) - 1)
      AAdd( s_aDepends, { cTmp, AClone( aTempMacros ) } )
   ENDIF

RETURN NIL
#Endif

*----------------------------------
FUNCTION Findmacro( cMacro, cRead )
*----------------------------------

   LOCAL nPos
   LOCAL cTemp
   LOCAL aLocalMacros := {}

   cMacro := Substr( cMacro, 1, At( ")", cMacro ) )

   IF  "-" IN cMacro
      cMacro := Substr( cMacro, 3 )
   ENDIF

   IF  ";" IN cMacro
      cMacro := Substr( cMacro, At( ";", cMacro ) + 1 )
   ENDIF

   nPos := AScan( s_aMacros, { | x | "$(" + Alltrim( x[ 1 ] ) + ")" == cMacro } )

   IF nPos = 0
      cTemp := Strtran( cMacro, "$(", "" )
      cTemp := Strtran( cTemp, ")", "" )

      IF ! Empty( cTemp )
         cRead := Alltrim( Strtran( cRead, cMacro, Gete( cTemp ) ) )
      ENDIF

   ELSE
      cRead := Alltrim( Strtran( cRead, cMacro, s_aMacros[ nPos, 2 ] ) )
   ENDIF

RETURN cRead

*--------------------------------
FUNCTION ReplaceMacros( cMacros )
*--------------------------------

   LOCAL nCount       := 0
   LOCAL aTempMacros  := {}
   LOCAL aLocalMacros := {}
   LOCAL cLibPath := ""
   LOCAL cLibPath1 := ""
   LOCAL cLibPath2:= ""


   IF "/LIBPATH2:" IN cMacros
      cLibPath2 := "/LIBPATH2:"
      cMacros := StrTran(cMacros,cLibPath2,"cccccccccc ")
   ENDIF

   IF "/LIBPATH1:" IN cMacros
      cLibPath1 := "/LIBPATH1:"
      cMacros := StrTran(cMacros,cLibPath1,"bbbbbbbbbb ")
   ENDIF

   IF "/LIBPATH:" IN cMacros
      cLibPath := "/LIBPATH:"
      cMacros := StrTran(cMacros,cLibPath,"aaaaaaaaaa ")
   ENDIF

   aTempMacros := ListAsArray2( cMacros, " " )

   AEval( aTempMacros, { | xMacro | IIF(  "$" IN xMacro , ;
                         IIF(  ";" IN xMacro , ( aLocalMacros := ListAsArray2( xMacro, ";" ), ;
                         AEval( aLocalMacros, { | x | Findmacro( x, @cMacros ) } ) ), ;
                         Findmacro( xMacro, @cMacros ) ), ) } )

   IF !empty(cLibPath)

      cMacros := strtran(cMacros,"aaaaaaaaaa ","/LIBPATH:")
      cMacros := strtran(cMacros,"bbbbbbbbbb ","/LIBPATH:")
      cMacros := strtran(cMacros,"cccccccccc ","/LIBPATH:")

      IF s_lPocc
         cMacros := StrTran(cMacros,"\BIN","")
         cMacros := StrTran(cMacros,"\bin","")
      ENDIF

   ENDIF
   
RETURN cMacros

*------------------------
FUNCTION CreateMakefile()
*------------------------
* Create makefile.lnk to build your exe file from script file.
*-----------------------
   LOCAL cRead
   LOCAL nPos
   LOCAL aMacro
   LOCAL aTemp
   LOCAL cCurrentRead := ''
   LOCAL cMacro
   LOCAL xInfo
   LOCAL xItem
   LOCAL nMKhandle  // makefile.lnk handle

   cRead := Alltrim( ReadLN( @s_lEof ) )
   s_szProject := cRead

   aMacro := ListAsArray2( cRead, ":" )
   IF Len( aMacro ) > 1
      aTemp := ListAsArray2( aMacro[ 2 ], " " )
      AEval( aTemp, { | xItem | AAdd( s_aBuildOrder, xItem ) } )
   ENDIF

   AAdd( s_aBuildOrder, aMacro[ 1 ] )
   cRead := Strtran( cRead, "@&&!", "" )
   aMacro := ListAsArray2( cRead, '\' )

   AEval( aMacro, { | xMacro |  IIF(  "$" IN xMacro , FindMacro( xMacro, @cRead ), ) } )

   IF ! s_lLinux .AND. ! s_lMinGW

      s_cLinkCommands   := cRead + " @" + s_cMakeFileName
      nMKhandle := FCreate( s_cMakeFileName )

      IF nMKhandle = F_ERROR
         IF s_nLang = 1      // brazilian portuguese
            s_cAlertMsg := "<"+s_cMakeFileName + "> n∆o pode ser criado."
         ELSEIF s_nLang = 3  // spanish
            s_cAlertMsg := "<"+s_cMakeFileName + "> no pode ser criado."
         ELSE                // english
            s_cAlertMsg := "<"+s_cMakeFileName + "> cannot be created."
         ENDIF
         Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )
         RETURN NIL
      ENDIF

   ELSE
      s_cLinkCommands := cRead + " "
   ENDIF
/*
   There are 7 main flags to parse from script file :
   $(LFLAGS) +
   $(ALLOBJ), +
   $(PROJECT),, +
   $(ALLLIB), +
   $(DEFFILE), +
   $(ALLRES),
!
*/
   FOR nPos := 1 TO 7

      cRead := Alltrim( readln( @s_lEof ) )
      cCurrentRead := cRead
      aMacro := ListAsArray2( cRead, " " )

      FOR EACH cMacro IN aMacro

          IF ! ( "$" IN cMacro )
             LOOP
          ENDIF

             FindMacro( cMacro , @cRead )

             IF "$(PROJECT)" IN cCurrentRead

                IF ! s_lGcc

                   IF ! s_lLinux
                      IF s_lMSVcc .OR. s_lPocc
                         cRead := strtran(cRead,",","")
                         cRead := strtran(cRead,"+","")
                         xInfo := iif(s_lMSVcc," -out:","/out:")
                         xInfo += cRead
                         cRead := xInfo
                      ENDIF
                      if ! empty( cRead )
                         FWrite( nMKhandle, cRead + CRLF )
                      endif
                   ENDIF

                ELSEIF s_lGcc .AND. s_lLinux .OR. ( s_lGcc .AND. s_lMinGW)
                  s_cLinkCommands += "-o " + cRead + " "

                ELSEIF s_lGcc .AND. ! s_lLinux .AND. At( '.exe', cRead ) > 0
                  FWrite( nMKhandle, "-o " + cRead + CRLF )

                ENDIF

             ELSE

                IF ! s_lLinux

                   IF s_lMsVcc .OR. s_lPocc
                     cRead := strtran(cRead,",","")
                     cRead := strtran(cRead,"+","")
//                   cRead := strtran(cRead," ", '"' +CRLF+'"')
                     aTemp := ListAsArray2( cRead, " " )
                     cRead := ""

                     FOR EACH xItem IN aTemp
                         if ! empty( xItem )
                            cRead += xItem + CRLF
                         endif
                     NEXT

                     cRead := substr(cRead,1,rat(CRLF,cRead)-1)

                   ENDIF

                   IF s_lMinGW
                      s_cLinkCommands += strtran(cRead,"/","\") + " "
                   ELSE
                      if ! empty( cRead )
                         FWrite( nMKhandle, cRead + CRLF )
                      endif
                   ENDIF

                ELSE
                   s_cLinkCommands += cRead + " "
                ENDIF

             ENDIF
      NEXT

   NEXT

   //IF !s_lLinux .and. s_lMinGW
   IF s_lWin32 .OR. s_lOS2 .OR. s_lMinGW
      FClose( nMKhandle )
   ENDIF

   IF s_lMsVcc
      s_cLinkCommands +=" /nologo " + IIF( s_lGui, "/SUBSYSTEM:WINDOWS"," /SUBSYSTEM:CONSOLE") + " /force:multiple "
   ENDIF

   qqout( HbMake_Id() )
   qout( HbMake_Copyright() )

   IF s_lBcc .OR. s_lPocc .OR. s_lMSVcc
      qout( version() + ' / '+HB_Compiler() )
   ELSE
      qout(s_cLinkCommands)
   ENDIF

RETURN NIL

*----------------------
FUNCTION CompileFiles()
*----------------------

   LOCAL cComm
   LOCAL cOld
   LOCAL nPos
   LOCAL nFiles
   LOCAL cErrText := ""
   LOCAL aOrder   := ListAsArray2( s_aBuildOrder[ 2 ], " " )
   LOCAL lEnd     := .F.
   LOCAL xItem
   LOCAL lLinux   :=  s_lLinux
   LOCAL cPrg     := ''
   LOCAL cOrder   := ""
   LOCAL nFile    := 1
   LOCAL aGauge   := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B", '≤' )


   @  4,  5 SAY "Compiling: "

   FOR EACH cOrder IN aOrder

      IF ! s_lExtended

         IF cOrder == "$(CFILES)"
            nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".prg.c:" } )

            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ELSE
               nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".PRG.C:" } )

               IF nPos > 0
                  cComm := s_aCommands[ nPos, 2 ]
                  cOld  := cComm
               ENDIF

            ENDIF

            FOR EACH cPrg IN s_aPrgs

               xItem := Substr( cPrg, Rat( IIF( s_lGcc, '/', '\' ), ;
                                cPrg ) + 1 )
               nPos := AScan( s_aCFiles, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), ;
                  Left( x, At( ".", x ) ) == Left( xItem, At( ".", xItem ) ) } )

               IF nPos > 0
                  IF s_lMSVcc //.OR. s_lPocc
                     cComm := Strtran( cComm, "-Fo$*", "-Fo" + s_aCFiles[ nPos ] )
                  ELSE
                     cComm := Strtran( cComm, "o$*", "o" + s_aCFiles[ nPos ] )
                  ENDIF
                  cComm := Strtran( cComm, "$**", cPrg )
                  cComm += IIF( s_lLinux ,  " > "+ (s_cLog)," >>"+ (s_cLog))

                  Outstd( cComm )
                  Outstd( CRLF )
                  setpos(9,0)
                  __RUN( (cComm) )
                  cErrText := Memoread( (s_cLog) )
                  lEnd     := 'C2006' $ cErrText .OR. 'No code generated' $ cErrText

                  IF ! s_lIgnoreErrors .AND. lEnd
                     __run( s_cEditor +" "+(s_cLog) )
                     SET CURSOR ON
                     QUIT
                  ELSE
                     // Ferase( s_cLog )
                  ENDIF

                  cComm := cOld

               ENDIF

            NEXT

         ENDIF

         IF cOrder == "$(OBJFILES)"

            IF s_lGcc
               nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".c.o:" .OR. x[ 1 ] == ".cpp.o:" } )
            ELSE
               nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".c.obj:" .OR. x[ 1 ] == ".cpp.obj:" } )
            ENDIF

            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ELSE

               IF s_lGcc
                  nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".C.O:" } )

                  IF nPos > 0
                     cComm := s_aCommands[ nPos, 2 ]
                     cOld  := cComm
                  ENDIF

               ELSE
                  nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".C.OBJ:" } )

                  IF nPos > 0
                     cComm := s_aCommands[ nPos, 2 ]
                     cOld  := cComm
                  ENDIF

               ENDIF

            ENDIF

            FOR nFiles := 1 TO Len( s_aCFiles )

               xItem := Substr( s_aCFiles[ nFiles ], Rat( IIF( s_lGcc, '/', '\' ), ;
                                s_aCFiles[ nFiles ] ) + 1 )
               nPos := AScan( s_aObjs, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), ;
                       Left( x, At( ".", x ) ) == Left( xItem, At( ".", xItem ) ) } )

               IF nPos > 0

                  IF llinux
                     cComm := Strtran( cComm, "o$*", "o" + s_aObjs[ nPos ] )
                  ELSE
                      IF s_lMSVcc //.OR. s_lPocc
                         cComm := Strtran( cComm, "-Fo$*", "-Fo" + Strtran( s_aObjs[ nPos ], '/', '\' ) )
                      ELSE
                         cComm := Strtran( cComm, "o$*", "o" + Strtran( s_aObjs[ nPos ], '/', '\' ) )
                      ENDIF
                  ENDIF

                  cComm := Strtran( cComm, "$**", s_aCFiles[ nFiles ] )
                  cComm += IIF( s_lLinux ,  " > "+ (s_cLog)," >>"+ (s_cLog))
                  Outstd( " " )
                  Outstd( cComm )
                  Outstd( CRLF )
                  setpos(9,0)
                  __RUN( (cComm) )
                  cComm := cOld
               ENDIF

            NEXT

         ENDIF

      ELSE /****** Extended mode *****/

         IF cOrder == "$(CFILES)"

            IF s_lGcc
               nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".c.o:" .OR. x[ 1 ] == ".cpp.o:" } )
            ELSE
               nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".c.obj:" .OR. x[ 1 ] == ".cpp.obj:" } )
            ENDIF

            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ELSE
               nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".C.OBJ:" } )

               IF nPos > 0
                  cComm := s_aCommands[ nPos, 2 ]
                  cOld  := cComm
               ENDIF

            ENDIF

            IF Len( s_aCFiles ) > 0

               GaugeDisplay( aGauge )
               nFile := 1

               FOR nFiles := 1 TO Len( s_aCFiles )
                  @  4, 16 SAY Space( 50 )
                  xItem := Substr( s_aCFiles[ nFiles ], Rat( IIF( s_lGcc, '/', '\' ), ;
                                   s_aCFiles[ nFiles ] ) + 1 )
                  nPos := AScan( s_aObjsC, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), ;
                          Left( x, At( ".", x ) ) == Left( xitem, At( ".", xitem ) ) } )

                  IF nPos > 0

                     IF llinux
                        cComm := Strtran( cComm, "o$*", "o" + s_aObjsC[ nPos ] )
                     ELSE
                        IF s_lMSVcc //.OR. s_lPocc
                           cComm := Strtran( cComm, "-Fo$*", "-Fo" + Strtran( s_aObjsC[ nPos ], '/', '\' ) )
                        ELSE
                           cComm := Strtran( cComm, "o$*", "o" + Strtran( s_aObjsC[ nPos ], '/', '\' ) )
                        ENDIF
                     ENDIF

                     cComm := Strtran( cComm, "$**", s_aCFiles[ nFiles ] )

                     cComm += IIF( s_lLinux ,  " "," >>"+ (s_cLog))

                     @4,16 SAY s_aCFiles[ nFiles ]
                     GaugeUpdate( aGauge, nFile / Len( s_aCFiles ) )   // Changed s_aPrgs to s_aCFiles, Ath 2004-06-08
                     nFile ++
                     //Outstd( cComm )
                     setpos(9,0)
                     if s_lMingw
                     cComm := strtran(cComm ,"\","/")
                     endif
                     __RUN( (cComm) )
                     cErrText := Memoread( (s_cLog) )
                     lEnd     := 'Error E' $   cErrText
                     IF ! s_lIgnoreErrors .AND. lEnd
                        __run( s_cEditor + " "+(s_cLog) )
                        SET CURSOR ON
                        QUIT
                     ELSE
                        // FErase( s_cLog )
                     ENDIF
                     lEnd := 'Error F' $   cErrText

                     cComm := cOld

                  ENDIF

               NEXT

            ENDIF

         ENDIF

         IF cOrder == "$(OBJFILES)"

            IF s_lGcc
               nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".prg.o:" } )
            ELSE
               nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".prg.obj:" } )
            ENDIF

            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ELSE

               IF s_lGcc
                  nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".PRG.O:" } )
               ELSE
                  nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".PRG.OBJ:" } )
               ENDIF

            ENDIF

            GaugeDisplay( aGauge )
            nFile := 1

            FOR EACH cPrg IN s_aPrgs

               @  4, 16 SAY Space( 50 )
               xItem := Substr( cPrg, Rat( IIF( s_lGcc, '/', '\' ), ;
                                cPrg ) + 1 )
               nPos := AScan( s_aObjs, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), ;
                       Left( x, At( ".", x ) ) == Left( xItem, At( ".", xitem ) ) } )

               IF nPos > 0

                  IF llinux
                     cComm := Strtran( cComm, "o$*", "o" + s_aObjs[ nPos ] )
                  ELSE
//                     IF s_lMSVcc //.OR. s_lPocc
//                        cComm := Strtran( cComm, "-Fo$*", "-Fo" + Strtran( s_aObjs[ nPos ], '/', '\' ) )
//                     ELSE
                        cComm := Strtran( cComm, "o$*", "o" + Strtran( s_aObjs[ nPos ], '/', '\' ) )

//                     ENDIF
                  ENDIF

                  cComm := Strtran( cComm, "$**", cPrg )
                  cComm += IIF( s_lLinux ,  " > "+ (s_cLog)," >>"+ (s_cLog))


                  @4,16 SAY cPrg
                  GaugeUpdate( aGauge, nFile / Len( s_aPrgs ) )
                  // Outstd( CRLF )
                  nFile ++
                  setpos(9,0)
                  __RUN( (cComm) )
                  cErrText := Memoread( (s_cLog) )
                  lEnd     := 'C2006' $ cErrText .OR. 'No code generated' $ cErrText .or. "Error E" $ cErrText .or. "Error F" $ cErrText

                  IF ! s_lIgnoreErrors .AND. lEnd
                     __run( s_cEditor + " "+(s_cLog) )
                     SET CURSOR ON
                     QUIT
                  ELSE
                     //FErase( s_cLog )
                  ENDIF

                  cComm := cOld

               ENDIF

            NEXT

         ENDIF

      ENDIF

      IF cOrder == "$(RESDEPEN)"
         nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".rc.res:" } )

         IF nPos > 0
            cComm := s_aCommands[ nPos, 2 ]
            cOld  := cComm
         ENDIF

         FOR nFiles := 1 TO Len( s_aResources )

            IF ! Empty( s_aResources[ nFiles ] )
               cComm := Strtran( cComm, "$<", s_aResources[ nFiles ] )
               outstd( " " )
               ? cComm
               setpos(9,0)
               __RUN( (cComm) )
            ENDIF

            cComm := cOld

         NEXT

      ENDIF

   NEXT

RETURN NIL

*-------------------------------
FUNCTION GetParaDefines( cTemp )
*-------------------------------

   LOCAL nPos
   LOCAL cRead
   LOCAL aSet     := {}
   LOCAL nMakePos

   IF  "\.." IN cTemp
      cTemp := Substr( cTemp, 1, At( "\..", cTemp ) - 1 )
   ELSEIF  "/.." IN cTemp
      cTemp := Substr( cTemp, 1, At( "/..", cTemp ) - 1 )
   ENDIF
   if at("=",ctemp) == 0
      cTemp += [=""]
   endif
   aSet := ListAsArray2( cTemp, "=" )
   nPos := AScan( s_aDefines, { | x | x[ 1 ] == aSet[ 1 ] } )

   IF nPos == 0
      cRead    := Alltrim( Strtran( aSet[ 2 ], "$(", "" ) )
      cRead    := Strtran( cRead, ")", "" )
      nMakePos := AScan( s_aDefines, { | x | x[ 1 ] == cRead } )

      IF nMakePos = 0
         if(aSet[1] == "MYDEFINES")
         ASet[ 2 ] := Strtran( aSet[ 2 ], ",", ";" )
         else
         ASet[ 2 ] := Strtran( aSet[ 2 ], ",", " " )
         endif
         AAdd( s_aDefines, { aSet[ 1 ], aSet[ 2 ] } )
         AAdd( s_aMacros, { aSet[ 1 ], aSet[ 2 ] } )
      else
         s_aDefines[nMakepos,2] +=";"+aSet[ 2 ]
         s_aMacros[nMakepos,2]+=";"+ aSet[ 2 ]

      ENDIF

   ENDIF

RETURN NIL

*---------------------
FUNCTION PrintMacros()
*---------------------

   Outstd( HbMake_Id()+ " "+HbMake_Copyright()+ CRLF )
   Outstd( "" + CRLF )
   Outstd( "Macros:" + CRLF )
   AEval( s_aMacros, { | xItem | Outstd( "     " + xItem[ 1 ] + " = " + xItem[ 2 ] + CRLF ) } )
   Outstd( "Implicit Rules:" + CRLF )
   AEval( s_aCommands, { | xItem | Outstd( "     " + xItem[ 1 ] + CRLF + "        " + xItem[ 2 ] + CRLF ) } )
   Outstd( "" + CRLF )
   Outstd( "Targets:" )
   Outstd( "    " + s_szProject + ":" + CRLF )
   Outstd( "        " + "Flags :" + CRLF )
   Outstd( "        " + "Dependents :" )
   AEval( s_aCFiles, { | xItem | Outstd( xitem + " " ) } )
   AEval( s_aObjs, { | xItem | Outstd( xitem + " " ) } )
   Outstd( " " + CRLF )
   Outstd( "        commands:" + s_aBuildOrder[ Len( s_aBuildOrder ) ] )
   Outstd( " " + CRLF )
   Outstd( " " + CRLF )
   Outstd( " " + CRLF )

RETURN NIL

*--------------------------------------------------
FUNCTION CreateScript( cFile, lCreateAndCompile )
*--------------------------------------------------
* Create script file to hbmake build makefile.lnk
*--------------------------------------------------
   LOCAL aInFiles     := {}
   LOCAL aOutFiles    := {}
   LOCAL aOutc        := {}
   LOCAL aSrc         := Directory( "*.prg" )
   LOCAL nLenaSrc     := Len( aSrc )

   LOCAL lFwh         := .F.
   LOCAL lMiniGui     := .F.
   LOCAL lHwGui       := .F.
   LOCAL lWhat32      := .F.
   LOCAL lGtCgi       := .F.
   LOCAL lGtWvt       := .F.
   LOCAL lGtWvw       := .F.
   LOCAL lMWvw        := .F.
   LOCAL lXwt         := .F.
   LOCAL lxHGtk       := .F.

   LOCAL lRddAds      := .F.
   LOCAL lMediator    := .F.
   LOCAL lApollo      := .F.

// LOCAL lMt          := .F.
   LOCAL cOS          := IIF( s_lUnix, "Unix", IIF( s_lLinux, "Linux", iif(s_lOS2,"OS/2","Win32") ) )
   LOCAL cCompiler    := IIF( s_lLinux .OR. s_lGcc, "GCC",iif(s_lPocc,"POCC",iif(s_lMSVcc,"MSVC","BCC")))

   // External GUI Libs
   LOCAL cFwhPath     := Space( 200 )
   LOCAL cMiniPath    := Space( 200 )
   LOCAL cHwPath      := Space( 200 )
   LOCAL cxHGPath     := Space( 200 )

   LOCAL cMedPath     := Space( 200 )
   LOCAL cApolloPath  := Space( 200 )

   LOCAL cObjDir      := s_cObjDir + space( 20 )
   LOCAL lAutoMemvar  := .F.
   LOCAL lVarIsMemvar := .F.
   LOCAL lDebug       := .F.
   LOCAL lSupressLine := .F.
   LOCAL nPos
   LOCAL cHarbourFlags  := ""

// LOCAL nWarningLevel :=0

   LOCAL lUseXharbourDll := .F.

   LOCAL lCompMod         := .F.

//   LOCAL lGenppo          := .F.
   LOCAL x
   LOCAL getlist          := {}
   LOCAL cTopFile         := Space(50)
   LOCAL cAppName         := padr(s_cAppName,50)
   LOCAL cDefaultLibs     := "lang.lib vm.lib rtl.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib dbffpt.lib common.lib gtwin.lib codepage.lib ct.lib tip.lib pcrepos.lib hsx.lib hbsix.lib zlib.lib"
   LOCAL cDefGccLibs      := "-lvm -lrtl -lpcrepos -lgtdos -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -ldbffpt -lhsx -lhbsix -lcommon -lcodepage  -lm "
   LOCAL cDefGccLibsUnix  := "-lvm -lcodepage -ltef -lrtl  -lrdd  -lrtl  -lvm  -lmacro  -lpp  -llang  -lcommon  -lnulsys  -lbmdbfcdx  -ldbfntx  -ldbfcdx  -ldbffpt  -lhbsix  -lhsx  -lusrrdd -lpcrepos  -lgtsln -lslang -lm -lrt "
   #ifdef __ARCH64BIT__
   LOCAL cDefGccLibsw     := "-lvm64 -lrtl64 -lpcrepos64 -lgtwin64 -llang64 -lrdd64 -lrtl64 -lvm64 -lmacro64 -lpp64 -ldbfntx64 -ldbfcdx64 -ldbffpt64 -lhsx64 -lhbsix64 -lcommon64 -lcodepage64 -lm"
   #else
   LOCAL cDefGccLibsw     := "-lvm -lrtl -lpcrepos -lgtwin -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -ldbffpt -lhsx -lhbsix -lcommon -lcodepage -lm"
   #endif
   LOCAL cGccLibsOs2      := "-lvm -lrtl -lpcrepos -lgtos2 -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -ldbffpt -lhsx -lhbsix -lcommon -lcodepage -lm"
   LOCAL cDefLibGccLibs   := "-lvm -lrtl -lpcrepos -lgtcrs -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -ldbffpt -lhsx -lhbsix -lcommon -lcodepage "
   LOCAL cDefaultLibsMt    := "lang.lib vmmt.lib rtlmt.lib rddmt.lib macromt.lib ppmt.lib dbfntxmt.lib dbfcdxmt.lib  dbffptmt.lib common.lib gtwin.lib codepage.lib ctmt.lib tipmt.lib pcrepos.lib hsxmt.lib hbsixmt.lib zlib.lib"
   LOCAL cDefGccLibsUnixMt := "-lvmmt -lcodepage -ltef -lrtlmt  -lrddmt  -lrtl  -lvmmt  -lmacromt  -lpp  -llang  -lcommon  -lnulsys  -lbmdbfcdx  -ldbfntx  -ldbfcdx  -ldbffpt  -lhbsix  -lhsx  -lusrrdd -lpcrepos -lgtsln -lslang -lm -lrt"
   LOCAL cDefGccLibsMt    := "-lvmmt -lrtlmt -lpcrepos -lgtdos -llang -lrddmt -lrtlmt -lvmmt -lmacromt -lppmt -ldbfntxmt -ldbfcdxmt -ldbffptmt -lhsxmt -lhbsixmt -lcommon -lcodepage -lm"
   LOCAL cDefGccLibsMtw    := "-lvmmt -lrtlmt -lpcrepos -lgtwin -llang -lrddmt -lrtlmt -lvmmt -lmacromt -lppmt -ldbfntxmt -ldbfcdxmt -ldbffptmt -lhsxmt -lhbsixmt -lcommon -lcodepage -lm"
   LOCAL cGccLibsOs2Mt    := "-lvmmt -lrtlmt -lpcrepos -lgtos2 -llang -lrddmt -lrtlmt -lvmmt -lmacromt -lppmt -ldbfntxmt -ldbfcdxmt -ldbffptmt -lhsxmt -lhbsixmt -lcommon -lcodepage -lm"
//   LOCAL cDefLibGccLibsMt := "-lvmmt -lrtlmt -lpcrepos -lgtcrs -llang -lrddmt -lrtlmt -lvmmt -lmacromt -lppmt -ldbfntxmt -ldbfcdxmt -ldbffptmt -lhsxmt -lhbsixmt -lcommon -lcodepage"
   LOCAL cDefLibGccLibsMt := "-lvmmt -lrtlmt -lpcrepos -lgtsln -llang -lrddmt -lrtlmt -lvmmt -lmacromt -lppmt -ldbfntxmt -ldbfcdxmt -ldbffptmt -lhsxmt -lhbsixmt -lcommon -lcodepage"
   LOCAL cHarbDll         := "xharbour.lib"
   LOCAL cHARso           := "-lxharbour -lncurses -lgpm -lslang -lpthread -lm"
   LOCAL cSystemLibs      := If( s_lUnix, "", "-lncurses " ) + "-lslang " + If( s_lUnix, "", "-lgpm " ) + " -lpthread -lm"

   LOCAL cLibs        := ""
   LOCAL citem        := ""
   LOCAL cExt         := ""
   LOCAL cDrive       := ""
   LOCAL cPath        := ""
   LOCAL cTest        := ""
   LOCAL cGTLib       := "CONSOLE"
   LOCAL aLibs
   LOCAL aLibsIn      := {}
   LOCAL aLibsOut     := {}
   LOCAL cGt          := ""

   LOCAL cOldLib      := ""
   LOCAL cHtmlLib     := ""
   LOCAL lLinux       := s_lLinux
   LOCAL lUnix        := s_lUnix
   LOCAL nWriteFiles  := 0
   LOCAL cResName     := space(200)
   LOCAL aSelFiles

   LOCAL cBuild       := " "
   LOCAL cBuildForced := " "
   LOCAL cBuildParam  := NIL

   LOCAL aUserDefs
   LOCAL cCurrentDef  := ""
   LOCAL cRdd         := "None"
   LOCAL cCurrentDir  := ""
   LOCAL nOption
   LOCAL lNew         := .F.
   LOCAL oMake
   LOCAL cAllRes      := ""
   LOCAL cTemp
   LOCAL cExtraLibs   :=""
   LOCAL cTempLibs    := ""
   LOCAL aTempLibs

   LOCAL aUserLibs
   LOCAL cUserLib

   #IFdef HBM_USE_DEPENDS
      LOCAL cIncl              := ""
      LOCAL lScanIncludes      := .F.
      // Provisions for recursive scanning
      LOCAL lScanIncRecursive := .F.
      LOCAL cExcludeExts       := PadR(".ch",40)
   #ENDIF

   #ifndef __PLATFORM__Windows
       LOCAL lHashhso := File("/usr/lib/libxharbour.so")  .or. File("/usr/lib64/libxharbour.so") .or. File("/usr/local/lib/libxharbour.so") .or. File("/usr/local/lib64/libxharbour.so")
       LOCAL lusexhb := FILE("/usr/bin/xhb-build") .or. FILE("/usr/local/bin/xhb-build")
   #ELSE
       LOCAL lusexhb := .F.
   #ENDIF

   LOCAL cHarbourLibDir := s_cHarbourDir + iif(s_lLinux,"/lib","\lib")
   LOCAL lCancelMake := .F.
   LOCAL nSFhandle   // script file handle

   s_cUserInclude  := space(200)
   s_cUserDefine   := space(200)
   s_cUserLibs     := space(200)


   IF File( cFile )

      CLS

      IF s_nLang == 1 // Portuguese-BR
         nOption := Alert( "O makefile <" + cFile +"> j† existe.",{ "Editar", "Criar Novo" , "Cancelar" } )
      ELSEIF s_nLang == 3 // Spanish
         nOption := Alert( "Lo makefile <" + cFile +"> ya existe.",{ "Editar", "Crear Nuevo" , "Cancelar" } )
      ELSE // English
         nOption := Alert( "The makefile <" + cFile +"> already exist ",{ "Edit" , "Create New" , "Cancel" } )
      ENDIF


      IF nOption = 1 // edit makefile

         // Verify if "cFile" can be openned to write mode.

         nSFhandle := FOpen( cFile, FO_WRITE )

         IF nSFhandle = F_ERROR

            CLS

            IF s_nLang = 1      // brazilian portuguese
               s_cAlertMsg := "<"+cFile + "> n∆o pode ser aberto para ediá∆o."
            ELSEIF s_nLang = 3  // spanish
               s_cAlertMsg := "<"+cFile + "> no pode ser abierto para edici¢n."
            ELSE                // english
               s_cAlertMsg := "<"+cFile + "> cannot be openned for edition."
            ENDIF

            Alert( s_cAlertMsg+" FERROR ("+LTrim(Str(FError()))+")" )

            RETURN NIL
         ELSE
            FClose( nSFhandle )
         ENDIF

         oMake :=THbMake():new()
         oMake:cMakefile := cFile
         oMake:cMacro := iif(s_lMSVcc,"#MSVC",iif(s_lPocc,"#POCC",iif(s_lGcc,"#GCC","#BCC")))
         oMake:ReadMakefile(cFile)

         FRename(cFile,cFile+".old")

         IF LEN(oMake:aRes) >0
            FOR EACH cTemp IN oMake:aRes
                cAllRes += cTemp+ " "
            NEXT
         ENDIF

         lAutoMemVar     := oMake:lAutomemvar
         lVarIsMemVar    := oMake:lvarismemvar
         lDebug          := oMake:ldebug
         lSupressline    := oMake:lSupressline
         lCompMod        := oMake:lCompMod
         s_lGenppo       := oMake:lGenppo
         s_lGui          := oMake:lGui
         cRdd            := IIF( oMake:lRddAds, "RddAds",;
                            IIF( oMake:lMediator, "Mediator", "None" ) )
         cGTLib          := IIF( oMake:lFwh   , "FWH", ;
                            IIF( oMake:lMiniGui  , "MINIGUI", ;
                            IIF( oMake:lHwGui , "HWGUI", ;
                            IIF( oMake:lCgi , "GTCGI", ;
                            IIF( oMake:lGtWvt , "GTWVT/GTWVG", ;
                            IIF( oMake:lMWvW  , "GTWVW+MWVW", ;
                            IIF( oMake:lGtWvw , "GTWVW", ;
                            IIF( oMake:lXWt   , "XWT", ;
                            IIF( oMake:lWhat32, "WHAT32", ;
                            IIF( oMake:lxHGtk , "XHGTK", "CONSOLE" ) ) ) ) ) ) ) ) ) ) 
         cFwhpath        := padr(oMake:cFmc,200)
         cApolloPath     := padr(oMake:cFmc,200)
         cMiniPath       := padr(oMake:cFmc,200)
         cHwPath         := padr(oMake:cFmc,200)
         cxHGPath        := padr(oMake:cFmc,200)
         cMedpath        := padr(oMake:cMedpath,200)
         cAppName        := padr(oMake:cAppLibName,50)
         s_cAppName      := cAppName
         s_lCompress     := oMake:lCompress
         s_lExternalLib  := oMake:lExternalLib
         s_cUserInclude  := padr(oMake:cUserInclude,200)
         s_cUserDefine   := padr(oMake:cUserDef,200)
         s_cUserLibs     := padr(oMake:cUserLib,200)
         s_lxFwh         := oMake:lxFwh
         s_nFilesToAdd   := oMake:cFilesToAdd
         s_lMt           := oMake:lMt
         s_nWarningLevel := oMake:cWarningLevel
         cTopFile        := PadR(oMake:cTopModule,50," ")
         cResName        := PadR(oMake:cRes,200)
         s_cObjDir       := oMake:cObj
         cObjDir         := s_cObjDir + space(20)
         s_lGenCsource   := oMake:lGenCsource
         s_cEditor       := trim(oMake:cEditor)

         if Empty( s_cEditor )
            s_cEditor := HBMake_Editor()
         endif

         if !s_lRecursive
            s_lRecursive := oMake:lRecurse
         endif


         IF nLenaSrc == 0 .and. !s_lRecursive

            CLS

            IF s_nLang=1 // PT-BR
               s_cAlertMsg := "N∆o h† nenhum prg na pasta "+CurDir()+". Use o modo recursivo -r"
            ELSEIF s_nLang=3 // Spanish
               s_cAlertMsg := "No hay ning£n prg en la carpeta "+CurDir()+". Use lo modo recursivo -r"
            ELSE
               s_cAlertMsg := "Does not have any prg in "+CurDir()+" folder. Use the recursive mode -r"
            ENDIF

            Alert( s_cAlertMsg )

            SetColor("W/N,N/W")
            CLS
            set cursor on
            QUIT

         ENDIF

         if s_lCancelRecursive
            s_lRecursive := .F.
         endif

         // after oMake read, recreate other clean makefile to edit.
         nSFhandle := FCreate(cFile)

         if nSFhandle = F_ERROR

            CLS

            IF s_nLang = 1      // brazilian portuguese
               s_cAlertMsg := "<"+cFile + "> n∆o pode ser aberto para ediá∆o."
            ELSEIF s_nLang = 3  // spanish
               s_cAlertMsg := "<"+cFile + "> no pode ser abierto para edici¢n."
            ELSE                // english
               s_cAlertMsg := "<"+cFile + "> cannot be openned for edition."
            ENDIF

            Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )

            RETURN NIL

         endif

         WriteMakeFileHeader(nSFhandle)

         s_lEditMake := .T.


      ELSEIF nOption = 2 // create a new makefile


         IF nLenaSrc == 0 .and. !s_lRecursive

            CLS

            IF s_nLang=1 // PT-BR
               s_cAlertMsg := "N∆o h† nenhum prg na pasta "+CurDir()+". Use o modo recursivo -r"
            ELSEIF s_nLang=3 // Spanish
               s_cAlertMsg := "No hay ning£n prg en la carpeta "+CurDir()+". Use lo modo recursivo -r"
            ELSE
               s_cAlertMsg := "Does not have any prg in "+CurDir()+" folder. Use the recursive mode -r"
            ENDIF

            Alert( s_cAlertMsg )
            SetColor("W/N,N/W")
            CLS
            set cursor on
            QUIT

         ENDIF

         s_lEditMake := .F.

         nSFhandle := FCreate( cFile )

         if nSFhandle = F_ERROR

            CLS

            IF s_nLang = 1      // brazilian portuguese
               s_cAlertMsg := "<"+cFile + "> n∆o pode ser criado."
            ELSEIF s_nLang = 3  // spanish
               s_cAlertMsg := "<"+cFile + "> no pode ser criado."
            ELSE                // english
               s_cAlertMsg := "<"+cFile + "> cannot be created."
            ENDIF

            Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )

            RETURN NIL

         endif


         WriteMakeFileHeader(nSFhandle)
         lNew := .T.

      ELSE
         SetColor("W/N,N/W")
         CLS
         set cursor on
         QUIT
      ENDIF

   ELSE

      nSFhandle := FCreate( cFile )

      if nSFhandle = F_ERROR

         CLS

         IF s_nLang = 1      // brazilian portuguese
            s_cAlertMsg := "<"+cFile + "> n∆o pode ser criado."
         ELSEIF s_nLang = 3  // spanish
            s_cAlertMsg := "<"+cFile + "> no pode ser criado."
         ELSE                // english
            s_cAlertMsg := "<"+cFile + "> cannot be created."
         ENDIF

         Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )

         RETURN NIL

      ENDIF

      WriteMakeFileHeader(nSFhandle)
      nOption := 2  // create a new makefile
      lNew := .T.

   ENDIF

IF ! lCreateAndCompile

While .t.

   CLS
   Setcolor( 'w/b+,b+/w,w+/b,w/b+,w/b,w+/b' )
   @  0,  0, Maxrow(), Maxcol() BOX( Chr( 201 ) + Chr( 205 ) + Chr( 187 ) + Chr( 186 ) + Chr( 188 ) + Chr( 205 ) + Chr( 200 ) + Chr( 186 ) + Space( 1 ) )

   Attention( HbMake_Id() + space(10)+s_aLangMessages[ 27 ], 0 )

   Attention( s_aLangMessages[ 47 ], maxrow() )

   @ 01,01       SAY s_aLangMessages[ 28 ]

   @ 01,16,06,21 GET cOS;
                 LISTBOX { "Win32", "OS/2", "Linux","Unix" };
                 MESSAGE s_aLangMessages[ 49 ];
                 STATE OsSpec(getlist,1,@cOS);
                 DROPDOWN

   @ 01,23       SAY s_aLangMessages[ 29 ]

   @ 01,47,08,52 GET cCompiler;
                 LISTBOX { "BCC", "MSVC", "GCC", "POCC","MINGW" };
                 MESSAGE s_aLangMessages[ 50 ];
                 STATE OsSpec(getlist,2,@cCompiler);
                 DROPDOWN

   @ 01,55       SAY s_aLangMessages[ 30 ]

   @ 01,66,10,78 GET cGTLib;
                 LISTBOX { "CONSOLE","FWH","GTCGI","GTWVT/GTWVG","GTWVW","GTWVW+MWVW","HWGUI","MINIGUI","XWT","WHAT32","XHGTK"};
                 STATE OsSpec(getlist,3,@cGTLib);
                 DROPDOWN;
                 WHEN CheckCompiler(cOS);
                 MESSAGE s_aLangMessages[ 51 ]

   @ 02,01       SAY s_aLangMessages[ 48 ]

   @ 02,16,08,26 GET cRdd;
                 LISTBOX { "None","RddAds","Mediator","Apollo"};
                 WHEN cOS == "Win32" .or. cOS == "Linux";
                 DROPDOWN;
                 MESSAGE s_aLangMessages[ 52 ]

   @ 02,30       GET s_lCompress;
                 CHECKBOX;
                 CAPTION s_aLangMessages[ 53 ];
                 STYLE "[X ]";
                 MESSAGE s_aLangMessages[ 54 ]

   @ 02,53       GET lUseXHarbourDll;
                 CHECKBOX;
                 CAPTION "use xHarbour[.dll|.so]" style "[X ]";
                 WHEN cOS == "Win32" .or. cOS == "Linux";
                 MESSAGE s_aLangMessages[ 55 ]

   @ 03,01       SAY "Obj Files Dir";
                 GET cObjDir;
                 PICT "@S20";
                 MESSAGE s_aLangMessages[ 56 ]

   @ 03,47       SAY s_aLangMessages[ 66 ]

#ifdef __PLATFORM__Windows
   @ 03,68,07,77 GET s_cEditor;
                 LISTBOX { "edit", "notepad" };
                 MESSAGE s_aLangMessages[ 67 ];
                 STATE   OsSpec(getlist,4,@s_cEditor);
                 DROPDOWN
#endif
   @ 04,01       SAY s_aLangMessages[ 45 ];
                 GET cAppName;
                 PICT "@S15";
                 VALID ! Empty( cAppName );
                 MESSAGE s_aLangMessages[ 57 ]

   @ 04,53       GET s_lasdll;
                 CHECKBOX;
                 CAPTION "Create dll";
                 STYLE "[X ]"

   READ MSG AT MaxRow() - 1, 1, MaxCol() - 1

   s_cAppName := alltrim( cAppName )

   IF cOS != "Linux" .or.  cOS != "Unix"
      IF s_lasdll
         s_cAppName += ".dll"
      ELSE
         s_cAppName += ".exe"
      ENDIF
   ENDIF

   if s_lasdll
      lUseXharbourDll:= .T.
   endif

   lFwh      := "FWH"      IN alltrim(cGTLib)
   lMiniGui  := "MINIGUI"  IN alltrim(cGTLib)
   lHwGui    := "HWGUI"    IN alltrim(cGTLib)
   lWhat32   := "WHAT32"   IN alltrim(cGTLib)
   lGtCgi    := "GTCGI"    IN alltrim(cGTLib)
   lGtWvt    := "GTWVT"    IN alltrim(cGTLib)
   lGtWvw    := "GTWVW"    IN alltrim(cGTLib)
   lMWvw     := "MWVW"     IN alltrim(cGTLib)
   lXwt      := "XWT"      IN alltrim(cGTLib)
   lxHGtk    := "XHGTK"    IN alltrim(cGTLib)

   s_lGui := lFwh .or. lMinigui .or. lGtWvt .or. lHwGui .or. lXwt .or. lWhat32 .or. lxHGtk .or. lGtWvw .or. lMWvw

   lRddAds   := "RddAds"   IN cRdd
   lMediator := "Mediator" IN cRdd
   lApollo   := "Apollo"   IN cRdd

   IF lUseXharbourDll
      cDefLibGccLibs := cHARso
      if s_lasdll
         cDefaultLibs   := cHarbDll + " dllmain.lib "
      else
         cDefaultLibs   := " use_dll.lib " + cHarbDll 
      endif
   ENDIF

   IF lFwh
      @  3, 40 SAY space(maxcol()-40-1)
      @  3, 40 SAY "FWH path";
               GET cFwhPath;
               PICT "@S25"
   ELSEIF lMiniGui
      @  3, 40 SAY space(maxcol()-40-1)
      @  3, 40 SAY "MiniGui path";
               GET cMiniPath;
               PICT "@S25"
   ELSEIF lHwGui
      @  3, 40 SAY space(maxcol()-40-1)
      @  3, 40 SAY "HwGUI path";
               GET cHwPath;
               PICT "@S25"
   ELSEIF lxHGtk
      @  3, 40 SAY space(maxcol()-40-1)
      @  3, 40 SAY "xHGtk path";
               GET cxHGPath;
               PICT "@S25"
   ENDIF

   IF lMediator
      @  3, 40 SAY space(maxcol()-40-1)
      @  3, 40 SAY "Mediator path";
               GET cMedPath;
               PICT "@S25"
   ENDIF

   IF lApollo
      @  3, 40 SAY space(maxcol()-40-1)
      @ 03, 40 SAY "Apollo path";
               GET cApolloPath;
               PICT "@S25"
   ENDIF

   IF nOption = 2 // create a new makefile
      cResName := PadR(alltrim(cResName)+iIF(!empty(cResName)," ","")+alltrim(cAllRes),200 )
   ENDIF

   Attention( s_aLangMessages[ 31 ], 5 )

   @ 06, 01 GET lAutoMemVar;
            CHECKBOX;
            CAPTION s_aLangMessages[ 32 ];
            STYLE "[X ]"

   @ 06, 40 GET lVarIsMemVar;
            CHECKBOX;
            CAPTION s_aLangMessages[ 33 ];
            STYLE "[X ]"

   @ 07, 01 GET lDebug;
            CHECKBOX;
            CAPTION s_aLangMessages[ 34 ];
            STYLE "[X ]"

   @ 07, 40 GET lSupressLine;
            CHECKBOX;
            CAPTION s_aLangMessages[ 35 ];
            STYLE "[X ]"

   @ 08, 01 GET s_lGenppo;
            CHECKBOX;
            CAPTION s_aLangMessages[ 36 ];
            STYLE "[X ]"

   @ 08, 40 GET lCompMod;
            CHECKBOX;
            CAPTION s_aLangMessages[ 37 ];
            STYLE "[X ]"

   @ 09, 01 SAY s_aLangMessages[ 38 ];
            GET s_cUserDefine;
            PICT "@s23"

   @ 09, 40 SAY s_aLangMessages[ 39 ];
            GET s_cUserInclude;
            PICT "@s18"

   @ 10, 01 GET s_lExternalLib;
            CHECKBOX;
            CAPTION s_aLangMessages[ 40 ];
            STYLE "[X ]"

   @ 10, 40 GET s_lxFwh;
            CHECKBOX;
            CAPTION "xHarbour FWH";
            STYLE "[X ]"

   @ 11, 01 SAY "Resource file Name: ";
            GET cResName;
            PICT "@S55"

   @ 12, 01 SAY s_aLangMessages[ 43 ];
            GET s_nFilestoAdd;
            PICT "99";
            VALID s_nFilestoAdd > 0

   @ 13, 01 GET s_lMt;
            CHECKBOX;
            CAPTION s_aLangMessages[ 44 ];
            STYLE "[X ]"

   @ 13, 40 SAY s_aLangMessages[ 46 ];
            GET s_nWarningLevel;
            PICT "9";
            VALID s_nWarningLevel>=0 .AND. s_nWarningLevel <= 4

   @ 14, 01 GET s_lGenCsource;
            CHECKBOX;
            CAPTION "Generate C-source, not PCode (-go3)";
            STYLE "[X ]"

   @ 15, 01 SAY s_aLangMessages[ 65 ];
            GET s_cUserLibs;
            PICT "@S58"

   READ msg at maxrow()-1,1,maxcol()-1

   IF LastKey()==K_ESC
      SET CURSOR ON
      SetColor("W/N")
      CLS
      RETURN NIL
   ENDIF

   IF Empty( cAppName )
      IF s_nLang == 1
         Alert("Falta nome da aplicaá∆o.")
      ELSEIF s_nLang == 3
         Alert("Falta lo nombre de la aplicacion.")
      ELSE
         Alert("Application name is missing.")
      ENDIF
   ELSE
      EXIT
   ENDIF

Enddo

Endif // Create and compile

   IF ! Empty( s_cUserDefine )
      aUserDefs := ListasArray2(Alltrim( s_cUserDefine ), ";")

      FOR EACH cCurrentDef in aUserDefs
         cHarbourFlags += " -D" + Alltrim( cCurrentDef ) + " "
      NEXT
   ENDIF

   IF ! Empty( s_cUserLibs )
      aUserLibs := ListAsArray2( alltrim(s_cUserLibs), ";" )
      FOR EACH cUserLib IN aUserLibs
          if !file( cUserLib )
             alert( "User Lib: "+cUserLib +" not found.")
             exit
          endif
      NEXT
   ENDIF

   IF ! Empty( s_cUserInclude )
      cHarbourFlags += " -I" + Alltrim( s_cUserInclude ) + " "
   ENDIF

   s_lBcc   := "BCC"   IN cCompiler
   s_lMSVcc := "MSVC"  IN cCompiler
   s_lGcc   := "GCC"   IN cCompiler
   s_lPocc  := "POCC"  IN cCompiler
   s_lMinGW := "MINGW" IN cCompiler

   if s_lMinGW
      s_lGcc := .T.
      s_lBcc := s_lMSVcc := s_lPocc := .F.
   endif

   cObjDir  := Alltrim( cObjDir )

   IF "Linux" in cOS .or. "Unix" in cOS
       cCurrentDir := "/"+CurDir()
   ELSE
       cCurrentDir := CurDrive()+":\"+CurDir()
   ENDIF

   IF ! Empty( cObjDir )

      IF !IsDirectory( cObjDir )
         MakeDir( cObjDir )
      ENDIF

   ENDIF

   s_aMacros := GetSourceDirMacros( s_lGcc, cOS )

   IF lLinux .or. lUnix
      cObjDir := Alltrim( cObjDir )

      IF ! Empty( cObjDir )
         cObjDir += '/'
      ENDIF

      cTest := cObjDir
   ELSE
      cObjDir := Alltrim( cObjDir )

      IF ! Empty( cObjDir )
         cObjDir += '\'
      ENDIF

      cTest := Upper( cObjDir ) + '\'
   ENDIF

   AEval( s_aMacros, { | x, y | cItem := Substr( x[ 2 ], 1, Len( x[ 2 ] ) ), IIF( At( citem, cTest ) > 0, ( s_aMacros[ y, 1 ] := 'OBJ', s_aMacros[ y, 2 ] := cObjDir ), ) } )

   IF lAutomemvar
      cHarbourFlags += " -a "
   ENDIF

   IF lvarismemvar
      cHarbourFlags += " -v "
   ENDIF


   IF lDebug
      cHarbourFlags    += " -b "
      cDefaultLibs     += " debug.lib "
      cDefGccLibs      += " -ldebug "
      cDefGccLibsw     += " -ldebug "
      cGccLibsOs2      += " -ldebug "
      cDefLibGccLibs   += " -ldebug "
      cDefGccLibsUnix  += " -ldebug "
      cDefGccLibsUnixMt+= " -ldebug "
      cDefaultLibsMt   += " debug.lib "
      cDefGccLibsMt    += " -ldebug "
      cDefGccLibsMtw   += " -ldebug "
      cGccLibsOs2Mt    += " -ldebug "
      cDefLibGccLibsMt += " -ldebug "
   ENDIF

   IF lSupressline
      cHarbourFlags += " -l "
   ENDIF

   IF s_lGenppo
      cHarbourFlags += " -p "
   ENDIF

   IF lCompmod
      cHarbourFlags += " -m "
   ENDIF

   if lMWvw
      cHarbourFlags += ' -u+mwvw.ch '
   endif


   IF s_nWarningLevel >= 0
      cHarbourFlags += " -w" + Str(s_nWarningLevel,1)
   ENDIF


   IF s_lBcc

      AAdd( s_aCommands, { ".cpp.obj:", "$(CC_DIR)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $**" } )
      AAdd( s_aCommands, { ".c.obj:", "$(CC_DIR)\BIN\bcc32 -I$(HB_DIR)\include $(CFLAG1) $(CFLAG2) -o$* $**" } )

      IF s_lExtended
         AAdd( s_aCommands, { ".prg.obj:", "$(HB_DIR)\bin\harbour -D__EXPORT__ -n"+if(s_lasdll,"1","")+" -go" + if(s_lGenCsource,"3","") + " -I$(HB_DIR)\include $(HARBOURFLAGS)" + IIF( lFwh, " -I$(FWH)\include", IIF( lMinigui, " -I$(MINIGUI)\include",IIF( lHwgui, " -I$(HWGUI)\include","" ) ) )+  IIF( lMediator," -I$(MEDIATOR)\include ","")+" -o$* $**" } )
      ELSE
         AAdd( s_aCommands, { ".prg.c:", "$(HB_DIR)\bin\harbour -n -I$(HB_DIR)\include $(HARBOURFLAGS)" + IIF( lFwh, " -I$(FWH)\include", IIF( lMinigui, " -I$(MINIGUI)\include",IIF( lHwgui, " -I$(HWGUI)\include","" ) )) + " -o$* $**" } )
      ENDIF

      AAdd( s_aCommands, { ".rc.res:", "$(CC_DIR)\BIN\brcc32 $(RFLAGS) $<" } )

   ELSEIF s_lGcc

      IF  ("linux" IN Lower(Getenv( "HB_ARCHITECTURE" ) )  .OR. cOS == "Linux" ) .or.;
          ("unix" IN Lower(Getenv( "HB_ARCHITECTURE" ) )  .OR. cOS == "Unix" .OR. cOS == "HP-UX" )
         AAdd( s_aCommands, { ".cpp.o:", "gcc $(CFLAG1) $(CFLAG2) -o$* $**" } )
         AAdd( s_aCommands, { ".c.o:", "gcc -I/usr/include/xharbour  -I/usr/local/include/xharbour  $(CFLAG1) $(CFLAG2) -I. -g -o$* $**" } )

         IF s_lExtended
            AAdd( s_aCommands, { ".prg.o:", "harbour -D__EXPORT__  -n"+if(s_lasdll,"1","")+"  -go" + if(s_lGenCsource,"3","") + " -I/usr/include/xharbour  -I/usr/local/include/xharbour  $(HARBOURFLAGS) -I.  -o$* $**" } )
         ELSE
            AAdd( s_aCommands, { ".prg.c:", "harbour -n -I/usr/include/xharbour  -I/usr/local/include/xharbour  $(HARBOURFLAGS) -I.  -o$* $**" } )
         ENDIF

      ELSE
         AAdd( s_aCommands, { ".cpp.o:", "$(CC_DIR)\bin\gcc $(CFLAG1) $(CFLAG2) -o$* $**" } )
         AAdd( s_aCommands, { ".c.o:", "$(CC_DIR)\bin\gcc -I$(HB_DIR)/include $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )

         IF s_lExtended
            AAdd( s_aCommands, { ".prg.o:", "$(HB_DIR)\bin\harbour -D__EXPORT__  -n"+if(s_lasdll,"1","")+" -go" + if(s_lGenCsource,"3","") + " -I$(HB_DIR)/include $(HARBOURFLAGS) " +IIF( lHwgui, " -I$(HWGUI)/include","" ) +" -o$* $**" } )
         ELSE
            AAdd( s_aCommands, { ".prg.c:", "$(HB_DIR)\bin\harbour -n -I$(HB_DIR)/include $(HARBOURFLAGS) " +IIF( lHwgui, " -I$(HWGUI)/include","" ) +"   -o$* $**" } )
         ENDIF

      ENDIF

   ELSEIF s_lMSVcc

      AAdd( s_aCommands, { ".cpp.obj:", "$(CC_DIR)\bin\cl $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
      AAdd( s_aCommands, { ".c.obj:", "$(CC_DIR)\bin\cl -I$(HB_DIR)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )

      IF s_lExtended
         AAdd( s_aCommands, { ".prg.obj:", "$(HB_DIR)\bin\harbour -D__EXPORT__  -n -I$(HB_DIR)\include $(HARBOURFLAGS)  -go" + if(s_lGenCsource,"3","")+ IIF( lFwh, " -I$(FWH)\include", IIF( lMinigui, " -I$(MINIGUI)\include",IIF( lHwgui, " -I$(HWGUI)\include","" ) ) )+ + IIF( lMediator," -I$(MEDIATOR)\include ","")+ " -o$* $**" } )
      ELSE
         AAdd( s_aCommands, { ".prg.c:", "$(HB_DIR)\bin\harbour -n -I$(HB_DIR)\include $(HARBOURFLAGS) -o$* $**" } )
      ENDIF

      AAdd( s_aCommands, { ".rc.res:", "$(CC_DIR)\rc $(RFLAGS) $<" } )

   ELSEIF s_lPocc

      AAdd( s_aCommands, { ".cpp.obj:", "$(CC_DIR)\BIN\pocc $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
      AAdd( s_aCommands, { ".c.obj:", "$(CC_DIR)\BIN\pocc -I$(HB_DIR)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )

      IF s_lExtended
         AAdd( s_aCommands, { ".prg.obj:", "$(HB_DIR)\bin\harbour -D__EXPORT__ -n"+if(s_lasdll,"1","")+" -go" + if(s_lGenCsource,"3","") + " -I$(HB_DIR)\include $(HARBOURFLAGS)" + IIF( lFwh, " -I$(FWH)\include", IIF( lMinigui, " -I$(MINIGUI)\include",IIF( lHwgui, " -I$(HWGUI)\include","" ) ) )+  IIF( lMediator," -I$(MEDIATOR)\include ","")+" -o$** $**" } )
      ELSE
         AAdd( s_aCommands, { ".prg.c:", "$(HB_DIR)\bin\harbour -n -I$(HB_DIR)\include $(HARBOURFLAGS)" + IIF( lFwh, " -I$(FWH)\include", IIF( lMinigui, " -I$(MINIGUI)\include",IIF( lHwgui, " -I$(HWGUI)\include","" ) )) + " -o$** $**" } )
      ENDIF

      AAdd( s_aCommands, { ".rc.res:", "$(CC_DIR)\BIN\porc $(RFLAGS) $<" } )

   ENDIF


   // Selecting PRG files.

   aInFiles := GetSourceFiles( s_lRecursive, s_lGcc, cOS )
   nLenaSrc := Len( aInFiles )

   IF nLenaSrc == 0

      IF s_nLang=1 // PT-BR
         s_cAlertMsg := "Nenhum prg foi encontrado."
      ELSEIF s_nLang=3 // Spanish
         s_cAlertMsg := "Ning£n prg foi encontrado."
      ELSE
         s_cAlertMsg := "No one prg were found."
      ENDIF

      lCancelMake := .T.

      Alert( s_cAlertMsg )

   ENDIF


   IF "Win32" IN cOS
      AEval( aInFiles, { | , y| aInFiles[y] := Upper( aInFiles[y] ) } )
   ENDIF

   aOutFiles := AClone( aInFiles )

   //if Len( aOutFiles ) > 1
   if Len( aOutFiles ) > 1 .AND. ! lCreateAndCompile

      Attention( s_aLangMessages[ 41 ], 22 )

      if s_nLang=1
         s_cAlertMsg := "Selecione os PRGs a compilar"
      elseif s_nLang=3
         s_cAlertMsg := "Seleccione los PRG a compilar"
      else
         s_cAlertMsg := "Select the PRG files to compile"
      endif


      IF nOption !=2 // not create a makefile
         pickarry( 11, 15, 20, 64, aInFiles, aOutFiles ,ArrayAJoin( { oMake:aPrgs, oMake:aCs } ), .T., s_cAlertMsg )
      ELSE
         pickarry( 11, 15, 20, 64, aInFiles, aOutFiles, {}, .T., s_cAlertMsg )
      ENDIF


      AEval( aOutFiles, { | , y | aOutFiles[ y ] := Trim( Substr( aOutFiles[ y ], 1, At( ' ', aOutFiles[ y ] ) ) ) } )

      aOutFiles := ASort( aOutFiles )

      @ 22,01 say space(78)

      aSelFiles := GetSelFiles( aInFiles, aOutFiles )

      ASort( aSelFiles )

   else
       AEval( aOutFiles, { | , y | aOutFiles[ y ] := Trim( Substr( aOutFiles[ y ], 1, At( ' ', aOutFiles[ y ] ) ) ) } )
       aSelFiles := aOutFiles
   endif


   if Len( aSelFiles ) = 1

      cTopFile := aSelFiles[1]
      cTopFile := PadR( Left(cTopfile,At(Upper(".prg"),Upper(cTopFile))+4 ), 50)

   elseif Len( aSelFiles ) = 0

      cTopFile := ""

      IF s_nLang=1 // PT
         s_cAlertMsg := "Nenhum PRG foi selecionado."
      ELSEIF s_nLang=3
         s_cAlertMsg := "Ning£m PRG foi seleccionado."
      ELSE
         s_cAlertMsg := "No one PRG were selected."
      ENDIF

      Alert( s_cAlertMsg )

   endif


   WHILE Len( aSelFiles ) > 1

     IF !lCreateAndCompile

      IF s_nLang=1 // PT
         s_cAlertMsg := "Informe o PRG principal da sua aplicaá∆o:"
      ELSEIF s_nLang=3
         s_cAlertMsg := "Informe o PRG principale de su aplicacion:"
      ELSE
         s_cAlertMsg := "Inform the main PRG of your application:"
      ENDIF

      @ 15,01 say s_cAlertMsg Get cTopFile pict "@S35" valid !empty(cTopFile)
      READ

      if LastKey() = K_ESC
         Exit
      endif

     ELSE
      cTopFile := alltrim(cAppName)+".prg"
     ENDIF

      IF "Win32" IN cOS
         cTopFile := Upper( cTopFile )
      ENDIF


      IF ! File( alltrim(cTopFile) )

         IF s_nLang=1 // PT
            s_cAlertMsg := "Arquivo "+alltrim(cTopFile)+" n∆o encontrado."+iif(s_lRecursive," O flag -r est† ativo. Informe o subdir tambÇm se o PRG principal estiver dentro dele.","")
         ELSEIF s_nLang=3
            s_cAlertMsg := "Fichero "+alltrim(cTopFile)+" no encontrado."+iif(s_lRecursive," Lo flag -r esta activado. Informe lo subdir tambiÇn si lo PRG principale est†s dentro dele.","")
         ELSE
            s_cAlertMsg := "File "+alltrim(cTopFile)+" not found."+iif(s_lRecursive," The flag -r is active. Inform the subdir also if the main PRG is within it.","")
         ENDIF

         Alert( s_cAlertMsg )

         IF lCreateAndCompile
            FClose( nSFhandle )
            FErase( cFile )
            RETURN NIL
         ENDIF

      ELSE
         EXIT
      ENDIF

   END


   // Selecting External Libs.
   IF s_lExternalLib

      aLibs := GetLibs( s_lGcc, cHarbourLibDir )

      if len(aLibs)=0
         alert("aLibs is empty")
      endif

      IF s_nLang == 1 // PT
         s_cAlertMsg := '<Espaáo> para selecionar. <Enter> para continuar o processo.'
      ELSEIF s_nLang == 2
         s_cAlertMsg := '<Spacebar> to select. <Enter> to continue process'
      ELSEIF s_nLang == 3
         s_cAlertMsg := '<Espacio> para seleccionar. <Enter> para continuar o proceso.'
      ENDIF

      Attention( s_cAlertMsg, 22 )

      AEval( aLibs, { | x | AAdd( aLibsIn, x[ 1 ] ) } )
      AEval( aLibs, { | x | AAdd( aLibsOut, x[ 2 ] ) } )

      if s_nLang=1
         s_cAlertMsg := "Selecione as LIBs externas a compilar"
      elseif s_nLang=3
         s_cAlertMsg := "Seleccione las LIB externas a compilar"
      else
         s_cAlertMsg := "Select the external LIBs to compile"
      endif


      IF nOption != 2 // not create makefile
         pickarry( 11, 15, 20, 64, aLibsIn, aLibsOut ,oMake:aExtLibs, .T. , s_cAlertMsg, .T. )
      ELSE
         pickarry( 11, 15, 20, 64, aLibsIn, aLibsOut ,{}, .T., s_cAlertMsg, .T. )
      ENDIF

   ENDIF

#IFDEF HBM_USE_DEPENDS

   if ! lCreateAndCompile

   clear typeahead

   Attention( "HbMake options", 16 )

   @ 17, 01 GET lScanIncludes;
            CHECKBOX;
            CAPTION "Create #DEPENDS from #include";
            STYLE "[X ]"
   // Provisions for recursive scanning
   @ 17, 40 GET lScanIncRecursive;
            CHECKBOX;
            CAPTION "Scan recursive" style "[X ]" //when lScanIncludes
   @ 18, 01 SAY "Excluding these extensions :" ;
            GET cExcludeExts;
            WHEN lScanIncludes
   READ

   Endif

#ENDIF

   if ! lCreateAndCompile

   AEval( aOutFiles, { | xItem | IIF(  '.c'IN xItem  .OR.  '.C' IN xItem , AAdd( aOutc, xItem ), ) } )
   AEval( aOutc, { | x, z | cItem := x, z := AScan( aOutFiles, { | t | t = cItem } ), IIF( z > 0, aSize( aDel( aOutFiles, z ), Len( aOutFiles ) - 1 ), ) } )

   @ 22,01 say space(78)

   aOutFiles  := ASort( aOutFiles )

   else
      aOutFiles := { alltrim(cAppName) }
   endif

   s_aPrgs    := AClone( aOutFiles )
   s_aObjs    := AClone( aOutFiles )

   s_aExtLibs := AClone( aLibsOut )

   // searching for main prg file into obj array.
   x := AScan( s_aObjs, { | x | Lower( x ) in Lower( alltrim(cTopFile) ) } )

   // putting main prg in the top
   IF x > 0
      ADel( s_aObjs, x )
      ASize( s_aObjs, Len( s_aObjs ) - 1 )
      ASize( s_aObjs, Len( s_aObjs ) + 1 )
      AIns( s_aObjs, 1 )
      s_aObjs[ 1 ] := AllTrim( cTopFile )
   ENDIF

   // searching for main prg file into prg array.
   x := AScan( s_aPrgs, { | x | Lower( x ) in Lower( alltrim(cTopFile) ) } )

   // putting main prg in the top
   IF x > 0
      ADel( s_aPrgs, x )
      ASize( s_aPrgs, Len( s_aPrgs ) - 1 )
      ASize( s_aPrgs, Len( s_aPrgs ) + 1 )
      AIns( s_aPrgs, 1 )
      s_aPrgs[ 1 ] :=  AllTrim( cTopFile )
   ENDIF

   AEval( s_aObjs, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, s_aObjs[ x ] := cObjDir + cTest + "." + Exte( cExt, 2 ), s_aObjs[ x ] := cObjDir + cTest + "." + Exte( cExt, 3 ) ) } )
   s_aCFiles := aClone( aOutc )

   IF ! s_lExtended
      AEval( aOutc, { | xItem | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, AAdd( s_aObjs, cObjDir + cTest + "." + Exten( cExt, 2 ) ), AAdd( s_aObjs, cObjDir + cTest + "." + Exten( cExt, 1 ) ) ) } )
      AEval( aOutFiles, { | xItem | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cExt := Substr( cExt, 2 ), AAdd( s_aCFiles, cObjDir + cTest + "." + Exte( cExt, 1 ) ) } )
   ELSE
      s_aObjsC := aClone( aOutc )
      AEval( aOutc, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, s_aObjsC[ x ] := IIF( ! Empty( cObjDir ), cObjDir, '' ) + cTest + "." + Exten( cExt, 2 ), s_aObjsC[ x ] := IIF( ! Empty( cObjDir ), cObjDir, '' ) + cTest + "." + Exten( cExt, 1 ) ) } )
   ENDIF

   FWrite( nSFhandle, "RECURSE=" + IIF( s_lRecursive, " YES ", " NO " ) + CRLF )
   FWrite( nSFhandle, " " + CRLF )
   FWrite( nSFhandle, "SHELL = " + CRLF )
   FWrite( nSFhandle, "COMPRESS = " + IIF( s_lCompress, "YES", "NO" ) + CRLF )
   FWrite( nSFhandle, "EXTERNALLIB = " + IIF( s_lExternalLib, "YES", "NO" ) + CRLF )
   FWrite( nSFhandle, "XFWH = " + IIF( s_lxFwh, "YES", "NO" ) + CRLF )
   FWrite( nSFhandle, "FILESTOADD = " + Str( s_nFilesToAdd, 2 ) + CRLF )
   FWrite( nSFhandle, "WARNINGLEVEL = " + Str(s_nWarningLevel, 2) + CRLF )
   FWrite( nSFhandle, "USERDEFINE = " + alltrim(s_cUserDefine) + CRLF )
   FWrite( nSFhandle, "USERINCLUDE = " + alltrim(s_cUserInclude) + CRLF )
   FWrite( nSFhandle, "USERLIBS = " + alltrim(s_cUserLibs) + CRLF )
   FWrite( nSFhandle, "EDITOR = " + s_cEditor + CRLF )

   IF lFwh
      FWrite( nSFhandle, "FWH = " + alltrim(cFwhPath) + CRLF )
   ELSEIF lMiniGui
      FWrite( nSFhandle, "MINIGUI = " + alltrim(cMiniPath) + CRLF )
   ELSEIF lHwGui
      FWrite( nSFhandle, "HWGUI = " + if(!s_lMinGW,alltrim(cHwPath),strtran(alltrim(cHwPath),"\","/")) + CRLF )
   ELSEIF lGtwvt
      FWrite( nSFhandle, "GTWVT = " + CRLF )
   ELSEIF lGtwvw
      FWrite( nSFhandle, "GTWVW = " + CRLF )

      IF lMwvw
         FWrite( nSFhandle, "MWVW = " + CRLF )
      endif

   ELSEIF lXwt
      FWrite( nSFhandle, "XWT = " + CRLF )
   ELSEIF lWhat32
      FWrite( nSFhandle, "WHAT32 = " + CRLF )
   ELSEIF lxHGtk
      FWrite( nSFhandle, "XHGTK = " + CRLF )
   ENDIF

   IF lMediator
      FWrite( nSFhandle, "MEDIATOR = " + alltrim(cMedPath) + CRLF )
   ENDIF

   IF lApollo
      FWrite( nSFhandle, "APOLLO = " + alltrim(cApolloPath) + CRLF )
   ENDIF

   FWrite( nSFhandle, "CGI = " + iif( lGtCgi , "YES", "NO" ) + CRLF )
   FWrite( nSFhandle, "GUI = " + iif(lFwh .or. lMinigui .or. lGtWvt .or. lHwGui .or. lXwt .or. lWhat32 .or. lxHGtk .or. lGtWvw , "YES", "NO" ) + CRLF )
   FWrite( nSFhandle, "MT = " + IIF( s_lMt, "YES", "NO" ) + CRLF )

   FOR x := 1 TO Len( s_aMacros )

      IF ! Empty( s_aMacros[ x, 2 ] )

         cItem := s_aMacros[ x, 2 ]
         nPos  := AScan( s_aPrgs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0
            AEval( s_aPrgs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aPrgs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ')/', ')\' ) ), ) } )

            IF ! s_aMacros[ x, 3 ]
               FWrite( nSFhandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
               s_aMacros[ x, 3 ] := .T.
            ENDIF

         ENDIF

         nPos := AScan( s_aCFiles, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0

            IF ! s_aMacros[ x, 3 ]
               AEval( s_aCFiles, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aCFiles[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )
               FWrite( nSFhandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
               s_aMacros[ x, 3 ] := .T.
            ENDIF

         ENDIF

         nPos := AScan( s_aObjs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0

            IF ! Empty( cObjDir )
               AEval( s_aObjs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aObjs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )
               FWrite( nSFhandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
            ENDIF

         ENDIF

         IF s_lExtended
            nPos := AScan( s_aObjsC, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

            IF nPos > 0

               IF ! Empty( cObjDir )
                  AEval( s_aObjsC, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aObjsC[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )
               ENDIF

            ENDIF

         ENDIF

      ENDIF

   NEXT

   IF s_lGcc
      IF ( "linux" IN Lower( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOS == "Linux" ) .or.;
         ( "unix" IN Lower( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOS == "Unix" .or. cOS == "HP-UX" )
         FWrite( nSFhandle, "PROJECT = " + Alltrim( Lower( cAppName ) ) + " $(PR) " + CRLF )
      ELSE
         FWrite( nSFhandle, "PROJECT = " + Alltrim( Lower( cAppName ) ) + ".exe"   + " $(PR) " + CRLF )
      ENDIF
   ELSE
      FWrite( nSFhandle, "PROJECT = " + Alltrim( Lower( cAppName ) ) + if(s_lasdll,".dll",".exe" ) + " $(PR) " + CRLF )
   ENDIF


   IF ! s_lExtended

      FWrite( nSFhandle, "OBJFILES = " )

      IF Len( s_aObjs ) < 1
         FWrite( nSFhandle, + " $(OB) " + CRLF )
      ELSE
         AEval( s_aObjs, { | x, i | IIF( ( i <> Len( s_aObjs ) .AND. x <> alltrim(cTopfile)  ), FWrite( nSFhandle, ' ' + Alltrim( x ) ), FWrite( nSFhandle, " " + " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )
      ENDIF

      FWrite( nSFhandle, "CFILES =" )

      IF Len( s_aCFiles ) < 1
         FWrite( nSFhandle, + " $(CF)" + CRLF )
      ELSE
         AEval( s_aCFiles, { | x, i | IIF( ( i <> Len( s_aCFiles ) .AND. x <> alltrim(cTopfile)  ), FWrite( nSFhandle, ' ' + Alltrim( x ) ), FWrite( nSFhandle, " " + Alltrim( x ) + " $(CF) " + CRLF ) ) } )
      ENDIF

      FWrite( nSFhandle, "PRGFILE =" )

      IF Len( s_aPrgs ) < 1
         FWrite( nSFhandle, + " $(PS)" + CRLF )
      ELSE
         AEval( s_aPrgs, { | x, i | IIF( i <> Len( s_aPrgs) .AND. x <> alltrim(cTopfile) , FWrite( nSFhandle, ' ' + Alltrim( x ) ), FWrite( nSFhandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )
      ENDIF

   ELSE

      FWrite( nSFhandle, "OBJFILES =" )

      IF Len( s_aObjs ) < 1
         FWrite( nSFhandle, + " $(OB) " + CRLF )
      ELSE
         AEval( s_aObjs, { | x, i | nWriteFiles ++, IIF( ( i <> Len( s_aObjs ) .AND. x <> alltrim(cTopfile)  ), FWrite( nSFhandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % s_nFilestoAdd == 0, " //" + CRLF, "" ) ), FWrite( nSFhandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )
      ENDIF

      nWriteFiles := 0
      FWrite( nSFhandle, "PRGFILES =" )

      IF Len( s_aPrgs ) < 1
         FWrite( nSFhandle, + " $(PS)" + CRLF )
      ELSE
         AEval( s_aPrgs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aPrgs ), FWrite( nSFhandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % s_nFilestoAdd == 0, " //" + CRLF, "" ) ), FWrite( nSFhandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )
      ENDIF

      nWriteFiles := 0
      FWrite( nSFhandle, "OBJCFILES =" )

      IF Len( s_aObjsC ) < 1
         FWrite( nSFhandle, + " $(OBC) " + CRLF )
      ELSE
         AEval( s_aObjsC, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aObjsC ), FWrite( nSFhandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % s_nFilestoAdd == 0, " //" + CRLF, "" ) ), FWrite( nSFhandle, " " + Alltrim( x ) + " $(OBC) " + CRLF ) ) } )
      ENDIF

      nWriteFiles := 0
      FWrite( nSFhandle, "CFILES =" )

      IF Len( s_aCFiles ) < 1
         FWrite( nSFhandle, + " $(CF)" + CRLF )
      ELSE
         AEval( s_aCFiles, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aCFiles ), FWrite( nSFhandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % s_nFilestoAdd == 0, " //" + CRLF, "" ) ), FWrite( nSFhandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )
      ENDIF


   ENDIF

   cResName := Lower( alltrim(cResName) )
   FWrite( nSFhandle, "RESFILES = " + cResName + CRLF )
   FWrite( nSFhandle, "RESDEPEN = " + StrTran( cResName, ".rc", ".res" ) + CRLF )
   FWrite( nSFhandle, "TOPMODULE = " + alltrim(cTopFile) + CRLF )

   IF lRddads
      cDefaultLibs     += " rddads.lib ace32.lib"
      cDefLibGccLibs   += " -lrddads -ladsloc "
      cDefaultLibsMt   += " rddads.lib ace32.lib"
      cDefLibGccLibsMt += " -lrddads -ladsloc "
      cDefGccLibsw     += " -lrddads -lads32 "
      cExtraLibs += " -lrddads -ladsloc "
   ENDIF

   // remove obsolete bcc640xx.lib
   //
   IF "bcc640" IN cDefaultLibs
      cDefaultLibs   := StrTran( cDefaultLibs, "bcc640.lib", "")
   ENDIF

   IF "bcc640" IN cDefaultLibsMt
      cDefaultLibsMt := StrTran( cDefaultLibsMt, "bcc640mt.lib", "")
   ENDIF

   if lGtCgi
      cDefaultLibs   := StrTran( cDefaultLibs, "gtwin.lib", "gtwin.lib gtcgi.lib")
      cDefaultLibsMt := StrTran( cDefaultLibsMt, "gtwinmt.lib", "gtwinmt.lib gtcgimt.lib")
   endif

   // if external libs was selected...
   IF ! Empty( aLibsOut ) .and. s_lExternalLib 

      IF s_lMSVcc .OR. s_lBcc .OR. s_lPocc

         IF ! s_lMt
            cOldLib := cDefaultLibs
         ELSE
            cOldLib := cDefaultLibsMt
         ENDIF

         // searching for html/cgi lib...
         nPos := AScan( aLibsOut, { | z | At( "html", Lower( z ) ) > 0 } )
         if nPos = 0
            nPos := AScan( aLibsOut, { | z | At( "cgi", Lower( z ) ) > 0 } )
         endif

         IF nPos > 0  
            if nPos > 0
               cHtmlLib += aLibsOut[ nPos ]
               ADel( aLibsOut, nPos )
               ASize( aLibsOut, Len( aLibsOut ) - 1 )
            endif
            // cOldLib := StrTran( cOldLib, "gtwin", "gtcgi" )
         ENDIF

         // searching for mysql lib...
         AEval( aLibsOut, { | cLib | cLibs += " " + cLib } )
         nPos := AScan( aLibsOut, { | z | At( "mysql", Lower( z ) ) > 0 } )

         IF nPos >0
            cLibs += " libmysql.lib"
         ENDIF

         // searching for postgre lib...
         nPos := AScan( aLibsOut, { | z | At( "hbpg", Lower( z ) ) > 0 } )
         IF nPos >0
            cLibs += " libpq.lib"
            cLibs := strtran(cLibs,"hbpg","hbpg")
         ENDIF

         IF ! s_lmt
            cDefaultLibs := cHtmlLib + " " + cOldLib + " " + cLibs
         ELSE
            cDefaultLibsMt := cHtmlLib + " " + cOldLib + " " + cLibs
         ENDIF

      ENDIF

      IF s_lGcc

         nPos := AScan( aLibsOut, { | z | At( "html", Lower( z ) ) > 0 } )
         if nPos = 0
            nPos := AScan( aLibsOut, { | z | At( "cgi", Lower( z ) ) > 0 } )
         endif

         IF nPos > 0
            cHtmlLib += "-l" + Strtran( aLibsOut[ nPos ], '.a', "" )
            aDel( aLibsOut, nPos )
            aSize( aLibsOut, Len( aLibsOut ) - 1 )
         ENDIF

         AEval( aLibsOut, { | cLib | iif( Len(aTempLibs :=ListAsArray2( cLib, " ") )> 0 ,cLibs += SetthisLibs(AtempLibs) ,cLibs += " -l" + Strtran( cLib, '.a', "" ))} )

         nPos := AScan( aLibsOut, { | z | At( "mysql", Lower( z ) ) > 0 } )

         if nPos >0
            cLibs += " -lmysqlclient"
         endif
         nPos := AScan( aLibsOut, { | z | At( "hbpg", Lower( z ) ) > 0 } )

         if nPos >0
            cLibs += " -lpq"
         endif

         cExtraLibs := cLibs

         IF cOS == "Linux"

            IF ! s_lMt
               cOldLib        := " " + cDefLibGccLibs
               cDefLibGccLibs := cHtmlLib + " " + cOldLib + " " + cLibs

               IF "html" IN cDefLibGccLibs .or. "cgi" IN cDefLibGccLibs .or. lGtCgi
                   cDefLibGccLibs := StrTran( cDefLibGccLibs, "gtcrs" , "gtcgi" )
                   cDefLibGccLibs := StrTran( cDefLibGccLibs, "ncurses" , "" )
               ENDIF

            ELSE

               cOldLib          := " " + cDefLibGccLibsMt
               cDefLibGccLibsMt := cHtmlLib + " " + cOldLib + " " + cLibs

               IF "html" IN cDefLibGccLibsMt .or. "cgi" IN cDefLibGccLibsMt .or. lGtCgi
                   cDefLibGccLibsMt := StrTran( cDefLibGccLibsMt, "gtcrs" , "gtcgi" )
                   cDefLibGccLibsMt := StrTran( cDefLibGccLibsMt, "ncurses" , "" )
               ENDIF

           ENDIF

         ELSEIF cOS == "OS/2"

            IF ! s_lMt
               cOldLib     := " " + cGccLibsOs2
               cGccLibsOs2 := cHtmlLib + " " + cOldLib + " " + cLibs

               IF "html" IN cGccLibsOs2 .or. "cgi" IN cGccLibsOs2 .or. lGtCgi
                   cGccLibsOs2 := StrTran( cGccLibsOs2, "gtos2" , "gtcgi" )
               ENDIF

            ELSE
               cOldLib       := " " + cGccLibsOs2Mt
               cGccLibsOs2Mt := cHtmlLib + " " + cOldLib + " " + cLibs
               IF "html" IN cGccLibsOs2Mt .or. "cgi" IN cGccLibsOs2Mt .or. lGtCgi
                   cGccLibsOs2Mt := StrTran( cGccLibsOs2Mt, "gtos2" , "gtcgi" )
               ENDIF

            ENDIF

         ELSE

            IF s_lMt
               cOldLib       := " " + cDefGccLibsMt
               cDefGccLibsMt := cHtmlLib + " " + cOldLib + " " + cLibs
            else
               cOldLib       := " " + cDefGccLibsw
               cDefGccLibsw  := cHtmlLib + " " + cOldLib + " " + cLibs
            endif


        ENDIF

      ENDIF

   ENDIF


   IF s_lBcc .OR. s_lMSVcc .OR. s_lPocc
      if lFwh .or. lMiniGui .or. lHwGui .or. lWhat32
            cDefaultLibs   := strtran(cDefaultLibs,"gtwin.lib","gtgui.lib")
            cDefaultLibsMt := strtran(cDefaultLibsMt,"gtwin.lib","gtgui.lib")
      endif

      IF lFwh
         IF s_lxFwh
            FWrite( nSFhandle, "LIBFILES = $(FWH)\lib\fivehx.lib $(FWH)\lib\fivehc.lib " + IIF( ! s_lMt, cDefaultLibs, cDefaultLibsMt ) + CRLF )
         ELSE
            FWrite( nSFhandle, "LIBFILES = $(FWH)\lib\fiveh.lib $(FWH)\lib\fivehc.lib " + IIF( ! s_lMt, cDefaultLibs, cDefaultLibsMt ) + CRLF )
         ENDIF
      ELSEIF lMiniGui
         FWrite( nSFhandle, "LIBFILES = minigui.lib " + IIF( ! s_lMt, cDefaultLibs, cDefaultLibsMt ) + CRLF )
      ELSEIF lWhat32
         FWrite( nSFhandle, "LIBFILES = what32.lib " + IIF( ! s_lMt, cDefaultLibs, cDefaultLibsMt ) + CRLF )
      ELSEIF lHwGui
         FWrite( nSFhandle, "LIBFILES = hwgui.lib procmisc.lib  " + IIF( ! s_lMt, cDefaultLibs, cDefaultLibsMt ) + CRLF )
      ELSE
         if lGtwvt
            cDefaultLibs   := strtran(cDefaultLibs,"gtwin.lib","gtwvt.lib gtwvg.lib")
            cDefaultLibsMt := strtran(cDefaultLibsMt,"gtwin.lib","gtwvt.lib gtwvg.lib")
         elseif lGtwvw
            cDefaultLibs   := if(lMWvW,'mwvw.lib ', '') + strtran(cDefaultLibs,"gtwin.lib","gtwvw.lib ")
            cDefaultLibsMt := if(lMWvW,'mwvw.lib ', '') + strtran(cDefaultLibsMt,"gtwin.lib","gtwvw.lib ")

         endif

         FWrite( nSFhandle, "LIBFILES = " + IIF( ! s_lMt, cDefaultLibs, cDefaultLibsMt ) + CRLF )
      ENDIF

   ELSEIF s_lGcc

      IF cOS == "Linux"
         FWrite( nSFhandle, "LIBFILES = " + IIF(lusexhb, cExtraLibs , "-Wl,--start-group " + IIF( ! s_lMt, cDefLibGccLibs, cDefLibGccLibsMt ) + " -Wl,--end-group " + cSystemLibs ) + CRLF )
      ELSEIF cOS == "Unix" .or.  cOS == "UP-UX"
          FWrite( nSFhandle, "LIBFILES = " + IIF(lusexhb, cExtraLibs , " " + IIF( ! s_lMt, cDefGccLibsUnix, cDefGccLibsUnixmt ) + " " + cSystemLibs ) + CRLF )
      ELSEIF cOS == "OS/2"
         FWrite( nSFhandle, "LIBFILES = " + IIF( ! s_lMt, cGccLibsOs2, cGccLibsOs2Mt ) + CRLF )
      ELSEIF  "MINGW" IN cCompiler
         IF lHwGui
            cDefGccLibsw :=strtran( cDefGccLibsw,"-lgtwin" ,"-lgtgui")
            cDefGccLibsMtw :=strtran( cDefGccLibsMtw,"-lgtwin" ,"-lgtgui")
            FWrite( nSFhandle, "LIBFILES = " + "-Wl,--allow-multiple-definition -Wl,--start-group " + "-lhwgui -lprocmisc -lhwg_qhtm " +  IIF( ! s_lMt, cDefGccLibsw, cDefGccLibsMtw ) + " -Wl,--end-group " + CRLF)
         else
            FWrite( nSFhandle, "LIBFILES = " + "-Wl,--allow-multiple-definition -mconsole  -Wl,--start-group " + IIF( ! s_lMt, cDefGccLibsw, cDefGccLibsMtw ) + " -Wl,--end-group " + CRLF )
         endif
      ELSE
         FWrite( nSFhandle, "LIBFILES = " + IIF( ! s_lMt, cDefGccLibs, cDefGccLibs ) + CRLF )
      ENDIF

   ENDIF

   nWriteFiles := 0
   FWrite( nSFhandle, "EXTLIBFILES =" )

   if Len(s_aExtLibs) < 1
      FWrite( nSFhandle, CRLF )
   else
      AEval( s_aExtLibs, { | x | nWriteFiles++, FWrite( nSFhandle, " " + Alltrim( x )  ) } )
      FWrite( nSFhandle, CRLF )
   endif

   FWrite( nSFhandle, "DEFFILE = " + CRLF )
   FWrite( nSFhandle, "HARBOURFLAGS = " + cHarbourFlags + CRLF )


   IF s_lBcc

      FWrite( nSFhandle, "CFLAG1 =  -OS $(SHELL)  $(CFLAGS) -d -c -L$(HB_DIR)\lib"+iif(lFwh,";$(FWH)\lib ","")+iif(!empty(s_cUserInclude)," -I" + alltrim( s_cUserInclude ),"") + " " +CRLF )
      FWrite( nSFhandle, "CFLAG2 =  -I$(HB_DIR)\include;$(CC_DIR)\include" + iif( s_lMt, " -DHB_THREAD_SUPPORT " , "" ) + CRLF )

      FWrite( nSFhandle, "RFLAGS = " + CRLF )
/* added "-x" flag to LFLAGS statment to suppress creation of map file and speed up link. */
      FWrite( nSFhandle, "LFLAGS = -L$(CC_DIR)\lib\obj;$(CC_DIR)\lib;$(HB_DIR)\lib -Gn -M -m -s -Tp"+ if(s_lasdll,"d","e") + " -x" + IIF( lFWH .or. lMiniGui .or. lHwgui .or. lGtWvt .or. lGtWvw ," -aa"," -ap") + IIF( lMinigui, " -L$(MINIGUI)\lib",IIF( lFwh, " -L$(FWH)\lib",IIF( lHwgui, " -L$(HWGUI)\lib","" ))) + CRLF )
      FWrite( nSFhandle, "IFLAGS = " + CRLF )
      FWrite( nSFhandle, "LINKER = ilink32" + CRLF )
      FWrite( nSFhandle, " " + CRLF )
      FWrite( nSFhandle, "ALLOBJ = " + IIF( ( lWhat32 .OR. lFwh .OR. lMinigui .OR. lHwgui .or. lGtWvt .or. lGtWvw .or. lXwt .or. lxHGtk ), "c0w32.obj", if(s_lAsDll,"c0d32.obj","c0x32.obj" )) + " $(OBJFILES)" + IIF( s_lExtended, " $(OBJCFILES)", " " ) + CRLF )
      FWrite( nSFhandle, "ALLRES = $(RESDEPEN)" + CRLF )
      FWrite( nSFhandle, "ALLLIB = $(USERLIBS) $(LIBFILES) import32.lib " + IIF( s_lMt,"cw32mt.lib", "cw32.lib" )+ CRLF )
      FWrite( nSFhandle, ".autodepend" + CRLF )

   ELSEIF s_lMSVcc

      FWrite( nSFhandle, "CFLAG1 =  -I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(SHELL)  $(CFLAGS)" +IIF( s_lMt, " -DHB_THREAD_SUPPORT " , "" ) + CRLF )
      FWrite( nSFhandle, "CFLAG2 =  -c" +" -I" + alltrim( s_cUserInclude ) + " " + CRLF )
      FWrite( nSFhandle, "RFLAGS = " + CRLF )
      FWrite( nSFhandle, "LFLAGS = /LIBPATH:$(CC_DIR)\lib /LIBPATH:$(HB_DIR)\lib "  + if(lHwGui,"/LIBPATH:$(HWGUI)\LIB ","" ) +IIF(s_lMt, " /Nodefaultlib:LIBCMT "," /Nodefaultlib:LIBC " ) + iif( s_lGui," /SUBSYSTEM:WINDOWS"," /SUBSYSTEM:CONSOLE" ) + CRLF )
      FWrite( nSFhandle, "IFLAGS = " + CRLF )
      FWrite( nSFhandle, "LINKER = link" + CRLF )
      FWrite( nSFhandle, " " + CRLF )
      FWrite( nSFhandle, "ALLOBJ = " + "$(OBJFILES)" + IIF( s_lExtended, " $(OBJCFILES)", " " ) + CRLF )
      FWrite( nSFhandle, "ALLRES = $(RESDEPEN)" + CRLF )
      FWrite( nSFhandle, "ALLLIB = $(USERLIBS) $(LIBFILES) kernel32.lib user32.lib gdi32.lib winspool.lib comctl32.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib mpr.lib vfw32.lib winmm.lib ws2_32.lib " + CRLF )

   ELSEIF s_lPocc

      FWrite( nSFhandle, "CFLAG1 = $(SHELL)  /Ze /Go /Ot /Tx86-coff /I$(INCLUDE_DIR) $(C_USR) $(CFLAGS)" +IIF( s_lMt, ' /D"HB_THREAD_SUPPORT" /MT' , "" ) + CRLF )
      FWrite( nSFhandle, "CFLAG2 = " + CRLF )
      FWrite( nSFhandle, "RFLAGS = " + CRLF )
// use this line if you are using PellesC 5.x
//      FWrite( nSFhandle, "LFLAGS = /LIBPATH:$(CC_DIR)\LIB /LIBPATH:$(CC_DIR)\LIB\WIN /LIBPATH:$(HB_DIR)\LIB "+ if(lMinigui,"/LIBPATH:$(MINIGUI)\LIB ","" ) + if(lHwGui,"/LIBPATH:$(HWGUI)\LIB ","" ) + " /MACHINE:IX86" + iif( s_lGui," /SUBSYSTEM:WINDOWS"," /SUBSYSTEM:CONSOLE" ) + " /ALTERNATENAME:_WizMain=WizMain /ALTERNATENAME:_errno=___errno" + CRLF )
      FWrite( nSFhandle, "LFLAGS = /LIBPATH:$(CC_DIR)\LIB /LIBPATH:$(CC_DIR)\LIB\WIN /LIBPATH:$(HB_DIR)\LIB "+ if(lMinigui,"/LIBPATH:$(MINIGUI)\LIB ","" ) + if(lHwGui,"/LIBPATH:$(HWGUI)\LIB ","" ) + " /MACHINE:IX86" + iif( s_lGui," /SUBSYSTEM:WINDOWS"," /SUBSYSTEM:CONSOLE" ) + CRLF )
      FWrite( nSFhandle, "IFLAGS = " + CRLF )
      FWrite( nSFhandle, "LINKER = polink" + CRLF )
      FWrite( nSFhandle, " " + CRLF )
      FWrite( nSFhandle, "ALLOBJ = " + "$(OBJFILES)" + IIF( s_lExtended, " $(OBJCFILES)", " " ) + CRLF )
      FWrite( nSFhandle, "ALLRES = $(RESDEPEN)" + CRLF )
      FWrite( nSFhandle, "ALLLIB = $(USERLIBS) $(LIBFILES) "+IIF(s_lMT,"crtmt.lib","crt.lib") + " kernel32.lib user32.lib gdi32.lib winspool.lib comctl32.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib mpr.lib winmm.lib wsock32.lib schannel.lib comctl32.lib" + CRLF )

   ELSEIF s_lGcc

      FWrite( nSFhandle, "CFLAG1 = $(SHELL) " +IIF( !EMPTY(s_cUserInclude ) ," -I" + Alltrim( s_cUserInclude )+ " "  ,"") + IIF( "Unix" in cOs , " -I/usr/include/xharbour ", "" ) + IIF(  "Linux" IN cOS, "-I/usr/include/xharbour -I/usr/local/include/xharbour ", " -I$(HB_DIR)/include" ) + " -c -Wall" + IIF( s_lMt, " -DHB_THREAD_SUPPORT " , "" )  + " " + CRLF )
      #ifdef __ARCH64BIT__
      FWrite( nSFhandle, "CFLAG2 = " + IIF(  "Linux" IN cOS, "-L$(HB_LIB_INSTALL)", " -L$(HB_DIR)/lib  -L$(CC_DIR)/lib" ) +  IIF( "Unix" in cOs , " -L/usr/lib64/xharbour ", "" ) + IIF( lHwgui, " -L$(HWGUI)\lib","" ) + CRLF )
      #else
      FWrite( nSFhandle, "CFLAG2 = " + IIF(  "Linux" IN cOS, "-L$(HB_LIB_INSTALL)", " -L$(HB_DIR)/lib  -L$(CC_DIR)/lib" ) +  IIF( "Unix" in cOs , " -L/usr/lib/xharbour ", "" ) + IIF( lHwgui, " -L$(HWGUI)\lib","" ) + CRLF )
      #endif
      FWrite( nSFhandle, "RFLAGS = " + CRLF )
      FWrite( nSFhandle, "LFLAGS = " + if(!s_lLinux," ","-Wl,--noinhibit-exec ") + IIF(lUseXhb ,IIF(lUseXharbourDll,"","-static ") + if(lXwt .or. lhwgui ,"-gtcgi " , "-gtcrs "), "$(CFLAG2)") + iif(lXwt,"`pkg-config --libs gtk+-2.0` -lxwt -lxwt_gtk -lxwt","") + iif( lxHGtk, "`pkg-config --libs gtk+-2.0 libglade-2.0` -lxhgtk ","") + iif( lhwgui .and. !s_lMinGW, " `pkg-config --libs gtk+-2.0 libglade-2.0 libgnomeprint-2.2` -hwgui ","")  + iif(lhwgui .and. s_lMinGW," -mwindows " ,"" )+  iif(s_lLinux .and. s_lmt ," -mt "," "  ) +CRLF )
      FWrite( nSFhandle, "IFLAGS = " + CRLF )
      FWrite( nSFhandle, "LINKER = "+ IIF(lusexhb,"xhblnk","gcc") + CRLF )
      FWrite( nSFhandle, " " + CRLF )
      FWrite( nSFhandle, "ALLOBJ = $(OBJFILES) " + IIF( s_lExtended, " $(OBJCFILES)", " " ) + CRLF )
      FWrite( nSFhandle, "ALLRES = $(RESDEPEN) " + CRLF )
      FWrite( nSFhandle, "ALLLIB = $(USERLIBS) $(LIBFILES) " +if(s_lMinGW," -luser32 -lwinspool -lgdi32 -lcomctl32 -lcomdlg32 -lole32 -loleaut32 -luuid -lmpr -lwsock32 -lws2_32 -lmapi32","") + CRLF )
      FWrite( nSFhandle, ".autodepend" + CRLF )

   ENDIF

#IFdef HBM_USE_DEPENDS

   FWrite( nSFhandle, " " + CRLF )
   FWrite( nSFhandle, "#DEPENDS" + CRLF )

   IF lScanIncludes
      // Clipper/(x)Harbour sources: .prg
      IF Len( s_aPrgs ) = Len( s_aObjs )
         Attention("Scanning .PRG sources...",19)
         FOR nPos := 1 to Len(s_aPrgs)
            cIncl := ScanInclude( ReplaceMacros( s_aPrgs[ nPos ] ), lScanIncRecursive, cExcludeExts )
            // Only add in list if dependencies exist
            IF ! Empty(cIncl)
               FWrite( nSFhandle, s_aObjs[ nPos ] + ': ' + Alltrim( cIncl ) + CRLF, "" )
            ENDIF
         NEXT
      ENDIF

      // C-sources: .c
      IF Len( s_aCFiles ) = Len( s_aObjsC )
         Attention("Scanning .C sources...",19)
         FOR nPos := 1 to Len(s_aCFiles)
            cIncl := ScanInclude( s_aCFiles[ nPos ], lScanIncRecursive, cExcludeExts )
            // Only add in list if dependencies exist
            IF ! Empty(cIncl)
               FWrite( nSFhandle, s_aObjsC[ nPos ] + ': ' + Alltrim( cIncl ) + CRLF, "" )
            ENDIF
         NEXT
      ENDIF

      // Cleanup message
      @ 19, 1 say Space(MaxCol() - 2)
   ENDIF

#ENDIF

   FWrite( nSFhandle, " " + CRLF )
   FWrite( nSFhandle, "#COMMANDS" + CRLF )

   AEval( s_aCommands, { | xItem | FWrite( nSFhandle, xitem[ 1 ] + CRLF ), FWrite( nSFhandle, xitem[ 2 ] + CRLF ), FWrite( nSFhandle, " " + CRLF ) } )

   IF s_lBcc .OR. s_lMSVcc .OR. s_lPocc

      FWrite( nSFhandle, "#BUILD" + CRLF )
      FWrite( nSFhandle, " " + CRLF )
      FWrite( nSFhandle, "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)" + CRLF )
      FWrite( nSFhandle, "    $(CC_DIR)\BIN\$(LINKER) @&&!  " + CRLF )
      FWrite( nSFhandle, "    $(LFLAGS) +" + CRLF )
      FWrite( nSFhandle, "    $(ALLOBJ), +" + CRLF )
      FWrite( nSFhandle, "    $(PROJECT),, +" + CRLF )
      FWrite( nSFhandle, "    $(ALLLIB), +" + CRLF )
      FWrite( nSFhandle, "    $(DEFFILE), +" + CRLF )
      FWrite( nSFhandle, "    $(ALLRES)" + if(s_lPocc,","," ") + CRLF )
      FWrite( nSFhandle, "!" + CRLF )

   ELSEIF s_lGcc

      FWrite( nSFhandle, "#BUILD" + CRLF )
      FWrite( nSFhandle, " " + CRLF )
      FWrite( nSFhandle, "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)" + CRLF )

      IF 'Linux' IN cOS
         FWrite( nSFhandle, "    $(LINKER) @&&!" + CRLF )
      ELSEIF 'Unix' IN cOS .or. "HP-UX" IN cOS
         FWrite( nSFhandle, "    $(LINKER) @&&!" + CRLF )
      ELSE
         FWrite( nSFhandle, "    $(CC_DIR)\bin\$(LINKER) @&&!" + CRLF )
      ENDIF

      FWrite( nSFhandle, "    $(PROJECT) " + CRLF )
      FWrite( nSFhandle, "    $(ALLOBJ)  " + CRLF )
      FWrite( nSFhandle, "    $(LFLAGS)  " + CRLF )
      FWrite( nSFhandle, "    $(ALLLIB)  " + CRLF )
      FWrite( nSFhandle, "!" + CRLF )

   ENDIF

   FClose( nSFhandle )

   IF !lCancelMake

      IF s_nLang == 1 .OR. s_nLang == 3
         s_cAlertMsg := "Compilar app ? (S/N) "
      ELSE // English
         s_cAlertMsg := "Build app ? (Y/N) "
      ENDIF

      if ! lCreateAndCompile

      @ 20,5 SAY s_cAlertMsg;
             GET cBuild;
             PICT "!";
             VALID cBuild $ iif(s_nLang=2,"YN","SN")
      READ

      else
         cBuild := "Y"
      endif

      IF ( cBuild == "S" .or. cBuild == "Y" )

         IF lNew  .OR. Len(aSelFiles)=1
            cBuildForced := "Y"
         ELSE

            IF s_nLang == 1
               s_cAlertMsg := "Foráar recompilaá∆o para todos PRGs </f> ? (S/N) "
            ELSEIF s_nLang == 3 // Spanish
               s_cAlertMsg := "Forzar recompilaci¢n para todos los PRGs </f> ? (S/N) "
            ELSE // English
               s_cAlertMsg := "Force recompiling for all PRGs </f> ? (Y/N) "
            ENDIF

            @ 21,5 SAY s_cAlertMsg;
                   GET cBuildForced;
                   PICT "!";
                   VALID cBuildForced $ iif(s_nLang=2,"YN","SN")
            READ

         ENDIF

         IF cBuildForced == "S" .or. cBuildForced == "Y"
            cBuildParam := " -f " + iif(s_nLang=1,"-lPT",iif(s_nLang=3,"-lES","-lEN"))
         ENDIF

      ENDIF


      IF cBuild == "S" .OR. cBuild == "Y"
         ResetInternalVars()
         SetColor("W/N,N/W")
         Clear
         SetPos(9,0)
//       Main( cFile, " -f "+iif(s_nLang=1,"-lPT",iif(s_nLang=3,"-lES","-lEN")) )
         if cBuildParam != NIL
            // Main( cFile, cBuildParam )
            _AppMain( cFile, cBuildParam )
         else
            // Main( cFile )
            _AppMain( cFile )
         endif
      ELSE
         set cursor on
         setcolor("W/N,N/W")
         clear
      ENDIF

   ELSE
      set cursor on
      setcolor("W/N,N/W")
      clear
   ENDIF

RETURN NIL

#IfDef HBM_USE_DEPENDS

*------------------------------------------------------------
FUNCTION ScanInclude( cFile, lRecursive, cExclExtent, aFiles)
*------------------------------------------------------------
// Search for #Include & Set Procedure To & Set Proc To

   LOCAL cFileList := ""
   LOCAL nHandle   := -1
   LOCAL lEof      := .F.
   LOCAL cTemp     := ""
   LOCAL cBuffer   := ""
   LOCAL aQuotes   := {{'"','"'},{"'","'"},{"[","]"},{"<",">"}}
   LOCAL cQuote    := ""
   LOCAL cQuoteA   := ""
   LOCAL cInclude  := ""
   LOCAL lPrg      := .F.
   Local lC        := .F.
   LOCAL lCh       := .F.
   LOCAL cPath     := ""
   LOCAL cFnam     := ""
   LOCAL cExt      := ""
   LOCAL cDrive    := ""
   LOCAL cContinue := ""

   DEFAULT lRecursive  TO .F.
   DEFAULT cExclExtent TO ""    // specify extensions to exclude like ".ch.def" etc., including the dot
   DEFAULT aFiles      TO {}

   IF File(cFile)

       HB_FNAMESPLIT( cFile, @cPath, @cFnam, @cExt, @cDrive )

       lPrg := (Lower(cExt) == ".prg")
       lC := (Lower(cExt) == ".c")
       lCh := (Lower(cExt) == ".ch")
       cContinue := IIF(lPrg,";",IIF(lC,"\",""))

       nHandle := FOpen(cFile)

       IF nHandle != F_ERROR

           // Provisions for recursive scanning
           // Add current file to list, making it by default the first in the list

           IF s_lWin32
               IF AScan(aFiles, {| x | Lower( x ) == Lower( cFnam + cExt ) } ) = 0       // Case IN-sensitive!
                   AAdd(aFiles,  cFnam + cExt)
               ENDIF
           ELSE
               IF AScan(aFiles, cFnam + cExt ) = 0       // Case Sensitive!
                   AAdd(aFiles,  cFnam + cExt)
               ENDIF
           ENDIF

           lEof := (HB_FReadLine(nHandle,@cTemp,{chr(13)+chr(10), chr(10)}) = -1)
           cTemp := LTrim( cTemp )
           // Loop reading file
           WHILE !lEof
               IF lPrg .OR. lC      // Check for line-continuation
                   WHILE Right(cTemp, 1 ) == cContinue

                       cTemp := Left( cTemp , Len( cTemp ) - 1)
                       IF !lEof
                          lEof := (HB_FReadLine(nHandle,@cBuffer,{chr(13)+chr(10), chr(10)}) = -1)
                          cTemp += LTrim( cBuffer)
                       ENDIF

                   ENDDO
               ENDIF
               // Dependencies
               IF Upper(Left( cTemp ,8)) == "#INCLUDE"
                   cTemp := AllTrim(SubStr( cTemp, 9))
               Else
                   IF lPrg .and. Upper(Left( cTemp, 16)) == "SET PROCEDURE TO"
                       cTemp := AllTrim(SubStr( cTemp, 17))
                   ELSE
                       IF lPrg .and. Upper(Left( cTemp, 11)) == "SET PROC TO"  // Alternative
                           cTemp := AllTrim(SubStr( cTemp, 12))
                       ELSE
                           cTemp := ""
                       ENDIF
                   ENDIF
               Endif
               // Something Ok?
               IF Len(cTemp) > 0
                  cQuote := Left( cTemp, 1)
                  cQuoteA := ""
                  AEval(aQuotes,{| x |Iif(x[1] == cQuote,cQuoteA := x[2],)})     // Determine closing quote
                  IF cQuoteA == ""
                      cInclude := AllTrim(Left(cTemp, At(" ", cTemp + " ") - 1)) // Handle set procedure to, not using quotes
                  ELSE
                      cTemp := SubStr(cTemp, 2)
                      cInclude := AllTrim(Left(cTemp, At(cQuoteA, cTemp) - 1))   // Find closing quote
                  ENDIF
                  IF Len(cInclude) > 0 .and. Len(Alltrim(cExclExtent)) > 0
                      HB_FNAMESPLIT( cInclude, @cPath, @cFnam, @cExt, @cDrive )
                      IF lPrg .AND. Len(cExt) = 0
                          cInclude := cInclude + ".prg"        // Handle set procedure to, using default extension
                      ENDIF
                      IF AT(Lower(cExt), Lower(cExclExtent)) > 0
                          cInclude := ""
                      ENDIF
                  ENDIF
                  IF Len(cInclude) > 0
                      // Still Ok, add to list?
                      IF s_lWin32
                          IF AScan(aFiles, {| x | Lower( x ) == Lower( cInclude ) } ) = 0       // Case IN-sensitive!
                              AAdd(aFiles, (cInclude) )
                              // recursive scanning
                              IF lRecursive
                                  ScanInclude(FileInIncludePath(cInclude), lRecursive, cExclExtent, aFiles )
                              ENDIF
                          ENDIF
                      ELSE
                          IF AScan(aFiles, cInclude ) = 0       // Case Sensitive!
                              AAdd(aFiles, (cInclude) )
                              // recursive scanning
                              IF lRecursive
                                  ScanInclude(FileInIncludePath(cInclude), lRecursive, cExclExtent, aFiles )
                              ENDIF
                          ENDIF
                      ENDIF
                  ENDIF
               ENDIF
               IF !lEof
                   lEof := (HB_FReadLine(nHandle,@cTemp,{chr(13)+chr(10), chr(10)}) = -1)
                   cTemp := LTrim( cTemp)
               ENDIF
           ENDDO

         FClose( nHandle )
       ENDIF

   ENDIF
   // Return results, a space-separated list of filenames, unsorted
   IF Len(aFiles) > 1   // Skip generation of list if only main source (1) was added, caller knows what to do
       AEval(aFiles,{| x | cFileList := cFileList + " " + x } )
   ENDIF

RETURN cFileList
#Endif

*-----------------------------
FUNCTION CompileUpdatedFiles()
*-----------------------------

   LOCAL cComm
   LOCAL cOld
   LOCAL nPos

   LOCAL aCtocompile := {}
   LOCAL aOrder      := ListAsArray2( s_aBuildOrder[ 2 ], " " )
   LOCAL lEnd
   LOCAL cErrText    := ""
   LOCAL xItem
   LOCAL nObjPos
   LOCAL cOrder      := ""
   LOCAL cPrg        := ""
   LOCAL nFiles
   LOCAL nFile       := 1
   LOCAL lNewer      := .F.
#IfDef HBM_USE_DEPENDS
   LOCAL nPos1       := 0
   LOCAL cDepSrc     := ""
#Endif
   LOCAL aGauge      := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B", '≤' )

   @ 4,5 SAY "Compiling: "

   FOR EACH cOrder in aOrder
      IF ! s_lExtended

         IF cOrder == "$(CFILES)"
            nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".prg.c:" } )

            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ENDIF

            FOR nFiles := 1 TO Len( s_aPrgs )
               xItem   := Substr( s_aPrgs[ nFiles ], Rat( IIF( s_lGcc, '/', '\' ), s_aPrgs[ nFiles ] ) + 1 )
               nPos    := AScan( s_aCFiles, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( xItem, At( ".", xItem ) ) } )
               nObjPos := AScan( s_aObjs, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( xItem, At( ".", xItem ) ) } )

#IfDef HBM_USE_DEPENDS
               lNewer := .F.
               nPos1 := AScan( s_aDepends, { | x |lower(x[1]) == lower( s_aObjs[ npos ] )})
               IF nPos1 > 0
                  FOR EACH cDepSrc in s_aDepends[ nPos1 , 2 ]
                      lNewer := lNewer .OR. Fileisnewer( cDepSrc, s_aObjs[ npos ], .T. )
                  NEXT
               ENDIF
#Endif

               IF lNewer .or. FileIsNewer( s_aPrgs[ nFiles ], s_aObjs[ nObjPos ] )

                  IF nPos > 0
                     AAdd( aCtocompile, s_aCFiles[ nPos ] )
                     IF s_lMSVcc //.OR. s_lPocc
                        cComm := Strtran( cComm, "-Fo$*", "-Fo" + s_aCFiles[ nPos ] )
                     ELSE
                        cComm := Strtran( cComm, "o$*", "o" + s_aCFiles[ nPos ] )
                     ENDIF
                     cComm := Strtran( cComm, "$**", s_aPrgs[ nFiles ] )
                     cComm += IIF( s_lLinux , " > "+ (s_cLog)," >>"+ (s_cLog))

                     //Outstd( cComm )
                     //Outstd( CRLF )
                     setpos(9,0)
                     __RUN( (cComm) )
                     cErrText := Memoread( (s_cLog) )
                     lEnd     := 'C2006' $ cErrText .OR. 'No code generated' $ cErrText

                     IF ! s_lIgnoreErrors .AND. lEnd
                        __run( s_cEditor +" "+ (s_cLog) )
                        set cursor on
                        QUIT
                     ELSE
                        // FErase( s_cLog )
                     ENDIF

                     cComm := cOld

                  ENDIF

               ENDIF

            NEXT

         ENDIF

         IF cOrder == "$(OBJFILES)"

            IF s_lGcc
               nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".c.o:" .OR. x[ 1 ] == ".cpp.o:" } )
            ELSE
               nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".c.obj:" .OR. x[ 1 ] == ".cpp.obj:" } )
            ENDIF

            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ENDIF

            IF Len( aCtoCompile ) >= 1

               FOR nFiles := 1 TO Len( s_aCFiles )
                  nPos := AScan( s_aCFiles, { | x | Left( x, At( ".", x ) ) == Left( aCtoCompile[ nfiles ], At( ".", aCtoCompile[ nfiles ] ) ) } )

                  IF nPos == 0
                     AAdd( aCtoCompile, s_aCFiles[ nFiles ] )
                  ENDIF

               NEXT

            ENDIF

            FOR nFiles := 1 TO Len( aCtocompile )
               xItem := Substr( aCtocompile[ nFiles ], Rat( IIF( s_lGcc, '/', '\' ), aCtocompile[ nFiles ] ) + 1 )
               nPos  := AScan( s_aObjs, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( aCtocompile[ nFiles ], At( ".", xItem ) ) } )

               IF nPos > 0
                  IF s_lMSVcc //.OR. s_lPocc
                     cComm := Strtran( cComm, "-Fo$*", "-Fo" + s_aObjs[ nPos ] )
                  ELSE
                     cComm := Strtran( cComm, "o$*", "o" + s_aObjs[ nPos ] )
                  ENDIF
                  cComm := Strtran( cComm, "$**", aCtocompile[ nFiles ] )
                  cComm += IIF( s_lLinux ,  " > "+ (s_cLog)," >>"+ (s_cLog))
                  Outstd( " " )

                  Outstd( cComm )
                  Outstd( CRLF )
                  setpos(9,0)
                  __RUN( (cComm) )
                  cComm := cOld
               ENDIF

            NEXT

         ENDIF

      ELSE /**************Extended mode ******/             ////

         IF cOrder == "$(CFILES)"
            nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".c.obj:" .OR. x[ 1 ] == ".cpp.obj:" .or. x[ 1 ] == ".cpp.o:" .or. x[ 1 ] == ".c.o:"  } )

            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ELSE
               nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".C.OBJ:" } )

               IF nPos > 0
                  cComm := s_aCommands[ nPos, 2 ]
                  cOld  := cComm
               ENDIF

            ENDIF

            GaugeDisplay( aGauge )

            FOR nFiles := 1 TO Len( s_aCFiles )
               @  4, 16 SAY Space( 50 )
               xItem := Substr( s_aCFiles[ nFiles ], Rat( IIF( s_lGcc, '/', '\' ), s_aCFiles[ nFiles ] ) + 1 )
               nPos  := AScan( s_aObjsC, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( xitem, At( ".", xitem ) ) } )

#IfDef HBM_USE_DEPENDS
               lNewer := .F.
               nPos1 := AScan( s_aDepends, { | x |lower(x[1]) == lower( s_aObjs[ npos ] )})
               IF nPos1 > 0
                  FOR EACH cDepSrc in s_aDepends[ nPos1 , 2 ]
                      lNewer := lNewer .OR. Fileisnewer( cDepSrc, s_aObjs[ npos ], .T. )
                  NEXT
               ENDIF
#Endif
               IF lNewer .or. Fileisnewer( s_aCFiles[ nFiles ], s_aObjsC[ nPos ] )

                  IF nPos > 0
                     IF s_lMSVcc //.OR. s_lPocc
                        cComm := Strtran( cComm, "-Fo$*", "-Fo" + s_aObjsC[ nPos ] )
                     ELSE
                        cComm := Strtran( cComm, "o$*", "o" + s_aObjsC[ nPos ] )
                     ENDIF
                     cComm := Strtran( cComm, "$**", s_aCFiles[ nFiles ] )
                     cComm += IIF( s_lLinux ,  " > "+ (s_cLog)," >>"+ (s_cLog))
                     @4,16 SAY s_aCFiles[ nFiles ]
                     GaugeUpdate( aGauge, nFile / Len( s_aCFiles ) )  // changed s_aPrgs to s_aCFiles Ath 2004-06-08
                     nFile ++
                     //Outstd( cComm )
                     //Outstd( CRLF )
                     setpos(9,0)
                     __RUN( (cComm) )
                     cErrText := Memoread( (s_cLog) )
                     lEnd     := 'Error E' $ cErrText

                     IF ! s_lIgnoreErrors .AND. lEnd
                        __run( s_cEditor +" "+ (s_cLog) )
                        set cursor on
                        QUIT
                     ELSE
                        // FErase( s_cLog )
                     ENDIF

                     cComm := cOld

                  ENDIF

               ENDIF

            NEXT
            //nFile++
         ENDIF

         GaugeDisplay( aGauge )

         IF cOrder == "$(OBJFILES)"

            IF s_lGcc
               nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".prg.o:" } )
            ELSE
               nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".prg.obj:" } )
            ENDIF
            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ELSE

               IF s_lGcc
                  nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".PRG.O:" } )
               ELSE
                  nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".PRG.OBJ:" } )
               ENDIF

            ENDIF

            nFile := 1

            FOR EACH cPrg IN s_aPrgs
               @  4, 16 SAY Space( 50 )
               xItem := Substr( cPrg, Rat( IIF( s_lGcc, '/', '\' ), cPrg ) + 1 )
               nPos  := AScan( s_aObjs, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( xItem, At( ".", xitem ) ) } )

#IfDef HBM_USE_DEPENDS
               lNewer := .F.
               nPos1 := AScan( s_aDepends, { | x |lower(x[1]) == lower( s_aObjs[ npos ] )})
               IF nPos1 > 0
                  FOR EACH cDepSrc in s_aDepends[ nPos1 , 2 ]
                      lNewer := lNewer .OR. Fileisnewer( cDepSrc, s_aObjs[ npos ], .T. )
                  NEXT
               ENDIF
#Endif

               IF !empty( cPrg ) .AND. (lNewer .OR. Fileisnewer( cPrg, s_aObjs[ npos ] ))

                  IF nPos > 0
                     IF s_lMSVcc //.OR. s_lPocc
                        cComm := Strtran( cComm, "-Fo$*", "-Fo" + s_aObjs[ nPos ] )
                     ELSE
                        cComm := Strtran( cComm, "o$*", "o" + s_aObjs[ nPos ] )
                     ENDIF
                     cComm := Strtran( cComm, "$**", cPrg )
                     cComm += IIF( s_lLinux ,  " > "+ (s_cLog)," >>"+ (s_cLog))
                     @4,16 SAY cPrg
                     GaugeUpdate( aGauge, nFile / Len( s_aPrgs ) )
                     nFile ++     // moved from outside 'FOR EACH', Ath 2004-06-08

                     setpos(9,0)
                     __RUN( (cComm) )
                     cErrText := Memoread( (s_cLog) )
                     lEnd     := 'C2006' $ cErrText .OR. 'No code generated' $ cErrText .or. "Error E" $ cErrText .or. "Error F" $ cErrText

                     IF ! s_lIgnoreErrors .AND. lEnd
                        __run( s_cEditor +" "+ (s_cLog) )
                        set cursor on
                        QUIT
                     ELSE
                        // FErase( s_cLog )
                     ENDIF

                     cComm := cOld

                  ENDIF

               ENDIF

            NEXT

            // nFile ++    // removed, useless, Ath 2004-06-08

         ENDIF

      ENDIF

      IF cOrder == "$(RESDEPEN)"
         nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".rc.res:" } )

         IF nPos > 0
            cComm := s_aCommands[ nPos, 2 ]
            cOld  := cComm
         ENDIF

         FOR nFiles := 1 TO Len( s_aResources )

            IF ! Empty( s_aResources[ nFiles ] )
               cComm := Strtran( cComm, "$<", s_aResources[ nFiles ] )
               outstd( " " )
               setpos(9,0)
               __RUN( (cComm) )
            ENDIF

            cComm := cOld

         NEXT

      ENDIF

   NEXT

RETURN NIL

*------------------------------------------
FUNCTION FileIsNewer( cFile, as, lInclude )
*------------------------------------------

   LOCAL nCount    := 0
   LOCAL cSrcPath  := ""

   DEFAULT lInclude TO .F.

   // Check all paths in INCLUDE environment variable, if requested
   IF lInclude
       cFile := FileInIncludePath(cFile)
   ENDIF

   IF ! s_lExtended

      FOR nCount := 1 TO Len( s_aPrgs )
         s_aDir := { cFile,, Hbmake_Filedate( cFile ), hbmake_filetime( cFile ), ;
                   as[ nCount ], Hbmake_Filedate( as[ nCount ] ), hbmake_filetime( as[ nCount ] ) }

         IF Empty( s_aDir[ 7 ] )
            s_aDir[ 2 ] := .T.
         ELSE
            s_aDir[ 2 ] := td2jul( s_aDir[ 4 ], s_aDir[ 3 ] ) > td2jul( s_aDir[ 7 ], s_aDir[ 6 ] )
         ENDIF

      NEXT

   ELSE
      s_aDir := { cFile,, Hbmake_Filedate( cFile ), hbmake_filetime( cFile ), ;
                as, Hbmake_Filedate( as ), hbmake_filetime( as ) }

      IF Empty( s_aDir[ 7 ] )
         s_aDir[ 2 ] := .T.
      ELSE
         s_aDir[ 2 ] := td2jul( s_aDir[ 4 ], s_aDir[ 3 ] ) > td2jul( s_aDir[ 7 ], s_aDir[ 6 ] )
      ENDIF

   ENDIF

RETURN s_aDir[ 2 ]

*--------------------------------
FUNCTION FileInIncludePath(cFile)
*--------------------------------

 LOCAL cFilePath := ""
 LOCAL cSrcPath  := ""

    IF Len(s_aSrcPaths) = 0
        s_aSrcPaths := ListAsArray2( GetEnv( "INCLUDE" ) , HB_OSPATHLISTSEPARATOR() )
    ENDIF
    IF ! File(cFile)
        FOR EACH cSrcPath IN s_aSrcPaths
            IF Len(cSrcPath) > 0 .and. Right(cSrcPath,1) <> HB_OSPATHSEPARATOR()
                cSrcPath := cSrcPath + HB_OSPATHSEPARATOR()
            ENDIF
            IF File(cSrcPath + cFile)
                cFile := cSrcPath + cFile
                EXIT
            ENDIF
        NEXT
    ENDIF

RETURN cFile

*----------------------------------
FUNCTION CreateScriptLib( cFile )
*----------------------------------
* Create script file to hbmake build makefile.lnk
*----------------------------------
   LOCAL aInFiles  := {}
   LOCAL aOutFiles := {}
   LOCAL aSrc      := Directory( "*.prg" )
   LOCAL nLenaSrc  := Len( aSrc )

   LOCAL aOutC     := {}
   LOCAL aSrcC     := Directory( "*.c" )
   LOCAL cOS       := IIF( s_lLinux, "Linux", "Win32")
   LOCAL cCompiler := IIF( s_lLinux, "GCC",IIF(s_lMSVcc,"MSVC",IIF(s_lPocc,"POCC","BCC")))
   LOCAL cLibName  := Padr( Left( cFile, At( '.', cFile ) - 1 ) ,40)

   LOCAL lAutomemvar     := .F.
   LOCAL lVarIsMemvar    := .F.
   LOCAL lDebug          := .F.
   LOCAL lSupressline    := .F.
   LOCAL cHarbourFlags   := ""
   LOCAL cObjDir         := s_cObjDir + space( 20 )
   LOCAL lCompMod        := .F.
   LOCAL lInstallLib     := .F.
   LOCAL x
   LOCAL nPos
//   LOCAL lGenppo         := .F.
   LOCAL GetList         := {}
   LOCAL cItem           := ""
   LOCAL cExt            := ""
   LOCAL cDrive          := ""
   LOCAL cPath           := ""
   LOCAL cTest           := ""
   LOCAL cLast           := ''
   LOCAL nWriteFiles     := 0
   LOCAL aUserDefs
   LOCAL cCurrentDef     := ""
   LOCAL cBuild          := " "
   LOCAL lCancelMake     := .F.
   LOCAL nOption
   LOCAL lNew            := .F.
   LOCAL oMake
   LOCAL cAllRes         := ""
   LOCAL cTemp
   LOCAl nSFhandle

   IF nLenaSrc == 0 .AND. !s_lRecursive
      IF s_nLang == 1 // Portuguese-BR
         s_cAlertMsg := "N∆o h† prg na pasta "+curdir()
      ELSEIF s_nLang == 3 // Spanish
         s_cAlertMsg := "No hay ning£n prg en la carpeta "+curdir()
      ELSE
         s_cAlertMsg := "Have not any prg in "+curdir()+" folder."
      ENDIF
      Alert( s_cAlertMsg )
      RETURN NIL
   ENDIF

   s_cUserInclude  := space(200)
   s_cUserDefine   := space(200)


   IF File( cFile )

      CLS

      IF s_nLang == 1 // Portuguese-BR
         nOption := Alert( "O makefile <" + cFile +"> j† existe.",{ "Editar", "Criar Novo" , "Cancelar" } )
      ELSEIF s_nLang == 3 // Spanish
         nOption := Alert( "Lo makefile <" + cFile +"> ya existe.",{ "Editar", "Crear Nuevo" , "Cancelar" } )
      ELSE // English
         nOption := Alert( "The makefile <" + cFile +"> already exist ",{ "Edit" , "Create New" , "Cancel" } )
      ENDIF


      IF nOption = 1 // edit makefile


         // Verify if "cFile" can be openned to write mode.

         nSFhandle := FOpen( cFile, FO_WRITE )

         if nSFhandle = F_ERROR

            CLS

            IF s_nLang = 1      // brazilian portuguese
               s_cAlertMsg := "<"+cFile + "> n∆o pode ser aberto para ediá∆o."
            ELSEIF s_nLang = 3  // spanish
               s_cAlertMsg := "<"+cFile + "> no pode ser abierto para edici¢n."
            ELSE                // english
               s_cAlertMsg := "<"+cFile + "> cannot be openned for edition."
            ENDIF

            Alert( s_cAlertMsg+" FERROR ("+LTrim(Str(FError()))+")" )

            RETURN NIL
         else
            FClose( nSFhandle )
         endif

         oMake :=THbMake():new()
         oMake:cMakefile := cFile
         oMake:cMacro := iif(s_lMSVcc,"#MSVC",iif(s_lPocc,"#POCC",iif(s_lGcc,"#GCC","#BCC")))
         oMake:ReadMakefile(cFile)

         FRename(cFile,cFile+".old")

         IF LEN(oMake:aRes) >0
            FOR EACH cTemp IN oMake:aRes
                cAllRes += cTemp+ " "
            NEXT
         ENDIF

         lAutoMemVar     := oMake:lAutomemvar
         lVarIsMemVar    := oMake:lVarIsMemvar
         lDebug          := oMake:ldebug
         lSupressline    := oMake:lSupressline
         lCompMod        := oMake:lCompMod
         s_lGenppo       := oMake:lGenppo
         lInstallLib     := oMake:lInstallLib
         s_cUserInclude  := PadR(oMake:cUserInclude,200 )
         s_cUserDefine   := PadR(oMake:cUserDef,200 )
         s_cUserLibs     := PadR(oMake:cUserLib,200 )
         s_cEditor       := trim(oMake:cEditor)

         if Empty( s_cEditor )
            s_cEditor := HBMake_Editor()
         endif

         if !empty(oMake:cFmc)
             cLibName    := PadR(oMake:cFmc,200)
         endif


         if !s_lRecursive
            s_lRecursive := oMake:lRecurse
         endif


         IF nLenaSrc == 0 .and. !s_lRecursive

            CLS

            IF s_nLang=1 // PT-BR
               s_cAlertMsg := "N∆o h† nenhum prg na pasta "+CurDir()+". Use o modo recursivo -r"
            ELSEIF s_nLang=3 // Spanish
               s_cAlertMsg := "No hay ning£n prg en la carpeta "+CurDir()+". Use lo modo recursivo -r"
            ELSE
               s_cAlertMsg := "Does not have any prg in "+CurDir()+" folder. Use the recursive mode -r"
            ENDIF

            Alert( s_cAlertMsg )

            SetColor("W/N,N/W")
            CLS
            set cursor on
            QUIT

         ENDIF

         if s_lCancelRecursive
            s_lRecursive := .F.
         endif

         // after oMake read, recreate other clean makefile to edit.
         nSFhandle := FCreate(cFile)

         if nSFhandle = F_ERROR

            CLS

            IF s_nLang = 1      // brazilian portuguese
               s_cAlertMsg := "<"+cFile + "> n∆o pode ser aberto para ediá∆o."
            ELSEIF s_nLang = 3  // spanish
               s_cAlertMsg := "<"+cFile + "> no pode ser abierto para edici¢n."
            ELSE                // english
               s_cAlertMsg := "<"+cFile + "> cannot be openned for edition."
            ENDIF

            Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )

            RETURN NIL

         endif

         WriteMakeFileHeader(nSFhandle)

         s_lEditMake := .T.


      ELSEIF nOption = 2 // create a new makefile


         IF nLenaSrc == 0 .and. !s_lRecursive

            CLS

            IF s_nLang=1 // PT-BR
               s_cAlertMsg := "N∆o h† nenhum prg na pasta "+CurDir()+". Use o modo recursivo -r"
            ELSEIF s_nLang=3 // Spanish
               s_cAlertMsg := "No hay ning£n prg en la carpeta "+CurDir()+". Use lo modo recursivo -r"
            ELSE
               s_cAlertMsg := "Does not have any prg in "+CurDir()+" folder. Use the recursive mode -r"
            ENDIF

            Alert( s_cAlertMsg )
            SetColor("W/N,N/W")
            CLS
            set cursor on
            QUIT

         ENDIF

         s_lEditMake := .F.

         nSFhandle := FCreate( cFile )

         if nSFhandle = F_ERROR

            CLS

            IF s_nLang = 1      // brazilian portuguese
               s_cAlertMsg := "<"+cFile + "> n∆o pode ser criado."
            ELSEIF s_nLang = 3  // spanish
               s_cAlertMsg := "<"+cFile + "> no pode ser criado."
            ELSE                // english
               s_cAlertMsg := "<"+cFile + "> cannot be created."
            ENDIF

            Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )

            RETURN NIL

         endif

         WriteMakeFileHeader(nSFhandle)
         lNew := .T.

      ELSE
         SetColor("W/N,N/W")
         CLS
         set cursor on
         QUIT
      ENDIF

   ELSE

      nSFhandle := FCreate( cFile )

      if nSFhandle = F_ERROR

         CLS

         IF s_nLang = 1      // brazilian portuguese
            s_cAlertMsg := "<"+cFile + "> n∆o pode ser criado."
         ELSEIF s_nLang = 3  // spanish
            s_cAlertMsg := "<"+cFile + "> no pode ser criado."
         ELSE                // english
            s_cAlertMsg := "<"+cFile + "> cannot be created."
         ENDIF

         Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )

         RETURN NIL

      ENDIF

      WriteMakeFileHeader(nSFhandle)
      nOption := 2  // create a new makefile
      lNew := .T.

   ENDIF

   CLS
   Setcolor( 'w/b+,b+/w,w+/b,w/b+,w/b,w+/b' )
   @  0,  0, Maxrow(), Maxcol() BOX( Chr( 201 ) + Chr( 205 ) + Chr( 187 ) + Chr( 186 ) + Chr( 188 ) + Chr( 205 ) + Chr( 200 ) + Chr( 186 ) + Space( 1 ) )

   Attention( HbMake_Id() + space(10)+s_aLangMessages[ 27 ], 0 )

   SET CURSOR OFF

   @ 01,01 SAY s_aLangMessages[ 28 ]

   @ 01,17,06,24 GET cOS;
                 LISTBOX { "Win32", "OS/2", "Linux" };
                 MESSAGE s_aLangMessages[ 49 ];
                 STATE OsSpec(getlist,1,@cOS);
                 DROPDOWN

   @ 01,30 SAY s_aLangMessages[ 29 ]

   @ 01,54,06,59 GET cCompiler;
                 LISTBOX { "BCC", "MSVC", "POCC", "GCC","MINGW" };
                 MESSAGE s_aLangMessages[ 50 ];
                 STATE OsSpec(getlist,2,@cCompiler);
                 DROPDOWN

   @ 02,47       SAY s_aLangMessages[ 66 ]

#ifdef __PLATFORM__Windows
   @ 02,68,06,77 GET s_cEditor;
                 LISTBOX { "edit", "notepad" };
                 MESSAGE s_aLangMessages[ 67 ];
                 STATE   OsSpec(getlist,3,@s_cEditor);
                 DROPDOWN
#endif

   @ 03,01 SAY s_aLangMessages[ 59 ];
           GET cLibName;
           PICT "@S15";
           MESSAGE s_aLangMessages[ 58 ]

   @ 03,35 SAY s_aLangMessages[ 60 ];
           GET cObjDir;
           PICT "@S15"

   Attention( "xHarbour Options", 5 )

   @ 06,01 GET lAutoMemvar;
           CHECKBOX;
           CAPTION s_aLangMessages[ 32 ];
           STYLE "[X ]"

   @ 06,40 GET lVarIsMemvar;
           CHECKBOX;
           CAPTION s_aLangMessages[ 33 ];
           STYLE "[X ]"

   @ 07,01 GET lDebug;
           CHECKBOX;
           CAPTION s_aLangMessages[ 34 ];
           STYLE "[X ]"

   @ 07,40 GET lSupressLine;
           CHECKBOX;
           CAPTION s_aLangMessages[ 35 ];
           STYLE "[X ]"

   @ 08,01 GET s_lGenppo;
           CHECKBOX;
           CAPTION s_aLangMessages[ 36 ];
           STYLE "[X ]"

   @ 08,40 GET lCompMod;
           CHECKBOX;
           CAPTION s_aLangMessages[ 37 ];
           STYLE "[X ]"

   @ 09,01 SAY s_aLangMessages[ 38 ];
           GET s_cUserDefine;
           PICT "@S23"

   @ 09,40 SAY s_aLangMessages[ 39 ];
           GET s_cUserInclude;
           PICT "@S18"

   @ 10,01 GET lInstallLib;
           CHECKBOX;
           CAPTION s_aLangMessages[ 61 ];
           STYLE "[X ]"

   READ MSG AT MaxRow() - 1, 1, MaxCol() - 1

// SET CURSOR ON

   IF ! Empty( s_cUserDefine )
      aUserDefs := ListasArray2(Alltrim( s_cUserDefine ), ";")

      FOR EACH cCurrentDef in aUserDefs
         cHarbourFlags += " -D" + Alltrim( cCurrentDef ) + " "
      NEXT
   ENDIF

   IF ! Empty( s_cUserInclude )
      cHarbourFlags += " -I" + Alltrim( s_cUserInclude ) + " "
   ENDIF

   s_lBcc   :=  "BCC"   IN cCompiler
   s_lMSVcc :=  "MSVC"  IN cCompiler
   s_lGcc   :=  "GCC"   IN cCompiler
   s_lPocc  :=  "POCC"  IN cCompiler
   s_lMinGW :=  "MINGW" IN cCompiler

   if s_lMinGW
      s_lGcc := .T.
      s_lBcc :=  s_lMSVcc :=  s_lPocc :=  .F.
   endif

   cObjDir := Alltrim( cObjDir )

   IF ! Empty( cObjDir )

      IF !IsDirectory( cObjDir )
         MakeDir( cObjDir )
      ENDIF

   ENDIF

   s_aMacros := GetSourceDirMacros( s_lGcc, cOS )

   IF s_lGcc
      cObjDir := Alltrim( cObjDir )

      IF ! Empty( cObjDir )
         cObjDir += '/'
      ENDIF

      cTest := cObjDir + '/'
   ELSE
      cObjDir := Alltrim( cObjDir )

      IF ! Empty( cObjDir )
         cObjDir += '\'
      ENDIF

      cTest := cObjDir + '\'
   ENDIF

   AEval( s_aMacros, { | x, y | cItem := Substr( x[ 2 ], 1, Len( x[ 2 ] ) ), IIF( At( citem, cTest ) > 0, ( s_aMacros[ y, 1 ] := 'OBJ', s_aMacros[ y, 2 ] := cObjDir ), ) } )

   IF lAutoMemvar
      cHarbourFlags += " -a "
   ENDIF

   IF lVarIsMemvar
      cHarbourFlags += " -v "
   ENDIF

   IF lDebug
      cHarbourFlags += " -b "
   ENDIF

   IF lSupressline
      cHarbourFlags += " -l "
   ENDIF

   IF s_lGenppo
      cHarbourFlags += " -p "
   ENDIF

   IF lCompmod
      cHarbourFlags += " -m "
   ENDIF

   IF s_lBcc
      AAdd( s_aCommands, { ".cpp.obj:", "$(CC_DIR)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $**" } )
      AAdd( s_aCommands, { ".c.obj:", "$(CC_DIR)\BIN\bcc32 -I$(HB_DIR)\include $(CFLAG1) $(CFLAG2) -o$* $**" } )

      IF s_lExtended
         AAdd( s_aCommands, { ".prg.obj:", "$(HB_DIR)\bin\harbour -n -go" + if(s_lGenCsource,"3","") + " -I$(HB_DIR)\include $(HARBOURFLAGS) -I$(FWH)\include -o$* $**" } )
      ELSE
         AAdd( s_aCommands, { ".prg.c:", "$(HB_DIR)\bin\harbour -n -I$(HB_DIR)\include $(HARBOURFLAGS) -I$(FWH)\include -o$* $**" } )
      ENDIF

      AAdd( s_aCommands, { ".rc.res:", "$(CC_DIR)\BIN\brcc32 $(RFLAGS) $<" } )

   ELSEIF s_lGcc

      IF  "linux" IN Lower( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOS == "Linux"
         AAdd( s_aCommands, { ".cpp.o:", "gcc $(CFLAG1) $(CFLAG2) -o$* $**" } )
         AAdd( s_aCommands, { ".c.o:", "gcc -I/usr/include/xharbour $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )

         IF s_lExtended
            AAdd( s_aCommands, { ".prg.o:", "harbour -n $(HARBOURFLAGS) -I/usr/include/xharbour  -I/usr/local/include/xharbour  -I. -go" + if(s_lGenCsource,"3","") + "  -o$* $**" } )
         ELSE
            AAdd( s_aCommands, { ".prg.c:", "harbour -n $(HARBOURFLAGS) -I/usr/include/xharbour  -I/usr/local/include/xharbour -I.  -o$* $**" } )
         ENDIF

      ELSE
         AAdd( s_aCommands, { ".cpp.o:", "$(CC_DIR)\bin\gcc $(CFLAG1) $(CFLAG2) -o$* $**" } )
         AAdd( s_aCommands, { ".c.o:", "$(CC_DIR)\bin\gcc -I$(HB_DIR)/include $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )

         IF s_lExtended
            AAdd( s_aCommands, { ".prg.o:", "$(HB_DIR)\bin\harbour -n -go" + if(s_lGenCsource,"3","") + " -I$(HB_DIR)/include $(HARBOURFLAGS)  -o$* $**" } )
         ELSE
            AAdd( s_aCommands, { ".prg.c:", "$(HB_DIR)\bin\harbour -n -I$(HB_DIR)/include $(HARBOURFLAGS)  -o$* $**" } )
         ENDIF

      ENDIF

   ELSEIF s_lMSVcc
      AAdd( s_aCommands, { ".cpp.obj:", "$(CC_DIR)\bin\cl $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
      AAdd( s_aCommands, { ".c.obj:", "$(CC_DIR)\bin\cl -I$(HB_DIR)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )

      IF s_lExtended
         AAdd( s_aCommands, { ".prg.obj:", "$(HB_DIR)\bin\harbour -go" + if(s_lGenCsource,"3","") + " -n -I$(HB_DIR)\include $(HARBOURFLAGS) -o$* $**" } )
      ELSE
         AAdd( s_aCommands, { ".prg.c:", "$(HB_DIR)\bin\harbour -n -I$(HB_DIR)\include $(HARBOURFLAGS) -o$* $**" } )
      ENDIF

      AAdd( s_aCommands, { ".rc.res:", "$(CC_DIR)\BIN\rc $(RFLAGS) $<" } )

   ELSEIF s_lPocc
      AAdd( s_aCommands, { ".cpp.obj:", "$(CC_DIR)\BIN\pocc $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
      AAdd( s_aCommands, { ".c.obj:", "$(CC_DIR)\BIN\pocc -I$(HB_DIR)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )

      IF s_lExtended
         AAdd( s_aCommands, { ".prg.obj:", "$(HB_DIR)\bin\harbour -n -go" + if(s_lGenCsource,"3","") + " -I$(HB_DIR)\include $(HARBOURFLAGS) -I$(FWH)\include -o$** $**" } )
      ELSE
         AAdd( s_aCommands, { ".prg.c:", "$(HB_DIR)\bin\harbour -n -I$(HB_DIR)\include $(HARBOURFLAGS) -I$(FWH)\include -o$** $**" } )
      ENDIF

      AAdd( s_aCommands, { ".rc.res:", "$(CC_DIR)\BIN\porc $(RFLAGS) $<" } )

   ENDIF


   if s_nLang=1
      s_cAlertMsg := "Selecione os PRGs a compilar"
   elseif s_nLang=3
      s_cAlertMsg := "Seleccione los PRG a compilar"
   else
      s_cAlertMsg := "Select the PRG files to compile"
   endif

   aInFiles := GetSourceFiles( s_lRecursive, s_lGcc, cOS )

   nLenaSrc := Len( aInFiles )

   aOutFiles := aClone( aInFiles )


   IF nOption !=2 // not create a makefile
      pickarry( 10, 15, 19, 64, aInFiles, aOutFiles, ArrayAJoin( { oMake:aPrgs, oMake:aCs } ), .T., s_cAlertMsg )
   ELSE
      pickarry( 10, 15, 19, 64, aInFiles, aOutFiles, {}, .T., s_cAlertMsg )
   ENDIF


   AEval( aOutFiles, { | , y | aOutFiles[ y ] := Trim( Substr( aOutFiles[ y ], 1, At( ' ', aOutFiles[ y ] ) ) ) } )
   AEval( aOutFiles, { | xItem | IIF( At( '.c', xItem ) > 0 .OR. At( '.C', xItem ) > 0 .OR. At( '.cpp', xItem ) > 0 .OR. At( '.CPP', xItem ) > 0, AAdd( aOutc, xitem ), ) } )
   AEval( aOutc, { | x, z | citem := x, z := AScan( aOutFiles, { | t | t = citem } ), IIF( z > 0, aSize( aDel( aOutFiles, z ), Len( aOutFiles ) - 1 ), ) } )

   aOutFiles  := aSort( aOutFiles )
   s_aPrgs := aClone( aOutFiles )

   s_aObjs := aClone( aOutFiles )
   AEval( s_aObjs, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, s_aObjs[ x ] := cObjDir + cTest + "." + Exte( cExt, 2 ), s_aObjs[ x ] := cObjDir + cTest + "." + Exte( cExt, 3 ) ) } )
   s_aCFiles := aClone( aOutc )

   IF ! s_lExtended
      AEval( aOutc, { | xItem | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, AAdd( s_aObjs, cObjDir + cTest + "." + Exten( cExt, 2 ) ), AAdd( s_aObjs, cObjDir + cTest + "." + Exten( cExt, 1 ) ) ) } )
      AEval( aOutFiles, { | xItem | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cExt := Substr( cExt, 2 ), AAdd( s_aCFiles, cObjDir + cTest + "." + Exte( cExt, 1 ) ) } )
   ELSE
      s_aObjsC := aClone( aOutc )
      AEval( aOutc, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, s_aObjsC[ x ] := cObjDir + cTest + "." + Exten( cExt, 2 ), s_aObjsC[ x ] := cObjDir + cTest + "." + Exten( cExt, 1 ) ) } )
   ENDIF

   FOR x := 1 TO Len( s_aMacros )

      IF ! Empty( s_aMacros[ x, 2 ] )
         cItem := s_aMacros[ x, 2 ]
         nPos  := AScan( s_aPrgs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0
            AEval( s_aPrgs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aPrgs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )

            IF ! s_aMacros[ x, 3 ]
               FWrite( nSFhandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
               s_aMacros[ x, 3 ] := .T.
            ENDIF

         ENDIF

         nPos := AScan( s_aCFiles, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0
            AEval( s_aCFiles, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aCFiles[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )

            IF ! s_aMacros[ x, 3 ]
               FWrite( nSFhandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
               s_aMacros[ x, 3 ] := .T.
            ENDIF

         ENDIF

         nPos := AScan( s_aObjs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0

            IF ! Empty( cObjDir )
               AEval( s_aObjs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aObjs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )
               FWrite( nSFhandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
            ENDIF

         ENDIF

         IF s_lExtended
            nPos := AScan( s_aObjsC, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

            IF nPos > 0

               IF ! Empty( cObjDir )
                  AEval( s_aObjsC, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aObjsC[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )
               ENDIF

            ENDIF

         ENDIF

      ENDIF

   NEXT

   IF s_lGcc

      IF  "linux" IN Lower( Getenv( "HB_ARCHITECTURE" ) ) .OR. cOS == "Linux"
         FWrite( nSFhandle, "PROJECT = " + IIF( lInstallLib, "$(HB_DIR)/lib/", "" ) + Alltrim( cLibName ) + ".a " + CRLF )
      ELSE
         FWrite( nSFhandle, "PROJECT = " + IIF( lInstallLib, "$(HB_DIR)\lib\", "" ) + Alltrim( Lower( cLibName ) ) + ".a " + CRLF )
      ENDIF

   ELSE
         FWrite( nSFhandle, "PROJECT = " + IIF( lInstallLib, "$(HB_DIR)\lib\", "" ) + Alltrim( Lower( cLibName ) ) + ".lib " + "$(PR)" + CRLF )

   ENDIF

   IF ! s_lExtended
      nWriteFiles := 0
      FWrite( nSFhandle, "OBJFILES =" )

      IF Len( s_aObjs ) < 1
         FWrite( nSFhandle, + " $(OB) " + CRLF )
      ELSE
         nWriteFiles := 0
         AEval( s_aObjs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aObjs ), FWrite( nSFhandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), FWrite( nSFhandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )
      ENDIF

      nWriteFiles := 0
      FWrite( nSFhandle, "CFILES =" )

      IF Len( s_aCFiles ) < 1
         FWrite( nSFhandle, + " $(CF)" + CRLF )
      ELSE
         AEval( s_aCFiles, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aCFiles ), FWrite( nSFhandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), FWrite( nSFhandle, " " + Alltrim( x ) + " $(CF) " + CRLF ) ) } )
      ENDIF

      FWrite( nSFhandle, "PRGFILE =" )
      nWriteFiles := 0

      IF Len( s_aPrgs ) < 1
         FWrite( nSFhandle, + " $(PS)" + CRLF )
      ELSE
         AEval( s_aPrgs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aPrgs ), FWrite( nSFhandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), FWrite( nSFhandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )
      ENDIF

   ELSE /****extended moded ****/
      FWrite( nSFhandle, "OBJFILES =" )
      nWriteFiles := 0

      IF Len( s_aObjs ) < 1
         FWrite( nSFhandle, + " $(OB) " + CRLF )
      ELSE
         AEval( s_aObjs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aObjs ), FWrite( nSFhandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), FWrite( nSFhandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )
      ENDIF

      FWrite( nSFhandle, "PRGFILES =" )
      nWriteFiles := 0

      IF Len( s_aPrgs ) < 1
         FWrite( nSFhandle, + " $(PS)" + CRLF )
      ELSE
         AEval( s_aPrgs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aPrgs ), FWrite( nSFhandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), FWrite( nSFhandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )
      ENDIF

      nWriteFiles := 0

      IF Len( s_aObjsC ) > 0
         FWrite( nSFhandle, "OBJCFILES =" )

         IF Len( s_aObjsC ) < 1
            FWrite( nSFhandle, + " $(OBC) " + CRLF )
         ELSE
            AEval( s_aObjsC, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aObjsC ), FWrite( nSFhandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), FWrite( nSFhandle, " " + Alltrim( x ) + " $(OBC) " + CRLF ) ) } )
         ENDIF

      ENDIF

      nWriteFiles := 0

      IF Len( s_aCFiles ) > 0
         FWrite( nSFhandle, "CFILES =" )

         IF Len( s_aCFiles ) < 1
            FWrite( nSFhandle, + " $(CF)" + CRLF )
         ELSE
            AEval( s_aCFiles, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aCFiles ), FWrite( nSFhandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), FWrite( nSFhandle, " " + Alltrim( x ) + " $(CF) " + CRLF ) ) } )
         ENDIF

      ENDIF

   ENDIF

   FWrite( nSFhandle, "RESFILES =" + CRLF )
   FWrite( nSFhandle, "RESDEPEN = $(RESFILES)" + CRLF )
   FWrite( nSFhandle, "DEFFILE = " + CRLF )
   FWrite( nSFhandle, "HARBOURFLAGS = " + cHarbourFlags + CRLF )
   FWrite( nSFhandle, "INSTALLLIB = " + IIF( lInstallLib, "YES","NO" ) + CRLF )
   FWrite( nSFhandle, "USERDEFINE = " + alltrim(s_cUserDefine) + CRLF )
   FWrite( nSFhandle, "USERINCLUDE = " + alltrim(s_cUserInclude) + CRLF )
   FWrite( nSFhandle, "EDITOR = " + s_cEditor + CRLF )

   IF s_lBcc

      FWrite( nSFhandle, "CFLAG1 =  -OS $(SHELL)  $(CFLAGS) -d -L$(HB_DIR)\lib;$(FWH)\lib -c" + CRLF )
      FWrite( nSFhandle, "CFLAG2 =  -I$(HB_DIR)\include -I$(CC_DIR)\include -I" + Alltrim( s_cUserInclude ) + CRLF )
      FWrite( nSFhandle, "RFLAGS = " + CRLF )
      FWrite( nSFhandle, "LFLAGS = /P32 /0" + CRLF )
      FWrite( nSFhandle, "IFLAGS = " + CRLF )
      FWrite( nSFhandle, "LINKER = tlib $(LFLAGS) $(PROJECT)" + CRLF )
      FWrite( nSFhandle, " " + CRLF )
      FWrite( nSFhandle, "ALLOBJ =  $(OBJFILES) $(OBJCFILES)" + CRLF )
      FWrite( nSFhandle, "ALLRES = $(RESDEPEN)" + CRLF )
      FWrite( nSFhandle, "ALLLIB = " + CRLF )
      FWrite( nSFhandle, ".autodepend" + CRLF )

   ELSEIF s_lMSVcc

      FWrite( nSFhandle, "CFLAG1 =  -I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(SHELL) $(CFLAGS)" + CRLF )
      FWrite( nSFhandle, "CFLAG2 =  -c -I" + Alltrim( s_cUserInclude ) + CRLF )
      FWrite( nSFhandle, "RFLAGS = " + CRLF )
      FWrite( nSFhandle, "LFLAGS = " + CRLF )
      FWrite( nSFhandle, "IFLAGS = " + CRLF )
      FWrite( nSFhandle, "LINKER = lib " + CRLF )
      FWrite( nSFhandle, " " + CRLF )
      FWrite( nSFhandle, "ALLOBJ = $(OBJFILES) $(OBJCFILES) " + CRLF )
      FWrite( nSFhandle, "ALLRES = $(RESDEPEN)" + CRLF )
      FWrite( nSFhandle, "ALLLIB = " + CRLF )

   ELSEIF s_lPocc

      FWrite( nSFhandle, "CFLAG1 = " + CRLF )
      FWrite( nSFhandle, "CFLAG2 = " + CRLF )
      FWrite( nSFhandle, "RFLAGS = " + CRLF )
      FWrite( nSFhandle, "LFLAGS = " + CRLF )
      FWrite( nSFhandle, "IFLAGS = " + CRLF )
//      FWrite( nSFhandle, "LINKER = polib $(PROJECT)" + CRLF )
      FWrite( nSFhandle, "LINKER = polib " + CRLF )
      FWrite( nSFhandle, " " + CRLF )
      FWrite( nSFhandle, "ALLOBJ = $(OBJFILES) $(OBJCFILES) " + CRLF )
      FWrite( nSFhandle, "ALLRES = $(RESDEPEN)" + CRLF )
      FWrite( nSFhandle, "ALLLIB = " + CRLF )

   ELSEIF s_lGcc

      FWrite( nSFhandle, "CFLAG1 = " + IIF( s_lLinux , "-I/usr/include/xharbour  -I/usr/local/include/xharbour ", " -I$(HB_DIR)/include " ) + " $(SHELL)  -c -Wall" + CRLF )
      #ifdef __ARCH64BIT__
      FWrite( nSFhandle, "CFLAG2 = " + IIF( s_lLinux , "-L /usr/lib64/xharbour", " -L $(HB_DIR)/lib" ) + CRLF )
      #else
      FWrite( nSFhandle, "CFLAG2 = " + IIF( s_lLinux , "-L /usr/lib/xharbour", " -L $(HB_DIR)/lib" ) + CRLF )
      #endif
      FWrite( nSFhandle, "RFLAGS = " + CRLF )
      FWrite( nSFhandle, "LFLAGS = " + CRLF )
      FWrite( nSFhandle, "IFLAGS = " + CRLF )

      IF "linux" IN Lower( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOS == "Linux" .OR. s_lLinux
         FWrite( nSFhandle, "LINKER = ar -M " + CRLF )
      ELSE
         FWrite( nSFhandle, "LINKER = ar -M " + CRLF )
      ENDIF

      FWrite( nSFhandle, " " + CRLF )
      FWrite( nSFhandle, "ALLOBJ = $(OBJFILES) $(OBJCFILES) " + CRLF )
      FWrite( nSFhandle, "ALLRES = $(RESDEPEN) " + CRLF )
      FWrite( nSFhandle, "ALLLIB = $(LIBFILES) " + CRLF )
      FWrite( nSFhandle, ".autodepend" + CRLF )

   ENDIF

   FWrite( nSFhandle, " " + CRLF )
   FWrite( nSFhandle, "#COMMANDS" + CRLF )
   AEval( s_aCommands, { | xItem | FWrite( nSFhandle, xitem[ 1 ] + CRLF ), FWrite( nSFhandle, xitem[ 2 ] + CRLF ), FWrite( nSFhandle, " " + CRLF ) } )

   IF s_lBcc

      FWrite( nSFhandle, "#BUILD" + CRLF )
      FWrite( nSFhandle, " " + CRLF )
      FWrite( nSFhandle, "$(PROJECT): $(CFILES) $(OBJFILES)" + CRLF )
      FWrite( nSFhandle, "    $(CC_DIR)\BIN\$(LINKER) @&&!" + CRLF )
      FWrite( nSFhandle, "    $(ALLOBJ)" + CRLF )
      FWrite( nSFhandle, "!" + CRLF )

   ELSEIF s_lMSVcc .OR. s_lPocc

      FWrite( nSFhandle, "#BUILD" + CRLF )
      FWrite( nSFhandle, " " + CRLF )
      FWrite( nSFhandle, "$(PROJECT): $(CFILES) $(OBJFILES)" + CRLF )
      FWrite( nSFhandle, "    $(CC_DIR)\BIN\$(LINKER) @&&!" + CRLF )
      FWrite( nSFhandle, "    $(PROJECT)" + CRLF )
      FWrite( nSFhandle, "    $(ALLOBJ)" + CRLF )
      FWrite( nSFhandle, "!" + CRLF )

   ELSEIF s_lGcc

      FWrite( nSFhandle, "#BUILD" + CRLF )
      FWrite( nSFhandle, " " + CRLF )
      FWrite( nSFhandle, "$(PROJECT): $(CFILES) $(OBJFILES) " + CRLF )

      IF  "linux" IN Lower( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOS == "Linux"
         FWrite( nSFhandle, "    $(LINKER) @&&!" + CRLF )
      ELSE
         FWrite( nSFhandle, "    $(CC_DIR)\bin\$(LINKER) @&&!" + CRLF )
      ENDIF

      FWrite( nSFhandle, "    $(PROJECT) " + CRLF )
      FWrite( nSFhandle, "    $(ALLOBJ)  " + CRLF )
      FWrite( nSFhandle, "!" + CRLF )

   ENDIF

   FClose( nSFhandle  )

   IF !lCancelMake

      IF s_nLang == 1 .OR. s_nLang == 3
        s_cAlertMsg := "Compilar lib ? (S/N) "
      ELSE // English
        s_cAlertMsg := "Build lib ? (Y/N) "
      ENDIF

      @ 20,5 Say s_cAlertMsg Get cBuild PICT "!" Valid cBuild $ iif(s_nLang=2,"YN","SN")
      READ

      IF cBuild == "S" .OR. cBuild == "Y"
         ResetInternalVars()
         SetColor("W/N,N/W")
         Clear
         SetPos(9,0)
         // Main( cFile, " -f "+iif(s_nLang=1,"-lPT",iif(s_nLang=3,"-lES","-lEN")) )
         _AppMain( cFile, " -f "+iif(s_nLang=1,"-lPT",iif(s_nLang=3,"-lES","-lEN")) )
      ELSE
         set cursor on
         SetColor("W/N,N/W")
         Clear
      ENDIF

   ELSE
      set cursor on
      SetColor("W/N,N/W")
      Clear
   ENDIF

RETURN NIL

*-----------------------
FUNCTION CreateMakelib()
*-----------------------
* create makefile.lnk to build your lib file from script file.
*----------------------
   LOCAL cRead as String
   LOCAL nPos as Numeric
   LOCAL aMacro as Array
   LOCAL aTemp as Array
   LOCAL nCount as Numeric
   LOCAL aCurobjs as Array
   LOCAL nObjPos as Numeric
   LOCAL cLib
   LOCAL xInfo
   LOCAL nMKhandle  // makefile.lnk handle

   cRead := Alltrim( readln( @s_lEof ) )
   nMKhandle := FCreate( s_cMakeFileName )


   IF nMKhandle = F_ERROR

      IF s_nLang = 1      // brazilian portuguese
         s_cAlertMsg := "<"+s_cMakeFileName + "> n∆o pode ser criado."
      ELSEIF s_nLang = 3  // spanish
         s_cAlertMsg := "<"+s_cMakeFileName + "> no pode ser criado."
      ELSE                // english
         s_cAlertMsg := "<"+s_cMakeFileName + "> cannot be created."
      ENDIF

      Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )
      RETURN NIL

   ENDIF

   s_szProject := cRead
   aMacro      := ListAsArray2( cRead, ":" )

   IF Len( aMacro ) > 1
      aTemp := ListAsArray2( aMacro[ 2 ], " " )
      AEval( aTemp, { | xItem | AAdd( s_aBuildOrder, xItem ) } )
   ENDIF

   AAdd( s_aBuildOrder, aMacro[ 1 ] )
   cRead  := Strtran( cRead, "@&&!", "" )
   aMacro := ListAsArray2( cRead, '\' )
   AEval( aMacro, { | xMacro | Findmacro( xMacro, @cRead ) } )

   IF s_lBcc .OR. s_lMSVcc .OR. s_lPocc
      s_cLinkCommands := cRead + " @" + s_cMakeFileName
   ELSE
      s_cLinkCommands := cRead + " < " + s_cMakeFileName
   ENDIF


   FOR nPos := 1 TO 7

      cRead  := Alltrim( readln( @s_lEof ) )
      aMacro := ListAsArray2( cRead, " " )

      FOR nCount := 1 TO Len( aMacro )


         IF  "$" IN aMacro[ nCount ]

            IF ( aMacro[ nCount ] = "$(PROJECT)" ) .AND. (s_lGcc .OR. s_lMSVcc .OR. s_lPocc)

               FindMacro( aMacro[ nCount ], @cRead )

               IF s_lGcc
                  FWrite( nMKhandle, "CREATE " + " lib" + cRead + CRLF )
                  cLib := "lib" + cRead
//                  FWrite( nMKhandle, cRead + CRLF )
               ELSE
                  cRead := strtran(cRead,",","")
                  cRead := strtran(cRead,"+","")
                  xInfo := iif(s_lMSVcc," -out:","/out:")
                  xInfo += cRead
                  cRead := xInfo
                  FWrite( nMKhandle, cRead + CRLF )
               ENDIF

            ELSEIF ( aMacro[ nCount ] == "$(ALLOBJ)" )

               Findmacro( aMacro[ nCount ], @cRead )
               aCurObjs := ListAsArray2( cRead, " " )

               FOR nObjPos := 1 TO Len( aCurObjs )

                  IF s_lGcc
                     FWrite( nMKhandle, "ADDMOD " + aCurObjs[ nObjPos ] + CRLF )
                  ENDIF

                  IF s_lBcc .OR. s_lMSVcc .OR. s_lPocc

                     IF nObjPos < Len( aCurObjs )
                        FWrite( nMKhandle, iif(s_lBcc,"+-","") + aCurObjs[ nObjPos ] + iif(s_lBcc," &","") + CRLF )
                     ELSE
                        FWrite( nMKhandle, iif(s_lBcc,"+-","") + aCurObjs[ nObjPos ] + CRLF )
                     ENDIF

                  ENDIF

               NEXT

            ENDIF

         ENDIF

      NEXT

   NEXT

   IF s_lGcc
      FWrite( nMKhandle, "SAVE" + CRLF )
      FWrite( nMKhandle, "END " + CRLF )
   ENDIF

   FClose( nMKhandle )

   IF s_lLinux
      s_cLinkCommands += " || rm -f " + cLib
   ENDIF

RETURN NIL

*---------------------------------
FUNCTION FindCfile( citem, aSrcc )
*---------------------------------
 LOCAL nReturnPos := 0

 nReturnPos := AScan( aSrcc, { | x | Lower( x[ 1 ] ) == cItem } )

RETURN nReturnPos

/*
#IFNDEF __HARBOUR__
FUNCTION CRLF
   RETURN Chr( 13 ) + Chr( 10 )
#ENDIF
*/

*----------------------------
FUNCTION CheckIFfile( cFile )
*----------------------------

   LOCAL cNextLine := ''
   LOCAL cCommand  := ''
   LOCAL cTemp

   cTemp := Substr( cFile, At( " ", cFile ) + 1 )

   IF File( cTemp )
      cNextLine := Trim( Substr( ReadLN( @s_lEof ), 1 ) )

      IF  "!" IN  cNextLine
         cCommand := Substr( cNextLine, At( ' ', cNextLine ) + 1 )
         RUN( cCommand )
      ENDIF

      RETURN .T.

   ENDIF

RETURN .F.

*----------------------------
FUNCTION Checkstdout( cText )
*----------------------------

   cText := Strtran( cText, "!stdout", "" )
   Outstd( cText )

RETURN NIL

*---------------------------
FUNCTION CheckIFdef( cTemp )
*---------------------------

   LOCAL nPos
   LOCAL cRead    := ""
   LOCAL aSet     := {}
   LOCAL nMakePos

   IF cTemp == "!endif"
      RETURN NIL
   ENDIF

   WHILE At( "!endif", cRead ) == 0
      cRead := Trim( Substr( ReadLN( @s_lEof ), 1 ) )

      IF  "!endif" IN cRead
         FT_FSKIP( - 1 )
         EXIT
      ENDIF

      cTemp := Strtran( cTemp, "!ifdef ", "" )

        IF  '=' IN cRead

         IF  "\.." IN cRead
            cRead := Substr( cRead, 1, At( "\..", cRead ) - 1 )
         ELSEIF  "/.." IN cRead
            cRead := Substr( cRead, 1, At( "/..", cRead ) - 1 )
         ENDIF

         aSet := ListAsArray2( cRead, "=" )
         nPos := AScan( s_aDefines, { | x | x[ 1 ] == cTemp } )

         IF nPos > 0
            cRead    := Alltrim( Strtran( aSet[ 1 ], "$(", "" ) )
            cRead    := Strtran( cRead, ")", "" )
            nMakePos := AScan( s_aMacros, { | x | x[ 1 ] == cRead } )

            IF nMakePos == 0
               AAdd( s_aMacros, { aSet[ 1 ], aSet[ 2 ] } )
            ENDIF

         ELSE /* Locate For !ELSE    */

            WHILE At( "!endif", cRead ) == 0
               cRead := Trim( Substr( ReadLN( @s_lEof ), 1 ) )

               IF  "!ELSE" IN cRead

                  WHILE At( "!endif", cRead ) == 0
                     cRead := Trim( Substr( ReadLN( @s_lEof ), 1 ) )

                     IF  "!endif" IN cRead
                        FT_FSKIP( - 1 )
                        EXIT
                     ENDIF

                     aSet := ListAsArray2( cRead, "=" )
                     AAdd( s_aMacros, { aSet[ 1 ], aSet[ 2 ] } )
                   ENDDO

               ENDIF

            ENDDO

         ENDIF

      ELSEIF '!stdout' IN cRead
         Checkstdout( cRead )
      ENDIF

   ENDDO

RETURN NIL

*-------------------------
FUNCTION BuildBccCfgFile()
*-------------------------
LOCAL cCfg := s_cHarbourDir + '\bin\'+s_cHarbourCFG
LOCAL nCfg

   IF !File( cCfg ) .or. s_lForce

      nCfg := FCreate( cCfg )

      if nCfg = F_ERROR
         IF s_nLang = 1      // brazilian portuguese
            s_cAlertMsg := cCfg + " n∆o pode ser criado."
         ELSEIF s_nLang = 3  // spanish
            s_cAlertMsg := cCfg + " no pode ser criado."
         ELSE                // english
            s_cAlertMsg := cCfg + " cannot be created."
         ENDIF
         Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )
         RETURN NIL
      endif

      FWrite( nCfg, "CC=BCC32" + CRLF )
//    FWrite( nCfg, "CFLAGS= -c " + ReplaceMacros( "-I$(HB_DIR)\include -OS $(CFLAGS) -d -L$(HB_DIR)\lib" ) + CRLF )
      FWrite( nCfg, "CFLAGS= -c -D__EXPORT__ " + ReplaceMacros( "-I$(HB_DIR)\include $(CFLAGS) -d -L$(HB_DIR)\lib" ) + CRLF )
      FWrite( nCfg, "VERBOSE=YES" + CRLF )
      FWrite( nCfg, "DELTMP=YES" + CRLF )
      FClose( nCfg )
   ENDIF

RETURN NIL

*-------------------------
FUNCTION BuildMscCfgFile()
*-------------------------
   LOCAL cCfg := s_cHarbourDir + '\bin\'+s_cHarbourCFG
   LOCAL nCfg

   IF !File( cCfg )  .or. s_lForce

      nCfg := FCreate( cCfg )

      if nCfg = F_ERROR
         IF s_nLang = 1      // brazilian portuguese
            s_cAlertMsg := cCfg + " n∆o pode ser criado."
         ELSEIF s_nLang = 3  // spanish
            s_cAlertMsg := cCfg + " no pode ser criado."
         ELSE                // english
            s_cAlertMsg := cCfg + " cannot be created."
         ENDIF
         Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )
         RETURN NIL
      endif

      FWrite( nCfg, "CC=cl" + CRLF )
      FWrite( nCfg, "CFLAGS= -c -D__EXPORT__ " + ReplaceMacros( "-I$(HB_DIR)\include -TP -W3 -nologo $(C_USR) $(CFLAGS)" ) + CRLF )
      FWrite( nCfg, "VERBOSE=YES" + CRLF )
      FWrite( nCfg, "DELTMP=YES" + CRLF )
      FClose( nCfg )
   ENDIF

RETURN NIL


*-------------------------
FUNCTION BuildPccCfgFile()
*-------------------------
   LOCAL cCfg := s_cHarbourDir + '\bin\'+s_cHarbourCFG
   LOCAL nCfg

   IF !File( cCfg )  .or. s_lForce

      nCfg := FCreate( cCfg )

      if nCfg = F_ERROR
         IF s_nLang = 1      // brazilian portuguese
            s_cAlertMsg := cCfg + " n∆o pode ser criado."
         ELSEIF s_nLang = 3  // spanish
            s_cAlertMsg := cCfg + " no pode ser criado."
         ELSE                // english
            s_cAlertMsg := cCfg + " cannot be created."
         ENDIF
         Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )
         RETURN NIL
      endif

      FWrite( nCfg, "CC=POCC" + CRLF )
      FWrite( nCfg, "CFLAGS= /Ze /Go /Ot /Tx86-coff /D__EXPORT__ " + ReplaceMacros( "-I$(HB_DIR)\include $(C_USR) $(CFLAGS)" )  + CRLF )
      FWrite( nCfg, "VERBOSE=YES" + CRLF )
      FWrite( nCfg, "DELTMP=YES" + CRLF )
      FClose( nCfg )

   ENDIF

RETURN NIL



*-------------------------
FUNCTION BuildGccCfgFile()
*-------------------------
   LOCAL cCfg
   LOCAL nCfg
   LOCAL cDir := s_cHarbourDir
//   LOCAL cBhc := Alltrim( Strtran( ReplaceMacros( '$(HB_DIR)' ), '\', '/' ) )

//   cDir := Strtran( cDir, '/', '\' )

   cCfg := s_cHarbourDir + '\bin\'+s_cHarbourCFG

   IF !File( cCfg ) .or. s_lForce

      nCfg := FCreate( cCfg )

      if nCfg = F_ERROR
         IF s_nLang = 1      // brazilian portuguese
            s_cAlertMsg := cCfg + " n∆o pode ser criado."
         ELSEIF s_nLang = 3  // spanish
            s_cAlertMsg := cCfg + " no pode ser criado."
         ELSE                // english
            s_cAlertMsg := cCfg + " cannot be created."
         ENDIF
         Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )
         RETURN NIL
      endif

      FWrite( nCfg, "CC=gcc" + CRLF )
      FWrite( nCfg, "CFLAGS= -c -D__EXPORT__ " + ReplaceMacros( "-I" + s_cHarbourDir + "/include $(C_USR)  -L" + s_cHarbourDir + "/lib" )  + CRLF )
      FWrite( nCfg, "VERBOSE=YES" + CRLF )
      FWrite( nCfg, "DELTMP=YES" + CRLF )
      FClose( nCfg )
   ENDIF

RETURN NIL

*--------------------------
FUNCTION BuildGccCfgFileL()
*--------------------------
   LOCAL cCfg := '/etc/'+s_cHarbourCFG
   LOCAL nCfg

   IF !File( cCfg )  .or. s_lForce

      nCfg := FCreate( cCfg )

      if nCfg = F_ERROR
         IF s_nLang = 1      // brazilian portuguese
            s_cAlertMsg := cCfg + " n∆o pode ser criado."
         ELSEIF s_nLang = 3  // spanish
            s_cAlertMsg := cCfg + " no pode ser criado."
         ELSE                // english
            s_cAlertMsg := cCfg + " cannot be created."
         ENDIF
         Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )
         RETURN NIL
      endif

      FWrite( nCfg, "CC=gcc" + CRLF )
      FWrite( nCfg, "CFLAGS= -c -I/usr/include/xharbour" + CRLF )
      FWrite( nCfg, "VERBOSE=YES" + CRLF )
      FWrite( nCfg, "DELTMP=YES" + CRLF )
      FClose( nCfg )
   ENDIF

RETURN NIL

*------------------------------
FUNCTION FindHarbourCfg( cCfg )
*------------------------------

   LOCAL cPath AS STRING := ''
   LOCAL lFound AS LOGICAL := .F.
   LOCAL cEnv AS STRING
   LOCAL aEnv as Array of String
   LOCAL lLinux :=  s_lLinux
   LOCAL nPos

   IF ! lLinux .OR. s_lOs2
      cEnv := Gete( "PATH" ) + ";" + Curdir()
      aEnv := ListAsArray2( cEnv, ";" )

      FOR nPos := 1 TO Len( aEnv )

         IF File( aEnv[ nPos ] + '\'+s_cHarbourCFG )
            cPath  := aEnv[ nPos ]
            lFound := .T.
            EXIT
         ENDIF

      NEXT

   ELSE

      IF File( '/etc/'+s_cHarbourCFG )
         lFound := .T.
         cPath  := '/etc/'+s_cHarbourCFG
      ENDIF

      IF !lFound

         IF File( '/usr/local/etc/'+s_cHarbourCFG )
            lFound := .T.
            cPath  := '/usr/local/etc/'+s_cHarbourCFG
         ENDIF

      ENDIF

   ENDIF

   cCfg := cPath

RETURN lFound

*---------------------------
FUNCTION TestforPrg( cFile )
*---------------------------

   LOCAL aFiles AS ARRAY  := {}
   LOCAL cPath AS STRING  := ''
   LOCAL cTest AS STRING  := ""
   LOCAL cDrive AS STRING := ""
   LOCAL cExt AS STRING   := ""
   LOCAL cItem AS STRING  := ""
   LOCAL aDir AS ARRAY
   LOCAL nPos AS NUMERIC
   LOCAL nFiles AS NUMERIC

   hb_FNAMESPLIT( cFile, @cPath, @cTest, @cExt, @cDrive )

   cExt := Substr( cExt, 2 )
   aDir := Directory( cTest + '.*' )

   FOR nPos := 1 TO 7
      cItem := cTest + "." + Extenprg( cExt, nPos )
      AAdd( aFiles, cItem )
   NEXT

   FOR nFiles := 1 TO Len( aFiles )
      nPos := AScan( aDir, { | a | a[ 1 ] == aFiles[ nFiles ] } )

      IF nPos > 0
         AAdd( s_aPrgs, aFiles[ nFiles ] )
      ENDIF

   NEXT

RETURN NIL

*-------------------
FUNCTION GetGccDir()
*-------------------

   LOCAL cPath AS STRING := ''
   LOCAL cEnv AS STRING
   LOCAL aEnv AS Array of string
   LOCAL nPos as Numeric

   IF s_lLinux
      cPath := "."
   ELSE
      cEnv := Gete( "PATH" )
      aEnv := ListAsArray2( cEnv, ";" )

      FOR nPos := 1 TO Len( aEnv )

         IF File( aEnv[ nPos ] + '\gcc.exe' ) .OR. File( Upper( aEnv[ nPos ] ) + '\GCC.EXE' )
            cPath := aEnv[ nPos ]
            cPath := Left( cPath, Rat( '\', cPath ) - 1 )
            EXIT
         ENDIF

      NEXT

   ENDIF

RETURN cPath

*-------------------------------------------------------------
FUNCTION ConvertParams( cFile, aFile, p1, p2, p3, p4, p5, p6 )
*-------------------------------------------------------------

   LOCAL cParam := ""

   IF ! Empty( cFile )

      IF Left( cFile, 1 ) $ "- /?" .OR. "credits" IN cFile
         cParam += cFile+" "
      ELSE
         cFile := cFile
         AAdd( aFile, cFile )
      ENDIF

   ENDIF

   IF ! Empty( p1 )

      IF Left( p1, 1 ) $ "- /?" .OR. "credits" IN p1
         cParam += p1+" "
      ELSE
         cFile := p1
         AAdd( aFile, cFile )
      ENDIF

   ENDIF

   IF ! Empty( p2 )

      IF Left( p2, 1 ) $ "- /"
         cParam += p2+" "
      ELSE
         cFile := p2
         AAdd( aFile, cFile )
       ENDIF

   ENDIF

   IF ! Empty( p3 )

      IF Left( p3, 1 ) $ "- /"
         cParam += p3+" "
      ELSE
         cFile := p3
         AAdd( aFile, cFile )
      ENDIF

   ENDIF

   IF ! Empty( p4 )

      IF Left( p4, 1 ) $ "- /"
         cParam += p4+" "
      ELSE
         cFile := p4
         AAdd( aFile, cFile )
      ENDIF

   ENDIF

   IF ! Empty( p5 )

      IF Left( p5, 1 ) $ "- /"
         cParam += p5+" "
      ELSE
         cFile := p5
         AAdd( aFile, cFile )
      ENDIF

   ENDIF

   IF ! Empty( p6 )

      IF Left( p6, 1 ) $ "- /"
         cParam += p6+" "
      ELSE
         cFile := p6
         AAdd( aFile, cFile )
      ENDIF

   ENDIF

   cParam := Strtran( cParam, "/", "-" )
   cParam := Strtran( cParam, "-elx", "-ELX" )
   cParam := Strtran( cParam, "-el", "-ELX" )
   cParam := Strtran( cParam, "-ex", "-EX" )
   cParam := Strtran( cParam, "-e", "-EX" )
   cParam := Strtran( cParam, "-i", "-I" )
   cParam := Strtran( cParam, "-p", "-P" )
   cParam := Strtran( cParam, "-b", "-B" )
   cParam := Strtran( cParam, "-gl", "-GL" )
   cParam := Strtran( cParam, "-g", "-G" )
   cParam := Strtran( cParam, "-v", "-V" )
   cParam := Strtran( cParam, "-m", "-M" )
   cParam := Strtran( cParam, "-pc", "-PC" )
   cParam := Strtran( cParam, "-f", "-F" )
   cParam := Strtran( cParam, "-r", "-R" )
   cParam := Strtran( cParam, "-nr", "-NR" ) // cancel recursive search
   cParam := Strtran( cParam, "-l", "-L" )
   cParam := Strtran( cParam, "-c", "-C" )  // compile after create.

   IF "-EX" IN cParam .OR. "-ELX" IN cParam
      IF "-ELX" IN cParam
         s_lLibrary := .T.
      ENDIF
   ENDIF

   IF "-L" IN cParam
      s_cDefLang := Substr( cParam, At( "-L", cParam ) + 2, 2 )
   ENDIF


RETURN cParam


*------------------------------------
FUNCTION ProcessParameters( cParams )
*------------------------------------

   LOCAL aDef

   // Force to recompile all sources
   IF "-F" IN cParams
      s_lForce := .T.
      cParams  := StrTran( cParams, "-F", "" )
   ENDIF


   // Recursive source search
   IF "-R" IN cParams
      s_lRecursive := .T.
      cParams      := StrTran( cParams, "-R", "" )
   ENDIF

   // cancel -R
   IF "-NR" IN cParams
      s_lRecursive := .F.
      s_lCancelRecursive := .T.
      cParams := StrTran( cParams, "-NR", "" )
   ENDIF

   // Use BCC as default C/C++
   IF "-B" IN cParams
      s_lBcc   := .T.
      s_lGcc   := .F.
      s_lMSVcc := .F.
      s_lPocc  := .F.
      s_lMinGW := .F.
      cParams  := Strtran( cParams, "-B", "" )
   ENDIF

   // Use GCC as default C/C++ at Linux
   IF "-GL" IN cParams
      s_lBcc   := .F.
      s_lGcc   := .T.
      s_lMSVcc := .F.
      s_lPocc  := .F.
      s_lLinux := .T.
      s_lMinGW := .F.
      cParams  := Strtran( cParams, "-GL", "" )
   ENDIF

   // Use GCC as default C/C++ at Windows
   IF "-G" IN cParams
      s_lBcc   := .F.
      s_lGcc   := .T.
      s_lMSVcc := .F.
      s_lPocc  := .F.
      s_lMinGW := .F.
      cParams  := Strtran( cParams, "-G", "" )
   ENDIF

   // Use MS-VC as default C/C++
   IF "-V" IN cParams
      s_lBcc   := .F.
      s_lGcc   := .F.
      s_lMSVcc := .T.
      s_lPocc  := .F.
      cParams  := Strtran( cParams, "-V", "" )
   ENDIF

   // Use Pelles C as default C/C++
   IF "-PC" IN cParams
      s_lBcc   := .F.
      s_lGcc   := .F.
      s_lMSVcc := .F.
      s_lPocc  := .T.
      s_lMinGW := .F.
      cParams  := Strtran( cParams, "-PC", "" )
   ENDIF

   // compile only the module
   IF "-M" IN cParams
      s_lBcc   := .F.
      s_lGcc   := .T.
      s_lMSVcc := .F.
      s_lPocc  := .F.
      s_lMinGW := .T.
      cParams  := Strtran( cParams, "-M", "" )
   ENDIF

   // Ignore warnings
   IF "-I" IN cParams
      s_lIgnoreErrors := .T.
      cParams         := Strtran( cParams, "-I", "" )
   ENDIF

   // Print all commands and dependencies
   IF "-P" IN cParams
      s_lPrint := .T.
      cParams  := Strtran( cParams, "-P", "" )
   ENDIF

   // Define a macro.
   IF "-D" IN cParams
      cParams := "-D" + Strtran( cParams, "-D", ";" )
      cParams := Strtran( cParams, "-D;", "-D" )
      aDef    := ListAsArray2( Alltrim( Substr( cParams, 3 ) ), ";" )
      AEval( aDef, { | xDef | IIF( At( '=', xDef ) > 0, GetParaDefines( xDef ), ) } )
   ENDIF

   // Build a Library
   IF "-EL" IN cParams  .OR.  "-ELX" IN cParams

      IF At( "-ELX", cParams ) > 0
         cParams := Strtran( cParams, "-ELX", "" )
      ELSE
         cParams := Strtran( cParams, "-EL", "" )
      ENDIF

      s_lEdit     := .T.
      s_lExtended := .T.
      s_lLibrary  := .T.
      s_cMakeFileName := "makelib.lnk"

   ENDIF

   // Build an application
   IF "-E" IN cParams .OR. "-EX" IN cParams

      IF "-EX" IN cParams
         cParams := Strtran( cParams, "-EX", "" )
      ELSE
         cParams := Strtran( cParams, "-E", "" )
      ENDIF

      s_lEdit     := .T.
      s_lExtended := .T.

   ENDIF

RETURN NIL

*-------------------------------------
FUNCTION WriteMakeFileHeader(nSFhandle)
*-------------------------------------

IF s_lMSVcc
   FWrite( nSFhandle, "#MSVC" + CRLF )
ELSEIF s_lPocc
   FWrite( nSFhandle, "#POCC" + CRLF )
ELSEIF s_lGcc
   FWrite( nSFhandle, "#GCC" + CRLF )
ELSE
   FWrite( nSFhandle, "#BCC" + CRLF )
   FWrite( nSFhandle, "VERSION=BCB.01" + CRLF )
ENDIF


   FWrite( nSFhandle, "!ifndef CC_DIR" + CRLF )
   FWrite( nSFhandle, "CC_DIR = $(MAKE_DIR)" + CRLF )
   FWrite( nSFhandle, "!endif" + CRLF )
   FWrite( nSFhandle, CRLF )
   FWrite( nSFhandle, "!ifndef HB_DIR" + CRLF )
   FWrite( nSFhandle, "HB_DIR = $(HARBOUR_DIR)" + CRLF )
   FWrite( nSFhandle, "!endif" + CRLF )
   FWrite( nSFhandle, " " + CRLF )
// FWrite( nSFhandle, "RECURSE=" + IIF( s_lRecursive, " YES ", " NO " ) + CRLF )
// FWrite( nSFhandle, " " + CRLF )

RETURN NIL

*-------------------------------
FUNCTION BuildLangArray( cLang )
*-------------------------------
LOCAL aLang := Array( 67 )

   DEFAULT cLang TO "EN"

   AFill( aLang, "" )

   aLang[1] := HbMake_Id()  // Hbmake identification.

   IF cLang == "EN"   // English

      aLang[2]  := "Syntax:  hbmake <cFile>.bc [options] - Example: hbmake hello.bc /ex"
      aLang[3]  := "Options:  /e[x]   Create a new Makefile. If /ex is used it create a"
      aLang[4]  := "                  new make file in extended mode."
      aLang[5]  := "          /el[x]  Create a new Makefile. If /elx is used it create a"
      aLang[6]  := "                  new make file to build a LIB in extended mode."
      aLang[7]  := "          /D      Define a macro."
      aLang[8]  := "          /p      Print all commands and depedencies."
      aLang[9]  := "          /b      Use Borland C/C++ as C compiler"
      aLang[10] := "          /g+     Use GNU C/C++ as C compiler"
      aLang[11] := "          /b+     Use Borland C/C++ as C compiler"
      aLang[12] := "          /g      Use GNU C/C++ as C compiler"
      aLang[13] := "          /gl     Use GNU C/C++ as C compiler in Linux"
      aLang[14] := "          /gl+    Use GNU C/C++ as C compiler in Linux"
      aLang[15] := "          /v      Use MS-Visual C/C++ as C compiler"
      aLang[16] := "          /f      Force recompiltion of all files"
      aLang[17] := "          /i      Ignore errors returned by command"
      aLang[18] := "          /r /nr  Activate recursive mode. /nr Deactivate recursive mode."
      aLang[19] := "                  Note: /p and /D can be used together"
      aLang[20] := "                        /r and /e[x]/el[x] can be used together"
      aLang[21] := "                  Options with + are the default values"
      aLang[22] := "                  -D switch can accept multiple macros on the same line"
      aLang[23] := "                  or use one macro per -D switch"
      aLang[24] := "                  /l[LANGID] Specify the language to be used on hbmake"
      aLang[25] := "                  LANGID= (EN/PT/ES). On Windows, the default will be the S.O."
      aLang[26] := "                  language. On OS/2, FreeBSD and LINUX will be English."
      aLang[27] :=  "Enviroment options"
      aLang[28] := "Select the OS"
      aLang[29] := "Select the C Compiler"
      aLang[30] := "GT/GUI Lib"
      aLang[31] := "xHarbour Options"
      aLang[32] := "Automatic memvar declaration /a"
      aLang[33] := "Variables are assumed M-> /v"
      aLang[34] := "Debug info /b"
      aLang[35] := "Suppress line number information /l"
      aLang[36] := "Generate pre-processed output /p"
      aLang[37] := "compile module only /m"
      aLang[38] := "User Defines "
      aLang[39] := "User include Path"
      aLang[40] := "Use External Libs"
      aLang[41] := "<Spacebar>-Select <Enter>-Continue process <F5> Sel/Unsel All"
      aLang[42] := "Warning level /w"
      aLang[43] := "Numbers of source files per line on makefile"
      aLang[44] := "Use Multi Thread Library"
      aLang[45] := "Executable file name"
      aLang[46] := "Warning Level /w"
      aLang[47] := "<Tab>-Next <Sh-Tab>-Prev <Enter>-Sel <"+chr(24)+chr(25)+">-Change Sel <Spacebar>-Open Box"
      /* Messages Start Here */
      aLang[48] := "3rd Party Rdd"
      aLang[49] := "What OS you Use"
      aLang[50] := "What C compiler  you has"
      aLang[51] := "This app use Graphical libraries"
      aLang[52] := "Do you use 3rd Party Rdd"
      aLang[53] := "Compress this app"
      aLang[54] := "Compress the app after Linked (use upx ?)"
      aLang[55] := "Your app will be linked to user harbour.dll"
      aLang[56] := "Where the .obj/.o files will be generates"
      aLang[57] := "Inform executable name (without .exe extention)"
      /* More messages for LIB build */
      aLang[58] := "Inform the lib name (without extension)"
      /* More xHarbour options for LIB build */
      aLang[59] := "Lib name:"
      aLang[60] := "Obj dir files:"
      aLang[61] := "Install the lib at the xHarbour lib folder"
      aLang[62] := "          /pc     Use Pelles C/C++ as C compiler"
      aLang[63] := "          /m      Use MinGW (GCC) as C compiler"
      aLang[64] := "          /credits"

      aLang[65] := "User Libs: "
      aLang[66] := "Errorlog Editor:"
      aLang[67] := "Inform the log error editor for your choice."

   ELSEIF cLang == "ES"   // Spanish

      aLang[2] := "Sintaxe:  hbmake <cArchivo>.bc [opciones] - Exemplo: hbmake hello.bc /ex"
      aLang[3] := "Opciones: /e[x]   Crea un Makefile nuevo. Si se usa /ex se crea un nuevo"
      aLang[4] := "                  makefile en modo extendido."
      aLang[5] := "          /el[x]  Crea un Makefile nuevo. Si se usa /elx se crea un nuevo"
      aLang[6] := "                  makefile para construir una LIB en modo extendido."
      aLang[7] := "          /D      Define una macro."
      aLang[8] := "          /p      Imprime todos los comandos y dependencias."
      aLang[9] := "          /b      Usa Borland C/C++ como compilador C"
      aLang[10] := "          /g+     Usa GNU C/C++ como compilador C"
      aLang[11] := "          /b+     Usa Borland C/C++ como compilador C"
      aLang[12] := "          /g      Usa GNU C/C++ como compilador C"
      aLang[13] := "          /gl     Usa GNU C/C++ como compilador C en Linux"
      aLang[14] := "          /gl+    Usa GNU C/C++ como compilador C en Linux"
      aLang[15] := "          /v      Usa MS-Visual C/C++ como compilador C"
      aLang[16] := "          /f      Forza la recompilaci¢n de todos los archivos"
      aLang[17] := "          /i      Ignora los errores devueltos por el comando"
      aLang[18] := "          /r /nr  Activa modo recursivo.  /nr Desactivar modo recursivo."
      aLang[19] := "                  Nota: /p y /D pueden ser usados juntos"
      aLang[20] := "                        /r y /e[x]/el[x] pueden ser usados juntos"
      aLang[21] := "                  Las opciones con + son los valores por omisi¢n"
      aLang[22] := "                  El parametro -D puede aceptar multiplas macros en la misma"
      aLang[23] := "                  linea ou use una macro por parametro -D"
      aLang[24] := "                  /l[LANGID] especifica una linguagem a ser utilizada por"
      aLang[25] := "                   hbmake. LANGID = (EN/PT/ES). En sistemas Windows, Lo padr¢n"
      aLang[26] := "                   es la linguagem de SO. En OS/2, FreeBSD y LINUX ser†n InglÇs."
      aLang[27] := "Opciones de Ambiente"
      aLang[28] := "Seleccione SO"
      aLang[29] := "Seleccione Compilador C"
      aLang[30] := "GT/GUI Lib"
      aLang[31] := "Opciones de lo xHarbour"
      aLang[32] := "Declaraci¢n automatica de memvar /a"
      aLang[33] := "Variables ser†n assumidas M-> /v "
      aLang[34] := "Info. Debug /b"
      aLang[35] := "Suprime info del n£mero de linea /l"
      aLang[36] := "Gera salida pre-processada /p"
      aLang[37] := "Compila solamente lo modulo /m"
      aLang[38] := "Define del usuarios:"
      aLang[39] := "Path includes del usuario:"
      aLang[40] := "Usar libs externas"
      aLang[41] := "<Espacio>-Seleccionar <Enter>-Continuar proceso <F5> Selec/Deselec todo."
      aLang[42] := "Nivel del aviso de lo compilador /w"
      aLang[43] := "Cuantos PRGs por linea no makefile:"
      aLang[44] := "Usar la libreria Multi Thread"
      aLang[45] := "Nombre del ejecutable"
      aLang[46] := "Nivel de Avisos /w"
      aLang[47] := "<Tab>-Avanzar <Sh-Tab>-Volver <Enter>-Selec <"+chr(24)+chr(25)+">-Mudar Selec <Espacio>-Caja"
      /* Messages Start Here */
      aLang[48] := "Rdd Terceros"
      aLang[49] := "Cual OS usted usa"
      aLang[50] := "Cual compilador C usted usa"
      aLang[51] := "Esta App usa Lib Grafica o No"
      aLang[52] := "Usted usa Rdd de terceros"
      aLang[53] := "Comprimir app"
      aLang[54] := "Prensar la App despuÇs de enlazada (usar upx) ?"
      aLang[55] := "Su aplicacion ser† ligada para usar la harbour.dll"
      aLang[56] := "Donde los ficheros *.obj ser†n generados"
      aLang[57] := "Informe lo nombre de lo executable (sin la extension .exe)"
      /* More messages for LIB build */
      aLang[58] := "Informe lo nombre de la lib (sin la extension)"
      /* More xHarbour options for LIB build */
      aLang[59] := "Nombre de la Lib:"
      aLang[60] := "Direct¢rio de los Obj:"
      aLang[61] := "Alojar la lib en el direct¢rio lib de xHarbour"
      aLang[62] := "          /pc     Usa Pelles C/C++ como compilador C"
      aLang[63] := "          /m      Usa MinGW (GCC) como compilador C"
      aLang[64] := "          /credits"
      aLang[65] := "Libs del usuario: "
      aLang[66] := "Editor de errorlog:"
      aLang[67] := "Informe lo editor de log de errores de su preferencia."

   ELSEIF cLang == "PT"  // Portuguese-Brazil

      aLang[2] := "Sintaxe:  hbmake <arquivo>.bc [opá‰es] -  Exemplo: hbmake hello.bc /ex"
      aLang[3] := "Opá‰es:  /e[x]  Cria um Makefile novo. Se for usado /ex cria um makefile"
      aLang[4] := "                em modo extendido."
      aLang[5] := "         /el[x] Cria um Makefile novo. Se for usado /elx cria um makefile"
      aLang[6] := "                para construir uma LIB, em modo extendido."
      aLang[7] := "         /D     Define uma macro."
      aLang[8] := "         /p     Imprime todos os comandos e dependàncias."
      aLang[9] := "         /b     Usa Borland C/C++ como compilador C"
      aLang[10] := "         /g+    Usa GNU C/C++ como compilador C"
      aLang[11] := "         /b+    Usa Borland C/C++ como compilador C"
      aLang[12] := "         /g     Usa GNU C/C++ como compilador C"
      aLang[13] := "         /gl    Usa GNU C/C++ como compilador C no Linux"
      aLang[14] := "         /gl+   Usa GNU C/C++ como compilador C no Linux"
      aLang[15] := "         /v     Usa MS-Visual C/C++ como compilador C"
      aLang[16] := "         /f     Foráa a recompilaá∆o de todos os arquivos."
      aLang[17] := "         /i     Ignora os erros devolvidos pelo comando."
      aLang[18] := "         /r /nr Ativa modo recursivo. /nr Desativar modo recursivo."
      aLang[19] := "                Nota:  /p e /D podem ser usados juntos"
      aLang[20] := "                       /r e /e[x]/el[x] podem ser usados juntos"
      aLang[21] := "                As opá‰es com + s∆o os valores padr∆o."
      aLang[22] := "                O parÉmetro -D pode aceitar m£ltiplas macros na mesma linha"
      aLang[23] := "                ou use uma macro por parÉmetro -D"
      aLang[24] := "                /l[LANGID] especifica a linguagem a ser utilizada pelo hbmake,"
      aLang[25] := "                LANGID = (EN/PT/ES). Em Windows, o padr∆o ser† a linguagem"
      aLang[26] := "                definida no S.O. Em OS/2, FreeBSD e LINUX o padr∆o ser† Inglàs."
      aLang[27] := "Opá‰es de Ambiente"
      aLang[28] := "Selecione o SO"
      aLang[29] := "Selecione Compilador C"
      aLang[30] := "GT/GUI lib"
      aLang[31] := "Opá‰es do xHarbour"
      aLang[32] := "Declaraá∆o Autom†tica de Memvar /a"
      aLang[33] := "Vari†veis s∆o assumidas M-> /v"
      aLang[34] := "Info. Debug /b"
      aLang[35] := "Suprime info de n£mero da linha /l"
      aLang[36] := "Gera sa°da prÇ-processada /p"
      aLang[37] := "Compila apenas o m¢dulo /m"
      aLang[38] := "User Defines:"
      aLang[39] := "User Include Path:"
      aLang[40] := "Usa Libs Externas ?"
      aLang[41] := "<Espaáo>-Seleciona <Enter> p/ continuar processo <F5>-Sel/DeSel. tudo."
      aLang[42] := "N°vel de aviso do compilador /w"
      aLang[43] := "Qtd de PRGs por linha, no makefile: "
      aLang[44] := "Usar a biblioteca Multi Thread ?"
      aLang[45] := "Nome Execut†vel:"
      aLang[46] := "N°vel de Warnings /w"
      aLang[47] := "<Tab>-Avanáa <Sh-Tab>-Volta <Enter>-Sel. <"+chr(24)+chr(25)+">-Muda Sel. <Espc>-Abre Box"
      /* Messages Start Here */
      aLang[48] := "Rdd Terceiros"
      aLang[49] := "Selecione o Sistema Operacional"
      aLang[50] := "Selecione o compilador C/C++"
      aLang[51] := "Esta aplicaá∆o vai usar Lib Grafica ? Qual ?"
      aLang[52] := "Esta aplicaá∆o vai usar Rdd de terceiros ? Qual ?"
      aLang[53] := "Comprimir App ?"
      aLang[54] := "Comprimir a aplicaá∆o ap¢s linkada (usar upx) ?"
      aLang[55] := "Sua aplicaá∆o ser† linkada para usar a harbour.dll ?"
      aLang[56] := "Informe a pasta onde os arquivos *.obj ser∆o gerados"
      aLang[57] := "Informe o nome do execut†vel (sem a extens∆o .exe)"
      /* More messages for LIB build */
      aLang[58] := "Informe o nome da lib (sem a extens∆o)"
      /* More xHarbour options for LIB build */
      aLang[59] := "Nome da Lib:"
      aLang[60] := "Diret¢rio dos Obj:"
      aLang[61] := "Instalar a lib no diret¢rio lib do xHarbour"
      aLang[62] := "         /pc    Usa Pelles C/C++ como compilador C"
      aLang[63] := "         /m     Usa MinGW (GCC) como compilador C"
      aLang[64] := "         /credits"
      aLang[65] := "Libs de usu†rio: "
      aLang[66] := "Editor de errorlog:"
      aLang[67] := "Informe o editor de log de erros de sua preferància."

   ENDIF

RETURN aLang


*------------------------------------------
FUNCTION GetSelFiles( aInFiles, aOutFiles )
*------------------------------------------

   LOCAL aRet  := {}
   LOCAL cItem
   LOCAL nPos

   FOR EACH cItem IN aInFiles

      nPos := AScan( aOutFiles, { | x | x == Left( cItem, At( ' ', citem ) - 1 ) } )

      IF nPos > 0
         AAdd( aRet, cItem )
      ENDIF

   NEXT

RETURN aRet

*---------------------------
FUNCTION ResetInternalVars()
*---------------------------

   s_lPrint        := .F.
   s_lEdit         := .F.
   s_aDefines      := {}
   s_aBuildOrder   := {}
   s_aCommands     := {}
   s_aMacros       := {}
   s_aPrgs         := {}
   s_aExtLibs      := {}
   s_aCFiles           := {}
   s_aObjs         := {}
   s_aObjsC        := {}
   s_lEof          := .F.
   s_aResources    := {}
   s_cLinkCommands := ""

if s_lBcc
   s_lGcc          := .F.
   s_lMSVcc        := .F.
   s_lPocc         := .F.
   s_lMinGW        := .F.
elseif s_lMSVcc
   s_lBcc          := .F.
   s_lGcc          := .F.
   s_lPocc         := .F.
   s_lMinGw        := .F.
elseif s_lPocc
   s_lBcc          := .F.
   s_lGcc          := .F.
   s_lMSVcc        := .F.
   s_lMinGW        := .F.
endif
   s_lForce        := .F.
   s_lLinux        := .F.
   s_szProject     := ""
   s_lLibrary      := .F.
   s_lIgnoreErrors := .F.
   s_lExtended     := .T.
   s_lOs2          := .F.
   s_lRecursive    := .F.
   s_lEditMake     := .F.
   s_aDir          := {}
   s_aLangMessages := {}

RETURN NIL

*----------------------------------
FUNCTION OsSpec(GetList,nPos,cSpec)
*----------------------------------
   local oGet := GetList[nPos]
   local oControl
   oControl := oGet:Control
   IF oControl != NIL
      cSpec := oControl:GetData( oControl:Value )
//   keyboard chr( K_TAB )
   ENDIF

RETURN .T.

*--------------------------
FUNCTION CheckCompiler(cOS)
*--------------------------
RETURN ( ("Win32" IN cOS) .or. ("Linux" In cOS) )

*------------------------------
FUNCTION SetThisLibs(aTempLibs)
*------------------------------

Local c := ""
Local n

for Each n In aTempLibs
     c += "-l"
     c += StrTran( n, '.a', "" )
     c+= " "
next

RETURN c

*----------------
FUNCTION AsDll(x)
*----------------
Local y := x
 x := !y
RETURN .T.

*--------------------
FUNCTION Delete_ppo()
*--------------------
LOCAL cFile := alltrim(s_cAppName)+".ppo"

  FErase( cFile )

RETURN NIL

*------------------
FUNCTION ShowHelp()
*------------------
/*
   CLS

   OutStd( s_aLangMessages[ 1 ] + CRLF )
   OutStd( HbMake_Copyright()  + CRLF )
   OutStd( CRLF )
   OutStd( s_aLangMessages[ 2 ] + CRLF )
   OutStd( s_aLangMessages[ 3 ] + CRLF )
   OutStd( s_aLangMessages[ 4 ] + CRLF )
   OutStd( s_aLangMessages[ 5 ] + CRLF )
   OutStd( s_aLangMessages[ 6 ] + CRLF )
   OutStd( s_aLangMessages[ 7 ] + CRLF )
   OutStd( s_aLangMessages[ 8 ] + CRLF )

   IF s_lOS2
      OutStd( s_aLangMessages[ 9 ] + CRLF )
      OutStd( s_aLangMessages[ 10 ] + CRLF )
      OutStd( s_aLangMessages[ 13 ] + CRLF )

   ELSEIF  s_lLinux
      OutStd( s_aLangMessages[ 9 ] + CRLF )
      OutStd( s_aLangMessages[ 12 ] + CRLF )
      OutStd( s_aLangMessages[ 14 ] + CRLF )

   ELSE
      OutStd( s_aLangMessages[ 11 ] + CRLF )
      OutStd( s_aLangMessages[ 12 ] + CRLF )
      OutStd( s_aLangMessages[ 13 ] + CRLF )
   ENDIF

   OutStd( s_aLangMessages[ 15 ] + CRLF )
   OutStd( s_aLangMessages[ 62 ] + CRLF )
   OutStd( s_aLangMessages[ 63 ] + CRLF )
   OutStd( s_aLangMessages[ 16 ] + CRLF )
   OutStd( s_aLangMessages[ 17 ] + CRLF )
   OutStd( s_aLangMessages[ 18 ] + CRLF )
   OutStd( s_aLangMessages[ 19 ] + CRLF )
   OutStd( s_aLangMessages[ 20 ] + CRLF )
   OutStd( s_aLangMessages[ 21 ] + CRLF )
   OutStd( s_aLangMessages[ 22 ] + CRLF )
   OutStd( s_aLangMessages[ 23 ] + CRLF )

   setpos(maxrow()-1,0)
   WAIT
   setpos(maxrow(),0)

   OutStd( s_aLangMessages[ 24 ] + CRLF )
   OutStd( s_aLangMessages[ 25 ] + CRLF )
   OutStd( s_aLangMessages[ 26 ] + CRLF )
*/
      OutStd( HbMake_ID() + HB_OSNewLine() +;
              HbMake_Copyright() + HB_OSNewLine() +;
              HB_OSNewLine() +;
              "Syntax:  hbmake <makefile> [switches]" + HB_OSNewLine() +;
              "Switches:" + HB_OSNewLine() +;
              "  -b   Use Bcc as the C compiler. This options is default under Win32" + HB_OSNewLine() +;
              "  -g   Use Gcc as the C compiler. This options is default under OS/2" + HB_OSNewLine() +;
              "  -gl  Use Gcc as the C compiler on Linux" + HB_OSNewLine() +;
              "  -v   Use MSVC as the C compiler" + HB_OSNewLine() +;
              "  -D   Define an macro. Multiple macros can be used in an single line" + HB_OSNewLine() +;
              "       Ex: -DOBJ=c.obj;d.obj" + HB_OSNewLine() +;
              "           Multiple -D is also supported" + HB_OSNewLine() +;
              "  -p   Print all command and defines" + HB_OSNewLine() +;
              "  -f   Force all files to be rebuild" + HB_OSNewLine() +;
              "  -e   Open the editor mode." + HB_OSNewLine() +;
              "  -ec  create/edit and compile (only for a unique prg)" + HB_OSNewLine() +;
              "  -ex  Open the editor mode in extended mode" + HB_OSNewLine() +;
              "  -el  Open the editor mode for creating libraries." + HB_OSNewLine() +;
              "  -elx Open the editor mode for creating libraries in extented mode." + HB_OSNewLine() +;
              "  -i   Ignore errors returned by commands" + HB_OSNewLine() +;
              "  -r   Recurse directories for source code" + HB_OSNewLine() +;
              HB_OSNewLine() +;
              "Note:  Linked with " + Version() + HB_OSNewLine() )

RETURN NIL

*-------------------
FUNCTION HbMake_ID()
*-------------------
RETURN ( "HbMake v"+s_cHbMakeVersion+" - xHarbour Make Utility")

*--------------------------
FUNCTION HbMake_Copyright()
*--------------------------
RETURN ( "Copyright (C) 2000-" +_HB_CURR_YEAR +" xHarbour project - http://www.xharbour.org")

*---------------------
FUNCTION ShowCredits()
*---------------------
local i,aCredits := {}

 CLS

 aadd( aCredits,"Credits:" )
 aadd( aCredits," " )
 aadd( aCredits,"Luiz Rafael Culik <culikr@uol.com.br>" )

 qqout( HbMake_Id() )
 qout( HbMake_copyright() )
 qout( " ")

 for i := 1 to Len( aCredits )
     qout( aCredits[i] )
 next

 qout(" ")

RETURN NIL

*------------------------------
STATIC FUNCTION HbMake_Editor()
*------------------------------
Local cEditor := ""

  IF s_lOS2 .OR. s_lLinux

     cEditor := "mcedit"

  ELSEIF s_lWin32

     cEditor := "notepad"

  ENDIF

RETURN cEditor
