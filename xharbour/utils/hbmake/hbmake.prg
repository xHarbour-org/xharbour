/*
 * $Id: hbmake.prg,v 1.136 2004/12/28 23:00:00 modalsist Exp $
 */
/*
 * xHarbour Project source code:
 * hbmake.prg xHarbour make utility main file
 *
 * Copyright 2000,2001,2002,2003,2004 Luiz Rafael Culik <culikr@uol.com.br>
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
#include "fileio.ch"
#include "common.ch"
#include "radios.ch"
//#include "wvtgui.ch"
#define EOL Hb_OsNewLine()
#define CRLF Hb_OsNewLine()
#xtranslate TimeToSec(<x>) => ( ( Val( Substr( <x>, 1, 2 ) ) * 3600 ) +( Val( Substr( <x>, 4, 2 ) ) * 60 ) + ( Val( Substr( <x>, 7, 2 ) ) ) )


#translate DateDiff(<x>,<y>) => (<x>-<y>)

#Define HBM_USE_DEPENDS    // Set this to have section #DEPENDS parsed like RMake, Ath 2004-06
                           // An extra parameter is added to FileIsNewer() to have it check the INCLUDE paths also
                           // Interactive mode asks whether sources should be scanned for dependencies (#include, set procedure to, set proc to

/*
Beginning Static Variables Table
Default Values for core variables are set here
New Core vars should only be added on this section
*/

STATIC s_cHbMakeVersion := "1.136"
STATIC s_lPrint        := .F.
STATIC s_nHandle
STATIC s_aDefines      := {}
STATIC s_aBuildOrder   := {}
STATIC s_aCommands     := {}
STATIC s_aMacros       := {}
STATIC s_aPrgs         := {}
STATIC s_aExtLibs      := {}
#IfDef HBM_USE_DEPENDS
STATIC s_aDepends      := {}
#Endif
STATIC s_aCs           := {}
STATIC s_aObjs         := {}
STATIC s_aObjsc        := {}
STATIC s_aSrcPaths     := {}
STATIC s_lEof          := .F.
STATIC s_aRes          := {}
STATIC s_nLinkHandle
STATIC s_cLinker       := "makefile.lnk"
STATIC s_cLinkComm     := ''
STATIC s_lBcc          := .T.
STATIC s_lGcc          := .F.
STATIC s_lMSVcc        := .F.
STATIC s_lForce        := .F.
STATIC s_lLinux        := .F.
STATIC s_szProject     := ""
STATIC s_lLibrary      := .F.
STATIC s_lIgnoreErrors := .F.
STATIC s_lExtended     := .T.
STATIC s_lOs2          := .F.
STATIC s_lRecurse      := .F.
STATIC s_lEditMode     := .F.
STATIC s_lCompress     := .F.
STATIC s_lExternalLib  := .F.
STATIC s_aDir
STATIC s_aLangMessages := {}
STATIC s_cAppName      := ""
STATIC s_cDefLang
STATIC s_cLog          := ""
STATIC s_nLang         := 2    //  default is english 
STATIC s_lMt           := .F.
STATIC s_cUserDef      := "                                        "
STATIC s_cUserInclude  := "                                        "
STATIC s_lxFwh         := .F.
STATIC s_nFilesToAdd   := 5
STATIC s_nWarningLevel := 0
STATIC s_AppName       := ""
STATIC s_lAsDll        := .F.
STATIC s_cMsg          := ""

*---------------------------------------------
FUNCTION MAIN( cFile, p1, p2, p3, p4, p5, p6 )
*---------------------------------------------

   LOCAL nPos
   LOCAL aFile    := {}
   LOCAL aDef     := {}
   LOCAL cOs      := Os()
   LOCAL AllParam
   LOCAL nLang    := GETUSERLANG()

   cls

   Ferase( s_cLinker )
   SET(39,159)

   AllParam := ConvertParams( @cFile, aFile, p1, p2, p3, p4, p5, p6 )

   IF !empty(AllParam)

      AllParam := upper(AllParam)

      IF "-LPT" $ AllParam
         nLang := 1
      ELSEIF "-LEN" $ AllParam
         nLang := 2
      ELSEIF "-LES" $ AllParam
         nLang := 3
      ENDIF

   ENDIF

   s_nLang := nLang
   s_cDefLang := IIF( s_nLang == 1, "PT", IIF( s_nLang == 2, "EN", "ES" ) )
   s_aLangMessages := BuildLangArray( s_cDefLang )

   IF Pcount() == 0    .or. ;
      "/?" IN AllParam .or. ;
      "-?" IN AllParam .or. ;
      "-h" IN AllParam .or. ;
      "/h" IN AllParam 
      ShowHelp()
      RETURN NIL
   ENDIF

   IF Upper( Os() ) == "WINDOWS XP"
      s_cLinker := "makefile.tmp"
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

   // Assing Default C Compiler upon The OS 

   IF "OS/2" IN  cOs
      s_lGcc   := .T.
      s_lLinux := .F.
      s_lBcc   := .F.
   ENDIF

   IF "LINUX" IN Upper( cOs )
      s_lGcc   := .T.
      s_lLinux := .T.
      s_lBcc   := .F.
   ENDIF

   IF Len( aFile ) > 1
      IF s_nLang=1
         s_cMsg :="Arquivo definido mais que uma vez."
      ELSEIF s_nLang=3
         s_cMsg:="Fichero definido m†s que una vez."
      ELSE
         s_cMsg:="File defined more than once."
      ENDIF
      alert( s_cMsg )
      RETURN NIL
   ENDIF

   IF Len( aFile ) > 0
      cFile := aFile[ 1 ]
   ELSE
      cFile := ""
   ENDIF

   IF ( Empty( cFile ) .AND. ! s_lEditMode )
      IF s_nLang=1
         s_cMsg := "Arquivo n∆o encontrado."
      ELSEIF s_nLang=3
         s_cMsg := "Fichero no encontrado."
      ELSE
         s_cMsg := "File not Found."
      ENDIF
      alert( s_cMsg )
      RETURN NIL
   ENDIF

   // We have at least one parameter . check IF is an valid file name

   IF Pcount() >= 1

      IF File( cFile )
         ProcessParameters( AllParam )
      ELSEIF  s_lEditMode
         ProcessParameters( AllParam )
      ELSE

         IF ! s_lEditMode
            IF s_nLang=1
               s_cMsg := "Arquivo n∆o encontrado."
            ELSEIF s_nLang=3
               s_cMsg := "Fichero no encontrado."
            ELSE
               s_cMsg := "File not found."
            ENDIF
            Alert( s_cMsg )
            RETURN NIL
         ENDIF

      ENDIF

   ENDIF

   s_cLog := Substr( cFile,1 , AT(".",cFile) -1) + ".out"
   s_AppName := Substr( cFile,1 , AT(".",cFile) -1)

   FErase( (s_cLog) )

   IF s_lEditMode

      IF s_lLibrary
         CreateLibMakeFile( cFile )
      ELSE
         CreateMakeFile( cFile )
      ENDIF

      RETURN NIL

   ENDIF

   CLS

   // Make file are parsed here

   IF !ParseMakeFile( cFile )

      IF s_nLang = 1      // brazilian portuguese 
         s_cMsg := cFile + " n∆o pode ser aberto. HbMake ser† fechado."
      ELSEIF s_nLang = 3  // spanish
         s_cMsg := cFile + " no pode ser abierto. HbMake ser† cerrado."
      ELSE                // english
         s_cMsg := cFile + " cannot be openned. HbMake will be closed."
      ENDIF
      Alert( s_cMsg )
      RETURN NIL

   ENDIF

   IF s_lPrint
      PrintMacros()
   ENDIF

   set cursor off

   IF s_lForce
      CompileFiles()
   ELSE
      CompileUpdatedFiles()
   ENDIF

   set cursor on
   setpos(9,0)
   Outstd( s_cLinkComm + CRLF )
   set cursor off
   __RUN( (s_cLinkComm) )

   IF s_lCompress .AND. !s_lLibrary
      set cursor on
      setpos(9,0)
      __Run( " upx -9 "+ (s_cAppName) )
      set cursor off
   ENDIF

   tracelog( s_lasdll)

   IF s_lasdll .or. lower(right(s_cAppName,3)) == 'dll'
       __Run( Replacemacros("implib $(BHC)\lib\" + left(s_cAppName,at(".",s_cAppName)-1)+".lib " +s_cAppName ))
   ENDIF

   set cursor on

RETURN NIL

*------------------------------
FUNCTION ParseMakeFile( cFile )
*------------------------------

   LOCAL nPos
   LOCAL cBuffer     := {}
   LOCAL cMacro      := "#BCC"
   LOCAL cDep        := "#DEPENDS"
   LOCAL cOpt        := "#OPTS"
   LOCAL cCom        := "#COMMANDS"
   LOCAL cBuild      := "#BUILD"
   LOCAL cTemp       := ""
   LOCAL cTemp1      := ''
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
   LOCAL lLinux      :=  'linux' IN  Lower( Os() )
   LOCAL aLib
   LOCAL aLibx
   LOCAL lDjgpp      := "GNU C" in HB_COMPILER()
   LOCAL x :=1
   LOCAL ct


   s_nHandle := FT_FUSE( cFile )

   IF s_nHandle < 0
      RETURN .F.
   ENDIF


   #IFndef __PLATFORM__Windows
      IF !FILE("hbtemp.c")
         CreateLink()
      ENDIF
   #ENDIF

   cBuffer := Trim( Substr( ReadLN( @s_lEof ), 1 ) )

   AAdd( s_aDefines, { "HMAKEDIR", GetHarbourDir() } )

   IF s_lBcc
      AAdd( s_aDefines, { "MAKEDIR", GetBccDir() } )
   ELSEIF s_lGcc
      AAdd( s_aDefines, { "MAKEDIR", GetGccDir() } )
   ELSEIF s_lMSVcc
      AAdd( s_aDefines, { "MAKEDIR", GetVccDir() } )
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
         FClose( s_nHandle )
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

      aTemp := ListAsArray2( Alltrim( cTemp ), "=" )

      IF lmacrosec

         IF Alltrim( Left( cTemp, 7 ) ) <> '!ifndef' .AND. Alltrim( Left( cTemp, 6 ) ) <> "!endif" .AND. Alltrim( Left( cTemp, 7 ) ) <> '!IFfile' .AND. Alltrim( Left( cTemp, 7 ) ) <> '!stdout' .AND. Alltrim( Left( cTemp, 6 ) ) <> '!ifdef'

            IF Len( aTemp ) > 1

                IF  "$" IN aTemp[ 2 ]

                  IF s_lGcc .AND. aTemp[ 1 ] = "CFLAG1" .OR. s_lGcc .AND. aTemp[ 1 ] = "CFLAG2"
                      AAdd( s_aMacros, { aTemp[ 1 ], Strtran( Replacemacros( aTemp[ 2 ] ), "\", "/" ) } )

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
                              s_lMSVcc    := .F.
                              s_aDefines[2] := { "MAKEDIR", GetGccDir() }
                           ENDIF

                        NEXT

                     ENDIF

                     IF aTemp[ 1 ] == "ALLOBJ" .AND. ! s_lMt

                     ENDIF

                     AAdd( s_aMacros, { aTemp[ 1 ], Replacemacros( aTemp[ 2 ] ) } )

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
                              s_lMSVcc    := .F.
                              s_aDefines[2] := { "MAKEDIR", GetGccDir() }
                              s_aMacros[2,2] :=  GetGccDir()
                           ENDIF

                        NEXT

                     ENDIF
                        AAdd( s_aMacros, { aTemp[ 1 ], aTemp[ 2 ] } )
                  ENDIF

               ENDIF

            ENDIF

            IF aTemp[ 1 ] == "COMPRESS"
               s_lCompress := "YES" IN aTemp[ 2 ]
            ENDIF

            IF aTemp[ 1 ] == "EXTERNALLIB"
               s_lExternalLib := "YES" IN aTemp[ 2 ]
            ENDIF

            IF aTemp[ 1 ] == "PROJECT"

               IF At( '.lib', aTemp[ 2 ] ) > 0 .OR. At( '.a', aTemp[ 2 ] ) > 0
                  s_lLibrary := .T.
               ENDIF

               s_cAppName := SubStr( aTemp[ 2 ], 1, AT( ' ', aTemp[ 2 ] ) -1 )

            ENDIF

            IF aTemp[ 1 ] == "OBJFILES"
               s_aObjs := ListAsArray2( replacemacros( aTemp[ 2 ] ), " " )
            ENDIF

            IF aTemp[ 1 ] == "OBJCFILES"

               aTemp1 := ListAsArray2( replacemacros( aTemp[ 2 ] ), " " )

               IF Len( aTemp1 ) == 1

                  IF ! Empty( aTemp[ 1 ] )
                      s_aObjsC := ListAsArray2( replacemacros( aTemp[ 2 ] ), " " )
                   ENDIF

               ELSE
                  s_aObjsC := ListAsArray2( replacemacros( aTemp[ 2 ] ), " " )
               ENDIF

            ENDIF

            IF aTemp[ 1 ] == "PRGFILES"
               s_aPrgs     := ListAsArray2( replacemacros( aTemp[ 2 ] ), " " )
               s_lExtended := .T.
               lCfgFound := Findharbourcfg( @cCfg )
            ENDIF

            IF aTemp[ 1 ] == "PRGFILE"
               s_aPrgs := ListAsArray2( replacemacros( aTemp[ 2 ] ), " " )
            ENDIF

            IF aTemp[ 1 ] == "EXTLIBFILES"
               s_aExtLibs  := ListAsArray2( replacemacros( aTemp[ 2 ] ), " " )
            ENDIF

            IF aTemp[ 1 ] == "CFILES"

               IF s_lExtended
                  aTempCFiles := ListAsArray2( replacemacros( aTemp[ 2 ] ), " " )

                  IF ( Len( aTempCFiles ) == 1 )

                     IF ! Empty( aTempCFiles[ 1 ] )
                        s_aCs := ListAsArray2( replacemacros( aTemp[ 2 ] ), " " )
                     ENDIF

                  ELSE
                     s_aCs := ListAsArray2( replacemacros( aTemp[ 2 ] ), " " )
                  ENDIF

               ELSE
                  s_aCs := ListAsArray2( replacemacros( aTemp[ 2 ] ), " " )
               ENDIF



            ENDIF

            IF aTemp[ 1 ] == "RESFILES"
               s_aRes := ListAsArray2( replacemacros( aTemp[ 2 ] ), " " )
            ENDIF

         ELSE

            IF '!ifndef' IN cTemp
               Checkdefine( cTemp )
            ELSEIF  '!ifdef' IN cTemp
               CheckIFdef( cTemp )
            ELSEIF '!iffile' IN cTemp
               CheckIFfile( cTemp )
            ELSEIF  '!stdout' IN cTemp
               Checkstdout( cTemp )
            ENDIF

         ENDIF

      ENDIF

      IF lbuildSec
         s_szProject   := cTemp
         s_aBuildOrder := ListAsArray2( cTemp, ":" )


         IF ! s_lLibrary
            SetBuild()
         ELSE
            SetLibBuild()
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

   FT_FUse()     // Close the opened file & release memory

   IF s_lExtended .AND. ! lCfgFound

      IF s_lBcc
         BuildBorCfgFile()
      ELSEIF s_lMSVcc
         Buildmsccfgfile()
      ELSEIF s_lGcc .AND. ! lLinux
         Buildgcccfgfile()
      ELSEIF s_lGcc .AND. lLinux
         BuildGccCfgFilel()
      ENDIF

   ENDIF

//RETURN NIL
RETURN .T.

*----------------------------
FUNCTION Checkdefine( cTemp )
*----------------------------

   LOCAL cDef
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
   LOCAL nPos
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

   LOCAL nPos
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
      cTemp := Strtran( cmacro, "$(", "" )
      cTemp := Strtran( cTemp, ")", "" )

      IF ! Empty( cTemp )
         cRead := Alltrim( Strtran( cRead, cmacro, Gete( cTemp ) ) )
      ENDIF

   ELSE
      cRead := Alltrim( Strtran( cRead, cmacro, s_aMacros[ npos, 2 ] ) )
   ENDIF

RETURN cRead

*--------------------------------
FUNCTION ReplaceMacros( cMacros )
*--------------------------------

   LOCAL nPos
   LOCAL nCount       := 0
   LOCAL aTempMacros  := {}
   LOCAL aLocalMacros := {}

   aTempMacros := ListAsArray2( cMacros, " " )
   AEval( aTempMacros, { | xMacro | IIF(  "$" IN xMacro , ;
                         IIF(  ";" IN xMacro , ( aLocalMacros := ListAsArray2( xMacro, ";" ), ;
                         AEval( aLocalMacros, { | x | Findmacro( x, @cMacros ) } ) ), ;
                         Findmacro( xMacro, @cMacros ) ), ) } )


RETURN cmacros

*------------------
FUNCTION SetBuild()
*------------------

   LOCAL cRead
   LOCAL nPos
   LOCAL aMacro
   LOCAL aTemp
   LOCAL nCount
   LOCAL cCurrentRead := ''
   LOCAL cMacro

   cRead     := Alltrim( readln( @s_lEof ) )
   s_szProject := cRead
   aMacro    := ListAsArray2( cRead, ":" )

   IF Len( aMacro ) > 1
      aTemp := ListAsArray2( aMacro[ 2 ], " " )
      AEval( aTemp, { | xItem | AAdd( s_aBuildOrder, xItem ) } )
   ENDIF

   AAdd( s_aBuildOrder, aMacro[ 1 ] )
   cRead := Strtran( cRead, "@&&!", "" )

   aMacro := ListAsArray2( cRead, '\' )

   AEval( aMacro, { | xMacro |  IIF(  "$" IN xmacro , Findmacro( xMacro, @cRead ), ) } )

   IF ! s_lLinux
      s_cLinkComm   := cRead + "  @" + s_cLinker
      s_nLinkHandle := FCreate( s_cLinker )
   ELSE
      s_cLinkComm := cRead + " "
   ENDIF

   FOR nPos := 1 TO 7

      cRead        := Alltrim( readln( @s_lEof ) )
      cCurrentRead := cRead

      aMacro       := ListAsArray2( cRead, " " )

        FOR EACH cMacro IN aMacro

         IF  "$" IN  cMacro
            Findmacro( cMacro , @cRead )

            IF At( '$(PROJECT)', cCurrentRead ) > 0

               IF ! s_lGcc

                  IF ! s_lLinux
                     FWrite( s_nLinkHandle, cRead + CRLF )
                  ENDIF

               ELSEIF s_lGcc .AND. s_lLinux
                  s_cLinkComm += "-o " + cRead + " "
               ELSEIF s_lGcc .AND. ! s_lLinux .AND. At( '.exe', cread ) > 0
                  FWrite( s_nLinkHandle, "-o " + cRead + CRLF )
               ENDIF

            ELSE

               IF ! s_lLinux
                  FWrite( s_nLinkHandle, cRead + CRLF )
               ELSE
                  s_cLinkComm += cRead + " "
               ENDIF

            ENDIF

         ENDIF

      NEXT

   NEXT

   IF ! s_lLinux
      FClose( s_nLinkHandle )
   ENDIF

RETURN NIL

*----------------------
FUNCTION CompileFiles()
*----------------------

   LOCAL cComm
   LOCAL cOld
   LOCAL nPos
   LOCAL nCount
   LOCAL nFiles
   LOCAL cErrText := ""
   LOCAL aOrder   := ListAsArray2( s_aBuildOrder[ 2 ], " " )
   LOCAL lEnd     := .F.
   LOCAL xItem
   LOCAL lLinux   :=  'linux' IN  Lower( Os() )
   LOCAL cPrg     := ''
   LOCAL cOrder   := ""
   LOCAL nFile    := 1
   LOCAL aGauge   := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B", '≤' )

   @  4,  5 SAY "Compiling :"

   FOR EACH cOrder in aOrder

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

            FOR EACH cPrg in s_aPrgs

               xItem := Substr( cPrg, Rat( IIF( s_lGcc, '/', '\' ), ;
                                cPrg ) + 1 )
               nPos := AScan( s_aCs, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), ;
                  Left( x, At( ".", x ) ) == Left( xItem, At( ".", xItem ) ) } )

               IF nPos > 0
                  cComm := Strtran( cComm, "o$*", "o" + s_aCs[ nPos ] )
                  cComm := Strtran( cComm, "$**", cPrg )
                  cComm += IIF( AT("LINUX" ,upper( Os() ) ) >0 ,  " > "+ (s_cLog)," >>"+ (s_cLog))
                  Outstd( cComm )
                  Outstd( Hb_OsNewLine() )
                  setpos(9,0)
                  __RUN( (cComm) )
                  cErrText := Memoread( (s_cLog) )
                  lEnd     := 'C2006' $ cErrText .OR. 'No code generated' $ cErrText

                  IF ! s_lIgnoreErrors .AND. lEnd
                     IIF(  "LINUX" IN Upper( Os() ), __run( "mcedit " +(s_cLog) ), __run( "Notepad " + (s_cLog) ) )
                     set cursor on
                     QUIT
                  ELSE
                     // Ferase( (s_cLog) )
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

            FOR nFiles := 1 TO Len( s_aCs )

               xItem := Substr( s_aCs[ nFiles ], Rat( IIF( s_lGcc, '/', '\' ), ;
                                s_aCs[ nFiles ] ) + 1 )
               nPos := AScan( s_aObjs, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), ;
                  Left( x, At( ".", x ) ) == Left( xItem, At( ".", xItem ) ) } )

               IF nPos > 0

                  IF llinux
                     cComm := Strtran( cComm, "o$*", "o" + s_aObjs[ nPos ] )
                  ELSE
                      cComm := Strtran( cComm, "o$*", "o" + Strtran( s_aObjs[ nPos ], '/', '\' ) )
                  ENDIF

                  cComm := Strtran( cComm, "$**", s_aCs[ nFiles ] )
                  cComm += IIF( AT("LINUX" ,upper( Os() ) ) >0 ,  " > "+ (s_cLog)," >>"+ (s_cLog))
                  Outstd( " " )
                  Outstd( cComm )
                  Outstd( Hb_OsNewLine() )
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

            IF Len( s_aCs ) > 0
               GaugeDisplay( aGauge )
               nFile := 1

               FOR nFiles := 1 TO Len( s_aCs )
                  @  4, 16 SAY Space( 50 )
                  xItem := Substr( s_aCs[ nFiles ], Rat( IIF( s_lGcc, '/', '\' ), ;
                                   s_aCs[ nFiles ] ) + 1 )
                  nPos := AScan( s_aObjsc, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), ;
                     Left( x, At( ".", x ) ) == Left( xitem, At( ".", xitem ) ) } )

                  IF nPos > 0

                     IF llinux
                        cComm := Strtran( cComm, "o$*", "o" + s_aObjsc[ nPos ] )
                     ELSE
                        cComm := Strtran( cComm, "o$*", "o" + Strtran( s_aObjsc[ nPos ], '/', '\' ) )
                     ENDIF

                     cComm := Strtran( cComm, "$**", s_aCs[ nFiles ] )

                     cComm += IIF( AT("LINUX" ,upper( Os() ) ) >0 ,  " "," >>"+ (s_cLog))

                     @  4, 16 SAY s_aCs[ nFiles ]
                     GaugeUpdate( aGauge, nFile / Len( s_aCs ) )   // Changed s_aPrgs to s_aCs, Ath 2004-06-08
                     nFile ++
                     //                            Outstd( cComm )
                     setpos(9,0)
                     __RUN( (cComm) )
                     cErrText := Memoread( (s_cLog) )
                     lEnd     := 'Error E' $   cErrText
                     IF ! s_lIgnoreErrors .AND. lEnd
                        IIF(  "LINUX" IN Upper( Os() ), __run( "mcedit " + (s_cLog) ), __run( "Notepad " + (s_cLog)) )
                        set cursor on
                        QUIT
                     ELSE
                        //                                Ferase( (s_cLog) )
                      ENDIF
                     lEnd     := 'Error F' $   cErrText

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

            FOR EACH cPrg In s_aPrgs

               @  4, 16 SAY Space( 50 )
               xItem := Substr( cPrg, Rat( IIF( s_lGcc, '/', '\' ), ;
                                cPrg ) + 1 )
               nPos := AScan( s_aObjs, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), ;
                  Left( x, At( ".", x ) ) == Left( xItem, At( ".", xitem ) ) } )

               IF nPos > 0

                  IF llinux
                     cComm := Strtran( cComm, "o$*", "o" + s_aObjs[ nPos ] )
                  ELSE
                     cComm := Strtran( cComm, "o$*", "o" + Strtran( s_aObjs[ nPos ], '/', '\' ) )
                  ENDIF

                  cComm := Strtran( cComm, "$**", cPrg )
                  cComm += IIF( AT("LINUX" ,upper( Os() ) ) >0 ,  " > "+ (s_cLog)," >>"+ (s_cLog))

                  @  4, 16 SAY cPrg
                  GaugeUpdate( aGauge, nFile / Len( s_aPrgs ) )
                  //                        Outstd( Hb_OsNewLine() )
                  nFile ++
                  setpos(9,0)
                  __RUN( (cComm) )
                  cErrText := Memoread( (s_cLog) )
                  lEnd     := 'C2006' $ cErrText .OR. 'No code generated' $ cErrText .or. "Error E" $ cErrText .or. "Error F" $ cErrText

                  IF ! s_lIgnoreErrors .AND. lEnd
                     IIF(  "LINUX" IN Upper( Os() ), __run( "mcedit " + (s_cLog) ), __run( "Notepad " + (s_cLog)) )
                     set cursor on
                     QUIT
                  ELSE
                     //                            Ferase( (s_cLog) )
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

         FOR nFiles := 1 TO Len( s_aRes )

            IF ! Empty( s_aRes[ nFiles ] )
               cComm := Strtran( cComm, "$<", s_aRes[ nFiles ] )
               Outstd( " " )
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

   aSet := ListAsArray2( cTemp, "=" )
   nPos := AScan( s_aDefines, { | x | x[ 1 ] == aSet[ 1 ] } )

   IF nPos == 0
      cRead    := Alltrim( Strtran( aSet[ 2 ], "$(", "" ) )
      cRead    := Strtran( cRead, ")", "" )
      nMakePos := AScan( s_aDefines, { | x | x[ 1 ] == cRead } )

      IF nMakePos = 0
         ASet[ 2 ] := Strtran( aSet[ 2 ], ",", " " )
         AAdd( s_aDefines, { aSet[ 1 ], aSet[ 2 ] } )
         AAdd( s_aMacros, { aSet[ 1 ], aSet[ 2 ] } )
      ENDIF

   ENDIF

RETURN NIL

*---------------------
FUNCTION PrintMacros()
*---------------------

   LOCAL nPos

   Outstd( "HBMAKE - Version ", HBMakeVersion(), "CopyRight (c) 2000-2004 The xHarbour Project" + CRLF )
   Outstd( "" + CRLF )
   Outstd( "Macros:" + CRLF )
   AEval( s_aMacros, { | xItem | Outstd( "     " + xItem[ 1 ] + " = " + xItem[ 2 ] + CRLF ) } )
   Outstd( "Implicit Rules:" + CRLF )
   AEval( s_aCommands, { | xItem | Outstd( "     " + xItem[ 1 ] + Hb_OsNewLine() + "        " + xItem[ 2 ] + CRLF ) } )
   Outstd( "" + CRLF )
   Outstd( "Targets:" )
   Outstd( "    " + s_szProject + ":" + CRLF )
   Outstd( "        " + "Flags :" + CRLF )
   Outstd( "        " + "Dependents :" )
   AEval( s_aCs, { | xItem | Outstd( xitem + " " ) } )
   AEval( s_aObjs, { | xItem | Outstd( xitem + " " ) } )
   Outstd( " " + CRLF )
   Outstd( "        commands:" + s_aBuildOrder[ Len( s_aBuildOrder ) ] )
   Outstd( " " + CRLF )
   Outstd( " " + CRLF )
   Outstd( " " + CRLF )

RETURN NIL

*-------------------------------
FUNCTION CreateMakeFile( cFile )
*-------------------------------

   LOCAL aInFiles     := {}
   LOCAL aOutFiles    := {}
   LOCAL aOutc        := {}
   LOCAL aSrc         := Directory( "*.prg" )
   LOCAL nLenaSrc     := Len( aSrc )
   LOCAL nLenaOut

   LOCAL lFwh         := .F.
// LOCAL lxFwh        := .F. 
   LOCAL lC4W         := .F. 
   LOCAL lMiniGui     := .F.
   LOCAL lHwGui       := .F.
   LOCAL lWhoo        := .F.
   LOCAL lWhat32      := .F.
   LOCAL lGtWvt       := .F.
   LOCAL lXwt         := .F.

   LOCAL lRddAds      := .F.
   LOCAL lMediator    := .F.
   LOCAL lApollo      := .F.

// LOCAL lMt          := .F.
   LOCAL cOs          := IIF( "LINUX" IN UPPER( OS() ), "Linux", "Win32")
   LOCAL cCompiler    := IIF( "LINUX" IN UPPER( OS() ), "GCC","BCC")

   // External GUI Libs
   LOCAL cFwhPath     := Space( 200 )
   LOCAL cC4WPath     := Space( 200 )
   LOCAL cMiniPath    := Space( 200 )
   LOCAL cHwPath      := Space( 200 )

   LOCAL cMedPath     := Space( 200 )
   LOCAL cApolloPath  := Space( 200 )

   LOCAL cObjDir      := "obj" + Space( 20 )
   LOCAL lAutoMemvar  := .F.
   LOCAL lVarIsMemvar := .F.
   LOCAL lDebug       := .F.
   LOCAL lSupressLine := .F.
   LOCAL nPos
   LOCAL cDefHarOpts  := ""

// LOCAL nWarningLevel :=0

   LOCAL lUseXharbourDll := .F.

   LOCAL lCompMod         := .F.

   LOCAL lGenppo          := .F.
   LOCAL x
   LOCAL getlist          := {}
   LOCAL cTopFile         := Space( 50 )
   LOCAL cAppName         := s_Appname + Space( 50 )
   LOCAL cDefBccLibs      := "bcc640.lib lang.lib vm.lib rtl.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib dbffpt.lib dbfdbt.lib common.lib gtwin.lib codepage.lib ct.lib tip.lib"
   LOCAL cDefGccLibs      := "-lvm -lrtl -lgtdos -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -ldbffpt -ldbfdbt -lcommon -lcodepage -lm"
   LOCAL cGccLibsOs2      := "-lvm -lrtl -lgtos2 -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -ldbffpt -ldbfdbt -lcommon -lcodepage -lm"
   LOCAL cDefLibGccLibs   := "-lvm -lrtl -lgtcrs -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -ldbffpt -ldbfdbt -lcommon -lcodepage -lgtnul"
   LOCAL cDefBccLibsMt    := "bcc640mt.lib lang.lib vmmt.lib rtlmt.lib rddmt.lib macromt.lib ppmt.lib dbfntxmt.lib dbfcdxmt.lib  dbffptmt.lib dbfdbtmt.lib common.lib gtwin.lib codepage.lib ctmt.lib tipmt.lib"
   LOCAL cDefGccLibsMt    := "-lvmmt -lrtlmt -lgtdos -llang -lrddmt -lrtlmt -lvmmt -lmacromt -lppmt -ldbfntxmt -ldbfcdxmt -ldbffptmt -ldbfdbtmt -lcommon -lcodepage -lm"
   LOCAL cGccLibsOs2Mt    := "-lvmmt -lrtlmt -lgtos2 -llang -lrddmt -lrtlmt -lvmmt -lmacromt -lppmt -ldbfntxmt -ldbfcdxmt -ldbffptmt -ldbfdbtmt -lcommon -lcodepage -lm"
   LOCAL cDefLibGccLibsMt := "-lvmmt -lrtlmt -lgtcrs -llang -lrddmt -lrtlmt -lvmmt -lmacromt -lppmt -ldbfntxmt -ldbfcdxmt -ldbffptmt -ldbfdbtmt -lcommon -lcodepage"
   LOCAL cHarbDll         := "bcc640.lib harbour.lib"
   LOCAL cHARso           := "-lxharbour -lncurses -lgpm -lslang -lpthread -lm"
   LOCAL cSystemLibs      := "-lncurses -lslang -lgpm -lpthread -lm"

   LOCAL cLibs        := ""
   LOCAL citem        := ""
   LOCAL cExt         := ""
   LOCAL cDrive       := ""
   LOCAL cPath        := ""
   LOCAL cTest        := ""
   LOCAL cGuiLib      := "None"
   LOCAL aLibs
   LOCAL aLibsIn      := {}
   LOCAL aLibsOut     := {}
   LOCAL cGt          := ""

   LOCAL cOldLib      := ""
   LOCAL cHtmlLib     := ""
   LOCAL lLinux       := "linux" IN Lower( Os() )
   LOCAL nWriteFiles  := 0
   LOCAL cResName     := Space( 50 )
   LOCAL aSelFiles

   LOCAL cBuild       := " "
   LOCAL aUserDefs
   LOCAL cCurrentDef  := ""
   LOCAL cRdd         := "None"
   LOCAL cCurrentDir  := ""
   LOCAL nOption
   LOCAL lNew := .F.
   LOCAL oMake
   LOCAL cAllRes      := ""
   LOCAL cTemp
   LOCAL cExtraLibs   :=""
   LOCAL cTempLibs    := ""
   LOCAL aTempLibs

   #IFdef HBM_USE_DEPENDS
   LOCAL cIncl              := ""
   LOCAL lScanIncludes      := .F.
   // Provisions for recursive scanning
   LOCAL lScanInclRecursive := .F.
   LOCAL cExcludeExts       := PadR(".ch",40)
   #ENDIF

   #ifndef __PLATFORM__Windows
       LOCAL lHashhso := File("/usr/lib/libxharbour.so")
       LOCAL lusexhb := FILE("/usr/bin/xhb-build")
   #ELSE
       LOCAL lusexhb := .F.
   #ENDIF

   LOCAL cHarbourLibDir := GetHarbourDir()+iif(s_lLinux,"/lib","\lib")


   IF nLenaSrc == 0 .and. !s_lRecurse
      IF s_nLang=1 // PT-BR
         s_cMsg := "N∆o h† nenhum prg na pasta "+curdir()
      ELSEIF s_nLang=3 // Spanish
         s_cMsg := "No hay ning£n prg en la carpeta "+curdir()
      ELSE
         s_cMsg := "Have not any prg in "+curdir()+" folder."
      ENDIF
      Alert( s_cMsg )
      RETURN NIL
   ENDIF


   IF File( cFile )

      IF s_nLang == 1 // Portuguese-BR
          nOption := Alert( "O makefile <" + cFile +"> j† existe.",{ "Criar Novo" , "Editar","Cancelar" } )
      ELSEIF s_nLang == 3 // Spanish
          nOption := Alert( "Lo makefile <" + cFile +"> ya existe.",{ "Crear Nuevo" , "Editar","Cancelar" } )
      ELSE // English
          nOption := Alert( "The makefile <" + cFile +"> already exist ",{ "Create New" , "Edit" , "Cancel" } )
      ENDIF


      IF nOption == 1 // create a new makefile

         s_nLinkHandle := FCreate( cFile )
         WriteMakeFileHeader()
         lNew := .T.

      ELSEIF nOption == 2 // edit the makefile

         oMake :=ThbMake():new()
         oMake:cMakefile:=cFile
         oMake:ReadMakefile(cFile)
         frename(cFile,cfile+".old")

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
         lGenppo         := oMake:lGenppo
         cRdd            := IIF( oMake:lRddAds, "RddAds", IIF( oMake:lMediator, "Mediator", "None" ) )
         cGuiLib         := IIF( oMake:lFwh, "FWH", IIF( oMake:lMini , "MINIGUI",IIF(oMake:lWhoo, "WHOO",  IIF( oMake:lCw, "C4W", IIF( oMake:lHwGui, "HWGUI",IIF( oMake:lGtWvt, "GTWVT", IIF( oMake:lXWt, "XWT", IIF( oMake:lWhat32, "WHAT32","" ) ) ) ) ) ) ) )
         cFwhpath        := padr(oMake:cFmc,200)
         cApolloPath     := padr(oMake:cFmc,200)
         cC4WPath        := padr(oMake:cFmc,200)
         cMiniPath       := padr(oMake:cFmc,200)
         cHwPath         := padr(oMake:cFmc,200)
         cMedpath        := padr(oMake:cMedpath,200)
         cAppName        := PadR(oMake:cAppLibName,20," ")
         s_cAppName      := oMake:cAppLibName
         s_lCompress     := oMake:lCompress
         s_lExternalLib  := oMake:lExternalLib
         s_cUserInclude  := PadR(oMake:cUserInclude,40," ")
         s_cUserDef      := PadR(oMake:cUserDef,40," ")
         s_lxFwh         := oMake:lxFwh
         s_nFilesToAdd   := oMake:cFilesToAdd
         s_lMt           := oMake:lMt
         s_nWarningLevel := oMake:cWarningLevel
         cTopFile        := PadR(oMake:cTopModule,50," ")
         cResName        := PadR(oMake:cRes,50)
         s_lRecurse      := oMake:lRecurse

      ELSE
         SetColor("W/N,N/W")
         CLS
         set cursor on
         QUIT
      ENDIF

   ELSE
      s_nLinkHandle := FCreate( cFile )
      WriteMakeFileHeader()
      nOption := 1  // create a new makefile
      lNew := .T.
   ENDIF

   CLS
   Setcolor( 'w/b+,b+/w,w+/b,w/b+,w/b,w+/b' )
   @  0,  0, Maxrow(), Maxcol() BOX( Chr( 201 ) + Chr( 205 ) + Chr( 187 ) + Chr( 186 ) + Chr( 188 ) + Chr( 205 ) + Chr( 200 ) + Chr( 186 ) + Space( 1 ) )

   Attention( s_aLangMessages[ 27 ], 0 )
   Attention( s_aLangMessages[ 47 ], maxrow() )
   @  1,  1 SAY s_aLangMessages[ 28 ]

   @ 01,12,06,18 get cOs listbox { "Win32", "OS/2", "Linux" } message s_aLangMessages[ 49 ] state OsSpec(getlist,1,@cos)  DROPDOWN
   @ 01,21       say s_aLangMessages[ 29 ]
   @ 01,40,06,46 get cCompiler listbox { "BCC", "MSVC", "GCC" }  message s_aLangMessages[ 50 ] state OsSpec(getlist,2,@cCompiler) DROPDOWN
   @ 01,48       say s_aLangMessages[ 30 ]
   @ 01,60,10,78 get cGuiLib ListBox { "None","C4W","FWH","GTWVT","HWGUI","MINIGUI","XWT","WHAT32","WHOO"} state OsSpec(getlist,3,@cGuiLib) DROPDOWN  When CheckCompiler(cOs) message s_aLangMessages[ 51 ]
   @ 02,01       say s_aLangMessages[ 48 ]
   @ 02,16,08,26 get cRdd ListBox { "None","RddAds","Mediator","Apollo"}  WHEN Cos == "Win32" .or. Cos == "Linux" DROPDOWN message s_aLangMessages[ 52 ]
   @ 02,30       get s_lCompress CheckBox  caption s_aLangMessages[ 53 ] style "[o ]" message s_aLangMessages[ 54 ]
   @ 02,53       get lUseXharbourDll CheckBox caption "use xHarbour[.dll|.so]" style "[o ]" WHEN Cos == "Win32" .or. Cos == "Linux" message s_aLangMessages[ 55 ]
   @ 03,01       say "Obj Files Dir" GET cObjDir PICT "@s20" message s_aLangMessages[ 56 ]
   @ 04,01       say s_aLangMessages[ 45 ] GET cAppName  pict "@s15"valid !Empty( cAppName ) message s_aLangMessages[ 57 ]
   @ 04,53       get s_lasdll CheckBox  Caption "Create dll" style "[o ]"

   READ MSG AT MaxRow() - 1, 1, MaxCol() - 1

   s_cAppName := alltrim(cAppName)

   IF cOs != "Linux"
      if s_lasdll
         s_cAppName += ".dll"
      else
         s_cAppName += ".exe"
      endif
   ENDIF

   if s_lasdll
      lUseXharbourDll:= .T.
   endif

   lFwh      := "FWH"      IN alltrim(cGuiLib)
   lC4W      := "C4W"      IN alltrim(cGuiLib)
   lMiniGui  := "MINIGUI"  IN alltrim(cGuiLib)
   lHwGui    := "HWGUI"    IN alltrim(cGuiLib)
   lWhoo     := "WHOO"     IN alltrim(cGuiLib)
   lWhat32   := "WHAT32"   IN alltrim(cGuiLib)
   lGtWvt    := "GTWVT"    IN alltrim(cGuiLib)
   lXwt      := "XWT"      IN alltrim(cGuiLib)

   lRddAds   := "RddAds"   IN cRdd
   lMediator := "Mediator" IN cRdd
   lApollo   := "Apollo"   IN cRdd

   tracelog(lUseXharbourDll)

   IF lUseXharbourDll
      cDefLibGccLibs   := cHARso
      cDefBccLibs      := cHarbDll
   ENDIF

   IF lFwh
      @  3, 40 SAY "FWH path" GET cFwhPath PICT "@s25"
   ELSEIF lC4W
      @  3, 40 SAY "C4W path" GET cC4WPath PICT "@s25"
   ELSEIF lMiniGui
      @  3, 40 SAY "MiniGui path" GET cMiniPath PICT "@s25"
   ELSEIF lHwGui
      @  3, 40 SAY "HwGUI path" GET cHwPath PICT "@s25"
   ENDIF

   IF lMediator
      @  3, 40 SAY "Mediator path" GET cMedPath PICT "@s25"
   ENDIF

   IF lApollo
      @  3, 40 SAY "Apollo path" GET cApolloPath PICT "@s25"
   ENDIF

   IF nOption == 1 // create a new makefile
      cResName := PadR(alltrim(cResName)+iIF(!empty(cResName)," ","")+alltrim(cAllRes),50," ")
   ENDIF

//   @  3, 40 SAY "Obj Files Dir" GET cObjDir PICT "@s15"
//   @  4, 1  SAY  s_aLangMessages[ 45 ] GET cAppName VALID ! Empty( cAppName )
   Attention( s_aLangMessages[ 31 ], 5 )

   @ 06, 01 GET lautomemvar checkbox caption s_aLangMessages[ 32 ] style "[o ]"
   @ 06, 40 GET lvarismemvar checkbox caption s_aLangMessages[ 33 ] style "[o ]"
   @ 07, 01 GET lDebug checkbox caption s_aLangMessages[ 34 ] style "[o ]"
   @ 07, 40 GET lSupressline checkbox caption s_aLangMessages[ 35 ] style "[o ]"
   @ 08, 01 GET lGenppo checkbox caption s_aLangMessages[ 36 ] style "[o ]"
   @ 08, 40 GET lCompMod checkbox caption s_aLangMessages[ 37 ] style "[o ]"
   @ 09, 01 SAY s_aLangMessages[ 38 ] GET s_cUserDef PICT "@s15"
   @ 09, 40 SAY s_aLangMessages[ 39 ] GET s_cUserInclude PICT "@s10"
   @ 10, 01 GET s_lExternalLib checkbox caption s_aLangMessages[ 40 ] style "[o ]"
   @ 10, 40 GET s_lxFwh checkbox caption "xHarbour FWH" style "[o ]"
   @ 11, 01 SAY "Resource file Name: " GET cResName
   @ 12, 01 SAY s_aLangMessages[ 43 ] GET s_nFilestoAdd PICT "99" VALID s_nFilestoAdd > 0
   @ 13, 01 GET s_lMt checkbox caption s_aLangMessages[ 44 ] style "[o ]"
   @ 13, 40 SAY s_aLangMessages[ 46 ] GET s_nWarningLevel Pict "9" VALID s_nWarningLevel>=0 .AND. s_nWarningLevel <= 4
// READ
   READ msg at maxrow()-1,1,maxcol()-1

   IF ! Empty( s_cUserDef )
      aUserDefs := ListasArray2(Alltrim( s_cUserDef ), ";")

      FOR EACH cCurrentDef in aUserDefs
         cDefHarOpts += " -D" + Alltrim( cCurrentDef ) + " "
      NEXT

   ENDIF

   IF ! Empty( s_cUserInclude )
      cDefHarOpts += " -I" + Alltrim( s_cUserInclude ) + " "
   ENDIF

   s_lBcc := "BCC"  IN cCompiler
   s_lMSVcc := "MSVC" IN cCompiler
   s_lGcc := "GCC"  IN cCompiler

   cObjDir := Alltrim( cObjDir )

   IF "Linux" in cOs
       cCurrentDir:='/'+CurDir()
   ELSE
       cCurrentDir:=CurDrive()+":\"+CurDir()
   ENDIF

   IF ! Empty( cObjDir )

      IF DirChange( cObjDir ) != 0
         MakeDir( cObjDir )
      ELSE
   //    DirChange( '..' )
         DirChange( cCurrentDir )
      ENDIF

   ENDIF

   s_aMacros := GetSourceDirMacros( s_lGcc, cos )

   IF lLinux
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
      cDefHarOpts += " -a "
   ENDIF

   IF lvarismemvar
      cDefHarOpts += " -v "
   ENDIF


   IF ldebug
      cDefHarOpts      += " -b "
      cDefBccLibs      += " debug.lib "
      cDefGccLibs      += " -ldebug "
      cGccLibsOs2      += " -ldebug "
      cDefLibGccLibs   += " -ldebug "
      cDefBccLibsMt    += " debug.lib "
      cDefGccLibsMt    += " -ldebug "
      cGccLibsOs2Mt    += " -ldebug "
      cDefLibGccLibsMt += " -ldebug "
   ENDIF

   IF lSupressline
      cDefHarOpts += " -l "
   ENDIF

   IF lGenppo
      cDefHarOpts += " -p "
   ENDIF

   IF lCompmod
      cDefHarOpts += " -m "
   ENDIF

/*   IF s_nWarningLevel >= 0
      IF At("-w",cDefHarOpts)>0
         StrTran( cDefHarOpts, "-w", SubStr( cDefHarOpts, At( "-w", cDefHarOpts), 3 ), "-w" + Str(s_nWarningLevel,1) )
      ELSE
         cDefHarOpts += " -w" + str(s_nWarningLevel,1)
      ENDIF
   ELSE
      IF At( "-w", cDefHarOpts ) > 0
         StrTran( cDefHarOpts, "-w", SubStr( cDefHarOpts, At( "-w", cDefHarOpts), 3 ), "" )
      ENDIF
   ENDIF
*/

   IF s_nWarningLevel >= 0
      cDefHarOpts += " -w" + Str(s_nWarningLevel,1)
   ENDIF


   IF s_lBcc
      AAdd( s_aCommands, { ".cpp.obj:", "$(BCB)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $**" } )
      AAdd( s_aCommands, { ".c.obj:", "$(BCB)\BIN\bcc32 -I$(BHC)\include $(CFLAG1) $(CFLAG2) -o$* $**" } )

      IF s_lExtended
         AAdd( s_aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -D__EXPORT__ -n"+if(s_lasdll,"1","")+" -go -I$(BHC)\include $(HARBOURFLAGS)" + IIF( lFwh, " -I$(FWH)\include", IIF( lMinigui, " -I$(MINIGUI)\include",IIF( lHwgui, " -I$(HWGUI)\include","" ) ) )+IIF( lWhoo," -I$(WHOO)\include ","")+  IIF( lMediator," -I$(MEDIATOR)\include ","")+" -o$* $**" } )
      ELSE
         AAdd( s_aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS)" + IIF( lFwh, " -I$(FWH)\include", IIF( lMinigui, " -I$(MINIGUI)\include",IIF( lHwgui, " -I$(HWGUI)\include","" ) )) + " -o$* $**" } )
      ENDIF

      AAdd( s_aCommands, { ".rc.res:", "$(BCB)\BIN\brcc32 $(RFLAGS) $<" } )
   ELSEIF s_lGcc

        IF  "linux" IN Lower(Getenv( "HB_ARCHITECTURE" ) )  .OR. cOs == "Linux"
         AAdd( s_aCommands, { ".cpp.o:", "gcc $(CFLAG1) $(CFLAG2) -o$* $**" } )
         AAdd( s_aCommands, { ".c.o:", "gcc -I/usr/include/xharbour $(CFLAG1) $(CFLAG2) -I. -g -o$* $**" } )

         IF s_lExtended
            AAdd( s_aCommands, { ".prg.o:", "harbour -D__EXPORT__  -n"+if(s_lasdll,"1","")+"  -go -I/usr/include/xharbour $(HARBOURFLAGS) -I.  -o$* $**" } )
         ELSE
            AAdd( s_aCommands, { ".prg.c:", "harbour -n -I/usr/include/xharbour $(HARBOURFLAGS) -I.  -o$* $**" } )
         ENDIF

      ELSE
         AAdd( s_aCommands, { ".cpp.o:", "$(BCB)\bin\gcc $(CFLAG1) $(CFLAG2) -o$* $**" } )
         AAdd( s_aCommands, { ".c.o:", "$(BCB)\bin\gcc -I$(BHC)/include $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )

         IF s_lExtended
            AAdd( s_aCommands, { ".prg.o:", "$(BHC)\bin\harbour -D__EXPORT__  -n"+if(s_lasdll,"1","")+" -go -I$(BHC)/include $(HARBOURFLAGS)  -o$* $**" } )
         ELSE
            AAdd( s_aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)/include $(HARBOURFLAGS)  -o$* $**" } )
         ENDIF

      ENDIF

   ELSEIF s_lMSVcc
      AAdd( s_aCommands, { ".cpp.obj:", "$(BCB)\bin\cl $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
      AAdd( s_aCommands, { ".c.obj:", "$(BCB)\bin\cl -I$(BHC)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )

      IF s_lExtended
         AAdd( s_aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -D__EXPORT__  -n -I$(BHC)\include $(HARBOURFLAGS) -go  -I$(C4W)\include" + IIF( lMediator," -I$(MEDIATOR)\include ","")+ "-o$* $**" } )
      ELSE
         AAdd( s_aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -I$(C4W)\include -o$* $**" } )
      ENDIF

      AAdd( s_aCommands, { ".rc.res:", "$(BCB)\rc $(RFLAGS) $<" } )
   ENDIF

   Attention( s_aLangMessages[ 41 ], 22 )

   // Selecting PRG files.
   IF ! s_lRecurse
      aInFiles := GetSourceFiles( .F., s_lGcc, cOs )
      nLenaSrc := Len( aInFiles )
   ELSE
      aInFiles := GetSourceFiles(, s_lGcc, cOs )
      nLenaSrc := Len( aSrc )
   ENDIF

   IF "Win32" IN cOs
       AEval( aInFiles, { |x,y| aInFiles[y] := Upper( aInFiles[y] ) } )
   ENDIF

   aOutFiles := AClone( aInFiles )

   if s_nLang=1
      s_cMsg := "Selecione os PRGs a compilar"
   elseif s_nLang=3
      s_cMsg := "Seleccione los PRG a compilar"
   else
      s_cMsg := "Select the PRG files to compile"
   endif


   IF nOption != 1 // not create a new makefile
      pickarry( 11, 15, 20, 64, aInFiles, aOutFiles ,ArrayAJoin( { oMake:aPrgs, oMake:aCs } ), .T., s_cMsg )
   ELSE
      pickarry( 11, 15, 20, 64, aInFiles, aOutFiles, {}, .T., s_cMsg )
   ENDIF

   nLenaOut := Len( aOutFiles )

   AEval( aOutFiles, { | x, y | aOutFiles[ y ] := Trim( Substr( aOutFiles[ y ], 1, At( ' ', aOutFiles[ y ] ) ) ) } )

   aOutFiles := ASort( aOutFiles )

   @ 22,01 say space(78)

   aSelFiles := GetSelFiles( aInFiles, aOutFiles )

   ASort( aSelFiles )

   if Len( aSelFiles ) = 1
      cTopFile := aSelFiles[1] 
      cTopFile := PadR( Left(cTopfile,At(Upper(".prg"),Upper(cTopFile))+4 ), 50)
   endif

   WHILE Len( aSelFiles ) > 1

      IF s_nLang=1 // PT
         s_cMsg := "Informe o PRG principal da sua aplicaá∆o:"
      ELSEIF s_nLang=3
         s_cMsg := "Informe o PRG principale de su aplicacion:"
      ELSE
         s_cMsg := "Inform the main PRG of your application:"
      ENDIF

      @ 15,01 say s_cMsg Get cTopFile pict "@S35" valid !empty(cTopFile)
      READ

      if LastKey()=27
         Exit
      endif

      IF "Win32" IN cOs
         cTopFile := Upper( cTopFile )
      ENDIF


      IF !File( alltrim(cTopFile) )
         IF s_nLang=1 // PT
            s_cMsg := "Arquivo "+alltrim(cTopFile)+" n∆o encontrado."
         ELSEIF s_nLang=3
            s_cMsg := "Fichero "+alltrim(cTopFile)+" no encontrado."
         ELSE
            s_cMsg := "File "+alltrim(cTopFile)+" not found."
         ENDIF
         Alert( s_cMsg )
      ELSE
         EXIT
      ENDIF

   END


   // Selecting External Libs.
   IF s_lExternalLib

//    aLibs := GetLibs( s_lGcc, GetMakeDir() + '\lib' )
//      aLibs := GetLibs( s_lGcc, GetHarbourDir() + '\lib' )
      aLibs := GetLibs( s_lGcc, cHarbourLibDir )

      if len(aLibs)=0
         alert("aLibs is empty")
      endif

      IF s_nLang == 1 // PT
         s_cMsg := '<Espaáo> para selecionar. <Enter> para continuar o processo.'
      ELSEIF s_nLang == 2
         s_cMsg := '<Spacebar> to select. <Enter> to continue process'
      ELSEIF s_nLang == 3
         s_cMsg := '<Espacio> para seleccionar. <Enter> para continuar o proceso.'
      ENDIF

      Attention( s_cMsg, 22 )

      AEval( aLibs, { | x | AAdd( aLibsIn, x[ 1 ] ) } )
      AEval( aLibs, { | x | AAdd( aLibsOut, x[ 2 ] ) } )

      if s_nLang=1
         s_cMsg := "Selecione as LIBs externas a compilar"
      elseif s_nLang=3
         s_cMsg := "Seleccione las LIB externas a compilar"
      else
         s_cMsg := "Select the external LIBs to compile"
      endif


      IF nOption != 1 // not create a new makefile
         pickarry( 11, 15, 20, 64, aLibsIn, aLibsOut ,oMake:aExtLibs, .T. , s_cMsg, .T. )
      ELSE
         pickarry( 11, 15, 20, 64, aLibsIn, aLibsOut ,{}, .T., s_cMsg, .T. )
      ENDIF


   ENDIF

#IFDEF HBM_USE_DEPENDS
   clear typeahead
   Attention( "HBMake options", 16 )
   @ 17, 01 GET lScanIncludes checkbox      caption "Create #DEPENDS from #include" style "[o ]"
   // Provisions for recursive scanning
   @ 17, 40 GET lScanInclRecursive checkbox caption "Scan recursive" style "[o ]" //when lScanIncludes
   @ 18, 01 SAY "Excluding these extensions :" GET cExcludeExts WHEN lScanIncludes
   READ
#ENDIF


   AEval( aOutFiles, { | xItem | IIF(  '.c'IN xItem  .OR.  '.C' IN xItem , AAdd( aOutc, xItem ), ) } )
   AEval( aOutc, { | x, z | cItem := x, z := AScan( aOutFiles, { | t | t = cItem } ), IIF( z > 0, aSize( aDel( aOutFiles, z ), Len( aOutFiles ) - 1 ), ) } )

   @ 22,01 say space(78)

   aOutFiles  := ASort( aOutFiles )
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
   s_aCs := aClone( aOutc )

   IF ! s_lExtended
      AEval( aOutc, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, AAdd( s_aObjs, cObjDir + cTest + "." + Exten( cExt, 2 ) ), AAdd( s_aObjs, cObjDir + cTest + "." + Exten( cExt, 1 ) ) ) } )
      AEval( aOutFiles, { | xItem | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cExt := Substr( cExt, 2 ), AAdd( s_aCs, cObjDir + cTest + "." + Exte( cExt, 1 ) ) } )
   ELSE
      s_aObjsc := aClone( aOutc )
      AEval( aOutc, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, s_aObjsc[ x ] := IIF( ! Empty( cObjDir ), cObjDir, '' ) + cTest + "." + Exten( cExt, 2 ), s_aObjsc[ x ] := IIF( ! Empty( cObjDir ), cObjDir, '' ) + cTest + "." + Exten( cExt, 1 ) ) } )
   ENDIF

   IF !lNew
      s_nLinkHandle := FCreate( cFile )
      WriteMakeFileHeader()
   ENDIF

   FWrite( s_nLinkHandle, "COMPRESS = " + IIF( s_lCompress, "YES", "NO" ) + CRLF )

   FWrite( s_nLinkHandle, "EXTERNALLIB = " + IIF( s_lExternalLib, "YES", "NO" ) + CRLF )

   FWrite( s_nLinkHandle, "XFWH = " + IIF( s_lxFwh, "YES", "NO" ) + CRLF )

   FWrite( s_nLinkHandle, "FILESTOADD = " + Str( s_nFilesToAdd, 2 ) + CRLF )

   FWrite( s_nLinkHandle, "WARNINGLEVEL = " + Str(s_nWarningLevel, 2) + CRLF )

   FWrite( s_nLinkHandle, "USERDEFINE = " + s_cUserDef + CRLF )

   FWrite( s_nLinkHandle, "USERINCLUDE = " + s_cUserInclude + CRLF )

   IF lFwh
      FWrite( s_nLinkHandle, "FWH = " + alltrim(cFwhPath) + CRLF )
   ELSEIF lC4W
      FWrite( s_nLinkHandle, "C4W = " + alltrim(cC4WPath) + CRLF )
   ELSEIF lMiniGui
      FWrite( s_nLinkHandle, "MINIGUI = " + alltrim(cMiniPath) + CRLF )
   ELSEIF lHwGui
      FWrite( s_nLinkHandle, "HWGUI = " + alltrim(cHwPath) + CRLF )
   ELSEIF lGtwvt
      FWrite( s_nLinkHandle, "GTWVT = " + CRLF )
   ELSEIF lXwt
      FWrite( s_nLinkHandle, "XWT = " + CRLF )
   ELSEIF lWhoo
      FWrite( s_nLinkHandle, "WHOO = " + CRLF )
   ELSEIF lWhat32
      FWrite( s_nLinkHandle, "WHAT32 = " + CRLF )
   ENDIF

   IF lMediator
      FWrite( s_nLinkHandle, "MEDIATOR = " + alltrim(cMedPath) + CRLF )
   ENDIF

   IF lApollo
      FWrite( s_nLinkHandle, "APOLLO = " + alltrim(cApolloPath) + CRLF )
   ENDIF

   FWrite( s_nLinkHandle, "GUI = " + iif(lWhoo .or. lFwh .or. lC4W .or. lMinigui .or. lGtWvt .or. lHwGui .or. lXwt .or. lWhat32 , "YES", "NO" ) + CRLF )
   FWrite( s_nLinkHandle, "MT = " + IIF( s_lMt, "YES", "NO" ) + CRLF )

   FOR x := 1 TO Len( s_aMacros )

      IF ! Empty( s_aMacros[ x, 2 ] )

         cItem := s_aMacros[ x, 2 ]
         nPos  := AScan( s_aPrgs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0
            AEval( s_aPrgs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aPrgs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ')/', ')\' ) ), ) } )

            IF ! s_aMacros[ x, 3 ]
               FWrite( s_nLinkHandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
               s_aMacros[ x, 3 ] := .T.
            ENDIF

         ENDIF

         nPos := AScan( s_aCs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0

            IF ! s_aMacros[ x, 3 ]
               AEval( s_aCs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aCs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )
               FWrite( s_nLinkHandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
               s_aMacros[ x, 3 ] := .T.
            ENDIF

         ENDIF

         nPos := AScan( s_aObjs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0

            IF ! Empty( cObjDir )
               AEval( s_aObjs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aObjs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )
               FWrite( s_nLinkHandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
            ENDIF

         ENDIF

         IF s_lExtended
            nPos := AScan( s_aObjsc, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

            IF nPos > 0

               IF ! Empty( cObjDir )
                  AEval( s_aObjsc, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aObjsc[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )
               ENDIF

            ENDIF

         ENDIF

      ENDIF

   NEXT

   IF s_lGcc
      IF  "linux" IN Lower( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOs == "Linux"
         FWrite( s_nLinkHandle, "PROJECT = " + Alltrim( Lower( cAppName ) ) + " $(PR) " + CRLF )
      ELSE
         FWrite( s_nLinkHandle, "PROJECT = " + Alltrim( Lower( cAppName ) ) + ".exe"   + " $(PR) " + CRLF )
      ENDIF
   ELSE
      FWrite( s_nLinkHandle, "PROJECT = " + Alltrim( Lower( cAppName ) ) + if(s_lasdll,".dll",".exe" ) + " $(PR) " + CRLF )
   ENDIF


   IF ! s_lExtended

      FWrite( s_nLinkHandle, "OBJFILES = " )

      IF Len( s_aObjs ) < 1
         FWrite( s_nLinkHandle, + " $(OB) " + CRLF )
      ELSE
         AEval( s_aObjs, { | x, i | IIF( ( i <> Len( s_aObjs ) .AND. x <> alltrim(cTopfile)  ), FWrite( s_nLinkHandle, ' ' + Alltrim( x ) ), FWrite( s_nLinkHandle, " " + " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )
      ENDIF

      FWrite( s_nLinkHandle, "CFILES =" )

      IF Len( s_aCs ) < 1
         FWrite( s_nLinkHandle, + " $(CF)" + CRLF )
      ELSE
         AEval( s_aCs, { | x, i | IIF( ( i <> Len( s_aCs ) .AND. x <> alltrim(cTopfile)  ), FWrite( s_nLinkHandle, ' ' + Alltrim( x ) ), FWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(CF) " + CRLF ) ) } )
      ENDIF

      FWrite( s_nLinkHandle, "PRGFILE =" )

      IF Len( s_aPrgs ) < 1
         FWrite( s_nLinkHandle, + " $(PS)" + CRLF )
      ELSE
         AEval( s_aPrgs, { | x, i | IIF( i <> Len( s_aPrgs) .AND. x <> alltrim(cTopfile) , FWrite( s_nLinkHandle, ' ' + Alltrim( x ) ), FWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )
      ENDIF

   ELSE

      FWrite( s_nLinkHandle, "OBJFILES =" )

      IF Len( s_aObjs ) < 1
         FWrite( s_nLinkHandle, + " $(OB) " + CRLF )
      ELSE
         AEval( s_aObjs, { | x, i | nWriteFiles ++, IIF( ( i <> Len( s_aObjs ) .AND. x <> alltrim(cTopfile)  ), FWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % s_nFilestoAdd == 0, " //" + CRLF, "" ) ), FWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )
      ENDIF

      nWriteFiles := 0
      FWrite( s_nLinkHandle, "PRGFILES =" )

      IF Len( s_aPrgs ) < 1
         FWrite( s_nLinkHandle, + " $(PS)" + CRLF )
      ELSE
         AEval( s_aPrgs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aPrgs ), FWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % s_nFilestoAdd == 0, " //" + CRLF, "" ) ), FWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )
      ENDIF

      nWriteFiles := 0
      FWrite( s_nLinkHandle, "OBJCFILES =" )

      IF Len( s_aObjsc ) < 1
         FWrite( s_nLinkHandle, + " $(OBC) " + CRLF )
      ELSE
         AEval( s_aObjsc, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aObjsc ), FWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % s_nFilestoAdd == 0, " //" + CRLF, "" ) ), FWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(OBC) " + CRLF ) ) } )
      ENDIF

      nWriteFiles := 0
      FWrite( s_nLinkHandle, "CFILES =" )

      IF Len( s_aCs ) < 1
         FWrite( s_nLinkHandle, + " $(CF)" + CRLF )
      ELSE
         AEval( s_aCs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aCs ), FWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % s_nFilestoAdd == 0, " //" + CRLF, "" ) ), FWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )
      ENDIF


   ENDIF

   cResName := Lower( cResName )
   FWrite( s_nLinkHandle, "RESFILES = " + cResName + CRLF )
   FWrite( s_nLinkHandle, "RESDEPEN = " + Strtran( cResName, ".rc", ".res" ) + CRLF )
   FWrite( s_nLinkHandle, "TOPMODULE = " + alltrim(cTopFile) + CRLF )

   IF lRddads
      cDefBccLibs      += " rddads.lib ace32.lib"
      cDefLibGccLibs   += " -lrddads -ladsloc "
      cDefBccLibsMt    += " rddads.lib ace32.lib"
      cDefLibGccLibsMt += " -lrddads -ladsloc "
      cExtraLibs += " -lrddads -ladsloc "
   ENDIF


//   if !file(GetHarbourDir()+"\lib\ct.lib")
   if !file(cHarbourLibDir+"\ct.lib")
      cDefBccLibs   := StrTran( cDefBccLibs  , "ct.lib","hbct.lib")
      cDefBccLibsMt := StrTran( cDefBccLibsMt, "ctmt.lib","hbctmt.lib")
   endif

// if !file(GetHarbourDir()+"\lib\tip.lib")
   if !file(cHarbourLibDir+"\tip.lib")
      cDefBccLibs   := StrTran( cDefBccLibs  , "tip.lib","hbtip.lib")
      cDefBccLibsMt := StrTran( cDefBccLibsMt, "tipmt.lib","hbtipmt.lib")
   endif



   // if external libs was selected...
   IF Len( aLibsOut ) > 0 .AND. s_lExternalLib

      IF s_lMSVcc .OR. s_lBcc

         IF s_lMSVcc // remove bcc640.lib form msvc
            cDefBccLibs   := StrTran( cDefBccLibs, "bcc640.lib ", "")
            cDefBccLibsMt := StrTran( cDefBccLibsMt, "bcc640mt.lib ", "")
         ENDIF

         IF ! s_lMt
            cOldLib := cDefBccLibs
         ELSE
            cOldLib := cDefBccLibsMt
         ENDIF

         // searching for html lib...
         nPos := AScan( aLibsOut, { | z | At( "html", Lower( z ) ) > 0 } )

         IF nPos > 0
            cHtmlLib += aLibsOut[ nPos ]
            aDel( aLibsOut, nPos )
            aSize( aLibsOut, Len( aLibsOut ) - 1 )
            cOldLib := StrTran( cOldLib, "gtwin" , "gtcgi" )
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

         IF ! s_lMt
            cDefBccLibs := cHtmlLib + " " + cOldLib + " " + cLibs
         ELSE
            cDefBccLibsMt := cHtmlLib + " " + cOldLib + " " + cLibs
         ENDIF

      ENDIF

      IF s_lGcc

         nPos := AScan( aLibsOut, { | z | At( "html", Lower( z ) ) > 0 } )

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

         IF cOs == "Linux"

            IF ! s_lMt
               cOldLib        := " " + cDefLibGccLibs
               cDefLibGccLibs := cHtmlLib + " " + cOldLib + " " + cLibs

               IF "html" IN cDefLibGccLibs
                   cDefLibGccLibs := StrTran( cDefLibGccLibs, "gtcrs" , "gtcgi" )
                   cDefLibGccLibs := StrTran( cDefLibGccLibs, "ncurses" , "" )
               ENDIF

            ELSE

               cOldLib          := " " + cDefLibGccLibsMt
               cDefLibGccLibsMt := cHtmlLib + " " + cOldLib + " " + cLibs

               IF "html" IN cDefLibGccLibsMt
                   cDefLibGccLibsMt := StrTran( cDefLibGccLibsMt, "gtcrs" , "gtcgi" )
                   cDefLibGccLibsMt := StrTran( cDefLibGccLibsMt, "ncurses" , "" )
               ENDIF

           ENDIF

         ELSEIF cOs == "OS/2"

            IF ! s_lMt
               cOldLib     := " " + cGccLibsOs2
               cGccLibsOs2 := cHtmlLib + " " + cOldLib + " " + cLibs

               IF "html" IN cGccLibsOs2
                   cGccLibsOs2 := StrTran( cGccLibsOs2, "gtos2" , "gtcgi" )
               ENDIF

            ELSE
               cOldLib       := " " + cGccLibsOs2Mt
               cGccLibsOs2Mt := cHtmlLib + " " + cOldLib + " " + cLibs
               IF "html" IN cGccLibsOs2Mt
                   cGccLibsOs2Mt := StrTran( cGccLibsOs2Mt, "gtos2" , "gtcgi" )
               ENDIF

            ENDIF

         ELSE

            IF s_lMt
               cOldLib       := " " + cDefGccLibsMt
               cDefGccLibsMt := cHtmlLib + " " + cOldLib + " " + cLibs
            ENDIF

        ENDIF

      ENDIF

   ENDIF


   IF s_lBcc .OR. s_lMSVcc

      IF lFwh
         IF s_lxFwh
            FWrite( s_nLinkHandle, "LIBFILES = $(FWH)\lib\fivehx.lib $(FWH)\lib\fivehc.lib optgui"+ IIF( ! s_lMt, "", "mt" ) +".lib " + IIF( ! s_lMt, cDefBccLibs, cDefBccLibsMt ) + CRLF )
         ELSE
            FWrite( s_nLinkHandle, "LIBFILES = $(FWH)\lib\fiveh.lib $(FWH)\lib\fivehc.lib optgui"+ IIF( ! s_lMt, "", "mt" ) +".lib " + IIF( ! s_lMt, cDefBccLibs, cDefBccLibsMt ) + CRLF )
         ENDIF
      ELSEIF lMiniGui
         FWrite( s_nLinkHandle, "LIBFILES = minigui.lib optgui.lib " + IIF( ! s_lMt, cDefBccLibs, cDefBccLibsMt ) + CRLF )
      ELSEIF lWhoo
         FWrite( s_nLinkHandle, "LIBFILES = whoo.lib what32.lib " + IIF( ! s_lMt, cDefBccLibs, cDefBccLibsMt ) + CRLF )
      ELSEIF lWhat32
         FWrite( s_nLinkHandle, "LIBFILES = what32.lib " + IIF( ! s_lMt, cDefBccLibs, cDefBccLibsMt ) + CRLF )
      ELSEIF lHwGui
         FWrite( s_nLinkHandle, "LIBFILES = hwgui.lib procmisc.lib hwg_qhtm.lib " + IIF( ! s_lMt, cDefBccLibs, cDefBccLibsMt ) + CRLF )
      ELSEIF lC4W
         FWrite( s_nLinkHandle, "LIBFILES = $(C4W)\c4wclass.lib $(C4W)\wbrowset.lib $(C4W)\otabt.lib $(C4W)\clip4win.lib optgui.lib "  + IIF( ! s_lMt, cDefBccLibs, cDefBccLibsMt ) + CRLF )
      ELSE
         if lGtwvt
            cDefBccLibs   := strtran(cDefBccLibs,"gtwin","gtwvt")
            cDefBccLibsMt := strtran(cDefBccLibsMt,"gtwin","gtwvt")
         endif
         FWrite( s_nLinkHandle, "LIBFILES = " + iif( lGTWVT,"optgui",iif(!s_lasdll,"optcon","")) + iif( !s_lMt, "", "mt" ) + ".lib " + IIF( ! s_lMt, cDefBccLibs, cDefBccLibsMt ) + CRLF )
      ENDIF

   ELSEIF s_lGcc

      IF cOs == "Linux"
         FWrite( s_nLinkHandle, "LIBFILES = " + IIF(lusexhb, cExtraLibs , "-Wl,--start-group " + IIF( ! s_lMt, cDefLibGccLibs, cDefLibGccLibsMt ) + " -Wl,--end-group " + cSystemLibs ) + CRLF )
      ELSEIF cOs == "OS/2"
         FWrite( s_nLinkHandle, "LIBFILES = " + IIF( ! s_lMt, cGccLibsOs2, cGccLibsOs2Mt ) + CRLF )
      ELSE
         FWrite( s_nLinkHandle, "LIBFILES = " + IIF( ! s_lMt, cDefGccLibs, cDefGccLibs ) + CRLF )
      ENDIF

   ENDIF

   nWriteFiles := 0
   FWrite( s_nLinkHandle, "EXTLIBFILES =" )

   if Len(s_aExtLibs) < 1
      FWrite( s_nLinkHandle, CRLF ) 
   else
      AEval( s_aExtLibs, { | x, i | nWriteFiles ++, FWrite( s_nLinkHandle, " " + Alltrim( x )  ) } )
      FWrite( s_nLinkHandle, CRLF ) 
   endif

   FWrite( s_nLinkHandle, "DEFFILE = " + CRLF )
   FWrite( s_nLinkHandle, "HARBOURFLAGS = " + cDefHarOpts + CRLF )

   IF s_lBcc

      FWrite( s_nLinkHandle, "CFLAG1 =  -OS $(CFLAGS) -d -L$(BHC)\lib"+iif(lFwh,";$(FWH)\lib -c","")+iif(!empty(s_cUserInclude)," -I" + alltrim( s_cUserInclude ),"") + " " +CRLF )
      FWrite( s_nLinkHandle, "CFLAG2 =  -I$(BHC)\include;$(BCB)\include" + iif( s_lMt, "-DHB_THREAD_SUPPORT" , "" ) + CRLF )

      FWrite( s_nLinkHandle, "RFLAGS = " + CRLF )
      FWrite( s_nLinkHandle, "LFLAGS = -L$(BCB)\lib\obj;$(BCB)\lib;$(BHC)\lib -Gn -M -m -s -Tp"+ if(s_lasdll,"d","e") + IIF( lFWH, " -aa", IIF( lMiniGui .or. lWhoo , " -aa", IIF( lHwgui .or. lGtWvt, " -aa"," -ap" ) ) ) + IIF( lMinigui, " -L$(MINIGUI)\lib",IIF( lFwh, " -L$(FWH)\lib",IIF( lHwgui, " -L$(HWGUI)\lib","" ))) + CRLF )
      FWrite( s_nLinkHandle, "IFLAGS = " + CRLF )
      FWrite( s_nLinkHandle, "LINKER = ilink32" + CRLF )
      FWrite( s_nLinkHandle, " " + CRLF )
      FWrite( s_nLinkHandle, "ALLOBJ = " + IIF( ( lWhoo .OR. lWhat32 .OR. lFwh .OR. lMinigui .OR. lHwgui .or. lGtWvt .or. lXwt ), "c0w32.obj", if(s_lAsDll,"c0d32.obj","c0x32.obj" )) + " $(OBJFILES)" + IIF( s_lExtended, " $(OBJCFILES)", " " ) + CRLF )
      FWrite( s_nLinkHandle, "ALLRES = $(RESDEPEN)" + CRLF )
      FWrite( s_nLinkHandle, "ALLLIB = $(LIBFILES) import32.lib " + IIF( s_lMt,"cw32mt.lib", "cw32.lib" )+ CRLF )
      FWrite( s_nLinkHandle, ".autodepend" + CRLF )

   ELSEIF s_lMSVcc

      FWrite( s_nLinkHandle, "CFLAG1 =  -I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(CFLAGS)" +IIF( s_lMt, "-DHB_THREAD_SUPPORT" , "" ) + CRLF )
      FWrite( s_nLinkHandle, "CFLAG2 =  -c" +" -I" + Alltrim( s_cUserInclude ) + " " + CRLF )
      FWrite( s_nLinkHandle, "RFLAGS = " + CRLF )
      FWrite( s_nLinkHandle, "LFLAGS = /LIBPATH:$(BCB)\lib;$(BHC)\lib;$(C4W)\lib /SUBSYSTEM:CONSOLE" +IIF(s_lMt, " /Nodefaultlib:LIBC "," /Nodefaultlib:LIBCMT " ) + CRLF )
      FWrite( s_nLinkHandle, "IFLAGS = " + CRLF )
      FWrite( s_nLinkHandle, "LINKER = link" + CRLF )
      FWrite( s_nLinkHandle, " " + CRLF )
      FWrite( s_nLinkHandle, "ALLOBJ = " + IIF( lC4W, "$(C4W)\initc.obj", "" ) + "$(OBJFILES)" + IIF( s_lExtended, " $(OBJCFILES)", " " ) + CRLF )
      FWrite( s_nLinkHandle, "ALLRES = $(RESDEPEN)" + CRLF )
      FWrite( s_nLinkHandle, "ALLLIB = $(LIBFILES) comdlg32.lib shell32.lib user32.lib gdi32.lib" + CRLF )

   ELSEIF s_lGcc

      FWrite( s_nLinkHandle, "CFLAG1 = " +IIF( !EMPTY(s_cUserInclude ) ," -I" + Alltrim( s_cUserInclude ),"")        + IIF(  "Linux" IN cOs, "-I/usr/include/xharbour", " -I$(BHC)/include" ) + " -c -Wall" + IIF( s_lMt, "-DHB_THREAD_SUPPORT" , "" ) + CRLF )
      FWrite( s_nLinkHandle, "CFLAG2 = " + IIF(  "Linux" IN cOs, "-L$(HB_LIB_INSTALL)", " -L$(BHC)/lib" )  + CRLF )

      FWrite( s_nLinkHandle, "RFLAGS = " + CRLF )
      FWrite( s_nLinkHandle, "LFLAGS =" + IIF(lUseXhb ,IIF(lUseXharbourDll,"","-static ") + if(lXwt,"-gtcgi " , "-gtcrs "), "$(CFLAG2)") + iif(lXwt,"`pkg-config --libs gtk+-2.0` -lxwt -lxwt_gtk -lxwt","") + CRLF )
      FWrite( s_nLinkHandle, "IFLAGS = " + CRLF )
      FWrite( s_nLinkHandle, "LINKER = "+ IIF(lusexhb,"xhblnk","gcc") + CRLF )
      FWrite( s_nLinkHandle, " " + CRLF )
      FWrite( s_nLinkHandle, "ALLOBJ = $(OBJFILES) " + IIF( s_lExtended, " $(OBJCFILES)", " " ) + CRLF )
      FWrite( s_nLinkHandle, "ALLRES = $(RESDEPEN) " + CRLF )
      FWrite( s_nLinkHandle, "ALLLIB = $(LIBFILES) " + CRLF )
      FWrite( s_nLinkHandle, ".autodepend" + CRLF )

   ENDIF

#IFdef HBM_USE_DEPENDS

   FWrite( s_nLinkHandle, " " + CRLF )
   FWrite( s_nLinkHandle, "#DEPENDS" + CRLF )

   IF lScanIncludes
      // Clipper/(x)Harbour sources: .prg
      IF Len( s_aPrgs ) = Len( s_aObjs )
         Attention("Scanning .PRG sources...",19)
         FOR nPos := 1 to Len(s_aPrgs)
            cIncl := ScanInclude( s_aPrgs[ nPos ], lScanInclRecursive, cExcludeExts )
            // Only add in list if dependencies exist
            IF ! Empty(cIncl)
               FWrite( s_nLinkHandle, s_aObjs[ nPos ] + ': ' + Alltrim( cIncl ) + CRLF, "" )
            ENDIF
         NEXT
      ENDIF

      // C-sources: .c
      IF Len( s_aCs ) = Len( s_aObjsc )
         Attention("Scanning .C sources...",19)
         FOR nPos := 1 to Len(s_aCs)
            cIncl := ScanInclude( s_aCs[ nPos ], lScanInclRecursive, cExcludeExts )
            // Only add in list if dependencies exist
            IF ! Empty(cIncl)
               FWrite( s_nLinkHandle, s_aObjsc[ nPos ] + ': ' + Alltrim( cIncl ) + CRLF, "" )
            ENDIF
         NEXT
      ENDIF

      // Cleanup message
      @ 19, 1 say Space(MaxCol() - 2)
   ENDIF

#ENDIF

   FWrite( s_nLinkHandle, " " + CRLF )
   FWrite( s_nLinkHandle, "#COMMANDS" + CRLF )

   AEval( s_aCommands, { | xItem | FWrite( s_nLinkHandle, xitem[ 1 ] + CRLF ), FWrite( s_nLinkHandle, xitem[ 2 ] + CRLF ), FWrite( s_nLinkHandle, " " + CRLF ) } )

   IF s_lBcc
      FWrite( s_nLinkHandle, "#BUILD" + CRLF )
      FWrite( s_nLinkHandle, " " + CRLF )
      FWrite( s_nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)" + CRLF )
      FWrite( s_nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!  " + CRLF )
      FWrite( s_nLinkHandle, "    $(LFLAGS) +" + CRLF )
      FWrite( s_nLinkHandle, "    $(ALLOBJ), +" + CRLF )
      FWrite( s_nLinkHandle, "    $(PROJECT),, +" + CRLF )
      FWrite( s_nLinkHandle, "    $(ALLLIB), +" + CRLF )
      FWrite( s_nLinkHandle, "    $(DEFFILE), +" + CRLF )
      FWrite( s_nLinkHandle, "    $(ALLRES) " + CRLF )
      FWrite( s_nLinkHandle, "!" + CRLF )
   ELSEIF s_lMSVcc
      FWrite( s_nLinkHandle, "#BUILD" + CRLF )
      FWrite( s_nLinkHandle, "" + CRLF )
      FWrite( s_nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)" + CRLF )
      FWrite( s_nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&! " + CRLF )
      FWrite( s_nLinkHandle, "    $(LFLAGS)" + CRLF )
      FWrite( s_nLinkHandle, "    $(ALLOBJ) " + CRLF )
      FWrite( s_nLinkHandle, "    $(PROJECT)" + CRLF )
      FWrite( s_nLinkHandle, "    $(PROJECTMAP)" + CRLF )
      FWrite( s_nLinkHandle, "    $(ALLLIB) " + CRLF )
      FWrite( s_nLinkHandle, "    $(DEFFILE) " + CRLF )
      FWrite( s_nLinkHandle, "    $(ALLRES) " + CRLF )
      FWrite( s_nLinkHandle, "! " + CRLF )
   ELSEIF s_lGcc
      FWrite( s_nLinkHandle, "#BUILD" + CRLF )
      FWrite( s_nLinkHandle, " " + CRLF )
      FWrite( s_nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)" + CRLF )

      IF 'Linux' IN cOs
         FWrite( s_nLinkHandle, "    $(LINKER) @&&!" + CRLF )
      ELSE
         FWrite( s_nLinkHandle, "    $(BCB)\bin\$(LINKER) @&&!" + CRLF )
      ENDIF

      FWrite( s_nLinkHandle, "    $(PROJECT) " + CRLF )
      FWrite( s_nLinkHandle, "    $(ALLOBJ)  " + CRLF )
      FWrite( s_nLinkHandle, "    $(LFLAGS)  " + CRLF )
      FWrite( s_nLinkHandle, "    $(ALLLIB)  " + CRLF )
      FWrite( s_nLinkHandle, "!" + CRLF )
   ENDIF

   FClose( s_nLinkHandle )

   IF s_nLang == 1 .OR. s_nLang == 3
      s_cMsg := "Compilar app ? (S/N) "
   ELSE // English
      s_cMsg := "Build app ? (Y/N) "
   ENDIF

   @ 20,5 Say s_cMsg Get cBuild PICT "!" Valid cBuild $ iif(s_nLang=2,"YN","SN")
   READ

   IF cBuild == "S" .OR. cBuild == "Y"
      ResetInternalVars()
      SetColor("W/N,N/W")
      Clear
      SetPos(9,0)
      Main( cFile, " -f "+iif(s_nLang=1,"-lPT",iif(s_nLang=3,"-lES","-lEN")) )
   ELSE
      SetColor("W/N,N/W")
      Clear
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

       IF nHandle > 0

           // Provisions for recursive scanning
           // Add current file to list, making it by default the first in the list

           IF AT("WINDOWS" ,Upper( Os() ) ) > 0
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
                      IF AT("WINDOWS" ,Upper( Os() ) ) > 0
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
           FClose(nHandle)
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
   LOCAL nCount

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

   @ 4,5 SAY "Compiling :"

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
               nPos    := AScan( s_aCs, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( xItem, At( ".", xItem ) ) } )
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
                     AAdd( aCtocompile, s_aCs[ nPos ] )
                     cComm := Strtran( cComm, "o$*", "o" + s_aCs[ nPos ] )
                     cComm := Strtran( cComm, "$**", s_aPrgs[ nFiles ] )
                     cComm += IIF( AT("LINUX" ,upper( Os() ) ) >0 , " > "+ (s_cLog)," >>"+ (s_cLog))

                     //                   Outstd( cComm )
                     //                   Outstd( Hb_OsNewLine() )
                     setpos(9,0)
                     __RUN( (cComm) )
                     cErrText := Memoread( (s_cLog) )
                     lEnd     := 'C2006' $ cErrText .OR. 'No code generated' $ cErrText

                     IF ! s_lIgnoreErrors .AND. lEnd
                        IIF(  "LINUX" IN Upper( Os() ) , __run( "mcedit " + (s_cLog) ), __run( "Notepad " + (s_cLog) ) )
                        set cursor on
                        QUIT
                     ELSE
                        // Ferase( (s_cLog) )
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

               FOR nFiles := 1 TO Len( s_aCs )
                  nPos := AScan( s_aCs, { | x | Left( x, At( ".", x ) ) == Left( aCtoCompile[ nfiles ], At( ".", aCtoCompile[ nfiles ] ) ) } )

                  IF nPos == 0
                     AAdd( aCtoCompile, s_aCs[ nFiles ] )
                  ENDIF

               NEXT

            ENDIF

            FOR nFiles := 1 TO Len( aCtocompile )
               xItem := Substr( aCtocompile[ nFiles ], Rat( IIF( s_lGcc, '/', '\' ), aCtocompile[ nFiles ] ) + 1 )
               nPos  := AScan( s_aObjs, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( aCtocompile[ nFiles ], At( ".", xItem ) ) } )

               IF nPos > 0
                  cComm := Strtran( cComm, "o$*", "o" + s_aObjs[ nPos ] )
                  cComm := Strtran( cComm, "$**", aCtocompile[ nFiles ] )
                  cComm += IIF( AT("LINUX" ,upper( Os() ) ) >0 ,  " > "+ (s_cLog)," >>"+ (s_cLog))
                  Outstd( " " )

                  Outstd( cComm )
                  Outstd( Hb_OsNewLine() )
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

            FOR nFiles := 1 TO Len( s_aCs )
               @  4, 16 SAY Space( 50 )
               xItem := Substr( s_aCs[ nFiles ], Rat( IIF( s_lGcc, '/', '\' ), s_aCs[ nFiles ] ) + 1 )
               nPos  := AScan( s_aObjsc, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( xitem, At( ".", xitem ) ) } )

#IfDef HBM_USE_DEPENDS
               lNewer := .F.
               nPos1 := AScan( s_aDepends, { | x |lower(x[1]) == lower( s_aObjs[ npos ] )})
               IF nPos1 > 0
                  FOR EACH cDepSrc in s_aDepends[ nPos1 , 2 ]
                      lNewer := lNewer .OR. Fileisnewer( cDepSrc, s_aObjs[ npos ], .T. )
                  NEXT
               ENDIF
#Endif
               IF lNewer .or. Fileisnewer( s_aCs[ nFiles ], s_aObjsc[ nPos ] )

                  IF nPos > 0
                     cComm := Strtran( cComm, "o$*", "o" + s_aObjsc[ nPos ] )
                     cComm := Strtran( cComm, "$**", s_aCs[ nFiles ] )
                     cComm += IIF( AT("LINUX" ,upper( Os() ) ) >0 ,  " > "+ (s_cLog)," >>"+ (s_cLog))
                     @  4, 16 SAY s_aCs[ nFiles ]
                     GaugeUpdate( aGauge, nFile / Len( s_aCs ) )  // changed s_aPrgs to s_aCs Ath 2004-06-08
                     nFile ++
                     //                            Outstd( cComm )
                     //                            Outstd( Hb_OsNewLine() )
                     setpos(9,0)
                     __RUN( (cComm) )
                     cErrText := Memoread( (s_cLog) )
                     lEnd     := 'Error E' $ cErrText

                     IF ! s_lIgnoreErrors .AND. lEnd
                        IIF(  "LINUX" IN Upper( Os() ) , __run( "mcedit "  + (s_cLog)), __run( "Notepad " + (s_cLog) ) )
                        set cursor on
                        QUIT
                     ELSE
                        //                                Ferase( (s_cLog) )
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
                     cComm := Strtran( cComm, "o$*", "o" + s_aObjs[ nPos ] )
                     cComm := Strtran( cComm, "$**", cPrg )
                     cComm += IIF( AT("LINUX" ,upper( Os() ) ) >0 ,  " > "+ (s_cLog)," >>"+ (s_cLog))
                     @  4, 16 SAY cPrg
                     GaugeUpdate( aGauge, nFile / Len( s_aPrgs ) )
                     nFile ++     // moved from outside 'FOR EACH', Ath 2004-06-08

                     setpos(9,0)
                     __RUN( (cComm) )
                     cErrText := Memoread( (s_cLog) )
                     lEnd     := 'C2006' $ cErrText .OR. 'No code generated' $ cErrText .or. "Error E" $ cErrText .or. "Error F" $ cErrText

                     IF ! s_lIgnoreErrors .AND. lEnd
                        IIF( "LINUX" IN Upper( Os() ) , __run( "mcedit " + (s_cLog) ), __run( "Notepad "  + (s_cLog) ) )
                        set cursor on
                        QUIT
                     ELSE
                        //                                Ferase( (s_cLog) )
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

         FOR nFiles := 1 TO Len( s_aRes )

            IF ! Empty( s_aRes[ nFiles ] )
               cComm := Strtran( cComm, "$<", s_aRes[ nFiles ] )
               Outstd( " " )
               setpos(9,0)
               __RUN( (cComm) )
            ENDIF

            cComm := cOld

         NEXT

      ENDIF

   NEXT

RETURN NIL

*------------------------------------------
FUNCTION Fileisnewer( cFile, as, lInclude )
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
FUNCTION CreateLibMakeFile( cFile )
*----------------------------------

   LOCAL aInFiles  := {}
   LOCAL aOutFiles := {}
   LOCAL aSrc      := Directory( "*.prg" )
   LOCAL nLenaSrc  := Len( aSrc )
   LOCAL nLenaOut

   LOCAL aOutC     := {}
   LOCAL aSrcC     := Directory( "*.c" )
   LOCAL cOs       := IIF( "LINUX" IN UPPER( OS() ), "Linux", "Win32")
   LOCAL cCompiler := IIF( "LINUX" IN UPPER( OS() ), "GCC","BCC")
   LOCAL cFwhPath  := Left( cfile, At( '.', cfile ) - 1 ) + Space( 40 )

   LOCAL lAutomemvar     := .F.
   LOCAL lvarismemvar    := .F.
   LOCAL ldebug          := .F.
   LOCAL lSupressline    := .F.
   LOCAL cDefHarOpts     := ""
   LOCAL cObjDir         := 'obj' + Space( 20 )
   LOCAL lCompMod        := .F.
   LOCAL lInstallLibrary := .F.
   LOCAL x
   LOCAL y
   LOCAL nPos
   LOCAL lGenppo         := .F.
   LOCAL getlist         := {}
   LOCAL citem           := ""
   LOCAL cExt            := ""
   LOCAL cDrive          := ""
   LOCAL cPath           := ""
   LOCAL cTest           := ""
   LOCAL cLast           := ''
// LOCAL cUserdef        := Space( 40 )
// LOCAL cUserInclude    := Space( 40 )
   LOCAL nWriteFiles     := 0
   Local aUserDefs
   Local cCurrentDef     := ""

   IF nLenaSrc == 0 .AND. !s_lRecurse
      IF s_nLang == 1 // Portuguese-BR
         s_cMsg := "N∆o h† prg na pasta "+curdir()
      ELSEIF s_nLang == 3 // Spanish
         s_cMsg := "No hay ning£n prg en la carpeta "+curdir()
      ELSE
         s_cMsg := "Have not any prg in "+curdir()+" folder."
      ENDIF
      Alert( s_cMsg )
      RETURN NIL
   ENDIF

   s_nLinkHandle := FCreate( cFile )
   WriteMakeFileHeader()
   CLS
   Setcolor( 'w/b+,b+/w,w+/b,w/b+,w/b,w+/b' )
   @  0,  0, Maxrow(), Maxcol() BOX( Chr( 201 ) + Chr( 205 ) + Chr( 187 ) + Chr( 186 ) + Chr( 188 ) + Chr( 205 ) + Chr( 200 ) + Chr( 186 ) + Space( 1 ) )
   Attention( s_aLangMessages[ 27 ], 0 )

   @  1,  1 SAY "Select Os"
   @  1, 12 GET cOS radio { "Win32", "OS/2", "Linux" }
   @  1, 23 SAY "Select C Compiler"
   @  1, 40 GET cCompiler radio { "BCC", "MSVC", "GCC" }

   READ

   SET CURSOR ON

   @  4,  1 SAY "Library name with our extention" GET cFwhPath PICT "@s15"
   @  4, 40 SAY "Obj Files Dir"                   GET cObjDir  PICT "@s15"

   Attention( "xHarbour Options", 5 )

   @  6,  1 GET lautomemvar checkbox caption "Automatic memvar declaration"
   @  6, 40 GET lvarismemvar checkbox caption "Variables are assumed M->"
   @  7,  1 GET lDebug checkbox caption "Debug info"
   @  7, 40 GET lSupressline checkbox caption "Suppress line number information"
   @  8,  1 GET lGenppo checkbox caption "Generate pre-processed output"
   @  8, 40 GET lCompMod checkbox caption "compile module only"
   @  9,  1 SAY "User Defines " GET s_cUserDef PICT "@s15"
   @  9, 40 SAY "User include Path" GET s_cUserInclude PICT "@s15"
   @ 10,  1 GET lInstallLibrary checkbox caption "Install Library to xHarbour Lib Directory"
   READ

   IF ! Empty( s_cUserDef )
      aUserDefs := ListasArray2(Alltrim( s_cUserDef ), ";")

      FOR EACH cCurrentDef in aUserDefs
         cDefHarOpts += " -D" + Alltrim( cCurrentDef ) + " "
      NEXT
   ENDIF

   IF ! Empty( s_cUserInclude )
      cDefHarOpts += " -I" + Alltrim( s_cUserInclude ) + " "
   ENDIF

   s_lBcc    :=  "BCC"  IN cCompiler
   s_lMSVcc    :=  "MSVC" IN cCompiler
   s_lGcc    :=  "GCC"  IN cCompiler

   cObjDir := Alltrim( cObjDir )

   IF ! Empty( cObjDir )

      IF DirChange( cObjDir ) != 0
         MakeDir( cObjDir )
      ELSE
         DirChange( '..' )
      ENDIF

   ENDIF

   s_aMacros := GetSourceDirMacros( s_lGcc, cos )

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

   IF lAutomemvar
      cDefHarOpts += " -a "
   ENDIF

   IF lvarismemvar
      cDefHarOpts += " -v "
   ENDIF

   IF ldebug
      cDefHarOpts += " -b "
   ENDIF

   IF lSupressline
      cDefHarOpts += " -l "
   ENDIF

   IF lGenppo
      cDefHarOpts += " -p "
   ENDIF

   IF lCompmod
      cDefHarOpts += " -m "
   ENDIF

   IF s_lBcc
      AAdd( s_aCommands, { ".cpp.obj:", "$(BCB)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $**" } )
      AAdd( s_aCommands, { ".c.obj:", "$(BCB)\BIN\bcc32 -I$(BHC)\include $(CFLAG1) $(CFLAG2) -o$* $**" } )

      IF s_lExtended
         AAdd( s_aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -n -go -I$(BHC)\include $(HARBOURFLAGS) -I$(FWH)\include -o$* $**" } )
      ELSE
         AAdd( s_aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -I$(FWH)\include -o$* $**" } )
      ENDIF

      AAdd( s_aCommands, { ".rc.res:", "$(BCB)\BIN\brcc32 $(RFLAGS) $<" } )

   ELSEIF s_lGcc

      IF  "linux" IN Lower( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOs == "Linux"
         AAdd( s_aCommands, { ".cpp.o:", "gcc $(CFLAG1) $(CFLAG2) -o$* $**" } )
         AAdd( s_aCommands, { ".c.o:", "gcc -I/usr/include/xharbour $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )

         IF s_lExtended
            AAdd( s_aCommands, { ".prg.o:", "harbour -n $(HARBOURFLAGS) -I/usr/include/xharbour -I. -go  -o$* $**" } )
         ELSE
            AAdd( s_aCommands, { ".prg.c:", "harbour -n $(HARBOURFLAGS) -I/usr/include/xharbour -I.  -o$* $**" } )
         ENDIF

      ELSE
         AAdd( s_aCommands, { ".cpp.o:", "$(BCB)\bin\gcc $(CFLAG1) $(CFLAG2) -o$* $**" } )
         AAdd( s_aCommands, { ".c.o:", "$(BCB)\bin\gcc -I$(BHC)/include $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )

         IF s_lExtended
            AAdd( s_aCommands, { ".prg.o:", "$(BHC)\bin\harbour -n -go -I$(BHC)/include $(HARBOURFLAGS)  -o$* $**" } )
         ELSE
            AAdd( s_aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)/include $(HARBOURFLAGS)  -o$* $**" } )
         ENDIF

      ENDIF

   ELSEIF s_lMSVcc
      AAdd( s_aCommands, { ".cpp.obj:", "$(BCB)\bin\cl $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
      AAdd( s_aCommands, { ".c.obj:", "$(BCB)\bin\cl -I$(BHC)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )

      IF s_lExtended
         AAdd( s_aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -go -n -I$(BHC)\include $(HARBOURFLAGS) -I$(C4W)\include -o$* $**" } )
      ELSE
         AAdd( s_aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -I$(C4W)\include -o$* $**" } )
      ENDIF

      AAdd( s_aCommands, { ".rc.res:", "$(BCB)\BIN\rc $(RFLAGS) $<" } )

   ENDIF

   Attention( 'Spacebar to select, Enter to continue process', 22 )

   IF ! s_lRecurse
      aInFiles := GetSourceFiles( .F., s_lGcc, cOs )
      nLenaSrc := Len( aInFiles )
   ELSE
      aInFiles := GetSourceFiles(, s_lGcc, cOs )
      nLenaSrc := Len( aInFiles )
   ENDIF

   aOutFiles := aClone( aInFiles )
   pickarry( 10, 15, 19, 64, aInFiles, aOutFiles ,{}, .T.)
   nLenaOut := Len( aOutFiles )

   AEval( aOutFiles, { | x, y | aOutFiles[ y ] := Trim( Substr( aOutFiles[ y ], 1, At( ' ', aOutFiles[ y ] ) ) ) } )
   AEval( aOutFiles, { | xItem | IIF( At( '.c', xItem ) > 0 .OR. At( '.C', xItem ) > 0 .OR. At( '.cpp', xItem ) > 0 .OR. At( '.CPP', xItem ) > 0, AAdd( aOutc, xitem ), ) } )
   AEval( aOutc, { | x, z | citem := x, z := AScan( aOutFiles, { | t | t = citem } ), IIF( z > 0, aSize( aDel( aOutFiles, z ), Len( aOutFiles ) - 1 ), ) } )

   aOutFiles  := aSort( aOutFiles )
   s_aPrgs := aClone( aOutFiles )

   s_aObjs := aClone( aOutFiles )
   AEval( s_aObjs, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, s_aObjs[ x ] := cObjDir + cTest + "." + Exte( cExt, 2 ), s_aObjs[ x ] := cObjDir + cTest + "." + Exte( cExt, 3 ) ) } )
   s_aCs := aClone( aOutc )

   IF ! s_lExtended
      AEval( aOutc, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, AAdd( s_aObjs, cObjDir + cTest + "." + Exten( cExt, 2 ) ), AAdd( s_aObjs, cObjDir + cTest + "." + Exten( cExt, 1 ) ) ) } )
      AEval( aOutFiles, { | xItem | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cExt := Substr( cExt, 2 ), AAdd( s_aCs, cObjDir + cTest + "." + Exte( cExt, 1 ) ) } )
   ELSE
      s_aObjsc := aClone( aOutc )
      AEval( aOutc, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, s_aObjsc[ x ] := cObjDir + cTest + "." + Exten( cExt, 2 ), s_aObjsc[ x ] := cObjDir + cTest + "." + Exten( cExt, 1 ) ) } )
   ENDIF

   FOR x := 1 TO Len( s_aMacros )

      IF ! Empty( s_aMacros[ x, 2 ] )
         cItem := s_aMacros[ x, 2 ]
         nPos  := AScan( s_aPrgs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0
            AEval( s_aPrgs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aPrgs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )

            IF ! s_aMacros[ x, 3 ]
               FWrite( s_nLinkHandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
               s_aMacros[ x, 3 ] := .T.
            ENDIF

         ENDIF

         nPos := AScan( s_aCs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0
            AEval( s_aCs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aCs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )

            IF ! s_aMacros[ x, 3 ]
               FWrite( s_nLinkHandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
               s_aMacros[ x, 3 ] := .T.
            ENDIF

         ENDIF

         nPos := AScan( s_aObjs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0

            IF ! Empty( cObjDir )
               AEval( s_aObjs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aObjs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )
               FWrite( s_nLinkHandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
            ENDIF

         ENDIF

         IF s_lExtended
            nPos := AScan( s_aObjsc, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

            IF nPos > 0

               IF ! Empty( cObjDir )
                  AEval( s_aObjsc, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aObjsc[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )
               ENDIF

            ENDIF

         ENDIF

      ENDIF

   NEXT

   IF s_lGcc

      IF  "linux" IN Lower( Getenv( "HB_ARCHITECTURE" ) ) .OR. cOs == "Linux"
         FWrite( s_nLinkHandle, "PROJECT = " + IIF( lInstallLibrary, "$(BHC)/lib/", "" ) + Alltrim( cFwhPath ) + ".a " + CRLF )
      ELSE
         FWrite( s_nLinkHandle, "PROJECT = " + IIF( lInstallLibrary, "$(BHC)\lib\", "" ) + Alltrim( Lower( cFwhPath ) ) + ".a " + CRLF )
      ENDIF

   ELSE
      FWrite( s_nLinkHandle, "PROJECT = " + IIF( lInstallLibrary, "$(BHC)\lib\", "" ) + Alltrim( Lower( cFwhPath ) ) + ".lib " + CRLF )
   ENDIF

   IF ! s_lExtended
      nWriteFiles := 0
      FWrite( s_nLinkHandle, "OBJFILES =" )

      IF Len( s_aObjs ) < 1
         FWrite( s_nLinkHandle, + " $(OB) " + CRLF )
      ELSE
         nWriteFiles := 0
         AEval( s_aObjs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aObjs ), FWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), FWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )
      ENDIF

      nWriteFiles := 0
      FWrite( s_nLinkHandle, "CFILES =" )

      IF Len( s_aCs ) < 1
         FWrite( s_nLinkHandle, + " $(CF)" + CRLF )
      ELSE
         AEval( s_aCs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aCs ), FWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), FWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(CF) " + CRLF ) ) } )
      ENDIF

      FWrite( s_nLinkHandle, "PRGFILE =" )
      nWriteFiles := 0

      IF Len( s_aPrgs ) < 1
         FWrite( s_nLinkHandle, + " $(PS)" + CRLF )
      ELSE
         AEval( s_aPrgs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aPrgs ), FWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), FWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )
      ENDIF

   ELSE /****extended moded ****/
      FWrite( s_nLinkHandle, "OBJFILES =" )
      nWriteFiles := 0

      IF Len( s_aObjs ) < 1
         FWrite( s_nLinkHandle, + " $(OB) " + CRLF )
      ELSE
         AEval( s_aObjs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aObjs ), FWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), FWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )
      ENDIF

      FWrite( s_nLinkHandle, "PRGFILES =" )
      nWriteFiles := 0

      IF Len( s_aPrgs ) < 1
         FWrite( s_nLinkHandle, + " $(PS)" + CRLF )
      ELSE
         AEval( s_aPrgs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aPrgs ), FWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), FWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )
      ENDIF

      nWriteFiles := 0

      IF Len( s_aObjsc ) > 0
         FWrite( s_nLinkHandle, "OBJCFILES =" )

         IF Len( s_aObjsc ) < 1
            FWrite( s_nLinkHandle, + " $(OBC) " + CRLF )
         ELSE
            AEval( s_aObjsc, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aObjsc ), FWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), FWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(OBC) " + CRLF ) ) } )
         ENDIF

      ENDIF

      nWriteFiles := 0

      IF Len( s_aCs ) > 0
         FWrite( s_nLinkHandle, "CFILES =" )

         IF Len( s_aCs ) < 1
            FWrite( s_nLinkHandle, + " $(CF)" + CRLF )
         ELSE
            AEval( s_aCs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aCs ), FWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), FWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(CF) " + CRLF ) ) } )
         ENDIF

      ENDIF

   ENDIF

   FWrite( s_nLinkHandle, "RESFILES =" + CRLF )
   FWrite( s_nLinkHandle, "RESDEPEN = $(RESFILES)" + CRLF )
   FWrite( s_nLinkHandle, "DEFFILE = " + CRLF )
   FWrite( s_nLinkHandle, "HARBOURFLAGS = " + cDefHarOpts + CRLF )

   IF s_lBcc
      FWrite( s_nLinkHandle, "CFLAG1 =  -OS $(CFLAGS) -d -L$(BHC)\lib;$(FWH)\lib -c" + CRLF )
      FWrite( s_nLinkHandle, "CFLAG2 =  -I$(BHC)\include -I$(BCB)\include -I" + Alltrim( s_cUserInclude ) + CRLF )
      FWrite( s_nLinkHandle, "RFLAGS = " + CRLF )
      FWrite( s_nLinkHandle, "LFLAGS = /P32 /0" + CRLF )
      FWrite( s_nLinkHandle, "IFLAGS = " + CRLF )
      FWrite( s_nLinkHandle, "LINKER = tlib $(LFLAGS) $(PROJECT)" + CRLF )
      FWrite( s_nLinkHandle, " " + CRLF )
      FWrite( s_nLinkHandle, "ALLOBJ =  $(OBJFILES) $(OBJCFILES)" + CRLF )
      FWrite( s_nLinkHandle, "ALLRES = $(RESDEPEN)" + CRLF )
      FWrite( s_nLinkHandle, "ALLLIB = " + CRLF )
      FWrite( s_nLinkHandle, ".autodepend" + CRLF )
   ELSEIF s_lMSVcc
      FWrite( s_nLinkHandle, "CFLAG1 =  -I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(CFLAGS)" + CRLF )
      FWrite( s_nLinkHandle, "CFLAG2 =  -c -I" + Alltrim( s_cUserInclude ) + CRLF )
      FWrite( s_nLinkHandle, "RFLAGS = " + CRLF )
      FWrite( s_nLinkHandle, "LFLAGS = " + CRLF )
      FWrite( s_nLinkHandle, "IFLAGS = " + CRLF )
      FWrite( s_nLinkHandle, "LINKER = lib $(PROJECT)" + CRLF )
      FWrite( s_nLinkHandle, " " + CRLF )
      FWrite( s_nLinkHandle, "ALLOBJ = $(OBJFILES) $(OBJCFILES) " + CRLF )
      FWrite( s_nLinkHandle, "ALLRES = $(RESDEPEN)" + CRLF )
      FWrite( s_nLinkHandle, "ALLLIB = " + CRLF )
   ELSEIF s_lGcc
      FWrite( s_nLinkHandle, "CFLAG1 = " + IIF( "linux" IN Lower( Os() ) , "-I/usr/include/xharbour", " -I$(BHC)/include" ) + " -c -Wall" + CRLF )
      FWrite( s_nLinkHandle, "CFLAG2 = " + IIF( "linux" IN Lower( Os() ) , "-L /usr/lib/xharbour", " -L $(BHC)/lib" ) + CRLF )
      FWrite( s_nLinkHandle, "RFLAGS = " + CRLF )
      FWrite( s_nLinkHandle, "LFLAGS = " + CRLF )
      FWrite( s_nLinkHandle, "IFLAGS = " + CRLF )

      IF "linux" IN Lower( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOs == "Linux" .OR.  "linux" IN Lower( Os() )
         FWrite( s_nLinkHandle, "LINKER = ar -M " + CRLF )
      ELSE
         FWrite( s_nLinkHandle, "LINKER = ar -M " + CRLF )
      ENDIF

      FWrite( s_nLinkHandle, " " + CRLF )
      FWrite( s_nLinkHandle, "ALLOBJ = $(OBJFILES) $(OBJCFILES) " + CRLF )
      FWrite( s_nLinkHandle, "ALLRES = $(RESDEPEN) " + CRLF )
      FWrite( s_nLinkHandle, "ALLLIB = $(LIBFILES) " + CRLF )
      FWrite( s_nLinkHandle, ".autodepend" + CRLF )
   ENDIF

   FWrite( s_nLinkHandle, " " + CRLF )
   FWrite( s_nLinkHandle, "#COMMANDS" + CRLF )
   AEval( s_aCommands, { | xItem | FWrite( s_nLinkHandle, xitem[ 1 ] + CRLF ), FWrite( s_nLinkHandle, xitem[ 2 ] + CRLF ), FWrite( s_nLinkHandle, " " + CRLF ) } )

   IF s_lBcc
      FWrite( s_nLinkHandle, "#BUILD" + CRLF )
      FWrite( s_nLinkHandle, " " + CRLF )
      FWrite( s_nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES)" + CRLF )
      FWrite( s_nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!" + CRLF )
      FWrite( s_nLinkHandle, "    $(ALLOBJ)" + CRLF )
      FWrite( s_nLinkHandle, "!" + CRLF )
   ELSEIF s_lMSVcc
      FWrite( s_nLinkHandle, "#BUILD" + CRLF )
      FWrite( s_nLinkHandle, "" + CRLF )
      FWrite( s_nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES)" + CRLF )
      FWrite( s_nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!" + CRLF )
      FWrite( s_nLinkHandle, "    $(ALLOBJ) " + CRLF )
      FWrite( s_nLinkHandle, "!" + CRLF )
   ELSEIF s_lGcc
      FWrite( s_nLinkHandle, "#BUILD" + CRLF )
      FWrite( s_nLinkHandle, " " + CRLF )
      FWrite( s_nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) " + CRLF )

      IF  "linux" IN Lower( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOs == "Linux"
         FWrite( s_nLinkHandle, "    $(LINKER) @&&!" + CRLF )
      ELSE
         FWrite( s_nLinkHandle, "    $(BCB)\bin\$(LINKER) @&&!" + CRLF )
      ENDIF

      FWrite( s_nLinkHandle, "    $(PROJECT) " + CRLF )
      FWrite( s_nLinkHandle, "    $(ALLOBJ)  " + CRLF )
      FWrite( s_nLinkHandle, "!" + CRLF )
   ENDIF

RETURN NIL

*---------------------
FUNCTION SetLibBuild()
*---------------------

   LOCAL cRead as String
   LOCAL nPos as Numeric
   LOCAL aMacro as Array
   LOCAL aTemp as Array
   LOCAL nCount as Numeric
   LOCAL aCurobjs as Array
   LOCAL nObjPos as Numeric
   LOCAL cLib

   cRead       := Alltrim( readln( @s_lEof ) )
   s_nLinkHandle := FCreate( s_cLinker )
   s_szProject   := cRead
   aMacro      := ListAsArray2( cRead, ":" )

   IF Len( aMacro ) > 1
      aTemp := ListAsArray2( aMacro[ 2 ], " " )
      AEval( aTemp, { | xItem | AAdd( s_aBuildOrder, xItem ) } )
   ENDIF

   AAdd( s_aBuildOrder, aMacro[ 1 ] )
   cRead  := Strtran( cRead, "@&&!", "" )
   aMacro := ListAsArray2( cRead, '\' )
   AEval( aMacro, { | xMacro | Findmacro( xMacro, @cRead ) } )

   IF s_lBcc .OR. s_lMSVcc
      s_cLinkComm := cRead + "  @" + s_cLinker
   ELSE
      s_cLinkComm := cRead + " < " + s_cLinker
   ENDIF

   FOR nPos := 1 TO 7
      cRead  := Alltrim( readln( @s_lEof ) )
      aMacro := ListAsArray2( cRead, " " )

      FOR ncount := 1 TO Len( aMacro )

         IF  "$" IN aMacro[ nCount ]

            IF ( aMacro[ nCount ] = "$(PROJECT)" ) .AND. s_lGcc
               Findmacro( aMacro[ nCount ], @cRead )
               FWrite( s_nLinkHandle, "CREATE " + " lib" + cRead + CRLF )
               cLib := "lib" + cRead
            ELSEIF ( aMacro[ nCount ] == "$(ALLOBJ)" )
               Findmacro( aMacro[ nCount ], @cRead )
               aCurObjs := ListAsArray2( cRead, " " )

               FOR nObjPos := 1 TO Len( aCurObjs )

                  IF s_lGcc
                     FWrite( s_nLinkHandle, "ADDMOD " + aCurObjs[ nObjPos ] + CRLF )
                  ENDIF

                  IF s_lBcc .OR. s_lMSVcc

                     IF nObjPos < Len( aCurObjs )
                        FWrite( s_nLinkHandle, "+-" + aCurObjs[ nObjPos ] + " &" + CRLF )
                     ELSE
                        FWrite( s_nLinkHandle, "+-" + aCurObjs[ nObjPos ] + CRLF )
                     ENDIF

                  ENDIF

               NEXT

            ENDIF

         ENDIF

      NEXT

   NEXT

   IF s_lGcc
      FWrite( s_nLinkHandle, "SAVE" + CRLF )
      FWrite( s_nLinkHandle, "END " + CRLF )
   ENDIF

   FClose( s_nLinkHandle )

   IF s_lLinux
      s_cLinkComm += " || rm -f " + cLib
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

*----------------------
FUNCTION Hb_OsNewLine()
*----------------------
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
FUNCTION BuildBorCfgFile()
*-------------------------

   LOCAL nCfg

//   IF ! File( GetMakeDir() + '\bin\harbour.cfg' )
   IF ! File( GetHarbourDir() + '\bin\harbour.cfg' )
//      nCfg := FCreate( GetMakeDir() + '\bin\harbour.cfg' )
      nCfg := FCreate( GetHarbourDir() + '\bin\harbour.cfg' )
      FWrite( nCfg, "CC=BCC32" + CRLF )
      FWrite( nCfg, "CFLAGS= -c " + Replacemacros( "-I$(BHC)\include -OS $(CFLAGS) -d -L$(BHC)\lib" ) + CRLF )
      FWrite( nCfg, "VERBOSE=NO" + CRLF )
      FWrite( nCfg, "DELTMP=YES" + CRLF )
      FClose( nCfg )
   ENDIF

RETURN NIL

*-------------------------
FUNCTION Buildmsccfgfile()
*-------------------------

   LOCAL nCfg

//   IF ! File( GetMakeDir() + '\bin\harbour.cfg' )
   IF ! File( GetHarbourDir() + '\bin\harbour.cfg' )
//      nCfg := FCreate( GetMakeDir() + '\bin\harbour.cfg' )
      nCfg := FCreate( GetHarbourDir() + '\bin\harbour.cfg' )
      FWrite( nCfg, "CC=cl" + CRLF )
      FWrite( nCfg, "CFLAGS= -c " + Replacemacros( "-I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(CFLAGS)" ) + CRLF )
      FWrite( nCfg, "VERBOSE=NO" + CRLF )
      FWrite( nCfg, "DELTMP=YES" + CRLF )
      FClose( nCfg )
   ENDIF

RETURN NIL

*-------------------------
FUNCTION Buildgcccfgfile()
*-------------------------

   LOCAL nCfg
//   LOCAL cDir := GetMakeDir()
   LOCAL cDir := GetHarbourDir()
   LOCAL cBhc := Alltrim( Strtran( replacemacros( '$(BHC)' ), '\', '/' ) )

   cDir := Strtran( cDir, '/', '\' )

   IF ! File( cdir + '\bin\harbour.cfg' )
      nCfg := FCreate( cdir + '\bin\harbour.cfg' )
      FWrite( nCfg, "CC=gcc" + CRLF )
      FWrite( nCfg, "CFLAGS= -c " + Replacemacros( "-I" + cBhc + "/include $(C_USR)  -L" + cBhc + "/lib" ) + CRLF )
      FWrite( nCfg, "VERBOSE=NO" + CRLF )
      FWrite( nCfg, "DELTMP=YES" + CRLF )
      FClose( nCfg )
   ENDIF

RETURN NIL

*--------------------------
FUNCTION BuildGccCfgFileL()
*--------------------------

   LOCAL nCfg

   IF ! File( '/etc/harbour.cfg' )
      nCfg := FCreate( '/etc/harbour.cfg' )
      FWrite( nCfg, "CC=gcc" + CRLF )
      FWrite( nCfg, "CFLAGS= -c -I/usr/include/xharbour" + CRLF )
      FWrite( nCfg, "VERBOSE=YES" + CRLF )
      FWrite( nCfg, "DELTMP=YES" + CRLF )
      FClose( nCfg )
   ENDIF

RETURN NIL

*------------------------------
FUNCTION Findharbourcfg( cCfg )
*------------------------------

   LOCAL cPath AS STRING := ''
   LOCAL lFound AS LOGICAL := .F.
   LOCAL cEnv AS STRING
   LOCAL aEnv as Array of String
   LOCAL lLinux :=  'linux' IN Lower( Os() )
   LOCAL nPos

   IF ! lLinux .OR. s_lOs2
      cEnv := Gete( "PATH" ) + ";" + Curdir()
      aEnv := ListAsArray2( cEnv, ";" )

      FOR nPos := 1 TO Len( aEnv )

         IF File( aEnv[ nPos ] + '\harbour.cfg' )
            cPath  := aEnv[ nPos ]
            lFound := .T.
            EXIT
         ENDIF

      NEXT

   ELSE

      IF File( '/etc/harbour.cfg' )
         lFound := .T.
         cPath  := '/etc/harbour.cfg'
      ENDIF

      IF ! lfound

         IF File( '/usr/local/etc/harbour.cfg' )
            lFound := .T.
            cPath  := '/usr/local/etc/harbour.cfg'
         ENDIF

      ENDIF

   ENDIF

   cCfg := cPath

RETURN lFound

*---------------------------
FUNCTION TestforPrg( cFile )
*---------------------------

   LOCAL aFiles AS ARRAY := {}
   LOCAL cPath AS STRING := ''
   LOCAL cTest AS STRING := ""
   LOCAL cDrive AS STRING := ""
   LOCAL cExt AS STRING := ""
   LOCAL cItem AS STRING := ""
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

      IF Left( cFile, 1 ) $ "- /?"
         cParam += cFile+" "
      ELSE
         cFile := cFile
         AAdd( aFile, cFile )
      ENDIF

   ENDIF

   IF ! Empty( p1 )

      IF Left( p1, 1 ) $ "- /?"
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
   cParam := Strtran( cParam, "-f", "-F" )
   cParam := Strtran( cParam, "-r", "-R" )
   cParam := Strtran( cParam, "-l", "-L" )

  IF  "-EX" IN cParam   .OR.  "-ELX" IN cParam

      IF  "-ELX" IN cParam
         s_lLibrary := .T.
      ENDIF
      s_lEditMode := .T.
   ENDIF

   IF  "-L" IN cParam
      s_cDefLang := Substr( cParam, At( "-L", cParam ) + 2, 2 )
   ENDIF

RETURN cParam

*------------------
FUNCTION ShowHelp()
*------------------

   LOCAL cOs := Upper( Os() )
   cls
   ? s_aLangMessages[ 1 ]
   ? "Copyright 2000-2004 Luiz Rafael Culik <culikr@uol.com.br>"
//   ? ""
   ? s_aLangMessages[ 2 ]
//   ? ""
   ? s_aLangMessages[ 3 ]
   ? s_aLangMessages[ 4 ]
   ? s_aLangMessages[ 5 ]
   ? s_aLangMessages[ 6 ]
   ? s_aLangMessages[ 7 ]
   ? s_aLangMessages[ 8 ]

   IF  "OS/2" IN cOs
      ? s_aLangMessages[ 9 ]
      ? s_aLangMessages[ 10 ]
      ? s_aLangMessages[ 13 ]
   ELSEIF  'LINUX' IN  cOs
      ? s_aLangMessages[ 9 ]
      ? s_aLangMessages[ 12 ]
      ? s_aLangMessages[ 14 ]
   ELSE
      ? s_aLangMessages[ 11 ]
      ? s_aLangMessages[ 12 ]
      ? s_aLangMessages[ 13 ]
   ENDIF

   ? s_aLangMessages[ 15 ]
   ? s_aLangMessages[ 16 ]
   ? s_aLangMessages[ 17 ]
   ? s_aLangMessages[ 18 ]
   ? s_aLangMessages[ 19 ]
   ? s_aLangMessages[ 20 ]
   ? s_aLangMessages[ 21 ]
   ? s_aLangMessages[ 22 ]
   ? s_aLangMessages[ 23 ]
   ? s_aLangMessages[ 24 ]
   ? s_aLangMessages[ 25 ]
   ? s_aLangMessages[ 26 ]

RETURN NIL

*------------------------------------
FUNCTION ProcessParameters( cParams )
*------------------------------------

   LOCAL aDef
   

   IF  "-F" IN cParams
      s_lForce  := .T.
      cParams := Strtran( cParams, "-F", "" )
   ENDIF

   IF  "-R" IN cParams
      s_lRecurse := .T.
      
      cParams  := Strtran( cParams, "-R", "" )
   ENDIF


   IF  "-B" IN cParams
      s_lBcc    := .T.
      s_lGcc    := .F.
      s_lMSVcc    := .F.
      cParams := Strtran( cParams, "-B", "" )
   ENDIF

   IF  "-GL" IN cParams
      s_lBcc    := .F.
      s_lGcc    := .T.
      s_lMSVcc  := .F.
      s_lLinux  := .T.
      cParams := Strtran( cParams, "-GL", "" )
   ENDIF

   IF "-G" IN cParams
      s_lBcc    := .F.
      s_lGcc    := .T.
      s_lMSVcc    := .F.
      cParams := Strtran( cParams, "-G", "" )
   ENDIF

   IF "-V" IN cParams
      s_lBcc    := .F.
      s_lGcc    := .F.
      s_lMSVcc    := .T.
      cParams := Strtran( cParams, "-V", "" )
   ENDIF

   IF  "-I" IN cParams
      s_lIgnoreErrors := .T.
      cParams       := Strtran( cParams, "-I", "" )
   ENDIF


   IF  "-P" IN cParams
      s_lPrint  := .T.
      cParams := Strtran( cParams, "-P", "" )
   ENDIF

   IF  "-D" IN cParams
      cParams := "-D" + Strtran( cParams, "-D", ";" )
      cParams := Strtran( cParams, "-D;", "-D" )

      aDef := ListAsArray2( Alltrim( Substr( cParams, 3 ) ), ";" )
      AEval( aDef, { | xDef | IIF( At( '=', xDef ) > 0, GetParaDefines( xDef ), ) } )
   ENDIF

   IF  "-EL" IN cParams  .OR.  "-ELX" IN cParams

      IF At( "-ELX", cParams ) > 0
         cParams := Strtran( cParams, "-ELX", "" )
      ELSE
         cParams := Strtran( cParams, "-EL", "" )
      ENDIF

      s_lExtended := .T.
      s_lLibrary  := .T.
      s_lEditMode := .T.

   ENDIF

   IF  "-E" IN cParams  .OR.  "-EX" IN cParams

      IF  "-EX" IN cParams
         cParams := Strtran( cParams, "-EX", "" )
      ELSE
         cParams := Strtran( cParams, "-E", "" )
      ENDIF

      s_lExtended := .T.
      s_lEditMode := .T.

   ENDIF

RETURN NIL

*-----------------------------
FUNCTION WriteMakeFileHeader()
*-----------------------------

   FWrite( s_nLinkHandle, "#BCC" + CRLF )
   FWrite( s_nLinkHandle, "VERSION=BCB.01" + CRLF )
   FWrite( s_nLinkHandle, "!ifndef BCB" + CRLF )
   FWrite( s_nLinkHandle, "BCB = $(MAKEDIR)" + CRLF )
   FWrite( s_nLinkHandle, "!endif" + CRLF )
   FWrite( s_nLinkHandle, CRLF )
   FWrite( s_nLinkHandle, "!ifndef BHC" + CRLF )
   FWrite( s_nLinkHandle, "BHC = $(HMAKEDIR)" + CRLF )
   FWrite( s_nLinkHandle, "!endif" + CRLF )
   FWrite( s_nLinkHandle, " " + CRLF )
   FWrite( s_nLinkHandle, "RECURSE=" + IIF( s_lRecurse, " YES ", " NO " ) + CRLF )
   FWrite( s_nLinkHandle, " " + CRLF )

RETURN NIL

*-------------------------------
FUNCTION BuildLangArray( cLang )
*-------------------------------

   LOCAL aLang := {}

   DEFAULT cLang TO "EN"

   IF cLang == "EN"

      AAdd( aLang, "Harbour Make Utility - the xHarbour make programm - version "+HbMakeVersion() )
      AAdd( alang, "Syntax:  hbmake <cFile>.bc [options] - Example: hbmake hello.bc /ex")
      AAdd( aLang, "Options:  /e[x]   Create a new Makefile. If /ex is used it create a" )
      AAdd( aLang, "                  new make file in extended mode." )
      AAdd( aLang, "          /el[x]  Create a new Makefile. If /elx is used it create a")
      AAdd( aLang, "                  new make file to build a LIB in extended mode." )
      AAdd( aLang, "          /D      Define a macro." )
      AAdd( aLang, "          /p      Print all commands and depedencies." )
      AAdd( aLang, "          /b      Use BCC as C compiler" )
      AAdd( aLang, "          /g+     Use GCC as C compiler" )
      AAdd( aLang, "          /b+     Use BCC as C compiler" )
      AAdd( aLang, "          /g      Use GCC as C compiler" )
      AAdd( aLang, "          /gl     Use GCC as C compiler in Linux" )
      AAdd( aLang, "          /gl+    Use GCC as C compiler in Linux" )
      AAdd( aLang, "          /v      Use MSVC as C compiler" )
      AAdd( aLang, "          /f      Force recompiltion of all files" )
      AAdd( aLang, "          /i      Ignore errors returned by command" )
      AAdd( aLang, "          /r      Recurse Source Directory" )
      AAdd( aLang, "                  Note: /p and /D can be used together" )
      AAdd( aLang, "                        /r and /e[x]/el[x] can be used together")
      AAdd( aLang, "                  Options with + are the default values" )
      AAdd( aLang, "                  -D switch can accept multiple macros on the same line")
      AAdd( aLang, "                  or use one macro per -D switch" )
      AAdd( aLang, "                  /l[LANGID] Specify the language to be used on hbmake")
      AAdd( aLang, "                  LANGID= (EN/PT/ES). On Windows, the default will be the S.O.")
      AAdd( aLang, "                  language. On OS/2, FreeBSD and LINUX will be English." )
      AAdd( aLang, "Enviroment options" )
      AAdd( aLang, "Select Os" )
      AAdd( aLang, "Select C Compiler" )
      AAdd( aLang, "Graphic Lib" )
      AAdd( aLang, "xHarbour Options" )
      AAdd( aLang, "Automatic memvar declaration /a" )
      AAdd( aLang, "Variables are assumed M-> /v" )
      AAdd( aLang, "Debug info /b" )
      AAdd( aLang, "Suppress line number information /l" )
      AAdd( aLang, "Generate pre-processed output /p" )
      AAdd( aLang, "compile module only /m" )
      AAdd( aLang, "User Defines " )
      AAdd( aLang, "User include Path" )
      AAdd( aLang, "Use External Libs" )
      AAdd( aLang, "<Spacebar>-Select <Enter>-Continue process <F5> Sel/Unsel All" )
      AAdd( aLang, "Warning level /w" )
      AAdd( aLang, "Numbers of source files per line on makefile" )
      AAdd( aLang, "Use Multi Thread Library" )
      AAdd( aLang, "Executable file name" )
      AAdd( aLang, "Warning Level /w" )
      AAdd( aLang, "<Tab>-Next <Sh-Tab>-Prev <Enter>-Sel <"+chr(24)+chr(25)+">-Change Sel <Spacebar>-Open Box")
      /* Messages Start Here */
      AAdd( aLang, "3rd Party Rdd")
      AAdd( aLang, "What OS you Use")
      AAdd( aLang, "What C compiler  you has")
      AAdd( aLang, "This app use Graphical libraries")
      AAdd( aLang, "Do you use 3rd Party Rdd")
      AAdd( aLang, "Compress this app")
      AAdd( aLang, "Compress the app after Linked(use upx)")
      AAdd( aLang, "Your app will be linked to user harbour.dll")
      AAdd( aLang, "Where the .obj/.o files will be generates")
      AAdd( aLang, "Inform executable name (without .exe extention)" )

   ELSEIF cLang == "ES"

      AAdd( aLang, "Harbour Make Utility  -  Programa Make de xHarbour - version "+HbMakeVersion() )
      AAdd( aLang, "Sintaxe:  hbmake <cArchivo>.bc [opciones] - Exemplo: hbmake hello.bc /ex")
      AAdd( aLang, "Opciones: /e[x]   Crea un Makefile nuevo. Si se usa /ex se crea un nuevo" )
      AAdd( aLang, "                  makefile en modo extendido." )
      AAdd( aLang, "          /el[x]  Crea un Makefile nuevo. Si se usa /elx se crea un nuevo" )
      AAdd( aLang, "                  makefile para construir una LIB en modo extendido." )
      AAdd( aLang, "          /D      Define una macro." )
      AAdd( aLang, "          /p      Imprime todos los comandos y dependencias." )
      AAdd( aLang, "          /b      Usar BCC como compilador C" )
      AAdd( aLang, "          /g+     Usar GCC como compilador C" )
      AAdd( aLang, "          /b+     Usar BCC como compilador C" )
      AAdd( aLang, "          /g      Usar GCC como compilador C" )
      AAdd( aLang, "          /gl     Usar GCC como compilador C en Linux" )
      AAdd( aLang, "          /gl+    Usar GCC como compilador C en Linux" )
      AAdd( aLang, "          /v      Usar MSVC como compilador C" )
      AAdd( aLang, "          /f      Forzar la recompilaciÛn de todos los archivos" )
      AAdd( aLang, "          /i      Ignorar los errores devueltos por el comando" )
      AAdd( aLang, "          /r      Recorrer el directorio fuente recursivamente" )
      AAdd( aLang, "                  Nota: /p y /D pueden ser usados juntos" )
      AAdd( aLang, "                        /r y /e[x]/el[x] pueden ser usados juntos" )
      AAdd( aLang, "                  Las opciones con + son los valores por omisiÛn" )
      AAdd( aLang, "                  El par·metro -D puede aceptar m˙ltiples macros en la misma" )
      AAdd( aLang, "                  lÌnea ou use uma macro por parÉmetro -D" )
      AAdd( aLang, "                  /l[LANGID] especifica una linguagem a ser utilizada por")
      AAdd( aLang, "                   hbmake. LANGID = (EN/PT/ES). En sistemas Windows, O padr∆o")
      AAdd( aLang, "                   es a linguagem do SO. Em OS/2, FreeBSD i LINUX ser† Ingles." )
      AAdd( aLang, "Opciones de Ambiente" )
      AAdd( aLang, "Selecion Os" )
      AAdd( aLang, "Selecion Compilador C" )
      AAdd( aLang, "Lib Grafica" )
      AAdd( aLang, "Opciones do xHarbour" )
      AAdd( aLang, "Declaracion Automatica de memvar /a" )
      AAdd( aLang, "Variables ser†n assumidas M-> /v " )
      AAdd( aLang, "Info. Debug /b" )
      AAdd( aLang, "Suprime info de numero da linha /l" )
      AAdd( aLang, "Gera salida pre-processada /p" )
      AAdd( aLang, "Compila solamente o modulo /m" )
      AAdd( aLang, "Define de usu†rios:" )
      AAdd( aLang, "Path p/ includes de usu†rio:" )
      AAdd( aLang, "Usar Libs Externas" )
      AAdd( aLang, "<Espacio>-Seleccionar <Enter>-Continuar proceso <F5> Selec/Deselec todo." )
      AAdd( aLang, "N°vel de aviso do compilador /w" )
      AAdd( aLang, "Qtd de PRGs por linea, no makefile:" )
      AAdd( aLang, "Use a libreria Multi Thread" )
      AAdd( aLang, "Nome Executable" )
      AAdd( aLang, "Nivel Warning /w" )
      AAdd( aLang, "<Tab>-Avanzar <Sh-Tab>-Volver <Enter>-Selec <"+chr(24)+chr(25)+">-Mudar Selec <Espacio>-Caja")
      /* Messages Start Here */
      AAdd( aLang, "Rdd Terceros")
      AAdd( aLang, "Qual OS usted usa")
      AAdd( aLang, "Qual compilador C usted ten")
      AAdd( aLang, "Esta App usa Lib Grafica o No")
      AAdd( aLang, "Usted usa Rdd de terceros")
      AAdd( aLang, "Comprimir app")
      AAdd( aLang, "Comprimir la app despois de enlazada (usar upx)")
      AAdd( aLang, "Su aplicacione sera linkada para usar la harbour.dll")
      AAdd( aLang, "Donde los ficheros *.obj ser†n generados")
      AAdd( aLang, "Informe o nombre de lo executable (sin a extension .exe)")

   ELSEIF cLang == "PT"

      AAdd( aLang, "Harbour Make Utility  -  Programa Make do xHarbour - vers∆o "+HbMakeVersion() )
      AAdd( aLang, "Sintaxe:  hbmake <arquivo>.bc [opá‰es] -  Exemplo: hbmake hello.bc /ex")
      AAdd( aLang, "Opá‰es:  /e[x]  Cria um Makefile novo. Se for usado /ex cria um makefile" )
      AAdd( aLang, "                em modo extendido." )
      AAdd( aLang, "         /el[x] Cria um Makefile novo. Se for usado /elx cria um makefile" )
      AAdd( aLang, "                para construir uma LIB, em modo extendido." )
      AAdd( aLang, "         /D     Define uma macro." )
      AAdd( aLang, "         /p     Imprime todos os comandos e dependàncias." )
      AAdd( aLang, "         /b     Usar BCC como compilador C" )
      AAdd( aLang, "         /g+    Usar GCC como compilador C" )
      AAdd( aLang, "         /b+    Usar BCC como compilador C" )
      AAdd( aLang, "         /g     Usar GCC como compilador C" )
      AAdd( aLang, "         /gl    Usar GCC como compilador C no Linux" )
      AAdd( aLang, "         /gl+   Usar GCC como compilador C no Linux" )
      AAdd( aLang, "         /v     Usar MSVC como compilador C" )
      AAdd( aLang, "         /f     Foráar a recompilaá∆o de todos os arquivos." )
      AAdd( aLang, "         /i     Ignora os erros devolvidos pelo comando." )
      AAdd( aLang, "         /r     Recorrer o diret¢rio fonte recursivamente." )
      AAdd( aLang, "                Nota:  /p e /D podem ser usados juntos" )
      AAdd( aLang, "                       /r e /e[x]/el[x] podem ser usados juntos" )
      AAdd( aLang, "                As opá‰es com + s∆o os valores padr∆o." )
      AAdd( aLang, "                O parÉmetro -D pode aceitar m£ltiplas macros na mesma linha")
      AAdd( aLang, "                ou use uma macro por parÉmetro -D" )
      AAdd( aLang, "                /l[LANGID] especifica a linguagem a ser utilizada pelo hbmake,")
      AAdd( aLang, "                LANGID = (EN/PT/ES). Em Windows, o padr∆o ser† a linguagem")
      AAdd( aLang, "                definida no S.O. Em OS/2, FreeBSD e LINUX o padr∆o ser† Inglàs.")
      AAdd( aLang, "Opá‰es de Ambiente" )
      AAdd( aLang, "Seleá∆o OS" )
      AAdd( aLang, "Seleá∆o Compilador C" )
      AAdd( aLang, "Lib Gr†fica" )
      AAdd( aLang, "Opá‰es do xHarbour" )
      AAdd( aLang, "Declaraá∆o Autom†tica de Memvar /a" )
      AAdd( aLang, "Vari†veis s∆o assumidas M-> /v" )
      AAdd( aLang, "Info. Debug /b" )
      AAdd( aLang, "Suprime info de n£mero da linha /l" )
      AAdd( aLang, "Gera sa°da prÇ-processada /p" )
      AAdd( aLang, "Compila apenas o m¢dulo /m" )
      AAdd( aLang, "User Defines:" )
      AAdd( aLang, "User Include Path:" )
      AAdd( aLang, "Usa Libs Externas ?" )
      AAdd( aLang, "<Espaáo>-Seleciona <Enter> p/ continuar processo <F5>-Sel/DeSel. tudo." )
      AAdd( aLang, "N°vel de aviso do compilador /w" )
      AAdd( aLang, "Qtd de PRGs por linha, no makefile: " )
      AAdd( aLang, "Usar a biblioteca Multi Thread ?" )
      AAdd( aLang, "Nome Execut†vel:" )
      AAdd( aLang, "N°vel Warning /w" )
      AAdd( aLang, "<Tab>-Avanáa <Sh-Tab>-Volta <Enter>-Sel. <"+chr(24)+chr(25)+">-Muda Sel. <Espc>-Abre Box")
      /* Messages Start Here */
      AAdd( aLang, "Rdd Terceiros")
      AAdd( aLang, "Selecione o Sistema Operacional")
      AAdd( aLang, "Selecione o compilador C/C++")
      AAdd( aLang, "Esta aplicaá∆o vai usar Lib Grafica ? Qual ?")
      AAdd( aLang, "Esta aplicaá∆o vai usar Rdd de terceiros ? Qual ?")
      AAdd( aLang, "Comprimir App ?")
      AAdd( aLang, "Comprimir a aplicaá∆o ap¢s linkada (usar UPX) ?")
      AAdd( aLang, "Sua aplicaá∆o ser† linkada para usar a harbour.dll ?")
      AAdd( aLang, "Informe a pasta onde os arquivos *.obj ser∆o gerados")
      AAdd( aLang, "Informe o nome do execut†vel (sem a extens∆o .exe)")
   ENDIF

RETURN aLang

*------------------------------------------
FUNCTION GetSelFiles( aInFiles, aOutFiles )
*------------------------------------------

   LOCAL aRet  := {}
   LOCAL cItem
   LOCAL nPos

   FOR EACH cItem IN aInFiles

      nPos := AScan( aOutFiles, { | x, y | x == Left( cItem, At( ' ', citem ) - 1 ) } )

      IF nPos > 0
         AAdd( aRet, cItem )
      ENDIF

   NEXT

RETURN aRet

*---------------------------
FUNCTION ResetInternalVars()
*---------------------------

   s_lPrint        := .F.
   s_aDefines      := {}
   s_aBuildOrder   := {}
   s_aCommands     := {}
   s_aMacros       := {}
   s_aPrgs         := {}
   s_aExtLibs      := {}
   s_aCs           := {}
   s_aObjs         := {}
   s_aObjsc        := {}
   s_lEof          := .F.
   s_aRes          := {}
   s_cLinker       := "makefile.lnk"
   s_cLinkComm     := ''
   s_lBcc          := .T.
   s_lGcc          := .F.
   s_lMSVcc        := .F.
   s_lForce        := .F.
   s_lLinux        := .F.
   s_szProject     := ""
   s_lLibrary      := .F.
   s_lIgnoreErrors := .F.
   s_lExtended     := .T.
   s_lOs2          := .F.
   s_lRecurse      := .F.
   s_lEditMode     := .F.
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
//   keyboard chr(9)
   ENDIF

RETURN .T.

*--------------------------
FUNCTION CheckCompiler(cOs)
*--------------------------
RETURN ( ("Win32" IN cOs) .or. ("Linux" In cOs) )

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

*-----------------------
FUNCTION HBMakeVersion()
*-----------------------
RETURN (s_cHbMakeVersion)
