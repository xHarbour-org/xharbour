/*
 * $Id: hbmake.prg,v 1.123 2004/06/17 20:17:24 modalsist Exp $
 */
/*
 * Harbour Project source code:
 * hbmake.Prg Harbour make utility main file
 *
 * Copyright 2000,2001,2002,2003,2004 Luiz Rafael Culik <culikr@uol.com.br>
 * www - http://www.harbour-project.org
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
/*
      Beginning Static Variables Table

      Default Values for core variables are set here
      New Core vars should only be added on this section
      */

STATIC s_lPrint        := .F.
STATIC s_nHandle
STATIC s_aDefines      := {}
STATIC s_aBuildOrder   := {}
STATIC s_aCommands     := {}
STATIC s_aMacros       := {}
STATIC s_aPrgs         := {}
STATIC s_aCs           := {}
STATIC s_aObjs         := {}
STATIC s_aObjsc        := {}
STATIC s_lEof          := .F.
STATIC s_aRes          := {}
STATIC s_nLinkHandle
STATIC s_cLinker       := "makefile.lnk"
STATIC s_cLinkComm     := ''
STATIC s_lBcc          := .T.
STATIC s_lGcc          := .F.
STATIC s_lVcc          := .F.
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
STATIC s_nLang         := 0
STATIC s_lMt           := .F.
STATIC s_cUserDef      := "                                        "   
STATIC s_cUserInclude  := "                                        "   
STATIC s_lxFwh         := .F.
STATIC s_nFilesToAdd   := 5
STATIC s_nWarningLevel := 0
STATIC s_AppName       := ""
Static s_lasDll        := .F.
FUNCTION MAIN( cFile, p1, p2, p3, p4, p5, p6 )

   LOCAL nPos
   LOCAL aFile    := {}
   LOCAL aDef     := {}
   LOCAL cOs      := Os()
   LOCAL AllParam
   LOCAL nLang    := GETUSERLANG()

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
   s_cDefLang := IIF( nLang == 1, "PT", IIF( nLang == 2, "EN", "ES" ) )
   s_aLangMessages := BuildLangArray( s_cDefLang )

   IF Pcount() == 0  .or. "?" $ AllParam
      ShowHelp()
      RETURN NIL
   ENDIF

   IF Upper( Os() ) == "WINDOWS XP"
      s_cLinker := "makefile.tmp"
   ENDIF

   SET DATE Ansi
   SET SCORE Off
   SET CENTURY ON
   set trace on

   DEFAULT p1 TO ""
   DEFAULT p2 TO ""
   DEFAULT p3 TO ""
   DEFAULT p4 TO ""
   DEFAULT p5 TO ""
   DEFAULT p6 TO ""

   /* Assing Default C Compiler upon The OS */

   IF "OS/2" IN  cOs
      s_lGcc   := .t.
      s_lLinux := .f.
      s_lBcc   := .f.
   ENDIF

   IF "LINUX" IN Upper( cOs )
      s_lGcc   := .t.
      s_lLinux := .t.
      s_lBcc   := .f.
   ENDIF
   
   IF Len( aFile ) > 1
      IF s_nLang=1
         Alert("Arquivo definido mais que uma vez.")
      ELSEIF s_nLang=3
         Alert("Fichero definido m†s que una vez.")
      ELSE
         Alert("File defined more than once.")
      ENDIF
      RETURN NIL
   ENDIF

   IF Len( aFile ) > 0
      cFile := aFile[ 1 ]
   ELSE
      cFile := ""
   ENDIF

   IF ( Empty( cFile ) .AND. ! s_lEditMode )
      IF nLang=1
         Alert("Arquivo n∆o encontrado.")
      ELSEIF nLang=3
         Alert("Fichero no encontrado.")
      ELSE
         Alert("File not Found.")
      ENDIF
      RETURN NIL
   ENDIF

   /* We have at least one parameter . check IF is an valid file name */

   IF Pcount() >= 1

      IF File( cFile )
         ProcessParameters( AllParam )
      elseif  s_lEditMode
         ProcessParameters( AllParam )
      ELSE

         IF ! s_lEditMode
            IF nLang=1
               Alert("Arquivo n∆o encontrado.")
            ELSEIF nLang=3
               Alert("Fichero no encontrado.")
            ELSE
               Alert("File not Found.")
            ENDIF
            RETURN NIL
         ENDIF

      ENDIF

   ENDIF

   s_cLog := Substr( cFile,1 , AT(".",cFile) -1) + ".out"
   s_AppName := Substr( cFile,1 , AT(".",cFile) -1)

   Ferase( (s_cLog) )

   IF s_lEditMode

      IF s_lLibrary
         CreateLibMakeFile( cFile )
      ELSE
         CreateMakeFile( cFile )
      ENDIF

      RETURN NIL

   ENDIF

   CLS

   /* Make file are parsed here */

   ParseMakeFile( cFile )

   IF s_lPrint
      PrintMacros()
   ENDIF

   IF s_lForce
      CompileFiles()
   ELSE
      CompileUpdatedFiles()
   ENDIF

   setpos(9,0)
   Outstd( s_cLinkComm )
   __RUN( (s_cLinkComm) )

   IF s_lCompress .AND. !s_lLibrary 
      setpos(9,0)
      __Run( " upx -9 "+ (s_cAppName) )
   ENDIF
   tracelog( s_lasdll)
   if s_lasdll .or. lower(right(s_cAppName,3)) == 'dll'
       __Run( Replacemacros("implib $(BHC)\lib\" + left(s_cAppName,at(".",s_cAppName)-1)+".lib " +s_cAppName ))
   endif
RETURN NIL

FUNCTION ParseMakeFile( cFile )

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
   LOCAL lMacrosec   := .f.
   LOCAL lBuildSec   := .f.
   LOCAL lComSec     := .f.
   LOCAL aTemp1      := {}
   LOCAL cCfg        := ""
   LOCAL lCfgFound   := .F.
   LOCAL aTempCFiles := {}
   LOCAL lLinux      :=  'linux' IN  Lower( Os() )
   LOCAL aLib
   LOCAL aLibx
   LOCAL lGui        := .f.
   LOCAL lDjgpp      := "GNU C" in HB_COMPILER()
   LOCAL x :=1
   LOCAL ct

//   IF lDjgpp
//      s_lBcc    := .F.
//      s_lGcc    := .T.
//      s_lVcc    := .F.
//   ENDIF


   s_nHandle := FT_FUSE( cFile )

   IF s_nHandle < 0
      RETURN NIL
   ENDIF


   #IFndef __PLATFORM__Windows
      IF !FILE("hbtemp.c")
         CreateLink()
      ENDIF
   #ENDIF

   cBuffer := Trim( Substr( ReadLN( @s_lEof ), 1 ) )

   aAdd( s_aDefines, { "HMAKEDIR", GetMakeDir() } )

   IF s_lBcc
      aAdd( s_aDefines, { "MAKEDIR", Getbccdir() } )
   ELSEIF s_lGcc
      aAdd( s_aDefines, { "MAKEDIR", GetGccDir() } )
   ELSEIF s_lVcc
      aAdd( s_aDefines, { "MAKEDIR", Getvccdir() } )
   ENDIF

   WHILE ! s_lEof

      IF  cMacro IN  cBuffer
         lMacroSec := .T.
         lBuildSec := .f.
         lComSec   := .f.
      ELSEIF  cBuild IN cBuffer
         lMacroSec := .f.
         lBuildSec := .T.
         lComSec   := .f.
      ELSEIF  cCom IN  cBuffer
         lBuildSec := .f.
         lComSec   := .t.
         lMacroSec := .f.
      ELSE
         ? "Invalid Make File"
         fClose( s_nHandle )
         RETURN NIL
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
                      aAdd( s_aMacros, { aTemp[ 1 ], Strtran( Replacemacros( aTemp[ 2 ] ), "\", "/" ) } )

                      x++
                   ELSE
                     IF aTemp[ 1 ] == "GUI" .AND. aTemp[ 2 ] == "YES"
                        lGui := .T.
                     ENDIF

                     IF aTemp[ 1 ] == "MT" .AND. aTemp[ 2 ] == "YES"
                        s_lMt := .T.
                     ENDIF

                     IF aTemp[ 1 ] == "LIBFILES" .AND. ! s_lMt
                        aLib := ListAsArray2( aTemp[ 2 ], ' ' )

                        FOR each aLibx in aLib

                           IF At( 'mt.lib', Lower( aLibx ) ) > 0
                              s_lMt := .T.
                           ENDIF

                           IF At( 'fivehc.lib', Lower( aLibx ) ) > 0 .OR. At( 'minigui.lib', Lower( aLibx ) ) > 0 .OR. At( 'whoo.lib', Lower( aLibx ) ) > 0 .or. At( 'hwgui.lib', Lower( aLibx ) ) > 0
                              lGui := .T.
                           ENDIF

                           IF "-l" in Lower( aLibx )
                              s_lBcc    := .F.
                              s_lGcc    := .T.
                              s_lVcc    := .F.

                           s_aDefines[2] := { "MAKEDIR", GetGccDir() }
                           ENDIF

                        NEXT

                     ENDIF

                     IF aTemp[ 1 ] == "ALLOBJ" .AND. ! s_lMt

                     ENDIF

                     aAdd( s_aMacros, { aTemp[ 1 ], Replacemacros( aTemp[ 2 ] ) } )

                  ENDIF

               ELSE

                  IF s_lGcc .AND. aTemp[ 1 ] = "CFLAG1" .OR. s_lGcc .AND. aTemp[ 1 ] = "CFLAG2"
                     aAdd( s_aMacros, { aTemp[ 1 ], Strtran( aTemp[ 2 ], "\", "/" ) } )

                      x++

                  ELSE
                     IF aTemp[ 1 ] == "LIBFILES" .AND. ! s_lMt
                        aLib := ListAsArray2( aTemp[ 2 ], ' ' )

                        FOR each aLibx in aLib

                           IF At( 'mt.lib', Lower( aLibx ) ) > 0
                              s_lMt := .T.
                           ENDIF

                           IF At( 'fivehc.lib', Lower( aLibx ) ) > 0 .OR. At( 'minigui.lib', Lower( aLibx ) ) > 0
                              lGui := .T.
                           ENDIF

                           IF "-l" in Lower( aLibx )
                              s_lBcc    := .F.
                              s_lGcc    := .T.
                              s_lVcc    := .F.

                           s_aDefines[2] := { "MAKEDIR", GetGccDir() }
                           s_aMacros[2,2] :=  GetGccDir()
                           ENDIF

                        NEXT

                     ENDIF
                        aAdd( s_aMacros, { aTemp[ 1 ], aTemp[ 2 ] } )
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
                  s_lLibrary := .t.
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

      IF cTemp = "#BUILD"
         cBuffer := cTemp
      ELSEIF cTemp == "#COMMANDS"
         cbuffer := cTemp
      ENDIF

   ENDDO

   IF s_lExtended .AND. ! lCfgFound

      IF s_lBcc
         BuildBorCfgFile()
      ELSEIF s_lVcc
         Buildmsccfgfile()
      ELSEIF s_lGcc .AND. ! lLinux
         Buildgcccfgfile()
      ELSEIF s_lGcc .AND. lLinux
         BuildGccCfgFilel()
      ENDIF

   ENDIF

RETURN NIL

FUNCTION Checkdefine( cTemp )

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
   nPos := aScan( s_aDefines, { | x | x[ 1 ] == aSet[ 1 ] } )

   IF nPos = 0
      cRead    := Alltrim( Strtran( aSet[ 2 ], "$(", "" ) )
      cRead    := Strtran( cRead, ")", "" )
      nMakePos := aScan( s_aDefines, { | x | x[ 1 ] == cRead } )

      IF nMakePos > 0
         aAdd( s_aDefines, { aSet[ 1 ], s_aDefines[ nMakePos, 2 ] } )
         aAdd( s_aMacros, { aSet[ 1 ], s_aDefines[ nMakePos, 2 ] } )
      ENDIF

   ENDIF

RETURN NIL

FUNCTION Setcommands( cTemp )

   LOCAL cRead        := Alltrim( readln( @s_lEof ) )
   LOCAL nPos
   LOCAL nCount       := 0
   LOCAL aTempMacros  := {}
   LOCAL aLocalMacros := {}

   aTempMacros := ListAsArray2( cREad, " " )

   aEval( aTempMacros, { | xMacro | IIF( At( "$", xMacro ) > 0, ;
                         IIF( At( ";", xMacro ) > 0, ( aLocalMacros := ListAsArray2( xMacro, ";" ), ;
                         aEval( aLocalMacros, { | x | Findmacro( x, @cRead ) } ) ), ;
                         Findmacro( xMacro, @cRead ) ), ) } )
   aAdd( s_aCommands, { cTemp, cRead } )

RETURN NIL

FUNCTION Findmacro( cMacro, cRead )

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

   nPos := aScan( s_aMacros, { | x | "$(" + Alltrim( x[ 1 ] ) + ")" == cMacro } )

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

FUNCTION ReplaceMacros( cMacros )

   LOCAL nPos
   LOCAL nCount       := 0
   LOCAL aTempMacros  := {}
   LOCAL aLocalMacros := {}

   aTempMacros := ListAsArray2( cMacros, " " )
   aEval( aTempMacros, { | xMacro | IIF(  "$" IN xMacro , ;
                         IIF(  ";" IN xMacro , ( aLocalMacros := ListAsArray2( xMacro, ";" ), ;
                         aEval( aLocalMacros, { | x | Findmacro( x, @cMacros ) } ) ), ;
                         Findmacro( xMacro, @cMacros ) ), ) } )


RETURN cmacros

FUNCTION SetBuild()

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
      aEval( aTemp, { | xItem | aAdd( s_aBuildOrder, xItem ) } )
   ENDIF

   aAdd( s_aBuildOrder, aMacro[ 1 ] )
   cRead := Strtran( cRead, "@&&!", "" )

   aMacro := ListAsArray2( cRead, '\' )

   aEval( aMacro, { | xMacro |  IIF(  "$" IN xmacro , Findmacro( xMacro, @cRead ), ) } )

   IF ! s_lLinux
      s_cLinkComm   := cRead + "  @" + s_cLinker
      s_nLinkHandle := Fcreate( s_cLinker )
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
                     fWrite( s_nLinkHandle, cRead + CRLF )
                  ENDIF

               ELSEIF s_lGcc .AND. s_lLinux
                  s_cLinkComm += "-o " + cRead + " "
               ELSEIF s_lGcc .AND. ! s_lLinux .AND. At( '.exe', cread ) > 0
                  fWrite( s_nLinkHandle, "-o " + cRead + CRLF )
               ENDIF

            ELSE

               IF ! s_lLinux
                  fWrite( s_nLinkHandle, cRead + CRLF )
               ELSE
                  s_cLinkComm += cRead + " "
               ENDIF

            ENDIF

         ENDIF

      NEXT

   NEXT

   IF ! s_lLinux
      fClose( s_nLinkHandle )
   ENDIF

RETURN NIL

FUNCTION CompileFiles()

   LOCAL cComm
   LOCAL cOld
   LOCAL nPos
   LOCAL nCount
   LOCAL nFiles
   LOCAL cErrText := ""
   LOCAL aOrder   := ListAsArray2( s_aBuildOrder[ 2 ], " " )
   LOCAL lEnd     := .f.
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
            nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".prg.c:" } )

            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ELSE
               nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".PRG.C:" } )

               IF nPos > 0
                  cComm := s_aCommands[ nPos, 2 ]
                  cOld  := cComm
               ENDIF

            ENDIF

            FOR EACH cPrg in s_aPrgs

               xItem := Substr( cPrg, Rat( IIF( s_lGcc, '/', '\' ), ;
                                cPrg ) + 1 )
               nPos := aScan( s_aCs, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), ;
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
                     QUIT
                  ELSE
                     //                            Ferase( (s_cLog) )
                  ENDIF

                  cComm := cOld

               ENDIF

            NEXT

         ENDIF

         IF cOrder == "$(OBJFILES)"

            IF s_lGcc
               nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".c.o:" .OR. x[ 1 ] == ".cpp.o:" } )
            ELSE
               nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".c.obj:" .OR. x[ 1 ] == ".cpp.obj:" } )
            ENDIF

            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ELSE

               IF s_lGcc
                  nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".C.O:" } )

                  IF nPos > 0
                     cComm := s_aCommands[ nPos, 2 ]
                     cOld  := cComm
                  ENDIF

               ELSE
                  nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".C.OBJ:" } )

                  IF nPos > 0
                     cComm := s_aCommands[ nPos, 2 ]
                     cOld  := cComm
                  ENDIF

               ENDIF

            ENDIF

            FOR nFiles := 1 TO Len( s_aCs )

               xItem := Substr( s_aCs[ nFiles ], Rat( IIF( s_lGcc, '/', '\' ), ;
                                s_aCs[ nFiles ] ) + 1 )
               nPos := aScan( s_aObjs, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), ;
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
               nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".c.o:" .OR. x[ 1 ] == ".cpp.o:" } )
            ELSE
               nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".c.obj:" .OR. x[ 1 ] == ".cpp.obj:" } )
            ENDIF

            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ELSE
               nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".C.OBJ:" } )

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
                  nPos := aScan( s_aObjsc, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), ;
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
                     GaugeUpdate( aGauge, nFile / Len( s_aPrgs ) )
                     nFile ++
                     //                            Outstd( cComm )
                     setpos(9,0)
                     __RUN( (cComm) )
                     cErrText := Memoread( (s_cLog) )
                     lEnd     := 'Error E' $   cErrText
                     IF ! s_lIgnoreErrors .AND. lEnd
                        IIF(  "LINUX" IN Upper( Os() ), __run( "mcedit " + (s_cLog) ), __run( "Notepad " + (s_cLog)) )
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
               nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".prg.o:" } )
            ELSE
               nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".prg.obj:" } )
            ENDIF

            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ELSE

               IF s_lGcc
                  nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".PRG.O:" } )
               ELSE
                  nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".PRG.OBJ:" } )
               ENDIF

            ENDIF

            GaugeDisplay( aGauge )
            nFile := 1

            FOR EACH cPrg In s_aPrgs

               @  4, 16 SAY Space( 50 )
               xItem := Substr( cPrg, Rat( IIF( s_lGcc, '/', '\' ), ;
                                cPrg ) + 1 )
               nPos := aScan( s_aObjs, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), ;
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
         nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".rc.res:" } )

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

FUNCTION GetParaDefines( cTemp )

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
   nPos := aScan( s_aDefines, { | x | x[ 1 ] == aSet[ 1 ] } )

   IF nPos == 0
      cRead    := Alltrim( Strtran( aSet[ 2 ], "$(", "" ) )
      cRead    := Strtran( cRead, ")", "" )
      nMakePos := aScan( s_aDefines, { | x | x[ 1 ] == cRead } )

      IF nMakePos = 0
         aSet[ 2 ] := Strtran( aSet[ 2 ], ",", " " )
         aAdd( s_aDefines, { aSet[ 1 ], aSet[ 2 ] } )
         aAdd( s_aMacros, { aSet[ 1 ], aSet[ 2 ] } )
      ENDIF

   ENDIF

RETURN NIL

FUNCTION PrintMacros()

   LOCAL nPos

   Outstd( "HBMAKE Version ", Version(), "CopyRight (c) 2000-2003 The xHarbour Project" + CRLF )
   Outstd( "" + CRLF )
   Outstd( "Macros:" + CRLF )
   aEval( s_aMacros, { | xItem | Outstd( "     " + xItem[ 1 ] + " = " + xItem[ 2 ] + CRLF ) } )
   Outstd( "Implicit Rules:" + CRLF )
   aEval( s_aCommands, { | xItem | Outstd( "     " + xItem[ 1 ] + Hb_OsNewLine() + "        " + xItem[ 2 ] + CRLF ) } )
   Outstd( "" + CRLF )
   Outstd( "Targets:" )
   Outstd( "    " + s_szProject + ":" + CRLF )
   Outstd( "        " + "Flags :" + CRLF )
   Outstd( "        " + "Dependents :" )
   aEval( s_aCs, { | xItem | Outstd( xitem + " " ) } )
   aEval( s_aObjs, { | xItem | Outstd( xitem + " " ) } )
   Outstd( " " + CRLF )
   Outstd( "        commands:" + s_aBuildOrder[ Len( s_aBuildOrder ) ] )
   Outstd( " " + CRLF )
   Outstd( " " + CRLF )
   Outstd( " " + CRLF )

RETURN NIL

FUNC CreateMakeFile( cFile )

   LOCAL ain          := {}
   LOCAL aOut         := {}
   LOCAL aOutc        := {}
   LOCAL aSrc         := Directory( "*.prg" )
   LOCAL nLenaSrc     := Len( aSrc )
   LOCAL nLenaOut
   LOCAL lFwh         := .f.
// LOCAL lxFwh        := .f.
   LOCAL lCw          := .f.
   LOCAL lMiniGui     := .f.
   LOCAL lHwGui       := .f.
   LOCAL lRddAds      := .f.
   LOCAL lWhoo        := .f.
   LOCAL lMediator    := .f.
   LOCAL lApollo      := .f.
// LOCAL lMt          := .F.
   LOCAL cOs          := IIF( "LINUX" IN UPPER( OS() ), "Linux", "Win32")
   LOCAL cCompiler    := IIF( "LINUX" IN UPPER( OS() ), "GCC","BCC")
   LOCAL cfwhpath     := Space( 200 )
   LOCAL cMedpath     := Space( 200 )
   LOCAL cApolloPath  := Space( 200 )
   LOCAL ccwpath      := Space( 200 )
   LOCAL cMiniPath    := Space( 200 )
   LOCAL cWhooPath    := Space( 200 )
   LOCAL cHwPath      := Space( 200 )
   LOCAL cObjDir      := "obj" + Space( 20 )
   LOCAL lAutomemvar  := .f.
   LOCAL lvarismemvar := .f.
   LOCAL ldebug       := .f.
   LOCAL lSupressline := .f.
   LOCAL nPos
   LOCAL cDefHarOpts  := ""

// LOCAL nWarningLevel :=0

   LOCAL lUseXharbourDll := .F.

   LOCAL lCompMod         := .f.

   LOCAL lGenppo          := .f.
   LOCAL x
   LOCAL getlist          := {}
   LOCAL cTopFile         := Space( 30 )
   LOCAL cAppName         := s_Appname + Space( 50 )
   LOCAL cDefBccLibs      := "bcc640.lib lang.lib vm.lib rtl.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib dbffpt.lib dbfdbt.lib common.lib gtwin.lib codepage.lib"
   LOCAL cDefGccLibs      := "-lvm -lrtl -lgtdos -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -ldbffpt -ldbfdbt -lcommon -lcodepage -lm"
   LOCAL cGccLibsOs2      := "-lvm -lrtl -lgtos2 -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -ldbffpt -ldbfdbt -lcommon -lcodepage -lm"
   LOCAL cDefLibGccLibs   := "-lvm -lrtl -lgtcrs -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -ldbffpt -ldbfdbt -lcommon -lcodepage -lgtnul"
   LOCAL cDefBccLibsMt    := "bcc640mt.lib lang.lib vmmt.lib rtlmt.lib rddmt.lib macromt.lib ppmt.lib dbfntxmt.lib dbfcdxmt.lib  dbffptmt.lib dbfdbtmt.lib common.lib gtwin.lib codepage.lib"
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
   LOCAL cGui         := "None"
   LOCAL aLibs
   LOCAL aLibsIn      := {}
   LOCAL aLibsOut     := {}
   Local cGt := ""
   Local lGtWvt       := .F.
   Local lXwt       := .F.

   LOCAL cOldLib      := ""
   LOCAL cHtmlLib     := ""
   LOCAL lLinux       :=  'linux' IN Lower( Os() )
   LOCAL nWriteFiles  := 0
   LOCAL cResName     := Space( 50 )
   LOCAL aSelFiles

   LOCAL lGui         := .F.
   LOCAL cBuild       := " "
   Local aUserDefs
   Local cCurrentDef  := ""
   Local cRdd         := "None"
   LOCAL cCurrentDir  :=""
   Local nO
   Local lNew := .F.
   LOCAL oMake
   Local cAllRes := ""
   Local cTemp
   LOCAL cExtraLibs :=""
   Local cTempLibs := ""
   Local aTempLibs 
   #IFndef __PLATFORM__Windows
       Local lHashhso := File("/usr/lib/libxharbour.so")
       LOCAL lusexhb := FILE("/usr/bin/xhb-build")

   #ELSE
       LOCAL lusexhb := .F.
   #ENDIF

   IF nLenaSrc == 0 .and. !s_lRecurse
      IF s_nLang=1 // PT-BR
         Alert("N∆o h† nenhum prg na pasta "+curdir())
      ELSEIF s_nLang=3 // Spanish
         Alert("No hay ning£n prg en la carpeta "+curdir())
      ELSE
         Alert("Have not any prg in "+curdir()+" folder.")
      ENDIF
      RETURN NIL
   ENDIF


   IF File( cFile )

       IF s_nLang == 1 // Portuguese-BR
          nO := Alert( "O makefile <" + cFile +"> j† existe.",{ "Criar Novo" , "Editar","Cancelar" } )
       ELSEIF s_nLang == 3 // Spanish
          nO := Alert( "Lo makefile <" + cFile +"> ya existe.",{ "Crear Nuevo" , "Editar","Cancelar" } )
       ELSE // English
          nO := Alert( "The makefile <" + cFile +"> already exist ",{ "Create New" , "Edit" , "Cancel" } )
       ENDIF


      IF nO == 1
         s_nLinkHandle := Fcreate( cFile )
         WriteMakeFileHeader()
         lNew := .T.
      ELSEIF nO == 2
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
         cGui            := IIF( oMake:lFwh, "FWH", IIF( oMake:lmini , "MiniGui",IIF(oMake:lWhoo, "Whoo",  IIF( oMake:lCw, "C4W", IIF( oMake:lHwGui, "HWGUI",IIF( oMake:lGtWvt, "Gtwvt","" )) ) ) ) )
         cFwhpath        := oMake:cFmc
         ccwpath         := oMake:cFmc
         cMiniPath       := oMake:cFmc
         cHwPath         := oMake:cFmc
         cWhooPath       := oMake:cFmc
         cMedpath        := oMake:cMedpath
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
         cTopFile        := PadR(oMake:cTopModule,20," ")
         cResName        := oMake:cRes
         s_lRecurse      := oMake:lRecurse
      ELSE
         SetColor("W/N,N/W")
         CLS
         QUIT
      ENDIF
   ELSE
         s_nLinkHandle := Fcreate( cFile )
         WriteMakeFileHeader()
         nO := 1
   ENDIF

   CLS
   Setcolor( 'w/b+,b+/w,w+/b,w/b+,w/b,w+/b' )
   @  0,  0, Maxrow(), Maxcol() BOX( Chr( 201 ) + Chr( 205 ) + Chr( 187 ) + Chr( 186 ) + Chr( 188 ) + Chr( 205 ) + Chr( 200 ) + Chr( 186 ) + Space( 1 ) )

   Attention( s_aLangMessages[ 27 ], 0 )
   Attention( s_aLangMessages[ 47 ], maxrow() )
   @  1,  1 SAY s_aLangMessages[ 28 ]

// @ 01,12 GET cOS radio { "Win32", "OS/2", "Linux" }   VALID ! Empty( cOS )
   @ 01,12,06,18 get cOs listbox { "Win32", "OS/2", "Linux" } message s_aLangMessages[ 49 ] state OsSpec(getlist,1,@cos)  DROPDOWN
   @ 01,21 SAY s_aLangMessages[ 29 ]
// @ 01,40 GET cCompiler radio { "BCC", "MSVC", "GCC" } VALID ! Empty( cCompiler )
   @ 01,40,06,46 get cCompiler listbox { "BCC", "MSVC", "GCC" }  message s_aLangMessages[ 50 ] state OsSpec(getlist,2,@cCompiler) DROPDOWN
   @ 01,48 SAY s_aLangMessages[ 30 ]
// @ 01,64 GET lFwh checkbox caption "Use FWH"          WHEN Cos == "Win32" style "[o ]"
// @ 02,64 GET lcw checkbox caption "Use C4W"           WHEN Cos == "Win32" style "[o ]"
// @ 04,64 GET lMiniGui checkbox caption "Use Minigui"  WHEN Cos == "Win32" style "[o ]"
   @ 01,60,08,78 Get cGui ListBox { "None","Xwt","Gtwvt","FWH","MiniGui","What32","Whoo","C4W","HWGUI"}  DROPDOWN state OsSpec(getlist,3,@cGui) When CheckCompiler(cOs) message s_aLangMessages[ 51 ]
// @ 02,01 GET lRddads checkbox caption "Use RddAds"    WHEN Cos == "Win32" .OR. Cos == "Linux" style "[o ]"
   @ 02,01 Say s_aLangMessages[ 48 ]
   @ 02,16,06,26 get cRdd ListBox { "None","RddAds","Mediator","Apollo"}  WHEN Cos == "Win32" .or. Cos == "Linux" DROPDOWN message s_aLangMessages[ 52 ]
   @ 02,30 Get s_lCompress CheckBox  caption s_aLangMessages[ 53 ] style "[o ]" message s_aLangMessages[ 54 ]
   @ 02,53 Get lUseXharbourDll CheckBox caption "use Xharbour[.dll|.so]" style "[o ]" WHEN Cos == "Win32" .or. Cos == "Linux" message s_aLangMessages[ 55 ]
   @ 03,01 SAY "Obj Files Dir" GET cObjDir PICT "@s15" message s_aLangMessages[ 56 ]
   @ 04,01 SAY  s_aLangMessages[ 45 ] GET cAppName  pict "@s15"valid !Empty( cAppName ) message s_aLangMessages[ 57 ]
   @ 4,53 get s_lasdll CheckBox  Caption "Create dll" style "[o ]" 

   IF nO == 1
      READ MSG AT MaxRow() - 1, 1, MaxCol() - 1

      s_cAppName := alltrim(cAppName)
      IF cOs != "Linux"
         if s_lasdll
            s_cAppName += ".dll"
         else
            s_cAppName += ".exe"
         endif
      ENDIF

   ENDIF
 if  s_lasdll
 lUseXharbourDll:= .t.
 endif
   lFwh      := "FWH"      IN cGui
   lMiniGui  := "MiniGui"  IN cGui
   lRddAds   := "RddAds"   IN cRdd
   lMediator := "Mediator" IN cRdd
   lApollo   := "Apollo"   IN cRdd
   lHwGui    := "HWGUI"    IN cGui
   lWhoo     := "Whoo"     IN cGui
   lGtWvt    := "Gtwvt"    IN cGui
   lXwt      := "Xwt"    IN cGui
   tracelog(lUseXharbourDll)
   IF lUseXharbourDll
      cDefLibGccLibs   := cHARso
      cDefBccLibs      := cHarbDll
   ENDIF

   IF lFwh
      @  3, 40 SAY "FWH path" GET cfwhpath PICT "@s20"
   ELSEIF lCw
      @  3, 40 SAY "C4W path" GET ccwpath PICT "@s20"
   ELSEIF lMiniGui
      @  3, 40 SAY "MiniGui path" GET cMiniPath PICT "@s20"
   ELSEIF lHwGui
      @  3, 40 SAY "HwGUI path" GET cHwPath PICT "@s20"
   ELSEIF lWhoo
      @  3, 40 SAY "Whoo path" GET cWhooPath PICT "@s20"
   ENDIF
   IF lMediator
      @  3, 40 SAY "Mediator path" GET cMedPath PICT "@s20"
   ENDIF
   IF lApollo
      @  3, 40 SAY "Apollo path" GET cApolloPath PICT "@s20"
   ENDIF
   IF nO == 1
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
   @ 11, 01 SAY "Resource file Name" GET cResName
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

   s_lBcc    := "BCC"  IN cCompiler
   s_lVcc    := "MSVC" IN cCompiler
   s_lGcc    := "GCC"  IN cCompiler

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
   //      DirChange( '..' )
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

   aEval( s_aMacros, { | x, y | cItem := Substr( x[ 2 ], 1, Len( x[ 2 ] ) ), IIF( At( citem, cTest ) > 0, ( s_aMacros[ y, 1 ] := 'OBJ', s_aMacros[ y, 2 ] := cObjDir ), ) } )

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
      aAdd( s_aCommands, { ".cpp.obj:", "$(BCB)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $**" } )
      aAdd( s_aCommands, { ".c.obj:", "$(BCB)\BIN\bcc32 -I$(BHC)\include $(CFLAG1) $(CFLAG2) -o$* $**" } )

      IF s_lExtended
         aAdd( s_aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -D__EXPORT__ -n"+if(s_lasdll,"1","")+" -go -I$(BHC)\include $(HARBOURFLAGS)" + IIF( lFwh, " -I$(FWH)\include", IIF( lMinigui, " -I$(MINIGUI)\include",IIF( lHwgui, " -I$(HWGUI)\include","" ) ) )+IIF( lWhoo," -I$(WHOO)\include ","")+  IIF( lMediator," -I$(MEDIATOR)\include ","")+" -o$* $**" } )
      ELSE
         aAdd( s_aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS)" + IIF( lFwh, " -I$(FWH)\include", IIF( lMinigui, " -I$(MINIGUI)\include",IIF( lHwgui, " -I$(HWGUI)\include","" ) )) + " -o$* $**" } )
      ENDIF

      aAdd( s_aCommands, { ".rc.res:", "$(BCB)\BIN\brcc32 $(RFLAGS) $<" } )
   ELSEIF s_lGcc

        IF  "linux" IN Lower(Getenv( "HB_ARCHITECTURE" ) )  .OR. cOs == "Linux"
         aAdd( s_aCommands, { ".cpp.o:", "gcc $(CFLAG1) $(CFLAG2) -o$* $**" } )
         aAdd( s_aCommands, { ".c.o:", "gcc -I/usr/include/xharbour $(CFLAG1) $(CFLAG2) -I. -g -o$* $**" } )

         IF s_lExtended
            aAdd( s_aCommands, { ".prg.o:", "harbour -D__EXPORT__  -n"+if(s_lasdll,"1","")+"  -go -I/usr/include/xharbour $(HARBOURFLAGS) -I.  -o$* $**" } )
         ELSE
            aAdd( s_aCommands, { ".prg.c:", "harbour -n -I/usr/include/xharbour $(HARBOURFLAGS) -I.  -o$* $**" } )
         ENDIF

      ELSE
         aAdd( s_aCommands, { ".cpp.o:", "$(BCB)\bin\gcc $(CFLAG1) $(CFLAG2) -o$* $**" } )
         aAdd( s_aCommands, { ".c.o:", "$(BCB)\bin\gcc -I$(BHC)/include $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )

         IF s_lExtended
            aAdd( s_aCommands, { ".prg.o:", "$(BHC)\bin\harbour -D__EXPORT__  -n"+if(s_lasdll,"1","")+" -go -I$(BHC)/include $(HARBOURFLAGS)  -o$* $**" } )
         ELSE
            aAdd( s_aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)/include $(HARBOURFLAGS)  -o$* $**" } )
         ENDIF

      ENDIF

   ELSEIF s_lVcc
      aAdd( s_aCommands, { ".cpp.obj:", "$(BCB)\bin\cl $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
      aAdd( s_aCommands, { ".c.obj:", "$(BCB)\bin\cl -I$(BHC)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )

      IF s_lExtended
         aAdd( s_aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -D__EXPORT__  -n -I$(BHC)\include $(HARBOURFLAGS) -go  -I$(C4W)\include" + IIF( lMediator," -I$(MEDIATOR)\include ","")+ "-o$* $**" } )
      ELSE
         aAdd( s_aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -I$(C4W)\include -o$* $**" } )
      ENDIF

      aAdd( s_aCommands, { ".rc.res:", "$(BCB)\rc $(RFLAGS) $<" } )
   ENDIF

   Attention( s_aLangMessages[ 41 ], 22 )

   IF ! s_lRecurse
      aIn      := GetSourceFiles( .f., s_lGcc, cOs )
      nLenaSrc := Len( aIn )
   ELSE
      aIn      := GetSourceFiles(, s_lGcc, cOs )
      nLenaSrc := Len( aSrc )
   ENDIF

   aOut := aClone( aIn )

   IF nO != 1
      pickarry( 11, 15, 20, 64, aIn, aOut ,ArrayAJoin( { oMake:aPrgs, oMake:aCs } ), .T. )
   ELSE
      pickarry( 11, 15, 20, 64, aIn, aOut, {}, .T. )
   ENDIF

   nLenaOut := Len( aOut )

   aEval( aout, { | x, y | aout[ y ] := Trim( Substr( aOut[ y ], 1, At( ' ', aout[ y ] ) ) ) } )

   aOut := aSort( aOut )

   @ 22,01 say space(78)

   aSelFiles := GetSelFiles( aIn, aOut )

   aSort( aSelFiles )

   if Len( aSelFiles ) = 1
      cTopFile := aSelFiles[1] 
      cTopFile := PadR( Left(cTopfile,At(Upper(".prg"),Upper(cTopFile))+4 ), 20)
   endif

   WHILE Len( aSelFiles ) > 1

      IF s_nLang=1 // PT
         @ 15,01 say "Informe o PRG principal da sua aplicaá∆o: " Get cTopFile valid !empty(cTopFile)
      ELSEIF s_nLang=3
         @ 15,01 say "Informe o PRG principale de su aplicacion: " Get cTopFile valid !empty(cTopFile)
      ELSE
         @ 15,01 say "Inform the main PRG of your application: " Get cTopFile valid !empty(cTopFile)
      ENDIF

      READ
      
      if lastkey()=27
         exit
      endif 

      IF !file(ALLTRIM(cTopFile))
         IF s_nLang=1 // PT
            Alert("Arquivo "+alltrim(cTopFile)+" n∆o encontrado.")
         ELSEIF s_nLang=3
            Alert("Fichero "+alltrim(cTopFile)+" no encontrado.")
         ELSE
            Alert("File "+alltrim(cTopFile)+" not found.")
         ENDIF
      ELSE
         EXIT
      ENDIF

   END

   // Selecting External Libs.
   IF s_lExternalLib
      aLibs := Getlibs( s_lGcc, GetMakeDir() + '\lib' )

      IF s_nLang == 1 .OR. s_nLang == 3 // PT
         Attention( 'Barra de espaáo para selecionar, Enter para continuar o processo.', 22 )
      ELSE
         Attention( 'Spacebar to select, Enter to continue process', 22 )
      ENDIF

      aEval( aLibs, { | x | aAdd( aLibsIn, x[ 1 ] ) } )
      aEval( aLibs, { | x | aAdd( aLibsOut, x[ 2 ] ) } )
      pickarry( 11, 15, 20, 64, aLibsIn, aLibsOut ,{} )
   ENDIF

   aEval( aout, { | xItem | IIF(  '.c'IN xItem  .OR.  '.C' IN xItem , aAdd( aoutc, xitem ), ) } )
   aEval( aoutc, { | x, z | citem := x, z := aScan( aout, { | t | t = citem } ), IIF( z > 0, aSize( aDel( aout, z ), Len( aout ) - 1 ), ) } )

   @ 22,01 say space(78)

   aOut  := aSort( aOut )
   s_aPrgs := aClone( aout )

   s_aObjs := aClone( aout )

   x     := aScan( s_aObjs, { | x | Lower( x ) in Lower( cTopFile ) } )

   IF x > 0
      aDel( s_aObjs, x )
      aSize( s_aObjs, Len( s_aObjs ) - 1 )
      aSize( s_aObjs, Len( s_aObjs ) + 1 )
      aIns( s_aObjs, 1 )
      s_aObjs[ 1 ] := AllTrim( cTopFile )
   ENDIF

   x := aScan( s_aPrgs, { | x | Lower( x ) in Lower( cTopFile ) } )

   IF x > 0
      aDel( s_aPrgs, x )
      aSize( s_aPrgs, Len( s_aPrgs ) - 1 )
      aSize( s_aPrgs, Len( s_aPrgs ) + 1 )
      aIns( s_aPrgs, 1 )
      s_aPrgs[ 1 ] :=  AllTrim( cTopFile )

   ENDIF

   aEval( s_aObjs, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, s_aObjs[ x ] := cObjDir + cTest + "." + Exte( cExt, 2 ), s_aObjs[ x ] := cObjDir + cTest + "." + Exte( cExt, 3 ) ) } )
   s_aCs := aClone( aoutc )

   IF ! s_lExtended
      aEval( aOutc, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, aAdd( s_aObjs, cObjDir + cTest + "." + Exten( cExt, 2 ) ), aAdd( s_aObjs, cObjDir + cTest + "." + Exten( cExt, 1 ) ) ) } )
      aEval( aout, { | xItem | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cExt := Substr( cExt, 2 ), aAdd( s_aCs, cObjDir + cTest + "." + Exte( cExt, 1 ) ) } )
   ELSE
      s_aObjsc := aClone( aoutc )
      aEval( aoutc, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, s_aObjsc[ x ] := IIF( ! Empty( cObjDir ), cObjDir, '' ) + cTest + "." + Exten( cExt, 2 ), s_aObjsc[ x ] := IIF( ! Empty( cObjDir ), cObjDir, '' ) + cTest + "." + Exten( cExt, 1 ) ) } )
   ENDIF
   IF !lNew
         s_nLinkHandle := Fcreate( cFile )
         WriteMakeFileHeader()
   ENDIF

   fWrite( s_nLinkHandle, "COMPRESS = " + IIF( s_lCompress, "YES", "NO" ) + CRLF )

   fWrite( s_nLinkHandle, "EXTERNALLIB = " + IIF( s_lExternalLib, "YES", "NO" ) + CRLF )

   fWrite( s_nLinkHandle, "XFWH = " + IIF( s_lxFwh, "YES", "NO" ) + CRLF )

   fWrite( s_nLinkHandle, "FILESTOADD = " + Str( s_nFilesToAdd, 2 ) + CRLF )

   fWrite( s_nLinkHandle, "WARNINGLEVEL = " + Str(s_nWarningLevel, 2) + CRLF )

   fWrite( s_nLinkHandle, "USERDEFINE = " + s_cUserDef + CRLF )

   fWrite( s_nLinkHandle, "USERINCLUDE = " + s_cUserInclude + CRLF )
   
   IF lFwh
      fWrite( s_nLinkHandle, "FWH = " + cfwhpath + CRLF )
      lGui := .T.
   ELSEIF lCw
      fWrite( s_nLinkHandle, "C4W = " + ccwpath + CRLF )
      lGui := .T.
   ELSEIF lMiniGui
      fWrite( s_nLinkHandle, "MINIGUI = " + cMiniPath + CRLF )
      lGui := .T.
   ELSEIF lHwGui
      fWrite( s_nLinkHandle, "HWGUI = " + cHwPath + CRLF )
      lGui := .T.
   ELSEIF lgtWvt
      fWrite( s_nLinkHandle, "GTWVT = " +  CRLF )
      lGui := .T.

   ELSEIF lWhoo
      fWrite( s_nLinkHandle, "WHOO = " + cWhooPath + CRLF )
      lGui := .T.

   ENDIF

   IF lMediator
      fWrite( s_nLinkHandle, "MEDIATOR = " + cMedPath + CRLF )
      lGui := .F.
   ENDIF

   fWrite( s_nLinkHandle, "GUI = " + IIF(lWhoo .OR. lFwh .OR. lCw .OR. lMinigui, "YES", "NO" ) + CRLF )
   fWrite( s_nLinkHandle, "MT = " + IIF( s_lMt, "YES", "NO" ) + CRLF )

   FOR x := 1 TO Len( s_aMacros )

      IF ! Empty( s_aMacros[ x, 2 ] )
         cItem := s_aMacros[ x, 2 ]
         nPos  := aScan( s_aPrgs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0
            aEval( s_aPrgs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aPrgs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ')/', ')\' ) ), ) } )

            IF ! s_aMacros[ x, 3 ]
               fWrite( s_nLinkHandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
               s_aMacros[ x, 3 ] := .t.
            ENDIF

         ENDIF

         nPos := aScan( s_aCs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0

            IF ! s_aMacros[ x, 3 ]
               aEval( s_aCs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aCs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )
               fWrite( s_nLinkHandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
               s_aMacros[ x, 3 ] := .t.
            ENDIF

         ENDIF

         nPos := aScan( s_aObjs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0

            IF ! Empty( cObjDir )
               aEval( s_aObjs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aObjs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )
               fWrite( s_nLinkHandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
            ENDIF

         ENDIF

         IF s_lExtended
            nPos := aScan( s_aObjsc, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

            IF nPos > 0

               IF ! Empty( cObjDir )
                  aEval( s_aObjsc, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aObjsc[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )
               ENDIF

            ENDIF

         ENDIF

      ENDIF

   NEXT

   IF s_lGcc

      IF  "linux" IN Lower( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOs == "Linux"
         fWrite( s_nLinkHandle, "PROJECT = " + Alltrim( Lower( cAppName ) ) + " $(PR) " + CRLF )
      ELSE
         fWrite( s_nLinkHandle, "PROJECT = " + Alltrim( Lower( cAppName ) ) + ".exe"   + " $(PR) " + CRLF )
      ENDIF

   ELSE
      fWrite( s_nLinkHandle, "PROJECT = " + Alltrim( Lower( cAppName ) ) + if(s_lasdll,".dll",".exe" ) + " $(PR) " + CRLF )
   ENDIF


   IF ! s_lExtended
      fWrite( s_nLinkHandle, "OBJFILES = " )

      IF Len( s_aObjs ) < 1
         fWrite( s_nLinkHandle, + " $(OB) " + CRLF )
      ELSE
         aEval( s_aObjs, { | x, i | IIF( ( i <> Len( s_aObjs ) .AND. x <> cTopfile  ), fWrite( s_nLinkHandle, ' ' + Alltrim( x ) ), fWrite( s_nLinkHandle, " " + " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )
      ENDIF

      fWrite( s_nLinkHandle, "CFILES =" )

      IF Len( s_aCs ) < 1
         fWrite( s_nLinkHandle, + " $(CF)" + CRLF )
      ELSE
         aEval( s_aCs, { | x, i | IIF( ( i <> Len( s_aCs ) .AND. x <> cTopfile  ), fWrite( s_nLinkHandle, ' ' + Alltrim( x ) ), fWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(CF) " + CRLF ) ) } )
      ENDIF

      fWrite( s_nLinkHandle, "PRGFILE =" )

      IF Len( s_aPrgs ) < 1
         fWrite( s_nLinkHandle, + " $(PS)" + CRLF )
      ELSE
         aEval( s_aPrgs, { | x, i | IIF( i <> Len( s_aPrgs) .AND. x <> cTopfile , fWrite( s_nLinkHandle, ' ' + Alltrim( x ) ), fWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )
      ENDIF

   ELSE
      fWrite( s_nLinkHandle, "OBJFILES =" )

      IF Len( s_aObjs ) < 1
         fWrite( s_nLinkHandle, + " $(OB) " + CRLF )
      ELSE
         aEval( s_aObjs, { | x, i | nWriteFiles ++, IIF( ( i <> Len( s_aObjs ) .AND. x <> cTopfile  ), fWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % s_nFilestoAdd == 0, " //" + CRLF, "" ) ), fWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )
      ENDIF

      nWriteFiles := 0
      fWrite( s_nLinkHandle, "PRGFILES =" )

      IF Len( s_aPrgs ) < 1
         fWrite( s_nLinkHandle, + " $(PS)" + CRLF )
      ELSE
         aEval( s_aPrgs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aPrgs ), fWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % s_nFilestoAdd == 0, " //" + CRLF, "" ) ), fWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )
      ENDIF

      nWriteFiles := 0
      fWrite( s_nLinkHandle, "OBJCFILES =" )

      IF Len( s_aObjsc ) < 1
         fWrite( s_nLinkHandle, + " $(OBC) " + CRLF )
      ELSE
         aEval( s_aObjsc, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aObjsc ), fWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % s_nFilestoAdd == 0, " //" + CRLF, "" ) ), fWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(OBC) " + CRLF ) ) } )
      ENDIF

      nWriteFiles := 0
      fWrite( s_nLinkHandle, "CFILES =" )

      IF Len( s_aCs ) < 1
         fWrite( s_nLinkHandle, + " $(CF)" + CRLF )
      ELSE
         aEval( s_aCs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aCs ), fWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % s_nFilestoAdd == 0, " //" + CRLF, "" ) ), fWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )
      ENDIF

   ENDIF

   CResName := Lower( CResName )
   fWrite( s_nLinkHandle, "RESFILES = " + CResName + CRLF )
   fWrite( s_nLinkHandle, "RESDEPEN = " + Strtran( CResName, ".rc", ".res" ) + CRLF )
   fWrite( s_nLinkHandle, "TOPMODULE = " + cTopFile + CRLF )

   IF lRddads
      cDefBccLibs      += " rddads.lib ace32.lib"
      cDefLibGccLibs   += " -lrddads -ladsloc "
      cDefBccLibsMt    += " rddads.lib ace32.lib"
      cDefLibGccLibsMt += " -lrddads -ladsloc "
      cExtraLibs += " -lrddads -ladsloc "
   ENDIF

   IF Len( aLibsOut ) > 0 .AND. s_lExternalLib

      IF s_lVcc .OR. s_lBcc

         IF s_lVcc // remove bcc640.lib form msvc
            cDefBccLibs   := StrTran( cDefBccLibs, "bcc640.lib ", "")
            cDefBccLibsMt := StrTran( cDefBccLibsMt, "bcc640mt.lib ", "")
         ENDIF

         IF ! s_lMt
            cOldLib := cDefBccLibs
         ELSE
            cOldLib := cDefBccLibsMt
         ENDIF

         nPos := aScan( aLibsOut, { | z | At( "html", Lower( z ) ) > 0 } )

         IF npos > 0
            cHtmlLib += aLibsOut[ npos ]
            aDel( aLibsOut, nPos )
            aSize( aLibsOut, Len( aLibsOut ) - 1 )
            cOldLib := StrTran( cOldLib, "gtwin" , "gtcgi" )
         ENDIF

         aEval( aLibsOut, { | cLib | cLibs += " " + cLib } )
	 nPos := aScan( aLibsOut, { | z | At( "mysql", Lower( z ) ) > 0 } )
	 
	 if nPos >0 
        cLibs += " libmysql.lib"
	 endif 

     nPos := aScan( aLibsOut, { | z | At( "hbpg", Lower( z ) ) > 0 } )
	 
	 if nPos >0 
        cLibs += " libpq.lib"
        cLibs := strtran(cLibs,"hbpg","hbpg")
	 endif 

         IF ! s_lMt
            cDefBccLibs := cHtmlLib + " " + cOldLib + " " + cLibs
         ELSE
            cDefBccLibsMt := cHtmlLib + " " + cOldLib + " " + cLibs
         ENDIF

      ENDIF

      IF s_lGcc
         nPos := aScan( aLibsOut, { | z | At( "html", Lower( z ) ) > 0 } )

         IF npos > 0
            cHtmlLib += "-l" + Strtran( aLibsOut[ npos ], '.a', "" )
            aDel( aLibsOut, nPos )
            aSize( aLibsOut, Len( aLibsOut ) - 1 )
         ENDIF

         aEval( aLibsOut, { | cLib | iif( Len(aTempLibs :=ListAsArray2( cLib, " ") )> 0 ,cLibs += SetthisLibs(AtempLibs) ,cLibs += " -l" + Strtran( cLib, '.a', "" ))} )

 	 nPos := aScan( aLibsOut, { | z | At( "mysql", Lower( z ) ) > 0 } )
	 
	 if nPos >0 
	    cLibs += " -lmysqlclient"
	 endif
     nPos := aScan( aLibsOut, { | z | At( "hbpg", Lower( z ) ) > 0 } )
	 
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

   IF s_lBcc .OR. s_lVcc


      IF lFwh

         IF s_lXfwh
            fWrite( s_nLinkHandle, "LIBFILES = $(FWH)\lib\fivehx.lib $(FWH)\lib\fivehc.lib optgui"+ IIF( ! s_lMt, "", "mt" ) +".lib " + IIF( ! s_lMt, cDefBccLibs, cDefBccLibsMt ) + CRLF )
         ELSE
            fWrite( s_nLinkHandle, "LIBFILES = $(FWH)\lib\fiveh.lib $(FWH)\lib\fivehc.lib optgui"+ IIF( ! s_lMt, "", "mt" ) +".lib " + IIF( ! s_lMt, cDefBccLibs, cDefBccLibsMt ) + CRLF )
         ENDIF

      ELSEIF lMiniGui
         fWrite( s_nLinkHandle, "LIBFILES = Minigui.lib optgui.lib " + IIF( ! s_lMt, cDefBccLibs, cDefBccLibsMt ) + CRLF )
      ELSEIF lWhoo
         fWrite( s_nLinkHandle, "LIBFILES = whoo.lib what32.lib " + IIF( ! s_lMt, cDefBccLibs, cDefBccLibsMt ) + CRLF )
      ELSEIF lHwGui
         fWrite( s_nLinkHandle, "LIBFILES = hwgui.lib procmisc.lib hwg_qhtm.lib " + IIF( ! s_lMt, cDefBccLibs, cDefBccLibsMt ) + CRLF )
      ELSEIF lCw
         fWrite( s_nLinkHandle, "LIBFILES = $(C4W)\c4wclass.lib $(C4W)\wbrowset.lib $(C4W)\otabt.lib $(C4W)\clip4win.lib optgui.lib "  + IIF( ! s_lMt, cDefBccLibs, cDefBccLibsMt ) + CRLF )
      ELSE
       if lGtwvt
         cDefBccLibs := strtran(cDefBccLibs,"gtwin","gtwvt")
         cDefBccLibsMt := strtran(cDefBccLibsMt,"gtwin","gtwvt")
      endif
         fWrite( s_nLinkHandle, "LIBFILES = " + iif( lgtWvt,"optgui",if(!s_lasdll,"optcon","")) + IIF( ! s_lMt, "", "mt" ) + ".lib " + IIF( ! s_lMt, cDefBccLibs, cDefBccLibsMt ) + CRLF )
      ENDIF

   ELSEIF s_lGcc

      IF cOs == "Linux"
         fWrite( s_nLinkHandle, "LIBFILES = " + IIF(lusexhb, cExtraLibs , "-Wl,--start-group " + IIF( ! s_lMt, cDefLibGccLibs, cDefLibGccLibsMt ) + " -Wl,--end-group " + cSystemLibs ) + CRLF )
      ELSEIF cOs == "OS/2"
         fWrite( s_nLinkHandle, "LIBFILES = " + IIF( ! s_lMt, cGccLibsOs2, cGccLibsOs2Mt ) + CRLF )
      ELSE
         fWrite( s_nLinkHandle, "LIBFILES = " + IIF( ! s_lMt, cDefGccLibs, cDefGccLibs ) + CRLF )
      ENDIF

   ENDIF

   fWrite( s_nLinkHandle, "DEFFILE = " + CRLF )
   fWrite( s_nLinkHandle, "HARBOURFLAGS = " + cDefHarOpts + CRLF )

   IF s_lBcc
      fWrite( s_nLinkHandle, "CFLAG1 =  -OS $(CFLAGS) -d -L$(BHC)\lib;$(FWH)\lib -c" +" -I" + Alltrim( s_cUserInclude ) + " " +CRLF )
      fWrite( s_nLinkHandle, "CFLAG2 =  -I$(BHC)\include;$(BCB)\include" + IIF( s_lMt, "-DHB_THREAD_SUPPORT" , "" ) + CRLF )

      fWrite( s_nLinkHandle, "RFLAGS = " + CRLF )
      fWrite( s_nLinkHandle, "LFLAGS = -L$(BCB)\lib\obj;$(BCB)\lib;$(BHC)\lib -Gn -M -m -s -Tp"+ if(s_lasdll,"d","e") + IIF( lFWH, " -aa", IIF( lMiniGui .or. lWhoo , " -aa", IIF( lHwgui .or. lgtWvt, " -aa"," -ap" ) ) ) + IIF( lMinigui, " -L$(MINIGUI)\lib",IIF( lFwh, " -L$(FWH)\lib",IIF( lHwgui, " -L$(HWGUI)\lib","" ))) + CRLF )
      fWrite( s_nLinkHandle, "IFLAGS = " + CRLF )
      fWrite( s_nLinkHandle, "LINKER = ilink32" + CRLF )
      fWrite( s_nLinkHandle, " " + CRLF )
      fWrite( s_nLinkHandle, "ALLOBJ = " + IIF( ( lWhoo .OR. lFwh .OR. lMinigui .OR. lHwgui .or. lgtWvt ), "c0w32.obj", if(s_lasdll,"c0d32.obj","c0x32.obj" )) + " $(OBJFILES)" + IIF( s_lExtended, " $(OBJCFILES)", " " ) + ;
               + CRLF )
      fWrite( s_nLinkHandle, "ALLRES = $(RESDEPEN)" + CRLF )
      fWrite( s_nLinkHandle, "ALLLIB = $(LIBFILES) import32.lib " + IIF( s_lMt,"cw32mt.lib", "cw32.lib" )+ CRLF )
      fWrite( s_nLinkHandle, ".autodepend" + CRLF )
   ELSEIF s_lVcc
      fWrite( s_nLinkHandle, "CFLAG1 =  -I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(CFLAGS)" +IIF( s_lMt, "-DHB_THREAD_SUPPORT" , "" ) + CRLF )
      fWrite( s_nLinkHandle, "CFLAG2 =  -c" +" -I" + Alltrim( s_cUserInclude ) + " " + CRLF )
      fWrite( s_nLinkHandle, "RFLAGS = " + CRLF )
      fWrite( s_nLinkHandle, "LFLAGS = /LIBPATH:$(BCB)\lib;$(BHC)\lib;$(C4W)\lib /SUBSYSTEM:CONSOLE" +IIF(s_lMt, " /Nodefaultlib:LIBC "," /Nodefaultlib:LIBCMT " ) + CRLF )
      fWrite( s_nLinkHandle, "IFLAGS = " + CRLF )
      fWrite( s_nLinkHandle, "LINKER = link" + CRLF )
      fWrite( s_nLinkHandle, " " + CRLF )
      fWrite( s_nLinkHandle, "ALLOBJ = " + IIF( lCw, "$(C4W)\initc.obj", "" ) + "$(OBJFILES)" + IIF( s_lExtended, " $(OBJCFILES)", " " ) + CRLF )
      fWrite( s_nLinkHandle, "ALLRES = $(RESDEPEN)" + CRLF )
      fWrite( s_nLinkHandle, "ALLLIB = $(LIBFILES) comdlg32.lib shell32.lib user32.lib gdi32.lib" + CRLF )
   ELSEIF s_lGcc
      fWrite( s_nLinkHandle, "CFLAG1 = " +IIF( !EMPTY(s_cUserInclude ) ," -I" + Alltrim( s_cUserInclude ),"")        + IIF(  "Linux" IN cOs, "-I/usr/include/xharbour", " -I$(BHC)/include" ) + " -c -Wall" + IIF( s_lMt, "-DHB_THREAD_SUPPORT" , "" ) + CRLF )
      fWrite( s_nLinkHandle, "CFLAG2 = " + IIF(  "Linux" IN cOs, "-L$(HB_LIB_INSTALL)", " -L$(BHC)/lib" )  + CRLF )

      fWrite( s_nLinkHandle, "RFLAGS = " + CRLF )
      fWrite( s_nLinkHandle, "LFLAGS =" + IIF(lUseXhb ,IIF(lUseXharbourDll,"","-static ") + if(lXwt,"-gtcgi " , "-gtcrs "), "$(CFLAG2)") + iif(lXwt,"`pkg-config --libs gtk+-2.0` -lxwt -lxwt_gtk -lxwt","") + CRLF )
      fWrite( s_nLinkHandle, "IFLAGS = " + CRLF )
      fWrite( s_nLinkHandle, "LINKER = "+ IIF(lusexhb,"xhblnk","gcc") + CRLF )
      fWrite( s_nLinkHandle, " " + CRLF )
      fWrite( s_nLinkHandle, "ALLOBJ = $(OBJFILES) " + IIF( s_lExtended, " $(OBJCFILES)", " " ) + CRLF )
      fWrite( s_nLinkHandle, "ALLRES = $(RESDEPEN) " + CRLF )
      fWrite( s_nLinkHandle, "ALLLIB = $(LIBFILES) " + CRLF )
      fWrite( s_nLinkHandle, ".autodepend" + CRLF )
   ENDIF

   fWrite( s_nLinkHandle, " " + CRLF )
   fWrite( s_nLinkHandle, "#COMMANDS" + CRLF )

   aEval( s_aCommands, { | xItem | fWrite( s_nLinkHandle, xitem[ 1 ] + CRLF ), fWrite( s_nLinkHandle, xitem[ 2 ] + CRLF ), fWrite( s_nLinkHandle, " " + CRLF ) } )

   IF s_lBcc
      fWrite( s_nLinkHandle, "#BUILD" + CRLF )
      fWrite( s_nLinkHandle, " " + CRLF )
      fWrite( s_nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)" + CRLF )
      fWrite( s_nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!  " + CRLF )
      fWrite( s_nLinkHandle, "    $(LFLAGS) +" + CRLF )
      fWrite( s_nLinkHandle, "    $(ALLOBJ), +" + CRLF )
      fWrite( s_nLinkHandle, "    $(PROJECT),, +" + CRLF )
      fWrite( s_nLinkHandle, "    $(ALLLIB), +" + CRLF )
      fWrite( s_nLinkHandle, "    $(DEFFILE), +" + CRLF )
      fWrite( s_nLinkHandle, "    $(ALLRES) " + CRLF )
      fWrite( s_nLinkHandle, "!" + CRLF )
   ELSEIF s_lVcc
      fWrite( s_nLinkHandle, "#BUILD" + CRLF )
      fWrite( s_nLinkHandle, "" + CRLF )
      fWrite( s_nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)" + CRLF )
      fWrite( s_nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&! " + CRLF )
      fWrite( s_nLinkHandle, "    $(LFLAGS)" + CRLF )
      fWrite( s_nLinkHandle, "    $(ALLOBJ) " + CRLF )
      fWrite( s_nLinkHandle, "    $(PROJECT)" + CRLF )
      fWrite( s_nLinkHandle, "    $(PROJECTMAP)" + CRLF )
      fWrite( s_nLinkHandle, "    $(ALLLIB) " + CRLF )
      fWrite( s_nLinkHandle, "    $(DEFFILE) " + CRLF )
      fWrite( s_nLinkHandle, "    $(ALLRES) " + CRLF )
      fWrite( s_nLinkHandle, "! " + CRLF )
   ELSEIF s_lGcc
      fWrite( s_nLinkHandle, "#BUILD" + CRLF )
      fWrite( s_nLinkHandle, " " + CRLF )
      fWrite( s_nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)" + CRLF )

      IF 'Linux' IN cOs
         fWrite( s_nLinkHandle, "    $(LINKER) @&&!" + CRLF )
      ELSE
         fWrite( s_nLinkHandle, "    $(BCB)\bin\$(LINKER) @&&!" + CRLF )
      ENDIF

      fWrite( s_nLinkHandle, "    $(PROJECT) " + CRLF )
      fWrite( s_nLinkHandle, "    $(ALLOBJ)  " + CRLF )
      fWrite( s_nLinkHandle, "    $(LFLAGS)  " + CRLF )
      fWrite( s_nLinkHandle, "    $(ALLLIB)  " + CRLF )
      fWrite( s_nLinkHandle, "!" + CRLF )
   ENDIF

   IF s_nLang == 1 .OR. s_nLang == 3
      @ 19,5 Say "Compilar app ? (S/N) " get cBuild PICT "!" Valid cBuild $"NS"
   ELSE // English
      @ 19,5 Say "Build app ? (Y/N) " get cBuild PICT "!" Valid cBuild $"YN"
   ENDIF

   READ

   IF cBuild == "S" .OR. cBuild == "Y" 
      ResetInternalVars()
      SetColor("W/N,N/W")
      Clear
      Main( cFile, " -f")
   ELSE
      SetColor("W/N,N/W")
      Clear
   ENDIF



RETURN NIL

FUNCTION CompileUpdatedFiles()

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
   LOCAL aGauge      := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B", '≤' )

   @ 4,5 SAY "Compiling :"

   FOR EACH cOrder in aOrder
      IF ! s_lExtended

         IF cOrder == "$(CFILES)"
            nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".prg.c:" } )

            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ENDIF

            FOR nFiles := 1 TO Len( s_aPrgs )
               xItem   := Substr( s_aPrgs[ nFiles ], Rat( IIF( s_lGcc, '/', '\' ), s_aPrgs[ nFiles ] ) + 1 )
               nPos    := aScan( s_aCs, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( xItem, At( ".", xItem ) ) } )
               nObjPos := aScan( s_aObjs, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( xItem, At( ".", xItem ) ) } )

               IF Fileisnewer( s_aPrgs[ nFiles ], s_aObjs[ nObjPos ] )

                  IF nPos > 0
                     aAdd( aCtocompile, s_aCs[ nPos ] )
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
               nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".c.o:" .OR. x[ 1 ] == ".cpp.o:" } )
            ELSE
               nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".c.obj:" .OR. x[ 1 ] == ".cpp.obj:" } )
            ENDIF

            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ENDIF

            IF Len( aCtoCompile ) >= 1

               FOR nFiles := 1 TO Len( s_aCs )
                  nPos := aScan( s_aCs, { | x | Left( x, At( ".", x ) ) == Left( aCtoCompile[ nfiles ], At( ".", aCtoCompile[ nfiles ] ) ) } )

                  IF nPos == 0
                     aAdd( aCtoCompile, s_aCs[ nFiles ] )
                  ENDIF

               NEXT

            ENDIF

            FOR nFiles := 1 TO Len( aCtocompile )
               xItem := Substr( aCtocompile[ nFiles ], Rat( IIF( s_lGcc, '/', '\' ), aCtocompile[ nFiles ] ) + 1 )
               nPos  := aScan( s_aObjs, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( aCtocompile[ nFiles ], At( ".", xItem ) ) } )

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
            nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".c.obj:" .OR. x[ 1 ] == ".cpp.obj:" .or. x[ 1 ] == ".cpp.o:" .or. x[ 1 ] == ".c.o:"  } )

            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ELSE
               nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".C.OBJ:" } )

               IF nPos > 0
                  cComm := s_aCommands[ nPos, 2 ]
                  cOld  := cComm
               ENDIF

            ENDIF

            GaugeDisplay( aGauge )

            FOR nFiles := 1 TO Len( s_aCs )
               @  4, 16 SAY Space( 50 )
               xItem := Substr( s_aCs[ nFiles ], Rat( IIF( s_lGcc, '/', '\' ), s_aCs[ nFiles ] ) + 1 )
               nPos  := aScan( s_aObjsc, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( xitem, At( ".", xitem ) ) } )

               IF Fileisnewer( s_aCs[ nFiles ], s_aObjsc[ nPos ] )

                  IF nPos > 0
                     cComm := Strtran( cComm, "o$*", "o" + s_aObjsc[ nPos ] )
                     cComm := Strtran( cComm, "$**", s_aCs[ nFiles ] )
                     cComm += IIF( AT("LINUX" ,upper( Os() ) ) >0 ,  " > "+ (s_cLog)," >>"+ (s_cLog))
                     @  4, 16 SAY s_aCs[ nFiles ]
                     GaugeUpdate( aGauge, nFile / Len( s_aPrgs ) )
                     nFile ++
                     //                            Outstd( cComm )
                     //                            Outstd( Hb_OsNewLine() )
                     setpos(9,0)
                     __RUN( (cComm) )
                     cErrText := Memoread( (s_cLog) )
                     lEnd     := 'Error E' $ cErrText

                     IF ! s_lIgnoreErrors .AND. lEnd
                        IIF(  "LINUX" IN Upper( Os() ) , __run( "mcedit "  + (s_cLog)), __run( "Notepad " + (s_cLog) ) )
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
               nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".prg.o:" } )
            ELSE
               nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".prg.obj:" } )
            ENDIF
            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ELSE

               IF s_lGcc
                  nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".PRG.O:" } )
               ELSE
                  nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".PRG.OBJ:" } )
               ENDIF

            ENDIF

            nFile := 1

            FOR EACH cPrg IN s_aPrgs
               @  4, 16 SAY Space( 50 )
               xItem := Substr( cPrg, Rat( IIF( s_lGcc, '/', '\' ), cPrg ) + 1 )
               nPos  := aScan( s_aObjs, { | x | x := Substr( x, Rat( IIF( s_lGcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( xItem, At( ".", xitem ) ) } )

               IF !empty( cPrg ) .AND. Fileisnewer( cPrg, s_aObjs[ npos ] )

                  IF nPos > 0
                     cComm := Strtran( cComm, "o$*", "o" + s_aObjs[ nPos ] )
                     cComm := Strtran( cComm, "$**", cPrg )
                     cComm += IIF( AT("LINUX" ,upper( Os() ) ) >0 ,  " > "+ (s_cLog)," >>"+ (s_cLog))
                     @  4, 16 SAY cPrg
                     GaugeUpdate( aGauge, nFile / Len( s_aPrgs ) )

                     setpos(9,0)
                     __RUN( (cComm) )
                     cErrText := Memoread( (s_cLog) )
                     lEnd     := 'C2006' $ cErrText .OR. 'No code generated' $ cErrText .or. "Error E" $ cErrText .or. "Error F" $ cErrText

                     IF ! s_lIgnoreErrors .AND. lEnd
                        IIF( "LINUX" IN Upper( Os() ) , __run( "mcedit " + (s_cLog) ), __run( "Notepad "  + (s_cLog) ) )
                        QUIT
                     ELSE
                        //                                Ferase( (s_cLog) )
                     ENDIF

                     cComm := cOld

                  ENDIF

               ENDIF

            NEXT

            nFile ++

         ENDIF

      ENDIF

      IF cOrder == "$(RESDEPEN)"
         nPos := aScan( s_aCommands, { | x | x[ 1 ] == ".rc.res:" } )

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

FUNCTION Fileisnewer( cFile, as )

   LOCAL nCount := 0

   IF ! s_lExtended

      FOR nCount := 1 TO Len( s_aPrgs )
         s_aDir := { cFile,, Hbmake_Filedate( cFile ), hbmake_filetime( cFile ), ;
                   as[ nCount ], Hbmake_Filedate( as[ nCount ] ), hbmake_filetime( as[ nCount ] ) }

         IF Empty( s_aDir[ 7 ] )
            s_aDir[ 2 ] := .t.
         ELSE
            s_aDir[ 2 ] := td2jul( s_aDir[ 4 ], s_aDir[ 3 ] ) > td2jul( s_aDir[ 7 ], s_aDir[ 6 ] )
         ENDIF

      NEXT

   ELSE
      s_aDir := { cFile,, Hbmake_Filedate( cFile ), hbmake_filetime( cFile ), ;
                as, Hbmake_Filedate( as ), hbmake_filetime( as ) }

      IF Empty( s_aDir[ 7 ] )
         s_aDir[ 2 ] := .t.
      ELSE
         s_aDir[ 2 ] := td2jul( s_aDir[ 4 ], s_aDir[ 3 ] ) > td2jul( s_aDir[ 7 ], s_aDir[ 6 ] )
      ENDIF

   ENDIF

RETURN s_aDir[ 2 ]

FUNC CreateLibMakeFile( cFile )

   LOCAL aIn      := {}
   LOCAL aOut     := {}
   LOCAL aSrc     := Directory( "*.prg" )
   LOCAL nLenaSrc := Len( aSrc )
   LOCAL nLenaOut

   LOCAL aOutC     := {}
   LOCAL aSrcC     := Directory( "*.c" )
   LOCAL cOs       := IIF( "LINUX" IN UPPER( OS() ), "Linux", "Win32")
   LOCAL cCompiler := IIF( "LINUX" IN UPPER( OS() ), "GCC","BCC")
   LOCAL cFwhPath  := Left( cfile, At( '.', cfile ) - 1 ) + Space( 40 )

   LOCAL lAutomemvar     := .f.
   LOCAL lvarismemvar    := .f.
   LOCAL ldebug          := .f.
   LOCAL lSupressline    := .f.
   LOCAL cDefHarOpts     := ""
   LOCAL cObjDir         := 'obj' + Space( 20 )
   LOCAL lCompMod        := .f.
   LOCAL lInstallLibrary := .f.
   LOCAL x
   LOCAL y
   LOCAL nPos
   LOCAL lGenppo         := .f.
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

   IF nLenaSrc == 0
      IF s_nLang == 1 // Portuguese-BR
         Alert("N∆o h† prg na pasta "+curdir())
      ELSEIF s_nLang == 3 // Spanish
         Alert("No hay ning£n prg en la carpeta "+curdir())
      ELSE
         Alert("Have not any prg in "+curdir()+" folder.")
      ENDIF
      RETURN NIL
   ENDIF

   s_nLinkHandle := Fcreate( cFile )
   WriteMakeFileHeader()
   CLS
   Setcolor( 'w/b+,b+/w,w+/b,w/b+,w/b,w+/b' )
   @  0,  0, Maxrow(), Maxcol() BOX( Chr( 201 ) + Chr( 205 ) + Chr( 187 ) + Chr( 186 ) + Chr( 188 ) + Chr( 205 ) + Chr( 200 ) + Chr( 186 ) + Space( 1 ) )
   Attention( s_aLangMessages[ 27 ], 0 )

   @  1,  1 SAY "Select Os"
   @  1, 12 GET cos radio { "Win32", "OS/2", "Linux" }
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
   @ 10,  1 GET lInstallLibrary checkbox caption "Install Library to xharbour Lib Directory"
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
   s_lVcc    :=  "MSVC" IN cCompiler
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

   aEval( s_aMacros, { | x, y | cItem := Substr( x[ 2 ], 1, Len( x[ 2 ] ) ), IIF( At( citem, cTest ) > 0, ( s_aMacros[ y, 1 ] := 'OBJ', s_aMacros[ y, 2 ] := cObjDir ), ) } )

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
      aAdd( s_aCommands, { ".cpp.obj:", "$(BCB)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $**" } )
      aAdd( s_aCommands, { ".c.obj:", "$(BCB)\BIN\bcc32 -I$(BHC)\include $(CFLAG1) $(CFLAG2) -o$* $**" } )

      IF s_lExtended
         aAdd( s_aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -n -go -I$(BHC)\include $(HARBOURFLAGS) -I$(FWH)\include -o$* $**" } )
      ELSE
         aAdd( s_aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -I$(FWH)\include -o$* $**" } )
      ENDIF

      aAdd( s_aCommands, { ".rc.res:", "$(BCB)\BIN\brcc32 $(RFLAGS) $<" } )

   ELSEIF s_lGcc

      IF  "linux" IN Lower( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOs == "Linux"
         aAdd( s_aCommands, { ".cpp.o:", "gcc $(CFLAG1) $(CFLAG2) -o$* $**" } )
         aAdd( s_aCommands, { ".c.o:", "gcc -I/usr/include/xharbour $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )

         IF s_lExtended
            aAdd( s_aCommands, { ".prg.o:", "harbour -n $(HARBOURFLAGS) -I/usr/include/xharbour -I. -go  -o$* $**" } )
         ELSE
            aAdd( s_aCommands, { ".prg.c:", "harbour -n $(HARBOURFLAGS) -I/usr/include/xharbour -I.  -o$* $**" } )
         ENDIF

      ELSE
         aAdd( s_aCommands, { ".cpp.o:", "$(BCB)\bin\gcc $(CFLAG1) $(CFLAG2) -o$* $**" } )
         aAdd( s_aCommands, { ".c.o:", "$(BCB)\bin\gcc -I$(BHC)/include $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )

         IF s_lExtended
            aAdd( s_aCommands, { ".prg.o:", "$(BHC)\bin\harbour -n -go -I$(BHC)/include $(HARBOURFLAGS)  -o$* $**" } )
         ELSE
            aAdd( s_aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)/include $(HARBOURFLAGS)  -o$* $**" } )
         ENDIF

      ENDIF

   ELSEIF s_lVcc
      aAdd( s_aCommands, { ".cpp.obj:", "$(BCB)\bin\cl $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
      aAdd( s_aCommands, { ".c.obj:", "$(BCB)\bin\cl -I$(BHC)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )

      IF s_lExtended
         aAdd( s_aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -go -n -I$(BHC)\include $(HARBOURFLAGS) -I$(C4W)\include -o$* $**" } )
      ELSE
         aAdd( s_aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -I$(C4W)\include -o$* $**" } )
      ENDIF

      aAdd( s_aCommands, { ".rc.res:", "$(BCB)\BIN\rc $(RFLAGS) $<" } )

   ENDIF

   Attention( 'Spacebar to select, Enter to continue process', 22 )

   IF ! s_lRecurse
      aIn      := GetSourceFiles( .f., s_lGcc, cOs )
      nLenaSrc := Len( aIn )
   ELSE
      aIn      := GetSourceFiles(, s_lGcc, cOs )
      nLenaSrc := Len( aIn )
   ENDIF

   aOut := aClone( aIn )
   pickarry( 10, 15, 19, 64, aIn, aOut ,{}, .T.)
   nLenaOut := Len( aOut )

   aEval( aout, { | x, y | aout[ y ] := Trim( Substr( aOut[ y ], 1, At( ' ', aout[ y ] ) ) ) } )
   aEval( aout, { | xItem | IIF( At( '.c', xItem ) > 0 .OR. At( '.C', xItem ) > 0 .OR. At( '.cpp', xItem ) > 0 .OR. At( '.CPP', xItem ) > 0, aAdd( aoutc, xitem ), ) } )
   aEval( aoutc, { | x, z | citem := x, z := aScan( aout, { | t | t = citem } ), IIF( z > 0, aSize( aDel( aout, z ), Len( aout ) - 1 ), ) } )

   aOut  := aSort( aOut )
   s_aPrgs := aClone( aout )

   s_aObjs := aClone( aout )
   aEval( s_aObjs, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, s_aObjs[ x ] := cObjDir + cTest + "." + Exte( cExt, 2 ), s_aObjs[ x ] := cObjDir + cTest + "." + Exte( cExt, 3 ) ) } )
   s_aCs := aClone( aoutc )

   IF ! s_lExtended
      aEval( aOutc, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, aAdd( s_aObjs, cObjDir + cTest + "." + Exten( cExt, 2 ) ), aAdd( s_aObjs, cObjDir + cTest + "." + Exten( cExt, 1 ) ) ) } )
      aEval( aout, { | xItem | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cExt := Substr( cExt, 2 ), aAdd( s_aCs, cObjDir + cTest + "." + Exte( cExt, 1 ) ) } )
   ELSE
      s_aObjsc := aClone( aoutc )
      aEval( aoutc, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, s_aObjsc[ x ] := cObjDir + cTest + "." + Exten( cExt, 2 ), s_aObjsc[ x ] := cObjDir + cTest + "." + Exten( cExt, 1 ) ) } )
   ENDIF

   FOR x := 1 TO Len( s_aMacros )

      IF ! Empty( s_aMacros[ x, 2 ] )
         cItem := s_aMacros[ x, 2 ]
         nPos  := aScan( s_aPrgs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0
            aEval( s_aPrgs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aPrgs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )

            IF ! s_aMacros[ x, 3 ]
               fWrite( s_nLinkHandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
               s_aMacros[ x, 3 ] := .t.
            ENDIF

         ENDIF

         nPos := aScan( s_aCs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0
            aEval( s_aCs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aCs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )

            IF ! s_aMacros[ x, 3 ]
               fWrite( s_nLinkHandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
               s_aMacros[ x, 3 ] := .t.
            ENDIF

         ENDIF

         nPos := aScan( s_aObjs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0

            IF ! Empty( cObjDir )
               aEval( s_aObjs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aObjs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )
               fWrite( s_nLinkHandle, s_aMacros[ x, 1 ] + ' = ' + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + CRLF )
            ENDIF

         ENDIF

         IF s_lExtended
            nPos := aScan( s_aObjsc, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

            IF nPos > 0

               IF ! Empty( cObjDir )
                  aEval( s_aObjsc, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aObjsc[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ')\' ) ), ) } )
               ENDIF

            ENDIF

         ENDIF

      ENDIF

   NEXT

   IF s_lGcc

      IF  "linux" IN Lower( Getenv( "HB_ARCHITECTURE" ) ) .OR. cOs == "Linux"
         fWrite( s_nLinkHandle, "PROJECT = " + IIF( lInstallLibrary, "$(BHC)\lib\", "" ) + Alltrim( cFwhPath ) + ".a " + CRLF )
      ELSE
         fWrite( s_nLinkHandle, "PROJECT = " + IIF( lInstallLibrary, "$(BHC)\lib\", "" ) + Alltrim( Lower( cFwhPath ) ) + ".a " + CRLF )
      ENDIF

   ELSE
      fWrite( s_nLinkHandle, "PROJECT = " + IIF( lInstallLibrary, "$(BHC)\lib\", "" ) + Alltrim( Lower( cFwhPath ) ) + ".lib " + CRLF )
   ENDIF

   IF ! s_lExtended
      nWriteFiles := 0
      fWrite( s_nLinkHandle, "OBJFILES =" )

      IF Len( s_aObjs ) < 1
         fWrite( s_nLinkHandle, + " $(OB) " + CRLF )
      ELSE
         nWriteFiles := 0
         aEval( s_aObjs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aObjs ), fWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), fWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )
      ENDIF

      nWriteFiles := 0
      fWrite( s_nLinkHandle, "CFILES =" )

      IF Len( s_aCs ) < 1
         fWrite( s_nLinkHandle, + " $(CF)" + CRLF )
      ELSE
         aEval( s_aCs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aCs ), fWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), fWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(CF) " + CRLF ) ) } )
      ENDIF

      fWrite( s_nLinkHandle, "PRGFILE =" )
      nWriteFiles := 0

      IF Len( s_aPrgs ) < 1
         fWrite( s_nLinkHandle, + " $(PS)" + CRLF )
      ELSE
         aEval( s_aPrgs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aPrgs ), fWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), fWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )
      ENDIF

   ELSE /****extended moded ****/
      fWrite( s_nLinkHandle, "OBJFILES =" )
      nWriteFiles := 0

      IF Len( s_aObjs ) < 1
         fWrite( s_nLinkHandle, + " $(OB) " + CRLF )
      ELSE
         aEval( s_aObjs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aObjs ), fWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), fWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )
      ENDIF

      fWrite( s_nLinkHandle, "PRGFILES =" )
      nWriteFiles := 0

      IF Len( s_aPrgs ) < 1
         fWrite( s_nLinkHandle, + " $(PS)" + CRLF )
      ELSE
         aEval( s_aPrgs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aPrgs ), fWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), fWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )
      ENDIF

      nWriteFiles := 0

      IF Len( s_aObjsc ) > 0
         fWrite( s_nLinkHandle, "OBJCFILES =" )

         IF Len( s_aObjsc ) < 1
            fWrite( s_nLinkHandle, + " $(OBC) " + CRLF )
         ELSE
            aEval( s_aObjsc, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aObjsc ), fWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), fWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(OBC) " + CRLF ) ) } )
         ENDIF

      ENDIF

      nWriteFiles := 0

      IF Len( s_aCs ) > 0
         fWrite( s_nLinkHandle, "CFILES =" )

         IF Len( s_aCs ) < 1
            fWrite( s_nLinkHandle, + " $(CF)" + CRLF )
         ELSE
            aEval( s_aCs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aCs ), fWrite( s_nLinkHandle, ' ' + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), fWrite( s_nLinkHandle, " " + Alltrim( x ) + " $(CF) " + CRLF ) ) } )
         ENDIF

      ENDIF

   ENDIF

   fWrite( s_nLinkHandle, "RESFILES =" + CRLF )
   fWrite( s_nLinkHandle, "RESDEPEN = $(RESFILES)" + CRLF )
   fWrite( s_nLinkHandle, "DEFFILE = " + CRLF )
   fWrite( s_nLinkHandle, "HARBOURFLAGS = " + cDefHarOpts + CRLF )

   IF s_lBcc
      fWrite( s_nLinkHandle, "CFLAG1 =  -OS $(CFLAGS) -d -L$(BHC)\lib;$(FWH)\lib -c" + CRLF )
      fWrite( s_nLinkHandle, "CFLAG2 =  -I$(BHC)\include -I$(BCB)\include -I" + Alltrim( s_cUserInclude ) + CRLF )
      fWrite( s_nLinkHandle, "RFLAGS = " + CRLF )
      fWrite( s_nLinkHandle, "LFLAGS = /P32 /0" + CRLF )
      fWrite( s_nLinkHandle, "IFLAGS = " + CRLF )
      fWrite( s_nLinkHandle, "LINKER = tlib $(LFLAGS) $(PROJECT)" + CRLF )
      fWrite( s_nLinkHandle, " " + CRLF )
      fWrite( s_nLinkHandle, "ALLOBJ =  $(OBJFILES) $(OBJCFILES)" + CRLF )
      fWrite( s_nLinkHandle, "ALLRES = $(RESDEPEN)" + CRLF )
      fWrite( s_nLinkHandle, "ALLLIB = " + CRLF )
      fWrite( s_nLinkHandle, ".autodepend" + CRLF )
   ELSEIF s_lVcc
      fWrite( s_nLinkHandle, "CFLAG1 =  -I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(CFLAGS)" + CRLF )
      fWrite( s_nLinkHandle, "CFLAG2 =  -c -I" + Alltrim( s_cUserInclude ) + CRLF )
      fWrite( s_nLinkHandle, "RFLAGS = " + CRLF )
      fWrite( s_nLinkHandle, "LFLAGS = " + CRLF )
      fWrite( s_nLinkHandle, "IFLAGS = " + CRLF )
      fWrite( s_nLinkHandle, "LINKER = lib $(PROJECT)" + CRLF )
      fWrite( s_nLinkHandle, " " + CRLF )
      fWrite( s_nLinkHandle, "ALLOBJ = $(OBJFILES) $(OBJCFILES) " + CRLF )
      fWrite( s_nLinkHandle, "ALLRES = $(RESDEPEN)" + CRLF )
      fWrite( s_nLinkHandle, "ALLLIB = " + CRLF )
   ELSEIF s_lGcc
      fWrite( s_nLinkHandle, "CFLAG1 = " + IIF( "linux" IN Lower( Os() ) , "-I/usr/include/xharbour", " -I$(BHC)/include" ) + " -c -Wall" + CRLF )
      fWrite( s_nLinkHandle, "CFLAG2 = " + IIF( "linux" IN Lower( Os() ) , "-L /usr/lib/xharbour", " -L $(BHC)/lib" ) + CRLF )
      fWrite( s_nLinkHandle, "RFLAGS = " + CRLF )
      fWrite( s_nLinkHandle, "LFLAGS = " + CRLF )
      fWrite( s_nLinkHandle, "IFLAGS = " + CRLF )

      IF "linux" IN Lower( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOs == "Linux" .OR.  "linux" IN Lower( Os() )
         fWrite( s_nLinkHandle, "LINKER = ar -M " + CRLF )
      ELSE
         fWrite( s_nLinkHandle, "LINKER = ar -M " + CRLF )
      ENDIF

      fWrite( s_nLinkHandle, " " + CRLF )
      fWrite( s_nLinkHandle, "ALLOBJ = $(OBJFILES) $(OBJCFILES) " + CRLF )
      fWrite( s_nLinkHandle, "ALLRES = $(RESDEPEN) " + CRLF )
      fWrite( s_nLinkHandle, "ALLLIB = $(LIBFILES) " + CRLF )
      fWrite( s_nLinkHandle, ".autodepend" + CRLF )
   ENDIF

   fWrite( s_nLinkHandle, " " + CRLF )
   fWrite( s_nLinkHandle, "#COMMANDS" + CRLF )
   aEval( s_aCommands, { | xItem | fWrite( s_nLinkHandle, xitem[ 1 ] + CRLF ), fWrite( s_nLinkHandle, xitem[ 2 ] + CRLF ), fWrite( s_nLinkHandle, " " + CRLF ) } )

   IF s_lBcc
      fWrite( s_nLinkHandle, "#BUILD" + CRLF )
      fWrite( s_nLinkHandle, " " + CRLF )
      fWrite( s_nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES)" + CRLF )
      fWrite( s_nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!" + CRLF )
      fWrite( s_nLinkHandle, "    $(ALLOBJ)" + CRLF )
      fWrite( s_nLinkHandle, "!" + CRLF )
   ELSEIF s_lVcc
      fWrite( s_nLinkHandle, "#BUILD" + CRLF )
      fWrite( s_nLinkHandle, "" + CRLF )
      fWrite( s_nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES)" + CRLF )
      fWrite( s_nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!" + CRLF )
      fWrite( s_nLinkHandle, "    $(ALLOBJ) " + CRLF )
      fWrite( s_nLinkHandle, "!" + CRLF )
   ELSEIF s_lGcc
      fWrite( s_nLinkHandle, "#BUILD" + CRLF )
      fWrite( s_nLinkHandle, " " + CRLF )
      fWrite( s_nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) " + CRLF )

      IF  "linux" IN Lower( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOs == "Linux"
         fWrite( s_nLinkHandle, "    $(LINKER) @&&!" + CRLF )
      ELSE
         fWrite( s_nLinkHandle, "    $(BCB)\bin\$(LINKER) @&&!" + CRLF )
      ENDIF

      fWrite( s_nLinkHandle, "    $(PROJECT) " + CRLF )
      fWrite( s_nLinkHandle, "    $(ALLOBJ)  " + CRLF )
      fWrite( s_nLinkHandle, "!" + CRLF )
   ENDIF

RETURN NIL

FUNCTION setlibBuild()

   LOCAL cRead as String
   LOCAL nPos as Numeric
   LOCAL aMacro as Array
   LOCAL aTemp as Array
   LOCAL nCount as Numeric
   LOCAL aCurobjs as Array
   LOCAL nObjPos as Numeric
   LOCAL cLib

   cRead       := Alltrim( readln( @s_lEof ) )
   s_nLinkHandle := Fcreate( s_cLinker )
   s_szProject   := cRead
   aMacro      := ListAsArray2( cRead, ":" )

   IF Len( aMacro ) > 1
      aTemp := ListAsArray2( aMacro[ 2 ], " " )
      aEval( aTemp, { | xItem | aAdd( s_aBuildOrder, xItem ) } )
   ENDIF

   aAdd( s_aBuildOrder, aMacro[ 1 ] )
   cRead  := Strtran( cRead, "@&&!", "" )
   aMacro := ListAsArray2( cRead, '\' )
   aEval( aMacro, { | xMacro | Findmacro( xMacro, @cRead ) } )

   IF s_lBcc .OR. s_lVcc
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
               fWrite( s_nLinkHandle, "CREATE " + " lib" + cRead + CRLF )
               cLib := "lib" + cRead
            ELSEIF ( aMacro[ nCount ] == "$(ALLOBJ)" )
               Findmacro( aMacro[ nCount ], @cRead )
               aCurObjs := ListAsArray2( cRead, " " )

               FOR nObjPos := 1 TO Len( aCurObjs )

                  IF s_lGcc
                     fWrite( s_nLinkHandle, "ADDMOD " + aCurObjs[ nObjPos ] + CRLF )
                  ENDIF

                  IF s_lBcc .OR. s_lVcc

                     IF nObjPos < Len( aCurObjs )
                        fWrite( s_nLinkHandle, "+-" + aCurObjs[ nObjPos ] + " &" + CRLF )
                     ELSE
                        fWrite( s_nLinkHandle, "+-" + aCurObjs[ nObjPos ] + CRLF )
                     ENDIF

                  ENDIF

               NEXT

            ENDIF

         ENDIF

      NEXT

   NEXT

   IF s_lGcc
      fWrite( s_nLinkHandle, "SAVE" + CRLF )
      fWrite( s_nLinkHandle, "END " + CRLF )
   ENDIF

   fClose( s_nLinkHandle )

   IF s_lLinux
      s_cLinkComm += " || rm -f " + cLib
   ENDIF

RETURN NIL

FUNC FindCfile( citem, aSrcc )

   LOCAL nRETURNPos := 0

   nRETURNPos := aScan( aSrcc, { | x | Lower( x[ 1 ] ) == cItem } )

   RETURN nRETURNPos

#IFndef __HARBOUR__
FUNCTION Hb_OsNewLine()
   RETURN Chr( 13 ) + Chr( 10 )
#ENDIF

FUNCTION CheckIFfile( cFile )

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

FUNCTION Checkstdout( cText )

   cText := Strtran( cText, "!stdout", "" )
   Outstd( cText )

RETURN NIL

FUNCTION CheckIFdef( cTemp )

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
         nPos := aScan( s_aDefines, { | x | x[ 1 ] == cTemp } )

         IF nPos > 0
            cRead    := Alltrim( Strtran( aSet[ 1 ], "$(", "" ) )
            cRead    := Strtran( cRead, ")", "" )
            nMakePos := aScan( s_aMacros, { | x | x[ 1 ] == cRead } )

            IF nMakePos == 0
               aAdd( s_aMacros, { aSet[ 1 ], aSet[ 2 ] } )
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
                     aAdd( s_aMacros, { aSet[ 1 ], aSet[ 2 ] } )
                   ENDDO

               ENDIF

            ENDDO

         ENDIF

      ELSEIF '!stdout' IN cRead
         Checkstdout( cRead )
      ENDIF

   ENDDO

RETURN NIL

FUNCTION BuildBorCfgFile()

   LOCAL nCfg

   IF ! File( GetMakeDir() + '\bin\harbour.cfg' )
      nCfg := Fcreate( GetMakeDir() + '\bin\harbour.cfg' )
      fWrite( nCfg, "CC=BCC32" + CRLF )
      fWrite( nCfg, "CFLAGS= -c " + Replacemacros( "-I$(BHC)\include -OS $(CFLAGS) -d -L$(BHC)\lib" ) + CRLF )
      fWrite( nCfg, "VERBOSE=NO" + CRLF )
      fWrite( nCfg, "DELTMP=YES" + CRLF )
      fClose( nCfg )
   ENDIF

RETURN NIL

FUNCTION Buildmsccfgfile()

   LOCAL nCfg

   IF ! File( GetMakeDir() + '\bin\harbour.cfg' )
      nCfg := Fcreate( GetMakeDir() + '\bin\harbour.cfg' )
      fWrite( nCfg, "CC=cl" + CRLF )
      fWrite( nCfg, "CFLAGS= -c " + Replacemacros( "-I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(CFLAGS)" ) + CRLF )
      fWrite( nCfg, "VERBOSE=NO" + CRLF )
      fWrite( nCfg, "DELTMP=YES" + CRLF )
      fClose( nCfg )
   ENDIF

RETURN NIL

FUNCTION Buildgcccfgfile()

   LOCAL nCfg
   LOCAL cDir := GetMakeDir()
   LOCAL cBhc := Alltrim( Strtran( replacemacros( '$(BHC)' ), '\', '/' ) )

   cDir := Strtran( cDir, '/', '\' )

   IF ! File( cdir + '\bin\harbour.cfg' )
      nCfg := Fcreate( cdir + '\bin\harbour.cfg' )
      fWrite( nCfg, "CC=gcc" + CRLF )
      fWrite( nCfg, "CFLAGS= -c " + Replacemacros( "-I" + cBhc + "/include $(C_USR)  -L" + cBhc + "/lib" ) + CRLF )
      fWrite( nCfg, "VERBOSE=NO" + CRLF )
      fWrite( nCfg, "DELTMP=YES" + CRLF )
      fClose( nCfg )
   ENDIF

RETURN NIL

FUNCTION BuildGccCfgFileL()

   LOCAL nCfg

   IF ! File( '/etc/harbour.cfg' )
      nCfg := Fcreate( '/etc/harbour.cfg' )
      fWrite( nCfg, "CC=gcc" + CRLF )
      fWrite( nCfg, "CFLAGS= -c -I/usr/include/xharbour" + CRLF )
      fWrite( nCfg, "VERBOSE=YES" + CRLF )
      fWrite( nCfg, "DELTMP=YES" + CRLF )
      fClose( nCfg )
   ENDIF

RETURN NIL

FUNCTION Findharbourcfg( cCfg )

   LOCAL cPath AS STRING := ''
   LOCAL lFound AS LOGICAL := .f.
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
         lFound := .t.
         cPath  := '/etc/harbour.cfg'
      ENDIF

      IF ! lfound

         IF File( '/usr/local/etc/harbour.cfg' )
            lFound := .t.
            cPath  := '/usr/local/etc/harbour.cfg'
         ENDIF

      ENDIF

   ENDIF

   cCfg := cPath

RETURN lFound

FUNCTION TestforPrg( cFile )

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
      aAdd( aFiles, cItem )
   NEXT

   FOR nFiles := 1 TO Len( aFiles )
      nPos := aScan( aDir, { | a | a[ 1 ] == aFiles[ nFiles ] } )

      IF nPos > 0
         aAdd( s_aPrgs, aFiles[ nFiles ] )
      ENDIF

   NEXT

RETURN NIL

FUNCTION GetGccDir()

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

FUNCTION ConvertParams( cFile, aFile, p1, p2, p3, p4, p5, p6 )

   LOCAL cParam := ""

   IF ! Empty( cFile )

      IF Left( cFile, 1 ) $ "- /?"
         cParam += cFile+" "
      ELSE
         cFile := cFile
         aAdd( aFile, cFile )
      ENDIF

   ENDIF

   IF ! Empty( p1 )

      IF Left( p1, 1 ) $ "- /?"
         cParam += p1+" "
      ELSE
         cFile := p1
         aAdd( aFile, cFile )
      ENDIF

   ENDIF

   IF ! Empty( p2 )

      IF Left( p2, 1 ) $ "- /"
         cParam += p2+" "
      ELSE
         cFile := p2
         aAdd( aFile, cFile )
       ENDIF

   ENDIF

   IF ! Empty( p3 )

      IF Left( p3, 1 ) $ "- /"
         cParam += p3+" "
      ELSE
         cFile := p3
         aAdd( aFile, cFile )
      ENDIF

   ENDIF

   IF ! Empty( p4 )

      IF Left( p4, 1 ) $ "- /"
         cParam += p4+" "
      ELSE
         cFile := p4
         aAdd( aFile, cFile )
      ENDIF

   ENDIF

   IF ! Empty( p5 )

      IF Left( p5, 1 ) $ "- /"
         cParam += p5+" "
      ELSE
         cFile := p5
         aAdd( aFile, cFile )
      ENDIF

   ENDIF

   IF ! Empty( p6 )

      IF Left( p6, 1 ) $ "- /"
         cParam += p6+" "
      ELSE
         cFile := p6
         aAdd( aFile, cFile )
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

FUNCTION ShowHelp()

   LOCAL cOs := Upper( Os() )

   ? s_aLangMessages[ 1 ]
   ? "Copyright 2000,2001,2002,2003 Luiz Rafael Culik <culikr@uol.com.br>"
   ? ""
   ? s_aLangMessages[ 2 ]
   ? ""
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

FUNCTION ProcessParameters( cParams )

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
      s_lVcc    := .F.
      cParams := Strtran( cParams, "-B", "" )
   ENDIF

   IF  "-GL" IN cParams
      s_lBcc    := .F.
      s_lGcc    := .T.
      s_lVcc    := .F.
      s_lLinux  := .T.
      cParams := Strtran( cParams, "-GL", "" )
   ENDIF

   IF "-G" IN cParams
      s_lBcc    := .F.
      s_lGcc    := .T.
      s_lVcc    := .F.
      cParams := Strtran( cParams, "-G", "" )
   ENDIF

   IF "-V" IN cParams
      s_lBcc    := .F.
      s_lGcc    := .F.
      s_lVcc    := .T.
      cParams := Strtran( cParams, "-V", "" )
   ENDIF

   IF  "-I" IN cParams
      s_lIgnoreErrors := .T.
      cParams       := Strtran( cParams, "-I", "" )
   ENDIF


   IF  "-P" IN cParams
      s_lPrint  := .t.
      cParams := Strtran( cParams, "-P", "" )
   ENDIF

   IF  "-D" IN cParams
      cParams := "-D" + Strtran( cParams, "-D", ";" )
      cParams := Strtran( cParams, "-D;", "-D" )

      aDef := ListAsArray2( Alltrim( Substr( cParams, 3 ) ), ";" )
      aEval( aDef, { | xDef | IIF( At( '=', xDef ) > 0, GetParaDefines( xDef ), ) } )
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

FUNCTION WriteMakeFileHeader()

   fWrite( s_nLinkHandle, "#BCC" + CRLF )
   fWrite( s_nLinkHandle, "VERSION=BCB.01" + CRLF )
   fWrite( s_nLinkHandle, "!ifndef BCB" + CRLF )
   fWrite( s_nLinkHandle, "BCB = $(MAKEDIR)" + CRLF )
   fWrite( s_nLinkHandle, "!endif" + CRLF )
   fWrite( s_nLinkHandle, CRLF )
   fWrite( s_nLinkHandle, "!ifndef BHC" + CRLF )
   fWrite( s_nLinkHandle, "BHC = $(HMAKEDIR)" + CRLF )
   fWrite( s_nLinkHandle, "!endif" + CRLF )
   fWrite( s_nLinkHandle, " " + CRLF )
   fWrite( s_nLinkHandle, "RECURSE=" + IIF( s_lRecurse, " YES ", " NO " ) + CRLF )
   fWrite( s_nLinkHandle, " " + CRLF )

RETURN NIL

FUNCTION BuildLangArray( cLang )

   LOCAL aLang := {}

   DEFAULT cLang TO "EN"

   IF cLang == "EN"
      aAdd( aLang, "Harbour Make Utility" )
      aAdd( alang, "Syntax:  hbmake cFile [options]" )
      aAdd( aLang, "Options:  /e[x]  Create a new Makefile. IF /ex is" )
      aAdd( aLang, "          used it creates a new make file in extended mode" )
      aAdd( aLang, "          /el[x]  Create a new Makefile. IF /elx is" )
      aAdd( aLang, "          used it creates a new make file to build a library in extended mode" )
      aAdd( aLang, "          /D  Define a macro" )
      aAdd( aLang, "          /p  Print all commands and depedencies" )
      aAdd( aLang, "          /b  Use BCC as C compiler" )
      aAdd( aLang, "          /g+ Use GCC as C compiler" )
      aAdd( aLang, "          /b+ Use BCC as C compiler" )
      aAdd( aLang, "          /g  Use GCC as C compiler" )
      aAdd( aLang, "          /gl Use GCC as C compiler in Linux" )
      aAdd( aLang, "          /gl+ Use GCC as C compiler in Linux" )
      aAdd( aLang, "          /v  Use MSVC as C compiler" )
      aAdd( aLang, "          /f  Force recompiltion of all files" )
      aAdd( aLang, "          /i  Ignore errors RETURNed by command" )
      aAdd( aLang, "          /r  Recurse Source Directory" )
      aAdd( aLang, "          Note: /p and /D can be used together" )
      aAdd( aLang, "          Note: /r and /e[x]/el[x] can be used together" )
      aAdd( aLang, "          Options with + are the default values" )
      aAdd( aLang, "          -D switch can accept multiple macros on the same line" )
      aAdd( aLang, "          or use one macro per -D switch" )
      aAdd( aLang, "          /l[LANGID] SpecIFy the language to be used on hbmake Texts LANGID = (EN/PT/ES) " )
      aAdd( aLang, "          On Windows System, the default will be the SO language IF is found" )
      aAdd( aLang, "          Otherwise, will be English. On OS/2;FreeBSD/LINUX the default is English" )
      aAdd( aLang, "Enviroment options" )
      aAdd( aLang, "Select Os" )
      aAdd( aLang, "Select C Compiler" )
      aAdd( aLang, "Graphic Library" )
      aAdd( aLang, "Harbour Options" )
      aAdd( aLang, "Automatic memvar declaration /a" )
      aAdd( aLang, "Variables are assumed M-> /v" )
      aAdd( aLang, "Debug info /b" )
      aAdd( aLang, "Suppress line number information /l" )
      aAdd( aLang, "Generate pre-processed output /p" )
      aAdd( aLang, "compile module only /m" )
      aAdd( aLang, "User Defines " )
      aAdd( aLang, "User include Path" )
      aAdd( aLang, "Use External Libs" )
      aAdd( aLang, "Spacebar to select, Enter to continue process F5 sel/unsel All" )
      aAdd( aLang, "Warning level /w" )
      aAdd( aLang, "Numbers of source files per line on makefile" )
      aAdd( aLang, "Use Multi Thread Library" )
      aAdd( aLang, "Executable file name" )
      aAdd( aLang, "Warning Level /w" )
      aadd( aLang, "Enter Select|Arrow Change Selection|Spacebar Open Box")
      aadd( aLang, "3rd Partie Rdd")
      aadd( aLang, "What OS you Use")
      aadd( aLang, "What C compiler  you has")
      aadd( aLang, "This app use Graphical libraries")
      aadd( aLang, "Do you use 3rd Parties Rdd")
      aAdd( aLang, "Compress this app")
      aAdd( aLang, "Compress the app after Linked(use upx)")
      aAdd( aLang, "Your app will be linked to user harbour.dll")
      aadd( aLang, "Where the .obj/.o files will be generates")
      aadd( aLang, "Inform executable name (without .exe extention)" )

   ELSEIF cLang == "ES"
      aAdd( aLang, "Harbour Make Utility  -  Programa Make de Harbour" )
      aAdd( aLang, "Sintaxis:  hbmake cArchivo [opciones]" )
      aAdd( aLang, "Opciones:  /e[x]  Crea un Makefile nuevo. Si se usa /ex" )
      aAdd( aLang, "          se crea un nuevo makefile en modo extendido" )
      aAdd( aLang, "          /el[x]  Crea un Makefile nuevo. Si se usa /elx" )
      aAdd( aLang, "          se crea un nuevo makefile para construir una librerÌa en modo extendido" )
      aAdd( aLang, "          /D  Define una macro" )
      aAdd( aLang, "          /p  Imprime todos los comandos y dependencias" )
      aAdd( aLang, "          /b  Usar BCC como compilador C" )
      aAdd( aLang, "          /g+ Usar GCC como compilador C" )
      aAdd( aLang, "          /b+ Usar BCC como compilador C" )
      aAdd( aLang, "          /g  Usar GCC como compilador C" )
      aAdd( aLang, "          /gl Usar GCC como compilador C en Linux" )
      aAdd( aLang, "          /gl+ Usar GCC como compilador C en Linux" )
      aAdd( aLang, "          /v  Usar MSVC como compilador C" )
      aAdd( aLang, "          /f  Forzar la recompilaciÛn de todos los archivos" )
      aAdd( aLang, "          /i  Ignorar los errores devueltos por el comando" )
      aAdd( aLang, "          /r  Recorrer el directorio fuente recursivamente" )
      aAdd( aLang, "          Nota: /p y /D pueden ser usados juntos" )
      aAdd( aLang, "          Nota: /r y /e[x]/el[x] pueden ser usados juntos" )
      aAdd( aLang, "          Las opciones con + son los valores por omisiÛn" )
      aAdd( aLang, "          El par·metro -D puede aceptar m˙ltiples macros en la misma lÌnea" )
      aAdd( aLang, "          ou use uma macro por parÉmetro -D" )
      aAdd( aLang, "          /l[LANGID] especIFica a linguagem a ser utilizada nos textos do hbmake LANGID = (EN/PT/ES) " )
      aAdd( aLang, "          Em sistemas Windows, O padr∆o e a linguagem do SO se encontrada" )
      aAdd( aLang, "          Sen∆o, sera Ingles. Em OS/2;FreeBSD/LINUX o padr∆o Ç Ingles" )
      aAdd( aLang, "Opciones de Ambiente" )
      aAdd( aLang, "Selecion Os" )
      aAdd( aLang, "Selecion Compilador C" )
      aAdd( aLang, "Lib Gr†fica" )
      aAdd( aLang, "Opciones do xHarbour" )
      aAdd( aLang, "Declaracion Automatica de memvar /a" )
      aAdd( aLang, "Variables ser†n assumidas M-> /v " )
      aAdd( aLang, "Info. Debug /b" )
      aAdd( aLang, "Suprime info. de numero da linha /l" )
      aAdd( aLang, "Gera salida pre-processada /p" )
      aAdd( aLang, "Compila solamente o modulo /m" )
      aAdd( aLang, "Define de usu†rios:" )
      aAdd( aLang, "Path p/ includes de usu†rio:" )
      aAdd( aLang, "Usar Libs Externas" )
      aAdd( aLang, "<Espaáo> para selecionar, <Enter> p/ continuar processo,F5 sel/desel todos" )
      aAdd( aLang, "N°vel de aviso do compilador /w" )
      aAdd( aLang, "Qtd de PRGs por linea, no makefile:" )

      aAdd( aLang, "Use a libreria Multi Thread" )
      aAdd( aLang, "Nome Executable" )
      aAdd( aLang, "Nivel Warning /w" )
      aadd( aLang, "Enter Seleciona|Setas muda seleá∆o|Espaáo abre caixa")
      aadd( aLang, "Rdd Terceros")
      aadd( aLang, "Qual OS usted usa")
      aadd( aLang, "Qual compildor C usted ten")
      aadd( aLang, "Esta App usa Lib Grafica o No")
      aadd( aLang, "Usted usa Rdd de terceros")
      aAdd( aLang, "Comprimir app")
      aAdd( aLang, "Comprimir la app despois de enlazada (usar upx)")
      aAdd( aLang, "Su aplicacione sera linkada para usar la harbour.dll")
      aadd( aLang, "Donde los ficheros .obj/.o ser†n generados")

      aadd( aLang, "Informe o nombre de lo executable (sin a extension .exe)")

   ELSEIF cLang == "PT"
      aAdd( aLang, "Harbour Make Utility  -  Programa Make do Harbour" )
      aAdd( aLang, "Sintaxis:  hbmake cArquivo [opá‰es]" )
      aAdd( aLang, "Opá‰es:  /e[x]  Cria um Makefile novo. Se for usado /ex" )
      aAdd( aLang, "          cria um novo makefile em modo extendido" )
      aAdd( aLang, "          /el[x]  cria un Makefile novo. Se for usado /elx" )
      aAdd( aLang, "          cria um novo makefile para construir una Biblioteca em modo extendido" )
      aAdd( aLang, "          /D  Define uma macro" )
      aAdd( aLang, "          /p  Imprime todos los comandos e dependàncias" )
      aAdd( aLang, "          /b Usar BCC como compilador C" )
      aAdd( aLang, "          /g+ Usar GCC como compilador C" )
      aAdd( aLang, "          /b+ Usar BCC como compilador C" )
      aAdd( aLang, "          /g  Usar GCC como compilador C" )
      aAdd( aLang, "          /gl Usar GCC como compilador C en Linux" )
      aAdd( aLang, "          /gl+ Usar GCC como compilador C en Linux" )
      aAdd( aLang, "          /v  Usar MSVC como compilador C" )
      aAdd( aLang, "          /f  Foráar a recompilaá∆o de todos os arquivos" )
      aAdd( aLang, "          /i  Ignora os errores devolvidos pelo comando" )
      aAdd( aLang, "          /r  Recorrer o diret¢rio fonte recursivamente" )
      aAdd( aLang, "          Nota: /p e /D podem ser usados juntos" )
      aAdd( aLang, "          Nota: /r e /e[x]/el[x] podem ser usados juntos" )
      aAdd( aLang, "          As opá‰es com + s∆o os valores padr∆o" )
      aAdd( aLang, "          O parÉmetro -D pode aceitar multiplas macros na mesma linha" )
      aAdd( aLang, "          ou use una macro por parÉmetro -D" )
      aAdd( aLang, "          /l[LANGID] especIFica a linguagem a ser utilizada nos textos do hbmake LANGID = (EN/PT/ES) " )
      aAdd( aLang, "          Em sistemas Windows, O padr∆o e a linguagem do SO se encontrada" )
      aAdd( aLang, "          Sen∆o, sera Ingles. Em OS/2;FreeBSD/LINUX o padr∆o Ç Ingles" )
      aAdd( aLang, "Opá‰es de Ambiente" )
      aAdd( aLang, "Seleá∆o Os" )
      aAdd( aLang, "Seleá∆o Compilador C" )
      aAdd( aLang, "Lib Graf°ca" )
      aAdd( aLang, "Opá‰es do Harbour" )
      aAdd( aLang, "Declaraá∆o Autom†tica de memvar /a" )
      aAdd( aLang, "Variaveis s∆o assumidas M-> /v" )
      aAdd( aLang, "Info. Debug /b" )
      aAdd( aLang, "Suprime a info. de numero da linha /l" )
      aAdd( aLang, "Gene Sa°da pre-processada /p" )
      aAdd( aLang, "Compile apenas o modulo /m" )
      aAdd( aLang, "User Defines " )
      aAdd( aLang, "User include Path" )
      aAdd( aLang, "Usa Libs Externas" )
      aAdd( aLang, "Espaáo para selecionar, Enter p/ continuar processo F5 sel/desel todos" )
      aAdd( aLang, "N°vel de Aviso do compilador /w" )
      aAdd( aLang, "Qtd de PRGs por linha, no makefile: " )
      aAdd( aLang, "Use a Biblioteca Multi Thread" )
      aAdd( aLang, "Nome Executavel" )
      aAdd( aLang, "Nivel Warning /w" )
      aAdd( aLang,"Enter Seleciona|Setas muda seleá∆o|Espaáo abre caixa")
      /* Messages Start Here */
      aAdd( aLang, "Rdd Terceiros")
      aAdd( aLang, "Qual OS vocà Usa")
      aAdd( aLang, "Qual compildor C vocà tem")
      aAdd( aLang, "Essa App usa Lib Grafica ou N∆o")
      aAdd( aLang, "Vocà usa Rdd's de terceiros")
      aAdd( aLang, "Comprimir app")
      aAdd( aLang, "Comprimir a app depois de linkada(usa upx)")
      aAdd( aLang, "Sua aplicaá∆o Sera linkada para usar a harbour.dll")
      aadd( aLang, "Onde os Arquivos .obj/.o ser∆o gerados")
      aadd( aLang, "Informe o nome do execut†vel (sem a extens∆o .exe)")
   ENDIF

RETURN aLang

FUNCTION GetSelFiles( aIn, aOut )

   LOCAL aRet  := {}
   LOCAL cItem
   LOCAL nPos

   FOR EACH cItem IN aIn

      nPos := aScan( aOut, { | x, y | x == Left( cItem, At( ' ', citem ) - 1 ) } )

      IF nPos > 0
         aAdd( aRet, cItem )
      ENDIF

   NEXT

RETURN aRet

FUNCTION ResetInternalVars()

   s_lPrint        := .f.
   s_aDefines      := {}
   s_aBuildOrder   := {}
   s_aCommands     := {}
   s_aMacros       := {}
   s_aPrgs         := {}
   s_aCs           := {}
   s_aObjs         := {}
   s_aObjsc        := {}
   s_lEof          := .F.
   s_aRes          := {}
   s_cLinker       := "makefile.lnk"
   s_cLinkComm     := ''
   s_lBcc          := .T.
   s_lGcc          := .F.
   s_lVcc          := .F.
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

Function OsSpec(GetList,nPos,cSpec)
   local oGet := GetList[nPos]
   local oControl
   oControl := oGet:Control
   IF oControl != NIL
      cSpec := oControl:GetData( oControl:Value )
//   keyboard chr(9)
   ENDIF

RETURN .T.

function CheckCompiler(cOs)

RETURN ( ("Win32" IN cOs) .or. ("Linux" In cOs) )
function SetthisLibs(AtempLibs)
Local c := ""
local n
For each n in AtempLibs
     c += "-l"
     c += Strtran( n, '.a', "" )
     c+= " "
NEXT
return c

function asdll(x)
Local y :=x
 x := !y
return .t.
