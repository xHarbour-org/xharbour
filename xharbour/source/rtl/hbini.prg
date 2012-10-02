/*
* $Id$
*/

/*
* xHarbour Project source code:
* HB_ReadIni - Reading .ini files
*
* Copyright 2002 Giancarlo Niccolai [gian@niccolai.ws]
* www - http://www.xharbour.org
*
* This small procedure reads a .ini file into an associative array in
* the standard .ini format:
*    ; A line starting with a ';' is a comment
*    # Also, a '#' marks a comment up to the end of the line
*    [NewSection]
*    Variable = Value
*    OtherVariable: Value
*
* You can pass a list of "potential" .ini files in a ';' separated path;
* the first readable file will be loaded.
*
* On error, the function returns NIL. On success, you will have an associative
* array of this form:
*
* { 'MAIN' => { 'Key1' => 'Val1', ... ,'KeyN' => 'ValN'},
*   'Section1' => { 'Key1' => 'Val1', ... ,'KeyN' => 'ValN'},
*   ...
*   'SectionN' => { 'Key1' => 'Val1', ... ,'KeyN' => 'ValN'}
* }
*
* Main is the default section (variables that are declared without a section).
*
* Copyright 2002 Giancarlo Niccolai [gian@niccolai.ws]
* www - http://www.xharbour.org
*
* this program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General public License as published by
* the Free Software Foundation; either version 2, or (at your option)
* any later version.
*
* this program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the
* GNU General public License for more details.
*
* You should have received a copy of the GNU General public License
* along with this software; see the file COPYING.  if not, write to
* the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
* Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
*
* As a special exception, xHarbour license gives permission for
* additional uses of the text contained in its release of xHarbour.
*
* The exception is that, if you link the xHarbour libraries with other
* files to produce an executable, this does not by itself cause the
* resulting executable to be covered by the GNU General public License.
* Your use of that executable is in no way restricted on account of
* linking the xHarbour library code into it.
*
* this exception does not however invalidate any other reasons why
* the executable file might be covered by the GNU General public License.
*
* this exception applies only to the code released with this xHarbour
* explicit exception.  if you add/copy code from other sources,
* as the General public License permits, the above exception does
* not apply to the code that you add in this way.  To avoid misleading
* anyone as to the status of such modified files, you must delete
* this exception notice from them.
*
* if you write modifications of your own for xHarbour, it is your choice
* whether to permit this exception to apply to your modifications.
* if you do not wish that, delete this exception notice.
*
* hb_itemClear() and hb_itemCopy() are derivative work of original code
* in the Harbour Project http://harbour-project.org (source/vm/itemapi.c)
* Copyright of Antonio Linares <alinares@fivetech.com>
*
*/

#include "fileio.ch"

   GLOBAL cLineComment := ";"
   GLOBAL cHalfLineComment := "#"

PROCEDURE HB_SetIniComment( cLc, cHlc )

   cLineComment := cLc
   cHalfLineComment := cHlc

   RETURN

FUNCTION HB_ReadIni( cFileSpec, bKeyCaseSens, cSplitters, bAutoMain )

   LOCAL hIni := Hash()

   IF bAutoMain == NIL
      bAutoMain := .T.
   END

   IF bAutoMain
      hIni[ "MAIN" ] := Hash()
   END

   RETURN HB_ReadIni2( hIni, cFileSpec, bKeyCaseSens, cSplitters, bAutoMain )

STATIC FUNCTION HB_ReadIni2( aIni, cFileSpec, bKeyCaseSens, cSplitters, bAutoMain )

   LOCAL aFiles
   LOCAL cFile, nLen
   LOCAL aKeyVal, hCurrentSection
   LOCAL fHandle, nLineEnd
   LOCAL cData, cBuffer, cLine
   LOCAL reComment

   reComment := hb_regexComp( cHalfLineComment + "|^[ \t]*" + cLineComment )

   aFiles := hb_regexSplit( ";", cFileSpec )
   IF Empty( aFiles )
      aFiles := { cFileSpec }
   ENDIF

   FOR EACH cFile IN aFiles
      IF File( cFile )
         fHandle := FOpen( cFile )
         EXIT
      ENDIF
   NEXT

   IF Empty( fHandle ) .OR. fHandle <= 0
      RETURN NIL
   ENDIF

// Default case sensitiveness for keys
   IF bKeyCaseSens == NIL
      bKeyCaseSens := .T.
   ENDIF

   IF cSplitters == NIL
      cSplitters := "=|:"
   ENDIF

   /* we'll read the whole file, then we'll break it in lines. */
   cBuffer := Space( 1024 )
   cData := ""
   DO WHILE ( nLen := FRead( fHandle, @cBuffer, 1024 ) ) > 0
      cData += SubStr( cBuffer, 1, nLen )
   ENDDO
   FClose( fHandle )

   /* Always begin with the MAIN section */
   IF bAutoMain
      hCurrentSection := aIni[ "MAIN" ]
   ELSE
      hCurrentSection := aIni
   END

   cLine := ""
   DO WHILE Len( cData ) > 0
      nLineEnd := At( Chr( 10 ), Left( cData, 256 ) )
      IF nLineEnd == 0
         IF nLineEnd == 0
            // Support for MAC line termination (13)
            nLineEnd := At( Chr( 13 ), Left( cData, 256 ) )
            IF nLineEnd == 0
               nLineEnd := Len( cData ) + 1
            ENDIF
         ENDIF
      ELSE
         // 13 + 10
         IF nLineEnd > 1 .AND. cData[ nLineEnd - 1 ] == 13
            nLineEnd--
         ENDIF
      ENDIF

      // Get the current line
      cLine += AllTrim( Left( cData, nLineEnd - 1 ) )

      // if line terminator is 13 + 10 restore eol position
      IF Len( cData ) > nLineEnd .AND. cData[ nLineEnd ] == 13 .AND. cData[ nLineEnd + 1 ] == 10
         nLineEnd++
      ENDIF

      // remove current line
      cData := SubStr( cData, nLineEnd + 1 )

      //Skip void lines
      IF Len( cLine ) == 0
         LOOP
      ENDIF

      //Sum up lines terminating with "<space>||" ...
      IF Len( cLine ) > 3 .AND. cLine[ -1 ] == "|" .AND. cLine[ -2 ] == "|" .AND. cLine[ -3 ] == " "
         cLine := SubStr( cLine, 1, Len( cLine ) - 2 )

         // ... but proceed if stream over
         IF Len( cData ) > 0
            LOOP
         ENDIF

      ENDIF

      // remove eventual comments
      aKeyVal := hb_regexSplit( reComment, cLine )
      IF .NOT. Empty( aKeyVal )
         cLine := AllTrim( aKeyVal[1] )
      ENDIF

      //Skip all comment lines
      IF Len( cLine ) == 0
         LOOP
      ENDIF

      // Is it an "INCLUDE" statement ?
      aKeyVal := hb_regex( "include (.*)", cLine )
      IF .NOT. Empty( aKeyVal )
         // ignore void includes
         aKeyVal[2] := AllTrim( aKeyVal[2] )
         IF Len( aKeyVal[2] ) == 0
            LOOP
         ENDIF
         HB_ReadIni2( aIni, AllTrim( aKeyVal[2] ), bKeyCaseSens, cSplitters, bAutoMain )
         cLine := ""
         LOOP
      ENDIF

      //Is it a NEW section?
      aKeyVal := hb_regex( "[[](.*)[]]", cLine )
      IF .NOT. Empty( aKeyVal )
         cLine := AllTrim( aKeyVal[2] )
         //Sanitizing
         IF Len( cLine ) != 0
            hCurrentSection := Hash()
            IF .NOT. bKeyCaseSens
               cLine := Upper( cLine )
            ENDIF
            aIni[ cLine ] := hCurrentSection
         ENDIF
         cLine := ""
         LOOP
      ENDIF

      //Is it a valid key?

      aKeyVal := hb_regexSplit( cSplitters, cLine, , , 2 )
      IF Len( aKeyVal ) == 1
         //TODO: Signal error
         cLine := ""
         LOOP
      ENDIF

      // If not case sensitive, use upper keys
      IF .NOT. bKeyCaseSens
         aKeyVal[1] := Upper( aKeyVal[1] )
      ENDIF

      hCurrentSection[ AllTrim(aKeyVal[1] ) ] := AllTrim( aKeyVal[2] )
      cLine := ""
   ENDDO

   RETURN aIni

FUNCTION HB_WriteIni( cFileName, hIni, cCommentBegin, cCommentEnd, bAutoMain )

   LOCAL nFileId := 0
   LOCAL cSection
   LOCAL hCurrentSection
   LOCAL cNewLine := hb_osNewLine()

   IF bAutoMain == NIL
      bAutoMain := .T.
   END

   IF !HB_ISSTRING( cFileName )
      nFileId = cFileName
   ELSE
      nFileId = FCreate( cFileName )
      IF nFileId <= 0
         RETURN .F.
      ENDIF
   ENDIF

   IF !Empty( cCommentBegin )
      FWrite( nFileId, cCommentBegin + cNewLine )
   ENDIF

// Write toplevel section

   IF bAutoMain
      // When automain is on, write the main section
      hCurrentSection = hIni[ "MAIN" ]

      HEval( hCurrentSection, ;
         { | cKey, xVal |  FWrite( nFileId, Cstr( cKey ) + " = " + CStr( xVal ) + cNewLine ) };
         )
   ELSE
      // When automain is off, just write all the toplevel variables.
      HEval( hIni, ;
         { | cKey, xVal | ;
         iif( .NOT. HB_ISHASH( xVal ), ;
         FWrite( nFileId, Cstr( cKey ) + " = " + CStr( xVal ) + cNewLine ), /* nothing */ ) };
         )
   ENDIF


   FOR EACH cSection IN hIni:Keys

      // Avoid re-processing main section
      IF bAutoMain
         // When automain is on, skip section named MAIN
         IF cSection == "MAIN"
            LOOP
         ENDIF
         hCurrentSection = hIni[ cSection ]
      ELSE
         // When automain is off, skip all the toplevel variables.
         hCurrentSection = hIni[ cSection ]
         IF .NOT. HB_ISHASH( hCurrentSection )
            LOOP
         END
      ENDIF

      IF FWrite( nFileId, cNewLine + "[" + CStr( cSection ) + "]" + cNewLine ) <= 0
         RETURN .F.
      ENDIF

      HEval( hCurrentSection, ;
         { | cKey, xVal |  FWrite( nFileId, CStr( cKey ) + "=" + CStr( xVal ) + cNewLine ) };
         )
   NEXT

   IF !Empty( cCommentEnd )
      IF FWrite( nFileId, cCommentEnd + cNewLine ) <= 0
         RETURN .F.
      ENDIF
   ENDIF

   IF nFileId > 0
      FClose( nFileId )
   ENDIF

   RETURN .T.

