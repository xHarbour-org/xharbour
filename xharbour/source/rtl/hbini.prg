/*
* $Id: hbini.prg,v 1.1 2003/11/24 15:15:25 lf_sfnet Exp $
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


FUNCTION HB_ReadIni( cFileSpec, bKeyCaseSens )
   LOCAL hIni := Hash()

   hIni[ "MAIN" ] := Hash()

RETURN HB_ReadIni2( hIni, cFileSpec, bKeyCaseSens )



STATIC FUNCTION HB_ReadIni2( aIni, cFileSpec, bKeyCaseSens )
   LOCAL aFiles
   LOCAL cFile, nLen
   LOCAL aKeyVal, hCurrentSection
   LOCAL fHandle, nLineEnd
   LOCAL cData, cBuffer, cLine
   LOCAL reComment

   reComment := HB_RegexComp( cHalfLineComment + "|^[ \t]*" + cLineComment )

   aFiles := HB_RegexSplit( ";", cFileSpec )
   IF Empty( aFiles )
      aFiles := { cFileSpec }
   ENDIF

   FOR EACH cFile IN aFiles
      IF File( cFile )
         fHandle := fopen( cFile )
         EXIT
      ENDIF
   NEXT

   IF Empty( fHandle ) .or. fHandle <= 0
      RETURN NIL
   ENDIF

   // Default case sensitiveness for keys
   IF bKeyCaseSens == NIL
      bKeyCaseSens := .T.
   ENDIF

   /* we'll read the whole file, then we'll break it in lines. */
   cBuffer := Space( 1024 )
   cData := ""
   DO WHILE ( nLen := Fread( fHandle, @cBuffer, 1024 ) ) > 0
      cData += substr( cBuffer, 1, nLen )
   ENDDO
   Fclose( fHandle )

   /* Always begin with the MAIN section */
   hCurrentSection := aIni[ "MAIN" ]

   cLine := ""
   DO WHILE Len( cData ) > 0
      nLineEnd := At( chr(13)+chr(10), Substr( cData, 1, 256) )
      IF nLineEnd == 0
         nLineEnd := At( chr(10), Substr( cData, 1, 256) )
         IF nLineEnd == 0
            nLineEnd := At( chr(13), Substr( cData, 1, 256) )
            IF nLineEnd == 0
               nLineEnd := Len( cData )
            ENDIF
         ENDIF
      ENDIF

      // Get the current line
      cLine += AllTrim( Substr( cData, 1, nLineEnd-1 ) )
      // if line terminator is 13/10 add one character
      // (added also support for MAC line termination 10 + 13)
      IF Len( cData ) > nLineEnd .and. ;
            ( cData[ nLineEnd + 1 ] == chr(10) .or. cData[ nLineEnd + 1 ] == chr(13) )
         nLineEnd++
      ENDIF

      // remove current line
      cData := Substr( cData, nLineEnd+1 )

      //Skip void lines
      IF Len( cLine ) == 0
         LOOP
      ENDIF

      //Sum up lines terminating with "<space>||" ...
      IF Len( cLine ) > 3 .and. cLine[ -1 ] == "|" .and. cLine[ -2 ] == "|" .and. cLine[ -3 ] == " "
         cLine := Substr( cLine, 1, Len( cLine ) -2 )

         // ... but proceed if stream over
         IF Len( cData ) > 0
            LOOP
         ENDIF

      ENDIF

      // remove eventual comments
      aKeyVal := HB_RegexSplit( reComment, cLine )
      IF .not. Empty( aKeyVal )
         cLine := AllTrim( aKeyVal[1] )
      ENDIF

      //Skip all comment lines
      IF Len( cLine ) == 0
         LOOP
      ENDIF

      // Is it an "INCLUDE" statement ?
      aKeyVal := HB_RegEx("include (.*)", cLine )
      IF .not. Empty( aKeyVal )
         // ignore void includes
         aKeyVal[2] := AllTrim(aKeyVal[2])
         IF Len( aKeyVal[2] ) == 0
            LOOP
         ENDIF
          HB_ReadIni2( aIni, AllTrim(aKeyVal[2]), bKeyCaseSens )
         cLine := ""
         LOOP
      ENDIF

      //Is it a NEW section?
      aKeyVal := HB_Regex( "[[](.*)[]]", cLine )
      IF .not. Empty( aKeyVal )
         cLine := AllTrim( aKeyVal[2] )
         //Sanitizing
         IF Len( cLine ) != 0
            hCurrentSection := Hash()
            IF .not. bKeyCaseSens
               cLine := Upper( cLine )
            ENDIF
            aIni[ cLine ] := hCurrentSection
         ENDIF
         cLine := ""
         LOOP
      ENDIF

      //Is it a valid key?

      aKeyVal := HB_RegexSplit( "=|:", cLine,,,2 )
      IF Len( aKeyVal ) == 1
         //TODO: Signal error
         cLine := ""
         LOOP
      ENDIF

      // If not case sensitive, use upper keys
      IF .not. bKeyCaseSens
         aKeyVal[1] := Upper( aKeyVal[1] )
      ENDIF

      hCurrentSection[ AllTrim(aKeyVal[1]) ] := AllTrim(aKeyVal[2])
      cLine := ""
   ENDDO

RETURN aIni



function HB_WriteIni( cFileName, hIni, cCommentBegin, cCommentEnd )

   local nFileId := 0
   local cSection
   local hCurrentSection
   local cNewLine := HB_OSNewLine()

   if !HB_IsString( cFileName )

      nFileId = cFileName

   else

      nFileId = FCreate( cFileName )

      if nFileId <= 0
         return .f.
      endif

   endif

   if !Empty( cCommentBegin )
      FWrite( nFileId, cCommentBegin + cNewLine )
   endif

   hCurrentSection = hIni[ "MAIN" ]

   HEval( hCurrentSection, ;
          { | cKey, xVal |  FWrite( nFileId, cKey + "=" + cKey + cNewLine ) };
        )

   for each cSection in hIni:Keys

       if cSection == "MAIN"
          loop
       endif

       hCurrentSection = hIni[ cSection ]

       if FWrite( nFileId, cNewLine + "[" + cSection + "]" + cNewLine ) <= 0
          return .f.
       endif

       HEval( hCurrentSection, ;
              { | cKey, xVal |  FWrite( nFileId, cKey + "=" + xVal + cNewLine ) };
            )

   next

   if !Empty( cCommentEnd )
      if FWrite( nFileId, cCommentEnd + cNewLine ) <= 0
         return .f.
      endif
   endif

   if nFileId > 0
      FClose( nFileId )
   endif

return .t.


