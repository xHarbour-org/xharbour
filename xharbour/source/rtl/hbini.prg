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


procedure HB_SetIniComment( cLc, cHlc )
   cLineComment = cLc
   cHalfLineComment = cHlc
return


function HB_ReadIni( cFileSpec, bKeyCaseSens )

   local hIni := Hash()

return HB_ReadIni2( hIni, cFileSpec, bKeyCaseSens )


static function HB_ReadIni2( aIni, cFileSpec, bKeyCaseSens )

   local aFiles := HB_RegexSplit( ";", cFileSpec )
   local cFile, nLen
   local aKeyVal, hCurrentSection
   local fHandle, nLineEnd
   local cData := ""
   local cBuffer := Space( 1024 )
   local cLine
   local reComment := HB_RegexComp( cHalfLineComment + "|^[ \t]*" + cLineComment )

   if Empty( aFiles )
      aFiles = { cFileSpec }
   endif

   for each cFile in aFiles
      if File( cFile )
         fHandle = FOpen( cFile )
         exit
      endif
   next

   if Empty( fHandle ) .or. fHandle < 0
      return nil
   endif

   // Default case sensitiveness for keys
   if bKeyCaseSens == nil
      bKeyCaseSens = .t.
   endif

   // we'll read the whole file, then we'll break it in lines.
   while ( nLen := FRead( fHandle, @cBuffer, 1024 ) ) > 0
      cData += SubStr( cBuffer, 1, nLen )
   enddo

   FClose( fHandle )

   cLine = ""
   while Len( cData ) > 0

      nLineEnd = At( Chr( 13 ) + Chr( 10 ), SubStr( cData, 1, 256) )

      if nLineEnd == 0

         nLineEnd = At( Chr( 10 ), SubStr( cData, 1, 256 ) )

         if nLineEnd == 0

            nLineEnd = At( Chr( 13 ), SubStr( cData, 1, 256 ) )

            if nLineEnd == 0
               nLineEnd = Len( cData )
            endif

         endif

      endif

      // Get the current line
      cLine += AllTrim( SubStr( cData, 1, nLineEnd - 1 ) )

      // if line terminator is 13/10 add one character
      // (added also support for MAC line termination 10 + 13)
      if Len( cData ) > nLineEnd .and. ( cData[ nLineEnd + 1 ] == Chr( 10 ) .or. cData[ nLineEnd + 1 ] == Chr( 13 ) )
         nLineEnd++
      endif

      // remove current line
      cData = SubStr( cData, nLineEnd+1 )

      //Skip void lines
      if Len( cLine ) == 0
         loop
      endif

      //Sum up lines terminating with "<space>||" ...
      if Len( cLine ) > 3 .and. cLine[ -1 ] == "|" .and. cLine[ -2 ] == "|" .and. cLine[ -3 ] == " "

         cLine = SubStr( cLine, 1, Len( cLine ) -2 )

         // ... but proceed if stream over
         if Len( cData ) > 0
            loop
         endif

      endif

      // remove eventual comments
      aKeyVal = HB_RegexSplit( reComment, cLine )
      if !Empty( aKeyVal )
         cLine = AllTrim( aKeyVal[ 1 ] )
      endif

      //Skip all comment lines
      if Len( cLine ) == 0
         loop
      endif

      // Is it an "INCLUDE" statement ?
      aKeyVal = HB_RegEx("include (.*)", cLine )
      if !Empty( aKeyVal )
         // ignore void includes
         aKeyVal[ 2 ] = AllTrim( aKeyVal[ 2 ] )
         if Len( aKeyVal[ 2 ] ) == 0
            loop
         endif
         HB_ReadIni2( aIni, AllTrim( aKeyVal[ 2 ] ), bKeyCaseSens )
         cLine = ""
         loop
      endif

      //Is it a NEW section?
      aKeyVal = HB_Regex( "[[](.*)[]]", cLine )
      if !Empty( aKeyVal )
         cLine = AllTrim( aKeyVal[ 2 ] )
         //Sanitizing
         if Len( cLine ) > 0
            hCurrentSection = Hash()
            if !bKeyCaseSens
               cLine = Upper( cLine )
            endif
            aIni[ cLine ] := hCurrentSection
         endif
         cLine = ""
         loop
      endif

      //Is it a valid key?
      aKeyVal = HB_RegexSplit( "=|:", cLine, nil, nil, 2 )
      if Len( aKeyVal ) == 1
         //TODO: Signal error
         cLine = ""
         loop
      endif

      // If not case sensitive, use upper keys
      if !bKeyCaseSens
         aKeyVal[ 1 ] = Upper( aKeyVal[ 1 ] )
      endif

      hCurrentSection[ AllTrim( aKeyVal[ 1 ] ) ] = AllTrim( aKeyVal[ 2 ] )

      cLine = ""

   enddo

return aIni


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

   for each cSection in hIni:Keys

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


