/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
*
*-----------------------------------------------------------------------------
* Parts of this project come from:
* "Harbour MiniGUI"
*                   Copyright 2002 Roberto Lopez <roblez@ciudad.com.ar>
*                   http://www.geocities.com/harbour_minigui/
* "Harbour GUI framework for Win32"
*                   Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
*                   Copyright 2001 Antonio Linares <alinares@fivetech.com>
*                   http://www.harbour-project.org
*-----------------------------------------------------------------------------
*
*/
// Modified from Ron Pinkas \harbour\samples\guestbk\inifiles.prg
//
// $Id: TIniFile.prg,v 1.1 2002/09/05 23:13:04 ronpinkas Exp $
//
//

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"
#include "windows.ch"

CLASS TIniFile FROM TObject

   DATA cFileName              // define this class objects datas
   DATA aContents

   METHOD New() CONSTRUCTOR
   METHOD ReadString()
   METHOD WriteString()
   METHOD ReadNumber()
   METHOD WriteNumber()
   METHOD ReadDate()
   METHOD WriteDate()
   METHOD ReadBool()
   METHOD WriteBool()
   METHOD ReadSection()
   METHOD ReadSections()
   METHOD DeleteKey()
   METHOD EraseSection()
   METHOD UpdateFile()

ENDCLASS

METHOD New( cFileName ) CLASS TIniFile
   local lDone, hFile, cFile, cLine, cIdent, nPos
   local aCurrArray

   if empty( cFileName )
      // raise an error?
      outerr('No filename passed to TIniFile():New()')
      return nil

   else
      ::cFileName := cFilename
      ::aContents := {}
      aCurrArray := ::aContents

      if File(cFileName)
         hFile := fopen(cFilename, 0)
      else
         hFile := fcreate(cFilename)
      endif

      cLine := ''
      lDone := .f.
      while !lDone
         cFile := space(256)
         lDone := (fread(hFile, @cFile, 256) <= 0)

         cFile := strtran(cFile, chr(10), '') // so we can just search for CHR(13)

         // prepend last read
         cFile := cLine + cFile
         while !empty(cFile)
            if (nPos := at(chr(13), cFile)) > 0
               cLine := left(cFile, nPos - 1)
               cFile := substr(cFile, nPos + 1)

               if !empty(cLine)
                  if Left(cLine, 1) == '[' // new section
                     if (nPos := At(']', cLine)) > 1
                        cLine := substr(cLine, 2, nPos - 2)
                     else
                        cLine := substr(cLine, 2)
                     endif

                     AAdd(::aContents, { cLine, { /* this will be CurrArray */ } } )
                     aCurrArray := ::aContents[Len(::aContents)][2]

                  elseif Left(cLine, 1) == ';' // preserve comments
                     AAdd( aCurrArray, { NIL, cLine } )

                  else
                     if (nPos := At('=', cLine)) > 0
                        cIdent := Left(cLine, nPos - 1)
                        cLine := SubStr(cLine, nPos + 1)

                        AAdd( aCurrArray, { cIdent, cLine } )

                     else
                        AAdd( aCurrArray, { cLine, '' } )
                     endif
                  endif
                  cLine := '' // to stop prepend later on
               endif

            else
               cLine := cFile
               cFile := ''
            endif
         end
      end

      fclose(hFile)
   endif

return Self

METHOD ReadString(cSection, cIdent, cDefault) CLASS TIniFile
   local cResult := cDefault
   local i, j, cFind

   if Empty(cSection)
      cFind := lower(cIdent)
      j := AScan( ::aContents, {|x| valtype(x[1]) == 'C' .and. ;
                                    lower(x[1]) == cFind .and. ;
                                    ValType(x[2]) == 'C'} )

      if j > 0
          cResult := ::aContents[j][2]
      endif

   else
      cFind := lower(cSection)
      i := AScan( ::aContents, {|x| valtype(x[1]) == 'C' .and. lower(x[1]) == cFind} )

      if i > 0
         cFind := lower(cIdent)
         j := AScan( ::aContents[i][2], {|x| valtype(x[1]) == 'C' .and. lower(x[1]) == cFind} )

         if j > 0
            cResult := ::aContents[i][2][j][2]
         endif
      endif
   endif
return cResult

METHOD WriteString(cSection, cIdent, cString) CLASS TIniFile
   local i, j, cFind

   if Empty(cIdent)
      outerr('Must specify an identifier')

   elseif Empty(cSection)
      cFind := lower(cIdent)
      j := AScan( ::aContents, {|x| valtype(x[1]) == 'C' .and.;
                                    lower(x[1]) == cFind .and.;
                                    ValType(x[2]) == 'C'} )

      if j > 0
         ::aContents[j][2] := cString

      else
         AAdd(::aContents, nil)
         AIns(::aContents, 1)
         ::aContents[1] := {cIdent, cString}
      endif

   else
      cFind := lower(cSection)
      if (i := AScan( ::aContents, {|x| valtype(x[1]) == 'C' .and.;
                                        lower(x[1]) == cFind .and.;
                                        ValType(x[2]) == 'A'})) > 0
         cFind := lower(cIdent)
         j := AScan( ::aContents[i][2], {|x| valtype(x[1]) == 'C' .and. lower(x[1]) == cFind} )

         if j > 0
            ::aContents[i][2][j][2] := cString

         else
            AAdd( ::aContents[i][2], {cIdent, cString} )
         endif

      else
         AAdd( ::aContents, {cSection, {{cIdent, cString}}} )
      endif
   endif
return TRUE

METHOD ReadNumber(cSection, cIdent, nDefault) CLASS TIniFile
return Val( ::ReadString(cSection, cIdent, str(nDefault)) )

METHOD WriteNumber(cSection, cIdent, nNumber) CLASS TIniFile
return ::WriteString( cSection, cIdent, alltrim(str(nNumber)) )

METHOD ReadDate(cSection, cIdent, dDefault) CLASS TIniFile
return SToD( ::ReadString(cSection, cIdent, DToS(dDefault)) )

METHOD WriteDate(cSection, cIdent, dDate) CLASS TIniFile
return ::WriteString( cSection, cIdent, DToS(dDate) )

METHOD ReadBool(cSection, cIdent, lDefault) CLASS TIniFile
   local cDefault := Iif( lDefault, '.t.', '.f.' )
return ::ReadString(cSection, cIdent, cDefault) == '.t.'

METHOD WriteBool(cSection, cIdent, lBool) CLASS TIniFile
return ::WriteString( cSection, cIdent, Iif(lBool, '.t.', '.f.') )

METHOD DeleteKey(cSection, cIdent) CLASS TIniFile
   local i, j

   cSection := lower(cSection)
   i := AScan( ::aContents, {|x| valtype(x[1]) == 'C' .and. lower(x[1]) == cSection} )

   if i > 0
      cIdent := lower(cIdent)
      j := AScan( ::aContents[i][2], {|x| valtype(x[1]) == 'C' .and. lower(x[1]) == cIdent} )

      ADel( ::aContents[i][2], j )
      ASize( ::aContents[i][2], Len(::aContents[i][2]) - 1 )
   endif
return TRUE

METHOD EraseSection(cSection) CLASS TIniFile
   local i

   if Empty(cSection)
      while (i := AScan( ::aContents, {|x| valtype(x[1]) == 'C' .and. ValType(x[2]) == 'C'})) > 0
         ADel( ::aContents, i )
         ASize( ::aContents, len(::aContents) - 1 )
      end

   else
      cSection := lower(cSection)
      if (i := AScan( ::aContents, {|x| valtype(x[1]) == 'C' .and. lower(x[1]) == cSection .and. ValType(x[2]) == 'A'})) > 0
         ADel( ::aContents, i )
         ASize( ::aContents, Len(::aContents) - 1 )
      endif
   endif
return TRUE

METHOD ReadSection(cSection) CLASS TIniFile
   local i, j, aSection := {}

   if Empty(cSection)
      for i := 1 to len(::aContents)
         if valtype(::aContents[i][1]) == 'C' .and. valtype(::aContents[i][2]) == 'C'
            aadd(aSection, ::aContents[i][1])
         endif
      next

   else
      cSection := lower(cSection)
      if (i := AScan( ::aContents, {|x| valtype(x[1]) == 'C' .and. x[1] == cSection .and. ValType(x[2]) == 'A'})) > 0

         for j := 1 to Len(::aContents[i][2])

            if ::aContents[i][2][j][1] <> NIL
               AAdd(aSection, ::aContents[i][2][j][1])
            endif
         next
      endif
   endif
return aSection

METHOD ReadSections() CLASS TIniFile
   local i, aSections := {}

   for i := 1 to Len(::aContents)

      if ValType(::aContents[i][2]) == 'A'
         AAdd(aSections, ::aContents[i][1])
      endif
   next
return aSections

METHOD UpdateFile() CLASS TIniFile
   local i, j, hFile

   hFile := fcreate(::cFileName)

   for i := 1 to Len(::aContents)
      if ::aContents[i][1] == NIL
         fwrite(hFile, ::aContents[i][2] + Chr(13) + Chr(10))

      elseif ValType(::aContents[i][2]) == 'A'
         fwrite(hFile, '[' + ::aContents[i][1] + ']' + Chr(13) + Chr(10))
         for j := 1 to Len(::aContents[i][2])

            if ::aContents[i][2][j][1] == NIL
               fwrite(hFile, ::aContents[i][2][j][2] + Chr(13) + Chr(10))

            else
               fwrite(hFile, ::aContents[i][2][j][1] + '=' + ::aContents[i][2][j][2] + Chr(13) + Chr(10))
            endif
         next
         fwrite(hFile, Chr(13) + Chr(10))

      elseif ValType(::aContents[i][2]) == 'C'
         fwrite(hFile, ::aContents[i][1] + '=' + ::aContents[i][2] + Chr(13) + Chr(10))

      endif
   next
   fclose(hFile)
return TRUE // FSG - Check file errors
