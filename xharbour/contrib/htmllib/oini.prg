/*
 * $Id: oini.prg,v 1.1 2003/02/23 23:15:17 lculik Exp $
 */

/*
 * Harbour Project source code:
 * Ini Class
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */
/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See doc/license.txt for licensing terms.
 *
 */

/*
**
**    oIni.prg
**    --------
**
**    This implementation of an .ini file reader presents a different
**    approach to the problem as it creates a two dimensional array to
**    store and analyse data.
**
**    The first dimension keeps all sections and the second dimension
**    their entries. This way we avoid misinterpretation of entries (an
**    entry could be mistaken for section and vice versa) and keep
**    a clearer view of the whole structure in memory.
**
**    Entries are separated in entry tags, data and comments, e.g.
**
**    TestTag="Data"  ; comment
**
**    Header comments are supported only if preceded with "/*", "*" or "/*"
**    and only at the beginning of the .ini file before any sections.
**
**
**    Organisation of the file in memory (two dimensional array):
**    -----------------------------------------------------------
**
**      [section]
**                    entry=data
**                    entry=data
**                    ...
**      [section]
**                    ...
**
**
**    organisation of the file on disk:
**    ---------------------------------
**      /*
**      **  Header
**      */
// *
// *      [section]
// *      entry=data   ; comment
// *      entry=data
// *      [section]
// *      ...
// *      ; comment
// *
// *
// /

#include "hbclass.ch"
#include "default.ch"

#ifdef TEST
PROC TestMe()

   LOCAL o := oINI():New( "Test.ini" )
   //o:Open()
   o:Read()
   objectViewer( o )
   ? "handle", o:handle
   ? o:Get( "methods", "method45", 45 )
   ? o:Get( "methods", "method455", .t. )
   ? o:Put( "methods", "method8", .f. )
   ? o:Get( "Test", "Test1", 1234567890 )
   o:DumpSections()
   ? "handle", o:handle
   o:Save( ";" + CRLF() + ;
           ";TEST.INI" + CRLF() + ;
           ";Created on " + Dtoc( Date() ) + " - " + Time() + CRLF() + ;
           ";" + CRLF() )
   o:close()
   RETURN

#endif

   //컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
   //컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
CLASS oIni FROM FileBase
   DATA aLines INIT {}
   DATA aSections INIT {}
   DATA lineSize INIT 200

   METHOD New( cFile )

   METHOD READ()

   METHOD GET( cSection, cEntry, uDefault )

   METHOD Put( cSection, cEntry, uValue )

   METHOD Save( c )

   METHOD DumpSections()

ENDCLASS

   //컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD New( cFile ) CLASS oIni

   //컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴

   Super():New( cFile )
   ::Name      := cFile
   ::aLines    := {}
   ::aSections := {}
   ::handle    := - 1

RETURN Self

/*
**  ::Read()
**
**  Reads the .ini file in memory, separating [sections] from entries
**
*/

METHOD READ() CLASS oIni

   LOCAL nSection := 0

   //? "ohandle",::handle
   IF ::handle == NIL .or. ::Handle <= 0

      IF ::Open()

         ::aLines    := {}
         ::aSections := {}
         ::Size()
         ::GoTop()

         WHILE !::EOF()

            ::ReadLine( ::lineSize )

            // ::aLines keep anything before the first section
            // like comments, headers etc.
            IF !Empty( ::Buffer ) .and. nSection == 0

               IF ::Buffer = "/*" .or. ;
                     ::Buffer = "**" .or. ;
                     ::Buffer = "*" .or. ;
                     ::Buffer = "*/"

                  Aadd( ::aLines, ::Buffer )
               ENDIF

            ENDIF

            IF LEFT( Alltrim( ::Buffer ), 1 ) == "["
               nSection ++
               Aadd( ::aSections, { Lower( Alltrim( ::Buffer ) ), {} } )
            ENDIF

            IF nSection > 0

               IF !Empty( ::Buffer )
                  Aadd( ::aSections[ nSection, 2 ], ::buffer )
               ENDIF

            ENDIF

            IF ::EOF()
               EXIT
            ENDIF

         ENDDO

         Aeval( ::aSections, ;
                { | e | Adel( e[ 2 ], 1 ), Asize( e[ 2 ], Len( e[ 2 ] ) - 1 ) } )

      ENDIF         // ::open()

   ENDIF            // ::handle

RETURN Self

/*
**  ::Get( cSection, cEntry, uDefault )
**
**  Retrieves an entry from memory. If it doesn't exist it creates it with
**  default data. Also it creates the section if it doesn't exist.
*/

METHOD GET( cSection, cEntry, uDefault ) CLASS oIni

   LOCAL cRet
   LOCAL nPos
   LOCAL nSection
   LOCAL nEntry

   DEFAULT uDefault := ""

   cSection := "[" + Lower( Alltrim( cSection ) ) + "]"
   nSection := Ascan( ::aSections, { | e | e[ 1 ] = cSection } )

   IF nSection > 0

      nEntry := Ascan( ::aSections[ nSection, 2 ], ;
                       { | e | Upper( cEntry ) == Upper( Alltrim( Left( e, At( "=", e ) - 1 ) ) ) } )

      IF nEntry > 0                     // Found entry. Get value. Cast to data type

         cRet := STR2ANY( GetEntryData( ::aSections[ nSection, 2, nEntry ] ), ;
                          uDefault )

      ELSE          // Entry not found. Insert new entry

         Aadd( ::aSections[ nSection, 2 ], cEntry + "=" + ANY2STR( uDefault ) )
         cRet := uDefault
      ENDIF

   ELSE             // insert new section and entry...
      Aadd( ::aSections, { cSection, ;
                           { cEntry + "=" + ANY2STR( uDefault ) } } )
      cRet := uDefault

   ENDIF

RETURN ( cRet )

/*
**  ::Put()
**
**
**
*/

METHOD Put( cSection, cEntry, uValue ) CLASS oIni

   LOCAL cRet     := ""
   LOCAL cComment
   LOCAL nPos
   LOCAL nSection
   LOCAL nEntry

   DEFAULT uValue := ""

   cSection := "[" + Lower( Alltrim( cSection ) ) + "]"
   nSection := Ascan( ::aSections, { | e | e[ 1 ] = cSection } )                //$ e[1] } )
   IF nSection > 0

      nEntry := Ascan( ::aSections[ nSection, 2 ], ;
                       { | e | Upper( cEntry ) == Upper( Left( e, At( "=", e ) - 1 ) ) } )

      IF nEntry > 0                     // Found entry. Get value. Cast to data type

         // return old value
         cRet := STR2ANY( GetEntryData( ::aSections[ nSection, 2, nEntry ] ), ;
                          uValue )

         cComment := GetEntryComment( ::aSections[ nSection, 2, nEntry ] )

         // put new value
         ::aSections[ nSection, 2, nEntry ] := cEntry + "=" + ;
                                               ANY2STR( uValue ) + ;
                                               Space( 2 ) + ;
                                               cComment

      ELSE          // Entry not found. Insert new entry.

         Aadd( ::aSections[ nSection, 2 ], cEntry + "=" + ANY2STR( uValue ) )
         cRet := uValue
      ENDIF

   ELSE             // Section not found. Insert section and entry.

      Aadd( ::aSections, { cSection, ;
                           { cEntry + "=" + ANY2STR( uValue ) } } )
      cRet := uValue

   ENDIF

RETURN ( cRet )

/*
**  ::Save()
**
**  Stores the .ini file back to disk from memory. All new changes are
**  saved, including comments...
*/

METHOD Save( cComment ) CLASS oIni

   LOCAL i
   LOCAL j

   DEFAULT cComment := ""

   ::Close()

   // Delete and create file

   IF ::Create( ::Name )

      ::Write( cComment + CRLF() )

      Aeval( ::aLines, { | e | Fwrite( ::Handle, e + Chr( 13 ) + Chr( 10 ) ) } )

      ::Write( CRLF() )

      FOR i := 1 TO Len( ::aSections )

         //alert( ::aSections[i,1]  )

         ::Write( ::aSections[ i, 1 ] + CRLF() )
         FOR j := 1 TO Len( ::aSections[ i, 2 ] )
            ::Write( ::aSections[ i, 2, j ] + CRLF() )
         NEXT
         ::Write( CRLF() )
      NEXT

   ENDIF

   ::Close()

RETURN Self

METHOD DumpSections() cLASS OINI

   LOCAL i
   LOCAL j

   FOR i := 1 TO Len( ::aSections )
      Outstd( ::aSections[ i, 1 ] + CRLF() )

      FOR j := 1 TO Len( ::aSections[ i, 2 ] )
         Outstd( Space( 5 ) + ::aSections[ i, 2, j ] + CRLF() )
      NEXT

   NEXT

RETURN Self

/*
**  stripSection()
**
**  Removes "[]" characters from a section entry
*/
STATIC FUNCTION StripSection( cSection )

   cSection := Lower( Alltrim( cSection ) )

   DO WHILE "[" $ cSection
      cSection := Stuff( cSection, At( "[", cSection ), 1 )
   ENDDO

   DO WHILE "]" $ cSection
      cSection := Stuff( cSection, At( "]", cSection ), 1 )
   ENDDO

RETURN "[" + cSection + "]"

/*
**  GetEntryData( cEntry )
**
**  Retrieves the data part of an .ini entry. Supports comments to the
**  right of the data.
*/

STATIC FUNCTION GetEntryData( cEntry )

   LOCAL cRet      := ""
   LOCAL nPos      := 0
   LOCAL isComment := ( nPos := At( ";", cEntry ) ) > 0

   IF isComment
      cRet := Substr( cEntry, At( "=", cEntry ) + 1, Len( cEntry ) - nPos - 1 )                     //AT( ";", cEntry )-1 )
   ELSE
      cRet := Substr( cEntry, At( "=", cEntry ) + 1 )
   ENDIF

RETURN Alltrim( cRet )

/*
**  GetEntryComment( cEntry )
**
**  Retrieves the comment of an .ini entry.
*/

STATIC FUNCTION GetEntryComment( cEntry )

   LOCAL cRet      := ""
   LOCAL nPos      := 0
   LOCAL isComment := ( nPos := At( ";", cEntry ) ) > 0

   IF isComment
      cRet := Substr( cEntry, At( ";", cEntry ), Len( cEntry ) )
   ELSE
      cRet := ""
   ENDIF

RETURN Alltrim( cRet )

/*
**  ::DumpSections()
**
**  Display [sections] and entries from memory ( debug method )
**
*/

/****
*
*     UTILITIES
*     ---------
*
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC FUNCTION Any2Str( xVal )

   LOCAL cRet  := ""
   LOCAL cType := Valtype( xVal )
   SWITCH cType
      CASE  "D" 
         cRet := Dtoc( xVal )
         EXIT
      CASE  "N" 
         cRet := Alltrim( Str( xVal ) )
         EXIT
      CASE  "L" 
         cRet := IIF( xVal == .T., ".T.", ".F." )
         EXIT
      CASE  "B" 
         cRet := "{|| ... }"
         EXIT
      CASE  "O" 
         cRet := xVal:ClassName()
         EXIT
      CASE  "U" 
         cRet := ""
         EXIT
      DEFAULT 
         cRet := xVal
         EXIT
   END

RETURN cRet

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC FUNCTION Str2Any( cVal, uType )

   LOCAL bError
   LOCAL uRet   := ""
   LOCAL cType  := Valtype( uType )
   bError := Errorblock( { | o | Break( o ) } )
   cVal   := Alltrim( cVal )

   BEGIN SEQUENCE

      SWITCH cType
         CASE  "D" 
            uRet := Ctod( cVal )
            EXIT
         CASE  "N" 
            uRet := IIF( !Empty( cVal ), Val( cVal ), 0 )
            EXIT
         CASE  "L"
            uRet := IIF( cVal == ".T.", .T., .F. )
            EXIT
         CASE  "B" 
            uRet := { || .T. }
            EXIT
         CASE  "O" 
            uRet := NIL
            EXIT
         CASE  "U" 
            uRet := NIL
            EXIT
         DEFAULT
            uRet := cVal
            EXIT
      END

   RECOVER
      uRet := cVal
   END SEQUENCE

   Errorblock( bError )

RETURN uRet

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC FUNCTION Token( cStr, cDelim, nToken )

   LOCAL nPos
   LOCAL cToken
   LOCAL nCounter := 1

   DEFAULT nToken := 1

   WHILE .T.

      IF ( nPos := At( cDelim, cStr ) ) == 0

         IF nCounter == nToken
            cToken := cStr
         ENDIF

         EXIT

      ENDIF

      IF ++ nCounter > nToken
         cToken := LEFT( cStr, nPos - 1 )
         EXIT
      ENDIF

      cStr := Substr( cStr, nPos + 1 )

   ENDDO

RETURN cToken

