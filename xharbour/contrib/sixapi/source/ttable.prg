/*
 * $Id$
 */

/*
 * SixAPI Project source code:
 *
 * Copyright 2010 Andi Jahja <xharbour@telkom.net.id>
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
 */

#include "hbclass.ch"

#define SX_READWRITE		1
#define SX_READONLY		2
#define SX_EXCLUSIVE		3

// SX_DBINFO( CALIAS ) =>   ARRAY
#define SXINFO_AREA		1  // INTEGER
#define SXINFO_FILENAME		2  // STRING
#define SXINFO_ALIAS		3  // STRING
#define SXINFO_SHARED		4  // LOGICAL
#define SXINFO_READONLY		5  // LOGICAL
#define SXINFO_RDETYPE		6  // INTEGER
#define SXINFO_MODE		7  // INTEGER
#define SXINFO_RDD		8  // STRING
#define SXINFO_COMMITLEVEL	9  // INTEGER
#define SXINFO_RECSIZE		10 // INTEGER
#define SXINFO_FIELDCOUNT	11 // INTEGER
#define SXINFO_FIELDINFO	12 // ARRAY

CLASS TApollo

   DATA cDBFFile
   DATA	cIndexFile
   DATA cAlias
   DATA nWorkArea
   DATA	nOpenMode INIT SX_READWRITE  // { "READWRITE","READONLY","EXCLUSIVE" };
   DATA	nCommitLevel INIT 1  // 1 or 2
   DATA cRDD                 // "SDENTX","SDEFOX","SDENSX","SDENSX_DBT"
   DATA aFieldName INIT {}

   METHOD New()
   METHOD Open()
   METHOD Close()
   METHOD RecCount()
   METHOD LastRec()
   METHOD RecNo()
   METHOD Commit()
   METHOD DBGoTop()
   METHOD DBGoTo( nRecNo )
   METHOD DBGoBottom()
   METHOD DBSkip( nSkip )
   METHOD FCount()
   METHOD FieldName( iFieldNum )
   METHOD FieldGet( cFieldName )
   METHOD Bof()
   METHOD Eof()
   METHOD Replace( cpFieldName, xData )
   METHOD DBSeek( cSeek )
   METHOD Found()
   METHOD DBLocate( cpExpression, iDirection, bContinue )

ENDCLASS

METHOD New( cDBFFile, nOpenMode ) CLASS TApollo

   ::cDBFFile  := cDBFFile
   ::nOpenMode := nOpenMode

return Self

METHOD Open( nOpenMode ) CLASS TApollo

   LOCAL i, j, cField
   LOCAL hClass := self:ClassH

   IF valtype( nOpenMode ) == "N"
      ::nOpenMode := nOpenMode
   ENDIF

   ::nWorkArea := sx_Use(;
        ::cDBFFile,;
        ::cAlias,;
        ::nOpenMode,;
        ::cRDD,;
        ::nCommitLevel )

   j := sx_FieldCount( ::cAlias )

   FOR i := 1 TO j
      cField := sx_FieldName( i, ::cAlias )
      AADD( ::aFieldName, cField )
      __clsAddMsg( hClass,       cField, __blockGet( cField ), HB_OO_MSG_INLINE )
      __clsAddMsg( hClass, "_" + cField, __blockPut( cField ), HB_OO_MSG_INLINE )
   NEXT

return Self

METHOD RecCount() CLASS TApollo
   return sx_RecCount( ::cAlias )

METHOD LastRec() CLASS TApollo
   return sx_RecCount( ::cAlias )

METHOD Close() CLASS TApollo
   return sx_Close( ::cAlias )

METHOD RecNo() CLASS TApollo
   return sx_RecNo( ::cAlias )

METHOD DBGoTo( nRecNo ) CLASS TApollo
   return sx_Go( nRecNo, ::cAlias )

METHOD DBGoTop() CLASS TApollo
   return sx_GoTop( ::cAlias )

METHOD DBGoBottom() CLASS TApollo
   return sx_GoBottom( ::cAlias )

METHOD FCount() CLASS TApollo
   return sx_FieldCount( ::cAlias )

METHOD FieldName( iFieldNum ) CLASS TApollo
   return sx_FieldName( iFieldNum, ::cAlias )

METHOD FieldGet( cFieldName ) CLASS TApollo
   return sx_FieldGet( cFieldName, ::cAlias )

METHOD Bof() CLASS TApollo
   return sx_Bof( ::cAlias )

METHOD Eof() CLASS TApollo
   return sx_Eof( ::cAlias )

METHOD Commit() CLASS TApollo
   return sx_Commit( ::cAlias )

METHOD DBSkip( nSkip ) CLASS TApollo

   if nSkip == nil
     nSkip := 1
   endif

   return sx_Skip( nSkip, ::cAlias )

METHOD Replace( cpFieldName, xData ) CLASS TAPOLLO
   return sx_Replace( cpFieldName, xData, ::cAlias )

METHOD DBSeek( cSeek ) CLASS TAPOLLO
   return sx_Seek( cSeek, ::cAlias )

METHOD Found() CLASS TAPOLLO
   return sx_Found( ::cAlias )

METHOD DBLocate( cpExpression, iDirection, bContinue ) CLASS TAPOLLO
  return sx_Locate ( cpExpression, iDirection, bContinue, ::cAlias )

STATIC FUNCTION __blockGet( cField )
   return { | self | sx_FieldGet( cField, ::cAlias ) }

STATIC FUNCTION __blockPut( cField )
   return { | self, xval | if( xval == nil,,sx_Replace( cField, xval, ::cAlias ) ) }
