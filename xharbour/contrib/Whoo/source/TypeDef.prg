/*
 * $Id:
 */

/*
 * xHarbour Project source code:
 *
 * Whoo.lib TypeDef - Type constant declaration
 *
 * Copyright 2002 Francesco Saverio Giudice [info@fsgiudice.com]
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
 */

*-----------------------------------------------------------------------------*


#define TYPE_NAME    1
#define TYPE_LIST    2

STATIC s_aTypes := {}

// These lines declare TYPE
// Please don't touch
                                             //
#define _DEFINE_TYPES_                       //
                                             //
#define _TYPE_GLOBAL_DECLARATION_            //
#include "types.ch"                          //
#undef  _TYPE_GLOBAL_DECLARATION_            //
                                             //
INIT PROCEDURE _InitTypes()                  // Don't touch these lines
                                             //
   #include "types.ch"                       //
                                             //
RETURN                                       //
                                             //
#undef _DEFINE_TYPES_                        //
                                             //
// End of automatic declaration


PROCEDURE hb_declareType( cTypeName AS STRING, aTypeList AS ARRAY )
   LOCAL nPos, aTypes, n, cType
   // Search trought the array of type to check if exist
   nPos := aScan( s_aTypes, {|e| e[TYPE_NAME] == Upper( cTypeName ) } )

   // Type definition not found, so i can add
   IF nPos == 0
      // Add type name
      aAdd( s_aTypes, { Upper( cTypeName ), {} } )

      // Retrieve the type list array
      aTypes := aTail( s_aTypes )[TYPE_LIST]

      FOR n := 1 TO LEN( aTypeList )
          cType := aTypeList[n]
          // Assign a binary value: 1, 2, 4, etc.
          &cType := 2^(n-1)
          // Add the type value
          aAdd( aTypes, Upper( aTypeList[n] ) )
      NEXT
   ENDIF

RETURN

PROCEDURE hb_declareTypeAsValues( cTypeName AS STRING, aTypeList AS ARRAY )
   LOCAL nPos, aTypes, n
   // Search trought the array of type to check if exist
   nPos := aScan( s_aTypes, {|e| e[TYPE_NAME] == Upper( cTypeName ) } )

   // Type definition not found, so i can add
   IF nPos == 0
      // Add type name
      aAdd( s_aTypes, { Upper( cTypeName ), {} } )

      // Retrieve the type list array
      aTypes := aTail( s_aTypes )[TYPE_LIST]

      FOR n := 1 TO LEN( aTypeList )
          // Add the type value
          aAdd( aTypes, aTypeList[n] )
      NEXT
   ENDIF

RETURN

FUNCTION hb_GetType( cTypeName AS STRING )
   LOCAL nPos
   LOCAL aRet
   // Search trought the array of type to check if exist
   nPos := aScan( s_aTypes, {|e| e[TYPE_NAME] == Upper( cTypeName ) } )
   IF nPos > 0
      aRet := s_aTypes[ nPos ]
   ENDIF
RETURN aRet
