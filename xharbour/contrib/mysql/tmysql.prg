 /*
 * $Id: tmysql.prg,v 1.7 2003/02/18 17:15:41 lculik Exp $
 */

 /*
 * Harbour Project source code:
 * MySQL DBMS classes.
 * These classes try to emulate clipper dbXXXX functions on a SQL query
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
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

 /*
 2002-01-28 21:30 UTC+0100 Patrick Mast <email@patrickmast.com>
   * contrib/mysql/tmysql
     + Added DateTime field
     * Added more info on Alert message for Unknown type
     * Modified ClipValue2SQL() to process empty strings
 */

 /*
 2002-02-18 07:30 UTC+0100 Srdjan Dragojlovic <digikv@yahoo.com>
   * contrib/mysql/tmysql
     + Bug in GOTO Method
 */


#include "hbclass.ch"
#include "common.ch"
#include "dbstruct.ch"
#include "mysql.ch"



// Every single row of an answer
CLASS TMySQLRow

   DATA  aRow              // a single row of answer
   DATA  aDirty            // array of booleans set to .T. if corresponding field of aRow has been changed
   DATA  aOldValue         // If aDirty[n] is .T. aOldValue[n] keeps a copy of changed value if aRow[n] is part of a primary key

   DATA  aFieldStruct      // type of each field
   DATA  cTable            // Name of table containing this row, empty if TMySQLQuery returned this row

   METHOD   New( aRow, aFStruct, cTableName )     // Create a new Row object

   METHOD   FieldGet( cnField )          // Same as clipper ones, but FieldGet() and FieldPut() accept a string as
   METHOD   FieldPut( cnField, Value )   // field identifier, not only a number
   METHOD   FieldName( nNum )
   METHOD   FieldPos( cFieldName )

   METHOD   FieldLen( nNum )             // Length of field N
   METHOD   FieldDec( nNum )             // How many decimals in field N
   METHOD   FieldType( nNum )            // Clipper type of field N

   METHOD   MakePrimaryKeyWhere()        // returns a WHERE x=y statement which uses primary key (if available)

ENDCLASS


METHOD New( aRow, aFStruct, cTableName ) CLASS TMySQLRow

   default cTableName to ""
   default aFStruct to {}

   ::aRow         := aRow
   ::aFieldStruct := aFStruct
   ::cTable       := cTableName

   ::aDirty       := Array( Len( ::aRow ) )
   ::aOldValue    := Array( Len( ::aRow ) )

   AFill( ::aDirty, .F. )

return Self


METHOD FieldGet( cnField ) CLASS TMySQLRow

   local nNum

   if ValType( cnField ) == "C"
      nNum := ::FieldPos( cnField )
   else
      nNum := cnField
   endif

   if nNum > 0 .AND. nNum <= Len( ::aRow )

      // Char fields are padded with spaces since a real .dbf field would be
      if ::FieldType( nNum ) == "C"
         return PadR( ::aRow[ nNum ], ::aFieldStruct[ nNum ][ MYSQL_FS_LENGTH ])
      else
         return ::aRow[ nNum ]
      endif

   endif

return nil


METHOD FieldPut( cnField, Value ) CLASS TMySQLRow

   local nNum

   if ValType( cnField ) == "C"
      nNum := ::FieldPos( cnField )
   else
      nNum := cnField
   endif

   if nNum > 0 .AND. nNum <= Len( ::aRow )

      if Valtype( Value ) == Valtype( ::aRow[ nNum ] ) .OR. ::aRow[ nNum ] == NIL  // .OR. Empty(::aRow[nNum])

         // if it is a char field remove trailing spaces
         if ValType( Value ) == "C"
            Value := RTrim( Value )
         endif

         // Save starting value for this field
         if !::aDirty[ nNum ]
            ::aOldValue[ nNum ] := ::aRow[ nNum ]
            ::aDirty[ nNum ]    := .T.
         endif

         ::aRow[ nNum ] := Value

         return Value
      endif
   endif

return nil


// Given a field name returns it's position
METHOD FieldPos( cFieldName ) CLASS TMySQLRow

   local cUpperName, nPos

   cUpperName := Upper( cFieldName )

   nPos := AScan( ::aFieldStruct, {| aItem | Upper( aItem[ MYSQL_FS_NAME ] ) == cUpperName } )

return nPos


// Returns name of field N
METHOD FieldName( nNum ) CLASS TMySQLRow

   if nNum >= 1 .AND. nNum <= Len( ::aFieldStruct )
      return ::aFieldStruct[ nNum ][ MYSQL_FS_NAME ]
   endif

return ""


METHOD FieldLen(nNum) CLASS TMySQLRow

   if nNum >= 1 .AND. nNum <= Len( ::aFieldStruct )
      return ::aFieldStruct[ nNum ][ MYSQL_FS_LENGTH ]
   endif

return 0


METHOD FieldDec( nNum ) CLASS TMySQLRow

   if nNum >= 1 .AND. nNum <= Len( ::aFieldStruct )
      return ::aFieldStruct[ nNum ][ MYSQL_FS_DECIMALS ]
   endif

return 0


METHOD FieldType( nNum ) CLASS TMySQLRow

   if nNum >= 1 .AND. nNum <= Len( ::aFieldStruct )
      Return SQL2ClipType( ::aFieldStruct[ nNum ][ MYSQL_FS_TYPE ] )
   endif

return "U"


// returns a WHERE x=y statement which uses primary key (if available)
METHOD MakePrimaryKeyWhere() CLASS TMySQLRow

   local cWhere := " WHERE ", aField

   for each aField in ::aFieldStruct

      // search for fields part of a primary key
      if ( sqlAND( aField[ MYSQL_FS_FLAGS ], PRI_KEY_FLAG ) == PRI_KEY_FLAG ) .OR.;
         ( sqlAND( aField[ MYSQL_FS_FLAGS ], MULTIPLE_KEY_FLAG ) == MULTIPLE_KEY_FLAG )

         cWhere += aField[ MYSQL_FS_NAME ] + "="

         // if a part of a primary key has been changed, use original value
         if ::aDirty[ HB_EnumIndex() ]
            cWhere += ClipValue2SQL( ::aOldValue[ HB_EnumIndex() ] )
         else
            cWhere += ClipValue2SQL( ::aRow[ HB_EnumIndex() ] )
         endif

         cWhere += " AND "
      endif

   next

   // remove last " AND "
   cWhere := Left( cWhere, Len( cWhere ) - 5 )

return cWhere

/* ----------------------------------------------------------------------------------------*/

// Every single query submitted to MySQL server
CLASS TMySQLQuery

   DATA  nSocket           // connection handle to MySQL server
   DATA  nResultHandle     // result handle received from MySQL

   DATA  cQuery            // copy of query that generated this object

   DATA  nNumRows          // number of rows available on answer NOTE MySQL is 0 based
   DATA  nCurRow           // I'm currently over row number

   DATA  nNumFields        // how many fields per row
   DATA  aFieldStruct      // type of each field, a copy is here a copy inside each row
   DATA  aRow

   DATA  lError            // .T. if last operation failed

   DATA  loRow             // If return oRow in GetRow(), 

   METHOD   New( nSocket, cQuery, loRow )  // New query object

   METHOD   Destroy()  INLINE sqlFreeR( ::nResultHandle ), Self
                                           // Free result handle and associated resources
   METHOD   End()      INLINE ::Destroy()

   METHOD   Refresh()                      // ReExecutes the query (cQuery) so that changes to table are visible

   METHOD   GetRow( nRow, loRow, lSkip )   // return Row n of answer

   METHOD   Skip( nRows )                  // Same as clipper ones

   METHOD   Bof()        INLINE ::lBof  // ::nCurRow == 1
   METHOD   Eof()        INLINE ::lEof  // ::nCurRow == ::nNumRows
   METHOD   RecNo()      INLINE ::nCurRow
   METHOD   LastRec()    INLINE ::nNumRows
   METHOD   GoTop()      INLINE iif( ::nNumRows > 0, ( ::lEof := .f. , ::lBof := .f.), ),;
                                ::getRow( 1 )
   METHOD   GoBottom()   INLINE iif( ::nNumRows > 0, ( ::lEof := .f. , ::lBof := .f.), ),;
                                ::getRow( ::nNumRows ) //-1 )
   METHOD   GoTo( nRow ) INLINE ::lEof := ( ::nCurRow + nRow > ::nNumRows ),;
                                ::lBof := ( ::nCurRow + nRow < 1 ),;
                                ::GetRow( nRow )

   METHOD   FCount()     INLINE ::nNumFields

   METHOD   NetErr()     INLINE ::lError      // Returns .T. if something went wrong

   METHOD   Error()      INLINE ::lError := .F., sqlGetErr(::nSocket)
                                              // Returns textual description of last error and clears ::lError

   METHOD   FieldName( nNum )
   METHOD   FieldPos( cFieldName )
   METHOD   FieldGet( cnField )

   METHOD   FieldLen( nNum )                    // Length of field N
   METHOD   FieldDec( nNum )                    // How many decimals in field N
   METHOD   FieldType( nNum )                   // Clipper type of field N

   METHOD   Locate( cFieldName, Value, bPartialKey, bSoftSeek )

   PROTECTED:

   DATA lEof
   DATA lBof

ENDCLASS


METHOD New( nSocket, cQuery, loRow ) CLASS TMySQLQuery

   local nI, aField, rc, bBlock

   Default loRow to .t.

   ::nSocket       := nSocket
   ::cQuery        := cQuery

   ::lError        := .F.
   ::aFieldStruct  := {}
   ::nCurRow       := 1
   ::nResultHandle := nil
   ::nNumFields    := 0
   ::nNumRows      := 0
   ::loRow         := loRow


   if ( rc := sqlQuery( nSocket, cQuery ) ) == 0

      // save result set
      if ( ::nResultHandle := sqlStoreR( nSocket ) ) > 0

         ::nNumRows     := sqlNRows( ::nResultHandle )
         ::nNumFields   := sqlNumFi( ::nResultHandle )

         ::aFieldStruct := Array( ::nNumFields )
         ::aRow         := Array( ::nNumFields )

         if ::nNumRows > 0
            ::lEof      := .f.
            ::lBof      := .f.
         else
            ::lEof      := .t.
            ::lBof      := .t.
         endif

         for each aField in ::aFieldStruct

            aField := sqlFetchF( ::nResultHandle )

            bBlock := ArrayBlock( HB_EnumIndex() )

            __objAddInline( Self,     aField[ MYSQL_FS_NAME ], bBlock )
            __objAddInline( Self, "_"+aField[ MYSQL_FS_NAME ], bBlock )

         next

         ::getRow( ::nCurRow )

      else
         // Should query have returned rows? (Was it a SELECT like query?)

         if ( ::nNumFields := sqlNumFi( nSocket ) ) == 0

            // Was not a SELECT so reset ResultHandle changed by previous sqlStoreR()
            ::nResultHandle := nil
            ::lEof   := .t.
            ::lBof   := .t.
         else
            ::lError := .T.

         endif
      endif

   else
      ::lError := .T.

   endif

return Self


METHOD Refresh() CLASS TMySQLQuery

   local rc

   // free present result handle
   sqlFreeR( ::nResultHandle )

   ::lError := .F.

   if ( rc := sqlQuery( ::nSocket, ::cQuery ) ) == 0

      // save result set
      ::nResultHandle := sqlStoreR( ::nSocket )
      ::nNumRows := sqlNRows( ::nResultHandle )

      // NOTE: I presume that number of fields doesn't change (that is nobody alters this table) between
      // successive refreshes of the same

      // But row number could very well change
      if ::nCurRow > ::nNumRows
         ::nCurRow := ::nNumRows
      endif

      ::getRow( ::nCurRow )

   else
      ::lError := .T.

   endif

return !::lError


METHOD Skip( nRows ) CLASS TMySQLQuery
Local nOldrow := ::nCurRow
   // NOTE: MySQL row count starts from 0
   default nRows to 1

   if ::nNumRows > 0
      ::lBof := .f.
      ::lEof := .f.
   else
      ::lBof := .t.
      ::lEof := .t.
   endif

   if nRows == 0
      // No move

   elseif nRows < 0
      // Negative movement
      if (::nCurRow += nRows) < 1
         ::nCurRow := 0
         ::lBof    := .t.
      endif

   else
      // positive movement
      if (::nCurRow += nRows) > ::nNumRows
         ::nCurRow := ::nNumRows + 1
         ::lEof    := .t.
      endif

   endif

   ::getRow( ::nCurRow )

return ::nCurRow - nOldRow


// Get row n of a query and return it as a TMySQLRow object
METHOD GetRow( nRow, loRow, lSkip ) CLASS TMySQLQuery

   local cType, xField
   local cDateFormat := Lower( Set( 4 ) )

   default loRow to ::loRow
   default nRow  to ::nCurRow
   default lSkip to .f.

   if ::nResultHandle <> NIL
      if lSkip
         nRow := ::nCurRow + 1
      endif

      if ::nNumRows == 0
         ::lBof    := .t.
         ::lEof    := .t.
         ::nCurRow := 1
      else
         if nRow < 1
            ::lBof    := .t.
            ::lEof    := .t.
            ::nCurRow := ::nNumRows + 1
         endif
         if nRow > 0 .and. nRow <= ::nNumRows //- 1
            ::lBof    := .f.
            ::lEof    := .f.
            ::nCurRow := nRow
         endif
         if nRow > ::nNumRows
            ::lBof    := .f.
            ::lEof    := .t.
            ::nCurRow := ::nNumRows + 1
         endif
      endif
      nRow := ::nCurRow

      if nRow > 0 .AND. nRow <= ::nNumRows

         // NOTE: row count starts from 0
         sqlDataS( ::nResultHandle, nRow - 1 )
         ::nCurRow := nRow
         ::aRow := sqlFetchR( ::nResultHandle )

      elseif nRow == ::nNumRows + 1
         ::aRow := Array( Len( ::aFieldStruct ) )
         Afill( ::aRow, "" )

      else
         ::aRow := NIL
      endif


      if ::aRow <> NIL

         // Convert answer from text field to correct clipper types
         for each xField in ::aRow

            cType := SQL2ClipType( ::aFieldStruct[ HB_EnumIndex() ][ MYSQL_FS_TYPE ] )

            switch cType
               case "L"
                  xField := !( Val( xField ) == 0 )
                  exit

               case "N"
               case "I"
               case "T"
                  xField := Val( xField )
                  exit

               case "D"
                  if Empty(xField)
                     xField := CToD( "" )

                  elseif cDateFormat = 'mm-dd-yyyy' // USA
                     xField := ctod(substr(xField,6,2)+"-"+right(xField,2,0)+ "-" + Left(xField, 4))

                  elseif  cDateFormat = 'dd/mm/yyyy' .or. cDateFormat = 'dd/mm/yy' // BRITISH ou FRENCH
                     xField :=  ctod(right(xField,2,0)+ "/"+ substr(xField,6,2)+"/"+ Left(xField, 4))

                  elseif cDateFormat = 'yyyy.mm.dd' // ANSI
                     xField := ctod(Left(xField, 4)+ "."+substr(xField,6,2)+"."+right(xField,2,0)) 

                  elseif cDateFormat = 'dd.mm.yyyy' //GERMAN
                     xField :=ctod(right(xField,2,0)+ "."+ substr(xField,6,2)+"."+ Left(xField, 4 ))

                  elseif cDateFormat = 'dd-mm-yyyy'  //ITALIAN
                     xField :=ctod(right(xField,2,0)+ "-"+ substr(xField,6,2)+"-"+ Left(xField, 4))

                  elseif cDateFormat = 'yyyy/mm/dd' //JAPAN
                     xField :=  ctod(Left(xField, 4)+ "/"+substr(xField,6,2)+"/"+right(xField,2,0)) 

                  elseif cDateFormat = 'mm/dd/yyyy' // AMERICAN
                     xField :=  ctod(substr(xField,6,2)+"/"+right(xField,2,0)+ "/" + Left(xField, 4))
                  else
                     xField := "''"

                  endif
                  exit

               case "C"
                  xField := PadR( xField , ::aFieldStruct[ HB_EnumIndex() ][ MYSQL_FS_LENGTH ] )
               case "M"
               case "B"
                  // Character or Memo field
                  exit

               default

//                  Alert("Unknown type from SQL Server Field: " + LTrim(Str(i))+" is type "+LTrim(Str(nType)))

            end

//            __objsetValuelist(Self,{{::aFieldStruct[i][MYSQL_FS_NAME],xField}})

         next

         if loRow
            Return TMySQLRow():New( ::aRow, ::aFieldStruct )
         endif

      endif

   endif

return nil


// Given a field name returns it's position
METHOD FieldPos( cFieldName ) CLASS TMySQLQuery

   local cUpperName, nPos := 0

   cUpperName := Upper( cFieldName )

   nPos := AScan( ::aFieldStruct, {| aItem | Upper( aItem[ MYSQL_FS_NAME ] ) == cUpperName } )

return nPos


// Returns name of field N
METHOD FieldName( nNum ) CLASS TMySQLQuery

   if nNum >= 1 .AND. nNum <= Len( ::aFieldStruct )
      return ::aFieldStruct[ nNum ][ MYSQL_FS_NAME ]
   endif

return ""

METHOD FieldGet(cnField) CLASS TMySQLQuery

   local nNum, Value

   if ValType( cnField ) == "C"
      nNum := ::FieldPos( cnField )
   else
      nNum := cnField
   endif

   if nNum > 0 .AND. nNum <= ::nNumfields
//       Value :=  __objsendmsg(Self,::aFieldStruct[nNum][MYSQL_FS_NAME])
      Value := ::aRow[ nNum ]

      // Char fields are padded with spaces since a real .dbf field would be
      if ::FieldType( nNum ) == "C"
         return PadR( Value, ::aFieldStruct[ nNum ][ MYSQL_FS_LENGTH ] )
      else
         return Value
      endif

   endif

return nil


METHOD FieldLen(nNum) CLASS TMySQLQuery

   if nNum > 0 .AND. nNum <= ::nNumFields
      return ::aFieldStruct[ nNum ][ MYSQL_FS_LENGTH ]
   endif

return 0


METHOD FieldDec( nNum ) CLASS TMySQLQuery

   if nNum > 0 .AND. nNum <= ::nNumFields
      return ::aFieldStruct[ nNum ][ MYSQL_FS_DECIMALS ]
   endif

return 0


METHOD FieldType( nNum ) CLASS TMySQLQuery

   local cType := "U"

   if nNum >= 0 .AND. nNum <= ::nNumFields

      cType := SQL2ClipType( ::aFieldStruct[ nNum ][ MYSQL_FS_TYPE ] )

   endif

return cType


METHOD Locate( cFieldName, Value, bPartialKey, bSoftSeek ) CLASS TMySQLQuery
local nRecPrec := ::recno(), bFound := .F.

   //bSoftSeek cause the record pointer to be moved to the next record 
   
   if bSoftSeek   == NIL ; bSoftSeek := .F. ; endif         
   if bPartialKey == NIL ; bPartialKey := .T. ; endif

   ::gotop()
   while ! ::eof() 
     bFound := (::FieldGet(::FieldPos(cFieldName)) == Value) .or. ((::FieldGet(::FieldPos(cFieldName)) = Value) .and. bPartialKey)
     
     if !bFound .and. ((::FieldGet(::FieldPos(cFieldName)) > Value) .and. bSoftSeek)
       bFound := .T.
     endif
     
     if bFound
       exit
     endif
     
     ::skip()
   enddo

   if !bFound
     ::goto(nRecPrec)
   endif

return bFound



/* ----------------------------------------------------------------------------------------*/

// A Table is a query without joins; this way I can Insert() e Delete() rows.
// NOTE: it's always a SELECT result, so it will contain a full table only if
//       SELECT * FROM ... was issued
CLASS TMySQLTable FROM TMySQLQuery

   DATA  cTable               // name of table
   DATA  aOldValue         //  keeps a copy of old value

   METHOD   New( nSocket, cQuery, cTableName, loRow )
   METHOD   GetRow( nRow, loRow, lSkip )
   METHOD   Skip( nRow )

   METHOD   Update( oRow )                // Gets an oRow and updates changed fields
   METHOD   Save( oRow )   INLINE ::Update( oRow )
   METHOD   Delete( oRow )                // Deletes passed row from table
   METHOD   Append( oRow )                // Inserts passed row into table

   METHOD   GetBlankRow( loRow )          // Returns an empty row with all available fields empty
   METHOD   Blank( loRow ) INLINE ::GetBlankRow( loRow )

   METHOD   FieldPut( cnField, Value )    // field identifier, not only a number
   METHOD   Refresh()
   METHOD   MakePrimaryKeyWhere()         // returns a WHERE x=y statement which uses primary key (if available)

ENDCLASS


METHOD New(nSocket, cQuery, cTableName, loRow) CLASS TMySQLTable

Local xValue

   super:New(nSocket, AllTrim(cQuery), loRow)

   ::cTable    := Lower(cTableName)
   ::aOldValue := Array( ::nNumFields )

   for each xValue in ::aOldValue
      xValue := ::fieldget( HB_EnumIndex() )
   next

return Self


METHOD GetRow( nRow, loRow, lSkip ) CLASS TMySQLTable

   local oRow := super:GetRow( nRow, loRow, lSkip ), xValue

   if oRow <> NIL
      oRow:cTable := ::cTable
   endif

   ::aOldvalue := Array( ::nNumFields )

   for each xValue in ::aOldValue
      xValue := ::fieldget( HB_EnumIndex() )
   next

return oRow


METHOD Skip(nRow) CLASS TMySQLTable
   Local xValue, nSkipRows

     nSkipRows := super:skip(nRow)

     for each xValue in ::aOldValue
        xValue := ::fieldget( HB_EnumIndex() )
     next

return nSkipRows


/* Creates an update query for changed fields and submits it to server */
METHOD Update( oRow ) CLASS TMySQLTable

   local cUpdateQuery := "UPDATE " + ::cTable + " SET "
   local xValue

   ::lError := .F.


          // default Current row
   if oRow == nil

         for each xValue in ::aOldValue
            if xValue == NIL .or. xValue <> ::FieldGet( HB_EnumIndex() )
               cUpdateQuery += ::aFieldStruct[ HB_EnumIndex() ][MYSQL_FS_NAME] + "=" + ClipValue2SQL(::FieldGet( HB_EnumIndex() ),::FieldType( HB_EnumIndex() )) + ","
            endif
         next

         // no Change
         if right(cUpdateQuery,4)=="SET "; return !::lError; end

         // remove last comma
         cUpdateQuery := Left(cUpdateQuery, Len(cUpdateQuery) -1)


         cUpdateQuery += ::MakePrimaryKeyWhere()

// alert( cUpdateQuery )

         if sqlQuery( ::nSocket, cUpdateQuery ) == 0

            ::refresh()
            for each xValue in ::aOldValue
               xValue := ::fieldget( HB_EnumIndex() )
            next

         else
            ::lError := .T.

         endif

   else

      WITH OBJECT oRow

         if :cTable == ::cTable

            for each xValue in :aRow
               if :aDirty[ HB_EnumIndex() ]
                  cUpdateQuery += :aFieldStruct[ HB_EnumIndex() ][ MYSQL_FS_NAME ] + "=" + ClipValue2SQL( :aRow[ HB_EnumIndex() ], :FieldType( HB_EnumIndex() ) ) + ","
               endif
            next

            // remove last comma
            cUpdateQuery := Left( cUpdateQuery, Len(cUpdateQuery ) - 1 )

            cUpdateQuery += :MakePrimaryKeyWhere()

// alert( cUpdateQuery )

            if sqlQuery( ::nSocket, cUpdateQuery ) == 0

               // All values are commited
               Afill( :aDirty   , .F. )
               Afill( :aOldValue, nil )

            else
               ::lError := .T.

            endif

         endif
      END  // WITH
   endif

return !::lError


METHOD Delete( oRow ) CLASS TMySQLTable

   local cDeleteQuery := "DELETE FROM " + ::cTable, xValue

   // is this a row of this table ?
   
   if oRow == nil

         cDeleteQuery += ::MakePrimaryKeyWhere()

         if sqlQuery( ::nSocket, cDeleteQuery ) == 0
            ::lError := .F.
            // ::nCurRow--
            ::refresh()

            for each xValue in ::aOldValue
               xValue := ::fieldget( HB_EnumIndex() )
            next

         else
            ::lError := .T.

         endif

   else

      WITH OBJECT oRow
         if :cTable == ::cTable

            cDeleteQuery += :MakePrimaryKeyWhere()

            if sqlQuery( ::nSocket, cDeleteQuery ) == 0
               ::lError := .F.

            else
               ::lError := .T.

            endif

          endif
      END  // WITH
  Endif

return !::lError


// Adds a row with values passed into oRow
METHOD Append( oRow ) CLASS TMySQLTable

   local cInsertQuery := "INSERT INTO " + ::cTable + " ("
   local xValue

           // default Current row
   if oRow == nil

            // field names
            for each xValue in ::aFieldStruct
               if xValue[ MYSQL_FS_FLAGS ] <> AUTO_INCREMENT_FLAG
                  cInsertQuery += xValue[ MYSQL_FS_NAME ] + ","
               endif
            next

            // remove last comma from list
            cInsertQuery := Left( cInsertQuery, Len( cInsertQuery ) - 1 ) + ") VALUES ("

            // field values
            for each xValue in ::aFieldStruct
               if xValue[ MYSQL_FS_FLAGS ] <> AUTO_INCREMENT_FLAG
                  cInsertQuery += ClipValue2SQL( ::FieldGet(HB_EnumIndex() ), ::FieldType(HB_EnumIndex()) ) + ","
               endif
            next

            // remove last comma from list of values and add closing parenthesis
            cInsertQuery := Left( cInsertQuery, Len(cInsertQuery) - 1 ) + ")"

// alert( cInsertQuery )

            if sqlQuery( ::nSocket, cInsertQuery ) == 0

               ::refresh()
               for each xValue in ::aOldValue
                  xValue := ::fieldget( HB_EnumIndex() )
               next
               return .T.
            else
               ::lError := .T.
            endif


   else

      WITH OBJECT oRow
         if :cTable == ::cTable

            // field names
            for each xValue in :aFieldStruct
               if xValue[ MYSQL_FS_FLAGS ] <> AUTO_INCREMENT_FLAG
                  cInsertQuery += xValue[ MYSQL_FS_NAME ] + ","
               endif
            next
            // remove last comma from list
            cInsertQuery := Left( cInsertQuery, Len( cInsertQuery ) - 1 ) + ") VALUES ("

            // field values
            for each xValue in :aRow
               if :aFieldStruct[ HB_EnumIndex() ][ MYSQL_FS_FLAGS ] <> AUTO_INCREMENT_FLAG
                  cInsertQuery += ClipValue2SQL( xValue, :FieldType(HB_EnumIndex()) ) + ","
               endif
            next

            // remove last comma from list of values and add closing parenthesis
            cInsertQuery := Left( cInsertQuery, Len( cInsertQuery ) - 1 ) + ")"

// alert( cInsertQuery )

            if sqlQuery( ::nSocket, cInsertQuery ) == 0
               return .T.
            else
               ::lError := .T.
            endif

         endif
      END  // WITH
   Endif
return .F.


METHOD GetBlankRow( loRow ) CLASS TMySQLTable

   local cType
   local xValue

   Default loRow to ::loRow

   for each xValue in ::aRow

      cType := SQL2ClipType( ::aFieldStruct[ HB_EnumIndex() ][ MYSQL_FS_TYPE ] )

      switch cType
      case "C"
      case "M"
      case "B"
         ::aOldValue[ HB_EnumIndex() ] := xValue := ""
         exit

      case "N"
      case "I"
         ::aOldValue[ HB_EnumIndex() ] := xValue := 0
         exit

      case "L"
         ::aOldValue[ HB_EnumIndex() ] := xValue := .F.
         exit

      case "D"
         ::aOldValue[ HB_EnumIndex() ] := xValue := CToD("")
         exit

      default
         ::aOldValue[ HB_EnumIndex() ] := xValue := nil

      end
   next


   if loRow
      return TMySQLRow():New( ::aRow, ::aFieldStruct, ::cTable )
   endif

return nil


METHOD FieldPut( cnField, Value ) CLASS TMySQLTable

   local nNum

   if ValType( cnField ) == "C"
      nNum := ::FieldPos( cnField )
   else
      nNum := cnField
   endif

   if nNum > 0 .AND. nNum <= ::nNumFields

//      if Valtype( Value ) == Valtype( ::FieldGet( nNum ) ) .OR. ::Fieldget( nNum ) == NIL )
      if Valtype( Value ) == Valtype( ::aRow[ nNum ] ) .OR. ::aRow[ nNum ] == NIL

         // if it is a char field remove trailing spaces
         if ValType( Value ) == "C"
            Value := RTrim( Value )
         endif

         ::aRow[ nNum ] := Value

         return Value
      endif
   endif

return nil

METHOD Refresh() CLASS TMySQLTABLE

   local rc

   // free present result handle
   sqlFreeR( ::nResultHandle )

   ::lError := .F.

   if ( rc := sqlQuery( ::nSocket, ::cQuery ) ) == 0

      // save result set
      ::nResultHandle := sqlStoreR( ::nSocket )
      ::nNumRows      := sqlNRows( ::nResultHandle )

      // NOTE: I presume that number of fields doesn't change (that is nobody alters this table) between
      // successive refreshes of the same

      // But row number could very well change
      if ::nCurRow > ::nNumRows
         ::nCurRow := ::nNumRows
      endif

      ::getRow( ::nCurRow )

   else
      ::lError := .T.

   endif

return !::lError


// returns a WHERE x=y statement which uses primary key (if available)
METHOD MakePrimaryKeyWhere() CLASS TMySQLTable

   local ni, cWhere := " WHERE ", aField

   for each aField in ::aFieldStruct

      // search for fields part of a primary key
      if ( sqlAND( aField[ MYSQL_FS_FLAGS ], PRI_KEY_FLAG ) == PRI_KEY_FLAG ) .OR.;
         ( sqlAND( aField[ MYSQL_FS_FLAGS ], MULTIPLE_KEY_FLAG ) == MULTIPLE_KEY_FLAG )

         cWhere += aField[ MYSQL_FS_NAME ] + "="

         // if a part of a primary key has been changed, use original value

            cWhere += ClipValue2SQL( ::aOldValue[ HB_EnumIndex() ]  )

         cWhere += " AND "
      endif

   next

   // remove last " AND "
   cWhere := Left( cWhere, Len( cWhere ) - 5 )

return cWhere



/* ----------------------------------------------------------------------------------------*/

// Every available MySQL server
CLASS TMySQLServer

   DATA  nSocket                 // connection handle to server (currently pointer to a MYSQL structure)
   DATA  cServer                 // server name
   DATA  cDBName                 // Selected DB
   DATA  cUser                   // user accessing db
   DATA  cPassword               // his/her password
   DATA  lError                  // .T. if occurred an error
   DATA  cCreateQuery

   METHOD   New( cServer, cUser, cPassword ) // Opens connection to a server, returns a server object

   METHOD   Destroy()    INLINE sqlClose( ::nSocket ), Self
                                             // Closes connection to server

   METHOD   SelectDB( cDBName )              // Which data base I will use for subsequent queries

   METHOD   CreateDatabase( cDataBase )      // Create an New Mysql Database

   METHOD   ListDBs()            INLINE sqlListDB(::nSocket)
                                             // returns an array with list of data bases available

   METHOD   DBExist( cDB )       INLINE ( cDB IN ::ListDBs() )
                                             // return .T. if cTable exist in the database

   METHOD   CreateTable( cTable, aStruct, cPrimaryKey, cUniqueKey, cAuto)
                                             // Create new table using the same syntax of dbCreate()

   METHOD   DeleteTable( cTable )            // delete table

   METHOD   TableExist( cTable ) INLINE ( cTable IN ::ListTables() )
                                             // return .T. if cTable exist in the database

   METHOD   ListTables()         INLINE sqlListTbl(::nSocket)
                                             // returns an array with list of available tables in current database

   METHOD   TableStruct( cTable )            // returns a structure array compatible with clipper's dbStruct() ones

   METHOD   CreateIndex( cName, cTable, aFNames, lUnique )
                                             // Create an index (unique) on field name(s) passed as an array of strings aFNames

   METHOD   DeleteIndex( cName, cTable )     // Delete index cName from cTable


   METHOD   Query( cQuery, loRow )           // Gets a textual query and returns a TMySQLQuery or TMySQLTable object

   METHOD   NetErr()             INLINE ::lError
                                             // Returns .T. if something went wrong

   METHOD   Error()                          // Returns textual description of last error

ENDCLASS


METHOD New( cServer, cUser, cPassword ) CLASS TMySQLServer

   ::cServer   := cServer
   ::cUser     := cUser
   ::cPassword := cPassword
   ::nSocket   := sqlConnect(cServer, cUser, cPassword)
   ::lError    := .F.

   if ::nSocket == 0
      ::lError := .T.
   endif

return Self


METHOD SelectDB( cDBName ) CLASS TMySQLServer

   ::lError := .F.

   if sqlSelectD( ::nSocket, cDBName ) != 0   // table not exist
      ::cDBName :=""
      ::lError  := .T.
   else                                       // table exist
      ::cDBName := cDBName
      ::lError  := .F.
      return .T.
   endif

return .F.



METHOD CreateDatabase ( cDataBase ) CLASS TMySQLServer
   local cCreateQuery := "CREATE DATABASE "+ lower( cDatabase )

   if sqlQuery( ::nSocket, cCreateQuery ) == 0
      return .T.
   endif

return .F.


// NOTE: OS/2 port of MySQL is picky about table names, that is if you create a table with
// an upper case name you cannot alter it (for example) using a lower case name, this violates
// OS/2 case insensibility about names
METHOD CreateTable( cTable, aStruct, cPrimaryKey, cUniqueKey, cAuto ) CLASS TMySQLServer

   /* NOTE: all table names are created with lower case */

   local aField

   // returns NOT NULL if extended structure has DBS_NOTNULL field to true
   local cNN := {| aArr | iif( Len( aArr ) > DBS_DEC, iif( aArr[ DBS_NOTNULL ], " NOT NULL ", "" ), "" ) }


   ::cCreateQuery := "CREATE TABLE " + Lower(cTable) + " ("


   for each aField in aStruct

      switch aField[ DBS_TYPE ]
      case "C"
         ::cCreateQuery += aField[ DBS_NAME ] + " char(" + AllTrim(Str(aField[DBS_LEN])) + ")" + Eval(cNN, aField)+ if(aField[DBS_NAME]==cPrimaryKey," NOT NULL ",'' )+ ","
         exit

      case "M"
         ::cCreateQuery += aField[ DBS_NAME ] + " text" + Eval(cNN, aField) + ","
         exit

      case "N"

         if aField[ DBS_DEC ] == 0 .and. aField[ DBS_LEN ] <= 18

            do case
               case aField[ DBS_LEN ] <= 4
                  ::cCreateQuery += aField[ DBS_NAME ] + " smallint("  + AllTrim( Str( aField[ DBS_LEN ] ) ) + ")"

               case aField[ DBS_LEN ] <= 6
                  ::cCreateQuery += aField[ DBS_NAME ] + " mediumint(" + AllTrim( Str( aField[ DBS_LEN ] ) ) + ")"

               case aField[ DBS_LEN ] <= 9
                  ::cCreateQuery += aField[ DBS_NAME ] + " int("       + AllTrim( Str( aField[ DBS_LEN ] ) ) + ")"

               otherwise
                  ::cCreateQuery += aField[ DBS_NAME ] + " bigint("    + AllTrim( Str( aField[ DBS_LEN ] ) ) + ")"

            endcase

            ::cCreateQuery += Eval( cNN, aField ) + if( aField[ DBS_NAME ] == cPrimaryKey, " NOT NULL ", "" ) + if( aField[ DBS_NAME ] == cAuto, " auto_increment ", "" ) + ","

         else
            ::cCreateQuery += aField[ DBS_NAME ] + " real(" + AllTrim( Str( aField[ DBS_LEN ] ) ) + "," + AllTrim( Str( aField[ DBS_DEC ] ) ) + ")" + Eval( cNN, aField ) + ","

         endif
         exit

      case "D"
         ::cCreateQuery += aField[ DBS_NAME ] + " date " + Eval( cNN, aField ) + ","
         exit

      case "L"
         ::cCreateQuery += aField[ DBS_NAME ] + " tinyint " + Eval( cNN, aField ) + ","
         exit

      case "B"
         ::cCreateQuery += aField[ DBS_NAME ] + " mediumblob " + Eval( cNN, aField ) + ","
         exit

      case "I"
         ::cCreateQuery += aField[ DBS_NAME ] + " mediumint " + Eval( cNN, aField ) + ","
         exit

      case "T"
         ::cCreateQuery += aField[ DBS_NAME ] + " timestamp(" + AllTrim( Str( aField[ DBS_LEN ] ) ) + ")" + Eval( cNN, aField ) + ","
         exit

      default
         ::cCreateQuery += aField[ DBS_NAME ] + " char(" + AllTrim(Str(aField[DBS_LEN])) + ")" + Eval( cNN, aField ) + ","

      end

   next

   if cPrimarykey != NIL
        ::cCreateQuery += ' PRIMARY KEY (' + cPrimaryKey + '),'
   endif

   if cUniquekey != NIL
        ::cCreateQuery += ' UNIQUE ' + cUniquekey + ' (' + cUniqueKey + '),'
   endif

   // remove last comma from list
   ::cCreateQuery := Left( ::cCreateQuery, Len( ::cCreateQuery ) - 1 ) + ");"

   if sqlQuery( ::nSocket, ::cCreateQuery ) == 0
      return .T.
   else
      ::lError := .T.
   endif

return .F.


METHOD CreateIndex( cName, cTable, aFNames, lUnique ) CLASS TMySQLServer

   local cCreateQuery := "CREATE "
   local cField

   default lUnique to .F.

   if lUnique
      cCreateQuery += "UNIQUE INDEX "
   else
      cCreateQuery += "INDEX "
   endif

   cCreateQuery += cName + " ON " + Lower( cTable ) + " ("

   for each cField in aFNames
      cCreateQuery += cField + ","
   next

   // remove last comma from list
   cCreateQuery := Left( cCreateQuery, Len( cCreateQuery ) - 1 ) + ")"

   if sqlQuery( ::nSocket, cCreateQuery ) == 0
      return .T.
   endif

return .F.


METHOD DeleteIndex( cName, cTable ) CLASS TMySQLServer

   local cDropQuery := "DROP INDEX " + cName + " FROM " + Lower( cTable )

   if sqlQuery( ::nSocket, cDropQuery ) == 0
      return .T.
   endif

return .F.


METHOD DeleteTable( cTable ) CLASS TMySQLServer

   local cDropQuery := "DROP TABLE " + Lower( cTable )

   if sqlQuery( ::nSocket, cDropQuery ) == 0
      return .T.

   endif

return .F.


METHOD Query( cQuery, loRow ) CLASS TMySQLServer

   local oQuery, cTableName, i, cUpperQuery, nNumTables, cToken

   default cQuery to ""


   cUpperQuery := Upper( AllTrim( cQuery ) )
   i           := 1
   nNumTables  := 1

   while (cToken := __StrToken( cUpperQuery, i++, " " ) ) <> "FROM" .AND. !Empty( cToken )
   enddo

   // first token after "FROM" is a table name
   // NOTE: SubSelects ?
   cTableName := __StrToken( cUpperQuery, i++, " " )

   while ( cToken := __StrToken( cUpperQuery, i++, " " ) ) <> "WHERE" .AND. !Empty( cToken )
      // do we have more than one table referenced ?
      if cToken == "," .OR. cToken == "JOIN"
         nNumTables++
      endif
   enddo

   if nNumTables == 1
      oQuery := TMySQLTable():New( ::nSocket, cQuery, cTableName, loRow )
   else
      oQuery := TMySQLQuery():New(::nSocket, cQuery, loRow )
   endif

   if oQuery:NetErr()
      ::lError := .T.
   endif

return oQuery


METHOD Error() CLASS TMySQLServer

   ::lError := .F.

return iif(::nSocket > 0, sqlGetErr(::nSocket), "No connection to server")


/* TOFIX: Conversion creates a .dbf with fields of wrong dimension (often) */
METHOD TableStruct( cTable ) CLASS TMySQLServer

   local nRes, aField, aStruct, aSField, i


   aStruct := {}

   /* TODO: rewrite for MySQL */
   nRes := sqlListF( ::nSocket, cTable )

   if nRes > 0
      for i := 1 to sqlNumFi( nRes )

         aField  := sqlFetchF( nRes )
         aSField := Array( DBS_DEC )

         // don't count indexes as real fields
//         if aField[ MYSQL_FS_TYPE ] <= MYSQL_LAST_REAL_TYPE

            aSField[ DBS_NAME ] := Left( aField[ MYSQL_FS_NAME ], 10 )
            aSField[ DBS_DEC  ] := 0

            asField[ DBS_TYPE ] :=  SQL2ClipType( aField[ MYSQL_FS_TYPE ] )

            switch aField[ MYSQL_FS_TYPE ]
            case MYSQL_TINY_TYPE
               aSField[ DBS_TYPE ] := "L"
               aSField[ DBS_LEN ]  := 1
               exit

            case MYSQL_SHORT_TYPE
               aSField[ DBS_TYPE ] := "N"
               aSField[ DBS_LEN ]  := Min( 6, aField[ MYSQL_FS_LENGTH ] )
               exit

            case MYSQL_INT24_TYPE
               aSField[ DBS_TYPE ] := "N"
               aSField[ DBS_LEN ]  := Min( 8, aField[ MYSQL_FS_LENGTH ] )
               exit

            case MYSQL_LONG_TYPE
               aSField[ DBS_TYPE ] := "N"
               aSField[ DBS_LEN ]  := Min( 11, aField[ MYSQL_FS_LENGTH ] )
               exit

            case MYSQL_LONGLONG_TYPE
               aSField[ DBS_TYPE ] := "N"
               aSField[ DBS_LEN ]  := Min( 20, aField[ MYSQL_FS_LENGTH ] )
               exit

            case MYSQL_FLOAT_TYPE
            case MYSQL_DOUBLE_TYPE
            case MYSQL_DECIMAL_TYPE
               aSField[ DBS_TYPE ] := "N"
               aSField[ DBS_LEN ] := aField[ MYSQL_FS_LENGTH ]
               aSFIeld[ DBS_DEC ] := aField[ MYSQL_FS_DECIMALS ]
               exit

/*            case FIELD_TYPE_INT24
               aSField[ DBS_TYPE ] := "I"
               aSField[ DBS_LEN ] := aField[ MYSQL_FS_LENGTH ]
               aSFIeld[ DBS_DEC ] := aField[ MYSQL_FS_DECIMALS ]
               exit  */

            case MYSQL_STRING_TYPE
            case MYSQL_VAR_STRING_TYPE
            case MYSQL_DATETIME_TYPE
            case MYSQL_TIME_TYPE
               aSField[ DBS_TYPE ] := "C"
               aSField[ DBS_LEN ] := aField[ MYSQL_FS_LENGTH ]
               exit

            case MYSQL_DATE_TYPE
               aSField[ DBS_TYPE ] := "D"
               aSField[ DBS_LEN ] := 8
               exit

            case MYSQL_MEDIUM_BLOB_TYPE
               aSField[ DBS_TYPE ] := "B"
               aSField[ DBS_LEN ] := aField[ MYSQL_FS_LENGTH ]
               exit

            case MYSQL_BLOB_TYPE
               aSField[ DBS_TYPE ] := "M"
               aSField[ DBS_LEN ] := 10
               exit

            case MYSQL_TIMESTAMP_TYPE
               aSField[ DBS_TYPE ] := "N"
               aSField[ DBS_LEN ]  := aField[ MYSQL_FS_LENGTH ]
               exit

            default
               aSField[ DBS_TYPE ] := "C"
               aSField[ DBS_LEN ] := aField[ MYSQL_FS_LENGTH ]

            end

            AAdd( aStruct, aSField )
//         endif
      next

      sqlFreeR( nRes )

   endif

return aStruct


// Returns an SQL string with clipper value converted ie. Date() -> "'YYYY-MM-DD'"
static function ClipValue2SQL(Value, cType)

   local cValue := ""
   local cDateFormat := Lower( Set( 4 ) )

   Default cType to ValType( Value )

   switch cType
      case "N"
      case "I"
         cValue := AllTrim( Str( Value ) )
         exit

      case "D"
         if !Empty( Value )

            if cDateFormat == 'mm-dd-yyyy' // USA
               cValue := "'"+PadL(Month(Value), 2, "0") + '-'+ PadL(Day(Value), 2, "0") + "-" + Str(Year(Value), 4) + "'"
            elseif  cDateFormat == 'dd/mm/yyyy' // BRITISH ou FRENCH
               //cValue := "'"+PadL(Day(Value), 2, "0") + "/" + PadL(Month(Value), 2, "0") + "/" + Str(Year(Value), 4) + "'"
               cValue := "'"+Str(Year(Value), 4) + "-" + PadL(Month(Value), 2, "0") + "-" + PadL(Day(Value), 2, "0") + "'"
            elseif cDateFormat == 'yyyy.mm.dd' // ANSI
               cValue := "'"+Str(Year(Value), 4)  + "." + PadL(Month(Value), 2, "0") + "." + PadL(Day(Value), 2, "0") + "'"
            elseif cDateFormat == 'dd.mm.yyyy' //GERMAN
               cValue := "'"+PadL(Day(Value), 2, "0") + "." + PadL(Month(Value), 2, "0") + "." + Str(Year(Value), 4) +  "'"
            elseif cDateFormat == 'dd-mm-yyyy'  //ITALIAN
               cValue := "'"+PadL(Day(Value), 2, "0") + "-" + PadL(Month(Value), 2, "0") + "-" + Str(Year(Value), 4)  + "'"
            elseif cDateFormat == 'yyyy/mm/dd' //JAPAN
               cValue := "'"+Str(Year(Value), 4)  + "/" + PadL(Month(Value), 2, "0") + "/" + PadL(Day(Value), 2, "0") + "'"
            elseif cDateFormat == 'mm/dd/yyyy' // AMERICAN
               cValue := "'"+Str(Year(Value), 4)     + "/" + PadL(Month(Value), 2, "0") + "/" + PadL(Day(Value), 2, "0") + "'"
            endif

         else
            cValue := "''"
         endif
         exit

      case "C"
      case "M"
      case "B"
         IF Empty( Value)
            cValue := "''"
         ELSE
            cValue := "'" + DATATOSQL( value ) + "'"
         ENDIF
         exit

      case "L"
         cValue := AllTrim( Str( iif(Value, 1, 0 ) ) )
         exit

      case "T"
         cValue := iif( Value < 0, "NULL", Alltrim( str( Value ) ) )
         exit

      default
         cValue := "''"       // NOTE: Here we lose values we cannot convert

   end

return cValue


static function SQL2ClipType( nType )

   switch nType
      case MYSQL_TINY_TYPE
         Return "L"

      case MYSQL_SHORT_TYPE
      case MYSQL_LONG_TYPE
      case MYSQL_LONGLONG_TYPE
      case MYSQL_FLOAT_TYPE
      case MYSQL_DOUBLE_TYPE
      case MYSQL_DECIMAL_TYPE
         Return "N"

      case MYSQL_DATE_TYPE
         Return "D"

      case MYSQL_BLOB_TYPE
         Return "M"

      case MYSQL_VAR_STRING_TYPE
      case MYSQL_STRING_TYPE
      case MYSQL_DATETIME_TYPE
      case MYSQL_TIME_TYPE
         Return "C"

      case MYSQL_INT24_TYPE
         Return "I"

      case MYSQL_MEDIUM_BLOB_TYPE
         Return "B"

      case MYSQL_TIMESTAMP_TYPE
         Return "T"

   end

Return "U"


/* Given a three letter month name gives back month number as two char string (ie. Apr -> 04) */
static function NMonth(cMonthValue)

   static cMonths := {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dec" }
   local nMonth

   nMonth := AScan(cMonths, cMonthValue)

return PadL(nMonth, 2, "0")


static function ARRAYBLOCK( nIndex )
   Local bBlock

   bBlock := {|Self, x | iif( PCount() == 1, ::aRow[ nIndex ], ::aRow[ nIndex ] := x ) }

return bBlock

