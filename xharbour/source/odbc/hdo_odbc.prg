/*
 * Harbour Project source code:
 * Harbour Data Object ODBC fast/simple access class
 *
 * Copyright 1999 Felipe G. Coury <fcoury@creation.com.br>
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

#include "hbclass.ch"
#include "common.ch"
#include "sql.ch"

*+--------------------------------------------------------------------
*+
*+    Class HDO_ODBC_Connection
*+    Manages HDO_ODBC_ Windows ODBC Connections
*+
*+--------------------------------------------------------------------
*+
CLASS HDO_ODBC_Connection FROM HBClass

   DATA Provider
   DATA ConnectionString

   DATA hEnv
   DATA hDbc

   METHOD New( cCnn )
   METHOD Close()
   METHOD SQLError()

ENDCLASS

METHOD New( cCnn ) CLASS HDO_ODBC_Connection

   LOCAL nRet
   LOCAL xBuf

   ::ConnectionString := cCnn

   SQLAllocEn( @xBuf )
   ::hEnv := xBuf

   SQLAllocCo( ::hEnv, @xBuf )
   ::hDbc := xBuf

   SQLDriverC( ::hDbc, ::ConnectionString, @xBuf )     // Connects to Driver
   ::Provider := xBuf

RETURN Self

METHOD Close() CLASS HDO_ODBC_Connection

   SQLDisconn( ::hDbc )                        // Disconnects from Driver
   SQLFreeCon( ::hDbc )                        // Frees the connection
   SQLFreeEnv( ::hEnv )                        // Frees the environment

RETURN NIL

METHOD SQLError( hStmt ) CLASS HDO_ODBC_Connection

   LOCAL cSqlState, cErrorMsg

   SQLError( ::hEnv, ::hDbc, hStmt, @cSqlState,, @cErrorMsg )

   alert( left( cErrorMsg, 70 ) + ";" + "SQLSTATE: " + cSqlState )

RETURN NIL

*+--------------------------------------------------------------------
*+
*+    Class HDO_ODBC_Command
*+    Manages HDO_ODBC_ Windows ODBC Commands
*+
*+--------------------------------------------------------------------
*+
CLASS HDO_ODBC_Command FROM HBClass

   DATA ActiveConnection
   DATA CommandText

   DATA hStmt
   
   METHOD New()
   METHOD Prepare()
   METHOD Execute()
   METHOD ExecuteDir()
   METHOD GetScalar()
   METHOD Close()
 
ENDCLASS

METHOD New( cCmd, oCnn ) CLASS HDO_ODBC_Command

   LOCAL xBuf

   ::CommandText := cCmd
   ::ActiveConnection := oCnn

   SQLAllocSt( ::ActiveConnection:hDbc, @xBuf )

   ::hStmt := xBuf

RETURN Self

METHOD Prepare() CLASS HDO_ODBC_Command

RETURN SQLPrepare( ::hStmt, ::CommandText )

METHOD Execute() CLASS HDO_ODBC_Command

RETURN SQLExecute( ::hStmt )

METHOD ExecuteDir() CLASS HDO_ODBC_Command

RETURN SQLExecDir( ::hStmt, ::CommandText )

METHOD GetScalar( xValue ) CLASS HDO_ODBC_Command

   LOCAL xRet

   xRet := SQLFetch( ::hStmt )

   if xRet <> SQL_NO_DATA_FOUND 
      xRet := SQLGetData( ::hStmt, 1, SQL_CHAR, 255, @xValue )
   endif

RETURN xRet

METHOD Close() CLASS HDO_ODBC_Command

   SQLFreeStm( ::hStmt )

   ::hStmt := nil

RETURN nil

*+--------------------------------------------------------------------
*+
*+    Class HDO_ODBC_RecordSet
*+    Manages HDO_ODBC_ Windows ODBC RecordSets
*+
*+--------------------------------------------------------------------
*+
CLASS HDO_ODBC_RecordSet FROM HBClass

   DATA ActiveCommand
   DATA ActiveConnection
   
   DATA hStmt

   DATA aFields
   DATA aValues
   DATA nNumFields

   METHOD New()
   METHOD Open()
   METHOD CLOSE()

   METHOD FieldPos( cField )
   METHOD Fields( cField )
   METHOD FieldAttr( cField, nAttr )

   METHOD Read()

ENDCLASS

METHOD New() CLASS HDO_ODBC_RecordSet

RETURN Self

METHOD Open( cSql, oCnn ) CLASS HDO_ODBC_RecordSet

   LOCAL i
   LOCAL nRet
   LOCAL nCols
   LOCAL cName
   LOCAL nNameLen
   LOCAL nType
   LOCAL nSize
   LOCAL nDecs
   LOCAL nNul

   IF empty( oCnn )

      RETURN SQL_ERROR

   ENDIF

   ::ActiveConnection := oCnn

   // SQL statement is mandatory
   IF empty( cSQL )

      // TODO: Some error here
      // SQL Statement not defined

      RETURN SQL_ERROR

   ENDIF

   // Allocates and executes the statement
   ::ActiveCommand := HDO_ODBC_Command():New( cSql, oCnn )

   IF ::ActiveCommand:ExecuteDir() <> SQL_SUCCESS
      oCnn:SQLError( ::ActiveCommand:hStmt )
      RETURN SQL_ERROR
   ENDIF
               
   ::hStmt := ::ActiveCommand:hStmt
   
   SQLNumRes( ::hStmt, @nCols )
   ::nNumFields := nCols

   ::aFields := TAssociativeArray() // array( nCols )

   FOR i := 1 TO nCols

      SQLDescrib( ::hStmt, i, @cName, 255, @nNameLen, @nType, ;
                  @nSize, @nDecs, @nNul )

      ::aFields[ cName ] := { i, cName, nType, nSize, nDecs, ( nNul != 0 ) }

   NEXT

   RETURN SQL_SUCCESS

// Closes the dataset
METHOD CLOSE() CLASS HDO_ODBC_RecordSet

   ::aFields := nil
   ::aValues := nil

   // Frees the statement
   ::ActiveCommand:Close()

RETURN NIL

// Returns the Field number from a Field name
METHOD FieldPos( cField ) CLASS HDO_ODBC_RecordSet

   LOCAL aField

   aField := ::aFields[ cField ]

RETURN aField[ 1 ]

METHOD Fields( cField ) CLASS HDO_ODBC_RecordSet

   LOCAL xValue
   LOCAL aField
   LOCAL nField

   aField := ::aFields[ cField ]

   nField := aField[ 1 ]

   xValue := ::aValues[ nField ]

   if xValue == NIL
      SQLGetData( ::hStmt, nField, SQL_CHAR, 255, @xValue )
      xValue := Buf2Var( aField[ 3 ], xValue )
      ::aValues[ nField ] := xValue
   endif

RETURN xValue

METHOD FieldAttr( cField, nAttr ) CLASS HDO_ODBC_RecordSet

   LOCAL aField

   aField := ::aFields[ cField ]

RETURN aFields[ nAttr ]

METHOD Read() CLASS HDO_ODBC_RecordSet

   ::aValues := array( ::nNumFields )

RETURN ( SQLFetch( ::hStmt ) <> SQL_NO_DATA_FOUND )

FUNCTION Buf2Var( nType, xBuffer )

   LOCAL xValue

   if xBuffer != nil

      SWITCH nType
         CASE SQL_CHAR
         CASE SQL_VARCHAR
         CASE SQL_NVARCHAR
            xValue := xBuffer
            exit
         CASE SQL_TIMESTAMP 
         CASE SQL_DATE
            xValue := stod( substr(xBuffer,1,4) + substr(xBuffer,6,2) + substr(xBuffer,9,2) )         
            exit
         CASE SQL_NUMERIC
         CASE SQL_DECIMAL
         CASE SQL_DOUBLE
         CASE SQL_INTEGER
         CASE SQL_FLOAT
         CASE SQL_REAL
            xValue := val( xBuffer )
            exit
         CASE SQL_BIT
         CASE SQL_SMALLINT
            xValue := iif( xBuffer == "1", .t., .f. )
       END

   endif

   RETURN xValue
