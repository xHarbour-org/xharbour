/*
 * $Id$
 *
 * xHarbour Project source code:
 * PostgreSQL RDBMS low level (client api) interface code.
 *
 * Copyright 2003 Rodrigo Moreno rodrigo_moreno@yahoo.com
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
 * See doc/license.txt for licensing terms.
 *
 */

#include "common.ch"
#include "hbclass.ch"

#define CONNECTION_OK                   0
#define CONNECTION_BAD                  1
#define CONNECTION_STARTED              2
#define CONNECTION_MADE                 3
#define CONNECTION_AWAITING_RESPONSE    4
#define CONNECTION_AUTH_OK              5
#define CONNECTION_SETENV               6
#define CONNECTION_SSL_STARTUP          7
#define CONNECTION_NEEDED               8

#define PGRES_EMPTY_QUERY               0
#define PGRES_COMMAND_OK                1
#define PGRES_TUPLES_OK                 2
#define PGRES_COPY_OUT                  3
#define PGRES_COPY_IN                   4
#define PGRES_BAD_RESPONSE              5
#define PGRES_NONFATAL_ERROR            6
#define PGRES_FATAL_ERROR               7

#define PQTRANS_IDLE                    0
#define PQTRANS_ACTIVE                  1
#define PQTRANS_INTRANS                 2
#define PQTRANS_INERROR                 3
#define PQTRANS_UNKNOWN                 4

CLASS TPQServer
    DATA     pDb
    DATA     lTrans
    DATA     lallCols  INIT .T.
    DATA     Schema    INIT 'public'
   
    METHOD   New( cHost, cDatabase, cUser, cPass, nPort, Schema )
    METHOD   Destroy()            INLINE PQClose(::pDb)
    METHOD   Close()              INLINE ::Destroy()

    METHOD   StartTransaction()
    METHOD   TransactionStatus()  INLINE PQtransactionstatus(::pDb)
    METHOD   Commit()
    METHOD   Rollback()

    METHOD   Query( cQuery )
    METHOD   Execute( cQuery )    INLINE ::Query(cQuery)
    METHOD   SetSchema( cSchema )

    METHOD   NetErr()             INLINE PQstatus(::pDb) != CONNECTION_OK
    METHOD   Error()              INLINE PQerrormessage(::pDb)
    
    METHOD   TableExists( cTable )
    METHOD   ListTables()
    METHOD   TableStruct( cTable )
    METHOD   CreateTable( cTable, aStruct )
    METHOD   DeleteTable( cTable  )

ENDCLASS


METHOD New( cHost, cDatabase, cUser, cPass, nPort, Schema ) CLASS TPQserver
    DEFAULT nPort TO 5432
    
    ::pDB := PQconnect(cDatabase, cHost, cUser, cPass, nPort)
    
    if ! Empty(Schema) 
        ::SetSchema(Schema)
    endif                

RETURN self

METHOD SetSchema( cSchema ) CLASS TPQserver
    Local res
    Local result := .F.
    
    if PQstatus(::pDb) == CONNECTION_OK
        ::Schema := cSchema
        res := PQexec( ::pDB, 'SET search_path TO ' + cSchema )        
        result := (PQresultStatus(res) == PGRES_COMMAND_OK)
        PQclear(res)
    endif        
RETURN result


METHOD StartTransaction() CLASS TPQserver
    Local pQuery, lError
    
    pQuery := PQexec( ::pDB, 'BEGIN' )        
    lError := PQresultstatus( pQuery ) != PGRES_COMMAND_OK
    PQclear(pQuery)
RETURN lError


METHOD Commit() CLASS TPQserver
    Local pQuery, lError
    
    pQuery := PQexec( ::pDB, 'COMMIT' )    
    lError := PQresultstatus( pQuery ) != PGRES_COMMAND_OK
    PQclear(pQuery)
RETURN lError

    
METHOD Rollback() CLASS TPQserver
    Local pQuery, lError
    
    pQuery := PQexec( ::pDB, 'ROLLBACK' )    
    lError := PQresultstatus( pQuery ) != PGRES_COMMAND_OK
    PQclear(pQuery)
RETURN lError


METHOD Query( cQuery ) CLASS TPQserver
    Local oQuery
    
    oQuery := TPQquery():New(::pDB, cQuery, ::lallCols, ::Schema)
RETURN oQuery

    
METHOD TableExists( cTable ) CLASS TPQserver
    Local result := .F.
    Local cQuery
    Local res
    
    cQuery := "select table_name "
    cQuery += "  from information_schema.tables "
    cQuery += " where table_type = 'BASE TABLE' and table_schema = " + DataToSql(::Schema) + " and table_name = " + DataToSql(lower(cTable))
    
    res := PQexec( ::pDB, cQuery )
    
    if PQresultstatus(res) == PGRES_TUPLES_OK
        result := (PQlastrec(res) != 0)
    end
    
    PQclear(res)
RETURN result    


METHOD ListTables() CLASS TPQserver
    Local result := {}
    Local cQuery
    Local res
    Local i
    
    cQuery := "select table_name "
    cQuery += "  from information_schema.tables "
    cQuery += " where table_schema = " + DataToSql(::Schema) + " and table_type = 'BASE TABLE' "
    
    res := PQexec( ::pDB, cQuery )
    
    if PQresultstatus(res) == PGRES_TUPLES_OK
        For i := 1 to PQlastrec(res)
            aadd( result, PQgetvalue( res, i, 1 ) )
        Next            
    end
    
    PQclear(res)
RETURN result  

METHOD TableStruct( cTable ) CLASS TPQserver
    Local result := {}
    Local cQuery
    Local res
    Local i
    Local cField
    Local cType
    Local nSize
    Local nDec
    
    cQuery := "SELECT column_name, data_type, character_maximum_length, numeric_precision, numeric_scale "
    cQuery += "  FROM information_schema.columns "
    cQuery += " WHERE table_schema = " + DataToSql(::Schema) + " and table_name = " + DataToSql(lower(cTable))
    cQuery += "ORDER BY ordinal_position "                                                             
    
    res := PQexec( ::pDB, cQuery )
    
    if PQresultstatus(res) == PGRES_TUPLES_OK
        For i := 1 to PQlastrec(res)
            cField    := PQgetvalue(res, i, 1)
            cType     := PQgetvalue(res, i, 2)            
            nSize     := PQgetvalue(res, i, 4)
            nDec      := PQgetvalue(res, i, 5)
                            
            if 'char' $ cType
                cType := 'C'
                nSize := Val(PQgetvalue(res, i, 3))
                nDec  := 0

            elseif 'text' $ cType                 
                cType := 'M'
                nSize := 10
                nDec := 0

            elseif 'boolean' $ cType
                cType := 'L'
                nSize := 1
                nDec  := 0

            elseif 'smallint' $ cType 
                cType := 'N'
                nSize := 5
                nDec  := 0
                
            elseif 'integer' $ cType .or. 'serial' $ cType 
                cType := 'N'
                nSize := 9
                nDec  := 0
                
            elseif 'bigint' $ cType .or. 'bigserial' $ cType
                cType := 'N'
                nSize := 19
                nDec  := 0
                
            elseif 'decimal' $ cType .or. 'numeric' $ cType
                cType := 'N'
                nSize := val(nSize)
                nDec  := val(nDec)

            elseif 'real' $ cType 
                cType := 'N'
                nSize := 15
                nDec  :=  4
                
            elseif 'double precision' $ cType
                cType := 'N'
                nSize := 15
                nDec  := 8
                
            elseif 'money' $ cType               
                cType := 'N'
                nSize := 9
                nDec  := 2
                
            elseif 'timestamp' $ cType               
                cType := 'C'
                nSize := 20
                nDec  := 0

            elseif 'date' $ cType               
                cType := 'D'
                nSize := 8
                nDec  := 0

            elseif 'time' $ cType               
                cType := 'C'
                nSize := 10
                nDec  := 0

            else
                // Unsuported
                cType := 'U'
                nSize := 0
                nDec  := -1

            end               

            if cType <> 'U'
                aadd( result, { cField, cType, nSize, nDec } )
            end                

        Next
    end
    
    PQclear(res)
RETURN result  

METHOD CreateTable( cTable, aStruct ) CLASS TPQserver
    Local result := .t.
    Local cQuery
    Local res
    Local i
    
    cQuery := 'CREATE TABLE ' + ::Schema + '.' + cTable + '( '
    
    For i := 1 to Len(aStruct)
    
        cQuery += aStruct[i, 1]
        
        if aStruct[ i, 2 ] == "C"
            cQuery += ' Char(' + ltrim(str(aStruct[i, 3])) + ')'
                
        elseif aStruct[ i, 2 ] == "D"
            cQuery += ' Date '                                                        
            
        elseif aStruct[ i, 2 ] == "N"
            cQuery += ' Numeric(' + ltrim(str(aStruct[i, 3])) + ',' + ltrim(str(aStruct[i,4])) + ')'

        elseif aStruct[ i, 2 ] == "L"
            cQuery += ' boolean '
            
        elseif aStruct[ i, 2 ] == "M"
            cQuery += ' text '
        end
        
        if i == Len(aStruct)
            cQuery += ')'
        else
            cQuery += ','
        end    
    Next

    res := PQexec( ::pDB, cQuery )
        
    if PQresultstatus(res) != PGRES_COMMAND_OK
        result := .f.
    end
    
    PQclear(res)
RETURN result


METHOD DeleteTable( cTable  ) CLASS TPQserver
    Local result := .t.
    Local res

    res := PQexec( ::pDB, 'DROP TABLE ' + ::Schema + '.' + cTable  )
    
    if PQresultstatus(res) != PGRES_COMMAND_OK
        result := .f.
    end
    
    PQclear(res)
RETURN result



CLASS TPQQuery
    DATA     pQuery
    DATA     pDB

    DATA     lBof
    DATA     lEof
    DATA     lClosed
    DATA     lallCols INIT .T.

    DATA     cQuery
    DATA     nRecno
    DATA     nFields
    DATA     nLastrec

    DATA     aStruct
    DATA     aKeys              
    DATA     TableName
    DATA     Schema

    METHOD   New( pDB, cQuery, lallCols, cSchema )
    METHOD   Destroy()          
    METHOD   Close()            INLINE ::Destroy()

    METHOD   Refresh()                      
    METHOD   Fetch()            INLINE ::Skip()
    METHOD   Skip( nRecno )             

    METHOD   Bof()              INLINE ::lBof
    METHOD   Eof()              INLINE ::lEof
    METHOD   RecNo()            INLINE ::nRecno
    METHOD   Lastrec()          INLINE ::nLastrec
    METHOD   Goto(nRecno)       

    METHOD   NetErr()           INLINE PQresultStatus(::pQuery) != PGRES_COMMAND_OK .and. PQresultStatus(::pQuery) != PGRES_TUPLES_OK 
    METHOD   Error()            INLINE PQresultErrormessage(::pQuery)       

    METHOD   FCount()           INLINE ::nFields
    METHOD   FieldName( nField )
    METHOD   FieldPos( cField )
    METHOD   FieldLen( nField )
    METHOD   FieldDec( nField )
    METHOD   FieldType( nField )
    METHOD   Update( oRow )
    METHOD   Delete( oRow )
    METHOD   Append( oRow )
    METHOD   SetKey()

    METHOD   Changed(nField)    INLINE ::aRow[nField] != ::aOld[nField]
    METHOD   Blank()            INLINE ::GetBlankRow()

    METHOD   Struct()
    
    METHOD   FieldGet( nRow, nField )
    METHOD   GetRow( nRow )   
    METHOD   GetBlankRow()  
ENDCLASS


METHOD New( pDB, cQuery, lallCols, cSchema ) CLASS TPQquery
    ::pDB      := pDB
    ::cQuery   := RemoveSpaces(cQuery)
    ::lClosed  := .F.    
    ::lallCols := lallCols
    ::Schema   := cSchema
    
    ::Refresh()        
RETURN self


METHOD Destroy() CLASS TPQquery
    PQclear( ::pQuery )    
    ::lClosed := .t.
RETURN .t.


METHOD Refresh() CLASS TPQquery
    Local res
    Local result
    Local cTableCodes := ''
    Local cFieldCodes := ''
    Local aStruct := {}
    Local aTemp := {}
    Local i
    Local n
    Local temp
    Local cQuery
    Local cType, nType, nDec, nSize

    if ! ::lClosed
        ::Destroy()    
        ::lClosed := .F.
    end

    ::lBof := .F.
    ::lEof := .F.
    
    ::nRecno := 0
    ::nFields := 0
    ::nLastrec := 0
    
    ::aStruct := {}

    res := PQexec( ::pDB, ::cQuery )

    if PQresultstatus(res) == PGRES_TUPLES_OK     
        // Get some information about metadata
        aTemp := PQmetadata(res)

        if ISARRAY(aTemp)                        
            For i := 1 to Len(aTemp)
                cType := aTemp[ i, 2 ]
                nSize := aTemp[ i, 3 ]
                nDec  := aTemp[ i, 4 ]
        
                if 'char' $ cType 
                    cType := 'C'
        
                elseif 'text' $ cType                 
                    cType := 'M'
        
                elseif 'boolean' $ cType
                    cType := 'L'
                    nSize := 1
        
                elseif 'smallint' $ cType 
                    cType := 'N'
                    nSize := 5
                
                elseif 'integer' $ cType .or. 'serial' $ cType 
                    cType := 'N'
                    nSize := 9
                
                elseif 'bigint' $ cType .or. 'bigserial' $ cType
                    cType := 'N'
                    nSize := 19
                
                elseif 'decimal' $ cType .or. 'numeric' $ cType
                    cType := 'N'
        
                elseif 'real' $ cType 
                    cType := 'N'
                    nSize := 15
                    nDec  :=  4
                
                elseif 'double precision' $ cType
                    cType := 'N'
                    nSize := 15
                    nDec  := 8
                
                elseif 'money' $ cType               
                    cType := 'N'
                    nSize := 9
                    nDec  := 2
                
                elseif 'timestamp' $ cType               
                    cType := 'C'
                    nSize := 20
        
                elseif 'date' $ cType               
                    cType := 'D'
                    nSize := 8
        
                elseif 'time' $ cType               
                    cType := 'C'
                    nSize := 10
        
                else
                    // Unsuported
                    cType := 'K'
                endif               
                
                aadd( aStruct, {aTemp[ i, 1 ], cType, nSize, nDec, aTemp[i, 5], aTemp[i, 6]} )
            Next    
            
            ::nFields := PQfcount(res)
            ::nLastrec := PQlastrec(res)
            
            if ::nLastrec <> 0
                ::nRecno := 1
            endif                
                
            ::aStruct := aStruct        
        endif
    
        result := .T.            
    
    elseif PQresultstatus(res) == PGRES_COMMAND_OK
        result := .T.
        
    else
        result := .F.            
    endif            
    
   ::pQuery := res

RETURN result    
    
    
METHOD Struct() CLASS TPQquery
    Local result := {}
    Local i
    
    For i := 1 to Len(::aStruct)
        aadd( result, { ::aStruct[i, 1], ::aStruct[i, 2], ::aStruct[i, 3], ::aStruct[i, 4] })    
    Next    
RETURN result


METHOD Skip( nrecno ) CLASS TPQquery
    DEFAULT nRecno TO 1
    
    if ::nRecno + nRecno > 0 .and. ::nRecno + nRecno <= ::nLastrec
        ::nRecno := ::nRecno + nRecno
        ::lEof := .F.
        ::lBof := .F.
    
    else            
        if ::nRecno + nRecno > ::nLastRec 
            ::nRecno := ::nLastRec + 1
            ::lEof := .T.        
        end
    
        if ::nRecno + nRecno < 0
            ::nRecno := 1
            ::lBof := .T.
        end
    end        
RETURN .T.


METHOD Goto( nRecno ) CLASS TPQquery    
    if nRecno > 0 .and. nRecno <= ::nLastrec
        ::nRecno := nRecno
    end
RETURN .T.    
    
    
METHOD FieldPos( cField ) CLASS TPQquery
    Local result  := 0
    
    if ! ::NetErr()
        result := AScan( ::aStruct, {|x| x[1] == trim(Lower(cField)) })
    end        

RETURN result
    

METHOD FieldName( nField ) CLASS TPQquery
    Local result

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    endif
        
    if ! ::NetErr() .and. nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 1]    
    endif
    
RETURN result


METHOD FieldType( nField ) CLASS TPQquery
    Local result
    
    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    endif

    if ! ::NetErr() .and. nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 2]    
    end
    
RETURN result


METHOD FieldLen( nField ) CLASS TPQquery
    Local result
    
    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    endif

    if ! ::NetErr() .and. nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 3]    
    end
RETURN result


METHOD FieldDec( nField ) CLASS TPQquery
    Local result

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    endif
    
    if ! ::NetErr() .and. nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 4]    
    end
RETURN result


METHOD Delete(oRow) CLASS TPQquery
    Local result 
    Local res
    Local i
    Local nField
    Local xField
    Local cWhere := ''
    Local aParams := {}
    
    ::SetKey()
    
    if ! Empty(::Tablename) .and. ! Empty(::aKeys)
        For i := 1 to len(::aKeys)
            nField := oRow:Fieldpos(::aKeys[i])
            xField := oRow:FieldGetOld(nField)

            cWhere += ::aKeys[i] + ' = $' + ltrim(str(i))
            
            AADD( aParams, ValueToString(xField) )

            if i <> len(::aKeys)
                cWhere += ' and '
            end                    
        Next                        

        if ! (cWhere == '')
            res := PQexecParams( ::pDB, 'DELETE FROM ' + ::Schema + '.' + ::Tablename + ' WHERE ' + cWhere, aParams)    
            if PQresultstatus(res) != PGRES_COMMAND_OK            
                result := PQresultErrorMessage(res)
            endif                
            PQclear(res)
        end            
    end
RETURN result


METHOD Append( oRow ) CLASS TPQquery
    Local result
    Local cQuery
    Local i
    Local res
    Local lChanged := .f.
    Local aParams := {}
    Local nParams := 0

    ::SetKey()
        
    if ! Empty(::Tablename)
        cQuery := 'INSERT INTO ' + ::Schema + '.' + ::Tablename + '('
        For i := 1 to oRow:FCount()
            if ::lallCols .or. oRow:changed(i)
                lChanged := .t.
                cQuery += oRow:Fieldname(i) + ','
            end                
        Next

        cQuery := Left( cQuery, len(cQuery) - 1 ) +  ') VALUES (' 

        For i := 1 to oRow:FCount()
            if ::lallCols .or. oRow:Changed(i)
                nParams++
                cQuery += '$' + ltrim(str(nParams)) + ','
                aadd( aParams, ValueToString(oRow:FieldGet(i)) )
            end                
        Next
        
        cQuery := Left( cQuery, len(cQuery) - 1  ) + ')'

        if lChanged
            res := PQexecParams( ::pDB, cQuery, aParams)    
            if PQresultstatus(res) != PGRES_COMMAND_OK            
                result := PQresultErrorMessage(res)
            endif                

            PQclear(res)
        end            
    end            
RETURN result


METHOD Update(oRow) CLASS TPQquery
    Local result := .F.
    Local cQuery
    Local i
    Local nField
    Local xField
    Local cWhere
    Local res
    Local lChanged := .f.
    Local aParams := {}
    Local nParams := 0

    ::SetKey()

    if ! Empty(::Tablename) .and. ! Empty(::aKeys)
        cWhere := ''
        For i := 1 to len(::aKeys)
    
            nField := oRow:Fieldpos(::aKeys[i])            
            xField := oRow:FieldGetOld(nField)
            
            cWhere += ::aKeys[i] + '=' + DataToSql(xField)
                
            if i <> len(::aKeys)
                cWhere += ', '
            end
        Next                        
                
        cQuery := 'UPDATE ' + ::Schema + '.' + ::Tablename + ' SET '
        For i := 1 to oRow:FCount()
            if ::lallcols .or. oRow:Changed(i)
                lChanged := .t.
                nParams++
                cQuery += oRow:Fieldname(i) + ' = $' + ltrim(str(nParams)) + ','
                aadd( aParams, ValueToString(oRow:FieldGet(i)) )
            end                
        Next
        
        if ! (cWhere == '') .and. lChanged

            cQuery := Left( cQuery, len(cQuery) - 1 ) + ' WHERE ' + cWhere                        
            res := PQexecParams( ::pDB, cQuery, aParams)    
            if PQresultstatus(res) != PGRES_COMMAND_OK            
                result := PQresultErrorMessage(res)
            endif                
        end            
    end            
RETURN result


METHOD FieldGet( nRow, nField ) CLASS TPQquery
    Local result
    Local i
    Local cType
    Local nSize
    Local tmp

    if ! ::NetErr().and. nField >= 1 .and. nField <= ::nFields .and. ! ::lclosed
        
        if ISCHARACTER(nField)
            nField := ::Fieldpos(nField)
        endif
                    
        result := PQgetvalue( ::pQuery, nRow, nField)
        cType := ::aStruct[ nField, 2 ] 
        nSize := ::aStruct[ nField, 3 ] 
                                    
        if cType == "N"
            if ! ISNIL(result)
                result := val(result)
            else
                result := 0
            end
        
        elseif cType == "D"
            if ! ISNIL(result)
                tmp := SET (_SET_DATEFORMAT)   
                tmp := strtran( tmp, 'dd', substr(result, 9, 2) )
                tmp := strtran( tmp, 'mm', substr(result, 6, 2) )
                tmp := strtran( tmp, 'yyyy', left(result, 4) )
                result := CtoD(tmp)
            else
                result := CtoD('')
            end
            
        elseif cType == "L"
            if ! ISNIL(result)
                result := (result == 't')
            else
                result := .F.
            end
            
        elseif cType == "C"
            if ISNIL(result)
                result := Space(nSize)
            else
                result := PadR(result, nSize)                
            end
                        
        elseif cType == "M"
            if ISNIL(result)
                result := ""
            else
                result := result
            end
                        
        end
    end                    
RETURN result


METHOD Getrow( nRow ) CLASS TPQquery
    Local result, aRow := {}, aOld := {}, nCol
    
    DEFAULT nRow TO ::nRecno
    
    if ! ::NetErr().and. ! ::lclosed 
        
        if nRow > 0 .and. nRow <= ::nLastRec

            ASize(aRow, ::nFields)
            ASize(aOld, ::nFields)

            For nCol := 1 to ::nFields
                aRow[nCol] := ::Fieldget(nRow, nCol)    
                aOld[nCol] := ::Fieldget(nRow, nCol)                    
            Next

            result := TPQRow():New( aRow, aOld, ::aStruct )
            
        elseif nRow > ::nLastrec        
            result := ::GetBlankRow()
        end            
    end            
RETURN result


METHOD GetBlankRow() CLASS TPQquery
    Local result, aRow := {}, aOld := {}, i
    
    ASize(aRow, ::nFields)
    ASize(aOld, ::nFields)
    
    For i := 1 to ::nFields
        if ::aStruct[i, 2] == 'C'
            aRow[i] := ''
            aOld[i] := ''
        elseif ::aStruct[i, 2] == 'N'
            aRow[i] := 0
            aOld[i] := 0
        elseif ::aStruct[i, 2] == 'L'
            aRow[i] := .F.
            aOld[i] := .F.
        elseif ::aStruct[i, 2] == 'D'
            aRow[i] := CtoD('')
            aOld[i] := CtoD('')
        elseif ::aStruct[i, 2] == 'M'
            aRow[i] := ''
            aOld[i] := ''
        end                                
    Next
    
    result := TPQRow():New( aRow, aOld, ::aStruct )
RETURN result


METHOD SetKey() CLASS TPQquery
    Local cQuery
    Local i, x
    Local nTableId, xTableId := -1
    Local nCount := 0
    Local cTable
    Local aKeys := {}
    Local cField := ''
    Local res
    Local nPos

    if PQresultstatus(::pQuery) == PGRES_TUPLES_OK         
        if ISNIL(::Tablename)
            /* set the table name looking for table oid */
            for i := 1 to len(::aStruct)
                /* Store table codes oid */
                nTableId := ::aStruct[i, 5]
                
                if nTableId != xTableId
                    xTableId := nTableId
                    nCount++
                endif                
            next
            
            if nCount == 1            
                /* first, try get the table name from select, else get from pg_catalog */
                if (npos := at('FROM ', Upper(::cQuery))) != 0
                    cQuery := lower(ltrim(substr( ::cQuery, nPos + 5 )))
                    
                    if (npos := at('.', cQuery)) != 0
                        ::Schema := alltrim(left(cQuery,npos-1))
                        cQuery := substr(cQuery, nPos + 1)
                    endif
                                            
                    if (npos := at(' ', cQuery)) != 0
                        ::Tablename := trim(Left(cQuery, npos))
                    else
                        ::Tablename := cQuery                                            
                    endif                
                endif
                
                if empty(::Tablename)
                    cQuery := 'select typname from pg_type where typrelid in (' + str(xTableId) + ')'
                    cQuery += ' union all '
                    cQuery += 'select relname from pg_class where reltype in (' + str(xTableId) + ')'

                    res := PQexec(::pDB, cQuery)
            
                    if PQresultstatus(res) == PGRES_TUPLES_OK .and. PQlastrec(res) != 0
                        ::Tablename := trim(PQgetvalue(res, 1, 1))
                    endif        
                
                    PQclear(res)
                endif                    
            endif            
        endif
        
        if ISNIL(::aKeys) .and. ! empty(::Tablename)
            /* Set the table primary keys */
            for i := 1 to len(::aStruct)
                /* store the ordinal columns numbers */
                cField += str(::aStruct[ i, 6 ])                    
                
                if i <> len(::aStruct)
                    cField += ','
                endif
            next
                                        
            cQuery := "select column_name "
            cQuery += "  from information_schema.key_column_usage "
            cQuery += " where table_schema = " + DataToSql(::Schema)
            cQuery += "   and ordinal_position in (" + cField + ") and table_name = '" + ::Tablename + "'"

            res := PQexec(::pDB, cQuery)
            
            if PQresultstatus(res) == PGRES_TUPLES_OK .and. PQlastrec(res) != 0
                For x := 1 To PQlastrec(res)
                    aadd( aKeys, PQgetvalue( res, x, 1 ) )
                Next                          
            endif   
            
            PQclear(res)
        endif
        ::aKeys := aKeys            

    endif    
    
RETURN nil

CLASS TPQRow
   DATA     aRow
   DATA     aOld
   DATA     aStruct
   
   METHOD   New( row, old, struct )

   METHOD   FCount()           INLINE Len(::aRow)
   METHOD   FieldGet( nField )
   METHOD   FieldPut( nField, Value )  
   METHOD   FieldName( nField )
   METHOD   FieldPos( cFieldName )
   METHOD   FieldLen( nField )             
   METHOD   FieldDec( nField )             
   METHOD   FieldType( nField ) 
   METHOD   Changed( nField )     INLINE ! (::aRow[nField] == ::aOld[nField])              
   METHOD   FieldGetOld( nField ) INLINE ::aOld[nField]
ENDCLASS


METHOD new( row, old, struct) CLASS TPQrow
    ::aRow := row
    ::aOld := old
    ::aStruct := struct            
RETURN self


METHOD FieldGet( nField ) CLASS TPQrow
    Local result

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    endif
    
    if nField >= 1 .and. nField <= len(::aRow)
        result := ::aRow[nField]    
    end
    
RETURN result


METHOD FieldPut( nField, Value ) CLASS TPQrow
    Local result
    
    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    endif

    if nField >= 1 .and. nField <= len(::aRow)
        result := ::aRow[nField] := Value
    end
RETURN result


METHOD FieldName( nField ) CLASS TPQrow
    Local result

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    endif
    
    if nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 1]    
    end
    
RETURN result


METHOD FieldPos( cField ) CLASS TPQrow
    Local result  := 0
    
    result := AScan( ::aStruct, {|x| x[1] == trim(lower(cField)) })

RETURN result
    

METHOD FieldType( nField ) CLASS TPQrow
    Local result

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    endif
    
    if nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 2]    
    end
    
RETURN result


METHOD FieldLen( nField ) CLASS TPQrow
    Local result

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    endif
    
    if nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 3]    
    end
RETURN result


METHOD FieldDec( nField ) CLASS TPQrow
    Local result

    if ISCHARACTER(nField)
        nField := ::Fieldpos(nField)
    endif
    
    if nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 4]    
    end
RETURN result


Static Function RemoveSpaces( cQuery )
    Do While AT("  ", cQuery) != 0
        cQuery := Strtran(cQuery, "  ", " ")
    end
Return cQuery


Static Function DataToSql(xField)
        Local cType, result := 'NULL'

        cType := ValType(xField)
        
        if cType == "C" .or. cType == "M"
                result := "'"+ strtran(xField, "'", ' ') + "'"
        elseif cType == "D" .and. ! Empty(xField)
                result := "'" + StrZero(month(xField),2) + '/' + StrZero(day(xField),2) + '/' + StrZero(Year(xField),4) + "'"
        elseif cType == "N"
                result := str(xField)
        elseif cType == "L"
                result := iif( xField, "'t'", "'f'" )
        end        
return result           

Static Function ValueToString(xField)
        Local cType, result := 'NULL'

        cType := ValType(xField)
        
        if cType == "D" .and. ! Empty(xField)
                result := StrZero(month(xField),2) + '/' + StrZero(day(xField),2) + '/' + StrZero(Year(xField),4)
        elseif cType == "N"
                result := str(xField)
        elseif cType == "L"
                result := iif( xField, "t", "f" )
        elseif cType == "C" .or. cType == "M"
                result := xField                                
        end        
return result           


                                             
