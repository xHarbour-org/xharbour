/*
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


CLASS TPQServer
    DATA     pDb
    DATA     lTrans
    DATA     lError
    DATA     cError
   
    METHOD   New( cHost, cDatabase, cUser, cPass, nPort )
    METHOD   Destroy()  INLINE PQClose(::pDb)
    METHOD   Close()    INLINE PQClose(::pDb)

    METHOD   StartTransaction()
    METHOD   Commit()
    METHOD   Rollback()

    METHOD   Query( cQuery )
    METHOD   Execute( cQuery ) INLINE ::Query(cQuery)

    METHOD   NetErr()   INLINE ::lError 
    METHOD   Error()    INLINE ::cError
    
    METHOD   TableExists( cTable )
    METHOD   ListTables()
    METHOD   TableStruct( cTable )
    METHOD   CreateTable( cTable, aStruct )
    METHOD   DeleteTable( cTable  )

ENDCLASS


METHOD New( cHost, cDatabase, cUser, cPass, nPort ) CLASS TPQserver
    DEFAULT nPort TO 5432
    
    ::pDB := PQconnect(cDatabase, cHost, cUser, cPass, nPort)

    ::lError := (ISCHAR(::pDB))
    
    if ::lError
        ::cError := ::pDB
    end

RETURN self


METHOD StartTransaction() CLASS TPQserver
    Local pQuery
    
    pQuery := PQexec( ::pDB, 'BEGIN' )
    
    ::lError := (ISCHAR(pQuery))
    
    if ::lError
        ::cError := pQuery
    end
    
    PQclear(pQuery)
RETURN nil


METHOD Commit() CLASS TPQserver
    Local pQuery
    
    pQuery := PQexec( ::pDB, 'COMMIT' )
    
    ::lError := (ISCHAR(pQuery))
    
    if ::lError
        ::cError := pQuery
    end
    
    PQclear(pQuery)
RETURN nil

    
METHOD Rollback() CLASS TPQserver
    Local pQuery
    
    pQuery := PQexec( ::pDB, 'ROLLBACK' )
    
    ::lError := (ISCHAR(pQuery))
    
    if ::lError
        ::cError := pQuery
    end
    
    PQclear(pQuery)
RETURN nil


METHOD Query( cQuery ) CLASS TPQserver
    Local oQuery
    
    oQuery := TPQquery():New(::pDB, cQuery)
RETURN oQuery

    
METHOD TableExists( cTable ) CLASS TPQserver
    Local result := .F.
    Local cQuery
    Local res
    
    cQuery := "select table_name "
    cQuery += "  from information_schema.tables "
    cQuery += " where table_type = 'BASE TABLE' and table_name = " + DataToSql(lower(cTable))
    
    res := PQexec( ::pDB, cQuery )
    
    if ! ISCHAR(res)
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
    cQuery += " where table_schema = 'public' and table_type = 'BASE TABLE' "
    
    res := PQexec( ::pDB, cQuery )
    
    if ! ISCHAR(res)
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
    cQuery += " WHERE table_schema = 'public' and table_name = " + DataToSql(lower(cTable))
    cQuery += "ORDER BY ordinal_position "                                                             
    
    res := PQexec( ::pDB, cQuery )
    
    if ! ISCHAR(res)
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
                cType := 'K'
                nSize := 0
                nDec  := -1

            end               

            if cType <> 'K'
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
    
    cQuery := 'CREATE TABLE ' + cTable + '( '
    
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
    
    if (::lError := (ISCHAR(res)))
        ::cError := res
        result := .f.
    end
    
    PQclear(res)
RETURN result


METHOD DeleteTable( cTable  ) CLASS TPQserver
    Local result := .t.
    Local res

    res := PQexec( ::pDB, 'DROP TABLE ' + cTable  )
    
    if (::lError := (ISCHAR(res)))
        ::cError := res
        result := .f.
    end
    
    PQclear(res)
RETURN result



CLASS TPQQuery
    DATA     pQuery
    DATA     pDB

    DATA     cError
    DATA     lError
    DATA     lBof
    DATA     lEof
    DATA     lClosed

    DATA     cQuery
    DATA     nRecno
    DATA     nFields
    DATA     nLastrec

    DATA     aStruct
    DATA     aKeys
    DATA     aTables

    METHOD   New( pDB, cQuery )
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

    METHOD   NetErr()           INLINE ::lError
    METHOD   Error()            INLINE ::cError

    METHOD   FCount()           INLINE ::nFields
    METHOD   FieldName( nField )
    METHOD   FieldPos( cField )
    METHOD   FieldLen( nField )
    METHOD   FieldDec( nField )
    METHOD   FieldType( nField )
    METHOD   Update( oRow )
    METHOD   Delete( oRow )
    METHOD   Append( oRow )

    METHOD   Changed(nField)    INLINE ::aRow[nField] != ::aOld[nField]
    METHOD   Blank()            INLINE ::GetBlankRow()

    METHOD   Struct()
    
    METHOD   FieldGet( nRow, nField )
    METHOD   GetRow( nRow )   
    METHOD   GetBlankRow()  
    METHOD   GetKeyField()      INLINE ::aKeys

ENDCLASS


METHOD New( pDB, cQuery ) CLASS TPQquery
    ::pDB := pDB
    ::cQuery := RemoveSpaces(cQuery)
    ::aKeys := NIL
    ::lClosed := .F.    
    
    ::Refresh()        
RETURN self


METHOD Destroy() CLASS TPQquery
    PQclear( ::pQuery )    
    ::lClosed := .t.
RETURN .t.


METHOD Refresh() CLASS TPQquery
    Local res
    Local result
    Local aTable := {}
    Local aKeys := {}
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
    ::lError := .F.
    ::cError := ''
    
    ::nRecno := 0
    ::nFields := 0
    ::nLastrec := 0
    
    ::aKeys := {}
    ::aStruct := {}

    res := PQexec( ::pDB, ::cQuery )
        
    if ! ISCHAR(res)
    
        // Get some information about metadata
        aTemp := PQmetadata(res)

        IF ISARRAY(aTemp)        
                For i := 1 to Len(aTemp)
                    cTableCodes += str(aTemp[ i, 2 ])
                    cFieldCodes += str(aTemp[ i, 3 ])
                    
                    if i <> len(aTemp)
                        cTableCodes += ','
                        cFieldCodes += ','
                    end
                                
                    aadd( aStruct, { aTemp[i, 1], ; // Field Name used on query
                                     nil, nil, nil, nil, nil, ;
                                     aTemp[i, 2] , ; // Table code see pg_class
                                     aTemp[i, 3] }) // Ordinal Number
                Next
        
                cQuery := "SELECT a.ordinal_position, a.column_name, b.constraint_name, a.data_type, c.relfilenode, "
                cQuery += "       a.character_maximum_length, a.numeric_precision, a.numeric_scale, a.table_name "
                cQuery += "  FROM pg_class c, information_schema.columns a "
                cQuery += "  LEFT OUTER JOIN information_schema.key_column_usage b "
                cQuery += "    ON (a.ordinal_position = b.ordinal_position and b.table_schema = 'public') and a.table_name = b.table_name "
                cQuery += " WHERE a.table_schema = 'public' and a.table_name = c.relname and "
                cQuery += "       c.relfilenode in (" + cTableCodes + ") and a.ordinal_position in (" + cFieldCodes + ")"
        
                cQuery := RemoveSpaces(cQuery)
                
                temp := PQexec(::pDB, cQuery)
                
                if ! ISCHAR(temp)
                    For i := 1 to PQlastrec(temp)                
                        n := AScan( aStruct, {|x| x[8] == val(PQgetvalue( temp, i, 1 )) .and.;
                                                  x[7] == val(PQgetvalue( temp, i, 5 )) })                
                    
                        if n != 0
                            aStruct[n, 2] := PQgetvalue( temp, i, 4 ) // data type
                            
                            if ! ISNIL(PQgetvalue( temp, i, 6 ))
                                aStruct[n, 3] := PQgetvalue( temp, i, 6 ) // Size
                            else                        
                                aStruct[n, 3] := PQgetvalue( temp, i, 7 ) // Size
                            end
                            
                            aStruct[ n, 4 ] := PQgetvalue( temp, i, 8 ) // Dec
                            aStruct[ n, 5 ] := PQgetvalue( temp, i, 9 ) // Original table name
                            aStruct[ n, 6 ] := PQgetvalue( temp, i, 2 ) // Original field name
        
                        end
                        
                        // Tables in query
                        if (ASCAN(aTable, PQgetvalue( temp, i, 9 )) == 0)
                            aadd( aTable, PQgetvalue( temp, i, 9 ))
                        end                        
                        
                        // Primary Keys in Query
                        if ! ISNIL(PQgetvalue( temp, i, 3 ))                            
                            if (ASCAN(aKeys, PQgetvalue( temp, i, 2 )) == 0)
                                aadd( aKeys, PQgetvalue( temp, i, 2 ))
                            end                        
                        end                            
        
                    Next
        
                    // Fix the sizes and data types
                    For i := 1 to Len(aStruct)
        
                        cType := aStruct[ i, 2 ]
                        nSize := aStruct[ i, 3 ]
                        nDec  := aStruct[ i, 4 ]
        
                        if 'char' $ cType 
                            cType := 'C'
                            nSize := val(nSize)
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
                            cType := 'K'
                            nSize := 0
                            nDec  := -1
        
                        end               
        
                        aStruct[ i, 2 ] := cType
                        aStruct[ i, 3 ] := nSize
                        aStruct[ i, 4 ] := nDec
                    Next        
                end    
        
                PQclear(temp)
            
                ::nFields := PQfcount(res)
                ::nLastrec := PQlastrec(res)
                
                if ::nLastrec <> 0
                    ::nRecno := 1
                end                    
                
                ::aStruct := aStruct
        
                ::aTables := aTable
                ::aKeys := aKeys            
        end
    
        result := .T.            
    
    else
        ::lError := .T.
        ::cError := res
        result := .F.            
        
        PQclear(res)
        
    end            
    
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
    
    if ! ::lError
        result := AScan( ::aStruct, {|x| x[1] == trim(Lower(cField)) })
    end        

RETURN result
    

METHOD FieldName( nField ) CLASS TPQquery
    Local result
        
    if ! ::lError .and. nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 1]    
    end
    
RETURN result


METHOD FieldType( nField ) CLASS TPQquery
    Local result
    
    if ! ::lError .and. nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 2]    
    end
    
RETURN result


METHOD FieldLen( nField ) CLASS TPQquery
    Local result
    
    if ! ::lError .and. nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 3]    
    end
RETURN result


METHOD FieldDec( nField ) CLASS TPQquery
    Local result
    
    if ! ::lError .and. nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 4]    
    end
RETURN result


METHOD Delete(oRow) CLASS TPQquery
    Local result := .F.
    Local res
    Local i
    Local nField
    Local xField
    Local cWhere
    
    if len(::aTables) == 1
        // Cannot delete joined tables 

        cWhere := ''
        
        For i := 1 to len(::aKeys)
            nField := oRow:Fieldpos(::aKeys[i])
            xField := oRow:FieldGetOld(nField)

            cWhere += ::aKeys[i] + '=' + DataToSql(xField)                    

            if i <> len(::aKeys)
                cWhere += ','
            end                    
        Next                        

        if ! (cWhere == '')
            res := PQexec( ::pDB, 'DELETE FROM ' + ::aTables[1] + ' WHERE ' + cWhere)            
            result := ! ISCHAR(res)  
            
            if ! result     
                ::lError := .t.
                ::cError := res
            else
                ::lError := .f.
                ::cError := ''
            end          
            
            PQclear(res)
        end            
    end
RETURN result


METHOD Append( oRow ) CLASS TPQquery
    Local result := .F.
    Local cQuery
    Local i
    Local res
    Local lChanged := .f.
        
    if len(::aTables) == 1
        // Can insert only one table, not in joined tables 
                
        cQuery := 'INSERT INTO ' + ::aTables[1] + '('
        For i := 1 to oRow:FCount()
//            if oRow:changed(i)
                lChanged := .t.
                cQuery += oRow:Fieldname(i) + ','
//            end                
        Next

        cQuery := Left( cQuery, len(cQuery) - 1 ) +  ') VALUES (' 

        For i := 1 to oRow:FCount()
//            if oRow:Changed(i)
                cQuery += DataToSql(oRow:FieldGet(i)) + ','
//            end                
        Next
        
        cQuery := Left( cQuery, len(cQuery) - 1  ) + ')'

        if lChanged
            res := PQexec( ::pDB, cQuery)            
            result := ! ISCHAR(res)   

            if ! result     
                ::lError := .t.
                ::cError := res
            else
                ::lError := .f.
                ::cError := ''
            end
                
            PQclear(res)
        else            
            ::lError := .t.
            ::cError := 'No changed fields found.'
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
        
    if len(::aTables) == 1
         // Can't insert joined tables

        cWhere := ''
        For i := 1 to len(::aKeys)
    
            nField := oRow:Fieldpos(::aKeys[i])            
            xField := oRow:FieldGetOld(nField)
            
            cWhere += ::aKeys[i] + '=' + DataToSql(xField)
                
            if i <> len(::aKeys)
                cWhere += ', '
            end
        Next                        
                
        cQuery := 'UPDATE ' + ::aTables[1] + ' SET '
        For i := 1 to oRow:FCount()
//            if oRow:Changed(i)
                lChanged := .t.
                cQuery += oRow:Fieldname(i) + ' = ' + DataToSql(oRow:FieldGet(i)) + ','
//            end                
        Next
        
        if ! (cWhere == '') .and. lChanged

            cQuery := Left( cQuery, len(cQuery) - 1 ) + ' WHERE ' + cWhere            
            
            res := PQexec( ::pDB, cQuery)            
            result := ! ISCHAR(res)   
        
            if ! result     
                ::lError := .t.
                ::cError := res
            else
                ::lError := .f.
                ::cError := ''
            end                
            
            PQclear(res)
            
        elseif ! lChanged
            ::lError := .t.
            ::cError := 'No changed fields found.'            
        end            
    end            
RETURN result


METHOD FieldGet( nRow, nField ) CLASS TPQquery
    Local result
    Local i
    Local cType
    Local nSize

    if ! ::lError .and. nField >= 1 .and. nField <= ::nFields .and. ! ::lclosed
        
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
                result := StoD(left(result,4) + substr(result, 5, 2) + substr(result, 7, 2))
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
                        
        end
    end                    
RETURN result


METHOD Getrow( nRow ) CLASS TPQquery
    Local result, aRow := {}, aOld := {}, nCol
    
    DEFAULT nRow TO ::nRecno

    if ! ::lError .and. ! ::lclosed 
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
    
    if ! ::lError       
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
    end            
RETURN result


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
   METHOD   Changed( nField )   INLINE ! (::aRow[nField] == ::aOld[nField])              
   METHOD   FieldGetOld( nField ) INLINE ::aOld[nField]
ENDCLASS


METHOD new( row, old, struct) CLASS TPQrow
    ::aRow := row
    ::aOld := old
    ::aStruct := struct            
RETURN self


METHOD FieldGet( nField ) CLASS TPQrow
    Local result
    
    if nField >= 1 .and. nField <= len(::aRow)
        result := ::aRow[nField]    
    end
    
RETURN result


METHOD FieldPut( nField, Value ) CLASS TPQrow
    Local result
    
    if nField >= 1 .and. nField <= len(::aRow)
        result := ::aRow[nField] := Value
    end
RETURN result


METHOD FieldName( nField ) CLASS TPQrow
    Local result
    
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
    
    if nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 2]    
    end
    
RETURN result


METHOD FieldLen( nField ) CLASS TPQrow
    Local result
    
    if nField >= 1 .and. nField <= len(::aStruct)
        result := ::aStruct[nField, 3]    
    end
RETURN result


METHOD FieldDec( nField ) CLASS TPQrow
    Local result
    
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
        Local cType, result 

        cType := ValType(xField)
        
        if cType == "C" .or. cType == "M"
                result := "'"+ strtran(xField, "'", ' ') + "'"
        elseif cType == "D"
                result := "'" + StrZero(month(xField),2) + '/' + StrZero(day(xField),2) + '/' + StrZero(Year(xField),4) + "'"
        elseif cType == "N"
                result := str(xField)
        elseif cType == "L"
                result := iif( xField, "'t'", "'f'" )
        end
        
return result           

                                             
