/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    MySQL RDD
 *
 * Copyright 2006 Gianluca Piemonte <info / at / areainformatica / dot / com>
 * www - http://www.harbour-project.org
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
 */

/*
 * This is an experimental RDD for xharbour/contrib/mysql interface.
 * It has been created to test the possibilities of usrrdd.
 * It doesn't support many functions and commands and many things could be optimized.
 */

#include "rddsys.ch"
#include "usrrdd.ch"
#include "fileio.ch"
#include "error.ch"
#include "dbstruct.ch"
#include "common.ch"

#define AREA_QUERY    1
#define AREA_ROW      2
#define AREA_APPEND   3

#define AREA_LEN      3

ANNOUNCE MYSQLRDD

STATIC s_aConnections := {}

FUNCTION DBMYSQLCONNECTION( cConnString )

   LOCAL aParams
   LOCAL oServer
   LOCAL nConn
   LOCAL cHost
   LOCAL cDatabase
   LOCAL cUser
   LOCAL cPassword

   aParams := HB_ATOKENS( cConnString, ";" )

   asize( aParams, 6 )

   cHost     := aParams[1]
   cDatabase := aParams[2]
   cUser     := aParams[3]
   cPassword := aParams[4]

   oServer := TMySQLServer():New( cHost, cUser, cPassword)

   IF oServer:NetErr()
      alert( oServer:ErrorMsg() )
      RETURN 0
   ELSE
      oServer:SelectDB( cDataBase )
      aadd( s_aConnections, oServer )
      nConn := len( s_aConnections )
   ENDIF

RETURN nConn

FUNCTION DBMYSQLCLEARCONNECTION( nConn )

   LOCAL oServer

   oServer := s_aConnections[ nConn ]

   oServer:Destroy()

   s_aConnections[ nConn ] := nil

RETURN SUCCESS

/*
 * non work area methods receive RDD ID as first parameter
 * Methods INIT and EXIT does not have to execute SUPER methods - these is
 * always done by low level USRRDD code
 */
STATIC FUNCTION MYSQL_INIT( nRDD )

   USRRDD_RDDDATA( nRDD )

RETURN SUCCESS

/*
 * methods: NEW and RELEASE receive pointer to work area structure
 * not work area number. It's necessary because the can be executed
 * before work area is allocated
 * these methods does not have to execute SUPER methods - these is
 * always done by low level USRRDD code
 */
STATIC FUNCTION MYSQL_NEW( pWA )

   USRRDD_AREADATA( pWA, array( AREA_LEN ) )

RETURN SUCCESS

STATIC FUNCTION MYSQL_OPEN( nWA, aOpenInfo )
   LOCAL aField, oError, lError, cError, nResult
   LOCAL oServer, oQuery, aStruct, aFieldStruct
   LOCAL aWAData   := USRRDD_AREADATA( nWA )

   if !empty( aOpenInfo[ UR_OI_CONNECT ] ) .and. aOpenInfo[ UR_OI_CONNECT ] <= len( s_aConnections )
      oServer := s_aConnections[ aOpenInfo[ UR_OI_CONNECT ] ]
   endif

   if !empty( oServer )
      oServer:lAllCols := .F.
      oQuery := oServer:Query( aOpenInfo[ UR_OI_NAME ] )
      lError := oQuery:NetErr()
      cError := oQuery:ErrorMsg()
   else
      lError := .T.
      cError := "Invalid connection handle"
   endif

   IF lError
      oError := ErrorNew()
      oError:GenCode     := EG_OPEN
      oError:SubCode     := 1000
      oError:Description := HB_LANGERRMSG( EG_OPEN ) + ", " + cError
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      oError:CanDefault  := .T.
      UR_SUPER_ERROR( nWA, oError )
      RETURN FAILURE
   ELSE
      aWAData[ AREA_QUERY ] := oQuery
   ENDIF

   UR_SUPER_SETFIELDEXTENT( nWA, oQuery:nNumFields )
   aStruct := oQuery:aFieldStruct //Struct()

   FOR EACH aFieldStruct IN aStruct

       aField := ARRAY( UR_FI_SIZE )
       aField[ UR_FI_NAME ]    := aFieldStruct[ DBS_NAME ]
       aField[ UR_FI_TYPE ]    := aFieldStruct[ DBS_TYPE ]
       aField[ UR_FI_TYPEEXT ] := 0
       aField[ UR_FI_LEN ]     := aFieldStruct[ DBS_LEN ]
       aField[ UR_FI_DEC ]     := aFieldStruct[ DBS_DEC ]
       UR_SUPER_ADDFIELD( nWA, aField )

   NEXT

   /* Call SUPER OPEN to finish allocating work area (f.e.: alias settings) */
   nResult := UR_SUPER_OPEN( nWA, aOpenInfo )

RETURN nResult

STATIC FUNCTION MYSQL_CLOSE( nWA )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )

   aWAData[ AREA_QUERY ]:Close()

RETURN UR_SUPER_CLOSE( nWA )

STATIC FUNCTION MYSQL_GETVALUE( nWA, nField, xValue )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )

   if !empty( aWAData[ AREA_ROW ] )
      xValue := aWAData[ AREA_ROW ]:FieldGet( nField )
   else
      xValue := aWAData[ AREA_QUERY ]:FieldGet( nField )
   endif

RETURN SUCCESS

STATIC FUNCTION MYSQL_PUTVALUE( nWA, nField, xValue )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )

   if empty( aWAData[ AREA_ROW ] )
      aWAData[ AREA_ROW ] := aWAData[ AREA_QUERY ]:GetRow()
   endif

   aWAData[ AREA_ROW ]:FieldPut( nField, xValue )

RETURN SUCCESS

STATIC FUNCTION MYSQL_SKIP( nWA, nRecords )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )

   if !empty( aWAData[ AREA_ROW ] )
      MYSQL_FLUSH( nWA )
   endif

   aWAData[ AREA_QUERY ]:Skip( nRecords )

RETURN SUCCESS

STATIC FUNCTION MYSQL_GOTOP( nWA )
RETURN MYSQL_GOTO( nWA, 1 )

STATIC FUNCTION MYSQL_GOBOTTOM( nWA )
RETURN MYSQL_GOTO( nWA, -1 )

STATIC FUNCTION MYSQL_GOTOID( nWA, nRecord )
RETURN MYSQL_GOTO( nWA, nRecord )

STATIC FUNCTION MYSQL_GOTO( nWA, nRecord )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   IF (VALTYPE( nRecord) != "N")          // IF added to prevent error
     nRecord := 0
   ENDIF

   if !empty( aWAData[ AREA_ROW ] )
      MYSQL_FLUSH( nWA )
   endif

   if nRecord < 0
      nRecord := aWAData[ AREA_QUERY ]:LastRec()
   elseif nRecord == 0
      nRecord := aWAData[ AREA_QUERY ]:Recno()
   endif

   aWAData[ AREA_QUERY ]:Goto( nRecord )

RETURN SUCCESS

STATIC FUNCTION MYSQL_RECCOUNT( nWA, nRecords )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )

   nRecords := aWAData[ AREA_QUERY ]:LastRec()

RETURN SUCCESS

STATIC FUNCTION MYSQL_BOF( nWA, lBof )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )

   lBof := aWAData[ AREA_QUERY ]:Bof()

RETURN SUCCESS

STATIC FUNCTION MYSQL_EOF( nWA, lEof )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )

   lEof := aWAData[ AREA_QUERY ]:Eof()

RETURN SUCCESS

STATIC FUNCTION MYSQL_RECID( nWA, nRecNo )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )

   nRecno := aWAData[ AREA_QUERY ]:RecNo()

RETURN SUCCESS

STATIC FUNCTION MYSQL_DELETED( nWA, lDeleted )
(nWA)
   lDeleted := .F.
RETURN SUCCESS

STATIC FUNCTION MYSQL_FLUSH( nWA )
   LOCAL oError
   LOCAL aWAData   := USRRDD_AREADATA( nWA )
   LOCAL nRecno

   if aWAData[ AREA_ROW ] != nil
      if !empty( aWAData[ AREA_APPEND ] )
         aWAData[ AREA_QUERY ]:Append( aWAData[ AREA_ROW ] )
      else
         nRecno := aWAData[ AREA_QUERY ]:nRecNo
         aWAData[ AREA_QUERY ]:Update( aWAData[ AREA_ROW ] )
      endif

      IF aWAData[ AREA_QUERY ]:lError
         oError := ErrorNew()
         oError:GenCode     := EG_DATATYPE
         oError:SubCode     := 3000
         oError:Description := HB_LANGERRMSG( EG_DATATYPE ) + ", " + aWAData[ AREA_QUERY ]:ErrorMsg()
         UR_SUPER_ERROR( nWA, oError )
         RETURN FAILURE
      ENDIF

/*
 * The :Refresh() below costs a lot in term of performance.
 * It redo the select to include inserts and updates.
 * It is the only solution I've found so far to simulate dbf behaviour
 */
      aWAData[ AREA_QUERY ]:Refresh( .T., .F. )

      if !empty( aWAData[ AREA_APPEND ] )
         aWAData[ AREA_APPEND ] := .F.
         nRecno := aWAData[ AREA_QUERY ]:LastRec()
      endif

      aWAData[ AREA_ROW ] := nil

      MYSQL_GOTO( nWA, nRecno )

   endif

RETURN SUCCESS

STATIC FUNCTION MYSQL_APPEND( nWA, nRecords )
   LOCAL aWAData   := USRRDD_AREADATA( nWA )
    (nRecords)
   aWAData[ AREA_ROW ] := aWAData[ AREA_QUERY ]:GetBlankRow()

   aWAData[ AREA_APPEND ] := .T.

RETURN SUCCESS

STATIC FUNCTION MYSQL_DELETE( nWA )
   LOCAL oError
   LOCAL aWAData   := USRRDD_AREADATA( nWA )

   aWAData[ AREA_ROW ] := aWAData[ AREA_QUERY ]:GetRow()

   aWAData[ AREA_QUERY ]:Delete( aWAData[ AREA_ROW ] )

   IF aWAData[ AREA_QUERY ]:lError
      oError := ErrorNew()
      oError:GenCode     := EG_DATATYPE
      oError:SubCode     := 2000
      oError:Description := HB_LANGERRMSG( EG_DATATYPE ) + ", " + aWAData[ AREA_QUERY ]:ErrorMsg()
      UR_SUPER_ERROR( nWA, oError )
      RETURN FAILURE
   ENDIF

   aWAData[ AREA_ROW ] := nil

RETURN SUCCESS

/*
 * This function have to exist in all RDD and then name have to be in
 * format: <RDDNAME>_GETFUNCTABLE
 */
FUNCTION MYSQLRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID )
   LOCAL cSuperRDD := NIL     /* NO SUPER RDD */
   LOCAL aMyFunc[ UR_METHODCOUNT ]

   aMyFunc[ UR_INIT         ] := ( @MYSQL_INIT()         )
   aMyFunc[ UR_NEW          ] := ( @MYSQL_NEW()          )
   aMyFunc[ UR_OPEN         ] := ( @MYSQL_OPEN()         )
   aMyFunc[ UR_GETVALUE     ] := ( @MYSQL_GETVALUE()     )
   aMyFunc[ UR_PUTVALUE     ] := ( @MYSQL_PUTVALUE()     )
   aMyFunc[ UR_SKIP         ] := ( @MYSQL_SKIP()         )
   aMyFunc[ UR_GOTO         ] := ( @MYSQL_GOTO()         )
   aMyFunc[ UR_GOTOID       ] := ( @MYSQL_GOTOID()       )
   aMyFunc[ UR_GOTOP        ] := ( @MYSQL_GOTOP()        )
   aMyFunc[ UR_GOBOTTOM     ] := ( @MYSQL_GOBOTTOM()     )
   aMyFunc[ UR_RECNO ]        := ( @MYSQL_RECID() )
   aMyFunc[ UR_RECCOUNT     ] := ( @MYSQL_RECCOUNT()     )
   aMyFunc[ UR_RECID        ] := ( @MYSQL_RECID()        )
   aMyFunc[ UR_BOF          ] := ( @MYSQL_BOF()          )
   aMyFunc[ UR_EOF          ] := ( @MYSQL_EOF()          )
   aMyFunc[ UR_DELETED      ] := ( @MYSQL_DELETED()      )
   aMyFunc[ UR_FLUSH        ] := ( @MYSQL_FLUSH()        )
   aMyFunc[ UR_APPEND       ] := ( @MYSQL_APPEND()       )
   aMyFunc[ UR_DELETE       ] := ( @MYSQL_DELETE()       )
   aMyFunc[ UR_CLOSE        ] := ( @MYSQL_CLOSE()        )

RETURN USRRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID, ;
                            cSuperRDD, aMyFunc )

INIT PROC MYSQL_INIT()
   rddRegister( "MYSQLRDD", RDT_FULL )
RETURN
