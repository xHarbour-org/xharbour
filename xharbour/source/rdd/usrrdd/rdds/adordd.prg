/*
 * $Id: adordd.prg,v 1.6 2007/05/09 15:05:56 marchuet Exp $
 */

/*
 * Harbour Project source code:
 * ADORDD - RDD to automatically manage Microsoft ADO
 *
 * Copyright 2007 Fernando Mancera <fmancera@viaopen.com> and
 * Antonio Linares <alinares@fivetechsoft.com>
 * www - http://www.harbour-project.org
 *
 * Copyright 2007 Miguel Angel Marchuet <miguelangel@marchuet.net>
 *  ADO_GOTOID( nWA, nRecord )
 *  ADO_GOTO( nWA, nRecord )
 *  ADO_OPEN( nWA, aOpenInfo ) some modifications
 *     Open: Excel files
 *           Paradox files
 *           Access with password
 *           FireBird
 *  ADO_CLOSE( nWA )
 *  ADO_ZAP( nWA )
 *  ADO_ORDINFO( nWA, nIndex, aOrderInfo ) some modifications
 *  ADO_RECINFO( nWA, nRecord, nInfoType, uInfo )
 *
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

#include "rddsys.ch"
#include "fileio.ch"
#include "error.ch"
#include "adordd.ch"
#include "common.ch"
#include "dbstruct.ch"

#ifndef __XHARBOUR__
   #include "hbusrrdd.ch"
   #xcommand TRY              => bError := errorBlock( {|oErr| break( oErr ) } ) ;;
                                 BEGIN SEQUENCE
   #xcommand CATCH [<!oErr!>] => errorBlock( bError ) ;;
                                 RECOVER [USING <oErr>] <-oErr-> ;;
                                 errorBlock( bError )
   #command FINALLY           => ALWAYS
#else
   #include "usrrdd.ch"
#endif

#define WA_RECORDSET  1
#define WA_BOF        2
#define WA_EOF        3
#define WA_CONNECTION 4
#define WA_CATALOG    5
#define WA_TABLENAME  6
#define WA_ENGINE     7
#define WA_SERVER     8
#define WA_USERNAME   9
#define WA_PASSWORD  10
#define WA_QUERY     11
#define WA_LOCATEFOR 12
#define WA_SCOPEINFO 13
#define WA_SQLTRUCT  14
#define WA_CONNOPEN  15

#define WA_SIZE      15

ANNOUNCE ADORDD

static bError, s_cTableName, s_cEngine, s_cServer, s_cUserName, s_cPassword, s_cQuery := ""

#ifdef __XHARBOUR__

static function HB_TokenGet( cText, nPos, cSep )

   local aTokens := HB_ATokens( cText, cSep )

return If( nPos <= Len( aTokens ), aTokens[ nPos ], "" )

#endif

STATIC FUNCTION ADO_INIT( nRDD )

   LOCAL aRData

   USRRDD_RDDDATA( nRDD, aRData )

RETURN SUCCESS

STATIC FUNCTION ADO_NEW( nWA )

   LOCAL aWAData := Array( WA_SIZE )

   aWAData[ WA_BOF ] = .F.
   aWAData[ WA_EOF ] = .F.

   USRRDD_AREADATA( nWA, aWAData )

RETURN SUCCESS

STATIC FUNCTION ADO_CREATE( nWA, aOpenInfo )

   LOCAL cDataBase   := HB_TokenGet( aOpenInfo[ UR_OI_NAME ], 1, ";" )
   LOCAL cTableName  := HB_TokenGet( aOpenInfo[ UR_OI_NAME ], 2, ";" )
   LOCAL cDbEngine   := HB_TokenGet( aOpenInfo[ UR_OI_NAME ], 3, ";" )
   LOCAL cServer     := HB_TokenGet( aOpenInfo[ UR_OI_NAME ], 4, ";" )
   LOCAL cUserName   := HB_TokenGet( aOpenInfo[ UR_OI_NAME ], 5, ";" )
   LOCAL cPassword   := HB_TokenGet( aOpenInfo[ UR_OI_NAME ], 6, ";" )
   LOCAL oConnection := TOleAuto():New( "ADODB.Connection" )
   LOCAL oCatalog    := TOleAuto():New( "ADOX.Catalog" )
   local aWAData     := USRRDD_AREADATA( nWA )
   local oError

   DO CASE
      CASE Lower( Right( cDataBase, 4 ) ) == ".mdb"
           IF ! File( cDataBase )
              oCatalog:Create( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + cDataBase )
           ENDIF
           oConnection:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + cDataBase )

      CASE Lower( Right( cDataBase, 4 ) ) == ".xls"
           IF ! File( cDataBase )
              oCatalog:Create( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + cDataBase + ";Extended Properties='Excel 8.0;HDR=YES';Persist Security Info=False" )
           ENDIF
           oConnection:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + cDataBase + ";Extended Properties='Excel 8.0;HDR=YES';Persist Security Info=False")

      CASE Lower( Right( cDataBase, 3 ) ) == ".db"
           IF ! File( cDataBase )
              oCatalog:Create( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + cDataBase + ";Extended Properties='Paradox 3.x';" )
           ENDIF
           oConnection:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + cDataBase + ";Extended Properties='Paradox 3.x';" )

      case Upper( cDbEngine ) == "MYSQL"
           oConnection:Open( "DRIVER={MySQL ODBC 3.51 Driver};" + ;
                             "server=" + cServer + ;
                             ";database=" + cDataBase + ;
                             ";uid=" + cUserName + ;
                             ";pwd=" + cPassword )

   ENDCASE

   TRY
      oConnection:Execute( "DROP TABLE " + cTableName )
   CATCH
   END TRY

   TRY
      oConnection:Execute( "CREATE TABLE [" + cTableName + "] (" + aWAData[ WA_SQLTRUCT ] + ")" )
   CATCH
      oError := ErrorNew()
      oError:GenCode     := EG_CREATE
      oError:SubCode     := 1004
      oError:Description := HB_LANGERRMSG( EG_CREATE ) + " (" + ;
                            HB_LANGERRMSG( EG_UNSUPPORTED ) + ")"
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      oError:CanDefault  := .T.
      UR_SUPER_ERROR( nWA, oError )
   END

   oConnection:Close()

RETURN SUCCESS

STATIC FUNCTION ADO_CREATEFIELDS( nWA, aStruct )

  LOCAL aWAData := USRRDD_AREADATA( nWA )
  LOCAL n

  aWAData[ WA_SQLTRUCT ] = ""

  FOR n = 1 to Len( aStruct )
     IF n > 1
        aWAData[ WA_SQLTRUCT ] += ", "
     ENDIF
     aWAData[ WA_SQLTRUCT ] += "[" + aStruct[ n ][ DBS_NAME ] + "]"
     DO CASE
        CASE aStruct[ n ][ DBS_TYPE ] $ "C,Character"
             aWAData[ WA_SQLTRUCT ] += " CHAR(" + AllTrim( Str( aStruct[ n ][ DBS_LEN ] ) ) + ") NULL"

        CASE aStruct[ n ][ DBS_TYPE ] == "V"
             aWAData[ WA_SQLTRUCT ] += " VARCHAR(" + AllTrim( Str( aStruct[ n ][ DBS_LEN ] ) ) + ") NULL"

        CASE aStruct[ n ][ DBS_TYPE ] == "B"
             aWAData[ WA_SQLTRUCT ] += " DOUBLE NULL"

        CASE aStruct[ n ][ DBS_TYPE ] == "Y"
             aWAData[ WA_SQLTRUCT ] += " SMALLINT NULL"

        CASE aStruct[ n ][ DBS_TYPE ] == "I"
             aWAData[ WA_SQLTRUCT ] += " MEDIUMINT NULL"

        CASE aStruct[ n ][ DBS_TYPE ] == "D"
             aWAData[ WA_SQLTRUCT ] += " DATE NULL"

        CASE aStruct[ n ][ DBS_TYPE ] == "T"
             aWAData[ WA_SQLTRUCT ] += " DATETIME NULL"

        CASE aStruct[ n ][ DBS_TYPE ] == "@"
             aWAData[ WA_SQLTRUCT ] += " TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP"

        CASE aStruct[ n ][ DBS_TYPE ] == "M"
             aWAData[ WA_SQLTRUCT ] += " TEXT NULL"

        CASE aStruct[ n ][ DBS_TYPE ] == "N"
             aWAData[ WA_SQLTRUCT ] += " NUMERIC(" + AllTrim( Str( aStruct[ n ][ DBS_LEN ] ) ) + ")"

        CASE aStruct[ n ][ DBS_TYPE ] == "L"
             aWAData[ WA_SQLTRUCT ] += " LOGICAL"
     ENDCASE
  NEXT

RETURN SUCCESS

STATIC FUNCTION ADO_OPEN( nWA, aOpenInfo )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL cName, aField, oError, nResult
   LOCAL oRecordSet, nTotalFields, n

   // When there is no ALIAS we will create new one using file name
   IF aOpenInfo[ UR_OI_ALIAS ] == NIL
      HB_FNAMESPLIT( aOpenInfo[ UR_OI_NAME ], , @cName )
      aOpenInfo[ UR_OI_ALIAS ] := cName
   ENDIF

   IF Empty( aOpenInfo[ UR_OI_CONNECT ] )
      aWAData[ WA_CONNECTION ] = TOleAuto():New( "ADODB.Connection" )
      aWAData[ WA_TABLENAME ] = s_cTableName
      aWAData[ WA_QUERY ]    = s_cQuery
      aWAData[ WA_USERNAME ] = s_cUserName
      aWAData[ WA_PASSWORD ] = s_cPassword
      aWAData[ WA_SERVER ] = s_cServer
      aWAData[ WA_ENGINE ] = s_cEngine
      aWAData[ WA_CONNOPEN ] = .T.


      DO CASE
      CASE Lower( Right( aOpenInfo[ UR_OI_NAME ], 4 ) ) == ".mdb"
           IF Empty( aWAData[ WA_PASSWORD ] )
              aWAData[ WA_CONNECTION ]:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + aOpenInfo[ UR_OI_NAME ] )
           ELSE
              aWAData[ WA_CONNECTION ]:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + aOpenInfo[ UR_OI_NAME ] + ";Jet OLEDB:Database Password=" + AllTrim( aWAData[ WA_PASSWORD ] ) )
           ENDIF

      CASE Lower( Right( aOpenInfo[ UR_OI_NAME ], 4 ) ) == ".xls"
           aWAData[ WA_CONNECTION ]:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + aOpenInfo[ UR_OI_NAME ] + ";Extended Properties='Excel 8.0;HDR=YES';Persist Security Info=False" )

      CASE Lower( Right( aOpenInfo[ UR_OI_NAME ], 4 ) ) == ".dbf"
           aWAData[ WA_CONNECTION ]:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + aOpenInfo[ UR_OI_NAME ] + ";Extended Properties=dBASE IV;User ID=Admin;Password=;" )

      CASE Lower( Right( aOpenInfo[ UR_OI_NAME ], 3 ) ) == ".db"
           aWAData[ WA_CONNECTION ]:Open( "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" + aOpenInfo[ UR_OI_NAME ] + ";Extended Properties='Paradox 3.x';" )

      CASE aWAData[ WA_ENGINE ] == "MYSQL"
           aWAData[ WA_CONNECTION ]:Open( "DRIVER={MySQL ODBC 3.51 Driver};" + ;
                                          "server=" + aWAData[ WA_SERVER ] + ;
                                          ";database=" + aOpenInfo[ UR_OI_NAME ] + ;
                                          ";uid=" + aWAData[ WA_USERNAME ] + ;
                                          ";pwd=" + aWAData[ WA_PASSWORD ] )

      case aWAData[ WA_ENGINE ] == "SQL"
           aWAData[ WA_CONNECTION ]:Open( "Provider=SQLOLEDB;" + ;
                                          "server=" + aWAData[ WA_SERVER ] + ;
                                          ";database=" + aOpenInfo[ UR_OI_NAME ] + ;
                                          ";uid=" + aWAData[ WA_USERNAME ] + ;
                                          ";pwd=" + aWAData[ WA_PASSWORD ] )

      CASE aWAData[ WA_ENGINE ] == "ORACLE"
           aWAData[ WA_CONNECTION ]:Open( "Provider=MSDAORA.1;" + ;
                                          "Persist Security Info=False" + ;
                                          If( Empty( aWAData[ WA_SERVER ] ),;
                                          "", ";Data source=" + aWAData[ WA_SERVER ] ) + ;
                                          ";User ID=" + aWAData[ WA_USERNAME ] + ;
                                          ";Password=" + aWAData[ WA_PASSWORD ] )
      CASE aWAData[ WA_ENGINE ] == "FIREBIRD"
           aWAData[ WA_CONNECTION ]:Open( "Driver=Firebird/InterBase(r) driver;" + ;
                                          "Persist Security Info=False" +;
                                          ";Uid=" + aWAData[ WA_USERNAME ] +;
                                          ";Pwd=" + aWAData[ WA_PASSWORD ] +;
                                          ";DbName=" + aOpenInfo[ UR_OI_NAME ] )
      ENDCASE
   ELSE
      aWAData[ WA_CONNECTION ] = TOleAuto():New( aOpenInfo[ UR_OI_CONNECT ], "ADODB.Connection" )
      aWAData[ WA_TABLENAME ] = s_cTableName
      aWAData[ WA_QUERY ]    = s_cQuery
      aWAData[ WA_USERNAME ] = s_cUserName
      aWAData[ WA_PASSWORD ] = s_cPassword
      aWAData[ WA_SERVER ] = s_cServer
      aWAData[ WA_ENGINE ] = s_cEngine
      aWAData[ WA_CONNOPEN ] = .F.
   ENDIF

   oRecordSet := TOleAuto():New( "ADODB.Recordset" )

   IF oRecordSet == NIL
      oError := ErrorNew()
      oError:GenCode     := EG_OPEN
      oError:SubCode     := 1001
      oError:Description := HB_LANGERRMSG( EG_OPEN )
      oError:FileName    := aOpenInfo[ UR_OI_NAME ]
      oError:OsCode      := fError()
      oError:CanDefault  := .T.

      UR_SUPER_ERROR( nWA, oError )
      RETURN FAILURE
   ENDIF
   oRecordSet:CursorType     = adOpenDynamic
   oRecordSet:CursorLocation = adUseClient
   oRecordSet:LockType       = adLockPessimistic
   oRecordSet:Open( aWAData[ WA_QUERY ] + aWAData[ WA_TABLENAME ], aWAData[ WA_CONNECTION ] )
   aWAData[ WA_RECORDSET ] := oRecordSet
   aWAData[ WA_BOF ] := aWAData[ WA_EOF ] := .F.

   UR_SUPER_SETFIELDEXTENT( nWA, nTotalFields := oRecordSet:Fields:Count )

   FOR n = 1 TO nTotalFields
      aField := ARRAY( UR_FI_SIZE )
      aField[ UR_FI_NAME ]    := oRecordSet:Fields( n - 1 ):Name
      aField[ UR_FI_TYPE ]    := ADO_GETFIELDTYPE( oRecordSet:Fields( n - 1 ):Type )
      aField[ UR_FI_TYPEEXT ] := 0
      aField[ UR_FI_LEN ]     := ADO_GETFIELDSIZE( aField[ UR_FI_TYPE ], oRecordSet:Fields( n - 1 ):DefinedSize )
      aField[ UR_FI_DEC ]     := 0
      UR_SUPER_ADDFIELD( nWA, aField )
   NEXT

   nResult := UR_SUPER_OPEN( nWA, aOpenInfo )

   IF nResult == SUCCESS
      ADO_GOTOP( nWA )
   ENDIF

RETURN nResult

STATIC FUNCTION ADO_CLOSE( nWA )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   TRY
      oRecordSet:Close()
      IF ! Empty( aWAData[ WA_CONNOPEN ] )
        IF aWAData[ WA_CONNECTION ]:State != adStateClosed
           IF aWAData[ WA_CONNECTION ]:State != adStateOpen
              aWAData[ WA_CONNECTION ]:Cancel()
           ELSE
              aWAData[ WA_CONNECTION ]:Close()
           ENDIF
        ENDIF
      ENDIF
   CATCH
   END

RETURN UR_SUPER_CLOSE( nWA )

STATIC FUNCTION ADO_GETVALUE( nWA, nField, xValue )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   IF aWAData[ WA_EOF ] .or. oRecordSet:RecordCount() == 0
      xValue := nil
   ELSE
      xValue := oRecordSet:Fields( nField - 1 ):Value
   ENDIF

RETURN SUCCESS

STATIC FUNCTION ADO_GOTO( nWA, nRecord )

   LOCAL nRecNo

   WITH OBJECT USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
      IF :RecordCount > 0
         :MoveFirst()
         :Move( nRecord - 1, 0 )
      ENDIF
      ADO_RECID( nWA, @nRecNo )
   END WITH

RETURN If( nRecord == nRecNo, SUCCESS, FAILURE )

STATIC FUNCTION ADO_GOTOID( nWA, nRecord )
   LOCAL nRecNo

   WITH OBJECT USRRDD_AREADATA( nWA )[ WA_RECORDSET ]
      IF :RecordCount > 0
         :MoveFirst()
         :Move( nRecord - 1, 0 )
      ENDIF
      ADO_RECID( nWA, @nRecNo )
   END WITH

RETURN If( nRecord == nRecNo, SUCCESS, FAILURE )

STATIC FUNCTION ADO_GOTOP( nWA )

   LOCAL aWAData    := USRRDD_AREADATA( nWA )
   LOCAL oRecordSet := aWAData[ WA_RECORDSET ]

   IF oRecordSet:RecordCount != 0
      oRecordSet:MoveFirst()
   ENDIF
   aWAData[ WA_BOF ] = .F.
   aWAData[ WA_EOF ] = .F.

RETURN SUCCESS

STATIC FUNCTION ADO_GOBOTTOM( nWA )

   LOCAL aWAData    := USRRDD_AREADATA( nWA )
   LOCAL oRecordSet := aWAData[ WA_RECORDSET ]

   oRecordSet:MoveLast()
   USRRDD_AREADATA( nWA )[ 2 ] = .f.
   USRRDD_AREADATA( nWA )[ 3 ] = .f.

RETURN SUCCESS

STATIC FUNCTION ADO_SKIPRAW( nWA, nRecords )

   LOCAL aWAData    := USRRDD_AREADATA( nWA )
   LOCAL oRecordSet := aWAData[ WA_RECORDSET ]

   IF nRecords != 0
      IF aWAData[ WA_EOF ]
         IF nRecords > 0
            RETURN SUCCESS
         ENDIF
         ADO_GOBOTTOM( nWA )
         ++nRecords
       ENDIF
       IF nRecords < 0 .AND. oRecordSet:AbsolutePosition <= -nRecords
          oRecordSet:MoveFirst()
          aWAData[ WA_BOF ] := .T.
          aWAData[ WA_EOF ] := oRecordSet:EOF
       ELSEIF nRecords != 0
          oRecordSet:Move( nRecords )
          aWAData[ WA_BOF ] := .F.
          aWAData[ WA_EOF ] := oRecordSet:EOF
       ENDIF
   ENDIF

RETURN SUCCESS

STATIC FUNCTION ADO_BOF( nWA, lBof )

   LOCAL aWAData := USRRDD_AREADATA( nWA )

   lBof := aWAData[ WA_BOF ]

RETURN SUCCESS

STATIC FUNCTION ADO_EOF( nWA, lEof )

   local oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   lEof := ( oRecordSet:AbsolutePosition == -3 )

RETURN SUCCESS

STATIC FUNCTION ADO_DELETED( nWA, lDeleted )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   TRY
      IF oRecordSet:Status == adRecDeleted
	      lDeleted := .T.
	   ELSE
	      lDeleted := .F.
	   ENDIF
   CATCH
      lDeleted := .F.
   END TRY

RETURN SUCCESS

STATIC FUNCTION ADO_DELETE( nWA )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   oRecordSet:Delete()

   ADO_SKIPRAW( nWA, 1 )

RETURN SUCCESS

STATIC FUNCTION ADO_RECID( nWA, nRecNo )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   nRecno := If( oRecordSet:AbsolutePosition == -3, oRecordSet:RecordCount() + 1, oRecordSet:AbsolutePosition )

RETURN SUCCESS

STATIC FUNCTION ADO_RECCOUNT( nWA, nRecords )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   nRecords := oRecordSet:RecordCount()

RETURN SUCCESS

STATIC FUNCTION ADO_PUTVALUE( nWA, nField, xValue )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL oRecordSet := aWAData[ WA_RECORDSET ]

   IF ! aWAData[ WA_EOF ]
      oRecordSet:Fields( nField - 1 ):Value := xValue
      TRY
         oRecordSet:Update()
      CATCH
      END
   ENDIF

RETURN SUCCESS

STATIC FUNCTION ADO_APPEND( nWA, lUnLockAll )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   oRecordSet:AddNew()

	TRY
      oRecordSet:Update()
	CATCH
   END

RETURN SUCCESS

STATIC FUNCTION ADO_FLUSH( nWA )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   oRecordSet:Update()

RETURN SUCCESS

STATIC FUNCTION ADO_ORDINFO( nWA, nIndex, aOrderInfo )

   LOCAL aWAData    := USRRDD_AREADATA( nWA )
   LOCAL oRecordSet := aWAData[ WA_RECORDSET ]
   LOCAL nResult := SUCCESS

	DO CASE
	CASE nIndex == UR_ORI_TAG
	   IF aOrderInfo[ UR_ORI_TAG ] < aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes:Count
         aOrderInfo[ UR_ORI_RESULT ] = aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes( aOrderInfo[ UR_ORI_TAG ] ):Name
      ELSE
         aOrderInfo[ UR_ORI_RESULT ] = ""
      ENDIF
   CASE nIndex == 26 // #define DBOI_KEYCOUNT             26  /* The count of keys in scope and filter */
      IF aWAData[ WA_CONNECTION ]:State != adStateClosed
         aOrderInfo[ UR_ORI_RESULT ] := oRecordSet:RecordCount
      ELSE
         aOrderInfo[ UR_ORI_RESULT ] := 0
         nResult := FAILURE
      ENDIF
   CASE nIndex == UR_ORC_DESCEND
      aOrderInfo[ UR_ORI_RESULT ] := .F.
	ENDCASE

RETURN nResult

STATIC FUNCTION ADO_RECINFO( nWA, nRecord, nInfoType, uInfo )

   LOCAL nResult := SUCCESS
   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   DO CASE
   CASE nInfoType == UR_DBRI_DELETED
      uInfo := .F.
   CASE nInfoType == UR_DBRI_LOCKED
      uInfo := .T.
   CASE nInfoType == UR_DBRI_RECSIZE
   CASE nInfoType == UR_DBRI_RECNO
      nResult := ADO_RECID( nWA, @nRecord )
   CASE nInfoType == UR_DBRI_UPDATED
      uInfo := .F.
   CASE nInfoType == UR_DBRI_ENCRYPTED
      uInfo := .F.
   CASE nInfoType == UR_DBRI_RAWRECORD
      uInfo := ""
   CASE nInfoType == UR_DBRI_RAWMEMOS
      uInfo := ""
   CASE nInfoType == UR_DBRI_RAWDATA
      nResult := ADO_GOTO( nWA, nRecord )
      uInfo := ""
   ENDCASE

RETURN nResult

STATIC FUNCTION ADO_ORDLSTFOCUS( nWA, aOrderInfo )
/*
   LOCAL nRecNo

   WITH OBJECT USRRDD_AREADATA( nWA )[ 1 ]
      ADO_RECID( nWA, @nRecNo )

      :Close()
      __OutDebug( aOrderInfo )
      __outDebug("SELECT * FROM " + s_aTableNames[ nWA ] )
      IF aOrderInfo[ UR_ORI_TAG ] = 0
          :Open( "SELECT * FROM " + s_aTableNames[ nWA ] , HB_QWith(), adOpenDynamic, adLockPessimistic )
      ELSE
          //:Open( "SELECT * FROM " + ::oTabla:cTabla + ' ORDER BY ' + ::OrdKey( uTag ) , QWith(), adOpenDynamic, adLockPessimistic, adCmdUnspecified )
          :Open( "SELECT * FROM " + s_aTableNames[ nWA ], HB_QWith(), adOpenDynamic, adLockPessimistic )
      ENDIF
      aOrderInfo[ UR_ORI_RESULT ] = aOrderInfo[ UR_ORI_TAG ]

      ADO_GOTOP( nWA )
      ADO_GOTO( nWA, nRecNo )
   END WITH
*/
RETURN SUCCESS

STATIC FUNCTION ADO_PACK( nWA )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

RETURN SUCCESS

STATIC FUNCTION ADO_RAWLOCK( nWA, nAction, nRecNo )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

RETURN SUCCESS

STATIC FUNCTION ADO_LOCK( nWA, aLockInfo  )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

	aLockInfo[ UR_LI_METHOD ] := DBLM_MULTIPLE
   aLockInfo[ UR_LI_RECORD ] := RECNO()
   aLockInfo[ UR_LI_RESULT ] := .T.

RETURN SUCCESS

STATIC FUNCTION ADO_UNLOCK( nWA, xRecID )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

RETURN SUCCESS

STATIC FUNCTION ADO_SETFILTER( nWA, aFilterInfo )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   oRecordSet:Filter = SQLTranslate( aFilterInfo[ UR_FRI_CEXPR ] )

RETURN SUCCESS

STATIC FUNCTION ADO_CLEARFILTER( nWA )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   TRY
      oRecordSet:Filter = ""
	CATCH
   END

RETURN SUCCESS

STATIC FUNCTION ADO_ZAP( nWA )

  LOCAL aWAData    := USRRDD_AREADATA( nWA )
  LOCAL oRecordSet := aWAData[ WA_RECORDSET ]

  IF aWAData[ WA_CONNECTION ] != NIL .and. aWAData[ WA_TABLENAME ] != nil
     TRY
        // Intentamos un ZAP real para inicializar tambien los contadores de campos autoincrementales
        aWAData[ WA_CONNECTION ]:Execute( "TRUNCATE TABLE " + aWAData[ WA_TABLENAME ] )
     CATCH
        aWAData[ WA_CONNECTION ]:Execute( "DELETE * FROM " + aWAData[ WA_TABLENAME ] )
     END
     oRecordSet:Requery()
  ENDIF

RETURN SUCCESS

STATIC FUNCTION ADO_SETLOCATE( nWA, aScopeInfo )

   LOCAL aWAData := USRRDD_AREADATA( nWA )

   aScopeInfo[ UR_SI_CFOR ] = SQLTranslate( aWAData[ WA_LOCATEFOR ] )

   aWAData[ WA_SCOPEINFO ] = aScopeInfo

return SUCCESS

STATIC FUNCTION ADO_LOCATE( nWA, lContinue )

   LOCAL aWAData    := USRRDD_AREADATA( nWA )
   LOCAL oRecordSet := aWAData[ WA_RECORDSET ]

   oRecordSet:Find( aWAData[ WA_SCOPEINFO ][ UR_SI_CFOR ], If( lContinue, 1, 0 ) )

RETURN SUCCESS

STATIC FUNCTION ADO_CLEARREL( nWA )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL nKeys := 0, cKeyName

   IF aWAData[ WA_CATALOG ] != nil .and. aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys != nil
      TRY
         nKeys = aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys:Count
      CATCH
      END
   ENDIF

   IF nKeys > 0
      cKeyName = aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys( nKeys - 1 ):Name
      IF Upper( cKeyName ) != "PRIMARYKEY"
         aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys:Delete( cKeyName )
      ENDIF
   ENDIF

RETURN SUCCESS

STATIC FUNCTION ADO_RELAREA( nWA, nRelNo, nRelArea )

   LOCAL aWAData := USRRDD_AREADATA( nWA )

   IF nRelNo <= aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys:Count()
      nRelArea = Select( aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys( nRelNo - 1 ):RelatedTable )
   ENDIF

RETURN SUCCESS

STATIC FUNCTION ADO_RELTEXT( nWA, nRelNo, cExpr )

   LOCAL aWAData := USRRDD_AREADATA( nWA )

   IF nRelNo <= aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys:Count()
      cExpr = aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys( nRelNo - 1 ):Columns( 0 ):RelatedColumn
   ENDIF

RETURN SUCCESS

STATIC FUNCTION ADO_SETREL( nWA, aRelInfo )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL cParent := Alias( aRelInfo[ UR_RI_PARENT ] )
   LOCAL cChild  := Alias( aRelInfo[ UR_RI_CHILD ] )
   LOCAL cKeyName := cParent + "_" + cChild

   TRY
      aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Keys:Append( cKeyName, adKeyForeign,;
                                    aRelInfo[ UR_RI_CEXPR ], cChild, aRelInfo[ UR_RI_CEXPR ] )
   CATCH
      // raise error for can't create relation
   END

RETURN SUCCESS

STATIC FUNCTION ADO_ORDLSTADD( nWA, aOrderInfo )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   TRY
      oRecordSet:Index = aOrderInfo[ UR_ORI_BAG ]
   CATCH
	END

RETURN SUCCESS

STATIC FUNCTION ADO_ORDLSTCLEAR( nWA )

   LOCAL oRecordSet := USRRDD_AREADATA( nWA )[ WA_RECORDSET ]

   TRY
      oRecordSet:Index = ""
   CATCH
   END

RETURN SUCCESS

STATIC FUNCTION ADO_ORDCREATE( nWA, aOrderCreateInfo )

   LOCAL aWAData := USRRDD_AREADATA( nWA )
   LOCAL oIndex

   TRY
      IF aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes( aOrderCreateInfo[ UR_ORCR_BAGNAME ] ) == nil
         oIndex = TOleAuto():New( "ADOX.Index" )
         oIndex:Name = aOrderCreateInfo[ UR_ORCR_BAGNAME ]
         oIndex:PrimaryKey = .F.
         oIndex:Unique = aOrderCreateInfo[ UR_ORCR_UNIQUE ]
         oIndex:Columns:Append( aOrderCreateInfo[ UR_ORCR_CKEY ] )
         aWAData[ WA_CATALOG ]:Tables( aWAData[ WA_TABLENAME ] ):Indexes:Append( oIndex )
      ENDIF
   CATCH
      // raise error for can't create index
   END

RETURN SUCCESS

FUNCTION ADORDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID )

   LOCAL cSuperRDD   /* NO SUPER RDD */
   LOCAL aMyFunc[ UR_METHODCOUNT ]

   aMyFunc[ UR_INIT ]      := ( @ADO_INIT() )
   aMyFunc[ UR_NEW ]       := ( @ADO_NEW() )
   aMyFunc[ UR_CREATE ]    := ( @ADO_CREATE() )
   aMyFunc[ UR_CREATEFIELDS ] := ( @ADO_CREATEFIELDS() )
   aMyFunc[ UR_OPEN ]      := ( @ADO_OPEN() )
   aMyFunc[ UR_CLOSE ]     := ( @ADO_CLOSE() )
   aMyFunc[ UR_BOF  ]      := ( @ADO_BOF() )
   aMyFunc[ UR_EOF  ]      := ( @ADO_EOF() )
   aMyFunc[ UR_DELETED ]   := ( @ADO_DELETED() )
   aMyFunc[ UR_SKIPRAW ]   := ( @ADO_SKIPRAW() )
   aMyFunc[ UR_GOTO ]      := ( @ADO_GOTO() )
   aMyFunc[ UR_GOTOID ]    := ( @ADO_GOTOID() )
   aMyFunc[ UR_GOTOP ]     := ( @ADO_GOTOP() )
   aMyFunc[ UR_GOBOTTOM ]  := ( @ADO_GOBOTTOM() )
   aMyFunc[ UR_RECID ]     := ( @ADO_RECID() )
   aMyFunc[ UR_RECCOUNT ]  := ( @ADO_RECCOUNT() )
   aMyFunc[ UR_GETVALUE ]  := ( @ADO_GETVALUE() )
   aMyFunc[ UR_PUTVALUE ]  := ( @ADO_PUTVALUE() )
   aMyFunc[ UR_DELETE ]    := ( @ADO_DELETE() )
   aMyFunc[ UR_APPEND ]    := ( @ADO_APPEND() )
   aMyFunc[ UR_FLUSH ]     := ( @ADO_FLUSH() )
   aMyFunc[ UR_ORDINFO ]   := ( @ADO_ORDINFO() )
   aMyFunc[ UR_RECINFO ]   := ( @ADO_RECINFO() )
   aMyFunc[ UR_ORDLSTFOCUS ] := ( @ADO_ORDLSTFOCUS() )
   aMyFunc[ UR_PACK ]      := ( @ADO_PACK() )
   aMyFunc[ UR_RAWLOCK ]   := ( @ADO_RAWLOCK() )
   aMyFunc[ UR_LOCK ]      := ( @ADO_LOCK() )
   aMyFunc[ UR_UNLOCK ]    := ( @ADO_UNLOCK() )
   aMyFunc[ UR_SETFILTER ] := ( @ADO_SETFILTER() )
   aMyFunc[ UR_CLEARFILTER ] := ( @ADO_CLEARFILTER() )
   aMyFunc[ UR_ZAP ]       := ( @ADO_ZAP() )
   aMyFunc[ UR_SETLOCATE ] := ( @ADO_SETLOCATE() )
   aMyFunc[ UR_LOCATE ]    := ( @ADO_LOCATE() )
   aMyFunc[ UR_CLEARREL ]  := ( @ADO_CLEARREL() )
   aMyFunc[ UR_RELAREA ]   := ( @ADO_RELAREA() )
   aMyFunc[ UR_RELTEXT ]   := ( @ADO_RELTEXT() )
   aMyFunc[ UR_SETREL ]    := ( @ADO_SETREL() )
   aMyFunc[ UR_ORDCREATE ] := ( @ADO_ORDCREATE() )
   aMyFunc[ UR_ORDLSTADD ] := ( @ADO_ORDLSTADD() )
   aMyFunc[ UR_ORDLSTCLEAR ] := ( @ADO_ORDLSTCLEAR() )

RETURN USRRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID, cSuperRDD,;
                            aMyFunc )

INIT PROCEDURE ADORDD_INIT()
   rddRegister( "ADORDD", RDT_FULL )
RETURN

STATIC FUNCTION ADO_GETFIELDSIZE( nDBFTypeField, nADOFieldSize )

   LOCAL nDBFFieldSize := 0

   DO CASE

      CASE nDBFTypeField == HB_FT_STRING
           nDBFFieldSize := nADOFieldSize

      CASE nDBFTypeField == HB_FT_INTEGER
           nDBFFieldSize := nADOFieldSize

      CASE nDBFTypeField == HB_FT_DATE
           nDBFFieldSize := 8

      CASE nDBFTypeField == HB_FT_DATETIME
           nDBFFieldSize := 8

      CASE nDBFTypeField == HB_FT_TIMESTAMP
           nDBFFieldSize := 8

      CASE nDBFTypeField == HB_FT_OLE
           nDBFFieldSize := 10

      CASE nDBFTypeField == HB_FT_LOGICAL
           nDBFFieldSize := 1

   ENDCASE

RETURN nDBFFieldSize

STATIC FUNCTION ADO_GETFIELDTYPE( nADOFielfType )

   LOCAL nDBFTypeField := 0

   DO CASE

		CASE nADOFielfType == adEmpty
		CASE nADOFielfType == adTinyInt
		CASE nADOFielfType == adSmallInt
		CASE nADOFielfType == adInteger
         nDBFTypeField := HB_FT_INTEGER

		CASE nADOFielfType == adBigInt
		CASE nADOFielfType == adUnsignedTinyInt
		CASE nADOFielfType == adUnsignedSmallInt
		CASE nADOFielfType == adUnsignedInt
		CASE nADOFielfType == adUnsignedBigInt
		CASE nADOFielfType == adSingle
		CASE nADOFielfType == adDouble
		CASE nADOFielfType == adCurrency
		CASE nADOFielfType == adDecimal
		CASE nADOFielfType == adNumeric
		CASE nADOFielfType == adBoolean
         nDBFTypeField := HB_FT_LOGICAL

		CASE nADOFielfType == adError
		CASE nADOFielfType == adUserDefined
		CASE nADOFielfType == adVariant
		CASE nADOFielfType == adIDispatch
		CASE nADOFielfType == adIUnknown
		CASE nADOFielfType == adGUID
		CASE nADOFielfType == adDate
         nDBFTypeField := HB_FT_DATETIME

		CASE nADOFielfType == adDBDate
         nDBFTypeField := HB_FT_DATETIME
		CASE nADOFielfType == adDBTime
		CASE nADOFielfType == adDBTimeStamp
         nDBFTypeField := HB_FT_TIMESTAMP
		CASE nADOFielfType == adBSTR
		CASE nADOFielfType == adChar
         nDBFTypeField := HB_FT_STRING

      CASE nADOFielfType == adVarChar
         nDBFTypeField := HB_FT_STRING

		CASE nADOFielfType == adLongVarChar
         nDBFTypeField := HB_FT_STRING

		CASE nADOFielfType == adWChar
         nDBFTypeField := HB_FT_STRING

		CASE nADOFielfType == adVarWChar
         nDBFTypeField := HB_FT_STRING

		CASE nADOFielfType == adLongVarWChar
         nDBFTypeField := HB_FT_STRING

		CASE nADOFielfType == adBinary
         nDBFTypeField := HB_FT_OLE
		CASE nADOFielfType == adVarBinary
         nDBFTypeField := HB_FT_OLE
		CASE nADOFielfType == adLongVarBinary
         nDBFTypeField := HB_FT_OLE
		CASE nADOFielfType == adChapter
		CASE nADOFielfType == adFileTime
		CASE nADOFielfType == adPropVariant
		CASE nADOFielfType == adVarNumeric
      // case nADOFielfType == adArray

   ENDCASE

RETURN nDBFTypeField

function HB_AdoSetTable( cTableName )

   s_cTableName = cTableName

return nil

function HB_AdoSetEngine( cEngine )

   s_cEngine = cEngine

return nil

function HB_AdoSetServer( cServer )

   s_cServer = cServer

return nil

function HB_AdoSetUser( cUser )

   s_cUserName = cUser

return nil

function HB_AdoSetPassword( cPassword )

   s_cPassword = cPassword

return nil

function HB_AdoSetQuery( cQuery )

   DEFAULT cQuery TO "SELECT * FROM "

   s_cQuery = cQuery

return nil

function HB_AdoSetLocateFor( cLocateFor )

   USRRDD_AREADATA( Select() )[ WA_LOCATEFOR ] = cLocateFor

return nil

static function SQLTranslate( cExpr )

  if Left( cExpr, 1 ) == '"' .and. Right( cExpr, 1 ) == '"'
     cExpr = SubStr( cExpr, 2, Len( cExpr ) - 2 )
  endif

  cExpr = StrTran( cExpr, '""', "" )
  cExpr = StrTran( cExpr, '"', "'" )
  cExpr = StrTran( cExpr, "''", "'" )
  cExpr = StrTran( cExpr, "==", "=" )
  cExpr = StrTran( cExpr, ".and.", "AND" )
  cExpr = StrTran( cExpr, ".or.", "OR" )
  cExpr = StrTran( cExpr, ".AND.", "AND" )
  cExpr = StrTran( cExpr, ".OR.", "OR" )

return cExpr

function HB_AdoRddGetConnection( nWA )

   DEFAULT nWA TO Select()

return USRRDD_AREADATA( nWA )[ WA_CONNECTION ]

function HB_AdoRddGetCatalog( nWA )

   DEFAULT nWA TO Select()

return USRRDD_AREADATA( nWA )[ WA_CATALOG ]

function HB_AdoRddGetRecordSet( nWA )

   local aWAData

   DEFAULT nWA TO Select()

   aWAData = USRRDD_AREADATA( nWA )

return If( aWAData != nil, aWAData[ WA_RECORDSET ], nil )