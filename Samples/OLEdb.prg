#include "ado.ch"

GLOBAL oCon

// *** MUST define proper values !!!
#define USER_NAME
#define PASSWORD
#define DATABASE

PROCEDURE Main( cFile, cUser, cPassword )

   IF PCount() == 0
      Alert( "Please pass <ACCESS .mdb File> [,<cUser>, <cPassword>] at command line." )
      RETURN
   ENDIF
   
   oCon := CreateObject( "ADODB.Connection" )
   
   MakeAccessConnection( oCon, cFile, cUSer, cPassword )
   //MakeDbaseConnection( oCon, cFile )
   
RETURN

EXIT PROCEDURE CloseApp()

   CloseConnection( oCon )
   oCon := NIL

RETURN

// =================================================
FUNCTION MakeDbaseConnection( oConnection, cDatabase )

   LOCAL lRet      // AS LOGICAL
   LOCAL cConStrg  // AS STRING
   LOCAL oError    // AS OBJECT

   cConStrg := "Provider=VFPOLEDB.1;Data Source=" + cDatabase

   TRY
      oConnection:CursorLocation := adUseClient
      oConnection:Open( cConStrg )
      lRet := .T.
   CATCH oError
      Alert(oError:Description)
   END

RETURN lRet

// =================================================
FUNCTION MakeExcelConnection( oConnection, cDatabase )

    LOCAL lRet // AS LOGICAL
    LOCAL cConStrg // AS STRING

    TRY
       cConStrg := "Provider=Microsoft.Jet.OLEDB.4.0;" +;
                   "Data Source=" + cDatabase + ";" +;
                   "Extended Properties='Excel 8.0;HDR=YES'"

       oConnection:CursorLocation := adUseClient
       oConnection:Open( cConStrg )

       lRet := .T.
    CATCH
       lRet := .F.
    END

RETURN lRet

//================================================================================
FUNCTION MakeAccessConnection( oConnection, cDataFile, cUser, cPassword, lMessage )

   LOCAL oErr      // AS OBJECT
   LOCAL lRet      // AS LOGICAL
   LOCAL cConStrg  //AS STRING
   LOCAL RecordSet
   
   IIF( Empty( cPassword ), cPassword := "''", NIL )
   IIF( Empty( cUser ), cUser := "''", NIL )
   IIF( lMessage == NIL, lMessage := .T., NIL )

   TRY
      cConStrg := "Provider=Microsoft.Jet.OLEDB.4.0;" +;
                  "Jet OLEDB:Database Password=" + cPassword + ";" +;
                  "Data Source=" + cDataFile + ";" +;
                  "Persist Security Info=True;"

      oConnection:CursorLocation := adUseClient
      oConnection:Open(cConStrg)

      RecordSet := CreateObject( "ADODB.Recordset" )
   
      RecordSet:Open( "Names", oConnection, 0, 1, 2 )
   
      Do While ! RecordSet:Eof()
        Alert( RecordSet:Fields[ "First" ]:Value )
        RecordSet:MoveNext()
      ENDDO
   
      RecordSet:Close()
      RecordSet := NIL

      lRet := .T.
   CATCH oErr
      IF lMessage
         Alert( "Error: " + oErr:Operation + " -> " + oErr:Description )
      ENDIF

      lRet := .F.
   END

RETURN lRet

//=================================================================================
FUNCTION MakeOracleConnection( oConnection, cServerName, cUser, cPassword, lMessage )

   LOCAL oErr  // AS OBJECT
   LOCAL lRet   // AS LOGICAL
   LOCAL cConStrg  //AS STRING

   IIF( Empty( cPassword ), cPassword := "''", NIL )
   IIF( Empty( cUser ), cUser := "''", NIL )
   IIF( lMessage == NIL, lMessage := .T., NIL )

   TRY
      cConStrg := "Provider=MSDAORA;Data Source=" + cServerName + ;
                  ";User ID=" + cUser + "; Password=" + cPassword + ";"

      oConnection:CursorLocation := adUseClient
      oConnection:Open(cConStrg)

      lRet := .T.
   CATCH oErr
      IF lMessage
         Alert( "Error: " + oErr:Operation + " -> " + oErr:Description )
      ENDIF

      lRet := .F.
   END

RETURN lRet

//===========================================================================================
FUNCTION MakeMySQLConnection( oConnection, cServerName, cDatabase, cUser, cPassword, lMessage )

   LOCAL oErr // AS OBJECT
   LOCAL lRet //AS LOGICAL
   LOCAL cConStrg //AS STRING

    IIF( cServerName == NIL, cServerName := "localhost", NIL )
    IIF( Empty(cPassword), cPassword := "''", NIL )
    IIF( Empty(cUser), cUser := "''", NIL )
    IIF( lMessage == NIL, lMessage := .T., NIL )

   TRY
      cConStrg := "Driver={MySQL ODBC 3.51 Driver}; Server=" + cServerName + ;
                  ";Port=3306; Option=3; Database=" + cDatabase + "; Uid=" + ;
                  cUser + "; Pwd=" + cPassword + ";"

      oConnection:CursorLocation := adUseClient
      oConnection:Open(cConStrg)

      lRet := .T.
   CATCH oErr
      IF lMessage
         Alert( "Error: " + oErr:Operation + " -> " + oErr:Description )
      ENDIF

      lRet := .F.
   END

RETURN lRet

//===================================
PROCEDURE CloseConnection( oConnection )

   IF oConnection != NIL
      IF oConnection:State != adStateClosed
         IF oConnection:State != adStateOpen
            oConnection:Cancel()
         ELSE
            oConnection:Close()
         ENDIF
      ENDIF
   ENDIF

RETURN

//==========================================================
PROCEDURE CreateAccessDatabase( cDatabase, cPassword, lEncrypt )

   LOCAL oCatalog // AS ADOX.Catalog

   IIF( cPassword == NIL, cPassword := "''", NIL )
   IIF( lEncrypt == NIL, lEncrypt := .F., NIL )

   oCatalog := CreateObject( "ADOX.Catalog" )

   oCatalog:Create( "Provider=Microsoft.Jet.OLEDB.4.0;" +;
                    "Data Source=" + cDatabase + ";" +;
                    "JET OLEDB:Database Password=" + cPassWord + ";" +;
                    "JET OLEDB:Engine Type=4;" +;
                    "JET OLEDB:Encrypt Database=" + IIF(lEncrypt, "TRUE", "FALSE" ) )

   oCatalog := NIL //NULL_OBJECT

RETURN