#include "hbclass.ch"

CLASS HTTPData
   
   PUBLIC:
    DATA cURL, cServer, cPort, cDatabase, cUser, cPassword, cTable
    DATA cCondition
    DATA cRecNo
    DATA nRecNo          INIT 0
    DATA lError          INIT .F.
    DATA lFound          INIT .F.
    DATA cRemoteResponse INIT ""
    DATA nTimeOut        INIT 20000
   
    METHOD Use()
    METHOD GoTo()
    METHOD Seek()
    METHOD AppendBlank()
    METHOD FieldGet()
    METHOD FieldPut()
    METHOD Delete()
    
    METHOD RecNo()       INLINE ::nRecNo
    METHOD Found()       INLINE ::lFound
    
    METHOD RecCount()    INLINE 0
    
    METHOD Close() VIRTUAL
       
ENDCLASS


METHOD Use(cURL, cServer, cPort, cDatabase, cUser, cPassword, cTable, cRecNo) CLASS HTTPData
   LOCAL oConn

// TraceLog("In Use()")
   TraceLog(cURL, cServer, cPort, cDatabase, cUser, cPassword, cTable, cRecNo)
   
   ::cURL      := cURL
   ::cServer   := cServer
   ::cPort     := cPort
   ::cDatabase := cDatabase
   ::cUser     := cUser
   ::cPassword := cPassword
   ::cTable    := cTable
   ::cRecNo    := If(Empty(cRecNo),"RecNo",cRecNo)
   
   ::cRemoteResponse:=""
   ::lFound:=.F.

   /*
   oConn := Tipclient():New( TURL():New(cURL) )
   oConn:nConnTimeout := ::nTimeOut

   IF oConn:Open(cURL)
      ::cRemoteResponse := oConn:Readall()
      oConn:Close()
      ::lError:=.F.
      TraceLog(cURL + " opened OK!")
      TraceLog(::cRemoteResponse)
   ELSE
      ::lError:=.T.
      TraceLog("Failed to open " + cURL)
   ENDIF
   */
RETURN self




//--[ GoTo ]------------------------------------
METHOD GoTo(nRecNo) CLASS HTTPData
   LOCAL cURL, oConn

   TraceLog(nRecNo)
   
   IF ValType(nRecNo) == "N"
      ::cCondition := ::cRecNo + "=" + LTrim(Str(nRecNo))
      ::nRecNo:=nRecNo      
   ELSE
      ::Seek(nRecNo)
   ENDIF   
   
   TraceLog( ::cCondition )
   TraceLog( ::nRecNo )

RETURN NIL




//--[ Seek ]------------------------------------
METHOD Seek(cSeek) CLASS HTTPData
   LOCAL cURL, oConn

   TraceLog(cSeek)
   
   IF ValType(cSeek) == "N"
      ::GoTo(cSeek)
   ELSE
      ::cCondition := cSeek
      ::nRecNo:=::FieldGet(::cRecNo)
      ::lFound:=!Empty(::nRecNo)
   ENDIF   
   
   TraceLog( ::cCondition )
   TraceLog( ::nRecNo )
   TraceLog( ::lFound )

RETURN NIL





//--[ FieldGet ]------------------------------------
METHOD FieldGet(cField) CLASS HTTPData
   LOCAL cDBConn := "HST=" + ::cServer + ";UID=" + ::cUser + ";PWD=" + ::cPassword + ";DTB=" + ::cDatabase, nDBConn := 0
   LOCAL oSQL, cSQL, rsItems := {}

   // CREATE CONNECTION TO MYSQL DATABASE
   TRY
      nDBConn := SR_AddConnection(3, cDBConn)
   CATCH oErr
      Alert(oErr:Operation + " -> " + oErr:Description)
      nDBConn := 0
   END

   IF nDBConn == 1
      SR_SetActiveConnection(nDBConn)
      oSQL := SR_GetConnection()

      IF LEN(::cCondition) <= 0
         cSQL := "select " + cField + " as 'custfield' from " + ::cTable
      ELSE
         cSQL := "select " + cField + " as 'custfield' from " + ::cTable + " where " + ::cCondition
      END
      oSQL:exec(cSQL, , .T., @rsItems)

      IF LEN(rsItems) > 0
         ::cRemoteResponse := ""
         FOR i := 1 TO LEN(rsItems)
            IF VALTYPE(rsItems[i][1]) == "N"
               ::cRemoteResponse := ::cRemoteResponse + ALLTRIM(STR(rsItems[i][1]))
            ELSE
               ::cRemoteResponse := ::cRemoteResponse + rsItems[i][1]
            END
         NEXT
      END

      ::lError := .F.
   ELSE
      ::lError := .T.
   END   

   // CLOSE CONNECTION
   IF nDBConn == 1
      SR_EndConnection(nDBConn)
   END   
RETURN ::cRemoteResponse





//--[ FieldPut ]------------------------------------
METHOD FieldPut(cField, cValue) CLASS HTTPData
   LOCAL cDBConn := "HST=" + ::cServer + ";UID=" + ::cUser + ";PWD=" + ::cPassword + ";DTB=" + ::cDatabase, nDBConn := 0
   LOCAL oSQL, cSQL, rsItems := {}

   // CREATE CONNECTION TO MYSQL DATABASE
   TRY
      nDBConn := SR_AddConnection(3, cDBConn)
   CATCH oErr
      Alert(oErr:Operation + " -> " + oErr:Description)
      nDBConn := 0
   END

   IF nDBConn == 1
      SR_SetActiveConnection(nDBConn)
      oSQL := SR_GetConnection()

      IF VALTYPE(cValue) == "L"
         IF cValue
            cValue := "1"
         ELSE
            cValue := "0"
         END
      END

      cSQL := "update " + ::cTable + " set " + cField + " = '" + STRTRAN(cValue, "'", "''") + "' where " + ::cCondition
      oSQL:exec(cSQL, , .T., @rsItems)

      ::lError := .F.
   ELSE
      ::lError := .T.
   END   

   // CLOSE CONNECTION
   IF nDBConn == 1
      SR_EndConnection(nDBConn)
   END
RETURN ::cRemoteResponse





//--[ AppendBlank ]------------------------------------
METHOD AppendBlank() CLASS HTTPData
   LOCAL cDBConn := "HST=" + ::cServer + ";UID=" + ::cUser + ";PWD=" + ::cPassword + ";DTB=" + ::cDatabase, nDBConn := 0
   LOCAL oSQL, cSQL, rsItems := {}

   // CREATE CONNECTION TO MYSQL DATABASE
   TRY
      nDBConn := SR_AddConnection(3, cDBConn)
   CATCH oErr
      nDBConn := 0
   END

   IF nDBConn == 1
      SR_SetActiveConnection(nDBConn)
      oSQL := SR_GetConnection()

      cSQL := "insert into " + ::cTable + " values()"
      oSQL:exec(cSQL, , .T., @rsItems)
      cSQL := "select max(" + ::cRecNo + ") from " + ::cTable
      oSQL:exec(cSQL, , .T., @rsItems)

      IF LEN(rsItems) > 0
         ::cRemoteResponse := rsItems[1][1]
      END

      ::lError := .F.
   ELSE
      ::lError := .T.
   END   

   // CLOSE CONNECTION
   IF nDBConn == 1
      SR_EndConnection(nDBConn)
   END   
RETURN ::cRemoteResponse




//--[ Delete ]------------------------------------
METHOD Delete() CLASS HTTPData
   LOCAL cURL, oConn, cRemoteResponse := "", cTempString := "", i := 1
   
   TraceLog("In Delete()")

   cURL := ::cURL + "?server=" + ToHTMLHex(::cServer) + "&port=" + ToHTMLHex(::cPort) + "&database=" + ToHTMLHex(::cDatabase) + "&user=" + ToHTMLHex(::cUser) + "&password=" + ToHTMLHex(::cPassword)
   cURL := cURL + "&action=" + ToHTMLHex("delete")
   cURL := cURL + "&table=" + ToHTMLHex(::cTable)
   cURL := cURL + "&condition=" + ToHTMLHex(::cCondition)

   oConn := TipClient():New( TURL():New(cURL) )
   oConn:nConnTimeout := ::nTimeOut

   IF oConn:Open(cURL)
      cRemoteResponse := oConn:Readall()
      oConn:Close()
      ::lError:=.F.
   ELSE
      ::lError:=.T.
      cRemoteResponse := ""
   END

RETURN cRemoteResponse




 //-- CONVERT DECIMAL STRING TO HEX STRING ------------------------------------------------------------//
STATIC Function ToHTMLHex(cString)

   LOCAL cHexString := ""
   LOCAL i := 1

   FOR i := 1 TO Len(cString)
      cHexString := cHexString + "%" + StrToHex(cString[i])
   NEXT
   
RETURN cHexString 