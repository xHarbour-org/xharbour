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
   LOCAL cURL, oConn

   TraceLog(cField)
   
   cURL := ::cURL + "?server=" + ToHTMLHex(::cServer) + "&port=" + ToHTMLHex(::cPort) + "&database=" + ToHTMLHex(::cDatabase) + "&user=" + ToHTMLHex(::cUser) + "&password=" + ToHTMLHex(::cPassword)
   cURL := cURL + "&action=" + ToHTMLHex("fieldget")
   cURL := cURL + "&table=" + ToHTMLHex(::cTable)
   cURL := cURL + "&field=" + ToHTMLHex(cField)
   cURL := cURL + "&condition=" + ToHTMLHex(::cCondition)
   
   TraceLog(cURL)

   oConn := TipClient():New( TURL():New(cURL) )
   oConn:nConnTimeout := ::nTimeOut

   IF oConn:Open(cURL)
      ::cRemoteResponse := oConn:Readall()
      oConn:Close()
      ::lError:=.F.
   ELSE
      ::cRemoteResponse := ""
      ::lError:=.T.
   END
   
RETURN ::cRemoteResponse





//--[ FieldPut ]------------------------------------
METHOD FieldPut(cField, cValue) CLASS HTTPData
   LOCAL cURL, oConn, cTempString := "", i := 1, cConcat := "0"
   
   TraceLog(cField, cValue)

   IF VALTYPE(cValue) == "L"
      IF cValue
         cValue := "1"
      ELSE
         cValue := "0"
      ENDIF
   END

   WHILE i <= LEN(cValue)
   
      IF i + 50 < LEN(cValue)
         cTempString := SUBSTR(cValue, i, 50)
      ELSE
         cTempString := SUBSTR(cValue, i)
      ENDIF

      cURL := ::cURL + "?server=" + ToHTMLHex(::cServer) + "&port=" + ToHTMLHex(::cPort) + "&database=" + ToHTMLHex(::cDatabase) + "&user=" + ToHTMLHex(::cUser) + "&password=" + ToHTMLHex(::cPassword)
      cURL := cURL + "&action=" + ToHTMLHex("fieldput")
      cURL := cURL + "&table=" + ToHTMLHex(::cTable)
      cURL := cURL + "&field=" + ToHTMLHex(cField)
      cURL := cURL + "&value=" + ToHTMLHex(cTempString)
      cURL := cURL + "&condition=" + ToHTMLHex(::cCondition)
      cURL := cURL + "&concat=" + ToHTMLHex(cConcat)
      
      TraceLog(cURL)

      IF i == 1
         cConcat := "1"
      ENDIF

      oConn := TipClient():New( TURL():New(cURL) )
      oConn:nConnTimeout := ::nTimeOut

      IF oConn:Open(cURL)
         ::cRemoteResponse := oConn:Readall()
         oConn:Close()
         ::lError:=.F.
      ELSE
         ::lError:=.T.
         ::cRemoteResponse := ""
      ENDIF

      i := i + 50
      
   ENDDO
   
RETURN ::cRemoteResponse





//--[ AppendBlank ]------------------------------------
METHOD AppendBlank() CLASS HTTPData
   LOCAL oConn
   
   cURL := ::cURL + "?server=" + ToHTMLHex(::cServer) + "&port=" + ToHTMLHex(::cPort) + "&database=" + ToHTMLHex(::cDatabase) + "&user=" + ToHTMLHex(::cUser) + "&password=" + ToHTMLHex(::cPassword)
   cURL := cURL + "&action=" + ToHTMLHex("appendblank")
   cURL := cURL + "&table=" + ToHTMLHex(::cTable)
   cURL := cURL + "&id=" + ToHTMLHex(::cRecNo)

   TraceLog(cURL)
   
   oConn := TipClient():New( TURL():New(cURL) )
   oConn:nConnTimeout := ::nTimeOut
   
   IF oConn:Open(cURL)
      ::cRemoteResponse := oConn:Readall()
      ::GoTo(VAL(::cRemoteResponse))
      
      TraceLog(::cRemoteResponse)
      oConn:Close()
      ::lError:=.F.

   ELSE
      ::cRemoteResponse := ""
      TraceLog("Failed to open "+cURL)
      ::lError:=.T.
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