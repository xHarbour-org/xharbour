#xcommand VIEW  <x> [ <y> ]  =>     OutputDebugString( IF(Empty(ProcName( 1 ) ),"",Trim( ProcName( 1 ) ) + "(" + LTrim(Str( ProcLine( 1 ) ) ) + ")->" ) + IF(Empty(ProcName( 0 ) ),"",Trim( ProcName( 0 ) ) + "(" + LTrim(Str( ProcLine( 0 ) ) ) + ")" ) + " : " + <x>   + " -> [" + ValType(<y>) + "] " + v(<y>) ) ; TraceLog( <x>   + " -> [" + ValType(<y>) + "] " + v(<y>) )
#xcommand VIEW  <x>          =>     OutputDebugString( IF(Empty(ProcName( 1 ) ),"",Trim( ProcName( 1 ) ) + "(" + LTrim(Str( ProcLine( 1 ) ) ) + ")->" ) + IF(Empty(ProcName( 0 ) ),"",Trim( ProcName( 0 ) ) + "(" + LTrim(Str( ProcLine( 0 ) ) ) + ")" ) + " : " + <"x"> + " -> [" + ValType(<x>) + "] " + v(<x>) ) ; TraceLog( <"x"> + " -> [" + ValType(<x>) + "] " + v(<x>) )
#xcommand VIEW2 <x>          =>     OutputDebugString( IF(Empty(ProcName( 1 ) ),"",Trim( ProcName( 1 ) ) + "(" + LTrim(Str( ProcLine( 1 ) ) ) + ")->" ) + IF(Empty(ProcName( 0 ) ),"",Trim( ProcName( 0 ) ) + "(" + LTrim(Str( ProcLine( 0 ) ) ) + ")" ) + " : " + <x> )                                            ; TraceLog( <x> )

PROCEDURE Main
   LOCAL oSQL, cTmp
   
   SetMode(0, 0)
/*
   TRY
      oSQL:=GetActiveObject( "HTTPData" )
      TraceLog( "GetActiveObject( 'HTTPData' )" + " was OK")
   CATCH

      TRY
         oSQL:= CreateObject( "HTTPData" )
         TraceLog( "CreateObject( 'HTTPData' )" + " was OK")
      CATCH
         TraceLog("ERROR! HTTPData not avialable. [" + Ole2TxtError()+ "]")
         MessageBox(, "HTTPData not avialable. [" + Ole2TxtError()+ "]", "Error" )
         RETURN
      END
   END

*/

   oSQL:=HTTPData()

   oSQL:Use("http://data.bekz.net/data.asp",; // URL
            "mysql.bekz.be",;                 // Server
            "",;                              // Port
            "winfakt_v2",;                    // Database
            "wfLogin",;                       // Username
            "16wf23pW",;                      // Password
            "wf_Serienummers",;               // Database
            "ID")                             // cRecNo
                  
/*                  
   VIEW oSQL:lError   
   VIEW oSQL:cURL
   VIEW oSQL:cServer
   VIEW oSQL:cPort
   VIEW oSQL:cDatabase
   VIEW oSQL:cUser
   VIEW oSQL:cPassword
   VIEW oSQL:cTable
   VIEW oSQL:cCondition
   VIEW oSQL:cRecNo
*/  

   IF oSQL:lError
   
      VIEW "Error on HTTPData:Use()"
   
   ELSE

      VIEW2 "Connected to wf_Serienummers"
      
//      cTmp := oSQL:AppendBlank()
//      VIEW cTmp

      oSQL:Seek("Serienr=41084")
      
      MessageBox(, If(oSQL:Found(),".T.",".F") )
      
      MessageBox(,"nRecno = " + oSQL:nRecNo + Chr(10) + "RecNo() = " + oSQL:RecNo(), "Record number")
      
      MessageBox(, oSQL:FieldGet("Benaming")  ,"Benaming" ) 
      MessageBox(, oSQL:FieldGet("SerienrHD") ,"SerienrHD" ) 
      
   ENDIF

   
   oSQL:Close()
   
   MessageBox(,"End","HTTPData Test")

RETURN





#include "HTTPData_Class.prg"




//==================================================================================================
//==================================================================================================
//==================================================================================================
//==================================================================================================

Function V(x,lRTrim,lCR,nDecimals)
   LOCAL y:=ValType(x),cRetu:=""
   DO CASE
      CASE y=="C" ; RETU If(lRTrim=NIL,x,RTrim(x))+If(lCR=NIL,"",Chr(13)+Chr(10))
      CASE y=="M" ; RETU If(lRTrim=NIL,x,RTrim(x))+If(lCR=NIL,"",Chr(13)+Chr(10))
      CASE y=="N" ; RETU If(Empty(x),"",Tx(x,.f.,If(nDecimals=NIL,2,nDecimals)))
      CASE y=="D" ; RETU If(Empty(x),"",DtoC(x))
      CASE y=="A" ; RETU (AEval(x,{|a|cRetu+=RTrim(V(a))+","}),SubStr(cRetu,1,Len(cRetu)-1))
      CASE y=="L" ; RETU If(x," Ja ","Neen")
      CASE y=="U" ; RETU "_NIL_"
      CASE y=="B" ; RETU "_BLOCK_" //V({Eval(x)})
      CASE y=="O" ; RETU "_OBJECT_"
      OTHER       ; RETU "_?_"
   ENDCASE
RETURN x


//==================================================================================================


Function Tx(n,lTransform,nMax)
   LOCAL f

   IF nMax=NIL 
      nMax:=11
   ENDIF
 
   IF Round(n,0)==n
      IF lTransform=NIL .OR. lTransform 
         RETURN LTrim(Transform(n,"999 999 999"))
      ENDIF
      RETURN LTrim(Str(n,12,0))
   ENDIF
   FOR f=0 TO nMax-1
      IF Round(n,f)==Round(n,10)
         EXIT
      ENDIF
   NEXT

   IF lTransform=NIL .OR. lTransform
      RETURN LTrim(Transform(n,"999 999 999."+Replicate("9",f)))
   ENDIF

RETURN LTrim(Transform(n,"999999999."+Replicate("9",f))) 