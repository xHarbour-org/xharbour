#include "html.ch"
#include "default.ch"



//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROC GenError( cType )

LOCAL oCgi, cQuery

SET DATE BRITISH

IF "POST" $ UPPER(GETENV("REQUEST_METHOD"))
      oCgi   := TCGI():New()
      cQuery := UPPER(oCgi:query_String)          // just in case...
ELSE
      oCgi   := TCGI():New()
      cQuery := UPPER(GETENV("QUERY_STRING"))
ENDIF
       
IF cType == NIL
   blankPage(oCgi)
ELSEIF cType == "DATABASE"
   dbError(oCgi)
ELSEIF cType == "STRING"
   stringError(oCgi)
ENDIF

RETURN




/****
*
*     Create a Database related error
*
*/

STATIC Function dbError( oCgi )
LOCAL oHtm

oHtm := THTML():CGINew( "userForm.html", "User Validation" )


oHtm:SetPageColor("white")
oHtm:SetTextColor("black")
oHtm:Setcenter( .T. )

Start Font "Verdana" SIZE 3 OF oHtm

USE dummyDb NEW VIA "_DBFCDX"

HTMLBrowse()

END FONT "Verdana" of oHtm

oHtm:CGIClose()

RETURN NIL


/****
*
*     Create a String related error
*
*/

STATIC Function StringError( oCgi )
LOCAL oHtm

oHtm := THTML():CGINew( "userForm.html", "User Validation" )

oHtm:SetPageColor("white")
oHtm:SetTextColor("black")
oHtm:Setcenter( .T. )

Start Font "Verdana" SIZE 3 OF oHtm


oHtm:WriteLN( "Strings:" )
oHtm:WriteLN( "------------" )
oHtm:WriteLN( "1000" )
oHtm:WriteLN( "10000" )
oHtm:WriteLN( "100000" )
oHtm:WriteLN( "" )
oHtm:WriteLN( "Numbers:" )
oHtm:WriteLN( "------------" )
oHtm:WriteLN( 1000 )
oHtm:WriteLN( 10000 )
oHtm:WriteLN( 100000 )

HTMLBrowse()

END FONT "Verdana" OF oHtm

oHtm:CGIClose()
RETURN NIL



/****
*     Returns a blank page
*
*/

PROCEDURE BlankPage( oCgi )
LOCAL oHtm
oHtm := THTML():CGINew(, "ACTION Error !!!" )

oHtm:SetPageColor("white")
oHtm:SetTextColor("black")
oHtm:Setcenter( .T. )

oHtm:WriteLN("")
oHtm:WriteLN("ERROR !!!")
oHtm:WriteLN("")
oHtm:WriteLN("Please specify an appropriate action...")

oHtm:cgiClose()

RETURN

