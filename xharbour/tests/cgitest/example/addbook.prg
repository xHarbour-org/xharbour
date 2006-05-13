#include "html.ch"
#include "htmlform.ch"
#include "htmlclrs.ch"

request dbfcdx

STATIC scSession  := ""
STATIC scCmd := ""
STATIC scQuery := ""

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROC Process( cCmd, cRecno )  // --> CMD line params. passed by server
LOCAL oCgi, cQuery, aCmd

SET DATE BRITISH

IF cCmd == NIL
   cCmd := ""
ENDIF


// some web servers pass URL encoded parameters
// the following code solves this problem:

IF "%" $ cCmd
   cCmd := HTMLDecodeURL( cCmd )
   aCmd := HB_ATOKENS( cCmd, " ")
   cCmd := aCmd[1]
   IF LEN( aCmd ) > 1
      cRecno := aCmd[2]
   ENDIF
ENDIF

scCmd := cCmd


IF "POST" $ UPPER(GETENV("REQUEST_METHOD"))
      oCgi   := TCGI():New()
      cQuery := UPPER(oCgi:query_String)          // just in case...
ELSE
      oCgi   := TCGI():New()
      cQuery := UPPER(GETENV("QUERY_STRING"))
ENDIF

scQuery := cQuery

//
//  simple redirection based on command line arguments.
//  Each of the following routines produces stand alone HTML/CGI output
//
//

IF FILE ("ADDBOOK.DBF")
   USE ADDBOOK  VIA "dbfcdx"
   IF !File( "addbook.cdx" )
      INDEX ON Addbook->LName TO Addbook.cdx
   ENDIF
ENDIF

IF "BLDPAGE" $ cCmd
    BuildAddbk(oCgi)
ELSEIF "BROWSE" $ cCmd
    BrowseAddbk(oCgi)
ELSEIF "GET" $ cCmd
    getAddbk(oCgi,cRecno)
ELSEIF "EDIT" $ cCmd
    SaveData(oCgi, cRecno, .F.)
ELSEIF "APPEND" $ cCmd
    SaveData(oCgi, cRecno, .T.)
ELSE
    BlankPage()
ENDIF

CLOSE ALL

RETURN

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROC BuildAddbk(oCgi)
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm

DEFINE CGI ;
       TITLE "Right Top Page" ;
       BGIMAGE "/images/back/bg4.gif" ;
       BGCOLOR "white" ;
       OF oHtm
oHtm:SetTextColor("black")
oHtm:Setcenter( .T. )

oHtm:Write("")

INLINE FRAME NAME "FrAddbk1" ;
       SRC "/cgi-bin/addbook.exe?BROWSE" ;
       HEIGHT 270 WIDTH 700 ;
       MARGINHEIGHT 0 ;
       MARGINWIDTH 0 ;
       SCROLLING ;
       OF oHtm

oHtm:write("<br>")
oHtm:write("<br>")

INLINE FRAME NAME "FrAddbk2" ;
       HEIGHT 320 WIDTH 600 ;
       MARGINHEIGHT 0 ;
       MARGINWIDTH 0 ;
       SCROLLING ;
       OF oHtm

oHtm:cgiClose()

RETURN



//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROC BrowseAddbk(oCgi)
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm

DEFINE CGI  ;
       TITLE "Browse AddressBook" ;
       BASEURL "" ;
       BASETARGET "FrAddbk2" ;
       OF oHtm

oHtm:SetPageColor("lightYellow")  //slateblue")
oHtm:SetTextColor("black")
oHtm:Setcenter( .T. )
START FONT "Verdana" SIZE 1 OF oHtm
htmlBrowse( oHtm, ;
           "top.RightTop.FrAddbk2.location.href='/cgi-bin/addbook.exe?GET '+this.name;")


htmljsCmd( ,"top.RightTop.FrAddbk2.location.href='/cgi-bin/addbook.exe?GET B1';")

oHtm:cgiClose()

RETURN


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROC GetAddbk(oCgi, cRecno)
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm := THTML():CGINew(, "Browse AddressBook" )
LOCAL oForm
LOCAL oFNAME
LOCAL oLNAME
LOCAL oPHONE
LOCAL oFAX
LOCAL oADDRESS
LOCAL oCITY
LOCAL oZIP
LOCAL oNOTES
LOCAL oSub, oApnd, oReset, oHd, oDel

oHtm:SetPageColor("slateblue")
oHtm:SetTextColor("black")
oHtm:Setcenter( .T. )

START FONT "Courier" SIZE 1 OF oHtm

if cRecno == NIL
oHtm:WriteLN( any2str( oCgi:Query_String ) )
oHtm:WriteLN( scCmd )
oHtm:WriteLN( "RECNO = "+any2str( cRecno ) )
//oCgi:debug()
cRecno := "1"
GOTO VAL( cRecno )
ELSE
cRecno := SUBSTR( cRecno, 2 )
GOTO VAL( cRecno )
ENDIF

DEFINE FORM oForm NAME "addbkForm" ;
       FRAME ;
       CAPTION "Edit AddressBook" ;
       COLOR WHITE ;
       ACTION "/cgi-bin/addbook.exe?EDIT%20"+"B"+cRecno  // --> NOTE !!!

oForm:captionColor := "blue"
oForm:capFontColor := "white"
oForm:bgImage := "/images/sidebars/sb3.gif"

DEFINE HIDDEN oHd   NAME "recno"   ;  // hide Recno() in a control
       VALUE ("B"+cRecno) ;           // for retrieval
       IN oForm

DEFINE EDIT oFName   NAME "FName"   ;
       CAPTION (HtmlPadL("FName:",10) )   ;
       VALUE (RTRIM(Addbook->FName))  ;
       MAXCHARS 30 IN oForm

LINE BREAK IN oForm

DEFINE EDIT oLName   NAME "LName"   ;
       CAPTION (HtmlPadL("LName:",10))   ;
       VALUE (RTRIM(Addbook->LName))  ;
       MAXCHARS 30 IN oForm
LINE BREAK IN oForm
DEFINE EDIT oPhone   NAME "Phone"   ;
       CAPTION (HtmlPadL("Phone:",10))   ;
       VALUE (RTRIM(Addbook->Phone))  ;
       MAXCHARS 30 IN oForm
DEFINE EDIT oFax     NAME "Fax"     ;
       CAPTION ( HtmlPADL("Fax:", 10 ) )     ;
       VALUE (RTRIM(Addbook->Fax))  ;
       MAXCHARS 30 IN oForm
LINE BREAK IN oForm
DEFINE EDIT oAddress NAME "Address" ;
       CAPTION ( HtmlPADL("Address:", 10 ) ) ;
       VALUE (RTRIM(Addbook->Address))  ;
       MAXCHARS 30 IN oForm
LINE BREAK IN oForm
DEFINE EDIT oCity    NAME "City"    ;
       CAPTION (HtmlPadL("City:",10))    ;
       VALUE (RTRIM(Addbook->City))  ;
       MAXCHARS 30 IN oForm
LINE BREAK IN oForm
DEFINE EDIT oZip     NAME "Zip"     ;
       CAPTION (HtmlPadL("Zip:",10))     ;
       VALUE (RTRIM(Addbook->Zip))  ;
       MAXCHARS 30 IN oForm
LINE BREAK IN oForm
DEFINE EDIT oNotes   NAME "Notes"   ;
       CAPTION (HtmlPadL("Notes:",10))   ;
       VALUE (RTRIM(Addbook->Notes))  ;
       MAXCHARS 50 IN oForm

LINE BREAK IN oForm

DEFINE SUBMIT oSub ;
       VALUE "  Save  " ;
       NAME  "Save" ;
       IN oForm

SPACE 5 IN oForm

DEFINE BUTTON oApnd ;
       VALUE "  New  " ;
       ONCLICK "document.addbkForm.action='/cgi-bin/addbook.exe?APPEND%20"+"B"+cRecno+"';document.addbkForm.submit();top.RightTop.FrAddbk2.location.reload();" ;
       NAME  "New" ;
       IN oForm

SPACE 5 IN oForm

DEFINE RESET oReset ;
       VALUE "  Cancel  " ;
       NAME  "Reset" ;
       IN oForm

SPACE 5 IN oForm

DEFINE BUTTON oDel ;
       VALUE "  Delete  " ;
       ONCLICK "alert('*DELETE* not yet implemented  ! ! !\nDo it yourself :)');" ;
       NAME  "Delete" ;
       IN oForm

ACTIVATE oForm

oHtm:endFont()

oHtm:cgiClose()

RETURN



//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC Function SaveData( oCgi, cRecno, lAppend )
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm

if valtype( oCgi ) != "O"
   blankPage()
   RETURN nil
endif

DEFINE CGI  ;
       TITLE "Confirmation" ;
       ONLOAD "top.RightTop.FrAddbk1.location.reload();" ;
       OF oHtm

oHtm:SetPageColor("white")
oHtm:SetTextColor("black")
oHtm:Setcenter( .T. )

START FONT "Courier" SIZE 2 OF oHtm

if cRecno == NIL
   oHtm:WriteLN( "ERROR !!!" )
   oHtm:WriteLN( any2str( oCgi:Query_String ) )
   oHtm:WriteLN( scCmd )
   oHtm:WriteLN( "RECNO = "+any2str( cRecno ) )
   oHtm:cgiClose()
   RETURN NIL
ELSE
   cRecno := SUBSTR( cRecno, 2 )
   GOTO VAL( cRecno )
ENDIF

IF lAppend
   Addbook->( dbAppend() )
ENDIF

Addbook->FNAME    := oCgi:FNAME
Addbook->LNAME    := oCgi:LNAME
Addbook->PHONE    := oCgi:PHONE
Addbook->FAX      := oCgi:FAX
Addbook->ADDRESS  := oCgi:ADDRESS
Addbook->CITY     := oCgi:CITY
Addbook->ZIP      := oCgi:ZIP
Addbook->NOTES    := oCgi:NOTES
Addbook->( dbCommit() )

oHtm:WriteLN( "Success !!!" )
IF lAppend
oHtm:WriteLN( "New record Saved..." )
ELSE
oHtm:WriteLN( "Changes Saved..." )
ENDIF

oHtm:cgiClose()

RETURN Nil


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE BlankPage( oCgi )
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
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



//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC FUNCTION IncCounter()
LOCAL n := 0

IF FILE("Counter.dat")
   n := VAL(MEMOREAD("counter.dat"))
ELSE
   n := 0
ENDIF
MEMOWRIT( "counter.dat", STR( n+1 ) )
RETURN n


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC FUNCTION GetCounter()
LOCAL n := 0

IF FILE("Counter.dat")
   n := VAL(MEMOREAD("counter.dat"))
ELSE
   incCounter()
ENDIF
RETURN n


function iniget(c,a,x)
Local xSect
local cret

xSect  := soini[c]
cRet :=  xSect[a]
if empty(cret)
cret :=x
endif
tracelog(cret)
return cret

