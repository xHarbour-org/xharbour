#include "webSite.ch"

request DBFCDX,dbffpt

STATIC soINI


// to test this:
//
// TestCGI > c:\test.htm
//
// C:\Start iexplore c:\test.htm
//


PROC Test1(P1,P2,P3,P4,P5)

LOCAL nH := 0, n := 0
LOCAL oHtm
LOCAL cBuffer := ""
LOCAL i, oCgi, lIsPost := .F.
LOCAL aFirst  := {}
LOCAL aSecond := {}
LOCAL cText   := ""
LOCAL oEd, oCh, oSub, oBut, oImg, oFrm, oSel

SET DATE BRITISH

IF "POST" $ UPPER(GETENV("REQUEST_METHOD"))
   lIsPost := .T.
      oCgi :=TCGI():New()
ENDIF


soIni := hb_readini("website.ini")

DEFINE CGI ;
       TITLE "Web Server variables & a DBF Browser" ;
       BGIMAGE ( COMMON_BGIMAGE ) ;
       BGCOLOR ( COMMON_BGCOLOR ) ;
       REFRESH 15 ;
       REFRESHURL "/cgi-bin/testcgi.exe" ;
       OF oHtm

oHtm:SetTextColor(CLR_BLACK)
oHtm:Setcenter( .f. )


oHtm:qout( "" )
oHtm:qout( "This page will be refreshed (reloaded) every 15 seconds" )
oHtm:qout( "The following paragraphs are colored via STYLE" )
oHtm:qout( "" )
oHtm:hLine( 3, 50 )
oHtm:qout( "" )
oHtm:paragraph( .T.,, "color='blue';background-color='lightblue';background-image='/images/buttons/user_green.gif'" )
oHtm:qqout( memoread("c:\config.sys") )
oHtm:Paragraph(.F.)
oHtm:paragraph( .T.,, "color='white';background-color='red';background-image='/images/buttons/user_green.gif'" )
oHtm:qqout( memoread("c:\config.sys") )
oHtm:Paragraph(.F.)

oHtm:qout( "" )
aFirst := hb_atokens( cBuffer, "&" )

FOR i=1 TO LEN( aFirst )
    AADD( aSecond, hb_atokens( aFirst[i], "=" ) )
NEXT

SAY "Web Server variables & a DBF Browser" ;
FONT "courier" ;
SIZE 3 ;
TYPE "<B><I>" ;
OF oHtm

//oHtm:writeLN( "Web Server variables & a DBF Browser" )

IF lIsPost
   oHtm:QOut("")
   oHtm:defineTable( 2,, 80,, "#9196A0" )  //CLR_LIGHT_GRAY )
   oHtm:TableHead( "CGI Variables", "black",,,,"white")
   oHtm:TableHead( "CGI Results", "black",,,,"white")
   FOR i=1 TO LEN( oCGI:aQueryFields )

       oHtm:newTableRow()

           oHtm:newTableCell()
             oHTM:QOut( oCGi:aQueryFields[i,1] )
           oHtm:EndTableCell()

           oHtm:newTableCell()
             oHTM:QOut( HTMLDecodeUrl(oCgi:aQueryFields[i,2]) )
             cText += HTMLDecodeUrl(oCgi:aQueryFields[i,2])
           oHtm:EndTableCell()

       oHtm:EndTableRow()

       oHtm:newTableRow()

           oHtm:newTableCell()
             oHTM:QOut( oCGi:aQueryFields[i,1] )
           oHtm:EndTableCell()

          oHtm:newTableCell()
             oHTM:QOut( oCgi:aQueryFields[i,2] )
          oHtm:EndTableCell()

       oHtm:EndTableRow()

   NEXT

   oHtm:newTableRow()

       oHtm:newTableCell()
         oHTM:QOut( "IVar cEdit1" )
       oHtm:EndTableCell()

      oHtm:newTableCell()
         oHTM:QOut( oCgi:cEdit1 )
      oHtm:EndTableCell()

   oHtm:EndTableRow()

   oHtm:newTableRow()

       oHtm:newTableCell()
         oHTM:QOut( "IVar cEdit2" )
       oHtm:EndTableCell()

      oHtm:newTableCell()
         oHTM:QOut( oCgi:cEdit2 )
      oHtm:EndTableCell()

   oHtm:EndTableRow()

   MEMOWRIT("var.txt", cText)

   oHtm:newTableRow()
   oHtm:newTableCell()
   oHtm:EndTableCell()
   oHtm:newTableCell()
   oHtm:EndTableCell()
   oHtm:endTableRow()
   oHtm:EndTable()
   oHtm:QOut("")
ENDIF


//ÄÄÄÄ[ FORM ]ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ

SET FONT "Courier" SIZE 4 OF ohtm

DEFINE FORM oFrm ;
       NAME "MyForm";
       FRAME ;
       CAPTION "Test Form"

CONTROL EDIT NAME "cEdit1" ;
       VALUE "This is a test..." ;
       MAXCHARS 100 ;
       SIZE     80 ;
       PICTURE (greek2Html("€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—")) ;
       CAPTION "My Edit1" ;
       IN oFrm

CONTROL EDIT NAME "cEdit2" ;
       VALUE "This is a test..." ;
       MAXCHARS 100 ;
       SIZE     80 ;
       PICTURE (greek2Html("€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—")) ;
       CAPTION "My Edit2" ;
       IN oFrm

CONTROL SELECT OF oSel NAME "Sel1" ;
       SIZE     1 ;
       CAPTION "Selection" ;
       IN oFrm

DEFINE OPTION "Test1" OF oSel
DEFINE OPTION "Test2" OF oSel
DEFINE OPTION "Test3" OF oSel SELECTED
DEFINE OPTION "Test5" OF oSel

LINE BREAK IN oFrm

CONTROL EDIT NAME "cEdit3" ;
       VALUE "This is a test..." ;
       MAXCHARS 100 ;
       SIZE     80 ;
       PICTURE "€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—" ;
       IN oFrm

CONTROL EDIT NAME "cEdit4" ;
       VALUE "This is a test..." ;
       MAXCHARS 100 ;
       SIZE     80 ;
       PICTURE "(XXX)-(XXX)-XXXXXXXXXXXXXXXXXXXXXXXX" ;
       IN oFrm

LINE BREAK IN oFrm
LINE IN oFrm
LINE BREAK IN oFrm

CONTROL CHECKBOX NAME "cEdit5" ;
       VALUE "This is a test..." ;
       CHECKED ;
       IN oFrm

LINE BREAK IN oFrm

CONTROL SUBMIT NAME cSubmit VALUE "   Ok    " IN oFrm

ACTIVATE oFrm


oHtm:QOut("")
oHtm:QOut("")
oHtm:defineTable( 2,, 80,, "#9196A0" )
oHtm:TableHead( "Server Environment", "black",,,,"white")
oHtm:TableHead( "", "black",,,,"white")
oHtm:newTableRow()  ; oHtm:newTableCell()
oHTM:QOut( "SERVER_SOFTWARE") ; oHtm:EndTableCell() ; oHtm:newTableCell()
oHTM:QOut( "-"+GETENV("SERVER_SOFTWARE") )
oHtm:newTableRow()  ; oHtm:newTableCell()
oHTM:QOut( "SERVER_NAME") ; oHtm:EndTableCell() ; oHtm:newTableCell()
oHTM:QOut( "-"+GETENV("SERVER_NAME") )
oHtm:newTableRow()  ; oHtm:newTableCell()
oHTM:QOut( "SERVER_PORT") ; oHtm:EndTableCell() ; oHtm:newTableCell()
oHTM:QOut( "-"+GETENV("SERVER_PORT") )
oHtm:newTableRow()  ; oHtm:newTableCell()
oHTM:QOut( "REQUEST_METHOD") ; oHtm:EndTableCell() ; oHtm:newTableCell()
oHTM:QOut( "-"+GETENV("REQUEST_METHOD") )
oHtm:newTableRow()  ; oHtm:newTableCell()
oHTM:QOut( "QUERY_STRING")   ; oHtm:EndTableCell() ; oHtm:newTableCell()
oHTM:QOut( "-"+GETENV("QUERY_STRING") )
oHtm:newTableRow()  ; oHtm:newTableCell()
oHTM:QOut( "REMOTE_HOST")    ; oHtm:EndTableCell() ; oHtm:newTableCell()
oHTM:QOut( "-"+GETENV("REMOTE_HOST") )
oHtm:newTableRow()  ; oHtm:newTableCell()
oHTM:QOut( "REMOTE_ADDR")    ; oHtm:EndTableCell() ; oHtm:newTableCell()
oHTM:QOut( "-"+GETENV("REMOTE_ADDR") )
oHtm:newTableRow()  ; oHtm:newTableCell()
oHTM:QOut( "CONTENT_TYPE")   ; oHtm:EndTableCell() ; oHtm:newTableCell()
oHTM:QOut( "-"+GETENV("CONTENT_TYPE") )
oHtm:newTableRow()  ; oHtm:newTableCell()
oHTM:QOut( "CONTENT_LENGTH") ; oHtm:EndTableCell() ; oHtm:newTableCell()
oHTM:QOut( "-"+GETENV("CONTENT_LENGTH") )
oHtm:newTableRow()  ; oHtm:newTableCell()
oHTM:QOut( "PATH_INFO")      ; oHtm:EndTableCell() ; oHtm:newTableCell()
oHTM:QOut( "-"+GETENV("PATH_INFO") )
oHtm:newTableRow()  ; oHtm:newTableCell()
oHTM:QOut( "PATH_TRANSLATED") ; oHtm:EndTableCell() ; oHtm:newTableCell()
oHTM:QOut( "-"+GETENV("PATH_TRANSLATED") )
oHtm:newTableRow()  ; oHtm:newTableCell()
oHTM:QOut( "SCRIPT_NAME")     ; oHtm:EndTableCell() ; oHtm:newTableCell()
oHTM:QOut( "-"+GETENV("SCRIPT_NAME") )
oHtm:newTableRow()  ; oHtm:newTableCell()
oHTM:QOut( "CMD Line" )       ; oHtm:EndTableCell() ; oHtm:newTableCell()
oHTM:QOut( "-"+any2str(p1)+" - "+ any2str(p2)+" - "+ any2str(p3)+" - "+ any2str(p4) )

oHtm:EndTableCell() ; oHtm:EndTableRow() ; oHtm:EndTable()
oHtm:QOut("")

SET DATE BRITISH



//
//     
//  Your database here...
//
     
rddSetDefault("DBFCDX")
//USE "F:\X000\DATA\Fixing" SHARED NEW
//USE _Fixing SHARED NEW
USE Fixing SHARED NEW

IF NETERR()
   oHtm:QOut("Error Opening Database...")
   oHtm:CgiClose()
   RETURN
ENDIF

IF !file( "FIXING.CDX" )
   INDEX ON FIXING->CUR TO FIXING.IDX
ELSE
   SET INDEX TO Fixing
ENDIF

fixing->( ordSetFOCUS(1) ) ; fixing->( dbGoTop() )

oHtm:PutHeading("Fixing "+DTOC(Date()), 2)
oHtm:defineTable( 3,, 80,,CLR_LIGHT_GRAY )


oHtm:setFont(,.T.,.T., .F. )
oHtm:TableHead( "Currency" )
oHtm:TableHead( "Fixing" )
oHtm:TableHead( "%" )

WHILE !Fixing->( EOF() )
IF n == 0
     oHtm:newTableRow("lightyellow")  //CLR_LIGHT_BLUE)
     n := 1
ELSE
     oHtm:newTableRow("#9196A0")
     n := 0
ENDIF

oHtm:newTableCell() ; oHtm:QQOut( Fixing->CUR )
                      oHtm:endTableCell()
oHtm:newTableCell() ; oHtm:QQOut( TRANSFORM(Fixing->FIXING,"9999.99999") )
                      oHtm:endTableCell()
oHtm:newTableCell() ; oHtm:sayColor( TRANSFORM(Fixing->LAST_PCENT,"9999.99999"),;
                                     IF(Fixing->LAST_PCENT < 0, "red", "black"))
                      oHtm:endTableCell()
oHtm:endTableRow()
fixing->( dbSkip())
ENDDO

oHtm:endTable()

oHtm:QQOut("</font>")

oHtm:CGIclose()

RETURN


function greek2Html(c)
return "@!"

function iniget(c,a,x)
Local xSect
local xret
xSect  := soini[c]
cRet :=  xSect[a]
if empty(xret)
xret :=c
endif
return xret

