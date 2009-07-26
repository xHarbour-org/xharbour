#include "website.ch"
REQUEST HB_GT_CGI_DEFAULT
request DBFCDX

STATIC scSession  := ""
STATIC scCmd := ""
STATIC scQuery := ""
STATIC soIni


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROC MAIN( cCmd, cRecno )  // --> CMD line params. passed by server
LOCAL oCgi, aCmd, cQuery

SET DATE BRITISH
rddsetdefault("DBFCDX")
IF cCmd == NIL
   cCmd := ""
ENDIF

//IF "%" $ cCmd
TraceLog("chegou ",cCmd,cRecno)
if "%" in cCmd  .or. " " in cCmd
   cCmd := HTMLDecodeURL( cCmd )
   TraceLog(cCmd,valtoprg(aCmd))
   aCmd := HB_ATOKENS( cCmd, " ")
   cCmd := aCmd[1]
   IF LEN( aCmd ) > 1
      cRecno := aCmd[2]
   ENDIF
ENDIF
if valtype(crecno) != "U"
TraceLog(cCmd,valtoprg(aCmd),"crecno ="+cRecno)
else
TraceLog(cCmd,valtoprg(aCmd))
endif
scCmd := cCmd


IF "POST" $ UPPER(GETENV("REQUEST_METHOD"))
      oCgi   := TCGI():New()
      cQuery := UPPER(oCgi:query_String)          // just in case...
ELSE
      oCgi   := TCGI():New()
      cQuery := UPPER(GETENV("QUERY_STRING"))
ENDIF

scQuery := cQuery

soIni := hb_readini("website.ini")

//
//  simple redirection based on command line arguments.
//  Each of the following routines produces stand alone HTML/CGI output
//
//

IF "VALIDATE" $ cCmd
    validateUser( oCgi )
ELSEIF "GETUSER" $ cCmd
    getUser( oCgi )
ELSEIF "QUERY" $ cCmd
    UserQuery( oCgi )
ELSEIF "GETREC" $ cCmd
    UserGetRec( oCgi, cRecno )  // cRecno is "B"+<cRecNr>
ELSEIF "BROWSE" $ cCmd
    SimpleBrowse(oCgi)
ELSEIF "REDIRECT" $ cCmd
    Redirect(oCgi)
ELSEIF "TESTFRAME1" $ cCmd
    TestFrame1(oCgi)
ELSEIF "SPECIAL" $ cCmd
    TestSpecial(oCgi)
ELSEIF "FORMDEMO" $ cCmd
    FormDemo(oCgi)
ELSEIF "FRMDEMOSUBMIT1" $ cCmd
    FrmSubDemo(oCgi)
ELSEIF "BUTDEMO" $ cCmd
    butDemo(oCgi)
ELSEIF "INSPECTCGI" $ cCmd
    inspectCgi(oCgi)
ELSEIF "UNDERCONSTRUCTION" $ cCmd
    underConstruction(oCgi)
ELSEIF "AUTOREFRESH" $ cCmd
    JSAutoRefresh(oCgi)
ELSEIF "CLIENTPULL" $ cCmd
    clientPull(oCgi)
ELSEIF "CLPULL1" $ cCmd
    clPull1(oCgi)
ELSEIF "CLPULL2" $ cCmd
    clPull2(oCgi)
ELSEIF "GETRATES" $ cCmd
    clPull3(oCgi,cRecno)
ELSE
    BlankPage()
ENDIF

RETURN




/****
*
*     This page is never entered
*     Output is immediately redirected to microsoft.com
*     The client never sees the actual output document...
*
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE Redirect()
LOCAL oHtm
DEFINE CGI ;
       TITLE "Redirection" ;
       JAVACODE "location.href='http://www.microsoft.com';" ;
       BGCOLOR "white" ;
       OF oHtm

oHtm:WriteLN("If you are online you will be redirected to <b><i>Micro$oft</b></i>")
oHtm:WriteLN("Please wait...")
oHtm:CGIClose()
RETURN



/****
*
*     Simple table browse
*
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE SimpleBrowse(oCgi)
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm, oForm, oImg

DEFINE CGI ;
       TITLE "Browse FIXING" ;
       ALINKCOLOR "black" ;
       VLINKCOLOR "blue" ;
       LINKCOLOR "black" ;
       BGCOLOR (COMMON_BGCOLOR) ;
       BGIMAGE (COMMON_BGIMAGE) ;
       OF oHtm


oHtm:SetTextColor("black")
oHtm:Setcenter( .T. )

oHtm:write("<br>")

MARQUEE "Browse Sample" ;
   RIGHT ;
   BGCOLOR "Black" ;
   OF oHtm

oHtm:write("<br><br>")

SET Font "Verdana" SIZE 1 OF oHtm

USE FIXING NEW VIA "DBFCDX"

    HtmlBrowse( oHtm )

USE

END FONT OF oHtm
oHtm:CGIClose()
RETURN



/****
*     TestSpecial()
*
*     Test some of the special features
*
*/

PROC TestSpecial( oCgi )
LOCAL oHtm

DEFINE CGI ;
       TITLE "Test CGI page" ;
       STYLE _WHITE_BLACK_STYLE ;
       BGIMAGE (COMMON_BGIMAGE) ;
       BGCOLOR (COMMON_BGCOLOR) ;
       IMAGES  {;
                {"OKon", "/images/buttons/ok_on.gif"},   ;
                {"OKpress","/images/buttons/ok_off.gif"},   ;
                {"OKoff","/images/buttons/ok_on_a.gif"},   ;
                {"ARROWon", "/images/buttons/arrow01.gif"},   ;
                {"ARROWoff","/images/buttons/arrow02.gif"},   ;
                {"USERon", "/images/buttons/user_red.gif"}, ;
                {"USERoff","/images/buttons/user_green.gif"},    ;
                {"BROWSEon", "/images/buttons/browse_blue.gif"}, ;
                {"BROWSEoff","/images/buttons/browse_green.gif"}    ;
               } ;
       OF oHtm

START FONT "Verdana" SIZE 2 OF oHtm

oHtm:WriteLN("<b>Marquee</b>")

MARQUEE "Marquee Sample (Right)" ;
   RIGHT ;
   BGCOLOR "Blue" ;
   OF oHtm

MARQUEE "Marquee Sample (Left)" ;
   LEFT ;
   BGCOLOR "red" ;
   OF oHtm

MARQUEE "Marquee Sample (Alternate)" ;
   ALTERNATE ;
   BGCOLOR "black" ;
   OF oHtm


oHtm:HLine()

oHtm:WriteLN("<b>Images with rollovers (click for action)</b>")

   // --> This is a simple Image URL link
   // --> you have to use the NAME clause for this to work

   IMAGE "/images/buttons/ok_on.gif" ;
   ONMOUSEOUT "imageOn('OK')" ;
   ONMOUSEOVER "imageOff('OK')" ;
   ONCLICK "imagePress('OK');";
   ALT "OK Button" ;
   NAME "OK" ;
   BORDER 0 ;
   OF oHtm

oHtm:Write(htmlSpace(10))
   IMAGE "/images/buttons/arrow01.gif" ;
   URL '' ;
   ONMOUSEOVER "imageOff('ARROW')" ;
   ONMOUSEOUT "imageOn('ARROW')" ;
   ONCLICK "history.back();" ;
   ALT "Go Back" ;
   NAME "ARROW" ;
   BORDER 0 ;
   OF oHtm

oHtm:Write(htmlSpace(10))
   IMAGE "/images/buttons/user_red.gif" ;
   URL '/cgi-bin/userform.exe?GETUSER' ;
   ONMOUSEOVER "imageOff('USER')" ;
   ONMOUSEOUT "imageOn('USER')" ;
   ONCLICK "location.href='/cgi-bin/userform.exe?GETUSER';" ;
   ALT "Go to Users" ;
   NAME "USER" ;
   BORDER 0 ;
   OF oHtm

oHtm:Write(htmlSpace(10))
   IMAGE "/images/buttons/browse_blue.gif" ;
   URL '/cgi-bin/userform.exe?GETUSER' ;
   ONMOUSEOVER "imageOff('BROWSE')" ;
   ONMOUSEOUT "imageOn('BROWSE')" ;
   ONCLICK "location.href='/cgi-bin/userform.exe?GETUSER';" ;
   ALT "Go to Browse" ;
   NAME "BROWSE" ;
   BORDER 0 ;
   OF oHtm


oHtm:HLine()
oHtm:WriteLN("<b>Counters</b>")
COUNTER NUMBER 0001234567890              ;
        FOLDER "/images/counters/counter0/" ;
        WIDTH 30 ;
        OF oHtm

COUNTER NUMBER 0001234567890              ;
        FOLDER "/images/counters/counter1/" ;
        WIDTH 40 ;
        OF oHtm

COUNTER NUMBER 0001234567890              ;
        FOLDER "/images/counters/counter2/" ;
        WIDTH 50 ;
        OF oHtm

COUNTER NUMBER 0001234567890              ;
        FOLDER "/images/counters/counter3/" ;
        WIDTH 60 ;
        OF oHtm

COUNTER NUMBER 0001234567890              ;
        FOLDER "/images/counters/counter4/" ;
        COLOR "white" ;
        WIDTH 70 ;
        OF oHtm
                        
oHtm:HLine()
oHtm:writeLN("<B>Menu like links using Styles (may not work under Netscape Navigator)</b>")

START FONT "Courier" SIZE 2 OF oHtm
oHtm:putBreak()
oHtm:putTextURL( htmlPadR("<b>1</b>. Menus with style definitions",70),"javascript:alert('onClick event')")
oHtm:putBreak()
oHtm:putTextURL(htmlPadR("<b>2</b>. Use styles to define Achoice like Menus",70),"javascript:alert('onClick event')")
oHtm:putBreak()
oHtm:putTextURL(htmlPadR("<b>3</b>. See the header STYLE clause",70),"javascript:alert('onClick event')")
oHtm:putBreak()
oHtm:putTextURL(htmlPadR("<b>4</b>. Click here for an action",70) ,"javascript:alert('onClick event')")
oHtm:putBreak()
END FONT  OF oHtm


oHtm:HLine()
oHtm:writeLN("<b>Inline <i>action</i> Buttons. This one uses the <b><i><big>STYLE</b></i></big> tag</b> ")

PUSH BUTTON NAME "testButton" ;
     CAPTION "  Click_for_an_Alert()_box  " ;
     ONCLICK "javascript:alert('onClick event')" ;
     STYLE ( "color='blue';background-color='lightblue';background-image='/images/buttons/user_green.gif'" )  ;
     OF oHtm

oHtm:HLine()
oHtm:writeLN("<b>Javascript Windows</b>")
oHtm:writeLN("")
oHtm:writeLN("It must be allready open...")
jWinTest()


oHtm:HLine()
oHtm:writeLN("<b>Inline Javascript</b>")
htmljscmd(, "document.write('Hello There...')" )

END FONT OF oHtm

oHtm:cgiClose()
RETURN



//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROC JWinTest()
LOCAL oWin := JWindow():New("test", "", "testWin", 200, 200, 200, 200 )
oWin:resizable := .F.
owin:bgColor   := "red"
owin:bgImage   := COMMON_BGIMAGE
oWin:put()
oWin:begin()
oWin:Write("<b><U>Javascript</b> Window </U><br> This is a window with red color and <br> a background Image...<br><br>Hello There !!!<br>")
oWin:end()
RETURN



/****
*
*     Client Pull
*
*
*
*/


PROCEDURE clientPull( oCgi )
LOCAL oFrame

NEW FRAMEPAGE ;
     TITLE "MyFrames1" ;
     OF oFrame

FRAMESET ;
     ROWS "10%", "20%", "70%" ;
     OF oFrame

          FRAME Name "Frame01" ;
                URL "/cgi-bin/userform.exe?CLPULL1";
                NORESIZE ;
                NOBORDER ;
                OF oFrame

          FRAME NAME "Frame02" ;
                URL '/cgi-bin/userform.Exe?CLPULL2' ;
                NORESIZE ;
                NOBORDER ;
                OF oFrame

          FRAME NAME "Frame03" ;
                URL '/cgi-bin/userform.Exe?CLPULL2' ;
                NORESIZE ;
                NOBORDER ;
                OF oFrame

oFrame:endSet()

oFrame:end()

RETURN



PROCEDURE clPull1( oCgi )
LOCAL cStr := " "
LOCAL oHTm

DEFINE CGI ;
       TITLE "Test Client Pull" ;
       BGCOLOR "black" ;
       OF oHtm

//       REFRESH 40 ;
//       REFRESHURL "/cgi-bin/userform.exe?CLPULL1" ;
START FONT "Verdana" SIZE 1 OF oHtm

USE Fixing New
WHILE !EOF()
IF Fixing->GRP == "A" //.OR. Fixing->GRP == "B" //.OR. Fixing->GRP == "C"
cStr += " - "
cStr += "<U><b>"+FIXING->CUR +"</U></b>: "
cStr += TRANSFORM(FIXING->FIXING,"999.9999")
ENDIF
Fixing->( dbSkip() )
ENDDO
CLOSE Fixing

MARQUEE ( "<b>Time</b>: "+LEFT(TIME(),5) + cStr ) ;
   FONT "Verdana" ;
   FONTSIZE 2 ;
   LEFT ;
   BGCOLOR "Black" ;
   LOOP 1 ;
   ONMSOVER "this.style.cursor='hand';this.stop()" ;
   ONMSOUT this.start() ;
   ONFINISH "window.location.reload();" ;
   OF oHtm

END FONT OF oHtm

oHtm:CgiClose()

RETURN



PROC ClPull2(oCgi)
LOCAL i, oHtm
LOCAL aFixing := {}
DEFINE CGI   ;
       TITLE "Client Pull 2" ;
       STYLE _WHITE_BLACK_STYLE ;
       BGIMAGE ( COMMON_BGIMAGE );
       OF oHtm

oHtm:writeLN("")

oHtm:writeLN("Client Pull Demo")

USE Fixing New
WHILE !EOF()
IF Fixing->GRP == "A" .OR. Fixing->GRP == "B" //.OR. Fixing->GRP == "C"
AADD( aFixing, { FIXING->CUR, ;
                 TRANSFORM(FIXING->FIXING,"999.9999"),;
                 fixing->last_pcent } ;
    )
ENDIF
Fixing->( dbSkip() )
ENDDO
//CLOSE Fixing

oHtm:setCenter( .T. )

START MARQUEE ;
   FONT "Verdana" ;
   FONTSIZE 2 ;
   LEFT ;
   BGCOLOR "WHITE" ;
   LOOP 1 ;
   ONMSOVER "this.style.cursor='hand';this.stop()" ;
   ONMSOUT this.start() ;
   ONFINISH "window.location.reload();" ;
   OF oHtm

DEFINE TABLE ;
       COLS ( LEN(aFixing) ) ;
       BORDER 0 ;
       COLORFORE "black" ;
       COLORBG "white"   ;
       RCOLS ;
       WIDTH 100 ;
       OF oHtm

START FONT "Verdana" OF oHtm
oHtm:newTableRow()
FOR i=1 TO LEN( aFixing )
    oHtm:newTableCell("center",,,,,,,10)
    //oHtm:sayColor( aFixing[i,1], "Black" )
    oHtm:putTextURL( "<b>"+aFixing[i,1]+"</b>", ;
                     "/cgi-bin/userform.exe?GETRATES "+aFixing[i,1],;
                     "location.href='/cgi-bin/userform.exe?GETRATES "+aFixing[i,1]+"';",;
                     "status='Get Rates for "+aFixing[i,1]+"';", "status='';",;
                     "top.RightTop.frame03" )
    oHtm:endTableCell()
NEXT
oHtm:endTableRow()

oHtm:newTableRow()
FOR i=1 TO LEN( aFixing )
    oHtm:newTableCell("center",,,,,,,10)
    IF aFixing[i,3] > 0
    oHtm:putImage("/images/buttons/u.gif", 0 )
    oHtm:sayColor( aFixing[i,2], "green" )
    elseif aFixing[i,3] < 0
    oHtm:putImage("/images/buttons/d.gif", 0 )
    oHtm:sayColor( aFixing[i,2], "red" )
    else
    oHtm:putImage("/images/buttons/u.gif", 0 )
    oHtm:sayColor( aFixing[i,2], "blue" )
    endif
    oHtm:endTableCell()
NEXT
oHtm:endTableRow()

END MARQUEE OF oHtm

oHtm:setCenter( .F. )
oHtm:cgiClose()
RETURN



//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE clPull3( oCgi, cCur )
LOCAL i, oHtm
DEFINE CGI   ;
       TITLE "Client Pull 2" ;
       STYLE _WHITE_BLACK_STYLE ;
       BGIMAGE ( COMMON_BGIMAGE );
       OF oHtm

oHtm:writeLN("")

oHtm:writeLN("Client Pull Demo")
oHtm:cgiClose()
RETURN


/****
*
*     Test Frames...
*     --------------
*
*     This example generates a document (test.htm) and then displays
*     it in a new frameset window. Each frame displays another instance
*     of the document.
*
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE TestFrame1(oCgi)
LOCAL oFrame
LOCAL i,j,oHtm
LOCAL aDir := DIRECTORY( "C:\*.*" )



DEFINE HTML ;
       FILE ( WEB_ROOT+"test.htm" ) ;   //  NOTE !!!
       TITLE "Test HTML page" ;
       OF oHtm

oHtm:setCenter(.T.)

DEFINE FONT "verdana" BOLD SIZE 3 of ohtm

DEFINE TABLE ;
       BORDER 1 ;
       COLS 6 ;
       WIDTH 80 ;
       COLORBG LIGHTGREY ;
       COLORFORE "black" ;
       3D ;
       OF oHtm

oHtm:newtablerow()
DEFINE CELL COLOR "yellow" FONTCOLOR "black";
       COLSPAN 6 ;
       CENTER ;
       OF oHtm
oHtm:qout("<c>colspan 1</c>" )
oHtm:EndtableCell() ; oHtm:EndtableRow()
oHtm:newtablerow()

DEFINE CELL COLOR "red" FONTCOLOR "white";
       COLSPAN 3 ;
       OF oHtm
oHtm:qout("colspan 3" )
oHtm:EndtableCell()

DEFINE CELL COLOR "green" FONTCOLOR "white";
       COLSPAN 3 ;
       OF oHtm
oHtm:qout("colspan 3" )
oHtm:EndtableCell() ; oHtm:Endtablerow()

FOR i=1 TO LEN( aDir )

    oHtm:newtablerow()
     for j=1 to 4
        DEFINE CELL WIDTH 20 ;
        OF oHtm
        oHtm:qout( any2str(aDir[i,j]) )
        oHtm:EndtableCell()
     next j

     DEFINE CELL FONTCOLOR "blue" SIZE 2 WIDTH 40 ;
     OF oHtm
     oHtm:qout( any2str(aDir[i,1]) )
     oHtm:EndtableCell()

     DEFINE CELL FONTCOLOR "white" ;
     WIDTH 20 ;
     OF oHtm
     oHtm:qout( any2str(aDir[i,2]) )
     oHtm:EndtableCell() ; oHtm:Endtablerow()
NEXT i

oHtm:newtablerow()

TABLE CELL COLOR "black" FONTCOLOR "yellow";
      COLSPAN 6 ;
      RIGHT ;
      OF oHtm

oHtm:qout(memoread("c:\config.sys" ))
oHtm:EndtableCell()
oHtm:EndtableRow()

oHtm:endTable()
oHtm:close()



NEW FRAMEPAGE ;
     TITLE "MyFrames1" ;
     OF oFrame

FRAMESET ;
     COLS "30%", "70%" ;
     OF oFrame

     FRAMESET ;
          ROWS "80%", "20%" ;
          OF oFrame

          FRAME Name "Frame01" ;
                URL "/test.htm" ;
                OF oFrame

          FRAME Name "Frame02" ;
                URL "" ;
                OF oFrame

     oFrame:endSet()

     FRAMESET ;
          ROWS "80%", "20%" ;
          OF oFrame

          FRAME Name "Frame03" ;
                URL "/test.htm";
                NORESIZE ;
                OF oFrame

          FRAME NAME "Frame04" ;
                URL '/test.htm' ;
                NORESIZE ;
                OF oFrame


     oFrame:endSet()

oFrame:endSet()

oFrame:end()

RETURN




//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROC FormDemo(oCgi)
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm
LOCAL oForm, oForm1,oUser, oPass, oLbx

LOCAL oText1, oText2
LOCAL oBut1, oBut2
LOCAL oSub, oReset

DEFINE CGI ;
       TITLE "Forms Demo" ;
       BGIMAGE (COMMON_BGIMAGE) ;
       BGCOLOR (COMMON_BGCOLOR) ;
       OF oHtm


Start Font "Courier" SIZE 2 OF oHtm

oHtm:write("<br><br>")
oHtm:write("<P ALIGN='right'>")

DEFINE FORM oForm1 NAME "TestForm" ;
       FRAME ;
       COLOR "WHITE" ;
       CAPTION [<IMG SRC='/images/email/email1.gif' onClick='history.back()' height=40>]+htmlspace(2)+"Mail form" ;
       CAPFONTCOLOR "red" ;
       CAPIMAGE "/images/sidebars/sbk.gif" ;
       BGIMAGE "/images/sidebars/sb1.gif" ;
       WIDTH 60 ;
       ACTION "mailto:luiz@xharbour.com.br"  // --> NOTE !!!

DEFINE EDIT oText1 NAME "text1" ;
       VALUE "" ;
       MAXCHARS 30 ;
       CAPTION "<b>"+(htmlPadL("EditField1 : ",20))+"</b>"  ;
       IN oForm1

LINE BREAK IN oForm1

DEFINE EDIT oText2 NAME "text2" ;
       VALUE "" ;
       MAXCHARS 30 ;
       CAPTION "<b>"+(htmlPadL("EditField2 : ",20))+"</b>"  ;
       IN oForm1

LINE BREAK IN oForm1
LINE BREAK IN oForm1

SPACE 25 IN oForm1
DEFINE SUBMIT oSub NAME "cSubmit" VALUE "    Submit    " IN oForm1


ACTIVATE oForm1


oForm1 := NIL

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴

oHtm:write("</P>")
oHtm:write("<P ALIGN='center'>")

DEFINE FORM oForm NAME "TestForm2" ;
       FRAME ;
       CAPTION [<button onClick="history.back()"><IMG SRC='/images/buttons/no1_1.jpg' onClick='history.back()' height=16></button>]+htmlspace(2)+"Processing Form" ;
       CAPIMAGE "/images/sidebars/sbh.jpg" ;
       CAPFONTCOLOR "blue" ;
       COLOR "WHITE" ;
       BGIMAGE "/images/sidebars/sb7.gif" ;
       WIDTH 60 ;
       ACTION "/cgi-bin/userForm.exe?FRMDEMOSUBMIT1"  // --> NOTE !!!

CONTROL EDIT ;
        NAME "UserName" ;
        VALUE "" ;
        MAXCHARS 30 ;
        CAPTION (htmlPadL("User Name : ",20) ) ;
        IN oForm

LINE BREAK IN oForm

CONTROL PASSWORD  ;
        NAME "UserPass" ;
        VALUE "" ;
        MAXCHARS 30 ;
        CAPTION (htmlPadl(+"Password : ",20)) ;
        IN oForm

LINE BREAK IN oForm
LINE BREAK IN oForm

CONTROL CHECKBOX  ;
        NAME "CheckBox1" ;
        CHECKED ;
        CAPTION (htmlPadl(+"Checkbox : ",20)) ;
        IN oForm

LINE BREAK IN oForm
LINE BREAK IN oForm

CONTROL SELECT  OF oLbx ;
        NAME "Select1" ;
        CAPTION (htmlPadl(+"ListBox : ",20)) ;
        IN oForm

ADD OPTION "Option 1" OF oLbx
ADD OPTION "Option 2" OF oLbx
ADD OPTION "Option 3" OF oLbx
ADD OPTION "Selected" SELECTED OF oLbx
ADD OPTION "Option 6" OF oLbx
ADD OPTION "Option 7" OF oLbx

LINE BREAK IN oForm
LINE BREAK IN oForm

CONTROL EDIT ;
        NAME "dis1" ;
        VALUE "Disabled Field..." ;
        MAXCHARS 30 ;
        CAPTION (htmlPadL("Disabled : ",20) ) ;
        DISABLED ;
        IN oForm

CONTROL HIDDEN ;
        NAME "Hidden1" ;
        VALUE "Hidden Field..." ;
        IN oForm

LINE BREAK IN oForm
LINE BREAK IN oForm

SPACE 25 IN oForm

CONTROL SUBMIT NAME "cSubmit" VALUE "    Submit    " IN oForm


ACTIVATE oForm

oHtm:write("</P>")
oHtm:write("<P ALIGN='left'>")

oForm := NIL


oHtm:cgiClose()


RETURN




//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROC FrmSubDemo( oCgi )
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm

DEFINE CGI ;
       TITLE "test Form Results..." ;
       BGCOLOR (COMMON_BGCOLOR) ;
       BGIMAGE (COMMON_BGIMAGE) ;
       OF oHtm

START FONT "Verdana" SIZE 3 OF oHtm

oHtm:WriteLN("<u><b><i>Form Submission results...</b></i></u>")

oHtm:writeLN("<b>UserName :</b>"+ oCgi:userName )
oHtm:writeLN("<b>UserPass :</b>"+ oCgi:userPass )
oHtm:writeLN("<b>CheckBox1:</b>"+ oCgi:checkBox1 )
oHtm:writeLN("<b>Select1:</b>"  + oCgi:select1 )
oHtm:writeLN("<b>Disabled:</b> NOT Returned..." )
oHtm:writeLN("<b>Hidden1:</b>"  + oCgi:Hidden1 )

oHtm:cgiClose()
RETURN


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROC ButDemo(oCgi)
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm

DEFINE CGI ;
       TITLE "Buttons Demo" ;
       BGIMAGE (COMMON_BGIMAGE) ;
       BGCOLOR (COMMON_BGCOLOR) ;
       OF oHtm


oHtm:write("<BR><BR>")
oHtm:writeln("<u>IE4-5 <i>BUTTONS</i> - Not visible in Netscape Navigator</u>")
oHtm:write("<BR><BR>")
oHtm:write("<BUTTON> <IMG SRC='/JList/bookslib.jpg'></BUTTON>")
oHtm:write(htmlSpace(5) )
oHtm:write("<BUTTON> <IMG SRC='/images/buttons/ok_1.bmp'></BUTTON>")
oHtm:write(htmlSpace(5) )
oHtm:write("<BR><BR>")
oHtm:write("<BUTTON> <IMG SRC='/images/buttons/yes1_1.gif' height=32> Confirm</BUTTON>")
oHtm:write(htmlSpace(5) )
oHtm:write("<BUTTON> <IMG SRC='/images/buttons/yes1_1.gif' height=18> Confirm</BUTTON>")
oHtm:write("<BR><BR>")
oHtm:write("<BUTTON> <IMG SRC='/images/buttons/no1_1.gif' height=32> Cancel</BUTTON>")
oHtm:write(htmlSpace(5) )
oHtm:write("<BUTTON> <IMG SRC='/images/buttons/no1_1.gif' height=20> Cancel</BUTTON>")

oHtm:write("<BR><BR><BR>")
oHtm:write("A table in a button !!!")
oHtm:write("<BR><BR><BR>")

BUTTON ONCLICK "alert('Incredible buttons !!!')" ;
       OF oHtm

DEFINE TABLE COLS 4 3d OF oHtm
oHtm:newTableRow()
oHtm:newTableCell() ; oHtm:write("A") ; oHtm:endTableCell()
oHtm:newTableCell() ; oHtm:write("B") ; oHtm:endTableCell()
oHtm:newTableCell() ; oHtm:write("C") ; oHtm:endTableCell()
oHtm:newTableCell() ; oHtm:putImage("/images/buttons/search.gif",0) ; oHtm:endTableCell()
oHtm:endTableRow()
oHtm:newTableRow()
oHtm:newTableCell() ; oHtm:write("D") ; oHtm:endTableCell()
oHtm:newTableCell() ; oHtm:write("E") ; oHtm:endTableCell()
oHtm:newTableCell() ; oHtm:write("F") ; oHtm:endTableCell()
oHtm:newTableCell() ; oHtm:putImage("/images/buttons/close1.jpg",0) ; oHtm:endTableCell()
oHtm:endTableRow()
oHtm:newTableRow()
oHtm:newTableCell() ; oHtm:write("G") ; oHtm:endTableCell()
oHtm:newTableCell() ; oHtm:write("G") ; oHtm:endTableCell()
oHtm:newTableCell() ; oHtm:write("I") ; oHtm:endTableCell()
oHtm:newTableCell() ; oHtm:putImage("/images/buttons/back.gif",0) ; oHtm:endTableCell()
oHtm:endTableRow()
oHtm:endTable()
END BUTTON OF oHtm

oHtm:write(htmlSpace(10)+"<b><------------- <u>Clickme</u> !!!</b>")

oHtm:cgiclose()
RETURN



//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROC inspectCgi(oCgi)
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm
LOCAL oForm

DEFINE CGI ;
       TITLE "CGI/Class Inspector" ;
       BGIMAGE (COMMON_BGIMAGE) ;
       BGCOLOR (COMMON_BGCOLOR) ;
       OF oHtm

START FONT "Verdana" SIZE 2 OF oHtm
oHtm:WriteLN("<b><u>The <i>::debug()</i> Method<b></u><br>")
oHtm:WriteLN("Every object in the libary contains a <i>::debug()<i> method.")
oHtm:WriteLN("It is a simple online class inspector that produces the following output:")

oCgi:debug( oCgi )

oHtm:cgiclose()
RETURN





//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE getUser( oCgi )
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm
LOCAL oForm, oUser, oPass,oSub

DEFINE CGI ;
       TITLE "User Validation" ;
       BGIMAGE (COMMON_BGIMAGE) ;
       BGCOLOR (COMMON_BGCOLOR) ;
       OF oHtm

oHtm:SetTextColor("black")
oHtm:Setcenter( .T. )

Start Font "Courier" SIZE 2 OF oHtm

oHtm:write("<br><br>")

DEFINE FORM oForm NAME "UserForm" ;
       FRAME ;
       CAPTION [<button onClick="history.back()"><IMG SRC='/images/buttons/no1_1.jpg' onClick='history.back()' height=16></button>]+htmlspace(2)+"User login" ;
       CAPFONTCOLOR "white" ;
       BGIMAGE "/images/sidebars/sb1.gif" ;
       WIDTH 60 ;
       ACTION "/cgi-bin/userform.exe?VALIDATE"  // --> NOTE !!!

DEFINE EDIT oUser NAME "UserName" ;
       VALUE "" ;
       MAXCHARS 30 ;
       CAPTION (htmlPadL("User Name : ",20) ) ;
       IN oForm

LINE BREAK IN oForm

DEFINE PASSWORD oPass NAME "UserPass" ;
       VALUE "" ;
       MAXCHARS 30 ;
       CAPTION (htmlPadl(+"Password : ",20)) ;
       IN oForm

LINE BREAK IN oForm
LINE BREAK IN oForm

SPACE 25 IN oForm
DEFINE SUBMIT oSub NAME "cSubmit" VALUE "    Submit    " IN oForm


ACTIVATE oForm

oHtm:QOut("<BR><BR>")
oHtm:HLine( 3, 80 )
Start Font "Verdana" SIZE 3 OF oHtm
oHtm:QOut("<BR><BR>User Name  is <b>Manos</b><BR>User Pass is <b>Manos</b>")

END FONT OF oHtm


oHtm:CGIClose()

RETURN


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROC ValidateUser( oCgi )
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL cUserName, cPassword
LOCAL lValid := .F.
tracelog(valtoprg(ocgi))
IF !EMPTY( oCgi:Field( "UserName" ) )
   cUserName := oCgi:username
   cPassword := oCgi:UserPass
ELSE
   logginError(oCgi)  //BlankPage( oCgi )
   RETURN
ENDIF

USE USERS SHARED NEW
IF NETERR()
   NetError(oCgi)
   RETURN
ENDIF
tracelog( "Arquivo aberto")
if !file("users.cdx")     
INDEX ON UPPER(UserName) TAG UserName
endif
USERS->( ordSetFocus("UserName") )
IF USERS->( dbSeek( UPPER(cUserName) ) )
   IF UPPER(USERS->PASSWORD) = UPPER( cPassword )
      lValid := .T.
      IF USERS->( dbRlock() )
         USERS->TIMES += 1
         //USERS->IP    := oCgi:ipAddress
         USERS->( dbCommit() )
         USERS->( dbRUnlock() )
      ENDIF

   ELSE
      lValid := .F.
   ENDIF
ELSE
   lValid := .F.
   LogginError( oCgi )
   RETURN
ENDIF

IF lValid
   MainAccess(USERS->TIMES, oCgi, cUserName)
ELSE
   LogginError( oCgi )
ENDIF

RETURN


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE MainAccess(nTimes, oCgi, cuserName)
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm, oForm, oSel, oDateStart, oDateEnd, oButt1, oSub

//IF !ValidateUser( oCgi )
//  logginError( oCgi )
//  RETURN
//ENDIF

DEFINE CGI ;
       TITLE "Main Access" ;
       BGIMAGE "/images/sidebars/sba.jpg" ;
       OF oHtm

oHtm:SetTextColor("Blue")

oHtm:SetCenter( .T. )

Start Font "Verdana" SIZE 3 OF oHtm

oHtm:WriteLN("<B>Congratulations </B>  "+cUserName +"<BR>")
oHtm:WriteLN("You have accessed this system ( <I>"+NTRIM( nTimes )+" times</I> )" )

Start Font "Verdana" SIZE 4 OF oHtm
oHtm:defineTable(4,,90,,)
oHtm:TableHead( "Your User Name",,,,3,,,)
oHtm:TableHead( "Your Password",,,,3,,,)
oHtm:TableHead( "Your Access Times",,,,3,,,)
oHtm:TableHead( "Your Ip Address",,,,3,,,)
oHtm:newTableRow()

    oHtm:newTableCell()
      oHTM:QOut( USERS->USERNAME )
    oHtm:EndTableCell()

    oHtm:newTableCell()
      oHTM:QOut( USERS->PASSWORD )
    oHtm:EndTableCell()

    oHtm:newTableCell()
      oHTM:QOut( NTRIM( USERS->TIMES ) )
    oHtm:EndTableCell()

    oHtm:newTableCell()
      oHTM:QOut( oCgi:remote_addr )
    oHtm:EndTableCell()

oHtm:EndTableRow()
oHtm:EndTable()
oHtm:write("<BR><BR>")

// Query Form

Start Font "Courier" SIZE 3 OF oHtm

DEFINE FORM oForm NAME "QueryForm" ;
       FRAME ;
       CAPTION "User Query" ;
       BGIMAGE "/images/sidebars/sbd.jpg" ;
       WIDTH 80 ;
       ACTION "/cgi-bin/userform.exe?QUERY"


DEFINE SELECT oSel NAME "cur" ;
       SIZE 1 ;
       CAPTION (htmlPadl("<b>Currency :</b>",25)) ;
       IN oForm

DEFINE OPTION "DEM" OF oSel
DEFINE OPTION "FRF" OF oSel
DEFINE OPTION "GBP" OF oSel
DEFINE OPTION "ITL" OF oSel
DEFINE OPTION "USD" OF oSel SELECTED

LINE BREAK IN oForm
LINE BREAK IN oForm

DEFINE EDIT oDateStart NAME "DateStart" ;
       VALUE "01/11/1999" ;
       MAXCHARS 10 ;
       CAPTION (htmlPadl("<b>Start Date :</b>",25)) ;
       IN oForm

LINE BREAK IN oForm
LINE BREAK IN oForm

DEFINE EDIT oDateEnd NAME "DateEnd" ;
       VALUE "30/11/1999" ;
       MAXCHARS 10 ;
       CAPTION (htmlPadl("<b>End Date :<b>",25)) ;
       IN oForm
       
LINE BREAK IN oForm
LINE BREAK IN oForm

space 5 IN oForm
DEFINE BUTTON oButt1 NAME "Button1" ;
       VALUE "  Press me  " ;
       ONCLICK "this.form.submit()" ;
       IN oForm

space 5 IN oForm
DEFINE BUTTON oButt1 NAME "Button2" ;
       VALUE "  << Go Back  " ;
       ONCLICK "history.back()" ;
       IN oForm

space 5 IN oForm
DEFINE SUBMIT oSub NAME "cSubmit" VALUE " Submit " IN oForm

ACTIVATE oForm

oHtm:endFont()
oHtm:endFont()

IF Valtype(oCgi) == "O"
//   oCgi:debug()
ELSE
   oHtm:writeLn( Valtype(oCgi) )
ENDIF


ohtm:cgiClose()

RETURN



/****
*
*     DBF Conditional browser
*
*     Uses values inserted in above form, loaded in an oCGI() object.
*
*     replace with your own data - pay attention to the BEGIN SEQUENCE...END...
*     check. It catches invalid user input. In this case date fields.
*
*
*/
                          
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE UserQuery( oCgi )
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm, cCur, cDateStart, cDateEnd, dDateStart, dDateEnd
LOCAL n := 0
LOCAL bOldError, oError

//USE "F:\X000\DATA\HInv02" SHARED NEW VIA "dbfcdx"
USE HInv02 SHARED NEW VIA "dbfcdx"
IF NETERR()
   netError()
   RETURN
ENDIF
IF FILE( "hInv02.cdx" )
SET INDEX TO HINV02
ELSE
INDEX ON DTOS( hInv02->DATE ) TAG D_DATE TO HInv02
ENDIF
             
bOldError := ERRORBLOCK( {|o| BREAK(o) } )
BEGIN SEQUENCE
   cCur       := oCgi:cur
   cDateStart := oCgi:dateStart
   cDateEnd   := oCgi:dateEnd
   dDateStart := CTOD( cDateStart )
   dDateEnd   := CTOD( cDateEnd )
RECOVER USING oError
   cCur       := NIL
   dDateStart := NIL
   dDateEnd   := NIL
END SEQUENCE
ERRORBLOCK( bOldError )


IF VALTYPE(cCur) == "C" .AND. ;
   VALTYPE( dDateStart ) == "D" .AND. ;
   VALTYPE( dDateEnd ) == "D"

   SET FILTER TO &("hInv02->CUR == '"+cCur+"' .AND. DTOS( hInv02->DATE ) >= '"+DTOS(dDateStart)+"' .AND. DTOS( hInv02->DATE ) <= '"+DTOS(dDateEnd)+"'")
   hInv02->( dbGoTop() )
   IF hInv02->( EOF() )
      queryError()
      RETURN
   ENDIF

   oHtm := THTML():CGINew(, "Success !!!" )
     
   oHtm:SetPageColor("white")
   oHtm:SetTextColor("black")
   oHtm:Setcenter( .T. )

   oHtm:writeLN("")
   oHtm:writeLN("<b>Press a <u>'?'</u> button to view the record data...</b>" )
   oHtm:writeLN("")

   oHtm:defineTable( 6,, 90,,CLR_LIGHT_GRAY )
                    
   oHtm:setFont(,.T.,.T., .F. )
   oHtm:TableHead( "Currency" )
   oHtm:TableHead( "X/T" )
   oHtm:TableHead( "Amount" )
   oHtm:TableHead( "Rate" )
   oHtm:TableHead( "Value" )
   oHtm:TableHead( "?" )
   oHtm:TableHead( "<>" )

   WHILE !hInv02->( EOF() )

         IF n == 0
              oHtm:newTableRow("lightyellow")  //CLR_LIGHT_BLUE)
              n := 1
         ELSE
              oHtm:newTableRow("#9196A0")
              n := 0
         ENDIF

         oHtm:newTableCell("center",,,,,,,50) ; oHtm:QQOut( "<strong>"+hInv02->CUR+"</strong>" )
                               oHtm:endTableCell()
         oHtm:newTableCell("center",,,,,,,30) ; oHtm:QQOut( hInv02->XT )
                               oHtm:endTableCell()
         oHtm:newTableCell("right",,,,,,,80) ; oHtm:QQOut( TRANSFORM(hInv02->AMT,"9,999,999,999") )
                               oHtm:endTableCell()
         oHtm:newTableCell("right",,,,,,,80) ; oHtm:QQOut( TRANSFORM(hInv02->Rate,"9,999.999999") )
                               oHtm:endTableCell()
         oHtm:newTableCell("right",,,,,,,80) ; oHtm:QQOut( TRANSFORM(hInv02->Value_GRD,"9,999,999,999") )
                               oHtm:endTableCell()
         oHtm:newTableCell("center",,,,,,,30)

         PUSH BUTTON ;
              NAME ("'B"+NTRIM(RECNO())+"'") ;
              CAPTION "' ? '" ;
              CGIAPP "'/cgi-bin/userform.exe?GETREC ' + this.name" ;
              OF oHtm
              //NAME "B"+NTRIM(RECNO()) ;

         oHtm:newTableCell("center",,,,,,,30) ; oHtm:Write("<...>")
                               oHtm:endTableCell()
         oHtm:endTableRow()
         hInv02->( dbSkip() )
   ENDDO

   oHtm:endTable()
   oHtm:cgiClose()

ELSE

   QueryError()

ENDIF

USE

RETURN

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE userGetRec( oCgi, cRecno )
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm
TRacelog(valtoprg(ocgi),crecno)
DEFINE CGI ;
       TITLE "View a Record ("+cRecno+")";
       JAVASOURCE "/autorefr.js" ;
       ONLOAD "beginrefresh();alert('\nEntering page\n')" ;
       ONUNLOAD "status='';clearTimeout('beginrefresh()');alert('\nExiting page\n')" ;
       BGIMAGE (COMMON_BGIMAGE) ;
       BGCOLOR (COMMON_BGCOLOR) ;
       OF oHtm

cRecno := SUBSTR( cRecno, 2 )
TraceLog(cRecno)
oHtm:SetTextColor("black")
oHtm:Setcenter( .T. )

USE HInv02 SHARED NEW VIA "dbfcdx"
//USE "F:\X000\DATA\HInv02" SHARED NEW VIA "dbfcdx"
IF NETERR()
   netError()
   RETURN
ENDIF
IF FILE( "hInv02.cdx" )
   set order to 1
ELSE
   INDEX ON DTOS( hInv02->DATE ) TAG D_DATE 
ENDIF
GOTO VAL( cRecno )

IF !EOF()

   oHtm:WriteLN( "This page has an autorefresh script attached to it...")
   oHtm:WriteLN( "Take a look at the status bar :)")
   oHtm:WriteLN( "")
   oHtm:WriteLN( "* * * * *")
   oHtm:WriteLN( "")
   oHtm:WriteLN( "Remote Address: "+GETENV("REMOTE_ADDR") )
   oHtm:WriteLN( "Remote User: "+GETENV("REMOTE_USER") )
   oHtm:WriteLN("-----------------------------------------------" )
   oHtm:WriteLN("RECNO "+ cRecno )
   oHtm:WriteLN( "CUR    "+hInv02->CUR )
   oHtm:WriteLN( "AMT    "+STR(hInv02->AMT) )
   oHtm:WriteLN( "RATE   "+STR(hInv02->RATE) )
   oHtm:WriteLN( "FIXING "+STR(hInv02->FIXING) )
   oHtm:WriteLN( "DATE   "+DTOC(hInv02->DATE) )

ELSE
   dbAccessError()
ENDIF

oHtm:cgiClose()

RETURN

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE JSAutoRefresh(oCgi)
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm

DEFINE CGI ;
       TITLE "Javascript Support !!!" ;
       JAVASOURCE "/autorefr.js" ;
       ONLOAD "beginrefresh()" ;
       ONUNLOAD "status='';clearTimeout('beginrefresh()')" ;
       BGIMAGE (COMMON_BGIMAGE) ;
       BGCOLOR (COMMON_BGCOLOR) ;
       OF oHtm

oHtm:SetTextColor("black")
oHtm:Setcenter( .T. )

oHtm:WriteLN("")
oHtm:WriteLN("")
oHtm:WriteLN("")
oHtm:WriteLN("")
oHtm:WriteLN("<B>T</B>his page will refresh itself in <B>30</b>seconds !!!")
oHtm:WriteLN("Take a look at the status bar...")
oHtm:WriteLN("")
oHtm:WriteLN("")
oHtm:WriteLN("")

oHtm:setCenter( .T. )
IMAGE "/images/buttons/previous.gif" ;
     ONCLICK "history.back();" ;
     ALT "go Back..." ;
     HEIGHT 6 ;
     OF oHtm
oHtm:setCenter( .F. )

oHtm:cgiClose()

RETURN

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE BlankPage( oCgi )
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm

DEFINE CGI ;
       BGIMAGE (COMMON_BGIMAGE) ;
       BGCOLOR (COMMON_BGCOLOR) ;
       TITLE "ACTION Error !!!" ;
       OF oHtm

oHtm:SetTextColor("black")
oHtm:Setcenter( .T. )

oHtm:WriteLN("")
oHtm:WriteLN("ERROR !!!")
oHtm:WriteLN("")
oHtm:WriteLN("Please specify an appropriate action...")

oHtm:cgiClose()

RETURN

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE underConstruction( oCgi )
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm

DEFINE CGI ;
       BGIMAGE (COMMON_BGIMAGE) ;
       BGCOLOR (COMMON_BGCOLOR) ;
       TITLE "Under Construction !!!" ;
       OF oHtm

oHtm:SetTextColor("black")
oHtm:Setcenter( .T. )

oHtm:WriteLN("")
oHtm:WriteLN("")
oHtm:WriteLN("")
oHtm:WriteLN("")
IMAGE "/images/uc/uc4.gif" OF oHtm

oHtm:cgiClose()

RETURN

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE dbAccessError()
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm
DEFINE CGI ;
       TITLE "DATABASE ACTION Error !!!" ;
       BGIMAGE (COMMON_BGIMAGE) ;
       BGCOLOR (COMMON_BGCOLOR) ;
       OF oHtm

oHtm:SetTextColor("black")
oHtm:Setcenter( .T. )
Start Font "Verdana" SIZE 2 OF oHtm

oHtm:WriteLN("")
oHtm:WriteLN("ERROR !!!")
oHtm:WriteLN("")
oHtm:WriteLN("There was an error processing your request.")
oHtm:WriteLN("Please specify another action...")

oHtm:cgiClose()

RETURN

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE NetError( oCgi )
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm
DEFINE CGI ;
       TITLE "Network Error !!!" ;
       BGIMAGE (COMMON_BGIMAGE) ;
       BGCOLOR (COMMON_BGCOLOR) ;
       OF oHtm

oHtm:SetTextColor("black")
oHtm:Setcenter( .T. )
Start Font "Verdana" SIZE 2 OF oHtm

oHtm:WriteLN("NETWORK ERROR !!!")
oHtm:WriteLN("")
oHtm:WriteLN("Please try later...")

oHtm:cgiClose()

RETURN


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE QueryError( oCgi )
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm
DEFINE CGI ;
       TITLE "User Query Error !!!" ;
       BGIMAGE (COMMON_BGIMAGE) ;
       BGCOLOR (COMMON_BGCOLOR) ;
       OF oHtm

oHtm:SetTextColor("black")
oHtm:Setcenter( .T. )
Start Font "Verdana" SIZE 2 OF oHtm

oHtm:WriteLN("USER QUERY ERROR !!!")
oHtm:WriteLN("")
oHtm:WriteLN("There was an error processing your request.")
oHtm:WriteLN("Please try again later...")

oHtm:cgiClose()

RETURN


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE logginError( oCGI )
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm
DEFINE CGI ;
       TITLE "LoggIn Error !!!" ;
       BGIMAGE (COMMON_BGIMAGE) ;
       BGCOLOR (COMMON_BGCOLOR) ;
       OF oHtm

oHtm:SetTextColor("black")
oHtm:Setcenter( .T. )
Start Font "Verdana" SIZE 2 OF oHtm

oHtm:WriteLN("LOGGIN ERROR !!!")
oHtm:WriteLN("<BR><BR><BR>")
oHtm:WriteLN("Invalid USER Name : "+ oCgi:Username )
oHtm:WriteLN("OR" )
oHtm:WriteLN("Invalid PASSword  : "+ oCgi:UserPass )
oHtm:WriteLN("<BR>")
oHtm:HLine(4, 80 )
oHtm:WriteLN("<BR>")
oHtm:WriteLN("Please try again...")

oHtm:cgiClose()

RETURN




/****
*
*     Determine if user is logged in
*
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
FUNCTION ValidateSession(oCgi)
LOCAL lRet := .F.
CLOSE SESSION
USE Session SHARED NEW VIA "dbfcdx"
IF FILE( "session.cdx")
   SET INDEX TO SESSION
ELSE
   USE Session EXCLUSIVE NEW VIA "dbfcdx"
   INDEX ON SESSION->IP TAG TG_IP TO SESSION.CDX
   CLOSE SESSION
   USE Session SHARED NEW VIA "dbfcdx"
   SET INDEX TO SESSION
ENDIF
Session->( dbGoTop() )
IF Session->( dbSeek( RTRIM(oCgi:IpAddress) ) )
   lRet := .T.
ELSE
   lRet := .F.
ENDIF
CLOSE SESSION
RETURN lRet



/****
*
*     Erase a user session
*
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
FUNCTION ReleaseSession(oCgi)

LOCAL lRet := .F.
USE
USE Session SHARED NEW VIA "dbfcdx"
INDEX ON SESSION->IP TAG TG_IP TO SESSION.CDX


IF FILE( "session.cdx")
   SET INDEX TO SESSION
ELSE
   USE
   USE Session EXCLUSIVE NEW VIA "dbfcdx"
   IF NETERR()
      RETURN .F.
   ENDIF
   INDEX ON SESSION->IP TAG TG_IP TO SESSION.CDX
   CLOSE SESSION
   USE Session SHARED NEW VIA "dbfcdx"
   SET INDEX TO SESSION
ENDIF


Session->( dbGoTop() )
IF Session->( dbSeek( RTRIM(oCgi:IpAddress) ) )
   Session->( dbDelete())
ENDIF
CLOSE SESSION
RETURN lRet



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

