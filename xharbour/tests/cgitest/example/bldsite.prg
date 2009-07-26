REQUEST HB_GT_CGI_DEFAULT
#include "website.ch"

request dbfcdx

STATIC scCmd      := ""
STATIC scSiteRoot := ""
STATIC scQuery    := ""

STATIC soIni

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROC Process( cCmd, cSiteRoot )  // --> CMD line params. passed by server
LOCAL oCgi, cQuery
LOCAL aCmd

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

      cSiteRoot := aCmd[2]

      IF EMPTY( cSiteRoot )
         cSiteRoot := "c:\apache2.2\htdocs"
      ENDIF
      TraceLog(cSiteroot)

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

soIni := hb_readini("website.ini")

//
//  simple redirection based on command line arguments.
//  Each of the following routines produces stand alone HTML/CGI output
//
//

IF "BUILD" $ cCmd
    buildSite()
ELSE
    BlankPage()
ENDIF

RETURN


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC PROC BuildSite( oCgi )
LOCAL oHtm
LOCAL oFrame

   IF !FILE(WEB_ROOT+"ltop.htm")
      BuildLTop()
   ENDIF
   IF !FILE(WEB_ROOT+"rtop.htm")
      BuildRTop()
   ENDIF
   IF !FILE(WEB_ROOT+"lbottom.htm")
      BuildLBottom()
   ENDIF
   IF !FILE(WEB_ROOT+"rbottom.htm")
      BuildRBottom()
   ENDIF


NEW FRAMEPAGE ;
     TITLE "HTML Lib Demo !!!" ;
     OF oFrame

FRAMESET ;
     COLS "20%", "80%" ;
     OF oFrame

     FRAMESET ;
          ROWS "90%", "10%" ;
          OF oFrame

          FRAME Name "LeftTop" ;
                URL "/JList/Menu.htm" ;  //"/ltop.htm" ;
                SCROLLING OFF ;
                OF oFrame

          FRAME Name "LeftBottom" ;
                URL "/lbottom.htm" ;
                SCROLLING OFF ;
                OF oFrame

     oFrame:endSet()

     FRAMESET ;
          ROWS "90%", "10%" ;
          OF oFrame

          FRAME Name "RightTop" ;
                URL "/rtop.htm";
                OF oFrame

          FRAME NAME "RightBottom" ;
                URL '/rbottom.htm' ;
                OF oFrame


     oFrame:endSet()

oFrame:endSet()

oFrame:end()

RETURN



//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC PROC BuildLTop()
LOCAL oHtm
DEFINE HTML ;
       FILE ( WEB_ROOT+"ltop.htm" ) ;   //  NOTE !!!
       TITLE "Left Top Page" ;
       BGIMAGE ( COMMON_BGIMAGE ) ;
       OF oHtm
oHtm:Close()
RETURN


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC PROC BuildRTop()
LOCAL oHtm
DEFINE HTML ;
       FILE ( WEB_ROOT+"rtop.htm" ) ;   //  NOTE !!!
       TITLE "Right Top Page" ;
       BGCOLOR COMMON_BGCOLOR ;
       BGIMAGE COMMON_BGIMAGE ;
       OF oHtm


START FONT "Verdana" SIZE 5 COLOR "blue" OF oHtm

oHtm:writeLN("Logo:"+COMMON_LOGO)
oHtm:writeLN("Root:"+WEB_ROOT)
oHtm:writeLN("BgClr:"+COMMON_BGCOLOR)
oHtm:writeLN("BgImg:"+COMMON_BGIMAGE)

oHtm:setCenter( .T. )
IMAGE COMMON_LOGO BORDER 1  OF oHtm
oHtm:WriteLN("") ; oHtm:WriteLN("")

oHtm:WriteLN("Welcome to <i>KAPA Change SA</i>" )

/*
INLINE FRAME NAME "testFrm1" ;
       URL "/colortab.htm"   ;
       HEIGHT 100 WIDTH 100  ;
       ALIGN "horizontal" ;
       SCROLLING ;
       OF oHtm

INLINE FRAME NAME "testFrm2" URL "/colortab.htm" ;
       HEIGHT 300 WIDTH 300 ;
       ALIGN "horizontal" ;
       OF oHtm
*/
oHtm:Close()
RETURN


//
// The eMail frame
// 

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC PROC BuildLBottom()
LOCAL oHtm
DEFINE HTML ;
       FILE ( WEB_ROOT + "lbottom.htm" ) ;   //  NOTE !!!
       TITLE "Left Bottom Page" ;
       BGCOLOR ( COMMON_BGCOLOR ) ;
       BGIMAGE ( COMMON_SIDEBAR ) ;
       OF oHtm

oHtm:setCenter( .T. )

IMAGE (COMMON_EMAIL) ;
    URL  ( "mailto:" + WEB_EMAIL + "?subject=test subject...'" ) ;
    BORDER 0 ;
    onMouseOver "status='Send me eMail...';return true;" ;
    onMouseOut  "status='';return true;" ;
    OF oHtm

/*IMAGE COMMON_EMAIL ;
    BORDER 0 ;
    onClick     "parent.location='mailto:" + (WEB_EMAIL) + "?subject=test subject...'" ;
    onMouseOver "status='Send me eMail...';return true;" ;
    onMouseOut  "status='';return true;" ;
    OF oHtm
*/
oHtm:Close()

RETURN


//
// The counter frame
//

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC PROC BuildRBottom()
LOCAL oHtm
DEFINE HTML ;
       FILE (WEB_ROOT+"rbottom.htm") ;   //  NOTE !!!
       TITLE "Right Bottom Page" ;
       BGCOLOR "black" ;
       BGIMAGE (COMMON_SIDEBAR) ;
       OF oHtm

COUNTER NUMBER (incCounter()) ;
        FOLDER (COMMON_COUNTER) ;
        DIGITS 8 ;
        WIDTH 22 ;
        OF oHtm

oHtm:Close()
RETURN


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
PROCEDURE BlankPage( oCgi )
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
LOCAL oHtm
oHtm := THTML():cgiNew(, "ACTION Error !!!" )

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

