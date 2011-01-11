#include "iegui.ch"

PROCEDURE Main()

   LOCAL cHtml, oIE, oForm, cFirstName := "John", cPhone := Space( 13 )
   LOCAL GetList := {}

   TEXT INTO cHtml

<html>
   <head>
      <title>My App</title>
   </head>

   <body>
      <form id="Sample" method="POST" action="">
         <p>Name:&nbsp;&nbsp;&nbsp;
         <input type="text" id="FirstName" size="20" dir="ltr" tabindex="1"></p>
         <p>&nbsp;</p>
         <p><input type="button" id="Ok" value="   Ok   ">&nbsp;&nbsp;&nbsp;
         <input type="button" id="Cancel" value="Cancel"></p>
      </form>
   </body>
</html>

   ENDTEXT

   DEFINE BROWSER oIE STATUSBAR

   DEFINE FORM Sample OF oIE AS oForm;
          TITLE "My HTML Application";
          BODY (cHtml)

   DEFINE BEHAVIOR HighLight;
      ON onmouseover DO oSource:style:color := "red";
      ON onmouseout DO oSource:style:color := "black"

   DEFINE BEHAVIOR Sound;
      ON onmouseover DO Tone( 320, 0.1 )

   REDEFINE GET FirstName OF oForm VAR cFirstName;
            PICTURE "@K!";
            WHEN When_FirstName();
            VALID Valid_FirstName()

   // Dynmaic creation
   DEFINE GET Phone OF oForm VAR cPhone;
          PICTURE "(999)999-9999";
          BEFORE Ok

   REDEFINE Ok OF oForm;
         ON onclick DO oIE:document:parentWindow:ShowModalDialog( "list.htm", "", "dialogHeight:400px;dialogWidth:400px;center:yes" );
         ON onmouseover DO oIE:StatusText := oIE:Path;
         BEHAVIORS HighLight, Sound

   ACTIVATE FORM oForm /*ID Sample OF oIE*/ MODAL

   CLOSE BROWSER oIE

   TraceLog( "Done" )

RETURN

FUNCTION When_FirstName( )

   TraceLog()

   IF GetActive():VarGet() = "!"
      RETURN .F.
   ENDIF

RETURN .T.

FUNCTION Valid_FirstName()

   TraceLog()

   IF Empty( GetActive():VarGet() )
      GetActive():Undo()
      RETURN .F.
   ENDIF

RETURN .T.


/*
onhelp
onclick
ondblclick
onkeypress
onkeydown
onkeyup
onmouseout
onmouseover
onmousemove
onmousedown
onmouseup
onselectstart
onfilterchange
ondragstart
onbeforeupdate
onafterupdate
onerrorupdate
onrowexit
onrowenter
ondatasetchanged
ondataavailable
ondatasetcomplete
onlosecapture
onpropertychange
onscroll
onfocus
onblur
onresize
ondrag
ondragend
ondragenter
ondragover
ondragleave
ondrop
onbeforecut
oncut
onbeforecopy
oncopy
onbeforepaste
onpaste
oncontextmenu
onrowsdelete
onrowsinserted
oncellchange
onreadystatechange
onlayoutcomplete
onpage
onmouseenter
onmouseleave
onactivate
ondeactivate
onbeforedeactivate
onbeforeactivate
onfocusin
onfocusout
onmove
oncontrolselect
onmovestart
onmoveend
onresizestart
onresizeend
onmousewheel
onsubmit
onreset
*/