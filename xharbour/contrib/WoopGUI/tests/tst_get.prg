#include "wingdi.ch"

PROCEDURE GetTest( oWnd )
   local oDlg
   local oCal, dVar
   local cString1 := "TryThis"
   local cString2 := space(len("(999)-9999999"))
   local cString3 := space(len('!!!99-99999999/A-(999999)'))
   local cEdit1
   local ddate
   local nNum1 := 1234567
   local nNum2 := 1234567.89
   local nNum3 := -1234567.89
   LOCAL GetList := {}
   LOCAL nR := 12

   SET CENTURY ON
   SET DATE TO ITALIAN

   ddate    := DATE()

   DEFINE DIALOG oDlg ;
          MODAL ;
          AT 30, 30 SIZE 210, 200 ;
          TITLE "Example Dialog with Get System" ;
          STATUSBAR ;
          OF oWnd

   DEFINE FONT oDlg:oFont EXTENDED ;
          HEIGHT            -11 ;
          WEIGHT            FW_NORMAL          ; // 400
          OUTPUTPRECISION   OUT_STRING_PRECIS  ; // 1
          CLIPPINGPRECISION CLIP_STROKE_PRECIS ; // 2
          QUALITY           DRAFT_QUALITY      ; // 1
          PITCHANDFAMILY    34                 ;
          FACE              "Courier New"

   // oDlg:oFont := TFont():NewExtended( -11, 0, 0, 0, 400, .F., .F.,;
   //                                       .F., 0, 1, 2, 1, 34, "Courier New" )

   SET CURRENT WINDOW oDlg


   @ 10, 10 LABEL "Valid string for 1st Get is TRY" SIZE 150, NIL COLOR "RED"


   @          30,10 XGET cString1 PICTURE "@K!" COLOR "W+/B"  VALID ( WG_DebugTrace("demo - postblock", "cString1", cString1), ;
                                                                     AllTrim(cString1) == "PIPPO" .OR. AllTrim(cString1) == "TRY" )
   @  Row() + nR,10 XGET cString2 PICTURE "(999)-9999999" COLOR "b/gr+" //WHEN ( WG_DebugTrace("demo - preblock", "cString2", cString2), ;
                                                                        //       AllTrim(cString1) == "PIPPO" )
   @  Row() + nR,10 XGET cString3 PICTURE '@S15 !!!99-99999999/A-(999999)'
   @  Row() + nR,10 XGET dDate    PICTURE "@K"
   @  Row() + nR,10 XGET nNum1    PICTURE "999999999"
   @  Row() + nR,10 XGET nNum2    PICTURE "@E 999,999,999.999"
   @  Row() + nR,10 XGET nNum3    PICTURE "@E 999,999,999.999"
   //@   40,10 GET dDate    PICTURE "@K"

   @  120,10 EDITBOX cEdit1 ;
             TOOLTIP "Edit string3" ;
             STATUS "Display" ;
             COLOR "W+/B"

   //@   10,80 PUSHBUTTON "Get &Object - Edit" ;
   //          TOOLTIP "Display 1 Get Object" ;
   //          STATUS "Display" ;
   //          ACTION GetList[1]:Control:DisplayData()

   @   30,80 PUSHBUTTON "Get &Object" ;
             TOOLTIP "Display 1 Get Object" ;
             ACTION WG_DisplayData( GetList[3] )

   @   50,80 PUSHBUTTON "&Get values" ;
             TOOLTIP "Show control values";
             ACTION ShowControlValues1( oDlg ) ;
             STATUS "Display all controls with values"

   @   70,80 PUSHBUTTON "&Cancel" ;
             ID IDCANCEL

   @  140, 0 EDITBOX MULTILINE oStatus VAR dummy ;
             SIZE 200, 50 ;
             CAPTION "Event Log" ;
             READONLY ;
             VSCROLL ;
             HSCROLL ;
             FONTNAME "Courier New";
             FONTSIZE 7.5;
             TOOLTIP "Status Window"

   //WG_ApplObj():SetLogWindow( oStatus )

   DIALOG oDlg CREATE READMODAL

RETURN

STATIC FUNCTION ShowControlValues1( oWnd )
   local cString := "", n, oCtrls := oWnd:GetChildren(), o

//   aEval( oWnd:aoControls, {|o| cString += o:ClassName + " - " + o:cName + " - " + ;
//                               o:GetValueAsString() + CRLF } )

   cString += PadC( "ClassName", 30 ) + " | "
   cString += PadC( "Name", 30 )      + " | "
   cString += PadC( "Type", 10 )      + " | "
   cString += PadC( "Value", 30 )
   cString += CRLF

   FOR n := 1 TO LEN( oCtrls )
     o := oCtrls[n]
     cString += PadR( o:ClassName, 30 )             + " | "
     cString += PadR( o:cName, 30 )                 + " | "
     cString += PadC( ValType( o:GetValue() ), 10 ) + " | "
     cString += PadR( o:GetValueAsString(), 30 )
     cString += CRLF
   NEXT n

   MessageBox(, cString, "Control values" )

RETURN -1
                 