
#include "woopgui.ch"
#include "common.ch"
#include "windows.ch"
//#include "demo.ch"

//#define WG_DEBUG


function Main   //( hInstance, hprevInstance, cCmdLine, nCmdShow )

   local oApp, oWnd1
   local oMenuBar, oContextMenu, oStatusBar
   local oNewFrmClass, oStatus, dummy, oPnl

   SET DATE TO ITALIAN
   SET CENTURY ON

   DEFINE APPLICATION oApp NAME "Sample" TITLE "Application title" ICONFILE "small.ico" ONQUIT Appl_Quit()
   //oApp := tApplication():New( "Sample", "Application title", "small.ico" )

   // OK - now create application instance
   APPLICATION CREATE
   //oApp:Create()

   // Define MAIN window
   DEFINE WINDOW oWnd1 AT 10,10 SIZE 700, 500 PIXEL STATUSBAR
   //oWnd1 := TFrame():New()
   //oWnd1:DisplayData()

   #ifdef WG_DEBUG
      //DEFINE PANEL oPnl ;
      //       AT 250, 0 SIZE 700, 150 PIXEL ;
      //          OF oWnd1
      //
      //   @   0,  0 LABEL "Click with right mouse button to activate context menu" ;
      //             SIZE 700, 50 ;
      //             PIXEL ;
      //             OF oPnl ;
      //             TOOLTIP "Label" ;
      //             CENTER ;
      //             COLOR "RED" // BACKCOLOR "yellow"
      //
      //   @  50,  0 EDITBOX MULTILINE oStatus VAR dummy ;
      //             SIZE 600, 100 ;
      //             CAPTION "Event Log" ;
      //             READONLY ;
      //             VSCROLL ;
      //             HSCROLL ;
      //             OF oPnl ;
      //             FONTNAME "Courier New";
      //             FONTSIZE 7.5;
      //             PIXEL ;
      //             TOOLTIP "Status Window"
      //
      //   @ 60, 610 PUSHBUTTON "Enable Status" ;
      //             PIXEL ;
      //             OF oPnl ;
      //             TOOLTIP "Enable Status Window";
      //             ACTION oApp:EnableLogWrite()
      //
      //   @ 90, 610 PUSHBUTTON "Disable Status" ;
      //             PIXEL ;
      //             OF oPnl ;
      //             TOOLTIP "Disable Status Window";
      //             ACTION oApp:DisableLogWrite()
      //
      //   @ 120,610 PUSHBUTTON "Clear Status" ;
      //             PIXEL ;
      //             OF oPnl ;
      //             TOOLTIP "Clear Status Window";
      //             ACTION oStatus:Clear()
      //
      //
      //PANEL oPnl CREATE
      ////oPnl:Create()
      //WINDOW oPnl ACTIVATE
      //
      //APPLICATION DEBUG EVENTS IN WINDOW oStatus FILE "events.log"

      APPLICATION DEBUG EVENTS FILE "events.log"

      // This function create a events.log file with applications events :-)
      //WG_ApplObj():SetLogWrite( TRUE,, TRUE )

   #endif // WG_DEBUG

   SET WINDOW oWnd1 STATUSBAR "Sample Form window - Status bar"
   //oWnd1:SetStatusBar("Sample Form window - Status bar")

   // DefineMenu( oWnd1 )
   oMenuBar := DefineMenu( oWnd1 )
   oContextMenu := ContextMenu( oWnd1, oMenuBar )

   SET WINDOW oWnd1 MENU oMenuBar
   SET WINDOW oWnd1 CONTEXTMENU oContextMenu
   //oWnd1:SetMenu( oMenuBar )

   oWnd1:SetBackgroundFromFile( "resource\bricks.bmp" )

   SET WINDOW oWnd1 TITLE "Francesco's Window"
   //oWnd1:SetTitle( "Francesco's Window" )

   SET WINDOW oWnd1 EVENTHANDLER {|hWnd, nMsg, wParam, lParam| MyEvent( hWnd, nMsg, wParam, lParam, oWnd1 ) }
   //oWnd1:SetEventHandler( {|hWnd, nMsg, wParam, lParam| MyEvent( hWnd, nMsg, wParam, lParam, oWnd1 ) } )

   // oWnd1:Show()
   WINDOW oWnd1 ACTIVATE
   // oWnd1:Activate()

   APPLICATION ACTIVATE
   //oApp:Activate()
   // APPLICATION QUIT -- Now called on exiting from application activate
   //oApp:Quit()

return (0)

STATIC PROCEDURE Appl_Quit()
  // Here you can put your closing application calls
RETURN

FUNCTION MyEvent( hWnd, nMsg, wParam, lParam, oWnd )
  LOCAL nRet := -1
  DO CASE
     CASE nMsg == WM_CLOSE
          WG_DebugTrace( "DEMO_MyEvent - WM_CLOSE" )
          IF ( MessageBox(hwnd, "Are you sure ?  ;-)", "Quit Application", ;
                          MB_YESNO) == IDYES)
              WG_DebugTrace( "DEMO_MyEvent - WM_CLOSE - Destroy Application Windows" )
              oWnd:Destroy()
          ENDIF
          nRet := 0  // Something is done.
  ENDCASE
RETURN nRet

PROCEDURE BaseDialog( oWnd1, lModal )
   local oDlg1
   local oBut1, oBut2, oBut3, oBut4, oBut5, oBut6, oBut7, oBut8, oBut9, oBut10
   local oStatusBar
   local aRows
   local bOldEventHandler
   local nVar1, nVar2, nVar3, nVar4, nVar5, nVar6, nVar7
   //local cVar1 := Space(10), cVar2 := Space(50)
   local cVar1, cVar2, oFont, nFont, oBtnF

   LOCAL oCM

   DEFAULT lModal TO TRUE

   IF lModal
      DEFINE DIALOG oDlg1 ;
             MODAL ;
             AT 30, 30 SIZE 600, 500 PIXEL ;
             TITLE "Example Modal Dialog" ;
             OF oWnd1
   ELSE
      DEFINE DIALOG oDlg1 ;
             AT 30, 30 SIZE 600, 600 PIXEL ;
             TITLE "Example Modeless Dialog" ;
             OF oWnd1 STATUSBAR
   ENDIF
   // oDlg1 := tDialog():New("Example Dialog",0,0,300,550, oWnd1)
   /*
   DEFINE DIALOG oDlg1 ;
          AT 0, 0 SIZE 100, 100 PIXEL ;
          TITLE "Example Dialog" ;
          OF oWnd1 STATUSBAR
   */

   // SET CURRENT WINDOW oDlg1  // Actually automatically called during dialog creation
   //WG_ApplObj():SetCurrentWindow( oDlg1 )
   /*
   @ 10,10 PUSHBUTTON "OK" ;
             size 10, 5 ;
             PIXEL ;
             TOOLTIP "Close Dialog Window" ;
             ACTION oDlg1:Destroy()

   @ 20,10 PUSHBUTTON "Close" ;
             size 10, 5 ;
             PIXEL ;
             TOOLTIP "Close Dialog Window" ;
             ACTION oDlg1:Destroy()
   */

   DEFINE MENUBAR oCM
       POPUP "Try"
           ITEM "&First Item of context menu"  ACTION NothingToDo()
           ITEM "&Second Item of context menu" ACTION NothingToDo()
       END POPUP
   END MENU

   @  10, 10 PUSHBUTTON oBut1 PROMPT "&Show Win 1" ;
             PIXEL ;
             TOOLTIP "Show Window 1";
             STATUS "Press to show window 1";
             ACTION (oWnd1:Show(), oDlg1:ToTop())

   oBut1:SetContextMenu( oCM )

             //FONTNAME "Courier New" FONTSIZE 10
   //oBut1 := tPushButton():New( "&Show Win 1", 10, 10, ,,,"Show Window 1", "Press to show window 1" )
   //oBut1:bAction   := {|| (oWnd1:Show(), oDlg1:ToTop()) }
   //oBut1:SetFontByAttribute( "Times New Roman", 11,TRUE,TRUE,TRUE,TRUE)

   @  40, 10 PUSHBUTTON "&Hide Win 1" ;
             PIXEL ;
             TOOLTIP "Hide Window 1";
             ACTION oWnd1:Hide() ;
             DEFAULT
   //oBut2 := tPushButton():New( "&Hide Win 1", 40,10,  ,,,"Hide Window 1" )
   //oBut2:bAction   := {|| oWnd1:Hide() }


   /*
   @  70, 10 PUSHBUTTON "&Min. Win 1" ;
             PIXEL ;
             TOOLTIP "Minimize Window 1";
             ACTION oWnd1:Minimize()
   //oBut3 := tPushButton():New( "&Min. Win 1", 70,10,  ,,,"Minimize Window 1" )
   //oBut3:bAction   := {|| oWnd1:Minimize() }
   */

   @ 100, 10 PUSHBUTTON "M&ax. Win 1" ;
             PIXEL ;
             TOOLTIP "Minimize Window 1";
             ACTION (oWnd1:Maximize(),  oDlg1:ToTop())
   //oBut4 := tPushButton():New( "M&ax. Win 1", 100,10, ,,,"Maximize Window 1" )
   //oBut4:bAction   := {|| (oWnd1:Maximize(),  oDlg1:ToTop()) }

   @ 130, 10 PUSHBUTTON "&Res. Win. 1" ;
             PIXEL ;
             TOOLTIP "Restore Window 1";
             ACTION (oWnd1:Restore(),  oDlg1:ToTop())
   //oBut5 := tPushButton():New( "&Res. Fin. 1",130,10, ,,,"Restore Window 1" )
   //oBut5:bAction   := {|| (oWnd1:Restore(),  oDlg1:ToTop()) }

   @ 160, 10 PUSHBUTTON "&Dis. But. 1" ;
             PIXEL ;
             TOOLTIP "Disable Button 1";
             ACTION oBut9:Disable()
   //oBut6 := tPushButton():New( "&Dis. But. 1",160,10, ,,,"Disable Button 1" )
   //oBut6:bAction   := {|| oBut9:Disable() }

   @ 190, 10 PUSHBUTTON "&Ena. But. 1" ;
             PIXEL ;
             TOOLTIP "Enable Button 1";
             ACTION oBut9:Enable()
   //oBut7 := tPushButton():New( "&Ena. But. 1",190, 10,,,,"Enable Button 1" )
   //oBut7:bAction   := {|| oBut9:Enable() }

   @ 220, 10 PUSHBUTTON "Get values" ;
             PIXEL ;
             TOOLTIP "Show control values";
             ACTION ShowControlValues( oDlg1 ) ;
             STATUS "Display all controls with values"
   //oBut8 := tPushButton():New( "Get values",220,10, ,,,"Show control values" )
   //oBut8:bAction   := {|| ShowControlValues( oDlg1 ) }

   @ 270, 10 CHECKBOX nVar1 PROMPT "Check Box";
             PIXEL ;
             TOOLTIP "Check Box";
             SELECTED ;
             COLOR "yellow" //BACKCOLOR "white"
   //oBut8 := tCheckBox():New( "Check Box",270,10, ,,,,"Check Box" )
   //oBut8 := tRadioButton():New( "Radio 1",300,10, ,,,,"Radio Button",,TRUE )
   //oBut8 := tRadioButton():New( "Radio 2",330,10, ,,,,"Radio Button" )

   @ 300, 10 RADIOGROUP nVar2 ITEMS "Radio &1","Radio &2" ;
             PIXEL ;
             TOOLTIP "Radio Button Vertical";
             SELECT 2 ;
             COLOR "MAGENTA" ;
             FONTNAME "Verdana" FONTSIZE 9
   //tRadioGroup():New( {"Radio 1","Radio 2"},300,10, ,,,,"Radio Button",, TRUE, 2 )

   @ 300,250 RADIOGROUP nVar3 ITEMS "Radio &3","Radio &4","Radio &5" ;
             PIXEL ;
             TOOLTIP "Radio Button Horizontal";
             SELECT 3 ;
             HORIZONTAL ;
             FONT (TFont():New( "Verdana", 8, TRUE ))
   //tRadioGroup():New( {"Radio 3","Radio 4","Radio 5"},300,250, ,,,,"Radio Button Horizontal",, FALSE )

   @ 360, 10 TRISTATEBUTTON nVar4 PROMPT "3 s&tate" ;
             PIXEL ;
             TOOLTIP "3State Button";
             COLOR "yellow" ;
             VALUE 2
   //oBut8 := t3StateButton():New( "3 state",360,10,,,,,"3State Button" )

   @ 250,  5 GROUPBOX SIZE 120,200 PROMPT "Group Box";
             PIXEL ;
             TOOLTIP "Group Box"
   //oBut8 := tGroupBox():New( "GroupBox",250,5,120,200,,"Group Box" )

   aRows := { "One", "Two", "Three", "Four", "Five", "Six" }

   @  10,120 COMBOBOX nVar5 ;
             ITEMS aRows ;
             PIXEL ;
             TOOLTIP "Simple Combo Box" ;
             COLOR "yellow"  BACKCOLOR "blue";
             VALUE 2

   //oBut8 := tSimpleComboBox():New( "Combo",10, 120, ,, , aRows,"Simple Combo Box" )

   @ 110,120 DROPDOWN nVar6 ;
             ITEMS aRows ;
             PIXEL ;
             TOOLTIP "Drop Down Combo Box" ;
             COLOR "yellow" BACKCOLOR "blue" ;
             VALUE 3
   //oBut8 := tDropDownComboBox():New( "Combo",110, 120, ,,, aRows,"Drop Down Combo Box" )

   @ 210,120 DROPLIST nVar7 ;
             ITEMS aRows ;
             PIXEL ;
             TOOLTIP "Drop List Combo Box" ;
             COLOR "yellow"  BACKCOLOR "blue" ;
             VALUE 4
   //oBut8 := tDropListComboBox():New( "Combo",210, 120, ,,, aRows,"Drop List Combo Box" )

   @ 240,150 PUSHBUTTON oBut8 PROMPT "Font &Object" ;
             PIXEL ;
             TOOLTIP "Display Font Object" ;
             ACTION oFont:=TFont():New(), WG_ParamDisplay( oFont, oFont ), oFont:GetValue():DisplayData()
   //oBut8 := tPushButton():New( "Font Object",240,150, ,,,"Choose Font Dialog Window" )
   //oBut8:bAction   := {|oW, oFont| oFont:=TFont():New(), WG_ParamDisplay( oFont, oFont ), WG_ObjDisplayData( oFont:GetValue() ) }

   @ 270,150 PUSHBUTTON oBtnF PROMPT "Choose &Font" ;
             PIXEL ;
             TOOLTIP "Choose a font from Font Dialog Window and change the font of this button" ;
             ACTION oFont := TFont():New(), oFont:Choose(),;
                    oFont:DisplayData(), iif( oFont:nHandle <> 0, oBtnF:SetFont( oFont ), NIL )
   // oBut8 := tPushButton():New( "Choose Font",270,150, ,,,"Choose Font Dialog Window" )
   // oBut8:bAction   := {|oW, oFont, nFont| oFont := TFont():New(), nFont:=ChooseFont( oFont ),;
   //                                        WG_ObjDisplayData( oFont ), iif( nFont <> 0, oW:SetFont( oFont ), NIL )  }

   @  10,300 EDITBOX oBut9 VAR cVar1 ;
             CAPTION "value" ;
             PIXEL ;
             TOOLTIP "Edit control limited to 5 chars" ;
             STATUS "Please insert something in singleline edit control" ;
             LIMIT 5



   @  40,300 EDITBOX MULTILINE oBut9 VAR cVar2 ;
             CAPTION "Default value" ;
             PIXEL ;
             TOOLTIP "Multiline Edit control limited to 200 chars" ;
             STATUS "Please insert something in multiline edit control" ;
             LIMIT 200


             //ACTION {|oW, oFont| oFont:=TFont():New(), WG_ParamDisplay( oFont, oFont ), oFont:GetValue():DisplayData() }
   //oBut9 := tEdit():New( "Font Object",330,300, ,,,"Choose Font Dialog Window" )
   //oBut9 := tEdit():New( "Font Object",30,30)
   //oBut8:bAction   := {|oW, oFont| oFont:=TFont():New(), WG_ParamDisplay( oFont, oFont ), WG_ObjDisplayData( oFont:GetValue() ) }

   @ 330,150 LABEL "Label" ;
             PIXEL ;
             TOOLTIP "Label" ;
             COLOR "RED" // BACKCOLOR "yellow"
   //oBut9 := tStaticText():New( "Label",330,150, ,,,"Label" )

   @ 360,150 PUSHBUTTON "Font But 1" ;
             PIXEL ;
             TOOLTIP "Display Font Object of oBut1" ;
             ACTION ( oBut1:DisplayData(), oBut1:GetFont():DisplayData() )
   // oBut8 := tPushButton():New( "Font But 1",360,150, ,,,"Choose Font Dialog Window" )
   // oBut8:bAction   := {|oW| WG_ObjDisplayData( oBut1:GetFont() ) }

   @ 390,150 PUSHBUTTON "Derive" ;
             PIXEL ;
             TOOLTIP "Display That TButton Class derive from TWindow Class" ;
             ACTION MessageBox(, "tBtn derive from tWind ? = " + WG_char( oBut8:IsDerivedFrom("TWINDOW") ) )
   //oBut8 := tPushButton():New( "Derive",390,150, ,,,"Choose Font Dialog Window" )
   //oBut8:bAction   := {|oW| MessageBox(, "tBtn derive from tWind ? = " + WG_char( __objDerivedFrom( oBut8, "TWINDOW") )) }

   @ 420,150 PUSHBUTTON "Ancestor" ;
             PIXEL ;
             TOOLTIP "Display the ancestor of this Button" ;
             ACTION oBut8:GetGrandParent():DisplayData()

   @ 390,250 PUSHBUTTON "Values" ;
             PIXEL ;
             TOOLTIP "Values" ;
             ACTION MessageBox(, "Value Check Box       = " + WG_CHAR(nVar1) + CRLF +;
                                       "Value 1th Radio Group = " + WG_CHAR(nVar2) + CRLF +;
                                       "Value 2th Radio Group = " + WG_CHAR(nVar3) + CRLF +;
                                       "Value 3State Button   = " + WG_CHAR(nVar4) ;
                                    );

   @ 450,300 PUSHBUTTON "&Close" ;
             PIXEL ;
             TOOLTIP "Close Dialog Window" ;
             ID IDOK

   @ 450,400 PUSHBUTTON oBut10 PROMPT "C&ancel" ;
             PIXEL ;
             TOOLTIP "Cancel Dialog Window" ;
             ID IDCANCEL

   DIALOG oDlg1 CREATE
   //oDlg1:Create()

   IF !lModal
      // From here if is modeless cannot be evaluate until end dialog
      SET DIALOG oDlg1 CENTER
      //oDlg1:Center()

      SET DIALOG oDlg1 STATUSBAR "Sample Dialog Window - Status bar"
      //oDlg1:SetStatusBar("Sample Dialog Window - Status bar")

      WINDOW oDlg1 ACTIVATE
   ENDIF

RETURN

PROCEDURE EditDialog( oWnd1, lModal )
   local oDlg1
   local oEdt1, oEdt2
   LOCAL oBut1, oBut2, oBut3, oBut4, oBut5, oBut6, oBut7, oBut8, oBut9, oBut10
   local oStatusBar, oCal, dVar1
   local aRows
   local bOldEventHandler
   local nVar1, nVar2, nVar3, nVar4, nVar5, nVar6, nVar7
   local cVar1, cVar2
   local oObjWithFocus

   DEFAULT lModal TO TRUE

   IF lModal
      DEFINE DIALOG oDlg1 ;
             MODAL ;
             AT 30, 30 SIZE 210, 150 ;
             TITLE "Example Dialog with Edit Test" ;
             OF oWnd1
   ELSE
      DEFINE DIALOG oDlg1 ;
             AT 30, 30 SIZE 210, 150 ;
             TITLE "Example Dialog with Edit Test" ;
             OF oWnd1 STATUSBAR
   ENDIF

   //SET CURRENT WINDOW oDlg1

   @  10, 10 EDITBOX oEdt1 VAR cVar1 ;
             CAPTION "Default value" ;
             TOOLTIP "Edit control limited to 20 chars" ;
             STATUS "Please insert something in singleline edit control" ;
             LIMIT 20 ;
             COLOR "rb"
             //COLOR "YELLOW" BACKCOLOR "WHITE"

   @  25, 10 EDITBOX MULTILINE oEdt2 VAR cVar2 ;
             CAPTION "Default value" ;
             TOOLTIP "Multiline Edit control limited to 200 chars" ;
             STATUS "Please insert something in multiline edit control" ;
             LIMIT 200 ;
             VSCROLL ;
             COLOR "rb"


   @   80,10 CALENDAR oCal VAR dVar1 ;
             CAPTION "Default value" ;
             TOOLTIP "Calendar control" ;
             STATUS "Please select a date from calendar control" ;
             COLOR "rb"

   @  10,100 LABEL "Label"  ;
             TOOLTIP "Label" ;
             SIZE 20, 10 ;
             /*FONTNAME "Verdana" FONTSIZE 12*/ ;
             COLOR "B"
             //COLOR "YELLOW" BACKCOLOR "BLUE"
             //COLOR RGB( 0,0,255)  BACKCOLOR RGB( 255,0,0)

   @  10,130 LABEL "Label"  ;
             TOOLTIP "Label" ;
             SIZE 20, 10 ;
             /*FONTNAME "Verdana" FONTSIZE 12*/ ;
             COLOR "w/b"
             //COLOR "YELLOW" BACKCOLOR "BLUE"
             //COLOR RGB( 0,0,255)  BACKCOLOR RGB( 255,0,0)

   @  10,160 PUSHBUTTON "Get values" ;
             TOOLTIP "Show control values";
             ACTION ShowControlValues( oDlg1 ) ;
             STATUS "Display all controls with values"

   @  25,160 PUSHBUTTON oBut1 PROMPT "Select Value" ;
             TOOLTIP "Select value from edit controls";
             ACTION oEdt1:SetSelection( 1, 5 ), oEdt1:SetFocus()  // SetFocus() display selection

             ///*oObjWithFocus := ::GetFocus(),*/ , ::MessageBox( cStr( ::GetFocus() ) )
              //oEdt1:SelectAll() //, oObjWithFocus:SetFocus()
             //, oEdt1:Redraw(), oEdt2:SetFocus(FALSE), oEdt2:SelectAll(), oEdt2:Redraw() )

   @  40,160 PUSHBUTTON oBut2 PROMPT "Clear Value" ;
             TOOLTIP "Clear value from edit controls";
             ACTION ( oEdt1:Clear(), oEdt1:Redraw(), oEdt2:Clear(), oEdt2:Redraw() )

   @  55,160 PUSHBUTTON oBut3 PROMPT "Active passwd" ;
             TOOLTIP "Set oEdt1 as password edit box";
             ACTION ( oEdt1:SetPassword(), oEdt1:Redraw() )

   @  70,160 PUSHBUTTON oBut3 PROMPT "Remove passwd" ;
             TOOLTIP "UnSet oEdt1 as password edit box";
             ACTION ( oEdt1:SetPassword( FALSE ), oEdt1:Redraw() )

   @  85,160 PUSHBUTTON oBut3 PROMPT "Change color 1" ;
             TOOLTIP "Change color to Edit Control";
             ACTION oEdt1:PushColors( "W+/B" )

   @ 100,160 PUSHBUTTON oBut3 PROMPT "Change color 2" ;
             TOOLTIP "Change color to Edit Control";
             ACTION oEdt1:PopColors()

   @ 120,160 PUSHBUTTON "Values" ;
             TOOLTIP "Values" ;
             ACTION oDlg1:MessageBox(  "Value Single Line Edit Box = " + cVar1 + CRLF +;
                                       "Value MultiLine   Edit Box = " + cVar2  ;
                                    )

   @ 120, 10 PUSHBUTTON "&Close" ;
             DEFAULT ;
             TOOLTIP "Close Dialog Window" ;
             STATUS "Close this dialog window" ;
             ID IDOK

   @ 120, 60 PUSHBUTTON oBut10 PROMPT "C&ancel" ;
             TOOLTIP "Cancel Dialog Window" ;
             STATUS "Cancel this dialog window" ;
             ID IDCANCEL


   /*

   @ 330,150 LABEL "Label" ;
             PIXEL ;
             TOOLTIP "Label"


   @ 450,300 PUSHBUTTON "&Close" ;
             PIXEL ;
             TOOLTIP "Close Dialog Window" ;
             ID IDOK


   @ 450,400 PUSHBUTTON oBut10 PROMPT "C&ancel" ;
             PIXEL ;
             TOOLTIP "Cancel Dialog Window" ;
             ID IDCANCEL //;
   */

   DIALOG oDlg1 CREATE
   //oDlg1:Create()

   IF !lModal
      // From here if is modeless cannot be evaluate until end dialog
      SET DIALOG oDlg1 CENTER
      //oDlg1:Center()

      SET DIALOG oDlg1 STATUSBAR "Sample Dialog Window - Status bar"
      //oDlg1:SetStatusBar("Sample Dialog Window - Status bar")

      WINDOW oDlg1 ACTIVATE
   ENDIF

RETURN

STATIC FUNCTION DefineMenu( oWnd )
   LOCAL oMenu, oItem1, oItem2

   DEFINE MENUBAR oMenu
       POPUP "&File"
           ITEM "&Dialog Window - Modal"  MESSAGE "Open a Modal Dialog Window"  ;
                ACTION BaseDialog( oWnd, TRUE )
           ITEM "&Dialog Window - Modeless" MESSAGE "Open a Modeless Dialog Window";
                ACTION BaseDialog( oWnd, FALSE )
           SEPARATOR
           ITEM "Edit   &Window - Modal"    ACTION EditDialog( oWnd, TRUE )
           //ITEM "&Dialog Window - Modeless" ACTION BaseDialog( oWnd, FALSE )
           SEPARATOR
           POPUP "&Preferences"
               ITEM oItem1 CAPTION "&Memory"   DISABLED  MENUID 50 ACTION NothingToDo()
               ITEM oItem2 CAPTION "&Printers" CHECKED   MENUID 60  //ACTION
               POPUP "&Others"
                   POPUP "&Another Submenu"
                       ITEM "&Hey Guys :-)" ACTION NothingToDo()
                   END POPUP
               END POPUP
               //ITEM "Enable Memory menu"    ACTION oMenu:Enable( 50 )
               //ITEM "Disable Memory menu"   ACTION oMenu:Disable( 50 )
               //ITEM "Check Printers menu"   ACTION oMenu:Check( 60 )
               //ITEM "UnCheck Printers menu" ACTION oMenu:UnCheck( 60 )
               //ITEM "Memory && Printer Menu status" ACTION MessageBox( , "Memory   : " + IIF( oMenu:IsEnabled( 50 ), "Enabled", "Disabled" ) + CRLF +;
               //                                                         "Printers : " + IIF( oMenu:IsChecked( 60 ), "Checked", "UnChecked" ) )
               //ITEM "Memory Menu status via item object" ACTION MessageBox( , "Memory   : " + IIF( oItem1:IsEnabled(), "Enabled", "Disabled" ) )
               //ITEM "Printers Menu status via item object" ACTION MessageBox( , "Printers : " + IIF( oItem2:IsChecked(), "Checked", "UnChecked" ) )
           END POPUP
           SEPARATOR
           ITEM "&Exit" MESSAGE "Terminate application" ACTION oWnd:Destroy()
       END POPUP
       POPUP "&Modify"
           ITEM "&Cut"
           ITEM "&Copy"
           ITEM "&Paste"
       END POPUP
       POPUP "Mi&sc"
           ITEM "&DatePicker control test"  ;
                ACTION CalendarTest( oWnd )
           ITEM "&Get Test"  ;
                ACTION GetTest( oWnd )
       END POPUP
       POPUP "&Information"
           ITEM "&Status Bar" MESSAGE "Display informations about status bar" ;
                ACTION MessageBox(,"Number of parts : " + cStr( oWnd:oStatusBar:GetFieldsCount() ) )
       END POPUP
       POPUP "&?"
           ITEM "&About" ACTION AboutBox(oWnd)
       END POPUP
   END MENU

RETURN oMenu

STATIC FUNCTION ContextMenu( oWnd, oMenuBar )
   LOCAL oContextMenu, oItem1, oItem2

   LOCAL oMenuItem //:= TMenuItem():New(, 200, "Item inserted",,,, "Item inserted on the fly" )
   LOCAL oMenuItem1
   //LOCAL oPopup

   ITEM oMenuItem CAPTION "-- ITEM INSERTED --" ;
                  MESSAGE "Item inserted and deleted on fly" ;
                  ACTION MessageBox(0, "Inserted on fly !" ) ;
                  DEFERRED // This is because we are outside a menu definition

   ITEM oMenuItem1 CAPTION "-- ITEM MODIFIED --" ;
                  MESSAGE "Item modified on fly" ;
                  ACTION MessageBox(0, "Modified on fly !" ) ;
                  DEFERRED // This is because we are outside a menu definition


   DEFINE CONTEXTMENU oContextMenu
       //POPUP oPopUp CAPTION "Dummy"
           ITEM "Enable Memory menu"    MESSAGE "Enable Memory menu in base window menu" ;
                                        ACTION oMenuBar:Enable( 50 ), MessageBox( , "Enabled" )
           ITEM "Disable Memory menu"   MESSAGE "Disable Memory menu in base window menu" ;
                                        ACTION oMenuBar:Disable( 50 ), MessageBox( , "Disabled" )
           ITEM "Check Printers menu"   ACTION oMenuBar:Check( 60 ), MessageBox( , "Checked" )
           ITEM "UnCheck Printers menu" ACTION oMenuBar:UnCheck( 60 ), MessageBox( , "UnChecked" )
           ITEM "Memory && Printer Menu status" ACTION MessageBox( , "Memory   : " + IIF( oMenuBar:IsEnabled( 50 ), "Enabled", "Disabled" ) + CRLF +;
                                                                    "Printers : " + IIF( oMenuBar:IsChecked( 60 ), "Checked", "UnChecked" ) )
           SEPARATOR
           ITEM "&Insert Item on the fly" MENUID 89 MESSAGE "Insert Item on the fly (pay attention to the menu to use)" ;
                                        ACTION oContextMenu:Insert( oMenuItem, 2 ), oContextMenu:Redraw(), oContextMenu:Enable( 90 ), oContextMenu:Enable( 91 ), oContextMenu:Disable( 89 )
           ITEM "&Delete Item on the fly" DISABLED MENUID 90 MESSAGE "Delete Item on the fly (pay attention to the menu to use)" ;
                                        ACTION oContextMenu:Delete( 2 ), oContextMenu:Redraw(), oContextMenu:Enable( 89 ), oContextMenu:Disable( 90 ), oContextMenu:Disable( 91 )
           ITEM "&Modify Item on the fly" DISABLED MENUID 91 MESSAGE "Modify Item on the fly (pay attention to the menu to use)" ;
                                        ACTION oContextMenu:Modify( oMenuItem1, 2 ), oContextMenu:Redraw()
           SEPARATOR
           ITEM "&About" ACTION AboutBox(oWnd)
           SEPARATOR
           ITEM "&Exit" MESSAGE "Terminate application" ACTION oWnd:Destroy()
       //END POPUP
   END CONTEXTMENU

RETURN oContextMenu

STATIC FUNCTION ShowControlValues( oWnd )
   local cString := "", n, oCtrls := oWnd:GetChildren(), o

//   aEval( oWnd:aoControls, {|o| cString += o:ClassName + " - " + o:cName + " - " + ;
//                               o:GetValueAsString() + CRLF } )

   FOR n := 1 TO LEN( oCtrls )
     o := oCtrls[n]
     cString += o:ClassName + " - "
     cString += o:cName + " - "
     cString += o:GetValueAsString()
     cString += CRLF
   NEXT n

   MessageBox(, cString, "Control values" )

RETURN 0

PROCEDURE AboutBox(oWnd)
   MessageBox(oWnd:nHandle, "WoopGUI - A Win32 OOP GUI for Harbour"+CRLF+;
                            "(C) Francesco Saverio Giudice - 2002")
RETURN

PROCEDURE NothingToDo()
   MessageBox(0, "This Item do nothing !" )
RETURN

#include "tst_dp.prg"
#include "tst_get.prg"