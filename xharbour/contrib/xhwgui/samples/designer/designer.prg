/*
 * HWIDE
 * Main file
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "guilib.ch"

Function Main()
Local oMainWindow, oPanel, oTab, oFont, hDCwindow
Local hWnd
Private oBtnPressed
Private oDlgCurrent
Private aTermMetr := { 800 }
Private cClipbrd := ""
Private oCtrlMenu, oDlgMenu
Private oStatus
Private aCtrlGroups := { "Standard","Win32","HwGUI" }
Public mypath := "\" + CURDIR() + IIF( EMPTY( CURDIR() ), "", "\" )
Public addItem := ""
Public crossCursor, vertCursor, horzCursor

   IF ( hWnd := Hwg_FindWindow( "HwGUI_App","Designer" ) ) != 0
      IF Hwg_IsIconic( hWnd )
         Hwg_RestoreWindow( hWnd )
      ENDIF
      Hwg_SetForegroundWindow( hWnd )
      Return
   ENDIF

   PREPARE FONT oFont NAME "MS Sans Serif" WIDTH 0 HEIGHT -13
   crossCursor := LoadCursor( IDC_CROSS )
   horzCursor := LoadCursor( IDC_SIZEWE )
   vertCursor := LoadCursor( IDC_SIZENS )

   INIT WINDOW oMainWindow MAIN  ;
      TITLE "Designer" ON EXIT {||EndIde()}

   MENU OF oMainWindow
      MENU TITLE "&File"
         MENUITEM "&New Form" ACTION HFormGen():New()
         MENUITEM "&Open Form" ACTION HFormGen():Open()
         SEPARATOR
         MENUITEM "&Save Form"   ACTION Iif(oDlgCurrent!=Nil,oDlgCurrent:oParent:Save(),MsgStop("No Form in use!"))
         MENUITEM "&Save as ..." ACTION Iif(oDlgCurrent!=Nil,oDlgCurrent:oParent:Save(.T.),MsgStop("No Form in use!"))
         MENUITEM "&Close Form"  ACTION Iif(oDlgCurrent!=Nil,oDlgCurrent:oParent:End(),MsgStop("No Form in use!"))
         SEPARATOR
         MENUITEM "&Exit" ACTION EndWindow()
      ENDMENU
      MENU TITLE "&Edit"
         MENUITEM "&Copy control" ACTION (cClipBrd:=Ctrl2String(GetCtrlSelected(oDlgCurrent)),Iif(!Empty(cClipBrd),EnableMenuItem(,101,.T.,.T.),.F.))
         MENUITEM "&Paste" ID 101 ACTION addItem := "PASTE"
      ENDMENU
      MENU TITLE "&Control"
         MENUITEM "&Style"     ACTION SetStyle( oDlgCurrent )
         MENUITEM "&Extended..."   ACTION SetExten( oDlgCurrent )
         SEPARATOR
         MENUITEM "&Delete"  ACTION DeleteCtrl()
      ENDMENU
      MENU TITLE "&Service"
         MENUITEM "&Sort"    ACTION SortCtrls()
      ENDMENU
      MENU TITLE "&Help"
         MENUITEM "&About" ACTION MsgInfo("About")
      ENDMENU
   ENDMENU

   EnableMenuItem( ,101, .F., .T. )

   hDCwindow := GetDC( oMainWindow:handle )
   aTermMetr := GetDeviceArea( hDCwindow )
   MoveWindow(oMainWindow:handle,0,0,aTermMetr[1],115)
   DeleteDC( hDCwindow )

   @ 0,0 PANEL oPanel SIZE 0,60

   @ 2,3 OWNERBUTTON OF oPanel       ;
       ON CLICK {||HFormGen():New()} ;
       SIZE 24,24 FLAT               ;
       BITMAP "BMP_NEW" FROM RESOURCE COORDINATES 0,4,0,0 ;
       TOOLTIP "New Form"
   @ 26,3 OWNERBUTTON OF oPanel       ;
       ON CLICK {||HFormGen():Open()} ;
       SIZE 24,24 FLAT                ;
       BITMAP "BMP_OPEN" FROM RESOURCE COORDINATES 0,4,0,0 ;
       TOOLTIP "Open Form"

   @ 55,6 LINE LENGTH 18 VERTICAL

   @ 60,3 OWNERBUTTON OF oPanel       ;
       ON CLICK {||Iif(oDlgCurrent!=Nil,oDlgCurrent:oParent:Save(),MsgStop("No Form in use!"))} ;
       SIZE 24,24 FLAT                ;
       BITMAP "BMP_SAVE" FROM RESOURCE COORDINATES 0,4,0,0 ;
       TOOLTIP "Save Form"

   // @ 5,30 LINE LENGTH 200
   @ 5,32 SAY oStatus CAPTION "" SIZE 200,24 FONT oFont STYLE WS_BORDER

   @ aTermMetr[1]-450,2 TAB oTab ITEMS {} OF oPanel SIZE 450,58 ;
        FONT oFont

   BEGIN PAGE "Standard" OF oTab

      @ 4,30 OWNERBUTTON                     ;
          SIZE 30,24 FLAT                    ;
          ON CLICK {|o,id|ClickBtn(o,id,"STATIC","Text",80,22)} ;
          BITMAP "BMP_STATIC" FROM RESOURCE  ;
          TOOLTIP "Static text"

      @ 34,30 OWNERBUTTON  SIZE 30,24 FLAT   ;
          ON CLICK {|o,id|ClickBtn(o,id,"EDITBOX","",80,24)} ;
          BITMAP "BMP_EDIT" FROM RESOURCE    ;
          TOOLTIP "Edit control"

      @ 64,30 OWNERBUTTON  SIZE 30,24 FLAT   ;
          ON CLICK {|o,id|ClickBtn(o,id,"BUTTON","Button",80,32)} ;
          BITMAP "BMP_BUTTON" FROM RESOURCE  ;
          TOOLTIP "Push button"

      @ 94,30 OWNERBUTTON  SIZE 30,24 FLAT   ;
          ON CLICK {|o,id|ClickBtn(o,id,"CHECKBOX","CheckBox",110,22)}  ;
          BITMAP "BMP_CHECK" FROM RESOURCE   ;
          TOOLTIP "Checkbox"

      @ 124,30 OWNERBUTTON  SIZE 30,24 FLAT  ;
          ON CLICK {|o,id|ClickBtn(o,id,"RADIOBUTTON","RadioButton",110,22)}  ;
          BITMAP "BMP_RADIO" FROM RESOURCE   ;
          TOOLTIP "RadioButton"

      @ 154,30 OWNERBUTTON SIZE 30,24 FLAT   ;
          ON CLICK {|o,id|ClickBtn(o,id,"GROUP","Group",100,80)}  ;
          BITMAP "BMP_GROUP" FROM RESOURCE   ;
          TOOLTIP "Group"

      @ 184,30 OWNERBUTTON SIZE 30,24 FLAT   ;
          ON CLICK {|o,id|ClickBtn(o,id,"COMBOBOX","",110,24)}  ;
          BITMAP "BMP_COMBO" FROM RESOURCE   ;
          TOOLTIP "ComboBox"

   END PAGE OF oTab

   BEGIN PAGE "Win32" OF oTab

      @ 4,28 OWNERBUTTON SIZE 30,26 FLAT     ;
          ON CLICK {|o,id|ClickBtn(o,id,"UPDOWN","",80,24)}  ;
          BITMAP "BMP_UPDOWN" FROM RESOURCE  ;
          TOOLTIP "Up-Down control"

      @ 34,28 OWNERBUTTON  SIZE 30,26 FLAT   ;
          ON CLICK {|o,id|ClickBtn(o,id,"DATEPICKER","",80,24)}  ;
          BITMAP "BMP_DATE" FROM RESOURCE    ;
          TOOLTIP "DatePicker"

   END PAGE OF oTab

   BEGIN PAGE "HwGUI" OF oTab

      @ 4,28 OWNERBUTTON SIZE 30,26 FLAT     ;
          ON CLICK {|o,id|ClickBtn(o,id,"HLINE","",100,2)} ;
          BITMAP "BMP_HLINE" FROM RESOURCE   ;
          TOOLTIP "Horizontal Line"

      @ 34,28 OWNERBUTTON  SIZE 30,26 FLAT   ;
          ON CLICK {|o,id|ClickBtn(o,id,"VLINE","",2,30)} ;
          BITMAP "BMP_VLINE" FROM RESOURCE   ;
          TOOLTIP "Vertical Line"

      @ 64,30 OWNERBUTTON  SIZE 30,24 FLAT   ;
          ON CLICK {|o,id|ClickBtn(o,id,"OWNERBUTTON","",80,32)}  ;
          BITMAP "BMP_BUTTON" FROM RESOURCE  ;
          TOOLTIP "Owner drawn button"

      @ 94,30 OWNERBUTTON  SIZE 30,24 FLAT   ;
          ON CLICK {|o,id|ClickBtn(o,id,"BROWSE","",200,150)}  ;
          BITMAP "BMP_BROWSE" FROM RESOURCE  ;
          TOOLTIP "Browse"

      @ 124,30 OWNERBUTTON  SIZE 30,24 FLAT  ;
          ON CLICK {|o,id|ClickBtn(o,id,"PANEL","",200,50)}  ;
          BITMAP "BMP_PANEL" FROM RESOURCE  ;
          TOOLTIP "Panel"

   END PAGE OF oTab

   CONTEXT MENU oCtrlMenu
      MENUITEM "Style"  ACTION SetCtrlStyle( GetCtrlSelected(oDlgCurrent) )
      MENUITEM "Extended..."  ACTION SetCtrlExt( GetCtrlSelected(oDlgCurrent) )
      MENUITEM "Copy"   ACTION (cClipBrd:=Ctrl2String(GetCtrlSelected(oDlgCurrent)),Iif(!Empty(cClipBrd),EnableMenuItem(,101,.T.,.T.),.F.))
      SEPARATOR
      MENUITEM "Delete" ACTION DeleteCtrl()
   ENDMENU

   CONTEXT MENU oDlgMenu
      MENUITEM "Style"  ACTION DlgStyle( oDlgCurrent:oParent )
      MENUITEM "Extended..."  ACTION DlgExten( oDlgCurrent:oParent )
   ENDMENU

   ACTIVATE WINDOW oMainWindow
   oCtrlMenu:End()

Return Nil

Static Function ClickBtn( oTab,nId, cItem,cText,nWidth,nHeight )
Local oBtn := oTab:FindControl( nId )
   IF !Empty( HFormGen():aForms )
      addItem := cItem + ";" + cText + ";0;0;0;" + Ltrim(Str(nWidth)) + ";" ;
          + Ltrim(Str(nHeight)) + ";0;;;"
      IF oBtnPressed != Nil
         oBtnPressed:Release()
      ENDIF
      oBtn:Press()
      oBtnPressed := oBtn
   ENDIF
Return Nil

Function DeleteCtrl()
Local oCtrl, i
   IF oDlgCurrent != Nil .AND. ( oCtrl := GetCtrlSelected(oDlgCurrent) ) != Nil
      DestroyWindow( oCtrl:handle )
      IF ( i := Ascan( oDlgCurrent:aControls, {|o|o:handle==oCtrl:handle} ) ) != 0
         Adel( oDlgCurrent:aControls,i )
         Asize( oDlgCurrent:aControls,Len(oDlgCurrent:aControls)-1 )
      ENDIF
      SetCtrlSelected( oDlgCurrent )
      oDlgCurrent:oParent:lChanged := .T.
   ENDIF
Return

Static Function SortCtrls()
Return

Static Function EndIde
Local i, alen := Len( HFormGen():aForms )

  IF alen > 0
     IF MsgYesNo( "Are you really want to quit ?" )
        FOR i := Len( HFormGen():aForms ) TO 1 STEP -1
           HFormGen():aForms[i]:End()
        NEXT
        Return .T.
     ELSE
        Return .F.
     ENDIF
  ELSE
     Return .T.
  ENDIF

Return .T.

Function ShowCtrlPos( oCtrl )
   IF oCtrl == Nil
      SetWindowText( oStatus:handle, "" )
   ELSE
      SetWindowText( oStatus:handle, "x: "+ Ltrim(Str(oCtrl:nLeft)) + ;
              "  y: " + Ltrim(Str(oCtrl:nTop)) +    ;
              "  cx: " + Ltrim(Str(oCtrl:nWidth)) + ;
              "  cy: " + Ltrim(Str(oCtrl:nHeight)) ) 
   ENDIF
Return Nil
