/*
 * HWIDE
 * Edit properties of items
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "guilib.ch"

Function SetStyle( oDlg )
Local oCtrl := GetCtrlSelected( oDlg )

   IF oCtrl != Nil
      SetCtrlStyle( oCtrl )
   ELSE
      DlgStyle( oDlg:oParent )
   ENDIF

Return Nil

Function SetExten( oDlg )
Local oCtrl := GetCtrlSelected( oDlg )

   IF oCtrl != Nil
      SetCtrlExt( oCtrl )
   ELSE
      DlgExten( oDlg:oParent )
   ENDIF

Return Nil

Function SetCtrlStyle( oCtrl )
   IF oCtrl != Nil
      IF oCtrl:classname() == "HSTATIC"
         DlgStatic( oCtrl )
      ELSEIF oCtrl:classname() == "HEDITGEN"
         DlgEdit( oCtrl )
      ELSEIF oCtrl:classname() == "HBUTTONGEN"
         DlgButton( oCtrl )
      ELSEIF oCtrl:classname() == "HCHECKBOXGEN"
         DlgCheck( oCtrl )
      ELSEIF oCtrl:classname() == "HRADIOBUTTONGEN"
         DlgRadio( oCtrl )
      ELSEIF oCtrl:classname() == "HGROUP"
         DlgGroup( oCtrl )
      ENDIF
   ENDIF
Return Nil

Static Function DlgStatic( oCtrl )
Local oDlg, oEdit, oFont := HFont():Add( "MS Sans Serif",0,-13 )
Local cTitle := oCtrl:title
Local i
Local aType := { { SS_LEFT,1 }, { SS_CENTER,2 }, { SS_RIGHT,3 },              ;
           { SS_BLACKFRAME,4 }, { SS_GRAYFRAME,5 }, { SS_WHITEFRAME,6 },      ;
           { SS_BLACKRECT,7 }, { SS_GRAYRECT,8 }, { SS_WHITERECT,9 },         ;
           { SS_ETCHEDFRAME,10 }, { SS_ETCHEDHORZ,11 }, { SS_ETCHEDVERT,12 }, ;
           { SS_OWNERDRAW,13 } }
Local nType
Local nStyle
Local tcolor := Iif( oCtrl:tcolor==Nil,0,oCtrl:tcolor )
Local bcolor := Iif( oCtrl:bcolor==Nil,GetSysColor( COLOR_3DFACE ),oCtrl:bcolor )
Local lRedraw := .F.
Local oCtrlFont, oSayF

   nStyle := Hwg_BitAnd( oCtrl:style,31 )
   nType := Iif( ( i:=Ascan( aType, {|a|a[1]==nStyle} ) ) != 0, aType[i,2], 1 )

   INIT DIALOG oDlg TITLE "Static control Properties"  ;
   AT 210,10  SIZE 400,320                             ;
   FONT oFont

   @ 10,10 SAY "Text:" SIZE 80, 22
   @ 10,32 GET oEdit VAR cTitle       ;
        STYLE WS_BORDER     ;
        SIZE 220, 26        ;
        COLOR tcolor        ;
        BACKCOLOR bcolor

   @ 10,70  BUTTON "Text color" SIZE 110, 24 ;
     ON CLICK {||Iif((i:=Hwg_ChooseColor(tcolor))!=Nil,(tcolor:=i,oEdit:Setcolor(tcolor,,.T.)),.T. ) }
   @ 130,70 BUTTON "Background" SIZE 110, 24 ;
     ON CLICK {||Iif((i:=Hwg_ChooseColor(bcolor))!=Nil,(bcolor:=i,oEdit:Setcolor(,bcolor,.T.)),.T. ) }

   @ 10, 100 GROUPBOX "Font" SIZE 120,90
   @ 25, 115 SAY oSayF CAPTION "" SIZE 90,22
   @ 25, 150 BUTTON "Change"  SIZE 90, 24 ;
        ON CLICK {||ChangeCtrlFont(oSayF,@oCtrlFont)}

   @ 260,10 GROUPBOX "" SIZE 120,310
   GET RADIOGROUP nType
   @ 270,30  RADIOBUTTON "Left text"   SIZE 90, 22
   @ 270,52  RADIOBUTTON "Center text" SIZE 90, 22
   @ 270,74  RADIOBUTTON "Right text"  SIZE 90, 22
   @ 270,96  RADIOBUTTON "Blackframe"  SIZE 100, 22
   @ 270,118 RADIOBUTTON "Grayframe"   SIZE 100, 22
   @ 270,140 RADIOBUTTON "Whiteframe"  SIZE 100, 22
   @ 270,162 RADIOBUTTON "Blackrect"   SIZE 100, 22
   @ 270,184 RADIOBUTTON "Grayrect"    SIZE 100, 22
   @ 270,206 RADIOBUTTON "Whiterect"   SIZE 100, 22
   @ 270,228 RADIOBUTTON "Etchedframe" SIZE 100, 22
   @ 270,250 RADIOBUTTON "Etchedhorz"  SIZE 100, 22
   @ 270,272 RADIOBUTTON "Etchedvert"  SIZE 100, 22
   @ 270,294 RADIOBUTTON "Owner Draw"  SIZE 90, 22
   END RADIOGROUP


   @ 20,280 BUTTON "Ok" ID IDOK  SIZE 100, 32
   @ 140,280 BUTTON "Cancel" ID IDCANCEL  SIZE 100, 32

   ACTIVATE DIALOG oDlg
   oFont:Release()

   IF oDlg:lResult
      IF ! ( oCtrl:title == cTitle )
         oCtrl:title := cTitle
         SetDlgItemText( oCtrl:oParent:handle, oCtrl:id, oCtrl:title )
      ENDIF

      IF tcolor != oCtrl:tcolor .AND. ( oCtrl:tcolor != Nil .OR. tcolor != 0 )
         oCtrl:tcolor := tcolor
         lRedraw := .T.
      ENDIF
      IF bcolor != oCtrl:bcolor .AND. ( oCtrl:bcolor != Nil .OR. bcolor != GetSysColor( COLOR_3DFACE ) )
         oCtrl:bcolor := bcolor
         lRedraw := .T.
      ENDIF
      IF lRedraw
         oCtrl:SetColor( tcolor,bcolor )
      ENDIF
      IF oCtrlFont != Nil
         oCtrl:oFont := oCtrlFont
         SetCtrlFont( oCtrl:oParent:handle, oCtrl:id, oCtrlFont:handle )
         lRedraw := .T.
      ENDIF

      i := Ascan( aType,{|a|a[2]==nType} )
      nStyle := Iif( i>0,aType[i,1],0 )
      IF oCtrl:style != nStyle
         oCtrl:style := nStyle + WS_VISIBLE + WS_CHILD
         Hwg_SetWindowStyle( oCtrl:handle,oCtrl:style )
         lRedraw := .T.
      ENDIF
      IF lRedraw
         RedrawWindow( oCtrl:handle, RDW_ERASE + RDW_INVALIDATE )
      ENDIF
      oCtrl:oParent:oParent:lChanged := .T.
   ENDIF
Return Nil

Static Function ChangeCtrlFont( oText,oCtrlFont )
Local oFont := HFont():Select()
   IF oFont != Nil
      oCtrlFont := oFont
      SetDlgItemText( oText:oParent:handle, oText:id, oFont:name+","+Ltrim(Str(oFont:width))+","+Ltrim(Str(oFont:height)) )
   ENDIF
Return Nil

Static Function DlgEdit( oCtrl )
Local oDlg, oEdit, oFont := HFont():Add( "MS Sans Serif",0,-13 )
Local cTitle := oCtrl:title
Local tcolor := Iif( oCtrl:tcolor==Nil,0,oCtrl:tcolor )
Local bcolor := Iif( oCtrl:bcolor==Nil,GetSysColor( COLOR_BTNHIGHLIGHT ),oCtrl:bcolor )
Local lRedraw := .F.
Local oCtrlFont, oSayF
Local nStyle := oCtrl:Style
Local nAlign := Hwg_BitAnd( nStyle,3 ) + 1
Local lMulti := ( Hwg_BitAnd( nStyle,ES_MULTILINE ) > 0 )
Local lBorder := ( Hwg_BitAnd( nStyle,WS_BORDER ) > 0 )
Local lDlgframe := ( Hwg_BitAnd( nStyle,WS_DLGFRAME ) > 0 )
Local lHScroll := ( Hwg_BitAnd( nStyle,WS_HSCROLL ) > 0 )
Local lVScroll := ( Hwg_BitAnd( nStyle,WS_VSCROLL ) > 0 )
Local lAutoH := ( Hwg_BitAnd( nStyle,ES_AUTOHSCROLL ) > 0 )
Local lAutoV := ( Hwg_BitAnd( nStyle,ES_AUTOVSCROLL ) > 0 )

   INIT DIALOG oDlg TITLE "Edit control Properties"  ;
   AT 210,10  SIZE 400,320                         ;
   FONT oFont

   @ 10,10 SAY "Caption:" SIZE 80, 22
   @ 10,32 GET oEdit VAR cTitle  ;
        STYLE WS_BORDER          ;
        SIZE 220, 26

   @ 10,70  BUTTON "Text color" SIZE 110, 24 ;
     ON CLICK {||Iif((i:=Hwg_ChooseColor(tcolor))!=Nil,(tcolor:=i,oEdit:Setcolor(tcolor,,.T.)),.T. ) }
   @ 130,70 BUTTON "Background" SIZE 110, 24 ;
     ON CLICK {||Iif((i:=Hwg_ChooseColor(bcolor))!=Nil,(bcolor:=i,oEdit:Setcolor(,bcolor,.T.)),.T. ) }

   @ 10, 100 GROUPBOX "Font" SIZE 115,90
   @ 25, 115 SAY oSayF CAPTION "" SIZE 90,22
   @ 25, 150 BUTTON "Change"  SIZE 90, 24 ;
        ON CLICK {||ChangeCtrlFont(oSayF,@oCtrlFont)}

   @ 135, 100 GROUPBOX "Alignment" SIZE 115,90
   GET RADIOGROUP nAlign
   @ 145,115  RADIOBUTTON "Left"   SIZE 90, 22
   @ 145,138  RADIOBUTTON "Center" SIZE 90, 22
   @ 145,161  RADIOBUTTON "Right"  SIZE 90, 22
   END RADIOGROUP

   @ 260,10 GROUPBOX "" SIZE 120,90
   @ 270,25 GET CHECKBOX lMulti CAPTION "Multiline" SIZE 100,22
   @ 270,48 GET CHECKBOX lBorder CAPTION "Border" SIZE 100,22
   @ 270,71 GET CHECKBOX lDlgFrame CAPTION "Dialog Frame" SIZE 100,22

   @ 260,110 GROUPBOX "Scroll bar" SIZE 120,65
   @ 270,125 GET CHECKBOX lHscroll CAPTION "Horizontal" SIZE 100,22
   @ 270,148 GET CHECKBOX lVscroll CAPTION "Vertical" SIZE 100,22

   @ 260,185 GROUPBOX "Auto scroll" SIZE 120,65
   @ 270,200 GET CHECKBOX lAutoH CAPTION "Horizontal" SIZE 100,22
   @ 270,223 GET CHECKBOX lAutoV CAPTION "Vertical" SIZE 100,22

   @ 20,280 BUTTON "Ok" ID IDOK  SIZE 100, 32
   @ 140,280 BUTTON "Cancel" ID IDCANCEL  SIZE 100, 32

   ACTIVATE DIALOG oDlg
   oFont:Release()

   IF oDlg:lResult

      IF ! ( oCtrl:title == cTitle )
         oCtrl:title := cTitle
         SetDlgItemText( oCtrl:oParent:handle, oCtrl:id, oCtrl:title )
      ENDIF
      IF tcolor != oCtrl:tcolor .AND. ( oCtrl:tcolor != Nil .OR. tcolor != 0 )
         oCtrl:tcolor := tcolor
         lRedraw := .T.
      ENDIF
      IF bcolor != oCtrl:bcolor .AND. ( oCtrl:bcolor != Nil .OR. bcolor != GetSysColor( COLOR_BTNHIGHLIGHT ) )
         oCtrl:bcolor := bcolor
         lRedraw := .T.
      ENDIF
      IF lRedraw
         oCtrl:SetColor( tcolor,bcolor )
      ENDIF
      IF oCtrlFont != Nil
         oCtrl:oFont := oCtrlFont
         SetCtrlFont( oCtrl:oParent:handle, oCtrl:id, oCtrlFont:handle )
         lRedraw := .T.
      ENDIF

      nStyle := Hwg_BitOr( Hwg_BitAndInverse(nStyle,3), nAlign-1 )
      nStyle := Hwg_BitOr( Hwg_BitAndInverse(nStyle,ES_MULTILINE), ;
                                            Iif(lMulti,ES_MULTILINE,0) )
      nStyle := Hwg_BitOr( Hwg_BitAndInverse(nStyle,WS_BORDER), ;
                                            Iif(lBorder,WS_BORDER,0) )
      nStyle := Hwg_BitOr( Hwg_BitAndInverse(nStyle,WS_DLGFRAME), ;
                                            Iif(lDlgframe,WS_DLGFRAME,0) )
      nStyle := Hwg_BitOr( Hwg_BitAndInverse(nStyle,WS_HSCROLL), ;
                                            Iif(lHscroll,WS_HSCROLL,0) )
      nStyle := Hwg_BitOr( Hwg_BitAndInverse(nStyle,WS_VSCROLL), ;
                                            Iif(lVscroll,WS_VSCROLL,0) )
      nStyle := Hwg_BitOr( Hwg_BitAndInverse(nStyle,ES_AUTOHSCROLL), ;
                                            Iif(lAutoH,ES_AUTOHSCROLL,0) )
      nStyle := Hwg_BitOr( Hwg_BitAndInverse(nStyle,ES_AUTOVSCROLL), ;
                                            Iif(lAutoV,ES_AUTOVSCROLL,0) )
      IF nStyle != oCtrl:style
         oCtrl:style := nStyle
         Hwg_SetWindowStyle( oCtrl:handle,oCtrl:style )
         lRedraw := .T.
      ENDIF

      IF lRedraw
         RedrawWindow( oCtrl:handle, RDW_ERASE + RDW_INVALIDATE )
      ENDIF
      oCtrl:oParent:oParent:lChanged := .T.
   ENDIF
Return Nil

Static Function DlgButton( oCtrl )
Local oDlg, oFont := HFont():Add( "MS Sans Serif",0,-13 )
Local cTitle := oCtrl:title
Local nIdType := Iif(oCtrl:id==IDOK,1,Iif(oCtrl:id==IDCANCEL,2,3))
Local lDef := ( Hwg_BitAnd(oCtrl:style,BS_DEFPUSHBUTTON) > 0 )
Local lRedraw := .F.
Local oCtrlFont, oSayF

   INIT DIALOG oDlg TITLE "PushButton Properties"  ;
   AT 210,10  SIZE 400,300                         ;
   FONT oFont

   @ 10,10 SAY "Caption:" SIZE 80, 22
   @ 10,32 GET cTitle       ;
        STYLE WS_BORDER     ;
        SIZE 220, 26

   @ 10, 70 GROUPBOX "Font" SIZE 120,95
   @ 25, 85 SAY oSayF CAPTION "" SIZE 90,22
   @ 25, 120 BUTTON "Change"  SIZE 90, 24 ;
        ON CLICK {||ChangeCtrlFont(oSayF,@oCtrlFont)}

   @ 140, 70 GROUPBOX "Id" SIZE 120,95
   GET RADIOGROUP nIdType
   @ 155,85  RADIOBUTTON "Ok Button" SIZE 100,22
   @ 155,110 RADIOBUTTON "Cancel Button" SIZE 100,22
   @ 155,135 RADIOBUTTON "Other" SIZE 100,22
   END RADIOGROUP

   @ 10,180 GET CHECKBOX lDef CAPTION "Default Push Button" SIZE 140,22

   @ 20,240 BUTTON "Ok" ID IDOK  SIZE 100, 32
   @ 140,240 BUTTON "Cancel" ID IDCANCEL  SIZE 100, 32

   ACTIVATE DIALOG oDlg
   oFont:Release()

   IF oDlg:lResult
      IF ! ( oCtrl:title == cTitle )
         oCtrl:title := cTitle
         SetDlgItemText( oCtrl:oParent:handle, oCtrl:id, oCtrl:title )
      ENDIF
      IF oCtrlFont != Nil
         oCtrl:oFont := oCtrlFont
         SetCtrlFont( oCtrl:oParent:handle, oCtrl:id, oCtrlFont:handle )
         lRedraw := .T.
      ENDIF
      IF lDef
         oCtrl:style := Hwg_BitOr( oCtrl:style,BS_DEFPUSHBUTTON )
      ENDIF
      IF nIdType == 1
         oCtrl:id := IDOK
      ELSEIF nIdType == 2
         oCtrl:id := IDCANCEL
      ELSEIF nIdType == 3 .AND. ( oCtrl:id == IDOK .OR. oCtrl:id == IDCANCEL )
         oCtrl:id := 0
      ENDIF

      IF lRedraw
         RedrawWindow( oCtrl:handle, RDW_ERASE + RDW_INVALIDATE )
      ENDIF
      oCtrl:oParent:oParent:lChanged := .T.
   ENDIF
Return Nil

Static Function DlgGroup( oCtrl )
Local oDlg, oFont := HFont():Add( "MS Sans Serif",0,-13 ), oEdit
Local cTitle := oCtrl:title
Local tcolor := Iif( oCtrl:tcolor==Nil,0,oCtrl:tcolor )
Local bcolor := Iif( oCtrl:bcolor==Nil,GetSysColor( COLOR_3DFACE ),oCtrl:bcolor )
Local lRedraw := .F.
Local oCtrlFont, oSayF

   INIT DIALOG oDlg TITLE "Group control Properties"  ;
   AT 210,10  SIZE 300,220                            ;
   FONT oFont

   @ 10,10 SAY "Caption:" SIZE 80, 22
   @ 10,32 GET oEdit VAR cTitle       ;
        STYLE WS_BORDER     ;
        SIZE 220, 26        ;
        COLOR tcolor        ;
        BACKCOLOR bcolor

   @ 10,70  BUTTON "Text color" SIZE 110, 24 ;
     ON CLICK {||Iif((i:=Hwg_ChooseColor(tcolor))!=Nil,(tcolor:=i,oEdit:Setcolor(tcolor,,.T.)),.T. ) }
   @ 130,70 BUTTON "Background" SIZE 110, 24 ;
     ON CLICK {||Iif((i:=Hwg_ChooseColor(bcolor))!=Nil,(bcolor:=i,oEdit:Setcolor(,bcolor,.T.)),.T. ) }

   @ 10, 100 GROUPBOX "Font" SIZE 120,90
   @ 25, 115 SAY oSayF CAPTION "" SIZE 90,22
   @ 25, 150 BUTTON "Change"  SIZE 90, 24 ;
        ON CLICK {||ChangeCtrlFont(oSayF,@oCtrlFont)}

   @ 20,180 BUTTON "Ok" ID IDOK  SIZE 100, 32
   @ 140,180 BUTTON "Cancel" ID IDCANCEL  SIZE 100, 32

   ACTIVATE DIALOG oDlg
   oFont:Release()

   IF oDlg:lResult
      IF ! ( oCtrl:title == cTitle )
         oCtrl:title := cTitle
         SetDlgItemText( oCtrl:oParent:handle, oCtrl:id, oCtrl:title )
      ENDIF
      IF tcolor != oCtrl:tcolor .AND. ( oCtrl:tcolor != Nil .OR. tcolor != 0 )
         oCtrl:tcolor := tcolor
         lRedraw := .T.
      ENDIF
      IF bcolor != oCtrl:bcolor .AND. ( oCtrl:bcolor != Nil .OR. bcolor != GetSysColor( COLOR_3DFACE ) )
         oCtrl:bcolor := bcolor
         lRedraw := .T.
      ENDIF
      IF oCtrlFont != Nil
         oCtrl:oFont := oCtrlFont
         SetCtrlFont( oCtrl:oParent:handle, oCtrl:id, oCtrlFont:handle )
         lRedraw := .T.
      ENDIF

      IF lRedraw
         oCtrl:SetColor( tcolor,bcolor )
      ENDIF
      IF lRedraw
         RedrawWindow( oCtrl:handle, RDW_ERASE + RDW_INVALIDATE )
      ENDIF
      oCtrl:oParent:oParent:lChanged := .T.
   ENDIF
Return Nil

Static Function DlgCheck( oCtrl )
Local oDlg, oFont := HFont():Add( "MS Sans Serif",0,-13 ), oEdit
Local cTitle := oCtrl:title
Local tcolor := Iif( oCtrl:tcolor==Nil,0,oCtrl:tcolor )
Local bcolor := Iif( oCtrl:bcolor==Nil,GetSysColor( COLOR_3DFACE ),oCtrl:bcolor )
Local lRedraw := .F.
Local oCtrlFont, oSayF

   INIT DIALOG oDlg TITLE "Checkbox Properties"  ;
   AT 210,10  SIZE 300,250                       ;
   FONT oFont

   @ 10,10 SAY "Caption:" SIZE 80, 22
   @ 10,32 GET oEdit VAR cTitle       ;
        STYLE WS_BORDER     ;
        SIZE 220, 26        ;
        COLOR tcolor        ;
        BACKCOLOR bcolor

   @ 10,70  BUTTON "Text color" SIZE 110, 24 ;
     ON CLICK {||Iif((i:=Hwg_ChooseColor(tcolor))!=Nil,(tcolor:=i,oEdit:Setcolor(tcolor,,.T.)),.T. ) }
   @ 130,70 BUTTON "Background" SIZE 110, 24 ;
     ON CLICK {||Iif((i:=Hwg_ChooseColor(bcolor))!=Nil,(bcolor:=i,oEdit:Setcolor(,bcolor,.T.)),.T. ) }

   @ 10, 100 GROUPBOX "Font" SIZE 120,90
   @ 25, 115 SAY oSayF CAPTION "" SIZE 90,22
   @ 25, 150 BUTTON "Change"  SIZE 90, 24 ;
        ON CLICK {||ChangeCtrlFont(oSayF,@oCtrlFont)}


   @ 20,210 BUTTON "Ok" ID IDOK  SIZE 100, 32
   @ 140,210 BUTTON "Cancel" ID IDCANCEL  SIZE 100, 32

   ACTIVATE DIALOG oDlg
   oFont:Release()

   IF oDlg:lResult
      IF ! ( oCtrl:title == cTitle )
         oCtrl:title := cTitle
         SetDlgItemText( oCtrl:oParent:handle, oCtrl:id, oCtrl:title )
      ENDIF
      IF tcolor != oCtrl:tcolor .AND. ( oCtrl:tcolor != Nil .OR. tcolor != 0 )
         oCtrl:tcolor := tcolor
         lRedraw := .T.
      ENDIF
      IF bcolor != oCtrl:bcolor .AND. ( oCtrl:bcolor != Nil .OR. bcolor != GetSysColor( COLOR_3DFACE ) )
         oCtrl:bcolor := bcolor
         lRedraw := .T.
      ENDIF
      IF oCtrlFont != Nil
         oCtrl:oFont := oCtrlFont
         SetCtrlFont( oCtrl:oParent:handle, oCtrl:id, oCtrlFont:handle )
         lRedraw := .T.
      ENDIF

      IF lRedraw
         oCtrl:SetColor( tcolor,bcolor )
      ENDIF

      IF lRedraw
         RedrawWindow( oCtrl:handle, RDW_ERASE + RDW_INVALIDATE )
      ENDIF
      oCtrl:oParent:oParent:lChanged := .T.
   ENDIF
Return Nil

Static Function DlgRadio( oCtrl )
Local oDlg, oFont := HFont():Add( "MS Sans Serif",0,-13 ), oEdit
Local cTitle := oCtrl:title
Local tcolor := Iif( oCtrl:tcolor==Nil,0,oCtrl:tcolor )
Local bcolor := Iif( oCtrl:bcolor==Nil,GetSysColor( COLOR_3DFACE ),oCtrl:bcolor )
Local lRedraw := .F.
Local oCtrlFont, oSayF

   INIT DIALOG oDlg TITLE "Radiobutton Properties"  ;
   AT 210,10  SIZE 300,250                          ;
   FONT oFont

   @ 10,10 SAY "Caption:" SIZE 80, 22
   @ 10,32 GET oEdit VAR cTitle       ;
        STYLE WS_BORDER     ;
        SIZE 220, 26        ;
        COLOR tcolor        ;
        BACKCOLOR bcolor

   @ 10,70  BUTTON "Text color" SIZE 110, 24 ;
     ON CLICK {||Iif((i:=Hwg_ChooseColor(tcolor))!=Nil,(tcolor:=i,oEdit:Setcolor(tcolor,,.T.)),.T. ) }
   @ 130,70 BUTTON "Background" SIZE 110, 24 ;
     ON CLICK {||Iif((i:=Hwg_ChooseColor(bcolor))!=Nil,(bcolor:=i,oEdit:Setcolor(,bcolor,.T.)),.T. ) }

   @ 10, 100 GROUPBOX "Font" SIZE 120,90
   @ 25, 115 SAY oSayF CAPTION "" SIZE 90,22
   @ 25, 150 BUTTON "Change"  SIZE 90, 24 ;
        ON CLICK {||ChangeCtrlFont(oSayF,@oCtrlFont)}


   @ 20,210 BUTTON "Ok" ID IDOK  SIZE 100, 32
   @ 140,210 BUTTON "Cancel" ID IDCANCEL  SIZE 100, 32

   ACTIVATE DIALOG oDlg
   oFont:Release()

   IF oDlg:lResult
      IF ! ( oCtrl:title == cTitle )
         oCtrl:title := cTitle
         SetDlgItemText( oCtrl:oParent:handle, oCtrl:id, oCtrl:title )
      ENDIF
      IF tcolor != oCtrl:tcolor .AND. ( oCtrl:tcolor != Nil .OR. tcolor != 0 )
         oCtrl:tcolor := tcolor
         lRedraw := .T.
      ENDIF
      IF bcolor != oCtrl:bcolor .AND. ( oCtrl:bcolor != Nil .OR. bcolor != GetSysColor( COLOR_3DFACE ) )
         oCtrl:bcolor := bcolor
         lRedraw := .T.
      ENDIF
      IF oCtrlFont != Nil
         oCtrl:oFont := oCtrlFont
         SetCtrlFont( oCtrl:oParent:handle, oCtrl:id, oCtrlFont:handle )
         lRedraw := .T.
      ENDIF

      IF lRedraw
         oCtrl:SetColor( tcolor,bcolor )
      ENDIF

      IF lRedraw
         RedrawWindow( oCtrl:handle, RDW_ERASE + RDW_INVALIDATE )
      ENDIF
      oCtrl:oParent:oParent:lChanged := .T.
   ENDIF
Return Nil

Function DlgStyle( oFrm )
Local oDlg, oFont := HFont():Add( "MS Sans Serif",0,-13 ), oEdit, oSayF
Local cTitle := oFrm:name
Local lRedraw := .F.
Local oDlgFont, i

   INIT DIALOG oDlg TITLE cTitle+" Properties" ;
   AT 10,10  SIZE 300,260                      ;
   FONT oFont

   @ 10,10 SAY "Caption:" SIZE 80, 22
   @ 10,32 GET oEdit VAR cTitle  ;
        STYLE WS_BORDER          ;
        SIZE 220, 26

   @ 10, 100 GROUPBOX "Font" SIZE 120,90
   @ 25, 115 SAY oSayF CAPTION "" SIZE 90,22
   @ 25, 150 BUTTON "Change"  SIZE 90, 24 ;
        ON CLICK {||ChangeCtrlFont(oSayF,@oDlgFont)}

   @ 20,210 BUTTON "Ok" ID IDOK  SIZE 100, 32
   @ 140,210 BUTTON "Cancel" ID IDCANCEL  SIZE 100, 32

   ACTIVATE DIALOG oDlg
   oFont:Release()

   IF oDlg:lResult
      IF oFrm:name != cTitle
         oFrm:name := cTitle
         SetWindowText( oFrm:oDlg:handle, cTitle )
      ENDIF
      IF oDlgFont != Nil
         oFrm:oDlg:oFont := oDlgFont
         FOR i := 1 TO Len( oFrm:oDlg:aControls )
            SetCtrlFont( oFrm:oDlg:handle, oFrm:oDlg:aControls[i]:id, oDlgFont:handle )
         NEXT
         lRedraw := .T.
      ENDIF

      IF lRedraw
         RedrawWindow( oFrm:oDlg:handle, RDW_ERASE + RDW_INVALIDATE )
      ENDIF
      oFrm:lChanged := .T.
   ENDIF

Return Nil

Function DlgExten( oFrm )
Local oDlg, oFont := HFont():Add( "MS Sans Serif",0,-13 )
Local cTitle := oFrm:name
Local oEdit1, oEdit2
Local lGet := oFrm:lGet, lClipper := oFrm:oDlg:lClipper
Local cScript1 := oFrm:script1, cScript2 := oFrm:script2

   INIT DIALOG oDlg TITLE cTitle+" Extended Properties"  ;
   AT 10,10  SIZE 400,370  ;
   FONT oFont

   @ 10,10 GET CHECKBOX lGet CAPTION "Use Get System" SIZE 120, 24
   @ 10,35 GET CHECKBOX lClipper CAPTION "Clipper style" SIZE 100, 24

   @ 10,70 SAY "Initial script:" SIZE 80, 22
   @ 10,92 GET cScript1  ;
        STYLE WS_BORDER+ES_MULTILINE+ES_AUTOVSCROLL+ES_AUTOHSCROLL ;
        SIZE 380, 120

   @ 10,220 SAY "End script:" SIZE 80, 22
   @ 10,242 GET cScript2 ;
        STYLE WS_BORDER+ES_MULTILINE+ES_AUTOVSCROLL+ES_AUTOHSCROLL ;
        SIZE 380, 120


   @ 240,10 BUTTON "Ok" ID IDOK  SIZE 100, 32
   @ 240,50 BUTTON "Cancel" ID IDCANCEL  SIZE 100, 32

   ACTIVATE DIALOG oDlg
   oFont:Release()

   IF oDlg:lResult
      oFrm:lGet := lGet
      oFrm:oDlg:lClipper := lClipper
      oFrm:script1 := Ltrim( Rtrim( cScript1 ) )
      oFrm:script2 := Ltrim( Rtrim( cScript2 ) )
      oFrm:lChanged := .T.
   ENDIF

Return Nil

Function SetCtrlExt( oCtrl )
Local oDlg, oFont, oEdit1, oEdit2
Local cObjName := oCtrl:Cargo[1], lObjName := oCtrl:Cargo[2]
Local cVarName := oCtrl:Cargo[3], lVarName := oCtrl:Cargo[4]
Local cInit  := oCtrl:Cargo[5]
Local cValid := oCtrl:Cargo[6]

   IF oCtrl == Nil
      Return Nil
   ENDIF

   oFont := HFont():Add( "MS Sans Serif",0,-13 )

   INIT DIALOG oDlg TITLE "Scripts"  ;
   AT 60,10  SIZE 400,270            ;
   FONT oFont

   @ 10, 10 SAY "Object name" SIZE 80, 22
   @ 95, 10 GET oEdit1 VAR cObjName  STYLE WS_BORDER  SIZE 80, 26
   @ 190,10 GET CHECKBOX lObjName CAPTION "Use it" SIZE 80, 26 ;
      VALID {|l|EnableWindow(oEdit1:handle,l),.T.}

   IF oCtrl:oParent:oParent:lGet .AND. __ObjHasMsg( oCtrl,"BSETGET" )
      @ 10, 45 SAY "Var name" SIZE 80, 22
      @ 95, 45 GET oEdit2 VAR cVarName  STYLE WS_BORDER  SIZE 80, 26
      @ 190,45 GET CHECKBOX lVarName CAPTION "Declare it" SIZE 80, 26

      @ 10, 80 SAY "Initialize" SIZE 80, 22
      @ 95, 80 GET cInit  STYLE WS_BORDER  SIZE 290, 26

      @ 10, 115 SAY "Valid" SIZE 80, 22
      @ 95, 115 GET cValid  STYLE WS_BORDER  SIZE 290, 26
   ENDIF

   @ 20,210 BUTTON "Ok" ID IDOK  SIZE 100, 32
   @ 140,210 BUTTON "Cancel" ID IDCANCEL  SIZE 100, 32

   ACTIVATE DIALOG oDlg
   oFont:Release()

   IF oDlg:lResult
      oCtrl:Cargo[1] := cObjName
      oCtrl:Cargo[2] := lObjName
      oCtrl:Cargo[3] := cVarName
      oCtrl:Cargo[4] := lVarName
      oCtrl:Cargo[5] := cInit
      oCtrl:Cargo[6] := cValid

      oCtrl:oParent:oParent:lChanged := .T.
   ENDIF
Return Nil
