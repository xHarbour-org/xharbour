/*
 *$Id: guilib.ch,v 1.10 2004/02/18 11:32:24 lculik Exp $
 */

#include "guilib.h"

// Acrescentado por jamaj
#ifndef WINCOMMCTRLAPI
#include "commctrl.ch"
#endif


// Commands for windows, dialogs handling
// Alterado por jamaj - CHILD window clause
#xcommand INIT WINDOW <oWnd>                ;
             [ MAIN ]                       ;
             [<lMdi: MDI>]                  ;
             [<lMdiChild: MDICHILD>]           ;
             [<lChild: CHILD>]        ;
             [ APPNAME <appname> ]          ;
             [ TITLE <cTitle> ]             ;
             [ AT <x>, <y> ]                ;
             [ SIZE <width>, <height> ]     ;
             [ ICON <ico> ]                 ;
             [ COLOR <clr> ]                ;
             [ BACKGROUND BITMAP <oBmp> ]   ;
             [ STYLE <nStyle> ]             ;
             [ FONT <oFont> ]               ;
             [ MENU <cMenu> ]               ;
             [ MENUPOS <nPos> ]             ;
             [ ON INIT <bInit> ]            ;
             [ ON SIZE <bSize> ]            ;
             [ ON PAINT <bPaint> ]          ;
             [ ON GETFOCUS <bGfocus> ]      ;
             [ ON LOSTFOCUS <bLfocus> ]     ;
             [ ON OTHER MESSAGES <bOther> ] ;
             [ ON EXIT <bExit> ]            ;
             [<lMaximize: MAXIMIZE>]        ;
          => ;
   <oWnd> := HWindow():New( Iif(<.lMdi.>,WND_MDI,Iif(<.lMdiChild.>,WND_MDICHILD, Iif(<.lChild.>,WND_CHILD,WND_MAIN) )), ;
                   <ico>,<clr>,<nStyle>,<x>,<y>,<width>,<height>,<cTitle>, ;
                   <cMenu>,<nPos>,<oFont>,<bInit>,<bExit>, ;
                   <bSize>, <bPaint>,<bGfocus>,<bLfocus>,<bOther>,<appname>,<oBmp>,<.lMaximize.>)

#xcommand INIT DIALOG <oDlg>                ;
             [<res: FROM RESOURCE>]         ;
             [ TITLE <cTitle> ]             ;
             [ AT <x>, <y> ]                ;
             [ SIZE <width>, <height> ]     ;
             [ ICON <ico> ]                 ;
             [ BACKGROUND BITMAP <oBmp> ]   ;
             [ STYLE <nStyle> ]             ;
             [ FONT <oFont> ]               ;
             [<lClipper: CLIPPER>]          ;
             [<lExitOnEnter: NOEXIT>]       ; //Modified By Sandro
             [ ON INIT <bInit> ]            ;
             [ ON SIZE <bSize> ]            ;
             [ ON PAINT <bPaint> ]          ;
             [ ON GETFOCUS <bGfocus> ]      ;
             [ ON LOSTFOCUS <bLfocus> ]     ;
             [ ON OTHER MESSAGES <bOther> ] ;
             [ ON EXIT <bExit> ]            ;
          => ;
   <oDlg> := HDialog():New( Iif(<.res.>,WND_DLG_RESOURCE,WND_DLG_NORESOURCE), ;
                   <nStyle>,<x>,<y>,<width>,<height>,<cTitle>,<oFont>,<bInit>,<bExit>,;
                   <bSize>, <bPaint>,<bGfocus>,<bLfocus>,<bOther>,<.lClipper.>,<oBmp>,<ico>,<.lExitOnEnter.> )

#xcommand ACTIVATE WINDOW <oWnd> [<lNoShow: NOSHOW>]     ;
           => ;
      <oWnd>:Activate( !<.lNoShow.>) 
                                                        
  
#xcommand ACTIVATE DIALOG <oDlg>                        ;
            [ <lNoModal: NOMODAL> ]                     ;
          => ;
      <oDlg>:Activate( <.lNoModal.> )

#xcommand MENU FROM RESOURCE OF <oWnd> ON <id1> ACTION <b1>      ;
                                 [ ON <idn> ACTION <bn> ]    ;
          => ;
   <oWnd>:aEvents := \{ \{ 0,<id1>, <{b1}> \} [ , \{ 0,<idn>, <{bn}> \} ] \}

#xcommand DIALOG ACTIONS OF <oWnd> ON <id1>,<id2> ACTION <b1>      ;
                                 [ ON <idn1>,<idn2> ACTION <bn> ]  ;
          => ;
   <oWnd>:aEvents := \{ \{ <id1>,<id2>, <b1> \} [ , \{ <idn1>,<idn2>, <bn> \} ] \}


// Commands for control handling

#xcommand ADD STATUS [ TO <oWnd> ] ;
            [ ID <nId> ]           ;
            [ ON INIT <bInit> ]    ;
            [ ON SIZE <bSize> ]    ;
            [ ON PAINT <bDraw> ]   ;
            [ STYLE <nStyle> ]     ;
            [ FONT <oFont> ]       ;
            [ PARTS <aparts,...> ] ;
          => ;
    HStatus():New( <oWnd>,<nId>,<nStyle>,<oFont>,\{<aparts>\},<bInit>,<bSize>,<bDraw> )


#xcommand @ <x>,<y> SAY [ <oSay> CAPTION ] <caption> ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [<lTransp: TRANSPARENT>]   ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ STYLE <nStyle> ]         ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oSay> := ] HStatic():New( <oWnd>,<nId>,<nStyle>,<x>,<y>,<width>, ;
        <height>,<caption>,<oFont>,<bInit>,<bSize>,<bDraw>,<ctoolt>,<color>,<bcolor>,<.lTransp.> )

#xcommand REDEFINE SAY [ <oSay> CAPTION ] <cCaption>      ;
            [ OF <oWnd> ]              ;
            ID <nId>                   ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [<lTransp: TRANSPARENT>]   ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oSay> := ] HStatic():Redefine( <oWnd>,<nId>,<cCaption>, ;
        <oFont>,<bInit>,<bSize>,<bDraw>,<ctoolt>,<color>,<bcolor>,<.lTransp.> )


#xcommand @ <x>,<y> BITMAP [ <oBmp> SHOW ] <bitmap> ;
            [<res: FROM RESOURCE>]     ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oBmp> := ] HSayBmp():New( <oWnd>,<nId>,<x>,<y>,<width>, ;
        <height>,<bitmap>,<.res.>,<bInit>,<bSize>,<ctoolt> )

#xcommand REDEFINE BITMAP [ <oBmp> SHOW ] <bitmap> ;
            [<res: FROM RESOURCE>]     ;
            [ OF <oWnd> ]              ;
            ID <nId>                   ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oBmp> := ] HSayBmp():Redefine( <oWnd>,<nId>,<bitmap>,<.res.>, ;
        <bInit>,<bSize>,<ctoolt> )

#xcommand @ <x>,<y> ICON [ <oIco> SHOW ] <icon> ;
            [<res: FROM RESOURCE>]     ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oIco> := ] HSayIcon():New( <oWnd>,<nId>,<x>,<y>,<width>, ;
        <height>,<icon>,<.res.>,<bInit>,<bSize>,<ctoolt> )

#xcommand REDEFINE ICON [ <oIco> SHOW ] <icon> ;
            [<res: FROM RESOURCE>]     ;
            [ OF <oWnd> ]              ;
            ID <nId>                   ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oIco> := ] HSayIcon():Redefine( <oWnd>,<nId>,<icon>,<.res.>, ;
        <bInit>,<bSize>,<ctoolt> )

#xcommand @ <x>,<y> IMAGE [ <oImage> SHOW ] <image> ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oImage> := ] HSayFImage():New( <oWnd>,<nId>,<x>,<y>,<width>, ;
        <height>,<image>,<bInit>,<bSize>,<ctoolt> )

#xcommand REDEFINE IMAGE [ <oImage> SHOW ] <image> ;
            [ OF <oWnd> ]              ;
            ID <nId>                   ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oImage> := ] HSayFImage():Redefine( <oWnd>,<nId>,<image>, ;
        <bInit>,<bSize>,<ctoolt> )


#xcommand @ <x>,<y> LINE [ <oLine> ]   ;
            [ LENGTH <length> ]        ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [<lVert: VERTICAL>]        ;
            [ ON SIZE <bSize> ]        ;
          => ;
    [<oLine> := ] HLine():New( <oWnd>,<nId>,<.lVert.>,<x>,<y>,<length>,<bSize> )

#xcommand @ <x>,<y> EDITBOX [ <oEdit> CAPTION ] <caption> ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ ON GETFOCUS <bGfocus> ]  ;
            [ ON LOSTFOCUS <bLfocus> ] ;
            [ STYLE <nStyle> ]         ;
            [<lnoborder: NOBORDER>]    ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oEdit> := ] HEdit():New( <oWnd>,<nId>,<caption>,,<nStyle>,<x>,<y>,<width>, ;
                    <height>,<oFont>,<bInit>,<bSize>,<bDraw>,<bGfocus>, ;
                    <bLfocus>,<ctoolt>,<color>,<bcolor>,,<.lnoborder.> )


#xcommand REDEFINE EDITBOX [ <oEdit> ] ;
            [ OF <oWnd> ]              ;
            ID <nId>                   ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ FONT <oFont> ]           ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ ON GETFOCUS <bGfocus> ]  ;
            [ ON LOSTFOCUS <bLfocus> ] ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oEdit> := ] HEdit():Redefine( <oWnd>,<nId>,,,<oFont>,<bInit>,<bSize>,<bDraw>, ;
                   <bGfocus>,<bLfocus>,<ctoolt>,<color>,<bcolor> )

#xcommand @ <x>,<y> RICHEDIT [ <oEdit> TEXT ] <vari> ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ ON GETFOCUS <bGfocus> ]  ;
            [ ON LOSTFOCUS <bLfocus> ] ;
            [ STYLE <nStyle> ]         ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oEdit> := ] HRichEdit():New( <oWnd>,<nId>,<vari>,<nStyle>,<x>,<y>,<width>, ;
                    <height>,<oFont>,<bInit>,<bSize>,<bDraw>,<bGfocus>, ;
                    <bLfocus>,<ctoolt>,<color>,<bcolor> )


#xcommand @ <x>,<y> BUTTON [ <oBut> CAPTION ] <caption> ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ ON CLICK <bClick> ]      ;
            [ STYLE <nStyle> ]         ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oBut> := ] HButton():New( <oWnd>,<nId>,<nStyle>,<x>,<y>,<width>, ;
             <height>,<caption>,<oFont>,<bInit>,<bSize>,<bDraw>,<bClick>,<ctoolt>,<color>,<bcolor> )

#xcommand REDEFINE BUTTON [ <oBut> ]   ;
            [ OF <oWnd> ]              ;
            ID <nId>                   ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ FONT <oFont> ]           ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ ON CLICK <bClick> ]      ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oBut> := ] HButton():Redefine( <oWnd>,<nId>,<oFont>,<bInit>,<bSize>,<bDraw>, ;
                    <bClick>,<ctoolt>,<color>,<bcolor> )

#xcommand @ <x>,<y> GROUPBOX [ <oGroup> CAPTION ] <caption> ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ FONT <oFont> ]           ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ STYLE <nStyle> ]         ;
          => ;
    [<oGroup> := ] HGroup():New( <oWnd>,<nId>,<nStyle>,<x>,<y>,<width>, ;
             <height>,<caption>,<oFont>,<bInit>,<bSize>,<bDraw>,<color>,<bcolor> )

#xcommand @ <x>,<y> TREE [ <oTree> ]   ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ FONT <oFont> ]           ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ STYLE <nStyle> ]         ;
            [<lEdit: EDITABLE>]        ;
            [ BITMAP <aBmp>  [<res: FROM RESOURCE>] ]  ;
          => ;
    [<oTree> := ] HTree():New( <oWnd>,<nId>,<nStyle>,<x>,<y>,<width>, ;
             <height>,<oFont>,<bInit>,<bSize>,<color>,<bcolor>,<aBmp>,<.res.>,<.lEdit.> )

#xcommand INSERT NODE [ <oNode> CAPTION ] <cTitle>  ;
            TO <oTree>                            ;
            [ AFTER <oPrev> ]                     ;
            [ BEFORE <oNext> ]                    ;
            [ BITMAP <aBmp> ]                     ;
            [ ON CLICK <bClick> ]                 ;
          => ;
    [<oNode> := ] <oTree>:AddNode( <cTitle>,<oPrev>,<oNext>,<bClick>,<aBmp> )

#xcommand @ <x>,<y> TAB [ <oTab> ITEMS ] <aItems> ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ STYLE <nStyle> ]         ;
            [ FONT <oFont> ]           ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ ON CHANGE <bChange> ]    ;
          => ;
    [<oTab> := ] HTab():New( <oWnd>,<nId>,<nStyle>,<x>,<y>,<width>, ;
             <height>,<oFont>,<bInit>,<bSize>,<bDraw>,<aItems>,<bChange> )

#xcommand BEGIN PAGE <cname> OF <oTab> ;
          => ;
    <oTab>:StartPage( <cname> )

#xcommand END PAGE OF <oTab> ;
          => ;
    <oTab>:EndPage()

#xcommand ENDPAGE OF <oTab> ;
          => ;
    <oTab>:EndPage()


#xcommand @ <x>,<y> CHECKBOX [ <oCheck> CAPTION ] <caption> ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ INIT <lInit> ]           ;
            [ SIZE <width>, <height> ] ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ ON CLICK <bClick> ]      ;
            [ STYLE <nStyle> ]         ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oCheck> := ] HCheckButton():New( <oWnd>,<nId>,<lInit>,,<nStyle>,<x>,<y>, ;
         <width>,<height>,<caption>,<oFont>,<bInit>,<bSize>,<bDraw>,<bClick>,<ctoolt>,<color>,<bcolor> )

#xcommand REDEFINE CHECKBOX [ <oCheck> ] ;
            [ OF <oWnd> ]              ;
            ID <nId>                   ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ INIT <lInit>    ]        ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ ON CLICK <bClick> ]      ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oCheck> := ] HCheckButton():Redefine( <oWnd>,<nId>,<lInit>,,<oFont>, ;
          <bInit>,<bSize>,<bDraw>,<bClick>,<ctoolt>,<color>,<bcolor> )


#xcommand RADIOGROUP  ;
          => HRadioGroup():New()

#xcommand GET RADIOGROUP [ <ogr> VAR ] <vari>  ;
          => [<ogr> := ] HRadioGroup():New( <vari>, {|v|Iif(v==Nil,<vari>,<vari>:=v)} )

#xcommand END RADIOGROUP [ SELECTED <nSel> ] ;
          => HRadioGroup():EndGroup( <nSel> )

#xcommand @ <x>,<y> RADIOBUTTON [ <oRadio> CAPTION ] <caption> ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ ON CLICK <bClick> ]      ;
            [ STYLE <nStyle> ]         ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oRadio> := ] HRadioButton():New( <oWnd>,<nId>,<nStyle>,<x>,<y>, ;
         <width>,<height>,<caption>,<oFont>,<bInit>,<bSize>,<bDraw>,<bClick>,<ctoolt>,<color>,<bcolor> )

#xcommand REDEFINE RADIOBUTTON [ <oRadio> ] ;
            [ OF <oWnd> ]              ;
            ID <nId>                   ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ ON CLICK <bClick> ]      ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oRadio> := ] HRadioButton():Redefine( <oWnd>,<nId>,<oFont>,<bInit>,<bSize>, ;
          <bDraw>,<bClick>,<ctoolt>,<color>,<bcolor> )


#xcommand @ <x>,<y> COMBOBOX [ <oCombo> ITEMS ] <aItems> ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ INIT <nInit> ]           ;
            [ SIZE <width>, <height> ] ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ ON CHANGE <bChange> ]    ;
            [ STYLE <nStyle> ]         ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oCombo> := ] HComboBox():New( <oWnd>,<nId>,<nInit>,,<nStyle>,<x>,<y>,<width>, ;
                  <height>,<aItems>,<oFont>,<bInit>,<bSize>,<bDraw>,<bChange>,<ctoolt> )

#xcommand REDEFINE COMBOBOX [ <oCombo> ITEMS ] <aItems> ;
            [ OF <oWnd> ]              ;
            ID <nId>                   ;
            [ INIT <nInit>    ]        ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ ON CHANGE <bChange> ]    ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oCombo> := ] HComboBox():Redefine( <oWnd>,<nId>,<nInit>,,<aItems>,<oFont>,<bInit>, ;
             <bSize>,<bDraw>,<bChange>,<ctoolt> )


#xcommand @ <x>,<y> UPDOWN [ <oUpd> INIT ] <nInit> ;
            RANGE <nLower>,<nUpper>    ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ WIDTH <nUpDWidth> ]      ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ ON GETFOCUS <bGfocus> ]  ;
            [ ON LOSTFOCUS <bLfocus> ] ;
            [ STYLE <nStyle> ]         ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oUpd> := ] HUpDown():New( <oWnd>,<nId>,<nInit>,,<nStyle>,<x>,<y>,<width>, ;
                    <height>,<oFont>,<bInit>,<bSize>,<bDraw>,<bGfocus>,         ;
                    <bLfocus>,<ctoolt>,<color>,<bcolor>,<nUpDWidth>,<nLower>,<nUpper> )


#xcommand @ <x>,<y> PANEL [ <oPanel> ] ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ STYLE <nStyle> ]         ;
          => ;
    [<oPanel> :=] HPanel():New( <oWnd>,<nId>,<nStyle>,<x>,<y>,<width>,<height>,<bInit>,<bSize>,<bDraw> )

#xcommand REDEFINE PANEL [ <oPanel> ]  ;
            [ OF <oWnd> ]              ;
            ID <nId>                   ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ HEIGHT <nHeight> ]       ;
          => ;
    [<oPanel> :=] HPanel():Redefine( <oWnd>,<nId>,<nHeight>,<bInit>,<bSize>,<bDraw> )

#xcommand @ <x>,<y> BROWSE [ <oBrw> ]  ;
            [ <lArr: ARRAY> ]          ;
            [ <lDb: DATABASE> ]        ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ ON CLICK <bEnter> ]      ;
            [ ON GETFOCUS <bGfocus> ]  ;
            [ ON LOSTFOCUS <bLfocus> ] ;
            [ STYLE <nStyle> ]         ;
            [ <lNoVScr: NO VSCROLL> ]  ;
            [ <lNoBord: NO BORDER> ]   ;
            [ FONT <oFont> ]           ;
            [ <lAppend: APPEND> ]      ;
            [ <lAutoedit: AUTOEDIT> ]  ;
            [ ON UPDATE <bUpdate> ]    ;
            [ ON KEYDOWN <bKeyDown> ]  ;
            [ ON POSCHANGE <bPosChg> ] ;
          => ;
    [<oBrw> :=] HBrowse():New( Iif(<.lDb.>,BRW_DATABASE,Iif(<.lArr.>,BRW_ARRAY,0)),;
        <oWnd>,<nId>,<nStyle>,<x>,<y>,<width>,<height>,<oFont>,<bInit>,<bSize>, ;
        <bDraw>,<bEnter>,<bGfocus>,<bLfocus>,<.lNoVScr.>,<.lNoBord.>, <.lAppend.>,;
        <.lAutoedit.>, <bUpdate>, <bKeyDown>, <bPosChg> )

#xcommand REDEFINE BROWSE [ <oBrw> ]   ;
            [ <lArr: ARRAY> ]          ;
            [ <lDb: DATABASE> ]        ;
            [ OF <oWnd> ]              ;
            ID <nId>                   ;
            [ ON INIT <bInit> ]        ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ ON CLICK <bEnter> ]      ;
            [ ON GETFOCUS <bGfocus> ]  ;
            [ ON LOSTFOCUS <bLfocus> ] ;
            [ FONT <oFont> ]           ;
          => ;
    [<oBrw> :=] HBrowse():Redefine( Iif(<.lDb.>,BRW_DATABASE,Iif(<.lArr.>,BRW_ARRAY,0)),;
        <oWnd>,<nId>,<oFont>,<bInit>,<bSize>,<bDraw>,<bEnter>,<bGfocus>,<bLfocus> )

#xcommand ADD COLUMN <block> TO <oBrw> ;
            [ HEADER <cHeader> ]       ;
            [ TYPE <cType> ]           ;
            [ LENGTH <nLen> ]          ;
            [ DEC <nDec>    ]          ;
            [ <lEdit: EDITABLE> ]      ;
            [ JUSTIFY HEAD <nJusHead> ];
            [ JUSTIFY LINE <nJusLine> ];
            [ PICTURE <cPict> ]        ;
            [ VALID <bValid> ]         ;
            [ WHEN <bWhen> ]           ;
            [ ITEMS <aItem> ]          ;
            [ BITMAP <oBmp> ]          ;
          => ;
    <oBrw>:AddColumn( HColumn():New( <cHeader>,<block>,<cType>,<nLen>,<nDec>,<.lEdit.>,;
                      <nJusHead>, <nJusLine>, <cPict>, <{bValid}>, <{bWhen}>, <aItem>, <oBmp> ) )


#xcommand @ <x>,<y> OWNERBUTTON [ <oOwnBtn> ]  ;
            [ OF <oWnd> ]             ;
            [ ID <nId> ]              ;
            [ SIZE <width>, <height> ] ;
            [ ON INIT <bInit> ]     ;
            [ ON SIZE <bSize> ]     ;
            [ ON DRAW <bDraw> ]     ;
            [ ON CLICK <bClick> ]   ;
            [ STYLE <nStyle> ]      ;
            [ <flat: FLAT> ]        ;
            [ TEXT <cText>          ;
                 [ COLOR <color>] [ FONT <font> ] ;
                 [ COORDINATES  <xt>, <yt>, <widtht>, <heightt> ] ;
            ] ;
            [ BITMAP <bmp>  [<res: FROM RESOURCE>] [<ltr: TRANSPARENT>] ;
                 [ COORDINATES  <xb>, <yb>, <widthb>, <heightb> ] ;
            ] ;
            [ TOOLTIP <ctoolt> ]    ;
          => ;
    [<oOwnBtn> :=] HOWNBUTTON():New( <oWnd>,<nId>,<nStyle>,<x>,<y>,<width>, ;
          <height>,<bInit>,<bSize>,<bDraw>, ;
          <bClick>,<.flat.>, ;
              <cText>,<color>,<font>,<xt>, <yt>,<widtht>,<heightt>, ;
              <bmp>,<.res.>,<xb>,<yb>,<widthb>,<heightb>,<.ltr.>, <ctoolt> )


#xcommand REDEFINE OWNERBUTTON [ <oOwnBtn> ]  ;
            [ OF <oWnd> ]                     ;
            ID <nId>                          ;
            [ ON INIT <bInit> ]     ;
            [ ON SIZE <bSize> ]     ;
            [ ON DRAW <bDraw> ]     ;
            [ ON CLICK <bClick> ]   ;
            [ <flat: FLAT> ]        ;
            [ TEXT <cText>          ;
                 [ COLOR <color>] [ FONT <font> ] ;
                 [ COORDINATES  <xt>, <yt>, <widtht>, <heightt> ] ;
            ] ;
            [ BITMAP <bmp>  [<res: FROM RESOURCE>] [<ltr: TRANSPARENT>] ;
                 [ COORDINATES  <xb>, <yb>, <widthb>, <heightb> ] ;
            ] ;
            [ TOOLTIP <ctoolt> ]    ;
          => ;
    [<oOwnBtn> :=] HOWNBUTTON():Redefine( <oWnd>,<nId>, ;
          <bInit>,<bSize>,<bDraw>, ;
          <bClick>,<.flat.>, ;
              <cText>,<color>,<font>,<xt>, <yt>,<widtht>,<heightt>, ;
              <bmp>,<.res.>,<xb>, <yb>,<widthb>,<heightb>,<.ltr.>, <ctoolt> )

#xcommand @ <x>,<y> DATEPICKER [ <oPick> ]  ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ INIT <dInit> ]           ;
            [ ON INIT <bInit> ]        ;
            [ ON GETFOCUS <bGfocus> ]  ;
            [ ON LOSTFOCUS <bLfocus> ] ;
            [ STYLE <nStyle> ]         ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oPick> :=] HDatePicker():New( <oWnd>,<nId>,<dInit>,,<nStyle>,<x>,<y>, ;
        <width>,<height>,<oFont>,<bInit>,<bGfocus>,<bLfocus>,<ctoolt>, ;
        <color>,<bcolor> )


#xcommand @ <x>,<y> SPLITTER [ <oSplit> ] ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ ON SIZE <bSize> ]        ;
            [ ON PAINT <bDraw> ]       ;
            [ DIVIDE <aLeft> FROM <aRight> ] ;
          => ;
    [<oSplit> :=] HSplitter():New( <oWnd>,<nId>,<x>,<y>,<width>,<height>,<bSize>,<bDraw>,<color>,<bcolor>,<aLeft>,<aRight> )


#xcommand PREPARE FONT <oFont>       ;
             NAME <cName>            ;
             WIDTH <nWidth>          ;
             HEIGHT <nHeight>        ;
             [ WEIGHT <nWeight> ]    ;
             [ CHARSET <charset> ]   ;
             [ <ita: ITALIC> ]       ;
             [ <under: UNDERLINE> ]  ;
             [ <strike: STRIKEOUT> ] ;
          => ;
    <oFont> := HFont():Add( <cName>, <nWidth>, <nHeight>, <nWeight>, <charset>, ;
                iif( <.ita.>,1,0 ), iif( <.under.>,1,0 ), iif( <.strike.>,1,0 ) )

/* Print commands */

#xcommand START PRINTER DEFAULT    ;
          => ;
    OpenDefaultPrinter(); StartDoc()

/* SAY ... GET system     */

#xcommand @ <x>,<y> GET [ <oEdit> VAR ]  <vari>  ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ PICTURE <cPicture> ]     ;
            [ WHEN  <bGfocus> ]        ;
            [ VALID <bLfocus> ]        ;
            [ STYLE <nStyle> ]         ;
            [<lnoborder: NOBORDER>]    ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oEdit> := ] HEdit():New( <oWnd>,<nId>,<vari>,               ;
                   {|v|Iif(v==Nil,<vari>,<vari>:=v)},             ;
                   <nStyle>,<x>,<y>,<width>,<height>,<oFont>,,,,  ;
                   <bGfocus>,<bLfocus>,<ctoolt>,<color>,<bcolor>,<cPicture>,<.lnoborder.> )

#xcommand REDEFINE GET [ <oEdit> VAR ] <vari>  ;
            [ OF <oWnd> ]              ;
            ID <nId>                   ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ WHEN  <bGfocus> ]        ;
            [ VALID <bLfocus> ]        ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oEdit> := ] HEdit():Redefine( <oWnd>,<nId>,<vari>, ;
                   {|v|Iif(v==Nil,<vari>,<vari>:=v)},    ;
                   <oFont>,,,,<bGfocus>,<bLfocus>,<ctoolt>,<color>,<bcolor> )


#xcommand @ <x>,<y> GET CHECKBOX [ <oCheck> VAR ] <vari>  ;
            CAPTION  <caption>         ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ <valid: VALID, ON CLICK> <bClick> ] ;
            [ STYLE <nStyle> ]         ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oCheck> := ] HCheckButton():New( <oWnd>,<nId>,<vari>,              ;
                    {|v|Iif(v==Nil,<vari>,<vari>:=v)},                   ;
                    <nStyle>,<x>,<y>,<width>,<height>,<caption>,<oFont>, ;
                    ,,,<bClick>,<ctoolt>,<color>,<bcolor> )

#xcommand REDEFINE GET CHECKBOX [ <oCheck> VAR ] <vari>  ;
            [ OF <oWnd> ]              ;
            ID <nId>                   ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ <valid: VALID, ON CLICK> <bClick> ] ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oCheck> := ] HCheckButton():Redefine( <oWnd>,<nId>,<vari>, ;
                    {|v|Iif(v==Nil,<vari>,<vari>:=v)},           ;
                    <oFont>,,,,<bClick>,<ctoolt>,<color>,<bcolor> )

#xcommand @ <x>,<y> GET COMBOBOX [ <oCombo> VAR ] <vari> ;
            ITEMS  <aItems>            ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ ON CHANGE <bChange> ]    ;
            [ STYLE <nStyle> ]         ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oCombo> := ] HComboBox():New( <oWnd>,<nId>,<vari>,    ;
                    {|v|Iif(v==Nil,<vari>,<vari>:=v)},      ;
                    <nStyle>,<x>,<y>,<width>,<height>,      ;
                    <aItems>,<oFont>,,,,<bChange>,<ctoolt> )

#xcommand REDEFINE GET COMBOBOX [ <oCombo> VAR ] <vari> ;
            ITEMS  <aItems>            ;
            [ OF <oWnd> ]              ;
            ID <nId>                   ;
            [ ON CHANGE <bChange> ]    ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oCombo> := ] HComboBox():Redefine( <oWnd>,<nId>,<vari>, ;
                    {|v|Iif(v==Nil,<vari>,<vari>:=v)},        ;
                    <aItems>,<oFont>,,,,<bChange>,<ctoolt> )

#xcommand @ <x>,<y> GET UPDOWN [ <oUpd> VAR ]  <vari>  ;
            RANGE <nLower>,<nUpper>    ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ WIDTH <nUpDWidth> ]      ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ PICTURE <cPicture> ]     ;
            [ WHEN  <bGfocus> ]        ;
            [ VALID <bLfocus> ]        ;
            [ STYLE <nStyle> ]         ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oUpd> := ] HUpDown():New( <oWnd>,<nId>,<vari>,               ;
                   {|v|Iif(v==Nil,<vari>,<vari>:=v)},              ;
                    <nStyle>,<x>,<y>,<width>,<height>,<oFont>,,,,  ;
                    <bGfocus>,<bLfocus>,<ctoolt>,<color>,<bcolor>, ;
                    <nUpDWidth>,<nLower>,<nUpper> )


#xcommand @ <x>,<y> GET DATEPICKER [ <oPick> VAR ] <vari> ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ WHEN <bGfocus> ]         ;
            [ VALID <bLfocus> ]        ;
            [ STYLE <nStyle> ]         ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oPick> :=] HDatePicker():New( <oWnd>,<nId>,<vari>,    ;
                    {|v|Iif(v==Nil,<vari>,<vari>:=v)},      ;
                    <nStyle>,<x>,<y>,<width>,<height>,      ;
                    <oFont>,,<bGfocus>,<bLfocus>,<ctoolt>,<color>,<bcolor> )


#xcommand SAY <value> TO <oDlg> ID <id> ;
          => ;
    SetDlgItemText( <oDlg>:handle, <id>, <value> )

/*   Menu system     */

#xcommand MENU [ OF <oWnd> ] [ ID <nId> ] [ TITLE <cTitle> ] ;
          => ;
    Hwg_BeginMenu( <oWnd>, <nId>, <cTitle> )

#xcommand CONTEXT MENU <oMenu> ;
          => ;
    <oMenu> := Hwg_ContextMenu()

#xcommand ENDMENU           => Hwg_EndMenu()

#xcommand MENUITEM <item> [ ID <nId> ]    ;
            ACTION <act>                  ;
            [ ACCELERATOR <flag>, <key> ] ;
            [<lDisabled: DISABLED>]       ;
          => ;
    Hwg_DefineMenuItem( <item>, <nId>, <{act}>, <.lDisabled.>, <flag>, <key> )

#xcommand SEPARATOR         => Hwg_DefineMenuItem()

#xcommand SET TIMER <oTimer> [ OF <oWnd> ] [ ID <id> ] ;
             VALUE <value> ACTION <bAction> ;
          => ;
    <oTimer> := HTimer():New( <oWnd>, <id>, <value>, <bAction> )


#xcommand SET KEY <nctrl>,<nkey> [ OF <oDlg> ] [ TO <func> ] ;
          => ;
    SetDlgKey( <oDlg>, <nctrl>, <nkey>, <{func}> )

/*             */
#xcommand @ <x>,<y> GRAPH [ <oGraph> DATA ] <aData> ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ COLOR <color> ]          ;
            [ BACKCOLOR <bcolor> ]     ;
            [ ON SIZE <bSize> ]        ;
            [ FONT <oFont> ]           ;
            [ TOOLTIP <ctoolt> ]       ;
          => ;
    [<oGraph> := ] HGraph():New( <oWnd>,<nId>,<aData>,<x>,<y>,<width>, ;
        <height>,<oFont>,<bSize>,<ctoolt>,<color>,<bcolor> )

/* open an .dll resource */
#xcommand SET RESOURCES TO <cName1> ;
       => ;
            LoadResource( <cName1> )

#xcommand SET RESOURCES TO => LOADRESOURCE( NIL )

// Addded by jamaj 
#xcommand DEFAULT <uVar1> := <uVal1> ;
               [, <uVarN> := <uValN> ] => ;
                  <uVar1> := IIf( <uVar1> == nil, <uVal1>, <uVar1> ) ;;
                [ <uVarN> := IIf( <uVarN> == nil, <uValN>, <uVarN> ); ]

#xcommand @ <x>,<y> GET IPADDRESS [ <oIp> VAR ] <vari> ;
            [ OF <oWnd> ]              ;
            [ ID <nId> ]               ;
            [ SIZE <width>, <height> ] ;
            [ BACKCOLOR <bcolor> ]     ;
            [ STYLE <nStyle> ]         ;
            [ FONT <oFont> ]           ;
            [ ON GETFOCUS <bGfocus> ]      ;
            [ ON LOSTFOCUS <bLfocus> ]     ;
          => ;
    [<oIp> := ] HIpEdit():New( <oWnd>,<nId>,{|v| iif(v==Nil,<vari>,<vari>:=v)},<nStyle>,<x>,<y>,<width>,<height>,<vari>,<oFont>, <bGfocus>, <bLfocus> )

#define ISOBJECT(c)    ( Valtype(c) == "O" )


/* Commands for PrintDos Class*/

#xcommand SET PRINTER TO <oPrinter> OF <oPtrObj>     ;
           => ;
      <oPtrObj>:=Printdos():New( <oPrinter>)

#xcommand @ <x>,<y> PSAY  <vari>  ;
            [ PICTURE <cPicture> ] OF <oPtrObj>   ;
          => ;
          <oPtrObj>:Say(<x>, <y>, <vari>, <cPicture>)

#xcommand  EJECT OF <oPtrObj> => <oPtrObj>:Eject()

#xcommand  END PRINTER <oPtrObj> => <oPtrObj>:End()

/*
Command for MonthCalendar Class
Added by Marcos Antonio Gambeta
*/

#xcommand @ <x>,<y> MONTHCALENDAR [ <oMonthCalendar> ] ;
            [ OF <oWnd> ]                              ;
            [ ID <nId> ]                               ;
            [ SIZE <nWidth>,<nHeight> ]                ;
            [ INIT <dInit> ]                           ;
            [ ON INIT <bInit> ]                        ;
            [ ON CHANGE <bChange> ]                    ;
            [ STYLE <nStyle> ]                         ;
            [ FONT <oFont> ]                           ;
            [ TOOLTIP <cTooltip> ]                     ;
            [ < notoday : NOTODAY > ]                  ;
            [ < notodaycircle : NOTODAYCIRCLE > ]      ;
            [ < weeknumbers : WEEKNUMBERS > ]          ;
          => ;
    [<oMonthCalendar> :=] HMonthCalendar():New( <oWnd>,<nId>,<dInit>,<nStyle>,;
        <x>,<y>,<nWidth>,<nHeight>,<oFont>,<bInit>,<bChange>,<cTooltip>,;
        <.notoday.>,<.notodaycircle.>,<.weeknumbers.>)

