// Definizioni della libreria

#define NULL NIL

#translate IF <x> THEN <y>              => if <x> ; <y> ; end
#command UPDATE <v1> TO <v2> NOT NIL    => IF <v2> <> NIL ; <v1> := <v2> ; END
#translate CRLF => chr(13)+chr(10)
#Define CR                 CHR(13)   //   Carriage return
#define TAB                CHR(9)   //   Tab


#command BREAK IF <log>         => IF (<log>); BREAK; END
#command EXIT IF <log>          => IF (<log>); EXIT; END
#command LOOP IF <log>          => IF (<log>); LOOP; END

#translate ASSIGN <var> WITH <value> [DEFAULT <def>] => <var> := IIF( <value> <> NIL, <value>, <def> )

// Constants for the return value of oXbp:status()
#define WOG_STAT_INIT     0  // Part object is initialized
#define WOG_STAT_CREATE   1  // The :create() method was called successfully
#define WOG_STAT_FAILURE  2  // The :create() method failed to obtain system resources

#DEFINE MAKELPARAM( nLow, nHigh ) ( ( nLow ) + ( nHigh ) * 65536 )
#DEFINE MAKEWPARAM( nLow, nHigh ) ( ( nLow ) + ( nHigh ) * 65536 )

// XTRANSLATES
#xtrans NTrim( <s> [, <n>] )       => LTrim( Str( <s> [, <n>] ) )
#xtrans LocalBlock( <l> )          => {|x| IIf( x == NIL, <l>, <l> := x ) }
#xtrans MakeBlock( <e> )           => &( "{||" + <e> + "}" )
#xtrans Esc()                      => ( LastKey() == K_ESC )

#define StripFieldAlias( cKey )    StrTran( Upper( cKey ), "FIELD->" )
#define StripAlias( cKey )         Upper( Substr( cKey, At( "->", cKey )+2 ) )


// WOOPGUI PREPROCESSOR COMMANDS

// ----------------------------------------------------------------------

// Menu commands

#xcommand DEFINE CONTEXTMENU <oMenu> ;
      =>  <oMenu> := WG_BeginContextMenu()

#xcommand DEFINE MENUBAR <oMenu> ;
      =>  <oMenu> := WG_BeginMenuBar()

#command  POPUP [<oPopup> CAPTION ] <caption> ;
      =>  [<oPopup> := ] WG_BeginMenuPopup( <caption> )

#xcommand ITEM [ <oItem> CAPTION ] <caption> [ACTION <uAction,...>];
          [ <checked: CHECK, CHECKED> ] ;
          [ <disabled: DISABLE, DISABLED> ] ;
          [ <grayed: GRAY, GRAYED> ] ;
          [ BEFOREACTION <bfAction> ] [ AFTERACTION <afAction> ] ;
          [ MENUID <nId> ] [ MESSAGE <cMsg> ] ;
          [ <lOnBuildMenu: DEFERRED> ] ;
	  =>  [<oItem> := ] WG_DefineMenuItem( <caption> , [{|Self|<uAction>}], <.checked.>,, <cMsg>, <nId>,;
	                      [ !<.disabled.> ], [ <.grayed.> ], ;
	                      [ {|Self|<bfAction> } ],[ {|Self| <afAction> } ], ;
	                      !<.lOnBuildMenu.>  )

#command  SEPARATOR ;
          [ <lOnBuildMenu: DEFERRED> ] ;
	  =>  WG_DefineMenuItemSeparator(!<.lOnBuildMenu.>)

#xcommand END POPUP ;
      =>  WG_EndMenuPopup()

#xcommand END MENU ;
      =>  WG_EndMenuBar()

#xcommand END CONTEXTMENU ;
      =>  WG_EndContextMenu()

#xcommand SET WINDOW <oWnd> CONTEXTMENU <oMenu> ;
      =>  <oWnd>:SetContextMenu( <oMenu> )

// ----------------------------------------------------------------------

// Application commands

#command DEFINE APPLICATION [<oApp> NAME ] <cName> [TITLE <cTitle>] [ICONFILE <cIconFile>] ;
                            [CURSOR <cCursor>] [BACKCOLOR <nBackColor>] [ICONSMALLFILE <cIconSmallFile>];
                            [ONINIT <bInit>] [ONQUIT <bQuit> ] ;
      => [<oApp> := ] TApplication():New( <cName>, <cTitle>, <cIconFile>, <{bInit}>, <{bQuit}> )

#command APPLICATION ACTIVATE ;
      => WG_ApplObj():Activate()

#xcommand APPLICATION DEBUG EVENTS [IN WINDOW <oWnd>] [FILE <cFileName>] [<lAppend: APPEND>] ;
      => WG_ApplObj():SetLogWrite( TRUE, <cFileName>, <.lAppend.>, <oWnd> )

//#xcommand APPLICATION DEBUG EVENTS TO WINDOW <oWnd> ;
//      => WG_ApplObj():SetLogWindow( <oWnd> )

#command APPLICATION CREATE ;
      => WG_ApplObj():Create()

#command APPLICATION QUIT ;
      => WG_ApplObj():Quit()

#command SET CURRENT WINDOW <oWin> ;
      => TWindow():SetCurrentWindow( <oWin> )

#command SET APPLICATION CURRENT WINDOW <oWin> ;
      => SET CURRENT WINDOW <oWin>

// ----------------------------------------------------------------------

// Dialog commands
#xcommand DEFINE DIALOG <oDlg> ;
             [ AT <nRow>, <nCol> ] ;
             [ SIZE <nWidth>, <nHeight> ] ;
             [ <lPixel: PIXEL> ] ;
             [ TITLE <cTitle> ] ;
             [ <color: COLOR, COLORS> <nClrFore> [,<nClrBack>] ];
             [ OF <oParent> ] ;
             [ BRUSH <oBrush> ] ;                         // Contained Objects
             [ CURSOR <oCursor> ] ;
             [ ICON  <oIcon> ] ;
             [ MENU <oMenu> ] ;
             [ <lStatusBar: STATUSBAR> ];
             [ BORDER <border: NONE, SINGLE> ] ;
             [ <NoSysMenu:  NOSYSMENU, NO SYSMENU> ] ;
             [ <NoCaption:  NOCAPTION, NO CAPTION, NO TITLE> ] ;
             [ <NoIconize:  NOICONIZE, NOMINIMIZE> ] ;
             [ <NoMaximize: NOZOOM, NO ZOOM, NOMAXIMIZE, NO MAXIMIZE> ] ;
             [ <vScroll: VSCROLL, VERTICAL SCROLL> ] ;
             [ <hScroll: HSCROLL, HORIZONTAL SCROLL> ] ;
             [ <lModal: MODAL> ] ;
             [ FONT <oFont> ] ;
             [ FONTNAME <cFontName> ] ;
             [ FONTSIZE <nFontSize> ] ;
      => <oDlg> := TDialog():NewExtended( <cTitle>, <nRow>, <nCol>, <nWidth>, <nHeight>,;
             <oMenu>, <oBrush>, <oIcon>, <oParent>, <.lStatusBar.>, <.lPixel.>, <.lModal.>, ;
             [<.vScroll.>], [<.hScroll.>], <nClrFore>, <nClrBack>, <oCursor>,;
             [Upper(<(border)>)], !<.NoSysMenu.>, !<.NoCaption.>,;
             !<.NoIconize.>, !<.NoMaximize.>, <oFont>, <cFontName>, <nFontSize> )

// Panel commands
#xcommand DEFINE PANEL <oDlg> ;
             [ AT <nRow>, <nCol> ] ;
             [ SIZE <nWidth>, <nHeight> ] ;
             [ <lPixel: PIXEL> ] ;
             [ <color: COLOR, COLORS> <nClrFore> [,<nClrBack>] ];
             [ OF <oParent> ] ;
             [ BRUSH <oBrush> ] ;                         // Contained Objects
             [ CURSOR <oCursor> ] ;
             [ ICON  <oIcon> ] ;
             [ MENU <oMenu> ] ;
             [ <lBorder: BORDER > ] ;
             [ <vScroll: VSCROLL, VERTICAL SCROLL> ] ;
             [ <hScroll: HSCROLL, HORIZONTAL SCROLL> ] ;
             [ <lModal: MODAL> ] ;
             [ FONT <oFont> ] ;
             [ FONTNAME <cFontName> ] ;
             [ FONTSIZE <nFontSize> ] ;
      => <oDlg> := TPanel():NewExtended( ,,, <nRow>, <nCol>, <nWidth>, <nHeight>,;
             <oMenu>, <oBrush>, <oIcon>, <oParent>, <.lPixel.>, <.lModal.>, ;
             [<.vScroll.>], [<.hScroll.>], <nClrFore>, <nClrBack>, <oCursor>,;
             [<.lBorder.>], <oFont>, <cFontName>, <nFontSize> )

#translate SET DIALOG <*x*>  =>;
           SET WINDOW <x>

#translate SET PANEL <*x*>  =>;
           SET WINDOW <x>

// ----------------------------------------------------------------------

// Window commands
#xcommand DEFINE WINDOW <oWnd> ;
             [ AT <nRow>, <nCol> ] ;
             [ SIZE <nWidth>, <nHeight> ] ;
             [<lPixel: PIXEL>] ;
             [ TITLE <cTitle> ] ;
             [ <color: COLOR, COLORS> <nClrFore> [,<nClrBack>] ];
             [ OF <oParent> ] ;
             [ BRUSH <oBrush> ] ;                         // Contained Objects
             [ CURSOR <oCursor> ] ;
             [ ICON  <oIcon> ] ;
             [ MENU <oMenu> ] ;
             [ <lStatusBar: STATUSBAR> ];
             [ STYLE <nStyle> ] ;                          // Styles
             [ BORDER <border: NONE, SINGLE> ] ;
             [ <NoSysMenu:  NOSYSMENU, NO SYSMENU> ] ;
             [ <NoCaption:  NOCAPTION, NO CAPTION, NO TITLE> ] ;
             [ <NoIconize:  NOICONIZE, NOMINIMIZE> ] ;
             [ <NoMaximize: NOZOOM, NO ZOOM, NOMAXIMIZE, NO MAXIMIZE> ] ;
             [ <vScroll: VSCROLL, VERTICAL SCROLL> ] ;
             [ <hScroll: HSCROLL, HORIZONTAL SCROLL> ] ;
             [ FONT <oFont> ] ;
             [ FONTNAME <cFontName> ] ;
             [ FONTSIZE <nFontSize> ] ;
      => <oWnd> := TFrame():NewExtended( <cTitle>, <nStyle>, <nRow>, <nCol>, <nWidth>, <nHeight>,;
             <oMenu>, <oBrush>, <oIcon>, <oParent>, <.lStatusBar.>, ;
             [<.vScroll.>], [<.hScroll.>], <nClrFore>, <nClrBack>, <oCursor>,;
             [Upper(<(border)>)], !<.NoSysMenu.>, !<.NoCaption.>,;
             !<.NoIconize.>, !<.NoMaximize.>, <.lPixel.>, <oFont>, <cFontName>, <nFontSize> )

#xcommand SET WINDOW <oWnd> CENTERED ;
      => <oWnd>:Center()

#xcommand SET WINDOW <oWnd> CENTER ;
      => <oWnd>:Center()

#xcommand SET WINDOW <oWnd> EVENTHANDLER <bEvent> ;
      => <oWnd>:SetEventHandler( <{bEvent}> )

#xcommand SET WINDOW <oWnd> MENU <oMenu> ;
      => <oWnd>:SetMenu( <oMenu> )

#xcommand SET WINDOW <oWnd> STATUSBAR <cText> ;
      => <oWnd>:SetStatusBar(<cText>)

#xcommand SET WINDOW <oWnd> TITLE <cTitle> ;
      => <oWnd>:SetTitle( <cTitle> )

#xcommand WINDOW <oWnd> ACTIVATE ;
      => <oWnd>:Activate()

#xcommand WINDOW <oWnd> CREATE ;
      => <oWnd>:Create()

#xcommand DIALOG <oWnd> CREATE READMODAL;
      => <oWnd>:Create( GetList )

#xcommand DIALOG <oWnd> CREATE ;
      => <oWnd>:Create()

#xcommand PANEL <oWnd> CREATE ;
      => <oWnd>:Create()


// ----------------------------------------------------------------------

// Control commands

#xcommand @ <nRow>, <nCol> CHECKBOX [ <oBtn> VAR ] <lVar> ;
             [ PROMPT <cCaption> ] ;
             [ SIZE <nWidth>, <nHeight> ] ;
             [ <lPixel: PIXEL> ] ;
             [ ACTION <bAction,...> ] ;
             [ <of:OF, WINDOW, DIALOG> <oWnd> ] ;
             [ <help:HELP, HELPID, HELP ID> <nHelpId> ] ;
             [ TOOLTIP <cToolTip> ] ;
             [ STATUS <cStatusMsg> ] ;
             [ FONT <oFont> ] ;
             [ FONTNAME <cFontName> ] ;
             [ FONTSIZE <nFontSize> ] ;
             [ WHEN <bWhen> ] ;
             [ VALID <bValid> ] ;
             [ <lSelected: SELECTED> ];
             [ ID <nID> ] ;
             [ COLOR <ncFgColor> ] ;
             [ BACKCOLOR <ncBgColor> ] ;
      => [ <oBtn> := ] TCheckBox():NewExtended( <cCaption>, <nRow>, <nCol>, <nWidth>, <nHeight>,;
                                                  <oWnd>, [ {|Self|<bAction> } ], <cToolTip>, <cStatusMsg>, <.lPixel.>,;
                                                  <nID>, ;
                                                  <.lSelected>, LocalBlock(<lVar>), <oFont>, <cFontName>, <nFontSize>, <{bWhen}>,;
                                                  <{bValid}>, <ncFgColor>, <ncBgColor> )

#xcommand @ <nRow>, <nCol> GROUPBOX [ TO <oBtn> ] ;
             [ <label:LABEL,PROMPT> <cLabel> ] ;
             [ SIZE <nWidth>, <nHeight> ] ;
             [ <lPixel: PIXEL> ] ;
             [ <of:OF, WINDOW, DIALOG> <oWnd> ] ;
             [ TOOLTIP <cToolTip> ] ;
             [ STATUS <cStatusMsg> ] ;
             [ FONT <oFont> ] ;
             [ FONTNAME <cFontName> ] ;
             [ FONTSIZE <nFontSize> ] ;
             [ COLOR <ncFgColor> ] ;
             [ BACKCOLOR <ncBgColor> ] ;
      => [ <oBtn> := ] TGroupBox():NewExtended( <cLabel>, <nRow>, <nCol>, <nWidth>, <nHeight>,;
                                                  <oWnd>, , <cToolTip>,,<.lPixel.>,;
                                                  , <oFont>, <cFontName>, <nFontSize>,,, <ncFgColor>, <ncBgColor>)


#xcommand @ <nRow>, <nCol> PUSHBUTTON [ <oBtn> PROMPT ] <cCaption> ;
             [ SIZE <nWidth>, <nHeight> ] ;
             [ <lPixel: PIXEL> ] ;
             [ ACTION <bAction,...> ] ;
             [ <of:OF, WINDOW, DIALOG> <oWnd> ] ;
             [ <help:HELP, HELPID, HELP ID> <nHelpId> ] ;
             [ TOOLTIP <cToolTip> ] ;
             [ STATUS <cStatusMsg> ] ;
             [ FONT <oFont> ] ;
             [ FONTNAME <cFontName> ] ;
             [ FONTSIZE <nFontSize> ] ;
             [ WHEN <bWhen> ] ;
             [ VALID <bValid> ] ;
             [ <lDefault: DEFAULT> ];
             [ ID <nID> ] ;
             [ COLOR <ncFgColor> ] ;
             [ BACKCOLOR <ncBgColor> ] ;
      => [ <oBtn> := ] TPushButton():NewExtended( <cCaption>, <nRow>, <nCol>, <nWidth>, <nHeight>,;
                                                  <oWnd>, [ {|Self|<bAction> } ], <cToolTip>, <cStatusMsg>, <.lPixel.>,;
                                                  <.lDefault.>, <nID>, ;
                                                  <oFont>, <cFontName>, <nFontSize>, <{bWhen}>,;
                                                  <{bValid}>, <ncFgColor>, <ncBgColor> )

#xcommand @ <nRow>, <nCol> RADIOGROUP [ <oBtn> VAR ] <nVar>;
             [ <prm: PROMPT, ITEMS> <cItems,...> ] ;
             [ SIZE <nWidth>, <nHeight> ] ;
             [ <lPixel: PIXEL> ] ;
             [ ACTION <bAction,...> ] ;
             [ <of:OF, WINDOW, DIALOG> <oWnd> ] ;
             [ <help:HELP, HELPID, HELP ID> <nHelpId> ] ;
             [ TOOLTIP <cToolTip> ] ;
             [ STATUS <cStatusMsg> ] ;
             [ FONT <oFont> ] ;
             [ FONTNAME <cFontName> ] ;
             [ FONTSIZE <nFontSize> ] ;
             [ WHEN <bWhen> ] ;
             [ VALID <bValid> ] ;
             [ <lHoriz: HORIZONTAL> ];
             [ SELECT <nPos> ] ;
             [ COLOR <ncFgColor> ] ;
             [ BACKCOLOR <ncBgColor> ] ;
      => [ <oBtn> := ] TRadioGroup():NewExtended( {<cItems>}, <nRow>, <nCol>, <nWidth>, <nHeight>,;
                                                  <oWnd>, [ {|Self|<bAction> } ], <cToolTip>, <cStatusMsg>, <.lPixel.>,;
                                                  !<.lHoriz.>, <nPos>, ;
                                                  LocalBlock(<nVar>), <oFont>, <cFontName>, <nFontSize>, <{bWhen}>,;
                                                  <{bValid}>, <ncFgColor>, <ncBgColor> )

#xcommand @ <nRow>, <nCol> TRISTATEBUTTON [ <oBtn> VAR ] <nVar> ;
             [ PROMPT <cCaption> ] ;
             [ SIZE <nWidth>, <nHeight> ] ;
             [ <lPixel: PIXEL> ] ;
             [ ACTION <bAction,...> ] ;
             [ <of:OF, WINDOW, DIALOG> <oWnd> ] ;
             [ <help:HELPID, HELP ID> <nHelpId> ] ;
             [ TOOLTIP <cToolTip> ] ;
             [ STATUS <cStatusMsg> ] ;
             [ FONT <oFont> ] ;
             [ FONTNAME <cFontName> ] ;
             [ FONTSIZE <nFontSize> ] ;
             [ WHEN <bWhen> ] ;
             [ VALID <bValid> ] ;
             [ VALUE <nValue> ] ;
             [ ID <nID> ] ;
             [ COLOR <ncFgColor> ] ;
             [ BACKCOLOR <ncBgColor> ] ;
      => [ <oBtn> := ] T3StateButton():NewExtended( <cCaption>, <nRow>, <nCol>, <nWidth>, <nHeight>,;
                                                    <oWnd>, [ {|Self|<bAction> } ], <cToolTip>, <cStatusMsg>, <.lPixel.>,;
                                                    <nID>, ;
                                                    <nValue>, LocalBlock(<nVar>), <oFont>, <cFontName>, <nFontSize>, <{bWhen}>,;
                                                    <{bValid}>, <ncFgColor>, <ncBgColor> )

#xcommand @ <nRow>, <nCol> COMBOBOX [ <oCbx> VAR ] <nVar> ;
             [ PROMPT <cCaption> ] ;
             [ <it: PROMPTS, ITEMS, ROWS> <aItems> ] ;
             [ SIZE <nWidth>, <nHeight> ] ;
             [ <lPixel: PIXEL> ] ;
             [ ACTION <bAction,...> ] ;
             [ <of:OF, WINDOW, DIALOG> <oWnd> ] ;
             [ <help:HELPID, HELP ID> <nHelpId> ] ;
             [ TOOLTIP <cToolTip> ] ;
             [ STATUS <cStatusMsg> ] ;
             [ FONT <oFont> ] ;
             [ FONTNAME <cFontName> ] ;
             [ FONTSIZE <nFontSize> ] ;
             [ WHEN <bWhen> ] ;
             [ VALID <bValid> ] ;
             [ VALUE <nValue> ] ;
             [ ID <nID> ] ;
             [ COLOR <ncFgColor> ] ;
             [ BACKCOLOR <ncBgColor> ] ;
      => [ <oCbx> := ] TSimpleComboBox():NewExtended( <cCaption>, <nRow>, <nCol>, <nWidth>, <nHeight>,;
                                                   <oWnd>, <aItems>, [ {|Self|<bAction> } ], <cToolTip>, <cStatusMsg>, <.lPixel.>,;
                                                   <nID>, ;
                                                   <nValue>, LocalBlock(<nVar>), <oFont>, <cFontName>, <nFontSize>, <{bWhen}>,;
                                                   <{bValid}>, <ncFgColor>, <ncBgColor> )

#xcommand @ <nRow>, <nCol> DROPDOWN [ <oCbx> VAR ] <nVar> ;
             [ PROMPT <cCaption> ] ;
             [ <it: PROMPTS, ITEMS, ROWS> <aItems> ] ;
             [ SIZE <nWidth>, <nHeight> ] ;
             [ <lPixel: PIXEL> ] ;
             [ ACTION <bAction,...> ] ;
             [ <of:OF, WINDOW, DIALOG> <oWnd> ] ;
             [ <help:HELPID, HELP ID> <nHelpId> ] ;
             [ TOOLTIP <cToolTip> ] ;
             [ STATUS <cStatusMsg> ] ;
             [ FONT <oFont> ] ;
             [ FONTNAME <cFontName> ] ;
             [ FONTSIZE <nFontSize> ] ;
             [ WHEN <bWhen> ] ;
             [ VALID <bValid> ] ;
             [ VALUE <nValue> ] ;
             [ ID <nID> ] ;
             [ COLOR <ncFgColor> ] ;
             [ BACKCOLOR <ncBgColor> ] ;
      => [ <oCbx> := ] TDropDownComboBox():NewExtended( <cCaption>, <nRow>, <nCol>, <nWidth>, <nHeight>,;
                                                   <oWnd>, <aItems>, [ {|Self|<bAction> } ], <cToolTip>, <cStatusMsg>, <.lPixel.>,;
                                                   <nID>, ;
                                                   <nValue>, LocalBlock(<nVar>), <oFont>, <cFontName>, <nFontSize>, <{bWhen}>,;
                                                   <{bValid}>, <ncFgColor>, <ncBgColor> )

#xcommand @ <nRow>, <nCol> DROPLIST [ <oCbx> VAR ] <nVar> ;
             [ PROMPT <cCaption> ] ;
             [ <it: PROMPTS, ITEMS, ROWS> <aItems> ] ;
             [ SIZE <nWidth>, <nHeight> ] ;
             [ <lPixel: PIXEL> ] ;
             [ ACTION <bAction,...> ] ;
             [ <of:OF, WINDOW, DIALOG> <oWnd> ] ;
             [ <help:HELPID, HELP ID> <nHelpId> ] ;
             [ TOOLTIP <cToolTip> ] ;
             [ STATUS <cStatusMsg> ] ;
             [ FONT <oFont> ] ;
             [ FONTNAME <cFontName> ] ;
             [ FONTSIZE <nFontSize> ] ;
             [ WHEN <bWhen> ] ;
             [ VALID <bValid> ] ;
             [ VALUE <nValue> ] ;
             [ ID <nID> ] ;
             [ COLOR <ncFgColor> ] ;
             [ BACKCOLOR <ncBgColor> ] ;
      => [ <oCbx> := ] TDropListComboBox():NewExtended( <cCaption>, <nRow>, <nCol>, <nWidth>, <nHeight>,;
                                                   <oWnd>, <aItems>, [ {|Self|<bAction> } ], <cToolTip>, <cStatusMsg>, <.lPixel.>,;
                                                   <nID>, ;
                                                   <nValue>, LocalBlock(<nVar>), <oFont>, <cFontName>, <nFontSize>, <{bWhen}>,;
                                                   <{bValid}>, <ncFgColor>, <ncBgColor> )

#xcommand @ <nRow>, <nCol> LABEL <cLabel> [ TO <oBtn> ] ;
             [ SIZE <nWidth>, <nHeight> ] ;
             [ <lPixel: PIXEL> ] ;
             [ <of:OF, WINDOW, DIALOG> <oWnd> ] ;
             [ TOOLTIP <cToolTip> ] ;
             [ STATUS <cStatusMsg> ] ;
             [ FONT <oFont> ] ;
             [ FONTNAME <cFontName> ] ;
             [ FONTSIZE <nFontSize> ] ;
             [ ID <nID> ] ;
             [ COLOR <ncFgColor> ] ;
             [ BACKCOLOR <ncBgColor> ] ;
             [ <cAlign: LEFT, CENTER, CENTERED, RIGHT> ] ;
      => [ <oBtn> := ] TStaticText():NewExtended( <cLabel>, <nRow>, <nCol>, <nWidth>, <nHeight>,;
                                                  <oWnd>, , <cToolTip>,,<.lPixel.>,;
                                                  <nID>, [<(cAlign)>],,;
                                                  <oFont>, <cFontName>, <nFontSize>,,,;
                                                  <ncFgColor>, <ncBgColor> )

#xcommand @ <nRow>, <nCol> EDITBOX [ <oEdt> VAR ] <nVar> ;
             [ CAPTION <cCaption> ] ;
             [ SIZE <nWidth>, <nHeight> ] ;
             [ <lPixel: PIXEL> ] ;
             [ ACTION <bAction,...> ] ;
             [ <of:OF, WINDOW, DIALOG> <oWnd> ] ;
             [ <help:HELPID, HELP ID> <nHelpId> ] ;
             [ TOOLTIP <cToolTip> ] ;
             [ STATUS <cStatusMsg> ] ;
             [ FONT <oFont> ] ;
             [ FONTNAME <cFontName> ] ;
             [ FONTSIZE <nFontSize> ] ;
             [ WHEN <bWhen> ] ;
             [ VALID <bValid> ] ;
             [ VALUE <cValue> ] ;
             [ LIMIT <nLimit> ] ;
             [ ID <nID> ] ;
             [ COLOR <ncFgColor> ] ;
             [ BACKCOLOR <ncBgColor> ] ;
             [ <lReadOnly: READONLY> ] ;
             [ <lPassword: PASSWORD> ] ;
      => [ <oEdt> := ] TEdit():NewExtended( <cCaption>, <nRow>, <nCol>, <nWidth>, <nHeight>,;
                                                   <oWnd>, [ {|Self|<bAction> } ], <cToolTip>, <cStatusMsg>, <.lPixel.>,;
                                                   <nID>, ;
                                                   <cValue>, <nLimit>, <.lReadOnly.>, <.lPassword.>, LocalBlock(<nVar>), <oFont>, <cFontName>, <nFontSize>, <{bWhen}>,;
                                                   <{bValid}>, <ncFgColor>, <ncBgColor> )

#xcommand @ <nRow>, <nCol> EDITBOX MULTILINE [ <oEdt> VAR ] <nVar> ;
             [ CAPTION <cCaption> ] ;
             [ SIZE <nWidth>, <nHeight> ] ;
             [ <lPixel: PIXEL> ] ;
             [ ACTION <bAction,...> ] ;
             [ <of:OF, WINDOW, DIALOG> <oWnd> ] ;
             [ <help:HELPID, HELP ID> <nHelpId> ] ;
             [ TOOLTIP <cToolTip> ] ;
             [ STATUS <cStatusMsg> ] ;
             [ FONT <oFont> ] ;
             [ FONTNAME <cFontName> ] ;
             [ FONTSIZE <nFontSize> ] ;
             [ WHEN <bWhen> ] ;
             [ VALID <bValid> ] ;
             [ VALUE <cValue> ] ;
             [ LIMIT <nLimit> ] ;
             [ ID <nID> ] ;
             [ COLOR <ncFgColor> ] ;
             [ BACKCOLOR <ncBgColor> ] ;
             [ <lHScroll: HSCROLL, HORIZZONTAL SCROLL> ] ;
             [ <lVScroll: VSCROLL, VERTICAL SCROLL> ] ;
             [ <lReadOnly: READONLY> ] ;
             [ <lPassword: PASSWORD> ] ;
      => [ <oEdt> := ] TEditMultiline():NewExtended( <cCaption>, <nRow>, <nCol>, <nWidth>, <nHeight>,;
                                                   <oWnd>, [ {|Self|<bAction> } ], <cToolTip>, <cStatusMsg>, <.lPixel.>,;
                                                   <nID>, ;
                                                   <cValue>, <nLimit>, <.lReadOnly.>, <.lPassword.>, <.lHScroll.>, <.lVScroll.>, ;
                                                   LocalBlock(<nVar>), <oFont>, <cFontName>, <nFontSize>, <{bWhen}>,;
                                                   <{bValid}>, <ncFgColor>, <ncBgColor> )

#xcommand @ <nRow>, <nCol> CALENDAR [ <oCal> VAR ] <dVar> ;
             [ CAPTION <cCaption> ] ;
             [ SIZE <nWidth>, <nHeight> ] ;
             [ <lPixel: PIXEL> ] ;
             [ ACTION <bAction,...> ] ;
             [ <of:OF, WINDOW, DIALOG> <oWnd> ] ;
             [ <help:HELPID, HELP ID> <nHelpId> ] ;
             [ TOOLTIP <cToolTip> ] ;
             [ STATUS <cStatusMsg> ] ;
             [ FONT <oFont> ] ;
             [ FONTNAME <cFontName> ] ;
             [ FONTSIZE <nFontSize> ] ;
             [ WHEN <bWhen> ] ;
             [ VALID <bValid> ] ;
             [ VALUE <dValue> ] ;
             [ ID <nID> ] ;
             [ COLOR <ncFgColor> ] ;
             [ BACKCOLOR <ncBgColor> ] ;
      => [ <oCal> := ] TCalendar():NewExtended( <cCaption>, <nRow>, <nCol>, <nWidth>, <nHeight>,;
                                                   <oWnd>, [ {|Self|<bAction> } ], <cToolTip>, <cStatusMsg>, <.lPixel.>,;
                                                   <nID>, ;
                                                   <dValue>, LocalBlock(<dVar>), <oFont>, <cFontName>, <nFontSize>, <{bWhen}>,;
                                                   <{bValid}>, <ncFgColor>, <ncBgColor> )


// ----------------------------------------------------------------------


***
*  @..XSAY ... XGET
*
#command @ <row>, <col> XSAY <sayxpr>                                   ;
                        [<sayClauses,...>]                              ;
                        XGET <var>                                      ;
                        [<getClauses,...>]                              ;
                                                                        ;
      => @ <row>, <col> SAY <sayxpr> [<sayClauses>] ;
       ; @ Row(), Col() + 1 XGET <var> [<getClauses>]


// XGET COMBO ...............................................................

#command @ <row>, <col> XGET COMBO <var>                                  ;
                        [<comboClauses,...>]                              ;
=>;
         @ <row>, <col> XGET <var>                                        ;
                        TYPE    COMBO                                     ;
                        DISPLAY {|xg,lR| CoGetDisplay( xg, lR ) }         ;
                        READER  {|xg,i,p| CoGetReader( xg, i, @p ) }      ;
                        [<comboClauses>]

// XGET BUTTON ..............................................................

#command @ <row>, <col> XGET BUTTON <var>                                 ;
                        [COLOR <color>]                                   ;
                        [<buttonClauses,...>]                             ;
=>;
         @ <row>, <col> XGET <var>                                        ;
                        TYPE    BUTTON                                    ;
                        DISPLAY {|xg,lR,lP| BuGetDisplay( xg,lR,lP ) }    ;
                        READER  {|xg,i,p| BuGetReader( xg,i,@p ) }        ;
                        HELP    {|xg| NIL}                                ;
                        [<buttonClauses>]

//                        COLOR   IF( <.color.>, <color>, Color2nd( ColorGetData() ) );

// XGET CHECK ...............................................................

#command @ <row>, <col> XGET CHECK <var>                                  ;
                        [<checkClauses,...>]                              ;
=>;
         @ <row>, <col> XGET <var>                                        ;
                        TYPE    CHECK                                     ;
                        DISPLAY {|xg,lR| ChGetDisplay( xg,lR ) }          ;
                        READER  {|xg,i,p| ChGetReader( xg,i,@p ) }        ;
                        [<checkClauses>]

// XGET RADIOBUTTON .........................................................

#command @ <row>, <col> XGET RADIOBUTTON <var>                            ;
                        [BUTTONS <buttons>]                               ;
                        [ARRAY <ar>]                                      ;
                        [LABEL <label>]                                   ;
                        [<horiz: HORIZONTAL>]                             ;
                        [<nobox: NOBOX>]                                  ;
                        [<double: DOUBLE>]                                ;
                        [<nosay: NOSAY>]                                  ;
                        [<radioClauses,...>]                              ;
=>;
         @ <row>, <col> XGET <var>                                        ;
                        TYPE    RADIOBUTTON                               ;
                        DISPLAY {|xg,lR| RaGetDisplay( xg,lR,             ;
                                         <.nobox.>, <.horiz.>, <label>, <.nosay.> ) };
                        READER  {|xg,i,p| RaGetReader( xg,i,@p,           ;
                                          <.nobox.>, <.horiz.>,           ;
                                          <label>, <.nosay.> ) }          ;
                        HELP    {|xg| NIL}                                ;
                        ARRAY { <label>, <.ar.>, if( <.ar.>, <ar>, <buttons> ), <.nobox.>, ;
                                <.double.>, <.horiz.>, <.nosay.> }        ;
                        [<radioClauses>]

// XGET SECRET ..............................................................

#command @ <row>, <col> XGET SECRET <var>                                 ;
                        [<secretClauses,...>]                             ;
=>;
         @ <row>, <col> XGET <var>                                        ;
                        TYPE    SECRET                                    ;
                        DISPLAY {|xg,lR| SeGetDisplay( xg,lR ) }          ;
                        READER  {|xg,i,p| SeGetReader( xg,i,@p ) }        ;
                        HELP    {|xg| NIL}                                ;
                        [<secretClauses>]

// XGET CHAR ................................................................

#command @ <row>, <col> XGET CHAR <var>                                   ;
                        [<charClauses,...>]                               ;
=>;
         @ <row>, <col> XGET <var>                                        ;
                        TYPE    CHAR                                      ;
                        DISPLAY {|xg,lR| GcGetDisplay( xg,lR ) }          ;
                        READER  {|xg,i,p| GcGetReader( xg,i,@p ) }        ;
                        [<charClauses>]

// XGET NORMAL ..............................................................

#command @ <row>, <col> XGET EDIT <var>                                 ;
                        [<normClauses,...>]                               ;
=>;
         @ <row>, <col> XGET <var>                                        ;
                        TYPE    EDIT                                    ;
                        DISPLAY {|xg,lR| xg:Display() }                   ;
                        READER  {|xg,i,p| GetReader( xg,i,@p ) }          ;
                        [<normClauses>]

// XGET .....................................................................

#xcommand @ <nRow>, <nCol> XGET [ <oXg> VAR ] <xVar>;             // Extended Gets
                         [ TYPE <cType: EDIT, COMBO, CHECK, RADIOBUTTON,    ;
                                        BUTTON, SECRET, CHAR>]                ;
                         [ CAPTION <cCaption> ]                               ;
                         [ SIZE <nWidth>, <nHeight> ]                         ;
                         [ <of:OF, WINDOW, DIALOG> <oWnd> ]                   ;
                         [ <lPixel: PIXEL> ]                                  ;
                         [ DISPLAY <bDisp> ]                                  ;
                         [ READER  <bRdr> ]                                   ;
                         [ READBLOCK <bRblk> ]                                ;
                         [ <ar:ARRAY, DATA, ROWS, ITEMS, PROMPTS> <aVal> ]    ;
                         [ DEFAULT <xDef> ]                                   ;
                         [ PICTURE <cPicture> ]                               ;
                         [ LIMIT <nLimit> ]                                   ;
                         [ HELP <bHelp> ]                                     ;
                         [ MEANING <bMeaning> ]                               ;
                         [ ALIAS <cAlias> ]                                   ;
                         [ MAINT <oMaint> ]                                   ;
                         [ KEY <nKey> ]                                       ;
                         [ LETTER <cLetter> ]                                 ;
                         [ ACTION <bAction,...> ]                             ;
                         [ <help:HELPID, HELP ID> <nHelpId> ]                 ;
                         [ TOOLTIP <cToolTip> ]                               ;
                         [ STATUS <cStatusMsg> ]                              ;
                         [ MESSAGE <cMsg> ]                                   ;
                         [ WHEN <bWhen> ]                                     ;
                         [ VALID <bValid> ]                                   ;
                         [ EXIT <bExit> ]                                     ;
                         [ ID <nID> ]                                         ;
                         [ COLOR <ncFgColor> ]                                ;
                         [ BACKCOLOR <ncBgColor> ]                            ;
                         [ <lReadOnly: READONLY> ]                            ;
                         [ <lPassword: PASSWORD> ]                            ;
                         [ FONT <oFont> ]                                     ;
                         [ FONTNAME <cFontName> ]                             ;
                         [ FONTSIZE <nFontSize> ]                             ;
                         [ EXTRASTYLE <nStyle> ]                              ;
                         [ <lNoAutoSize: NOAUTOSIZE> ]                        ;
=>;
         ( AAdd( GetList, [ <oXg> := ] _XGET_( <(xVar)>, ;
                                     LocalBlock(<xVar>),;
                                     <"cType">,;
                                     <cCaption>,;
                                     IIF( <.nRow.>, <nRow>, Row() ), ;
                                     IIF( <.nCol.>, <nCol>, Col() ), ;
                                     <nWidth>, <nHeight>, ;
                                     <oWnd>,;
                                     <cToolTip>, <cStatusMsg>, <cMsg>, <.lPixel.>, <nID>, ;
                                     <nLimit>, ;
                                     <aVal>, ;
                                     <xDef>, ;
                                     <cPicture>, ;
                                     IIF( <.bDisp.>, <{bDisp}>, {|xg,lR| xg:Display() } ),;
                                     IIF( <.bRdr.>,  <{bRdr}>,  {|xg,bInkey,pos| GetReader( xg, bInkey, @pos ) } ),;
                                     <{bValid}>, <{bWhen}>, <{bHelp}>, <{bMeaning}>, <"cAlias">, ;
                                     [ {|Self|<bAction> } ], ;
                                     IIF( <.bRblk.>, {|x,g| Eval( <{bRblk}>, x, g ) }, NIL ), ;
                                     <oMaint>, ;
                                     <nKey>, ;
                                     <"cLetter">, ;
                                     <{bExit}>, ;
                                     <.lReadOnly.>,;
                                     <.lPassword.>,;
                                     <oFont>, <cFontName>, <nFontSize>, ;
                                     <ncFgColor>, <ncBgColor>, ;
                                     <nStyle>, ;
                                     <.lNoAutoSize.> ;
               ) ), [ <oXg> ] ) ;



#command XREAD => XGetListRead( GetList ); GetList:= {}


// XSAY .....................................................................

#command @ <r>, <c>  XSAY <fld>;             // Extended Says
                     ALIAS <a>;
                     [<xsayClauses,...>];
=>;
         @ <r>, <c>  XSAY <fld>;
                     FROM <a>-><fld>;
                     [<xsayClauses>]

#command @ <r>, <c>  XSAY <blk>;             // Extended Says
                     [FROM <fld>];
                     [PICTURE <p>];
                     [COLOR <clr>];
                     [MEANING <bMean>];
=>;
         Aadd( SayList, _XSAY_( <r> + BoxTr( MaintMapPanel( m ) ), ;
                                <c> + BoxLc( MaintMapPanel( m ) ), ;
                                <{blk}>, ;
                                <"fld">, ;
                                <p>,     ;
                                <clr>,   ;
                                <{bMean}> ) )

#command XDISPLAY => AEVAL( SayList, {|s| Eval( s ) } )


//-----------------------------------------------------------------------------------------------
// FONT command
//-----------------------------------------------------------------------------------------------

#xcommand DEFINE FONT <oFnt> ;
                 [ FACE            <cFace>              ] ;    // typeface name
                 [ SIZE            <nSize>              ] ;    // average character width
                 [ <lItalic: ITALIC>                    ] ;    // italic attribute option
                 [ <lUnderline: UNDERLINE>              ] ;    // underline attribute option
                 [ <lStrikeOut: STRIKEOUT>              ] ;    // strikeout attribute option
                 [ <lBold: BOLD>                        ] ;    // bold attribute option
      => <oFnt> := TFont():New( <cFace>, <nSize>, <.lItalic.>, <.lUnderline.>,;
                                   <.lStrikeOut.>, <.lBold.> )

#xcommand DEFINE FONT <oFnt> EXTENDED ;
                 [ HEIGHT          <nHeight>            ] ;    // height of font
                 [ WIDTH           <nWidth>             ] ;    // average character width
                 [ ESCAPEMENT      <nEscapement>        ] ;    // angle of escapement
                 [ ORIENTATION     <nOrientation>       ] ;    // base-line orientation angle
                 [ WEIGHT          <nWeight>            ] ;    // font weight
                 [ <lItalic: ITALIC>                    ] ;    // italic attribute option
                 [ <lUnderline: UNDERLINE>              ] ;    // underline attribute option
                 [ <lStrikeOut: STRIKEOUT>              ] ;    // strikeout attribute option
                 [ CHARSET         <nCharSet>           ] ;    // character set identifier
                 [ OUTPUTPRECISION <nOutputPrecision>   ] ;    // output precision
                 [ CLIPPINGPRECISION <nClipPrecision>   ] ;    // clipping precision
                 [ QUALITY         <nQuality>           ] ;    // output quality
                 [ PITCHANDFAMILY  <nPitchAndFamily>    ] ;    // pitch and family
                 [ FACE            <cFace>              ] ;    // typeface name
      => <oFnt> := TFont():NewExtended( <nHeight>, <nWidth>, <nEscapement>, <nOrientation>, <nWeight>, <.lItalic.>, <.lUnderline.>,;
                    <.lStrikeOut.>, <nCharSet>, <nOutputPrecision>, <nClipPrecision>, <nQuality>, <nPitchAndFamily>,;
                    <cFace> )

#xcommand RELEASE FONT <oFnt> ;
      => <oFnt>:Destroy()

//-----------------------------------------------------------------------------------------------
