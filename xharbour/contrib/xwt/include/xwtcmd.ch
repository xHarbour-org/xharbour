/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwtcmd.ch,v 1.1 2003/07/23 15:58:10 lculik Exp $

   Definitions
*/


#ifndef __XTWCMD_CH__
#define __XTWCMD_CH__

#xcommand DEFINE PANE <oPane> [ <lHasBox: BOX >];
	  [ TITLE <cTitle>] [ OF <oParent> ];
          => <oPane> := XWTPane():New( <.lHasBox.>, <cTitle>, <oParent> )

#xcommand DEFINE LAYOUT <oLayout> [ MODE <nMode>];
          [ SIZE <nWidth>,<nHeigth> ] ;
          [ PADDING <nPadding>] [BORDER <nBorder> ];
          [ OF <oParent> ] => <oLayout> := XWTLayout():New( <nMode>, <nWidth>, <nHeigth>, <nPadding>, <nBorder>,  <oParent> )


// Button ----------------------------------------------------------------------
#xcommand DEFINE BUTTON <oBtn> [ TEXT <cText> ] [ MOVE <x>,<y> ][ OF <oParent> ];
          => <oBtn> := XWTButton():New( <cText>, <x>, <y>, <oParent> )

#xcommand @ <x>,<y> BUTTON <oBtn> [ TEXT <cText>] [ OF <oParent> ];
          => <oBtn> := XWTButton():New( <cText>, <x>, <y>, <oParent>)

// Label -------------------------------------------------------------------------
#xcommand DEFINE LABEL <oLabel> [ TEXT <cText> ] [ MOVE <x>,<y> ] [ OF <oParent> ] [FONT <cFont>] [SIZE <nSize>] [COLOR <clr>];
          => <oLabel> := XWTLabel():New( <cText>, <x>, <y>, <oParent>, <(cFont)>,<nSize>,<(clr)>)

#xcommand @ <x>,<y> LABEL <oLabel> [ TEXT <cText> ] [ OF <oParent> ] [FONT <cFont>] [SIZE <nSize>] [COLOR <clr>;
          => <oLabel> := XWTLabel():New( <cText>, <x>, <y>, <oParent>, <(cFont)>,<nSize>,<(clr)>)

// TextBox -------------------------------------------------------------------------
#xcommand DEFINE TEXTBOX [ <oTextBox> ] [ VAR <cText> ] [MOVE <x>,<y>] [ OF <oParent> ];
          => [ <oTextBox> := ] XWTTextBox():New( <cText>, <x>, <y>, <oParent> )

#xcommand @ <x>,<y> TEXTBOX [ <oTextBox> ] [ VAR <cText> ] [ OF <oParent> ];
          => [ <oTextBox> := ] XWTTextBox():New( <cText>, <x>, <y>, <oParent> )

// Checkbox ---------------------------------------------------------------------------
#xcommand DEFINE CHECKBOX [<oCheck>] [TEXT <cText>] [ VAR <lStatus>];
                          [ MOVE <x>,<y>] [ OF <oParent> ];
                  => [<oCheck> := ] XWTCheckbox():New( <cText>,<lStatus>,<x>,<y>,<oParent> )

#xcommand @ <x>,<y> CHECKBOX [<oCheck>] [TEXT <cText>] [ VAR <lStatus>] [ OF <oParent> ];
                  => [<oCheck> := ] XWTCheckbox():New( <cText>,<lStatus>,<x>,<y>,<oParent> )

// ToggleButton ---------------------------------------------------------------------------
#xcommand DEFINE TOGGLEBUTTON [<oToggle>] [TEXT <cText>] [ VAR <lStatus>];
                          [ MOVE <x>,<y>] [ OF <oParent> ];
                  => [<oToggle> := ] XWTToggleButton():New( <cText>,<lStatus>,<x>,<y>,<oParent> )

#xcommand @ <x>,<y> TOGGLEBUTTON [<oToggle>] [TEXT <cText>] [ VAR <lStatus>] [ OF <oParent> ];
                  => [<oToggle> := ] XWTToggleButton():New( <cText>,<lStatus>,<x>,<y>,<oParent> )

// Splitter --------------------------------------------------------------------------------
#xcommand DEFINE SPLITTER  [<oSplitter>] [ MODE <nMode> ][ FIRSTWIDGET <oFirstWidget> ][SECONDWIDGET <oSecondWidget>];
                           [ SIZE <nSizeX>,<nSizeY> ] [ OF <oParent> ];
 => [ <oSplitter> := ] XWTSplitter():New( <nMode>, <oFirstWidget>, <oSecondWidget>, <nSizeX>, <nSizeY>, <oParent> )


// Image -------------------------------------------------------------------------------------
#xcommand DEFINE IMAGE [ <oImg> ] [FILE <cFile> ] [ OF <oParent> ];
                 => [ <oImg> := ] XWTImage():New( <cFile>,<oParent>)

// Menus ----------------------------------------------------------------------------------------
#xcommand MENU <oMenu> [ PROMPT < cPrompt> ] [ OF <oParent> ] ;
          => <oMenu> := XWTMenu():New( <cPrompt>, <oParent> )

#xcommand MENUITEM [<oMenuItem>] [ PROMPT <cPrompt> ] [ ID <nId>] [ICON <cIcon>];
                               [ ACTION <pAction> ] [ METHOD <oMethod> ][ OF <oMenu> ];
          => [<oMenuItem> := ] XWTMenuItem():New( <cPrompt>, <nId>, <pAction>, <oMethod>, <cIcon>, <oMenu> )
	  
#endif