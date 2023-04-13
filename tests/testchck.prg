#xcommand @ <nRow>, <nCol> CHECKBOX [ <oCbx> VAR ] <lVar> ;
             [ PROMPT <cCaption> ] ;
             [ <of:OF, WINDOW, DIALOG> <oWnd> ] ;
             [ SIZE <nWidth>, <nHeight> ] ;
             [ <help:HELPID, HELP ID> <nHelpId> ] ;
             [ FONT <oFont> ] ;
             [ <change: ON CLICK, ON CHANGE> <uClick> ] ;
             [ VALID   <ValidFunc> ] ;
             [ <color: COLOR, COLORS> <nClrFore> [,<nClrBack>] ] ;
             [ <design: DESIGN> ] ;
             [ <pixel: PIXEL> ] ;
             [ MESSAGE <cMsg> ] ;
             [ <update: UPDATE> ] ;
             [ WHEN <WhenFunc> ] ;
      => ;
         [ <oCbx> := ] TCheckBox():New( <nRow>, <nCol>, <cCaption>,;
             bSETGET(<lVar>), <oWnd>, <nWidth>, <nHeight>, <nHelpId>,;
             [<{uClick}>], <oFont>, <{ValidFunc}>, <nClrFore>, <nClrBack>,;
             <.design.>, <.pixel.>, <cMsg>, <.update.>, <{WhenFunc}> )

   @ 3, 3 CHECKBOX oChk VAR lValue PROMPT "&ClickMe" SIZE 100, 20 OF oWnd ;
      ON CHANGE oChk:SetText( "New Text" ) COLOR "W+/B"
