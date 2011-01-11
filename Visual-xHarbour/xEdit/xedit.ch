#ifndef _XEDIT_CH
   #define _XEDIT_CH

   #include "inkey.ch"
   #include "fileio.ch"
   #include "directry.ch"


   #ifdef WIN
     #include "winapi.ch"
     #include "wingdi.ch"
     #include "commctrl.ch"
     #include "commdlg.ch"

     #if 1
        #include "structures.ch"
     #else
        IMPORT C STRUCTURE TVITEM
        IMPORT C STRUCTURE SCROLLINFO
        IMPORT C STRUCTURE OPENFILENAME
        IMPORT C STRUCTURE RECT
     #endif

     #include "ole.ch"

     #include "xedit.h"

     #define LEFT_PAD ::TextMetric:tmHeight

     #define FIND_DIALOG_WIDTH  455
     #define FIND_DIALOG_HEIGHT 275

      #ifndef OLE
         REQUEST xEditControlWindowProc
      #endif
   #else
      #ifndef IDOK
         //REQUEST HB_GT_WIN
         #define IDOK                1
         #define IDCANCEL            2
         #define IDABORT             3
         #define IDRETRY             4
         #define IDIGNORE            5
         #define IDYES               6
         #define IDNO                7
      #endif
      #xtranslate HideCaret() => SetCursor( .F. )
      #xtranslate ShowCaret([<x>]) => SetCursor( .T. )
      #xtranslate TextOut( <col>, <row>, <text> [, <color>] ) => DispOutAt( <row>, <col>, <text>, <color> )
   #endif

   #include "xEditConstants.ch"

   #ifdef __PLATFORM__Windows
     #define DIR_SEPARATOR '\'
     #define EOL Chr(13) + Chr(10)
   #else
     #define DIR_SEPARATOR '/'
     #define EOL Chr(10)
   #endif

   #define NAVIGATION_BEGIN ;
      ;LOCAL nLineOrigin := ::nLine, nColumnOrigin := ::nColumn, nPendingLineFrom, nPendingColumnFrom;
      ;;
      ;IF ::nNavigation++ == 0;
         ;IF ::lShift;
            ;IF ::nLineFrom == 0;
               ;nPendingLineFrom   := ::nLine;
               ;nPendingColumnFrom := ::nColumn;
            ;ELSE;
               ;nPendingLineFrom   := ::nLineFrom;
               ;nPendingColumnFrom := ::nColumnFrom;
            ;ENDIF;
         ;ELSE;
            ;IF ::nLineFrom > 0;
                ;::Action( { { ED_GOTO, ::nLine, ::nColumn }, { ( 2 << 8 ) | ED_UNSELECT, ::nLineFrom, ::nColumnFrom, ::nLineTo, ::nColumnTo, ::lSquare, ::nDirection } }, ::aUnDo );
            ;ENDIF;
            ;;
         ;ENDIF;
      ;ENDIF;
      ;;
      ;BEGIN SEQUENCE

    #ifdef WIN
       #define NAVIGATION_END ;
          ;END SEQUENCE;
          ;;
          ;IF --::nNavigation > 0;
             ;RETURN Self;
          ;ENDIF;
          ;;
          IF ::lShift;
             ;IF ::nLineFrom == 0;
                ;::nLineFrom := nPendingLineFrom;
                ;::nColumnFrom := nPendingColumnFrom;
             ;ENDIF;
             ;;
             ;WITH OBJECT ::oDisplay;
                 ;SendMessage( :hWnd, WM_MOUSEMOVE, MK_LBUTTON, MAKELPARAM( /*LEFT_PAD*/ :TextMetric:tmHeight + ( :nColumn * :TextMetric:tmAveCharWidth ), ( :nRow ) * :TextMetric:tmHeight ) );
             ;END;
          ;ENDIF
    #else
       #define NAVIGATION_END ;
          ;END SEQUENCE;
          ;;
          ;IF --::nNavigation > 0;
              ;RETURN Self;
          ;ENDIF;
          ;;
          IF ::lShift;
             ;WITH OBJECT ::oDisplay;
                 ;::Action( { { ED_GOTO, ::nLine, ::nColumn, nLineOrigin, nColumnOrigin }, ;
                              { ED_SELECT, nPendingLineFrom, nPendingColumnFrom, ::nLine, ::nColumn - 1, ::lSquare, 1 }, ;
                              { ED_UNSELECT, ::nLineFrom, ::nColumnFrom, ::nLineTo, ::nColumnTo, ::lSquare, ::nDirection }, ;
                              { ( 4 << 8 ) | ED_GOTO, ::nLine, ::nColumn, nLineOrigin, nColumnOrigin } }, ::aUndo );
             ;END;
          ;ENDIF
    #endif
#endif
