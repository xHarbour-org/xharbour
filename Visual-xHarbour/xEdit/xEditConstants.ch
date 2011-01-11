#define ED_BUFFER          1
#define ED_NEXTLINE        2
#define ED_PREVLINE        3
#define ED_MODIFICATIONS   4
   #define ED_BASE_LEN     4

#define ED_COLLAPSED_START 5
#define ED_COLLAPSED_END   6
#define ED_COLLAPSED_LINES 7
   #define ED_FULL_LEN     7

#define ED_BACKSPACE     1
#define ED_OVERRIDE      2
#define ED_REPLACE       3
#define ED_DELETE        4
#define ED_INSERT        5
#define ED_INSERTLINE    6
#define ED_DELETELINE    7
#define ED_ADDLINE       8
#define ED_BREAKLINE     9
#define ED_CUT          10
#define ED_PASTE        11

#define ED_NOIMPACT     12

#define ED_SELECT       12
#define ED_UNSELECT     13
#define ED_COLLAPSE     14
#define ED_EXPAND       15
#define ED_COLLAPSEALL  16
#define ED_EXPANDALL    17
#define ED_GOTO         18
#define ED_TOGGLEINSERT 19

#ifndef FR_DOWN
  #define FR_DOWN                                  1
  #define FR_WHOLEWORD                             2
  #define FR_MATCHCASE                             4
  #define FR_FINDNEXT                              8
  #define FR_REPLACE                              16
  #define FR_REPLACEALL                           32
#endif

// ADDED by xEdit
#define FR_FROMTOP                             256
#define FR_SELECTED                            512

#define EN_GETEDITOR ( WM_USER + 1 )

#define EN_SETSTATUSWINDOW ( WM_USER + 2 )
