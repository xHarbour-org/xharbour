/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt.ch,v 1.6 2003/04/01 22:36:27 gian Exp $

   Definitions
*/

#ifndef XWT_CH
#define XWT_CH

/* Event types */
#define XWT_E_SHOW         1  // No params
#define XWT_E_HIDE         2  // No params
#define XWT_E_MOVE         3  // x,y
#define XWT_E_RESIZE       4  // width, height (-1 == unchanged )
#define XWT_E_TEXT         5  // cText
#define XWT_E_ADDCHILD     6  // oChild
#define XWT_E_REMOVECHILD  7  // oChild

#define XWT_E_ENTER        10 // No Params
#define XWT_E_LEAVE        11 // No Params
#define XWT_E_MOUSEMOVE    12 // X,Y

#define XWT_E_PRESSED      20 // No Params
#define XWT_E_RELEASED     21 // No Params
#define XWT_E_CLICKED      22 // No Params or x,y if available

#define XWT_E_CHANGED      30  // Depending on the sender object, generally a text
#define XWT_E_UPDATED      31  // Means a "definitive" change, like pressing enter in a textbox
#define XWT_E_REVERTED     32  // Means a "Revert" request, or a cancelation of current op

#define XWT_E_CREATE       100 // No Params
#define XWT_E_DESTROY_REQ  101 // No Params
#define XWT_E_DESTROY      102 // No Params


/**** Widget properties ****/
#define XWT_PROP_FOCUS     1  // bool
#define XWT_PROP_POSITION  2  // int, int
#define XWT_PROP_SIZE      3  // int, int
#define XWT_PROP_TEXT      4  // text (char *)
#define XWT_PROP_NAME      5  // text (char *)
#define XWT_PROP_SENSIBLE  6  // Verbosity of mouse events; BOOL

#define XWT_PROP_EDITABLE  10 // bool
#define XWT_PROP_VISIBLE   11 // bool
#define XWT_PROP_SELREGION 12 // x,y (stacked in position)

#define XWT_PROP_CONTENT   20 // data * (number generally holds the size)
#define XWT_PROP_RESOURCE  21 // Char * or number depending driver


#define XWT_PROP_FIXED      100 // bool
#define XWT_PROP_MODAL      101 // bool
#define XWT_PROP_VISIBILITY 102 // int (see widget visibility)

/* Fake propertyes */
#define XWT_PROP_SETMENUBAR 1000 // PHB_ITEM (HB_IT_ARRAY) in data
#define XWT_PROP_RSTMENUBAR 1001 // idem

/*User defined properties */
#define XWT_PROP_USER       10000

/* Widget visibility */
#define XWT_VIS_HIDDEN       0
#define XWT_VIS_NORMAL       1
#define XWT_VIS_MAXIMIZED_H  2
#define XWT_VIS_MAXIMIZED_V  3
#define XWT_VIS_MAXIMIZED    4
#define XWT_VIS_MINIMIZED    5

/* Widget alignment */
#define XWT_ALIGN_TOP        0
#define XWT_ALIGN_LEFT       0
#define XWT_ALIGN_CENTER     1
#define XWT_ALIGN_BOTTOM     2
#define XWT_ALIGN_RIGHT      2


/*** Message box ***/
#define XWT_MSGBOX_INFO     1
#define XWT_MSGBOX_QUESTION 2
#define XWT_MSGBOX_ERROR    3
#define XWT_MSGBOX_WARNING  4

#define XWT_MSGBOX_OK        1
#define XWT_MSGBOX_CANCEL    2
#define XWT_MSGBOX_CLOSE     4
#define XWT_MSGBOX_ABORT     8
#define XWT_MSGBOX_RETRY     16
#define XWT_MSGBOX_YES       32
#define XWT_MSGBOX_NO        64

/**** Image status ********/
#define XWT_IMG_NOTREADY      0
#define XWT_IMG_ERROR        -1
#define XWT_IMG_PROGRESS      1
#define XWT_IMG_READY         2


/**** Type of widgets *****/
#define XWT_TYPE_WIDGET   0
#define XWT_TYPE_WINDOW   1
#define XWT_TYPE_FRAME    2
#define XWT_TYPE_PANE     3
#define XWT_TYPE_VPANE    4
#define XWT_TYPE_HPANE    5
#define XWT_TYPE_GRIDPANE 6

#define XWT_TYPE_BUTTON   10
#define XWT_TYPE_LABEL    20
#define XWT_TYPE_MENU     30
#define XWT_TYPE_MENUITEM 31
#define XWT_TYPE_TEXTBOX  40
#define XWT_TYPE_IMAGE    50

#endif
