/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_win.h,v 1.3 2003/10/14 23:12:12 jonnymind Exp $

   GTK interface
*/

#ifndef XWT_WIN_H
#define XWT_WIN_H

#include <windows.h>
#include <xwt_api.h>

/* Import important global variables known by the WM */
HB_EXPORT extern HANDLE hb_hInstance;
HB_EXPORT extern HANDLE hb_hPrevInstance;
HB_EXPORT extern int    hb_iCmdShow;

#define XWT_WIN_FRMCLSNAME "XWTWIN_Framewnd"
#define XWT_WIN_PROP_SELF "XWT_SELF"

#define XWT_WIN_MAKESELF( var )\
   HB_ITEM Self; \
   PXWT_WIDGET _wSelf; \
   Self.type = HB_IT_OBJECT;\
   _wSelf = (PXWT_WIDGET) GetWindowLong( var, GWL_USERDATA ); \
   Self.item.asArray.value = _wSelf ? _wSelf->owner: 0; 

LRESULT CALLBACK xwt_gtk_framewndproc(
    HWND hwnd,
    UINT uMsg,
    WPARAM wParam,
    LPARAM lParam
);

#ifdef __cplusplus
typedef struct tag_xwt_win_base_data
{
   PXWT_WIDGET xwt_widget;
   HWND  hMain;
} XWT_WIN_BASE_DATA, *PXWT_WIN_BASE_DATA;

typedef struct tag_xwt_win_data : public tag_xwt_win_base_data
{
   PHB_BASEARRAY pMenu;
   HWND  hMainWidget;
   HWND  hStatusBar;
} XWT_WIN_DATA, *PXWT_WIN_DATA;

#else
typedef struct tag_xwt_win_base_data
{
   PXWT_WIDGET xwt_widget;
   HWND  hMain;
} XWT_WIN_BASE_DATA, *PXWT_WIN_BASE_DATA;

typedef struct tag_xwt_win_data
{
   XWT_WIN_BASE_DATA;
   PHB_BASEARRAY pMenu;
   HWND  hMainWidget;
   HWND  hStatusBar;
} XWT_WIN_DATA, *PXWT_WIN_DATA;
#endif


/* Windows menu are not considered widgets (popup menus are, but only at toplevel);
Making them real widgets would require a complete rewrite of windows menu (as GTK
for windows does). To avoid this, at least now, we use "invisible" widgests that
are translated into proper menu representation (window menu or popup menu) on need. */

typedef struct tag_xwt_win_menudata
{
   LPSTR szLabel;
   HBITMAP hBitmap; 
} XWT_WIN_MENUDATA, *PXWT_WIN_MENUDATA;


/* generic support functions */
void *xwt_win_get_topwidget_neuter( void *);
void *xwt_win_get_neuter( void *data );


/* Management of single widgets */
BOOL xwt_win_createFrameWindow( PXWT_WIDGET xwtData );
void xwt_win_setMenuBar( PXWT_WIDGET xwtData, PHB_ITEM pMenuArray );
void xwt_win_resetMenuBar( PXWT_WIDGET xwtData );
void xwt_win_free_wnd( void *);

BOOL xwt_win_createMenu( PXWT_WIDGET xwtData );
BOOL xwt_win_createMenuItem( PXWT_WIDGET xwtData );
void xwt_win_delete_menu( void *data );

#endif
