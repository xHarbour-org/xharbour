/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_win.h,v 1.1 2003/10/09 23:18:34 jonnymind Exp $

   GTK interface
*/

#ifndef XWT_WIN_H
#define XWT_WIN_H

#include <windows.h>
#include <xwt_api.h>

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


typedef struct tag_xwt_win_base_data
{
   PXWT_WIDGET *xwt_widget;
   HWND  hMain;
} XWT_WIN_BASE_DATA, *PXWT_WIN_BASE_DATA;


typedef struct tag_xwt_win_data
{
   XWT_WIN_BASE_DATA;
   HMENU hMenu;
   HWND  hMainWidget;
   HWND  hStatusBar;
} XWT_WIN_DATA, *PXWT_WIN_DATA;


void *xwt_win_get_topwidget_neuter( void *);
void xwt_win_free_wnd( void *);

BOOL xwt_win_createFrameWindow( PXWT_WIDGET xwtData );

#endif
