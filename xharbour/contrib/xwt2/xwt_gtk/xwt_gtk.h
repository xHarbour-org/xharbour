/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk.h,v 1.3 2004/05/20 15:41:38 jonnymind Exp $

   GTK interface
*/

/**
   XWT_GTK inheritance map.

   XWT_GTK prefixes all widget types.

   BASE <---+---- MODAL
            |---- TEXTAREA
            |---- SPLITTER
            |---- WND <---- FRAMEWND (*)
            |---- IDWID <--- MENUITEM (**)
            |---- ALIGN <---+---- SENSIBLE <---+---- IMAGE
                            |                  |---- LABEL
                            |
                            |---- CONTAINER <---+---- LAYOUT
                                                |---- GRID

(*) Collaborates with MENUITEM
(**) Collaborates with ALIGN, LABEL and IMAGE
*/

#ifndef XWT_GTK_H
#define XWT_GTK_H

#include <gtk/gtk.h>
#include <xwt_api.h>

#if __GNUC__ < 3 || defined( XWT_GTK_OLD_INHERITANCE )
#   define INHERIT( something )     something inherit
#   define INH( field )             inherit.field
#else
#   define INHERIT( something )     something
#   define INH( field )             field
#endif

gboolean xwt_idle_function( gpointer data );
void *xwt_gtk_get_topwidget_neuter( void *);


typedef struct tag_xwt_gtk_base
{
   GtkWidget *main_widget;
   GtkWidget * (*top_widget)( PXWT_WIDGET self );
   int x;
   int y;
   int width;
   int height;
   int nId;
   BOOL bBroadcast;
} XWT_GTK_BASE, *PXWT_GTK_BASE;


typedef struct tag_xwt_gtk_modal
{
  INHERIT( XWT_GTK_BASE );
  BOOL modal;
  BOOL canceled;
} XWT_GTK_MODAL, *PXWT_GTK_MODAL;

typedef struct tag_xwt_gtk_textarea
{
   INHERIT( XWT_GTK_BASE );
   GtkTextBuffer *textbuf;
   GtkScrolledWindow *frame;
} XWT_GTK_TEXTAREA, *PXWT_GTK_TEXTAREA;


typedef struct tag_xwt_gtk_menu
{
  INHERIT( XWT_GTK_BASE );
  GtkWidget *bar_item;
  GtkWidget *label;
} XWT_GTK_MENU, *PXWT_GTK_MENU;


typedef struct tag_xwt_gtk_splitter
{
   INHERIT( XWT_GTK_BASE );
   GtkWidget *first_widget;
   GtkWidget *second_widget;
   BOOL bShrink1;
   BOOL bShrink2;
} XWT_GTK_SPLITTER, *PXWT_GTK_SPLITTER;


// Also basic connector (for now)
typedef struct tag_xwt_gtk_wnd
{
   INHERIT( XWT_GTK_BASE );
   BOOL (*connect)( PXWT_WIDGET wParent, PXWT_WIDGET wChild );
   BOOL (*disconnect)( PXWT_WIDGET wParent );
   GtkWidget *window;
} XWT_GTK_WND, *PXWT_GTK_WND;


typedef struct tag_xwt_gtk_framewnd
{
   INHERIT( XWT_GTK_WND );
   GtkWidget *vbox;
   GtkWidget *menu_bar;
   GtkWidget *menu_box;
   GtkWidget *status_bar;
   guint status_context;
   char *status_message;
} XWT_GTK_FRAMEWND, *PXWT_GTK_FRAMEWND;


typedef struct tag_xwt_gtk_idwid
{
   INHERIT( XWT_GTK_BASE );
   char szEventName[XWT_EVENT_NAME_SIZE+1];
} XWT_GTK_IDWID, *PXWT_GTK_IDWID;


typedef struct tag_xwt_gtk_menuitem
{
   INHERIT( XWT_GTK_IDWID );
   GtkWidget *hbox;
   GtkWidget *image;
   GtkWidget *label;
   GtkWidget *align;
   BOOL      bIsCheck;
   BOOL      bValue;
} XWT_GTK_MENUITEM, *PXWT_GTK_MENUITEM;


typedef struct tag_xwt_gtk_align
{
   INHERIT( XWT_GTK_BASE );
   GtkWidget *align;
   int iVAlign;
   int iHAlign;
} XWT_GTK_ALIGN, *PXWT_GTK_ALIGN;


typedef struct tag_xwt_gtk_sensible
{
   INHERIT( XWT_GTK_ALIGN );
   GtkWidget *evt_window;
} XWT_GTK_SENSIBLE, *PXWT_GTK_SENSIBLE;


typedef struct tag_xwt_gtk_image
{
   INHERIT( XWT_GTK_SENSIBLE );
   GdkPixmap *pixmap;
   char *filename;
} XWT_GTK_IMAGE, *PXWT_GTK_IMAGE;


typedef struct tag_xwt_gtk_container
{
   INHERIT( XWT_GTK_ALIGN );
   GtkWidget *frame;
   BOOL (*add)( PXWT_WIDGET self, PXWT_WIDGET child);
   BOOL (*remove)( PXWT_WIDGET self, PXWT_WIDGET child);
} XWT_GTK_CONTAINER, *PXWT_GTK_CONTAINER;


typedef struct tag_xwt_gtk_layout
{
   INHERIT( XWT_GTK_CONTAINER );
   BOOL bFill;
   BOOL bExpand;
   int iMode;
   int iPadding;
} XWT_GTK_LAYOUT, *PXWT_GTK_LAYOUT;


typedef struct tag_xwt_gtk_grid
{
   INHERIT( XWT_GTK_CONTAINER );
   BOOL bFill;
   BOOL bExpand;
   int iRows;
   int iCols;
   BOOL bShrink;
   int iYPad, iXPad;
} XWT_GTK_GRID, *PXWT_GTK_GRID;


typedef struct tag_xwt_gtk_calendar
{
  gboolean  settings[5];
  GtkWidget *window;
  GtkWidget *calendar;

  GtkWidget *vbox;
  GtkWidget *hbox;
  GtkWidget *hbbox;
  GtkWidget *button;
  GtkWidget *button1;
  GtkWidget *frame;
  GtkWidget *bbox;
} XWT_GTK_CALENDAR, *PXWT_GTK_CALENDAR;


BOOL xwt_gtk_createButton( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createFrameWindow( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createLabel( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createMenu( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createMenuItem( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createPane( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createTextbox( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createTextarea( PXWT_WIDGET xwtData );
BOOL xwt_gtk_createWindow( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createImage( PXWT_WIDGET wSelf );

BOOL xwt_gtk_createGrid( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createViewPort( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createRadioButton( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createFileSelection( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createCheckbox( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createSplitter( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createToggleButton( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createTreelist( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createBrowse( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createFontSelection( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createCalendar( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createCalendarModal( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createComboBox( PXWT_WIDGET wSelf );
BOOL xwt_gtk_createListBox( PXWT_WIDGET xwtData );
BOOL xwt_gtk_createProgressBar( PXWT_WIDGET xwtData );
BOOL xwt_gtk_createColorSelection( PXWT_WIDGET xwtData );
void xwt_gtk_ComboAddItem(PXWT_WIDGET xwtData, PHB_ITEM pComboArray );
BOOL xwt_gtk_imageLoad( PXWT_WIDGET xwtData, const char *fname );
void xwt_gtk_ListAddItem(PXWT_WIDGET xwtData, PHB_ITEM pComboArray );

BOOL xwt_gtk_splitter_create_with_mode( PXWT_WIDGET wWidget, int mode );


BOOL xwt_gtk_treelist_set_content( PXWT_WIDGET xwtData, PHB_ITEM pContent );
BOOL xwt_gtk_treelist_set_columns( PXWT_WIDGET xwtData, PHB_ITEM pCols );
BOOL xwt_gtk_treelist_create_columns( PXWT_WIDGET xwtData, int nCols );
BOOL xwt_gtk_treelist_set_colattr( PXWT_WIDGET xwtData, char *prop, void *data );

/* In the browse model, all the data we need is in the owner (XWTBrowse). */
BOOL xwt_gtk_browse_set_content( PXWT_WIDGET xwtData );

/*** XWT GTK ALIGNMENT ***/
void xwt_gtk_set_alignment( XWT_GTK_ALIGN* widget );

/*** Type translation between xharbour and gtk type representation ***/
int xwt_gtk_translate_type( int iType );
BOOL xwt_gtk_getxy( GtkWidget *wTop, int *xpos, int *ypos );

/** generic basic property managers */
BOOL xwt_gtk_base_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue );
BOOL xwt_gtk_base_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue );
BOOL xwt_gtk_setpgroup( PXWT_WIDGET widget, PHB_ITEM pValue );
BOOL xwt_gtk_base_getall( PXWT_WIDGET widget, PHB_ITEM pProps );
void xwt_gtk_base_signal_connect( PXWT_WIDGET widget );
void xwt_gtk_base_general_connect( PXWT_WIDGET widget );
void xwt_gtk_base_focus_connect( PXWT_WIDGET widget );

BOOL xwt_gtk_base_destroy( PXWT_WIDGET wWidget );
GtkWidget *xwt_gtk_base_topwidget( PXWT_WIDGET widget );

/** properties managers that are shared by many widgets */
BOOL xwt_gtk_container_destroy( PXWT_WIDGET widget );
BOOL xwt_gtk_container_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue );
BOOL xwt_gtk_container_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue );
BOOL xwt_gtk_container_getall( PXWT_WIDGET widget, PHB_ITEM pProps );
BOOL xwt_gtk_container_remove( PXWT_WIDGET wParent, PXWT_WIDGET wChild );
BOOL xwt_gtk_container_destroy( PXWT_WIDGET wWidget );
GtkWidget *xwt_gtk_container_topwidget( PXWT_WIDGET widget );

BOOL xwt_gtk_enframe( PXWT_WIDGET wWidget, GtkWidget *framed );
BOOL xwt_gtk_deframe( PXWT_WIDGET wWidget, GtkWidget *framed );

BOOL xwt_gtk_sensible_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue );
BOOL xwt_gtk_sensible_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue );
BOOL xwt_gtk_sensible_getall( PXWT_WIDGET widget, PHB_ITEM pProps );
BOOL xwt_gtk_sensible_destroy( PXWT_WIDGET widget );
BOOL xwt_gtk_sensibilize( PXWT_WIDGET wWidget, GtkWidget *master );
BOOL xwt_gtk_desensibilize( PXWT_WIDGET wWidget, GtkWidget *master );
GtkWidget *xwt_gtk_sensible_topwidget( PXWT_WIDGET widget );

BOOL xwt_gtk_align_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue );
BOOL xwt_gtk_align_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue );
BOOL xwt_gtk_align_getall( PXWT_WIDGET widget, PHB_ITEM pProps );
BOOL xwt_gtk_align_destroy( PXWT_WIDGET widget );
GtkWidget *xwt_gtk_align_topwidget( PXWT_WIDGET widget );

/** Window widget */
BOOL xwt_gtk_window_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue );
BOOL xwt_gtk_window_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue );
BOOL xwt_gtk_window_getall( PXWT_WIDGET widget, PHB_ITEM pRet );
BOOL xwt_gtk_window_destroy( PXWT_WIDGET widget );
BOOL xwt_gtk_window_connect( PXWT_WIDGET wWindow, PXWT_WIDGET wChild );
BOOL xwt_gtk_window_disconnect( PXWT_WIDGET wWindow );

GtkWidget *xwt_gtk_window_topwidget( PXWT_WIDGET widget );

/** Frame window widget */
void xwt_gtk_setMenuBar( PXWT_WIDGET xwtData, PHB_ITEM pMenuArray );
void xwt_gtk_resetMenuBar( PXWT_WIDGET xwtData );

/** Layout */
BOOL xwt_gtk_createLayout( PXWT_WIDGET wSelf );
BOOL xwt_gtk_layout_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue );
BOOL xwt_gtk_layout_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue );
BOOL xwt_gtk_layout_getall( PXWT_WIDGET widget, PHB_ITEM pProps );
BOOL xwt_gtk_layout_create_with_mode( PXWT_WIDGET wWidget );
BOOL xwt_gtk_layout_repack( PXWT_WIDGET wWidget );

/** Message box */
int xwt_gtk_message_box( PXWT_WIDGET xwtParent, PHB_ITEM pSettings );

/** Driver functions */
BOOL xwt_gtk_add( PXWT_WIDGET wWidget, PXWT_WIDGET wChild );
BOOL xwt_gtk_remove( PXWT_WIDGET wWidget, PXWT_WIDGET wChild );
BOOL xwt_gtk_connect( PXWT_WIDGET wWidget, PXWT_WIDGET wChild );
BOOL xwt_gtk_disconnect( PXWT_WIDGET wWidget );

int xwt_gtk_translate_key( unsigned int keyval );

#endif
