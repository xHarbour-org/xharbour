/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk.h,v 1.6 2003/04/12 23:47:15 jonnymind Exp $

   GTK interface
*/

#ifndef XWT_GTK_H
#define XWT_GTK_H

#include <gtk/gtk.h>
#include <xwt_api.h>

#define XWT_GTK_MAKESELF( var )\
   HB_ITEM Self; \
   Self.type = HB_IT_OBJECT;\
   Self.item.asArray.value = (PHB_BASEARRAY ) (var);

gboolean xwt_idle_function( gpointer data );
void *xwt_gtk_get_topwidget_neuter( void *);

typedef struct tag_xwt_gtk_base
{
   GtkWidget *main_widget;
   PHB_BASEARRAY owner;

} XWT_GTK_BASE, *PXWT_GTK_BASE;

typedef struct tag_xwt_gtk_wnd
{
   XWT_GTK_BASE;
   GtkWidget *window;
} XWT_GTK_WND, *PXWT_GTK_WND;

typedef struct tag_xwt_gtk_framewnd
{
   XWT_GTK_WND;
   GtkWidget *vbox;
   GtkWidget *menu_bar;
   GtkWidget *menu_box;
   GtkWidget *status_bar;
} XWT_GTK_FRAMEWND, *PXWT_GTK_FRAMEWND;


typedef struct tag_xwt_gtk_menuitem
{
   XWT_GTK_BASE;
   GtkWidget *hbox;
   GtkWidget *image;
   GtkWidget *label;
   GtkWidget *align;
} XWT_GTK_MENUITEM, *PXWT_GTK_MENUITEM;


typedef struct tag_xwt_gtk_align
{
   XWT_GTK_BASE;
   GtkWidget *align;
   int iVAlign;
   int iHAlign;
} XWT_GTK_ALIGN, *PXWT_GTK_ALIGN;

typedef struct tag_xwt_gtk_sensible
{
   XWT_GTK_ALIGN;
   GtkWidget *evt_window;
} XWT_GTK_SENSIBLE, *PXWT_GTK_SENSIBLE;

typedef struct tag_xwt_gtk_image
{
   XWT_GTK_SENSIBLE;
   GdkPixmap *pixmap;
   char *filename;
} XWT_GTK_IMAGE, *PXWT_GTK_IMAGE;


typedef struct tag_xwt_gtk_container
{
   XWT_GTK_ALIGN;
   GtkWidget *frame;
} XWT_GTK_CONTAINER, *PXWT_GTK_CONTAINER;


typedef struct tag_xwt_gtk_laycontainer
{
   XWT_GTK_CONTAINER;
   BOOL bFill;
   BOOL bExpand;
} XWT_GTK_LAYCONTAINER, *PXWT_GTK_LAYCONTAINER;


typedef struct tag_xwt_gtk_layout
{
   XWT_GTK_LAYCONTAINER;
   int iMode;
   int iPadding;
} XWT_GTK_LAYOUT, *PXWT_GTK_LAYOUT;


typedef struct tag_xwt_gtk_grid
{
   XWT_GTK_LAYCONTAINER;
   int iRows;
   int iCols;
   BOOL bShrink;
   int iYPad, iXPad;
} XWT_GTK_GRID, *PXWT_GTK_GRID;


PXWT_WIDGET xwt_gtk_createButton( PHB_ITEM pSelf );
PXWT_WIDGET xwt_gtk_createFrameWindow( PHB_ITEM pSelf );
PXWT_WIDGET xwt_gtk_createLabel( PHB_ITEM pSelf );
PXWT_WIDGET xwt_gtk_createMenu( PHB_ITEM pSelf );
PXWT_WIDGET xwt_gtk_createMenuItem( PHB_ITEM pSelf );
PXWT_WIDGET xwt_gtk_createPane( PHB_ITEM pSelf );
PXWT_WIDGET xwt_gtk_createTextbox( PHB_ITEM pSelf );
PXWT_WIDGET xwt_gtk_createWindow( PHB_ITEM pSelf );
PXWT_WIDGET xwt_gtk_createImage( PHB_ITEM pSelf );
PXWT_WIDGET xwt_gtk_createLayout( PHB_ITEM pSelf );
PXWT_WIDGET xwt_gtk_createGrid( PHB_ITEM pSelf );
PXWT_WIDGET xwt_gtk_createViewPort( PHB_ITEM pSelf );
PXWT_WIDGET xwt_gtk_createRadioButton( PHB_ITEM pSelf );
PXWT_WIDGET xwt_gtk_createFileSelection( PHB_ITEM pSelf );


void xwt_gtk_setMenuBar( PXWT_WIDGET xwtData, PHB_ITEM pMenuArray );
void xwt_gtk_resetMenuBar( PXWT_WIDGET xwtData, PHB_ITEM pMenuArray );
BOOL xwt_gtk_imageLoad( PXWT_WIDGET xwtData, const char *fname );
BOOL xwt_gtk_image_setSensible( PXWT_WIDGET wSelf );

BOOL xwt_gtk_layout_create_with_mode( PXWT_WIDGET wWidget, int mode );

BOOL xwt_gtk_container_set_box( PXWT_WIDGET wWidget );
BOOL xwt_gtk_container_reset_box( PXWT_WIDGET wWidget );

void *container_get_mainwidget( void *data );
void *container_get_topwidget( void *data );

void *xwt_gtk_get_topwidget_neuter( void *data );
void *xwt_gtk_get_mainwidget_base( void *data );
void *xwt_gtk_get_topwidget_base( void *data );
void *xwt_gtk_get_topwidget_align( void *data );
void *xwt_gtk_get_topwidget_sensible( void *data );

/*** Putting a widget in a frame ****/
GtkWidget *xwt_gtk_enframe( GtkWidget *framed );
void xwt_gtk_deframe( GtkWidget *frame, GtkWidget *framed );

/*** XWT GTK ALIGNMENT ***/
void xwt_gtk_set_alignment( XWT_GTK_ALIGN* widget );

#endif
