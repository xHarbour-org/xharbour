/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk.h,v 1.2 2003/04/07 10:27:45 jonnymind Exp $

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

typedef struct tag_xwt_gtk_wnd
{
   GtkWidget *window;
   GtkWidget *main_widget;

   PHB_BASEARRAY owner;

} XWT_GTK_WND, *PXWT_GTK_WND;

typedef struct tag_xwt_gtk_framewnd
{
   GtkWidget *window;
   GtkWidget *vbox;
   GtkWidget *menu_bar;
   GtkWidget *main_widget;
   GtkWidget *status_bar;
   /* Owner object */
   PHB_BASEARRAY owner;
} XWT_GTK_FRAMEWND, *PXWT_GTK_FRAMEWND;


typedef struct tag_xwt_gtk_image
{
   GtkImage *image;
   GdkPixmap *pixmap;
   GtkWidget *evt_window;
   char *filename;
   PHB_BASEARRAY owner;
} XWT_GTK_IMAGE, *PXWT_GTK_IMAGE;


typedef struct tag_xwt_gtk_layout
{
   int iMode;
   BOOL bFill;
   BOOL bExpand;
   int iPadding;

   GtkWidget *frame;
   GtkWidget *layout; // it will be an HBox or a Vbox depending on iMode;
   // We should not need an owner; leaving it here for future reference
   // (Maybe option list boxes?)
   PHB_BASEARRAY owner;
} XWT_GTK_LAYOUT, *PXWT_GTK_LAYOUT;

typedef struct tag_xwt_gtk_pane
{
   GtkWidget *frame;
   GtkWidget *pane; // main widget
   // We should not need an owner; leaving it here for future reference
   // (Maybe option list boxes?)
   PHB_BASEARRAY owner;
} XWT_GTK_PANE, *PXWT_GTK_PANE;

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

void xwt_gtk_setMenuBar( PXWT_WIDGET xwtData, PHB_ITEM pMenuArray );
void xwt_gtk_resetMenuBar( PXWT_WIDGET xwtData, PHB_ITEM pMenuArray );
BOOL xwt_gtk_imageLoad( PXWT_WIDGET xwtData, const char *fname );
BOOL xwt_gtk_image_setSensible( PXWT_WIDGET wSelf );

BOOL xwt_gtk_layout_create_with_mode( PXWT_WIDGET wWidget, int mode );
BOOL xwt_gtk_layout_set_box( PXWT_WIDGET wWidget );
BOOL xwt_gtk_layout_reset_box( PXWT_WIDGET wWidget );

BOOL xwt_gtk_pane_set_box( PXWT_WIDGET wWidget );
BOOL xwt_gtk_pane_reset_box( PXWT_WIDGET wWidget );

/*** Putting a widget in a frame ****/
GtkWidget *xwt_gtk_enframe( GtkWidget *framed );
void xwt_gtk_deframe( GtkWidget *frame, GtkWidget *framed );


#endif
