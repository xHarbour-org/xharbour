/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_api.h,v 1.2 2003/04/18 13:28:49 jonnymind Exp $

   XWT DRIVER PROGRAMMING INTERFACE - header
*/


#ifndef XWT_API_H
#define XWT_API_H

#include "xwt.ch"
#include "hbapi.h"

int xwt_rise_event( PHB_ITEM pObject, int EventType, int argc, ... );

/* This data is used as RawWidget and passed to the internal function.
   To simplify development of specific platform drivers, some key rules
   are codified as virtual functions in "handler" member*/
typedef struct tag_xwt_widget
{
   // type is at disposal of drivers to interpret widget data correctly
   int type; 
    
   void (*destructor)(void *data);
   void *(*get_main_widget)( void *data );
   void *(*get_top_widget)( void *data );

   // pointer to raw widget data that is known by virtual functions
   void *widget_data;

   // back reference to xharbour object owner
   PHB_BASEARRAY owner;

} XWT_WIDGET, *PXWT_WIDGET;


#define XWT_CREATE_WIDGET( name ) \
      { name = (PXWT_WIDGET) hb_xgrab( sizeof( XWT_WIDGET )); }

typedef struct tag_xwt_position
{
   int x;
   int y;
} XWT_POSITION, *PXWT_POSITION;


typedef struct tag_xwt_size
{
   int width;
   int height;
} XWT_SIZE, *PXWT_SIZE;


typedef struct tag_xwt_rect
{
   int x;
   int y;
   int width;
   int height;
} XWT_RECT, *PXWT_RECT;


typedef struct tag_xwt_property
{
   int type;
   union {
      int number;
      XWT_POSITION position;
      XWT_SIZE size;
      XWT_RECT rect;
      BOOL setting;
      PXWT_WIDGET widget;
      const char *text;
      void *data;
   } value;
} XWT_PROPERTY, *PXWT_PROPERTY;

#define XWT_CREATE_PROPERTY( name ) \
      { PROPERTY = (PXWT_PROPERTY) hb_xgrab( sizeof( XWT_PROPERTY )); }

/********************************************************
* This driver functions must be reimplemented by drivers
*********************************************************/
BOOL xwt_drv_set_property( PXWT_WIDGET wSelf, PXWT_PROPERTY prop );
BOOL xwt_drv_get_property( PXWT_WIDGET wSelf, PXWT_PROPERTY prop );

// Text is automagically updated in the ::cText property of widget.
BOOL xwt_drv_add( PXWT_WIDGET wSelf, PXWT_WIDGET wChild );
BOOL xwt_drv_remove( PXWT_WIDGET wSelf, PXWT_WIDGET wChild );

BOOL xwt_drv_create( PXWT_WIDGET wSelf );
BOOL xwt_drv_destroy( PXWT_WIDGET wSelf );

/* executes a widget in modal state */
void xwt_drv_modal( PXWT_WIDGET wSelf );

BOOL xwt_drv_init( int argc, char **argv );
BOOL xwt_drv_quit();
BOOL xwt_drv_process_events();

#endif
