/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_api.h,v 1.8 2004/01/26 20:30:08 lculik Exp $

   XWT DRIVER PROGRAMMING INTERFACE - header
*/


#ifndef XWT_API_H
#define XWT_API_H

#include "hbapi.h"

#define XWT_WIDGET_SIGN 0x64F6E80A


/* Macro to request a specific XWT module, for both C and Harbour level */
#define XWT_MODULE_REQUEST( id )           XWT_MODULE_REQUEST_( id )
#define XWT_MODULE_REQUEST_( id )          HB_FUNC_EXTERN( HB_XWTMOD_##id ); \
                                        void hb_xwtmod_ForceLink( void ) \
                                        { \
                                           HB_FUNCNAME( HB_XWTMOD_##id )(); \
                                        }
                                        
/* Macro to publish a specific XWT module, for both C and Harbour level */
#define XWT_MODULE_ANNOUNCE( id )          HB_FUNC( HB_XWTMOD_##id ) {}

#define XWT_EVENT_NAME_SIZE 16

/* This data is used as RawWidget and passed to the internal function.
   To simplify development of specific platform drivers, some key rules
   are codified as virtual functions in "handler" member*/
typedef struct tag_xwt_widget
{
   ULONG sign;
   
   // type is at disposal of drivers to interpret widget data correctly
   int type; 
   
   // pointer to raw widget data that is known by underlying driver
   void *widget_data;
    
   // Destroys the underlying data.
   BOOL (*destroy)( struct tag_xwt_widget *widget );
   
   // Sets a given property to a (variable type of) value
   BOOL (*set_property)( struct tag_xwt_widget *widget, char *prop, PHB_ITEM pValue );
   
   // Send a group change; pValue is a hash of properties to be changed in one step
   // useful to move, resize or reset the whole geometry.
   // drivers must cache all the changes and apply them at once on the best function
   BOOL (*set_pgroup)( struct tag_xwt_widget *widget, PHB_ITEM pValue );
   
   // retreive a single property of the underlying widget
   BOOL (*get_property)( struct tag_xwt_widget *widget, char *prop, PHB_ITEM pValue );
   
   // creates a hash containing all the properties valid for this widget.
   // properties are to be put in the PHB_ITEM pProps that is to be provided by the caller,
   // the modules will receive it ready to accept properties
   BOOL (*get_all_properties)( struct tag_xwt_widget *widget, PHB_ITEM pProps );

   HB_ITEM hbOwner; // shell holding the owner
   PHB_ITEM pOwner; // commodity reference to the owner object

} XWT_WIDGET, *PXWT_WIDGET;


typedef struct tag_xwt_driver
{
   // name of the driver
   char name[12];
   
   // initializes the driver
   BOOL (*init)( int argc, char **argv );
   // close the widget subsystem
   BOOL (*quit)(void);
   // call the event processor loop.
   BOOL (*process_events)(void);
   
   // executes a widget in modal state
   void (*do_modal)( PXWT_WIDGET wSelf );

   // creates a widget of a given type
   BOOL (*create)( PXWT_WIDGET wSelf );
   // creates the unmissable alert box and shows it modally
   int (*message_box)( PXWT_WIDGET wParent, PHB_ITEM pHashSetting );
   // adds a subwidget to an existing widget
   BOOL (*add) ( PXWT_WIDGET wSelf, PXWT_WIDGET wChild );
   // removes a subwidget from an existing widget
   BOOL (*remove)( PXWT_WIDGET wSelf, PXWT_WIDGET wChild );
   // Connects a widget to a paret representor
   // tipically connects panes or layouts to windows or boxes.
   BOOL (*connect)( PXWT_WIDGET wSelf, PXWT_WIDGET wChild );
   BOOL (*disconnect)( PXWT_WIDGET wSelf );
   
} XWT_DRIVER, *PXWT_DRIVER;


void xwt_register_driver( PXWT_DRIVER drv );
int xwt_rise_event( PHB_ITEM pObject, char *szEventType, int argc, ... );
void xwt_widget_set_owner( PXWT_WIDGET widget, PHB_ITEM owner );

#endif
