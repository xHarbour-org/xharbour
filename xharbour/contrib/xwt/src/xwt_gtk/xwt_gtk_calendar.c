/*
   XWT - xHarbour Windowing Toolkit

   (C) 2004 Luiz Rafael Culik

   $Id: xwt_gtk_calendar.c,v 1.2 2004/01/26 13:52:21 lculik Exp $

   GTK interface - File Selection Box 
*/

#include "hbapi.h"
#include <xwt_calendar.h>
#include <xwt_api.h>
#include <xwt_gtk.h>
#include <hbdate.h>

#define DEF_PAD 10
#define DEF_PAD_SMALL 5

#define TM_YEAR_BASE 1900
void calendar_set_flags (XWT_GTK_CALENDAR *calen)
{
  gint i;
  gint options = 0;
  for (i = 0;i < 5; i++) 
    if (calen->settings[i])
      {
	options = options + (1 << i);
      }
  if (calen->calendar)
    gtk_calendar_display_options (GTK_CALENDAR (calen->calendar), options);
}

static void calendar_ok_sel( GtkWidget *widget,  gpointer cb_data )
{
   PXWT_GTK_MODAL xwtFilew = (PXWT_GTK_MODAL) ((PXWT_WIDGET)cb_data)->widget_data;
   HB_ITEM itmFileName;
   guint  year,month,day;
   // this builds the Self object
   // If you use this macro, you must put it AFTER variable decl,
   // and BEFORE any other statement
   XWT_GTK_MAKESELF( (((PXWT_WIDGET)cb_data)->owner) );
  #if __GNUC__ <3
     gtk_calendar_get_date (GTK_CALENDAR(GTK_XWTCALENDAR_SELECTION_DIALOG( xwtFilew->a.main_widget)->calendar ),&year,&month,&day
   );
   #else
    gtk_calendar_get_date(
      GTK_CALENDAR( GTK_XWTCALENDAR_SELECTION_DIALOG  ( xwtFilew->main_widget)->calendar ),&year,&month,&day
   );

   #endif

   // itemPutC uses the char* parameter as it were const: it does not
   // mangles with that, it just creates a new local copy of the param.
   hb_itemPutD( &itmFileName, (long) year,(long) month,(long) day );

   //rising the updated event, to signal that we have a candidate filename
   xwt_rise_event( &Self, XWT_E_UPDATED, 1, &itmFileName );

   // now we can reset the modal status
   xwtFilew->modal = FALSE;
}

static void calendar_cancel_sel( GtkWidget *widget,  gpointer cb_data )
{
   PXWT_GTK_MODAL xwtFilew = (PXWT_GTK_MODAL) ((PXWT_WIDGET)cb_data)->widget_data;

   XWT_GTK_MAKESELF( (((PXWT_WIDGET)cb_data)->owner) );

   //rising the updated event, to signal that we have a candidate filename
   xwt_rise_event( &Self, XWT_E_CANCELED, 0);

   // now we can reset the modal status
   xwtFilew->modal = FALSE;
   // and say we have been canceled
   xwtFilew->canceled = TRUE;
}
void calendar_month_changed (GtkWidget    *widget,
                               gpointer cb_data )
{
   PHB_ITEM pData = hb_itemNew( NULL );
   guint year,month,day;
   XWT_GTK_MAKESELF( cb_data );
   gtk_calendar_get_date( GTK_CALENDAR ( widget) ,&year,&month,&day);
   hb_itemPutD( pData, (long) year,(long) month,(long) day );
   xwt_rise_event( &Self, XWT_E_CHANGED, 1, pData );
   hb_itemRelease( pData );

}

void calendar_day_selected (GtkWidget    *widget,
                            gpointer cb_data)
{
   PHB_ITEM pData = hb_itemNew( NULL );
   guint year,month,day;
   XWT_GTK_MAKESELF( cb_data );
   gtk_calendar_get_date( GTK_CALENDAR ( widget) ,&year,&month,&day);
   hb_itemPutD( pData, (long) year,(long) month,(long) day );
   xwt_rise_event( &Self, XWT_E_CHANGED, 1, pData );
   hb_itemRelease( pData );
}


void calendar_prev_month (GtkWidget    *widget,
                            gpointer cb_data )
{
   PHB_ITEM pData = hb_itemNew( NULL );
   guint year,month,day;
   XWT_GTK_MAKESELF( cb_data );
   gtk_calendar_get_date( GTK_CALENDAR ( widget) ,&year,&month,&day);
   hb_itemPutD( pData, (long) year,(long) month,(long) day );
   xwt_rise_event( &Self, XWT_E_CHANGED, 1, pData );
   hb_itemRelease( pData );

}

void calendar_next_month (GtkWidget    *widget,
                          gpointer cb_data)
{
   PHB_ITEM pData = hb_itemNew( NULL );
   guint year,month,day;
   XWT_GTK_MAKESELF( cb_data );
   gtk_calendar_get_date( GTK_CALENDAR ( widget) ,&year,&month,&day);
   hb_itemPutD( pData, (long) year,(long) month,(long) day );
   xwt_rise_event( &Self, XWT_E_CHANGED, 1, pData );
   hb_itemRelease( pData );

}

void calendar_prev_year (GtkWidget    *widget,
                         gpointer cb_data)
{
   PHB_ITEM pData = hb_itemNew( NULL );
   guint year,month,day;
   XWT_GTK_MAKESELF( cb_data );
   gtk_calendar_get_date( GTK_CALENDAR ( widget) ,&year,&month,&day);
   hb_itemPutD( pData, (long) year,(long) month,(long) day );
   xwt_rise_event( &Self, XWT_E_CHANGED, 1, pData );
   hb_itemRelease( pData );

}

void calendar_next_year (GtkWidget    *widget,
                         gpointer cb_data)
{
   PHB_ITEM pData = hb_itemNew( NULL );
   guint year,month,day;
   XWT_GTK_MAKESELF( cb_data );
   gtk_calendar_get_date( GTK_CALENDAR ( widget) ,&year,&month,&day);
   hb_itemPutD( pData, (long) year,(long) month,(long) day );
   xwt_rise_event( &Self, XWT_E_CHANGED, 1, pData );
   hb_itemRelease( pData );

}


BOOL xwt_gtk_createCalendar( PXWT_WIDGET xwtData )
{
   GtkWidget *filew;
   long lyear,lmonth,lday;
   hb_dateToday(&lyear,&lmonth,&lday );
       

   filew = gtk_calendar_new ();
  gtk_calendar_select_month ( GTK_CALENDAR(filew), lmonth, lyear);
  gtk_calendar_mark_day ( GTK_CALENDAR(filew),lday);	
  gtk_calendar_display_options(GTK_CALENDAR(filew),GTK_CALENDAR_SHOW_DAY_NAMES | GTK_CALENDAR_SHOW_HEADING);
   // we need both the owner of the widget, and the widget itself;
   // so it is useful to pass the xwt_gtk data.
  g_signal_connect (filew, "month_changed", 
		    G_CALLBACK (calendar_month_changed),
		    xwtData->owner);
  g_signal_connect (filew, "day_selected", 
		    G_CALLBACK (calendar_day_selected),
		    xwtData->owner);
  g_signal_connect (filew, "prev_month", 
		    G_CALLBACK (calendar_prev_month),
		    xwtData->owner);
  g_signal_connect (filew, "next_month", 
		   G_CALLBACK( calendar_next_month),
		    xwtData->owner);
  g_signal_connect (filew, "prev_year", 
		    G_CALLBACK (calendar_prev_year),
		    xwtData->owner);
  g_signal_connect (filew, "next_year", 
		    G_CALLBACK (calendar_next_year),
		    xwtData->owner);

   // you ALWAYS need to set the xwtData->widget_data.
   // if no driver level widget wrapper is needed, you can
   // use the gtkWidget here, and set NULL for the destructor.
   
   xwtData->destructor = NULL;
   xwtData->get_main_widget = xwtData->get_top_widget = xwt_gtk_get_topwidget_neuter;
   
   xwtData->widget_data = (void *)filew;
   // xwtData->widget_data is just allocated with hb_xgrab;
   // an xfree will be enough to get rid of it.
   gtk_widget_show_all( filew);
   TraceLog(NULL,"do new do calendat 2)");
   return TRUE;
}

BOOL xwt_gtk_createCalendarModal( PXWT_WIDGET xwtData )
{

   GtkWidget *filew;
   PXWT_GTK_MODAL xwtCalendarW;
   long lyear,lmonth,lday;

   xwtCalendarW = (PXWT_GTK_MODAL) hb_xgrab( sizeof( XWT_GTK_MODAL ) );

   filew = gtk_XwtCalendar_selection_dialog_new("");
   // this widget is NOT displayed by default
   #if __GNUC__ <3
   xwtCalendarW->a.main_widget = filew;
   #else
   xwtCalendarW->main_widget = filew;
   #endif
   xwtCalendarW->modal = FALSE;
   xwtCalendarW->canceled = FALSE;

  hb_dateToday(&lyear,&lmonth,&lday );

//  xwtCalendarW->calendar = calendar;
//  calendar_set_flags (&xwtCalendarW);
  gtk_calendar_select_month ( GTK_CALENDAR( GTK_XWTCALENDAR_SELECTION_DIALOG(filew)->calendar), lmonth, lyear);
  gtk_calendar_mark_day ( GTK_CALENDAR( GTK_XWTCALENDAR_SELECTION_DIALOG(filew)->calendar),lday);	
  gtk_calendar_display_options(GTK_CALENDAR( GTK_XWTCALENDAR_SELECTION_DIALOG(filew)->calendar),GTK_CALENDAR_SHOW_DAY_NAMES | GTK_CALENDAR_SHOW_HEADING  );

  g_signal_connect ( G_OBJECT (GTK_XWTCALENDAR_SELECTION_DIALOG(filew)->button1),
   "clicked", 
		    G_CALLBACK (calendar_ok_sel), 
		    xwtData);
  g_signal_connect ( G_OBJECT (GTK_XWTCALENDAR_SELECTION_DIALOG(filew)->button),
   "clicked", 
		    G_CALLBACK (calendar_cancel_sel), 
		    xwtData);
   xwtData->widget_data = xwtCalendarW;
   // xwtData->widget_data is just allocated with hb_xgrab;
   // an xfree will be enough to get rid of it.
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = xwt_gtk_get_mainwidget_base;
   xwtData->get_top_widget = xwt_gtk_get_mainwidget_base;

		    
  return TRUE;

//  gtk_widget_grab_default (button1);
}
