/*
   XWT - xHarbour Windowing Toolkit

   (C) 2004 Luiz Rafael Culik

   $Id: xwt_calendar.h,v 1.2 2004/01/26 13:52:21 lculik Exp $

   GTK interface - calendar widget
*/

#ifndef __GTK_XWTCALENDAR_SELECTION_DIALOG_H__
#define __GTK_XWTCALENDAR_SELECTION_DIALOG_H__

#include <gtk/gtkdialog.h>
//#include <gtk/gtkcolorsel.h>
#include <gtk/gtkvbox.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define GTK_TYPE_XWTCALENDAR_SELECTION_DIALOG            (gtk_XwtCalendar_selection_dialog_get_type ())
#define GTK_XWTCALENDAR_SELECTION_DIALOG(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GTK_TYPE_XWTCALENDAR_SELECTION_DIALOG, GtkXwtCalendarSelectionDialog))
#define GTK_XWTCALENDAR_SELECTION_DIALOG_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GTK_TYPE_XWTCALENDAR_SELECTION_DIALOG, GtkXwtCalendarSelectionDialogClass))
#define GTK_IS_XWTCALENDAR_SELECTION_DIALOG(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GTK_TYPE_XWTCALENDAR_SELECTION_DIALOG))
#define GTK_IS_XWTCALENDAR_SELECTION_DIALOG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GTK_TYPE_XWTCALENDAR_SELECTION_DIALOG))
#define GTK_XWTCALENDAR_SELECTION_DIALOG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GTK_TYPE_XWTCALENDAR_SELECTION_DIALOG, GtkXwtCalendarSelectionDialogClass))


typedef struct _GtkXwtCalendarSelectionDialog       GtkXwtCalendarSelectionDialog;
typedef struct _GtkXwtCalendarSelectionDialogClass  GtkXwtCalendarSelectionDialogClass;


struct _GtkXwtCalendarSelectionDialog
{
  GtkDialog parent_instance;
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
};

struct _GtkXwtCalendarSelectionDialogClass
{
  GtkDialogClass parent_class;

  /* Padding for future expansion */
  void (*_gtk_reserved1) (void);
  void (*_gtk_reserved2) (void);
  void (*_gtk_reserved3) (void);
  void (*_gtk_reserved4) (void);
};


/* XwtCalendarSelectionDialog */ 
GType      gtk_XwtCalendar_selection_dialog_get_type (void) G_GNUC_CONST;
GtkWidget* gtk_XwtCalendar_selection_dialog_new      (char * title); 
void       gtk_XwtCalendar_GetDate( GtkXwtCalendarSelectionDialog *p, long *year,long *month,long *day);


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_COLOR_SELECTION_DIALOG_H__ */
