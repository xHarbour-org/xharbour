/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/. 
 */
#include <glib.h>
#include "xwt_calendar.h"
#include <xwt_gtk.h>

#define DEF_PAD 10
#define DEF_PAD_SMALL 5


static void gtk_XwtCalendar_selection_dialog_class_init (GtkXwtCalendarSelectionDialogClass *klass);

static void gtk_XwtCalendar_selection_dialog_init (GtkXwtCalendarSelectionDialog *colorseldiag);

static GtkWindowClass *XwtCalendar_selection_dialog_parent_class = NULL;


/***************************/
/* GtkXwtCalendarSelectionDialog */
/***************************/

GType
gtk_XwtCalendar_selection_dialog_get_type (void)
{
  static GType XwtCalendar_selection_dialog_type = 0;
  
  if (!XwtCalendar_selection_dialog_type)
    {
      static const GTypeInfo XwtCalendarsel_diag_info =
      {
	sizeof (GtkXwtCalendarSelectionDialogClass),
	NULL,		/* base_init */
	NULL,		/* base_finalize */
	(GClassInitFunc) gtk_XwtCalendar_selection_dialog_class_init,
	NULL,		/* class_finalize */
	NULL,		/* class_data */
	sizeof (GtkXwtCalendarSelectionDialog),
	0,		/* n_preallocs */
	(GInstanceInitFunc) gtk_XwtCalendar_selection_dialog_init,
      };
      
      XwtCalendar_selection_dialog_type =
	g_type_register_static (GTK_TYPE_DIALOG, "GtkXwtCalendarSelectionDialog",
				&XwtCalendarsel_diag_info, 0);
    }
  
  return XwtCalendar_selection_dialog_type;
}

static void
gtk_XwtCalendar_selection_dialog_class_init (GtkXwtCalendarSelectionDialogClass *klass)
{
  XwtCalendar_selection_dialog_parent_class = g_type_class_peek_parent (klass);
}

static void
gtk_XwtCalendar_selection_dialog_init (GtkXwtCalendarSelectionDialog *XwtCalendarseldiag)
{
  GtkWidget * frame;
  gint i;
  for (i = 0; i < 5; i++) {
    XwtCalendarseldiag->settings[i] = 0;
  }

/*  XwtCalendarseldiag->window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (XwtCalendarseldiag->window), "GtkCalendar Example");
  gtk_container_set_border_width (GTK_CONTAINER (XwtCalendarseldiag->window), 5);
  gtk_window_set_resizable (GTK_WINDOW (XwtCalendarseldiag->window), FALSE);

  XwtCalendarseldiag->vbox = gtk_vbox_new (FALSE, DEF_PAD);
  gtk_container_add (GTK_CONTAINER (XwtCalendarseldiag->window), XwtCalendarseldiag->vbox);


  XwtCalendarseldiag->hbox = gtk_hbox_new (FALSE, DEF_PAD);
  gtk_box_pack_start (GTK_BOX (XwtCalendarseldiag->vbox), XwtCalendarseldiag->hbox, TRUE, TRUE, DEF_PAD);
  XwtCalendarseldiag->hbbox = gtk_hbutton_box_new ();
  gtk_box_pack_start (GTK_BOX (XwtCalendarseldiag->hbox), XwtCalendarseldiag->hbbox, FALSE, FALSE, DEF_PAD);
  gtk_button_box_set_layout (GTK_BUTTON_BOX (XwtCalendarseldiag->hbbox), GTK_BUTTONBOX_SPREAD);
  gtk_box_set_spacing (GTK_BOX (XwtCalendarseldiag->hbbox), 5);


  XwtCalendarseldiag->frame = gtk_frame_new ("Calendar");
  gtk_box_pack_start(GTK_BOX (XwtCalendarseldiag->hbbox), XwtCalendarseldiag->frame, FALSE, TRUE, DEF_PAD);
  XwtCalendarseldiag->calendar = 


  gtk_container_add (GTK_CONTAINER (XwtCalendarseldiag->frame), XwtCalendarseldiag->calendar);


  XwtCalendarseldiag->bbox = gtk_hbutton_box_new ();
  gtk_box_pack_start (GTK_BOX (XwtCalendarseldiag->vbox), XwtCalendarseldiag->bbox, FALSE, FALSE, 0);
  gtk_button_box_set_layout (GTK_BUTTON_BOX (XwtCalendarseldiag->bbox), GTK_BUTTONBOX_END);
  */
  frame = gtk_frame_new (NULL);
  gtk_frame_set_shadow_type (GTK_FRAME (frame), GTK_SHADOW_NONE);
  gtk_container_add (GTK_CONTAINER (GTK_DIALOG (XwtCalendarseldiag)->vbox), frame);
  gtk_container_set_border_width (GTK_CONTAINER (frame), 10); 
  gtk_widget_show (frame); 
  
  XwtCalendarseldiag->calendar = gtk_calendar_new (); 
//  gtk_color_selection_set_has_palette (GTK_COLOR_SELECTION(colorseldiag->colorsel), FALSE); 
//  gtk_color_selection_set_has_opacity_control (GTK_COLOR_SELECTION(colorseldiag->colorsel), FALSE);
  gtk_container_add (GTK_CONTAINER (frame), XwtCalendarseldiag->calendar);
  gtk_widget_show (XwtCalendarseldiag->calendar);


  XwtCalendarseldiag->button1 = gtk_dialog_add_button (GTK_DIALOG (XwtCalendarseldiag),
                                                   GTK_STOCK_OK,
                                                   GTK_RESPONSE_OK);
  
  XwtCalendarseldiag->button = gtk_dialog_add_button (GTK_DIALOG (XwtCalendarseldiag),
                                                       GTK_STOCK_CANCEL,
                                                      GTK_RESPONSE_CANCEL);

}

GtkWidget*
gtk_XwtCalendar_selection_dialog_new (void)
{
  GtkXwtCalendarSelectionDialog *XwtCalendarseldiag;
  
  XwtCalendarseldiag = g_object_new (GTK_TYPE_XWTCALENDAR_SELECTION_DIALOG, NULL);
//  gtk_window_set_title (GTK_WINDOW (XwtCalendarseldiag), title);
  gtk_window_set_resizable (GTK_WINDOW (XwtCalendarseldiag), FALSE);
  
  return GTK_WIDGET (XwtCalendarseldiag);
}
