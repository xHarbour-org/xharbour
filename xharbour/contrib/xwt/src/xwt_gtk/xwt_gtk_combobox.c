/*
   XWT - xHarbour Windowing Toolkit

   (C) 2004 Luiz Rafael Culik

   $Id: xwt_gtk_radiobutton.c,v 1.3 2003/06/08 14:05:36 jonnymind Exp $

   GTK interface - management of combobox
*/

#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static void combo_list_append(GtkCombo *combo,char * string)
{
   GtkWidget *li=gtk_list_item_new_with_label(string);
   gtk_container_add(GTK_CONTAINER(combo->list),li);
   gtk_widget_show(li);
}


void xwt_gtk_ComboAddItem(PXWT_WIDGET xwtData, PHB_ITEM pComboArray )
{
   GtkCombo * combo;
   PHB_ITEM pItem;
   ULONG ulPos;
   combo  = (GtkCombo*) xwtData->widget_data;

   for ( ulPos = 0; ulPos < hb_arrayLen(pComboArray); ulPos++ )
   {  
      pItem = hb_arrayGetItemPtr( pComboArray, ulPos + 1 );
      combo_list_append(combo,hb_itemGetCPtr(pItem));
   }      

}


BOOL xwt_gtk_createComboBox( PXWT_WIDGET xwtData )
{
   GtkWidget *button;

   button = gtk_combo_new ();
   // add a container to the window

   xwtData->widget_data = button;
   xwtData->destructor = NULL;
   xwtData->get_main_widget = xwtData->get_top_widget = xwt_gtk_get_topwidget_neuter;

   gtk_widget_show( button );

   return TRUE;
}

