##################################
# $Id$
#
# Makefile for XWT GTK interface
#

ROOT = ../../../../

C_SOURCES=\
    xwt_calendar.c \
    xwt_gtk.c \
    xwt_gtk_browse.c \
    xwt_gtk_button.c \
    xwt_gtk_calendar.c \
    xwt_gtk_checkbox.c \
    xwt_gtk_colorselect.c \
    xwt_gtk_combobox.c \
    xwt_gtk_container.c \
    xwt_gtk_editbox.c \
    xwt_gtk_fileselect.c \
    xwt_gtk_fontselect.c \
    xwt_gtk_framewnd.c \
    xwt_gtk_grid.c \
    xwt_gtk_image.c \
    xwt_gtk_label.c \
    xwt_gtk_layout.c \
    xwt_gtk_listbox.c \
    xwt_gtk_menu.c \
    xwt_gtk_menuitem.c \
    xwt_gtk_msgbox.c \
    xwt_gtk_notebook.c \
    xwt_gtk_pane.c \
    xwt_gtk_progressbar.c \
    xwt_gtk_radiobutton.c \
    xwt_gtk_splitter.c \
    xwt_gtk_textbox.c \
    xwt_gtk_togglebutton.c \
    xwt_gtk_treelist.c \
    xwt_gtk_viewport.c \
    xwt_gtk_window.c \

LIBNAME=xwt_gtk

include $(TOP)$(ROOT)config/lib.cf

GTK_CFLAGS := $(shell pkg-config --cflags gtk+-2.0 )
CFLAGS += -I$(TOP) -I$(TOP)../../include $(GTK_CFLAGS)
