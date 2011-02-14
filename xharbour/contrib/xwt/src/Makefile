##################################
# $Id$
#
# Makefile for XWT library
#

ROOT = ../../../

C_SOURCES=\
    xwt_api.c \

PRG_SOURCES=\
    bcolumn.prg \
    browse.prg \
    button.prg \
    calendar.prg \
    checkbox.prg \
    combobox.prg \
    container.prg \
    editbox.prg \
    event.prg \
    filesel.prg \
    fontsel.prg \
    framewindow.prg \
    grid.prg \
    hex.prg \
    image.prg \
    inputmask.prg \
    label.prg \
    laycontainer.prg \
    layout.prg \
    listbox.prg \
    menu.prg \
    menuitem.prg \
    msgbox.prg \
    notebook.prg \
    pane.prg \
    radiobutton.prg \
    splitter.prg \
    textbox.prg \
    togglebutton.prg \
    treeitem.prg \
    treelist.prg \
    viewport.prg \
    widget.prg \
    window.prg \

LIBNAME=xwt

include $(TOP)$(ROOT)config/lib.cf

HB_INC_COMPILE += -I$(TOP)../include
CFLAGS += -I$(TOP)../include

ifeq ($(HB_ARCHITECTURE),w32)
DIRS=\
    xwt_win \

else
DIRS=\
    xwt_gtk \

endif
include $(TOP)$(ROOT)config/dir.cf
