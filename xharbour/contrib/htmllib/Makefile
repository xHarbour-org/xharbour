#
# $Id: Makefile,v 1.3 2003/05/28 03:10:17 paultucker Exp $
#

ROOT = ../../

C_SOURCES=\

PRG_HEADERS=\
	include/html.ch \
	include/colors.ch \
	include/default.ch \
	include/ferror.ch \
	include/forms.ch \
	include/lowfiles.ch \
 
PRG_SOURCES=\
    ohtm.prg \
    ocgi.prg \
    oedit.prg \
    oframe.prg \
    oini.prg \
    ofile.prg \
    jwindow.prg \
    htmutil.prg \
    counter.prg \
    hterrsys.prg \
    htmbrows.prg \
    jlist.prg \

LIBNAME=html
ifneq ($(HB_ARCHITECTURE),dos)
MT_LIBNAME=htmlmt
endif

include $(TOP)$(ROOT)config/lib.cf
include $(TOP)$(ROOT)config/header.cf