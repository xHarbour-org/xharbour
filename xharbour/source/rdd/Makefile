#
# $Id: Makefile,v 1.2 2002/08/28 00:33:05 lculik Exp $
#

ROOT = ../../

C_SOURCES=\
	dbcmd.c \
	workarea.c \
	dbf1.c \
	dbnubs.c \
	sdf1.c \
	delim1.c \
	hbdbsort.c \

PRG_SOURCES=\
	dbf0.prg \
   dbfuncs.prg \
   dbtotal.prg \
	dblist.prg \
	dbsort.prg \
	dbstrux.prg \
	dbupdat.prg \
	sdf0.prg \
	delim0.prg \
	rddord.prg \
	rddsys.prg \
 
ifeq ($(HB_MT),MT)
LIBNAME=rddmt
ARCH_SUFFIX=_mt
else
LIBNAME=rdd
endif


# The list of all valid DB drivers is defined in config/global.cf.
DIRS=$(HB_DB_DRIVERS)

include $(TOP)$(ROOT)config/lib.cf
include $(TOP)$(ROOT)config/dir.cf
