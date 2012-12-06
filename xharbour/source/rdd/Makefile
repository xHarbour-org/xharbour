#
# $Id$
#

ROOT = ../../

C_SOURCES=\
	dbcmd.c \
	workarea.c \
	wacore.c \
	wafunc.c \
	dbf1.c \
	dbf1net.c \
	dbnubs.c \
	dbrename.c \
	delim1.c \
	sdf1.c \
	hbdbsort.c \
	dbcmdx.c \
	fieldhb.c \
	dbcmdhb.c \
	dbcmd53.c \
	dbdrop.c \
	dbexists.c \
	dbrename.c \
	rddinfo.c \

PRG_SOURCES=\
	dbdelim.prg \
	dbfuncs.prg \
	dbjoin.prg \
	dbtotal.prg \
	dblist.prg \
	dbsdf.prg \
	dbsort.prg \
	dbstrux.prg \
	dbupdat.prg \
	rddord.prg \
	rddsys.prg \
	dblistx.prg \
	dbupdatx.prg \
	dbjoinx.prg \
	dbsortx.prg \
	dbstruxx.prg \
	dbtotalx.prg \
	dbstruxu.prg \
 
ifeq ($(HB_MT),MT)
   MT_LIBNAME=rddmt
endif

LIBNAME=rdd


# The list of all valid DB drivers is defined in config/global.cf.
DIRS=$(HB_DB_DRIVERS)

include $(TOP)$(ROOT)config/lib.cf
include $(TOP)$(ROOT)config/dir.cf
