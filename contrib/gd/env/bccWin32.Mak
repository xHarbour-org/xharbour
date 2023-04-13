#-- path vari -----------
OUTDIR=build
OBJDIR=obj
SRCDIR=source
LIBDIR=lib


#-- HARBOUR COMPILER ----
hbc     = $(HBR_DIR)\BIN\harbour
hbcdebug =
#hbcdebug = -p -b
hbcflags  = -p -n -w2 -es2 -i$(APP_DIR)\include;$(XD_DIR)\include;$(HBR_DIR)\include;$(WH_DIR)\include

#-- C COMPILER ----------
cc      = bcc32
cdebug  =
#cdebug  = -gb
#cflags   = -O2 -tW -M -c -DHB_OS_WIN_32_USED -DHB_API_MACROS -DHB_STACK_MACROS -DHB_OS_WIN_32 -DHB_FM_STATISTICS_OFF -I$(APP_DIR)\include;$(XD_DIR)\include;$(HBR_DIR)\include
cflags   = -O2 -tW -M -c -DHB_OS_WIN_32_USED -DHB_OS_WIN_32 -DHB_FM_STATISTICS_OFF -I$(APP_DIR)\include;$(XD_DIR)\include;$(HBR_DIR)\include
cvarsmt =

#-- RESOURCE COMPILER ---
rc      = brc32
rcflags = -r -32
rcvars  =

#-- LINKER --------------
link       = ilink32
linkdebug  =
guilflags  = -Gn -Tpe -s
#guilflags  = -Gn -aa -Tpe -s
guilibs    =
#guilibs    = cw32.lib import32.lib
EXTRA_LIBS =

#-- LIBS ----------------

lib        = tlib
libflags   =


