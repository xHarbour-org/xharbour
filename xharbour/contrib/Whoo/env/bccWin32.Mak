#-- path vari -----------
OUTDIR=build
OBJDIR=obj\b32
SRCDIR=source
LIBDIR=..\..\lib\b32


#-- HARBOUR COMPILER ----
hbc     = $(HBR_DIR)\BIN\harbour
hbcdebug =
hbcflags  = -n -w -es2 -i$(WH_DIR)\include;$(HBR_DIR)\include;

#-- C COMPILER ----------
cc      = bcc32
cdebug  =
cflags  = -tWM -DHB_API_MACROS -DHB_STACK_MACROS -DHB_OS_WIN_32 -DHB_FM_STATISTICS_OFF -d -a8 -OS -O2 -5 -c -w-8075 -I$(WH_DIR)\include;$(HBR_DIR)\include
#cflags  = -tWM -DHB_API_MACROS -DHB_STACK_MACROS -DHB_OS_WIN_32 -d -a8 -OS -O2 -5 -c -w-8075 -I$(WH_DIR)\include;$(HBR_DIR)\include
#cflags  = -tWM -DHB_API_MACROS -M -O2 -c -w-8075 -I$(WH_DIR)\include;$(HBR_DIR)\include
#cflags  = -tWM -5 -c -I$(WH_DIR)\include
cvarsmt =

#-- RESOURCE COMPILER ---
rc      = brc32
rcflags = -r
rcvars  =

#-- LINKER --------------
link       = ilink32
linkdebug  =
guilflags  = -Gn -aa -Tpe -L$(HBR_DIR)\lib\ -s
guilibs    = cw32.lib import32.lib
EXTRA_LIBS =

#-- LIBS ----------------

lib        = tlib
libflags   =


