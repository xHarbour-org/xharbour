##############################################################
#                                                            #
#             Makefile for Win32 Library Manager             #
#                                                            #
#           Copyright (c) 1997-2003, Pelle Orinius           #
#                                                            #
##############################################################

#PELLESCDIR = c:\progra~1\pellesc

# PRERELEASE = -DPRERELEASE
# POWERDEBUG = -DPODEBUG

# No Unicode support (no need).
# POWERUNICODE = 0

!IFDEF POWERDEBUG
!MESSAGE *** Remember to remove POWERDEBUG from release version!
!ENDIF

all: mslib.exe xlib.exe

##############################################################
# Microsoft C compiler
##############################################################

!IFDEF POWERDEBUG
#mslibs = kernel32.lib user32.lib libcd.lib
!ELSE
#mslibs = kernel32.lib user32.lib libc.lib
!ENDIF

mscflags = -Gz -c -W4 -nologo -Gy $(CFLAGS)
mscvars  = $(POWERDEBUG) $(PRERELEASE) -I..\include

!IFDEF POWERDEBUG
mscdebug = -Z7
!ENDIF

!IFDEF POWERUNICODE
mscvars = $(mscvars) -DUNICODE -D_UNICODE
!ENDIF

!IFDEF POWERDEBUG
mslflags = /incremental:no /pdb:none /debug /nologo
!ELSE
mslflags = /incremental:no /pdb:none /release /nologo
!ENDIF

mscc = cl $(mscflags) $(mscvars) $(mscdebug) -DXHARBOUR
msrc = rc -dXHARBOUR
msmc = mc

{}.c{MSOBJ}.obj:
    $(mscc) -Fo$*.obj %|fF.c

{}.rc{MSOBJ}.res:
    $(msrc) -fo$@ $<

mslib.exe:      MSOBJ\archive.obj MSOBJ\dictary.obj MSOBJ\export.obj \
                MSOBJ\main.obj MSOBJ\object.obj MSOBJ\utils.obj \
                MSOBJ\polib.res
    link -out:$@ $(mslflags) $(mslibs) User32.lib $**

MSOBJ\archive.obj:  archive.c lib.h msg.h
MSOBJ\dictary.obj:  dictary.c lib.h msg.h
MSOBJ\export.obj :  export.c  lib.h msg.h
MSOBJ\main.obj:     main.c lib.h msg.h
MSOBJ\object.obj:   object.c lib.h msg.h
MSOBJ\utils.obj:    utils.c lib.h msg.h
MSOBJ\polib.res:    polib.rc MSOBJ\msg00001.bin

MSOBJ\msg00001.bin \
msg.h:              msg.mc
    $(msmc) -c -r MSOBJ msg.mc

##############################################################
# Pelles C compiler
##############################################################

polibs = kernel32.lib user32.lib crt.lib

pocflags = -Ot -Ze -Gz -W1 -X -I"$(PELLESCDIR)\INCLUDE" -I"$(PELLESCDIR)\INCLUDE\WIN"
pocvars  = $(POWERDEBUG) $(PRERELEASE) -I..\include

!IFDEF POWERDEBUG
pocdebug = -Zi
!ENDIF

!IFDEF POWERUNICODE
pocvars = $(pocvars) -DUNICODE -D_UNICODE
!ENDIF

!IFDEF POWERDEBUG
polflags = /debug /libpath:"$(PELLESCDIR)\LIB" /libpath:"$(PELLESCDIR)\LIB\WIN"
!ELSE
polflags = /release /libpath:"$(PELLESCDIR)\LIB" /libpath:"$(PELLESCDIR)\LIB\WIN"
!ENDIF

cc = "$(PELLESCDIR)\bin\pocc" $(pocflags) $(pocvars) $(pocdebug) -DXHARBOUR

{}.c{OBJ}.obj:
    $(cc) -Fo$*.obj %|fF.c

xlib.exe:       OBJ\archive.obj OBJ\dictary.obj OBJ\export.obj \
                OBJ\main.obj OBJ\object.obj OBJ\utils.obj \
                MSOBJ\polib.res
    "$(PELLESCDIR)\bin\polink" -out:$@ $(polflags) $(polibs) $**

OBJ\archive.obj:    archive.c  lib.h msg.h
OBJ\dictary.obj:    dictary.c  lib.h msg.h
OBJ\export.obj :    export.c lib.h msg.h
OBJ\main.obj:       main.c lib.h msg.h
OBJ\object.obj:     object.c lib.h msg.h
OBJ\utils.obj:      utils.c lib.h msg.h

