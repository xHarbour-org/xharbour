##############################################################
#                                                            #
#             Makefile for Win32 Linkage Editor              #
#                                                            #
#           Copyright (c) 1998-2003, Pelle Orinius           #
#                                                            #
##############################################################

#PELLESCDIR = c:\program Files\PellesC

# PRERELEASE = -DPRERELEASE
# POWERDEBUG = -DPODEBUG

# No Unicode support (no need).
# POWERUNICODE = 0

# MSVCRT = 1

!IFDEF POWERDEBUG
!MESSAGE *** Remember to remove POWERDEBUG for release version!
!ENDIF

all: msxLink.exe xLink.exe

##############################################################
# Microsoft C compiler
##############################################################

!IFDEF POWERDEBUG
!IFDEF MSVCRT
mslibs = kernel32.lib user32.lib msvcrtd.lib
!ELSE
mslibs = kernel32.lib user32.lib
!ENDIF
!ELSE
!IFDEF MSVCRT
mslibs = kernel32.lib user32.lib msvcrt.lib
!ELSE
mslibs = kernel32.lib user32.lib
!ENDIF
!ENDIF

!IFDEF MSVCRT
mscflags = -Gz -c -W4 -nologo -Gy $(CFLAGS)
!ELSE
mscflags = -Gz -c -W4 -nologo -Gy $(CFLAGS)
!ENDIF

!IFDEF POWERDEBUG
mscdebug = -Z7
!ENDIF

mscvars = $(POWERDEBUG) $(PRERELEASE) -I..\include

!IFDEF POWERUNICODE
mscvars = $(mscvars) -DUNICODE -D_UNICODE
!ENDIF

!IFDEF POWERDEBUG
mslflags = /nodefaultlib /incremental:no /pdb:none /debug /nologo
!ELSE
mslflags = /incremental:no /pdb:none /release /nologo /profile $(LFLAGS)
!ENDIF

mscc = cl $(mscflags) $(mscvars) $(mscdebug)
msrc = rc
msmc = mc

{}.c{MSOBJ}.obj:
    $(mscc) -Fo$*.obj %|fF.c

{}.rc{MSOBJ}.res:
    $(msrc) -fo$@ $<


msxLink.exe:    MSOBJ\archive.obj MSOBJ\debug.obj MSOBJ\delayimp.obj \
                MSOBJ\dictary.obj MSOBJ\execfile.obj MSOBJ\export.obj \
                MSOBJ\fixup.obj MSOBJ\main.obj MSOBJ\map.obj MSOBJ\object.obj \
                MSOBJ\order.obj MSOBJ\resource.obj MSOBJ\utils.obj \
                MSOBJ\xLink.res
    link -out:$@ $(mslflags) $(mslibs) $**

MSOBJ\archive.obj:  archive.c link.h
MSOBJ\debug.obj:    debug.c link.h
MSOBJ\delayimp.obj: delayimp.c link.h
MSOBJ\dictary.obj:  dictary.c link.h
MSOBJ\execfile.obj: execfile.c link.h
MSOBJ\export.obj:   export.c link.h
MSOBJ\fixup.obj:    fixup.c link.h
MSOBJ\main.obj:     main.c link.h
MSOBJ\map.obj:      map.c link.h
MSOBJ\object.obj:   object.c link.h
MSOBJ\order.obj:    order.c link.h
MSOBJ\resource.obj: resource.c link.h
MSOBJ\utils.obj:    utils.c link.h
MSOBJ\xLink.res:    xLink.rc MSOBJ\msg00001.bin

MSOBJ\msg00001.bin \
msg.h:              msg.mc
    $(msmc) -c -A -r MSOBJ msg.mc

link.h:             msg.h defs.h


##############################################################
# Pelles C compiler
##############################################################

polibs = kernel32.lib user32.lib crt.lib

pocflags = -c -Ot -Ze -Gz -W1 -Tx86-coff -X -I"$(PELLESCDIR)\INCLUDE" -I"$(PELLESCDIR)\INCLUDE\WIN"
pocvars  = $(POWERDEBUG) $(PRERELEASE) -I..\include

!IFDEF POWERDEBUG
pocdebug = -Zi
!ENDIF

!IFDEF POWERUNICODE
pocvars = $(pocvars) -DUNICODE -D_UNICODE
!ENDIF

!IFDEF POWERDEBUG
polflags = /debug /map /libpath:"$(PELLESCDIR)\LIB" /libpath:"$(PELLESCDIR)\LIB\WIN"
!ELSE
polflags = /release /map /libpath:"$(PELLESCDIR)\LIB" /libpath:"$(PELLESCDIR)\LIB\WIN"
!ENDIF

cc = pocc.exe $(pocflags) $(pocvars) $(pocdebug)

{}.c{OBJ}.obj:
    $(cc) -Fo$*.obj %|fF.c


xLink.exe:      OBJ\archive.obj OBJ\debug.obj OBJ\delayimp.obj \
                OBJ\dictary.obj OBJ\execfile.obj OBJ\export.obj \
                OBJ\fixup.obj OBJ\main.obj OBJ\map.obj OBJ\object.obj \
                OBJ\order.obj OBJ\resource.obj OBJ\utils.obj \
                MSOBJ\xLink.res
    polink.exe -out:$@ $(polflags) $(polibs) $**

OBJ\archive.obj:    archive.c link.h
OBJ\debug.obj:      debug.c link.h
OBJ\delayimp.obj:   delayimp.c link.h
OBJ\dictary.obj:    dictary.c link.h
OBJ\execfile.obj:   execfile.c link.h
OBJ\export.obj:     export.c link.h
OBJ\fixup.obj:      fixup.c link.h
OBJ\main.obj:       main.c link.h
OBJ\map.obj:        map.c link.h
OBJ\object.obj:     object.c link.h
OBJ\order.obj:      order.c link.h
OBJ\resource.obj:   resource.c link.h
OBJ\utils.obj:      utils.c link.h
OBJ\xLink.res:      xLink.rc

link.h:             defs.h

