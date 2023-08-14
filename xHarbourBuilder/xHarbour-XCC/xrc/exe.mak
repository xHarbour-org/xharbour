##############################################################
#                                                            #
#            Makefile for Win32 Resource Compiler            #
#                                                            #
#           Copyright (c) 1997-2001, Pelle Orinius           #
#                                                            #
##############################################################

#xccroot = \pc32
#incfolder = include
#libfolder = lib

xccroot = \xhb
incfolder = c_include
libfolder = c_lib

# PRERELEASE = -DPRERELEASE
# POWERDEBUG = -DPODEBUG

# No Unicode support (input).
# POWERUNICODE = 0

!IFDEF POWERDEBUG
!MESSAGE *** Kom ih†g att ta bort POWERDEBUG f”r release version!
!ENDIF

all: msrc.exe xrc.exe

##############################################################
# Microsofts C-kompilator
##############################################################

!IFDEF POWERDEBUG
#mslibs = kernel32.lib user32.lib libcd.lib msrc.lib
!ELSE
#mslibs = kernel32.lib user32.lib libc.lib msrc.lib
!ENDIF

mscflags = -Gz -c -W4 -nologo -Gy -J $(CFLAGS)
mscvars = $(POWERDEBUG) $(PRERELEASE) 

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

mscc = cl $(mscflags) $(mscvars) $(mscdebug)
msrc = rc

{}.c{MSOBJ}.obj:
    $(mscc) -Fo$*.obj %|fF.c

{}.rc{MSOBJ}.res:
    $(msrc) -fo$*.res $**


msrc.exe:       MSOBJ\rc.obj MSOBJ\rc.res
    link -out:$@ $(mslflags) $(mslibs) User32.lib Kernel32.lib msrc.lib $**

MSOBJ\rc.obj:   rc.c resdll.h msg.h
MSOBJ\rc.res:   rc.rc


##############################################################
# Pelles C-kompilator
##############################################################

polibs = kernel32.lib user32.lib $(xccroot)\$(libfolder)\crt.lib xrc.lib

pocflags = -c -Ot -Ze -Gz -W1 -X -I$(xccroot)\$(incfolder) -I$(xccroot)\$(incfolder)\win
pocvars  = $(POWERDEBUG) $(PRERELEASE) -I$(xccroot)\$(incfolder)

!IFDEF POWERDEBUG
pocdebug = -Zi
!ENDIF

!IFDEF POWERUNICODE
pocvars  = $(pocvars) -DUNICODE -D_UNICODE
!ENDIF

!IFDEF POWERDEBUG
polflags = /debug /map
!ELSE
polflags = /release
!ENDIF

xcc = $(xccroot)\bin\xcc $(pocflags) $(pocvars) $(pocdebug)
xlink = $(xccroot)\bin\xlink

{}.c{OBJ}.obj:
    $(xcc) -Fo$*.obj %|fF.c


xrc.exe:        OBJ\rc.obj MSOBJ\rc.res
    $(xlink) -out:$@ $(polflags) $(polibs) $**

OBJ\rc.obj:     rc.c resdll.h msg.h

