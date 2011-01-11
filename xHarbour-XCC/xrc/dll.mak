##############################################################
#                                                            #
#          Makefile for Win32 Resource Compiler DLL          #
#                                                            #
#           Copyright (c) 2001-2003, Pelle Orinius           #
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

all: msrc.dll xrc.dll

##############################################################
# Microsofts C-kompilator
##############################################################

!IFDEF POWERDEBUG
mslibs = kernel32.lib user32.lib libcdmt.lib
!ELSE
mslibs = kernel32.lib user32.lib libcmt.lib
!ENDIF

mscflags = -Gs -Gz -c -W4 -nologo -Gy -J $(CFLAGS)
mscvars = $(POWERDEBUG) $(PRERELEASE) -D_RESDLL_

!IFDEF POWERDEBUG
mscdebug = -Z7
!ENDIF

!IFDEF POWERUNICODE
mscvars = $(mscvars) -DUNICODE -D_UNICODE
!ENDIF

!IFDEF POWERDEBUG
mslflags = /nodefaultlib /incremental:no /pdb:none /debug /nologo
!ELSE
mslflags = /nodefaultlib /incremental:no /pdb:none /release /nologo
!ENDIF

mscc = cl $(mscflags) $(mscvars) $(mscdebug)
msrc = rc
msmc = mc

{}.c{MSOBJ}.obj:
    $(mscc) -Fo$*.obj %|fF.c

{}.rc{MSOBJ}.res:
    $(msrc) -fo$@ $<


msrc.dll:       MSOBJ\dllmain.obj MSOBJ\rcmain.obj MSOBJ\rcompile.obj MSOBJ\utils.obj \
                MSOBJ\cpp.obj MSOBJ\eval.obj MSOBJ\hideset.obj MSOBJ\include.obj \
                MSOBJ\lex.obj MSOBJ\macro.obj MSOBJ\nlist.obj MSOBJ\scanner.obj \
                MSOBJ\tokens.obj MSOBJ\resdll.res
    link -dll -out:$@ $(mslflags) $(mslibs) $**

MSOBJ\dllmain.obj:  dllmain.c rc.h
MSOBJ\rcmain.obj:   rcmain.c rc.h
MSOBJ\rcompile.obj: rcompile.c rc.h
MSOBJ\utils.obj:    utils.c rc.h
MSOBJ\cpp.obj:      cpp.c rc.h
MSOBJ\eval.obj:     eval.c rc.h
MSOBJ\hideset.obj:  hideset.c rc.h
MSOBJ\include.obj:  include.c rc.h
MSOBJ\lex.obj:      lex.c rc.h
MSOBJ\macro.obj:    macro.c rc.h
MSOBJ\nlist.obj:    nlist.c rc.h
MSOBJ\scanner.obj:  scanner.c rc.h
MSOBJ\tokens.obj:   tokens.c rc.h
MSOBJ\resdll.res:   resdll.rc MSOBJ\msg00001.bin

MSOBJ\msg00001.bin \
msg.h:              msg.mc
    $(msmc) -c -A -r MSOBJ msg.mc

rc.h:               cpp.h msg.h

##############################################################
# Pelles C-kompilator
##############################################################

polibs = kernel32.lib user32.lib $(xccroot)\$(libfolder)\crtmt.lib

pocflags = -c -Ot -Ze -Gz -MT -W1 -X -I$(xccroot)\$(incfolder) -I$(xccroot)\$(incfolder)\win
pocvars = $(POWERDEBUG) $(PRERELEASE) -I$(xccroot)\$(incfolder) -D_RESDLL_

!IFDEF POWERDEBUG
pocdebug = -Zi
!ENDIF

!IFDEF POWERUNICODE
pocvars = $(pocvars) -DUNICODE -D_UNICODE
!ENDIF

!IFDEF POWERDEBUG
polflags = /debug /map
!ELSE
polflags = /release
!ENDIF

xcc = $(xccroot)\bin\xcc $(pocflags) $(pocvars) $(pocdebug)
xlink = $(xccroot)\bin\xlink

{}.c{OBJ}.obj:
    $(xcc) -I$(xccroot)\$(incfolder) -I$(xccroot)\$(incfolder)\win -Fo$*.obj %|fF.c


xrc.dll:        OBJ\dllmain.obj OBJ\rcmain.obj OBJ\rcompile.obj OBJ\utils.obj \
                OBJ\cpp.obj OBJ\eval.obj OBJ\hideset.obj OBJ\include.obj \
                OBJ\lex.obj OBJ\macro.obj OBJ\nlist.obj OBJ\scanner.obj \
                OBJ\tokens.obj MSOBJ\resdll.res
    $(xlink) -dll -out:$@ $(polflags) $(polibs) $**

OBJ\dllmain.obj:    dllmain.c rc.h
OBJ\rcmain.obj:     rcmain.c rc.h
OBJ\rcompile.obj:   rcompile.c rc.h
OBJ\utils.obj:      utils.c rc.h
OBJ\cpp.obj:        cpp.c rc.h
OBJ\eval.obj:       eval.c rc.h
OBJ\hideset.obj:    hideset.c rc.h
OBJ\include.obj:    include.c rc.h
OBJ\lex.obj:        lex.c rc.h
OBJ\macro.obj:      macro.c rc.h
OBJ\nlist.obj:      nlist.c rc.h
OBJ\scanner.obj:    scanner.c rc.h
OBJ\tokens.obj:     tokens.c rc.h

