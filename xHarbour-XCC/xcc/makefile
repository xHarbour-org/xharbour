##############################################################
#                                                            #
#             Makefile for Win32 ISO C Compiler              #
#                                                            #
#           Copyright (c) 1999-2005, Pelle Orinius           #
#                                                            #
##############################################################

#MSVCDIR = C:\Program Files\Microsoft Visual Studio\VC98 # Directory for Microsoft Visual C++
# MSVCDIR = C:\Program\MSVC2003                         # Directory for Microsoft Visual C++
# MSVCDIR = C:\Program\Microsoft visual studio 8\VC     # Directory for Microsoft Visual C++

#PSDKDIR = C:\Program Files\Microsoft Platform SDK for Windows XP SP2 # Directory for Microsoft SDK

#PELLESCDIR = C:\Program Files\PellesC                   # Directory for Pelles C executables

INCLUDEDIR = ..\include                                 # Directory for additional include files
CLIBDIR = $(PELLESCDIR)\lib #..\crt\src                                       # Directory for Pelles C CRT.LIB
CINCDIR = crt\include                                   # Directory for Pelles C include files
CWINDIR = ..\include\win                                # Directory for Pelles C Windows include files

##############################################################
# Macros for conditional compilation
##############################################################

# PRERELEASE = -DPRERELEASE
# POWERDEBUG = -DPODEBUG

# PROFILE = 1
# XREF = 1

# No Unicode support (no need).
# POWERUNICODE = 0

# MSDEBUG = -DPODEBUG

!IFDEF POWERDEBUG
!MESSAGE *** Kom ih†g att ta bort POWERDEBUG f”r release version!
!ENDIF

##############################################################
# Output files
##############################################################

# all: mscc.exe

### all: lburg.exe mscc.exe pocc.exe bprint.exe cc.exe
all: lburg.exe mscc.exe pocc.exe xcc.exe xcc.dll bprint.exe cc.exe

##############################################################
# Microsoft C compiler
##############################################################

# Microsoft output directory
MSOBJ = CC\MSOBJ

mslibs = kernel32.lib user32.lib
!IFDEF POWERDEBUG
mslibs = $(mslibs) "$(CLIBDIR)\memdbg.lib"
!ENDIF

mscflags = -Gd -c -W3 -nologo -Gy $(CFLAGS)

!IFDEF MSDEBUG
mscvars  = -DPODEBUG -D_DEBUG $(PRERELEASE) -DWIN32
mscdebug = -Z7
!ELSEIFDEF PRERELEASE
mscvars  = $(PRERELEASE) -DWIN32
mscdebug =
!ELSE
mscvars  = -DNDEBUG $(PRERELEASE) -DWIN32
mscdebug =
!ENDIF

!IFDEF POWERUNICODE
mscvars = $(mscvars) -DUNICODE -D_UNICODE
!ENDIF

!IFDEF MSDEBUG
mslflags = /incremental:no /pdb:none /debug /nologo /stack:0x1000000 "/libpath:$(MSVCDIR)\LIB" $(LFLAGS)
!ELSE
mslflags = /incremental:no /pdb:none /release /nologo /profile /stack:0x1000000 "/libpath:$(MSVCDIR)\LIB" $(LFLAGS)
!ENDIF

mscc = "$(MSVCDIR)\BIN\cl" $(mscflags) $(mscvars) $(mscdebug) "-I$(MSVCDIR)\include" "-I$(INCLUDEDIR)"
msrc = "$(PSDKDIR)\BIN\rc"
msmc = "$(PSDKDIR)\BIN\mc"
msld = "$(MSVCDIR)\BIN\link"

{cc}.c{$(MSOBJ)}.obj:
!IFDEF MSDEBUG
    $(mscc) -Fo$@ $<
!ELSE
    $(mscc) -Fo$@ $<
!ENDIF

{cc}.rc{$(MSOBJ)}.res:
    $(msrc) -fo$@ $<

{BPRINT}.c{BPRINT\OBJ}.obj:
    $(mscc) -Fo$@ $<

{LBURG}.c{LBURG\OBJ}.obj:
    $(mscc) -Fo$@ $<


#
# Compiler (CC).
#
mscc.exe: \
    $(MSOBJ)\main.obj \
# preprocessor
    $(MSOBJ)\cpp.obj \
    $(MSOBJ)\cpplex.obj \
    $(MSOBJ)\eval.obj \
    $(MSOBJ)\hideset.obj \
    $(MSOBJ)\include.obj \
    $(MSOBJ)\macro.obj \
    $(MSOBJ)\nlist.obj \
    $(MSOBJ)\tokens.obj \
# compiler
    $(MSOBJ)\alloc.obj \
    $(MSOBJ)\bind.obj \
    $(MSOBJ)\dag.obj \
    $(MSOBJ)\decl.obj \
    $(MSOBJ)\enode.obj \
    $(MSOBJ)\event.obj \
    $(MSOBJ)\expr.obj \
    $(MSOBJ)\gen.obj \
    $(MSOBJ)\init.obj \
    $(MSOBJ)\input.obj \
    $(MSOBJ)\intrin.obj \
    $(MSOBJ)\lex.obj \
    $(MSOBJ)\list.obj \
    $(MSOBJ)\output.obj \
    $(MSOBJ)\prof.obj \
    $(MSOBJ)\simp.obj \
    $(MSOBJ)\stmt.obj \
    $(MSOBJ)\string.obj \
    $(MSOBJ)\sym.obj \
    $(MSOBJ)\tree.obj \
    $(MSOBJ)\types.obj \
    $(MSOBJ)\utils.obj \
# targets
    $(MSOBJ)\arm.obj \
    $(MSOBJ)\bytecode.obj \
    $(MSOBJ)\null.obj \
    $(MSOBJ)\x86.obj \
# assembler
    $(MSOBJ)\armemit.obj \
    $(MSOBJ)\armparse.obj \
    $(MSOBJ)\armscan.obj \
    $(MSOBJ)\asm.obj \
    $(MSOBJ)\dynarr.obj \
    $(MSOBJ)\evaluate.obj \
    $(MSOBJ)\ieee754.obj \
    $(MSOBJ)\labels.obj \
    $(MSOBJ)\outcoff.obj \
    $(MSOBJ)\outdbg.obj \
    $(MSOBJ)\outnull.obj \
    $(MSOBJ)\peephole.obj \
    $(MSOBJ)\x86emit.obj \
    $(MSOBJ)\x86parse.obj \
    $(MSOBJ)\x86scan.obj \
# resources
    $(MSOBJ)\pocc.res
    $(msld) -out:$@ $(mslflags) $(mslibs) $**


#
# Entry point.
#
$(MSOBJ)\main.obj:      cc\main.c cc\lcc.h
#
# Preprocessor (CPP).
#
$(MSOBJ)\cpp.obj:       cc\cpp.c cc\lcc.h
$(MSOBJ)\cpplex.obj:    cc\cpplex.c cc\lcc.h
$(MSOBJ)\eval.obj:      cc\eval.c cc\lcc.h
$(MSOBJ)\hideset.obj:   cc\hideset.c cc\lcc.h
$(MSOBJ)\include.obj:   cc\include.c cc\lcc.h
$(MSOBJ)\macro.obj:     cc\macro.c cc\lcc.h
$(MSOBJ)\nlist.obj:     cc\nlist.c cc\lcc.h
$(MSOBJ)\tokens.obj:    cc\tokens.c cc\lcc.h
#
# Compiler (RCC).
#
$(MSOBJ)\alloc.obj:     cc\alloc.c cc\lcc.h
$(MSOBJ)\bind.obj:      cc\bind.c cc\lcc.h
$(MSOBJ)\dag.obj:       cc\dag.c cc\lcc.h
$(MSOBJ)\decl.obj:      cc\decl.c cc\lcc.h
$(MSOBJ)\enode.obj:     cc\enode.c cc\lcc.h cc\token.h
$(MSOBJ)\event.obj:     cc\event.c cc\lcc.h
$(MSOBJ)\expr.obj:      cc\expr.c cc\lcc.h cc\token.h
$(MSOBJ)\gen.obj:       cc\gen.c cc\lcc.h
$(MSOBJ)\init.obj:      cc\init.c cc\lcc.h
$(MSOBJ)\input.obj:     cc\input.c cc\lcc.h
$(MSOBJ)\intrin.obj:    cc\intrin.c cc\lcc.h
$(MSOBJ)\lex.obj:       cc\lex.c cc\lcc.h cc\token.h
$(MSOBJ)\list.obj:      cc\list.c cc\lcc.h
$(MSOBJ)\output.obj:    cc\output.c cc\lcc.h cc\token.h
$(MSOBJ)\prof.obj:      cc\prof.c cc\lcc.h
$(MSOBJ)\simp.obj:      cc\simp.c cc\lcc.h
$(MSOBJ)\stmt.obj:      cc\stmt.c cc\lcc.h
$(MSOBJ)\string.obj:    cc\string.c cc\lcc.h
$(MSOBJ)\sym.obj:       cc\sym.c cc\lcc.h
$(MSOBJ)\tree.obj:      cc\tree.c cc\lcc.h
$(MSOBJ)\types.obj:     cc\types.c cc\lcc.h
$(MSOBJ)\utils.obj:     cc\utils.c cc\lcc.h
#
# Targets.
#
$(MSOBJ)\arm.obj:       cc\arm.c cc\lcc.h
$(MSOBJ)\bytecode.obj:  cc\bytecode.c cc\lcc.h
$(MSOBJ)\null.obj:      cc\null.c cc\lcc.h
$(MSOBJ)\x86.obj:       cc\x86.c cc\lcc.h cc\codeview.c
#
# Assembler (generic).
#
$(MSOBJ)\asm.obj:       cc\asm.c cc\lcc.h
$(MSOBJ)\dynarr.obj:    cc\dynarr.c cc\lcc.h
$(MSOBJ)\evaluate.obj:  cc\evaluate.c cc\lcc.h
$(MSOBJ)\ieee754.obj:   cc\ieee754.c cc\lcc.h
$(MSOBJ)\labels.obj:    cc\labels.c cc\lcc.h
$(MSOBJ)\outcoff.obj:   cc\outcoff.c cc\lcc.h "$(INCLUDEDIR)\coff.h"
$(MSOBJ)\outdbg.obj:    cc\outdbg.c cc\lcc.h
$(MSOBJ)\outnull.obj:   cc\outnull.c cc\lcc.h
$(MSOBJ)\peephole.obj:  cc\peephole.c cc\lcc.h
#
# Assembler (X86).
#
$(MSOBJ)\x86emit.obj:   cc\x86emit.c cc\lcc.h cc\x86.h
$(MSOBJ)\x86parse.obj:  cc\x86parse.c cc\lcc.h cc\x86.h
$(MSOBJ)\x86scan.obj:   cc\x86scan.c cc\lcc.h cc\x86.h
#
# Assembler (ARM).
#
$(MSOBJ)\armemit.obj:   cc\armemit.c cc\lcc.h cc\arm.h
$(MSOBJ)\armparse.obj:  cc\armparse.c cc\lcc.h cc\arm.h
$(MSOBJ)\armscan.obj:   cc\armscan.c cc\lcc.h cc\arm.h
#
# Resources.
#
$(MSOBJ)\pocc.res:      cc\pocc.rc cc\msg00001.bin

cc\msg00001.bin \
cc\msg.h:               cc\msg.mc
    $(msmc) -c -A -r cc -h cc -m 256 cc\msg.mc

#
# Machine descriptions.
#
cc\x86.c: lburg.exe lburg\x86.md
    lburg lburg\x86.md >cc\x86.c

cc\arm.c: lburg.exe lburg\arm.md
    lburg lburg\arm.md >cc\arm.c

cc\x86insa.c cc\x86insn.c cc\x86insi.h: cc\x86.dat
    cd cc
    ..\utils\perl x86.pl
    cd ..

cc\arminsa.c cc\arminsn.c cc\arminsi.h: cc\arm.dat
    cd cc
    ..\utils\perl arm.pl
    cd ..

#
# Include file dependencies.
#
cc\lcc.h:   cc\msg.h cc\cpp.h cc\config.h cc\token.h cc\asm.h
cc\x86.h:   cc\x86insi.h cc\x86insn.c cc\x86insa.c
cc\arm.h:   cc\arminsi.h cc\arminsn.c cc\arminsa.c

#
# Profiler listings (BPRINT).
#
bprint.exe: bprint\obj\bprint.obj
    $(msld) -out:$@ $(mslflags) $(mslibs) $**

bprint\obj\bprint.obj:  bprint\bprint.c

#
# LBURG utility.
#
lburg.exe:  lburg\obj\gram.obj \
            lburg\obj\lburg.obj
    $(msld) -out:$@ $(mslflags) $(mslibs) $**

lburg\obj\gram.obj:     lburg\gram.c lburg\lburg.h
lburg\obj\lburg.obj:    lburg\lburg.c lburg\lburg.h

##############################################################
# Pelles C compiler - standard version
##############################################################

# Pelles output directory
POOBJ = CC\OBJ

mylibs = kernel32.lib user32.lib
!IFDEF POWERDEBUG
mylibs = $(mylibs) "$(CLIBDIR)\memdbg.lib"
!ENDIF
!IFDEF PROFILE
mylibs = $(mylibs) ..\profiler\profiler.lib
!ENDIF

mycflags = -Ot -J -W1 -Ze -X "-I$(CINCDIR)" "-I$(CWINDIR)" -v

!IFDEF POWERDEBUG
mycvars  = -DPODEBUG -D_DEBUG -DWIN32 -DPRERELEASE
mycdebug = -Zi
!ELSEIFDEF PROFILE
mycflags = $(mycflags) -Gh
mycvars  = -DNDEBUG -DWIN32
mycdebug = -Zi
!ELSE
mycvars  = -DNDEBUG -DWIN32
mycdebug =
!ENDIF

!IFDEF POWERUNICODE
mycvars = $(mycvars) -DUNICODE -D_UNICODE
!ENDIF

!IFDEF POWERDEBUG
mylflags = /debug /stack:0x1000000 /map
!ELSEIFDEF PROFILE
mylflags = /debug /debugtype:coff /stack:0x1000000 /map
!ELSE
mylflags = /release /stack:0x1000000 /map
!ENDIF

mycc = mscc $(mycflags) $(mycvars) $(mycdebug) "-I$(INCLUDEDIR)"
myrc = "$(PELLESCDIR)\bin\porc"
mymc = "$(PELLESCDIR)\bin\pomc"
myld = "$(PELLESCDIR)\bin\polink"

{cc}.c{$(POOBJ)}.obj:
    $(mycc) -Fo$@ $<


#
# Compiler (CC).
#
pocc.exe: \
    $(POOBJ)\main.obj \
# preprocessor
    $(POOBJ)\cpp.obj \
    $(POOBJ)\cpplex.obj \
    $(POOBJ)\eval.obj \
    $(POOBJ)\hideset.obj \
    $(POOBJ)\include.obj \
    $(POOBJ)\macro.obj \
    $(POOBJ)\nlist.obj \
    $(POOBJ)\tokens.obj \
# compiler
    $(POOBJ)\alloc.obj \
    $(POOBJ)\bind.obj \
    $(POOBJ)\dag.obj \
    $(POOBJ)\decl.obj \
    $(POOBJ)\enode.obj \
    $(POOBJ)\event.obj \
    $(POOBJ)\expr.obj \
    $(POOBJ)\gen.obj \
    $(POOBJ)\init.obj \
    $(POOBJ)\input.obj \
    $(POOBJ)\intrin.obj \
    $(POOBJ)\lex.obj \
    $(POOBJ)\list.obj \
    $(POOBJ)\output.obj \
    $(POOBJ)\prof.obj \
    $(POOBJ)\simp.obj \
    $(POOBJ)\stmt.obj \
    $(POOBJ)\string.obj \
    $(POOBJ)\sym.obj \
    $(POOBJ)\tree.obj \
    $(POOBJ)\types.obj \
    $(POOBJ)\utils.obj \
# targets
    $(POOBJ)\arm.obj \
    $(POOBJ)\bytecode.obj \
    $(POOBJ)\null.obj \
    $(POOBJ)\x86.obj \
# assembler
    $(POOBJ)\armemit.obj \
    $(POOBJ)\armparse.obj \
    $(POOBJ)\armscan.obj \
    $(POOBJ)\asm.obj \
    $(POOBJ)\dynarr.obj \
    $(POOBJ)\evaluate.obj \
    $(POOBJ)\ieee754.obj \
    $(POOBJ)\labels.obj \
    $(POOBJ)\outcoff.obj \
    $(POOBJ)\outdbg.obj \
    $(POOBJ)\outnull.obj \
    $(POOBJ)\peephole.obj \
    $(POOBJ)\x86emit.obj \
    $(POOBJ)\x86parse.obj \
    $(POOBJ)\x86scan.obj \
# resources
    $(MSOBJ)\pocc.res \
    "$(CLIBDIR)\crt.lib"
    $(myld) -out:$@ $(mylflags) $(mylibs) $**


#
# Entry point.
#
$(POOBJ)\main.obj:      cc\main.c cc\lcc.h
#
# Preprocessor (CPP).
#
$(POOBJ)\cpp.obj:       cc\cpp.c cc\lcc.h
$(POOBJ)\cpplex.obj:    cc\cpplex.c cc\lcc.h
$(POOBJ)\eval.obj:      cc\eval.c cc\lcc.h
$(POOBJ)\hideset.obj:   cc\hideset.c cc\lcc.h
$(POOBJ)\include.obj:   cc\include.c cc\lcc.h
$(POOBJ)\macro.obj:     cc\macro.c cc\lcc.h
$(POOBJ)\nlist.obj:     cc\nlist.c cc\lcc.h
$(POOBJ)\tokens.obj:    cc\tokens.c cc\lcc.h
#
# Compiler (RCC).
#
$(POOBJ)\alloc.obj:     cc\alloc.c cc\lcc.h
$(POOBJ)\bind.obj:      cc\bind.c cc\lcc.h
$(POOBJ)\dag.obj:       cc\dag.c cc\lcc.h
$(POOBJ)\decl.obj:      cc\decl.c cc\lcc.h
$(POOBJ)\enode.obj:     cc\enode.c cc\lcc.h cc\token.h
$(POOBJ)\event.obj:     cc\event.c cc\lcc.h
$(POOBJ)\expr.obj:      cc\expr.c cc\lcc.h cc\token.h
$(POOBJ)\gen.obj:       cc\gen.c cc\lcc.h
$(POOBJ)\init.obj:      cc\init.c cc\lcc.h
$(POOBJ)\input.obj:     cc\input.c cc\lcc.h
$(POOBJ)\intrin.obj:    cc\intrin.c cc\lcc.h
$(POOBJ)\lex.obj:       cc\lex.c cc\lcc.h cc\token.h
$(POOBJ)\list.obj:      cc\list.c cc\lcc.h
$(POOBJ)\output.obj:    cc\output.c cc\lcc.h cc\token.h
$(POOBJ)\prof.obj:      cc\prof.c cc\lcc.h
$(POOBJ)\simp.obj:      cc\simp.c cc\lcc.h
$(POOBJ)\stmt.obj:      cc\stmt.c cc\lcc.h
$(POOBJ)\string.obj:    cc\string.c cc\lcc.h
$(POOBJ)\sym.obj:       cc\sym.c cc\lcc.h
$(POOBJ)\tree.obj:      cc\tree.c cc\lcc.h
$(POOBJ)\types.obj:     cc\types.c cc\lcc.h
$(POOBJ)\utils.obj:     cc\utils.c cc\lcc.h
#
# Targets.
#
$(POOBJ)\arm.obj:       cc\arm.c cc\lcc.h
$(POOBJ)\bytecode.obj:  cc\bytecode.c cc\lcc.h
$(POOBJ)\null.obj:      cc\null.c cc\lcc.h
$(POOBJ)\x86.obj:       cc\x86.c cc\lcc.h cc\codeview.c
#
# Assembler (generic).
#
$(POOBJ)\asm.obj:       cc\asm.c cc\lcc.h
$(POOBJ)\dynarr.obj:    cc\dynarr.c cc\lcc.h
$(POOBJ)\evaluate.obj:  cc\evaluate.c cc\lcc.h
$(POOBJ)\ieee754.obj:   cc\ieee754.c cc\lcc.h
$(POOBJ)\labels.obj:    cc\labels.c cc\lcc.h
$(POOBJ)\outcoff.obj:   cc\outcoff.c cc\lcc.h "$(INCLUDEDIR)\coff.h"
$(POOBJ)\outdbg.obj:    cc\outdbg.c cc\lcc.h
$(POOBJ)\outnull.obj:   cc\outnull.c cc\lcc.h
$(POOBJ)\peephole.obj:  cc\peephole.c cc\lcc.h
#
# Assembler (X86).
#
$(POOBJ)\x86emit.obj:   cc\x86emit.c cc\lcc.h cc\x86.h
$(POOBJ)\x86parse.obj:  cc\x86parse.c cc\lcc.h cc\x86.h
$(POOBJ)\x86scan.obj:   cc\x86scan.c cc\lcc.h cc\x86.h
#
# Assembler (ARM).
#
$(POOBJ)\armemit.obj:   cc\armemit.c cc\lcc.h cc\arm.h
$(POOBJ)\armparse.obj:  cc\armparse.c cc\lcc.h cc\arm.h
$(POOBJ)\armscan.obj:   cc\armscan.c cc\lcc.h cc\arm.h

##############################################################
# Pelles C compiler driver (CCL) - standard version
##############################################################

cc.exe: ccl\obj\main.obj ccl\obj\cc.res "$(CLIBDIR)\crt.lib"
    $(myld) -out:$@ $(mylflags) $(mylibs) $** advapi32.lib

ccl\obj\main.obj: ccl\main.c ccl\cc.h ccl\msg.h
    $(mycc) -Foccl\obj\main.obj ccl\main.c

ccl\obj\cc.res: ccl\cc.rc ccl\msg00001.bin
    $(myrc) -foccl\obj\cc.res ccl\cc.rc

ccl\msg00001.bin \
ccl\msg.h:              ccl\msg.mc
    $(msmc) -c -A -r ccl -h ccl -m 256 ccl\msg.mc

##############################################################
# Pelles C compiler - xHarbour version
##############################################################

# xHarbour directories
XOBJ = CC\XOBJ

{cc}.c{$(XOBJ)}.obj:
    $(mycc) -DXHARBOUR -Fo$@ $<


#
# Compiler (CC).
#
xcc.exe: \
    $(XOBJ)\main.obj \
# preprocessor
    $(XOBJ)\cpp.obj \
    $(XOBJ)\cpplex.obj \
    $(XOBJ)\eval.obj \
    $(XOBJ)\hideset.obj \
    $(XOBJ)\include.obj \
    $(XOBJ)\macro.obj \
    $(XOBJ)\nlist.obj \
    $(XOBJ)\tokens.obj \
# compiler
    $(XOBJ)\alloc.obj \
    $(XOBJ)\bind.obj \
    $(XOBJ)\dag.obj \
    $(XOBJ)\decl.obj \
    $(XOBJ)\enode.obj \
    $(XOBJ)\event.obj \
    $(XOBJ)\expr.obj \
    $(XOBJ)\gen.obj \
    $(XOBJ)\init.obj \
    $(XOBJ)\input.obj \
    $(XOBJ)\intrin.obj \
    $(XOBJ)\lex.obj \
    $(XOBJ)\list.obj \
    $(XOBJ)\output.obj \
    $(XOBJ)\prof.obj \
    $(XOBJ)\simp.obj \
    $(XOBJ)\stmt.obj \
    $(XOBJ)\string.obj \
    $(XOBJ)\sym.obj \
    $(XOBJ)\tree.obj \
    $(XOBJ)\types.obj \
    $(XOBJ)\utils.obj \
# targets
    $(XOBJ)\arm.obj \
    $(XOBJ)\bytecode.obj \
    $(XOBJ)\null.obj \
    $(XOBJ)\x86.obj \
# assembler
    $(XOBJ)\armemit.obj \
    $(XOBJ)\armparse.obj \
    $(XOBJ)\armscan.obj \
    $(XOBJ)\asm.obj \
    $(XOBJ)\dynarr.obj \
    $(XOBJ)\evaluate.obj \
    $(XOBJ)\ieee754.obj \
    $(XOBJ)\labels.obj \
    $(XOBJ)\outcoff.obj \
    $(XOBJ)\outdbg.obj \
    $(XOBJ)\outnull.obj \
    $(XOBJ)\peephole.obj \
    $(XOBJ)\x86emit.obj \
    $(XOBJ)\x86parse.obj \
    $(XOBJ)\x86scan.obj \
# resources
    $(MSOBJ)\pocc.res \
    "$(CLIBDIR)\crt.lib"
    $(myld) -out:$@ $(mylflags) $(mylibs) $**


#
# Entry point.
#
$(XOBJ)\main.obj:       cc\main.c cc\lcc.h
#
# Preprocessor (CPP).
#
$(XOBJ)\cpp.obj:        cc\cpp.c cc\lcc.h
$(XOBJ)\cpplex.obj:     cc\cpplex.c cc\lcc.h
$(XOBJ)\eval.obj:       cc\eval.c cc\lcc.h
$(XOBJ)\hideset.obj:    cc\hideset.c cc\lcc.h
$(XOBJ)\include.obj:    cc\include.c cc\lcc.h
$(XOBJ)\macro.obj:      cc\macro.c cc\lcc.h
$(XOBJ)\nlist.obj:      cc\nlist.c cc\lcc.h
$(XOBJ)\tokens.obj:     cc\tokens.c cc\lcc.h
#
# Compiler (RCC).
#
$(XOBJ)\alloc.obj:      cc\alloc.c cc\lcc.h
$(XOBJ)\bind.obj:       cc\bind.c cc\lcc.h
$(XOBJ)\dag.obj:        cc\dag.c cc\lcc.h
$(XOBJ)\decl.obj:       cc\decl.c cc\lcc.h
$(XOBJ)\enode.obj:      cc\enode.c cc\lcc.h cc\token.h
$(XOBJ)\event.obj:      cc\event.c cc\lcc.h
$(XOBJ)\expr.obj:       cc\expr.c cc\lcc.h cc\token.h
$(XOBJ)\gen.obj:        cc\gen.c cc\lcc.h
$(XOBJ)\init.obj:       cc\init.c cc\lcc.h
$(XOBJ)\input.obj:      cc\input.c cc\lcc.h
$(XOBJ)\intrin.obj:     cc\intrin.c cc\lcc.h
$(XOBJ)\lex.obj:        cc\lex.c cc\lcc.h cc\token.h
$(XOBJ)\list.obj:       cc\list.c cc\lcc.h
$(XOBJ)\output.obj:     cc\output.c cc\lcc.h cc\token.h
$(XOBJ)\prof.obj:       cc\prof.c cc\lcc.h
$(XOBJ)\simp.obj:       cc\simp.c cc\lcc.h
$(XOBJ)\stmt.obj:       cc\stmt.c cc\lcc.h
$(XOBJ)\string.obj:     cc\string.c cc\lcc.h
$(XOBJ)\sym.obj:        cc\sym.c cc\lcc.h
$(XOBJ)\tree.obj:       cc\tree.c cc\lcc.h
$(XOBJ)\types.obj:      cc\types.c cc\lcc.h
$(XOBJ)\utils.obj:      cc\utils.c cc\lcc.h
#
# Targets.
#
$(XOBJ)\arm.obj:        cc\arm.c cc\lcc.h
$(XOBJ)\bytecode.obj:   cc\bytecode.c cc\lcc.h
$(XOBJ)\null.obj:       cc\null.c cc\lcc.h
$(XOBJ)\x86.obj:        cc\x86.c cc\lcc.h cc\codeview.c
#
# Assembler (generic).
#
$(XOBJ)\asm.obj:        cc\asm.c cc\lcc.h
$(XOBJ)\dynarr.obj:     cc\dynarr.c cc\lcc.h
$(XOBJ)\evaluate.obj:   cc\evaluate.c cc\lcc.h
$(XOBJ)\ieee754.obj:    cc\ieee754.c cc\lcc.h
$(XOBJ)\labels.obj:     cc\labels.c cc\lcc.h
$(XOBJ)\outcoff.obj:    cc\outcoff.c cc\lcc.h "$(INCLUDEDIR)\coff.h"
$(XOBJ)\outdbg.obj:     cc\outdbg.c cc\lcc.h
$(XOBJ)\outnull.obj:    cc\outnull.c cc\lcc.h
$(XOBJ)\peephole.obj:   cc\peephole.c cc\lcc.h
#
# Assembler (X86).
#
$(XOBJ)\x86emit.obj:    cc\x86emit.c cc\lcc.h cc\x86.h
$(XOBJ)\x86parse.obj:   cc\x86parse.c cc\lcc.h cc\x86.h
$(XOBJ)\x86scan.obj:    cc\x86scan.c cc\lcc.h cc\x86.h
#
# Assembler (ARM).
#
$(XOBJ)\armemit.obj:    cc\armemit.c cc\lcc.h cc\arm.h
$(XOBJ)\armparse.obj:   cc\armparse.c cc\lcc.h cc\arm.h
$(XOBJ)\armscan.obj:    cc\armscan.c cc\lcc.h cc\arm.h

##############################################################
# Pelles C compiler - xHarbour version (DLL)
##############################################################

# xHarbour directories
XDLLOBJ = CC\XDLLOBJ

{cc}.c{$(XDLLOBJ)}.obj:
    $(mycc) -DXHARBOUR -DLIBMAIN -Fo$@ $<


#
# Compiler (CC).
#
xcc.dll: \
    $(XDLLOBJ)\main.obj \
# preprocessor
    $(XDLLOBJ)\cpp.obj \
    $(XDLLOBJ)\cpplex.obj \
    $(XDLLOBJ)\eval.obj \
    $(XDLLOBJ)\hideset.obj \
    $(XDLLOBJ)\include.obj \
    $(XDLLOBJ)\macro.obj \
    $(XDLLOBJ)\nlist.obj \
    $(XDLLOBJ)\tokens.obj \
# compiler
    $(XDLLOBJ)\alloc.obj \
    $(XDLLOBJ)\bind.obj \
    $(XDLLOBJ)\dag.obj \
    $(XDLLOBJ)\decl.obj \
    $(XDLLOBJ)\enode.obj \
    $(XDLLOBJ)\event.obj \
    $(XDLLOBJ)\expr.obj \
    $(XDLLOBJ)\gen.obj \
    $(XDLLOBJ)\init.obj \
    $(XDLLOBJ)\input.obj \
    $(XDLLOBJ)\intrin.obj \
    $(XDLLOBJ)\lex.obj \
    $(XDLLOBJ)\list.obj \
    $(XDLLOBJ)\output.obj \
    $(XDLLOBJ)\prof.obj \
    $(XDLLOBJ)\simp.obj \
    $(XDLLOBJ)\stmt.obj \
    $(XDLLOBJ)\string.obj \
    $(XDLLOBJ)\sym.obj \
    $(XDLLOBJ)\tree.obj \
    $(XDLLOBJ)\types.obj \
    $(XDLLOBJ)\utils.obj \
# targets
    $(XDLLOBJ)\arm.obj \
    $(XDLLOBJ)\bytecode.obj \
    $(XDLLOBJ)\null.obj \
    $(XDLLOBJ)\x86.obj \
# assembler
    $(XDLLOBJ)\armemit.obj \
    $(XDLLOBJ)\armparse.obj \
    $(XDLLOBJ)\armscan.obj \
    $(XDLLOBJ)\asm.obj \
    $(XDLLOBJ)\dynarr.obj \
    $(XDLLOBJ)\evaluate.obj \
    $(XDLLOBJ)\ieee754.obj \
    $(XDLLOBJ)\labels.obj \
    $(XDLLOBJ)\outcoff.obj \
    $(XDLLOBJ)\outdbg.obj \
    $(XDLLOBJ)\outnull.obj \
    $(XDLLOBJ)\peephole.obj \
    $(XDLLOBJ)\x86emit.obj \
    $(XDLLOBJ)\x86parse.obj \
    $(XDLLOBJ)\x86scan.obj \
# resources
    $(MSOBJ)\pocc.res \
    "$(CLIBDIR)\crt.lib"
    $(myld) -dll -out:$@ $(mylflags) $(mylibs) $**


#
# Entry point.
#
$(XDLLOBJ)\main.obj:       cc\main.c cc\lcc.h
#
# Preprocessor (CPP).
#
$(XDLLOBJ)\cpp.obj:        cc\cpp.c cc\lcc.h
$(XDLLOBJ)\cpplex.obj:     cc\cpplex.c cc\lcc.h
$(XDLLOBJ)\eval.obj:       cc\eval.c cc\lcc.h
$(XDLLOBJ)\hideset.obj:    cc\hideset.c cc\lcc.h
$(XDLLOBJ)\include.obj:    cc\include.c cc\lcc.h
$(XDLLOBJ)\macro.obj:      cc\macro.c cc\lcc.h
$(XDLLOBJ)\nlist.obj:      cc\nlist.c cc\lcc.h
$(XDLLOBJ)\tokens.obj:     cc\tokens.c cc\lcc.h
#
# Compiler (RCC).
#
$(XDLLOBJ)\alloc.obj:      cc\alloc.c cc\lcc.h
$(XDLLOBJ)\bind.obj:       cc\bind.c cc\lcc.h
$(XDLLOBJ)\dag.obj:        cc\dag.c cc\lcc.h
$(XDLLOBJ)\decl.obj:       cc\decl.c cc\lcc.h
$(XDLLOBJ)\enode.obj:      cc\enode.c cc\lcc.h cc\token.h
$(XDLLOBJ)\event.obj:      cc\event.c cc\lcc.h
$(XDLLOBJ)\expr.obj:       cc\expr.c cc\lcc.h cc\token.h
$(XDLLOBJ)\gen.obj:        cc\gen.c cc\lcc.h
$(XDLLOBJ)\init.obj:       cc\init.c cc\lcc.h
$(XDLLOBJ)\input.obj:      cc\input.c cc\lcc.h
$(XDLLOBJ)\intrin.obj:     cc\intrin.c cc\lcc.h
$(XDLLOBJ)\lex.obj:        cc\lex.c cc\lcc.h cc\token.h
$(XDLLOBJ)\list.obj:       cc\list.c cc\lcc.h
$(XDLLOBJ)\output.obj:     cc\output.c cc\lcc.h cc\token.h
$(XDLLOBJ)\prof.obj:       cc\prof.c cc\lcc.h
$(XDLLOBJ)\simp.obj:       cc\simp.c cc\lcc.h
$(XDLLOBJ)\stmt.obj:       cc\stmt.c cc\lcc.h
$(XDLLOBJ)\string.obj:     cc\string.c cc\lcc.h
$(XDLLOBJ)\sym.obj:        cc\sym.c cc\lcc.h
$(XDLLOBJ)\tree.obj:       cc\tree.c cc\lcc.h
$(XDLLOBJ)\types.obj:      cc\types.c cc\lcc.h
$(XDLLOBJ)\utils.obj:      cc\utils.c cc\lcc.h
#
# Targets.
#
$(XDLLOBJ)\arm.obj:        cc\arm.c cc\lcc.h
$(XDLLOBJ)\bytecode.obj:   cc\bytecode.c cc\lcc.h
$(XDLLOBJ)\null.obj:       cc\null.c cc\lcc.h
$(XDLLOBJ)\x86.obj:        cc\x86.c cc\lcc.h cc\codeview.c
#
# Assembler (generic).
#
$(XDLLOBJ)\asm.obj:        cc\asm.c cc\lcc.h
$(XDLLOBJ)\dynarr.obj:     cc\dynarr.c cc\lcc.h
$(XDLLOBJ)\evaluate.obj:   cc\evaluate.c cc\lcc.h
$(XDLLOBJ)\ieee754.obj:    cc\ieee754.c cc\lcc.h
$(XDLLOBJ)\labels.obj:     cc\labels.c cc\lcc.h
$(XDLLOBJ)\outcoff.obj:    cc\outcoff.c cc\lcc.h "$(INCLUDEDIR)\coff.h"
$(XDLLOBJ)\outdbg.obj:     cc\outdbg.c cc\lcc.h
$(XDLLOBJ)\outnull.obj:    cc\outnull.c cc\lcc.h
$(XDLLOBJ)\peephole.obj:   cc\peephole.c cc\lcc.h
#
# Assembler (X86).
#
$(XDLLOBJ)\x86emit.obj:    cc\x86emit.c cc\lcc.h cc\x86.h
$(XDLLOBJ)\x86parse.obj:   cc\x86parse.c cc\lcc.h cc\x86.h
$(XDLLOBJ)\x86scan.obj:    cc\x86scan.c cc\lcc.h cc\x86.h
#
# Assembler (ARM).
#
$(XDLLOBJ)\armemit.obj:    cc\armemit.c cc\lcc.h cc\arm.h
$(XDLLOBJ)\armparse.obj:   cc\armparse.c cc\lcc.h cc\arm.h
$(XDLLOBJ)\armscan.obj:    cc\armscan.c cc\lcc.h cc\arm.h

