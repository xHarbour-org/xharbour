# Makefile for Pelles C. 

# Will build a Win32 static library named libpq.lib

# This make file was tested under postgresql-8.3.1 release.

# Pelles C base install directory goes here
CC_DIR=C:\PellesC

# For Pelles 5.xx uncomment the next line.
#.PHONY ALL:

.SILENT

!IF "$(CC_DIR)" == ""
!MESSAGE You must edit pocc.mak and define CC_DIR at the top
!ERROR misssing CC_DIR
!ENDIF

!MESSAGE Building the Win32 DLL and Static Library...
!MESSAGE
!IF "$(CFG)" == ""
CFG=Release
!MESSAGE No configuration specified. Defaulting to Release.
!MESSAGE
!ELSE
!MESSAGE Configuration "$(CFG)"
!MESSAGE
!ENDIF

!IF "$(CFG)" != "Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!ENDIF

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

OUT_DIR=.\Release
OBJ_DIR= $(OUT_DIR)\obj

OUTFILENAME=libpq

USERDEFINES = /D"FRONTEND" /D"NDEBUG" /D"WIN32" /D"_WINDOWS" /D"WIN32_ONLY_COMPILER" /D"HAVE_STRUCT_SOCKADDR_STORAGE" /D"HAVE_STRUCT_SOCKADDR_STORAGE_SS_FAMILY" /D"HAVE_STRUCT_ADDRINFO"

CC_EXE=$(CC_DIR)\bin\pocc.exe
CC_FLAGS = /Ze /Ox /Go /Tx86-coff /I"$(CC_DIR)\include" /I"$(CC_DIR)\include\win" /I"..\..\include" /I"..\..\include\port\win32" /I"..\..\include\port\win32_msvc" /I"..\..\port" $(USERDEFINES) 

RES_EXE=$(CC_DIR)\bin\porc.exe
RES_FLAGS= /I"$(CC_DIR)\include" /I"$(CC_DIR)\include\win" /Fo"$(OUT_DIR)\libpq.res"

LINK_EXE=$(CC_DIR)\bin\polink.exe
LINK_FLAGS = /MACHINE:X86 /FORCE:MULTIPLE /SUBSYSTEM:CONSOLE /LIBPATH:"$(CC_DIR)"\lib /LIBPATH:"$(CC_DIR)"\lib\win /LIBPATH:"$(OUT_DIR)"

LIB_EXE=$(CC_DIR)\bin\polib.exe
LIB_FLAGS= 

ALL : config \
      $(OUT_DIR)\
      $(OBJ_DIR)\
      $(OUT_DIR)\libpq.lib

CLEAN :
        -@erase $(OUT_DIR)\$(OUTFILENAME).lib
        -@erase $(OBJ_DIR)\*.obj


LIB_OBJS= \
        $(OBJ_DIR)\win32.obj \
        $(OBJ_DIR)\getaddrinfo.obj \
        $(OBJ_DIR)\pgstrcasecmp.obj \
        $(OBJ_DIR)\thread.obj \
        $(OBJ_DIR)\inet_aton.obj \
        $(OBJ_DIR)\crypt.obj \
        $(OBJ_DIR)\noblock.obj \
        $(OBJ_DIR)\md5.obj \
        $(OBJ_DIR)\ip.obj \
        $(OBJ_DIR)\fe-auth.obj \
        $(OBJ_DIR)\fe-protocol2.obj \
        $(OBJ_DIR)\fe-protocol3.obj \
        $(OBJ_DIR)\fe-connect.obj \
        $(OBJ_DIR)\fe-exec.obj \
        $(OBJ_DIR)\fe-lobj.obj \
        $(OBJ_DIR)\fe-misc.obj \
        $(OBJ_DIR)\fe-print.obj \
        $(OBJ_DIR)\fe-secure.obj \
        $(OBJ_DIR)\pqexpbuffer.obj \
        $(OBJ_DIR)\pqsignal.obj \
        $(OBJ_DIR)\wchar.obj \
        $(OBJ_DIR)\encnames.obj \
        $(OBJ_DIR)\snprintf.obj \
        $(OBJ_DIR)\strlcpy.obj \
        $(OBJ_DIR)\pthread-win32.obj


config: ..\..\include\pg_config.h pg_config_paths.h ..\..\include\pg_config_os.h

..\..\include\pg_config.h: ..\..\include\pg_config.h.win32
	copy ..\..\include\pg_config.h.win32 ..\..\include\pg_config.h

..\..\include\pg_config_os.h:
	copy ..\..\include\port\win32.h ..\..\include\pg_config_os.h

pg_config_paths.h: pocc.mak
        echo #define SYSCONFDIR "" > pg_config_paths.h

$(OUT_DIR) : 
        @if not exist "$(OUT_DIR)/$(NULL)" mkdir "$(OUT_DIR)"

$(OBJ_DIR) : 
        @if not exist "$(OBJ_DIR)/$(NULL)" mkdir "$(OBJ_DIR)"

$(OUT_DIR)\libpq.lib : 
        $(OUT_DIR)\libpq.lib \
        $(OUT_DIR)\libpq.res

$(OUT_DIR)\libpq.lib : $(LIB_OBJS)
        $(LIB_EXE) /OUT:$@ $**

$(OUT_DIR)\libpq.res : \
        $(RES_EXE) $(RES_FLAGS) libpq.rc


#
# DEPENDENCIES
#

$(OBJ_DIR)\fe-auth.obj : fe-auth.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\fe-connect.obj : fe-connect.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\fe-exec.obj : fe-exec.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\fe-lobj.obj : fe-lobj.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\fe-misc.obj : fe-misc.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\fe-print.obj : fe-print.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\fe-protocol2.obj : fe-protocol2.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\fe-protocol3.obj : fe-protocol3.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\fe-secure.obj : fe-secure.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\pqexpbuffer.obj : pqexpbuffer.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\pqsignal.obj : pqsignal.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\pthread-win32.obj : pthread-win32.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\win32.obj : win32.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\getaddrinfo.obj : ..\..\port\getaddrinfo.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\pgstrcasecmp.obj : ..\..\port\pgstrcasecmp.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\thread.obj : ..\..\port\thread.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\inet_aton.obj : ..\..\port\inet_aton.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\crypt.obj : ..\..\port\crypt.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\noblock.obj : ..\..\port\noblock.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\md5.obj : ..\..\backend\libpq\md5.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\ip.obj : ..\..\backend\libpq\ip.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\wchar.obj : ..\..\backend\utils\mb\wchar.c
        $(CC_EXE) /I"." $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\encnames.obj : ..\..\backend\utils\mb\encnames.c
        $(CC_EXE) /I"." $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\snprintf.obj : ..\..\port\snprintf.c
        $(CC_EXE) /I"." $(CC_FLAGS) /Fo$@ $**

$(OBJ_DIR)\strlcpy.obj : ..\..\port\strlcpy.c
        $(CC_EXE) $(CC_FLAGS) /Fo$@ $**
