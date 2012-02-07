#
# $Id$
#

ROOT = ../../../

C_SOURCES=\
	pcre_chartables.c \
	pcre_compile.c \
	pcre_config.c \
	pcre_dfa_exec.c \
	pcre_exec.c \
	pcre_fullinfo.c \
	pcre_get.c \
	pcre_globals.c \
	pcre_byte_order.c \
	pcre_maketables.c \
	pcre_newline.c \
	pcre_ord2utf8.c \
	pcre_refcount.c \
	pcre_study.c \
	pcre_tables.c \
	pcre_valid_utf8.c \
	pcre_version.c \
	pcre_xclass.c \
   pcre_ucd.c \
	pcreposix.c 

LIBNAME=pcrepos

include $(TOP)$(ROOT)config/lib.cf

CFLAGS += -DHAVE_CONFIG_H

# it's necessary for POCC/XCC compilers
CFLAGS += -I../../
