
ROOT = ../../

C_SOURCES=\
        postgres.c \

PRG_SOURCES=\
        TPostgres.prg  \

LIBNAME=hbpg

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_COMPILER),mingw32)
   CFLAGS := $(CFLAGS) -I/postgres/include
else
   CFLAGS := $(CFLAGS) -I/usr/local/pgsql/include -I/usr/local/pgsql/include/libpq -I/usr/include/pgsql
endif
