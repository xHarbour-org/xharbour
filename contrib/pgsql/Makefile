
ROOT = ../../

C_SOURCES=\
        postgres.c \

PRG_SOURCES=\
        tpostgres.prg

LIBNAME=hbpg

include $(TOP)$(ROOT)config/lib.cf

ifeq ($(HB_COMPILER),mingw32)
   CFLAGS := $(CFLAGS) -I/postgres/include
else
   CFLAGS := $(CFLAGS) -I/usr/local/pgsql/include -I/usr/local/pgsql/include/libpq -I/usr/include/pgsql -I/usr/include/postgresql/
endif
