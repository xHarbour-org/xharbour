#
# $Id: Makefile,v 1.1 2003/02/03 05:19:18 walito Exp $
#

ROOT = ../../

ifeq ($(HB_ARCHITECTURE),linux)
#
# MYSQL_INC_PATH is a directory name of installed mysql.h
# If you install mysql by RPM(Red-hat package manager) then it is be /usr/include/mysql.
# MYSQL_LIB_PATH does not use at make libmysql.a but use at compile application that used libmysql.a
# 

MYSQL_LIB_PATH=/usr/local/mysql/lib/mysql
MYSQL_INC_PATH=/usr/local/mysql/include/mysql

C_USR += -I$(MYSQL_INC_PATH)
LINKPATH += -L$(MYSQL_LIB_PATH)

endif

ifneq ($(HB_ARCHITECTURE),dos)

C_SOURCES=\
        mysql.c \

PRG_SOURCES=\
        tmysql.prg  \
        tsqlbrw.prg \

LIBNAME=mysql

include $(TOP)$(ROOT)config/lib.cf

endif

