#
# $Id: Makefile,v 1.4 2003/04/09 17:47:00 lculik Exp $
#

ROOT = ../../

ifeq ($(HB_ARCHITECTURE),linux)

# MYSQL_INC_PATH is a directory name of installed mysql.h
# If you install mysql by RPM(Red-hat package manager) then it is be /usr/include/mysql.
# MYSQL_LIB_PATH does not use at make libmysql.a but use at compile application that used libmysql.a
# 
# MYSQL_LIB_PATH=/usr/local/mysql/lib/mysql
# MYSQL_INC_PATH=/usr/local/mysql/include/mysql
# C_USR += -I$(MYSQL_INC_PATH)
# LINKPATH += -L$(MYSQL_LIB_PATH)

# a workaround of a problem with different include directories 
# for mysql header files between different unix distributions
CFLAGS += -I/usr/include/mysql -I/usr/local/include/mysql \
          -I/usr/local/mysql/include/mysql

endif

ifneq ($(HB_ARCHITECTURE),dos)

C_SOURCES=\
        mysql.c \

PRG_SOURCES=\
        tmysql.prg  \
        tsqlbrw.prg \

LIBNAME=mysql
ifneq ($(HB_ARCHITECTURE),dos)
MT_LIBNAME=mysqlmt
endif

include $(TOP)$(ROOT)config/lib.cf

endif

