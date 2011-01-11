ROOT = ../../

YACC_FLAGS = -p hb_comp

YACC_SOURCE=sql.y

#YACC_HEADERS=\
#	hbsql.h \
#	msg.ch \
#	hbapi.h \
#	hbapiitm.h \

C_SOURCES=\
odbc.c \
pgs.c \
sqlact.c \
sqllex.c \
sqlrdd1.c \

#mysql.c \
#oracle.c \
#orautils.c \
#orautils2.c \

ifeq ($(HB_MT),MT)
   MT_LIBNAME=sqlmt
endif

LIBNAME=sql

PRG_SOURCES= \
sqlconnection.prg \
sqlgen1.prg \
sqlodbc.prg \
sqlpgs.prg \
sqlrdd0.prg \
sqlrdd2.prg \
utils.prg \
utilslang.prg \

#sqloracle.prg \
#sqlmy.prg \

include $(TOP)$(ROOT)config/lib.cf
