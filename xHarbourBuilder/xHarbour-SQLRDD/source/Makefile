ROOT = ../../../

YACC_FLAGS = -p hb_comp

YACC_SOURCE=sql.y

#YACC_HEADERS=\
#	hbsql.h \
#	msg.ch \
#	hbapi.h \
#	hbapiitm.h \

C_SOURCES=\
	firebird.c \
	mysql.c \
	pgs.c \
	sqlact.c \
	sqllex.c \
	sqlora.c \
	sqlorax.c \
	sqlrdd1.c \
	sqlsrodbc.c \
	sqlex1.c \
	sqlex2.c \
	sqlex3.c \
	sqly.c \


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
	sqlfirebird.prg \
	sqlgen1.prg \
	sqlmy.prg \
	sqlodbc.prg \
	sqloracle.prg \
	sqlpgs.prg \
	sqlrdd0.prg \
	sqlrdd2.prg \
	utils.prg \
	utilslang.prg \
	exprtransl.prg \
	exprparser.prg \
	exprsimpl.prg \
	exprobjs.prg \
	exprrelation.prg \
	exprudf.prg \
	sqlmaria.prg \



#sqloracle.prg \
#sqlmy.prg \

include $(TOP)$(ROOT)config/lib.cf

CFLAGS += -I. -I../.. -I../../Include 
