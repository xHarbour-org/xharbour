#
# $Id$
#

ROOT = ../

ifeq ($(HB_COMPILER),bcc16)

# Don't build the Harbour compiler for 16-bit DOS,
# because it frequently runs out of memory, so it
# is necessary to use the DJGPP Harbour compiler.

DIRS=\
	common \
	pp \
	rtl \
	vm \
	macro \
	codepage \
	lang \
	rdd \
	debug \

else

HB_COMP_REF={compiler}

DIRS=\
	common \
	pp{common} \
	compiler{pp} \
	rtl$(HB_COMP_REF) \
	vm$(HB_COMP_REF) \
	macro \
	codepage \
	lang \
	rdd$(HB_COMP_REF) \
	debug$(HB_COMP_REF) \
	odbc$(HB_COMP_REF) \
	ct$(HB_COMP_REF) \
	cgi$(HB_COMP_REF)

ifneq ($(HB_ARCHITECTURE),dos)

DIRS+=\
	tip$(HB_COMP_REF) 

endif

endif

include $(ROOT)config/dir.cf
