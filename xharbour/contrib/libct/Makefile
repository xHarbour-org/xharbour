#
# $Id: Makefile,v 1.16 2004/01/26 14:59:46 druzus Exp $
#

ROOT = ../../

PRG_HEADERS = \
	include/ct.ch \
	include/cterror.ch \
	include/ctwin.ch \

C_SOURCES = \
	addascii.c \
	asciisum.c \
	ascpos.c \
	atadjust.c \
	atnum.c \
	atrepl.c \
	bit1.c \
	bit2.c \
	bit3.c \
	charevod.c \
	charlist.c \
	charmirr.c \
	charmix.c \
	charone.c \
	charonly.c \
	charop.c \
	charrepl.c \
	charsort.c \
	charswap.c \
	count.c \
	ctc.c \
	ctmath.c \
	ctset.c \
	ctstr.c \
	ctchksum.c \
	ctcrypt.c \
	dbftools.c \
    disk.c \
	exponent.c \
	files.c \
	finan.c \
	ftoc.c \
	justify.c \
	keyset.c \
	math.c \
	misc1.c \
	net.c \
	num1.c \
	numat.c \
	numlohi.c \
	pad.c \
	pos1.c \
	pos2.c \
	posdiff.c \
	print.c \
	range.c \
	relation.c \
	remove.c \
	replace.c \
	screen1.c \
    settime.c \
	strdiff.c \
	strswap.c \
	tab.c \
	token1.c \
	token2.c \
	trig.c \
	wordrepl.c \
	wordtoch.c \

PRG_SOURCES= \
	blank.prg  \
	color.prg \
	ct.prg \
	ctwin.prg \
	ctmisc.prg \
	datetime.prg \
	dattime2.prg \
    dattime3.prg \
    diskutil.prg \
	fcopy.prg \
	invrtwin.prg \
	numconv.prg \
	strfile.prg \
	screen2.prg \
	time.prg \
    util.prg 


LIBNAME=ct
ifeq ($(HB_MT),MT)
  MT_LIBNAME=ctmt
endif

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
