#
# $Id: Makefile,v 1.8 2004/09/19 03:59:20 kaddath Exp $
#

ROOT = ../../

C_SOURCES=\
      cdpesdos.c    \
      cdpeswin.c    \
      cdpgedos.c    \
      cdpgewin.c    \
      cdphr852.c    \
      cdpltwin.c    \
      cdppl852.c    \
      cdppliso.c    \
      cdpplmaz.c    \
      cdpplwin.c    \
      cdppt850.c    \
      cdpptiso.c    \
      cdpru866.c    \
      cdprukoi.c    \
      cdpruwin.c    \
      cdpsl852.c    \
      cdpsliso.c    \
      cdpslwin.c    \
      cdpsrwin.c    \
      uc1250.c      \
      uc1251.c      \
      uc1257.c      \
      uc850.c       \
      uc852.c       \
      uc866.c       \
      uc8859_1.c    \
      uc8859_2.c    \
      uckoi8.c      \
      ucmaz.c       \

# temporary hack for 8.3 file name length in DOS
ifneq ($(HB_ARCHITECTURE),dos)
C_SOURCES += cdpesmwin.c cdphr1250.c
endif

LIBNAME=codepage

include $(TOP)$(ROOT)config/lib.cf
