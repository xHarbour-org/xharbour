/* This definition has to be placed before #include "hbapigt.h" */
#define HB_GT_FNPREF(x) crs ## x

#include "hbapigt.h"
#include "inkey.ch"
#include "setcurs.ch"

#ifndef HB_OS_LINUX

#define __LINUX__
#define HAVE_GPM_H
#define HB_OS_LINUX

#define hb_xgrab	malloc
#define hb_xfree	free
#define hb_xrealloc	realloc

#define hb_idleState()	
#define hb_idleReset()	

#define HB_MIN(x,y)	(x < y ? x : y)
#define HB_MAX(x,y)	(x > y ? x : y)

#define HB_TRACE(x,y)	
#define HB_TR_DEBUG	0
#define HB_SYMBOL_UNUSED(x)	{(x)=(x);}
#define USHORT	unsigned short
#define ULONG	unsigned long
#define UINT	unsigned int
#define SHORT	short
#define BOOL	int
#define BYTE	unsigned char

#define HB_inkey_enum	int

#define hb_gtRectSize( uiTop, uiLeft, uiBottom, uiRight, puiBuffSize ) (*puiBuffSize = 0)

#endif

#ifndef _POSIX_PATH_MAX
#define _POSIX_PATH_MAX 256
#endif

extern int HB_GT_FUNC(gt_chrmapinit( int *piTransTbl, char *pszTerm ));

#define BASE_INFD	0
#define BASE_OUTFD	1
#define BASE_ERRFD	2
#define MAXFD		1024


#define NO_IFDS		10
#define ESC_DELAY	300
#define DBLCLK_DELAY	250

#define MAX_IOBASE	32
#define STDIN_BUFLEN	128

#define TERM_LINUX	1
#define TERM_XTERM	2

#define IS_EVTFDSTAT(x)	((x) >= 0x01 && (x) <= 0x03)
#define EVTFDSTAT_RUN	0x01
#define EVTFDSTAT_STOP	0x02
#define EVTFDSTAT_DEL	0x03

#define CTRL_SEQ	"\036"
#define ALT_SEQ		"\037"
//#define NATION_SEQ	"\016"

#define KP_CENTER               332   /* * Keypad 5                      */

#define K_UNDEF  	0x10000
#define K_METAALT	0x10001
#define K_METACTRL	0x10002
#define K_NATIONAL	0x10003
#define K_MOUSETERM	0x10004
#define K_PRTSCR	0x10005
#define K_PAUSE		0x10006

#define SC_UNDEF	-1

#define KEY_ALTMASK	0x10000000
#define KEY_CTRLMASK	0x20000000
#define KEY_EXTDMASK	0x40000000
#define KEY_CLIPMASK	0x80000000
#define KEY_MASK	0xF0000000

#define CLR_KEYMASK(x)	((x) & ~KEY_MASK)
#define GET_KEYMASK(x)	((x) & KEY_MASK)

#define IS_CLIPKEY(x)	((((x) & ~0xffff) ^ KEY_CLIPMASK) == 0)
#define SET_CLIPKEY(x)	(((x) & 0xffff) | KEY_CLIPMASK)
#define GET_CLIPKEY(x)	((((x) & 0x8000) ? ~0xffff : 0) | ((x) & 0xffff))

#define NO_STDKEYS	96
#define NO_EXTDKEYS	30

#define EXKEY_F1	( 0 | KEY_EXTDMASK)
#define EXKEY_F2	( 1 | KEY_EXTDMASK)
#define EXKEY_F3	( 2 | KEY_EXTDMASK)
#define EXKEY_F4	( 3 | KEY_EXTDMASK)
#define EXKEY_F5	( 4 | KEY_EXTDMASK)
#define EXKEY_F6	( 5 | KEY_EXTDMASK)
#define EXKEY_F7	( 6 | KEY_EXTDMASK)
#define EXKEY_F8	( 7 | KEY_EXTDMASK)
#define EXKEY_F9	( 8 | KEY_EXTDMASK)
#define EXKEY_F10	( 9 | KEY_EXTDMASK)
#define EXKEY_F11	(10 | KEY_EXTDMASK)
#define EXKEY_F12	(11 | KEY_EXTDMASK)
#define EXKEY_UP	(12 | KEY_EXTDMASK)
#define EXKEY_DOWN	(13 | KEY_EXTDMASK)
#define EXKEY_LEFT	(14 | KEY_EXTDMASK)
#define EXKEY_RIGHT	(15 | KEY_EXTDMASK)
#define EXKEY_INS	(16 | KEY_EXTDMASK)
#define EXKEY_DEL	(17 | KEY_EXTDMASK)
#define EXKEY_HOME	(18 | KEY_EXTDMASK)
#define EXKEY_END	(19 | KEY_EXTDMASK)
#define EXKEY_PGUP	(20 | KEY_EXTDMASK)
#define EXKEY_PGDN	(21 | KEY_EXTDMASK)
#define EXKEY_BS	(22 | KEY_EXTDMASK)
#define EXKEY_TAB	(23 | KEY_EXTDMASK)
#define EXKEY_ESC	(24 | KEY_EXTDMASK)
#define EXKEY_ENTER	(25 | KEY_EXTDMASK)
#define EXKEY_KPENTER	(26 | KEY_EXTDMASK)
#define EXKEY_CENTER	(27 | KEY_EXTDMASK)
#define EXKEY_PRTSCR	(28 | KEY_EXTDMASK)
#define EXKEY_PAUSE	(29 | KEY_EXTDMASK)

#define M_BUTTON_LEFT	0x01
#define M_BUTTON_RIGHT	0x02
#define M_BUTTON_MIDDLE	0x04
#define M_BUTTON_LDBLCK	0x10
#define M_BUTTON_RDBLCK	0x20
#define M_BUTTON_MDBLCK	0x40

#define TIMEVAL_GET(tv)		gettimeofday(&(tv), NULL);
#define TIMEVAL_LESS(tv1, tv2)	(((tv1).tv_sec == (tv2).tv_sec ) ?	\
				 ((tv1).tv_usec < (tv2).tv_usec) :	\
				 ((tv1).tv_sec  < (tv2).tv_sec ))
#define TIMEVAL_ADD(dst, src, n)	{				\
	(dst).tv_sec = (src).tv_sec + n / 1000;				\
	if (((dst).tv_usec = (src).tv_usec+(n%1000)*1000)>=1000000) {	\
	    (dst).tv_usec -= 1000000; (dst).tv_sec++;			\
	} \
    }
