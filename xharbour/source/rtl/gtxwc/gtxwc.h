/*
 * $Id: gtxwc.h,v 1.3 2005/02/06 20:35:43 druzus Exp $
 */

/*
 * Xharbour Project source code:
 * XWindow Terminal
 * Copyright 2003 - Giancarlo Niccolai <antispam /at/ niccolai.ws>
 * Copyright 2004 - Przemys³aw Czerpak <druzus /at/ priv.onet.pl>
 *
 * See doc/license.txt for licensing terms.
 *
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* NOTE: User programs should never call this layer directly! */

#ifndef HB_GTXWC_H
#define HB_GTXWC_H

/* This definition has to be placed before #include "hbapigt.h" */
#define HB_GT_NAME	XWC

#include "hbset.h"
#include "hbvm.h"
#include "hbapi.h"
#include "hbapigt.h"
#include "hbapierr.h"
#include "inkey.ch"
#include "error.ch"

#ifndef HB_CDP_SUPPORT_OFF
#include "hbapicdp.h"
#endif

#include <unistd.h>
#include <signal.h>
#include <sys/time.h>

#include <X11/Xlib.h>
#include <X11/Xcms.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

/************************************************************/
/* Utility functions                                        */
typedef unsigned long HB_GT_PIXELTYPE;
typedef USHORT HB_GT_CELLTYPE;

#define XVT_CHAR_QUEUE_SIZE         128
#define XVT_CHAR_BUFFER            1024
#define XVT_MIN_ROWS                  3
#define XVT_MIN_COLS                  6
#define XVT_MAX_ROWS                256
#define XVT_MAX_COLS                256
#define XVT_DEFAULT_ROWS             25
#define XVT_DEFAULT_COLS             80
#define XVT_DEFAULT_COLOR             7
#define XVT_MAX_BUTTONS               8
#define XVT_MAX_CHAR_POINTS        1024
#define XVT_DBLCLK_DELAY            250

/* Font definition */
#define XVT_DEFAULT_FONT_HEIGHT      18
#define XVT_DEFAULT_FONT_WIDTH        9
#define XVT_DEFAULT_FONT_WEIGHT    "medium"
#define XVT_DEFAULT_FONT_NAME      "fixed"
#define XVT_DEFAULT_FONT_ENCODING  "iso10646-1"

//#define XVT_DEFAULT_FONT_WEIGHT    "*"
//#define XVT_DEFAULT_FONT_ENCODING  "iso8859-1"
//#define XVT_DEFAULT_FONT_NAME   "Lucida Console"

//#define XVT_DEFAULT_FONT_HEIGHT      20
//#define XVT_DEFAULT_FONT_WIDTH        9
//#define XVT_DEFAULT_FONT_WEIGHT    "medium"
//#define XVT_DEFAULT_FONT_NAME      "rcsoft"
//#define XVT_DEFAULT_FONT_ENCODING  "iso10646-1"

#define XVT_DEFAULT_FONT_FIXMETRIC  FALSE
#define XVT_DEFAULT_FONT_CLRBKG     FALSE
#define XVT_DEFAULT_FONT_DRAWBOX    TRUE


#define XVT_SYNC_UPDATE         0
#define XVT_ASYNC_UPDATE        1

#define XVT_FULL_PIXMAP
//#define XVT_CHAR_PIXMAP

#define CLIP_STDKEY_COUNT      96
#define CLIP_EXTKEY_COUNT      30

#define XVT_EXTKEYMASK        0x10000000
#define XVT_KEYMASK           0xF0000000
#define XVT_CLR_KEYMASK(x)    ((x) & ~XVT_KEYMASK)
#define XVT_IS_EXTKEY(x)      (((x) & XVT_EXTKEYMASK) != 0)

#define EXKEY_F1              ( 0 | XVT_EXTKEYMASK)
#define EXKEY_F2              ( 1 | XVT_EXTKEYMASK)
#define EXKEY_F3              ( 2 | XVT_EXTKEYMASK)
#define EXKEY_F4              ( 3 | XVT_EXTKEYMASK)
#define EXKEY_F5              ( 4 | XVT_EXTKEYMASK)
#define EXKEY_F6              ( 5 | XVT_EXTKEYMASK)
#define EXKEY_F7              ( 6 | XVT_EXTKEYMASK)
#define EXKEY_F8              ( 7 | XVT_EXTKEYMASK)
#define EXKEY_F9              ( 8 | XVT_EXTKEYMASK)
#define EXKEY_F10             ( 9 | XVT_EXTKEYMASK)
#define EXKEY_F11             (10 | XVT_EXTKEYMASK)
#define EXKEY_F12             (11 | XVT_EXTKEYMASK)
#define EXKEY_UP              (12 | XVT_EXTKEYMASK)
#define EXKEY_DOWN            (13 | XVT_EXTKEYMASK)
#define EXKEY_LEFT            (14 | XVT_EXTKEYMASK)
#define EXKEY_RIGHT           (15 | XVT_EXTKEYMASK)
#define EXKEY_INS             (16 | XVT_EXTKEYMASK)
#define EXKEY_DEL             (17 | XVT_EXTKEYMASK)
#define EXKEY_HOME            (18 | XVT_EXTKEYMASK)
#define EXKEY_END             (19 | XVT_EXTKEYMASK)
#define EXKEY_PGUP            (20 | XVT_EXTKEYMASK)
#define EXKEY_PGDN            (21 | XVT_EXTKEYMASK)
#define EXKEY_BS              (22 | XVT_EXTKEYMASK)
#define EXKEY_TAB             (23 | XVT_EXTKEYMASK)
#define EXKEY_ESC             (24 | XVT_EXTKEYMASK)
#define EXKEY_ENTER           (25 | XVT_EXTKEYMASK)
#define EXKEY_KPENTER         (26 | XVT_EXTKEYMASK)
#define EXKEY_CENTER          (27 | XVT_EXTKEYMASK)
#define EXKEY_PRTSCR          (28 | XVT_EXTKEYMASK)
#define EXKEY_PAUSE           (29 | XVT_EXTKEYMASK)


#define XVT_STD_MASK    ( ExposureMask | StructureNotifyMask | FocusChangeMask | \
                          ButtonPressMask | ButtonReleaseMask | PointerMotionMask | \
                          KeyPressMask | KeyReleaseMask )

/* Box char unicode values */
#define HB_GTXVG_ARROW_R   0x0010
#define HB_GTXVG_ARROW_L   0x0011
#define HB_GTXVG_ARROW_U   0x001E
#define HB_GTXVG_ARROW_D   0x001F

#define HB_GTXVT_DBL_LT    0x2554 /* BOX DRAWINGS DOUBLE DOWN AND RIGHT (Double left top angle) */
#define HB_GTXVT_DBL_TD    0x2566 /* BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL (Double top with junction down) */
#define HB_GTXVT_DBL_RT    0x2557 /* BOX DRAWINGS DOUBLE DOWN AND LEFT (Double right top angle) */

#define HB_GTXVT_DBL_LB    0x255A /* BOX DRAWINGS DOUBLE UP AND RIGHT (Double left bottom angle) */
#define HB_GTXVT_DBL_BU    0x2569 /* BOX DRAWINGS DOUBLE UP AND HORIZONTAL (Double bottom with junction up) */
#define HB_GTXVT_DBL_RB    0x255D /* BOX DRAWINGS DOUBLE DOWN AND LEFT (Double right bottom angle) */

#define HB_GTXVT_DBL_VL    0x2560 /* BOX DRAWINGS DOUBLE VERTICAL AND RIGHT (Double Vertical with left junction) */
#define HB_GTXVT_DBL_VR    0x2563 /* BOX DRAWINGS DOUBLE VERTICAL AND LEFT (Double vertical with right junction) */
#define HB_GTXVT_DBL_CRS   0x256C /* BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL (Double cross) */

#define HB_GTXVT_DBL_HOR   0x2550 /* BOX DRAWINGS DOUBLE HORIZONTAL (Double Horizontal bar) */
#define HB_GTXVT_DBL_VRT   0x2551 /* BOX DRAWINGS DOUBLE VERTICAL (Double Vertical bar) */

#define HB_GTXVT_SNG_LT    0x250C /* BOX DRAWINGS LIGHT DOWN AND RIGHT (Single left top angle) */
#define HB_GTXVT_SNG_TD    0x252C /* BOX DRAWINGS LIGHT DOWN AND HORIZONTAL (Single top with junction down) */
#define HB_GTXVT_SNG_RT    0x2510 /* BOX DRAWINGS LIGHT DOWN AND LEFT (Single right top angle) */

#define HB_GTXVT_SNG_LB    0x2514 /* BOX DRAWINGS LIGHT UP AND RIGHT (Single left bottom angle) */
#define HB_GTXVT_SNG_BU    0x2534 /* BOX DRAWINGS LIGHT UP AND HORIZONTAL (Single bottom with junction up) */
#define HB_GTXVT_SNG_RB    0x2518 /* BOX DRAWINGS LIGHT UP AND LEFT (Single right bottom angle) */

#define HB_GTXVT_SNG_VL    0x251C /* BOX DRAWINGS LIGHT VERTICAL AND RIGHT (Single Vertical with left junction) */
#define HB_GTXVT_SNG_VR    0x2524 /* BOX DRAWINGS LIGHT VERTICAL AND LEFT (Single vertical with right junction) */
#define HB_GTXVT_SNG_CRS   0x253C /* BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL (Single cross) */

#define HB_GTXVT_SNG_HOR   0x2500 /* BOX DRAWINGS LIGHT HORIZONTAL (Single Horizontal bar) */
#define HB_GTXVT_SNG_VRT   0x2502 /* BOX DRAWINGS LIGHT VERTICAL (Single Vertical bar) */


#define HB_GTXVT_SNG_L_DBL_T 0x2552 /* BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE (Single left double top angle) */
#define HB_GTXVT_SNG_T_DBL_D 0x2565 /* BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE (Single top with double junction down) */
#define HB_GTXVT_SNG_R_DBL_T 0x2556 /* BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE (Single right double top angle) */

#define HB_GTXVT_SNG_L_DBL_B 0x2558 /* BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE (Single left double bottom angle) */
#define HB_GTXVT_SNG_B_DBL_U 0x2568 /* BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE (Single bottom double with junction up) */
#define HB_GTXVT_SNG_R_DBL_B 0x255C /* BOX DRAWINGS UP DOUBLE AND LEFT SINGLE (Single right double bottom angle) */

#define HB_GTXVT_SNG_V_DBL_L 0x255E /* BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE (Single Vertical double left junction) */
#define HB_GTXVT_SNG_V_DBL_R 0x2561 /* BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE (Single vertical double right junction) */
#define HB_GTXVT_SNG_DBL_CRS 0x256A /* BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE (Single cross (double horiz) */


#define HB_GTXVT_DBL_L_SNG_T 0x2553 /* BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE (Double left single top angle) */
#define HB_GTXVT_DBL_T_SNG_D 0x2564 /* BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE (Double top signle junction down) */
#define HB_GTXVT_DBL_R_SNG_T 0x2555 /* BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE (Double right single top angle) */

#define HB_GTXVT_DBL_L_SNG_B 0x2559 /* BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE (Double left single bottom angle) */
#define HB_GTXVT_DBL_B_SNG_U 0x2567 /* BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE (Double bottom single junction up) */
#define HB_GTXVT_DBL_R_SNG_B 0x255B /* BOX DRAWINGS UP SINGLE AND LEFT DOUBLE (Double right single bottom angle) */

#define HB_GTXVT_DBL_V_SNG_R 0x2562 /* BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE (Double Vertical single left junction) */
#define HB_GTXVT_DBL_V_SNG_L 0x255F /* BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE (Double vertical single right junction) */
#define HB_GTXVT_DBL_SNG_CRS 0x256B /* BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE (Double cross (single horiz) */

#define HB_GTXVG_FULL      0x2588   /* FULL BLOCK */
#define HB_GTXVG_FULL_B    0x2584   /* LOWER HALF BLOCK */
#define HB_GTXVG_FULL_L    0x258C   /* LEFT HALF BLOCK */
#define HB_GTXVG_FULL_R    0x2590   /* RIGHT HALF BLOCK */
#define HB_GTXVG_FULL_T    0x2580   /* UPPER HALF BLOCK */

#define HB_GTXVG_FILLER1   0x2591   /* LIGHT SHADE */
#define HB_GTXVG_FILLER2   0x2592   /* MEDIUM SHADE */
#define HB_GTXVG_FILLER3   0x2593   /* DARK SHADE */

#define HB_GTXVG_SQUARE    0x25A0   /* BLACK SQUARE */

/********************** Unix to graphic box translation ******************/

#ifdef HB_CDP_SUPPORT_OFF
typedef struct tag_UnixBox {
    BYTE   c;
    USHORT u16;
} UnixBoxChar;
#endif

typedef enum
{
   CH_CHAR,          /* normal U16 character */
   CH_CHBX,          /* U16 character built by DrawBoxChar */
   CH_NONE,          /* no character share */
   CH_IMG,           /* character built from image */
   CH_PTS,           /* character built from relative points */
   CH_LINE,          /* character built from relative lines */
   CH_SEG,           /* character built from lines (segments) */
   CH_RECT,          /* character built from rectangles */
   CH_POLY           /* character built by polygon */
} XVT_CharType;

typedef struct tag_XVT_CharTrans
{
   XVT_CharType   type;
   union
   {
      XImage      *img;
      XPoint      *pts;
      XSegment    *seg;
      XRectangle  *rect;
      USHORT      ch16;
   } u;
   BYTE  size;
   BOOL  inverse;
} XVT_CharTrans;

#endif
