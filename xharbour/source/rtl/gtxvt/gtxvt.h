/*
 * $Id: gtxvt.c,v 1.1 2003/12/29 23:38:57 jonnymind Exp $
 */

/*
 * Xharbour Project source code:
 * X11 Virtual terminal
 * Copyright 2003 - Giancarlo Niccolai <antispam /at/ niccolai.ws>
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

/* This definition has to be placed before #include "hbapigt.h" */

#ifndef HB_GTXVT_H
#define HB_GTXVT_H

#define HB_GT_NAME	XVT

#include "hbset.h"
#include "hbvm.h"
#include "hbapi.h"
#include "hbapigt.h"
#include "hbapierr.h"
#include "inkey.ch"
#include "error.ch"
#include <signal.h>
#include <sys/time.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

/************************************************************/
/* Utility functions                                        */
typedef USHORT HB_GT_CELLTYPE;
#define HB_GT_CELLSIZE  sizeof( HB_GT_CELLTYPE )

#define HB_GT_INDEXOF( wnd, col, row ) \
      (( row * wnd->cols + col ) )

#ifndef max
   #define max( a, b) ( a > b ? a : b )
#endif

#ifndef min
   #define min( a, b ) ( a < b ? a : b )
#endif


#define XVT_CHAR_QUEUE_SIZE  128
#define XVT_CHAR_BUFFER     1024
#define XVT_MAX_ROWS         256
#define XVT_MAX_COLS         256
#define XVT_DEFAULT_ROWS      25
#define XVT_DEFAULT_COLS      80
#define XVT_MAX_BUTTONS       8
#define CLIP_KEY_COUNT	122

#define XVT_STD_MASK    (ExposureMask | ButtonPressMask | ButtonReleaseMask | PointerMotionMask | KeyPressMask | KeyReleaseMask | ButtonPressMask | StructureNotifyMask)

/* Box char definitions - these are compatible with unicode, so that it can
   be used inside unicode char definitions*/

#define HB_GTXVT_DBL_LT    0xE000 /* Double left top angle*/
#define HB_GTXVT_DBL_TD    0xE001 /* Double top with junction down */
#define HB_GTXVT_DBL_RT    0xE002 /* Double right top angle */

#define HB_GTXVT_DBL_LB    0xE003 /* Double left bottom angle*/
#define HB_GTXVT_DBL_BU    0xE004 /* Double bottom with junction up */
#define HB_GTXVT_DBL_RB    0xE005 /* Double right bottom angle */

#define HB_GTXVT_DBL_VL    0xE006 /* Double Vertical with left junction */
#define HB_GTXVT_DBL_VR    0xE007 /* Double vertical with right junction */
#define HB_GTXVT_DBL_CRS   0xE008 /* Double cross */

#define HB_GTXVT_DBL_HOR   0xE00A /* Double Horizontal bar */
#define HB_GTXVT_DBL_VRT   0xE00B /* Double Vertical bar*/

#define HB_GTXVT_SNG_LT    0xE010 /* Single left top angle*/
#define HB_GTXVT_SNG_TD    0xE011 /* Single top with junction down */
#define HB_GTXVT_SNG_RT    0xE012 /* Single right top angle */

#define HB_GTXVT_SNG_LB    0xE013 /* Single left bottom angle*/
#define HB_GTXVT_SNG_BU    0xE014 /* Single bottom with junction up */
#define HB_GTXVT_SNG_RB    0xE015 /* Single right bottom angle */

#define HB_GTXVT_SNG_VL    0xE016 /* Single Vertical with left junction */
#define HB_GTXVT_SNG_VR    0xE017 /* Single vertical with right junction */
#define HB_GTXVT_SNG_CRS   0xE018 /* Single cross */

#define HB_GTXVT_SNG_HOR   0xE01A /* Single Horizontal bar */
#define HB_GTXVT_SNG_VRT   0xE01B /* Single Vertical bar*/

#define HB_GTXVG_FULL      0xE090 /* Full character filler */
#define HB_GTXVG_FULL_T    0xE091
#define HB_GTXVG_FULL_B    0xE092
#define HB_GTXVG_FULL_L    0xE094
#define HB_GTXVG_FULL_R    0xE095

#define HB_GTXVG_FILLER1   0xE0A0
#define HB_GTXVG_FILLER2   0xE0A1
#define HB_GTXVG_FILLER3   0xE0A2

#endif
