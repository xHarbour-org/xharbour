/*
 * $Id: gtxvt.h,v 1.4 2004/01/05 04:54:30 jonnymind Exp $
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

#ifdef HB_BIG_ENDIAN
   #define XVT_SWAP_ENDIAN( value )
#else
   #define XVT_SWAP_ENDIAN( value ) \
      (0xFFFF & ( (value << 8) | (value>>8) ))
#endif

#define XVT_INITIALIZE \
   if ( ! s_gtxvt_initialized ) {\
      s_gtxvt_initialized = TRUE;\
      xvt_InitDisplay( s_buffer, s_status );\
   }

#define COMMIT_STATUS( status ) \
   msync( status, sizeof( XVT_STATUS ), MS_INVALIDATE | MS_ASYNC )

#define COMMIT_BUFFER( buffer ) \
   msync( buffer, sizeof( XVT_BUFFER ), MS_INVALIDATE | MS_ASYNC )

#define XVT_CHAR_QUEUE_SIZE  128
#define XVT_CHAR_BUFFER     1024
#define XVT_MAX_ROWS          64
#define XVT_MAX_COLS         168
#define XVT_DEFAULT_ROWS      25
#define XVT_DEFAULT_COLS      80
#define XVT_MAX_BUTTONS        8
#define CLIP_KEY_COUNT       122
#define XVT_BOX_CHARS         49

/************************************************************************/
/* XVT intercommunication protocol XVT_ICM                              */
#define XVT_ICM_KEYSTORE      0     // follows a CLIPPER keystore (USHORT)
#define XVT_ICM_RESIZE        1     // Resize request. Follows an ICM_DATA_RESIZE structure
#define XVT_ICM_UPDATE        2     // UPDATE request. Follows an ICM_DATA_UPDATE struture
#define XVT_ICM_MOUSEMOVE     3     // Change mouse position. Follows an ICM_DATA_RESIZE struture
#define XVT_ICM_SETCURSOR     4     // Application has changed cursor shape
#define XVT_ICM_BEGIN         50
#define XVT_ICM_QUIT          100   // App requests message loop to quit


typedef struct tag_ICM_RESIZE
{
   USHORT rows;
   USHORT cols;
} ICM_DATA_RESIZE;

typedef struct tag_ICM_UPDATE
{
   USHORT x1;
   USHORT y1;
   USHORT x2;
   USHORT y2;
} ICM_DATA_UPDATE;

/********************************************************************/

#define XVT_STD_MASK    (ExposureMask | ButtonPressMask | ButtonReleaseMask | PointerMotionMask | KeyPressMask | KeyReleaseMask | ButtonPressMask | StructureNotifyMask | VisibilityChangeMask)

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



#define HB_GTXVT_SNG_L_DBL_T 0xE020 /* Single left double top angle*/
#define HB_GTXVT_SNG_T_DBL_D 0xE021 /* Single top with double junction down */
#define HB_GTXVT_SNG_R_DBL_T 0xE022 /* Single right double top angle */

#define HB_GTXVT_SNG_L_DBL_B 0xE023 /* Single left double bottom angle*/
#define HB_GTXVT_SNG_B_DBL_U 0xE024 /* Single bottom double with junction up */
#define HB_GTXVT_SNG_R_DBL_B 0xE025 /* Single right double bottom angle */

#define HB_GTXVT_SNG_V_DBL_L 0xE026 /* Single Vertical double left junction */
#define HB_GTXVT_SNG_V_DBL_R 0xE027 /* Single vertical double right junction */
#define HB_GTXVT_SNG_DBL_CRS 0xE028 /* Single cross (double horiz)*/


#define HB_GTXVT_DBL_L_SNG_T 0xE030 /* Double left single top angle*/
#define HB_GTXVT_DBL_T_SNG_D 0xE031 /* Double top signle junction down */
#define HB_GTXVT_DBL_R_SNG_T 0xE032 /* Double right single top angle */

#define HB_GTXVT_DBL_L_SNG_B 0xE033 /* Double left single bottom angle*/
#define HB_GTXVT_DBL_B_SNG_U 0xE034 /* Double bottom single junction up */
#define HB_GTXVT_DBL_R_SNG_B 0xE035 /* Double right single bottom angle */

#define HB_GTXVT_DBL_V_SNG_L 0xE036 /* Double Vertical single left junction */
#define HB_GTXVT_DBL_V_SNG_R 0xE037 /* Double vertical single right junction */
#define HB_GTXVT_DBL_SNG_CRS 0xE038 /* Double cross (single horiz) */


#define HB_GTXVT_FULL      0xE090 /* Full character filler */
#define HB_GTXVT_FULL_T    0xE091
#define HB_GTXVT_FULL_B    0xE092
#define HB_GTXVT_FULL_L    0xE094
#define HB_GTXVT_FULL_R    0xE095

#define HB_GTXVT_FILLER1   0xE0A0
#define HB_GTXVT_FILLER2   0xE0A1
#define HB_GTXVT_FILLER3   0xE0A2

/********************** Unix to graphic box translation ******************/

typedef struct tag_UnixBox {
    USHORT c1;
    USHORT c2;
} UnixBoxChar;


/********************** Virtual Buffer logical structure ******************/

typedef struct tag_xvt_buffer
{
   // cursor:
   int col;
   int row;

   // Directly clipper cursor style
   USHORT curs_style;

   // size in character cells
   USHORT cols;
   USHORT rows;

   // buffer informations
   HB_GT_CELLTYPE pBuffer[XVT_MAX_ROWS * XVT_MAX_COLS];
   HB_GT_CELLTYPE pAttributes[XVT_MAX_ROWS * XVT_MAX_COLS];
   HB_GT_CELLTYPE background;
   ULONG bufsize;
   BOOL bInvalid;
   XSegment rInvalid;
} XVT_BUFFER, *PXVT_BUFFER;

/************************ Window status logical structure *****************/

typedef struct tag_xvt_status
{
   // Mouse functions
   int mouseCol;
   int mouseRow;
   int mouseNumButtons;
   int mouseDblClick1TO;
   int mouseDblClick2TO;
   int lastMouseEvent;
   BOOL mouseButtons[XVT_MAX_BUTTONS];

   BOOL bUpdateDone;

} XVT_STATUS, *PXVT_STATUS;


/********************** Phisical screen window structure ******************/

typedef struct tag_x_wnddef
{
   Display *dpy;
   Window window;
   GC gc;
   Colormap colors;

   // functionc called when the window receives a message.
   void (*eventManager)( struct tag_x_wnddef* wnd, XEvent *evt );

   // size in pixels
   USHORT width;
   USHORT height;
   BOOL bResizing;

   // cursor:
   USHORT cursorHeight;
   SHORT cursRow;
   SHORT cursCol;

   // font informations
   XFontStruct *xfs;
   int fontHeight;
   int fontWidth;

   XVT_BUFFER *buffer;
   XVT_STATUS *status;

} XWND_DEF, *PXWND_DEF;


#endif
