/*
 * $Id: gtxvt.c,v 1.34 2004/05/08 18:10:53 fsgiudice Exp $
 */

/*
 * Xharbour Project source code:
 * X11 Virtual terminal
 * Copyright 2003 - Giancarlo Niccolai <antispam /at/ niccolai.ws>
 *
 * based on
 *   Video subsystem for Win32 using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemys?aw Czerpak <druzus@polbox.com>
 *   Video subsystem for Win32 compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemys?aw Czerpak <druzus@polbox.com>
 *
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

#include "gtxvt.h"
#include <X11/Xcms.h>
#include <sys/mman.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <unistd.h>


typedef struct tag_modifiers
{
   BOOL bCtrl;
   BOOL bAlt;
   BOOL bAltGr;
   BOOL bShift;
} MODIFIERS;


typedef struct ClipKeyCode {
    int key;
    int alt_key;
    int ctrl_key;
    int shift_key;
} ClipKeyCode;

static const ClipKeyCode stdKeyTab[CLIP_KEY_COUNT] = {
    {K_SPACE,              0,             0,         0}, /*  32 */
    {'!',                  0,             0,         0}, /*  33 */
    {'"',                  0,             0,         0}, /*  34 */
    {'#',                  0,             0,         0}, /*  35 */
    {'$',                  0,             0,         0}, /*  36 */
    {'%',                  0,             0,         0}, /*  37 */
    {'&',                  0,             0,         0}, /*  38 */
    {'\'',               296,             7,         0}, /*  39 */
    {'(',                  0,             0,         0}, /*  40 */
    {')',                  0,             0,         0}, /*  41 */
    {'*',                  0,             0,         0}, /*  42 */
    {'+',                  0,             0,         0}, /*  43 */
    {',',                307,             0,         0}, /*  44 */
    {'-',                386,            31,         0}, /*  45 */
    {'.',                308,             0,         0}, /*  46 */
    {'/',                309,           127,         0}, /*  47 */
    {'0',            K_ALT_0,             0,         0}, /*  48 */
    {'1',            K_ALT_1,             0,         0}, /*  49 */
    {'2',            K_ALT_2,           259,         0}, /*  50 */
    {'3',            K_ALT_3,            27,         0}, /*  51 */
    {'4',            K_ALT_4,            28,         0}, /*  52 */
    {'5',            K_ALT_5,            29,         0}, /*  53 */
    {'6',            K_ALT_6,            30,         0}, /*  54 */
    {'7',            K_ALT_7,            31,         0}, /*  55 */
    {'8',            K_ALT_8,           127,         0}, /*  56 */
    {'9',            K_ALT_9,             0,         0}, /*  57 */
    {':',                  0,             0,         0}, /*  58 */
    {';',                295,             0,         0}, /*  59 */
    {'<',                  0,             0,         0}, /*  60 */
    {'=',       K_ALT_EQUALS,             0,         0}, /*  61 */
    {'>',                  0,             0,         0}, /*  62 */
    {'?',                  0,             0,         0}, /*  63 */
    {'@',                  0,             0,         0}, /*  64 */
    {'A',            K_ALT_A,      K_CTRL_A,         0}, /*  65 */
    {'B',            K_ALT_B,      K_CTRL_B,         0}, /*  66 */
    {'C',            K_ALT_C,      K_CTRL_C,         0}, /*  67 */
    {'D',            K_ALT_D,      K_CTRL_D,         0}, /*  68 */
    {'E',            K_ALT_E,      K_CTRL_E,         0}, /*  69 */
    {'F',            K_ALT_F,      K_CTRL_F,         0}, /*  70 */
    {'G',            K_ALT_G,      K_CTRL_G,         0}, /*  71 */
    {'H',            K_ALT_H,      K_CTRL_H,         0}, /*  72 */
    {'I',            K_ALT_I,      K_CTRL_I,         0}, /*  73 */
    {'J',            K_ALT_J,      K_CTRL_J,         0}, /*  74 */
    {'K',            K_ALT_K,      K_CTRL_K,         0}, /*  75 */
    {'L',            K_ALT_L,      K_CTRL_L,         0}, /*  76 */
    {'M',            K_ALT_M,      K_CTRL_M,         0}, /*  77 */
    {'N',            K_ALT_N,      K_CTRL_N,         0}, /*  78 */
    {'O',            K_ALT_O,      K_CTRL_O,         0}, /*  79 */
    {'P',            K_ALT_P,      K_CTRL_P,         0}, /*  80 */
    {'Q',            K_ALT_Q,      K_CTRL_Q,         0}, /*  81 */
    {'R',            K_ALT_R,      K_CTRL_R,         0}, /*  82 */
    {'S',            K_ALT_S,      K_CTRL_S,         0}, /*  83 */
    {'T',            K_ALT_T,      K_CTRL_T,         0}, /*  84 */
    {'U',            K_ALT_U,      K_CTRL_U,         0}, /*  85 */
    {'V',            K_ALT_V,      K_CTRL_V,         0}, /*  86 */
    {'W',            K_ALT_W,      K_CTRL_W,         0}, /*  87 */
    {'X',            K_ALT_X,      K_CTRL_X,         0}, /*  88 */
    {'Y',            K_ALT_Y,      K_CTRL_Y,         0}, /*  89 */
    {'Z',            K_ALT_Z,      K_CTRL_Z,         0}, /*  90 */
    {'[',                282,            27,         0}, /*  91 */
    {'\\',               299,            28,         0}, /*  92 */
    {']',                283,            29,         0}, /*  93 */
    {'^',            K_ALT_6,            30,         0}, /*  94 */
    {'_',                386,            31,         0}, /*  95 */
    {'`',                297,           297,         0}, /*  96 */
    {'a',            K_ALT_A,      K_CTRL_A,         0}, /*  97 */
    {'b',            K_ALT_B,      K_CTRL_B,         0}, /*  98 */
    {'c',            K_ALT_C,      K_CTRL_C,         0}, /*  99 */
    {'d',            K_ALT_D,      K_CTRL_D,         0}, /* 100 */
    {'e',            K_ALT_E,      K_CTRL_E,         0}, /* 101 */
    {'f',            K_ALT_F,      K_CTRL_F,         0}, /* 102 */
    {'g',            K_ALT_G,      K_CTRL_G,         0}, /* 103 */
    {'h',            K_ALT_H,      K_CTRL_H,         0}, /* 104 */
    {'i',            K_ALT_I,      K_CTRL_I,         0}, /* 105 */
    {'j',            K_ALT_J,      K_CTRL_J,         0}, /* 106 */
    {'k',            K_ALT_K,      K_CTRL_K,         0}, /* 107 */
    {'l',            K_ALT_L,      K_CTRL_L,         0}, /* 108 */
    {'m',            K_ALT_M,      K_CTRL_M,         0}, /* 109 */
    {'n',            K_ALT_N,      K_CTRL_N,         0}, /* 110 */
    {'o',            K_ALT_O,      K_CTRL_O,         0}, /* 111 */
    {'p',            K_ALT_P,      K_CTRL_P,         0}, /* 112 */
    {'q',            K_ALT_Q,      K_CTRL_Q,         0}, /* 113 */
    {'r',            K_ALT_R,      K_CTRL_R,         0}, /* 114 */
    {'s',            K_ALT_S,      K_CTRL_S,         0}, /* 115 */
    {'t',            K_ALT_T,      K_CTRL_T,         0}, /* 116 */
    {'u',            K_ALT_U,      K_CTRL_U,         0}, /* 117 */
    {'v',            K_ALT_V,      K_CTRL_V,         0}, /* 118 */
    {'w',            K_ALT_W,      K_CTRL_W,         0}, /* 119 */
    {'x',            K_ALT_X,      K_CTRL_X,         0}, /* 120 */
    {'y',            K_ALT_Y,      K_CTRL_Y,         0}, /* 121 */
    {'z',            K_ALT_Z,      K_CTRL_Z,         0}, /* 122 */
    {'{',                282,            27,         0}, /* 123 */
    {'|',                299,            28,         0}, /* 124 */
    {'}',                283,            29,         0}, /* 125 */
    {'~',                297,           297,         0}, /* 126 */
    {K_CTRL_BS,     K_ALT_BS,           127,         0}, /* 127 */
    {K_F1,          K_ALT_F1,     K_CTRL_F1,   K_SH_F1}, /*  00 */
    {K_F2,          K_ALT_F2,     K_CTRL_F2,   K_SH_F2}, /*  01 */
    {K_F3,          K_ALT_F3,     K_CTRL_F3,   K_SH_F3}, /*  02 */
    {K_F4,          K_ALT_F4,     K_CTRL_F4,   K_SH_F4}, /*  03 */
    {K_F5,          K_ALT_F5,     K_CTRL_F5,   K_SH_F5}, /*  04 */
    {K_F6,          K_ALT_F6,     K_CTRL_F6,   K_SH_F6}, /*  05 */
    {K_F7,          K_ALT_F7,     K_CTRL_F7,   K_SH_F7}, /*  06 */
    {K_F8,          K_ALT_F8,     K_CTRL_F8,   K_SH_F8}, /*  07 */
    {K_F9,          K_ALT_F9,     K_CTRL_F9,   K_SH_F9}, /*  08 */
    {K_F10,        K_ALT_F10,    K_CTRL_F10,  K_SH_F10}, /*  09 */
    {K_F11,        K_ALT_F11,    K_CTRL_F11,  K_SH_F11}, /*  10 */
    {K_F12,        K_ALT_F12,    K_CTRL_F12,  K_SH_F12}, /*  11 */
    {K_UP,          K_ALT_UP,     K_CTRL_UP,   K_SH_UP}, /*  12 */
    {K_DOWN,      K_ALT_DOWN,   K_CTRL_DOWN, K_SH_DOWN}, /*  13 */
    {K_LEFT,      K_ALT_LEFT,   K_CTRL_LEFT, K_SH_LEFT}, /*  14 */
    {K_RIGHT,    K_ALT_RIGHT,  K_CTRL_RIGHT, K_SH_RIGHT}, /*  15 */
    {K_INS,        K_ALT_INS,    K_CTRL_INS,         0}, /*  16 */
    {K_DEL,        K_ALT_DEL,    K_CTRL_DEL,         0}, /*  17 */
    {K_HOME,      K_ALT_HOME,   K_CTRL_HOME,         0}, /*  18 */
    {K_END,        K_ALT_END,    K_CTRL_END,         0}, /*  19 */
    {K_PGUP,      K_ALT_PGUP,   K_CTRL_PGUP,         0}, /*  20 */
    {K_PGDN,      K_ALT_PGDN,   K_CTRL_PGDN,         0}, /*  21 */
    {K_BS,          K_ALT_BS,           127,         0}, /*  22 */
    {K_TAB,        K_ALT_TAB,    K_CTRL_TAB,  K_SH_TAB}, /*  23 */
    {K_ESC,        K_ALT_ESC,         K_ESC,         0}, /*  24 */
    {K_ENTER,    K_ALT_ENTER,  K_CTRL_ENTER,         0}  /*  25 */
};

static const UnixBoxChar boxTranslate[ XVT_BOX_CHARS ] ={
   { 176, HB_GTXVT_FILLER1},
   { 177, HB_GTXVT_FILLER2},
   { 178, HB_GTXVT_FILLER3},
   { 179, HB_GTXVT_SNG_VRT},
   { 180, HB_GTXVT_SNG_VR},
   { 181, HB_GTXVT_SNG_V_DBL_R},
   { 182, HB_GTXVT_DBL_V_SNG_R},
   { 183, HB_GTXVT_SNG_R_DBL_T},
   { 184, HB_GTXVT_DBL_R_SNG_T},
   { 185, HB_GTXVT_DBL_VR},
   { 186, HB_GTXVT_DBL_VRT},
   { 187, HB_GTXVT_DBL_RT},
   { 188, HB_GTXVT_DBL_RB},
   { 189, HB_GTXVT_SNG_R_DBL_B},
   { 190, HB_GTXVT_DBL_R_SNG_B},
   { 191, HB_GTXVT_SNG_RT},
   { 192, HB_GTXVT_SNG_LB},
   { 193, HB_GTXVT_SNG_BU},
   { 194, HB_GTXVT_SNG_TD},
   { 195, HB_GTXVT_SNG_VL},
   { 196, HB_GTXVT_SNG_HOR},
   { 197, HB_GTXVT_SNG_CRS},
   { 198, HB_GTXVT_SNG_V_DBL_L},
   { 199, HB_GTXVT_DBL_V_SNG_L},
   { 200, HB_GTXVT_DBL_LB},
   { 201, HB_GTXVT_DBL_LT},
   { 202, HB_GTXVT_DBL_BU},
   { 203, HB_GTXVT_DBL_TD},
   { 204, HB_GTXVT_DBL_VL},
   { 205, HB_GTXVT_DBL_HOR},
   { 206, HB_GTXVT_DBL_CRS},
   { 207, HB_GTXVT_DBL_B_SNG_U},
   { 208, HB_GTXVT_SNG_B_DBL_U},
   { 209, HB_GTXVT_DBL_T_SNG_D},
   { 210, HB_GTXVT_SNG_T_DBL_D},
   { 211, HB_GTXVT_DBL_L_SNG_B},
   { 212, HB_GTXVT_SNG_L_DBL_B},
   { 213, HB_GTXVT_SNG_L_DBL_T},
   { 214, HB_GTXVT_DBL_L_SNG_T},
   { 215, HB_GTXVT_DBL_SNG_CRS},
   { 216, HB_GTXVT_SNG_DBL_CRS},
   { 217, HB_GTXVT_SNG_RB},
   { 218, HB_GTXVT_SNG_LT},
   { 219, HB_GTXVT_FULL},
   { 220, HB_GTXVT_FULL_B},
   { 221, HB_GTXVT_FULL_R},
   { 222, HB_GTXVT_FULL_L},
   { 223, HB_GTXVT_FULL_T},
   { 251, HB_GTXVT_CHECK }
};


/******************************************************************/

static void xvt_InitStatics( Display *dpy );
static void xvt_InitDisplay( PXVT_BUFFER buf, PXVT_STATUS stat );
static PXVT_BUFFER xvt_bufferNew( USHORT col, USHORT row, USHORT bkg );
static BOOL xvt_bufferResize( PXVT_BUFFER buf,  USHORT cols, USHORT rows );
static void xvt_bufferInvalidate( PXVT_BUFFER buf,int left, int top, int right, int bottom );
//static void xvt_bufferClear( PXVT_BUFFER buf, HB_GT_CELLTYPE chr, HB_GT_CELLTYPE bkg );
static void xvt_bufferClearRange(
   PXVT_BUFFER buf, HB_GT_CELLTYPE chr, HB_GT_CELLTYPE bkg,
   int x1, int y1, int x2, int y2 );
static void xvt_bufferQueueKey( PXVT_BUFFER buf, int data );
static BOOL xvt_bufferDeqeueKey( PXVT_BUFFER buf, int *c );
static void xvt_bufferWriteBytes( PXVT_BUFFER buf,
   USHORT col, USHORT row,
   BYTE attr,
   BYTE *sBuffer, USHORT length);

static PXWND_DEF xvt_windowCreate( Display *dpy, PXVT_BUFFER buf, PXVT_STATUS status );
static void xvt_windowResize( PXWND_DEF wnd );
static void xvt_windowSetCursor( PXWND_DEF wnd );
static void xvt_windowSetHints( PXWND_DEF wnd );
static XFontStruct *xvt_fontNew( Display *dpy, char *fontFace, char *weight, int size,  char *encoding );
static void xvt_windowSetFont( PXWND_DEF wnd, XFontStruct * xfs );
void xvt_windowUpdate( PXWND_DEF wnd, XSegment *rUpdate );
static void xvt_windowRepaintColRow( PXWND_DEF wnd,
   int colStart, int rowStart, int colStop, int rowStop );

static void xvt_windowRepaintGraphical( PXWND_DEF wnd,
   int colStart, int rowStart, int colStop, int rowStop );

static BOOL xvt_windowDrawText( PXWND_DEF wnd,  USHORT col, USHORT row, char * str, USHORT cbString );
static void xvt_windowDrawBox( PXWND_DEF wnd, int col, int row, int boxchar );
static void xvt_windowSetColors( PXWND_DEF wnd, BYTE attr );

static void xvt_eventKeyProcess( PXVT_BUFFER buffer, XKeyEvent *evt);
static void xvt_eventManage( PXWND_DEF wnd, XEvent *evt );
static void xvt_processMessages( PXWND_DEF wnd );
static void xvt_appProcess( USHORT usWaitFor );

static PXVT_STATUS xvt_statusNew( void );
static int xvt_statusQueryMouseBtns( PXVT_STATUS status, Display *dpy );

static HB_GT_CELLTYPE xvt_charTranslate( unsigned char ch );
static BYTE xvt_charUntranslate( HB_GT_CELLTYPE ch );
static int xvt_keyTranslate( int key );
static void xvt_putTextInternal (
      USHORT top, USHORT left, USHORT bottom, USHORT right,
      USHORT width,
      BYTE * sBuffer );
void xvt_cursorPaint( PXWND_DEF wnd );


static char *color_refs[] = {
   "rgb:00/00/00",   /* black         */
   "rgb:00/00/AA",   /* blue          */
   "rgb:00/AA/00",   /* green         */
   "rgb:00/AA/AA",   /* cyan          */
   "rgb:AA/00/00",   /* red           */
   "rgb:AA/00/AA",   /* magenta       */
   "rgb:AA/55/00",   /* brown         */
   "rgb:AA/AA/AA",   /* light gray    */
   "rgb:55/55/55",   /* gray          */
   "rgb:55/55/FF",   /* light blue    */
   "rgb:55/FF/55",   /* light green   */
   "rgb:55/FF/FF",   /* light cyan    */
   "rgb:FF/55/55",   /* light red     */
   "rgb:FF/55/FF",   /* light magenta */
   "rgb:FF/FF/55",   /* yellow        */
   "rgb:FF/FF/FF"    /* white         */
};

static XColor s_xcolor[16];

/************************ globals ********************************/
static PXVT_BUFFER s_buffer = 0;
static PXVT_STATUS s_status = 0;
static BOOL s_gtxvt_initialized = FALSE;
static int s_childPid = 0;

static USHORT  s_uiDispCount;

static int s_iStdIn, s_iStdOut, s_iStdErr;

int s_cursorState = 0;
MODIFIERS s_modifiers;

// Pipe stream for update queue (app to wnd )
static int streamUpdate[2];
// Pipe stream for message queue (wnd to app )
static int streamFeedback[2];
// Pipe stream for input queue (wnd to app)
static int streamChr[2];

// cached value of DELETE WINDOW X atom
Atom s_atom_delwin;
Atom s_atom_clientprot;
XPoint s_FillerPts[2][16*16];
int s_countPoints[2];

// we try to connect in initialization, so we can immediately
// report a critical error
Display *s_Xdisplay;

static HB_GT_GOBJECT *s_gLastObj = NULL;

PXWND_DEF s_wnd = NULL;

/*** Current clipboard buffer **/
static char *s_clipboard = NULL;
static ULONG s_clipsize = 0;

/** Font request cache **/
static int s_fontReqSize = XVT_DEFAULT_FONT_HEIGHT;
static int s_fontReqWidth = XVT_DEFAULT_FONT_WIDTH;

/**********************************************************************
*                                                                     *
* PART 1: XVT INTERNAL API FUNCTIONS                                  *
*                                                                     *
**********************************************************************/


/************************************************************/

static int s_errorHandler( Display *dpy, XErrorEvent *e )
{
    char errorText[1024];
    sprintf( errorText, "%s", "Xlib error: " );

    XGetErrorText( dpy, e->error_code,
         errorText + strlen( errorText ),
         sizeof(errorText) - strlen( errorText ) );

   hb_errInternal( EG_CREATE, errorText, NULL, NULL );

    return 1;
}

static void xvt_InitStatics( Display *dpy )
{

   XSetErrorHandler( s_errorHandler );

   s_modifiers.bCtrl  = FALSE;
   s_modifiers.bAlt   = FALSE;
   s_modifiers.bAltGr = FALSE;
   s_modifiers.bShift = FALSE;

   s_atom_delwin = XInternAtom( dpy, "WM_DELETE_WINDOW", 1);

}

/*** Prepare the default window ***/
static void xvt_InitDisplay( PXVT_BUFFER buf, PXVT_STATUS status )
{
   int icol, irow, icount, istart=1;
   Display *dpy;
   PXWND_DEF wnd;
   XColor dummy;

   // Pipe stream for update queue (app to wnd )
   pipe( streamUpdate );
   pipe( streamFeedback );
   // Pipe stream for input queue (wnd to app)
   pipe( streamChr );

   if ( hb_set.HB_SET_GTMODE == 1 )
   {
      s_childPid = fork();
      if ( s_childPid != 0 )
      {
         return;
      }
   }

   // With NULL, it gets the DISPLAY environment variable.
   dpy = XOpenDisplay( NULL );
   if ( dpy == NULL )
   {
      hb_errRT_TERM( EG_CREATE, 10001, NULL, "Can't connect to X server", 0, 0 );
      return;
   }

   xvt_InitStatics( dpy );

   xvt_statusQueryMouseBtns( status, dpy );
   wnd = xvt_windowCreate( dpy, buf, status );

   // ok, now we can inform the X manager about our new status:
   xvt_windowSetHints( wnd );

   XMapWindow( wnd->dpy, wnd->window );

   // create commonly used color items
   for (irow = 0; irow < 16; irow ++ )
   {
      XAllocNamedColor( wnd->dpy, wnd->colors,
              color_refs[irow], &dummy, &s_xcolor[ irow ] );
   }

   // creates the sparse points
   // sparse points

   icount = 0;
   istart = 0;
   for ( irow = 0; irow < wnd->fontHeight; irow += 3)
   {
      if ( icount > 0 )
      {
         s_FillerPts[0][icount].x = -istart ;
         istart = 0;
         s_FillerPts[0][icount].y = 3;
      }

      icount++;

      for ( icol = istart ; icol < wnd->fontWidth; icol += 3 )
      {
         s_FillerPts[0][icount].x = 3;
         istart+=3;
         s_FillerPts[0][icount].y = 0;
         icount++;
      }
   }
   s_countPoints[0] = icount;

   icount = 0;
   istart = 0;
   for ( irow = 0; irow < wnd->fontHeight; irow += 2 )
   {
      if ( icount > 0 )
      {
         s_FillerPts[1][icount].x = -istart ;
         istart = 0;
         s_FillerPts[1][icount].y = 2;
      }

      icount++;

      for ( icol = istart ; icol < wnd->fontWidth; icol += 2 )
      {
         s_FillerPts[1][icount].x = 2;
         istart+=2;
         s_FillerPts[1][icount].y = 0;
         icount++;
      }
   }
   s_countPoints[1] = icount;

   XGrabKey( wnd->dpy, AnyKey, ControlMask | Mod1Mask , wnd->window,
      False,
      GrabModeAsync, GrabModeAsync );

   if ( hb_set.HB_SET_GTMODE == 1 )
   {
      //we'r done. Now the ball passes to the window managing function
      xvt_processMessages( wnd );

      // exiting
      if ( wnd->xfs ) {
         XFreeFont( wnd->dpy, wnd->xfs );
      }
      XCloseDisplay( wnd->dpy );
      hb_xfree( wnd );
      exit( 0 );
   }
   else
   {
      s_wnd = wnd;
   }
}

/**********************************************************************
* XVT Buffer oriented operations                                      *
**********************************************************************/

/******************** Build XVT buffer *********************************/
static PXVT_BUFFER xvt_bufferNew( USHORT col, USHORT row, USHORT bkg )
{
   PXVT_BUFFER buf;

   if ( row <= XVT_MAX_ROWS && col <= XVT_MAX_COLS )
   {

      buf = (PXVT_BUFFER) mmap( 0, sizeof( XVT_BUFFER ), PROT_READ | PROT_WRITE,
         MAP_SHARED |MAP_ANON, -1, 0  );

      buf->col = 0;
      buf->row = 0;
      buf->cols = col;
      buf->rows = row;
      buf->background = bkg;
      buf->curs_style = SC_NORMAL;

      buf->bufsize = col * row * HB_GT_CELLSIZE;
      buf->bInvalid = FALSE;
      COMMIT_BUFFER( buf );

      return buf;
   }
   else
   {
      return NULL;
   }
}

/******************** Resizes an existing buffer ****************************/

static BOOL xvt_bufferResize( PXVT_BUFFER buf,  USHORT cols, USHORT rows )
{
   int oldcols, oldrows;

   if ( rows <= XVT_MAX_ROWS && cols <= XVT_MAX_COLS )
   {
      oldcols = buf->cols;
      oldrows = buf->rows;

      buf->bufsize = cols * rows * HB_GT_CELLSIZE;
      buf->cols = cols;
      buf->rows = rows;

      if ( cols > oldcols )
      {
         xvt_bufferClearRange( buf, 0x0020, buf->background,
            oldcols, 0, cols, rows > oldrows ? oldrows : rows );
      }

      if ( rows > oldrows )
      {
         xvt_bufferClearRange( buf, 0x0020, buf->background,
            0, oldrows, cols > oldcols ? cols : oldcols, rows );
      }

      if ( buf->col >= cols )
      {
         buf->col = cols -1;
      }

      if ( buf->row >= rows )
      {
         buf->row = rows -1;
      }
      COMMIT_BUFFER( buf );

      return TRUE;
   }
   else
   {
      return FALSE;
   }

}

/******************** Marks a buffer area as to be redrawn *********************/
static void xvt_bufferInvalidate( PXVT_BUFFER buf,
   int left, int top, int right, int bottom )
{
   /* process but don't wait */
   xvt_appProcess( 0 );

   if ( buf->bInvalid == FALSE ) {
      buf->bInvalid = TRUE;
      buf->rInvalid.y1 = top;
      buf->rInvalid.x1 = left;
      buf->rInvalid.y2 = bottom;
      buf->rInvalid.x2 = right;
      // wake up our friend.
   }
   else {
      if ( buf->rInvalid.x1 > left ) buf->rInvalid.x1 = left;
      if ( buf->rInvalid.y1 > top ) buf->rInvalid.y1 = top;
      if ( buf->rInvalid.x2 < right ) buf->rInvalid.x2 = right;
      if ( buf->rInvalid.y2 < bottom ) buf->rInvalid.y2 = bottom;
   }

   if ( s_uiDispCount == 0 && (s_childPid > 0 || s_wnd != NULL) )
   {
      USHORT appMsg;

      if ( s_wnd == NULL )
      {
         COMMIT_BUFFER( buf );
      }
      // new objects added?
      if ( hb_gt_gobjects == NULL && s_gLastObj != NULL )
      {
         appMsg = XVT_ICM_CLEAROBJECTS;
         write( streamUpdate[1], &appMsg, sizeof( appMsg ) );
         s_gLastObj = NULL;
      }
      else {
         HB_GT_GOBJECT *pObj = hb_gt_gobjects;
         appMsg = XVT_ICM_ADDOBJECT;
         while ( pObj != s_gLastObj )
         {
            write( streamUpdate[1], &appMsg, sizeof( appMsg ) );
            write( streamUpdate[1], pObj, sizeof( HB_GT_GOBJECT ) );
            if ( pObj->type == GTO_TEXT )
            {
               write( streamUpdate[1], pObj->data, pObj->data_len );
            }

            pObj = pObj->next;
         }
         s_gLastObj = hb_gt_gobjects;
      }

      appMsg = XVT_ICM_UPDATE;
      write( streamUpdate[1], &appMsg, sizeof( appMsg ) );
      write( streamUpdate[1], &buf->rInvalid, sizeof( XSegment ) );

      /* Single process? --> update! */
      if ( s_wnd != NULL )
      {
         xvt_processMessages( s_wnd );
      }

      buf->bInvalid = FALSE;
   }


}

/******************** Clears the whole buffer *******************************/
/*
static void xvt_bufferClear( PXVT_BUFFER buf, HB_GT_CELLTYPE chr, HB_GT_CELLTYPE bkg )
{
   int i;
   HB_GT_CELLTYPE *pBuffer = buf->pBuffer;
   HB_GT_CELLTYPE *pAttributes = buf->pAttributes;

   chr = XVT_SWAP_ENDIAN( chr );

   buf->background = bkg;

   for ( i = 0; i < buf->cols * buf->rows; i++ )
   {
      *pBuffer = chr;
      *pAttributes = bkg;
      pBuffer++;
      pAttributes++;
   }

   buf->bInvalid = TRUE;
   buf->rInvalid.x1 = buf->rInvalid.y1 = 0;
   buf->rInvalid.x2 = buf->cols;
   buf->rInvalid.x2 = buf->rows;
}*/

/******************** Clears a part of the buffer *******************************/

static void xvt_bufferClearRange(
   PXVT_BUFFER buf, HB_GT_CELLTYPE chr, HB_GT_CELLTYPE bkg,
   int x1, int y1, int x2, int y2 )
{
   int i,j;
   HB_GT_CELLTYPE *pBuffer;
   HB_GT_CELLTYPE *pAttributes;

   chr = XVT_SWAP_ENDIAN( chr );

   for ( i = y1; i <= y2; i++ )
   {
      pBuffer = buf->pBuffer + i * buf->cols + x1;
      pAttributes = buf->pAttributes + i * buf->cols + x1;

      for ( j = x1; j <= x2; j++ )
      {
         *pBuffer= chr;
         *pAttributes = bkg;
         pBuffer++;
         pAttributes++;
      }
   }

   xvt_bufferInvalidate(buf, x1, y1, x2, y2 );
}

/******************** Add a keystore to the input queue *******************************/

static void xvt_bufferQueueKey( PXVT_BUFFER buf, int data )
{
   USHORT appMsg = XVT_ICM_KEYSTORE;

   HB_SYMBOL_UNUSED( buf );

   //TODO: use select to decide wether write is possible
   write( streamChr[1], &appMsg, sizeof( appMsg ) );
   write( streamChr[1], &data, sizeof( data ) );
}

/******************** Gets a keystore from the input queue ****************************/

static BOOL xvt_bufferDeqeueKey( PXVT_BUFFER buf, int *c )
{
   BOOL bRet = FALSE;
   fd_set keySet;
   USHORT appMsg;
   struct timeval timeout = {0,0};

   HB_SYMBOL_UNUSED( buf );

   *c = 0;

   FD_ZERO(&keySet);
   FD_SET(streamChr[0], &keySet );

   // See if there is data available
   if ( select( streamChr[0] + 1, &keySet, NULL , NULL, &timeout) )
   {

      if ( read( streamChr[0], &appMsg, sizeof( appMsg ) ) > 0 )
      {
         switch( appMsg )
         {
            case XVT_ICM_KEYSTORE:
               read( streamChr[0], c, sizeof( *c ) );
               bRet = TRUE;
            break;

            case XVT_ICM_QUIT:
               hb_gtHandleClose();
               break;

            default:
               bRet = FALSE;
               // Signal error?
         }
      }
   }

   return bRet;
}

/******************** Writes a string into the buffer ****************************/

static void xvt_bufferWriteBytes( PXVT_BUFFER buf,
   USHORT col, USHORT row,
   BYTE attr,
   BYTE *sBuffer, USHORT length)
{
   USHORT index;
   HB_GT_CELLTYPE *pBuffer;
   HB_GT_CELLTYPE *pAttributes;


   // determine the index and put the string into the TextBuffer
   index = HB_GT_INDEXOF(buf, col, row);
   if ( (ULONG) (length + index) <= buf->bufsize)
   {
      for ( pAttributes = buf->pAttributes + index;
         pAttributes < buf->pAttributes + index + length;
         pAttributes++)
      {
         *pAttributes = attr;
      }

      for ( pBuffer = buf->pBuffer + index ;
         pBuffer < buf->pBuffer + index + length;
         pBuffer ++, sBuffer++ )
      {
         *pBuffer = xvt_charTranslate( *sBuffer );
      }

      //determine bounds of rect around character to refresh
      xvt_bufferInvalidate( buf, col, row, col + length, row  );
   }
}


/**********************************************************************
* XVT terminal Window oriented operations                             *
**********************************************************************/

/************************ Create a default window *************************/

static PXWND_DEF xvt_windowCreate( Display *dpy, PXVT_BUFFER buf, PXVT_STATUS status )
{
   PXWND_DEF wnd;
   int whiteColor;
   XFontStruct *xfs;
   XSetWindowAttributes xAttr;
   Window wndRoot;
   XWindowAttributes rootAttr;


   // load the standard font
   xfs = xvt_fontNew( dpy, XVT_DEFAULT_FONT_NAME, XVT_DEFAULT_FONT_WEIGHT,
                  XVT_DEFAULT_FONT_HEIGHT, NULL );
   if ( xfs == NULL )
   {
      hb_errInternal( EG_CREATE, "Can't load '%s' defualt font", XVT_DEFAULT_FONT_NAME, NULL );
      return NULL;
   }

   wnd = ( PXWND_DEF ) hb_xgrab( sizeof( XWND_DEF ) );
   wnd->dpy = dpy;
   wnd->bResizing = FALSE;
   wnd->cursRow = -1;
   wnd->cursCol = -1;
   wnd->buffer = buf;
   wnd->status = status;
   wnd->gc = NULL;
   wnd->usFlags = 0;
   xvt_windowSetFont( wnd, xfs );

   /* Create the phisical window */
   whiteColor = WhitePixel(dpy, DefaultScreen(dpy));
   /* Gets root window geometry */
   wndRoot = XDefaultRootWindow( dpy );
   XGetWindowAttributes( dpy, wndRoot, &rootAttr );

   wnd->window = XCreateSimpleWindow(dpy,
      wndRoot,
      (rootAttr.width - wnd->width)/2, (rootAttr.height - wnd->height)/2,
      wnd->width, wnd->height,
      0, whiteColor, whiteColor);

   xAttr.win_gravity = CenterGravity;
   XChangeWindowAttributes( dpy, wnd->window, CWWinGravity, &xAttr );

   /* Setfont requires a wnd->gc and wnd->buffer to be in place */
   wnd->gc = XCreateGC( dpy, wnd->window, 0, NULL );
   wnd->buffer = buf;
   XSetFont( wnd->dpy, wnd->gc, wnd->xfs->fid );

   /* Initial cursor height == whole cell height */
   xvt_windowSetCursor( wnd );

   // Line width 2
   XSetLineAttributes( dpy, wnd->gc, 1, LineSolid, CapRound, JoinBevel );
   wnd->colors = DefaultColormap( dpy, DefaultScreen( dpy ));
   XSelectInput( dpy, wnd->window, XVT_STD_MASK);

   // Sets the event manager fot this window
   wnd->eventManager = xvt_eventManage;

   return wnd;
}

static void xvt_windowResize( PXWND_DEF wnd )
{
   wnd->width = wnd->buffer->cols * wnd->fontWidth;
   wnd->height = wnd->buffer->rows * wnd->fontHeight;
   XResizeWindow( wnd->dpy, wnd->window,
      wnd->width,
      wnd->height );
}

/*********** Explain the Window Manager the way we want to be treated *********/
static void xvt_windowSetHints( PXWND_DEF wnd )
{
   XSizeHints xsize;
   XClassHint xclass;
   PHB_FNAME pFileName = 0;
   char **argv;
   int argc;
   int i;
   Window wndRoot;
   XWindowAttributes rootAttr;


   wndRoot = XDefaultRootWindow( wnd->dpy );
   XGetWindowAttributes( wnd->dpy, wndRoot, &rootAttr );

   //xsize.flags = PWinGravity | PBaseSize | PResizeInc | PMinSize;
   xsize.flags = USPosition| PWinGravity | PResizeInc | PMinSize;
   xsize.x = (rootAttr.width - wnd->width)/2;
   xsize.y = (rootAttr.height - wnd->height)/2;
   xsize.win_gravity = CenterGravity;
   xsize.width_inc = wnd->fontWidth;
   xsize.height_inc = wnd->fontHeight;
   xsize.min_width = wnd->fontWidth*6;
   xsize.min_height = wnd->fontHeight*3;
   xsize.base_width = wnd->width;
   xsize.base_height = wnd->height;

   XSetWMNormalHints( wnd->dpy, wnd->window, &xsize);

   // Signal that we are interested to know we are being destroyed
   XSetWMProtocols(wnd->dpy, wnd->window, &s_atom_delwin, 1);

   // sets application name.
   // Application name is NOT window title; app name is the name that is
   // used to retreive X database resources.
   // on posix systems, check 1) for -name parameter, 2) for RESOURCE_NAME envvar,
   // 3) for commandline program name.
   argv = hb_cmdargARGV();
   argc =hb_cmdargARGC();
   xclass.res_name = 0;
   for (i = 1; i < argc; i++ )
   {
      if ( strcmp( argv[i], "-name" ) == 0 && argc > i + 1 )
      {
         xclass.res_name = argv[i+1];
         break;
      }
   }
   // found?
   if ( xclass.res_name == 0 )
   {
      xclass.res_name = getenv( "RESOURCE_NAME" );
   }
   // found?
   if ( xclass.res_name == 0)
   {
      pFileName = hb_fsFNameSplit( argv[0] );
      xclass.res_name = pFileName->szName;
   }

   xclass.res_class = XVT_CLASS_NAME;
   XSetClassHint( wnd->dpy, wnd->window, &xclass );

   // default window title is the same as application name
   XStoreName( wnd->dpy, wnd->window, xclass.res_name );
   XSetIconName( wnd->dpy, wnd->window, xclass.res_name );

   //TODO: verify if name must stay until the end of the program or
   //      it can be deleted now.
   if ( pFileName )
   {
      hb_xfree( pFileName );
   }
}

/*********** Update cursor height based on buffer cursor style **********/
static void xvt_windowSetCursor( PXWND_DEF wnd )
{
   switch( wnd->buffer->curs_style )
   {
      case SC_NONE:
         wnd->cursorHeight = 0;
         break ;
      case SC_INSERT:
         wnd->cursorHeight = wnd->fontHeight/4;
         break;
      case SC_SPECIAL1:
         wnd->cursorHeight = wnd->fontHeight/2;
         break;
      case SC_SPECIAL2:
         wnd->cursorHeight = -wnd->fontHeight/2;
         break;
      case SC_NORMAL:
      default:
         wnd->cursorHeight = wnd->fontHeight;
         break;
   }
}

/*************** Loads a font into memory ********************/
static XFontStruct * xvt_fontNew( Display *dpy, char *fontFace, char *weight, int size,  char *encoding )
{
   char fontString[150];
   XFontStruct *xfs;

   snprintf( fontString, 149, "-*-%s-%s-r-normal--%d-*-*-*-*-*-%s",
      fontFace, weight, size, encoding == NULL ? "*-*" : encoding);

   xfs = XLoadQueryFont( dpy, fontString );

   return xfs;
}


/*************** Sets a font into an existing window **************************/
static void xvt_windowSetFont( PXWND_DEF wnd, XFontStruct * xfs )
{
   wnd->xfs = xfs;
   wnd->fontHeight = xfs->max_bounds.ascent + xfs->max_bounds.descent;
   wnd->fontWidth = xfs->max_bounds.rbearing - xfs->min_bounds.lbearing;
   wnd->width = wnd->buffer->cols * wnd->fontWidth;
   wnd->height = wnd->buffer->rows * wnd->fontHeight;

   if ( wnd->gc != NULL )
   {
      XSetFont( wnd->dpy, wnd->gc, wnd->xfs->fid );
   }
}


/******************** Repaint the window if necessary **********************/
void xvt_windowUpdate( PXWND_DEF wnd, XSegment *rUpdate )
{
   //USHORT appMsg;

   xvt_windowRepaintColRow( wnd,
      rUpdate->x1, rUpdate->y1,
      rUpdate->x2, rUpdate->y2);
   XFlush( wnd->dpy );

   /* Signal the buffer filler that we have done with it */
   //appMsg = XVT_ICM_UPDATE;
   //write( streamFeedback[1], &appMsg, sizeof( appMsg ) );
}

/******** Draws the GT graphical objects **********/

static void xvt_windowRepaintGraphical( PXWND_DEF wnd, int x1, int y1, int x2, int y2 )
{
   HB_GT_GOBJECT *pObj;
   XRectangle cliprect;
   XColor color;

   pObj = hb_gt_gobjects;

   /* Set clipping region for X */
   cliprect.x = x1;
   cliprect.y = y1;
   cliprect.width = x2 - x1+1;
   cliprect.height = y2 - y1+1;

   XSetClipRectangles( wnd->dpy, wnd->gc, 0, 0, &cliprect, 1, YXBanded );

   while ( pObj )
   {
      /* Check if pObj boundaries are inside the area to be updated */
      if ( hb_gtGobjectInside( pObj, x1, y1, x2, y2 ) )
      {
         color.red = pObj->color.usRed;
         color.green = pObj->color.usGreen;
         color.blue = pObj->color.usBlue;
         color.flags = DoRed | DoGreen | DoBlue;
         /* Ignore alpha for now */
         XAllocColor( wnd->dpy, wnd->colors, &color );
         XSetForeground( wnd->dpy, wnd->gc, color.pixel );
         XFreeColors( wnd->dpy, wnd->colors, &(color.pixel), 1, 0 );

         switch( pObj->type )
         {
            case GTO_POINT:
               XDrawPoint( wnd->dpy, wnd->window, wnd->gc, pObj->x, pObj->y );
            break;

            case GTO_LINE:
               /* For lines, width and height represent X2, Y2 */
               XDrawLine( wnd->dpy, wnd->window, wnd->gc,
                  pObj->x, pObj->y,
                  pObj->width, pObj->height );
            break;

            case GTO_SQUARE:
               /* For lines, width and height represent X2, Y2 */
               XDrawRectangle( wnd->dpy, wnd->window, wnd->gc,
                  pObj->x, pObj->y,
                  pObj->width, pObj->height );
            break;

            case GTO_RECTANGLE:
               /* For lines, width and height represent X2, Y2 */
               XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
                  pObj->x, pObj->y,
                  pObj->width, pObj->height );
            break;


            case GTO_CIRCLE:
               XDrawArc( wnd->dpy, wnd->window, wnd->gc,
                  pObj->x, pObj->y,
                  pObj->width, pObj->height,
                  0, 360*64);
            break;

            case GTO_DISK:
               XFillArc( wnd->dpy, wnd->window, wnd->gc,
                  pObj->x, pObj->y,
                  pObj->width, pObj->height,
                  0, 360*64);
            break;

            case GTO_TEXT:
               XDrawString( wnd->dpy, wnd->window, wnd->gc,
                  pObj->x, pObj->y,
                  pObj->data, pObj->data_len );
            break;
         }
      }
      pObj = pObj->next;
   }

   XSetClipMask(wnd->dpy, wnd->gc, None);
}

/******** Draw a part (or all) of the window based on buffer content **********/
static void xvt_windowRepaintColRow( PXWND_DEF wnd,
   int colStart, int rowStart, int colStop, int rowStop )
{
   int irow;
   USHORT icol, index, startIndex, startCol, len;
   BYTE oldAttrib, attrib;
   PXVT_BUFFER buf = wnd->buffer;

   if ( rowStop >= buf->rows )
   {
      rowStop = buf->rows-1;
   }

   if ( colStop >= buf->cols )
   {
      colStop = buf->cols-1;
   }

   if ( colStart < 0 )
   {
      colStart = 0;
   }

   if ( rowStart < 0 )
   {
      rowStart = 0;
   }

   for ( irow = rowStart; irow <= rowStop; irow++ )
   {
      icol = colStart;
      index = icol +  irow * buf->cols;
      startIndex = index;
      startCol = icol;
      len = 0;
      oldAttrib = buf->pAttributes[index];
      /* attribute may change mid line...
      * so buffer up text with same attrib, and output it
      * then do next section with same attrib, etc
      */
      while (icol <= colStop)
      {
         if (index >= buf->bufsize )
         {
            break;
         }
         attrib = buf->pAttributes[index];
         if (attrib != oldAttrib)
         {
            xvt_windowSetColors(wnd, oldAttrib);
            xvt_windowDrawText( wnd, startCol, irow, (char *) (buf->pBuffer+startIndex), len );
            oldAttrib = attrib;
            startIndex = index;
            startCol = icol;
            len = 0;

         }
         icol++;
         len++;
         index++;
      }
      xvt_windowSetColors(wnd, oldAttrib);
      xvt_windowDrawText(wnd, startCol, irow, (char *) (buf->pBuffer+startIndex), len );
   }

   // must the cursor be repainted?
   if ( wnd->cursorHeight > 0 &&
      buf->col >= colStart && buf->col <= colStop &&
      buf->row >= rowStart && buf->row <= rowStop )
   {
      index = buf->col +  buf->row * buf->cols;
      if ( s_cursorState ) // currently on
      {
         oldAttrib = buf->pAttributes[ index ];
         if ( (oldAttrib & 0x70) != 0x70) {
            attrib = 0x70 | (oldAttrib ^ 0x0f);
         }
         else {
            attrib = (oldAttrib & 0x0f) | 0x08;
         }

         xvt_windowSetColors( wnd, attrib);
         xvt_windowDrawText( wnd, buf->col, buf->row,
               (char *) (buf->pBuffer+index), 1 );
      }
      else
      {
         xvt_windowSetColors( wnd, buf->pAttributes[ index ]);
         xvt_windowDrawText( wnd, buf->col, buf->row,
               (char *) (buf->pBuffer+index), 1 );
      }
   }

   // do the graphical updates
   if ( hb_gt_gobjects != NULL )
   {
      xvt_windowRepaintGraphical( wnd,
         colStart * wnd->fontWidth, rowStart * wnd->fontHeight,
         (colStop+1) * wnd->fontWidth, (rowStop+1) * wnd->fontHeight );
   }

}

/********************** Draw text into a window **************************/
static BOOL xvt_windowDrawText( PXWND_DEF wnd,  USHORT col, USHORT row, char * str, USHORT len )
{
   int pos,lastpos;
   USHORT *usString;
   HB_GT_CELLTYPE cell;

   if (len > wnd->buffer->cols) // make sure string is not too long
   {
      len = wnd->buffer->cols;
   }

   /* Draw eventual graphical chars */
   lastpos = 0;
   usString = (USHORT *) str;

   for ( pos = 0; pos < len; pos ++ )
   {
      #ifdef HB_BIG_ENDIAN
      cell = usString[pos];
      #else
      cell = 0xFFFF & ((usString[pos] << 8) | (usString[pos]>>8));
      #endif

      if ( cell >= HB_GTXVT_DBL_LT )
      {
         // draw the string up to this position
         if ( pos > lastpos )
         {
            XDrawImageString16( wnd->dpy, wnd->window, wnd->gc,
               (col+lastpos) * wnd->fontWidth, row * wnd->fontHeight+wnd->xfs->ascent,
               (XChar2b *) (usString+lastpos), pos+1-lastpos );
         }
         // draw the box
         xvt_windowDrawBox( wnd, col + pos, row, cell );
         lastpos = pos+1;
      }
   }
   // last drawn
   if ( len > lastpos )
   {
      XDrawImageString16( wnd->dpy, wnd->window, wnd->gc,
         (col+lastpos) * wnd->fontWidth, row * wnd->fontHeight+wnd->xfs->ascent,
         (XChar2b *) (usString+lastpos), len-lastpos );
   }
   return TRUE;
}

/*************** Draws the extended virtual box characters ********************/
static void xvt_windowDrawBox( PXWND_DEF wnd, int col, int row, int boxchar )
{
   XSegment segs[9];
   int nsegs = 0;
   char space = ' ';
   int cellx = wnd->fontWidth;
   int celly = wnd->fontHeight;
   int basex = col * cellx;
   int basey = row * celly;

   /* Drawing a filler ? */
   if ( boxchar >= HB_GTXVT_FILLER1 )
   {
      int attr = wnd->buffer->pAttributes[ HB_GT_INDEXOF( wnd->buffer, col, row )  ];
      int fore = attr & 0x000F;
      int back = (attr & 0x00F0)>>4;
      XPoint *pts =  boxchar == HB_GTXVT_FILLER2 ? s_FillerPts[1] : s_FillerPts[0];
      int icount = s_countPoints[ boxchar == HB_GTXVT_FILLER2 ? 1 : 0 ];
      pts[0].x = basex+1;
      pts[0].y = basey+1;

      if ( boxchar != HB_GTXVT_FILLER3 )
      {
         XSetForeground( wnd->dpy, wnd->gc, s_xcolor[back].pixel );

         // erase background!
         XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
            basex, basey, cellx, celly );

         XSetForeground( wnd->dpy, wnd->gc, s_xcolor[fore].pixel );

         // draw in foreground!
         XDrawPoints( wnd->dpy, wnd->window, wnd->gc,
              pts, icount, CoordModePrevious );
      }
      else
      {

         // erase Foreground!
         XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
            basex, basey, cellx, celly );

         XSetForeground( wnd->dpy, wnd->gc, s_xcolor[back].pixel );

         // draw in background!
         XDrawPoints( wnd->dpy, wnd->window, wnd->gc,
               pts, icount, CoordModePrevious );

         // reset foreground
         XSetForeground( wnd->dpy, wnd->gc, s_xcolor[fore].pixel );
      }

      // done here
      return;
   }

   /* Clears background */
   XDrawImageString( wnd->dpy, wnd->window, wnd->gc,
      basex, basey+wnd->xfs->ascent,
      &space, 1 );

   /* Drawing a full square? */
   if ( boxchar >= HB_GTXVT_FULL )
   {
      switch( boxchar )
      {
         case HB_GTXVT_FULL:
            XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
               basex, basey, cellx, celly );
            return;

         case HB_GTXVT_FULL_T:
            XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
               basex, basey, cellx, celly/2 );
            return;

         case HB_GTXVT_FULL_B:
            XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
               basex, basey + celly/2, cellx, celly/2 );
            return;

         case HB_GTXVT_FULL_L:
            XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
               basex, basey, cellx/2, celly );
            return;

         case HB_GTXVT_FULL_R:
            XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
               basex + cellx/2, basey, cellx/2, celly );
            return;
      }
   }

   switch( boxchar )
   {
      case HB_GTXVT_SNG_LT:
         segs[0].x1 = basex + cellx/2;
         segs[0].y1 = basey + celly;
         segs[0].x2 = basex + cellx/2;
         segs[0].y2 = basey + celly/2;

         segs[1].x1 = segs[0].x2;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = basex + cellx;
         segs[1].y2 = basey + celly/2;

         nsegs = 2;
      break;

      case HB_GTXVT_SNG_TD:
         segs[0].x1 = basex;
         segs[0].y1 = basey + celly/2;
         segs[0].x2 = basex + cellx;
         segs[0].y2 = basey + celly/2;

         segs[1].x1 = basex + cellx/2;
         segs[1].y1 = segs[0].y1;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = basey + celly;

         nsegs = 2;
      break;

      case HB_GTXVT_SNG_RT:
         segs[0].x1 = basex + cellx/2;
         segs[0].y1 = basey + celly;
         segs[0].x2 = basex + cellx/2;
         segs[0].y2 = basey + celly/2;

         segs[1].x1 = segs[0].x2;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = basex;
         segs[1].y2 = basey + celly/2;

         nsegs = 2;
      break;

      case HB_GTXVT_SNG_LB:
         segs[0].x1 = basex + cellx/2;
         segs[0].y1 = basey;
         segs[0].x2 = basex + cellx/2;
         segs[0].y2 = basey + celly/2;

         segs[1].x1 = segs[0].x2;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = basex + cellx;
         segs[1].y2 = basey + celly/2;

         nsegs = 2;
      break;

      case HB_GTXVT_SNG_BU:
         segs[0].x1 = basex + cellx/2;
         segs[0].y1 = basey;
         segs[0].x2 = basex + cellx/2;
         segs[0].y2 = basey + celly/2;

         segs[1].x1 = basex;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = basex + cellx;
         segs[1].y2 = basey + celly/2;

         nsegs = 2;
      break;

      case HB_GTXVT_SNG_RB:
         segs[0].x1 = basex + cellx/2;
         segs[0].y1 = basey;
         segs[0].x2 = basex + cellx/2;
         segs[0].y2 = basey + celly/2;

         segs[1].x1 = segs[0].x2;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = basex;
         segs[1].y2 = basey + celly/2;

         nsegs = 2;
      break;

      case HB_GTXVT_SNG_VL:
         segs[0].x1 = basex + cellx/2;
         segs[0].y1 = basey;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey + celly;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = basey+celly/2;
         segs[1].x2 = basex+cellx;
         segs[1].y2 = segs[1].y1;

         nsegs = 2;
      break;

      case HB_GTXVT_SNG_VR:
         segs[0].x1 = basex + cellx/2;
         segs[0].y1 = basey;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey + celly;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = basey + celly/2;
         segs[1].x2 = basex;
         segs[1].y2 = segs[1].y1;

         nsegs = 2;
      break;

      case HB_GTXVT_SNG_CRS:
         segs[0].x1 = basex + cellx/2;
         segs[0].y1 = basey;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey + celly;

         segs[1].x1 = basex;
         segs[1].y1 = basey+celly/2;
         segs[1].x2 = basex+cellx;
         segs[1].y2 = segs[1].y1;

         nsegs = 2;
      break;

      case HB_GTXVT_SNG_HOR:
         segs[0].x1 = basex;
         segs[0].y1 = basey + celly/2;
         segs[0].x2 = basex + cellx;
         segs[0].y2 = segs[0].y1;

         nsegs = 1;
      break;

      case HB_GTXVT_SNG_VRT:
         segs[0].x1 = basex + cellx/2;
         segs[0].y1 = basey;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey + celly;

         nsegs = 1;
      break;

      case HB_GTXVT_DBL_LT:
         segs[0].x1 = basex + cellx/2-1;
         segs[0].y1 = basey + celly;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey + celly/2-1;

         segs[1].x1 = segs[0].x2;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = basex + cellx;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = basex + cellx/2+1;
         segs[2].y1 = basey + celly;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = basey + celly/2+1;

         segs[3].x1 = segs[2].x2;
         segs[3].y1 = segs[2].y2;
         segs[3].x2 = basex + cellx;
         segs[3].y2 = segs[2].y2;

         nsegs = 4;
      break;

      case HB_GTXVT_DBL_TD:
         segs[0].x1 = basex;
         segs[0].y1 = basey + celly/2-1;
         segs[0].x2 = basex + cellx;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = basey + celly/2+1;
         segs[1].x2 = basex + cellx/2-1;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = basex + cellx/2+1;
         segs[2].y1 = basey + celly/2+1;
         segs[2].x2 = basex + cellx;
         segs[2].y2 = segs[2].y1;

         segs[3].x1 = segs[1].x2;
         segs[3].y1 = segs[1].y1;
         segs[3].x2 = segs[1].x2;
         segs[3].y2 = basey + celly;

         segs[4].x1 = segs[2].x1;
         segs[4].y1 = segs[2].y1;
         segs[4].x2 = segs[2].x1;
         segs[4].y2 = basey + celly;

         nsegs = 5;
      break;

      case HB_GTXVT_DBL_RT:
         segs[0].x1 = basex + cellx/2-1;
         segs[0].y1 = basey + celly;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey + celly/2+1;

         segs[1].x1 = segs[0].x2;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = basex;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = basex + cellx/2+1;
         segs[2].y1 = basey + celly;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = basey + celly/2-1;

         segs[3].x1 = segs[2].x2;
         segs[3].y1 = segs[2].y2;
         segs[3].x2 = basex;
         segs[3].y2 = segs[2].y2;

         nsegs = 4;
      break;

      case HB_GTXVT_DBL_LB:
         segs[0].x1 = basex + cellx/2-1;
         segs[0].y1 = basey;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey + celly/2+1;

         segs[1].x1 = segs[0].x2;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = basex + cellx;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = basex + cellx/2+1;
         segs[2].y1 = basey;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = basey + celly/2-1;

         segs[3].x1 = segs[2].x2;
         segs[3].y1 = segs[2].y2;
         segs[3].x2 = basex + cellx;
         segs[3].y2 = segs[2].y2;

         nsegs = 4;
      break;

      case HB_GTXVT_DBL_BU:
         segs[0].x1 = basex;
         segs[0].y1 = basey + celly/2+1;
         segs[0].x2 = basex + cellx;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = basey + celly/2-1;
         segs[1].x2 = basex + cellx/2-1;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = basex + cellx/2+1;
         segs[2].y1 = basey + celly/2-1;
         segs[2].x2 = basex + cellx;
         segs[2].y2 = segs[2].y1;

         segs[3].x1 = segs[1].x2;
         segs[3].y1 = segs[1].y1;
         segs[3].x2 = segs[1].x2;
         segs[3].y2 = basey;

         segs[4].x1 = segs[2].x1;
         segs[4].y1 = segs[2].y1;
         segs[4].x2 = segs[2].x1;
         segs[4].y2 = basey;

         nsegs = 5;
      break;

      case HB_GTXVT_DBL_RB:
         segs[0].x1 = basex + cellx/2-1;
         segs[0].y1 = basey;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey + celly/2-1;

         segs[1].x1 = segs[0].x2;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = basex;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = basex + cellx/2+1;
         segs[2].y1 = basey;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = basey + celly/2+1;

         segs[3].x1 = segs[2].x2;
         segs[3].y1 = segs[2].y2;
         segs[3].x2 = basex;
         segs[3].y2 = segs[2].y2;

         nsegs = 4;
      break;

      case HB_GTXVT_DBL_VL:
         segs[0].x1 = basex + cellx/2-1;
         segs[0].y1 = basey;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey+ celly;

         segs[1].x1 = basex + cellx/2+1;
         segs[1].y1 = basey;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = basey + celly/2-1;

         segs[2].x1 = segs[1].x1;
         segs[2].y1 = basey + celly/2+1;
         segs[2].x2 = segs[1].x1;
         segs[2].y2 = basey + celly;

         segs[3].x1 = segs[1].x1;
         segs[3].y1 = segs[1].y2;
         segs[3].x2 = basex + cellx;
         segs[3].y2 = segs[3].y1;

         segs[4].x1 = segs[2].x1;
         segs[4].y1 = segs[2].y1;
         segs[4].x2 = basex + cellx;
         segs[4].y2 = segs[2].y1;

         nsegs = 5;
      break;


      case HB_GTXVT_DBL_VR:
         segs[0].x1 = basex + cellx/2+1;
         segs[0].y1 = basey;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey+ celly;

         segs[1].x1 = basex + cellx/2-1;
         segs[1].y1 = basey;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = basey + celly/2-1;

         segs[2].x1 = segs[1].x1;
         segs[2].y1 = basey + celly/2+1;
         segs[2].x2 = segs[1].x1;
         segs[2].y2 = basey + celly;

         segs[3].x1 = segs[1].x1;
         segs[3].y1 = segs[1].y2;
         segs[3].x2 = basex;
         segs[3].y2 = segs[3].y1;

         segs[4].x1 = segs[2].x1;
         segs[4].y1 = segs[2].y1;
         segs[4].x2 = basex;
         segs[4].y2 = segs[2].y1;

         nsegs = 5;
      break;

      case HB_GTXVT_DBL_CRS:
         segs[0].x1 = basex + cellx/2-1;
         segs[0].y1 = basey;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey + celly/2-1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = basey + celly/2+1;
         segs[1].x2 = segs[0].x1;
         segs[1].y2 = basey + celly;

         segs[2].x1 = segs[0].x1;
         segs[2].y1 = segs[0].y2;
         segs[2].x2 = basex;
         segs[2].y2 = segs[2].y1;

         segs[3].x1 = segs[1].x1;
         segs[3].y1 = segs[1].y1;
         segs[3].x2 = basex;
         segs[3].y2 = segs[1].y1;

         segs[4].x1 = basex + cellx/2+1;
         segs[4].y1 = basey;
         segs[4].x2 = segs[4].x1;
         segs[4].y2 = basey + celly/2-1;

         segs[5].x1 = segs[4].x1;
         segs[5].y1 = basey + celly/2+1;
         segs[5].x2 = segs[4].x1;
         segs[5].y2 = basey + celly;

         segs[6].x1 = segs[4].x1;
         segs[6].y1 = segs[4].y2;
         segs[6].x2 = basex + cellx;
         segs[6].y2 = segs[6].y1;

         segs[7].x1 = segs[5].x1;
         segs[7].y1 = segs[5].y1;
         segs[7].x2 = basex + cellx;
         segs[7].y2 = segs[5].y1;

         nsegs = 8;
      break;

      case HB_GTXVT_DBL_HOR:
         segs[0].x1 = basex;
         segs[0].y1 = basey + celly/2+1;
         segs[0].x2 = basex + cellx;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = basex;
         segs[1].y1 = basey + celly/2-1;
         segs[1].x2 = basex + cellx;
         segs[1].y2 = segs[1].y1;

         nsegs = 2;
      break;

      case HB_GTXVT_DBL_VRT:
         segs[0].x1 = basex + cellx/2-1;
         segs[0].y1 = basey;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey+ celly;

         segs[1].x1 = basex + cellx/2+1;
         segs[1].y1 = basey;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = basey+ celly;

         nsegs = 2;
      break;

      case HB_GTXVT_SNG_L_DBL_T:
         segs[0].x1 = basex + cellx/2;
         segs[0].y1 = basey + celly/2-1;
         segs[0].x2 = basex + cellx;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = basey + celly/2+1;
         segs[1].x2 = segs[0].x2;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = basex + cellx/2;
         segs[2].y1 = basey + celly/2-1;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = basey + celly;

         nsegs = 3;
      break;

      case HB_GTXVT_SNG_T_DBL_D:
         segs[0].x1 = basex;
         segs[0].y1 = basey + celly/2;
         segs[0].x2 = basex + cellx;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = basex + cellx/2-1;
         segs[1].y1 = basey+ celly/2;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = basey + celly;

         segs[2].x1 = basex + cellx/2+1;
         segs[2].y1 = segs[1].y1;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = segs[1].y2;

         nsegs = 3;
      break;

      case HB_GTXVT_SNG_R_DBL_T:
         segs[0].x1 = basex;
         segs[0].y1 = basey + celly/2;
         segs[0].x2 = basex + cellx/2+1;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = basex + cellx/2+1;
         segs[1].y1 = segs[0].y1;
         segs[1].x2 = segs[0].x2;
         segs[1].y2 = basey + celly;

         segs[2].x1 = basex + cellx/2-1;
         segs[2].y1 = basey + celly/2;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = basey + celly;

         nsegs = 3;
      break;

      case HB_GTXVT_SNG_L_DBL_B:
         segs[0].x1 = basex + cellx/2;
         segs[0].y1 = basey + celly/2-1;
         segs[0].x2 = basex + cellx;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = basey + celly/2+1;
         segs[1].x2 = segs[0].x2;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = basex + cellx/2;
         segs[2].y1 = basey;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = basey + celly/2+1;

         nsegs = 3;
      break;

      case HB_GTXVT_SNG_B_DBL_U:
         segs[0].x1 = basex;
         segs[0].y1 = basey + celly/2;
         segs[0].x2 = basex + cellx;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = basex + cellx/2-1;
         segs[1].y1 = basey;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = basey + celly/2;

         segs[2].x1 = basex + cellx/2+1;
         segs[2].y1 = basey;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = basey + celly/2;

         nsegs = 3;
      break;

      case HB_GTXVT_SNG_R_DBL_B:
         segs[0].x1 = basex;
         segs[0].y1 = basey + celly/2;
         segs[0].x2 = basex + cellx/2+1;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = basex + cellx/2+1;
         segs[1].y1 = basey;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = basey + celly/2;

         segs[2].x1 = basex + cellx/2-1;
         segs[2].y1 = basey;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = basey + celly/2;

         nsegs = 3;
      break;

      case HB_GTXVT_SNG_V_DBL_L:
         segs[0].x1 = basex + cellx/2;
         segs[0].y1 = basey;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey + celly;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = basey + celly/2-1;
         segs[1].x2 = basex + cellx;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = segs[0].x1;
         segs[2].y1 = basey + celly/2+1;
         segs[2].x2 = segs[1].x2;
         segs[2].y2 = segs[2].y1;

         nsegs = 3;
      break;

      case HB_GTXVT_SNG_V_DBL_R:
         segs[0].x1 = basex + cellx/2;
         segs[0].y1 = basey;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey + celly;

         segs[1].x1 = basex;
         segs[1].y1 = basey + celly/2-1;
         segs[1].x2 = segs[0].x1;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = basex;
         segs[2].y1 = basey + celly/2+1;
         segs[2].x2 = segs[0].x1;
         segs[2].y2 = segs[2].y1;

         nsegs = 3;
      break;

      case HB_GTXVT_SNG_DBL_CRS:
         segs[0].x1 = basex + cellx/2;
         segs[0].y1 = basey;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey + celly;

         segs[1].x1 = basex;
         segs[1].y1 = basey + celly/2-1;
         segs[1].x2 = basex + cellx;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = basex;
         segs[2].y1 = basey + celly/2+1;
         segs[2].x2 = segs[1].x2;
         segs[2].y2 = segs[2].y1;

         nsegs = 3;
      break;


      case HB_GTXVT_DBL_L_SNG_T:
         segs[0].x1 = basex + cellx/2-1;
         segs[0].y1 = basey + celly/2;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey + celly;

         segs[1].x1 = basex + cellx/2 + 1;
         segs[1].y1 = segs[0].y1;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = segs[0].x1;
         segs[2].y1 = segs[0].y1;
         segs[2].x2 = basex + cellx;
         segs[2].y2 = segs[0].y1;

         nsegs = 3;
      break;

      case HB_GTXVT_DBL_T_SNG_D:
         segs[0].x1 = basex;
         segs[0].y1 = basey + celly/2-1;
         segs[0].x2 = basex + cellx;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = basey + celly/2+1;
         segs[1].x2 = segs[0].x2;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = basex + cellx/2;
         segs[2].y1 = basey + celly/2+1;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = basey + celly;

         nsegs = 3;
      break;

      case HB_GTXVT_DBL_R_SNG_T:
         segs[0].x1 = basex;
         segs[0].y1 = basey + celly/2-1;
         segs[0].x2 = basex + cellx/2;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = basey + celly/2+1;
         segs[1].x2 = segs[0].x2;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = basex + cellx/2;
         segs[2].y1 = basey + celly/2-1;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = basey + celly;

         nsegs = 3;
      break;

      case HB_GTXVT_DBL_L_SNG_B:
         segs[0].x1 = basex + cellx/2-1;
         segs[0].y1 = basey;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey + celly/2;

         segs[1].x1 = basex + cellx/2+1;
         segs[1].y1 = segs[0].y1;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = basex + cellx/2-1;
         segs[2].y1 = segs[0].y2;
         segs[2].x2 = basex + cellx;
         segs[2].y2 = segs[0].y2;

         nsegs = 3;
      break;

      case HB_GTXVT_DBL_B_SNG_U:
         segs[0].x1 = basex;
         segs[0].y1 = basey + celly/2-1;
         segs[0].x2 = basex + cellx;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = basey + celly/2+1;
         segs[1].x2 = segs[0].x2;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = basex + cellx/2;
         segs[2].y1 = basey;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = basey + celly/2-1;

         nsegs = 3;
      break;

      case HB_GTXVT_DBL_R_SNG_B:
         segs[0].x1 = basex;
         segs[0].y1 = basey + celly/2-1;
         segs[0].x2 = basex + cellx/2;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = basey + celly/2+1;
         segs[1].x2 = segs[0].x2;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = basex + cellx/2;
         segs[2].y1 = basey;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = basey + celly/2+1;

         nsegs = 3;
      break;

      case HB_GTXVT_DBL_V_SNG_L:
         segs[0].x1 = basex + cellx/2-1;
         segs[0].y1 = basey;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey + celly;

         segs[1].x1 = basex + cellx/2+1;
         segs[1].y1 = segs[0].y1;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = segs[1].x1;
         segs[2].y1 = basey + celly/2;
         segs[2].x2 = basex + cellx;
         segs[2].y2 = segs[2].y1;

         nsegs = 3;
      break;

      case HB_GTXVT_DBL_V_SNG_R:
         segs[0].x1 = basex + cellx/2-1;
         segs[0].y1 = basey;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey + celly;

         segs[1].x1 = basex + cellx/2+1;
         segs[1].y1 = segs[0].y1;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = basex;
         segs[2].y1 = basey + celly/2;
         segs[2].x2 = segs[0].x1;
         segs[2].y2 = segs[2].y1;

         nsegs = 3;
      break;

      case HB_GTXVT_DBL_SNG_CRS:
         segs[0].x1 = basex + cellx/2-1;
         segs[0].y1 = basey;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = basey + celly;

         segs[1].x1 = basex + cellx/2+1;
         segs[1].y1 = segs[0].y1;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = basex;
         segs[2].y1 = basey + celly/2;
         segs[2].x2 = basex + cellx;
         segs[2].y2 = segs[2].y1;

         nsegs = 3;
      break;

      case HB_GTXVT_CHECK:
         segs[0].x1 = basex;
         segs[0].y1 = basey+ celly/2;
         segs[0].x2 = basex+2;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x2;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = basex + cellx/2;
         segs[1].y2 = basey+ celly;

         segs[2].x1 = segs[1].x2;
         segs[2].y1 = segs[1].y2;
         segs[2].x2 = basex + cellx;
         segs[2].y2 = basey;

         nsegs = 3;
      break;
   }

   if ( nsegs == 0  ) {
      XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
         basex+2, basey+2,
         cellx-2, celly-2);
   }
   else
   {
      XDrawSegments( wnd->dpy, wnd->window, wnd->gc,
         segs, nsegs);
   }
}

/*
   sets foreground and background colours from attribute and set them for window
*/

static void xvt_windowSetColors( PXWND_DEF wnd, BYTE attr )
{
   int fore = attr & 0x000F;
   int back = (attr & 0x00F0)>>4;

   XSetForeground( wnd->dpy, wnd->gc, s_xcolor[fore].pixel );
   XSetBackground( wnd->dpy, wnd->gc, s_xcolor[back].pixel );
}

/**********************************************************************
* XVT Window message operations                                       *
**********************************************************************/

/*********************** Manage events of KEY type  *****************/

static void xvt_eventKeyProcess( PXVT_BUFFER buffer, XKeyEvent *evt)
{
   unsigned char buf[5];
   KeySym out;
   int ikey = 0;
   XComposeStatus compose;

   XLookupString( evt , buf, 5, &out, &compose );
   //printf( "BUF: %s\n", buf );

   switch( out )
   {
      // First of all, let's scan for special codes
      case XK_Shift_L: case XK_Shift_R:
         s_modifiers.bShift = TRUE;
      return;

      case XK_Control_L: case XK_Control_R:
         s_modifiers.bCtrl = TRUE;
      return;

      case XK_Alt_L:
         s_modifiers.bAlt = TRUE;
      return;

      case XK_Meta_R: case XK_Alt_R:
         s_modifiers.bAltGr = TRUE;
      return;

      //Then we scan for movement
      case XK_Left: case XK_KP_Left:
         ikey = K_LEFT;
         break;
      case XK_Right: case XK_KP_Right:
         ikey =  K_RIGHT ;
         break;
      case XK_Up: case XK_KP_Up:
         ikey =  K_UP ;
         break;
      case XK_Down: case XK_KP_Down:
         ikey =  K_DOWN ;
         break;
      case XK_Begin: case XK_Home: case XK_KP_Home: case XK_KP_Begin:
         ikey =  K_HOME ;
         break;
      case XK_End: case XK_KP_End:
         ikey =  K_END ;
         break;
      case XK_Page_Up: case XK_KP_Page_Up:
         ikey =  K_PGUP ;
         break;
      case XK_Page_Down: case XK_KP_Page_Down:
         ikey =  K_PGDN ;
         break;
      case XK_KP_Enter:
         ikey =  K_ENTER ;
         break;

      // Special cursor operations

      case XK_Delete: case XK_KP_Delete:
         ikey = K_DEL;
         break;
      case XK_Insert: case XK_KP_Insert:
         ikey = K_INS;
         break;
      case XK_BackSpace:
         ikey = K_BS;
         break;
      case XK_Tab:
         ikey = K_TAB;
         break;
      case XK_Linefeed: case XK_Return:
         ikey = K_ENTER;
         break;
      case XK_Escape:
         xvt_bufferQueueKey( buffer, K_ESC );
         return;

      // then we scan for function keys
      case XK_F1:
         ikey = K_F1;
         break;
      case XK_F2:
         ikey = K_F2;
         break;
      case XK_F3:
         ikey = K_F3;
         break;
      case XK_F4:
         ikey = K_F4;
         break;
      case XK_F5:
         ikey = K_F5;
         break;
      case XK_F6:
         ikey = K_F6;
         break;
      case XK_F7:
         ikey = K_F7;
         break;
      case XK_F8:
         ikey = K_F8;
         break;
      case XK_F9:
         ikey = K_F9;
         break;
      case XK_F10:
         ikey = K_F10;
         break;
      case XK_F11:
         ikey = K_F11;
         break;
      case XK_F12:
         ikey = K_F12;
         break;

      // Keys with special meanings to clipper
      case XK_KP_5:
         if ( s_modifiers.bCtrl ) {
            xvt_bufferQueueKey( buffer, KP_CTRL_5 );
            break;
         }
         // else fallback and add a normal 5
      break;

      case XK_Pause:
         if ( s_modifiers.bCtrl ) {
            // Pretend Alt+C pressed
            xvt_bufferQueueKey( buffer, HB_BREAK_FLAG );
            break;
         }
      break;

      case XK_KP_Multiply:
         ikey = '*';
         break;
      case XK_KP_Add:
         ikey = '+';
         break;
      case XK_KP_Subtract:
         ikey = '-';
         break;
      case XK_KP_Divide:
         ikey = '/';
         break;
   }

   // we found it in the special keys
   if ( ikey ) {
      xvt_bufferQueueKey( buffer, xvt_keyTranslate( ikey ) );
      return;
   }

   // if it is a ? we can have still one special CLIPPER character
   if ( *buf == '?' && buf[1] == 0 && s_modifiers.bCtrl )
   {
      xvt_bufferQueueKey( buffer, K_CTRL_QUESTION );
   }
   else
   {
      if ( (s_modifiers.bCtrl && *buf <=26 && buf[1] == 0) ||
            (! s_modifiers.bCtrl && ! s_modifiers.bAlt ) )
      {
         // ready for UTF input!!!
         if ( (out >= XK_KP_0 && out <= XK_KP_9) || out == XK_KP_Decimal )
         {
            xvt_bufferQueueKey( buffer, *buf );
         }
         else
         {
            xvt_bufferQueueKey( buffer, *buf + (buf[1] << 8) );
         }
      }
      else {
         xvt_bufferQueueKey( buffer, xvt_keyTranslate( *buf + (buf[1] << 8) ) );
      }
   }
}

/*********************** Manage events received by a terminal window  *****************/
static void xvt_eventManage( PXWND_DEF wnd, XEvent *evt )
{
   switch (evt->type)
   {

      case Expose:
         xvt_windowRepaintColRow( wnd,
            evt->xexpose.x / wnd->fontWidth, evt->xexpose.y / wnd->fontHeight,
            (evt->xexpose.x + evt->xexpose.width) / wnd->fontWidth,
            (evt->xexpose.y + evt->xexpose.height) / wnd->fontHeight);

      break;

      case KeyPress:
      {
         xvt_eventKeyProcess( wnd->buffer, &evt->xkey );
      }
      break;

      case KeyRelease:
      {
         KeySym out = XLookupKeysym( &evt->xkey, 0 );

         switch( out )
         {
            // First of all, let's scan for special codes
            case XK_Shift_L: case XK_Shift_R:
               s_modifiers.bShift = FALSE;
            return;

            case XK_Control_L: case XK_Control_R:
               s_modifiers.bCtrl = FALSE;
            return;

            case XK_Alt_L:
               s_modifiers.bAlt = FALSE;
            return;

            case XK_Meta_R: case XK_Alt_R:
               s_modifiers.bAltGr = FALSE;
            return;
         }
      }
      break;

      case MappingNotify:
         XRefreshKeyboardMapping( &evt->xmapping );
      break;

      case MotionNotify:
         wnd->status->mouseCol = evt->xmotion.x / wnd->fontWidth;
         wnd->status->mouseRow = evt->xmotion.y / wnd->fontHeight;
         wnd->status->lastMouseEvent = K_MOUSEMOVE;
      break;

      case ButtonPress: case ButtonRelease:
      {
         unsigned char map[ XVT_MAX_BUTTONS ];
         int button=0;
         PXVT_STATUS status = wnd->status;

         XGetPointerMapping( wnd->dpy, map, wnd->status->mouseNumButtons );

         switch ( evt->xbutton.button )
         {
            case Button1:
               button = map[0];
               if ( evt->type == ButtonPress )
               {
                  if ( status->mouseDblClick1TO > 0 )
                  {
                     status->lastMouseEvent = K_LDBLCLK;
                  }
                  else
                  {
                     status->lastMouseEvent = K_LBUTTONDOWN;
                  }
                  // about half a second
                  status->mouseDblClick1TO = 8;
               }
               else {
                  status->lastMouseEvent = K_LBUTTONUP;
               }
            break;

            case Button2:
               button = map[1];
               if ( status->mouseNumButtons == 2 )
               {
                  if ( evt->type == ButtonPress )
                  {
                     if ( status->mouseDblClick2TO > 0 )
                     {
                         status->lastMouseEvent = K_RDBLCLK;
                     }
                     else
                     {
                         status->lastMouseEvent = K_RBUTTONDOWN;
                     }
                     // about half a second
                     status->mouseDblClick2TO = 8;
                  }
                  else {
                     status->lastMouseEvent = K_RBUTTONUP;
                  }
               }
               else
               {
                  /* paste content of the paste buffer */
                  if ( evt->type == ButtonPress )
                  {
                     int aprop, atarget, asel;

                     asel = XInternAtom( wnd->dpy, "PRIMARY", 1 );
                     aprop = XInternAtom( wnd->dpy, "CUT_BUFFER0", 1 );
                     atarget = XInternAtom( wnd->dpy, "STRING", 1 );
                     /* signal a X-Paste request */
                     wnd->usFlags = 1;
                     XConvertSelection( wnd->dpy,
                        asel, atarget, aprop,
                        wnd->window,
                        CurrentTime );
                  }
               }
            break;

            case Button3:
               button = map[2];
               if ( status->mouseNumButtons >= 3 )
               {
                  if ( evt->type == ButtonPress )
                  {
                     if ( status->mouseDblClick2TO > 0 )
                     {
                        status->lastMouseEvent = K_RDBLCLK;
                     }
                     else
                     {
                        status->lastMouseEvent = K_RBUTTONDOWN;
                     }
                     // about half a second
                     status->mouseDblClick2TO = 8;
                  }
                  else {
                     status->lastMouseEvent = K_RBUTTONUP;
                  }
               }
            break;

            case Button4:
               button = map[3];
               if ( evt->type == ButtonPress )
               {
                  status->lastMouseEvent = 0;
               }
               else
               {
                  status->lastMouseEvent = K_MWFORWARD;
               }
            break;

            case Button5:
               button = map[4];
               if ( evt->type == ButtonPress )
               {
                  status->lastMouseEvent = 0;
               }
               else
               {
                  status->lastMouseEvent = K_MWBACKWARD;
               }
            break;
         }

         button--;

         if ( button < 0 || button >= status->mouseNumButtons )
         {
            button = 0;
         }

         if ( evt->type == ButtonPress )
         {
            status->mouseButtons[ button ] = TRUE;
         }
         else
         {
            status->mouseButtons[ button ] = FALSE;
         }
      }
      break;

      case ConfigureNotify:
      {
         ICM_DATA_RESIZE resize;
         USHORT appMsg;
         resize.cols = evt->xconfigure.width/wnd->fontWidth;
         resize.rows = evt->xconfigure.height/wnd->fontHeight;
         if ( resize.cols != wnd->buffer->cols || resize.rows != wnd->buffer->rows )
         {
            appMsg = XVT_ICM_RESIZE;
            write( streamFeedback[1], &appMsg, sizeof( appMsg ) );
            write( streamFeedback[1], &resize, sizeof( resize ) );
         }
      }
      break;

      // Protocol request from the window manager (usually delete window)
      case ClientMessage:
         // yes; a delete request
         if ( (ULONG) evt->xclient.data.l[0] == s_atom_delwin )
         {
            USHORT appMsg = XVT_ICM_QUIT;
            //write( streamFeedback[1], &appMsg, sizeof( appMsg ) );
            write( streamChr[1], &appMsg, sizeof( appMsg ) );
         }
      break;

      // Notifies of sucessful cut&paste recival
      case SelectionNotify:
      {
         XTextProperty text;
         int data, stream;
         USHORT appMsg;
         ULONG nCount, npos;
         char cData;

         // have we got a property to read?
         if ( evt->xselection.property != None )
         {
            if ( XGetTextProperty( wnd->dpy,
                  wnd->window, &text, evt->xselection.property) != 0 )
            {
               // is the action generated internally?
               if ( wnd->usFlags )
               {
                  stream = streamChr[1];
                  appMsg = XVT_ICM_KEYSTORE;

                  /* Signal the application we have received the data */
                  /* TODO: use UTF buffer for clipboard*/
                  for( npos = 0; npos < text.nitems; npos++ )
                  {
                     switch (text.format)
                     {
                        case 8: data = (int) text.value[npos]; break;
                        case 16: data = (int) ((unsigned short *)text.value)[npos]; break;
                        case 32: data = (int) ((unsigned int *)text.value)[npos]; break;
                        default: data = 0;
                     }
                     write( stream, &appMsg, sizeof( appMsg ) );
                     write( stream, &data, sizeof( data ) );
                  }
               }
               else
               {
                  stream = streamFeedback[1];
                  appMsg= XVT_ICM_SETSELECTION;
                  write( stream, &appMsg, sizeof( appMsg ) );
                  nCount = (ULONG) text.nitems;
                  write( stream, &nCount, sizeof( nCount ) );
                  for( npos = 0; npos < text.nitems; npos++ )
                  {
                     switch (text.format)
                     {
                        case 8: cData = (int) text.value[npos]; break;
                        case 16: cData = (int) ((unsigned short *)text.value)[npos]; break;
                        case 32: cData = (int) ((unsigned int *)text.value)[npos]; break;
                        default: cData = 0;
                     }
                     write( stream, &cData, sizeof( cData ) );
                  }
               }
            }
         }
         wnd->usFlags = 0;
      }
      break;

      // Someone asked us to give away our selection
      case SelectionRequest:
      {
         XSelectionRequestEvent *req;
         XEvent respond;
         ULONG atarget, atomtg;

         atarget = XInternAtom( wnd->dpy, "STRING", 1 );
         atomtg = XInternAtom( wnd->dpy, "TARGETS", 1 );

         req = &(evt->xselectionrequest);
         if (req->target == atarget)
         {
            XChangeProperty (wnd->dpy,
               req->requestor,
               req->property, atarget, 8, PropModeReplace,
               (unsigned char*) s_clipboard,
               s_clipsize);
            respond.xselection.property = req->property;
         }
         else if ( req->target == atomtg )
         {
            XChangeProperty (wnd->dpy,
               req->requestor,
               req->property, atomtg, 32,
               PropModeReplace,
               (unsigned char*) &atarget,
               1);
         }
         else
         {
            respond.xselection.property = None;
         }

         respond.xselection.type = SelectionNotify;
         respond.xselection.display = req->display;
         respond.xselection.requestor = req->requestor;
         respond.xselection.selection = req->selection;
         respond.xselection.target = req->target;
         respond.xselection.time = CurrentTime;

         XSendEvent (wnd->dpy, req->requestor, 0, 0, &respond);
         XFlush (wnd->dpy);
      }
      break;
   }
}


static void xvt_processMessages( PXWND_DEF wnd )
{
   static int count = 0;
   USHORT appMsg;
   XEvent evt;
   fd_set updateSet;
   struct timeval timeout;
   HB_GT_GOBJECT *pgObj;
   //struct timespec ts;
   BOOL bLoop = TRUE, bUpdate;
   int aprop, atarget, asel;

   XSegment rUpdate = {32000,32000,-1,-1};

   FD_ZERO(&updateSet);

   while ( bLoop )
   {
      bUpdate = FALSE;

      //ts.tv_sec = 0;
      //ts.tv_nsec = 25000000;
      //nanosleep( &ts, NULL );
      // wait for app input
      //usleep( 25000 );
      timeout.tv_sec = 0;
      if ( s_wnd == NULL )
      {
         timeout.tv_usec = 25000;
      }
      else {
         timeout.tv_usec = 0;
      }

      FD_SET(streamUpdate[0], &updateSet );
      bLoop = TRUE;
      while( bLoop &&
         select( streamUpdate[0] + 1, &updateSet, NULL, NULL, &timeout ) )
      {
         appMsg = 0xFFFF;
         if ( read( streamUpdate[0], &appMsg, sizeof( appMsg ) ) <= 0 )
         {
            return;
         }

         switch( appMsg )
         {
            // when the app requires a resize, resize value is inside the buffer.
            case XVT_ICM_RESIZE:
               xvt_windowResize( wnd );
            break;

            case XVT_ICM_MOUSEMOVE:
            {
               ICM_DATA_RESIZE data;
               read( streamUpdate[0], &data, sizeof( data ) );

               XWarpPointer( wnd->dpy, None, wnd->window, 0,0,0,0,
                  data.cols * wnd->fontWidth + wnd->fontWidth/2,
                  data.rows * wnd->fontHeight + wnd->fontHeight/2 );
            }
            break;

            case XVT_ICM_UPDATE:
            {
               XSegment added;
               bUpdate = TRUE;
               //read update data
               if ( read (streamUpdate[0], &added, sizeof( added ) ) <= 0 )
               {
                  return;
               }
               if ( added.x1 < rUpdate.x1 ) rUpdate.x1 = added.x1;
               if ( added.y1 < rUpdate.y1 ) rUpdate.y1 = added.y1;
               if ( added.x2 > rUpdate.x2 ) rUpdate.x2 = added.x2;
               if ( added.y2 > rUpdate.y2 ) rUpdate.y2 = added.y2;
            }
            break;

            case XVT_ICM_SETCURSOR:
               xvt_windowSetCursor( wnd );
            break;

            /* Manage copy requests */
            case XVT_ICM_SETSELECTION:
            {
               ULONG nlen;

               // read data from application
               read( streamUpdate[0], &nlen, sizeof( nlen ) );
               if ( s_clipboard != NULL )
               {
                  hb_xfree( s_clipboard );
               }
               s_clipboard = hb_xgrab( nlen +1);
               read( streamUpdate[0], s_clipboard, nlen);
               s_clipboard[ nlen ] = '\0';
               s_clipsize = nlen;

               asel = XInternAtom( wnd->dpy, "PRIMARY", 1 );
               // tell the server we own the new selection
               XSetSelectionOwner(wnd->dpy, asel, wnd->window, CurrentTime);
            }

            /*If a request to achieve a selection is received... */
            case XVT_ICM_GETSELECTION:

               asel = XInternAtom( wnd->dpy, "PRIMARY", 1 );
               aprop = XInternAtom( wnd->dpy, "CUT_BUFFER0", 1 );
               atarget = XInternAtom( wnd->dpy, "STRING", 1 );

               /* ask X to send the selection */
               wnd->usFlags = 0; // request event handler to put data in clipboard.
               XConvertSelection( wnd->dpy,
                  asel, atarget, aprop,
                  wnd->window,
                  CurrentTime );
            break;

            case XVT_ICM_CLEAROBJECTS:
               hb_gtClearGobjects();
            break;

            case XVT_ICM_ADDOBJECT:
               pgObj = hb_xgrab( sizeof( HB_GT_GOBJECT ) );
               read( streamUpdate[0], pgObj, sizeof( HB_GT_GOBJECT ) );
               if ( pgObj->type == GTO_TEXT )
               {
                  pgObj->data = hb_xgrab( pgObj->data_len+1 );
                  read( streamUpdate[0], pgObj->data, pgObj->data_len );
                  pgObj->data[ pgObj->data_len ] = 0;
               }
               hb_gtAddGobject( pgObj );
            break;

            case XVT_ICM_FONTSIZE:
            {
               XFontStruct *xfs;
               USHORT usData;

               read( streamUpdate[0], &usData, sizeof( usData ) );
               s_fontReqWidth = (int) usData;
               read( streamUpdate[0], &usData, sizeof( usData ) );
               s_fontReqSize = (int) usData;

               xfs = xvt_fontNew( wnd->dpy, XVT_DEFAULT_FONT_NAME,
                  XVT_DEFAULT_FONT_WEIGHT,
                  s_fontReqSize, NULL );
               if ( xfs )
               {
                  xvt_windowSetFont( wnd, xfs );
               }
            }
            break;

            case XVT_ICM_QUIT:
               return;
         }

         timeout.tv_sec = 0;
         timeout.tv_usec = 0;
         FD_SET(streamUpdate[0], &updateSet );
      }

      // now manage periodic changes
      if ( wnd->status->mouseDblClick1TO > 0 ) {
         wnd->status->mouseDblClick1TO--;
      }

      if ( wnd->status->mouseDblClick2TO > 0 )
      {
         wnd->status->mouseDblClick2TO--;
      }

      if ( ++count == 10 ) {
         s_cursorState = s_cursorState ? 0: 1;
         xvt_cursorPaint( wnd );
         count = 0;
      }

      if ( bUpdate )
      {
         xvt_windowUpdate( wnd, &rUpdate );
      }

      while ( XEventsQueued( wnd->dpy, QueuedAfterFlush) )
      {
         XNextEvent( wnd->dpy, &evt );
         xvt_eventManage( wnd, &evt );
      }

      /* Do just one loop if in single process mode */
      if ( s_wnd != NULL )
      {
         break;
      }
   }
}


/** App process is called by both inkey and output aware functions to
   process pending data.
   If wait for is different from 0, the app loop continues until the
   wait for is achieved. This is used for semi-synchronous communications
   with the parallel application.

*/
static void xvt_appProcess( USHORT usWaitFor )
{
   static int period = 50;
   USHORT appMsg;
   fd_set keySet;
   struct timeval timeout = {0,0};

   period --;

   if ( period == 0 || usWaitFor != 0)
   {
      do
      {
         // quit immediately if child is died
         if( s_childPid != 0 &&
                  waitpid( s_childPid, NULL, WNOHANG ) == s_childPid )
         {
            hb_vmRequestQuit();
            return;
         }

         FD_ZERO(&keySet);
         FD_SET(streamFeedback[0], &keySet );

         while ( select( streamFeedback[0] + 1, &keySet, NULL , NULL, &timeout) )
         {
            read( streamFeedback[0], &appMsg, sizeof( appMsg ) );
            switch( appMsg )
            {

               case XVT_ICM_RESIZE:
               {
                  ICM_DATA_RESIZE resize;
                  read( streamFeedback[0], &resize, sizeof( resize ) );
                  hb_gtSetMode( resize.rows, resize.cols );
                  hb_gtHandleResize();
               }
               break;

               case XVT_ICM_QUIT:
                  hb_gtHandleClose();
               break;

               // receiving a selection storage request
               case XVT_ICM_SETSELECTION:
               {
                  ULONG nlen;

                  // read data from application
                  read( streamFeedback[0], &nlen, sizeof( nlen ) );
                  if ( s_clipboard != NULL )
                  {
                     hb_xfree( s_clipboard );
                  }
                  s_clipboard = hb_xgrab( nlen + 1 );
                  read( streamFeedback[0], s_clipboard, nlen);
                  s_clipboard[ nlen ] = 0;
                  s_clipsize = nlen;
               }
               break;
            }

            if ( usWaitFor != 0 )
            {
               break;
            }

            timeout.tv_sec = 0;
            timeout.tv_usec = 0;
            FD_SET(streamFeedback[0], &keySet );
         }
         timeout.tv_usec = 2500;
      } while( usWaitFor != 0 && appMsg != usWaitFor );

      period = 50;
   }
}

/**********************************************************************
* XVT Status Oriented utilities                                       *
**********************************************************************/

static PXVT_STATUS xvt_statusNew( void )
{
   PXVT_STATUS status;

   status = (PXVT_STATUS) mmap( 0, sizeof( XVT_STATUS ), PROT_READ | PROT_WRITE,
         MAP_SHARED |MAP_ANON, -1, 0  );

   status->mouseDblClick1TO = 0;
   status->mouseDblClick2TO = 0;
   status->lastMouseEvent = 0;
   status->mouseNumButtons = 0;
   status->bUpdateDone = FALSE;

   COMMIT_STATUS( status );
   return status;
}

static int xvt_statusQueryMouseBtns( PXVT_STATUS status, Display *dpy )
{
   unsigned char map[1];
   int i;

   status->mouseNumButtons = XGetPointerMapping( dpy, map, 1 );

   if ( status->mouseNumButtons > XVT_MAX_BUTTONS )
   {
      status->mouseNumButtons = XVT_MAX_BUTTONS;
   }

   for ( i = 0; i < status->mouseNumButtons; i ++ )
   {
      status->mouseButtons[ i ] = FALSE;
   }

   return status->mouseNumButtons;
}

/**********************************************************************
* XVT Generic utility                                                 *
**********************************************************************/

/*************** Translate ASCII char into unicode representation **********/
static HB_GT_CELLTYPE xvt_charTranslate( unsigned char ch )
{
   int i;

   if ( ch <= 127 ) {
      #ifdef HB_BIG_ENDIAN
         return ch;
      #else
         return ch << 8;
      #endif
   }

   for( i = 0; i < XVT_BOX_CHARS; i++ )
   {
      if ( ch == boxTranslate[ i ].c1 )
      {
         #ifdef HB_BIG_ENDIAN
            return boxTranslate[ i ].c2;
         #else
            return 0xFFFF & ((boxTranslate[ i ].c2 >> 8) | (boxTranslate[ i ].c2 << 8));
         #endif
      }
   }

   #ifdef HB_BIG_ENDIAN
      return ch;
   #else
      return ch << 8;
   #endif
}


/****************** Translate Unicode representation char into ASCII char ********/
static BYTE xvt_charUntranslate( HB_GT_CELLTYPE ch )
{
   int i;

   #ifdef HB_BIG_ENDIAN
   if ( ch <= 127 )
   {
      return ch;
   }
   #else
   if ( (0xFFFF &((ch >> 8)|(ch<<8))) <= 127 )
   {
      return ch >> 8;
   }
   #endif

   for( i = 0; i < XVT_BOX_CHARS; i++ )
   {
      #ifdef HB_BIG_ENDIAN
      if ( ch == boxTranslate[ i ].c2 )
      #else
      if ( (0xFFFF &
               ((boxTranslate[ i ].c2 >> 8) | (boxTranslate[ i ].c2 << 8))
           )
         == ch )
      #endif
      {
         return boxTranslate[ i ].c1;
      }
   }

   #ifdef HB_BIG_ENDIAN
      return ch;
   #else
      return ch >> 8;
   #endif
}

/****** Translate a Unicode representation of an input char to clipper keycode *****/
static int xvt_keyTranslate( int key )
{
   int i, trans = 0;

   if ( s_modifiers.bShift || s_modifiers.bAlt || s_modifiers.bCtrl )
   {
      for( i = 0; i < CLIP_KEY_COUNT; i++ )
      {
         if ( key == stdKeyTab[ i ].key )
         {
            if( s_modifiers.bShift )
            {
               trans = stdKeyTab[ i ].shift_key;
            }
            else if( s_modifiers.bAlt )
            {
               trans = stdKeyTab[ i ].alt_key;
            }
            else
            {
               trans = stdKeyTab[ i ].ctrl_key;
            }
            break;
         }
      }
   }

   if ( trans != 0 ) {
      key = trans;
   }

   return key;
}


void xvt_cursorPaint( PXWND_DEF wnd )
{
   if ( wnd->cursRow > 0 )
   {
      xvt_windowRepaintColRow( wnd,
         wnd->cursCol, wnd->cursRow, wnd->cursCol, wnd->cursRow );
   }

   wnd->cursRow = wnd->buffer->row;
   wnd->cursCol = wnd->buffer->col;

   xvt_windowRepaintColRow( wnd,
      wnd->cursCol, wnd->cursRow, wnd->cursCol, wnd->cursRow );
}



/*****************************************************************************
*
* TODO
*
******************************************************************************/

// Exported functions for API calls

BOOL HB_EXPORT hb_xvt_gtSetMenuKeyEvent(int iMenuKeyEvent)
{
   HB_SYMBOL_UNUSED( iMenuKeyEvent );
   return FALSE;
}


BOOL HB_EXPORT hb_xvt_gtSetCodePage(int iCodePage)
{
   HB_SYMBOL_UNUSED( iCodePage );
   return FALSE;
}

int HB_EXPORT hb_xvt_gtGetLastMenuEvent(void)
{
   return 0;
}

void HB_EXPORT hb_xvt_gtSetWindowTitle(PXWND_DEF wnd, char * title)
{
   XStoreName( wnd->dpy, wnd->window, title );
}


void HB_EXPORT hb_xvt_gtSetWindowIcon(int icon)
{
   HB_SYMBOL_UNUSED(icon);
}


int HB_EXPORT hb_xvt_gtGetWindowTitle(PXWND_DEF wnd, char *title, int length)
{
   char *name;
   XFetchName( wnd->dpy, wnd->window, &name );
   strncpy( title, name, length );

   return( strlen( title ) );
}


/**********************************************************************
*                                                                     *
* PART 2: XVT- XHARBOUR GT INTERFACE FUNCTIONS                        *
*                                                                     *
**********************************************************************/

void HB_GT_FUNC(gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

   /* stdin && stdout && stderr */
   s_iStdIn  = iFilenoStdin;
   s_iStdOut = iFilenoStdout;
   s_iStdErr = iFilenoStderr;

   // In case of a fatal error, immediately stops
   //s_Xdisplay = XOpenDisplay( NULL );

   /* Prepare the GT to be started as soon as possible,
      but don't start it NOW */
   s_buffer = xvt_bufferNew( XVT_DEFAULT_COLS, XVT_DEFAULT_ROWS, 0x07 );
   s_status = xvt_statusNew();
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Exit( void ))
{
   USHORT appMsg = XVT_ICM_QUIT;
   int result;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

   if ( s_childPid > 0 )
   {
      write( streamUpdate[1], &appMsg, sizeof( appMsg ) );
      close( streamUpdate[1] );
      waitpid( s_childPid, &result, 0 );
   }

   if ( s_wnd != NULL )
   {
      // exiting
      if ( s_wnd->xfs ) {
         XFreeFont( s_wnd->dpy, s_wnd->xfs );
      }
      XCloseDisplay( s_wnd->dpy );
      hb_xfree( s_wnd );
   }

   if ( s_clipboard != NULL )
   {
      hb_xfree( s_clipboard );
   }

   munmap( s_buffer, sizeof( XVT_BUFFER) );
   munmap( s_status, sizeof( XVT_STATUS) );
}

/* *********************************************************************** */
/* returns the number of displayable columns
 */
USHORT HB_GT_FUNC(gt_GetScreenWidth( void ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));
  return(s_buffer->cols);
}

/* *********************************************************************** */
/* returns the number of displayable rows
 */
USHORT HB_GT_FUNC(gt_GetScreenHeight( void ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));
  return(s_buffer->rows);
}

/* *********************************************************************** */

SHORT HB_GT_FUNC(gt_Col( void ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));
  return(s_buffer->col);
}

/* *********************************************************************** */

SHORT HB_GT_FUNC(gt_Row( void ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));
  return(s_buffer->row);
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetPos( SHORT sRow, SHORT sCol, SHORT sMethod ))
{
   SHORT oldCol, oldRow, tmp;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", sRow, sCol, sMethod));
   HB_SYMBOL_UNUSED( sMethod );

   if (sRow >= 0 && sRow< s_buffer->rows && sCol>=0 && sCol <= s_buffer->cols )
   {
      oldCol = s_buffer->col;
      oldRow = s_buffer->row;
      s_buffer->col = sCol;
      s_buffer->row = sRow;

      if ( sCol > oldCol )
      {
         tmp = sCol;
         sCol = oldCol;
         oldCol = tmp;
      }

      if ( sRow > oldRow )
      {
         tmp = sRow;
         sRow = oldRow;
         oldRow = tmp;
      }

      xvt_bufferInvalidate( s_buffer, sCol, sRow, oldCol, oldRow );
   }
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_AdjustPos( BYTE * pStr, ULONG ulLen ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));
  HB_SYMBOL_UNUSED( pStr );
  HB_SYMBOL_UNUSED( ulLen );
  return(FALSE);
}


/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_IsColor( void ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));
  return(TRUE);
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_GetCursorStyle( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));
   return s_buffer->curs_style;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetCursorStyle( USHORT usStyle ))
{
   USHORT appMsg;
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", usStyle));

   s_buffer->curs_style = usStyle;

   if ( s_childPid > 0 )
   {
      appMsg = XVT_ICM_SETCURSOR;
      write( streamUpdate[1], &appMsg, sizeof( appMsg ) );
   }
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_DispBegin( void ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));
  ++s_uiDispCount;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_DispEnd())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

   if (s_uiDispCount > 0)
   {
      --s_uiDispCount;

      if ( s_uiDispCount == 0 && ( s_childPid > 0 || s_wnd != NULL ) )
      {
         USHORT appMsg;

         msync( s_buffer, sizeof( XVT_BUFFER ), MS_INVALIDATE | MS_ASYNC );
         appMsg = XVT_ICM_UPDATE;
         write( streamUpdate[1], &appMsg, sizeof( appMsg ) );
         write( streamUpdate[1], &s_buffer->rInvalid, sizeof( XSegment ) );
         /* Single process? --> update! */
         if ( s_wnd != NULL )
         {
            xvt_processMessages( s_wnd );
         }
      }
   }
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_DispCount())
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispCount()"));
  return s_uiDispCount;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Puts( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE *pbyStr, ULONG ulLen ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", usRow, usCol, (int) byAttr, pbyStr, ulLen));
  XVT_INITIALIZE

  xvt_bufferWriteBytes( s_buffer, usCol, usRow, byAttr, pbyStr, ulLen);
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Replicate( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE byChar, ULONG ulLen ))
{
   BYTE ucBuff[XVT_CHAR_BUFFER], *byChars;
   ULONG i;
   BOOL bMalloc = FALSE;
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", usRow, usCol, byAttr, byChar, ulLen));

   if (ulLen > XVT_CHAR_BUFFER)
   {  // Avoid allocating memory if possible
      byChars = (BYTE*) hb_xgrab(ulLen);
      bMalloc= TRUE;
   }
   else
   {
      byChars = ucBuff ;
   }

   for (i = 0; i < ulLen; i++)
   {
      *(byChars+i) = byChar;
   }

   xvt_bufferWriteBytes( s_buffer, usCol, usRow, byAttr, byChars, ulLen);

   if (bMalloc)
   {
      hb_xfree(byChars);
   }
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_RectSize( USHORT rows, USHORT cols ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_RectSize()"));
  return(rows * cols * 2);
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_GetText( USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * sBuffer ))
{
   USHORT irow, icol, index, j;
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", top, left, bottom, right, sBuffer));

   j = 0;
   for (irow = top; irow <= bottom; irow++)
   {
      index = HB_GT_INDEXOF(s_buffer, left, irow);
      for (icol = left; icol <= right; icol++, index++)
      {
         if (index >= s_buffer->bufsize)
         {
            break;
         }
         else
         {
            sBuffer[j++] = xvt_charUntranslate( s_buffer->pBuffer[index] );
            sBuffer[j++] = (BYTE) s_buffer->pAttributes[index];
         }
      }
   }
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_PutText( USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * sBuffer ))
{
   USHORT irow, icol, index, j;
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", top, left, bottom, right, sBuffer));

   XVT_INITIALIZE

   j = 0;
   for (irow = top; irow <= bottom; irow++)
   {
      index = HB_GT_INDEXOF( s_buffer, left, irow );
      for (icol = left; icol <= right; icol++, index++ )
      {
         if (index >= s_buffer->bufsize/HB_GT_CELLSIZE)
         {
            break;
         }
         else
         {
            s_buffer->pBuffer[index] = xvt_charTranslate( sBuffer[j++] );
            s_buffer->pAttributes[index] = sBuffer[j++];
         }
      }
   }

   xvt_bufferInvalidate( s_buffer, left, top, right, bottom);
}

static void xvt_putTextInternal (
      USHORT top, USHORT left, USHORT bottom, USHORT right,
      USHORT width,
      BYTE * sBuffer )
{
   USHORT irow, icol, index, j;
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", top, left, bottom, right, sBuffer));

   j = 0;
   for (irow = top; irow <= bottom; irow++)
   {
      index = HB_GT_INDEXOF( s_buffer, left, irow );
      j = irow * width*2;
      for (icol = left; icol <= right; icol++, index++ )
      {
         if (index >= s_buffer->bufsize/HB_GT_CELLSIZE)
         {
            break;
         }
         else
         {
            s_buffer->pBuffer[index] = xvt_charTranslate( sBuffer[j++] );
            s_buffer->pAttributes[index] = sBuffer[j++];
         }
      }
   }

   xvt_bufferInvalidate( s_buffer, left, top, right, bottom);
}
/* *********************************************************************** */

void HB_GT_FUNC(gt_SetAttribute( USHORT rowStart, USHORT colStart, USHORT rowStop, USHORT colStop, BYTE attr ))
{
   USHORT irow, icol, index;
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d", usTop, usLeft, usBottom, usRight, (int) attr));
   for ( irow = rowStart; irow <=rowStop; irow++)
   {
      index = HB_GT_INDEXOF(s_buffer, colStart, irow);
      for (icol = colStart; icol <= colStop; icol++)
      {
         if (index >= s_buffer->bufsize/HB_GT_CELLSIZE )
         {
            break;
         }
         else
         {
            s_buffer->pAttributes[index++] = attr;
         }
      }
   }

   xvt_bufferInvalidate( s_buffer, colStart, rowStart, colStop, rowStop );
}


/* *********************************************************************** */

void HB_GT_FUNC(gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE byAttr, SHORT iRows, SHORT iCols ))
{
   SHORT usSaveRow, usSaveCol;
   unsigned char ucBlank[XVT_CHAR_BUFFER], ucBuff[XVT_CHAR_BUFFER*2] ;
   unsigned char * fpBlank ;
   unsigned char * fpBuff  ;
   int iLength = ( usRight - usLeft ) + 1;
   int iCount, iColOld, iColNew, iColSize;
   BOOL bMalloc = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRight, (int) byAttr, iRows, iCols));

   if (iLength > XVT_CHAR_BUFFER)
   { // Avoid allocating memory if possible
      fpBlank = ( unsigned char * ) hb_xgrab( iLength );
      fpBuff  = ( unsigned char * ) hb_xgrab( iLength * 2 );  //*2 room for attribs
      bMalloc = TRUE;
   }
   else
   {
      fpBlank = ucBlank ;
      fpBuff  = ucBuff  ;
   }

   s_buffer->background = byAttr;
   memset( fpBlank, ' ', iLength );

   iColOld = iColNew = usLeft;
   iColSize = iLength -1;
   if( iCols >= 0 )
   {
      iColOld += iCols;
      iColSize -= iCols;
   }
   else
   {
      iColNew -= iCols;
      iColSize += iCols;
   }

   HB_GT_FUNC(gt_DispBegin());

   usSaveCol = HB_GT_FUNC(gt_Col() ) ;
   usSaveRow = HB_GT_FUNC(gt_Row() ) ;
   for( iCount = ( iRows >= 0 ? usTop : usBottom );
         ( iRows >= 0 ? iCount <= usBottom : iCount >= usTop );
         ( iRows >= 0 ? iCount++ : iCount-- ) )
   {
         int iRowPos = iCount + iRows;


         /* Read the text to be scrolled into the current row */
         if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
         {
         HB_GT_FUNC(gt_GetText( iRowPos, iColOld, iRowPos, iColOld + iColSize, fpBuff ));
         }

         /* Blank the scroll region in the current row */
         HB_GT_FUNC(gt_Puts( iCount, usLeft, byAttr, fpBlank, iLength ));

         /* Write the scrolled text to the current row */
         if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
         {
            HB_GT_FUNC(gt_PutText( iCount, iColNew, iCount, iColNew + iColSize, fpBuff ));
         }
   }
   HB_GT_FUNC(gt_SetPos( usSaveRow, usSaveCol, HB_GT_SET_POS_AFTER ));

   HB_GT_FUNC(gt_DispEnd());

   xvt_bufferInvalidate( s_buffer, 0, 0, s_buffer->cols-1, s_buffer->rows-1 );

   if (bMalloc)
   {
      hb_xfree( fpBlank );
      hb_xfree( fpBuff );
   }

}

/* ***********************************************************************
   resize the (existing) window
 */
BOOL HB_GT_FUNC(gt_SetMode( USHORT row, USHORT col ))
{
   BOOL bResult= FALSE;
   int oldrows, oldcols;
   BYTE *memory;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", row, col));

   oldrows = s_buffer->rows-1;
   oldcols = s_buffer->cols-1;

   // ignore stupid requests
   if ( col < 1 || row < 1 ||
        col > XVT_MAX_COLS || row > XVT_MAX_ROWS ||
        ( col == s_buffer->rows && row == s_buffer->rows) )
   {
      return FALSE;
   }

   memory = (BYTE *) hb_xgrab( (s_buffer->rows *s_buffer->cols+ 1) * HB_GT_CELLSIZE );

   HB_GT_FUNC( gt_DispBegin() );
   HB_GT_FUNC(gt_GetText( 0, 0, oldrows, oldcols, memory ));

   if (row<= XVT_MAX_ROWS && col<= XVT_MAX_COLS)
   {
      bResult = xvt_bufferResize( s_buffer, col, row);
      if ( bResult )
      {
         if ( row > oldrows )
         {
            row = oldrows;
         }
         if ( col > oldcols  )
         {
            col = oldcols;
         }
         xvt_putTextInternal( 0, 0, row, col, oldcols+1,  memory );

         xvt_bufferInvalidate( s_buffer, 0, 0, s_buffer->cols, s_buffer->rows );
      }
   }

   hb_xfree( memory );
   HB_GT_FUNC( gt_DispEnd() );

   return(bResult);
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_GetBlink())
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));
  return(TRUE);
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetBlink( BOOL bBlink ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));
  HB_SYMBOL_UNUSED( bBlink );
}

/* *********************************************************************** */

char * HB_GT_FUNC(gt_Version( void ))
{
  return("xHarbour Terminal: XWindows buffered XVT");
}

/* *********************************************************************** */

static void HB_GT_FUNC(gt_xPutch( USHORT iRow, USHORT iCol, BYTE bAttr, BYTE bChar ))
{
   USHORT index;
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %i)", iRow, iCol, (int) bAttr, bChar));

   XVT_INITIALIZE

   index = HB_GT_INDEXOF( s_buffer, iCol, iRow);
   if (index < s_buffer->bufsize )
   {
      s_buffer->pBuffer[index] = xvt_charTranslate( bChar );
      s_buffer->pAttributes[index] = bAttr;

      //determine bounds of rect around character to refresh
      // but do not invalidate during screen updates
      xvt_bufferInvalidate(s_buffer, iCol, iRow, iCol, iRow);
   }
}

/* *********************************************************************** */

// copied from gtwin
USHORT HB_GT_FUNC(gt_Box( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
                          BYTE * szBox, BYTE byAttr ))
{
    USHORT ret = 1;
    SHORT Row;
    SHORT Col;
    SHORT Height;
    SHORT Width;
    USHORT sWidth = s_buffer->cols;
    USHORT sHeight = s_buffer->rows;

    if( ( Left   >= 0 && Left   < sWidth  ) ||
        ( Right  >= 0 && Right  < sWidth  ) ||
        ( Top    >= 0 && Top    < sHeight ) ||
        ( Bottom >= 0 && Bottom < sHeight ) )
    {
        /* Ensure that box is drawn from top left to bottom right. */
        if( Top > Bottom )
        {
            Row = Top;
            Top = Bottom;
            Bottom = Row;
        }
        if( Left > Right )
        {
            Row = Left;
            Left = Right;
            Right = Row;
        }

        /* Draw the box or line as specified */
        Height = Bottom - Top + 1;
        Width  = Right - Left + 1;

        HB_GT_FUNC(gt_DispBegin());

        if( Height > 1 && Width > 1 &&
               Top >= 0 && Top < sHeight &&
              Left >= 0 && Left < sWidth )
        {
          HB_GT_FUNC(gt_xPutch( Top, Left, byAttr, szBox[ 0 ] )); /* Upper left corner */
        }

        Col = ( Height > 1 ? Left + 1 : Left );
        if( Col < 0 )
        {
            Width += Col;
            Col = 0;
        }
        if( Right >= sWidth )
        {
            Width -= Right - sWidth;
        }

        if( Col <= Right && Col < sWidth &&
                Top >= 0 && Top < sHeight )
        {
            HB_GT_FUNC(gt_Replicate( Top, Col, byAttr, szBox[ 1 ], Width + ( (Right - Left) > 1 ? -2 : 0 ) )); /* Top line */
        }
        if( Height > 1 &&
               (Right - Left) > 1 && Right < sWidth &&
               Top >= 0 && Top < sHeight )
        {
            HB_GT_FUNC(gt_xPutch( Top, Right, byAttr, szBox[ 2 ] )); /* Upper right corner */
        }
        if( szBox[ 8 ] && Height > 2 && Width > 2 )
        {
            for( Row = Top + 1; Row < Bottom; Row++ )
            {
                if( Row >= 0 && Row < sHeight )
                {
                    Col = Left;
                    if( Col < 0 )
                    {
                      Col = 0; /* The width was corrected earlier. */
                    }
                    else
                    {
                      HB_GT_FUNC(gt_xPutch( Row, Col++, byAttr, szBox[ 7 ] )); /* Left side */
                    }
                    HB_GT_FUNC(gt_Replicate( Row, Col, byAttr, szBox[ 8 ], Width - 2 )); /* Fill */
                    if( Right < sWidth )
                    {
                      HB_GT_FUNC(gt_xPutch( Row, Right, byAttr, szBox[ 3 ] )); /* Right side */
                    }
                }
            }
        }
        else
        {
            for( Row = ( Width > 1 ? Top + 1 : Top ); Row < ( (Right - Left ) > 1 ? Bottom : Bottom + 1 ); Row++ )
            {
                if( Row >= 0 && Row < sHeight )
                {
                    if( Left >= 0 && Left < sWidth )
                    {
                        HB_GT_FUNC(gt_xPutch( Row, Left, byAttr, szBox[ 7 ] )); /* Left side */
                    }
                    if( ( Width > 1 || Left < 0 ) && Right < sWidth )
                    {
                        HB_GT_FUNC(gt_xPutch( Row, Right, byAttr, szBox[ 3 ] )); /* Right side */
                    }
                }
            }
        }

        if( Height > 1 && Width > 1 )
        {
            if( Left >= 0 && Bottom < sHeight )
            {
                HB_GT_FUNC(gt_xPutch( Bottom, Left, byAttr, szBox[ 6 ] )); /* Bottom left corner */
            }
            Col = Left + 1;
            if( Col < 0 )
            {
                Col = 0; /* The width was corrected earlier. */
            }
            if( Col <= Right && Bottom < sHeight )
            {
                HB_GT_FUNC(gt_Replicate( Bottom, Col, byAttr, szBox[ 5 ], Width - 2 )); /* Bottom line */
            }
            if( Right < sWidth && Bottom < sHeight )
            {
                HB_GT_FUNC(gt_xPutch( Bottom, Right, byAttr, szBox[ 4 ] )); /* Bottom right corner */
            }
        }
        HB_GT_FUNC(gt_DispEnd());
        ret = 0;
    }

    return(ret);
}



/* *********************************************************************** */

//copied from gtwin
USHORT HB_GT_FUNC(gt_BoxD( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr ))
{
    return( HB_GT_FUNC(gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr )));
}

/* *********************************************************************** */
//copied from gtwin

USHORT HB_GT_FUNC(gt_BoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr ))
{
    return( HB_GT_FUNC(gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr )));
}

/* *********************************************************************** */
//copied from gtwin

USHORT HB_GT_FUNC(gt_HorizLine( SHORT Row, SHORT Left, SHORT Right, BYTE byChar, BYTE byAttr ))
{
  USHORT ret = 1;
  USHORT sWidth = HB_GT_FUNC(gt_GetScreenWidth());
  if( Row >= 0 && Row < sWidth )
  {
      if( Left < 0 )
      {
          Left = 0;
      }
      else if( Left >= sWidth )
      {
          Left = sWidth - 1;
      }
      if( Right < 0 )
      {
          Right = 0;
      }
      else if( Right >= sWidth )
      {
          Right = sWidth - 1;
      }
      if( Left < Right )
      {
          HB_GT_FUNC(gt_Replicate( Row, Left, byAttr, byChar, Right - Left + 1 ));
      }
      else
      {
          HB_GT_FUNC(gt_Replicate( Row, Right, byAttr, byChar, Left - Right + 1 ));
      }
      ret = 0;
  }
  return( ret);
}

/* *********************************************************************** */
//copied from gtwin

USHORT HB_GT_FUNC(gt_VertLine( SHORT Col, SHORT Top, SHORT Bottom, BYTE byChar, BYTE byAttr ))
{
    USHORT ret = 1;
    USHORT sWidth = HB_GT_FUNC(gt_GetScreenWidth());
    USHORT sHeight = HB_GT_FUNC(gt_GetScreenHeight());
    SHORT Row;

    if( Col >= 0 && Col < sWidth )
    {
        if( Top < 0 )
        {
            Top = 0;
        }
        else if( Top >= sHeight )
        {
            Top = sHeight - 1;
        }
        if( Bottom < 0 )
        {
            Bottom = 0;
        }
        else if( Bottom >= sHeight )
        {
            Bottom = sHeight - 1;
        }
        if( Top <= Bottom )
        {
            Row = Top;
        }
        else
        {
            Row = Bottom;
            Bottom = Top;
        }

        HB_GT_FUNC(gt_DispBegin());

        while( Row <= Bottom )
        {
            HB_GT_FUNC(gt_xPutch( Row++, Col, byAttr, byChar ));
        }
        HB_GT_FUNC(gt_DispEnd());

        ret = 0;
    }
    return( ret);
}

/* *********************************************************************** */
// like gtwin

BOOL HB_GT_FUNC(gt_Suspend())
{
  return(TRUE);
}

/* *********************************************************************** */
// like gtwin

BOOL HB_GT_FUNC(gt_Resume())
{
  return(TRUE);
}

/* *********************************************************************** */
// like gtwin

BOOL HB_GT_FUNC(gt_PreExt())
{
  return(TRUE);
}

/* *********************************************************************** */
// like gtwin

BOOL HB_GT_FUNC(gt_PostExt())
{
  return(TRUE);
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_OutStd( BYTE * pbyStr, ULONG ulLen ))
{
  hb_fsWriteLarge( s_iStdOut, ( BYTE * ) pbyStr, ulLen );
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_OutErr( BYTE * pbyStr, ULONG ulLen ))
{
  hb_fsWriteLarge( s_iStdErr, ( BYTE * ) pbyStr, ulLen );
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_ExtendedKeySupport())
{
    return(FALSE);  // Only use standard Clipper hey handling
}


/* *********************************************************************** */
int HB_GT_FUNC(gt_ReadKey( HB_inkey_enum eventmask ))
{
   int c=0;
   BOOL bKey = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

   XVT_INITIALIZE

   if ( s_wnd != NULL )
   {
      xvt_processMessages( s_wnd );
   }

   xvt_appProcess( 0 );

   if ( eventmask & ( INKEY_KEYBOARD /* | HB_INKEY_RAW | HB_INKEY_EXTENDED */ ) )
   {
      bKey = xvt_bufferDeqeueKey( s_buffer, &c );
   }

   if (! bKey  && ( eventmask & INKEY_MOVE ) > 0)
   {
      if ( s_status->lastMouseEvent == K_MOUSEMOVE )
      {
         bKey = TRUE;
         c = K_MOUSEMOVE;
         s_status->lastMouseEvent = 0;
      }
   }

   if (! bKey  && ( eventmask & INKEY_LDOWN ) > 0)
   {
      if ( s_status->lastMouseEvent == K_LBUTTONDOWN ||
           s_status->lastMouseEvent == K_LDBLCLK )
      {
         bKey = TRUE;
         c = s_status->lastMouseEvent;
         s_status->lastMouseEvent = 0;
      }
   }

   if (! bKey  && ( eventmask & INKEY_LUP ) > 0)
   {
      if ( s_status->lastMouseEvent == K_LBUTTONUP )
      {
         bKey = TRUE;
         c = s_status->lastMouseEvent;
         s_status->lastMouseEvent = 0;
      }
   }

   if (! bKey  && ( eventmask & INKEY_RDOWN ) > 0)
   {
      if ( s_status->lastMouseEvent == K_RBUTTONDOWN ||
           s_status->lastMouseEvent == K_RDBLCLK )
      {
         bKey = TRUE;
         c = s_status->lastMouseEvent;
         s_status->lastMouseEvent = 0;
      }
   }

   if (! bKey  && ( eventmask & INKEY_RUP ) > 0)
   {
      if ( s_status->lastMouseEvent == K_RBUTTONUP )
      {
         bKey = TRUE;
         c = s_status->lastMouseEvent;
         s_status->lastMouseEvent = 0;
      }
   }

   if (! bKey  && ( eventmask & INKEY_MWHEEL ) > 0)
   {
      if ( s_status->lastMouseEvent == K_MWFORWARD ||
            s_status->lastMouseEvent == K_MWBACKWARD )
      {
         bKey = TRUE;
         c = s_status->lastMouseEvent;
         s_status->lastMouseEvent = 0;
      }
   }
   return ( bKey ? c : 0);
}



/* *********************************************************************** */

void HB_GT_FUNC(gt_Tone( double dFrequency, double dDuration ))
{
   XKeyboardControl kbc;
   Display *disp;
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

   // we need our own instance of the display
   disp = XOpenDisplay( NULL );
   if ( disp )
   {
      kbc.bell_percent = 50;
      kbc.bell_pitch = dFrequency;
      kbc.bell_duration = dDuration/ 18.0 * 1000.0;

      XChangeKeyboardControl( disp,
         KBBellPercent | KBBellPitch | KBBellDuration,
         &kbc
         );
      XBell( disp, 100 );
      // this also flushes the request
      XCloseDisplay( disp );
   }
   // anyhow, perform a sleep
   usleep( kbc.bell_duration * 1000 );

}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_Init( void ))
{
   // can be done ONLY in the subprocess. GT requests are ignored.
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_Exit( void ))
{
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(mouse_IsPresent( void ))
{
   return( TRUE);
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_Show( void ))
{
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_Hide( void ))
{
}

/* *********************************************************************** */

int HB_GT_FUNC(mouse_Col( void ))
{
  return s_status->mouseCol;
}

/* *********************************************************************** */

int HB_GT_FUNC(mouse_Row( void ))
{
  return s_status->mouseRow;
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_SetPos( int iRow, int iCol ))
{
   HB_SYMBOL_UNUSED( iRow );
   HB_SYMBOL_UNUSED( iCol );
   /*s_status->mouseGotoRow = iRow;
   s_status->mouseGotoCol = iCol;*/
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(mouse_IsButtonPressed( int iButton ))
{
   if ( iButton >= s_status->mouseNumButtons || iButton < 0 )
   {
      return FALSE;
   }

   return s_status->mouseButtons[ iButton ] = TRUE;
}

/* *********************************************************************** */

int HB_GT_FUNC(mouse_CountButton( void ))
{
   return s_status->mouseNumButtons;
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight ))
{
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( iBottom );
   HB_SYMBOL_UNUSED( iRight );
}


void HB_GT_FUNC(mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight ))
{
   HB_SYMBOL_UNUSED( piTop );
   HB_SYMBOL_UNUSED( piLeft );
   HB_SYMBOL_UNUSED( piBottom );
   HB_SYMBOL_UNUSED( piRight );
}

/* ************************** Clipboard support ********************************** */

void HB_GT_FUNC( gt_GetClipboard( char *szData, ULONG *pulMaxSize ) )
{
   USHORT appMsg;
   appMsg = XVT_ICM_GETSELECTION;

   write( streamUpdate[1], &appMsg, sizeof( appMsg ) );
   // wait until the feedback is done
   xvt_appProcess( XVT_ICM_SETSELECTION );

   if ( *pulMaxSize == 0 || (unsigned) (s_clipsize) < *pulMaxSize )
   {
      *pulMaxSize = s_clipsize;
   }

   if ( *pulMaxSize != 0 )
   {
      memcpy( szData, s_clipboard, *pulMaxSize );
   }
}

void HB_GT_FUNC( gt_SetClipboard( char *szData, ULONG ulSize ) )
{
   USHORT appMsg;

   if ( s_clipboard != NULL )
   {
      hb_xfree( s_clipboard );
   }

   // send data to our application
   appMsg = XVT_ICM_SETSELECTION;
   write( streamUpdate[1], &appMsg, sizeof( appMsg ) );
   write( streamUpdate[1], &ulSize, sizeof( ulSize ) );
   write( streamUpdate[1], szData, ulSize );

   // temporarily, copy also our data locally
   s_clipboard = (char *) hb_xgrab( ulSize +1 );
   memcpy( s_clipboard, szData, ulSize );
   s_clipboard[ ulSize ] = '\0';
   s_clipsize = ulSize;
}

ULONG HB_GT_FUNC( gt_GetClipboardSize( void ) )
{
   USHORT appMsg;
   appMsg = XVT_ICM_GETSELECTION;

   write( streamUpdate[1], &appMsg, sizeof( appMsg ) );
   // wait until the feedback is done
   xvt_appProcess( XVT_ICM_SETSELECTION );

   return s_clipsize;
}

/* *********************************************************************** */

int HB_GT_FUNC( gt_info(int iMsgType, BOOL bUpdate, int iParam, void *vpParam ) )
{
   int iOldValue;
   USHORT appMsg;

   HB_SYMBOL_UNUSED( vpParam );

   switch ( iMsgType )
   {
      case GTI_ISGRAPHIC:
      return (int) TRUE;

      case GTI_SCREENWIDTH:
         // if the window is already open, and is reachable, this is an
         // easy task.
         if ( s_wnd != 0 )
         {
            iOldValue =  s_wnd->width;
         }
         // TODO: else, use our local image.
         else
         {
            iOldValue = s_buffer->cols * XVT_DEFAULT_FONT_WIDTH;
         }

         if ( bUpdate )
         {
            //send requst to change width
            hb_gtSetMode( s_buffer->rows, iParam / XVT_DEFAULT_FONT_WIDTH );
            if ( s_wnd != 0 || s_childPid != 0 )
            {
               appMsg = XVT_ICM_RESIZE;
               write( streamUpdate[1], &appMsg, sizeof( appMsg ) );
            }
         }
      return iOldValue;

      case GTI_SCREENHEIGHT:
         // if the window is already open, and is reachable, this is an
         // easy task.
         if ( s_wnd != 0 )
         {
            iOldValue =  s_wnd->height;
         }
         // else, use our local image.
         else
         {
            iOldValue = s_buffer->rows * XVT_DEFAULT_FONT_HEIGHT;
         }

         if ( bUpdate )
         {
            //send requst to change width
            hb_gtSetMode( iParam / XVT_DEFAULT_FONT_HEIGHT, s_buffer->cols );
            if ( s_wnd != 0 || s_childPid != 0 )
            {
               appMsg = XVT_ICM_RESIZE;
               write( streamUpdate[1], &appMsg, sizeof( appMsg ) );
            }
         }
      return iOldValue;

      case GTI_SCREENDEPTH:
         return -1;

      case GTI_FONTSIZE:
         if ( s_wnd != 0 )
         {
            iOldValue =  s_wnd->fontHeight;
         }
         // else, use our local image.
         else
         {
            iOldValue = s_fontReqSize;
         }

         if ( bUpdate )
         {
            //send requst to change width
            if ( s_wnd != 0 || s_childPid != 0 )
            {
               s_fontReqSize = iParam;

               appMsg = XVT_ICM_FONTSIZE;
               write( streamUpdate[1], &appMsg, sizeof( appMsg ) );
               appMsg = (USHORT) s_fontReqWidth;
               write( streamUpdate[1], &appMsg, sizeof( appMsg ) );
               appMsg = (USHORT) s_fontReqSize;
               write( streamUpdate[1], &appMsg, sizeof( appMsg ) );
            }
         }
      return iOldValue;

      case GTI_FONTWIDTH:
         if ( s_wnd != 0 )
         {
            iOldValue =  s_wnd->fontWidth;
         }
         // else, use our local image.
         else
         {
            iOldValue = s_fontReqWidth;
         }

         if ( bUpdate )
         {
            s_fontReqWidth = iParam;
         }
      return iOldValue;

      case GTI_DESKTOPDEPTH:
         return -1;

      case GTI_DESKTOPWIDTH:
      case GTI_DESKTOPHEIGHT:
      case GTI_DESKTOPCOLS:
      case GTI_DESKTOPROWS:
      {
         Display *dpy;
         Window wnd;
         XWindowAttributes wndAttr;

         dpy = XOpenDisplay( NULL );
         if ( dpy )
         {
            wnd = XDefaultRootWindow( dpy );
            if ( wnd )
            {
               XGetWindowAttributes( dpy, wnd, &wndAttr );
               switch( iMsgType )
               {
                  case GTI_DESKTOPWIDTH:
                     return wndAttr.width;
                  case GTI_DESKTOPHEIGHT:
                     return wndAttr.height;
                  case GTI_DESKTOPCOLS:
                     return wndAttr.width / s_fontReqWidth;
                  case GTI_DESKTOPROWS:
                     return wndAttr.height / s_fontReqSize;
               }
            }
         }
      }
      return -1;

      case GTI_INPUTFD:
         return fileno( stdin );

      case GTI_OUTPUTFD:
         return fileno( stdout );

      case GTI_ERRORFD:
         return fileno( stderr );
   }
   // DEFAULT: there's something wrong if we are here.
   return -1;
}


#ifdef HB_MULTI_GT

static void HB_GT_FUNC(gtFnInit( PHB_GT_FUNCS gt_funcs ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gtFnInit(%p)", gt_funcs));

    gt_funcs->Init                  = HB_GT_FUNC( gt_Init );
    gt_funcs->Exit                  = HB_GT_FUNC( gt_Exit );
    gt_funcs->GetScreenWidth        = HB_GT_FUNC( gt_GetScreenWidth );
    gt_funcs->GetScreenHeight       = HB_GT_FUNC( gt_GetScreenHeight );
    gt_funcs->Col                   = HB_GT_FUNC( gt_Col );
    gt_funcs->Row                   = HB_GT_FUNC( gt_Row );
    gt_funcs->SetPos                = HB_GT_FUNC( gt_SetPos );
    gt_funcs->AdjustPos             = HB_GT_FUNC( gt_AdjustPos );
    gt_funcs->IsColor               = HB_GT_FUNC( gt_IsColor );
    gt_funcs->GetCursorStyle        = HB_GT_FUNC( gt_GetCursorStyle );
    gt_funcs->SetCursorStyle        = HB_GT_FUNC( gt_SetCursorStyle );
    gt_funcs->DispBegin             = HB_GT_FUNC( gt_DispBegin );
    gt_funcs->DispEnd               = HB_GT_FUNC( gt_DispEnd );
    gt_funcs->DispCount             = HB_GT_FUNC( gt_DispCount );
    gt_funcs->Puts                  = HB_GT_FUNC( gt_Puts );
    gt_funcs->Replicate             = HB_GT_FUNC( gt_Replicate );
    gt_funcs->RectSize              = HB_GT_FUNC( gt_RectSize );
    gt_funcs->GetText               = HB_GT_FUNC( gt_GetText );
    gt_funcs->PutText               = HB_GT_FUNC( gt_PutText );
    gt_funcs->SetAttribute          = HB_GT_FUNC( gt_SetAttribute );
    gt_funcs->Scroll                = HB_GT_FUNC( gt_Scroll );
    gt_funcs->SetMode               = HB_GT_FUNC( gt_SetMode );
    gt_funcs->GetBlink              = HB_GT_FUNC( gt_GetBlink );
    gt_funcs->SetBlink              = HB_GT_FUNC( gt_SetBlink );
    gt_funcs->Version               = HB_GT_FUNC( gt_Version );
    gt_funcs->Box                   = HB_GT_FUNC( gt_Box );
    gt_funcs->BoxD                  = HB_GT_FUNC( gt_BoxD );
    gt_funcs->BoxS                  = HB_GT_FUNC( gt_BoxS );
    gt_funcs->HorizLine             = HB_GT_FUNC( gt_HorizLine );
    gt_funcs->VertLine              = HB_GT_FUNC( gt_VertLine );
    gt_funcs->Suspend               = HB_GT_FUNC( gt_Suspend );
    gt_funcs->Resume                = HB_GT_FUNC( gt_Resume );
    gt_funcs->PreExt                = HB_GT_FUNC( gt_PreExt );
    gt_funcs->PostExt               = HB_GT_FUNC( gt_PostExt );
    gt_funcs->OutStd                = HB_GT_FUNC( gt_OutStd );
    gt_funcs->OutErr                = HB_GT_FUNC( gt_OutErr );
    gt_funcs->Tone                  = HB_GT_FUNC( gt_Tone );
    gt_funcs->ExtendedKeySupport    = HB_GT_FUNC( gt_ExtendedKeySupport );
    gt_funcs->ReadKey               = HB_GT_FUNC( gt_ReadKey );
    gt_funcs->info                  = HB_GT_FUNC( gt_info );
    gt_funcs->SetClipboard          = HB_GT_FUNC( gt_SetClipboard );
    gt_funcs->GetClipboard          = HB_GT_FUNC( gt_GetClipboard );
    gt_funcs->GetClipboardSize      = HB_GT_FUNC( gt_GetClipboardSize );
}

/* ********************************************************************** */

static void HB_GT_FUNC(mouseFnInit( PHB_GT_FUNCS gt_funcs ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_mouseFnInit(%p)", gt_funcs));

    gt_funcs->mouse_Init            = HB_GT_FUNC( mouse_Init );
    gt_funcs->mouse_Exit            = HB_GT_FUNC( mouse_Exit );
    gt_funcs->mouse_IsPresent       = HB_GT_FUNC( mouse_IsPresent );
    gt_funcs->mouse_Show            = HB_GT_FUNC( mouse_Show );
    gt_funcs->mouse_Hide            = HB_GT_FUNC( mouse_Hide );
    gt_funcs->mouse_Col             = HB_GT_FUNC( mouse_Col );
    gt_funcs->mouse_Row             = HB_GT_FUNC( mouse_Row );
    gt_funcs->mouse_SetPos          = HB_GT_FUNC( mouse_SetPos );
    gt_funcs->mouse_IsButtonPressed = HB_GT_FUNC( mouse_IsButtonPressed );
    gt_funcs->mouse_CountButton     = HB_GT_FUNC( mouse_CountButton );
    gt_funcs->mouse_SetBounds       = HB_GT_FUNC( mouse_SetBounds );
    gt_funcs->mouse_GetBounds       = HB_GT_FUNC( mouse_GetBounds );
}
/* ********************************************************************** */



/* ********************************************************************** */

static HB_GT_INIT gtInit = { HB_GT_DRVNAME( HB_GT_NAME ),
                             HB_GT_FUNC(gtFnInit), HB_GT_FUNC(mouseFnInit) };

HB_GT_ANNOUNCE( HB_GT_NAME );

HB_CALL_ON_STARTUP_BEGIN( _hb_startup_gt_Init_ )
   hb_gtRegister( &gtInit );
HB_CALL_ON_STARTUP_END( _hb_startup_gt_Init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_startup_gt_Init_
#endif

#endif  /* HB_MULTI_GT */


/* *********************************************************************** */

