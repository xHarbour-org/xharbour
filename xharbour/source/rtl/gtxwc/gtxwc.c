/*
 * $Id: gtxwc.c,v 1.19 2006/03/01 15:45:34 druzus Exp $
 */

/*
 * Xharbour Project source code:
 * XWindow Terminal
 * Copyright 2003 - Giancarlo Niccolai <antispam /at/ niccolai.ws>
 * Copyright 2004 - Przemys³aw Czerpak <druzus /at/ priv.onet.pl>
 *
 * based on
 *   Video subsystem for Win32 using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemys³aw Czerpak <druzus@polbox.com>
 *   Video subsystem for Win32 compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemys³aw Czerpak <druzus@polbox.com>
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

/* #define XVT_DEBUG */
#include "gtxwc.h"

/* mouse button mapping into Clipper keycodes */
static int mousePressKeys[ XVT_MAX_BUTTONS ]    = { K_LBUTTONDOWN, K_MBUTTONDOWN, K_RBUTTONDOWN, K_MWFORWARD, K_MWBACKWARD };
static int mouseReleaseKeys[ XVT_MAX_BUTTONS ]  = { K_LBUTTONUP,   K_MBUTTONUP,   K_RBUTTONUP   };
static int mouseDblPressKeys[ XVT_MAX_BUTTONS ] = { K_LDBLCLK,     K_MDBLCLK,     K_RDBLCLK    , K_MWFORWARD, K_MWBACKWARD };

typedef struct tag_ClipKeyCode {
    int key;
    int alt_key;
    int ctrl_key;
    int shift_key;
} ClipKeyCode;

/* The tables below are indexed by internal key value,
 * It cause that we don't have to make any linear scans
 * to access proper ClipKeyCode entry
 */
static const ClipKeyCode stdKeyTab[CLIP_STDKEY_COUNT] = {
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
    {'/',    K_CTRL_QUESTION,           127,         0}, /*  47 */
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
    {'?',                  0, K_CTRL_QUESTION,       0}, /*  63 */
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
};

static const ClipKeyCode extKeyTab[CLIP_EXTKEY_COUNT] = {
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
    {K_INS,        K_ALT_INS,    K_CTRL_INS,  K_SH_INS}, /*  16 */
    {K_DEL,        K_ALT_DEL,    K_CTRL_DEL,  K_SH_DEL}, /*  17 */
    {K_HOME,      K_ALT_HOME,   K_CTRL_HOME, K_SH_HOME}, /*  18 */
    {K_END,        K_ALT_END,    K_CTRL_END,  K_SH_END}, /*  19 */
    {K_PGUP,      K_ALT_PGUP,   K_CTRL_PGUP, K_SH_PGUP}, /*  20 */
    {K_PGDN,      K_ALT_PGDN,   K_CTRL_PGDN, K_SH_PGDN}, /*  21 */
    {K_BS,          K_ALT_BS,           127,   K_SH_BS}, /*  22 */
    {K_TAB,        K_ALT_TAB,    K_CTRL_TAB,  K_SH_TAB}, /*  23 */
    {K_ESC,        K_ALT_ESC,         K_ESC,         0}, /*  24 */
    {K_ENTER,    K_ALT_ENTER,  K_CTRL_ENTER,         0}, /*  25 */
    {K_ENTER,   KP_ALT_ENTER,  K_CTRL_ENTER,         0}, /*  26 */
    {KP_CENTER,            0,     KP_CTRL_5,         0}, /*  27 */
    {0,                    0, K_CTRL_PRTSCR,         0}, /*  28 */
    {0,                    0, HB_BREAK_FLAG,         0}  /*  29 */
};

#ifdef HB_CDP_SUPPORT_OFF
static const UnixBoxChar boxTranslate[] ={
   {  16, HB_GTXVG_ARROW_R},
   {  17, HB_GTXVG_ARROW_L},
   {  30, HB_GTXVG_ARROW_U},
   {  31, HB_GTXVG_ARROW_D},
   { 176, HB_GTXVG_FILLER1},
   { 177, HB_GTXVG_FILLER2},
   { 178, HB_GTXVG_FILLER3},
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
   { 219, HB_GTXVG_FULL},
   { 220, HB_GTXVG_FULL_B},
   { 221, HB_GTXVG_FULL_L},
   { 222, HB_GTXVG_FULL_R},
   { 223, HB_GTXVG_FULL_T},
   { 254, HB_GTXVG_SQUARE}
};
#define XVT_BOX_CHARS   (sizeof(boxTranslate) / sizeof(UnixBoxChar))
#endif

/* these are standard PC console colors in RGB */
static char *rgb_colors[] = {
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

static Atom s_atomDelWin;
static Atom s_atomTimestamp;
static Atom s_atomAtom;
static Atom s_atomInteger;
static Atom s_atomString;
static Atom s_atomUTF8String;
static Atom s_atomPrimary;
static Atom s_atomSecondary;
static Atom s_atomClipboard;
static Atom s_atomTargets;
static Atom s_atomCutBuffer0;
static Atom s_atomText;
static Atom s_atomCompoundText;


typedef struct tag_rect
{
   int top;
   int left;
   USHORT right;
   USHORT bottom;
} RECT;

typedef struct tag_modifiers
{
   BOOL bCtrl;
   BOOL bAlt;
   BOOL bAltGr;
   BOOL bShift;
} MODIFIERS;

typedef struct tag_x_wnddef
{
   Display *dpy;
   Window window;
   GC gc;
   Colormap colors;
   HB_GT_PIXELTYPE pixels[16];
   Pixmap pm;
   Drawable drw;

   void (*evt_callback)(void);

   /* is main window initialized */
   BOOL fInit;

   /* window size in character cells */
   USHORT cols;
   USHORT rows;

   /* window size in pixels */
   USHORT width;
   USHORT height;

   /* Set to true when Windows is resized */
   BOOL fWinResize;
   USHORT newWidth;
   USHORT newHeight;

   /* window title */
   char *szTitle;
   BOOL fDspTitle;

   /* used font informations */
   XFontStruct *xfs;
   char *szFontName;
   char *szFontWeight;
   char *szFontEncoding;
   int fontHeight;
   int fontWidth;
   /* if font has bad metric then try to fix it and display only single
      char at cell position at once */
   BOOL fFixMetric;
   /* if font has bad size and doesn't clear background cell we can
      try to fix it and clear background drawing rectangle before
      displaying font */
   BOOL fClearBkg;
   /* if TRUE then BOX characters will be drawn by GTXVT instead of
      using build in font ones */
   BOOL fDrawBox;

   /* CodePage support */
   PHB_CODEPAGE hostCDP;
   /* PHB_CODEPAGE outCDP; */
   PHB_CODEPAGE inCDP;

   /* disp{begin,end} counter */
   USHORT uiDispCount;

   /* current cursor and color settings */
   int col;
   int row;
   int cursorType;
   
   /* last cursor position and shape */
   int lastCursorCol;
   int lastCursorRow;
   int lastCursorType;

   /* Mouse informations */
   int mouseCol;
   int mouseRow;
   int mouseGotoCol;
   int mouseGotoRow;
   int mouseNumButtons;
   int mouseButtonsState;
   BYTE mouseButtonsMap[XVT_MAX_BUTTONS];
   Time mouseButtonsTime[XVT_MAX_BUTTONS];

   /* internal screen buffers */
   BYTE *pChars;   /* it will be changed to USHORT when we will have internal unicode support */
   BYTE *pColors;
   /* BYTE *pAttribs; */ /* additional atribs for future use like BOLD, UNDERSCORE, ... */
   ULONG *pCurr;   /* current screen contents (attr<<24)|(color<<16)|char */

   /* character translation table, it changes BYTES in pChars into UNICODE or graphs primitives */
   XVT_CharTrans charTrans[256];

   BOOL fInvalidChr;
   RECT rInvalidChr;

   BOOL fInvalidPts;
   RECT rInvalidPts;

   /* Keyboard buffer */
   int keyBuffPointer;
   int keyBuffNO;
   int KeyBuff[ XVT_CHAR_QUEUE_SIZE ];
   MODIFIERS keyModifiers;

   /* Clipboard buffer */
   unsigned char * ClipboardData;
   ULONG ClipboardSize;
   Atom ClipboardRequest;
   Time ClipboardTime;
   BOOL ClipboardOwner;
   BOOL ClipboardRcvd;

   /* Keep last event time */
   Time lastEventTime;

} XWND_DEF, *PXWND_DEF;

/******************************************************************/

static void hb_xvt_gtProcessMessages( PXWND_DEF wnd );
static void hb_xvt_gtInitialize( PXWND_DEF wnd );
static void hb_xvt_gtInvalidatePts( PXWND_DEF wnd, int left, int top, int right, int bottom );
static void hb_xvt_gtSetWindowTitle( PXWND_DEF wnd, char * title );

/************************ globals ********************************/

static int s_iStdIn, s_iStdOut, s_iStdErr;

static PXWND_DEF s_wnd = NULL;

static BOOL s_forceRefresh;
static BOOL s_cursorState = TRUE;
static ULONG s_cursorBlinkRate = 350;
static ULONG s_cursorStateTime = 0;

static int s_updateMode = XVT_SYNC_UPDATE;
//static int s_updateMode = XVT_ASYNC_UPDATE;
static int s_iUpdateCounter;

#define _GetScreenWidth()  (s_wnd->cols)
#define _GetScreenHeight() (s_wnd->rows)


/* *********************************************************************** */

static int s_errorHandler( Display *dpy, XErrorEvent *e )
{
   char errorText[1024];

   sprintf( errorText, "%s", "Xlib error: " );
   XGetErrorText( dpy, e->error_code,
                  errorText + strlen( errorText ),
                  sizeof(errorText) - strlen( errorText ) );
   hb_errInternal( 10001, errorText, "", "" );
   return 1;
}

/* *********************************************************************** */

static void hb_xvt_sigHandler( int iSig )
{
   HB_SYMBOL_UNUSED( iSig );

   if ( s_updateMode == XVT_ASYNC_UPDATE && s_wnd && s_wnd->fInit )
   {
      if( s_iUpdateCounter )
         --s_iUpdateCounter;
      hb_xvt_gtProcessMessages( s_wnd );
   }
}

/* *********************************************************************** */

static void hb_xvt_gtDisable( void )
{
   if ( s_updateMode == XVT_ASYNC_UPDATE )
   {
      signal( SIGALRM, SIG_IGN);
   }
}

/* *********************************************************************** */

static void hb_xvt_gtEnable( void )
{
   if ( s_updateMode == XVT_ASYNC_UPDATE )
   {
      struct itimerval itv;

      signal( SIGALRM, hb_xvt_sigHandler);
      itv.it_interval.tv_sec = 0;
      itv.it_interval.tv_usec = 25000;
      itv.it_value = itv.it_interval;
      setitimer( ITIMER_REAL, &itv, NULL);
   }
}


/* *********************************************************************** */

/*
 *  functions for building character conversion and box chars shapes
 */

/* *********************************************************************** */

static BOOL hb_xvt_gtDefineBoxChar( PXWND_DEF wnd, USHORT usCh, XVT_CharTrans *bxCh )
{
   XSegment       segs[9];
   XPoint         pts[XVT_MAX_CHAR_POINTS];
   XRectangle     rect[4];
   XVT_CharType   type = CH_CHAR;
   int            size = 0;
   BOOL           inverse = FALSE;

   int cellx = wnd->fontWidth;
   int celly = wnd->fontHeight;
   int i;

   switch( usCh )
   {
      case HB_GTXVG_FILLER1:
      case HB_GTXVG_FILLER2:
      case HB_GTXVG_FILLER3:
      {
         int x, y, xx, yy, skip, start, mod;

         if ( usCh == HB_GTXVG_FILLER1 )
         {
            skip = 4;
            start = mod = 1;
         }
         else if ( usCh == HB_GTXVG_FILLER2 )
         {
            skip = 2;
            start = 0;
            mod = 1;
         }
         else
         {
            skip = 4;
            start = mod = 0;
            inverse = TRUE;
         }
         xx = yy = 0;
         for ( y = 0; y < celly; y++ )
         {
            for ( x = start + ( skip >> 1 ) * ( ( y & 1 ) ^ mod ); x < cellx; x += skip )
            {
               /* this is font size dependent, we have to add this checking
                * to at least avoid GPF for if user set very large font though
                * character definition will not be finished
                */
               if ( size >= XVT_MAX_CHAR_POINTS )
               {
                  break;
               }
               pts[size].x = x - xx;
               pts[size].y = y - yy;
               xx = x;
               yy = y;
               size++;
            }
         }
         type = CH_PTS;
         break;
      }

      case HB_GTXVG_ARROW_R:
         i = HB_MIN( ( celly >> 1 ), cellx ) - 3;
         pts[0].x = ( ( cellx - i ) >> 1 );
         pts[0].y = ( celly >> 1 ) - i;
         pts[1].x = i;
         pts[1].y = i;
         pts[2].x = -i;
         pts[2].y = i;
         size = 3;
         type = CH_POLY;
         break;

      case HB_GTXVG_ARROW_L:
         i = HB_MIN( ( celly >> 1 ), cellx ) - 3;
         pts[0].x = ( ( cellx - i ) >> 1 ) + i;
         pts[0].y = ( celly >> 1 ) - i;
         pts[1].x = - i;
         pts[1].y = i;
         pts[2].x = i;
         pts[2].y = i;
         size = 3;
         type = CH_POLY;
         break;

      case HB_GTXVG_ARROW_U:
         i = HB_MIN( celly, cellx >> 1 );
         pts[0].x = ( cellx >> 1 ) - i;
         pts[0].y = ( ( celly - i ) >> 1 ) + i;
         pts[1].x = i;
         pts[1].y = -i;
         pts[2].x = i;
         pts[2].y = i;
         size = 3;
         type = CH_POLY;
         break;

      case HB_GTXVG_ARROW_D:
         i = HB_MIN( celly, cellx >> 1 );
         pts[0].x = ( cellx >> 1 ) - i;
         pts[0].y = ( ( celly - i ) >> 1 );
         pts[1].x = i;
         pts[1].y = i;
         pts[2].x = i;
         pts[2].y = -i;
         size = 3;
         type = CH_POLY;
         break;

      case HB_GTXVG_FULL:
         inverse = TRUE;
         type = CH_NONE;
         break;

      case HB_GTXVG_FULL_B:
         inverse = TRUE;
      case HB_GTXVG_FULL_T:
         rect[0].x = 0;
         rect[0].y = 0;
         rect[0].width = cellx;
         rect[0].height = celly/2;
         size = 1;
         type = CH_RECT;
         break;

      case HB_GTXVG_FULL_R:
         inverse = TRUE;
      case HB_GTXVG_FULL_L:
         rect[0].x = 0;
         rect[0].y = 0;
         rect[0].width = cellx/2;
         rect[0].height = celly;
         size = 1;
         type = CH_RECT;
         break;

      case HB_GTXVT_SNG_LT:
         segs[0].x1 = cellx/2;
         segs[0].y1 = celly - 1;
         segs[0].x2 = cellx/2;
         segs[0].y2 = celly/2;

         segs[1].x1 = segs[0].x2;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = cellx - 1;
         segs[1].y2 = celly/2;

         size = 2;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_TD:
         segs[0].x1 = 0;
         segs[0].y1 = celly/2;
         segs[0].x2 = cellx - 1;
         segs[0].y2 = celly/2;

         segs[1].x1 = cellx/2;
         segs[1].y1 = segs[0].y1;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = celly - 1;

         size = 2;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_RT:
         segs[0].x1 = cellx/2;
         segs[0].y1 = celly - 1;
         segs[0].x2 = cellx/2;
         segs[0].y2 = celly/2;

         segs[1].x1 = segs[0].x2;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = 0;
         segs[1].y2 = celly/2;

         size = 2;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_LB:
         segs[0].x1 = cellx/2;
         segs[0].y1 = 0;
         segs[0].x2 = cellx/2;
         segs[0].y2 = celly/2;

         segs[1].x1 = segs[0].x2;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = cellx - 1;
         segs[1].y2 = celly/2;

         size = 2;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_BU:
         segs[0].x1 = cellx/2;
         segs[0].y1 = 0;
         segs[0].x2 = cellx/2;
         segs[0].y2 = celly/2;

         segs[1].x1 = 0;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = cellx - 1;
         segs[1].y2 = celly/2;

         size = 2;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_RB:
         segs[0].x1 = cellx/2;
         segs[0].y1 = 0;
         segs[0].x2 = cellx/2;
         segs[0].y2 = celly/2;

         segs[1].x1 = segs[0].x2;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = 0;
         segs[1].y2 = celly/2;

         size = 2;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_VL:
         segs[0].x1 = cellx/2;
         segs[0].y1 = 0;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly - 1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = celly/2;
         segs[1].x2 = cellx - 1;
         segs[1].y2 = segs[1].y1;

         size = 2;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_VR:
         segs[0].x1 = cellx/2;
         segs[0].y1 = 0;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly - 1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = celly/2;
         segs[1].x2 = 0;
         segs[1].y2 = segs[1].y1;

         size = 2;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_CRS:
         segs[0].x1 = cellx/2;
         segs[0].y1 = 0;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly - 1;

         segs[1].x1 = 0;
         segs[1].y1 = celly/2;
         segs[1].x2 = cellx - 1;
         segs[1].y2 = segs[1].y1;

         size = 2;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_HOR:
         segs[0].x1 = 0;
         segs[0].y1 = celly/2;
         segs[0].x2 = cellx - 1;
         segs[0].y2 = segs[0].y1;

         size = 1;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_VRT:
         segs[0].x1 = cellx/2;
         segs[0].y1 = 0;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly - 1;

         size = 1;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_LT:
         segs[0].x1 = cellx/2-1;
         segs[0].y1 = celly - 1;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly/2-1;

         segs[1].x1 = segs[0].x2;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = cellx - 1;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = cellx/2+1;
         segs[2].y1 = celly - 1;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = celly/2+1;

         segs[3].x1 = segs[2].x2;
         segs[3].y1 = segs[2].y2;
         segs[3].x2 = cellx - 1;
         segs[3].y2 = segs[2].y2;

         size = 4;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_TD:
         segs[0].x1 = 0;
         segs[0].y1 = celly/2-1;
         segs[0].x2 = cellx - 1;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = celly/2+1;
         segs[1].x2 = cellx/2-1;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = cellx/2+1;
         segs[2].y1 = celly/2+1;
         segs[2].x2 = cellx - 1;
         segs[2].y2 = segs[2].y1;

         segs[3].x1 = segs[1].x2;
         segs[3].y1 = segs[1].y1;
         segs[3].x2 = segs[1].x2;
         segs[3].y2 = celly - 1;

         segs[4].x1 = segs[2].x1;
         segs[4].y1 = segs[2].y1;
         segs[4].x2 = segs[2].x1;
         segs[4].y2 = celly - 1;

         size = 5;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_RT:
         segs[0].x1 = cellx/2-1;
         segs[0].y1 = celly - 1;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly/2+1;

         segs[1].x1 = segs[0].x2;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = 0;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = cellx/2+1;
         segs[2].y1 = celly - 1;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = celly/2-1;

         segs[3].x1 = segs[2].x2;
         segs[3].y1 = segs[2].y2;
         segs[3].x2 = 0;
         segs[3].y2 = segs[2].y2;

         size = 4;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_LB:
         segs[0].x1 = cellx/2-1;
         segs[0].y1 = 0;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly/2+1;

         segs[1].x1 = segs[0].x2;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = cellx - 1;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = cellx/2+1;
         segs[2].y1 = 0;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = celly/2-1;

         segs[3].x1 = segs[2].x2;
         segs[3].y1 = segs[2].y2;
         segs[3].x2 = cellx - 1;
         segs[3].y2 = segs[2].y2;

         size = 4;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_BU:
         segs[0].x1 = 0;
         segs[0].y1 = celly/2+1;
         segs[0].x2 = cellx - 1;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = celly/2-1;
         segs[1].x2 = cellx/2-1;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = cellx/2+1;
         segs[2].y1 = celly/2-1;
         segs[2].x2 = cellx - 1;
         segs[2].y2 = segs[2].y1;

         segs[3].x1 = segs[1].x2;
         segs[3].y1 = segs[1].y1;
         segs[3].x2 = segs[1].x2;
         segs[3].y2 = 0;

         segs[4].x1 = segs[2].x1;
         segs[4].y1 = segs[2].y1;
         segs[4].x2 = segs[2].x1;
         segs[4].y2 = 0;

         size = 5;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_RB:
         segs[0].x1 = cellx/2-1;
         segs[0].y1 = 0;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly/2-1;

         segs[1].x1 = segs[0].x2;
         segs[1].y1 = segs[0].y2;
         segs[1].x2 = 0;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = cellx/2+1;
         segs[2].y1 = 0;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = celly/2+1;

         segs[3].x1 = segs[2].x2;
         segs[3].y1 = segs[2].y2;
         segs[3].x2 = 0;
         segs[3].y2 = segs[2].y2;

         size = 4;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_VL:
         segs[0].x1 = cellx/2-1;
         segs[0].y1 = 0;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly - 1;

         segs[1].x1 = cellx/2+1;
         segs[1].y1 = 0;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = celly/2-1;

         segs[2].x1 = segs[1].x1;
         segs[2].y1 = celly/2+1;
         segs[2].x2 = segs[1].x1;
         segs[2].y2 = celly - 1;

         segs[3].x1 = segs[1].x1;
         segs[3].y1 = segs[1].y2;
         segs[3].x2 = cellx - 1;
         segs[3].y2 = segs[3].y1;

         segs[4].x1 = segs[2].x1;
         segs[4].y1 = segs[2].y1;
         segs[4].x2 = cellx - 1;
         segs[4].y2 = segs[2].y1;

         size = 5;
         type = CH_SEG;
         break;


      case HB_GTXVT_DBL_VR:
         segs[0].x1 = cellx/2+1;
         segs[0].y1 = 0;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly - 1;

         segs[1].x1 = cellx/2-1;
         segs[1].y1 = 0;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = celly/2-1;

         segs[2].x1 = segs[1].x1;
         segs[2].y1 = celly/2+1;
         segs[2].x2 = segs[1].x1;
         segs[2].y2 = celly - 1;

         segs[3].x1 = segs[1].x1;
         segs[3].y1 = segs[1].y2;
         segs[3].x2 = 0;
         segs[3].y2 = segs[3].y1;

         segs[4].x1 = segs[2].x1;
         segs[4].y1 = segs[2].y1;
         segs[4].x2 = 0;
         segs[4].y2 = segs[2].y1;

         size = 5;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_CRS:
         segs[0].x1 = cellx/2-1;
         segs[0].y1 = 0;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly/2-1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = celly/2+1;
         segs[1].x2 = segs[0].x1;
         segs[1].y2 = celly - 1;

         segs[2].x1 = segs[0].x1;
         segs[2].y1 = segs[0].y2;
         segs[2].x2 = 0;
         segs[2].y2 = segs[2].y1;

         segs[3].x1 = segs[1].x1;
         segs[3].y1 = segs[1].y1;
         segs[3].x2 = 0;
         segs[3].y2 = segs[1].y1;

         segs[4].x1 = cellx/2+1;
         segs[4].y1 = 0;
         segs[4].x2 = segs[4].x1;
         segs[4].y2 = celly/2-1;

         segs[5].x1 = segs[4].x1;
         segs[5].y1 = celly/2+1;
         segs[5].x2 = segs[4].x1;
         segs[5].y2 = celly - 1;

         segs[6].x1 = segs[4].x1;
         segs[6].y1 = segs[4].y2;
         segs[6].x2 = cellx - 1;
         segs[6].y2 = segs[6].y1;

         segs[7].x1 = segs[5].x1;
         segs[7].y1 = segs[5].y1;
         segs[7].x2 = cellx - 1;
         segs[7].y2 = segs[5].y1;

         size = 8;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_HOR:
         segs[0].x1 = 0;
         segs[0].y1 = celly/2+1;
         segs[0].x2 = cellx - 1;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = 0;
         segs[1].y1 = celly/2-1;
         segs[1].x2 = cellx - 1;
         segs[1].y2 = segs[1].y1;

         size = 2;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_VRT:
         segs[0].x1 = cellx/2-1;
         segs[0].y1 = 0;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly - 1;

         segs[1].x1 = cellx/2+1;
         segs[1].y1 = 0;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = celly - 1;

         size = 2;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_L_DBL_T:
         segs[0].x1 = cellx/2;
         segs[0].y1 = celly/2-1;
         segs[0].x2 = cellx - 1;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = celly/2+1;
         segs[1].x2 = segs[0].x2;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = cellx/2;
         segs[2].y1 = celly/2-1;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = celly - 1;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_T_DBL_D:
         segs[0].x1 = 0;
         segs[0].y1 = celly/2;
         segs[0].x2 = cellx - 1;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = cellx/2-1;
         segs[1].y1 = celly/2;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = celly - 1;

         segs[2].x1 = cellx/2+1;
         segs[2].y1 = segs[1].y1;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = segs[1].y2;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_R_DBL_T:
         segs[0].x1 = 0;
         segs[0].y1 = celly/2;
         segs[0].x2 = cellx/2+1;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = cellx/2+1;
         segs[1].y1 = segs[0].y1;
         segs[1].x2 = segs[0].x2;
         segs[1].y2 = celly - 1;

         segs[2].x1 = cellx/2-1;
         segs[2].y1 = celly/2;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = celly - 1;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_L_DBL_B:
         segs[0].x1 = cellx/2;
         segs[0].y1 = celly/2-1;
         segs[0].x2 = cellx - 1;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = celly/2+1;
         segs[1].x2 = segs[0].x2;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = cellx/2;
         segs[2].y1 = 0;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = celly/2+1;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_B_DBL_U:
         segs[0].x1 = 0;
         segs[0].y1 = celly/2;
         segs[0].x2 = cellx - 1;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = cellx/2-1;
         segs[1].y1 = 0;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = celly/2;

         segs[2].x1 = cellx/2+1;
         segs[2].y1 = 0;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = celly/2;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_R_DBL_B:
         segs[0].x1 = 0;
         segs[0].y1 = celly/2;
         segs[0].x2 = cellx/2+1;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = cellx/2+1;
         segs[1].y1 = 0;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = celly/2;

         segs[2].x1 = cellx/2-1;
         segs[2].y1 = 0;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = celly/2;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_V_DBL_L:
         segs[0].x1 = cellx/2;
         segs[0].y1 = 0;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly - 1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = celly/2-1;
         segs[1].x2 = cellx - 1;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = segs[0].x1;
         segs[2].y1 = celly/2+1;
         segs[2].x2 = segs[1].x2;
         segs[2].y2 = segs[2].y1;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_V_DBL_R:
         segs[0].x1 = cellx/2;
         segs[0].y1 = 0;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly - 1;

         segs[1].x1 = 0;
         segs[1].y1 = celly/2-1;
         segs[1].x2 = segs[0].x1;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = 0;
         segs[2].y1 = celly/2+1;
         segs[2].x2 = segs[0].x1;
         segs[2].y2 = segs[2].y1;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVT_SNG_DBL_CRS:
         segs[0].x1 = cellx/2;
         segs[0].y1 = 0;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly - 1;

         segs[1].x1 = 0;
         segs[1].y1 = celly/2-1;
         segs[1].x2 = cellx - 1;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = 0;
         segs[2].y1 = celly/2+1;
         segs[2].x2 = segs[1].x2;
         segs[2].y2 = segs[2].y1;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_L_SNG_T:
         segs[0].x1 = cellx/2-1;
         segs[0].y1 = celly/2;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly - 1;

         segs[1].x1 = cellx/2 + 1;
         segs[1].y1 = segs[0].y1;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = segs[0].x1;
         segs[2].y1 = segs[0].y1;
         segs[2].x2 = cellx - 1;
         segs[2].y2 = segs[0].y1;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_T_SNG_D:
         segs[0].x1 = 0;
         segs[0].y1 = celly/2-1;
         segs[0].x2 = cellx - 1;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = celly/2+1;
         segs[1].x2 = segs[0].x2;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = cellx/2;
         segs[2].y1 = celly/2+1;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = celly - 1;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_R_SNG_T:
         segs[0].x1 = 0;
         segs[0].y1 = celly/2-1;
         segs[0].x2 = cellx/2;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = celly/2+1;
         segs[1].x2 = segs[0].x2;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = cellx/2;
         segs[2].y1 = celly/2-1;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = celly - 1;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_L_SNG_B:
         segs[0].x1 = cellx/2-1;
         segs[0].y1 = 0;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly/2;

         segs[1].x1 = cellx/2+1;
         segs[1].y1 = segs[0].y1;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = cellx/2-1;
         segs[2].y1 = segs[0].y2;
         segs[2].x2 = cellx - 1;
         segs[2].y2 = segs[0].y2;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_B_SNG_U:
         segs[0].x1 = 0;
         segs[0].y1 = celly/2-1;
         segs[0].x2 = cellx - 1;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = celly/2+1;
         segs[1].x2 = segs[0].x2;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = cellx/2;
         segs[2].y1 = 0;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = celly/2-1;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_R_SNG_B:
         segs[0].x1 = 0;
         segs[0].y1 = celly/2-1;
         segs[0].x2 = cellx/2;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = celly/2+1;
         segs[1].x2 = segs[0].x2;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = cellx/2;
         segs[2].y1 = 0;
         segs[2].x2 = segs[2].x1;
         segs[2].y2 = celly/2+1;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_V_SNG_L:
         segs[0].x1 = cellx/2-1;
         segs[0].y1 = 0;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly - 1;

         segs[1].x1 = cellx/2+1;
         segs[1].y1 = segs[0].y1;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = segs[1].x1;
         segs[2].y1 = celly/2;
         segs[2].x2 = cellx - 1;
         segs[2].y2 = segs[2].y1;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_V_SNG_R:
         segs[0].x1 = cellx/2-1;
         segs[0].y1 = 0;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly - 1;

         segs[1].x1 = cellx/2+1;
         segs[1].y1 = segs[0].y1;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = 0;
         segs[2].y1 = celly/2;
         segs[2].x2 = segs[0].x1;
         segs[2].y2 = segs[2].y1;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVT_DBL_SNG_CRS:
         segs[0].x1 = cellx/2-1;
         segs[0].y1 = 0;
         segs[0].x2 = segs[0].x1;
         segs[0].y2 = celly - 1;

         segs[1].x1 = cellx/2+1;
         segs[1].y1 = segs[0].y1;
         segs[1].x2 = segs[1].x1;
         segs[1].y2 = segs[0].y2;

         segs[2].x1 = 0;
         segs[2].y1 = celly/2;
         segs[2].x2 = cellx - 1;
         segs[2].y2 = segs[2].y1;

         size = 3;
         type = CH_SEG;
         break;

      case HB_GTXVG_SQUARE:
         rect[0].x = ( ( cellx - rect[0].width ) >> 1 );
         rect[0].y = ( ( celly - rect[0].height ) >> 1 );
         rect[0].width = cellx - HB_MAX(cellx >> 2, 2);
         rect[0].height = rect[0].width;
         size = 1;
         type = CH_RECT;
         break;
/*
      default:
         rect[0].x = 1;
         rect[0].y = 1;
         rect[0].width = cellx - 2;
         rect[0].height = celly - 2;
         size = 1;
         type = CH_RECT;
         break;
*/
   }

   if ( size > 0 )
   {
      bxCh->type = type;
      bxCh->u.ch16 = usCh;
      bxCh->size = size;
      bxCh->inverse = inverse;
      switch ( type )
      {
         case CH_SEG:
            bxCh->u.seg = (XSegment*) hb_xgrab( sizeof(XSegment) * size );
            memcpy( bxCh->u.seg, segs, sizeof(XSegment) * size );
            break;
         case CH_RECT:
            bxCh->u.rect = (XRectangle*) hb_xgrab( sizeof(XRectangle) * size );
            memcpy( bxCh->u.rect, rect, sizeof(XRectangle) * size );
            break;
         case CH_PTS:
         case CH_LINE:
         case CH_POLY:
            bxCh->u.pts = (XPoint*) hb_xgrab( sizeof(XPoint) * size );
            memcpy( bxCh->u.pts, pts, sizeof(XPoint) * size );
            break;
         default:
            break;
      }
      return TRUE;
   }
   return FALSE;
}

/* *********************************************************************** */

static void hb_xvt_gtDestroyCharTrans( PXWND_DEF wnd )
{
   int i;

   for ( i = 0; i < 256; i++ )
   {
      switch ( wnd->charTrans[ i ].type )
      {
         case CH_IMG:
            XDestroyImage( wnd->charTrans[ i ].u.img );
            break;
         case CH_SEG:
            hb_xfree( wnd->charTrans[ i ].u.seg );
            break;
         case CH_RECT:
            hb_xfree( wnd->charTrans[ i ].u.rect );
            break;
         case CH_PTS:
         case CH_LINE:
         case CH_POLY:
            hb_xfree( wnd->charTrans[ i ].u.pts );
            break;
         default:
            break;
      }
      memset( &wnd->charTrans[ i ], 0, sizeof( XVT_CharTrans ) );
   }
}

/* *********************************************************************** */

static void hb_xvt_gtBuildCharTrans( PXWND_DEF wnd )
{
   int i;
   USHORT usCh16;

   hb_xvt_gtDestroyCharTrans( wnd );

   for ( i = 0; i < 256; i++ )
   {
#ifndef HB_CDP_SUPPORT_OFF
      usCh16 = hb_cdpGetU16( wnd->hostCDP, TRUE, ( BYTE ) i );
#else
      int j;
      usCh16 = ( USHORT ) i;
      for ( j = 0; j < (int) XVT_BOX_CHARS; j++ )
      {
         if ( boxTranslate[j].c == usCh16 )
         {
            usCh16 = boxTranslate[j].u16;
            break;
         }
      }
#endif
      wnd->charTrans[ i ].type = CH_CHAR;
      wnd->charTrans[ i ].u.ch16 = usCh16;
      wnd->charTrans[ i ].size = 0;
      wnd->charTrans[ i ].inverse = FALSE;
      if ( wnd->fDrawBox )
      {
         hb_xvt_gtDefineBoxChar( wnd, usCh16, &wnd->charTrans[ i ] );
      }
   }
}

/* *********************************************************************** */

/*
 *  functions for handling the input queues for the mouse and keyboard
 */

/* *********************************************************************** */

static void hb_xvt_gtMouseInit( PXWND_DEF wnd )
{
   wnd->mouseNumButtons = XGetPointerMapping( wnd->dpy,
                                              wnd->mouseButtonsMap,
                                              XVT_MAX_BUTTONS );

   if ( wnd->mouseNumButtons > XVT_MAX_BUTTONS )
   {
      wnd->mouseNumButtons = XVT_MAX_BUTTONS;
   }
   wnd->mouseButtonsState = 0;
   wnd->mouseGotoCol = -1;
   wnd->mouseGotoRow = -1;
}

static void hb_xvt_gtAddCharToInputQueue( PXWND_DEF wnd, int keyCode )
{
   if ( wnd->keyBuffNO < XVT_CHAR_QUEUE_SIZE )
   {
      wnd->KeyBuff[ wnd->keyBuffPointer++ ] = keyCode ;
      if ( wnd->keyBuffPointer == XVT_CHAR_QUEUE_SIZE )
      {
         wnd->keyBuffPointer = 0;
      }
      wnd->keyBuffNO++;
   }
}

/* *********************************************************************** */

static BOOL hb_xvt_gtGetCharFromInputQueue( PXWND_DEF wnd, int *keyCode )
{
   *keyCode = 0;
   if ( wnd->keyBuffNO > 0 )
   {
      int index = wnd->keyBuffPointer - wnd->keyBuffNO;
      if ( index < 0 )
      {
         index += XVT_CHAR_QUEUE_SIZE;
      }
      *keyCode = wnd->KeyBuff[ index ];
      wnd->keyBuffNO--;
      return TRUE;
   }
   return FALSE;
}

/* *********************************************************************** */

static int hb_xvt_gtLastCharInInputQueue( PXWND_DEF wnd )
{
   if ( wnd->keyBuffNO > 0 )
   {
      int index = wnd->keyBuffPointer - 1;
      if ( index < 0 )
      {
         index += XVT_CHAR_QUEUE_SIZE;
      }
      return wnd->KeyBuff[ index ];
   }
   return 0;
}

/* *********************************************************************** */

static void hb_xvt_gtTranslateKey( PXWND_DEF wnd, int key )
{
   const ClipKeyCode *clipKey = NULL;

   if ( key >= K_SPACE && key <= K_CTRL_BS )
   {
      clipKey = &stdKeyTab[ key - K_SPACE ];
   }
   else if ( key > 0 && key < K_SPACE && wnd->keyModifiers.bCtrl )
   {
      clipKey = &stdKeyTab[ key + '@' ];
   }
   else if ( XVT_IS_EXTKEY( key ) )
   {
      clipKey = &extKeyTab[ XVT_CLR_KEYMASK( key ) ];
   }
   if ( clipKey != NULL )
   {
      if( wnd->keyModifiers.bAlt )
      {
         key = clipKey->alt_key;
      }
      else if( wnd->keyModifiers.bCtrl )
      {
         key = clipKey->ctrl_key;
      }
      else if( wnd->keyModifiers.bShift )
      {
         key = clipKey->shift_key;
      }
      else
      {
         key = clipKey->key;
      }
   }
   if ( key != 0 )
   {
      hb_xvt_gtAddCharToInputQueue( wnd, key );
   }
}

/* *********************************************************************** */

static void hb_xvt_processKey( PXWND_DEF wnd, XKeyEvent *evt)
{
   unsigned char buf[5];
   KeySym outISO, out = XLookupKeysym( evt, 0 );
   int ikey = 0, n, i;

#ifdef XVT_DEBUG
   n = XLookupString( evt , ( char * ) buf, sizeof(buf), &outISO, NULL );
   buf[HB_MAX(n,0)] = '\0';
   printf( "KeySym=%lx, keySymISO=%lx, keystr=[%s]\r\n", out, outISO, buf); fflush(stdout);
#endif

   /* First look for keys which should be processed before XLookupKeysym */
   switch( out )
   {
      /* First of all, let's scan for special codes */
      case XK_Shift_L:
      case XK_Shift_R:
         wnd->keyModifiers.bShift = TRUE;
         return;

      case XK_Control_L:
      case XK_Control_R:
         wnd->keyModifiers.bCtrl = TRUE;
         return;

      case XK_Meta_L:
      case XK_Alt_L:
         wnd->keyModifiers.bAlt = TRUE;
         return;

      case XK_Meta_R:
      case XK_Alt_R:
         wnd->keyModifiers.bAltGr = TRUE;
         return;

      /* Then we scan for movement */
      case XK_Left:
         ikey = EXKEY_LEFT;
         break;
      case XK_Right:
         ikey = EXKEY_RIGHT ;
         break;
      case XK_Up:
         ikey = EXKEY_UP ;
         break;
      case XK_Down:
         ikey = EXKEY_DOWN ;
         break;
      //case XK_Begin: case XK_KP_Begin:
      case XK_Home:
         ikey = EXKEY_HOME ;
         break;
      case XK_End:
         ikey = EXKEY_END ;
         break;
      case XK_Page_Up:
         ikey = EXKEY_PGUP ;
         break;
      case XK_Page_Down:
         ikey = EXKEY_PGDN ;
         break;

      /* Special cursor operations */
      case XK_Delete:
         ikey = EXKEY_DEL;
         break;
      case XK_Insert:
         ikey = EXKEY_INS;
         break;
      case XK_BackSpace:
         ikey = EXKEY_BS;
         break;
      case XK_Tab:
         ikey = EXKEY_TAB;
         break;
      case XK_Linefeed:
      case XK_Return:
         ikey = EXKEY_ENTER;
         break;
      case XK_KP_Enter:
         ikey = EXKEY_KPENTER;
         break;
      case XK_Escape:
         ikey = EXKEY_ESC;
         break;

      /* then we scan for function keys */
      case XK_F1:
         ikey = EXKEY_F1;
         break;
      case XK_F2:
         ikey = EXKEY_F2;
         break;
      case XK_F3:
         ikey = EXKEY_F3;
         break;
      case XK_F4:
         ikey = EXKEY_F4;
         break;
      case XK_F5:
         ikey = EXKEY_F5;
         break;
      case XK_F6:
         ikey = EXKEY_F6;
         break;
      case XK_F7:
         ikey = EXKEY_F7;
         break;
      case XK_F8:
         ikey = EXKEY_F8;
         break;
      case XK_F9:
         ikey = EXKEY_F9;
         break;
      case XK_F10:
         ikey = EXKEY_F10;
         break;
      case XK_F11:
         ikey = EXKEY_F11;
         break;
      case XK_F12:
         ikey = EXKEY_F12;
         break;

      /* Keys with special meanings to clipper */
      case XK_Pause:
         ikey = EXKEY_PAUSE;
         break;
      case XK_Print:
         ikey = EXKEY_PRTSCR;
         break;
   }
   if ( ikey )
   {
      hb_xvt_gtTranslateKey( wnd, ikey );
      return;
   }

   /* First check if there is no string bound with with a key, because
      we not check all modifiers in all possible keyboards */
   if ( ( n = XLookupString( evt, ( char * ) buf, sizeof(buf), &outISO, NULL ) ) <= 0 )
   {
      /*
       * This is a temporary hack for Latin-x input see gt_SetKeyCP
       */
      if ( outISO >= 0x0100 && outISO <= 0x0fff && ( outISO & 0x80 ) == 0x80 )
      {
         buf[0] = (BYTE) (outISO & 0xff);
         n = 1;
      }
   }
   if ( n > 0 )
   {
      if ( wnd->inCDP && wnd->hostCDP && wnd->inCDP != wnd->hostCDP )
      {
         hb_cdpnTranslate( (char *) buf, wnd->inCDP, wnd->hostCDP, n );
      }
#ifdef XVT_DEBUG
      buf[n] = '\0';
      printf( "keySymISO=%lx keystr=[%s]\r\n", outISO, buf); fflush(stdout);
#endif
      for ( i = 0; i < n; i++ )
      {
         if ( wnd->keyModifiers.bAlt || wnd->keyModifiers.bCtrl )
         {
            hb_xvt_gtTranslateKey( wnd, buf[i] );
         }
         else
         {
            hb_xvt_gtAddCharToInputQueue( wnd, buf[i] );
         }
      }
      return;
   }

   /* look for special keys if they haven't been processed by XLookupString,
      mostly keypad keys */
   switch( out )
   {
      case XK_KP_Left:
         ikey = EXKEY_LEFT;
         break;
      case XK_KP_Right:
         ikey = EXKEY_RIGHT ;
         break;
      case XK_KP_Up:
         ikey = EXKEY_UP ;
         break;
      case XK_KP_Down:
         ikey = EXKEY_DOWN ;
         break;
      case XK_KP_Home:
         ikey = EXKEY_HOME ;
         break;
      case XK_KP_End:
         ikey = EXKEY_END ;
         break;
      case XK_KP_Page_Up:
         ikey = EXKEY_PGUP ;
         break;
      case XK_KP_Page_Down:
         ikey = EXKEY_PGDN ;
         break;
      case XK_KP_Begin:
      case XK_KP_5:
         ikey = EXKEY_CENTER;
         break;
      case XK_KP_Insert:
         ikey = EXKEY_INS;
         break;
      case XK_KP_Delete:
         ikey = EXKEY_DEL;
         break;
      case XK_KP_Enter:
         ikey = EXKEY_KPENTER;
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
   if ( ikey )
   {
      hb_xvt_gtTranslateKey( wnd, ikey );
   }
}

/* *********************************************************************** */

static void hb_xvt_gtWndProc( PXWND_DEF wnd, XEvent *evt )
{
   KeySym out;

   switch (evt->type)
   {
      case Expose:
#ifdef XVT_DEBUG
         printf("Event: Expose\r\n"); fflush(stdout);
#endif
         hb_xvt_gtInvalidatePts( wnd,
                                 evt->xexpose.x , evt->xexpose.y,
                                 evt->xexpose.x + evt->xexpose.width,
                                 evt->xexpose.y + evt->xexpose.height );
         break;

      case NoExpose:
#ifdef XVT_DEBUG
         printf("Event: NoExpose\r\n"); fflush(stdout);
#endif
         break;

      case KeyPress:
#ifdef XVT_DEBUG
         printf("Event: KeyPress\r\n"); fflush(stdout);
#endif
         if( evt->xkey.time != CurrentTime )
            wnd->lastEventTime = evt->xkey.time;
         hb_xvt_processKey( wnd, &evt->xkey );
         break;

      case KeyRelease:
#ifdef XVT_DEBUG
         printf("Event: KeyRelease\r\n"); fflush(stdout);
#endif
         if( evt->xkey.time != CurrentTime )
            wnd->lastEventTime = evt->xkey.time;
         out = XLookupKeysym( &evt->xkey, 0 );
         switch( out )
         {
            // First of all, let's scan for special codes
            case XK_Shift_L: case XK_Shift_R:
               wnd->keyModifiers.bShift = FALSE;
               return;

            case XK_Control_L: case XK_Control_R:
               wnd->keyModifiers.bCtrl = FALSE;
               return;

            case XK_Meta_L: case XK_Alt_L:
               wnd->keyModifiers.bAlt = FALSE;
               return;

            case XK_Meta_R: case XK_Alt_R:
               wnd->keyModifiers.bAltGr = FALSE;
               return;
         }
         break;

      case MotionNotify:
#ifdef XVT_DEBUG
         printf("Event: MotionNotify\r\n"); fflush(stdout);
#endif
         if( evt->xmotion.time != CurrentTime )
            wnd->lastEventTime = evt->xmotion.time;

         wnd->mouseCol = evt->xmotion.x / wnd->fontWidth;
         wnd->mouseRow = evt->xmotion.y / wnd->fontHeight;
         if ( hb_xvt_gtLastCharInInputQueue( wnd ) != K_MOUSEMOVE )
         {
            hb_xvt_gtAddCharToInputQueue( wnd, K_MOUSEMOVE );
         }
         break;

      case ButtonPress:
      case ButtonRelease:
      {
         int button = evt->xbutton.button - 1;

#ifdef XVT_DEBUG
         printf("Event: %s, button=%d\r\n", evt->type == ButtonPress ? "ButtonPress" : "ButtonRelease", button); fflush(stdout);
#endif
         if( evt->xbutton.time != CurrentTime )
            wnd->lastEventTime = evt->xbutton.time;
         if ( button >= 0 && button < XVT_MAX_BUTTONS )
         {
            button = wnd->mouseButtonsMap[ button ] - 1;
         }
         if ( button >= 0 && button < wnd->mouseNumButtons )
         {
            int key = 0;

            if ( evt->type == ButtonPress )
            {
               Time evtTime = evt->xbutton.time;

               if ( evtTime - wnd->mouseButtonsTime[ button ] < XVT_DBLCLK_DELAY )
               {
                  key = mouseDblPressKeys[ button ];
               }
               else
               {
                  key = mousePressKeys[ button ];
               }
               wnd->mouseButtonsState |= 1 << button;
               wnd->mouseButtonsTime[ button ] = evtTime;
            }
            else
            {
               key = mouseReleaseKeys[ button ];
               wnd->mouseButtonsState &= ~(1 << button);
            }
            if ( key != 0 )
            {
               hb_xvt_gtAddCharToInputQueue( wnd, key );
            }
         }
         break;
      }

      case MappingNotify:
#ifdef XVT_DEBUG
         printf("Event: MappingNotify\r\n"); fflush(stdout);
#endif
         XRefreshKeyboardMapping( &evt->xmapping );
         break;

      case FocusIn:
#ifdef XVT_DEBUG
         printf("Event: FocusIn\r\n"); fflush(stdout);
#endif
         XRefreshKeyboardMapping( &evt->xmapping );
         wnd->keyModifiers.bCtrl  =
         wnd->keyModifiers.bAlt   =
         wnd->keyModifiers.bAltGr =
         wnd->keyModifiers.bShift = FALSE;
         break;

      case FocusOut:
#ifdef XVT_DEBUG
         printf("Event: FocusOut\r\n"); fflush(stdout);
#endif
         break;

      case ConfigureNotify:
#ifdef XVT_DEBUG
         printf("Event: ConfigureNotify\r\n"); fflush(stdout);
#endif
         wnd->newWidth  = evt->xconfigure.width;
         wnd->newHeight = evt->xconfigure.height;
         wnd->fWinResize = TRUE;
         break;

      case ClientMessage:
#ifdef XVT_DEBUG
         printf("Event: ClientMessage:%ld (%s)\r\n", evt->xclient.data.l[ 0 ], XGetAtomName(wnd->dpy, (Atom) evt->xclient.data.l[ 0 ])); fflush(stdout);
#endif
         if( ( Atom ) evt->xclient.data.l[ 0 ] == s_atomDelWin )
         {
            hb_vmRequestQuit();
         }
         break;

      case SelectionNotify:
      {
         Atom aNextRequest = None;
#ifdef XVT_DEBUG
         printf("Event: SelectionNotify: selection=%ld (%s), property=%ld (%s), target=%ld (%s) => %ld (%s)\r\n",
                     evt->xselection.selection,
                     evt->xselection.selection == None ? "None" : XGetAtomName(wnd->dpy, evt->xselection.selection),
                     evt->xselection.property,
                     evt->xselection.property == None ? "None" : XGetAtomName(wnd->dpy, evt->xselection.property),
                     evt->xselection.target,
                     evt->xselection.target == None ? "None" : XGetAtomName(wnd->dpy, evt->xselection.target),
                     wnd->ClipboardRequest,
                     wnd->ClipboardRequest == None ? "None" : XGetAtomName(wnd->dpy, wnd->ClipboardRequest)); fflush(stdout);
#endif
         if( evt->xselection.property != None )
         {
            XTextProperty text;
            unsigned long nItem;

            if( XGetTextProperty( wnd->dpy, wnd->window, &text,
                                  evt->xselection.property ) != 0 )
            {
               if( evt->xselection.target == s_atomUTF8String && text.format == 8 )
               {
#ifdef XVT_DEBUG
                  fprintf(stdout, "UTF8String='%s'\r\n", text.value); fflush(stdout);
#endif
                  nItem = hb_cdpUTF8StringLength( text.value, text.nitems );
                  if( wnd->ClipboardData != NULL )
                     hb_xfree( wnd->ClipboardData );
                  wnd->ClipboardData = ( unsigned char * ) hb_xgrab( nItem + 1 );
                  wnd->ClipboardSize = nItem;
                  hb_cdpUTF8ToStrn( wnd->hostCDP, text.value, text.nitems,
                                    wnd->ClipboardData, nItem + 1 );
                  wnd->ClipboardTime = evt->xselection.time;
                  wnd->ClipboardRcvd = TRUE;
               }
               else if( evt->xselection.target == s_atomString && text.format == 8 )
               {
#ifdef XVT_DEBUG
                  printf("String='%s'\r\n", text.value); fflush(stdout);
#endif
                  if( wnd->ClipboardData != NULL )
                     hb_xfree( wnd->ClipboardData );
                  wnd->ClipboardData = ( unsigned char * ) hb_xgrab( text.nitems + 1 );
                  memcpy( wnd->ClipboardData, text.value, text.nitems );
                  if( wnd->inCDP && wnd->hostCDP && wnd->inCDP != wnd->hostCDP )
                     hb_cdpnTranslate( ( char * ) wnd->ClipboardData, wnd->inCDP, wnd->hostCDP, text.nitems );
                  wnd->ClipboardData[ text.nitems ] = '\0';
                  wnd->ClipboardSize = text.nitems;
                  wnd->ClipboardTime = evt->xselection.time;
                  wnd->ClipboardRcvd = TRUE;
               }
               else if( evt->xselection.target == s_atomTargets && text.format == 32 )
               {
                  Atom aValue;
#ifdef XVT_DEBUG
                  printf("text.nitems=%ld, text.format=%d\r\n", text.nitems, text.format); fflush(stdout);
#endif
                  for( nItem = 0; nItem < text.nitems; ++nItem )
                  {
                     aValue = ( ( unsigned int * ) text.value )[ nItem ];
                     if( aValue == s_atomUTF8String )
                        aNextRequest = s_atomUTF8String;
                     else if( aValue == s_atomString && aNextRequest == None )
                        aNextRequest = s_atomString;
#ifdef XVT_DEBUG
                     printf("%ld, %8lx (%s)\r\n", nItem, aValue, XGetAtomName(wnd->dpy, aValue));fflush(stdout);
#endif
                  }
               }
            }
         }
         else if( wnd->ClipboardRequest == s_atomTargets )
         {
            aNextRequest = s_atomUTF8String;
         }
         else if( wnd->ClipboardRequest == s_atomUTF8String )
         {
            aNextRequest = s_atomString;
         }
         wnd->ClipboardRequest = aNextRequest;
         break;
      }

      case SelectionRequest:
      {
         XSelectionRequestEvent * req = &evt->xselectionrequest;
         XEvent respond;

#ifdef XVT_DEBUG
         printf("Event: SelectionRequest: %ld (%s)\r\n", req->target,
                        XGetAtomName(wnd->dpy, req->target)); fflush(stdout);
#endif
         respond.xselection.property = req->property;

         if ( req->target == s_atomTimestamp )
         {
            XChangeProperty( wnd->dpy, req->requestor, req->property,
                             s_atomInteger, 8 * sizeof( Time ), PropModeReplace,
                             ( unsigned char * ) &wnd->ClipboardTime, 1 );
         }
         else if ( req->target == s_atomTargets )
         {
            Atom aProp[] = { s_atomTimestamp, s_atomTargets,
                             s_atomString, s_atomUTF8String,
                             s_atomCompoundText, s_atomText };
            XChangeProperty( wnd->dpy, req->requestor, req->property,
                             s_atomAtom, 8 * sizeof( Atom ), PropModeReplace,
                             ( unsigned char * ) aProp, sizeof( aProp ) / sizeof( Atom ) );
         }
         else if( req->target == s_atomString )
         {
            if( wnd->inCDP && wnd->hostCDP && wnd->inCDP != wnd->hostCDP )
            {
               BYTE * pBuffer = ( BYTE * ) hb_xgrab( wnd->ClipboardSize + 1 );
               memcpy( pBuffer, wnd->ClipboardData, wnd->ClipboardSize + 1 );
               hb_cdpnTranslate( ( char * ) pBuffer, wnd->hostCDP, wnd->inCDP, wnd->ClipboardSize );
               XChangeProperty( wnd->dpy, req->requestor, req->property,
                                s_atomString, 8, PropModeReplace,
                                pBuffer, wnd->ClipboardSize );
               hb_xfree( pBuffer );
            }
            else
            {
               XChangeProperty( wnd->dpy, req->requestor, req->property,
                                s_atomString, 8, PropModeReplace,
                                wnd->ClipboardData, wnd->ClipboardSize );
            }
         }
         else if( req->target == s_atomUTF8String )
         {
            ULONG ulLen = hb_cdpStringInUTF8Length( wnd->hostCDP, FALSE, wnd->ClipboardData, wnd->ClipboardSize );
            BYTE * pBuffer = ( BYTE * ) hb_xgrab( ulLen + 1 );

            hb_cdpStrnToUTF8( wnd->hostCDP, FALSE, wnd->ClipboardData, wnd->ClipboardSize, pBuffer );
#ifdef XVT_DEBUG
            printf("SelectionRequest: (%s)->(%s) [%s]\r\n", wnd->ClipboardData, pBuffer, wnd->hostCDP->id); fflush(stdout);
#endif
            XChangeProperty( wnd->dpy, req->requestor, req->property,
                             s_atomUTF8String, 8, PropModeReplace,
                             pBuffer, ulLen );
            hb_xfree( pBuffer );
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
         respond.xselection.time = req->time;

         XSendEvent( wnd->dpy, req->requestor, 0, 0, &respond );
         break;
      }

      case SelectionClear:
#ifdef XVT_DEBUG
         printf("Event: SelectionClear\r\n"); fflush(stdout);
#endif
         wnd->ClipboardOwner = FALSE;
         break;

      case PropertyNotify:
#ifdef XVT_DEBUG
         printf("Event: PropertyNotify\r\n"); fflush(stdout);
#endif
         if( evt->xproperty.time != CurrentTime )
            wnd->lastEventTime = evt->xproperty.time;
         break;

#ifdef XVT_DEBUG
      default:
         printf("Event: #%d\r\n", evt->type); fflush(stdout);
         break;
#endif
   }
}

/* *********************************************************************** */

/*
 * functions for drawing on the virtual screen (pixmap) and window updating
 */

/* *********************************************************************** */
/* collor allocation */
static int hb_xvt_gtGetColormapSize( PXWND_DEF wnd )
{
   XVisualInfo visInfo, *visInfoPtr;
   int uiCMapSize = -1, nItems;

   visInfo.visualid = XVisualIDFromVisual( DefaultVisual( wnd->dpy,
                                           DefaultScreen( wnd->dpy ) ) );
   visInfoPtr = XGetVisualInfo( wnd->dpy, ( long ) VisualIDMask,
                                &visInfo, &nItems);
   if ( nItems >= 1 )
   {
      uiCMapSize = visInfoPtr->colormap_size;
   }
   XFree( ( char * ) visInfoPtr );

   return uiCMapSize;
}

/* *********************************************************************** */

static BOOL hb_xvt_gtAllocColor( PXWND_DEF wnd, XColor *pColor )
{
   BOOL fOK = FALSE;
   int uiCMapSize;

   if ( XAllocColor( wnd->dpy, wnd->colors, pColor ) != 0 )
   {
      /* the exact color allocated */
      fOK = TRUE;
   }
   else if ( ( uiCMapSize = hb_xvt_gtGetColormapSize( wnd ) ) > 0 )
   {
      /* try to find the best approximation of chosen color in
       * already allocated colors
       * Based on xterm "find_closest_color()" which was based on
       * Monish Shah's "find_closest_color()" for Vim 6.0, modified
       * with ideas from David Tong's "noflash" library ;-)
       */
      int     i, uiClosestColor;
      double  dDiff, dDistance, dClosestColorDist = 0;
      XColor *colorTable;
      BYTE   *checkTable;

      colorTable = ( XColor * ) hb_xgrab( uiCMapSize * sizeof( XColor ) );
      checkTable = ( BYTE * ) hb_xgrab( uiCMapSize * sizeof( BYTE ) );
      for ( i = 0; i  < uiCMapSize; i++ )
      {
         colorTable[i].pixel = (HB_GT_PIXELTYPE) i;
         checkTable[i] = FALSE;
      }
      XQueryColors ( wnd->dpy, wnd->colors, colorTable, uiCMapSize );

      do
      {
         uiClosestColor = -1;
         /*
          * Look for the color that best approximates the desired one
          * and has not been checked so far and try to allocate it.
          * If allocation fails, it must mean that the color was read-write
          * (so we can't use it, since its owner might change it) or else
          * it was already freed. Repeat until something succeeds or
          * we test all colors in given maximum of approximation.
          *
          * set the maximum for accepted approximation,
          * now we accept any valid color MAX_INT * MAX_INT * 3 < 1e20
          */
         dClosestColorDist = 1e20;
         for ( i = 0; i < uiCMapSize; i++ )
         {
            if ( ! checkTable[uiClosestColor] )
            {
               /*
                * Use Euclidean distance in RGB space, weighted by Y (of YIQ)
                * as the objective function, this accounts for differences
                * in the color sensitivity of the eye.
                */
               dDiff = 0.30 * ( ( (int) pColor->red  ) - (int) colorTable[i].red );
               dDistance = dDiff * dDiff;
               dDiff = 0.61 * ( ( (int) pColor->green) - (int) colorTable[i].green );
               dDistance += dDiff * dDiff;
               dDiff = 0.11 * ( ( (int) pColor->blue ) - (int) colorTable[i].blue );
               dDistance += dDiff * dDiff;
               if ( dDistance < dClosestColorDist )
               {
                  uiClosestColor = i;
                  dClosestColorDist = dDistance;
               }
            }
         }
         if ( uiClosestColor > 0 )
         {
            if ( XAllocColor( wnd->dpy, wnd->colors, &colorTable[uiClosestColor] ) != 0 )
            {
               *pColor = colorTable[uiClosestColor];
               fOK = TRUE;
               break;
            }
            checkTable[uiClosestColor] = TRUE;
         }
      }
      while ( uiClosestColor > 0 );

      hb_xfree( colorTable );
      hb_xfree( checkTable );
   }

   return fOK;
}

/* *********************************************************************** */

/* Draws the GT graphical objects */
static void xvt_windowRepaintGraphical( PXWND_DEF wnd, int x1, int y1, int x2, int y2 )
{
   HB_GT_GOBJECT *pObj;
   XColor color;

   // Set clipping region for X
   /* TODO: this is wrong and has to be fixed */
   XRectangle cliprect;

   cliprect.x = x1;
   cliprect.y = y1;
   cliprect.width = x2 - x1 + 1;
   cliprect.height = y2 - y1 + 1;

   XSetClipRectangles( wnd->dpy, wnd->gc, 0, 0, &cliprect, 1, YXBanded );

   pObj = hb_gt_gobjects;
   while ( pObj )
   {
      /* Check if pObj boundaries are inside the area to be updated */
//      if ( hb_gtGobjectInside( pObj, x1, y1, x2, y2 ) )
      {
         color.red = pObj->color.usRed;
         color.green = pObj->color.usGreen;
         color.blue = pObj->color.usBlue;
         color.flags = DoRed | DoGreen | DoBlue;
         /* Ignore alpha for now */
         hb_xvt_gtAllocColor( wnd, &color );
         XSetForeground( wnd->dpy, wnd->gc, color.pixel );
         XFreeColors( wnd->dpy, wnd->colors, &(color.pixel), 1, 0 );

         switch( pObj->type )
         {
            case GTO_POINT:
               XDrawPoint( wnd->dpy, wnd->drw, wnd->gc, pObj->x, pObj->y );
               break;

            case GTO_LINE:
               /* For lines, width and height represent X2, Y2 */
               XDrawLine( wnd->dpy, wnd->drw, wnd->gc,
                          pObj->x, pObj->y, pObj->width, pObj->height );
               break;

            case GTO_SQUARE:
               /* For lines, width and height represent X2, Y2 */
               XDrawRectangle( wnd->dpy, wnd->drw, wnd->gc,
                               pObj->x, pObj->y, pObj->width, pObj->height );
               break;

            case GTO_RECTANGLE:
               /* For lines, width and height represent X2, Y2 */
               XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                               pObj->x, pObj->y, pObj->width, pObj->height );
               break;

            case GTO_CIRCLE:
               XDrawArc( wnd->dpy, wnd->drw, wnd->gc,
                         pObj->x, pObj->y, pObj->width, pObj->height, 0, 360*64 );
               break;

            case GTO_DISK:
               XFillArc( wnd->dpy, wnd->drw, wnd->gc,
                         pObj->x, pObj->y, pObj->width, pObj->height, 0, 360*64 );
               break;

            case GTO_TEXT:
               XDrawString( wnd->dpy, wnd->drw, wnd->gc,
                            pObj->x, pObj->y, pObj->data, pObj->data_len );
               break;
         }
      }
      pObj = pObj->next;
   }
   //XSetClipMask(wnd->dpy, wnd->gc, None);
}


/* *********************************************************************** */

static void hb_xvt_gtDrawString( PXWND_DEF wnd, int col, int row, BYTE color, USHORT *usChBuf, int len )
{
   if ( wnd->fClearBkg )
   {
      XSetForeground( wnd->dpy, wnd->gc, wnd->pixels[color >> 4] );
      XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                      col * wnd->fontWidth, row * wnd->fontHeight,
                      wnd->fontWidth * len, wnd->fontHeight );
      XSetForeground( wnd->dpy, wnd->gc, wnd->pixels[color & 0x0F] );
      XDrawString16( wnd->dpy, wnd->drw, wnd->gc,
                     col * wnd->fontWidth,
                     row * wnd->fontHeight + wnd->xfs->ascent,
                     (XChar2b *) usChBuf, len );
   }
   else
   {
      XSetBackground( wnd->dpy, wnd->gc, wnd->pixels[color >> 4] );
      XSetForeground( wnd->dpy, wnd->gc, wnd->pixels[color & 0x0F] );
      XDrawImageString16( wnd->dpy, wnd->drw, wnd->gc,
                          col * wnd->fontWidth,
                          row * wnd->fontHeight + wnd->xfs->ascent,
                          (XChar2b *) usChBuf, len );
   }
}

/* *********************************************************************** */

static ULONG hb_xvt_gtHashCurrChar( BYTE attr, BYTE color, USHORT chr )
{
   return ((ULONG)attr<<24)|((ULONG)color<<16)|(ULONG)chr;
}

/* *********************************************************************** */

static void hb_xvt_gtRepaintChar( PXWND_DEF wnd, int colStart, int rowStart, int colStop, int rowStop )
{
   USHORT irow, icol, index, startCol = 0, len, basex, basey, nsize;
   BYTE oldColor = 0, color;
   USHORT usCh16, usChBuf[XVT_MAX_COLS];
   ULONG ulCurr = 0xFFFFFFFFL;
   int i;

#ifdef XVT_DEBUG
   printf( "Repaint(%d,%d,%d,%d)[%dx%d]\r\n", rowStart, colStart, rowStop, colStop, wnd->fontHeight, wnd->fontWidth );fflush(stdout);
#endif

   if ( rowStop >= wnd->rows ) rowStop = wnd->rows-1;
   if ( colStop >= wnd->cols ) colStop = wnd->cols-1;
   if ( colStart < 0 ) colStart = 0;
   if ( rowStart < 0 ) rowStart = 0;

   for ( irow = rowStart; irow <= rowStop; irow++ )
   {
      icol = colStart;
      index = icol +  irow * wnd->cols;
      len = 0;
      /* attribute may change mid line...
       * so buffer up text with same attrib, and output it
       * then do next section with same attrib, etc
       */
      while (icol <= colStop)
      {
         usCh16 = wnd->pChars[index];
         color = wnd->pColors[index];
         /*
         attr = wnd->pAttr[index];
         ulCurr = hb_xvt_gtHashCurrChar( attr, color, usCh16 );
         */
         ulCurr = hb_xvt_gtHashCurrChar( 0, color, usCh16 );
         if ( wnd->charTrans[ usCh16 ].inverse )
         {
            color = ( color << 4 ) | ( color >> 4 );
         }
         if ( len > 0 && ( wnd->charTrans[ usCh16 ].type != CH_CHAR ||
                           color != oldColor || ulCurr == wnd->pCurr[index] ) )
         {
            hb_xvt_gtDrawString( wnd, startCol, irow, oldColor, usChBuf, len );
            len = 0;
         }
         if ( wnd->pCurr[index] != ulCurr )
         {
            switch ( wnd->charTrans[ usCh16 ].type )
            {
               case CH_CHAR:
                  if ( wnd->fFixMetric )
                  {
                     HB_PUT_BE_UINT16( &usChBuf[ 0 ], wnd->charTrans[ usCh16 ].u.ch16 );
                     hb_xvt_gtDrawString( wnd, icol, irow, color, usChBuf, 1 );
                  }
                  else
                  {
                     if ( len == 0 )
                     {
                        oldColor = color;
                        startCol = icol;
                     }
                     HB_PUT_BE_UINT16( &usChBuf[ len ], wnd->charTrans[ usCh16 ].u.ch16 );
		     len++;
                  }
                  break;

               case CH_CHBX:
                  //hb_xvt_gtDrawBoxChar( wnd, icol, irow, wnd->charTrans[ usCh16 ].u.ch16, color );
                  break;

               case CH_NONE:
                  XSetForeground( wnd->dpy, wnd->gc, wnd->pixels[color >> 4] );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  icol * wnd->fontWidth, irow * wnd->fontHeight,
                                  wnd->fontWidth, wnd->fontHeight );
                  break;

               case CH_IMG:
                  XSetBackground( wnd->dpy, wnd->gc, wnd->pixels[color >> 4] );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->pixels[color & 0x0F] );
                  XPutImage( wnd->dpy, wnd->drw, wnd->gc,
                             wnd->charTrans[ usCh16 ].u.img, 0, 0,
                             icol * wnd->fontWidth, irow * wnd->fontHeight,
                             wnd->fontWidth, wnd->fontHeight );
                  break;

               case CH_PTS:
                  /* we use CoordModePrevious so only first point posiotion has to be updated */
                  wnd->charTrans[ usCh16 ].u.pts[0].x = (wnd->charTrans[ usCh16 ].u.pts[0].x % wnd->fontWidth ) + icol * wnd->fontWidth;
                  wnd->charTrans[ usCh16 ].u.pts[0].y = (wnd->charTrans[ usCh16 ].u.pts[0].y % wnd->fontHeight ) + irow * wnd->fontHeight;
                  XSetForeground( wnd->dpy, wnd->gc, wnd->pixels[color >> 4] );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  icol * wnd->fontWidth, irow * wnd->fontHeight,
                                  wnd->fontWidth, wnd->fontHeight );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->pixels[color & 0x0F] );
                  XDrawPoints( wnd->dpy, wnd->drw, wnd->gc,
                               wnd->charTrans[ usCh16 ].u.pts,
                               wnd->charTrans[ usCh16 ].size, CoordModePrevious );
                  break;

               case CH_LINE:
                  /* we use CoordModePrevious so only first point posiotion has to be updated */
                  wnd->charTrans[ usCh16 ].u.pts[0].x = (wnd->charTrans[ usCh16 ].u.pts[0].x % wnd->fontWidth ) + icol * wnd->fontWidth;
                  wnd->charTrans[ usCh16 ].u.pts[0].y = (wnd->charTrans[ usCh16 ].u.pts[0].y % wnd->fontHeight ) + irow * wnd->fontHeight;
                  XSetForeground( wnd->dpy, wnd->gc, wnd->pixels[color >> 4] );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  icol * wnd->fontWidth, irow * wnd->fontHeight,
                                  wnd->fontWidth, wnd->fontHeight );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->pixels[color & 0x0F] );
                  XDrawLines( wnd->dpy, wnd->drw, wnd->gc,
                               wnd->charTrans[ usCh16 ].u.pts,
                               wnd->charTrans[ usCh16 ].size, CoordModePrevious );
                  break;

               case CH_POLY:
                  /* we use CoordModePrevious so only first point posiotion has to be updated */
                  wnd->charTrans[ usCh16 ].u.pts[0].x = (wnd->charTrans[ usCh16 ].u.pts[0].x % wnd->fontWidth ) + icol * wnd->fontWidth;
                  wnd->charTrans[ usCh16 ].u.pts[0].y = (wnd->charTrans[ usCh16 ].u.pts[0].y % wnd->fontHeight ) + irow * wnd->fontHeight;
                  XSetForeground( wnd->dpy, wnd->gc, wnd->pixels[color >> 4] );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  icol * wnd->fontWidth, irow * wnd->fontHeight,
                                  wnd->fontWidth, wnd->fontHeight );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->pixels[color & 0x0F] );
                  XFillPolygon( wnd->dpy, wnd->drw, wnd->gc,
                                wnd->charTrans[ usCh16 ].u.pts,
                                wnd->charTrans[ usCh16 ].size,
                                Convex, CoordModePrevious );
                  break;

               case CH_SEG:
                  basex = icol * wnd->fontWidth;
                  basey = irow * wnd->fontHeight;
                  nsize = wnd->charTrans[ usCh16 ].size;
                  for ( i = 0; i < nsize; i++ )
                  {
                     wnd->charTrans[ usCh16 ].u.seg[i].x1 = (wnd->charTrans[ usCh16 ].u.seg[i].x1 % wnd->fontWidth ) + basex;
                     wnd->charTrans[ usCh16 ].u.seg[i].y1 = (wnd->charTrans[ usCh16 ].u.seg[i].y1 % wnd->fontHeight ) + basey;
                     wnd->charTrans[ usCh16 ].u.seg[i].x2 = (wnd->charTrans[ usCh16 ].u.seg[i].x2 % wnd->fontWidth ) + basex;
                     wnd->charTrans[ usCh16 ].u.seg[i].y2 = (wnd->charTrans[ usCh16 ].u.seg[i].y2 % wnd->fontHeight ) + basey;
                  }
                  XSetForeground( wnd->dpy, wnd->gc, wnd->pixels[color >> 4] );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  basex, basey, wnd->fontWidth, wnd->fontHeight );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->pixels[color & 0x0F] );
                  XDrawSegments( wnd->dpy, wnd->drw, wnd->gc,
                                 wnd->charTrans[ usCh16 ].u.seg, nsize );
                  break;

               case CH_RECT:
                  basex = icol * wnd->fontWidth;
                  basey = irow * wnd->fontHeight;
                  nsize = wnd->charTrans[ usCh16 ].size;
                  for ( i = 0; i < nsize; i++ )
                  {
                     wnd->charTrans[ usCh16 ].u.rect[i].x = (wnd->charTrans[ usCh16 ].u.rect[i].x % wnd->fontWidth ) + basex;
                     wnd->charTrans[ usCh16 ].u.rect[i].y = (wnd->charTrans[ usCh16 ].u.rect[i].y % wnd->fontHeight ) + basey;
                  }
                  XSetForeground( wnd->dpy, wnd->gc, wnd->pixels[color >> 4] );
                  XFillRectangle( wnd->dpy, wnd->drw, wnd->gc,
                                  basex, basey, wnd->fontWidth, wnd->fontHeight );
                  XSetForeground( wnd->dpy, wnd->gc, wnd->pixels[color & 0x0F] );
                  XFillRectangles( wnd->dpy, wnd->drw, wnd->gc,
                                   wnd->charTrans[ usCh16 ].u.rect, nsize );
                  break;

            }
            wnd->pCurr[index] = ulCurr;
         }
         icol++;
         index++;
      }
      if ( len > 0 )
      {
         hb_xvt_gtDrawString( wnd, startCol, irow, oldColor, usChBuf, len );
      }
   }

   /* if we've just overwritten the cursor then set it last state as SC_NONE */
   if ( wnd->lastCursorType != SC_NONE &&
        wnd->lastCursorCol >= colStart && wnd->lastCursorCol <= colStop &&
        wnd->lastCursorRow >= rowStart && wnd->lastCursorRow <= rowStop )
   {
      wnd->lastCursorType = SC_NONE;
   }

}

/* *********************************************************************** */

static void hb_xvt_gtRestoreArea( PXWND_DEF wnd,
                                  int left, int top, int right, int bottom )
{
   XCopyArea( wnd->dpy, wnd->pm, wnd->window, wnd->gc,
              wnd->fontWidth * left, wnd->fontHeight * top,
              wnd->fontWidth * (right - left + 1), wnd->fontHeight * (bottom - top + 1),
              wnd->fontWidth * left, wnd->fontHeight * top );
}

/* *********************************************************************** */

static void hb_xvt_gtInvalidateChar( PXWND_DEF wnd,
                                     int left, int top, int right, int bottom )
{
   if ( wnd->fInvalidChr == FALSE )
   {
      wnd->rInvalidChr.top    = top;
      wnd->rInvalidChr.left   = left;
      wnd->rInvalidChr.bottom = bottom;
      wnd->rInvalidChr.right  = right;
   }
   else
   {
      if ( wnd->rInvalidChr.top    > top    ) wnd->rInvalidChr.top    = top;
      if ( wnd->rInvalidChr.left   > left   ) wnd->rInvalidChr.left   = left;
      if ( wnd->rInvalidChr.right  < right  ) wnd->rInvalidChr.right  = right;
      if ( wnd->rInvalidChr.bottom < bottom ) wnd->rInvalidChr.bottom = bottom;
   }
   /*
    * It's a race condition in async update mode.
    * wnd->fInvalidChr has to be set _always_ after update region is defined
    * to make sure that screen will be updated (sometimes maybe twice but
    * it shouldn't hurt us)
    */
   wnd->fInvalidChr = TRUE;
}

/* *********************************************************************** */

static void hb_xvt_gtInvalidatePts( PXWND_DEF wnd,
                                    int left, int top, int right, int bottom )
{
   if ( ! wnd->fInvalidPts )
   {
      wnd->rInvalidPts.top    = top;
      wnd->rInvalidPts.left   = left;
      wnd->rInvalidPts.bottom = bottom;
      wnd->rInvalidPts.right  = right;
   }
   else
   {
      if ( wnd->rInvalidPts.top    > top    ) wnd->rInvalidPts.top    = top;
      if ( wnd->rInvalidPts.left   > left   ) wnd->rInvalidPts.left   = left;
      if ( wnd->rInvalidPts.right  < right  ) wnd->rInvalidPts.right  = right;
      if ( wnd->rInvalidPts.bottom < bottom ) wnd->rInvalidPts.bottom = bottom;
   }
   wnd->fInvalidPts = TRUE;
   s_forceRefresh = TRUE;
}

/* *********************************************************************** */

static void hb_xvt_gtUpdateCursor( PXWND_DEF wnd )
{
   int cursorType = s_cursorState ? wnd->cursorType : SC_NONE;

   /* must the mouse cursor be positioned? */
   if( wnd->mouseGotoRow >= 0 && wnd->mouseGotoCol >= 0 )
   {
      XWarpPointer( wnd->dpy, None, wnd->window, 0,0,0,0,
                    wnd->mouseGotoCol * wnd->fontWidth + (wnd->fontWidth>>1),
                    wnd->mouseGotoRow * wnd->fontHeight + (wnd->fontHeight>>1) );
      wnd->mouseGotoRow = -1;
   }

   /* must the screen cursor be repainted? */
   if( cursorType != wnd->lastCursorType ||
       wnd->lastCursorCol != wnd->col || wnd->lastCursorRow != wnd->row )
   {
      if( wnd->lastCursorType != SC_NONE )
      {
         /* restore character under previous cursor position */
         hb_xvt_gtRestoreArea( wnd, wnd->lastCursorCol, wnd->lastCursorRow,
                                    wnd->lastCursorCol, wnd->lastCursorRow );
      }
      if( cursorType != SC_NONE )
      {
         USHORT basex = wnd->col * wnd->fontWidth,
                basey = wnd->row * wnd->fontHeight,
                size;

         switch( cursorType )
         {
            case SC_NORMAL:
               size = 2;
               basey += wnd->fontHeight - 3;
               break;
            case SC_INSERT:
               size = ( wnd->fontHeight - 2 ) >> 1;
               basey += wnd->fontHeight - size - 1;
               break;
            case SC_SPECIAL1:
               size = wnd->fontHeight - 2;
               basey += 1;
               break;
            case SC_SPECIAL2:
               size = ( wnd->fontHeight - 2 ) >> 1;
               basey += 1;
               break;
            default:
               size = 0;
               break;
         }
         if( size )
         {
            BYTE color = wnd->pColors[ wnd->col + wnd->row * wnd->cols ];
            XSetForeground( wnd->dpy, wnd->gc, wnd->pixels[color & 0x0f] );
            XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
                            basex, basey, wnd->fontWidth, size );
         }
      }
      wnd->lastCursorType = cursorType;
      wnd->lastCursorCol = wnd->col;
      wnd->lastCursorRow = wnd->row;
   }
}

/* *********************************************************************** */

static void hb_xvt_gtUpdatePts( PXWND_DEF wnd )
{
   if ( wnd->fInvalidPts )
   {
      int left, top, right, bottom;

      left   = wnd->rInvalidPts.left;
      top    = wnd->rInvalidPts.top;
      right  = wnd->rInvalidPts.right;
      bottom = wnd->rInvalidPts.bottom;
      wnd->fInvalidPts = FALSE;
      XCopyArea( wnd->dpy, wnd->pm, wnd->window, wnd->gc,
                 left, top, right - left + 1, bottom - top + 1, left, top );
   }
}

/* *********************************************************************** */

static void hb_xvt_gtUpdateChr( PXWND_DEF wnd )
{
   if ( wnd->fInvalidChr && ( wnd->uiDispCount == 0 || s_updateMode == XVT_ASYNC_UPDATE ) )
   {
      int left, top, right, bottom;

      left   = wnd->rInvalidChr.left;
      top    = wnd->rInvalidChr.top;
      right  = wnd->rInvalidChr.right;
      bottom = wnd->rInvalidChr.bottom;
      wnd->fInvalidChr = FALSE;

      hb_xvt_gtRepaintChar( wnd, left, top, right, bottom );

      left   = wnd->fontWidth  * left;
      top    = wnd->fontHeight * top ;
      right  = wnd->fontWidth  * ( right + 1 ) - 1;
      bottom = wnd->fontHeight * ( bottom + 1 ) - 1;

      hb_xvt_gtInvalidatePts( wnd, left, top, right, bottom );

      if ( hb_gt_gobjects != NULL )
      {
         xvt_windowRepaintGraphical( wnd, left, top, right, bottom );
      }
   }
}

/* *********************************************************************** */

static void hb_xvt_gtUpdateSize( PXWND_DEF wnd )
{
   if ( wnd->fWinResize )
   {
      wnd->fWinResize = FALSE;
      hb_gtSetMode( wnd->newHeight / wnd->fontHeight,
                    wnd->newWidth / wnd->fontWidth );
   }
}

/* *********************************************************************** */

static ULONG hb_xvt_CurrentTime( void )
{
   struct timeval tv;
   gettimeofday( &tv, NULL );
   return tv.tv_sec * 1000 + tv.tv_usec / 1000;
}

/* *********************************************************************** */

static void hb_xvt_gtProcessMessages( PXWND_DEF wnd )
{
   XEvent evt;

   if( s_cursorBlinkRate == 0 )
   {
      s_cursorState = TRUE;
   }
   else
   {
      ULONG ulCurrentTime = hb_xvt_CurrentTime();

      if( ulCurrentTime - s_cursorStateTime > s_cursorBlinkRate )
      {
         s_cursorState = !s_cursorState;
         s_cursorStateTime = ulCurrentTime;
      }
   }

   hb_xvt_gtUpdateChr( wnd );
   if( wnd->fDspTitle )
   {
      wnd->fDspTitle = FALSE;
      XStoreName( wnd->dpy, wnd->window, wnd->szTitle ? wnd->szTitle : "" );
   }

   do
   {
      while( XEventsQueued( wnd->dpy, QueuedAfterFlush ) )
      {
         XNextEvent( wnd->dpy, &evt );
         hb_xvt_gtWndProc( wnd, &evt );
      }
      hb_xvt_gtUpdateSize( wnd );
      hb_xvt_gtUpdatePts( wnd );
      hb_xvt_gtUpdateCursor( wnd );
   }
   while( XEventsQueued( wnd->dpy, QueuedAfterFlush ) );
}

/* *********************************************************************** */

static void hb_xvt_gtSetSelection( PXWND_DEF wnd, char *szData, ULONG ulSize )
{
   if( wnd->ClipboardOwner && ulSize == 0 )
   {
      XSetSelectionOwner( wnd->dpy, s_atomPrimary, None, wnd->ClipboardTime );
      XSetSelectionOwner( wnd->dpy, s_atomClipboard, None, wnd->ClipboardTime );
   }

   if( wnd->ClipboardData != NULL )
      hb_xfree( wnd->ClipboardData );
   wnd->ClipboardData = ( unsigned char * ) hb_xgrab( ulSize + 1 );
   memcpy( wnd->ClipboardData, szData, ulSize );
   wnd->ClipboardData[ ulSize ] = '\0';
   wnd->ClipboardSize = ulSize;
   wnd->ClipboardTime = wnd->lastEventTime;
   wnd->ClipboardOwner = FALSE;

   if( ulSize > 0 )
   {
      XSetSelectionOwner( wnd->dpy, s_atomPrimary, wnd->window, wnd->ClipboardTime );
      if( XGetSelectionOwner( wnd->dpy, s_atomPrimary ) == wnd->window )
      {
         wnd->ClipboardOwner = TRUE;
         XSetSelectionOwner( wnd->dpy, s_atomClipboard, wnd->window, wnd->ClipboardTime );
      }
      else
      {
         char * cMsg = "Cannot set primary selection\r\n";
         hb_gt_OutErr( ( BYTE * ) cMsg, strlen( cMsg ) );
      }
   }
}

/* *********************************************************************** */

static void hb_xvt_gtRequestSelection( PXWND_DEF wnd )
{
   if( !wnd->ClipboardOwner )
   {
      Atom aRequest;
      int iConnFD = ConnectionNumber( wnd->dpy );
      struct timeval timeout;
      fd_set readfds;

      timeout.tv_sec = 3;
      timeout.tv_usec = 0;

      wnd->ClipboardRcvd = FALSE;
      wnd->ClipboardRequest = s_atomTargets;
      aRequest = None;

      if( s_updateMode == XVT_ASYNC_UPDATE )
         s_iUpdateCounter = 150;

      do
      {
         if( aRequest != wnd->ClipboardRequest )
         {
            aRequest = wnd->ClipboardRequest;
            if( aRequest == None )
               break;
#ifdef XVT_DEBUG
            printf("XConvertSelection: %ld (%s)\r\n", aRequest,
               XGetAtomName(wnd->dpy, aRequest)); fflush(stdout);
#endif
            XConvertSelection( wnd->dpy, s_atomPrimary, aRequest,
                               s_atomCutBuffer0, wnd->window, wnd->lastEventTime );
         }

         if( s_updateMode == XVT_ASYNC_UPDATE )
         {
            if( s_iUpdateCounter == 0 )
               break;
            sleep( 1 );
         }
         else
         {
            hb_xvt_gtProcessMessages( wnd );
            if( !wnd->ClipboardRcvd && wnd->ClipboardRequest == aRequest )
            {
               FD_ZERO( &readfds );
               FD_SET( iConnFD, &readfds );
               if( select( iConnFD + 1, &readfds, NULL, NULL, &timeout ) <= 0 )
                  break;
            }
         }
      }
      while( !wnd->ClipboardRcvd && wnd->ClipboardRequest != None );

      wnd->ClipboardRequest = None;
   }
}

/* *********************************************************************** */

/* *********************************************************************** */

static void hb_xvt_gtRefresh( void )
{
   static BOOL s_fRefresh = FALSE;

   hb_xvt_gtInitialize( s_wnd );

   if ( s_updateMode == XVT_SYNC_UPDATE && !s_fRefresh &&
        ( s_wnd->uiDispCount == 0 || s_forceRefresh ) )
   {
      s_fRefresh = TRUE;
      s_forceRefresh = FALSE;
      hb_xvt_gtProcessMessages( s_wnd );
      s_fRefresh = FALSE;
   }
}

/* *********************************************************************** */

static void hb_xvt_gtRefreshLate( void )
{
   if ( s_wnd->fInit )
      hb_xvt_gtRefresh();
}

/* *********************************************************************** */

static void hb_xvt_gtSetScrBuff( PXWND_DEF wnd, USHORT cols, USHORT rows )
{
   if ( rows <= XVT_MAX_ROWS && cols <= XVT_MAX_COLS &&
        ( wnd->cols != cols || wnd->rows != rows || wnd->pChars == NULL ) )
   {
      int iSize = cols * rows;

      wnd->cols = cols;
      wnd->rows = rows;

      if ( wnd->pChars == NULL )
      {
         wnd->pChars   = ( BYTE * ) hb_xgrab( iSize * sizeof( BYTE ) );
         wnd->pColors  = ( BYTE * ) hb_xgrab( iSize * sizeof( BYTE ) );
         /* wnd->pAttribs = ( BYTE * ) hb_xgrab( col * row * sizeof( BYTE ) ); */
         wnd->pCurr    = ( ULONG* ) hb_xgrab( iSize * sizeof( ULONG ) );
      }
      else
      {
         wnd->pChars   = ( BYTE * ) hb_xrealloc( wnd->pChars,   iSize * sizeof( BYTE ) );
         wnd->pColors  = ( BYTE * ) hb_xrealloc( wnd->pColors,  iSize * sizeof( BYTE ) );
         /* wnd->pAttribs = ( BYTE * ) hb_xrealloc( wnd->pAttribs, iSize * sizeof( BYTE ) ); */
         wnd->pCurr    = ( ULONG* ) hb_xrealloc( wnd->pCurr,    iSize * sizeof( ULONG ) );
      }

      memset( wnd->pChars, ' ', iSize * sizeof( BYTE ) );
      memset( wnd->pColors, XVT_DEFAULT_COLOR, iSize * sizeof( BYTE ) );
      /* memset( wnd->pAttribs, 0, col * row * sizeof( BYTE ) ); */
      memset( wnd->pCurr, 0xFFFFFFFFL, iSize * sizeof( ULONG ) );
      hb_xvt_gtInvalidateChar( wnd, 0, 0, wnd->cols - 1, wnd->rows - 1 );
   }
}

/* *********************************************************************** */

static BOOL hb_xvt_gtResize( PXWND_DEF wnd, USHORT cols, USHORT rows )
{
   if ( rows <= XVT_MAX_ROWS && cols <= XVT_MAX_COLS )
   {
      USHORT width, height;

      hb_xvt_gtSetScrBuff( wnd, cols, rows );

      width  = cols * wnd->fontWidth;
      height = rows * wnd->fontHeight;

      if ( width != wnd->width || height != wnd->height )
      {
         wnd->width = width;
         wnd->height = height;

         if ( wnd->window )
         {
            if ( wnd->pm )
            {
               XFreePixmap( wnd->dpy, wnd->pm );
            }
            wnd->pm = XCreatePixmap( wnd->dpy, wnd->window,
                                     wnd->width, wnd->height,
                                     DefaultDepth( wnd->dpy, DefaultScreen( wnd->dpy ) ) );
            wnd->drw = wnd->pm;
            XResizeWindow( wnd->dpy, wnd->window, wnd->width, wnd->height );
         }
      }

      return TRUE;
   }

   return FALSE;
}

/* *********************************************************************** */

static BOOL HB_EXPORT hb_xvt_gtSetFont( PXWND_DEF wnd, char *fontFace, char *weight, int size,  char *encoding )
{
   char fontString[150];
   XFontStruct *xfs;

//   snprintf( fontString, sizeof(fontString)-1, "-*-%s-%s-r-normal--%d-*-*-*-*-*-%s",
//             fontFace, weight, size, encoding == NULL ? "*-*" : encoding );
   snprintf( fontString, sizeof(fontString)-1, "-*-%s-%s-r-*--%d-*-*-*-*-*-%s",
             fontFace, weight, size, encoding == NULL ? "*-*" : encoding );

//   snprintf( fontString, sizeof(fontString)-1, "-rcsoft-*-*-*-*--*-*-*-*-*-*-*-*" );

   xfs = XLoadQueryFont( wnd->dpy, fontString );

   if ( xfs == NULL )
      return FALSE;

   /* a shortcut for window height and width */
   wnd->fontHeight = xfs->max_bounds.ascent + xfs->max_bounds.descent;
   wnd->fontWidth = xfs->max_bounds.rbearing - xfs->min_bounds.lbearing;
   wnd->xfs = xfs;

   return TRUE;
}

/* *********************************************************************** */

static PXWND_DEF hb_xvt_gtCreateWndDef( void )
{
   PHB_FNAME pFileName;
   PXWND_DEF wnd = ( PXWND_DEF ) hb_xgrab( sizeof( XWND_DEF ) );

   /* clear whole structure */
   memset( wnd, 0, sizeof( XWND_DEF ) );

   wnd->dpy = NULL;
   wnd->fInit = FALSE;
   hb_xvt_gtSetScrBuff( wnd, XVT_DEFAULT_COLS, XVT_DEFAULT_ROWS );
   wnd->fWinResize = FALSE;
   wnd->hostCDP = hb_cdp_page;
   wnd->cursorType = SC_NORMAL;
   
   /* Window Title */
   pFileName = hb_fsFNameSplit( hb_cmdargARGV()[0] );
   wnd->szTitle = hb_strdup( pFileName->szName );
   wnd->fDspTitle = TRUE;
   hb_xfree( pFileName );

   /* Font parameters */
   wnd->fontHeight = XVT_DEFAULT_FONT_HEIGHT;
   wnd->fontWidth = XVT_DEFAULT_FONT_WIDTH;
   wnd->szFontName = hb_strdup( XVT_DEFAULT_FONT_NAME );
   wnd->szFontWeight = hb_strdup( XVT_DEFAULT_FONT_WEIGHT );
   wnd->szFontEncoding = hb_strdup( XVT_DEFAULT_FONT_ENCODING );
   /* set GTXVT extension for chosen font */
   wnd->fFixMetric = XVT_DEFAULT_FONT_FIXMETRIC;
   wnd->fClearBkg = XVT_DEFAULT_FONT_CLRBKG;
   wnd->fDrawBox = XVT_DEFAULT_FONT_DRAWBOX;

   /* Clear keyboard buffer */
   wnd->keyBuffNO = 0;
   wnd->keyBuffPointer = 0;
   wnd->keyModifiers.bCtrl  = FALSE;
   wnd->keyModifiers.bAlt   = FALSE;
   wnd->keyModifiers.bAltGr = FALSE;
   wnd->keyModifiers.bShift = FALSE;

   wnd->lastEventTime = CurrentTime;

   return wnd;
}

/* *********************************************************************** */

static BOOL hb_xvt_gtConnectX( PXWND_DEF wnd, BOOL fExit )
{
   if ( wnd->dpy != NULL )
      return TRUE;

   /* with NULL, it gets the DISPLAY environment variable. */
   wnd->dpy = XOpenDisplay( NULL );

   if ( wnd->dpy == NULL )
   {
      if ( fExit )
      {
         /* TODO: a standard Harbour error should be generated here when
                  it can run without console!
         hb_errRT_TERM( EG_CREATE, 10001, NULL, "Can't connect to X server", 0, 0 );
         return;
         */
         hb_errInternal( 10001, "Can't connect to X server", "", "" );
      }
      return FALSE;
   }
   XSetErrorHandler( s_errorHandler );
   hb_xvt_gtMouseInit( wnd );

   /* set atom identifiers for atom names we will use */
   s_atomDelWin       = XInternAtom( wnd->dpy, "WM_DELETE_WINDOW", True );
   s_atomTimestamp    = XInternAtom( wnd->dpy, "TIMESTAMP", False );
   s_atomAtom         = XInternAtom( wnd->dpy, "ATOM", False );
   s_atomInteger      = XInternAtom( wnd->dpy, "INTEGER", False );
   s_atomString       = XInternAtom( wnd->dpy, "STRING", False );
   s_atomUTF8String   = XInternAtom( wnd->dpy, "UTF8_STRING", False );
   s_atomPrimary      = XInternAtom( wnd->dpy, "PRIMARY", False );
   s_atomSecondary    = XInternAtom( wnd->dpy, "SECONDARY", False );
   s_atomClipboard    = XInternAtom( wnd->dpy, "CLIPBOARD", False );
   s_atomTargets      = XInternAtom( wnd->dpy, "TARGETS", False );
   s_atomCutBuffer0   = XInternAtom( wnd->dpy, "CUT_BUFFER0", False );
   s_atomText         = XInternAtom( wnd->dpy, "TEXT", False );
   s_atomCompoundText = XInternAtom( wnd->dpy, "COMPOUND_TEXT", False );

   return TRUE;
}

static void hb_xvt_gtDissConnectX( PXWND_DEF wnd )
{
   hb_xvt_gtDestroyCharTrans( wnd );

   if ( wnd->dpy != NULL )
   {
      if ( wnd->pm )
      {
         XFreePixmap( wnd->dpy, wnd->pm);
         wnd->pm = 0;
      }
      if ( wnd->xfs )
      {
         XFreeFont( wnd->dpy, wnd->xfs );
         wnd->xfs = NULL;
      }
      if ( wnd->gc )
      {
         XFreeGC( wnd->dpy, wnd->gc );
         wnd->gc = 0;
      }
      XCloseDisplay( wnd->dpy );
      wnd->dpy = NULL;
   }
}

/* *********************************************************************** */

static void hb_xvt_gtDestroyWndDef( PXWND_DEF wnd )
{
   hb_xvt_gtDissConnectX( wnd );

   if ( wnd->szTitle )
      hb_xfree( wnd->szTitle );
   if ( wnd->szFontName )
      hb_xfree( wnd->szFontName );
   if ( wnd->szFontWeight )
      hb_xfree( wnd->szFontWeight );
   if ( wnd->szFontEncoding )
      hb_xfree( wnd->szFontEncoding );
   if ( wnd->pChars )
      hb_xfree( wnd->pChars );
   if ( wnd->pColors )
      hb_xfree( wnd->pColors );
/*
   if ( wnd->pAttribs )
      hb_xfree( wnd->pAttribs );
*/
   if ( wnd->pCurr )
      hb_xfree( wnd->pCurr );
   if( wnd->ClipboardData )
      hb_xfree( wnd->ClipboardData );

   hb_xfree( wnd );
}

/* *********************************************************************** */

static void hb_xvt_gtCreateWindow( PXWND_DEF wnd )
{
   int whiteColor, blackColor, i;
   XSizeHints xsize;
   XColor color, dummy;

   /* load the standard font */
   if ( ! hb_xvt_gtSetFont( wnd, wnd->szFontName, wnd->szFontWeight, wnd->fontHeight, wnd->szFontEncoding ) )
   {
      /* TODO: a standard Harbour error should be generated here when
               it can run without console!
      hb_errRT_TERM( EG_CREATE, 10001, NULL, "Can't load 'fixed' font", 0, 0 );
      return;
      */
      hb_errInternal( 10001, "Can't load 'fixed' font", "", "" );
   }

   whiteColor = WhitePixel( wnd->dpy, DefaultScreen( wnd->dpy ) );
   blackColor = BlackPixel( wnd->dpy, DefaultScreen( wnd->dpy ) );
   wnd->window = XCreateSimpleWindow( wnd->dpy, DefaultRootWindow( wnd->dpy ),
                           0, 0,
                           wnd->fontWidth * wnd->cols,
                           wnd->fontHeight * wnd->rows,
                           0, blackColor, blackColor );
   wnd->gc = XCreateGC( wnd->dpy, wnd->window, 0, NULL );

   /* Line width 2 */
   XSetLineAttributes( wnd->dpy, wnd->gc, 1, LineSolid, CapRound, JoinBevel );

   /* Set standard colors */
   wnd->colors = DefaultColormap( wnd->dpy, DefaultScreen( wnd->dpy ) );
   for ( i = 0; i < 16; i++ )
   {
      if ( XLookupColor( wnd->dpy, wnd->colors, rgb_colors[i], &dummy, &color ) != 0 )
      {
         if ( hb_xvt_gtAllocColor( wnd, &color ) )
         {
            wnd->pixels[i] = color.pixel;
#ifdef XVT_DEBUG
            printf("hb_xvt_gtAllocColor[%d]='%x/%x/%x'\r\n", i, color.red, color.green, color.blue); fflush(stdout);
#endif
         }
      }
   }

   /* build character translation table */
   hb_xvt_gtBuildCharTrans( wnd );
   XSetFont( wnd->dpy, wnd->gc, wnd->xfs->fid );
   XSelectInput( wnd->dpy, wnd->window, XVT_STD_MASK );
   XStoreName( wnd->dpy, wnd->window, wnd->szTitle );

   /* wnd->fWinResize = TRUE; */
   hb_xvt_gtResize( wnd, wnd->cols, wnd->rows );

   XMapWindow( wnd->dpy, wnd->window );
   /* ok, now we can inform the X manager about our new status: */
   /* xsize.flags = PWinGravity | PBaseSize | PResizeInc | PMinSize; */
   xsize.flags = PWinGravity | PResizeInc | PMinSize | PMaxSize;
   xsize.win_gravity = CenterGravity;
   xsize.width_inc = wnd->fontWidth;
   xsize.height_inc = wnd->fontHeight;
   xsize.min_width = wnd->fontWidth * XVT_MIN_COLS;
   xsize.min_height = wnd->fontHeight * XVT_MIN_ROWS;
   xsize.max_width = wnd->fontWidth * XVT_MAX_COLS;
   xsize.max_height = wnd->fontHeight * XVT_MAX_ROWS;
   xsize.base_width = wnd->width;
   xsize.base_height = wnd->height;
   XSetWMNormalHints( wnd->dpy, wnd->window, &xsize );

   /* Request WM to deliver destroy event */
   XSetWMProtocols( wnd->dpy, wnd->window, &s_atomDelWin, 1 );
}

/* *********************************************************************** */

static void hb_xvt_gtInitialize( PXWND_DEF wnd )
{
   if ( !wnd->fInit )
   {
      if ( hb_xvt_gtConnectX( s_wnd, TRUE ) )
      {
         hb_xvt_gtCreateWindow( wnd );
         wnd->fInit = TRUE;
         s_forceRefresh = TRUE;
         hb_xvt_gtEnable();
      }
   }
}

/* *********************************************************************** */

/* *********************************************************************** */

void HB_GT_FUNC(gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

   /* stdin && stdout && stderr */
   s_iStdIn  = iFilenoStdin;
   s_iStdOut = iFilenoStdout;
   s_iStdErr = iFilenoStderr;

   s_wnd = hb_xvt_gtCreateWndDef();
   //hb_xvt_gtConnectX( s_wnd, TRUE );
   //hb_xvt_gtInitialize( s_wnd );
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Exit( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

   hb_xvt_gtDisable();

   if ( s_wnd )
   {
      hb_xvt_gtDestroyWndDef( s_wnd );
      s_wnd = NULL;
   }
}

/* *********************************************************************** */
/* returns the number of displayable columns
 */
USHORT HB_GT_FUNC(gt_GetScreenWidth( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

   return _GetScreenWidth();
}

/* *********************************************************************** */
/* returns the number of displayable rows
 */
USHORT HB_GT_FUNC(gt_GetScreenHeight( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

   return _GetScreenHeight();
}

/* *********************************************************************** */

SHORT HB_GT_FUNC(gt_Col( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

   return s_wnd->col;
}

/* *********************************************************************** */

SHORT HB_GT_FUNC(gt_Row( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

   return s_wnd->row;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetPos( SHORT sRow, SHORT sCol, SHORT sMethod ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", sRow, sCol, sMethod));

   HB_SYMBOL_UNUSED( sMethod );

   s_wnd->col = sCol;
   s_wnd->row = sRow;

   hb_xvt_gtRefreshLate();
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_AdjustPos( BYTE * pStr, ULONG ulLen ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

   HB_SYMBOL_UNUSED( pStr );
   HB_SYMBOL_UNUSED( ulLen );

   return FALSE;
}


/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_IsColor( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

   return TRUE;
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_GetCursorStyle( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

   return s_wnd->cursorType;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetCursorStyle( USHORT usStyle ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", usStyle));

   switch( usStyle )
   {
      case SC_NONE:
      case SC_INSERT:
      case SC_SPECIAL1:
      case SC_SPECIAL2:
      case SC_NORMAL:
         s_wnd->cursorType = usStyle;
         break ;
      default:
         s_wnd->cursorType = SC_NORMAL;
         break;
   }

   hb_xvt_gtRefreshLate();
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_DispBegin( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

   s_wnd->uiDispCount++;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_DispEnd())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

   if ( s_wnd->uiDispCount > 0 )
   {
      --s_wnd->uiDispCount;
   }

   hb_xvt_gtRefreshLate();
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_DispCount())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispCount()"));

   return s_wnd->uiDispCount;
}

/* *********************************************************************** */

static void HB_GT_FUNC(gt_xPutch( USHORT iRow, USHORT iCol, BYTE bAttr, BYTE bChar ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %i)", iRow, iCol, (int) bAttr, bChar));

   if ( iRow < _GetScreenHeight() && iCol < _GetScreenWidth() )
   {
      USHORT index = iRow * _GetScreenWidth() + iCol;
      s_wnd->pChars[index] = bChar;
      s_wnd->pColors[index] = bAttr;
      hb_xvt_gtInvalidateChar(s_wnd, iCol, iRow, iCol, iRow);
   }
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Puts( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE *pbyStr, ULONG ulLen ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", usRow, usCol, (int) byAttr, pbyStr, ulLen));

   if ( ( SHORT ) ulLen > _GetScreenWidth() - usCol ) /* make sure string is not too long */
   {
      ulLen = _GetScreenWidth() - usCol;
   }
   if ( ulLen > 0 && usRow < _GetScreenHeight() && usCol < _GetScreenWidth() )
   {
      USHORT index = usRow * _GetScreenWidth() + usCol;

      memset( s_wnd->pColors + index, byAttr, ulLen );
      memcpy( s_wnd->pChars + index, pbyStr, ulLen );
      hb_xvt_gtInvalidateChar( s_wnd, usCol, usRow, usCol + ulLen, usRow );
      hb_xvt_gtRefresh();
   }
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Replicate( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE byChar, ULONG ulLen ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", usRow, usCol, byAttr, byChar, ulLen));

   if ( ( SHORT ) ulLen > _GetScreenWidth() - usCol ) /* make sure string is not too long */
   {
      ulLen = _GetScreenWidth() - usCol;
   }
   if ( ulLen > 0 && usRow < _GetScreenHeight() && usCol < _GetScreenWidth() )
   {
      USHORT index = usRow * _GetScreenWidth() + usCol;

      memset( s_wnd->pColors + index, byAttr, ulLen );
      memset( s_wnd->pChars + index, byChar, ulLen );
      hb_xvt_gtInvalidateChar( s_wnd, usCol, usRow, usCol + ulLen, usRow );
      hb_xvt_gtRefresh();
   }
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_RectSize( USHORT rows, USHORT cols ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_RectSize(%hu, %hu)", rows, cols));

   return rows * cols * 2;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_GetText( USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * pBuffer ))
{
   USHORT col;
   int index;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", top, left, bottom, right, pBuffer));

   if ( bottom >= _GetScreenHeight() )
      bottom = _GetScreenHeight() - 1;

   while( top <= bottom )
   {
      index = _GetScreenWidth() * top + left;
      for( col = left; col <= right; col++, index++ )
      {
         if ( col < _GetScreenWidth() )
         {
            *(pBuffer++) = s_wnd->pChars[index];
            *(pBuffer++) = s_wnd->pColors[index];
         }
         else
         {
            *(pBuffer++) = ' ';
            *(pBuffer++) = 0x07;
         }
      }
      top++;
   }
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_PutText( USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * pBuffer ))
{
   USHORT col, row;
   int index;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", top, left, bottom, right, pBuffer));

   if ( bottom >= _GetScreenHeight() )
      bottom = _GetScreenHeight() - 1;

   for ( row = top; row <= bottom; row++ )
   {
      index = _GetScreenWidth() * row + left;
      for( col = left; col <= right; col++, index++ )
      {
         if ( col < _GetScreenWidth() )
         {
            s_wnd->pChars[index] = *(pBuffer++);
            s_wnd->pColors[index] = *(pBuffer++);
         }
         else
         {
            pBuffer += 2;
         }
      }
   }
   hb_xvt_gtInvalidateChar( s_wnd, left, top, right, bottom);
   hb_xvt_gtRefreshLate();
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetAttribute( USHORT rowStart, USHORT colStart, USHORT rowStop, USHORT colStop, BYTE attr ))
{
   USHORT row, col;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d", rowStart, colStart, rowStop, colStop, (int) attr));

   if ( rowStop >= _GetScreenHeight() )
      rowStop = _GetScreenHeight() - 1;
   if ( colStop >= _GetScreenWidth() )
      colStop = _GetScreenWidth() - 1;

   for ( row = rowStart; row <=rowStop; row++ )
   {
      for (col = colStart; col <= colStop; col++ )
      {
         s_wnd->pColors[ row * _GetScreenWidth() + col ] = attr;
      }
   }

   hb_xvt_gtInvalidateChar( s_wnd, colStart, rowStart, colStop, rowStop );
   hb_xvt_gtRefresh();
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE byAttr, SHORT iRows, SHORT iCols ))
{
   SHORT usSaveRow, usSaveCol;
   unsigned char ucBlank[ XVT_CHAR_BUFFER ], ucBuff[ XVT_CHAR_BUFFER << 1 ];
   unsigned char * fpBlank ;
   unsigned char * fpBuff  ;
   int iLength = ( usRight - usLeft ) + 1;
   int iCount, iColOld, iColNew, iColSize;
   BOOL bMalloc = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRight, (int) byAttr, iRows, iCols));

   if (iLength > XVT_CHAR_BUFFER)
   {  // Avoid allocating memory if possible
      fpBlank = ( unsigned char * ) hb_xgrab( iLength );
      fpBuff  = ( unsigned char * ) hb_xgrab( iLength << 1 );  //*2 room for attribs
      bMalloc = TRUE;
   }
   else
   {
      fpBlank = ucBlank ;
      fpBuff  = ucBuff  ;
   }

   memset( fpBlank, hb_ctGetClearB(), iLength );

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
   hb_xvt_gtInvalidateChar( s_wnd, 0, 0, _GetScreenWidth()-1, _GetScreenHeight()-1 );
   HB_GT_FUNC(gt_DispEnd());

   if (bMalloc)
   {
      hb_xfree( fpBlank );
      hb_xfree( fpBuff );
   }

}

/* *********************************************************************** */
/* resize the (existing) window
 */
BOOL HB_GT_FUNC(gt_SetMode( USHORT row, USHORT col ))
{
   BOOL fResult = FALSE;
   int oldrows, oldcols;
   BYTE *memory;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", row, col));

   /* ignore stupid requests */
   if ( col < XVT_MIN_COLS || row < XVT_MIN_ROWS || 
        col > XVT_MAX_COLS || row > XVT_MAX_ROWS ||
        ( col == _GetScreenWidth() && row == _GetScreenHeight()) )
   {
      return FALSE;
   }

   if ( !s_wnd->fInit )
   {
      hb_xvt_gtSetScrBuff( s_wnd, col, row );
      return TRUE;
   }

   oldrows = _GetScreenHeight()-1;
   oldcols = _GetScreenWidth()-1;

   /* take current screen buffer */
   memory = (BYTE *) hb_xgrab( HB_GT_FUNC(gt_RectSize( _GetScreenHeight(), _GetScreenWidth() )) );
   HB_GT_FUNC(gt_GetText( 0, 0, oldrows, oldcols, memory ));

   hb_xvt_gtDisable();
   HB_GT_FUNC(gt_DispBegin());

   fResult = hb_xvt_gtResize( s_wnd, col, row );

   if ( fResult )
   {
      s_wnd->cols = col;
      s_wnd->rows = row;

      if ( s_wnd->col >= col )
      {
         s_wnd->col = col -1;
      }
      if ( s_wnd->row >= row )
      {
         s_wnd->row = row -1;
      }
      HB_GT_FUNC(gt_PutText( 0, 0, oldrows, oldcols, memory ));
   }
   hb_xfree( memory );

   s_forceRefresh = TRUE;
   HB_GT_FUNC(gt_DispEnd());
   hb_xvt_gtEnable();

   return fResult;
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_GetBlink())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

   return FALSE;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetBlink( BOOL bBlink ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

   HB_SYMBOL_UNUSED( bBlink );
}

/* *********************************************************************** */

char * HB_GT_FUNC(gt_Version( int iType ))
{
   if ( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "xHarbour Terminal: XWindow Console XWC";
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
   USHORT sWidth = _GetScreenWidth();
   USHORT sHeight = _GetScreenHeight();

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

      if( Col <= Right && Col < sWidth && Top >= 0 && Top < sHeight )
      {
         HB_GT_FUNC(gt_Replicate( Top, Col, byAttr, szBox[ 1 ], Width + ( (Right - Left) > 1 ? -2 : 0 ) )); /* Top line */
      }
      if( Height > 1 && ( Right - Left) > 1 &&
          Right < sWidth && Top >= 0 && Top < sHeight )
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
   return ret;
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
   return HB_GT_FUNC(gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr ));
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
  return ret;
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
   return ret;
}

/* *********************************************************************** */
// like gtwin

BOOL HB_GT_FUNC(gt_Suspend())
{
   return TRUE;
}

/* *********************************************************************** */
// like gtwin

BOOL HB_GT_FUNC(gt_Resume())
{
   return TRUE;
}

/* *********************************************************************** */
// like gtwin

BOOL HB_GT_FUNC(gt_PreExt())
{
   return TRUE;
}

/* *********************************************************************** */
// like gtwin

BOOL HB_GT_FUNC(gt_PostExt())
{
   return TRUE;
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
   return FALSE;  // Only use standard Clipper hey handling
}


/* *********************************************************************** */
int HB_GT_FUNC(gt_ReadKey( HB_inkey_enum eventmask ))
{
   int c = 0;

   /* eventmask is not intentionally checked here
    * it's bug in [x]Harbour GT API which should be fixed and gt_ReadKey
    * should not receive any parameters
    */
   HB_SYMBOL_UNUSED( eventmask ); // we ignore the eventmask!

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

   s_forceRefresh = TRUE;
   hb_xvt_gtRefreshLate();
//   hb_xvt_gtRefresh();

   if ( hb_xvt_gtGetCharFromInputQueue( s_wnd, &c ) )
   {
      return c;
   }
   return 0;
}



/* *********************************************************************** */

void HB_GT_FUNC(gt_Tone( double dFrequency, double dDuration ))
{
   XKeyboardControl XkbCtrl;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

   /* The conversion from Clipper (DOS) timer tick units to
      milliseconds is * 1000.0 / 18.2. */
   dDuration /= 18.2;

   if( s_wnd->dpy != NULL )
   {
      XkbCtrl.bell_pitch = (int) dFrequency;
      XkbCtrl.bell_duration = (int) (dDuration * 1000);
      XChangeKeyboardControl( s_wnd->dpy, KBBellPitch | KBBellDuration, &XkbCtrl );
      XBell( s_wnd->dpy, 0 );
   }
   hb_idleSleep( dDuration );
   hb_xvt_gtRefreshLate();
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_Init( void ))
{
   ;
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_Exit( void ))
{
   ;
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(mouse_IsPresent( void ))
{
   return s_wnd->mouseNumButtons > 0;
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_Show( void ))
{
   ;
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_Hide( void ))
{
   ;
}

/* *********************************************************************** */

int HB_GT_FUNC(mouse_Col( void ))
{
   return s_wnd->mouseCol;
}

/* *********************************************************************** */

int HB_GT_FUNC(mouse_Row( void ))
{
   return s_wnd->mouseRow;
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_SetPos( int iRow, int iCol ))
{
   s_wnd->mouseGotoRow = iRow;
   s_wnd->mouseGotoCol = iCol;
   hb_xvt_gtRefreshLate();
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(mouse_IsButtonPressed( int iButton ))
{
   if ( iButton >= s_wnd->mouseNumButtons || iButton < 0 )
   {
      return FALSE;
   }
   return ( s_wnd->mouseButtonsState & 1 << iButton ) != 0;
}

/* *********************************************************************** */

int HB_GT_FUNC(mouse_CountButton( void ))
{
   return s_wnd->mouseNumButtons;
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight ))
{
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( iBottom );
   HB_SYMBOL_UNUSED( iRight );
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight ))
{
   HB_SYMBOL_UNUSED( piTop );
   HB_SYMBOL_UNUSED( piLeft );
   HB_SYMBOL_UNUSED( piBottom );
   HB_SYMBOL_UNUSED( piRight );
}

/* *********************************************************************** */
/* extended GT functions */

void HB_GT_FUNC(gt_SetDispCP( char * pszTermCDP, char * pszHostCDP, BOOL fBox ))
{
   HB_SYMBOL_UNUSED( fBox );
#ifndef HB_CDP_SUPPORT_OFF
   /*
    * We are displaying text in U16 so pszTermCDP is unimportant.
    * We only have to know what is the internal application codepage
    * to make proper translation
    */
   if ( !pszHostCDP || !*pszHostCDP )
   {
      if ( hb_cdp_page )
         pszHostCDP = hb_cdp_page->id;
      else if ( pszTermCDP && *pszTermCDP )
         pszHostCDP = pszTermCDP;
   }
   if ( pszHostCDP && *pszHostCDP )
   {
      PHB_CODEPAGE cdpHost = hb_cdpFind( pszHostCDP );
      if ( cdpHost && cdpHost != s_wnd->hostCDP )
      {
         s_wnd->hostCDP = cdpHost;
         if ( s_wnd->fInit )
            hb_xvt_gtBuildCharTrans( s_wnd );
      }
   }
#else
   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );
#endif
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetKeyCP( char * pszTermCDP, char * pszHostCDP ))
{
#ifndef HB_CDP_SUPPORT_OFF
   /*
    * Basic Xlib api has no function to return character key val in
    * unicode so far. We can use some nonstandard extension or try
    * to make translation XKEY_* to unicode ourself.
    * Now I don't have time to build the full conversion table so I only
    * add a simple hack which should work for LATIN-x encoding and
    * probably some others
    */
   if ( !pszTermCDP || !*pszTermCDP )
   {
      if ( hb_cdp_page )
         pszTermCDP = hb_cdp_page->id;
      else if ( pszHostCDP && *pszHostCDP )
         pszTermCDP = pszHostCDP;
   }
   if ( pszTermCDP && *pszTermCDP )
   {
      PHB_CODEPAGE cdpTerm = hb_cdpFind( pszTermCDP );
      if ( cdpTerm && cdpTerm )
      {
         s_wnd->inCDP = cdpTerm;
      }
   }
#else
   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );
#endif
}

/* *********************************************************************** */

int HB_GT_FUNC( gt_info(int iMsgType, BOOL bUpdate, int iParam, void *vpParam ) )
{
   int iRet = -1;

   HB_SYMBOL_UNUSED( vpParam );

   switch ( iMsgType )
   {
      case GTI_ISGRAPHIC:
         iRet = (int) TRUE;
         break;

      case GTI_SCREENWIDTH:
         iRet = s_wnd->width;
         if ( bUpdate )
            hb_gtSetMode( _GetScreenHeight(), iParam / s_wnd->fontWidth );
         break;

      case GTI_SCREENHEIGHT:
         iRet =  s_wnd->height;
         if ( bUpdate )
            hb_gtSetMode( iParam / s_wnd->fontHeight, _GetScreenWidth() );
         break;

      case GTI_SCREENDEPTH:
         break;

      case GTI_FONTSIZE:
         iRet =  s_wnd->fontHeight;
         if ( bUpdate ) /* TODO */
            s_wnd->fontHeight = iParam;
         break;

      case GTI_FONTWIDTH:
         iRet =  s_wnd->fontWidth;
         if ( bUpdate ) /* TODO */
            s_wnd->fontWidth = iParam;
         break;

      case GTI_FONTNAME:
         if ( bUpdate && vpParam != NULL ) /* TODO */
         {
            if ( s_wnd->szFontName )
               hb_xfree( s_wnd->szFontName );
            s_wnd->szFontName = hb_strdup( ( char * ) vpParam );
            iRet = 1;
         }
         break;

      case GTI_DESKTOPDEPTH:
         if ( s_wnd->dpy )
            iRet = DefaultDepth( s_wnd->dpy, DefaultScreen( s_wnd->dpy ) );
         break;

      case GTI_DESKTOPWIDTH:
      case GTI_DESKTOPHEIGHT:
      case GTI_DESKTOPCOLS:
      case GTI_DESKTOPROWS:
         if ( s_wnd->dpy )
         {
            XWindowAttributes wndAttr;
            XGetWindowAttributes( s_wnd->dpy, DefaultRootWindow( s_wnd->dpy ), &wndAttr );
            switch( iMsgType )
            {
               case GTI_DESKTOPWIDTH:
                  iRet = wndAttr.width;
                  break;
               case GTI_DESKTOPHEIGHT:
                  iRet = wndAttr.height;
                  break;
               case GTI_DESKTOPCOLS:
                  iRet = wndAttr.width / s_wnd->fontWidth;
                  break;
               case GTI_DESKTOPROWS:
                  iRet = wndAttr.height / s_wnd->fontHeight;
                  break;
            }
         }
         break;

      case GTI_INPUTFD:
         iRet = ConnectionNumber( s_wnd->dpy );
         break;

      case GTI_OUTPUTFD:
         iRet = s_iStdOut;
         break;

      case GTI_ERRORFD:
         iRet = s_iStdErr;
         break;

      case GTI_WINTITLE:
         {
            hb_xvt_gtSetWindowTitle( s_wnd, (char *) vpParam );
            return 1;   
         }

      case GTI_VIEWMAXWIDTH:
         iRet = _GetScreenWidth();
         break;

      case GTI_VIEWMAXHEIGHT:
         iRet = _GetScreenHeight();
         break;

      case GTI_CURSORBLINKRATE:
         iRet = s_cursorBlinkRate;
         if ( bUpdate )
            s_cursorBlinkRate = iParam;
         break;
   }

   return iRet;
}

/* *********************************************************************** */

void HB_GT_FUNC( gt_GetClipboard( char *szData, ULONG *pulMaxSize ) )
{
   hb_xvt_gtRefresh();
   hb_xvt_gtRequestSelection( s_wnd );

   if( *pulMaxSize == 0 || s_wnd->ClipboardSize < *pulMaxSize )
   {
      *pulMaxSize = s_wnd->ClipboardSize;
   }

   if( *pulMaxSize != 0 )
   {
      memcpy( szData, s_wnd->ClipboardData, *pulMaxSize );
   }
}

void HB_GT_FUNC( gt_SetClipboard( char *szData, ULONG ulSize ) )
{
   hb_xvt_gtRefresh();
   hb_xvt_gtSetSelection( s_wnd, szData, ulSize );
   hb_xvt_gtRefresh();
}

ULONG HB_GT_FUNC( gt_GetClipboardSize( void ) )
{
   hb_xvt_gtRefresh();
   hb_xvt_gtRequestSelection( s_wnd );

   return s_wnd->ClipboardSize;
}

/* *********************************************************************** */
// Exported functions for API calls

void hb_xvt_gtSetWindowTitle( PXWND_DEF wnd, char * title )
{
   if ( wnd->szTitle )
      hb_xfree( wnd->szTitle );

   if ( title )
      wnd->szTitle = hb_strdup( title );
   else
      wnd->szTitle = NULL;

   wnd->fDspTitle = TRUE;
}

/*
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

void HB_EXPORT hb_xvt_gtSetWindowTitle( PXWND_DEF wnd, char * title )
{
   if ( wnd->szTitle )
      hb_xfree( wnd->szTitle );

   if ( title )
      wnd->szTitle = hb_strdup( title );
   else
      wnd->szTitle = NULL;

   wnd->fDspTitle = TRUE;
}

int HB_EXPORT hb_xvt_gtGetWindowTitle( PXWND_DEF wnd, char *title, int length )
{
   if ( title != NULL && length > 0 )
   {
      strncpy( title, wnd->szTitle ? wnd->szTitle : "", length );
      title[ length - 1 ] = '\0';
   }
   return wnd->szTitle ? strlen( wnd->szTitle ) : 0;
}

void HB_EXPORT hb_xvt_gtSetWindowIcon(int icon)
{
   HB_SYMBOL_UNUSED(icon);
}

void HB_EXPORT hb_xvt_gtSetCloseEvent(int iEvent)
{
   HB_SYMBOL_UNUSED(iEvent);
}

void HB_EXPORT hb_xvt_gtSetShutdownEvent(int iEvent)
{
   HB_SYMBOL_UNUSED(iEvent);
}

BOOL HB_EXPORT hb_xvt_gtSetWindowPos(int left, int top)
{
  XMoveWindow( s_wnd->dpy, s_wnd->window, left, top );
  return TRUE;
}

BOOL HB_EXPORT hb_xvt_gtSetAltF4Close( BOOL bCanClose)
{
   HB_SYMBOL_UNUSED( bCanClose );
   return FALSE;
}
*/

void HB_GT_FUNC( gt_ProcessMessages( void ) )
{
   return;
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
    /* extended GT functions */
    gt_funcs->info                  = HB_GT_FUNC( gt_info );
    gt_funcs->SetDispCP             = HB_GT_FUNC( gt_SetDispCP );
    gt_funcs->SetKeyCP              = HB_GT_FUNC( gt_SetKeyCP );

    gt_funcs->SetClipboard          = HB_GT_FUNC( gt_SetClipboard );
    gt_funcs->GetClipboard          = HB_GT_FUNC( gt_GetClipboard );
    gt_funcs->GetClipboardSize      = HB_GT_FUNC( gt_GetClipboardSize );

    gt_funcs->ProcessMessages       = HB_GT_FUNC( gt_ProcessMessages );

    /* Graphics API */
/*
    gt_funcs->gfxPrimitive          = HB_GT_FUNC( gt_gfxPrimitive );
    gt_funcs->gfxText               = HB_GT_FUNC( gt_gfxText );
*/
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
