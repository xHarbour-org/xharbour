/*
 * $Id: gtxvt.c,v 1.7 2004/01/02 14:02:55 druzus Exp $
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

/* This definition has to be placed before #include "hbapigt.h" */

#include "gtxvt.h"
typedef struct tag_point
{
   int x;
   int y;
} POINT;

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

   void (*evt_callback)(void);

   // size in character cells
   USHORT cols;
   USHORT rows;

   // size in pixels
   USHORT width;
   USHORT height;
   // Set to true during resize operations
   BOOL bResizing;

   // cursor:
   int col;
   int row;
   USHORT cursorHeight;

   // Mouse functions
   int mouseCol;
   int mouseRow;
   int mouseGotoCol;
   int mouseGotoRow;
   int mouseNumButtons;
   int mouseDblClick1TO;
   int mouseDblClick2TO;
   int lastMouseEvent;
   BOOL mouseButtons[XVT_MAX_BUTTONS];

   // font informations
   XFontStruct *xfs;
   int fontHeight;
   int fontWidth;

   // buffer informations
   HB_GT_CELLTYPE *pBuffer;
   HB_GT_CELLTYPE *pAttributes;
   HB_GT_CELLTYPE background;
   ULONG bufsize;

   BOOL bInvalid;
   RECT rInvalid;

   // Key pointer
   int keyPointerIn;
   int keyPointerOut;
   int Keys[ XVT_CHAR_QUEUE_SIZE ];

} XWND_DEF, *PXWND_DEF;


typedef struct ClipKeyCode {
    int key;
    int alt_key;
    int ctrl_key;
    int shift_key;
} ClipKeyCode;


#define KP_CENTER 10001

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
    {K_UP,          K_ALT_UP,     K_CTRL_UP,         0}, /*  12 */
    {K_DOWN,      K_ALT_DOWN,   K_CTRL_DOWN,         0}, /*  13 */
    {K_LEFT,      K_ALT_LEFT,   K_CTRL_LEFT,         0}, /*  14 */
    {K_RIGHT,    K_ALT_RIGHT,  K_CTRL_RIGHT,         0}, /*  15 */
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
   { 221, HB_GTXVG_FULL_R},
   { 222, HB_GTXVG_FULL_L},
   { 223, HB_GTXVG_FULL_T}
};


#ifdef XVT_DEBUG
static int nCountPuts=0,nCountScroll=0, nCountPaint=0, nSetFocus=0, nKillFocus=0;
#endif

/******************************************************************/

//private
static void    gt_hbInitStatics(void);
static PXWND_DEF  hb_xvt_gtCreateWindow( Display *dpy );
static BOOL    hb_xvt_gtInitWindow(PXWND_DEF wnd, USHORT col, USHORT row);
static BOOL    hb_xvt_gtAllocSpBuffer( PXWND_DEF wnd, USHORT col, USHORT row);
void HB_EXPORT    hb_xvt_gtProcessMessages(int);

static void    hb_xvt_gtTranslateKey(int key );
static HB_GT_CELLTYPE hb_xvt_gtTranslateChar(unsigned char ch);
static BYTE hb_xvt_gtUntranslateChar( HB_GT_CELLTYPE ch );
static BOOL    hb_xvt_gtGetCharFromInputQueue (int * c);
static void    hb_xvt_gtAddCharToInputQueue (int data);
//static void    hb_xvt_gtTranslateKey(int key, int shiftkey, int altkey, int controlkey);

static void    hb_xvt_gtRepaintChar( PXWND_DEF wnd, int x, int y, int x1, int y1 );
static BOOL    hb_xvt_gtSetFont( PXWND_DEF wnd, char *face, char *weight, int size, char *encoding );
void HB_EXPORT hb_xvt_gtSetWindowTitle(PXWND_DEF wnd, char * title);
int HB_EXPORT hb_xvt_gtGetWindowTitle(PXWND_DEF wnd, char *title, int length);

static BOOL    hb_xvt_gtSetColors(PXWND_DEF wnd, BYTE attr);

static BOOL    hb_xvt_gtTextOut(PXWND_DEF wnd, USHORT col, USHORT row, char *lpString,  USHORT cbString );
static void    hb_xvt_gtSetStringInTextBuffer(USHORT col, USHORT row, BYTE attr, BYTE *sBuffer, USHORT length);
static USHORT  hb_xvt_gtGetIndexForTextBuffer(USHORT col, USHORT row);

static void    hb_xvt_gtInvalidate( PXWND_DEF wnd, int left, int top, int right, int bottom );
static void    hb_xvt_gtInvalidateChar( PXWND_DEF wnd, int left, int top, int right, int bottom );
static void    hb_xvt_gtUpdate( PXWND_DEF wnd );

static void    hb_xvt_gtDrawBoxChar( PXWND_DEF wnd, int col, int row, int boxchar );
static void hb_xvt_gtDisable( void );
static void hb_xvt_gtEnable( void );

static char *color_refs[] = {
   "black",
   "blue",
   "green",
   "cyan",
   "red",
   "magenta",
   "brown",
   "lightgray",
   "gray",
   "lightblue",
   "lightgreen",
   "lightcyan",
   "lightred",
   "lightmagenta",
   "yellow",
   "white"
};


/************************ globals ********************************/
static PXWND_DEF s_wnd = 0;

static USHORT  s_uiDispCount;
static USHORT  s_usCursorStyle;
static USHORT  s_usOldCurStyle;

static int s_iStdIn, s_iStdOut, s_iStdErr;

int s_cursorState = 0;
MODIFIERS s_modifiers;
BOOL sig_allarming = FALSE;

/* *********************************************************************** */

int s_errorHandler( Display *dpy, XErrorEvent *e )
{
    char errorText[1024];
    sprintf( errorText, "%s", "Xlib error: " );

    XGetErrorText( dpy, e->error_code,
         errorText + strlen( errorText ),
         sizeof(errorText) - strlen( errorText ) );
   hb_errRT_TERM( EG_CREATE, 10001, NULL, errorText, 0, 0 );
   return 1;
}

/************************************************************************/

void HB_GT_FUNC(gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr ))
{
   Display *dpy;
   PHB_FNAME pFileName;
   XSizeHints xsize;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

   /* stdin && stdout && stderr */
   s_iStdIn  = iFilenoStdin;
   s_iStdOut = iFilenoStdout;
   s_iStdErr = iFilenoStderr;

   s_usOldCurStyle = s_usCursorStyle = SC_NORMAL;

   gt_hbInitStatics();

   // With NULL, it gets the DISPLAY environment variable.
   dpy = XOpenDisplay( NULL );
   if ( dpy == NULL )
   {
      hb_errRT_TERM( EG_CREATE, 10001, NULL, "Can't connect to X server", 0, 0 );
      return;
   }

   s_wnd = hb_xvt_gtCreateWindow( dpy );

   pFileName = hb_fsFNameSplit( hb_cmdargARGV()[0] );
   hb_xvt_gtSetWindowTitle( s_wnd, pFileName->szName );
   hb_xfree( pFileName );
   HB_GT_FUNC(mouse_Init());

   XMapWindow( s_wnd->dpy, s_wnd->window );
   // ok, now we can inform the X manager about our new status:

   //xsize.flags = PWinGravity | PBaseSize | PResizeInc | PMinSize;
   xsize.flags = PWinGravity | PResizeInc | PMinSize;
   xsize.win_gravity = CenterGravity;
   xsize.width_inc = s_wnd->fontWidth;
   xsize.height_inc = s_wnd->fontHeight;
   xsize.min_width = s_wnd->fontWidth*6;
   xsize.min_height = s_wnd->fontHeight*3;
   xsize.base_width = s_wnd->width;
   xsize.base_height = s_wnd->height;

   XSetWMNormalHints(s_wnd->dpy, s_wnd->window, &xsize);

   /* Now we can set background cursor function */

   //hb_messageLoopHandler = hb_xvt_gtProcessMessages;
   hb_xvt_gtEnable();
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Exit( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

   hb_xvt_gtDisable();

   if (s_wnd)
   {
      HB_GT_FUNC(mouse_Exit());

      if ( s_wnd->xfs ) {
         XFreeFont( s_wnd->dpy, s_wnd->xfs );
      }

      XCloseDisplay( s_wnd->dpy );

      hb_xfree( s_wnd->pBuffer );
      hb_xfree( s_wnd->pAttributes );
      hb_xfree( s_wnd );
      s_wnd = 0;
   }

}

/* *********************************************************************** */
/* returns the number of displayable columns
 */
USHORT HB_GT_FUNC(gt_GetScreenWidth( void ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));
  return(s_wnd->cols);
}

/* *********************************************************************** */
/* returns the number of displayable rows
 */
USHORT HB_GT_FUNC(gt_GetScreenHeight( void ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));
  return(s_wnd->rows);
}

/* *********************************************************************** */

SHORT HB_GT_FUNC(gt_Col( void ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));
  return(s_wnd->col);
}

/* *********************************************************************** */

SHORT HB_GT_FUNC(gt_Row( void ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));
  return(s_wnd->row);
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetPos( SHORT sRow, SHORT sCol, SHORT sMethod ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", sRow, sCol, sMethod));
  HB_SYMBOL_UNUSED( sMethod );
  if (sRow >= 0 && sRow< s_wnd->rows && sCol>=0 && sCol <= s_wnd->cols )
  {
    hb_xvt_gtInvalidateChar( s_wnd, s_wnd->col, s_wnd->row, s_wnd->col, s_wnd->row );
    s_wnd->col = sCol;
    s_wnd->row = sRow;
    hb_xvt_gtInvalidateChar( s_wnd, s_wnd->col, s_wnd->row, s_wnd->col, s_wnd->row );
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
  return(s_usCursorStyle);
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetCursorStyle( USHORT usStyle ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", usStyle));

  s_usCursorStyle = usStyle;

  switch( usStyle )
  {
    case SC_NONE:
      s_wnd->cursorHeight = 0;
      break ;
    case SC_INSERT:
      s_wnd->cursorHeight = s_wnd->fontHeight/2;
      break;
    case SC_SPECIAL1:
      s_wnd->cursorHeight = s_wnd->fontHeight;
      break;
    case SC_SPECIAL2:
      s_wnd->cursorHeight = -s_wnd->fontHeight/2;
      break;
    case SC_NORMAL:
    default:
      s_wnd->cursorHeight = s_wnd->fontHeight/4;
      break;
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
    /*if ( s_uiDispCount == 0 && s_wnd->bInvalid ) {
       hb_xvt_gtUpdate( s_wnd );
    }*/
  }
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_DispCount())
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispCount()"));
  return(s_uiDispCount);
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Puts( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE *pbyStr, ULONG ulLen ))
{
  HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", usRow, usCol, (int) byAttr, pbyStr, ulLen));
  hb_xvt_gtSetStringInTextBuffer( usCol, usRow, byAttr, pbyStr, ulLen);
#ifdef XVT_DEBUG
  nCountPuts++;
#endif
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

  hb_xvt_gtSetStringInTextBuffer(usCol, usRow, byAttr, byChars, ulLen);
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
      index = HB_GT_INDEXOF(s_wnd, left, irow);
      for (icol = left; icol <= right; icol++, index++)
      {
         if (index >= s_wnd->bufsize)
         {
         break;
         }
         else
         {
         sBuffer[j++] = hb_xvt_gtUntranslateChar( s_wnd->pBuffer[index] );
         sBuffer[j++] = (BYTE) s_wnd->pAttributes[index];
         }
      }
   }
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_PutText( USHORT top, USHORT left, USHORT bottom, USHORT right, BYTE * sBuffer ))
{
   USHORT irow, icol, index, j;
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", top, left, bottom, right, sBuffer));

   j = 0;
   for (irow = top; irow <= bottom; irow++)
   {
      index = HB_GT_INDEXOF( s_wnd, left, irow );
      for (icol = left; icol <= right; icol++, index++ )
      {
         if (index >= s_wnd->bufsize/HB_GT_CELLSIZE)
         {
            break;
         }
         else
         {
            s_wnd->pBuffer[index] = hb_xvt_gtTranslateChar( sBuffer[j++] );
            s_wnd->pAttributes[index] = sBuffer[j++];
         }
      }
   }

   hb_xvt_gtInvalidateChar( s_wnd, left, top, right, bottom);
}

static void hb_xvt_gtPutTextInternal
   ( USHORT top, USHORT left, USHORT bottom, USHORT right,
      USHORT width,
      BYTE * sBuffer )
{
   USHORT irow, icol, index, j;
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", top, left, bottom, right, sBuffer));

   j = 0;
   for (irow = top; irow <= bottom; irow++)
   {
      index = HB_GT_INDEXOF( s_wnd, left, irow );
      j = irow * width*2;
      for (icol = left; icol <= right; icol++, index++ )
      {
         if (index >= s_wnd->bufsize/HB_GT_CELLSIZE)
         {
            break;
         }
         else
         {
            s_wnd->pBuffer[index] = hb_xvt_gtTranslateChar( sBuffer[j++] );
            s_wnd->pAttributes[index] = sBuffer[j++];
         }
      }
   }

   hb_xvt_gtInvalidateChar( s_wnd, left, top, right, bottom);
}
/* *********************************************************************** */

void HB_GT_FUNC(gt_SetAttribute( USHORT rowStart, USHORT colStart, USHORT rowStop, USHORT colStop, BYTE attr ))
{
   USHORT irow, icol, index;
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d", usTop, usLeft, usBottom, usRight, (int) attr));
   for ( irow = rowStart; irow <=rowStop; irow++)
   {
      index = HB_GT_INDEXOF(s_wnd, colStart, irow);
      for (icol = colStart; icol <= colStop; icol++)
      {
         if (index >= s_wnd->bufsize/HB_GT_CELLSIZE )
         {
            break;
         }
         else
         {
            s_wnd->pAttributes[index++] = attr;
         }
      }
   }

   hb_xvt_gtInvalidateChar( s_wnd, colStart, rowStart, colStop, rowStop );
}

/* Send an order that should repaint a part of the window, but measured
   in char cells coordinates. */
void hb_xvt_gtRepaintChar( PXWND_DEF wnd, int colStart, int rowStart, int colStop, int rowStop)
{
   int irow;
   USHORT icol, index, startIndex, startCol, len;
   BYTE oldAttrib, attrib;

   if ( rowStop >= wnd->rows )
   {
      rowStop = wnd->rows-1;
   }

   if ( colStop >= wnd->cols )
   {
      colStop = wnd->cols-1;
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
      index = icol +  irow * wnd->cols;
      startIndex = index;
      startCol = icol;
      len = 0;
      oldAttrib = wnd->pAttributes[index];
      /* attribute may change mid line...
      * so buffer up text with same attrib, and output it
      * then do next section with same attrib, etc
      */
      while (icol <= colStop)
      {
         if (index >= wnd->bufsize )
         {
            break;
         }
         attrib = wnd->pAttributes[index];
         if (attrib != oldAttrib)
         {
            hb_xvt_gtSetColors(wnd, oldAttrib);
            hb_xvt_gtTextOut( wnd, startCol, irow, (char *) (wnd->pBuffer+startIndex), len );
            oldAttrib = attrib;
            startIndex = index;
            startCol = icol;
            len = 0;

         }
         icol++;
         len++;
         index++;
      }
      hb_xvt_gtSetColors(wnd, oldAttrib);
      hb_xvt_gtTextOut(wnd, startCol, irow, (char *) (wnd->pBuffer+startIndex), len );
   }

   // must the cursor be repainted?
   if ( wnd->cursorHeight > 0 &&
      wnd->col >= colStart && wnd->col <= colStop &&
      wnd->row >= rowStart && wnd->row <= rowStop )
   {
      index = wnd->col +  wnd->row * wnd->cols;
      if ( s_cursorState ) // currently on
      {
         oldAttrib = wnd->pAttributes[ index ];
         if ( (oldAttrib & 0x70) != 0x70) {
            attrib = 0x70 | (oldAttrib ^ 0x0f);
         }
         else {
            attrib = (oldAttrib & 0x0f) | 0x08;
         }

         hb_xvt_gtSetColors( wnd, attrib);
         hb_xvt_gtTextOut( wnd, wnd->col, wnd->row,
               (char *) (wnd->pBuffer+index), 1 );
      }
      else
      {
         hb_xvt_gtSetColors( wnd, wnd->pAttributes[ index ]);
         hb_xvt_gtTextOut( wnd, wnd->col, wnd->row,
               (char *) (wnd->pBuffer+index), 1 );
      }
   }

}

void hb_xvt_gtUpdate( PXWND_DEF wnd )
{
   if ( wnd->bInvalid )
   {
      wnd->bInvalid = FALSE;
      hb_xvt_gtRepaintChar( wnd,
         wnd->rInvalid.left, wnd->rInvalid.top,
         wnd->rInvalid.right, wnd->rInvalid.bottom);
   }
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

   s_wnd->background = byAttr;
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

   hb_xvt_gtInvalidateChar( s_wnd, 0, 0, s_wnd->cols-1, s_wnd->rows-1 );

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
   BOOL bResult= FALSE;
   int oldrows, oldcols;
   BYTE *memory;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", row, col));

   oldrows = s_wnd->rows-1;
   oldcols = s_wnd->cols-1;

   // ignore stupid requests
   if ( col < 1 || row < 1 ||
        col > XVT_MAX_COLS || row > XVT_MAX_ROWS ||
        ( col == s_wnd->rows && row == s_wnd->rows) )
   {
      return FALSE;
   }

   memory = (BYTE *) hb_xgrab( (s_wnd->rows *s_wnd->cols+ 1) * HB_GT_CELLSIZE );

   HB_GT_FUNC(gt_GetText( 0, 0, oldrows, oldcols, memory ));

   hb_xvt_gtDisable();

   if (row<= XVT_MAX_ROWS && col<= XVT_MAX_COLS)
   {
      if ( s_wnd->bResizing ) {
         bResult = hb_xvt_gtAllocSpBuffer( s_wnd, col, row);
         if ( bResult )
         {
            s_wnd->width = col * s_wnd->fontWidth;
            s_wnd->height = row * s_wnd->fontHeight;
         }
      }
      else
      {
         bResult = hb_xvt_gtInitWindow(s_wnd, col, row);
      }
   }

   if ( bResult )
   {
      s_wnd->cols = col;
      s_wnd->rows = row;

      if ( s_wnd->col >= s_wnd->cols )
      {
         s_wnd->col = s_wnd->cols -1;
      }

      if ( s_wnd->row >= s_wnd->rows )
      {
         s_wnd->row = s_wnd->rows -1;
      }

      if ( row > oldrows )
      {
         row = oldrows;
      }
      if ( col > oldcols  )
      {
         col = oldcols;
      }
      hb_xvt_gtPutTextInternal( 0, 0, row, col, oldcols+1,  memory );
   }

   hb_xfree( memory );

   hb_xvt_gtInvalidateChar( s_wnd, 0, 0, s_wnd->cols, s_wnd->rows );
   hb_xvt_gtUpdate(s_wnd);
   hb_xvt_gtEnable();

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

   index = hb_xvt_gtGetIndexForTextBuffer(iCol, iRow);
   if (index < s_wnd->bufsize )
   {
      s_wnd->pBuffer[index] = hb_xvt_gtTranslateChar( bChar );
      //s_wnd->pBuffer[index] = bChar;
      s_wnd->pAttributes[index] = bAttr;

      //determine bounds of rect around character to refresh
      // but do not invalidate during screen updates
      hb_xvt_gtInvalidateChar(s_wnd, iCol, iRow, iCol, iRow);
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
    USHORT sWidth = s_wnd->cols;
    USHORT sHeight = s_wnd->rows;

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
   HB_SYMBOL_UNUSED( eventmask ); // we ignore the eventmask!
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

   if ( eventmask & ( INKEY_KEYBOARD | HB_INKEY_RAW | HB_INKEY_EXTENDED ) )
   {
      bKey = hb_xvt_gtGetCharFromInputQueue(&c);
   }

   if (! bKey  && ( eventmask & INKEY_MOVE ) > 0)
   {
      if ( s_wnd->lastMouseEvent == K_MOUSEMOVE )
      {
         bKey = TRUE;
         c = K_MOUSEMOVE;
         s_wnd->lastMouseEvent = 0;
      }
   }

   if (! bKey  && ( eventmask & INKEY_LDOWN ) > 0)
   {
      if ( s_wnd->lastMouseEvent == K_LBUTTONDOWN ||
           s_wnd->lastMouseEvent == K_LDBLCLK )
      {
         bKey = TRUE;
         c = s_wnd->lastMouseEvent;
         s_wnd->lastMouseEvent = 0;
      }
   }

   if (! bKey  && ( eventmask & INKEY_LUP ) > 0)
   {
      if ( s_wnd->lastMouseEvent == K_LBUTTONUP )
      {
         bKey = TRUE;
         c = s_wnd->lastMouseEvent;
         s_wnd->lastMouseEvent = 0;
      }
   }

   if (! bKey  && ( eventmask & INKEY_RDOWN ) > 0)
   {
      if ( s_wnd->lastMouseEvent == K_RBUTTONDOWN ||
           s_wnd->lastMouseEvent == K_RDBLCLK )
      {
         bKey = TRUE;
         c = s_wnd->lastMouseEvent;
         s_wnd->lastMouseEvent = 0;
      }
   }

   if (! bKey  && ( eventmask & INKEY_RUP ) > 0)
   {
      if ( s_wnd->lastMouseEvent == K_RBUTTONUP )
      {
         bKey = TRUE;
         c = s_wnd->lastMouseEvent;
         s_wnd->lastMouseEvent = 0;
      }
   }

   return ( bKey ? c : 0);
}



/* *********************************************************************** */

void HB_GT_FUNC(gt_Tone( double dFrequency, double dDuration ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));
   // to be done.
   HB_SYMBOL_UNUSED( dFrequency );
   HB_SYMBOL_UNUSED( dDuration );
}

/* *********************************************************************** */

void HB_GT_FUNC(mouse_Init( void ))
{
   unsigned char map[1];
   int i;

   s_wnd->mouseDblClick1TO = 0;
   s_wnd->mouseDblClick2TO = 0;
   s_wnd->lastMouseEvent = 0;
   s_wnd->mouseGotoCol = -1;
   s_wnd->mouseGotoRow = -1;
   s_wnd->mouseNumButtons = XGetPointerMapping( s_wnd->dpy, map, 1 );

   if ( s_wnd->mouseNumButtons > XVT_MAX_BUTTONS )
   {
      s_wnd->mouseNumButtons = XVT_MAX_BUTTONS;
   }

   for ( i = 0; i < s_wnd->mouseNumButtons; i ++ )
   {
      s_wnd->mouseButtons[ i ] = FALSE;
   }

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
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(mouse_IsButtonPressed( int iButton ))
{
   if ( iButton >= s_wnd->mouseNumButtons || iButton < 0 )
   {
      return FALSE;
   }

   return s_wnd->mouseButtons[ iButton ] = TRUE;
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


void HB_GT_FUNC(mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight ))
{
   HB_SYMBOL_UNUSED( piTop );
   HB_SYMBOL_UNUSED( piLeft );
   HB_SYMBOL_UNUSED( piBottom );
   HB_SYMBOL_UNUSED( piRight );
}

/* *********************************************************************** */

static void hb_xvt_gtDisable( void )
{
   signal( SIGALRM, SIG_IGN);
}

static void hb_xvt_gtEnable( void )
{
   struct itimerval itv;

   sig_allarming = FALSE;
   signal( SIGALRM, hb_xvt_gtProcessMessages);
   itv.it_interval.tv_sec = 0;
   itv.it_interval.tv_usec = 25000;
   itv.it_value = itv.it_interval;
   setitimer( ITIMER_REAL, &itv, NULL);
}

static BOOL hb_xvt_gtAllocSpBuffer( PXWND_DEF wnd, USHORT col, USHORT row)
{
   int i;

   if ( row <= XVT_MAX_ROWS && col <= XVT_MAX_COLS )
   {
      wnd->cols = col;
      wnd->rows = row;

      wnd->bufsize = col * row * HB_GT_CELLSIZE;
      if ( wnd->pBuffer != 0 )
      {
         wnd->pBuffer = ( HB_GT_CELLTYPE *) hb_xrealloc( wnd->pBuffer, wnd->bufsize );
         wnd->pAttributes = ( HB_GT_CELLTYPE *) hb_xrealloc( wnd->pAttributes, wnd->bufsize );
      }
      else
      {
         wnd->pBuffer = ( HB_GT_CELLTYPE *) hb_xgrab( wnd->bufsize );
         wnd->pAttributes = ( HB_GT_CELLTYPE *) hb_xgrab( wnd->bufsize );
      }

      for ( i = 0; i < wnd->bufsize/ HB_GT_CELLSIZE; i++ )
      {
         #ifdef HB_BIG_ENDIAN
         wnd->pBuffer[i] = 0x0020;
         #else
         wnd->pBuffer[i] = 0x2000;
         #endif
         wnd->pAttributes[i] = wnd->background;
      }

      return TRUE;
   }
   else
   {
      return FALSE;
   }
}


static void    hb_xvt_gtDrawBoxChar( PXWND_DEF wnd, int col, int row, int boxchar )
{
   XSegment segs[9];
   int nsegs = 0;
   char space = ' ';
   int cellx = wnd->fontWidth;
   int celly = wnd->fontHeight;
   int basex = col * cellx;
   int basey = row * celly;


   /* Drawing a filler ? */
   if ( boxchar >= HB_GTXVG_FILLER1 )
   {
      XColor color, dummy;
      int attr = wnd->pAttributes[ HB_GT_INDEXOF( wnd, col, row )  ];
      int fore = attr & 0x000F;
      int back = (attr & 0x00F0)>>4;
      XPoint pts[16*16];
      int icol, irow, icount = 0, istart = 1;

      for ( irow = 0; irow < celly;
            irow += boxchar != HB_GTXVG_FILLER1 ? 2 : 3 )
      {
         for ( icol = istart ; icol < cellx;
               icol += boxchar == HB_GTXVG_FILLER1 ? 2 : 3 )
         {
            pts[icount].x = basex + icol;
            pts[icount].y = basey + irow;
            icount++;
         }
         istart = istart != 1 ? 1 : 2;
      }

      if ( boxchar != HB_GTXVG_FILLER3 )
      {

         XAllocNamedColor( wnd->dpy, wnd->colors, color_refs[back], &color, &dummy );
         XSetForeground( wnd->dpy, wnd->gc, color.pixel );

         // erase background!
         XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
            basex, basey, cellx, celly );

         XAllocNamedColor( wnd->dpy, wnd->colors, color_refs[fore], &color, &dummy );
         XSetForeground( wnd->dpy, wnd->gc, color.pixel );

         // draw in foreground!
         XDrawPoints( wnd->dpy, wnd->window, wnd->gc,
               pts, icount, CoordModeOrigin );
      }
      else
      {

         // erase Foreground!
         XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
            basex, basey, cellx, celly );

         XAllocNamedColor( wnd->dpy, wnd->colors, color_refs[back], &color, &dummy );
         XSetForeground( wnd->dpy, wnd->gc, color.pixel );

         // draw in background!
         XDrawPoints( wnd->dpy, wnd->window, wnd->gc,
               pts, icount, CoordModeOrigin );

         // reset foreground
         XAllocNamedColor( wnd->dpy, wnd->colors, color_refs[fore], &color, &dummy );
         XSetForeground( wnd->dpy, wnd->gc, color.pixel );
      }

      // done here
      return;
   }

   /* Clears background */
   XDrawImageString( wnd->dpy, wnd->window, wnd->gc,
      basex, basey+wnd->xfs->ascent,
      &space, 1 );

   /* Drawing a full square? */
   if ( boxchar >= HB_GTXVG_FULL )
   {
      switch( boxchar )
      {
         case HB_GTXVG_FULL:
            XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
               basex, basey, cellx, celly );
            return;

         case HB_GTXVG_FULL_T:
            XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
               basex, basey, cellx, celly/2 );
            return;

         case HB_GTXVG_FULL_B:
            XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
               basex, basey + celly/2, cellx, celly/2 );
            return;

         case HB_GTXVG_FULL_L:
            XFillRectangle( wnd->dpy, wnd->window, wnd->gc,
               basex, basey, cellx/2, celly );
            return;

         case HB_GTXVG_FULL_R:
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
         segs[0].x2 = basex + cellx/2+1;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = basey + celly/2+1;
         segs[1].x2 = segs[0].x2;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = basex + cellx/2+1;
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
         segs[0].x2 = basex + cellx/2+1;
         segs[0].y2 = segs[0].y1;

         segs[1].x1 = segs[0].x1;
         segs[1].y1 = basey + celly/2+1;
         segs[1].x2 = segs[0].x2;
         segs[1].y2 = segs[1].y1;

         segs[2].x1 = basex + cellx/2+1;
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


static BOOL hb_xvt_gtTextOut( PXWND_DEF wnd,  USHORT col, USHORT row, char * str, USHORT cbString )
{
   int pos;
   USHORT *usString;
   HB_GT_CELLTYPE cell;

   if (cbString > wnd->cols) // make sure string is not too long
   {
      cbString = wnd->cols;
   }

   XDrawImageString16( wnd->dpy, wnd->window, wnd->gc,
      col * wnd->fontWidth, row * wnd->fontHeight+wnd->xfs->ascent, (XChar2b *) str, cbString );

   /* Draw eventual graphical chars */

   usString = (USHORT *) str;
   for ( pos = 0; pos < cbString; pos ++ )
   {
      #ifdef HB_BIG_ENDIAN
      cell = usString[pos];
      if ( cell > HB_GTXVT_DBL_LT )
      #else
      cell = 0xFFFF & ((usString[pos] << 8) | (usString[pos]>>8));
      if ( cell >= HB_GTXVT_DBL_LT )
      #endif
      {
         hb_xvt_gtDrawBoxChar( wnd, col + pos, row, cell );
      }
   }
   return TRUE;
}


static BOOL hb_xvt_gtInitWindow( PXWND_DEF wnd, USHORT col, USHORT row)
{

   if ( hb_xvt_gtAllocSpBuffer( wnd, col, row) )
   {
      wnd->width = col * wnd->fontWidth;
      wnd->height = row * wnd->fontHeight;

      // resize the window
      wnd->bResizing = TRUE;
      XResizeWindow( wnd->dpy, wnd->window,
         wnd->width,
         wnd->height );
      wnd->bResizing = FALSE;
      return TRUE;
   }

   return FALSE;
}


static void hb_xvt_processKey( XKeyEvent *evt)
{
   unsigned char buf[5];
   KeySym out = XLookupKeysym( evt, 0 );
   int ikey = 0;

   switch( out )
   {
      // First of all, let's scan for special codes
      case XK_Shift_L: case XK_Shift_R:
         s_modifiers.bShift = TRUE;
      return;

      case XK_Control_L: case XK_Control_R:
         s_modifiers.bCtrl = TRUE;
      return;

      case XK_Meta_L: case XK_Alt_L:
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
         hb_xvt_gtAddCharToInputQueue(K_ESC);
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
            hb_xvt_gtAddCharToInputQueue(KP_CTRL_5);
            break;
         }
         // else fallback and add a normal 5
      break;

      case XK_Pause:
         if ( s_modifiers.bCtrl ) {
            // Pretend Alt+C pressed
            hb_xvt_gtAddCharToInputQueue(HB_BREAK_FLAG);
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
      hb_xvt_gtTranslateKey( ikey );
      return;
   }

   // We have not found it. Obtain a string.

   if ( XLookupString( evt , buf, 5, &out, NULL ) )
   {
      // if it is a ? we can have still one special CLIPPER character
      if ( *buf == '?' && buf[1] == 0 && s_modifiers.bCtrl )
      {
         hb_xvt_gtAddCharToInputQueue( K_CTRL_QUESTION );
      }
      else
      {
         if ( ! s_modifiers.bCtrl && ! s_modifiers.bAlt )
         {
            // ready for UTF input!!!
            hb_xvt_gtAddCharToInputQueue( *buf + (buf[1] << 8) );
         }
         else {
            hb_xvt_gtTranslateKey( *buf );
         }
      }
   }
}


static void hb_xvt_gtWndProc( XEvent *evt )
{

   switch (evt->type)
   {

      case Expose:
         hb_xvt_gtInvalidate( s_wnd,
            evt->xexpose.x , evt->xexpose.y,
            evt->xexpose.x + evt->xexpose.width,
            evt->xexpose.height + evt->xexpose.y );

         //hb_xvt_gtUpdate( s_wnd );
      break;


      case KeyPress:
      {
         hb_xvt_processKey( &evt->xkey );
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

            case XK_Meta_L: case XK_Alt_L:
               s_modifiers.bAlt = FALSE;
            return;

            case XK_Meta_R: case XK_Alt_R:
               s_modifiers.bAltGr = FALSE;
            return;
         }
      }
      break;

      case MotionNotify:
         s_wnd->mouseCol = evt->xmotion.x / s_wnd->fontWidth;
         s_wnd->mouseRow = evt->xmotion.y / s_wnd->fontHeight;
         s_wnd->lastMouseEvent = K_MOUSEMOVE;
      break;

      case ButtonPress: case ButtonRelease:
      {
         unsigned char map[ XVT_MAX_BUTTONS ];
         int button=0;

         XGetPointerMapping( s_wnd->dpy, map, s_wnd->mouseNumButtons );

         switch ( evt->xbutton.button )
         {
            case Button1:
               button = map[0];
               if ( evt->type == ButtonPress )
               {
                  if ( s_wnd->mouseDblClick1TO > 0 )
                  {
                     s_wnd->lastMouseEvent = K_LDBLCLK;
                  }
                  else
                  {
                     s_wnd->lastMouseEvent = K_LBUTTONDOWN;
                  }
                  // about half a second
                  s_wnd->mouseDblClick1TO = 8;
               }
               else {
                  s_wnd->lastMouseEvent = K_LBUTTONUP;
               }
            break;

            case Button2:
               button = map[1];
               if ( s_wnd->mouseNumButtons == 2 )
               {
                  if ( evt->type == ButtonPress )
                  {
                     if ( s_wnd->mouseDblClick2TO > 0 )
                     {
                         s_wnd->lastMouseEvent = K_RDBLCLK;
                     }
                     else
                     {
                         s_wnd->lastMouseEvent = K_RBUTTONDOWN;
                     }
                     // about half a second
                     s_wnd->mouseDblClick2TO = 8;
                  }
                  else {
                     s_wnd->lastMouseEvent = K_RBUTTONUP;
                  }
               }
            break;

            case Button3:
               button = map[2];
               if ( s_wnd->mouseNumButtons >= 3 )
               {
                  if ( evt->type == ButtonPress )
                  {
                     if ( s_wnd->mouseDblClick2TO > 0 )
                     {
                        s_wnd->lastMouseEvent = K_RDBLCLK;
                     }
                     else
                     {
                        s_wnd->lastMouseEvent = K_RBUTTONDOWN;
                     }
                     // about half a second
                     s_wnd->mouseDblClick2TO = 8;
                  }
                  else {
                     s_wnd->lastMouseEvent = K_RBUTTONUP;
                  }
               }
            break;

            case Button4:
               button = map[3];
            break;

            case Button5:
               button = map[4];
            break;
         }

         button--;

         if ( button < 0 || button >= s_wnd->mouseNumButtons )
         {
            button = 0;
         }

         if ( evt->type == ButtonPress )
         {
            s_wnd->mouseButtons[ button ] = TRUE;
         }
         else
         {
            s_wnd->mouseButtons[ button ] = FALSE;
         }
      }
      break;

      case ConfigureNotify:
         if ( s_wnd->bResizing )
         {
            break;
         }

         s_wnd->bResizing = TRUE;
         // will silently ignore resetting to current dimensions
         hb_gtSetMode(
            evt->xconfigure.height/s_wnd->fontHeight,
            evt->xconfigure.width/s_wnd->fontWidth );
         s_wnd->bResizing = FALSE;

      break;

   }


}

/* Translate ASCII char into unicode representation */
static HB_GT_CELLTYPE hb_xvt_gtTranslateChar( unsigned char ch )
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


/* Translate Unicode representation char into ASCII char */
static BYTE hb_xvt_gtUntranslateChar( HB_GT_CELLTYPE ch )
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

static void hb_xvt_gtTranslateKey( int key )
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

   hb_xvt_gtAddCharToInputQueue(key);
}


static PXWND_DEF hb_xvt_gtCreateWindow( Display *dpy )
{
   PXWND_DEF wnd = ( PXWND_DEF ) hb_xgrab( sizeof( XWND_DEF ) );
   int whiteColor;

   // load the standard font
   wnd->dpy = dpy;
   wnd->rows = XVT_DEFAULT_ROWS;
   wnd->cols = XVT_DEFAULT_COLS;
   wnd->bResizing = FALSE;
   if (! hb_xvt_gtSetFont( wnd, "fixed", "medium", 18, NULL ) )
   {
      hb_xfree( wnd );
      hb_errRT_TERM( EG_CREATE, 10001, NULL, "Can't load 'fixed' font", 0, 0 );
      return NULL;
   }

   /* gtInitWindow will update them */
   wnd->pBuffer = 0;
   wnd->pAttributes = 0;

   wnd->mouseCol = 0;
   wnd->mouseRow = 0;
   wnd->cursorHeight = wnd->fontHeight;
   wnd->col = 0;
   wnd->row = 0;
   /* Clear keyboard buffer */
   wnd->keyPointerIn = 1;
   wnd->keyPointerOut = 0;

   whiteColor = WhitePixel(dpy, DefaultScreen(dpy));
   wnd->window = XCreateSimpleWindow(dpy,
      DefaultRootWindow(dpy),
      0, 0, wnd->width, wnd->height,
      0, whiteColor, whiteColor);

   wnd->gc = XCreateGC( dpy, wnd->window, 0, NULL );
   // Line width 2
   XSetLineAttributes( dpy, wnd->gc, 1, LineSolid, CapRound, JoinBevel );
   wnd->colors = DefaultColormap( dpy, DefaultScreen( dpy ));

   wnd->background = 0x07;
   XSetFont( dpy, wnd->gc, wnd->xfs->fid );


   XSelectInput( dpy, wnd->window, XVT_STD_MASK);

   hb_xvt_gtInitWindow( wnd, XVT_DEFAULT_COLS, XVT_DEFAULT_ROWS );

   wnd->bInvalid = TRUE;
   wnd->rInvalid.top = 0;
   wnd->rInvalid.left = 0;
   wnd->rInvalid.bottom = wnd->rows;
   wnd->rInvalid.right = wnd->cols;

   return wnd;
}


static void hb_xvt_gtInvalidateChar( PXWND_DEF wnd,
   int left, int top, int right, int bottom )
{
   if ( wnd->bInvalid == FALSE ) {
      wnd->bInvalid = TRUE;
      wnd->rInvalid.top = top;
      wnd->rInvalid.left = left;
      wnd->rInvalid.bottom = bottom;
      wnd->rInvalid.right = right;
   }
   else {
      if ( wnd->rInvalid.top > top ) wnd->rInvalid.top = top;
      if ( wnd->rInvalid.left > left ) wnd->rInvalid.left = left;
      if ( wnd->rInvalid.right < right ) wnd->rInvalid.right = right;
      if ( wnd->rInvalid.bottom < bottom ) wnd->rInvalid.bottom = bottom;
   }
}

static void hb_xvt_gtInvalidate( PXWND_DEF wnd,
   int left, int top, int right, int bottom )
{
   hb_xvt_gtInvalidateChar( wnd,
      left / wnd->fontWidth, top / wnd->fontHeight,
      right / wnd->fontWidth +1, bottom / wnd->fontHeight+1);
}

void HB_EXPORT hb_xvt_gtProcessMessages(int test)
{
   static int count = 0;
   // for now, just handle s_wnd events
   XEvent evt;

   sig_allarming = TRUE;


   // optionally move the pointer
   if ( s_wnd->mouseGotoRow >= 0 && s_wnd->mouseGotoCol >= 0 )
   {
      XWarpPointer( s_wnd->dpy, None, s_wnd->window, 0,0,0,0,
         s_wnd->mouseGotoCol * s_wnd->fontWidth + s_wnd->fontWidth/2,
         s_wnd->mouseGotoRow * s_wnd->fontHeight + s_wnd->fontHeight/2 );
      s_wnd->mouseGotoRow = -1;
   }

   if ( s_wnd->mouseDblClick1TO > 0 ) {
      s_wnd->mouseDblClick1TO--;
   }

   if ( s_wnd->mouseDblClick2TO > 0 )
   {
      s_wnd->mouseDblClick2TO--;
   }


   evt.type = 0;
   while ( XCheckMaskEvent( s_wnd->dpy, XVT_STD_MASK, &evt) )
   {
      hb_xvt_gtWndProc( &evt );
   }

   if ( ++count == 10 ) {
      s_cursorState = s_cursorState ? 0: 1;
      hb_xvt_gtInvalidateChar( s_wnd, s_wnd->col, s_wnd->row, s_wnd->col, s_wnd->row);
      count = 0;
   }
   hb_xvt_gtUpdate( s_wnd );

   sig_allarming = FALSE;
}


/*
 * hb_xvt_gtGetIndexForTextBuffer takes a row and column,
 * nd returns the appropriate index into the screen Text buffer
 */
static USHORT hb_xvt_gtGetIndexForTextBuffer(USHORT col, USHORT row)
{
  return(row * s_wnd->cols + col);
}

/* get for and background colours from attribute and set them for window
*/
static BOOL hb_xvt_gtSetColors( PXWND_DEF wnd, BYTE attr)
{
   XColor color, dummy;
   int fore = attr & 0x000F;
   int back = (attr & 0x00F0)>>4;

   XAllocNamedColor( wnd->dpy, wnd->colors, color_refs[fore], &color, &dummy );
   XSetForeground( wnd->dpy, wnd->gc, color.pixel );

   XAllocNamedColor( wnd->dpy, wnd->colors, color_refs[back], &color, &dummy );
   XSetBackground( wnd->dpy, wnd->gc, color.pixel );

   return(TRUE);
}


static void gt_hbInitStatics(void)
{
   XSetErrorHandler( s_errorHandler );

   s_modifiers.bCtrl  = FALSE;
   s_modifiers.bAlt   = FALSE;
   s_modifiers.bAltGr = FALSE;
   s_modifiers.bShift = FALSE;
}

/*
 *  functions for handling the input queues for the mouse and keyboard
 */
static void hb_xvt_gtAddCharToInputQueue ( int data)
{
  int iNextPos;

  iNextPos = (s_wnd->keyPointerIn >= XVT_CHAR_QUEUE_SIZE) ? 0 : s_wnd->keyPointerIn+1 ;
  if (iNextPos != s_wnd->keyPointerOut ) // Stop accepting characters once the buffer is full
  {
    s_wnd->Keys[s_wnd->keyPointerIn] = data ;
    s_wnd->keyPointerIn = iNextPos ;
  }
}

static BOOL hb_xvt_gtGetCharFromInputQueue (int *c)
{
  int iNextPos;
  BOOL bRet = FALSE;
  *c = 0;
  iNextPos = (s_wnd->keyPointerOut >= XVT_CHAR_QUEUE_SIZE) ? 0 : s_wnd->keyPointerOut+1 ;
  if (iNextPos != s_wnd->keyPointerIn )  // No more events in queue ??
  {
    *c = s_wnd->Keys[iNextPos] ;
    s_wnd->keyPointerOut = iNextPos ;
    bRet =  TRUE;
  }
  return(bRet);
}

/*
 * hb_xvt_gtSetStringInTextBuffer puts the string of the specified length into the TextBuffer at
 * the specified caret position
 * It then determines the invalid rectangle, so the string will be displayed
 */
static void hb_xvt_gtSetStringInTextBuffer(USHORT col, USHORT row, BYTE attr, BYTE *sBuffer, USHORT length)
{
   USHORT index;
   int pos;

   // determine the index and put the string into the TextBuffer
   index = HB_GT_INDEXOF(s_wnd, col, row);
   if (length + index <= s_wnd->bufsize)
   {
      if (attr != ' ') // if no attribute, don't overwrite
      {
         memset((s_wnd->pAttributes+index), attr, length);
         for ( pos = 0; pos < length; pos ++ )
         {
            s_wnd->pAttributes[index + pos] = attr;
         }
      }

      // translate characters
      //memcpy((s_wnd->pBuffer+index), sBuffer, length);
      for ( pos = 0; pos < length; pos ++ )
      {
         s_wnd->pBuffer[index + pos] = hb_xvt_gtTranslateChar(sBuffer[ pos ]);
      }

      //determine bounds of rect around character to refresh
      hb_xvt_gtInvalidateChar( s_wnd, col, row, col + length, row  );
   }
}


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

BOOL HB_EXPORT hb_xvt_gtSetFont( PXWND_DEF wnd, char *fontFace, char *weight, int size,  char *encoding )
{
   char fontString[150];
   XFontStruct *xfs;

   snprintf( fontString, 149, "-*-%s-%s-r-normal--%d-*-*-*-*-*-%s",
      fontFace, weight, size, encoding == NULL ? "*-*" : encoding);

   xfs = XLoadQueryFont( wnd->dpy, fontString );

   if ( xfs == NULL )
   {
      snprintf( fontString, 149, "Can't load '%s' font", fontFace );
      hb_errRT_TERM( EG_CREATE, 10001, NULL, fontString, 0, 0 );
      return FALSE;
   }

   // a shortcut for window height and width
   wnd->fontHeight = xfs->max_bounds.ascent + xfs->max_bounds.descent;
   wnd->fontWidth = xfs->max_bounds.rbearing - xfs->min_bounds.lbearing;
   wnd->xfs = xfs;

   wnd->width = wnd->cols * wnd->fontWidth;
   wnd->height = wnd->rows * wnd->fontHeight;

   return TRUE;
}


void HB_EXPORT hb_xvt_gtSetCloseEvent(int iEvent)
{

}


void HB_EXPORT hb_xvt_gtSetShutdownEvent(int iEvent)
{

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

HB_CALL_ON_STARTUP_BEGIN( HB_GT_FUNC(_gt_Init_) )
   hb_gtRegister( &gtInit );
HB_CALL_ON_STARTUP_END( HB_GT_FUNC(_gt_Init_) )
#if defined(HB_STATIC_STARTUP) || ( (! defined(__GNUC__)) && (! defined(_MSC_VER)) )
   #pragma startup HB_GT_FUNC(_gt_Init_)
#endif

#endif  /* HB_MULTI_GT */


/* *********************************************************************** */
