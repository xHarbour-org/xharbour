/*
 * $Id: gtwin.c,v 1.53 2004/05/07 15:33:32 lf_sfnet Exp $
 */

/*
 * Harbour Project source code:
 * Video subsystem for Win32 compilers ver.2
 * Copyright 2002 Przemys³aw Czerpak <druzus@polbox.com>
 *
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemys³aw Czerpak <druzus@polbox.com>
 *   Video subsystem for Win32 compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_gt_CtrlHandler()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_Tone()
 *    hb_gt_ReadKey()
 *
 * See doc/license.txt for licensing terms.
 *
 * www - http://www.harbour-project.org
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
#define HB_GT_NAME	WIN

/* TODO: include any standard headers here */
/* *********************************************************************** */

#define HB_OS_WIN_32_USED


#include "hbapi.h"
#include "hbapigt.h"
#include "hbapifs.h"
#include "hbset.h"
#include "hbvm.h"
#include "inkey.ch"

#include <string.h>
#include <time.h>
//#include <io.h>

#if defined( _MSC_VER )
  #include <conio.h>
#endif

/*
 To disable mouse, initialization was made in cmdarg.c
*/
extern BOOL b_MouseEnable;

/* *********************************************************************** */

#if defined(__IBMCPP__)
   #undef WORD                            /* 2 bytes unsigned */
   typedef unsigned short int WORD;
#else
   #if ! defined(HB_DONT_DEFINE_BASIC_TYPES)
      #undef WORD                            /* 2 bytes unsigned */
      typedef USHORT WORD;

      #undef DWORD                           /* 4 bytes unsigned */
      typedef ULONG DWORD;
   #endif
#endif

#if ! defined(__GNUC__) && defined(__CYGWIN__)
   typedef WORD far * LPWORD;
#endif

#if defined(__RSXNT__)
   #ifndef FROM_LEFT_1ST_BUTTON_PRESSED
      #define FROM_LEFT_1ST_BUTTON_PRESSED    0x0001
   #endif
   #ifndef RIGHTMOST_BUTTON_PRESSED
      #define RIGHTMOST_BUTTON_PRESSED        0x0002
   #endif
   #ifndef MOUSE_MOVED
      #define MOUSE_MOVED                     0x0001
   #endif
   #ifndef DOUBLE_CLICK
      #define DOUBLE_CLICK                    0x0002
   #endif
#endif

/* *********************************************************************** */

#define MK_SCREEN_UPDATE() HB_GT_FUNC(gt_ScreenUpdate())

static DWORD s_dwAltGrBits;        /* JC: used to verify ALT+GR on different platforms */
static BOOL s_bBreak;            /* Used to signal Ctrl+Break to hb_inkeyPoll() */
static USHORT s_uiDispCount;
static USHORT s_usCursorStyle;
static USHORT s_usOldCurStyle;
static SHORT s_sCurRow;
static SHORT s_sCurCol;
static USHORT s_usUpdtTop;
static USHORT s_usUpdtBottom;
static USHORT s_usUpdtLeft;
static USHORT s_usUpdtRight;
static CHAR_INFO * s_pCharInfoScreen = NULL;

static int s_iStdIn, s_iStdOut, s_iStdErr;

static HANDLE s_HInput  = INVALID_HANDLE_VALUE;
static HANDLE s_HOutput = INVALID_HANDLE_VALUE;
static CONSOLE_SCREEN_BUFFER_INFO s_csbi,     /* active screen mode */
                                  s_origCsbi; /* to restore screen mode on exit */

#define INPUT_BUFFER_LEN 128

static DWORD        s_cNumRead;   /* Ok to use DWORD here, because this is specific... */
static DWORD        s_cNumIndex;  /* ...to the Windows API, which defines DWORD, etc.  */
static WORD         s_wRepeated = 0;  /* number of times the event (key) was repeated */
static INPUT_RECORD s_irInBuf[ INPUT_BUFFER_LEN ];
int    s_mouseLast;  /* Last mouse button to be pressed                     */

extern int hb_mouse_iCol;
extern int hb_mouse_iRow;

typedef struct tag_ClipKeyCode {
    int key;
    int alt_key;
    int ctrl_key;
    int shift_key;
} ClipKeyCode;

#define CLIP_STDKEY_COUNT      96
#define CLIP_EXTKEY_COUNT      30

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
    {'[',                  0,             0,         0}, /*  91 */
    {'\\',                 0,             0,         0}, /*  92 */
    {']',                  0,             0,         0}, /*  93 */
    {'^',            K_ALT_6,             0,         0}, /*  94 */
    {'_',                  0,             0,         0}, /*  95 */
    {'`',                  0,             0,         0}, /*  96 */
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

#define EXKEY_F1              ( 0 )
#define EXKEY_F2              ( 1 )
#define EXKEY_F3              ( 2 )
#define EXKEY_F4              ( 3 )
#define EXKEY_F5              ( 4 )
#define EXKEY_F6              ( 5 )
#define EXKEY_F7              ( 6 )
#define EXKEY_F8              ( 7 )
#define EXKEY_F9              ( 8 )
#define EXKEY_F10             ( 9 )
#define EXKEY_F11             (10 )
#define EXKEY_F12             (11 )
#define EXKEY_UP              (12 )
#define EXKEY_DOWN            (13 )
#define EXKEY_LEFT            (14 )
#define EXKEY_RIGHT           (15 )
#define EXKEY_INS             (16 )
#define EXKEY_DEL             (17 )
#define EXKEY_HOME            (18 )
#define EXKEY_END             (19 )
#define EXKEY_PGUP            (20 )
#define EXKEY_PGDN            (21 )
#define EXKEY_BS              (22 )
#define EXKEY_TAB             (23 )
#define EXKEY_ESC             (24 )
#define EXKEY_ENTER           (25 )
#define EXKEY_KPENTER         (26 )
#define EXKEY_CENTER          (27 )
#define EXKEY_PRTSCR          (28 )
#define EXKEY_PAUSE           (29 )

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
    {K_INS,        K_ALT_INS,    K_CTRL_INS,         0}, /*  16 */
    {K_DEL,        K_ALT_DEL,    K_CTRL_DEL,         0}, /*  17 */
    {K_HOME,      K_ALT_HOME,   K_CTRL_HOME,         0}, /*  18 */
    {K_END,        K_ALT_END,    K_CTRL_END,         0}, /*  19 */
    {K_PGUP,      K_ALT_PGUP,   K_CTRL_PGUP,         0}, /*  20 */
    {K_PGDN,      K_ALT_PGDN,   K_CTRL_PGDN,         0}, /*  21 */
    {K_BS,          K_ALT_BS,           127,         0}, /*  22 */
    {K_TAB,        K_ALT_TAB,    K_CTRL_TAB,  K_SH_TAB}, /*  23 */
    {K_ESC,        K_ALT_ESC,         K_ESC,         0}, /*  24 */
    {K_ENTER,    K_ALT_ENTER,  K_CTRL_ENTER,         0}, /*  25 */
    {K_ENTER,   KP_ALT_ENTER,  K_CTRL_ENTER,         0}, /*  26 */
    {KP_CENTER,            0,     KP_CTRL_5,         0}, /*  27 */
    {0,                    0, K_CTRL_PRTSCR,         0}, /*  28 */
    {0,                    0, HB_BREAK_FLAG,         0}  /*  29 */
};

/* *********************************************************************** */

static void HB_GT_FUNC(gt_xSetCursorPos( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_xSetCursorPos()"));

    s_csbi.dwCursorPosition.Y = s_sCurRow;
    s_csbi.dwCursorPosition.X = s_sCurCol;
    SetConsoleCursorPosition( s_HOutput, s_csbi.dwCursorPosition );
}

/* *********************************************************************** */

static void HB_GT_FUNC(gt_xSetCursorStyle( void ))
{
    CONSOLE_CURSOR_INFO cci;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_xSetCursorStyle(%hu)", s_usCursorStyle));

    GetConsoleCursorInfo( s_HOutput, &cci );

    switch( s_usCursorStyle )
    {
    case SC_NONE:
//        cci.dwSize = 25;
        cci.bVisible = FALSE;
        break;

    case SC_INSERT:
        cci.bVisible = TRUE;
        cci.dwSize = 50;
        break;

    case SC_SPECIAL1:
        cci.bVisible = TRUE;
        cci.dwSize = 99;
        break;

    case SC_SPECIAL2:
        cci.bVisible = TRUE;
        cci.dwSize = 66;
        /* In their infinite wisdom, MS doesn't support cursors that
           don't start at the bottom of the cell */
        break;

    case SC_NORMAL:
    default:
        cci.bVisible = TRUE;
#ifndef HB_GTWIN_NORMAL_CURSOR
        cci.dwSize = 25;  /* this was 12, but when used in full screen dos window
                             cursor state is erratic  - doesn't turn off, etc.
			     09-10-2002 druzus: I hope now it's OK.
			     09-14-2003 ptucker:Not really....
                                        Make your case before changing this */
#else
        cci.dwSize = 12;
#endif
        break;
    }
    s_usOldCurStyle = s_usCursorStyle;
    SetConsoleCursorInfo( s_HOutput, &cci );
}

/* *********************************************************************** */

static void HB_GT_FUNC(gt_xScreenUpdate( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_xScreenUpdate()"));

    if ( s_pCharInfoScreen != NULL )
    {
        if ( s_uiDispCount == 0 && s_usUpdtTop <= s_usUpdtBottom )
        {
            COORD coDest, coSize;
            SMALL_RECT srWin;

            coSize.Y = s_csbi.dwSize.Y;
            coSize.X = s_csbi.dwSize.X;
            coDest.Y = s_usUpdtTop;
            coDest.X = s_usUpdtLeft;
            srWin.Top    = ( SHORT ) s_usUpdtTop;
            srWin.Left   = ( SHORT ) s_usUpdtLeft;
            srWin.Bottom = ( SHORT ) s_usUpdtBottom;
            srWin.Right  = ( SHORT ) s_usUpdtRight;

            WriteConsoleOutput( s_HOutput,         /* output handle */
                                s_pCharInfoScreen, /* data to write */
                                coSize,            /* col/row size of source buffer */
                                coDest,            /* upper-left cell to write data from in src */
                                &srWin );          /* screen buffer rect to write data to */

            s_usUpdtTop = s_csbi.dwSize.Y;
            s_usUpdtLeft = s_csbi.dwSize.X;
            s_usUpdtBottom = s_usUpdtRight = 0;
        }

        if ( s_usOldCurStyle != s_usCursorStyle &&
             ( s_uiDispCount == 0 || s_usCursorStyle == SC_NONE ) )
            HB_GT_FUNC(gt_xSetCursorStyle());

        if ( s_usCursorStyle != SC_NONE && s_uiDispCount == 0 &&
             ( s_csbi.dwCursorPosition.Y != s_sCurRow ||
               s_csbi.dwCursorPosition.X != s_sCurCol ) )
            HB_GT_FUNC(gt_xSetCursorPos());

    }
}

/* *********************************************************************** */

static void HB_GT_FUNC(gt_xUpdtSet( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_xUpdtSet(%hu, %hu, %hu, %hu)", usTop, usLeft, usBottom, usRight));

    if ( usTop < s_usUpdtTop )
        s_usUpdtTop = usTop;
    if ( usLeft < s_usUpdtLeft )
        s_usUpdtLeft = usLeft;
    if ( usBottom > s_usUpdtBottom )
        s_usUpdtBottom = HB_MIN( usBottom, ( USHORT )s_csbi.dwSize.Y - 1);
    if ( usRight > s_usUpdtRight )
        s_usUpdtRight = HB_MIN( usRight, ( USHORT )s_csbi.dwSize.X - 1);
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_ScreenUpdate( void ))
{
    HB_GT_FUNC(gt_xScreenUpdate();)
}

/* *********************************************************************** */

/* *********************************************************************** */

static BOOL WINAPI HB_GT_FUNC(gt_CtrlHandler( DWORD dwCtrlType ))
{
   BOOL bHandled;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_CtrlHandler(%lu)", (ULONG) dwCtrlType));

   switch( dwCtrlType )
   {
   case CTRL_C_EVENT:
      bHandled = FALSE;
      break;

   case CTRL_BREAK_EVENT:
      s_bBreak = TRUE;
      bHandled = TRUE;
      break;

   case CTRL_CLOSE_EVENT:
   case CTRL_LOGOFF_EVENT:
   case CTRL_SHUTDOWN_EVENT:
   default:
      bHandled = FALSE;
   }

   return bHandled;
}

/* *********************************************************************** */

static void HB_GT_FUNC(gt_xInitScreenParam( void ))
{
    COORD coDest = { 0, 0 };

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_xInitScreenParam()"));

    if ( s_pCharInfoScreen != NULL )
        hb_xfree( s_pCharInfoScreen );

    if (GetConsoleScreenBufferInfo( s_HOutput, &s_csbi ))
        s_pCharInfoScreen = ( CHAR_INFO * ) hb_xgrab( s_csbi.dwSize.X *
                                                      s_csbi.dwSize.Y *
                                                      sizeof( CHAR_INFO ) );
    s_sCurRow = s_csbi.dwCursorPosition.Y;
    s_sCurCol = s_csbi.dwCursorPosition.X;
    s_usUpdtTop = s_csbi.dwSize.Y;
    s_usUpdtLeft = s_csbi.dwSize.X;
    s_usUpdtBottom = s_usUpdtRight = 0;

    /* read the screen rectangle into the buffer */
    if ( s_pCharInfoScreen != NULL )
        ReadConsoleOutput( s_HOutput,          /* screen handle */
                           s_pCharInfoScreen,  /* transfer area */
                           s_csbi.dwSize,      /* size of destination buffer */
                           coDest,             /* upper-left cell to write data to */
                           &s_csbi.srWindow);  /* screen buffer rectangle to read from */
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr ))
{
    OSVERSIONINFO osv;

    /* If Windows 95 or 98, use w9xTone for BCC32, MSVC */
    osv.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&osv);
    if (osv.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS)
    {
        s_dwAltGrBits = RIGHT_ALT_PRESSED;
    }
    else
    {
        s_dwAltGrBits = LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED;
    }

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

    /* stdin && stdout && stderr */
    s_iStdIn  = iFilenoStdin;
    s_iStdOut = iFilenoStdout;
    s_iStdErr = iFilenoStderr;

    s_bBreak = FALSE;
    s_cNumRead = 0;
    s_cNumIndex = 0;
    s_uiDispCount = 0;
    s_usOldCurStyle = s_usCursorStyle = SC_NORMAL;

    /* Add Ctrl+Break handler [vszakats] */
    SetConsoleCtrlHandler( HB_GT_FUNC(gt_CtrlHandler), TRUE );

    if( ( s_HInput = GetStdHandle( STD_INPUT_HANDLE ) ) == INVALID_HANDLE_VALUE )
    {
#ifdef HB_MULTI_GT
        AllocConsole(); /* It is a Windows app without a console, so we create one */
        s_HInput = GetStdHandle( STD_INPUT_HANDLE );
#else
        if( hb_dynsymFindName( "__DBGENTRY" ) ) /* the debugger is linked */
        {
            AllocConsole(); /* It is a Windows app without a console, so we create one */
            s_HInput = GetStdHandle( STD_INPUT_HANDLE );
        }
#endif
    }

    s_HOutput = CreateFile( "CONOUT$",     /* filename    */
                     GENERIC_READ    | GENERIC_WRITE,       /* Access flag */
                     FILE_SHARE_READ | FILE_SHARE_WRITE,    /* share mode  */
                     NULL,                                  /* security attributes */
                     OPEN_EXISTING,                         /* create mode */
                     0, 0 );

    if( s_HOutput != INVALID_HANDLE_VALUE )
    {
        GetConsoleScreenBufferInfo( s_HOutput, &s_csbi );

        /* save screen info to restore on exit */
        memcpy( &s_origCsbi, &s_csbi, sizeof( s_csbi ) );

        s_csbi.srWindow.Top = s_csbi.srWindow.Left = 0;
        s_csbi.srWindow.Right = HB_MIN( s_csbi.srWindow.Right, s_csbi.dwSize.X-1 );
        s_csbi.srWindow.Bottom = HB_MIN( s_csbi.srWindow.Bottom, s_csbi.dwSize.Y-1 );

        SetConsoleWindowInfo( s_HOutput, TRUE,  &s_csbi.srWindow );
        SetConsoleScreenBufferSize( s_HOutput, s_csbi.dwSize );

        HB_GT_FUNC(gt_xInitScreenParam());
    }

    if( s_HInput != INVALID_HANDLE_VALUE )
    {
        if( b_MouseEnable )
        {
           /* With Mouse */
           SetConsoleMode( s_HInput, ENABLE_MOUSE_INPUT );
           HB_GT_FUNC(mouse_Init());
        }
        else
           /* NOMOUSE */
           SetConsoleMode( s_HInput, 0x00000 );
    }
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Exit( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

    if ( s_uiDispCount > 0 )
    {
        s_uiDispCount = 0;
        MK_SCREEN_UPDATE();
    }

    if ( s_pCharInfoScreen != NULL )
    {
        hb_xfree( s_pCharInfoScreen );
        s_pCharInfoScreen = NULL;
    }

    if( s_HOutput != INVALID_HANDLE_VALUE )
    {
        SetConsoleScreenBufferSize( s_HOutput, s_origCsbi.dwSize );

        s_origCsbi.srWindow.Right -= s_origCsbi.srWindow.Left;
        s_origCsbi.srWindow.Bottom -= s_origCsbi.srWindow.Top;
        s_origCsbi.srWindow.Top = s_origCsbi.srWindow.Left = 0;

        SetConsoleWindowInfo( s_HOutput, TRUE, &s_origCsbi.srWindow );

        CloseHandle( s_HOutput );
    }
    /* Remove Ctrl+Break handler */
    SetConsoleCtrlHandler( HB_GT_FUNC(gt_CtrlHandler), FALSE );

    if( b_MouseEnable )
       HB_GT_FUNC(mouse_Exit());
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_GetScreenWidth( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

    return s_csbi.dwSize.X;
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_GetScreenHeight( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

    return s_csbi.dwSize.Y;
}

/* *********************************************************************** */

SHORT HB_GT_FUNC(gt_Col( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

    return s_sCurCol;
}

/* *********************************************************************** */

SHORT HB_GT_FUNC(gt_Row( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

    return s_sCurRow;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetPos( SHORT sRow, SHORT sCol, SHORT sMethod ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", sRow, sCol, sMethod));

    s_sCurRow = sRow;
    s_sCurCol = sCol;

    HB_SYMBOL_UNUSED( sMethod );

    MK_SCREEN_UPDATE();
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_AdjustPos( BYTE * pStr, ULONG ulLen ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

    HB_SYMBOL_UNUSED( pStr );
    HB_SYMBOL_UNUSED( ulLen );

    return TRUE;
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_IsColor( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

    /* TODO: need to call something to do this instead of returning TRUE */
    return TRUE;
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_GetCursorStyle( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

    return s_usCursorStyle;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetCursorStyle( USHORT usStyle ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", usStyle));

    switch( usStyle )
    {
    case SC_NONE:
    case SC_NORMAL:
    case SC_INSERT:
    case SC_SPECIAL1:
    case SC_SPECIAL2:
        s_usCursorStyle = usStyle;
        break;

    default:
        break;
    }

    MK_SCREEN_UPDATE();
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

    if ( s_uiDispCount > 0 )
        --s_uiDispCount;

    MK_SCREEN_UPDATE();
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_DispCount())
{
    return s_uiDispCount;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Puts( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE *pbyStr, ULONG ulLen ))
{
    int i, j; USHORT l, r, u;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", usRow, usCol, (int) byAttr, pbyStr, ulLen));

    if( ulLen > 0 && s_pCharInfoScreen != NULL )
    {
        i = ( int ) ulLen;
        j = ( int ) ( usRow * s_csbi.dwSize.X + usCol );

        if ( i > s_csbi.dwSize.Y * s_csbi.dwSize.X - j )
            i = s_csbi.dwSize.Y * s_csbi.dwSize.X - j;

        if ( i > 0 )
        {
            u = usRow + ( i + usCol - 1 ) / s_csbi.dwSize.X;
            if ( u > usRow )
            {
                l = 0;
                r = s_csbi.dwSize.X - 1;
            }
            else
            {
                l = usCol;
                r = i + usCol - 1;
            }
            while( i-- )
            {
                s_pCharInfoScreen[j].Char.AsciiChar = ( CHAR ) *pbyStr++;
                s_pCharInfoScreen[j].Attributes = ( WORD )( byAttr & 0xFF );
                ++j;
            }
            HB_GT_FUNC(gt_xUpdtSet( usRow, l, u, r ));
            MK_SCREEN_UPDATE();
        }
    }
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Replicate( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE byChar, ULONG ulLen ))
{
    int i, j; USHORT l, r, u;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", usRow, usCol, byAttr, byChar, ulLen));

    if( ulLen > 0 && s_pCharInfoScreen != NULL )
    {
        i = ( int ) ulLen;
        j = ( int ) ( usRow * s_csbi.dwSize.X + usCol );

        if ( i > s_csbi.dwSize.Y * s_csbi.dwSize.X - j )
            i = s_csbi.dwSize.Y * s_csbi.dwSize.X - j;

        if ( i > 0 )
        {
            u = usRow + ( i + usCol - 1 ) / s_csbi.dwSize.X;
            if ( u > usRow )
            {
                l = 0;
                r = s_csbi.dwSize.X - 1;
            }
            else
            {
                l = usCol;
                r = i + usCol - 1;
            }
            while( i-- )
            {
                s_pCharInfoScreen[j].Char.AsciiChar = ( CHAR ) byChar;
                s_pCharInfoScreen[j].Attributes = ( WORD )( byAttr & 0xFF );
                ++j;
            }
            HB_GT_FUNC(gt_xUpdtSet( usRow, l, u, r ));
            MK_SCREEN_UPDATE();
        }
    }
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_RectSize( USHORT rows, USHORT cols ))
{
    return rows * cols * 2;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_GetText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * pbyDst ))
{
    USHORT x,y;
    ULONG  i;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRight, pbyDst));

    if ( s_pCharInfoScreen != NULL )
    {
        if ( usBottom >= s_csbi.dwSize.Y )
            usBottom = s_csbi.dwSize.Y - 1;

        if ( usRight >= s_csbi.dwSize.X )
            usRight = s_csbi.dwSize.X - 1;

        for( y = usTop; y <= usBottom; y++ )
        {
            i = y * s_csbi.dwSize.X;
            for( x = usLeft; x <= usRight; x++ )
            {
                *(pbyDst++) = (BYTE) s_pCharInfoScreen[i+x].Char.AsciiChar;
                *(pbyDst++) = (BYTE) s_pCharInfoScreen[i+x].Attributes;
            }
        }
    }
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_PutText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * pbySrc ))
{
    USHORT x,y;
    ULONG  i;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRight, pbySrc));

    if ( s_pCharInfoScreen != NULL )
    {
        if ( usBottom >= s_csbi.dwSize.Y )
            usBottom = s_csbi.dwSize.Y - 1;

        if ( usRight >= s_csbi.dwSize.X )
            usRight = s_csbi.dwSize.X - 1;

        if ( usTop <= usBottom && usLeft <= usRight )
        {
            for( y = usTop; y <= usBottom; y++ )
            {
                i = y * s_csbi.dwSize.X;
                for( x = usLeft; x <= usRight; x++ )
                {
                    s_pCharInfoScreen[i+x].Char.AsciiChar = ( CHAR ) *(pbySrc++);
                    s_pCharInfoScreen[i+x].Attributes = ( WORD ) *(pbySrc++);
                }
            }
        }
        HB_GT_FUNC(gt_xUpdtSet( usTop, usLeft, usBottom, usRight ));
        MK_SCREEN_UPDATE();
    }
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetAttribute( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr ))
{
    USHORT x, y;
    ULONG i;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d", usTop, usLeft, usBottom, usRight, (int) attr));

    if ( s_pCharInfoScreen != NULL )
    {
        if ( usBottom >= s_csbi.dwSize.Y )
            usBottom = s_csbi.dwSize.Y - 1;

        if ( usRight >= s_csbi.dwSize.X )
            usRight = s_csbi.dwSize.X - 1;

        if ( usTop <= usBottom && usLeft <= usRight )
        {
            for( y = usTop; y <= usBottom; y++ )
            {
                i = y * s_csbi.dwSize.X;
                for( x = usLeft; x <= usRight; x++ )
                    s_pCharInfoScreen[i+x].Attributes = ( WORD )( attr & 0xFF );
            }
            HB_GT_FUNC(gt_xUpdtSet( usTop, usLeft, usBottom, usRight ));
            MK_SCREEN_UPDATE();
        }
    }
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE byAttr, SHORT iRows, SHORT iCols ))
{
    SHORT usSaveRow, usSaveCol;
    UINT uiSize;

    int iLength = ( usRight - usLeft ) + 1;
    int iCount, iColOld, iColNew, iColSize;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRight, (int) byAttr, iRows, iCols));

    if( hb_gtRectSize( usTop, usLeft, usBottom, usRight, &uiSize ) == 0 )
    {
        BYTE * fpBlank = ( BYTE * ) hb_xgrab( iLength );
        BYTE * fpBuff = ( BYTE * ) hb_xgrab( iLength * 2 );

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

        /* this is probably not compatible with Clipper */
        HB_GT_FUNC(gt_DispBegin());

        hb_gtGetPos( &usSaveRow, &usSaveCol );

        for( iCount = ( iRows >= 0 ? usTop : usBottom );
             ( iRows >= 0 ? iCount <= usBottom : iCount >= usTop );
             ( iRows >= 0 ? iCount++ : iCount-- ) )
        {
            int iRowPos = iCount + iRows;

            /* Read the text to be scrolled into the current row */
            if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
                HB_GT_FUNC(gt_GetText( iRowPos, iColOld, iRowPos, iColOld + iColSize, fpBuff ));

            /* Blank the scroll region in the current row */
            HB_GT_FUNC(gt_Puts( iCount, usLeft, byAttr, fpBlank, iLength ));

            /* Write the scrolled text to the current row */
            if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
                HB_GT_FUNC(gt_PutText( iCount, iColNew, iCount, iColNew + iColSize, fpBuff ));
        }

        hb_xfree( fpBlank );
        hb_xfree( fpBuff );

        hb_gtSetPos( usSaveRow, usSaveCol );
        /* HB_GT_FUNC(gt_SetPos( usSaveRow, usSaveCol, HB_GT_SET_POS_AFTER )); */

        /* this is probably not compatible with Clipper */
        HB_GT_FUNC(gt_DispEnd());
        MK_SCREEN_UPDATE();
    }
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_SetMode( USHORT usRows, USHORT usCols ))
{
    BOOL Ret = FALSE;
    SMALL_RECT srWin;
    COORD coBuf;
    USHORT uiDispCount = s_uiDispCount;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", usRows, usCols));

    if( s_HOutput != INVALID_HANDLE_VALUE )
    {
        while( s_uiDispCount )
            HB_GT_FUNC(gt_DispEnd());

        coBuf = GetLargestConsoleWindowSize( s_HOutput );
        if ( usRows > coBuf.Y )
            usRows = coBuf.Y;
        else
            coBuf.Y = usRows;  /* Thx to Peter Rees */

        if ( usCols > coBuf.X )
            usCols = coBuf.X;
        else
            coBuf.X = usCols;

        /* new console window size and scroll position */
        srWin.Top    = srWin.Left = 0;
        srWin.Bottom = ( SHORT ) ( usRows - 1 );
        srWin.Right  = ( SHORT ) ( usCols - 1 );

        /* if the current buffer is larger than what we want, resize the */
        /* console window first, then the buffer */
        if( ( DWORD ) s_csbi.dwSize.X * s_csbi.dwSize.Y > ( DWORD ) usCols * usRows )
        {
            if ( SetConsoleWindowInfo( s_HOutput, TRUE, &srWin ) )
            {
                SetConsoleScreenBufferSize( s_HOutput, coBuf );
                Ret = TRUE;
            }
        }
        else
        {
            if ( SetConsoleScreenBufferSize( s_HOutput, coBuf ) )
            {
                SetConsoleWindowInfo( s_HOutput, TRUE, &srWin );
                Ret = TRUE;
            }
        }

        if ( Ret )
            HB_GT_FUNC(gt_xInitScreenParam());

        while( s_uiDispCount < uiDispCount )
            HB_GT_FUNC(gt_DispBegin());
    }
    return ( Ret );
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_GetBlink())
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

    /* TODO */
    return TRUE;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetBlink( BOOL bBlink ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

    /* TODO: set the bit if it's supported */
    HB_SYMBOL_UNUSED( bBlink );
}

/* *********************************************************************** */

char * HB_GT_FUNC(gt_Version( void ))
{
    return "Harbour Terminal: Win32 buffered console";
}

/* *********************************************************************** */

static void HB_GT_FUNC(gt_xPutch( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE byChar ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %i)", usRow, usCol, (int) byAttr, byChar));

    if ( s_pCharInfoScreen != NULL &&
         usRow < s_csbi.dwSize.Y && usCol < s_csbi.dwSize.X )
    {
        int i = ( int ) ( usRow * s_csbi.dwSize.X + usCol );

        s_pCharInfoScreen[i].Char.AsciiChar = ( CHAR ) byChar;
        s_pCharInfoScreen[i].Attributes = ( WORD )( byAttr & 0xFF );

        HB_GT_FUNC(gt_xUpdtSet( usRow, usCol, usRow, usCol ));
    }
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_Box( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
                          BYTE * szBox, BYTE byAttr ))
{
    USHORT ret = 1;
    SHORT Row;
    SHORT Col;
    SHORT Height;
    SHORT Width;
    USHORT sWidth = HB_GT_FUNC(gt_GetScreenWidth()),
          sHeight = HB_GT_FUNC(gt_GetScreenHeight());

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
            HB_GT_FUNC(gt_xPutch( Top, Left, byAttr, szBox[ 0 ] )); /* Upper left corner */

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
            HB_GT_FUNC(gt_Replicate( Top, Col, byAttr, szBox[ 1 ], Width + ( (Right - Left) > 1 ? -2 : 0 ) )); /* Top line */

        if( Height > 1 &&
               (Right - Left) > 1 && Right < sWidth &&
               Top >= 0 && Top < sHeight )
            HB_GT_FUNC(gt_xPutch( Top, Right, byAttr, szBox[ 2 ] )); /* Upper right corner */

        if( szBox[ 8 ] && Height > 2 && Width > 2 )
        {
            for( Row = Top + 1; Row < Bottom; Row++ )
            {
                if( Row >= 0 && Row < sHeight )
                {
                    Col = Left;
                    if( Col < 0 )
                        Col = 0; /* The width was corrected earlier. */
                    else
                        HB_GT_FUNC(gt_xPutch( Row, Col++, byAttr, szBox[ 7 ] )); /* Left side */
                    HB_GT_FUNC(gt_Replicate( Row, Col, byAttr, szBox[ 8 ], Width - 2 )); /* Fill */
                    if( Right < sWidth )
                        HB_GT_FUNC(gt_xPutch( Row, Right, byAttr, szBox[ 3 ] )); /* Right side */
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
                        HB_GT_FUNC(gt_xPutch( Row, Left, byAttr, szBox[ 7 ] )); /* Left side */
                    if( ( Width > 1 || Left < 0 ) && Right < sWidth )
                        HB_GT_FUNC(gt_xPutch( Row, Right, byAttr, szBox[ 3 ] )); /* Right side */
                }
            }
        }

        if( Height > 1 && Width > 1 )
        {
            if( Left >= 0 && Bottom < sHeight )
                HB_GT_FUNC(gt_xPutch( Bottom, Left, byAttr, szBox[ 6 ] )); /* Bottom left corner */

            Col = Left + 1;
            if( Col < 0 )
                Col = 0; /* The width was corrected earlier. */

            if( Col <= Right && Bottom < sHeight )
                HB_GT_FUNC(gt_Replicate( Bottom, Col, byAttr, szBox[ 5 ], Width - 2 )); /* Bottom line */

            if( Right < sWidth && Bottom < sHeight )
                HB_GT_FUNC(gt_xPutch( Bottom, Right, byAttr, szBox[ 4 ] )); /* Bottom right corner */
        }

        HB_GT_FUNC(gt_DispEnd());
        MK_SCREEN_UPDATE();
        ret = 0;
    }

    return ret;
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_BoxD( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr ))
{
    return HB_GT_FUNC(gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr ));
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_BoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr ))
{
    return HB_GT_FUNC(gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr ));
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_HorizLine( SHORT Row, SHORT Left, SHORT Right, BYTE byChar, BYTE byAttr ))
{
    USHORT ret = 1;
    if( Row >= 0 && Row < HB_GT_FUNC(gt_GetScreenHeight()) )
    {
        if( Left < 0 )
            Left = 0;
        else if( Left >= HB_GT_FUNC(gt_GetScreenWidth()) )
            Left = HB_GT_FUNC(gt_GetScreenWidth()) - 1;

        if( Right < 0 )
            Right = 0;
        else if( Right >= HB_GT_FUNC(gt_GetScreenWidth()) )
            Right = HB_GT_FUNC(gt_GetScreenWidth()) - 1;

        if( Left < Right )
            HB_GT_FUNC(gt_Replicate( Row, Left, byAttr, byChar, Right - Left + 1 ));
        else
            HB_GT_FUNC(gt_Replicate( Row, Right, byAttr, byChar, Left - Right + 1 ));
        ret = 0;
    }
    return ret;
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_VertLine( SHORT Col, SHORT Top, SHORT Bottom, BYTE byChar, BYTE byAttr ))
{
    USHORT ret = 1;
    SHORT Row;

    if( Col >= 0 && Col < HB_GT_FUNC(gt_GetScreenWidth()) )
    {
        if( Top < 0 )
            Top = 0;
        else if( Top >= HB_GT_FUNC(gt_GetScreenHeight()) )
            Top = HB_GT_FUNC(gt_GetScreenHeight()) - 1;

        if( Bottom < 0 )
            Bottom = 0;
        else if( Bottom >= HB_GT_FUNC(gt_GetScreenHeight()) )
            Bottom = HB_GT_FUNC(gt_GetScreenHeight()) - 1;

        if( Top <= Bottom )
            Row = Top;
        else
        {
            Row = Bottom;
            Bottom = Top;
        }

        HB_GT_FUNC(gt_DispBegin());

        while( Row <= Bottom )
            HB_GT_FUNC(gt_xPutch( Row++, Col, byAttr, byChar ));

        HB_GT_FUNC(gt_DispEnd());

        MK_SCREEN_UPDATE();
        ret = 0;
    }
    return ret;
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_Suspend())
{
    return TRUE;
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_Resume())
{
    return TRUE;
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_PreExt())
{
    return TRUE;
}

/* *********************************************************************** */

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
    return( FALSE );
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_ReadKey( HB_inkey_enum eventmask ))
{
    int ch = 0, extKey = -1;
    const ClipKeyCode *clipKey = NULL;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

    /* First check for Ctrl+Break, which is handled by gt/gtwin.c */
    if( s_bBreak )
    {
       /* Reset the global Ctrl+Break flag */
       s_bBreak = FALSE;
       ch = HB_BREAK_FLAG; /* Indicate that Ctrl+Break was pressed */
    }
    /* Check for events only when the event buffer is exhausted. */
    else if( s_wRepeated == 0 && s_cNumRead <= s_cNumIndex )
    {
       /* Check for keyboard input */
       s_cNumRead = 0;
       GetNumberOfConsoleInputEvents( s_HInput, &s_cNumRead );
       if( s_cNumRead )
       {
          /* Read keyboard input */
          ReadConsoleInput(
             s_HInput,         /* input buffer handle    */
             s_irInBuf,        /* buffer to read into    */
             INPUT_BUFFER_LEN, /* size of read buffer    */
             &s_cNumRead);     /* number of records read */
          /* Set up to process the first input event */
          s_cNumIndex = 0;
       }
    }
    
    /* Only process one keyboard event at a time. */
    if( s_wRepeated > 0 || s_cNumRead > s_cNumIndex )
    {
       if( s_irInBuf[ s_cNumIndex ].EventType == KEY_EVENT )
       {
          /* Only process key down events */
          if( s_irInBuf[ s_cNumIndex ].Event.KeyEvent.bKeyDown )
          {
             /* Save the keyboard state and ASCII key code */
             DWORD dwState = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.dwControlKeyState;
             WORD wChar = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualKeyCode;
             /* Could be used in the future */
             /* WORD wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualScanCode; */
             ch = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.uChar.AsciiChar;
           
             if ( s_wRepeated == 0 )
             { 
                s_wRepeated = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wRepeatCount;
             } 
             if ( s_wRepeated > 0 ) /* Might not be redundant */
             {
                s_wRepeated--;
             }

             // printf( "\n\nhb_gt_ReadKey(): dwState is %ld, wChar is %d, wKey is %d, ch is %d", dwState, wChar, wKey, ch ); 

             if ( wChar == 8 )
             {
                extKey = EXKEY_BS;
             }
             else if ( wChar == 9 )
             {
                extKey = EXKEY_TAB;
             }
             else if ( wChar == 13 )
             {
                extKey = EXKEY_ENTER;
             }
             else if ( wChar == 27 )
             {
                extKey = EXKEY_ESC;
             }
             else if ( wChar >= 112 && wChar <= 123 ) // F1-F12
             {
                extKey = wChar - 112; 
             }
             else if ( wChar == 33 )
             {
                extKey = EXKEY_PGUP;
             }
             else if ( wChar == 34 )
             {
                extKey = EXKEY_PGDN;
             }
             else if ( wChar == 35 )
             {
                extKey = EXKEY_END;
             }
             else if ( wChar == 36 )
             {
                extKey = EXKEY_HOME;
             }
             else if ( wChar == 37 )
             {
                extKey = EXKEY_LEFT;
             }
             else if ( wChar == 38 )
             {
                extKey = EXKEY_UP;
             }
             else if ( wChar == 39 )
             {
                extKey = EXKEY_RIGHT;
             }
             else if ( wChar == 40 )
             {
                extKey = EXKEY_DOWN;
             }
             else if ( wChar == 45 )
             {
                extKey = EXKEY_INS;
             }
             else if ( wChar == 46 )
             {
                extKey = EXKEY_DEL;
             }
             else if ( ch >= K_SPACE && ch <= K_CTRL_BS )
             {
                clipKey = &stdKeyTab[ ch - K_SPACE ];
             }
             else if ( ch > 0 && ch < K_SPACE && ( dwState & ( LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED ) ) )
             {
                clipKey = &stdKeyTab[ ch + '@' ];
             }
             else if ( ch < 0 ) // international keys
             {
                ch += 256;
             }

             if ( extKey > -1 )
             {
                clipKey = &extKeyTab[ extKey ];
             }
             
             if ( clipKey != NULL )
             {
                if( ( dwState & SHIFT_PRESSED ) && ( dwState & ( LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED ) ) )               
                {
                   if( clipKey->key == K_TAB )
                   {
                      ch = K_CTRL_SH_TAB;
                   }
                }
                else if( dwState & ( LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED ) )
                {
                   ch = clipKey->alt_key;
                }
                else if( dwState & ( LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED ) )
                {
                   ch = clipKey->ctrl_key;
                }
                else if( dwState & SHIFT_PRESSED )
                {
                   ch = clipKey->shift_key;
                }                
                else
                {
                   ch = clipKey->key;
                }
                if( ch == 0 ) // for keys that are only on shift or AltGr 
                {
                   ch = clipKey->key;
                }
             }
          }
       }
       else if( b_MouseEnable && eventmask & ~( INKEY_KEYBOARD | INKEY_RAW )
                         && s_irInBuf[ s_cNumIndex ].EventType == MOUSE_EVENT )

       {

          hb_mouse_iCol = s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwMousePosition.X;
          hb_mouse_iRow = s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwMousePosition.Y;

          if( eventmask & INKEY_MOVE && s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == MOUSE_MOVED )
             ch = K_MOUSEMOVE;

          else if( eventmask & INKEY_LDOWN && s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState &
                   FROM_LEFT_1ST_BUTTON_PRESSED )
          {
             if( s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == DOUBLE_CLICK )
                ch = K_LDBLCLK;
             else
                ch = K_LBUTTONDOWN;

             s_mouseLast = K_LBUTTONDOWN;
          }
          else if( eventmask & INKEY_RDOWN && s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState &
                   RIGHTMOST_BUTTON_PRESSED )
          {
             if( s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == DOUBLE_CLICK )
                ch = K_RDBLCLK;
             else
                ch = K_RBUTTONDOWN;

             s_mouseLast = K_RBUTTONDOWN;
          }
          else if( s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == 0 &&
                   s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState == 0 )
          {
             if( eventmask & INKEY_LUP && s_mouseLast == K_LBUTTONDOWN )
                ch = K_LBUTTONUP;
             else if( eventmask & INKEY_RUP && s_mouseLast == K_RBUTTONDOWN )
                ch = K_RBUTTONUP;
          }
       }
       /* Set up to process the next input event (if any) */
       if ( s_wRepeated == 0 )
          s_cNumIndex++;
    }

    return ch;
}

/* *********************************************************************** */

#if defined(__BORLANDC__) || defined(_MSC_VER)
static int hb_Inp9x( USHORT usPort )
{
  USHORT usVal;

    HB_TRACE(HB_TR_DEBUG, ("hb_Inp9x(%hu)", usPort));

    #if defined(__BORLANDC__)
       _DX = usPort;
       __emit__(0xEC);        /* ASM  IN AL, DX */
       __emit__(0x32,0xE4);   /* ASM XOR AH, AH */
       usVal = _AX;
    #else

       usVal = _inp( usPort );
    #endif

    return usVal;
}

/* *********************************************************************** */

static int hb_Outp9x( USHORT usPort, USHORT usVal )
{
    HB_TRACE(HB_TR_DEBUG, ("hb_Outp9x(%hu, %hu)", usPort, usVal));

    #if defined(__BORLANDC__)
      _DX = usPort;
      _AL = usVal;
      __emit__(0xEE);        /* ASM OUT DX, AL */
      __emit__(0x32,0xE4);   /* ASM XOR AH, AH */
      usVal = _AX;
    #else
       _outp( usPort, usVal );
    #endif

    return usVal;
}

/* *********************************************************************** */
/* dDurat is in seconds */
static void HB_GT_FUNC(gt_w9xTone( double dFreq, double dDurat ))
{
    INT uLSB,uMSB;
    ULONG lAdjFreq;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_w9xtone(%lf, %lf)", dFreq, dDurat));

    /* sync with internal clock with very small time period */
    hb_idleSleep( 0.01 );

    /* Clipper ignores Tone() requests (but delays anyway) if Frequency is
       less than < 20 hz (and so should we) to maintain compatibility .. */

    if ( dFreq >= 20.0 )
    {
      /* Setup Sound Control Port Registers and timer channel 2 */
      hb_Outp9x(67, 182) ;

      lAdjFreq = (ULONG)( 1193180 / dFreq ) ;

      if( (LONG) lAdjFreq < 0 )
         uLSB = lAdjFreq + 65536;
      else
         uLSB = lAdjFreq % 256;

      if( (LONG) lAdjFreq < 0 )
         uMSB = lAdjFreq + 65536;
      else
         uMSB = lAdjFreq / 256;


      /* set the frequency (LSB,MSB) */

      hb_Outp9x(66, uLSB);
      hb_Outp9x(66, uMSB);

      /* Get current Port setting */
      /* enable Speaker Data & Timer gate bits */
      /* (00000011B is bitmask to enable sound) */
      /* Turn on Speaker - sound Tone for duration.. */

      hb_Outp9x(97, hb_Inp9x( 97 ) | 3);

      hb_idleSleep( dDurat );

      /* Read back current Port value for Reset */
      /* disable Speaker Data & Timer gate bits */
      /* (11111100B is bitmask to disable sound) */
      /* Turn off the Speaker ! */

      hb_Outp9x(97, hb_Inp9x( 97 ) & 0xFC);

    }
    else
    {
       hb_idleSleep( dDurat );
    }
}
#endif

/* *********************************************************************** */
/* dDurat is in seconds */
static void HB_GT_FUNC(gt_wNtTone( double dFreq, double dDurat ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_wNtTone(%lf, %lf)", dFreq, dDurat));

    /* Clipper ignores Tone() requests (but delays anyway) if Frequency is
       less than < 20 hz.  Windows NT minimum is 37... */

    /* sync with internal clock with very small time period */
    hb_idleSleep( 0.01 );

    if ( dFreq >= 37.0 )
    {
       Beep( (ULONG) dFreq, (ULONG) ( dDurat * 1000 ) ); /* Beep wants Milliseconds */
    }
    else
    {
       hb_idleSleep( dDurat );
    }
}

/* *********************************************************************** */
/* dDuration is in 'Ticks' (18.2 per second) */
void HB_GT_FUNC(gt_Tone( double dFrequency, double dDuration ))
{
    OSVERSIONINFO osv;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

    /*
      According to the Clipper NG, the duration in 'ticks' is truncated to the
      interger portion  ... Depending on the platform, xHarbour allows a finer
      resolution, but the minimum is 1 tick (for compatibility)
     */
    /* Convert from ticks to seconds */
    dDuration  = ( HB_MIN( HB_MAX( 1.0, dDuration ), ULONG_MAX ) ) / 18.2;

    /* keep the frequency in an acceptable range */
    dFrequency =   HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 );

    /* What version of Windows are you running? */
    osv.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&osv);

    /* If Windows 95 or 98, use w9xTone for BCC32, MSVC */
    if (osv.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS)
    {
       #if defined(__BORLANDC__) || defined( _MSC_VER )
          HB_GT_FUNC(gt_w9xTone( dFrequency, dDuration ));
       #else
          HB_GT_FUNC(gt_wNtTone( dFrequency, dDuration ));
       #endif
    }

    /* If Windows NT or NT2k, use wNtTone, which provides TONE()
       reset sequence support (new) */
    else if (osv.dwPlatformId == VER_PLATFORM_WIN32_NT)
    {
      HB_GT_FUNC(gt_wNtTone( dFrequency, dDuration ));
    }
}

/* ************************** Clipboard support ********************************** */

void HB_GT_FUNC( gt_GetClipboard( char *szData, ULONG *pulMaxSize ) )
{
   HGLOBAL   hglb;
   LPTSTR    lptstr;

   if ( !IsClipboardFormatAvailable(CF_TEXT) )
   {
     *pulMaxSize = 0;
     return;
   }

   if (!OpenClipboard( NULL ))
   {
     *pulMaxSize = 0;
     return;
   }

   hglb = GetClipboardData(CF_TEXT);
   if (hglb != NULL)
   {
      lptstr = (LPSTR) GlobalLock(hglb);
      if (lptstr != NULL)
      {
         // int iLen = strlen( lptstr );
         ULONG iLen = strlen( lptstr );
         if ( *pulMaxSize == 0 || *pulMaxSize > iLen )
         {
            *pulMaxSize = iLen;
         }

         // still nothing ?
         if ( *pulMaxSize == 0 )
         {
            return;
         }

         memcpy( szData, lptstr, *pulMaxSize );
         szData[*pulMaxSize] = '\0';
         GlobalUnlock(hglb);
      }
   }
   CloseClipboard();
}

void HB_GT_FUNC( gt_SetClipboard( char *szData, ULONG ulSize ) )
{
   LPTSTR  lptstrCopy;
   HGLOBAL hglbCopy;
   // char *  cText;
   // int     nLen;

   if ( !IsClipboardFormatAvailable( CF_TEXT ) )
   {
     return;
   }

   if ( ! OpenClipboard( NULL ) )
   {
     hb_retl( FALSE );
     return;
   }
   EmptyClipboard();


   // Allocate a global memory object for the text.
   //
   hglbCopy = GlobalAlloc( GMEM_MOVEABLE, ( ulSize+1 ) * sizeof( TCHAR ) );
   if ( hglbCopy == NULL )
   {
       CloseClipboard();
   }

   // Lock the handle and copy the text to the buffer.
   //
   lptstrCopy = ( LPSTR ) GlobalLock( hglbCopy );
   memcpy( lptstrCopy, szData, ( ulSize+1 ) * sizeof( TCHAR ) );
   lptstrCopy[ ulSize+1 ] = ( TCHAR ) 0;    // null character
   GlobalUnlock( hglbCopy );

   // Place the handle on the clipboard.
   //
   SetClipboardData( CF_TEXT, hglbCopy );

   CloseClipboard();
}

ULONG HB_GT_FUNC( gt_GetClipboardSize( void ) )
{
   HGLOBAL   hglb;
   LPTSTR    lptstr;
   int ret;

   if ( !IsClipboardFormatAvailable(CF_TEXT) )
   {
     return 0;
   }

   if (!OpenClipboard( NULL ))
   {
     return 0;
   }

   hglb = GetClipboardData(CF_TEXT);
   ret = 0;
   if (hglb != NULL)
   {
      lptstr = (LPSTR) GlobalLock(hglb);
      if (lptstr != NULL)
      {
         ret = strlen( lptstr );
         GlobalUnlock(hglb);
      }
   }
   CloseClipboard();
   return ret;

}

/* *********************************************************************** */

int HB_GT_FUNC( gt_info(int iMsgType, BOOL bUpdate, int iParam, void *vpParam ) )
{
   HB_SYMBOL_UNUSED( bUpdate );
   HB_SYMBOL_UNUSED( iParam );
   HB_SYMBOL_UNUSED( vpParam );

   switch ( iMsgType )
   {
      case GTI_ISGRAPHIC:
         return (int) FALSE;
      case GTI_INPUTFD:
         return s_iStdIn;
      case GTI_OUTPUTFD:
         return s_iStdOut;
   }
   // DEFAULT: there's something wrong if we are here.
   return -1;
}

/* *********************************************************************** */

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
    /* extended GT functions */
//    gt_funcs->SetDispCP             = HB_GT_FUNC( gt_SetDispCP );
//    gt_funcs->SetKeyCP              = HB_GT_FUNC( gt_SetKeyCP );
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
static HB_GT_INIT gtInit = { HB_GT_DRVNAME( HB_GT_NAME ),
                             HB_GT_FUNC(gtFnInit), HB_GT_FUNC(mouseFnInit) };

HB_GT_ANNOUNCE( HB_GT_NAME );

HB_CALL_ON_STARTUP_BEGIN( _hb_startup_gt_Init_ )
   hb_gtRegister( &gtInit );
HB_CALL_ON_STARTUP_END( _hb_startup_gt_Init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_startup_gt_Init_
#elif defined(_MSC_VER)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto__hb_startup_gt_Init_ = _hb_startup_gt_Init_;
   #pragma data_seg()
#endif

#endif  /* HB_MULTI_GT */
