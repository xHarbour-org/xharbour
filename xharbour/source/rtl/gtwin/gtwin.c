/*
 * $Id: gtwin.c,v 1.40 2004/02/07 20:06:35 andijahja Exp $
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
#include "hbinkey.ch"
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


/* *********************************************************************** */

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
        cci.dwSize = 25;  /* this was 12, but when used in full screen dos window
                             cursor state is erratic  - doesn't turn off, etc.
			     09-10-2002 druzus: I hope now it's OK.
			     09-14-2003 ptucker:Not really....
                                        Make your case before changing this */
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
        unsigned char * fpBlank = ( unsigned char * ) hb_xgrab( iLength );
        unsigned char * fpBuff = ( unsigned char * ) hb_xgrab( iLength * 2 );

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
    return 1;
}

/* *********************************************************************** */

static int StdFnKeys( WORD wKey, BOOL bEnhanced )
{
   int ch;
   /* Normal function key */
   ch = wKey + HB_INKEY_NONE;
   if( bEnhanced ) ch += HB_INKEY_ENHANCED;
   return ch;
}

/* *********************************************************************** */

static int IgnoreKeyCodes( int wKey )
{
   int ignore = 0;
   switch( wKey )
   {
      /* Virtual scan codes to ignore */
      case 29: /* Ctrl */
      /* case 40: */ /* Circle Accent */
      /* case 41: */ /* Tick Accent */
      case 42: /* Left Shift */
      case 43: /* Reverse Tick Accent */
      case 54: /* Right Shift */
      case 56: /* Alt */
      case 58: /* Caps Lock */
      case 69: /* Num Lock */
      case 70: /* Pause or Scroll Lock */
         ignore = -1;
   }
   return ignore;
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_ReadKey( HB_inkey_enum eventmask ))
{
    int ch = 0, extended = 0;

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
             WORD wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualScanCode;
             ch = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.uChar.AsciiChar;
             if ( s_wRepeated == 0 )
                s_wRepeated = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wRepeatCount;
             if ( s_wRepeated > 0 ) /* Might not be redundant */
                s_wRepeated--;

             if ( dwState & CAPSLOCK_ON )
             {
                if ( dwState & SHIFT_PRESSED )
                {
                   switch( ch ) {
                      case 39:
                         ch = 34;
                         break;
                      case 44:
                         ch = 60;
                         break;
                      case 45:
                         ch = 95;
                         break;
                      case 46:
                         ch = 62;
                         break;
                      case 47:
                         ch = 63;
                         break;
                      case 48:
                         ch = 41;
                         break;
                      case 49:
                         ch = 33;
                         break;
                      case 50:
                         ch = 64;
                         break;
                      case 51:
                         ch = 35;
                         break;
                      case 52:
                         ch = 36;
                         break;
                      case 53:
                         ch = 37;
                         break;
                      case 54:
                         ch = 94;
                         break;
                      case 55:
                         ch = 38;
                         break;
                      case 56:
                         ch = 42;
                         break;
                      case 57:
                         ch = 40;
                         break;
                      case 59:
                         ch = 58;
                         break;
                      case 61:
                         ch = 43;
                         break;
                      case 91:
                         ch = 123;
                         break;
                      case 92:
                         ch = 124;
                         break;
                      case 93:
                         ch = 125;
                         break;
                      case 96:
                         ch = 126;
                         break;
                   }
                }
                else
                {
                   switch( ch ) {
                      case 34:
                         ch = 39;
                         break;
                      case 33:
                      case 64:
                      case 35:
                      case 36:
                      case 37:
                      case 94:
                      case 38:
                      case 42:
                      case 40:
                      case 41:
                         ch = wChar;
                         break;
                      case 43:
                         ch = 61;
                         break;
                      case 58:
                         ch = 59;
                         break;
                      case 60:
                         ch = 44;
                         break;
                      case 62:
                         ch = 46;
                         break;
                      case 63:
                         ch = 47;
                         break;
                      case 95:
                         ch = 45;
                         break;
                      case 123:
                         ch = 91;
                         break;
                      case 124:
                         ch = 92;
                         break;
                      case 125:
                         ch = 93;
                         break;
                      case 126:
                         ch = 96;
                         break;
                   }
                }
             }

             #ifdef HB_DEBUG_KEYBOARD
                /* if( dwState & ENHANCED_KEY ) ch = -32; */
                fprintf( stdout, "\n\nhb_gt_ReadKey(): dwState is %ld, wChar is %d, wKey is %d, ch is %d", dwState, wChar, wKey, ch );
                if( dwState & CAPSLOCK_ON ) fprintf( stdout, " CL" );
                if( dwState & ENHANCED_KEY ) fprintf( stdout, " EK" );
                if( dwState & LEFT_ALT_PRESSED ) fprintf( stdout, " LA" );
                if( dwState & RIGHT_ALT_PRESSED ) fprintf( stdout, " RA" );
                if( dwState & LEFT_CTRL_PRESSED ) fprintf( stdout, " LC" );
                if( dwState & RIGHT_CTRL_PRESSED ) fprintf( stdout, " RC" );
                if( dwState & NUMLOCK_ON ) fprintf( stdout, " NL" );
                if( dwState & SCROLLLOCK_ON ) fprintf( stdout, " SL" );
                if( dwState & SHIFT_PRESSED ) fprintf( stdout, " SH" );
                fprintf( stdout, " " );
             #endif

             if( ch == 224 )
             {
                /* Strip extended key lead-in codes */
                ch = 0;
                #ifdef HB_DEBUG_KEYBOARD
                   fprintf( stdout, "-" );
                #endif
             }
             else if( ch < 0  && ch != -32 && ch != -16 /* Hopefully all "dead" keys generate ch = 0 when used alone... && !IgnoreKeyCodes( wKey ) */ )
             {
                /* Process international key codes */
                ch += 256;
                #ifdef HB_DEBUG_KEYBOARD
                   fprintf( stdout, "+" );
                #endif
             }
             else if( ch < 0 && ch != -32 && ch != -16 )
             {
                /* Ignore any negative character codes that didn't get handled
                   by the international keyboard processing and don't signify
                   extended key codes */
                ch = 0;
             }
             else
             {
                #ifdef HB_DEBUG_KEYBOARD
                   fprintf( stdout, "0" );
                #endif
                if( wChar == 27 )
                {
                   /* Fix for escape key problem with some international
                      keyboards and/or international versions of Windows */
                      ch = 27;
                }
                if( ( ( ch == 0 || ch == -32 || ch == -16 ) && ( dwState & ( SHIFT_PRESSED | LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED ) ) )
                || ( ( dwState & ( ENHANCED_KEY | LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED ) ) ) )
                {
                   extended = 1;
                   #ifdef HB_DEBUG_KEYBOARD
                      fprintf( stdout, "1" );
                   #endif
                }
                else if( ch == 0 )
                {
                   if( eventmask & INKEY_RAW )
                   {
                      extended = 1;
                      #ifdef HB_DEBUG_KEYBOARD
                         fprintf( stdout, "2" );
                      #endif
                   }
                   else if( IgnoreKeyCodes( wKey ) )
                   {
                      #ifdef HB_DEBUG_KEYBOARD
                         fprintf( stdout, "!" );
                      #endif
                   }
                   else
                   {
                      ch = StdFnKeys( wKey, 0 );
                      #ifdef HB_DEBUG_KEYBOARD
                         fprintf( stdout, "3" );
                      #endif
                   }
                }
                else if( ch == 9 && wKey == 15 && ( dwState & SHIFT_PRESSED ) )
                {
                   #ifdef HB_DEBUG_KEYBOARD
                      fprintf( stdout, "@" );
                   #endif
                   ch = wKey + 256;   /* Shift+TAB */
                }
             }
             if( extended )
             {
                #ifdef HB_DEBUG_KEYBOARD
                   fprintf( stdout, "4" );
                #endif
                /* Process non-ASCII key codes */
                if( eventmask & INKEY_RAW ) wKey = wChar;
                /* Discard standalone state key presses for normal mode only */
                if( ( eventmask & INKEY_RAW ) == 0 && IgnoreKeyCodes( wKey ) )
                {
                      wKey = 0;
                      #ifdef HB_DEBUG_KEYBOARD
                         fprintf( stdout, "5" );
                      #endif
                }
                if( wKey == 0 ) ch = 0;
                else
                {
                   #ifdef HB_DEBUG_KEYBOARD
                      fprintf( stdout, "6" );
                   #endif
                   if( eventmask & INKEY_RAW )
                   {
                      /* Pass along all virtual key codes with all
                         enhanced and state indicators accounted for */
                      wKey += 256;
                      if( dwState & ENHANCED_KEY ) wKey += 512;
                      if( dwState & SHIFT_PRESSED ) wKey += 1024;
                      if( dwState & LEFT_CTRL_PRESSED ) wKey += 2048;
                      if( dwState & RIGHT_CTRL_PRESSED ) wKey += 4096;
                      if( dwState & LEFT_ALT_PRESSED ) wKey += 8192;
                      if( dwState & RIGHT_ALT_PRESSED ) wKey += 16384;
                      ch = wKey;
                      #ifdef HB_DEBUG_KEYBOARD
                         fprintf( stdout, "7" );
                      #endif
                   }
                   else
                   {
                      /* Translate virtual scan codes to Clipper codes */
                      BOOL bAlt = dwState & ( LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED );
                      BOOL bCtrl = dwState & ( LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED );
                      BOOL bShift = dwState & SHIFT_PRESSED;
                      BOOL bAltGr = ( dwState & s_dwAltGrBits ) == s_dwAltGrBits;
                      BOOL bEnhanced = dwState & ENHANCED_KEY;
                      #ifdef HB_DEBUG_KEYBOARD
                         fprintf( stdout, "8" );
                      #endif

                      HB_TRACE(HB_TR_INFO, ("hb_gt_ReadKey(): wKey is %d, dwState is %d, ch is %d", wKey, dwState, ch));
                      if( bAlt )
                      {
                         #ifdef HB_DEBUG_KEYBOARD
                            fprintf( stdout, "9" );
                         #endif
                         /* Alt key held */
                         if( bAltGr && ch ) { /* It's actually Alt+Gr */ }
                         else
                         {
                            #ifdef HB_DEBUG_KEYBOARD
                               fprintf( stdout, "a" );
                            #endif
                            ch = wKey + HB_INKEY_ALT;
                            if( bEnhanced ) ch += HB_INKEY_ENHANCED;
                         }
                      }
                      else if( bCtrl )
                      {
                         #ifdef HB_DEBUG_KEYBOARD
                            fprintf( stdout, "b" );
                         #endif
                         /* Ctrl key held */
                         if( ch == 0 || bEnhanced ) ch = wKey + HB_INKEY_CTRL;
                         if( bEnhanced ) ch += HB_INKEY_ENHANCED;
                      }
                      else if( bShift )
                      {
                         #ifdef HB_DEBUG_KEYBOARD
                            fprintf( stdout, "c" );
                         #endif
                         /* Shift key held */
                         if( ch == 0 || bEnhanced ) ch = wKey + HB_INKEY_SHIFT;
                         if( bEnhanced ) ch += HB_INKEY_ENHANCED;
                      }
                      else
                      {
                         #ifdef HB_DEBUG_KEYBOARD
                            fprintf( stdout, "d" );
                         #endif
                         ch = StdFnKeys( wKey, bEnhanced );
                      }
                   }
                }
             }

#if 0
             /* Debug code: */
             else
             {
                WORD wKey;
                if( eventmask & INKEY_RAW )
                   wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualKeyCode;
                else
                   wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualScanCode;
                HB_TRACE(HB_TR_INFO, ("hb_gt_ReadKey(): wKey is %d", wKey));
             }
#endif
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
         int iLen = strlen( lptstr );
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

HB_CALL_ON_STARTUP_BEGIN( HB_GT_FUNC(_gt_Init_) )
   hb_gtRegister( &gtInit );
HB_CALL_ON_STARTUP_END( HB_GT_FUNC(_gt_Init_) )
#if defined(HB_STATIC_STARTUP) || ( (! defined(__GNUC__)) && (! defined(_MSC_VER)) )
   #pragma startup HB_GT_FUNC(_gt_Init_)
#endif

#endif  /* HB_MULTI_GT */

/* *********************************************************************** */
