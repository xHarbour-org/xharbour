/*
 * $Id: gtgui.c,v 1.1 2006/06/26 11:27:06 druzus Exp $
 */

/*
 * Harbour Project source code:
 *    Mini GT for GUI programs.
 *    Now it supports only low level TONE and CLIPBOARD code for W32
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
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
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 * Tone and Clipboard code are from gtwin.c
 * 
 */


/* NOTE: User programs should never call this layer directly! */

#define HB_GT_NAME   GUI


#define HB_OS_WIN_32_USED
#include "hbapigt.h"
#include "hbapifs.h"
#include "hbinit.h"

#if defined( _MSC_VER ) || defined(__WATCOMC__)
  #include <conio.h>
#endif

static OSVERSIONINFO s_osv;

static int s_iStdIn, s_iStdOut, s_iStdErr;

static USHORT s_uiDispCount;
static USHORT s_usCursorStyle;

static SHORT s_sCurRow;
static SHORT s_sCurCol;

/* *********************************************************************** */

void HB_GT_FUNC(gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

    /* If Windows 95 or 98, use w9xTone for BCC32, MSVC */
    s_osv.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
    GetVersionEx( &s_osv );

    /* stdin && stdout && stderr */
    s_iStdIn  = iFilenoStdin;
    s_iStdOut = iFilenoStdout;
    s_iStdErr = iFilenoStderr;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Exit( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_GetScreenWidth( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth(%d)"));

    return 25;
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_GetScreenHeight( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

    return 80;
}

/* *********************************************************************** */

SHORT HB_GT_FUNC(gt_Col( void ))
{
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
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_DispCount())
{
    return s_uiDispCount;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Puts( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE *pbyStr, ULONG ulLen ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", usRow, usCol, (int) byAttr, pbyStr, ulLen));

    HB_SYMBOL_UNUSED( usRow );
    HB_SYMBOL_UNUSED( usCol );
    HB_SYMBOL_UNUSED( byAttr );
    HB_SYMBOL_UNUSED( pbyStr );
    HB_SYMBOL_UNUSED( ulLen );
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Replicate( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE byChar, ULONG ulLen ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", usRow, usCol, byAttr, byChar, ulLen));

    HB_SYMBOL_UNUSED( usRow );
    HB_SYMBOL_UNUSED( usCol );
    HB_SYMBOL_UNUSED( byAttr );
    HB_SYMBOL_UNUSED( byChar );
    HB_SYMBOL_UNUSED( ulLen );
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_RectSize( USHORT rows, USHORT cols ))
{
    return rows * cols * 2;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_GetText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * pbyDst ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRight, pbyDst));

    HB_SYMBOL_UNUSED( usTop );
    HB_SYMBOL_UNUSED( usLeft );
    HB_SYMBOL_UNUSED( usBottom );
    HB_SYMBOL_UNUSED( usRight );
    HB_SYMBOL_UNUSED( pbyDst );
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_PutText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * pbySrc ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRight, pbySrc));

    HB_SYMBOL_UNUSED( usTop );
    HB_SYMBOL_UNUSED( usLeft );
    HB_SYMBOL_UNUSED( usBottom );
    HB_SYMBOL_UNUSED( usRight );
    HB_SYMBOL_UNUSED( pbySrc );
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetAttribute( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d", usTop, usLeft, usBottom, usRight, (int) attr));

    HB_SYMBOL_UNUSED( usTop );
    HB_SYMBOL_UNUSED( usLeft );
    HB_SYMBOL_UNUSED( usBottom );
    HB_SYMBOL_UNUSED( usRight );
    HB_SYMBOL_UNUSED( attr );
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE byAttr, SHORT iRows, SHORT iCols ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRight, (int) byAttr, iRows, iCols));

    HB_SYMBOL_UNUSED( usTop );
    HB_SYMBOL_UNUSED( usLeft );
    HB_SYMBOL_UNUSED( usBottom );
    HB_SYMBOL_UNUSED( usRight );
    HB_SYMBOL_UNUSED( byAttr);
    HB_SYMBOL_UNUSED( iRows );
    HB_SYMBOL_UNUSED( iCols );
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_SetMode( USHORT usRows, USHORT usCols ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", usRows, usCols));

    HB_SYMBOL_UNUSED( usRows );
    HB_SYMBOL_UNUSED( usCols );

    return TRUE;
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_GetBlink())
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

    return TRUE;
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
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_Version()" ) );

   if ( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

    return "Harbour Terminal: Strep GT driver for W32 GUI programs";
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_Box( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
                          BYTE * szBox, BYTE byAttr ))
{
    HB_SYMBOL_UNUSED( Top );
    HB_SYMBOL_UNUSED( Left );
    HB_SYMBOL_UNUSED( Bottom );
    HB_SYMBOL_UNUSED( Right );
    HB_SYMBOL_UNUSED( szBox );
    HB_SYMBOL_UNUSED( byAttr );

    return SUCCESS;
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
    HB_SYMBOL_UNUSED( Row );
    HB_SYMBOL_UNUSED( Left );
    HB_SYMBOL_UNUSED( Right );
    HB_SYMBOL_UNUSED( byChar );
    HB_SYMBOL_UNUSED( byAttr );

    return SUCCESS;
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_VertLine( SHORT Col, SHORT Top, SHORT Bottom, BYTE byChar, BYTE byAttr ))
{
    HB_SYMBOL_UNUSED( Col );
    HB_SYMBOL_UNUSED( Top );
    HB_SYMBOL_UNUSED( Bottom );
    HB_SYMBOL_UNUSED( byChar );
    HB_SYMBOL_UNUSED( byAttr );

    return SUCCESS;
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
    return FALSE;
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_ReadKey( HB_inkey_enum eventmask ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

    HB_SYMBOL_UNUSED( eventmask );
    return 0;
}

/* *********************************************************************** */

#if defined( HB_ARCH_32BIT ) && \
    ( defined(__BORLANDC__) || defined(_MSC_VER) || \
      defined(__WATCOMC__) || defined(__MINGW32__) )
static int hb_Inp9x( USHORT usPort )
{
   USHORT usVal;

   HB_TRACE(HB_TR_DEBUG, ("hb_Inp9x(%hu)", usPort));

   #if defined( __BORLANDC__ ) || defined(__DMC__)

      _DX = usPort;
      __emit__(0xEC);         /* ASM  IN AL, DX */
      __emit__(0x32,0xE4);    /* ASM XOR AH, AH */
      usVal = _AX;

   #elif defined( __XCC__ )

      __asm {
               mov   dx, usPort
               xor   ax, ax
               in    al, dx
               mov   usVal, ax
            }

   #elif defined( __MINGW32__ )
      __asm__ __volatile__ ("inb %w1,%b0":"=a" (usVal):"Nd" (usPort));

   #elif defined( __WATCOMC__ )

      usVal = inp( usPort );

   #else

      usVal = _inp( usPort );

   #endif

   return usVal;
}

/* *********************************************************************** */

static int hb_Outp9x( USHORT usPort, USHORT usVal )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_Outp9x(%hu, %hu)", usPort, usVal));

   #if defined( __BORLANDC__ ) || defined(__DMC__)

      _DX = usPort;
      _AL = usVal;
      __emit__(0xEE);        /* ASM OUT DX, AL */

   #elif defined( __XCC__ )

      __asm {
               mov   dx, usPort
               mov   ax, usVal
               out   dx, al
            }

   #elif defined( __MINGW32__ )

      __asm__ __volatile__ ("outb %b0,%w1": :"a" (usVal), "Nd" (usPort));

   #elif defined( __WATCOMC__ )

       outp( usPort, usVal );

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

    /* If Windows 95 or 98, use w9xTone for BCC32, MSVC */
    if( s_osv.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS )
    {
       #if defined( HB_ARCH_32BIT ) && \
           ( defined( __BORLANDC__ ) || defined( _MSC_VER ) || \
             defined( __WATCOMC__ )  || defined(__MINGW32__) )
          HB_GT_FUNC(gt_w9xTone( dFrequency, dDuration ));
       #else
          HB_GT_FUNC(gt_wNtTone( dFrequency, dDuration ));
       #endif
    }

    /* If Windows NT or NT2k, use wNtTone, which provides TONE()
       reset sequence support (new) */
    else if( s_osv.dwPlatformId == VER_PLATFORM_WIN32_NT )
    {
      HB_GT_FUNC(gt_wNtTone( dFrequency, dDuration ));
    }
}

/* ************************** Clipboard support ********************************** */

void HB_GT_FUNC( gt_GetClipboard( char *szData, ULONG *pulMaxSize ) )
{
   HGLOBAL   hglb;
   LPTSTR    lptstr;

   if ( !IsClipboardFormatAvailable(CF_OEMTEXT) )
   {
     *pulMaxSize = 0;
     return;
   }

   if (!OpenClipboard( NULL ))
   {
     *pulMaxSize = 0;
     return;
   }

   hglb = GetClipboardData(CF_OEMTEXT);
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

/*  This poses problems when some other application copies a bitmap on the
    clipboard. The only way to set text to clipboard is made possible
    only if another application copies some text on the clipboard.

   if ( !IsClipboardFormatAvailable( CF_OEMTEXT ) )
   {
     return;
   }
*/

   if ( ! OpenClipboard( NULL ) )
   {
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
   SetClipboardData( CF_OEMTEXT, hglbCopy );

   CloseClipboard();
}

ULONG HB_GT_FUNC( gt_GetClipboardSize( void ) )
{
   HGLOBAL   hglb;
   LPTSTR    lptstr;
   int ret;

   if ( !IsClipboardFormatAvailable(CF_OEMTEXT) )
   {
     return 0;
   }

   if (!OpenClipboard( NULL ))
   {
     return 0;
   }

   hglb = GetClipboardData(CF_OEMTEXT);
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

void HB_GT_FUNC( gt_ProcessMessages( void ) )
{
   return;
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

/* ********** Graphics API ********** */

int HB_GT_FUNC( gt_gfxPrimitive( int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor ) )
{
  HB_SYMBOL_UNUSED( iType );
  HB_SYMBOL_UNUSED( iTop );
  HB_SYMBOL_UNUSED( iLeft );
  HB_SYMBOL_UNUSED( iBottom );
  HB_SYMBOL_UNUSED( iRight );
  HB_SYMBOL_UNUSED( iColor );

  return 0;
}

void HB_GT_FUNC( gt_gfxText( int iTop, int iLeft, char *cBuf, int iColor, int iSize, int iWidth ) )
{
  HB_SYMBOL_UNUSED( iTop );
  HB_SYMBOL_UNUSED( iLeft );
  HB_SYMBOL_UNUSED( cBuf );
  HB_SYMBOL_UNUSED( iColor );
  HB_SYMBOL_UNUSED( iSize );
  HB_SYMBOL_UNUSED( iWidth );
}

/* ******** Graphics API end ******** */

/* *********************************************************************** */

void HB_GT_FUNC(mouse_Init( void ))
{
}

void HB_GT_FUNC(mouse_Exit( void ))
{
}

BOOL HB_GT_FUNC(mouse_IsPresent( void ))
{
   return TRUE;
}

void HB_GT_FUNC(mouse_Show( void ))
{
}

void HB_GT_FUNC(mouse_Hide( void ))
{
}

int HB_GT_FUNC(mouse_Col( void ))
{
   return 0;
}

int HB_GT_FUNC(mouse_Row( void ))
{
   return 0;
}

void HB_GT_FUNC(mouse_SetPos( int iRow, int iCol ))
{
   HB_SYMBOL_UNUSED( iRow );
   HB_SYMBOL_UNUSED( iCol );
}

BOOL HB_GT_FUNC(mouse_IsButtonPressed( int iButton ))
{
   HB_SYMBOL_UNUSED( iButton );
   return FALSE;
}

int HB_GT_FUNC(mouse_CountButton( void ))
{
   return 0;
}

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
    gt_funcs->SetClipboard          = HB_GT_FUNC( gt_SetClipboard );
    gt_funcs->GetClipboard          = HB_GT_FUNC( gt_GetClipboard );
    gt_funcs->GetClipboardSize      = HB_GT_FUNC( gt_GetClipboardSize );
    gt_funcs->ProcessMessages       = HB_GT_FUNC( gt_ProcessMessages );

    /* Graphics API */
    gt_funcs->gfxPrimitive          = HB_GT_FUNC( gt_gfxPrimitive );
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
#elif defined(HB_MSC_STARTUP)
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
