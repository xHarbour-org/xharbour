/*
 * $Id: gtapi.c,v 1.20 2004/01/27 03:11:46 ronpinkas Exp $
 */

/*
 * Harbour Project source code:
 * The Terminal API
 *
 * Copyright 1999 Bil Simser <bsimser@home.com>
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
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Paul Tucker <ptucker@sympatico.ca>
 *    hb_gtInit()
 *    hb_gtExit()
 *    hb_gtDispBegin()
 *    hb_gtDispEnd()
 *    hb_gtPreExt()
 *    hb_gtPostExt()
 *    hb_gtGetColorStr()
 *    hb_gtSetColorStr()
 *    hb_gtSetMode()
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_gtDrawShadow()
 *
 * Copyright 2004 Giancarlo Niccolai <antispam at niccolai dot ws>
 *   SetGTCloseHandler() system
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <ctype.h>

#include "hbapigt.h"
#include "hbset.h"
#include "hb_io.h"
#include "hbvm.h"
#include "inkey.ch"

static BOOL   s_bInit = FALSE;

static SHORT  s_iRow;
static SHORT  s_iCol;
static SHORT  s_Width;  /* added for optimization */
static SHORT  s_Height; /* added for optimization */

static USHORT s_uiPreCount;
static USHORT s_uiPreCNest;
static USHORT s_uiCursorStyle;

static USHORT s_uiColorIndex;
static USHORT s_uiColorCount;
static int *  s_pColor;

/* Close event returned by inkey for asynchronous GT kills */
static int s_closeEvent = 0;
static int s_shutdownEvent = 0;
static int s_resizeEvent = 0;
static BOOL s_closing = FALSE;
static PHB_ITEM s_pOnClose = 0;

/* GT graphic susbsystem */
HB_GT_GOBJECT *hb_gt_gobjects;
HB_GT_GOBJECT *hb_gt_gobjects_end;
HB_GT_COLDEF hb_gt_gcoldefs[ HB_GT_COLDEF_COUNT ] =
{
   { "N",  { 0xFFFF, 0x0000, 0x0000, 0x0000 } },
   { "B",  { 0xFFFF, 0x0000, 0x0000, 0xAAAA } },
   { "G",  { 0xFFFF, 0x0000, 0xAAAA, 0x0000 } },
   { "BG", { 0xFFFF, 0x0000, 0xAAAA, 0xAAAA } },
   { "R",  { 0xFFFF, 0xAAAA, 0x0000, 0x0000 } },
   { "RB", { 0xFFFF, 0xAAAA, 0x0000, 0xAAAA } },
   { "GR", { 0xFFFF, 0xAAAA, 0x5555, 0x0000 } },
   { "W",  { 0xFFFF, 0xAAAA, 0xAAAA, 0xAAAA } },
   { "N+", { 0xFFFF, 0x5555, 0x5555, 0x5555 } },
   { "B+", { 0xFFFF, 0x5555, 0x5555, 0xFFFF } },
   { "G+", { 0xFFFF, 0x5555, 0xFFFF, 0x5555 } },
   { "BG+",{ 0xFFFF, 0x5555, 0xFFFF, 0xFFFF } },
   { "R+", { 0xFFFF, 0xFFFF, 0x5555, 0x5555 } },
   { "RB+",{ 0xFFFF, 0xFFFF, 0x5555, 0xFFFF } },
   { "GR+",{ 0xFFFF, 0xFFFF, 0xFFFF, 0x5555 } },
   { "W+", { 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF } }
};

HB_GT_GCOLOR hb_gtGForeground;
HB_GT_GCOLOR hb_gtGBackground;

// to spare time, I call this HB_FUN directly.
extern void HB_EXPORT HB_FUN_HB_EXECFROMARRAY();

/* masks: 0x0007     Foreground
          0x0070     Background
          0x0008     Bright
          0x0080     Blink
          0x0800     Underline foreground
          0x8000     Underline background
 */

/*
 To disable mouse at run time by passing NOMOUSE upon running
 executable
*/
#if ( defined(HB_OS_WIN_32_USED) || defined(__WIN32__) )
   extern BOOL b_MouseEnable;
#endif

/* gt API functions */

void hb_gtInit( int s_iFilenoStdin, int s_iFilenoStdout, int s_iFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtInit()"));

   hb_gt_gobjects = NULL;

   s_pColor = ( int * ) hb_xgrab( ( HB_CLR_MAX_ + 1 ) * sizeof( int ) );
   s_uiColorCount = HB_CLR_MAX_ + 1;

#if ( defined(HB_OS_WIN_32_USED) || defined(__WIN32__) )
   if( hb_cmdargCheck( "NOMOUSE" ) )
      /* Mouse is disabled here */
      b_MouseEnable = FALSE;
#endif

   hb_gt_Init( s_iFilenoStdin, s_iFilenoStdout, s_iFilenoStderr );

   hb_gtSetColorStr( hb_set.HB_SET_COLOR );

   s_iRow = hb_gt_Row();
   s_iCol = hb_gt_Col();
   s_uiPreCount = 0;
   s_uiPreCNest = 0;
   s_Height = hb_gt_GetScreenHeight();
   s_Width = hb_gt_GetScreenWidth();

   /* This should be called after s_iRow/s_iCol initialization. */
   hb_gtSetCursor( SC_NORMAL );

   s_bInit = TRUE;

   if( hb_cmdargCheck( "INFO" ) )
   {
      hb_conOutErr( hb_gt_Version(), 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
   }

   /* we assume that the screen stays a fixed size from now on
      this avoids many function calls                          */
}

void hb_gtExit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtExit()"));

   if( s_bInit )
   {
      s_bInit = FALSE;

      while( hb_gt_DispCount() )
         hb_gt_DispEnd();

      hb_gt_Exit();

      hb_xfree( s_pColor );
   }

   hb_gtClearGobjects();
}

int HB_EXPORT hb_gtExtendedKeySupport()
{
    return hb_gt_ExtendedKeySupport();
}

/* Why this redundancy ??? */
#if 0
int HB_EXPORT hb_gtReadKey( HB_inkey_enum eventmask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtReadKey(%d)", (int) eventmask));

   return hb_gt_ReadKey( eventmask );
}
#endif

void HB_EXPORT hb_gtAdjustPos( int iHandle, char * pStr, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtAdjustPos()"));

   if( isatty( iHandle ) && hb_gt_AdjustPos( ( BYTE * ) pStr, ulLen ) )
   {
      /* Adjust the console cursor position to match the device driver */
      s_iRow = hb_gt_Row();
      s_iCol = hb_gt_Col();
   }
}

USHORT HB_EXPORT hb_gtBox( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame )
{
   USHORT Ret;
   USHORT tmp;
   BYTE cPadChar;
   BYTE szBox[ 10 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_gtBox(%hd, %hd, %hd, %hd, %p)", Top, Left, Bottom, Right, pbyFrame));

   /* NOTE: For full compatibility, pad box string with last char if too
            short [vszakats] */

   cPadChar = ' ';
   for( tmp = 0; *pbyFrame && tmp < 9; tmp++ )
      cPadChar = szBox[ tmp ] = *pbyFrame++;
   while( tmp < 8 )
      szBox[ tmp++ ] = cPadChar;
   szBox[ tmp ] = '\0';

   if( Top != Bottom )
   {
      if( Left != Right )
         Ret = hb_gt_Box( Top, Left, Bottom, Right, szBox, ( BYTE ) s_pColor[ s_uiColorIndex ] );
      else
         Ret = hb_gt_VertLine( Left, Top, Bottom, szBox[ 3 ], ( BYTE ) s_pColor[ s_uiColorIndex ] );
   }
   else
      Ret = hb_gt_HorizLine( Top, Left, Right, szBox[ 1 ], ( BYTE ) s_pColor[ s_uiColorIndex ] );

   hb_gtSetPosContext( HB_MAX(Top,0) + 1, HB_MAX(Left,0) + 1, HB_GT_SET_POS_AFTER );

   return Ret;
}

USHORT HB_EXPORT hb_gtBoxD( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right )
{
   USHORT Ret;

   if( Top != Bottom )
   {
      if( Left != Right )
         Ret = hb_gt_BoxD( Top, Left, Bottom, Right, ( BYTE * ) _B_DOUBLE, ( BYTE ) s_pColor[ s_uiColorIndex ] );
      else
         Ret = hb_gt_VertLine( Left, Top, Bottom, (unsigned char) HB_B_DOUBLE_V, ( BYTE ) s_pColor[ s_uiColorIndex ] );
   }
   else
      Ret = hb_gt_HorizLine( Top, Left, Right, (unsigned char) HB_B_DOUBLE_H, ( BYTE ) s_pColor[ s_uiColorIndex ] );

   hb_gtSetPosContext( HB_MAX(Top,0) + 1, HB_MAX(Left,0) + 1, HB_GT_SET_POS_AFTER );

   return Ret;
}

USHORT HB_EXPORT hb_gtBoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right )
{
   USHORT Ret;

   if( Top != Bottom )
   {
      if( Left != Right )
         Ret = hb_gt_BoxS( Top, Left, Bottom, Right, ( BYTE * ) _B_SINGLE, ( BYTE ) s_pColor[ s_uiColorIndex ] );
      else
         Ret = hb_gt_VertLine( Left, Top, Bottom, (unsigned char) HB_B_SINGLE_V, ( BYTE ) s_pColor[ s_uiColorIndex ] );
   }
   else
      Ret = hb_gt_HorizLine( Top, Left, Right, (unsigned char) HB_B_SINGLE_H, ( BYTE ) s_pColor[ s_uiColorIndex ] );

   hb_gtSetPosContext( HB_MAX(Top,0) + 1, HB_MAX(Left,0) + 1, HB_GT_SET_POS_AFTER );

   return Ret;
}

USHORT HB_EXPORT hb_gtColorSelect( USHORT uiColorIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtColorSelect(%hu)", uiColorIndex));

   if( uiColorIndex <= s_uiColorCount )
   {
      s_uiColorIndex = uiColorIndex;
      return 0;
   }
   else
      return 1;
}

USHORT HB_EXPORT hb_gtDispBegin( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtDispBegin()"));

   if( s_uiPreCount == 0 )
      hb_gt_DispBegin();
   else
      ++s_uiPreCount;  /* defined in each terminal driver */

   return 0;
}

USHORT HB_EXPORT hb_gtDispCount( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtDispCount()"));

   return hb_gt_DispCount();
}

USHORT HB_EXPORT hb_gtDispEnd( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtDispEnd()"));

   if( s_uiPreCount == 0 )
      hb_gt_DispEnd();
   else
      --s_uiPreCount;

   return 0;
}

USHORT HB_EXPORT hb_gtPreExt( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtPreExt()"));

   /* an external (printf...) write is about to take place */

   if( s_uiPreCNest == 0 )
   {
      if( s_uiPreCount == 0 )
      {
         USHORT uidc;

         uidc = s_uiPreCount = hb_gt_DispCount();

         while( uidc-- )
            hb_gt_DispEnd();

         /* call platform depend layer to flush all pending outputs and
         * to prepare screen for direct updating
         */
         hb_gt_PreExt();
      }

      s_uiPreCNest = 1;
   }
   else
      ++s_uiPreCNest;

   return 0;
}

USHORT HB_EXPORT hb_gtPostExt( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtPostExt()"));

   if( s_uiPreCNest == 1 )
   {
      /* call platform depend layer to restore all settings */
      hb_gt_PostExt();

      while( s_uiPreCount-- )
         hb_gt_DispBegin();

      s_uiPreCount = 0;
      s_uiPreCNest = 0;
   }
   else
      --s_uiPreCNest;

   return 0;
}

/* NOTE: szColorString must be at least CLR_STRLEN wide by the NG. It seems
         that CA-Cl*pper SETCOLOR() will return string lengths up to 131+EOF.
         That seems like a 127+1 buffer size, plus lazy overflow checking.
         [vszakats] */

USHORT HB_EXPORT hb_gtGetColorStr( char * pszColorString )
{
   USHORT uiColorIndex;
   int iPos = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetColorStr(%s)", pszColorString));

   /* Go on if there's space left for the largest color string plus EOF */
   for( uiColorIndex = 0; uiColorIndex < s_uiColorCount && iPos < ( CLR_STRLEN - 8 ); uiColorIndex++ )
   {
      int nColor = s_pColor[ uiColorIndex ] & 7;
      int j;

      if( uiColorIndex > 0 )
         pszColorString[ iPos++ ] = ',';

      for( j = 0; j <= 1; j++ )
      {
         if( ( s_pColor[ uiColorIndex ] & ( j ? 0x8000 : 0x0800 ) ) == 0 )
         {
            if( nColor == 7 )
                pszColorString[ iPos++ ] = 'W';
            else
            {
               if( nColor == 0 )
                  pszColorString[ iPos++ ] = 'N';
               else
               {
                  if( ( nColor & 1 ) != 0 )
                     pszColorString[ iPos++ ] = 'B';

                  if( ( nColor & 2 ) != 0 )
                     pszColorString[ iPos++ ] = 'G';

                  if( ( nColor & 4 ) != 0 )
                     pszColorString[ iPos++ ] = 'R';
               }
            }
         }
         else
            pszColorString[ iPos++ ] = 'U';

         if( j == 0 )
         {
            /* NOTE: When STRICT is on, Harbour will put both the "*" and "+"
                     chars to the first half of the colorspec (like "W*+/B"),
                     which is quite ugly, otherwise it will put the "+" to the
                     first half and the "*" to the second (like "W+/B*"), which
                     is how it should be done. [vszakats] */

#ifdef HB_C52_STRICT
            if( ( s_pColor[ uiColorIndex ] & 0x80 ) != 0 )
               pszColorString[ iPos++ ] = '*';
#endif

            if( ( s_pColor[ uiColorIndex ] & 0x08 ) != 0 )
               pszColorString[ iPos++ ] = '+';

            pszColorString[ iPos++ ] = '/';
         }
#ifndef HB_C52_STRICT
         else
         {
            if( ( s_pColor[ uiColorIndex ] & 0x80 ) != 0 )
               pszColorString[ iPos++ ] = '*';
         }
#endif

         nColor = ( s_pColor[ uiColorIndex ] >> 4 ) & 7;
      }
   }

   pszColorString[ iPos ] = '\0';

   return 0;
}

USHORT HB_EXPORT hb_gtColorToN( char * szColorString )
{
   char c;
   USHORT nColor = 0;
   BOOL bFore = TRUE;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtColorToN(%s)", szColorString));

   while( ( c = *szColorString++ ) != 0 )
   {
      c = toupper( c );

      switch( c )
      {
         case '*':
            nColor |= 0x80;
            break;

         case '+':
            nColor |= 0x08;
            break;

         case '/':
            bFore = FALSE;
            break;

         case 'B':
            if( * szColorString == 'G' || * szColorString == 'g' )
            {
               nColor |= bFore ? 0x03: 0x30;
               szColorString++;
            }
            else
               nColor |= bFore ? 0x01: 0x10;
            break;

         case 'G':
            if( * szColorString == 'R' || * szColorString == 'r' )
            {
               nColor |= bFore ? 0x06: 0x60;
               szColorString++;
            }
            else
               nColor |= bFore ? 0x02: 0x20;
            break;

            case 'W':
               nColor |= bFore ? 0x07: 0x70;
               break;

            case 'R':
               if( * szColorString == 'B' || * szColorString == 'b' )
               {
                  nColor |= bFore ? 0x05: 0x50;
                  szColorString++;
               }
               else
                  nColor |= bFore ? 0x04: 0x40;
               break;
      }
   }

   return nColor;
}

USHORT HB_EXPORT hb_gtSetColorStr( char * szColorString )
{
   char c;
   char buff[ 6 ];
   BOOL bHasI = FALSE;
   BOOL bHasU = FALSE;
   BOOL bHasX = FALSE;
   BOOL bSlash = FALSE;
   int nPos = 0;
   int nFore = 0;
   int nColor = 0;
   int nCount = -1, i = 0, y;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetColorStr(%s)", szColorString));

   if( szColorString == ( char * ) NULL )
      return 1;

   if( *szColorString == '\0' )
   {
      s_pColor[ 0 ] = 0x07;
      s_pColor[ 1 ] = 0x70;
      s_pColor[ 2 ] = 0;
      s_pColor[ 3 ] = 0;
      s_pColor[ 4 ] = 0x07;
   }

   do
   {
      c = *szColorString++;
      c = toupper( c );

      while( c <= '9' && c >= '0' && i < 6 )
      {
         if( i == 0 )
            memset( buff, '\0', 6 );

         buff[ i++ ] = c;
         c = *szColorString++;
      }

      if( i > 0 )
      {
         --i;
         nColor = 0;
         /* TODO: this can probably be replaced with atoi() */
         /* ie: nColor = atoi( buff ); */
         for( y = 1; i + 1; y *= 10, i-- )
         {
            if( buff[ i ] != '\0')
               nColor += ( ( buff[ i ] - '0' ) * y );
         }
         nColor &= 0x0F;
         i = 0;
         ++nCount;
      }

      ++nCount;

      switch( c )
      {
         case 'B':
            nColor |= 1;
            break;
         case 'G':
            nColor |= 2;
            break;
         case 'I':
            bHasI   = TRUE;
            break;
         case 'N':
            nColor  = 0;
            break;
         case 'R':
            nColor |= 4;
            break;
         case 'U':
            bHasU   = TRUE;
            break;
         case 'W':
            nColor  = 7;
            break;
         case 'X':                   /* always sets forground to 'N' */
            bHasX   = TRUE;
            break;
         case '*':
            nFore  |= 128;
            break;
         case '+':
            nFore  |= 8;
            break;
         case '/':
            if( bHasU )
            {
               bHasU = FALSE;
               nFore |= 0x0800;  /* foreground underline bit */
            }
            else if( bHasX )
            {
               nColor = 0;
               bHasX = FALSE;
            }
            else if( bHasI )
            {
               nColor = 7;
               bHasI = FALSE;
            }

            nFore |= nColor;
            nColor = 0;
            bSlash = TRUE;
            break;
         case ',':
         case '\0':
            if( ! nCount )
               nFore = s_pColor[ nPos ];
            nCount = -1;
            if( nPos == s_uiColorCount )
            {
               s_pColor = ( int * ) hb_xrealloc( s_pColor, sizeof( int ) * ( nPos + 1 ) );
               ++s_uiColorCount;
            }
            if( bHasX )
               nFore &= 0x88F8;

            if( bHasU ) /* background if slash, else foreground */
               nColor |= 0x0800;

            if( bHasI )
            {
               if( bSlash )
               {
                  nColor &= 0x088F;
                  nColor |= 0x0007;
                  nFore &= 0x88F8;
               }
               else
               {
                  nColor &= 0x08F8;
                  nColor |= 0x0070;
                  nFore &= 0x888F;
               }
            }
            if( ( nFore & 0x8800 ) != 0 && ( ( nFore | nColor ) & 0x0077 ) == 0)
               nFore |= 1;

            if( bSlash )
               s_pColor[ nPos++ ] = ( nColor << 4 ) | nFore;
            else
               s_pColor[ nPos++ ] = nColor | nFore;

            nColor = nFore = 0;
            bSlash = bHasX = bHasU = bHasI = FALSE;
      }
   }
   while( c );

   if( nPos > 0 && nPos < 4 )
      s_pColor[ 4 ] = s_pColor[ 1 ];

   s_uiColorIndex = HB_CLR_STANDARD; /* hb_gtColorSelect( HB_CLR_STANDARD ); */

   return 0;
}

USHORT HB_EXPORT hb_gtGetCursor( USHORT * uipCursorStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetCursor(%p)", uipCursorStyle));

   *uipCursorStyle = hb_gt_GetCursorStyle();

   return 0;
}

USHORT HB_EXPORT hb_gtSetCursor( USHORT uiCursorStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetCursor(%hu)", uiCursorStyle));

   if( uiCursorStyle <= SC_SPECIAL2 )
   {
      /* Set the cursor only when, it's in bounds. */
      if( s_iRow >= 0 && s_iRow < s_Height &&
          s_iCol >= 0 && s_iCol < s_Width )
         hb_gt_SetCursorStyle( uiCursorStyle );

      s_uiCursorStyle = uiCursorStyle;

      return 0;
   }
   else
      return 1;
}

USHORT HB_EXPORT hb_gtGetPos( SHORT * piRow, SHORT * piCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetPos(%p, %p)", piRow, piCol));

   if( s_Width <=0 || (s_iRow >= 0 && s_iRow < s_Height &&
       s_iCol >= 0 && s_iCol < s_Width))
   {
      /* Only return the actual cursor position if the current
         cursor position was not previously set out of bounds. */
      s_iRow = hb_gt_Row();
      s_iCol = hb_gt_Col();
   }

   *piRow = s_iRow;
   *piCol = s_iCol;

   return 0;
}

/* NOTE: Should be exactly the same as hb_gtSetPosContext(), but without the
         additional third parameter. */

USHORT HB_EXPORT hb_gtSetPos( SHORT iRow, SHORT iCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetPos(%hd, %hd)", iRow, iCol));

   /* Validate the new cursor position */
   if( s_Width <= 0|| (iRow >= 0 && iRow < s_Height &&
       iCol >= 0 && iCol < s_Width) )
   {
      hb_gt_SetPos( iRow, iCol, HB_GT_SET_POS_BEFORE );

      /* If cursor was out bounds, now enable it */
      if( s_Width > 0 && ( s_iRow < 0 || s_iRow >= s_Height ||
          s_iCol < 0 || s_iCol >= s_Width) )
         hb_gt_SetCursorStyle( s_uiCursorStyle );
   }
   else
      hb_gt_SetCursorStyle( SC_NONE ); /* Disable cursor if out of bounds */

   s_iRow = iRow;
   s_iCol = iCol;

   return 0;
}

/* NOTE: Should be exactly the same as hb_gtSetPos(), but with the additional
         parameter. */

USHORT HB_EXPORT hb_gtSetPosContext( SHORT iRow, SHORT iCol, SHORT iMethod )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetPosContext(%hd, %hd, %hd)", iRow, iCol, iMethod));

   /* Validate the new cursor position */
   if( s_Width <= 0|| (iRow >= 0 && iRow < s_Height &&
       iCol >= 0 && iCol < s_Width) )
   {
      hb_gt_SetPos( iRow, iCol, iMethod );

      /* If cursor was out bounds, now enable it */
      if( s_Width <= 0|| (s_iRow < 0 || s_iRow >= s_Height ||
          s_iCol < 0 || s_iCol >= s_Width) )
         hb_gt_SetCursorStyle( s_uiCursorStyle );
   }
   else
      hb_gt_SetCursorStyle( SC_NONE ); /* Disable cursor if out of bounds */

   s_iRow = iRow;
   s_iCol = iCol;

   return 0;
}

BOOL HB_EXPORT hb_gtIsColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtIsColor()"));

   return hb_gt_IsColor();
}

USHORT HB_EXPORT hb_gtMaxCol( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtMaxCol()"));

   return s_Width > 0 ? s_Width - 1: 0;
}

USHORT HB_EXPORT hb_gtMaxRow( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtMaxRow()"));

   return s_Height - 1;
}

USHORT HB_EXPORT hb_gtRectSize( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, UINT * uipBuffSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtRectSize(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, uipBuffSize));

   *uipBuffSize = hb_gt_RectSize( uiBottom - uiTop + 1, uiRight - uiLeft + 1 );

   return 0;
}

USHORT HB_EXPORT hb_gtRepChar( USHORT uiRow, USHORT uiCol, BYTE byChar, USHORT uiCount )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtRepChar(%hu, %hu, %d, %hu)", uiRow, uiCol, (int) byChar, uiCount));

   hb_gt_Replicate( uiRow, uiCol, ( BYTE ) s_pColor[ s_uiColorIndex ],
                    byChar, uiCount );

   return 0;
}

USHORT HB_EXPORT hb_gtRest( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, void * pScrBuff )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtRest(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pScrBuff));

   hb_gt_PutText( uiTop, uiLeft, uiBottom, uiRight, ( BYTE * ) pScrBuff );

   return 0;
}

USHORT HB_EXPORT hb_gtSave( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, void * pScrBuff )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSave(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pScrBuff));

   hb_gt_GetText( uiTop, uiLeft, uiBottom, uiRight, ( BYTE * ) pScrBuff );

   return 0;
}

USHORT HB_EXPORT hb_gtScrDim( USHORT * uipHeight, USHORT * uipWidth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtScrDim(%p, %p)", uipHeight, uipWidth));

   *uipHeight = s_Height - 1;
   *uipWidth = s_Width > 0 ? s_Width - 1: -1;

   return 0;
}

USHORT HB_EXPORT hb_gtGetBlink( BOOL * bpBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetBlink(%p)", bpBlink));

   *bpBlink = hb_gt_GetBlink();

   return 0;
}

USHORT HB_EXPORT hb_gtSetBlink( BOOL bBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetBlink(%d)", (int) bBlink));

   hb_gt_SetBlink( bBlink );

   return 0;
}

USHORT HB_EXPORT hb_gtSetMode( USHORT uiRows, USHORT uiCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetMode(%hu, %hu)", uiRows, uiCols));

   if (hb_gt_SetMode( uiRows, uiCols ))
   {
      s_Height = uiRows;
      s_Width = uiCols;
      return 0;
   }
   else return 1;
}

/* NOTE: This is a compatibility function.
         If you're running on a CGA and snow is a problem speak up! */

USHORT HB_EXPORT hb_gtSetSnowFlag( BOOL bNoSnow )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetSnowFlag(%d)", (int) bNoSnow));

   HB_SYMBOL_UNUSED( bNoSnow );

   return 0;
}

USHORT HB_EXPORT hb_gtWrite( BYTE * pStr, ULONG ulLength )
{

   HB_TRACE(HB_TR_DEBUG, ("hb_gtWrite(%p, %lu)", pStr, ulLength));

   /* Display the text if the cursor is on screen */
   if( s_Width <= 0 || ( s_iCol >= 0 && s_iCol < s_Width &&
       s_iRow >= 0 && s_iRow < s_Height ) )
   {
      /* Truncate the text if the cursor will end up off the right edge */
      hb_gt_Puts( s_iRow, s_iCol, ( BYTE ) s_pColor[ s_uiColorIndex ], pStr,
         s_Width > 0 ?
            HB_MIN( ulLength, ( ULONG ) ( s_Width - s_iCol ) ):
            ulLength );
   }

   /* Finally, save the new cursor position, even if off-screen */
   hb_gtSetPosContext( s_iRow, s_iCol + ( SHORT ) ulLength, HB_GT_SET_POS_AFTER );

   return 0;
}

USHORT HB_EXPORT hb_gtWriteAt( USHORT uiRow, USHORT uiCol, BYTE * pStr, ULONG ulLength )
{

   HB_TRACE(HB_TR_DEBUG, ("hb_gtWriteAt(%hu, %hu, %p, %lu)", uiRow, uiCol, pStr, ulLength));

   /* Display the text if the cursor is on screen */
   if( s_Width <= 0 ||  ( uiCol < s_Width && uiRow < s_Height ) )
   {
      /* Truncate the text if the cursor will end up off the right edge */
      hb_gt_Puts( uiRow, uiCol, ( BYTE ) s_pColor[ s_uiColorIndex ], pStr,
         s_Width > 0 ?
            HB_MIN( ulLength, ( ULONG ) ( s_Width - uiCol ) ):
            ulLength);
   }

   /* Finally, save the new cursor position, even if off-screen */
   hb_gtSetPosContext( uiRow, uiCol + ( SHORT ) ulLength, HB_GT_SET_POS_AFTER );

   return 0;
}

#define WRITECON_BUFFER_SIZE 512

USHORT HB_EXPORT hb_gtWriteCon( BYTE * pStr, ULONG ulLength )
{
   int iLen = 0;
   BOOL bDisp = FALSE;
   BOOL bNewLine = FALSE;
   SHORT iRow;
   SHORT iCol;
   SHORT iMaxRow;
   SHORT iMaxCol;
   BYTE szString[ WRITECON_BUFFER_SIZE ];

   HB_TRACE(HB_TR_DEBUG, ("hb_gtWriteCon(%p, %lu)", pStr, ulLength));

   iMaxRow = s_Height - 1;
   iMaxCol = s_Width - 1;

   /* Limit the starting cursor position to maxrow(),maxcol()
      on the high end, but don't limit it on the low end. */

   iRow = ( s_iRow <= iMaxRow ) ? s_iRow : iMaxRow;
   iCol = ( iMaxCol > 0 || s_iCol <= iMaxCol ) ? s_iCol : iMaxCol;

   if( iRow != s_iRow || iCol != s_iCol )
      hb_gtSetPos( iRow, iCol );

   while( ulLength-- )
   {
      BYTE ch = *pStr++;

      switch( ch )
      {
         case HB_CHAR_BEL:
            break;

         case HB_CHAR_BS:
            if( iCol > 0 )
            {
               --iCol;
               bDisp = TRUE;
            }
            else if( iMaxCol > 0 && iCol == 0 && iRow > 0 )
            {
               iCol = iMaxCol;
               --iRow;
               bDisp = TRUE;
            }
            break;

         case HB_CHAR_LF:
            iCol = 0;
            if( iRow >= 0 ) ++iRow;
            bDisp = TRUE;
            bNewLine = TRUE;
            break;

         case HB_CHAR_CR:
            iCol = 0;
            if( *pStr == HB_CHAR_LF )
            {
               if( iRow >= 0 ) ++iRow;
               bNewLine = TRUE;
               ++pStr;
               --ulLength;
            }
            bDisp = TRUE;
            break;

         default:
            ++iCol;
            if( iMaxCol > 0 && (iCol > iMaxCol || iCol <= 0) )
            {
               /* If the cursor position started off the left edge,
                  don't display the first character of the string */
               if( iCol > 0 ) szString[ iLen++ ] = ch;
               /* Always advance to the first column of the next row
                  when the right edge is reached or when the cursor
                  started off the left edge, unless the cursor is off
                  the top edge, in which case only change the column */
               iCol = 0;
               if( iRow >= 0 ) ++iRow;
               bDisp = TRUE;
               bNewLine = TRUE;
            }
            else
               szString[ iLen++ ] = ch;

            /* Special handling for a really wide screen or device */
            if( iLen >= WRITECON_BUFFER_SIZE ) bDisp = TRUE;
      }

      if( bDisp || ulLength == 0 )
      {
         if( iLen && s_iRow >= 0 )
            hb_gtWrite( szString, iLen );

         iLen = 0;
         if( iRow > iMaxRow )
         {
            /* Normal scroll */
            hb_gtScroll( 0, 0, iMaxRow, iMaxCol, iRow - iMaxRow, 0 );
            iRow = iMaxRow;
            iCol = 0;
         }
         else if( iRow < 0 && bNewLine )
         {
            /* Special case scroll when newline
               and cursor off top edge of display */
            hb_gtScroll( 0, 0, iMaxRow, iMaxCol, 1, 0 );
         }
         hb_gtSetPosContext( iRow, iCol, HB_GT_SET_POS_AFTER );
         bDisp = FALSE;
         bNewLine = FALSE;
      }
   }

   return 0;
}

USHORT HB_EXPORT hb_gtScroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, SHORT iRows, SHORT iCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtScroll(%hu, %hu, %hu, %hu, %hd, %hd)", uiTop, uiLeft, uiBottom, uiRight, iRows, iCols));

   /* Complete scroll? */
   if ( uiTop == 0 && uiLeft == 0 && uiBottom >= s_Height -1 && uiRight >= s_Width -1 &&
         iRows == 0 )
   {
      /* Destroy objects */
      hb_gtClearGobjects();
   }

   hb_gt_Scroll( uiTop, uiLeft, uiBottom, uiRight, ( BYTE ) s_pColor[ s_uiColorIndex ], iRows, iCols );

   return 0;
}

/* NOTE: It would be better if the clipping was done by the low level API */

USHORT HB_EXPORT hb_gtDrawShadow( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr )
{
   USHORT uiMaxRow;
   USHORT uiMaxCol;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtDrawShadow(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

   uiMaxRow = s_Height - 1;
   uiMaxCol = s_Width - 1;

   uiLeft += 2;
   ++uiBottom;

   /* Draw the bottom edge */

   if( uiBottom <= uiMaxRow && uiLeft <= uiMaxCol )
      hb_gt_SetAttribute( uiBottom, uiLeft, uiBottom, HB_MIN( uiRight, uiMaxCol ), byAttr );

   ++uiRight;
   ++uiTop;

   /* Draw the right edge */

   if( uiTop <= uiMaxRow && uiRight <= uiMaxCol )
      hb_gt_SetAttribute( uiTop, uiRight, uiBottom, HB_MIN( uiRight + 1, uiMaxCol ), byAttr );

   return 0;
}

void HB_EXPORT hb_gtTone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtTone(%lf, %lf)", dFrequency, dDuration));

   hb_gt_Tone( dFrequency, dDuration );
}

char HB_EXPORT * hb_gtVersion( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtVersion()"));

   return hb_gt_Version();
}
/* prepare the terminal for system call */
USHORT HB_EXPORT hb_gtSuspend( void )
{
   USHORT uidc;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSuspend()"));

   /* based on hb_gtPreExt() */

   uidc = s_uiPreCount = hb_gt_DispCount();

   while( uidc-- )
      hb_gt_DispEnd();

   /* call platform depend layer to flush all pending outputs and
    * to prepare screen for outside output
    */
   if( !hb_gt_Suspend() )
      return (USHORT) -1;

   return 0;
}

USHORT HB_EXPORT hb_gtResume( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtResume()"));

   /* call platform depend layer to restore all settings */
   if( hb_gt_Resume() )
   {
      while( s_uiPreCount-- )
         hb_gt_DispBegin();
   }
   else
      return (USHORT) -1;

   return  0;
}

int HB_EXPORT hb_gtIndexedColor( int idx )
{
    return s_pColor[ idx ];
}

int HB_EXPORT hb_gtCurrentColor()
{
    return s_pColor[ s_uiColorIndex ];
}

/** Handles a request to close the program received by GT.
   In moder systems, user may issue it's request to colose
   the application in various way, i.e. sending a TERM
   signal in POSIX systems, or asking the window manager
   to close the window containing the application.

   When this happens, the GT driver should call this function,
   that will properly quit the application, or if a handler
   is isntalled, ask the user about his intentions to close
   the app.

   Control will anyhow return to the GT: if the user request
   to quit is accepted, a gentle vm_quit request will be
   issued to the virtual machine, so the GT driver must return to
   VM as soon as possible.
*/

void HB_EXPORT hb_gtHandleClose()
{
   // this can work only in single threading, but we MUST have just
   // one thread that can send a close request even in MT.
   if ( s_closing )
   {
      return;
   }

   s_closing = TRUE;

   if (s_pOnClose == 0 )
   {
      if ( s_closeEvent == 0 )
      {
         hb_inkeyPut( HB_BREAK_FLAG ); // Pretend Alt+C pressed
         //hb_inkeyPut( K_ALT_C );
      }
      else
      {
         hb_inkeyPut( s_closeEvent );
      }
   }
   else {
      hb_execFromArray( s_pOnClose );
   }

   s_closing = FALSE;
}

int HB_EXPORT hb_gtHandleShutdown()
{
   // this can work only in single threading, but we MUST have just
   // one thread that can send a close request even in MT.
   if ( s_closing )
   {
      return FALSE;
   }

   if ( ! s_shutdownEvent )
   {
      return FALSE;
   }

   hb_inkeyPut( s_shutdownEvent );
   return TRUE;
}

BOOL HB_EXPORT hb_gtSetCloseHandler( PHB_ITEM handler )
{
   if ( s_pOnClose != 0 )
   {
      hb_itemRelease( s_pOnClose );
   }

   if ( HB_IS_ARRAY( handler ) )
   {
      s_pOnClose = hb_itemNew( 0 );
      hb_itemCopy( s_pOnClose, handler );
      hb_gcLock( s_pOnClose );
      return TRUE;
   }
   else if ( HB_IS_STRING( handler ) || HB_IS_NUMERIC( handler ) || HB_IS_BLOCK( handler ) )
   {
      s_pOnClose = hb_itemNew( 0 );
      hb_arrayNew( s_pOnClose, 1 );
      hb_itemCopy( hb_arrayGetItemPtr( s_pOnClose, 1 ), handler );
      hb_gcLock( s_pOnClose );
      return TRUE;
   }

   return FALSE;
}

PHB_ITEM HB_EXPORT hb_gtGetCloseHandler()
{
   return s_pOnClose;
}
/******************************************************************/

void HB_EXPORT hb_gtSetCloseEvent( int iEvent )
{
  s_closeEvent = iEvent;
}

void HB_EXPORT hb_gtSetShutdownEvent( int iEvent )
{
   s_shutdownEvent = iEvent;
}

int HB_EXPORT hb_gtGetCloseEvent( void )
{
  return( s_closeEvent );
}

int HB_EXPORT hb_gtGetShutdownEvent( void )
{
  return( s_shutdownEvent );
}

/******************************************************************/
void HB_EXPORT hb_gtSetResizeEvent( int iEvent )
{
   s_resizeEvent = iEvent;
}

int HB_EXPORT hb_gtGetResizeEvent( void )
{
  return s_resizeEvent;
}

void HB_EXPORT hb_gtHandleResize( void )
{
   if ( s_resizeEvent != 0 )
   {
      hb_inkeyPut( s_resizeEvent );
   }
}

/******************************************************************/
void HB_EXPORT hb_gtAddGobject( HB_GT_GOBJECT *gobject )
{
   gobject->next = NULL;
   gobject->prev = hb_gt_gobjects_end;

   if ( hb_gt_gobjects == NULL )
   {
      hb_gt_gobjects = gobject;
   }
   else
   {
      hb_gt_gobjects_end->next = gobject;
   }

   hb_gt_gobjects_end = gobject;
}

/* WARNING: This functions does NOT unlinks the object, it just free the memory
   it has allocated */
void HB_EXPORT hb_gtDestroyGobject( HB_GT_GOBJECT *gobject )
{
   switch( gobject->type )
   {
      case GTO_TEXT: hb_xfree( gobject->data ); break;
      /* Other special deletion may go here. */
   }

   hb_xfree( gobject );
}

void HB_EXPORT hb_gtClearGobjects( void )
{
   HB_GT_GOBJECT *p, *pnext;

   p = hb_gt_gobjects;

   while ( p )
   {
      pnext = p->next;
      hb_gtDestroyGobject( p );
      p = pnext;
   }

   hb_gt_gobjects = NULL;
   hb_gt_gobjects_end = NULL;
}

HB_EXPORT HB_GT_COLDEF* hb_gt_gcolorFromString( char *color_name )
{
   int i;


   for ( i = 0; i < HB_GT_COLDEF_COUNT; i ++ )
   {
      if ( hb_stricmp( color_name, hb_gt_gcoldefs[ i ].name ) == 0 )
      {
         return &(hb_gt_gcoldefs[0]) + i;
      }
   }

   return NULL;
}

BOOL HB_EXPORT hb_gtGobjectInside( HB_GT_GOBJECT *gobject,
   int x1, int y1, int x2, int y2 )
{
   int xx2, yy2;
   int xmin, ymin;
   int xmax,ymax;

   if( gobject->x >= x1 && gobject->x <= x2 &&
      gobject->y >= y1 && gobject->y <= y2 )
   {
      return TRUE;
   }

   /* using width/height? */
   if ( gobject->type == GTO_LINE ) // line has width == x2
   {
      xx2 = gobject->width;
      yy2 = gobject->height;
   }
   else if ( gobject->type != GTO_POINT && gobject->type != GTO_TEXT )
   {
      xx2 = gobject->x + gobject->width;
      yy2 = gobject->y + gobject->height;
   }
   else
   {
      return FALSE;
   }

   xmin = x1 > gobject->x ? x1 : gobject->x;
   xmax = x2 < xx2 ? x2 : xx2;
   ymin = y1 > gobject->y ? y1 : gobject->y;
   ymax = y2 < yy2 ? y2 : yy2;

   if ( xmin < xmax && ymin < ymax && xmin >= x1 && xmax <= x2 && ymin >= y1 && ymax <= y2 )
   {
      return TRUE;
   }

   return FALSE;
}

/******************************************************************/
void HB_EXPORT hb_gt_hasChanged( int status )
{
   
}