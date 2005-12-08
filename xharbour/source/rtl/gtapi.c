/*
 * $Id: gtapi.c,v 1.58 2005/10/24 01:04:35 druzus Exp $
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
 *    SetGTCloseHandler() system
 *
 * Copyright 2004 Henryk Olkowski <oh1@op.pl>
 *    CT3 Windows API
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <ctype.h>

#include "hbapigt.h"
#include "hbapiitm.h"
#include "hbset.h"
#include "hb_io.h"
#include "hbvm.h"
#include "inkey.ch"

/****************************************************************************/
/* gt API static */

static BOOL   s_bInit = FALSE;

static SHORT  s_iRow;
static SHORT  s_iCol;
static SHORT  s_Width;            /* added for optimization */
static SHORT  s_Height;           /* added for optimization */

static USHORT s_uiPreCount;
static USHORT s_uiPreCNest;
static USHORT s_uiCursorStyle;
static BOOL   s_ScNone;           /* Cursor invisible (out bounds) */

static USHORT s_uiColorIndex;
static USHORT s_uiColorCount;
static int *  s_pColor;

/* Close event returned by inkey for asynchronous GT kills */
static int s_closeEvent = 0;
static int s_shutdownEvent = 0;
static int s_resizeEvent = 0;
static BOOL s_closing = FALSE;
static PHB_ITEM s_pOnClose = NULL;

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

/****************************************************************************/
/* CT3 Windows API static (oh1) */

#define CLEARB_DEFAULT ' '             // ct_ClearB default value
        // Clipper uses 255
        // but we cannot not all platform/character set uses 255 as blank char

static BOOL         ct_Init  = FALSE;

static SHORT        ct_BFRow = 0;      // Wboard First Row
static SHORT        ct_BFCol = 0;      // Wboard First Column
static SHORT        ct_BLRow = 0;      // Wboard Last Row
static SHORT        ct_BLCol = 0;      // Wboard Last Column

static SHORT        ct_WFRow = 0;      // Window First Row
static SHORT        ct_WFCol = 0;      // Window First Column
static SHORT        ct_WLRow = 0;      // Window Last Row
static SHORT        ct_WLCol = 0;      // Window Last Column
static SHORT        ct_WNRow = -1;     // Window Number of Rows
static SHORT        ct_WNCol = -1;     // Window Number of Columns

static SHORT        ct_UFRow = 0;      // Used First Window Row
static SHORT        ct_UFCol = 0;      // Used First Window Column
static SHORT        ct_ULRow = 0;      // Used Last Window Row
static SHORT        ct_ULCol = 0;      // Used Last Window Column

static HB_CT_WND ** ct_Wind  = 0;      // Table of Windows Descriptions
static SHORT        ct_WMax  = 0;      // Size of ct_Wind[]
static HB_CT_WND *  ct_WCur  = NULL;   // Current Window Description
static SHORT        ct_NCur  = -1;     // Current Window Number
static SHORT *      ct_Stac  = 0;      // Table of Windows Stack
static SHORT        ct_SMax  = 0;      // Size of ct_Stac[]

static BOOL         ct_MMode = TRUE;   // Windows Interactive Movement Mode
static BOOL         ct_MFRow = FALSE;  // Move After Screen Top
static BOOL         ct_MFCol = FALSE;  // Move After Screen Left
static BOOL         ct_MLRow = FALSE;  // Move After Screen Bottom
static BOOL         ct_MLCol = FALSE;  // Move After Screen Right
static SHORT        ct_MRStep = 2;     // Move Rows Step
static SHORT        ct_MCStep = 5;     // Move Columns Step

static SHORT        ct_ClearA = 7;     // Windows Clear Attribute
static SHORT        ct_ClearB = CLEARB_DEFAULT;   // Windows Clear Char
static SHORT        ct_ShadowA = -1;   // Windows Shadow Attribute
static UINT         ct_CSize   = 0;    // Windows Buffer One Char Size

static void hb_ctInit( void );
static void hb_ctExit( void );
static void hb_ctSARest( SHORT FRow, SHORT FCol, SHORT LRow, SHORT LCol, void * uiAddr );
static void hb_ctSCSave( SHORT FRow, SHORT FCol, SHORT LRow, SHORT LCol, void ** uiAddr );
static void hb_ctSCRest( SHORT FRow, SHORT FCol, SHORT LRow, SHORT LCol, void * uiAddr );
static void hb_ctWBRest( HB_CT_WND * wnd );
static void hb_ctWBSave( HB_CT_WND * wnd );
static void hb_ctWFRest( HB_CT_WND * wnd );
static void hb_ctWFSave( HB_CT_WND * wnd );
static void hb_ctWSDisp( HB_CT_WND * wnd );

/****************************************************************************/
/* gt API functions                                                         */
/****************************************************************************/

/****************************************************************************/
void hb_gtInit( int s_iFilenoStdin, int s_iFilenoStdout, int s_iFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtInit()"));

   hb_gt_gobjects = NULL;

   s_uiColorCount = HB_CLR_MAX_ + 1;
   s_pColor = ( int * ) hb_xgrab( s_uiColorCount * sizeof( int ) );

#if ( defined(HB_OS_WIN_32_USED) || defined(__WIN32__) )
   if( hb_cmdargCheck( "NOMOUSE" ) )
      /* Mouse is disabled here */
      b_MouseEnable = FALSE;
#endif

   hb_gt_Init( s_iFilenoStdin, s_iFilenoStdout, s_iFilenoStderr );

   hb_gtSetColorStr( hb_set.HB_SET_COLOR );

   s_iRow = hb_gt_Row();
   s_iCol = hb_gt_Col();
   s_ScNone = FALSE;
   s_uiPreCount = 0;
   s_uiPreCNest = 0;
   s_Height = hb_gt_GetScreenHeight();
   s_Width = hb_gt_GetScreenWidth();

   hb_ctInit();

   /* This should be called after s_iRow/s_iCol initialization. */
   hb_gtSetCursor( SC_NORMAL );

   s_bInit = TRUE;

   if( hb_cmdargCheck( "INFO" ) )
   {
      hb_conOutErr( hb_gt_Version( 1 ), 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
   }

   /* we assume that the screen stays a fixed size from now on
      this avoids many function calls                          */
}

/****************************************************************************/
void hb_gtExit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtExit()"));

   if( s_bInit )
   {
      s_bInit = FALSE;

      hb_ctExit();

      while( hb_gt_DispCount() )
         hb_gt_DispEnd();

      hb_gt_Exit();

      if ( s_pOnClose != NULL )
      {
         hb_itemRelease( s_pOnClose );
         s_pOnClose = NULL;
      }
      hb_xfree( s_pColor );
   }

   hb_gtClearGobjects();
}

/****************************************************************************/
int HB_EXPORT hb_gtExtendedKeySupport()
{
    return hb_gt_ExtendedKeySupport();
}

/****************************************************************************/
int HB_EXPORT hb_gtReadKey( HB_inkey_enum eventmask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtReadKey(%d)", (int) eventmask));

   return hb_gt_ReadKey( eventmask );
}

/****************************************************************************/
void HB_EXPORT hb_gtAdjustPos( int iHandle, const char * pStr, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtAdjustPos()"));

   if( isatty( iHandle ) && hb_gt_AdjustPos( ( BYTE* ) pStr, ulLen ) )
   {
      /* Adjust the console cursor position to match the device driver */
      s_iRow = hb_gt_Row();
      s_iCol = hb_gt_Col();
   }
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtBox( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
                           BYTE * pbyFrame )
{
   SHORT p_WFRow = ct_BFRow, p_WFCol = ct_BFCol;
   SHORT FRow, FCol, LRow, LCol;
   USHORT Ret = 1;
   USHORT tmp;
   BYTE cPadChar, byAttr;
   BYTE szBox[ 10 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_gtBox(%hd, %hd, %hd, %hd, %p)", Top, Left, Bottom, Right, pbyFrame));

   hb_gtDispBegin();

   if( ct_NCur > 0 &&
        ( ct_WFRow < ct_BFRow || ct_WLRow > ct_BLRow ||
          ct_WFCol < ct_BFCol || ct_WLCol > ct_BLCol ) )
   {
      p_WFRow = ct_WFRow; p_WFCol = ct_WFCol;
      hb_ctWMove( ct_BFRow, ct_BFCol );
   }

   Top     += ct_UFRow;
   Left    += ct_UFCol;
   Bottom  += ct_UFRow;
   Right   += ct_UFCol;

   /* NOTE: For full compatibility, pad box string with last char if too
            short [vszakats] */

   cPadChar = ' ';

   for( tmp = 0; *pbyFrame && tmp < 9; tmp++ )
   {
      cPadChar = szBox[ tmp ] = *pbyFrame++;
   }

   while( tmp < 8 )
   {
      szBox[ tmp++ ] = cPadChar;
   }

   szBox[ tmp ] = '\0';

   if( s_Width > 0 && ct_WMax > 0 )         // CT3 new version
   {
      if( Bottom < ct_UFRow ) Bottom = ct_ULRow + 1;
      if( Right  < ct_UFCol ) Right  = ct_ULCol + 1;

      byAttr = s_pColor[ s_uiColorIndex ];

      FRow = HB_MAX( Top,    ct_UFRow );
      FCol = HB_MAX( Left,   ct_UFCol );
      LRow = HB_MIN( Bottom, ct_ULRow );
      LCol = HB_MIN( Right,  ct_ULCol );

      if( LRow > FRow && LCol > FCol )                     // Box
      {
         FRow = HB_MAX( Top + 1,    ct_UFRow );
         FCol = HB_MAX( Left + 1,   ct_UFCol );
         LRow = HB_MIN( Bottom - 1, ct_ULRow );
         LCol = HB_MIN( Right - 1,  ct_ULCol );

         if( Top >= ct_UFRow && Top <= ct_ULRow &&
             Left >= ct_UFCol && Left <= ct_ULCol )    /* Top left corner */
         {
            hb_gt_Replicate( Top, Left, byAttr, szBox[ 0 ], 1 );
            Ret = 0;
         }

         if( Top >= ct_UFRow && Top <= ct_ULRow && FCol <= LCol &&
             FCol >= ct_UFCol && LCol <= ct_ULCol )    /* Top Line */
         {
            Ret = hb_gt_HorizLine( Top, FCol, LCol, szBox[ 1 ], byAttr );
         }

         if( Top >= ct_UFRow && Top <= ct_ULRow &&
             Right >= ct_UFCol && Right <= ct_ULCol )  /* Top right corner */
         {
            hb_gt_Replicate( Top, Right, byAttr, szBox[ 2 ], 1 );
            Ret = 0;
         }

         if( FRow >= ct_UFRow && LRow <= ct_ULRow && FRow <= LRow &&
             Right >= ct_UFCol && Right <= ct_ULCol )  /* Right Line */
         {
            Ret = hb_gt_VertLine( Right, FRow, LRow, szBox[ 3 ], byAttr );
         }

         if( Bottom >= ct_UFRow && Bottom <= ct_ULRow &&
             Right >= ct_UFCol && Right <= ct_ULCol )  /* Bottom right corner */
         {
            hb_gt_Replicate( Bottom, Right, byAttr, szBox[ 4 ], 1 );
            Ret = 0;
         }

         if( Bottom >= ct_UFRow && Bottom <= ct_ULRow && FCol <= LCol &&
             FCol >= ct_UFCol && LCol <= ct_ULCol )    /* Bottom Line */
         {
            Ret = hb_gt_HorizLine( Bottom, FCol, LCol, szBox[ 5 ], byAttr );
         }

         if( Bottom >= ct_UFRow && Bottom <= ct_ULRow &&
             Left >= ct_UFCol && Left <= ct_ULCol )    /* Bottom left corner */
         {
            hb_gt_Replicate( Bottom, Left, byAttr, szBox[ 6 ], 1 );
            Ret = 0;
         }

         if( FRow >= ct_UFRow && LRow <= ct_ULRow && FRow <= LRow &&
             Left >= ct_UFCol && Left <= ct_ULCol )    /* Left Line */
         {
            Ret = hb_gt_VertLine( Left, FRow, LRow, szBox[ 7 ], byAttr );
         }

         if( FRow <= LRow && FCol <= LCol && szBox[ 8 ] )
         {
            for( tmp = FRow; tmp <= LRow; tmp++ )      /* Fill box */
            {
               hb_gt_Replicate( tmp, FCol, byAttr, szBox[ 8 ],
                                LCol - FCol + 1 );
               Ret = 0;
            }
         }

      }
      else if( LRow > FRow && LCol == FCol )             // Vertical Line
      {
         if( FRow >= ct_UFRow && LRow <= ct_ULRow &&
             FCol >= ct_UFCol && FCol <= ct_ULCol )
         {
            Ret = hb_gt_VertLine( FCol, FRow, LRow, szBox[ 3 ], byAttr );
         }
      }
      else if( LRow == FRow && LCol >= FCol )     // Horizontal Line or Point
      {
         if( FRow >= ct_UFRow && FRow <= ct_ULRow &&
             FCol >= ct_UFCol && LCol <= ct_ULCol )
         {
            Ret = hb_gt_HorizLine( FRow, FCol, LCol, szBox[ 1 ], byAttr );
         }
      }
   }
   else                                     //--- Old GT version ----//
   {
      if( Top != Bottom )
      {
         if( Left != Right )
         {
            Ret = hb_gt_Box( Top, Left, Bottom, Right, szBox,
                             ( BYTE ) s_pColor[ s_uiColorIndex ] );
         }
         else
         {
            Ret = hb_gt_VertLine( Left, Top, Bottom, szBox[ 3 ],
                                  ( BYTE ) s_pColor[ s_uiColorIndex ] );
         }
      }
      else
      {
         Ret = hb_gt_HorizLine( Top, Left, Right, szBox[ 1 ],
                                ( BYTE ) s_pColor[ s_uiColorIndex ] );
      }
   }

   if( Ret == 0 )
   {
      hb_gtSetPosContext( HB_MAX( Top + 1, ct_WFRow ) - ct_UFRow,
                          HB_MAX( Left + 1, ct_UFCol ) - ct_UFCol,
                          HB_GT_SET_POS_AFTER );
   }

   if( p_WFRow != ct_BFRow || p_WFCol != ct_BFCol )
   {
      hb_ctWMove( p_WFRow, p_WFCol );
   }
   hb_gtDispEnd();

   return Ret;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtBoxD( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right )
{
   USHORT Ret;

   Ret = hb_gtBox( Top, Left, Bottom, Right, ( BYTE* ) _B_DOUBLE );

/* //--- Old version ----//
   if( Top != Bottom )
   {
      if( Left != Right )
         Ret = hb_gt_BoxD( Top, Left, Bottom, Right, ( BYTE* ) _B_DOUBLE, ( BYTE ) s_pColor[ s_uiColorIndex ] );
      else
         Ret = hb_gt_VertLine( Left, Top, Bottom, (BYTE) HB_B_DOUBLE_V, ( BYTE ) s_pColor[ s_uiColorIndex ] );
   }
   else
      Ret = hb_gt_HorizLine( Top, Left, Right, (BYTE) HB_B_DOUBLE_H, ( BYTE ) s_pColor[ s_uiColorIndex ] );

   hb_gtSetPosContext( HB_MAX(Top,0) + 1, HB_MAX(Left,0) + 1, HB_GT_SET_POS_AFTER );
*/

   return Ret;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtBoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right )
{
   USHORT Ret;

   Ret = hb_gtBox( Top, Left, Bottom, Right, ( BYTE* ) _B_SINGLE );

/* //--- Old version ----//
   if( Top != Bottom )
   {
      if( Left != Right )
      {
         Ret = hb_gt_BoxS( Top, Left, Bottom, Right, ( BYTE* ) _B_SINGLE, ( BYTE ) s_pColor[ s_uiColorIndex ] );
      }
      else
      {
         Ret = hb_gt_VertLine( Left, Top, Bottom, (BYTE) HB_B_SINGLE_V, ( BYTE ) s_pColor[ s_uiColorIndex ] );
      }
   }
   else
   {
      Ret = hb_gt_HorizLine( Top, Left, Right, (BYTE) HB_B_SINGLE_H, ( BYTE ) s_pColor[ s_uiColorIndex ] );
   }

   hb_gtSetPosContext( HB_MAX(Top,0) + 1, HB_MAX(Left,0) + 1, HB_GT_SET_POS_AFTER );
*/

   return Ret;
}

/****************************************************************************/
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

/****************************************************************************/
USHORT HB_EXPORT hb_gtDispBegin( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtDispBegin()"));

   if( s_uiPreCount == 0 )
      hb_gt_DispBegin();
   else
      ++s_uiPreCount;  /* defined in each terminal driver */

   return 0;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtDispCount( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtDispCount()"));

   return hb_gt_DispCount();
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtDispEnd( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtDispEnd()"));

   if( s_uiPreCount == 0 )
      hb_gt_DispEnd();
   else
      --s_uiPreCount;

   return 0;
}

/****************************************************************************/
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

/****************************************************************************/
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

/****************************************************************************/
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

/****************************************************************************/
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

/****************************************************************************/
USHORT HB_EXPORT hb_gtSetColorStr( char * szColorString )
{
   char c;
   char buff[ 7 ];
   BOOL bHasI = FALSE;
   BOOL bHasU = FALSE;
   BOOL bHasX = FALSE;
   BOOL bSlash = FALSE;
   int nPos = 0;
   int nFore = 0;
   int nColor = 0;
   int nCount = -1, i = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetColorStr(%s)", szColorString));

   if( szColorString == ( char * ) NULL )
   {
      return 1;
   }

   if( *szColorString == '\0' )
   {
      s_pColor[ 0 ] = 0x07;
      s_pColor[ 1 ] = 0x70;
      s_pColor[ 2 ] = 0;
      s_pColor[ 3 ] = 0;
      s_pColor[ 4 ] = 0x70;
   }

   do
   {
      c = *szColorString++;

      while( c >= '0' && c <= '9' && i < 6 )
      {
         buff[ i++ ] = c;
         c = *szColorString++;
      }

      if( i > 0 )
      {
         buff[ i ] = '\0';
         nColor = atoi( buff ) & 0x0F;
         ++nCount;
         i = 0;
      }
      ++nCount;

      switch( toupper( c ) )
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
            {
               nFore = s_pColor[ nPos ];
            }
            nCount = -1;
            if( nPos == s_uiColorCount )
            {
               s_pColor = ( int * ) hb_xrealloc( s_pColor,
                                                 sizeof( int ) * ( nPos + 1 ) );
               ++s_uiColorCount;
            }
            if( bHasX )
            {
               nFore &= 0x88F8;
            }

            if( bHasU ) /* background if slash, else foreground */
            {
               nColor |= 0x0800;
            }

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
            {
               nFore |= 1;
            }

            if( bSlash )
            {
               s_pColor[ nPos++ ] = ( nColor << 4 ) | nFore;
            }
            else
            {
               s_pColor[ nPos++ ] = nColor | nFore;
            }

            nColor = nFore = 0;
            bSlash = bHasX = bHasU = bHasI = FALSE;
      }
   }
   while( c );

   if( nPos > 1 && nPos < 4 )
   {
      s_pColor[ 4 ] = s_pColor[ 1 ];
   }

   s_uiColorIndex = HB_CLR_STANDARD; /* hb_gtColorSelect( HB_CLR_STANDARD ); */

   return 0;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtGetCursor( USHORT * uipCursorStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetCursor(%p)", uipCursorStyle));

   /* 16/08/2004 - <maurilio.longo@libero.it>
                   Wrong since hb_gtSetPos() can call hb_gt_SetCursor(SC_NONE)
                   to force it off when cursor is out of screen bounds.
                   When, later, we ask to gtapi which is our cursor shape
                   we have to answer with the last one set with hb_gtSetCursor()
                   and not lower level hb_gt_XXX current one.

   *uipCursorStyle = hb_gt_GetCursorStyle();
   */

   *uipCursorStyle = s_uiCursorStyle;

   return 0;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtSetCursor( USHORT uiCursorStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetCursor(%hu)", uiCursorStyle));

   if( uiCursorStyle <= SC_SPECIAL2 )
   {
      /* Set the cursor only when, it's in bounds. */
      if( s_iRow >= ct_WFRow && s_iRow <= ct_ULRow &&
          s_iCol >= ct_UFCol && s_iCol <= ct_ULCol &&
          ( ct_NCur == 0 ||
            ( s_iRow >= ct_BFRow && s_iRow <= ct_BLRow &&
              s_iCol >= ct_BFCol && s_iCol <= ct_BLCol ) ) )
      {
         hb_gt_SetCursorStyle( uiCursorStyle );
      }

      s_uiCursorStyle = uiCursorStyle;

      return 0;
   }
   else
      return 1;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtGetPos( SHORT * piRow, SHORT * piCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetPos(%p, %p)", piRow, piCol));

   if( s_Width <= 0 || !s_ScNone )
   {
      /* Only return the actual cursor position if the current
         cursor position was not previously set out of bounds. */
      s_iRow = hb_gt_Row();
      s_iCol = hb_gt_Col();
   }

   *piRow = s_iRow - ct_UFRow;
   *piCol = s_iCol - ct_UFCol;

   return 0;
}

/****************************************************************************/
/* NOTE: Should be exactly the same as hb_gtSetPosContext(), but without the
         additional third parameter. */

USHORT HB_EXPORT hb_gtSetPos( SHORT iRow, SHORT iCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetPos(%hd, %hd)", iRow, iCol));

   iRow += ct_UFRow;
   iCol += ct_UFCol;

   /* Validate the new cursor position */
   if( s_Width <= 0 ||
       ( ( iRow >= ct_WFRow && iRow <= ct_ULRow &&
           iCol >= ct_UFCol && iCol <= ct_ULCol ) &&
         ( ct_NCur == 0 ||
           ( iRow >= ct_BFRow && iRow <= ct_BLRow &&
             iCol >= ct_BFCol && iCol <= ct_BLCol ) ) ) )
   {
      hb_gt_SetPos( iRow, iCol, HB_GT_SET_POS_BEFORE );

      /* If cursor was out bounds, now enable it */

      if( s_ScNone ) hb_gt_SetCursorStyle( s_uiCursorStyle );
      s_ScNone = FALSE;
   }
   else
   {
      hb_gt_SetCursorStyle( SC_NONE ); /* Disable cursor if out of bounds */
      s_ScNone = TRUE;
   }

   s_iRow = iRow;
   s_iCol = iCol;

   return 0;
}

/****************************************************************************/
/* NOTE: Should be exactly the same as hb_gtSetPos(), but with the additional
         parameter. */

USHORT HB_EXPORT hb_gtSetPosContext( SHORT iRow, SHORT iCol, SHORT iMethod )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetPosContext(%hd, %hd, %hd)", iRow, iCol, iMethod));

   iRow += ct_UFRow;
   iCol += ct_UFCol;

   /* Validate the new cursor position */
   if( s_Width <= 0 ||
       ( ( iRow >= ct_WFRow && iRow <= ct_ULRow &&
           iCol >= ct_UFCol && iCol <= ct_ULCol ) &&
         ( ct_NCur == 0 ||
           ( iRow >= ct_BFRow && iRow <= ct_BLRow &&
             iCol >= ct_BFCol && iCol <= ct_BLCol ) ) ) )
   {
      hb_gt_SetPos( iRow, iCol, iMethod );

      /* If cursor was out bounds, now enable it */

      if( s_ScNone ) hb_gt_SetCursorStyle( s_uiCursorStyle );
      s_ScNone = FALSE;
   }
   else
   {
      hb_gt_SetCursorStyle( SC_NONE ); /* Disable cursor if out of bounds */
      s_ScNone = TRUE;
   }

   s_iRow = iRow;
   s_iCol = iCol;

   return 0;
}

/****************************************************************************/
BOOL HB_EXPORT hb_gtIsColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtIsColor()"));

   return hb_gt_IsColor();
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtMaxCol( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtMaxCol()"));

   return s_Width > 0 ? ct_ULCol - ct_UFCol: 0;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtMaxRow( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtMaxRow()"));

   return ct_ULRow - ct_UFRow;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtRectSize( USHORT uiTop, USHORT uiLeft, USHORT uiBottom,
                                USHORT uiRight, UINT * uipBuffSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtRectSize(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, uipBuffSize));

   *uipBuffSize = hb_gt_RectSize( uiBottom - uiTop + 1, uiRight - uiLeft + 1 );

   return 0;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtRepChar( USHORT uiRow, USHORT uiCol, BYTE byChar,
                               USHORT uiCount )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtRepChar(%hu, %hu, %d, %hu)", uiRow, uiCol, (int) byChar, uiCount));

   if( s_Width > 0 )
   {
      uiRow += ct_UFRow;
      uiCol += ct_UFCol;

      if( uiRow < ct_WFRow ) uiRow = ct_WFRow;
      if( uiRow > ct_ULRow ) uiRow = ct_ULRow;
      if( uiCol < ct_UFCol ) uiCol = ct_UFCol;
      if( uiCol > ct_ULCol ) uiCol = ct_ULCol;

      if( uiCount > ( ct_ULRow - uiRow ) * ct_WNCol + ct_ULCol - uiCol + 1 )
      {
         uiCount = ( ct_ULRow - uiRow ) * ct_WNCol + ct_ULCol - uiCol + 1;
      }
   }

   hb_gt_Replicate( uiRow, uiCol, ( BYTE ) s_pColor[ s_uiColorIndex ],
                    byChar, uiCount );

   return 0;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtRest( USHORT uiTop, USHORT uiLeft, USHORT uiBottom,
                            USHORT uiRight, void * pScrBuff )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtRest(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pScrBuff));

   uiTop    += ct_UFRow;
   uiLeft   += ct_UFCol;
   uiBottom += ct_UFRow;
   uiRight  += ct_UFCol;

   hb_gt_PutText( uiTop, uiLeft, uiBottom, uiRight, ( BYTE* ) pScrBuff );

   return 0;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtSave( USHORT uiTop, USHORT uiLeft, USHORT uiBottom,
                            USHORT uiRight, void * pScrBuff )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSave(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pScrBuff));

   uiTop    += ct_UFRow;
   uiLeft   += ct_UFCol;
   uiBottom += ct_UFRow;
   uiRight  += ct_UFCol;

   hb_gt_GetText( uiTop, uiLeft, uiBottom, uiRight, ( BYTE* ) pScrBuff );

   return 0;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtScrDim( USHORT * uipHeight, USHORT * uipWidth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtScrDim(%p, %p)", uipHeight, uipWidth));

   *uipHeight = s_Height - 1;
   *uipWidth = s_Width > 0 ? s_Width - 1: -1;

   return 0;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtGetBlink( BOOL * bpBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetBlink(%p)", bpBlink));

   *bpBlink = hb_gt_GetBlink();

   return 0;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtSetBlink( BOOL bBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetBlink(%d)", (int) bBlink));

   hb_gt_SetBlink( bBlink );

   return 0;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtSetMode( USHORT uiRows, USHORT uiCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetMode(%hu, %hu)", uiRows, uiCols));

   if ( ct_SMax <= 1 && hb_gt_SetMode( uiRows, uiCols ) )
   {
      s_Height = uiRows;
      s_Width  = uiCols;

      ct_BLRow = ct_WLRow = ct_ULRow = HB_MAX( 0, s_Height - 1 );
      ct_BLCol = ct_WLCol = ct_ULCol = HB_MAX( 0, s_Width - 1 );

      if( ct_WCur != NULL )
      {
         ct_WCur->BLRow = ct_WCur->WLRow = ct_WCur->ULRow = ct_BLRow;
         ct_WCur->BLCol = ct_WCur->WLCol = ct_WCur->ULCol = ct_BLCol;
      }

      return 0;
   }

   return 1;
}

/****************************************************************************/
/* NOTE: This is a compatibility function.
         If you're running on a CGA and snow is a problem speak up! */

USHORT HB_EXPORT hb_gtSetSnowFlag( BOOL bNoSnow )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetSnowFlag(%d)", (int) bNoSnow));

   HB_SYMBOL_UNUSED( bNoSnow );

   return 0;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtWrite( BYTE * pStr, ULONG ulLength )
{
   SHORT p_WFRow = ct_BFRow, p_WFCol = ct_BFCol;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtWrite(%p, %lu)", pStr, ulLength));

   /* Display the text if the cursor is on screen */
   if( s_Width <= 0 ||
       ( s_iRow >= ct_WFRow && s_iRow <= ct_ULRow &&
         s_iCol >= ct_UFCol && s_iCol <= ct_ULCol ) )
   {
      hb_gtDispBegin();

      if( ct_NCur > 0 &&
           ( ct_WFRow < ct_BFRow || ct_WLRow > ct_BLRow ||
             ct_WFCol < ct_BFCol || ct_WLCol > ct_BLCol ) )
      {
         p_WFRow = ct_WFRow; p_WFCol = ct_WFCol;
         hb_ctWMove( ct_BFRow, ct_BFCol );
      }

      /* Truncate the text if the cursor will end up off the right edge */
      hb_gt_Puts( s_iRow, s_iCol, ( BYTE ) s_pColor[ s_uiColorIndex ], pStr,
                  s_Width > 0 ?
                     HB_MIN( ulLength, ( ULONG ) ( ct_ULCol - s_iCol + 1 ) ):
                     ulLength );

      if( p_WFRow != ct_BFRow || p_WFCol != ct_BFCol )
      {
         hb_ctWMove( p_WFRow, p_WFCol );
      }

      /* Finally, save the new cursor position, even if off-screen */
      hb_gtSetPosContext( s_iRow - ct_UFRow,
                          s_iCol + ( SHORT ) ulLength - ct_UFCol,
                          HB_GT_SET_POS_AFTER );

      /* Test End of line */
      if ( ct_WMax > 1 && s_Width > 0 && s_iCol > ct_ULCol )
      {
         if ( s_iRow < ct_ULRow )
            hb_gtSetPosContext( s_iRow - ct_UFRow + 1, 0, HB_GT_SET_POS_AFTER );
         else
            hb_gtSetPosContext( s_iRow - ct_UFRow , ct_ULCol - ct_UFCol,
                                HB_GT_SET_POS_AFTER );
      }
      hb_gtDispEnd();
   }

   return 0;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtWriteAt( USHORT uiRow, USHORT uiCol, BYTE * pStr,
                               ULONG ulLength, BOOL bSetPos )
{
   SHORT p_WFRow = ct_BFRow, p_WFCol = ct_BFCol;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtWriteAt(%hu, %hu, %p, %lu)", uiRow, uiCol, pStr, ulLength));

   uiRow += ct_UFRow;
   uiCol += ct_UFCol;

   /* Display the text if the cursor is on screen */
   if( s_Width <= 0 ||
       ( uiRow >= ct_WFRow && uiRow <= ct_ULRow &&
         uiCol >= ct_UFCol && uiCol <= ct_ULCol ) )
   {
      hb_gtDispBegin();

      if( ct_NCur > 0 &&
           ( ct_WFRow < ct_BFRow || ct_WLRow > ct_BLRow ||
             ct_WFCol < ct_BFCol || ct_WLCol > ct_BLCol ) )
      {
         p_WFRow = ct_WFRow; p_WFCol = ct_WFCol;
         hb_ctWMove( ct_BFRow, ct_BFCol );
         uiRow += ct_BFRow - p_WFRow;
         uiCol += ct_BFCol - p_WFCol;
      }

      /* Truncate the text if the cursor will end up off the right edge */
      hb_gt_Puts( uiRow, uiCol, ( BYTE ) s_pColor[ s_uiColorIndex ], pStr,
                  s_Width > 0 ?
                    HB_MIN( ulLength, ( ULONG ) ( ct_ULCol - uiCol + 1 ) ):
                    ulLength);

      if( p_WFRow != ct_BFRow || p_WFCol != ct_BFCol )
      {
         hb_ctWMove( p_WFRow, p_WFCol );
         uiRow -= ct_BFRow - p_WFRow;
         uiCol -= ct_BFCol - p_WFCol;
      }
      if ( bSetPos )
      {
         /* Finally, save the new cursor position, even if off-screen */
         hb_gtSetPosContext( uiRow - ct_UFRow,
                             uiCol + ( SHORT ) ulLength - ct_UFCol,
                             HB_GT_SET_POS_AFTER );

         /* Test End of line */
         if ( ct_WMax > 1 && s_Width > 0 && s_iCol > ct_ULCol )
         {
            if ( s_iRow < ct_ULRow )
               hb_gtSetPosContext( s_iRow - ct_UFRow + 1, 0, HB_GT_SET_POS_AFTER );
            else
               hb_gtSetPosContext( s_iRow - ct_UFRow , ct_ULCol - ct_UFCol,
                                   HB_GT_SET_POS_AFTER );
         }
      }
      hb_gtDispEnd();
   }

   return 0;
}

/****************************************************************************/
#define WRITECON_BUFFER_SIZE 512

USHORT HB_EXPORT hb_gtWriteCon( BYTE * pStr, ULONG ulLength )
{
   SHORT p_WFRow = ct_BFRow, p_WFCol = ct_BFCol;
   int iLen = 0;
   BOOL bDisp = FALSE;
   BOOL bNewLine = FALSE;
   SHORT iRow;
   SHORT iCol;
   SHORT iMaxRow;
   SHORT iMaxCol;
   BYTE szString[ WRITECON_BUFFER_SIZE ];

   HB_TRACE(HB_TR_DEBUG, ("hb_gtWriteCon(%p, %lu)", pStr, ulLength));

   hb_gtDispBegin();

   if( ct_NCur > 0 &&
        ( ct_WFRow < ct_BFRow || ct_WLRow > ct_BLRow ||
          ct_WFCol < ct_BFCol || ct_WLCol > ct_BLCol ) )
   {
      p_WFRow = ct_WFRow; p_WFCol = ct_WFCol;
      hb_ctWMove( ct_BFRow, ct_BFCol );
   }

   iMaxRow = ct_ULRow - ct_UFRow;
   iMaxCol = ct_ULCol - ct_UFCol;

   /* Limit the starting cursor position to maxrow(),maxcol()
      on the high end, but don't limit it on the low end. */

   iRow = ( s_iRow <= ct_ULRow )                ? s_iRow - ct_UFRow : iMaxRow;
   iCol = ( iMaxCol > 0 || s_iCol <= ct_ULCol ) ? s_iCol - ct_UFCol : iMaxCol;

   if( iRow != s_iRow - ct_UFRow || iCol != s_iCol - ct_UFCol )
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
            if( iRow >= ct_WFRow - ct_UFRow ) ++iRow;
            bDisp = TRUE;
            bNewLine = TRUE;
            break;

         case HB_CHAR_CR:
            iCol = 0;
            if( *pStr == HB_CHAR_LF )
            {
               if( iRow >= ct_WFRow - ct_UFRow ) ++iRow;
               bNewLine = TRUE;
               ++pStr;
               --ulLength;
            }
            bDisp = TRUE;
            break;

         default:
            ++iCol;
            if( iMaxCol > 0 && ( iCol > iMaxCol || iCol <= 0 ) )
            {
               /* If the cursor position started off the left edge,
                  don't display the first character of the string */

               if( iCol > 0 ) szString[ iLen++ ] = ch;

               /* Always advance to the first column of the next row
                  when the right edge is reached or when the cursor
                  started off the left edge, unless the cursor is off
                  the top edge, in which case only change the column */

               iCol = 0;
               if( iRow >= ct_WFRow - ct_UFRow ) ++iRow;
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
         if( iLen && s_iRow >= ct_WFRow )
            hb_gtWrite( szString, iLen );

         iLen = 0;
         if( iRow > iMaxRow )
         {
            /* Normal scroll */
            hb_gtScroll( 0, 0, iMaxRow, iMaxCol, iRow - iMaxRow, 0 );
            iRow = iMaxRow;
            iCol = 0;
         }
         else if( iRow < ct_WFRow - ct_UFRow && bNewLine )
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

   if( p_WFRow != ct_BFRow || p_WFCol != ct_BFCol )
   {
      hb_ctWMove( p_WFRow, p_WFCol );
   }
   hb_gtDispEnd();

   return 0;
}

/****************************************************************************/
USHORT HB_EXPORT hb_gtScroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom,
                              USHORT uiRight, SHORT iRows, SHORT iCols )
{
   SHORT p_WFRow = ct_BFRow, p_WFCol = ct_BFCol;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtScroll(%hu, %hu, %hu, %hu, %hd, %hd)", uiTop, uiLeft, uiBottom, uiRight, iRows, iCols));

   hb_gtDispBegin();

   if( ct_NCur > 0 &&
        ( ct_WFRow < ct_BFRow || ct_WFCol < ct_BFCol ||
          ct_WLRow > ct_BLRow || ct_WLCol > ct_BLCol ) )
   {
      p_WFRow = ct_WFRow; p_WFCol = ct_WFCol;
      hb_ctWMove( ct_BFRow, ct_BFCol );
   }

   uiTop    += ct_UFRow;
   uiLeft   += ct_UFCol;
   uiBottom += ct_UFRow;
   uiRight  += ct_UFCol;

   /* Complete scroll? */
   if ( uiTop == ct_UFRow && uiLeft == ct_UFCol &&
        uiBottom >= ct_ULRow && uiRight >= ct_ULCol &&
        iRows == 0 )
   {
      /* Destroy objects */
      hb_gtClearGobjects();
   }

   hb_gt_Scroll( uiTop, uiLeft, uiBottom, uiRight,
                 ( BYTE ) s_pColor[ s_uiColorIndex ], iRows, iCols );

   if( p_WFRow != ct_BFRow || p_WFCol != ct_BFCol )
   {
      hb_ctWMove( p_WFRow, p_WFCol );
   }

   hb_gtDispEnd();

   return 0;
}

/* NOTE: It would be better if the clipping was done by the low level API */

/****************************************************************************/
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

/****************************************************************************/
void HB_EXPORT hb_gtTone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtTone(%lf, %lf)", dFrequency, dDuration));

   hb_gt_Tone( dFrequency, dDuration );
}

/****************************************************************************/
char HB_EXPORT * hb_gtVersion( int iType )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtVersion()"));

   return hb_gt_Version( iType );
}
/****************************************************************************/
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

/****************************************************************************/
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

/****************************************************************************/
int HB_EXPORT hb_gtIndexedColor( int idx )
{
    return s_pColor[ idx ];
}

/****************************************************************************/
int HB_EXPORT hb_gtCurrentColor()
{
    return s_pColor[ s_uiColorIndex ];
}

/****************************************************************************/
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

   if( s_pOnClose == NULL )
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
   else
   {
      hb_itemRelease( hb_itemDo( s_pOnClose, 0 ) );
   }

   s_closing = FALSE;
}

/****************************************************************************/
BOOL HB_EXPORT hb_gtHandleShutdown()
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

/****************************************************************************/
BOOL HB_EXPORT hb_gtSetCloseHandler( PHB_ITEM handler )
{
   if ( s_pOnClose != NULL )
   {
      hb_itemRelease( s_pOnClose );
      s_pOnClose = NULL;
   }

   if ( HB_IS_ARRAY( handler ) )
   {
      s_pOnClose = hb_itemNew( handler );
      return TRUE;
   }
   else if ( HB_IS_STRING( handler ) || HB_IS_NUMERIC( handler ) || HB_IS_BLOCK( handler ) )
   {
      s_pOnClose = hb_itemNew( NULL );
      hb_arrayNew( s_pOnClose, 1 );
      return hb_arraySet( s_pOnClose, 1, handler );
   }

   return FALSE;
}

/****************************************************************************/
PHB_ITEM HB_EXPORT hb_gtGetCloseHandler()
{
   return s_pOnClose;
}
/****************************************************************************/

void HB_EXPORT hb_gtSetCloseEvent( int iEvent )
{
  s_closeEvent = iEvent;
}

/****************************************************************************/
void HB_EXPORT hb_gtSetShutdownEvent( int iEvent )
{
   s_shutdownEvent = iEvent;
}

/****************************************************************************/
int HB_EXPORT hb_gtGetCloseEvent( void )
{
  return( s_closeEvent );
}

/****************************************************************************/
int HB_EXPORT hb_gtGetShutdownEvent( void )
{
  return( s_shutdownEvent );
}

/****************************************************************************/
void HB_EXPORT hb_gtSetResizeEvent( int iEvent )
{
   s_resizeEvent = iEvent;
}

/****************************************************************************/
int HB_EXPORT hb_gtGetResizeEvent( void )
{
  return s_resizeEvent;
}

/****************************************************************************/
void HB_EXPORT hb_gtHandleResize( void )
{
   if ( s_resizeEvent != 0 )
   {
      hb_inkeyPut( s_resizeEvent );
   }
}

/****************************************************************************/
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

/****************************************************************************/
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

/****************************************************************************/
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

/****************************************************************************/
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

/****************************************************************************/
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

   if ( xmin < xmax && ymin < ymax && xmin >= x1 &&
        xmax <= x2 && ymin >= y1 && ymax <= y2 )
   {
      return TRUE;
   }

   return FALSE;
}

/****************************************************************************/
void HB_EXPORT hb_gt_hasChanged( int status )
{
   HB_SYMBOL_UNUSED( status );
}

/****************************************************************************/
void HB_EXPORT hb_gtGetClipboard( char *szData, ULONG *pulMaxSize )
{
   hb_gt_GetClipboard( szData, pulMaxSize );
}

/****************************************************************************/
void HB_EXPORT hb_gtSetClipboard( char *szData, ULONG ulSize )
{
   hb_gt_SetClipboard( szData, ulSize );
}

/****************************************************************************/
ULONG HB_EXPORT hb_gtGetClipboardSize( void )
{
   return hb_gt_GetClipboardSize();
}

void HB_EXPORT hb_gtProcessMessages( void )
{
   hb_gt_ProcessMessages();
   return ;
}

/****************************************************************************/
void HB_EXPORT hb_gtPasteFromClipboard( ULONG ulSize )
{
   ULONG ulClipSize, ulPos;
   char *szData;

   if ( ulSize == 0 )
   {
      /* Includes the extra 0 space */
      ulClipSize = hb_gtGetClipboardSize();
   }
   else
   {
      ulClipSize = ulSize;
   }

   if ( ulClipSize == 0 )
   {
      return;
   }

   szData = (char *) hb_xgrab( ulClipSize +1);
   hb_gt_GetClipboard( szData, &ulClipSize );

   for ( ulPos = 0; ulPos < ulClipSize; ulPos++ )
   {
      hb_inkeyPut( szData[ ulPos ] );
   }

   hb_xfree( szData );
}
/****************************************************************************/


/****************************************************************************/
/* New CT3 Windows API functions                                            */
/****************************************************************************/
/*
  hb_ctGetClearA()   - Get the default attribute for screen/window clear
  hb_ctGetClearB()   - Get the default character for screen/window clear
  hb_ctMaxCol()      - Get the highest column number for screen/window
  hb_ctMaxRow()      - Get the highest row number for screen/window
  hb_ctMouseCol()    - Get the mouse column number for screen/window
  hb_ctMouseRow()    - Get the mouse row number for screen/window
  hb_ctSetClearA()   - Set the default attribute for screen/window clear
  hb_ctSetClearB()   - Set the default character for screen/window clear
  hb_ctSetCurColor() - Set current color
  hb_ctSetPos()      - Move the cursor to a new position
  hb_ctWAClose()     - Close all windows
  hb_ctWBoard()      - Allocates screen area for windows
  hb_ctWCenter()     - Returns a window to the visible area, or centers it
  hb_ctWClose()      - Close the active window
  hb_ctWCurrent()    - Get the current windows info
  hb_ctWFormat()     - Set the usable area within a window
  hb_ctWFree()       - Free HB_CT_WND Table
  hb_ctWMode()       - Set the screen border overstep mode
  hb_ctWMove()       - Moves a window
  hb_ctWNew()        - Create new HB_CT_WND Table
  hb_ctWNum()        - Get the highest windows handle
  hb_ctWOpen()       - Opens a new window
  hb_ctWSelect()     - Activate Window
  hb_ctWSetMove()    - Set the interactive movement mode
  hb_ctWSetShadow()  - Set the window shadow color
  hb_ctWStep()       - Set the step width of interactive window movement

  WMCOL()            - Get the mouse column number for screen/window      (New)
  WMROW()            - Get the mouse row number for screen/window         (New)
  WINFO()            - Get the windows info                               (New)
  WSETMOUSE()        - Set the mouse cursor, row and column for windows   (New)
  WMSETPOS()         - Set the mouse row and column for windows           (New)

   Static functions:

  hb_ctInit()       - Initialize CT3 Windows API
  hb_ctExit()       - Destroy CT3 Windows API

  hb_ctSARest()     - Restore Window Screen Attribute
  hb_ctSCRest()     - Restore Window Screen Area
  hb_ctSCSave()     - Save Window Screen Area
  hb_ctWBRest()     - Restore Window Background
  hb_ctWBSave()     - Save Window Background
  hb_ctWFRest()     - Restore Window Foreground
  hb_ctWFSave()     - Save Window Foreground
  hb_ctWSDisp()     - Display Window Shadow
*/

/****************************************************************************/
/* Initialize CT3 Windows API */
static void hb_ctInit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctInit()"));

   if( ! ct_Init )
   {
      hb_gtRectSize( 1, 1, 1, 1, &ct_CSize );

      ct_BFRow = 0;
      ct_BFCol = 0;
      ct_BLRow = HB_MAX( 0, s_Height - 1 );
      ct_BLCol = HB_MAX( 0, s_Width - 1 );

      if( ct_CSize < 2 || ct_BLCol < 1 ) ct_CSize = 0;

      ct_WNRow = -1;
      ct_WNCol = -1;

      ct_MMode = TRUE;
      ct_MFRow = FALSE;
      ct_MFCol = FALSE;
      ct_MLRow = FALSE;
      ct_MLCol = FALSE;

      ct_MRStep = 2;     // Move Rows Step
      ct_MCStep = 5;     // Move Columns Step

      ct_ClearA  = 7;    // Windows Clear Attribute
      ct_ClearB  = CLEARB_DEFAULT;  // Windows Clear Char
      ct_ShadowA = -1;   // Windows Shadow Attribute

      ct_NCur = -1;
      ct_WMax = 0;
      ct_SMax = 0;
      ct_Wind = ( HB_CT_WND** ) hb_xgrab( sizeof( HB_CT_WND* ) );
      ct_Stac = ( SHORT* ) hb_xgrab( sizeof( SHORT ) );

      hb_ctWOpen( ct_BFRow, ct_BFCol, ct_BLRow, ct_BLCol, FALSE );

      ct_Init = TRUE;
   }

}
/****************************************************************************/
/* Destroy CT3 Windows API  */
static void hb_ctExit( void )
{
   int i, nw;

   HB_TRACE(HB_TR_DEBUG, ("hb_ctExit()"));

   if( ct_Init )
   {
      ct_Init = FALSE;

/*      hb_ctWAClose();   oh1 */
      for( i = ct_SMax - 1; i >= 0; i-- )
      {
         nw = ct_Stac[ i ];
         if( nw > 0 )
            {
               hb_ctWFree( ct_Wind[ nw ] );
               ct_Wind[ nw ] = NULL;
            }
      }

      if( ct_WMax > 0 ) hb_xfree( ct_Wind[ 0 ] );

      hb_xfree( ct_Wind );
      hb_xfree( ct_Stac );

      ct_SMax = 0;
      ct_WMax = 0;
      ct_NCur = -1;
   }

}

/****************************************************************************/
/* Restore Window Screen Attribute */
static void hb_ctSARest( SHORT FRow, SHORT FCol, SHORT LRow, SHORT LCol,
                          void * uiAddr )
{
   SHORT FRow2, FCol2, LRow2, LCol2, iRow, iCol;
   ULONG uiAddr2;

   if( uiAddr == NULL || ct_CSize < 2 ) return;

   FRow2 = HB_MAX( FRow, ct_BFRow );
   FCol2 = HB_MAX( FCol, ct_BFCol );
   LRow2 = HB_MIN( LRow, ct_BLRow );
   LCol2 = HB_MIN( LCol, ct_BLCol );

   if( LRow2 >= FRow2 && LCol2 >= FCol2 )
   {
      for( iRow = FRow2; iRow <= LRow2; iRow++ )
      {
         for( iCol = FCol2; iCol <= LCol2; iCol++ )
         {
            uiAddr2 = ( ULONG ) uiAddr +
                      ct_CSize * ( ( iRow - FRow ) * ( LCol - FCol + 1 )
                                + iCol - FCol ) + 1;

            hb_gt_SetAttribute( iRow, iCol, iRow, iCol,
                                * ( ( BYTE* ) uiAddr2 ) );
         }
      }
   }
}
/****************************************************************************/
/* Save Window Screen Area */
static void hb_ctSCSave( SHORT FRow, SHORT FCol, SHORT LRow, SHORT LCol,
                          void ** uiAddr )
{
   SHORT FRow2, FCol2, LRow2, LCol2, iRow;
   ULONG uiAddr2;

   if( ct_CSize < 2 ) return;

   if( *uiAddr == NULL )
   {
      *uiAddr = ( void * ) hb_xgrab( ct_CSize * ( LRow - FRow + 1 ) *
                                    ( LCol - FCol + 1 ) );
   }

   if( FRow >= ct_BFRow && FCol >= ct_BFCol &&
       LRow <= ct_BLRow && LCol <= ct_BLCol)
   {
      hb_gt_GetText( FRow, FCol, LRow, LCol, ( BYTE* ) *uiAddr );
   }
   else
   {
      FRow2 = HB_MAX( FRow, ct_BFRow );
      FCol2 = HB_MAX( FCol, ct_BFCol );
      LRow2 = HB_MIN( LRow, ct_BLRow );
      LCol2 = HB_MIN( LCol, ct_BLCol );

      if( LRow2 >= FRow2 && LCol2 >= FCol2 )
      {
         for( iRow = FRow2; iRow <= LRow2; iRow++ )
         {
            uiAddr2 = ( ULONG ) *uiAddr +
                      ct_CSize * ( ( iRow - FRow ) * ( LCol - FCol + 1 )
                                   + FCol2 - FCol );

            hb_gt_GetText( iRow, FCol2, iRow, LCol2, ( BYTE* ) uiAddr2 );
         }
      }
   }
}
/****************************************************************************/
/* Restore Window Screen Area */
static void hb_ctSCRest( SHORT FRow, SHORT FCol, SHORT LRow, SHORT LCol,
                          void * uiAddr )
{
   SHORT FRow2, FCol2, LRow2, LCol2, iRow;
   ULONG uiAddr2;

   if( uiAddr == NULL || ct_CSize < 2 ) return;

   if( FRow >= ct_BFRow && FCol >= ct_BFCol &&
       LRow <= ct_BLRow && LCol <= ct_BLCol)
   {
      hb_gt_PutText( FRow, FCol, LRow, LCol, ( BYTE* ) uiAddr );
   }
   else
   {
      FRow2 = HB_MAX( FRow, ct_BFRow );
      FCol2 = HB_MAX( FCol, ct_BFCol );
      LRow2 = HB_MIN( LRow, ct_BLRow );
      LCol2 = HB_MIN( LCol, ct_BLCol );

      if( LRow2 >= FRow2 && LCol2 >= FCol2 )
      {
         for( iRow = FRow2; iRow <= LRow2; iRow++ )
         {
            uiAddr2 = ( ULONG ) uiAddr +
                      ct_CSize * ( ( iRow - FRow ) * ( LCol - FCol + 1 )
                                   + FCol2 - FCol );

            hb_gt_PutText( iRow, FCol2, iRow, LCol2, ( BYTE* ) uiAddr2 );
         }
      }
   }

}
/****************************************************************************/
SHORT HB_EXPORT hb_ctShadow( SHORT iTop, SHORT iLeft, SHORT iBottom,
                                  SHORT iRight, BYTE byAttr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctShadow(%d, %d, %d, %d, %d)", iTop, iLeft, iBottom, iRight, (int) byAttr));

   if( iBottom < ct_BFRow || iRight < ct_BFCol || ct_CSize < 2 ) return 0;

   iLeft += 2;
   ++iBottom;

   /* Draw the bottom edge */

   if( iBottom <= ct_BLRow && iBottom >= ct_BFRow &&
       iLeft   <= ct_BLCol && iRight  >= ct_BFCol )
   {
      hb_gt_SetAttribute( iBottom, HB_MAX( iLeft, ct_BFCol ),
                          iBottom, HB_MIN( iRight, ct_BLCol ),
                          byAttr );
   }

   ++iRight;
   ++iTop;

   /* Draw the right edge */

   if( iTop   <= ct_BLRow && iBottom    >= ct_BFRow &&
       iRight <= ct_BLCol && iRight + 1 >= ct_BFCol )
   {
      hb_gt_SetAttribute( HB_MAX( iTop, ct_BFRow ),
                          HB_MAX( iRight, ct_BFCol ),
                          HB_MIN( iBottom, ct_BLRow ),
                          HB_MIN( iRight + 1, ct_BLCol ),
                          byAttr );
   }

   return 0;
}

/****************************************************************************/
/* Save Window Background */
static void hb_ctWBSave( HB_CT_WND * wnd )
{
   hb_ctSCSave( wnd->WFRow, wnd->WFCol, wnd->WLRow, wnd->WLCol, &(wnd->BufWB) );

   if( wnd->NCur > 0 && wnd->ShadowA != -1 )
   {
      /* Bottom Shadow */

      hb_ctSCSave( wnd->WLRow + 1, wnd->WFCol + 2,
                  wnd->WLRow + 1, wnd->WLCol + 2, &(wnd->BufSB) );

      /* Right Shadow */

      hb_ctSCSave( wnd->WFRow + 1, wnd->WLCol + 1,
                  wnd->WLRow,     wnd->WLCol + 2, &(wnd->BufSR) );
   }
}
/****************************************************************************/
/* Restore Window Background */
static void hb_ctWBRest( HB_CT_WND * wnd )
{
   hb_ctSCRest( wnd->WFRow, wnd->WFCol, wnd->WLRow, wnd->WLCol, wnd->BufWB );

   if( wnd->NCur > 0 && wnd->ShadowA != -1 )
   {
      /* Bottom Shadow */

      hb_ctSARest( wnd->WLRow + 1, wnd->WFCol + 2,
                  wnd->WLRow + 1, wnd->WLCol + 2, wnd->BufSB );

      /* Right Shadow */

      hb_ctSARest( wnd->WFRow + 1, wnd->WLCol + 1,
                  wnd->WLRow,     wnd->WLCol + 2, wnd->BufSR );
   }
}
/****************************************************************************/
/* Save Window Foreground */
static void hb_ctWFSave( HB_CT_WND * wnd )
{
   hb_ctSCSave( wnd->WFRow, wnd->WFCol, wnd->WLRow, wnd->WLCol, &(wnd->BufWF) );
}
/****************************************************************************/
/* Restore Window Foreground */
static void hb_ctWFRest( HB_CT_WND * wnd )
{
   hb_ctSCRest( wnd->WFRow, wnd->WFCol, wnd->WLRow, wnd->WLCol, wnd->BufWF );
}
/****************************************************************************/
/* Display Window Shadow */
static void hb_ctWSDisp( HB_CT_WND * wnd )
{
   if( wnd->NCur > 0 && wnd->ShadowA != -1 )
   {
      hb_ctShadow( wnd->WFRow, wnd->WFCol, wnd->WLRow, wnd->WLCol,
                   ( BYTE ) wnd->ShadowA );
   }
}
/****************************************************************************/
/* Set the default attribute for screen/window clear */
SHORT HB_EXPORT hb_ctSetClearA( SHORT nClearA )
{
   SHORT pClearA;

   pClearA = ct_ClearA;
   if( nClearA >= 0 ) ct_ClearA = nClearA;

   return pClearA;
}
/****************************************************************************/
/* Get the default attribute for screen/window clear */
SHORT HB_EXPORT hb_ctGetClearA( void )
{
   return ct_ClearA;
}
/****************************************************************************/
/* Get the default character for screen/window clear */
SHORT HB_EXPORT hb_ctGetClearB( void )
{
   return ct_ClearB;
}
/****************************************************************************/
/* Set the default character for screen/window clear */
SHORT HB_EXPORT hb_ctSetClearB( SHORT nClearB )
{
   SHORT pClearB = ct_ClearB;

   if( nClearB >= 0 && nClearB <= 255 ) ct_ClearB = nClearB;
   else if( nClearB == -1 )             ct_ClearB = CLEARB_DEFAULT;

   return pClearB;
}
/****************************************************************************/
/* Move the cursor to a new position */
SHORT HB_EXPORT hb_ctSetPos( SHORT iRow, SHORT iCol )
{
   if( s_Width > 0 && s_Height > 0 )
   {
      SHORT     iRC = FALSE;

      /* Clipper 5.3b compatible (oh1) */

      if( iRow < ct_WFRow - ct_UFRow )
      {
         iRow = ct_WFRow - ct_UFRow;
         iRC = TRUE;
      }

      while( iCol < 0 )
      {
         if( iRow > ct_WFRow - ct_UFRow) iRow--;
         iCol += ct_ULCol - ct_UFCol + 1;
         iRC = TRUE;
      }

      if( iRow > ct_ULRow - ct_UFRow + 1 &&
         ( iRow != s_iRow - ct_UFRow || iRC ||
           iRow > ct_WLRow - ct_WFRow ||
           iCol == ct_ULCol - ct_UFCol ) )
      {
         iRow = ct_ULRow - ct_UFRow + 1;
         iRC = TRUE;
      }

      if( iCol > ct_ULCol - ct_UFCol + 1 &&
          ( iCol != s_iCol - ct_UFCol || iRC ||
            iCol > ct_WLCol - ct_WFCol ||
            iRow < 0 ||
            iRow == ct_ULRow - ct_UFRow ) )
      {
         iCol = ct_ULCol - ct_UFCol + 1;
         iRC = TRUE;
      }

      if( iRow > ct_ULRow - ct_UFRow + 1 && iRC )
      {
         iRow = ct_ULRow - ct_UFRow + 1;
      }
   }

   return hb_gtSetPos( iRow, iCol );
}
/****************************************************************************/
/* Allocates screen area for windows */
SHORT HB_EXPORT hb_ctWBoard( SHORT FRow, SHORT FCol, SHORT LRow, SHORT LCol )
{
   if( ct_WMax < 1 || ct_SMax > 1 ) return -1;

   FRow = HB_MAX( 0, HB_MIN( FRow, s_Height - 1 ) );
   FCol = HB_MAX( 0, HB_MIN( FCol, s_Width - 1  ) );
   LRow = HB_MAX( 0, HB_MIN( LRow, s_Height - 1 ) );
   LCol = HB_MAX( 0, HB_MIN( LCol, s_Width - 1  ) );

   if( LRow < FRow || LCol < FCol ) return -1;

   ct_BFRow = ct_WCur->BFRow = FRow;
   ct_BFCol = ct_WCur->BFCol = FCol;
   ct_BLRow = ct_WCur->BLRow = LRow;
   ct_BLCol = ct_WCur->BLCol = LCol;

   return 0;
}
/****************************************************************************/
/* Returns a window to the visible area, or centers it */
SHORT HB_EXPORT hb_ctWCenter( BOOL WCen )
{
   SHORT FRow, FCol;

   FRow = ct_WFRow;
   FCol = ct_WFCol;

   if( WCen )
   {                                                   // Centers a window
      FRow += ( ct_BFRow + ct_BLRow - ct_WFRow - ct_WLRow ) / 2;
      FCol += ( ct_BFCol + ct_BLCol - ct_WFCol - ct_WLCol ) / 2;
   }

/* Returns a window to the visible area */

   if( FRow < ct_BFRow )                     FRow = ct_BFRow;
   else if( FRow + ct_WNRow - 1 > ct_BLRow ) FRow = ct_BLRow - ct_WNRow + 1;

   if( FCol < ct_BFCol )                     FCol = ct_BFCol;
   else if( FCol + ct_WNCol - 1 > ct_BLCol ) FCol = ct_BLCol - ct_WNCol + 1;

   if( FRow != ct_WFRow || FCol != ct_WFCol )
      hb_ctWMove( FRow, FCol );

   return ct_NCur;
}
/****************************************************************************/
/* Close the active window */
SHORT HB_EXPORT hb_ctWClose( void )
{
   if ( ct_NCur > 0 && ct_Stac[ ct_SMax - 1 ] == ct_NCur )
   {
      hb_ctWBRest( ct_WCur );
      ct_SMax--;
      hb_ctWFree( ct_WCur );
      ct_Wind[ ct_NCur ] = ct_WCur = NULL;
      hb_ctWSelect( ct_Stac[ ct_SMax - 1 ] );
   }

   return ct_NCur;
}
/****************************************************************************/
/* Create new HB_CT_WND Table */
HB_CT_WND HB_EXPORT * hb_ctWNew( SHORT FRow, SHORT FCol, SHORT LRow,
                                 SHORT LCol )
{
 HB_CT_WND *w;

 w = ( HB_CT_WND* ) hb_xgrab( sizeof( HB_CT_WND ) );

 w->NCur               = -1;

 w->WFRow = w->UFRow   = FRow;
 w->WFCol = w->UFCol   = FCol;
 w->WLRow = w->ULRow   = LRow;
 w->WLCol = w->ULCol   = LCol;
 w->WNRow              = w->WLRow - w->WFRow + 1;
 w->WNCol              = w->WLCol - w->WFCol + 1;
 w->iRow               = 0;
 w->iCol               = 0;

 w->BufWB              = NULL;
 w->BufWF              = NULL;
 w->BufSB              = NULL;
 w->BufSR              = NULL;

 w->BFRow              = ct_BFRow;
 w->BFCol              = ct_BFCol;
 w->BLRow              = ct_BLRow;
 w->BLCol              = ct_BLCol;

 w->ShadowA            = ct_ShadowA;
 w->s_uiColorIndex     = 0;
 w->s_uiColorCount     = 0;
 w->s_pColor           = NULL;
 w->hb_gt_gobjects     = NULL;
 w->hb_gt_gobjects_end = NULL;

 return w;
}
/****************************************************************************/
/* Free HB_CT_WND Table */
void HB_EXPORT hb_ctWFree( HB_CT_WND * wnd )
{

   if( wnd->BufWB != NULL)    hb_xfree( wnd->BufWB );
   if( wnd->BufWF != NULL)    hb_xfree( wnd->BufWF );
   if( wnd->BufSB != NULL)    hb_xfree( wnd->BufSB );
   if( wnd->BufSR != NULL)    hb_xfree( wnd->BufSR );
   if( wnd->s_pColor != NULL) hb_xfree( wnd->s_pColor );
   hb_xfree( wnd );

}
/****************************************************************************/
/* Set the usable area within a window */
SHORT HB_EXPORT hb_ctWFormat( SHORT FRow, SHORT FCol, SHORT LRow, SHORT LCol )
{

   if( ct_NCur < 1 ) return ct_NCur;

   ct_WCur->iRow = s_iRow - ct_UFRow;
   ct_WCur->iCol = s_iCol - ct_UFCol;

   FRow = ct_UFRow + FRow;
   FCol = ct_UFCol + FCol;
   LRow = ct_ULRow - LRow;
   LCol = ct_ULCol - LCol;

   if( FRow < ct_WFRow ) FRow = ct_WFRow;
   if( FCol < ct_WFCol ) FCol = ct_WFCol;
   if( LRow > ct_WLRow ) LRow = ct_WLRow;
   if( LCol > ct_WLCol ) LCol = ct_WLCol;

   if( FRow <= LRow &&  FCol <= LCol )
   {
      ct_WCur->UFRow = ct_UFRow = FRow;
      ct_WCur->ULRow = ct_ULRow = LRow;
      ct_WCur->UFCol = ct_UFCol = FCol;
      ct_WCur->ULCol = ct_ULCol = LCol;

      hb_gtSetPos( ct_WCur->iRow, ct_WCur->iCol );
   }

   return ct_NCur;
}
/****************************************************************************/
/* Set the screen border overstep mode */
void HB_EXPORT hb_ctWMode( BOOL MFRow, BOOL MFCol, BOOL MLRow, BOOL MLCol )
{

   /* these casts should not be required....*/

   if( MFRow == (BOOL) FALSE || MFRow == (BOOL) TRUE ) ct_MFRow = MFRow;
   if( MFCol == (BOOL) FALSE || MFCol == (BOOL) TRUE ) ct_MFCol = MFCol;
   if( MLRow == (BOOL) FALSE || MLRow == (BOOL) TRUE ) ct_MLRow = MLRow;
   if( MLCol == (BOOL) FALSE || MLCol == (BOOL) TRUE ) ct_MLCol = MLCol;

   return;
}
/****************************************************************************/
/* Moves a window */
SHORT HB_EXPORT hb_ctWMove( SHORT FRow, SHORT FCol )
{
   if( FRow < ct_BFRow && !ct_MFRow ) FRow = ct_BFRow;
   if( FCol < ct_BFCol && !ct_MFCol ) FCol = ct_BFCol;

   if( FRow + ct_WNRow - 1 > ct_BLRow && !ct_MLRow )
      FRow = ct_BLRow - ct_WNRow + 1;

   if( FCol + ct_WNCol - 1 > ct_BLCol && !ct_MLCol )
      FCol = ct_BLCol - ct_WNCol + 1;

   if( FRow > ct_BLRow + 1 )        FRow = ct_BLRow + 1;
   if( FCol > ct_BLCol + 1 )        FCol = ct_BLCol + 1;
   if( FRow + ct_WNRow < ct_BFRow ) FRow = ct_BFRow - ct_WNRow;
   if( FCol + ct_WNCol < ct_BFCol ) FCol = ct_BFCol - ct_WNCol;

   if( FRow != ct_WFRow || FCol != ct_WFCol )
   {
      hb_gtDispBegin();

      ct_WCur->iRow = s_iRow - ct_UFRow;
      ct_WCur->iCol = s_iCol - ct_UFCol;

      hb_ctWFSave( ct_WCur );
      hb_ctWBRest( ct_WCur );

      ct_WCur->UFRow = ct_UFRow = ct_UFRow + FRow - ct_WFRow;
      ct_WCur->UFCol = ct_UFCol = ct_UFCol + FCol - ct_WFCol;
      ct_WCur->ULRow = ct_ULRow = ct_ULRow + FRow - ct_WFRow;
      ct_WCur->ULCol = ct_ULCol = ct_ULCol + FCol - ct_WFCol;

      ct_WCur->WFRow = ct_WFRow = FRow;
      ct_WCur->WFCol = ct_WFCol = FCol;
      ct_WCur->WLRow = ct_WLRow = FRow + ct_WNRow - 1;
      ct_WCur->WLCol = ct_WLCol = FCol + ct_WNCol - 1;

      hb_ctWBSave( ct_WCur );
      hb_ctWFRest( ct_WCur );
      hb_ctWSDisp( ct_WCur );

      hb_gtSetPos( ct_WCur->iRow, ct_WCur->iCol );

      hb_gtDispEnd();
   }

   return ct_NCur;
}
/****************************************************************************/
/* Opens a new window */
SHORT HB_EXPORT hb_ctWOpen( SHORT FRow, SHORT FCol, SHORT LRow, SHORT LCol,
                            BOOL lDel )
{
   HB_CT_WND * wnd;
   SHORT       i, j;

   if( FRow < ct_BFRow )
   {
      i = ct_BFRow - FRow;
      FRow += i; LRow += i;
   }

   if( FCol < ct_BFCol )
   {
      i = ct_BFCol - FCol;
      FCol += i; LCol += i;
   }

   if( LRow > ct_BLRow )
   {
      i = LRow - ct_BLRow;
      FRow -= i; LRow -= i;
   }

   if( LCol > ct_BLCol )
   {
      i = LCol - ct_BLCol;
      FCol -= i; LCol -= i;
   }

   FRow = HB_MAX( ct_BFRow, HB_MIN( FRow, ct_BLRow ) );
   FCol = HB_MAX( ct_BFCol, HB_MIN( FCol, ct_BLCol ) );
   if( LRow > ct_BLRow ) LRow = ct_BLRow;
   if( LCol > ct_BLCol ) LCol = ct_BLCol;

   if( LRow < FRow || LCol < FCol ) return -1;

   wnd = hb_ctWNew( FRow, FCol, LRow, LCol );

   if( ct_WMax == 0 )
   {
      wnd->s_pColor = s_pColor;
      wnd->iRow = s_iRow;
      wnd->iCol = s_iCol;
   }
   else
   {
      wnd->s_pColor = ( int* ) hb_xgrab( s_uiColorCount * sizeof( int ) );
      memcpy( wnd->s_pColor, s_pColor, s_uiColorCount );
   }
   wnd->s_uiColorCount  = s_uiColorCount;
   wnd->s_uiCursorStyle = s_uiCursorStyle;
   wnd->s_uiColorIndex  = s_uiColorIndex;

   for ( i = 0; i < ct_WMax; i++ )
      if ( ct_Wind[ i ] == NULL ) break;
   if ( i >= ct_WMax)
   {
      ct_Wind = ( HB_CT_WND** ) hb_xrealloc( ct_Wind,
                                    ( ct_WMax + 1 ) * sizeof( HB_CT_WND* ) );
      i = ct_WMax;
      ct_WMax++;
   }
   wnd->NCur    = i;
   ct_Wind[ i ] = wnd;

   hb_ctWSelect( i );

   if( lDel || ct_WCur->ShadowA != -1 )
   {
      for( j = 0; j < ct_WCur->WNRow; j++ )
         hb_gtRepChar( j, 0, ( BYTE ) ct_ClearB, ct_WCur->WNCol );

      hb_gtSetPos( 0, 0 );
   }

   hb_ctWSDisp( ct_WCur );

   return ct_NCur;
}
/****************************************************************************/
/* Activate Window */
SHORT HB_EXPORT hb_ctWSelect( SHORT iwnd )
{
   SHORT i, j;

   if( iwnd != ct_NCur && iwnd >= 0 && iwnd < ct_WMax &&
       ct_Wind[ iwnd ] != NULL )
   {
      hb_gtDispBegin();

      if( ct_WCur != NULL )
      {
         ct_WCur->iRow               = s_iRow - ct_UFRow;
         ct_WCur->iCol               = s_iCol - ct_UFCol;
         ct_WCur->ScNone             = s_ScNone;
         ct_WCur->s_uiCursorStyle    = s_uiCursorStyle;
         ct_WCur->s_uiColorIndex     = s_uiColorIndex;
         ct_WCur->s_uiColorCount     = s_uiColorCount;
         ct_WCur->s_pColor           = s_pColor;
         ct_WCur->hb_gt_gobjects     = hb_gt_gobjects;
         ct_WCur->hb_gt_gobjects_end = hb_gt_gobjects_end;

         if ( ct_NCur > 0 ) hb_ctWFSave( ct_WCur );
      }

      for( i = 0; i < ct_SMax; i++ )
         if( ct_Stac[ i ] == iwnd ) break;

      if( i >= ct_SMax )                              // New Window
      {
         ct_Stac = ( SHORT* ) hb_xrealloc( ct_Stac, ( ct_SMax + 1 ) * sizeof( SHORT ) );
         ct_Stac[ ct_SMax ] = iwnd;
         ct_SMax++;

         if( iwnd > 0 ) hb_ctWBSave( ct_Wind[ iwnd ] );
      }
      else if ( iwnd == 0 )                           // Window_X -> Window_0
      {
/*         Removed: oh1-2005-12-08 ( Window-0 not moved )
//         for( j = i + 1; j < ct_SMax; j++ ) ct_Stac[ j - 1 ] = ct_Stac[ j ];
//         ct_Stac[ ct_SMax - 1 ] = iwnd;
*/
      }
      else if ( ct_NCur == 0 )                        // Window_0 -> Window_X
      {
         for( j = ct_SMax - 1; j >= 0; j-- )
            if( ct_Stac[ j ] > 0 )
            {
               hb_ctWFSave( ct_Wind[ ct_Stac[ j ] ] );
               hb_ctWBRest( ct_Wind[ ct_Stac[ j ] ] );
            }

         for( j = i + 1; j < ct_SMax; j++ ) ct_Stac[ j - 1 ] = ct_Stac[ j ];
         ct_Stac[ ct_SMax - 1 ] = iwnd;

         for( j = 0; j < ct_SMax; j++ )
         {
            if( ct_Stac[ j ] > 0 )
            {
               hb_ctWBSave( ct_Wind[ ct_Stac[ j ] ] );
               hb_ctWFRest( ct_Wind[ ct_Stac[ j ] ] );
               hb_ctWSDisp( ct_Wind[ ct_Stac[ j ] ] );
            }
         }
      }
      else if ( ct_NCur != iwnd )                     // Window_X -> Window_Y
      {
         for( j = ct_SMax - 1; j >= i; j-- )
         {
            if( ct_Stac[ j ] > 0 ) hb_ctWBRest( ct_Wind[ ct_Stac[ j ] ] );
         }

         for( j = i + 1; j < ct_SMax; j++ ) ct_Stac[ j - 1 ] = ct_Stac[ j ];
         ct_Stac[ ct_SMax - 1 ] = iwnd;

         for( j = i; j < ct_SMax; j++ )
         {
            if( ct_Stac[ j ] > 0 )
            {
               hb_ctWBSave( ct_Wind[ ct_Stac[ j ] ] );
               hb_ctWFRest( ct_Wind[ ct_Stac[ j ] ] );
               hb_ctWSDisp( ct_Wind[ ct_Stac[ j ] ] );
            }
         }
      }

      ct_NCur  = iwnd;
      ct_WCur  = ct_Wind[ ct_NCur ];

      ct_WFRow = ct_WCur->WFRow;
      ct_WFCol = ct_WCur->WFCol;
      ct_WLRow = ct_WCur->WLRow;
      ct_WLCol = ct_WCur->WLCol;

      ct_UFRow = ct_WCur->UFRow;
      ct_UFCol = ct_WCur->UFCol;
      ct_ULRow = ct_WCur->ULRow;
      ct_ULCol = ct_WCur->ULCol;

      ct_WNRow = ct_WCur->WNRow;
      ct_WNCol = ct_WCur->WNCol;

      s_uiCursorStyle    = ct_WCur->s_uiCursorStyle;
      s_uiColorIndex     = ct_WCur->s_uiColorIndex;
      s_uiColorCount     = ct_WCur->s_uiColorCount;
      s_pColor           = ct_WCur->s_pColor;
      hb_gt_gobjects     = ct_WCur->hb_gt_gobjects;
      hb_gt_gobjects_end = ct_WCur->hb_gt_gobjects_end;

      hb_gtSetPos( ct_WCur->iRow, ct_WCur->iCol );
      hb_gtDispEnd();
   }

   return ct_NCur;
}
/****************************************************************************/
/* Get the highest row number for screen or window */
SHORT HB_EXPORT hb_ctMaxRow( BOOL lMode )
{
   if ( lMode)
      return s_Height > 0 ? s_Height - 1 : 0;
   else
      return ct_ULRow - ct_UFRow;
}
/****************************************************************************/
/* Get the highest column number for screen or window */
SHORT HB_EXPORT hb_ctMaxCol( BOOL lMode )
{
   if ( lMode)
      return s_Width > 0 ? s_Width - 1 : 0;
   else
      return ct_ULCol - ct_UFCol;
}
/****************************************************************************/
/* Set current color */
int HB_EXPORT hb_ctSetCurColor( int iColor )
{
    int pColor = s_pColor[ s_uiColorIndex ];

    s_pColor[ s_uiColorIndex ] = iColor;

    return pColor;
}
/****************************************************************************/
/* Get the current windows info */
HB_CT_WND HB_EXPORT * hb_ctWCurrent( void )
{
   ct_WCur->iRow   = s_iRow - ct_UFRow;
   ct_WCur->iCol   = s_iCol - ct_UFCol;
   ct_WCur->ScNone = s_ScNone;

   return ct_WCur;
}
/****************************************************************************/
/* Close all windows */
SHORT HB_EXPORT hb_ctWAClose( void )
{
   int i, nw;

   if( ct_SMax <= 1 ) return -1;

   if( ct_NCur == 0 )
   {
      ct_WCur->iRow               = s_iRow - ct_UFRow;
      ct_WCur->iCol               = s_iCol - ct_UFCol;
      ct_WCur->ScNone             = s_ScNone;

      ct_WCur->s_uiCursorStyle    = s_uiCursorStyle;
      ct_WCur->s_uiColorIndex     = s_uiColorIndex;
      ct_WCur->s_uiColorCount     = s_uiColorCount;
      ct_WCur->s_pColor           = s_pColor;
      ct_WCur->hb_gt_gobjects     = hb_gt_gobjects;
      ct_WCur->hb_gt_gobjects_end = hb_gt_gobjects_end;
   }

   for( i = ct_SMax - 1; i >= 0; i-- )
   {
      nw = ct_Stac[ i ];
      if( nw > 0 )
         {
            hb_ctWBRest( ct_Wind[ nw ] );
            hb_ctWFree( ct_Wind[ nw ] );
            ct_Wind[ nw ] = NULL;
         }
   }

   ct_WCur           = NULL;
   ct_NCur           = -1;
   ct_Stac[ 0 ]      = 0;
   ct_WMax = ct_SMax = 1;

   hb_ctWSelect( 0 );

   return 0;
}
/****************************************************************************/
/* Get the highest windows handle */
SHORT HB_EXPORT hb_ctWNum( void )
{
   SHORT wnum;

   for( wnum = ct_WMax - 1; wnum >= 0; wnum-- )
      if( ct_Wind[ wnum ] != NULL ) break;

   return wnum;
}
/****************************************************************************/
/* Set the window shadow color */
SHORT HB_EXPORT hb_ctWSetShadow( SHORT nAttr )
{
   SHORT pAttr = ct_ShadowA;

   if( nAttr >= 0 || nAttr == -1 ) ct_ShadowA = nAttr;

   return pAttr;
}
/****************************************************************************/
/* Get the array of windows handle ( New ) */
void HB_EXPORT hb_ctWStack( SHORT ** Stac, SHORT * SMax )
{
   *Stac = ct_Stac;
   *SMax = ct_SMax;
}
/****************************************************************************/
/* Set the step width of interactive window movement */
SHORT HB_EXPORT hb_ctWStep( SHORT nRow, SHORT nCol )
{
   if( ct_SMax > 1 ) return -1;

   if( nRow < 1 )  nRow = 1;
   if( nRow > 6 )  nRow = 6;
   if( nCol < 1 )  nCol = 1;
   if( nCol > 20 ) nCol = 20;

   ct_MRStep = nRow;
   ct_MCStep = nCol;

   return 0;
}
/****************************************************************************/
/* Set the interactive movement mode */
BOOL HB_EXPORT hb_ctWSetMove( BOOL Mode )
{
   BOOL p_MMode = ct_MMode;

   if( Mode == (BOOL) FALSE || Mode == ( BOOL ) TRUE ) ct_MMode = Mode;

   return p_MMode;
}
/****************************************************************************/
/* Get the mouse column number for screen/window */
int HB_EXPORT hb_ctMouseRow( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctmouseRow()"));

   return hb_mouseRow() - ct_UFRow;
}
/****************************************************************************/
/* Get the mouse row number for screen/window */
int HB_EXPORT hb_ctMouseCol( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_ctmouseCol()"));

   return hb_mouseCol() - ct_UFCol;
}
/****************************************************************************/
/****************************************************************************/
/* Get the mouse column number for screen/window ( New ) */
HB_FUNC( WMCOL )
{
   hb_retni( hb_ctMouseCol() );
}
/****************************************************************************/
/* Get the mouse row number for screen/window ( New ) */
HB_FUNC( WMROW )
{
   hb_retni( hb_ctMouseRow() );
}
/****************************************************************************/
/* Get the windows info ( New ) */
/*  Return array(8): 1-ct_WFRow, 2-ct_WFCol, 3-ct_WLRow, 4-ct_WLCol,
                     5-ct_UFRow, 6-ct_UFCol, 7-ct_ULRow, 8-ct_ULCol
*/
HB_FUNC( WINFO )
{
   SHORT     iWnd;
   PHB_ITEM  pInfo, pN;
   HB_CT_WND * Wnd;

   if( hb_pcount() > 0 && ISNUM( 1 ) )
   {
      iWnd = hb_parni( 1 );
   }
   else
   {
      iWnd = ct_NCur;
   }

   pInfo = hb_itemArrayNew( 8 );

   if( iWnd >= 0 && iWnd < ct_WMax && ct_Wind[ iWnd ] != NULL )
   {
      Wnd  = ct_Wind[ iWnd ];

      pN = hb_itemPutNL( NULL, Wnd->WFRow );
      hb_arraySet( pInfo, 1, pN );
      hb_itemRelease( pN );

      pN = hb_itemPutNL( NULL, Wnd->WFCol );
      hb_arraySet( pInfo, 2, pN );
      hb_itemRelease( pN );

      pN = hb_itemPutNL( NULL, Wnd->WLRow );
      hb_arraySet( pInfo, 3, pN );
      hb_itemRelease( pN );

      pN = hb_itemPutNL( NULL, Wnd->WLCol );
      hb_arraySet( pInfo, 4, pN );
      hb_itemRelease( pN );

      pN = hb_itemPutNL( NULL, Wnd->UFRow );
      hb_arraySet( pInfo, 5, pN );
      hb_itemRelease( pN );

      pN = hb_itemPutNL( NULL, Wnd->UFCol );
      hb_arraySet( pInfo, 6, pN );
      hb_itemRelease( pN );

      pN = hb_itemPutNL( NULL, Wnd->ULRow );
      hb_arraySet( pInfo, 7, pN );
      hb_itemRelease( pN );

      pN = hb_itemPutNL( NULL, Wnd->ULCol );
      hb_arraySet( pInfo, 8, pN );
      hb_itemRelease( pN );
   }
   else
   {
     pN = hb_itemPutNL( NULL, 0 );
     hb_arraySet( pInfo, 1, pN );
     hb_arraySet( pInfo, 2, pN );
     hb_arraySet( pInfo, 5, pN );
     hb_arraySet( pInfo, 6, pN );
     hb_itemRelease( pN );

     pN = hb_itemPutNL( NULL, -1 );
     hb_arraySet( pInfo, 3, pN );
     hb_arraySet( pInfo, 4, pN );
     hb_arraySet( pInfo, 7, pN );
     hb_arraySet( pInfo, 8, pN );
     hb_itemRelease( pN );
   }

   hb_itemRelease( hb_itemReturn( pInfo ) );
}
/****************************************************************************/
/* Set the mouse cursor, row and column for windows (New) */
HB_FUNC( WSETMOUSE )
{
   hb_retl( hb_mouseGetCursor() );

   if( ISLOG( 1 ) )
      hb_mouseSetCursor( hb_parl( 1 ) );

   {
      PHB_ITEM pRow = hb_param( 2, HB_IT_NUMERIC );
      PHB_ITEM pCol = hb_param( 3, HB_IT_NUMERIC );

      if( pRow || pCol )
      {
         hb_mouseSetPos( pRow ? hb_itemGetNI( pRow ) + ct_UFRow : hb_mouseRow() ,
                         pCol ? hb_itemGetNI( pCol ) + ct_UFCol : hb_mouseCol() );
      }
   }
}
/****************************************************************************/
/* Set the mouse row and column for windows (New) */
HB_FUNC( WMSETPOS )
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
      hb_mouseSetPos( hb_parni( 1 ) + ct_UFRow, hb_parni( 2 ) + ct_UFCol);
}
/****************************************************************************/
