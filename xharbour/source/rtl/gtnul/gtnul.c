/*
 * $Id: gtnul.c,v 1.17 2004/02/03 18:06:21 ronpinkas Exp $
 */

/*
 * Harbour Project source code:
 * Null and multi_GT switch video subsystem.
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
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

/* *********************************************************************** */

#ifndef HB_MULTI_GT
#  define HB_MULTI_GT
#endif
/* This definition has to be placed before #include "hbapigt.h" */
#define HB_GT_NAME	NUL

#include "hbapigt.h"
#include "hbapifs.h"

#define HB_GT_MAX_ 16
static char * s_initGT = HB_GT_DRVNAME( HB_GT_NAME );

#if defined(HB_DEFAULT_GT)
   char * s_defaultGT = HB_GT_DRVNAME( HB_DEFAULT_GT );
   HB_GT_REQUEST( HB_DEFAULT_GT );
#elif defined(HB_GT_LIB)
   char * s_defaultGT = HB_GT_DRVNAME( HB_GT_LIB );
   HB_GT_REQUEST( HB_GT_LIB );
#elif defined(HB_OS_LINUX)
   char * s_defaultGT = "crs";
#elif defined(HB_OS_WIN_32)
   char * s_defaultGT = "win";
#elif defined(HB_OS_DOS)
   char * s_defaultGT = "dos";
#elif defined(HB_OS_OS2)
   char * s_defaultGT = "os2";
#else
   char * s_defaultGT = "std";
#endif

static PHB_GT_INIT s_gtInit[ HB_GT_MAX_ ];
static HB_GT_FUNCS GT_FUNCS;


static USHORT s_uiDispCount;
static USHORT s_uiMaxCol = 80;
static USHORT s_uiMaxRow = 24;
static int s_iKeyRet = 13;
static int s_iStdIn, s_iStdOut, s_iStdErr;

static char *s_clipboard = NULL;
static int s_clipsize = 0;

/* ********************************************************************** */

static int hb_gtFindPos( char * pszID )
{
   int iPos;

   if( pszID )
   {
      if( hb_stricmp( pszID, "null" ) == 0 )
         pszID = s_initGT;

      for( iPos = 0; iPos < HB_GT_MAX_ && s_gtInit[ iPos ]; iPos++ )
         if( hb_stricmp( s_gtInit[ iPos ]->id, pszID ) == 0 ||
             ( hb_strnicmp(pszID, "gt", 2) == 0 &&
               hb_stricmp( s_gtInit[ iPos ]->id, pszID + 2 ) == 0 ) )
            return iPos;
   }
   return -1;
}

static int hb_gtFindNoNul()
{
   int iPos;

   for( iPos = 0; iPos < HB_GT_MAX_ && s_gtInit[ iPos ]; iPos++ )
      if( hb_stricmp( s_gtInit[ iPos ]->id, s_initGT ) != 0 )
         return iPos;
   return -1;
}

static int hb_gtFindPosFree( char * pszID )
{
   int iPos = -1;

   if( pszID )
   {
      iPos = hb_gtFindPos( pszID );
      hb_xfree( pszID );
   }
   return iPos;
}

BOOL HB_EXPORT hb_gtRegister( PHB_GT_INIT gtInit )
{
   int iPos;

   if( gtInit )
      if ( hb_gtFindPos( gtInit->id ) == -1 )
         for( iPos = 0; iPos < HB_GT_MAX_; iPos++ )
            if( !s_gtInit[ iPos ] )
            {
               s_gtInit[ iPos ] = gtInit;
               return TRUE;
            }

   return FALSE;
}


/* ********************************************************************** */

void HB_GT_FUNC(gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_Init()"));

   s_uiDispCount = 0;

   s_iStdIn  = iFilenoStdin;
   s_iStdOut = iFilenoStdout;
   s_iStdErr = iFilenoStderr;
}

void HB_GT_FUNC(gt_Exit( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_Exit()"));
   if ( s_clipboard != NULL )
   {
      hb_xfree( s_clipboard );
   }
}

USHORT HB_GT_FUNC(gt_GetScreenWidth( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_GetScreenWidth()"));

   return s_uiMaxCol;
}

USHORT HB_GT_FUNC(gt_GetScreenHeight( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_GetScreenHeight()"));

   return s_uiMaxRow;
}

SHORT HB_GT_FUNC(gt_Col( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_Col()"));

   return 0;
}

SHORT HB_GT_FUNC(gt_Row())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_Row()"));

   return 0;
}

void HB_GT_FUNC(gt_SetPos( SHORT iRow, SHORT iCol, SHORT iMethod ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_SetPos(%hd, %hd, %hd)", iRow, iCol, iMethod));

   HB_SYMBOL_UNUSED( iRow );
   HB_SYMBOL_UNUSED( iCol );
   HB_SYMBOL_UNUSED( iMethod );
}

BOOL HB_GT_FUNC(gt_AdjustPos( BYTE * pStr, ULONG ulLen ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

   HB_SYMBOL_UNUSED( pStr );
   HB_SYMBOL_UNUSED( ulLen );

   return FALSE;
}

USHORT HB_GT_FUNC(gt_GetCursorStyle( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_GetCursorStyle()"));

   return SC_NONE;
}

void HB_GT_FUNC(gt_SetCursorStyle( USHORT uiCursorStyle ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_SetCursorStyle(%hu)", uiCursorStyle));

   HB_SYMBOL_UNUSED( uiCursorStyle );
}

BOOL HB_GT_FUNC(gt_IsColor( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_IsColor()"));

   return FALSE;
}

void HB_GT_FUNC(gt_DispBegin( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_DispBegin()"));

   ++s_uiDispCount;
}

void HB_GT_FUNC(gt_DispEnd())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_DispEnd()"));

   if ( s_uiDispCount )
      --s_uiDispCount;
}

USHORT HB_GT_FUNC(gt_DispCount())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_DispCount()"));

   return s_uiDispCount;
}


void HB_GT_FUNC(gt_Puts( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE * pbyStr, ULONG ulLen ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_Puts(%hu, %hu, %d, %p, %lu)", uiRow, uiCol, (int) byAttr, pbyStr, ulLen));

   HB_SYMBOL_UNUSED( uiRow );
   HB_SYMBOL_UNUSED( uiCol );
   HB_SYMBOL_UNUSED( byAttr );
   HB_SYMBOL_UNUSED( pbyStr );
   HB_SYMBOL_UNUSED( ulLen );
}

int HB_GT_FUNC(gt_RectSize( USHORT rows, USHORT cols ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_RectSize(%hu, %hu)", rows, cols));

   return rows * cols * 2;
}

void HB_GT_FUNC(gt_GetText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyDst ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_GetText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbyDst));

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( pbyDst );
}

void HB_GT_FUNC(gt_PutText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbySrc ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_PutText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbySrc));

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( pbySrc );
}

void HB_GT_FUNC(gt_SetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( byAttr );
}

void HB_GT_FUNC(gt_Scroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr, SHORT iRows, SHORT iCols ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr, iRows, iCols));

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( byAttr );
   HB_SYMBOL_UNUSED( iRows );
   HB_SYMBOL_UNUSED( iCols );
}

void HB_GT_FUNC(gt_Replicate( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar, ULONG nLength ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_Replicate(%hu, %hu, %i, %i, %lu)", uiRow, uiCol, byAttr, byChar, nLength));

   HB_SYMBOL_UNUSED( uiRow );
   HB_SYMBOL_UNUSED( uiCol );
   HB_SYMBOL_UNUSED( byAttr );
   HB_SYMBOL_UNUSED( byChar );
   HB_SYMBOL_UNUSED( nLength );
}

USHORT HB_GT_FUNC(gt_Box( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
                          BYTE * pbyFrame, BYTE byAttr ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_box(%i, %i, %i, %i, %s, %hu)", Top, Left, Bottom, Right, pbyFrame, byAttr));

   HB_SYMBOL_UNUSED( Top );
   HB_SYMBOL_UNUSED( Left );
   HB_SYMBOL_UNUSED( Bottom );
   HB_SYMBOL_UNUSED( Right );
   HB_SYMBOL_UNUSED( pbyFrame );
   HB_SYMBOL_UNUSED( byAttr );

   return 0;
}

USHORT HB_GT_FUNC(gt_BoxD( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_boxD(%i, %i, %i, %i, %s, %hu)", Top, Left, Bottom, Right, pbyFrame, byAttr));

   return hb_gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr );
}

USHORT HB_GT_FUNC(gt_BoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_boxS(%i, %i, %i, %i, %s, %hu)", Top, Left, Bottom, Right, pbyFrame, byAttr));

   return hb_gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr );
}

USHORT HB_GT_FUNC(gt_HorizLine( SHORT Row, SHORT Left, SHORT Right, BYTE byChar, BYTE byAttr ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_HorizLine(%i, %i, %i, %hu, %hu)", Row, Left, Right, byChar, byAttr));

   if( Left < Right )
      hb_gt_Replicate( Row, Left, byAttr, byChar, Right - Left + 1 );
   else
      hb_gt_Replicate( Row, Right, byAttr, byChar, Left - Right + 1 );

   return 0;
}

USHORT HB_GT_FUNC(gt_VertLine( SHORT Col, SHORT Top, SHORT Bottom, BYTE byChar, BYTE byAttr ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_VertLine(%i, %i, %i, %hu, %hu)", Col, Top, Bottom, byChar, byAttr));

   HB_SYMBOL_UNUSED( Col );
   HB_SYMBOL_UNUSED( Top );
   HB_SYMBOL_UNUSED( Bottom );
   HB_SYMBOL_UNUSED( byChar );
   HB_SYMBOL_UNUSED( byAttr );

   return 0;
}

BOOL HB_GT_FUNC(gt_PreExt())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_PreExt()"));

   return TRUE;
}

BOOL HB_GT_FUNC(gt_PostExt())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_PostExt()"));

   return TRUE;
}

BOOL HB_GT_FUNC(gt_Suspend())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_Suspend()"));

   return TRUE;
}

BOOL HB_GT_FUNC(gt_Resume())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_Resume()"));

   return TRUE;
}

void HB_GT_FUNC(gt_OutStd( BYTE * pbyStr, ULONG ulLen ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_OutStd(%s, %hu)", pbyStr, ulLen));

   hb_fsWriteLarge( s_iStdOut, ( BYTE * ) pbyStr, ulLen );
}

void HB_GT_FUNC(gt_OutErr( BYTE * pbyStr, ULONG ulLen ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_OutErr(%s, %hu)", pbyStr, ulLen));

   hb_fsWriteLarge( s_iStdErr, ( BYTE * ) pbyStr, ulLen );
}


char * HB_GT_FUNC(gt_Version( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_Version()"));

   return "Harbour Terminal: NULL";
}


/* ********************************************************************** */

BOOL HB_GT_FUNC(gt_SetMode( USHORT uiRows, USHORT uiCols ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_SetMode(%hu, %hu)", uiRows, uiCols));

   HB_SYMBOL_UNUSED( uiRows );
   HB_SYMBOL_UNUSED( uiCols );

   return FALSE;
}

BOOL HB_GT_FUNC(gt_GetBlink())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_GetBlink()"));

   return FALSE;
}

void HB_GT_FUNC(gt_SetBlink( BOOL bBlink ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_SetBlink(%d)", (int) bBlink));

   HB_SYMBOL_UNUSED( bBlink );
}

void HB_GT_FUNC(gt_Tone( double dFrequency, double dDuration ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_Tone(%lf, %lf)", dFrequency, dDuration));

   HB_SYMBOL_UNUSED( dFrequency );

   hb_idleSleep( dDuration / 18.2 );
}

/* ********************************************************************** */

int HB_GT_FUNC(gt_ExtendedKeySupport())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_ExtendedKeySupport()"));

   return 0;
}

int HB_GT_FUNC(gt_ReadKey( HB_inkey_enum eventmask ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_ReadKey(%d)", (int) eventmask));

   HB_SYMBOL_UNUSED( eventmask );

   return s_iKeyRet;
}

/* ********************************************************************** */

void HB_GT_FUNC(gt_SetDispCP(char * pszTermCDP, char * pszHostCDP, BOOL bBox))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_SetDispCP(%p, %p, %d)", pszTermCDP, pszHostCDP, (int) bBox));

   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );
   HB_SYMBOL_UNUSED( bBox );
}

/* ********************************************************************** */

void HB_GT_FUNC(gt_SetKeyCP(char * pszTermCDP, char * pszHostCDP))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_gt_SetDispCP(%p, %p)", pszTermCDP, pszHostCDP));

   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );
}

/* ********************************************************************** */

void HB_GT_FUNC(mouse_Init( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_mouse_Init()"));
}

void HB_GT_FUNC(mouse_Exit( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_mouse_Exit()"));
}

BOOL HB_GT_FUNC(mouse_IsPresent( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_mouse_IsPresent()"));

   return FALSE;
}

void HB_GT_FUNC(mouse_Show( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_mouse_Show()"));
}

void HB_GT_FUNC(mouse_Hide( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_mouse_Hide()"));
}

int HB_GT_FUNC(mouse_Col( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_mouse_Col()"));

   return 0;
}

int HB_GT_FUNC(mouse_Row( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_mouse_Row()"));

   return 0;
}

void HB_GT_FUNC(mouse_SetPos( int iRow, int iCol ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_mouse_SetPos(%i, %i)", iRow, iCol));

   HB_SYMBOL_UNUSED( iRow );
   HB_SYMBOL_UNUSED( iCol );
}

BOOL HB_GT_FUNC(mouse_IsButtonPressed( int iButton ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_mouse_IsButtonPressed(%i)", iButton));

   HB_SYMBOL_UNUSED( iButton );

   return FALSE;
}

int HB_GT_FUNC(mouse_CountButton( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_mouse_CountButton()"));

   return 0;
}

void HB_GT_FUNC(mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_mouse_SetBounds(%i, %i, %i, %i)", iTop, iLeft, iBottom, iRight));

   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( iBottom );
   HB_SYMBOL_UNUSED( iRight );
}

void HB_GT_FUNC(mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_NUL_mouse_GetBounds(%p, %p, %p, %p)", piTop, piLeft, piBottom, piRight));

   HB_SYMBOL_UNUSED( piTop );
   HB_SYMBOL_UNUSED( piLeft );
   HB_SYMBOL_UNUSED( piBottom );
   HB_SYMBOL_UNUSED( piRight );
}

/* ********************************************************************** */

void hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr )
{
   int iPos;

   iPos = hb_gtFindPos( s_initGT );
   if (iPos == -1)
   {
      char * errmsg = "\r\nInternal error : screen driver initialization failure\r\n";
      hb_fsWriteLarge( iFilenoStderr, ( BYTE *) errmsg, strlen( errmsg ) );
      exit( 20 );
   }
   s_gtInit[ iPos ]->gtInit( &GT_FUNCS );
   s_gtInit[ iPos ]->mouseInit( &GT_FUNCS );

   iPos = hb_gtFindPosFree( hb_cmdargString( "GT" ) );
   if ( iPos == -1 )
      iPos = hb_gtFindPosFree( hb_getenv( "HB_GT" ) );

   if ( iPos == -1 )
      iPos = hb_gtFindPos( s_defaultGT );

   if ( iPos == -1 )
      iPos = hb_gtFindNoNul();

   if ( iPos != -1 )
   {
      s_gtInit[ iPos ]->gtInit( &GT_FUNCS );
      s_gtInit[ iPos ]->mouseInit( &GT_FUNCS );
   }

   GT_FUNCS.Init( iFilenoStdin, iFilenoStdout, iFilenoStderr );
}

void hb_gt_Exit( void )
{
   GT_FUNCS.Exit();
}

USHORT hb_gt_GetScreenWidth( void )
{
   return GT_FUNCS.GetScreenWidth();
}

USHORT hb_gt_GetScreenHeight( void )
{
   return GT_FUNCS.GetScreenHeight();
}

SHORT hb_gt_Col( void )
{
   return GT_FUNCS.Col();
}

SHORT hb_gt_Row( void )
{
   return GT_FUNCS.Row();
}

void hb_gt_SetPos( SHORT iRow, SHORT iCol, SHORT iMethod )
{
   GT_FUNCS.SetPos( iRow, iCol, iMethod);
}

BOOL hb_gt_AdjustPos( BYTE * pStr, ULONG ulLen )
{
   return GT_FUNCS.AdjustPos( pStr, ulLen );
}

USHORT hb_gt_GetCursorStyle( void )
{
   return GT_FUNCS.GetCursorStyle();
}

void hb_gt_SetCursorStyle( USHORT uiCursorStyle )
{
   GT_FUNCS.SetCursorStyle( uiCursorStyle );
}

BOOL hb_gt_IsColor( void )
{
   return GT_FUNCS.IsColor();
}

void hb_gt_DispBegin( void )
{
   GT_FUNCS.DispBegin();
}

void hb_gt_DispEnd( void )
{
   GT_FUNCS.DispEnd();
}

USHORT hb_gt_DispCount( void )
{
   return GT_FUNCS.DispCount();
}

void hb_gt_Puts( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE * pbyStr, ULONG ulLen )
{
   GT_FUNCS.Puts( uiRow, uiCol, byAttr, pbyStr, ulLen);
}

int hb_gt_RectSize( USHORT rows, USHORT cols )
{
   return GT_FUNCS.RectSize( rows, cols );
}

void hb_gt_GetText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyDst )
{
   GT_FUNCS.GetText( uiTop, uiLeft, uiBottom, uiRight, pbyDst);
}

void hb_gt_PutText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbySrc )
{
   GT_FUNCS.PutText( uiTop, uiLeft, uiBottom, uiRight, pbySrc);
}

void hb_gt_SetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr )
{
   GT_FUNCS.SetAttribute( uiTop, uiLeft, uiBottom, uiRight, (int) byAttr);
}

void hb_gt_Scroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr, SHORT iRows, SHORT iCols )
{
   GT_FUNCS.Scroll( uiTop, uiLeft, uiBottom, uiRight, byAttr, iRows, iCols);
}

void hb_gt_Replicate( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar, ULONG nLength )
{
   GT_FUNCS.Replicate( uiRow, uiCol, byAttr, byChar, nLength);
}

USHORT hb_gt_Box( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr )
{
   return GT_FUNCS.Box( Top, Left, Bottom, Right, pbyFrame, byAttr );
}

USHORT hb_gt_BoxD( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr )
{
   return GT_FUNCS.BoxD( Top, Left, Bottom, Right, pbyFrame, byAttr );
}

USHORT hb_gt_BoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr )
{
   return GT_FUNCS.BoxS( Top, Left, Bottom, Right, pbyFrame, byAttr );
}

USHORT hb_gt_HorizLine( SHORT Row, SHORT Left, SHORT Right, BYTE byChar, BYTE byAttr )
{
   return GT_FUNCS.HorizLine( Row, Left, Right, byChar, byAttr);
}

USHORT hb_gt_VertLine( SHORT Col, SHORT Top, SHORT Bottom, BYTE byChar, BYTE byAttr )
{
   return GT_FUNCS.VertLine( Col, Top, Bottom, byChar, byAttr);
}

BOOL hb_gt_PreExt( void )
{
   return GT_FUNCS.PreExt();
}

BOOL hb_gt_PostExt( void )
{
   return GT_FUNCS.PostExt();
}

BOOL hb_gt_Suspend( void )
{
   return GT_FUNCS.Suspend();
}

BOOL hb_gt_Resume( void )
{
   return GT_FUNCS.Resume();
}

void hb_gt_OutStd( BYTE * pbyStr, ULONG ulLen )
{
   GT_FUNCS.OutStd( pbyStr, ulLen );
}

void hb_gt_OutErr( BYTE * pbyStr, ULONG ulLen )
{
   GT_FUNCS.OutErr( pbyStr, ulLen );
}

char * hb_gt_Version( void )
{
   return GT_FUNCS.Version();
}

BOOL hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
   return GT_FUNCS.SetMode( uiRows, uiCols);
}

BOOL hb_gt_GetBlink( void )
{
   return GT_FUNCS.GetBlink();
}

void hb_gt_SetBlink( BOOL bBlink )
{
   GT_FUNCS.SetBlink( bBlink );
}

void hb_gt_Tone( double dFrequency, double dDuration )
{
   GT_FUNCS.Tone( dFrequency, dDuration );
}

/* ********************************************************************** */

int hb_gt_ExtendedKeySupport()
{
   return GT_FUNCS.ExtendedKeySupport();
}

int hb_gt_ReadKey( HB_inkey_enum eventmask )
{
   return GT_FUNCS.ReadKey( eventmask );
}

/* ********************************************************************** */

void hb_gt_SetDispCP(char * pszTermCDP, char * pszHostCDP, BOOL bBox)
{
   GT_FUNCS.SetDispCP( pszTermCDP, pszHostCDP, bBox );
}

/* ********************************************************************** */

void hb_gt_SetKeyCP(char * pszTermCDP, char * pszHostCDP)
{
   GT_FUNCS.SetKeyCP( pszTermCDP, pszHostCDP );
}

/* ********************************************************************** */

int hb_gt_info(int iMsgType, BOOL bUpdate, int iParam, void *vpParam )
{
   return GT_FUNCS.info( iMsgType, bUpdate, iParam, vpParam );
}

/* ********************************************************************** */

void hb_mouse_Init( void )
{
    GT_FUNCS.mouse_Init();
}

void hb_mouse_Exit( void )
{
   GT_FUNCS.mouse_Exit();
}

BOOL hb_mouse_IsPresent( void )
{
   return GT_FUNCS.mouse_IsPresent();
}

void hb_mouse_Show( void )
{
   GT_FUNCS.mouse_Show();
}

void hb_mouse_Hide( void )
{
   GT_FUNCS.mouse_Hide();
}

int hb_mouse_Col( void )
{
   return GT_FUNCS.mouse_Col();
}

int hb_mouse_Row( void )
{
   return GT_FUNCS.mouse_Row();
}

void hb_mouse_SetPos( int iRow, int iCol )
{
   GT_FUNCS.mouse_SetPos( iRow, iCol );
}

BOOL hb_mouse_IsButtonPressed( int iButton )
{
   return GT_FUNCS.mouse_IsButtonPressed( iButton );
}

int hb_mouse_CountButton( void )
{
   return GT_FUNCS.mouse_CountButton();
}

void hb_mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight )
{
    GT_FUNCS.mouse_SetBounds( iTop, iLeft, iBottom, iRight );
}

void hb_mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight )
{
    GT_FUNCS.mouse_GetBounds( piTop, piLeft, piBottom, piRight );
}

int HB_GT_FUNC( gt_info(int iMsgType, BOOL bUpdate, int iParam, void *vpParam ) )
{
   HB_SYMBOL_UNUSED( bUpdate );
   HB_SYMBOL_UNUSED( iParam );
   HB_SYMBOL_UNUSED( vpParam );

   switch ( iMsgType )
   {
      case GTI_ISGRAPHIC:
      return (int) FALSE;
   }
   // DEFAULT: there's something wrong if we are here.
   return -1;
}

/* ************************** Clipboard support ********************************** */

void HB_GT_FUNC( gt_GetClipboard( char *szData, ULONG *pulMaxSize ) )
{
   if ( *pulMaxSize == 0 || s_clipsize < *pulMaxSize )
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
   if ( s_clipboard != NULL )
   {
      hb_xfree( s_clipboard );
   }

   s_clipboard = (char *) hb_xgrab( ulSize +1 );
   memcpy( s_clipboard, szData, ulSize );
   s_clipboard[ ulSize ] = '\0';
   s_clipsize = ulSize;
}

ULONG HB_GT_FUNC( gt_GetClipboardSize( void ) )
{
   return s_clipsize;
}

/* ********************************************************************** */

/*
static HB_GT_FUNCS GT_FUNCS = {
    Init:                   HB_GT_FUNC( gt_Init ),
    Exit:                   HB_GT_FUNC( gt_Exit ),
    GetScreenWidth:         HB_GT_FUNC( gt_GetScreenWidth ),
    GetScreenHeight:        HB_GT_FUNC( gt_GetScreenHeight ),
    Col:                    HB_GT_FUNC( gt_Col ),
    Row:                    HB_GT_FUNC( gt_Row ),
    SetPos:                 HB_GT_FUNC( gt_SetPos ),
    AdjustPos:              HB_GT_FUNC( gt_AdjustPos ),
    IsColor:                HB_GT_FUNC( gt_IsColor ),
    GetCursorStyle:         HB_GT_FUNC( gt_GetCursorStyle ),
    SetCursorStyle:         HB_GT_FUNC( gt_SetCursorStyle ),
    DispBegin:              HB_GT_FUNC( gt_DispBegin ),
    DispEnd:                HB_GT_FUNC( gt_DispEnd ),
    DispCount:              HB_GT_FUNC( gt_DispCount ),
    Puts:                   HB_GT_FUNC( gt_Puts ),
    Replicate:              HB_GT_FUNC( gt_Replicate ),
    RectSize:               HB_GT_FUNC( gt_RectSize ),
    GetText:                HB_GT_FUNC( gt_GetText ),
    PutText:                HB_GT_FUNC( gt_PutText ),
    SetAttribute:           HB_GT_FUNC( gt_SetAttribute ),
    Scroll:                 HB_GT_FUNC( gt_Scroll ),
    SetMode:                HB_GT_FUNC( gt_SetMode ),
    GetBlink:               HB_GT_FUNC( gt_GetBlink ),
    SetBlink:               HB_GT_FUNC( gt_SetBlink ),
    Version:                HB_GT_FUNC( gt_Version ),
    Box:                    HB_GT_FUNC( gt_Box ),
    BoxD:                   HB_GT_FUNC( gt_BoxD ),
    BoxS:                   HB_GT_FUNC( gt_BoxS ),
    HorizLine:              HB_GT_FUNC( gt_HorizLine ),
    VertLine:               HB_GT_FUNC( gt_VertLine ),
    Suspend:                HB_GT_FUNC( gt_Suspend ),
    Resume:                 HB_GT_FUNC( gt_Resume ),
    PreExt:                 HB_GT_FUNC( gt_PreExt ),
    PostExt:                HB_GT_FUNC( gt_PostExt ),
    OutStd:                 HB_GT_FUNC( gt_OutStd ),
    OutErr:                 HB_GT_FUNC( gt_OutErr ),
    Tone:                   HB_GT_FUNC( gt_Tone ),

    ExtendedKeySupport:     HB_GT_FUNC( gt_ExtendedKeySupport ),
    ReadKey:                HB_GT_FUNC( gt_ReadKey ),

    SetDispCP:              HB_GT_FUNC( gt_SetDispCP );
    SetKeyCP:               HB_GT_FUNC( gt_SetKeyCP );

    mouse_Init:             HB_GT_FUNC( mouse_Init ),
    mouse_Exit:             HB_GT_FUNC( mouse_Exit ),
    mouse_IsPresent:        HB_GT_FUNC( mouse_IsPresent ),
    mouse_Show:             HB_GT_FUNC( mouse_Show ),
    mouse_Hide:             HB_GT_FUNC( mouse_Hide ),
    mouse_Col:              HB_GT_FUNC( mouse_Col ),
    mouse_Row:              HB_GT_FUNC( mouse_Row ),
    mouse_SetPos:           HB_GT_FUNC( mouse_SetPos ),
    mouse_IsButtonPressed:  HB_GT_FUNC( mouse_IsButtonPressed ),
    mouse_CountButton:      HB_GT_FUNC( mouse_CountButton ),
    mouse_SetBounds:        HB_GT_FUNC( mouse_SetBounds ),
    mouse_GetBounds:        HB_GT_FUNC( mouse_GetBounds ),
};
*/

#ifdef HB_MULTI_GT

static void HB_GT_FUNC(gtFnInit( PHB_GT_FUNCS gt_funcs ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_%s_gtFnInit(%p)", HB_GT_DRVNAME( HB_GT_NAME ), gt_funcs));

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
    gt_funcs->SetDispCP             = HB_GT_FUNC( gt_SetDispCP );
    gt_funcs->SetKeyCP              = HB_GT_FUNC( gt_SetKeyCP );
    gt_funcs->info                  = HB_GT_FUNC( gt_info );
    gt_funcs->SetClipboard          = HB_GT_FUNC( gt_SetClipboard );
    gt_funcs->GetClipboard          = HB_GT_FUNC( gt_GetClipboard );
    gt_funcs->GetClipboardSize      = HB_GT_FUNC( gt_GetClipboardSize );
}

/* ********************************************************************** */

static void HB_GT_FUNC(mouseFnInit( PHB_GT_FUNCS gt_funcs ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_%s_mouseFnInit(%p)", HB_GT_DRVNAME( HB_GT_NAME ), gt_funcs));

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
#if defined(HB_STATIC_STARTUP) || ( (! defined(__GNUC__)) && (! defined(_MSC_VER)) && (! defined(__BORLANDC__)) )
   #pragma startup HB_GT_FUNC(_gt_Init_)
#endif

#if defined(__BORLANDC__)
   HB_CALL_ON_STARTUP_BEGIN( startup_function__gtnul )
      hb_gtRegister( &gtInit );
   HB_CALL_ON_STARTUP_END( startup_function__gtnul )
   #pragma startup startup_function__gtnul
#endif

#endif  /* HB_MULTI_GT */

/* ********************************************************************** */
