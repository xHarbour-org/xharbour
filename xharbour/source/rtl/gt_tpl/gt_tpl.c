/*
 * $Id: gt_tpl.c,v 1.3 2003/05/21 09:35:35 druzus Exp $
 */

/*
 * Harbour Project source code:
 * {Video subsystem template}
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

/* NOTE: User programs should never call this layer directly! */

/* This definition has to be placed before #include "hbapigt.h" */
#define HB_GT_NAME	TPL

/* TODO: include any standard headers here */

#include "hbapigt.h"

static USHORT s_uiDispCount;
static int s_iStdIn, s_iStdOut, s_iStdErr;

void HB_GT_FUNC(gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

   /* TODO: Is anything required to initialize the video subsystem? */

   s_uiDispCount = 0;
   s_iStdIn  = iFilenoStdin;
   s_iStdOut = iFilenoStdout;
   s_iStdErr = iFilenoStderr;
}

void HB_GT_FUNC(gt_Exit( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

   /* TODO: */
}

int HB_GT_FUNC(gt_ExtendedKeySupport())
{
   /* TODO: If this GTAPI supports the Harbour extended key
            codes, then change the return value from 0 to 1. */
   return 0;
}

int HB_GT_FUNC(gt_ReadKey( HB_inkey_enum eventmask ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

   HB_SYMBOL_UNUSED( eventmask );

   /* TODO: */

   return 0;
}

BOOL HB_GT_FUNC(gt_AdjustPos( BYTE * pStr, ULONG ulLen ))
{
   USHORT row = HB_GT_FUNC(gt_Row());
   USHORT col = HB_GT_FUNC(gt_Col());
   ULONG ulCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

   for( ulCount = 0; ulCount < ulLen; ulCount++ )
   {
      switch( *pStr++ )
      {
         case HB_CHAR_BEL:
            break;

         case HB_CHAR_BS:
            if( col )
               col--;
            else
            {
               col = HB_GT_FUNC(gt_GetScreenWidth());
               if( row )
                  row--;
            }
            break;

         case HB_CHAR_LF:
            if( row < HB_GT_FUNC(gt_GetScreenHeight()) )
               row++;
            break;

         case HB_CHAR_CR:
            col = 0;
            break;

         default:
            if( col < HB_GT_FUNC(gt_GetScreenWidth()) )
               col++;
            else
            {
               col = 0;
               if( row < HB_GT_FUNC(gt_GetScreenHeight()) )
                  row++;
            }
      }
   }

   HB_GT_FUNC(gt_SetPos( row, col, HB_GT_SET_POS_AFTER ));

   return TRUE;
}

BOOL HB_GT_FUNC(gt_IsColor( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

   /* TODO: How to detect this? */
   return TRUE;
}

USHORT HB_GT_FUNC(gt_GetScreenWidth( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

   /* TODO: How many columns on screen? */
   return 0;
}

USHORT HB_GT_FUNC(gt_GetScreenHeight( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

   /* TODO: How many rows on screen? */
   return 0;
}

void HB_GT_FUNC(gt_SetPos( SHORT iRow, SHORT iCol, SHORT iMethod ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", iRow, iCol, iMethod));

   /* TODO: How to reposition the cursor? */

   HB_SYMBOL_UNUSED( iRow );
   HB_SYMBOL_UNUSED( iCol );
   HB_SYMBOL_UNUSED( iMethod );
}

SHORT HB_GT_FUNC(gt_Col( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

   /* TODO: What Column is the cursor on? */
   return 0;
}

SHORT HB_GT_FUNC(gt_Row( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

   /* TODO: What Row is the cursor on? */
   return 0;
}

USHORT HB_GT_FUNC(gt_GetCursorStyle( void ))
{
   USHORT uiStyle = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

   /* TODO: What shape is the cursor? */

   /* example from the dos driver */

/*
   HB_GT_FUNC(gt_GetCursorSize( &start, &end ))

   if( start == 32 && end == 32 )
      uiStyle = SC_NONE;
   else if( start == 6 && end == 7 )
      uiStyle = SC_NORMAL;
   else if( start == 4 && end == 7 )
      uiStyle = SC_INSERT;
   else if( start == 0 && end == 7 )
      uiStyle = SC_SPECIAL1;
   else if( start == 0 && end == 3 )
      uiStyle = SC_SPECIAL2;
   else
      uiStyle = 0;
*/

   return uiStyle;
}

void HB_GT_FUNC(gt_SetCursorStyle( USHORT uiStyle ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", uiStyle));

   /* TODO: How to set the shape of the cursor? */
   /* see ..\..\..\tests\working\cursrtst.prg for an explanation */
   switch( uiStyle )
   {
   case SC_NONE:
      /* TODO: turn it off */
      break;

   case SC_NORMAL:
      break;

   case SC_INSERT:
      break;

   case SC_SPECIAL1:
      break;

   case SC_SPECIAL2:
      break;

   default:
      break;
   }
}

static void HB_GT_FUNC(gt_xPutch( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %i)", uiRow, uiCol, (int) byAttr, byChar));

   HB_SYMBOL_UNUSED( uiRow );
   HB_SYMBOL_UNUSED( uiCol );
   HB_SYMBOL_UNUSED( byAttr );
   HB_SYMBOL_UNUSED( byChar );
}

void HB_GT_FUNC(gt_Puts( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE * pbyStr, ULONG ulLen ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", uiRow, uiCol, (int) byAttr, pbyStr, ulLen));

   HB_SYMBOL_UNUSED( uiRow );
   HB_SYMBOL_UNUSED( uiCol );
   HB_SYMBOL_UNUSED( byAttr );
   HB_SYMBOL_UNUSED( pbyStr );
   HB_SYMBOL_UNUSED( ulLen );
}

int HB_GT_FUNC(gt_RectSize( USHORT rows, USHORT cols ))
{
   return rows * cols * 2;
}

void HB_GT_FUNC(gt_GetText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyDst ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbyDst));

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( pbyDst );
}

void HB_GT_FUNC(gt_PutText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbySrc ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbySrc));

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( pbySrc );
}

void HB_GT_FUNC(gt_SetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

   /* TODO: we want to take a screen that is say bright white on blue,
            and change the attributes only for a section of the screen
            to white on black.
   */

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( byAttr );
}

void HB_GT_FUNC(gt_Scroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr, SHORT iRows, SHORT iCols ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr, iRows, iCols));

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( iRows );
   HB_SYMBOL_UNUSED( iCols );
}

void HB_GT_FUNC(gt_DispBegin( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

   ++s_uiDispCount;

   /* TODO: Is there a way to change screen buffers?
            ie: can we write somewhere without it going to the screen
            and then update the screen from this buffer at a later time?
            We will initially want to copy the current screen to this buffer.
   */
}

void HB_GT_FUNC(gt_DispEnd())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

   --s_uiDispCount;

   /* TODO: here we flush the buffer, and restore normal screen writes */
}

BOOL HB_GT_FUNC(gt_SetMode( USHORT uiRows, USHORT uiCols ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", uiRows, uiCols));

   /* TODO: How to change the size of the screen? */

   HB_SYMBOL_UNUSED( uiRows );
   HB_SYMBOL_UNUSED( uiCols );

   return FALSE;
}

BOOL HB_GT_FUNC(gt_GetBlink())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

   /* TODO: under dos, the background 'intensity' bit can be switched
            from intensity to 'blinking'
            does this work under your platform?
   */

   return FALSE;
}

void HB_GT_FUNC(gt_SetBlink( BOOL bBlink ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

   /* TODO: set the bit if it's supported */

   HB_SYMBOL_UNUSED( bBlink );
}

void HB_GT_FUNC(gt_Tone( double dFrequency, double dDuration ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

   /* TODO: Implement this */

   HB_SYMBOL_UNUSED( dFrequency );
   HB_SYMBOL_UNUSED( dDuration );
}

char * HB_GT_FUNC(gt_Version( void ))
{
   return "Harbour Terminal: (template)";
}

USHORT HB_GT_FUNC(gt_DispCount())
{
   return s_uiDispCount;
}

void HB_GT_FUNC(gt_Replicate( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar, ULONG nLength ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", uiRow, uiCol, byAttr, byChar, nLength));

   /* TODO: replace it with native (optimized) version */
   while( nLength-- )
      HB_GT_FUNC(gt_xPutch( uiRow, uiCol++, byAttr, byChar ));
}

USHORT HB_GT_FUNC(gt_Box( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
                          BYTE * pbyFrame, BYTE byAttr ))
{
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
   return HB_GT_FUNC(gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr ));
}

USHORT HB_GT_FUNC(gt_BoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr ))
{
   return HB_GT_FUNC(gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr ));
}

USHORT HB_GT_FUNC(gt_HorizLine( SHORT Row, SHORT Left, SHORT Right, BYTE byChar, BYTE byAttr ))
{
   if( Left < Right )
      HB_GT_FUNC(gt_Replicate( Row, Left, byAttr, byChar, Right - Left + 1 ));
   else
      HB_GT_FUNC(gt_Replicate( Row, Right, byAttr, byChar, Left - Right + 1 ));

   return 0;
}

USHORT HB_GT_FUNC(gt_VertLine( SHORT Col, SHORT Top, SHORT Bottom, BYTE byChar, BYTE byAttr ))
{
   SHORT Row;

   if( Top <= Bottom )
      Row = Top;
   else
   {
      Row = Bottom;
      Bottom = Top;
   }

   while( Row <= Bottom )
      HB_GT_FUNC(gt_xPutch( Row++, Col, byAttr, byChar ));

   return 0;
}

BOOL HB_GT_FUNC(gt_PreExt())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PreExt()"));

   return TRUE;
}

BOOL HB_GT_FUNC(gt_PostExt())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PostExt()"));

   return TRUE;
}

BOOL HB_GT_FUNC(gt_Suspend())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Suspend()"));

   return TRUE;
}

BOOL HB_GT_FUNC(gt_Resume())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Resume()"));

   return TRUE;
}

void HB_GT_FUNC(gt_OutStd( BYTE * pbyStr, ULONG ulLen ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_OutStd(%s, %hu)", pbyStr, ulLen));

   hb_fsWriteLarge( s_iStdOut, ( BYTE * ) pbyStr, ulLen );
}

void HB_GT_FUNC(gt_OutErr( BYTE * pbyStr, ULONG ulLen ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_OutErr(%s, %hu)", pbyStr, ulLen));

   hb_fsWriteLarge( s_iStdErr, ( BYTE * ) pbyStr, ulLen );
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
