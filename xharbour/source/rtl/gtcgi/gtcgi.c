/*
 * $Id: gtcgi.c,v 1.9 2004/02/01 23:40:50 jonnymind Exp $
 */

/*
 * Harbour Project source code:
 * Video subsystem for plain ANSI C stream IO
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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
#define HB_GT_NAME	CGI

/* TODO: include any standard headers here */

#include "hbapifs.h"
#include "hbapigt.h"
#include "hbapifs.h"

static SHORT  s_iRow;
static SHORT  s_iCol;
static USHORT s_uiMaxRow;
static USHORT s_uiMaxCol;
static USHORT s_uiCursorStyle;
static BOOL   s_bBlink;
static USHORT s_uiDispCount;
static char * s_szCrLf;
static ULONG  s_ulCrLf;
static BOOL s_OutputDone;

static int s_iStdIn, s_iStdOut, s_iStdErr;

static char *s_clipboard = NULL;
static int s_clipsize = 0;

void HB_GT_FUNC(gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

   s_uiDispCount = 0;
   s_OutputDone = FALSE;
   s_iRow = 0;
   s_iCol = 0;
   s_uiMaxRow = 1;
   s_uiMaxCol = 0;
   s_uiCursorStyle = SC_NORMAL;
   s_bBlink = FALSE;

   s_szCrLf = hb_conNewLine();
   s_ulCrLf = strlen( s_szCrLf );

   /* stdin && stdout && stderr */
   s_iStdIn  = iFilenoStdin;
   s_iStdOut = iFilenoStdout;
   s_iStdErr = iFilenoStderr;

   hb_fsSetDevMode( s_iStdOut, FD_BINARY );

   HB_GT_FUNC(mouse_Init());
}

void HB_GT_FUNC(gt_Exit( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

   if ( s_clipboard != NULL )
   {
      hb_xfree( s_clipboard );
   }

   HB_GT_FUNC(mouse_Exit());
}

static void out_stdout( char * pStr, ULONG ulLen )
{
   unsigned uiErrorOld = hb_fsError(); /* Save current user file error code */
   hb_fsWriteLarge( s_iStdOut, ( BYTE * ) pStr, ulLen );
   hb_fsSetError( uiErrorOld );        /* Restore last user file error code */
   s_OutputDone = TRUE;
}

static void out_newline( void )
{
   if ( s_OutputDone )
      out_stdout( s_szCrLf, s_ulCrLf );
   s_OutputDone = TRUE;
}

int HB_GT_FUNC(gt_ExtendedKeySupport())
{
   return 0;
}
int HB_GT_FUNC(gt_ReadKey( HB_inkey_enum eventmask ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

   HB_SYMBOL_UNUSED( eventmask );

   return 13;
}

BOOL HB_GT_FUNC(gt_AdjustPos( BYTE * pStr, ULONG ulLen ))
{
   USHORT row = s_iRow;
   USHORT col = s_iCol;
   ULONG ulCount;
   BOOL bCrLfSeq;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

   /* CGI output allows only internet valid CRLF, indipendently from
      CR/LF combinations in the source data.*/

   for( ulCount = 0; ulCount < ulLen; ulCount++ )
   {
      switch( *pStr++  )
      {
         case HB_CHAR_BEL:
            bCrLfSeq = FALSE;
            break;

         case HB_CHAR_BS:
            if( col )
               col--;
            bCrLfSeq = FALSE;
            break;

         case HB_CHAR_CR:
            row++;
            col = 0;
            out_newline();
            bCrLfSeq = TRUE;
            break;

         case HB_CHAR_LF:
            if ( ! bCrLfSeq ) {
               row++;
               col = 0;
               out_newline();
               break;
            }
            bCrLfSeq = FALSE;
            break;

         default:
            col++;
      }
   }
   s_OutputDone = TRUE;
   HB_GT_FUNC(gt_SetPos( row, col, HB_GT_SET_POS_AFTER ));
   return TRUE;
}

BOOL HB_GT_FUNC(gt_IsColor( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

   return FALSE;
}

USHORT HB_GT_FUNC(gt_GetScreenWidth( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

   return s_uiMaxCol;
}

USHORT HB_GT_FUNC(gt_GetScreenHeight( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

   return s_uiMaxRow;
}

void HB_GT_FUNC(gt_SetPos( SHORT iRow, SHORT iCol, SHORT iMethod ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", iRow, iCol, iMethod));

   if ( iMethod == HB_GT_SET_POS_BEFORE && s_OutputDone )
   {
      if( iRow != s_iRow  )
      {
         out_newline();
         s_iCol = 0;
      }
      while( s_iCol++ < iCol-1 ) out_stdout( " ", 1 );
      s_OutputDone = TRUE;
   }

   s_iRow = iRow;
   s_iCol = iCol;
}

SHORT HB_GT_FUNC(gt_Col( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

   return s_iCol;
}

SHORT HB_GT_FUNC(gt_Row( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

   return s_iRow;
}

USHORT HB_GT_FUNC(gt_GetCursorStyle( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

   return s_uiCursorStyle;
}

void HB_GT_FUNC(gt_SetCursorStyle( USHORT uiCursorStyle ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", uiCursorStyle));

   s_uiCursorStyle = uiCursorStyle;
}

static void HB_GT_FUNC(gt_xPutch( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar ))
{
   char szBuffer[ 2 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %i)", uiRow, uiCol, (int) byAttr, byAttr));

   HB_SYMBOL_UNUSED( byAttr );

   HB_GT_FUNC(gt_SetPos( uiRow, uiCol, HB_GT_SET_POS_BEFORE ));

   szBuffer[ 0 ] = byChar;
   szBuffer[ 1 ] = '\0';
   out_stdout( szBuffer, 1 );

   HB_GT_FUNC(gt_AdjustPos( ( BYTE * ) szBuffer, 1 ));
}

void HB_GT_FUNC(gt_Puts( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE * pbyStr, ULONG ulLen ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", uiRow, uiCol, (int) byAttr, pbyStr, ulLen));

   HB_SYMBOL_UNUSED( byAttr );

   HB_GT_FUNC(gt_SetPos( uiRow, uiCol, HB_GT_SET_POS_BEFORE ));

   out_stdout( ( char * ) pbyStr, ulLen );

   HB_GT_FUNC(gt_AdjustPos( pbyStr, ulLen ));
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
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( byAttr );
}

void HB_GT_FUNC(gt_Scroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr, SHORT iRows, SHORT iCols ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hu, %hu)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr, iRows, iCols));

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( byAttr );
   HB_SYMBOL_UNUSED( iRows );
   HB_SYMBOL_UNUSED( iCols );

   out_newline();

   s_iRow = 0;
   s_iRow = 0;
}

void HB_GT_FUNC(gt_DispBegin( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

   ++s_uiDispCount;
   /* Do nothing else */
}

void HB_GT_FUNC(gt_DispEnd())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

   --s_uiDispCount;
   /* Do nothing else */
}

BOOL HB_GT_FUNC(gt_SetMode( USHORT uiMaxRow, USHORT uiMaxCol ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", uiMaxRow, uiMaxCol));

   s_uiMaxRow = uiMaxRow;
   s_uiMaxCol = uiMaxCol;

   return FALSE;
}

BOOL HB_GT_FUNC(gt_GetBlink())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

   return s_bBlink;
}

void HB_GT_FUNC(gt_SetBlink( BOOL bBlink ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

   s_bBlink = bBlink;
}

void HB_GT_FUNC(gt_Tone( double dFrequency, double dDuration ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

   HB_SYMBOL_UNUSED( dFrequency );
   HB_SYMBOL_UNUSED( dDuration );
}

char * HB_GT_FUNC(gt_Version( void ))
{
   return "Harbour Terminal: Standard stream console";
}

USHORT HB_GT_FUNC(gt_DispCount())
{
   return s_uiDispCount;
}

void HB_GT_FUNC(gt_Replicate( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar, ULONG nLength ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", uiRow, uiCol, byAttr, byChar, nLength));

   while( nLength-- )
      HB_GT_FUNC(gt_xPutch( uiRow, uiCol++, byAttr, byChar ));
}

USHORT HB_GT_FUNC(gt_Box( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
                          BYTE * szBox, BYTE byAttr ))
{
   USHORT ret = 1;
   SHORT Row;
   SHORT Col;
   SHORT Height;
   SHORT Width;

   if( ( Left   >= 0 && Left   < HB_GT_FUNC(gt_GetScreenWidth())  )  ||
       ( Right  >= 0 && Right  < HB_GT_FUNC(gt_GetScreenWidth())  )  ||
       ( Top    >= 0 && Top    < HB_GT_FUNC(gt_GetScreenHeight()) )  ||
       ( Bottom >= 0 && Bottom < HB_GT_FUNC(gt_GetScreenHeight()) ) )
   {

      /* Ensure that box is drawn from top left to bottom right. */
      if( Top > Bottom )
      {
         SHORT tmp = Top;
         Top = Bottom;
         Bottom = tmp;
      }
      if( Left > Right )
      {
         SHORT tmp = Left;
         Left = Right;
         Right = tmp;
      }

      /* Draw the box or line as specified */
      Height = Bottom - Top + 1;
      Width  = Right - Left + 1;

      HB_GT_FUNC(gt_DispBegin());

      if( Height > 1 && Width > 1 && Top >= 0 && Top < HB_GT_FUNC(gt_GetScreenHeight()) && Left >= 0 && Left < HB_GT_FUNC(gt_GetScreenWidth()) )
         HB_GT_FUNC(gt_xPutch( Top, Left, byAttr, szBox[ 0 ] )); /* Upper left corner */

      Col = ( Height > 1 ? Left + 1 : Left );
      if(Col < 0 )
      {
         Width += Col;
         Col = 0;
      }
      if( Right >= HB_GT_FUNC(gt_GetScreenWidth()) )
      {
         Width -= Right - HB_GT_FUNC(gt_GetScreenWidth());
      }

      if( Col <= Right && Col < HB_GT_FUNC(gt_GetScreenWidth()) && Top >= 0 && Top < HB_GT_FUNC(gt_GetScreenHeight()) )
         HB_GT_FUNC(gt_Replicate( Top, Col, byAttr, szBox[ 1 ], Width + ( (Right - Left) > 1 ? -2 : 0 ) )); /* Top line */

      if( Height > 1 && (Right - Left) > 1 && Right < HB_GT_FUNC(gt_GetScreenWidth()) && Top >= 0 && Top < HB_GT_FUNC(gt_GetScreenHeight()) )
         HB_GT_FUNC(gt_xPutch( Top, Right, byAttr, szBox[ 2 ] )); /* Upper right corner */

      if( szBox[ 8 ] && Height > 2 && Width > 2 )
      {
         for( Row = Top + 1; Row < Bottom; Row++ )
         {
            if( Row >= 0 && Row < HB_GT_FUNC(gt_GetScreenHeight()) )
            {
               Col = Left;
               if( Col < 0 )
                  Col = 0; /* The width was corrected earlier. */
               else
                  HB_GT_FUNC(gt_xPutch( Row, Col++, byAttr, szBox[ 7 ] )); /* Left side */
               HB_GT_FUNC(gt_Replicate( Row, Col, byAttr, szBox[ 8 ], Width - 2 )); /* Fill */
               if( Right < HB_GT_FUNC(gt_GetScreenWidth()) )
                  HB_GT_FUNC(gt_xPutch( Row, Right, byAttr, szBox[ 3 ] )); /* Right side */
            }
         }
      }
      else
      {
         for( Row = ( Width > 1 ? Top + 1 : Top ); Row < ( (Right - Left ) > 1 ? Bottom : Bottom + 1 ); Row++ )
         {
            if( Row >= 0 && Row < HB_GT_FUNC(gt_GetScreenHeight()) )
            {
               if( Left >= 0 && Left < HB_GT_FUNC(gt_GetScreenWidth()) )
                  HB_GT_FUNC(gt_xPutch( Row, Left, byAttr, szBox[ 7 ] )); /* Left side */
               if( ( Width > 1 || Left < 0 ) && Right < HB_GT_FUNC(gt_GetScreenWidth()) )
                  HB_GT_FUNC(gt_xPutch( Row, Right, byAttr, szBox[ 3 ] )); /* Right side */
            }
         }
      }

      if( Height > 1 && Width > 1 )
      {
         if( Left >= 0 && Bottom < HB_GT_FUNC(gt_GetScreenHeight()) )
            HB_GT_FUNC(gt_xPutch( Bottom, Left, byAttr, szBox[ 6 ] )); /* Bottom left corner */

         Col = Left + 1;
         if( Col < 0 )
            Col = 0; /* The width was corrected earlier. */

         if( Col <= Right && Bottom < HB_GT_FUNC(gt_GetScreenHeight()) )
            HB_GT_FUNC(gt_Replicate( Bottom, Col, byAttr, szBox[ 5 ], Width - 2 )); /* Bottom line */

         if( Right < HB_GT_FUNC(gt_GetScreenWidth()) && Bottom < HB_GT_FUNC(gt_GetScreenHeight()) )
            HB_GT_FUNC(gt_xPutch( Bottom, Right, byAttr, szBox[ 4 ] )); /* Bottom right corner */
      }
      HB_GT_FUNC(gt_DispEnd());
      ret = 0;
   }

   return ret;
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

USHORT HB_GT_FUNC(gt_VertLine( SHORT Col, SHORT Top, SHORT Bottom, BYTE byChar, BYTE byAttr ))
{
   USHORT ret = 1;
   USHORT Row;

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
      while( Row <= Bottom )
         HB_GT_FUNC(gt_xPutch( Row++, Col, byAttr, byChar ));
      ret = 0;
   }
   return ret;
}

BOOL HB_GT_FUNC(gt_PreExt())
{
   return TRUE;
}

BOOL HB_GT_FUNC(gt_PostExt())
{
   return TRUE;
}

BOOL HB_GT_FUNC(gt_Suspend())
{
   return TRUE;
}

BOOL HB_GT_FUNC(gt_Resume())
{
   return TRUE;
}

void HB_GT_FUNC(gt_OutStd( BYTE * pbyStr, ULONG ulLen ))
{
    hb_fsWriteLarge( s_iStdOut, ( BYTE * ) pbyStr, ulLen );
}

void HB_GT_FUNC(gt_OutErr( BYTE * pbyStr, ULONG ulLen ))
{
    hb_fsWriteLarge( s_iStdErr, ( BYTE * ) pbyStr, ulLen );
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

/* *********************************************************************** */
/*
* GTInfo() implementation
*/
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
