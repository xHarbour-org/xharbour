/*
 * $Id: gtpca.c,v 1.7 2003/06/30 17:08:57 ronpinkas Exp $
 */

/*
 * Harbour Project source code:
 * Video subsystem for ANSI terminals
 *
 * Copyright 2000 David G. Holm <dholm@jsd-llc.com>
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
 *  This module is partially based on VIDMGR by Andrew Clarke and modified
 *  for the Harbour project
 */

/* NOTE: User programs should never call this layer directly! */

/* This definition has to be placed before #include "hbapigt.h" */
#define HB_GT_NAME	PCA

#if defined(__GNUC__) && ! defined(__MINGW32__)
   #include <unistd.h>
   #if defined(__DJGPP__) || defined(__CYGWIN__) || defined(__EMX__)
      #include <io.h>
   #endif
#else
   #include <io.h>
#endif

#include <ctype.h>
#include <string.h>

#include "hbapigt.h"
#include "hbapifs.h"
#include "hbset.h"
#include "inkey.ch"

static USHORT s_usRow, s_usCol, s_usMaxRow, s_usMaxCol;
static int s_iFilenoStdin, s_iFilenoStdout, s_iFilenoStderr;
static int s_iAttribute;
static BOOL s_bColor;
static char s_szSpaces[] = "                                                                                    "; /* 84 spaces */

static void HB_GT_FUNC(gt_AnsiGetCurPos( USHORT * row, USHORT * col ));

static USHORT s_uiDispCount;

void HB_GT_FUNC(gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));
   s_iAttribute = 0;
   s_iFilenoStdin = iFilenoStdin;
   s_iFilenoStdout = iFilenoStdout;
   s_iFilenoStderr = iFilenoStderr;
   s_usRow = s_usCol = 0;
   HB_GT_FUNC(gt_AnsiGetCurPos( &s_usRow, &s_usCol ));
#ifdef OS_UNIX_COMPATIBLE
   s_usMaxRow = 23;
   s_bColor = FALSE;
#else
   s_usMaxRow = 24;
   s_bColor = TRUE;
#endif
   s_usMaxCol = 79;
   fprintf( stdout, "\x1B[=7h" ); /* Enable line wrap (for OUTSTD() and OUTERR()) */

   HB_GT_FUNC(mouse_Init());
}

void HB_GT_FUNC(gt_Exit( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

   HB_GT_FUNC(mouse_Exit());
   /* TODO: */
}

static void HB_GT_FUNC(gt_AnsiGetCurPos( USHORT * row, USHORT * col ))
{
   if( isatty( s_iFilenoStdin ) && isatty( s_iFilenoStdout ) )
   {
      USHORT ch, value = 0, index = 0;
      fprintf( stdout, "\x1B[6n" );
      do
      {
        ch = getc( stdin );
        if( isdigit( ch ) )
        {
           value = ( value * 10 ) + ( ch - '0' );
        }
        else if( ch == ';' )
        {
           *row = value - 1;
           value = 0;
        }
      }
      while( ch != 'R' && index < 10 );
      *col = value - 1;
   }
}

static void HB_GT_FUNC(gt_AnsiSetAttributes( BYTE attr ))
{
   static const int color[] = { 0, 4, 2, 6, 1, 5, 3, 7 };
   int bg_color = 40 + color[ ( attr & 0x70 ) >> 4 ];
   int fg_color = 30 + color[ attr & 0x07 ];
   int special = 0;
   if( attr & 0x08 ) special = 1;
   else if( attr & 0x80 )
   {
      if( hb_set.HB_SET_INTENSITY ) special = 1;
      else special = 5;
   }
   fprintf( stdout, "\x1B[%d;%d;%dm", special, fg_color, bg_color );
}

BOOL HB_GT_FUNC(gt_AdjustPos( BYTE * pStr, ULONG ulLen ))
{
   USHORT row = s_usRow;
   USHORT col = s_usCol;
   ULONG ulCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

   for( ulCount = 0; ulCount < ulLen; ulCount++ )
   {
      switch( *pStr++  )
      {
         case HB_CHAR_BEL:
            break;

         case HB_CHAR_BS:
            if( col )
               col--;
            else
            {
               col = s_usMaxCol;
               if( row )
                  row--;
            }
            break;

         case HB_CHAR_LF:
            if( row < s_usMaxRow )
               row++;
            break;

         case HB_CHAR_CR:
            col = 0;
            break;

         default:
            if( col < s_usMaxCol )
               col++;
            else
            {
               col = 0;
               if( row < s_usMaxRow )
                  row++;
            }
      }
   }
   HB_GT_FUNC(gt_SetPos( row, col, HB_GT_SET_POS_AFTER ));
   return TRUE;
}

int HB_GT_FUNC(gt_ExtendedKeySupport())
{
   return 0;
}

#ifdef HARBOUR_GCC_OS2
   #include "..\..\kbdos2.gcc"
#else
int HB_GT_FUNC(gt_ReadKey( HB_inkey_enum eventmask ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));
   HB_SYMBOL_UNUSED( eventmask );
   /* TODO: */
   return 13;
}
#endif

BOOL HB_GT_FUNC(gt_IsColor( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));
   /* TODO: */
   return s_bColor;
}

USHORT HB_GT_FUNC(gt_GetScreenWidth( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));
   /* TODO: */
   return s_usMaxCol + 1;
}

USHORT HB_GT_FUNC(gt_GetScreenHeight( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));
   /* TODO: */
   return s_usMaxRow + 1;
}

void HB_GT_FUNC(gt_SetPos( SHORT iRow, SHORT iCol, SHORT iMethod ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", iRow, iCol, iMethod));

   HB_SYMBOL_UNUSED( iMethod );

   if( iRow < 0 ) iRow = 0;
   else if( iRow > s_usMaxRow ) iRow = s_usMaxRow;
   if( iCol < 0 ) iCol = 0;
   else if( iCol > s_usMaxCol ) iCol = s_usMaxCol;
   s_usRow = iRow;
   s_usCol = iCol;
   fprintf( stdout, "\x1B[%d;%dH", s_usRow + 1, s_usCol + 1 );
}

SHORT HB_GT_FUNC(gt_Row( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));
   return s_usRow;
}

SHORT HB_GT_FUNC(gt_Col( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));
   return s_usCol;
}


void HB_GT_FUNC(gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr, SHORT sVert, SHORT sHoriz ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRight, (int) attr, sVert, sHoriz));
   HB_SYMBOL_UNUSED( attr );
   if( sVert == 0 && sHoriz == 0 )
   {
      /* Clear */
      if( usTop == 0 && usLeft == 0 && usBottom >= s_usMaxRow && usRight >= s_usMaxCol )
      {
         /* Clear the entire screen */
         fprintf( stdout, "\x1B[2J" );
      }
      else
      {
         /* Clear a screen region */
         USHORT i;
         for( i = usTop; i <= usBottom; i++ )
         {
            HB_GT_FUNC(gt_Puts( i, usLeft, s_iAttribute, ( BYTE * )s_szSpaces, (usRight - usLeft ) + 1 ));
         }
      }
   }
   else
   {
      if( usTop == 0 && usLeft == 0 && usBottom >= s_usMaxRow && usRight >= s_usMaxCol )
      {
         if( sVert > 0 && sHoriz == 0 )
         {
            /* Scroll the entire screen up */
            fprintf( stdout, "\x1B[25;80" );
            while( sVert-- ) fputc( '\n', stdout );
         }
         else
         {
            /* TODO: Scroll the entire screen any direction other than up */
         }
      }
      else
      {
         /* TODO: Scroll a screen region */
      }
   }
}

USHORT HB_GT_FUNC(gt_GetCursorStyle( void ))
{
   /* TODO: What shape is the cursor? */
   USHORT uiStyle = 0;
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));
   return uiStyle;
}

void HB_GT_FUNC(gt_SetCursorStyle( USHORT style ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", style));
   HB_SYMBOL_UNUSED( style );
}

static void HB_GT_FUNC(gt_xPutch( USHORT usRow, USHORT usCol, BYTE attr, BYTE byChar ))
{
   char tmp[ 2 ];
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %i)", usRow, usCol, (int) attr, byChar));

   /* TOFIX: add correct support for a single byte instead of a string
    */
   tmp[ 0 ] = byChar;
   tmp[ 1 ] = '\0';

   /* Disable line wrap, set the new cursor position, send the string, then
      enable line wrap (for OUTSTD() and OUTERR() ) */
   HB_GT_FUNC(gt_AnsiSetAttributes( attr ));
   fprintf( stdout, "\x1B[=7l\x1B[%d;%dH%s\x1B[=7h", usRow + 1, usCol + 1, tmp );

   /* Restore whatever used to be at the termination position */
   /* Update the cursor position */
   s_usRow = usRow;
   s_usCol = usCol + 1;
   if( s_usCol > s_usMaxCol ) s_usCol = s_usMaxCol;
}

void HB_GT_FUNC(gt_Puts( USHORT usRow, USHORT usCol, BYTE attr, BYTE * str, ULONG len ))
{
   /* Because Clipper strings don't have to be null terminated, add a null
      terminating character after saving what used to be at the termination
      position, because it might not even be part of the string object */
   char save = str[ len ];
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu, %s)", usRow, usCol, (int) attr, str, len, str));
   str[ len ] = '\0';
   /* Disable line wrap, set the new cursor position, send the string, then
      enable line wrap (for OUTSTD() and OUTERR() ) */
   HB_GT_FUNC(gt_AnsiSetAttributes( attr ));
   fprintf( stdout, "\x1B[=7l\x1B[%d;%dH%s\x1B[=7h", usRow + 1, usCol + 1, str );
   /* Restore whatever used to be at the termination position */
   str[ len ] = save;
   /* Update the cursor position */
   s_usRow = usRow;
   s_usCol = usCol + ( USHORT )len;
   if( s_usCol > s_usMaxCol ) s_usCol = s_usMaxCol;
}

int HB_GT_FUNC(gt_RectSize( USHORT rows, USHORT cols ))
{
   return rows * cols * 2;
}

void HB_GT_FUNC(gt_GetText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE *dest ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRight, dest));
   HB_SYMBOL_UNUSED( usTop );
   HB_SYMBOL_UNUSED( usLeft );
   HB_SYMBOL_UNUSED( usBottom );
   HB_SYMBOL_UNUSED( usRight );
   HB_SYMBOL_UNUSED( dest );
   /* TODO: */
}

void HB_GT_FUNC(gt_PutText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE *src ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRight, src) );
   HB_SYMBOL_UNUSED( usTop );
   HB_SYMBOL_UNUSED( usLeft );
   HB_SYMBOL_UNUSED( usBottom );
   HB_SYMBOL_UNUSED( usRight );
   HB_SYMBOL_UNUSED( src );
   /* TODO: */
}

void HB_GT_FUNC(gt_SetAttribute( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", usTop, usLeft, usBottom, usRight, (int) attr) );
   HB_SYMBOL_UNUSED( usTop );
   HB_SYMBOL_UNUSED( usLeft );
   HB_SYMBOL_UNUSED( usBottom );
   HB_SYMBOL_UNUSED( usRight );
   HB_SYMBOL_UNUSED( attr );
   /* TODO: */
   s_iAttribute = attr;
}

void HB_GT_FUNC(gt_DispBegin( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

   ++s_uiDispCount;

   /* TODO: */
}

void HB_GT_FUNC(gt_DispEnd( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

   --s_uiDispCount;

   /* TODO: here we flush the buffer, and restore normal screen writes */
}

BOOL HB_GT_FUNC(gt_SetMode( USHORT uiRows, USHORT uiCols ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", uiRows, uiCols) );
   HB_SYMBOL_UNUSED( uiRows );
   HB_SYMBOL_UNUSED( uiCols );
   /* TODO: */
   return 0;   /* 0 = Ok, other = Fail */
}

BOOL HB_GT_FUNC(gt_GetBlink())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));
   /* TODO: */
   return 1;               /* 0 = blink, 1 = intens      */
}

void HB_GT_FUNC(gt_SetBlink( BOOL bBlink ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink) );
   HB_SYMBOL_UNUSED( bBlink );
   /* TODO: */
}

void HB_GT_FUNC(gt_Tone( double dFrequency, double dDuration ))
{
   BYTE szBell[] = { HB_CHAR_BEL, 0 };

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

   /* TODO: Implement this */

   HB_SYMBOL_UNUSED( dFrequency );
   HB_SYMBOL_UNUSED( dDuration );

   fprintf( stdout, szBell );
   fflush( stdout );
   hb_idleSleep( dDuration / 18.2 );
}

char * HB_GT_FUNC(gt_Version( void ))
{
   return "Harbour Terminal: PC ANSI";
}

USHORT HB_GT_FUNC(gt_DispCount())
{
   return s_uiDispCount;
}


void HB_GT_FUNC(gt_Replicate( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar, ULONG nLength ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", uiRow, uiCol, byAttr, byChar, nLength));

   {
      while( nLength-- )
         HB_GT_FUNC(gt_xPutch( uiRow, uiCol++, byAttr, byChar ));
   }
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
    hb_fsWriteLarge( s_iFilenoStdout, ( BYTE * ) pbyStr, ulLen );
}

void HB_GT_FUNC(gt_OutErr( BYTE * pbyStr, ULONG ulLen ))
{
    hb_fsWriteLarge( s_iFilenoStderr, ( BYTE * ) pbyStr, ulLen );
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
