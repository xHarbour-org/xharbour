 /*
 * $Id: gtstd.c,v 1.6 2003/05/21 09:35:37 druzus Exp $
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
#define HB_GT_NAME	STD

/* TODO: include any standard headers here */

#include "hbapifs.h"
#include "hbapigt.h"

#if defined( OS_UNIX_COMPATIBLE )
   #include <unistd.h>  /* read() function requires it */
   #include <termios.h>
#else
   #if defined(_MSC_VER)
      #include <io.h>
      #include <conio.h>
   #endif
#endif

/* Add time function for BEL flood throttling.. */

#include <time.h>
#if defined( HB_OS_BSD )
   #include <sys/time.h>
#elif defined( OS_UNIX_COMPATIBLE )
   #include <sys/timeb.h>
#else
   #include <sys\timeb.h>
#endif


static SHORT  s_iRow;
static SHORT  s_iCol;
static USHORT s_uiMaxRow;
static USHORT s_uiMaxCol;
static USHORT s_uiCursorStyle;
static BOOL   s_bBlink;
static int    s_iFilenoStdout;
static int    s_iFilenoStderr;
static USHORT s_uiDispCount;
static BYTE * s_szCrLf;
static ULONG  s_ulCrLf;

#if defined( OS_UNIX_COMPATIBLE )
   static struct termios startup_attributes;
#endif

#if defined(_MSC_VER)
   static BOOL s_bStdinConsole;
#endif


void HB_GT_FUNC(gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

   HB_SYMBOL_UNUSED( iFilenoStdin );
   //HB_SYMBOL_UNUSED( iFilenoStderr );
#if ! defined( HB_UNIX_GT_DAEMON )
#if defined( OS_UNIX_COMPATIBLE )
   {
      struct termios ta;

      tcgetattr( STDIN_FILENO, &startup_attributes );
/*      atexit( restore_input_mode ); */

      tcgetattr( STDIN_FILENO, &ta );
      ta.c_lflag &= ~( ICANON | ECHO );
      ta.c_iflag &= ~ICRNL;
      ta.c_cc[ VMIN ] = 0;
      ta.c_cc[ VTIME ] = 0;
      tcsetattr( STDIN_FILENO, TCSAFLUSH, &ta );
   }
#endif
#endif

#if defined(_MSC_VER)
   s_bStdinConsole = _isatty(0);
#endif

   s_uiDispCount = 0;

   s_iRow = 0;
   s_iCol = 0;

/* #if defined(OS_UNIX_COMPATIBLE) */
   s_uiMaxRow = 24;
   s_uiMaxCol = 80;
/*
#else
   s_uiMaxRow = 32767;
   s_uiMaxCol = 32767;
#endif */

   s_uiCursorStyle = SC_NORMAL;
   s_bBlink = FALSE;
   s_iFilenoStdout = iFilenoStdout;
   s_iFilenoStderr = iFilenoStderr;
   hb_fsSetDevMode( s_iFilenoStdout, FD_BINARY );

   s_szCrLf = (BYTE *) hb_conNewLine();
   s_ulCrLf = strlen( (char *) s_szCrLf );

#if ! defined( HB_UNIX_GT_DAEMON )
   HB_GT_FUNC(mouse_Init());
#endif
}

void HB_GT_FUNC(gt_Exit( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

#if ! defined( HB_UNIX_GT_DAEMON )

   HB_GT_FUNC(mouse_Exit());

#if defined( OS_UNIX_COMPATIBLE )
   tcsetattr( STDIN_FILENO, TCSANOW, &startup_attributes );
#endif

#endif

}

int HB_GT_FUNC(gt_ExtendedKeySupport())
{
   return 0;
}

static void out_stdout( BYTE * pStr, ULONG ulLen )
{
   unsigned uiErrorOld = hb_fsError(); /* Save current user file error code */
   hb_fsWriteLarge( s_iFilenoStdout, pStr, ulLen );
   hb_fsSetError( uiErrorOld );        /* Restore last user file error code */
}

static void out_newline( void )
{
   out_stdout( s_szCrLf, s_ulCrLf );
}

int HB_GT_FUNC(gt_ReadKey( HB_inkey_enum eventmask ))
{
   int ch = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

   HB_SYMBOL_UNUSED( eventmask );

#if defined(OS_UNIX_COMPATIBLE)
   if( ! read( STDIN_FILENO, &ch, 1 ) )
      ch = 0;
#else

   #if defined(_MSC_VER)
   if( s_bStdinConsole )
   {
      if( _kbhit() ) ch = _getch();
   }
   else
   {
      if(! _eof(0) ) _read(0, &ch, 1);
   }
   #endif

#endif

   /* TODO: */

   return ch;
}

/* Parse out a string to determine the new cursor position */

BOOL HB_GT_FUNC(gt_AdjustPos( BYTE * pStr, ULONG ulLen ))
{
   USHORT row = s_iRow;
   USHORT col = s_iCol;
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
               col = s_uiMaxCol;
               if( row )
                  row--;
            }
            break;

         case HB_CHAR_LF:
            if( row < s_uiMaxRow )
               row++;
            break;

         case HB_CHAR_CR:
            col = 0;
            break;

         default:
            if( col < s_uiMaxCol )
               col++;
            else
            {
               col = 0;
               if( row < s_uiMaxRow )
                  row++;
            }
      }
   }

   s_iRow = row;
   s_iCol = col;

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
   BYTE szBuffer[2] = { 0, 0 };

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", iRow, iCol, iMethod));

   if( iMethod == HB_GT_SET_POS_BEFORE )
   {
      /* Only set the screen position when the cursor
         position is changed BEFORE text is displayed.

         Updates to cursor position AFTER test output is handled
         within this driver itself
      */

      if( iRow != s_iRow )
      {
         /* always go to a newline, even if request to go to row above.
            Although can't actually do unward movement, at least start
            a new line to avoid possibly overwriting text already on
            the current row */

         out_newline();
         s_iCol = 0;
         if (s_iRow < iRow)
         {
            /* if requested to move down more than one row, do extra
              newlines to render the correct vertical distance */

            while( ++s_iRow < iRow )
               out_newline();
         }
      }

      /* Use space and backspace to adjust horizontal position.. */

      if( s_iCol < iCol )
      {
         szBuffer[0] = ' ';
         while( s_iCol++ < iCol )
            out_stdout( szBuffer, 1 );
      }
      else if( s_iCol > iCol )
      {
         szBuffer[0] = HB_CHAR_BS;
         while( s_iCol-- > iCol )
            out_stdout( szBuffer, 1 );
      }

      s_iRow = iRow;
      s_iCol = iCol;
   }
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
   BYTE szBuffer[ 2 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %i)", uiRow, uiCol, (int) byAttr, byAttr));

   HB_SYMBOL_UNUSED( byAttr );

   HB_GT_FUNC(gt_SetPos( uiRow, uiCol, HB_GT_SET_POS_BEFORE ));

   /* make the char into a string so it can be passed to AdjustPos
      as well as being output */

   szBuffer[ 0 ] = byChar;
   szBuffer[ 1 ] = 0;
   out_stdout( szBuffer, 1 );

   HB_GT_FUNC(gt_AdjustPos( szBuffer, 1 ));
}

void HB_GT_FUNC(gt_Puts( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE * pbyStr, ULONG ulLen ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", uiRow, uiCol, (int) byAttr, pbyStr, ulLen));

   HB_SYMBOL_UNUSED( byAttr );

   HB_GT_FUNC(gt_SetPos( uiRow, uiCol, HB_GT_SET_POS_BEFORE ));

   out_stdout( pbyStr, ulLen );

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

   HB_SYMBOL_UNUSED( byAttr );

   /* Provide some basic scroll support for full screen */

   if( uiTop == 0 &&
      uiBottom >= (s_uiMaxRow - 1 ) &&
      uiLeft == 0 &&
      uiRight >= (s_uiMaxCol - 1 ) )
   {

      if ( iRows == 0 && iCols == 0 )
      {
         /* clear screen request.. */

         for( ; uiBottom; uiBottom-- )
            out_newline();

         s_iRow = 0;
         s_iCol = 0;
      }
      else
      {
         /* no true scroll capability */
         /* but newline for each upward scroll */
         /* as gtapi.c will call scroll when on last row */
         /* and not change the row value itself */

         while( iRows-- > 0 )
            out_newline();

         s_iCol = 0;
      }
   }
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

static int gtstd_get_seconds( void )
{
#if defined(_MSC_VER)
   #define timeb _timeb
   #define ftime _ftime
#endif
#if defined(HB_OS_BSD)
   struct timeval oTime;
   struct timezone oZone;
   gettimeofday( &oTime, &oZone );
   return ( oTime.tv_sec );
#else
   struct timeb tb;
   struct tm * oTime;

   ftime( &tb );
   oTime = localtime( &tb.time );

   return ( (int) oTime->tm_sec );
#endif
}

void HB_GT_FUNC(gt_Tone( double dFrequency, double dDuration ))
{
   BYTE szBell[] = { HB_CHAR_BEL, 0 };
   static int iSinceBell = -1;
   int iNow;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

   HB_SYMBOL_UNUSED( dFrequency );
   HB_SYMBOL_UNUSED( dDuration );

   /* Output an ASCII BEL character to cause a sound */
   /* but throttle to max once per second, in case of sound */
   /* effects prgs calling lots of short tone sequences in */
   /* succession leading to BEL hell on the terminal */

   iNow = gtstd_get_seconds();

   if ( iNow != iSinceBell )
      out_stdout( szBell, 1 );

   iSinceBell = iNow;
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
               if( ( Width > 1 || Left < 0 ) && Right < HB_GT_FUNC(gt_GetScreenWidth() ))
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
