/*
 * $Id: gtalleg.c,v 1.14 2004/02/01 23:40:49 jonnymind Exp $
 */

/*
* xHarbour Project source code:
* Allegro based virtual gt with graphic extensions.
*
* Copyright 2004 Mauricio Abre <maurifull@datafull.com>
* www - http://www.xharbour.org
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

#define HB_GT_NAME      ALLEG

#include <allegro.h>
#include "ssf.h"

#include "hbapi.h"
#include "hbapigt.h"
#include "hbapifs.h"
#include "hbset.h"
#include "hbvm.h"
#include "inkey.ch"

static int s_iStdIn, s_iStdOut, s_iStdErr;
static int s_iMsButtons, s_iMSBoundTop, s_iMSBoundLeft, s_iMSBoundBottom, s_iMSBoundRight;
static int s_iMSX, s_iMSY;
static BYTE s_byMSButtons;
static USHORT s_usScrWidth = 80, s_usScrHeight = 25;
static USHORT s_usGFXWidth = 0, s_usGFXHeight = 0;
static USHORT s_usUpdTop, s_usUpdLeft, s_usUpdBottom, s_usUpdRight;
static USHORT s_usDispCount, s_usCursorStyle;
static SHORT s_sCurCol, s_sCurRow;
static BYTE * s_pbyScrBuffer = NULL;
static int s_pClr[16];
static BYTE s_byFontSize = 16, s_byFontWidth = 8;
static AL_BITMAP *bmp;
BOOL lClearInit = TRUE;

static void hb_gt_DoCursor( void );

// I'm not sure of removing these (yet)
// (they used to be static vars to center gt in hw screen, but now the
//  font size is set based on screen size, so gtAlleg will use about all screen)
//
// NOTE: This is only a Linux fb & DOS issue, where we don't have windows
#define s_usHBorder 0
#define s_usVBorder 0

typedef struct {
   char al_key;
   int xhb_key;
} gtAllegKey;

#define GT_KEY_TABLE_SIZE 49

static const gtAllegKey sKeyTable[GT_KEY_TABLE_SIZE] = {
   {AL_KEY_ESC,    K_ESC},
   {AL_KEY_INSERT, K_INS},
   {AL_KEY_HOME,   K_HOME},
   {AL_KEY_PGUP,   K_PGUP},
   {AL_KEY_PGDN,   K_PGDN},
   {AL_KEY_END,    K_END},
   {AL_KEY_DEL,    K_DEL},
   {AL_KEY_UP,     K_UP},
   {AL_KEY_DOWN,   K_DOWN},
   {AL_KEY_LEFT,   K_LEFT},
   {AL_KEY_RIGHT,  K_RIGHT},
   {AL_KEY_A,      K_ALT_A},
   {AL_KEY_B,      K_ALT_B},
   {AL_KEY_C,      K_ALT_C},
   {AL_KEY_D,      K_ALT_D},
   {AL_KEY_E,      K_ALT_E},
   {AL_KEY_F,      K_ALT_F},
   {AL_KEY_G,      K_ALT_G},
   {AL_KEY_H,      K_ALT_H},
   {AL_KEY_I,      K_ALT_I},
   {AL_KEY_J,      K_ALT_J},
   {AL_KEY_K,      K_ALT_K},
   {AL_KEY_L,      K_ALT_L},
   {AL_KEY_M,      K_ALT_M},
   {AL_KEY_N,      K_ALT_N},
   {AL_KEY_O,      K_ALT_O},
   {AL_KEY_P,      K_ALT_P},
   {AL_KEY_Q,      K_ALT_Q},
   {AL_KEY_R,      K_ALT_R},
   {AL_KEY_S,      K_ALT_S},
   {AL_KEY_T,      K_ALT_T},
   {AL_KEY_U,      K_ALT_U},
   {AL_KEY_V,      K_ALT_V},
   {AL_KEY_W,      K_ALT_W},
   {AL_KEY_X,      K_ALT_X},
   {AL_KEY_Y,      K_ALT_Y},
   {AL_KEY_Z,      K_ALT_Z},
   {AL_KEY_F1,     K_F1},
   {AL_KEY_F2,     K_F2},
   {AL_KEY_F3,     K_F3},
   {AL_KEY_F4,     K_F4},
   {AL_KEY_F5,     K_F5},
   {AL_KEY_F6,     K_F6},
   {AL_KEY_F7,     K_F7},
   {AL_KEY_F8,     K_F8},
   {AL_KEY_F9,     K_F9},
   {AL_KEY_F10,    K_F10},
   {AL_KEY_F11,    K_F11},
   {AL_KEY_F12,    K_F12}
};

#define GT_UPD_RECT(t,l,b,r) if (t<s_usUpdTop) s_usUpdTop=t; if (l<s_usUpdLeft) s_usUpdLeft=l; if (b>s_usUpdBottom) s_usUpdBottom=b; if (r>s_usUpdRight) s_usUpdRight=r;
#define MK_GT8BCOLOR(n) (n & 0xFF) / 16 | (n & 0xFF00) / 256

/*
* We don't have a cursor in gfx mode, so I have to emulate it :-)
* Cursor is drawn with in XOR method, so redrawing it erases previous one
*
*/
static void hb_gt_DoCursor()
{
   static BOOL s_bVisible = 0;
   int iLeft, iTop, iBottom, iRight;

   if ( !s_bVisible && s_usDispCount == 0 )
   {
      s_bVisible = 1;
   }

   if ( s_bVisible && ( s_pbyScrBuffer != NULL ) )
   {
      iLeft = s_usHBorder + s_sCurCol * s_byFontWidth;
      iRight = iLeft;
      iTop = s_usVBorder + s_sCurRow * s_byFontSize;
      iBottom = iTop;

      switch ( s_usCursorStyle )
      {
         case SC_NORMAL:
            iBottom += s_byFontSize - 1;
            iTop = iBottom - 1;
            iRight += s_byFontWidth;
            break;

         case SC_INSERT:
            iBottom += s_byFontSize - 1;
            iTop = iBottom - ( s_byFontSize / 2 ) + 1;
            iRight += s_byFontWidth;
            break;

         case SC_SPECIAL1:
            iBottom += s_byFontSize - 1;
            iRight += s_byFontWidth;
            break;

         case SC_SPECIAL2:
            iBottom += ( s_byFontSize / 2 ) - 1;
            iRight += s_byFontWidth;
            break;
      }

      if ( iRight > iLeft )  // cursor != SC_NONE
      {
         al_scare_mouse_area( iLeft, iTop, iRight, iBottom );
         al_acquire_screen();
         al_drawing_mode( DRAW_MODE_XOR, NULL, 0, 0 );
         al_draw_rect_fill( al_screen, iLeft, iTop, iRight, iBottom, s_pClr[7]);
         al_drawing_mode( DRAW_MODE_SOLID, NULL, 0, 0 );
         al_release_screen();
         al_unscare_mouse();
      }
   }

   if ( s_bVisible && s_usDispCount > 1 )
   {
   s_bVisible = FALSE;  // prevent cursor flicker on buffered screen i/o
   }
}

void HB_GT_FUNC(gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr ))
{
   int iRet;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

   s_iStdIn = iFilenoStdin;
   s_iStdOut = iFilenoStdout;
   s_iStdErr = iFilenoStderr;
   s_usCursorStyle = SC_NORMAL;

   allegro_init();

   iRet = al_desktop_color_depth();

   if ( iRet > 0 )
   {
      al_set_color_depth(iRet);
//  setting depth to 16 defaults to a 320x200x8bpp under DOS if no matching mode :(
//   } else
//   {
//      al_set_color_depth(16);
   }
}

void HB_GT_FUNC(gt_Exit( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

   if ( s_pbyScrBuffer != NULL )
   {
      hb_xfree( s_pbyScrBuffer );
      s_pbyScrBuffer = NULL;
      al_destroy_bitmap(bmp);
   }
}


USHORT HB_GT_FUNC(gt_GetScreenWidth( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

   return s_usScrWidth;
}

USHORT HB_GT_FUNC(gt_GetScreenHeight( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

   return s_usScrHeight;
}

SHORT HB_GT_FUNC(gt_Col( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

   return s_sCurCol;
}

SHORT HB_GT_FUNC(gt_Row( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

   return s_sCurRow;
}

void HB_GT_FUNC(gt_SetPos( SHORT sRow, SHORT sCol, SHORT sMethod ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", sRow, sCol, sMethod));

   HB_SYMBOL_UNUSED( sMethod );

   hb_gt_DoCursor();  // hide cursor
   s_sCurRow = sRow;
   s_sCurCol = sCol;
   hb_gt_DoCursor();  // draw it at new location
}

BOOL HB_GT_FUNC(gt_AdjustPos( BYTE *pStr, ULONG ulLen ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen));

   HB_SYMBOL_UNUSED( pStr );
   HB_SYMBOL_UNUSED( ulLen);

   return 1;
}

BOOL HB_GT_FUNC(gt_IsColor( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

   return 1;
}

USHORT HB_GT_FUNC(gt_GetCursorStyle( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

   return s_usCursorStyle;
}

void HB_GT_FUNC(gt_SetCursorStyle( USHORT usStyle ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", usStyle));

   if ( usStyle != s_usCursorStyle )  // don't do unnecessary stuff
   {
      hb_gt_DoCursor();  // hide actual cursor
      switch ( usStyle )
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
      hb_gt_DoCursor();  // show new cursor
   }
}

static void HB_GT_FUNC(gt_ScreenUpdate( void ))
{
   USHORT x, y, z;
   BYTE byAttr, byChar;
   HB_GT_GOBJECT *gobject;
   int gcolor;
   char gtext[256];

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ScreenUpdate()"));

   if ( s_usDispCount == 0 && s_usUpdTop <= s_usUpdBottom && s_usUpdLeft <= s_usUpdRight )
   {
      al_acquire_bitmap( bmp );
      for ( y = s_usUpdTop; y <= s_usUpdBottom; y++ )
      {
         z = y * s_usScrWidth * 2 + s_usUpdLeft * 2;
         for ( x = s_usUpdLeft; x <= s_usUpdRight; x++ )
         {
            byChar = s_pbyScrBuffer[z++];
            byAttr = s_pbyScrBuffer[z++];
            al_draw_rect_fill( bmp,  x * s_byFontWidth, y * s_byFontSize, x * s_byFontWidth + s_byFontWidth - 1, y * s_byFontSize + s_byFontSize - 1, s_pClr[byAttr >> 4] );
            ssfDrawChar( bmp, ssfDefaultFont, byChar, x * s_byFontWidth, y * s_byFontSize, s_pClr[byAttr & 0x0F] );
         }
      }

      s_usUpdLeft *= s_byFontWidth;
      s_usUpdTop *= s_byFontSize;
      s_usUpdRight = s_usUpdRight * s_byFontWidth + s_byFontWidth - 1;
      s_usUpdBottom = s_usUpdBottom * s_byFontSize + s_byFontSize - 1;

      if ( hb_gt_gobjects )
      {
         gobject = hb_gt_gobjects;

         while ( gobject )
         {
            gcolor = al_make_color( MK_GT8BCOLOR(gobject->color.usRed), MK_GT8BCOLOR(gobject->color.usGreen), MK_GT8BCOLOR(gobject->color.usBlue) );
            s_usUpdLeft = MIN(s_usUpdLeft,gobject->x);
            s_usUpdTop = MIN(s_usUpdTop,gobject->y);

            switch ( gobject->type )
            {
               case GTO_POINT:
                  al_put_pixel( bmp, gobject->x, gobject->y, gcolor );
                  s_usUpdRight = MAX(s_usUpdRight,gobject->x);
                  s_usUpdBottom = MAX(s_usUpdBottom,gobject->y);
                  break;

               case GTO_LINE:
                  al_draw_line( bmp, gobject->x, gobject->y, gobject->width, gobject->height, gcolor );
                  if ( gobject->x > gobject->width )
                  {
                     s_usUpdLeft = MIN(s_usUpdLeft,gobject->width);
                     s_usUpdTop = MIN(s_usUpdTop,gobject->height);
                     s_usUpdRight = MAX(s_usUpdRight,gobject->x);
                     s_usUpdBottom = MAX(s_usUpdBottom,gobject->y);
                  }
                  else
                  {
                     s_usUpdRight = MAX(s_usUpdRight,gobject->width);
                     s_usUpdBottom = MAX(s_usUpdBottom,gobject->height);
                  }
                  break;

               case GTO_SQUARE:
                  al_draw_rect( bmp, gobject->x, gobject->y, gobject->x + gobject->width - 1, gobject->y + gobject->height - 1, gcolor );
                  s_usUpdRight = MAX(s_usUpdRight,gobject->x+gobject->width-1);
                  s_usUpdBottom = MAX(s_usUpdBottom,gobject->y+gobject->height-1);
                  break;

               case GTO_RECTANGLE:
                  al_draw_rect_fill( bmp, gobject->x, gobject->y, gobject->x + gobject->width - 1, gobject->y + gobject->height - 1, gcolor );
                  s_usUpdRight = MAX(s_usUpdRight,gobject->x+gobject->width-1);
                  s_usUpdBottom = MAX(s_usUpdBottom,gobject->y+gobject->height-1);
                  break;

               case GTO_CIRCLE:
// Should be ellipses, otherwise they'll not fill entire requested area
// But I leaved circles to match its name
//                 al_draw_ellipse( al_screen, gobject->x, gobject->y, gobject->width / 2, gobject->height / 2, al_make_color( gcolor.usRed, gcolor.usGreen, gcolor.usBlue ) );
                  al_draw_circle( bmp, gobject->x + gobject->width / 2, gobject->y + gobject->height / 2, gobject->width / 2 - 1, gcolor );
                  s_usUpdRight = MAX(s_usUpdRight,gobject->x+gobject->width-1);
                  s_usUpdBottom = MAX(s_usUpdBottom,gobject->y+gobject->height-1);
                  break;

               case GTO_DISK:
                  al_draw_circle_fill( bmp, gobject->x + gobject->width / 2, gobject->y + gobject->height / 2, gobject->width / 2 - 1, gcolor );
                  s_usUpdRight = MAX(s_usUpdRight,gobject->x+gobject->width-1);
                  s_usUpdBottom = MAX(s_usUpdBottom,gobject->y+gobject->height-1);
                  break;

               case GTO_TEXT:
                  memcpy( gtext, gobject->data, gobject->data_len );
                  gtext[gobject->data_len] = '\0';
// Commented out until GtText() font width & height support is added
//                if ( gobject->height != 0 )
//                {
//                   ssfDefaultFont->fsize = (unsigned short) gobject->height;
//                }
                  ssfDrawText( bmp, ssfDefaultFont, gtext, gobject->x, gobject->y, gcolor );
                  s_usUpdRight = MAX(s_usUpdRight,gobject->x+gobject->data_len*(ssfDefaultFont->fsize/2)-1);
                  s_usUpdBottom = MAX(s_usUpdBottom,gobject->y+ssfDefaultFont->fsize-1);
//                if ( gobject->height != 0 )
//                {
//                   ssfDefaultFont->fsize = (unsigned short) s_byFontSize;
//                }
                  break;

               default:
                  break;
            }
            gobject = gobject->next;
         }
      }

      al_acquire_screen();
      al_scare_mouse_area(s_usUpdLeft, s_usUpdTop, s_usUpdRight, s_usUpdBottom);
      al_blit(bmp, al_screen, s_usUpdLeft, s_usUpdTop, s_usUpdLeft, s_usUpdTop, s_usUpdRight - s_usUpdLeft + 1, s_usUpdBottom - s_usUpdTop + 1);
      al_release_bitmap(bmp);
      al_release_screen();
      al_unscare_mouse();

      if ( s_sCurCol * s_byFontWidth >= s_usUpdLeft &&
         s_sCurCol * s_byFontWidth <= s_usUpdRight &&
         s_sCurRow * s_byFontSize >= s_usUpdTop &&
         s_sCurRow * s_byFontSize <= s_usUpdBottom )
      {
         hb_gt_DoCursor();
      }

      s_usUpdTop = s_usScrHeight;
      s_usUpdLeft = s_usScrWidth;
      s_usUpdBottom = 0;
      s_usUpdRight = 0;
   }
}

void HB_GT_FUNC(gt_DispBegin( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

   if ( s_pbyScrBuffer == NULL )
   {
      HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   s_usDispCount++;
}

void HB_GT_FUNC(gt_DispEnd( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

   if ( s_pbyScrBuffer == NULL )
   {
      HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   if ( s_usDispCount > 0 )
   {
      s_usDispCount--;
      if ( s_usDispCount == 0 )
      {
         HB_GT_FUNC(gt_ScreenUpdate());
      }
   }
}

USHORT HB_GT_FUNC(gt_DispCount( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispCount()"));

   return s_usDispCount;
}

BOOL HB_GT_FUNC(gt_GetBlink( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

   return FALSE;
}

void HB_GT_FUNC(gt_SetBlink( BOOL bBlink ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink()"));

   HB_SYMBOL_UNUSED( bBlink );
}

int HB_GT_FUNC(gt_RectSize( USHORT usRows, USHORT usCols ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_RectSize(%hu, %hu)", usRows, usCols));

   return usRows * usCols * 2;
}

char * HB_GT_FUNC(gt_Version( void ))
{
   return "Harbour Terminal: Multiplatform Allegro graphics console";
}

BOOL HB_GT_FUNC(gt_Suspend())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Suspend()"));

   return 1;
}

BOOL HB_GT_FUNC(gt_Resume())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Resume()"));

   return 1;
}

BOOL HB_GT_FUNC(gt_PreExt())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PreExt()"));

   return 1;
}

BOOL HB_GT_FUNC(gt_PostExt())
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PostExt()"));

   return 1;
}

void HB_GT_FUNC(gt_OutStd( BYTE * pbyStr, ULONG ulLen ))
{
   hb_fsWriteLarge( s_iStdOut, ( BYTE * ) pbyStr, ulLen );
}

void HB_GT_FUNC(gt_OutErr( BYTE * pbyStr, ULONG ulLen ))
{
   hb_fsWriteLarge( s_iStdErr, ( BYTE * ) pbyStr, ulLen );
}

int HB_GT_FUNC(gt_ExtendedKeySupport( void ))
{
   return 1;
}

void HB_GT_FUNC(gt_Puts( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE *pbyStr, ULONG ulLen ))
{
   USHORT i, j, k;
   USHORT uL = usCol + ulLen, uR = usCol;
   BOOL lUpd = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", usRow, usCol, (int) byAttr, pbyStr, ulLen));

   if ( s_pbyScrBuffer == NULL )
   {
      HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   if ( usRow < s_usScrHeight )
   {
      j = (USHORT) ulLen;

      if ( usCol + j > s_usScrWidth )
      {
         j = s_usScrWidth - usCol;
      }

      k = usRow * s_usScrWidth * 2 + usCol * 2;

      for ( i = 0; i < j; i++ )
      {
         if ( ( s_pbyScrBuffer[k++] != pbyStr[i] ) | ( s_pbyScrBuffer[k++] != byAttr ) )
         {
            s_pbyScrBuffer[k - 2] = pbyStr[i];
            s_pbyScrBuffer[k - 1] = byAttr;
            lUpd = TRUE;
            uL = MIN(uL,usCol+i);
            uR = MAX(uR,usCol+i);
         }
      }

      if ( lUpd )
      {
         GT_UPD_RECT(usRow,uL,usRow,uR);
         HB_GT_FUNC(gt_ScreenUpdate());
      }
   }
}

void HB_GT_FUNC(gt_Replicate( USHORT usRow, USHORT usCol, BYTE byAttr, BYTE byChar, ULONG ulLen ))
{
   int i, l;
   BYTE pbyBuf[256];

   HB_TRACE(HB_TR_DEBUG, ("gt_Replicate(%hu, %hu, %d, %c, %lu)", usRow, usCol, (int) byAttr, (char) byChar, ulLen));

   l = (int) MIN(ulLen+1,256);

   for ( i = 0; i < l; i++ )
   {
      pbyBuf[i] = byChar;
   }

   pbyBuf[i] = '\0';
   l--;
   HB_GT_FUNC(gt_Puts( usRow, usCol, byAttr, (BYTE *) pbyBuf, (ULONG) l));
}

void HB_GT_FUNC(gt_GetText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE *pbyDst ))
{
   USHORT x, y, z;

   HB_TRACE(HB_TR_DEBUG, ("gt_GetText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRight, pbyDst));

   if ( s_pbyScrBuffer == NULL )
   {
      HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   if ( usTop > usBottom )
   {
      x = usTop;
      usTop = usBottom;
      usBottom = x;
   }

   if ( usLeft > usRight )
   {
      x = usLeft;
      usLeft = usRight;
      usRight = x;
   }

   if ( usBottom >= s_usScrHeight )
   {
      usBottom = s_usScrHeight - 1;
   }

   if ( usRight >= s_usScrWidth )
   {
      usRight = s_usScrWidth - 1;
   }

   if ( usTop < s_usScrHeight && usLeft < s_usScrWidth )
   {
      for ( y = usTop; y <= usBottom; y++ )
      {
         z = y * s_usScrWidth * 2 + usLeft * 2;

         for ( x = usLeft; x <= usRight; x++ )
         {
            *(pbyDst++) = s_pbyScrBuffer[z++];
            *(pbyDst++) = s_pbyScrBuffer[z++];
         }
      }
   }
}

void HB_GT_FUNC(gt_PutText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE *pbySrc ))
{
   USHORT x, y, z;
   USHORT uT, uL, uB, uR;
   BOOL lUpd = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("gt_PutText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRight, pbySrc));

   if ( s_pbyScrBuffer == NULL )
   {
      HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   // coord check
   if ( usTop > usBottom )
   {
      x = usTop;
      usTop = usBottom;
      usBottom = x;
   }

   if ( usLeft > usRight )
   {
      x = usLeft;
      usLeft = usRight;
      usRight = x;
   }

   uT = usBottom;
   uL = usRight;
   uB = usTop;
   uR = usLeft;

   if ( usBottom >= s_usScrHeight )
   {
      usBottom = s_usScrHeight - 1;
   }

   if ( usRight >= s_usScrWidth )
   {
      usRight = s_usScrWidth - 1;
   }

   if ( usTop < s_usScrHeight && usLeft < s_usScrWidth )
   {
      for ( y = usTop; y <= usBottom; y++ )
      {
         z = y * s_usScrWidth * 2 + usLeft * 2;
         for ( x = usLeft; x <= usRight; x++ )
         {
            if ( ( s_pbyScrBuffer[z++] != *(pbySrc++) ) | ( s_pbyScrBuffer[z++] != *(pbySrc++) ) )
            {
               s_pbyScrBuffer[z-1] = *(pbySrc-1);
               s_pbyScrBuffer[z-2] = *(pbySrc-2);
               lUpd = TRUE;
               uL = MIN(uL,x);
               uT = MIN(uT,y);
               uR = MAX(uR,x);
               uB = MAX(uB,y);
            }
         }
      }

      if ( lUpd )
      {
         GT_UPD_RECT(uT,uL,uB,uR);
         HB_GT_FUNC(gt_ScreenUpdate());
      }
   }
}

void HB_GT_FUNC(gt_SetAttribute( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE byAttr ))
{
   USHORT x, y, z;

   HB_TRACE(HB_TR_DEBUG, ("gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", usTop, usLeft, usBottom, usRight, (int) byAttr));

   if ( s_pbyScrBuffer == NULL )
   {
      HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   if ( usTop > usBottom )
   {
      x = usTop;
      usTop = usBottom;
      usBottom = x;
   }

   if ( usLeft > usRight )
   {
      x = usLeft;
      usLeft = usRight;
      usRight = x;
   }

   if ( usBottom >= s_usScrHeight )
   {
      usBottom = s_usScrHeight - 1;
   }

   if ( usRight >= s_usScrWidth )
   {
      usRight = s_usScrWidth - 1;
   }

   if ( usTop < s_usScrHeight && usLeft < s_usScrWidth )
   {
      for ( y = usTop; y <= usBottom; y++ )
      {
         z = y * s_usScrWidth * 2 + usLeft * 2;

         for ( x = usLeft; x <= usRight; x++ )
         {
            if ( s_pbyScrBuffer[++z] != byAttr )
            {
               s_pbyScrBuffer[z] = byAttr;
            }
         }
      }

      GT_UPD_RECT(usTop,usLeft,usBottom,usRight);
      HB_GT_FUNC(gt_ScreenUpdate());

   }
}

void HB_GT_FUNC(gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE byAttr, SHORT iRows, SHORT iCols ))
{

   USHORT usT, usL, usB, usR, i;
   BYTE *pbyScr;
   HB_GT_GOBJECT *gobject;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRight, (int) byAttr, iRows, iCols));

   if ( s_pbyScrBuffer == NULL )
   {
      HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   if ( usTop > usBottom )
   {
      usT = usTop;
      usTop = usBottom;
      usBottom = usT;
   }

   if ( usLeft > usRight )
   {
      usT = usLeft;
      usLeft = usRight;
      usRight = usT;
   }

   if ( usTop < s_usScrHeight && usLeft < s_usScrWidth )
   {
   HB_GT_FUNC(gt_DispBegin());
   if ( ( iRows != 0 ) | ( iCols != 0 ) )
   {
      if ( iRows < 0 )
      {
         usT = usTop;
         usB = usBottom + iRows;
      }
      else
      {
         usT = usTop + iRows;
         usB = usBottom;
      }

      if ( iCols < 0 )
      {
         usL = usLeft;
         usR = usRight + iCols;
      }
      else
      {
         usL = usLeft + iCols;
         usR = usRight;
      }

      pbyScr = (BYTE *) hb_xgrab(HB_GT_FUNC(gt_RectSize(usB - usT + 1, usR - usL + 1)));
      HB_GT_FUNC(gt_GetText(usT, usL, usB, usR, pbyScr));
      HB_GT_FUNC(gt_PutText(usT - iRows, usL - iCols, usB - iRows, usR - iCols, pbyScr));
      hb_xfree(pbyScr);

      if ( iRows < 0 )
      {
         usT = usB;
         usB = usT - iRows;
         usTop = MAX(usTop-iRows,0);
      }
      else
      {
         usB = usT;
         usT = usB - iRows;
         usBottom = MIN(usBottom - iRows,s_usScrHeight-1);
      }

      if ( iCols < 0 )
      {
         usL = usR;
         usR = usL - iCols;
         usLeft = MAX(usLeft-iCols,0);
      }
      else
      {
         usR = usL;
         usL = usR - iCols;
         usRight = MIN(usRight-iCols,s_usScrWidth-1);
      }
   }
   else
   {
      usT = usTop;
      usL = usLeft;
      usB = usBottom + 1;
      usR = usRight;
   }

   for ( i = usT; i < usB ; i++ )
   {
      HB_GT_FUNC(gt_Replicate(i, usL, byAttr, ' ', usR - usL + 1));
   }

   HB_GT_FUNC(gt_DispEnd());

   if ( hb_gt_gobjects )
   {
      gobject = hb_gt_gobjects;

      while ( gobject )
      {
         usLeft = MIN(usLeft,gobject->x/s_byFontWidth);
         usTop = MIN(usTop,gobject->x/s_byFontSize);
         if ( ( gobject->type == GTO_SQUARE ) | ( gobject->type == GTO_RECTANGLE ) )
         {
            usRight = MAX(usRight,usLeft+gobject->width/s_byFontWidth);
            usBottom = MAX(usBottom,usTop+gobject->height/s_byFontSize);
         }
         else
         {
            usRight = MAX(usRight,gobject->width/s_byFontWidth+s_byFontWidth-1);
            usBottom = MAX(usBottom,gobject->height/s_byFontSize+s_byFontSize-1);
         }

         gobject = gobject->next;
      }
   }

   GT_UPD_RECT(usTop,usLeft,usBottom,usRight);
   HB_GT_FUNC(gt_ScreenUpdate());
   }
}

BOOL HB_GT_FUNC(gt_SetMode( USHORT usRows, USHORT usCols ))
{
   PHB_FNAME pFileName;
   int iRet = 1, iWidth, iHeight;  // Don't remove iRet, ixFP and iyFP initializers!
   short ixFP = 0, iyFP = 0;
   BOOL lMode = FALSE, lPrev = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", usRows, usCols ));

   if ( s_pbyScrBuffer != NULL )
   {
      if ( lClearInit )
      {
         hb_xfree( s_pbyScrBuffer );
         s_pbyScrBuffer = NULL;
      }
      al_destroy_bitmap(bmp);
      lPrev = TRUE;
   }

   if ( ( s_usGFXWidth != 0 ) && ( s_usGFXHeight != 0 ) )
   {
   iWidth = (int) s_usGFXWidth;
   iHeight = (int) s_usGFXHeight;
   } else
   {
   iWidth = s_byFontWidth * usCols;
   iHeight = s_byFontSize * usRows;
   }
   if ( usRows > 11 && usCols > 23 && usRows < 129 && usCols < 257 )
   {
#if defined(AL_GFX_XWINDOWS)
      HB_TRACE(HB_TR_DEBUG, ("trying X DGA2 mode"));
      iRet = al_set_gfx_mode( AL_GFX_XDGA2, iWidth, iHeight, 0, 0 );
      if ( iRet != 0 )
      {
         HB_TRACE(HB_TR_DEBUG, ("trying X DGA mode"));
         iRet = al_set_gfx_mode( AL_GFX_XDGA, iWidth, iHeight, 0, 0 );
      }
      if ( iRet != 0 )
      {
         HB_TRACE(HB_TR_DEBUG, ("trying X Windows mode"));
         iRet = al_set_gfx_mode( AL_GFX_XWINDOWS, iWidth, iHeight, 0, 0 );
      }
#endif
#if defined (ALLEGRO_UNIX) | defined(ALLEGRO_LINUX) | defined(ALLEGRO_DOS)
      if ( iRet != 0 )
      {
         HB_TRACE(HB_TR_DEBUG, ("trying VBE/AF mode"));
         iRet = al_set_gfx_mode( AL_GFX_VBEAF, iWidth, iHeight, 0, 0 );
      }
#endif
#if defined(ALLEGRO_UNIX) | defined(ALLEGRO_LINUX)
      if ( iRet != 0 )
      {
         HB_TRACE(HB_TR_DEBUG, ("trying fb console mode"));
         iRet = al_set_gfx_mode( AL_GFX_FBCON, iWidth, iHeight, 0, 0 );
      }
#endif
      // Trying safe (slower) modes
      // Try a windowed mode first
      if ( iRet != 0 )
      {
         HB_TRACE(HB_TR_DEBUG, ("trying autodetect windowed mode"));
         iRet = al_set_gfx_mode( AL_GFX_AUTODETECT_WINDOWED, iWidth, iHeight, 0, 0 );
      }
#ifdef ALLEGRO_WINDOWS
      // GDI is slower, but it is more likely to bring a windowed mode than DirectX
      if ( iRet != 0 )
      {
         HB_TRACE(HB_TR_DEBUG, ("trying GDI windowed mode"));
         iRet = al_set_gfx_mode( AL_GFX_GDI, iWidth, iHeight, 0, 0 );
      }
      if ( iRet != 0 )
      {
         HB_TRACE(HB_TR_DEBUG, ("trying DirectX windowed mode"));
         iRet = al_set_gfx_mode( AL_GFX_DIRECTX_WIN, iWidth, iHeight, 0, 0 );
      }
#endif
      if ( iRet != 0 )
      {
         HB_TRACE(HB_TR_DEBUG, ("trying autodetect console mode"));
         iRet = al_set_gfx_mode( AL_GFX_AUTODETECT, iWidth, iHeight, 0, 0 );
      }
      if ( iRet != 0 )
      {
      /* If that fails (ie, plain DOS or Linux VESA Framebuffer)
         ensure to get any available gfx mode */
         HB_TRACE(HB_TR_DEBUG, ("trying safe mode"));
         iRet = al_set_gfx_mode(AL_GFX_SAFE, iWidth, iHeight, 0, 0 );
      }
      if ( iRet != 0 )  // Doh!
      {
         if ( lPrev )
         {
            usCols = s_usScrWidth;
            usRows = s_usScrHeight;
         }
         else
         {
            printf("gtAlleg FATAL: could not switch to graphic mode.\n");
            exit(1);
         }
      }
      else
      {
         lMode = TRUE;
      }

      pFileName = hb_fsFNameSplit(hb_cmdargARGV()[0]);
      al_set_window_title(pFileName->szName);

      if ( !lPrev )
      {
         al_install_timer();
         al_install_keyboard();
         s_iMsButtons = al_install_mouse();
      }
      s_iMSBoundLeft = 0;
      s_iMSBoundTop = 0;
      s_iMSBoundRight = AL_SCREEN_W - 1;
      s_iMSBoundBottom = AL_SCREEN_H - 1;
      s_byMSButtons = (BYTE) al_mouse_b;
      al_show_mouse(al_screen);
      s_usScrWidth = usCols;
      s_usScrHeight = usRows;

      // WAS: Center console in screen if we got a larger resolution than requested
      // NOW: Calculate proper font size
      // eg: Linux vesafb (doesn't support mode switching)
      //     or for DOS, we'll mostly request unavailable resolutions
      if ( AL_SCREEN_W != s_byFontWidth * s_usScrWidth )
      {
         ixFP = (BYTE) (AL_SCREEN_W / s_usScrWidth) * 2;
      }

      if ( AL_SCREEN_H != s_byFontSize * s_usScrHeight )
      {
         iyFP = (BYTE) (AL_SCREEN_H / s_usScrHeight);
         if ( iyFP % 2 == 1 )
         {
            iyFP--;
         }
      }

      if ( ixFP | iyFP )
      {
         if ( !ixFP )
         {
            ixFP = iyFP;
         }
         if ( !iyFP )
         {
            iyFP = ixFP;
         }
         s_byFontSize = ( ixFP < iyFP ? ixFP : iyFP );
         s_byFontWidth = s_byFontSize / 2;
      }

      s_iMSX = al_mouse_x / s_byFontWidth;
      s_iMSY = al_mouse_y / s_byFontSize;
      s_usUpdTop = s_usScrHeight;
      s_usUpdLeft = s_usScrWidth;
      s_usUpdBottom = 0;
      s_usUpdRight = 0;
      s_sCurCol = 0;
      s_sCurRow = 0;
      s_usDispCount = 0;
      ssfSetFontSize(ssfDefaultFont, s_byFontSize);
      s_pClr[ 0] = al_make_color(0x00, 0x00, 0x00);  // black
      s_pClr[ 1] = al_make_color(0x00, 0x00, 0xAA);  // blue
      s_pClr[ 2] = al_make_color(0x00, 0xAA, 0x00);  // green
      s_pClr[ 3] = al_make_color(0x00, 0xAA, 0xAA);  // cyan
      s_pClr[ 4] = al_make_color(0xAA, 0x00, 0x00);  // red
      s_pClr[ 5] = al_make_color(0xAA, 0x00, 0xAA);  // magenta
      s_pClr[ 6] = al_make_color(0xAA, 0x55, 0x00);  // brown
      s_pClr[ 7] = al_make_color(0xAA, 0xAA, 0xAA);  // white
      s_pClr[ 8] = al_make_color(0x55, 0x55, 0x55);  // gray
      s_pClr[ 9] = al_make_color(0x55, 0x55, 0xFF);  // bright blue
      s_pClr[10] = al_make_color(0x55, 0xFF, 0x55);  // bright green
      s_pClr[11] = al_make_color(0x55, 0xFF, 0xFF);  // bright cyan
      s_pClr[12] = al_make_color(0xFF, 0x55, 0x55);  // bright red
      s_pClr[13] = al_make_color(0xFF, 0x55, 0xFF);  // bright magenta
      s_pClr[14] = al_make_color(0xFF, 0xFF, 0x55);  // yellow
      s_pClr[15] = al_make_color(0xFF, 0xFF, 0xFF);  // bright white

      bmp = al_create_system_bitmap(AL_SCREEN_W, AL_SCREEN_H);
      if ( lClearInit )
      {
         s_pbyScrBuffer = (BYTE *) hb_xgrab( s_usScrWidth * s_usScrHeight * 2 );
         memset( s_pbyScrBuffer, 0, s_usScrWidth * s_usScrHeight * 2 );
      } else
      {
         al_clear_to_color( bmp, s_pClr[s_pbyScrBuffer[1] >> 4] );
         al_clear_to_color( al_screen, s_pClr[s_pbyScrBuffer[1] >> 4] );
      }

      if ( bmp == NULL )
      {
         bmp = al_create_bitmap(AL_SCREEN_W, AL_SCREEN_H);
      }

      hb_xfree( pFileName );

      if ( bmp == NULL )
      {
         printf("ERROR: could not allocate double buffer bitmap\n");
         exit(1);
      }

      hb_gt_DoCursor();  // show initial cursor
   }

   if ( !lClearInit )
   {
      s_usUpdTop = 0;
      s_usUpdLeft = 0;
      s_usUpdBottom = s_usScrHeight - 1;
      s_usUpdRight = s_usScrWidth - 1;
      HB_GT_FUNC(gt_ScreenUpdate());
   }

   lClearInit = TRUE;
   s_usGFXWidth = 0;
   s_usGFXHeight = 0;
   return lMode;
}

USHORT HB_GT_FUNC(gt_Box( SHORT sTop, SHORT sLeft, SHORT sBottom, SHORT sRight, BYTE *szBox, BYTE byAttr ))
{
   USHORT usRet = 1;
   SHORT x, y, sWidth, sHeight;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Box(%d, %d, %d, %d, %s, %d)", sTop, sLeft, sBottom, sRight, (char *) szBox, (int) byAttr));

   if ( s_pbyScrBuffer == NULL )
   {
      HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   // coord check
   if ( sTop > sBottom )
   {
      y = sTop;
      sTop = sBottom;
      sBottom = y;
   }

   if ( sLeft > sRight )
   {
      x = sLeft;
      sLeft = sRight;
      sRight = x;
   }

   if ( sTop < s_usScrHeight && sLeft < s_usScrWidth && sBottom >= 0 && sRight >= 0)
   {
      sWidth = sRight - sLeft + 1;
      sHeight = sBottom - sTop + 1;

      HB_GT_FUNC(gt_DispBegin());
      if ( sWidth == 1 )
      {
         if ( sTop < 0 )
         {
            sTop = 0;
         }

         if ( sBottom >= s_usScrHeight )
         {
            sBottom = s_usScrHeight - 1;
         }

         for ( y = sTop; y <= sBottom; y++ )
         {
            HB_GT_FUNC(gt_Replicate(y, sLeft, byAttr, szBox[7], 1));
         }
      }
      else if ( sHeight == 1 )
      {
         if ( sLeft < 0 )
         {
            sLeft = 0;
            sWidth = sRight - sLeft + 1;
         }
         if ( sRight >= s_usScrWidth )
         {
            sRight = s_usScrWidth - 1;
            sWidth = sRight - sLeft + 1;
         }
         HB_GT_FUNC(gt_Replicate(sTop, sLeft, byAttr, szBox[1], sWidth));
      }
      else
      {
         if ( sTop < 0 )
         {
            sHeight -= sTop * -1;
         }

         if ( sLeft < 0 )
         {
            sWidth -= sLeft * -1;
         }

         if ( sBottom >= s_usScrHeight )
         {
            sHeight -= sBottom - s_usScrHeight + 1;
         }

         if ( sRight >= s_usScrWidth )
         {
            sWidth -= sRight - s_usScrWidth + 1;
         }

         if ( sTop >= 0 )
         {
            x = ( sLeft >= 0 ? sLeft : 0 );
            if ( sLeft >= 0)
            {
               HB_GT_FUNC(gt_Replicate(sTop, x, byAttr, szBox[0], 1));
            }
            HB_GT_FUNC(gt_Replicate(sTop, x + 1, byAttr, szBox[1], sWidth - 2));
            if ( sRight < s_usScrWidth )
            {
               HB_GT_FUNC(gt_Replicate(sTop, x + sWidth - 1, byAttr, szBox[2], 1));
            }
         }

         if ( sRight < s_usScrWidth )
         {
            x = ( sTop >= 0 ? sTop : 0 );
            for ( y = 1; y < sHeight - 1; y++ )
            {
               HB_GT_FUNC(gt_Replicate(x + y, sRight, byAttr, szBox[3], 1));
               HB_GT_FUNC(gt_Replicate(x + y, sLeft > 0 ? sLeft + 1 : 1, byAttr, szBox[8], sWidth - 2));
               if ( sLeft >= 0 )
               {
                  HB_GT_FUNC(gt_Replicate(x + y, sLeft, byAttr, szBox[7], 1));
               }
            }
         }

         if ( sBottom < s_usScrHeight )
         {
            x = ( sLeft >= 0 ? sLeft : 0 );
            if ( sLeft >= 0)
            {
               HB_GT_FUNC(gt_Replicate(sBottom, x, byAttr, szBox[6], 1));
            }

            HB_GT_FUNC(gt_Replicate(sBottom, x + 1, byAttr, szBox[5], sWidth - 2));

            if ( sRight < s_usScrWidth )
            {
               HB_GT_FUNC(gt_Replicate(sBottom, x + sWidth - 1, byAttr, szBox[4], 1));
            }
         }
      }

      HB_GT_FUNC(gt_DispEnd());
   }

   usRet = 0;
   return usRet;
}

USHORT HB_GT_FUNC(gt_BoxD( SHORT sTop, SHORT sLeft, SHORT sBottom, SHORT sRight, BYTE *pbyFrame, BYTE byAttr))
{
   return HB_GT_FUNC(gt_Box(sTop, sLeft, sBottom, sRight, pbyFrame, byAttr));
}

USHORT HB_GT_FUNC(gt_BoxS( SHORT sTop, SHORT sLeft, SHORT sBottom, SHORT sRight, BYTE *pbyFrame, BYTE byAttr))
{
   return HB_GT_FUNC(gt_Box(sTop, sLeft, sBottom, sRight, pbyFrame, byAttr));
}

USHORT HB_GT_FUNC(gt_HorizLine( SHORT sRow, SHORT sLeft, SHORT sRight, BYTE byChar, BYTE byAttr))
{
   USHORT usRet = 1;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_HorizLine(%hd, %hd, %hd, %c, %d)", sRow, sLeft, sRight, byChar, (int) byAttr));

   if ( s_pbyScrBuffer == NULL )
   {
      HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   if ( sRow >= 0 && sRow < s_usScrHeight )
   {
      if ( sLeft < 0 )
      {
         sLeft = 0;
      }

      if ( sRight >= sLeft )
      {
         HB_GT_FUNC(gt_Replicate((USHORT) sRow, (USHORT) sLeft, byAttr, byChar, sRight - sLeft));
         usRet = 0;
      }
   }

   return usRet;
}

USHORT HB_GT_FUNC(gt_VertLine( SHORT sCol, SHORT sTop, SHORT sBottom, BYTE byChar, BYTE byAttr))
{
   USHORT usRet = 1, i;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_VertLine(%hd, %hd, %hd, %c, %d)", sCol, sTop, sBottom, byChar, (int) byAttr));

   if ( s_pbyScrBuffer == NULL )
   {
      HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   if ( sCol >= 0 && sCol < s_usScrWidth )
   {
      if ( sTop < 0 )
      {
         sTop = 0;
      }

      if ( sBottom >= sTop )
      {
         for ( i = (USHORT) sTop; i <= (USHORT) sBottom; i++)
         {
            HB_GT_FUNC(gt_Replicate(i, (USHORT) sCol, byAttr, byChar, 1));
         }
         usRet = 0;
      }
   }

   return usRet;
}

void HB_GT_FUNC(gt_Tone( double dFreq, double dInterval ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFreq, dInterval));

   HB_SYMBOL_UNUSED(dFreq);
   HB_SYMBOL_UNUSED(dInterval);
}

int HB_GT_FUNC(gt_ReadKey( HB_inkey_enum eventmask ))
{
   int nKey = 0;
   int i;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

   if ( s_pbyScrBuffer == NULL )
   {
      HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   if ( al_mouse_needs_poll() )
   {
      al_poll_mouse();
   }

   if ( ( ( al_mouse_x / s_byFontWidth ) != s_iMSX ) | ( ( al_mouse_y / s_byFontSize ) != s_iMSY ) )
   {
      s_iMSX = al_mouse_x / s_byFontWidth;
      s_iMSY = al_mouse_y / s_byFontSize;
      if ( eventmask & INKEY_MOVE )
      {
         nKey = K_MOUSEMOVE;
      }
   }

   if ( ( nKey == 0 ) && ( (BYTE) al_mouse_b != s_byMSButtons ) )
   {
      if ( ( al_mouse_b & 1 ) != ( s_byMSButtons & 1 ) )
      {
         if ( al_mouse_b & 1 )
         {
            if ( eventmask & INKEY_LDOWN )
            {
               nKey = K_LBUTTONDOWN;
            }
         }
         else
         {
            if ( eventmask & INKEY_LUP )
            {
               nKey = K_LBUTTONUP;
            }
         }
      } else if ( ( al_mouse_b & 2 ) != ( s_byMSButtons & 2 ) )
      {
         if ( al_mouse_b & 2 )
         {
            if ( eventmask & INKEY_RDOWN )
            {
               nKey = K_RBUTTONDOWN;
            }
         }
         else
         {
            if ( eventmask & INKEY_RUP )
            {
               nKey = K_RBUTTONUP;
            }
         }
      } /* else if ( ( al_mouse_b & 4 ) != ( s_byMSButtons & 4 ) )
      {
         if ( al_mouse_b & 4 )
         {
            if ( eventmask & INKEY_MDOWN )
            {
                  nKey = K_MBUTTONDOWN;
            }
         } else
         {
            if ( eventmask & INKEY_MUP )
            {
                  nKey = K_MBUTTONUP;
            }
         }
      } */
      // We need to define INKEY_M* & K_MBUTTON* in inkey.ch !
      s_byMSButtons = (BYTE) al_mouse_b;
   }

   if ( ( nKey == 0 ) && ( ( eventmask & INKEY_KEYBOARD ) | ( eventmask & HB_INKEY_RAW ) | ( eventmask & HB_INKEY_EXTENDED ) ) )
   {
      if ( al_keyboard_needs_poll() )
      {
         al_poll_keyboard();
      }

      if ( al_key_pressed() )
      {
         nKey = al_read_key();
      }

      if ( eventmask & HB_INKEY_RAW )
      {
         // leave the key as its raw value
      }
      else if ( nKey & 255 )
      {
         nKey = nKey & 255;
      }
      else if ( nKey != 0 )
      {
//       Good standard debuging...
//       printf("scancode: %d (0x%0x) ascii: %d (0x%0x)\n", nKey>>8, nKey>>8, nKey&0xff, nKey&0xff);
         for ( i = 0; i < GT_KEY_TABLE_SIZE; i++ )
         {
            if ( ( nKey >> 8 ) == sKeyTable[i].al_key )
            {
               nKey = sKeyTable[i].xhb_key;
               break;
            }
         }
      }
   }

   return nKey;
}

void HB_GT_FUNC(null_func( void ))
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
   if ( s_pbyScrBuffer == NULL )
   {
      HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   if ( al_mouse_needs_poll() )
   {
      al_poll_mouse();
   }

   return al_mouse_x / s_byFontWidth;
}

int HB_GT_FUNC(mouse_Row( void ))
{
   if ( s_pbyScrBuffer == NULL )
   {
   HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   if ( al_mouse_needs_poll() )
   {
   al_poll_mouse();
   }

   return al_mouse_y / s_byFontSize;
}

void HB_GT_FUNC(mouse_SetPos( int iRow, int iCol ))
{
   if ( s_pbyScrBuffer == NULL )
   {
      HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   al_position_mouse(iCol * s_byFontWidth, iRow * s_byFontSize);
}

BOOL HB_GT_FUNC(mouse_IsButtonPressed( int iButton ))
{
   BOOL lRet;

   if ( s_pbyScrBuffer == NULL )
   {
      HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   if ( al_mouse_needs_poll() )
   {
      al_poll_mouse();
   }

   lRet = FALSE;

   if ( iButton == 3 )
   {
      if ( ( al_mouse_b & 4 ) == 4 )
      {
         lRet = TRUE;
      }
   }
   else
   {
      if ( ( al_mouse_b & iButton ) == iButton )
      {
         lRet = TRUE;
      }
   }

   return lRet;
}

int HB_GT_FUNC(mouse_CountButton( void ))
{
   if ( s_pbyScrBuffer == NULL )
   {
      HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   return s_iMsButtons;
}

void HB_GT_FUNC(mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight ))
{
   if ( s_pbyScrBuffer == NULL )
   {
      HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   if ( iTop > -1 && iTop * s_byFontSize < AL_SCREEN_H )
   {
      s_iMSBoundTop = iTop * s_byFontSize;
   }

   if ( iLeft > -1 && iLeft * s_byFontWidth < AL_SCREEN_W )
   {
      s_iMSBoundLeft = iLeft * s_byFontWidth;
   }

   if ( iBottom >= iTop && iBottom * s_byFontSize < AL_SCREEN_H )
   {
      s_iMSBoundBottom = iBottom * s_byFontSize;
   }

   if ( iRight >= iLeft && iRight * s_byFontWidth < AL_SCREEN_W )
   {
      s_iMSBoundRight = iRight * s_byFontWidth;
   }

   al_set_mouse_range( s_iMSBoundLeft, s_iMSBoundTop, s_iMSBoundRight, s_iMSBoundBottom );
}

void HB_GT_FUNC(mouse_GetBounds( int *piTop, int *piLeft, int *piBottom, int *piRight ))
{
   if ( s_pbyScrBuffer == NULL )
   {
      HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
   }

   *piTop = s_iMSBoundTop;
   *piLeft = s_iMSBoundLeft;
   *piBottom = s_iMSBoundBottom;
   *piRight = s_iMSBoundRight;
}

/*
* GTInfo() implementation
*
*/
int HB_EXPORT HB_GT_FUNC( gt_info(int iMsgType, BOOL bUpdate, int iParam, void *vpParam ) )
{
   int iOldValue;
   int iWidth, iHeight;

   HB_SYMBOL_UNUSED( vpParam );

   switch ( iMsgType )
   {
      case GTI_ISGRAPHIC:
      return (int) TRUE;

      case GTI_SCREENWIDTH:
         iOldValue = AL_SCREEN_W;
         if ( bUpdate && iParam > 0 )
         {
            s_usGFXWidth = (USHORT) iParam;
//            lClearInit = ( s_pbyScrBuffer == NULL );
//            HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
         }
      return iOldValue;

      case GTI_SCREENHEIGHT:
         iOldValue = AL_SCREEN_H;
         if ( bUpdate && iParam > 0 )
         {
            s_usGFXHeight = (USHORT) iParam;
            lClearInit = ( s_pbyScrBuffer == NULL );
            HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
         }
      return iOldValue;

      case GTI_SCREENDEPTH:
         iOldValue = al_bitmap_color_depth( al_screen );
         if ( bUpdate &&
               (( iParam == 8 ) || ( iParam == 15 ) ||
                ( iParam == 16 ) || ( iParam == 24 ) || ( iParam == 32 ) )
            )
         {
            al_set_color_depth( iParam );
            lClearInit = ( s_pbyScrBuffer == NULL );
            HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
         }
      return iOldValue;

      case GTI_FONTSIZE:
         iOldValue = s_byFontSize;
         if ( bUpdate && iParam > 0 && iParam < 256 )
         {
            s_byFontSize = (char) iParam;
            s_byFontWidth = s_byFontSize / 2;
            lClearInit = ( s_pbyScrBuffer == NULL );
            HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
         }
      return iOldValue;

      case GTI_FONTWIDTH:
      return s_byFontWidth;

      case GTI_DESKTOPWIDTH:
         al_get_desktop_resolution( &iWidth, &iHeight );
      return iWidth;

      case GTI_DESKTOPHEIGHT:
         al_get_desktop_resolution( &iWidth, &iHeight );
      return iHeight;

      case GTI_DESKTOPDEPTH:
      return al_desktop_color_depth();
   }

   // DEFAULT: there's something wrong if we are here.
   return -1;
}

#ifdef HB_MULTI_GT

static void HB_GT_FUNC(gtFnInit( PHB_GT_FUNCS gt_funcs ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtFnInit(%p)", gt_funcs));

   gt_funcs->Init               = HB_GT_FUNC(gt_Init);
   gt_funcs->Exit               = HB_GT_FUNC(gt_Exit);
   gt_funcs->GetScreenWidth     = HB_GT_FUNC(gt_GetScreenWidth);
   gt_funcs->GetScreenHeight    = HB_GT_FUNC(gt_GetScreenHeight);
   gt_funcs->Col                = HB_GT_FUNC(gt_Col);
   gt_funcs->Row                = HB_GT_FUNC(gt_Row);
   gt_funcs->SetPos             = HB_GT_FUNC(gt_SetPos);
   gt_funcs->AdjustPos          = HB_GT_FUNC(gt_AdjustPos);
   gt_funcs->IsColor            = HB_GT_FUNC(gt_IsColor);
   gt_funcs->GetCursorStyle     = HB_GT_FUNC(gt_GetCursorStyle);
   gt_funcs->SetCursorStyle     = HB_GT_FUNC(gt_SetCursorStyle);
   gt_funcs->DispBegin          = HB_GT_FUNC(gt_DispBegin);
   gt_funcs->DispEnd            = HB_GT_FUNC(gt_DispEnd);
   gt_funcs->DispCount          = HB_GT_FUNC(gt_DispCount);
   gt_funcs->Puts               = HB_GT_FUNC(gt_Puts);
   gt_funcs->Replicate          = HB_GT_FUNC(gt_Replicate);
   gt_funcs->RectSize           = HB_GT_FUNC(gt_RectSize);
   gt_funcs->GetText            = HB_GT_FUNC(gt_GetText);
   gt_funcs->PutText            = HB_GT_FUNC(gt_PutText);
   gt_funcs->SetAttribute       = HB_GT_FUNC(gt_SetAttribute);
   gt_funcs->Scroll             = HB_GT_FUNC(gt_Scroll);
   gt_funcs->SetMode            = HB_GT_FUNC(gt_SetMode);
   gt_funcs->GetBlink           = HB_GT_FUNC(gt_GetBlink);
   gt_funcs->SetBlink           = HB_GT_FUNC(gt_SetBlink);
   gt_funcs->Version            = HB_GT_FUNC(gt_Version);
   gt_funcs->Box                = HB_GT_FUNC(gt_Box);
   gt_funcs->BoxD               = HB_GT_FUNC(gt_BoxD);
   gt_funcs->BoxS               = HB_GT_FUNC(gt_BoxS);
   gt_funcs->HorizLine          = HB_GT_FUNC(gt_HorizLine);
   gt_funcs->VertLine           = HB_GT_FUNC(gt_VertLine);
   gt_funcs->Suspend            = HB_GT_FUNC(gt_Suspend);
   gt_funcs->Resume             = HB_GT_FUNC(gt_Resume);
   gt_funcs->PreExt             = HB_GT_FUNC(gt_PreExt);
   gt_funcs->PostExt            = HB_GT_FUNC(gt_PostExt);
   gt_funcs->OutStd             = HB_GT_FUNC(gt_OutStd);
   gt_funcs->OutErr             = HB_GT_FUNC(gt_OutErr);
   gt_funcs->Tone               = HB_GT_FUNC(gt_Tone);
   gt_funcs->ExtendedKeySupport = HB_GT_FUNC(gt_ExtendedKeySupport);
   gt_funcs->ReadKey            = HB_GT_FUNC(gt_ReadKey);
   gt_funcs->info               = HB_GT_FUNC(gt_info);
   // todo: update
}

static void HB_GT_FUNC(mouseFnInit( PHB_GT_FUNCS gt_funcs ))
{

   HB_TRACE(HB_TR_DEBUG, ("hb_mouseFnInit(%p)", gt_funcs));

   gt_funcs->mouse_Init            = HB_GT_FUNC(null_func);
   gt_funcs->mouse_Exit            = HB_GT_FUNC(null_func);
   gt_funcs->mouse_IsPresent       = HB_GT_FUNC(mouse_IsPresent);
   gt_funcs->mouse_Show            = HB_GT_FUNC(mouse_Show);
   gt_funcs->mouse_Hide            = HB_GT_FUNC(mouse_Hide);
   gt_funcs->mouse_Col             = HB_GT_FUNC(mouse_Col);
   gt_funcs->mouse_Row             = HB_GT_FUNC(mouse_Row);
   gt_funcs->mouse_SetPos          = HB_GT_FUNC(mouse_SetPos);
   gt_funcs->mouse_IsButtonPressed = HB_GT_FUNC(mouse_IsButtonPressed);
   gt_funcs->mouse_CountButton     = HB_GT_FUNC(mouse_CountButton);
   gt_funcs->mouse_GetBounds       = HB_GT_FUNC(mouse_GetBounds);
   gt_funcs->mouse_SetBounds       = HB_GT_FUNC(mouse_SetBounds);
   // todo: update
}

static HB_GT_INIT gtInit = { HB_GT_DRVNAME( HB_GT_NAME ), HB_GT_FUNC(gtFnInit), HB_GT_FUNC(mouseFnInit) };

HB_GT_ANNOUNCE( HB_GT_NAME )

HB_CALL_ON_STARTUP_BEGIN( HB_GT_FUNC(_gt_Init_) )
   hb_gtRegister( &gtInit );
HB_CALL_ON_STARTUP_END( HB_GT_FUNC(_gt_Init_) )
#if defined(HB_STATIC_STARTUP) || ((!defined(__GNUC__)) && (!defined(_MSC_VER)) )
   #pragma startup HB_GT_FUNC(_gt_Init_)
#endif

#endif

/* 
* this is necessary if you want to link with .so allegro libs
* or when link staticalt and your linker will force to link main()
* from allegro library not the harbour one
*/
int _mangled_main( int argc, char * argv[] )
{
   HB_TRACE(HB_TR_DEBUG, ("_mangled_main(%d, %p)", argc, argv));

   hb_cmdargInit( argc, argv );
   hb_vmInit( TRUE );

   return hb_vmQuit();
}
void *_mangled_main_address = _mangled_main;

/*
 * GTInfo() implementation
 *
 */
 /*
HB_FUNC( GTINFO )
{
   int iWidth, iHeight;
   int iInfo = hb_itemGetNI( hb_param( 1, HB_IT_NUMERIC ) );

   switch ( iInfo )
   {
      case GTI_ISGRAPHIC:
         hb_retl( TRUE );
	 break;
      case GTI_SCREENWIDTH:
         if ( s_pbyScrBuffer == NULL )  // gt was NOT initialized
	 {
	    hb_retni( s_byFontWidth * s_usScrWidth );
	 } else
	 {
            hb_retni( AL_SCREEN_W );
	 }
	 iWidth = hb_itemGetNI( hb_param( 2, HB_IT_NUMERIC ) );
	 if ( iWidth > 0 )
	 {
	    s_usGFXWidth = (USHORT) iWidth;
//            lClearInit = ( s_pbyScrBuffer == NULL );
//            HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
	 }
	 break;
      case GTI_SCREENHEIGHT:
         if ( s_pbyScrBuffer == NULL )  // gt was NOT initialized
	 {
	    hb_retni( s_byFontSize * s_usScrHeight );
	 } else
	 {
            hb_retni( AL_SCREEN_H );
	 }
	 iHeight = hb_itemGetNI( hb_param( 2, HB_IT_NUMERIC ) );
	 if ( iHeight > 0 )
	 {
	    s_usGFXHeight = (USHORT) iHeight;
	    lClearInit = ( s_pbyScrBuffer == NULL );
            HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
	 }
	 break;
      case GTI_SCREENDEPTH:
         if ( s_pbyScrBuffer == NULL )  // gt was NOT initialized
	 {
	    hb_retni( al_get_desktop_depth() );
	 } else
	 {
            hb_retni( al_bitmap_color_depth( al_screen ) );
	 }
	 int iDepth = hb_itemGetNI( hb_param( 2, HB_IT_NUMERIC ) );
	 if ( ( iDepth == 8 ) || ( iDepth == 15 ) || ( iDepth == 16 ) || ( iDepth == 24 ) || ( iDepth == 32 ) )
	 {
	    al_set_color_depth( iDepth );
	    lClearInit = ( s_pbyScrBuffer == NULL );
            HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
	 }
	 break;
      case GTI_FONTSIZE:
         hb_retni( s_byFontSize );
	 int iSize = hb_itemGetNI( hb_param( 2, HB_IT_NUMERIC ) );
	 if ( iSize > 0 && iSize < 256 )
	 {
	    s_byFontSize = (char) iSize;
	    s_byFontWidth = s_byFontSize / 2;
	    lClearInit = ( s_pbyScrBuffer == NULL );
            HB_GT_FUNC(gt_SetMode(s_usScrHeight, s_usScrWidth));
	 }
	 break;
      case GTI_FONTWIDTH:
         hb_retni( s_byFontWidth );
	 break;
      case GTI_DESKTOPWIDTH:
	 al_get_desktop_resolution( &iWidth, &iHeight );
         hb_retni( iWidth );
	 break;
      case GTI_DESKTOPHEIGHT:
	 al_get_desktop_resolution( &iWidth, &iHeight );
         hb_retni( iHeight );
	 break;
      case GTI_DESKTOPDEPTH:
         hb_retni( al_desktop_color_depth() );
	 break;
   }
}

*/
