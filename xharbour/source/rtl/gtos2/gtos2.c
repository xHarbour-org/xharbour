/*
 * $Id: gtos2.c,v 1.15 2004/08/31 07:05:25 mauriliolongo Exp $
 */

/*
 * Harbour Project source code:
 * Video subsystem for OS/2 compilers
 *
 * Copyright 1999 - 2001 Harbour Project
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
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    HB_GT_FUNC(gt_ReadKey())
 *
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
 *    HB_GT_FUNC(gt_Tone())
 *    HB_GT_FUNC(gt_IsColor())
 *    HB_GT_FUNC(gt_Scroll())
 *    HB_GT_FUNC(gt_SetCursorSize())
 *    HB_GT_FUNC(gt_GetCursorStyle())
 *    HB_GT_FUNC(gt_SetCursorStyle())
 *    HB_GT_FUNC(gt_SetAttribute())
 *    HB_GT_FUNC(gt_GetBlink())
 *    HB_GT_FUNC(gt_SetBlink())
 *    _gt_GetCellSize()
 *
 * Copyright 2000 - 2001 Maurilio Longo <maurilio.longo@libero.it>
 *    HB_GT_FUNC(gt_DispBegin()) / HB_GT_FUNC(gt_DispEnd())
 *    _gt_ScreenPtr() and HB_GT_FUNC(gt_xYYYY()) functions and virtual screen support inside HB_GT_FUNC(gt_XXXX())s
 *    16 bit KBD subsystem use inside HB_GT_FUNC(gt_ReadKey())
 *
 * See doc/license.txt for licensing terms.
 *
 */

/*
 *  This module is partially based on VIDMGR by Andrew Clarke and modified
 *  for the Harbour project
 */

/* NOTE: User programs should never call this layer directly! */

/* This definition has to be placed before #include "hbapigt.h" */
#define HB_GT_NAME   OS2

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapigt.h"
#include "hbapifs.h"
#include "inkey.ch"


/* convert 16:16 address to 0:32 */
#define SELTOFLAT(ptr) (void *)(((((ULONG)(ptr))>>19)<<16)|(0xFFFF&((ULONG)(ptr))))

#if defined(HARBOUR_GCC_OS2)
   /* 25/03/2000 - maurilio.longo@libero.it
   OS/2 GCC hasn't got ToolKit headers available */
   #include <stdlib.h>
#else
   #include <bsedos.h>
   #ifndef KBDTRF_EXTENDED_CODE
      #define KBDTRF_EXTENDED_CODE 0x02
   #endif
#endif
#include <conio.h>


/* faster macro version for use inside this module */
#define _GetScreenWidth()  ( s_vi.col )
#define _GetScreenHeight() ( s_vi.row )
#define _gt_ScreenPtr( cRow, cCol )  ( (char *) (s_ulLVBptr + ( ( ( cRow * _GetScreenWidth() ) + cCol ) * 2 ) ) )
#define _gt_GetCellSize()   ( ( char )( s_vi.row ? ( s_vi.vres / s_vi.row ) - 1 : 0 ) )


static void HB_GT_FUNC(gt_xGetXY( USHORT cRow, USHORT cCol, BYTE * attr, BYTE * ch ));
static void HB_GT_FUNC(gt_xPutch( USHORT cRow, USHORT cCol, BYTE attr, BYTE ch ));

static char *s_clipboard = NULL;
static ULONG s_clipsize = 0;

/*
static void HB_GT_FUNC(gt_GetCursorSize( char * start, char * end ));
*/

/* how many nested BeginDisp() */
static USHORT s_uiDispCount;

/* pointer to offscreen video buffer */
static ULONG s_ulLVBptr;

/* length of video buffer */
static USHORT s_usLVBlength;

/* keyboard event record */
static PKBDKEYINFO s_key;

/* keyboard handle, 0 == default */
static PHKBD s_hk;

/* Code page ID of active codepage at the time harbour program was start */
static USHORT s_usOldCodePage;

/* Instead of calling VioGetMode() every time I need MaxRow() or MaxCol() I
   use this static which contains active mode info */
static VIOMODEINFO s_vi;

/* TRUE when there are I/O operations to video buffer */
static BOOL s_Dirty = FALSE;

static int s_iStdIn, s_iStdOut, s_iStdErr;



static void refresh_buffer(int Row, int Col, int Len) {

   static int nOffSet = -1, nLen = 0;

   if ( s_uiDispCount <= 1 ) {

      if ( nOffSet == -1 ) {

         if ( s_Dirty ) {
            VioShowBuf( ( ( Row * _GetScreenWidth() ) + Col ) * 2 , Len, 0);
            s_Dirty = FALSE;
         }

      } else {

         VioShowBuf( nOffSet, nLen, 0);
         nOffSet = -1;
         s_Dirty = FALSE;

      }

   } else {

      register int OldLen = nLen, OldOffSet = nOffSet;
      register int NewOffSet = ( ( Row * _GetScreenWidth() ) + Col ) * 2;

      if ( nOffSet == -1 ) {
         nOffSet = NewOffSet;
         nLen = Len;

      } else {

         if ( NewOffSet < nOffSet ) {
            nLen += nOffSet - NewOffSet + 1;
            nOffSet = NewOffSet;
         }

         if ( ( NewOffSet + Len ) > ( OldOffSet + OldLen ) ) {
            nLen += ( ( NewOffSet + Len ) - ( OldOffSet + OldLen ) + 1);
         }

      }
   }
}



void HB_GT_FUNC(gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr ))
{
   APIRET rc;           /* return code from DosXXX api call */

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

   /* stdin && stdout && stderr */
   s_iStdIn  = iFilenoStdin;
   s_iStdOut = iFilenoStdout;
   s_iStdErr = iFilenoStderr;

   s_vi.cb = sizeof( VIOMODEINFO );
   VioGetMode( &s_vi, 0 );        /* fill structure with current video mode settings */

   /* To "comment out" old code calling VIOXXX subsystem */
   s_uiDispCount = 1;

   if( VioGetBuf(&s_ulLVBptr, &s_usLVBlength, 0) == NO_ERROR ) {
      s_ulLVBptr = (ULONG) SELTOFLAT(s_ulLVBptr);
      VioShowBuf(0, s_usLVBlength, 0);

   } else {
      s_ulLVBptr = (ULONG) NULL;
   }

   /* Alloc tileable memory for calling a 16 subsystem */
   rc = DosAllocMem((PPVOID) &s_hk, sizeof(HKBD), PAG_COMMIT | OBJ_TILE | PAG_WRITE);
   if (rc != NO_ERROR) {
      hb_errInternal( HB_EI_XGRABALLOC, "hb_gt_ReadKey() memory allocation failure", NULL, NULL);
   }
   /* it is a long after all, so I set it to zero only one time since it never changes */
   memset(s_hk, 0, sizeof(HKBD));

   rc = DosAllocMem((PPVOID) &s_key, sizeof(KBDKEYINFO), PAG_COMMIT | OBJ_TILE | PAG_WRITE);
   if (rc != NO_ERROR) {
      hb_errInternal( HB_EI_XGRABALLOC, "hb_gt_ReadKey() memory allocation failure", NULL, NULL);
   }

   HB_GT_FUNC(mouse_Init());

   /* TODO: Is anything else required to initialize the video subsystem?
            I (Maurilio Longo) think that we should set correct codepage

      COMMENT: The correct behaviour is to inherit the codepage that is
               active when the program is started, which automatically
               happens by not setting the codepage. If somebody wants to
               change the codepage, there should be a separate function
               to do that. (David G. Holm <dholm@jsd-llc.com>)

      ANSWER:  But I have a different code page than you and box drawing
               chars are "wrong". So we need to set code page of
               box drawing chars. (Maurilio Longo - maurilio.longo@libero.it)
   */

   /* 21/08/2001 - <maurilio.longo@libero.it>
      NOTE: Box drawing characters need page 437 to show correctly, so, in your config.sys you
            need to have a CODEPAGE=x,y statement where x or y is equal to 437
   */

   VioGetCp(0, &s_usOldCodePage, 0);

   /* If I could not set codepage 437 I reset previous codepage, maybe I do not need to do this */
   if (VioSetCp(0, 437, 0) != NO_ERROR) {
      VioSetCp(0, s_usOldCodePage, 0);
   }
}


void HB_GT_FUNC(gt_Exit( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

   DosFreeMem(s_key);
   DosFreeMem(s_hk);

   if ( s_clipboard != NULL )
   {
      hb_xfree( s_clipboard );
   }

   HB_GT_FUNC(mouse_Exit());
   VioSetCp(0, s_usOldCodePage, 0);

   /* TODO: */
}


BOOL HB_GT_FUNC(gt_AdjustPos( BYTE * pStr, ULONG ulLen ))
{
   USHORT x, y;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

   HB_SYMBOL_UNUSED( pStr );
   HB_SYMBOL_UNUSED( ulLen );

   s_Dirty = TRUE;

   VioGetCurPos( &y, &x, 0 );
   hb_gtSetPos( ( SHORT ) y, ( SHORT ) x );

   return TRUE;
}


int HB_GT_FUNC(gt_ExtendedKeySupport())
{
   return 0;
}


int HB_GT_FUNC(gt_ReadKey( HB_inkey_enum eventmask ))
{
   int ch;                       /* next char if any */

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

   /* zero out keyboard event record */
   memset(s_key, 0, sizeof(KBDKEYINFO));

   /* Get next character without wait */
   KbdCharIn(s_key, IO_NOWAIT, (HKBD) * s_hk);

   /* extended key codes have 00h or E0h as chChar */
   if ((s_key->fbStatus & KBDTRF_EXTENDED_CODE) && (s_key->chChar == 0x00 || s_key->chChar == 0xE0))  {

      /* It was an extended function key lead-in code, so read the actual function key and then offset it by 256,
         unless extended keyboard events are allowed, in which case offset it by 512 */
      if ((s_key->chChar == 0xE0) && (eventmask & INKEY_RAW)) {
         ch = (int) s_key->chScan + 512;

      } else {
         ch = (int) s_key->chScan + 256;

      }

   } else if (s_key->fbStatus & KBDTRF_FINAL_CHAR_IN) {
      ch = (int) s_key->chChar;

   } else {
      ch = 0;

   }

   /* Perform key translations */
   switch( ch )
   {
      case 328:  /* Up arrow */
         ch = K_UP;
         break;
      case 336:  /* Down arrow */
         ch = K_DOWN;
         break;
      case 331:  /* Left arrow */
         ch = K_LEFT;
         break;
      case 333:  /* Right arrow */
         ch = K_RIGHT;
         break;
      case 327:  /* Home */
         ch = K_HOME;
         break;
      case 335:  /* End */
         ch = K_END;
         break;
      case 329:  /* Page Up */
         ch = K_PGUP;
         break;
      case 337:  /* Page Down */
         ch = K_PGDN;
         break;
      case 371:  /*  Ctrl + Left arrow */
         ch = K_CTRL_LEFT;
         break;
      case 372:  /* Ctrl + Right arrow */
         ch = K_CTRL_RIGHT;
         break;
      case 375:  /* Ctrl + Home */
         ch = K_CTRL_HOME;
         break;
      case 373:  /* Ctrl + End */
         ch = K_CTRL_END;
         break;
      case 388:  /* Ctrl + Page Up */
         ch = K_CTRL_PGUP;
         break;
      case 374:  /* Ctrl + Page Down */
         ch = K_CTRL_PGDN;
         break;
      case 338:  /* Insert */
         ch = K_INS;
         break;
      case 339:  /* Delete */
         ch = K_DEL;
         break;
      case 315:  /* F1 */
         ch = K_F1;
         break;
      case 316:  /* F2 */
      case 317:  /* F3 */
      case 318:  /* F4 */
      case 319:  /* F5 */
      case 320:  /* F6 */
      case 321:  /* F7 */
      case 322:  /* F8 */
      case 323:  /* F9 */
      case 324:  /* F10 */
         ch = 315 - ch;
         break;
      case 340:  /* Shift + F1 */
      case 341:  /* Shift + F2 */
      case 342:  /* Shift + F3 */
      case 343:  /* Shift + F4 */
      case 344:  /* Shift + F5 */
      case 345:  /* Shift + F6 */
      case 346:  /* Shift + F7 */
      case 347:  /* Shift + F8 */
      case 348:  /* Shift + F9 */
      case 349:  /* Shift + F10 */
      case 350:  /* Ctrl + F1 */
      case 351:  /* Ctrl + F2 */
      case 352:  /* Ctrl + F3 */
      case 353:  /* Ctrl + F4 */
      case 354:  /* Ctrl + F5 */
      case 355:  /* Ctrl + F6 */
      case 356:  /* Ctrl + F7 */
      case 357:  /* Ctrl + F8 */
      case 358:  /* Ctrl + F9 */
      case 359:  /* Ctrl + F10 */
      case 360:  /* Alt + F1 */
      case 361:  /* Alt + F2 */
      case 362:  /* Alt + F3 */
      case 363:  /* Alt + F4 */
      case 364:  /* Alt + F5 */
      case 365:  /* Alt + F6 */
      case 366:  /* Alt + F7 */
      case 367:  /* Alt + F8 */
      case 368:  /* Alt + F9 */
      case 369:  /* Alt + F10 */
         ch = 330 - ch;
         break;
      case 389:  /* F11 */
      case 390:  /* F12 */
      case 391:  /* Shift + F11 */
      case 392:  /* Shift + F12 */
      case 393:  /* Ctrl + F11 */
      case 394:  /* Ctrl + F12 */
      case 395:  /* Alt + F11 */
      case 396:  /* Alt + F12 */
         ch = 349 - ch;
   }

   return ch;
}


BOOL HB_GT_FUNC(gt_IsColor( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

   return s_vi.fbType != 0;        /* 0 = monochrom-compatible mode */
}



USHORT HB_GT_FUNC(gt_GetScreenWidth( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

   return s_vi.col;
}



USHORT HB_GT_FUNC(gt_GetScreenHeight( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

   return s_vi.row;
}



void HB_GT_FUNC(gt_SetPos( SHORT iRow, SHORT iCol, SHORT iMethod ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", iRow, iCol, iMethod));

   HB_SYMBOL_UNUSED( iMethod );

   s_Dirty = TRUE;

   VioSetCurPos( ( USHORT ) iRow, ( USHORT ) iCol, 0 );
   //refresh_buffer( iRow, iCol, 2 );
}


SHORT HB_GT_FUNC(gt_Row( void ))
{
   USHORT x, y;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

   VioGetCurPos( &y, &x, 0 );
   return ( SHORT ) y;
}


SHORT HB_GT_FUNC(gt_Col( void ))
{
   USHORT x, y;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

   VioGetCurPos( &y, &x, 0 );
   return ( SHORT ) x;
}


void HB_GT_FUNC(gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr, SHORT sVert, SHORT sHoriz ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRight, (int) attr, sVert, sHoriz));

   s_Dirty = TRUE;

   if(s_uiDispCount > 0)
   {
      int iRows = sVert, iCols = sHoriz;

      /* NOTE: 'SHORT' is used intentionally to correctly compile
       *  with C++ compilers
       */
      SHORT usRow, usCol;
      UINT uiSize;
      int iLength = ( usRight - usLeft ) + 1;
      int iCount, iColOld, iColNew, iColSize;

      hb_gtGetPos( &usRow, &usCol );

      if( hb_gtRectSize( usTop, usLeft, usBottom, usRight, &uiSize ) == 0 )
      {
         /* NOTE: 'unsigned' is used intentionally to correctly compile
          * with C++ compilers
          */
         BYTE * fpBlank = ( BYTE * ) hb_xgrab( iLength );
         BYTE * fpBuff = ( BYTE * ) hb_xgrab( iLength * 2 );

         memset( fpBlank, ' ', iLength );

         iColOld = iColNew = usLeft;
         if( iCols >= 0 )
         {
            iColOld += iCols;
            iColSize = ( int ) ( usRight - usLeft );
            iColSize -= iCols;
         }
         else
         {
            iColNew -= iCols;
            iColSize = ( int ) ( usRight - usLeft );
            iColSize += iCols;
         }

         for( iCount = ( iRows >= 0 ? usTop : usBottom );
         ( iRows >= 0 ? iCount <= usBottom : iCount >= usTop );
         ( iRows >= 0 ? iCount++ : iCount-- ) )
         {
            int iRowPos = iCount + iRows;

            /* Blank the scroll region in the current row */
            HB_GT_FUNC(gt_Puts( iCount, usLeft, attr, fpBlank, iLength ));

            if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
            {
               /* Read the text to be scrolled into the current row */
               HB_GT_FUNC(gt_GetText( iRowPos, iColOld, iRowPos, iColOld + iColSize, fpBuff ));

               /* Write the scrolled text to the current row */
               HB_GT_FUNC(gt_PutText( iCount, iColNew, iCount, iColNew + iColSize, fpBuff ));
            }
         }

         hb_xfree( fpBlank );
         hb_xfree( fpBuff );
      }

      hb_gtSetPos( usRow, usCol );

   }
   else
   {
      BYTE bCell[ 2 ];                          /* character/attribute pair */

      bCell [ 0 ] = ' ';
      bCell [ 1 ] = attr;

      if( ( sVert | sHoriz ) == 0 )             /* both zero, clear region */
         VioScrollUp ( usTop, usLeft, usBottom, usRight, 0xFFFF, bCell, 0 );

      else
      {
         if( sVert > 0 )                        /* scroll up */
            VioScrollUp ( usTop, usLeft, usBottom, usRight, sVert, bCell, 0 );

         else if( sVert < 0 )                   /* scroll down */
            VioScrollDn ( usTop, usLeft, usBottom, usRight, -sVert, bCell, 0 );

         if( sHoriz > 0 )                       /* scroll left */
            VioScrollLf ( usTop, usLeft, usBottom, usRight, sHoriz, bCell, 0 );

         else if( sHoriz < 0 )                  /* scroll right */
            VioScrollRt ( usTop, usLeft, usBottom, usRight, -sHoriz, bCell, 0 );
      }
   }
}

/* QUESTION: not been used, do we need this function ? */
/* Answer: In the dos version, this gets called by HB_GT_FUNC(gt_GetCursorStyle())
   as that function is written below, we don't need this */

/*
static void HB_GT_FUNC(gt_GetCursorSize( char * start, char * end ))
{
   VIOCURSORINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorSize(%p, %p)", start, end));

   VioGetCurType( &vi, 0 );
   *start = vi.yStart;
   *end = vi.cEnd;
}
*/


static void HB_GT_FUNC(gt_SetCursorSize( char start, char end, int visible ))
{
   VIOCURSORINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorSize(%d, %d, %d)", (int) start, (int) end, visible));

   s_Dirty = TRUE;

   vi.yStart = start;
   vi.cEnd = end;
   vi.cx = 0;
   vi.attr = ( visible ? 0 : -1 );
   VioSetCurType( &vi, 0 );

   //refresh_buffer( HB_GT_FUNC(gt_Row()), HB_GT_FUNC(gt_Col()), 2 );
}



USHORT HB_GT_FUNC(gt_GetCursorStyle( void ))
{
   int rc;
   char cellsize;
   VIOCURSORINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

   VioGetCurType( &vi, 0 );

   if( vi.attr )
      rc = SC_NONE;
   else
   {
      cellsize = _gt_GetCellSize();

      if( vi.yStart == 0 && vi.cEnd == 0 )
         rc = SC_NONE;

      else if( ( vi.yStart == cellsize - 1 || vi.yStart == cellsize - 2 ) && vi.cEnd == cellsize )
         rc = SC_NORMAL;

      else if( vi.yStart == cellsize / 2 && vi.cEnd == cellsize )
         rc = SC_INSERT;

      else if( vi.yStart == 0 && vi.cEnd == cellsize )
         rc = SC_SPECIAL1;

      else if( vi.yStart == 0 && vi.cEnd == cellsize / 2 )
         rc = SC_SPECIAL2;

      else
         rc = SC_NONE;
   }

   return rc;
}


void HB_GT_FUNC(gt_SetCursorStyle( USHORT style ))
{
   char cellsize;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", style));

   s_Dirty = TRUE;

   cellsize = _gt_GetCellSize();
   switch( style )
   {
   case SC_NONE:
      HB_GT_FUNC(gt_SetCursorSize( 0, 0, 0 ));
      break;

   case SC_NORMAL:
      HB_GT_FUNC(gt_SetCursorSize( cellsize - 1, cellsize, 1 ));
      break;

   case SC_INSERT:
      HB_GT_FUNC(gt_SetCursorSize( cellsize / 2, cellsize, 1 ));
      break;

   case SC_SPECIAL1:
      HB_GT_FUNC(gt_SetCursorSize( 0, cellsize, 1 ));
      break;

   case SC_SPECIAL2:
      HB_GT_FUNC(gt_SetCursorSize( 0, cellsize / 2, 1 ));
      break;

   default:
      break;
   }
}


static void HB_GT_FUNC(gt_xGetXY( USHORT cRow, USHORT cCol, BYTE * attr, BYTE * ch ))
{
   char * p;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xGetXY(%hu, %hu, %p, %p", cRow, cCol, ch, attr));

   p = _gt_ScreenPtr( cRow, cCol );
   *ch = *p;
   *attr = *( p + 1 );
}


static void HB_GT_FUNC(gt_xPutch( USHORT cRow, USHORT cCol, BYTE attr, BYTE ch ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %d", cRow, cCol, (int) attr, (int) ch));

   if (s_uiDispCount > 0) {
      USHORT * p = (USHORT *) _gt_ScreenPtr( cRow, cCol );
      *p = (attr << 8) + ch;

      /* No need to refresh_buffer() here, will do caller */

   } else {
      USHORT Cell = (attr << 8) + ch;
      VioWrtNCell((PBYTE) &Cell, 1, cRow, cCol, 0);
   }
}


void HB_GT_FUNC(gt_Puts( USHORT usRow, USHORT usCol, BYTE attr, BYTE * str, ULONG len ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", usRow, usCol, (int) attr, str, len));

   s_Dirty = TRUE;

   if (s_uiDispCount > 0) {
      register USHORT *p;
      register USHORT byAttr = attr << 8;
      register int x;

      p = (USHORT *) _gt_ScreenPtr( usRow, usCol );

      for ( x = 0; x < len; x++) {
         *p++ = byAttr + (*str++);
      }

      refresh_buffer( usRow, usCol, len * 2 );

   } else {
      VioWrtCharStrAtt( ( char * ) str, ( USHORT ) len, usRow, usCol, ( BYTE * ) &attr, 0 );

   }
}


int HB_GT_FUNC(gt_RectSize( USHORT rows, USHORT cols ))
{
   return rows * cols * 2;
}


void HB_GT_FUNC(gt_GetText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE *dest ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRight, dest));

   if (s_uiDispCount > 0) {
      USHORT x, y;

      for( y = usTop; y <= usBottom; y++ ) {
         for( x = usLeft; x <= usRight; x++ ) {
            HB_GT_FUNC(gt_xGetXY( y, x, dest + 1, dest ));
            dest += 2;
         }
      }

   } else {
      USHORT width, y;

      width = ( USHORT ) ( ( usRight - usLeft + 1 ) * 2 );
      for( y = usTop; y <= usBottom; y++ )
      {
         VioReadCellStr( dest, &width, y, usLeft, 0 );
         dest += width;
      }
   }
}


void HB_GT_FUNC(gt_PutText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE *srce ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRight, srce));

   s_Dirty = TRUE;

   if (s_uiDispCount > 0) {
      USHORT x, y;

      for( y = usTop; y <= usBottom; y++ ) {
         for( x = usLeft; x <= usRight; x++ ) {
            HB_GT_FUNC(gt_xPutch( y, x, *( srce + 1 ), *srce ));
            srce += 2;
         }
      }

      refresh_buffer( usTop, usLeft, ( ( ( usBottom * _GetScreenWidth() ) + usRight ) * 2 ) -
                                     ( ( ( usTop * _GetScreenWidth() ) + usLeft ) * 2 ) + 1 );

   } else {
      USHORT width, y;

      width = ( USHORT ) ( ( usRight - usLeft + 1 ) * 2 );
      for( y = usTop; y <= usBottom; y++ ) {
         VioWrtCellStr( srce, width, y, usLeft, 0 );
         srce += width;
      }
   }
}


void HB_GT_FUNC(gt_SetAttribute( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", usTop, usLeft, usBottom, usRight, (int) attr));

   s_Dirty = TRUE;

   if(s_uiDispCount >0) {

      USHORT x, y;

      for( y = usTop; y <= usBottom; y++ ) {

         BYTE scratchattr;
         BYTE ch;

         for( x = usLeft; x <= usRight; x++ ) {
            HB_GT_FUNC(gt_xGetXY( y, x, &scratchattr, &ch ));
            HB_GT_FUNC(gt_xPutch( y, x, attr, ch ));
         }
      }

      refresh_buffer( usTop, usLeft, ( ( ( usBottom * _GetScreenWidth() ) + usRight ) * 2 ) -
                                     ( ( ( usTop * _GetScreenWidth() ) + usLeft ) * 2 ) + 1 );


   } else {

      USHORT width, y;

      /*
         assume top level check that coordinate are all valid and fall
         within visible screen, else if width cannot be fit on current line
         it is going to warp to the next line
      */
      width = ( USHORT ) ( usRight - usLeft + 1 );
      for( y = usTop; y <= usBottom; y++ )
         VioWrtNAttr( &attr, width, y, usLeft, 0 );
   }
}


/* NOTE: 21/08/2001 - <maurilio.longo@libero.it>
         If you need to call this function from inside gtos2.c then there is
         something WRONG with your code :-)
*/
void HB_GT_FUNC(gt_DispBegin( void ))
{
   /* NOTE: 02/04/2000 - <maurilio.longo@libero.it>
            added support for DispBegin() and DispEnd() functions.
            OS/2 has an off screen buffer for every vio session. When a program calls DispBegin()
            every function dealing with screen writes/reads uses this buffer. DispEnd() resyncronizes
            off screen buffer with screen
   */
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

   /* pointer to the only one available screen buffer is set on startup,
      we only need to keep track of nesting */
   ++s_uiDispCount;
}


void HB_GT_FUNC(gt_DispEnd( void ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

   if (--s_uiDispCount == 1) {
      refresh_buffer( 0, 0, s_usLVBlength );
   }
}


BOOL HB_GT_FUNC(gt_SetMode( USHORT uiRows, USHORT uiCols ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", uiRows, uiCols));

   s_Dirty = TRUE;

   s_vi.cb = sizeof( VIOMODEINFO );
   VioGetMode( &s_vi, 0 );        /* fill structure with current settings */
   s_vi.row = uiRows;
   s_vi.col = uiCols;
   return ! ( BOOL ) VioSetMode( &s_vi, 0 );   /* 0 = Ok, other = Fail */
}


BOOL HB_GT_FUNC(gt_GetBlink())
{
   VIOINTENSITY vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

   vi.cb   = sizeof( VIOINTENSITY );    /* 6                          */
   vi.type = 2;                         /* get intensity/blink toggle */
   VioGetState( &vi, 0 );
   return ( vi.fs == 0 );               /* 0 = blink, 1 = intens      */
}


void HB_GT_FUNC(gt_SetBlink( BOOL bBlink ))
{
   VIOINTENSITY vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

   s_Dirty = TRUE;

   vi.cb   = sizeof( VIOINTENSITY );    /* 6                          */
   vi.type = 2;                         /* set intensity/blink toggle */
   vi.fs   = ( bBlink ? 0 : 1 );        /* 0 = blink, 1 = intens      */
   VioSetState( &vi, 0 );
}


void HB_GT_FUNC(gt_Tone( double dFrequency, double dDuration ))
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

   /* The conversion from Clipper timer tick units to
      milliseconds is * 1000.0 / 18.2. */

   dFrequency = HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 );
   dDuration = dDuration * 1000.0 / 18.2; /* milliseconds */

   while( dDuration > 0.0 )
   {
      USHORT temp = ( USHORT ) HB_MIN( HB_MAX( 0, dDuration ), USHRT_MAX );

      dDuration -= temp;
      if( temp <= 0 )
      {
         /* Ensure that the loop gets terminated when
            only a fraction of the delay time remains. */
         dDuration = -1.0;
      }
      else
      {
         DosBeep( ( USHORT ) dFrequency, temp );
      }
   }
}


char * HB_GT_FUNC(gt_Version( int iType ))
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_Version()" ) );

   if ( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: OS/2 console";
}


USHORT HB_GT_FUNC(gt_DispCount())
{
   return s_uiDispCount - 1;
}


void HB_GT_FUNC(gt_Replicate( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar, ULONG nLength ))
{
   USHORT byte = (byAttr << 8) + byChar;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", uiRow, uiCol, byAttr, byChar, nLength));

   s_Dirty = TRUE;

   if (s_uiDispCount > 0) {
      register USHORT *p;
      register int x;

      p = (USHORT *) _gt_ScreenPtr( uiRow, uiCol );

      for ( x = 0; x < nLength; x++ ) {
         *p++ = byte;
      }

      refresh_buffer( uiRow, uiCol, nLength * 2 );

   } else {
      VioWrtNCell((PBYTE) &byte, nLength, uiRow, uiCol, 0);

   }
}

USHORT HB_GT_FUNC(gt_Box( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * szBox, BYTE byAttr ))
{
   USHORT ret = 1;
   SHORT Row;
   SHORT Col;
   SHORT Height;
   SHORT Width;

   s_Dirty = TRUE;

   if( ( Left   >= 0 && Left   < _GetScreenWidth()  )  ||
       ( Right  >= 0 && Right  < _GetScreenWidth()  )  ||
       ( Top    >= 0 && Top    < _GetScreenHeight() )  ||
       ( Bottom >= 0 && Bottom < _GetScreenHeight() ) )
   {

      /* Collect all changes to buffer before refreshing full box */
      HB_GT_FUNC(gt_DispBegin());

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

      if( Height > 1 && Width > 1 && Top >= 0 && Top < _GetScreenHeight() && Left >= 0 && Left < _GetScreenWidth() ) {
         HB_GT_FUNC(gt_xPutch( Top, Left, byAttr, szBox[ 0 ] )); /* Upper left corner */
         refresh_buffer( Top, Left, 2 );
      }

      Col = ( Height > 1 ? Left + 1 : Left );
      if(Col < 0 )
      {
         Width += Col;
         Col = 0;
      }
      if( Right >= _GetScreenWidth() )
      {
         Width -= Right - _GetScreenWidth();
      }

      if( Col <= Right && Col < _GetScreenWidth() && Top >= 0 && Top < _GetScreenHeight() )
         HB_GT_FUNC(gt_Replicate( Top, Col, byAttr, szBox[ 1 ], Width + ( (Right - Left) > 1 ? -2 : 0 ) )); /* Top line */

      if( Height > 1 && (Right - Left) > 1 && Right < _GetScreenWidth() && Top >= 0 && Top < _GetScreenHeight() ) {
         HB_GT_FUNC(gt_xPutch( Top, Right, byAttr, szBox[ 2 ] )); /* Upper right corner */
         refresh_buffer( Top, Right, 2 );
      }

      if( szBox[ 8 ] && Height > 2 && Width > 2 )
      {
         for( Row = Top + 1; Row < Bottom; Row++ )
         {
            if( Row >= 0 && Row < _GetScreenHeight() )
            {
               Col = Left;

               if( Col < 0 )
                  Col = 0; /* The width was corrected earlier. */
               else {
                  HB_GT_FUNC(gt_xPutch( Row, Col++, byAttr, szBox[ 7 ] )); /* Left side */
                  refresh_buffer( Row, Col - 1, 2 );
               }

               HB_GT_FUNC(gt_Replicate( Row, Col, byAttr, szBox[ 8 ], Width - 2 )); /* Fill */

               if( Right < _GetScreenWidth() ) {
                  HB_GT_FUNC(gt_xPutch( Row, Right, byAttr, szBox[ 3 ] )); /* Right side */
                  refresh_buffer( Row, Right, 2 );
               }
            }
         }
      }
      else
      {
         for( Row = ( Width > 1 ? Top + 1 : Top ); Row < ( (Right - Left ) > 1 ? Bottom : Bottom + 1 ); Row++ )
         {
            if( Row >= 0 && Row < _GetScreenHeight() )
            {
               if( Left >= 0 && Left < _GetScreenWidth() ) {
                  HB_GT_FUNC(gt_xPutch( Row, Left, byAttr, szBox[ 7 ] )); /* Left side */
                  refresh_buffer( Row, Left, 2 );
               }

               if( ( Width > 1 || Left < 0 ) && Right < _GetScreenWidth() ) {
                  HB_GT_FUNC(gt_xPutch( Row, Right, byAttr, szBox[ 3 ] )); /* Right side */
                  refresh_buffer( Row, Right, 2 );
               }

            }
         }
      }

      if( Height > 1 && Width > 1 )
      {
         if( Left >= 0 && Bottom < _GetScreenHeight() ) {
            HB_GT_FUNC(gt_xPutch( Bottom, Left, byAttr, szBox[ 6 ] )); /* Bottom left corner */
            refresh_buffer( Bottom, Left, 2 );
         }

         Col = Left + 1;
         if( Col < 0 )
            Col = 0; /* The width was corrected earlier. */

         if( Col <= Right && Bottom < _GetScreenHeight() )
            HB_GT_FUNC(gt_Replicate( Bottom, Col, byAttr, szBox[ 5 ], Width - 2 )); /* Bottom line */

         if( Right < _GetScreenWidth() && Bottom < _GetScreenHeight() ) {
            HB_GT_FUNC(gt_xPutch( Bottom, Right, byAttr, szBox[ 4 ] )); /* Bottom right corner */
            refresh_buffer( Bottom, Right, 2 );
         }
      }
      ret = 0;

      refresh_buffer( Top, Left, ( ( ( Bottom * _GetScreenWidth() ) + Right ) * 2 ) -
                                 ( ( ( Top * _GetScreenWidth() ) + Left ) * 2 ) + 1 );
      HB_GT_FUNC(gt_DispEnd());

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

   s_Dirty = TRUE;

   if( Row >= 0 && Row < _GetScreenHeight() )
   {
      if( Left < 0 )
         Left = 0;
      else if( Left >= _GetScreenWidth() )
         Left = _GetScreenWidth() - 1;

      if( Right < 0 )
         Right = 0;
      else if( Right >= _GetScreenWidth() )
         Right = _GetScreenWidth() - 1;

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

   s_Dirty = TRUE;

   if( Col >= 0 && Col < _GetScreenWidth() )
   {
      if( Top < 0 )
         Top = 0;
      else if( Top >= _GetScreenHeight() )
         Top = _GetScreenHeight() - 1;

      if( Bottom < 0 )
         Bottom = 0;
      else if( Bottom >= _GetScreenHeight() )
         Bottom = _GetScreenHeight() - 1;

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

      refresh_buffer( Top, Col, ( ( ( Bottom * _GetScreenWidth() ) + Col ) * 2 ) -
                                ( ( ( Top * _GetScreenWidth() ) + Col ) * 2 ) + 1 );
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
    hb_fsWriteLarge( s_iStdOut, ( BYTE * ) pbyStr, ulLen );
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
int HB_GT_FUNC(gt_info(int iMsgType, BOOL bUpdate, int iParam, void *vpParam ))
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

/* ********** Graphics API ********** */

int HB_GT_FUNC(gt_gfxPrimitive( int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor ))
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
    gt_funcs->SetDispCP             = HB_GT_FUNC( gt_SetDispCP );
    gt_funcs->SetKeyCP              = HB_GT_FUNC( gt_SetKeyCP );
    gt_funcs->SetClipboard          = HB_GT_FUNC( gt_SetClipboard );
    gt_funcs->GetClipboard          = HB_GT_FUNC( gt_GetClipboard );
    gt_funcs->GetClipboardSize      = HB_GT_FUNC( gt_GetClipboardSize );

    /* Graphics API */
/*
    gt_funcs->gfxPrimitive          = HB_GT_FUNC( gt_gfxPrimitive );
    gt_funcs->gfxText               = HB_GT_FUNC( gt_gfxText );
*/
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


/* ********************************************************************** */

static HB_GT_INIT gtInit = { HB_GT_DRVNAME( HB_GT_NAME ),
                             HB_GT_FUNC(gtFnInit), HB_GT_FUNC(mouseFnInit) };

HB_GT_ANNOUNCE( HB_GT_NAME );

HB_CALL_ON_STARTUP_BEGIN( _hb_startup_gt_Init_ )
   hb_gtRegister( &gtInit );
HB_CALL_ON_STARTUP_END( _hb_startup_gt_Init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_startup_gt_Init_
#endif

#endif  /* HB_MULTI_GT */


/* *********************************************************************** */
