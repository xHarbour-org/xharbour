/*
 * $Id: gtdos.c,v 1.5 2002/11/13 15:37:05 walito Exp $
 */

/*
 * Harbour Project source code:
 * Video subsystem for DOS compilers
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 * http://www.xharbour.org
 *
 * See doc/license.txt for licensing terms.
 *
 */

/*
 *  This module is based on VIDMGR by Andrew Clarke and modified for
 *  the Harbour project
 */

#ifndef GTQTC_H
#define GTQTC_H

/* c++ vs c interface */
//extern "C" {

static void hb_gt_xGetXY( USHORT cRow, USHORT cCol, BYTE * attr, BYTE * ch );
static void hb_gt_xPutch( USHORT cRow, USHORT cCol, BYTE attr, BYTE ch );

static char hb_gt_GetScreenMode( void );
static void hb_gt_SetCursorSize( char start, char end );
static void hb_gt_GetCursorSize( char * start, char * end );

void hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr );
void hb_gt_Exit( void );
int hb_gt_ExtendedKeySupport();
int hb_gt_ReadKey( HB_inkey_enum eventmask );
BOOL hb_gt_AdjustPos( BYTE * pStr, ULONG ulLen );
BOOL hb_gt_IsColor( void );
static char hb_gt_GetScreenMode( void );
USHORT hb_gt_GetScreenWidth( void );
USHORT hb_gt_GetScreenHeight( void );
void hb_gt_SetPos( SHORT iRow, SHORT iCol, SHORT iMethod );
static void hb_gt_SetCursorSize( char start, char end );
static void hb_gt_GetCursorSize( char * start, char *end );
USHORT hb_gt_GetCursorStyle( void );
void hb_gt_SetCursorStyle( USHORT style );
static void hb_gt_xGetXY( USHORT cRow, USHORT cCol, BYTE * attr, BYTE * ch );
static void hb_gt_xPutch( USHORT cRow, USHORT cCol, BYTE attr, BYTE ch );
void hb_gt_Puts( USHORT cRow, USHORT cCol, BYTE attr, BYTE *str, ULONG len );
int hb_gt_RectSize( USHORT rows, USHORT cols );
void hb_gt_GetText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * dest );
void hb_gt_PutText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * srce );
void hb_gt_SetAttribute( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr );
SHORT hb_gt_Col( void );
SHORT hb_gt_Row( void );
void hb_gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr, SHORT sVert, SHORT sHoriz );
void hb_gt_DispBegin( void );
void hb_gt_DispEnd( void );
BOOL hb_gt_GetBlink();
void hb_gt_SetBlink( BOOL bBlink );
void hb_gt_Tone( double dFrequency, double dDuration );
char * hb_gt_Version( void );
USHORT hb_gt_DispCount();
void hb_gt_Replicate( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar, ULONG nLength );
USHORT hb_gt_Box( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
                  BYTE * szBox, BYTE byAttr );
USHORT hb_gt_BoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr );
USHORT hb_gt_HorizLine( SHORT Row, SHORT Left, SHORT Right, BYTE byChar, BYTE byAttr );
USHORT hb_gt_VertLine( SHORT Col, SHORT Top, SHORT Bottom, BYTE byChar, BYTE byAttr );
static USHORT hb_gt_GetDisplay( void );
BOOL hb_gt_SetMode( USHORT uiRows, USHORT uiCols );
BOOL hb_gt_PreExt();
BOOL hb_gt_PostExt();
BOOL hb_gt_Suspend();
BOOL hb_gt_Resume();
void hb_gt_OutStd( BYTE * pbyStr, ULONG ulLen );
void hb_gt_OutErr( BYTE * pbyStr, ULONG ulLen );
//}

// references to the qtconsole lib
class QTconsoleApp;
extern QTconsoleApp *qtcapp;

#endif

/* end of gtQTc.h */
