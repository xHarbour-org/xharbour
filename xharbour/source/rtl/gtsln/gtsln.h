/*
 * $Id: gtsln.h,v 1.6 2003/05/21 09:35:37 druzus Exp $
 */

/*
 * Harbour Project source code:
 * Video subsystem based on Slang screen library.
 *
 * Copyright 2000 Marek Paliwoda <paliwoda@inetia.pl>
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

/* *********************************************************************** */

/* This definition has to be placed before #include "hbapigt.h" */
#define HB_GT_NAME	SLN

#include <slang.h>
#include "hbapigt.h"
#include "hbapifs.h"
#include "inkey.ch"
#include <unistd.h>
#include <signal.h>

#ifndef HB_OS_DARWIN
#include <time.h>
#endif

#ifndef HB_CDP_SUPPORT_OFF
#include "hbapicdp.h"
extern PHB_CODEPAGE s_cdpage;
#endif

#if UTF8 && SLANG_VERSION >= 10409
    #define HB_SLN_UTF8
#endif

/* missing defines in previous versions of Slang - this may not work ok ! */
#if SLANG_VERSION < 10400
    typedef unsigned short SLsmg_Char_Type;
    #define SLSMG_EXTRACT_CHAR( x ) ( ( x ) & 0xFF )
    #define SLSMG_EXTRACT_COLOR( x ) ( ( ( x ) >> 8 ) & 0xFF )
    #define SLSMG_BUILD_CHAR( ch, color ) ( ( ( SLsmg_Char_Type ) ( unsigned char )( ch ) ) | ( ( color ) << 8 ) )

#if SLANG_VERSION < 10308
    #define SLSMG_DIAMOND_CHAR    0x04
    #define SLSMG_DEGREE_CHAR     0xF8
    #define SLSMG_PLMINUS_CHAR    0xF1
    #define SLSMG_BULLET_CHAR     0xF9
    #define SLSMG_LARROW_CHAR     0x1B
    #define SLSMG_RARROW_CHAR     0x1A
    #define SLSMG_DARROW_CHAR     0x19
    #define SLSMG_UARROW_CHAR     0x18
    #define SLSMG_BOARD_CHAR      0xB2
    #define SLSMG_BLOCK_CHAR      0xDB
    /*
    #define SLSMG_BOARD_CHAR      'h'
    #define SLSMG_BLOCK_CHAR      '0'
    */
#endif
#endif

#define HB_SLN_ACSC_ATTR        SLSMG_BUILD_CHAR( 0, 0x80 )
#define HB_SLN_BUILD_CHAR( ch, attr )   ( s_currOutTab[ (BYTE) ch ] | s_colorTab[ (BYTE) attr ] )

#ifndef HB_SLN_UTF8
extern int SLsmg_Is_Unicode;
#endif

/* *********************************************************************** */

/* if we can not manipulate cursor state */
#define SC_UNAVAIL -1


/* *********************************************************************** */

extern BOOL hb_gt_UnderLinuxConsole;
extern BOOL hb_gt_UnderXterm;
extern unsigned char s_inputTab[ 256 ];
#ifndef HB_CDP_SUPPORT_OFF
extern PHB_CODEPAGE s_gtSln_cdpIN;
#endif

/* *********************************************************************** */

void HB_GT_FUNC(gt_Init_TermType());
int  HB_GT_FUNC(mouse_Inkey( HB_inkey_enum EventMask ));
void HB_GT_FUNC(mouse_FixTrash());

/* *********************************************************************** */
