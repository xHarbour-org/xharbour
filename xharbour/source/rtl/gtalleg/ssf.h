/*
 * $Id: ssf.h,v 1.3 2004/01/22 01:01:46 maurifull Exp $
 */

/*
 *
 * This file was conceived while I was developing an allegro based gt
 * (gtAlleg) for xHarbour, so it is brought under the same license terms.
 *
 */

/*
 * xHarbour Project source code:
 * Simple Scalable Font library, main header file.
 *
 * Copyright 2004 Mauricio Abre <maurifull@datafull.com>
 * www - http: (yet to be constructed...)
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

#ifndef _SSF_H_
#define _SSF_H_

#include <allegro.h>

typedef enum {
    SSF_NONE,
    SSF_LINE,
    SSF_BOX,
    SSF_SPLINE1,
    SSF_SPLINE2,
    SSF_TRIANGLE,
    SSF_COLOR
} ssfType;

#ifndef SSF_MAXFRAMES
#define SSF_MAXFRAMES 128
#endif

#define THICK_LEFT	0
#define THICK_UP	1
#define THICK_RIGHT	2
#define THICK_DOWN	3

typedef struct _ssfFrame {
    char ftype;
    unsigned short left, top, right, bottom, thick, thickdir;
} ssfFrame;

typedef struct _ssfGlyph {
    int num;
    ssfFrame frames[SSF_MAXFRAMES];
} ssfGlyph;

typedef struct _ssfFont {
    unsigned short fsize;
    ssfGlyph **chars;
} ssfFont;

extern ssfFont *ssfDefaultFont;

void ssfSetFontSize(ssfFont *sfont, unsigned short fsize);
char ssfDrawChar(AL_BITMAP *dst, ssfFont *sfont, char c, int x, int y, int color);

#endif  // _SSF_H_
