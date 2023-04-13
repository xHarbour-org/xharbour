/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Header file for optional memory routines when for compiler creation
 *
 * Copyright 2005 Andi Jahja <xharbour@cbn.net.id>
 * www - http://www.xharbour.org
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
 * As a special exception, the xHarbour Project gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the xHarbour
 * Project under the name xHarbour.  If you copy code from other
 * xHarbour Project or Free Software Foundation releases into a copy of
 * xHarbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
  *****************************************************
  ***** ONLY TO BE USED WHEN CREATING HARBOUR.EXE *****
  *****************************************************
  Background:
  There are two definitions for each of the following functions:
     hb_xgrab    => source/compiler/harbour.c  source/vm/fm.c
     hb_xfree    => source/compiler/harbour.c  source/vm/fm.c
     hb_xrealloc => source/compiler/harbour.c  source/vm/fm.c
  These definitions may help to solve probability of name conflict.
  *****************************************************
  ***** ONLY TO BE USED WHEN CREATING HARBOUR.EXE *****
  *****************************************************
*/

#ifndef _HB_EXEMEM_H_
#define _HB_EXEMEM_H_

#if defined( HB_EXEMEM_USED )

#undef hb_xgrab
#undef hb_xfree
#undef hb_xrealloc

extern void * _hb_xgrab( ULONG ulSize );
extern void   _hb_xfree( void * pMem );
extern void * _hb_xrealloc( void * pMem, ULONG ulSize );

#define hb_xgrab    _hb_xgrab
#define hb_xfree    _hb_xfree
#define hb_xrealloc _hb_xrealloc

#endif /* HB_EXEMEM_USED */

#endif /* _HB_EXEMEM_H_  */
