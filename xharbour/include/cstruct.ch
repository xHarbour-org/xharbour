/*
 * $Id: cstruct.ch,v 1.1 2002/06/12 11:53:06 ronpinkas Exp $
 */

/*
 * xHarbour Project source code:
 *
 * Copyright 2000 Ron Pinkas <ronpinkas@profit-master.com>
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

#define CTYPE_CHAR 1
#define CTYPE_UNSIGNED_CHAR -1

#define CTYPE_CHAR_PTR 10
#define CTYPE_UNSIGNED_CHAR_PTR -10

#define CTYPE_INT 2
#define CTYPE_UNSIGNED_INT -2

#define CTYPE_INT_PTR 20
#define CTYPE_UNSIGNED_INT_PTR -20

#define CTYPE_LONG 3
#define CTYPE_UNSIGNED_LONG -3

#define CTYPE_LONG_PTR 30
#define CTYPE_UNSIGNED_LONG_PTR -30

#define CTYPE_FLOAT 4

#define CTYPE_FLOAT_PTR 40

#define CTYPE_DOUBLE 5

#define CTYPE_DOUBLE_PTR 50

#define CTYPE_VOID_PTR 6

#define CTYPE_STRUCTURE_PTR 1000

// Excluse from C compilation
#ifdef _SET_CH
   #command C STRUCTURE <!stru!> [ALIGN <align> ] => INIT PROCEDURE __INIT_<stru>; MEMVAR <stru>; PUBLIC <stru> := __ActiveStructure( #<stru>, { #<stru> }, <align> )
   #command MEMBER <!elem!> AS <type> => aAdd( __ActiveStructure(), {#<elem>, <type> } )
   #command MEMBER <!elem!> AS C STRUCTURE <!stru!> => aAdd( __ActiveStructure(), { #<elem>, HB_CStructureID( #<stru> ) } )
   #command END C STRUCTURE [<!stru!>] => ; __ActiveStructure( NIL ); RETURN
   #translate := C STRUCTURE <!stru!> => := HB_CStructure( #<stru>, M-><stru> )
#endif
