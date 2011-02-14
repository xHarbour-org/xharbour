/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   Test for CT3 bit functions
 *
 * Copyright 2006 Pavel Tsarenko <tpe2@mail.ru>
 *
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

function main

   cls
   ? "NumAnd(1, 3, 7)", NumAnd(1, 3, 7)
   ? "NumAnd('1', '3', 'F')", NumAnd('1', '3', 'F')
   ? "NumAnd('10001', '10000')", NumAnd('10001', '10000')
   ? "NumAnd('F0000001', 'F0000000')", NumAnd('F0000001', 'F0000000')

   ? "NumOr(1, 3, 7)", NumOr(1, 3, 7)
   ? "NumOr('1', '3', 'F')", NumOr('1', '3', 'F')
   ? "NumOr('10001', '10000')", NumOr('10001', '10000')
   ? "NumOr('F0000001', 'F0000000')", NumOr('F0000001', 'F0000000')
   ? "NumOr('FFFF0000', 'FFFF')", NumOr('FFFF0000', 'FFFF')

   ? "NumXor(1, 3, 7)", NumXor(1, 3, 7)
   ? "NumXor('1', '3', 'F')", NumXor('1', '3', 'F')
   ? "NumXor('10001', '30000')", NumXor('10001', '30000')
   ? "NumXor('F0000001', '00000003')", NumXor('F0000001', '00000003')

   ? "NumNot(1)", NumNot(1)
   ? "NumNot('3')", NumNot('3')
   ? "NumNot('10001')", NumNot('10001')
   ? "NumNot('F0000001')", NumNot('F0000001')

   ? "NumRol('1000', 8)", NumRol('1000', 8)
   ? "NumRol('1001', 3)", NumRol('1001', 3)
   ? "NumMirr(128 + 64 + 8 + 2)", NumMirr(128 + 64 + 8 + 2)

return nil
