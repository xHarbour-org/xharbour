/*
 * $Id: common.ch,v 1.3 2003/11/03 20:09:24 brianhays Exp $
 */

/*
 * Harbour Project source code:
 * Header file for common macros
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

#ifndef HB_COMMON_CH_
#define HB_COMMON_CH_

/* Friendly logical aliases */
#define TRUE                    .T.
#define FALSE                   .F.
#define YES                     .T.
#define NO                      .F.

/* Type checking macros */

#xtranslate ISNIL( <xValue> )       => ( HB_ISNIL( <xValue> ) )
#xtranslate ISARRAY( <xValue> )     => ( HB_ISARRAY( <xValue> ) )
#xtranslate ISBLOCK( <xValue> )     => ( HB_ISBLOCK( <xValue> ) )
#xtranslate ISCHARACTER( <xValue> ) => ( HB_ISSTRING( <xValue> ) )
#xtranslate ISDATE( <xValue> )      => ( HB_ISDATE( <xValue> ) )
#xtranslate ISLOGICAL( <xValue> )   => ( HB_ISLOGICAL( <xValue> ) )
#xtranslate ISMEMO( <xValue> )      => ( HB_ISMEMO( <xValue> ) )
#xtranslate ISNUMBER( <xValue> )    => ( HB_ISNUMERIC( <xValue> ) )
#xtranslate ISOBJECT( <xValue> )    => ( HB_ISOBJECT( <xValue> ) )

#xtranslate VALTYPE( <Expr> ) == "A" => ( HB_ISARRAY( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) == "B" => ( HB_ISBLOCK( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) == "C" => ( HB_ISSTRING( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) == "D" => ( HB_ISDATE( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) == "L" => ( HB_ISLOGICAL( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) == "N" => ( HB_ISNUMERIC( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) == "O" => ( HB_ISOBJECT( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) == "U" => ( HB_ISNIL( <Expr> ) )

#xtranslate VALTYPE( <Expr> ) != "A" => !( HB_ISARRAY( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) != "B" => !( HB_ISBLOCK( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) != "C" => !( HB_ISSTRING( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) != "D" => !( HB_ISDATE( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) != "L" => !( HB_ISLOGICAL( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) != "N" => !( HB_ISNUMERIC( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) != "O" => !( HB_ISOBJECT( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) != "U" => !( HB_ISNIL( <Expr> ) )

#xtranslate VALTYPE( <Expr> ) <> "A" => !( HB_ISARRAY( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) <> "B" => !( HB_ISBLOCK( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) <> "C" => !( HB_ISSTRING( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) <> "D" => !( HB_ISDATE( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) <> "L" => !( HB_ISLOGICAL( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) <> "N" => !( HB_ISNUMERIC( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) <> "O" => !( HB_ISOBJECT( <Expr> ) )
#xtranslate VALTYPE( <Expr> ) <> "U" => !( HB_ISNIL( <Expr> ) )

/* DEFAULT and UPDATE commands */
#xcommand DEFAULT <v1> TO <x1> [, <vn> TO <xn> ] => ;
                                IF <v1> == NIL ; <v1> := <x1> ; END ;
                                [; IF <vn> == NIL ; <vn> := <xn> ; END ]

#command UPDATE <v1> IF <exp> TO <v2> => ;
                                IF <exp> ; <v1> := <v2> ; END

#endif /* HB_COMMON_CH_ */
