/*
 *  $Id: util.prg,v 1.0 2004/01/16 121:08:40 modalsist Exp $
 */

/*
 * xHarbour Project source code:
 * LibCT util functions used by another libct functions.
 *
 * Default()
 * IsDir()
 * Occurs()
 * WhatWin()
 *
 * Copyright 2004 Eduardo Fernandes <eduardo@modalsistemas.com.br>
 * http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modIFy
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
 * along with this software; see the file COPYING.  IF not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, IF you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  IF you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modIFied files, you must delete
 * this exception notice from them.
 *
 * IF you write modIFications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modIFications.
 * IF you do not wish that, delete this exception notice.
 *
 */
#include "common.ch"

/*  $DOC$
 *  $FUNCNAME$
 *      DEFAULT()
 *  $CATEGORY$
 *      LIBCT MemVar functions
 *  $ONELINER$
 *      Assign a default value to a NIL argument.
 *  $SYNTAX$
 *      Default( @<uVar>, <uDefault> ) ---> NIL
 *  $ARGUMENTS$
 *      <uVar> The variable to which the default value will be assigned.
 *      <uVar> must be passed by reference to the Default() function.
 * 
 *      <uDefault> The default value to assign.
 *  $RETURNS$
 *      NIL
 *  $DESCRIPTION$
 *      Default() assigns <uDefault> to <uVar> IF <uVar> is equal to NIL.
 *      It is functionally equivalent to either of these code samples:
 *
 *      IF uVar == NIL
 *         uVar := uDefault
 *      ENDIF
 *
 *      IF IsNil(uVar)
 *   		uVar := uDefault
 *      ENDIF
 *  $EXAMPLES$
 *      Function Test( Arg1, Arg2 ... )
 *      Default(@Arg1,1)
 *      Default(@Arg2,"A")
 *  $TESTS$
 *      See examples
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function is CA-Visual Objects compatible.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is util.prg, library is libct.lib
 *  $SEEALSO$
 *      DEFAULT TO COMMAND
 *  $END$
 */

*********************************
FUNCTION Default( cVar , uValue )
*********************************

IF cVar == NIL
   cVar := uValue
ENDIF

RETURN (Nil)

/*  $DOC$
 *  $FUNCNAME$
 *      ISDIR()
 *  $CATEGORY$
 *      LIBCT Directory management
 *  $ONELINER$
 *      VerIFy IF a directory exist.
 *  $SYNTAX$
 *      IsDir( <cDirectory> ) ---> lSuccess
 *  $ARGUMENTS$
 *      <cDirectory> The directory name to verIFy.
 *  $RETURNS$
 *      True IF <cDirectory> exist, otherwise false.
 *  $DESCRIPTION$
 *  $STATUS$
 *      Ready
 *  $PLATFORMS$
 *      Windows
 *  $FILES$
 *      Source is util.prg, library is libct.lib
 *  $SEEALSO$
 *      DirName(),CurDir(),DirMake(),DirChange()
 *  $END$
 */
****************************
FUNCTION IsDir( cDirectory )
****************************

   LOCAL lExist,cCurDir

   IF valtype( cDirectory ) != "C"
      RETURN ( .F. )
   ENDIF

   cCurDir := DiskName()+":\"+CurDir()

   lExist := ( DirChange( cDirectory ) == 0  )

   IF lExist
      DirChange( cCurDir )
   ENDIF

RETURN ( lExist )


/*  $DOC$
 *  $FUNCNAME$
 *      OCCURS()
 *  $CATEGORY$
 *      LIBCT String functions
 *  $ONELINER$
 *      Return the number of times that a substring occurs in a string.
 *  $SYNTAX$
 *      Occurs( <cSearch> , <cTarget> ) ---> nTimes
 *  $ARGUMENTS$
 *      <cSearch> The substring for which to search.
 *      <cTarget> The string in which to search.  (To specIFy an offset, use Occurs3()).
 *  $RETURNS$
 *      The number of times that <cSearch> appears in <cTarget>.
 *  $DESCRIPTION$
 *      Occurs() is case-sensitive.
 *  $EXAMPLES$
 *      This example shows typical use of Occurs():
 *  LOCAL cSearch,cTarget
 *      cSearch := "any"
 *      cTarget := "Anything goes anyway anytime."
 *      ? Occurs(cSearch, cTarget) // 2
 *      ? Occurs("any","ANY any")   // 1
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function is CA-Visual Objects compatible.
 *  $PLATFORMS$
 *      Windows
 *  $FILES$
 *      Source is util.prg, library is libct.lib
 *  $SEEALSO$
 *  $END$
 */
********************************
FUNCTION Occurs( cStr1 , cStr2 )
********************************

   LOCAL i,nOccurs

   i := 0
   nOccurs := 0

   IF valtype( cStr1 ) != "C" .or. valtype( cStr2 ) != "C"
      RETURN ( nOccurs )
   ENDIF

   IF Len( cStr1 ) = 0 .or. Len( cStr2 ) = 0
      RETURN ( nOccurs )
   ENDIF

   LOCALi := 1 to Len( cStr2 ) 
      IF cStr1 == substr( cStr2 , i , Len( cStr1) )
         nOccurs += 1
      ENDIF  
   NEXT

RETURN ( nOccurs )

/*  $DOC$
 *  $FUNCNAME$
 *        WHATWIN()
 *  $CATEGORY$
 *      LIBCT System information
 *  $ONELINER$
 *      VerIFy Windows type operational system.
 *  $SYNTAX$
 *		WhatWin() -> nWindowsType
 *  $ARGUMENTS$
 *		Nome.
 *  $RETURNS$
 *		A number indicating the Windows type.
 *  $DESCRIPTION$
 *		WhatWin() is a system function based in OS() function.
 *  $EXAMPLES$
 *      IF WhatWin() = 1
 *			? "This is Windows 95"
 *      ELSEIF WhatWin() = 2
 *			? "This is Windows 98"
 *      ELSEIF WhatWin() = 3
 *			? "This is Windows ME"
 *      ELSEIF WhatWin() = 4
 *			? "This is Windows NT"
 *      ELSEIF WhatWin() = 5
 *			? "This is Windows 2000"
 *      ELSEIF WhatWin() = 6
 *			? "This is Windows XP"
 *      ELSE
 *       ? "This is unknow Windows system"
 *      ENDIF
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function is new in xHarbour.
 *  $PLATFORMS$
 *      Windows
 *  $FILES$
 *      Source is util.prg, library is libct.lib
 *  $SEEALSO$
 *  $END$
 */
******************
FUNCTION WhatWin()
******************
   LOCAL nRet
   
   nRet := 0

   IF ( "Windows 95" $ OS() )
      nRet := 1
   ELSEIF ( "Windows 98" $ OS() )
      nRet := 2
   ELSEIF ( "Windows ME" $ OS() .or. "Windows Millenium" $ OS())
      nRet := 3
   ELSEIF ( "Windows NT" $ OS())
      nRet := 4
   ELSEIF ( "Windows 2000" $ OS() )
      nRet := 5
   ELSEIF ( "Windows XP" $ OS() )
      nRet := 6
   ENDIF

RETURN ( nRet )
