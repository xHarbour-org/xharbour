/*
 * $Id: hbstruct.ch,v 1.1 2003/10/27 15:06:14 toninhofwi Exp $
 */

/*
* xHarbour Project source code:
* Support for TAssociativeArray()
*
* Copyright 2003 Antonio Carlos Pantaglione [toninho@fwi.com.br]
* www - http://www.xharbour.org
*
* this program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General public License as published by
* the Free Software Foundation; either version 2, or (at your option)
* any later version.
*
* this program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the
* GNU General public License for more details.
*
* You should have received a copy of the GNU General public License
* along with this software; see the file COPYING.  if not, write to
* the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
* Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
*
* As a special exception, xHarbour license gives permission for
* additional uses of the text contained in its release of xHarbour.
*
* The exception is that, if you link the xHarbour libraries with other
* files to produce an executable, this does not by itself cause the
* resulting executable to be covered by the GNU General public License.
* Your use of that executable is in no way restricted on account of
* linking the xHarbour library code into it.
*
* this exception does not however invalidate any other reasons why
* the executable file might be covered by the GNU General public License.
*
* this exception applies only to the code released with this xHarbour
* explicit exception.  if you add/copy code from other sources,
* as the General public License permits, the above exception does
* not apply to the code that you add in this way.  To avoid misleading
* anyone as to the status of such modified files, you must delete
* this exception notice from them.
*
* If you write modifications of your own for xHarbour, it is your choice
* whether to permit this exception to apply to your modifications.
* if you do not wish that, delete this exception notice.
*
*/

#ifndef _C_EXSTRUCT
#define _C_EXSTRUCT

#xcommand STRUCTURE <oStruct> => <oStruct> := TAssociativeArray() ; #undef _TSTRUCT_ ; #define _TSTRUCT_ <oStruct>
#xcommand STRUC <oStruct> => <oStruct> := TAssociativeArray() ; #undef _TSTRUCT_ ; #define _TSTRUCT_ <oStruct>

#xcommand MEMBER <cName, ...> ;
             [ AS <type:LOGICAL,NUMERIC,STRING,DATE,CODEBLOCK,ARRAY,OBJECT> ] ;
             [ LEN <nLen> ]          ;
             [ INIT <uValue> ]          ;
          => ;
          TAssociativeArrayMember( {<(cName)>}, <(type)>, <uValue>, _TSTRUCT_ )

#xcommand ENDSTRUCTURE =>
#xcommand ENDSTRUC =>

#translate DESTROY STRUCTURE <o> => <o> := nil

#endif


