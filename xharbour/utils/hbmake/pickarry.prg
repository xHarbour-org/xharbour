/*
 * $Id: pickarry.prg,v 1.3 2003/05/09 16:12:06 lculik Exp $
 */

*+²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²
*+
*+    Source Module => D:\SRC\PBMAKE\PICKARRY.PRG
*+
*+    PBMake is a Clipper, C, ASM, Xbase++ make engine.
*+    
*+    Copyright(C) 1996-1999 by Phil Barnett.
*+       
*+    This program is free software; you can redistribute it and/or modify it
*+    under the terms of the GNU General Public License as published by the
*+    Free Software Foundation; either version 2 of the License, or (at your
*+    option) any later version.
*+    
*+    This program is distributed in the hope that it will be useful, but
*+    WITHOUT ANY WARRANTY; without even the implied warranty of
*+    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*+    General Public License for more details.
*+    
*+    You should have received a copy of the GNU General Public License along
*+    with this program; if not, write to the Free Software Foundation, Inc.,
*+    675 Mass Ave, Cambridge, MA 02139, USA.
*+    
*+    You can contact me at:
*+    
*+    Phil Barnett
*+    Box 944
*+    Plymouth, Florida  32768
*+    
*+    or
*+    
*+    philb@iag.net
*+    
*+
*+    Functions: Function PICKARRY()
*+               Function Keys()
*+
*+    Reformatted by Click! 2.03 on Mar-30-1999 at 11:19 pm
*+
*+²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²²

static someitems

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function PICKARRY()
*+
*+    Called from ( makelink.prg )   1 - function makelink()
*+
   *+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
function PICKARRY( T, L, b, r, IN_ARRAY, OUT_ARRAY ,aDefault)

local nChoice    := 1
local x
local NEW_ARRAY  := {}
local NUM_ELEMS  := len( IN_ARRAY )
local PAD_LEN    := ( r - 1 ) - ( L + 1 )
local lIsChecked := .f.
Local aitems := IN_ARRAY
Local aTemp 
Local cItem,cItem1,cTemp
Local cOldColor := SetColor()

someitems := 0

putscreen()
   Setcolor( 'gr+/rb,b+/w,w+/b,w/b+,w/b,w+/b' )
@ T - 1, L - 1 clear to b + 1, r + 1
@ T - 1, L - 1 to b + 1, r + 1 double

for x := 1 to NUM_ELEMS
   IN_ARRAY[ X ]  := padr( '   ' + IN_ARRAY[ X ], PAD_LEN )
   tracelog('"'+IN_ARRAY[ X ]+'"')
   OUT_ARRAY[ X ] := ' ' + OUT_ARRAY[ X ]
next

//aTemp :=GetFiles(aitems)

if Len(ADefault) >0
   For each cItem in aDefault
   
   x:= ascan( IN_ARRAY,{| a,y |  substr(a,4,at(' ',alltrim(a))-1)==cItem})
   tracelog(IN_ARRAY[1])   
   tracelog(x,substr(IN_ARRAY[1],4,at(' ',alltrim(IN_ARRAY[1]))-1))
   if x != 0

      IN_ARRAY[ x ]  := stuff( IN_ARRAY[ x ], 2, 1, if( lIsChecked, ' ', 'û' ) )

      OUT_ARRAY[ x ] := stuff( OUT_ARRAY[ x ], 1, 1, if( lIsChecked, ' ', 'û' ) )
SOMEITEMS ++
   else
      cItem:=substr(cItem,rat('\',cItem)-1)

      x:= ascan( aTemp,{| a,y |   substr(a,4,at(' ',a)-1)==cItem})

      if x != 0
      IN_ARRAY[ x ]  := stuff( IN_ARRAY[ x ], 2, 1, if( lIsChecked, ' ', 'û' ) )

      OUT_ARRAY[ x ] := stuff( OUT_ARRAY[ x ], 1, 1, if( lIsChecked, ' ', 'û' ) )
SOMEITEMS ++
       endif
    endif
   next   
endif

do while nChoice != 0
   nChoice := achoice(    T, L    , b      , r     , IN_ARRAY,     , 'keys'   , nChoice, 1 )

   if nChoice > 0

      lIsChecked := substr( IN_ARRAY[ nChoice ], 2, 1 ) == 'û'

      IN_ARRAY[ nChoice ]  := stuff( IN_ARRAY[ nChoice ], 2, 1, if( lIsChecked, ' ', 'û' ) )
      OUT_ARRAY[ nChoice ] := stuff( OUT_ARRAY[ nChoice ], 1, 1, if( lIsChecked, ' ', 'û' ) )

      if lIsChecked
         SOMEITEMS --
      else
         SOMEITEMS ++
      endif

      nChoice ++

   endif

enddo

for x := 1 to NUM_ELEMS
   if left( OUT_ARRAY[ X ], 1 ) == 'û'
      aadd( NEW_ARRAY, substr( OUT_ARRAY[ X ], 2 ) )
   endif
   IN_ARRAY[ X ] := substr( IN_ARRAY[ X ], 4 )

next

asize( OUT_ARRAY, len( NEW_ARRAY ) )
acopy( NEW_ARRAY, OUT_ARRAY )

getscreen()
SetColor(coldColor)
return len( NEW_ARRAY )

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function Keys()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
function Keys( MODE )

local RETVAL := 2
local THEKEY := lastkey()

if MODE = 1
   keyboard chr( 30 )
elseif MODE = 2
   keyboard chr( 31 )
elseif MODE = 3
   if THEKEY = 32
      RETVAL := 1
   elseif THEKEY = 27
      RETVAL := 0
   elseif THEKEY = 13 .and. SOMEITEMS < 1
      RETVAL := 1
      keyboard chr( 13 )
   elseif THEKEY = 13
      keyboard chr( 24 )
      RETVAL := 0
   endif
endif

return ( RETVAL )

*+ EOF: PICKARRY.PRG
static function GetFiles(aIn)
Local aRet:={}
Local cItem:=""
For each citem in ain
citem:=substr(citem,1,at(' ',citem)-1)

   aadd(aret,substr(citem,1,at(' ',citem)))
next
return aret
