/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Text Mode Screen Designer
 *
 * Copyright 2004 Andi Jahja <xharbour@cbn.net.id>
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

#include "getexit.ch"
#include "inkey.ch"
#ifndef __CLIPPER__
#include "hbclass.ch"
#else
#include "objects.ch"
#endif
#xtranslate _checkcolor       => rad_component\[1\]
#xtranslate unselected_radio  => rad_component\[2\]
#xtranslate selected_radio    => rad_component\[3\]
#xtranslate radio_button      => rad_component\[4\]
#xtranslate blank_button      => rad_component\[5\]
static rad_component[5]
********************************************************************************
class getradio from get

  DATA radiogsb
  DATA radiogets
  DATA radiotext

endclass

********************************************************************************
function radiogets(bvar, cvar, achoices, agetlist, ccolor, aposition, apreblock, apostblock )

local acolor := {}
local lrowpos
local oget
local nrow  := row(), ncol := col()
local ngets := len(achoices)
local nget
local nstartget := len(agetlist)
local _ccolor

hb_default( @aposition, {} )

if _checkcolor == nil
   _checkcolor      := ccolor
   if _checkcolor == NIL
      _checkcolor := "gr+/bg"
   endif
   unselected_radio := "( ) "
   selected_radio   := "() "
   radio_button     := ""
   blank_button     := " "
endif

if apostblock == nil
   apostblock := array(ngets)
   afill(apostblock,{||.t.})
endif

if apreblock == nil
   apreblock := array(ngets)
   afill(apreblock,{||.t.})
endif

lrowpos := ( len( aposition ) == ngets )

for nget := 1 to ngets
  oget := getradio():new()
  oget:radiotext := ""
  oget:colorspec := _checkcolor
  oget:preblock  := apreblock[nget]
  oget:postblock := apostblock[nget]
  oget:name      := cvar
  oget:block     := t(achoices[nget])
  oget:radiogsb  := bvar
  oget:reader    := {|o|radioreader(o,agetlist,lrowpos)}
  if lrowpos
     dispoutat( aposition[nget][1], aposition[nget][2],if(eval(oget:radiogsb) == eval(oget:block),SELECTED_RADIO,UNSELECTED_RADIO),_checkcolor)
     oget:col := aposition[nget][2] + 4
     oget:row := aposition[nget][1]
  else
     dispoutat( nrow, ncol,if(eval(oget:radiogsb) == eval(oget:block),SELECTED_RADIO,UNSELECTED_RADIO),_checkcolor )
     oget:col := ncol + 4
     oget:row := nrow ++
  endif
  oget:radiogets := array(ngets)
  aeval(oget:radiogets,{|x,n|oget:radiogets[n]:=nstartget+n})
  aadd(agetlist,oget)
next
return oget

********************************************************************************
static function t(c)
return { |x| c }

********************************************************************************
static function radioreader( oget, agetlist, lrowpos )

if ( getprevalidate(oget) )
  oget:setfocus()
  while ( oget:exitstate == GE_NOEXIT )
    if ( oget:typeout )
      oget:exitstate := GE_ENTER
    endif
    while ( oget:exitstate == GE_NOEXIT )
      setcursor(1)
#ifdef __CLIPPER__
      while nextkey() == 0
        ol_yield()
      enddo
      radioapplykey(oget, inkey(), agetlist, lrowpos )
#endif
      radioapplykey(oget, inkey(0), agetlist, lrowpos )
    enddo
    if ( !getpostvalidate(oGet) )
      oget:exitstate := GE_NOEXIT
    endif
  enddo
  oget:killfocus()
endif

return "RADIO"

********************************************************************************
static procedure radioapplykey(oget, nkey, agetlist, lrowpos )

local ckey, _nscan
local bkeyblock

#ifdef __CLIPPER__
do case
  case ( nkey == 0 )

  case ( ( bkeyblock := setkey( nkey ) ) <> nil )
     getdosetkey(bkeyblock, oget)

  case ( _nscan := ascan( { K_UP, K_LEFT, K_DOWN, K_RIGHT }, nkey ) ) > 0
/*
    if eval(oget:radiogsb) == eval(oget:block)
      eval(oget:radiogsb, "")
      drawradios(agetlist, oget)
    endif
*/
    if _nscan < 3
      oget:exitstate := GE_UP
    else
      oget:exitstate := GE_DOWN
    endif

  case ( nkey == K_SH_TAB )
    oget:exitstate := GE_UP

  case ( nkey == K_TAB )
    oget:exitstate := GE_DOWN

  case ( nkey == K_ENTER )
    oget:exitstate := GE_ENTER

  case nkey == K_SPACE
    eval(oget:radiogsb,if(eval(oget:radiogsb) == eval(oget:block),"",eval(oget:block)))
    drawradios(agetlist, oget)

  case ( nkey == K_ESC )
    if ( set(_SET_ESCAPE) )
      oget:undo()
      oget:exitstate := GE_ESCAPE
    endif

  case (nkey == K_PGUP )
    oget:exitstate := GE_WRITE

  case (nkey == K_PGDN )
    oget:exitstate := GE_WRITE

  case ( nkey == K_CTRL_HOME )
    oget:exitstate := GE_TOP

  case (nkey == K_CTRL_W)
    oget:exitstate := GE_WRITE

  case (nkey == K_INS)
    set( _SET_INSERT, !set(_SET_INSERT) )

endcase
#else
  if nkey == 0
  elseif ( bkeyblock := setkey( nkey ) ) <> nil
     getdosetkey(bkeyblock, oget)
  else
     switch nKey
     case K_UP
     case K_DOWN
        /*
        if eval(oget:radiogsb) == eval(oget:block)
           eval(oget:radiogsb, "")
           drawradios(agetlist, oget)
        endif
        */
        if nKey == K_UP
           oget:exitstate := GE_UP
        else
           oget:exitstate := GE_DOWN
        endif
        exit

     case K_SH_TAB
        oget:exitstate := GE_UP
        exit

     case K_TAB
        oget:exitstate := GE_DOWN
        exit

     case K_ENTER
        oget:exitstate := GE_ENTER
        exit

     case K_SPACE
        eval(oget:radiogsb,if(eval(oget:radiogsb) == eval(oget:block),"",eval(oget:block)))
        drawradios(agetlist, oget)
        exit

     case K_ESC
        if ( set(_SET_ESCAPE) )
           oget:undo()
           oget:exitstate := GE_ESCAPE
        endif
        exit

     case K_PGUP
        oget:exitstate := GE_WRITE
        exit

     case K_PGDN
        oget:exitstate := GE_WRITE
        exit

     case K_CTRL_HOME
        oget:exitstate := GE_TOP
        exit

     case K_CTRL_W
        oget:exitstate := GE_WRITE
        exit

     case K_INS
        set( _SET_INSERT, !set(_SET_INSERT) )
        exit

     end
  endif
#endif

return

********************************************************************************
static procedure drawradios(agetlist, oget)

local nrow := row()
local ncol := col()
local oget1
local lchose
local x, i
local __nradio := len( oget:radiogets )
local nCursor := SetCursor(0)
dispbegin()
#ifdef __CLIPPER__
for i := 1 to __nradio
   x      := oget:radiogets[i]
   lchose := (eval(agetlist[x]:radiogsb)==eval(agetlist[x]:block))
   dispoutat(agetlist[x]:row,agetlist[x]:col-3,if(lchose,RADIO_BUTTON,BLANK_BUTTON),_checkcolor)
   if lchose
      agetlist[x]:radiotext := eval(agetlist[x]:radiogsb)
   else
      agetlist[x]:radiotext := ""
   endif
next
#else
for each x in oget:radiogets
   lchose := (eval(agetlist[x]:radiogsb)==eval(agetlist[x]:block))
   dispoutat(agetlist[x]:row,agetlist[x]:col-3,if(lchose,RADIO_BUTTON,BLANK_BUTTON),_checkcolor)
   if lchose
      agetlist[x]:radiotext := eval(agetlist[x]:radiogsb)
   else
      agetlist[x]:radiotext := ""
   endif
next
#endif
setpos( nrow, ncol )
SetCursor(nCursor)
dispend()
return
