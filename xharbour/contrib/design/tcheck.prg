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
#xtranslate _checkcolor => check_component\[1\]
#xtranslate b_checked   => check_component\[2\]
#xtranslate b_unchecked => check_component\[3\]
static check_component[3]
********************************************************************************
class getcheck from get

  data checkgsb

  method drawcheck()
  method checkapplykey()
  method checkreader()

endclass

********************************************************************************
function checkgetnew( nrow, ncol, bvar, cvar, cstr, ccolor, preblock, postblock )

local acolor := {}
local nsaverow, nsavecol
local oget
local _ccolor

if _checkcolor == nil
   if ( _checkcolor := ccolor ) == NIL
      _checkcolor := "gr+/bg"
   endif
   b_checked   := "[X] "
   b_unchecked := "[ ] "
endif

oget := getcheck():new()
oget:colorspec := _checkcolor
if empty( oget:preblock )
   oget:preblock := if( valtype( preblock  ) == "B", preblock,  {||.t.} )
endif
if empty( oget:postblock )
   oget:postblock := if( valtype( postblock ) == "B", postblock, {||.t.} )
endif
oget:col      := ncol + 4
oget:row      := nrow
oget:name     := cvar
oget:checkgsb := bvar
oget:block    := {|| cstr }
oget:reader   := { |g| oget:checkreader( g ) }
oget:drawcheck()
return oget

********************************************************************************
method checkreader( getlist ) class getcheck

setcursor(1)
if eval( ::preblock, self, getlist )
  ::exitstate := GE_NOEXIT
  ::setfocus()
  while ( ::exitstate == GE_NOEXIT )
    if ( ::typeout )
      ::exitstate := GE_ENTER
    endif
    while ( ::exitstate == GE_NOEXIT )
#ifdef __CLIPPER__
      while nextkey() == 0
        ol_yield()
      enddo
      ::checkapplykey(inkey(),getlist )
#else
       ::checkapplykey(inkey(0),getlist )
#endif
    enddo
    if ( !getpostvalidate(self) )
      ::exitstate := GE_NOEXIT
    endif
  enddo
  ::killfocus()
endif
return self

********************************************************************************
method checkapplykey( nkey, getlist ) class getcheck

local n
local ckey
local bkeyblock
local nsaverow, nsavecol
local ncursor   := setcursor(1)
local _ngetlist := len( getlist )

do case
  case nkey == 0

  case ( (bkeyblock := setkey(nkey)) <> nil )
     getdosetkey(bkeyblock, self)

  case ( nkey == K_UP ) .or. ( nkey == K_SH_TAB )
    ::exitstate := GE_UP

  case ( nkey == K_DOWN ) .or. ( nkey == K_TAB )
    ::exitstate := GE_DOWN

  case ( nkey == K_ENTER )
    ::exitstate := GE_ENTER

  case ( nkey == K_SPACE )
    eval(::checkgsb,!eval(::checkgsb))
    ::drawcheck()

  case ( nkey == K_ESC )
    if ( set(_SET_ESCAPE) )
      ::undo()
      ::exitstate := GE_ESCAPE
    endif

  case (nkey == K_PGUP ) .or. (nkey == K_PGDN ) .or. (nKey == K_CTRL_W)
    ::exitstate := GE_WRITE

  case ( nkey == K_CTRL_HOME )
    ::exitstate := GE_TOP

  case (nkey == K_INS)
    set( _SET_INSERT, !set(_SET_INSERT) )

endcase
// setcursor( ncursor )
return self

********************************************************************************
method drawcheck() class getcheck

local nrow := row(), ncol := col()
dispoutat(::row,::col-4,if(eval(::checkgsb),b_checked,b_unchecked),_checkcolor)
setpos(nrow,ncol)
return self
