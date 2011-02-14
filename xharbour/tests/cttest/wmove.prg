/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 *   CT3 - Test WMOVE(), WBOARD(), WSETSHADOW()
 *
 * Copyright 2004 Henryk Olkowski <oh1@op.pl>
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

/*****************************************************************************/
#include "box.ch"
#include "ct.ch"
#include "inkey.ch"
#include "setcurs.ch"
/*****************************************************************************/
#define true .t.
#define false .f.
/*****************************************************************************/
procedure main()
local i,j,k,tx
local w,w0,w1,w2

clear screen
tt("clear screen")

wboard(3,3,maxrow()-3,maxcol()-3)
tt("wboard(3,3,maxrow()-3,maxcol()-3)")

setcolor("w+/g")
tx:=" wmove.prg: Test WMOVE(), WBOARD(), WSETSHADOW() "
@ 0,(maxcol()-len(tx))/2 say tx
tt("@ 0,20 say tx")

w0:=wopen(0,0,100,100,true)
clear screen
tt("wopen(0,0,100,100,true); cls")

setcolor("w+/r*")
tt("setcolor(w/r*)")

wsetshadow("w/r")
tt("wsetshadow(w/r)")

w1:=wopen(10,10,20,75,true)
wbox()
tt("w1:=wopen(10,10,20,75,true); wbox()")

setcolor("w+/b*")
tt("setcolor(w+/b*)")

wsetshadow("w/b")
tt("wsetshadow(w/b)")

w2:=wopen(5,5,15,70,true)
wbox()
tt("w2:=wopen(5,5,15,70,true); wbox()")

//wmode(true, false, true, false)
//tt("wmode(true,false,true,false)")

wmode(true, true, true, true)
tt("wmode(true,true,true,true)")

wselect(0)
tx:=" Test WMOVE(): Arrows-move, b-Box, c-Clear, w-Wind, Esc-EXIT "
@ 1,(maxcol()-len(tx))/2 say tx

wselect(w1)
setcolor("w+/r*")
? tx
wselect(w2)
setcolor("w+/b*")
? tx

j:=0
while true
 i:=inkey(0)
 w:=wselect()
 do case
  case i==K_ESC;   exit

  case i==asc("7");  wmove(3,3)
                   j++
                   tx:="["+xc(j)+"] wmove(FR,FC) wrow="+xc(wrow())+;
                     " wcol="+xc(wcol())+"   "
                   wselect(0); setpos(maxrow()-1,0)
                   ? tx
                   wselect(w)
                   ? tx

  case i==asc("8");  wmove(3,wcol(true))
                   j++
                   tx:="["+xc(j)+"] wmove(FR,CE) wrow="+xc(wrow())+;
                     " wcol="+xc(wcol())+"   "
                   wselect(0); setpos(maxrow()-1,0)
                   ? tx
                   wselect(w)
                   ? tx

  case i==asc("9");  wmove(3,wcol()+maxcol(true)-3-wlastcol())
                   j++
                   tx:="["+xc(j)+"] wmove(FR,LC) wrow="+xc(wrow())+;
                     " wcol="+xc(wcol())+"   "
                   wselect(0); setpos(maxrow()-1,0)
                   ? tx
                   wselect(w)
                   ? tx

  case i==asc("4");  wmove(wrow(true),3)
                   j++
                   tx:="["+xc(j)+"] wmove(CE,FC) wrow="+xc(wrow())+;
                     " wcol="+xc(wcol())+"   "
                   wselect(0); setpos(maxrow()-1,0)
                   ? tx
                   wselect(w)
                   ? tx

  case i==asc("5");  wcenter(true)
                   j++
                   tx:="["+xc(j)+"] wcenter(true) wrow="+xc(wrow())+;
                     " wcol="+xc(wcol())+"   "
                   wselect(0); setpos(maxrow()-1,0)
                   ? tx
                   wselect(w)
                   ? tx

  case i==asc("6");  wmove(wrow(true),wcol()+maxcol(true)-3-wlastcol())
                   j++
                   tx:="["+xc(j)+"] wmove(CE,LC) wrow="+xc(wrow())+;
                     " wcol="+xc(wcol())+"   "
                   wselect(0); setpos(maxrow()-1,0)
                   ? tx
                   wselect(w)
                   ? tx

  case i==asc("1");  wmove(wrow()+maxrow(true)-3-wlastrow(),3)
                   j++
                   tx:="["+xc(j)+"] wmove(LR,FC) wrow="+xc(wrow())+;
                     " wcol="+xc(wcol())+"   "
                   wselect(0); setpos(maxrow()-1,0)
                   ? tx
                   wselect(w)
                   ? tx

  case i==asc("2");  wmove(wrow()+maxrow(true)-3-wlastrow(),wcol(true))
                   j++
                   tx:="["+xc(j)+"] wmove(LR,CE) wrow="+xc(wrow())+;
                     " wcol="+xc(wcol())+"   "
                   wselect(0); setpos(maxrow()-1,0)
                   ? tx
                   wselect(w)
                   ? tx

  case i==asc("3");  wmove(wrow()+maxrow(true)-3-wlastrow(),;
                           wcol()+maxcol(true)-3-wlastcol())
                   j++
                   tx:="["+xc(j)+"] wmove(LR,LC) wrow="+xc(wrow())+;
                     " wcol="+xc(wcol())+"   "
                   wselect(0); setpos(maxrow()-1,0)
                   ? tx
                   wselect(w)
                   ? tx

  case i==K_UP;    wmove(wrow()-1,wcol())
                   j++
                   tx:="["+xc(j)+"] wmove(R-1,C) wrow="+xc(wrow())+;
                     " wcol="+xc(wcol())+"   "
                   wselect(0); setpos(maxrow()-1,0)
                   ? tx
                   wselect(w)
                   ? tx

  case i==K_LEFT;  wmove(wrow(),wcol()-1)
                   j++
                   tx:="["+xc(j)+"] wmove(R,C-1) wrow="+xc(wrow())+;
                     " wcol="+xc(wcol())+"   "
                   wselect(0); setpos(maxrow()-1,0)
                   ? tx
                   wselect(w)
                   ? tx

  case i==K_DOWN;  wmove(wrow()+1,wcol())
                   j++
                   tx:="["+xc(j)+"] wmove(R+1,C) wrow="+xc(wrow())+;
                     " wcol="+xc(wcol())+"   "
                   wselect(0); setpos(maxrow()-1,0)
                   ? tx
                   wselect(w)
                   ? tx

  case i==K_RIGHT; wmove(wrow(),wcol()+1)
                   j++
                   tx:="["+xc(j)+"] wmove(R,C+1) wrow="+xc(wrow())+;
                     " wcol="+xc(wcol())+"   "
                   wselect(0); setpos(maxrow()-1,0)
                   ? tx
                   wselect(w)
                   ? tx

  case i==asc("b") .or. i==asc("B")
                   wbox()
                   j++
                   tx:="["+xc(j)+"] wbox() wrow="+xc(wrow())+;
                     " wcol="+xc(wcol())+"   "
                   wselect(0); setpos(maxrow()-1,0)
                   ? tx
                   wselect(w)
                   ? tx

  case i==asc("c") .or. i==asc("C")
                   wformat(-100,-100,-100,-100)
                   clear screen
                   j++
                   tx:="["+xc(j)+"] clear screen wrow="+xc(wrow())+;
                     " wcol="+xc(wcol())+"   "
                   wselect(0); setpos(maxrow()-1,0)
                   ? tx
                   wselect(w)
                   ? tx

  case i==asc("s") .or. i==asc("s")
                   j++
                   if wsetshadow()==-1
                    wsetshadow("w+/g*")
                    tx:="["+xc(j)+"] wsetshadow(w+/g*)"
                   else
                    wsetshadow(-1)
                    tx:="["+xc(j)+"] wsetshadow(-1)"
                   endif
                   wselect(0); setpos(maxrow()-1,0)
                   ? tx
                   wselect(w)
                   ? tx

  case i==asc("w") .or. i==asc("W") .or. i==K_TAB
                   j++
                   if wselect()==w1
                    w:=wselect(w2)
                    tx:="["+xc(j)+"] wselect(w2)"
                   else
                    w:=wselect(w1)
                    tx:="["+xc(j)+"] wselect(w1)"
                   endif
                   wselect(0); setpos(maxrow()-1,0)
                   ? tx
                   wselect(w)
                   ? tx
 endcase
end

waclose()
tt("waclose()")

return
/*****************************************************************************/
/* Short version */
function tt(tx)

// ? tx
// inkey(0)

return 0
/*****************************************************************************/
function xc(n)
local tx

do case
 case valtype(n)="C"; tx:='"'+n+'"'
 case valtype(n)="D"; tx:=dtos(n)
 case valtype(n)="L"; tx:=iif(n,".t,",".f.")
 case valtype(n)="N"; tx:=alltrim(str(n,10))
 case valtype(n)="U"; tx:="nil"
 otherwise;           tx:="?"+valtype(n)+"?"
endcase

return tx
/*****************************************************************************/
