/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 *   CT3 - Test MROW(), MCOL(), WSTACK(), WINFO()
 *
 * Copyright 2005 Henryk Olkowski <oh1@op.pl>
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
#include "inkey.ch"
#include "set.ch"
/*****************************************************************************/
procedure main()
local w1,w2,w3,row,col,c,i,ws,wi,nwind

cls

wbox()
@ 0,2 say " WIND-0 "

w1:=wopen(5,1,15,77)
wbox()
@ -1,1 say " WIND-1 "

w2:=wopen(2,20,18,70)
wbox()
@ -1,1 say " WIND-2 "

w3:=wopen(10,10,23,60)
wbox()
@ -1,1 say " WIND-3 "

wselect(0)
@ maxrow()-1,maxcol()-10 say "ESC-Quit"

wselect(w2)
nwind:=wselect()

c:=0
while c!=K_ESC
 dispbegin()

 wselect(0)
 colorwin(0,0,maxrow(),maxcol(),iif(nwind==0,"w+/r*","w+/b"))

 wselect(w1)
 wformat(-1,-1,-1,-1)
 colorwin(0,0,maxrow(),maxcol(),iif(nwind==w1,"w+/r*","w+/g*"))
 wformat(1,1,1,1)

 wselect(w2)
 wformat(-1,-1,-1,-1)
 colorwin(0,0,maxrow(),maxcol(),iif(nwind==w2,"n/r*","n/gr*"))
 wformat(1,1,1,1)

 wselect(w3)
 wformat(-1,-1,-1,-1)
 colorwin(0,0,maxrow(),maxcol(),iif(nwind==w3,"w+/r*","w+/bg*"))
 wformat(1,1,1,1)

 wselect(w2)
 dispend()

 c:=inkey(0,INKEY_ALL)
 row:=MRow(); col:=MCol()

 setcolor(iif(nwind==wselect(),"n/r*","n/gr*"))
 setpos(0,0)
 row:=MRow(.t.); col:=MCol(.t.)
 ? " WINDOW-0","  MRow(.t.)=",str(row,3),"  MCol(.t.)=",str(col,3)

 row:=MRow(); col:=MCol()
 ?; ? " WINDOW-2","       MRow=",str(row,3),"       MCol=",str(col,3)

 row:=MRow(.t.); col:=MCol(.t.)
 ws:=wstack(); nwind:=-1
 for i:=len(ws) to 1 step -1
  wi:=winfo(ws[i])
  if row>=wi[1] .and. row<=wi[3] .and. col>=wi[2] .and. col<=wi[4]
   nwind:=ws[i]; row-=wi[5]; col-=wi[6]; exit
  endif
 endfor i

 ?; ? " WINDOW-"+str(nwind,1),"        Row=",str(row,3),"        Col=",str(col,3)
end

wselect(0)
setpos(maxrow()-1,0)

return
/*****************************************************************************/
