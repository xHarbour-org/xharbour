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

#ifndef __CLIPPER__
#include "hbclass.ch"
#else
#include "objects.ch"
#endif
#include "inkey.ch"

class menu from basebox

   DATA ctitle
   DATA npad
   DATA cbuffer
   DATA aprompt
   DATA property
   DATA menucolor
   DATA execcolor
   DATA barcolor
   DATA aaccelerate
   DATA horizontal
   DATA htitle
   DATA lrestscreen
   DATA cargo
   DATA nrightpad
   DATA lextend
   DATA nselect
   DATA escisquit
   DATA hotkeys
   DATA brefresh
   DATA brestscreen
   DATA ldrawbox
   DATA obox
   DATA enabledoubleclick
   DATA classname
   DATA smallbox
   DATA smallcolor
   DATA pulldown
   DATA cdisplay
   DATA ncursor
   DATA lloop

   METHOD new()
   METHOD add()
   METHOD exec()
   METHOD hide()
   METHOD refresh()
   METHOD killfocus()
   METHOD setfocus()
   method display()

   DATA nprevious

   METHOD set()
   METHOD isloadable()
   METHOD configure()
   METHOD say()

end class

********************************************************************************
METHOD New( _ctitle, top, left, bottom, right, _aprompt, _boxcolor, _color, _execcolor, _barcolor, lhorizontal, ldrawbox, lsmallbox, npad ) CLASS MENU

::ldrawbox    := if( ldrawbox  != NIL, ldrawbox   , .t. )
::smallbox    := if( lsmallbox != NIL, lsmallbox  , .f. )

if( lhorizontal!= NIL, ::horizontal := lhorizontal, )
if( nPad       != NIL, ::npad       :=  nPad      , 1 )

::ncursor     := setcursor()
::pulldown    := .f.
::smallcolor  := nil
::classname   := "MENU"
::enabledoubleclick := .f.
::brestscreen := nil
::brefresh    := nil
::hotkeys     := {}
::escisquit   := .t.
::lextend     := .f.
::set( _ctitle, top, left, bottom, right, _aprompt, _boxcolor, _color, _execcolor, _barcolor )
return self

*******************************************************************************
METHOD display() CLASS MENU
local ntop      := ::ntop
local nleft     := ::nleft
local nbottom   := ::nbottom
local nright    := ::nright
local xcolor    := "W+/B"
local cboxcolor := pickcolor(xcolor,1,"/")
local acolor    := {}
local chorcolor := "w+/bg,n/w*"
local _ccolor

if(::boxcolor == nil,::boxcolor:=if(!::horizontal,xcolor,chorcolor), )
::nselect   := 1
::nprevious := 0
if(empty(::htitle),::htitle:="",)
iif(empty(::lrestscreen),::lrestscreen:=.f.,)
if ::cbuffer == nil
   ::cbuffer := iif(!::lextend,(savescreen( ntop, nleft, nbottom+1, nright+2 )),)
endif
dispbegin()
  if ::lextend .or. !::ldrawbox
     ::say()
  else
     ::obox := wbox(::htitle,ntop,nleft,nbottom,nright,,,::boxcolor)
     if ::pulldown
        ::obox:topbar := .f.
     endif
     ::obox:activate:display()
     ::say()
  endif
dispend()
return self

*******************************************************************************
METHOD refresh() CLASS MENU
begin sequence
setcursor(0)
if ::cdisplay <> nil
   if ::brestscreen == nil
      scrunpack(::cdisplay)
   else
      eval( ::brestscreen, ::cdisplay )
   endif
endif
if !empty(::brefresh)
   eval( ::brefresh )
endif
end sequence
return self

*******************************************************************************
METHOD isloadable() CLASS MENU
local vloadable := ::property[::nselect][3]
if valtype(vloadable) == "L"
   return vloadable
elseif valtype(vloadable) == "B"
   return eval( vloadable )
endif
return .f.

*******************************************************************************
METHOD exec( noption ) CLASS MENU
static avar
local isy2k, autoupdate, lsave := .f.
local ctxt    := ""
local adummy, cscr
local acolor  := ::smallcolor
local nhot
local nlastclicked
local lastclick := 0
local nkey, okey
local nmrow   := 0
local nmcol   := 0
local msavrow := 0
local msavcol := 0
local lcheck  := .f.
local lfirst  := .f.
local nidlemenu := 600

g_save()
if !empty( noption )
   ::nselect := noption
else
   ::nselect := 1
endif
if ::lloop == nil
   ::lloop := .t.
endif

lfirst := .t.

while .t.
   setcursor(0)
   if lfirst
      lfirst := .f.
   endif
   if valtype( ::cargo ) == "B"
      eval( ::cargo )
   endif
   if ::lextend .and. ::ldrawbox
      aeval( ::property, { |e| e[3] := .F. }, 1, 14 )
   endif
   ::setfocus()
#ifdef __CLIPPER__
   while nextkey() == 0
      ol_yield()
   enddo
   nkey := inkey()
#else
   nkey := inkey(0)
#endif
   do case
   case nkey == 0
   case setkey( nkey ) != nil
      eval( setkey( nkey ), procname(1), procline(1), readvar() )
   case nkey < 400
      do case
         case ( nkey == K_UP .or. nkey == K_DOWN ) .and. ::horizontal
         case ( nkey == K_LEFT .or. nkey == K_RIGHT ) .and. !::horizontal
            if ::pulldown
               exit
            endif
            if valtype(::obox)=="O".and.!empty(::obox:topbarmenu)
               keyboard chr( K_ENTER )
            endif
         case ( nkey == K_UP .AND. !::Horizontal ) .OR. ( nkey == K_LEFT .AND. ::Horizontal )
            if(--::nSelect < 1,::nSelect := len(::aPrompt),)
         case ( nkey == K_DOWN .AND. !::Horizontal ) .OR. ( nkey == K_RIGHT .AND. ::Horizontal )
            if(++::nSelect > len(::aPrompt),::nSelect := 1,)
         case nkey == K_HOME .OR. nkey == K_PGUP
            ::nSelect := 1
         case nkey == K_END .OR. nkey == K_PGDN
            ::nSelect := len(::aPrompt)
         case nkey == K_ENTER
            if ::isloadable
               ::cdisplay := scrpack()
               if !( valtype( ::property[::nselect][1] ) == "B" ) .and. !( valtype( ::property[::nselect][1] ) == "O" )
                  exit
               endif
               if(::lrestscreen,::hide(),)
               if valtype( ::property[::nselect][1] ) == "B"
                  eval( ::property[::nselect][1], ::nselect )
               else
                  ::property[::nselect][1]:display:exec()
               endif
               if !::lloop
                  exit
               endif
               ::refresh()
               lcheck := .f.
               lfirst := .t.
            endif
         case nkey == K_ESC
            if ::escisquit
               ::nselect := 0
               exit
            endif
         otherwise
            do case
            case ( nlastclicked := ascan( ::aaccelerate, { |a| lower( a ) == lower(chr(nkey)) } ) ) > 0
               ::nselect := nlastclicked
               keyboard chr( K_ENTER )
            case ( nhot := ascan( ::hotkeys, { |e| nkey == e[1] } ) ) > 0
               ::cdisplay := scrpack()
               eval( ::hotkeys[nhot][2], ::nselect )
               ::refresh()
            endcase
         endcase
   endcase
   ::killfocus()
   // ft_idle()
enddo
// ft_idle()
if(::lrestscreen,::hide(),)
setcursor(::ncursor)
noption := ::nselect
g_restore()
return ::nselect

********************************************************************************
METHOD add( a_prompt ) CLASS MENU
static pd_coord := 0 // bottom,right,nrightpad
local ctext
local nmark
local nrightpos
local ntoppad := 2
local n_right
local n_mse
local caccel
if ::pulldown .and. empty( ::aprompt )  // initialized
   pd_coord  := 0
   ::ntop    := ::ntop + 1
   ::nbottom := ::ntop + 1
   ::npad    := 1
endif
::lextend := ( len(a_prompt) >= 6 )
if(::horizontal == nil,::horizontal := .f.,)
if ::horizontal .and. len(a_prompt) < 6
   if(::npad == nil,::npad := 0,)
   aadd(::aprompt,{0,0,ctext:=" "+a_prompt[1]+" ",0,strtran(ctext,"~",""),0,""})
   aadd(::aaccelerate,atail(atail(::aprompt)))
else
   if ::lextend .or. !::ldrawbox
      if len( a_prompt ) < 7
         asize( a_prompt, 7 )
         a_prompt[7] := .t.
      endif
      if !::ldrawbox
         ::nrightpad := len(a_prompt[1]) + 2
      endif
      aadd(::aprompt,{;
      a_prompt[5]    ,;
      a_prompt[6]    ,;
      ctext := padr(space(if(empty(::npad),6 /* used to be 7 */,::npad))+a_prompt[1],::nrightpad),;
      nmark := at("~",ctext)           ,;
      strtran(ctext,"~","")            ,;
      a_prompt[6]+nmark-1              ,;
      caccel := substr(ctext,nmark+1,1),;
      a_prompt[7]})
      aadd(::aaccelerate, caccel )
   else
      if ::pulldown
         if ( nrightpos := len( a_prompt[1] ) + 1 ) > pd_coord
            pd_coord    := nrightpos + 2
            ::nright    := ::nleft + pd_coord
         endif
         ntoppad := 1
         ::nbottom ++
      endif
      aadd(::aprompt,{;
      ::ntop+ntoppad+len(::aprompt),;
      ::nleft+1                    ,;
      ctext := padr(space(if(empty(::npad),6 /* Previously 7 */,::npad))+a_prompt[1],::nright-::nleft),;
      nmark := at("~",ctext),;
      strtran(ctext,"~","") ,;
      ::nleft+nmark         ,;
      caccel := substr(ctext,nmark+1,1)})
      aadd(::aaccelerate, caccel )
      if ::pulldown
         n_right := len( atail( ::aprompt )[5] )
         aeval( ::aprompt, { |e,n| ::aprompt[n][5] := padr( e[5], n_right ) } )
      endif
   endif
endif
aadd(::property,{ a_prompt[2], a_prompt[3], a_prompt[4] } )
return self

********************************************************************************
METHOD hide( lhide ) CLASS MENU
restscreen( ::ntop, ::nleft, ::nbottom+1, ::nright+2, (::cbuffer ))
return self

*******************************************************************************
METHOD say() CLASS MENU
local i, z
local aprompt     := ::aprompt
local ccolor      := pickcolor(::boxcolor,2,"/")
local asmallcolor := ::smallcolor
local menucolor   := ::menucolor
local execcolor   := ::execcolor
local horizontal  := ::horizontal
local nbutton     := 1
local acolor      := {}
local _ccolor

if ::horizontal .and. ( !empty( _ccolor ) .and. len( acolor := str2arr( _ccolor ) ) >= 2 )
   ::menucolor := menucolor := acolor[1]
   ::execcolor := execcolor := acolor[2]
   if len( acolor ) >= 4
      ccolor := acolor[3]
   endif
endif

if( asmallcolor == nil,asmallcolor := ::smallcolor := { ::boxcolor,"/"+ccolor, ::menucolor },)
if( horizontal,::configure(),)

::smallbox := ( nbutton <> 1 )
if !::horizontal
   ::smallbox := .f.
endif
z := len( aprompt )
if !::smallbox
   for i := 1 to z
      dispoutat(aprompt[i][1],aprompt[i][2],aprompt[i][5],menucolor)
      dispoutat(aprompt[i][1],aprompt[i][6],aprompt[i][7],execcolor)
      if horizontal
         dispoutat(aprompt[i][1],aprompt[i][2]+len(aprompt[i][3])-1,"ß",ccolor)
         dispoutat(aprompt[i][1]+1,aprompt[i][2]+1,repl("Ü",len(aprompt[i][3])-1),ccolor)
      endif
   next
else
   for i := 1 to z
      smallbox(aprompt[i][1],aprompt[i][2],aprompt[i][5],asmallcolor,if(len(aprompt[i])>=8,aprompt[i][8],.t.))
      dispoutat(aprompt[i][1],aprompt[i][6],aprompt[i][7],::execcolor)
   next
endif
return self

*******************************************************************************
METHOD set( _ctitle, top, left, bottom, right, _aprompt, _boxcolor, _color, _execcolor, _barcolor ) CLASS MENU

::ctitle         := _ctitle
::ntop           := top
::nleft          := left
::nbottom        := bottom
::nright         := right
::aaccelerate    := {}
::property       := {}
::cdisplay       := nil
::boxcolor       := _boxcolor
if( (::aprompt   := _aprompt   ) == nil, ::aprompt   := {}       , )
if( (::menucolor := _color     ) == nil, ::menucolor := "BG+/B", )
if( (::execcolor := _execcolor ) == nil, ::execcolor := "gr+/b", )
if( (::barcolor  := _barcolor  ) == nil, ::barcolor  := "N/BG*", )
return self

// local _color := pickcolor( _colors,,,.t.)
// local __clr  := pickcolor( _color[ncolor],,"/",.t.)
// HelpColor=W+/B,BG+/B
// MenuColor=N/W*,B+/W*,N/BG*
// BoxColor=BG+/B,N*/BG,N/N,N/N,N/N
// ClrJudul=GR+/GR*,N/G,N/N,N/N,N/N
// ClrHili=N/BG*,N/N,N/N,N/N,N/N
// ClrInput=N*/BG,W+/R*,N/N,N/R,GR+/B
// ClrText=G+/G,G+/B,N/N,BG+/B,G+/B
// ClrLayar=N+/N*,N/N,N/N,BG/N,BG+/R,N/N
// ClrPesan=BG+/N,N/N,N/N,N/N,N/N
// CharExec=GR+/B,N/N,N/N,N/N,N/N
// DialogColor=W+,BG+,B,N/B*,N/B,B/B,N/BG*,B+/B,N/B
// ClrGauge=GR+/B,N/N,N/N,N/N,N/N

*******************************************************************************
METHOD killfocus() CLASS MENU
local boxcolor  := ::boxcolor
local nprevious := ::nprevious
local aprompt   := ::aprompt
local nleft     := ::nleft
local nright    := ::nright
local nrow      := aprompt[nprevious][1]
local ncol      := aprompt[nprevious][2]
local cprompt   := aprompt[nprevious][5]

if ( ::nselect <> nprevious )
   dispoutat( nrow,ncol,cprompt,::menucolor)
   dispoutat( nrow, aprompt[ nprevious][6],aprompt[ nprevious][7],::execcolor)
endif
return self

*******************************************************************************
METHOD setfocus() CLASS MENU
local aprompt   := ::aprompt
local nselect   := ::nselect
local nleft     := ::nleft
local nright    := ::nright
local nrow      := aprompt[nselect][1]
local ncol      := aprompt[nselect][2]
local cprompt   := aprompt[nselect][5]
local lloadable := ( (valtype(::property[::nselect][3])=="B") .or. ::property[::nselect][3] )

nrow      := aprompt[nselect][1]
ncol      := aprompt[nselect][2]
cprompt   := aprompt[nselect][5]
lloadable := ( (valtype(::property[::nselect][3])=="B") .or. ::property[::nselect][3] )

if ( ::nselect <> ::nprevious )
   dispoutat( nrow, ncol,cprompt, if(lloadable,::barcolor, "n/n*" ))
   ::nprevious := ::nselect
endif
return self

*******************************************************************************
METHOD configure() CLASS MENU
local n
local nmark
local ntextlen   := 0
local nspace     := 0
local ntop       := ::ntop
local nleft      := ::nleft
local ntotlength := ::nright - nleft - 1
local aprompt    := ::aprompt
local _n         := len(aprompt)

for n := 1 to _n
   ntextlen += len(aprompt[n][3])
next

nspace := int( (ntotlength - ntextlen )/ ( len(aprompt) + 1 ) )
for n := 1 to _n
   if empty(aprompt[n][1]) .or. empty(aprompt[n][2])
      aprompt[n][1]  := ntop + 2
      aprompt[n][2]  := nspace + if( n == 1, ::npad + nleft + 1, aprompt[n-1][2] + len(aprompt[n-1][3]) )
   endif
   aprompt[n][6]    := (nmark := at("~",aprompt[n][3])) + aprompt[n][2] - 1
   aprompt[n][7]    := substr(aprompt[n][3],nmark+1,1)
   ::aaccelerate[n] := aprompt[n][7]
next
// ::aprompt := aclone( aprompt )
::aprompt := aprompt
return self
