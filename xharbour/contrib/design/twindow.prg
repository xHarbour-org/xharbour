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
#include "box.ch"
#xtranslate radio     => acontrol\[1\]
#xtranslate checkbox  => acontrol\[2\]
static acontrol
#ifdef __HARBOUR__
#undef K_ALT_DOWN
#define K_ALT_DOWN 1232
#endif
CLASS WINDOW FROM BASEBOX

    METHOD new()
    METHOD display()
    METHOD hide()
    METHOD edit()
    METHOD adddialog()
    METHOD addprompt()
    METHOD addbox()
    METHOD addnotopbox()
    METHOD addmultibox()
    METHOD addsmallbox()
    METHOD addthickbox()
    METHOD changeprompt()
    METHOD write()
    METHOD grabcomponent()
    METHOD parsebutton()
    METHOD parseprompt()

    METHOD displaybox()
    METHOD displaybar()
    METHOD displayallbar()
    METHOD designbar()
    METHOD textdesign()
    METHOD hidebar()
    METHOD hideallbar()
    METHOD set()
    METHOD savecur()
    METHOD restprev()
    METHOD configprompt()

    DATA  cnamepos
    DATA  showbox
    DATA  obox
    DATA  dlgbar
    DATA  dlgcolor
    DATA  promptcolor
    DATA  titlecolor
    DATA  ctitle
    DATA  aprompt
    DATA  lclear
    DATA  cboxchar
    DATA  boxname
    DATA  abox
    DATA  amultibox
    DATA  athickbox
    DATA  anotopbox
    DATA  asmallbox
    DATA  athickbox
    DATA  cargo
    DATA  cproject
    DATA  barproperty
    DATA  barbuffer
    DATA  abuffer
    DATA  adummy
    DATA  lconfig
    DATA  atestbar

ENDCLASS

********************************************************************************
METHOD New( top, left, bottom, right, _ctitle, _aprompt, _dlgbar, dlgcolor, _aprnprompt, _lclear, _boxname ) CLASS WINDOW
if acontrol == nil
   acontrol := array(2)
   RADIO    := "()"
   CHECKBOX := "[X]"
endif
::dlgcolor := dlgcolor
::cproject := ""
::set( top, left, bottom, right, _ctitle, _aprompt, _dlgbar, dlgcolor, _aprnprompt, _lclear, _boxname )
return self

********************************************************************************
METHOD changeprompt( bchange ) CLASS WINDOW
eval( bchange, ::aprompt  )
return self

********************************************************************************
METHOD addbox( abox ) CLASS WINDOW
aadd( ::abox, abox )
return self

********************************************************************************
METHOD addmultibox( abox ) CLASS WINDOW
aadd( ::amultibox, abox )
return self

********************************************************************************
METHOD addsmallbox( abox ) CLASS WINDOW
if LEN( abox ) == 4
   aadd( abox, .t. )
endif
aadd( ::asmallbox, abox )
return self

********************************************************************************
METHOD addthickbox( abox ) CLASS WINDOW
aadd( ::athickbox, abox )
return self

********************************************************************************
METHOD addnotopbox( abox ) CLASS WINDOW
aadd( ::anotopbox, abox )
return self

********************************************************************************
METHOD hide() CLASS WINDOW
::obox:hide()
return self

********************************************************************************
METHOD addprompt( nrow, ncol, cprompt, ccolor ) CLASS WINDOW
aadd( ::aprompt, { nrow-::ntop, ncol-::nleft, cprompt, ccolor } )
::lconfig := .f.
return self

********************************************************************************
METHOD adddialog( nrow, ncol, nbar ) CLASS WINDOW
if( pcount() >= 3, aadd( ::dlgbar, { nrow, ncol, nbar } ), )
return self

********************************************************************************
METHOD display( cfile ) CLASS WINDOW
local n
local _ccolor := "w+,gr+,bg,n/bg*,n/bg,bg/bg,n/bg*"
local scol      := setcolor()

if !empty( cfile ) .and. file( cFile )
   ::cproject := cfile
   ::grabcomponent( cfile )
   ::displaybox()
   return self
endif

if Empty( ::dlgColor )
   ::dlgcolor    := str2arr( _ccolor )
   if empty(::boxcolor)
      ::boxcolor    := ::dlgcolor[1]+"/"+::dlgcolor[3]+",n/w*"
   endif
   if empty(::promptcolor)
      ::promptcolor := ::dlgcolor[2]+"/"+::dlgcolor[3]
   endif
   ::titlecolor  := ::dlgcolor[2]+"/"+::dlgcolor[3]
endif

if ::showbox == nil
   ::showbox := .t.
endif

if !empty(::aprompt)
  aeval( ::aprompt, { |e| if( len(e) == 3,aadd( e, ::promptcolor ), ) } )
endif
dispbegin()
if ::lconfig
   ::configprompt()
   ::lconfig := .f.
endif
// if( ::lclear, layar(), )
if !empty( ::boxname )
  if ::cnamepos == nil
     ::cnamepos := "MIDDLE"
  endif
  ::obox:setname( ::boxname, ::cnamepos )
endif
::obox:boxcolor := ::boxcolor
if ::showbox
   ::obox:display()
endif
if( !( ::ctitle == nil ),dispoutat( ::ctitle[1], ::ctitle[2],::ctitle[3], ::titlecolor ),)
if !empty( ::abox )
   aeval( ::abox, { |e| box3d( e[1],e[2],e[3],e[4],{::dlgcolor[3]+"+","N",::dlgcolor[3]})})
endif
if !empty( ::asmallbox )
   aeval( ::asmallbox, { |e| smallbox( e[1],e[2],e[3],e[4],e[5] ) } )
endif
if !empty( ::athickbox )
   aeval( ::athickbox, { |e| specialbox( e[1],e[2],e[3],e[4],e[5] ) } )
endif
if !empty( ::amultibox )
   aeval( ::amultibox, { |e| biggetbox( e[1],e[2],e[3],e[4],e[5] ) } )
endif
if !empty( ::anotopbox )
   aeval( ::anotopbox, { |e| notopbox( e[1],e[2],e[3],e[4] ) } )
endif
if( !empty(::aprompt),;
   aeval( ::aprompt,;
   { |e|;
     dispoutat( e[1]+::ntop,e[2]+::nleft,e[3],::promptcolor );
   }),)
if( !empty( ::dlgbar ),(;
   aeval( ::dlgbar,;
   { |e|;
     dispoutat( e[1], e[2],if( valtype( e[3] ) == "N", space( e[3] ), e[3] ), ::dlgcolor[4] ),;
     dispoutat( e[1], e[2]+if(valtype(e[3])=="N",e[3],len(e[3])),"ß", ::dlgcolor[3] ),;
     dispoutat( e[1]+1,e[2]+1,repl("Ü",if(valtype(e[3])=="N",e[3],len(e[3]))), ::dlgcolor[3] ),;
     aadd( ::adummy, {e[1]-::ntop,e[2]-::nleft,e[3]} ),;
     aadd( ::abuffer, (savescreen(e[1],e[2],e[1]+1,e[2]+if(valtype(e[3])=="N",e[3],len(e[3]))+1 ) ) );
   }),NIL),)
if(!(::cargo==nil),eval(::cargo),)
dispend()
return self

*******************************************************************************
METHOD set( top, left, bottom, right, _ctitle, _aprompt, _dlgbar,dlgcolor, _aprnprompt, _lclear, _boxname ) CLASS WINDOW

setblink(.f.)
::anotopbox := {}
::athickbox := {}
::amultibox := {}
::asmallbox := {}
::abox      := {}
::lconfig   := .f.
::ntop      := top
::nleft     := left
::nbottom   := bottom
::nright    := right
::ctitle    := _ctitle
if empty( ::dlgcolor ) .or. len( ::dlgcolor ) < 7
   ::dlgcolor := {"W+","GR+","BG","N/BG*","N/BG","BG/BG","N/BG*"}
endif
if( ::boxcolor    == nil, ::boxcolor    := ::dlgcolor[1]+"/"+::dlgcolor[3]+",n/w*", )
if( ::promptcolor == nil, ::promptcolor := ::dlgcolor[2]+"/"+::dlgcolor[3], )
if( ::titlecolor  == nil, ::titlecolor  := ::dlgcolor[2]+"/"+::dlgcolor[3], )
if( (::dlgbar  := _dlgbar)  == nil, ::dlgbar := {}, )
if( (::aprompt := _aprompt) == nil, ( ::aprompt := {}, ::lconfig := .t. ), ::configprompt() )
if( (::lclear  := _lclear)  == nil, ::lclear := .t., )
::adummy   := {}
::abuffer  := {}
::obox := wbox("",::ntop,::nleft,::nbottom,::nright,,,::boxcolor)
::obox:activate()
::boxname  := _boxname
return self

*******************************************************************************
METHOD write( nmargin ) CLASS WINDOW

local cmargin := space(nmargin)
local cret    := chr(13) + chr(10)
local lmenu   := .f.
local i
local nlen    := 0
local cmenu
local cwrite  := ""
local acolor  := ::dlgcolor
local aprompt := ::aprompt
local ntop    := ::ntop
local nleft   := ::nleft
local n
local ctext
local cname   := padr(::cproject,18)
local nhandle
local cscreen := (savescreen(11,24,14,56))
local getlist
local aradio  := {}, agets := {} , achecks := {}
local cmain   := ""
local chead   := ""
local ltype   := .f.
local lget    := .f.

local cProgBody := ""
local nRadio    := 0
local cRadArray := ""
local cRadCoor  := ""
local cRadGet   := ""
local aProgBody := {}
local cCheck    := ""
local nCheck    := 0
local nGet      := 0
local cGet      := ""

//ÄÄÄÄÄ file name to save ÄÄÄÄÄ//
if empty( cname )
   cname := space(18)
endif
boxshadow(11,24,13,55,"w+/b")
dispoutat(12,27,"Save As","w+/b")
getlist := getnew(12,35,{|e|if(e==NIL,cName,cName:=e)},"cName","@!","N/BG*,N/BG*")
getlist:display()
setcursor(1)
readmodal( { getlist } )
setcursor(0)
restscreen(11,24,14,56,(cscreen))
if is_escape()
   return self
endif
if empty( cname := alltrim( cname ) )
   for i := 1 to 10000
      if !file( cName := "DLG"+___nourut(i) + ".PRG" )
         exit
      endif
   next
endif
if at(".",cname) == 0
   cname += ".PRG"
endif
nhandle := fcreate( cname )

//ÄÄÄÄÄ Write The Program ÄÄÄÄÄ//
sboxnew("Writing Codes .....")
chead  += "// " + alltrim(upper(cname))+" Created By xHarbour Screen Designer" + cret
chead  += "// Created On : " + dtoc(date()) + " at " + time() + cret + cret
chead  += "#define RADIO      chr(40)+chr(4)+chr(41)" + cret
chead  += "#define CHECKBOX   chr(91)+chr(88)+chr(93)" + cret
// chead  += '#include "ibsverbs.ch"' + cret
// chead  += '#include "ibsinkey.ch"' + cret
cwrite += repl("*",80) + cret
cwrite += "static function q_dialog()" + cret
cwrite += repl("*",80) + cret
cwrite += "local odialog    := window():new("+ltrim(str(::ntop))+","+ltrim(str(::nleft))+","+ltrim(str(::nbottom))+","+ltrim(str(::nright))+;
          ',,,,{"w+","gr+","b","n/b","n/b","b/b","n/b"},,.f.)' + cret
if !empty( ::adummy )
   lmenu := .t.
   for i := 1 to len( ::adummy )
      if valtype( ::adummy[i][3] ) <> "C"
         nlen := 0
         exit
      endif
      if nlen < len( ::adummy[i][3] )
         nlen := len( ::adummy[i][3] )
      endif
   next
   if nlen > 0
      nlen += 1
   endif
   cwrite += 'local omenu      := menu():new("",'+ltrim(str(::ntop))+","+ltrim(str(::nleft))+","+ltrim(str(::nbottom))+","+ltrim(str(::nright))+",,,,,,.t.,.f.,.t.,1 )" + cret + cret
   aeval( ::adummy, { |e| cmenu := addtilde(e[3]), cwrite += ;
     'omenu:add({'+ cmenu + if(nlen>0,space(nlen-len(cmenu)),"") + ;
     ', nil, nil, .t., '+ ltrim(str(e[1]+::ntop))+", "+ltrim(str(e[2]+::nleft)) +" } )" + cret  } )

   cwrite += 'omenu:menucolor  := "n/w*"'  + cret
   cwrite += 'omenu:execcolor  := "gr/w*"' + cret
   cwrite += 'omenu:barcolor   := "w+/r"'  + cret
   cwrite += 'omenu:boxcolor   := odialog:boxcolor' + cret

   cwrite += cret
endif
if !empty(::boxname)
   cwrite += 'odialog:boxname := "' + ::boxname + '"' + cret
endif

//ÄÄÄ Write Codes For Other Components ÄÄÄ//
if !empty( aprompt )
   for n := 1 to len( aprompt )
     ctext := aprompt[n][3]
     if RADIO$ctext
        if empty(cmain)
           cmain := mainprog(@lGet)
        endif
        if !ltype
           ++ nRadio
           cmain += 'local ctype'+ltrim(str(nRadio))+'  := "' + subs(ctext,5) + '"' + cret
           ltype := .t.
        endif
        aadd(aradio,{aprompt[n][1]+ntop,aprompt[n][2]+nleft,subs(ctext,5)})
        ctext     := 'RADIO +"'+subs(ctext,4)+'" )'
        cwrite    += 'odialog:addprompt('+ltrim(str(aprompt[n][1]+ntop))+','+ltrim(str(aprompt[n][2]+nleft))+','+ ctext + cret
        cRadCoor  += "{"+ ltrim(str(atail( aradio )[1])) +"," + ltrim(str(atail( aradio )[2])) + "},"
        cRadArray += '"' + atail( aradio )[3] + '",'
        LOOP
     else
        if !Empty( cRadArray )
           if empty(cmain)
              cmain := mainprog(@lGet)
           endif
           // Trimming the last comma
#ifdef __CLIPPER__
           cRadArray := "{" + Left( cRadArray, len(cRadArray) - 1 ) + "}"
           cRadCoor  := "{" + Left( cRadCoor, len(cRadCoor) - 1 ) + "}"
#else
           cRadArray[-1] := " "
           cRadArray     := "{" + alltrim(cRadArray) + "}"
           cRadCoor[-1]  := " "
           cRadCoor      := "{" + alltrim(cRadCoor) + "}"
#endif
           cRadGet   := 'radiogets({|x|iif(x==nil,ctype'+ltrim(str(nRadio))+',ctype'+ltrim(str(nRadio))+':=x)},"CTYPE'+ltrim(str(nRadio))+'",' +;
                         cRadArray + ',getlist,ccolor1,' + cRadCoor + ',,)'
           // Reset Radio
           AADD( aProgBody, cmargin + cRadGet ) // cmargin
           cRadGet   := ""
           cRadArray := ""
           cRadCoor  := ""
           lType     := .F.
        endif
        // Write Radio Here
        if CHECKBOX$ctext
           ++ nCheck
           if empty(cmain)
              cmain := mainprog(@lGet)
           endif
           aadd(achecks,{aprompt[n][1]+ntop,aprompt[n][2]+nleft,subs(ctext,5)})
           cmain  += padr("local lcheck"+ltrim(str(len(achecks))),14)+":= .f." + cret
           ctext  := 'CHECKBOX +"'+subs(ctext,4)+'" )'
           cwrite += 'odialog:addprompt('+LTrim(Str(aPrompt[N][1]+nTop))+','+LTrim(Str(aPrompt[N][2]+nLeft))+','+ ctext + cret
           cCheck := [aadd(getlist,checkgetnew(]+padl(ltrim(str(atail(aChecks)[1])),2)+','+padl(ltrim(str(atail(aChecks)[2])),2)+[,{|x|iif(x==nil,]+'lcheck'+ltrim(str( nCheck))+','+'lcheck'+ltrim(str( nCheck))+':= x)},"'+'LCHECK'+ltrim(str( nCheck))+[","]+atail(aChecks)[3]+["]+',ccolor1,))'
           AAdd( aProgBody, cmargin + cCheck )
        elseif CHR(219)$ctext
           ++nGet
           if empty(cmain)
              cmain := mainprog(@lGet)
           endif
           aadd(agets,{aprompt[n][1]+ntop,aprompt[n][2]+nleft,ctext})
           if !lget
              cmain += "local getlist := {}" + cret
              lget  := .t.
           endif
           cmain  += padr("local aget"+ltrim(str(len(agets))),14) + ":= space("+ltrim(str(len(ctext))) + ")" + cret
           ctext  := _transtext( ctext )
           cwrite += 'odialog:addsmallbox({'+ltrim(str(aprompt[n][1]+ntop))+','+ltrim(str(aprompt[n][2]+nleft))+','+ctext+',,.f.})'+cret
           AAdd( aProgBody, cmargin + 'aadd(getlist,getnew('+padl(ltrim(str(atail(agets)[1])),2)+','+padl(ltrim(str(atail(agets)[2])),2)+',{|x|if(x==nil,'+'aget'+ltrim(str(nGet))+','+'aget'+ltrim(str(nGet))+':=x)},"VAR'+ltrim(str(nGet))+'",,ccolor))' )
           AAdd( aProgBody,cmargin + 'atail(getlist):preblock  := {||.t.}' )
           AAdd( aProgBody,cmargin + 'atail(getlist):postblock := {||.t.}' )
        else
           cwrite += 'odialog:addprompt('+ltrim(str(aprompt[n][1]+ntop))+','+ltrim(str(aprompt[n][2]+nleft))+',"'+ ctext+'")'+ cret
        endif
     endif
   next
   // Should Check Again Here
   // Because There is a LOOP in FOR-NEXT Above
   if !Empty( cRadArray )
      if empty(cmain)
         cmain := mainprog(@lGet)
      endif
      // Trimming the last comma
#ifdef __CLIPPER__
      cRadArray := "{" + Left( cRadArray, len(cRadArray) - 1 ) + "}"
      cRadCoor  := "{" + Left( cRadCoor, len(cRadCoor) - 1 ) + "}"
#else
      cRadArray[-1] := " "
      cRadCoor[-1]  := " "
      cRadArray     := "{" + alltrim(cRadArray) + "}"
      cRadCoor      := "{" + alltrim(cRadCoor)  + "}"
#endif
      cRadGet   := 'radiogets({|x|iif(x==nil,ctype'+ltrim(str(nRadio))+',ctype'+ltrim(str(nRadio))+':=x)},"CTYPE'+ltrim(str(nRadio))+'",' +;
                    cRadArray + ',getlist,ccolor1,' + cRadCoor + ',,)'
      AADD( aProgBody, cmargin + cRadGet ) // cmargin
   endif
endif

if lmenu
   cwrite += "return { odialog, omenu }" + cret
else
   cwrite += "return { odialog }" + cret
endif

if !empty(cmain)
   cmain += cret + 'dispbegin()'+ cret
   cmain += cmargin + 'adialog[1]:display()'+ cret
   if lMenu
      cmain += cmargin + 'adialog[2]:display()'+ cret
   endif
else
   cmain := repl("*",80) + cret
   cmain += "function main()" + cret
   cmain += repl("*",80) + cret
   cmain += "local adialog := q_dialog()"    + cret
   cmain += 'local ccolor  := "n/bg*,n/bg*"' + cret
   cmain += cret + 'dispbegin()'+ cret
   if lMenu
      cmain += cmargin + 'adialog[1]:display()'+ cret
      cmain += cmargin + 'adialog[2]:display()'+ cret
   else
      cmain += cmargin + 'adialog[1]:display()'+ cret
   endif
endif

if !empty(aProgBody)
   for n := 1 to len(aProgBody)
      cmain += aProgBody[n] + cret
   next
endif

if !empty( cmain )
   if lGet
      cmain += cmargin + 'aeval( getlist, { |o| o:display() } )'+ cret
   endif
   cmain += 'dispend()'+ cret
   if lGet
      cmain += 'setcursor(1)' + cret
      cmain += 'readmodal(getlist)' + cret
      cmain += 'setcursor(0)' + cret + cret
   endif
   if lmenu
      cmain += 'if adialog[2]:exec() == 1' + cret
      cmain += cmargin + 'adialog[1]:hide()' + cret
      cmain += cmargin + '// do something here' + cret
      cmain += 'endif' + cret
      cmain += 'adialog[1]:hide()' + cret
   endif
   cmain += 'return nil' + cret
   cwrite := chead + cmain + cret + cwrite
else
   cwrite := chead + cwrite
endif

fwrite( nhandle , cwrite )
fclose( nhandle )
un_sbox()
alert("Resource Saved As " + cName )
// alert_01("Resource Saved As " + cName )
// ft_idle()
return cname

*******************************************************************************
static function mainprog( lGet )
local cret  := chr(13)+chr(10)
local cmain := repl("*",80) + cret
cmain += "function main()" + cret
cmain += repl("*",80) + cret
cmain += "local adialog := q_dialog()"   + cret
cmain += 'local ccolor  := "n/bg*,/bg*"' + cret
cmain += 'local ccolor1 := "gr+/b,gr+/b"'+ cret
cmain += 'local getlist :=  {}'          + cret
lGet := .T.
return cmain

*******************************************************************************
static function addtilde( element )
local nspace
if valtype(element)=="N"
   return 'space('+ltrim(str(element))+')'
endif
nspace := ((len( element ) - len( alltrim( element ) ))/2)-1
return '"'+space(nspace)+'~'+alltrim(element)+space(nspace)+'"'

*******************************************************************************
static function _transtext( ctext )
if chr(219)$ctext
   return 'space('+ltrim(str(len(ctext)))+')'
endif
return '"'+ctext+'"'

*******************************************************************************
METHOD edit() CLASS WINDOW
local t,l,b,r,i
local nchoice := 1, rchoice := 1
local _atestbar,_tempscreen
local cbarcaption := space(::nright-::nleft-1)
local lget := 0
local aselect := {;
                 { " Get         ALT+G    " ,;
                   " Check       ALT+K    " ,;
                   " Horizontal  ALT+L    " ,;
                   " Grid        ALT+O    " ,;
                   " Radio       ALT+R    " ,;
                   " Upper Left  ALT+Q    " ,;
                   " Text        ALT+T    " ,;
                   " Vertical    ALT+V    " ,;
                   " Upper Right ALT+X    " ,;
                   " Lower Left  ALT+Y    " ,;
                   " Lower Right ALT+Z    " ,;
                   " Button      ALT+Enter" ,;
                   " Center      F2       " ,;
                   " View        F3       " ,;
                   " Save        F6       " ,;
                   " Title       F7       " ,;
                   " Load        ALT+F6   "},;
                 { K_ALT_G                  ,;
                   K_ALT_K                  ,;
                   K_ALT_L                  ,;
                   K_ALT_O                  ,;
                   K_ALT_R                  ,;
                   K_ALT_Q                  ,;
                   K_ALT_T                  ,;
                   K_ALT_V                  ,;
                   K_ALT_X                  ,;
                   K_ALT_Y                  ,;
                   K_ALT_Z                  ,;
                   K_ALT_ENTER              ,;
                   K_F2                     ,;
                   K_F3                     ,;
                   K_F6                     ,;
                   K_F7                     ,;
                   K_ALT_F6                 }}
local nselect := 1, nrow := 1
local getnew
local ctext  := space(::nright-::nleft-1)
local nele, _aele := {}
local aele   := {}
local nkey
local lok    := .f.
local nevent := 0
local nedit  := 0
local _xcolor := "gr+/"+pickcolor(pickcolor(::boxcolor,1,","),2,"/")
local ominimize := window():new(23,0,24,5,,,,,,.f.)
local isopen    := .t.
local aret, fl := space(50)
local nprompts, iele
g_save()
ominimize:activate()
if ::atestbar == nil
   ::atestbar := {}
endif
if( empty( ::adummy  ), ::adummy   := {},)
if( empty( ::abuffer ), ::abuffer  := {},)

while !lok
   t           := ::ntop
   l           := ::nleft
   b           := ::nbottom
   r           := ::nright
   _aele       := {}
   aele        := {}
   cbarcaption := space(::nright-::nleft-1)
   if nevent == 0
      lget  := 0
      nedit := 0
#ifdef __CLIPPER__
      while nextkey() == 0
         ol_yield()
      enddo
      nkey  := inkey()
#else
      nkey  := inkey(0)
#endif
   endif
   do case
   case nkey == K_F2
      boxcenter(@t,@l,@b,@r)
      ::ntop    :=  t
      ::nleft   :=  l
      ::nbottom :=  b
      ::nright  :=  r
      dispbegin()
         ::hide:displaybox()
      dispend()
      nevent := 0
   case nkey == K_F1
      if ( nselect := menuchoice(aselect[1],{00,00,19,27},,@nselect,"") ) > 0
         nevent  := 1
         nkey    := aselect[2][nselect]
      else
         nselect := 1
         nrow    := 1
      endif
   case nkey == K_F3
      _aele    := {}
      aele     := {}
      iele     := 1
      nprompts := 0
      if !empty(::aprompt) .OR. !empty(::aDummy)
         for nele := 1 to len( ::aprompt )
            aadd( _aele, "PROMPT" )
            aadd( aele , " "+str( iele,2) + " ³ "+str(::aprompt[nele][1],2)+" ³ "+ str(::aprompt[nele][2],2)+ " ³ "+ padr(::aprompt[nele][3],20) )
            iele ++
         next
         nprompts := len( aele )
         for nele := 1 to len(::adummy)
            aadd( _aele,  "BUTTON" )
            aadd( aele  , " "+str(iele,2)+" ³ "+str(::adummy[nele][1],2)+" ³ "+ str(::adummy[nele][2],2)+ " ³ "+ ;
            if(valtype(::adummy[nele][3])=="C",padr(alltrim(::adummy[nele][3]),20),str(::adummy[nele][3],2)) )
            iele ++
         next
         if ( nchoice := menuchoice(aele,{00,00,10,55},,@nchoice,"ntop="+ltrim(str(::ntop))+" nleft="+ltrim(str(::nleft))+" nright="+ltrim(str(::nright))+" nbottom="+ltrim(str(::nbottom))) ) > 0
            if _aele[nchoice] == "BUTTON"
               nedit  := nchoice - nPrompts
               nevent := 1
               nkey   := K_ALT_B
            elseif _aele[nchoice] == "PROMPT"
               nedit  := nchoice
               nevent := 1
               nkey   := K_ALT_E
            endif
         else
            nchoice := 1
            rchoice := 1
         endif
      else
         nevent := 0
      endif
   case nkey == K_ESC
      lok := .t.
   case nkey == K_ALT_F6
      if !atail( aret := getfname( "File Name",,.f., @fl,.f.,nil,))
      // if !atail( aret := getfname( "File Name",,.f., @fl,.f.,nil,{ |x| x:varput( padr( dirmanager (,,,1), 50 ) ) ,__keyboard(chr(13))} ))
         exit
      endif
      setcursor(0)
      ::grabcomponent( aret[1] )
      dispbegin()
         ::hide:displaybox()
      dispend()
      nevent := 0
   case nkey == K_F6
      if isopen
         lok := .t.
      endif
   case nkey == K_ALT_E
      if !empty( ::aprompt )
         dispoutat(::ntop+::aprompt[nedit][1],::nleft+::aprompt[nedit][2],::aprompt[nedit][3],_xcolor)
         if chr(219)$::aprompt[nedit][3]
            lget := 1
         elseif chr(196)$::aprompt[nedit][3]
            lget := 2
         endif
         ::textdesign( nedit, lget )
         dispbegin()
         ::hide:displaybox()
         dispend()
      endif
      nevent := 0
   case nkey == K_CTRL_ENTER
      if !empty( ::adummy )
         ::hideallbar()
         ::atestbar := {}
         ::adummy   := {}
         ::abuffer  := {}
      endif
   case nkey == K_ALT_O
      if " "$::oBox:Frame
         ::obox:frame := strtran(::obox:frame," ",".")
      else
         ::obox:frame := strtran(::obox:frame,"."," ")
      endif
      dispbegin()
      ::hide():displaybox()
      dispend()
      nevent := 0
   case nkey == K_ALT_ENTER
      ::barproperty := {::ntop+1, ::nleft+1, 10 }
      _tempscreen   := (savescreen(::ntop+1,::nleft+1,::ntop+1,::nright-1))
      getnew        := getnew(::ntop+1,::nleft+1,{|x|if(x==nil,cbarcaption,cbarcaption:=x)},"cbarcaption","@k","n/bg*,n/bg*" )
      getnew:display()
      setcursor(1)
      readmodal( { getnew } )
      setcursor(0)
      restscreen(::ntop+1,::nleft+1,::ntop+1,::nright-1,(_tempscreen))
      if !( lastkey() == 27 ) .and. !empty( cbarcaption )
         ::barproperty[3] := " " + alltrim(cbarcaption) + " "
      endif
      ::designbar(.t., nedit )
      nevent := 0
   case nkey == K_ALT_B
      if !empty( ::adummy )
         ::barproperty := if( nedit == 0,{atail(::adummy)[1]+::ntop,atail(::adummy)[2]+::nleft,atail(::adummy)[3]},{::adummy[nedit][1]+::ntop,::adummy[nedit][2]+::nleft,::adummy[nedit][3]} )
         ::designbar(.f., nedit )
         dispbegin()
         ::hide():displaybox()
         dispend()
      endif
      nevent := 0
   case ascan({K_UP,K_DOWN,K_LEFT,K_RIGHT,K_CTRL_UP,K_CTRL_DOWN,K_CTRL_LEFT,K_CTRL_RIGHT,K_ALT_UP,K_ALT_DOWN,K_ALT_LEFT,K_ALT_RIGHT},{|e|e==nkey}) > 0
      do case
      case nkey == K_UP
         if !( ::ntop == 0 )
            ::ntop --
            ::nbottom --
         endif
      case nkey == K_DOWN
         if !( ::nbottom == maxrow() )
            ::ntop ++
            ::nbottom ++
         endif
      case nkey == K_LEFT
         if !( ::nleft == 0 )
            ::nleft--
            ::nright--
         endif
      case nkey == K_RIGHT
         if !( ::nright == maxcol() )
            ::nleft ++
            ::nright ++
         endif
      case nkey == K_CTRL_UP
         if !( ::ntop == 0 )
            ::ntop --
         endif
      case nkey == K_CTRL_DOWN
         if !( ::nbottom == maxrow() )
            ::nbottom ++
         endif
      case nkey == K_CTRL_LEFT
         if !( ::nleft == 0 )
            ::nleft--
         endif
      case nkey == K_CTRL_RIGHT
         if !( ::nright == maxcol() )
            ::nright ++
         endif
      case nkey == K_ALT_UP
         if !( ::nbottom == ::ntop )
            ::nbottom --
         endif
      case nkey == K_ALT_DOWN
         if !( ::ntop == ::nbottom )
            ::ntop ++
         endif
      case nkey == K_ALT_LEFT
         if !( ::nRight == ::nLeft )
            ::nright--
         endif
      case nkey == K_ALT_RIGHT
         if !( ::nLeft == ::nRight )
            ::nleft++
         endif
      endcase
      dispbegin()
      ::hide()
      ::displaybox()
      dispend()
   case nkey == K_CTRL_F10
      if isopen
         lok := .t.
      endif
   case ascan({ K_ALT_T, K_ALT_R, K_ALT_S, K_ALT_K, K_ALT_G, K_ALT_L, K_ALT_V, K_ALT_Q, K_ALT_X, K_ALT_Y, K_ALT_Z, K_F7 }, nkey ) > 0
      if isopen
         _xcolor := "gr+/"+pickcolor(pickcolor(::boxcolor,1,","),2,"/")
         if ascan({K_ALT_T,K_ALT_R,K_ALT_K,K_F7,K_ALT_S},{|e|e==nkey}) > 0
            ctext := space(::nright-::nleft-1)
            _xcolor += ",N/BG*"
            if nkey == K_F7
               if !empty(::boxname)
                  ctext := padr(::boxname,::nright-::nleft-1)
               endif
            endif
         elseif nkey == K_ALT_G
            lget  := 1
            ctext := repl(chr(219),10)
         elseif nkey == K_ALT_L
            lget  := 2
            ctext := repl(chr(196),10)
         elseif nkey == K_ALT_V
            lget  := 4
            ctext := "³"
         elseif nkey == K_ALT_Q
            lget  := 4
            ctext := "Ú"
         elseif nkey == K_ALT_X
            lget  := 4
            ctext := "¿"
         elseif nkey == K_ALT_Y
            lget  := 4
            ctext := "À"
         elseif nkey == K_ALT_Z
            lget  := 4
            ctext := "Ù"
         endif
         if lget != 3
            getnew := getnew(::ntop+1,::nleft+1,{|x|if(x==nil,ctext,ctext:=x)},"ctext","@k",_xcolor )
            getnew:preblock  := {||ascan({K_ALT_T,K_ALT_R,K_ALT_K,K_F7,K_ALT_S},{|e|e==nkey})>0}
            getnew:postblock := {||if(nkey==K_F7,.t.,attachsymbol( @ctext, nkey, ::ntop+1,::nleft+1,::nright-::nleft-1, _xcolor ))}
            getnew:display()
            setcursor(1)
            readmodal( { getnew } )
            setcursor(0)
            if !( lastkey() == 27 ) .and. !empty( ctext )
               setcursor(0)
               if nkey != K_F7
                  aadd( ::aprompt, { 1,1, ctext } )
                  ::textdesign(nedit,lget)
               else
                  ::boxname := alltrim( ctext )
               endif
            endif
            dispbegin()
            ::hide:displaybox()
            dispend()
         endif
      endif
      nevent := 0
   case setkey( nkey ) != nil
      eval( setkey( nkey ), procname(1), procline(1), readvar() )
   endcase
#ifdef __CLIPPER__
   ft_idle()
#endif
enddo
g_restore()
// ft_idle()
return self

*******************************************************************************
METHOD grabcomponent( cfile ) CLASS WINDOW

local atext := afilltext( cfile )
local n, i
local acoord := {}, scoord

for n := 1 to len( atext )
   if "window"$lower(atext[n])
      if ( i := at( "new", lower(atext[n]) ) ) > 0
         scoord    := subs( atext[n], i + 4 )
         acoord    := pickcolor( scoord,,",",.t.)
         ::ntop    := val( acoord[1] )
         ::nleft   := val( acoord[2] )
         ::nbottom := val( acoord[3] )
         ::nright  := val( acoord[4] )
      endif
   endif
                         //   12345678901234567
   if lower(left( atext[n],17 )) == "odialog:addprompt"
      ::parseprompt( atext[n], 1 )
   endif
   if lower(left( atext[n],19)) == "odialog:addsmallbox"
      ::parseprompt( atext[n], 2 )
   endif
   if lower(left( atext[n],9 )) == "omenu:add"
      ::parsebutton( atext[n] )
   endif
   if lower(left( atext[n], 15)) == "odialog:boxname"
      ::boxname := subs( atext[n], at( '"',atext[n] ) + 1 )
      ::boxname := strtran(::boxname,'"',"")
   endif
next
return self

*******************************************************************************
METHOD parsebutton( ctext ) CLASS WINDOW
local cparse
local nrat := rat("}", ctext )
local s_sample := {}

if ::atestbar == nil
  ::atestbar := {}
endif
if ::dlgbar == nil
  ::dlgbar := {}
endif

cparse      := subs( ctext, 12 )
cparse      := strtran( cparse,"} )","")
s_sample    := pickcolor( cparse,,",",.t.)
s_sample[1] := left(s_sample[1],rat(["],s_sample[1])-1)
s_sample[1] := strtran(s_sample[1],'"',"")
s_sample[1] := strtran(s_sample[1],'~',"")
aadd( ::adummy, { val(s_sample[5])-::ntop,val(s_sample[6])-::nleft," "+s_sample[1]+" " } )
aadd( ::abuffer, (savescreen(;
  val(s_sample[5]),val(s_sample[6]),;
  val(s_sample[5])+1,val(s_sample[6])+1+len(" "+s_sample[1]+" ") ) ))
aadd( ::atestbar, { val(s_sample[5]),val(s_sample[6])," "+s_sample[1]+" " })
aadd( ::dlgbar,   { val(s_sample[5]),val(s_sample[6])," "+s_sample[1]+" " })

return self

*******************************************************************************
METHOD parseprompt( ctext, nprompt ) CLASS WINDOW

local s_sample := {}
local nparse
local cspace
local nspace

if nprompt == 1
   nparse := 19
   ctext := subs( ctext, nparse )
   ctext := strtran(ctext,")","")
else
   nparse := 22
   ctext := subs( ctext, nparse )
   ctext := strtran(ctext,"}","")
   ctext := left( ctext, rat(")",ctext ) - 1  )
endif

s_sample := pickcolor( alltrim(ctext),,",",.t.)

if nprompt == 1
   if "RADIO"$s_sample[3]
      s_sample[3] := strtran(s_sample[3],"RADIO ",CHR(40)+CHR(4)+CHR(41))
   elseif "CHECKBOX"$s_sample[3]
      s_sample[3] := strtran(s_sample[3],"CHECKBOX ",CHR(91)+CHR(88)+CHR(93))
   endif
   s_sample[3] := strtran(s_sample[3],'"',"")
   s_sample[3] := strtran(s_sample[3],'+',"")
else
   cspace      := s_sample[3]
   nspace      := subs( cspace, at( "(", cspace ) + 1,  rat(")",cspace ) - 1 )
   nspace      := val( nspace )
   s_sample[3] := repl(chr(219), nspace )
endif
aadd( ::aprompt, { val( s_sample[1] )-::ntop, val( s_sample[2] )-::nleft, s_sample[3] } )
return self

*******************************************************************************
static function attachsymbol( ctext, nkey, nrow, ncol, npad, ccolor )
ctext := alltrim( ctext )
if nkey == K_ALT_R
   ctext := RADIO + " " + ctext
elseif nkey == K_ALT_K
   ctext := CHECKBOX + " " + ctext
endif
dispoutat( nrow,ncol,padr( ctext, npad ), ccolor )
return .t.

*******************************************************************************
METHOD textdesign( nedit, lget ) CLASS WINDOW
local xchars   := if(lget == 1, chr(219), chr(196) )
local lstart   := .t.
local nkey
local lok      := .f.
local testtext := atail( ::aprompt )
local cscreen
if !empty( nedit )
   testtext := ::aprompt[ nedit ]
endif
cscreen  := (savescreen( ::ntop+testtext[1], ::nleft+testtext[2], ::ntop+testtext[1], ::nleft+testtext[2]+len(testtext[3]) ))
while !lok
#ifdef __CLIPPER__
   while nextkey() == 0
      ol_yield()
   enddo
   nkey := inkey()
#else
   nkey := inkey(0)
#endif
   do case
   case nkey == K_ALT_RIGHT .AND. lGET > 0 .AND. lget < 4
      if !( testtext[2]+::nleft+len(testtext[3]) == ::nright - 1 )
         if( lstart, ::savecur( testtext, .t. ), ::restprev( testtext, cscreen ) )
         lstart := .f.
         testtext[3] := repl( xchars ,len(testtext[3])+ 1)
         cscreen  := (savescreen( ::ntop+testtext[1], ::nleft+testtext[2], ::ntop+testtext[1], ::nleft+testtext[2]+len(testtext[3]) ))
         ::savecur( testtext, .f. )
      endif
   case nkey == K_ALT_LEFT .AND. lGET > 0  .AND. lget < 4
      if len(testtext[3]) > 1
         if( lstart, ::savecur( testtext, .t. ), ::restprev( testtext, cscreen ) )
         lstart := .f.
         testtext[3] := repl( xchars, len(testtext[3]) - 1 )
         cscreen  := (savescreen( ::ntop+testtext[1], ::nleft+testtext[2], ::ntop+testtext[1], ::nleft+testtext[2]+len(testtext[3]) ))
         ::savecur( testtext, .f. )
      endif
   case nkey == K_UP
      if !( ::ntop + testtext[1] == ::ntop + 1 )
        if( lstart, ::savecur( testtext, .t. ), ::restprev( testtext, cscreen ) )
        lstart := .f.
        testtext[1] --
        cscreen  := (savescreen( ::ntop+testtext[1], ::nleft+testtext[2], ::ntop+testtext[1], ::nleft+testtext[2]+len(testtext[3]) ))
        ::savecur( testtext, .f. )
      endif
   case nkey == K_DOWN
      if !( ::ntop + testtext[1] == ::nbottom - 1 )
        if( lstart, ::savecur( testtext, .t. ), ::restprev( testtext, cscreen ) )
        lstart := .f.
        testtext[1] ++
        cscreen  := (savescreen( ::ntop+testtext[1], ::nleft+testtext[2], ::ntop+testtext[1], ::nleft+testtext[2]+len(testtext[3]) ))
        ::savecur( testtext, .f. )
      endif
   case nkey == K_LEFT
      if !( ::nleft + testtext[2] == ::nleft + 1 )
        if( lstart, ::savecur( testtext, .t. ), ::restprev( testtext, cscreen ) )
        lstart := .f.
        testtext[2] --
        cscreen  := (savescreen( ::ntop+testtext[1], ::nleft+testtext[2], ::ntop+testtext[1], ::nleft+testtext[2]+len(testtext[3]) ))
        ::savecur( testtext, .f. )
      endif
   case nkey == K_DEL
#ifndef __CLIPPER__
      ADel(::aprompt,nedit,.T.)
#else
      ADel(::aprompt,nedit)
      ASize(::aprompt,Len(::aprompt)-1)
#endif
      lok := .t.
   case nkey == K_RIGHT
      if !( ::nleft + testtext[2] + len( testtext[3] )== ::nright - 1 )
        if( lstart, ::savecur( testtext, .t. ), ::restprev( testtext, cscreen ) )
        lstart := .f.
        testtext[2] ++
        cscreen  := (savescreen( ::ntop+testtext[1], ::nleft+testtext[2], ::ntop+testtext[1], ::nleft+testtext[2]+len(testtext[3]) ))
        ::savecur( testtext, .f. )
      endif
   case nkey == K_ENTER
      if empty( nedit )
         atail( ::aprompt )[1] := testtext[1]
         atail( ::aprompt )[2] := testtext[2]
      else
         ::aprompt[nedit][1] := testtext[1]
         ::aprompt[nedit][2] := testtext[2]
      endif
      lok := .t.
   endcase
   // ft_idle()
enddo
return self

********************************************************************************
method savecur( testtext, lblank ) CLASS WINDOW
local ccolor := "GR+/"+pickcolor(::promptcolor,2,"/")
dispoutat( ::ntop+testtext[1], ::nleft+testtext[2],if(lblank, space( len( testtext[3] )), testtext[3] ), ccolor /*::promptcolor*/ )
return self

********************************************************************************
method restprev( testtext, cscreen ) CLASS WINDOW
restscreen( ::ntop+testtext[1], ::nleft+testtext[2], ::ntop+testtext[1], ::nleft+testtext[2]+len(testtext[3]),(cscreen ))
return self

********************************************************************************
METHOD designbar( lnew, nedit ) CLASS WINDOW
local ntextlen
local nbarlen, ltext
local lbar := .f., nbar, ldisp := .t.
local nup, ndown

while !lbar
   if ldisp
      dispbegin()
      if !lnew
        ::hidebar()
      endif
      ::displaybar()
      dispend()
      ldisp := .f.
   endif
#ifdef __CLIPPER__
   while nextkey() == 0
      ol_yield()
   enddo
   nbar  := inkey()
#else
   nbar := inkey(0)
#endif
   do case
   case nbar == K_ENTER
      ltext   := (valtype(::barproperty[3])=="C")
      nbarlen := if(ltext,len(::barproperty[3]),::barproperty[3])
      if !lnew
         if( nedit == 0, nedit := len( ::atestbar ), )
         ::atestbar[nedit] := ::barproperty
         ::adummy  [nedit] := {::barproperty[1]-::ntop,::barproperty[2]-::nleft,::barproperty[3]}
         ::abuffer [nedit] := (savescreen(::barproperty[1],::barproperty[2],::barproperty[1]+1,::barproperty[2]+nbarlen+1 ))
         ::abuffer [nedit] := (savescreen(::barproperty[1],::barproperty[2],::barproperty[1]+1,::barproperty[2]+nbarlen+1 ))
      else
         aadd( ::atestbar, ::barproperty )
         aadd( ::adummy, {::barproperty[1]-::ntop,::barproperty[2]-::nleft,::barproperty[3]} )
         aadd( ::abuffer, (savescreen(::barproperty[1],::barproperty[2],::barproperty[1]+1,::barproperty[2]+nbarlen+1 ) ))
      endif
      dispbegin()
      ::hide()
      ::displaybox()
      dispend()
      ::dlgbar := aclone( ::atestbar )
      lbar     := .t.
   case nbar == K_UP
      if !( ::barproperty[1] == ::ntop + 1 )
        dispbegin()
        ::hidebar():barproperty[1]--
        ::displaybar()
        dispend()
      endif
   case nbar == K_ALT_UP
      dispbegin()
      ::hidebar()
      nup := ::barproperty[1] - 3
      if nup > ::ntop + 1
        ::barproperty[1] -= 3
      else
        ::barproperty[1] := ::ntop + 1
      endif
      ::displaybar()
      dispend()
   case nbar == K_DOWN
      if !( ::barproperty[1] == ::nbottom - 2 )
        dispbegin()
        ::hidebar():barproperty[1]++
        ::displaybar()
        dispend()
      endif
   case nbar == K_ALT_DOWN
      dispbegin()
      ::hidebar()
      ndown := ::BarProperty[1] + 3
      if ndown < ::nBottom - 2
        ::hidebar():barproperty[1] += 3
      else
        ::hidebar():barproperty[1] := ::nBottom - 2
      endif
      ::displaybar()
      dispend()
   case nbar == K_RIGHT
      if !( ::BarProperty[2]+if(ValType(::BarProperty[3])=="N",::BarProperty[3],Len(::BarProperty[3])) == ::nRight - 1 )
        dispbegin()
        ::hidebar():barproperty[2]++
        ::displaybar()
        dispend()
      endif
   case nbar == K_ALT_RIGHT
      dispbegin()
      ::hidebar()
      nup := ::BarProperty[2] + 3 + if(ValType(::BarProperty[3])=="N",::BarProperty[3],Len(::BarProperty[3]))
      if nup < ::nright - 1
        ::barproperty[2] += 3
      else
        ::barproperty[2] := ::nright - 1 - if(ValType(::BarProperty[3])=="N",::BarProperty[3],Len(::BarProperty[3]))
      endif
      ::displaybar()
      dispend()
   case nbar == K_LEFT
      if !( ::barproperty[2] == ::nleft + 1 )
        dispbegin()
        ::hidebar():barproperty[2]--
        ::displaybar()
        dispend()
      endif
   case nbar == K_ALT_LEFT
      dispbegin()
      ::hidebar()
      nup := ::barproperty[2] - 3
      if nup > ::nleft + 1
        ::barproperty[2] -= 3
      else
        ::barproperty[2] := ::nleft + 1
      endif
      ::displaybar()
      dispend()
   case nbar == K_DEL
#ifndef __CLIPPER__
      Adel( ::atestbar, nedit, .T. )
      ADel( ::adummy, nedit, .T. )
      ADel( ::abuffer, nedit, .T. )
#else
      Adel( ::atestbar, nedit )
      ADel( ::adummy, nedit )
      ADel( ::abuffer, nedit )
      ASize( ::atestbar, Len(::atestbar) -1 )
      ASize( ::adummy,   Len(::adummy  ) -1 )
      ASize( ::abuffer,  Len(::abuffer ) -1 )
#endif
      lbar := .t.
   case nbar == K_CTRL_RIGHT
      ltext   := (valtype(::barproperty[3])=="C")
      nbarlen := if(ltext,Len(::barproperty[3]),::barproperty[3])
      if !( nbarlen == ::nright - ::nleft - 2 )
         dispbegin()
         ::hidebar()
         nbarlen ++
         ::barproperty[3] := if(ltext,padc(alltrim(::barproperty[3]),nbarlen),nbarlen)
         ::displaybar()
         dispend()
      endif
   case nbar == K_CTRL_LEFT
      ltext    := (valtype(::barproperty[3])=="C")
      nbarlen  := if(ltext,Len(::barproperty[3]),::barproperty[3])
      if ltext
        ntextlen := len(alltrim(::barproperty[3])) + 2
        if !( nbarlen == ntextlen )
          dispbegin()
          ::hidebar()
          nbarlen --
          ::barproperty[3] := padc(alltrim(::barproperty[3]),nbarlen)
          ::displaybar()
          dispend()
        endif
      else
        if !( nbarlen == 1 )
          dispbegin()
          ::hidebar():barproperty[3] --
          ::displaybar()
          dispend()
        endif
      endif
   endcase
   // ft_idle()
enddo
return self

********************************************************************************
METHOD displaybox() CLASS WINDOW
local scol := setcolor()
dispbegin()
  if !empty( ::boxname )
    ::obox:setname( ::boxname )
  endif
  ::obox:ntop    := ::ntop
  ::obox:nleft   := ::nleft
  ::obox:nbottom := ::nbottom
  ::obox:nright  := ::nright
  ::obox:display()
  ::displayallbar()
  if( !empty(::aprompt), aeval( ::aprompt, { |e| dispoutat( e[1]+::ntop,e[2]+::nleft,e[3], ::promptcolor )}), )
dispend()
return self

*******************************************************************************
METHOD displayallbar() CLASS WINDOW
local ntop     := ::ntop
local nleft    := ::nleft
local adummy   := ::adummy
local dlgcolor := ::dlgcolor
local i

if !empty( adummy )
   for i := 1 to len( adummy )
      ::abuffer[i] := (savescreen(ntop+adummy[i][1],nleft+adummy[i][2],ntop+adummy[i][1]+1,nleft+adummy[i][2]+if(valtype(adummy[i][3])=="N",adummy[i][3],len(adummy[i][3]))+1))
      dispoutat(ntop+adummy[i][1],nleft+adummy[i][2],if(valtype(adummy[i][3])=="N",space(adummy[i][3]),adummy[i][3]), dlgcolor[7])
      dispoutat(ntop+adummy[i][1],nleft+adummy[i][2]+if(valtype(adummy[i][3])=="N",adummy[i][3],len(adummy[i][3])),"ß",::dlgcolor[3])
      dispoutat(ntop+adummy[i][1]+1,nleft+adummy[i][2]+1,repl("Ü",if(valtype(adummy[i][3])=="N",adummy[i][3],len(adummy[i][3]))),::dlgcolor[3])
   next
endif
return self

*******************************************************************************
METHOD hideallbar() CLASS WINDOW
aeval( ::adummy, { |e,n| restscreen(::ntop+e[1],::nleft+e[2],::ntop+e[1]+1,::nleft+e[2]+if(valtype(e[3])=="N",e[3],len(e[3]))+1,(::abuffer[n])) } )
return self

*******************************************************************************
METHOD hidebar() CLASS WINDOW
if !empty(::barbuffer)
   restscreen(::barproperty[1],::barproperty[2],::barproperty[1]+1,::barproperty[2]+if(valtype(::barproperty[3])=="N",::barproperty[3],len(::barproperty[3]))+1,(::barbuffer))
endif
return self

*******************************************************************************
METHOD configprompt() CLASS WINDOW
aeval( ::aprompt, { |e| e[1] := e[1] - ::ntop, e[2] := e[2] - ::nleft } )
return self

*******************************************************************************
METHOD displaybar() CLASS WINDOW
local barproperty := ::barproperty
local dlgcolor    := ::dlgcolor
::barbuffer       := (savescreen(barproperty[1],barproperty[2],barproperty[1]+1,barproperty[2]+if(valtype(barproperty[3])=="N",barproperty[3],len(barproperty[3]))+1))
dispoutat(barproperty[1],barproperty[2],if(valtype(barproperty[3])=="N",space(barproperty[3]),barproperty[3]),dlgcolor[4])
dispoutat(barproperty[1],barproperty[2]+if(valtype(barproperty[3])=="N",barproperty[3],len(barproperty[3])),"ß",::dlgcolor[3])
dispoutat(barproperty[1]+1,barproperty[2]+1,repl("Ü",if(valtype(barproperty[3])=="N",barproperty[3],len(barproperty[3]))),::dlgcolor[3])
return self

********************************************************************************
static function menuchoice(amenu,abox,ccolor,nstart,ctitle)
********************************************************************************
local acolor
local ncursor := setcursor(0)
local nstruel := 1
local okey
local npos
local bkeepbrowsing := .t.
local laccomodate
local ostru
local boxcolor
local obox
local _ccolor := "W+/B"
local akey := {;
   { K_UP,   { || ostru:up() } },       ;
   { K_DOWN, { || ostru:down() } },     ;
   { K_PGUP, { || ostru:pageup() } },   ;
   { K_PGDN, { || ostru:pagedown() } }, ;
   { K_HOME, { || ostru:gotop()   } },  ;
   { K_END,  { || ostru:gobottom() } }  ;
   }

if empty( amenu )
   errorbeep()
   return 0
endif

hb_default( @abox, {05,14,20,65} )
hb_default( @ccolor, "W+/B,W+/N" )
hb_default( @cTitle, "Select Option" )

laccomodate         := len( amenu ) > ( abox[3] - abox[1] - 2 )
nstruel             := 1
ostru               := tbrowsenew(abox[1]+2,abox[2]+1,abox[3]-1,abox[4]-if(laccomodate,2,1))
ostru:gotopblock    := { || nstruel := 1 }
ostru:gobottomblock := { || nstruel := len( amenu ) }
ostru:skipblock     := { |n| skiparray( len( amenu ), @nstruel, n ) }
ostru:colorspec     := "W+/B,W+/N"
ostru:addcolumn(tbcolumnnew("",{||padr(amenu[nstruel],abox[4]-abox[2]+1)}))

boxcolor       := "W+/B,N/W*"
obox           := wbox(ctitle,abox[1],abox[2],abox[3],abox[4],,,boxcolor)
obox:barcolor  := pickcolor(obox:boxcolor,1,"/")+"/w*"
obox:activate()
dispbegin()
   obox:display()
dispend()

if nstart#nil .and. nstart <= len( amenu )
    ostru:rowpos := nstart
endif

while bkeepbrowsing
   dispbegin()
      ostru:forcestable()
   dispend()
#ifdef __CLIPPER__
   while nextkey() == 0
      ol_yield()
   enddo
   okey := inkey()
#else
   okey := inkey(0)
#endif
   do case
   case okey == K_UP
       if nstruel > 1
          ostru:up()
       endif
   case okey == K_DOWN
       if nstruel < len( amenu )
          ostru:down()
       endif
   case okey == K_ESC
      obox:hide()
      nStart := ostru:rowpos
      g_restore()
      return 0
   case okey == K_ENTER
      setcursor( ncursor )
      obox:hide()
      nStart := ostru:rowpos
      g_restore()
      return nstruel
   case navigationkey( ostru, okey, akey )
   endcase
enddo
nStart := ostru:rowpos
obox:hide()
setcursor( ncursor )
return nstruel

********************************************************************************
STATIC FUNCTION SKIPARRAY( nlastele, ncurrele, ntoskip )
********************************************************************************
local nabletoskip := ntoskip
if ntoskip >= 0
  if ncurrele + ntoskip > nlastele
    nabletoskip := nlastele - ncurrele
    ncurrele := nlastele
  else
    ncurrele += ntoskip
  endif
else
  if ncurrele + ntoskip < 1
    nabletoskip := 1 - ncurrele
    ncurrele := 1
  else
    ncurrele += ntoskip
  endif
endif
return nabletoskip

********************************************************************************
static function ac_goto(nnew,ncurrent,otb)
********************************************************************************
local niter
local ndiff := abs(nnew-ncurrent)
dispbegin()
if nnew > ncurrent
  for niter := 1 to ndiff
    otb:down()
    otb:forcestable()
  next
else
  for niter := 1 to ndiff
    otb:up()
    otb:forcestable()
  next
endif
dispend()
return nil

********************************************************************************
STATIC FUNCTION NAVIGATIONKEY( ob, nkey, akey )
********************************************************************************
LOCAL nPos := 0
hb_default( @aKey,{ ;
                  { K_UP,         { || oB:Up() } },       ;
                  { K_DOWN,       { || oB:Down() } },     ;
                  { K_LEFT,       { || oB:Left() } },     ;
                  { K_RIGHT,      { || oB:Right() } },    ;
                  { K_PGUP,       { || oB:PageUp() } },   ;
                  { K_PGDN,       { || oB:PageDown() } }, ;
                  { K_CTRL_LEFT,  { || oB:PanLeft() } },  ;
                  { K_CTRL_RIGHT, { || oB:PanRight() } }, ;
                  { K_CTRL_PGUP,  { || oB:GoTop() } },    ;
                  { K_CTRL_PGDN,  { || oB:GoBottom() } }, ;
                  { K_HOME,       { || oB:Home() } },     ;
                  { K_END,        { || oB:End() } },      ;
                  { K_CTRL_HOME,  { || oB:PanHome() } },  ;
                  { K_CTRL_END,   { || oB:PanEnd() } }    ;
                  })
IF ( nPos := Ascan( aKey, { |pair| nKey == pair[1] } ) ) != 0
   Eval( aKey[ nPos, 2 ] )
ENDIF
RETURN ( nPos != 0 )
