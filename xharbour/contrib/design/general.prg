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

#include "inkey.ch"
#include "setcurs.ch"
#xtranslate _dlgcolor => a_dlgcolor\[1\]
#xtranslate _boxcolor => a_dlgcolor\[2\]

static a_dlgcolor := { nil, nil }
static RetScr
static asav := {;
{;
 K_F1,K_F2,K_F3,K_F4,K_F5,K_F6,K_F7,K_F8,K_F9,K_F10,K_F11,K_F12,;
 K_CTRL_F1,K_CTRL_F2,K_CTRL_F3,K_CTRL_F4,K_CTRL_F5,K_CTRL_F6,K_CTRL_F7,K_CTRL_F8,K_CTRL_F9,K_CTRL_F10,K_CTRL_F11,K_CTRL_F12,;
 K_ALT_F1,K_ALT_F2,K_ALT_F3,K_ALT_F4,K_ALT_F5,K_ALT_F6,K_ALT_F7,K_ALT_F8,K_ALT_F9,K_ALT_F10,K_ALT_F11,K_ALT_F12,;
 K_SH_F1,K_SH_F2,K_SH_F3,K_SH_F4,K_SH_F5,K_SH_F6,K_SH_F7,K_SH_F8,K_SH_F9,K_SH_F10,K_SH_F11,K_SH_F12,;
 K_ALT_K,K_ALT_R,K_ALT_A,K_ALT_M,K_ALT_T,K_ALT_B,K_ALT_F,K_ALT_L,K_ALT_O,K_ALT_W,K_ALT_X,K_CTRL_T,K_CTRL_ENTER,K_ALT_ENTER,K_ALT_G,K_ALT_V,K_ALT_S,K_ALT_Y},{}}

********************************************************************************
function pickcolor( cstring, ncolor, csep, lall )
********************************************************************************
Local a_sample := {}
Local i
hb_default( @cSep, "," )
hb_default( @lAll, .F. )
If Empty( cstring )
   Return ""
Endif
a_sample := str2arr( cstring , cSep )
if !lAll
   return a_Sample[ nColor ]
endif
return a_sample

********************************************************************************
function g_save()
********************************************************************************
local k_sav := {}
aeval( asav[1], { |e| aadd( k_sav, setkey( e ) ) } )
aadd( asav[2], k_sav )
return k_sav

********************************************************************************
function g_restore()
********************************************************************************
local k_sav
if !empty( asav[2] )
   k_sav := atail( asav[2] )
   aeval( asav[1], { |e,i| setkey( e, k_sav[i] ) } )
   asize( asav[2], len( asav[2] ) - 1 )
endif
return len( asav[2] )

********************************************************************************
function ___nourut(urut)
********************************************************************************
if urut < 100000
   if urut <= 9
      return "0000" + str( urut, 1 )
   elseif urut >= 10 .and. urut <= 99
      return "000" + str( urut, 2 )
   elseif urut >= 100 .and. urut <= 999
      return "00" + str( urut, 3 )
   elseif urut >= 1000 .and. urut <= 9999
      return "0" + str( urut, 4 )
   elseif urut >= 10000
      return str( urut, 5 )
   endif
endif
return( "*****" )
********************************************************************************
FUNCTION SCRPACK()
********************************************************************************
Return SaveScreen(0,0,MaxRow(),MaxCol())

********************************************************************************
FUNCTION SCRUNPACK( cscr )
********************************************************************************
Return RestScreen(0,0,MaxRow(),MaxCol(),cscr)

********************************************************************************
FUNCTION getfname( cTitle, cMessage, lLayar, Fl, lPesan, bValid, bHelp )
********************************************************************************
hb_default( @Fl, space(50) )
hb_default( @lLayar, .T. )
hb_default( @cTitle, "File Name" )
hb_default( @cMessage, "Enter File Name To Process." )
hb_default( @lPesan, .T. )
hb_default( @bValid, {||.T.} )
if  Fl == ""
   Fl := space(50)
endif
if !lPesan
   cMessage := NIL
endif
if !atail(DSG_whatdir(nil,cTitle,nil,nil,lLayar,.T.,@Fl,cMessage,bValid,lPesan,bHelp))
   return {"",.F.}
endif
return { alltrim( Fl ), .T. }

********************************************************************************
function errorbeep()
********************************************************************************
return tone(100,2)

********************************************************************************
FUNCTION IS_ESCAPE()
********************************************************************************
RETURN LASTKEY() == 27

********************************************************************************
function hb_default( xvar, xdefault )
********************************************************************************
if valtype( xvar ) == valtype( xdefault ) .and. xvar <> nil
   xvar := xvar
else
   xvar := xdefault
endif
return xvar

********************************************************************************
FUNCTION DSG_WHATDIR( aBox, cMesg, cColor, bColor, lClear, lScreen, cDir, cInfo, bValid, lPesan, bHelp )
********************************************************************************
Local oDlg
Local GetName
Local sKey := SetKey( K_F2 )

if bHelp != NIL
   SetKey( K_F2, { || Eval( bHelp, GetName ) } )
endif

//ÄÄÄ default parameters ÄÄÄ//
hb_default( @cDir   , space(50) )
hb_default( @lPesan,  .T. )
hb_default( @lScreen, .T. )
hb_default( @cInfo  , "Complete Path of Directory, Store Data Base Files." )
hb_default( @bValid , { |x| !empty(x)} )
hb_default( @lClear , .F. )
hb_default( @cMesg  , "Directory Name" )
oDlg := Window():New(9,16,15,63)
oDlg:AddPrompt(11,21,cMesg)
oDlg:AddSmallBox({13,21,Space(38),,.F.})
oDlg:lClear := lClear
oDlg:Display()
GetName := GetNew( 13,21,{|p|If(p==NIL,cDir,cDir:=p)},"cDir","@K@S38","N/BG*,N/BG*")
GetName:PostBlock := {||Eval(bValid,cDir)}
GetName:PreBlock  := {||.T.}
GetName:display()
ReadModal( { getname } )
SetCursor(0)
SetKey( K_F2, sKey )
If( lScreen, oDlg:Hide(), )
return { cDir, !Is_Escape() }

//ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ//
function boxcenter(ntop, nleft, nbottom, nright)

local _ntop   := nbottom - ntop  - 1
local _nright := nright  - nleft + 1

ntop    := int((maxrow() - _ntop) / 2) - 1
nbottom := ntop + _ntop + 1
nleft   := int((maxcol() - _nright) / 2)
nright  := nleft + _nright - 1
ntop    += 1
nbottom += 1

return nil

********************************************************************************
function wbox( ctitle,ntop,nleft,nright,nbottom,nheight,nwidth,cwincolor )
********************************************************************************
local obox
hb_default( @cwincolor, "W+/R" )
hb_default( @ctitle,"NO TITLE" )
hb_default( @ntop,    10  )
hb_default( @nleft,   08  )
hb_default( @nright,  14  )
hb_default( @nbottom, 72  )
hb_default( @nheight,  6  )
hb_default( @nwidth,   6  )
obox := basebox():new( ctitle,ntop,nleft,nright,nbottom,nheight,nwidth,cwincolor )
obox:boxcolor    := cwincolor
obox:titlecolor  := "n/w*"
obox:buttoncolor := "w+/n"
return obox

********************************************************************************
function arr2txt( ar, csep, lBlank )

local cret := ""
local i, ctype, t

if lBlank == NIL
   lBlank := .T.
endif

if csep == NIL
   csep := ","
endif

if ( t := len( ar ) ) > 0

   for i := 1 to t
      if ( ctype := valtype( ar[i] ) ) $ "DCNL"
         do case
         case ctype == "C"
            cret += rtrim( ar[i] )
         case ctype == "D"
            cret += dtoc( ar[i] )
         case ctype == "N"
            cret += ltrim(str(ar[i]))
         case ctype == "L"
            cret += if(ar[i],"yes","no")
         endcase
         cret += if( i< t,csep,"" )
      endif
   next

   if !empty( cret )
      cret := alltrim( cret )
      if !lBlank
         while right( cret, 1 ) == csep
            cret := left( cret, rat( csep, cret ) - 1 )
         end
         while left( cret, 1 ) == csep
            cret := subs( cret, 2 )
         end
      endif
   end
endif
return cret

********************************************************************************
function str2arr( csample, csep, lnum, _ltrim )

local aret := {}
local i := 0, cret, nAT

if lnum == NIL
   lnum := .F.
endif

if csep == NIL
   csep := ","
endif

if _ltrim == nil
   _ltrim := .F.
endif

if !empty( csample )
   While ( nAt := At( cSep, cSample ) ) > 0
       cret := left( csample, nAt - 1 )
       cSample := substr( cSample, nAT + 1 )
       if lnum
          aadd( aret, val( cret ) )
       else
          if _ltrim
             aadd( aret, alltrim( cret ) )
          else
             aadd( aret, cret )
          endif
       endif
   enddo
   if lnum
      aadd( aret, val( csample ) )
   else
      if _ltrim
         aadd( aret, alltrim( csample ) )
      else
         aadd( aret, csample )
      endif
   endif
endif

return aret

********************************************************************************
function shadowonly( ntop, nleft, nbottom, nright )
********************************************************************************
local nbottom2

if nright <= maxcol()
   if nbottom >= maxrow()
      nbottom2 := nbottom - 1
   else
      nbottom2 := nbottom
   endif
#ifndef __CLIPPER__
   hb_shadow( ntop, nright, nbottom2, nright )
#else
   ft_shadow( ntop, nright, nbottom2, nright )
#endif
endif
if nbottom <= maxrow()
#ifndef __CLIPPER__
   hb_shadow( nbottom, nleft, nbottom, nright )
#else
   ft_shadow( nbottom, nleft, nbottom, nright )
#endif
endif
return nil

********************************************************************************
FUNCTION AFILLTEXT( cFileName )
********************************************************************************

#ifndef __CLIPPER__
Local aRetValue := {}
hb_fuse( cFileName )
while !hb_feof()
   AADD( aRetValue, hb_freadln() )
   hb_fskip()
enddo
hb_fuse()
#else
Local aRetValue := {}
ft_fuse( cFileName )
while !ft_feof()
   AADD( aRetValue, ft_freadln() )
   ft_fskip()
enddo
ft_fuse()
#endif

Return( aRetValue )

********************************************************************************
function boxshadow( t, l, b, r, scmode, ldiff )
********************************************************************************
local obox
hb_default( @scmode, "W+/B" )
hb_default( @ldiff,  .T. )
obox := basebox():new("",t, l, b, r)
obox:boxcolor := scmode
obox:frame := "ÚÄ¿³ÙÄÀ³ "
if !ldiff
   obox:frame := "³ ³³³ ³³ "
endif
obox:topbar   := .f.
obox:display()
return savescreen( t, l, b + 1, r + 1 )

********************************************************************************
function sbox()
********************************************************************************
retscr := savescreen(11,14,14,66)
boxshadow(11,14,13,65)
return retscr

********************************************************************************
function sboxnew( ctext )
********************************************************************************
retscr := savescreen(11,14,14,66)
boxshadow(11,14,13,65)
dispoutat(12,15,padc(ctext,50),"w+/b")
return retscr

********************************************************************************
function sayinbox( ctext )
********************************************************************************
return dispoutat(12,15,padc(ctext,50),"GR+/B")

********************************************************************************
function un_sbox()
********************************************************************************
restscreen(11,14,14,66,retscr)
return ( retscr := nil )

********************************************************************************
function box3d( nTop, nLeft, nBottom, nRight, aColor )
********************************************************************************
local nVertical
If(Empty(aColor),aColor:={"W+","N","BG"},)
DispBegin()
   dispoutat(nTop,nLeft,"Ú"+repl("Ä",nRight-nLeft-1),aColor[1]+"/"+aColor[3] )
   dispoutat(nTop,nRight,"¿",aColor[2]+"/"+aColor[3] )
   for nVertical := nTop + 1 to nBottom - 1
      dispoutat(nVertical,nLeft,"³",aColor[1]+"/"+aColor[3] )
      dispoutat(nVertical,nRight,"³",aColor[2]+"/"+aColor[3] )
   next
   dispoutat(nBottom,nLeft,"À" ,acolor[1]+"/"+aColor[3] )
   dispoutat(nBottom,nLeft+1,repl("Ä",nRight-nLeft-1)+"Ù",aColor[2]+"/"+aColor[3] )
DispEnd()
return nil

********************************************************************************
function smallbox( top, left, ctext, acolor, lvert )
********************************************************************************
local right   := left + len( ctext )
local nline   := len( ctext )

//             1   2   3  4     5     6    7      8     9
// DialogColor=W+,GR+,BG,N/BG*,N/BG,BG/BG,N/BG*,BG+/BG,N/BG
// DialogColor=W+,BG+,B,N/B*,N/B,B/B,N/BG*,B+/B,N/B

if( lvert == nil, lvert := .t., )
if _boxcolor == NIL
   initboxdlg()
endif
dispoutat(top,left,ctext,_boxcolor[3])

return nil

********************************************************************************
function notopbox( top, left, ctext, acolor )
********************************************************************************
dispoutat(top,left,ctext,_boxcolor[3])
return nil

********************************************************************************
function specialbox( top, left, bottom, right, acolor )
********************************************************************************
local nline := right - left
local n
if _boxcolor == NIL
   initboxdlg()
endif
box3d( top, left, bottom, right, {pickcolor(_dlgcolor[8],1,"/"),"N",pickcolor(_dlgcolor[9],2,"/")} )
return nil

********************************************************************************
function biggetbox( top, left, bottom, right, acolor )
********************************************************************************
local n
if _boxcolor == NIL
   initboxdlg()
endif
for n := top to bottom
   dispoutat(n,left,space(right-left+1),_dlgcolor[7])
next
return nil

********************************************************************************
function initboxdlg()
********************************************************************************
_dlgcolor := str2arr("W+,GR+,BG,N/BG*,N/BG,BG/BG,N/BG*,BG+/BG,N/BG")
_boxcolor := {_dlgcolor[5],_dlgcolor[6],_dlgcolor[7]}
return nil

********************************************************************************
function add_edge()
********************************************************************************
dispoutat(20,00,"Ã","w+/b")
dispoutat(20,79,"´","w+/b")
return .t.
