/*
 * $Id: xTree.prg,v 1.4 2002/10/10 02:51:46 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TBrowser CLASS wrapper to whBrowse in WhatPlus.lib
 *
 * Copyright 2002 Augusto Infante [augusto@2vias.com.ar]
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
 */

#include "windows.ch"
#include "hbclass.ch"
#include "what32.ch"
#include "wingdi.ch"
#include "debug.ch"

CLASS TCBrowser FROM whBrowse
   DATA Parent AS OBJECT
   DATA Left   INIT   0
   DATA Top    INIT   0
   DATA Width  INIT 100
   DATA Height INIT 100
   DATA Header AS ARRAY
   DATA Info   AS ARRAY
   METHOD New() CONSTRUCTOR
   METHOD Create()
ENDCLASS

*-----------------------------------------------------------------------------*
   
METHOD New( oParent, nL, nT, nW, nH, aHeader, aInfo ) CLASS TCBrowser
   ::Parent     := oParent
   ::Left       := IFNIL( nL, ::Left,   nL )
   ::Top        := IFNIL( nT, ::Top,    nT )
   ::Width      := IFNIL( nW, ::Width,  nW )
   ::Height     := IFNIL( nH, ::Height, nH )

   ::Header     := aHeader
   ::Info       := aInfo
   
return( super:New( if( valtype( ::Info[1] )=="A", ::Info, ALIAS() ) ) )


METHOD Create() CLASS TCBrowser
   local oCol, aCols, c,i, a
   super:Create( ::Parent:handle,  ::Left, ::Top, ::Width, ::Height, WS_CHILD+WS_VISIBLE+WS_VSCROLL, WS_EX_CLIENTEDGE )

   for i:=1 to len( ::Header )
       oCol:=GetAColumn(::header,i)
       oCol:VertAlign :=TA_CENTER
       oCol:bSaveBlock:= ::Header[i][3]
       ::addColumn(oCol)
   next
   
   ::HeadFont      := GetMessageFont()
   ::Font          := GetMessageFont()
   ::bKillBlock    := {|| DeleteObject(::Font),DeleteObject(::HeadFont)}

   ::configure()

   return( self )


*-----------------------------------------------------------------------------*

FUNCTION GetAColumn(a,i)
 RETURN whColumn():INIT(a[i][1],{|oCol,oB,n| asString(oB:source[n,i]) } ,DT_LEFT, a[i][2] )

