/*
 * $Id: TStrings.prg,v 1.5 2002/11/05 21:39:58 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TStrings CLASS
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

#include "winuser.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "wintypes.ch"
#include "cstruct.ch"
#include "debug.ch"

*------------------------------------------------------------------------------*

CLASS TStrings FROM TPersistent

   DATA Capacity       PROTECTED  
   DATA CommaText      PROTECTED  
   DATA Count          PROTECTED  
   DATA DelimitedText  PROTECTED  
   DATA Delimiter      PROTECTED  
   DATA Names          PROTECTED  
   DATA QuoteChar      PROTECTED  
   DATA Strings        EXPORTED INIT {}
   DATA StringsAdapter PROTECTED  
   DATA UpdateCount    PROTECTED  
   DATA Values         PROTECTED  
   DATA Text AS ARRAY  PROTECTED

   METHOD Add( c )  INLINE AADD( ::Strings, c )

//   METHOD AddObject
//   METHOD AddStrings
//   METHOD Append
//   METHOD Assign
//   METHOD BeginUpdate
//   METHOD Clear
//   METHOD CompareStrings
//   METHOD DefineProperties
//   METHOD Delete
//   METHOD Destroy
//   METHOD EndUpdate
//   METHOD Equals
//   METHOD Error
//   METHOD Exchange
//   METHOD ExtractName
//   METHOD Get

//   METHOD GetCapacity
//   METHOD GetCount
//   METHOD GetObject
//   METHOD GetText
//   METHOD GetTextStr
//   METHOD IndexOf
//   METHOD IndexOfName
//   METHOD IndexOfObject
//   METHOD Insert
//   METHOD InsertObject
//   METHOD LoadFromFile
//   METHOD LoadFromStream
//   METHOD Move
//   METHOD Put
//   METHOD PutObject
//   METHOD SaveToFile

//   METHOD SaveToStream
//   METHOD SetCapacity
//   METHOD SetText
//   METHOD SetTextStr
//   METHOD SetUpdateState

ENDCLASS

