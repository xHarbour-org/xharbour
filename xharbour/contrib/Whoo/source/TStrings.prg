/*
 * $Id: TCListBox.prg,v 1.14 2002/10/22 17:24:20 what32 Exp $
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

#include "windows.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "wintypes.ch"
#include "cstruct.ch"
#include "debug.ch"

*------------------------------------------------------------------------------*

CLASS TStrings FROM TPersistent

   DATA Capacity       EXPORTED  
   DATA CommaText      EXPORTED  
   DATA Count          EXPORTED  
   DATA DelimitedText  EXPORTED  
   DATA Delimiter      EXPORTED  
   DATA Names          EXPORTED  
   DATA Objects        EXPORTED  
   DATA QuoteChar      EXPORTED  
   DATA Strings        EXPORTED  
   DATA StringsAdapter EXPORTED  
   DATA Text           EXPORTED  
   DATA UpdateCount    EXPORTED  
   DATA Values         EXPORTED  
     
   METHOD New() CONSTRUCTOR
/*
   METHOD Add
   METHOD AddObject
   METHOD AddStrings
   METHOD Append
   METHOD Assign
   METHOD BeginUpdate
   METHOD Clear
   METHOD CompareStrings
   METHOD DefineProperties
   METHOD Delete
   METHOD Destroy
   METHOD EndUpdate
   METHOD Equals
   METHOD Error
   METHOD Exchange
   METHOD ExtractName
   METHOD Get

   METHOD GetCapacity
   METHOD GetCount
   METHOD GetObject
   METHOD GetText
   METHOD GetTextStr
   METHOD IndexOf
   METHOD IndexOfName
   METHOD IndexOfObject
   METHOD Insert
   METHOD InsertObject
   METHOD LoadFromFile
   METHOD LoadFromStream
   METHOD Move
   METHOD Put
   METHOD PutObject
   METHOD SaveToFile

   METHOD SaveToStream
   METHOD SetCapacity
   METHOD SetText
   METHOD SetTextStr
   METHOD SetUpdateState
*/
ENDCLASS

*------------------------------------------------------------------------------*

METHOD New() CLASS TStrings
   RETURN( self )

*------------------------------------------------------------------------------*

