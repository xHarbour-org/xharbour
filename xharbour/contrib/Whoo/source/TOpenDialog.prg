/*
 * $Id: TOpenDialog.prg,v 1.2 2002/11/01 08:15:25 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TOpenDialog CLASS
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
#include "commdlg.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "debug.ch"

*------------------------------------------------------------------------------*

CLASS TOpenDialog FROM TCommonDialog

   DATA DefaultExt
   DATA FileEditStyle
   DATA FileName
   DATA Files         INIT {}
   DATA Filter
   DATA FilterIndex
   DATA HistoryList
   DATA InitialDir
   DATA Options       INIT OFN_EXPLORER + OFN_FILEMUSTEXIST
   DATA OptionsEx
   DATA Title

   METHOD Execute()
   METHOD Create( oParent ) INLINE ::Parent := oParent, super:Create()
ENDCLASS

METHOD Execute() CLASS TOpenDialog
   local cPath, x, n, c

   ::Filename := GetOpenFileName( GetActiveWindow(), ::Title, ::Filter, ::Options, ::InitialDir, ::DefaultExt, ::FilterIndex )
   
   IF ValType( ::FileName ) == "A"
      ::Files    := ::FileName
      ::FileName := NIL
   ENDIF

RETURN( self )

//           aFilter = { {"All Files (*.*)","*.*"} }
//           cFile:=GetOpenFileName( ::handle, space(255), "Send FTP File", aFilter, OFN_EXPLORER+OFN_FILEMUSTEXIST, "" ,)
