/*
 * Harbour Project source code:
 * Standalone Harbour Portable Object file runner
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

#include "hbextern.ch"

EXTERNAL HCHECKBUTTON
EXTERNAL HEDIT
EXTERNAL HRADIOBUTTON
EXTERNAL HBROWSE
EXTERNAL HPANEL
EXTERNAL HCOMBOBOX
EXTERNAL HOWNBUTTON
EXTERNAL HPRINTER

EXTERNAL CHECKMENUITEM
EXTERNAL ISCHECKEDMENUITEM

EXTERNAL COPYSTRINGTOCLIPBOARD
EXTERNAL ENABLEWINDOW
EXTERNAL DESTROYWINDOW
EXTERNAL HIDEWINDOW
EXTERNAL SHOWWINDOW
EXTERNAL ISWINDOWENABLED
EXTERNAL SETWINDOWTEXT

EXTERNAL CREATEENHMETAFILE
EXTERNAL CREATEMETAFILE
EXTERNAL CLOSEENHMETAFILE
EXTERNAL DELETEENHMETAFILE
EXTERNAL PLAYENHMETAFILE

EXTERNAL SELECTFONT
EXTERNAL SELECTFILE
EXTERNAL SAVEFILE
EXTERNAL PRINTSETUP

EXTERNAL RDINI
EXTERNAL RUNSCRIPT

FUNCTION Main( cHRBFile, cPar1, cPar2, cPar3, cPar4, cPar5, cPar6, cPar7, cPar8, cPar9 )
   LOCAL xRetVal

   IF Empty( cHRBFile )
      MsgInfo( "Harbour Runner - HwGUI version" + HB_OSNewLine() +;
              "Copyright 1999-2000, http://www.harbour-project.org" + HB_OSNewLine() +;
              HB_OSNewLine() +;
              "Syntax:  hbrun <hrbfile[.hrb]> [parameters]" + HB_OSNewLine() +;
              HB_OSNewLine() +;
              "Note:  Linked with " + Version() + HB_OSNewLine() )
   ELSE
      xRetVal := __hrbRun( cHRBFile, cPar1, cPar2, cPar3, cPar4, cPar5, cPar6, cPar7, cPar8, cPar9 )
   ENDIF

   RETURN xRetVal

