/*
 * $Id: ttextlin.prg,v 1.3 2004/06/06 23:43:13 modalsist Exp $
 */

/*
 * Harbour Project source code:
 * HBTextLine Class (used by HBEditor class, see teditor.prg )
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
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

/*
 * v.1.2 - 2004-05-11 Eduardo Fernandes <eduardo@modalsistemas.com.br>
 *
 *  - Revision to proper working with tab columns.
 *    See teditor.prg and memoedit.prg to more details.
 *
 * v.1.3 - 2004-06-03 Eduardo Fernandes <eduardo@modalsistemas.com.br>
 *
 *  - Added # include "common.ch" to better DATA vars initialization in
 *    New() Method.
 */
 
#include "common.ch"
#include "hbclass.ch"

CLASS HBTextLine

   DATA cText       // Text line
   DATA lSoftCR     // 3 State:  .T.   if text line end with SoftCR at Word Wrap Column (See HBEditor class in teditor.prg).
                    //           .F.   if text line end with HardCR 
                    //           .NIL. if text line end with null char
   DATA aTabCol     // array to save/restore tab columns

   METHOD New( cLine, lEOL , aTab )

ENDCLASS

// Create a new line of text
METHOD New( cLine, lEOL, aTab ) CLASS HBTextLine

   default cLine to ""
   default lEOL to nil
   default aTab to {}
   
   ::cText   := cLine
   ::lSoftCR := lEOL
   ::aTabCol := aTab

RETURN Self

