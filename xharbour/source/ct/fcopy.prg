/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 FileCopy(), FileCOpen(), FileCClose(), FileAppend() functions
 *
 *    Program..: fcopy.prg
 *    Author...: Frederic J. Bell
 *    Dated....: Jun,17 94
 *    Revised..: Sep,20 94
 *    Purpose..: Replaces the following ca-tools functions which generate GPF's
 *               FileCopy(), FileCOpen() & FileAppend()!
 *    Relies on: Clipper (can you believe it!)
 *    Compile..: /n /m /w /[/p /b /l] /es2
 *    Notes....:
 *    No copyright - released into the public domain NSA.
 *
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



#include "fileio.ch"
#define  F_BLOCK   512

Static nSrchand
Static lStillOpen := .F.

/*
* FileCopy()
* This is a replacement for the CA-tools III function of the
* same name that causes GPF's.
*/
Function FileCopy(cSource, cDest, lMode)

  Local nDestHand
  Local cBuffer   := Space(F_BLOCK)
  Local lDone     := .F.
  Local nSrcBytes, nDestBytes, nTotBytes := 0

  lStillOpen := .F.
  nSrcHand := fOpen(cSource, FO_READ)

  IF nSrcHand > 0

   nDestHand := fCreate(cDest)

     IF nDestHand > 0
         Do while ! lDone
            nSrcBytes  := fRead(nSrcHand, @cBuffer, F_BLOCK)
            nDestBytes := fWrite(nDestHand, cBuffer, nSrcBytes)
            if nDestBytes < nSrcBytes
               lStillOpen := .T.
               lDone      := .T.
            else
               lDone := (nSrcBytes == 0)
            endif
            nTotBytes += nDestBytes
      Enddo

   //    if lStillOpen
   //       fSeek(nSrcHand, nTotBytes, FS_SET)
   //    else
            /* 28/04/2004 - <maurilio.longo@libero.it>
               Since lMode is not supported (fully, at least) if file has been fully copyed into destination
               close source file handle or else it stays open */
            fClose(nSrcHand)
   //    endif
         fClose(nDestHand)
    ELSE
      fClose(nSrcHand)
     ENDIF
   endif
Return(nTotBytes)

/***/
Function FileCOpen()
Return(lStillOpen)

/***/
Function FileCCont(cDest)

Local nDestHand  := fCreate(cDest)
Local cBuffer   := Space(F_BLOCK)
Local lDone     := .F.
Local nSrcBytes, nDestBytes, nTotBytes := 0

lStillOpen := .F.

Do while ! lDone
   nSrcBytes := fRead(nSrcHand, @cBuffer, F_BLOCK)
   nDestBytes := fWrite(nDestHand, cBuffer, nSrcBytes)
   if nDestBytes < nSrcBytes
      lStillOpen := .T.
      lDone      := .T.
   else
      lDone := (nSrcBytes == 0)
   endif
   nTotBytes += nDestBytes
Enddo
if lStillOpen
   fSeek(nSrcHand, nTotBytes, FS_SET)
endif
fClose(nDestHand)
Return(nTotBytes)

Function FileCClose()
Return(fClose(nSrcHand))

/***/
Function FileAppend(cSrc, cDest)

Local cBuffer   := Space(F_BLOCK)
Local lDone     := .F.
Local nSrcBytes, nDestBytes, nTotBytes := 0
Local nSrcHand  := fOpen(cSrc, FO_READ)
Local nDestHand

if ! file(cDest)
   nDestHand := fCreate(cDest)
else
   nDestHand := fOpen(cDest, FO_WRITE)
   fSeek(cDest, 0, FS_END)
endif

Do while ! lDone
   nSrcBytes := fRead(nSrcHand, @cBuffer, F_BLOCK)
   nDestBytes := fWrite(nDestHand, cBuffer, nSrcBytes)
   if nDestBytes < nSrcBytes
      lDone := .T. // error in this case
   else
      lDone := (nSrcBytes == 0)
   endif
   nTotBytes += nDestBytes
Enddo
fClose(nSrcHand)
fClose(nDesthand)
Return(nTotBytes)

// eof: fcopy.prg
