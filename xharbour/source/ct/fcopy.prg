/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 FileCopy(), FileCCount(), FileCOpen(), FileCClose(), FileAppend() functions
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
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 *    added FILECDATI()
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
#include "common.ch"
#define  F_BLOCK   512

STATIC nSrchand
STATIC lStillOpen := .F.
STATIC s_lSetDati := .T.

/*
* FileCopy()
* This is a replacement for the CA-tools III function of the
* same name that causes GPF's.
*/

FUNCTION FileCopy( cSource, cDest, lMode )

   LOCAL nDestHand
   LOCAL cBuffer   := Space( F_BLOCK )
   LOCAL lDone     := .F.
   LOCAL nSrcBytes, nDestBytes, nTotBytes := 0

   lStillOpen := .F.
   nSrcHand := FOpen( cSource, FO_READ )

   HB_SYMBOL_UNUSED( lMode )

   IF nSrcHand > 0

      nDestHand := FCreate( cDest )

      IF nDestHand > 0
         DO WHILE ! lDone
            nSrcBytes  := FRead( nSrcHand, @cBuffer, F_BLOCK )
            nDestBytes := FWrite( nDestHand, cBuffer, nSrcBytes )
            IF nDestBytes < nSrcBytes
               lStillOpen := .T.
               lDone      := .T.
            ELSE
               lDone := ( nSrcBytes == 0 )
            ENDIF
            nTotBytes += nDestBytes
         ENDDO

         //    if lStillOpen
         //       fSeek(nSrcHand, nTotBytes, FS_SET)
         //    else
            /* 28/04/2004 - <maurilio.longo@libero.it>
               Since lMode is not supported (fully, at least) if file has been fully copyed into destination
               close source file handle or else it stays open */
         FClose( nSrcHand )
         //    endif
         FClose( nDestHand )

         IF s_lSetDati
           /* Set target date/time same as source date/time (default).
              If you want change to system date/time, call filecdati(.f.)
              before filecopy
            */
            Setfdati( cDest, filedate( cSource ) , filetime( cSource ) )
         ENDIF

      ELSE
         FClose( nSrcHand )
      ENDIF
   ENDIF

   RETURN( nTotBytes )

   /***/

FUNCTION FileCOpen()

   RETURN( lStillOpen )

   /***/

FUNCTION FileCCont( cDest )

   LOCAL nDestHand  := FCreate( cDest )
   LOCAL cBuffer   := Space( F_BLOCK )
   LOCAL lDone     := .F.
   LOCAL nSrcBytes, nDestBytes, nTotBytes := 0

   lStillOpen := .F.

   DO WHILE ! lDone
      nSrcBytes := FRead( nSrcHand, @cBuffer, F_BLOCK )
      nDestBytes := FWrite( nDestHand, cBuffer, nSrcBytes )
      IF nDestBytes < nSrcBytes
         lStillOpen := .T.
         lDone      := .T.
      ELSE
         lDone := ( nSrcBytes == 0 )
      ENDIF
      nTotBytes += nDestBytes
   ENDDO
   IF lStillOpen
      FSeek( nSrcHand, nTotBytes, FS_SET )
   ENDIF
   FClose( nDestHand )

   RETURN( nTotBytes )

FUNCTION FileCClose()

   RETURN( FClose( nSrcHand ) )

   /***/

FUNCTION FileAppend( cSrc, cDest )

   LOCAL cBuffer   := Space( F_BLOCK )
   LOCAL lDone     := .F.
   LOCAL nSrcBytes, nDestBytes, nTotBytes := 0
   LOCAL nSrcHand  := FOpen( cSrc, FO_READ )
   LOCAL nDestHand

   IF ! File( cDest )
      nDestHand := FCreate( cDest )
   ELSE
      nDestHand := FOpen( cDest, FO_WRITE )
      FSeek( nDestHand, 0, FS_END )
   ENDIF

   DO WHILE ! lDone
      nSrcBytes := FRead( nSrcHand, @cBuffer, F_BLOCK )
      nDestBytes := FWrite( nDestHand, cBuffer, nSrcBytes )
      IF nDestBytes < nSrcBytes
         lDone := .T. // error in this case
      ELSE
         lDone := ( nSrcBytes == 0 )
      ENDIF
      nTotBytes += nDestBytes
   ENDDO
   FClose( nSrcHand )
   FClose( nDesthand )

   RETURN( nTotBytes )

   /****/

FUNCTION FileCDati( lNewMode )

   LOCAL lOldMode := s_lSetDati

   IF HB_ISLOGICAL( lNewMode )
      s_lSetDati := lNewMode
   ENDIF

   RETURN lOldMode

// eof: fcopy.prg
