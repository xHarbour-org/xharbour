/*
* Program..: fcopy.prg
* Author...: Frederic J. Bell
* Dated....: Jun,17 94
* Revised..: Sep,20 94
* Purpose..: Replaces the following ca-tools functions which generate GPF's
*            FileCopy(), FileCOpen() & FileAppend()!
* Relies on: Clipper (can you believe it!)
* Compile..: /n /m /w /[/p /b /l] /es2
* Notes....:
* No copyright - released into the public domain NSA.
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
nDestHand := fCreate(cDest)

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
if lStillOpen
   fSeek(nSrcHand, nTotBytes, FS_SET)
endif
fClose(nDestHand)
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
