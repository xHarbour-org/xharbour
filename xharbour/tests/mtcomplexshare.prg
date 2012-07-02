*
* Complex example of Multi thread usage
*
* Giancarlo Niccolai
* $Id$
*
* Here we have a main thread counting, and some secondary
* threads counting too (in different fashons).
* A control thread is notified when each secondary
* thread finishes, and some of the secondary threads
* are killed before they are able to reach the end.
*
* Added atomic or volatile increment and decrement of counters of complex
* types and structures. Now is safe to share complex data type in diferent
* threads if C compiler or OS support function or create ASM thread-safe to
* inc/dec.
* NOTE: Atomic increment and decrement need counter memory aligned and 32 bits
* of lenght.

Static lEnd := .f.
Static nGlobalCounter := 0
Static aResult[2]

Function Main()
Local mtxCounter
cls

mtxCounter := HB_MutexCreate()

StartThread(@paralell(),1,mtxCounter)
StartThread(@paralell(),2,mtxCounter)
DevOut("Press any key or or wait 5 seconds to exit",,1,0)
inkey(5)
lEnd := .t.
WaitForThreads()
cls
@2,0 say "Thread 1 - LocalCounter="+str(aResult[1])
@3,0 say "Thread 2 - LocalCounter="+str(aResult[2])
@4,0 say "Total                   "+str(aResult[1]+aResult[2])
@6,0 say "          GlobalCounter="+str(nGlobalCounter)
@8,0 say "Difference              "+str(aResult[1]+aResult[2]-nGlobalCounter)

Return NIL

Procedure paralell( nId, mtxCounter )
Local nLocalCounter := 0
do while !lEnd
  nLocalCounter++
  HB_MutexLock(mtxCounter)
  nGlobalCounter++
  HB_MutexUnlock(mtxCounter)
enddo
aResult[nId] := nLocalCounter
Return
