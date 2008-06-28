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
