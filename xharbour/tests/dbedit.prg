/*
* New enhanced DBEdit() test
*
* See how aCols[1] is an array of 2 items
* The first is, as usual, the column data
* The second is the codeblock that returns a pair of {std,enh} colors
* just like in tbrowse :-)
*
* NOTE: - Color codeblock takes a parameter (actual data)
*         Iïve ignored (used SALARY instead) it here to demostrate how flexible a TBRowse is.
*       - Try moving columns with K_CTRL_UP & K_CTRL_DOWN
*
* Is DBEdit() deprecated? NO!
* Enjoy
* Mauricio <maurifull@datafull.com>
* Comments, suggestions, bugs report welcome
*
*/
#include "inkey.ch"
#include "dbedit.ch"

Function Main()
Local aCols := {{"PadR(AllTrim(FIRST)+' '+AllTrim(LAST),30)", {|x| IIf(SALARY<10000, {3,2}, IIf(SALARY<100000,{1,2},{4,5}))}},;
                "CITY",;
                "SALARY"}

  Use test
  DBEdit(,,,, aCols, "TstFnc",,{"Name","City","Salary"})
  Close
  Cls
  ? "Have a nice day ;)"
Return Nil

Function TstFnc(nMode, nCol, oTBR)
Local nRet := DE_CONT

  If nMode == -1 // EXTENSION: Initialization mode
    oTBR:colorSpec := "n/bg,w/n,r/bg,w+/bg,w+/gr"
  ElseIf LastKey() == K_ESC .Or. LastKey() == K_ENTER
    nRet := DE_ABORT
  End
Return nRet
