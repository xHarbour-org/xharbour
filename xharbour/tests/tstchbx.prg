/*
 * $Id: tstchbx.prg,v 1.3 2004/02/15 19:11:55 jonnymind Exp $
 */

#include "inkey.ch"

function Main
   Local lx :=.f.
   local ly :=.f.
   Local citem:="Windows NT/2000"
   Local aitems[4]
   LOCAL GetList := {}

   SET EVENTMASK TO INKEY_ALL

   aitems[1]:=RadioButton( 3,3,"&Windows NT/2000")
   aitems[2]:=RadioButton( 4,3,"W&indows 9x")
   aitems[3]:=RadioButton( 5,3,"&Linux")
   aitems[4]:=RadioButton( 6,3,"&Mac OS")

   cls
   Setcolor('w/b+,r/b,g+/r,b+/r+,bg/n+,w/bg,rb/bg')

   @ 2,2,7,40 GET citem radiogroup aitems color 'w/b+,r/b,g/b+' MESSAGE "Select Your Os"
   @ 8, 3  SAY "Married"
   @ 8, 12 GET lx CHECKBOX color 'w/b+,w/b,w+/r,w/g+' MESSAGE "Are You Married?"
   @ 9, 3  SAY "Singer"
   @ 9, 12 Get ly CHECKBOX color 'w/b+,w/b,w+/r,w/g+' MESSAGE "Are You a Singer"

   READ MSG AT maxrow(), 0, maxcol() MSG Color "w/b+"

   ? "Is the Person Married",if(lx," Yes ", " No ")
   ? "Is the Person a Singer",if(ly," Yes ", " No ")
   ? "Your Os is ",cItem

return Nil
