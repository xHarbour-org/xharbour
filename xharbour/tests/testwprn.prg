#include 'set.ch'
function main
local c
local aprinter:=getprinters()

? SET(_SET_DEVICE)
set(24, "WIN:"+aprinter[1,1])
set device to printer
set console off

set printer on
for c:=1 to 120  
qout(c)
    if c==60
        eject
    endif
next

set printer off
set console on
set device to screen
set printer to
? SET(_SET_DEVICE)
return nil
