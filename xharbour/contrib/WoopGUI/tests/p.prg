
proc main

   local x1 := "prova1"
   local x2 := "prova2"
   local d1 := Date()

   set century on
   set date to italian

   clear
   @ 10, 5 get x1 pict "@!"
   @ 14, 5 get d1 pict "@K"
   @ 16, 5 get x2 pict "AAAAA9"

   read

return