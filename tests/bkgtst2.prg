#include "set.ch"
#include "inkey.ch"

#define ATTEMPTS 10000000

PROCEDURE Main()
   LOCAL s, nId4, n, m, n1
   nId4 := HB_BackgroundAdd( { @TimerFunc() }, 1000 )
   ? "Background Tasks defined but not running."

   s := seconds()

   FOR n := 1 TO ATTEMPTS
   NEXT

   n1 := seconds() - s

   ? "Time", n1

   SET BACKGROUND TASKS ON
   /* With this SET you can change the # of pCodes executed
      before checking Tasks. Default is 1000. When you raise this number,
      system saves time checking Tasks, but lowering it you can
      have a more realist multitasking */
   SET BACKGROUNDTICK 10000

   ? "Background Tasks defined and running."

   s := seconds()

   FOR n := 1 TO ATTEMPTS
   NEXT

   m := seconds() - s

   ? "Time", m
   ? "Delay", m-n1
   ? "Delay", (((m-n1)/n1) ) * 100, "%"

RETURN

PROCEDURE TimerFunc()
  ? "Time: " + Time()
RETURN
