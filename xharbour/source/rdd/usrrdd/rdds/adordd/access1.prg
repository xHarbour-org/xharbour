#include "adordd.ch"

REQUEST ADORDD

FUNCTION Main()

   USE Test.mdb VIA "ADORDD" TABLE "Tabla1"

   Browse()

   USE

   RETURN nil
