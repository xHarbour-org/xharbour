Procedure Main()

   LOCAL cVar := "Hello", cVar2 := "World", GetList := {}, Counter
   LOCAL oGet, xProperty
   LOCAL nGet, nGets, nProperty, nProperties
   LOCAL nStart

   CLS

   @ 10,10 GET cVar
   @ 12,10 GET cVar2

#ifdef __XHARBOUR__
   nStart := Seconds()

   FOR Counter := 1 TO 10000
      FOR EACH oGet IN GetList
         FOR EACH xProperty IN oGet
         NEXT
      NEXT
   NEXT

   ? Seconds() - nStart
#endif

   nStart := Seconds()

   FOR Counter := 1 TO 10000
      nGets := Len( GetList )
      FOR nGet := 1 TO nGets
         oGet := GetList[ nGet ]

         nProperties := Len( oGet )
         FOR nProperty := 1 TO nProperties
            xProperty := oGet[nProperty]
         NEXT
      NEXT
   NEXT

   ? Seconds() - nStart

RETURN
