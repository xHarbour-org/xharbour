PROCEDURE MAIN( cAddress )
   LOCAL aHosts
   LOCAL cName

   CLEAR SCREEN

   InetInit()

   IF cAddress == NIL
      cAddress := "www.altavista.com"
   ENDIF

   ? "Scanning IP hosts for " + cAddress
   aHosts := InetGetHosts( cAddress )

   IF aHosts == NIL
      ? "Host not found"
      RETURN
   ELSE
      ? "List of IP address for " + cAddress
      FOR EACH cName IN aHosts
         ? cName
      NEXT
   ENDIF

   ? "-----"
   ? "List of name aliases for " + cAddress
   aHosts := InetGetAlias( cAddress )
   IF aHosts == NIL
      ? "Host not found"
   ELSE
      FOR EACH cName IN aHosts
         ? cName
      NEXT
   ENDIF

   ?
   ? "Please, press a key"
   INKEY( 0 )
   InetCleanup()

RETURN
