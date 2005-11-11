/*
 * $Id: libnf.prg,v 1.0 2005/11/11 18:56 mac Exp $
 */
/*
   LIBNF.PRG

   Simplest demo program to show usage for libnf

*/

function Main()
  LOCAL aList1,aList2,var0,nstart,nstop,nelapsed,nCtr
  LOCAL aSubTotals := { 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2 }
  local aRet[8], i

  //
  // FT_ADDITION example
  //
  setcolor ('w+/b')
  CLS
  ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AADDITION"
  ? "ออออออออออออออออออออออออออออออออออออออออออออ"
  ?
  aList1 := {"apple", "orange", "pear"}
  aList2 := {"apple ", "banana", "PEAR"}
  ? "aList1 : "
  AEVAL( aList1, { |x| QQOUT(x + ",") } )
  ?
  ? "aList2 : "
  AEVAL( aList2, { |x| QQOUT(x + ",") } )
  ?

  nstart := SECONDS()
  FOR nCtr := 1 to 1000
     var0 := FT_AADDITION( aList1, aList2 )
  NEXT
  nstop := SECONDS()
  nelapsed := nstop - nstart
  ? "time for 1000 merges:", nelapsed
  ?
  ? PADR("FT_AADDITION( aList1, aList2 ) ->",44)
  AEVAL( var0, { |x| QQOUT(x + ",") } )
  ?
  var0 := FT_AADDITION( aList1, aList2, , .F. )
  ? PADR("FT_AADDITION( aList1, aList2, , .F. ) ->",44)
  AEVAL( var0, { |x| QQOUT(x + ",") } )
  ?
  var0 := FT_AADDITION( aList1, aList2, .F., .F. )
  ? PADR("FT_AADDITION( aList1, aList2, .F., .F. ) ->",44)
  AEVAL( var0, { |x| QQOUT(x + ",") } )
  ?

  FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
  inkey(0)

  //
  // FT_ADDITION example
  //
  setcolor ('w+/b')
  CLS
  ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AAVG"
  ? "อออออออออออออออออออออออออออออออออออออออ"
  ?
  ?
  ? "aSubTotals : "
  AEVAL( aSubTotals, { |x| QQOUT( transform( x, "9.99" ) + ",") } )
  ?
  var0 := FT_AAVG( aSubTotals )
  ? PADR("FT_AAVG( aSubTotals ) ->", 44) + transform( var0, "9.99" )
  ?
  var0 := FT_AAVG( aSubTotals, 2, 4 )
  ? PADR("FT_AAVG( aSubTotals, 2, 4 ) ->", 44) + transform( var0, "9.99" )
  ?
  var0 := FT_AAVG( aSubTotals, 5 )
  ? PADR("FT_AAVG( aSubTotals, 5 ) ->", 44) + transform( var0, "9.99" )
  ?
  var0 := FT_AAVG( aSubTotals, , 10 )
  ? PADR("FT_AAVG( aSubTotals, , 10 ) ->", 44) + transform( var0, "9.99" )
  ?

  FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
  inkey(0)

  //
  // FT_CALENDAR example
  //
  setcolor ('w+/b')
  cls
  ? "TEST TO DEMONSTRATE EXAMPLES OF FT_CALENDAR"
  ? "อออออออออออออออออออออออออออออออออออออออออออ"
  ?
  keyboard chr (28)
  aRet := ft_calendar (10,40,'w+/rb',.t.,.t.) //display calendar, return all.
  cls
  ? "TEST TO DEMONSTRATE EXAMPLES OF FT_CALENDAR"
  ? "อออออออออออออออออออออออออออออออออออออออออออ"
  ?
  @  9, 10 SAY 'FT_CALENDAR return values'
  @ 11, 10 SAY 'Date        :'+dtoc(aRet[1])
  @ 12, 10 SAY 'Month Number:'+str(aRet[2],2,0)
  @ 13, 10 SAY 'Day Number  :'+str(aRet[3],2,0)
  @ 14, 10 SAY 'Year Number :'+str(aRet[4],4,0)
  @ 15, 10 SAY 'Month       :'+aRet[5]
  @ 16, 10 SAY 'Day         :'+aRet[6]
  @ 17, 10 SAY 'Julian Day  :'+str(aRet[7],3,0)
  @ 18, 10 SAY 'Current Time:'+aRet[8]
  FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
  inkey(0)

  //
  // FT_DISPFILE example
  //
  setcolor ('w+/b')
  CLS
  ? "TEST TO DEMONSTRATE EXAMPLES OF FT_DISPFILE"
  ? "อออออออออออออออออออออออออออออออออออออออออออ"
  ? "Press aAbB to terminate."
  @ 4,9 TO 21,71
  FT_DFSETUP("libnf.prg", 5, 10, 20, 70, 1, 48, 124, "AaBb" , .f., 5, 132, 4096)
  cKey := FT_DISPFILE()
  FT_DFCLOSE()
  @ 23, 10 SAY "Key that terminated FT_DISPFILE() was: " + '[' + cKey + ']'
  FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
  inkey(0)
RETURN( NIL )


