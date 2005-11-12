/*
 * $Id: libnf.prg,v 1.0 2005/11/11 18:56 mac Exp $
 */
/*
   LIBNF.PRG

   Simplest demo program to show usage for libnf

*/

#include "directry.ch"
#include "inkey.ch"

* Default heading, column, footer separators
#define DEF_HSEP    "อัอ"
#define DEF_CSEP    " ณ "
#define DEF_FSEP    "อฯอ"

* Default info for tb_methods section
#define KEY_ELEM 1
#define BLK_ELEM 2


function Main()
  LOCAL var0, nstart, nstop, nelapsed, nCtr
  local aRet[8], i
  LOCAL ar[3, 26], aBlocks[3], aHeadings[3], nElem := 1, bGetFunc, cRet

  //
  // First group of routines
  //
  setcolor ('w+/b')
  CLS
  ? "DEMO AND TEST OF ARRAY FUNCTIONS FROM LIBNF"
  ? "อออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ? " FT_AADDITION()   Add elements unique of source array to target array"
  ? " FT_AAVG()        Average numeric values in an array"
  ? " FT_ADESSORT()    Sort an array in descending order"
  ? " FT_AEMAXLEN()    Find longest element within an array"
  ? " FT_AEMINLEN()    Find shortest element within an array"
  ? " FT_AMEDIAN()     Find middle value in array, or average of two middle values"
  ? " FT_ANOMATCHES()  Find the number of array elements meeting a condition"
  ? " FT_AREDIT()      2 dimensional array editing function using TBrowse"
  //? " FT_ASUM()        Sum the elements of an array"
  //? " FT_RESTARR()     Restore a Clipper array from a disc file"
  //? " FT_SAVEARR()     Save Clipper array to a disc file."

  FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, 0.5 )
  inkey(0)
  FT_BLINKW32CANCEL()

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
  release aList, aList2

  FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, 0.5 )
  inkey(0)
  FT_BLINKW32CANCEL()

  //
  // FT_ADDITION example
  //
  aSubTotals := { 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2 }
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
  release aSubTotals

  FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, 0.5 )
  inkey(0)
  FT_BLINKW32CANCEL()

  //
  // FT_ADESSORT example
  //
  aNames := { 'Mary', 'Albert' , 'John', 'Frank', 'Daniel', 'Giuliano'}
  setcolor ('w+/b')
  CLS
  ? "TEST TO DEMONSTRATE EXAMPLES OF FT_ADESSORT"
  ? "อออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ?
  ? "aNames : "
  AEVAL( aNames, { |x| QQOUT( x + ",") } )
  ?
  var0 := FT_ADESSORT( aNames )
  ? PADR("FT_ADESSORT( aNames ) ->", 30)
  AEVAL( var0, { |x| QQOUT(x + ",") } )
  ?
  var0 := FT_ADESSORT( aNames, 3 )
  ? PADR("FT_ADESSORT( aNames, 3 ) ->", 30)
  AEVAL( var0, { |x| QQOUT(x + ",") } )
  ?
  var0 := FT_ADESSORT( aNames, , 3 )
  ? PADR("FT_ADESSORT( aNames, , 3 ) ->", 30)
  AEVAL( var0, { |x| QQOUT(x + ",") } )
  ?
  var0 := FT_ADESSORT( aNames, 2, 5 )
  ? PADR("FT_ADESSORT( aNames, 2, 5 ) ->", 30)
  AEVAL( var0, { |x| QQOUT(x + ",") } )
  ?
  release aNames

  FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, 0.5 )
  inkey(0)
  FT_BLINKW32CANCEL()

  //
  // FT_AEMAXLEN example
  //
  setcolor ('w+/b')
  myarray1 := DIRECTORY()
  CLS
  ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AEMAXLEN"
  ? "อออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ? "myarray1 = DIRECTORY()"
  ?
  //aEval( myarray1, {|v| qout( padr(v[1],12), v[2], v[3], v[4], v[5] ) } )
  //?
  var0 := FT_AEMAXLEN( myarray1 )
  ? PADR('FT_AEMAXLEN( myarray1 ) ->', 30 )
  ?? var0
  ?
  var0 := FT_AEMAXLEN( myarray1, 2 )
  ? PADR('FT_AEMAXLEN( myarray1, 2 ) ->', 30 )
  ?? var0
  ?
  var0 := FT_AEMAXLEN( myarray1, 3 )
  ? PADR('FT_AEMAXLEN( myarray1, 3 ) ->', 30 )
  ?? var0
  ?
  var0 := FT_AEMAXLEN( aTail( myarray1 ) )
  ? PADR('FT_AEMAXLEN( aTail( myarray1 ) ) ->', 30 )
  ?? var0
  ?
  release myarray1

  FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, 0.5 )
  inkey(0)
  FT_BLINKW32CANCEL()

  //
  // FT_AEMINLEN example
  //
  setcolor ('w+/b')
  myarray1 := DIRECTORY()
  CLS
  ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AEMINLEN"
  ? "อออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ? "myarray1 = DIRECTORY()"
  ?
  //aEval( myarray1, {|v| qout( padr(v[1],12), v[2], v[3], v[4], v[5] ) } )
  //?
  var0 := FT_AEMINLEN( myarray1 )
  ? PADR('FT_AEMINLEN( myarray1 ) ->',30)
  ?? var0
  ?
  var0 := FT_AEMINLEN( myarray1,2 )
  ? PADR('FT_AEMINLEN( myarray1,2 ) ->',30)
  ?? var0
  ?
  var0 := FT_AEMINLEN( myarray1[2] )
  ? PADR('FT_AEMINLEN( myarray1[2] ) ->',30)
  ?? var0
  ?
  var0 := FT_AEMINLEN( myarray1,3 )
  ? PADR('FT_AEMINLEN( myarray1,3 ) ->',30)
  ?? var0
  ?
  release myarray1

  FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, 0.5 )
  inkey(0)
  FT_BLINKW32CANCEL()

  //
  // FT_AEMEDIAN example
  //
  setcolor ('w+/b')
  myarray0 := DIRECTORY()
  myarray1 := {}
  CLS
  ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AMEDIAN"
  ? "ออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ?
  ? "myarray0 = DIRECTORY()"
  ?
  //aEval( myarray0, {|v| qout( padr(v[1],12), v[2], v[3], v[4], v[5] ) } )
  //?
  AEVAL( myarray0, { |x| AADD( myarray1, x[ F_SIZE ]) } )
  var0 := FT_AMEDIAN( myarray1 )
  ? PADR('FT_AMEDIAN( myarray1 ) ->',35)
  ?? var0
  ?
  var0 := FT_AMEDIAN( myarray1, 2 )
  ? PADR('FT_AMEDIAN( myarray1, 2 ) ->',35)
  ?? var0
  ?
  var0 := FT_AMEDIAN( myarray1, , 9 )
  ? PADR('FT_AMEDIAN( myarray1, , 9 ) ->',35)
  ?? var0
  ?
  var0 := FT_AMEDIAN( myarray1, 8, 40 )
  ? PADR('FT_AMEDIAN( myarray1, 8, 40 ) ->',35)
  ?? var0
  ?
  release myarray0, myarray1

  FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, 0.5 )
  inkey(0)
  FT_BLINKW32CANCEL()

  //
  // FT_ANOMATCHES example
  //
  setcolor ('w+/b')
  aNames := { 'Mary', 'Albert' , 'John', 'Frank', 'Daniel', 'Giuliano'}
  CLS
  ? "TEST TO DEMONSTRATE EXAMPLES OF FT_ANOMATCHES"
  ? "อออออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ?
  ? "myarray0 = DIRECTORY()"
  ?
  ? "aNames : "
  AEVAL( aNames, { |x| QQOUT( x + ",") } )
  ?
  var0 := FT_ANOMATCHES( aNames, { |x| at( 'a', x ) > 0 } )
  ? PADR('FT_ANOMATCHES( aNames, { |x| at( "a", x ) > 0 } ) ->',60)
  ?? var0
  ?
  var0 := FT_ANOMATCHES( aNames, { |x| at( 'an', x ) > 0 } )
  ? PADR('FT_ANOMATCHES( aNames, { |x| at( "an", x ) > 0 } ) ->',60)
  ?? var0
  ?
  var0 := FT_ANOMATCHES( aNames, { |x| at( 'an', x ) > 0 }, 1, 3 )
  ? PADR('FT_ANOMATCHES( aNames, { |x| at( "an", x ) > 0 }, 1, 3 ) ->',60)
  ?? var0
  ?
  release aNames

  FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, 0.5 )
  inkey(0)
  FT_BLINKW32CANCEL()

  //
  // FT_AREDIT example
  //
  setcolor ('w+/b')
  CLS
  ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AREDIT"
  ? "อออออออออออออออออออออออออออออออออออออออออ"
  * set up 2 dimensional array ar[]
  FOR i = 1 TO 26
    ar[1, i] := i          //  1  ->  26  Numeric
    ar[2, i] := CHR(i+64)  // "A" -> "Z"  Character
    ar[3, i] := CHR(91-i)  // "Z" -> "A"  Character
  NEXT i
  * Set Up aHeadings[] for column headings
  aHeadings  := { "Numbers", "Letters", "Reverse" }
  * Set Up Blocks Describing Individual Elements in Array ar[]
  aBlocks[1] := {|| STR(ar[1, nElem], 2)}  // to prevent default 10 spaces
  aBlocks[2] := {|| ar[2, nElem]}
  aBlocks[3] := {|| ar[3, nElem]}
  * Set up TestGet() as bGetFunc
  bGetFunc   := {|b, ar, nDim, nElem|TestGet(b, ar, nDim, nElem)}
  SET SCOREBOARD OFF
  @ 21,4 SAY "Use Cursor Keys To Move Between Fields, <F7> = Delete Row, <F8> = Add Row"
  @ 22,7 SAY "<ESC> = Quit Array Edit, <Enter> or <Any Other Key> Edits Element"
  SetColor( "N/W, W/N, , , W/N" )
  cRet := FT_ArEdit(3, 5, 18, 75, ar, @nElem, aHeadings, aBlocks, bGetFunc)
  setcolor ('w+/b')
  @ 24, 0
  @ 23, 0
  @ 22, 0
  @ 21, 0
  ? "Return Value   :", cRet
  ? "Lastkey() = ESC:", LASTKEY() == K_ESC

  FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, 0.5 )
  inkey(0)
  FT_BLINKW32CANCEL()

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
  FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, 0.5 )
  inkey(0)
  FT_BLINKW32CANCEL()

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
  FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, 0.5 )
  inkey(0)
  FT_BLINKW32CANCEL()
RETURN( NIL )

FUNCTION TestGet( b, ar, nDim, nElem)
  LOCAL GetList   := {}
  LOCAL nRow      := ROW()
  LOCAL nCol      := COL()
  LOCAL cSaveScrn := SAVESCREEN(21, 0, 22, MaxCol())
  LOCAL cOldColor := SetColor( "W/N")
  @ 21, 0 CLEAR TO 22, MaxCol()
  @ 21,29 SAY "Editing Array Element"
  SetColor(cOldColor)
  DO CASE
    CASE nDim == 1
      @ nRow, nCol GET ar[1, nElem] PICTURE "99"
      READ
      b:refreshAll()
    CASE nDim == 2
      @ nRow, nCol GET ar[2, nElem] PICTURE "!"
      READ
      b:refreshAll()
    CASE nDim == 3
      @ nRow, nCol GET ar[3, nElem] PICTURE "!"
      READ
      b:refreshAll()
  ENDCASE
  RESTSCREEN(21, 0, 22, MaxCol(), cSaveScrn)
  @ nRow, nCol SAY ""
RETURN(.t.)
