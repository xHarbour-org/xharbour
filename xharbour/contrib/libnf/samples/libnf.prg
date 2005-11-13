/*
 * $Id: libnf.prg,v 1.0 2005/11/11 18:56 mac Exp $
 */
/*
   LIBNF.PRG

   Simplest demo program to show usage for libnf
   
   NOTE: for xHarbour define is_xharbour and undefine it for clipper

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

STATIC nWait := 0.2

function Main()
  LOCAL var0, nstart, nstop, nelapsed, nCtr
  local aRet[8], i
  LOCAL ar[3, 26], aBlocks[3], aHeadings[3], nElem := 1, bGetFunc, cRet
  //
  // Local task
  //
  #ifdef is_xharbour
    REQUEST HB_LANG_ES
	  HB_LANGSELECT("ES")
    MyLanguage := HB_LANGSELECT()
    //ALERT( "Language ID: " + MyLanguage )
  #endif

  //
  // Array group of routines
  //
  setcolor ('w+/b')
  CLS
  ? "DEMO AND TEST OF ARRAY FUNCTIONS FROM LIBNF"
  ? "อออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ? "FT_AADDITION()   Add elements unique of source array to target array"
  ? "FT_AAVG()        Average numeric values in an array"
  ? "FT_ADESSORT()    Sort an array in descending order"
  ? "FT_AEMAXLEN()    Find longest element within an array"
  ? "FT_AEMINLEN()    Find shortest element within an array"
  ? "FT_AMEDIAN()     Find middle value in array, or average of two middle values"
  ? "FT_ANOMATCHES()  Find the number of array elements meeting a condition"
  ? "FT_AREDIT()      2 dimensional array editing function using TBrowse"
  ? "FT_ASUM()        Sum the elements of an array"
  ? "FT_RESTARR()     Restore a Clipper array from a disc file"
  ? "FT_SAVEARR()     Save Clipper array to a disc file."

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

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

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // FT_AAVG example
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

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // FT_ADESSORT example
  //
  setcolor ('w+/b')
  CLS
  ? "TEST TO DEMONSTRATE EXAMPLES OF FT_ADESSORT"
  ? "อออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ?
  ? "aNames : "
  aNames := { 'Mary', 'Albert' , 'John', 'Frank', 'Daniel', 'Giuliano'}
  AEVAL( aNames, { |x| QQOUT( x + ",") } )
  ?
  aNames := { 'Mary', 'Albert' , 'John', 'Frank', 'Daniel', 'Giuliano'}
  var0 := FT_ADESSORT( aNames )
  ? PADR("FT_ADESSORT( aNames ) ->", 30)
  AEVAL( var0, { |x| QQOUT(x + ",") } )
  ?
  aNames := { 'Mary', 'Albert' , 'John', 'Frank', 'Daniel', 'Giuliano'}
  var0 := FT_ADESSORT( aNames, 3 )
  ? PADR("FT_ADESSORT( aNames, 3 ) ->", 30)
  AEVAL( var0, { |x| QQOUT(x + ",") } )
  ?
  aNames := { 'Mary', 'Albert' , 'John', 'Frank', 'Daniel', 'Giuliano'}
  var0 := FT_ADESSORT( aNames, , 3 )
  ? PADR("FT_ADESSORT( aNames, , 3 ) ->", 30)
  AEVAL( var0, { |x| QQOUT(x + ",") } )
  ?
  aNames := { 'Mary', 'Albert' , 'John', 'Frank', 'Daniel', 'Giuliano'}
  var0 := FT_ADESSORT( aNames, 2, 5 )
  ? PADR("FT_ADESSORT( aNames, 2, 5 ) ->", 30)
  AEVAL( var0, { |x| QQOUT(x + ",") } )
  ?
  release aNames

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

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

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

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

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

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

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

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

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

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

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // FT_ASUM example
  //
  aSubTotals := { 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2 }
  setcolor ('w+/b')
  CLS
  ? "TEST TO DEMONSTRATE EXAMPLES OF FT_ASUM"
  ? "อออออออออออออออออออออออออออออออออออออออ"
  ?
  ?
  ? "aSubTotals : "
  AEVAL( aSubTotals, { |x| QQOUT( transform( x, "999.99" ) + ",") } )
  ?
  var0 := FT_ASUM( aSubTotals )
  ? PADR("FT_ASUM( aSubTotals ) ->", 44) + transform( var0, "999.99" )
  ?
  var0 := FT_ASUM( aSubTotals, 2, 4 )
  ? PADR("FT_ASUM( aSubTotals, 2, 4 ) ->", 44) + transform( var0, "999.99" )
  ?
  var0 := FT_ASUM( aSubTotals, 5 )
  ? PADR("FT_ASUM( aSubTotals, 5 ) ->", 44) + transform( var0, "999.99" )
  ?
  var0 := FT_ASUM( aSubTotals, , 10 )
  ? PADR("FT_ASUM( aSubTotals, , 10 ) ->", 44) + transform( var0, "999.99" )
  ?
  release aSubTotals

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // FT_RESTARR AND FT_SAVEARR example
  //
  aArray := { {'Invoice 1', CTOD('04/15/91'), 1234.32, .T.},;
              {'Invoice 2', DATE(), 234.98, .F.},;
              {'Invoice 3', DATE() + 1, 0, .T.}  }
  nErrorCode := 0
  setcolor ('w+/b')
  cls
  ? "TEST TO DEMONSTRATE EXAMPLES OF FT_RESTARR AND FT_SAVEARR"
  ? "อออออออออออออออออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ? "Saving array ..."
  ?
  FT_SAVEARR(aArray,'INVOICE.DAT',@nErrorCode)
  IF nErrorCode = 0
    DispArray(aArray)
    aSave := FT_RESTARR('INVOICE.DAT',@nErrorCode)
    IF nErrorCode = 0
      ?
      ? "Restoring array from disk ..."
      ?
      DispArray(aSave)
    ELSE
      ? 'Error restoring array'
    ENDIF
  ELSE
    ? 'Error writing array'
  ENDIF
  release aArray, aSave, nErrorCode

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // Conversion group of routines
  //
  setcolor ('w+/b')
  CLS
  ? "DEMO AND TEST OF CONVERSION FUNCTIONS FROM LIBNF"
  ? "ออออออออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ? "FT_BYT2BIT()     Convert byte to string of 1's and 0's"
  ? "FT_BYT2HEX()     Convert byte to hexadecimal version of its binary value"
  ? "FT_D2E()         Convert decimal to scientific notation"
  ? "FT_DEC2BIN()     Convert decimal to binary"
  ? "FT_E2D()         Convert scientific notation string to a decimal"
  ? "FT_ESCCODE()     Convert Lotus style escape codes"
  ? "FT_HEX2DEC()     Convert a hex number to decimal"
  ? "FT_INVCLR()      Get the inverse of a color"
  ? "FT_NTOW()        Translate numeric value to words"
  ? "FT_SQZN()        Compress a numeric value into a character string"
  ? "FT_STOD()        Convert a date string to a Clipper date data type"
  ? "FT_UNSQZN()      Uncompress a numeric compressed by FT_SQZN()"
  ? "FT_XTOY()        Convert from any data type to any other data type"

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // FT_BYT2BIT() example
  //
  setcolor ('w+/b')
  CLS
  ? "TEST TO DEMONSTRATE EXAMPLES OF FT_BYT2BIT()"
  ? "ออออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ?
  var0 := FT_BYT2BIT( 'a' )
  ? PADR("FT_BYT2BIT( 'a' ) ->", 44) + var0
  ?
  var0 := FT_BYT2BIT( 'mac' )
  ? PADR("FT_BYT2BIT( 'mac' )", 44) + var0
  ?
  var0 := FT_BYT2BIT( 'A' )
  ? PADR("FT_BYT2BIT( 'A' ) ->", 44) + var0
  ?
  var0 := FT_BYT2BIT( 'C' )
  ? PADR("FT_BYT2BIT( 'C' ) ->", 44) + var0
  ?

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // DOS / BIOS group of routines
  //
  setcolor ('w+/b')
  CLS
  ? "DEMO AND TEST OF DOS/BIOS FUNCTIONS FROM LIBNF"
  ? "ออออออออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ? "FT_CHDIR()       Change the current directory"
  ? "FT_DEFAULT()     Retrieve and optionally change the current default drive"
  ? "FT_DOSVER        Return the current DOS major and minor version as a string"
  ? "FT_DSKFREE()     Return the amount of available disk space"
  ? "FT_DSKSIZE()     Return the maximum capacity of a fixed disk"
  ? "FT_IAMIDLE()     Inform the operating system that the application is idle."
  ? "FT_INP()         Retrieve a byte from a specified I/O port"
  ? "FT_INT86()       Execute a software interrupt"
  ? "FT_ISPRINT()     Check printer status"
  ? "FT_ISSHARE()     Determine if DOS Share is installed"
  ? "FT_MKDIR()       Create a subdirectory"
  ? "FT_OUTP()        Write a byte to a specified I/O port"
  ? "FT_PEEK()        Retrieve a byte from a specified memory location."
  ? "FT_POKE()        Write a byte to a specified memory location"
  ? "FT_REBOOT()      Force a warm or cold boot"
  ? "FT_RMDIR()       Delete a subdirectory"
  ? "FT_SETDATE()     Set the DOS system date"
  ? "FT_SETTIME()     Set the DOS system time"
  ? "FT_SYSMEM()      Determine the amount of conventional memory installed"
  ? "FT_TEMPFIL()     Create a file with a unique name"

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // DATE / TIME group of routines
  //
  setcolor ('w+/b')
  CLS
  ? "DEMO AND TEST OF DATE / TIME FUNCTIONS FROM LIBNF"
  ? "อออออออออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ? "FT_ACCTADJ()     Adjust beginning or ending fiscal pd. dates to acctg. dates"
  ? "FT_ACCTMONTH()   Return accounting month data"
  ? "FT_ACCTQTR()     Return accounting quarter data"
  ? "FT_ACCTWEEK()    Return accounting week data"
  ? "FT_ACCTYEAR()    Return accounting year data"
  ? "FT_ADDWKDY()     Return true number of days to add given number of workdays"
  ? "FT_CALENDAR()    Display date/time calendar, find a date, return calendar data."
  ? "FT_CIV2MIL()     Convert usual civilian format time to military time."
  ? "FT_DATECNFG()    Set beginning of year/week for FT_ date functions"
  ? "FT_DAYOFYR()     Return calendar, fiscal or accounting day data"
  ? "FT_DAYTOBOW()    Calculate no. of days between date and beginning of week"
  ? "FT_DOY()         Find number of day within year"
  ? "FT_EASTER()      Return the date of Easter"
  ? "FT_ELAPMIN()     Return difference, in minutes, between two mil format times."
  ? "FT_ELAPSED()     Return elapsed time between two days and/or times"
  ? "FT_ELTIME()      Compute difference between times in hours, minutes, seconds."
  ? "FT_FDAY()        Return first day of the month"
  ? "FT_LDAY()        Return last day of the month"

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // DATE / TIME group of routines
  //
  setcolor ('w+/b')
  CLS
  ? "DEMO AND TEST OF DATE / TIME FUNCTIONS FROM LIBNF"
  ? "อออออออออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ? "FT_MADD()        Add or subtract months to/from a date"
  ? "FT_MIL2CIV()     Convert time in military format to civilian format."
  ? "FT_MIL2MIN()     Convert time in military format to number of minute of day."
  ? "FT_MIN2DHM()     Convert numeric minutes to days, hours and minutes."
  ? "FT_MIN2MIL()     Convert minute of day to military format time."
  ? "FT_MONTH()       Return Calendar or Fiscal Month Data"
  ? "FT_QTR()         Return Calendar or Fiscal Quarter Data."
  ? "FT_SYS2MIL()     Convert system time to military time format."
  ? "FT_WEEK()        Return calendar or fiscal week data"
  ? "FT_WORKDAYS()    Return number of work days between two dates"
  ? "FT_WOY()         Find number of week within year"
  ? "FT_YEAR()        Return calendar or fiscal year data"

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

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

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // ENVIRONMENT group of routines
  //
  setcolor ('w+/b')
  CLS
  ? "DEMO AND TEST OF ENVIRONMENT FUNCTIONS FROM LIBNF"
  ? "อออออออออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ? "FT_GETE()        Return the entire current environment"
  ? "FT_LINKED()      Determine if a function was linked in"
  ? "FT_ORIGIN()      Report the drive, path and filename of the current program"
  ? "FT_RESTSETS()    Restore status of all SET command settings"
  ? "FT_SAVESETS()    Save the status of all the SET command settings"
  ? "FT_SETCENTURY()  Check/Set the CENTURY Setting"
  ? "FT_TREE()        Locate all directories and subdirectories on a drive"
  ? "FT_WHEREIS()     Locate all occurrences of a filespec on a drive"

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // EVENT group of routines
  //
  setcolor ('w+/b')
  CLS
  ? "DEMO AND TEST OF EVENT FUNCTIONS FROM LIBNF"
  ? "อออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ? "FT_IDLE()       Generate an idle event to allow incremental garbage collection."
  ? "FT_ONIDLE()     Evaluate a designated code block during idle states."
  ? "FT_ONTICK()     Evaluate a designated code block at a designated interval."

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // FILE I/O group of routines
  //
  setcolor ('w+/b')
  CLS
  ? "DEMO AND TEST OF FILE I/O FUNCTIONS FROM LIBNF"
  ? "ออออออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ? "FT_DFCLOSE()     Close file displayed by FT_DISPFILE()"
  ? "FT_DFSETUP()     Set up parameters for FT_DISPFILE()"
  ? "FT_DISPFILE()    Browse a text file"

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

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

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // GAME group of routines
  //
  setcolor ('w+/b')
  CLS
  ? "DEMO AND TEST OF GAME FUNCTIONS FROM LIBNF"
  ? "ออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ? " FT_PEGS()        FT_PEGS GAME (all work and no play...)"

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // KEYBOARD / MOUSE group of routines
  //
  setcolor ('w+/b')
  CLS
  ? "DEMO AND TEST OF KEYBOARD / MOUSE FUNCTIONS FROM LIBNF"
  ? "ออออออออออออออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ? "FT_ALT()         Determine status of the Alt key"
  ? "FT_CAPLOCK()     Determine and optionally change the status of CapLock key"
  ? "FT_CTRL()        Determine status of the Ctrl key"
  ? "FT_LASTKEY()     Force LastKey() to return a programmer-defined value."
  ? "FT_MBUTPRS()     Retrieve button press status"
  ? "FT_MBUTREL()     Get mouse button release information"
  ? "FT_MCONOFF()     Turn mouse cursur off if in specified region"
  ? "FT_MCURSOR()     Set the mouse cursor"
  ? "FT_MDBLCLK()     Return true if a double click was detected"
  ? "FT_MDEFCRS()     Define the mouse cursor"
  ? "FT_MGETCOORD()   Get mouse cursor position (text coord.) and button status"
  ? "FT_MGETPAGE()    Get the display page for the mouse pointer"
  ? "FT_MGETPOS()     Get mouse cursor position and button status"
  ? "FT_MGETSENS()    Get the mouse sensitivity parameters"
  ? "FT_MGETX()       Get mouse cursor row position"
  ? "FT_MGETY()       Get mouse cursor column position"
  ? "FT_MHIDECRS()    Decrement internal mouse cursor flag and hide mouse cursor"
  ? "FT_MINIT()       Initialize the mouse driver, vars and return status of mouse"
  ? "FT_MINREGION()   Test if the mouse cursor is in the passed region"

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // KEYBOARD / MOUSE group of routines
  //
  setcolor ('w+/b')
  CLS
  ? "DEMO AND TEST OF KEYBOARD / MOUSE FUNCTIONS FROM LIBNF"
  ? "ออออออออออออออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ? "FT_MMICKEYS()    Get mickeys"
  ? "FT_MRESET()      Reset mouse driver and return status of mouse"
  ? "FT_MSETCOORD()   Position the mouse cursor using text screen coordinates"
  ? "FT_MSETPAGE()    Set the display page for the mouse pointer"
  ? "FT_MSETPOS()     Position the mouse cursor using virtual screen coordinates"
  ? "FT_MSETSENS()    Set the mouse sensitivity parameters"
  ? "FT_MSHOWCRS()    Increment internal cursor flag and display mouse cursor"
  ? "FT_MVERSION()    Get the mouse driver version"
  ? "FT_MXLIMIT()     Set vertical bounds of mouse using virtual screen coord."
  ? "FT_MYLIMIT()     Set horiz. bounds of mouse using virtual screen coordinates"
  ? "FT_NUMLOCK()     Return status of NumLock key"
  ? "FT_PRTSCR()      Enable or disable the Print Screen key"
  ? "FT_PUTKEY()      Stuff a keystroke into the keyboard buffer"
  ? "FT_SCANCODE()    Wait for keypress and return keyboard scan code"
  ? "FT_SETKEYS()     Get array of keys redirected via the SetKey() or SET KEY"
  ? "FT_SETRATE()     Set the keyboard delay and repeat rate on PC/AT & PS/2"
  ? "FT_SHIFT()       Determine status of shift key"
  ? "FT_SINKEY()      Replacement for INKEY() that tests for SET KEY procedures"

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // MATH group of routines
  //
  setcolor ('w+/b')
  CLS
  ? "DEMO AND TEST OF MATH FUNCTIONS FROM LIBNF"
  ? "ออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ? "FT_GCD()         Calculate greatest common divisor of two numbers"
  ? "FT_NETPV()       Calculate net present value"
  ? "FT_RAND1()       Generate a random number"
  ? "FT_ROUND()       Rounds a number to a specific place"

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // MENU / PROMPTS group of routines
  //
  setcolor ('w+/b')
  CLS
  ? "DEMO AND TEST OF MENU / PROMPTS FUNCTIONS FROM LIBNF"
  ? "ออออออออออออออออออออออออออออออออออออออออออออออออออออ"
  ?
  ? "FT_ADDER()       Pop up a simple calculator"
  ? "FT_BLINK()       Display a blinking message on the screen"
  ? "FT_BRWSWHL()     Browse an indexed database limited to a while condition"
  ? "FT_CLRSEL()      User Selectable Colour Routine"
  ? "FT_DISPMSG()     Display a message and optionally waits for a keypress"
  ? "FT_FILL()        Declare menu options for FT_MENU1()"
  ? "FT_MENU1()       Pulldown menu system"
  ? "FT_MENU2()       Vertical lightbar menu"
  ? "FT_MENUTO()      Execute light bar menu using prompts created with @...PROMPT"
  ? "FT_PENDING()     Display same-line pending messages after a wait."
  ? "FT_PICKDAY()     Picklist of days of week"
  ? "FT_PROMPT()      Define a menu item for use with FT_MenuTo()"
  ? "FT_SLEEP         Wait for a specified amount of time"
  ? "FT_XBOX()        Display a self-sizing message box and message"

  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

  //
  // FT_ADDER example
  //
  setcolor ('w+/b')
  CLS
  ? "TEST TO DEMONSTRATE EXAMPLES OF FT_ADDER"
  ? "อออออออออออออออออออออออออออออออออออออออออออ"
  ? 
  nSickHrs := 0
  nPersHrs := 0
  nVacaHrs := 0
  GetList  := {}
  SET SCOREBOARD OFF
  //_ftSetScrColor( STD_SCREEN, STD_VARIABLE)
  setcolor ('w+/b')
  CLS
  
  SET KEY K_ALT_A  TO FT_Adder        // Make <ALT-A> call FT_Adder

  * SIMPLE Sample of program data entry!

  @ 12,5 SAY 'Please enter the total Sick, Personal, and Vacation hours.'
  @ 15,22 SAY 'Sick hrs.'
  @ 15,40 SAY 'Pers. hrs.'
  @ 15,60 SAY 'Vaca. hrs.'
  @ 23,20 SAY 'Press <ALT-A> to Pop - Up the Adder.'
  @ 24,20 SAY 'Press <ESC> to Quit the adder Demo.'
  DO WHILE .T.                               // Get the sick, personal, & vaca
    @ 16,24 GET nSickHrs PICTURE '9999.999'  // Normally I have a VALID()
    @ 16,43 GET nPersHrs PICTURE '9999.999'  // to make sure the value is
    @ 16,63 GET nVacaHrs PICTURE '9999.999'  // within the allowable range.
    SET CURSOR ON                            // But, like I said it is a
    CLEAR TYPEAHEAD                          // SIMPLE example <g>.
    READ
    SET CURSOR OFF
    IF LASTKEY() == K_ESC                    // <ESC> - ABORT
      CLEAR TYPEAHEAD
      EXIT
    ENDIF
  ENDDO
  SET CURSOR ON
  SET KEY K_ALT_A                     // Reset <ALT-A>
  
  #ifdef is_xharbour
    FT_BLINKW32( "[ Hit a key to continue ]", 24, 0, nwait )
    inkey(0)
    FT_BLINKW32CANCEL()
  #else
    FT_BLINK( "[ Hit a key to continue ]", 24, 0 )
    inkey(0)
  #endif

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

 FUNCTION DispArray(aTest)
   LOCAL nk
   FOR nk := 1 TO LEN(aTest)
     ? aTest[nk, 1]
     ?? '  '
     ?? DTOC(aTest[nk, 2])
     ?? '  '
     ?? STR(aTest[nk, 3])
     ?? '  '
     ?? IF(aTest[nk, 4], 'true', 'false')
   NEXT
 RETURN Nil
