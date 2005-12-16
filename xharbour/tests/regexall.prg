* regexall.prg
* $Id: regex.prg,v 1.4 2003/05/27 20:05:54 jonnymind Exp $
* Test for regular expression functions using hb_RegExAll() function
*
* (C) 2005 - Francesco Saverio Giudice
*

FUNCTION Main()

   LOCAL Start := 1, Len, nLenght
   LOCAL lCaseSensitive := .f.
   LOCAL cFound
   LOCAL cString := "/C=IT/O=xHarbour/OU=www.xharbour.com/CN=GIUDICE_FRANCESCO_SAVERIO/email=info@fsgiudice.com"

   // I want to get from above string field names and values.
   LOCAL cRegExString  := "(?:\/(\w+)=)([\w.@]+)"
   LOCAL cRegEx  := HB_RegExComp( cRegExString )

   ? "X H A R B O U R - Regular expression scan tests using hb_RegExAll()"
   ?
   ? "String = ", cString
   ? "Regex  = ", cRegExString
   ?

   ? "Getting All Strings, Fields and Values"
   ? ValToPrg( HB_RegExAll( cRegEx, cString ) )
   ?
   ? "Getting All Strings, Fields and Values + positions"
   ? ValToPrg( HB_RegExAll( cRegEx, cString,,,,,.F. ) )
   ?
   wait
   ?
   ? "Getting Only All Fields names"
   ? ValToPrg( HB_RegExAll( cRegEx, cString,,,,2 ) )
   ?
   ? "Getting Only All Fields names + positions"
   ? ValToPrg( HB_RegExAll( cRegEx, cString,,,,2,.F. ) )
   ?
   wait
   ?
   ? "Getting Only All Fields values"
   ? ValToPrg( HB_RegExAll( cRegEx, cString,,,,3 ) )
   ?
   ? "Getting Only All Fields values + positions"
   ? ValToPrg( HB_RegExAll( cRegEx, cString,,,,3,.F. ) )
   ?
   wait
   ?
   ? "Getting first two strings"
   ? ValToPrg( HB_RegExAll( cRegEx, cString,,,2,1 ) )
   ?
   wait

RETURN
