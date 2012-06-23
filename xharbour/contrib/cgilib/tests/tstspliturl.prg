
PROCEDURE Main()
  LOCAL cUrl, hUrl

  AltD()

  ? "Testing SplitUrl() function"
  ?

  cUrl := "http://www.xharbour.org"
  ? cUrl
  hUrl := SplitUrl( cUrl )
  ? ValToPrg( hUrl )
  ?

  cUrl := "http://www.xharbour.org/script.exe"
  ? cUrl
  hUrl := SplitUrl( cUrl )
  ? ValToPrg( hUrl )
  ?

  cUrl := "http://www.xharbour.org/script.exe?module=&var=test"
  ? cUrl
  hUrl := SplitUrl( cUrl )
  ? ValToPrg( hUrl )
  ?

  cUrl := "http://www.xharbour.org/script.exe?module=&var=test#position"
  ? cUrl
  hUrl := SplitUrl( cUrl )
  ? ValToPrg( hUrl )
  ?

  cUrl := "http://www.xharbour.org/?module=&var=test#position"
  ? cUrl
  hUrl := SplitUrl( cUrl )
  ? ValToPrg( hUrl )
  ?

  cUrl := "http://www.xharbour.org/index.htm#position"
  ? cUrl
  hUrl := SplitUrl( cUrl )
  ? ValToPrg( hUrl )
  ?

  cUrl := "http://user:pass@www.xharbour.org/index.htm#position"
  ? cUrl
  hUrl := SplitUrl( cUrl )
  ? ValToPrg( hUrl )
  ?

  cUrl := "http://user:pass@www.xharbour.org:8081/index.htm?varname=test&var2=test2#position"
  ? cUrl
  hUrl := SplitUrl( cUrl )
  ? ValToPrg( hUrl )
  ?


RETURN