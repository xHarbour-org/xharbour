/*
 Made by Sylvain Robert 2004-01-23
 Tested under Windows with Xharbour Build .92
  test

*/

Function Main(cZip,cZipWithPath)

  LOCAL cCurdir, cZipDir, nError := 0,nZip,nZipMode := 0

  CLS
  cCurdir:=cZipdir:=CURDRIVE()+":\"+CURDIR()+"\"

  IF VALTYPE( cZip ) == "U"
    cZip:="MYTEST.ZIP"
  ENDIF

  IF (nZip:=ZipNew()) == -1
    ALERT("Error while initialising Zip Object")
    QUIT
  ENDIF

? "Zip handle Object: "+STR(nzip)

  IF UPPER( LEFT( cZip ,2 ) ) == "A:"
    ? "ZipSetOndisk"
    ZipSetOnDisk( nZip , { |x| ALERT("Please Insert Disk No:"+ HB_VALTOSTR( x ) )  } )
    nZipMode := 1

? "ZipTestPk"

    Alert(" Please insert last disk of the backup set" )
    DO WHILE ( nError :=Zip_TestPk( cZip ) ) <> -1 // -1 pkzip spanning mode !
      IF Alert("Error: "+HB_VALTOSTR( nError )+" ! Try Again ?",{"YES","NO"}) == 2
        ZipClose( nZip )
        QUIT
      ENDIF
    ENDDO
  ENDIF

? "ZipSetAct"
  ZipSetAct( nZip,{|nFile,nPos| Qout( nFile,nPos ) })

? "Opening: "+ cZip
  IF ZipOpen( nZip , cZip,nZipMode ) <> 0
    ALERT("Error on ZipOpen: "+ HB_VALTOSTR( ZipGetLastError() ) )
  ENDIF

? "ZipSetFilePath"

  IF VALTYPE( cZipWithPath ) <> "U" .AND. UPPER( cZipWithPath ) == "NO"
    ZipSetFilePath( nZip, .F. )
  ELSE
    ZipSetFilePath( nZip, .T. )
  ENDIF

? "ZipSetExtractPath"
  ZipSetExtractPath( nZip, cCurDir )

? "Begin extract"
  IF ZipExtractFiles( nZip ) == -1
    ALERT( "Error when uncompressing file"+HB_VALTOSTR( ZipGetLastError() ) )
  ENDIF

  ZipClose( nZip )

  RETURN NIL

