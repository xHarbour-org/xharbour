/*
 Made By Sylvain Robert 2004-02-19
 Tested on Windows with Xharbour build .92

*/


FUNCTION Main(cZip,cZipWithPath)

  LOCAL aDir := DIRECTORY( "*.*","D" ), aFiles:= {}
  LOCAL cPath := CURDRIVE()+":\"+CURDIR()+"\"
  LOCAL nZip,nZipmode:=0

  CLS

  IF VALTYPE( cZip ) == "U"
    cZip:="MYTEST.ZIP"
  ENDIF

  IF ( nZip := Zipnew() ) == -1
    ALERT( "Error initializing Zip object" )
    QUIT
  ENDIF

* Tell if we want to have the Fullpath with the file in the Archive
  IF VALTYPE(cZipWithPath) <>"U" .AND. UPPER( cZipWithPath ) == "NO"
    ZipSetFilePath( nZip, .F. )
  ELSE
    ZipSetFilePath( nZip, .T. )
  ENDIF

* IF the archive is to be created on Floppy we set the spanning codeblock

  IF UPPER( LEFT( cZip, 2 ) ) == "A:"
    ZipSetOnDisk( nZip, { |x| ALERT("Please Insert Disk No:"+ HB_VALTOSTR( x ) , .T.) } )
    nZipMode := 1
  ENDIF


/*
 Now there is different option that you can set !

*Set the codeblock when processing files
*nPos is the number of byte processed for the current file
*nTotal is the length in byte of the file currently process
  ZipSetAct( nZip, { |npos,nTotal,cfile| QOUT( npos,nTotal,cfile ) } )

*Set the compress level for the current Zip object
  ZipSetCompressLevel( nZip, 9 )

*Set the password for the current Zip object
  ZipSetPassword( nZip,"mypassword")

*Set a comment for the current Zip object
  ZipSetGlobalComment( nZip," This is my comment ")

*Set the buffer size for Write/Read/Search
  ZipSetBuffer( nZip,81920, 65036,  65036)
*/
? "Before ZipCreate"
  IF ZipCreate( nZip, cZip , nZipMode ) <> 0
    ALERT("Error when creating Zip file: "+HB_VALTOSTR( ZipGetLastError() ) )
    ZipClose( nZip )
    QUIT
  ENDIF

  FillFiles( aFiles, aDir, cPath )

* Adding the file to the archive you also can use ZipSetFiles(), ZipAddFile()
* See Xbuilder getting start manual for more information
? "Adding File"
  IF ZipAddFiles( nZip, aFiles ) <> 0
    ALERT("Error when Adding Files: "+HB_VALTOSTR( ZipGetLastError() ) )
  ENDIF

  ZipClose( nZip )

RETURN NIL

FUNCTION FillFiles( aFiles, cDir, cPath )

  LOCAL aSubDir,cItem

  FOR EACH cItem IN cDir
    IF cItem[5] <> "D"
      AADD( aFiles, cPath+cItem[1] )
    ELSEIF cItem[1] <> "." .AND. cItem[1] <> ".."
      aSubDir := DIRECTORY( cPath+cItem[1]+"\*.*", "D" )
      aFiles:=FillFiles( aFiles ,aSubdir , cPath+cItem[1]+"\" )
    ENDIF
  NEXT

  RETURN aFiles

