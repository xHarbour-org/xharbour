//
// $Id: testzip.prg,v 1.2 2003/02/04 23:43:44 ronpinkas Exp $
//

// Requires samples.lib for gauge support
//

proc Main()
Local aFiles, aGauge, nLen, aDir

   ZipCreate( "TEST.ZIP", "testzip.prg" )

   aFiles := {"testzip.prg",GetEnv("windir")+ "\win.ini"}
   nLen   := Len(afiles)

   ZipCreate( "TEST1.ZIP", aFiles[2] )
   ZipCreate( "TEST2.ZIP", aFiles, 8, {|cFile,nPos| qout("Added " + cFile)})

   // something here may not be clipper compatible
   ?;?;?

   aGauge := GaugeNew( row()-2, 5, row(),74 , "W/B", "W+/B" ,'²')
   GaugeDisplay( aGauge )

   aDir   := Directory( "*.prg" )
   aFiles := {}
   Aeval( aDir, {|a| aadd( aFiles, a[1]) })
   nLen   := Len(afiles)

   ZipCreate("test3.zip", aFiles, 8, ;
              {|cFile,nPos| GaugeUpdate(aGauge,nPos/nLen) },,'hello')

   ? str( nlen ) +" files were added to the zip"

   aFiles :=  hb_GetFilesInZip( "test3.zip" )
   ? str( Len( aFiles ) ) + " files are in the zip"


   ? "TEST1.ZIP has " + iif(hb_ZipWithPassword("TEST1.ZIP"),"a","no" )+ " password"
   ? "test3.zip has " + iif(hb_ZipWithPassword("test3.zip"),"a","no" )+ " password"

function ZipCreate( cFile, uContents, nLevel, bUpdate, lOverwrite, password)
   Local lRet

   ferase(cFile)

   IF ( lRet := HB_ZIPFILE( cFile, uContents, nLevel, bUpdate, lOverwrite, password ) )
      ? cFile + " was successfully created"
   ENDIF

Return lRet
