//
// $Id: testzip.prg,v 1.1 2003/09/12 22:20:56 paultucker Exp $
//

// Requires samples.lib for gauge support
//

#include "common.ch"

proc Main()
Local aFiles, aGauge, nLen, aDir
Local aGaugeFile

   ZipCreate( "TEST.ZIP", "testzip.prg" )

   aFiles := {"testzip.prg",GetEnv("windir")+ "\win.ini"}
   nLen   := Len(afiles)

   ZipCreate( "TEST1.ZIP", aFiles[2] )
   ZipCreate( "TEST2.ZIP", aFiles, 8, {|cFile,nPos| qout("Added " + cFile)})

   // something here may not be clipper compatible
   ?;?;?

   aGauge := GaugeNew( row()-2, 5, row(),74 , "W/B", "W+/B" ,'²')
   GaugeDisplay( aGauge )

   aGaugeFile := GaugeNew( 12, 5, 14,74 , "W/B", "W+/B" ,'²')
   GaugeDisplay( aGaugeFile )


   aDir   := Directory( "*.prg" )
   aFiles := {}
   Aeval( aDir, {|a| aadd( aFiles, a[1]) })
   nLen   := Len(afiles)

   ZipCreate("test3.zip", aFiles, 8, ;
              {|cFile,nPos| GaugeUpdate(aGauge,nPos/nLen) },,'hello',,,{|nPos,nCur| GaugeUpdate(aGaugeFile,nPos/nCur)})

   ? str( nlen ) +" files were added to the zip"

   aFiles :=  hb_GetFilesInZip( "test3.zip" )
   if aFiles != NIL
      ? str( Len( aFiles ) ) + " files are in the zip"
   endif

   ? "TEST1.ZIP has " + iif(hb_ZipWithPassword("TEST1.ZIP"),"a","no" )+ " password"
   ? "test3.zip has " + iif(hb_ZipWithPassword("test3.zip"),"a","no" )+ " password"

function ZipCreate( cFile, uContents, nLevel, bUpdate, lOverwrite, password, lPath, lDrive, bFileUpdate)
   Local lRet

   Default lPath to .t.
   Default lDrive to .F.
   Default bFileUpdate to NIL

   ferase(cFile)

   IF ( lRet := HB_ZIPFILE( cFile, uContents, nLevel, bUpdate, lOverwrite, password ,lPath ,lDrive , bFileUpdate) )
      ? cFile + " was successfully created"
   ENDIF

Return lRet
