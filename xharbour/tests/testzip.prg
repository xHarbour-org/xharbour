//
// $Id: testzip.prg,v 1.5 2003/09/14 22:09:10 lculik Exp $
//

// Requires samples.lib for gauge support
//

#include "common.ch"
#include "setcurs.ch"

proc Main()
Local aFiles, aGauge, nLen, aDir
Local aSaveFiles
Local aGaugeFile

   // This is the 'safe' thing to do until Alert() is given focus.
   SetMode( 50,80 )

   ZipCreate( "TEST.ZIP", "testzip.prg" )

   aFiles := {"testzip.prg",GetEnv("windir")+ "\win.ini"}
   nLen   := Len(afiles)

   ZipCreate( "TEST1.ZIP", aFiles[2] )
   ZipCreate( "TEST2.ZIP", aFiles, 8, {|cFile,nPos| qout("Added " + cFile)})

   // something here is not clipper compatible
   // (These can be removed after the box drawing is corrected)
   ?;?;?
   ?
   ?;?;?

   aGauge := GaugeNew( row()-6, 5, row()-4,74 , "W/B", "W+/B" ,'²')
   GaugeDisplay( aGauge )

   aGaugeFile := GaugeNew( row()+2, 5, row()+4,74 , "W/B", "W+/B" ,'²')
   GaugeDisplay( aGaugeFile )

   aFiles := {}

   aDir   := Directory( "*.prg" )
   Aeval( aDir, {|a| aadd( aFiles, a[1]) })

   /* lets add an new bigger files on this example*/
   aDir   := Directory( "*.map" )
   Aeval( aDir, {|a| aadd( aFiles, a[1]) })

   aDir   := Directory( "*.c" )
   Aeval( aDir, {|a| aadd( aFiles, a[1]) })
   nLen   := Len(afiles)

   // Lets save aFile  Array for later usage
   aSaveFiles := aFiles

   set cursor off

   ZipCreate("test3.zip", aFiles, 8, ;
              {|cFile,nPos| GaugeUpdate(aGauge,nPos/nLen) },,'hello',,,;
              {|nPos,nCur| GaugeUpdate(aGaugeFile,nPos/nCur)})

   set cursor on

   ? str( nlen ) +" files were added to the zip"

   // method 1
   aFiles :=  hb_GetFilesInZip( "test3.zip" )
   if aFiles != NIL
      ? str( Len( aFiles ) ) + " files are in the zip"
   endif
   // or simpler, method 2
   ? str( hb_GetFileCount("test3.zip" ) ) + " files using alternate method"

   ZipHasPassword( "TEST1.ZIP" )
   ZipHasPassword( "test3.zip" )

   //ok, now we create an file on an floppy
   ? "Put a formatted Floppy/Zip disk in drive A: and press any key - (ESC) to skip"
   ? "Existing files will not be deleted!"
   Inkey( 0 )
   if lastkey() != 27

      Cls
      // no need for all the qout's after a cls.

      aGauge := GaugeNew( row(), 5, row()+2,74 , "W/B", "W+/B" ,'²')
      GaugeDisplay( aGauge )
      ?;?
      aGaugeFile := GaugeNew( row(), 5, row()+2,74 , "W/B", "W+/B" ,'²')
      GaugeDisplay( aGaugeFile )

      HB_SETDISKZIP( { | x |  Alert( "Please insert disk no " + Str( x , 3 ) ) } )
 
      set cursor off

      ZipCreateToFloppy("test4.zip", aSaveFiles, 9, ;
                 {|cFile,nPos| GaugeUpdate(aGauge,nPos/nLen) },,'hello',,,;
                 {|nPos,nCur| GaugeUpdate(aGaugeFile,nPos/nCur)})

      set cursor on

   endif

function ZipCreate(cFile, uContents, nLevel, bUpdate, lOverwrite, password,;
                   lPath, lDrive, bFileUpdate)
   Local lRet

   Default lOverwrite to .t.
   Default lPath to .t.

   IF ( lRet := HB_ZIPFILE( cFile, uContents, nLevel, bUpdate, lOverwrite,;
                            password, lPath, lDrive, bFileUpdate) )
      ? cFile + " was successfully created"
   ENDIF

Return lRet

function ZipCreateToFloppy(cFile, uContents, nLevel, bUpdate, lOverwrite, password,;
                   lPath, lDrive, bFileUpdate)
   Local lRet

   Default lOverwrite to .t.
   Default lPath to .t.

   IF ( lRet :=  HB_ZIPFILEBYPKSPAN ( "a:\" + cFile, uContents, nLevel, bUpdate, lOverwrite,;
                            password, lPath, lDrive, bFileUpdate) )
      ? cFile + " was successfully created"
   ENDIF

Return lRet


Function ZipHasPassword( cFile )
   Local lRet

   ? cFile + " has " + iif(lRet := hb_ZipWithPassword(cFile),"a","no" )+ " password"

Return lRet
