#include "vt.ch"

PROCEDURE Backup( cFile )

   LOCAL objStream
   LOCAL aStream

   IF Empty( cFile )
      cFile := ProcFile()
   ENDIF

   IF File( cFile + ".bak" )
      Alert( "Backup file already exists, for: " + cFile )
      RETURN
   ENDIF

   objStream := CreateObject( "ADODB.Stream" )
   objStream:Open()
   objStream:Type := 1//adTypeBinary
   objStream:LoadFromFile( cFile )
   aStream := objStream:Read()
   objStream:Close()

   objStream:Open()
   objStream:Type := 1//adTypeBinary
   objStream:Write( aStream )
   objStream:SaveToFile( cFile + ".bak" )
   objStream:Close()

   Alert( "Backup completed, for: " + cFile )

RETURN
