#include "hbclass.ch"
#include "directry.ch"

CLASS TUpdate FROM HBPersistent

   PUBLISHED:
   DATA cFileName
   DATA nFileSize
   DATA dDate
   DATA cTime

   DATA Version
   DATA Level
   DATA TargetFolder  INIT ""
   DATA InstallScript

   PUBLIC:
   METHOD New( aFile ) CONSTRUCTOR

ENDCLASS

METHOD New( aFile ) CLASS TUpdate

   LOCAL aaUpdate

   IF ! File( aFile[F_NAME ] )
       THROW( ErrorNew( ) )
   ENDIF

   WITH OBJECT Self
      :cFileName := aFile[ F_NAME ]
      :nFileSize := aFile[ F_SIZE ]
      :dDate     := aFile[ F_DATE ]
      :cTime     := aFile[ F_TIME ]

      IF File( aFile[ F_NAME ] + ".upd" )
         aaUpdate := HB_ReadIni( aFile[ F_NAME ] + ".upd", .F. ):Main

         :Version       := aaUpdate:Version
         :Level         := aaUpdate:Level
         :TargetFolder  := aaUpdate:TargetFolder
         :InstallScript := aaUpdate:InstallScript

         TraceLog( :Version, :Level, :TargetFolder, :InstallScript )
      ENDIF
   END

RETURN Self
