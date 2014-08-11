#include "directry.ch"

STATIC s_cFlags := " /U /R /D"

PROCEDURE Main( cFileMask, cTargetPath, cFlags )

   IF Empty( cFileMask )
      ? "Nothing to copy!"
      RETURN
   ELSEIF cFileMask == "/?" .OR. cFileMask == "-?"
      ? "Copy2Subs <FileMask> [, <TargetPath> [, <XCOPY flags>]]"
      RETURN
   ENDIF
   
   IF ! '\' $ cFileMask
      cFileMask := "\" +  CurDir() + "\" + cFileMask
   ENDIF

   IF ! File( cFileMask )
      Alert( "File not found!;" + cFileMask )
      BREAK
   ENDIF

   IF ! Empty( cFlags )
      s_cFlags := cFlags
   ENDIF
   
   Copy2Subs( cFileMask, cTargetPath )

PROCEDURE Copy2Subs( cFileMask, cTargetPath )

   LOCAL cPresetDir, aFile, aaFiles

   cPresetDir := "\" + CurDir()

   DirChange( cTargetPath )

   TraceLog( "Try:", cFileMask, "\" + CurDir() )
   Update( cFileMask )

   aaFiles := Directory( , "HD" )

   //? "Dir:", cTargetPath, CurDir(), "Files:", Len( aaFiles )

   FOR EACH aFile IN aaFiles
      IF aFile[ F_NAME ] = "."
         LOOP
      ENDIF

      //? aFile[ F_NAME ], aFile[ F_ATTR ]

      IF 'D' $ aFile[ F_ATTR ]
         TraceLog( "Nest:", aFile[ F_NAME ] )
         Main( cFileMask, aFile[ F_NAME ] )
      ENDIF
   NEXT

   DirChange( cPresetDir )

RETURN

STATIC PROCEDURE Update( cSource )

   LOCAL nAt, cSourceDir, cFileMask

   nAt :=  RAt( "\", cSource )
   cSourceDir := Left( cSource, nAt - 1 )
   cFileMask := SubStr( cSource, nAt + 1 )

   IF cSourceDir == "\" + CurDir()
      //? "Avoided same!"
      RETURN
   ENDIF

   IF File( cFileMask )
      TraceLog( CurDir() )
      __Run( "XCOPY " + cSource + s_cFlags )
   ENDIF

RETURN
