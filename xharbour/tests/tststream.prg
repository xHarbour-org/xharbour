PROCEDURE CopyFile( cSource, cTarget )

   LOCAL oSourceStream
   LOCAL oTargetStream

   IF ! ( HB_IsString( cSource ) .AND. HB_IsString( cTarget ) )
      Alert( "Syntax: TstStream <srcFile> <targetFile>" )
      BREAK
   ENDIF

   oSourceStream := FileReader( cSource )
   oTargetStream := FileWriter( cTarget )

   oSourceStream:CopyTo( oTargetStream )

RETURN
