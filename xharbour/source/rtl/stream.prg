#include "hbclass.ch"
#include "fileio.ch"

#define BUFFER_SIZE 16384

FUNCTION FileReader( cFile, nMode )

RETURN TStreamFileReader():New( cFile, nMode )

FUNCTION FileWriter( cFile, nMode )

RETURN TStreamFileWriter():New( cFile, nMode )

CLASS TStream

   DATA lCanRead  INIT .F.
   DATA lCanWrite INIT .F.
   DATA lCanSeek  INIT .F.

   DATA nLength   INIT 0
   DATA nPosition INIT 0

   //METHOD BeginRead( sBuffer, nOffset, nCount, bCallback, oState )
   //METHOD EndRead( oAsync ) VIRTUAL

   //METHOD BeginWrite( sBuffer, nOffset, nCount, bCallback, oState )
   //METHOD EndWrite( oAsync ) VIRTUAL

   METHOD Close() VIRTUAL
   METHOD Flush() VIRTUAL
   METHOD Seek( nOffset, Origin ) VIRTUAL

   METHOD Read( sBuffer, nOffset, nCount ) VIRTUAL
   METHOD ReadByte() VIRTUAL

   METHOD Write( sBuffer, nOffset, nCount ) VIRTUAL
   METHOD WriteByte() VIRTUAL

   METHOD CopyTo( oTargetStream )

ENDCLASS

PROCEDURE CopyTo( oTargetStream ) CLASS TStream

   LOCAL nBytesToRead := ::nLength
   LOCAL sBuffer := Space( BUFFER_SIZE )
   LOCAL nRead
   LOCAL nPosition

   IF oTargetStream:lCanWrite == .F.
      Throw( ErrorNew( "Stream", 1001, ProcName(), "Target not writable.", HB_aParams() ) )
   ENDIF

   // Save.
   nPosition := ::nPosition

   ::Seek( 0, FS_SET )
   oTargetStream:Seek( 0, FS_SET )

   WHILE nBytesToRead > 0
      nRead := ::Read( @sBuffer, 0, BUFFER_SIZE )
      oTargetStream:Write( sBuffer, 0, nRead )
      nBytesToRead -= nRead
   END

   //Truncate incase it was a bigger file.
   oTargetStream:Write( "", 0, 0 )

   // Restore.
   ::Seek( nPosition, FS_SET )

RETURN

CLASS TStreamFileReader FROM TStream

   DATA   cFile
   DATA   Handle

   METHOD New( cFile ) CONSTRUCTOR

   METHOD Close() INLINE IIF( ::Handle > 0, FClose( ::Handle ), ), ::Handle := -2
   METHOD Seek( nOffset Origin ) INLINE FSeek( ::Handle, nOffset, Origin )

   METHOD Read( sBuffer, nOffset, nCount )
   METHOD ReadByte()

   DESTRUCTOR Finalize

ENDCLASS

METHOD New( cFile, nMode ) CLASS TStreamFileReader

   ::lCanRead := .T.

   ::cFile := cFile

   ::Handle := FOpen( cFile, nMode )
   IF ::Handle <= 0
      Throw( ErrorNew( "Stream", 1004, ProcName(), "Open Error: " + Str( FError() ), HB_aParams() ) )
   ENDIF

   ::nPosition := 0
   ::nLength := FSeek( ::Handle, 0, FS_END )

   FSeek( ::Handle, 0, FS_SET )

RETURN Self

PROCEDURE Finalize CLASS TStreamFileReader
   ::Close()
RETURN

METHOD Read( sBuffer, nOffset, nCount ) CLASS TStreamFileReader

   LOCAL nRead

   IF ! HB_IsByRef( @sBuffer )
      Throw( ErrorNew( "Stream", 1002, ProcName(), "Buffer not BYREF.", HB_aParams() ) )
   ENDIF

   nRead := FRead( ::Handle, @sBuffer, nCount, nOffset )

   ::nPosition += nRead

RETURN nRead

METHOD ReadByte()  CLASS TStreamFileReader

   LOCAL sBuffer := " "

   IF FRead( ::Handle, @sBuffer, 1 ) == 1
      ::nPosition++
      RETURN sBuffer
   ENDIF

RETURN ""

CLASS TStreamFileWriter FROM TStream

   DATA   cFile
   DATA   Handle

   METHOD New( cFile ) CONSTRUCTOR

   METHOD Close() INLINE IIF( ::Handle > 0, FClose( ::Handle ), ), ::Handle := -2
   METHOD Seek( nOffset Origin ) INLINE ::nPosition := FSeek( ::Handle, nOffset, Origin )

   METHOD Write( sBuffer, nOffset, nCount )
   METHOD WriteByte( cByte )

   DESTRUCTOR Finalize

ENDCLASS

METHOD New( cFile, nMode ) CLASS TStreamFileWriter

   IF nMode == NIL
      nMode := 2
   ENDIF

   ::lCanWrite := .T.

   ::cFile := cFile

   IF File( cFile )
      ::Handle := FOpen( cFile, nMode )
      IF ::Handle <= 0
         Throw( ErrorNew( "Stream", 1004, ProcName(), "Open Error: " + Str( FError() ), HB_aParams() ) )
      ENDIF

      ::nLength := FSeek( ::Handle, 0, FS_END )
      ::nPosition := ::nLength
   ELSE
      ::Handle := FCreate( cFile, nMode )
      IF ::Handle <= 0
         Throw( ErrorNew( "Stream", 1004, ProcName(), "Create Error: " + Str( FError() ), HB_aParams() ) )
      ENDIF

      ::nPosition := 0
      ::nLength := 0
   ENDIF

RETURN Self

PROCEDURE Finalize CLASS TStreamFileWriter
   ::Close()
RETURN

METHOD Write( sBuffer, nOffset, nCount ) CLASS TStreamFileWriter

   LOCAL nWritten

   nWritten := FWrite( ::Handle, sBuffer, nCount, nOffset )

   ::nPosition += nWritten

   IF nWritten != nCount
      Throw( ErrorNew( "Stream", 1003, ProcName(), "Write failed - written:" + Str( nWritten ) + " bytes", HB_aParams() ) )
   ENDIF

RETURN nWritten

PROCEDURE WriteByte( cByte ) CLASS TStreamFileWriter

   LOCAL nWritten := FWrite( ::Handle, cByte, 1 )

   ::nPosition += nWritten

   IF nWritten != 1
      Throw( ErrorNew( "Stream", 1006, ProcName(), "Write failed", HB_aParams() ) )
   ENDIF

RETURN
