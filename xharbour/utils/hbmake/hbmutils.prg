
#include "common.ch"
#ifndef __HARBOUR__
    #include 'hbclip.ch'
#else
    DECLARE ExtenPrg( cExt AS STRING, nType AS NUMERIC ) AS STRING
    DECLARE Exten( cExt as string, nType as numeric ) as string
    DECLARE GetSourceFiles( lSubDir as logical ) as ARRAY
    DECLARE GetDirs( cPat as USUAL ) as Array
    DECLARE GetBccDir() as String
    DECLARE GetVccDir() as String
    DECLARE GetMakeDir() as String
    DECLARE HB_ARGV( n as numeric ) as string
    DECLARE HbMake_FileDate( c as String ) as string

#endif

FUNCTION GetSourceFiles( lSubDir, lGcc, cOs )

   LOCAL aDirs
   LOCAL aRet      := {}
   LOCAL lLinux    := AT( 'linux', LOWER( cOs ) ) > 0 .OR. lGcc
   LOCAL cDir      := IIF( ! lLinux, '\' + CURDIR() + '\', '/' + CURDIR() + '/' )
   LOCAL aStru     := { cDir }
   LOCAL aData
   LOCAL nCounter  := 0
   LOCAL nArrayLen
   LOCAL nDatalen
   LOCAL y
   LOCAL cItem
   LOCAL cExt
   LOCAL cPath
   LOCAL cDrive
   LOCAL nPos
   LOCAL xItem
   LOCAL nLen
   LOCAL cFile
   DEFAULT lSubDir TO .t.
   WHILE ++ nCounter <= LEN( aStru )

      IF ! EMPTY( aDirs := GetDirs( aStru[ nCounter ], lGcc ) )                  // There are elements!
         AEVAL( aDirs, { | xItem | AADD( aStru, xItem ) } )
      ENDIF

   ENDDO

   aDirs := {}

   ASORT( aStru )
   nArrayLen := LEN( aStru )

   FOR nCounter := 1 TO nArrayLen

      IF LEN( aData := DIR_MULTI( aStru[ nCounter ] + "*.prg |" + aStru[ nCounter ] + "*.c |" + aStru[ nCounter ] + "*.cpp" ) ) != 0
         nDataLen := LEN( aData )

         FOR y := 1 TO nDataLen
            IF AT( '.PRG', UPPER( aData[ y, 1 ] ) ) > 0 .OR. AT( '.C', UPPER( aData[ y, 1 ] ) ) > 0 .OR. AT( '.CPP', UPPER( aData[ y, 1 ] ) ) > 0

               IF lSubDir

                  nLen := AT( " ", aData[ y, 1 ] ) + 1

                  AADD( aRet, STRTRAN( aStru[ nCounter ], cDir, '' ) + aData[ y, 1 ] + ;
                        STR( aData[ y, 2 ], 8 ) + '  ' + ;
                        DTOC( aData[ y, 3 ] ) + '  ' + ;
                        aData[ y, 4 ] )

               ELSEIF ! lSubDir .AND. AT( IIF( lLinux, "/", "\" ), STRTRAN( aStru[ nCounter ], cDir, '' ) ) == 0

                  AADD( aRet, aData[ y, 1 ] + ;
                        STR( aData[ y, 2 ], 8 ) + '  ' + ;
                        DTOC( aData[ y, 3 ] ) + '  ' + ;
                        aData[ y, 4 ] )

               ENDIF

            ENDIF

         NEXT

      ENDIF

   NEXT

   //     For nCounter := 1 To Len( aRet )
   FOR EACH cFile IN aRet

      xItem := SUBSTR( cFile, RAT( IIF( lLinux, "/", '\' ), cFile ) + 1 )
      nPos  := ASCAN( aStru, { | x | x := SUBSTR( x, RAT( IIF( lLinux, "/", '\' ), x ) + 1 ), LEFT( x, AT( ".", x ) ) == LEFT( xitem, AT( ".", xitem ) ) } )

      IF nPos > 0
         ADEL( aStru, nPos )
         ASIZE( aStru, LEN( aStru ) - 1 )
      ENDIF

   NEXT

   FOR EACH cFile IN aStru

      HB_FNAMESPLIT( LEFT( cFile, AT( ' ', cFile ) - 1 ), @cPath, @cItem, @cExt, @cDrive )

      IF ( cExt == '.C' ) .OR. ( cExt == ".c" ) .OR. ( cExt == '.CPP' ) .OR. ( cExt == ".cpp" )
         AADD( aRet, cFile )
      ENDIF

   NEXT
RETURN aRet

FUNCTION ExtenPrg( cExt, nType )

   LOCAL aExt   := { "C", "c" }
   LOCAL nPos
   LOCAL cTemp  := ""

   nPos := ASCAN( aExt, { | a | a == cExt } )

   IF nPos > 0
      SWITCH nType
      CASE 1
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'prg' )
         EXIT
      CASE  2
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'prG' )
         EXIT
      CASE  3
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'pRg' )
         EXIT
      CASE  4
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'Prg' )
         EXIT
      CASE  5
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'PRg' )
         EXIT
      CASE  6
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'PrG' )
         EXIT
      CASE  7
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'PRG' )
         EXIT
      END
   ENDIF

RETURN cTemp

STATIC FUNCTION GetDirs( cPattern, lGcc )

   LOCAL aDir   := {}
   LOCAL lLinux := AT( 'linux', LOWER( OS() ) ) > 0 .OR. lGcc

   AEVAL( DIRECTORY( cPattern + IIF( lLinux, "*", "*.*" ), "D" ), ;
          { | xItem | IIF( xItem[ 5 ] = "D" .AND. ;
          ( xItem[ 1 ] != "." .AND. xItem[ 1 ] != ".." ), ;
          ( AADD( aDir, cPattern + xItem[ 1 ] + IIF( lLinux, "/", '\' ) ), ;
          ), "" ) } )

RETURN ( aDir )

FUNCTION GetBccDir()

   LOCAL cPath   := ''
   LOCAL cEnv    := GETE( "PATH" )
   LOCAL aEnv    := HB_ATokens( cEnv, ";" )
   LOCAL nPos
   LOCAL cCurEnv := ""

   FOR EACH cCurEnv IN aEnv

      IF FILE( cCurEnv + '\bcc32.exe' ) .OR. FILE( UPPER( cCurEnv ) + '\BCC32.EXE' )
         cPath := cCurEnv
         cPath := LEFT( cPath, RAT( '\', cPath ) - 1 )
         EXIT
      ENDIF

   NEXT

RETURN cPath

FUNCTION GetVccDir()

   LOCAL cPath   := ''
   LOCAL cEnv    := GETE( "PATH" )
   LOCAL aEnv    := HB_ATokens( cEnv, ";" )
   LOCAL nPos
   LOCAL cCurEnv := ""

   FOR EACH cCurEnv IN aEnv

      IF FILE( cCurEnv + '\cl.exe' ) .OR. FILE( UPPER( cCurEnv ) + '\cl.EXE' )
         cPath := cCurEnv
         cPath := LEFT( cPath, RAT( '\', cPath ) - 1 )
         EXIT
      ENDIF

   NEXT

RETURN cPath

FUNCTION Exten( cExt, nType )

   LOCAL aExt    := { 'C', 'c', "CPP", "cpp" }
   LOCAL nPos
   LOCAL cTemp   := ""

   nPos := ASCAN( aExt, { | a | a == cExt } )
   IF nPos > 0

      SWITCH  nType
      CASE 1
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'o' )
         EXIT

      CASE 2
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'obj' )
         EXIT

      END

   ENDIF

RETURN cTemp


FUNCTION GetMakeDir()

   LOCAL cPath := ""
   LOCAL cExe  := HB_ARGV( 0 )

   cExe  := STRTRAN( cExe, "/", "\" )
   cPath := LEFT( cexe, RAT( "\", cexe ) - 1 )
   cPath := LEFT( cPath, RAT( "\", cPath ) - 1 )

RETURN cPath

FUNCTION GetSourceDirMacros()

   LOCAL aDirs
   LOCAL lLinux    := AT( 'linux', LOWER( OS() ) ) > 0
   LOCAL cDir      := IIF( lLinux, '/' + CURDIR() + '/', '\' + CURDIR() + '\' )
   LOCAL aStru     := { cDir }

   LOCAL nCounter  := 0
   LOCAL aMacros   := {}

   WHILE ++ nCounter <= LEN( aStru )

      IF ! EMPTY( aDirs := GetDirs( aStru[ nCounter ], lLinux ) )                // There are elements!
         AEVAL( aDirs, { | xItem | AADD( aStru, xItem ) } )
      ENDIF

   ENDDO

   FOR nCounter := 1 TO LEN( aStru )
      AADD( aMacros, { "SRC" + STRZERO( nCounter, 2, 0 ), STRTRAN( aStru[ nCounter ], cDir, '' ), .f. } )
   NEXT

RETURN aMacros

FUNCTION HbMake_FileDate( cFileName )

   LOCAL aFiles := DIRECTORY( cFileName )

RETURN IIF( LEN( aFiles ) == 1, aFiles[ 1, 3 ], CTOD( '' ) )

FUNCTION HbMake_FileTime( cFileName )

   LOCAL aFiles := DIRECTORY( cFileName )

RETURN IIF( LEN( aFiles ) == 1, aFiles[ 1, 4 ], '' )

FUNCTION TD2JUL( CTIME, DDATE )

RETURN DDATE - CTOD( '01/01/1900' ) + ( PRB_INT( TTOS( CTIME ) / 100000,, 5 ) )

FUNCTION TTOS( CTIME )

RETURN ( VAL( SUBSTR( CTIME, 7, 2 ) ) ) + ;
         ( VAL( SUBSTR( CTIME, 4, 2 ) ) * 60 ) + ;
         ( VAL( SUBSTR( CTIME, 1, 2 ) ) * 3600 )

FUNCTION PRB_INT( SOMENUMBER, length, NUM_DECIMALS )

   LOCAL NEGATIVE   := ( SOMENUMBER < 0 )
   LOCAL SOMESTRING
   LOCAL dotat

   DEFAULT NUM_DECIMALS TO 0
   DEFAULT length TO 19

   IF NEGATIVE
      SOMENUMBER := ABS( SOMENUMBER )
   ENDIF

   SOMENUMBER += .0000000000000005

   SOMESTRING := ALLTRIM( STR( SOMENUMBER ) )

   dotat := AT( '.', somestring )

   DO CASE
      CASE NUM_DECIMALS == 0
         IF dotat > 0
            somestring := LEFT( somestring, dotat - 1 )
         ENDIF

      CASE NUM_DECIMALS > 0
         IF dotat > 0
            somestring := LEFT( somestring, dotat + num_decimals )
         ENDIF

   ENDCASE

   IF NEGATIVE
      SOMESTRING := '-' + SOMESTRING
   ENDIF

RETURN VAL( SOMESTRING )

FUNCTION Exte( cExt, nType )

   LOCAL aExt  := { 'prg', 'prG', 'pRg', 'Prg', 'PRg', 'PrG', 'PRG' }
   LOCAL nPos
   LOCAL cTemp := ""

   nPos := ASCAN( aExt, { | a | a == cExt } )
   IF nPos > 0
      IF nType == 1
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'c' )
      ELSEIF nType == 2
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'obj' )
      ELSEIF nType == 3
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'o' )
      ENDIF

   ENDIF

RETURN cTemp

PROCEDURE ATTENTION( CSTRING, NLINENUM, CCOLOR )

   LOCAL COLDCOLOR

   DEFAULT NLINENUM TO 24
   DEFAULT CCOLOR TO 'GR+/R'

   COLDCOLOR := SETCOLOR( CCOLOR )

   CSTRING := '  ' + ALLTRIM( CSTRING ) + '  '

   DEVPOS( NLINENUM, c( CSTRING ) )

   DEVOUT( CSTRING )

   SETCOLOR( COLDCOLOR )

RETURN

FUNCTION c( CSTRING )

RETURN MAX( ( MAXCOL() / 2 ) - INT( LEN( CSTRING ) / 2 ), 0 )

FUNCTION ReadLN( lEof )

   LOCAL cBuffer := ""
   cBuffer := FT_FREADLN()
   cBuffer := STRTRAN( cBuffer, CHR( 13 ), '' )
   cBuffer := STRTRAN( cBuffer, CHR( 10 ), '' )
   FT_FSKIP( 1 )
   lEof := ft_FEOF()

RETURN cBuffer

FUNCTION GetInstaledLibs( clibs, lGcc )

   LOCAL cSuffix := IIF( lGCC, ".a", ".lib" )

   LOCAL aDefLib := { 'lang' + cSuffix, ;
                      'vm' + cSuffix, ;
                      'rtl' + cSuffix, ;
                      'rdd' + cSuffix, ;
                      'macro' + cSuffix, ;
                      'pp' + cSuffix, ;
                      'dbfntx' + cSuffix, ;
                      'dbfcdx' + cSuffix, ;
                      'common' + cSuffix, ;
                      'gtwin' + cSuffix, ;
                      'debug' + cSuffix, ;
                      'gtpca' + cSuffix, ;
                      'gtdos' + cSuffix, ;
                      'gtsln' + cSuffix, ;
                      'gtstd' + cSuffix, ;
                      'ziparchive' + cSuffix, ;
                      'rddads' + cSuffix, ;
                      'ace32' + cSuffix, ;
                      'libnf' + cSuffix, ;
                      'libct' + cSuffix, ;
                      'html' + cSuffix, ;
                      'libgt' + cSuffix, ;
                      'libmisc' + cSuffix, ;
                      'mysql' + cSuffix, ;
                      'libmysql' + cSuffix, ;
                      'mysqlclient' + cSuffix, ;
                      'samples' + cSuffix, ;
                      'pdflib' + cSuffix, ;
                      'nulsys' + cSuffix, ;
                      'gtcgi' + cSuffix, ;
                      'vmmt' + cSuffix, ;
                      'rtlmt' + cSuffix, ;
                      'rddmt' + cSuffix, ;
                      'ppmt' + cSuffix, ;
                      'dbfntxmt' + cSuffix, ;
                      'dbfcdxmt' + cSuffix, ;
                      'macromt' + cSuffix,;
                      'codepage' + cSuffix,;
                      'gtnul' + cSuffix }

   LOCAL aReturnLibs := {}
   LOCAL aLibs       := DIRECTORY( clibs )
   LOCAL nPos
   LOCAL nCount
   LOCAL cItem

   IF lGcc
      AEVAL( aLibs, { | x, y | cItem := x[ 1 ], IIF( LEFT( cItem, 3 ) == "lib", aLibs[ y, 1 ] := SUBSTR( cItem, 4 ), ) } )
   ENDIF

   FOR nCount := 1 TO LEN( aLibs )

      cItem := LOWER( aLibs[ nCount, 1 ] )

      nPos := ASCAN( aDefLib, { | a | LOWER( a ) == cItem } )
      IF nPos == 0
         AADD( aReturnLibs, aLibs[ nCount, 1 ] )
      ENDIF

   NEXT

RETURN aReturnLibs

FUNCTION Getlibs( lGcc, cDir )

   LOCAL lLinux        := AT( 'linux', LOWER( OS() ) ) > 0
   LOCAL cEnv          := GETENV( "HB_LIB_INSTALL" )
   LOCAL ainstaledlibs := Getinstaledlibs( IIF( ! lLinux, IIF( ! lGcc, cDir + "\*.lib", cDir + "\*.a" ), cEnv + '/*.a' ), lGcc )
   LOCAL aLibsDesc     := { { "Harbour Ct3 library - Libct",  IIF( lGcc, 'ct.a', 'libct.lib' ) }, ;
                        { "Harbour Misc library - Libmisc", IIF( lGcc, 'misc.a', 'libmisc.lib' ) }, ;
                        { "Harbour html library - Htmllib", 'html' + IIF( lGcc, '.a', '.lib' ) }, ;
                        { "Harbour Nanfor library - Libnf", 'nf' + IIF( lGcc, '.a', '.lib' ) }, ;
                        { "Harbour Gt library - Libgt", 'nf' + IIF( lGcc, '.a', '.lib' ) }, ;
                        { "Harbour Zip library ", 'ziparchive' + IIF( lGcc, '.a', '.lib' ) + IIF( lLinux, ' stdc++.a z.a', ' ' ) }, ;
                        { "Harbour Hbole library Hbole", 'hbole' + IIF( lGcc, '.a', '.lib' ) + ' ole2' + IIF( lGcc, '.a', '.lib' ) }, ;
                        { "Harbour Mysql library - MySql", 'mysql' + IIF( lGcc, '.a', '.lib' ) + ' libmysql' + IIF( lGcc, '.a', '.lib' ) + ' mysqlclient' + IIF( lGcc, '.a', '.lib' ) }, ;
                        { "Harbour Samples library - Samples", 'samples' + IIF( lGcc, '.a', '.lib' ) } }

   AEVAL( ainstaledlibs, { | x | AADD( aLibsDesc, { "User - " + x + " Library", x } ) } )

RETURN aLibsDesc

FUNCTION DIR_MULTI( cFileMaskList, cAttr )

   LOCAL aList := listasarray2( cFileMaskList, "|" )
   AEVAL( aList, { | tmp, tmp1 | aList[ tmp1 ] := DIRECTORY( tmp, cAttr ) } )

RETURN ArrayAJoin( alist )

FUNCTION ArrayAJoin( aArray )

   LOCAL tmp
   LOCAL nLenArray := LEN( aArray )
   LOCAL nLen
   LOCAL nPos      := LEN( aArray[ 1 ] ) + 1

   nLen := 0

   FOR tmp := 1 TO nLenArray
      nLen += LEN( aArray[ tmp ] )
   NEXT

   ASIZE( aArray[ 1 ], nLen )

   FOR tmp := 2 TO nLenArray
      ACOPY( aArray[ tmp ], aArray[ 1 ],,, nPos )
      nPos += LEN( aArray[ tmp ] )
   NEXT
RETURN aArray[ 1 ]

FUNCTION ListAsArray2( cList, cDelimiter )

   LOCAL nPos
   LOCAL aList  := {}              // Define an empty array

   IF cDelimiter = NIL
      cDelimiter := ","
   ENDIF
   //
   DO WHILE ( nPos := AT( cDelimiter, cList ) ) != 0
      AADD( aList, ALLTRIM( SUBSTR( cList, 1, nPos - 1 ) ) )                    // Add a new element
      cList := SUBSTR( cList, nPos + 1 )

   ENDDO
   AADD( aList, ALLTRIM( cList ) )      // Add final element
   //
RETURN aList        // Return the array

*+ EOF: HBMUTILS.PRG
