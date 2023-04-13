/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SevenZip xHarbour Interface
 *
 * Copyright 2011 Andi Jahja <andi.jahja@yahoo.co.id>
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "t7zip.ch"

//------------------------------------------------------------------------------
CREATE CLASS T7ZIP

   DATA hWndOwner AS INTEGER INIT 0              // handle of parent window
   DATA nError AS INTEGER INIT 0                 // Error Message
   DATA handle AS INTEGER INIT 0
   DATA lShowProcessDlg AS LOGICAL INIT .F.      // .T. - show progress dialog
   DATA lAlwaysOverWrite AS LOGICAL INIT .T.     // overwrite when extract
   DATA cArcName                                 // Output filename
   DATA cBuffer AS STRING INIT ""                // Buffer to hold DLL output
   DATA nBuffer AS INTEGER INIT 0                // Buffer Size to hold DLL output
   DATA cCompressionMethod AS STRING INIT "PPMd" // "LZMA" - LZ-based algorithm
                                                 // "LZMA2" - LZMA-based algorithm
                                                 // "PPMd" - Dmitry Shkarin's PPMdH with small changes
                                                 // "BZip2" - BWT algorithm
                                                 // "Deflate" - LZ+Huffman
                                                 // "Copy" - No Compression
   DATA nCompressionMethod AS INTEGER INIT 3     // 3 = PPMd, default
   DATA nZipCompressionLevel AS INTEGER INIT 5   // Zip compression Level
                                                 // Range: 0 - 9
   DATA cCommand AS STRING INIT ""               // Command line to pass
   DATA nArctype AS INTEGER INIT 1               // "7z" - 7z format
   DATA cArcType AS STRING INIT "7z"             // "7z" - 7z Format
                                                 // "zip" - zip format
   DATA aFiles AS ARRAY INIT {}                  // array of files
                                                 // if empty, all files in current
                                                 // directory will be archived
   DATA cPassword AS STRING INIT ""              // password for archive file
   DATA lRecursive AS LOGICAL INIT .F.           // .T. = include sub-dir
   DATA aExcludeFiles AS ARRAY INIT {}           // do not include these files
                                                 // in archive.
   DATA aVolumes AS ARRAY INIT {}                // creates multi-volume archive
   DATA lSolid AS LOGICAL INIT .T.               // create solid archive. if set
                                                 // to .T., archive cannot be updated
                                                 // Note: setting this to .F. will enable
                                                 // update but the compression
                                                 // will be degraded, ie bigger size
   DATA lMultiCPU AS LOGICAL INIT .F.            // set this to .T. when using
                                                 // multi CPU such as dual[quad]-core
   DATA lConvertANSIToOEM AS LOGICAL INIT .T.    // Some system requires to convert
                                                 // ANSI file name to OEM

   METHOD New() INLINE Self
   METHOD Create()                               // Create archive

   METHOD Open() INLINE;
      ::handle := HB_SevenZipOpenArchive( ::hWndOwner, ::cArcName, 0 )

   METHOD List() INLINE;
      ::nError := HB_SevenZip( ::hWndOwner, 'l "' + HB_ANSITOOEM( ::cArcName ) + '"', @::cBuffer, ::nBuffer )

   METHOD Test() INLINE;
      ::nError := HB_SevenZip( ::hWndOwner, 't "' + HB_ANSITOOEM( ::cArcName ) + '"', @::cBuffer, ::nBuffer )

   METHOD Extract( lWithPath ) INLINE;
      ::nError := HB_SevenZip( ::hWndOwner, if( valtype( lWithPath ) == "L" .AND. lWithPath, 'x ', 'e ' ) + if( ::lAlwaysOverWrite, '-y ', '' ) + if( ::lShowProcessDlg, '-hide ', '' ) + '"' + HB_ANSITOOEM( ::cArcName ) + '"', @::cBuffer, ::nBuffer )

   METHOD ErrorDescription()

   METHOD Close()                INLINE HB_SevenZipCloseArchive( ::handle )
   METHOD GetArcFileSize      () INLINE HB_SevenZipGetArcfilesize      ( ::handle )
   METHOD GetArcOriginalSize  () INLINE HB_SevenZipGetArcoriginalsize  ( ::handle )
   METHOD GetArcCompressedSize() INLINE HB_SevenZipGetArccompressedsize( ::handle )
   METHOD GetArcRatio         () INLINE HB_SevenZipGetArcratio         ( ::handle )
   METHOD GetOriginaLSize     () INLINE HB_SevenZipGetOriginalsize     ( ::handle )
   METHOD GetCompressedSize   () INLINE HB_SevenZipGetCompressedsize   ( ::handle )
   METHOD GetRatio            () INLINE HB_SevenZipGetRatio            ( ::handle )

END CLASS

//------------------------------------------------------------------------------
METHOD T7Zip:Create()

   LOCAL cFile

   // Currently only support 7z and zip
   IF Valtype( ::nArcType ) == "N" .AND. ::nArcType > 0 .AND. ::nArcType <= 2

      ::cArcType := aArcType[ ::nArcType ]
      ::cCommand := 'a'

      IF !::lShowProcessDlg
         ::cCommand += ' ' + '-hide'
      ENDIF

      ::cCommand += ' ' + '-t' + ::cArcType

      SWITCH ::nArcType

         CASE ARCTYPE_ZIP
            IF ::nZipCompressionLevel >= 0 .AND. ::nZipCompressionLevel<= 9
               ::cCommand += ' ' + '-mx' + LTRIM( STR( ::nZipCompressionLevel ) )
            ENDIF
            EXIT

         DEFAULT
            IF ValType( ::nCompressionMethod ) == "N" .AND. ::nCompressionMethod > 0 .AND. ::nCompressionMethod <= Len( aArcMethod )
               ::cCompressionMethod := aArcMethod[ ::nCompressionMethod ]
               ::cCommand += ' ' + '-m0=' + ::cCompressionMethod
            ENDIF
      END

      IF Valtype( ::cPassword ) == "C" .AND. Len( ::cPassword ) > 0
         ::cCommand += ' ' + '-p' + ALLTRIM( ::cPassword )
      ENDIF

      IF Valtype( ::lRecursive ) == "L" .AND. ::lRecursive
         ::cCommand += ' ' + '-r'
      ENDIF

      IF Valtype( ::lSolid ) == "L" .AND. !::lSolid
         ::cCommand += ' ' + '-ms=off'
      ENDIF

      IF Valtype( ::lMultiCPU ) == "L" .AND. ::lMultiCPU
         ::cCommand += ' ' + '-mmt'
      ENDIF

      IF Valtype( ::aExcludeFiles ) == "A"
         FOR EACH cFile IN ::aExcludeFiles
            ::cCommand += ' ' + '-x!' + ALLTRIM( cFile )
         NEXT
      ELSEIF Valtype( ::aExcludeFiles ) == "C"
         ::cCommand += ' ' + '-x!' + ALLTRIM( ::aExcludeFiles )
      ENDIF

      IF Valtype( ::aVolumes ) == "A"
         FOR EACH cFile IN ::aVolumes
            ::cCommand += ' ' + '-v' + LTRIM( STR( cFile ) ) + 'b'
         NEXT
      ELSEIF Valtype( ::aVolumes ) == "N"
         ::cCommand += ' ' + '-v' + LTRIM( STR( ::aVolumes ) ) + 'b'
      ENDIF

      ::cCommand += ' ' + HB_7ZIPCONVERTFILENAME( ::cArcName, ::lConvertANSIToOEM )

      IF Valtype( ::aFiles ) == "A"
         FOR EACH cFile IN ::aFiles
            ::cCommand += ' ' + HB_7ZIPCONVERTFILENAME( cFile, ::lConvertANSIToOEM )
         NEXT
      ELSEIF Valtype( ::aFiles ) == "C"
         ::cCommand += ' ' + HB_7ZIPCONVERTFILENAME( ::aFiles, ::lConvertANSIToOEM )
      ENDIF

      RETURN ::nError := HB_SevenZip( ::hWndOwner, ::cCommand, @::cBuffer, ::nBuffer )
   ENDIF

   RETURN ::nError := ERROR_NOT_SUPPORT

//------------------------------------------------------------------------------
METHOD T7Zip:ErrorDescription()

   LOCAL i

   IF ::nError == 0
      RETURN "ERROR_OK"
   ELSEIF ( i := AScan( AERRDEF, { | e | e[ 2 ] == ::nError } ) ) > 0
      RETURN AERRDEF[ i ] [ 1 ]
   ENDIF

   RETURN "ERROR_UNKNOWN"

//------------------------------------------------------------------------------
STATIC FUNCTION HB_7ZIPCONVERTFILENAME( cFileName, lConvert )

   IF lConvert
      RETURN '"' + HB_ANSITOOEM( cFileName ) + '"'
   ENDIF

   RETURN cFileName

//------------------------------------------------------------------------------
INIT PROCEDURE _7ZINIT

   INIT7ZIPDLL()
   RETURN

//------------------------------------------------------------------------------
EXIT PROCEDURE _7ZEXIT

   EXIT7ZIPDLL()
   RETURN

