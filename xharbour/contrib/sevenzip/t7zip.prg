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

#include "hbclass.ch"

STATIC AERRDEF := {;
     {"ERROR_DISK_SPACE",0x8005},{"ERROR_READ_ONLY",0x8006},;
     {"ERROR_USER_SKIP",0x8007},{"ERROR_UNKNOWN_TYPE",0X8008},;
     {"ERROR_METHOD",0x8009},{"ERROR_PASSWORD_FILE",0X800A},;
     {"ERROR_VERSION",0x800B},{"ERROR_FILE_CRC",0x800C},;
     {"ERROR_FILE_OPEN",0x800D},{"ERROR_MORE_FRESH",0x800E},;
     {"ERROR_NOT_EXIST",0x800F},{"ERROR_ALREADY_EXIST",0X8010},;
     {"ERROR_TOO_MANY_FILES",0X8011},{"ERROR_MAKEDIRECTORY",0X8012},;
     {"ERROR_CANNOT_WRITE",0X8013},{"ERROR_HUFFMAN_CODE",0X8014},;
     {"ERROR_COMMENT_HEADER",0X8015},{"ERROR_HEADER_CRC",0x8016},;
     {"ERROR_HEADER_BROKEN",0X8017},{"ERROR_ARC_FILE_OPEN",0X8018},;
     {"ERROR_NOT_ARC_FILE",0X8019},{"ERROR_CANNOT_READ",0X801A},;
     {"ERROR_FILE_STYLE",0x801B},{"ERROR_COMMAND_NAME",0X801C},;
     {"ERROR_MORE_HEAP_MEMORY",0X801D},{"ERROR_ENOUGH_MEMORY",0X801E},;
     {"ERROR_ALREADY_RUNNING",0X801F},{"ERROR_USER_CANCEL",0X8020},;
     {"ERROR_HARC_ISNOT_OPENED",0X8021},{"ERROR_NOT_SEARCH_MODE",0X8022},;
     {"ERROR_NOT_SUPPORT",0X8023},{"ERROR_TIME_STAMP",0x8024},;
     {"ERROR_TMP_OPEN",0x8025},{"ERROR_LONG_FILE_NAME",0X8026},;
     {"ERROR_ARC_READ_ONLY",0X8027},{"ERROR_SAME_NAME_FILE",0X8028},;
     {"ERROR_NOT_FIND_ARC_FILE",0X8029},{"ERROR_RESPONSE_READ",0X802A},;
     {"ERROR_NOT_FILENAME",0X802B},{"ERROR_TMP_COPY",0x802C},;
     {"ERROR_EOF",0x802D},{"ERROR_ADD_TO_LARC",0X802E},;
     {"ERROR_TMP_BACK_SPACE",0X802F},{"ERROR_SHARING",0x8030},;
     {"ERROR_NOT_FIND_FILE",0X8031},{"ERROR_LOG_FILE",0x8032},;
     {"ERROR_NO_DEVICE",0x8033},{"ERROR_GET_ATTRIBUTES",0X8034},;
     {"ERROR_SET_ATTRIBUTES",0X8035},{"ERROR_GET_INFORMATION",0X8036},;
     {"ERROR_GET_POINT",0x8037},{"ERROR_SET_POINT",0x8038},;
     {"ERROR_CONVERT_TIME",0X8039},{"ERROR_GET_TIME",0x803A},;
     {"ERROR_SET_TIME",0x803B},{"ERROR_CLOSE_FILE",0x803C},{"ERROR_HEAP_MEMORY",0X803D},;
     {"ERROR_HANDLE",0x803e},{"ERROR_TIME_STAMP_RANGE",0X803F},;
     {"ERROR_MAKE_ARCHIVE",0X8040},{"ERROR_NOT_CONFIRM_NAME",0X8041},;
     {"ERROR_UNEXPECTED_EOF",0X8042},{"ERROR_INVALID_END_MARK",0X8043},;
     {"ERROR_INVOLVED_LZH",0X8044},{"ERROR_NO_END_MARK",0X8045},;
     {"ERROR_HDR_INVALID_SIZE",0X8046},{"ERROR_UNKNOWN_LEVEL",0X8047},;
     {"ERROR_BROKEN_DATA",0X8048},{"ERROR_7ZIP_START",0x8100},{"ERROR_WARNING",0x8101},;
     {"ERROR_FATAL",0x8102},{"ERROR_DURING_DECOMPRESSION",0X8103},;
     {"ERROR_DIR_FILE_WITH_64BIT_SIZE",0X8104},{"ERROR_FILE_CHANGED_DURING_OPERATION",0X8105}}

//------------------------------------------------------------------------------
CREATE CLASS T7ZIP

   DATA nError AS INTEGER INIT 0                    // Error Message
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

   DATA cCommand AS STRING INIT ""               // Command line to pass
   DATA cArcType AS STRING INIT "7z"             // "7z" - 7z Format
                                                 // "zip" - zip format
   DATA aFiles AS ARRAY INIT {}                  // array of files
                                                 // if empty, all files in current
                                                 // directory will be archived
   METHOD New() INLINE Self
   METHOD Create()                               // Create archive

   METHOD Open() INLINE;
      ::handle := HB_SevenZipOpenArchive( 0, ::cArcName, 0 )

   METHOD List() INLINE;
      ::nError := HB_SevenZip( 0, 'l ' + ::cArcName, @::cBuffer, ::nBuffer )

   METHOD Test() INLINE;
      ::nError := HB_SevenZip( 0, 't ' + ::cArcName, @::cBuffer, ::nBuffer )

   METHOD Extract( lWithPath ) INLINE;
      ::nError := HB_SevenZip( 0, if( valtype( lWithPath ) == "L" .AND. lWithPath, 'x ', 'e ' ) + if( ::lAlwaysOverWrite, '-y ', '' ) + if( ::lShowProcessDlg, '-hide ', '' ) + ::cArcName, @::cBuffer, ::nBuffer )

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

   ::cCommand := 'a'
   IF !::lShowProcessDlg
      ::cCommand += ' ' + '-hide'
   ENDIF
   ::cCommand += ' ' + '-t' + ::cArcType
   ::cCommand += ' ' + '-m0=' + ::cCompressionMethod
   ::cCommand += ' ' + ::cArcName

   IF Valtype( ::aFiles ) == "A"
      FOR EACH cFile IN ::aFiles
         ::cCommand += ' ' + cFile
      NEXT
   ELSEIF Valtype( ::aFiles ) == "C"
      ::cCommand += ' ' + ::aFiles
   ENDIF

   RETURN ::nError := HB_SevenZip( 0, ::cCommand, @::cBuffer, ::nBuffer )

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
INIT PROCEDURE _7ZINIT

   INIT7ZIPDLL()
   RETURN

//------------------------------------------------------------------------------
EXIT PROCEDURE _7ZEXIT

   EXIT7ZIPDLL()
   RETURN

