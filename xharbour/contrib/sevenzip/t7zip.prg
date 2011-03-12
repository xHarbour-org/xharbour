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

//------------------------------------------------------------------------------
CREATE CLASS T7ZIP

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
      HB_SevenZip( 0, 'l ' + ::cArcName, @::cBuffer, ::nBuffer )

   METHOD Test() INLINE;
      HB_SevenZip( 0, 't ' + ::cArcName, @::cBuffer, ::nBuffer )

   METHOD Extract( lWithPath ) INLINE;
      HB_SevenZip( 0, if( valtype( lWithPath ) == "L" .AND. lWithPath, 'x ', 'e ' ) + if( ::lAlwaysOverWrite, '-y ', '' ) + if( ::lShowProcessDlg, '-hide ', '' ) + ::cArcName, @::cBuffer, ::nBuffer )

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

   FOR EACH cFile IN ::aFiles
      ::cCommand += ' ' + cFile
   NEXT

   RETURN HB_SevenZip( 0, ::cCommand, @::cBuffer, ::nBuffer )

//------------------------------------------------------------------------------
INIT PROCEDURE _7ZINIT

   INIT7ZIPDLL()
   RETURN

//------------------------------------------------------------------------------
EXIT PROCEDURE _7ZEXIT

   EXIT7ZIPDLL()
   RETURN