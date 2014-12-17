/*
 * $Id$
 */

/*
 * CABINET SDK Project source code:
 *
 * Copyright 2012 Andi Jahja <xharbour@telkom.net.id>
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
 */

#ifndef __CABINET_H_INCLUDED
#define __CABINET_H_INCLUDED

#define HB_OS_WIN_USED

#if defined( _MSC_VER ) && ( _MSC_VER >= 1400 )
   #if ! defined( _CRT_SECURE_NO_WARNINGS )
      #define _CRT_SECURE_NO_WARNINGS
   #endif
#elif defined( __BORLANDC__ )
     #pragma warn -8004
     #pragma warn -8057
     #pragma warn -8071
#endif

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <io.h>
#include <fcntl.h>

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbinit.h"
#include "hbvm.h"

/* The following compilers :
   1. Borland BCC 5.5.1
   2. MinGW
   3. Open Watcom
   4. Digital Mars
   5. MSVS 2000
   6. MSVS 2003
   do not have the following files: fci.h and fdi.h. Therefore, for praticalily
   the files are included in this stuff.
 */

#include "fci.h"
#include "fdi.h"

#ifdef __cplusplus
extern "C" {
#endif

#define CAB_DLL "cabinet.dll"

typedef BOOL ( DIAMONDAPI * FCIADDFILE )( HFCI hfci, char * pszSourceFile, char * pszFileName, BOOL fExecute, PFNFCIGETNEXTCABINET GetNextCab, PFNFCISTATUS pfnProgress, PFNFCIGETOPENINFO pfnOpenInfo, TCOMP typeCompress );
typedef HFCI ( DIAMONDAPI * FCICREATE )( PERF perf, PFNFCIFILEPLACED pfnfiledest, PFNFCIALLOC pfnalloc, PFNFCIFREE pfnfree, PFNFCIOPEN pfnopen, PFNFCIREAD pfnread, PFNFCIWRITE pfnwrite, PFNFCICLOSE pfnclose, PFNFCISEEK pfnseek, PFNFCIDELETE pfndelete, PFNFCIGETTEMPFILE pfntemp, PCCAB pccab, void * pv );
typedef BOOL ( DIAMONDAPI * FCIFLUSHCABINET )( HFCI hfci, BOOL fGetNextCab, PFNFCIGETNEXTCABINET GetNextCab, PFNFCISTATUS pfnProgress );
typedef BOOL ( DIAMONDAPI * FCIFLUSHFOLDER )( HFCI hfci, PFNFCIGETNEXTCABINET GetNextCab, PFNFCISTATUS pfnProgress );
typedef BOOL ( DIAMONDAPI * FCIDESTROY )( HFCI hfci );
extern BOOL hb_FCIAddFile( HFCI hfci, char * pszSourceFile, char * pszFileName, BOOL fExecute, PFNFCIGETNEXTCABINET GetNextCab, PFNFCISTATUS pfnProgress, PFNFCIGETOPENINFO pfnOpenInfo, TCOMP typeCompress );
extern HFCI hb_FCICreate( PERF perf, PFNFCIFILEPLACED pfnfiledest, PFNFCIALLOC pfnalloc, PFNFCIFREE pfnfree, PFNFCIOPEN pfnopen, PFNFCIREAD pfnread, PFNFCIWRITE pfnwrite, PFNFCICLOSE pfnclose, PFNFCISEEK pfnseek, PFNFCIDELETE pfndelete, PFNFCIGETTEMPFILE pfntemp, PCCAB pccab, void * pv );
extern BOOL hb_FCIFlushCabinet( HFCI hfci, BOOL fGetNextCab, PFNFCIGETNEXTCABINET GetNextCab, PFNFCISTATUS pfnProgress );
extern BOOL hb_FCIFlushFolder( HFCI hfci, PFNFCIGETNEXTCABINET GetNextCab, PFNFCISTATUS pfnProgress );
extern BOOL hb_FCIDestroy( HFCI hfci );
typedef HFDI ( DIAMONDAPI * FDICREATE )( PFNALLOC pfnalloc, PFNFREE pfnfree, PFNOPEN pfnopen, PFNREAD pfnread, PFNWRITE pfnwrite, PFNCLOSE pfnclose, PFNSEEK pfnseek, int cpuType, PERF perf );
typedef BOOL ( DIAMONDAPI * FDIISCABINET )( HFDI hfdi, int hf, PFDICABINETINFO pfdici );
typedef BOOL ( DIAMONDAPI * FDICOPY )( HFDI hfdi, char * pszCabinet, char * pszCabPath, int flags, PFNFDINOTIFY pfnfdin, PFNFDIDECRYPT pfnfdid, void * pvUser );
typedef BOOL ( DIAMONDAPI * FDIDESTROY )( HFDI hfdi );
extern HFDI hb_FDICreate( PFNALLOC pfnalloc, PFNFREE pfnfree, PFNOPEN pfnopen, PFNREAD pfnread, PFNWRITE pfnwrite, PFNCLOSE pfnclose, PFNSEEK pfnseek, int cpuType, PERF perf );
extern BOOL hb_FDIIsCabinet( HFDI hfdi, int hf, PFDICABINETINFO pfdici );
extern BOOL hb_FDICopy( HFDI hfdi, char * pszCabinet, char * pszCabPath, int flags, PFNFDINOTIFY pfnfdin, PFNFDIDECRYPT pfnfdid, void * pvUser );
extern BOOL hb_FDIDestroy( HFDI hfdi );

#ifdef _HB_API_INTERNAL_
   #define HB_PUSHEVALSYM()   hb_vmPushSymbol(&hb_symEval )
#else
   #define HB_PUSHEVALSYM()   hb_vmPushEvalSym()
#endif

#if ! defined( FA_ARCH )
   #define FA_ARCH         32
#endif

#if ! defined ( _O_SEQUENTIAL )
   #define _O_SEQUENTIAL   0x0020  /* file access is primarily sequential */
#endif

#if ! defined ( _S_IREAD )
   #define _S_IREAD        0x0100  /* read permission, owner */
#endif

#if ! defined ( _S_IWRITE )
   #define _S_IWRITE       0x0080  /* write permission, owner */
#endif

/*
 * When a CAB file reaches this size, a new CAB will be created
 * automatically.  This is useful for fitting CAB files onto disks.
 *
 * If you want to create just one huge CAB file with everything in
 * it, change this to a very very large number.
 */
#define MEDIA_SIZE         300000

/*
 * When a folder has this much compressed data inside it,
 * automatically flush the folder.
 *
 * Flushing the folder hurts compression a little bit, but
 * helps random access significantly.
 */
#define FOLDER_THRESHOLD   900000

/*
 * Compression type to use
 */
#define COMPRESSION_TYPE   tcompTYPE_MSZIP

#if defined ( __POCC__ )
   #define TEMPNAME()   tmpnam( NULL )
#else
   #define TEMPNAME()   _tempnam( "", "xx" )
#endif

#if defined( __BORLANDC__ )
   #define ERROR_NO  ( * __errno() )
#elif defined( __POCC__ ) || defined( __WATCOMC__ ) || defined( __DMC__ )
   #include "errno.h"
   #define ERROR_NO  errno
#elif defined( _MSC_VER ) || defined( __MINGW32__ )
   #define ERROR_NO  ( * _errno() )
#endif

#if defined( __BORLANDC__ ) && ( __BORLANDC__ <= 0x590 )
   #define _OPEN( a, b, c )   _open( a, b )
#else
   #define _OPEN( a, b, c )   _open( a, b, c )
#endif

#if defined( __COMPRESS_C )
   static char * return_fci_error_string( FCIERROR err );
   static INT_PTR DIAMONDAPI fci_open( LPSTR pszFile, int oflag, int pmode, int * err, void * pv );
   static UINT DIAMONDAPI fci_read( INT_PTR hf, void * memory, UINT cb, int * err, void * pv );
   static UINT DIAMONDAPI fci_write( INT_PTR hf, void * memory, UINT cb, int * err, void * pv );
   static int DIAMONDAPI fci_close( INT_PTR hf, int * err, void * pv );
   static long DIAMONDAPI fci_seek( INT_PTR hf, long dist, int seektype, int * err, void * pv );
   static int DIAMONDAPI fci_delete( LPSTR pszFile, int * err, void * pv );
   static int DIAMONDAPI file_placed( PCCAB pccab, char * pszFile, long cbFile, BOOL fContinuation, void * pv );
   static BOOL DIAMONDAPI get_temp_file( char * pszTempName, int cbTempName, void * pv );
   static long DIAMONDAPI progress( UINT typeStatus, ULONG cb1, ULONG cb2, void * pv );
   static void store_cab_name( char * cabname, int iCab );
   static BOOL DIAMONDAPI get_next_cabinet( PCCAB pccab, ULONG cbPrevCab, void * pv );
   static INT_PTR DIAMONDAPI get_open_info( char * pszName, USHORT * pdate, USHORT * ptime, USHORT * pattribs, int * err, void * pv );
   static void set_cab_parameters( PCCAB cab_parms, char * szDestinationDir, ULONG uSize, ULONG uTreshHold, USHORT setCabID, char * szCabDisk );
   static void strip_path( char * filename, char * stripped_name );
   static int           iFile          = 1;
   static const char *  szMaskFileName = NULL;
   static void * DIAMONDAPI mem_alloc( ULONG cb );
   static void DIAMONDAPI mem_free( void * memory );
#elif defined( __DECOMPRESS_C )
   static INT_PTR DIAMONDAPI file_open( LPSTR pszFile, int oflag, int pmode );
   static UINT DIAMONDAPI file_read( INT_PTR hf, void * pv, UINT cb );
   static UINT DIAMONDAPI file_write( INT_PTR hf, void * pv, UINT cb );
   static int DIAMONDAPI file_close( INT_PTR hf );
   static long DIAMONDAPI file_seek( INT_PTR hf, long dist, int seektype );
   static INT_PTR DIAMONDAPI notification_function( FDINOTIFICATIONTYPE fdint, PFDINOTIFICATION pfdin );
   static char * DIAMONDAPI return_fdi_error_string( FDIERROR err );
   static void * DIAMONDAPI mem_alloc( ULONG cb );
   static void DIAMONDAPI mem_free( void * memory );
#endif /* __COMPRESS_C */

#if defined( __XHARBOUR__ )
   #define HB_STORNL hb_stornl
   #define HB_STORNI hb_storni
   #define HB_STORC  hb_storc
#else
   #define HB_STORNL hb_storvnl
   #define HB_STORNI hb_storvni
   #define HB_STORC  hb_storvc
#endif

#ifdef __cplusplus
}
#endif

#endif /* __CABINET_H_INCLUDED */
