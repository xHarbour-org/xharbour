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

#include "cabinet.h"

static HMODULE hModule = NULL;

static FARPROC cab_GetProcAddress( char * szFuncName )
{
   FARPROC pFunc = GetProcAddress( hModule, szFuncName );

   if( pFunc )
   {
      return pFunc;
   }
   else
   {
      char __szError[ 256 ];
      hb_snprintf( __szError, sizeof( __szError ),
                   "Cannot find function address: %s", szFuncName );
      MessageBox( NULL, __szError, szFuncName, MB_ICONSTOP );
      hb_vmRequestQuit();
      return NULL;
   }
}

HFDI hb_FDICreate( PFNALLOC pfnalloc, PFNFREE pfnfree, PFNOPEN pfnopen, PFNREAD pfnread, PFNWRITE pfnwrite, PFNCLOSE pfnclose, PFNSEEK pfnseek, int cpuType, PERF perf )
{
#ifdef __cplusplus
   static FDICREATE
#else
   static FDICREATE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FDICREATE ) cab_GetProcAddress( "FDICreate" );

   return pFunc( pfnalloc, pfnfree, pfnopen, pfnread, pfnwrite, pfnclose, pfnseek, cpuType, perf );
}

BOOL hb_FDIIsCabinet( HFDI hfdi, int hf, PFDICABINETINFO pfdici )
{
#ifdef __cplusplus
   static FDIISCABINET
#else
   static FDIISCABINET pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FDIISCABINET ) cab_GetProcAddress( "FDIIsCabinet" );

   return pFunc( hfdi, hf, pfdici );
}

BOOL hb_FDICopy( HFDI hfdi, char * pszCabinet, char * pszCabPath, int flags, PFNFDINOTIFY pfnfdin, PFNFDIDECRYPT pfnfdid, void * pvUser )
{
#ifdef __cplusplus
   static FDICOPY
#else
   static FDICOPY pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FDICOPY ) cab_GetProcAddress( "FDICopy" );

   return pFunc( hfdi, pszCabinet, pszCabPath, flags, pfnfdin, pfnfdid, pvUser );
}

BOOL hb_FDIDestroy( HFDI hfdi )
{
#ifdef __cplusplus
   static FDIDESTROY
#else
   static FDIDESTROY pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FDIDESTROY ) cab_GetProcAddress( "FDIDestroy" );

   return pFunc( hfdi );
}

BOOL hb_FCIAddFile( HFCI hfci, char * pszSourceFile, char * pszFileName, BOOL fExecute, PFNFCIGETNEXTCABINET GetNextCab, PFNFCISTATUS pfnProgress, PFNFCIGETOPENINFO pfnOpenInfo, TCOMP typeCompress )
{
#ifdef __cplusplus
   static FCIADDFILE
#else
   static FCIADDFILE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FCIADDFILE ) cab_GetProcAddress( "FCIAddFile" );

   return pFunc( hfci, pszSourceFile, pszFileName, fExecute, GetNextCab, pfnProgress, pfnOpenInfo, typeCompress );
}

HFCI hb_FCICreate( PERF perf, PFNFCIFILEPLACED pfnfiledest, PFNFCIALLOC pfnalloc, PFNFCIFREE pfnfree, PFNFCIOPEN pfnopen, PFNFCIREAD pfnread, PFNFCIWRITE pfnwrite, PFNFCICLOSE pfnclose, PFNFCISEEK pfnseek, PFNFCIDELETE pfndelete, PFNFCIGETTEMPFILE pfntemp, PCCAB pccab, void * pv )
{
#ifdef __cplusplus
   static FCICREATE
#else
   static FCICREATE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FCICREATE ) cab_GetProcAddress( "FCICreate" );

   return pFunc( perf, pfnfiledest, pfnalloc, pfnfree, pfnopen, pfnread, pfnwrite, pfnclose, pfnseek, pfndelete, pfntemp, pccab, pv );
}

BOOL hb_FCIFlushCabinet( HFCI hfci, BOOL fGetNextCab, PFNFCIGETNEXTCABINET GetNextCab, PFNFCISTATUS pfnProgress )
{
#ifdef __cplusplus
   static FCIFLUSHCABINET
#else
   static FCIFLUSHCABINET pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FCIFLUSHCABINET ) cab_GetProcAddress( "FCIFlushCabinet" );

   return pFunc( hfci, fGetNextCab, GetNextCab, pfnProgress );
}

BOOL hb_FCIFlushFolder( HFCI hfci, PFNFCIGETNEXTCABINET GetNextCab, PFNFCISTATUS pfnProgress )
{
#ifdef __cplusplus
   static FCIFLUSHFOLDER
#else
   static FCIFLUSHFOLDER pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FCIFLUSHFOLDER ) cab_GetProcAddress( "FCIFlushFolder" );

   return pFunc( hfci, GetNextCab, pfnProgress );
}

BOOL hb_FCIDestroy( HFCI hfci )
{
#ifdef __cplusplus
   static FCIDESTROY
#else
   static FCIDESTROY pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( FCIDESTROY ) cab_GetProcAddress( "FCIDestroy" );

   return pFunc( hfci );
}

static void hb_cabinetInit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   hModule = LoadLibrary( ( LPCSTR ) CAB_DLL );

   if( ! hModule )
   {
      char __szError[ 256 ];
      hb_snprintf( __szError, sizeof( __szError ), "Cannot load %s", CAB_DLL );
      hb_errInternal( 5178, __szError, NULL, NULL );
      return;
   }
}

static void hb_cabinetExit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( hModule )
      FreeLibrary( hModule );
}

#define __PRG_SOURCE__        __FILE__

#ifdef HB_PCODE_VER
   #undef HB_PRG_PCODE_VER
   #define HB_PRG_PCODE_VER   HB_PCODE_VER
#endif

HB_CALL_ON_STARTUP_BEGIN( _hb_cab_init_ )
hb_vmAtInit( hb_cabinetInit, NULL );
hb_vmAtExit( hb_cabinetExit, NULL );
HB_CALL_ON_STARTUP_END( _hb_cab_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_cab_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY HB_DATASEG_FUNC( _hb_cab_init_ )
   #include "hbiniseg.h"
#endif

