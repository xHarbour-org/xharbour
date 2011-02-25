/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Compiler main file
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */
#define HB_OS_WIN_USED

#include "hbcomp.h"
#include "hbset.h"
#include "hbverbld.h"
#include "hbapierr.h"
#include "hbmemory.ch"

static int  s_iFileCase = HB_SET_CASE_MIXED;
static int  s_iDirCase  = HB_SET_CASE_MIXED;
static BOOL s_fFnTrim   = FALSE;
static char s_cDirSep   = HB_OS_PATH_DELIM_CHR;

/* ------------------------------------------------------------------------- */
/* FM statistic module */
/* ------------------------------------------------------------------------- */

/* remove this 'undef' when number of memory leaks will be reduced to
   reasonable size */
//#undef HB_FM_STATISTICS

#ifdef HB_FM_STATISTICS

#define HB_MEMINFO_SIGNATURE  0xDEADBEAF
#define HB_MEMSTR_BLOCK_MAX   32

typedef struct _HB_MEMINFO
{
   struct _HB_MEMINFO * pPrevBlock;
   struct _HB_MEMINFO * pNextBlock;
   ULONG  ulSize;
   int  iSourceLine;
   UINT32 Signature;
} HB_MEMINFO, * PHB_MEMINFO;

#ifdef HB_ALLOC_ALIGNMENT
#  define HB_MEMINFO_SIZE     ( ( sizeof( HB_MEMINFO ) + HB_ALLOC_ALIGNMENT - 1 ) - \
                                ( sizeof( HB_MEMINFO ) + HB_ALLOC_ALIGNMENT - 1 ) % HB_ALLOC_ALIGNMENT )
#else
#  define HB_MEMINFO_SIZE     sizeof( HB_MEMINFO )
#endif

static PHB_MEMINFO s_pMemBlocks = NULL;
static LONG s_ulMemoryBlocks = 0;      /* memory blocks used */
static LONG s_ulMemoryMaxBlocks = 0;   /* maximum number of used memory blocks */
static LONG s_ulMemoryMaxConsumed = 0; /* memory size consumed */
static LONG s_ulMemoryConsumed = 0;    /* memory max size consumed */

#endif /* HB_FM_STATISTICS */

#ifndef hb_xgrab
void * hb_xgrab( ULONG ulSize )        /* allocates fixed memory, exits on failure */
{
#ifdef HB_FM_STATISTICS
   #ifdef _MSC_VER
      void * pMem = HeapAlloc( GetProcessHeap(), 0, ulSize + HB_MEMINFO_SIZE + sizeof( UINT32 ) );
   #else
      void * pMem = malloc( ulSize + HB_MEMINFO_SIZE + sizeof( UINT32 ) );
   #endif

   if( pMem )
   {
      if( s_pMemBlocks )
         s_pMemBlocks->pPrevBlock = ( PHB_MEMINFO ) pMem;
      ( ( PHB_MEMINFO ) pMem )->pNextBlock = s_pMemBlocks;
      ( ( PHB_MEMINFO ) pMem )->pPrevBlock = NULL;
      s_pMemBlocks = ( PHB_MEMINFO ) pMem;
      ( ( PHB_MEMINFO ) pMem )->ulSize = ulSize;
      ( ( PHB_MEMINFO ) pMem )->iSourceLine = hb_comp_iLine;
      ( ( PHB_MEMINFO ) pMem )->Signature = HB_MEMINFO_SIGNATURE;
      HB_PUT_LE_UINT32( ( ( BYTE * ) pMem ) + HB_MEMINFO_SIZE + ulSize, HB_MEMINFO_SIGNATURE );

      s_ulMemoryConsumed += ulSize;
      if( s_ulMemoryMaxConsumed < s_ulMemoryConsumed )
         s_ulMemoryMaxConsumed = s_ulMemoryConsumed;
      s_ulMemoryBlocks++;
      if( s_ulMemoryMaxBlocks < s_ulMemoryBlocks )
         s_ulMemoryMaxBlocks = s_ulMemoryBlocks;
      pMem = ( BYTE * ) pMem + HB_MEMINFO_SIZE;
   }
#else
   #ifdef _MSC_VER
      void * pMem = HeapAlloc( GetProcessHeap(), 0, ulSize );
   #else
      void * pMem = malloc( ulSize );
   #endif
#endif

   if( ! pMem )
   {
      char szSize[ 32 ];

      hb_snprintf( szSize, sizeof( szSize ), "%lu", ulSize );
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMALLOC, szSize, NULL );
   }

   return pMem;
}
#endif

#ifndef hb_xrealloc
void * hb_xrealloc( void * pMem, ULONG ulSize )       /* reallocates memory */
{
#ifdef HB_FM_STATISTICS
   PHB_MEMINFO pMemBlock;
   ULONG ulMemSize;
   void * pResult;

   if( ulSize == 0 )
   {
      if( pMem )
         hb_xfree( pMem );
      return NULL;
   }
   else if( ! pMem )
      return hb_xgrab( ulSize );

   pMemBlock = ( PHB_MEMINFO ) ( ( BYTE * ) pMem - HB_MEMINFO_SIZE );
   ulMemSize = pMemBlock->ulSize;

   if( pMemBlock->Signature != HB_MEMINFO_SIGNATURE )
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMCORRUPT, NULL, NULL );

   if( HB_GET_LE_UINT32( ( ( BYTE * ) pMem ) + ulMemSize ) != HB_MEMINFO_SIGNATURE )
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMOVERFLOW, NULL, NULL );

   HB_PUT_LE_UINT32( ( ( BYTE * ) pMem ) + ulMemSize, 0 );

   #ifdef _MSC_VER
      pResult = HeapReAlloc( GetProcessHeap(), 0, pMemBlock, ulSize + HB_MEMINFO_SIZE + sizeof( UINT32 ) );
   #else
      pResult = realloc( pMemBlock, ulSize + HB_MEMINFO_SIZE + sizeof( UINT32 ) );
   #endif

   if( pResult )
   {
      if( s_pMemBlocks == pMemBlock )
         s_pMemBlocks = ( PHB_MEMINFO ) pResult;
      else
         ( ( PHB_MEMINFO ) pResult )->pPrevBlock->pNextBlock = ( PHB_MEMINFO ) pResult;

      if( ( ( PHB_MEMINFO ) pResult )->pNextBlock )
         ( ( PHB_MEMINFO ) pResult )->pNextBlock->pPrevBlock = ( PHB_MEMINFO ) pResult;
         s_ulMemoryConsumed += ( ulSize - ulMemSize );

      if( s_ulMemoryMaxConsumed < s_ulMemoryConsumed )
         s_ulMemoryMaxConsumed = s_ulMemoryConsumed;

      ( ( PHB_MEMINFO ) pResult )->ulSize = ulSize;  /* size of the memory block */
      HB_PUT_LE_UINT32( ( ( BYTE * ) pResult ) + ulSize + HB_MEMINFO_SIZE, HB_MEMINFO_SIGNATURE );
      pResult = ( BYTE * ) pResult + HB_MEMINFO_SIZE;
   }
#else
   #ifdef _MSC_VER
      void * pResult = HeapReAlloc( GetProcessHeap(), 0, pMem, ulSize );
   #else
      void * pResult = realloc( pMem, ulSize );
   #endif
#endif

   if( ! pResult && ulSize )
   {
      char szSize[ 32 ];

      hb_snprintf( szSize, sizeof( szSize ), "%lu", ulSize );
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMREALLOC, szSize, NULL );
   }

   return pResult;
}
#endif

#ifndef hb_xfree
void hb_xfree( void * pMem )            /* frees fixed memory */
{
   if( pMem )
   {
#ifdef HB_FM_STATISTICS
      PHB_MEMINFO pMemBlock = ( PHB_MEMINFO ) ( ( BYTE * ) pMem - HB_MEMINFO_SIZE );

      if( pMemBlock->Signature != HB_MEMINFO_SIGNATURE )
         hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMCORRUPT, NULL, NULL );

      if( HB_GET_LE_UINT32( ( ( BYTE * ) pMem ) + pMemBlock->ulSize ) != HB_MEMINFO_SIGNATURE )
         hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMOVERFLOW, NULL, NULL );

      s_ulMemoryConsumed -= pMemBlock->ulSize;
      s_ulMemoryBlocks--;
      if( s_pMemBlocks == pMemBlock )
         s_pMemBlocks = pMemBlock->pNextBlock;
      else
         pMemBlock->pPrevBlock->pNextBlock = pMemBlock->pNextBlock;

      if( pMemBlock->pNextBlock )
         pMemBlock->pNextBlock->pPrevBlock = pMemBlock->pPrevBlock;

      pMemBlock->Signature = 0;
      HB_PUT_LE_UINT32( ( ( BYTE * ) pMem ) + pMemBlock->ulSize, 0 );
      pMem = ( BYTE * ) pMem - HB_MEMINFO_SIZE;
#endif
      #if defined( _MSC_VER )
         HeapFree( GetProcessHeap(), 0, pMem );
      #else
         free( pMem );
      #endif
   }
   else
   {
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMFREE, NULL, NULL );
   }
}
#endif

ULONG hb_xquery( USHORT uiMode )
{
   ULONG ulResult = 0;

#ifdef HB_FM_STATISTICS
   switch( uiMode )
   {
      case HB_MEM_USED:
         ulResult = s_ulMemoryConsumed;
         break;

      case HB_MEM_USEDMAX:
         ulResult = s_ulMemoryMaxConsumed;
         break;
   }
#else
   HB_SYMBOL_UNUSED( uiMode );
#endif
   return ulResult;
}

#ifdef HB_FM_STATISTICS
static char * hb_memToStr( char * szBuffer, void * pMem, ULONG ulSize )
{
   unsigned char *byMem = ( BYTE * ) pMem;
   char * pDest = szBuffer;
   int iSize, i, iPrintable;

   if( ulSize > HB_MEMSTR_BLOCK_MAX )
      iSize = HB_MEMSTR_BLOCK_MAX;
   else
      iSize = ( int ) ulSize;

   iPrintable = 0;
   for( i = 0; i < iSize; ++i )
      if( ( byMem[ i ] & 0x7f ) >= 0x20 )
         iPrintable++;

   if( ( iPrintable * 100 ) / iSize > 70 ) /* more then 70% printable chars */
   {
      /* format as string of original chars */
      for( i = 0; i < iSize; ++i )
         if( ( byMem[ i ] & 0x7f ) >= 0x20 )
            * pDest++ = byMem[ i ];
         else
            * pDest++ = '.';
   }
   else
   {
     /* format as hex */
      for( i = 0; i < iSize; ++i )
      {
         int iLo = byMem[ i ] & 0x0f, iHi = byMem[ i ] >> 4;
         * pDest++ = '\\';
         * pDest++ = iHi <= 9 ? '0' + iHi : 'A' - 10 + iHi;
         * pDest++ = iLo <= 9 ? '0' + iLo : 'A' - 10 + iLo;
      }
   }
   * pDest = '\0';

   return szBuffer;
}
#endif

void hb_xexit( void )
{
#ifdef HB_FM_STATISTICS
   if( s_ulMemoryBlocks /* || hb_cmdargCheck( "INFO" ) */ )
   {
      char szBuffer[ HB_MAX( 3 * HB_MEMSTR_BLOCK_MAX + 1, 100 ) ];
      PHB_MEMINFO pMemBlock;
      int i;

      hb_conOutErr( hb_conNewLine(), 0 );
      hb_conOutErr( "----------------------------------------", 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      hb_snprintf( szBuffer, sizeof( szBuffer ), "Total memory allocated: %lu bytes (%lu blocks)", s_ulMemoryMaxConsumed, s_ulMemoryMaxBlocks );
      hb_conOutErr( szBuffer, 0 );

      if( s_ulMemoryBlocks )
      {
         hb_conOutErr( hb_conNewLine(), 0 );
         hb_snprintf( szBuffer, sizeof( szBuffer ), "WARNING! Memory allocated but not released: %lu bytes (%lu blocks)", s_ulMemoryConsumed, s_ulMemoryBlocks );
         hb_conOutErr( szBuffer, 0 );
      }

      hb_conOutErr( hb_conNewLine(), 0 );

      for( i = 1, pMemBlock = s_pMemBlocks; pMemBlock; ++i, pMemBlock = pMemBlock->pNextBlock )
         HB_TRACE( HB_TR_ERROR, ( "Line %i Block %i %p (size %lu) \"%s\"", pMemBlock->iSourceLine - 1, i,
                ( char * ) pMemBlock + HB_MEMINFO_SIZE, pMemBlock->ulSize,
                hb_memToStr( szBuffer, ( char * ) pMemBlock + HB_MEMINFO_SIZE,
                             pMemBlock->ulSize ) ) );
   }
#endif
}

/* NOTE: Use as minimal calls from here, as possible.
         Don't allocate memory from this function. [vszakats] */
void hb_errInternal( ULONG ulIntCode, const char * szText, const char * szPar1, const char * szPar2 )
{
   char buffer[ 1024 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_errInternal(%lu, %s, %s, %s)", ulIntCode, szText, szPar1, szPar2));

   hb_conOutErr( hb_conNewLine(), 0 );
   hb_snprintf( buffer, sizeof( buffer ), "Unrecoverable error %lu: ", ulIntCode );
   hb_conOutErr( buffer, 0 );
   if( szText )
   {
      hb_snprintf( buffer, sizeof( buffer ), szText, szPar1, szPar2 );
      hb_conOutErr( buffer, 0 );
   }
   hb_conOutErr( hb_conNewLine(), 0 );
   exit( EXIT_FAILURE );
}

void hb_conOutErr( const char * pStr, ULONG ulLen )
{
   if( ulLen == 0 )
      ulLen = strlen( pStr );

   fprintf( hb_comp_errFile, "%.*s", ( int ) ulLen, pStr );
}

char * hb_conNewLine( void )
{
   return ( char * ) "\n";
}

const char * hb_fsNameConv( const char * szFileName, char ** pszFree )
{
   if( s_fFnTrim || s_cDirSep != HB_OS_PATH_DELIM_CHR ||
       s_iFileCase != HB_SET_CASE_MIXED || s_iDirCase != HB_SET_CASE_MIXED )
   {
      PHB_FNAME pFileName;
      ULONG ulLen;

      if( pszFree )
      {
         szFileName = *pszFree = hb_strncpy( ( char * ) hb_xgrab( HB_PATH_MAX ),
                                             szFileName, HB_PATH_MAX - 1 );
      }

      if( s_cDirSep != HB_OS_PATH_DELIM_CHR )
      {
         char *p = ( char * ) szFileName;
         while( *p )
         {
            if( *p == s_cDirSep )
               *p = HB_OS_PATH_DELIM_CHR;
            p++;
         }
      }

      pFileName = hb_fsFNameSplit( szFileName );

      /* strip trailing and leading spaces */
      if( s_fFnTrim )
      {
         if( pFileName->szName )
         {
            ulLen = strlen( pFileName->szName );
            while( ulLen && pFileName->szName[ulLen - 1] == ' ' )
               --ulLen;
            while( ulLen && pFileName->szName[0] == ' ' )
            {
               ++pFileName->szName;
               --ulLen;
            }
            ( ( char * ) pFileName->szName )[ulLen] = '\0';
         }
         if( pFileName->szExtension )
         {
            ulLen = strlen( pFileName->szExtension );
            while( ulLen && pFileName->szExtension[ulLen - 1] == ' ' )
               --ulLen;
            while( ulLen && pFileName->szExtension[0] == ' ' )
            {
               ++pFileName->szExtension;
               --ulLen;
            }
            ( ( char * ) pFileName->szExtension )[ulLen] = '\0';
         }
      }

      /* FILECASE */
      if( s_iFileCase == HB_SET_CASE_LOWER )
      {
         if( pFileName->szName )
            hb_strlow( ( char * ) pFileName->szName );
         if( pFileName->szExtension )
            hb_strlow( ( char * ) pFileName->szExtension );
      }
      else if( s_iFileCase == HB_SET_CASE_UPPER )
      {
         if( pFileName->szName )
            hb_strupr( ( char * ) pFileName->szName );
         if( pFileName->szExtension )
            hb_strupr( ( char * ) pFileName->szExtension );
      }

      /* DIRCASE */
      if( pFileName->szPath )
      {
         if( s_iDirCase == HB_SET_CASE_LOWER )
            hb_strlow( ( char * ) pFileName->szPath );
         else if( s_iDirCase == HB_SET_CASE_UPPER )
            hb_strupr( ( char * ) pFileName->szPath );
      }

      hb_fsFNameMerge( ( char * ) szFileName, pFileName );
      hb_xfree( pFileName );
   }
   else if( pszFree )
      *pszFree = NULL;

   return szFileName;
}

int hb_setGetDirSeparator( void )
{
   return s_cDirSep;
}

/* ChangeLog CVS revision number */
int hb_verCvsID( void )
{
   return HB_VER_CVSID;
}

/* ChangeLog ID string */
const char * hb_verCvsChangeLogID( void )
{
   return HB_VER_CHLCVS;
}

/* ChangeLog last entry string */
const char * hb_verCvsLastEntry( void )
{
   return HB_VER_LENTRY;
}

/* build time C compiler flags in C_USR envvar */
const char * hb_verFlagsC( void )
{
#ifdef HB_VER_C_USR
   return HB_VER_C_USR;
#else
   return "";
#endif
}

/* build time linker flags in L_USR envvar */
const char * hb_verFlagsL( void )
{
#ifdef HB_VER_L_USR
   return HB_VER_L_USR;
#else
   return "";
#endif
}

/* build time Harbour compiler flags in PRG_USR envvar */
const char * hb_verFlagsPRG( void )
{
#ifdef HB_VER_PRG_USR
   return HB_VER_PRG_USR;
#else
   return "";
#endif
}

/* set deferred flag via pragma in prg file */
void hb_compSetDeferredFlagOn( void )
{
   hb_comp_autoDeferred = TRUE;
}
