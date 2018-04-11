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


#include "hbcomp.h"
#include "hbset.h"
#include "hbverbld.h"
#include "hbapierr.h"
#include "hbdate.h"
#include "hbmemory.ch"

#if defined( HB_OS_WIN )
   #include <windows.h>
#endif

static int  s_iFileCase = HB_SET_CASE_MIXED;
static int  s_iDirCase  = HB_SET_CASE_MIXED;
static BOOL s_fFnTrim   = FALSE;
static char s_cDirSep   = HB_OS_PATH_DELIM_CHR;

extern void _hb_compDeclaredInit( void );
extern char* hb_exeName( void );
extern void cleansPair( void );

/* ------------------------------------------------------------------------- */
/* FM statistic module */
/* ------------------------------------------------------------------------- */

/* remove this 'undef' when number of memory leaks will be reduced to
   reasonable size */

#if ( defined( __BORLANDC__ ) && ( __BORLANDC__ == 0x620 ) )
/* Disabled: Borland C 6.2 internal error when compiling dlmalloc.c */
   #if defined( HB_FM_DL_ALLOC )
      #undef HB_FM_DL_ALLOC
   #endif
#endif

#if defined( __HB_COMP_TRACE__ )
/* Disable external memory manager when debugging */
   #undef HB_FM_DL_ALLOC
#endif

#if defined( HB_FM_DL_ALLOC )
   #undef HB_FM_STD_ALLOC
   #undef HB_FM_WIN32_ALLOC
#elif defined( __EXPORT__ ) && ! defined( HB_FM_WIN32_ALLOC )
   #define HB_FM_WIN32_ALLOC
   #undef HB_FM_DL_ALLOC
   #undef HB_FM_STD_ALLOC
#elif defined( HB_FM_WIN32_ALLOC )
   #undef HB_FM_DL_ALLOC
   #undef HB_FM_STD_ALLOC
#elif defined( HB_FM_STD_ALLOC )
   #undef HB_FM_DL_ALLOC
   #undef HB_FM_WIN32_ALLOC
#else
   #define HB_FM_STD_ALLOC
   #undef HB_FM_DL_ALLOC
   #undef HB_FM_WIN32_ALLOC
#endif

#if defined( HB_FM_DL_ALLOC )
#  define USE_LOCKS       0
#  if defined( HB_DEBUG )
#     if ! defined( DEBUG )
#        define DEBUG     1
#     endif
#  endif
#  undef FORCEINLINE
#  if ! defined( FORCEINLINE )
#     define FORCEINLINE   HB_FORCEINLINE
#  endif
#  define REALLOC_ZERO_BYTES_FREES
#  if defined( __BORLANDC__ )
#     pragma warn -8019
#     pragma warn -8027
#     pragma warn -8084
#     pragma warn -8041
#     pragma warn -8008
#     pragma warn -8004
#     pragma warn -8066
#  elif defined( _MSC_VER ) || defined( __DMC__ ) || defined( __WATCOMC__ )
#     if ( defined( _MSC_VER ) && ( _MSC_VER < 1400 ) && ! defined( __POCC__ ) )
#        include "intsafe.h"
#     endif
#     if defined( __WATCOMC__ )
#        pragma disable_message ( 201 )
#        pragma disable_message ( 302 )
#     endif
#     define USE_DL_PREFIX
#  endif
#  include "errno.h"
#  if defined( __POCC__ )
#     pragma warn( push )
#     pragma warn( disable:2154 )
#     pragma warn( disable:2243 )
#  endif
#  if defined( __XCC__ )
#     include "source/vm/dlmalloc.c"
#  else
#     include "../source/vm/dlmalloc.c"
#  endif
#  if defined( __WATCOMC__ )
#     pragma enable_message ( 201 )
#  endif
#  if defined( __POCC__ )
#     pragma warn( pop )
#  endif
#  if defined( __BORLANDC__ )
#     pragma warn +8019
#     pragma warn +8027
#     pragma warn +8084
#     pragma warn +8041
#     pragma warn +8008
#     pragma warn +8004
#     pragma warn +8066
#  endif
#  if defined( USE_DL_PREFIX )
#     define malloc( n )      dlmalloc( ( n ) )
#     define realloc( p, n )  dlrealloc( ( p ), ( n ) )
#     define free( p )        dlfree( ( p ) )
#  endif
#elif defined( HB_FM_WIN32_ALLOC ) && defined( HB_OS_WIN )
#  if defined( HB_FM_LOCALALLOC )
#     define malloc( n )      ( void * ) LocalAlloc( LMEM_FIXED, ( n ) )
#     define realloc( p, n )  ( void * ) LocalReAlloc( ( HLOCAL ) ( p ), ( n ), LMEM_MOVEABLE )
#     define free( p )        LocalFree( ( HLOCAL ) ( p ) )
#  else
static HANDLE hProcessHeap = 0;
#     define malloc( n )      ( assert( hProcessHeap ), ( void * ) HeapAlloc( hProcessHeap, 0, ( n ) ) )
#     define realloc( p, n )  ( void * ) HeapReAlloc( hProcessHeap, 0, ( void * ) ( p ), ( n ) )
#     define free( p )        HeapFree( hProcessHeap, 0, ( void * ) ( p ) )
#  endif
#endif

#if defined( __HB_COMP_TRACE__ )

#define HB_MEMINFO_SIGNATURE  0xDEADBEAF
#define HB_MEMSTR_BLOCK_MAX   32

typedef struct _HB_MEMINFO
{
   struct _HB_MEMINFO * pPrevBlock;
   struct _HB_MEMINFO * pNextBlock;
   HB_SIZE              ulSize;
   const char *         szSourceFile;
   const char *         szFuncName;
   int                  iSourceLine;
   UINT32               Signature;
} HB_MEMINFO, * PHB_MEMINFO;

#ifdef HB_ALLOC_ALIGNMENT
#  define HB_MEMINFO_SIZE  ( ( sizeof( HB_MEMINFO ) + HB_ALLOC_ALIGNMENT - 1 ) - \
                             ( sizeof( HB_MEMINFO ) + HB_ALLOC_ALIGNMENT - 1 ) % HB_ALLOC_ALIGNMENT )
#else
#  define HB_MEMINFO_SIZE  sizeof( HB_MEMINFO )
#endif

static PHB_MEMINFO   s_pMemBlocks            = NULL;
static HB_SIZE          s_ulMemoryBlocks        = 0;  /* memory blocks used */
static HB_SIZE          s_ulMemoryMaxBlocks     = 0;  /* maximum number of used memory blocks */
static HB_SIZE          s_ulMemoryMaxConsumed   = 0;  /* memory size consumed */
static HB_SIZE          s_ulMemoryConsumed      = 0;  /* memory max size consumed */

#endif /* __HB_COMP_TRACE__ */


HB_SIZE hb_xquery( USHORT uiMode )
{
   HB_SIZE ulResult = 0;

#if defined( __HB_COMP_TRACE__ )
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

#if defined( __HB_COMP_TRACE__ )
static char * hb_memToStr( char * szBuffer, void * pMem, HB_SIZE ulSize )
{
   BYTE *   byMem = ( BYTE * ) pMem;
   HB_SIZE  i, iPrintable;

  
   iPrintable = 0;
   for( i = 0; i < ulSize; ++i )
      if( ( byMem[ i ] & 0x60 ) != 0 )
         iPrintable++;

   if( ( iPrintable * 100 ) / ulSize > 70 ) /* more then 70% printable chars */
   {
      /* format as string of original chars */
      for( i = 0; i < ulSize; ++i ) 
	  {
         if(  byMem[ i ] >= ' ' )
            szBuffer[ i ] = byMem[ i ];
         else
            szBuffer[ i ] = '.';
      }
	  szBuffer[ i ] = '\0';	
   }
   else
   {
      /* format as hex */
      for( i = 0; i < ulSize; ++i )
      {
		 HB_BYTE hinibble = byMem[ i ] >> 4;
         HB_BYTE lownibble = byMem[ i ] & 0x0F;         
         szBuffer[ i * 2 ] = hinibble <= 9 ? ('0' + hinibble) : ('A' - 10 + hinibble);
         szBuffer[ i * 2 + 1 ]= lownibble <= 9 ? ('0' + lownibble) : ('A' - 10 + lownibble);
      }
	   szBuffer[ i * 2 ] = '\0';
   }
   

   return szBuffer;
}
#endif

/* allocates fixed memory, exits on failure
 */
void * hb_xgrabEx( HB_SIZE ulSize, const char* szSourceFile, int iLine, const char*  szFuncName )
{
#if defined( __HB_COMP_TRACE__ )

   void *   pMem  = malloc( ulSize + HB_MEMINFO_SIZE + sizeof( UINT32 ) );

   if( pMem )
   {
      if( s_pMemBlocks )
         s_pMemBlocks->pPrevBlock = ( PHB_MEMINFO ) pMem;
      ( ( PHB_MEMINFO ) pMem )->pNextBlock   = s_pMemBlocks;
      ( ( PHB_MEMINFO ) pMem )->pPrevBlock   = NULL;
      s_pMemBlocks                           = ( PHB_MEMINFO ) pMem;
      ( ( PHB_MEMINFO ) pMem )->ulSize       = ulSize;
      ( ( PHB_MEMINFO ) pMem )->szSourceFile = szSourceFile;
      ( ( PHB_MEMINFO ) pMem )->szFuncName   = szFuncName;
      ( ( PHB_MEMINFO ) pMem )->iSourceLine  = iLine;
      ( ( PHB_MEMINFO ) pMem )->Signature    = HB_MEMINFO_SIGNATURE;
      HB_PUT_LE_UINT32( ( ( BYTE * ) pMem ) + HB_MEMINFO_SIZE + ulSize, HB_MEMINFO_SIGNATURE );

      s_ulMemoryConsumed                     += ulSize;
      if( s_ulMemoryMaxConsumed < s_ulMemoryConsumed )
         s_ulMemoryMaxConsumed = s_ulMemoryConsumed;
      s_ulMemoryBlocks++;
      if( s_ulMemoryMaxBlocks < s_ulMemoryBlocks )
         s_ulMemoryMaxBlocks = s_ulMemoryBlocks;
      pMem = ( BYTE * ) pMem + HB_MEMINFO_SIZE;
   }
#else
   void *   pMem  = malloc( ( size_t ) ulSize );
   HB_SYMBOL_UNUSED( szSourceFile );
   HB_SYMBOL_UNUSED( szFuncName );
   HB_SYMBOL_UNUSED( iLine );
#endif

   if( ! pMem )
   {
      char szSize[ 32 ];

      hb_snprintf( szSize, sizeof( szSize ), "%lu", (long unsigned int) ulSize );
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMALLOC, szSize, NULL );
   }

   return pMem;
}

/* frees fixed memory
 */
void hb_xfreeEx( void * pMem, const char* szSourceFile, int iLine, const char*  szFuncName  )
{
   if( pMem )
   {
#if defined( __HB_COMP_TRACE__ )
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

      pMemBlock->szSourceFile = szSourceFile;
      pMemBlock->szFuncName   = szFuncName;
      pMemBlock->iSourceLine  = iLine;
      pMemBlock->Signature    = 0;
      HB_PUT_LE_UINT32( ( ( BYTE * ) pMem ) + pMemBlock->ulSize, 0 );
      pMem                 = ( BYTE * ) pMem - HB_MEMINFO_SIZE;
#endif
      HB_SYMBOL_UNUSED( szSourceFile );
      HB_SYMBOL_UNUSED( szFuncName );
      HB_SYMBOL_UNUSED( iLine );

      free( pMem );
   }
   else
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMFREE, NULL, NULL );
}

/* reallocates memory
 */
void * hb_xreallocEx( void * pMem, HB_SIZE ulSize, const char* szSourceFile, int iLine, const char*  szFuncName )
{
#if defined( __HB_COMP_TRACE__ )
   PHB_MEMINFO pMemBlock;
   HB_SIZE       ulMemSize;
   void *      pResult;

   if( ulSize == 0 )
   {
      if( pMem )
         hb_xfreeEx( pMem, szSourceFile, iLine, szFuncName );
      return NULL;
   }
   else if( ! pMem )
      return hb_xgrabEx( ulSize, szSourceFile, iLine, szFuncName );

   pMemBlock   = ( PHB_MEMINFO ) ( ( BYTE * ) pMem - HB_MEMINFO_SIZE );
   ulMemSize   = pMemBlock->ulSize;

   if( pMemBlock->Signature != HB_MEMINFO_SIGNATURE )
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMCORRUPT, NULL, NULL );

   if( HB_GET_LE_UINT32( ( ( BYTE * ) pMem ) + ulMemSize ) != HB_MEMINFO_SIGNATURE )
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMOVERFLOW, NULL, NULL );

   HB_PUT_LE_UINT32( ( ( BYTE * ) pMem ) + ulMemSize, 0 );

   pResult  = realloc( pMemBlock, ulSize + HB_MEMINFO_SIZE + sizeof( UINT32 ) );

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

      ( ( PHB_MEMINFO ) pResult )->ulSize       = ulSize;  /* size of the memory block */
      ( ( PHB_MEMINFO ) pResult )->szSourceFile = szSourceFile;
      ( ( PHB_MEMINFO ) pResult )->szFuncName   = szFuncName;
      ( ( PHB_MEMINFO ) pResult )->iSourceLine  = iLine;

      HB_PUT_LE_UINT32( ( ( BYTE * ) pResult ) + ulSize + HB_MEMINFO_SIZE, HB_MEMINFO_SIGNATURE );
      pResult                             = ( BYTE * ) pResult + HB_MEMINFO_SIZE;
   }
#else
   void *   pResult  = realloc( pMem, ( size_t ) ulSize );
   HB_SYMBOL_UNUSED( szSourceFile );
   HB_SYMBOL_UNUSED( szFuncName );
   HB_SYMBOL_UNUSED( iLine );
#endif

   if( ! pResult && ulSize )
   {
      char szSize[ 32 ];

      hb_snprintf( szSize, sizeof( szSize ), "%lu", (long unsigned int) ulSize );
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMREALLOC, szSize, NULL );
   }

   return pResult;
}

void hb_xexitEx( void )
{
#if defined( __HB_COMP_TRACE__ )
   if( s_ulMemoryBlocks /* || hb_cmdargCheck( "INFO" ) */ )
   {
      char        szBuffer[ HB_MAX( 3 * HB_MEMSTR_BLOCK_MAX + 1, 100 ) ];
      PHB_MEMINFO pMemBlock;
      int         i;
      FILE *      hLog = hb_fopen( "fm.log", "a+" );

      if( hLog )
      {
         char *   szPlatform  = hb_verPlatform();
         char *   szCompiler  = hb_verCompiler();
         char *   szHarbour   = hb_verHarbour();
         char *   szTime      = ( char * ) hb_xgrab( 9 );
         int      iYear, iMonth, iDay;

         hb_dateToday( &iYear, &iMonth, &iDay );
         hb_dateTimeStr( szTime );

         fprintf( hLog, "Memory Allocation Report\n" );
         fprintf( hLog, "Application: %s\n", hb_exeName() );
         fprintf( hLog, "xHarbour Version: %s\n", szHarbour );
         fprintf( hLog, "Compiler: %s\n", szCompiler );
         fprintf( hLog, "Platform: %s\n", szPlatform );
         fprintf( hLog, "Time Occured: %04d.%02d.%02d %s\n", iYear, iMonth, iDay, szTime );

         hb_xfree( szPlatform );
         hb_xfree( szCompiler );
         hb_xfree( szHarbour  );
         hb_xfree( szTime );
      }

      hb_conOutErr( hb_conNewLine(), 0 );
      hb_conOutErr( "----------------------------------------", 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      hb_snprintf( szBuffer, sizeof( szBuffer ), "Total memory allocated: %" HB_PFS "u bytes (%" HB_PFS "u blocks)", s_ulMemoryMaxConsumed, s_ulMemoryMaxBlocks );
      hb_conOutErr( szBuffer, 0 );

      fprintf( hLog, "%s\n", szBuffer );

      hb_conOutErr( hb_conNewLine(), 0 );
      hb_snprintf( szBuffer, sizeof( szBuffer ), "WARNING! Memory allocated but not released: %" HB_PFS "u bytes (%" HB_PFS "u blocks)", s_ulMemoryConsumed, s_ulMemoryBlocks );
      hb_conOutErr( szBuffer, 0 );

      fprintf( hLog, "--------------------------------------------------------------------------------\n" );
      fprintf( hLog, "%s\n\n", szBuffer );

      hb_conOutErr( hb_conNewLine(), 0 );

      for( i = 1, pMemBlock = s_pMemBlocks; pMemBlock; ++i, pMemBlock = pMemBlock->pNextBlock )
      {
         char _Error[ 256 ];
         hb_snprintf ( _Error, sizeof(_Error), "%s(%i) %s() Block %i %p (size %" HB_PFS "u) \"%s\"\n", pMemBlock->szSourceFile, pMemBlock->iSourceLine, pMemBlock->szFuncName, i,
                                  ( char * ) pMemBlock + HB_MEMINFO_SIZE, pMemBlock->ulSize,
                                  hb_memToStr( szBuffer, ( char * ) pMemBlock + HB_MEMINFO_SIZE,
                                               pMemBlock->ulSize ) ) ;

         hb_conOutErr( _Error, 0 );
         fprintf( hLog,"%s(%i) %s() Block %i %p (size %" HB_PFS "u) \"%s\"\n", pMemBlock->szSourceFile, pMemBlock->iSourceLine, pMemBlock->szFuncName, i,
                                  ( char * ) pMemBlock + HB_MEMINFO_SIZE, pMemBlock->ulSize,
                                  hb_memToStr( szBuffer, ( char * ) pMemBlock + HB_MEMINFO_SIZE,
                                               pMemBlock->ulSize ));
      }
      fprintf( hLog, "--------------------------------------------------------------------------------\n" );
      fclose( hLog );
   }
#endif
}


/* NOTE: Use as minimal calls from here, as possible.
         Don't allocate memory from this function. */
void hb_errInternal( ULONG ulIntCode, const char * szText, const char * szPar1, const char * szPar2 )
{
   char buffer[ 1024 ];

   HB_TRACE( HB_TR_DEBUG, ( "hb_errInternal(%lu, %s, %s, %s)", ulIntCode, szText, szPar1, szPar2 ) );

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

void hb_conOutErr( const char * pStr, HB_SIZE ulLen )
{
   if( ulLen == 0 )
      ulLen = strlen( pStr );

   fprintf( stderr, "%.*s", ( int ) ulLen, pStr );
   fflush( stderr );
}

void hb_conOutStd( const char * pStr, HB_SIZE ulLen )
{
   if( ulLen == 0 )
      ulLen = strlen( pStr );

   fprintf( stdout, "%.*s", ( int ) ulLen, pStr );
   fflush( stdout );
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
      PHB_FNAME   pFileName;
      HB_SIZE     ulLen;

      if( pszFree )
      {
         szFileName = *pszFree = hb_strncpy( ( char * ) hb_xgrab( HB_PATH_MAX ),
                                             szFileName, HB_PATH_MAX - 1 );
      }

      if( s_cDirSep != HB_OS_PATH_DELIM_CHR )
      {
         char * p = ( char * ) szFileName;
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
            while( ulLen && pFileName->szName[ ulLen - 1 ] == ' ' )
               --ulLen;
            while( ulLen && pFileName->szName[ 0 ] == ' ' )
            {
               ++pFileName->szName;
               --ulLen;
            }
            ( ( char * ) pFileName->szName )[ ulLen ] = '\0';
         }
         if( pFileName->szExtension )
         {
            ulLen = strlen( pFileName->szExtension );
            while( ulLen && pFileName->szExtension[ ulLen - 1 ] == ' ' )
               --ulLen;
            while( ulLen && pFileName->szExtension[ 0 ] == ' ' )
            {
               ++pFileName->szExtension;
               --ulLen;
            }
            ( ( char * ) pFileName->szExtension )[ ulLen ] = '\0';
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

/* ChangeLog CVS Date in YYYYMMDD */
int hb_verSVNDateID( void )
{
   return HB_VER_BUILDDATE;
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

static void hb_setWarnVars( void )
{
   hb_comp_bWarnUnUsedLocals        = TRUE;
   hb_comp_bWarnUnUsedStatics       = TRUE;
   hb_comp_bWarnUnUsedGlobals       = TRUE;
   hb_comp_bWarnUnUsedMemvars       = TRUE;
   hb_comp_bWarnUnUsedFields        = TRUE;
   hb_comp_bWarnUnUsedBlockParams   = TRUE;
}

void hb_compSetCOutput( int iOutput )
{
   switch( iOutput )
   {
      case 0:
         hb_comp_iGenCOutput = HB_COMPGENC_COMPACT;
         break;

      case 1:
         hb_comp_iGenCOutput = HB_COMPGENC_NORMAL;
         break;

      case 2:
         hb_comp_iGenCOutput = HB_COMPGENC_VERBOSE;
         break;

      case 3:
         hb_comp_iGenCOutput = HB_COMPGENC_REALCODE;
         break;

      case 4:
         hb_comp_iGenVarList = TRUE;
         break;

      case 5:
         hb_comp_OutputIsCpp = TRUE;
         break;

      case 6:
         hb_comp_iWarnings = 0;
         break;

      case 7:
         hb_comp_iWarnings = 1;
         break;

      case 8:
         hb_comp_iWarnings = 2;
         hb_setWarnVars();
         break;

      case 9:
         hb_comp_iWarnings = 3;
         hb_setWarnVars();
         _hb_compDeclaredInit();
         break;
   }
}

void hb_compCleanUp( void )
{
   if( hb_comp_PP )
   {
      hb_pp_free( hb_comp_PP );
      hb_comp_PP = NULL;
   }

   hb_compIdentifierClose();

   if ( hb_comp_pFileName )
   {
      hb_xfree( hb_comp_pFileName );
      hb_comp_pFileName = NULL;
   }

   if( hb_Command_Line )
   {
      hb_xfree( hb_Command_Line );
      hb_Command_Line = NULL;
   }

   if( hb_comp_pOutPath )
   {
      hb_xfree( hb_comp_pOutPath );
      hb_comp_pOutPath = NULL;
   }

   if( hb_comp_ppo_pOutPath )
   {
      hb_xfree( hb_comp_ppo_pOutPath );
      hb_comp_ppo_pOutPath = NULL;
   }

   if( hb_comp_PrgFileName )
   {
      hb_xfree( hb_comp_PrgFileName );
      hb_comp_PrgFileName = NULL;
   }

   cleansPair();
}

void hb_compOutStd( char * szMessage )
{
   if( ! hb_comp_bQuiet )
   {
      if( hb_outStdFunc )
         hb_outStdFunc( hb_compHandle, szMessage );
      else {		  
         fprintf( stdout, "%s", szMessage ); fflush( stdout );
   }
}
}

void hb_compOutErr( char * szMessage )
{
   if( hb_outErrFunc )
      hb_outErrFunc( hb_compHandle, szMessage );
   else {
      fprintf( stderr, "%s", szMessage ); fflush( stderr );
}
}


//#ifndef __HB_COMPILER__
#ifndef hb_xgrab
void * hb_xgrab( HB_SIZE ulSize )        /* allocates fixed memory, exits on failure */
{
#ifdef __HB_COMP_TRACE__

   void *   pMem  = malloc( ulSize + HB_MEMINFO_SIZE + sizeof( UINT32 ) );

   if( pMem )
   {
      if( s_pMemBlocks )
         s_pMemBlocks->pPrevBlock = ( PHB_MEMINFO ) pMem;
      ( ( PHB_MEMINFO ) pMem )->pNextBlock   = s_pMemBlocks;
      ( ( PHB_MEMINFO ) pMem )->pPrevBlock   = NULL;
      s_pMemBlocks                           = ( PHB_MEMINFO ) pMem;
      ( ( PHB_MEMINFO ) pMem )->ulSize       = ulSize;
      ( ( PHB_MEMINFO ) pMem )->iSourceLine  = hb_comp_iLine;
      ( ( PHB_MEMINFO ) pMem )->Signature    = HB_MEMINFO_SIGNATURE;
      HB_PUT_LE_UINT32( ( ( BYTE * ) pMem ) + HB_MEMINFO_SIZE + ulSize, HB_MEMINFO_SIGNATURE );

      s_ulMemoryConsumed                     += ulSize;
      if( s_ulMemoryMaxConsumed < s_ulMemoryConsumed )
         s_ulMemoryMaxConsumed = s_ulMemoryConsumed;
      s_ulMemoryBlocks++;
      if( s_ulMemoryMaxBlocks < s_ulMemoryBlocks )
         s_ulMemoryMaxBlocks = s_ulMemoryBlocks;
      pMem = ( BYTE * ) pMem + HB_MEMINFO_SIZE;
   }
#else
   void *   pMem  = malloc( ( size_t ) ulSize );
#endif

   if( ! pMem )
   {
      char szSize[ 32 ];

      hb_snprintf( szSize, sizeof( szSize ), "%lu", (long unsigned int) ulSize );
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMALLOC, szSize, NULL );
   }

   return pMem;
}
#endif

#ifndef hb_xrealloc
void * hb_xrealloc( void * pMem, HB_SIZE ulSize )       /* reallocates memory */
{
#ifdef __HB_COMP_TRACE__
   PHB_MEMINFO pMemBlock;
   ULONG       ulMemSize;
   void *      pResult;

   if( ulSize == 0 )
   {
      if( pMem )
         hb_xfree( pMem );
      return NULL;
   }
   else if( ! pMem )
      return hb_xgrab( ulSize );

   pMemBlock   = ( PHB_MEMINFO ) ( ( BYTE * ) pMem - HB_MEMINFO_SIZE );
   ulMemSize   = pMemBlock->ulSize;

   if( pMemBlock->Signature != HB_MEMINFO_SIGNATURE )
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMCORRUPT, NULL, NULL );

   if( HB_GET_LE_UINT32( ( ( BYTE * ) pMem ) + ulMemSize ) != HB_MEMINFO_SIGNATURE )
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMOVERFLOW, NULL, NULL );

   HB_PUT_LE_UINT32( ( ( BYTE * ) pMem ) + ulMemSize, 0 );

   pResult  = realloc( pMemBlock, ulSize + HB_MEMINFO_SIZE + sizeof( UINT32 ) );

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
      pResult                             = ( BYTE * ) pResult + HB_MEMINFO_SIZE;
   }
#else
   void *   pResult  = realloc( pMem, ( size_t ) ulSize );
#endif

   if( ! pResult && ulSize )
   {
      char szSize[ 32 ];

      hb_snprintf( szSize, sizeof( szSize ), "%lu", (long unsigned int) ulSize );
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
#ifdef __HB_COMP_TRACE__
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
      pMem                 = ( BYTE * ) pMem - HB_MEMINFO_SIZE;
#endif

      free( pMem );
   }
   else
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMFREE, NULL, NULL );
}
#endif

#ifndef hb_xexit
void hb_xexit( void )
{
#ifdef __HB_COMP_TRACE__
   if( s_ulMemoryBlocks /* || hb_cmdargCheck( "INFO" ) */ )
   {
      char        szBuffer[ HB_MAX( 3 * HB_MEMSTR_BLOCK_MAX + 1, 100 ) ];
      PHB_MEMINFO pMemBlock;
      int         i;
      FILE *      hLog = hb_fopen( "fm.log", "a+" );

      if( hLog )
      {
         char *   szPlatform  = hb_verPlatform();
         char *   szCompiler  = hb_verCompiler();
         char *   szHarbour   = hb_verHarbour();
         char *   szTime      = ( char * ) hb_xgrab( 9 );
         int      iYear, iMonth, iDay;

         hb_dateToday( &iYear, &iMonth, &iDay );
         hb_dateTimeStr( szTime );

         fprintf( hLog, "Memory Allocation Report\n" );
         fprintf( hLog, "Application: %s\n", hb_exeName() );
         fprintf( hLog, "xHarbour Version: %s\n", szHarbour );
         fprintf( hLog, "Compiler: %s\n", szCompiler );
         fprintf( hLog, "Platform: %s\n", szPlatform );
         fprintf( hLog, "Time Occured: %04d.%02d.%02d %s\n", iYear, iMonth, iDay, szTime );

         hb_xfree( szPlatform );
         hb_xfree( szCompiler );
         hb_xfree( szHarbour  );
         hb_xfree( szTime );
      }

      hb_conOutErr( hb_conNewLine(), 0 );
      hb_conOutErr( "----------------------------------------", 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
      hb_snprintf( szBuffer, sizeof( szBuffer ), "Total memory allocated: %" HB_PFS "u bytes (%" HB_PFS "u blocks)", s_ulMemoryMaxConsumed, s_ulMemoryMaxBlocks );
      hb_conOutErr( szBuffer, 0 );

      fprintf( hLog, "%s\n", szBuffer );

      hb_conOutErr( hb_conNewLine(), 0 );
      hb_snprintf( szBuffer, sizeof( szBuffer ), "WARNING! Memory allocated but not released: %" HB_PFS "u bytes (%" HB_PFS "u blocks)", s_ulMemoryConsumed, s_ulMemoryBlocks );
      hb_conOutErr( szBuffer, 0 );

      fprintf( hLog, "--------------------------------------------------------------------------------\n" );
      fprintf( hLog, "%s\n\n", szBuffer );

      hb_conOutErr( hb_conNewLine(), 0 );

      for( i = 1, pMemBlock = s_pMemBlocks; pMemBlock; ++i, pMemBlock = pMemBlock->pNextBlock )
      {
         HB_TRACE( HB_TR_ERROR, ( "Line %i Block %i %p (size %" HB_PFS "u) \"%s\"", pMemBlock->iSourceLine - 1, i,
                                  ( char * ) pMemBlock + HB_MEMINFO_SIZE, pMemBlock->ulSize,
                                  hb_memToStr( szBuffer, ( char * ) pMemBlock + HB_MEMINFO_SIZE,
                                               pMemBlock->ulSize ) ) );
         fprintf( hLog,"Block %i %p (size %" HB_PFS "u) \"%s\"\n", i,
                                  ( char * ) pMemBlock + HB_MEMINFO_SIZE, pMemBlock->ulSize,
                                  hb_memToStr( szBuffer, ( char * ) pMemBlock + HB_MEMINFO_SIZE,
                                               pMemBlock->ulSize ));
      }
      fprintf( hLog, "--------------------------------------------------------------------------------\n" );
      fclose( hLog );
   }
#endif
}
#endif
//#endif
