/*
 * $Id: runner.c,v 1.31 2004/08/14 07:57:54 ronpinkas Exp $
 */

/*
 * Harbour Project source code:
 * Harbour Portable Object (.HRB) file runner
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 * www - http://www.harbour-project.org
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
/*
 * The following functions are added Feb 01,2002 by
 *       Alexander Kresin <alex@belacy.belgorod.su>
 *
 *  __HRBLOAD()
 *  __HRBDO()
 *  __HRBUNLOAD()
 *  __HRBGETFU()
 *  __HRBDOFU()
 */

#include "hbapi.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbvm.h"
#include "hbpcode.h"
#include "hb_io.h"

// extern HB_EXPORT PSYMBOLS hb_vmLastModule( void );

/* TODO: Fill the error codes with valid ones (instead of 9999) */
/* TOFIX: Change this assembler hack to something standard and portable */
/* TOFIX: Fix the memory leak on error. */

/* NOTE: This is the assembler output from : hb_vmExecute( pcode, symbols ).  */

/* #if INTEL32 */

static BYTE prgFunction[] =
{
   0x68, 0x00, 0x00, 0x00, 0x00,  /* push offset Globals             */
   0x68, 0x00, 0x00, 0x00, 0x00,  /* push offset symbols             */
   0x68, 0x00, 0x00, 0x00, 0x00,  /* push offset pcode               */
   0xE8, 0x00, 0x00, 0x00, 0x00,  /* call near relative hb_vmExecute */
   0x83, 0xC4, 0x0C,              /* add esp, 12                     */
   0xC3                           /* ret near                        */
};

/* #elseif INTEL16 */
/* #elseif MOTOROLA */
/* #elseif ... */
/* #endif */

typedef union
{
   BYTE *   pAsmData;                           /* The assembler bytes      */
   PHB_FUNC pFunPtr;                            /* The (dynamic) harbour
                                                   function                 */
} ASM_CALL, * PASM_CALL;

typedef struct
{
   char *     szName;                           /* Name of the function     */
   PASM_CALL  pAsmCall;                         /* Assembler call           */
   BYTE *     pCode;                            /* P-code                   */
} HB_DYNF, * PHB_DYNF;

typedef struct
{
   ULONG ulSymbols;                             /* Number of symbols        */
   ULONG ulFuncs;                               /* Number of functions      */
   BOOL fInit;                                  /* should be INIT functions executed */
   BOOL fExit;                                  /* should be EXIT functions executed */
   LONG ulSymStart;                             /* Startup Symbol           */
   PHB_SYMB pSymRead;                           /* Symbols read             */
   PHB_DYNF pDynFunc;                           /* Functions read           */
   PSYMBOLS pPrevLastModule;
} HRB_BODY, * PHRB_BODY;


#define SYM_NOLINK  0                           /* Symbol does not have to
                                                                  be linked */
#define SYM_FUNC    1                           /* Defined function         */
#define SYM_EXTERN  2                           /* Prev. defined function   */

#define SYM_NOT_FOUND 0xFFFFFFFF                /* Symbol not found.
                                                   FindSymbol               */
HB_EXTERN_BEGIN
HB_EXPORT PASM_CALL hb_hrbAsmCreateFun( PHB_SYMB pSymbols, BYTE * pCode ); /* Create a dynamic function*/
HB_EXPORT PHRB_BODY hb_hrbLoad( char* szHrbBody, ULONG ulBodySize );
HB_EXPORT PHRB_BODY hb_hrbLoadFromFile( char* szHrb );
HB_EXPORT void hb_hrbDo( PHRB_BODY pHrbBody, int argc, char * argv[] );
HB_EXPORT void hb_hrbUnLoad( PHRB_BODY pHrbBody );
HB_EXPORT void hb_hrbAsmPatch( BYTE * pCode, ULONG ulOffset, void * Address );
HB_EXPORT void hb_hrbAsmPatchRelative( BYTE * pCode, ULONG ulOffset, void * Address, ULONG ulNext );
HB_EXTERN_END

static void hb_hrbInit( PHRB_BODY pHrbBody, int argc, char * argv[] );

static ULONG     s_ulSymEntry = 0;              /* Link enhancement         */

/*
   __HRBRUN( <cFile> [, xParam1 [, xParamN ] ] ) -> return value.

   This program will get the data from the .HRB file and run the p-code
   contained in it.

   In due time it should also be able to collect the data from the
   binary/executable itself
*/

HB_FUNC( __HRBRUN )
{
   int argc = hb_pcount();

   if( argc >= 1 )
   {
      PHRB_BODY pHrbBody = hb_hrbLoadFromFile( hb_parcx( 1 ) );

      if( pHrbBody )
      {
         char **argv = NULL;
         int i;

         if( argc > 1 )
         {
            argv = (char**) hb_xgrab( sizeof(char*) * (argc-1) );

            for( i=0; i<argc-1; i++ )
            {
               argv[i] = hb_parcx( i+2 );
            }
         }

         hb_hrbDo( pHrbBody, argc-1, argv );

         if( argv )
         {
            hb_xfree( argv );
         }

         hb_retl( 1 );

         hb_hrbUnLoad( pHrbBody );
      }
      else
      {
         hb_retl( 0 );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBRUN", 0 );
   }
}

HB_FUNC( __HRBLOAD )
{
   int argc = hb_pcount();

   if( argc >= 1 )
   {
      BYTE szHead[] = { (BYTE)192,'H','R','B' };
      char * fileOrBody = hb_parc( 1 );
      PHRB_BODY pHrbBody;

      /* If parameter string */
      if ( fileOrBody && hb_parclen( 1 ) > 4 && strncmp( ( char * ) szHead, ( char * ) fileOrBody, 4 ) == 0  )
      {
         pHrbBody = hb_hrbLoad( fileOrBody, hb_parclen( 1 ) );
      }
      else
      {
         pHrbBody = hb_hrbLoadFromFile( fileOrBody );
      }
      if ( pHrbBody )
      {
         char **argv = NULL;
         int i;
         
         if( argc > 1 )
         {
            argv = (char**) hb_xgrab( sizeof(char*) * (argc-1) );

            for( i=0; i<argc-1; i++ )
            {
               argv[i] = hb_parcx( i+2 );
            }
         }

         hb_hrbInit( pHrbBody, argc-1, argv );

         if( argv )
         {
            hb_xfree( argv );
         }
      }
      hb_retptr( ( void *) pHrbBody );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 9998, NULL, "__HRBLOAD", 0 );
   }
}

HB_FUNC( __HRBDO )
{
   int argc = hb_pcount();

   if( argc >= 1 )
   {
      int i;
      char **argv = NULL;
      PHRB_BODY pHrbBody = (PHRB_BODY) hb_parptr( 1 );

      if( pHrbBody )
      {
         if( argc > 1 )
         {
            argv = (char**) hb_xgrab( sizeof(char*) * (argc-1) );

            for( i=0; i<argc-1; i++ )
            {
               argv[i] = hb_parcx( i+2 );
            }
         }

         hb_hrbDo( pHrbBody, argc-1, argv );

         if( argv )
         {
            hb_xfree( argv );
         }
      }
      else
      {
         hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBDO", 0 );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBDO", 0 );
   }
}

HB_FUNC( __HRBUNLOAD )
{
   if( hb_pcount() >= 1 )
   {
      hb_hrbUnLoad( (PHRB_BODY) hb_parptr( 1 ) );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBUNLOAD", 0 );
   }
}

HB_FUNC( __HRBGETFU )
{
   if( hb_pcount() > 1 && ISPOINTER( 1 ) && ISCHAR( 2 ) )
   {
      PHRB_BODY pHrbBody = (PHRB_BODY) hb_parptr( 1 );
      ULONG ulPos = 0;

      if( pHrbBody )
      {
         char * szName = hb_strupr( hb_strdup( hb_parcx( 2 ) ) );

         while( ulPos < pHrbBody->ulSymbols )
         {
            if( !strcmp( szName, pHrbBody->pSymRead[ ulPos ].szName ) )
            {
               break;
            }

            ulPos++;
         }

         if( ulPos < pHrbBody->ulSymbols )
         {
            hb_retptr( ( void *) ( pHrbBody->pSymRead + ulPos ) );
         }
         else
         {
            hb_retptr( NULL );
         }

         hb_xfree( szName );
      }
      else
      {
         hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBGETFU", 0 );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBGETFU", 0 );
   }
}

HB_FUNC( __HRBDOFU )
{
   int argc = hb_pcount();

   if( argc >=1 )
   {
      int i;
      PHB_SYMB pSym = (PHB_SYMB) hb_parptr( 1 );

      if( pSym )
      {
         hb_vmPushSymbol( pSym );
         hb_vmPushNil();

         for( i = 0; i < argc-1; i++ ) /* Push other  params  */
         {
            hb_vmPush( hb_param( i + 2, HB_IT_ANY ) );
         }

         hb_vmDo( argc-1 );            /* Run function        */
      }
      else
      {
         hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBDOFU", 0 );
      }
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBDOFU", 0 );
   }
}


static ULONG hb_hrbFindSymbol( char * szName, PHB_DYNF pDynFunc, ULONG ulLoaded )
{
   ULONG ulRet;

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbFindSymbol(%s, %p, %lu)", szName, pDynFunc, ulLoaded));

   if( ( s_ulSymEntry < ulLoaded ) &&  /* Is it a normal list ? */ ! strcmp( szName, pDynFunc[ s_ulSymEntry ].szName ) )
   {
      ulRet = s_ulSymEntry++;
   }
   else
   {
      BOOL bFound = FALSE;

      ulRet = 0;

      while( ! bFound && ulRet < ulLoaded )
      {
         if( ! strcmp( szName, pDynFunc[ ulRet ].szName ) )
         {
            bFound = TRUE;
         }
         else
         {
            ulRet++;
         }
      }

      if( !bFound )
      {
         ulRet = SYM_NOT_FOUND;
      }
   }

   return ulRet;
}

static int hb_hrbReadHead( char * szBody, ULONG ulBodySize, ULONG * ulBodyOffset )
{
   BYTE szHead[] = { (BYTE)192,'H','R','B' };
   char cInt[ 2 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbReadHead(%p,%i,%i)", szBody, ulBodySize, * ulBodyOffset ));

   if( ulBodySize < 6 || strncmp( ( char * ) szHead, ( char * ) szBody, 4 ) )
   {
      hb_errRT_BASE( EG_CORRUPTION, 9999, NULL, "__HRBLOAD", 0 );
      return 0;
   }

   cInt[0] = szBody[(*ulBodyOffset)+4];
   cInt[1] = szBody[(*ulBodyOffset)+5];

   * ulBodyOffset += 6;    // header + version offset

   return HB_PCODE_MKSHORT( cInt );
}

/* ReadId
   Read the next (zero terminated) identifier */
static char * hb_hrbReadId( char * szBody, BOOL bUseFM, ULONG ulBodySize, ULONG * ulBodyOffset )
{
   char * szIdx;
   char * szRet;

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbReadId(%p,%i,%i,%i)", szBody, bUseFM, ulBodySize, *ulBodyOffset));

   szIdx = &szBody[ *ulBodyOffset ];

   do
   {
      if ( *ulBodyOffset > ulBodySize )
      {
         hb_errRT_BASE( EG_CORRUPTION, 9999, NULL, "__HRBLOAD", 0 );
         return "";
      }
   }
   while( szBody[ ( *ulBodyOffset )++ ] );

   if( bUseFM )
   {
      szRet = ( char * ) hb_xgrab( &szBody[ *ulBodyOffset ] - szIdx );
   }
   else
   {
      /*
         Intentionally using malloc() instead of hb_xgrab() to "mask" unavoidable "leak" because:
         1. Symbols MUST remain for the duration of the App, or Dynamic Symbol Table will become
            a GPF trap when attrmpting to access such no longer existing Symbol.
         2. It seems that it's not worth keeping track of each unloaded module symbols, since
            release can only take place at app termination as per #1 above.
      */
      szRet = ( char * ) malloc( &szBody[ *ulBodyOffset ] - szIdx );
   }

   return strcpy( szRet, szIdx );
}


static LONG hb_hrbReadLong( char * szBody, ULONG ulBodySize, ULONG * ulBodyOffset )
{
   char cLong[ 4 ];                               /* Temporary long           */

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbReadLong(%p,%i,%i)", szBody, ulBodySize, * ulBodyOffset));

   if ( (* ulBodyOffset + 4) > ulBodySize )
   {
      hb_errRT_BASE( EG_CORRUPTION, 9999, NULL, "__HRBLOAD", 0 );
      return 0;
   }

   memcpy( cLong, (char *) (szBody+(*ulBodyOffset)), 4 );

   * ulBodyOffset += 4;

   if( cLong[ 3 ] )                             /* Convert to long if ok    */
   {
      hb_errRT_BASE( EG_CORRUPTION, 9999, NULL, "__HRBLOAD", 0 );
      return 0;
   }
   else
   {
      return HB_PCODE_MKLONG( cLong );
   }
}

static void hb_hrbInitStatic( PHRB_BODY pHrbBody )
{
   if ( ! pHrbBody->fInit && ! pHrbBody->fExit )
   {
      ULONG ul;

      pHrbBody->fInit = TRUE;
      /* Initialize static variables first */
      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )    /* Check _INITSTATICS functions */
      {
         if( ( pHrbBody->pSymRead[ ul ].cScope & HB_FS_INITEXIT ) == HB_FS_INITEXIT )
         {
            /* call (_INITSTATICS) function. This function assigns
             * literal values to static variables only. There is no need
             * to pass any parameters to this function because they
             * cannot be used to initialize static variable.
             */
            pHrbBody->pSymRead[ ul ].value.pFunPtr();
         }
      }
   }
}

static void hb_hrbInit( PHRB_BODY pHrbBody, int argc, char * argv[] )
{
   if ( pHrbBody->fInit )
   {
      ULONG ul;
      int i;

      pHrbBody->fInit = FALSE;
      pHrbBody->fExit = TRUE;

      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )    /* Check INIT functions */
      {
         if( ( pHrbBody->pSymRead[ ul ].cScope & HB_FS_INITEXIT ) == HB_FS_INIT )
         {
            hb_vmPushSymbol( pHrbBody->pSymRead + ul );
            hb_vmPushNil();
            for( i = 0; i < argc; i++ ) /* Push other cmdline params*/
               hb_vmPushString( argv[i],strlen(argv[i]) );

            hb_vmDo( argc );            /* Run init function        */
         }
      }
   }
}

static void hb_hrbExit( PHRB_BODY pHrbBody )
{
   if ( pHrbBody->fExit )
   {
      ULONG ul;

      pHrbBody->fExit = FALSE;
      pHrbBody->fInit = TRUE;

      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )    /* Check EXIT functions     */
      {
         if( ( pHrbBody->pSymRead[ ul ].cScope & HB_FS_INITEXIT ) == HB_FS_EXIT )
         {
            hb_vmPushSymbol( pHrbBody->pSymRead + ul );
            hb_vmPushNil();
            hb_vmDo( 0 );                   /* Run exit function        */
            pHrbBody->pSymRead[ ul ].cScope = pHrbBody->pSymRead[ ul ].cScope & ( ~HB_FS_EXIT );
                                            /* Exit function cannot be
                                               handled by main in hvm.c */
         }
      }
   }
}

void hb_hrbUnLoad( PHRB_BODY pHrbBody )
{
   ULONG ul;

   hb_hrbExit( pHrbBody );

   if( pHrbBody->pPrevLastModule && pHrbBody->pPrevLastModule->pNext )
   {
      hb_xfree( pHrbBody->pPrevLastModule->pNext->szModuleName );

      // Release PSYMBOLS of our module.
      hb_xfree( pHrbBody->pPrevLastModule->pNext );

      // Reset
      pHrbBody->pPrevLastModule->pNext = NULL;
   }

   for( ul = 0; ul < pHrbBody->ulFuncs; ul++ )
   {
      PHB_DYNS pDyn;

      hb_dynsymLock();
      pDyn = hb_dynsymFind( pHrbBody->pDynFunc[ ul ].szName );

      if( pDyn && pDyn->pFunPtr && pDyn->pFunPtr == pHrbBody->pDynFunc[ ul ].pAsmCall->pFunPtr )
      {
         //printf( "Reset >%s<\n", pHrbBody->pDynFunc[ ul ].szName );

         pDyn->pFunPtr = NULL;
         pDyn->pSymbol->value.pFunPtr = NULL;
      }
      hb_dynsymUnlock();

      hb_xfree( pHrbBody->pDynFunc[ ul ].pAsmCall->pAsmData );
      hb_xfree( pHrbBody->pDynFunc[ ul ].pAsmCall );
      hb_xfree( pHrbBody->pDynFunc[ ul ].pCode );
      hb_xfree( pHrbBody->pDynFunc[ ul ].szName );
   }

   //printf( "Done with Functions\n" );

   /* We can NOT release the symbols or else the Dynamic-Symbols-Table will get corrupted!!!
    * The list as whole is NOT a candidate for possible recycling because the future hrb may be modified!
    * It is possible to recycle individual symbols but I'm not sure it's worth the efforts.
   for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )
   {
      //printf( "# %i/%i Freeing: >%s<\n", ul, pHrbBody->ulSymbols, pHrbBody->pSymRead[ ul ].szName );

      hb_xfree( pHrbBody->pSymRead[ ul ].szName );
   }
   hb_xfree( pHrbBody->pSymRead );
   */

   hb_xfree( pHrbBody->pDynFunc );
   hb_xfree( pHrbBody );
}

PHRB_BODY hb_hrbLoad( char* szHrbBody, ULONG ulBodySize )
{
   PHRB_BODY pHrbBody = NULL;
   ULONG ulBodyOffset = 0;

   if( szHrbBody )
   {
      ULONG ulSize;                                /* Size of function         */
      ULONG ul, ulPos;

      PHB_SYMB pSymRead;                           /* Symbols read             */
      PHB_DYNF pDynFunc;                           /* Functions read           */
      PHB_DYNS pDynSym;
      int nVersion = hb_hrbReadHead( (char *) szHrbBody, (ULONG) ulBodySize, &ulBodyOffset );

      if( !nVersion )
      {
         return NULL;
      }

      pHrbBody = ( PHRB_BODY ) hb_xgrab( sizeof( HRB_BODY ) );

      pHrbBody->fInit = FALSE;
      pHrbBody->fExit = FALSE;
      pHrbBody->ulSymStart = -1;
      pHrbBody->ulSymbols = hb_hrbReadLong( (char *) szHrbBody, ulBodySize, &ulBodyOffset );
      pHrbBody->pPrevLastModule = NULL;

      /*
         Intentionally using malloc() instead of hb_xgrab() to "mask" unavoidable "leak" because:

         1. Symbols MUST remain for the duration of the App, or Dynamic Symbol Table will become
            a GPF trap when attrmpting to access such no longer existing Symbol.

         2. It seems that it's not worth keeping track of each unloaded module symbols, since
            release can only take place at app termination as per #1 above.
      */
      pSymRead = ( PHB_SYMB ) malloc( pHrbBody->ulSymbols * sizeof( HB_SYMB ) );

      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )  /* Read symbols in .HRB     */
      {
         pSymRead[ ul ].szName  = hb_hrbReadId( (char *) szHrbBody, FALSE, ulBodySize, &ulBodyOffset );
         //printf( "at offset %i, found symbol %s, scope %i, type %i\n", ulBodyOffset, pSymRead[ ul ].szName, szHrbBody[ulBodyOffset], szHrbBody[ulBodyOffset+1] );
         pSymRead[ ul ].cScope  = szHrbBody[ulBodyOffset++];
         pSymRead[ ul ].value.pFunPtr = ( PHB_FUNC ) ( HB_PTRDIFF ) szHrbBody[ulBodyOffset++];
         pSymRead[ ul ].pDynSym = NULL;

         if ( pHrbBody->ulSymStart == -1 && pSymRead[ ul ].cScope & HB_FS_FIRST && ! ( pSymRead[ ul ].cScope & HB_FS_INITEXIT ) )
         {
            pHrbBody->ulSymStart = ul;
         }
      }

      pHrbBody->ulFuncs = hb_hrbReadLong( (char *) szHrbBody, ulBodySize, &ulBodyOffset );  /* Read number of functions */

      pDynFunc = ( PHB_DYNF ) hb_xgrab( pHrbBody->ulFuncs * sizeof( HB_DYNF ) );
      memset( pDynFunc, 0, pHrbBody->ulFuncs * sizeof( HB_DYNF ) );

      for( ul = 0; ul < pHrbBody->ulFuncs; ul++ )         /* Read symbols in .HRB     */
      {
         pDynFunc[ ul ].szName = hb_hrbReadId( (char *) szHrbBody, TRUE, ulBodySize, &ulBodyOffset );

         ulSize = hb_hrbReadLong( (char *) szHrbBody, ulBodySize, &ulBodyOffset );      /* Read size of function    */
         pDynFunc[ ul ].pCode = ( BYTE * ) hb_xgrab( ulSize );

         /* Read the block           */
         memcpy( ( char * ) pDynFunc[ ul ].pCode, (char *) (szHrbBody + ulBodyOffset), ulSize );
         ulBodyOffset += ulSize;

        /* Create matching dynamic function */
         pDynFunc[ ul ].pAsmCall = hb_hrbAsmCreateFun( pSymRead, pDynFunc[ ul ].pCode );

         //printf( "#%i/%i Sym: >%s<, pAsm: %p, offset %i\n", ul, pHrbBody->ulFuncs, pDynFunc[ ul ].szName, pDynFunc[ ul ].pAsmCall, ulBodyOffset );
      }

      pHrbBody->pSymRead = pSymRead;
      pHrbBody->pDynFunc = pDynFunc;
      s_ulSymEntry = 0;

      for( ul = 0; ul < pHrbBody->ulSymbols; ul++ )    /* Linker                   */
      {
         //printf( "linking #%i/%i Sym: >%s<\n", ul, pHrbBody->ulSymbols, pSymRead[ ul ].szName );

         if( ( ( HB_PTRDIFF ) pSymRead[ ul ].value.pFunPtr ) == SYM_FUNC )
         {
            ulPos = hb_hrbFindSymbol( pSymRead[ ul ].szName, pDynFunc, pHrbBody->ulFuncs );

            //printf( "Pos: %i\n", ulPos );

            if( ulPos == SYM_NOT_FOUND )
            {
               pSymRead[ ul ].value.pFunPtr = ( PHB_FUNC ) SYM_EXTERN;
            }
            else
            {
               /* Exists and NOT static ?  */
               /*
               if( hb_dynsymFind( pSymRead[ ul ].szName ) && ! ( pSymRead[ ul ].cScope & HB_FS_STATIC ) )
               {
                  hb_errRT_BASE( EG_ARG, 9999, "Duplicate symbol", pSymRead[ ul ].szName );
                  hb_hrbUnLoad( pHrbBody );
                  pHrbBody = NULL;
                  break;
               }
               */
               pSymRead[ ul ].value.pFunPtr = pDynFunc[ ulPos ].pAsmCall->pFunPtr;
            }
         }

         /* External function        */
         if( ( ( HB_PTRDIFF ) pSymRead[ ul ].value.pFunPtr ) == SYM_EXTERN )
         {
            //printf( "External\n" );

            hb_dynsymLock();
            pDynSym = hb_dynsymFind( pSymRead[ ul ].szName );

            //printf( "Found: %p\n", pDynSym );

            if( pDynSym )
            {
               pSymRead[ ul ].value.pFunPtr = pDynSym->pFunPtr;
               hb_dynsymUnlock();
            }
            else
            {
               char szName[21];

               hb_dynsymUnlock();

               strncpy( szName, pSymRead[ ul ].szName, 20 );

               hb_hrbUnLoad( pHrbBody );
               pHrbBody = NULL;
               hb_errRT_BASE( EG_ARG, 9999, "Unknown or unregistered symbol", szName, 0 );
               break;
            }
         }
      }

      if( pHrbBody )
      {
         pHrbBody->pPrevLastModule = hb_vmLastModule();

//         hb_vmProcessSymbols( pHrbBody->pSymRead, ( USHORT ) pHrbBody->ulSymbols, szFileName, (int) HB_PCODE_VER );
         hb_vmProcessSymbols( pHrbBody->pSymRead, ( USHORT ) pHrbBody->ulSymbols, "PCODE_HRB_FILE.hrb", (int) HB_PCODE_VER );

         /*
          * initialize static variables
          * so far there is no method to deinitialize static vars and
          * free the place in static variable array :-(
          * so when program load and unloads modules with static vars
          * each time the new area for statics is allocated and we are
          * losing memory. It is exactly: number_of_statics * sizeof(HB_ITEM)
          * To resolve this problem we have to find a way to free the statics
          * when module is unloaded. We know the offset to the statics array
          * (it is stored in pSymbol->value.iStaticsBase) but we do not know
          * the number of static vars. We will have to retrieve it from VM
          * after statics registration and store somewhere. Then when module
          * is unloaded we should call VM function to mark n items from given
          * offset as unused for future reuse.
          * The second thing which have to be changed is the method of freeing
          * symbol table - now we have next memory leak in repeated module
          * load/unload process. It could be also eliminated.
          * We should simply use existing symbols or if they not exist create
          * new one by hb_symbolNew() and use our own function instead of
          * hb_vmProcessSymbols() In fact it will be enough to calling the
          * hb_dynsymGet() instead of hb_dynsymFind()/hb_dynsymNew(). It seems
          * to be enough but I would like to know what bad will happen when
          * we register hrb module without pModuleSymbols. Which functionality
          * we will lose.
          * And there will be a problem with codeblocks created in .hrb
          * module and still active after unloading it - they may access
          * pModuleSymbols when evaluated. This is much more serious
          * problem :-( Here I do not see any easy solution. We have the
          * following choices:
          *   1. The pModuleSymbols will be freed by garbage collector
          *   2. We will remember all allocated pModuleSymbols here
          *      and will try to reuse it when the same module is loaded
          *      again.
          *   3. User will not create codeblocks in .hrb modules and then
          *      try to use it when module is unloaded - I do not trust
          *      them ;-)
          * [Druzus]
          */
         hb_hrbInitStatic( pHrbBody );
      }
   }

   return pHrbBody;
}

PHRB_BODY hb_hrbLoadFromFile( char* szHrb )
{
   char szFileName[ _POSIX_PATH_MAX + 1 ];
   PHRB_BODY pHrbBody = NULL;
   PHB_FNAME pFileName;
   FHANDLE file;

   /* Create full filename */

   pFileName = hb_fsFNameSplit( szHrb );

   if( ! pFileName->szExtension )
   {
      pFileName->szExtension = ".hrb";
   }

   hb_fsFNameMerge( szFileName, pFileName );

   hb_xfree( pFileName );

   /* Open as binary */

   while ( ( file = hb_fsOpen( ( BYTE *)szFileName, FO_READ )) == 0 )
   {
      USHORT uiAction = hb_errRT_BASE_Ext1( EG_OPEN, 9999, NULL, szFileName, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY, 1, hb_paramError( 1 ) );

      if( uiAction == E_DEFAULT || uiAction == E_BREAK )
      {
         break;
      }
   }

   if (file)
   {
      ULONG ulBodySize = hb_fsSeek( file, 0, FS_END );

      if( ulBodySize )
      {
         BYTE * pbyBuffer;

         pbyBuffer = ( BYTE * ) hb_xgrab( ulBodySize + sizeof( char ) + 1 );
         hb_fsSeek( file, 0, FS_SET );
         hb_fsReadLarge( file, pbyBuffer, ulBodySize );
         pbyBuffer[ ulBodySize ] = '\0';

         pHrbBody = hb_hrbLoad( (char*) pbyBuffer, (ULONG) ulBodySize );

         hb_xfree( pbyBuffer );
      }
      hb_fsClose( file );
   }
   return( pHrbBody );
}

void hb_hrbDo( PHRB_BODY pHrbBody, int argc, char * argv[] )
{
   PHB_ITEM pRetVal = NULL;
   int i;

   hb_hrbInit( pHrbBody, argc, argv );

   /* May not have a startup symbol, if first symbol was an INIT Symbol (was executed already).*/
   if ( pHrbBody->ulSymStart >= 0 )
   {
       hb_vmPushSymbol( &( pHrbBody->pSymRead[ pHrbBody->ulSymStart ] ) );
       hb_vmPushNil();

       for( i = 0; i < ( hb_pcount() - 1 ); i++ )
       {
          hb_vmPush( hb_param( i + 2, HB_IT_ANY ) ); /* Push other cmdline params*/
       }

       hb_vmDo( hb_pcount() - 1 );                   /* Run the thing !!!        */

       pRetVal = hb_itemNew( NULL );
       hb_itemCopy( pRetVal, &(HB_VM_STACK.Return) );
   }

   hb_hrbExit( pHrbBody );

   if( pRetVal )
   {
      hb_itemRelease( hb_itemReturn( pRetVal ) );
   }
}

/*
   Create dynamic function.

   This function is needed, since it will allow the existing strategy of
   function pointers to work properly.

   For each Harbour function a little program calling the virtual machine
   should be present (see : *.c)

   Since these programs no longer exists when using this system, they should
   be create dynamically at run-time.

   If a .PRG contains 10 functions, 10 dynamic functions are created which
   are all the same :-) except for 2 pointers.
*/
HB_EXPORT PASM_CALL hb_hrbAsmCreateFun( PHB_SYMB pSymbols, BYTE * pCode )
{
   PASM_CALL asmRet;

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbAsmCreateFun(%p, %p)", pSymbols, pCode));

   asmRet = ( PASM_CALL ) hb_xgrab( sizeof( ASM_CALL ) );
   asmRet->pAsmData = ( BYTE * ) hb_xgrab( sizeof( prgFunction ) );
   memcpy( asmRet->pAsmData, prgFunction, sizeof( prgFunction ) );
                                              /* Copy new assembler code in */
/* #if INTEL32 */

   //hb_hrbAsmPatch( asmRet->pAsmData, 1, NULL );       /* Insert pointer to globals */
   hb_hrbAsmPatch( asmRet->pAsmData, 6, pSymbols );   /* Insert pointer to symbols */
   hb_hrbAsmPatch( asmRet->pAsmData, 11, pCode );      /* Insert pointer to pcode */
   hb_hrbAsmPatchRelative( asmRet->pAsmData, 16, ( void * ) hb_vmExecute, 20 );
                                      /* Insert pointer to hb_vmExecute() */

/* #elseif INTEL16 */
/* #elseif MOTOROLA */
/* #elseif ... */
/* #endif */
   return asmRet;
}

/* Patch an address of the dynamic function */
void hb_hrbAsmPatch( BYTE * pCode, ULONG ulOffset, void * Address )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_hrbAsmPatch(%p, %lu, %p)", pCode, ulOffset, Address));

/* #if 32 bits and low byte first */

   pCode[ ulOffset     ] = ( BYTE ) ( ( ( ULONG ) Address       ) & 0xFF );
   pCode[ ulOffset + 1 ] = ( BYTE ) ( ( ( ULONG ) Address >>  8 ) & 0xFF );
   pCode[ ulOffset + 2 ] = ( BYTE ) ( ( ( ULONG ) Address >> 16 ) & 0xFF );
   pCode[ ulOffset + 3 ] = ( BYTE ) ( ( ( ULONG ) Address >> 24 ) & 0xFF );

/* #elseif 16 bits and low byte first */
/* #elseif 32 bits and high byte first */
/* #elseif ... */
/* #endif */
}


/* Intel specific ?? Patch an address relative to the next instruction */
void hb_hrbAsmPatchRelative( BYTE * pCode, ULONG ulOffset, void * Address, ULONG ulNext )
{
   ULONG ulBase;
   ULONG ulRelative;

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbAsmPatchRelative(%p, %lu, %p, %lu)", pCode, ulOffset, Address, ulNext));

/* #if 32 bits and low byte first */
   ulBase = ( ULONG ) pCode + ulNext;
                                /* Relative to next instruction */
   ulRelative = ( ULONG ) Address - ulBase;

   pCode[ ulOffset     ] = ( BYTE ) ( ( ulRelative       ) & 0xFF );
   pCode[ ulOffset + 1 ] = ( BYTE ) ( ( ulRelative >>  8 ) & 0xFF );
   pCode[ ulOffset + 2 ] = ( BYTE ) ( ( ulRelative >> 16 ) & 0xFF );
   pCode[ ulOffset + 3 ] = ( BYTE ) ( ( ulRelative >> 24 ) & 0xFF );

/* #elseif 16 bits and low byte first */
/* #elseif 32 bits and high byte first */
/* #elseif ... */
/* #endif */
}
