/*
 * $Id: maindllp.c,v 1.32 2009/02/02 11:25:10 marchuet Exp $
 */

/*
 * Harbour Project source code:
 * Windows pcode DLL entry point and VM/RTL routing functions
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
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

#define __NO_EXPORT__
#define HB_OS_WIN_USED
#include "hbtypes.h"

HB_EXTERN_BEGIN

#if defined( HB_OS_WIN )

static HMODULE hModule = NULL;
static FARPROC pExtIsArray = NULL;
static FARPROC hb_GetProcAddress( char* szFuncName );

#if defined(HB_DLL_REQUIRED_DLLMAIN)
BOOL WINAPI DllMain( HINSTANCE hInstance, DWORD fdwReason, PVOID pvReserved )
{
   HB_TRACE( HB_TR_DEBUG, ( "DllMain( %p, %p, %d )", hInstance, fdwReason,
             pvReserved ) );
#else
BOOL WINAPI DllEntryPoint( HINSTANCE hInstance, DWORD fdwReason, PVOID pvReserved )
{
   HB_TRACE( HB_TR_DEBUG, ( "DllEntryPoint( %p, %p, %d )", hInstance, fdwReason,
             pvReserved ) );
#endif

   HB_SYMBOL_UNUSED( hInstance );
   HB_SYMBOL_UNUSED( pvReserved );

   switch( fdwReason )
   {
      case DLL_PROCESS_ATTACH:
         hModule = GetModuleHandle( NULL );
         pExtIsArray = hb_GetProcAddress( "_hb_extIsArray" );
         break;

      case DLL_PROCESS_DETACH:
         hModule = NULL;
         pExtIsArray = NULL;
         break;
   }

   return TRUE;
}

static FARPROC hb_GetProcAddress( char* szFuncName )
{
   FARPROC pFunc = GetProcAddress( hModule, szFuncName );

   /* Try #1: Conventional cdecl flavor */
   if ( pFunc )
   {
      return pFunc;
   }
   else
   {
      /* Try #2: Microsoft cdecl flavor */
      pFunc = GetProcAddress( hModule, szFuncName + 1 );
   }

   if ( pFunc )
   {
      return pFunc;
   }
   else
   {
      /* Try #3: Watcom register calling flavor */
      char * szFuncName_ = (char*) hb_xgrab( strlen( szFuncName ) + 1 );
      hb_snprintf( szFuncName_, strlen( szFuncName ) + 1, "%s_", szFuncName + 1 );
      pFunc = GetProcAddress( hModule, szFuncName_ );
      hb_xfree( szFuncName_ );

      if ( pFunc )
      {
         return pFunc;
      }
      else
      {
	 /* Hands-up ..... */
         char __szError[256];
         hb_snprintf( __szError, sizeof( __szError ), "Cannot find function address: %s", szFuncName );
         MessageBox( NULL, __szError, szFuncName, MB_ICONSTOP );
	 return NULL;
      }
   }
}

/* module symbols initialization */
PSYMBOLS hb_vmProcessSymbols( PHB_SYMB pSymbols, USHORT uiModuleSymbols, char *szModule, int iPCodeVer, PHB_ITEM *pGlobals ) /* module symbols initialization */
{
   static FARPROC pProcessSymbols = NULL;
   HB_SYMBOL_UNUSED( pGlobals );

   /* notice hb_vmProcessSysDllSymbols() must be used, and not
    * hb_vmProcessSymbols(), as some special symbols pointers
    * adjustments are required
    */
   if ( !pProcessSymbols )
   {
      pProcessSymbols = hb_GetProcAddress( "_hb_vmProcessSysDllSymbols" );
   }

   if( pProcessSymbols )
   {
      return ( ( VM_PROCESS_DLL_SYMBOLS ) pProcessSymbols )( pSymbols, uiModuleSymbols, szModule, iPCodeVer, pGlobals );
   }
   /* else
    *    may we issue an error ? */

   return NULL;
}

void hb_vmExecute( const BYTE * pCode, PHB_SYMB pSymbols )
{
   static FARPROC pExecute = NULL;

   if ( !pExecute )
   {
      pExecute = hb_GetProcAddress( "_hb_vmExecute" );
   }

   if( pExecute )
   {
      ( ( VM_DLL_EXECUTE ) pExecute )( pCode, pSymbols );
   }

   /* else
    *    may we issue an error ? */
}

/* extend API implementation for pcode DLLs */

char * hb_parc( int iParam, ... )
{
   static FARPROC pParC = NULL;

   if ( !pParC )
   {
      pParC = hb_GetProcAddress( "_hb_parc" );
   }

   if( pExtIsArray && pParC )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( EXT_PARC2 ) pParC )( iParam, ulArrayIndex );
      }
      else
      {
         return ( ( EXT_PARC1 ) pParC )( iParam );
      }
   }
   else
   {
      return "";
   }
}

PHB_ITEM hb_param( int iParam, LONG lMask ) /* retrieve a generic parameter */
{
   PHB_ITEM pReturn = NULL;
   static FARPROC pParam = NULL;

   if ( !pParam )
   {
      pParam=hb_GetProcAddress( "_hb_param" );
   }

   if( pParam )
   {
      pReturn = ( ( HB_PARAM ) pParam )( iParam,lMask );
   }

   return pReturn;
}

PHB_ITEM hb_paramError( int iParam ) /* Returns either the generic parameter or a NIL item if param not provided */
{
   PHB_ITEM pReturn = NULL;
   static FARPROC pParamError = NULL;

   if ( !pParamError )
   {
      pParamError=hb_GetProcAddress( "_hb_paramError" );
   }

   if( pParamError )
   {
      pReturn = ( ( HB_PARAMERROR ) pParamError )( iParam );
   }

   return pReturn;
}

#undef hb_pcount
int  hb_pcount( void )          /* returns the number of suplied parameters */
{
   int iReturn = 0;
   static FARPROC pCounts = NULL;

   if ( !pCounts )
   {
      pCounts=hb_GetProcAddress( "_hb_pcount" );
   }

   if( pCounts )
   {
      iReturn = ( ( HB_PCOUNTS ) pCounts )();
   }

   return iReturn;
}

//----------------------------------------------------------------------------//
#undef hb_ret
void hb_ret( void )
{
   static HB_RET pFunc = NULL;

   if ( !pFunc )
   {
      pFunc = (HB_RET) hb_GetProcAddress( "_hb_ret" );
   }

   if( pFunc )
   {
      pFunc ();
   }

   return;
}

#undef hb_retc
void hb_retc( const char * szText )   /* returns a string */
{
   static FARPROC pRetc = NULL;

   if ( !pRetc )
   {
      pRetc = hb_GetProcAddress( "_hb_retc" );
   }

   if( pRetc )
   {
      ( ( HB_RETC ) pRetc )( szText );
   }
}

#undef hb_retclen
void hb_retclen( const char * szText, ULONG ulLen ) /* returns a string with a specific length */
{
   static FARPROC pRetclen = NULL;

   if ( !pRetclen )
   {
      pRetclen = hb_GetProcAddress( "_hb_retclen" );
   }

   if( pRetclen )
   {
      ( ( HB_RETCLEN ) pRetclen )( szText,ulLen );
   }
}

#undef hb_retds
void hb_retds( const char * szDate )  /* returns a date, must use yyyymmdd format */
{
   static FARPROC pRetds = NULL;

   if ( !pRetds )
   {
      pRetds=hb_GetProcAddress( "_hb_retds" );
   }

   if( pRetds )
   {
      ( ( HB_RETDS ) pRetds )( szDate );
   }
}

#undef hb_retd
void hb_retd( int iYear, int iMonth, int iDay ) /* returns a date */
{
   static FARPROC pRetd = NULL;

   if ( !pRetd )
   {
      pRetd = hb_GetProcAddress( "_hb_retd" );
   }

   if( pRetd )
   {
      ( ( HB_RETD ) pRetd )( iYear, iMonth, iDay );
   }
}

#undef hb_retdl
void hb_retdl( LONG lJulian )   /* returns a LONG value as a julian date */
{
   static FARPROC pRet = NULL;

   if ( !pRet )
   {
      pRet = hb_GetProcAddress( "_hb_retdl" );
   }

   if( pRet )
   {
      ( ( HB_RETDL ) pRet )( lJulian );
   }

}

#undef hb_retl
void hb_retl( int iTrueFalse ) /*  returns a logical integer */
{
   static FARPROC pRet = NULL;

   if ( !pRet )
   {
      pRet = hb_GetProcAddress( "_hb_retl" );
   }

   if( pRet )
   {
      ( ( HB_RETDL ) pRet )( iTrueFalse );
   }
}

#undef hb_retnd
void hb_retnd( double dNumber ) /* returns a double */
{
   static FARPROC pRet = NULL;

   if ( !pRet )
   {
      pRet = hb_GetProcAddress( "_hb_retnd" );
   }

   if( pRet )
   {
      ( ( HB_RETND ) pRet )( dNumber );
   }
}

#undef hb_retni
void hb_retni( int iNumber )    /* returns a integer number */
{
   static FARPROC pRet = NULL;

   if ( !pRet )
   {
      pRet = hb_GetProcAddress( "_hb_retni" );
   }

   if( pRet )
   {
      ( ( HB_RETNI ) pRet )( iNumber );
   }
}

#undef hb_retnl
void hb_retnl( LONG lNumber )   /* returns a LONG number */
{
   static FARPROC pRet = NULL;

   if ( !pRet )
   {
      pRet = hb_GetProcAddress( "_hb_retnl" );
   }

   if( pRet )
   {
      ( ( HB_RETNL ) pRet )( lNumber );
   }
}

#undef hb_retnlen
void hb_retnlen( double dNumber, int iWidth, int iDec ) /* returns a double, with specific width and decimals */
{
   static FARPROC pRet = NULL;

   if ( !pRet )
   {
      pRet = hb_GetProcAddress( "_hb_retnlen" );
   }

   if( pRet )
   {
      ( ( HB_RETNLEN ) pRet )( dNumber,iWidth,iDec );
   }
}

#undef hb_retndlen
void hb_retndlen( double dNumber, int iWidth, int iDec ) /* returns a double, with specific width and decimals */
{
   static FARPROC pRet = NULL;

   if ( !pRet )
   {
      pRet = hb_GetProcAddress( "_hb_retndlen" );
   }

   if( pRet )
   {
      ( ( HB_RETNDLEN ) pRet )( dNumber,iWidth,iDec );
   }
}

#undef hb_retnilen
void hb_retnilen( int iNumber, int iWidth ) /* returns a integer number, with specific width */
{
    static FARPROC pRet = NULL;

    if ( !pRet )
    {
       pRet = hb_GetProcAddress( "_hb_retnilen" );
    }

    if( pRet )
    {
      ( ( HB_RETNILEN ) pRet )( iNumber,iWidth );
    }
}

#undef hb_retnllen
void hb_retnllen( LONG lNumber, int iWidth ) /* returns a LONG number, with specific width */
{
   static FARPROC pRet = NULL;

   if ( !pRet )
   {
      pRet = hb_GetProcAddress( "_hb_retnilen" );
   }

   if( pRet )
   {
      ( ( HB_RETNLLEN ) pRet )( lNumber,iWidth );
   }
}

#undef hb_reta
void hb_reta( ULONG ulLen )  /* returns an array with a specific length */
{
   static FARPROC pRet = NULL;

   if ( !pRet )
   {
      pRet = hb_GetProcAddress( "_hb_reta" );
   }

   if( pRet )
   {
      ( ( HB_RETA ) pRet )( ulLen );
   }
}

#undef hb_parinfa
ULONG hb_parinfa( int iParamNum, ULONG uiArrayIndex ) /* retrieve length or element type of an array parameter */
{
   ULONG ulReturn = 0;
   static FARPROC pParinfa = NULL;

   if ( !pParinfa )
   {
      pParinfa = hb_GetProcAddress( "_hb_parinfa" );
   }

   if( pParinfa )
   {
      ulReturn = ( ( HB_PARINFA ) pParinfa )( iParamNum,uiArrayIndex );
   }

   return ulReturn;
}

#undef hb_parinfo
ULONG hb_parinfo( int iParam ) /* Determine the param count or data type */
{
   ULONG ulReturn = 0;
   static FARPROC pParinfo = NULL;

   if ( !pParinfo )
   {
      pParinfo = hb_GetProcAddress( "_hb_parinfo" );
   }

   if( pParinfo )
   {
      ulReturn = ( ( HB_PARINFO ) pParinfo )( iParam );
   }

   return ulReturn;
}

#undef hb_parclen
ULONG hb_parclen( int iParam, ... ) /* retrieve a string parameter length */
{
   static FARPROC pParC = NULL;

   if ( !pParC )
   {
      pParC = hb_GetProcAddress( "_hb_parclen" );
   }

   if( pExtIsArray && pParC )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( HB_PARCLEN2 ) pParC )( iParam, ulArrayIndex );
      }
      else
      {
         return ( ( HB_PARCLEN ) pParC )( iParam );
      }
   }
   else
   {
      return 0;
   }
}

#undef hb_parcsiz
ULONG hb_parcsiz( int iParam, ... ) /* retrieve a by-reference string parameter length, including terminator */
{
   static FARPROC pParcSiz = NULL;

   if ( !pParcSiz )
   {
      pParcSiz = hb_GetProcAddress( "_hb_parcsiz" );
   }

   if( pExtIsArray && pParcSiz )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( HB_PARCSIZ2 ) pParcSiz )( iParam, ulArrayIndex );
      }
      else
      {
         return ( ( HB_PARCSIZ ) pParcSiz )( iParam );
      }
   }
   else
   {
      return 0;
   }
}

#undef hb_pards
char * hb_pards( int iParam, ... ) /* retrieve a date as a string yyyymmdd */
{
   static FARPROC pParDs = NULL;

   if ( !pParDs )
   {
      pParDs = hb_GetProcAddress( "_hb_pards" );
   }

   if( pExtIsArray && pParDs )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( HB_PARDS2 ) pParDs )( iParam, ulArrayIndex );
      }
      else
      {
         return ( ( HB_PARDS ) pParDs )( iParam );
      }
   }
   else
   {
      return "";
   }
}

#undef hb_pardsbuff
char * hb_pardsbuff( char * szDate, int iParam, ... ) /* retrieve a date as a string yyyymmdd */
{
   static FARPROC pParDsBuff = NULL;

   if ( !pParDsBuff )
   {
      pParDsBuff = hb_GetProcAddress( "_hb_pardsbuff" );
   }

   if( pExtIsArray && pParDsBuff )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( HB_PARDSBUFF2 ) pParDsBuff )( szDate, iParam, ulArrayIndex );
      }
      else
      {
         return ( ( HB_PARDSBUFF ) pParDsBuff )( szDate, iParam );
      }
   }
   else
   {
      return "";
   }
}

#undef hb_parl
int hb_parl( int iParam, ... ) /* retrieve a logical parameter as an int */
{
   /* int iReturn; */
   static FARPROC pParL = NULL;

   if ( !pParL )
   {
      pParL = hb_GetProcAddress( "_hb_parl" );
   }

   if( pExtIsArray && pParL )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( HB_PARL2 ) pParL )( iParam, ulArrayIndex );
      }
      else
      {
         return ( ( HB_PARL ) pParL )( iParam );
      }
   }
   else
   {
      return 0;
   }
}

#undef hb_parnd
double hb_parnd( int iParam, ... ) /* retrieve a numeric parameter as a double */
{
   static FARPROC pParNd = NULL;

   if ( !pParNd )
   {
      pParNd = hb_GetProcAddress( "_hb_parnd" );
   }

   if( pExtIsArray && pParNd )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( HB_PARND2 ) pParNd )( iParam, ulArrayIndex );
      }
      else
      {
         return ( ( HB_PARND ) pParNd )( iParam );
      }
   }
   else
   {
      return 0;
   }
}

#undef hb_parni
int hb_parni( int iParam, ... ) /* retrieve a numeric parameter as a integer */
{
   /* int iReturn; */
   static FARPROC pParNi = NULL;

   if ( !pParNi )
   {
      pParNi = hb_GetProcAddress( "_hb_parni" );
   }

   if( pExtIsArray && pParNi )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( HB_PARNI2 ) pParNi )( iParam, ulArrayIndex );
      }
      else
      {
         return ( ( HB_PARNI ) pParNi )( iParam );
      }
   }
   else
   {
      return 0;
   }
}

#undef hb_parnl
LONG hb_parnl( int iParam, ... ) /* retrieve a numeric parameter as a LONG */
{
   static FARPROC pParNl = NULL;

   if ( !pParNl )
   {
      pParNl = hb_GetProcAddress( "_hb_parnl" );
   }

   if( pExtIsArray && pParNl )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         return ( ( HB_PARNL2 ) pParNl )( iParam, ulArrayIndex );
      }
      else
      {
         return ( ( HB_PARNL ) pParNl )( iParam );
      }
   }
   else
   {
      return 0;
   }
}

#undef hb_storc
void hb_storc( const char * szText, int iParam, ... )
{
   static FARPROC pStorC = NULL;

   if ( !pStorC )
   {
      pStorC = hb_GetProcAddress( "_hb_storc" );
   }

   if( pExtIsArray && pStorC )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORC2 ) pStorC )( szText, iParam, ulArrayIndex );
      }
      else
      {
         ( ( HB_STORC ) pStorC )( szText, iParam );
      }
   }
}

#undef hb_storclen
void hb_storclen( const char * szText, ULONG ulLen, int iParam, ... )
{
   static FARPROC pStorC = NULL;

   if ( !pStorC )
   {
      pStorC = hb_GetProcAddress( "_hb_storclen" );
   }

   if( pExtIsArray && pStorC )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORCLEN2 ) pStorC )( szText, ulLen, iParam, ulArrayIndex );
      }
      else
      {
         ( ( HB_STORCLEN ) pStorC )( szText, ulLen, iParam );
      }
   }
}

#undef hb_stords
void hb_stords( const char * szDate, int iParam, ... )
{
   static FARPROC pStorDs = NULL;

   if ( !pStorDs )
   {
      pStorDs = hb_GetProcAddress( "_hb_stords" );
   }

   if( pExtIsArray && pStorDs )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORDS2 ) pStorDs )( szDate, iParam, ulArrayIndex );
      }
      else
      {
         ( ( HB_STORDS ) pStorDs )( szDate, iParam );
      }
   }
}

#undef hb_storl
void hb_storl( int iLogical, int iParam, ... )
{
   static FARPROC pStorL = NULL;

   if ( !pStorL )
   {
      pStorL = hb_GetProcAddress( "_hb_storl" );
   }

   if( pExtIsArray && pStorL )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORL2 ) pStorL )( iLogical, iParam, ulArrayIndex );
      }
      else
      {
         ( ( HB_STORL ) pStorL )(  iLogical, iParam );
      }
   }
}

#undef hb_storni
void hb_storni( int iValue, int iParam, ... )
{
   static FARPROC pStorNi = NULL;

   if ( !pStorNi )
   {
      pStorNi = hb_GetProcAddress( "_hb_storni" );
   }

   if( pExtIsArray && pStorNi )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORNI2 ) pStorNi )( iValue, iParam, ulArrayIndex );
      }
      else
      {
         ( ( HB_STORNI ) pStorNi )(  iValue, iParam );
      }
   }
}

#undef hb_stornl
void hb_stornl( LONG lValue, int iParam, ... )
{
   static FARPROC pStorNl = NULL;

   if ( !pStorNl )
   {
      pStorNl = hb_GetProcAddress( "_hb_stornl" );
   }

   if( pExtIsArray && pStorNl )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORNL2 ) pStorNl )( lValue, iParam, ulArrayIndex );
      }
      else
      {
         ( ( HB_STORNL ) pStorNl )(  lValue, iParam );
      }
   }
}

#undef hb_stornd
void hb_stornd( double dNumber, int iParam, ... )
{
   static FARPROC pStorNd = NULL;

   if ( !pStorNd )
   {
      pStorNd = hb_GetProcAddress( "_hb_stornd" );
   }

   if( pExtIsArray && pStorNd )
   {
      if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
      {
         va_list va;
         ULONG ulArrayIndex;

         va_start( va, iParam );
         ulArrayIndex = va_arg( va, ULONG );
         va_end( va );

         ( ( HB_STORND2 ) pStorNd )( dNumber, iParam, ulArrayIndex );
      }
      else
      {
         ( ( HB_STORND ) pStorNd )( dNumber, iParam );
      }
   }
}

BOOL hb_arrayNew( PHB_ITEM pItem, ULONG ulLen )  /* creates a new array */
{
   static HB_ARRAYNEW pArrayNew = NULL;

   if ( !pArrayNew )
   {
      pArrayNew = ( HB_ARRAYNEW ) hb_GetProcAddress( "_hb_arrayNew" );
   }

   if( pArrayNew )
   {
      return pArrayNew( pItem, ulLen );
   }
   else
   {
      return FALSE;
   }
}

ULONG hb_arrayLen( PHB_ITEM pArray )  /* retrives the array len */
{
   static HB_ARRAYLEN pArrayLen = NULL;

   if ( !pArrayLen )
   {
      pArrayLen = ( HB_ARRAYLEN ) hb_GetProcAddress( "_hb_arrayLen" );
   }

   if( pArrayLen )
   {
      return pArrayLen( pArray );
   }
   else
   {
      return 0;
   }
}

BOOL hb_arrayIsObject( PHB_ITEM pArray )  /* retrives if the array is an object */
{
   static HB_ARRAYISOBJECT pArrayIsObject = NULL;

   if ( !pArrayIsObject )
   {
      pArrayIsObject = ( HB_ARRAYISOBJECT ) hb_GetProcAddress( "_hb_arrayIsObject" );
   }

   if( pArrayIsObject )
   {
      return pArrayIsObject( pArray );
   }
   else
   {
      return FALSE;
   }
}

BOOL hb_arrayAdd( PHB_ITEM pArray, PHB_ITEM pItemValue )  /* add a new item to the end of an array item */
{
   static HB_ARRAYADD pArrayAdd = NULL;

   if ( !pArrayAdd )
   {
      pArrayAdd = ( HB_ARRAYADD ) hb_GetProcAddress( "_hb_arrayAdd" );
   }

   if( pArrayAdd )
   {
      return pArrayAdd( pArray, pItemValue );
   }
   else
   {
      return FALSE;
   }
}

BOOL hb_arrayIns( PHB_ITEM pArray, ULONG ulIndex )  /* insert a nil item into an array, without changing the length */
{
   static HB_ARRAYINS pArrayIns = NULL;

   if ( !pArrayIns )
   {
      pArrayIns = ( HB_ARRAYINS ) hb_GetProcAddress( "_hb_arrayIns" );
   }

   if( pArrayIns )
   {
      return pArrayIns( pArray, ulIndex );
   }
   else
   {
      return FALSE;
   }
}

BOOL hb_arrayDel( PHB_ITEM pArray, ULONG ulIndex )  /* delete an array item, without changing length */
{
   static HB_ARRAYDEL pArrayDel = NULL;

   if ( !pArrayDel )
   {
      pArrayDel = ( HB_ARRAYDEL ) hb_GetProcAddress( "_hb_arrayDel" );
   }

   if( pArrayDel )
   {
      return pArrayDel( pArray, ulIndex );
   }
   else
   {
      return FALSE;
   }
}

BOOL hb_arraySize( PHB_ITEM pArray, ULONG ulLen )  /* sets the array total length */
{
   static HB_ARRAYSIZE pArraySize = NULL;

   if ( !pArraySize )
   {
      pArraySize = ( HB_ARRAYSIZE ) hb_GetProcAddress( "_hb_arraySize" );
   }

   if( pArraySize )
   {
      return pArraySize( pArray, ulLen );
   }
   else
   {
      return FALSE;
   }
}

BOOL hb_arrayLast( PHB_ITEM pArray, PHB_ITEM pResult )  /* retrieve last item in an array */
{
   static HB_ARRAYLAST pArrayLast = NULL;

   if ( !pArrayLast )
   {
      pArrayLast = ( HB_ARRAYLAST ) hb_GetProcAddress( "_hb_arrayLast" );
   }

   if( pArrayLast )
   {
      return pArrayLast( pArray, pResult );
   }
   else
   {
      return FALSE;
   }
}

BOOL hb_arrayRelease( PHB_ITEM pArray )  /* releases an array - don't call it - use ItemRelease() !!! */
{
   static HB_ARRAYRELEASE pArrayRelease = NULL;

   if ( !pArrayRelease )
   {
      pArrayRelease = ( HB_ARRAYRELEASE ) hb_GetProcAddress( "_hb_arrayRelease" );
   }

   if( pArrayRelease )
   {
      return pArrayRelease( pArray );
   }
   else
   {
      return FALSE;
   }
}

BOOL hb_arraySet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )  /* sets an array element */
{
   static HB_ARRAYSET pArraySet = NULL;

   if ( !pArraySet )
   {
      pArraySet = ( HB_ARRAYSET ) hb_GetProcAddress( "_hb_arraySet" );
   }

   if( pArraySet )
   {
      return pArraySet( pArray, ulIndex, pItem );
   }
   else
   {
      return FALSE;
   }
}

BOOL hb_arrayGet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )  /* retrieves an item */
{
   static HB_ARRAYGET pArrayGet = NULL;

   if ( !pArrayGet )
   {
      pArrayGet = ( HB_ARRAYGET ) hb_GetProcAddress( "_hb_arrayGet" );
   }

   if( pArrayGet )
   {
      return pArrayGet( pArray, ulIndex, pItem );
   }
   else
   {
      return FALSE;
   }
}

#undef hb_xinit
int hb_xinit( void )                         /* Initialize fixed memory subsystem */
{
   static HB_XINIT pXinit = NULL;

   if ( !pXinit )
   {
      pXinit =  ( HB_XINIT ) hb_GetProcAddress( "_hb_xinit" );
   }

   if( pXinit )
   {
      pXinit();
   }
   return 1;
}

#undef hb_xexit
void hb_xexit( void )                         /* Deinitialize fixed memory subsystem */
{
   static HB_XEXIT pXexit = NULL;

   if ( !pXexit )
   {
      pXexit = ( HB_XEXIT ) hb_GetProcAddress( "_hb_xexit" );
   }

   if( pXexit )
   {
      pXexit();
   }
}

#undef hb_xalloc
void * hb_xalloc( ULONG ulSize )                /* allocates memory, returns NULL on failure */
{
   static HB_XALLOC pXalloc = NULL;

   if ( !pXalloc )
   {
      pXalloc = ( HB_XALLOC ) hb_GetProcAddress( "_hb_xalloc" );
   }

   if( pXalloc )
   {
      return pXalloc( ulSize );
   }

   return NULL;
}

#undef hb_xgrab
void * hb_xgrab( ULONG ulSize )                 /* allocates memory, exits on failure */
{
   static HB_XGRAB pXgrab = NULL;

   if ( !pXgrab )
   {
      pXgrab = ( HB_XGRAB ) hb_GetProcAddress( "_hb_xgrab" );
   }

   if( pXgrab )
   {
      return pXgrab( ulSize );
   }

   return NULL;
}

#undef hb_xfree
void hb_xfree( void * pMem )                  /* frees memory */
{
   static HB_XFREE pXfree = NULL;

   if ( !pXfree )
   {
      pXfree = ( HB_XFREE ) hb_GetProcAddress( "_hb_xfree" );
   }

   if( pXfree )
   {
      pXfree( pMem );
   }
}

#undef hb_xrealloc
void * hb_xrealloc( void * pMem, ULONG ulSize ) /* reallocates memory */
{
   static HB_XREALLOC pXrealloc = NULL;

   if ( !pXrealloc )
   {
      pXrealloc =  ( HB_XREALLOC ) hb_GetProcAddress( "_hb_xrealloc" );
   }

   if( pXrealloc )
   {
      return ( void* ) pXrealloc( pMem, ulSize );
   }

   return NULL;
}

#undef hb_xsize
ULONG hb_xsize( void * pMem )                  /* returns the size of an allocated memory block */
{
   static HB_XSIZE pXsize = NULL;

   if ( !pXsize )
   {
      pXsize = ( HB_XSIZE ) hb_GetProcAddress( "_hb_xsize" );
   }

   if( pXsize )
   {
      return pXsize( ( void * ) pMem );
   }

   return 0;
}

//----------------------------------------------------------------------------//
#undef hb_fsDelete
BOOL hb_fsDelete( BYTE * pszFileName )
{
   BOOL pReturn = FALSE;
   static HB_FSDELETE pFunc = NULL;

   if ( !pFunc )
   {
      pFunc = (HB_FSDELETE) hb_GetProcAddress( "_hb_fsDelete" );
   }

   if( pFunc )
   {
      pReturn = pFunc ( pszFileName );
   }

   return pReturn;
}

//----------------------------------------------------------------------------//
#undef hb_fsWrite
USHORT hb_fsWrite( HB_FHANDLE hFileHandle, BYTE * pBuff, USHORT ulCount )
{
   USHORT pReturn = 0;
   static HB_FSWRITE pFunc = NULL;

   if ( !pFunc )
   {
      pFunc = (HB_FSWRITE) hb_GetProcAddress( "_hb_fsWrite" );
   }

   if( pFunc )
   {
      pReturn = pFunc ( hFileHandle, pBuff, ulCount );
   }

   return pReturn;
}

//----------------------------------------------------------------------------//
#undef hb_fsSeek
ULONG hb_fsSeek( HB_FHANDLE hFileHandle, LONG lOffset, USHORT uiMode )
{
   ULONG pReturn = 0;
   static HB_FSSEEK pFunc = NULL;

   if ( !pFunc )
   {
      pFunc = (HB_FSSEEK) hb_GetProcAddress( "_hb_fsSeek" );
   }

   if( pFunc )
   {
      pReturn = pFunc ( hFileHandle, lOffset, uiMode );
   }

   return pReturn;
}

//----------------------------------------------------------------------------//
#undef hb_fsCreate
HB_FHANDLE hb_fsCreate( BYTE * pszFileName, USHORT uiAttr )
{
   HB_FHANDLE pReturn = ( HB_FHANDLE ) -1;
   static HB_FSCREATE pFunc = NULL;

   if ( !pFunc )
   {
      pFunc = (HB_FSCREATE) hb_GetProcAddress( "_hb_fsCreate" );
   }

   if( pFunc )
   {
      pReturn = pFunc ( pszFileName, uiAttr );
   }

   return pReturn;
}

//----------------------------------------------------------------------------//
#undef hb_fsRead
USHORT hb_fsRead( HB_FHANDLE hFileHandle, BYTE * pBuff, USHORT ulCount )
{
   USHORT pReturn = 0;
   static HB_FSREAD pFunc = NULL;

   if ( !pFunc )
   {
      pFunc = (HB_FSREAD) hb_GetProcAddress( "_hb_fsRead" );
   }

   if( pFunc )
   {
      pReturn = pFunc ( hFileHandle, pBuff, ulCount );
   }

   return pReturn;
}

//----------------------------------------------------------------------------//
#undef hb_fsOpen
HB_FHANDLE hb_fsOpen( BYTE * pszFileName, USHORT uiFlags )
{
   HB_FHANDLE pReturn = ( HB_FHANDLE ) -1;
   static HB_FSOPEN pFunc = NULL;

   if ( !pFunc )
   {
      pFunc = (HB_FSOPEN) hb_GetProcAddress( "_hb_fsOpen" );
   }

   if( pFunc )
   {
      pReturn = pFunc ( pszFileName, uiFlags );
   }

   return pReturn;
}

//----------------------------------------------------------------------------//
#undef hb_fsClose
void hb_fsClose( HB_FHANDLE hFileHandle  )
{
   static HB_FSCLOSE pFunc = NULL;

   if ( !pFunc )
   {
      pFunc = (HB_FSCLOSE) hb_GetProcAddress( "_hb_fsClose" );
   }

   if( pFunc )
   {
      pFunc ( hFileHandle );
   }

   return;
}

HB_EXTERN_END
#endif

#undef _HB_SNPRINTF_ADD_EOS
#undef hb_snprintf
/* NOTE: The full size of the buffer is expected as nSize. [vszakats] */
ULONG hb_snprintf( char * buffer, ULONG nSize, const char * format, ... )
{
   va_list arglist;
   ULONG result;

   va_start( arglist, format );

#if defined( __DJGPP__ ) && ( __DJGPP__ < 2 || ( __DJGPP__ == 2 && __DJGPP_MINOR__ <= 3 ) )
   /* Use vsprintf() for DJGPP <= 2.03.
      This is a temporary hack, should implement a C99 snprintf() ourselves. */
   result = vsprintf( buffer, format, arglist );
#elif defined( _MSC_VER ) && _MSC_VER >= 1400
   result = _vsnprintf_s( buffer, nSize, _TRUNCATE, format, arglist );
#elif ( defined( _MSC_VER ) || defined( __DMC__ ) ) && !defined( __XCC__ )
   result = _vsnprintf( buffer, nSize, format, arglist );
   #define _HB_SNPRINTF_ADD_EOS
#elif defined( __WATCOMC__ ) && __WATCOMC__ < 1200
   result = _vbprintf( buffer, nSize, format, arglist );
#else
   result = vsnprintf( buffer, nSize, format, arglist );
#endif

   va_end( arglist );

#ifdef _HB_SNPRINTF_ADD_EOS
   if( buffer && nSize )
      buffer[ nSize - 1 ] = '\0';
#endif

   return result;
}
