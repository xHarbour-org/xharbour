/*
 * $Id$
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

#define HB_OS_WIN_USED

#ifndef _CRT_SECURE_NO_WARNINGS
#   define _CRT_SECURE_NO_WARNINGS
#endif

#include "hbtypes.h"

HB_EXTERN_BEGIN

#if defined( HB_OS_WIN )
#include "windows.h"
static HMODULE hModule = NULL;
static FARPROC pExtIsArray = NULL;
static FARPROC hb_GetProcAddress( char * szFuncName );

#if defined( HB_DLL_REQUIRED_DLLMAIN )
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
         hModule     = GetModuleHandle( NULL );
         pExtIsArray = hb_GetProcAddress( "_hb_extIsArray" );
         break;

      case DLL_PROCESS_DETACH:
         hModule     = NULL;
         pExtIsArray = NULL;
         break;
   }

   return TRUE;
}

static FARPROC hb_GetProcAddress( char * szFuncName )
{
   /* Try #1: Conventional cdecl flavor */
   FARPROC pFunc = GetProcAddress( hModule, szFuncName );

   if( ! pFunc )
   {
      /* Try #2: Microsoft cdecl flavor */
      pFunc = GetProcAddress( hModule, szFuncName + 1 );

      if( ! pFunc )
      {
         /* Try #3: Watcom register calling flavor */
         char * szFuncName_ = ( char * ) malloc( strlen( szFuncName ) + 1 );
         sprintf( szFuncName_, "%s_", szFuncName + 1 );
         pFunc = GetProcAddress( hModule, szFuncName_ );
         free( szFuncName_ );

         if( ! pFunc )
         {
            /*
               Hands-up ...
               Application will exit here ...
             */
            char __szError[ 256 ];
            sprintf( __szError, "Cannot find function address: %s", szFuncName );
            MessageBox( NULL, __szError, szFuncName, MB_ICONSTOP );
            exit( 0 );
         }
      }
   }

   return pFunc;
}

/* module symbols initialization */
PSYMBOLS hb_vmProcessSymbols( PHB_SYMB pSymbols, USHORT uiModuleSymbols, const char * szModule,  int iPCodeVer, PHB_ITEM * pGlobals ) /* module symbols initialization */
{
   /* notice hb_vmProcessSysDllSymbols() must be used, and not
    * hb_vmProcessSymbols(), as some special symbols pointers
    * adjustments are required
    */
#if defined( __cplusplus )
   static VM_PROCESS_DLL_SYMBOLS
#else
   static VM_PROCESS_DLL_SYMBOLS pProcessSymbols = NULL;
   if( ! pProcessSymbols )
#endif
   pProcessSymbols = ( VM_PROCESS_DLL_SYMBOLS ) hb_GetProcAddress( "_hb_vmProcessSysDllSymbols" );

   HB_SYMBOL_UNUSED( pGlobals );
   return pProcessSymbols( pSymbols, uiModuleSymbols, szModule, iPCodeVer, pGlobals );
}

void hb_vmExecute( const BYTE * pCode, PHB_SYMB pSymbols )
{
#if defined( __cplusplus )
   static VM_DLL_EXECUTE
#else
   static VM_DLL_EXECUTE pExecute = NULL;
   if( ! pExecute )
#endif
   pExecute = ( VM_DLL_EXECUTE ) hb_GetProcAddress( "_hb_vmExecute" );

   pExecute( pCode, pSymbols );
}

/* extend API implementation for pcode DLLs */

const char * hb_parc( int iParam, ... )
{
#if defined( __cplusplus )
   static EXT_PARC2
#else
   static EXT_PARC2 pParC = NULL;
   if( ! pParC )
#endif
   pParC = ( EXT_PARC2 ) hb_GetProcAddress( "_hb_parc" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      return pParC( iParam, ulArrayIndex );
   }
   else
   {
      return ( ( EXT_PARC1 ) pParC )( iParam );
   }
}

PHB_ITEM hb_param( int iParam, LONG lMask ) /* retrieve a generic parameter */
{
#if defined( __cplusplus )
   static HB_PARAM
#else
   static HB_PARAM pParam = NULL;
   if( ! pParam )
#endif
   pParam = ( HB_PARAM ) hb_GetProcAddress( "_hb_param" );

   return pParam( iParam, lMask );
}

PHB_ITEM hb_paramError( int iParam ) /* Returns either the generic parameter or a NIL item if param not provided */
{
#if defined( __cplusplus )
   static HB_PARAMERROR
#else
   static HB_PARAMERROR pParamError = NULL;
   if( ! pParamError )
#endif
   pParamError = ( HB_PARAMERROR ) hb_GetProcAddress( "_hb_paramError" );

   return pParamError( iParam );
}

#undef hb_pcount
int  hb_pcount( void )          /* returns the number of suplied parameters */
{
#if defined( __cplusplus )
   static HB_PCOUNTS
#else
   static HB_PCOUNTS pCounts = NULL;
   if( ! pCounts )
#endif
   pCounts = ( HB_PCOUNTS ) hb_GetProcAddress( "_hb_pcount" );

   return pCounts();
}

/*---------------------------------------------------------------------------*/
#undef hb_ret
void hb_ret( void )
{
#if defined( __cplusplus )
   static HB_RET
#else
   static HB_RET pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( HB_RET ) hb_GetProcAddress( "_hb_ret" );

   pFunc();
}

#undef hb_retc
void hb_retc( const char * szText )   /* returns a string */
{
#if defined( __cplusplus )
   static HB_RETC
#else
   static HB_RETC pRetc = NULL;
   if( ! pRetc )
#endif
   pRetc = ( HB_RETC ) hb_GetProcAddress( "_hb_retc" );

   pRetc( szText );
}

#undef hb_retclen
void hb_retclen( const char * szText, HB_SIZE ulLen ) /* returns a string with a specific length */
{
#if defined( __cplusplus )
   static HB_RETCLEN
#else
   static HB_RETCLEN pRetclen = NULL;
   if( ! pRetclen )
#endif
   pRetclen = ( HB_RETCLEN ) hb_GetProcAddress( "_hb_retclen" );

   pRetclen( szText, ulLen );
}

#undef hb_retds
void hb_retds( const char * szDate )  /* returns a date, must use yyyymmdd format */
{
#if defined( __cplusplus )
   static HB_RETDS
#else
   static HB_RETDS pRetds = NULL;
   if( ! pRetds )
#endif
   pRetds = ( HB_RETDS ) hb_GetProcAddress( "_hb_retds" );

   pRetds( szDate );
}

#undef hb_retd
void hb_retd( int iYear, int iMonth, int iDay ) /* returns a date */
{
#if defined( __cplusplus )
   static HB_RETD
#else
   static HB_RETD pRetd = NULL;
   if( ! pRetd )
#endif
   pRetd = ( HB_RETD ) hb_GetProcAddress( "_hb_retd" );

   pRetd( iYear, iMonth, iDay );
}

#undef hb_retdl
void hb_retdl( LONG lJulian )   /* returns a LONG value as a julian date */
{
#if defined( __cplusplus )
   static HB_RETDL
#else
   static HB_RETDL pRet = NULL;
   if( ! pRet )
#endif
   pRet = ( HB_RETDL ) hb_GetProcAddress( "_hb_retdl" );

   pRet( lJulian );
}

#undef hb_retl
void hb_retl( int iTrueFalse ) /*  returns a logical integer */
{
#if defined( __cplusplus )
   static HB_RETDL
#else
   static HB_RETDL pRet = NULL;
   if( ! pRet )
#endif
   pRet = ( HB_RETDL ) hb_GetProcAddress( "_hb_retl" );

   pRet( iTrueFalse );
}

#undef hb_retnd
void hb_retnd( double dNumber ) /* returns a double */
{
#if defined( __cplusplus )
   static HB_RETND
#else
   static HB_RETND pRet = NULL;
   if( ! pRet )
#endif
   pRet = ( HB_RETND ) hb_GetProcAddress( "_hb_retnd" );

   pRet( dNumber );
}

#undef hb_retni
void hb_retni( int iNumber )    /* returns a integer number */
{
#if defined( __cplusplus )
   static HB_RETNI
#else
   static HB_RETNI pRet = NULL;
   if( ! pRet )
#endif
   pRet = ( HB_RETNI ) hb_GetProcAddress( "_hb_retni" );

   pRet( iNumber );
}

#undef hb_retnl
void hb_retnl( LONG lNumber )   /* returns a LONG number */
{
#if defined( __cplusplus )
   static HB_RETNL
#else
   static HB_RETNL pRet = NULL;
   if( ! pRet )
#endif
   pRet = ( HB_RETNL ) hb_GetProcAddress( "_hb_retnl" );

   pRet( lNumber );
}

#undef hb_retns
void hb_retns( HB_ISIZ lNumber )   /* returns a LONG number */
{
#if defined( __cplusplus )
   static HB_RETNS
#else
   static HB_RETNS pRet = NULL;
   if( ! pRet )
#endif
   pRet = ( HB_RETNS ) hb_GetProcAddress( "_hb_retns" );

   pRet( lNumber );
}

#undef hb_retnlen
void hb_retnlen( double dNumber, int iWidth, int iDec ) /* returns a double, with specific width and decimals */
{
#if defined( __cplusplus )
   static HB_RETNLEN
#else
   static HB_RETNLEN pRet = NULL;
   if( ! pRet )
#endif
   pRet = ( HB_RETNLEN ) hb_GetProcAddress( "_hb_retnlen" );

   pRet( dNumber, iWidth, iDec );
}

#undef hb_retndlen
void hb_retndlen( double dNumber, int iWidth, int iDec ) /* returns a double, with specific width and decimals */
{
#if defined( __cplusplus )
   static HB_RETNDLEN
#else
   static HB_RETNDLEN pRet = NULL;
   if( ! pRet )
#endif
   pRet = ( HB_RETNDLEN ) hb_GetProcAddress( "_hb_retndlen" );

   pRet( dNumber, iWidth, iDec );
}

#undef hb_retnilen
void hb_retnilen( int iNumber, int iWidth ) /* returns a integer number, with specific width */
{
#if defined( __cplusplus )
   static HB_RETNILEN
#else
   static HB_RETNILEN pRet = NULL;
   if( ! pRet )
#endif
   pRet = ( HB_RETNILEN ) hb_GetProcAddress( "_hb_retnilen" );

   pRet( iNumber, iWidth );
}

#undef hb_retnllen
void hb_retnllen( LONG lNumber, int iWidth ) /* returns a LONG number, with specific width */
{
#if defined( __cplusplus )
   static HB_RETNLLEN
#else
   static HB_RETNLLEN pRet = NULL;
   if( ! pRet )
#endif
   pRet = ( HB_RETNLLEN ) hb_GetProcAddress( "_hb_retnilen" );

   pRet( lNumber, iWidth );
}

#undef hb_reta
void hb_reta( HB_SIZE ulLen )  /* returns an array with a specific length */
{
#if defined( __cplusplus )
   static HB_RETA
#else
   static HB_RETA pRet = NULL;
   if( ! pRet )
#endif
   pRet = ( HB_RETA ) hb_GetProcAddress( "_hb_reta" );

   pRet( ulLen );
}

#undef hb_parinfa
HB_SIZE hb_parinfa( int iParamNum, HB_SIZE uiArrayIndex ) /* retrieve length or element type of an array parameter */
{
#if defined( __cplusplus )
   static HB_PARINFA
#else
   static HB_PARINFA pParinfa = NULL;
   if( ! pParinfa )
#endif
   pParinfa = ( HB_PARINFA ) hb_GetProcAddress( "_hb_parinfa" );

   return pParinfa( iParamNum, uiArrayIndex );
}

#undef hb_parinfo
HB_SIZE hb_parinfo( int iParam ) /* Determine the param count or data type */
{
#if defined( __cplusplus )
   static HB_PARINFO
#else
   static HB_PARINFO pParinfo = NULL;
   if( ! pParinfo )
#endif
   pParinfo = ( HB_PARINFO ) hb_GetProcAddress( "_hb_parinfo" );

   return pParinfo( iParam );
}

#undef hb_parclen
HB_SIZE hb_parclen( int iParam, ... ) /* retrieve a string parameter length */
{
#if defined( __cplusplus )
   static HB_PARCLEN2
#else
   static HB_PARCLEN2 pParC = NULL;
   if( ! pParC )
#endif
   pParC = ( HB_PARCLEN2 ) hb_GetProcAddress( "_hb_parclen" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      return pParC( iParam, ulArrayIndex );
   }
   else
   {
      return ( ( HB_PARCLEN ) pParC )( iParam );
   }
}

#undef hb_parcsiz
HB_SIZE hb_parcsiz( int iParam, ... ) /* retrieve a by-reference string parameter length, including terminator */
{
#if defined( __cplusplus )
   static HB_PARCSIZ2
#else
   static HB_PARCSIZ2 pParcSiz = NULL;
   if( ! pParcSiz )
#endif
   pParcSiz = ( HB_PARCSIZ2 ) hb_GetProcAddress( "_hb_parcsiz" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      return pParcSiz( iParam, ulArrayIndex );
   }
   else
   {
      return ( ( HB_PARCSIZ ) pParcSiz )( iParam );
   }
}

#undef hb_pards
const char * hb_pards( int iParam, ... ) /* retrieve a date as a string yyyymmdd */
{
#if defined( __cplusplus )
   static HB_PARDS2
#else
   static HB_PARDS2 pParDs = NULL;
   if( ! pParDs )
#endif
   pParDs = ( HB_PARDS2 ) hb_GetProcAddress( "_hb_pards" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      return pParDs( iParam, ulArrayIndex );
   }
   else
   {
      return ( ( HB_PARDS ) pParDs )( iParam );
   }
}

#undef hb_pardsbuff
char * hb_pardsbuff( char * szDate, int iParam, ... ) /* retrieve a date as a string yyyymmdd */
{
#if defined( __cplusplus )
   static HB_PARDSBUFF2
#else
   static HB_PARDSBUFF2 pParDsBuff = NULL;
   if( ! pParDsBuff )
#endif
   pParDsBuff = ( HB_PARDSBUFF2 ) hb_GetProcAddress( "_hb_pardsbuff" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      return pParDsBuff( szDate, iParam, ulArrayIndex );
   }
   else
   {
      return ( ( HB_PARDSBUFF ) pParDsBuff )( szDate, iParam );
   }
}

#undef hb_parl
int hb_parl( int iParam, ... ) /* retrieve a logical parameter as an int */
{
#if defined( __cplusplus )
   static HB_PARL2
#else
   static HB_PARL2 pParL = NULL;
   if( ! pParL )
#endif
   pParL = ( HB_PARL2 ) hb_GetProcAddress( "_hb_parl" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      return pParL( iParam, ulArrayIndex );
   }
   else
   {
      return ( ( HB_PARL ) pParL )( iParam );
   }
}

#undef hb_parnd
double hb_parnd( int iParam, ... ) /* retrieve a numeric parameter as a double */
{
#if defined( __cplusplus )
   static HB_PARND2
#else
   static HB_PARND2 pParNd = NULL;
   if( ! pParNd )
#endif
   pParNd = ( HB_PARND2 ) hb_GetProcAddress( "_hb_parnd" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      return pParNd( iParam, ulArrayIndex );
   }
   else
   {
      return ( ( HB_PARND ) pParNd )( iParam );
   }
}

#undef hb_parni
int hb_parni( int iParam, ... ) /* retrieve a numeric parameter as a integer */
{
#if defined( __cplusplus )
   static HB_PARNI2
#else
   static HB_PARNI2 pParNi = NULL;
   if( ! pParNi )
#endif
   pParNi = ( HB_PARNI2 ) hb_GetProcAddress( "_hb_parni" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      return pParNi( iParam, ulArrayIndex );
   }
   else
   {
      return ( ( HB_PARNI ) pParNi )( iParam );
   }
}

#undef hb_parnl
LONG hb_parnl( int iParam, ... ) /* retrieve a numeric parameter as a LONG */
{
#if defined( __cplusplus )
   static HB_PARNL2
#else
   static HB_PARNL2 pParNl = NULL;
   if( ! pParNl )
#endif
   pParNl = ( HB_PARNL2 ) hb_GetProcAddress( "_hb_parnl" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      return pParNl( iParam, ulArrayIndex );
   }
   else
   {
      return ( ( HB_PARNL ) pParNl )( iParam );
   }
}

#undef hb_parns
HB_ISIZ hb_parns( int iParam, ... ) /* retrieve a numeric parameter as a LONG */
{
#if defined( __cplusplus )
   static HB_PARNS2
#else
   static HB_PARNS2 pParNS = NULL;
   if( ! pParNS )
#endif
   pParNS = ( HB_PARNS2 ) hb_GetProcAddress( "_hb_parns" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      return pParNS( iParam, ulArrayIndex );
   }
   else
   {
      return ( ( HB_PARNS ) pParNS )( iParam );
   }
}

#undef hb_storc
void hb_storc( const char * szText, int iParam, ... )
{
#if defined( __cplusplus )
   static HB_STORC2
#else
   static HB_STORC2 pStorC = NULL;
   if( ! pStorC )
#endif
   pStorC = ( HB_STORC2 ) hb_GetProcAddress( "_hb_storc" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      pStorC( szText, iParam, ulArrayIndex );
   }
   else
   {
      ( ( HB_STORC ) pStorC )( szText, iParam );
   }
}

#undef hb_storclen
void hb_storclen( const char * szText, HB_SIZE ulLen, int iParam, ... )
{
#if defined( __cplusplus )
   static HB_STORCLEN2
#else
   static HB_STORCLEN2 pStorC = NULL;
   if( ! pStorC )
#endif
   pStorC = ( HB_STORCLEN2 ) hb_GetProcAddress( "_hb_storclen" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      pStorC( szText, ulLen, iParam, ulArrayIndex );
   }
   else
   {
      ( ( HB_STORCLEN ) pStorC )( szText, ulLen, iParam );
   }
}

#undef hb_stords
void hb_stords( const char * szDate, int iParam, ... )
{
#if defined( __cplusplus )
   static HB_STORDS2
#else
   static HB_STORDS2 pStorDs = NULL;
   if( ! pStorDs )
#endif
   pStorDs = ( HB_STORDS2 ) hb_GetProcAddress( "_hb_stords" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      pStorDs( szDate, iParam, ulArrayIndex );
   }
   else
   {
      ( ( HB_STORDS ) pStorDs )( szDate, iParam );
   }
}

#undef hb_storl
void hb_storl( int iLogical, int iParam, ... )
{
#if defined( __cplusplus )
   static HB_STORL2
#else
   static HB_STORL2 pStorL = NULL;
   if( ! pStorL )
#endif
   pStorL = ( HB_STORL2 ) hb_GetProcAddress( "_hb_storl" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      pStorL( iLogical, iParam, ulArrayIndex );
   }
   else
   {
      ( ( HB_STORL ) pStorL )(  iLogical, iParam );
   }
}

#undef hb_storni
void hb_storni( int iValue, int iParam, ... )
{
#if defined( __cplusplus )
   static HB_STORNI2
#else
   static HB_STORNI2 pStorNi = NULL;
   if( ! pStorNi )
#endif
   pStorNi = ( HB_STORNI2 ) hb_GetProcAddress( "_hb_storni" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      pStorNi( iValue, iParam, ulArrayIndex );
   }
   else
   {
      ( ( HB_STORNI ) pStorNi )(  iValue, iParam );
   }
}

#undef hb_stornl
void hb_stornl( LONG lValue, int iParam, ... )
{
#if defined( __cplusplus )
   static HB_STORNL2
#else
   static HB_STORNL2 pStorNl = NULL;
   if( ! pStorNl )
#endif
   pStorNl = ( HB_STORNL2 ) hb_GetProcAddress( "_hb_stornl" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      pStorNl( lValue, iParam, ulArrayIndex );
   }
   else
   {
      ( ( HB_STORNL ) pStorNl )(  lValue, iParam );
   }
}

#undef hb_storns
int hb_storns( HB_ISIZ lValue, int iParam, ... )
{
#if defined( __cplusplus )
   static HB_STORNS2
#else
   static HB_STORNS2 pStorNS = NULL;
   if( ! pStorNS )
#endif
   pStorNS = ( HB_STORNS2 ) hb_GetProcAddress( "_hb_storns" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      pStorNS( lValue, iParam, ulArrayIndex );
      return 1;
   }
   else
   {
      ( ( HB_STORNS ) pStorNS )(  lValue, iParam );
   }

   return 0;
}

#undef hb_stornd
void hb_stornd( double dNumber, int iParam, ... )
{
#if defined( __cplusplus )
   static HB_STORND2
#else
   static HB_STORND2 pStorNd = NULL;
   if( ! pStorNd )
#endif
   pStorNd = ( HB_STORND2 ) hb_GetProcAddress( "_hb_stornd" );

   if( ( ( EXT_IS_ARRAY ) pExtIsArray )( iParam ) )
   {
      va_list  va;
      ULONG    ulArrayIndex;

      va_start( va, iParam );
      ulArrayIndex = va_arg( va, ULONG );
      va_end( va );

      pStorNd( dNumber, iParam, ulArrayIndex );
   }
   else
   {
      ( ( HB_STORND ) pStorNd )( dNumber, iParam );
   }
}

BOOL hb_arrayNew( PHB_ITEM pItem, HB_SIZE ulLen )  /* creates a new array */
{
#if defined( __cplusplus )
   static HB_ARRAYNEW
#else
   static HB_ARRAYNEW pArrayNew = NULL;
   if( ! pArrayNew )
#endif
   pArrayNew = ( HB_ARRAYNEW ) hb_GetProcAddress( "_hb_arrayNew" );

   return pArrayNew( pItem, ulLen );
}

HB_SIZE hb_arrayLen( PHB_ITEM pArray )  /* retrives the array len */
{
#if defined( __cplusplus )
   static HB_ARRAYLEN
#else
   static HB_ARRAYLEN pArrayLen = NULL;
   if( ! pArrayLen )
#endif
   pArrayLen = ( HB_ARRAYLEN ) hb_GetProcAddress( "_hb_arrayLen" );

   return pArrayLen( pArray );
}

BOOL hb_arrayIsObject( PHB_ITEM pArray )  /* retrives if the array is an object */
{
#if defined( __cplusplus )
   static HB_ARRAYISOBJECT
#else
   static HB_ARRAYISOBJECT pArrayIsObject = NULL;
   if( ! pArrayIsObject )
#endif
   pArrayIsObject = ( HB_ARRAYISOBJECT ) hb_GetProcAddress( "_hb_arrayIsObject" );

   return pArrayIsObject( pArray );
}

BOOL hb_arrayAdd( PHB_ITEM pArray, PHB_ITEM pItemValue )  /* add a new item to the end of an array item */
{
#if defined( __cplusplus )
   static HB_ARRAYADD
#else
   static HB_ARRAYADD pArrayAdd = NULL;
   if( ! pArrayAdd )
#endif
   pArrayAdd = ( HB_ARRAYADD ) hb_GetProcAddress( "_hb_arrayAdd" );

   return pArrayAdd( pArray, pItemValue );
}

BOOL hb_arrayIns( PHB_ITEM pArray, HB_SIZE ulIndex )  /* insert a nil item into an array, without changing the length */
{
#if defined( __cplusplus )
   static HB_ARRAYINS
#else
   static HB_ARRAYINS pArrayIns = NULL;
   if( ! pArrayIns )
#endif
   pArrayIns = ( HB_ARRAYINS ) hb_GetProcAddress( "_hb_arrayIns" );

   return pArrayIns( pArray, ulIndex );
}

BOOL hb_arrayDel( PHB_ITEM pArray, HB_SIZE ulIndex )  /* delete an array item, without changing length */
{
#if defined( __cplusplus )
   static HB_ARRAYDEL
#else
   static HB_ARRAYDEL pArrayDel = NULL;
   if( ! pArrayDel )
#endif
   pArrayDel = ( HB_ARRAYDEL ) hb_GetProcAddress( "_hb_arrayDel" );

   return pArrayDel( pArray, ulIndex );
}

BOOL hb_arraySize( PHB_ITEM pArray, HB_SIZE ulLen )  /* sets the array total length */
{
#if defined( __cplusplus )
   static HB_ARRAYSIZE
#else
   static HB_ARRAYSIZE pArraySize = NULL;
   if( ! pArraySize )
#endif
   pArraySize = ( HB_ARRAYSIZE ) hb_GetProcAddress( "_hb_arraySize" );

   return pArraySize( pArray, ulLen );
}

BOOL hb_arrayLast( PHB_ITEM pArray, PHB_ITEM pResult )  /* retrieve last item in an array */
{
#if defined( __cplusplus )
   static HB_ARRAYLAST
#else
   static HB_ARRAYLAST pArrayLast = NULL;
   if( ! pArrayLast )
#endif
   pArrayLast = ( HB_ARRAYLAST ) hb_GetProcAddress( "_hb_arrayLast" );

   return pArrayLast( pArray, pResult );
}

BOOL hb_arrayRelease( PHB_ITEM pArray )  /* releases an array - don't call it - use ItemRelease() !!! */
{
#if defined( __cplusplus )
   static HB_ARRAYRELEASE
#else
   static HB_ARRAYRELEASE pArrayRelease = NULL;
   if( ! pArrayRelease )
#endif
   pArrayRelease = ( HB_ARRAYRELEASE ) hb_GetProcAddress( "_hb_arrayRelease" );

   return pArrayRelease( pArray );
}

BOOL hb_arraySet( PHB_ITEM pArray, HB_SIZE ulIndex, PHB_ITEM pItem )  /* sets an array element */
{
#if defined( __cplusplus )
   static HB_ARRAYSET
#else
   static HB_ARRAYSET pArraySet = NULL;
   if( ! pArraySet )
#endif
   pArraySet = ( HB_ARRAYSET ) hb_GetProcAddress( "_hb_arraySet" );

   return pArraySet( pArray, ulIndex, pItem );
}

BOOL hb_arrayGet( PHB_ITEM pArray, HB_SIZE ulIndex, PHB_ITEM pItem )  /* retrieves an item */
{
#if defined( __cplusplus )
   static HB_ARRAYGET
#else
   static HB_ARRAYGET pArrayGet = NULL;
   if( ! pArrayGet )
#endif
   pArrayGet = ( HB_ARRAYGET ) hb_GetProcAddress( "_hb_arrayGet" );

   return pArrayGet( pArray, ulIndex, pItem );
}

#undef hb_xinit
void hb_xinit( void )                         /* Initialize fixed memory subsystem */
{
#if defined( __cplusplus )
   static HB_XINIT
#else
   static HB_XINIT pXinit = NULL;
   if( ! pXinit )
#endif
   pXinit = ( HB_XINIT ) hb_GetProcAddress( "_hb_xinit" );

   pXinit();
   return;
}

#undef hb_xexit
void hb_xexit( void )                         /* Deinitialize fixed memory subsystem */
{
#if defined( __cplusplus )
   static HB_XEXIT
#else
   static HB_XEXIT pXexit = NULL;
   if( ! pXexit )
#endif
   pXexit = ( HB_XEXIT ) hb_GetProcAddress( "_hb_xexit" );

   pXexit();
}

#undef hb_xalloc
void * hb_xalloc( HB_SIZE ulSize )                /* allocates memory, returns NULL on failure */
{
#if defined( __cplusplus )
   static HB_XALLOC
#else
   static HB_XALLOC pXalloc = NULL;
   if( ! pXalloc )
#endif
   pXalloc = ( HB_XALLOC ) hb_GetProcAddress( "_hb_xalloc" );

   return pXalloc( ulSize );
}

#undef hb_xgrab
void * hb_xgrab( HB_SIZE ulSize )                 /* allocates memory, exits on failure */
{
#if defined( __cplusplus )
   static HB_XGRAB
#else
   static HB_XGRAB pXgrab = NULL;
   if( ! pXgrab )
#endif
   pXgrab = ( HB_XGRAB ) hb_GetProcAddress( "_hb_xgrab" );

   return pXgrab( ulSize );
}

#undef hb_xfree
void hb_xfree( void * pMem )                  /* frees memory */
{
#if defined( __cplusplus )
   static HB_XFREE
#else
   static HB_XFREE pXfree = NULL;
   if( ! pXfree )
#endif
   pXfree = ( HB_XFREE ) hb_GetProcAddress( "_hb_xfree" );

   pXfree( pMem );
}

#undef hb_xrealloc
void * hb_xrealloc( void * pMem, HB_SIZE ulSize ) /* reallocates memory */
{
#if defined( __cplusplus )
   static HB_XREALLOC
#else
   static HB_XREALLOC pXrealloc = NULL;
   if( ! pXrealloc )
#endif
   pXrealloc = ( HB_XREALLOC ) hb_GetProcAddress( "_hb_xrealloc" );

   return ( void * ) pXrealloc( pMem, ulSize );
}

#undef hb_xsize
HB_SIZE hb_xsize( void * pMem )                  /* returns the size of an allocated memory block */
{
#if defined( __cplusplus )
   static HB_XSIZE
#else
   static HB_XSIZE pXsize = NULL;
   if( ! pXsize )
#endif
   pXsize = ( HB_XSIZE ) hb_GetProcAddress( "_hb_xsize" );

   return pXsize( ( void * ) pMem );
}

/*---------------------------------------------------------------------------*/
#undef hb_fsDelete
BOOL hb_fsDelete( const char * pszFileName )
{
#if defined( __cplusplus )
   static HB_FSDELETE
#else
   static HB_FSDELETE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( HB_FSDELETE ) hb_GetProcAddress( "_hb_fsDelete" );

   return pFunc( pszFileName );
}

/*---------------------------------------------------------------------------*/
#undef hb_fsWrite
USHORT hb_fsWrite( HB_FHANDLE hFileHandle, const void * pBuff, USHORT ulCount )
{
#if defined( __cplusplus )
   static HB_FSWRITE pFunc = NULL;
#else
   static HB_FSWRITE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( HB_FSWRITE ) hb_GetProcAddress( "_hb_fsWrite" );

   return pFunc( hFileHandle, pBuff, ulCount );
}

/*---------------------------------------------------------------------------*/
#undef hb_fsSeek
ULONG hb_fsSeek( HB_FHANDLE hFileHandle, LONG lOffset, USHORT uiMode )
{
#if defined( __cplusplus )
   static HB_FSSEEK
#else
   static HB_FSSEEK pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( HB_FSSEEK ) hb_GetProcAddress( "_hb_fsSeek" );

   return pFunc( hFileHandle, lOffset, uiMode );
}

/*---------------------------------------------------------------------------*/
#undef hb_fsCreate

HB_FHANDLE hb_fsCreate( const char * pszFileName, ULONG uiAttr )
{
#if defined( __cplusplus )
   static HB_FSCREATE
#else
   static HB_FSCREATE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( HB_FSCREATE ) hb_GetProcAddress( "_hb_fsCreate" );

   return pFunc( pszFileName, uiAttr );
}

/*---------------------------------------------------------------------------*/
#undef hb_fsRead
USHORT hb_fsRead( HB_FHANDLE hFileHandle, void * pBuff, USHORT ulCount )
{
#if defined( __cplusplus )
   static HB_FSREAD
#else
   static HB_FSREAD pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( HB_FSREAD ) hb_GetProcAddress( "_hb_fsRead" );

   return pFunc( hFileHandle, pBuff, ulCount );
}

/*---------------------------------------------------------------------------*/
#undef hb_fsOpen
HB_FHANDLE hb_fsOpen( const char * pszFileName, USHORT uiFlags )
{
#if defined( __cplusplus )
   static HB_FSOPEN
#else
   static HB_FSOPEN pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( HB_FSOPEN ) hb_GetProcAddress( "_hb_fsOpen" );

   return pFunc( pszFileName, uiFlags );
}

/*---------------------------------------------------------------------------*/
#undef hb_fsClose
void hb_fsClose( HB_FHANDLE hFileHandle  )
{
#if defined( __cplusplus )
   static HB_FSCLOSE
#else
   static HB_FSCLOSE pFunc = NULL;
   if( ! pFunc )
#endif
   pFunc = ( HB_FSCLOSE ) hb_GetProcAddress( "_hb_fsClose" );

   pFunc( hFileHandle );
}

HB_EXTERN_END

#endif /* HB_OS_WIN */
