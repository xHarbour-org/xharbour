/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler Harbour Portable Object (.HRB) generation
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 * www - http://www.harbour-project.org
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

#define SYM_NOLINK   0              /* Symbol does not have to be linked */
#define SYM_FUNC     1              /* Defined function                  */
#define SYM_EXTERN   2              /* Previously defined function       */

void hb_compGenPortObj( PHB_FNAME pFileName )
{
   char           szFileName[ HB_PATH_MAX ];
   PFUNCTION      pFunc /*= hb_comp_functions.pFirst */;
   PCOMSYMBOL     pSym = hb_comp_symbols.pFirst;
   HB_SYMBOLSCOPE hSymScope;
   ULONG          lPCodePos;
   LONG           lSymbols;
   HB_SIZE        ulCodeLength;
   FILE *         yyc;     /* file handle for C output */
   char           cInt[ 2 ];

   if( ! pFileName->szExtension )
      pFileName->szExtension = ".hrb";

   hb_fsFNameMerge( szFileName, pFileName );

   yyc = hb_fopen( szFileName, "wb" );

   if( ! yyc )
   {
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
   }

   hb_snprintf( hb_comp_szMsgBuf, SIZE_OF_SZMSGBUF, "Generating Harbour Portable Object output to \'%s\'... ", szFileName  );
   hb_compOutStd( hb_comp_szMsgBuf );

   /* writes the symbol table */

   lSymbols = 0;                /* Count number of symbols */

   while( pSym )
   {
      lSymbols++;
      pSym = pSym->pNext;
   }

   fputc( ( BYTE ) 192, yyc );
   fputs( "HRB", yyc );

   HB_PUT_LE_UINT16( cInt, HB_HRB_VER );

   fputc( cInt[ 0 ], yyc );
   fputc( cInt[ 1 ], yyc );

   if( hb_comp_UsedNamespaces.pFirst )
   {
      PNAMESPACE pNamespace = hb_comp_UsedNamespaces.pFirst;

      do
      {
         if( ( pNamespace->type & NSTYPE_SPACE ) )
         {
            if( ( pNamespace->type & NSTYPE_USED ) == NSTYPE_USED )
            {
               fputs( pNamespace->szFullPath, yyc );
               fputc( 0, yyc );
            }
         }

         pNamespace = pNamespace->pNext;
      }
      while( pNamespace );
   }
   fputc( 0, yyc );

   fputc( ( BYTE ) ( ( lSymbols ) & 255 ), yyc );       /* Write number symbols */
   fputc( ( BYTE ) ( ( lSymbols >> 8 ) & 255 ), yyc );
   fputc( ( BYTE ) ( ( lSymbols >> 16 ) & 255 ), yyc );
   fputc( ( BYTE ) ( ( lSymbols >> 24 ) & 255 ), yyc );

   pSym = hb_comp_symbols.pFirst;

   while( pSym )
   {
      pFunc = NULL;

      if( ( pSym->iFlags & SYMF_FUNCALL ) == SYMF_FUNCALL )
      {
#if defined( HB_COMP_DEBUG )
         printf( "Sym: '%s' Namespace: '%s', Scope: %i\n", pSym->szName, pSym->szNamespace, pSym->cScope );
#endif
         if( ( ( pSym->iFlags & SYMF_NS_EXPLICITPATH ) == SYMF_NS_EXPLICITPATH ) || ( ( pSym->iFlags & SYMF_NS_EXPLICITPTR ) == SYMF_NS_EXPLICITPTR ) )
         {
            if( ( pSym->iFlags & SYMF_NS_EXPLICITPATH ) == SYMF_NS_EXPLICITPATH )
               pFunc = hb_compFunctionFind( pSym->szName, pSym->Namespace, SYMF_NS_EXPLICITPATH );
            else
               pFunc = hb_compFunctionFind( pSym->szName, pSym->Namespace, SYMF_NS_EXPLICITPTR );

            if( pFunc )
            {
               if( ( pSym->cScope & HB_FS_LOCAL ) )
               {
                  assert( ( ( pFunc->pNamespace->type & NSTYPE_RUNTIME ) == NSTYPE_RUNTIME ) ||
                          ( ( pFunc->pNamespace->type & NSTYPE_OPTIONAL ) == NSTYPE_OPTIONAL ) );
                  assert( ( pFunc->cScope & HB_FS_PUBLIC ) );
               }
               else
               {
                  assert( ( ( pFunc->pNamespace->type & NSTYPE_RUNTIME ) != NSTYPE_RUNTIME ) ||
                          ( ( pFunc->pNamespace->type & NSTYPE_OPTIONAL ) != NSTYPE_OPTIONAL ) );

                  assert( ( pSym->cScope & ( HB_FS_PUBLIC | HB_FS_STATIC ) ) == 0 );
                  pSym->cScope |= ( HB_FS_LOCAL | HB_FS_STATIC );
               }
            }
            else
            {
               if( ! ( ( pSym->cScope & HB_FS_DEFERRED ) == HB_FS_DEFERRED ) )
                  pSym->cScope |= ( HB_FS_INDIRECT | HB_FS_PUBLIC );
            }
         }
         else if( ( pSym->iFlags & SYMF_NS_RESOLVE ) == SYMF_NS_RESOLVE )
         {
            pFunc = hb_compFunctionResolve( pSym->szName, ( PNAMESPACE ) pSym->Namespace, pSym );
#if defined( __XCC__ )
            if( pFunc == ( PFUNCTION ) 1 )
#else
            if( pFunc == ( PFUNCTION ) ( HB_LONG ) 1 )
#endif
            {
               /* Resolved to external member. */
               pFunc = NULL;

               /* TODO: Error message */
            }
         }
         else if( ( pSym->cScope & HB_FS_LOCAL ) != HB_FS_LOCAL )
         {
            /* is it a function defined in this module */
            if( hb_compFunctionFind( pSym->szName, pSym->Namespace, pSym->iFlags ) )
               assert( 0 );
            else
            {
               assert( ( pSym->cScope & SYMF_STATIC ) == 0 );
               pSym->cScope |= HB_FS_PUBLIC;
            }
         }
      }

      if( pSym->Namespace )
      {
         if( ( pSym->iFlags & SYMF_NS_EXPLICITPTR ) == SYMF_NS_EXPLICITPTR )
         {
            if( ( ( ( PNAMESPACE ) pSym->Namespace )->type & NSTYPE_OPTIONAL ) != NSTYPE_OPTIONAL )
            {
               fputs( ( ( PNAMESPACE ) pSym->Namespace )->szFullPath, yyc );
               fputc( '.', yyc );
            }
         }
         else if( ( pSym->iFlags & SYMF_NS_EXPLICITPATH ) == SYMF_NS_EXPLICITPATH )
         {
            if( pFunc == NULL || ( ( pFunc->pNamespace->type & NSTYPE_OPTIONAL ) != NSTYPE_OPTIONAL ) )
            {
               fputs( ( char * ) pSym->Namespace, yyc );
               fputc( '.', yyc );
            }
         }
         else
            assert( 0 );
      }

      if( pSym->szName[ 0 ] == '<' )
         fputs( "(_INITLINES)", yyc );
      else
         fputs( pSym->szName, yyc );

      fputc( 0, yyc );

      hSymScope = pSym->cScope;

      fputc( ( BYTE ) ( ( hSymScope ) & 255 ), yyc );
      fputc( ( BYTE ) ( ( hSymScope >> 8 ) & 255 ), yyc );
      fputc( ( BYTE ) ( ( hSymScope >> 16 ) & 255 ), yyc );
      fputc( ( BYTE ) ( ( hSymScope >> 24 ) & 255 ), yyc );

      if( ( pSym->cScope & HB_FS_LOCAL ) == HB_FS_LOCAL || ( pSym->cScope & HB_FS_INITEXIT ) == HB_FS_INITEXIT )
         fputc( SYM_FUNC, yyc );
      else
      {
         if( ( pSym->iFlags & SYMF_FUNCALL ) == SYMF_FUNCALL ) /* hb_compFunCallFind( pSym->szName, pSym->Namespace, pSym->iFlags ) ) */
            fputc( SYM_EXTERN, yyc );
         else
            fputc( SYM_NOLINK, yyc );
      }

      pSym = pSym->pNext;
   }

   pFunc = hb_comp_functions.pFirst;

   if( ! hb_comp_bStartProc )
      pFunc = pFunc->pNext;

   lSymbols = 0;                /* Count number of symbols */

   while( pFunc )
   {
      lSymbols++;
      pFunc = pFunc->pNext;
   }

   fputc( ( BYTE ) ( ( lSymbols ) & 255 ), yyc );       /* Write number symbols */
   fputc( ( BYTE ) ( ( lSymbols >> 8 ) & 255 ), yyc );
   fputc( ( BYTE ) ( ( lSymbols >> 16 ) & 255 ), yyc );
   fputc( ( BYTE ) ( ( lSymbols >> 24 ) & 255 ), yyc );

   /* Generate functions data
    */
   pFunc = hb_comp_functions.pFirst;

   if( ! hb_comp_bStartProc )
      pFunc = pFunc->pNext; /* No implicit starting procedure */

   while( pFunc )
   {
      if( pFunc->pNamespace )
      {
         if( ( pFunc->pNamespace->type & NSTYPE_OPTIONAL ) != NSTYPE_OPTIONAL )
         {
            fputs( pFunc->pNamespace->szFullPath, yyc );
            fputc( '.', yyc );
         }
      }

      if( pFunc->szName[ 0 ] == '<' )
         fputs( "(_INITLINES)", yyc );
      else
         fputs( pFunc->szName, yyc );

      fputc( 0, yyc );

      ulCodeLength = pFunc->lPCodePos;
      fputc( ( BYTE ) ( ( ulCodeLength ) & 255 ), yyc );       /* Write size */
      fputc( ( BYTE ) ( ( ulCodeLength >> 8 ) & 255 ), yyc );
      fputc( ( BYTE ) ( ( ulCodeLength >> 16 ) & 255 ), yyc );
      fputc( ( BYTE ) ( ( ulCodeLength >> 24 ) & 255 ), yyc );

      lPCodePos = 0;

      while( lPCodePos < pFunc->lPCodePos )
         fputc( pFunc->pCode[ lPCodePos++ ], yyc );

      pFunc = pFunc->pNext;
   }

   fclose( yyc );

   hb_compOutStd( "Done.\n" );
}

