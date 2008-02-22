/*
 * $Id: genhrb.c,v 1.7 2008/02/14 19:38:40 ronpinkas Exp $
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

#define SYM_NOLINK  0              /* Symbol does not have to be linked */
#define SYM_FUNC    1              /* Defined function                  */
#define SYM_EXTERN  2              /* Previously defined function       */

void hb_compGenPortObj( PHB_FNAME pFileName )
{
   char szFileName[ _POSIX_PATH_MAX ];
   PFUNCTION pFunc /*= hb_comp_functions.pFirst */;
   PCOMSYMBOL pSym = hb_comp_symbols.pFirst;
   HB_SYMBOLSCOPE hSymScope;
   ULONG lPCodePos;
   LONG lSymbols;
   ULONG ulCodeLength;
   FILE * yyc;             /* file handle for C output */

   if( ! pFileName->szExtension )
   {
      pFileName->szExtension = ".hrb";
   }

   hb_fsFNameMerge( szFileName, pFileName );

   yyc = hb_fopen( szFileName, "wb" );

   if( ! yyc )
   {
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
   }

   if( ! hb_comp_bQuiet )
   {
      printf( "Generating Harbour Portable Object output to \'%s\'... ", szFileName );
      fflush( stdout );
   }

   /* writes the symbol table */

   lSymbols = 0;                /* Count number of symbols */

   while( pSym )
   {
      lSymbols++;
      pSym = pSym->pNext;
   }

   fputc( ( BYTE ) 192, yyc );
   fputs( "HRB", yyc );
   fputc( 2, yyc );
   fputc( 0, yyc );

   fputc( ( BYTE ) ( ( lSymbols       ) & 255 ), yyc ); /* Write number symbols */
   fputc( ( BYTE ) ( ( lSymbols >> 8  ) & 255 ), yyc );
   fputc( ( BYTE ) ( ( lSymbols >> 16 ) & 255 ), yyc );
   fputc( ( BYTE ) ( ( lSymbols >> 24 ) & 255 ), yyc );

   pSym = hb_comp_symbols.pFirst;

   while( pSym )
   {
      pFunc = NULL;

      if( ( pSym->iFlags & SYMF_FUNCALL ) == SYMF_FUNCALL )
      {
         //printf( "Sym: '%s' Namespace: '%s', Scope: %i\n", pSym->szName, pSym->szNamespace, pSym->cScope );

         if( ( ( pSym->iFlags & SYMF_NS_EXPLICITPATH ) == SYMF_NS_EXPLICITPATH ) || ( ( pSym->iFlags & SYMF_NS_EXPLICITPTR ) == SYMF_NS_EXPLICITPTR ) )
         {
            if( ( pSym->iFlags & SYMF_NS_EXPLICITPATH ) == SYMF_NS_EXPLICITPATH )
            {
               pFunc = hb_compFunctionFind( pSym->szName, pSym->Namespace, SYMF_NS_EXPLICITPATH );
            }
            else
            {
               pFunc = hb_compFunctionFind( pSym->szName, pSym->Namespace, SYMF_NS_EXPLICITPTR );
            }

            if( pFunc )
            {
               pSym->cScope |= HB_FS_LOCAL;

               if( ( pFunc->pNamespace->type & NSTYPE_RUNTIME ) == NSTYPE_RUNTIME )
               {
               }
               else if( ( pFunc->pNamespace->type & NSTYPE_IMPLEMENTS ) == NSTYPE_IMPLEMENTS )
               {
               }
               else if( ( pFunc->pNamespace->type & NSTYPE_OPTIONAL ) == NSTYPE_OPTIONAL )
               {
               }
               else
               {
                  pSym->cScope |= HB_FS_STATIC;
               }
            }
            else
            {
               if( ! ( ( pSym->cScope & HB_FS_DEFERRED ) == HB_FS_DEFERRED ) )
               {
                  pSym->cScope |= HB_FS_INDIRECT;
               }
            }
         }
         else if( ( pSym->iFlags & SYMF_NS_RESOLVE ) == SYMF_NS_RESOLVE )
         {
            pFunc = hb_compFunctionResolve( pSym->szName, (PNAMESPACE) pSym->Namespace, pSym );

            if( pFunc == (PFUNCTION) 1 )
            {
               // Resolved to external member.
               pFunc = NULL;

               //TODO: Error message
            }
            else if( pFunc && pFunc->pNamespace )
            {
               if( ( pFunc->pNamespace->type & NSTYPE_OPTIONAL ) != NSTYPE_OPTIONAL )
               {
                  pSym->cScope |= HB_FS_STATIC;
               }

               pSym->cScope |= HB_FS_LOCAL;

               pSym->iFlags &= ~SYMF_NS_RESOLVE;
               pSym->iFlags |= SYMF_NS_EXPLICITPTR;

               pSym->Namespace = (void *) pFunc->pNamespace;
            }
            else
            {
               pSym->iFlags &= ~SYMF_NS_RESOLVE;
               pSym->iFlags |= SYMF_FUNCALL;

               pSym->Namespace = NULL;

               if( pFunc == NULL )
               {
                  pFunc = hb_compFunctionFind( pSym->szName, NULL, SYMF_FUNCALL );

                  if( pFunc )
                  {
                     pSym->cScope |= HB_FS_LOCAL;
                  }
               }
            }
         }
         else if( ( pSym->iFlags & SYMF_NS_EXPLICITPTR ) == SYMF_NS_EXPLICITPTR )
         {
            pFunc = hb_compFunctionFind( pSym->szName, pSym->Namespace, SYMF_NS_EXPLICITPTR );
         }
         else if( ( pSym->cScope & HB_FS_LOCAL ) == HB_FS_LOCAL )
         {
            pFunc = hb_compFunctionFind( pSym->szName, pSym->Namespace, pSym->iFlags );
         }
         else if( ( pSym->cScope & HB_FS_LOCAL ) != HB_FS_LOCAL )
         {
            pFunc = hb_compFunctionFind( pSym->szName, pSym->Namespace, pSym->iFlags );

            /* is it a function defined in this module */
            if( pFunc )
            {
               pSym->cScope |= HB_FS_LOCAL;
            }
         }
      }

      if( pSym->Namespace )
      {
         if( ( pSym->iFlags & SYMF_NS_EXPLICITPTR ) == SYMF_NS_EXPLICITPTR )
         {
            if( ( ( (PNAMESPACE) pSym->Namespace )->type & NSTYPE_OPTIONAL ) != NSTYPE_OPTIONAL )
            {
               fputs( ( (PNAMESPACE) pSym->Namespace )->szFullPath, yyc );
               fputc( '.', yyc );
            }
         }
         else if( ( pSym->iFlags & SYMF_NS_EXPLICITPATH ) == SYMF_NS_EXPLICITPATH )
         {
            if( pFunc == NULL || ( ( pFunc->pNamespace->type & NSTYPE_OPTIONAL ) != NSTYPE_OPTIONAL ) )
            {
               fputs( (char *) pSym->Namespace, yyc );
               fputc( '.', yyc );
            }
         }
      }

      if ( pSym->szName[ 0 ] == '<' )
      {
         fputs( "(_INITLINES)", yyc );
      }
      else
      {
         fputs( pSym->szName, yyc );
      }
      fputc( 0, yyc );

      hSymScope = pSym->cScope;

      if( ( hSymScope & ( HB_FS_STATIC | HB_FS_INITEXIT ) ) != 0 )
      {
         hSymScope &= ~HB_FS_PUBLIC;
      }

      fputc( ( BYTE ) ( ( hSymScope       ) & 255 ), yyc );
      fputc( ( BYTE ) ( ( hSymScope >> 8  ) & 255 ), yyc );
      fputc( ( BYTE ) ( ( hSymScope >> 16 ) & 255 ), yyc );
      fputc( ( BYTE ) ( ( hSymScope >> 24 ) & 255 ), yyc );

      /* specify the function address if it is a defined function or a
         external called function */
      if( pFunc ) /* is it a defined function ? */
      {
         fputc( SYM_FUNC, yyc );
      }
      else
      {
         if( ( pSym->iFlags & SYMF_FUNCALL ) == SYMF_FUNCALL )//hb_compFunCallFind( pSym->szName, pSym->Namespace, pSym->iFlags ) )
         {
            fputc( SYM_EXTERN, yyc );
         }
         else
         {
            fputc( SYM_NOLINK, yyc );
         }
      }

      pSym = pSym->pNext;
   }

   pFunc = hb_comp_functions.pFirst;

   if( ! hb_comp_bStartProc )
   {
      pFunc = pFunc->pNext;
   }

   lSymbols = 0;                /* Count number of symbols */

   while( pFunc )
   {
      lSymbols++;
      pFunc = pFunc->pNext;
   }

   fputc( ( BYTE ) ( ( lSymbols       ) & 255 ), yyc ); /* Write number symbols */
   fputc( ( BYTE ) ( ( lSymbols >> 8  ) & 255 ), yyc );
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

      if ( pFunc->szName[ 0 ] == '<' )
      {
         fputs( "(_INITLINES)", yyc );
      }
      else
      {
         fputs( pFunc->szName, yyc );
      }
      fputc( 0, yyc );

      ulCodeLength = pFunc->lPCodePos;
      fputc( ( BYTE ) ( ( ulCodeLength       ) & 255 ), yyc ); /* Write size */
      fputc( ( BYTE ) ( ( ulCodeLength >> 8  ) & 255 ), yyc );
      fputc( ( BYTE ) ( ( ulCodeLength >> 16 ) & 255 ), yyc );
      fputc( ( BYTE ) ( ( ulCodeLength >> 24 ) & 255 ), yyc );

      lPCodePos = 0;
      while( lPCodePos < pFunc->lPCodePos )
         fputc( pFunc->pCode[ lPCodePos++ ], yyc );

      pFunc = pFunc->pNext;
   }

   fclose( yyc );

   if( ! hb_comp_bQuiet )
      printf( "Done.\n" );
}

