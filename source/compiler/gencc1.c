/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler C source generation
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#include <assert.h>
#include <time.h>

#include "hbcomp.h"
#include "hbdate.h"
#include "hbexemem.h"

static int hb_comp_iBaseLine;

extern void hb_compGenCReadable( PFUNCTION pFunc, FILE * yyc );
extern void hb_compGenCCompact( PFUNCTION pFunc, FILE * yyc );

/* helper structure to pass information */
typedef struct HB_stru_genc_info
{
   FILE * yyc;
   BOOL   bVerbose;
   USHORT iNestedCodeblock;
} HB_GENC_INFO, * PHB_GENC_INFO;

#define HB_GENC_FUNC( func ) HB_PCODE_FUNC( func, PHB_GENC_INFO )

typedef HB_GENC_FUNC ( HB_GENC_FUNC_ );
typedef HB_GENC_FUNC_ * PHB_GENC_FUNC;

static HB_GENC_FUNC( hb_p_and )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_AND,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_arraypush )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ARRAYPUSH,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_arraypop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ARRAYPOP,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_dec )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DEC,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_arraydim )
{
   fprintf( cargo->yyc, "\tHB_P_ARRAYDIM, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_divide )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DIVIDE,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_do )
{
   fprintf( cargo->yyc, "\tHB_P_DO, %i, %i,\n",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   return 3;
}

static HB_GENC_FUNC( hb_p_doshort )
{
   fprintf( cargo->yyc, "\tHB_P_DOSHORT, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );

   return 2;
}

static HB_GENC_FUNC( hb_p_duplicate )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DUPLICATE,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_dupltwo )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DUPLTWO,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_equal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_EQUAL,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_exactlyequal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_EXACTLYEQUAL,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_endblock )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   --cargo->iNestedCodeblock;
   fprintf( cargo->yyc, "\tHB_P_ENDBLOCK,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_endproc )
{
   if( ( lPCodePos + 1 ) == pFunc->lPCodePos )
      fprintf( cargo->yyc, "\tHB_P_ENDPROC\n" );
   else
      fprintf( cargo->yyc, "\tHB_P_ENDPROC,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_false )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FALSE,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_fortest )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FORTEST,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_frame )
{
   fprintf( cargo->yyc, "\tHB_P_FRAME, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* locals, params */" );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_funcptr )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FUNCPTR,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_function )
{
   fprintf( cargo->yyc, "\tHB_P_FUNCTION, %i, %i,\n",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   return 3;
}

static HB_GENC_FUNC( hb_p_functionshort )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FUNCTIONSHORT, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );

   return 2;
}

static HB_GENC_FUNC( hb_p_arraygen )
{
   fprintf( cargo->yyc, "\tHB_P_ARRAYGEN, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_hashgen )
{
   fprintf( cargo->yyc, "\tHB_P_HASHGEN, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_greater )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_GREATER,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_greaterequal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_GREATEREQUAL,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_inc )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_INC,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_instring )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_INSTRING,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_jumpnear )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPNEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      LONG lOffset = ( signed char ) ( pFunc->pCode[ lPCodePos + 1 ] );

      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_jump )
{
   fprintf( cargo->yyc, "\tHB_P_JUMP, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_jumpfar )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPFAR, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      fprintf( cargo->yyc, "\t/* %li (abs: %08li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 4;
}

static HB_GENC_FUNC( hb_p_jumpfalsenear )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPFALSENEAR, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      LONG lOffset = ( signed char ) ( pFunc->pCode[ lPCodePos + 1 ] );

      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_jumpfalse )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPFALSE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_jumpfalsefar )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPFALSEFAR, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      fprintf( cargo->yyc, "\t/* %li (abs: %08li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 4;
}

static HB_GENC_FUNC( hb_p_jumptruenear )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPTRUENEAR, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      LONG lOffset = ( signed char ) ( pFunc->pCode[ lPCodePos + 1 ] );

      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_jumptrue )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPTRUE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_jumptruefar )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPTRUEFAR, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      fprintf( cargo->yyc, "\t/* %li (abs: %08li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 4;
}

static HB_GENC_FUNC( hb_p_less )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_LESS,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_lessequal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_LESSEQUAL,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_line )
{
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "/* %05li */ ", ( long int ) lPCodePos );
   else
      fprintf( cargo->yyc, "\t" );

   fprintf( cargo->yyc, "HB_P_LINE, %i, %i,", pFunc->pCode[ lPCodePos + 1 ], pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_localname )
{
   HB_SIZE ulStart = lPCodePos;

   fprintf( cargo->yyc, "\tHB_P_LOCALNAME, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", ( char * ) pFunc->pCode + lPCodePos + 3 );

   fprintf( cargo->yyc, "\n" );

   lPCodePos += 3;
   while( pFunc->pCode[ lPCodePos ] )
   {
      char chr = pFunc->pCode[ lPCodePos++ ];

      if( chr == '\'' || chr == '\\' )
         fprintf( cargo->yyc, " \'\\%c\',", chr );
      else
         fprintf( cargo->yyc, " \'%c\',", chr );
   }
   fprintf( cargo->yyc, " 0,\n" );

   return lPCodePos - ulStart + 1;
}

static HB_GENC_FUNC( hb_p_macropop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPOP, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );

   return 2;
}

static HB_GENC_FUNC( hb_p_macropopaliased )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPOPALIASED, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );

   return 2;
}

static HB_GENC_FUNC( hb_p_macropush )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSH, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );

   return 2;
}

static HB_GENC_FUNC( hb_p_macropusharg )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHARG, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );

   return 2;
}

static HB_GENC_FUNC( hb_p_macropushlist )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHLIST, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );

   return 2;
}

static HB_GENC_FUNC( hb_p_macropushindex )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHINDEX, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );

   return 2;
}

static HB_GENC_FUNC( hb_p_macropushpare )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHPARE, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );

   return 2;
}

static HB_GENC_FUNC( hb_p_macropushaliased )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHALIASED, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );

   return 2;
}

static HB_GENC_FUNC( hb_p_macrosymbol )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROSYMBOL,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_macrotext )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROTEXT,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_message )
{
   fprintf( cargo->yyc, "\tHB_P_MESSAGE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_minus )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MINUS,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_modulename )
{
   HB_SIZE ulStart = lPCodePos;

   fprintf( cargo->yyc, "\tHB_P_MODULENAME," );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", ( char * ) pFunc->pCode + lPCodePos + 1 );

   fprintf( cargo->yyc, "\n" );

   lPCodePos++;
   while( pFunc->pCode[ lPCodePos ] )
   {
      char chr = pFunc->pCode[ lPCodePos++ ];
      if( chr == '\'' || chr == '\\' )
         fprintf( cargo->yyc, " \'\\%c\',", chr );
      else
         fprintf( cargo->yyc, " \'%c\',", chr );
   }
   fprintf( cargo->yyc, " 0,\n" );

   return lPCodePos - ulStart + 1;
}

static HB_GENC_FUNC( hb_p_modulus )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MODULUS,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_mult )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MULT,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_negate )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_NEGATE,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_not )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_NOT,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_notequal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_NOTEQUAL,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_or )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_OR,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_parameter )
{
   fprintf( cargo->yyc, "\tHB_P_PARAMETER, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );

   fprintf( cargo->yyc, "\n" );

   return 4;
}

static HB_GENC_FUNC( hb_p_plus )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PLUS,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_pop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_POP,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_popalias )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_POPALIAS,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_popaliasedfield )
{
   fprintf( cargo->yyc, "\tHB_P_POPALIASEDFIELD, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_popaliasedfieldnear )
{
   fprintf( cargo->yyc, "\tHB_P_POPALIASEDFIELDNEAR, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] )->szName );

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_popaliasedvar )
{
   fprintf( cargo->yyc, "\tHB_P_POPALIASEDVAR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_popfield )
{
   fprintf( cargo->yyc, "\tHB_P_POPFIELD, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_poplocal )
{
   fprintf( cargo->yyc, "\tHB_P_POPLOCAL, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      int iVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */
      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         else
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
      }
      else
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_poplocalnear )
{
   fprintf( cargo->yyc, "\tHB_P_POPLOCALNEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      int iVar = ( int ) ( signed char ) pFunc->pCode[ lPCodePos + 1 ];

      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */
      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         else
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
      }
      else
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_popmemvar )
{
   fprintf( cargo->yyc, "\tHB_P_POPMEMVAR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_popstatic )
{
   fprintf( cargo->yyc, "\tHB_P_POPSTATIC, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      PVAR      pVar;
      PFUNCTION pTmp = hb_comp_functions.pFirst;
      USHORT    wVar = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
         pTmp = pTmp->pNext;

      pVar = hb_compVariableFind( pTmp->pStatics, ( USHORT ) ( wVar - pTmp->iStaticsBase ) );

      fprintf( cargo->yyc, "\t/* %s */", pVar->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_popvariable )
{
   fprintf( cargo->yyc, "\tHB_P_POPVARIABLE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_power )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_POWER,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_pushalias )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHALIAS,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_pushaliasedfield )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHALIASEDFIELD, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_pushaliasedfieldnear )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHALIASEDFIELDNEAR, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] )->szName );

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_pushaliasedvar )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHALIASEDVAR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_pushblock )
{
   USHORT  wVar, w;
   HB_SIZE ulStart = lPCodePos;

   ++cargo->iNestedCodeblock;

   fprintf( cargo->yyc, "\tHB_P_PUSHBLOCK, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );

   fprintf( cargo->yyc, "\n" );

   w = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 3 ] ) );
   fprintf( cargo->yyc, "\t%i, %i,",
            pFunc->pCode[ lPCodePos + 3 ],
            pFunc->pCode[ lPCodePos + 4 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* number of local parameters (%i) */", w );

   fprintf( cargo->yyc, "\n" );

   wVar = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 5 ] ) );

   fprintf( cargo->yyc, "\t%i, %i,",
            pFunc->pCode[ lPCodePos + 5 ],
            pFunc->pCode[ lPCodePos + 6 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* number of local variables (%i) */", wVar );

   fprintf( cargo->yyc, "\n" );

   lPCodePos += 7;  /* codeblock size + number of parameters + number of local variables */

   /* create the table of referenced local variables */
   while( wVar-- )
   {
      w = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos ] ) );
      fprintf( cargo->yyc, "\t%i, %i,", pFunc->pCode[ lPCodePos ], pFunc->pCode[ lPCodePos + 1 ] );

      /* NOTE:
       * When a codeblock is used to initialize a static variable
       * the names of local variables cannot be determined
       * because at the time of C code generation we don't know
       * in which function was defined this local variable
       */
      if( ( pFunc->cScope & HB_FS_INITEXIT ) != HB_FS_INITEXIT )
      {
         if( cargo->bVerbose )
            fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, w )->szName );
      }

      fprintf( cargo->yyc, "\n" );

      lPCodePos += 2;
   }

   return lPCodePos - ulStart;
}

static HB_GENC_FUNC( hb_p_pushblockshort )
{
   ++cargo->iNestedCodeblock;

   fprintf( cargo->yyc, "\tHB_P_PUSHBLOCKSHORT, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", pFunc->pCode[ lPCodePos + 1 ] );

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_pushdouble )
{
   int i;

   fprintf( cargo->yyc, "\tHB_P_PUSHDOUBLE," );
   ++lPCodePos;
   for( i = 0; i < ( int ) ( sizeof( double ) + sizeof( BYTE ) + sizeof( BYTE ) ); ++i )
      fprintf( cargo->yyc, " %i,", ( ( BYTE * ) pFunc->pCode )[ lPCodePos + i ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %.*f, %d, %d */",
               *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) + sizeof( BYTE ) ] ) ),
               HB_PCODE_MKDOUBLE( &( pFunc->pCode[ lPCodePos ] ) ),
               *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) ] ) ),
               *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) + sizeof( BYTE ) ] ) ) );
   }

   fprintf( cargo->yyc, "\n" );

   return sizeof( double ) + sizeof( BYTE ) + sizeof( BYTE ) + 1;
}

static HB_GENC_FUNC( hb_p_pushfield )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHFIELD, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_pushbyte )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHBYTE, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", ( signed char ) pFunc->pCode[ lPCodePos + 1 ] );

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_pushint )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHINT, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_pushlocal )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLOCAL, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      int iVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */
      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         else
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
      }
      else
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_pushlocalnear )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLOCALNEAR, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      int iVar = ( int ) ( signed char ) pFunc->pCode[ lPCodePos + 1 ];

      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         else
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
      }
      else
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_localnearinc )
{
   fprintf( cargo->yyc, "\tHB_P_LOCALNEARINC, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      int iVar = ( int ) ( signed char ) pFunc->pCode[ lPCodePos + 1 ];

      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         else
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
      }
      else
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_localneardec )
{
   fprintf( cargo->yyc, "\tHB_P_LOCALNEARDEC, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      int iVar = ( int ) ( signed char ) pFunc->pCode[ lPCodePos + 1 ];

      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         else
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
      }
      else
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_pushlocalnearinc )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLOCALNEARINC, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      int iVar = ( int ) ( signed char ) pFunc->pCode[ lPCodePos + 1 ];

      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         else
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
      }
      else
      {
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_pushlocalneardec )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLOCALNEARDEC, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      int iVar = ( int ) ( signed char ) pFunc->pCode[ lPCodePos + 1 ];

      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         else
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
      }
      else
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_pushlocalref )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLOCALREF, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      SHORT iVar = HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */
      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         else
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
      }
      else
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_pushlong )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLONG, %i, %i, %i, %i, ",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ],
            pFunc->pCode[ lPCodePos + 4 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %li */", HB_PCODE_MKLONG( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );

   fprintf( cargo->yyc, "\n" );

   return 5;
}

static HB_GENC_FUNC( hb_p_pushlonglong )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLONGLONG, %i, %i, %i, %i, %i, %i, %i, %i, ",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ],
            pFunc->pCode[ lPCodePos + 4 ],
            pFunc->pCode[ lPCodePos + 5 ],
            pFunc->pCode[ lPCodePos + 6 ],
            pFunc->pCode[ lPCodePos + 7 ],
            pFunc->pCode[ lPCodePos + 8 ] );

   if( cargo->bVerbose )
   {
#ifdef HB_LONG_LONG_OFF
      fprintf( cargo->yyc, "\t/* %f */", HB_PCODE_MKLONGLONG( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
#else
      fprintf( cargo->yyc, "\t/* %" PFLL "i */", HB_PCODE_MKLONGLONG( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
#endif
   }

   fprintf( cargo->yyc, "\n" );

   return 9;
}

static HB_GENC_FUNC( hb_p_pushdatetime )
{
   int i;

   fprintf( cargo->yyc, "\tHB_P_PUSHDATETIME, " );

   for( i = 0; i < 8; ++i )
      fprintf( cargo->yyc, " %i,", ( ( BYTE * ) pFunc->pCode )[ lPCodePos + i + 1 ] );

   if( cargo->bVerbose )
   {
      char szDateTime[ 24 ];

      fprintf( cargo->yyc, "\t/* %s */",
               hb_datetimeDecStr( szDateTime, HB_PCODE_MKLONG( ( pFunc->pCode + lPCodePos + 1 ) ), HB_PCODE_MKLONG( ( pFunc->pCode + lPCodePos + 5 ) ) ) );

/*      printf("szDateTime=%s lDate=%d lTime=%d\n", szDateTime, HB_PCODE_MKLONG( ( pFunc->pCode + lPCodePos + 1 ) ), HB_PCODE_MKLONG( ( pFunc->pCode + lPCodePos + 5 ) ) );
 */
   }

   fprintf( cargo->yyc, "\t/* HB_ET_DDATETIME */\n" );

   return 9;
}

static HB_GENC_FUNC( hb_p_pushdate )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHDATE, " );

   fprintf( cargo->yyc, "%i, %i, %i, %i, ",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ],
            pFunc->pCode[ lPCodePos + 4 ] );

   fprintf( cargo->yyc, "\t/* HB_ET_DDATE */\n" );

   return 5;
}

static HB_GENC_FUNC( hb_p_pushmemvar )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHMEMVAR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_pushmemvarref )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHMEMVARREF, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_pushnil )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHNIL,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_pushself )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHSELF,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_pushstatic )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSTATIC, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      PVAR      pVar;
      PFUNCTION pTmp = hb_comp_functions.pFirst;
      USHORT    wVar = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
         pTmp = pTmp->pNext;

      pVar = hb_compVariableFind( pTmp->pStatics, ( USHORT ) ( wVar - pTmp->iStaticsBase ) );
      fprintf( cargo->yyc, "\t/* %s */", pVar->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_pushstaticref )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSTATICREF, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      PVAR      pVar;
      PFUNCTION pTmp = hb_comp_functions.pFirst;
      USHORT    wVar = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
         pTmp = pTmp->pNext;

      pVar = hb_compVariableFind( pTmp->pStatics, ( USHORT) ( wVar - pTmp->iStaticsBase ) );
      fprintf( cargo->yyc, "\t/* %s */", pVar->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_pushstr )
{
   HB_SIZE ulStart = lPCodePos;
   USHORT  wLen    = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

   fprintf( cargo->yyc, "\tHB_P_PUSHSTR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", wLen );

   lPCodePos += 3;
   if( wLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );
      while( wLen-- )
      {
         BYTE uchr = ( BYTE ) pFunc->pCode[ lPCodePos++ ];
         /*
          * NOTE: After optimization some CHR(n) can be converted
          *    into a string containing nonprintable characters.
          *
          * TODO: add switch to use hexadecimal format "%#04x"
          */
         if( ( uchr < ( BYTE ) ' ' ) || ( uchr >= 127 ) )
            fprintf( cargo->yyc, "%i, ", uchr );
         else if( strchr( "\'\\\"", uchr ) )
            fprintf( cargo->yyc, "%i, ", uchr );
         else
            fprintf( cargo->yyc, "\'%c\', ", uchr );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return lPCodePos - ulStart;
}

static HB_GENC_FUNC( hb_p_pushstrshort )
{
   HB_SIZE ulStart = lPCodePos;
   USHORT  wLen    = pFunc->pCode[ lPCodePos + 1 ];

   fprintf( cargo->yyc, "\tHB_P_PUSHSTRSHORT, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", wLen );

   lPCodePos += 2;
   if( wLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );
      while( wLen-- )
      {
         BYTE uchr = ( BYTE ) pFunc->pCode[ lPCodePos++ ];
         /*
          * NOTE: After optimization some CHR(n) can be converted
          *    into a string containing nonprintable characters.
          *
          * TODO: add switch to use hexadecimal format "%#04x"
          */
         if( ( uchr < ( BYTE ) ' ' ) || ( uchr >= 127 ) )
            fprintf( cargo->yyc, "%i, ", uchr );
         else if( strchr( "\'\\\"", uchr ) )
            fprintf( cargo->yyc, "%i, ", uchr );
         else
            fprintf( cargo->yyc, "\'%c\', ", uchr );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return lPCodePos - ulStart;
}

static HB_GENC_FUNC( hb_p_pushsym )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSYM, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_pushsymnear )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSYMNEAR, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] )->szName );

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_pushvariable )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHVARIABLE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) )->szName );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_retvalue )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_RETVALUE,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_send )
{
   fprintf( cargo->yyc, "\tHB_P_SEND, %i, %i,\n",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   return 3;
}

static HB_GENC_FUNC( hb_p_sendshort )
{
   fprintf( cargo->yyc, "\tHB_P_SENDSHORT, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );

   return 2;
}

static HB_GENC_FUNC( hb_p_seqbegin )
{
   fprintf( cargo->yyc, "\tHB_P_SEQBEGIN, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %li (abs: %08li) */", lOffset, ( long int ) ( lPCodePos + lOffset ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 4;
}

static HB_GENC_FUNC( hb_p_seqend )
{
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "/* %05li */ ", ( long int ) lPCodePos );
   else
      fprintf( cargo->yyc, "\t" );

   fprintf( cargo->yyc, "HB_P_SEQEND, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      fprintf( cargo->yyc, "\t/* %li (abs: %08li) */", lOffset, ( long int ) ( lPCodePos + lOffset ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 4;
}

static HB_GENC_FUNC( hb_p_seqrecover )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_SEQRECOVER,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_sframe )
{
   fprintf( cargo->yyc, "\tHB_P_SFRAME, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* symbol (_INITSTATICS) */" );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_statics )
{
   LONG lByteCount = 5;

   fprintf( cargo->yyc, "\tHB_P_STATICS, %i, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ],
            pFunc->pCode[ lPCodePos + 4 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* symbol (_INITSTATICS), %i statics */", HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 3 ] ) ) );

   fprintf( cargo->yyc, "\n" );

   return lByteCount;
}

static HB_GENC_FUNC( hb_p_staticname )
{
   HB_SIZE ulStart = lPCodePos;

   fprintf( cargo->yyc, "\tHB_P_STATICNAME, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", ( char * ) pFunc->pCode + lPCodePos + 4 );

   fprintf( cargo->yyc, "\n" );

   lPCodePos += 4;
   while( pFunc->pCode[ lPCodePos ] )
   {
      char chr = pFunc->pCode[ lPCodePos++ ];

      if( chr == '\'' || chr == '\\' )
         fprintf( cargo->yyc, " \'\\%c\',", chr );
      else
         fprintf( cargo->yyc, " \'%c\',", chr );
   }
   fprintf( cargo->yyc, " 0,\n" );

   return lPCodePos - ulStart + 1;
}

static HB_GENC_FUNC( hb_p_swapalias )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_SWAPALIAS,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_true )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_TRUE,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_one )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ONE,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_zero )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ZERO,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_noop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_NOOP,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_dummy )
{
   HB_SYMBOL_UNUSED( cargo );
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   return 1;
}

static HB_GENC_FUNC( hb_p_macrolist )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROLIST,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_macrolistend )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROLISTEND,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_localnearaddint )
{
   fprintf( cargo->yyc, "\tHB_P_LOCALNEARADDINT, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      int iVar = ( int ) ( signed char ) pFunc->pCode[ lPCodePos + 1 ];

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         else
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
      }
      else
         fprintf( cargo->yyc, "\t/* %s %i*/", hb_compLocalVariableFind( pFunc, iVar )->szName, HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 2 ] ) ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 4;
}

static HB_GENC_FUNC( hb_p_localnearsetint )
{
   fprintf( cargo->yyc, "\tHB_P_LOCALNEARSETINT, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      int iVar = ( int ) ( signed char ) pFunc->pCode[ lPCodePos + 1 ];

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         else
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
      }
      else
         fprintf( cargo->yyc, "\t/* %s %i*/", hb_compLocalVariableFind( pFunc, iVar )->szName, HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 2 ] ) ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 4;
}

static HB_GENC_FUNC( hb_p_addint )
{
   fprintf( cargo->yyc, "\tHB_P_ADDINT, %i, %i,", pFunc->pCode[ lPCodePos + 1 ], pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i*/", HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_localnearsetstr )
{

   HB_SIZE  ulStart = lPCodePos;
   USHORT   uLen    = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 2 ] ) );

   fprintf( cargo->yyc, "\tHB_P_LOCALNEARSETSTR, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      int iVar = ( int ) ( signed char ) pFunc->pCode[ lPCodePos + 1 ];

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         else
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
      }
      else
         fprintf( cargo->yyc, "\t/* %s %i*/", hb_compLocalVariableFind( pFunc, iVar )->szName, uLen );
   }

   lPCodePos += 4;

   if( uLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );

      while( uLen-- )
      {
         BYTE uchr = ( BYTE ) pFunc->pCode[ lPCodePos++ ];
         /*
          * NOTE: After optimization some CHR(n) can be converted
          *    into a string containing nonprintable characters.
          *
          * TODO: add switch to use hexadecimal format "%#04x"
          */
         if( ( uchr < ( BYTE ) ' ' ) || ( uchr >= 127 ) )
            fprintf( cargo->yyc, "%i, ", uchr );
         else if( strchr( "\'\\\"", uchr ) )
            fprintf( cargo->yyc, "%i, ", uchr );
         else
            fprintf( cargo->yyc, "\'%c\', ", uchr );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return lPCodePos - ulStart;
}

static HB_GENC_FUNC( hb_p_left )
{
   fprintf( cargo->yyc, "\tHB_P_LEFT, %i, %i,", pFunc->pCode[ lPCodePos + 1 ], pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_right )
{
   fprintf( cargo->yyc, "\tHB_P_RIGHT, %i, %i,", pFunc->pCode[ lPCodePos + 1 ], pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_substr )
{
   fprintf( cargo->yyc, "\tHB_P_SUBSTR, %i, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ], pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ], pFunc->pCode[ lPCodePos + 4 ] );

   if( cargo->bVerbose )
   {
      fprintf( cargo->yyc, "\t/* %i %i*/",
               HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ),
               HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 3 ] ) ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 5;
}

static HB_GENC_FUNC( hb_p_lineoffset )
{
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "/* %05li */ ", ( long int ) lPCodePos );
   else
      fprintf( cargo->yyc, "\t" );

   fprintf( cargo->yyc, "HB_P_LINEOFFSET, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", hb_comp_iBaseLine + pFunc->pCode[ lPCodePos + 1 ] );

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_baseline )
{
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "/* %05li */ ", ( long int ) lPCodePos );
   else
      fprintf( cargo->yyc, "\t" );

   fprintf( cargo->yyc, "HB_P_BASELINE, %i, %i,", pFunc->pCode[ lPCodePos + 1 ], pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
   {
      hb_comp_iBaseLine = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %i */", hb_comp_iBaseLine );
   }

   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_withobject )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_WITHOBJECT,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_sendwith )
{
   fprintf( cargo->yyc, "\tHB_P_SENDWITH, %i, %i,\n",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   return 3;
}

static HB_GENC_FUNC( hb_p_sendwithshort )
{
   fprintf( cargo->yyc, "\tHB_P_SENDWITHSHORT, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );

   return 2;
}

static HB_GENC_FUNC( hb_p_endwithobject )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ENDWITHOBJECT,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_foreach )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FOREACH,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_enumerate )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ENUMERATE,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_endenumerate )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ENDENUMERATE,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_enumindex )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ENUMINDEX,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_pushglobal )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHGLOBAL, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compVariableFind( hb_comp_pGlobals, ( USHORT ) pFunc->pCode[ lPCodePos + 1 ] + 1 )->szName );

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_popglobal )
{
   fprintf( cargo->yyc, "\tHB_P_POPGLOBAL, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compVariableFind( hb_comp_pGlobals, ( USHORT ) pFunc->pCode[ lPCodePos + 1 ] + 1 )->szName );

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_pushglobalref )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHGLOBALREF, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compVariableFind( hb_comp_pGlobals, ( USHORT ) pFunc->pCode[ lPCodePos + 1 ] + 1 )->szName );

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_switchcase )
{
   fprintf( cargo->yyc, "\tHB_P_SWITCHCASE, %i, %i, %i, %i, ",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ],
            pFunc->pCode[ lPCodePos + 4 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %li */", HB_PCODE_MKLONG( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );

   fprintf( cargo->yyc, "\n" );

   return 5;
}

static HB_GENC_FUNC( hb_p_like )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_LIKE,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_match )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MATCH,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_pushmacroref )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHMACROREF,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_ivarref )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_IVARREF,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_classsetmodule )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_CLASSSETMODULE,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_bitand )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_BITAND,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_bitor )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_BITOR,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_bitxor )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_BITXOR,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_bitshiftr )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_BITSHIFTR,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_bitshiftl )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_BITSHIFTL,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_largeframe )
{
   fprintf( cargo->yyc, "\tHB_P_LARGEFRAME, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* (lo)locals, (hi)locals, params */" );

   fprintf( cargo->yyc, "\n" );

   return 4;
}

static HB_GENC_FUNC( hb_p_pushwith )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHWITH,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_pushstrhidden )
{
   HB_SIZE  ulStart    = lPCodePos;
   USHORT   wLen       = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );
   BYTE     bType      = pFunc->pCode[ lPCodePos + 3 ];
   USHORT   wLenBuffer = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 4 ] ) );

   fprintf( cargo->yyc, "\tHB_P_PUSHSTRHIDDEN, %i, %i, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],      // LO: String length
            pFunc->pCode[ lPCodePos + 2 ],      // HI: String length
            pFunc->pCode[ lPCodePos + 3 ],      // Hide type
            pFunc->pCode[ lPCodePos + 4 ],      // LO: Buffer length
            pFunc->pCode[ lPCodePos + 5 ] );    // HI: Buffer length

   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i, %i, %i */", wLen, bType, wLenBuffer );

   lPCodePos += 6;
   if( wLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );
      while( wLenBuffer-- )
      {
         BYTE uchr = ( BYTE ) pFunc->pCode[ lPCodePos++ ];
         /*
          * NOTE: After optimization some CHR(n) can be converted
          *    into a string containing nonprintable characters.
          *
          * TODO: add switch to use hexadecimal format "%#04x"
          */
         if( ( uchr < ( BYTE ) ' ' ) || ( uchr >= 127 ) )
            fprintf( cargo->yyc, "%i, ", uchr );
         else if( strchr( "\'\\\"", uchr ) )
            fprintf( cargo->yyc, "%i, ", uchr );
         else
            fprintf( cargo->yyc, "\'%c\', ", uchr );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return lPCodePos - ulStart;
}

static HB_GENC_FUNC( hb_p_localnearsetstrhidden )
{
   HB_SIZE  ulStart     = lPCodePos;
   USHORT   uLen        = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 2 ] ) );
   BYTE     bType       = pFunc->pCode[ lPCodePos + 4 ];
   USHORT   wLenBuffer  = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 5 ] ) );

   fprintf( cargo->yyc, "\tHB_P_LOCALNEARSETSTRHIDDEN, %i, %i, %i, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],      // LO: String length
            pFunc->pCode[ lPCodePos + 3 ],      // HI: String length
            pFunc->pCode[ lPCodePos + 4 ],      // Hide type
            pFunc->pCode[ lPCodePos + 5 ],      // LO: Buffer length
            pFunc->pCode[ lPCodePos + 6 ] );    // HI: Buffer length

   if( cargo->bVerbose )
   {
      int iVar = ( int ) ( signed char ) pFunc->pCode[ lPCodePos + 1 ];

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
            fprintf( cargo->yyc, "\t/* localvar%i, %i, %i */", -iVar, bType, wLenBuffer );
         else
            fprintf( cargo->yyc, "\t/* codeblockvar%i, %i, %i */", iVar, bType, wLenBuffer );
      }
      else
         fprintf( cargo->yyc, "\t/* %s %i, %i, %i*/", hb_compLocalVariableFind( pFunc, iVar )->szName, uLen, bType, wLenBuffer );
   }

   lPCodePos += 7;

   if( uLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );

      while( wLenBuffer-- )
      {
         BYTE uchr = ( BYTE ) pFunc->pCode[ lPCodePos++ ];

         /*
          * NOTE: After optimization some CHR(n) can be converted
          *    into a string containing nonprintable characters.
          *
          * TODO: add switch to use hexadecimal format "%#04x"
          */
         if( ( uchr < ( BYTE ) ' ' ) || ( uchr >= 127 ) )
            fprintf( cargo->yyc, "%i, ", uchr );
         else if( strchr( "\'\\\"", uchr ) )
            fprintf( cargo->yyc, "%i, ", uchr );
         else
            fprintf( cargo->yyc, "\'%c\', ", uchr );
      }
   }

   fprintf( cargo->yyc, "\n" );

   return lPCodePos - ulStart;
}

static HB_GENC_FUNC( hb_p_trybegin )
{
   fprintf( cargo->yyc, "\tHB_P_TRYBEGIN, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );

      fprintf( cargo->yyc, "\t/* %li (abs: %08li) */", lOffset, ( long int ) ( lPCodePos + lOffset ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 4;
}

static HB_GENC_FUNC( hb_p_tryend )
{
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "/* %05li */ ", ( long int ) lPCodePos );
   else
      fprintf( cargo->yyc, "\t" );

   fprintf( cargo->yyc, "HB_P_TRYEND, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %li (abs: %08li) */", lOffset, ( long int ) ( lPCodePos + lOffset ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 4;
}

static HB_GENC_FUNC( hb_p_tryrecover )
{
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "/* %05li */ ", ( long int ) lPCodePos );
   else
      fprintf( cargo->yyc, "\t" );

   fprintf( cargo->yyc, "HB_P_TRYRECOVER, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );

   if( cargo->bVerbose )
   {
      LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );
      fprintf( cargo->yyc, "\t/* %li (abs: %08li) */", lOffset, ( long int ) ( lPCodePos + lOffset ) );
   }

   fprintf( cargo->yyc, "\n" );

   return 4;
}

static HB_GENC_FUNC( hb_p_finally )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FINALLY,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_endfinally )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ENDFINALLY,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_localnearadd )
{
   fprintf( cargo->yyc, "\tHB_P_LOCALNEARADD, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
   {
      int iVar = ( int ) ( signed char ) pFunc->pCode[ lPCodePos + 1 ];

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( iVar < 0 )
            fprintf( cargo->yyc, "\t/* localvar%i */", -iVar );
         else
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", iVar );
      }
      else
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, iVar )->szName );
   }

   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_arraypushref )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ARRAYPUSHREF,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_arraypopplus )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ARRAYPOPPLUS,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_divert )
{
   if( ( lPCodePos + 1 ) == pFunc->lPCodePos )
      fprintf( cargo->yyc, "\tHB_P_DIVERT\n" );
   else
      fprintf( cargo->yyc, "\tHB_P_DIVERT,\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_divertof )
{
   if( ( lPCodePos + 1 ) == pFunc->lPCodePos )
      fprintf( cargo->yyc, "\tHB_P_DIVERTOF\n" );
   else
      fprintf( cargo->yyc, "\tHB_P_DIVERTOF,\n" );

   return 1;
}

/* NOTE: The order of functions has to match the order of opcodes mnemonics
 */
static PHB_GENC_FUNC s_verbose_table[] = {
   hb_p_and,                                          /* HB_P_AND,                  */
   hb_p_arraypush,                                    /* HB_P_ARRAYPUSH,            */
   hb_p_arraypop,                                     /* HB_P_ARRAYPOP,             */
   hb_p_arraydim,                                     /* HB_P_ARRAYDIM,             */
   hb_p_arraygen,                                     /* HB_P_ARRAYGEN,             */
   hb_p_equal,                                        /* HB_P_EQUAL,                */
   hb_p_endblock,                                     /* HB_P_ENDBLOCK,             */
   hb_p_endproc,                                      /* HB_P_ENDPROC,              */
   hb_p_exactlyequal,                                 /* HB_P_EXACTLYEQUAL,         */
   hb_p_false,                                        /* HB_P_FALSE,                */
   hb_p_fortest,                                      /* HB_P_FORTEST,              */
   hb_p_function,                                     /* HB_P_FUNCTION,             */
   hb_p_functionshort,                                /* HB_P_FUNCTIONSHORT,        */
   hb_p_frame,                                        /* HB_P_FRAME,                */
   hb_p_funcptr,                                      /* HB_P_FUNCPTR,              */
   hb_p_greater,                                      /* HB_P_GREATER,              */
   hb_p_greaterequal,                                 /* HB_P_GREATEREQUAL,         */
   hb_p_dec,                                          /* HB_P_DEC,                  */
   hb_p_divide,                                       /* HB_P_DIVIDE,               */
   hb_p_do,                                           /* HB_P_DO,                   */
   hb_p_doshort,                                      /* HB_P_DOSHORT,              */
   hb_p_duplicate,                                    /* HB_P_DUPLICATE,            */
   hb_p_dupltwo,                                      /* HB_P_DUPLTWO,              */
   hb_p_inc,                                          /* HB_P_INC,                  */
   hb_p_instring,                                     /* HB_P_INSTRING,             */
   hb_p_jumpnear,                                     /* HB_P_JUMPNEAR,             */
   hb_p_jump,                                         /* HB_P_JUMP,                 */
   hb_p_jumpfar,                                      /* HB_P_JUMPFAR,              */
   hb_p_jumpfalsenear,                                /* HB_P_JUMPFALSENEAR,        */
   hb_p_jumpfalse,                                    /* HB_P_JUMPFALSE,            */
   hb_p_jumpfalsefar,                                 /* HB_P_JUMPFALSEFAR,         */
   hb_p_jumptruenear,                                 /* HB_P_JUMPTRUENEAR,         */
   hb_p_jumptrue,                                     /* HB_P_JUMPTRUE,             */
   hb_p_jumptruefar,                                  /* HB_P_JUMPTRUEFAR,          */
   hb_p_lessequal,                                    /* HB_P_LESSEQUAL,            */
   hb_p_less,                                         /* HB_P_LESS,                 */
   hb_p_line,                                         /* HB_P_LINE,                 */
   hb_p_localname,                                    /* HB_P_LOCALNAME,            */
   hb_p_macropop,                                     /* HB_P_MACROPOP,             */
   hb_p_macropopaliased,                              /* HB_P_MACROPOPALIASED,      */
   hb_p_macropush,                                    /* HB_P_MACROPUSH,            */
   hb_p_macropusharg,                                 /* HB_P_MACROPUSHARG,         */
   hb_p_macropushlist,                                /* HB_P_MACROPUSHLIST,        */
   hb_p_macropushindex,                               /* HB_P_MACROPUSHINDEX,       */
   hb_p_macropushpare,                                /* HB_P_MACROPUSHPARE,        */
   hb_p_macropushaliased,                             /* HB_P_MACROPUSHALIASED,     */
   hb_p_macrosymbol,                                  /* HB_P_MACROSYMBOL,          */
   hb_p_macrotext,                                    /* HB_P_MACROTEXT,            */
   hb_p_message,                                      /* HB_P_MESSAGE,              */
   hb_p_minus,                                        /* HB_P_MINUS,                */
   hb_p_modulus,                                      /* HB_P_MODULUS,              */
   hb_p_modulename,                                   /* HB_P_MODULENAME,           */
   /* start: pcodes generated by macro compiler */
   hb_p_dummy,                                        /* HB_P_MMESSAGE,             */
   hb_p_dummy,                                        /* HB_P_MPOPALIASEDFIELD,     */
   hb_p_dummy,                                        /* HB_P_MPOPALIASEDVAR,       */
   hb_p_dummy,                                        /* HB_P_MPOPFIELD,            */
   hb_p_dummy,                                        /* HB_P_MPOPMEMVAR,           */
   hb_p_dummy,                                        /* HB_P_MPUSHALIASEDFIELD,    */
   hb_p_dummy,                                        /* HB_P_MPUSHALIASEDVAR,      */
   hb_p_dummy,                                        /* HB_P_MPUSHBLOCK,           */
   hb_p_dummy,                                        /* HB_P_MPUSHFIELD,           */
   hb_p_dummy,                                        /* HB_P_MPUSHMEMVAR,          */
   hb_p_dummy,                                        /* HB_P_MPUSHMEMVARREF,       */
   hb_p_dummy,                                        /* HB_P_MPUSHSYM,             */
   hb_p_dummy,                                        /* HB_P_MPUSHVARIABLE,        */
   /* end: */
   hb_p_mult,                                         /* HB_P_MULT,                 */
   hb_p_negate,                                       /* HB_P_NEGATE,               */
   hb_p_noop,                                         /* HB_P_NOOP,                 */
   hb_p_not,                                          /* HB_P_NOT,                  */
   hb_p_notequal,                                     /* HB_P_NOTEQUAL,             */
   hb_p_or,                                           /* HB_P_OR,                   */
   hb_p_parameter,                                    /* HB_P_PARAMETER,            */
   hb_p_plus,                                         /* HB_P_PLUS,                 */
   hb_p_pop,                                          /* HB_P_POP,                  */
   hb_p_popalias,                                     /* HB_P_POPALIAS,             */
   hb_p_popaliasedfield,                              /* HB_P_POPALIASEDFIELD,      */
   hb_p_popaliasedfieldnear,                          /* HB_P_POPALIASEDFIELDNEAR,  */
   hb_p_popaliasedvar,                                /* HB_P_POPALIASEDVAR,        */
   hb_p_popfield,                                     /* HB_P_POPFIELD,             */
   hb_p_poplocal,                                     /* HB_P_POPLOCAL,             */
   hb_p_poplocalnear,                                 /* HB_P_POPLOCALNEAR,         */
   hb_p_popmemvar,                                    /* HB_P_POPMEMVAR,            */
   hb_p_popstatic,                                    /* HB_P_POPSTATIC,            */
   hb_p_popvariable,                                  /* HB_P_POPVARIABLE,          */
   hb_p_power,                                        /* HB_P_POWER,                */
   hb_p_pushalias,                                    /* HB_P_PUSHALIAS,            */
   hb_p_pushaliasedfield,                             /* HB_P_PUSHALIASEDFIELD,     */
   hb_p_pushaliasedfieldnear,                         /* HB_P_PUSHALIASEDFIELDNEAR, */
   hb_p_pushaliasedvar,                               /* HB_P_PUSHALIASEDVAR,       */
   hb_p_pushblock,                                    /* HB_P_PUSHBLOCK,            */
   hb_p_pushblockshort,                               /* HB_P_PUSHBLOCKSHORT,       */
   hb_p_pushfield,                                    /* HB_P_PUSHFIELD,            */
   hb_p_pushbyte,                                     /* HB_P_PUSHBYTE,             */
   hb_p_pushint,                                      /* HB_P_PUSHINT,              */
   hb_p_pushlocal,                                    /* HB_P_PUSHLOCAL,            */
   hb_p_pushlocalnear,                                /* HB_P_PUSHLOCALNEAR,        */
   hb_p_pushlocalref,                                 /* HB_P_PUSHLOCALREF,         */
   hb_p_pushlong,                                     /* HB_P_PUSHLONG,             */
   hb_p_pushmemvar,                                   /* HB_P_PUSHMEMVAR,           */
   hb_p_pushmemvarref,                                /* HB_P_PUSHMEMVARREF,        */
   hb_p_pushnil,                                      /* HB_P_PUSHNIL,              */
   hb_p_pushdouble,                                   /* HB_P_PUSHDOUBLE,           */
   hb_p_pushself,                                     /* HB_P_PUSHSELF,             */
   hb_p_pushstatic,                                   /* HB_P_PUSHSTATIC,           */
   hb_p_pushstaticref,                                /* HB_P_PUSHSTATICREF,        */
   hb_p_pushstr,                                      /* HB_P_PUSHSTR,              */
   hb_p_pushstrshort,                                 /* HB_P_PUSHSTRSHORT,         */
   hb_p_pushsym,                                      /* HB_P_PUSHSYM,              */
   hb_p_pushsymnear,                                  /* HB_P_PUSHSYMNEAR,          */
   hb_p_pushvariable,                                 /* HB_P_PUSHVARIABLE,         */
   hb_p_retvalue,                                     /* HB_P_RETVALUE,             */
   hb_p_send,                                         /* HB_P_SEND,                 */
   hb_p_sendshort,                                    /* HB_P_SENDSHORT,            */
   hb_p_seqbegin,                                     /* HB_P_SEQBEGIN,             */
   hb_p_seqend,                                       /* HB_P_SEQEND,               */
   hb_p_seqrecover,                                   /* HB_P_SEQRECOVER,           */
   hb_p_sframe,                                       /* HB_P_SFRAME,               */
   hb_p_statics,                                      /* HB_P_STATICS,              */
   hb_p_staticname,                                   /* HB_P_STATICNAME,           */
   hb_p_swapalias,                                    /* HB_P_SWAPALIAS,            */
   hb_p_true,                                         /* HB_P_TRUE,                 */
   hb_p_zero,                                         /* HB_P_ZERO,                 */
   hb_p_one,                                          /* HB_P_ONE,                  */
   hb_p_macrolist,                                    /* HB_P_MACROLIST,            */
   hb_p_macrolistend,                                 /* HB_P_MACROLISTEND,         */
   hb_p_localnearaddint,                              /* HB_P_LOCALNEARADDINT,      */
   hb_p_localnearsetint,                              /* HB_P_LOCALNEARSETINT,      */
   hb_p_localnearsetstr,                              /* HB_P_LOCALNEARSETSTR,      */
   hb_p_addint,                                       /* HB_P_ADDINT,               */
   hb_p_left,                                         /* HB_P_LEFT,                 */
   hb_p_right,                                        /* HB_P_RIGHT,                */
   hb_p_substr,                                       /* HB_P_SUBSTR,               */
   hb_p_dummy,                                        /* HB_P_MPUSHSTR,             */
   hb_p_baseline,                                     /* HB_P_BASELINE,             */
   hb_p_lineoffset,                                   /* HB_P_LINEOFFSET,           */
   hb_p_withobject,                                   /* HB_P_WITHOBJECT,           */
   hb_p_sendwith,                                     /* HB_P_SENDWITH,             */
   hb_p_sendwithshort,                                /* HB_P_SENDWITHSHORT,        */
   hb_p_endwithobject,                                /* HB_P_ENDWITHOBJECT,        */
   hb_p_foreach,                                      /* HB_P_FOREACH,              */
   hb_p_enumerate,                                    /* HB_P_ENUMERATE,            */
   hb_p_endenumerate,                                 /* HB_P_ENDENUMERATE,         */
   hb_p_pushglobal,                                   /* HB_P_PUSHGLOBAL,           */
   hb_p_popglobal,                                    /* HB_P_POPGLOBAL,            */
   hb_p_pushglobalref,                                /* HB_P_PUSHGLOBALREF,        */
   hb_p_enumindex,                                    /* HB_P_ENUMINDEX,            */
   hb_p_switchcase,                                   /* HB_P_SWITCHCASE,           */
   hb_p_like,                                         /* HB_P_LIKE,                 */
   hb_p_match,                                        /* HB_P_MATCH,                */
   hb_p_pushmacroref,                                 /* HB_P_PUSHMACROREF,         */
   hb_p_ivarref,                                      /* HB_P_IVARREF,              */
   hb_p_classsetmodule,                               /* HB_P_CLASSSETMODULE,       */
   hb_p_bitand,                                       /* HB_P_BITAND,               */
   hb_p_bitor,                                        /* HB_P_BITOR,                */
   hb_p_bitxor,                                       /* HB_P_BITXOR,               */
   hb_p_bitshiftr,                                    /* HB_P_SHIFTR,               */
   hb_p_bitshiftl,                                    /* HB_P_SHIFTL,               */
   hb_p_largeframe,                                   /* HB_P_LARGEFRAME,           */
   hb_p_pushwith,                                     /* HB_P_PUSHWITH,             */
   hb_p_pushlonglong,                                 /* HB_P_PUSHLONGLONG,         */
   hb_p_pushstrhidden,                                /* HB_P_PUSHSTRHIDDEN,        */
   hb_p_localnearsetstrhidden,                        /* HB_P_LOCALNEARSETSTRHIDDEN,*/
   hb_p_trybegin,                                     /* HB_P_TRYBEGIN,             */
   hb_p_tryend,                                       /* HB_P_TRYEND,               */
   hb_p_tryrecover,                                   /* HB_P_TRYRECOVER,           */
   hb_p_finally,                                      /* HB_P_FINALLY,              */
   hb_p_endfinally,                                   /* HB_P_ENDFINALLY,           */
   hb_p_localnearadd,                                 /* HB_P_LOCALNEARADD          */
   hb_p_arraypushref,                                 /* HB_P_ARRAYPUSHREF          */
   hb_p_arraypopplus,                                 /* HB_P_ARRAYPOPPLUS          */
   hb_p_pushdatetime,                                 /* HB_P_PUSHDATETIME          */
   hb_p_pushdate,                                     /* HB_P_PUSHDATE              */
   hb_p_hashgen,                                      /* HB_P_HASHGEN               */
   hb_p_localnearinc,                                 /* HB_P_LOCALNEARINC,         */
   hb_p_localneardec,                                 /* HB_P_LOCALNEARDEC,         */
   hb_p_pushlocalnearinc,                             /* HB_P_PUSHLOCALNEARINC,     */
   hb_p_pushlocalneardec,                             /* HB_P_PUSHLOCALNEARDEC,     */
   hb_p_divert,                                       /* HB_P_DIVERT                */
   hb_p_divertof                                      /* HB_P_DIVERTOF              */
};

void hb_compGenCReadable( PFUNCTION pFunc, FILE * yyc )
{
   HB_GENC_INFO genc_info;

   /* Make sure that table is correct */
   assert( HB_P_LAST_PCODE == sizeof( s_verbose_table ) / sizeof( PHB_GENC_FUNC ) );

   genc_info.iNestedCodeblock = 0;
   genc_info.bVerbose         = ( hb_comp_iGenCOutput == HB_COMPGENC_VERBOSE );
   genc_info.yyc              = yyc;

   fprintf( yyc, "{\n   static const BYTE pcode[] =\n   {\n" );

   hb_compPCodeEval( pFunc, ( PHB_PCODE_FUNC * ) s_verbose_table, ( void * ) &genc_info );
   if( genc_info.bVerbose )
      fprintf( yyc, "/* %05li */\n", ( long int ) pFunc->lPCodePos );

   fprintf( yyc, "   };\n\n" );

   if( pFunc->cScope & HB_FS_CRITICAL )
      fprintf( yyc, "   HB_CRITICAL_LOCK( s_Critical%s );\n", pFunc->szName );

   fprintf( yyc, "   hb_vmExecute( pcode, symbols );\n" );

   if( pFunc->cScope & HB_FS_CRITICAL )
      fprintf( yyc, "   HB_CRITICAL_UNLOCK( s_Critical%s );\n", pFunc->szName );

   fprintf( yyc, "}\n" );
}

void hb_compGenCCompact( PFUNCTION pFunc, FILE * yyc )
{
   ULONG lPCodePos = 0;
   int   nChar;

   fprintf( yyc, "{\n   static const BYTE pcode[] =\n   {\n\t" );

   nChar = 0;

   while( lPCodePos < pFunc->lPCodePos )
   {
      ++nChar;

      if( nChar > 1 )
         fprintf( yyc, "," );

      if( nChar == 15 )
      {
         fprintf( yyc, "\n\t" );
         nChar = 1;
      }

      /* Displaying as decimal is more compact than hex */
      fprintf( yyc, "%d", ( int ) pFunc->pCode[ lPCodePos++ ] );

   }

   if( nChar != 0 )
      fprintf( yyc, "\n" );

   fprintf( yyc, "   };\n\n" );

   if( pFunc->cScope & HB_FS_CRITICAL )
      fprintf( yyc, "   HB_CRITICAL_LOCK( s_Critical%s );\n", pFunc->szName );

   fprintf( yyc, "   hb_vmExecute( pcode, symbols );\n" );

   if( pFunc->cScope & HB_FS_CRITICAL )
      fprintf( yyc, "   HB_CRITICAL_UNLOCK( s_Critical%s );\n", pFunc->szName );

   fprintf( yyc, "}\n" );
}
