/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler C source with real code generation
 *
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
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
#include "hbcomp.h"

extern void hb_compGenCRealCode( PFUNCTION pFunc, FILE * yyc );

#define HB_GENC_FUNC( func )  HB_PCODE_FUNC( func, PHB_LABEL_INFO )

typedef HB_GENC_FUNC ( HB_GENC_FUNC_ );
typedef HB_GENC_FUNC_ * PHB_GENC_FUNC;

#define HB_GENC_GETLABEL( l ) ( l < pFunc->lPCodePos ? cargo->pulLabels[ l ] : 0 )

#define HB_GENC_LABEL()       do { \
      ULONG ulLab = HB_GENC_GETLABEL( lPCodePos ); \
      if( ulLab != 0 ) \
         fprintf( cargo->yyc, "lab%05ld: ;\n", ulLab ); \
} while( 0 )

#define HB_GENC_ERROR( s )    do { \
      fprintf( cargo->yyc, "\t#error: \"" s "\"\n" ); \
} while( 0 )

static void hb_gencc_string_put( FILE * yyc, BYTE * pText, HB_SIZE usLen )
{
   USHORT nPos;

   fputc( '"', yyc );
   for( nPos = 0; nPos < ( USHORT ) usLen; nPos++ )
   {
      BYTE uchr = ( BYTE ) pText[ nPos ];
      /*
       * NOTE: After optimization some CHR(n) can be converted
       *       into a string containing nonprintable characters.
       *
       * ? is escaped to avoid conflicts with trigraph sequences which
       * are part of ANSI C standard
       */
      if( uchr == '"' || uchr == '\\' || uchr == '?' )
         fprintf( yyc, "\\%c", uchr );
      else if( uchr < ( BYTE ) ' ' || uchr >= 127 )
      {
         BYTE uchrnext = nPos < ( USHORT ) usLen - 1 ? pText[ nPos + 1 ] : 0;

         fprintf( yyc, "\\x%02X%s", uchr,
                  ( uchrnext >= ( BYTE ) '0' && uchrnext <= ( BYTE ) '9' ) ||
                  ( uchrnext >= ( BYTE ) 'a' && uchrnext <= ( BYTE ) 'z' ) ||
                  ( uchrnext >= ( BYTE ) 'A' && uchrnext <= ( BYTE ) 'Z' ) ? "\" \"" : "" );
      }
      else
         fprintf( yyc, "%c", uchr );
   }
   fputc( '"', yyc );
}

static int hb_gencc_checkNumAhead( LONG lValue, PFUNCTION pFunc, HB_SIZE lPCodePos, PHB_LABEL_INFO cargo )
{
   if( HB_GENC_GETLABEL( lPCodePos ) == 0 )
   {
      switch( pFunc->pCode[ lPCodePos ] )
      {
         case HB_P_ARRAYPUSH:
            if( lValue > 0 )
            {
               fprintf( cargo->yyc, "\tif( hb_xvmArrayItemPush( %ld ) ) break;\n", lValue );
               return 1;
            }
            break;

         case HB_P_ARRAYPOP:
            if( lValue > 0 )
            {
               fprintf( cargo->yyc, "\tif( hb_xvmArrayItemPop( %ld ) ) break;\n", lValue );
               return 1;
            }
            break;

         case HB_P_MULT:
            fprintf( cargo->yyc, "\tif( hb_xvmMultByInt( %ld ) ) break;\n", lValue );
            return 1;

         case HB_P_DIVIDE:
            fprintf( cargo->yyc, "\tif( hb_xvmDivideByInt( %ld ) ) break;\n", lValue );
            return 1;

         case HB_P_PLUS:
            fprintf( cargo->yyc, "\tif( hb_xvmAddInt( %ld ) ) break;\n", lValue );
            return 1;

         case HB_P_MINUS:
            fprintf( cargo->yyc, "\tif( hb_xvmAddInt( -%ld ) ) break;\n", lValue );
            return 1;
      }
   }
   return 0;
}

static HB_GENC_FUNC( hb_p_and )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmAnd() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraypush )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmArrayPush() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraypop )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmArrayPop() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_dec )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmDec() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraydim )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmArrayDim( %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_divide )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmDivide() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_do )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmDo( %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_doshort )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmDo( %d ) ) break;\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_duplicate )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmDuplicate();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_dupltwo )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmDuplTwo();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_equal )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmEqual( FALSE ) ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_exactlyequal )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmEqual( TRUE ) ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_false )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushLogical( FALSE );\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_fortest )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmForTest() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_frame )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmFrame( %hu, %hu );\n",
            pFunc->pCode[ lPCodePos + 1 ], pFunc->pCode[ lPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_funcptr )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmFuncPtr();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_function )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmFunction( %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_functionshort )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmFunction( %d ) ) break;\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_arraygen )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmArrayGen( %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_hashgen )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmHashGen( %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_greater )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmGreater() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_greaterequal )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmGreaterEqual() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_inc )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmInc() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_instring )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmInstring() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_jumpnear )
{
   LONG lOffset = ( signed char ) ( pFunc->pCode[ lPCodePos + 1 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tgoto lab%05ld;\n",
            HB_GENC_GETLABEL( lPCodePos + lOffset ) );
   return 2;
}

static HB_GENC_FUNC( hb_p_jump )
{
   LONG lOffset = HB_PCODE_MKSHORT( &pFunc->pCode[ lPCodePos + 1 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tgoto lab%05ld;\n",
            HB_GENC_GETLABEL( lPCodePos + lOffset ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_jumpfar )
{
   LONG lOffset = HB_PCODE_MKINT24( &pFunc->pCode[ lPCodePos + 1 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tgoto lab%05ld;\n",
            HB_GENC_GETLABEL( lPCodePos + lOffset ) );
   return 4;
}

static HB_GENC_FUNC( hb_p_jumpfalsenear )
{
   LONG lOffset = ( signed char ) ( pFunc->pCode[ lPCodePos + 1 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopLogical( &fValue ) ) break;\n\tif( !fValue )\n\t\tgoto lab%05ld;\n",
            HB_GENC_GETLABEL( lPCodePos + lOffset ) );
   return 2;
}

static HB_GENC_FUNC( hb_p_jumpfalse )
{
   LONG lOffset = HB_PCODE_MKSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopLogical( &fValue ) ) break;\n\tif( !fValue )\n\t\tgoto lab%05ld;\n",
            HB_GENC_GETLABEL( lPCodePos + lOffset ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_jumpfalsefar )
{
   LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopLogical( &fValue ) ) break;\n\tif( !fValue )\n\t\tgoto lab%05ld;\n",
            HB_GENC_GETLABEL( lPCodePos + lOffset ) );
   return 4;
}

static HB_GENC_FUNC( hb_p_jumptruenear )
{
   LONG lOffset = ( signed char ) ( pFunc->pCode[ lPCodePos + 1 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopLogical( &fValue ) ) break;\n\tif( fValue )\n\t\tgoto lab%05ld;\n",
            HB_GENC_GETLABEL( lPCodePos + lOffset ) );
   return 2;
}

static HB_GENC_FUNC( hb_p_jumptrue )
{
   LONG lOffset = HB_PCODE_MKSHORT( &pFunc->pCode[ lPCodePos + 1 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopLogical( &fValue ) ) break;\n\tif( fValue )\n\t\tgoto lab%05ld;\n",
            HB_GENC_GETLABEL( lPCodePos + lOffset ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_jumptruefar )
{
   LONG lOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ lPCodePos + 1 ] ) );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopLogical( &fValue ) ) break;\n\tif( fValue )\n\t\tgoto lab%05ld;\n",
            HB_GENC_GETLABEL( lPCodePos + lOffset ) );
   return 4;
}

static HB_GENC_FUNC( hb_p_less )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmLess() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_lessequal )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmLessEqual() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_line )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmSetLine( %d );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_localname )
{
   HB_SIZE usLen;

   HB_GENC_LABEL();

   usLen = strlen( ( char * ) &pFunc->pCode[ lPCodePos + 3 ] );
   fprintf( cargo->yyc, "\thb_xvmLocalName( %hu, ",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   hb_gencc_string_put( cargo->yyc, &pFunc->pCode[ lPCodePos + 3 ], usLen );
   fprintf( cargo->yyc, " );\n" );
   return usLen + 4;
}

static HB_GENC_FUNC( hb_p_macropop )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroPop( %d ) ) break;\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropopaliased )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroPopAliased( %d ) ) break;\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropush )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroPush( %d ) ) break;\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropusharg )
{
   USHORT usSize, usSymbol;

   HB_GENC_LABEL();

   if( pFunc->pCode[ lPCodePos + 2 ] == HB_P_PUSHSYMNEAR )
   {
      usSymbol = pFunc->pCode[ lPCodePos + 3 ];
      usSize   = 4;
   }
   else
   {
      usSymbol = HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 3 ] );
      usSize   = 5;
   }

   fprintf( cargo->yyc, "\tif( hb_xvmMacroPushArg( symbols + %hu, %d ) ) break;\n",
            usSymbol, pFunc->pCode[ lPCodePos + 1 ] );
   return usSize;
}

static HB_GENC_FUNC( hb_p_macropushlist )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroPushList( %d ) ) break;\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropushindex )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroPushIndex( %d ) ) break;\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropushpare )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroPushPare( %d ) ) break;\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropushaliased )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroPushAliased( %d ) ) break;\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macrosymbol )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroSymbol() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_macrotext )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroText() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_message )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushSymbol( symbols + %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_minus )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMinus() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_modulename )
{
   HB_SIZE usLen;

   HB_GENC_LABEL();

   usLen = strlen( ( char * ) &pFunc->pCode[ lPCodePos + 1 ] );
   fprintf( cargo->yyc, "\thb_xvmModuleName( " );
   hb_gencc_string_put( cargo->yyc, &pFunc->pCode[ lPCodePos + 1 ], usLen );
   fprintf( cargo->yyc, " );\n" );
   return usLen + 2;
}

static HB_GENC_FUNC( hb_p_modulus )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmModulus() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_mult )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMult() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_negate )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmNegate() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_not )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmNot() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_notequal )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmNotEqual() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_or )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmOr() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_parameter )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmParameter( symbols + %hu, %d );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ),
            pFunc->pCode[ lPCodePos + 3 ] );
   return 4;
}

static HB_GENC_FUNC( hb_p_plus )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPlus() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pop )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_stackPop();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_popalias )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopAlias() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_popaliasedfield )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopAliasedField( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_popaliasedfieldnear )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopAliasedField( symbols + %hu ) ) break;\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_popaliasedvar )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopAliasedVar( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_popfield )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopField( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_poplocal )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPopLocal( %hd );\n",
            HB_PCODE_MKSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_poplocalnear )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPopLocal( %hd );\n",
            ( signed char ) pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_popmemvar )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopMemvar( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_popstatic )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPopStatic( %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_popvariable )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopVariable( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_power )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPower() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushalias )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushAlias() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushaliasedfield )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushAliasedField( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushaliasedfieldnear )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushAliasedField( symbols + %d ) ) break;\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushaliasedvar )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushAliasedVar( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushblock )
{
   USHORT usSize, us;

   HB_GENC_LABEL();

   usSize = HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) - 3;

   fprintf( cargo->yyc, "\t{\n\t\tstatic const BYTE codeblock[ %hd ] = {", usSize );

   for( us = 0; us < usSize; ++us )
   {
      if( ( us & 0x0f ) == 0 )
         fprintf( cargo->yyc, "\n\t\t\t" );
      if( us == usSize - 1 )
         fprintf( cargo->yyc, "%d", pFunc->pCode[ lPCodePos + 3 + us ] );
      else
         fprintf( cargo->yyc, "%d, ", pFunc->pCode[ lPCodePos + 3 + us ] );
   }
   fprintf( cargo->yyc, " };\n\t\thb_xvmPushBlock( codeblock, %hu );\n\t}\n", usSize );

   return 3 + usSize;
}

static HB_GENC_FUNC( hb_p_pushblockshort )
{
   USHORT usSize, us;

   HB_GENC_LABEL();

   usSize = pFunc->pCode[ lPCodePos + 1 ] - 2;

   fprintf( cargo->yyc, "\t{\n\t\tstatic const BYTE codeblock[ %hd ] = {", usSize );

   for( us = 0; us < usSize; ++us )
   {
      if( ( us & 0x0f ) == 0 )
         fprintf( cargo->yyc, "\n\t\t\t" );
      if( us == usSize - 1 )
         fprintf( cargo->yyc, "%d", pFunc->pCode[ lPCodePos + 2 + us ] );
      else
         fprintf( cargo->yyc, "%d, ", pFunc->pCode[ lPCodePos + 2 + us ] );
   }
   fprintf( cargo->yyc, " };\n\t\thb_xvmPushBlockShort( codeblock, %hu );\n\t}\n", usSize );

   return 2 + usSize;
}

static HB_GENC_FUNC( hb_p_pushdouble )
{
   HB_GENC_LABEL();

#if 0
   fprintf( cargo->yyc, "\thb_xvmPushDouble( %.*f, %d, %d );\n",
            pFunc->pCode[ lPCodePos + 1 + sizeof( double ) + sizeof( BYTE ) ] + 1,
            HB_PCODE_MKDOUBLE( &pFunc->pCode[ lPCodePos + 1 ] ),
            pFunc->pCode[ lPCodePos + 1 + sizeof( double ) ],
            pFunc->pCode[ lPCodePos + 1 + sizeof( double ) + sizeof( BYTE ) ] );
#else
   /*
    * This version keeps double calculation compatible with RT FL functions
    */
   fprintf( cargo->yyc, "\thb_xvmPushDouble( * ( double * ) " );
   {
      double d = HB_PCODE_MKDOUBLE( &pFunc->pCode[ lPCodePos + 1 ] );
      hb_gencc_string_put( cargo->yyc, ( BYTE * ) &d, sizeof( double ) );
   }
   fprintf( cargo->yyc, ", %d, %d );\n",
            pFunc->pCode[ lPCodePos + 1 + sizeof( double ) ],
            pFunc->pCode[ lPCodePos + 1 + sizeof( double ) + sizeof( BYTE ) ] );
#endif
   return sizeof( double ) + sizeof( BYTE ) + sizeof( BYTE ) + 1;
}

static HB_GENC_FUNC( hb_p_pushfield )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushField( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushbyte )
{
   int iVal = ( signed char ) pFunc->pCode[ lPCodePos + 1 ], iSkip;

   HB_GENC_LABEL();

   iSkip = hb_gencc_checkNumAhead( iVal, pFunc, lPCodePos + 2, cargo );

   if( iSkip == 0 )
      fprintf( cargo->yyc, "\thb_xvmPushInteger( %d );\n", iVal );
   return 2 + iSkip;
}

static HB_GENC_FUNC( hb_p_pushint )
{
   int iVal = HB_PCODE_MKSHORT( &pFunc->pCode[ lPCodePos + 1 ] ), iSkip;

   HB_GENC_LABEL();

   iSkip = hb_gencc_checkNumAhead( iVal, pFunc, lPCodePos + 3, cargo );

   if( iSkip == 0 )
      fprintf( cargo->yyc, "\thb_xvmPushInteger( %d );\n", iVal );
   return 3 + iSkip;
}

static HB_GENC_FUNC( hb_p_pushlocal )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushLocal( %d );\n",
            HB_PCODE_MKSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushlocalnear )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushLocal( %d );\n",
            ( signed char ) pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_localnearinc )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmLocalAddInt( %d, 1 );\n",
            ( signed char ) pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_localneardec )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmLocalAddInt( %d, -1 );\n",
            ( signed char ) pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushlocalnearinc )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmLocalAddInt( %d, 1 );\n",
            ( signed char ) pFunc->pCode[ lPCodePos + 1 ] );
   fprintf( cargo->yyc, "\thb_xvmPushLocal( %d );\n",
            ( signed char ) pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushlocalneardec )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmLocalAddInt( %d, -1 );\n",
            ( signed char ) pFunc->pCode[ lPCodePos + 1 ] );
   fprintf( cargo->yyc, "\thb_xvmPushLocal( %d );\n",
            ( signed char ) pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushlocalref )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushLocalByRef( %d );\n",
            HB_PCODE_MKSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushlong )
{
   LONG lVal = HB_PCODE_MKLONG( &pFunc->pCode[ lPCodePos + 1 ] ), iSkip;

   HB_GENC_LABEL();

   iSkip = hb_gencc_checkNumAhead( lVal, pFunc, lPCodePos + 5, cargo );

   if( iSkip == 0 )
   {
#if HB_INT_MAX >= INT32_MAX
      fprintf( cargo->yyc, "\thb_xvmPushInteger( %d );\n", ( int ) lVal );
#else
      fprintf( cargo->yyc, "\thb_xvmPushLong( %ldL );\n", ( long ) lVal );
#endif
   }
   return 5 + iSkip;
}

static HB_GENC_FUNC( hb_p_pushlonglong )
{
#ifdef HB_LONG_LONG_OFF
   HB_GENC_LABEL();
   fprintf( cargo->yyc, "\thb_xvmPushLongLong( %.1f );\n", HB_PCODE_MKLONGLONG( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 9;
#elif LONG_MAX == LONGLONG_MAX
   LONGLONG llVal = HB_PCODE_MKLONGLONG( &pFunc->pCode[ lPCodePos + 1 ] ), iSkip;

   HB_GENC_LABEL();

   iSkip = hb_gencc_checkNumAhead( llVal, pFunc, lPCodePos + 9, cargo );

   if( iSkip == 0 )
   {
      fprintf( cargo->yyc, "\thb_xvmPushLong( %ldL );\n", ( long ) llVal );
   }
   return 9 + iSkip;
#else
   LONGLONG llVal = HB_PCODE_MKLONGLONG( &pFunc->pCode[ lPCodePos + 1 ] );
   char szBuf[ 24 ];	
   HB_GENC_LABEL();
   //fprintf( cargo->yyc, "\thb_xvmPushLongLong( HB_LL( %" PFLL "i ) );\n", HB_PCODE_MKLONGLONG( &pFunc->pCode[ lPCodePos + 1 ] ) );
   fprintf( cargo->yyc, "\thb_xvmPushLongLong( HB_LL( %s ) );\n", hb_numToStr( szBuf, sizeof( szBuf ), llVal ) );
   return 9;
#endif
}

static HB_GENC_FUNC( hb_p_pushmemvar )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushMemvar( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushmemvarref )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushMemvarByRef( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushnil )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushNil();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushself )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushSelf();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushstatic )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushStatic( %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushstaticref )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushStaticByRef( %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushstr )
{
   USHORT usLen = HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) - 1;

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushStringConst( " );
   hb_gencc_string_put( cargo->yyc, &pFunc->pCode[ lPCodePos + 3 ], usLen );
   fprintf( cargo->yyc, ", %hu );\n", usLen );

   return 4 + usLen;
}

static HB_GENC_FUNC( hb_p_pushstrshort )
{
   USHORT usLen = pFunc->pCode[ lPCodePos + 1 ] - 1;

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushStringConst( " );
   hb_gencc_string_put( cargo->yyc, &pFunc->pCode[ lPCodePos + 2 ], usLen );
   fprintf( cargo->yyc, ", %hu );\n", usLen );

   return 3 + usLen;
}

static HB_GENC_FUNC( hb_p_pushsym )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushSymbol( symbols + %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushsymnear )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushSymbol( symbols + %d );\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushvariable )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushVariable( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_retvalue )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmRetValue();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_send )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmSend( %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_sendshort )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmSend( %d ) ) break;\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_sframe )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmSFrame( symbols + %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_statics )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmStatics( symbols + %hu, %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ),
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 3 ] ) );
   return 5;
}

static HB_GENC_FUNC( hb_p_staticname )
{
   HB_SIZE usLen;

   HB_GENC_LABEL();

   usLen = strlen( ( char * ) &pFunc->pCode[ lPCodePos + 4 ] );
   fprintf( cargo->yyc, "\thb_xvmStaticName( %hu, %hu, ",
            ( USHORT ) pFunc->pCode[ lPCodePos + 1 ],
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 2 ] ) );
   hb_gencc_string_put( cargo->yyc, &pFunc->pCode[ lPCodePos + 4 ], usLen );
   fprintf( cargo->yyc, " );\n" );
   return usLen + 5;
}

static HB_GENC_FUNC( hb_p_swapalias )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmSwapAlias() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_true )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushLogical( TRUE );\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_one )
{
   int iSkip;

   HB_GENC_LABEL();

   iSkip = hb_gencc_checkNumAhead( 1, pFunc, lPCodePos + 1, cargo );
   if( iSkip == 0 )
      fprintf( cargo->yyc, "\thb_xvmPushInteger( 1 );\n" );
   return 1 + iSkip;
}

static HB_GENC_FUNC( hb_p_zero )
{
   int iSkip;

   HB_GENC_LABEL();

   iSkip = hb_gencc_checkNumAhead( 0, pFunc, lPCodePos + 1, cargo );
   if( iSkip == 0 )
      fprintf( cargo->yyc, "\thb_xvmPushInteger( 0 );\n" );
   return 1 + iSkip;
}

static HB_GENC_FUNC( hb_p_noop )
{
   HB_GENC_LABEL();

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
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmMacroList();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_macrolistend )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmMacroListEnd();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_localnearaddint )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmLocalAddInt( %d, %d ) ) break;\n",
            ( signed char ) pFunc->pCode[ lPCodePos + 1 ],
            HB_PCODE_MKSHORT( &pFunc->pCode[ lPCodePos + 2 ] ) );
   return 4;
}

static HB_GENC_FUNC( hb_p_localnearsetint )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmLocalSetInt( %d, %d );\n",
            ( signed char ) pFunc->pCode[ lPCodePos + 1 ],
            HB_PCODE_MKSHORT( &pFunc->pCode[ lPCodePos + 2 ] ) );
   return 4;
}

static HB_GENC_FUNC( hb_p_localnearsetstr )
{
   USHORT usLen = HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 2 ] ) - 1;

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmLocalSetStr( %d, ",
            ( signed char ) pFunc->pCode[ lPCodePos + 1 ] );
   hb_gencc_string_put( cargo->yyc, &pFunc->pCode[ lPCodePos + 4 ], usLen );
   fprintf( cargo->yyc, ", %hu );\n", usLen );

   return 5 + usLen;
}

static HB_GENC_FUNC( hb_p_addint )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmAddInt( %hd ) ) break;\n",
            HB_PCODE_MKSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_left )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmLeft( %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_right )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmRight( %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_substr )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmSubstr( %hu, %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ),
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 5;
}

static HB_GENC_FUNC( hb_p_lineoffset )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmLineOffset( %d );\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_baseline )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmBaseLine( %d );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_withobject )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmWithObject() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_sendwith )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmSendWith( %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_sendwithshort )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmSendWith( %hu ) ) break;\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_endwithobject )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmEndWithObject();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_foreach )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmForEach() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_enumerate )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmEnumerate() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_endenumerate )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmEndEnumerate() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_enumindex )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmEnumIndex();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushglobal )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushGlobal( %d, &ppGlobals );\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushglobalref )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushGlobalByRef( %d, &ppGlobals );\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_popglobal )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPopGlobal( %d, &ppGlobals );\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_switchcase )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmSwitchCase( %ld ) ) break;\n",
            HB_PCODE_MKLONG( &pFunc->pCode[ lPCodePos + 1 ] ) );
   return 5;
}

static HB_GENC_FUNC( hb_p_like )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmLike() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_match )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMatch() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushmacroref )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushMacroRef() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_ivarref )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmIVarRef() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_classsetmodule )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmClassSetModule() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_bitand )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmBitAnd() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_bitor )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmBitOr() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_bitxor )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmBitXor() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_bitshiftr )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmBitShiftR() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_bitshiftl )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmBitShiftL() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_largeframe )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmFrame( %hu, %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ),
            pFunc->pCode[ lPCodePos + 3 ] );
   return 4;
}

static HB_GENC_FUNC( hb_p_pushwith )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushWith();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushstrhidden )
{
   USHORT usLen       = HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] );
   BYTE   bType       = pFunc->pCode[ lPCodePos + 3 ];
   USHORT usBufferLen = HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 4 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushStringHidden( %d, %hu, ", bType, usLen );
   hb_gencc_string_put( cargo->yyc, &pFunc->pCode[ lPCodePos + 6 ], usBufferLen );
   fprintf( cargo->yyc, ", %hu );\n", usBufferLen );

   return 6 + usBufferLen;
}

static HB_GENC_FUNC( hb_p_localnearsetstrhidden )
{
   signed char cLocal      = ( signed char ) pFunc->pCode[ lPCodePos + 1 ];
   USHORT      usLen       = HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 2 ] );
   BYTE        bType       = pFunc->pCode[ lPCodePos + 4 ];
   USHORT      usBufferLen = HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 5 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmLocalSetStringHidden( %d, %d, %hu, ",
            cLocal, bType, usLen );
   hb_gencc_string_put( cargo->yyc, &pFunc->pCode[ lPCodePos + 7 ], usBufferLen );
   fprintf( cargo->yyc, ", %hu );\n", usBufferLen );

   return 7 + usBufferLen;
}

static HB_GENC_FUNC( hb_p_localnearadd )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmLocalAdd( %d ) ) break;\n",
            ( signed char ) pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_arraypushref )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmArrayPushRef() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraypopplus )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmArrayPopPlus() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_seqbegin )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmSeqBegin();\n\tdo {\n" );
   cargo->iNestedBlock++;
   return 4;
}

static HB_GENC_FUNC( hb_p_seqend )
{
   LONG lOffset = HB_PCODE_MKINT24( &pFunc->pCode[ lPCodePos + 1 ] );

   HB_GENC_LABEL();

   if( lOffset == 4 )   /* no RECOVER clasue */
      fprintf( cargo->yyc, "\t} while( 0 );\n\thb_xvmSeqEnd();\n" );
   else                 /* RECOVER exists */
      fprintf( cargo->yyc, "\thb_xvmSeqEnd();\n\tgoto lab%05ld;\n\t} while( 0 );\n",
               HB_GENC_GETLABEL( lPCodePos + lOffset ) );
   return 4;
}

static HB_GENC_FUNC( hb_p_seqrecover )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmSeqRecover();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_trybegin )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmTryBegin();\n\tdo {\n" );
   cargo->iNestedBlock++;
   return 4;
}

static HB_GENC_FUNC( hb_p_tryend )
{
   LONG lOffset = HB_PCODE_MKINT24( &pFunc->pCode[ lPCodePos + 1 ] );

   HB_GENC_LABEL();

   cargo->iNestedBlock--;

   if( lOffset == 4 ) /* no RECOVER clasue */
   {
      /*
       * Now we know that FINALLY has to follow this PCODE
       * so we use hb_xvmTryEndFin() to PASS an information that
       * currently set error object and code should be keept
       */
      fprintf( cargo->yyc, "\t} while( 0 );\n\thb_xvmTryEndFin();\n" );

      /* open dummy RECOVER block, it will be closed by FINALLY pcode */
      fprintf( cargo->yyc, "\tdo {\n" );
      cargo->iFinally++;
      cargo->iNestedBlock++;
   }
   else /* RECOVER exists */
   {
      /*
       * PCODE generated by compiler has been fixed and we do not have to
       * extract finally address from RECOVER clause
       */
#if 1
      fprintf( cargo->yyc, "\thb_xvmTryEnd();\n\tgoto lab%05ld;\n\t} while( 0 );\n",
               HB_GENC_GETLABEL( lPCodePos + lOffset ) );
#else
      /*
       * the lOffset skipps throu recover section which has to begin
       * HB_P_TRYRECOVER pcode with optional lFinally jump
       */
      LONG  lFinally = 0;
      ULONG ulPos;

      if( pFunc->pCode[ lPCodePos + 4 ] == HB_P_TRYRECOVER )
         lFinally = HB_PCODE_MKINT24( &pFunc->pCode[ lPCodePos + 5 ] );

      ulPos = lPCodePos + ( lFinally == 0 ? lOffset : ( lFinally + 4 ) );
      fprintf( cargo->yyc, "\thb_xvmTryEnd();\n\tgoto lab%05ld;\n\t} while( 0 );\n",
               HB_GENC_GETLABEL( ulPos ) );
#endif
   }
   return 4;
}

static HB_GENC_FUNC( hb_p_tryrecover )
{
   LONG lOffset = HB_PCODE_MKINT24( &pFunc->pCode[ lPCodePos + 1 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmTryRecover();\n" );

   if( lOffset != 0 )
   {
      /* open the RECOVER block for FINALLY support */
      fprintf( cargo->yyc, "\tdo {\n" );
      cargo->iFinally++;
      cargo->iNestedBlock++;
   }

   return 4;
}

static HB_GENC_FUNC( hb_p_finally )
{
   /* close the RECOVER block */
   fprintf( cargo->yyc, "\t} while(0);\n" );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\t{\n\tUSHORT uiActionRequest%d = hb_xvmBeginFinally();\n\tdo {\n",
            cargo->iFinally );

   return 1;
}

static HB_GENC_FUNC( hb_p_endfinally )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\t} while(0);\n" );
   fprintf( cargo->yyc, "\tif( hb_xvmEndFinally( uiActionRequest%d ) ) break;\n\t}\n",
            cargo->iFinally );
   cargo->iFinally--;
   cargo->iNestedBlock--;

   return 1;
}

static HB_GENC_FUNC( hb_p_endproc )
{
   BOOL fEnd = ( lPCodePos == pFunc->lPCodePos - 1 );

   HB_GENC_LABEL();

   if( fEnd )
   {
      while( cargo->iFinally )
      {
         fprintf( cargo->yyc, "\t} while(0);\n" );
         fprintf( cargo->yyc, "\tif( hb_xvmEndFinally( uiActionRequest%d ) ) break;\n\t}\n",
                  cargo->iFinally );
         cargo->iFinally--;
         cargo->iNestedBlock--;
      }
   }

   fprintf( cargo->yyc, "\t/* *** END PROC *** */\n" );
   if( ! fEnd )
   {
      if( cargo->iNestedBlock > 0 )
      {
         if( cargo->iFinally )
         {
            fprintf( cargo->yyc, "\thb_vmRequest( HB_ENDPROC_REQUESTED );\n" );
            fprintf( cargo->yyc, "\tbreak;\n" );
         }
         else
         {
            fprintf( cargo->yyc, "\tgoto labEND;\n" );
            cargo->fEndProc = TRUE;
         }
      }
      else
      {
         fprintf( cargo->yyc, "\tbreak;\n" );
      }
   }
   return 1;
}

static HB_GENC_FUNC( hb_p_endblock )
{
   HB_GENC_LABEL();

   HB_GENC_ERROR( "HB_P_ENDBLOCK" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushdatetime )
{
   LONG lVal1 = HB_PCODE_MKLONG( &pFunc->pCode[ lPCodePos + 1 ] );
   LONG lVal2 = HB_PCODE_MKLONG( &pFunc->pCode[ lPCodePos + 5 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushDateTime( %ldL, %ldL );\n", lVal1, lVal2 );
   return 9;
}

static HB_GENC_FUNC( hb_p_pushdate )
{
   LONG lVal = HB_PCODE_MKLONG( &pFunc->pCode[ lPCodePos + 1 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushDate( %ldL );\n", lVal );
   return 5;
}

static HB_GENC_FUNC( hb_p_divert )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmDivert( FALSE );\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_divertof )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmDivert( TRUE );\n" );
   return 1;
}

/* NOTE: The order of functions have to match the order of opcodes mnemonics
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

void hb_compGenCRealCode( PFUNCTION pFunc, FILE * yyc )
{
   HB_LABEL_INFO label_info;

   /* Make sure that table is correct */
   assert( HB_P_LAST_PCODE == sizeof( s_verbose_table ) / sizeof( PHB_GENC_FUNC ) );

   label_info.yyc          = yyc;
   label_info.fVerbose     = ( hb_comp_iGenCOutput == HB_COMPGENC_VERBOSE );
   label_info.fSetSeqBegin = FALSE;
   label_info.fCondJump    = label_info.fEndProc = FALSE;
   label_info.iNestedBlock = label_info.iFinally = 0;

   if( pFunc->lPCodePos == 0 )
   {
      label_info.pulLabels = NULL;
   }
   else
   {
      label_info.pulLabels = ( ULONG * ) hb_xgrab( pFunc->lPCodePos * sizeof( ULONG ) );
      memset( label_info.pulLabels, 0, ( size_t ) ( pFunc->lPCodePos * sizeof( HB_SIZE ) ) );
      hb_compGenLabelTable( pFunc, &label_info );
   }

   fprintf( yyc, "{\n" );

   if( label_info.fCondJump )
      fprintf( yyc, "   BOOL fValue;\n\n" );
   else
      fprintf( yyc, "\n" );

   if( pFunc->cScope & HB_FS_CRITICAL )
      fprintf( yyc, "   HB_CRITICAL_LOCK( s_Critical%s );\n", pFunc->szName );

   fprintf( yyc, "   do {\n" );

   hb_compPCodeEval( pFunc, ( PHB_PCODE_FUNC * ) s_verbose_table, ( void * ) &label_info );

   fprintf( yyc, "   } while ( 0 );\n" );


   if( label_info.fEndProc )
      fprintf( yyc, "labEND:\n" );

   if( pFunc->cScope & HB_FS_CRITICAL )
      fprintf( yyc, "   HB_CRITICAL_UNLOCK( s_Critical%s );\n", pFunc->szName );

   fprintf( yyc, "\n   hb_xvmExitProc();\n" );
   fprintf( yyc, "}\n" );

   if( label_info.pulLabels )
      hb_xfree( label_info.pulLabels );
}
