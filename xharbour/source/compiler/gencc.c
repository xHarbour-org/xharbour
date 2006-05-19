/*
 * $Id: gencc.c,v 1.7 2006/05/16 22:57:08 druzus Exp $
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

#define HB_GENC_FUNC( func ) HB_PCODE_FUNC( func, PHB_LABEL_INFO )
typedef HB_GENC_FUNC( HB_GENC_FUNC_ );
typedef HB_GENC_FUNC_ * HB_GENC_FUNC_PTR;

#define HB_GENC_GETLABEL(l)   ( l < pFunc->lPCodePos ? cargo->pulLabels[ l ] : 0 )

#define HB_GENC_LABEL()       do { \
                                 ULONG ulLab = HB_GENC_GETLABEL( lPCodePos ); \
                                 if( ulLab != 0 ) \
                                    fprintf( cargo->yyc, "lab%05ld: ;\n", ulLab ); \
                              } while( 0 )

#define HB_GENC_ERROR(s)       do { \
                                 fprintf( cargo->yyc, "\t#error: \"" s "\"\n" ); \
                              } while( 0 )

static void hb_gencc_string_put( FILE * yyc, BYTE * pText, USHORT usLen )
{
   USHORT usPos;

   fputc( '"', yyc );
   for( usPos = 0; usPos < usLen; usPos++ )
   {
      BYTE uchr = ( BYTE ) pText[ usPos ];
      /*
       * NOTE: After optimization some CHR(n) can be converted
       *    into a string containing nonprintable characters.
       *
       * TODO: add switch to use hexadecimal format "%#04x"
       *
       * ? is escaped to avoid conflicts with trigraph sequences which
       * are part of ANSI C standard
       */
      if( uchr == '"' || uchr == '\\' || uchr == '?' )
         fprintf( yyc, "\\%c", uchr );
      else if( uchr < ( BYTE ) ' ' || uchr >= 127 )
         fprintf( yyc, "\\%03o", uchr );
      else
         fprintf( yyc, "%c", uchr );
   }
   fputc( '"', yyc );
}

static int hb_gencc_checkNumAhead( LONG lValue, PFUNCTION pFunc, ULONG lPCodePos, PHB_LABEL_INFO cargo )
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
   USHORT usLen;

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
      usSize = 4;
   }
   else
   {
      usSymbol = HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 3 ] );
      usSize = 5;
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
   USHORT usLen;

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
   fprintf( cargo->yyc, " };\n\t\thb_xvmPushBlock( codeblock, %hu, symbols, %s );\n\t}\n",
            usSize, hb_comp_pGlobals ? "&pGlobals" : "NULL" );

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
   fprintf( cargo->yyc, " };\n\t\thb_xvmPushBlockShort( codeblock, %hu, symbols, %s );\n\t}\n",
            usSize, hb_comp_pGlobals ? "&pGlobals" : "NULL" );

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
   hb_gencc_string_put( cargo->yyc, &pFunc->pCode[ lPCodePos + 1 ], sizeof( double ) );
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
   HB_GENC_LABEL();
   fprintf( cargo->yyc, "\thb_xvmPushLongLong( HB_LL( %" PFLL "i ) );\n", HB_PCODE_MKLONGLONG( &pFunc->pCode[ lPCodePos + 1 ] ) );
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
   USHORT usLen;

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
            pFunc->pCode[ lPCodePos + 1 ],
            HB_PCODE_MKSHORT( &pFunc->pCode[ lPCodePos + 2 ] ) );
   return 4;
}

static HB_GENC_FUNC( hb_p_localnearsetint )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmLocalSetInt( %d, %d );\n",
            pFunc->pCode[ lPCodePos + 1 ],
            HB_PCODE_MKSHORT( &pFunc->pCode[ lPCodePos + 2 ] ) );
   return 4;
}

static HB_GENC_FUNC( hb_p_localnearsetstr )
{
   USHORT usLen = HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 2 ] ) - 1;

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmLocalSetStr( %d, ",
            pFunc->pCode[ lPCodePos + 1 ] );
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

   fprintf( cargo->yyc, "\thb_xvmPushGlobal( %d, &pGlobals );\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushglobalref )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushGlobalByRef( %d, &pGlobals );\n",
            pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_popglobal )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPopGlobal( %d, &pGlobals );\n",
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
   USHORT usLen = HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] );
   BYTE bType = pFunc->pCode[ lPCodePos + 3 ];
   USHORT usBufferLen = HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 4 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushStringHidden( %d, %hu, ", bType, usLen );
   hb_gencc_string_put( cargo->yyc, &pFunc->pCode[ lPCodePos + 6 ], usBufferLen );
   fprintf( cargo->yyc, ", %hu );\n", usBufferLen );

   return 6 + usBufferLen;
}

static HB_GENC_FUNC( hb_p_localnearsetstrhidden )
{
   BYTE bLocal = pFunc->pCode[ lPCodePos + 1 ];
   USHORT usLen = HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 2 ] );
   BYTE bType = pFunc->pCode[ lPCodePos + 4 ];
   USHORT usBufferLen = HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 5 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmLocalSetStringHidden( %d, %d, %hu, ",
            bLocal, bType, usLen );
   hb_gencc_string_put( cargo->yyc, &pFunc->pCode[ lPCodePos + 7 ], usBufferLen );
   fprintf( cargo->yyc, ", %hu );\n", usBufferLen );

   return 7 + usBufferLen;
}

static HB_GENC_FUNC( hb_p_localnearadd )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmLocalAdd( %d ) ) break;\n",
            pFunc->pCode[ lPCodePos + 1 ] );
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

   if( lOffset == 4 ) /* no RECOVER clasue */
      fprintf( cargo->yyc, "\t} while( 0 );\n\thb_xvmSeqEnd();\n" );
   else /* RECOVER exists */
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
      LONG lFinally = 0;
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
   if( !fEnd )
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

/* NOTE: The  order of functions have to match the order of opcodes
 *       mnemonics
 */
static HB_GENC_FUNC_PTR s_verbose_table[] = {
   hb_p_and,
   hb_p_arraypush,
   hb_p_arraypop,
   hb_p_arraydim,
   hb_p_arraygen,
   hb_p_equal,
   hb_p_endblock,
   hb_p_endproc,
   hb_p_exactlyequal,
   hb_p_false,
   hb_p_fortest,
   hb_p_function,
   hb_p_functionshort,
   hb_p_frame,
   hb_p_funcptr,
   hb_p_greater,
   hb_p_greaterequal,
   hb_p_dec,
   hb_p_divide,
   hb_p_do,
   hb_p_doshort,
   hb_p_duplicate,
   hb_p_dupltwo,
   hb_p_inc,
   hb_p_instring,
   hb_p_jumpnear,
   hb_p_jump,
   hb_p_jumpfar,
   hb_p_jumpfalsenear,
   hb_p_jumpfalse,
   hb_p_jumpfalsefar,
   hb_p_jumptruenear,
   hb_p_jumptrue,
   hb_p_jumptruefar,
   hb_p_lessequal,
   hb_p_less,
   hb_p_line,
   hb_p_localname,
   hb_p_macropop,
   hb_p_macropopaliased,
   hb_p_macropush,
   hb_p_macropusharg,
   hb_p_macropushlist,
   hb_p_macropushindex,
   hb_p_macropushpare,
   hb_p_macropushaliased,
   hb_p_macrosymbol,
   hb_p_macrotext,
   hb_p_message,
   hb_p_minus,
   hb_p_modulus,
   hb_p_modulename,
   /* start: pcodes generated by macro compiler */
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   /* end: */
   hb_p_mult,
   hb_p_negate,
   hb_p_noop,
   hb_p_not,
   hb_p_notequal,
   hb_p_or,
   hb_p_parameter,
   hb_p_plus,
   hb_p_pop,
   hb_p_popalias,
   hb_p_popaliasedfield,
   hb_p_popaliasedfieldnear,
   hb_p_popaliasedvar,
   hb_p_popfield,
   hb_p_poplocal,
   hb_p_poplocalnear,
   hb_p_popmemvar,
   hb_p_popstatic,
   hb_p_popvariable,
   hb_p_power,
   hb_p_pushalias,
   hb_p_pushaliasedfield,
   hb_p_pushaliasedfieldnear,
   hb_p_pushaliasedvar,
   hb_p_pushblock,
   hb_p_pushblockshort,
   hb_p_pushfield,
   hb_p_pushbyte,
   hb_p_pushint,
   hb_p_pushlocal,
   hb_p_pushlocalnear,
   hb_p_pushlocalref,
   hb_p_pushlong,
   hb_p_pushmemvar,
   hb_p_pushmemvarref,
   hb_p_pushnil,
   hb_p_pushdouble,
   hb_p_pushself,
   hb_p_pushstatic,
   hb_p_pushstaticref,
   hb_p_pushstr,
   hb_p_pushstrshort,
   hb_p_pushsym,
   hb_p_pushsymnear,
   hb_p_pushvariable,
   hb_p_retvalue,
   hb_p_send,
   hb_p_sendshort,
   hb_p_seqbegin,
   hb_p_seqend,
   hb_p_seqrecover,
   hb_p_sframe,
   hb_p_statics,
   hb_p_staticname,
   hb_p_swapalias,
   hb_p_true,
   hb_p_zero,
   hb_p_one,
   hb_p_macrolist,
   hb_p_macrolistend,

   hb_p_localnearaddint,
   hb_p_localnearsetint,
   hb_p_localnearsetstr,
   hb_p_addint,
   hb_p_left,
   hb_p_right,
   hb_p_substr,
   hb_p_dummy,
   hb_p_baseline,
   hb_p_lineoffset,
   hb_p_withobject,
   hb_p_sendwith,
   hb_p_sendwithshort,
   hb_p_endwithobject,
   hb_p_foreach,
   hb_p_enumerate,
   hb_p_endenumerate,
   hb_p_pushglobal,
   hb_p_popglobal,
   hb_p_pushglobalref,
   hb_p_enumindex,
   hb_p_switchcase,
   hb_p_like,
   hb_p_match,
   hb_p_pushmacroref,
   hb_p_ivarref,
   hb_p_classsetmodule,
   hb_p_bitand,
   hb_p_bitor,
   hb_p_bitxor,
   hb_p_bitshiftr,
   hb_p_bitshiftl,
   hb_p_largeframe,
   hb_p_pushwith,
   hb_p_pushlonglong,
   hb_p_pushstrhidden,
   hb_p_localnearsetstrhidden,
   hb_p_trybegin,
   hb_p_tryend,
   hb_p_tryrecover,
   hb_p_finally,
   hb_p_endfinally,
   hb_p_localnearadd,
   hb_p_arraypushref,
   hb_p_arraypopplus
};

void hb_compGenCRealCode( PFUNCTION pFunc, FILE * yyc )
{
   HB_LABEL_INFO label_info;

   /* Make sure that table is correct */
   assert( HB_P_LAST_PCODE == sizeof( s_verbose_table ) / sizeof( HB_GENC_FUNC_PTR ) );

   label_info.yyc = yyc;
   label_info.fVerbose = ( hb_comp_iGenCOutput == HB_COMPGENC_VERBOSE );
   label_info.fSetSeqBegin = FALSE;
   label_info.fCondJump = label_info.fEndProc = FALSE;
   label_info.iNestedBlock = label_info.iFinally = 0;

   if( pFunc->lPCodePos == 0 )
   {
      label_info.pulLabels = NULL;
   }
   else
   {
      label_info.pulLabels = ( ULONG * ) hb_xgrab( pFunc->lPCodePos * sizeof( ULONG ) );
      memset( label_info.pulLabels, 0, pFunc->lPCodePos * sizeof( ULONG ) );
      hb_compGenLabelTable( pFunc, &label_info );
   }

   fprintf( yyc, "{\n" );
   fprintf( yyc, "   ULONG ulPrivateBase = hb_memvarGetPrivatesBase();\n" );

   if( label_info.fCondJump )
   {
      fprintf( yyc, "   BOOL fValue;\n\n" );
   }

   if( pFunc->cScope & HB_FS_CRITICAL )
   {
      fprintf( yyc, "   HB_CRITICAL_LOCK( s_Critical%s );\n", pFunc->szName );
   }

   fprintf( yyc, "   do {\n" );

   hb_compPCodeEval( pFunc, ( HB_PCODE_FUNC_PTR * ) s_verbose_table, ( void * ) &label_info );

   fprintf( yyc, "   } while ( 0 );\n" );


   if( label_info.fEndProc )
   {
      fprintf( yyc, "labEND:\n" );
   }

   if( pFunc->cScope & HB_FS_CRITICAL )
   {
      fprintf( yyc, "   HB_CRITICAL_UNLOCK( s_Critical%s );\n", pFunc->szName );
   }

   fprintf( yyc, "   hb_xvmExitPorc( ulPrivateBase );\n" );
   fprintf( yyc, "}\n" );

   if( label_info.pulLabels )
   {
      hb_xfree( label_info.pulLabels );
   }
}
