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


#include "hbcomp.h"

static void hb_pp_ErrorGen( void * cargo,
                            const char * szMsgTable[], char cPrefix, int iErrorCode,
                            const char * szParam1, const char * szParam2 )
{
   int iLine = hb_comp_iLine;

   HB_SYMBOL_UNUSED( cargo );
   hb_comp_iLine = hb_pp_line( hb_comp_PP ) + 1;
   if( cPrefix == 'W' )
      hb_compGenWarning( szMsgTable, cPrefix, iErrorCode, szParam1, szParam2 );
   else
      hb_compGenError( szMsgTable, cPrefix, iErrorCode, szParam1, szParam2 );

   hb_comp_iLine = iLine;
}

static void hb_pp_PragmaDump( void * cargo, char * pBuffer, ULONG ulSize,
                              int iLine )
{
   PINLINE pInline;

   HB_SYMBOL_UNUSED( cargo );
   /* hb_comp_iLine = iLine + 1; */
   pInline                    = hb_compInlineAdd( NULL );
   pInline->pCode             = ( BYTE * ) hb_xgrab( ulSize + 1 );
   HB_MEMCPY( pInline->pCode, pBuffer, ulSize );
   pInline->pCode[ ulSize ]   = '\0';
   pInline->lPCodeSize        = ulSize;
   pInline->iLine             = iLine;
}

static void hb_pp_hb_inLine( void * cargo, char * szFunc,
                             char * pBuffer, ULONG ulSize, int iLine )
{
   HB_SYMBOL_UNUSED( cargo );
   /* hb_comp_iLine = iLine + 1; */
   if( hb_comp_iLanguage != LANG_C && hb_comp_iLanguage != LANG_OBJ_MODULE )
   {
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_REQUIRES_C, NULL, NULL );
   }
   else
   {
      PINLINE pInline = hb_compInlineAdd( hb_compIdentifierNew( szFunc, TRUE ) );
      pInline->pCode             = ( BYTE * ) hb_xgrab( ulSize + 1 );
      HB_MEMCPY( pInline->pCode, pBuffer, ulSize );
      pInline->pCode[ ulSize ]   = '\0';
      pInline->lPCodeSize        = ulSize;
      pInline->iLine             = iLine;
      hb_comp_cInlineID++;
   }
}

static BOOL hb_pp_CompilerSwitch( void * cargo, const char * szSwitch,
                                  int iValue, int * piPrevious, char cIndex[ 1 ] )
{
   BOOL     fError   = FALSE;
   HB_SIZE  i        = strlen( szSwitch );

   HB_SYMBOL_UNUSED( cargo );

   if( i > 1 && ( ( int ) ( szSwitch[ i - 1 ] - '0' ) ) == iValue )
      --i;

   if( i == 1 )
   {
      switch( szSwitch[ 0 ] )
      {
         case 'a':
         case 'A':
            if( piPrevious )
            {
               *piPrevious = ( int ) hb_comp_bAutoMemvarAssume;
               cIndex[ 0 ] = ( char ) HB_TOLOWER( szSwitch[ 0 ] );
            }

            hb_comp_bAutoMemvarAssume = iValue != 0;
            break;

         case 'b':
         case 'B':
            if( piPrevious )
            {
               *piPrevious = ( int ) hb_comp_bDebugInfo;
               cIndex[ 0 ] = ( char ) HB_TOLOWER( szSwitch[ 0 ] );
            }

            hb_comp_bDebugInfo = iValue != 0;
            break;

         case 'e':
            /* If adding such switch then MUST change hard coded cIndex[0] of "es" switch!!!
             */
            fError = TRUE;
            break;

         case 'g':
            /* If adding such switch then MUST change hard coded cIndex[0] of "TRACEPRAGMAS" switch!!!
             */
            fError = TRUE;
            break;

         case 'h':
            /* If adding such switch then MUST change hard coded cIndex[0] of "TEXHHIDDEN" switch!!!
             */
            fError = TRUE;
            break;

         case 'l':
         case 'L':
            if( piPrevious )
            {
               *piPrevious = ( int ) hb_comp_bLineNumbers;
               cIndex[ 0 ] = ( char ) HB_TOLOWER( szSwitch[ 0 ] );
            }

            hb_comp_bLineNumbers = iValue != 0;
            break;

         case 'n':
         case 'N':
            if( piPrevious )
            {
               *piPrevious = ( int ) hb_comp_bStartProc;
               cIndex[ 0 ] = ( char ) HB_TOLOWER( szSwitch[ 0 ] );
            }

            hb_comp_bStartProc = iValue != 0;
            break;

         case 'p':
         case 'P':
            if( piPrevious )
            {
               *piPrevious = ( int ) hb_comp_bPPO;
               cIndex[ 0 ] = ( char ) HB_TOLOWER( szSwitch[ 0 ] );
            }

            hb_comp_bPPO = iValue != 0;
            break;

         case 'q':
         case 'Q':
            if( piPrevious )
            {
               *piPrevious = ( int ) hb_comp_bQuiet;
               cIndex[ 0 ] = ( char ) HB_TOLOWER( szSwitch[ 0 ] );
            }

            hb_comp_bQuiet = iValue != 0;
            break;

         case 'r':
            /* If adding such switch then MUST change hard coded cIndex[0] of "RECURSELEVEL" switch!!!
             */
            fError = TRUE;
            break;

         case 't':
            /* If adding such switch then MUST change hard coded cIndex[0] of "pt" switch!!!
             */
            fError = TRUE;
            break;

         case 'v':
         case 'V':
            if( piPrevious )
            {
               *piPrevious = ( int ) hb_comp_bForceMemvars;
               cIndex[ 0 ] = ( char ) HB_TOLOWER( szSwitch[ 0 ] );
            }

            hb_comp_bForceMemvars = iValue != 0;
            break;

         case 'z':
         case 'Z':
            if( piPrevious )
            {
               *piPrevious = ( int ) hb_comp_bShortCuts;
               cIndex[ 0 ] = ( char ) HB_TOLOWER( szSwitch[ 0 ] );
            }

            hb_comp_bShortCuts = iValue == 0;
            break;

         case 'w':
         case 'W':
            if( piPrevious )
            {
               *piPrevious = ( int ) hb_comp_iWarnings;
               cIndex[ 0 ] = ( char ) HB_TOLOWER( szSwitch[ 0 ] );
            }

            if( iValue >= 0 && iValue <= 4 )
               hb_comp_iWarnings = iValue;
            else
               fError = TRUE;
            break;


         default:
            fError = TRUE;
      }
   }
   else if( i == 2 )
   {
      if( hb_strnicmp( szSwitch, "es", 2 ) == 0 &&
          ( iValue == HB_EXITLEVEL_DEFAULT ||
            iValue == HB_EXITLEVEL_SETEXIT ||
            iValue == HB_EXITLEVEL_DELTARGET ) )
      {
         if( piPrevious )
         {
            *piPrevious = ( int ) hb_comp_iExitLevel;
            cIndex[ 0 ] = 'e';
         }

         hb_comp_iExitLevel = iValue;
      }
      else if( hb_stricmp( szSwitch, "pt" ) == 0 )
      {
         if( piPrevious )
         {
            *piPrevious = ( int ) hb_comp_bTracePP;
            cIndex[ 0 ] = 't';
         }

         hb_comp_bTracePP = iValue != 0;
      }
      else
         fError = TRUE;
   }
   else if( i >= 4 && hb_strnicmp( szSwitch, "TEXTHIDDEN", i ) == 0 && iValue >= 0 && iValue <= 1 )
   {
      if( piPrevious )
      {
         *piPrevious = ( int ) hb_comp_iHidden;
         cIndex[ 0 ] = 'h';
      }

      hb_comp_iHidden = iValue;
   }
   else
      fError = TRUE;

   return fError;
}


void hb_compInitPP( int argc, char * argv[] )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_compInitPP()" ) );

   if( hb_comp_PP )
   {
      hb_pp_init( hb_comp_PP, hb_comp_bQuiet,
                  1024,
                  NULL, NULL, NULL,
                  hb_pp_ErrorGen, NULL, hb_pp_PragmaDump,
                  HB_COMP_ISSUPPORTED( HB_COMPFLAG_HB_INLINE ) ?
                  hb_pp_hb_inLine : NULL, hb_pp_CompilerSwitch );

      if( ! hb_pp_STD_CH )
      {
         hb_pp_setStdRules( hb_comp_PP );

         hb_pp_initDynDefines( hb_comp_PP );
      }
      else if( hb_pp_STD_CH[ 0 ] > ' ' )
      {
         if( hb_pp_STD_CH_ADDITIVE )
            hb_pp_setStdRules( hb_comp_PP );

         hb_pp_initDynDefines( hb_comp_PP );

         hb_pp_readRules( hb_comp_PP, hb_pp_STD_CH );

         hb_xfree( hb_pp_STD_CH );
         hb_pp_STD_CH = NULL;
      }
      else if( ! hb_comp_bQuiet )
      {
         hb_compOutStd( "Standard command definitions excluded.\n" );
         hb_pp_initDynDefines( hb_comp_PP );
      }

      /* Add /D and /undef: command line or envvar defines */
      hb_compChkDefines( argc, argv );

      /* mark current rules as standard ones */
      hb_pp_setStdBase( hb_comp_PP );
   }
}
