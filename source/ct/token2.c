/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 string functions
 *     - TOKENINIT()
 *     - TOKENEXIT()
 *     - TOKENNEXT()
 *     - TOKENNUM()
 *     - TOKENAT()
 *     - SAVETOKEN()
 *     - RESTTOKEN()
 *     - TOKENEND()
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
 *
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

#include "ct.h"

/* ==================================================================== */
/* static functions for token environment management                    */
/* ==================================================================== */

#define TOKEN_ENVIRONMENT_STEP 100

typedef struct _TOKEN_POSITION
{
   HB_SIZE sStartPos; /* relative 0-based index of first char of token */
   HB_SIZE sEndPos;   /* relative 0-based index of first char BEHIND token,
                        so that length = sEndPos-sStartPos */
} TOKEN_POSITION;

typedef TOKEN_POSITION * TOKEN_ENVIRONMENT;

/* -------------------------------------------------------------------- */
/* alloc new token environment                                          */
/* -------------------------------------------------------------------- */
static TOKEN_ENVIRONMENT sTokEnvNew( void )
{

   TOKEN_ENVIRONMENT env = ( TOKEN_ENVIRONMENT ) hb_xalloc( sizeof( TOKEN_POSITION ) * ( 2 + TOKEN_ENVIRONMENT_STEP ) );

   if( env == NULL )
   {
      return NULL;
   }

   /* use the first element to store current length and use of token env */
   env[ 0 ].sStartPos   = 0;     /* 0-based index to next free, unused element */
   env[ 0 ].sEndPos     = 100;   /* but there are 100 elements ready for use */

   /* use second element to store actual index with tokennext() */
   env[ 1 ].sStartPos   = 0; /* 0-based index value that is to be used NEXT */

   return env;

}

/* -------------------------------------------------------------------- */
/* add a tokenizing position to a token environment                     */
/* -------------------------------------------------------------------- */

static int sTokEnvAddPos( TOKEN_ENVIRONMENT * penv, TOKEN_POSITION * pPos )
{
   HB_SIZE                 index;
   TOKEN_ENVIRONMENT env = *penv;

   /* new memory needed ? */
   if( env[ 0 ].sStartPos == env[ 0 ].sEndPos )
   {
      *penv = ( TOKEN_ENVIRONMENT ) hb_xrealloc( env,
                                                 sizeof( TOKEN_POSITION ) *
                                                 ( 2 + env[ 0 ].sEndPos + TOKEN_ENVIRONMENT_STEP ) );
      env   = *penv;
      if( env == NULL )
      {
         return 0;
      }

      env[ 0 ].sEndPos += TOKEN_ENVIRONMENT_STEP;
   }

   index                   = env[ 0 ].sStartPos + 2; /* +2  because of extra elements */
   env[ index ].sStartPos  = pPos->sStartPos;
   env[ index ].sEndPos    = pPos->sEndPos;
   env[ 0 ].sStartPos++;

   return 1;

}

/* -------------------------------------------------------------------- */
/* check to see if token pointer is at end of environment               */
/* -------------------------------------------------------------------- */

static int sTokEnvEnd( TOKEN_ENVIRONMENT env )
{
   return env[ 1 ].sStartPos >= env[ 0 ].sStartPos;
}

/* -------------------------------------------------------------------- */
/* get size of token environment in memory                              */
/* -------------------------------------------------------------------- */

static HB_SIZE sTokEnvGetSize( TOKEN_ENVIRONMENT env )
{

   return sizeof( TOKEN_POSITION ) *
          ( 2 + env[ 0 ].sEndPos );

}

/* -------------------------------------------------------------------- */
/* get position element pointed to by tokenizing pointer                */
/* -------------------------------------------------------------------- */

static TOKEN_POSITION * sTokEnvGetPos( TOKEN_ENVIRONMENT env )
{

   if( env[ 1 ].sStartPos >= env[ 0 ].sStartPos )
   {
      return NULL;
   }

   return env + 2 + ( env[ 1 ].sStartPos ); /* "+2" because of extra elements */

}

/* -------------------------------------------------------------------- */
/* get position element pointed to by given 0-based index               */
/* -------------------------------------------------------------------- */

static TOKEN_POSITION * sTokEnvGetPosIndex( TOKEN_ENVIRONMENT env, HB_SIZE index )
{
   if( index >= env[ 0 ].sStartPos )
   {
      return NULL;
   }

   return env + 2 + index; /* "+2" because of extra elements */
}

/* -------------------------------------------------------------------- */
/* increment tokenizing pointer by one                                  */
/* -------------------------------------------------------------------- */

static int sTokEnvIncPtr( TOKEN_ENVIRONMENT env )
{

   if( env[ 1 ].sStartPos >= env[ 0 ].sStartPos )
   {
      return 0;
   }
   else
   {
      env[ 1 ].sStartPos++;
      return 1;
   }

}

/* -------------------------------------------------------------------- */
/* set tokenizing pointer to 0-based value                              */
/* -------------------------------------------------------------------- */

static int sTokEnvSetPtr( TOKEN_ENVIRONMENT env, size_t sCnt )
{

   if( sCnt >= env[ 0 ].sStartPos )
   {
      return 0;
   }
   else
   {
      env[ 1 ].sStartPos = sCnt;
      return 1;
   }

}

/* -------------------------------------------------------------------- */
/* decrement tokenizing pointer by one                                  */
/* -------------------------------------------------------------------- */

/* sTokEnvDecPtr currently not used ! */
/* static int sTokEnvDecPtr (TOKEN_ENVIRONMENT env)
   {

   if (env[1].sStartPos <= 0)
   {
    return (0);
   }
   else
   {
    env[1].sStartPos--;
    return (1);
   }

   }  */

/* -------------------------------------------------------------------- */
/* get value of tokenizing pointer                                      */
/* -------------------------------------------------------------------- */

static HB_SIZE sTokEnvGetPtr( TOKEN_ENVIRONMENT env )
{
   return env[ 1 ].sStartPos;
}

/* -------------------------------------------------------------------- */
/* get token count                                                      */
/* -------------------------------------------------------------------- */

static HB_SIZE sTokEnvGetCnt( TOKEN_ENVIRONMENT env )
{
   return env[ 0 ].sStartPos;
}

/* -------------------------------------------------------------------- */
/* free token environment                                               */
/* -------------------------------------------------------------------- */

static void sTokEnvDel( TOKEN_ENVIRONMENT env )
{
   hb_xfree( env );
}

/* ==================================================================== */
/* HARBOUR functions                                                    */
/* ==================================================================== */

/* static data */
/* TODO: make thread safe */
static const char *      spcSeparatorStr      = "\x00" "\x09" "\x0A" "\x0C" "\x1A" "\x20" "\x8A" "\x8C" ",.;:!\?/\\<>()#&%+-*";
static const HB_SIZE     ssSeparatorStrLen    = 26;
static TOKEN_ENVIRONMENT ssTokenEnvironment   = NULL;

HB_FUNC( TOKENINIT )
{
   if( ISCHAR( 1 ) )
   {
      const char *      pcString = hb_parc( 1 );
      HB_SIZE           sStrLen  = hb_parclen( 1 );
      const char *      pcSeparatorStr;
      HB_SIZE           sSeparatorStrLen;
      ULONG             ulSkipCnt, ulSkip;

      const char *      pcSubStr, * pc;
      HB_SIZE           sSubStrLen;

      TOKEN_ENVIRONMENT sTokenEnvironment;
      TOKEN_POSITION    sTokenPosition;

      /* separator string */
      if( ISCHAR( 2 ) && ( ( sSeparatorStrLen = hb_parclen( 2 ) ) != 0 ) )
      {
         pcSeparatorStr = hb_parc( 2 );
      }
      else
      {
         pcSeparatorStr    = ( char * ) spcSeparatorStr;
         sSeparatorStrLen  = ssSeparatorStrLen;
      }

      /* skip width */
      if( ISNUM( 3 ) )
      {
         ulSkip = hb_parnl( 3 );
      }
      else
      {
         ulSkip = HB_MKULONG( 255, 255, 255, 255 );
      }
      if( ulSkip == 0 )
      {
         ulSkip = HB_MKULONG( 255, 255, 255, 255 );
      }

      /* allocate new token environment */
      if( ( sTokenEnvironment = sTokEnvNew() ) == NULL )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
         {
            ct_error( ( USHORT ) iArgErrorMode, EG_MEM, CT_ERROR_TOKENINIT,
                      NULL, "TOKENINIT", 0, EF_CANDEFAULT, 4,
                      hb_paramError( 1 ), hb_paramError( 2 ),
                      hb_paramError( 3 ), hb_paramError( 4 ) );
         }

         hb_retl( 0 );
         return;
      }

      pcSubStr    = pcString;
      sSubStrLen  = sStrLen;

      /* scan start condition */
      pc          = pcSubStr - 1;

      while( 1 )
      {
         HB_SIZE sMatchedPos = sSeparatorStrLen;

         /* ulSkip */
         ulSkipCnt = 0;

         do
         {
            sSubStrLen  -= ( pc - pcSubStr ) + 1;
            pcSubStr    = pc + 1;
            pc          = ct_at_charset_forward( pcSubStr, sSubStrLen,
                                                 pcSeparatorStr, sSeparatorStrLen,
                                                 &sMatchedPos );
            ulSkipCnt++;
         }
         while( ( ulSkipCnt < ulSkip ) && ( pc == pcSubStr ) );

         if( sSubStrLen == 0 )
            break;

         sTokenPosition.sStartPos = pcSubStr - pcString;
         if( pc == NULL )
         {
            sTokenPosition.sEndPos = pcSubStr - pcString + sSubStrLen;
         }
         else
         {
            sTokenPosition.sEndPos = pc - pcString;
         }

         if( ! sTokEnvAddPos( &sTokenEnvironment, &sTokenPosition ) )
         {
            int iArgErrorMode = ct_getargerrormode();
            if( iArgErrorMode != CT_ARGERR_IGNORE )
            {
               ct_error( ( USHORT ) iArgErrorMode, EG_MEM, CT_ERROR_TOKENINIT,
                         NULL, "TOKENINIT", 0, EF_CANDEFAULT, 4,
                         hb_paramError( 1 ), hb_paramError( 2 ),
                         hb_paramError( 3 ), hb_paramError( 4 ) );
            }

            sTokEnvDel( sTokenEnvironment );
            hb_retl( 0 );
            return;
         }

         if( pc == NULL )
            break;

      } /* while (1); */

      /* save token environment to 4th parameter OR to the static */
      if( ISBYREF( 4 ) )
      {
         hb_storclen( ( char * ) sTokenEnvironment,
                      sTokEnvGetSize( sTokenEnvironment ), 4 );
         sTokEnvDel( sTokenEnvironment );
      }
      else
      {
         if( ssTokenEnvironment != NULL )
            sTokEnvDel( ssTokenEnvironment );
         ssTokenEnvironment = sTokenEnvironment;
      }

      hb_retl( 1 );

   }
   else /* ISCHAR (1) */
   {
      /* if there is a token environment stored in either the 4th parameter or
         in the static variable -> rewind to first token */
      TOKEN_ENVIRONMENT sTokenEnvironment;

      if( ISCHAR( 4 ) && ISBYREF( 4 ) )
      {
         sTokenEnvironment = ( TOKEN_ENVIRONMENT ) hb_parc( 4 );
      }
      else
      {
         sTokenEnvironment = ssTokenEnvironment;
      }

      if( sTokenEnvironment != NULL )
      {
         /* rewind to first token */
         hb_retl( sTokEnvSetPtr( sTokenEnvironment, 0 ) );
         if( ISCHAR( 4 ) && ISBYREF( 4 ) )
         {
            hb_storclen( ( char * ) sTokenEnvironment, sTokEnvGetSize( sTokenEnvironment ), 4 );
         }
      }
      else
      {
         /* nothing to rewind -> return .f. */
         PHB_ITEM pSubst         = NULL;
         int      iArgErrorMode  = ct_getargerrormode();
         if( iArgErrorMode != CT_ARGERR_IGNORE )
         {
            pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG,
                                     CT_ERROR_TOKENINIT, NULL, "TOKENINIT",
                                     0, EF_CANSUBSTITUTE, 4,
                                     hb_paramError( 1 ), hb_paramError( 2 ),
                                     hb_paramError( 3 ), hb_paramError( 4 ) );
         }

         if( pSubst != NULL )
         {
            hb_itemRelease( hb_itemReturnForward( pSubst ) );
         }
         else
         {
            hb_retl( 0 );
         }
      }

   }
}

HB_FUNC( TOKENNEXT )
{
   if( ISCHAR( 1 ) )
   {
      const char * pcString = hb_parc( 1 );
      HB_SIZE      sStrLen  = hb_parclen( 1 );

      TOKEN_ENVIRONMENT sTokenEnvironment;
      TOKEN_POSITION *  psTokenPosition;

      /* token environment by parameter ... */
      if( ISCHAR( 3 ) && ISBYREF( 3 ) )
      {
         size_t sStrLen3 = ( size_t ) hb_parclen( 3 );

         if( sStrLen3 < sizeof( TOKEN_POSITION ) * 2 )
         {
            int iArgErrorMode = ct_getargerrormode();
            if( iArgErrorMode != CT_ARGERR_IGNORE )
            {
               ct_error( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_TOKENNEXT,
                         NULL, "TOKENNEXT", 0, EF_CANDEFAULT, 3,
                         hb_paramError( 1 ), hb_paramError( 2 ),
                         hb_paramError( 3 ) );
            }

            hb_retc( "" );
            return;
         }
         sTokenEnvironment = ( TOKEN_ENVIRONMENT ) hb_xgrab( sStrLen3 );
         hb_xmemcpy( ( char * ) sTokenEnvironment, hb_parc( 3 ), sStrLen3 );

      }
      else
      {
         /* ... or static  ? */
         if( ssTokenEnvironment == NULL )
         {
            int iArgErrorMode = ct_getargerrormode();
            if( iArgErrorMode != CT_ARGERR_IGNORE )
            {
               ct_error( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_TOKENNEXT,
                         NULL, "TOKENNEXT", 0, EF_CANDEFAULT, 3,
                         hb_paramError( 1 ), hb_paramError( 2 ),
                         hb_paramError( 3 ) );
            }

            hb_retc( "" );
            return;
         }
         sTokenEnvironment = ssTokenEnvironment;

      }

      /* nth token or next token ?  */
      if( ISNUM( 2 ) )
      {
         psTokenPosition = sTokEnvGetPosIndex( sTokenEnvironment,
                                               hb_parnl( 2 ) - 1 );
         /* no increment here */
      }
      else
      {
         psTokenPosition = sTokEnvGetPos( sTokenEnvironment );
         /* increment counter */
         sTokEnvIncPtr( sTokenEnvironment );
      }

      if( ( psTokenPosition == NULL ) ||
          ( sStrLen <= psTokenPosition->sStartPos ) )
      {
         int iArgErrorMode = ct_getargerrormode();
         if( iArgErrorMode != CT_ARGERR_IGNORE )
         {
            ct_error( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_TOKENNEXT,
                      NULL, "TOKENNEXT", 0, EF_CANDEFAULT, 3,
                      hb_paramError( 1 ), hb_paramError( 2 ),
                      hb_paramError( 3 ) );
         }

         if( ISCHAR( 3 ) && ISBYREF( 3 ) )
         {
            hb_storclen( ( char * ) sTokenEnvironment, sTokEnvGetSize( sTokenEnvironment ), 3 );
            hb_xfree( ( char * ) sTokenEnvironment );
         }
         hb_retc( "" );
         return;
      }

      if( sStrLen < psTokenPosition->sEndPos )
      {
         hb_retclen( pcString + psTokenPosition->sStartPos,
                     sStrLen - ( psTokenPosition->sStartPos ) );
      }
      else
      {
         hb_retclen( pcString + psTokenPosition->sStartPos,
                     ( psTokenPosition->sEndPos ) - ( psTokenPosition->sStartPos ) );
      }

      if( ISCHAR( 3 ) && ISBYREF( 3 ) )
      {
         hb_storclen( ( char * ) sTokenEnvironment, sTokEnvGetSize( sTokenEnvironment ), 3 );
         hb_xfree( ( char * ) sTokenEnvironment );
      }

   }
   else
   {
      /* no string given, no token returns */
      PHB_ITEM pSubst         = NULL;
      int      iArgErrorMode  = ct_getargerrormode();
      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_TOKENNEXT, NULL, "TOKENNEXT",
                                  0, EF_CANSUBSTITUTE, 3,
                                  hb_paramError( 1 ), hb_paramError( 2 ),
                                  hb_paramError( 3 ) );
      }

      if( pSubst != NULL )
      {
         hb_itemRelease( hb_itemReturnForward( pSubst ) );
      }
      else
      {
         hb_retc( "" );
      }
   }
}

HB_FUNC( TOKENNUM )
{
   TOKEN_ENVIRONMENT sTokenEnvironment;

   if( ISCHAR( 1 ) && ISBYREF( 1 ) )
   {
      sTokenEnvironment = ( TOKEN_ENVIRONMENT ) hb_parc( 1 );
   }
   else
   {
      sTokenEnvironment = ssTokenEnvironment;
   }

   if( ( void * ) sTokenEnvironment != NULL )
   {
      hb_retnl( ( LONG ) sTokEnvGetCnt( sTokenEnvironment ) );
   }
   else
   {
      PHB_ITEM pSubst         = NULL;
      int      iArgErrorMode  = ct_getargerrormode();
      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_TOKENNUM, NULL, "TOKENNUM",
                                  0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
      }

      if( pSubst != NULL )
      {
         hb_itemRelease( hb_itemReturnForward( pSubst ) );
      }
      else
      {
         hb_retnl( 0 );
      }
   }
}

HB_FUNC( TOKENEND )
{
   TOKEN_ENVIRONMENT sTokenEnvironment;

   if( ISCHAR( 1 ) && ISBYREF( 1 ) )
      sTokenEnvironment = ( TOKEN_ENVIRONMENT ) hb_parc( 1 );
   else
      sTokenEnvironment = ssTokenEnvironment;

   if( ( void * ) sTokenEnvironment != NULL )
   {
      hb_retl( sTokEnvEnd( sTokenEnvironment ) );
   }
   else
   {
      PHB_ITEM pSubst         = NULL;
      int      iArgErrorMode  = ct_getargerrormode();
      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_TOKENEND, NULL, "TOKENEND",
                                  0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
      }

      if( pSubst != NULL )
      {
         hb_itemRelease( hb_itemReturnForward( pSubst ) );
      }
      else
      {
         /* it is CTIII behaviour to return .T. if there's no string TOKENINIT'ed */
         hb_retl( 1 );
      }
   }
}

HB_FUNC( TOKENEXIT )
{
   if( ssTokenEnvironment != NULL )
   {
      sTokEnvDel( ssTokenEnvironment );
      ssTokenEnvironment = NULL;
      hb_retl( 1 );
   }
   else
   {
      hb_retl( 0 );
   }
}

HB_FUNC( TOKENAT )
{
   int               iSeparatorPos = 0;
   HB_SIZE           sCurrentIndex;
   TOKEN_ENVIRONMENT sTokenEnvironment;
   TOKEN_POSITION *  psTokenPosition;

   if( ISLOG( 1 ) )
      iSeparatorPos = hb_parl( 1 );

   if( ISCHAR( 3 ) && ISBYREF( 3 ) )
      sTokenEnvironment = ( TOKEN_ENVIRONMENT ) hb_parc( 3 );
   else
      sTokenEnvironment = ssTokenEnvironment;

   if( ( void * ) sTokenEnvironment == NULL )
   {
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         ct_error( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_TOKENAT,
                   NULL, "TOKENAT", 0, EF_CANDEFAULT, 3,
                   hb_paramError( 1 ), hb_paramError( 2 ),
                   hb_paramError( 3 ) );
      }

      hb_retnl( 0 );
      return;
   }

   if( ISNUM( 2 ) )
      sCurrentIndex = hb_parnl( 2 ) - 1;
   else
      sCurrentIndex = sTokEnvGetPtr( sTokenEnvironment );

   psTokenPosition = sTokEnvGetPosIndex( sTokenEnvironment, sCurrentIndex );

   if( psTokenPosition == NULL )
   {
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         ct_error( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_TOKENAT,
                   NULL, "TOKENAT", 0, EF_CANDEFAULT, 3,
                   hb_paramError( 1 ), hb_paramError( 2 ),
                   hb_paramError( 3 ) );
      }

      hb_retnl( 0 );
      return;
   }

   if( iSeparatorPos )
      hb_retnl( ( LONG ) psTokenPosition->sEndPos + 1 );
   else
      hb_retnl( ( LONG ) psTokenPosition->sStartPos + 1 );
}

HB_FUNC( SAVETOKEN )
{
   if( ssTokenEnvironment != NULL )
   {
      hb_retclen( ( char * ) ssTokenEnvironment, sTokEnvGetSize( ssTokenEnvironment ) );
   }
   else
   {
      hb_retc( "" );
   }
}

HB_FUNC( RESTTOKEN )
{
   if( ISCHAR( 1 ) )
   {
      const char * pcString = hb_parc( 1 );
      HB_SIZE      sStrLen  = hb_parclen( 1 );

      TOKEN_ENVIRONMENT sTokenEnvironment;

      if( sStrLen != 0 )
      {
         /* alloc memory for new environment */
         sTokenEnvironment = ( TOKEN_ENVIRONMENT ) hb_xalloc( sStrLen );
         if( sTokenEnvironment == NULL )
         {
            int iArgErrorMode = ct_getargerrormode();
            if( iArgErrorMode != CT_ARGERR_IGNORE )
            {
               ct_error( ( USHORT ) iArgErrorMode, EG_MEM, CT_ERROR_RESTTOKEN,
                         NULL, "RESTTOKEN", 0, EF_CANDEFAULT, 1,
                         hb_paramError( 1 ) );
            }
            hb_retc( "" );
            return;
         }

         hb_xmemcpy( sTokenEnvironment, pcString, (size_t) sStrLen );
      }
      else
      {
         /* restored env has length 0 */
         sTokenEnvironment = NULL;
      }

      /* return current environment, then delete it */
      if( ssTokenEnvironment != NULL )
      {
         hb_retclen( ( char * ) ssTokenEnvironment, sTokEnvGetSize( ssTokenEnvironment ) );
         sTokEnvDel( ssTokenEnvironment );
      }
      else
      {
         hb_retc( "" );
      }

      ssTokenEnvironment = sTokenEnvironment;

   }
   else
   {
      PHB_ITEM pSubst         = NULL;
      int      iArgErrorMode  = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_RESTTOKEN, NULL, "RESTTOKEN",
                                  0, EF_CANSUBSTITUTE, 1, hb_paramError( 1 ) );
      }

      if( pSubst != NULL )
      {
         hb_itemRelease( hb_itemReturnForward( pSubst ) );
      }
      else
      {
         hb_retc( "" );
      }
   }
}

