/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 string functions
 *
 *   - AFTERATNUM()
 *   - BEFORATNUM()
 *   - ATNUM()
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

#define DO_ATNUM_AFTERATNUM   0
#define DO_ATNUM_BEFORATNUM   1
#define DO_ATNUM_ATNUM        2

/* helper function */
static void do_atnum( int iSwitch )
{
   if( ( ISCHAR( 1 ) ) && ( ISCHAR( 2 ) ) )
   {
      const char *   pcStringToMatch   = hb_parc( 1 );
      HB_SIZE        sStrToMatchLen    = hb_parclen( 1 );
      const char *   pcString          = hb_parc( 2 );
      HB_SIZE        sStrLen           = hb_parclen( 2 );
      int            iMultiPass        = ct_getatmupa();
      int            iAtLike           = ct_getatlike();
      char           cAtLike           = ct_getatlikechar();
      HB_SIZE        sIgnore, sMatchStrLen = 0;
      ULONG          ulCounter;
      const char *   pc                = NULL;

      /* eventually ignore some characters */
      if( ISNUM( 4 ) )
         sIgnore = ( size_t ) hb_parnl( 4 );
      else
         sIgnore = 0;

      if( sIgnore >= sStrLen )
      {
         switch( iSwitch )
         {
            case DO_ATNUM_AFTERATNUM:
            {
               /* AFTERATNUM */
               int iArgErrorMode = ct_getargerrormode();
               if( iArgErrorMode != CT_ARGERR_IGNORE )
               {
                  ct_error( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_AFTERATNUM,
                            NULL, "AFTERATNUM", 0, EF_CANDEFAULT, 4,
                            hb_paramError( 1 ), hb_paramError( 2 ),
                            hb_paramError( 3 ), hb_paramError( 4 ) );
               }
               ;
               hb_retc( "" );
            }; break;

            case DO_ATNUM_BEFORATNUM:
            {
               /* BEFORATNUM */
               int iArgErrorMode = ct_getargerrormode();
               if( iArgErrorMode != CT_ARGERR_IGNORE )
               {
                  ct_error( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_BEFORATNUM,
                            NULL, "BEFORATNUM", 0, EF_CANDEFAULT, 4,
                            hb_paramError( 1 ), hb_paramError( 2 ),
                            hb_paramError( 3 ), hb_paramError( 4 ) );
               }
               ;
               hb_retc( "" );
            }; break;

            case DO_ATNUM_ATNUM:
            {
               /* ATNUM */
               int iArgErrorMode = ct_getargerrormode();
               if( iArgErrorMode != CT_ARGERR_IGNORE )
               {
                  ct_error( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ATNUM,
                            NULL, "ATNUM", 0, EF_CANDEFAULT, 4,
                            hb_paramError( 1 ), hb_paramError( 2 ),
                            hb_paramError( 3 ), hb_paramError( 4 ) );
               }
               ;
               hb_retnl( 0 );
            }; break;
         }

         return;
      }
      else
      {
         pcString += sIgnore;
         sStrLen  -= sIgnore;
      }

      /* nth match or last match ? */
      if( ISNUM( 3 ) && ( ( ulCounter = hb_parnl( 3 ) ) != 0 ) )
      {
         /* find the <ulCounter>th match */
         const char *   pcSubStr;
         HB_SIZE        sSubStrLen;
         ULONG          ulMatchCounter = 0;

         pcSubStr    = pcString;
         sSubStrLen  = sStrLen;

         while( ulMatchCounter < ulCounter )
         {
            switch( iAtLike )
            {
               case CT_SETATLIKE_EXACT:
               {
                  pc = ct_at_exact_forward( pcSubStr, sSubStrLen,
                                            pcStringToMatch, sStrToMatchLen,
                                            &sMatchStrLen );
               }; break;

               case CT_SETATLIKE_WILDCARD:
               {
                  pc = ct_at_wildcard_forward( pcSubStr, sSubStrLen,
                                               pcStringToMatch, sStrToMatchLen,
                                               cAtLike, &sMatchStrLen );
               }; break;

               default:
               {
                  pc = NULL;
               };
            }

            if( pc == NULL )
            {
               /* no match found; if this happens at this point,
                  there are no <ulCounter> matches, so return an empty string */
               switch( iSwitch )
               {
                  case DO_ATNUM_AFTERATNUM:
                  case DO_ATNUM_BEFORATNUM:
                  {
                     /* AFTERATNUM */
                     /* BEFORATNUM */
                     hb_retc( "" );
                  }; break;

                  case DO_ATNUM_ATNUM:
                  {
                     /* ATNUM */
                     hb_retnl( 0 );
                  }; break;
               }

               return;
            }

            ulMatchCounter++;

            if( iMultiPass )
               pcSubStr = pc + 1;
            else
               pcSubStr = pc + sMatchStrLen;
            sSubStrLen = sStrLen - ( pcSubStr - pcString );
         }

      }
      else /* (ISNUM (3) && ((ulCounter = hb_parnl (3)) != 0) */
      {
         /* we have to find the last match and return the
            string after that last match */

         switch( iAtLike )
         {
            case CT_SETATLIKE_EXACT:
            {
               pc = ct_at_exact_backward( pcString, sStrLen,
                                          pcStringToMatch, sStrToMatchLen,
                                          &sMatchStrLen );
            }; break;

            case CT_SETATLIKE_WILDCARD:
            {
               pc = ct_at_wildcard_backward( pcString, sStrLen,
                                             pcStringToMatch, sStrToMatchLen,
                                             cAtLike, &sMatchStrLen );
            }; break;

            default:
            {
               pc = NULL;
            };
         }

         if( pc == NULL )
         {
            /* no matches found */
            switch( iSwitch )
            {
               case DO_ATNUM_AFTERATNUM:
               case DO_ATNUM_BEFORATNUM:
               {
                  /* AFTERATNUM */
                  /* BEFORATNUM */
                  hb_retc( "" );
               }; break;

               case DO_ATNUM_ATNUM:
               {
                  /* ATNUM */
                  hb_retnl( 0 );
               }; break;
            }

            return;
         }

      }

      switch( iSwitch )
      {
         case DO_ATNUM_AFTERATNUM:
         {
            /* AFTERATNUM */
            if( pc + sMatchStrLen >= pcString + sStrLen )
               hb_retc( "" );
            else
               hb_retclen( pc + sMatchStrLen, sStrLen - ( pc + sMatchStrLen - pcString ) );
         }; break;

         case DO_ATNUM_BEFORATNUM:
         {
            /* BEFORATNUM */
            hb_retclen( pcString - sIgnore, pc - ( pcString - sIgnore ) );
         }; break;

         case DO_ATNUM_ATNUM:
         {
            /* ATNUM */
            hb_retns( pc - ( pcString - sIgnore ) + 1 );
         }; break;
      }

   }
   else /* ((ISCHAR (1)) && (ISCHAR (2))) */
   {
      switch( iSwitch )
      {
         case DO_ATNUM_AFTERATNUM:
         case DO_ATNUM_BEFORATNUM:
         {
            /* AFTERATNUM */
            PHB_ITEM pSubst         = NULL;
            int      iArgErrorMode  = ct_getargerrormode();
            if( iArgErrorMode != CT_ARGERR_IGNORE )
            {
               pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG,
                                        ( iSwitch == DO_ATNUM_AFTERATNUM ? CT_ERROR_AFTERATNUM : CT_ERROR_BEFORATNUM ),
                                        NULL,
                                        ( iSwitch == DO_ATNUM_AFTERATNUM ? "AFTERATNUM" : "BEFORATNUM" ),
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
               hb_retc( "" );
            }
         }; break;

         case DO_ATNUM_ATNUM:
         {
            /* ATNUM */
            PHB_ITEM pSubst         = NULL;
            int      iArgErrorMode  = ct_getargerrormode();
            if( iArgErrorMode != CT_ARGERR_IGNORE )
            {
               pSubst = ct_error_subst( ( USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ATNUM,
                                        NULL, "ATNUM", 0, EF_CANSUBSTITUTE, 4,
                                        hb_paramError( 1 ), hb_paramError( 2 ),
                                        hb_paramError( 3 ), hb_paramError( 4 ) );
            }

            if( pSubst != NULL )
            {
               hb_itemRelease( hb_itemReturnForward( pSubst ) );
            }
            else
            {
               hb_retnl( 0 );
            }
         }; break;
      }
   }
}

HB_FUNC( AFTERATNUM )
{
   do_atnum( DO_ATNUM_AFTERATNUM );
}

HB_FUNC( BEFORATNUM )
{
   do_atnum( DO_ATNUM_BEFORATNUM );
}

HB_FUNC( ATNUM )
{
   do_atnum( DO_ATNUM_ATNUM );
}

