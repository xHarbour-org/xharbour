/*
 * $Id: val.c,v 1.4 2003/05/30 06:49:32 ronpinkas Exp $
 */

/*
 * Harbour Project source code:
 * VAL() function
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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

#include <math.h>
#include <ctype.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* returns the numeric value of a character string representation of a number */
/*
  ... to remain compatible with Harbour version which utilizes a 2nd paramater ulLen
 */
double HB_EXPORT hb_strVal( const char * szText, ... )
{
   double dResult;
   char *pCopy = NULL;
   ULONG ulPad = 0, ulLen;

   HB_TRACE(HB_TR_DEBUG, ("hb_strVal(%s, %d)", szText));

   while( isspace( szText[ulPad] ) )
   {
      ulPad++;
   }

   if( szText[ulPad] == '-' || szText[ulPad] == '+' )
   {
      ulPad++;
   }

   ulLen = ulPad;

   while( szText[ulLen] )
   {
      if( ! ( isdigit( szText[ulLen] ) || szText[ulLen] == '.' ) )
      {
         if( ulLen > ulPad )
         {
            pCopy = (char *) hb_xgrab( ulLen+1 );
            strncpy( pCopy, szText, ulLen );
            pCopy[ulLen] = '\0';
            szText = pCopy;
            break;
         }
         else
         {
            return 0.0;
         }
      }

      ulLen++;
   }

   dResult = atof( szText );

   // Maybe -0.00
   if( dResult == -0.00 )
   {
      dResult = 0.0;
   }

   //printf( "String: >%s< Val:%f\n", szText, dResult );

   if( pCopy )
   {
      hb_xfree( (void *) pCopy );
   }

   return dResult;
}

/* returns the numeric value of a character string representation of a number  */
HB_FUNC( VAL )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
   {
      char * szText = hb_itemGetCPtr( pText );
      int iWidth, iLen = ( int ) hb_itemGetCLen( pText );
      int iDec;
      double dValue = hb_strVal( szText, hb_itemGetCLen( pText ) );

      for( iWidth = 0; iWidth < iLen; iWidth++ )
      {
         if( szText[ iWidth ] == '.' )
         {
            break;
         }
      }

      if( iWidth >= iLen - 1 )
      {
         hb_retnlen( dValue, iLen, 0 );
      }
      else
      {
         iDec = iLen - iWidth - 1;

         if( iWidth == 0 )
         {
            iWidth++;
         }
         else if( iWidth == 1 && szText[ 0 ] == '-' /*&& dValue != 0.0*/ )
         {
            iWidth++;
         }

         hb_retnlen( dValue, iWidth, iDec );
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1098, NULL, "VAL", 1, hb_paramError( 1 ) );
   }
}
