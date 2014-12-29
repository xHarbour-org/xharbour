/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CHR(), ASC() functions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#include <ctype.h>

#include "hbapi.h"
#include "hbfast.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbstack.h"

/* converts an ASCII code to a character value */
HB_FUNC( CHR )
{
   if( hb_param( 1, HB_IT_NUMERIC ) )
   {
      /* NOTE: CA-Cl*pper's compiler optimizer will be wrong for those
               CHR() cases where the passed parameter is a constant which
               can be divided by 256 but it's not zero, in this case it
               will return an empty string instead of a Chr(0). [vszakats] */

      hb_retclenStatic( ( char * ) hb_szAscii[ ( UCHAR ) hb_parni( 1 ) ], 1 );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1104, NULL, "CHR", 1, hb_paramError( 1 ) );
   }
}

/* converts a character value to an ASCII code */
HB_FUNC( ASC )
{
   const char * szValue = hb_parc( 1 ) ;   

   if( szValue )
   {
      hb_retni( ( hb_parclen( 1 ) > 0 ) ? ( BYTE ) *szValue : 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1107, NULL, "ASC", 1, hb_paramError( 1 ) );
}

