/*
 * $Id: readline.c,v 1.0 2003/11/27 13:27:21 lf_sfnet Exp $
 */

/*
 * xHarbour Project source code:
 * Text file reading functions
 *
 * Copyright 2003 Marcelo Lombardo - lombardo@uol.com.br
 * http://www.xharbour.org
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
#include "hbapifs.h"
#include "hb_io.h"
#include "hbset.h"
#include "hbapiitm.h"
#include "hbapierr.h"

USHORT HB_EXPORT hb_fsReadLine( FHANDLE hFileHandle, BYTE * pBuff, USHORT uiMaxLineLen, char ** Proto, int * iprotosize, USHORT iprotos  )
{
   USHORT protos, read, iPos, iPosProto;
   BOOL bProtoFound;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsReadLine(%p, %p, %hu, %p, %p, %hu)", hFileHandle, pBuff, uiMaxLineLen, Proto, iprotosize, iprotos ));

   /* read from file */
   read = hb_fsRead( hFileHandle, pBuff, uiMaxLineLen );

   /* scan the read buffer */

   if (read>0)
   {
      for( iPos=0; iPos<read; iPos++ )
      {
         bProtoFound = 0;
         for( protos=0;protos < iprotos;protos++)
         {
            /* Compare with the LAST char in every terminator */
            if( pBuff[iPos] == Proto[protos][iprotosize[protos]-1] && (iprotosize[protos]-1) <= iPos )
            {
               bProtoFound = 1;
               for(iPosProto=0; iPosProto < (iprotosize[protos]-1); iPosProto++)
               {
                  if(Proto[protos][iPosProto] != pBuff[ (iPos-iprotosize[protos])+iPosProto+1 ])
                  {
                     bProtoFound = 0;
                     break;
                  }
               }
               if(bProtoFound)
               {
                  break;
               }
            }
         }
         if(bProtoFound)
         {
            break;
         }
      }
      if(bProtoFound)
      {
         pBuff[iPos-iprotosize[protos]+1] = '\0';
         /* Set handle pointer in the end of the line */
         hb_fsSeek( hFileHandle, (((((long)read)-((long)iPos)))*-1)+1, FS_RELATIVE );
         return( iPos-iprotosize[protos] );
      }
   }
   return( read );
}

/* PRG level fReadLine( <Handle>, <@buffer>, [<MaxLineLen>], [<aTerminators | cTerminator>] ) */

HB_FUNC( FREADLINE )
{
   PHB_ITEM pProto, pProtoOpt;
   PHB_ITEM pResult     = hb_param( 2, HB_IT_BYREF );
   PHB_ITEM pMaxSize    = hb_param( 3, HB_IT_NUMERIC );
   FHANDLE hFileHandle  = (FHANDLE) hb_parnl( 1 );
   char ** Proto;
   BYTE * pBuffer;
   int * iprotosize;
   int result;
   USHORT iMax, i, iprotos;

   if( (!ISBYREF( 2 )) || (!ISNUM( 1 )) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "FREADLINE", 4,
         hb_paramError(1), hb_paramError(2),
         hb_paramError(3), hb_paramError(4) );
      return;
   }

   if( ISARRAY( 4 ) || ISCHAR( 4 ) )
   {
      if( ISARRAY( 4 ) )
      {
         pProto  = hb_param( 4, HB_IT_ARRAY );
         iprotos = (int) pProto->item.asArray.value->ulLen;

         if( iprotos <= 0 )
         {
            hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "FREADLINE", 4,
               hb_paramError(1), hb_paramError(2),
               hb_paramError(3), hb_paramError(4) );
            return;
         }

         Proto   = (char**) hb_xgrab( sizeof(char*) * iprotos );
         iprotosize = (int *) hb_xgrab( sizeof(int) * iprotos );

         for(i=0;i<iprotos;i++)
         {
            pProtoOpt     = hb_itemArrayGet( pProto, i+1 );
            Proto[i]      = (char *) pProtoOpt->item.asString.value;
            iprotosize[i] = pProtoOpt->item.asString.length;
            hb_itemRelease( pProtoOpt );
         }
      }
      else
      {
         pProto        = hb_param( 4, HB_IT_STRING );
         Proto         = (char**) hb_xgrab( sizeof(char*) );
         iprotosize    = (int *) hb_xgrab( sizeof(int) );
         Proto[0]      = (char *) pProto->item.asString.value;
         iprotosize[0] = pProto->item.asString.length;
         iprotos       = 1;
      }
   }
   else
   {
      Proto         = (char**) hb_xgrab( sizeof(char*) );
      iprotosize    = (int *) hb_xgrab( sizeof(int) );
      Proto[0]      = (char *) "\r\n";    /* Should be preplaced with the default EOL sequence */
      iprotos       = 1;
      iprotosize[0] = 2;
   }

   if( pMaxSize )
   {
      iMax = hb_itemGetNI( pMaxSize );
   }
   else
   {
      iMax = pResult->item.asString.length;
   }

   pBuffer = ( BYTE * ) pResult->item.asString.value;
   result  = hb_fsReadLine( hFileHandle, pBuffer, iMax, Proto, iprotosize, iprotos  );

   if( result > 0 )
   {
      /* It works, I hope this is not a crime :-| */
      pResult->item.asString.length = result+1;
   }

   hb_retnl( result );
   hb_xfree( Proto );
   hb_xfree( iprotosize );
}
