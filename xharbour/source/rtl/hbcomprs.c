/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Compression related functions
 *
 * Copyright 2003 Giancarlo Niccolai <giancarlo@niccolai.ws>
 * www - http://www.xharbour.org
 * SEE ALSO COPYRIGHT NOTICE FOR ZLIB BELOW.
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
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

/* This file is based upon ZLIB source code, whose copyright holder is:
 *
 * Copyright (C) 1995-2002 Jean-loup Gailly.
 *
 * Also, this file includes code slices from adler32.c for advanced CRC
 * Holder of copyright for this code is:
 *
 * Copyright (C) 1995-2002 Mark Adler
 *
 * ZLIB (containing adler32 code) can be found at:
 * http://www.gzip.org/zlib/
 */

#include "hbzlib.h"
int s_hb_compress_error;

static int hb_gz_compress( char ** pDstPtr, HB_SIZE * pnDst,
                           const char * pSrc, HB_SIZE nSrc, int level )
{
   z_stream stream;
   int      iResult;

   memset( &stream, 0, sizeof( z_stream ) );
   stream.next_in    = ( Bytef * ) pSrc;
   stream.avail_in   = ( uInt ) nSrc;
   iResult           = deflateInit2( &stream, level, Z_DEFLATED, 15 + 16, 8,
                                     Z_DEFAULT_STRATEGY );
   if( iResult == Z_OK )
   {
      if( *pDstPtr == NULL )
      {
         if( *pnDst == 0 )
            *pnDst = deflateBound( &stream, ( uLong ) nSrc );
         *pDstPtr = ( char * ) hb_xalloc(  *pnDst + 1 );
         if( *pDstPtr == NULL )
            iResult = Z_MEM_ERROR;
      }
   }

   if( iResult == Z_OK )
   {
      stream.next_out   = ( Bytef * ) *pDstPtr;
      stream.avail_out  = ( uInt ) * pnDst;

      do
      {
         iResult = deflate( &stream, Z_FINISH );
      }
      while( iResult == Z_OK );

      if( iResult == Z_STREAM_END )
      {
         *pnDst   = stream.total_out;
         iResult  = Z_OK;
      }
      deflateEnd( &stream );
   }

   return iResult;
}

static HB_SIZE hb_zlibUncompressedSize( const char * szSrc, HB_SIZE nLen,
                                      int * piResult )
{
   Byte     buffer[ 1024 ];
   z_stream stream;
   HB_SIZE    nDest = 0;

   memset( &stream, 0, sizeof( z_stream ) );

   stream.next_in    = ( Bytef * ) szSrc;
   stream.avail_in   = ( uInt ) nLen;
/*
   stream.zalloc    = Z_NULL;
   stream.zfree     = Z_NULL;
   stream.opaque    = NULL;
 */

   *piResult         = inflateInit2( &stream, 15 + 32 );
   if( *piResult == Z_OK )
   {
      do
      {
         stream.next_out   = buffer;
         stream.avail_out  = sizeof( buffer );
         *piResult         = inflate( &stream, Z_NO_FLUSH );
      }
      while( *piResult == Z_OK );

      if( *piResult == Z_STREAM_END )
      {
         nDest       = stream.total_out;
         *piResult   = Z_OK;
      }
      inflateEnd( &stream );
   }

   return nDest;
}

static int hb_zlibUncompress( char * pDst, HB_SIZE * pnDst,
                              const char * pSrc, HB_SIZE nSrc )
{
   z_stream stream;
   int      iResult;

   memset( &stream, 0, sizeof( z_stream ) );
   stream.next_in    = ( Bytef * ) pSrc;
   stream.avail_in   = ( uInt ) nSrc;
   iResult           = inflateInit2( &stream, 15 + 32 );

   if( iResult == Z_OK )
   {
      stream.next_out   = ( Bytef * ) pDst;
      stream.avail_out  = ( uInt ) * pnDst;

      do
      {
         iResult = inflate( &stream, Z_FINISH );
      }
      while( iResult == Z_OK );

      if( iResult == Z_STREAM_END )
      {
         *pnDst   = stream.total_out;
         iResult  = Z_OK;
      }
      inflateEnd( &stream );
   }

   return iResult;
}
/*
 * HB_ZUNCOMPRESS( <cCompressedData>, [<nDstBufLen>|<@cBuffer>], [<@nResult>] )
 *    => <cUnCompressedData> or NIL on Error
 */
HB_FUNC( HB_ZUNCOMPRESS )
{
   PHB_ITEM       pBuffer  = ISBYREF( 2 ) ? hb_param( 2, HB_IT_STRING ) : NULL;
   const char *   szData   = hb_parc( 1 );

   if( szData )
   {
      HB_SIZE nLen = hb_parclen( 1 );

      if( nLen )
      {
         HB_SIZE  nDstLen;
         char *   pDest    = NULL;
         int      iResult  = Z_OK;

         if( pBuffer )
         {
            if( ! hb_itemGetWriteCL( pBuffer, &pDest, &nDstLen ) )
               iResult = Z_MEM_ERROR;
         }
         else
         {
            nDstLen = ISNUM( 2 ) ? (HB_SIZE) hb_parns( 2 ) :
                      hb_zlibUncompressedSize( szData, nLen, &iResult );
            if( iResult == Z_OK )
            {
               pDest = ( char * ) hb_xalloc(  nDstLen + 1 );
               if( ! pDest )
                  iResult = Z_MEM_ERROR;
            }
         }

         if( iResult == Z_OK )
         {
            iResult = hb_zlibUncompress( pDest, &nDstLen, szData, nLen );

            if( ! pBuffer )
            {
               if( iResult == Z_OK )
                  hb_retclen_buffer( pDest,  nDstLen );
               else
                  hb_xfree( pDest );
            }
            else if( iResult == Z_OK )
               hb_retclen( pDest,  nDstLen );
         }
         hb_storni( iResult, 3 );
      }
      else
      {
         hb_retc_null();
         hb_storni( Z_OK, 3 );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * HB_GZCOMPRESS( <cData>, [<nDstBufLen>|<@cBuffer>], [<@nResult>], [<nLevel>] )
 *    => <cCompressedData> or NIL on Error
 *
 * Note: this function does not create any references to gz* ZLIB functions
 *       so it's intentionally here not in hbzlibgz.c file.
 */
HB_FUNC( HB_GZCOMPRESS )
{
   const char * szData = hb_parc( 1 );

   if( szData )
   {
      HB_SIZE nLen = hb_parclen( 1 );

      if( nLen )
      {
         PHB_ITEM pBuffer  = ISBYREF( 2 ) ? hb_param( 2, HB_IT_STRING ) : NULL;
         BOOL     fAlloc   = FALSE;
         HB_SIZE  nDstLen;
         char *   pDest;
         int      iResult;

         if( pBuffer )
         {
            if( ! hb_itemGetWriteCL( pBuffer, &pDest, &nDstLen ) )
               pDest = NULL;
         }
         else
         {
            if( ISNUM( 2 ) )
            {
               nDstLen  = hb_parns( 2 );
               pDest    = ( char * ) hb_xalloc(  nDstLen + 1 );
            }
            else
            {
               pDest    = NULL;
               nDstLen  = 0;
               fAlloc   = TRUE;
            }
         }

         if( pDest || fAlloc )
         {
            iResult = hb_gz_compress( &pDest, &nDstLen, szData, nLen,
                                      hb_parnidef( 4, Z_DEFAULT_COMPRESSION ) );
            if( ! pBuffer )
            {
               if( iResult == Z_OK )
                  hb_retclen_buffer( pDest,  nDstLen );
               else if( pDest )
                  hb_xfree( pDest );
            }
            else if( iResult == Z_OK )
               hb_retclen( pDest,  nDstLen );
         }
         else
            iResult = Z_MEM_ERROR;

         hb_storni( iResult, 3 );
      }
      else
      {
         hb_retc_null();
         hb_storni( Z_OK, 3 );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/*******************************************************************
 * Giancarlo Niccolai:
 * Calculates the minimum length of destination buffer
 */

HB_SIZE hb_destBuflen( HB_SIZE srclen )
{
   HB_SIZE ret = srclen;

   ret += ret / 100 * 15 + 12;
   if( srclen % 100 != 0 )
      ret += 15;
   return ret;
}

/****** COMPRESSOR WRAPPER *****
 *  HB_COMPRESS( cSource [, nSourceLen ] ) --> cDest
 *  HB_COMPRESS( nComprFactor, cSource [,nSourceLen ] ) --> cDest
 *  HB_COMPRESS( cSource, nSourceLen, @cDest, @nDestLen ) --> nError
 *  HB_COMPRESS( nComprFactor, cSource, nSourceLen, @cDest, @nDestLen ) --> nError
 */

HB_FUNC( HB_COMPRESS )
{
   char *   cDest, * cSource;
   HB_SIZE  ulDstlen;
   HB_SIZE  ulSrclen;
   HB_SIZE  ulBufLen;
   PHB_ITEM pSource, pDest = NULL, pDestLen = NULL;
   int      nCompFactor, iFirst;
   int      cerr;

   if( ISNUM( 1 ) )
   {
      nCompFactor = hb_parni( 1 );
      iFirst      = 1;
   }
   else
   {
      nCompFactor = Z_DEFAULT_COMPRESSION;
      iFirst      = 0;
   }

   pSource = hb_param( iFirst + 1, HB_IT_STRING );

   if( pSource == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
      return;
   }

   cSource = hb_itemGetCPtr( pSource );
   if( hb_pcount() > iFirst + 1 )
   {
      ulSrclen =  hb_parns( iFirst + 2 );
   }
   else
   {
      ulSrclen = hb_itemGetCLen( pSource );
   }

   /* Allocation mode: user provided or allocated here */
   if( hb_pcount() == iFirst + 4 )
   {
      pDest    = hb_param( iFirst + 3, HB_IT_BYREF );
      pDestLen = hb_param( iFirst + 4, HB_IT_BYREF );
      ulDstlen = hb_parns( iFirst + 4 );
      cDest    = NULL;
      ulBufLen = 0;
      if( pDest && pDestLen && ulDstlen > 0 )
      {
         hb_itemGetWriteCL( pDest, &cDest, &ulBufLen );
      }
      if( cDest == NULL || ulBufLen < ulDstlen )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
         return;
      }
   }
   else
   {
      ulDstlen = hb_destBuflen( ulSrclen );
      cDest    = ( char * ) hb_xgrab( ulDstlen + 1 );
   }

   cerr = compress2( ( Bytef * ) cDest, ( uLongf * ) &ulDstlen, ( const Bytef * ) cSource,  (uLong)ulSrclen,
                    nCompFactor );

   if( cerr != Z_OK )
   {
      if( pDest != NULL )
      {
         hb_retni( cerr );
      }
      else
      {
         hb_xfree( cDest );
         hb_ret();
      }
   }
   else
   {
      if( pDestLen != NULL )
      {
         hb_stornl( iFirst + 4, ( LONG ) ulDstlen );
         hb_retni( Z_OK );
      }
      else
      {
         hb_retclenAdoptRaw( cDest, ulDstlen );
      }
   }
   s_hb_compress_error = cerr;
}


/****** DECOMPRESSOR WRAPPER *****
 *  HB_UNCOMPRESS( nDestLen, cSource [, nSourceLen ] ) --> cDest
 *  HB_UNCOMPRESS( nDestLen, cSource, nSourceLen, @cDest ) --> nError
 */

HB_FUNC( HB_UNCOMPRESS )
{
   char *   cDest, * cSource;
   HB_SIZE    ulSrclen;
   ULONG      ulDstlen;
   HB_SIZE  ulBufLen;
   PHB_ITEM pSource, pDest;
   int      cerr;

   pSource = hb_param( 2, HB_IT_STRING );

   if( ! ISNUM( 1 ) || pSource == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
      return;
   }

   cSource  = hb_itemGetCPtr( pSource );
   ulDstlen = ( HB_SIZE ) hb_parnl( 1 );
   if( hb_pcount() > 2 )
   {
      ulSrclen = ( HB_SIZE ) hb_parnl( 3 );
   }
   else
   {
      ulSrclen = ( HB_SIZE ) hb_itemGetCLen( pSource );
   }

   /* Allocation mode: user provided or allocated here */
   if( hb_pcount() == 4 )
   {
      pDest    = hb_param( 4, HB_IT_BYREF );
      cDest    = NULL;
      ulBufLen = 0;
      if( pDest )
      {
         hb_itemGetWriteCL( pDest, &cDest, &ulBufLen );
      }
      if( cDest == NULL || ulBufLen < ulDstlen )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
         return;
      }
   }
   else
   {
      cDest = ( char * ) hb_xgrab( ulDstlen + 1 );
   }

   cerr = uncompress( ( Bytef * ) cDest, &ulDstlen, ( const Bytef * ) cSource, (uLong) ulSrclen );

   if( cerr != Z_OK )
   {
      if( hb_pcount() == 4 )
      {
         hb_retni( cerr );
      }
      else
      {
         hb_xfree( cDest );
         hb_ret();
      }
   }
   else
   {
      if( hb_pcount() == 4 )
      {
         hb_retni( Z_OK );
      }
      else
      {
         hb_retclenAdopt( cDest, ulDstlen );
      }
   }
   s_hb_compress_error = cerr;
}

/*********************************
 * HB_COMPRESSERROR() --> nError
 */
HB_FUNC( HB_COMPRESSERROR )
{
   hb_retni( s_hb_compress_error );
}

/*********************************
 * HB_COMPRESSERRORDESC( nErrorCode ) --> cDesc
 */

HB_FUNC( HB_COMPRESSERRORDESC )
{
   hb_retcAdopt( hb_strdup( zError( hb_parni( 1 ) ) ) );
}

/*******************************
 * HB_COMPRESSBUFLEN( nSrcLen ) -->nDestLen
 */

HB_FUNC( HB_COMPRESSBUFLEN )
{
   hb_retnl( ( LONG ) hb_destBuflen( hb_parni( 1 ) ) );
}

