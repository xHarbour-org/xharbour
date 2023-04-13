/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour Unicode Support
 *
 * Source codes for functions:
 *    HB_CSGETERROR()
 *    HB_CSREG()
 *    HB_CSUNREG()
 *    HB_CSOPENED()
 *    HB_CSAVAIL()
 *    HB_CSLIST()
 *    HB_CSREADY()
 *    HB_CSTOCS()
 *    HB_B64ENCODE()
 *    HB_B64DECODE()
 *    HB_B64DECODE_FILE()
 *
 * Copyright 2011 Andi Jahja <andi.jahja@yahoo.co.id>
 * Copyright 2004 Dmitry V. Korzhov <dk@april26.spb.ru>
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

/*
   HB_CSGETERROR()
      Returns last error code from charset operations (see include file)
   Parameters:
      None
   Returns
      Error code from last operation

   HB_CSLIST() -> charset_string
      returns opened charsets list
   Parameters:
      None
   Returns:
      Comma separated active charsets list as a string

   HB_CSREADY() -> array of charset_string
      returns array of charset_string ready for use
   Parameters:
      None
   Returns:
      one dimension array

   HB_CSOPENED(charset_name) -> charset_handle
      returns a handle to opened charset table
   Parameters:
      charset_name - name of active charset table
   Returns:
      handle to charset or HB_CSINVALID on error

   HB_CSAVAIL(charset_name) -> .t. or .f.
      Checks availability of "charset_name" .cst file
      The sense of this function is it does not allocate memory
      if charset_name does not need to become active.
   Parameters:
      charset_name - name of charset table file (.cst) without extension
   Returns:
      .t. if charset (table file) found, else .f.

   HB_CSREG(charset_name)
      Register charset and make it active
   Parameters:
      charset_name - name of available charset table
   Returns:
      handle of opened charset table or HB_CSINVALID on error

   HB_CSUNREG(charset)
      unregisters (makes inactive) unnecessary charset (frees memory)
   Parameters:
      charset - active charset (handle or name)
   Returns
      Nil

   HB_CSTOCS(src_string,src_charset,dst_charset) -> dst_string
      Converts string values between charsets
      This is main function for character conversion.
   Parameters:
      src_string  - source character string
      src_charset - source active charset handle
      dst_charset - destination active charset handle
      ("active" means preset or previously registered with HB_CSREG())
   Returns:
      converted string, empty string on error

   HB_B64ENCODE(string) -> b64_string
      Encodes string to base64
   Parameters:
      string  - source character string
   Returns:
      encoded string

   HB_B64DECODE(b64_string) -> string
      Decodes string from base64
   Parameters:
      b64_string  - base64 encoded string
   Returns:
      decoded string

   B64DECODE_FILE( <cFileInput>, [<cFileOutput>] ) -> int
   Description:
      Decode a Base64 encoded file
   Parameters:
      cFileInput = string, source filename to be decoded
                   OR
                   array, an array of file chunks arranged in proper order
      cFileOutput = output filename
   Returns:
      Upon succesful decoding the function returns numnber of bytes written

 */
#include "hbcc.h"

#define HB_CSINVALID                0
#define HB_CSPRESET                 4096
#define HB_CSUNICODE                4097
#define HB_CSUCS2                   4097
#define HB_CSUCS2LE                 4097
#define HB_CSUCS2BE                 4098
#define HB_CSUTF8                   4099
#define HB_CSUTF7                   4100

#define HB_CSERR_OK                 0
#define HB_CSERR_BADARG             1
#define HB_CSERR_BADCS              2
#define HB_CSERR_BADCHAR            3
#define HB_CSERR_LIMIT              4
#define HB_CSERR_ALREADY_REGISTERED 5
#define HB_CSERR_NOFUNC             6
#define HB_CSERR_ERROR              99

#define MAX_CHARSIZE                256
#define MAX_WCHARSIZE               65536
#define CSINFO_HEADSZ               64
#define CSINFO_MAXNAME              48
#define CSINFO_MAXLEAD              5
#define CSINFO_OFFCHSZ              48
#define CSINFO_OFFDEFC              49
#define CSINFO_OFFLEAD              52
#define CSINFO_OFFTBSZ              63
#define B64_PADCH                   '='
#define B64_LINELEN                 60
#define UTF7START                   '+'

typedef struct
{
   BYTE name[ 48 ];
   char szfunc[ 64 ];
} HB_CSINIT;

typedef struct int_hb_csinfo
{
   BYTE name[ 48 ];
   BYTE hrbname[ 48 ];
   BYTE leads[ MAX_CHARSIZE ];
   BYTE chsz;
   BYTE defchar[ 2 ];
   BYTE ltc2u;
   BYTE reserved;
   BYTE * tc2u;
   BYTE * tu2c;
} HB_CSINFO;

//Common static variables
static ULONG         LastError   = 0, lcs = 0;
static HB_CSINFO **  pcs         = NULL;
static BYTE *        base64a     = ( BYTE * ) "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static BYTE *        trchars     = ( BYTE * ) "/\047(),.:?\n\r\v\t ";
static BYTE *        wspchars    = ( BYTE * ) "\n\r\v\t ";

//Protos are static to hide them from other modules
static BOOL b64invalid( BYTE );
static ULONG b64enc( BYTE *, ULONG, BYTE * );
static ULONG b64dec( BYTE *, ULONG, BYTE * );
static ULONG uni2ut7( BYTE *, ULONG, BYTE * );
static ULONG ut72uni( BYTE *, ULONG, BYTE * );
static ULONG ut82uni( BYTE *, ULONG, BYTE * );
static ULONG uni2ut8( BYTE *, ULONG, BYTE * );
static ULONG uni2chr( ULONG, BYTE *, ULONG, BYTE * );
static ULONG chr2uni( ULONG, BYTE *, ULONG, BYTE * );
static ULONG uni2uni( BYTE *, ULONG, BYTE * );

HB_FUNC_EXIT(   HB_INT_CSEXIT );
HB_FUNC_EXTERN( HB_BGMIK      );
HB_FUNC_EXTERN( HB_CP862      );
HB_FUNC_EXTERN( HB_CP866      );
HB_FUNC_EXTERN( HB_CP852      );
HB_FUNC_EXTERN( HB_CP1251     );
HB_FUNC_EXTERN( HB_CP1253     );
HB_FUNC_EXTERN( HB_KOI8R      );
HB_FUNC_EXTERN( HB_KOI8U      );

static HB_CSINIT  csInit[] = {
   { "BGMIK",  "HB_BGMIK"  },
   { "CP852",  "HB_CP852"  },
   { "CP862",  "HB_CP862"  },
   { "CP866",  "HB_CP866"  },
   { "CP1251", "HB_CP1251" },
   { "CP1253", "HB_CP1253" },
   { "KOI8R",  "HB_KOI8R"  },
   { "KOI8U",  "HB_KOI8U"  }
};

static UINT       lcsInit = sizeof( csInit ) / ( sizeof( csInit[ 0 ].name ) + sizeof( csInit[ 0 ].szfunc ) );

HB_FUNC_EXIT( HB_INT_CSEXIT )
{
   ULONG i;

   for( i = lcs; i; i-- )
   {
      if( pcs[ i - 1 ]->tu2c )
         hb_xfree( pcs[ i - 1 ]->tu2c );

      if( pcs[ i - 1 ]->tc2u )
         hb_xfree( pcs[ i - 1 ]->tc2u );

      if( pcs[ i - 1 ] )
         hb_xfree( pcs[ i - 1 ] );
   }

   LastError = HB_CSERR_OK;

   if( pcs )
      hb_xfree( pcs );

   pcs   = ( HB_CSINFO ** ) NULL;
   lcs   = 0;

   return;
}

HB_FUNC( HB_CSGETERROR )
{
   hb_retnl( LastError );
}

HB_FUNC( HB_CSREG )
{
   if( ISCHAR( 1 ) )
   {
      if( lcs < HB_CSPRESET )
      {
         char *   csname = hb_strdup( hb_parc( 1 ) );
         ULONG    i;
         ULONG    j, r;
         BYTE     c;
         char *   szProperties;
         BYTE *   szchartable;
         BOOL     bIsRegistered  = FALSE;
         PHB_DYNS pFunc          = NULL;

         hb_strupr( csname );

         for( i = 0; i < lcs; i++ )
         {
            if( strcmp( ( char * ) pcs[ i ]->hrbname, csname ) == 0 )
            {
               bIsRegistered = TRUE;
               break;
            }
         }

         if( bIsRegistered )
         {
            hb_xfree( csname );
            hb_retnl( i );
            return;
         }

         for( i = 0; i < lcsInit; i++ )
         {
            if( strcmp( ( char * ) csInit[ i ].name, csname ) == 0 )
            {
               pFunc = hb_dynsymFind( csInit[ i ].szfunc );
               break;
            }
         }

         if( pFunc == NULL )
         {
            hb_xfree( csname );
            LastError = HB_CSERR_NOFUNC;
            hb_retnl( HB_CSINVALID );
            return;
         }

         hb_vmPushSymbol( hb_dynsymSymbol( pFunc ) );
         hb_vmPushNil();
         hb_vmDo( 0 );
         szchartable = ( BYTE * ) hb_parcx( -1 );

         lcs++;

         pcs                           = pcs ? ( HB_CSINFO ** ) hb_xrealloc( pcs, lcs * sizeof( HB_CSINFO * ) ) :
                                         ( HB_CSINFO ** ) hb_xgrab( lcs * sizeof( HB_CSINFO * ) );

         pcs[ lcs - 1 ]                = ( HB_CSINFO * ) hb_xgrab( sizeof( HB_CSINFO ) );
         memcpy( pcs[ lcs - 1 ]->name, szchartable, CSINFO_MAXNAME );
         memcpy( pcs[ lcs - 1 ]->hrbname, csname, CSINFO_MAXNAME );
         r                             = CSINFO_MAXNAME;

         szProperties                  = ( char * ) hb_xgrab( CSINFO_HEADSZ - CSINFO_MAXNAME );
         memcpy( szProperties, szchartable + r, CSINFO_HEADSZ - CSINFO_MAXNAME );
         r                             += CSINFO_HEADSZ - CSINFO_MAXNAME;

         pcs[ lcs - 1 ]->chsz          = szProperties[ CSINFO_OFFCHSZ - CSINFO_MAXNAME ];
         pcs[ lcs - 1 ]->defchar[ 0 ]  = szProperties[ CSINFO_OFFDEFC - CSINFO_MAXNAME ];
         pcs[ lcs - 1 ]->defchar[ 1 ]  = szProperties[ CSINFO_OFFDEFC - CSINFO_MAXNAME + 1 ];
         pcs[ lcs - 1 ]->ltc2u         = szProperties[ CSINFO_OFFTBSZ - CSINFO_MAXNAME ];

         j                             = 0;

         for( i = 0; i < CSINFO_MAXLEAD; i++ )
         {
            if( szProperties[ 2 * i + CSINFO_OFFLEAD - CSINFO_MAXNAME ] && szProperties[ 2 * i + CSINFO_OFFLEAD - CSINFO_MAXNAME + 1 ] )
            {
               for( c = szProperties[ 2 * i + CSINFO_OFFLEAD - CSINFO_MAXNAME ]; c <= szProperties[ 2 * i + CSINFO_OFFLEAD - CSINFO_MAXNAME + 1 ]; c++ )
                  pcs[ lcs - 1 ]->leads[ j++ ] = c;
            }
         }

         hb_xfree( csname );
         hb_xfree( szProperties );

         j                    = ( 2 * MAX_CHARSIZE ) * pcs[ lcs - 1 ]->ltc2u;
         pcs[ lcs - 1 ]->tc2u = ( BYTE * ) hb_xgrab( j );
         memcpy( pcs[ lcs - 1 ]->tc2u, szchartable + r, j );
         r                    += j;

         j                    = MAX_WCHARSIZE * pcs[ lcs - 1 ]->chsz;
         pcs[ lcs - 1 ]->tu2c = ( BYTE * ) hb_xgrab( j );
         memcpy( pcs[ lcs - 1 ]->tu2c, szchartable + r, j );

         LastError            = HB_CSERR_OK;
         hb_retnl( lcs );
      }
      else
      {
         LastError = HB_CSERR_LIMIT;
         hb_retnl( HB_CSINVALID );
         return;
      }
   }
   else
   {
      LastError = HB_CSERR_BADARG;
      hb_retnl( HB_CSINVALID );
   }
}

HB_FUNC( HB_CSUNREG )
{
   ULONG    i;
   char *   csname = NULL;

   if( ISCHAR( 1 ) )
   {
      csname = hb_strdup( hb_parc( 1 ) );

      hb_strupr( csname );

      for( i = 0; i < lcs; i++ )
      {
         if( strcmp( ( char * ) pcs[ i ]->hrbname, csname ) == 0 )
            break;
      }

      if( i == lcs )
      {
         hb_xfree( csname );
         LastError = HB_CSERR_BADCS;
         hb_retl( FALSE );
         return;
      }
   }
   else if( ISNUM( 1 ) )
   {
      i = hb_parnl( 1 );

      if( i == 0 )
      {
         hb_retl( FALSE );
         return;
      }

      if( i > HB_CSPRESET )
      {
         LastError = HB_CSERR_BADCS;
         hb_retl( FALSE );
         return;
      }
      else
         i--;

      if( i > lcs )
      {
         LastError = HB_CSERR_BADCS;
         hb_retl( FALSE );
         return;
      }
   }
   else
   {
      LastError = HB_CSERR_BADARG;
      hb_retl( FALSE );
      return;
   }

   hb_xfree( pcs[ i ]->tu2c );
   hb_xfree( pcs[ i ]->tc2u );
   hb_xfree( pcs[ i ] );
   i++;

   for(; i < lcs; i++ )
   {
      pcs[ i - 1 ] = pcs[ i ];
   }
   lcs--;

   pcs = ( HB_CSINFO ** ) hb_xrealloc( pcs, lcs * sizeof( HB_CSINFO * ) );

   if( csname )
      hb_xfree( csname );

   hb_retl( TRUE );
}

HB_FUNC( HB_CSOPENED )
{
   if( ISCHAR( 1 ) )
   {
      ULONG    i;
      char *   csname   = hb_strdup( hb_parc( 1 ) );
      BOOL     bFound   = FALSE;

      hb_strupr( csname );

      LastError = HB_CSERR_BADCS;

      for( i = 0; i < lcs; i++ )
      {
         if( strcmp( csname, ( char * ) pcs[ i ]->hrbname ) == 0 )
         {
            bFound      = TRUE;
            LastError   = HB_CSERR_OK;
            break;
         }
      }

      if( bFound )
      {
         hb_retnl( i + 1 );
         hb_xfree( csname );
         return;
      }

      if( i == lcs )
      {
         if( ( hb_stricmp( csname, "UNICODE" ) == 0 ) ||
             ( hb_stricmp( csname, "UCS2" ) == 0 ) ||
             ( hb_stricmp( csname, "UCS2LE" ) == 0 ) )
         {
            LastError = HB_CSERR_OK;
            hb_retnl( HB_CSUCS2LE );
         }
         else if( hb_stricmp( csname, "UCS2BE" ) == 0 )
         {
            LastError = HB_CSERR_OK;
            hb_retnl( HB_CSUCS2BE );
         }
         else if( ( hb_stricmp( csname, "UTF8" ) == 0 ) ||
                  ( hb_stricmp( csname, "UTF-8" ) ) == 0 )
         {
            LastError = HB_CSERR_OK;
            hb_retnl( HB_CSUTF8 );
         }
         else if( ( hb_stricmp( csname, "UTF7" ) == 0 ) ||
                  ( hb_stricmp( csname, "UTF-7" ) ) == 0 )
         {
            LastError = HB_CSERR_OK;
            hb_retnl( HB_CSUTF7 );
         }
         else
         {
            LastError = HB_CSERR_BADCS;
            hb_retnl( HB_CSINVALID );
         }
      }
      hb_xfree( csname );
   }
   else
   {
      LastError = HB_CSERR_BADARG;
      hb_retnl( HB_CSINVALID );
   }
}

HB_FUNC( HB_CSAVAIL )
{
   BOOL x = FALSE;

   if( ISCHAR( 1 ) )
   {
      ULONG    i;
      char *   csname = hb_strdup( hb_parc( 1 ) );

      hb_strupr( csname );

      for( i = 0; i < lcs; i++ )
      {
         if( strcmp( ( char * ) pcs[ i ]->hrbname, csname ) == 0 )
         {
            x = TRUE;
            break;
         }
      }

      hb_xfree( csname );

      LastError = x ? HB_CSERR_OK : HB_CSERR_BADCS;
   }
   else
      LastError = HB_CSERR_BADARG;

   hb_retl( x );
}

HB_FUNC( HB_CSLIST )
{
   ULONG    i;
   char *   cslist = ( char * ) hb_xgrab( 1 );

   LastError   = HB_CSERR_OK;
   cslist[ 0 ] = '\0';

   for( i = 0; i < lcs; i++ )
   {
      cslist = ( char * ) hb_xrealloc( cslist, strlen( cslist ) + strlen( ( char * ) pcs[ i ]->name ) + 2 );
      hb_xstrcat( cslist, ( char * ) ",", ( char * ) pcs[ i ]->name, 0 );
   }

   hb_strupr( cslist );
   hb_retc( cslist + 1 );
   hb_xfree( cslist );
}

HB_FUNC( HB_CSREADY )
{
   ULONG    i;
   PHB_ITEM pReady = hb_itemArrayNew( 0 );

   LastError = HB_CSERR_OK;

   for( i = 0; i < lcsInit; i++ )
   {
      PHB_ITEM pCST = hb_itemPutC( NULL, ( char * ) csInit[ i ].name );
      hb_arrayAdd( pReady, pCST );
      hb_itemRelease( pCST );
   }

   hb_itemRelease( hb_itemReturn( pReady ) );
}

HB_FUNC( HB_CSTOCS )
{
   if( hb_pcount() == 3 )
   {
      PHB_ITEM pstr  = hb_param( 1, HB_IT_STRING );
      ULONG    h1    = hb_parnl( 2 );
      ULONG    h2    = hb_parnl( 3 );

      if( pstr && h1 && h2 )
      {
         BYTE *   srcstr   = ( BYTE * ) hb_itemGetCPtr( pstr );
         ULONG    srclen   = ( ULONG ) hb_itemGetCLen( pstr );

         LastError = HB_CSERR_OK;

         if( srclen )
         {
            if( h1 != h2 )
            {
               ULONG    dstlen   = 0, intlen = 0, x = 0;
               BYTE *   dststr   = 0, * intstr = 0;

               if( h1 == HB_CSUCS2BE )
               {
                  intlen   = srclen;
                  intstr   = srcstr;
               }
               else if( h1 == HB_CSUCS2LE )
               {
                  x        |= 1;
                  intstr   = ( BYTE * ) hb_xgrab( srclen );
                  intlen   = uni2uni( srcstr, srclen, intstr );
               }
               else if( h1 == HB_CSUTF8 )
               {
                  x        |= 1;
                  intlen   = ut82uni( srcstr, srclen, NULL );
                  intstr   = ( BYTE * ) hb_xgrab( intlen );
                  ut82uni( srcstr, srclen, intstr );
               }
               else if( h1 == HB_CSUTF7 )
               {
                  x        |= 1;
                  intlen   = ut72uni( srcstr, srclen, NULL );
                  intstr   = ( BYTE * ) hb_xgrab( intlen );
                  ut72uni( srcstr, srclen, intstr );
               }
               else if( h1 <= lcs )
               {
                  x        |= 1;
                  h1--;
                  intlen   = chr2uni( h1, srcstr, srclen, NULL );
                  intstr   = ( BYTE * ) hb_xgrab( intlen );
                  chr2uni( h1, srcstr, srclen, intstr );
               }
               else
                  LastError = HB_CSERR_BADCS;

               if( LastError == HB_CSERR_OK )
               {
                  if( h2 == HB_CSUCS2BE )
                  {
                     dstlen   = intlen;
                     dststr   = intstr;
                  }
                  else if( h2 == HB_CSUCS2LE )
                  {
                     x        |= 2;
                     dststr   = ( BYTE * ) hb_xgrab( intlen );
                     dstlen   = uni2uni( intstr, intlen, dststr );
                  }
                  else if( h2 == HB_CSUTF8 )
                  {
                     x        |= 2;
                     dstlen   = uni2ut8( intstr, intlen, NULL );
                     dststr   = ( BYTE * ) hb_xgrab( dstlen );
                     uni2ut8( intstr, intlen, dststr );
                  }
                  else if( h2 == HB_CSUTF7 )
                  {
                     x        |= 2;
                     dstlen   = uni2ut7( intstr, intlen, NULL );
                     dststr   = ( BYTE * ) hb_xgrab( dstlen );
                     uni2ut7( intstr, intlen, dststr );
                  }
                  else if( h2 <= lcs )
                  {
                     x        |= 2;
                     h2--;
                     dstlen   = uni2chr( h2, intstr, intlen, NULL );
                     dststr   = ( BYTE * ) hb_xgrab( dstlen );
                     uni2chr( h2, intstr, intlen, dststr );
                  }
                  else
                     LastError = HB_CSERR_BADCS;
               }
               else
                  hb_retc( "" );

               if( LastError == HB_CSERR_OK )
                  hb_retclen( ( char * ) dststr, dstlen );
               else
                  hb_retc( "" );

               if( x & 1 )
                  hb_xfree( intstr );

               if( x & 2 )
                  hb_xfree( dststr );
            }
            else
               hb_retclen( ( char * ) srcstr, srclen );
         }
         else
            hb_retc( "" );
      }
      return;
   }

   LastError = HB_CSERR_BADARG;
   hb_retc( "" );
}

HB_FUNC( HB_B64ENCODE )
{
   PHB_ITEM phbstr = hb_param( 1, HB_IT_STRING );
   ULONG    srclen, dstlen, i, n, k;
   BYTE *   srcstr, * dststr;

   if( phbstr )
   {
      srcstr   = ( BYTE * ) hb_itemGetCPtr( phbstr );
      srclen   = ( ULONG ) hb_itemGetCLen( phbstr );
      i        = b64enc( srcstr, srclen, NULL );
      k        = i & 3;

      if( k == 2 )
      {
         i += 2;
      }
      else if( k == 3 )
      {
         i++;
      }
      else if( k == 1 )
      {
         hb_retc( "" );
         return;
      }

      dstlen   = n = i + ( i / B64_LINELEN ) * 2; //In RFC CRLF's are standard
      dststr   = ( BYTE * ) hb_xgrab( dstlen );
      b64enc( srcstr, srclen, dststr );

      if( k )
      {
         dststr[ i - 1 ] = B64_PADCH;
      }

      if( k == 2 )
      {
         dststr[ i - 2 ] = B64_PADCH;
      }

      while( i && n )
      {
         if( i % B64_LINELEN == 0 )
         {
            dststr[ --n ]  = '\n';
            dststr[ --n ]  = '\r';
         }
         dststr[ --n ] = dststr[ --i ];
      }

      hb_retclenAdoptRaw( ( char * ) dststr, dstlen );
   }
   else
   {
      hb_retc( "" );
   }
}

HB_FUNC( HB_B64DECODE )
{
   PHB_ITEM phbstr = hb_param( 1, HB_IT_STRING );
   ULONG    srclen, dstlen, tmplen = 0, i = 0;
   BYTE *   srcstr, * dststr, * tmpstr;

   if( phbstr )
   {
      srcstr   = ( BYTE * ) hb_itemGetCPtr( phbstr );
      srclen   = ( ULONG ) hb_itemGetCLen( phbstr );
      tmpstr   = ( BYTE * ) hb_xgrab( srclen );

      while( i < srclen )
      {
         if( strchr( ( char * ) wspchars, srcstr[ i ] ) != NULL )
         {
            i++;
         }
         else if( b64invalid( srcstr[ i ] ) )
         {
            break;
         }
         else
         {
            tmpstr[ tmplen++ ] = srcstr[ i++ ];
         }
      }

      dstlen   = b64dec( tmpstr, tmplen, NULL );
      dststr   = ( BYTE * ) hb_xgrab( dstlen );
      b64dec( tmpstr, tmplen, dststr );
      hb_retclenAdoptRaw( ( char * ) dststr, dstlen );
      hb_xfree( tmpstr );
   }
   else
   {
      hb_retc( "" );
   }
}

//internal funcs

static BOOL b64invalid( BYTE c )
{
   return ( strchr( ( char * ) base64a, c ) == NULL ) || ( c == '\0' );
}

static ULONG b64enc( BYTE * srcstr, ULONG srclen, BYTE * dststr )
{
   ULONG dstlen, i;

   if( dststr )
   {
      dstlen   = 0;
      i        = 0;
      while( i < srclen )
      {
         dststr[ dstlen++ ] = base64a[ ( srcstr[ i ] & 0xFC ) >> 2 ];

         if( ++i == srclen )
         {
            dststr[ dstlen++ ] = base64a[ ( srcstr[ i - 1 ] & 0x03 ) << 4 ];
            break;
         }

         dststr[ dstlen++ ] = base64a[ ( ( srcstr[ i - 1 ] & 0x03 ) << 4 ) | ( ( srcstr[ i ] & 0xF0 ) >> 4 ) ];

         if( ++i == srclen )
         {
            dststr[ dstlen++ ] = base64a[ ( srcstr[ i - 1 ] & 0x0F ) << 2 ];
            break;
         }

         dststr[ dstlen++ ]   = base64a[ ( ( srcstr[ i - 1 ] & 0x0F ) << 2 ) | ( ( srcstr[ i ] & 0xC0 ) >> 6 ) ];
         dststr[ dstlen++ ]   = base64a[ srcstr[ i ] & 0x3F ];

         if( ++i == srclen )
         {
            break;
         }
      }
   }
   else
   {
      dstlen = ( srclen / 3 ) * 4 + ( srclen % 3 ? ( srclen % 3 + 1 ) : 0 );
   }

   return dstlen;
}

static ULONG b64dec( BYTE * srcstr, ULONG srclen, BYTE * dststr )
{
   ULONG    dstlen = 0, i;
   BYTE *   dummy;
   BYTE     tmp[ 4 ];

   i = 0;

   while( i < srclen )
   {
      dummy = ( BYTE * ) strchr( ( char * ) base64a, srcstr[ i++ ] );

      if( dummy == NULL )
      {
         break;
      }

      tmp[ 0 ] = ( BYTE ) ( dummy - base64a );

      if( i == srclen )
      {
         break;
      }

      dummy = ( BYTE * ) strchr( ( char * ) base64a, srcstr[ i++ ] );

      if( dummy == NULL )
      {
         break;
      }

      tmp[ 1 ] = ( BYTE ) ( dummy - base64a );

      if( dststr )
      {
         dststr[ dstlen++ ] = ( BYTE ) ( tmp[ 0 ] << 2 ) | ( ( tmp[ 1 ] & '\060' ) >> 4 );
      }
      else
      {
         dstlen++;
      }

      if( i == srclen )
      {
         break;
      }

      dummy = ( BYTE * ) strchr( ( char * ) base64a, srcstr[ i++ ] );

      if( dummy == NULL )
      {
         break;
      }

      tmp[ 2 ] = ( BYTE ) ( dummy - base64a );

      if( dststr )
      {
         dststr[ dstlen++ ] = ( BYTE ) ( ( tmp[ 1 ] & '\017' ) << 4 ) | ( ( tmp[ 2 ] & '\074' ) >> 2 );
      }
      else
      {
         dstlen++;
      }

      if( i == srclen )
      {
         break;
      }

      dummy = ( BYTE * ) strchr( ( char * ) base64a, srcstr[ i++ ] );

      if( dummy == NULL )
      {
         break;
      }

      tmp[ 3 ] = ( BYTE ) ( dummy - base64a );

      if( dststr )
      {
         dststr[ dstlen++ ] = ( BYTE ) ( ( tmp[ 2 ] & '\003' ) << 6 ) | tmp[ 3 ];
      }
      else
      {
         dstlen++;
      }
   }

   return dstlen;
}

static ULONG uni2uni( BYTE * srcstr, ULONG srclen, BYTE * dststr )
{
   ULONG i;

   for( i = 0; i < ( srclen - 1 ); )
   {
      dststr[ i ]       = srcstr[ i + 1 ];
      dststr[ i + 1 ]   = srcstr[ i ];
      i                 += 2;
   }

   return i;
}

static ULONG chr2uni( ULONG h, BYTE * chrstr, ULONG chrlen, BYTE * unistr )
{
   ULONG    i = 0, c, n, unilen = 0;
   BYTE *   x;

   while( i < chrlen )
   {
      if( pcs[ h ]->chsz == 1 )
      {
         n  = 1;
         c  = chrstr[ i ];
      }
      else
      {
         x = ( BYTE * ) strchr( ( char * ) pcs[ h ]->leads, chrstr[ i ] );

         if( x )
         {
            n  = 2;
            c  = ( ULONG ) ( ( x + 1 - pcs[ h ]->leads ) * MAX_CHARSIZE + chrstr[ i + 1 ] );
         }
         else
         {
            n  = 1;
            c  = chrstr[ i ];
         }
      }

      if( unistr )
      {
         unistr[ unilen ]     = pcs[ h ]->tc2u[ 2 * c + 1 ];
         unistr[ unilen + 1 ] = pcs[ h ]->tc2u[ 2 * c ];
      }

      i        += n;
      unilen   += 2;
   }

   return unilen;
}

static ULONG uni2chr( ULONG h, BYTE * unistr, ULONG unilen, BYTE * chrstr )
{
   ULONG i = 0, c, n, chrlen = 0;

   while( i < unilen - 1 )
   {
      c = ( ( ( ( ULONG ) unistr[ i ] ) << 8 ) | ( ULONG ) unistr[ i + 1 ] ) * ( ( ULONG ) pcs[ h ]->chsz );

      if( pcs[ h ]->chsz == 1 )
         n = 1;
      else
         n = ( pcs[ h ]->tu2c[ c + 1 ] ) ? 2 : 1;

      if( chrstr )
      {
         chrstr[ chrlen ] = pcs[ h ]->tu2c[ c ];

         if( n > 1 )
            chrstr[ chrlen + 1 ] = pcs[ h ]->tu2c[ c + 1 ];
      }

      chrlen   += n;
      i        += 2;
   }

   return chrlen;
}

static ULONG ut82uni( BYTE * utfstr, ULONG utflen, BYTE * unistr )
{
   ULONG i, n, unilen = 0;

   for( i = 0; i < utflen; )
   {
      if( utfstr[ i ] & 0x80 )
      {
         if( utfstr[ i ] & 0x40 )
         {
            if( utfstr[ i ] & 0x20 )
               n = 3;   //0x800-0xFFFF,3
            else
               n = 2;   //0x80-0x7FF,2
         }
         else
            n = 0;
      }
      else
         n = 1;       //0x00-0x7F,1

      if( ( i + n ) > utflen ) //incomplete
      {
         if( unistr )
         {
            unistr[ unilen++ ]   = '\0';
            unistr[ unilen++ ]   = '?';
         }
         else
            unilen += 2;

         return unilen;
      }

      if( unistr )
      {
         switch( n )
         {
            case 0: //wrong utf-8 char
               unistr[ unilen++ ]   = '\0';
               unistr[ unilen++ ]   = '?';
               break;

            case 1:
               unistr[ unilen++ ]   = '\0';
               unistr[ unilen++ ]   = utfstr[ i ];
               break;

            case 2:
               unistr[ unilen++ ]   = ( utfstr[ i ] & '\034' ) >> 2;
               unistr[ unilen++ ]   = ( utfstr[ i + 1 ] & '\077' ) | ( ( utfstr[ i ] & '\003' ) << 6 );
               break;

            case 3:
               unistr[ unilen++ ]   = ( ( utfstr[ i ] & '\017' ) << 4 ) | ( ( utfstr[ i + 1 ] & '\074' ) >> 2 );
               unistr[ unilen++ ]   = ( ( utfstr[ i + 1 ] & '\003' ) << 6 ) | ( utfstr[ i + 2 ] & '\077' );
               break;
         }
      }
      else
         unilen += 2;

      i += n;
   }

   return unilen;
}

static ULONG uni2ut8( BYTE * unistr, ULONG unilen, BYTE * utfstr )
{
   ULONG i, n, utflen = 0;

   for( i = 0; i < ( unilen - 1 ); )
   {
      if( ( unistr[ i ] | ( unistr[ i + 1 ] & '\200' ) ) == '\0' )
         n = 1;   //0x00-0x7F
      else if( unistr[ i ] < '\010' )
         n = 2;   //0x80-0x7ff
      else
         n = 3;   //0x0800-0xffff

      if( utfstr )
      {
         switch( n )
         {
            case 1:
               utfstr[ utflen++ ] = unistr[ i + 1 ];
               break;

            case 2:
               utfstr[ utflen++ ]   = '\300' | ( ( '\007' & unistr[ i ] ) << 2 ) | ( '\003' & ( unistr[ i + 1 ] >> 6 ) );
               utfstr[ utflen++ ]   = '\200' | ( '\077' & unistr[ i + 1 ] );
               break;

            case 3:
               utfstr[ utflen++ ]   = '\340' | ( '\017' & ( unistr[ i ] >> 4 ) );
               utfstr[ utflen++ ]   = '\200' | ( ( '\017' & unistr[ i ] ) << 2 ) | ( '\003' & ( unistr[ i + 1 ] >> 6 ) );
               utfstr[ utflen++ ]   = '\200' | ( '\077' & unistr[ i + 1 ] );
               break;
         }
      }
      else
         utflen += n;

      i += 2;
   }

   return utflen;
}

static ULONG ut72uni( BYTE * utfstr, ULONG utflen, BYTE * unistr )
{
   ULONG    i, j, unilen = 0, state = 0;
   BYTE *   dummy = 0, c;

   for( i = 0; i < utflen; i++ )
   {
      if( state == 1 )
      {
         if( b64invalid( utfstr[ i ] ) )
         {
            state = 0;

            if( ( utfstr[ i ] == '-' ) && ( utfstr[ i - 1 ] == '+' ) )
               c = '+';
            else
               c = utfstr[ i ];

            j = b64dec( dummy, ( ULONG ) ( i - ( dummy - utfstr ) ), NULL );

            if( unistr )
               b64dec( dummy, ( ULONG ) ( i - ( dummy - utfstr ) ), unistr + unilen );

            unilen += j;

            if( c != '-' )
            {
               if( unistr )
               {
                  unistr[ unilen++ ]   = '\0';
                  unistr[ unilen++ ]   = c;
               }
               else
                  unilen += 2;
            }
         }
      }
      else if( utfstr[ i ] == UTF7START )
      {
         dummy = utfstr + ( i + 1 );
         state = 1;
      }
      else
      {
         if( unistr )
         {
            unistr[ unilen++ ]   = '\0';
            unistr[ unilen++ ]   = utfstr[ i ];
         }
         else
            unilen += 2;
      }
   }

   if( state == 1 )
   {
      j = b64dec( dummy, ( ULONG ) ( utflen - ( dummy - utfstr ) ), NULL );

      if( unistr )
         b64dec( dummy, ( ULONG ) ( utflen - ( dummy - utfstr ) ), unistr + unilen );

      unilen += j;
   }

   return unilen;
}

static ULONG uni2ut7( BYTE * unistr, ULONG unilen, BYTE * utfstr )
{
   ULONG    i, j, utflen = 0, state = 0;
   BYTE *   dummy = 0;

   for( i = 0; i < unilen - 1; i += 2 )
   {
      if( ( unistr[ i ] == '\0' ) && ( unistr[ i + 1 ] == '+' ) )
      {
         if( state == 1 )
         {
            j = b64enc( dummy, ( ULONG ) ( i - ( dummy - unistr ) ), NULL );

            if( utfstr )
            {
               utfstr[ utflen++ ]   = '+';
               b64enc( dummy, ( ULONG ) ( i - ( dummy - unistr ) ), utfstr + utflen );
               utflen               += j;
               utfstr[ utflen++ ]   = '-';
            }
            else
               utflen += ( j + 2 );
         }

         if( utfstr )
         {
            utfstr[ utflen++ ]   = '+';
            utfstr[ utflen++ ]   = '-';
         }
         else
            utflen += 2;

         state = 0;
      }
      else if( ( unistr[ i ] == '\0' ) && ( unistr[ i + 1 ] == '-' ) )
      {
         if( state == 1 )
         {
            j = b64enc( dummy, ( ULONG ) ( i - ( dummy - unistr ) ), NULL );

            if( utfstr )
            {
               utfstr[ utflen++ ]   = '+';
               b64enc( dummy, ( ULONG ) ( i - ( dummy - unistr ) ), utfstr + utflen );
               utflen               += j;
               utfstr[ utflen++ ]   = '-';
            }
            else
               utflen += ( j + 2 );
         }

         if( utfstr )
            utfstr[ utflen++ ] = '-';
         else
            utflen++;

         state = 0;
      }
      else if( ( unistr[ i ] == '\0' ) && ( strchr( ( char * ) base64a, unistr[ i + 1 ] ) != NULL ) && ( unistr[ i + 1 ] != 0 ) )
      {
         if( state == 1 )
         {
            j = b64enc( dummy, ( ULONG ) ( i - ( dummy - unistr ) ), NULL );

            if( utfstr )
            {
               utfstr[ utflen++ ]   = '+';
               b64enc( dummy, ( ULONG ) ( i - ( dummy - unistr ) ), utfstr + utflen );
               utflen               += j;
               utfstr[ utflen++ ]   = '-';
            }
            else
               utflen += ( j + 2 );
         }

         if( utfstr )
            utfstr[ utflen++ ] = unistr[ i + 1 ];
         else
            utflen++;

         state = 0;
      }
      else if( ( unistr[ i ] == '\0' ) && ( strchr( ( char * ) trchars, unistr[ i + 1 ] ) != NULL ) && ( unistr[ i + 1 ] != 0 ) )
      {
         if( state == 1 )
         {
            j = b64enc( dummy, ( ULONG ) ( i - ( dummy - unistr ) ), NULL );

            if( utfstr )
            {
               utfstr[ utflen++ ]   = '+';
               b64enc( dummy, ( ULONG ) ( i - ( dummy - unistr ) ), utfstr + utflen );
               utflen               += j;
            }
            else
               utflen += ( j + 1 );
         }

         if( utfstr )
            utfstr[ utflen++ ] = unistr[ i + 1 ];
         else
            utflen++;

         state = 0;
      }
      else if( state == 0 )
      {
         dummy = unistr + i;
         state = 1;
      }
   }

   if( state == 1 )
   {
      j = b64enc( dummy, ( ULONG ) ( unilen - ( dummy - unistr ) ), NULL );

      if( utfstr )
      {
         utfstr[ utflen++ ]   = '+';
         b64enc( dummy, ( ULONG ) ( unilen - ( dummy - unistr ) ), utfstr + utflen );
         utflen               += j;
      }
      else
         utflen += ( j + 1 );
   }

   return utflen;
}

//----------------------------------------------------------------------------//
HB_FUNC( B64DECODE_FILE )
{
   PHB_ITEM pinFile        = hb_param( 1, HB_IT_ANY );
   PHB_ITEM poutFile       = hb_param( 2, HB_IT_STRING );
   FILE *   inFile, * outFile = NULL;
   char *   string, * szFileName;
   ULONG    nBytesWritten  = 0;
   HB_ITEM  Struct, Item;
   USHORT   uiLen          = 1, uiCount;
   BOOL     bOutFile       = FALSE;

   ULONG    srclen, dstlen, tmplen, i;
   BYTE *   dststr, * tmpstr;

   char *   szFile = NULL;

   if( pinFile )
   {
      if( ISCHAR( 1 ) )
      {
         if( strlen( pinFile->item.asString.value ) == 0 )
         {
            hb_retni( -1 );
            return;
         }
         else
         {
            Struct.type = HB_IT_NIL;
            Item.type   = HB_IT_NIL;
            hb_arrayNew( &Struct, 1 );
            hb_arraySet( &Struct, 1, hb_itemPutC( &Item, pinFile->item.asString.value ) );
            hb_itemClear( &Item );
         }
      }
      else if( ISARRAY( 1 ) )
      {
         Struct.type = HB_IT_NIL;
         hb_itemCopy( &Struct, hb_param( 1, HB_IT_ARRAY ) );
         uiLen       = ( USHORT ) Struct.item.asArray.value->ulLen;

         if( uiLen <= 0 )
         {
            hb_itemClear( &Struct );
            hb_retni( -2 );
            return;
         }
      }
      else
      {
         hb_itemClear( &Struct );
         hb_retni( -3 );
         return;
      }
   }
   else
   {
      hb_retni( -4 );
      return;
   }

   if( poutFile )
   {
      if( strlen( poutFile->item.asString.value ) == 0 )
      {
         hb_itemClear( &Struct );
         hb_retni( -5 );
         return;
      }
   }

   string = ( char * ) hb_xgrab( SHRT_MAX );

   for( uiCount = 0; uiCount < uiLen; uiCount++ )
   {
      szFileName = hb_arrayGetC( &Struct, uiCount + 1 );

      if( ! szFileName )
      {
         hb_itemClear( &Struct );
         hb_xfree( string );
         hb_retni( -6 );
         return;
      }

      if( strlen( szFileName ) == 0 )
      {
         hb_itemClear( &Struct );
         hb_xfree( szFileName );
         hb_xfree( string );
         hb_retni( -7 );
         return;
      }

      inFile = hb_fopen( szFileName, "rb" );

      if( ! inFile )
      {
         hb_itemClear( &Struct );
         hb_xfree( szFileName );
         hb_xfree( string );
         hb_retni( -8 );
         return;
      }

      if( ! bOutFile )
      {
         while( hbcc_file_read( inFile, string ) )
         {
            if( string )
            {
               if( strstr( string, "Content-Transfer-Encoding: base64" ) != NULL )
               {
                  break;
               }

               if( ! bOutFile )
               {
                  if( poutFile )
                  {
                     outFile = hb_fopen( poutFile->item.asString.value, "wb" );

                     if( ! outFile )
                     {
                        break;
                     }

                     bOutFile = TRUE;
                  }
                  else
                  {
                     if( strstr( string, "Content-Type: application/octet-stream; name=" ) != NULL )
                     {
                        szFile                           = string + 46;
                        szFile[ strlen( szFile ) - 1 ]   = '\0';
                     }

                     if( szFile )
                     {
                        outFile = hb_fopen( szFile, "wb" );

                        if( ! outFile )
                        {
                           break;
                        }

                        bOutFile = TRUE;
                     }
                  }
                  continue;
               }
            }
         }
      }

      if( ! bOutFile )
      {
         hb_itemClear( &Struct );
         fclose( inFile );
         hb_xfree( string );
         if( szFileName )
         {
            hb_xfree( szFileName );
         }
         hb_retni( -9 );
         return;
      }

      while( hbcc_file_read( inFile, string ) )
      {
         srclen = ( ULONG ) strlen( string );

         if( srclen )
         {
            i        = 0;
            tmplen   = 0;
            tmpstr   = ( BYTE * ) hb_xgrab( srclen );

            while( i < srclen )
            {
               if( strchr( ( char * ) wspchars, string[ i ] ) != NULL )
               {
                  i++;
               }
               else if( b64invalid( string[ i ] ) )
               {
                  break;
               }
               else
               {
                  tmpstr[ tmplen++ ] = string[ i++ ];
               }
            }

            dstlen         = b64dec( tmpstr, tmplen, NULL );
            dststr         = ( BYTE * ) hb_xgrab( dstlen );
            b64dec( tmpstr, tmplen, dststr );

            nBytesWritten  += ( ULONG ) fwrite( dststr, sizeof( BYTE ), dstlen, outFile );

            hb_xfree( dststr );
            hb_xfree( tmpstr );
         }
      }

      fclose( inFile );

      if( szFileName )
      {
         hb_xfree( szFileName );
      }
   }

   hb_retnl( nBytesWritten );

   hb_xfree( string );

   if( outFile )
   {
      fclose( outFile );
   }

   hb_itemClear( &Struct );

}

#define __PRG_SOURCE__     "hbcc.c"
#ifdef HB_PCODE_VER
#  undef HB_PRG_PCODE_VER
#  define HB_PRG_PCODE_VER HB_PCODE_VER
#endif

HB_INIT_SYMBOLS_BEGIN( hbcc_InitExit )
{
   "HB_BGMIK", { HB_FS_PUBLIC }, { HB_FUNCNAME( HB_BGMIK    ) }, NULL
},
{ "HB_CP866", { HB_FS_PUBLIC }, { HB_FUNCNAME( HB_CP866    ) }, NULL },
{ "HB_CP862", { HB_FS_PUBLIC }, { HB_FUNCNAME( HB_CP862    ) }, NULL },
{ "HB_CP852", { HB_FS_PUBLIC }, { HB_FUNCNAME( HB_CP852    ) }, NULL },
{ "HB_CP1251", { HB_FS_PUBLIC }, { HB_FUNCNAME( HB_CP1251   ) }, NULL },
{ "HB_CP1253", { HB_FS_PUBLIC }, { HB_FUNCNAME( HB_CP1253   ) }, NULL },
{ "HB_KOI8R", { HB_FS_PUBLIC }, { HB_FUNCNAME( HB_KOI8R    ) }, NULL },
{ "HB_KOI8U", { HB_FS_PUBLIC }, { HB_FUNCNAME( HB_KOI8U    ) }, NULL },
{ "HB_INT_CSEXIT$", { HB_FS_EXIT | HB_FS_LOCAL }, { HB_EXIT_FUNCNAME( HB_INT_CSEXIT ) }, &ModuleFakeDyn }
HB_INIT_SYMBOLS_END( hbcc_InitExit )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hbcc_InitExit
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY HB_DATASEG_FUNC( hbcc_InitExit )
   #include "hbiniseg.h"
#endif

