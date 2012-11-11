/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour Unicode Support
 *
 * Source codes for functions:
 *
 * Copyright 2004 Andi Jahja <xharbour@cbn.net.id>
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
#include "hbcc.h"

#define MAX_LEADS    10
#define MAX_CHAR     0x100
#define MAX_2CHAR    0x200
#define MAX_WCHAR    0x10000
#define MAX_2WCHAR   0x20000

static char    s_defchar   = '?';
static char    s_maxchar   = '\001';
static char    s_cleads[ MAX_CHAR ];
static char    c_leads[ MAX_CHAR ];
static char    s_tuni[ MAX_2CHAR ];
static char    s_tchar[ MAX_2WCHAR ];
static char *  s_aleads = NULL;

#undef LINE_MAX
#define LINE_MAX SHRT_MAX

//----------------------------------------------------------------------------//
BYTE * hbcc_getfilename( BYTE * strFullPath )
{
   static BYTE strTmp[ HB_PATH_MAX ];
   PHB_FNAME   pFileName = hb_fsFNameSplit( ( const char * ) strFullPath );

   hb_snprintf( ( char * ) strTmp, sizeof( strTmp ), pFileName->szName );
   hb_xfree( pFileName );
   return strTmp;
}

//----------------------------------------------------------------------------//
BOOL hbcc_file_read( FILE * fileHandle, char * string )
{
   int ch, cnbr = 0;

   memset( string, ' ', LINE_MAX );

   for(;; )
   {
      ch = fgetc( fileHandle );

      if( ( ch == '\n' ) || ( ch == EOF ) || ( ch == 26 ) )
      {
         string[ cnbr ] = '\0';
         return ch == '\n' || cnbr;
      }
      else
      {
         if( cnbr < LINE_MAX )
         {
            if( ch != '\r' )
               string[ cnbr++ ] = ( char ) ch;
         }
      }

      if( cnbr >= LINE_MAX )
      {
         string[ LINE_MAX ] = '\0';
         return TRUE;
      }
   }
}

//----------------------------------------------------------------------------//
static void wsconv( char * ins )
{
   int i;

   for( i = 0; ins[ i ]; i++ )
      if( ins[ i ] < ' ' )
         ins[ i ] = ' ';

}

//----------------------------------------------------------------------------//
static int alltrim( char * ins )
{
   int i, j, k;

   j = ( int ) strlen( ins ) - 1;

   for( i = 0; i < j; i++ )
   {
      if( ins[ i ] == ' ' )
         continue;
      else
         break;
   }

   for(; i < j; j-- )
   {
      if( ins[ j ] == ' ' )
         continue;
      else
         break;
   }

   for( k = 0; k <= j - i; k++ )
      ins[ k ] = ins[ k + i ];

   ins[ k ] = '\0';
   return j - i;
}

//----------------------------------------------------------------------------//
int hbcc_txt2cst( const char * szInputFile )
{
   FILE *      hin;
   FILE *      hout;
   char *      fout;
   char *      fnew;
   int         i, j, k, n;
   PHB_FNAME   pFileName;

   hin = hb_fopen( szInputFile, "r" );

   if( ! hin )
   {
      hb_fsSetFError( 0 );
      hb_errRT_BASE( EG_ARG, 2021, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }

   pFileName               = hb_fsFNameSplit( szInputFile );
   fout                    = ( char * ) hb_xgrab( HB_PATH_MAX );

   pFileName->szExtension  = "cst";
   hb_fsFNameMerge( fout, pFileName );

   hout                    = hb_fopen( fout, "wb" );

   if( ! hout )
   {
      hb_xfree( pFileName );
      hb_xfree( fout );
      hb_fsSetFError( 0 );
      hb_errRT_BASE( EG_ARG, 2021, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }

   fout = ( char * ) hb_xrealloc( fout, MAX_2CHAR );

   for( i = 0; i < MAX_WCHAR; i++ )
      s_tchar[ i ] = s_defchar;

   for( i = 0; i < MAX_2CHAR; i++ )
      s_tuni[ i ] = ( i & 1 ) ? '\0' : s_defchar;

   for( i = 0; i < MAX_CHAR; i++ )
   {
      s_cleads[ i ]  = '\0';
      c_leads[ i ]   = '\0';
   }

   while( fgets( fout, MAX_2CHAR, hin ) != NULL )
   {
      fnew = strchr( fout, '#' );

      if( fnew != NULL )
         fout[ fnew - fout ] = '\0';

      wsconv( fout );

      k = alltrim( fout );

      if( k <= 0 )
         continue;

      if( k > 4 )
         sscanf( fout, "0x%X 0x%X", &i, &j );
      else
         continue;

      if( s_maxchar == 1 )
      {
         if( i > MAX_CHAR )
         {
            s_maxchar = ( char ) 2;
            for( k = MAX_WCHAR; k; k-- )
            {
               s_tchar[ 2 * k - 2 ] = s_tchar[ k - 1 ];
               s_tchar[ 2 * k - 1 ] = '\0';
            }
         }
         else
         {
            s_tuni[ 2 * i ]      = ( char ) ( j & ( MAX_CHAR - 1 ) );
            s_tuni[ 2 * i + 1 ]  = ( char ) ( ( j >> 8 ) & ( MAX_CHAR - 1 ) );
            s_tchar[ j ]         = ( char ) i;
         }
      }
      if( s_maxchar == 2 )
      {
         if( i < MAX_CHAR )
         {
            s_tchar[ 2 * j ]     = ( char ) ( i & ( MAX_CHAR - 1 ) );
            s_tchar[ 2 * j + 1 ] = '\0';
         }
         else
         {
            s_tchar[ 2 * j ]     = ( char ) ( ( i >> 8 ) & ( MAX_CHAR - 1 ) );
            s_tchar[ 2 * j + 1 ] = ( char ) ( i & ( MAX_CHAR - 1 ) );
         }

         if( ( i >> 8 ) & ( MAX_CHAR - 1 ) )
         {
            if( c_leads[ ( i >> 8 ) & ( MAX_CHAR - 1 ) ] )
            {
               fnew  = strchr( s_cleads, ( i >> 8 ) & ( MAX_CHAR - 1 ) );
               k     = ( int ) ( fnew - s_cleads );
            }
            else
            {
               c_leads[ ( i >> 8 ) & ( MAX_CHAR - 1 ) ]  = ( char ) 1;
               k                                         = ( int ) strlen( s_cleads );
               s_cleads[ k ]                             = ( char ) ( ( i >> 8 ) & ( MAX_CHAR - 1 ) );
               s_aleads                                  = ( char * ) hb_xrealloc( s_aleads, ( k + 1 ) * ( MAX_2CHAR ) );

               for( n = 0; n < MAX_CHAR; n++ )
               {
                  s_aleads[ k * MAX_2CHAR + 2 * n ]      = s_defchar;
                  s_aleads[ k * MAX_2CHAR + 2 * n + 1 ]  = '\0';
               }
            }
            s_aleads[ k * MAX_2CHAR + 2 * ( i & ( MAX_CHAR - 1 ) ) ]       = ( char ) ( j & ( MAX_CHAR - 1 ) );
            s_aleads[ k * MAX_2CHAR + 2 * ( i & ( MAX_CHAR - 1 ) ) + 1 ]   = ( char ) ( ( j >> 8 ) & ( MAX_CHAR - 1 ) );
         }
         else
         {
            s_tuni[ 2 * i ]      = ( char ) ( j & ( MAX_CHAR - 1 ) );
            s_tuni[ 2 * i + 1 ]  = ( char ) ( ( j >> 8 ) & ( MAX_CHAR - 1 ) );
         }
      }
   }

   hb_xfree( fout );
   fclose( hin );

   fout = ( char * ) hb_xgrab( MAX_LEADS + 1 );

   for( i = 0; i <= MAX_LEADS; i++ )
      fout[ i ] = '\0';

   j = 0;

   for( i = 0; i < MAX_CHAR; i++ )
   {
      if( c_leads[ i ] )
      {
         if( ( j & 1 ) == 0 )
         {
            fout[ j ] = ( char ) i;
            j++;
         }

         if( c_leads[ i + 1 ] )
            continue;
         else
         {
            fout[ j ] = ( char ) i;
            j++;
         }
      }

      if( j == MAX_LEADS )
         break;
   }

   for( i = 0; i < ( int ) strlen( pFileName->szName ); i++ )
      fputc( pFileName->szName[ i ], hout );

   for(; i < 48; i++ )
      fputc( 0, hout );

   fputc( s_maxchar, hout );
   fputc( s_defchar, hout );
   fputc( 0, hout );
   fputc( 0, hout );

   for( i = 0; i < MAX_LEADS; i++ )
      fputc( fout[ i ], hout );

   fputc( 0, hout );
   fputc( ( int ) strlen( s_cleads ) + 1, hout );

   for( i = 0; i < MAX_2CHAR; i++ )
      fputc( s_tuni[ i ], hout );

   for( k = 0; k < ( int ) strlen( s_cleads ); k++ )
      for( i = 0; i < MAX_2CHAR; i++ )
         fputc( s_aleads[ k * MAX_2CHAR + i ], hout );

   for( i = 0; i < ( ( int ) s_maxchar ) * MAX_WCHAR; i++ )
      fputc( s_tchar[ i ], hout );

   fclose( hout );

   if( fout )
      hb_xfree( fout );

   if( s_aleads )
      hb_xfree( s_aleads );

   if( pFileName )
      hb_xfree( pFileName );

   return 0;
}

//----------------------------------------------------------------------------//
HB_FUNC( HB_TXT2CST )
{
   hb_retni( ISCHAR( 1 ) ? hbcc_txt2cst( hb_parcx( 1 ) ) : -1 );
}
