/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Internationalization routines
 *
 * Copyright 2003 Giancarlo Niccolai <giancarlo@niccolai.ws>
 * www - http://www.xharbour.org
 * SEE ALSO COPYRIGHT NOTICE FOR NXS BELOW.
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

#define HB_THREAD_OPTIMIZE_STACK

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "stdio.h"
#include "hbi18n.h"

static char ***s_i18n_table;
int s_i18n_count;

BOOL hb_i18n_read_table( FILE *fp )
{
   HB_I18N_TAB_HEADER header;
   int nStrLen, nRead;
   int i,j;

   nRead = fread( &header, 1, sizeof( header ), fp );
   if ( nRead != sizeof( header ) )
   {
      return FALSE;
   }

   s_i18n_count = header.entries;
   s_i18n_table = ( char ***) hb_xgrab( header.entries * sizeof( char *** ) );

   for ( i = 0; i < s_i18n_count; i ++ )
   {
      s_i18n_table[ i ] = ( char **) hb_xgrab( 2 * sizeof( char * ) );

      nRead = fread( &nStrLen, 1, sizeof( nStrLen ), fp );
      if ( nRead != sizeof( nStrLen ) )
      {
         return FALSE;
      }

      for ( j = 0; j < 2 ; j ++ )
      {
         // zero is included
         s_i18n_table[ i ][j] = ( char * ) hb_xgrab( nStrLen );

         nRead = fread( s_i18n_table[ i ][j], 1, nStrLen, fp );

         // using trailing zero as file integrity check
         if ( nRead != nStrLen || s_i18n_table[ i ][j][nStrLen] != 0 )
         {
            return FALSE;
         }
      }
   }

   return TRUE;
}

char *hb_i18n( char *cInt )
{

   if ( s_i18n_table == NULL )
   {
      return cInt;
   }

   return hb_i18n_scan_table( cInt );
}


char *hb_i18n_scan_table( char *cInt )
{
   int iPoint = s_i18n_count / 2;
   int iLower = 0;
   int iHigher = s_i18n_count;
   int iRes;

   while ( 1 )
   {
      iRes = strcmp( s_i18n_table[ iPoint ][0], cInt );

      if ( iRes == 0 )
      {
         return s_i18n_table[ iPoint ][1];
      }
      else {
         if ( iLower == iHigher )
         {
            break;
         }

         if ( iRes < 0 )
         {
            iHigher = iPoint;
            iPoint = ( iLower + iHigher ) / 2;
         }
         else
         {
            iLower = iPoint;
            iPoint = ( iLower + iHigher ) / 2;
         }
      }
   }

   // entry not found
   return cInt;
}


HB_FUNC( I18N )
{
   HB_THREAD_STUB

   PHB_ITEM pStr = hb_param( 1, HB_IT_STRING );

   if ( pStr == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, "I18N must be called on a string", NULL,
         1, hb_param( 1, HB_IT_ANY ) );
      return;
   }

   if ( s_i18n_table == NULL )
   {
      hb_itemCopy( &( HB_VM_STACK.Return), pStr );
      return;
   }

   hb_retc( hb_i18n_scan_table( hb_itemGetC( pStr ) ) );
}



/***********************************************
* VM interface
************************************************/

BOOL hb_i18nInit( char *i18n_dir, char *language )
{
   char *path;
   FILE *fp;
   BOOL ret = FALSE;

   if ( language == NULL )
   {
      language = getenv( "LC_LANGUAGE" );
      if ( language == NULL )
      {
         language = getenv( "LC_ALL" );
      }
   }

   /* No automatic internationalization can be found */
   if ( language == NULL )
   {
      s_i18n_table = NULL;
      // but we know that we don't want internationalization
      return TRUE;
   }

   // path? (if null, it is i18n/ subdir)
   if ( i18n_dir == NULL )
   {
      i18n_dir = HB_DEFAULT_I18N_PATH;
   }

   if ( strlen( i18n_dir ) > 0 )
   {
      path = hb_xgrab(
         strlen( i18n_dir ) +
         strlen( language ) +
         strlen( HB_I18N_TAB_EXT) + 3 ); // '/', dot and '\0'

      sprintf( path, "%s%c%s.%s",
            i18n_dir,
            OS_PATH_DELIMITER,
            language,
            HB_I18N_TAB_EXT );
   }
   else
   {
      path = hb_xgrab(
         strlen( language ) +
         strlen( HB_I18N_TAB_EXT) + 2 ); // dot and '\0'

      sprintf( path, "%s.%s",
            language,
            HB_I18N_TAB_EXT );
   }

   if ( ( fp = fopen( path, "r" ) ) != NULL )
   {
      ret = hb_i18n_read_table( fp );
      fclose( fp );
      // warning: deliberately not deallocating the memory held
      if ( ! ret || ferror( fp ) )
      {
         s_i18n_table = NULL;
         ret = FALSE;
      }
   }

   hb_xfree( path );
   return ret;
}

void hb_i18nExit( void )
{
   int i;

   if ( s_i18n_table != NULL )
   {
      for ( i = 0; i < s_i18n_count; i ++ )
      {
         hb_xfree( s_i18n_table[ i ][0] );
         hb_xfree( s_i18n_table[ i ][1] );
         hb_xfree( s_i18n_table[ i ] );
      }
      hb_xfree( s_i18n_table );
   }
}


