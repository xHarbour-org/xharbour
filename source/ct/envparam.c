/*
 * $Id$
 */
/*
 * Harbour Project source code:
 *   CT3 functions: ENVPARAM()
 *
 * Copyright 2007 Pavel Tsarenko <tpe2@mail.ru>
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

#include "hbapi.h"

#ifdef HB_OS_WIN
#include <windows.h>
#endif

#define CRLF "\x0D\x0A"

HB_FUNC( ENVPARAM )
{
#if defined( HB_OS_DOS )
   {
      extern char ** _environ;
      char *         buffer   = NULL;
      int            x;
      HB_SIZE        buffsize = 0;

      // scan strings first and add up total size
      for( x = 0;; x++ )
      {
         if( ! _environ[ x ] )
            break;
         buffsize += ( strlen( _environ[ x ] ) + 2 );
      }

      buffer      = ( char * ) hb_xalloc( buffsize + 1 );
      buffer[ 0 ] = 0;

      for( x = 0;; x++ )
      {
         if( ! _environ[ x ] )
            break;

         hb_xstrcat( buffer, _environ[ x ], CRLF, 0 );
      }

      buffer[ buffsize ] = 0;
      hb_retclenAdopt( buffer, buffsize );

   }
#elif defined( HB_OS_WIN )
   {
      char *   buffer;
      LPVOID   lpEnviron   = GetEnvironmentStrings();
      char *   sCurEnv;
      HB_SIZE  buffsize    = 0;

      // scan strings first and add up total size
      for( sCurEnv = ( LPTSTR ) lpEnviron; *sCurEnv; sCurEnv++ )
      {
         buffsize += ( strlen( ( char * ) sCurEnv ) + 2 );
         while( *sCurEnv )
            sCurEnv++;
      }

      buffer      = ( char * ) hb_xalloc( buffsize + 1 );
      buffer[ 0 ] = 0;

      for( sCurEnv = ( LPTSTR ) lpEnviron; *sCurEnv; sCurEnv++ )
      {

         hb_xstrcat( buffer, ( char * ) sCurEnv, CRLF, 0 );

         while( *sCurEnv )
            sCurEnv++;
      }

      FreeEnvironmentStrings( ( LPCH ) lpEnviron );

      buffer[ buffsize ] = 0;
      hb_retclenAdopt( buffer, buffsize );
   }

#endif
}
