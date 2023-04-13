/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Clipper tracing API.
 *
 * Copyright 1999 Gonzalo A. Diethelm <gonzalo.diethelm@iname.com>
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

#include "hbtrace.h"
#include "hbapi.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "thread.h"
#include "hbset.h"

#ifdef HB_EXTENSION

// It is Thread Safe - It's manipulated only once at primary thread startup.
static BOOL s_bDoInit = TRUE;

void hb_traceInit( void )
{
   FILE *   fpTrace;
   PHB_DYNS pTraceLog = hb_dynsymFind( "TRACELOG" );

   if( s_bDoInit && pTraceLog && pTraceLog->pSymbol->value.pFunPtr && hb_fsFile( hb_setGetTraceFile() ) )
   {
      s_bDoInit = FALSE;

      // Might have been initialized if TraceLog() was called before traceInit!
      hb_threadLockInit( S_TRACEMTX );

      /* Empty the file if it exists. */
      fpTrace = hb_fopen( hb_setGetTraceFile(), "w" );

      if( fpTrace )
         fclose( fpTrace );
   }
}

void hb_traceExit( void )
{
   hb_threadLockDestroy( S_TRACEMTX );
}

void TraceLog( const char * sFile, const char * sTraceMsg, ... )
{
   FILE *   hFile;
   BOOL     bEmpty = FALSE;

   if( ! sTraceMsg )
      return;

   if( s_bDoInit )
   {
      s_bDoInit   = FALSE;
      bEmpty      = TRUE;

      hb_threadLockInit( S_TRACEMTX );
   }

   hb_threadLock( S_TRACEMTX );

   if( sFile == NULL )
   {
      if( bEmpty )
         /* Empty the file if it exists. */
         hFile = hb_fopen( hb_setGetTraceFile(), "w" );
      else
         hFile = hb_fopen( hb_setGetTraceFile(), "a" );
   }
   else
      hFile = hb_fopen( sFile, "a" );

   if( hFile )
   {
      va_list ap;

      va_start( ap, sTraceMsg );
      vfprintf( hFile, sTraceMsg, ap );
      va_end( ap );

      fclose( hFile );
   }

   hb_threadUnLock( S_TRACEMTX );
}

HB_FUNC( HB_TRACESTATE )
{
   hb_retni( hb_tracestate( ISNUM( 1 ) ? hb_parni( 1 ) : -1 ) );
}

HB_FUNC( HB_TRACELEVEL )
{
   hb_retni( hb_tracelevel( ISNUM( 1 ) ? hb_parni( 1 ) : -1 ) );
}

HB_FUNC( HB_TRACESTRING )
{
   HB_TRACE( HB_TR_ALWAYS, ( hb_parcx( 1 ) ) );
}

#endif

