/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HBSYSLOG() function
 *
 * Copyright 2004 Giancarlo Niccolai <gc -at- niccolai [dot] ws>
 *
 * www - http://www.xharbour.org
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
   Event logging system
 */

#include "hbapi.h"
#include "hblogdef.ch"

#if defined( HB_OS_WIN )
#include "windows.h"

static HANDLE s_RegHandle;

/* Determines if this system is an NT, 2000 or XP windows */
static BOOL s_IsWindowsNt( void )
{
   OSVERSIONINFO osVer;

   osVer.dwOSVersionInfoSize = sizeof( osVer );
   if( GetVersionEx( &osVer ) && osVer.dwPlatformId == VER_PLATFORM_WIN32_NT )
   {
      return TRUE;
   }
   return FALSE;
}

#elif ( defined( HB_OS_UNIX ) || defined( HB_OS_LINUX ) ) && ! defined( __WATCOMC__ )

#include <syslog.h>

#endif

HB_FUNC( HB_SYSLOGOPEN )
{
   #if defined( HB_OS_WIN )
      #if ( WINVER >= 0x0400 )
   //Ok, we compiled under NT, but we must not use this function
   // when RUNNING on a win98.
   if( s_IsWindowsNt() )
   {
      s_RegHandle = RegisterEventSource( NULL, ( LPCTSTR ) hb_parcx( 1 ) );
      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
      #else
   s_RegHandle = NULL;
   hb_retl( FALSE );
      #endif
   #elif defined( HB_OS_UNIX ) && ! defined( __WATCOMC__ )
   openlog( hb_parcx( 1 ), LOG_NDELAY | LOG_NOWAIT | LOG_PID, LOG_USER );
   hb_retl( TRUE );
   #else
   hb_retl( FALSE );
   #endif
}

HB_FUNC( HB_SYSLOGCLOSE )
{
   #if defined( HB_OS_WIN )
      #if ( WINVER >= 0x0400 )
   if( s_IsWindowsNt() )
   {
      DeregisterEventSource( s_RegHandle );
      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
      #else
   hb_retl( FALSE );
      #endif
   #elif defined( HB_OS_UNIX ) && ! defined( __WATCOMC__ )
   closelog();
   hb_retl( TRUE );
   #else
   hb_retl( FALSE );
   #endif
}

HB_FUNC( HB_SYSLOGMESSAGE )
{
   #if defined( HB_OS_WIN )
      #if ( WINVER >= 0x0400 )
   WORD logval;
   if( s_IsWindowsNt() )
   {
      const char * szMsg = hb_parcx( 1 );
      switch( hb_parni( 2 ) )
      {
         case HB_LOG_CRITICAL: logval  = EVENTLOG_ERROR_TYPE; break;
         case HB_LOG_ERROR: logval     = EVENTLOG_ERROR_TYPE; break;
         case HB_LOG_WARN: logval      = EVENTLOG_WARNING_TYPE; break;
         case HB_LOG_INFO: logval      = EVENTLOG_INFORMATION_TYPE; break;
         default:
            logval                     = EVENTLOG_AUDIT_SUCCESS;
      }
      if( ReportEvent( s_RegHandle,             // event log handle
                       logval,                  // event type
                       0,                       // category zero
                       ( DWORD ) hb_parnl( 3 ), // event identifier
                       NULL,                    // no user security identifier
                       1,                       // one substitution string
                       0,                       // no data
                       ( LPCTSTR * ) &szMsg,    // pointer to string array
                       NULL ) )                 // pointer to data
      {
         hb_retl( TRUE );
      }
      else
      {
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_retl( FALSE );
   }

      #else
   hb_retl( FALSE );
      #endif
   #elif defined( HB_OS_UNIX ) && ! defined( __WATCOMC__ )
   int logval;

   switch( hb_parni( 2 ) )
   {
      case HB_LOG_CRITICAL: logval  = LOG_CRIT; break;
      case HB_LOG_ERROR: logval     = LOG_ERR; break;
      case HB_LOG_WARN: logval      = LOG_WARNING; break;
      case HB_LOG_INFO: logval      = LOG_INFO; break;
      default:
         logval                     = LOG_DEBUG;
   }

   syslog( logval, "[%lX]: %s", hb_parnl( 3 ), hb_parcx( 1 ) );
   hb_retl( TRUE );
   #else
   hb_retl( FALSE );
   #endif
}
