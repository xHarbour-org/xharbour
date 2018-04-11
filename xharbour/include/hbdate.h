/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Date API
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

#ifndef HB_DATE_H_
#define HB_DATE_H_

#include "hbsetup.h"

HB_EXTERN_BEGIN

extern HB_EXPORT double   hb_dateSeconds( void );
extern HB_EXPORT HB_ULONG hb_dateMilliSeconds( void );
extern HB_EXPORT double   hb_secondsCPU( int n );
extern HB_EXPORT void     hb_dateTimeStamp( long * plJulian, long * plMilliSec );
extern HB_EXPORT char *   hb_timeStampStr( char * szTime, long lMillisec );
extern HB_EXPORT void     hb_dbaselockEncode( char * pszTimeDate );
extern HB_EXPORT long     hb_timeStampEncode( int iHour, int iMinutes, int iSeconds, int iMSec );
extern HB_EXPORT void     hb_timeStampDecode( long lMillisec, int * piHour, int * piMinutes, int * piSeconds, int * piMSec );
extern HB_EXPORT void     hb_timeStrGet( const char * szTime, int * piHour, int * piMinutes, int * piSeconds, int * piMSec );
extern HB_EXPORT char *   hb_dateTimeStampStr( char * szDateTime, long lJulian, long lMillisec );
extern HB_EXPORT void     hb_dateTimeStampStrGet( const char * szDateTime, long * plJulian, long * plMillisec );
extern HB_EXPORT void     hb_dateToday( int * piYear, int * piMonth, int * piDay );
extern HB_EXPORT void     hb_dateTimeStr( char * pszTime );
extern HB_EXPORT void     hb_dateTime( int * piHour, int * piMinute, double * pdSeconds );
extern HB_EXPORT char *   hb_dateCMonth( int iMonth );
extern HB_EXPORT char *   hb_dateCDOW( int iDay );
extern HB_EXPORT int      hb_dateDOW( int iYear, int iMonth, int iDay );
extern HB_EXPORT int      hb_dateJulianDOW( long lJulian );
extern HB_EXPORT char *   hb_dateFormat( const char * szDate, char * szFormattedDate, const char * szDateFormat );
extern HB_EXPORT char *   hb_timeFormat( const char * szDate, char * szFormattedTime, const char * szTimeFormat );
extern HB_EXPORT char *   hb_datetimeFormat( const char * szDateTime, char * szFormattedDateTime, const char * szDateFormat, const char * szTimeFormat );
extern HB_EXPORT long     hb_dateEncode( int iYear, int iMonth, int iDay );
extern HB_EXPORT void     hb_dateDecode( long julian, int * piYear, int * piMonth, int * piDay );
extern HB_EXPORT void     hb_dateStrPut( char * szDate, int iYear, int iMonth, int iDay );
extern HB_EXPORT void     hb_dateStrGet( const char * szDate, int * piYear, int * piMonth, int * piDay );
extern HB_EXPORT char *   hb_dateDecStr( char * szDate, long lJulian );
extern HB_EXPORT long     hb_dateEncStr( const char * szDate );

extern HB_EXPORT long     hb_timeEncStr( const char * szTime );  /* Hecho */
extern HB_EXPORT char *   hb_timeDecStr( char * szTime, long lSeconds );  /* Hecho */

#define  hb_timeL2Sec( lTime )   ((double) lTime / (double)HB_DATETIMEINSEC)

extern HB_EXPORT long     hb_timeEncode( int iHour, int iMinute, double dSeconds );  /* Hecho */
extern HB_EXPORT void     hb_timeDecode( long lTime, int * piHour, int * piMinute, double * pdSeconds );  /* Hecho */
extern HB_EXPORT double   hb_timeEncodeSec( int iHour, int iMinute, double dSeconds );  /* Hecho */
extern HB_EXPORT void     hb_timeDecodeSec( double dTime, int * piHour, int * piMinute, double * pdSeconds );  /* Hecho */
extern HB_EXPORT void     hb_timeStrGet( const char * szTime, int * piHour, int * piMinutes, int * piSeconds, int * piMSec );

extern HB_EXPORT void     hb_datetimeEncode( long * plDate, long * plTime, int iYear, int iMonth, int iDay, int iHour, int iMinute, double dSeconds, int iAmPm, int * iOk );  /* Hecho */
extern HB_EXPORT void     hb_datetimeDecode( long lDate, long lTime, int * piYear, int * piMonth, int * piDay, int * piHour, int * piMinute, double * pdSeconds );  /* Hecho */

extern HB_EXPORT void     hb_datetimeEncStr( const char * szDateTime, long *plDate, long *plTime );
extern HB_EXPORT char *   hb_datetimeDecStr( char * szDateTime, long lDate, long lTime );

extern HB_EXPORT void     hb_datetimeUnpack( double dDateTime, long * plDate, long * plTime );
extern HB_EXPORT double   hb_datetimePack( long lDate, long lTime );
extern HB_EXPORT double   hb_datetimePackInSec( long lJulian, long lTime );

extern HB_EXPORT void     hb_timeStampUnpackDT( double dTimeStamp, long * plJulian, long * plMilliSec );
extern HB_EXPORT void     hb_timeStampGetLocal( int * piYear, int * piMonth, int * piDay,int * piHour, int * piMinutes,int * piSeconds, int * piMSec );
extern HB_EXPORT void     hb_timeStampGet( long * plJulian, long * plMilliSec );

extern HB_EXPORT HB_MAXUINT hb_timerGet( void );
extern HB_EXPORT HB_MAXUINT hb_timerInit( HB_MAXINT nTimeOut );
extern HB_EXPORT HB_MAXINT  hb_timerTest( HB_MAXINT nTimeOut, HB_MAXUINT * pnTimer );

HB_EXTERN_END

#define HB_MINUTES_PER_DAY    ( 24 * 60 )
#define HB_SECONDS_PER_DAY    ( HB_MINUTES_PER_DAY * 60 )
#define HB_MILLISECS_PER_DAY  ( HB_SECONDS_PER_DAY * 1000 )
#define HB_TIMEDIFF_DEC       6

#if ( defined( _POSIX_C_SOURCE ) || defined( _XOPEN_SOURCE ) || \
      defined( _BSD_SOURCE ) || defined( _SVID_SOURCE ) || \
      defined( HB_OS_SUNOS ) || defined( HB_OS_BEOS ) || \
      defined( HB_OS_ANDROID ) ) && \
   ! defined( HB_OS_DARWIN_5 ) && ! defined( HB_HAS_LOCALTIME_R )
#  define HB_HAS_LOCALTIME_R
#endif

#endif /* HB_DATE_H_ */
