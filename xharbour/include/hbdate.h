/*
 * $Id: hbdate.h,v 1.10 2007/10/31 08:34:22 marchuet Exp $
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

extern HB_EXPORT double hb_dateSeconds( void );
extern HB_EXPORT double hb_secondsCPU( int n );
extern HB_EXPORT void   hb_dateTimeStamp( LONG * plJulian, LONG * plMilliSec );
extern HB_EXPORT char * hb_timeStampStr( char * szTime, LONG lMillisec );
extern HB_EXPORT void   hb_timeStampDecode( LONG lMillisec, int * piHour, int * piMinutes, int * piSeconds, int * piMSec );
extern HB_EXPORT char * hb_dateTimeStampStr( char * szDateTime, LONG lJulian, LONG lMillisec );
extern HB_EXPORT void   hb_dateTimeStampStrGet( const char * szDateTime, LONG * plJulian, LONG * plMillisec );
extern HB_EXPORT void   hb_dateToday( int * piYear, int * piMonth, int * piDay );
extern HB_EXPORT void   hb_dateTimeStr( char * pszTime );
extern HB_EXPORT void   hb_dateTime( int * piHour, int * piMinute, double * pdSeconds );
extern HB_EXPORT char * hb_dateCMonth( int iMonth );
extern HB_EXPORT char * hb_dateCDOW( int iDay );
extern HB_EXPORT int    hb_dateDOW( int iYear, int iMonth, int iDay );
extern HB_EXPORT int    hb_dateJulianDOW( LONG lJulian );
extern HB_EXPORT char * hb_dateFormat( const char * szDate, char * szFormattedDate, const char * szDateFormat );
extern HB_EXPORT char * hb_timeFormat( const char * szDate, char * szFormattedTime, const char * szTimeFormat );
extern HB_EXPORT char * hb_datetimeFormat( const char * szDateTime, char * szFormattedDateTime, const char * szDateFormat, const char * szTimeFormat );
extern HB_EXPORT LONG   hb_dateEncode( int iYear, int iMonth, int iDay );
extern HB_EXPORT void   hb_dateDecode( LONG julian, int * piYear, int * piMonth, int * piDay );
extern HB_EXPORT void   hb_dateStrPut( char * szDate, int iYear, int iMonth, int iDay );
extern HB_EXPORT void   hb_dateStrGet( const char * szDate, int * piYear, int * piMonth, int * piDay );
extern HB_EXPORT char * hb_dateDecStr( char * szDate, LONG lJulian );
extern HB_EXPORT LONG   hb_dateEncStr( const char * szDate );

extern HB_EXPORT LONG   hb_timeEncStr( const char * szTime );  /* Hecho */
extern HB_EXPORT char * hb_timeDecStr( char * szTime, LONG lSeconds );  /* Hecho */

#define  hb_timeL2Sec( lTime )   ((double) lTime / (double)HB_DATETIMEINSEC)

extern HB_EXPORT LONG     hb_timeEncode( int iHour, int iMinute, double dSeconds );  /* Hecho */
extern HB_EXPORT void     hb_timeDecode( LONG lTime, int * piHour, int * piMinute, double * pdSeconds );  /* Hecho */
extern HB_EXPORT double   hb_timeEncodeSec( int iHour, int iMinute, double dSeconds );  /* Hecho */
extern HB_EXPORT void     hb_timeDecodeSec( double dTime, int * piHour, int * piMinute, double * pdSeconds );  /* Hecho */

extern HB_EXPORT void     hb_datetimeEncode( LONG * plDate, LONG * plTime, int iYear, int iMonth, int iDay, int iHour, int iMinute, double dSeconds, int iAmPm, int * iOk );  /* Hecho */
extern HB_EXPORT void     hb_datetimeDecode( LONG lDate, LONG lTime, int * piYear, int * piMonth, int * piDay, int * piHour, int * piMinute, double * pdSeconds );  /* Hecho */

extern HB_EXPORT void     hb_datetimeEncStr( const char * szDateTime, LONG *plDate, LONG *plTime );
extern HB_EXPORT char     * hb_datetimeDecStr( char * szDateTime, LONG lDate, LONG lTime );

extern HB_EXPORT void     hb_datetimeUnpack( double dDateTime, LONG * plDate, LONG * plTime );
extern HB_EXPORT double   hb_datetimePack( LONG lDate, LONG lTime );
extern HB_EXPORT double   hb_datetimePackInSec( LONG lJulian, LONG lTime );

HB_EXTERN_END

#endif /* HB_DATE_H_ */
