/*
 * SQLRDD - Harbour/xHarbour compatibility functions
 * (c) copyright xHarbour.com Inc. http://www.xHarbour.com
 * Author: Przemyslaw Czerpak (druzus/at/poczta.onet.pl)
 *
 * This source file is an intellectual property of xHarbour.com Inc.
 * You may NOT forward or share this file under any conditions!
 */


#include "compat.h"

#ifndef __XHARBOUR__

void TraceLog( const char * szFile, const char * szTraceMsg, ... )
{
   if( szTraceMsg )
   {
      FILE * hFile = hb_fopen( szFile ? szFile : "trace.log", "a" );
      if( hFile )
      {
         va_list ap;

         va_start( ap, szTraceMsg );
         vfprintf( hFile, szTraceMsg, ap );
         va_end( ap );

         fclose( hFile );
      }
   }
}

long hb_timeEncStr( const char * szTime )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_timeEncStr(%s)", szTime ) );

   if( szTime )
   {
      int ulLen = ( int ) strlen( szTime );

      if( ulLen >= 4 )
      {
         return ( long ) hb_strVal( szTime, 2 ) * 3600 * HB_DATETIMEINSEC +
                ( long ) hb_strVal( szTime + 2, 2 ) * 60 * HB_DATETIMEINSEC +
                ( long ) ( hb_strVal( szTime + 4, ulLen - 4 ) * HB_DATETIMEINSEC );
      }
   }

   return 0;
}

char * hb_timeDecStr( char * szTime, long lSeconds )
{
   int iHour, iMinute, iSeconds, iMillisec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_timeDecStr(%s,%f)", szTime, lSeconds ) );

   hb_timeDecode( lSeconds, &iHour, &iMinute, &iSeconds, &iMillisec );

   hb_snprintf( szTime, 8 + HB_DATETIMEDECIMALS, "%02d%02d%0*.*f", iHour, iMinute, HB_DATETIMEDECIMALS + 3, HB_DATETIMEDECIMALS, (double)iSeconds );

   return szTime;
}

#endif
