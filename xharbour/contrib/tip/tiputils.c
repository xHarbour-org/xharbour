/**********************************************
* tiputils.c
*
* Class oriented Internet protocol library
*
* (C) 2002 Giancarlo Niccolai
* $Id: tiputils.c,v 1.4 2003/12/03 15:07:05 jonnymind Exp $
************************************************/

#include "hbapi.h"
#include "hbdate.h"

#ifndef HB_OS_WIN_32
   #include <time.h>
#endif

#if defined _MSC_VER || defined __BORLANDC__
   #include <windows.h>
#endif

/************************************************************
* Useful internet timestamp based on RFC822
*/

/* sadly, many strftime windows implementations are broken */
#ifdef HB_OS_WIN_32

HB_FUNC( TIP_TIMESTAMP )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );
   ULONG ulHour = hb_parl(2);
   int nLen;
   TIME_ZONE_INFORMATION tzInfo;
   long lDate, lYear, lMonth, lDay;
   char *days[] = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
   char *months[] = {
         "Jan", "Feb", "Mar",
         "Apr", "May", "Jun",
         "Jul", "Aug", "Sep",
         "Oct", "Nov", "Dec" };
   char *szRet = (char *) hb_xgrab( 64 );
   SYSTEMTIME st;


   if ( !ulHour )
   {
      ulHour = 0;
   }

   if ( GetTimeZoneInformation( &tzInfo ) == TIME_ZONE_ID_INVALID )
   {
      tzInfo.Bias = 0;
   }
   else
   {
      tzInfo.Bias -= tzInfo.Bias;
   }

   if ( !pDate )
   {
      GetLocalTime( &st );

      sprintf( szRet, "%s, %d %s %d %02d:%02d:%02d %+03d%02d",
            days[ st.wDayOfWeek ], st.wDay, months[ st.wMonth -1],
            st.wYear,
            st.wHour, st.wMinute, st.wSecond,
            tzInfo.Bias/60,
            tzInfo.Bias % 60 > 0 ? - tzInfo.Bias % 60 : tzInfo.Bias % 60 );
   }
   else
   {
      lDate = hb_itemGetDL( pDate );
      hb_dateDecode( lDate, &lYear, &lMonth, &lDay );

      sprintf( szRet, "%s, %d %s %d %02d:%02d:%02d %+03d%02d",
            days[ hb_dateDOW( lYear, lMonth, lDay ) - 1 ], lDay,
            months[ lMonth -1], lYear,
            ulHour / 3600, (ulHour % 3600) / 60, (ulHour % 60),
            tzInfo.Bias/60,
            tzInfo.Bias % 60 > 0 ? - tzInfo.Bias % 60 : tzInfo.Bias % 60 );
   }


   nLen = strlen( szRet );

   if ( nLen < 64 )
   {
      szRet = (char *) hb_xrealloc( szRet, nLen + 1 );
   }
   hb_retclenAdoptRaw( szRet, nLen );

}

#else

HB_FUNC( TIP_TIMESTAMP )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );
   ULONG ulHour = hb_parl(2);
   int nLen;
   char szDate[9];
   struct tm tmTime;
   time_t current;

   char *szRet = (char *) hb_xgrab( 64 );

   if ( !ulHour )
   {
      ulHour = 0;
   }


   /* init time structure anyway */
   time( &current );
   #ifdef HB_OS_OS2
   memcpy((void *)&tmTime, (void *)localtime( &current ), sizeof(tmTime));
   #else
   localtime_r( &current , &tmTime );
   #endif

   if ( pDate )
   {
      hb_itemGetDS( pDate, szDate );

      tmTime.tm_year = (
         (szDate[0] - '0') * 1000 +
         (szDate[1] - '0') * 100 +
         (szDate[2] - '0') * 10 +
         (szDate[3] - '0') ) -1900;

      tmTime.tm_mon = (
         (szDate[4] - '0') * 10 +
         (szDate[5] - '0') ) -1;

      tmTime.tm_mday =
         (szDate[6] - '0') * 10 +
         (szDate[7] - '0');

      tmTime.tm_hour = ulHour / 3600;
      tmTime.tm_min = (ulHour % 3600) / 60;
      tmTime.tm_sec = (ulHour % 60);
   }

   nLen = strftime( szRet, 64, "%a, %d %b %Y %H:%M:%S %z", &tmTime );

   if ( nLen < 64 )
   {
      szRet = (char *) hb_xrealloc( szRet, nLen + 1 );
   }
   hb_retclenAdoptRaw( szRet, nLen );
}

#endif
