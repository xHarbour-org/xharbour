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

#endif
