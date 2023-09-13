/*
 * SQLRDD - Harbour/xHarbour compatibility definitions
 * (c) copyright xHarbour.com Inc. http://www.xHarbour.com
 * Author: Przemyslaw Czerpak (druzus/at/poczta.onet.pl)
 *
 * This source file is an intellectual property of xHarbour.com Inc.
 * You may NOT forward or share this file under any conditions!
 */

#ifndef _SQL_RDD_COMPAT_H
#define _SQL_RDD_COMPAT_H

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapirdd.h"
#include "hbapierr.h"
#include "hbset.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbdate.h"

#ifdef __XHARBOUR__

   #include "hbfast.h"
   #include "hashapi.h"

   #ifndef hb_itemPutCLPtr
      #define hb_itemPutCLPtr( i, s, l )           hb_itemPutCPtr( i, s, l )
   #endif
   #define hb_vmCDP()                           hb_cdppage()

//    #define HB_FALSE           0
//    #define HB_TRUE            (!0)

//   #define HB_ISLOG(x)        ISLOG(x)
//    #define HB_ISNUM(x)        ISNUM(x)
   #ifdef SQLRDD_COMPAT_PRE_1_1
   typedef ULONG              HB_SIZE;
   #endif
   typedef BOOL               HB_BOOL;
   #ifdef SQLRDD_COMPAT_PRE_1_1
   typedef HB_LONG            HB_MAXINT;
   typedef HB_ULONG           HB_MAXUINT;
   #endif

   // Support for old xHarbour versions
   #ifndef HB_FT_STRING
      #define HB_FT_STRING HB_IT_STRING
   #endif
   #ifndef HB_FT_MEMO
      #define HB_FT_MEMO HB_IT_MEMO
   #endif
   #ifndef HB_FT_LONG
      #define HB_FT_LONG HB_IT_LONG
   #endif
   #ifndef HB_FT_LOGICAL
      #define HB_FT_LOGICAL HB_IT_LOGICAL
   #endif
   #ifndef HB_FT_DATE
      #define HB_FT_DATE HB_IT_DATE
   #endif
   #ifndef HB_FT_DATETIME
      #define HB_FT_DATETIME HB_IT_DATETIME
   #endif

   #ifdef SQLRDD_COMPAT_PRE_1_1
      #ifndef HB_RDD_MAX_ALIAS_LEN
         #define HB_RDD_MAX_ALIAS_LEN     32
      #endif
      #define hb_setGetDeleted()    hb_set.HB_SET_DELETED
      #define hb_setGetAutOpen()    hb_set.HB_SET_AUTOPEN
      #define hb_cdppage()          hb_cdp_page
      #define HB_ERRCODE            ERRCODE
      #define HB_SUCCESS            SUCCESS
      #define HB_FAILURE            FAILURE
   #endif

#else

   #include "hbapicls.h"
   #define HB_DATETIMEINSEC    1000
   #define HB_DATETIMEDECIMALS 3

   HB_EXTERN_BEGIN

   extern void TraceLog( const char * sFile, const char * sTraceMsg, ... );
   extern HB_EXPORT long     hb_timeEncStr( const char * szTime );  /* Hecho */
   extern HB_EXPORT char *   hb_timeDecStr( char * szTime, long lSeconds );  /* Hecho */

   HB_EXTERN_END

   typedef HB_BYTE            BYTE;
   typedef HB_SCHAR           SCHAR;
   typedef HB_SHORT           SHORT;
   typedef HB_USHORT          USHORT;
   typedef HB_UINT            UINT;
   typedef HB_LONG            LONG;
   typedef HB_ULONG           ULONG;
   typedef HB_BOOL            BOOL;
   typedef HB_LONGLONG        LONGLONG;

   #define EVALINFO           HB_EVALINFO

   #ifndef FALSE
      #define FALSE           0
   #endif
   #ifndef TRUE
      #define TRUE               (!0)
   #endif

   #define ISBYREF(x)         HB_ISBYREF(x)
   #define ISCHAR(x)          HB_ISCHAR(x)
   #define ISNUM(x)           HB_ISNUM(x)
   #define ISLOG(x)           HB_ISLOG(x)

   #define hb_retcAdopt( szText )               hb_retc_buffer( (szText) )
   #define hb_retclenAdopt( szText, ulLen )     hb_retclen_buffer( (szText), (ulLen) )
   #define hb_retcStatic( szText )              hb_retc_const( (szText) )
   #define hb_storclenAdopt                     hb_storclen_buffer
   #define hb_itemPutCRawStatic                 hb_itemPutCLConst
   #define hb_itemForwardValue( dst, src )      hb_itemMove( dst, src )
   #define hb_cdppage()                         hb_vmCDP()

   #define hb_dynsymLock()
   #define hb_dynsymUnlock()

   #define hb_stordtl hb_stortdt
   #define hb_dateTimeStampStrGet hb_timeStampStrGetDT
#endif

#endif /* _SQL_RDD_COMPAT_H */
