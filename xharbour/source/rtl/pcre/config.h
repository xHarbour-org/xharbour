/*
 * $Id: config.h,v 1.8 2008/01/16 05:17:32 andijahja Exp $
 */

#ifndef _CONFIG_H
   #define _CONFIG_H

   #define PCRE_STATIC

   #if defined( _MSC_VER )
      #pragma warning( push, 0 )
   #endif

   #if defined( __BORLANDC__ )
      #pragma warn -use
      #pragma warn -csu
      #pragma warn -aus
   #endif

   #include "config.h.generic"
#endif
