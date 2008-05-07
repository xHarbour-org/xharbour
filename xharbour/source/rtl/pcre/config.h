/*
 * $Id: config.h,v 1.9 2008/02/01 04:57:37 andijahja Exp $
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
