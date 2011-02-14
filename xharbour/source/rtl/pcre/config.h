/*
 * $Id$
 */
#ifndef _CONFIG_H
   #define _CONFIG_H
   
   #include "hbsetup.h"

   #define SUPPORT_UTF8
   #define PCRE_STATIC

   #if defined( _MSC_VER )
      #pragma warning( disable: 4018 )
      #pragma warning( disable: 4065 )
   #endif

   #if defined( __BORLANDC__ )
      #pragma warn -use
      #pragma warn -csu
      #pragma warn -aus
      #pragma warn -sig
   #endif

   #include "config.h.generic"
#endif
