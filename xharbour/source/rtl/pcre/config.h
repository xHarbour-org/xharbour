/*
 * $Id$
 */
#ifndef _CONFIG_H
   #define _CONFIG_H

   #if defined( _MSC_VER )
      #pragma warning( disable: 4018 )
      #pragma warning( disable: 4065 )
      #if ( _MSC_VER >= 1400 )
         #define _CRT_SECURE_NO_WARNINGS
      #endif
   #endif

   #include "hbsetup.h"

   #define SUPPORT_UTF8
   #define PCRE_STATIC

   #if defined( __BORLANDC__ )
      #pragma warn -use
      #pragma warn -csu
      #pragma warn -aus
      #pragma warn -sig
      #pragma warn -ccc
      #pragma warn -rch
   #endif

   #include "config.h.generic"
#endif
