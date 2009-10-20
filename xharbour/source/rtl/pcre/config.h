/*
 * $Id: config.h,v 1.11 2008/09/05 19:41:18 andijahja Exp $
 */
#ifndef _CONFIG_H
   #define _CONFIG_H

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
