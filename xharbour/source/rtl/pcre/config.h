/*
 * $Id: config.h,v 1.12 2009/10/20 05:37:22 andijahja Exp $
 */
#ifndef _CONFIG_H
   #define _CONFIG_H

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
