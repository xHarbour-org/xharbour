/*
 * $Id$
 */
#ifndef _CONFIG_H
   #define _CONFIG_H

   #if defined( _MSC_VER ) && ( _MSC_VER >= 1400 ) &&(!defined(_CRT_SECURE_NO_WARNINGS))
      #define _CRT_SECURE_NO_WARNINGS
   #endif

   #include "hbsetup.h"

   #define SUPPORT_UTF8
   #define PCRE_STATIC

   #if defined( __POCC__ )
      #pragma warn(push)
      #pragma warn(disable:2154)
      #pragma warn(disable:2229)
   #elif defined( _MSC_VER )
      #pragma warning( disable: 4146 )
      #pragma warning( disable: 4018 )
      #pragma warning( disable: 4065 )
      #pragma warning( disable: 4244 )
   #elif defined( __WATCOMC__ )
      #pragma disable_message ( 201 )
   #elif defined( __BORLANDC__ )
      #pragma warn -use
      #pragma warn -csu
      #pragma warn -aus
      #pragma warn -sig
      #pragma warn -ccc
      #pragma warn -rch
   #endif

   #include "config.h.generic"
#endif
