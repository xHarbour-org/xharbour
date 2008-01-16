/*
 * $Id: pcre.c,v 1.0 2008/01/16 12:00:00 andijahja Exp $
 */

#ifndef _CONFIG_H
   #define _CONFIG_H

   #if !defined(_WIN32)
      #define _WIN32
   #endif

   #if !defined(PCRE_STATIC)
      #define PCRE_STATIC
   #endif

   #ifdef __cplusplus
      #define PCREPOSIX_EXP_DECL  extern "C"
      #define PCREPOSIX_EXP_DEFN  extern "C"
   #else
      #define PCREPOSIX_EXP_DECL  extern
      #define PCREPOSIX_EXP_DEFN  extern
   #endif

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
