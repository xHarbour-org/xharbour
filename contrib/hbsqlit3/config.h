/*
 * $Id$
 */

#ifndef SQLLITE3_CONFIG_H
   #define SQLLITE3_CONFIG_H

   #define SQLITE_OMIT_DEPRECATED
   #define SQLITE_ENABLE_COLUMN_METADATA

   #if defined( __BORLANDC__ )
      #pragma warn -par                 /* Parameter '%s' is never used              */
      #pragma warn -pia                 /* Possibly incorrect assignment             */
      #pragma warn -prc                 /* Suggest parentheses to clarify precedence */
      #pragma warn -use                 /* '%s' is declared but never used           */
   #elif defined( __MINGW32__ )
      #pragma GCC diagnostic ignored "-Warray-bounds"
      #pragma GCC diagnostic ignored "-Wunused-but-set-variable"
      #pragma GCC diagnostic ignored "-Wunused-parameter"
      #pragma GCC diagnostic ignored "-Wunused-function"
      #pragma GCC diagnostic ignored "-Wsign-compare"
   #elif defined( __WATCOMC__ )
      #pragma disable_message ( 201 )
      #pragma disable_message ( 136 )
      #pragma disable_message ( 202 )
   #elif defined( __POCC__ )
      #pragma warn(push)
      #pragma warn(disable:2154)
      #pragma warn(disable:2071)
      #pragma warn(disable:2214)
      #pragma warn(disable:2114)
      #pragma warn(disable:2135)
   #elif defined( _MSC_VER )
      #if ( _MSC_VER <=1400 )
         #pragma warning(disable:4244)
         #pragma warning(disable:4761)
         #pragma warning(disable:4049)
         #pragma warning(disable:4056)
      #endif
      #pragma warning(disable:4232)
      #pragma warning(disable:4706)
   #endif

#endif /* SQLLITE3_CONFIG_H */
