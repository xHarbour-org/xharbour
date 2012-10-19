/*
 * $Id$
 */

#ifndef SQLLITE3_CONFIG_H
   #define SQLLITE3_CONFIG_H

   #define SQLITE_OMIT_DEPRECATED
   #define SQLITE_ENABLE_COLUMN_METADATA

   #if defined( __BORLANDC__ )
      #pragma warn -rch /* unreachable code */
      #pragma warn -ccc /* Condition is always true or false */
      #pragma warn -aus /* Assigned value is never used */
      #pragma warn -csu /* Comparing signed and unsigned */
      #pragma warn -spa /* Suspicious pointer arithmetic */
      #pragma warn -prc
      #pragma warn -pia
      #pragma warn -use
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
