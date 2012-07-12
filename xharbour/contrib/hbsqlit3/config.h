/*
 * $Id$
 */

#ifndef SQLLITE3_CONFIG_H
   #define SQLLITE3_CONFIG_H

   #define SQLITE_OMIT_DEPRECATED
   #define SQLITE_ENABLE_COLUMN_METADATA

   #if defined( __BORLANDC__ )
      #pragma warn -prc
      #pragma warn -pia
      #pragma warn -use
   #elif defined( __POCC__ )
      #pragma warn(push)
      #pragma warn(disable:2154)
      #pragma warn(disable:2071)
      #pragma warn(disable:2214)
      #pragma warn(disable:2114)
      #pragma warn(disable:2135)
   #endif

#endif /* SQLLITE3_CONFIG_H */
