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
   #endif

#endif /* SQLLITE3_CONFIG_H */
