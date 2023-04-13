/*
 * $Id$
 */

#include "common.ch"
#include "sqlite3.ch"

FUNCTION main()
   LOCAL cFile := ":memory:", cSQLTEXT
   LOCAL pDb, cb
   //
   IF Empty( pDb := PrepareDB(cFile) )
      RETURN 1
   ENDIF
   // Authorizer1  
   sqlite3_set_authorizer( pDb, (@Authorizer()) /*"Authorizer"*/ )

   QOut( cSQLTEXT := "SELECT * FROM main.person WHERE age BETWEEN 20 AND 40" )
   cb := (@CallBack()) // "CallBack"
   Qout( cErrorMsg(sqlite3_exec(pDb, cSQLTEXT, cb)) )

   sqlite3_sleep( 3000 )
   // Authorizer2
   Qout( cErrorMsg(sqlite3_set_authorizer(pDb, (@Authorizer2()) /*"Authorizer2"*/)) )

   QOut( cSQLTEXT := "SELECT * FROM main.person WHERE age BETWEEN 20 AND 40" )
   Qout( cErrorMsg(sqlite3_exec(pDb, cSQLTEXT, cb)) )

   sqlite3_sleep( 3000 )
   // Authorizer3
   Qout( cErrorMsg(sqlite3_set_authorizer(pDb, (@Authorizer3()) /*"Authorizer3"*/)) )

   QOut( cSQLTEXT := "SELECT * FROM main.person WHERE age BETWEEN 20 AND 40" )
   Qout( cErrorMsg(sqlite3_exec(pDb, cSQLTEXT, cb), FALSE) )

   sqlite3_sleep( 3000 )
   //   
   pDb := Nil   // close database
   //
RETURN 0

/**
*/
FUNCTION Authorizer( nAction, cName1, cName2, cDatabaseName, cTriggerOrViewName )
LOCAL oldColor := SetColor( "R/N" )
   //
   Qout( "=>", StrZero(nAction, 2), cName1, cName2, cDatabaseName, cTriggerOrViewName )

   SetColor( oldColor )
   //
RETURN SQLITE_OK

/**
*/
FUNCTION Authorizer2( nAction, cName1, cName2, cDatabaseName, cTriggerOrViewName )
LOCAL oldColor := SetColor( "R/N" ) 
   //
   Qout( "=>", StrZero(nAction, 2), cName1, cName2, cDatabaseName, cTriggerOrViewName )

   SetColor( oldColor )
   //
RETURN iif( cName2 == "pasw", SQLITE_IGNORE, SQLITE_OK )

/**
*/
FUNCTION Authorizer3( nAction, cName1, cName2, cDatabaseName, cTriggerOrViewName )
   //
RETURN iif( nAction == SQLITE_SELECT, SQLITE_DENY, SQLITE_OK )

/**
*/
FUNCTION CallBack( nColCount, aValue, aColName )
LOCAL nI
LOCAL oldColor := SetColor( "G/N" )
   //
   FOR nI := 1 TO nColCount
      Qout( Padr(aColName[nI], 5) , " == ", aValue[nI] )
   NEXT

   SetColor( oldColor )
   //
RETURN 0

/**
*/
STATIC FUNCTION cErrorMsg( nError, lShortMsg )
   LOCAL aErrorCodes := { ;
      { SQLITE_ERROR      , "SQLITE_ERROR"      , "SQL error or missing database"               }, ;
      { SQLITE_INTERNAL   , "SQLITE_INTERNAL"   , "NOT USED. Internal logic error in SQLite"    }, ;
      { SQLITE_PERM       , "SQLITE_PERM"       , "Access permission denied"                    }, ;
      { SQLITE_ABORT      , "SQLITE_ABORT"      , "Callback routine requested an abort"         }, ;
      { SQLITE_BUSY       , "SQLITE_BUSY"       , "The database file is locked"                 }, ;
      { SQLITE_LOCKED     , "SQLITE_LOCKED"     , "A table in the database is locked"           }, ;
      { SQLITE_NOMEM      , "SQLITE_NOMEM"      , "A malloc() failed"                           }, ;
      { SQLITE_READONLY   , "SQLITE_READONLY"   , "Attempt to write a readonly database"        }, ;
      { SQLITE_INTERRUPT  , "SQLITE_INTERRUPT"  , "Operation terminated by sqlite3_interrupt()" }, ;
      { SQLITE_IOERR      , "SQLITE_IOERR"      , "Some kind of disk I/O error occurred"        }, ;
      { SQLITE_CORRUPT    , "SQLITE_CORRUPT"    , "The database disk image is malformed"        }, ;
      { SQLITE_NOTFOUND   , "SQLITE_NOTFOUND"   , "NOT USED. Table or record not found"         }, ;
      { SQLITE_FULL       , "SQLITE_FULL"       , "Insertion failed because database is full"   }, ;
      { SQLITE_CANTOPEN   , "SQLITE_CANTOPEN"   , "Unable to open the database file"            }, ;
      { SQLITE_PROTOCOL   , "SQLITE_PROTOCOL"   , "NOT USED. Database lock protocol error"      }, ;
      { SQLITE_EMPTY      , "SQLITE_EMPTY"      , "Database is empty"                           }, ;
      { SQLITE_SCHEMA     , "SQLITE_SCHEMA"     , "The database schema changed"                 }, ;
      { SQLITE_TOOBIG     , "SQLITE_TOOBIG"     , "String or BLOB exceeds size limit"           }, ;
      { SQLITE_CONSTRAINT , "SQLITE_CONSTRAINT" , "Abort due to constraint violation"           }, ;
      { SQLITE_MISMATCH   , "SQLITE_MISMATCH"   , "Data type mismatch"                          }, ;
      { SQLITE_MISUSE     , "SQLITE_MISUSE"     , "Library used incorrectly"                    }, ;
      { SQLITE_NOLFS      , "SQLITE_NOLFS"      , "Uses OS features not supported on host"      }, ;
      { SQLITE_AUTH       , "SQLITE_AUTH"       , "Authorization denied"                        }, ;
      { SQLITE_FORMAT     , "SQLITE_FORMAT"     , "Auxiliary database format error"             }, ;
      { SQLITE_RANGE      , "SQLITE_RANGE"      , "2nd parameter to sqlite3_bind out of range"  }, ;
      { SQLITE_NOTADB     , "SQLITE_NOTADB"     , "File opened that is not a database file"     }, ;
      { SQLITE_ROW        , "SQLITE_ROW"        , "sqlite3_step() has another row ready"        }, ;
      { SQLITE_DONE       , "SQLITE_DONE"       , "sqlite3_step() has finished executing"       } ;
   }, nIndex, cErrorMsg := "UNKNOWN"
   //
   DEFAULT lShortMsg TO TRUE

   IF hb_IsNumeric( nError ) 
      IF nError == 0
         cErrorMsg := "SQLITE_OK"
      ELSE
         nIndex    := AScan( aErrorCodes, {|x| x[1] == nError } )
         cErrorMsg := iif( nIndex > 0, aErrorCodes[ nIndex ][ iif(lShortMsg,2,3) ], cErrorMsg )
      ENDIF
   ENDIF
   //
RETURN cErrorMsg

/**
*/
STATIC FUNCTION PrepareDB( cFile )
   LOCAL cSQLTEXT, cMsg
   LOCAL pDb, pStmt,ncValue
   
   pDb := sqlite3_open( cFile, TRUE )
   IF Empty( pDb )
      QOut( "Can't open/create database : ", cFile )

      RETURN NIL
   ENDIF

   cSQLTEXT := "CREATE TABLE person( name TEXT, age INTEGER, pasw TEXT(32) )"
   cMsg := cErrorMsg( sqlite3_exec(pDb, cSQLTEXT) )

   IF cMsg <> "SQLITE_OK"
      QOut( "Can't create table : person" )
      pDb := NIL // close database

      RETURN NIL
   ENDIF
   

   cSQLTEXT := "INSERT INTO person( name, age, pasw ) VALUES( :name, :age, :pasw )"
   pStmt := sqlite3_prepare( pDb, cSQLTEXT )
   IF Empty( pStmt )
      QOut( "Can't prepare statement : ", cSQLTEXT )
      pDb := NIL

      RETURN NIL
   ENDIF

   
   sqlite3_reset( pStmt )
   sqlite3_bind_text( pStmt, 1, "Bob" )
   sqlite3_bind_int( pStmt,  2, 28 )
   sqlite3_bind_text( pStmt, 3, hb_md5( "test") )
   sqlite3_step( pStmt )


   sqlite3_reset( pStmt )
   sqlite3_bind_text( pStmt, 1, "Paul" )
   sqlite3_bind_int( pStmt,  2, 52 )
   sqlite3_bind_text( pStmt, 3, hb_md5( "test") )
   sqlite3_step( pStmt )   
   

   sqlite3_clear_bindings( pStmt )
   sqlite3_finalize( pStmt )
   
RETURN pDb
