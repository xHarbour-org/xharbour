/* $CATEGORY$SQLRDD/HIDE$FILES$HIDE$
* SQLRDD Postgres native access routines
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbdefs.h"
#include "hbapierr.h"
#include "hashapi.h"
#include "hbfast.h"
#include "libpq-fe.h"

#include "sqlrddsetup.ch"
#include "sqlprototypes.h"
#include "pgs.ch"
#include "sqlodbc.ch"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

static PHB_DYNS s_pSym_SR_DESERIALIZE = NULL;

typedef struct _PSQL_SESSION
{
   int status;                   // Execution return value
   int numcols;                  // Result set columns
   int ifetch;                   // Fetch position in result set
   PGconn * dbh;                 // Connection handler
   PGresult * stmt;              // Current statement handler
} PSQL_SESSION;

// culik 11/9/2010 variavel para setar o comportamento do postgresql
static BOOL iOldBehavior      = 0;

typedef PSQL_SESSION * PPSQL_SESSION;

static void myNoticeProcessor(void * arg, const char * message)
{
   HB_SYMBOL_UNUSED( arg );
   HB_SYMBOL_UNUSED( message );
//   TraceLog( "sqlerror.log", "%s", message );
}

HB_FUNC( PGSCONNECT )   /* PGSConnect( ConnectionString ) => ConnHandle */
{
   PPSQL_SESSION session = (PPSQL_SESSION) hb_xgrab( sizeof( PSQL_SESSION ) );
   const char *szConn = hb_parc(1);

   memset( session, 0, sizeof( PSQL_SESSION ) );

   session->dbh = PQconnectdb( szConn );
   session->ifetch = -2;
   /* Setup Postgres Notice Processor */
   PQsetNoticeProcessor( session->dbh, myNoticeProcessor, NULL );
   hb_retptr( (void *) session );
}

HB_FUNC( PGSFINISH )    /* PGSFinish( ConnHandle ) */
{
   PPSQL_SESSION session  = ( PPSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   assert( session->dbh != NULL );
   PQfinish( session->dbh );
   hb_xfree( session );
   hb_ret();
}

HB_FUNC( PGSSTATUS )       /* PGSStatus( ConnHandle ) => nStatus */
{
   PPSQL_SESSION session  = ( PPSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   assert( session->dbh != NULL );

   if( PQstatus( session->dbh ) == CONNECTION_OK )
   {
      hb_retni( SQL_SUCCESS );
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}

HB_FUNC( PGSSTATUS2 )       /* PGSStatus( ConnHandle ) => nStatus */
{
   PPSQL_SESSION session  = ( PPSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   assert( session->dbh != NULL );
   hb_retni( (int) PQstatus( session->dbh ) );
}

HB_FUNC( PGSRESULTSTATUS )    /* PGSResultStatus( ResultSet ) => nStatus */
{
   int ret;
   PGresult * res = (PGresult *) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   assert( res != NULL );
   ret = (int) PQresultStatus( res );

   switch (ret)
   {
   case PGRES_EMPTY_QUERY:
      ret = SQL_ERROR;
      break;
   case PGRES_COMMAND_OK:
      ret = SQL_SUCCESS;
      break;
   case PGRES_TUPLES_OK:
      ret = SQL_SUCCESS;
      break;
   case PGRES_BAD_RESPONSE:
      ret = SQL_ERROR;
      break;
   case PGRES_NONFATAL_ERROR:
      ret = SQL_SUCCESS_WITH_INFO;
      break;
   case PGRES_FATAL_ERROR :
      ret = SQL_ERROR;
      break;
   }

   hb_retni( ret );
}

HB_FUNC( PGSEXEC )      /* PGSExec( ConnHandle, cCommand ) => ResultSet */
{
   /* TraceLog( NULL, "PGSExec : %s\n", hb_parc(2) ); */
   PPSQL_SESSION session  = ( PPSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   assert( session->dbh  != NULL );

   session->stmt = PQexec( session->dbh, hb_parc(2));
   hb_retptr( (void *) session->stmt );
   session->ifetch  = -1;
   session->numcols = PQnfields( session->stmt );
}

HB_FUNC( PGSFETCH )     /* PGSFetch( ResultSet ) => nStatus */
{
   int iTpl;
   PPSQL_SESSION session  = ( PPSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   assert( session->dbh  != NULL );
   assert( session->stmt != NULL );

   iTpl = PQresultStatus( session->stmt );

   if (iTpl != PGRES_TUPLES_OK)
   {
      hb_retni( SQL_INVALID_HANDLE );
   }
   else
   {
      if ( session->ifetch >= -1 )
      {
         session->ifetch++;
         iTpl = PQntuples( session->stmt )-1;
         if (session->ifetch > iTpl)
         {
            hb_retni( SQL_NO_DATA_FOUND );
         }
         else
         {
            hb_retni( SQL_SUCCESS );
         }
      }
      else
      {
         hb_retni( SQL_INVALID_HANDLE );
      }
   }
}

HB_FUNC( PGSRESSTATUS )    /* PGSResStatus( ResultSet ) => cErrMessage */
{
   PPSQL_SESSION session  = ( PPSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   assert( session->dbh  != NULL );
   assert( session->stmt != NULL );
   hb_retc( PQresStatus( PQresultStatus( session->stmt )) );
}

HB_FUNC( PGSCLEAR )        /* PGSClear( ResultSet ) */
{
   PPSQL_SESSION session  = ( PPSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   assert( session->dbh  != NULL );
   if( session->stmt )
   {
      PQclear( session->stmt );
      session->stmt   = NULL;
      session->ifetch = -2;
   }
}

HB_FUNC( PGSGETDATA )      /* PGSGetData( ResultSet, nColumn ) => cValue */
{
   PPSQL_SESSION session  = ( PPSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   assert( session->dbh  != NULL );
   assert( session->stmt != NULL );
   hb_retc( PQgetvalue( session->stmt, session->ifetch, hb_parnl(2)-1));
}

HB_FUNC( PGSCOLS )         /* PGSCols( ResultSet ) => nColsInQuery */
{
   PGresult * res = (PGresult *) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   assert( res != NULL );
   hb_retnl((long)PQnfields( res ));
}

HB_FUNC( PGSERRMSG )         /* PGSErrMsg( ConnHandle ) => cErrorMessage */
{
   PPSQL_SESSION session  = ( PPSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   assert( session->dbh  != NULL );
   hb_retc(PQerrorMessage( session->dbh ));
}

HB_FUNC( PGSCOMMIT )       /* PGSCommit( ConnHandle ) => nError */
{
   PPSQL_SESSION session  = ( PPSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   PGresult * res;
   assert( session->dbh  != NULL );
   res = PQexec( session->dbh, "COMMIT" );
   if (PQresultStatus(res) == PGRES_COMMAND_OK)
      hb_retni( SQL_SUCCESS );
   else
      hb_retni( SQL_ERROR );
}

HB_FUNC( PGSROLLBACK )     /* PGSRollBack( ConnHandle ) => nError */
{
   PPSQL_SESSION session  = ( PPSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   PGresult * res;
   assert( session->dbh  != NULL );
   res = PQexec( session->dbh, "ROLLBACK" );
   if (PQresultStatus(res) == PGRES_COMMAND_OK)
      hb_retni( SQL_SUCCESS );
   else
      hb_retni( SQL_ERROR );
}

HB_FUNC( PGSQUERYATTR )     /* PGSQueryAttr( ResultSet ) => aStruct */
{
   int row, rows, type;
   long nullable;
   PHB_ITEM ret, atemp, temp;
   LONG typmod;
   PPSQL_SESSION session  = ( PPSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   assert( session->dbh  != NULL );
   assert( session->stmt != NULL );

   if (hb_pcount() != 1)
   {
      hb_retnl( -2 );
      return;
   }

   rows  = PQnfields( session->stmt );
   ret   = hb_itemNew( NULL );
   temp  = hb_itemNew( NULL );
   atemp = hb_itemNew( NULL );

   hb_arrayNew( ret, rows );

   for ( row = 0; row < rows; row++ )
   {
      /* Column name */
      hb_arrayNew( atemp, 11 );
      hb_itemPutC( temp, hb_strupr( PQfname( session->stmt, row ) ) );
      hb_arraySetForward( atemp, FIELD_NAME, temp );
      hb_arraySetNL( atemp, FIELD_ENUM, row+1 );

      /* Data type, len, dec */
      type   = (int) PQftype( session->stmt, row );
      typmod = PQfmod( session->stmt, row );

     // if ( strcmp( PQgetvalue( session->stmt, row, 3 ), "f")  == 0 ) 
         nullable = PQgetisnull( session->stmt, row,PQfnumber( session->stmt,PQfname( session->stmt, row ) ) ) ;
      //else
      //   nullable = 0;
      
      if (typmod < 0L)
      {
         typmod = (LONG) PQfsize( session->stmt, row );
      }
/*
      if (typmod < 0L)
      {
         typmod = 20L;
      }
*/
      switch (type)
      {
         case CHAROID:
         case NAMEOID:
         case BPCHAROID:
         case VARCHAROID:
         case BYTEAOID:
         case ABSTIMEOID:
         case RELTIMEOID:
         case TINTERVALOID:
         case CASHOID:
         case MACADDROID:
         case INETOID:
         case CIDROID:
         case TIMETZOID:
         case TIMESTAMPOID:
         case TIMESTAMPTZOID:
            hb_itemPutC( temp, "C" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, typmod - 4 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_CHAR ) );
            break;
         case UNKNOWNOID:
            hb_itemPutC( temp, "C" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, PQgetlength( session->stmt, 0, row ) ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_CHAR ) );
            break;
         case NUMERICOID:
            hb_itemPutC( temp, "N" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            if (typmod > 0)
            {
               hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, ((typmod - 4L) >> 16L ) ) );
               hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, ((typmod - 4L) & 0xffff ) ) );
            }
            else
            {
               hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 18 ) );
               hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 6 ) );
            }
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
            break;
         case BOOLOID:
            hb_itemPutC( temp, "L" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 1 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_BIT ) );
            break;
         case TEXTOID:
            hb_itemPutC( temp, "M" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 10 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_LONGVARCHAR ) );
            break;
         case DATEOID:
            hb_itemPutC( temp, "D" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 8 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_DATE ) );
            break;
         case INT2OID:
            hb_itemPutC( temp, "N" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 6 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
            break;
         case INT8OID:
         case OIDOID:
            hb_itemPutC( temp, "N" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 20 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
            break;
         case INT4OID:
            hb_itemPutC( temp, "N" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 11 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
            break;
         case FLOAT4OID:
            hb_itemPutC( temp, "N" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 18 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 2 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
            break;
         case FLOAT8OID:
            hb_itemPutC( temp, "N" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 18 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 6 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
            break;
         default:
            TraceLog( "sqlerror.log", "Strange data type returned in query: %i\n", type );
            break;
      }

      /* Nullable */
      hb_arraySetForward( atemp, FIELD_NULLABLE, hb_itemPutL( temp, 0 ) );
      /* add to main array */
      hb_arraySetForward( ret, row+1, atemp );
   }
   hb_itemRelease( atemp );
   hb_itemRelease( temp );
   hb_itemReturnForward( ret );
   hb_itemRelease( ret );
}

HB_FUNC( PGSTABLEATTR )     /* PGSTableAttr( ConnHandle, cTableName ) => aStruct */
{
   char attcmm[512];
   int row, rows, type;
   PHB_ITEM ret, atemp, temp;
   long typmod;
   long nullable;
   PGresult * stmtTemp;
   PPSQL_SESSION session  = ( PPSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   assert( session->dbh  != NULL );

   if (hb_pcount() < 3)
   {
      hb_retnl( -2 );
      return;
   }

   sprintf( attcmm, "select a.attname, a.atttypid, a.atttypmod, a.attnotnull from pg_attribute a left join pg_class b on a.attrelid = b.oid left join pg_namespace c on b.relnamespace = c.oid where a.attisdropped IS FALSE and a.attnum > 0 and b.relname = '%s' and c.nspname = '%s' order by attnum", hb_parc(2), hb_parc(3) );

   stmtTemp = PQexec( session->dbh, attcmm );

   if (PQresultStatus( stmtTemp ) != PGRES_TUPLES_OK)
   {
      TraceLog( "sqlerror.log", "Query error : %i - %s\n", PQresultStatus( stmtTemp ), PQresStatus(PQresultStatus( stmtTemp )) );
      PQclear( stmtTemp );
   }

   rows  = PQntuples( stmtTemp );
   ret   = hb_itemNew( NULL );
   atemp = hb_itemNew( NULL );
   temp  = hb_itemNew( NULL) ;

   hb_arrayNew( ret, rows );

   for (row=0; row < rows; row++ )
   {
      /* Column name */
      hb_arrayNew( atemp, 11 );
      hb_itemPutC( temp, hb_strupr( PQgetvalue( stmtTemp, row, 0 ) ) );
      hb_arraySetForward( atemp, 1, temp );
      hb_arraySetNL( atemp, FIELD_ENUM, row+1 );

      /* Data type, len, dec */

      type   = atoi( PQgetvalue( stmtTemp, row, 1 ) );
      typmod = atol( PQgetvalue( stmtTemp, row, 2 ) );
      if ( iOldBehavior ) 
      {
         nullable = 0;
      }    
      else
      {
         if ( strcmp( PQgetvalue( stmtTemp, row, 3 ), "f")  == 0 ) 
         {
            nullable = 1 ;
         }   
         else
         {
            nullable = 0;
         }   
      }

      switch (type)
      {
         case CHAROID:
         case NAMEOID:
         case BPCHAROID:
         case VARCHAROID:
         case BYTEAOID:
         case ABSTIMEOID:
         case RELTIMEOID:
         case TINTERVALOID:
         case CASHOID:
         case MACADDROID:
         case INETOID:
         case CIDROID:
         case TIMETZOID:
            hb_itemPutC( temp, "C" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, typmod - 4 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_CHAR ) );
            break;
         case NUMERICOID:
            hb_itemPutC( temp, "N" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, ((typmod - 4L) >> 16L ) ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, ((typmod - 4L) & 0xffff ) ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
            break;
         case BOOLOID:
            hb_itemPutC( temp, "L" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 1 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_BIT ) );
            break;
         case TEXTOID:
            hb_itemPutC( temp, "M" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 10 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_LONGVARCHAR ) );
            break;
         case DATEOID:
            hb_itemPutC( temp, "D" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 8 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_DATE ) );
            break;
         case INT2OID:
            hb_itemPutC( temp, "N" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 6 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
            break;
         case INT8OID:
            hb_itemPutC( temp, "N" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 20 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
            break;
         case INT4OID:
            hb_itemPutC( temp, "N" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 11 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 0 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
            break;
         case FLOAT4OID:
            hb_itemPutC( temp, "N" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 18 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 2 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
            break;
         case FLOAT8OID:
            hb_itemPutC( temp, "N" );
            hb_arraySetForward( atemp, FIELD_TYPE, temp );
            hb_arraySetForward( atemp, FIELD_LEN, hb_itemPutNI( temp, 18 ) );
            hb_arraySetForward( atemp, FIELD_DEC, hb_itemPutNI( temp, 6 ) );
            hb_arraySetForward( atemp, FIELD_DOMAIN, hb_itemPutNI( temp, SQL_NUMERIC ) );
            break;
         default:
            TraceLog( "sqlerror.log", "Strange data type returned: %i\n", type );
            break;
      }

      /* Nullable */

      hb_arraySetForward( atemp, FIELD_NULLABLE, hb_itemPutL( temp, nullable ) );

      /* add to main array */
      hb_arraySetForward( ret, row+1, atemp );
   }
   hb_itemRelease( atemp );
   hb_itemRelease( temp );
   hb_itemReturnForward( ret );
   hb_itemRelease( ret );
   PQclear( stmtTemp );
}


//-----------------------------------------------------------------------------//

void PGSFieldGet( PHB_ITEM pField, PHB_ITEM pItem, char * bBuffer, LONG lLenBuff, BOOL bQueryOnly, ULONG ulSystemID, BOOL bTranslate )
{
   LONG lType;
   LONG lLen, lDec;
   PHB_ITEM pTemp;

   HB_SYMBOL_UNUSED( bQueryOnly );
   HB_SYMBOL_UNUSED( ulSystemID );

   lType = ( LONG ) hb_arrayGetNL( pField, 6 );
   lLen  = ( LONG ) hb_arrayGetNL( pField, 3 );
   lDec  = ( LONG ) hb_arrayGetNL( pField, 4 );

   if( lLenBuff <= 0 )     // database content is NULL
   {
      switch( lType )
      {
         case SQL_CHAR:
         {
            char * szResult = ( char * ) hb_xgrab( lLen + 1 );
            hb_xmemset( szResult, ' ', lLen );
            hb_itemPutCPtr( pItem, szResult, (ULONG) lLen );
            break;
         }
         case SQL_NUMERIC:
         case SQL_FAKE_NUM:
         {
            char szResult[2] = { ' ', '\0' };
            sr_escapeNumber( szResult, (ULONG) lLen, (ULONG) lDec, pItem );
            break;
         }
         case SQL_DATE:
         {
            char dt[9] = {' ',' ',' ',' ',' ',' ',' ',' ','\0'};
            hb_itemPutDS( pItem, dt );
            break;
         }
         case SQL_LONGVARCHAR:
         {
            hb_itemPutCL( pItem, bBuffer, 0 );
            break;
         }
         case SQL_BIT:
         {
            hb_itemPutL( pItem, FALSE );
            break;
         }

#ifdef SQLRDD_TOPCONN
         case SQL_FAKE_DATE:
         {
            hb_itemPutDS( pItem, bBuffer );
            break;
         }
#endif
         default:
            TraceLog( "oci.log", "Invalid data type detected: %i\n", lType );
      }
   }
   else
   {
      switch( lType )
      {
         case SQL_CHAR:
         {
            LONG lPos;
            char * szResult = ( char * ) hb_xgrab( lLen + 1 );
            hb_xmemcpy( szResult, bBuffer, ( LONG ) (lLen < lLenBuff ? lLen : lLenBuff ) );

            for( lPos = ( LONG ) lLenBuff; lPos < lLen; lPos++ )
            {
               szResult[ lPos ] = ' ';
            }
            hb_itemPutCPtr( pItem, szResult, (ULONG) lLen );
            break;
         }
         case SQL_NUMERIC:
         {
            sr_escapeNumber( bBuffer, (ULONG) lLen, (ULONG) lDec, pItem );
            break;
         }
         case SQL_DATE:
         {
            char dt[9];
            dt[0] = bBuffer[0];
            dt[1] = bBuffer[1];
            dt[2] = bBuffer[2];
            dt[3] = bBuffer[3];
            dt[4] = bBuffer[5];
            dt[5] = bBuffer[6];
            dt[6] = bBuffer[8];
            dt[7] = bBuffer[9];
            dt[8] = '\0';
            hb_itemPutDS( pItem, dt );
            break;
         }
         case SQL_LONGVARCHAR:
         {
            if( lLenBuff > 10 && strncmp( bBuffer, SQL_SERIALIZED_SIGNATURE, 10 ) == 0 && (!sr_lSerializedAsString()) )
            {
               if( s_pSym_SR_DESERIALIZE == NULL )
               {
                  hb_dynsymLock();
                  s_pSym_SR_DESERIALIZE = hb_dynsymFindName( "SR_DESERIALIZE" );
                  hb_dynsymUnlock();
                  if ( s_pSym_SR_DESERIALIZE  == NULL ) printf( "Could not find Symbol SR_DESERIALIZE\n" );
               }
               hb_vmPushSymbol( s_pSym_SR_DESERIALIZE->pSymbol );
               hb_vmPushNil();
               hb_vmPushString( bBuffer, lLenBuff );
               hb_vmDo( 1 );

               pTemp = hb_itemNew( NULL );
               hb_itemForwardValue( pTemp, hb_stackReturnItem() );

               if( HB_IS_HASH( pTemp ) && sr_isMultilang() && bTranslate )
               {
                  ULONG ulPos;
                  if( hb_hashScan( pTemp, sr_getCurrentLang(), &ulPos ) ||
                      hb_hashScan( pTemp, sr_getSecondLang(), &ulPos ) ||
                      hb_hashScan( pTemp, sr_getRootLang(), &ulPos ) )
                  {
                     hb_itemCopy( pItem, hb_hashGetValueAt( pTemp, ulPos ) );
                  }
               }
               else
               {
                  hb_itemForwardValue( pItem, pTemp );
               }
               hb_itemRelease( pTemp );
            }
            else
            {
               hb_itemPutCL( pItem, bBuffer, (ULONG) lLenBuff );
            }
            break;
         }
         case SQL_BIT:
         {
            hb_itemPutL( pItem, bBuffer[0] == 't' ? TRUE : FALSE );
//            hb_itemPutL( pItem, hb_strValInt( bBuffer, &iOverflow ) > 0  ? TRUE : FALSE );
            break;
         }

#ifdef SQLRDD_TOPCONN
         case SQL_FAKE_DATE:
         {
            hb_itemPutDS( pItem, bBuffer );
            break;
         }
#endif
         default:
            TraceLog( "oci.log", "Invalid data type detected: %i\n", lType );
      }
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( PGSLINEPROCESSED )
{
   PPSQL_SESSION session  = ( PPSQL_SESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   PHB_ITEM temp;
   USHORT i;
   char * col;
   PHB_ITEM pFields = hb_param( 3, HB_IT_ARRAY );
   BOOL  bQueryOnly = hb_parl( 4 );
   ULONG ulSystemID = hb_parnl( 5 );
   BOOL  bTranslate = hb_parl( 6 );
   PHB_ITEM pRet    = hb_param( 7, HB_IT_ARRAY );
   LONG lIndex, cols;

   assert( session->dbh  != NULL );
   assert( session->stmt != NULL );

   if( session )
   {
      cols = pFields->item.asArray.value->ulLen;

      for( i=0; i < cols; i++ )
      {
         temp = hb_itemNew( NULL );
         lIndex  = hb_arrayGetNL( hb_arrayGetItemPtr( pFields, i+1 ), FIELD_ENUM );

         if( lIndex != 0 )
         {
            col = PQgetvalue( session->stmt, session->ifetch, lIndex - 1 );
            PGSFieldGet( hb_arrayGetItemPtr( pFields, i+1 ), temp, (char * ) col, strlen( col ), bQueryOnly, ulSystemID, bTranslate );
         }
         hb_arraySetForward( pRet, i+1 , temp );
         hb_itemRelease( temp );
      }
   }
}

//-----------------------------------------------------------------------------//
HB_FUNC(SETPGSOLDBEHAVIOR)
{
   iOldBehavior = hb_parl( 1 ) ;
}