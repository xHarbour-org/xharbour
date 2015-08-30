/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SQLite3 library low level (client api) interface code
 *
 * Copyright 2007-2010 P.Chornyj <myorg63@mail.ru>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 * See COPYING for licensing terms.
 *
 */

#include "sqlite3.h"
/*-----------------------------------------------------------------------------------------
 XHarbour Port from orginale Harbour version by P.Chornyj <myorg63@mail.ru>
 November 18, 2011 by R.Visscher <richard@irvis.com>
-----------------------------------------------------------------------------------------*/

#ifndef PFLL
#  if defined( __BORLANDC__ ) || defined( _MSC_VER ) || defined( __MINGW32__ )
#     define PFLL    "I64"
#  else
#     define PFLL    "ll"
#  endif
#endif

#include "hbvm.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbapistr.h"
#include "hbstack.h"

/* TOFIX: verify the exact SQLITE3 version */
#if SQLITE_VERSION_NUMBER <= 3004001
#define sqlite3_int64                       HB_LONGLONG
#define sqlite3_uint64                      HB_ULONGLONG
#endif

#define HB_SQLITE3_DB                        6000001

#define HB_ERR_MEMSTRU_NOT_MEM_BLOCK         4001
#define HB_ERR_MEMSTRU_WRONG_MEMSTRU_BLOCK   4002
#define HB_ERR_MEMSTRU_DESTROYED             4003

#ifdef SQLITE3_DYNLIB
extern char * sqlite3_temp_directory;
#endif /* SQLITE3_DYNLIB */

static PHB_ITEM hb_sqlite3_itemPut( PHB_ITEM pItem, void * pMemAddr, int iType );
static void *   hb_sqlite3_itemGet( PHB_ITEM pItem, int iType, BOOL fError );
static void     hb_sqlite3_ret( void * pMemAddr, int iType );
static void *   hb_sqlite3_param( int iParam, int iType, BOOL fError );

static int  callback( void *, int, char **, char ** );
static int  authorizer( void *, int, const char *, const char *, const char *, const char * );
static int  busy_handler( void *, int );
static int  progress_handler( void * );
static int  hook_commit( void * );
static void hook_rollback( void * );
//static void func( sqlite3_context *, int, sqlite3_value ** );

typedef struct
{
   sqlite3 * db;
   PHB_ITEM cbAuthorizer;
   PHB_ITEM cbBusyHandler;
   PHB_ITEM cbProgressHandler;
   PHB_ITEM  cbHookCommit;
   PHB_ITEM  cbHookRollback;
   PHB_ITEM  cbFunc;
} HB_SQLITE3, * PHB_SQLITE3;

typedef struct
{
   int type;
   HB_SQLITE3 * hbsqlite3;
} HB_SQLITE3_HOLDER, * PHB_SQLITE3_HOLDER;

typedef sqlite3_stmt * psqlite3_stmt;


/*----------------------------------------------------------------------------------
  Static functions
----------------------------------------------------------------------------------*/

static int busy_handler( void * Cargo, int iNumberOfTimes )
{
   PHB_DYNS pCallback = ( PHB_DYNS ) Cargo;

   if( pCallback && hb_vmRequestReenter() )
   {
      int iRes;

	  hb_vmPushSymbol( hb_dynsymGetSymbol( hb_dynsymName( pCallback ) ) );
	  hb_vmPushNil();
	  hb_vmPushInteger( iNumberOfTimes );
	  hb_vmFunction( 1 );

      iRes = hb_parni( -1 );

      hb_vmRequestRestore();

      return iRes;
   }

   return 0;
}

static int progress_handler( void * Cargo )
{
   PHB_DYNS pCallback = ( PHB_DYNS ) Cargo;

   if( pCallback && hb_vmRequestReenter() )
   {
      int iRes;

      hb_vmPushSymbol( hb_dynsymGetSymbol( hb_dynsymName( pCallback ) ) );
	  hb_vmPushNil();
	  hb_vmFunction( 0 );

      iRes = hb_parni( -1 );

      hb_vmRequestRestore();

      return iRes;
   }

   return 0;
}

static int hook_commit( void * Cargo )
{
   PHB_DYNS pCallback = ( PHB_DYNS ) Cargo;

   if( pCallback && hb_vmRequestReenter() )
   {
      int iRes;

	hb_vmPushSymbol( hb_dynsymGetSymbol( hb_dynsymName( pCallback ) ) );
    hb_vmPushNil();
	hb_vmFunction( 0 );

      iRes = hb_parni( -1 );

      hb_vmRequestRestore();

      return iRes;
   }

   return 0;
}

static void hook_rollback( void * Cargo )
{
   PHB_DYNS pCallback = ( PHB_DYNS ) Cargo;

   if( pCallback && hb_vmRequestReenter() )
   {
	  hb_vmPushSymbol( hb_dynsymGetSymbol( hb_dynsymName( pCallback ) ) );
      hb_vmPushNil();
	  hb_vmFunction( 0 );

      hb_vmRequestRestore();
   }
}

static int callback( void * Cargo, int argc, char ** argv, char ** azColName )
{
    PHB_DYNS pCallback = ( PHB_DYNS ) Cargo;

   if( pCallback && hb_vmRequestReenter() )
   {
      PHB_ITEM pArrayValue = hb_itemArrayNew( argc );
      PHB_ITEM pArrayColName = hb_itemArrayNew( argc );
      int      iRes, i;
      const char * cFunc = hb_dynsymName( pCallback );


      for( i = 0; i < argc; i++ )
      {
         hb_arraySetC( pArrayValue, i + 1, ( const char * ) ( argv[ i ] ? argv[ i ] : "NULL" ) );
         hb_arraySetC( pArrayColName, i + 1, ( const char * ) azColName[ i ] );
      }

	  hb_vmPushSymbol( hb_dynsymGetSymbol( cFunc ) );
      hb_vmPushNil();

      hb_vmPushInteger( argc );
      hb_vmPush( pArrayValue );
      hb_vmPush( pArrayColName );

      iRes = hb_parni( -1 );

	  hb_vmFunction( 3 );

      hb_itemRelease( pArrayValue );
      hb_itemRelease( pArrayColName );

      hb_vmRequestRestore();

      return iRes;
   }

   return 0;
}

static int authorizer( void * Cargo, int iAction, const char * sName1, const char * sName2,
                       const char * sName3,
                       const char * sName4 )
{

   PHB_DYNS pCallback = ( PHB_DYNS ) Cargo;

   if( pCallback && hb_vmRequestReenter() )
   {

      int      iRes;
      PHB_ITEM pItem1 = hb_itemPutStrUTF8( NULL, sName1 );
      PHB_ITEM pItem2 = hb_itemPutStrUTF8( NULL, sName2 );
      PHB_ITEM pItem3 = hb_itemPutStrUTF8( NULL, sName3 );
      PHB_ITEM pItem4 = hb_itemPutStrUTF8( NULL, sName4 );

	  const char * cFunc = hb_dynsymName( pCallback );

	  hb_vmPushSymbol( hb_dynsymGetSymbol( cFunc ) );
      hb_vmPushNil();

	  hb_vmPushInteger( iAction );
      hb_vmPush( pItem1 );
      hb_vmPush( pItem2 );
      hb_vmPush( pItem3 );
      hb_vmPush( pItem4 );

	  hb_vmFunction( 5 );

      iRes = hb_parni( -1 );

      hb_itemRelease( pItem1 );
      hb_itemRelease( pItem2 );
      hb_itemRelease( pItem3 );
      hb_itemRelease( pItem4 );

      hb_vmRequestRestore();

      return iRes;
   }
   else
   {
	  HB_TRACE(HB_TR_ALWAYS, ("hb_strnull(%s)", "Callback ERROR"));
    }

   return 0;
}

static void SQL3ProfileLog( void * sFile, const char * sProfileMsg, sqlite3_uint64 int64 )
{
   if( sProfileMsg )
   {
      FILE *hFile = hb_fopen( sFile ? ( const char * ) sFile : "profile.log", "a" );

      if( hFile )
      {
      #if ( defined( _MSC_VER ) && _MSC_VER >= 1900 ) 	      
         fprintf( hFile, "%s - %lld\n", sProfileMsg, int64 );
      #else
         fprintf( hFile, "%s - %"PFLL "d\n", sProfileMsg, int64 ); 
      #endif   
         fclose( hFile );
      }
   }
}

static void SQL3TraceLog( void * sFile, const char * sTraceMsg )
{
   if( sTraceMsg )
   {
      FILE *hFile = hb_fopen( sFile ? ( const char * ) sFile : "trace.log", "a" );

      if( hFile )
      {
         fprintf( hFile, "%s \n", sTraceMsg );
         fclose( hFile );
      }
   }
}

static HB_GARBAGE_FUNC( hb_sqlite3_destructor )
{
   PHB_SQLITE3_HOLDER pStructHolder = ( PHB_SQLITE3_HOLDER ) Cargo;

   if( pStructHolder && pStructHolder->hbsqlite3 )
   {
		if( pStructHolder->hbsqlite3->db )
		{
			sqlite3_close( pStructHolder->hbsqlite3->db );
			pStructHolder->hbsqlite3->db = NULL;
		}

		if( pStructHolder->hbsqlite3->cbAuthorizer )
		{
			hb_itemRelease( pStructHolder->hbsqlite3->cbAuthorizer );
			pStructHolder->hbsqlite3->cbAuthorizer = NULL;
		}

		if( pStructHolder->hbsqlite3->cbHookCommit )
		{
			hb_itemRelease( pStructHolder->hbsqlite3->cbHookCommit );
			pStructHolder->hbsqlite3->cbHookCommit = NULL;
		}

		if( pStructHolder->hbsqlite3->cbHookRollback )
		{
			hb_itemRelease( pStructHolder->hbsqlite3->cbHookRollback );
			pStructHolder->hbsqlite3->cbHookRollback = NULL;
		}

		if( pStructHolder->hbsqlite3->cbBusyHandler )
		{
			hb_itemRelease( pStructHolder->hbsqlite3->cbBusyHandler );
			pStructHolder->hbsqlite3->cbBusyHandler = NULL;
		}

		if( pStructHolder->hbsqlite3->cbProgressHandler )
		{
			hb_itemRelease( pStructHolder->hbsqlite3->cbProgressHandler );
			pStructHolder->hbsqlite3->cbProgressHandler = NULL;
		}

		hb_xfree( pStructHolder->hbsqlite3 );
		pStructHolder->hbsqlite3 = NULL;
   }
}

static PHB_ITEM hb_sqlite3_itemPut( PHB_ITEM pItem, void * pMemAddr, int iType )
{
   PHB_SQLITE3_HOLDER pStructHolder;

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( pItem );

   pStructHolder = ( PHB_SQLITE3_HOLDER ) hb_gcAlloc( sizeof( HB_SQLITE3_HOLDER ), hb_sqlite3_destructor );
   pStructHolder->hbsqlite3 = ( HB_SQLITE3 * ) pMemAddr;
   pStructHolder->type = iType;

   return hb_itemPutPtrGC( pItem, pStructHolder );
}

static void * hb_sqlite3_itemGet( PHB_ITEM pItem, int iType, BOOL fError )
{
   PHB_SQLITE3_HOLDER   pStructHolder = ( PHB_SQLITE3_HOLDER ) hb_itemGetPtrGC( pItem, hb_sqlite3_destructor );
   int                  iError = 0;

   HB_SYMBOL_UNUSED( iError );

   if( ! pStructHolder )
      iError = HB_ERR_MEMSTRU_NOT_MEM_BLOCK;
   else if( pStructHolder->type != iType )
      iError = HB_ERR_MEMSTRU_WRONG_MEMSTRU_BLOCK;
   else if( ! pStructHolder->hbsqlite3 )
      iError = HB_ERR_MEMSTRU_DESTROYED;
   else
      return pStructHolder->hbsqlite3;

   if( fError )
      hb_errRT_BASE_SubstR( (HB_ERRCODE) EG_ARG, (HB_ERRCODE) iError, NULL, HB_ERR_FUNCNAME, ( ULONG ) HB_ERR_ARGS_BASEPARAMS );

   return NULL;
}

static void hb_sqlite3_ret( void * pMemAddr, int iType )
{
   hb_sqlite3_itemPut( hb_stackReturnItem(), pMemAddr, iType );
}

static void * hb_sqlite3_param( int iParam, int iType, BOOL fError )
{
   return hb_sqlite3_itemGet( hb_param( iParam, HB_IT_POINTER ), iType, fError );
}

/*-------------------------------------------------------------------------------------------------------------------
 SQLite3 API Functions
--------------------------------------------------------------------------------------------------------------------*/
HB_FUNC( SQLITE3_SET_AUTHORIZER )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
	{
		if( pHbSqlite3->cbAuthorizer )
		{
			hb_itemRelease( pHbSqlite3->cbAuthorizer );
			pHbSqlite3->cbAuthorizer = NULL;
		}

		if( ISPOINTER( 2 ) )
		{
			pHbSqlite3->cbAuthorizer = hb_itemNew( hb_param( 2, HB_IT_POINTER ) );

			hb_gcUnlock( pHbSqlite3->cbAuthorizer );

			hb_retni( sqlite3_set_authorizer( pHbSqlite3->db, authorizer,( void * ) pHbSqlite3->cbAuthorizer ) );
		}
		else
		{
			hb_retni( sqlite3_set_authorizer( pHbSqlite3->db, NULL, NULL ) );
		}
	}
}

HB_FUNC( SQLITE3_BACKUP_INIT )
{
	HB_SQLITE3 *      pHbSqlite3Dest = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );
	HB_SQLITE3 *      pHbSqlite3Source = ( HB_SQLITE3 * ) hb_sqlite3_param( 3, HB_SQLITE3_DB, TRUE );
	sqlite3_backup *  pBackup;

	if( pHbSqlite3Dest && pHbSqlite3Dest->db && pHbSqlite3Source && pHbSqlite3Source->db && ISCHAR( 2 ) && ISCHAR( 4 ) )
	{
		pBackup = sqlite3_backup_init( pHbSqlite3Dest->db, hb_parcx(
                                        2 ), pHbSqlite3Source->db, hb_parcx( 4 ) );

		if( pBackup )
			hb_retptr( pBackup );
		else
			hb_retptr( NULL );
	}
	else
		 hb_retptr( NULL );
}

HB_FUNC( SQLITE3_BACKUP_STEP )
{
	sqlite3_backup * pBackup = ( sqlite3_backup * ) hb_parptr( 1 );

	if( pBackup )
		hb_retni( sqlite3_backup_step( pBackup, hb_parni( 2 ) ) );
	else
		hb_retni( -1 );
}

HB_FUNC( SQLITE3_BACKUP_FINISH )
{
	sqlite3_backup * pBackup = ( sqlite3_backup * ) hb_parptr( 1 );

	if( pBackup )
		hb_retni( sqlite3_backup_finish( pBackup ) );
	else
		hb_retni( -1 );
}

HB_FUNC( SQLITE3_BACKUP_REMAINING )
{
	sqlite3_backup * pBackup = ( sqlite3_backup * ) hb_parptr( 1 );

	if( pBackup )
		hb_retni( sqlite3_backup_remaining( pBackup ) );
	else
		hb_retni( -1 );
}

HB_FUNC( SQLITE3_BACKUP_PAGECOUNT )
{
	sqlite3_backup * pBackup = ( sqlite3_backup * ) hb_parptr( 1 );

	if( pBackup )
		hb_retni( sqlite3_backup_pagecount( pBackup ) );
	else
		hb_retni( -1 );
}

HB_FUNC( SQLITE3_BIND_BLOB )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retni( sqlite3_bind_blob( pStmt, hb_parni( 2 ), hb_parcx( 3 ), ( int ) hb_parcsiz( 3 ) - 1,
                                   SQLITE_TRANSIENT ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_BIND_DOUBLE )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retni( sqlite3_bind_double( pStmt, hb_parni( 2 ), hb_parnd( 3 ) ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_BIND_INT )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retni( sqlite3_bind_int( pStmt, hb_parni( 2 ), hb_parni( 3 ) ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_BIND_INT64 )
{
	psqlite3_stmt  pStmt = ( psqlite3_stmt ) hb_parptr( 1 );
	sqlite3_int64  int64 = hb_parnint( 3 );

	if( pStmt )
		hb_retni( sqlite3_bind_int64( pStmt, hb_parni( 2 ), int64 ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_BIND_NULL )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retni( sqlite3_bind_null( pStmt, hb_parni( 2 ) ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_BIND_PARAMETER_COUNT )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retni( sqlite3_bind_parameter_count( pStmt ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_BIND_PARAMETER_INDEX )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
	{
		void * hParameterName;

		hb_retni( sqlite3_bind_parameter_index( pStmt, hb_parstr_utf8( 2, &hParameterName, NULL ) ) );

		hb_strfree( hParameterName );
	}
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_BIND_PARAMETER_NAME )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retstr_utf8( sqlite3_bind_parameter_name( pStmt, hb_parni( 2 ) ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_BIND_TEXT )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
	{
		void *         hSQLText;
		HB_SIZE        nSQLText;

		const char *   pszSQLText = hb_parstr_utf8( 3, &hSQLText, &nSQLText );

		hb_retni( sqlite3_bind_text( pStmt, hb_parni( 2 ), pszSQLText, ( int ) nSQLText,
                                   SQLITE_TRANSIENT ) );

		hb_strfree( hSQLText );
	}
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_BIND_ZEROBLOB )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retni( sqlite3_bind_zeroblob( pStmt, hb_parni( 2 ), hb_parni( 3 ) ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_BLOB_BYTES )
{
	sqlite3_blob * pBlob = ( sqlite3_blob * ) hb_parptr( 1 );

	if( pBlob )
		hb_retni( sqlite3_blob_bytes( pBlob ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_BLOB_CLOSE )
{
	sqlite3_blob * pBlob = ( sqlite3_blob * ) hb_parptr( 1 );

	if( pBlob )
		hb_retni( sqlite3_blob_close( pBlob ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_BLOB_OPEN )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
	{
		sqlite3_blob * ppBlob = NULL;

		void *         hDbName;
		void *         hTableName;
		void *         hColumnName;

		if
		(
			sqlite3_blob_open
			(
				pHbSqlite3->db,
				hb_parstr_utf8( 2, &hDbName, NULL ),
				hb_parstr_utf8( 3, &hTableName, NULL ),
				hb_parstr_utf8( 4, &hColumnName, NULL ),
				( sqlite3_int64 ) hb_parnint( 5 ) /* iRow */,
				hb_parni( 6 ) /* flags */,
				&ppBlob
			) == SQLITE_OK
		)
		{
			hb_retptr( ppBlob );
		}
		else
			hb_retptr( NULL );

		hb_strfree( hDbName );
		hb_strfree( hTableName );
		hb_strfree( hColumnName );
   }
   else
      hb_retptr( NULL );
}

HB_FUNC( SQLITE3_BLOB_READ )
{
	sqlite3_blob * pBlob = ( sqlite3_blob * ) hb_parptr( 1 );

	if( pBlob )
	{
		int      iLen = hb_parni( 2 );
		char *   buffer;

		if( iLen == 0 )
			iLen = sqlite3_blob_bytes( pBlob );

		buffer = ( char * ) hb_xgrab( iLen + 1 );

		if( SQLITE_OK == sqlite3_blob_read( pBlob, ( void * ) buffer, iLen, hb_parni( 3 ) ) )
		{
			buffer[ iLen ] = '\0';
			hb_retclen_buffer( buffer, iLen );
		}
		else
			hb_xfree( buffer );
	}
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_BLOB_REOPEN )
{
	sqlite3_blob * pBlob = ( sqlite3_blob * ) hb_parptr( 1 );

	if( pBlob )
		hb_retni( sqlite3_blob_reopen( pBlob, hb_parnint( 2 ) ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_BLOB_WRITE )
{
	sqlite3_blob * pBlob = ( sqlite3_blob * ) hb_parptr( 1 );

	if( pBlob )
	{
		int iLen = hb_parni( 3 );

		if( iLen == 0 )
			iLen = ( int ) hb_parcsiz( 2 ) - 1;

		hb_retni( sqlite3_blob_write( pBlob, hb_parcx( 2 ), iLen, hb_parni( 4 ) ) );
	}
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_BUSY_HANDLER )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
	{
		if( pHbSqlite3->cbBusyHandler )
		{
			hb_itemRelease( pHbSqlite3->cbBusyHandler );
			pHbSqlite3->cbBusyHandler = NULL;
		}

		if( ISPOINTER( 2 ) )
		{
			pHbSqlite3->cbBusyHandler = hb_itemNew( hb_param( 2, HB_IT_POINTER ) );
			hb_gcUnlock( pHbSqlite3->cbBusyHandler );

			sqlite3_busy_handler( pHbSqlite3->db, busy_handler,
								( void * ) pHbSqlite3->cbBusyHandler );
		}
		else
			sqlite3_busy_handler( pHbSqlite3->db, NULL, NULL );
	}
}

HB_FUNC( SQLITE3_BUSY_TIMEOUT )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
		hb_retni( sqlite3_busy_timeout( pHbSqlite3->db, hb_parni( 2 ) ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_CHANGES )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
		hb_retni( sqlite3_changes( pHbSqlite3->db ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_CLEAR_BINDINGS )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retni( sqlite3_clear_bindings( pStmt ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_CLOSE )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
	{
		hb_retni( sqlite3_close( pHbSqlite3->db ) );
	}
	else
	{
		hb_retni( -1 );
    }
}

HB_FUNC( SQLITE3_COMMIT_HOOK )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
	{
		if( pHbSqlite3->cbHookCommit )
		{
			hb_itemRelease( pHbSqlite3->cbHookCommit );
			pHbSqlite3->cbHookCommit = NULL;
		}

		if( ISPOINTER( 2 ) )
		{
			pHbSqlite3->cbHookCommit = hb_itemNew( hb_param( 2, HB_IT_POINTER ) );
			hb_gcUnlock( pHbSqlite3->cbHookCommit );

			sqlite3_commit_hook( pHbSqlite3->db, hook_commit, ( void * ) pHbSqlite3->cbHookCommit );
		}
		else
			sqlite3_commit_hook( pHbSqlite3->db, NULL, NULL );
   }
}

HB_FUNC( SQLITE3_ROLLBACK_HOOK )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
	{
		if( pHbSqlite3->cbHookRollback )
		{
			hb_itemRelease( pHbSqlite3->cbHookRollback );
			pHbSqlite3->cbHookRollback = NULL;
		}

		if( ISPOINTER( 2 ) )
		{
			pHbSqlite3->cbHookRollback = hb_itemNew( hb_param( 2, HB_IT_POINTER ) );
			hb_gcUnlock( pHbSqlite3->cbHookRollback );

			sqlite3_rollback_hook( pHbSqlite3->db, hook_rollback,
									( void * ) pHbSqlite3->cbHookRollback );
		}
		else
			sqlite3_rollback_hook( pHbSqlite3->db, NULL, NULL );
	}
}

HB_FUNC( SQLITE3_COMPILEOPTION_USED )
{
	hb_retl( ( BOOL ) sqlite3_compileoption_used( hb_parc( 1 ) ) );
}

HB_FUNC( SQLITE3_COMPILEOPTION_GET )
{
	hb_retc( sqlite3_compileoption_get( hb_parni( 1 ) ) );
}

HB_FUNC( SQLITE3_COLUMN_BLOB )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
	{
		int index = hb_parni( 2 ) - 1;
		hb_retclen( ( const char * ) sqlite3_column_blob( pStmt,index ), sqlite3_column_bytes( pStmt, index ) );
	}
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_COLUMN_BYTES )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retni( sqlite3_column_bytes( pStmt, hb_parni( 2 ) - 1 ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_COLUMN_COUNT )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retni( sqlite3_column_count( pStmt ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_COLUMN_DATABASE_NAME )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retc( sqlite3_column_database_name( pStmt, hb_parni( 2 ) - 1 ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_COLUMN_DECLTYPE )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retstr_utf8( sqlite3_column_decltype( pStmt, hb_parni( 2 ) - 1 ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_COLUMN_DOUBLE )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retnd( sqlite3_column_double( pStmt, hb_parni( 2 ) - 1 ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_COLUMN_INT )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retni( sqlite3_column_int( pStmt, hb_parni( 2 ) - 1 ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_COLUMN_INT64 )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retnint( sqlite3_column_int64( pStmt, hb_parni( 2 ) - 1 ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_COLUMN_NAME )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retstr_utf8( sqlite3_column_name( pStmt, hb_parni( 2 ) - 1 ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_TABLE_COLUMN_METADATA )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
	{
		char const *   pzDataType = NULL;
		char const *   pzCollSeq = NULL;
		int            iNotNull = 0;
		int            iPrimaryKey = 0;
		int            iAutoinc = 0;

		void *         hDbName;
		void *         hTableName;
		void *         hColumnName;

		if
		(
			sqlite3_table_column_metadata
			(
				pHbSqlite3->db,
				hb_parstr_utf8( 2, &hDbName, NULL ),
				hb_parstr_utf8( 3, &hTableName, NULL ),
				hb_parstr_utf8( 4, &hColumnName, NULL ),
				&pzDataType ,
				&pzCollSeq ,
				&iNotNull,
				&iPrimaryKey,
				&iAutoinc
			) == SQLITE_OK
		)
		{
			PHB_ITEM pArray = hb_itemArrayNew( 5 );

			hb_arraySetStrUTF8( pArray, 1, pzDataType );
			hb_arraySetStrUTF8( pArray, 2, pzCollSeq );
			hb_arraySetL( pArray, 3, ( BOOL ) ( iNotNull != 0 ) );
			hb_arraySetL( pArray, 4, ( BOOL ) ( iPrimaryKey != 0 ) );
			hb_arraySetL( pArray, 5, ( BOOL ) ( iAutoinc != 0 ) );

			hb_itemReturnRelease( pArray );
		}

		hb_strfree( hDbName );
		hb_strfree( hTableName );
		hb_strfree( hColumnName );
	}
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_COLUMN_ORIGIN_NAME )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retstr_utf8( sqlite3_column_origin_name( pStmt, hb_parni( 2 ) - 1 ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_COLUMN_TABLE_NAME )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retstr_utf8( sqlite3_column_table_name( pStmt, hb_parni( 2 ) - 1 ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_COLUMN_TEXT )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
	{
		int index = hb_parni( 2 ) - 1;
		hb_retstrlen_utf8( ( const char * ) sqlite3_column_text( pStmt,
                                                               index ),
							sqlite3_column_bytes( pStmt, index ) );
	}
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_COLUMN_TYPE )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retni( sqlite3_column_type( pStmt, hb_parni( 2 ) - 1 ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_COMPLETE )
{
	void * hSQLText;

	hb_retl( sqlite3_complete( hb_parstr_utf8( 1, &hSQLText, NULL ) ) );

	hb_strfree( hSQLText );
}

HB_FUNC( SQLITE3_LOAD_EXTENSION )
{
#ifndef SQLITE_OMIT_LOAD_EXTENSION
   HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

   if( pHbSqlite3 && pHbSqlite3->db )
      hb_retni( sqlite3_load_extension( pHbSqlite3->db, hb_parc( 2 ), 0, 0 ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
#else
   hb_retni( -1 );
#endif /* SQLITE_OMIT_LOAD_EXTENSION */

}

HB_FUNC( SQLITE3_ENABLE_LOAD_EXTENSION )
{
#ifndef SQLITE_OMIT_LOAD_EXTENSION
   HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

   if( pHbSqlite3 && pHbSqlite3->db )
      hb_retni( sqlite3_enable_load_extension( pHbSqlite3->db, hb_parl( 2 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
#else
   hb_retni( -1 );
#endif /* SQLITE_OMIT_LOAD_EXTENSION */
}

HB_FUNC( SQLITE3_ENABLE_SHARED_CACHE )
{
	hb_retni( sqlite3_enable_shared_cache( hb_parl( 1 ) ) );
}

HB_FUNC( SQLITE3_ERRCODE )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
		hb_retni( sqlite3_errcode( pHbSqlite3->db ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_ERRMSG )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
		hb_retstr_utf8( sqlite3_errmsg( pHbSqlite3->db ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_EXEC )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
	{
	    void *   hSQLText;
		char *   pszErrMsg = NULL;
		int      rc;

		if( ISPOINTER( 3 ) )
		{
			rc = sqlite3_exec( pHbSqlite3->db, hb_parstr_utf8( 2, &hSQLText,
                                                            NULL ), callback, ( void * ) hb_param( 3, HB_IT_POINTER ),&pszErrMsg );
		}
		else
		{
			 rc = sqlite3_exec( pHbSqlite3->db, hb_parstr_utf8( 2, &hSQLText,
                                                            NULL ), NULL, 0, &pszErrMsg );
		}

		if( rc != SQLITE_OK )
		{
			HB_TRACE( HB_TR_DEBUG, ( "sqlite3_exec(): Returned error: %s", pszErrMsg ) );
			sqlite3_free( pszErrMsg );
		}

		hb_strfree( hSQLText );

		hb_retni( rc );
	}
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_EXTENDED_ERRCODE )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
		hb_retni( sqlite3_extended_errcode( pHbSqlite3->db ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_EXTENDED_RESULT_CODES )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
		hb_retni( sqlite3_extended_result_codes( pHbSqlite3->db, hb_parl( 2 ) ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_FINALIZE )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retni( sqlite3_finalize( pStmt ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_GET_AUTOCOMMIT )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
		hb_retl( sqlite3_get_autocommit( pHbSqlite3->db ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_GET_TABLE )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
	{
		void *   hSQLText;
		PHB_ITEM pResultList = hb_itemArrayNew( 0 );
		int      iRow, iCol;
		char *   pszErrMsg = NULL;
		char **  pResult;

		if( sqlite3_get_table( pHbSqlite3->db,
								hb_parstr_utf8( 2, &hSQLText,
												NULL ), &pResult, &iRow, &iCol,
								&pszErrMsg ) == SQLITE_OK )
		{
			int i, j, k = 0;

			for( i = 0; i < iRow + 1; i++ )
			{
				PHB_ITEM pArray = hb_itemArrayNew( iCol );

				for( j = 1; j <= iCol; j++, k++ )
					hb_arraySetStrUTF8( pArray, j, ( const char * ) pResult[ k ] );

				hb_arrayAddForward( pResultList, pArray );
				hb_itemRelease( pArray );
			}
		}
		else
		{
			HB_TRACE( HB_TR_DEBUG, ( "sqlite3_get_table(): Returned error: %s", pszErrMsg ) );
			sqlite3_free( pszErrMsg );
		}

		sqlite3_free_table( pResult );

		hb_strfree( hSQLText );

		hb_itemReturnRelease( pResultList );
	}
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_INITIALIZE )
{
	hb_retni( sqlite3_initialize() );
}

HB_FUNC( SQLITE3_INTERRUPT )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
		sqlite3_interrupt( pHbSqlite3->db );
}

HB_FUNC( SQLITE3_LAST_INSERT_ROWID )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
		hb_retnint( sqlite3_last_insert_rowid( pHbSqlite3->db ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_LIBVERSION )
{
	hb_retc( sqlite3_libversion() );
}

HB_FUNC( SQLITE3_LIBVERSION_NUMBER )
{
	hb_retni( sqlite3_libversion_number() );
}

HB_FUNC( SQLITE3_LIMIT )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db && ( hb_pcount() > 2 ) && ISNUM( 2 ) && ISNUM( 3 ) )
		hb_retni( sqlite3_limit( pHbSqlite3->db, hb_parni( 2 ), hb_parni( 3 ) ) );
	else
      hb_retni( -1 );
}

HB_FUNC( SQLITE3_MEMORY_HIGHWATER )
{
	hb_retnint( sqlite3_memory_highwater( ( int ) hb_parl( 1 ) ) );
}

HB_FUNC( SQLITE3_MEMORY_USED )
{
	hb_retnint( sqlite3_memory_used() );
}

HB_FUNC( SQLITE3_OPEN )
{
	sqlite3 *      db;
	char *         pszFree;
	const char *   pszdbName = hb_fsNameConv( hb_parcx( 1 ), &pszFree );

	if( hb_fsFileExists( pszdbName ) || hb_parl( 2 ) )
	{
		if( sqlite3_open( pszdbName, &db ) == SQLITE_OK )
		{
			HB_SQLITE3 * hbsqlite3;

			hbsqlite3 = ( HB_SQLITE3 * ) hb_xgrab( sizeof( HB_SQLITE3 ) );
			hb_xmemset( hbsqlite3, 0, sizeof( HB_SQLITE3 ) );
			hbsqlite3->db = db;
			hb_sqlite3_ret( hbsqlite3, HB_SQLITE3_DB );
		}
		else
		{
			sqlite3_close( db );

			hb_retptr( NULL );
		}
	}
	else
	{
		HB_TRACE( HB_TR_DEBUG, ( "sqlite3_open(): Database doesn't exist %s", pszdbName ) );

		hb_retptr( NULL );
	}

	if( pszFree )
		hb_xfree( pszFree );
}

HB_FUNC( SQLITE3_OPEN_V2 )
{
	sqlite3 *      db;
	char *         pszFree;
	const char *   pszdbName = hb_fsNameConv( hb_parcx( 1 ), &pszFree );

	if( sqlite3_open_v2( pszdbName, &db, hb_parni( 2 ), NULL ) == SQLITE_OK )
	{
		HB_SQLITE3 * hbsqlite3;

		hbsqlite3 = ( HB_SQLITE3 * ) hb_xgrab( sizeof( HB_SQLITE3 ) );
		hb_xmemset( hbsqlite3, 0, sizeof( HB_SQLITE3 ) );
		hbsqlite3->db = db;
		hb_sqlite3_ret( hbsqlite3, HB_SQLITE3_DB );
	}
	else
	{
	    sqlite3_close( db );

		hb_retptr( NULL );
	}

	if( pszFree )
		hb_xfree( pszFree );
}

HB_FUNC( SQLITE3_PREPARE )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
	{
		PHB_ITEM SQL = hb_param( 2, HB_IT_STRING );

		if( SQL )
		{
			const char *   pSQL = hb_itemGetCPtr( SQL );
			int            ulLen = ( int ) hb_itemGetCLen( SQL );
			psqlite3_stmt  pStmt;
			const char *   pszTail;

			if( sqlite3_prepare_v2( pHbSqlite3->db, pSQL, ulLen, &pStmt, &pszTail ) == SQLITE_OK )
			{
				hb_retptr( pStmt );
			}
			else
			{
				sqlite3_finalize( pStmt );

				hb_retptr( NULL );
			}
		}
		else
			hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 2 ) );
   }
   else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_PREPARE_V2 )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
	{
		PHB_ITEM SQL = hb_param( 2, HB_IT_STRING );

		if( SQL )
		{
			const char *   pSQL = hb_itemGetCPtr( SQL );
			int            ulLen = ( int ) hb_itemGetCLen( SQL );
			psqlite3_stmt  pStmt;
			const char *   pszTail;

			if( sqlite3_prepare_v2( pHbSqlite3->db, pSQL, ulLen, &pStmt, &pszTail ) == SQLITE_OK )
			{
				hb_retptr( pStmt );
			}
			else
			{
				sqlite3_finalize( pStmt );

				hb_retptr( NULL );
			}
		}
		else
			hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 2 ) );
   }
   else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_PROFILE )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
		sqlite3_profile( pHbSqlite3->db, hb_parl( 2 ) ? SQL3ProfileLog : NULL,
						( void * ) ( ISCHAR( 3 ) ? hb_parcx( 3 ) : NULL ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_PROGRESS_HANDLER )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
	{
		if( pHbSqlite3->cbProgressHandler )
		{
			hb_itemRelease( pHbSqlite3->cbProgressHandler );
			pHbSqlite3->cbProgressHandler = NULL;
		}

		if( ISNUM( 2 ) && ISPOINTER( 3 ) )
		{
			pHbSqlite3->cbProgressHandler = hb_itemNew( hb_param( 3, HB_IT_POINTER ) );
			hb_gcUnlock( pHbSqlite3->cbProgressHandler );

			sqlite3_progress_handler( pHbSqlite3->db, hb_parni( 2 ), progress_handler,
                                   ( void * ) pHbSqlite3->cbProgressHandler );
		}
		else
			sqlite3_progress_handler( pHbSqlite3->db, 0, NULL, NULL );
	}
}

HB_FUNC( SQLITE3_RESET_AUTO_EXTENSION )
{
	sqlite3_reset_auto_extension();
}

HB_FUNC( SQLITE3_RESET )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retni( sqlite3_reset( pStmt ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_SHUTDOWN )
{
	hb_retni( sqlite3_shutdown() );
}

HB_FUNC( SQLITE3_SLEEP )
{
	hb_retni( sqlite3_sleep( hb_parni( 1 ) ) );
}

HB_FUNC( SQLITE3_SOURCEID )
{
#if SQLITE_VERSION_NUMBER >= 3006018
   hb_retc( sqlite3_sourceid() );
#else
   hb_retc_null();
#endif /* SQLITE_VERSION_NUMBER >= 3006018 */
}

HB_FUNC( SQLITE3_SQL )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retstr_utf8( sqlite3_sql( pStmt ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_STATUS )
{
	int iCurrent, iHighwater;

	if( hb_pcount() > 3 && ( ISNUM( 2 ) && ISBYREF( 2 ) ) && ( ISNUM( 3 ) && ISBYREF( 3 ) ) )
	{
		hb_retni( sqlite3_status( hb_parni( 1 ), &iCurrent, &iHighwater, ( int ) hb_parl( 4 ) ) );

		hb_storni( iCurrent, 2 );
		hb_storni( iHighwater, 3 );
	}
	else
		hb_retni( -1 );
}

HB_FUNC( SQLITE3_STEP )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retni( sqlite3_step( pStmt ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_STMT_READONLY )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retl( ( BOOL ) sqlite3_stmt_readonly( pStmt ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_STMT_STATUS )
{
	psqlite3_stmt pStmt = ( psqlite3_stmt ) hb_parptr( 1 );

	if( pStmt )
		hb_retni( sqlite3_stmt_status( pStmt, hb_parni( 2 ), ( int ) hb_parl( 3 ) ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_TEMP_DIRECTORY )
{
	BOOL bResult = FALSE;

	{
		char *         pszFree;
		const char *   pszDirName = hb_fsNameConv( hb_parcx( 1 ), &pszFree );

		if( hb_fsIsDirectory( pszDirName ) )
		{
			bResult = TRUE;
		}
		else
		{
			if( hb_parl( 2 ) )  /* create temp directory if not exist */
			{
				if( hb_fsMkDir( pszDirName ) )
				{
					bResult = TRUE;
				}
				else
				{
					HB_TRACE( HB_TR_DEBUG,
                         ( "sqlite_temp_directory(): Can't create directory %s", pszDirName ) );
				}
			}
			else
			{
				HB_TRACE( HB_TR_DEBUG,
                      ( "sqlite_temp_directory(): Directory doesn't exist %s", pszDirName ) );
			}
		}

		if( bResult )
			sqlite3_temp_directory = hb_strdup( pszDirName );

		if( pszFree )
			hb_xfree( pszFree );
	}
	hb_retl( bResult );
}

HB_FUNC( SQLITE3_THREADSAFE )
{
	hb_retni( sqlite3_threadsafe() );
}

HB_FUNC( SQLITE3_TOTAL_CHANGES )
{
	HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

	if( pHbSqlite3 && pHbSqlite3->db )
		hb_retni( sqlite3_total_changes( pHbSqlite3->db ) );
	else
		hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_TRACE )
{
   HB_SQLITE3 * pHbSqlite3 = ( HB_SQLITE3 * ) hb_sqlite3_param( 1, HB_SQLITE3_DB, TRUE );

   if( pHbSqlite3 && pHbSqlite3->db )
      sqlite3_trace( pHbSqlite3->db, hb_parl( 2 ) ? SQL3TraceLog : NULL,
                     ( void * ) ( ISCHAR( 3 ) ? hb_parcx( 3 ) : NULL ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

HB_FUNC( SQLITE3_FILE_TO_BUFF )
{
	HB_FHANDLE handle = hb_fsOpen( hb_parcx( 1 ), FO_READ );

	if( handle != FS_ERROR )
	{
		char *   buffer;
		int  nSize;

		nSize = hb_fsSeek( handle, 0, FS_END );
		hb_fsSeek( handle, 0, FS_SET );
		buffer = ( char * ) hb_xgrab( nSize + 1 );
		nSize = ( int ) hb_fsReadLarge( handle, buffer, nSize );
		buffer[ nSize ] = '\0';
		hb_fsClose( handle );

		hb_retclen_buffer( buffer, nSize );
	}
	else
		hb_retc_null();
}

HB_FUNC( SQLITE3_BUFF_TO_FILE )
{
	HB_FHANDLE  handle = hb_fsCreate( hb_parcx( 1 ), FC_NORMAL );
	unsigned int nSize = ( unsigned int ) (hb_parcsiz( 2 ) - 1);

	if( handle != FS_ERROR && nSize > 0 )
	{
		hb_retni( hb_fsWriteLarge( handle, hb_parcx( 2 ), nSize ) == nSize ? 0 : -1 );
		hb_fsClose( handle );
	}
	else
		hb_retni( 1 );
}
