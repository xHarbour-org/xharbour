/*
 * $Id: mysql.c,v 1.7 2004/03/04 21:49:09 peterrees Exp $
 */

/*
 * Harbour Project source code:
 * MySQL DBMS low level (client api) interface code.
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
 * www - http://www.harbour-project.org
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
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
 *    DATATOSQL(),FILETOSQLBINARY()
 *
 * See doc/license.txt for licensing terms.
 *
 */


/* NOTE: we need this to prevent base types redefinition */
#define _CLIPDEFS_H
#if defined(HB_OS_WIN_32_USED)
   #include <windows.h>
#endif

#include "hbapifs.h"
#include "extend.api"
#include "item.api"
#include "mysql.h"
#include <stdio.h>
#include <hb_io.h>
#include <fcntl.h>


/* NOTE: OS/2 EMX port of MySQL needs libmysqlclient.a from 3.21.33b build which has st and mt
   versions of client library. I'm using ST version since harbour is single threaded. You need
   also .h files from same distribution
*/

HB_FUNC(SQLCONNECT) // MYSQL *mysql_real_connect(MYSQL*, char * host, char * user, char * password, char * db, uint port, char *, uint flags)
{
   MYSQL * mysql;
   const char *szHost=hb_parc( 1 );
   const char *szUser=hb_parc( 2 );
   const char *szPass=hb_parc( 3 );
   unsigned int port  = ISNUM( 4 ) ? ( unsigned int ) hb_parni(4) :  MYSQL_PORT;
   unsigned int flags = ISNUM( 5 ) ? ( unsigned int ) hb_parni(5) :  0;
#if MYSQL_VERSION_ID > 32200
   /* from 3.22.x of MySQL there is a new parameter in mysql_real_connect() call, that is char * db
      which is not used here */
   mysql = mysql_init((MYSQL*) 0)       ;

   if ( ( mysql != NULL) )
   {
     if( mysql_real_connect( mysql, szHost, szUser, szPass, 0, port, NULL, flags) )
     {
        hb_retnl((long) mysql);
     }
     else
     {
       mysql_close( mysql );
       hb_retnl( 0 );
     }
   }
   else
   {
     hb_retnl( 0 );
   }
#else
   mysql = mysql_real_connect(NULL, _parc(1), _parc(2), _parc(3), 0, NULL, 0);
   hb_retnl((long) mysql);
#endif
}


HB_FUNC(SQLCLOSE) // void mysql_close(MYSQL *mysql)
{
   mysql_close((MYSQL *)_parnl(1));
   hb_ret();
}


HB_FUNC(SQLSELECTD) // int mysql_select_db(MYSQL *, char *)
{
   const   char *db=hb_parc(2);
   hb_retnl((long) mysql_select_db((MYSQL *)_parnl(1), db));
}


HB_FUNC(SQLQUERY) // int mysql_query(MYSQL *, char *)
{
   hb_retnl((long) mysql_query((MYSQL *)_parnl(1), _parc(2)));
}


HB_FUNC(SQLSTORER) // MYSQL_RES *mysql_store_result(MYSQL *)
{
   hb_retnl((long) mysql_store_result((MYSQL *)_parnl(1)));
}


HB_FUNC(SQLFREER) // void mysql_free_result(MYSQL_RES *)
{
   mysql_free_result((MYSQL_RES *)_parnl(1));
   hb_ret();
}


HB_FUNC(SQLFETCHR) // MYSQL_ROW *mysql_fetch_row(MYSQL_RES *)
{
   MYSQL_RES *mresult = (MYSQL_RES *)_parnl(1);
   UINT ui, uiNumFields = mysql_num_fields(mresult);
   ULONG *pulFieldLengths ;
   MYSQL_ROW mrow;
   HB_ITEM itRow, itTemp;
   itRow.type = HB_IT_NIL ;
   itTemp.type = HB_IT_NIL ;

   hb_arrayNew( &itRow, uiNumFields );
   mrow = mysql_fetch_row(mresult);
   pulFieldLengths = mysql_fetch_lengths( mresult ) ;
   if ( mrow  )
   {
     for (ui = 0; ui < uiNumFields; ui++)
     {
       if ( mrow[ ui ] == NULL )
       {
         hb_itemPutC( &itTemp , "" );  // if field is NULL
       }
       else  // Put the actual data in
       {
         hb_itemPutCL( &itTemp, mrow[ ui ], pulFieldLengths[ ui ] );
       }
       hb_arraySetForward( &itRow, ui+1, &itTemp );
     }
   }
   hb_itemReturn(&itRow);

}


HB_FUNC(SQLDATAS) // void mysql_data_seek(MYSQL_RES *, unsigned int)
{
   mysql_data_seek((MYSQL_RES *)_parnl(1), (unsigned int)_parni(2));
   hb_ret();
}


HB_FUNC(SQLNROWS) // my_ulongulong  mysql_num_rows(MYSQL_RES *)
{
   /* NOTE: I receive a my_ulongulong which I convert to a long, so I could lose precision */
   hb_retnl((long)mysql_num_rows(((MYSQL_RES *)_parnl(1))));
}


HB_FUNC(SQLFETCHF) // MYSQL_FIELD *mysql_fetch_field(MYSQL_RES *)
{
   /* NOTE: field structure of MySQL has 8 members as of MySQL 3.22.x */
   MYSQL_FIELD *mfield;
   HB_ITEM itField, itTemp;

   itField.type = HB_IT_NIL ;
   itTemp.type = HB_IT_NIL ;

   mfield = mysql_fetch_field((MYSQL_RES *)_parnl(1));
   hb_arrayNew( &itField, 8 );
   if (!(mfield == NULL))
   {
      hb_arraySetForward(&itField, 1, hb_itemPutC(&itTemp, mfield->name));
      hb_arraySetForward(&itField, 2, hb_itemPutC(&itTemp, mfield->table));
      hb_arraySetForward(&itField, 3, hb_itemPutC(&itTemp, mfield->def));
      hb_arraySetForward(&itField, 4, hb_itemPutNL(&itTemp, (long)mfield->type));
      hb_arraySetForward(&itField, 5, hb_itemPutNL(&itTemp, mfield->length));
      hb_arraySetForward(&itField, 6, hb_itemPutNL(&itTemp, mfield->max_length));
      hb_arraySetForward(&itField, 7, hb_itemPutNL(&itTemp, mfield->flags));
      hb_arraySetForward(&itField, 8, hb_itemPutNL(&itTemp, mfield->decimals));
   }
   hb_itemReturn(&itField);

}


HB_FUNC(SQLFSEEK) // MYSQL_FIELD_OFFSET mysql_field_seek(MYSQL_RES *, MYSQL_FIELD_OFFSET)
{
   mysql_field_seek((MYSQL_RES *)_parnl(1), (MYSQL_FIELD_OFFSET)_parni(2));
   hb_ret();
}


HB_FUNC(SQLNUMFI) // unsigned int mysql_num_fields(MYSQL_RES *)
{
   hb_retnl(mysql_num_fields(((MYSQL_RES *)_parnl(1))));
}

#if MYSQL_VERSION_ID > 32200
HB_FUNC(SQLFICOU) // unsigned int mysql_num_fields(MYSQL_RES *)
{
   hb_retnl(mysql_field_count(((MYSQL *)_parnl(1))));
}
#endif

HB_FUNC(SQLLISTF) // MYSQL_RES *mysql_list_fields(MYSQL *, char *);
{
   hb_retnl((long) mysql_list_fields((MYSQL *)_parnl(1), _parc(2), NULL));
}


HB_FUNC(SQLGETERR) // char *mysql_error(MYSQL *);
{
   hb_retc((char *)mysql_error((MYSQL *)_parnl(1)));
}

HB_FUNC(SQLGETERRNO)
{
   hb_retnl(mysql_errno((MYSQL *)_parnl(1)));
}

HB_FUNC(SQLLISTDB) // MYSQL_RES * mysql_list_dbs(MYSQL *, char * wild);
{
   MYSQL * mysql = (MYSQL *)_parnl(1);
   MYSQL_RES * mresult;
   MYSQL_ROW mrow;
   long nr, i;
   HB_ITEM itDBs, itTemp;

   mresult = mysql_list_dbs(mysql, NULL);

   nr = (LONG) mysql_num_rows(mresult);

   hb_arrayNew( &itDBs, 0 );

   for (i = 0; i < nr; i++)
   {
      mrow = mysql_fetch_row(mresult);
      hb_arrayAddForward((PHB_ITEM) &itDBs , hb_itemPutC(&itTemp, mrow[0]) );
   }

   mysql_free_result(mresult);
   hb_itemReturn(&itDBs);
}


HB_FUNC(SQLLISTTBL) // MYSQL_RES * mysql_list_tables(MYSQL *, char * wild);
{
   MYSQL * mysql = (MYSQL *)_parnl(1);
   MYSQL_RES * mresult;
   MYSQL_ROW mrow;
   long nr, i;
   HB_ITEM itTables, itTemp;
   itTables.type = HB_IT_NIL;
   itTemp.type = HB_IT_NIL ;
   mresult = mysql_list_tables(mysql, NULL);
   nr = (LONG) mysql_num_rows(mresult);

   hb_arrayNew(&itTables,0);

   for (i = 0; i < nr; i++)
   {
      mrow = mysql_fetch_row(mresult);
      hb_arrayAddForward((PHB_ITEM) &itTables , hb_itemPutC(&itTemp, mrow[0]) );
   }
   mysql_free_result(mresult);
   hb_itemReturn(&itTables);
}


// returns bitwise and of first parameter with second
HB_FUNC(SQLAND)
{
   hb_retnl(_parnl(1) & _parnl(2));
}

HB_FUNC(SQLAFFROWS)
{
   hb_retnl( (LONG) mysql_affected_rows( (MYSQL *)_parnl(1) ) );
}

HB_FUNC(SQLHOSTINFO)
{
   hb_retc((char *) mysql_get_host_info( (MYSQL *)_parnl(1) ) );
}

HB_FUNC(SQLSRVINFO)
{
   hb_retc((char *) mysql_get_server_info( (MYSQL *)_parnl(1) ) );
}

#ifdef __GNUC__
long filelength( int handle )
{
    long nEnd = hb_fsSeek( handle, 0 , 2 );
    long nStart = hb_fsSeek( handle , 0 , 0 );
    return nEnd - nStart;
}
#endif


HB_FUNC(DATATOSQL)
{
   char *FromBuffer ;
   int iSize, iFromSize ;
   char *ToBuffer;
   BOOL bResult = FALSE ;
   iSize= hb_parclen(1) ;
   iFromSize = iSize ;

   if ( iSize )
   {
     FromBuffer = hb_parc( 1 ) ;
     ToBuffer = ( char *) hb_xgrab( ( iSize*2 ) + 1 );
     if ( ToBuffer )
     {
       if ISNUM(2)
       {
         iSize = mysql_real_escape_string( (MYSQL *) hb_parnl(2), ToBuffer, FromBuffer, iSize);
       }
       else
       {
         iSize = mysql_escape_string( ToBuffer, FromBuffer, iSize );
       }
       hb_retclenAdopt( ( char *) ToBuffer, iSize ) ;
       bResult = TRUE ;
     }
   }
   if ( !bResult )
   {
     // Should we raise a runtime error here????? or just return the original string
     hb_retclen( (char *) FromBuffer, iFromSize ) ;
   }
}

HB_FUNC(FILETOSQLBINARY)
{
   BOOL bResult = FALSE ;
   char *szFile=hb_parc(1);
   int fHandle, iSize;
   char *ToBuffer;
   char *FromBuffer;
   if ( szFile && hb_parclen(1) )
   {
     fHandle    = hb_fsOpen(( BYTE *) szFile,2);
     if ( fHandle > 0 )
     {
       iSize      = filelength( fHandle );
       FromBuffer = ( char *) hb_xgrab( iSize );
       if ( FromBuffer )
       {
         iSize      = hb_fsReadLarge( fHandle , ( BYTE * ) FromBuffer , iSize );
         if ( iSize )
         {
           ToBuffer   = ( char *) hb_xgrab( ( iSize*2 ) + 1 );
           if ( ToBuffer )
           {
             if ISNUM(2)
             {
               iSize = mysql_real_escape_string( (MYSQL *) hb_parnl(2), ToBuffer, FromBuffer, iSize);
             }
             else
             {
               iSize = mysql_escape_string( ToBuffer, FromBuffer, iSize);
             }
             hb_retclenAdopt( ( char *) ToBuffer, iSize);
             bResult = TRUE ;
           }
         }
         hb_xfree( FromBuffer );
       }
       hb_fsClose( fHandle );
     }
   }
   if ( !bResult )
   {
     hb_retc( "" ) ;
   }
}
