/*
 * Chr(36) + "Id" + Chr(36)
 */

/*
 * xHarbour Project source code:
 * PostgreSQL RDBMS low level (client api) interface code.
 *
 * Copyright 2003 Rodrigo Moreno rodrigo_moreno@yahoo.com
 * www - http://www.xharbour.org
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
 * See doc/license.txt for licensing terms.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <hbapi.h>
#include <hbapifs.h>
#include <hbapiitm.h>
#include <libpq-fe.h>

#define _CLIPDEFS_H
#if defined(HB_OS_WIN_32_USED)
   #include <windows.h>
#endif


HB_FUNC(PQCONNECT)
{
    const char conninfo[128];
    PGconn         *conn;
    PHB_ITEM   db_handle;
    
    if (hb_pcount() != 5)
    {
        hb_retc("");
        return;
    }    
    
    sprintf(conninfo, "dbname = %s host = %s user = %s password = %s port = %i", 
                                           hb_parc(1), hb_parc(2), hb_parc(3), hb_parc(4), hb_parni(5) );

    conn = PQconnectdb(conninfo);
    
    if (PQstatus(conn) != CONNECTION_OK)
        {
        hb_retc(PQerrorMessage(conn));
        PQfinish(conn);
        return;
        }
        
        db_handle = hb_itemPutPtr( NULL, ( void * ) conn );        
    hb_itemReturn(db_handle);
    hb_itemRelease(db_handle); 
    
}    

HB_FUNC(PQCLOSE)
{
    if (hb_parinfo(1))
        PQfinish(( PGconn * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) ));
}

HB_FUNC(PQCLEAR)
{
    if (hb_parinfo(1))
        PQclear(( PGresult * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) ));
}

HB_FUNC(PQEXEC)
{
    PGconn     *conn;    
    PGresult   *res;
    PHB_ITEM   qry_handle;
    
    if (hb_pcount() == 2)
    {
        conn = ( PGconn * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
        
        res = PQexec(conn, hb_parc(2));
     
        if (PQresultStatus(res) != PGRES_COMMAND_OK && PQresultStatus(res) != PGRES_TUPLES_OK )
        {
            hb_retc(PQresultErrorMessage(res));
            PQclear(res);
            return;
        }
            
        qry_handle = hb_itemPutPtr( NULL, ( void * ) res );        
        hb_itemReturn(qry_handle);
        hb_itemRelease(qry_handle); 
    }
    else
        hb_retc("");        
}

HB_FUNC(PQEXECPARAMS)
{
    PGconn     *conn;    
    PGresult   *res;
    char       **paramvalues;
    int        i;
    long       n;

    PHB_ITEM   qry_handle;
    PHB_ITEM   aParam;
    
    if (hb_pcount() == 3)
    {
        aParam = hb_param(3,HB_IT_ARRAY);
                
        n = hb_arrayLen(aParam);
        
        paramvalues = (char **) hb_xgrab( sizeof( char ) * n );
        
        for (i=0;i < n;i++) 
            paramvalues[i] = hb_itemGetCPtr(hb_itemArrayGet( aParam, i + 1 ));
                        
        conn = ( PGconn * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
        
        res = PQexecParams(conn, hb_parc(2), n, NULL, paramvalues, NULL, NULL, 1);
        
        hb_xfree(paramvalues);  

        if (PQresultStatus(res) != PGRES_COMMAND_OK && PQresultStatus(res) != PGRES_TUPLES_OK )
        {
            hb_retc(PQresultErrorMessage(res));
            PQclear(res);
            return;
        }
            
        qry_handle = hb_itemPutPtr( NULL, ( void * ) res );        
        hb_itemReturn(qry_handle);
        hb_itemRelease(qry_handle); 
    }
    else
        hb_retc("");        
}

HB_FUNC(PQFCOUNT)
{
    PGresult   *res;
    int nFields = 0;

    if (hb_parinfo(1)) 
    {
        res = ( PGresult * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
        
        if (PQresultStatus(res) == PGRES_TUPLES_OK)        
                nFields = PQnfields(res);            
    }       
            
    hb_retni(nFields);        
}    

HB_FUNC(PQLASTREC)
{
    PGresult   *res;
    int nRows = 0;

    if (hb_parinfo(1))
    {
        res = ( PGresult * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
        
        if (PQresultStatus(res) == PGRES_TUPLES_OK)
            nRows = PQntuples(res);            
    }       
    hb_retni(nRows);          
}    

HB_FUNC(PQGETVALUE)
{
        PGresult   *res;
        int         nRow, nCol;

    if (hb_pcount() == 3)
    {
        res = ( PGresult * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
        
        if (PQresultStatus(res) == PGRES_TUPLES_OK)
        {
            nRow = hb_parni(2) - 1;
            nCol = hb_parni(3) - 1;
        
            if (! PQgetisnull(res, nRow, nCol))
                hb_retc(PQgetvalue(res, nRow, nCol));
        }
    }        
}    


HB_FUNC(PQMETADATA)
{
    PGresult   *res;
    int         nFields, i;
    ITEM aTemp;
    ITEM aNew;
    ITEM temp;

    if (hb_parinfo(1)) 
    {
        res = ( PGresult * ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
        
        if (PQresultStatus(res) == PGRES_TUPLES_OK)        
        {
            nFields = PQnfields(res);            
    
            aNew = hb_itemArrayNew( nFields );

            for (i=0; i < nFields; i++ )
            {
                aTemp = hb_itemArrayNew( 7 );
            
                temp = hb_itemPutC( NULL, PQfname( res, i ) );            
                hb_itemArrayPut( aTemp, 1, temp);
                hb_itemRelease( temp );

                temp = hb_itemPutNL( NULL, PQftable( res, i ) );            
                hb_itemArrayPut( aTemp, 2, temp);
                hb_itemRelease( temp );

                temp = hb_itemPutNI( NULL, PQftablecol( res, i ) );            
                hb_itemArrayPut( aTemp, 3, temp);
                hb_itemRelease( temp );

                temp = hb_itemPutNI( NULL, PQfformat( res, i ) );            
                hb_itemArrayPut( aTemp, 4, temp);
                hb_itemRelease( temp );

                temp = hb_itemPutNL( NULL, PQftype( res, i ) );            
                hb_itemArrayPut( aTemp, 5, temp);
                hb_itemRelease( temp );

                temp = hb_itemPutNI( NULL, PQfmod( res, i ) );            
                hb_itemArrayPut( aTemp, 6, temp);
                hb_itemRelease( temp );
        
                temp = hb_itemPutNI( NULL, PQfsize( res, i ) );            
                hb_itemArrayPut( aTemp, 7, temp);
                hb_itemRelease( temp );
        
                hb_itemArrayPut( aNew, i+1, aTemp );
                hb_itemRelease( aTemp );
            }
            
            hb_itemReturn(aNew);
            hb_itemRelease(aNew);                   
        }
    }                    
}    
