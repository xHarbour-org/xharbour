/* $CATEGORY$SQLRDD/HIDE$FILES$HIDE$
* SQLRDD Oracle native connection version 2
* Copyright (c) 2013 -2014  Luiz Rafael Culik Guimaraes <luiz@xharbour.com.br>
* All Rights Reserved
*/
#define SQLORA2
#include "sqlorastru.h"

enum sqlo_status_codes {
  SQLO_SUCCESS             = 0,   /**< General success code (maps to OCI_SUCCESS) */
  SQLO_ERROR               = -1,      /**< General error code (maps to OCI_ERROR) */
  SQLO_INVALID_HANDLE      = -2,                  /**< Maps to OCI_INVALID_HANDLE */
  SQLO_STILL_EXECUTING     = -3123,              /**< Maps to OCI_STILL_EXECUTING */
  SQLO_CONTINUE            = -24200,                    /**< Maps to OCI_CONTINUE */
  SQLO_SUCCESS_WITH_INFO   = 1,                /**< Maps to OCI_SUCCESS_WITH_INFO */
  SQLO_NEED_DATA           = 99,                       /**< Maps to OCI_NEED_DATA */
  SQLO_NO_DATA             = 100                         /**< Maps to OCI_NO_DATA */
};

#if !defined(__GNUC__) && defined(WIN32)
#define inline __inline

#endif
// static int nb_err  = 0;
// static int nb_warn = 0;

#define print_mt  printf

#define print_text(x)       printf(x)
#define print_frmt(f, x)    printf(f, x)
  #define print_mstr(x)   print_mt(MT("%s"), x)
#define MAX_CONNECTIONS    50
#define MAX_CURSORS        65535
#define MAX_COLUMNS        1024
#define LOGFILE               "oci2.log"
enum SQLO2_iStatus_codes {
  SQLO2_SUCCESS             = 0,   /**< General success code (maps to OCI_SUCCESS) */
  SQLO2_ERROR               = -1,      /**< General error code (maps to OCI_ERROR) */
  SQLO2_INVALID_HANDLE      = -2,                  /**< Maps to OCI_INVALID_HANDLE */
  SQLO2_STILL_EXECUTING     = -3123,              /**< Maps to OCI_STILL_EXECUTING */
  SQLO2_CONTINUE            = -24200,                    /**< Maps to OCI_CONTINUE */
  SQLO2_SUCCESS_WITH_INFO   = 1,                /**< Maps to OCI_SUCCESS_WITH_INFO */
  SQLO2_NEED_DATA           = 99,                       /**< Maps to OCI_NEED_DATA */
  SQLO2_NO_DATA             = 100                         /**< Maps to OCI_NO_DATA */
};

static PHB_DYNS s_pSym_SR_DESERIALIZE = NULL;
static PHB_DYNS s_pSym_SR_FROMJSON = NULL;



// static  OCI_ConnPool *pool=NULL;
//-----------------------------------------------------------------------------//








// typedef struct _OCI_ORASESSION
// {
//    int dbh;                      // Connection handler
//    int stmt;                     // Current statement handler
//    int iStatus;                   // Execution return value
//    int numcols;                  // Result set columns
//    char server_version[128];
//    //bellow for bind vars
//    SQLO2_stmt_handle_t stmtParam;
//    ORA_BIND_COLS *  pLink;
//    unsigned int   ubBindNum;
//    SQLO2_stmt_handle_t stmtParamRes;
// 
//} OCI_ORASESSION;
// typedef struct _OCI_ORASESSION
// {
// 	OCI_Connection *cn;    
//     OCI_Statement *stmt ;
//     OCI_Statement *stmtParamRes;
//     OCI_Resultset *rs;
//     int iStatus;                   // Execution return value
//     int numcols;                  // Result set columns
//     char server_version[1024];
// 
//    ORA_BIND_COLS *  pLink;
//    unsigned int   ubBindNum;
// } OCI_ORASESSION;	
// typedef OCI_ORASESSION * POCI_ORASESSION;

static USHORT OCI_initilized = 0;

#ifdef HAVE_USLEEP
#  define SQLO2_USLEEP usleep(20000)
#else
#  define SQLO2_USLEEP
#endif

#define MAX_CONN    20

//-----------------------------------------------------------------------------//

void err_handler(OCI_Error *err)
{
    int   err_type = OCI_ErrorGetType(err);
    char *err_msg  = (char *)OCI_ErrorGetString(err);

    printf("%s - %s\n", err_type == OCI_ERR_WARNING ? "warning" : "error", err_msg);
}

HB_FUNC( SQLO2_CONNECT )
{
//    POCI_ORASESSION session = (POCI_ORASESSION) hb_xgrab( sizeof( OCI_ORASESSION ) );
   POCI_ORASESSION session = (POCI_ORASESSION)hb_xgrabz( sizeof( OCI_ORASESSION )) ;
//    int lPool = 0 ; //  ISLOG(5)?hb_parl( 5 ) :0;
//    char sPool[30] = {0};

//    memset( session, 0, sizeof( OCI_ORASESSION ) );
   if (!OCI_initilized)
   {
   if (!OCI_Initialize(NULL, NULL,  OCI_ENV_DEFAULT | OCI_ENV_CONTEXT | OCI_ENV_THREADED  )) //  OCI_ENV_CONTEXT))
	      session->iStatus = SQLO2_ERROR;
	   else
	   {
          session->iStatus = SQLO2_SUCCESS ;
      }   
   }
   else
   {
      session->iStatus = SQLO2_SUCCESS;
   }
   
   OCI_initilized ++;

   if (SQLO2_SUCCESS != session->iStatus)
   {
      hb_retni( SQL_ERROR );
   }

//    session->iStatus = SQLO2_connect( &(session->dbh), hb_parcx(1) );
   //if ( lPool ) 
   //{   
   //   if (pool == NULL )   
   //      {
   //      pool = OCI_PoolCreate(hb_parc(1),  hb_parc(2), hb_parc(3), OCI_POOL_CONNECTION, OCI_ORASESSION_DEFAULT, 0, MAX_CONN, 1); 
   //   }
   //   TraceLog("pool.log","pool %p \n",pool);
   //   sprintf( sPool,"session%i", OCI_initilized); 
   //   session->cn = OCI_PoolGetConnection(pool,sPool);//OCI_ConnectionCreate( hb_parc(1),  hb_parc(2), hb_parc(3), OCI_SESSION_DEFAULT);
   //   TraceLog("pool.log","secao %p \n",session->cn);
   //}
   //else
   //{
      session->cn = OCI_ConnectionCreate( hb_parc(1),  hb_parc(2), hb_parc(3), OCI_SESSION_DEFAULT);

   //}
   
   if ( session->cn != NULL )
   {
	    session->iStatus=SQLO_SUCCESS ;
   }    
  

   if (SQLO2_SUCCESS != session->iStatus)
   {
      hb_retni( SQL_ERROR );
   }
   else
   {

      OCI_SetDefaultFormatDate(session->cn,"YYYYMMDD");      
      OCI_SetDefaultLobPrefetchSize(session->cn,4096 ) ;      
      strcpy( session->server_version, OCI_GetVersionServer(session->cn));
      hb_storptr( ( void * ) session, 4);
      hb_retni( SQL_SUCCESS );
   }
}


//-----------------------------------------------------------------------------//

HB_FUNC( SQLO2_DBMSNAME )
{
   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session )
   {
      hb_retc( session->server_version );
   }
   else
   {
      hb_retc( "Not connected to Oracle" );
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( SQLO2_DISCONNECT )
{
   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session )
   {
	   
      OCI_ConnectionFree( session->cn );

      OCI_initilized--;
      if( OCI_initilized == 0 )
      {
	// 	      if (pool ) 
	// 	         OCI_PoolFree(pool);
         OCI_Cleanup();
      }
      hb_xfree( session );
      hb_retni( SQL_SUCCESS );
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}


HB_FUNC( SQLO2_GETERRORDESCR )
{
   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session )
   {
      hb_retc( (char *) OCI_ErrorGetString(OCI_GetLastError()) );
   }
   else
   {
      hb_retc( "Not connected to Oracle" );
   }
}

HB_FUNC( SQLO2_GETERRORCODE )
{
   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session )
   {
       hb_retni( OCI_ErrorGetType(OCI_GetLastError()));
       
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}

HB_FUNC( SQLO2_NUMCOLS )
{
   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session )
   {
      hb_retni( session->numcols );
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}

HB_FUNC( SQLO2_COMMIT )
{
   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session  )
   {
	  
      session->iStatus = OCI_Commit(session->cn) ? 0 : -1;

      if( SQLO2_SUCCESS == session->iStatus )
      {
         hb_retni( SQL_SUCCESS );
      }
      else
      {
         hb_retni( SQL_ERROR );
      }
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}

//-----------------------------------------------------------------------------//

HB_FUNC( SQLO2_ROLLBACK )
{
   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session )
   {
      session->iStatus = OCI_Rollback(session->cn)  ? 0 : -1;
      if( SQLO2_SUCCESS == session->iStatus )
      {
         hb_retni( SQL_SUCCESS );
      }
      else
      {
         hb_retni( SQL_ERROR );
      }
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}


HB_FUNC( SQLO2_EXECDIRECT )
{
   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   const char * stm = hb_parcx(2);

   if( session )
   {
      session->stmt = OCI_StatementCreate(session->cn);

      if ( OCI_ExecuteStmt( session->stmt,stm ))
      {

          hb_retni( SQL_SUCCESS );
      }
      else
      {
          hb_retni( SQL_ERROR );
      }

      OCI_StatementFree( session->stmt ) ;         
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}
  
  
HB_FUNC( SQLO2_EXECUTE )
{
   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   BOOL  lStmt = ISLOG( 3 ) ? hb_parl( 3 ) : 0;
   if( session )
   {
      char * stm = (char * ) hb_parc(2);
      if (lStmt )
      {
         OCI_SetPrefetchSize(session->stmt,100 ) ;

         if ( ! OCI_Execute(session->stmt) ) 
         {

            session->numcols = 0;
            hb_retni( SQL_ERROR );
         }
         else
         {
            //session->numcols = SQLO2_ncols( session->stmt, 0 );
            session->rs = OCI_GetResultset(session->stmt);
            session->numcols = OCI_GetColumnCount(session->rs) ;
            hb_retni( SQL_SUCCESS );
         }
      }
      else
      {
         session->stmt = OCI_StatementCreate(session->cn);
         //OCI_SetFetchMode( session->stmt,OCI_SFM_SCROLLABLE);
         //OCI_SetFetchSize(session->stmt,100);
         OCI_SetPrefetchSize(session->stmt,100 ) ;

         if ( !OCI_ExecuteStmt( session->stmt,stm ))
         {
            session->numcols = 0;
            hb_retni( SQL_ERROR );
         }
         else
         {
            //session->numcols = SQLO2_ncols( session->stmt, 0 );
            session->rs = OCI_GetResultset(session->stmt);
            session->numcols = OCI_GetColumnCount(session->rs) ;
           hb_retni( SQL_SUCCESS );
         }
      }         
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}







HB_FUNC( ORACLEINBINDPARAM2 )
{

   POCI_ORASESSION Stmt= (POCI_ORASESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   int iParamNum  = hb_parni(2);
   int iParamType = hb_parni(3);
   int iFieldSize = hb_parni(4);
   int iPos = iParamNum-1;
   int ret = SQL_ERROR;
   BOOL lStmt = ISLOG( 7 ) ? hb_parl( 7 ) : 0;
   BOOL isNull = ISLOG( 8 ) ? hb_parl( 8 ) : 0;  


   if ( Stmt)
   {
  
      Stmt->pLink[iPos].bindname = (char *) hb_xgrabz(  sizeof(char) * 10);
//      memset(Stmt->pLink[iPos].bindname,'\0',10 * sizeof(char));
      sprintf(Stmt->pLink[iPos].bindname,":%i",iParamNum);
      Stmt->pLink[iPos].iFieldSize=iFieldSize;

      Stmt->pLink[iPos].sVal = isNull ? -1 : 0;
      Stmt->pLink[iPos].iType = iParamType;
      switch (Stmt->pLink[iPos].iType)
      {
         case 2 :
         {
             if (ISNUM( 6 ) )
             {
                Stmt->pLink[ iPos ].ulValue = hb_parnl( 6 );
               ret =  OCI_BindUnsignedInt( Stmt->stmt,Stmt->pLink[iPos].bindname,&Stmt->pLink[ iPos ].ulValue ) ;
               
             }  
             else
            {
               ret =  OCI_BindUnsignedInt( Stmt->stmt,Stmt->pLink[iPos].bindname,&Stmt->pLink[ iPos ].ulValue ) ;
               
            }
         }
         break;
         case 3 :
         {
             if (ISNUM( 6 ) )
             {
                Stmt->pLink[ iPos ].iValue = hb_parl( 6 );
                OCI_BindBigInt( Stmt->stmt,Stmt->pLink[iPos].bindname,&Stmt->pLink[ iPos ].iValue ) ;
             }
             else
             {
             ret = OCI_BindBigInt(Stmt->stmt, Stmt->pLink[iPos].bindname,&Stmt->pLink[ iPos ].iValue  ) ;
             }
             
         }
          break;      

         case  4 :
         {
            if (ISNUM( 6 ) )
            {
               Stmt->pLink[ iPos ].dValue = hb_parnd( 6 );
               OCI_BindDouble(Stmt->stmt,Stmt->pLink[iPos].bindname,&Stmt->pLink[ iPos ].dValue ) ;
             
            }  
            else
            {              
               ret = OCI_BindDouble(Stmt->stmt,Stmt->pLink[iPos].bindname,&Stmt->pLink[ iPos ].dValue ) ;
             
            }
         }
         break;
         case 5:
         {
             if (ISNUM( 6 ) )
             {
                Stmt->pLink[ iPos ].lValue = hb_parnll( 6 );
                OCI_BindBigInt( Stmt->stmt,Stmt->pLink[iPos].bindname,&Stmt->pLink[ iPos ].lValue ) ;
             }
             else
             {
             ret = OCI_BindBigInt(Stmt->stmt, Stmt->pLink[iPos].bindname,&Stmt->pLink[ iPos ].lValue  ) ;
             }
             
         }
          break;                  
         case 6 :
         {
          //ret =  SQLO2_bind_ref_cursor(Stmt->stmtParam, ":c1", &Stmt->stmtParamRes);
          ret = OCI_BindStatement(Stmt->stmt,":1",Stmt->stmtParamRes );
         }
         break;               
         case 8 :
         {
	        Stmt->pLink[ iPos ].date = OCI_DateCreate(Stmt->cn);
            if ( ISDATE( 6 ) )
            {
               int iYear, iMonth, iDay;
               PHB_ITEM pFieldData = hb_param(6,HB_IT_DATE);
               hb_dateDecode( hb_itemGetDL( pFieldData ), &iYear, &iMonth, &iDay );
               
               OCI_DateSetDate(Stmt->pLink[ iPos ].date,iYear, iMonth, iDay) ;
               ret = OCI_BindDate(Stmt->stmt, Stmt->pLink[iPos].bindname, Stmt->pLink[ iPos ].date);
            }   
            else
            {
            ret = OCI_BindDate(Stmt->stmt, Stmt->pLink[iPos].bindname,Stmt->pLink[ iPos ].date );
            }
         }
         break;
         
         case 9 :
         {
	         Stmt->pLink[ iPos ].date = OCI_DateCreate(Stmt->cn);
         #ifdef __XHARBOUR__	     
            if ( ISDATETIME( 6 ) )
         #else
            if ( HB_ISDATETIME( 6 ) )
         #endif          
            { 
	           int iYear, iMonth, iDay; 
               int  iHour,  iMin;
               #ifdef __XHARBOUR__
               double  dSec;
               #else
               int mSec;
               int iSeconds;
               #endif
	           PHB_ITEM pFieldData = hb_param(6,HB_IT_DATETIME);
	           #ifdef __XHARBOUR__
                  hb_dateDecode( hb_itemGetDL( pFieldData ), &iYear, &iMonth, &iDay );
                  hb_timeDecode( hb_itemGetT(  pFieldData) , &iHour, &iMin, &dSec );         
               #else
                  long  plJulian;
                  long  plMilliSec ;
                  hb_itemGetTDT(pFieldData,&plJulian, &plMilliSec );
                  hb_dateDecode( plJulian, &iYear, &iMonth, &iDay );
                  hb_timeDecode( plMilliSec , &iHour, &iMin, &iSeconds, &mSec );         
               
               #endif
               
               
               #ifdef __XHARBOUR__
                  OCI_DateSetDateTime(Stmt->pLink[ iPos ].date,iYear, iMonth, iDay,iHour,iMin,(int)dSec) ;         
               #else
                  OCI_DateSetDateTime(Stmt->pLink[ iPos ].date,iYear, iMonth, iDay,iHour,iMin,iSeconds) ;         
               #endif
                  OCI_BindDate(Stmt->stmt, Stmt->pLink[iPos].bindname, Stmt->pLink[ iPos ].date);
           }    
           else
           {
         
             ret = OCI_BindDate(Stmt->stmt, Stmt->pLink[iPos].bindname, Stmt->pLink[ iPos ].date);
           }
         }
         break;

           

         default :
         {
            Stmt->pLink[iPos].col_name = (char *) hb_xgrabz( sizeof(char) * (iFieldSize + 1));
//            memset(Stmt->pLink[iPos].col_name,'\0',(iFieldSize+1) * sizeof(char));
         
         
            if ( ISCHAR( 6 ) )
            {
               hb_xmemcpy(Stmt->pLink[ iPos ].col_name, hb_parc( 6 ),hb_parclen( 6 ) ) ;
               Stmt->pLink[ iPos ].col_name[hb_parclen( 6 )] ='\0';
               ret = OCI_BindString(Stmt->stmt, Stmt->pLink[iPos].bindname,Stmt->pLink[ iPos ].col_name, hb_parclen( 6 ) ) ;               
            }         
            else
            {
               ret =OCI_BindString(Stmt->stmt, Stmt->pLink[iPos].bindname,Stmt->pLink[ iPos ].col_name, iFieldSize  ) ;                    
               
            }
         }   
        break;
     }


   }
   
   if (Stmt->pLink[iPos].sVal == -1 ) 
   {
	   OCI_BindSetNull(  OCI_GetBind( Stmt->stmt , iParamNum ) );
   }
   ret = ret ? 1 : SQL_ERROR ;
   hb_retni( ret );
}

  
  

///// getbinddata

HB_FUNC( ORACLEGETBINDDATA2)
{

   POCI_ORASESSION p = (POCI_ORASESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   int iPos;
   

   PHB_ITEM p1 = hb_param( 2, HB_IT_ANY );

   if ( HB_IS_NUMBER( p1 ) &&   p )
   {

      iPos = hb_itemGetNI( p1 );
      if( p->pLink[ iPos - 1 ].iType == 2)
      {
          hb_retnll(p->pLink[ iPos - 1 ].ulValue);
      }
      else if( p->pLink[ iPos - 1 ].iType == 4 )
      {

         hb_retnd(p->pLink[ iPos - 1 ].dValue);
      }      
      else if( p->pLink[ iPos - 1 ].iType == 5 )
      {

         hb_retnll(p->pLink[ iPos - 1 ].lValue);
      }      
      else if( p->pLink[ iPos - 1 ].iType == 8)
      {
	    int iYear, iMonth, iDay; 
// 	    p->pLink[ iPos -1 ].date   = OCI_GetDate(  p->rs,iPos) ;
	    OCI_DateGetDate(p->pLink[ iPos -1 ].date, &iYear, &iMonth, &iDay); 
        hb_retd( iYear, iMonth, iDay );
      }  
      else if( p->pLink[ iPos - 1 ].iType == 9 )
      {
	    int iYear, iMonth, iDay; 
        int  iHour,  iMin;
        int iSeconds;

        long lDate;
        long lTime ;
        
	    
// 	    p->pLink[ iPos -1 ].date   = OCI_GetDate(  p->rs,iPos) ;
	    OCI_DateGetDateTime(p->pLink[ iPos -1 ].date, &iYear, &iMonth, &iDay,&iHour,&iMin,&iSeconds); 
	    lDate = hb_dateEncode( iYear, iMonth, iDay );
        lTime =hb_timeEncode(  iHour, iMin, (double) iSeconds );  

	    hb_retdtl ( lDate,lTime ) ;

      }  
      
      else
      {
         hb_retc( p->pLink[ iPos - 1 ].col_name );
      }
      return ;

   }
   hb_retc("");
}


HB_FUNC(ORACLEEXECDIR2)
{
   POCI_ORASESSION session= (POCI_ORASESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   int ret = SQL_ERROR ;
   int ret1;
   if ( session )
   {

      ret1 = OCI_Execute(session->stmt) ;
      if ( ret1)
      {

         
         OCI_StatementFree(session->stmt );
         session->stmt=NULL;
//          session->rs = OCI_GetResultset(session->stmt);
         hb_retni( 0 ) ;
         return;
     
       }       
       OCI_StatementFree(session->stmt );
             
   }
   hb_retni( ret );

}


HB_FUNC(ORACLEPREPARE2)
{
   POCI_ORASESSION session= (POCI_ORASESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   const char * szSql = hb_parc( 2 ) ;
   BOOL lStmt = ISLOG( 3 ) ? hb_parl( 3 ) : 0;
   int ret = -1 ;

   if ( session )
   {
	   if ( lStmt ) 
	   {
         session->stmt = OCI_StatementCreate(session->cn);
	      ret=  OCI_Prepare(session->stmt,szSql); 
        
      }
	   else
	   {
         session->stmt = OCI_StatementCreate(session->cn);
	      ret=  OCI_Prepare(session->stmt,szSql);         
      }   
	   if ( ret ) 
	      OCI_SetBindMode( session->stmt,OCI_BIND_BY_POS ) ;      
      hb_retni( ret == 1 ? 1 : -1  );
      return;
   }

   hb_retni( -1 );

}

HB_FUNC( ORACLEBINDALLOC2 )
{
   POCI_ORASESSION session= (POCI_ORASESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   int iBind ;

   if ( session )
   {
      iBind = hb_parni( 2 ) ;
      session->pLink = ( ORA_BIND_COLS2 * ) hb_xgrabz(  sizeof( ORA_BIND_COLS2 ) * iBind );
//      memset(session->pLink,0,sizeof( ORA_BIND_COLS ) * iBind );      
      session->ubBindNum = iBind;
   }
   hb_retni( 1 );
}
void OracleFreeLink2( int num_recs, POCI_ORASESSION p )
{
   int  i;

   if ( p->pLink )
   {

      for ( i =0 ; i < num_recs; i++ )
      {
         if ( p->pLink[ i ].col_name )
         {
            hb_xfree( p->pLink[ i ].col_name );
         }
         if ( p->pLink[ i ].bindname )
         {
            hb_xfree( p->pLink[ i ].bindname );
         }
         if ( p->pLink[i].date ) 
         {
	         OCI_DateFree(p->pLink[i].date);
         }    
      }

      hb_xfree( p->pLink );
      p->pLink = NULL;
      p->ubBindNum = 0;
   }

}




HB_FUNC(ORACLEFREEBIND2)
{
   POCI_ORASESSION Stmt= (POCI_ORASESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   if ( Stmt->pLink )
   {
      OracleFreeLink2( Stmt->ubBindNum, Stmt );
      //OCI_StatementFree(Stmt->stmt);
   }
}



void SQLO2_FieldGet( PHB_ITEM pField, PHB_ITEM pItem, int iField, BOOL bQueryOnly, ULONG ulSystemID, BOOL bTranslate,OCI_Resultset * rs )
{
   LONG lType;
   HB_SIZE lLen, lDec;
   PHB_ITEM pTemp;
   unsigned int uiLen;

   HB_SYMBOL_UNUSED( bQueryOnly );
   HB_SYMBOL_UNUSED( ulSystemID );

   lType = ( LONG ) hb_arrayGetNL( pField, 6 );
   lLen  =  hb_arrayGetNS( pField, 3 );
   lDec  =  hb_arrayGetNS( pField, 4 );

   //if( lLenBuff <= 0 )     // database content is NULL
   if (OCI_IsNull( rs, iField ) )
   {
      switch( lType )
      {
         case SQL_CHAR:
         {
            char * szResult = ( char * ) hb_xgrab( lLen + 1 );
            hb_xmemset( szResult, ' ', lLen );
            szResult[ lLen ] = '\0';
            hb_itemPutCLPtr( pItem, szResult,  lLen );
            break;
         }
         case SQL_NUMERIC:
         case SQL_FAKE_NUM:
         {
//             char szResult[2] = { ' ', '\0' };
//             sr_escapeNumber( szResult, (ULONG) lLen, (ULONG) lDec, pItem );
//             hb_itemPutNL(pItem,0);
            if (lDec > 0 ) 
            {
               
               hb_itemPutNDLen( pItem,0,lLen,lDec ) ;               
            }
            else
            {
	            
	            hb_itemPutNIntLen(pItem,0,lLen); 
	         }
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
            hb_itemPutCL( pItem, "", 0 );
            break;
         }
         case SQL_BIT:
         {
            hb_itemPutL( pItem, FALSE );
            break;
         }

//#ifdef SQLRDD_TOPCONN
//         case SQL_FAKE_DATE:
//         {
//            hb_itemPutDS( pItem, bBuffer );
//            break;
//         }
//#endif
         case SQL_DATETIME:
         {
            hb_itemPutTDT( pItem, 0, 0 );
            break;
         }

         default:
            TraceLog( LOGFILE, "Invalid data type detected: %i\n", lType );
      }
   }
   else
   {
      switch( lType )
      {
         case SQL_CHAR:
         {
            
            char * szResult = ( char * ) hb_xgrab( lLen + 1 );      
            memset(szResult,' ', lLen) ;
            uiLen = OCI_GetDataLength(rs,iField);

            hb_xmemcpy( szResult,(char*)OCI_GetString(rs,iField ),uiLen ) ;
            szResult[ lLen ] =  '\0';
            hb_itemPutCPtr( pItem, szResult,  lLen );
            break;
         }
         case SQL_NUMERIC:
         {
            if (lDec > 0 ) 
            {
               lLen -= (lDec + 1);
               hb_itemPutNDLen( pItem,    OCI_GetDouble(rs, iField ) ,lLen,lDec ) ;               
            }
            else
            {
	            hb_itemPutNIntLen(pItem,OCI_GetBigInt(rs,iField),lLen); 
	         }
            break;
         }
         case SQL_DATE:
         {
            OCI_Date *date = OCI_GetDate(rs,iField);
            int  year, month, day;
            
            OCI_DateGetDate(date,&year,&month,&day);
            hb_itemPutD(pItem,year,month,day);
            OCI_DateFree(date);
            break;
         }
         case SQL_LONGVARCHAR:
         { 
            char *bBuffer = (char*)OCI_GetString(rs,iField ) ;
            HB_SIZE lLenBuff = strlen( bBuffer ) ;
            if( lLenBuff > 0 && (strncmp( bBuffer, "[", 1 ) == 0 || strncmp( bBuffer, "[]", 2 ) )   && (sr_lSerializeArrayAsJson()) )
            {
               if (s_pSym_SR_FROMJSON == NULL )
               {
                  hb_dynsymLock();
                  s_pSym_SR_FROMJSON = hb_dynsymFindName( "HB_JSONDECODE" );
                  hb_dynsymUnlock();
                  if ( s_pSym_SR_FROMJSON  == NULL ) printf( "Could not find Symbol HB_JSONDECODE\n" );           
               }
               hb_vmPushDynSym( s_pSym_SR_FROMJSON );
               hb_vmPushNil();
               hb_vmPushString( bBuffer, lLenBuff );
               pTemp = hb_itemNew( NULL );
               hb_vmPush(pTemp);
               hb_vmDo( 2 );
               /* TOFIX: */
               hb_itemForwardValue( pItem, pTemp );
               hb_itemRelease( pTemp );

            }

            else if( lLenBuff > 10 && strncmp( bBuffer, SQL_SERIALIZED_SIGNATURE, 10 ) == 0  && (!sr_lSerializedAsString()) )
            {
               if( s_pSym_SR_DESERIALIZE == NULL )
               {
                  hb_dynsymLock();
                  s_pSym_SR_DESERIALIZE = hb_dynsymFindName( "SR_DESERIALIZE" );
                  hb_dynsymUnlock();
                  if ( s_pSym_SR_DESERIALIZE  == NULL ) printf( "Could not find Symbol SR_DESERIALIZE\n" );
               }
               hb_vmPushDynSym( s_pSym_SR_DESERIALIZE );
               hb_vmPushNil();
               hb_vmPushString( bBuffer, lLenBuff );
               hb_vmDo( 1 );

               pTemp = hb_itemNew( NULL );
               hb_itemForwardValue( pTemp, hb_stackReturnItem() );

               if( HB_IS_HASH( pTemp ) && sr_isMultilang() && bTranslate )
               {
                  PHB_ITEM pLangItem = hb_itemNew( NULL );
                  HB_SIZE ulPos;
                  if( hb_hashScan( pTemp, sr_getBaseLang( pLangItem ), &ulPos ) ||
                       hb_hashScan( pTemp, sr_getSecondLang( pLangItem ), &ulPos ) ||
                       hb_hashScan( pTemp, sr_getRootLang( pLangItem ), &ulPos ) )
                  {
                     hb_itemCopy( pItem, hb_hashGetValueAt( pTemp, ulPos ) );
                  }
                  hb_itemRelease( pLangItem );
               }
               else
               {
                  hb_itemForwardValue( pItem, pTemp );
               }
               hb_itemRelease( pTemp );
            }

            else
            {
               hb_itemPutCL( pItem, bBuffer,  lLenBuff );
            }
            break;
         }
         case SQL_BIT:
         {
            //hb_itemPutL( pItem, bBuffer[0] == '1' ? TRUE : FALSE );
            hb_itemPutL( pItem, OCI_GetBigInt(rs,iField) ==1 ? TRUE : FALSE );
            break;
         }

//#ifdef SQLRDD_TOPCONN
//         case SQL_FAKE_DATE:
//         {
//            hb_itemPutDS( pItem, bBuffer ); 
//            break;
//         }
//#endif
         case SQL_DATETIME:
         {
	        OCI_Timestamp * pTime =     OCI_GetTimestamp( rs, iField );


            //hb_retdts(bBuffer);
            int iYear,iMonth, iDay, iHour, iMin,  dSec,fsec;
//           DebugBreak();            

            OCI_TimestampGetDateTime(pTime,&iYear,&iMonth, &iDay, &iHour, &iMin,  &dSec,&fsec);
            hb_itemPutDT( pItem,iYear,iMonth, iDay, iHour, iMin,  (double)dSec,0 );
            OCI_TimestampFree( pTime );
            break;
         }

         default:
            TraceLog( LOGFILE, "Invalid data type detected: %i\n", lType );
      }
   }
}

//-----------------------------------------------------------------------------//
//
//HB_FUNC( SQLO2_LINE )
//{
//   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
//   const char ** line;
//   CONST unsigned int * lens;
//   PHB_ITEM ret, temp;
//   USHORT i;
//   SQLO2_stmt_handle_t stmtParamRes;
//
//   ret  = hb_itemNew( NULL );
//
//   if( session )
//   {
//      stmtParamRes = session->stmtParamRes != -1 ? session->stmtParamRes : session->stmt;
//      line = SQLO2_values(stmtParamRes, NULL, 0);
//      lens = SQLO2_value_lens(stmtParamRes, NULL);
//      hb_arrayNew( ret, session->numcols );
//
//      for( i=0; i < session->numcols; i++ )
//      {
//         temp = hb_itemNew( NULL );
//         hb_arraySetForward( ret, i+1 , hb_itemPutCL( temp, (char * ) line[i], lens[i] ) );
//         hb_itemRelease( temp );
//      }
//   }
//   hb_itemReturnForward(ret);
//   hb_itemRelease( ret );
//}
//

HB_FUNC( SQLO2_LINEPROCESSED )
{
   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   LONG lIndex;
   PHB_ITEM temp;
   HB_SIZE i, cols;
   PHB_ITEM pFields = hb_param( 3, HB_IT_ARRAY );
   BOOL  bQueryOnly = hb_parl( 4 );
   ULONG ulSystemID = hb_parnl( 5 );
   BOOL  bTranslate = hb_parl( 6 );
   PHB_ITEM pRet    = hb_param( 7, HB_IT_ARRAY );
//   SQLO2_stmt_handle_t stmtParamRes;

   if( session )
   {
      //stmtParamRes = session->stmtParamRes != -1 ? session->stmtParamRes : session->stmt;
      //line = SQLO2_values(stmtParamRes, NULL, 0);
      //lens = SQLO2_value_lens(stmtParamRes, NULL);

      cols = hb_arrayLen( pFields );

      for( i=0; i < cols; i++ )
      {
         lIndex  = hb_arrayGetNL( hb_arrayGetItemPtr( pFields, i+1 ), FIELD_ENUM );
         temp = hb_itemNew( NULL );

         if( lIndex != 0 )
         {
            SQLO2_FieldGet( hb_arrayGetItemPtr( pFields, i+1 ), temp, lIndex , bQueryOnly, ulSystemID, bTranslate , session->rs);
         }
         hb_arraySetForward( pRet, i+1 , temp );
         hb_itemRelease( temp );
      }
   }
}

int SQLO2_sqldtype( int type )                                                                            
{                                                                                                        
   int isqltype;                                                                                         
                                                                                                         
   switch( type )                                                                                        
   {                                                                                                     
      case OCI_CDT_TEXT:                                                                                 
      //case SQLOT_STR:                                                                                  
      //case SQLOT_VCS:                                                                                  
      //case SQLOT_NON:                                                                                  
      //case SQLOT_VBI:                                                                                  
      //case SQLOT_BIN:                                                                                  
      //case SQLOT_LBI:                                                                                  
      //case SQLOT_SLS:                                                                                  
      //case SQLOT_LVC:                                                                                  
      //case SQLOT_LVB:                                                                                  
      //case SQLOT_AFC:                                                                                  
      //case SQLOT_AVC:                                                                                  
      //case SQLOT_CUR:                                                                                  
      //case SQLOT_RDD:                                                                                  
      //case SQLOT_LAB:                                                                                  
      //case SQLOT_OSL:                                                                                  
      //case SQLOT_NTY:                                                                                  
      //case SQLOT_REF:                                                                                  
      //case SQLOT_TIME:                                                                                 
      //case SQLOT_TIME_TZ:                                                                              
      //case SQLOT_VST:                                                                                  
         isqltype = SQL_CHAR;                                                                            
         break;                                                                                          
      case OCI_CDT_LOB:                                                                                  
         isqltype = SQL_LONGVARCHAR;                                                                     
         break;                                                                                          
      case OCI_CDT_NUMERIC:                                                                              
         isqltype = SQL_NUMERIC;                                                                         
         break;                                                                                          
      case OCI_CDT_DATETIME:                                                                             
         isqltype = SQL_DATE;                                                                            
         break;                                                                                          
      case OCI_CDT_TIMESTAMP:                                                                            
         isqltype= SQL_DATETIME;                                                                         
         break;                                                                                          
      default:                                                                                           
         isqltype = 0;                                                                                   
   }                                                                                                     
   return isqltype;                                                                                      
}                                                                                                        
                                                                                                         
                                                                                                         
                                                                                                         
HB_FUNC( SQLO2_DESCRIBECOL ) // ( hStmt, nCol, @cName, @nDataType, @nColSize, @nDec, @nNull )             
{                                                                                                        
   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );               
    
   int prec, scale, nullok, type;                                                       
   unsigned int dbsize,dType,ncol;
   char * name;                                                                                          
//     SQLO2_stmt_handle_t stmtParamRes;                                                                     
                                                                                                         
   if( session )                                                                                         
   {                                                                                                     
      OCI_Column *col ;                                                                                  
      ncol = hb_parni(2);                                                                                
      //stmtParamRes = session->stmtParamRes != -1 ? session->stmtParamRes : session->stmt;              
      //SQLO2_describecol( stmtParamRes, ncol, &dType, &name, &namelen, &prec, &scale, &dbsize, &nullok );
                                                                                                         
      col = OCI_GetColumn( session->rs, ncol );
      nullok = OCI_GetColumnNullable(col);                                                               
      name = (char*)OCI_GetColumnName(col ) ;                                                                   
      prec = OCI_GetColumnPrecision(col);                                                                
      scale = OCI_GetColumnScale(col);                                                                   
      dbsize = OCI_GetColumnSize(col) ;                                                                  
      dType = OCI_ColumnGetType( col ) ;                                                                 
      type =SQLO2_sqldtype( dType );                                                                      

//       type = SQLO2_sqldtype( dType );                                                                     
      hb_storni( type, 4 );                                                                              
                                                                                                         
      if( type == SQL_CHAR )                                                                             
      {                                                                                                  
         hb_storni( 0, 6 );                                                                              
         hb_storni( dbsize, 5 );                                                                         
      }                                                                                                  
      else if ( type == SQL_NUMERIC )                                                                    
      {                                                                                                  
         if( prec == 0 )                                                                                 
         {                                                                                               
            hb_storni( 19, 5 );                                                                          
            hb_storni( 6, 6 );                                                                           
         }                                                                                               
         else                                                                                            
         {                                                                                               
            hb_storni( prec, 5 );                                                                        
            hb_storni( scale, 6 );                                                                       
         }                                                                                               
      }                                                                                                  
      else if ( type == SQL_DATETIME )                                                                   
      {                                                                                                  
         hb_storni( 0, 6 );                                                                              
         hb_storni( 8, 5 );                                                                              
      }                                                                                                  
      else                                                                                               
      {                                                                                                  
         hb_storni( prec, 5 );                                                                           
         hb_storni( scale, 6 );                                                                          
      }                                                                                                  
                                                                                                         
      hb_storl( nullok, 7 );                                                                             
      hb_storc( name, 3 );                                                                               
      hb_retni( SQL_SUCCESS );                                                                           
   }                                                                                                     
   else                                                                                                  
   {                                                                                                     
      hb_retni( SQL_ERROR );                                                                             
   }                                                                                                     
}                                                                                                        

HB_FUNC( SQLO2_FETCH )
{
   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   

   if( session )
   {

      session->iStatus = OCI_FetchNext(session->rs) ?1 : -1 ;

      if( session->iStatus == 0 || session->iStatus == 1 )
      {
         hb_retni( SQL_SUCCESS );
      }
      else if( session->iStatus < 0 )
      {
         hb_retni( SQL_NO_DATA_FOUND );
      }
      else
      {
         hb_retni( SQL_NO_DATA_FOUND );
      }
   }
   else
   {
      hb_retni( SQL_ERROR );
   }
}



HB_FUNC( SQLO2_CLOSESTMT )
{
   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session )
   {

      if ( session->stmtParamRes )    
      {
         OCI_StatementFree(session->stmtParamRes ) ;
         session->stmtParamRes = NULL;

      }	 

      if (session->stmt  ) 
      {

         session->iStatus = OCI_StatementFree(session->stmt ) ? 1 : -1;
      }     
      else
         session->iStatus = 1;
      session->stmt = NULL;
      hb_retni( session->iStatus );
   }
   hb_retni( SQL_SUCCESS );
}




HB_FUNC( ORACLEWRITEMEMO2 )
{
   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   const char * sTable = hb_parc( 2 );
   ULONG ulRecno = hb_parnl( 3 ) ;
   const char * sRecnoName = hb_parcx( 4 );
   //SQLO2_lob_desc_t loblp;
   //SQLO2_stmt_handle_t sth;
   OCI_Lob *lob1;
   OCI_Statement *stmt ;
   int status;

   PHB_ITEM pArray = hb_param( 5, HB_IT_ARRAY );

   HB_SIZE uiLen, uiSize;

   uiLen = hb_arrayLen( pArray );

   if (( !session ) || uiLen == 0 )
   {
      hb_retni( 0 );
      return;
   }
   else
   {
      for( uiSize = 0; uiSize < uiLen; uiSize++ )
      {
         PHB_ITEM pFieldDesc = hb_arrayGetItemPtr( pArray, uiSize + 1 );
         char szSql[256] = {0};
         char * sMemo  = hb_arrayGetCPtr( pFieldDesc, 2 );
         const char * sField = hb_arrayGetCPtr( pFieldDesc, 1 );
         OCI_Resultset * rs;
         sprintf( szSql, "UPDATE %s SET %s = EMPTY_CLOB() WHERE %s = %lu RETURNING %s INTO :b1", sTable, sField, sRecnoName, ulRecno, sField );
         
         //sth = SQLO2_prepare( session->dbh, szSql );
         //SQLO2_alloc_lob_desc( session->dbh, &loblp );
         //SQLO2_bind_by_pos(sth, 1, SQLOT_CLOB, &loblp, 0, NULL, 0);
         stmt = OCI_StatementCreate(session->cn);
         OCI_Prepare(stmt,szSql );
         OCI_RegisterLob(stmt, ":b1", OCI_CLOB);
         //status = SQLO2_execute(sth, 1);
         
         
         if (!OCI_Execute(stmt))
         {
            //SQLO2_free_lob_desc(session->dbh, &loblp);
	         //SQLO2_close(sth);
	         hb_retni( -1 );
            return;
         }
         rs = OCI_GetResultset(stmt);
         OCI_FetchNext(rs);
         lob1  = OCI_GetLob2(rs, ":b1");
         
         //status = SQLO2_lob_write_buffer(session->dbh, loblp, strlen(sMemo), sMemo, strlen(sMemo), SQLO2_ONE_PIECE );
        status= OCI_LobWrite( lob1, (void*)sMemo, strlen( sMemo ) );

         if (status < 0)
         {
            //SQLO2_free_lob_desc(session->dbh, &loblp);
	         //SQLO2_close(sth);
            OCI_LobFree(lob1);
            OCI_StatementFree(stmt ) ;	         
	         hb_retni( -2 );
            return;
         }

         //SQLO2_free_lob_desc( session->dbh, &loblp );
         OCI_LobFree(lob1);
         OCI_StatementFree( stmt ) ;
         //SQLO2_close(sth);
      }
      hb_retni(0) ;
   }
}



HB_FUNC( ORACLE_PROCCURSOR2 )
{ 
  POCI_ORASESSION session= (POCI_ORASESSION) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
//  SQLO2_stmt_handle_t sth = SQLO2_STH_INIT;
//  SQLO2_stmt_handle_t st2h;                     /* handle of the ref cursor */

  int ret = SQL_ERROR ;
   
  const char * stmt = hb_parc( 2 );
  const char * parc = hb_parc( 3 );
  
  if ( session )
   { 
   /* parse the statement */
   //ret = SQLO2_prepare(session->dbh, stmt);
   session->stmt = OCI_StatementCreate(session->cn);
   session->stmtParamRes = OCI_StatementCreate(session->cn);
	ret=  OCI_Prepare(session->stmt,stmt) ? 0 : -1; 
     
   if (ret >= SQLO_SUCCESS )
      {       
       //if ( 0 <= ( sth = ret )) 
       //{
        /* bind all variables */
        if (!OCI_BindStatement(session->stmt,parc,session->stmtParamRes ))
        {            
           hb_retni( SQL_ERROR );
           return ;
        }
        //}    
      
        if (!OCI_Execute(session->stmt))
        {   
           hb_retni( SQL_ERROR );          
           return;
        }

        session->rs = OCI_GetResultset(session->stmtParamRes);
        session->numcols = OCI_GetColumnCount(session->rs) ;           
      
      
     } 
     else 
     {
        hb_retni( SQL_ERROR );
        return ; 
     }
  } 
   
  hb_retni( ret );
  //SQLO2_close( session->stmt );   
  //SQLO2_close( session->stmtParamRes ); 
  //
}       




HB_FUNC( SQLO2_ORACLESETLOBPREFETCH )
{
   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session )
   {
	  hb_retl( OCI_SetDefaultLobPrefetchSize(session->cn , (unsigned int) hb_parni(2)));
      
   }
   else
   {
      hb_retl(0);
   }
}

HB_FUNC( SQLO2_SETSTATEMENTCACHESIZE)
{
   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session )
   {
	  hb_retl(  OCI_SetStatementCacheSize( session->cn , (unsigned int) hb_parni(2)));
      
   }
   else
   {
      hb_retl(0);
   }
}


HB_FUNC( SQLO2_GETSTATEMENTCACHESIZE)
{
   POCI_ORASESSION session  = ( POCI_ORASESSION ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if( session )
   {
	  hb_retni( (unsigned int) OCI_GetStatementCacheSize( session->cn ));
      
   }
   else
   {
      hb_retni(0);
   }
}

HB_FUNC( GETORAHANDLE2)
{
   OCI_ORASESSION* p  = ( OCI_ORASESSION* ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
   if ( p )
      hb_retptr(p->stmt);
}

HB_FUNC( SETORAHANDLE2)
{
   OCI_ORASESSION* p  = ( OCI_ORASESSION* ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

   if ( p )
   {
      p->stmt = (OCI_Statement*)hb_parptr(2);
   }
}


OCI_Connection * GetConnection( OCI_ORASESSION *  p )
{
	return p->cn;
}	

