#ifndef _SQL_ORASTRU_H
#define _SQL_ORASTRU_H

#include "compat.h"

#include "sqlrddsetup.ch"
#include "sqlprototypes.h"
#include "sqlodbc.ch"
#include "ocilib.h"


#if !defined(__GNUC__) && defined(WIN32)
#define inline __inline

#endif

#ifdef SQLORA2
typedef struct _ORA_BIND_COLS2
{
   char * col_name;	
   char *bindname;
   int iType;
   short sVal;
   double  dValue;
   unsigned int  ulValue;
   char sDate[ 7 ];  
   LONGLONG iValue;
   LONGLONG lValue; 
   OCI_Date *date;  
   int iFieldSize;
} ORA_BIND_COLS2 ;

#else
typedef struct _ORA_BIND_COLS
{
   char * col_name;	
   char *bindname;
   int iType;
   short sVal;
   double  dValue;
   unsigned int  ulValue;
   char sDate[ 7 ];  
   LONGLONG iValue;
   LONGLONG lValue; 
   OCI_Date *date;  
   int iFieldSize;
} ORA_BIND_COLS ;
#endif

typedef struct _OCI_ORASESSION
{
	OCI_Connection *cn;    
    OCI_Statement *stmt ;
    OCI_Statement *stmtParamRes;
    OCI_Resultset *rs;
    int iStatus;                   // Execution return value
    int numcols;                  // Result set columns
    char server_version[1024];
#ifdef SQLORA2
   ORA_BIND_COLS2 *  pLink;
#else
   ORA_BIND_COLS *  pLink;
#endif
   unsigned int   ubBindNum;
} OCI_ORASESSION;	
typedef OCI_ORASESSION * POCI_ORASESSION;

#endif