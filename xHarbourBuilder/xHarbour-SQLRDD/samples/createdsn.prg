/*
* SQLRDD Create Dynamic DSN
* Sample application to create an ODBC panel entry
* Copyright (c) 2006 - Marcelo Lombardo  <marcelo@xharbour.com.br>
* All Rights Reserved
*/

#include "sqlrdd.ch"
#include "dbinfo.ch"

#define SQL_DBMS_NAME                       17
#define SQL_DBMS_VER                        18

FUNCTION MAIN(cDsn)

   Local nDetected, cConnString, cAtributes, cDriver, i

   clear screen

   Alert( "This sample works only with MSSQL Server under Windows. Changing 'cDriver' and 'cAtributes' you can adapt to other databases." )

   ? [This will create a System DSN named "xHB001" in ODBC panel to access Nothwind sample database in MSSQL Server]
   
   cAtributes := "DSN=xHB001;Description=xHB Test;Server=.;Database=Northwind;UseProcForPrepare=Yes;Trusted_Connection=Yes;AnsiNPW=Yes;"
   cDriver    := "SQL Server"
   
   If SR_InstallDSN( cDriver, cAtributes )

      Alert( "If you go to ODBC setup panel you should find created DSN. Hit ok to try to connect to data source." )

      cConnString := "DSN=xHB001"
      nDetected   := DetectDBFromDSN( cConnString )
   
      If nDetected > SYSTEMID_UNKNOW
         ? "Connecting to", cConnString
         If SR_AddConnection( nDetected, cConnString ) > 0
            ? "Connected to ", SR_GetConnectionInfo(, SQL_DBMS_NAME ), SR_GetConnectionInfo(, SQL_DBMS_VER )
         Else
            ? "Connection failure"
         EndIf
      EndIf
   Else
      ? "DSN creation failure:"
      For i = 1 to 8
         ? SR_InstallError( i )
      Next
   EndIf
   
   ? ""
   ? "Press any key to quit"

   inkey(0)

Return

/*------------------------------------------------------------------------*/

#include "connect.prg"

/*------------------------------------------------------------------------*/