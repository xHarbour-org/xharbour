/*
 * $Id$
 */

/* Test program for reading GUID columns in SQL SERVER
*
* Eduardo Motta
*/

REQUEST HB_LANG_PT
REQUEST HB_CODEPAGE_PTISO
REQUEST SQLRDD
REQUEST SR_ODBC
REQUEST DBFCDX

#include "sqlrdd.ch"

Function test_sqlrdd_guid_columns()
Local cUserDB := "user"
Local cPassDB := "password"
Local cDriver := "driver name" // example: ODBC Driver 13 for SQL Server
Local cServer := "ip or nameserver" // example: 192.168.0.20,1433\DEVELOPMENT
Local cDataBaseName := "database name"
Local cDSN := "Driver=" + cDriver + ";UseProcForPrepare=Yes;Trusted_Connection=No;AnsiNPW=Yes;server=" + cServer + ";database=" + cDataBaseName + ";"
Local nH_Sql
Local cSelect := "SELECT top 10 CONNECTION_ID from sys.dm_exec_connections"
Local aSql := {}

nH_Sql = SR_AddConnection( CONNECT_ODBC, "uid="+cUserDB+";pwd="+cPassDB+";"+cDSN )

? SR_Version()
? nH_Sql

Sr_GetConnection():Exec( cSelect,,.t.,@aSql )

? Sr_ShowVector(aSql)


SR_EndTransaction()
SR_EndConnection(nH_Sql)
Sr_End()

Return
