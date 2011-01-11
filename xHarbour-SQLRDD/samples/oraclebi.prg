/*
* SQLRDD Test
* Copyright (c) 2008 - Luiz Rafael Culik Guimaraes  <Luiz@xharbour.com.br>
* All Rights Reserved
*/

#include "sqlrdd.ch"
#include "oracle.ch"
REQUEST SQLRDD, SR_ORACLE
Func main

   Local nc
   Local osql
   Local e
   Local nerror
   Local pretorno
   Local cSql

   Rddsetdefault( "SQLRDD" )
   nc := SR_ADDCONNECTION( CONNECT_ORACLE, "OCI=127.0.0.1;UID=SYSTEM;PWD=kl6qaxv9;TNS=XE" )

   If nc < 0
      Quit
   Endif
   cSql := 'begin test(2,2,:aa); end;  '
   oSql := sr_getconnection()
   oracleprePARE( osql:hdbc, cSql )
   oraclebindalloc( oSql:hdbc, 1 )
   OracleinBindParam( oSql:hdbc, 1, 2, 12, 0 )

   TRY
      nError := OracleExecDir( osql:hDbc )
   CATCH e
      nerror := - 1
   End
   If nerror >= 0
      pRetorno := ORACLEGETBINDDATA( osql:hdbc, 1 )
      ? "pretorno = ", pretorno
   Endif
   ORACLEFREEBIND( osql:hdbc )

   cSql := 'begin teste(2,2,6,:aa); end;  '
   oSql := sr_getconnection()
   oracleprePARE( osql:hdbc, cSql )
   oraclebindalloc( oSql:hdbc, 1 )
   OracleinBindParam( oSql:hdbc, 1, - 1, 12, 0 )

   TRY
      nError := OracleExecDir( osql:hDbc )
   CATCH e
      nerror := - 1
   End
   If nerror >= 0
      pRetorno := Alltrim( ORACLEGETBINDDATA( osql:hdbc, 1 ) )
      ? "pretorno = ", pretorno
   Endif
   ORACLEFREEBIND( osql:hdbc )

   cSql := 'begin testee(2,2,6,:aa,:bb); end;  '
   oSql := sr_getconnection()
   oracleprePARE( osql:hdbc, cSql )
   oraclebindalloc( oSql:hdbc, 2 )
   OracleinBindParam( oSql:hdbc, 1, - 1, 12, 0 )
   OracleinBindParam( oSql:hdbc, 2, - 1, 12, 0 )

   TRY
      nError := OracleExecDir( osql:hDbc )
   CATCH e
      nerror := - 1
   End
   If nerror >= 0
      pRetorno := Alltrim( ORACLEGETBINDDATA( osql:hdbc, 1 ) )
      ? "pretorno = ", pretorno
      pRetorno := Alltrim( ORACLEGETBINDDATA( osql:hdbc, 2 ) )
      ? "pretorno = ", pretorno

   Endif
   ORACLEFREEBIND( osql:hdbc )

   cSql := 'begin testedouble(2,5,:aa); end;  '
   oSql := sr_getconnection()
   oracleprePARE( osql:hdbc, cSql )
   oraclebindalloc( oSql:hdbc, 1 )
   OracleinBindParam( oSql:hdbc, 1, 5, 12, 0 )

   TRY
      nError := OracleExecDir( osql:hDbc )
   CATCH e
      nerror := - 1
   End
   If nerror >= 0
      pRetorno := ORACLEGETBINDDATA( osql:hdbc, 1 )
      ? "pretorno = ", pretorno
   Endif
   ORACLEFREEBIND( osql:hdbc )

   cSql := 'begin testeintout(2,5,:aa); end;  '
   oSql := sr_getconnection()
   oracleprePARE( osql:hdbc, cSql )
   oraclebindalloc( oSql:hdbc, 1 )
   OracleinBindParam( oSql:hdbc, 1, 2, 12, 0, 8 )

   TRY
      nError := OracleExecDir( osql:hDbc )
   CATCH e
      nerror := - 1
   End
   If nerror >= 0
      pRetorno := ORACLEGETBINDDATA( osql:hdbc, 1 )
      ? "pretorno = ", pretorno
   Endif
   ORACLEFREEBIND( osql:hdbc )

Return nil
