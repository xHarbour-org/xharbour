/*
* SQLRDD TopConnect compatibility test
* Sample application to test compatibility with TopConnect RDD
* Copyright (c) 2006 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#include "sqlrdd.ch"
#include "dbinfo.ch"

#define SQL_DBMS_NAME                       17
#define SQL_DBMS_VER                        18

FUNCTION MAIN(cDsn)

   Local oSql, i, n, aFiles, aOpt := {}, aTabs := {}, nTab

   Connect( cDSN )    // see connect.prg

   SR_SetlUseDBCatalogs( .T. )    // Diz ao SQLRDD para usar os índices do catálogo do banco de dados

   ? "Connected to :", SR_GetConnectionInfo(, SQL_DBMS_NAME ), SR_GetConnectionInfo(, SQL_DBMS_VER )
   ? "RDD Version  :", SR_Version()
   ? " "
   ? "Abrindo tabela SA1010"

   If sr_existtable( "SA1010" )     // Verifica se a tabela existe no banco de dados

      USE SA1010 via "SQLRDD"       // Abre a tabela e os índices!!

      ? "Estrutura da tabela :"
      Wait SR_ShowVector( dbStruct() )

      dbGoTop()

      ? " "
      ? "Indices             :"
      For i = 1 to 100
         If empty(OrdName(i))
            Exit
         EndIf
         ? OrdName( i ), OrdKey( i ) //, OrdKeyCount( i )
      Next

      ? ""
      ? "SEEK ", dbSeek( "  " + strZero( 65, 10 ) )

      wait "Pressione qualquer tecla para executar o browse()"
      clear

      browse()
      USE

   Else
      Wait "Tabela não existe: " + "SA1010"
//      Return
   EndIf

   clear

   ? "Listando todas as tabelas do banco de dados com suas chaves de indice"
   ? " "
   wait "Pressione qualquer tecla para iniciar"
   ? " "

   aFiles := SR_ListTables()                    // Busca todas as tabelas do banco de dados

   For n = 1 to len( aFiles )
      If left( aFiles[n], 3 ) $ "SR_;TOP;SYS;DTP"       // Não queremos tabelas de catálogo
         Loop
      EndIf
      TRY
         USE (aFiles[n]) VIA "SQLRDD"
         ? "Table " + aFiles[n] + " Lastrec() ", lastrec(), " Keys "
         For i = 1 to 100
            If empty(OrdName(i))
               Exit
            EndIf
            ?? OrdName( i ) + " "
         Next
         aadd( aOpt, aFiles[n] + " Lastrec() :" + str(lastrec()) + " Indices :" + str(i-1) )
         aadd( aTabs, aFiles[n] )
      CATCH
      END
   Next

   Wait

   nTab := 1

   While .t.

      clear

      @1,5 SAY "Escolha uma das tabelas para abrir, ESC para sair"

      nTab := achoice( 3, 10, 27, 60, aOpt,,, nTab )

      If nTab > 0
         USE (aTabs[nTab]) VIA "SQLRDD"
         browse()
      Else
         Exit
      EndIf

   EndDo

   clear

Return

/*------------------------------------------------------------------------*/

#include "connect.prg"

/*------------------------------------------------------------------------*/
