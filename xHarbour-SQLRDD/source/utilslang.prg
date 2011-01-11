/* $CATEGORY$SQLRDD/Utils$FILES$sql.lib$
* SQLRDD Language Utilities
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#include "hbclass.ch"
#include "common.ch"
#include "msg.ch"

Static nMessages   := 32

GLOBAL nBaseLang   := 1
GLOBAL nSecondLang := LANG_EN_US
GLOBAL nRootLang   := LANG_PT_BR    // Root since I do it on Brazil

Static aMsg1 := ;
{ ;
   "Attempt to write to an empty table without a previous Append Blank",;
   "Undefined SQL datatype: ",;
   "Insert not allowed in table : ",;
   "Update not allowed without a WHERE clause.",;
   "Update not allowed in table : ",;
   "Invalid record number : ",;
   "Not connected to the SQL dabase server",;
   "Error Locking line or table (record# + table + error code):",;
   "Unsupported data type in : ",;
   "Syntax Error in filter expression : ",;
   "Error selecting IDENTITY after INSERT in table : ",;
   "Delete not allowed in table : ",;
   "Delete Failure ",;
   "Last command sent to database : ",;
   "Insert Failure ",;
   "Update Failure ",;
   "Error creating index : ",;
   "Index Column not Found - ",;
   "Invalid Index number or tag in OrdListFocus() : ",;
   "Seek without active index",;
   "Unsupported data type at column ",;
   "Unsupported database : ",;
   "Could not setup stmt option",;
   "RECNO column not found (column / table): ",;
   "DELETED column not found (column / table): ",;
   "Invalid dbSeek or SetScope Key Argument",;
   "Error Opening table in SQL database",;
   "Data type mismatch in dbSeek()",;
   "Invalid columns lists in constraints creating",;
   "Error altering NOT NULL column",;
   "Invalid column in index expression",;
   "Table or View does not exist";
}

Static aMsg2 := ;
{ ;
   "Tentativa de gravar um registro em tabela vazia sem antes adicionar uma linha",;
   "Tipo de dado SQL indefinido : ",;
   "Inclusão não permitida na tabela : ",;
   "Update não permitido SEM cláusula WHERE.",;
   "Alteração não permitida na tabela : ",;
   "Número de Registro inexistente :",;
   "Não conectado ao servidor de banco de dados",;
   "Erro travando registro ou tabela (num.registro + tabela + cod.erro):",;
   "Tipo de dado não suportado em : ",;
   "Erro de sitaxe em expressão de filtro : ",;
   "Erro ao selecionar IDENTITY após INSERT na tabela : ",;
   "Exclusão não permitida na tabela : ",;
   "Erro na exclusão ",;
   "Último comando enviado ao banco de dados : ",;
   "Erro na inclusão ",;
   "Erro na alteração ",;
   "Erro criando índice : ",;
   "Coluna de índice não existente - ",;
   "Indice ou tag inválido em OrdListFocus() : ",;
   "Seek sem índice ativo.",;
   "Tipo de dado inválido na coluna ",;
   "Banco de dados não suportado : ",;
   "Erro configurando stmt",;
   "Coluna de RECNO não encontrada (coluna / tabela): ",;
   "Coluna de DELETED não encontrada (coluna / tabela): ",;
   "Argumento chave inválido em dbSeek ou SetScope",;
   "Erro abrindo tabela no banco SQL",;
   "Tipo de dado inválido em dbSeek()",;
   "Listas de colunas inválidas em criação de constraints",;
   "Erro alterando coluna para NOT NULL",;
   "Coluna inválida na chave de índice",;
   "Tabela ou View não existente";
}

Static aMsg3 := ;
{ ;
   "Attempt to write to an empty table without a previous Append Blank",;
   "Undefined SQL datatype: ",;
   "Insert not allowed in table : ",;
   "Update not allowed without a WHERE clause.",;
   "Update not allowed in table : ",;
   "Invalid record number : ",;
   "Not connected to the SQL dabase server",;
   "Error Locking line or table (record# + table + error code):",;
   "Unsupported data type in : ",;
   "Syntax Error in filter expression : ",;
   "Error selecting IDENTITY after INSERT in table : ",;
   "Delete not allowed in table : ",;
   "Delete Failure ",;
   "Last command sent to database : ",;
   "Insert Failure ",;
   "Update Failure ",;
   "Error creating index : ",;
   "Index Column not Found - ",;
   "Invalid Index numner or tag in OrdListFocus() : ",;
   "Seek without active index",;
   "Unsupported data type at column ",;
   "Unsupported database : ",;
   "Could not setup stmt option",;
   "RECNO column not found (column / table): ",;
   "DELETED column not found (column / table): ",;
   "Invalid dbSeek or SetScope Key Argument",;
   "Error Opening table in SQL database",;
   "Data type mismatch in dbSeek()",;
   "Invalid columns lists in constraints creating",;
   "Error altering NOT NULL column",;
   "Invalid column in index expression",;
   "Table or View does not exist";
}

Static aMsg4 := ;
{ ;
   "Attempt to write to an empty table without a previous Append Blank",;
   "Undefined SQL datatype: ",;
   "Insert not allowed in table : ",;
   "Update not allowed without a WHERE clause.",;
   "Update not allowed in table : ",;
   "Invalid record number : ",;
   "Not connected to the SQL dabase server",;
   "Error Locking line or table (record# + table + error code):",;
   "Unsupported data type in : ",;
   "Syntax Error in filter expression : ",;
   "Error selecting IDENTITY after INSERT in table : ",;
   "Delete not allowed in table : ",;
   "Delete Failure ",;
   "Last command sent to database : ",;
   "Insert Failure ",;
   "Update Failure ",;
   "Error creating index : ",;
   "Index Column not Found - ",;
   "Invalid Index number or tag in OrdListFocus() : ",;
   "Seek without active index",;
   "Unsupported data type at column ",;
   "Unsupported database : ",;
   "Could not setup stmt option",;
   "RECNO column not found (column / table): ",;
   "DELETED column not found (column / table): ",;
   "Invalid dbSeek or SetScope Key Argument",;
   "Error Opening table in SQL database",;
   "Data type mismatch in dbSeek()",;
   "Invalid columns lists in constraints creating",;
   "Error altering NOT NULL column",;
   "Invalid column in index expression",;
   "Table or View does not exist";
}

Static aMsg5 := ;
{ ;
   "Attempt to write to an empty table without a previous Append Blank",;
   "Undefined SQL datatype: ",;
   "Insert not allowed in table : ",;
   "Update not allowed without a WHERE clause.",;
   "Update not allowed in table : ",;
   "Invalid record number : ",;
   "Not connected to the SQL dabase server",;
   "Error Locking line or table (record# + table + error code):",;
   "Unsupported data type in : ",;
   "Syntax Error in filter expression : ",;
   "Error selecting IDENTITY after INSERT in table : ",;
   "Delete not allowed in table : ",;
   "Delete Failure ",;
   "Last command sent to database : ",;
   "Insert Failure ",;
   "Update Failure ",;
   "Error creating index : ",;
   "Index Column not Found - ",;
   "Invalid Index number or tag in OrdListFocus() : ",;
   "Seek without active index",;
   "Unsupported data type at column ",;
   "Unsupported database : ",;
   "Could not setup stmt option",;
   "RECNO column not found (column / table): ",;
   "DELETED column not found (column / table): ",;
   "Invalid dbSeek or SetScope Key Argument",;
   "Error Opening table in SQL database",;
   "Data type mismatch in dbSeek()",;
   "Invalid columns lists in constraints creating",;
   "Error altering NOT NULL column",;
   "Invalid column in index expression",;
   "Table or View does not exist";
}

Static aMsg6 := ;
{ ;
   "Attempt to write to an empty table without a previous Append Blank",;
   "Undefined SQL datatype: ",;
   "Insert not allowed in table : ",;
   "Update not allowed without a WHERE clause.",;
   "Update not allowed in table : ",;
   "Invalid record number : ",;
   "Not connected to the SQL dabase server",;
   "Error Locking line or table (record# + table + error code):",;
   "Unsupported data type in : ",;
   "Syntax Error in filter expression : ",;
   "Error selecting IDENTITY after INSERT in table : ",;
   "Delete not allowed in table : ",;
   "Delete Failure ",;
   "Last command sent to database : ",;
   "Insert Failure ",;
   "Update Failure ",;
   "Error creating index : ",;
   "Index Column not Found - ",;
   "Invalid Index number or tag in OrdListFocus() : ",;
   "Seek without active index",;
   "Unsupported data type at column ",;
   "Unsupported database : ",;
   "Could not setup stmt option",;
   "RECNO column not found (column / table): ",;
   "DELETED column not found (column / table): ",;
   "Invalid dbSeek or SetScope Key Argument",;
   "Error Opening table in SQL database",;
   "Data type mismatch in dbSeek()",;
   "Invalid columns lists in constraints creating",;
   "Error altering NOT NULL column",;
   "Invalid column in index expression",;
   "Table or View does not exist";
}

Static aMsg7 := ;
{ ;
   "Attempt to write to an empty table without a previous Append Blank",;
   "Undefined SQL datatype: ",;
   "Insert not allowed in table : ",;
   "Update not allowed without a WHERE clause.",;
   "Update not allowed in table : ",;
   "Invalid record number : ",;
   "Not connected to the SQL dabase server",;
   "Error Locking line or table (record# + table + error code):",;
   "Unsupported data type in : ",;
   "Syntax Error in filter expression : ",;
   "Error selecting IDENTITY after INSERT in table : ",;
   "Delete not allowed in table : ",;
   "Delete Failure ",;
   "Last command sent to database : ",;
   "Insert Failure ",;
   "Update Failure ",;
   "Error creating index : ",;
   "Index Column not Found - ",;
   "Invalid Index number or tag in OrdListFocus() : ",;
   "Seek without active index",;
   "Unsupported data type at column ",;
   "Unsupported database : ",;
   "Could not setup stmt option",;
   "RECNO column not found (column / table): ",;
   "DELETED column not found (column / table): ",;
   "Invalid dbSeek or SetScope Key Argument",;
   "Error Opening table in SQL database",;
   "Data type mismatch in dbSeek()",;
   "Invalid columns lists in constraints creating",;
   "Error altering NOT NULL column",;
   "Invalid column in index expression",;
   "Table or View does not exist";
};

Static aMsg

/*------------------------------------------------------------------------*/
INIT PROCEDURE SR_Init2

  aMSg := { aMsg1, aMsg2, aMsg3, aMsg4, aMsg5, aMsg6, aMsg7 }

RETURN

/*------------------------------------------------------------------------*/

Function SR_Msg(nMsg)

   If nMsg > 0 .and. nMsg <= len( aMsg[nBaseLang] )
      Return aMsg[nBaseLang, nMsg]
   EndIf

Return ""

/*------------------------------------------------------------------------*/

Function SR_SetBaseLang(nLang)

   Local nOldLang := nBaseLang

   If nLang == NIL
      nLang := 0
   EndIf

   If nLang > 0 .and. nLang <= Len( aMsg )
      nBaseLang := nLang
   EndIf

Return nOldLang

/*------------------------------------------------------------------------*/

Function SR_SetSecondLang(nLang)

   Local nOldLang := nSecondLang

   If nLang == NIL
      nLang := 0
   EndIf

   If nLang > 0 .and. nLang <= Len( aMsg )
      nSecondLang := nLang
   EndIf

Return nOldLang

/*------------------------------------------------------------------------*/

Function SR_SetRootLang(nLang)

   Local nOldLang := nRootLang

   If nLang == NIL
      nLang := 0
   EndIf

   If nLang > 0 .and. nLang <= Len( aMsg )
      nRootLang := nLang
   EndIf

Return nOldLang

/*------------------------------------------------------------------------*/

Function SR_GetErrMessageMax()

Return nMessages

/*------------------------------------------------------------------------*/

#pragma BEGINDUMP

PHB_ITEM HB_EXPORT sr_getCurrentLang( void )
{
   return &NBASELANG;
}

PHB_ITEM HB_EXPORT sr_getSecondLang( void )
{
   return &NSECONDLANG;
}

PHB_ITEM HB_EXPORT sr_getRootLang( void )
{
   return &NROOTLANG;
}

#pragma ENDDUMP

/*------------------------------------------------------------------------*/
