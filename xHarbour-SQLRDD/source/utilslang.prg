/* $CATEGORY$SQLRDD/Utils$FILES$sql.lib$
* SQLRDD Language Utilities
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#include "compat.ch"
#include "hbclass.ch"
#include "common.ch"
#include "msg.ch"

Static s_nMessages   := 32

Static s_aMsg1 := ;
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

Static s_aMsg2 := ;
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

Static s_aMsg3 := ;
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

Static s_aMsg4 := ;
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

Static s_aMsg5 := ;
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

Static s_aMsg6 := ;
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

Static s_aMsg7 := ;
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

Static s_aMsg

/*------------------------------------------------------------------------*/
INIT PROCEDURE SR_Init2

  s_aMsg := { s_aMsg1, s_aMsg2, s_aMsg3, s_aMsg4, s_aMsg5, s_aMsg6, s_aMsg7 }

  SR_SetMsgCount( len( s_aMsg ) )

  SR_SetBaseLang( 1 )
  SR_SetSecondLang( LANG_EN_US )
  SR_SetRootLang( LANG_PT_BR )      // Root since I do it on Brazil

RETURN

/*------------------------------------------------------------------------*/

Function SR_Msg( nMsg )
   Local nBaseLang := SR_SetBaseLang()

   If nMsg > 0 .and. nMsg <= len( s_aMsg[ nBaseLang ] )
      Return s_aMsg[ nBaseLang, nMsg ]
   EndIf

Return ""

/*------------------------------------------------------------------------*/

Function SR_GetErrMessageMax()

Return s_nMessages

/*------------------------------------------------------------------------*/

#pragma BEGINDUMP

#include "compat.h"

static int s_iMsgCount = 0;

static int s_iBaseLang   = 0;
static int s_iSecondLang = 0;
static int s_iRootLang   = 0;

PHB_ITEM HB_EXPORT sr_getBaseLang( PHB_ITEM pLangItm )
{
   return hb_itemPutNI( pLangItm, s_iBaseLang );
}

PHB_ITEM HB_EXPORT sr_getSecondLang( PHB_ITEM pLangItm )
{
   return hb_itemPutNI( pLangItm, s_iSecondLang );
}

PHB_ITEM HB_EXPORT sr_getRootLang( PHB_ITEM pLangItm )
{
   return hb_itemPutNI( pLangItm, s_iRootLang );
}

HB_FUNC_STATIC( SR_SETMSGCOUNT )
{
   hb_retni( s_iMsgCount );
   if( HB_ISNUM( 1 ) )
      s_iMsgCount = hb_parni( 1 );
}

HB_FUNC( SR_SETBASELANG )
{
   int iLang = hb_parni( 1 );

   hb_retni( s_iBaseLang );

   if( iLang > 0 && iLang <= s_iMsgCount )
      s_iBaseLang = iLang;
}

HB_FUNC( SR_SETSECONDLANG )
{
   int iLang = hb_parni( 1 );

   hb_retni( s_iSecondLang );

   if( iLang > 0 && iLang <= s_iMsgCount )
      s_iSecondLang = iLang;
}

HB_FUNC( SR_SETROOTLANG )
{
   int iLang = hb_parni( 1 );

   hb_retni( s_iRootLang );

   if( iLang > 0 && iLang <= s_iMsgCount )
      s_iRootLang = iLang;
}

#pragma ENDDUMP

/*------------------------------------------------------------------------*/
