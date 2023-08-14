#include "common.h"

static int Hash( char *Name )
{ 
   unsigned int uiHash = 5381;

   while( *Name )
   {
      uiHash = ( ( uiHash << 5 ) + uiHash ) + *Name; // uiHash *= 33 + Char
      Name++;
   }

   return uiHash % HASHSIZE;
}

SYMBOLTABLE *Symbol_InitTable()
{ 
   SYMBOLTABLE *pSymbolTable = NEW( SYMBOLTABLE );
   int i;

   for( i = 0; i < HASHSIZE; i++ )
   {
      pSymbolTable->Buckets[i] = NULL;
   }

   return pSymbolTable;
}

SYMBOL * Symbol_Put( SYMBOLTABLE *pSymbolTable, char *Name, SYMBOL_KIND Kind, PARSER_CONTEXT *Parser_pContext )
{ 
   int iHash;
   SYMBOL *pSymbol = pSymbolTable->Buckets[ ( iHash = Hash( Name ) ) ];

   while( pSymbol )
   {
      if( strcmp( pSymbol->pID->Name, Name ) == 0 )
      {
         pSymbol->Kind = (SYMBOL_KIND) ( pSymbol->Kind | Kind );
         return pSymbol;
      }
      
      pSymbol = pSymbol->pNext;
   }

   pSymbol = NEW( SYMBOL );

   pSymbol->pID   = New_ID( Name, Parser_pContext );
   pSymbol->Kind  = Kind;
   pSymbol->pNext = pSymbolTable->Buckets[ iHash ];

   pSymbolTable->Buckets[ iHash ] = pSymbol;

   return pSymbol;
}
  
SYMBOL *Symbol_Get( SYMBOLTABLE *pSymbolTable, char *Name, PARSER_CONTEXT *Parser_pContext )
{
   int iHash;
   SYMBOL *pSymbol = pSymbolTable->Buckets[ ( iHash = Hash( Name ) ) ];

   while( pSymbol )
   {
      if( strcmp( pSymbol->pID->Name, Name ) == 0 )
      {
         return pSymbol;
      }
      
      pSymbol = pSymbol->pNext;
   }

   return NULL;
}
