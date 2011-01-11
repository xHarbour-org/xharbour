#include "common.h"

int Hash( char *Name )
{ 
   unsigned int uiHash = 0;

   while( *Name )
   {
      uiHash = ( uiHash << 1 ) + *Name;
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

SYMBOL * Symbol_Put( SYMBOLTABLE *pSymbolTable, char *Name, SYMBOL_KIND Kind )
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
   }

   pSymbol = NEW( SYMBOL );

   pSymbol->Kind  = Kind;
   pSymbol->pID   = New_ID( Name ); 
   pSymbol->pNext = pSymbolTable->Buckets[ iHash ];

   pSymbolTable->Buckets[ iHash ] = pSymbol;

   return pSymbol;
}
  
SYMBOL *Symbol_Get( SYMBOLTABLE *pSymbolTable, char *Name )
{
   int iHash;
   SYMBOL *pSymbol = pSymbolTable->Buckets[ ( iHash = Hash( Name ) ) ];

   while( pSymbol )
   {
      if( strcmp( pSymbol->pID->Name, Name ) == 0 )
      {
         return pSymbol;
      }
   }

   return NULL;
}
