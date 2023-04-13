/*
 * ClipNet Project source code:
 * Common defines.
 *
 * Copyright 2001 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.RonPinkas.com
 *
 */

#ifndef SYMBOL_DEFINED

   #define SYMBOL_DEFINED

   #define HASHSIZE 2039

   typedef enum 
   {
      SYMBOL_KIND_VAR       = 0x0010,
      SYMBOL_KIND_STATIC    = SYMBOL_KIND_VAR | 0x01,
      SYMBOL_KIND_LOCAL     = SYMBOL_KIND_VAR | 0x02,
      SYMBOL_KIND_MEMVAR    = SYMBOL_KIND_VAR | 0x03,
      SYMBOL_KIND_FIELD     = SYMBOL_KIND_VAR | 0x04,
      SYMBOL_KIND_GLOBAL    = SYMBOL_KIND_VAR | 0x05,

      SYMBOL_KIND_FUNCTION  = 0x0100,
      SYMBOL_KIND_FUNC_CALL = 0x0200,
   } 
   SYMBOL_KIND;

   typedef struct _SYMBOL
   {  
      ID *pID;
      SYMBOL_KIND Kind;
      struct _SYMBOL *pNext;
   } SYMBOL;

   typedef struct _SYMBOLTABLE
   {  
      SYMBOL *Buckets[ HASHSIZE ];
   } SYMBOLTABLE;
        
   /* Initialize the hash table which makes up the symbol table. */   
   SYMBOLTABLE * Symbol_InitTable(void);   
      
   SYMBOL * Symbol_Put( SYMBOLTABLE *SymbolTable, char *Name, SYMBOL_KIND Kind, PARSER_CONTEXT *Parser_pContext );
   SYMBOL * Symbol_Get( SYMBOLTABLE *SymbolTable, char *Name, PARSER_CONTEXT *Parser_pContext );

#endif
