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

   #define HASHSIZE 317

   typedef enum 
   {
      SYMBOL_KIND_FUNCTION  = 0x0001, 
      SYMBOL_KIND_FUNC_CALL = 0x0002,

      SYMBOL_KIND_VAR       = 0x0064,
      SYMBOL_KIND_STATIC    = 0x0065,
      SYMBOL_KIND_LOCAL     = 0x0066,
      SYMBOL_KIND_MEMVAR    = 0x0067,
      SYMBOL_KIND_FIELD     = 0x0066,
      SYMBOL_KIND_GLOBAL    = 0x0068,
   } 
   SYMBOL_KIND;

   typedef struct _SYMBOL
   {  
       SYMBOL_KIND Kind;
       ID *pID;
       struct _SYMBOL *pNext;   
   } SYMBOL;

   typedef struct _SYMBOLTABLE
   {  
       SYMBOL *Buckets[ HASHSIZE ];   
   } SYMBOLTABLE;
        
   /* Initialize the hash table which makes up the symbol table. */   
   SYMBOLTABLE * Symbol_InitTable();   
      
   SYMBOL * Symbol_Put( SYMBOLTABLE *SymbolTable, char *Name, SYMBOL_KIND Kind );         
   SYMBOL * Symbol_Get( SYMBOLTABLE *SymbolTable, char *Name);

#endif