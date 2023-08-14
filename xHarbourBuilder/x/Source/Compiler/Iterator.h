//
//  Iterator.h
//  x
//
//  Created by Ron Pinkas on 6/26/13.
//  Copyright (c) 2013 http://www.xHarbour.com. All rights reserved.
//

#ifndef ITERATOR_DEFINED

   #define ITERATOR_DEFINED

   extern int IterateMacro( MACRO * pMacro, PARSER_CONTEXT *Parser_pContext, int iNestingLevel, BOOL bAssign );
   extern int IterateVariable( DECLARED *pVariable, PARSER_CONTEXT *Parser_pContext, int iNestingLevel, VALUE_KIND Kind );
   extern int IterateAliasedField( ALIASED_FIELD *pAliasedField, PARSER_CONTEXT *Parser_pContext, int iNestingLevel, BOOL bAssign );
   extern int IterateAST( PARSER_CONTEXT *Parset_pContext );
   extern int IterateBody( BODY *pBody, PARSER_CONTEXT *Parser_pContext, int iNestedLevel );
   extern int IterateLine( LINE *pLine, PARSER_CONTEXT *Parser_pContext, int iNestedLevel );
   extern int IterateBinary( BINARY *pBinary, PARSER_CONTEXT *Parser_pContext, int iNestingLevel );
   extern int IterateValue( VALUE *pValue, PARSER_CONTEXT *Parser_pContext, int iNestedLevel );

#endif
