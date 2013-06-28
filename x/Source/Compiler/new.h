#ifndef NEW_DEFINED

   #define NEW_DEFINED

   #define NEW( TYPE )   (TYPE *) ClipNet_alloc( sizeof( TYPE ) )
   #define ZERO( p ) memset( p, 0, sizeof( *p ) )

   PARSER_CONTEXT *    New_Context();

   ID *                New_ID( char *sName, PARSER_CONTEXT *Parser_pContext );

   FUNCTION *          New_Function( char *sFunc, FUNC_KIND Kind, PARSER_CONTEXT *Parser_pContext );
   BODY *              New_Body();
   LINE *              New_Line( void *x, LINE_KIND Kind, PARSER_CONTEXT *Parser_pContext );
   INLINE *            New_Inline( char *sName, PARSER_CONTEXT *Parser_pContext );
                      
   VALUE *             New_Value( void *x, VALUE_KIND Kind, PARSER_CONTEXT *Parser_pContext );
   VALUE *             New_NIL( PARSER_CONTEXT *Parser_pContext );
   VALUE *             New_Constant( CONSTANT *pConstant, PARSER_CONTEXT *Parser_pContext );                      
   VALUE *             New_Block( PARSER_CONTEXT *Parser_pContext );
   VALUE *             New_Unary( VALUE * pValue, UNARY_KIND Kind, UNARY_WHEN When, PARSER_CONTEXT *Parser_pContext );
   VALUE *             New_LValue( void *x, LVALUE_KIND Kind, PARSER_CONTEXT *Parser_pContext );
   VALUE *             New_LValueID( char *sName, DECLARED_KIND Kind, PARSER_CONTEXT *Parser_pContext );
   VALUE *             New_Binary( VALUE * pLeft, VALUE * pRight, BINARY_KIND Kind, PARSER_CONTEXT *Parser_pContext );
   VALUE *             New_Macro( void *x, MACRO_KIND Kind, PARSER_CONTEXT *Parser_pContext );                      
   VALUE *             New_Aliased( VALUE * pArea, VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE *             New_IIF( VALUE * pCond, VALUE * pTrue, VALUE * pFalse, PARSER_CONTEXT *Parser_pContext );
   VALUE *             New_Assignment( VALUE * pLValue, VALUE * pValue, ASSIGNMENT_KIND Kind, PARSER_CONTEXT *Parser_pContext );
   VALUE *             New_FuncCall( VALUE * pSymVal, VALUE * pArgList, PARSER_CONTEXT *Parser_pContext );                      
   VALUE *             New_ArrayElement( VALUE * pArray, VALUE * pIndexList, PARSER_CONTEXT *Parser_pContext );
                                            
   LINE *              New_If( VALUE * pCondExp, BODY *pBody, PARSER_CONTEXT *Parser_pContext );
   LINE *              New_ElseIf( VALUE * pCondExp, BODY *pBody, PARSER_CONTEXT *Parser_pContext );
   LINE *              New_Else( BODY *pBody, PARSER_CONTEXT *Parser_pContext );
                                            
   DECLARED *          New_Declared( PARSER_CONTEXT *Parser_pContext ); 
   DECLARED *          New_DeclaredID( char *sName, DECLARED_KIND Kind, PARSER_CONTEXT *Parser_pContext );

   LIST *              New_ListNode( LIST *pList, VALUE *pValue, PARSER_CONTEXT *Parser_pContext );
   LIST *              New_List( VALUE *pValue, PARSER_CONTEXT *Parser_pContext );
                      
 #if 0                
   SYMBOL *           New_SymbolID( char *sName, PARSER_CONTEXT *Parser_pContext );
   SYMBOL *           New_SymbolFromValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   ID *               New_IDFromValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   LIST *             New_ListFromValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
 #endif                                                          

#endif