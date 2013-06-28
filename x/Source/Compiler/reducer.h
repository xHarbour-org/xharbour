#ifndef REDUCER_DEFINED

   #define REDUCER_DEFINED

   extern int BinaryPrecedence[ TOKEN_MAX_TOKENS ];

   void Reducer_Init( void );

   VALUE * Reduce_NIL( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_Constant( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_LValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_Array( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_Block( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_Unary( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_Binary( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_Aliased( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_Assignment( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_FuncCall( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_IIF( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_MethodCall( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_List( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );

   VALUE * Get_LValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );

#endif