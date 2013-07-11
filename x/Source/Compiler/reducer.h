#ifndef REDUCER_DEFINED

   #define REDUCER_DEFINED

   extern int BinaryPrecedence[ TOKEN_MAX_TOKENS ];

   void Reducer_Init( void );

   // Worker
   LIST  * Reduce_List( LIST * pList, PARSER_CONTEXT *Parser_pContext );

   VALUE * Reduce_ConstantValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );

   VALUE * Reduce_ArrayValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_BlockValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_ByRefValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_ListValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );

   VALUE * Reduce_IIFValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );

   VALUE * Reduce_UnaryValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );

   VALUE * Reduce_AliasedValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_BinaryValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_AssignmentValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );

   VALUE * Reduce_FuncCallValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_MethodCallValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );

   VALUE * Reduce_VariableValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_MacroValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_ArrayElementValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_ObjectPropertyValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );
   VALUE * Reduce_AliasedFieldValue( VALUE * pValue, PARSER_CONTEXT *Parser_pContext );

#endif