#ifndef RELEASE_DEFINED

   #define RELEASE_DEFINED

   void Release_LValue( LVALUE *pLValue );
   void Release_List( LIST *pList );
   void Release_Block( BLOCK *pBlock );
   void Release_Unary( UNARY *pUnary );
   void Release_Binnary( BINARY *pBinary );
   void Release_Alias( ALIASED *pAliased );
   void Release_Assignment( ASSIGNMENT *pAssignment );
   void Release_FuncCall( FUNC_CALL *pFuncCall );
   void Release_IIF( IIF *pIIF );
   void Release_MethodCall( METHOD_CALL *pMethodCall );
   void Release_Value( VALUE *pValue );

#endif