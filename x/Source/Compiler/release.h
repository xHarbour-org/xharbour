#ifndef RELEASE_DEFINED

   #define RELEASE_DEFINED

   void Release_ListNode( LIST_NODE *pListNode );
   void Release_List( LIST *pList );
   void Release_Block( BLOCK *pBlock );
   void Release_Unary( UNARY *pUnary );
   void Release_Binnary( BINARY *pBinary );
   void Release_Alias( ALIASED *pAliased );
   void Release_Assignment( ASSIGNMENT *pAssignment );
   void Release_FunctionCall( FUNCTION_CALL *pFunctionCall );
   void Release_IIF( IIF *pIIF );
   void Release_MethodCall( METHOD_CALL *pMethodCall );
   void Release_Value( VALUE *pValue );

   void Release_Line( LINE *pLine );
#endif
