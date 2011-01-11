#ifndef NEW_DEFINED

   #define NEW_DEFINED

   #define NEW( TYPE )   (TYPE *) ClipNet_alloc( sizeof( TYPE ) )
   #define ZERO( p ) memset( p, 0, sizeof( *p ) )

   ID *                New_ID( char *sName );

   FUNCTION *          New_Function( char *sFunc, FUNC_KIND Kind );
   BODY *              New_Body();
   LINE *              New_Line( void *x, LINE_KIND Kind );
   INLINE *            New_Inline( char *sName );
                      
   VALUE *             New_Value( void *x, VALUE_KIND Kind );
   VALUE *             New_NIL( void );
   VALUE *             New_Constant( CONSTANT *pConstant );                      
   VALUE *             New_Block( void );
   VALUE *             New_Unary( VALUE * pValue, UNARY_KIND Kind, UNARY_WHEN When );
   VALUE *             New_LValue( void *x, LVALUE_KIND Kind );
   VALUE *             New_LValueID( char *sName );
   VALUE *             New_Binary( VALUE * pLeft, VALUE * pRight, BINARY_KIND Kind );
   VALUE *             New_Macro( void *x, MACRO_KIND Kind );                      
   VALUE *             New_Aliased( VALUE * pArea, VALUE * pValue );
   VALUE *             New_IIF( VALUE * pCond, VALUE * pTrue, VALUE * pFalse );
   VALUE *             New_Assignment( VALUE * pLValue, VALUE * pValue, ASSIGNMENT_KIND Kind );
   VALUE *             New_FuncCall( VALUE * pSymVal, VALUE * pArgList );                      
   VALUE *             New_ArrayElement( VALUE * pArray, VALUE * pIndexList );
                                            
   LINE *              New_If( VALUE * pCondExp, BODY *pBody );
   LINE *              New_ElseIf( VALUE * pCondExp, BODY *pBody );
   LINE *              New_Else( BODY *pBody );
                                            
   DECLARED *          New_Declared( void ); 
   DECLARED *          New_DeclaredID( char *sName, DECLARED_KIND Kind );

   EXECUTABLE_MEMVAR * New_ExecutableMemvar( VALUE * pName, EXECUTABLE_MEMVAR_KIND Kind );

   LIST *              New_ListNode( LIST *pList, VALUE *pValue );
   LIST *              New_List( VALUE *pValue );
                      
 #if 0                
   SYMBOL *           New_SymbolID( char *sName );
   SYMBOL *           New_SymbolFromValue( VALUE * pValue );
   ID *               New_IDFromValue( VALUE * pValue );
   LIST *             New_ListFromValue( VALUE * pValue );
 #endif                                                          

#endif