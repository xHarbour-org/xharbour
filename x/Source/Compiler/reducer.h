#ifndef REDUCER_DEFINED

   #define REDUCER_DEFINED

   VALUE * ( *ReduceValue[] )( VALUE * );

   int BinaryPrecedence[];

   void Reducer_Init( void );

   VALUE * Reduce_NIL( VALUE * pValue );
   VALUE * Reduce_Constant( VALUE * pValue );
   VALUE * Reduce_LValue( VALUE * pValue );
   VALUE * Reduce_Array( VALUE * pValue );
   VALUE * Reduce_Block( VALUE * pValue );
   VALUE * Reduce_Unary( VALUE * pValue );
   VALUE * Reduce_Binary( VALUE * pValue );
   VALUE * Reduce_Aliased( VALUE * pValue );
   VALUE * Reduce_Assignment( VALUE * pValue );
   VALUE * Reduce_FuncCall( VALUE * pValue );
   VALUE * Reduce_IIF( VALUE * pValue );
   VALUE * Reduce_MethodCall( VALUE * pValue );
   VALUE * Reduce_List( VALUE * pValue );

   VALUE * Get_LValue( VALUE * pValue );

#endif