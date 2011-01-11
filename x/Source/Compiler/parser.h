/*
 * ClipNet Project source code:
 * Parser data structures.
 *
 * Copyright 2001 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.RonPinkas.com
 *
 */

#ifndef PARSER_DEFINED
   #include "tokens.h"

   int yylex( void );

   #define ID_MAX_LEN 32

   #define PARSER_DEFINED

   #define LANG_C          0
   #define LANG_OBJ_MODULE 1

   #define LAST_TOKEN()   Parser_iToken

   #define NEXT_TOKEN()   ( Parser_iToken = ( Parser_iNextToken ? Parser_iNextToken : yylex() ), Parser_iNextToken = 0, Parser_iToken )
   #define PUSH_BACK()    ( Parser_iNextToken = Parser_iToken )

   #define PARSE_ERROR( i, text1, text2 ) \
              Parser_GenError( Parser_asErrors, 'E', i, text1, text2 ); \
              \
              if( text1 == NULL || text1[0] != '\n' ) \
              { \
                 while( ( Parser_iToken = yylex() ) > 0 && Parser_iToken != '\n' ) \
                 { \
                    ; \
                 } \
                 PUSH_BACK(); \
              }

   #define ACCEPT_EOS( iToken )  if( NEXT_TOKEN() != iToken && Parser_iToken != TOKEN_END ) \
                                 { \
                                    PARSE_ERROR( PARSER_ERR_UNCLOSED_STRU, yytext, NULL ); \
                                 }

   #define ACCEPT_EOL()  if( NEXT_TOKEN() != '\n' && Parser_iToken != -1 ) \
                         { \
                            PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", expected EOL." ); \
                         }

   #define EXPECTED_EOL()  if( Parser_iToken != '\n' && Parser_iToken != -1  ) \
                           { \
                              PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", expected EOL." ); \
                           }

   #define ACCEPT_TOKEN( iToken )  if( NEXT_TOKEN() != iToken ) \
                         { \
                            PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", expected " #iToken ); \
                         }

   #define EXPECTED_TOKEN( iToken )  if( Parser_iToken != iToken ) \
                           { \
                              PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", expected " #iToken ); \
                           }

   #define EOB( iToken ) ( ( iToken >= TOKEN_FUNC && iToken <= TOKEN_CRITICAL_STATIC_PROC ) || \
                             iToken == TOKEN_ELSE    || \
                             iToken == TOKEN_ELSEIF  || \
                             iToken == TOKEN_ENDIF   || \
                             iToken == TOKEN_END     || \
                             iToken == TOKEN_ENDCASE || \
                             iToken == TOKEN_ENDDO   || \
                             iToken == TOKEN_NEXT    || \
                             iToken == -1               \
                         )

   typedef enum
   {
      FUNC_KIND_FUNC                = TOKEN_FUNC,
      FUNC_KIND_FUNC_STATIC         = TOKEN_STATIC_FUNC,
      FUNC_KIND_FUNC_INIT           = TOKEN_INIT_FUNC,
      FUNC_KIND_FUNC_EXIT           = TOKEN_EXIT_FUNC,
      FUNC_KIND_FUNC_CRITICAL       = TOKEN_CRITICAL_FUNC,
      FUNC_KIND_FUNC_STATICCRITICAL = TOKEN_CRITICAL_STATIC_FUNC,
      FUNC_KIND_PROC                = TOKEN_PROC,
      FUNC_KIND_PROC_STATIC         = TOKEN_STATIC_PROC,
      FUNC_KIND_PROC_INIT           = TOKEN_INIT_PROC,
      FUNC_KIND_PROC_EXIT           = TOKEN_EXIT_PROC,
      FUNC_KIND_PROC_CRITICAL       = TOKEN_CRITICAL_PROC,
      FUNC_KIND_PROC_STATICCRITICAL = TOKEN_CRITICAL_STATIC_PROC
   } FUNC_KIND;

   typedef enum
   {
      PRG_TYPE_UNDEF   = 0x000,
      PRG_TYPE_NIL     = 0x001,
      PRG_TYPE_STRING  = 0x002,
      PRG_TYPE_CHAR    = 0x003,
      PRG_TYPE_LOGICAL = 0x004,
      PRG_TYPE_NUMERIC = 0x008,
      PRG_TYPE_DECIMAL = 0x009,
      PRG_TYPE_DATE    = 0x00A,
      PRG_TYPE_ARRAY   = 0x010,
      PRG_TYPE_BLOCK   = 0x020,
      PRG_TYPE_OBJECT  = 0x040,  
      PRG_TYPE_CLASS   = 0x080
   } PRG_TYPE;

   typedef enum
   {
      MACRO_KIND_SIMPLE,
      MACRO_KIND_COMPLEX
   } MACRO_KIND;

#if 0
   typedef enum
   {
      SYMBOL_KIND_ID,
      SYMBOL_KIND_MACRO
   } SYMBOL_KIND;
#endif

   typedef enum
   {
      DECLARED_KIND_NONE,
      DECLARED_KIND_LOCAL_PARAM,
      DECLARED_KIND_LOCAL,
      DECLARED_KIND_STATIC,
      DECLARED_KIND_MEMVAR,
      DECLARED_KIND_FIELD
   } DECLARED_KIND;

   typedef enum
   {
      EXECUTABLE_MEMVAR_KIND_PARAMETER,
      EXECUTABLE_MEMVAR_KIND_PRIVATE,
      EXECUTABLE_MEMVAR_KIND_PUBLIC
   } EXECUTABLE_MEMVAR_KIND;

   typedef enum
   {
      LVALUE_KIND_ID,
      LVALUE_KIND_MEMVAR,
      LVALUE_KIND_MACRO,

      LVALUE_KIND_ARRAY_ELEMENT,
      LVALUE_KIND_OBJ_PROPERTY,
      LVALUE_KIND_ALIASED_FIELD
   } LVALUE_KIND;

   typedef enum
   {
      CONSTANT_KIND_INTEGER,
      CONSTANT_KIND_LONG,
      CONSTANT_KIND_DOUBLE,
      CONSTANT_KIND_STRING,
      CONSTANT_KIND_BOOL,
      CONSTANT_KIND_DATE
   } CONSTANT_KIND;

   typedef enum
   {
     ASSIGNMENT_KIND_NORMAL   = TOKEN_INASSIGN,
     ASSIGNMENT_KIND_PLUS     = TOKEN_PLUSEQ,
     ASSIGNMENT_KIND_MINUS    = TOKEN_MINUSEQ,
     ASSIGNMENT_KIND_MULTIPLY = TOKEN_MULTEQ,
     ASSIGNMENT_KIND_DIVIDE   = TOKEN_DIVEQ,
     ASSIGNMENT_KIND_POWER    = TOKEN_POWER,
     ASSIGNMENT_KIND_EXP      = TOKEN_EXPEQ,
     ASSIGNMENT_KIND_MOD      = TOKEN_MODEQ
   } ASSIGNMENT_KIND;

   typedef enum
   {
      VALUE_KIND_NIL,
      VALUE_KIND_CONSTANT,
      VALUE_KIND_LVALUE,
      VALUE_KIND_ARRAY,
      VALUE_KIND_BLOCK,
      VALUE_KIND_UNARY,
      VALUE_KIND_BINARY,
      VALUE_KIND_ALIASED,
      VALUE_KIND_ASSIGNMENT,
      VALUE_KIND_FUNC_CALL,
      VALUE_KIND_IIF,
      VALUE_KIND_METHOD_CALL,
      VALUE_KIND_LIST,
      VALUE_KIND_BYREF
   } VALUE_KIND;

   typedef enum
   {                               
      BINARY_KIND_POWER        = '^',

      BINARY_KIND_MODULUS      = '%',
      BINARY_KIND_MULTIPLY     = '*',
      BINARY_KIND_DIVIDE       = '/',

      BINARY_KIND_PLUS         = '+',
      BINARY_KIND_MINUS        = '-',

      BINARY_KIND_BITLEFT      = TOKEN_BITSHIFTL,
      BINARY_KIND_BITRIGHT     = TOKEN_BITSHIFTR,

      BINARY_KIND_IN           = '$',
      BINARY_KIND_LIKE         = '~',
      BINARY_KIND_HAS          = '?',
      BINARY_KIND_GREATEREQUAL = 'ò',
      BINARY_KIND_LESSEREQUAL  = 'ó',
      BINARY_KIND_GREATER      = '>',
      BINARY_KIND_LESSER       = '<',

      BINARY_KIND_EXACTEQUAL   = TOKEN_EQ,
      BINARY_KIND_EQUAL        = '=',
      BINARY_KIND_NOTEQUAL     = '#',

      BINARY_KIND_BITOR        = TOKEN_BITOR,
      BINARY_KIND_BITXOR       = TOKEN_BITXOR,
      BINARY_KIND_BITAND       = TOKEN_BITAND,    

      BINARY_KIND_AND          = TOKEN_AND,
      BINARY_KIND_OR           = TOKEN_OR
   } BINARY_KIND;

   typedef enum
   {
      FLOW_KIND_LOOP,
      FLOW_KIND_EXIT,
      FLOW_KIND_BREAK
   } FLOW_KIND;

   typedef enum
   {
      LINE_KIND_ASSIGNMENT,

      LINE_KIND_CASE,
      LINE_KIND_OTHERWISE,

      LINE_KIND_FOR,
      LINE_KIND_WHILE,
      LINE_KIND_FLOW,

      LINE_KIND_FUNC_CALL,
      LINE_KIND_IIF,
      LINE_KIND_METHOD_CALL,

      LINE_KIND_IF,
      LINE_KIND_ELSEIF,
      LINE_KIND_ELSE,

      LINE_KIND_PARAMETERS,
      LINE_KIND_PRIVATES,
      LINE_KIND_PUBLICS,

      LINE_KIND_RETURN, 

      LINE_KIND_SEQUENCE,
      LINE_KIND_RECOVER,

      LINE_KIND_SWITCH,
      LINE_KIND_SWITCHCASE,
      LINE_KIND_SWITCHDEAFULT,

      LINE_KIND_TRY,
      LINE_KIND_CATCH,
      LINE_KIND_FINALLY,

      LINE_KIND_UNARY

   } LINE_KIND;
    
   typedef enum
   {
      UNARY_WHEN_PRE, 
      UNARY_WHEN_POST  
   } 
   UNARY_WHEN;

   typedef enum 
   {
      UNARY_KIND_INC = TOKEN_INC, 
      UNARY_KIND_DEC = TOKEN_DEC
   } 
   UNARY_KIND;

   typedef struct _CONSTANT
   {
      CONSTANT_KIND Kind;
      PRG_TYPE Type;
      union
      {
         struct
         {
            int iInteger;
         } Integer;

         struct 
         {
            long lLong;
         } Long;

         struct  
         {
            long lDate;
         } Date;

         struct
         {
            double dDouble;
            char   cWidth;
            char   cDec;
         } Double;

         char *sString;
         BOOL bLogical;
      } Value;

   } CONSTANT;

   /* Forward Declaration */
   struct _VALUE;

   typedef struct _ID 
   {
      char Name[ ID_MAX_LEN ];
   } ID; 

   typedef struct _MACRO
   {
       MACRO_KIND Kind;

       union
       {
          ID *           pID;
          struct _VALUE *pComplex;
       } Value;

   } MACRO;
  
   typedef struct _DECLARED
   {
       DECLARED_KIND Kind;

       ID *           pName;
       struct _VALUE *pInit;

       struct _DECLARED *pNext; 
   } DECLARED;

#if 0
   typedef struct _SYMBOL 
   { 
      SYMBOL_KIND Kind;
 
      union
      { 
         ID * pID;
         MACRO * pMacro;
     } Value;  
   } SYMBOL; 
#endif

   typedef struct _EXECUTABLE_MEMVAR 
   {
       EXECUTABLE_MEMVAR_KIND Kind;

       struct _VALUE *pName;
       struct _VALUE *pInit;

       struct _EXECUTABLE_MEMVAR *pNext; 
   } EXECUTABLE_MEMVAR;

   /* Forward Declarations. */
   struct _ARRAY_ELEMENT;
   struct _OBJ_PROPERTY;
   struct _ALIASED_FIELD;
   struct _ID;
  
   typedef struct _LVALUE
   {
      LVALUE_KIND Kind;
      PRG_TYPE Type;

      union
      {
         ID *                    pID;
         ID *                    pMemvar;
         MACRO *                 pMacro;

         struct _ARRAY_ELEMENT *pArrayElement;
         struct _OBJ_PROPERTY  *pProperty;
         struct _ALIASED_FIELD *pAliasedField;
      } Value;

   } LVALUE;

   /* Forward Declarations. */
   struct _BLOCK;
   struct _ASSIGNMENT;
   struct _UNARY;
   struct _BINARY;
   struct _ALIASED;
   struct _FUNC_CALL;
   struct _IIF;
   struct _METHOD_CALL;

   typedef struct _VALUE
   {
      VALUE_KIND Kind;
      PRG_TYPE   Type;

      BOOL       bNegate;

      union
      {
         CONSTANT *             pConstant;
         LVALUE *               pLValue; 

         struct _LIST          *pArray;
         struct _BLOCK         *pBlock;
         struct _ASSIGNMENT    *pAssignment;
         struct _UNARY         *pUnary;
         struct _BINARY        *pBinary;
         struct _ALIASED       *pAliased;
         struct _FUNC_CALL     *pFuncCall;
         struct _IIF           *pIIF;
         struct _METHOD_CALL   *pMethodCall;
         struct _LIST          *pList; 
      } Value;
 
   } VALUE;


   typedef struct _ARRAY_ELEMENT
   {
      PRG_TYPE Type;

      VALUE * pArray;
      VALUE * pIndexList;
   } ARRAY_ELEMENT;

   typedef struct _UNARY
   {
      UNARY_KIND Kind;
      PRG_TYPE Type;

      VALUE *    pLValue;
      UNARY_WHEN When;
   } UNARY;

   typedef struct _BINARY
   {
      BINARY_KIND Kind;
      PRG_TYPE Type;

      VALUE * pLeft;
      VALUE * pRight;
   } BINARY;

   typedef struct _ASSIGNMENT
   {
      ASSIGNMENT_KIND Kind;
      PRG_TYPE Type;

      VALUE *   pLValue;
      VALUE *   pValue;
   } ASSIGNMENT;

   typedef struct _ALIASED
   {
      PRG_TYPE Type;

      VALUE * pArea;
      VALUE * pValue;
   } ALIASED;
 
   typedef struct _FUNC_CALL
   {
      PRG_TYPE Type;

      VALUE *  pSymbol;
      VALUE *  pArguments;
   } FUNC_CALL;

   typedef struct _IIF
   {
      PRG_TYPE Type;

      VALUE * pCond;
      VALUE * pTrue;
      VALUE * pFalse;
   } IIF;

   typedef struct _OBJ_PROPERTY
   {
      PRG_TYPE Type;

      VALUE * pObject;
      VALUE * pSymbol;
   } OBJ_PROPERTY;

   typedef struct _ALIASED_FIELD
   {
      PRG_TYPE Type;

      VALUE * pAlias;
      VALUE * pSymbol;
   } ALIASED_FIELD;
 
   typedef struct _METHOD_CALL
   {
      PRG_TYPE Type;

      VALUE * pObject;
      VALUE * pMethod;
   } METHOD_CALL;

   typedef struct _LIST_NODE
   {
      VALUE *pValue;
      struct _LIST_NODE *pNext;
   } LIST_NODE;

   typedef struct _LIST
   {
      PRG_TYPE Type;

      int        iNodes;

      LIST_NODE *pFirst;
      LIST_NODE *pLast;
   } LIST;

   /* Forward Declaration. */
   struct _LINE;

   typedef struct _BODY
   {
      struct _LINE *pLines;
   }  BODY;

   typedef struct _IF
   {
      VALUE * pCondExp;
      BODY *  pBody;
      struct _LINE *pElseIf;
      struct _LINE *pElse;
   } IF;

   typedef struct _ELSEIF
   {
      VALUE * pCondExp;
      BODY *  pBody;
      struct _LINE *pNext;
   } ELSEIF;

   typedef struct _ELSE
   {
      BODY * pBody;
   } ELSE;

   typedef struct _FOR
   {
      ASSIGNMENT * pInit;
      VALUE *      pCondExp;
      BODY *       pBody;
      VALUE *      pStep;
   } FOR;

   typedef struct _WHILE
   {
      VALUE * pCondExp;
      BODY *  pBody;
   } WHILE;

   typedef struct _DOCASE
   {
      struct _LINE *pCase;
      struct _LINE *pOtherwise;
   } DOCASE;

   typedef struct _CASE
   {
      VALUE * pCondExp;
      BODY *  pBody;
      struct _LINE *pNext;
   } CASE;

   typedef struct _OTHERWISE
   {
      BODY * pBody;
   } OTHERWISE;

   typedef struct _SWITCH
   {
      VALUE * pSwitch;
      struct _LINE *pCase;
      struct _LINE *pDefault;
   } SWITCH;

   typedef struct _SWITCHCASE
   {
      CONSTANT *pConstant;
      BODY *  pBody;
      struct _LINE *pNext;
   } SWITCHCASE;

   typedef struct _SWITCHDEFAULT
   {
      BODY * pBody;
   } SWITCHDEFAULT;

   typedef struct _RECOVER
   {
      VALUE pUsing;
      BODY * pBody;
   } RECOVER;

   typedef struct _SEQUENCE
   {
      BODY    *pBody;
      RECOVER *pRecover;
   } SEQUENCE;


   typedef struct _CATCH
   {
      VALUE pCatcher;
      BODY * pBody;
   } CATCH;

   typedef struct _TRY
   {
      BODY  *pBody;
      CATCH *pCatch;
      BODY  *pFinally;
   } TRY;

   typedef struct _LINE
   {
      LINE_KIND Kind;

      int       iNo;

      union
      { 
         VALUE               *pAssignment; 

         CASE                *pCase;
         OTHERWISE           *pOtherwise;

         FOR                 *pFor;
         WHILE               *pWhile;
         FLOW_KIND           Flow; 

         IF                  *pIf;
         ELSEIF              *pElseIf;
         ELSE                *pElse;

         VALUE               *pFuncCall; 
         VALUE               *pIIF;
         VALUE               *pMethodCall;

         EXECUTABLE_MEMVAR   *pParamters;
         EXECUTABLE_MEMVAR   *pPrivates;
         EXECUTABLE_MEMVAR   *pPublics;

         SEQUENCE            *pSequence;
         RECOVER             *pRecover;

         VALUE               *pReturn;

         SWITCH              *pSwitch;
         SWITCHCASE          *pSwitchCase;
         SWITCHDEFAULT       *pSwitchDefault;

         TRY                 *pTry;
         CATCH               *pCatch;
         BODY                *pFinally;

         VALUE               *pUnary; 
      } Value;

      struct _LINE *pNext;
   } LINE;

   typedef struct _FUNCTION
   {
      FUNC_KIND          Kind;
      BOOL               bProcedure;

      ID *                pName;
      DECLARED *          pLocalParams;
      int                iLocalParams;
      DECLARED *          pLocals;
      int                iLocals;
      DECLARED *          pStatics;
      int                iStatics;
      DECLARED *          pMemvars;
      int                iMemvars;
      DECLARED *          pFields;
      int                iFields;
      EXECUTABLE_MEMVAR * pParams;
      int                iParams;
      EXECUTABLE_MEMVAR * pPrivates;
      int                iPrivates;
      EXECUTABLE_MEMVAR * pPublics;
      int                iPublics;
      BODY *              pBody;

      struct _FUNCTION *pNext;
   } FUNCTION;

   typedef struct _FUNCTIONS
   {
      struct _FUNCTION *pFirst;
      struct _FUNCTION *pLast;
      int              iFunctions;
   } FUNCTIONS;

   typedef struct _BLOCK
   {
      DECLARED * pBlockLocals;
      int    iBlockLocals;
      VALUE * pList;
   } BLOCK;

   typedef struct _INLINE
   {
      char           *sName;
      BYTE           *pCode;
      ULONG          lPCodeSize;
      char           *sFileName;
      int            iLine;
      struct _INLINE *pNext;
   } INLINE;

   typedef struct _INLINES
   {
      INLINE * pFirst;
      INLINE * pLast;
      int     iCount;
   } INLINES;

   typedef struct _YYSTYPE
   {
      char     *sText;
      CONSTANT Constant;
   } YYSTYPE;

   typedef struct _PARSED_FILE 
   {
      FILE  *hFile;
      char  *sName;
      struct _PARSED_FILE *pPrev;
      struct _PARSED_FILE *pNext;
      int   iLine;
   } PARSED_FILE;

   typedef struct
   {
      PARSED_FILE *pFirst;
      PARSED_FILE *pLast;
      int   iFiles;
   } PARSED_FILES;

   extern int Parser_iBackend;
   extern int Parser_iInlineID;

   extern int Parser_iLine, Parser_iErrors, Parser_iWarnings;
   extern int Parser_iLinePRG, Parser_iLineINLINE;
   extern int Parser_iNextToken;
   extern int Parser_iToken;
   
   extern BOOL Parser_bError, Parser_bAnyWarning, Parser_bMute, Parser_bPPO;
   
   extern FUNCTIONS    Parser_Functions;
   extern PARSED_FILES Parser_Files;
   extern INLINES      Parser_Inlines;
   extern FILE         *pParser_PPO;
   
   extern YYSTYPE   yylval;
   
   extern char *Parser_asErrors[];
   extern char *yytext;

   #if defined(__cplusplus)
      extern "C" {
   #endif

   void Parser_Init( void );

   INLINE * Parser_InlineAdd( char * szFunName );

   void DumpLine( LINE *pLine, int *piSpaces );

   #if defined(__cplusplus)
      }
   #endif

#endif