/*
 * ClipNet Project source code:
 * Parser data structures.
 *
 * Copyright 2001 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.RonPinkas.com
 *
 */

#ifndef PARSER_DEFINED
   #include <setjmp.h>

   #define ID_MAX_LEN 32

   #define PARSER_DEFINED

   #define LANG_C          0
   #define LANG_OBJ_MODULE 1


   #define LOOK_AHEAD_TOKEN() ( \
                                Parser_pContext->iNextToken ? \
                                  Parser_pContext->iNextToken : \
                                  ( Parser_pContext->iNextToken = yylex( CARGO )  ) \
                              )

   #define DROP_AHEAD_TOKEN() ( Parser_pContext->iNextToken = yylex( CARGO ) )

   #define USE_AHEAD_TOKEN()  ( \
                                Parser_pContext->iToken = Parser_pContext->iNextToken, \
                                Parser_pContext->iNextToken = yylex( CARGO ) \
                              )

   #define LAST_TOKEN()       ( Parser_pContext->iToken )

#ifdef PARSE_ERROR_FUNCTION
   void PARSE_ERROR( int iError, const char *sError1, const char *sError2, PARSER_CONTEXT *Parser_pContext );
#else
   #define PARSE_ERROR( i, text1, text2, Parser_pContext ) \
          Parser_pContext->sErrorSource = __SOURCE__; \
          Parser_pContext->sError1 = text1; \
          Parser_pContext->sError2 = text2; \
          \
          assert( Parser_pContext->bCanJump ); \
          \
          if( Parser_pContext->bCanJump ) \
          { \
             longjmp( Parser_pContext->JumpBuffer, i ); \
          }
#endif

   #define ACCEPT_END( iToken )  if( LOOK_AHEAD_TOKEN() == iToken || LOOK_AHEAD_TOKEN() == TOKEN_END ) \
                                 { \
                                    USE_AHEAD_TOKEN(); \
                                 } \
                                 else \
                                 { \
                                    PARSE_ERROR( PARSER_ERR_UNCLOSED_STRU, yytext, NULL, Parser_pContext ); \
                                 }

   #define ACCEPT_EOL()  if( LOOK_AHEAD_TOKEN() == '\n' || LOOK_AHEAD_TOKEN() == -1 ) \
                         { \
                            USE_AHEAD_TOKEN(); \
                         } \
                         else \
                         { \
                            PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", expected EOL." " Source: "__SOURCE__, Parser_pContext); \
                         }

   #define ACCEPT_TOKEN_AND_BREAK( iToken )  if( LOOK_AHEAD_TOKEN() == iToken ) \
                                   { \
                                      USE_AHEAD_TOKEN(); \
                                      break; \
                                    } \
                                    else \
                                    { \
                                       PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", expected " #iToken " Source: "__SOURCE__, Parser_pContext ); \
                                    }

   #define ACCEPT_TOKEN( iToken )  if( LOOK_AHEAD_TOKEN() == iToken ) \
                                   { \
                                      USE_AHEAD_TOKEN(); \
                                   } \
                                   else \
                                   { \
                                      PARSE_ERROR( PARSER_ERR_SYNTAX, yytext, ", expected " #iToken " Source: "__SOURCE__, Parser_pContext ); \
                                   }

   #define NEW_DEFINITION( iToken ) ( iToken >= FUNC_KIND_MIN && iToken <= FUNC_KIND_MAX )

   #define EOB( iToken ) ( \
                           NEW_DEFINITION( iToken ) || \
                           iToken == TOKEN_ELSE     || \
                           iToken == TOKEN_ELSEIF   || \
                           iToken == TOKEN_ENDIF    || \
                           iToken == TOKEN_END      || \
                           iToken == TOKEN_ENDCASE  || \
                           iToken == TOKEN_ENDDO    || \
                           iToken == TOKEN_NEXT     || \
                           iToken == -1                \
                         )

   typedef enum
   {
      FUNC_KIND_MIN                 = TOKEN_FUNC,
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
      FUNC_KIND_PROC_STATICCRITICAL = TOKEN_CRITICAL_STATIC_PROC,
      FUNC_KIND_MAX                 = TOKEN_CRITICAL_STATIC_PROC
   } FUNC_KIND;

   typedef enum
   {
      PRG_TYPE_UNDEF    = 0x0000,
      PRG_TYPE_NIL      = 0x0001,
      PRG_TYPE_STRING   = 0x0002,
      PRG_TYPE_CHAR     = 0x0003,
      PRG_TYPE_LOGICAL  = 0x0004,
      PRG_TYPE_NUMERIC  = 0x0008,
      PRG_TYPE_LONG     = PRG_TYPE_NUMERIC | 0x01,
      PRG_TYPE_DECIMAL  = PRG_TYPE_NUMERIC | 0x02,
      PRG_TYPE_DATE     = 0x0020,
      PRG_TYPE_DATETIME = 0x0021,
      PRG_TYPE_BLOCK    = 0x0040,
      PRG_TYPE_OBJECT   = 0x0080,
      PRG_TYPE_CLASS    = 0x0081,
      PRG_TYPE_ARRAY    = 0x0100,
      PRG_TYPE_POINTER  = 0x0200,
      PRG_TYPE_NONE     = 0xF000,
      PRG_TYPE_ANY      = 0xFFFF
   } PRG_TYPE;

   typedef enum
   {
      MACRO_KIND_SIMPLE,
      MACRO_KIND_COMPLEX
   } MACRO_KIND;

   typedef enum
   {
      MACRO_RESOLUTION_UNKNOWN = 0,
      
      MACRO_RESOLUTION_MEMVAR  = TOKEN_MEMVAR,
      MACRO_RESOLUTION_FIELD   = TOKEN_FIELD,
   } MACRO_RESOLUTION;

   typedef enum
   {
      DECLARED_KIND_NONE            = 0,
      DECLARED_KIND_LOCAL_PARAMETER = 1,
      
      DECLARED_KIND_GLOBAL          = TOKEN_GLOBAL,
      DECLARED_KIND_EXTERNALGLOBAL  = TOKEN_EXTERNGLOBAL,
      DECLARED_KIND_LOCAL           = TOKEN_LOCAL,
      DECLARED_KIND_STATIC          = TOKEN_STATIC,
      DECLARED_KIND_FIELD           = TOKEN_FIELD,
      DECLARED_KIND_MEMVAR          = TOKEN_MEMVAR,
      
      DECLARED_KIND_PARAMETER = TOKEN_PARAMETERS,
      DECLARED_KIND_PRIVATE   = TOKEN_PRIVATE,
      DECLARED_KIND_PUBLIC    = TOKEN_PUBLIC,

      DECLARE_KIND_DETACHED   = 0x0F00
   } DECLARED_KIND;

   typedef enum
   {
      CONSTANT_KIND_NIL,      
      CONSTANT_KIND_INTEGER,
      CONSTANT_KIND_LONG,
      CONSTANT_KIND_DOUBLE,
      CONSTANT_KIND_STRING,
      CONSTANT_KIND_BOOL,
      CONSTANT_KIND_DATE,
      CONSTANT_KIND_DATETIME
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
      VALUE_KIND_NONE            = 0x000,
      VALUE_KIND_CONSTANT        = 0x001,

      VALUE_KIND_ARRAY           = 0x002,
      VALUE_KIND_BLOCK           = 0x003,
      VALUE_KIND_BYREF           = 0x004,
      VALUE_KIND_LIST            = 0x005,

      
      VALUE_KIND_IIF             = 0x006,
      
      VALUE_KIND_UNARY           = 0x007,

      VALUE_KIND_ALIASED         = 0x008,
      VALUE_KIND_BINARY          = 0x009,
      VALUE_KIND_ASSIGNMENT      = 0x00A,
  
      VALUE_KIND_FUNCTION_CALL   = 0x00B,
      VALUE_KIND_METHOD_CALL     = 0x00C,

      VALUE_KIND_ASSIGNABLE_MASK = 0x100,

      VALUE_KIND_VARIABLE        = 0x00D | VALUE_KIND_ASSIGNABLE_MASK,
      VALUE_KIND_MACRO           = 0x00E | VALUE_KIND_ASSIGNABLE_MASK,
      VALUE_KIND_ARRAY_ELEMENT   = 0x00F | VALUE_KIND_ASSIGNABLE_MASK,
      VALUE_KIND_OBJECT_PROPERTY = 0x010 | VALUE_KIND_ASSIGNABLE_MASK,
      VALUE_KIND_ALIASED_FIELD   = 0x011 | VALUE_KIND_ASSIGNABLE_MASK,
      VALUE_KIND_FIELD           = 0x012 | VALUE_KIND_ASSIGNABLE_MASK,
      VALUE_KIND_MEMVAR          = 0x013 | VALUE_KIND_ASSIGNABLE_MASK,

      VALUE_KIND_ASSIGNED_MASK   = 0x200,

      VALUE_KIND_ERROR_MASK      = 0x800
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
      BINARY_KIND_GREATEREQUAL = (unsigned char) '\242',
      BINARY_KIND_LESSEREQUAL  = (unsigned char) '\243',
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
      FLOW_KIND_BREAK,
      FLOW_KIND_AGAIN // New AGAIN statement
   } FLOW_KIND;

   typedef enum
   {
      LINE_KIND_CASE,
      LINE_KIND_OTHERWISE,

      LINE_KIND_FOR,
      LINE_KIND_WHILE,
      LINE_KIND_FLOW,

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

      LINE_KIND_UNARY,
      LINE_KIND_ASSIGNMENT,
      LINE_KIND_IIF,
      LINE_KIND_FUNCTION_CALL,
      LINE_KIND_METHOD_CALL,
      
      LINE_KIND_LIST // REVIEW and TODO

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

   typedef enum
   {
      PARSING_STATE_GLOBAL_DECLARATIONS,
      PARSING_STATE_DECLARAIONS,
      PARSING_STATE_DEFINITION,
      PARSING_STATE_EXECUTABLE
   }
   PARSING_STATE;

   typedef struct _CONSTANT
   {
      CONSTANT_KIND Kind;
      PRG_TYPE Type;
 
      union
      {
         long lLong;

         long lDate;

         double dDateTime;

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
      struct _ID *pNext;
   } ID; 

   typedef struct _MACRO
   {
      MACRO_KIND Kind;
      MACRO_RESOLUTION Resolution;
      PRG_TYPE Type;

      union
      {
         ID *           pID;
         struct _VALUE *pComplex;
      } Value;

   } MACRO;
  
   typedef struct _DECLARED
   {
      DECLARED_KIND Kind;
      PRG_TYPE      Type;

      ID *             pID;
      
      union
      {
         struct _VALUE  *pInit;
         struct _ID     *pAlias;
      } Attribute;
      
      int              iLine;

      struct _DECLARED *pNext;
   } DECLARED;

   /* Forward Declarations. */
   struct _ARRAY_ELEMENT;
   struct _OBJECT_PROPERTY;
   struct _ALIASED_FIELD;
   struct _ID;

   /* Forward Declarations. */
   struct _BLOCK;
   struct _ASSIGNMENT;
   struct _UNARY;
   struct _BINARY;
   struct _ALIASED;
   struct _FUNCTION_CALL;
   struct _IIF;
   struct _METHOD_CALL;

   typedef struct _VALUE
   {
      VALUE_KIND Kind;
      PRG_TYPE   Type;

      char       bNegate;
      char       bNot;

      union
      {
         DECLARED                *pVariable;
         MACRO                   *pMacro;
         struct _ARRAY_ELEMENT   *pArrayElement;
         struct _OBJECT_PROPERTY *pObjectProperty;
         struct _ALIASED_FIELD   *pAliasedField;

         CONSTANT                *pConstant;
         
         struct _LIST            *pArray;
         struct _BLOCK           *pBlock;
         ID                      *pByRef;
         struct _LIST            *pList;

         struct _UNARY           *pUnary;

         struct _ASSIGNMENT      *pAssignment;
         struct _BINARY          *pBinary;
         struct _ALIASED         *pAliased;

         struct _IIF             *pIIF;

         struct _FUNCTION_CALL   *pFunctionCall;
         struct _METHOD_CALL     *pMethodCall;
      } Value;
 
   } VALUE;


   typedef struct _ARRAY_ELEMENT
   {
      PRG_TYPE Type;

      VALUE *        pArray;
      struct _LIST * pIndexList;
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
 
   typedef struct _FUNCTION_CALL
   {
      PRG_TYPE Type;

      VALUE *  pSymbol;
      VALUE *  pArguments;
   } FUNCTION_CALL;

   typedef struct _IIF
   {
      PRG_TYPE Type;

      VALUE * pCond;
      VALUE * pTrue;
      VALUE * pFalse;
   } IIF;

   typedef struct _OBJECT_PROPERTY
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
      VALUE *             pValue;
      struct _LIST_NODE * pNext;
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
         ASSIGNMENT          *pAssignment;

         CASE                *pCase;
         OTHERWISE           *pOtherwise;

         FOR                 *pFor;
         WHILE               *pWhile;
         FLOW_KIND           Flow; 

         IF                  *pIf;
         ELSEIF              *pElseIf;
         ELSE                *pElse;

         DECLARED            *pLocals;
         DECLARED            *pStatics;
         DECLARED            *pParamters;
         DECLARED            *pPrivates;
         DECLARED            *pPublics;

         SEQUENCE            *pSequence;
         RECOVER             *pRecover;

         VALUE               *pReturn;

         SWITCH              *pSwitch;
         SWITCHCASE          *pSwitchCase;
         SWITCHDEFAULT       *pSwitchDefault;

         TRY                 *pTry;
         CATCH               *pCatch;
         BODY                *pFinally;

         //Side Effect only
         FUNCTION_CALL       *pFuncttionCall;
         IIF                 *pIIF;
         METHOD_CALL         *pMethodCall;
         UNARY               *pUnary;
         LIST                *pList;
      } Value;

      struct _LINE *pNext;
   } LINE;

   typedef struct _FUNCTION
   {
      FUNC_KIND          Kind;
      PRG_TYPE           Type;

      ID *                pName;
      DECLARED *          pLocalParameters;
      int                 iLocalParameters;
      DECLARED *          pLocals;
      int                 iLocals;
      DECLARED *          pStatics;
      int                 iStatics;
      DECLARED *          pMemvars;
      int                 iMemvars;
      DECLARED *          pFields;
      int                 iFields;
      DECLARED *          pParameters;
      int                 iParameters;
      DECLARED *          pPrivates;
      int                 iPrivates;
      DECLARED *          pPublics;
      int                 iPublics;
      
      struct _MAP *       pDeclares;
      
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
      DECLARED *     pBlockLocals;
      DECLARED *     pDetached;
      int            iBlockLocals;
      struct _LIST * pList;
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

   typedef struct
   {
      int iBackend;
      int iInlineID;

      int iLine;
      int iErrors;
      int iWarnings;
      int iLinePRG;
      int iLineINLINE;
      int iNextToken;
      int iToken;
   
      const char *sErrorSource, *sError1, *sError2;
      
      BOOL bAnyWarning;
      BOOL bMute;
      BOOL bPPO;
   
      BOOL    bCanJump;
      jmp_buf JumpBuffer;
      
      DECLARED *   pExternGlobals;
      int          iExternGlobals;
      DECLARED *   pGlobals;
      int          iGlobals;
      DECLARED *   pStatics;
      int          iStatics;
      DECLARED *   pMemvars;
      int          iMemvars;
      DECLARED *   pFields;
      int          iFields;
      
      FUNCTIONS    Functions;
      PARSED_FILES Files;
      INLINES      Inlines;
      
      FILE *        pPPO;
      
      struct _MAP * pIDs;
      struct _MAP * pDeclares;

   } PARSER_CONTEXT;

   extern char *  Parser_asErrors[];

   extern YYSTYPE   yylval;
   extern char *yytext;

   #if defined(__cplusplus)
      extern "C" {
   #endif

   void Parser_Init( void );

   INLINE * Parser_InlineAdd( char * szFunName, PARSER_CONTEXT *Parser_pContext );

   void DumpLine( LINE *pLine, int *piSpaces, PARSER_CONTEXT *Parser_pContext );

#ifdef PARSE_ERROR_FUNCTION
   void PARSE_ERROR( int iError, const char *sError1, const char *sError2, PARSER_CONTEXT *Parser_pContext );
#endif
         
   #if defined(__cplusplus)
      }
   #endif

#endif
