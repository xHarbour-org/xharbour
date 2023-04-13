/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Harbour Compiler
 *
 * Copyright 1999 Ryszard Glab
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef HB_EXPROP_H_
#define HB_EXPROP_H_

#include "hbapi.h"
#include "hbpcode.h"

HB_EXTERN_BEGIN

/* value types seen at language level
 */
#define  HB_EV_UNKNOWN     0
#define  HB_EV_NIL         1
#define  HB_EV_NUMERIC     2
#define  HB_EV_DATE        4
#define  HB_EV_STRING      8
#define  HB_EV_CODEBLOCK   16
#define  HB_EV_LOGICAL     32
#define  HB_EV_OBJECT      64
#define  HB_EV_ARRAY       128
#define  HB_EV_SYMBOL      256
#define  HB_EV_VARREF      512
#define  HB_EV_FUNREF      1024

/* messages sent to expressions
 */
typedef enum
{
   HB_EA_REDUCE = 0,    /* reduce the expression into optimized one */
   HB_EA_ARRAY_AT,      /* check if the expession can be used as array */
   HB_EA_ARRAY_INDEX,   /* check if the expession can be used as index */
   HB_EA_LVALUE,        /* check if the expression can be used as lvalue (left side of an assigment) */
   HB_EA_PUSH_PCODE,    /* generate the pcodes to push the value of expression */
   HB_EA_POP_PCODE,     /* generate the pcodes to pop the value of expression */
   HB_EA_PUSH_POP,      /* generate the pcodes to push and pop the expression */
   HB_EA_STATEMENT,     /* generate the pcodes for a statement */
   HB_EA_DELETE         /* delete components of the expression */
} HB_EXPR_MESSAGE;

/* additional definitions used to distinguish numeric expressions
 */
#define  HB_ET_LONG     1
#define  HB_ET_DOUBLE   2

/* additional definitions used to distinguish macro expressions
 */
#define  HB_ET_MACRO_VAR       0   /* &variable */
#define  HB_ET_MACRO_SYMBOL    1   /* &funcall() */
#define  HB_ET_MACRO_ALIASED   2   /* &alias->&variable */
#define  HB_ET_MACRO_EXPR      4   /* &( expr ) */
#define  HB_ET_MACRO_ARGLIST   8   /* &variable used as a function call argument */
#define  HB_ET_MACRO_LIST     16   /* &variable used as in literal arrays or parentesised expressions. */
#define  HB_ET_MACRO_INDEX    32   /* &variable used as arrays index. */
#define  HB_ET_MACRO_PARE     64   /* &variable used as arrays index. */
#define  HB_ET_MACRO_VAR_REF 128   /* @&variable */

/* types of expressions
 * NOTE: the order of these definition is important - change it carefully
 *    All types <= HB_ET_FUNREF are constant values
 *    All types <= HB_ET_VARIABLE are a simple values
 *    All types > HB_ET_VARIABLE are operators
 */
typedef enum
{
   HB_ET_NONE = 0,
   HB_ET_EXTBLOCK,
   HB_ET_NIL,
   HB_ET_NUMERIC,
   HB_ET_DATE,
   HB_ET_STRING,
   HB_ET_CODEBLOCK,
   HB_ET_LOGICAL,
   HB_ET_SELF,
   HB_ET_ARRAY,
   HB_ET_VARREF,
   HB_ET_MEMVARREF,
   HB_ET_FUNREF,
   HB_ET_IIF,
   HB_ET_LIST,
   HB_ET_ARGLIST,
   HB_ET_ARRAYAT,
   HB_ET_MACRO,
   HB_ET_FUNCALL,
   HB_ET_ALIASVAR,
   HB_ET_ALIASEXPR,
   HB_ET_SEND,
   HB_ET_WITHSEND,
   HB_ET_FUNNAME,
   HB_ET_ALIAS,
   HB_ET_RTVAR,      /* PRIVATE or PUBLIC declaration of variable */
   HB_ET_VARIABLE,
   HB_EO_POSTINC,    /* post-operators -> lowest precedence */
   HB_EO_POSTDEC,
   HB_EO_ASSIGN,     /* assigments */
   HB_EO_PLUSEQ,
   HB_EO_MINUSEQ,
   HB_EO_MULTEQ,
   HB_EO_DIVEQ,
   HB_EO_MODEQ,
   HB_EO_EXPEQ,
   HB_EO_OR,         /* logical operators */
   HB_EO_AND,
   HB_EO_NOT,
   HB_EO_EQUAL,      /* relational operators */
   HB_EO_EQ,
   HB_EO_LT,
   HB_EO_GT,
   HB_EO_LE,
   HB_EO_GE,
   HB_EO_NE,
   HB_EO_IN,
   HB_EO_LIKE,
   HB_EO_MATCH,
   HB_EO_PLUS,       /* addition */
   HB_EO_MINUS,
   HB_EO_MULT,       /* multiple */
   HB_EO_DIV,
   HB_EO_MOD,
   HB_EO_POWER,
   HB_EO_BITAND,
   HB_EO_BITOR,
   HB_EO_BITXOR,
   HB_EO_BITSHIFTR,
   HB_EO_BITSHIFTL,
   HB_EO_NEGATE,     /* sign operator */
   HB_EO_PREINC,
   HB_EO_PREDEC      /* pre-operators -> the highest precedence */
} HB_EXPR_OPERATOR;


typedef struct HB_EXPR_
{
   union
   {
      struct
      {
         char * szName;                /* variable name */
         char * szNamespace;
      } asSymbol;

      BOOL asLogical;                  /* logical value */
      struct
      {
         USHORT type;                  /* type of datetime */
         long   date;                  /* date value */
         long   time;                  /* time value */
      } asDate;
      struct
      {
         char * string;                /* literal strings */
         BOOL   dealloc;               /* automatic deallocate on expresion deletion */
      } asString;
      struct
      {
        struct HB_EXPR_ * pMacro;      /* macro variable */
        char *            szName;      /* variable name  */
      } asRTVar;                       /* PUBLIC or PRIVATE variable declaration */
      struct
      {
         HB_LONG lVal;                 /* integer value */
         double dVal;                  /* double value */
         unsigned char bWidth;         /* unsigned char used intentionally */
         unsigned char bDec;           /* unsigned char used intentionally */
         unsigned char NumType;        /* used to distinguish LONG and DOUBLE */
      } asNum;
      struct
      {
         unsigned char     cMacroOp;   /* macro operator */
         unsigned char     SubType;    /* context in which macro is used */
         char *            szMacro;    /* identifier after the macro operator */
         struct HB_EXPR_ * pExprList;  /* list elements if &(...) was used */
         struct HB_EXPR_ * pFunCall;   /* pointer to a function if used as function's call argument */
      } asMacro;
      struct
      {
         struct HB_EXPR_ * pExprList;  /* list elements */
         struct HB_EXPR_ * pIndex;     /* array index, others */
         BOOL              bByRef;
         HB_PCODE          PopOp;
      } asList;
      struct
      {
         struct HB_EXPR_ * pAlias;     /* alias expression */
         struct HB_EXPR_ * pVar;       /* aliased variable or macro */
         struct HB_EXPR_ * pExpList;   /* aliased expression list */
      } asAlias;
      struct
      {
         struct HB_EXPR_ * pFunName;   /* function name */
         struct HB_EXPR_ * pParms;     /* function call parameters */
      } asFunCall;
      struct
      {
         struct HB_EXPR_ * pObject;    /* object */
         char *            szMessage;             /* message */
         struct HB_EXPR_ * pParms;     /* method parameters */
         BOOL              bByRef;
         struct HB_EXPR_ * pMacroMessage;
      } asMessage;
      struct
      {
         struct HB_EXPR_ * pLeft;      /* object */
         struct HB_EXPR_ * pRight;     /* object */
      } asOperator;
      struct
      {
         BYTE *  pCode;                /* pre-generated code */
         HB_SIZE ulLen;                /* length of pCode */
      } asExtBlock;
   } value;
   HB_SIZE           ulLength;
   HB_SIZE           Counter;
   unsigned char     ExprType;         /* internal expression type */
   USHORT            ValType;          /* language level value type */
   struct HB_EXPR_ * pNext;            /* next expression in the list of expressions */
} HB_EXPR, * PHB_EXPR;

/* Definitions of function templates used in expression's message
 * handling
 */
#ifdef HB_MACRO_SUPPORT
/* Compilation for macro compiler
 */
#define  HB_EXPR_FUNC( proc )  PHB_EXPR proc( PHB_EXPR pSelf, int iMessage, void * pMacro )

typedef  HB_EXPR_FUNC( HB_EXPR_FUNC_ );
typedef  HB_EXPR_FUNC_ * PHB_EXPR_FUNC;

extern PHB_EXPR_FUNC hb_comp_ExprTable[];

#define  HB_EXPR_USE( pSelf, iMessage )  \
         hb_comp_ExprTable[ (pSelf)->ExprType ]( (pSelf), (iMessage), pMacro )

typedef  PHB_EXPR HB_EXPR_ACTION( PHB_EXPR pSelf, int iMessage, void * pMacro );

#define HB_EXPR_PCODE0( action ) action( pMacro )
#define HB_EXPR_PCODE1( action, p1 ) action( (p1), pMacro )
#define HB_EXPR_PCODE2( action, p1, p2 ) action( (p1), (p2), pMacro )
#define HB_EXPR_PCODE3( action, p1, p2, p3 ) action( (p1), (p2), (p3), pMacro )
#define HB_EXPR_PCODE4( action, p1, p2, p3, p4 ) action( (p1), (p2), (p3), (p4), pMacro )
#define HB_EXPR_GENPCODE1( action, p1 ) action( (p1), pMacro )
#define HB_EXPR_GENPCODE2( action, p1, p2, p3 ) action( (p1), (p2), pMacro )
#define HB_EXPR_GENPCODE3( action, p1, p2, p3, p4 ) action( (p1), (p2), (p3), pMacro )
#define HB_EXPR_GENPCODE4( action, p1, p2, p3, p4, p5 ) action( (p1), (p2), (p3), (p4), pMacro )

#define HB_MACRO_VARNAME pMacro

#define HB_EXPR_ISEQUAL_IDS( szIdentifier1, szIdentifier2 ) ( strcmp( szIdentifier1, szIdentifier2 ) == 0 )
#define HB_EXPR_ISBUILTIN_ID( szIdentifier, BuiltInName ) HB_EXPR_ISEQUAL_IDS( szIdentifier, #BuiltInName )

#else

#define HB_EXPR_ISEQUAL_IDS( szIdentifier1, szIdentifier2 ) ( szIdentifier1 == szIdentifier2 )
#define HB_EXPR_ISBUILTIN_ID( szIdentifier, BuiltInName ) HB_EXPR_ISEQUAL_IDS( szIdentifier, hb_compExpr_IDs. BuiltInName )

#define HB_EXPR_FUNC( proc )  PHB_EXPR proc( PHB_EXPR pSelf, int iMessage )

typedef HB_EXPR_FUNC( HB_EXPR_FUNC_ );
typedef HB_EXPR_FUNC_ * PHB_EXPR_FUNC;

extern PHB_EXPR_FUNC hb_comp_ExprTable[];

#define HB_EXPR_USE( pSelf, iMessage )  \
        hb_comp_ExprTable[ (pSelf)->ExprType ]( (pSelf), (iMessage) )

typedef PHB_EXPR HB_EXPR_ACTION( PHB_EXPR pSelf, int iMessage );

#define HB_EXPR_PCODE0( action ) action( )
#define HB_EXPR_PCODE1( action, p1 ) action( (p1) )
#define HB_EXPR_PCODE2( action, p1, p2 ) action( (p1), (p2) )
#define HB_EXPR_PCODE3( action, p1, p2, p3 ) action( (p1), (p2), (p3) )
#define HB_EXPR_PCODE4( action, p1, p2, p3, p4 ) action( (p1), (p2), (p3), (p4) )
#define HB_EXPR_GENPCODE1( action, p1 ) action( (p1) )
#define HB_EXPR_GENPCODE2( action, p1, p2, p3 ) action( (p1), (p2), (p3) )
#define HB_EXPR_GENPCODE3( action, p1, p2, p3, p4 ) action( (p1), (p2), (p3), (p4) )
#define HB_EXPR_GENPCODE4( action, p1, p2, p3, p4, p5 ) action( (p1), (p2), (p3), (p4), (p5) )

/* pass NULL instead of macro structure pointer */
#define HB_MACRO_DECL void *pMacro
#define HB_MACRO_PARAM NULL
#define HB_MACRO_VARNAME pMacro
#endif

#define HB_EXPR_ISEQUAL_SYMBOLS( Exp1, Exp2 )  HB_EXPR_ISEQUAL_IDS( Exp1->value.asSymbol.szName, Exp2->value.asSymbol.szName )
#define HB_EXPR_ISBUILTIN_SYMBOL( Exp1, BuiltInName ) HB_EXPR_ISBUILTIN_ID( Exp1->value.asSymbol.szName, BuiltInName )

PHB_EXPR hb_compExprNew              ( int );
PHB_EXPR hb_compExprNewExtBlock      ( BYTE *, HB_SIZE );
PHB_EXPR hb_compExprNewEmpty         ( void );
PHB_EXPR hb_compExprNewNil           ( void );
PHB_EXPR hb_compExprNewDouble        ( double, BYTE, BYTE );
PHB_EXPR hb_compExprNewLong          ( HB_LONG );
PHB_EXPR hb_compExprNewString        ( char *, HB_SIZE, BOOL );
PHB_EXPR hb_compExprNewLogical       ( int );
PHB_EXPR hb_compExprNewDate          ( PHB_EXPR, PHB_EXPR, PHB_EXPR );
PHB_EXPR hb_compExprNewDateTime      ( PHB_EXPR, PHB_EXPR, PHB_EXPR, PHB_EXPR, PHB_EXPR, PHB_EXPR, int, int * );
PHB_EXPR hb_compExprNewDateTimeVal   ( long, long, USHORT );
PHB_EXPR hb_compExprNewSelf          ( void );
PHB_EXPR hb_compExprNewCodeBlock     ( void );
PHB_EXPR hb_compExprNewArray         ( PHB_EXPR );
PHB_EXPR hb_compExprNewVar           ( char * );
PHB_EXPR hb_compExprNewAliasVar      ( PHB_EXPR, PHB_EXPR );
PHB_EXPR hb_compExprNewAliasExpr     ( PHB_EXPR, PHB_EXPR );
PHB_EXPR hb_compExprNewMacro         ( PHB_EXPR, unsigned char, char * );
PHB_EXPR hb_compExprNewFunName       ( char * );
PHB_EXPR hb_compExprNewRTVar         ( char *, PHB_EXPR );
PHB_EXPR hb_compExprNewAlias         ( char * );
PHB_EXPR hb_compExprNewEQ            ( PHB_EXPR );
PHB_EXPR hb_compExprNewNE            ( PHB_EXPR );
PHB_EXPR hb_compExprNewLT            ( PHB_EXPR );
PHB_EXPR hb_compExprNewLE            ( PHB_EXPR );
PHB_EXPR hb_compExprNewGT            ( PHB_EXPR );
PHB_EXPR hb_compExprNewGE            ( PHB_EXPR );
PHB_EXPR hb_compExprNewIN            ( PHB_EXPR );
PHB_EXPR hb_compExprNewLike          ( PHB_EXPR );
PHB_EXPR hb_compExprNewMatch         ( PHB_EXPR );
PHB_EXPR hb_compExprNewPlus          ( PHB_EXPR );
PHB_EXPR hb_compExprNewMinus         ( PHB_EXPR );
PHB_EXPR hb_compExprNewMult          ( PHB_EXPR );
PHB_EXPR hb_compExprNewDiv           ( PHB_EXPR );
PHB_EXPR hb_compExprNewMod           ( PHB_EXPR );
PHB_EXPR hb_compExprNewPower         ( PHB_EXPR );
PHB_EXPR hb_compExprNewAssign        ( PHB_EXPR );
PHB_EXPR hb_compExprNewEqual         ( PHB_EXPR );
PHB_EXPR hb_compExprNewPlusEq        ( PHB_EXPR );
PHB_EXPR hb_compExprNewMinusEq       ( PHB_EXPR );
PHB_EXPR hb_compExprNewMultEq        ( PHB_EXPR );
PHB_EXPR hb_compExprNewDivEq         ( PHB_EXPR );
PHB_EXPR hb_compExprNewModEq         ( PHB_EXPR );
PHB_EXPR hb_compExprNewExpEq         ( PHB_EXPR );
PHB_EXPR hb_compExprNewPostInc       ( PHB_EXPR );
PHB_EXPR hb_compExprNewPostDec       ( PHB_EXPR );
PHB_EXPR hb_compExprNewPreInc        ( PHB_EXPR );
PHB_EXPR hb_compExprNewPreDec        ( PHB_EXPR );
PHB_EXPR hb_compExprNewAnd           ( PHB_EXPR );
PHB_EXPR hb_compExprNewOr            ( PHB_EXPR );
PHB_EXPR hb_compExprNewNot           ( PHB_EXPR );
PHB_EXPR hb_compExprNewBitAnd        ( PHB_EXPR );
PHB_EXPR hb_compExprNewBitOr         ( PHB_EXPR );
PHB_EXPR hb_compExprNewBitXOr        ( PHB_EXPR );
PHB_EXPR hb_compExprNewBitShiftR     ( PHB_EXPR );
PHB_EXPR hb_compExprNewBitShiftL     ( PHB_EXPR );
PHB_EXPR hb_compExprNewNegate        ( PHB_EXPR );
PHB_EXPR hb_compExprNewVarRef        ( char * );
PHB_EXPR hb_compExprNewMemVarRef     ( char * );
PHB_EXPR hb_compExprNewFunRef        ( char * );
PHB_EXPR hb_compExprNewCodeblockExpr ( PHB_EXPR, PHB_EXPR );
PHB_EXPR hb_compExprNewFunCallArg    ( PHB_EXPR, PHB_EXPR );
PHB_EXPR hb_compExprNewSend          ( PHB_EXPR, char * );
PHB_EXPR hb_compExprNewSendExp       ( PHB_EXPR, PHB_EXPR );
PHB_EXPR hb_compExprNewMethodCall    ( PHB_EXPR, PHB_EXPR );
PHB_EXPR hb_compExprNewWithSend      ( char * );
PHB_EXPR hb_compExprNewWithSendExp   ( PHB_EXPR );
PHB_EXPR hb_compExprNewWithMethodCall( PHB_EXPR, PHB_EXPR );
PHB_EXPR hb_compExprNewList          ( PHB_EXPR );
PHB_EXPR hb_compExprNewArgList       ( PHB_EXPR );
PHB_EXPR hb_compExprAddListExpr      ( PHB_EXPR, PHB_EXPR );
PHB_EXPR hb_compExprNewIIF           ( PHB_EXPR );
PHB_EXPR hb_compExprReduce           ( PHB_EXPR );
PHB_EXPR hb_compExprAssign           ( PHB_EXPR, PHB_EXPR );
PHB_EXPR hb_compExprEqual            ( PHB_EXPR, PHB_EXPR );
PHB_EXPR hb_compExprAssignStatic     ( PHB_EXPR, PHB_EXPR );
PHB_EXPR hb_compExprClone            ( PHB_EXPR pSrc );
HB_ULONG hb_compExprListLen          ( PHB_EXPR );
void     hb_compExprClear            ( PHB_EXPR );
char *   hb_compExprDescription      ( PHB_EXPR );
int      hb_compExprType             ( PHB_EXPR );

void     hb_compExprFree             ( PHB_EXPR, HB_MACRO_DECL );
void     hb_compExprErrorType        ( PHB_EXPR, HB_MACRO_DECL );
PHB_EXPR hb_compExprListStrip        ( PHB_EXPR, HB_MACRO_DECL );
PHB_EXPR hb_compExprListStripSingle  ( PHB_EXPR, HB_MACRO_DECL );
BOOL     hb_compExprCheckMacroVar    ( char * );
void     hb_compExprCBVarDel         ( PHB_CBVAR );
PHB_EXPR hb_compExprReducePlusStrings( PHB_EXPR, PHB_EXPR, HB_MACRO_DECL );

extern BOOL hb_compExprReduceUPPER( PHB_EXPR, HB_MACRO_DECL );

PHB_EXPR hb_compExprNewNamespaceFunName( char *, char * );
PHB_EXPR hb_compExprNewNamespaceFunRef ( char *, char * );

#ifdef HB_MACRO_SUPPORT

PHB_EXPR hb_compExprNewArrayAt  ( PHB_EXPR, PHB_EXPR, HB_MACRO_DECL );
PHB_EXPR hb_compExprSetOperand  ( PHB_EXPR, PHB_EXPR, HB_MACRO_DECL );
PHB_EXPR hb_compExprGenPop      ( PHB_EXPR, HB_MACRO_DECL );
PHB_EXPR hb_compExprGenPush     ( PHB_EXPR, HB_MACRO_DECL );
PHB_EXPR hb_compExprGenStatement( PHB_EXPR, HB_MACRO_DECL );
PHB_EXPR hb_compExprNewFunCall  ( PHB_EXPR, PHB_EXPR, HB_MACRO_DECL );
PHB_EXPR hb_compExprCBVarAdd    ( PHB_EXPR, char *, HB_MACRO_DECL );
void     hb_compExprDelete      ( PHB_EXPR, HB_MACRO_DECL );
PHB_EXPR hb_compExprSetGetBlock ( PHB_EXPR pExpr, HB_MACRO_DECL  );

#else

void     hb_compExprINIT( void );

PHB_EXPR hb_compExprNewArrayAt  ( PHB_EXPR, PHB_EXPR );
PHB_EXPR hb_compExprSetOperand  ( PHB_EXPR, PHB_EXPR );
PHB_EXPR hb_compExprGenPop      ( PHB_EXPR );
PHB_EXPR hb_compExprGenPush     ( PHB_EXPR );
PHB_EXPR hb_compExprGenStatement( PHB_EXPR );
PHB_EXPR hb_compExprNewFunCall  ( PHB_EXPR, PHB_EXPR );
PHB_EXPR hb_compExprCBVarAdd    ( PHB_EXPR, char *, BYTE );
void     hb_compExprDelete      ( PHB_EXPR );
PHB_EXPR hb_compExprSetGetBlock ( PHB_EXPR pExpr );

#endif

HB_EXTERN_END

#endif  /* HB_EXPROP_H_ */
