/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler Expression Optimizer
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

/* TOFIX: Split the code, since MSC8 can't compile it, even in Huge model. */

/* TODO:
 *    - Correct post- and pre- operations to correctly handle the following code
 *    a[ i++ ]++
 *    Notice: in current implementation (an in Clipper too) 'i++' is evaluated
 *    two times! This causes that the new value (after incrementation) is
 *    stored in next element of the array.
 */

#include <math.h>
#include "hbcomp.h"

/* memory allocation
 */
#define  HB_XGRAB( size )  hb_xgrab( (size) )
#define  HB_XFREE( pPtr )  hb_xfree( (void *)(pPtr) )

#include "hbexemem.h"

/* Table with operators precedence
 * NOTE:
 *    HB_ET_NIL is used for an ordinary values and post- operators
 *    HB_ET_NONE is used for invalid syntax, e.g. var := var1 += 2
 */
static BYTE s_PrecedTable[] = {
   HB_ET_NIL,                 /*   HB_ET_NONE = 0,    */
   HB_ET_NIL,                 /*   HB_ET_EXTBLOCK,    */
   HB_ET_NIL,                 /*   HB_ET_NIL,         */
   HB_ET_NIL,                 /*   HB_ET_NUMERIC,     */
   HB_ET_NIL,                 /*   HB_ET_DATE,        */
   HB_ET_NIL,                 /*   HB_ET_STRING,      */
   HB_ET_NIL,                 /*   HB_ET_CODEBLOCK,   */
   HB_ET_NIL,                 /*   HB_ET_LOGICAL,     */
   HB_ET_NIL,                 /*   HB_ET_SELF,        */
   HB_ET_NIL,                 /*   HB_ET_ARRAY,       */
   HB_ET_NIL,                 /*   HB_ET_VARREF,      */
   HB_ET_NIL,                 /*   HB_ET_MEMVARREF,   */
   HB_ET_NIL,                 /*   HB_ET_FUNREF,      */
   HB_ET_NIL,                 /*   HB_ET_IIF,         */
   HB_ET_NIL,                 /*   HB_ET_LIST,        */
   HB_ET_NIL,                 /*   HB_ET_ARGLIST,     */
   HB_ET_NIL,                 /*   HB_ET_ARRAYAT,     */
   HB_ET_NIL,                 /*   HB_ET_MACRO,       */
   HB_ET_NIL,                 /*   HB_ET_FUNCALL,     */
   HB_ET_NIL,                 /*   HB_ET_ALIASVAR,    */
   HB_ET_NIL,                 /*   HB_ET_ALIASEXPR,   */
   HB_ET_NIL,                 /*   HB_ET_SEND,        */
   HB_ET_NIL,                 /*   HB_ET_WITHSEND,    */
   HB_ET_NIL,                 /*   HB_ET_FUNNAME,     */
   HB_ET_NIL,                 /*   HB_ET_ALIAS,       */
   HB_ET_NIL,                 /*   HB_ET_RTVARIABLE,  */
   HB_ET_NIL,                 /*   HB_ET_VARIABLE,    */
   HB_ET_NIL,                 /*   HB_EO_POSTINC,     post-operators */
   HB_ET_NIL,                 /*   HB_EO_POSTDEC,     */
   HB_ET_NONE,                /*   HB_EO_ASSIGN,      assigments */
   HB_ET_NONE,                /*   HB_EO_PLUSEQ,      Invalid syntax */
   HB_ET_NONE,                /*   HB_EO_MINUSEQ,     */
   HB_ET_NONE,                /*   HB_EO_MULTEQ,      */
   HB_ET_NONE,                /*   HB_EO_DIVEQ,       */
   HB_ET_NONE,                /*   HB_EO_MODEQ,       */
   HB_ET_NONE,                /*   HB_EO_EXPEQ,       */
   HB_EO_OR,                  /*   HB_EO_OR,          logical operators */
   HB_EO_AND,                 /*   HB_EO_AND,         */
   HB_ET_NIL,                 /*   HB_EO_NOT,         */
   HB_EO_EQUAL,               /*   HB_EO_EQUAL,       relational operators */
   HB_EO_EQUAL,               /*   HB_EO_EQ,          */
   HB_EO_LT,                  /*   HB_EO_LT,          */
   HB_EO_LT,                  /*   HB_EO_GT,          */
   HB_EO_LT,                  /*   HB_EO_LE,          */
   HB_EO_LT,                  /*   HB_EO_GE,          */
   HB_EO_EQUAL,               /*   HB_EO_NE,          */
   HB_EO_IN,                  /*   HB_EO_IN,          */
   HB_EO_LT,                  /*   HB_EO_MATCH,       */
   HB_EO_LT,                  /*   HB_EO_LIKE,        */
   HB_EO_PLUS,                /*   HB_EO_PLUS,        addition */
   HB_EO_PLUS,                /*   HB_EO_MINUS,       */
   HB_EO_MULT,                /*   HB_EO_MULT,        multiple */
   HB_EO_MULT,                /*   HB_EO_DIV,         */
   HB_EO_MULT,                /*   HB_EO_MOD,         */
   HB_EO_POWER,               /*   HB_EO_POWER,       */
   HB_EO_BITAND,              /*   HB_EO_BITAND       */
   HB_EO_BITOR,               /*   HB_EO_BITOR        */
   HB_EO_BITXOR,              /*   HB_EO_BITXOR       */
   HB_EO_BITSHIFTR,           /*   HB_EO_BITSHIFTR    */
   HB_EO_BITSHIFTL,           /*   HB_EO_BITSHIFTL    */
   HB_ET_NIL,                 /*   HB_EO_NEGATE,      sign operator */
   HB_ET_NIL,                 /*   HB_EO_PREINC,      */
   HB_ET_NIL                  /*   HB_EO_PREDEC,      pre-operators */
};

/* ************************************************************************ */

#if defined( HB_MACRO_SUPPORT )

#else

HB_COMP_IDS hb_compExpr_IDs;

/* Initialize the optimizer
 */
void hb_compExprINIT( void )
{
   hb_compExpr_IDs.__INITLINES__   = hb_compIdentifierNew( "<_INITLINES]", TRUE );
   hb_compExpr_IDs.__INITSTATICS__ = hb_compIdentifierNew( "(_INITSTATICS)", TRUE );
   hb_compExpr_IDs.__CLSSETMODULE  = hb_compIdentifierNew( "__CLSSETMODULE", TRUE );
   hb_compExpr_IDs.__DBGENTRY      = hb_compIdentifierNew( "__DBGENTRY", TRUE );
   hb_compExpr_IDs.__DBLIST        = hb_compIdentifierNew( "__DBLIST", TRUE );
   hb_compExpr_IDs.__GET           = hb_compIdentifierNew( "__GET", TRUE );
   hb_compExpr_IDs.__GETA          = hb_compIdentifierNew( "__GETA", TRUE );
   hb_compExpr_IDs.__MVPRIVATE     = hb_compIdentifierNew( "__MVPRIVATE", TRUE );
   hb_compExpr_IDs.__MVPUBLIC      = hb_compIdentifierNew( "__MVPUBLIC", TRUE );
   hb_compExpr_IDs._1              = hb_compIdentifierNew( "_1", TRUE );
   hb_compExpr_IDs.ARRAY           = hb_compIdentifierNew( "ARRAY", TRUE );
   hb_compExpr_IDs.AT              = hb_compIdentifierNew( "AT", TRUE );
   hb_compExpr_IDs.ATAIL           = hb_compIdentifierNew( "ATAIL", TRUE );
   hb_compExpr_IDs.BREAK_          = hb_compIdentifierNew( "BREAK", TRUE );
   hb_compExpr_IDs.CHR             = hb_compIdentifierNew( "CHR", TRUE );
   hb_compExpr_IDs.CTOD            = hb_compIdentifierNew( "CTOD", TRUE );
   hb_compExpr_IDs.EVAL            = hb_compIdentifierNew( "EVAL", TRUE );
   hb_compExpr_IDs.FIELD_          = hb_compIdentifierNew( "FIELD", TRUE );
   hb_compExpr_IDs.GLOBAL_         = hb_compIdentifierNew( "GLOBAL", TRUE );
   hb_compExpr_IDs.HASH            = hb_compIdentifierNew( "HASH", TRUE );
   hb_compExpr_IDs.HB_ENUMINDEX    = hb_compIdentifierNew( "HB_ENUMINDEX", TRUE );
   hb_compExpr_IDs.HB_QWITH        = hb_compIdentifierNew( "HB_QWITH", TRUE );
   hb_compExpr_IDs.HB_SETWITH      = hb_compIdentifierNew( "HB_SETWITH", TRUE );
   hb_compExpr_IDs.I18N            = hb_compIdentifierNew( "I18N", TRUE );
   hb_compExpr_IDs.LEFT            = hb_compIdentifierNew( "LEFT", TRUE );
   hb_compExpr_IDs.LEN             = hb_compIdentifierNew( "LEN", TRUE );
   hb_compExpr_IDs.RIGHT           = hb_compIdentifierNew( "RIGHT", TRUE );
   hb_compExpr_IDs.STOD            = hb_compIdentifierNew( "STOD", TRUE );
   hb_compExpr_IDs.STR             = hb_compIdentifierNew( "STR", TRUE );
   hb_compExpr_IDs.SUBSTR          = hb_compIdentifierNew( "SUBSTR", TRUE );
   hb_compExpr_IDs.TYPE            = hb_compIdentifierNew( "TYPE", TRUE );
   hb_compExpr_IDs.UPPER           = hb_compIdentifierNew( "UPPER", TRUE );
   hb_compExpr_IDs.WHILE_          = hb_compIdentifierNew( "WHILE", TRUE );
}

#endif

/* Delete all components and delete self
 */
#if defined( HB_MACRO_SUPPORT )
void hb_compExprDelete( PHB_EXPR pExpr, HB_MACRO_DECL )
#else
void hb_compExprDelete( PHB_EXPR pExpr )
#endif
{
   HB_TRACE(HB_TR_DEBUG, ("hb_compExprDelete()"));
   if( --pExpr->Counter == 0 )
   {
      HB_EXPR_USE( pExpr, HB_EA_DELETE );

#ifndef HB_MACRO_SUPPORT
      if( hb_comp_exprs )
      {
         PHB_EXPR_LIST pExpItm = ( PHB_EXPR_LIST ) pExpr;

         if( hb_comp_exprs == pExpItm )
            hb_comp_exprs = pExpItm->pNext;
         else
            pExpItm->pPrev->pNext = pExpItm->pNext;
         if( pExpItm->pNext )
            pExpItm->pNext->pPrev = pExpItm->pPrev;

         HB_XFREE( pExpr );
      }
      else
         pExpr->ExprType = HB_ET_NONE;
#else
      HB_XFREE( pExpr );
#endif
   }
}

/* Delete all components and delete self
 */
void hb_compExprFree( PHB_EXPR pExpr, HB_MACRO_DECL )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_compExprFree()"));

#ifdef HB_MACRO_SUPPORT
   hb_compExprDelete( pExpr, HB_MACRO_VARNAME );
#else
   HB_SYMBOL_UNUSED( HB_MACRO_VARNAME );
   hb_compExprDelete( pExpr );
#endif
}

void hb_compExprErrorType( PHB_EXPR pExpr, HB_MACRO_DECL )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_compExprErrorType()"));
   hb_compErrorType( pExpr );
   HB_SYMBOL_UNUSED( pExpr );
   HB_SYMBOL_UNUSED( HB_MACRO_VARNAME );
}

/* Create a new declaration for codeblock local variable
 */
static PHB_CBVAR hb_compExprCBVarNew( char * szVarName, BYTE bType )
{
   PHB_CBVAR pVar;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprCBVarNew(%s)", szVarName));

   pVar = ( PHB_CBVAR ) HB_XGRAB( sizeof( HB_CBVAR ) );

   pVar->szName = szVarName;
   pVar->bType  = bType;
   pVar->pNext  = NULL;

   return pVar;
}

/* Add a new local variable declaration
 */
#ifdef HB_MACRO_SUPPORT
PHB_EXPR hb_compExprCBVarAdd( PHB_EXPR pCB, char * szVarName, HB_MACRO_DECL )
#else
PHB_EXPR hb_compExprCBVarAdd( PHB_EXPR pCB, char * szVarName, BYTE bType )
#endif
{
   PHB_CBVAR pVar;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprCBVarAdd(%s)", szVarName));

   if( pCB->value.asList.pIndex )
   {
      /* add it to the end of the list
      */
      pVar = ( PHB_CBVAR ) pCB->value.asList.pIndex;
      while( pVar )
      {
         if( pVar->szName && szVarName && HB_EXPR_ISEQUAL_IDS( szVarName, pVar->szName ) )
            hb_compErrorDuplVar( szVarName );

         if( pVar->pNext )
            pVar = pVar->pNext;
         else
         {
#ifdef HB_MACRO_SUPPORT
            pVar->pNext = hb_compExprCBVarNew( szVarName, ' ' );
#else
            pVar->pNext = hb_compExprCBVarNew( szVarName, bType );
#endif
            pVar = NULL;
         }
      }
   }
   else
#ifdef HB_MACRO_SUPPORT
      pCB->value.asList.pIndex = ( PHB_EXPR ) hb_compExprCBVarNew( szVarName, ' ' );
#else
      pCB->value.asList.pIndex = ( PHB_EXPR ) hb_compExprCBVarNew( szVarName, bType );
#endif

   return pCB;
}

/* Create a new IIF() expression or set arguments
 *
 * pIIF is a list of three expressions
 */
PHB_EXPR hb_compExprNewIIF( PHB_EXPR pExpr )
{
#ifndef HB_MACRO_SUPPORT
   PHB_EXPR pTmp;

   pExpr->ExprType = HB_ET_IIF;

   pTmp = pExpr->value.asList.pExprList;  /* get first expression */

   if( pTmp->ExprType == HB_ET_NONE )
      /* there is no conditional expression e.g. IIF( , true, false )
       */
      hb_compErrorSyntax( pExpr );
#else
   pExpr->ExprType = HB_ET_IIF;
#endif

   return pExpr;
}

/* Create function call
 */
#ifdef HB_MACRO_SUPPORT
PHB_EXPR hb_compExprNewFunCall( PHB_EXPR pName, PHB_EXPR pParms, HB_MACRO_DECL )
#else
PHB_EXPR hb_compExprNewFunCall( PHB_EXPR pName, PHB_EXPR pParms )
#endif
{
   PHB_EXPR pExpr;

   if( pName->ExprType == HB_ET_FUNNAME )
   {
      /* The name of a function is specified at compile time
       * e.g. MyFunc()
       *
       * NOTE:  'pName' can be a macro expression that will be resolved
       * at runtime - in this case pName is an expression of HB_ET_MACRO type
       * e.g. &MyVar()
       */

      HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewFunCall(%s)", pName->value.asSymbol.szName));
   }

   if( pName->ExprType == HB_ET_MACRO )
   {
      /* Signal that macro compiler have to generate a pcode that will
       * return function name as symbol instead of usual value
       */
      pName->value.asMacro.SubType = HB_ET_MACRO_SYMBOL;

      HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewFunCall(&)"));
   }

#ifdef HB_MACRO_SUPPORT
      HB_SYMBOL_UNUSED( HB_MACRO_VARNAME );

      if( pName->ExprType == HB_ET_VARIABLE )
      {
         /* &DotedMacro.More()
          * MACROTEXT is compiled into hb_compExprNewVar()
          * so we must reset to correct context
          */
         pName->ExprType = HB_ET_FUNNAME;
      }
#endif

   pExpr = hb_compExprNew( HB_ET_FUNCALL );
   pExpr->value.asFunCall.pParms = pParms;
   pExpr->value.asFunCall.pFunName = pName;

   if( hb_compExprListLen( pParms ) > HB_VAR_PARAM_FLAG )
   {
#ifdef HB_MACRO_SUPPORT
        hb_compErrorSyntax( pExpr );
#else
        if( pName->ExprType == HB_ET_FUNNAME )
        {
           if( ! HB_EXPR_ISBUILTIN_ID( pName->value.asSymbol.szName, HASH ) )
              hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_TOOMANY_ARGS, pName->value.asSymbol.szName, NULL );
        }
        else
           hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_TOOMANY_ARGS, "&", NULL );
#endif
   }

   return pExpr;
}

/* Create a new symbol used in function calls
 */
PHB_EXPR hb_compExprNewFunName( char * szName )
{
   PHB_EXPR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewFunName(%s)", szName));

   pExpr = hb_compExprNew( HB_ET_FUNNAME );
   pExpr->value.asSymbol.szName = szName;
   pExpr->value.asSymbol.szNamespace = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewNamespaceFunRef( char * szNamespace, char * szFunName )
{
   PHB_EXPR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewFunRef(%s)", szFunName));

   pExpr = hb_compExprNew( HB_ET_FUNREF );
   pExpr->value.asSymbol.szName = szFunName;
   pExpr->value.asSymbol.szNamespace = szNamespace;
   pExpr->ValType = HB_EV_FUNREF;
   return pExpr;
}

PHB_EXPR hb_compExprNewNamespaceFunName( char * szNamespace, char * szName )
{
   PHB_EXPR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewNamespaceFunName(%s, %s)", szNamespace, szName));

   pExpr = hb_compExprNew( HB_ET_FUNNAME );
   pExpr->value.asSymbol.szName = szName;
   pExpr->value.asSymbol.szNamespace = szNamespace;
   return pExpr;
}

/* Creates new array access expression
 *    pArray[ pIndex ]
 * NOTE: In case of multiple indexes it is called recursively
 *    array[ idx1, idx2 ] => ( array[ idx1 ] )[ idx2 ]
 */
#ifdef HB_MACRO_SUPPORT
PHB_EXPR hb_compExprNewArrayAt( PHB_EXPR pArray, PHB_EXPR pIndex, HB_MACRO_DECL )
#else
PHB_EXPR hb_compExprNewArrayAt( PHB_EXPR pArray, PHB_EXPR pIndex )
#endif
{
   PHB_EXPR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewArrayAt()"));

   pExpr = hb_compExprNew( HB_ET_ARRAYAT );

   /* Check if this expression can be indexed */
#ifdef HB_C52_STRICT
   HB_EXPR_USE( pArray, HB_EA_ARRAY_AT );
#else
     /* xHarbour supports type overloading.*/
#endif

   /* Check if this expression can be an index */
#ifdef HB_C52_STRICT
   HB_EXPR_USE( pIndex, HB_EA_ARRAY_INDEX );
#else
#ifdef HB_MACRO_SUPPORT
   HB_SYMBOL_UNUSED( HB_MACRO_VARNAME );
#endif
  /* xHarbour supports type overloading.*/
#endif

   pExpr->value.asList.pExprList = pArray;
   pExpr->value.asList.pIndex = pIndex;
   pExpr->value.asList.bByRef = FALSE;
   pExpr->value.asList.PopOp  = HB_P_NOOP;

   return pExpr;
}

/* ************************************************************************* */

#ifndef HB_MACRO_SUPPORT

BOOL hb_compCanUseAsConstant( PHB_EXPR pFunc, PHB_EXPR pStaticVar );

static void hb_compExprCheckStaticInitializers( PHB_EXPR pStaticVar, PHB_EXPR pRightExpr )
{
   PHB_EXPR pElem;
   PHB_EXPR pNext;
   PHB_EXPR * pPrev;

   if( pRightExpr == NULL )
      return;

   pElem = pRightExpr->value.asList.pExprList;
   pPrev = &pRightExpr->value.asList.pExprList;

   while( pElem )
   {
      /* NOTE: During reduction the expression can be replaced by the
       *    new one - this will break the linked list of expressions.
       * (classical case of replacing an item in a linked list)
       */
      pNext = pElem->pNext; /* store next expression in case the current  will be reduced */
      pElem = hb_compExprListStrip( HB_EXPR_USE( pElem, HB_EA_REDUCE ), HB_MACRO_PARAM );

      if( pElem->ExprType > HB_ET_FUNREF )
      {
         if( ! hb_compCanUseAsConstant( pElem, pStaticVar ) )
            hb_compErrorStatic( pStaticVar->value.asSymbol.szName, pElem );
      }

      *pPrev = pElem;   /* store a new expression into the previous one */
      pElem->pNext = pNext;  /* restore the link to next expression */
      pPrev  = &pElem->pNext;
      pElem  = pNext;
   }
}

/*
 TODO: There are other valid initializers that are currently disallowed, f.e.:
       STATIC x := {1}[1]
 */
BOOL hb_compCanUseAsConstant( PHB_EXPR pInit, PHB_EXPR pStaticVar )
{
   if( pInit->ExprType != HB_ET_FUNCALL )
      return FALSE;

   if( pInit->value.asFunCall.pFunName->value.asSymbol.szName == hb_compExpr_IDs.ARRAY ||
       pInit->value.asFunCall.pFunName->value.asSymbol.szName == hb_compExpr_IDs.HASH )
   {
      /* We must validate the arguments! */
      hb_compExprCheckStaticInitializers( pStaticVar, pInit->value.asFunCall.pParms );
      return TRUE;
   }

   return FALSE;
}

/* It initializes static variable.
 *    It is called in the following context:
 * STATIC sVar := expression
 *
 * pLeftExpr - is a variable name
 * pRightExpr - can be an expression of any type
 */
PHB_EXPR hb_compExprAssignStatic( PHB_EXPR pLeftExpr, PHB_EXPR pRightExpr )
{
   PHB_EXPR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprAssignStatic()"));

   pExpr = hb_compExprNew( HB_EO_ASSIGN );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   /* Try to reduce the assigned value */
   pRightExpr = hb_compExprListStrip( HB_EXPR_USE( pRightExpr, HB_EA_REDUCE ), HB_MACRO_PARAM );
   pExpr->value.asOperator.pRight = pRightExpr;

   if( pRightExpr->ExprType == HB_ET_ARGLIST )
   {
       /* HB_ET_ARGLIST is used in case of STATIC var[dim1, dim2, dimN]
        * was used - we have to check if all array dimensions are
        * constant values
        */
      hb_compExprCheckStaticInitializers( pLeftExpr, pRightExpr );
   }
   else if( pRightExpr->ExprType > HB_ET_FUNREF )
   {
      if( ! hb_compCanUseAsConstant( pRightExpr, pLeftExpr ) )
      {
         /* Illegal initializer for static variable (not a constant value)
          */
          hb_compErrorStatic( pLeftExpr->value.asSymbol.szName, pRightExpr );
      }
   }
   else if( pRightExpr->ExprType == HB_ET_ARRAY )
   {
      /* { elem1, elem2, elemN } was used as initializer
       * Scan an array for illegal initializers.
       * An array item have to be a const value too.
       */
      hb_compExprCheckStaticInitializers( pLeftExpr, pRightExpr );
   }

   return pExpr;
}
#endif


/* Sets the argument of an operation found previously
 */
#ifdef HB_MACRO_SUPPORT
PHB_EXPR hb_compExprSetOperand( PHB_EXPR pExpr, PHB_EXPR pItem, HB_MACRO_DECL )
#else
PHB_EXPR hb_compExprSetOperand( PHB_EXPR pExpr, PHB_EXPR pItem )
#endif
{
   BYTE ucRight;

   ucRight = s_PrecedTable[ pItem->ExprType ];

   if( ucRight == HB_ET_NIL )
   {
      /* the right side of an operator is an ordinary value
       * e.g. a := 1
       */
      pExpr->value.asOperator.pRight = pItem;
   }
   else if( ucRight == HB_ET_NONE )
   {
      /* the right side of an operator is an invalid expression
       * e.g.
       *    a := 1 + b:=2
       *    a := 1 + b += 2
       */

      if( pExpr->ExprType >= HB_EO_PLUSEQ && pExpr->ExprType <= HB_EO_EXPEQ )
      {
      }
      else
         hb_compErrorSyntax( pItem );

      pExpr->value.asOperator.pRight = pItem; /* set it anyway */
   }
   else
   {
      /* the right side of an operator is an expression with other operator
       * e.g. a := 2 + b * 3
       *   We have to set the proper order of evaluation using
       * precedence rules
       */
      BYTE ucLeft = s_PrecedTable[ pExpr->ExprType ];

      if( hb_comp_bShortCuts && ucLeft == ucRight && ( ucLeft == HB_EO_OR || ucLeft == HB_EO_AND ) )
         pExpr->value.asOperator.pRight = pItem;
      else if( ucLeft >= ucRight )
      {
         /* Left operator has the same or lower precedence then the right one
          * e.g.  a * b + c
          *    pItem -> b + c   -> L=b  R=c  O=+
          *    pExpr -> a *     -> l=a  r=   o=*
          *
          *    -> (a * b) + c    -> Lelf=(a * b)  Right=c  Oper=+
          *             Left  := l (o) L
          *             Right := R
          *             Oper  := O
          */

#ifdef HB_MACRO_SUPPORT
         pItem->value.asOperator.pLeft = hb_compExprSetOperand( pExpr, pItem->value.asOperator.pLeft, HB_MACRO_PARAM );
#else
         pItem->value.asOperator.pLeft = hb_compExprSetOperand( pExpr, pItem->value.asOperator.pLeft );
#endif
         pExpr = pItem;
      }
      else
      {
         /* Left operator has a lower precedence then the right one
          * e.g.  a + b * c
          *    pItem -> b * c    -> L=b  R=c  O=*
          *    pExpr -> a +      -> l=a  r=   o=+
          *
          *    -> a + (b * c)    -> Left=a  Right=(b * c)  Oper=+
          *             Left  := l
          *             Right := L (O) R  := pItem
          *             Oper  := o
          */
         pExpr->value.asOperator.pRight = pItem;
      }
   }

   return pExpr;
}

/* ************************************************************************* */

/* Generates pcode for inline expression used as a statement
 * NOTE: It doesn't not leave any value on the eval stack
 */
#ifdef HB_MACRO_SUPPORT
PHB_EXPR hb_compExprGenStatement( PHB_EXPR pExpr, HB_MACRO_DECL )
#else
PHB_EXPR hb_compExprGenStatement( PHB_EXPR pExpr )
#endif
{
   HB_TRACE(HB_TR_DEBUG, ("hb_compExprGenStatement(%i)", pExpr->ExprType));

   pExpr = HB_EXPR_USE( pExpr, HB_EA_REDUCE );

   HB_EXPR_USE( pExpr, HB_EA_STATEMENT );

   return pExpr;
}

/* Generates pcode to push an expressions
 * NOTE: It pushes a value on the stack and leaves this value on the stack
 */
#ifdef HB_MACRO_SUPPORT
PHB_EXPR hb_compExprGenPush( PHB_EXPR pExpr, HB_MACRO_DECL )
#else
PHB_EXPR hb_compExprGenPush( PHB_EXPR pExpr )
#endif
{
   HB_TRACE(HB_TR_DEBUG, ("hb_compExprGenPush(%i)", pExpr->ExprType));

   pExpr = HB_EXPR_USE( pExpr, HB_EA_REDUCE );
   HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
   return pExpr;
}

/* Generates pcode to pop an expressions
 */
#ifdef HB_MACRO_SUPPORT
PHB_EXPR hb_compExprGenPop( PHB_EXPR pExpr, HB_MACRO_DECL )
#else
PHB_EXPR hb_compExprGenPop( PHB_EXPR pExpr )
#endif
{
   HB_TRACE(HB_TR_DEBUG, ("hb_compExprGenPop(%i)", pExpr->ExprType));

   return HB_EXPR_USE( pExpr, HB_EA_POP_PCODE );
}

/* ************************************************************************* */

/* NOTE: This deletes all linked variables
 */
void hb_compExprCBVarDel( PHB_CBVAR pVars )
{
   PHB_CBVAR pDel;

   while( pVars )
   {
      pDel  = pVars;
      pVars = pVars->pNext;
#ifdef HB_MACRO_SUPPORT
      HB_XFREE( pDel->szName );
#endif
      HB_XFREE( pDel );
   }
}

#ifndef HB_MACRO_SUPPORT
PHB_EXPR hb_compExprReduce( PHB_EXPR pExpr )
{
  return hb_compExprListStrip( HB_EXPR_USE( pExpr, HB_EA_REDUCE ), NULL );
}
#endif
