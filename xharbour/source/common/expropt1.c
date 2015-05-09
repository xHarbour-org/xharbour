/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler Expression Optimizer - common expressions
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

/* TODO:
 *    - Correct post- and pre- operations to correctly handle the following code
 *    a[ i++ ]++
 *    Notice: in current implementation (an in Clipper too) 'i++' is evaluated
 *    two times! This causes that the new value (after incrementation) is
 *    stored in next element of the array.
 */

/* NOTE: This must be the first definition
 *    This is a common code shared by macro and standalone compiler
 */
#define  HB_MACRO_SUPPORT

#include <math.h>
#include "hbmacro.h"
#include "hbcomp.h"
#include "hbdate.h"

/* memory allocation
 */
#define  HB_XGRAB( size )  hb_xgrab( ( size ) )
#define  HB_XFREE( pPtr )  hb_xfree( ( void * ) ( pPtr ) )

#include "hbexemem.h"

#if defined( __HB_COMPILER__ )
   #include "hbcomp.h"
#endif

static char * s_OperTable[] = {
   "",               /* HB_ET_NONE      */
   "ExtCodeblock",   /* HB_ET_EXTBLOCK  */
   "NIL",            /* HB_ET_NIL       */
   "Numeric",        /* HB_ET_NUMERIC   */
   "Date",           /* HB_ET_DATE      */
   "String",         /* HB_ET_STRING    */
   "Codeblock",      /* HB_ET_CODEBLOCK */
   "Logical",        /* HB_ET_LOGICAL   */
   "SELF",           /* HB_ET_SELF      */
   "Array",          /* HB_ET_ARRAY     */
   "@",              /* HB_ET_VARREF    */
   "@M->",           /* HB_ET_MEMVARREF */
   "@Func()",        /* HB_ET_FUNREF    */
   "IIF",            /* HB_ET_IIF       */
   ",",              /* HB_ET_LIST      */
   ",",              /* HB_ET_ARGLIST   */
   "Array[]",        /* HB_ET_ARRAYAT   */
   "&",              /* HB_ET_MACRO     */
   "()",             /* HB_ET_FUNCALL   */
   "->",             /* HB_ET_ALIASVAR  */
   "()->",           /* HB_ET_ALIASEXPR */
   ":",              /* HB_ET_SEND      */
   "WITH:",          /* HB_ET_WITHSEND  */
   "",               /* HB_ET_FUNNAME   */
   "",               /* HB_ET_ALIAS     */
   "",               /* HB_ET_RTVAR     */
   "",               /* HB_ET_VARIABLE  */
   "++",             /* HB_EO_POSTINC   */
   "--",             /* HB_EO_POSTDEC   */
   ":=",             /* HB_EO_ASSIGN    */
   "+=",             /* HB_EO_PLUSEQ    */
   "-=",             /* HB_EO_MINUSEQ   */
   "*=",             /* HB_EO_MULTEQ    */
   "/=",             /* HB_EO_DIVEQ     */
   "%=",             /* HB_EO_MODEQ     */
   "^=",             /* HB_EO_EXPEQ     */
   ".OR.",           /* HB_EO_OR        */
   ".AND.",          /* HB_EO_AND       */
   ".NOT.",          /* HB_EO_NOT       */
   "=",              /* HB_EO_EQUAL     */
   "==",             /* HB_EO_EQ        */
   "<",              /* HB_EO_LT        */
   ">",              /* HB_EO_GT        */
   "<=",             /* HB_EO_LE        */
   ">=",             /* HB_EO_GE        */
   "!=",             /* HB_EO_NE        */
   "$",              /* HB_EO_IN        */
   "LIKE",           /* HB_EO_LIKE      */
   "MATCH",          /* HB_EO_MATCH     */
   "+",              /* HB_EO_PLUS      */
   "-",              /* HB_EO_MINUS     */
   "*",              /* HB_EO_MULT      */
   "/",              /* HB_EO_DIV       */
   "%",              /* HB_EO_MOD       */
   "^",              /* HB_EO_POWER     */
   "&",              /* HB_EO_BITAND    */
   "|",              /* HB_EO_BITOR     */
   "^^",             /* HB_EO_BITXOR    */
   ">>",             /* HB_EO_BITSHIFTR */
   "<<",             /* HB_EO_BITSHIFTL */
   "-",              /* HB_EO_NEGATE    */
   "++",             /* HB_EO_PREINC    */
   "--"              /* HB_EO_PREDEC    */
};

/* ************************************************************************* */

PHB_EXPR hb_compExprNew( int iType )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNew(%i)", iType ) );

#if defined( __HB_COMPILER__ )
   {
      PHB_EXPR_LIST pExprItm = ( PHB_EXPR_LIST ) hb_xgrab( sizeof( HB_EXPR_LIST ) );

      if( hb_comp_exprs )
         hb_comp_exprs->pPrev = pExprItm;
      pExprItm->pNext = hb_comp_exprs;
      pExprItm->pPrev = NULL;
      hb_comp_exprs   = pExprItm;

      pExpr           = &pExprItm->Expr;
   }
#else
   pExpr             = ( PHB_EXPR ) HB_XGRAB( sizeof( HB_EXPR ) );
#endif

   pExpr->ExprType   = ( unsigned char ) iType;
   pExpr->pNext      = NULL;
   pExpr->ValType    = HB_EV_UNKNOWN;
   pExpr->Counter    = 1;

   return pExpr;
}

/* Delete self - all components will be deleted somewhere else
 */
void hb_compExprClear( PHB_EXPR pExpr )
{
   if( --pExpr->Counter == 0 )
   {

#if defined( __HB_COMPILER__ )
      {
         PHB_EXPR_LIST pExpItm = ( PHB_EXPR_LIST ) pExpr;

         if( hb_comp_exprs == pExpItm )
            hb_comp_exprs = pExpItm->pNext;
         else
            pExpItm->pPrev->pNext = pExpItm->pNext;
         if( pExpItm->pNext )
            pExpItm->pNext->pPrev = pExpItm->pPrev;
      }
#endif

      HB_XFREE( pExpr );
   }
}

/* Increase a reference counter (this allows to share the same expression
 * in more then one context)
 */
PHB_EXPR hb_compExprClone( PHB_EXPR pSrc )
{
   pSrc->Counter++;

   return pSrc;
}

char * hb_compExprDescription( PHB_EXPR pExpr )
{
   if( pExpr )
      return s_OperTable[ pExpr->ExprType ];
   else
      return s_OperTable[ 0 ];
}

int hb_compExprType( PHB_EXPR pExpr )
{
   return ( int ) pExpr->ExprType;
}

/* ************************************************************************* */

PHB_EXPR hb_compExprNewExtBlock( BYTE * pCode, HB_SIZE ulLen )
{
   PHB_EXPR pExpr;

   pExpr                         = hb_compExprNew( HB_ET_EXTBLOCK );
   pExpr->value.asExtBlock.pCode = pCode;
   pExpr->value.asExtBlock.ulLen = ulLen;

   return pExpr;
}

PHB_EXPR hb_compExprNewEmpty( void )
{
   return hb_compExprNew( HB_ET_NONE );
}

PHB_EXPR hb_compExprNewDouble( double dValue, BYTE ucWidth, BYTE ucDec )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewDouble(%f, %i)", dValue, ucDec ) );

   pExpr                      = hb_compExprNew( HB_ET_NUMERIC );
   pExpr->value.asNum.dVal    = dValue;
   pExpr->value.asNum.bWidth  = ucWidth;
   pExpr->value.asNum.bDec    = ucDec;
   pExpr->value.asNum.NumType = HB_ET_DOUBLE;
   pExpr->ValType             = HB_EV_NUMERIC;

   return pExpr;
}

PHB_EXPR hb_compExprNewLong( HB_LONG lValue )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewLong(%" PFHL "i)", lValue ) );

   pExpr                      = hb_compExprNew( HB_ET_NUMERIC );
   pExpr->value.asNum.lVal    = lValue;
   pExpr->value.asNum.bDec    = 0;
   pExpr->value.asNum.NumType = HB_ET_LONG;
   pExpr->ValType             = HB_EV_NUMERIC;

   return pExpr;
}

PHB_EXPR hb_compExprNewDate( PHB_EXPR pYear, PHB_EXPR pMonth, PHB_EXPR pDate )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewDate()" ) );

   pExpr                      = hb_compExprNew( HB_ET_DATE );
   pExpr->value.asDate.date   = hb_dateEncode( ( int ) pYear->value.asNum.lVal, ( int ) pMonth->value.asNum.lVal, ( int ) pDate->value.asNum.lVal );
   pExpr->value.asDate.time   = 0;
   pExpr->value.asDate.type   = HB_ET_DDATE;
   pExpr->ValType             = HB_EV_DATE;

   return pExpr;
}

PHB_EXPR hb_compExprNewDateTime( PHB_EXPR pYear, PHB_EXPR pMonth, PHB_EXPR pDate, PHB_EXPR pHour, PHB_EXPR pMinute, PHB_EXPR pSeconds, int iAmPm, int * piOk )
{
   PHB_EXPR pExpr;

   pExpr = hb_compExprNew( HB_ET_DATE );

   if( pYear )
   {
      if( pSeconds )
      {
         if( pSeconds->value.asNum.NumType == HB_ET_DOUBLE )
         {
            hb_comp_datetimeEncode( &pExpr->value.asDate.date, &pExpr->value.asDate.time,
                                    ( int ) pYear->value.asNum.lVal, ( int ) pMonth->value.asNum.lVal, ( int ) pDate->value.asNum.lVal,
                                    ( int ) pHour->value.asNum.lVal, ( int ) pMinute->value.asNum.lVal, pSeconds->value.asNum.dVal, iAmPm, piOk );
         }
         else
         {
            hb_comp_datetimeEncode( &pExpr->value.asDate.date, &pExpr->value.asDate.time,
                                    ( int ) pYear->value.asNum.lVal, ( int ) pMonth->value.asNum.lVal, ( int ) pDate->value.asNum.lVal,
                                    ( int ) pHour->value.asNum.lVal, ( int ) pMinute->value.asNum.lVal, ( double ) pSeconds->value.asNum.lVal, iAmPm, piOk );
         }
      }
      else
      {
         hb_comp_datetimeEncode( &pExpr->value.asDate.date, &pExpr->value.asDate.time,
                                 ( int ) pYear->value.asNum.lVal, ( int ) pMonth->value.asNum.lVal, ( int ) pDate->value.asNum.lVal,
                                 ( int ) pHour->value.asNum.lVal, ( int ) pMinute->value.asNum.lVal, ( double ) 0, iAmPm, piOk );
      }
   }
   else
   {
      if( pSeconds )
      {
         if( pSeconds->value.asNum.NumType == HB_ET_DOUBLE )
         {
            hb_comp_datetimeEncode( &pExpr->value.asDate.date, &pExpr->value.asDate.time,
                                    1899, 12, 30,
                                    ( int ) pHour->value.asNum.lVal, ( int ) pMinute->value.asNum.lVal, pSeconds->value.asNum.dVal, iAmPm, piOk );
         }
         else
         {
            hb_comp_datetimeEncode( &pExpr->value.asDate.date, &pExpr->value.asDate.time,
                                    1899, 12, 30,
                                    ( int ) pHour->value.asNum.lVal, ( int ) pMinute->value.asNum.lVal, ( double ) pSeconds->value.asNum.lVal, iAmPm, piOk );
         }
      }
      else
      {
         hb_comp_datetimeEncode( &pExpr->value.asDate.date, &pExpr->value.asDate.time,
                                 1899, 12, 30,
                                 ( int ) pHour->value.asNum.lVal, ( int ) pMinute->value.asNum.lVal, ( double ) 0, iAmPm, piOk );
      }
      pExpr->value.asDate.date = 0;
   }
   pExpr->value.asDate.type   = HB_ET_DDATETIME;

   pExpr->ValType             = HB_EV_DATE;

   return pExpr;
}

PHB_EXPR hb_compExprNewDateTimeVal( long lDate, long lTime, USHORT uType )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewDateTimeVal(%d,%d,%hu)", lDate, lTime, uType ) );

   pExpr                      = hb_compExprNew( HB_ET_DATE );
   pExpr->value.asDate.type   = uType;
   pExpr->value.asDate.date   = lDate;
   pExpr->value.asDate.time   = lTime;
   pExpr->ValType             = HB_EV_DATE;

   return pExpr;
}

PHB_EXPR hb_compExprNewString( char * szValue, HB_SIZE ulLen, BOOL fDealloc )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewString(%s,%" HB_PFS "u)", szValue, ulLen ) );

   pExpr                         = hb_compExprNew( HB_ET_STRING );
   pExpr->value.asString.string  = szValue;
   pExpr->value.asString.dealloc = fDealloc;
   pExpr->ulLength               = ulLen;
   pExpr->ValType                = HB_EV_STRING;

   return pExpr;
}

PHB_EXPR hb_compExprNewCodeBlock( void )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewCodeBlock()" ) );

   pExpr                         = hb_compExprNew( HB_ET_CODEBLOCK );
   pExpr->value.asList.pExprList = NULL;
   pExpr->value.asList.pIndex    = NULL;  /* this will hold local variables declarations */
   pExpr->ValType                = HB_EV_CODEBLOCK;

   return pExpr;
}

PHB_EXPR hb_compExprNewLogical( int iValue )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewLogical(%i)", iValue ) );

   pExpr                   = hb_compExprNew( HB_ET_LOGICAL );
   pExpr->value.asLogical  = iValue;
   pExpr->ValType          = HB_EV_LOGICAL;

   return pExpr;
}


PHB_EXPR hb_compExprNewNil( void )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewNil()" ) );

   pExpr          = hb_compExprNew( HB_ET_NIL );
   pExpr->ValType = HB_EV_NIL;

   return pExpr;
}

PHB_EXPR hb_compExprNewSelf( void )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewSelf()" ) );

   pExpr          = hb_compExprNew( HB_ET_SELF );
   pExpr->ValType = HB_EV_OBJECT;

   return pExpr;
}

PHB_EXPR hb_compExprNewVarRef( char * szVarName )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewVarRef(%s)", szVarName ) );

   pExpr                               = hb_compExprNew( HB_ET_VARREF );
   pExpr->value.asSymbol.szName        = szVarName;
   pExpr->value.asSymbol.szNamespace   = NULL;
   pExpr->ValType                      = HB_EV_VARREF;

   return pExpr;
}

PHB_EXPR hb_compExprNewMemVarRef( char * szVarName )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewVarRef(%s)", szVarName ) );

   pExpr                               = hb_compExprNew( HB_ET_MEMVARREF );
   pExpr->value.asSymbol.szName        = szVarName;
   pExpr->value.asSymbol.szNamespace   = NULL;
   pExpr->ValType                      = HB_EV_VARREF;

   return pExpr;
}

PHB_EXPR hb_compExprNewFunRef( char * szFunName )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewFunRef(%s)", szFunName ) );

   pExpr                               = hb_compExprNew( HB_ET_FUNREF );
   pExpr->value.asSymbol.szName        = szFunName;
   pExpr->value.asSymbol.szNamespace   = NULL;
   pExpr->ValType                      = HB_EV_FUNREF;

   return pExpr;
}

/* Creates a new literal array { item1, item2, ... itemN }
 *    'pArrList' is a list of array elements
 */
PHB_EXPR hb_compExprNewArray( PHB_EXPR pArrList )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewArray()" ) );

   pArrList->ExprType   = HB_ET_ARRAY; /* change type from ET_LIST */
   pArrList->ValType    = HB_EV_ARRAY;
   pArrList->ulLength   = 0;

   pExpr                = pArrList->value.asList.pExprList; /* get first element on the list */

   /* Now we need to replace all EO_NONE expressions with ET_NIL expressions
    * If EO_NONE is the first expression and there is no more expressions
    * then it is an empty array {} and ET_NIL cannot be used
    */
   if( pExpr->ExprType == HB_ET_NONE && pExpr->pNext == NULL )
   {
      hb_compExprClear( pExpr );
      pArrList->value.asList.pExprList = NULL;
   }
   else
   {
      /* there are at least one non-empty element specified
       */
      while( pExpr )
      {
         /* if empty element was specified replace it with NIL value */
         if( pExpr->ExprType == HB_ET_NONE )
            pExpr->ExprType = HB_ET_NIL;
         pExpr = pExpr->pNext;
         ++pArrList->ulLength;
      }
   }

   pArrList->value.asList.pIndex = NULL;

   return pArrList;
}

/* Creates new macro expression
 */
PHB_EXPR hb_compExprNewMacro( PHB_EXPR pMacroExpr, unsigned char cMacroOp, char * szName )
{
   PHB_EXPR pExpr;

   if( szName )
   {
      HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewMacro(%s)", szName ) );

      /* Macro variable is used:  &identifier
       * or macro text: [text]&variable[more_macro_text]
       */
      /*
       * NOTE: Clipper assumes that all variables used in macro expressions
       * are memvar variables
       * NOTE: Clipper pushes the complete macro expression converted
       * to string in case complex expression is used, e.g.
       * My&var.1
       * is pushed as:
       * "MY&VAR.1"
       */
      pExpr                            = hb_compExprNew( HB_ET_MACRO );
      pExpr->value.asMacro.cMacroOp    = cMacroOp; /* '&' if variable or 0 if text */
      pExpr->value.asMacro.szMacro     = szName;   /* variable name or macro text */
      pExpr->value.asMacro.pExprList   = NULL;     /* this is not a parenthesized expressions */
      pExpr->value.asMacro.SubType     = HB_ET_MACRO_VAR;

      if( cMacroOp == 0 )
      {
         /* check if variable with valid scope is used in macro text
          * (local, static and field variables are not allowed)
          * e.g.
          * LOCAL var
          * ? &var      // this is OK
          * ? &var.ext  // this is invalid
          */
         hb_compExprCheckMacroVar( szName );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewMacro(&)" ) );

      /* Macro expression:  &( expression_list )
       */
      pExpr                            = hb_compExprNew( HB_ET_MACRO );
      pExpr->value.asMacro.pExprList   = pMacroExpr;
      pExpr->value.asMacro.szMacro     = NULL; /* this is used to distinguish &(...) from &ident */
      pExpr->value.asMacro.SubType     = HB_ET_MACRO_EXPR;
   }

   return pExpr;
}

/* Creates new aliased variable
 *    aliasexpr -> identifier
 */
PHB_EXPR hb_compExprNewAliasVar( PHB_EXPR pAlias, PHB_EXPR pVariable )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewAliasVar()" ) );

   pExpr                         = hb_compExprNew( HB_ET_ALIASVAR );

   pExpr->value.asAlias.pAlias   = pAlias;
   pExpr->value.asAlias.pVar     = pVariable;
   pExpr->value.asAlias.pExpList = NULL;

   /* macro expressions in alias context require a special handling
    */
   if( pAlias->ExprType == HB_ET_MACRO )
      pAlias->value.asMacro.SubType = HB_ET_MACRO_ALIASED;
   if( pVariable->ExprType == HB_ET_MACRO )
      pVariable->value.asMacro.SubType = HB_ET_MACRO_ALIASED;

   return pExpr;
}

/* Creates new aliased expression
 *    alias_expr -> ( expression )
 */
PHB_EXPR hb_compExprNewAliasExpr( PHB_EXPR pAlias, PHB_EXPR pExpList )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewAliasExpr()" ) );

   pExpr                         = hb_compExprNew( HB_ET_ALIASEXPR );

   pExpr->value.asAlias.pAlias   = pAlias;
   pExpr->value.asAlias.pExpList = pExpList;
   pExpr->value.asAlias.pVar     = NULL;

   if( pAlias->ExprType == HB_ET_MACRO )
   {
      /* Is it a special case &variable->( expressionList ) */
      //if( pAlias->value.asMacro.SubType == HB_ET_MACRO_VAR )
      pAlias->value.asMacro.SubType = HB_ET_MACRO_ALIASED;
   }

   return pExpr;
}

/* Creates new send expression
 *    pObject : szMessage
 */
PHB_EXPR hb_compExprNewSend( PHB_EXPR pObject, char * szMessage )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewSend(%p, %s)", pObject, szMessage ) );

   pExpr                                  = hb_compExprNew( HB_ET_SEND );
   pExpr->value.asMessage.szMessage       = szMessage;
   pExpr->value.asMessage.pObject         = pObject;
   pExpr->value.asMessage.pParms          = NULL;
   pExpr->value.asMessage.bByRef          = FALSE;
   pExpr->value.asMessage.pMacroMessage   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewSendExp( PHB_EXPR pObject, PHB_EXPR pMessage )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewSend(%p, %s)", pObject, pMessage->value.asSymbol.szName ) );

   pExpr                            = hb_compExprNew( HB_ET_SEND );
   pExpr->value.asMessage.pObject   = pObject;
   pExpr->value.asMessage.pParms    = NULL;
   pExpr->value.asMessage.bByRef    = FALSE;

   if( pMessage->ExprType == HB_ET_FUNNAME )
   {
      pExpr->value.asMessage.szMessage       = pMessage->value.asSymbol.szName;
      pExpr->value.asMessage.pMacroMessage   = NULL;

      pMessage->value.asSymbol.szName        = NULL;
      hb_compExprClear( pMessage );
   }
   else
   {
      pExpr->value.asMessage.szMessage       = NULL;
      pExpr->value.asMessage.pMacroMessage   = pMessage;
   }

   return pExpr;
}

PHB_EXPR hb_compExprNewWithSend( char * szMessage )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewWithSend(%s)", szMessage ) );

   pExpr                                  = hb_compExprNew( HB_ET_WITHSEND );
   pExpr->value.asMessage.pObject         = NULL;
   pExpr->value.asMessage.pParms          = NULL;
   pExpr->value.asMessage.szMessage       = szMessage;
   pExpr->value.asMessage.pMacroMessage   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewWithSendExp( PHB_EXPR pMessage )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewWithSendExp(%s)", pMessage->value.asSymbol.szName ) );

   pExpr                            = hb_compExprNew( HB_ET_WITHSEND );
   pExpr->value.asMessage.pObject   = NULL;
   pExpr->value.asMessage.pParms    = NULL;

   if( pMessage->ExprType == HB_ET_FUNNAME )
   {
      pExpr->value.asMessage.szMessage       = pMessage->value.asSymbol.szName;
      pExpr->value.asMessage.pMacroMessage   = NULL;

      pMessage->value.asSymbol.szName        = NULL;
      hb_compExprClear( pMessage );
   }
   else
   {
      pExpr->value.asMessage.szMessage       = NULL;
      pExpr->value.asMessage.pMacroMessage   = pMessage;
   }

   return pExpr;
}

/* Creates new method call
 *    pObject : identifier ( pArgList )
 *
 *    pObject = is an expression returned by hb_compExprNewSend
 *    pArgList = list of passed arguments - it will be HB_ET_NONE if no arguments
 *                are passed
 */
PHB_EXPR hb_compExprNewMethodCall( PHB_EXPR pObject, PHB_EXPR pArgList )
{
   pObject->value.asMessage.pParms = pArgList;

   return pObject;
}

PHB_EXPR hb_compExprNewWithMethodCall( PHB_EXPR pWithMessage, PHB_EXPR pArgList )
{
   pWithMessage->value.asMessage.pParms = pArgList;

   return pWithMessage;
}

/* Creates a list - all elements will be used
 * This list can be used to create an array or function's call arguments
 */
PHB_EXPR hb_compExprNewList( PHB_EXPR pFirstItem )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewList()" ) );

   pExpr                         = hb_compExprNew( HB_ET_LIST );
   pExpr->value.asList.pExprList = pFirstItem;

   return pExpr;
}

/* Creates a list of function call arguments
 */
PHB_EXPR hb_compExprNewArgList( PHB_EXPR pFirstItem )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewArgList()" ) );

   pExpr                         = hb_compExprNew( HB_ET_ARGLIST );
   pExpr->value.asList.pExprList = pFirstItem;

   return pExpr;
}

/* Adds new element to the list
 */
PHB_EXPR hb_compExprAddListExpr( PHB_EXPR pList, PHB_EXPR pNewItem )
{
   if( pList->value.asList.pExprList )
   {
      PHB_EXPR pExpr;

      /* add new item to the end of the list */
      pExpr          = pList->value.asList.pExprList;
      while( pExpr->pNext )
         pExpr = pExpr->pNext;
      pExpr->pNext   = pNewItem;
   }
   else
      pList->value.asList.pExprList = pNewItem;

   return pList;
}

PHB_EXPR hb_compExprNewVar( char * szName )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewVar(%s)", szName ) );

   pExpr                               = hb_compExprNew( HB_ET_VARIABLE );
   pExpr->value.asSymbol.szName        = szName;
   pExpr->value.asSymbol.szNamespace   = NULL;

   return pExpr;
}

/* Create a new declaration of PUBLIC or PRIVATE variable.
 *
 * szName is a string with variable name if 'PUBLIC varname' context
 * pMacroVar is a macro expression if 'PUBLIC &varname' context
 */
PHB_EXPR hb_compExprNewRTVar( char * szName, PHB_EXPR pMacroVar )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewRTVar(%s, %p)", szName, pMacroVar ) );

   pExpr                         = hb_compExprNew( HB_ET_RTVAR );
   pExpr->value.asRTVar.szName   = szName;
   pExpr->value.asRTVar.pMacro   = pMacroVar;

   if( pMacroVar )
      pMacroVar->value.asMacro.SubType = HB_ET_MACRO_SYMBOL;

   return pExpr;
}

/* Create a new symbol used in an alias expressions
 */
PHB_EXPR hb_compExprNewAlias( char * szName )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewAlias(%s)", szName ) );

   pExpr                               = hb_compExprNew( HB_ET_ALIAS );
   pExpr->value.asSymbol.szName        = szName;
   pExpr->value.asSymbol.szNamespace   = NULL;

   return pExpr;
}


/* ************************************************************************* */

PHB_EXPR hb_compExprNewEqual( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_EQUAL );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewPlus( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_PLUS );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewMinus( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_MINUS );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewMult( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_MULT );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewDiv( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_DIV );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewMod( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_MOD );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewPower( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_POWER );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewBitAnd( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_BITAND );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewBitOr( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_BITOR );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewBitXOr( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_BITXOR );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewBitShiftR( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_BITSHIFTR );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewBitShiftL( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_BITSHIFTL );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewPostInc( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_POSTINC );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewPostDec( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_POSTDEC );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewPreInc( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_PREINC );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewPreDec( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_PREDEC );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewPlusEq( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_PLUSEQ );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewMinusEq( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_MINUSEQ );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewMultEq( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_MULTEQ );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewDivEq( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_DIVEQ );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewModEq( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_MODEQ );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewExpEq( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_EXPEQ );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewAnd( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_AND );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewOr( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_OR );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewMatch( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_MATCH );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewLike( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_LIKE );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewNot( PHB_EXPR pNotExpr )
{
   PHB_EXPR pExpr;

   if( pNotExpr->ExprType == HB_ET_LOGICAL )
   {
      pNotExpr->value.asLogical  = ! pNotExpr->value.asLogical;
      pExpr                      = pNotExpr;
   }
   else
   {
      pExpr                            = hb_compExprNew( HB_EO_NOT );
      pExpr->value.asOperator.pLeft    = pNotExpr;
      pExpr->value.asOperator.pRight   = NULL;
   }

   return pExpr;
}

PHB_EXPR hb_compExprNewEQ( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_EQ );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewLT( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_LT );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewGT( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_GT );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewLE( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_LE );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewGE( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_GE );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewNE( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_NE );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

PHB_EXPR hb_compExprNewIN( PHB_EXPR pLeftExpr )
{
   PHB_EXPR pExpr = hb_compExprNew( HB_EO_IN );

   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = NULL;

   return pExpr;
}

/* NOTE: all invalid cases are handled by yacc rules
 */
PHB_EXPR hb_compExprNewNegate( PHB_EXPR pNegExpr )
{
   PHB_EXPR pExpr;

   if( pNegExpr->ExprType == HB_ET_NUMERIC )
   {
      if( pNegExpr->value.asNum.NumType == HB_ET_DOUBLE )
      {
         pNegExpr->value.asNum.dVal    = -pNegExpr->value.asNum.dVal;
         pNegExpr->value.asNum.bWidth  = ( unsigned char ) HB_DBL_LENGTH( pNegExpr->value.asNum.dVal );
      }
      else
      {
         pNegExpr->value.asNum.lVal    = -pNegExpr->value.asNum.lVal;
         pNegExpr->value.asNum.bWidth  = ( unsigned char ) HB_LONG_LENGTH( pNegExpr->value.asNum.lVal );
      }
      pExpr = pNegExpr;
   }
   else
   {
      pExpr                            = hb_compExprNew( HB_EO_NEGATE );
      pExpr->value.asOperator.pLeft    = pNegExpr;
      pExpr->value.asOperator.pRight   = NULL;
   }

   return pExpr;
}

/* ************************************************************************* */

/* Handles (expression := expression) syntax
 */
PHB_EXPR hb_compExprAssign( PHB_EXPR pLeftExpr, PHB_EXPR pRightExpr )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprAssign()" ) );

   pExpr                            = hb_compExprNew( HB_EO_ASSIGN );
   pExpr->value.asOperator.pLeft    = pLeftExpr;
   pExpr->value.asOperator.pRight   = pRightExpr;

   return pExpr;
}

/*  Return a number of elements on the linked list
 */
HB_ULONG hb_compExprListLen( PHB_EXPR pExpr )
{
   HB_ULONG ulLen = 0;

   if( pExpr )
   {
      pExpr = pExpr->value.asList.pExprList;

      while( pExpr )
      {
         pExpr = pExpr->pNext;
         ++ulLen;
      }
   }

   return ulLen;
}
