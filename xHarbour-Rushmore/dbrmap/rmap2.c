/*
 * $Id$
 */

/*
 * DBRMAP (Record Map filters) for [x]Harbour:
 *    Record Map - query analyzer
 *
 * Copyright 2004-2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * All rights reserved.
 *
 */

#include "hbrddrm.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbdate.h"
#include "hbset.h"

#ifdef __XHARBOUR__
#  include "hbfast.h"
#endif

/*
#define HB_RMQ_DEBUG
#define HB_RMQ_DBGDISP
*/

#define HB_RMQ_NONE     0x00
#define HB_RMQ_NOT      0x01
#define HB_RMQ_AND      0x02
#define HB_RMQ_OR       0x03

#define HB_RMQ_TAG      0x04
#define HB_RMQ_SKIP     0x05

#define HB_RMQ_LT       0x11
#define HB_RMQ_GT       0x12
#define HB_RMQ_LE       0x13
#define HB_RMQ_GE       0x14
#define HB_RMQ_EQU      0x15
#define HB_RMQ_NEQU     0x16
#define HB_RMQ_EEQU     0x17
#define HB_RMQ_NEEQU    0x18
#define HB_RMQ_RNGII    0x19        /* include left and right lims */
#define HB_RMQ_RNGEI    0x1A        /* exclude left, include right lims */
#define HB_RMQ_RNGIE    0x1B        /* include left, exclude right lims */
#define HB_RMQ_RNGEE    0x1C        /* exclude both lims */

#define HB_RMQ_RANGE    HB_RMQ_RNGII

#define HB_RMQ_STACKSIZE 32

#define HB_RMQ_ISIDCH( b )    ( (b)=='_' || ((b) >= 'A' && (b) <= 'Z') || \
                                            ((b) >= 'a' && (b) <= 'z') || \
                                            ((b) >= '0' && (b) <= '9') )

#define HB_RMQ_DELEXP   "DELETED()"

typedef struct _HB_RMQTAG
{
   int      iTag;             /* the tag number in AREA index list */
   char     cType;            /* type of key expression */
   HB_BOOL  fDescend;         /* descending order */
   char *   pKey;             /* the stripped tag expression */
   HB_SIZE  nKey;             /* expression length */
   char *   pFor;             /* the stripped FOR expression */
   HB_SIZE  nFor;             /* FOR expression length */
   HB_BOOL  fDelExp;          /* FOR is '!DELETED()' exp */
   struct _HB_RMQTAG * pNext; /* pointer to next expression */
} HB_RMQTAG;
typedef HB_RMQTAG * PHB_RMQTAG;

typedef struct _HB_RMQSTACKITM
{
   int            iOper;      /* type of operation HB_RMQ_* */
   int            iOpt;       /* optimization level */
   HB_BOOL        fNeg;       /* neg the operation */
   PHB_ITEM       pItmLo;     /* index low value */
   PHB_ITEM       pItmHi;     /* index high value */
   PHB_RMQTAG     pTag;       /* the Tag */
   char *         pszExpr;    /* the expression */
   HB_SIZE        nExpr;      /* length of expression */
} HB_RMQSTACKITM;
typedef HB_RMQSTACKITM * PHB_RMQSTACKITM;

typedef struct _HB_RMQSTACK
{
   int               iSize;   /* stack size */
   int               iPos;    /* current stack pos */
   int               iCur;    /* execute stack pos */
   PHB_RMQSTACKITM   pBase;   /* the real stack items */
   PHB_RMQTAG        pTags;   /* list of area tags */
   char *            pszExpr; /* the expression */
   HB_SIZE           nExpr;   /* length of expression */
   AREAP             pArea;   /* work area pointer */
   HB_ULONG          ulRecs;  /* number of records in work area */
   HB_BOOL           fNExpr;  /* save non optimizable expression */
} HB_RMQSTACK;
typedef HB_RMQSTACK * PHB_RMQSTACK;

static char s_szDelExp[ 10 ];
static HB_SIZE s_nDelExp = 0;

/*
 * evaluate macro string, return the ITEM result or NULL on error
 */
static PHB_ITEM hb_rmqMacroEval( const char * pszMacro, HB_SIZE nLen )
{
   int iCurrArea = hb_rddGetCurrentWorkAreaNumber();
   PHB_ITEM pItem;
   const char *szType;

   pItem = hb_itemPutCL( NULL, pszMacro, nLen );
   hb_rddSelectWorkAreaNumber( 0 );
#ifdef __XHARBOUR__
   szType = hb_macroGetType( pItem, 0 );
#else
   szType = hb_macroGetType( pItem );
#endif
   if( szType[ 0 ] == 'U' && szType[ 1 ] != 'I' )
   {
      hb_itemRelease( pItem );
      pItem = NULL;
   }
   else
   {
      hb_vmPush( pItem );
      hb_macroGetValue( hb_stackItemFromTop( -1 ), 0, 0 );
      if( hb_vmRequestQuery() == 0 )
      {
         hb_itemCopy( pItem, hb_stackItemFromTop( -1 ) );
         hb_stackPop();
      }
      else
      {
         hb_itemRelease( pItem );
         pItem = NULL;
      }
   }
   hb_rddSelectWorkAreaNumber( iCurrArea );
   return pItem;
}

static HB_SIZE hb_rmqCloseBrace( const char * pszExpr, HB_SIZE nLen )
{
   char bQuote = 0, b;
   HB_SIZE n = 0;
   int iB = 0;

   if( nLen == 0 )
      return 0;

   do
   {
      b = pszExpr[ n ];

      if( bQuote )
      {
         if( bQuote == b )
            bQuote = 0;
      }
      else if( b == '\'' || b == '"' || b == '[' )
      {
         bQuote = ( b == '[' ) ? ']' : b;
      }
      else
      {
         if( b == '(' )
            ++iB;
         else if( b == ')' )
            --iB;
      }
   }
   while( ++n < nLen && iB );

   if( iB )
      n = 0;

   return n;
}

static HB_SIZE hb_rmqStripBraces( const char * pszExpr, HB_SIZE nLen )
{
   HB_SIZE n = 0, l;
   while( ( n << 1 ) + 1 < nLen )
   {
      l = nLen - ( n << 1 );
      if( hb_rmqCloseBrace( &pszExpr[ n ], l ) == l )
         ++n;
      else
         break;
   }
   return n;
}

static HB_SIZE hb_rmqNextExp( const char * pszExpr, HB_SIZE nLen,
                              int * piOper, HB_SIZE * pnExp )
{
   char bQuote = 0, b;
   HB_SIZE n = 0, nLast = 0, nLast2 = 0;
   int iOper, iLastOp = HB_RMQ_NONE;

   iOper = piOper ? *piOper : HB_RMQ_NONE;

   while( n < nLen )
   {
      b = pszExpr[ n ];

      if( bQuote )
      {
         if( bQuote == b )
            bQuote = 0;
      }
      else if( b == '\'' || b == '"' || b == '[' )
      {
         bQuote = ( b == '[' ) ? ']' : b;
      }
      else
      {
         if( b == '(' )
         {
            HB_SIZE l = hb_rmqCloseBrace( &pszExpr[ n ], nLen - n );

            if( l == 0 )
            {
               break;
            }
            n += l - 1;
         }
         else if( b == '.' )
         {
            if( nLen - n >= 5 && hb_strnicmp( &pszExpr[ n ], ".AND.", 5 ) == 0 )
            {
               if( iLastOp == HB_RMQ_NONE )
               {
                  iLastOp = HB_RMQ_AND;
                  nLast = n + 5;
                  nLast2 = n;
               }
               n += 5;
               if( iOper == HB_RMQ_NONE || iOper == HB_RMQ_AND )
                  break;
               continue;
            }
            else if( nLen - n >= 4 && hb_strnicmp( &pszExpr[ n ], ".OR.", 4 ) == 0 )
            {
               if( iLastOp == HB_RMQ_NONE )
               {
                  iLastOp = HB_RMQ_OR;
                  nLast = n + 4;
                  nLast2 = n;
               }
               n += 4;
               if( iOper == HB_RMQ_NONE || iOper == HB_RMQ_OR )
                  break;
               continue;
            }
         }
      }
      n++;
   }

   if( piOper )
      *piOper = iLastOp;

   if( pnExp )
      *pnExp = nLast2;

   return nLast;
}

#if 1
static int hb_rmqNegOper( int iOper )
{
   switch( iOper )
   {
      case HB_RMQ_LT:
         return HB_RMQ_GE;
      case HB_RMQ_GT:
         return HB_RMQ_LE;
      case HB_RMQ_LE:
         return HB_RMQ_GT;
      case HB_RMQ_GE:
         return HB_RMQ_LT;
      case HB_RMQ_EQU:
         return HB_RMQ_NEQU;
      case HB_RMQ_NEQU:
         return HB_RMQ_EQU;
      case HB_RMQ_EEQU:
         return HB_RMQ_NEEQU;
      case HB_RMQ_NEEQU:
         return HB_RMQ_EEQU;
      default:
         return HB_RMQ_NONE;
   }
}

static HB_BOOL hb_rmqCanBeJoined( PHB_RMQSTACKITM pOp )
{
   HB_BOOL fCan = HB_FALSE;

   if( pOp->pTag )
   {
      switch( pOp->iOper )
      {
         case HB_RMQ_LT:
         case HB_RMQ_GT:
         case HB_RMQ_LE:
         case HB_RMQ_GE:
            fCan = HB_TRUE;
            break;
      }
   }
   return fCan;
}

static HB_BOOL hb_rmqJoinOper( HB_BOOL fTest, PHB_RMQSTACKITM pOp1,
                               PHB_RMQSTACKITM pOp2, HB_BOOL fNeg )
{
  HB_BOOL fJoined = HB_FALSE;

   if( hb_rmqCanBeJoined( pOp1 ) && hb_rmqCanBeJoined( pOp2 ) &&
       pOp1->pTag->iTag == pOp2->pTag->iTag )
   {
      HB_BOOL fCan = HB_TRUE;
      int iOper1 = pOp1->iOper, iOper2 = pOp2->iOper, iOper;
      PHB_ITEM pItmLo1 = pOp1->pItmLo, pItmHi1 = pOp1->pItmHi,
               pItmLo2 = pOp2->pItmLo, pItmHi2 = pOp2->pItmHi,
               pItmLo, pItmHi, pItm;

      if( pOp1->fNeg )
         fNeg = !fNeg;

      if( fNeg )
      {
         iOper = hb_rmqNegOper( iOper1 );
         if( iOper == HB_RMQ_NONE )
            fCan = HB_FALSE;
         else
         {
            iOper1 = iOper;
            pItm = pItmLo1;
            pItmLo1 = pItmHi1;
            pItmHi1 = pItm;
         }
      }
      if( fCan && pOp2->fNeg )
      {
         iOper = hb_rmqNegOper( iOper2 );
         if( iOper == HB_RMQ_NONE )
            fCan = HB_FALSE;
         else
         {
            iOper2 = iOper;
            pItm = pItmLo2;
            pItmLo2 = pItmHi2;
            pItmHi2 = pItm;
         }
      }

      if( pOp1->fNeg )
         fNeg = !fNeg;

      if( fCan )
      {
         iOper = HB_RMQ_NONE;
         pItmLo = pItmHi = NULL;
         if( ( iOper1 == HB_RMQ_LE || iOper1 == HB_RMQ_LT ) &&
             ( iOper2 == HB_RMQ_GE || iOper2 == HB_RMQ_GT ) )
         {
            if( iOper1 == HB_RMQ_LE )
            {
               if( iOper2 == HB_RMQ_GE )
                  iOper = HB_RMQ_RNGII;
               else
                  iOper = HB_RMQ_RNGEI;
            }
            else
            {
               if( iOper2 == HB_RMQ_GE )
                  iOper = HB_RMQ_RNGIE;
               else
                  iOper = HB_RMQ_RNGEE;
            }
            if( !fTest )
               pItmLo = hb_itemNew( pItmLo2 );
            pItmHi = pItmHi1;
         }
         else if( ( iOper1 == HB_RMQ_GE || iOper1 == HB_RMQ_GT ) &&
                  ( iOper2 == HB_RMQ_LE || iOper2 == HB_RMQ_LT ) )
         {
            if( iOper1 == HB_RMQ_GE )
            {
               if( iOper2 == HB_RMQ_LE )
                  iOper = HB_RMQ_RNGII;
               else
                  iOper = HB_RMQ_RNGIE;
            }
            else
            {
               if( iOper2 == HB_RMQ_LE )
                  iOper = HB_RMQ_RNGEI;
               else
                  iOper = HB_RMQ_RNGEE;
            }
            pItmLo = pItmLo1;
            if( !fTest )
               pItmHi = hb_itemNew( pItmHi2 );
         }

         if( iOper != HB_RMQ_NONE )
         {
            if( !fTest )
            {
               pOp1->iOper = iOper;
               pOp1->fNeg = fNeg;
               pOp1->pItmLo = pItmLo;
               pOp1->pItmHi = pItmHi;

               /*
               pOp2->iOper = HB_RMQ_SKIP;
               pOp2->pItmLo = pOp2->pItmHi = NULL;
               if( pItmLo1 )
                  hb_itemRelease( pItmLo1 );
               if( pItmHi1 )
                  hb_itemRelease( pItmHi1 );
               */
            }
            fJoined = HB_TRUE;
         }
      }
   }

   return fJoined;
}

#endif

static HB_SIZE hb_rmqOper( const char * pszExpr, HB_SIZE nLen,
                           int * piOper, HB_SIZE * pnExp )
{
   char bQuote = 0, b, bLast = 0;
   HB_SIZE n = 0;
   int iOper = HB_RMQ_NONE;

   while( n < nLen )
   {
      if( pnExp )
         *pnExp = n;

      b = pszExpr[ n ];

      if( bQuote )
      {
         if( bQuote == b )
            bQuote = 0;
      }
      else if( b == '\'' || b == '"' || b == '[' )
      {
         bQuote = ( b == '[' ) ? ']' : b;
      }
      else
      {
         if( b == '(' )
         {
            HB_SIZE l = hb_rmqCloseBrace( &pszExpr[ n ], nLen - n );

            if( l == 0 )
            {
               n = 0;
               break;
            }
            n += l - 1;
         }
         else if( b == '.' )
         {
            if( nLen - n >= 5 && hb_strnicmp( &pszExpr[ n ], ".AND.", 5 ) == 0 )
            {
               iOper = HB_RMQ_AND;
               n += 5;
               break;
            }
            else if( nLen - n >= 4 && hb_strnicmp( &pszExpr[ n ], ".OR.", 4 ) == 0 )
            {
               iOper = HB_RMQ_OR;
               n += 4;
               break;
            }
         }
         else if( b == '=' )
         {
            if( n == 0 || bLast != ':' )
            {
               if( nLen - n >= 1 && pszExpr[ n + 1 ] == '=' )
               {
                  iOper = HB_RMQ_EEQU;
                  n += 2;
               }
               else
               {
                  iOper = HB_RMQ_EQU;
                  n++;
               }
               break;
            }
         }
         else if( b == '>' )
         {
            if( n == 0 || bLast != '-' )
            {
               if( nLen - n >= 1 && pszExpr[ n + 1 ] == '=' )
               {
                  iOper = HB_RMQ_GE;
                  n += 2;
               }
               else
               {
                  iOper = HB_RMQ_GT;
                  n++;
               }
               break;
            }
         }
         else if( b == '<' )
         {
            if( nLen - n >= 1 && pszExpr[ n + 1 ] == '=' )
            {
               iOper = HB_RMQ_LE;
               n += 2;
            }
            else
            {
               iOper = HB_RMQ_LT;
               n++;
            }
            break;
         }
         else if( b == '#' )
         {
            iOper = HB_RMQ_NEQU;
            n++;
            break;
         }
      }
      bLast = b;
      n++;
   }

   if( piOper )
      *piOper = iOper;

   return ( n == nLen ) ? 0 : n;
}

static void hb_rmqStripExp( const char ** pExprPtr, HB_SIZE *pnLen, HB_BOOL *pfNeg )
{
   HB_SIZE n;

   do
   {
      n = hb_rmqStripBraces( *pExprPtr, *pnLen );
      if( n > 0 )
      {
         *pExprPtr += n;
         *pnLen -= ( n << 1 );
      }
      else if( ( *pExprPtr )[ 0 ] == '!' && pfNeg &&
               hb_rmqNextExp( *pExprPtr, *pnLen, NULL, NULL ) == 0 )
      {
         *pfNeg = !*pfNeg;
         n = 1;
         ( *pExprPtr )++;
         ( *pnLen )--;
      }
   }
   while( n > 0 );
}

static HB_BOOL hb_rmqIsAbbFunc( const char * pszExpr, int iLen, int *iNewLen )
{
   static const char * functions[] =
      { "SUBSTR", "DELETED", "REPLICATE", NULL };
   /* TODO: add other functions worth to detect */

   int iFunc, i;

   if( iLen > 4 )
   {
      for( iFunc = 0; functions[ iFunc ]; iFunc++ )
      {
         i = 0;
         while( i < iLen && functions[ iFunc ][ i ] == pszExpr[ i ] )
            i++;
         if( pszExpr[ i ] == '(' )
         {
            *iNewLen = 4;
            return HB_TRUE;
         }
      }
   }
   return HB_FALSE;
}

/*
 * compare two stripped expressions, it's like normal memcmp but
 * it ignores different quotas so expressions:
 * "F1='ABC'", 'F1="ABC"' and "F1=[ABC]" are equal
 */
static HB_BOOL hb_rmqCmp( const char * pszSrcExpr, HB_SIZE nSrcLen,
                          const char * pszDstExpr, HB_SIZE nDstLen,
                          HB_BOOL *pfNeg, HB_BOOL fStr )
{
   char bQuote1 = 0, bQuote2 = 0, b1, b2;
   HB_SIZE n;

   hb_rmqStripExp( &pszSrcExpr, &nSrcLen, pfNeg );
   hb_rmqStripExp( &pszDstExpr, &nDstLen, pfNeg );

   if( nSrcLen != nDstLen )
   {
      if( !fStr || nSrcLen > nDstLen || pszDstExpr[ nSrcLen ] != '+' )
         return HB_FALSE;
   }

   for( n = 0; n < nSrcLen; n++ )
   {
      b1 = pszSrcExpr[ n ];
      b2 = pszDstExpr[ n ];

      if( bQuote1 )
      {
         if( bQuote1 == b1 )
         {
            bQuote1 = 0;
            b1 = ']';
         }
      }
      else if( b1 == '\'' || b1 == '"' || b1 == '[' )
      {
         bQuote1 = ( b1 == '[' ) ? ']' : b1;
         b1 = '[';
      }

      if( bQuote2 )
      {
         if( bQuote2 == b2 )
         {
            bQuote2 = 0;
            b2 = ']';
         }
      }
      else if( b2 == '\'' || b2 == '"' || b2 == '[' )
      {
         bQuote2 = ( b2 == '[' ) ? ']' : b2;
         b2 = '[';
      }

      if( b1 != b2 || ( ( bQuote1 != 0 ) != ( bQuote2 != 0 ) ) )
         return HB_FALSE;
   }

   return HB_TRUE;
}

static HB_BOOL hb_rmqFuncCmp( const char * pszKeyExpr, HB_SIZE nKeyLen,
                              const char * pszFncExpr, HB_SIZE nFncLen,
                              const char * pszParExpr, HB_SIZE nParLen,
                              HB_BOOL * pfNeg, HB_BOOL fStr )
{
   HB_SIZE n;

   hb_rmqStripExp( &pszKeyExpr, &nKeyLen, pfNeg );

   if( nKeyLen > nFncLen + 2 )
   {
      if( memcmp( pszKeyExpr, pszFncExpr, nFncLen ) == 0 && pszKeyExpr[ nFncLen ] == '(' )
      {
         pszKeyExpr += nFncLen;
         nKeyLen -= nFncLen;
         n = hb_rmqCloseBrace( pszKeyExpr, nKeyLen );
         if( fStr && n > 0 && pszKeyExpr[ n ] == '+' )
            nKeyLen = n;
         if( n == nKeyLen )
            return hb_rmqCmp( pszKeyExpr, nKeyLen, pszParExpr, nParLen, NULL, HB_FALSE );
      }
   }

   return HB_FALSE;
}

/*
 * strip spaces and FIELD->, _FIELD->, <ALIAS>->expressions,
 * replace '.not.' with '!' and '!=', '<>' with '#',
 * and make all non quoted part upper
 */
static char * hb_rmqStrip( const char * pszSrcExpr, HB_SIZE nLen,
                           const char * szAlias, HB_SIZE *pnDstLen )
{
   char bQuote = 0, bLast = 0, * pszDstExpr, b;
   HB_SIZE nDst = 0, nAlias, nL;
   int iLen, iNewLen;

   pszDstExpr = ( char * ) hb_xgrab( nLen + 1 );

   nAlias = szAlias ? strlen( szAlias ) : 0;

   for( nL = 0; nL < nLen; nL++ )
   {
      b = pszSrcExpr[ nL ];

      if( bQuote )
      {
         if( bQuote == b )
         {
            bQuote = 0;
         }
         pszDstExpr[ nDst++ ] = b;
      }
      else if( b == '\'' || b == '"' || b == '[' )
      {
         bQuote = ( b == '[' ) ? ']' : b;
         pszDstExpr[ nDst++ ] = b;
      }
      else
      {
         if( b != ' ' )
         {
            if( bLast == ' ' && nDst > 0 &&
                 HB_RMQ_ISIDCH( b ) && HB_RMQ_ISIDCH( pszDstExpr[ nDst - 1 ] ) )
            {
               pszDstExpr[ nDst++ ] = bLast;
            }
            pszDstExpr[ nDst++ ] = ( char ) HB_TOUPPER( b );
            if( b == '>' && bLast == '-' )
            {
               HB_SIZE n = 0;

               if( nDst >= 7 && hb_strnicmp( &pszDstExpr[ nDst - 7 ], "FIELD", 5 ) == 0 )
               {
                  n = 7;
                  if( nDst >= 8 && pszDstExpr[ nDst - 8 ] == '_' )
                     ++n;
                  if( n < nDst && HB_RMQ_ISIDCH( pszDstExpr[ nDst - n - 1 ] ) )
                     n = 0;
               }
               if( n == 0 && nAlias && nDst >= nAlias + 2 )
               {
                  if( hb_strnicmp( &pszDstExpr[ nDst - nAlias - 2 ], szAlias, nAlias ) == 0 )
                  {
                     n = nAlias + 2;
                     if( n < nDst && HB_RMQ_ISIDCH( pszDstExpr[ nDst - n - 1 ] ) )
                        n = 0;
                  }
                  else if( nDst > 4 && pszDstExpr[ nDst - 3 ] == ')' )
                  {
                     HB_SIZE l = nDst - 2;
                     int iB = 0;

                     do
                     {
                        --l;
                        if( pszDstExpr[ l ] == '(' )
                           ++iB;
                        else if( pszDstExpr[ l ] == ')' )
                           --iB;
                     }
                     while( l > 0 && iB );

                     if( !iB )
                     {
                        PHB_ITEM pRet = hb_rmqMacroEval( &pszDstExpr[ l ], nDst - l - 2 );
                        if( pRet )
                        {
                           const char * cRet = hb_itemGetCPtr( pRet );
                           HB_SIZE ll = hb_itemGetCLen( pRet );
                           if( ll > 0 )
                           {
                              while( ll > 0 && cRet[ ll - 1 ] == ' ' )
                              {
                                 --ll;
                              }
                              while( *cRet == ' ' )
                              {
                                 ++cRet;
                                 --ll;
                              }
                              if( ll == nAlias && hb_strnicmp( cRet, szAlias, nAlias ) == 0 )
                              {
                                 n = nDst - l;
                              }
                           }
                           hb_itemRelease( pRet );
                        }
                     }
                  }
               }
               if( n > 0 )
               {
                  nDst -= n;
               }
            }
            else if( ( b == '=' && bLast == '!' ) || ( b == '>' && bLast == '<' ) )
            {
               nDst -= 2;
               pszDstExpr[ nDst++ ] = '#';
            }
            else if( b == '.' && nDst >= 5 )
            {
               if( bLast == 't' || bLast == 'T' )
               {
                  if( hb_strnicmp( &pszDstExpr[ nDst - 5 ], ".NOT.", 5 ) == 0 )
                  {
                     nDst -= 5;
                     pszDstExpr[ nDst++ ] = b = '!';
                  }
                  else if( pszDstExpr[ nDst - 3 ] == '.' )
                  {
                     if( pszDstExpr[ nDst - 4 ] == '=' )
                     {
                        if( pszDstExpr[ nDst - 5 ] == '=' )
                           nDst -= 5;
                        else if( pszDstExpr[ nDst - 5 ] != '>' && pszDstExpr[ nDst - 5 ] != '<' )
                           nDst -= 4;
                     }
                  }
               }
               else if( ( bLast == 'f' || bLast == 'F' ) && pszDstExpr[ nDst - 3 ] == '.' )
               {
                  if( pszDstExpr[ nDst - 4 ] == '#' )
                     nDst -= 4;
               }
            }
            else if( b == '(' && nDst > 1 )
            {
               iLen = 0;
               while( nDst - iLen >= 2 &&
                      HB_RMQ_ISIDCH( pszDstExpr[ nDst - iLen - 2 ] ) )
               {
                  ++iLen;
               }
               if( iLen > 4 && hb_rmqIsAbbFunc( &pszDstExpr[ nDst - 1 - iLen ], iLen, &iNewLen ) )
               {
                  nDst = nDst - 1 - iLen + iNewLen;
                  pszDstExpr[ nDst++ ] = '(';
               }
            }
            if( nDst >= 2 && b == '!' && pszDstExpr[ nDst - 2 ] == '!' )
            {
               nDst -= 2;
            }
         }
      }
      bLast = b;
   }

   nL = hb_rmqStripBraces( pszDstExpr, nDst );
   if( nL > 0 )
   {
      nDst -= ( nL << 1 );
      memmove( pszDstExpr, &pszDstExpr[ nL ], nDst );
   }

   pszDstExpr[ nDst ] = '\0';
   *pnDstLen = nDst;
   return pszDstExpr;
}

static void hb_rmqDestroyTagList( PHB_RMQTAG pRMQTags )
{
   PHB_RMQTAG pNext;

   while( pRMQTags )
   {
      pNext = pRMQTags->pNext;
      if( pRMQTags->pKey )
         hb_xfree( pRMQTags->pKey );
      if( pRMQTags->pFor )
         hb_xfree( pRMQTags->pFor );
      hb_xfree( pRMQTags );
      pRMQTags = pNext;
   }
}

#ifdef HB_RMQ_DBGDISP
static void hb_rmqDispTagList( PHB_RMQTAG pRMQTags )
{
   int i = 0;
   while( pRMQTags )
   {
      i++;
      printf("\n%d) iTag=%d ", i, pRMQTags->iTag);
      if( pRMQTags->pKey )
         printf("KEY=[%s]:%ld ", pRMQTags->pKey, pRMQTags->nKey);
      if( pRMQTags->pFor )
         printf("FOR=[%s]:%ld ", pRMQTags->pFor, pRMQTags->nFor);
      if( pRMQTags->fDescend )
         printf("DESCEND ");
      pRMQTags = pRMQTags->pNext;
   }
   printf("\n");fflush(stdout);
}
#endif

static void hb_rmqClearOrderInfo( LPDBORDERINFO pOrderInfo )
{
   if( pOrderInfo->itmOrder )
      hb_itemRelease( pOrderInfo->itmOrder );
   if( pOrderInfo->itmNewVal )
      hb_itemRelease( pOrderInfo->itmNewVal );
   if( pOrderInfo->itmResult )
      hb_itemRelease( pOrderInfo->itmResult );
}

static PHB_RMQTAG hb_rmqCreateTagList( AREAP pArea, const char * szAlias )
{
   PHB_RMQTAG pRMQTags = NULL, *pRMQTagPtr;
   DBORDERINFO OrderInfo;
   int iCount, i;

   pRMQTagPtr = &pRMQTags;

   memset( &OrderInfo, 0, sizeof( DBORDERINFO ) );
   OrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
   SELF_ORDINFO( pArea, DBOI_ORDERCOUNT, &OrderInfo );
   iCount = hb_itemGetNI( OrderInfo.itmResult );

   for( i = 1; i <= iCount; i++ )
   {
      OrderInfo.itmOrder = hb_itemPutNI( OrderInfo.itmOrder, i );
      OrderInfo.itmResult = hb_itemPutL( OrderInfo.itmResult, HB_FALSE );
      SELF_ORDINFO( pArea, DBOI_CUSTOM, &OrderInfo );
      if( ! hb_itemGetL( OrderInfo.itmResult ) )
      {
         SELF_ORDINFO( pArea, DBOI_UNIQUE, &OrderInfo );
         if( ! hb_itemGetL( OrderInfo.itmResult ) )
         {
            HB_BOOL fDesc, fDel;
            char * pszFor = NULL, *pszKey = NULL;
            HB_SIZE nFor = 0, nKey = 0, nLen;
            const char * szVal;
            char cType;

            SELF_ORDINFO( pArea, DBOI_ISDESC, &OrderInfo );
            fDesc = hb_itemGetL( OrderInfo.itmResult );

            OrderInfo.itmResult = hb_itemPutC( OrderInfo.itmResult, NULL );
            SELF_ORDINFO( pArea, DBOI_KEYTYPE, &OrderInfo );
            cType = hb_itemGetCPtr( OrderInfo.itmResult )[0];
            OrderInfo.itmResult = hb_itemPutC( OrderInfo.itmResult, NULL );
            SELF_ORDINFO( pArea, DBOI_CONDITION, &OrderInfo );
            szVal = hb_itemGetCPtr( OrderInfo.itmResult );
            nLen = hb_itemGetCLen( OrderInfo.itmResult );
            if( nLen && ! hb_strEmpty( szVal, nLen ) )
            {
               pszFor = hb_rmqStrip( szVal, nLen, szAlias, &nFor );
            }
            OrderInfo.itmResult = hb_itemPutC( OrderInfo.itmResult, NULL );
            SELF_ORDINFO( pArea, DBOI_EXPRESSION, &OrderInfo );
            szVal = hb_itemGetCPtr( OrderInfo.itmResult );
            nLen = hb_itemGetCLen( OrderInfo.itmResult );
            if( nLen && ! hb_strEmpty( szVal, nLen ) )
            {
               pszKey = hb_rmqStrip( szVal, nLen, szAlias, &nKey );
            }
            if( pszFor )
            {
               HB_BOOL fNeg = HB_FALSE;
               if( s_nDelExp == 0 )
               {
                  HB_SIZE nDelExp = 0;
                  char *pszExp = hb_rmqStrip( HB_RMQ_DELEXP, strlen( HB_RMQ_DELEXP ),
                                              szAlias, &nDelExp );
                  hb_strncpy( s_szDelExp, pszExp, sizeof( s_szDelExp ) - 1 );
                  s_nDelExp = nDelExp;
                  hb_xfree( pszExp );
               }
               fDel = hb_rmqCmp( pszFor, nFor, s_szDelExp, s_nDelExp, &fNeg, HB_FALSE ) &&
                      fNeg;
            }
            else
            {
               fDel = HB_FALSE;
            }
            *pRMQTagPtr = ( PHB_RMQTAG ) hb_xgrab( sizeof( HB_RMQTAG ) );
            (*pRMQTagPtr)->iTag     = i;
            (*pRMQTagPtr)->cType    = cType;
            (*pRMQTagPtr)->fDescend = fDesc;
            (*pRMQTagPtr)->pKey     = pszKey;
            (*pRMQTagPtr)->nKey     = nKey;
            (*pRMQTagPtr)->pFor     = pszFor;
            (*pRMQTagPtr)->nFor     = nFor;
            (*pRMQTagPtr)->fDelExp  = fDel;
            (*pRMQTagPtr)->pNext    = NULL;
            pRMQTagPtr = &( *pRMQTagPtr )->pNext;
         }
      }
   }

   hb_rmqClearOrderInfo( &OrderInfo );

   return pRMQTags;
}

static PHB_RMQTAG hb_rmqFindTag( PHB_RMQTAG pRMQTags, HB_BOOL * pfFor,
                                 const char * pszExpr, HB_SIZE nLen,
                                 HB_BOOL * pfNeg,
                                 PHB_ITEM pItem, int * piOper )
{
   HB_BOOL fNeg = HB_FALSE;

   while( pRMQTags )
   {
      if( pfFor )
      {
         if( pRMQTags->pFor )
         {
            if( hb_rmqCmp( pszExpr, nLen, pRMQTags->pFor, pRMQTags->nFor, &fNeg, HB_FALSE ) )
            {
               *pfFor = HB_TRUE;
               break;
            }
         }
         else if( pRMQTags->cType == 'L' )
         {
            if( hb_rmqCmp( pszExpr, nLen, pRMQTags->pKey, pRMQTags->nKey, &fNeg, HB_FALSE ) )
            {
               *pfFor = HB_FALSE;
               break;
            }
         }
         fNeg = HB_FALSE;
      }
      else if( pRMQTags->pFor )
      {
         if( ( hb_itemType( pItem ) & HB_IT_LOGICAL ) != 0 &&
             ( *piOper == HB_RMQ_EQU ||
               *piOper == HB_RMQ_NEQU ||
               *piOper == HB_RMQ_EEQU ||
               *piOper == HB_RMQ_NEEQU ) )
         {
            if( hb_rmqCmp( pszExpr, nLen, pRMQTags->pFor, pRMQTags->nFor, &fNeg, HB_FALSE ) )
            {
               if( !hb_itemGetL( pItem ) )
                  fNeg = !fNeg;
               if( *piOper == HB_RMQ_NEQU || *piOper == HB_RMQ_NEEQU )
                  fNeg = !fNeg;
               *piOper = HB_RMQ_TAG;
               break;
            }
            fNeg = HB_FALSE;
         }
      }
      else /* if( !pRMQTags->pFor || ( pRMQTags->fDelExp && hb_setGetDeleted() ) ) */
      {
         if( hb_rmqCmp( pszExpr, nLen, pRMQTags->pKey, pRMQTags->nKey, &fNeg, pRMQTags->cType == 'C' ) )
            break;
         fNeg = HB_FALSE;

         if( pItem && pRMQTags->cType == 'C' &&
             ( hb_itemType( pItem ) & HB_IT_DATETIME ) != 0 )
         {
            if( pRMQTags->nKey > 6 &&
                hb_rmqFuncCmp( pRMQTags->pKey, pRMQTags->nKey, "DTOS", 4,
                               pszExpr, nLen, &fNeg, HB_TRUE ) )
            {
               char szDate[ 9 ];
               hb_itemPutCL( pItem, hb_dateDecStr( szDate, hb_itemGetDL( pItem ) ), 8 );
               break;
            }
            fNeg = HB_FALSE;
         }
         else if( pItem && ( pRMQTags->cType == 'D' || pRMQTags->cType == 'T' ) &&
                  ( hb_itemGetCLen( pItem ) >= 8 ) != 0 )
         {
            if( nLen > 6 &&
                hb_rmqFuncCmp( pszExpr, nLen, "DTOS", 4,
                               pRMQTags->pKey, pRMQTags->nKey, &fNeg, HB_FALSE ) )
            {
               hb_itemPutDL( pItem, hb_dateEncStr( hb_itemGetCPtr( pItem ) ) );
               break;
            }
            fNeg = HB_FALSE;
         }
      }
      pRMQTags = pRMQTags->pNext;
   }
   if( pfNeg && pRMQTags )
      *pfNeg = fNeg;

   return pRMQTags;
}

static PHB_RMQSTACK hb_rmqStackCreate( AREAP pArea, PHB_RMQTAG pRMQTags,
                                       HB_ULONG ulRecords,
                                       char * pszExpr, HB_SIZE nLen )
{
   PHB_RMQSTACK pStack = ( PHB_RMQSTACK ) hb_xgrab( sizeof( HB_RMQSTACK ) );

   pStack->iPos = 0;
   pStack->iSize = HB_RMQ_STACKSIZE;
   pStack->pBase = ( PHB_RMQSTACKITM ) hb_xgrab( sizeof( HB_RMQSTACKITM ) *
                                                 HB_RMQ_STACKSIZE );
   pStack->pTags = pRMQTags;
   pStack->pszExpr = pszExpr;
   pStack->nExpr = nLen;
   pStack->ulRecs = ulRecords;
   pStack->pArea = pArea;

   return pStack;
}

static void hb_rmqStackPop( PHB_RMQSTACK pStack )
{
   if( pStack->iPos > 0 )
   {
      PHB_RMQSTACKITM pItm = &pStack->pBase[ --pStack->iPos ];

      if( pItm->pItmLo )
         hb_itemRelease( pItm->pItmLo );
      if( pItm->pItmHi )
         hb_itemRelease( pItm->pItmHi );
      if( pItm->pszExpr )
         hb_xfree( pItm->pszExpr );
   }
}

static void hb_rmqStackDestoy( PHB_RMQSTACK pStack )
{
   while( pStack->iPos > 0 )
   {
      hb_rmqStackPop( pStack );
   }
   hb_xfree( pStack->pBase );
   hb_rmqDestroyTagList( pStack->pTags );
   if( pStack->pszExpr )
      hb_xfree( pStack->pszExpr );
   hb_xfree( pStack );
}

static int hb_rmqStackOptLvl( PHB_RMQSTACK pStack, HB_BOOL fExp )
{
   int iOpt, iOpt1, iOpt2;

   if( pStack->iCur < pStack->iPos )
   {
      PHB_RMQSTACKITM pItm = &pStack->pBase[ pStack->iCur++ ];

      if( pItm->iOper == HB_RMQ_AND )
      {
         iOpt1 = hb_rmqStackOptLvl( pStack, fExp );
         iOpt2 = hb_rmqStackOptLvl( pStack, fExp );

         if( iOpt1 == RM_OPT_FULL && iOpt2 == RM_OPT_FULL )
            iOpt = RM_OPT_FULL;
         else if( iOpt1 == RM_OPT_NONE && iOpt2 == RM_OPT_NONE )
            iOpt = RM_OPT_NONE;
         else
            iOpt = RM_OPT_PART;
      }
      else if( pItm->iOper == HB_RMQ_OR )
      {
         iOpt1 = hb_rmqStackOptLvl( pStack, fExp );
         iOpt2 = hb_rmqStackOptLvl( pStack, fExp );

         if( iOpt1 == RM_OPT_FULL && iOpt2 == RM_OPT_FULL )
            iOpt = RM_OPT_FULL;
         else if( iOpt1 == RM_OPT_NONE || iOpt2 == RM_OPT_NONE )
            iOpt = RM_OPT_NONE;
         else
            iOpt = RM_OPT_PART;
      }
      else if( pItm->iOper == HB_RMQ_NOT )
      {
         iOpt = hb_rmqStackOptLvl( pStack, fExp );
         if( fExp && iOpt != RM_OPT_FULL )
            iOpt = RM_OPT_NONE;
      }
      else if( pItm->iOper == HB_RMQ_SKIP )
         iOpt = hb_rmqStackOptLvl( pStack, fExp );
      else if( pItm->pTag )
         iOpt = RM_OPT_FULL;
      else
         iOpt = RM_OPT_NONE;
      pItm->iOpt = iOpt;
   }
   else
   {
      iOpt = RM_OPT_NONE;
   }
   return iOpt;
}

static PHB_RMFILTER hb_rmqStackEval( PHB_RMQSTACK pStack )
{
   PHB_RMFILTER pRM = NULL, pRM2;

   if( pStack->iCur < pStack->iPos )
   {
      PHB_RMQSTACKITM pItm = &pStack->pBase[ pStack->iCur++ ];

#ifdef HB_RMQ_DBGDISP
      printf("\r\n(%d/%d) Eval: 0x%02x", pStack->iCur, pStack->iPos, pItm->iOper); fflush(stdout);
#endif

      if( pItm->iOpt == RM_OPT_NONE )
      {
         /* update the stack counter only */
         pStack->iCur--;
         hb_rmqStackOptLvl( pStack, HB_FALSE );
      }
      else if( pItm->iOper == HB_RMQ_AND )
      {
         pRM = hb_rmqStackEval( pStack );
         if( pRM )
         {
            pRM2 = hb_rmqStackEval( pStack );
            if( pRM2 )
               pRM = hb_rmAND( pRM, pRM2 );
            else
            {
               hb_rmDestroy( pRM );
               return NULL;
            }
         }
      }
      else if( pItm->iOper == HB_RMQ_OR )
      {
         pRM = hb_rmqStackEval( pStack );
         if( pRM )
         {
            pRM2 = hb_rmqStackEval( pStack );
            if( pRM2 )
               pRM = hb_rmOR( pRM, pRM2 );
            else
            {
               hb_rmDestroy( pRM );
               return NULL;
            }
         }
      }
      else if( pItm->iOper == HB_RMQ_NOT )
      {
         pRM = hb_rmqStackEval( pStack );
         if( !pRM )
            return NULL;
         pRM = hb_rmNOT( pRM );
      }
      else if( pItm->iOper == HB_RMQ_SKIP )
      {
         pRM = hb_rmqStackEval( pStack );
         if( !pRM )
            return NULL;
      }
      else if( pItm->pTag )
      {
         DBORDERINFO OrderInfo;

         pRM = hb_rmCreate( pStack->ulRecs );
         if( !pRM )
            return NULL;
         pRM2 = NULL;

         memset( &OrderInfo, 0, sizeof( DBORDERINFO ) );
         OrderInfo.itmOrder = hb_itemPutNI( NULL, pItm->pTag->iTag );
         if( pItm->pTag->fDescend )
         {
            OrderInfo.itmNewVal = hb_itemPutL( NULL, HB_FALSE );
            OrderInfo.itmResult = hb_itemPutL( NULL, HB_FALSE );
            SELF_ORDINFO( pStack->pArea, DBOI_ISDESC, &OrderInfo );
         }
#ifdef HB_RMQ_DBGDISP
         printf(" [TAG=%d <%s,%s>]", pItm->pTag->iTag,
                        hb_itemGetCPtr( pItm->pItmLo ),
                        hb_itemGetCPtr( pItm->pItmHi ) ); fflush(stdout);
#endif
         hb_rmSetLoHi( pStack->pArea, pRM, pItm->pItmLo, pItm->pItmHi,
                       OrderInfo.itmOrder, NULL );
         if( pItm->iOper == HB_RMQ_RNGEI || pItm->iOper == HB_RMQ_RNGEE )
         {
            if( !pRM2 )
            {
               pRM2 = hb_rmCreate( pStack->ulRecs );
               if( !pRM2 )
               {
                  hb_rmDestroy( pRM );
                  hb_rmqClearOrderInfo( &OrderInfo );
                  return NULL;
               }
            }
            hb_rmSetLoHi( pStack->pArea, pRM2, pItm->pItmLo, pItm->pItmLo,
                          OrderInfo.itmOrder, NULL );
         }
         if( pItm->iOper == HB_RMQ_RNGIE || pItm->iOper == HB_RMQ_RNGEE )
         {
            if( !pRM2 )
            {
               pRM2 = hb_rmCreate( pStack->ulRecs );
               if( !pRM2 )
               {
                  hb_rmDestroy( pRM );
                  hb_rmqClearOrderInfo( &OrderInfo );
                  return NULL;
               }
            }
            hb_rmSetLoHi( pStack->pArea, pRM2, pItm->pItmHi, pItm->pItmHi,
                          OrderInfo.itmOrder, NULL );
         }
         if( pRM2 )
         {
            pRM2 = hb_rmNOT( pRM2 );
            pRM = hb_rmAND( pRM, pRM2 );
         }
         if( pItm->pTag->fDescend )
         {
            OrderInfo.itmNewVal = hb_itemPutL( OrderInfo.itmNewVal, HB_TRUE );
            SELF_ORDINFO( pStack->pArea, DBOI_ISDESC, &OrderInfo );
         }
         hb_rmqClearOrderInfo( &OrderInfo );
#ifdef HB_RMQ_DBGDISP
         printf(" [%ld/", hb_rmCountRecords( pRM ) );
#endif
         if( pItm->fNeg )
         {
            pRM = hb_rmNOT( pRM );
         }
#ifdef HB_RMQ_DBGDISP
         printf("%ld]", hb_rmCountRecords( pRM ) ); fflush(stdout);
#endif
      }
   }

   if( !pRM )
   {
      pRM = hb_rmCreate( pStack->ulRecs );
      if( pRM )
         hb_rmFillMB( pRM );
   }
   return pRM;
}

static HB_BOOL hb_rmqStackDoOpt( HB_BOOL fTest, PHB_RMQSTACK pStack,
                                 PHB_RMQSTACKITM pWith, HB_BOOL fNeg )
{
   HB_BOOL fOpt = HB_FALSE, fOpt1, fOpt2;

   if( pStack->iCur < pStack->iPos )
   {
      PHB_RMQSTACKITM pItm = &pStack->pBase[ pStack->iCur++ ];

      if( pItm->iOper == HB_RMQ_AND )
      {
         if( !pWith && pStack->iCur < pStack->iPos )
         {
            if( hb_rmqCanBeJoined( &pStack->pBase[ pStack->iCur ] ) )
            {
               int iCur = pStack->iCur;
               pWith = &pStack->pBase[ pStack->iCur++ ];
               fOpt = hb_rmqStackDoOpt( HB_TRUE, pStack, pWith, HB_FALSE );
               if( fOpt && !fTest )
               {
                  pStack->iCur = iCur + 1;
                  fOpt = hb_rmqStackDoOpt( HB_FALSE, pStack, pWith, HB_FALSE );
                  pStack->pBase[ iCur - 1 ].iOper = HB_RMQ_SKIP;
                  pWith->iOper = HB_RMQ_SKIP;
               }
               pStack->iCur = iCur;
               pWith = NULL;
            }
         }
         fOpt1 = hb_rmqStackDoOpt( fTest, pStack, pWith, fNeg );
         fOpt2 = hb_rmqStackDoOpt( fTest, pStack, pWith, fNeg );
         fOpt = fOpt || fOpt1 || fOpt2;
      }
      else if( pItm->iOper == HB_RMQ_OR )
      {
         fOpt1 = hb_rmqStackDoOpt( fTest, pStack, pWith, fNeg );
         fOpt2 = hb_rmqStackDoOpt( fTest, pStack, pWith, fNeg );
         fOpt = fOpt1 && fOpt2;
      }
      else if( pItm->iOper == HB_RMQ_NOT )
      {
         fOpt = hb_rmqStackDoOpt( fTest, pStack, pWith, !fNeg );
      }
      else if( pItm->iOper == HB_RMQ_SKIP )
      {
         fOpt = hb_rmqStackDoOpt( fTest, pStack, pWith, fNeg );
      }
      else if( pWith && hb_rmqCanBeJoined( pItm ) )
      {
         fOpt = hb_rmqJoinOper( fTest, pItm, pWith, fNeg );
      }
   }
   return fOpt;
}

/* This is only added for SIX compatibility - I do not like it and
   such code will be buggy by definition for two reasons:
   1. In SIX only this part of expresion is later evaluated
      what can give wrong record set in network environemnt.
   2. It does not return the correct expression, f.e. we have
      database with fields ONE and TWO and index only on ONE.
      When we set filter expression to:
      "ONE='A' .AND. ( TWO='B' .OR. TWO='C' .OR. ( TWO='D' .AND. ONE='B' ) )"
      then SIX sets as non opitimized part only:
         "TWO='B'"
      when RMDBFCDX
         TWO='B'.OR.(TWO='C'.OR.TWO='D')
      better but in both cases it's wrong and full expression should be
      tested later.
   Update: I decided to make it valid and return the non optimized part
           ignoring SIX behavior at all.
 */
static char * hb_rmqNonOptExpr( PHB_RMQSTACK pStack, HB_SIZE *pnExp, HB_BOOL fAll )
{
   char  *pExp = NULL;
   char  *pExp1, *pExp2;
   HB_SIZE nExp1, nExp2, nLen = 0;

   if( pStack->iCur < pStack->iPos )
   {
      PHB_RMQSTACKITM pItm = &pStack->pBase[ pStack->iCur++ ];

      if( pItm->iOpt == RM_OPT_NONE )
      {
         fAll = HB_TRUE;
      }

      if( pItm->iOpt == RM_OPT_FULL && !fAll )
      {
         /* update the stack counter only */
         pStack->iCur--;
         hb_rmqStackOptLvl( pStack, HB_TRUE );
      }
      else if( pItm->iOper == HB_RMQ_AND )
      {
         pExp1 = hb_rmqNonOptExpr( pStack, &nExp1, fAll );
         pExp2 = hb_rmqNonOptExpr( pStack, &nExp2, fAll );
         if( pExp1 && pExp2 )
         {
            nLen = nExp1 + nExp2 + 7;
            pExp = ( char * ) hb_xgrab( nLen + 1 );
            pExp[ 0 ] = '(';
            memcpy( pExp + 1, pExp1, nExp1 );
            memcpy( pExp + 1 + nExp1, ".AND.", 5 );
            memcpy( pExp + 6 + nExp1, pExp2, nExp2 );
            pExp[ nLen - 1 ] = ')';
            hb_xfree( pExp1 );
            hb_xfree( pExp2 );
         }
         else if( pExp1 )
         {
            nLen = nExp1;
            pExp = pExp1;
         }
         else if( pExp2 )
         {
            nLen = nExp2;
            pExp = pExp2;
         }
      }
      else if( pItm->iOper == HB_RMQ_OR )
      {
         pExp1 = hb_rmqNonOptExpr( pStack, &nExp1, fAll );
         pExp2 = hb_rmqNonOptExpr( pStack, &nExp2, fAll );
         if( pExp1 && pExp2 )
         {
            nLen = nExp1 + nExp2 + 6;
            pExp = ( char * ) hb_xgrab( nLen + 1 );
            pExp[ 0 ] = '(';
            memcpy( pExp + 1, pExp1, nExp1 );
            memcpy( pExp + 1 + nExp1, ".OR.", 4 );
            memcpy( pExp + 5 + nExp1, pExp2, nExp2 );
            pExp[ nLen - 1 ] = ')';
            hb_xfree( pExp1 );
            hb_xfree( pExp2 );
         }
         else if( pExp1 )
         {
            nLen = nExp1;
            pExp = pExp1;
         }
         else if( pExp2 )
         {
            nLen = nExp2;
            pExp = pExp2;
         }
      }
      else if( pItm->iOper == HB_RMQ_NOT )
      {
         pExp1 = hb_rmqNonOptExpr( pStack, &nExp1, fAll );
         if( pExp1 )
         {
            nLen = nExp1 + 3;
            pExp = ( char * ) hb_xgrab( nLen + 1 );
            pExp[ 0 ] = '!';
            pExp[ 1 ] = '(';
            memcpy( pExp + 2, pExp1, nExp1 );
            pExp[ nLen - 1 ] = ')';
            hb_xfree( pExp1 );
         }
      }
      else if( pItm->iOper == HB_RMQ_SKIP )
      {
         pExp = hb_rmqNonOptExpr( pStack, &nLen, fAll );
      }
      else if( pItm->pTag && !fAll )
      {
         ;
      }
      else if( pItm->pszExpr && pItm->nExpr )
      {
         nLen = pItm->nExpr;
         pExp = ( char * ) hb_xgrab( nLen + 1 );
         memcpy( pExp, pItm->pszExpr, nLen );
      }
      else
      {
      }
   }
   *pnExp = nLen;
   if( pExp )
      pExp[ nLen ] = '\0';
   return pExp;
}

static void hb_rmqStackInit( PHB_RMQSTACK pStack )
{
   pStack->iCur = 0;
}


#if defined( HB_RMQ_DBGDISP ) || defined( HB_RMQ_DEBUG )
static void hb_rmqNonOptExprDsp( PHB_RMQSTACK pStack )
{
   char *pExpr;
   HB_SIZE nLen = 0;
   hb_rmqStackInit( pStack );
   pExpr = hb_rmqNonOptExpr( pStack, &nLen, HB_FALSE );
   printf("\n>>");
   printf("[%s][%ld]", pExpr, nLen);
   printf("<<\n");
   fflush(stdout);
   hb_xfree(pExpr);
}

static void hb_rmqStackDsp( PHB_RMQSTACK pStack )
{
   if( pStack->iCur < pStack->iPos )
   {
      PHB_RMQSTACKITM pItm = &pStack->pBase[ pStack->iCur++ ];
      if( pItm->iOper == HB_RMQ_AND )
      {
         printf("( ");
         hb_rmqStackDsp( pStack );
         printf(" .AND. ");
         hb_rmqStackDsp( pStack );
         printf(")");
      }
      else if( pItm->iOper == HB_RMQ_OR )
      {
         printf("( ");
         hb_rmqStackDsp( pStack );
         printf(" .OR. ");
         hb_rmqStackDsp( pStack );
         printf(" )");
      }
      else if( pItm->iOper == HB_RMQ_NOT )
      {
         printf(".NOT. ( ");
         hb_rmqStackDsp( pStack );
         printf(" )");
      }
      else if( pItm->iOper == HB_RMQ_SKIP )
      {
         hb_rmqStackDsp( pStack );
      }
      else if( pItm->pTag && pItm->fNeg &&
                ( pItm->iOper == HB_RMQ_RNGII ||
                  pItm->iOper == HB_RMQ_RNGEI ||
                  pItm->iOper == HB_RMQ_RNGIE ||
                  pItm->iOper == HB_RMQ_RNGEE ) )
      {
         pItm->fNeg = HB_FALSE;
         printf(".NOT. ( ");
         pStack->iCur--;
         hb_rmqStackDsp( pStack );
         printf(" )");
         pItm->fNeg = HB_TRUE;
      }
      else if( pItm->iOper == HB_RMQ_RANGE )
      {
         printf( "%s>='%s' .and. %s<='%s'", pItm->pTag->pKey, hb_itemGetCPtr( pItm->pItmLo ),
                                            pItm->pTag->pKey, hb_itemGetCPtr( pItm->pItmHi ) );
      }
      else if( pItm->iOper == HB_RMQ_RNGEI )
      {
         printf( "%s>'%s' .and. %s<='%s'", pItm->pTag->pKey, hb_itemGetCPtr( pItm->pItmLo ),
                                           pItm->pTag->pKey, hb_itemGetCPtr( pItm->pItmHi ) );
      }
      else if( pItm->iOper == HB_RMQ_RNGIE )
      {
         printf( "%s>='%s' .and. %s<'%s'", pItm->pTag->pKey, hb_itemGetCPtr( pItm->pItmLo ),
                                           pItm->pTag->pKey, hb_itemGetCPtr( pItm->pItmHi ) );
      }
      else if( pItm->iOper == HB_RMQ_RNGEE )
      {
         printf( "%s>'%s' .and. %s<'%s'", pItm->pTag->pKey, hb_itemGetCPtr( pItm->pItmLo ),
                                          pItm->pTag->pKey, hb_itemGetCPtr( pItm->pItmHi ) );
      }
      else if( pItm->pszExpr )
      {
         printf( "<%s>[%d,%d,%d,%s,%s]", pItm->pszExpr,
            pItm->pTag != NULL, pItm->fNeg, pItm->iOper,
            hb_itemGetCPtr( pItm->pItmLo ), hb_itemGetCPtr( pItm->pItmHi ) );
      }
   }
}

static void hb_rmqStackDisplay( PHB_RMQSTACK pStack )
{
   int iOpt;

   printf("\n>>");
   hb_rmqStackInit( pStack );
   hb_rmqStackDsp( pStack );
   printf("<< ");
   hb_rmqStackInit( pStack );
   iOpt = hb_rmqStackOptLvl( pStack, HB_FALSE );
   printf("[%d]\n", iOpt );
   fflush(stdout);
}
#endif

static void hb_rmqStackPush( PHB_RMQSTACK pStack, int iOper, HB_BOOL fNeg,
                             PHB_RMQTAG pTag, PHB_ITEM pItem,
                             char * pszExpr, HB_SIZE nLen )
{
   PHB_RMQSTACKITM pItm;

   if( pTag && pStack->iPos > 0 &&
        pStack->pBase[ pStack->iPos - 1 ].iOper == HB_RMQ_NOT )
   {
/*
      hb_rmqStackPop( pStack );
      fNeg = !fNeg;
*/
   }

   if( pStack->iPos == pStack->iSize )
   {
      pStack->iSize += HB_RMQ_STACKSIZE;
      pStack->pBase = ( PHB_RMQSTACKITM ) hb_xrealloc( pStack->pBase,
                                    sizeof( HB_RMQSTACKITM ) * pStack->iSize );
   }
   pItm = &pStack->pBase[ pStack->iPos++ ];
   pItm->iOper = iOper;
   pItm->iOpt = RM_OPT_FULL;
   pItm->fNeg = fNeg;
   pItm->pItmLo = pItm->pItmHi = NULL;
   pItm->pTag = pTag;
   if( pItem )
   {
      switch( iOper )
      {
         case HB_RMQ_LT:
            pItm->fNeg = !pItm->fNeg;
            pItm->iOper = HB_RMQ_GE;
         case HB_RMQ_GE:
            pItm->pItmLo = pItem;
            break;

         case HB_RMQ_GT:
            pItm->fNeg = !pItm->fNeg;
            pItm->iOper = HB_RMQ_LE;
         case HB_RMQ_LE:
            pItm->pItmHi = pItem;
            break;

         case HB_RMQ_NEQU:
            pItm->fNeg = !pItm->fNeg;
            pItm->iOper = HB_RMQ_EQU;
         case HB_RMQ_EQU:
            pItm->pItmLo = pItem;
            pItm->pItmHi = hb_itemNew( pItm->pItmLo );
            break;

         case HB_RMQ_NEEQU:
            pItm->fNeg = !pItm->fNeg;
            pItm->iOper = HB_RMQ_EEQU;
         case HB_RMQ_EEQU:
            pItm->pItmLo = pItem;
            pItm->pItmHi = hb_itemNew( pItm->pItmLo );
            break;

         default:
            hb_itemRelease( pItem );
            pItm->pTag = NULL;
            break;
      }
   }
   if( pszExpr )
   {
      pItm->pszExpr = ( char * ) hb_xgrab( nLen + 1 );
      pItm->nExpr = nLen;
      memcpy( pItm->pszExpr, pszExpr, nLen );
      pItm->pszExpr[ nLen ] = '\0';
   }
   else
   {
      pItm->pszExpr = NULL;
      pItm->nExpr = 0;
   }

   return;
}

static void hb_rmqStackPushNot( PHB_RMQSTACK pStack )
{
   if( pStack->iPos > 0 &&
        pStack->pBase[ pStack->iPos - 1 ].iOper == HB_RMQ_NOT )
      hb_rmqStackPop( pStack );
   else
      hb_rmqStackPush( pStack, HB_RMQ_NOT, HB_FALSE, NULL, NULL, NULL, 0 );
}

static void hb_rmqStackPushOper( PHB_RMQSTACK rmqStack, int iOper )
{
   hb_rmqStackPush( rmqStack, iOper, HB_FALSE, NULL, NULL, NULL, 0 );
}

static void hb_rmqDivide( char * pszExpr, HB_SIZE nLen, PHB_RMQSTACK rmqStack )
{
   PHB_RMQTAG pTag;
   HB_BOOL fNeg, fFor = HB_FALSE;
   int iOper;
   HB_SIZE n, nExp;

   pTag = hb_rmqFindTag( rmqStack->pTags, &fFor, pszExpr, nLen, &fNeg, NULL, NULL );
   if( pTag )
   {
      if( fFor )
      {
         if( fNeg )
            hb_rmqStackPushNot( rmqStack );
         hb_rmqStackPush( rmqStack, HB_RMQ_TAG, HB_FALSE, pTag, NULL, pszExpr, nLen );
      }
      else
      {
         hb_rmqStackPush( rmqStack, HB_RMQ_EQU, HB_FALSE, pTag,
                          hb_itemPutL( NULL, !fNeg ), pszExpr, nLen );
      }
      return;
   }

#ifdef HB_RMQ_DBGDISP
   {
      char b = pszExpr[ nLen ];
      pszExpr[ nLen ] = '\0';
      printf("\n[%s]", pszExpr);fflush(stdout);
      pszExpr[ nLen ] = b;
   }
#endif

   iOper = HB_RMQ_OR;
   n = hb_rmqNextExp( pszExpr, nLen, &iOper, &nExp );
#ifdef HB_RMQ_DBGDISP
   printf("[%ld,%d]\n", n, iOper);fflush(stdout);
#endif
   if( n == 0 )
   {
      if( pszExpr[ 0 ] == '!' )
      {
         hb_rmqStackPushNot( rmqStack );
         pszExpr++;
         nLen--;
      }
      n = hb_rmqStripBraces( pszExpr, nLen );
      if( n > 0 )
      {
         hb_rmqDivide( &pszExpr[ n ], nLen - ( n << 1 ), rmqStack );
      }
      else
      {
         PHB_ITEM pItem = NULL;

         pTag = NULL;
         n = hb_rmqOper( pszExpr, nLen, &iOper, &nExp );
         if( n == 0 )
         {
            pTag = hb_rmqFindTag( rmqStack->pTags, &fFor, pszExpr, nLen, &fNeg, NULL, NULL );
         }
         else
         {
#ifdef HB_RMQ_DBGDISP
            {
               char b = pszExpr[ nExp ], b2 = pszExpr[ nLen ];
               pszExpr[ nExp ] = '\0';
               pszExpr[ nLen ] = '\0';
               printf("\n[%ld:%ld:%ld]<%s><%s>", nExp, n, nLen,
                      pszExpr, &pszExpr[ n ] );fflush(stdout);
               pszExpr[ nExp ] = b;
               pszExpr[ nLen ] = b2;
            }
#endif
            pItem = hb_rmqMacroEval( &pszExpr[ n ], nLen - n );
#ifdef HB_RMQ_DBGDISP
            printf("[%s]", pItem ? hb_itemTypeStr( pItem ) : "U" );fflush(stdout);
#endif
            if( pItem )
            {
               pTag = hb_rmqFindTag( rmqStack->pTags, NULL, pszExpr, nExp, &fNeg, pItem, &iOper );
#ifdef HB_RMQ_DBGDISP
               printf(" %d\n", pTag ? pTag->iTag : 0 );fflush(stdout);
#endif
            }
         }
         if( pTag )
         {
            if( n == 0 )
            {
               if( fFor )
                  hb_rmqStackPush( rmqStack, HB_RMQ_TAG, fNeg, pTag, NULL, pszExpr, nLen );
               else
                  hb_rmqStackPush( rmqStack, HB_RMQ_EQU, HB_FALSE, pTag,
                                   hb_itemPutL( NULL, !fNeg ), pszExpr, nLen );
            }
            else if( iOper == HB_RMQ_TAG )
            {
               if( fNeg )
                  hb_rmqStackPushNot( rmqStack );
               hb_rmqStackPush( rmqStack, HB_RMQ_TAG, HB_FALSE, pTag, NULL, pszExpr, nLen );
            }
            else
            {
               hb_rmqStackPush( rmqStack, iOper, fNeg, pTag, pItem, pszExpr, nLen );
               pItem = NULL;
            }
         }
         else
         {
            hb_rmqStackPush( rmqStack, HB_RMQ_NONE, HB_FALSE, NULL, NULL, pszExpr, nLen );
         }
         if( pItem )
            hb_itemRelease( pItem );
      }
   }
   else
   {
      hb_rmqStackPushOper( rmqStack, iOper );
      hb_rmqDivide( pszExpr, nExp, rmqStack );
      hb_rmqDivide( &pszExpr[ n ], nLen - n, rmqStack );
   }
}

static PHB_RMQSTACK hb_rmqBuildStack( AREAP pArea, PHB_ITEM pQuery )
{
   PHB_RMQSTACK pStack = NULL;

   if( hb_itemGetCLen( pQuery ) > 0 )
   {
      char szAliasBuf[ HB_RDD_MAX_ALIAS_LEN + 1 ], * szAlias;
      PHB_RMQTAG pRMQTags;
      HB_ULONG ulRecords;

      if( SELF_ALIAS( ( AREAP ) pArea, szAliasBuf ) == HB_FAILURE ||
          SELF_RECCOUNT( ( AREAP ) pArea, &ulRecords ) == HB_FAILURE )
         return NULL;

      szAlias = *szAliasBuf ? szAliasBuf : NULL;

      pRMQTags = hb_rmqCreateTagList( pArea, szAlias );
      if( pRMQTags )
      {
         PHB_ITEM pExpr = hb_itemNew( pQuery );
         char * pszExpr;
         HB_SIZE nLen;

         hb_macroTextValue( pExpr );
         nLen = hb_itemGetCLen( pExpr );
         pszExpr = hb_rmqStrip( hb_itemGetCPtr( pExpr ), nLen, szAlias, &nLen );
         hb_itemRelease( pExpr );
         if( pszExpr )
         {
            pStack = hb_rmqStackCreate( pArea, pRMQTags, ulRecords, pszExpr, nLen );
#ifdef HB_RMQ_DBGDISP
            hb_rmqDispTagList( pRMQTags );
#endif
            hb_rmqDivide( pszExpr, nLen, pStack );
#ifdef HB_RMQ_DBGDISP
            hb_rmqStackDisplay( pStack );
#endif
         }

         if( !pStack )
         {
            hb_rmqDestroyTagList( pRMQTags );
         }
      }
   }

   return pStack;
}

int hb_rmqOptLevel( AREAP pArea, PHB_ITEM pQuery )
{
   int iOptLvl = RM_OPT_NONE;
   PHB_RMQSTACK pStack = hb_rmqBuildStack( pArea, pQuery );

   if( pStack )
   {
      hb_rmqStackInit( pStack );
      iOptLvl = hb_rmqStackOptLvl( pStack, HB_FALSE );
      hb_rmqStackDestoy( pStack );
   }

   return iOptLvl;
}

PHB_ITEM hb_rmqNonOptExpression( AREAP pArea, PHB_ITEM pQuery )
{
   int iOptLvl = RM_OPT_NONE;
   PHB_RMQSTACK pStack = hb_rmqBuildStack( pArea, pQuery );
   char * pNExpr = NULL;
   HB_SIZE nNExpr = 0;

   if( pStack )
   {
      hb_rmqStackInit( pStack );
      iOptLvl = hb_rmqStackOptLvl( pStack, HB_TRUE );

      if( iOptLvl == RM_OPT_PART )
      {
         hb_rmqStackInit( pStack );
         pNExpr = hb_rmqNonOptExpr( pStack, &nNExpr, HB_FALSE );
      }
      hb_rmqStackDestoy( pStack );
   }
   if( pNExpr )
   {
      return hb_itemPutCLPtr( NULL, pNExpr, nNExpr );
   }
   else if( iOptLvl == RM_OPT_NONE )
   {
      return hb_itemNew( pQuery );
   }
   return NULL;
}

PHB_RMFILTER hb_rmqBuildQRM( AREAP pArea, PHB_ITEM pQuery )
{
   int iOptLvl = RM_OPT_NONE;
   PHB_RMQSTACK pStack = hb_rmqBuildStack( pArea, pQuery );
   PHB_RMFILTER pRM = NULL;
   char * pNExpr = NULL;
   HB_SIZE nNExpr = 0;

   if( pStack )
   {
      hb_rmqStackInit( pStack );
      iOptLvl = hb_rmqStackOptLvl( pStack, HB_FALSE );

      if( iOptLvl != RM_OPT_NONE )
      {
         hb_rmqStackInit( pStack );
         iOptLvl = hb_rmqStackOptLvl( pStack, HB_TRUE );
         if( iOptLvl == RM_OPT_PART )
         {
            hb_rmqStackInit( pStack );
            pNExpr = hb_rmqNonOptExpr( pStack, &nNExpr, HB_FALSE );
         }

         hb_rmqStackInit( pStack );
         hb_rmqStackDoOpt( HB_FALSE, pStack, NULL, HB_FALSE);

         hb_rmqStackInit( pStack );
         hb_rmqStackOptLvl( pStack, HB_FALSE );

         hb_rmqStackInit( pStack );
         pRM = hb_rmqStackEval( pStack );
      }
      hb_rmqStackDestoy( pStack );
   }

   if( ! pRM )
   {
      HB_ULONG ulRecords;

      if( SELF_RECCOUNT( ( AREAP ) pArea, &ulRecords ) == HB_SUCCESS )
      {
         pRM = hb_rmCreate( ulRecords );
         if( pRM )
            hb_rmFillMB( pRM );
      }
   }
   if( pRM )
   {
      pRM->iOptLvl = iOptLvl;
      if( pRM->pExpr == NULL )
         pRM->pExpr = hb_itemNew( pQuery );
      else
         hb_itemCopy( pRM->pExpr, pQuery );
      if( iOptLvl != RM_OPT_FULL )
      {
         if( pNExpr )
         {
            pRM->pNonExpr = hb_itemPutCLPtr( pRM->pNonExpr, pNExpr, nNExpr );
            pNExpr = NULL;
         }
         else if( pRM->pNonExpr == NULL )
            pRM->pNonExpr = hb_itemNew( pQuery );
         else
            hb_itemCopy( pRM->pNonExpr, pQuery );
      }
      else if( pRM->pNonExpr != NULL )
      {
         hb_itemPutC( pRM->pNonExpr, NULL );
      }
   }
   if( pNExpr )
      hb_xfree( pNExpr );

   return pRM;
}

#ifdef HB_RMQ_DEBUG
HB_FUNC( RMQ_STRIPEXP )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   if( pArea && hb_parclen( 1 ) )
   {
      PHB_RMQSTACK pStack = hb_rmqBuildStack( pArea, hb_param( 1, HB_IT_STRING ) );

      if( pStack )
      {
         hb_retclen( pStack->pszExpr, pStack->nExpr );
         hb_rmqStackDestoy( pStack );
      }
      else
         hb_retc_null();
   }
   else
      hb_retc_null();
}

HB_FUNC( RMQ_OPTLEVEL )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   int iOptLvl = RM_OPT_NONE;

   if( pArea && hb_parclen( 1 ) )
   {
      iOptLvl = hb_rmqOptLevel( pArea, hb_param( 1, HB_IT_STRING ) );
   }
   hb_retni( iOptLvl );
}

HB_FUNC( RMQ_ISOPT )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL fOpt = HB_FALSE;

   if( pArea && hb_parclen( 1 ) )
   {
      PHB_RMQSTACK pStack = hb_rmqBuildStack( pArea, hb_param( 1, HB_IT_STRING ) );

      if( pStack )
      {
         hb_rmqStackInit( pStack );
         fOpt = hb_rmqStackDoOpt( HB_TRUE, pStack, NULL, HB_FALSE );
         hb_rmqStackDestoy( pStack );
      }
   }

   hb_retl( fOpt );
}

HB_FUNC( RMQ_DISPEXP )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_BOOL fOpt = HB_FALSE;

   if( pArea && hb_parclen( 1 ) )
   {
      PHB_RMQSTACK pStack = hb_rmqBuildStack( pArea, hb_param( 1, HB_IT_STRING ) );

      if( pStack )
      {
         hb_rmqStackDisplay( pStack );
         hb_rmqStackInit( pStack );
         fOpt = hb_rmqStackDoOpt( HB_FALSE, pStack, NULL, HB_FALSE );
         hb_rmqStackDisplay( pStack );
         hb_rmqStackDestoy( pStack );
      }
   }

   hb_retl( fOpt );
}
#endif

/*
 * TODO:
 * 1. Some function detections in L-value like DTOS, DTOC, YEAR
 * 2. Optimize the stack before evaluation
 * 3. Optimize for noninclusive ranges: a > x .and. a < y
 * 4. The error system sth like m6_error() to report different errors
 *    and early expression type checking
 *    Should I optimize UDFs R-value??? - now it's done.
 * 5. Check if the full expression is valid logical expression
 *
 *
 * 1 are IMHO necessary and I'll do it ASAP
 * 2-3 are for speed improvement
 * 4 mostly for future SIX m6_error() compatibility.
 */
