/*
 * $Id$
 */

/*
  (c) copyright xHarbour.com Inc. http://www.xHarbour.com
  Author: Przemyslaw Czerpak Przemek@xHarbour.com

  This source file is an intellectual property of xHarbour.com Inc.
  You may NOT forward or share this file under any conditions!
*/

#include "hbrddrm.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapierr.h"
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
   BOOL     fDescend;         /* descending order */
   BYTE *   pKey;             /* the stripped tag expression */
   ULONG    ulKey;            /* expression length */
   BYTE *   pFor;             /* the stripped FOR expression */
   ULONG    ulFor;            /* FOR expression length */
   BOOL     fDelExp;          /* FOR is '!DELETED()' exp */
   struct _HB_RMQTAG * pNext; /* pointer to next expression */
} HB_RMQTAG;
typedef HB_RMQTAG * PHB_RMQTAG;

typedef struct _HB_RMQSTACKITM
{
   int            iOper;      /* type of operation HB_RMQ_* */
   int            iOpt;       /* optimization level */
   BOOL           fNeg;       /* neg the operation */
   PHB_ITEM       pItmLo;     /* index low value */
   PHB_ITEM       pItmHi;     /* index high value */
   PHB_RMQTAG     pTag;       /* the Tag */
   BYTE *         byExpr;     /* the expression */
   ULONG          ulExpr;     /* length of expression */
} HB_RMQSTACKITM;
typedef HB_RMQSTACKITM * PHB_RMQSTACKITM;

typedef struct _HB_RMQSTACK
{
   int               iSize;   /* stack size */
   int               iPos;    /* current stack pos */
   int               iCur;    /* execute stack pos */
   PHB_RMQSTACKITM   pBase;   /* the real stack items */
   PHB_RMQTAG        pTags;   /* list of area tags */
   BYTE *            byExpr;  /* the expression */
   ULONG             ulExpr;  /* length of expression */
   AREAP             pArea;   /* work area pointer */
   ULONG             ulRecs;  /* number of records in work area */
   BOOL              fNExpr;  /* save non optimizable expression */
} HB_RMQSTACK;
typedef HB_RMQSTACK * PHB_RMQSTACK;

static BYTE s_byDelExp[ 10 ];
static ULONG s_ulDelExp = 0;

/*
 * evaluate macro string, return the ITEM result or NULL on error
 */
static PHB_ITEM hb_rmqMacroEval( BYTE * byMacro, ULONG ulLen )
{
   int iCurrArea = hb_rddGetCurrentWorkAreaNumber();
   PHB_ITEM pItem = hb_itemNew( NULL );
   char *cType;

   hb_itemPutCL( pItem, ( char * ) byMacro, ulLen );
   hb_rddSelectWorkAreaNumber( 0 );
#ifdef __XHARBOUR__
   cType = hb_macroGetType( pItem, 0 );
#else
   cType = hb_macroGetType( pItem );
#endif
   if( *cType == 'U' && *( cType + 1 ) != 'I' )
   {
      hb_itemRelease( pItem );
      pItem = NULL;
   }
   else
   {
      hb_vmPush( pItem );
      hb_macroGetValue( hb_stackItemFromTop( -1 ), 0, 0 );
      hb_itemCopy( pItem, hb_stackItemFromTop( -1 ) );
      hb_stackPop();
   }
   hb_rddSelectWorkAreaNumber( iCurrArea );
   return pItem;
}

static ULONG hb_rmqCloseBrace( BYTE * byExpr, ULONG ulLen )
{
   BYTE bQuote = 0, b;
   ULONG ul = 0;
   int iB = 0;

   if( ulLen == 0 )
      return 0;

   do
   {
      b = byExpr[ ul ];

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
      ul++;
   }
   while( ul < ulLen && iB );

   if( iB )
      ul = 0;

   return ul;
}

static ULONG hb_rmqStripBraces( BYTE * byExpr, ULONG ulLen )
{
   ULONG ul = 0, l;
   while( ( ul << 1 ) + 1 < ulLen )
   {
      l = ulLen - ( ul << 1 );
      if( hb_rmqCloseBrace( &byExpr[ ul ], l ) == l )
         ++ul;
      else
         break;
   }
   return ul;
}

static ULONG hb_rmqNextExp( BYTE * byExpr, ULONG ulLen,
                            int * piOper, ULONG * ulExp )
{
   BYTE bQuote = 0, b;
   ULONG ul = 0, ulLast = 0, ulLast2 = 0;
   int iOper, iLastOp = HB_RMQ_NONE;

   iOper = piOper ? *piOper : HB_RMQ_NONE;

   while( ul < ulLen )
   {
      b = byExpr[ ul ];

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
            ULONG l = hb_rmqCloseBrace( &byExpr[ ul ], ulLen - ul );

            if( l == 0 )
            {
               break;
            }
            ul += l - 1;
         }
         else if( b == '.' )
         {
            if( ulLen - ul >= 5 && hb_strnicmp( ( char * ) &byExpr[ ul ], ".AND.", 5 ) == 0 )
            {
               if( iLastOp == HB_RMQ_NONE )
               {
                  iLastOp = HB_RMQ_AND;
                  ulLast = ul + 5;
                  ulLast2 = ul;
               }
               ul += 5;
               if( iOper == HB_RMQ_NONE || iOper == HB_RMQ_AND )
                  break;
               continue;
            }
            else if( ulLen - ul >= 4 && hb_strnicmp( ( char * ) &byExpr[ ul ], ".OR.", 4 ) == 0 )
            {
               if( iLastOp == HB_RMQ_NONE )
               {
                  iLastOp = HB_RMQ_OR;
                  ulLast = ul + 4;
                  ulLast2 = ul;
               }
               ul += 4;
               if( iOper == HB_RMQ_NONE || iOper == HB_RMQ_OR )
                  break;
               continue;
            }
         }
      }
      ul++;
   }

   if( piOper )
      *piOper = iLastOp;

   if( ulExp )
      *ulExp = ulLast2;

   return ulLast;
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

static BOOL hb_rmqCanBeJoined( PHB_RMQSTACKITM pOp )
{
   BOOL fCan = FALSE;

   if( pOp->pTag )
   {
      switch( pOp->iOper )
      {
         case HB_RMQ_LT:
         case HB_RMQ_GT:
         case HB_RMQ_LE:
         case HB_RMQ_GE:
            fCan = TRUE;
            break;
      }
   }
   return fCan;
}

static BOOL hb_rmqJoinOper( BOOL fTest, PHB_RMQSTACKITM pOp1,
                                        PHB_RMQSTACKITM pOp2, BOOL fNeg )
{
   BOOL fJoined = FALSE;

   if( hb_rmqCanBeJoined( pOp1 ) && hb_rmqCanBeJoined( pOp2 ) &&
       pOp1->pTag->iTag == pOp2->pTag->iTag )
   {
      BOOL fCan = TRUE;
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
            fCan = FALSE;
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
            fCan = FALSE;
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
            fJoined = TRUE;
         }
      }
   }

   return fJoined;
}

#endif

static ULONG hb_rmqOper( BYTE * byExpr, ULONG ulLen,
                         int * piOper, ULONG * ulExp )
{
   BYTE bQuote = 0, b, bLast = 0;
   ULONG ul = 0;
   int iOper = HB_RMQ_NONE;

   while( ul < ulLen )
   {
      if( ulExp )
         *ulExp = ul;

      b = byExpr[ ul ];

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
            ULONG l = hb_rmqCloseBrace( &byExpr[ ul ], ulLen - ul );

            if( l == 0 )
            {
               ul = 0;
               break;
            }
            ul += l - 1;
         }
         else if( b == '.' )
         {
            if( ulLen - ul >= 5 && hb_strnicmp( ( char * ) &byExpr[ ul ], ".AND.", 5 ) == 0 )
            {
               iOper = HB_RMQ_AND;
               ul += 5;
               break;
            }
            else if( ulLen - ul >= 4 && hb_strnicmp( ( char * ) &byExpr[ ul ], ".OR.", 4 ) == 0 )
            {
               iOper = HB_RMQ_OR;
               ul += 4;
               break;
            }
         }
         else if( b == '=' )
         {
            if( ul == 0 || bLast != ':' )
            {
               if( ulLen - ul >= 1 && byExpr[ ul + 1 ] == '=' )
               {
                  iOper = HB_RMQ_EEQU;
                  ul += 2;
               }
               else
               {
                  iOper = HB_RMQ_EQU;
                  ul++;
               }
               break;
            }
         }
         else if( b == '>' )
         {
            if( ulLen - ul >= 1 && byExpr[ ul + 1 ] == '=' )
            {
               iOper = HB_RMQ_GE;
               ul += 2;
            }
            else
            {
               iOper = HB_RMQ_GT;
               ul++;
            }
            break;
         }
         else if( b == '<' )
         {
            if( ulLen - ul >= 1 && byExpr[ ul + 1 ] == '=' )
            {
               iOper = HB_RMQ_LE;
               ul += 2;
            }
            else if( ulLen - ul >= 1 && byExpr[ ul + 1 ] == '>' )
            {
               iOper = HB_RMQ_NEQU;
               ul += 2;
            }
            else
            {
               iOper = HB_RMQ_LT;
               ul++;
            }
            break;
         }
      }
      bLast = b;
      ul++;
   }

   if( piOper )
      *piOper = iOper;

   return ( ul == ulLen ) ? 0 : ul;
}

static void hb_rmqStripExp( BYTE ** byExpr, ULONG *ulLen, BOOL *fNeg )
{
   ULONG ul;

   do
   {
      ul = hb_rmqStripBraces( *byExpr, *ulLen );
      if( ul > 0 )
      {
         *byExpr += ul;
         *ulLen -= ( ul << 1 );
      }
      else if( (*byExpr)[ 0 ] == '!' &&
               hb_rmqNextExp( *byExpr, *ulLen, NULL, NULL ) == 0 )
      {
         *fNeg = !*fNeg;
         ul = 1;
         (*byExpr)++;
         (*ulLen)--;
      }
   }
   while( ul > 0 );
}

static BOOL hb_rmqIsAbbFunc( BYTE * byExpr, int iLen, int *iNewLen )
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
         while( i < iLen && functions[ iFunc ][ i ] == byExpr[ i ] )
            i++;
         if( byExpr[ i ] == '(' )
         {
            *iNewLen = 4;
            return TRUE;
         }
      }
   }
   return FALSE;
}

/*
 * compare two stripped expressions, it's like normal memcmp but
 * it ignores different quotas so expressions:
 * "F1='ABC'", 'F1="ABC"' and "F1=[ABC]" are equal
 */
static BOOL hb_rmqCmp( BYTE * bySrcExpr, ULONG ulSrcLen,
                       BYTE * byDstExpr, ULONG ulDstLen, BOOL *fNeg, BOOL fStr )
{
   BYTE bQuote1 = 0, bQuote2 = 0, b1, b2;
   ULONG ul;

   hb_rmqStripExp( &bySrcExpr, &ulSrcLen, fNeg );
   hb_rmqStripExp( &byDstExpr, &ulDstLen, fNeg );

   if( ulSrcLen != ulDstLen )
   {
      if( !fStr || ulSrcLen > ulDstLen || byDstExpr[ ulSrcLen ] != '+' )
         return FALSE;
   }

   for( ul = 0; ul < ulSrcLen; ul++ )
   {
      b1 = bySrcExpr[ ul ];
      b2 = byDstExpr[ ul ];

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
         return FALSE;
   }

   return TRUE;
}

/*
 * strip spaces and FIELD->, _FIELD->, <ALIAS>->expressions,
 * replace '.not.' with '!' and '!=' with '<>',
 * and make all non quoted part upper
 */
static BYTE * hb_rmqStrip( BYTE * bySrcExpr, ULONG ulLen, char * szAlias,
                           ULONG *ulDstLen )
{
   BYTE bQuote = 0, bLast = 0, * byDstExpr, b;
   ULONG ulDst = 0, ulAlias, ul;
   int iLen, iNewLen;

   byDstExpr = ( BYTE * ) hb_xgrab( ulLen + 1 );

   ulAlias = szAlias ? strlen( ( char * ) szAlias ) : 0;

   for( ul = 0; ul < ulLen; ul++ )
   {
      b = bySrcExpr[ ul ];

      if( bQuote )
      {
         if( bQuote == b )
         {
            bQuote = 0;
         }
         byDstExpr[ ulDst++ ] = b;
      }
      else if( b == '\'' || b == '"' || b == '[' )
      {
         bQuote = ( b == '[' ) ? ']' : b;
         byDstExpr[ ulDst++ ] = b;
      }
      else
      {
         if( b != ' ' )
         {
            if( bLast == ' ' && ulDst > 0 &&
                 HB_RMQ_ISIDCH( b ) && HB_RMQ_ISIDCH( byDstExpr[ ulDst - 1 ] ) )
            {
               byDstExpr[ ulDst++ ] = bLast;
            }
            byDstExpr[ ulDst++ ] = toupper( b );
            if( b == '>' && bLast == '-' )
            {
               ULONG n = 0;

               if( ulDst >= 7 && hb_strnicmp( ( char * ) &byDstExpr[ ulDst - 7 ], "FIELD", 5 ) == 0 )
               {
                  n = 7;
                  if( ulDst >= 8 && byDstExpr[ ulDst - 8 ] == '_' )
                     ++ n;
                  if( n < ulDst && HB_RMQ_ISIDCH( byDstExpr[ ulDst - n - 1 ] ) )
                     n = 0;
               }
               if( n == 0 && ulAlias && ulDst >= ulAlias + 2 )
               {
                  if( hb_strnicmp( ( char * ) &byDstExpr[ ulDst - ulAlias - 2 ], ( char * ) szAlias, ulAlias ) == 0 )
                  {
                     n = ulAlias + 2;
                     if( n < ulDst && HB_RMQ_ISIDCH( byDstExpr[ ulDst - n - 1 ] ) )
                        n = 0;
                  }
                  else if( ulDst > 4 && byDstExpr[ ulDst - 3 ] == ')' )
                  {
                     ULONG l = ulDst - 2;
                     int iB = 0;

                     do
                     {
                        --l;
                        if( byDstExpr[ l ] == '(' )
                           ++iB;
                        else if( byDstExpr[ l ] == ')' )
                           --iB;
                     }
                     while( l > 0 && iB );

                     if( !iB )
                     {
                        PHB_ITEM pRet = hb_rmqMacroEval( &byDstExpr[ l ], ulDst - l - 2 );
                        if( pRet )
                        {
                           char * cRet = hb_itemGetCPtr( pRet );
                           ULONG ll = hb_itemGetCLen( pRet );
                           if( ll > 0 )
                           {
                              while( cRet[ ll ] == ' ' )
                              {
                                 --ll;
                              }
                              while( *cRet == ' ' )
                              {
                                 ++cRet;
                                 --ll;
                              }
                              if( ll == ulAlias && hb_strnicmp( cRet, ( char * ) szAlias, ulAlias ) == 0 )
                              {
                                 n = ulDst - l;
                              }
                           }
                           hb_itemRelease( pRet );
                        }
                     }
                  }
               }
               if( n > 0 )
               {
                  ulDst -= n;
               }
            }
            else if( b == '=' && bLast == '!' )
            {
               ulDst -= 2;
               byDstExpr[ ulDst++ ] = '<';
               byDstExpr[ ulDst++ ] = '>';
            }
            else if( b == '.' && ( bLast == 't' || bLast == 'T' ) && ulDst >= 5 &&
                     hb_strnicmp( ( char * ) &byDstExpr[ ulDst - 5 ], ".NOT.", 5 ) == 0 )
            {
               ulDst -= 5;
               byDstExpr[ ulDst++ ] = b = '!';
            }
            else if( b == '(' && ulDst > 1 )
            {
               iLen = 0;
               while( ulDst - iLen >= 2 &&
                      HB_RMQ_ISIDCH( byDstExpr[ ulDst - iLen - 2 ] ) )
               {
                  ++iLen;
               }
               if( iLen > 4 && hb_rmqIsAbbFunc( &byDstExpr[ ulDst - 1 - iLen ], iLen, &iNewLen ) )
               {
                  ulDst = ulDst - 1 - iLen + iNewLen;
                  byDstExpr[ ulDst++ ] = '(';
               }
            }
            if( ulDst >= 2 && b == '!' && byDstExpr[ ulDst - 2 ] == '!' )
            {
               ulDst -= 2;
            }
         }
      }
      bLast = b;
   }

   ul = hb_rmqStripBraces( byDstExpr, ulDst );
   if( ul > 0 )
   {
      ulDst -= ( ul << 1 );
      memmove( byDstExpr, &byDstExpr[ ul ], ulDst );
   }

   byDstExpr[ ulDst ] = '\0';
   *ulDstLen = ulDst;
   return byDstExpr;
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
         printf("KEY=[%s]:%ld ", pRMQTags->pKey, pRMQTags->ulKey);
      if( pRMQTags->pFor )
         printf("FOR=[%s]:%ld ", pRMQTags->pFor, pRMQTags->ulFor);
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

static PHB_RMQTAG hb_rmqCreateTagList( AREAP pArea, char * szAlias )
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
      OrderInfo.itmResult = hb_itemPutL( OrderInfo.itmResult, FALSE );
      SELF_ORDINFO( pArea, DBOI_CUSTOM, &OrderInfo );
      if( ! hb_itemGetL( OrderInfo.itmResult ) )
      {
         SELF_ORDINFO( pArea, DBOI_UNIQUE, &OrderInfo );
         if( ! hb_itemGetL( OrderInfo.itmResult ) )
         {
            BOOL fDesc, fDel;
            BYTE * byFor = NULL, *byKey = NULL;
            ULONG ulFor = 0, ulKey = 0, ulLen;
            char cType, * szVal;

            SELF_ORDINFO( pArea, DBOI_ISDESC, &OrderInfo );
            fDesc = hb_itemGetL( OrderInfo.itmResult );

            OrderInfo.itmResult = hb_itemPutC( OrderInfo.itmResult, "" );
            SELF_ORDINFO( pArea, DBOI_KEYTYPE, &OrderInfo );
            cType = hb_itemGetCPtr( OrderInfo.itmResult )[0];
            OrderInfo.itmResult = hb_itemPutC( OrderInfo.itmResult, "" );
            SELF_ORDINFO( pArea, DBOI_CONDITION, &OrderInfo );
            szVal = hb_itemGetCPtr( OrderInfo.itmResult );
            ulLen = hb_itemGetCLen( OrderInfo.itmResult );
            if( ulLen && ! hb_strEmpty( szVal, ulLen ) )
            {
               byFor = hb_rmqStrip( ( BYTE * ) szVal, ulLen, szAlias, &ulFor );
            }
            OrderInfo.itmResult = hb_itemPutC( OrderInfo.itmResult, "" );
            SELF_ORDINFO( pArea, DBOI_EXPRESSION, &OrderInfo );
            szVal = hb_itemGetCPtr( OrderInfo.itmResult );
            ulLen = hb_itemGetCLen( OrderInfo.itmResult );
            if( ulLen && ! hb_strEmpty( szVal, ulLen ) )
            {
               byKey = hb_rmqStrip( ( BYTE * ) szVal, ulLen, szAlias, &ulKey );
            }
            if( byFor )
            {
               BOOL fNeg = FALSE;
               if( s_ulDelExp == 0 )
               {
                  BYTE *byExp = hb_rmqStrip( ( BYTE * ) HB_RMQ_DELEXP, strlen( HB_RMQ_DELEXP ),
                                             szAlias, &s_ulDelExp );
                  hb_strncpy( ( char * ) s_byDelExp, ( char * ) byExp, sizeof( s_byDelExp ) - 1 );
                  hb_xfree( byExp );
               }
               fDel = hb_rmqCmp( byFor, ulFor, s_byDelExp, s_ulDelExp, &fNeg, FALSE ) &&
                      fNeg;
            }
            else
            {
               fDel = FALSE;
            }
            *pRMQTagPtr = ( PHB_RMQTAG ) hb_xgrab( sizeof( HB_RMQTAG ) );
            (*pRMQTagPtr)->iTag     = i;
            (*pRMQTagPtr)->cType    = cType;
            (*pRMQTagPtr)->fDescend = fDesc;
            (*pRMQTagPtr)->pKey     = byKey;
            (*pRMQTagPtr)->ulKey    = ulKey;
            (*pRMQTagPtr)->pFor     = byFor;
            (*pRMQTagPtr)->ulFor    = ulFor;
            (*pRMQTagPtr)->fDelExp  = fDel;
            (*pRMQTagPtr)->pNext    = NULL;
            pRMQTagPtr = &( *pRMQTagPtr )->pNext;
         }
      }
   }

   hb_rmqClearOrderInfo( &OrderInfo );

   return pRMQTags;
}

static PHB_RMQTAG hb_rmqFindTag( PHB_RMQTAG pRMQTags, BOOL * pfFor,
                                 BYTE * byExpr, ULONG ulLen, BOOL * pfNeg )
{
   BOOL fNeg = FALSE;

   while( pRMQTags )
   {
      if( pfFor )
      {
         if( pRMQTags->pFor )
         {
            if( hb_rmqCmp( byExpr, ulLen, pRMQTags->pFor, pRMQTags->ulFor, &fNeg, FALSE ) )
            {
               *pfFor = TRUE;
               break;
            }
         }
         else if( pRMQTags->cType == 'L' )
         {
            if( hb_rmqCmp( byExpr, ulLen, pRMQTags->pKey, pRMQTags->ulKey, &fNeg, FALSE ) )
            {
               *pfFor = FALSE;
               break;
            }
         }
         fNeg = FALSE;
      }
      else if( !pRMQTags->pFor /* || ( pRMQTags->fDelExp && hb_setGetDeleted() ) */ )
      {
         if( hb_rmqCmp( byExpr, ulLen, pRMQTags->pKey, pRMQTags->ulKey, &fNeg, pRMQTags->cType == 'C' ) )
            break;
         fNeg = FALSE;
      }
      pRMQTags = pRMQTags->pNext;
   }
   if( pfNeg && pRMQTags )
      *pfNeg = fNeg;

   return pRMQTags;
}

static PHB_RMQSTACK hb_rmqStackCreate( AREAP pArea, PHB_RMQTAG pRMQTags,
                                       ULONG ulRecords, BYTE * byExpr, ULONG ulLen )
{
   PHB_RMQSTACK pStack = ( PHB_RMQSTACK ) hb_xgrab( sizeof( HB_RMQSTACK ) );

   pStack->iPos = 0;
   pStack->iSize = HB_RMQ_STACKSIZE;
   pStack->pBase = ( PHB_RMQSTACKITM ) hb_xgrab( sizeof( HB_RMQSTACKITM ) *
                                                 HB_RMQ_STACKSIZE );
   pStack->pTags = pRMQTags;
   pStack->byExpr = byExpr;
   pStack->ulExpr = ulLen;
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
      if( pItm->byExpr )
         hb_xfree( pItm->byExpr );
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
   if( pStack->byExpr )
      hb_xfree( pStack->byExpr );
   hb_xfree( pStack );
}

static int hb_rmqStackOptLvl( PHB_RMQSTACK pStack, BOOL fExp )
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
         hb_rmqStackOptLvl( pStack, FALSE );
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
            OrderInfo.itmNewVal = hb_itemPutL( NULL, FALSE );
            OrderInfo.itmResult = hb_itemPutL( NULL, FALSE );
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
            OrderInfo.itmNewVal = hb_itemPutL( OrderInfo.itmNewVal, TRUE );
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

static BOOL hb_rmqStackDoOpt( BOOL fTest, PHB_RMQSTACK pStack,
                              PHB_RMQSTACKITM pWith, BOOL fNeg )
{
   BOOL fOpt = FALSE, fOpt1, fOpt2;

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
               fOpt = hb_rmqStackDoOpt( TRUE, pStack, pWith, FALSE );
               if( fOpt && !fTest )
               {
                  pStack->iCur = iCur + 1;
                  fOpt = hb_rmqStackDoOpt( FALSE, pStack, pWith, FALSE );
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
static BYTE * hb_rmqNonOptExpr( PHB_RMQSTACK pStack, ULONG *pulExp, BOOL fAll )
{
   BYTE  *pExp = NULL;
   BYTE  *pExp1, *pExp2;
   ULONG ulExp1, ulExp2, ulLen = 0;

   if( pStack->iCur < pStack->iPos )
   {
      PHB_RMQSTACKITM pItm = &pStack->pBase[ pStack->iCur++ ];

      if( pItm->iOpt == RM_OPT_NONE )
      {
         fAll = TRUE;
      }

      if( pItm->iOpt == RM_OPT_FULL && !fAll )
      {
         /* update the stack counter only */
         pStack->iCur--;
         hb_rmqStackOptLvl( pStack, TRUE );
      }
      else if( pItm->iOper == HB_RMQ_AND )
      {
         pExp1 = hb_rmqNonOptExpr( pStack, &ulExp1, fAll );
         pExp2 = hb_rmqNonOptExpr( pStack, &ulExp2, fAll );
         if( pExp1 && pExp2 )
         {
            ulLen = ulExp1 + ulExp2 + 7;
            pExp = ( BYTE * ) hb_xgrab( ulLen + 1 );
            pExp[ 0 ] = '(';
            memcpy( pExp + 1, pExp1, ulExp1 );
            memcpy( pExp + 1 + ulExp1, ".AND.", 5 );
            memcpy( pExp + 6 + ulExp1, pExp2, ulExp2 );
            pExp[ ulLen - 1 ] = ')';
            hb_xfree( pExp1 );
            hb_xfree( pExp2 );
         }
         else if( pExp1 )
         {
            ulLen = ulExp1;
            pExp = pExp1;
         }
         else if( pExp2 )
         {
            ulLen = ulExp2;
            pExp = pExp2;
         }
      }
      else if( pItm->iOper == HB_RMQ_OR )
      {
         pExp1 = hb_rmqNonOptExpr( pStack, &ulExp1, fAll );
         pExp2 = hb_rmqNonOptExpr( pStack, &ulExp2, fAll );
         if( pExp1 && pExp2 )
         {
            ulLen = ulExp1 + ulExp2 + 6;
            pExp = ( BYTE * ) hb_xgrab( ulLen + 1 );
            pExp[ 0 ] = '(';
            memcpy( pExp + 1, pExp1, ulExp1 );
            memcpy( pExp + 1 + ulExp1, ".OR.", 4 );
            memcpy( pExp + 5 + ulExp1, pExp2, ulExp2 );
            pExp[ ulLen - 1 ] = ')';
            hb_xfree( pExp1 );
            hb_xfree( pExp2 );
         }
         else if( pExp1 )
         {
            ulLen = ulExp1;
            pExp = pExp1;
         }
         else if( pExp2 )
         {
            ulLen = ulExp2;
            pExp = pExp2;
         }
      }
      else if( pItm->iOper == HB_RMQ_NOT )
      {
         pExp1 = hb_rmqNonOptExpr( pStack, &ulExp1, fAll );
         if( pExp1 )
         {
            ulLen = ulExp1 + 3;
            pExp = ( BYTE * ) hb_xgrab( ulLen + 1 );
            pExp[ 0 ] = '!';
            pExp[ 1 ] = '(';
            memcpy( pExp + 2, pExp1, ulExp1 );
            pExp[ ulLen - 1 ] = ')';
            hb_xfree( pExp1 );
         }
      }
      else if( pItm->iOper == HB_RMQ_SKIP )
      {
         pExp = hb_rmqNonOptExpr( pStack, &ulLen, fAll );
      }
      else if( pItm->pTag && !fAll )
      {
         ;
      }
      else if( pItm->byExpr && pItm->ulExpr )
      {
         ulLen = pItm->ulExpr;
         pExp = ( BYTE * ) hb_xgrab( ulLen + 1 );
         memcpy( pExp, pItm->byExpr, ulLen );
      }
      else
      {
      }
   }
   *pulExp = ulLen;
   if( pExp )
      pExp[ ulLen ] = '\0';
   return pExp;
}

static void hb_rmqStackInit( PHB_RMQSTACK pStack )
{
   pStack->iCur = 0;
}


#if defined( HB_RMQ_DBGDISP ) || defined( HB_RMQ_DEBUG )
static void hb_rmqNonOptExprDsp( PHB_RMQSTACK pStack )
{
   BYTE *pExpr;
   ULONG ulLen = 0;
   hb_rmqStackInit( pStack );
   pExpr = hb_rmqNonOptExpr( pStack, &ulLen, FALSE );
   printf("\n>>");
   printf("[%s][%ld]", pExpr, ulLen);
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
         pItm->fNeg = FALSE;
         printf(".NOT. ( ");
         pStack->iCur--;
         hb_rmqStackDsp( pStack );
         printf(" )");
         pItm->fNeg = TRUE;
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
      else if( pItm->byExpr )
      {
         printf( "<%s>[%d,%d,%d,%s,%s]", pItm->byExpr,
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
   iOpt = hb_rmqStackOptLvl( pStack, FALSE );
   printf("[%d]\n", iOpt );
   fflush(stdout);
}
#endif

static void hb_rmqStackPush( PHB_RMQSTACK pStack, int iOper, BOOL fNeg,
                             PHB_RMQTAG pTag, PHB_ITEM pItem,
                             BYTE * byExpr, ULONG ulLen )
{
   PHB_RMQSTACKITM pItm;

   if( pTag && pStack->iPos > 0 &&
        pStack->pBase[ pStack->iPos - 1 ].iOper == HB_RMQ_NOT )
   {
//      hb_rmqStackPop( pStack );
//      fNeg = !fNeg;
   }

   if( pStack->iPos == pStack->iSize )
   {
      pStack->iSize += HB_RMQ_STACKSIZE;
      pStack->pBase = ( PHB_RMQSTACKITM ) hb_xrealloc( pStack->pBase,
                                                       pStack->iSize );
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
   if( byExpr )
   {
      pItm->byExpr = ( BYTE * ) hb_xgrab( ulLen + 1 );
      pItm->ulExpr = ulLen;
      memcpy( pItm->byExpr, byExpr, ulLen );
      pItm->byExpr[ ulLen ] = '\0';
   }
   else
   {
      pItm->byExpr = NULL;
      pItm->ulExpr = 0;
   }

   return;
}

static void hb_rmqStackPushNot( PHB_RMQSTACK pStack )
{
   if( pStack->iPos > 0 &&
        pStack->pBase[ pStack->iPos - 1 ].iOper == HB_RMQ_NOT )
      hb_rmqStackPop( pStack );
   else
      hb_rmqStackPush( pStack, HB_RMQ_NOT, FALSE, NULL, NULL, NULL, 0 );
}

static void hb_rmqStackPushOper( PHB_RMQSTACK rmqStack, int iOper )
{
   hb_rmqStackPush( rmqStack, iOper, FALSE, NULL, NULL, NULL, 0 );
}

static void hb_rmqDivide( BYTE * byExpr, ULONG ulLen, PHB_RMQSTACK rmqStack )
{
   PHB_RMQTAG pTag;
   BOOL fNeg, fFor = FALSE;
   int iOper;
   ULONG ul, ulExp;

   pTag = hb_rmqFindTag( rmqStack->pTags, &fFor, byExpr, ulLen, &fNeg );
   if( pTag )
   {
      if( fFor )
      {
         if( fNeg )
            hb_rmqStackPushNot( rmqStack );
         hb_rmqStackPush( rmqStack, HB_RMQ_TAG, FALSE, pTag, NULL, byExpr, ulLen );
      }
      else
      {
         hb_rmqStackPush( rmqStack, HB_RMQ_EQU, FALSE, pTag,
                          hb_itemPutL( NULL, !fNeg ), byExpr, ulLen );
      }
      return;
   }

#ifdef HB_RMQ_DBGDISP
   {
      BYTE b = byExpr[ ulLen ];
      byExpr[ ulLen ] = '\0';
      printf("\n[%s]", byExpr);fflush(stdout);
      byExpr[ ulLen ] = b;
   }
#endif

   iOper = HB_RMQ_OR;
   ul = hb_rmqNextExp( byExpr, ulLen, &iOper, &ulExp );
#ifdef HB_RMQ_DBGDISP
   printf("[%ld,%d]\n", ul, iOper);fflush(stdout);
#endif
   if( ul == 0 )
   {
      if( byExpr[ 0 ] == '!' )
      {
         hb_rmqStackPushNot( rmqStack );
         byExpr++;
         ulLen--;
      }
      ul = hb_rmqStripBraces( byExpr, ulLen );
      if( ul > 0 )
      {
         hb_rmqDivide( &byExpr[ ul ], ulLen - ( ul << 1 ), rmqStack );
      }
      else
      {
         PHB_ITEM pItem = NULL;

         pTag = NULL;
         ul = hb_rmqOper( byExpr, ulLen, &iOper, &ulExp );
         if( ul == 0 )
         {
            pTag = hb_rmqFindTag( rmqStack->pTags, &fFor, byExpr, ulLen, &fNeg );
         }
         else
         {
#ifdef HB_RMQ_DBGDISP
            {
               BYTE b = byExpr[ ulExp ], b2 = byExpr[ ulLen ];
               byExpr[ ulExp ] = '\0';
               byExpr[ ulLen ] = '\0';
               printf("\n[%ld:%ld:%ld]<%s><%s>", ulExp, ul, ulLen,
                      byExpr, &byExpr[ ul ] );fflush(stdout);
               byExpr[ ulExp ] = b;
               byExpr[ ulLen ] = b2;
            }
#endif
            pItem = hb_rmqMacroEval( &byExpr[ ul ], ulLen - ul );
#ifdef HB_RMQ_DBGDISP
            printf("[%s]", pItem ? hb_itemTypeStr( pItem ) : "U" );fflush(stdout);
#endif
            if( pItem )
            {
               pTag = hb_rmqFindTag( rmqStack->pTags, NULL, byExpr, ulExp, &fNeg );
#ifdef HB_RMQ_DBGDISP
               printf(" %d\n", pTag ? pTag->iTag : 0 );fflush(stdout);
#endif
            }
         }
         if( pTag )
         {
            if( ul == 0 )
            {
               if( fFor )
                  hb_rmqStackPush( rmqStack, HB_RMQ_TAG, fNeg, pTag, NULL, byExpr, ulLen );
               else
                  hb_rmqStackPush( rmqStack, HB_RMQ_EQU, FALSE, pTag,
                                   hb_itemPutL( NULL, !fNeg ), byExpr, ulLen );
            }
            else
            {
               hb_rmqStackPush( rmqStack, iOper, fNeg, pTag, pItem, byExpr, ulLen );
               pItem = NULL;
            }
         }
         else
         {
            hb_rmqStackPush( rmqStack, HB_RMQ_NONE, FALSE, NULL, NULL, byExpr, ulLen );
         }
         if( pItem )
            hb_itemRelease( pItem );
      }
   }
   else
   {
      hb_rmqStackPushOper( rmqStack, iOper );
      hb_rmqDivide( byExpr, ulExp, rmqStack );
      hb_rmqDivide( &byExpr[ ul ], ulLen - ul, rmqStack );
   }
}

static PHB_RMQSTACK hb_rmqBuildStack( AREAP pArea, PHB_ITEM pQuery )
{
   PHB_RMQSTACK pStack = NULL;

   if( hb_itemGetCLen( pQuery ) > 0 )
   {
      char szAliasBuf[ HB_RDD_MAX_ALIAS_LEN + 1 ], * szAlias;
      PHB_RMQTAG pRMQTags;
      ULONG ulRecords;

      if( SELF_ALIAS( ( AREAP ) pArea, szAliasBuf ) == FAILURE ||
          SELF_RECCOUNT( ( AREAP ) pArea, &ulRecords ) == FAILURE )
         return NULL;

      szAlias = *szAliasBuf ? szAliasBuf : NULL;

      pRMQTags = hb_rmqCreateTagList( pArea, szAlias );
      if( pRMQTags )
      {
         PHB_ITEM pExpr = hb_itemNew( pQuery );
         BYTE * byExpr;
         ULONG ulLen;

         hb_macroTextValue( pExpr );
         ulLen = hb_itemGetCLen( pExpr );
         byExpr = hb_rmqStrip( ( BYTE * ) hb_itemGetCPtr( pExpr ), ulLen, szAlias, &ulLen );
         hb_itemRelease( pExpr );
         if( byExpr )
         {
            pStack = hb_rmqStackCreate( pArea, pRMQTags, ulRecords, byExpr, ulLen );
#ifdef HB_RMQ_DBGDISP
            hb_rmqDispTagList( pRMQTags );
#endif
            hb_rmqDivide( byExpr, ulLen, pStack );
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
      iOptLvl = hb_rmqStackOptLvl( pStack, FALSE );
      hb_rmqStackDestoy( pStack );
   }

   return iOptLvl;
}

PHB_ITEM hb_rmqNonOptExpression( AREAP pArea, PHB_ITEM pQuery )
{
   int iOptLvl = RM_OPT_NONE;
   PHB_RMQSTACK pStack = hb_rmqBuildStack( pArea, pQuery );
   BYTE * pNExpr = NULL;
   ULONG ulNExpr = 0;

   if( pStack )
   {
      hb_rmqStackInit( pStack );
      iOptLvl = hb_rmqStackOptLvl( pStack, TRUE );

      if( iOptLvl == RM_OPT_PART )
      {
         hb_rmqStackInit( pStack );
         pNExpr = hb_rmqNonOptExpr( pStack, &ulNExpr, FALSE );
      }
      hb_rmqStackDestoy( pStack );
   }
   if( pNExpr )
   {
      return hb_itemPutCPtr( NULL, ( char * ) pNExpr, ulNExpr );
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
   BYTE * pNExpr = NULL;
   ULONG ulNExpr = 0;

   if( pStack )
   {
      hb_rmqStackInit( pStack );
      iOptLvl = hb_rmqStackOptLvl( pStack, FALSE );

      if( iOptLvl != RM_OPT_NONE )
      {
         hb_rmqStackInit( pStack );
         iOptLvl = hb_rmqStackOptLvl( pStack, TRUE );
         if( iOptLvl == RM_OPT_PART )
         {
            hb_rmqStackInit( pStack );
            pNExpr = hb_rmqNonOptExpr( pStack, &ulNExpr, FALSE );
         }

         hb_rmqStackInit( pStack );
         hb_rmqStackDoOpt( FALSE, pStack, NULL, FALSE);

         hb_rmqStackInit( pStack );
         hb_rmqStackOptLvl( pStack, FALSE );

         hb_rmqStackInit( pStack );
         pRM = hb_rmqStackEval( pStack );
      }
      hb_rmqStackDestoy( pStack );
   }

   if( ! pRM )
   {
      ULONG ulRecords;

      if( SELF_RECCOUNT( ( AREAP ) pArea, &ulRecords ) == SUCCESS )
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
            pRM->pNonExpr = hb_itemPutCPtr( pRM->pNonExpr, ( char * ) pNExpr, ulNExpr );
            pNExpr = NULL;
         }
         else if( pRM->pNonExpr == NULL )
            pRM->pNonExpr = hb_itemNew( pQuery );
         else
            hb_itemCopy( pRM->pNonExpr, pQuery );
      }
      else if( pRM->pNonExpr != NULL )
      {
         hb_itemPutC( pRM->pNonExpr, "" );
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
      PHB_ITEM pExpr = hb_itemParam( 1 );
      PHB_RMQSTACK pStack = hb_rmqBuildStack( pArea, pExpr );

      if( pStack )
      {
         hb_retclen( ( char * ) pStack->byExpr, pStack->ulExpr );
         hb_rmqStackDestoy( pStack );
      }
      else
         hb_retc( NULL );
      hb_itemRelease( pExpr );
   }
   else
      hb_retc( NULL );
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
   BOOL fOpt = FALSE;

   if( pArea && hb_parclen( 1 ) )
   {
      PHB_ITEM pExpr = hb_itemParam( 1 );
      PHB_RMQSTACK pStack = hb_rmqBuildStack( pArea, pExpr );

      if( pStack )
      {
         hb_rmqStackInit( pStack );
         fOpt = hb_rmqStackDoOpt( TRUE, pStack, NULL, FALSE);
         hb_rmqStackDestoy( pStack );
      }
      hb_itemRelease( pExpr );
   }

   hb_retl( fOpt );
}

HB_FUNC( RMQ_DISPEXP )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   BOOL fOpt = FALSE;

   if( pArea && hb_parclen( 1 ) )
   {
      PHB_ITEM pExpr = hb_itemParam( 1 );
      PHB_RMQSTACK pStack = hb_rmqBuildStack( pArea, pExpr );

      if( pStack )
      {
         hb_rmqStackDisplay( pStack );
         hb_rmqStackInit( pStack );
         fOpt = hb_rmqStackDoOpt( FALSE, pStack, NULL, FALSE);
         hb_rmqStackDisplay( pStack );
         hb_rmqStackDestoy( pStack );
      }
      hb_itemRelease( pExpr );
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
