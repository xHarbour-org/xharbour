/*
 * $Id: inline_c.prg,v 1.3 2002/01/02 04:40:08 ronpinkas Exp $
 */
PROCEDURE MAIN( cLine, cDelim )

   LOCAL a, i

   IF EMPTY( cLine )
      cLine := "This is a test a"
   END IF

   a := aTokens( cLine, cDelim )
   FOR i := 1 TO LEN( a )
      ? '"' + a[ i ] + '"'
   NEXT i

   QOut( HB_INLINE() )
         { hb_retc( "\na C String, including { and \" { \n" ); }

   QOut( C_Func() )

   QOut( PostDumpTest() )

   QOut( HB_INLINE() )
         {
	    REQUEST HB_BUILDINFO /* Force HB_BUILDINFO To be Linked */
            #define _HB_VER_AS_STRING  5
            PHB_ITEM pSymbol;
            HB_ITEM  pInfo;
            (&pInfo)->type = HB_IT_NIL;

            hb_itemPutNI( &pInfo, _HB_VER_AS_STRING );

            pSymbol = hb_itemDoC("HB_BUILDINFO",1,&pInfo);
            hb_itemClear( &pInfo );

            if( pSymbol != NULL )
            {
               hb_retc(pSymbol->item.asString.value);
               hb_itemRelease( pSymbol );
            }
	    else
               hb_retc("HB_BUILDINFO does not exist in symbol table\nPlease do a REQUEST");

	 }

   QOut( PostDumpTest_1() )

RETURN

FUNCTION aTokens( cLine, cDelimiter )

   LOCAL aTokens := {}

   #ifdef __HARBOUR__

      IF cDelimiter == NIL
         cDelimiter := ' '
      ENDIF

      HB_INLINE( aTokens, cLine, Asc( cDelimiter ) )
      {  // Note including {
         PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
         PHB_ITEM pLine  = hb_param( 2, HB_IT_STRING );
         char cDelimiter = (char) hb_parni(3);
         size_t i, iOffset = 0, iIndex = 1;

         /* Comment including { */
         for( i = 0; i < pLine->item.asString.length; i++ )
         {
            if( pLine->item.asString.value[i] == cDelimiter )
            {
               hb_arraySize( pArray, iIndex );
               hb_storclen( pLine->item.asString.value + iOffset, i - iOffset, 1, iIndex );
               iOffset = i + 1;
               iIndex++;
            }
         }
         if( iOffset < pLine->item.asString.length )
         {
            hb_arraySize( pArray, iIndex );
            hb_storclen( pLine->item.asString.value + iOffset, pLine->item.asString.length - iOffset, 1, iIndex );
         }
      }

   #else

      LOCAL nLen := Len( cLine ), i, nOffset := 1

      IF cDelimiter == NIL
         cDelimiter := ' '
      ENDIF

      FOR i := 1 to nLen
         IF SubStr( cLine, i, 1 ) == cDelimiter
            aAdd( aTokens, SubStr( cLine, nOffset, i - nOffset ) )
            nOffset := i + 1
         ENDIF
      NEXT
      IF nOffset < nLen - 1
         aAdd( aTokens, SubStr( cLine, nOffset ) )
      ENDIF

   #endif

RETURN aTokens

#pragma BEGINDUMP
HB_FUNC( C_FUNC )
{
   hb_retc( "returned from C_FUN\n" );
}
#pragma ENDDUMP

Function PostDumpTest()
RETURN "Post Dump Test"

#pragma BEGINDUMP
#include "hbapi.h"
HB_FUNC( POSTDUMPTEST_1 )
{
   EXTERNAL HB_MULTITHREAD // To Check if Harbour Is MultiThread
   PHB_ITEM pMT = hb_itemDoC( "HB_MULTITHREAD", 0, 0 );

   if( pMT->item.asLogical.value )
      hb_retc( "Harbour Is MULTI THREAD" );
   else
      hb_retc( "Harbour Is SINGLE THREAD" );

   hb_itemRelease( pMT );
}
#pragma ENDDUMP
