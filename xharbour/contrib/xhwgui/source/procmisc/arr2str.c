/*
 * HWGUI - Harbour Win32 GUI library source code:
 * Array / String conversion functions
 *
 * Copyright 2003 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"


static char * ReadArray( char * ptr, PHB_ITEM pItem )
{
   int  iArLen, i;
   PHB_ITEM temp;

   ptr ++;
   iArLen = *( (short int*)ptr );
   ptr ++; ptr ++;
   hb_arrayNew( pItem, iArLen );
   for( i=0; i<iArLen; i++ )
   {
      if( *ptr == '\6' )            // Array
      {
         temp = hb_itemNew( NULL );
         ptr = ReadArray( ptr, temp );
      }
      else if( *ptr == '\1' )       // Char
      {
         int iLen;
         ptr ++;
         iLen = *( (short int*)ptr );
         ptr ++; ptr ++;
         temp = hb_itemPutCL( NULL, ptr, iLen );
         ptr += iLen;
      }
      else if( *ptr == '\2' )       // Int
      {
         long int lValue;
         ptr ++;
         lValue = *( (long int*)ptr );
         temp = hb_itemPutNL( NULL, lValue );
         ptr += 4;
      }
      else if( *ptr == '\3' )       // Numeric
      {
         int iLen, iDec;
         double dValue;
         ptr ++;
         iLen = (int) *ptr++;
         iDec = (int) *ptr++;
         dValue = *( (double*)ptr );
         temp = hb_itemPutNDLen( NULL,dValue,iLen,iDec );
         ptr += 8;
      }
      else if( *ptr == '\4' )       // Date
      {
         long int lValue;
         ptr ++;
         lValue = *( (long int*)ptr );
         temp = hb_itemPutDL( NULL, lValue );
         ptr += 4;
      }
      else if( *ptr == '\5' )       // Logical
      {
         ptr ++;
         temp = hb_itemPutL( NULL, (int) *ptr++ );
      }
      else                            // Nil
      {
         ptr ++;
         temp = hb_itemNew( NULL );
      }

      hb_itemArrayPut( pItem, i+1, temp );
      hb_itemRelease( temp );
   }
   return ptr;

}

static long int ArrayMemoSize( PHB_ITEM pArray )
{
   long int lMemoSize = 3;
   unsigned int i;

   for( i=1; i<=pArray->item.asArray.value->ulLen; i++ )
   {
      switch( ( pArray->item.asArray.value->pItems + i - 1 )->type )
      {
         case HB_IT_STRING:
            lMemoSize += 3 + ( pArray->item.asArray.value->pItems + i - 1 )->item.asString.length;
            break;

         case HB_IT_DATE:
            lMemoSize += 5;
            break;

         case HB_IT_LOGICAL:
            lMemoSize += 2;
            break;

         case HB_IT_ARRAY:
            lMemoSize += ArrayMemoSize( pArray->item.asArray.value->pItems + i - 1 );
            break;

         case HB_IT_INTEGER:
         case HB_IT_LONG:
            lMemoSize += 5;
            break;

         case HB_IT_DOUBLE:
            lMemoSize += 11;
            break;

         default:
            lMemoSize += 1;
            break;
      }
   }

   return lMemoSize;
}

static char * WriteArray( char * ptr, PHB_ITEM pArray )
{
   unsigned int i, iLen;

   *ptr++ = '\6';
   *( (short int*)ptr ) = pArray->item.asArray.value->ulLen;
   ptr++; ptr++;

   for( i=1; i<=pArray->item.asArray.value->ulLen; i++ )
   {
      switch( ( pArray->item.asArray.value->pItems + i - 1 )->type )
      {
         case HB_IT_STRING:
            *ptr++ = '\1';
            iLen = ( pArray->item.asArray.value->pItems + i - 1 )->item.asString.length;
            *( (short int*)ptr ) = iLen;
            ptr++; ptr++;
            memcpy( ptr, ( pArray->item.asArray.value->pItems + i - 1 )->item.asString.value, iLen );
            ptr += iLen;
            break;

         case HB_IT_DATE:
            *ptr++ = '\4';
            *( (long int*)ptr ) = ( pArray->item.asArray.value->pItems + i - 1 )->item.asDate.value;
            ptr += 4;
            break;

         case HB_IT_LOGICAL:
            *ptr++ = '\5';
            *ptr++ = ( pArray->item.asArray.value->pItems + i - 1 )->item.asLogical.value;
            break;

         case HB_IT_ARRAY:
            ptr = WriteArray( ptr, pArray->item.asArray.value->pItems + i - 1 );
            break;

         case HB_IT_INTEGER:
         case HB_IT_LONG:
            *ptr++ = '\2';
            *( (long int*)ptr ) = hb_itemGetNL( pArray->item.asArray.value->pItems + i - 1 );
            ptr += 4;
            break;

         case HB_IT_DOUBLE:
            *ptr++ = '\3';
            *ptr++ = (char)( ( pArray->item.asArray.value->pItems + i - 1 )->item.asDouble.length );
            *ptr++ = (char)( ( pArray->item.asArray.value->pItems + i - 1 )->item.asDouble.decimal );
            *( (double*)ptr ) = hb_itemGetND( pArray->item.asArray.value->pItems + i - 1 );
            ptr += 8;
            break;

         default:
            *ptr++ = '\0';
      }
   }

   return ptr;
}

HB_FUNC( ARRAY2STRING )
{
   PHB_ITEM pArray    = hb_param( 1, HB_IT_ARRAY );
   long int lMemoSize = ArrayMemoSize( pArray );
   char * szResult    = (char*) hb_xgrab( lMemoSize + 10 );

   WriteArray( szResult, pArray );
   hb_retclen_buffer( szResult,lMemoSize );
}

HB_FUNC( STRING2ARRAY )
{
   char * szResult = hb_parc( 1 );
   PHB_ITEM pItem = hb_itemNew( NULL );

   if( hb_parclen(1) > 2 && *szResult == '\6' )
      ReadArray( szResult, pItem );

   // hb_itemReturn( pItem );
   // hb_itemRelease( pItem );
   hb_itemRelease( hb_itemReturn( pItem ) );
}
