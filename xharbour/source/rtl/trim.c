/*
 * $Id: trim.c,v 1.7 2004/02/21 14:39:01 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * *TRIM() functions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

extern char *hb_vm_sNull;
extern char *hb_vm_acAscii[256];

/* Memo type is handled here */
static PHB_ITEM hb_itemPutCLM( PHB_ITEM pItem, char * szText, ULONG ulLen, BOOL bIsMemo )
{
   HB_TRACE_STEALTH(HB_TR_DEBUG, ("hb_itemPutCLM(%p, %s, %lu)", pItem, szText, ulLen));

#if 0
   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
      {
         hb_itemClear( pItem );
      }
   }
   else
   {
      pItem = hb_itemNew( NULL );
   }
#endif

   pItem->type = bIsMemo ? HB_IT_MEMO : HB_IT_STRING ;

   if( szText == NULL || ulLen == 0 )
   {
      pItem->item.asString.length  = 0;
      pItem->item.asString.value   = hb_vm_sNull;
      pItem->item.asString.bStatic = TRUE;
   }
   else if( ulLen == 1 )
   {
      pItem->item.asString.length  = 1;
      pItem->item.asString.value   = hb_vm_acAscii[ (BYTE) ( szText[0] ) ];
      pItem->item.asString.bStatic = TRUE;
   }
   else
   {
      pItem->item.asString.puiHolders      = (ULONG*) hb_xgrab( sizeof( ULONG ) );
      *( pItem->item.asString.puiHolders ) = 1;
      pItem->item.asString.bStatic         = FALSE;
      pItem->item.asString.length          = ulLen;
      pItem->item.asString.value           = ( char * ) hb_xgrab( ulLen + 1 );
      hb_xmemcpy( pItem->item.asString.value, szText, ulLen );
      pItem->item.asString.value[ ulLen ]  = '\0';
   }

   return pItem;
}

/* trims from the left, and returns a new pointer to szText */
/* also returns the new length in lLen */
HB_EXPORT char * hb_strLTrim( const char * szText, ULONG * ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strLTrim(%s, %p)", szText, ulLen));

   while( *ulLen && HB_ISSPACE( *szText ) )
   {
      szText++;
      ( *ulLen )--;
   }

   return ( char * ) szText;
}

/* return length of szText ignoring trailing white space (or true spaces) */
ULONG HB_EXPORT hb_strRTrimLen( const char * szText, ULONG ulLen, BOOL bAnySpace )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strRTrimLen(%s, %lu. %d)", szText, ulLen, (int) bAnySpace));

   if( bAnySpace )
   {
      while( ulLen && HB_ISSPACE( szText[ ulLen - 1 ] ) )
         ulLen--;
   }
   else
   {
      while( ulLen && szText[ ulLen - 1 ] == ' ' )
         ulLen--;
   }

   return ulLen;
}

/* trims leading spaces from a string */

HB_FUNC( LTRIM )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
   {
      ULONG ulLen = pText->item.asString.length;
      char * szText = hb_strLTrim( pText->item.asString.value, &ulLen );

      hb_itemPutCLM( &(HB_VM_STACK).Return,
                     szText,
                     ulLen,
                     hb_param(1,HB_IT_MEMOFLAG) != NULL );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 1101, NULL, "LTRIM", 1, hb_paramError( 1 ) );
   }
}

/* trims trailing spaces from a string */

/* NOTE: The second parameter is a Harbour extension. */

HB_FUNC( RTRIM )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
   {
#ifdef HB_EXTENSION
      hb_itemPutCLM( &(HB_VM_STACK).Return,
                     pText->item.asString.value,
                     hb_strRTrimLen( pText->item.asString.value, pText->item.asString.length, ISLOG( 2 ) ? hb_parl( 2 ) : FALSE ),
                     hb_param(1,HB_IT_MEMOFLAG) != NULL );
#else
      hb_itemPutCLM( &(HB_VM_STACK).Return,
                     pText->item.asString.value,
                     hb_strRTrimLen( pText->item.asString.value,pText->item.asString.length, FALSE ), hb_param(1,HB_IT_MEMOFLAG) != NULL );
#endif
   }
   else
      /* NOTE: "TRIM" is right here [vszakats] */
      hb_errRT_BASE_SubstR( EG_ARG, 1100, NULL, "TRIM", 1, hb_paramError( 1 ) );
}

/* synonymn for RTRIM */
HB_FUNC( TRIM )
{
   HB_FUNCNAME( RTRIM )();
}

/* trims leading and trailing spaces from a string */

/* NOTE: The second parameter is a Harbour extension. */

HB_FUNC( ALLTRIM )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
   {
      char * pszText;
      ULONG ulLen = hb_strRTrimLen( pText->item.asString.value, pText->item.asString.length,
         ISLOG( 2 ) ? hb_parl( 2 ) : FALSE );

      pszText = hb_strLTrim( pText->item.asString.value, &ulLen );

      hb_itemPutCLM( &(HB_VM_STACK).Return,
                     pszText,
                     ulLen,
                     hb_param(1,HB_IT_MEMOFLAG) != NULL );
   }
   else
#ifdef HB_COMPAT_C53
      hb_errRT_BASE_SubstR( EG_ARG, 2022, NULL, "ALLTRIM", 1, hb_paramError( 1 ) ); /* NOTE: This appeared in CA-Cl*pper 5.3 [vszakats] */
#else
      hb_retc( "" );
#endif
}
