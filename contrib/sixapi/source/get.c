/*
 * $Id$
 */

/*
 * SixAPI Project source code:
 *
 * Copyright 2010 Andi Jahja <xharbour@telkom.net.id>
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
 */
#include "sxapi.h"

#if 0

/* Pending Item */

HB_FUNC( SX_GETBLOB )
{
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_GETBLOB" );
   else
   {
      WORD  iWorkArea = SX_DUMMY_NUMBER;
      PVOID vpVar;
      ULONG lBlob;

      if( ! ISNIL( 2 ) )
         iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

      vpVar   = ( PVOID ) sx_MemAlloc( sx_GetBlobLength( ( PBYTE ) hb_parc( 1 ) ) );
      lBlob   = sx_GetBlob( ( PBYTE ) hb_parc( 1 ), &vpVar );

      hb_itemReturn( ( PHB_ITEM ) vpVar );
      sx_MemDealloc( vpVar );

      if( iWorkArea != SX_DUMMY_NUMBER )
         sx_Select( iWorkArea );
   }
}
#endif

HB_FUNC( SX_GETBLOBLENGTH )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_GETBLOBLENGTH" );

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retnl( sx_GetBlobLength( ( PBYTE ) hb_parc( 1 ) /* cpFieldName */ ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GETBITMAP )
{
   WORD  iWorkArea  = SX_DUMMY_NUMBER;
   // HWND  hWnd       = ( HWND ) hb_parnl( 2 );
   HWND  hWnd       = ( HWND ) hb_parns( 2 );

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_GETBITMAP" );

   if( ! ISNIL( 3 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retl( sx_GetBitMap( ( PBYTE ) hb_parc( 1 ) /* cpFieldName */,
                          hWnd /* Window Handle */
                          ) );

   hb_stornl( ( ULONG ) hWnd, 2 );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GETBYTE )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_GETBYTE" );

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retc( ( char * ) SX_CONVFUNC( sx_GetByte( ( PBYTE ) hb_parc( 1 ) ) /* cpFieldName */ ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GETCOMMITLEVEL )
{
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_COMMITLEVEL" );

   if( ISNUM( 1 ) )
      hb_retni( sx_GetCommitLevel( ( WORD ) hb_parni( 1 ) ) );
   else if( ! ISNIL( 1 ) )
   {
      WORD iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );
      if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      {
         hb_retni( sx_GetCommitLevel( iWorkArea ) );
         sx_Select( iWorkArea );
      }
   }
}

HB_FUNC( SX_GETDATEJULIAN )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_GETDATEJULIAN" );

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retnl( sx_GetDateJulian( ( PBYTE ) hb_parc( 1 ) ) );       /* Field name  */

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GETDATESTRING )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_GETDATESTRING" );

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retc( ( char * ) SX_CONVFUNC( sx_GetDateString( ( PBYTE ) hb_parc( 1 ) ) /* cpFieldname */ ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

char * _sx_GetDateValue( PBYTE cFieldName )
{
   char *   szDate  = ( char * ) SX_CONVFUNC( sx_GetVariant( cFieldName ) );
   int      d_value = 0,
            m_value = 0,
            y_value = 0;
   char     szDateFormat[ 9 ];
   char *   cRetval = ( char * ) hb_xgrab( 9 );

   if( szDate )
   {
      int      d_pos   = 0,
               m_pos   = 0,
               y_pos   = 0;
      int      count,
               digit,
               non_digit;
      char *   szDF = ( char * ) hb_setGetDateFormat();
      int      size = ( int ) strlen( hb_setGetDateFormat() );

      for( count = 0; count < size; count++ )
      {
         switch( szDF[ count ] )
         {
            case 'D':
            case 'd':
               if( d_pos == 0 )
               {
                  if( m_pos == 0 && y_pos == 0 )
                     d_pos = 1;
                  else if( m_pos == 0 || y_pos == 0 )
                     d_pos = 2;
                  else
                     d_pos = 3;
               }
               break;

            case 'M':
            case 'm':
               if( m_pos == 0 )
               {
                  if( d_pos == 0 && y_pos == 0 )
                     m_pos = 1;
                  else if( d_pos == 0 || y_pos == 0 )
                     m_pos = 2;
                  else
                     m_pos = 3;
               }
               break;

            case 'Y':
            case 'y':
               if( y_pos == 0 )
               {
                  if( m_pos == 0 && d_pos == 0 )
                     y_pos = 1;
                  else if( m_pos == 0 || d_pos == 0 )
                     y_pos = 2;
                  else
                     y_pos = 3;
               }
         }
      }

      /* If there are non-digits at the start of the date field,
         they are not to be treated as date field separators */
      non_digit  = 1;
      size       = ( int ) strlen( szDate );
      for( count = 0; count < size; count++ )
      {
         digit = szDate[ count ];
         if( isdigit( digit ) )
         {
            /* Process the digit for the current date field */
            if( d_pos == 1 )
               d_value = ( d_value * 10 ) + digit - '0';
            else if( m_pos == 1 )
               m_value = ( m_value * 10 ) + digit - '0';
            else if( y_pos == 1 )
               y_value = ( y_value * 10 ) + digit - '0';

            /* Treat the next non-digit as a date field separator */
            non_digit = 0;
         }
         else if( digit != ' ' )
         {
            /* Process the non-digit */
            if( non_digit++ == 0 )
            {
               /* Only move to the next date field on the first
                  consecutive non-digit that is encountered */
               d_pos--;
               m_pos--;
               y_pos--;
            }
         }
      }

      if( y_value >= 0 && y_value < 100 )
      {
         count   = hb_setGetEpoch() % 100;
         digit   = hb_setGetEpoch() / 100;

         if( y_value >= count )
            y_value += ( digit * 100 );
         else
            y_value += ( ( digit * 100 ) + 100 );
      }
   }

   hb_snprintf( szDateFormat, 9, "%04i%02i%02i", y_value, m_value, d_value );
   memcpy( cRetval, szDateFormat, 8 );
   cRetval[ 8 ] = '\0';
   return cRetval;
}

HB_FUNC( SX_GETDOUBLE )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_GETDOUBLE" );

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retnd( sx_GetDouble( ( PBYTE ) hb_parc( 1 ) /* cpFieldName */ ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GETINTEGER )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_GETINTEGER" );

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retni( ( int ) sx_GetInteger( ( PBYTE ) hb_parc( 1 ) /* cpFieldname */ ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GETLOGICAL )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_GETLOGICAL" );

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retl( sx_GetLogical( ( PBYTE ) hb_parc( 1 ) ) );           /* Field name  */

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GETLONG )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_GETLONG" );

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retnl( sx_GetLong( ( PBYTE ) hb_parc( 1 ) /* cpFieldName */ ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GETMEMO )
{
   WORD  uiLineWidth;
   PBYTE cpFieldName,
         cpMemo;

   WORD  iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_GETMEMO" );

   if( ! ISNIL( 3 ) )
      iWorkArea = _sx_select( hb_param( 3, HB_IT_ANY ) );

   if( ISCHAR( 1 ) )
   {
      cpFieldName = ( PBYTE ) hb_parc( 1 );

      if( ISNUM( 2 ) )
         uiLineWidth = ( WORD ) hb_parnd( 2 );
      else
         uiLineWidth = 0;

      cpMemo = ( PBYTE ) SX_CONVFUNC( sx_GetMemo( cpFieldName, uiLineWidth ) );

      hb_retc( ( char * ) cpMemo );

      if( lstrlen( ( const char * ) cpMemo ) )
         sx_MemDealloc( cpMemo );
   }
   else
      hb_retc( "" );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GETRECORD )
{
   WORD  iWorkArea = SX_DUMMY_NUMBER;
   PBYTE cpRecord;

   if( ! ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   cpRecord = ( PBYTE ) hb_xgrab( ( LONG ) sx_RecSize() + 1 );
   sx_GetRecord( cpRecord );
   hb_retc( ( char * ) cpRecord );
   hb_xfree( cpRecord );

   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GETRECORDEX )
{
   WORD     iWorkArea = SX_DUMMY_NUMBER;
   PHB_ITEM pString;
   int      uiFieldCount;
   PBYTE    cpRecord;
   USHORT   i,
            iOffSet,
            iFieldWidth;
   PBYTE    cpFieldName;

   if( ! ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   uiFieldCount  = sx_FieldCount();
   cpRecord      = ( PBYTE ) hb_xgrab( ( LONG ) sx_RecSize() + 1 );

   hb_reta( uiFieldCount + 1 );

   sx_GetRecord( cpRecord );
   pString = hb_itemPutCL( NULL, ( char * ) ( cpRecord ), 1 );
   HB_STORC( ( char * ) HB_GETC( pString ), -1, 1 );
   if( pString )
      hb_itemRelease( pString );

   for( i = 0; i < uiFieldCount; i++ )
   {
      cpFieldName   = ( PBYTE ) SX_CONVFUNC( sx_FieldName( ( WORD ) ( i + 1 ) ) );
      iOffSet       = sx_FieldOffset( cpFieldName );
      iFieldWidth   = sx_FieldWidth( cpFieldName );
      pString       = hb_itemPutCL( NULL, ( char * ) ( cpRecord + iOffSet - 1 ),
                                    iFieldWidth );
      HB_STORC( HB_GETC( pString ), -1, i + 2 );
      if( pString )
         hb_itemRelease( pString );
   }

   if( cpRecord )
      hb_xfree( cpRecord );
   if( pString )
      hb_itemClear( pString );
   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GETSCOPE )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retc( ( char * ) SX_CONVFUNC( sx_GetScope( ( WORD ) hb_parni( 1 ) ) /* iWhichScope */ ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GETSTRING )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retc( ( char * ) SX_CONVFUNC( sx_GetString( ( PBYTE ) hb_parc( 1 ) ) ) );   /* Field name  */

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GETTRIMSTRING )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retc( ( LPSTR ) SX_CONVFUNC( sx_GetTrimString( ( PBYTE ) hb_parc( 1 ) ) /* cpFieldName */ ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GETVALUEDTOS )   // ( cpFieldName )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
   {
      hb_ret();
      return;
   }

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   if( ISCHAR( 1 ) )
   {
      PBYTE    cFieldName = ( PBYTE ) hb_parc( 1 );
      char *   cFieldType = ( char * ) SX_CONVFUNC( sx_FieldType( cFieldName ) );
      char     szDate[ 9 ];

      hb_retc( cFieldType[ 0 ] == 'D' ? hb_dateDecStr( szDate, sx_GetDateJulian( cFieldName ) ) : "" );
   }

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GETVALUESTR )    // ( cpFieldName )
{
   BOOL  bTrim;
   WORD  iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
   {
      hb_ret();
      return;
   }

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   if( ISCHAR( 1 ) )
   {
      char *   cFieldType = ( char * ) SX_CONVFUNC( sx_FieldType( ( PBYTE ) hb_parc( 1 ) ) );
      PBYTE    cFieldName = ( PBYTE ) hb_parc( 1 );

      switch( *cFieldType )
      {
         case 'D':
            hb_retc( ( char * ) SX_CONVFUNC( sx_GetDateString( cFieldName ) ) );
            break;

         case 'N':
         case 'M':
            hb_retc( ( char * ) SX_CONVFUNC( sx_GetVariant( cFieldName ) ) );
            break;

         case 'L':
            hb_retc( sx_EvalLogical( cFieldName ) ? ".T." : ".F." );
            break;

         case 'C':
            bTrim = ISLOG( 2 ) ? hb_parl( 2 ) : FALSE;
            hb_retc( ( bTrim || bSetTrimmedON
                       ) ? ( char * ) SX_CONVFUNC( sx_GetTrimString( cFieldName ) ) : ( char * ) SX_CONVFUNC( sx_GetString( cFieldName ) ) );
            break;

         default:
            hb_retc( "" );
      }
   }

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GETVALUE )       // ( cpFieldName )
{
   WORD  iWorkArea = SX_DUMMY_NUMBER;
   BOOL  bTrim;

   //char  *szDateStr;

   if( ! _sx_Used() )
   {
      hb_ret();
      return;
   }

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   if( ISCHAR( 1 ) )
   {
      PBYTE    cFieldName = ( PBYTE ) hb_parc( 1 );
      char *   cFieldType = ( char * ) SX_CONVFUNC( sx_FieldType( cFieldName ) );

      switch( *cFieldType )
      {
         case 'N':
         {
            int   iFieldWidth   = sx_FieldWidth( cFieldName );
            int   iDecimals     = sx_FieldDecimals( cFieldName );
            if( iDecimals > 0 )
            {
               // GPF With __DMC__
               double d = sx_GetDouble( cFieldName );

               // hb_retnd( d );
               hb_retndlen( d, iFieldWidth, iDecimals );
            }
            else
               hb_retnl( sx_GetLong( cFieldName ) );
         }
         break;

         case 'D':
         {
            int   piYear, piMonth, piDay;
            LONG  lJulian = sx_GetDateJulian( cFieldName );

            hb_dateDecode( lJulian, &piYear, &piMonth, &piDay );
            hb_retd( piYear, piMonth, piDay );
         }
         break;

         case 'M':
            hb_retc( ( char * ) SX_CONVFUNC( sx_GetVariant( cFieldName ) ) );
            break;

         case 'L':
            hb_retl( sx_EvalLogical( cFieldName ) );
            break;

         case 'C':
            bTrim = ISLOG( 2 ) ? hb_parl( 2 ) : FALSE;
            hb_retc( ( bTrim || bSetTrimmedON
                       ) ? ( char * ) SX_CONVFUNC( sx_GetTrimString( cFieldName ) ) : ( char * ) SX_CONVFUNC( sx_GetString( cFieldName ) ) );
            break;
      }  // end switch ( *cFieldType )
   }

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_FIELDGET )
{
   HB_FUNCNAME( SX_GETVALUE ) ();
}

HB_FUNC( SX_GETVALUEEX )  // ( area )
{
   WORD     iWorkArea = SX_DUMMY_NUMBER;
   char *   szDateStr;
   int      iField,
            i;

   if( ! ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   iField = sx_FieldCount();

   hb_reta( iField );

   for( i = 0; i < iField; i++ )
   {
      PBYTE    cFieldName = ( PBYTE ) SX_CONVFUNC( sx_FieldName( ( WORD ) ( i + 1 ) ) );
      char *   cFieldType = ( char * ) SX_CONVFUNC( sx_FieldType( cFieldName ) );

      switch( *cFieldType )
      {
         case 'N':
            HB_STORND( sx_GetDouble( cFieldName ), -1, i + 1 );
            break;

         case 'D':
            szDateStr = _sx_GetDateValue( cFieldName );
            HB_STORDS( szDateStr, -1, i + 1 );
            hb_xfree( szDateStr );
            break;

         case 'M':
            HB_STORC( ( char * ) SX_CONVFUNC( sx_GetVariant( cFieldName ) ), -1, i + 1 );
            break;

         case 'L':
            HB_STORL( sx_EvalLogical( cFieldName ), -1, i + 1 );
            break;

         case 'C':
            if( bSetTrimmedON )
               HB_STORC( ( char * ) SX_CONVFUNC( sx_GetTrimString( cFieldName ) ), -1, i + 1 );
            else
               HB_STORC( ( char * ) SX_CONVFUNC( sx_GetString( cFieldName ) ), -1, i + 1 );
            break;
      }  // end switch ( *cFieldType )
   }

   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_GETVARIANT )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_GETVARIANT" );

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retc( ( char * ) SX_CONVFUNC( sx_GetVariant( ( PBYTE ) hb_parc( 1 ) ) ) );  /* Field name  */

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}
