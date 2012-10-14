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

static BOOL bSDF = FALSE;

static void _sx_GetLine( PHB_ITEM pArray, FILE * hFileHandle, char * cDelimiter )
{
   LONG     uiLen,
            i;
   char *   cTemp,
   * cFieldName,
   * cFieldType;

   if( HB_IS_ARRAY( pArray ) )
   {
      uiLen = (LONG) HB_ARRAY_LEN( pArray );
      if( uiLen > 0 )
      {
         for( i = 0; i < uiLen; i++ )
         {
            cFieldName = ( char * ) hb_arrayGetC( pArray, i + 1 );
            cFieldType = ( char * ) SX_CONVFUNC( sx_FieldType( ( PBYTE ) cFieldName ) );

            if( ! ( strcmp( cFieldType, "M" ) == 0 ) )
            {
               cTemp = ( char * ) SX_CONVFUNC( sx_GetVariant( ( PBYTE ) cFieldName ) );

               if( ! bSDF )
                  cTemp = _sx_rtrim( cTemp );
               else
               {
                  cTemp      = _sx_padl( cTemp, ' ',
                                         sx_FieldWidth( ( PBYTE ) cFieldName ) + sx_FieldDecimals( ( PBYTE ) cFieldName ) );
                  cDelimiter = " ";
               }

               if( i < uiLen - 1 )
                  fprintf( hFileHandle, "%s%s", cTemp, cDelimiter );
               else
                  fprintf( hFileHandle, "%s", cTemp );
            }

            hb_xfree( cFieldName );
         }

         fprintf( hFileHandle, "\n" );
      }
      else
      {
         uiLen = sx_FieldCount();
         for( i = 0; i < uiLen; i++ )
         {
            cFieldName = ( char * ) SX_CONVFUNC( sx_FieldName( ( WORD ) ( i + 1 ) ) );
            cFieldType = ( char * ) SX_CONVFUNC( sx_FieldType( ( PBYTE ) cFieldName ) );

            if( ! ( strcmp( cFieldType, "M" ) == 0 ) )
            {
               cTemp = ( char * ) SX_CONVFUNC( sx_GetVariant( ( PBYTE ) cFieldName ) );

               if( ! bSDF )
                  cTemp = _sx_rtrim( cTemp );
               else
               {
                  cTemp      = _sx_padl( cTemp, ' ',
                                         sx_FieldWidth( ( PBYTE ) cFieldName ) + sx_FieldDecimals( ( PBYTE ) cFieldName ) );
                  cDelimiter = " ";
               }

               if( i < uiLen - 1 )
                  fprintf( hFileHandle, "%s%s", cTemp, cDelimiter );
               else
                  fprintf( hFileHandle, "%s", cTemp );
            }
         }

         fprintf( hFileHandle, "\n" );
      }
   }
}

HB_FUNC( __SX_DBDELIM )   // (file, delim, afields, bfor, bwhile, nnext, nrec, lrest, cAlias )
{ //    1     2       3      4      5       6      7     8      9
   LONG     nNextRecords,
            nRecNo;
   BOOL     lRest,
            bGoTop;
   LONG     nCurrentRecNo,
            ulCount,
            ulRecNo;
   PHB_ITEM paFields      = hb_param( 3, HB_IT_ARRAY );
   PHB_ITEM pbFor         = hb_param( 4, HB_IT_BLOCK );
   PHB_ITEM pbCondition   = hb_param( 5, HB_IT_BLOCK );
   FILE *   hFileHandle;
   char *   cpFileName;
   char *   cDelimiter;
   WORD     iWorkArea = SX_DUMMY_NUMBER;

   // Check WorkArea
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_ARG, EDBCMD_NOALIAS, NULL, "__SX_DBDELIM" );

   if( ! ISNIL( 9 ) )
      iWorkArea = _sx_select( hb_param( 9, HB_IT_ANY ) );

   // Current Record Position
   nCurrentRecNo = sx_RecNo();

   // FileName To Create Passed ?
   if( ! ISCHAR( 1 ) )
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "__SX_DBDELIM" );
   else
   {
      cpFileName = ( char * ) hb_parc( 1 );
      if( strlen( cpFileName ) == 0 )
         hb_errRT_DBCMD( EG_ARG, EDBCMD_NOALIAS, NULL, "__SX_DBDELIM" );

      // Delimiter passed ? Defaulted to space
      if( ISCHAR( 2 ) )
         cDelimiter = ( char * ) hb_parc( 2 );
      else
         cDelimiter = " ";

      if( strcmp( cDelimiter, "SDF" ) == 0 )
         bSDF = TRUE;
      else
         bSDF = FALSE;

      // CodeBlock passed ?
      if( ! ISNIL( 4 ) && ! ISBLOCK( 4 ) )
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "__SX_DBDELIM" );

      if( ! ISNIL( 5 ) && ! ISBLOCK( 5 ) )
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "__SX_DBDELIM" );

      // Next Records
      if( ISNUM( 6 ) )
         nNextRecords = hb_parnl( 6 );
      else
         nNextRecords = -1;

      // Record Number to process
      if( ISNUM( 7 ) )
      {
         nRecNo = hb_parnl( 7 );

         // nRecNo should be <= LastRec()
         if( ( nRecNo > sx_RecCount() ) || ( nRecNo < 1 ) )
            return;

         // nRecNo vs Current Position vs Next Clause
         if( ( nNextRecords > 0 ) && ( nRecNo <= nCurrentRecNo ) )
            return;
      }
      else
         nRecNo = -1;

      // Process all records ?
      if( ISLOG( 8 ) )
         lRest = hb_parl( 8 );
      else
         lRest = TRUE;

      if( ( nNextRecords == -1 ) && ( nRecNo == -1 ) )
      {
         lRest   = TRUE;
         bGoTop  = TRUE;
      }
      else
         bGoTop = FALSE;

      // Creating File Handle
      hFileHandle = hb_fopen( ( const char * ) cpFileName, "w+" );

      if( ! hFileHandle )
         hb_retl( FALSE );

      // Now Processing Data .....
      // All Records Until Eof() With For And Condition
      if( bGoTop && lRest && ISBLOCK( 4 ) && ISBLOCK( 5 ) )
      {
         sx_GoTop();
         while( _sx_Eval( pbCondition ) && ! sx_Eof() )
         {
            if( _sx_Eval( pbFor ) )
               _sx_GetLine( paFields, hFileHandle, cDelimiter );
            sx_Skip( 1 );
         }
      }
      else if( bGoTop && lRest && ISNIL( 4 ) && ISBLOCK( 5 ) )
      {
         sx_GoTop();
         while( _sx_Eval( pbCondition ) && ! sx_Eof() )
         {
            _sx_GetLine( paFields, hFileHandle, cDelimiter );
            sx_Skip( 1 );
         }
      }
      else if( bGoTop && lRest && ISBLOCK( 4 ) && ISNIL( 5 ) )
      {
         sx_GoTop();
         while( ! sx_Eof() )
         {
            if( _sx_Eval( pbFor ) )
               _sx_GetLine( paFields, hFileHandle, cDelimiter );
            sx_Skip( 1 );
         }
      }
      else if( bGoTop && lRest && ISNIL( 4 ) && ISNIL( 5 ) )
      {
         sx_GoTop();
         while( ! sx_Eof() )
         {
            _sx_GetLine( paFields, hFileHandle, cDelimiter );
            sx_Skip( 1 );
         }
      }
      // nNextRecords Clause
      // lRest is assumed FALSE
      else if( ! bGoTop && ( nNextRecords > 0 ) )
      {
         ulCount = 0;
         if( ISBLOCK( 4 ) && ISBLOCK( 5 ) )
         {
            while
            (
               ( ulCount <= nNextRecords - 1 ) &&
               _sx_Eval( pbCondition ) &&
               ! sx_Eof()
            )
            {
               if( _sx_Eval( pbFor ) )
                  _sx_GetLine( paFields, hFileHandle, cDelimiter );
               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( ISNIL( 4 ) && ISBLOCK( 5 ) )
         {
            while
            (
               ( ulCount <= nNextRecords - 1 ) &&
               _sx_Eval( pbCondition ) &&
               ! sx_Eof()
            )
            {
               _sx_GetLine( paFields, hFileHandle, cDelimiter );
               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( ISBLOCK( 4 ) && ISNIL( 5 ) )
         {
            while( ( ulCount <= nNextRecords - 1 ) && ! sx_Eof() )
            {
               if( _sx_Eval( pbFor ) )
                  _sx_GetLine( paFields, hFileHandle, cDelimiter );
               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( ISNIL( 4 ) && ISNIL( 5 ) )
         {
            while( ( ulCount < nNextRecords - 1 ) && ! sx_Eof() )
            {
               _sx_GetLine( paFields, hFileHandle, cDelimiter );
               sx_Skip( 1 );
               ulCount++;
            }
         }
      }
      // nNextRecords Clause with nRecNo Clause
      // lRest is assumed FALSE
      else if( ! bGoTop && ( nNextRecords > 0 ) && ( nRecNo > 0 ) )
      {
         ulCount = 0;
         if( ISBLOCK( 4 ) && ISBLOCK( 5 ) )
         {
            while
            (
               ( ulCount <= nNextRecords - 1 ) &&
               _sx_Eval( pbCondition ) &&
               ! sx_Eof()
            )
            {
               ulRecNo = sx_RecNo();
               if( ( nRecNo == ulRecNo ) && _sx_Eval( pbFor ) )
               {
                  _sx_GetLine( paFields, hFileHandle, cDelimiter );
                  break;
               }

               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( ISNIL( 4 ) && ISBLOCK( 5 ) )
         {
            while
            (
               ( ulCount <= nNextRecords - 1 ) &&
               _sx_Eval( pbCondition ) &&
               ! sx_Eof()
            )
            {
               ulRecNo = sx_RecNo();
               if( ( nRecNo == ulRecNo ) )
               {
                  _sx_GetLine( paFields, hFileHandle, cDelimiter );
                  break;
               }

               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( ISBLOCK( 4 ) && ISNIL( 5 ) )
         {
            while( ( ulCount <= nNextRecords - 1 ) && ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( ( nRecNo == ulRecNo ) && _sx_Eval( pbFor ) )
               {
                  _sx_GetLine( paFields, hFileHandle, cDelimiter );
                  break;
               }

               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( ISNIL( 4 ) && ISNIL( 5 ) )
         {
            while( ( ulCount <= nNextRecords - 1 ) && ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( nRecNo == ulRecNo )
               {
                  _sx_GetLine( paFields, hFileHandle, cDelimiter );
                  break;
               }

               sx_Skip( 1 );
               ulCount++;
            }
         }
      }
      // nRecNo Clause with no nNextRecords Clause
      // lRest is assumed FALSE
      else if( ! bGoTop && ( nRecNo > 0 ) )
      {
         if( ISBLOCK( 4 ) && ISBLOCK( 5 ) )
         {
            while( _sx_Eval( pbCondition ) && ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( ( nRecNo == ulRecNo ) && _sx_Eval( pbFor ) )
               {
                  _sx_GetLine( paFields, hFileHandle, cDelimiter );
                  break;
               }

               sx_Skip( 1 );
            }
         }
         else if( ISNIL( 4 ) && ISBLOCK( 5 ) )
         {
            while( _sx_Eval( pbCondition ) && ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( ( nRecNo == ulRecNo ) )
               {
                  _sx_GetLine( paFields, hFileHandle, cDelimiter );
                  break;
               }

               sx_Skip( 1 );
            }
         }
         else if( ISBLOCK( 4 ) && ISNIL( 5 ) )
         {
            while( ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( nRecNo == ulRecNo )
               {
                  _sx_GetLine( paFields, hFileHandle, cDelimiter );
                  break;
               }

               sx_Skip( 1 );
            }
         }
         else if( ISNIL( 4 ) && ISNIL( 5 ) )
         {
            while( ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( nRecNo == ulRecNo )
               {
                  _sx_GetLine( paFields, hFileHandle, cDelimiter );
                  break;
               }

               sx_Skip( 1 );
            }
         }
      }

      // Close The File
      fclose( hFileHandle );
      hb_retl( TRUE );
      if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
         sx_Select( iWorkArea );
   }
}
