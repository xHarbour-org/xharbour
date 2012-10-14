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

static void __sx_CopyRecord( PBYTE cTargetAlias, PBYTE cSourceAlias );

HB_FUNC( __SX_DBSORT ) // (file,afields,bfor,bwhile,nnext,nrec,lrest,rdd,ldescend,cAlias)
{ //   1     2     3     4      5    6     7    8      9	     10
   LONG     nNextRecords,
            nRecNo;
   BOOL     lRest,
            bGoTop;
   LONG     nCurrentRecNo,
            ulCount,
            ulRecNo;
   PHB_ITEM pArray        = hb_param( 2, HB_IT_ARRAY );
   PHB_ITEM pbFor         = hb_param( 3, HB_IT_BLOCK );
   PHB_ITEM pbCondition   = hb_param( 4, HB_IT_BLOCK );
   PHB_ITEM vParam        = hb_param( 10, HB_IT_ANY );
   int      iRDEType;                     /* RDD to use for new DBF file */
   PBYTE    cNewDBFFile;                  /* New DBF File name */
   PBYTE    cSourceAlias;                 /* New Alias for new DBF opened */
   PBYTE    cNewAlias;                    /* New Alias for new DBF opened */
   int      uilenpArray,
            ui;
   char *   cFieldType,
   * cFieldName;
   char     cIndexExpression[ 256 ];
   BOOL     bDescending   = FALSE;
   int      iWorkArea     = 0;

   // Check WorkArea
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_ARG, EDBCMD_NOALIAS, NULL, "__SX_DBSORT" );

   if( vParam && ! ISNIL( 10 ) )
      iWorkArea = _sx_select( vParam );

   // Alias of currently selected area
   cSourceAlias  = ( PBYTE ) sx_Alias( 0 );

   // Current Record Position
   nCurrentRecNo = sx_RecNo();

   // New file name passed ?
   if( ! ISCHAR( 1 ) )
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "__SX_DBSORT" );
   else
   {
      cNewDBFFile = ( PBYTE ) hb_parc( 1 );

      // Reject if empty string is passed
      if( strlen( ( char * ) cNewDBFFile ) == 0 )
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "__SX_DBSORT" );
      cNewAlias = ( PBYTE ) _sx_AutoAlias( ( char * ) hb_parc( 1 ) );

      if( ! ISNIL( 3 ) && ! ISBLOCK( 3 ) )
      {
         hb_xfree( cNewAlias );
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "__SX_DBSORT" );
      }

      if( ! ISNIL( 4 ) && ! ISBLOCK( 4 ) )
      {
         hb_xfree( cNewAlias );
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "__SX_DBSORT" );
      }

      // Next Records
      if( ISNUM( 5 ) )
         nNextRecords = hb_parnl( 5 );
      else
         nNextRecords = -1;

      // Record Number to process
      if( ISNUM( 6 ) )
      {
         nRecNo = hb_parnl( 6 );

         // nRecNo should be <= LastRec()
         if( ( nRecNo > sx_RecCount() ) || ( nRecNo < 1 ) )
         {
            hb_xfree( cNewAlias );
            hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "__SX_DBSORT" );
            return;
         }

         // nRecNo vs Current Position vs Next Clause
         if( ( nNextRecords > 0 ) && ( nRecNo <= nCurrentRecNo ) )
         {
            hb_xfree( cNewAlias );
            hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "__SX_DBSORT" );
            return;
         }
      }
      else
         nRecNo = -1;

      // Process all records ?
      if( ISLOG( 7 ) )
         lRest = hb_parl( 7 );
      else
         lRest = TRUE;

      if( ( nNextRecords == -1 ) && ( nRecNo == -1 ) )
      {
         lRest   = TRUE;
         bGoTop  = TRUE;
      }
      else
         bGoTop = FALSE;

      // RDE Type Passed ?
      if( ISCHAR( 8 ) )
      {
         char * cRDDChosen = ( char * ) hb_parc( 8 );
         iRDEType = _sx_CheckRDD( cRDDChosen );
      }
      else
         iRDEType = i_sxApi_RDD_Default;

      if( ISLOG( 9 ) )
         bDescending = hb_parl( 9 );

      // Now Processing Data .....
      // All Records Until Eof() With For And Condition
      sx_Select( sx_WorkArea( cSourceAlias ) );

      // Checking Array of Fields FOR MAKING INDEX !
      uilenpArray         = ( int ) HB_ARRAY_LEN( pArray );
      *cIndexExpression   = '\0';
      if( uilenpArray > 0 )
      {
         for( ui = 0; ui < uilenpArray; ui++ )
         {
            cFieldName = ( char * ) hb_arrayGetC( pArray, ui + 1 );
            cFieldType = ( char * ) SX_CONVFUNC( sx_FieldType( ( PBYTE ) cFieldName ) );

            switch( *cFieldType )
            {
               case 'C':
                  _sx_strcat( cIndexExpression, cFieldName, NULL );
                  if( ui < uilenpArray - 1 )
                     _sx_strcat( cIndexExpression, "+", NULL );
                  hb_xfree( cFieldName );
                  break;

               case 'D':
                  _sx_strcat( cIndexExpression, "DTOS(", cFieldName, ")", NULL );
                  if( ui < uilenpArray - 1 )
                     _sx_strcat( cIndexExpression, "+", NULL );
                  hb_xfree( cFieldName );
                  break;

               case 'N':
                  _sx_strcat( cIndexExpression, "STR(", cFieldName, ")", NULL );
                  if( ui < uilenpArray - 1 )
                     _sx_strcat( cIndexExpression, "+", NULL );
                  hb_xfree( cFieldName );
                  break;

               case 'L':
                  _sx_strcat( cIndexExpression, "IF(", cFieldName, ",'T','F')", NULL );
                  if( ui < uilenpArray - 1 )
                     _sx_strcat( cIndexExpression, "+", NULL );
                  hb_xfree( cFieldName );
                  break;
            }
         }

         // Make Index Here ...
         if( *cIndexExpression )
         {
            sx_Index( ( PBYTE ) "c:\\windows\\temp\\temporary",
                      ( PBYTE ) cIndexExpression, IDX_NONE, bDescending, ( PBYTE ) 0 );
         }
      }

      // Create New DBF based on Source Structure
      if( ! sx_CopyStructure( cNewDBFFile, cNewAlias ) )
         return;
      if( ! sx_Use( cNewDBFFile, cNewAlias, EXCLUSIVE, ( WORD ) iRDEType ) )
         return;

      // Back to Source Alias
      sx_Select( sx_WorkArea( cSourceAlias ) );

      if( bGoTop && lRest && ISBLOCK( 3 ) && ISBLOCK( 4 ) )
      {
         sx_GoTop();
         while( _sx_Eval( pbCondition ) && ! sx_Eof() )
         {
            if( _sx_Eval( pbFor ) )
               __sx_CopyRecord( cNewAlias, cSourceAlias );
            sx_Skip( 1 );
         }
      }
      else if( bGoTop && lRest && ISNIL( 3 ) && ISBLOCK( 4 ) )
      {
         sx_GoTop();
         while( _sx_Eval( pbCondition ) && ! sx_Eof() )
         {
            __sx_CopyRecord( cNewAlias, cSourceAlias );
            sx_Skip( 1 );
         }
      }
      else if( bGoTop && lRest && ISBLOCK( 3 ) && ISNIL( 4 ) )
      {
         sx_GoTop();
         while( ! sx_Eof() )
         {
            if( _sx_Eval( pbFor ) )
               __sx_CopyRecord( cNewAlias, cSourceAlias );
            sx_Skip( 1 );
         }
      }
      else if( bGoTop && lRest && ISNIL( 3 ) && ISNIL( 4 ) )
      {
         sx_GoTop();
         while( ! sx_Eof() )
         {
            __sx_CopyRecord( cNewAlias, cSourceAlias );
            sx_Skip( 1 );
         }
      }
      // nNextRecords Clause
      // lRest is assumed FALSE
      else if( ! bGoTop && ( nNextRecords > 0 ) )
      {
         ulCount = 0;
         if( ISBLOCK( 3 ) && ISBLOCK( 4 ) )
         {
            while
            (
               ( ulCount <= nNextRecords ) &&
               _sx_Eval( pbCondition ) &&
               ! sx_Eof()
            )
            {
               if( _sx_Eval( pbFor ) )
                  __sx_CopyRecord( cNewAlias, cSourceAlias );
               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( ISNIL( 3 ) && ISBLOCK( 4 ) )
         {
            while
            (
               ( ulCount <= nNextRecords ) &&
               _sx_Eval( pbCondition ) &&
               ! sx_Eof()
            )
            {
               __sx_CopyRecord( cNewAlias, cSourceAlias );
               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( ISBLOCK( 3 ) && ISNIL( 4 ) )
         {
            while( ( ulCount <= nNextRecords ) && ! sx_Eof() )
            {
               if( _sx_Eval( pbFor ) )
                  __sx_CopyRecord( cNewAlias, cSourceAlias );
               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( ISNIL( 3 ) && ISNIL( 4 ) )
         {
            while( ( ulCount < nNextRecords ) && ! sx_Eof() )
            {
               __sx_CopyRecord( cNewAlias, cSourceAlias );
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
         if( ISBLOCK( 3 ) && ISBLOCK( 4 ) )
         {
            while
            (
               ( ulCount <= nNextRecords ) &&
               _sx_Eval( pbCondition ) &&
               ! sx_Eof()
            )
            {
               ulRecNo = sx_RecNo();
               if( ( nRecNo == ulRecNo ) && _sx_Eval( pbFor ) )
               {
                  __sx_CopyRecord( cNewAlias, cSourceAlias );
                  break;
               }

               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( ISNIL( 3 ) && ISBLOCK( 4 ) )
         {
            while
            (
               ( ulCount <= nNextRecords ) &&
               _sx_Eval( pbCondition ) &&
               ! sx_Eof()
            )
            {
               ulRecNo = sx_RecNo();
               if( ( nRecNo == ulRecNo ) )
               {
                  __sx_CopyRecord( cNewAlias, cSourceAlias );
                  break;
               }

               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( ISBLOCK( 3 ) && ISNIL( 4 ) )
         {
            while( ( ulCount <= nNextRecords ) && ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( ( nRecNo == ulRecNo ) && _sx_Eval( pbFor ) )
               {
                  __sx_CopyRecord( cNewAlias, cSourceAlias );
                  break;
               }

               sx_Skip( 1 );
               ulCount++;
            }
         }
         else if( ISNIL( 3 ) && ISNIL( 4 ) )
         {
            while( ( ulCount <= nNextRecords ) && ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( nRecNo == ulRecNo )
               {
                  __sx_CopyRecord( cNewAlias, cSourceAlias );
                  break;
               }

               sx_Skip( 1 );
               ulCount++;
            }
         }
      }
      //  nRecNo Clause with no nNextRecords Clause
      // lRest is assumed FALSE
      else if( ! bGoTop && ( nRecNo > 0 ) )
      {
         if( ISBLOCK( 3 ) && ISBLOCK( 4 ) )
         {
            while( _sx_Eval( pbCondition ) && ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( ( nRecNo == ulRecNo ) && _sx_Eval( pbFor ) )
               {
                  __sx_CopyRecord( cNewAlias, cSourceAlias );
                  break;
               }

               sx_Skip( 1 );
            }
         }
         else if( ISNIL( 3 ) && ISBLOCK( 4 ) )
         {
            while( _sx_Eval( pbCondition ) && ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( ( nRecNo == ulRecNo ) )
               {
                  __sx_CopyRecord( cNewAlias, cSourceAlias );
                  break;
               }

               sx_Skip( 1 );
            }
         }
         else if( ISBLOCK( 3 ) && ISNIL( 4 ) )
         {
            while( ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( nRecNo == ulRecNo )
               {
                  __sx_CopyRecord( cNewAlias, cSourceAlias );
                  break;
               }

               sx_Skip( 1 );
            }
         }
         else if( ISNIL( 3 ) && ISNIL( 4 ) )
         {
            while( ! sx_Eof() )
            {
               ulRecNo = sx_RecNo();
               if( nRecNo == ulRecNo )
               {
                  __sx_CopyRecord( cNewAlias, cSourceAlias );
                  break;
               }

               sx_Skip( 1 );
            }
         }
      }

      // Free allocated memory
      sx_Select( sx_WorkArea( cNewAlias ) ); /* Select New Area */
      sx_Commit();
      sx_Close();                            /* Close It */
      if( vParam && ! ISNIL( 10 ) )
         sx_Select( ( WORD ) iWorkArea );

      // Now We In Source Area
      // sx_CloseIndexes();
      // unlink((char*)"c:\\windows\\temp\\temporary");
      if( cNewAlias )
         hb_xfree( cNewAlias );
   }
}

static void __sx_CopyRecord( PBYTE cTarget, PBYTE cSource )
{
   char cRecord[ 256 ];

   sx_GetRecord( ( PBYTE ) cRecord );     /* Get Record from source */
   sx_Select( sx_WorkArea( cTarget ) );   /* Select New Area */
   sx_AppendBlank();                      /* Append Blank Record */
   sx_PutRecord( ( PBYTE ) cRecord );     /* Put the new record */
   sx_Select( sx_WorkArea( cSource ) );   /* Select Source Area */
}
