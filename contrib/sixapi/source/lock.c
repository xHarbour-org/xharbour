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

HB_FUNC( SX_RLOCK )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_RLOCK" );

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   if( ISNUM( 1 ) )
   {
      hb_retl( sx_Rlock( hb_parni( 1 ) ) );
   }
   else if( ISARRAY( 1 ) )
   {
      PHB_ITEM vLock      = hb_param( 1, HB_IT_ARRAY );
      int      uilenLock  = ( int ) HB_ARRAY_LEN( vLock );
      int      ui,
               iRecNo;
      BOOL     bSucces    = TRUE;
      for( ui = 0; ui < uilenLock; ui++ )
      {
         iRecNo = hb_arrayGetNI( vLock, ui + 1 );
         if( ! sx_Rlock( iRecNo ) )
            bSucces = FALSE;
      }

      hb_retl( bSucces );
   }
   else
      hb_retl( sx_Rlock( sx_RecNo() ) );

   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_DBRLOCK )
{
   HB_FUNCNAME( SX_RLOCK ) ();
}

HB_FUNC( SX_LOCKED )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   if( ISNUM( 1 ) )
      hb_retl( sx_Locked( hb_parnl( 1 ) /* lRecNum */ ) );
   else
      hb_retl( sx_Locked( sx_RecNo() ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_LOCKCOUNT )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_LOCKCOUNT" );

   if( ! ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   hb_retni( sx_LockCount() );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_DBRLOCKLIST )
{
   int      iCount;
   int      i;
   WORD     iWorkArea = SX_DUMMY_NUMBER;
   PULONG   lLocks;

   if( ! _sx_Used() )
   {
      hb_reta( 0 );
      return;
   }

   if( ! ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   iCount = sx_LockCount();

   if( iCount )
   {
      lLocks = ( PULONG ) hb_xgrab( iCount * sizeof( PULONG ) );

      sx_DBRlockList( lLocks );

      hb_reta( iCount );

      for( i = 0; i < iCount; i++ )
         HB_STORNI( lLocks[ i ], -1, i + 1 );

      hb_xfree( lLocks );
   }
   else
      hb_reta( 0 );

   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_FLOCK )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_FLOCK" );

   if( ! ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   hb_retl( sx_Flock() );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_UNLOCK )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_UNLOCK" );

   if( ! ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   if( ISNUM( 1 ) )
      sx_Unlock( hb_parnl( 1 ) );
   else if( ISARRAY( 1 ) )
   {
      PHB_ITEM vLock      = hb_param( 1, HB_IT_ARRAY );
      int      uilenLock  = ( int ) HB_ARRAY_LEN( vLock );
      int      ui,
               iRecNo;
      for( ui = 0; ui < uilenLock; ui++ )
      {
         iRecNo = hb_arrayGetNI( vLock, ui + 1 );
         sx_Unlock( iRecNo );
      }
   }
   else
      sx_Unlock( sx_RecNo() );

   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_UNLOCKALL )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_UNLOCKALL" );

   if( ! ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   sx_Unlock( 0 );

   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_DBRUNLOCK )
{
   HB_FUNCNAME( SX_UNLOCK ) ();
}
