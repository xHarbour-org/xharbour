/*
 * $Id$
 */

/*
 * xharbour Project source code:
 * The session API
 *
 * Copyright 2009 Miguel Angel Marchuet Frutos <miguelangel@marchuet.net>
 * www - http://www.xharbour.org
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
 * As a special exception, the xHarbour Project gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the xHarbour
 * Project under the name xHarbour.  If you copy code from other
 * xHarbour Project or Free Software Foundation releases into a copy of
 * xHarbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbapi.h"

#include "hbapiitm.h"
#include "hbapises.h"
#include "hbapierr.h"

#define HB_SESSION_MAX_ 1024

static HB_SESSION s_session = { 1, "XHARBOUR", 0 };

HB_SESSION_ANNOUNCE()

static PHB_SESSION s_SessionList[ HB_SESSION_MAX_ ] = { &s_session };
PHB_SESSION pSession = &s_session;

PHB_SESSION hb_session( void )
{
   return pSession;
}

static int hb_sessionFindPos( const int sessionID )
{
   int iPos;

   if( sessionID )
   {
      for( iPos = 0; iPos < HB_SESSION_MAX_ && s_SessionList[ iPos ]; iPos++ )
      {
         if( s_SessionList[ iPos ]->id == sessionID )
            return iPos;
      }
   }

   return -1;
}

PHB_SESSION hb_sessionFind( const int sessionID )
{
   int iPos;

   HB_TRACE( HB_TR_DEBUG, ( "hb_sessionFind(%i)", sessionID ) );

   iPos = hb_sessionFindPos( sessionID );

   return ( iPos != -1 ) ? s_SessionList[ iPos ] : NULL;
}

PHB_SESSION hb_sessionSelect( PHB_SESSION session )
{
   PHB_SESSION sessionOld = pSession;

   HB_TRACE( HB_TR_DEBUG, ( "hb_sessionSelect(%p)", session ) );

   if( session )
      pSession = session;

   return sessionOld;
}

int hb_sessionID( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_sessionID()" ) );

   return pSession ? ( int ) pSession->id : 0;
}

int hb_sessionSelectID( const int sessionID )
{
   int sessionIDOld;

   HB_TRACE( HB_TR_DEBUG, ( "hb_sessionSelectID(%s)", sessionID ) );

   sessionIDOld = hb_sessionID();
   hb_sessionSelect( hb_sessionFind( sessionID ) );

   return sessionIDOld;
}

/*
   void hb_sessionReleaseAll( void )
   {
   int iPos = 0;

   while( iPos < HB_SESSION_MAX_ && s_SessionList[iPos] )
   {
      if( s_SessionList[iPos]->username )
         hb_xfree( s_SessionList[iPos]->username );
      iPos++;
   }
   }
 */

BOOL hb_sessionRegister( PHB_SESSION session )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_sessionRegister(%p)", session ) );

   if( session )
   {
      int iPos = hb_sessionFindPos( session->id );

      if( iPos == -1 )
      {
         for( iPos = 0; iPos < HB_SESSION_MAX_; iPos++ )
         {
            if( ! s_SessionList[ iPos ] )
            {
               session->id             = iPos;
               s_SessionList[ iPos ]   = session;
               return TRUE;
            }
         }
      }
   }
   return FALSE;
}

HB_FUNC( HB_SESSIONNEW )
{
   PHB_SESSION session = ( PHB_SESSION ) hb_xalloc( sizeof( PHB_SESSION ) );

   session->id = 0;
   if( ISCHAR( 1 ) )
      session->username = hb_parc( 1 );
   else
      session->username = "XHARBOUR";

   hb_retl( hb_sessionRegister( session ) );
}

HB_FUNC( HB_SESSIONSELECT )
{
   hb_retni( hb_sessionID() );

   if( ISNUM( 1 ) )
      hb_sessionSelectID( hb_parni( 1 ) );
}

HB_FUNC( HB_SESSIONUSERNAME )
{
   PHB_SESSION session = hb_sessionFind( hb_parni( 1 ) );

   hb_retc( session ? session->username : NULL );
}

HB_FUNC( HB_SETSESSION )
{
   HB_FUNC_EXEC( HB_SESSIONSELECT );
}

