/*
 * $Id: inkey.c,v 1.30 2004/04/01 22:00:42 druzus Exp $
 */

/*
 * Harbour Project source code:
 * The Keyboard API
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    HB_KEYPUT()
 *
 * Copyright 2002 Walter Negro <anegro@overnet.com.ar>
 *    hb_setInkeyLast()
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@acn.waw.pl>
 *    SETLASTKEY()
 *
 * Copyright 2004 Peter Rees <peter@rees.co.nz>
 *    SETINKEYBEFOREBLOCK()
 *    SETINKEYAFTERBLOCK()
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* NOTE: For OS/2. Must be ahead of any and all #include statements */
#define INCL_DOSPROCESS
#define INCL_NOPMAPI

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbapigt.h"
#include "hbvm.h"
#include "hbset.h"
#include "hbinkey.ch"
#include "inkey.ch"

#include <time.h>
#if defined( HB_OS_UNIX )
  #include <sys/times.h>
  #include <unistd.h>
#endif

static int *  s_inkeyBuffer = NULL; /* Harbour keyboard buffer (empty if head == tail)     */
static int    s_inkeyHead;       /* Harbour keyboard buffer head pointer (next insert)  */
static int    s_inkeyTail;       /* Harbour keyboard buffer tail pointer (next extract) */
static int    s_inkeyLast = 0;       /* Last key extracted from Harbour keyboard buffer     */
static BOOL   s_inkeyPoll;       /* Flag to override no polling when TYPEAHEAD is 0     */
static int    s_inkeyForce;      /* Variable to hold keyboard input when TYPEAHEAD is 0 */

static PHB_inkeyKB s_inkeyKB = NULL;
static HB_inkey_enum s_eventmask;

static HB_ITEM s_inKeyBlockBefore = HB_ITEM_NIL;
static HB_ITEM s_inKeyBlockAfter  = HB_ITEM_NIL;

static void hb_inkeyKBfree( void )
{
   PHB_inkeyKB pNext;

   if( s_inkeyKB )
   {
      pNext     = s_inkeyKB->pNext;
      hb_xfree( s_inkeyKB->String );
      hb_xfree( s_inkeyKB );
      s_inkeyKB = pNext;
   }
}

static int hb_inkeyFetch( void ) /* Extract the next key from the keyboard buffer */
{
   int key;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyFetch()"));

   hb_inkeyPoll();
   if( hb_set.HB_SET_TYPEAHEAD )
   {
      /* Proper typeahead support is set */
      if( s_inkeyHead == s_inkeyTail ) key = 0;    /* Keyboard buffer is empty */
      else
      {                                            /* Keyboard buffer is not empty */
         s_inkeyLast = s_inkeyBuffer[ s_inkeyTail++ ];

         if( s_inkeyLast == -99 && s_inkeyKB )
         {
            s_inkeyLast = s_inkeyKB->String[ s_inkeyKB->Pos-- ];

            if( s_inkeyKB->Pos >= 0 )
            {
               s_inkeyTail--;
            }
            else
            {
               hb_inkeyKBfree( );
            }
         }

         if( s_inkeyTail >= hb_set.HB_SET_TYPEAHEAD )
            s_inkeyTail = 0;

         key = s_inkeyLast;
      }
      s_inkeyForce = 0;
   }
   else
   {
      s_inkeyLast = s_inkeyForce;           /* Typeahead support is disabled */

      if( s_inkeyKB )
      {
         s_inkeyLast = s_inkeyKB->String[ s_inkeyKB->Pos-- ];

         if( s_inkeyKB->Pos < 0 )
         {
            hb_inkeyKBfree( );
            s_inkeyForce = 0;
         }
      }
      key = s_inkeyLast;
   }

   return key;
}

int hb_inkey( BOOL bWait, double dSeconds, HB_inkey_enum event_mask )
{
   int key;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkey(%d, %lf, %d)", (int) bWait, dSeconds, (int) event_mask));

   s_eventmask = event_mask;                   /* Set current input event mask */
   s_inkeyPoll = TRUE;                         /* Force polling */

   /* Wait for input events if requested */
   if( bWait )
   {
      if( ( dSeconds * CLOCKS_PER_SEC ) < 1 )  /* Wait forever ? */
      {
         /* There is no point in waiting forever for no input events! */
         if( ( event_mask & ( INKEY_ALL + INKEY_RAW ) ) != 0 )
         {
            while( hb_inkeyNext( event_mask ) == 0 )
            {
               // immediately break if a VM request is pending.
               if ( hb_vmRequestQuery() != 0 )
               {
                  return 0;
               }

               hb_idleState();
            }
            hb_idleReset();
         }
      }
      else
      {
#if defined( HB_OS_UNIX )
         /* NOTE: clock() returns a time used by a program - if it is suspended
          * then this time will be zero
         */
         clock_t end_clock;
         struct tms tm;

         end_clock = times( &tm ) + ( clock_t ) ( dSeconds * sysconf(_SC_CLK_TCK) );
         while( hb_inkeyNext( event_mask ) == 0 && (times( &tm ) < end_clock) )
#else
         clock_t end_clock = clock() + ( clock_t ) ( dSeconds * CLOCKS_PER_SEC );
         while( hb_inkeyNext( event_mask ) == 0 && clock() < end_clock )
#endif
         {
            if ( hb_vmRequestQuery() != 0 )
            {
               return 0;
            }
            hb_idleState();
         }
         hb_idleReset();
      }
   }

   key = hb_inkeyFetch();            /* Get the current input event or 0 */

   s_inkeyPoll = FALSE;                        /* Stop forced polling */
   s_eventmask = hb_set.HB_SET_EVENTMASK;      /* Restore original input event mask */

   return hb_inkeyTranslate( key, event_mask );
}

int hb_inkeyLast( HB_inkey_enum event_mask )      /* Return the value of the last key that was extracted */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyLast()"));

   hb_inkeyPoll();

   return hb_inkeyTranslate( s_inkeyLast, event_mask );
}

int hb_setInkeyLast( int ch )      /* Force a value to s_inkeyLast and return previous value */
{
   int last = s_inkeyLast;

   HB_TRACE(HB_TR_DEBUG, ("hb_setInkeyLast()"));

   s_inkeyLast = ch;

   return last;
}

int hb_inkeyNext( HB_inkey_enum event_mask )      /* Return the next key without extracting it */
{
   int key = s_inkeyForce;    /* Assume that typeahead support is disabled */

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyNext()"));

   hb_inkeyPoll();

   if( hb_set.HB_SET_TYPEAHEAD )
   {
      /* Proper typeahead support is enabled */
      if( s_inkeyHead == s_inkeyTail )
      {
         key = 0;
      }
      else
      {
         key = s_inkeyBuffer[ s_inkeyTail ];    /* Next key */

         if( key == -99  && s_inkeyKB )
         {
            key = s_inkeyKB->String[ s_inkeyKB->Pos ];
         }
      }
   }
   else
   {
      if( s_inkeyKB )
         key = s_inkeyKB->String[ s_inkeyKB->Pos ];
   }

   return hb_inkeyTranslate( key, event_mask );
}

void hb_inkeyPoll( void )     /* Poll the console keyboard to stuff the Harbour buffer */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyPoll()"));

   if( hb_set.HB_SET_TYPEAHEAD || s_inkeyPoll )
   {
      int ch = hb_gt_ReadKey( s_eventmask );

      switch( ch )
      {
         case HB_BREAK_FLAG:        /* Check for Ctrl+Break */
            if( !hb_set.HB_SET_CANCEL ) ch = 0; /* Ignore if cancel disabled */
         case K_ALT_C:              /* Check for normal Alt+C */
            if( hb_set.HB_SET_CANCEL )
            {
               ch = 3;              /* Pretend it's a Ctrl+C */
               hb_vmRequestCancel();/* Request cancellation */
            }
            break;
         case K_ALT_D:              /* Check for normal Alt+D */
            if( hb_set.HB_SET_DEBUG )
            {
               ch = 0;              /* Make the keystroke disappear */
               hb_vmRequestDebug(); /* Request the debugger */
            }
      }

      hb_inkeyPut( ch );
   }
}

void hb_inkeyReset( BOOL allocate )     /* Reset the keyboard buffer */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyReset(%d)", (int) allocate));

   /* Reset the buffer head and tail pointers, the last key value,
      and the polling override flag */
   s_inkeyHead = 0;
   s_inkeyTail = 0;
   //s_inkeyLast = 0;
   s_inkeyPoll = FALSE;
   s_inkeyForce = 0;

   while( s_inkeyKB )
   {
      hb_inkeyKBfree( );
   }

   s_inkeyKB = NULL;

   /* The allocate flag allows the same function to be used to reset the
      buffer or to reset and allocate, reallocate, or free the buffer */
   if( allocate )
   {
      /* If the buffer already exists, free it */
      if( s_inkeyBuffer )
      {
         hb_xfree( s_inkeyBuffer );
         s_inkeyBuffer = NULL;
      }

      /* Always allocate a new buffer, unless it's being freed from hb_setRelease() */
      if( hb_set.HB_SET_TYPEAHEAD > -1 )
      {
         /* The buffer min and max are determined by SET(HB_SET_TYPEAHEAD, but it
            can also set the typeahead to 0 to disable polling, in which case the
            minimum buffer size (which is 16) must still be allocated, because
            even when polling is disabled, calling INKEY() or NEXTKEY() will
            temporarily re-enable polling */
         s_inkeyBuffer = ( int * ) hb_xgrab( sizeof( int ) * ( hb_set.HB_SET_TYPEAHEAD == 0 ? 16 : hb_set.HB_SET_TYPEAHEAD ) );
      }
   }
}

HB_FUNC( INKEY )
{
  BOOL bContinue = TRUE ;
  USHORT uiPCount = hb_pcount();
  int iKey;

  if( s_inKeyBlockBefore.type == HB_IT_BLOCK )
  {
     hb_vmEvalBlock( &s_inKeyBlockBefore );
  }

  do
  {
    iKey = hb_inkey( uiPCount == 1 || ( uiPCount > 1 && ISNUM( 1 ) ),
                       hb_parnd( 1 ),
                       ISNUM( 2 ) ? ( HB_inkey_enum ) hb_parni( 2 ) : hb_set.HB_SET_EVENTMASK );

    bContinue = iKey && s_inKeyBlockAfter.type == HB_IT_BLOCK;

    if ( bContinue )
    {
      HB_ITEM Key;
      PHB_ITEM pReturn;

      Key.type = HB_IT_NIL;
      hb_itemPutNI( &Key, iKey );
      pReturn = hb_vmEvalBlockV( &s_inKeyBlockAfter, 1, &Key );

      // set iKey from return value of EvalBlock()
      iKey = hb_itemGetNI( pReturn );
      hb_setInkeyLast( iKey ) ; // Set LASTKEY()
      // if iKey = 0 then we continue as iKey was handled in hb_vmEvalBlock( s_inKeyBlockAfter )
      bContinue = (BOOL) !iKey ;
      hb_itemClear( &Key );
    }
  }
  while ( bContinue );

  hb_retni(iKey);
}

HB_FUNC( SETINKEYBEFOREBLOCK )
{
   USHORT uiPCount = hb_pcount();

   hb_itemReturnCopy( &s_inKeyBlockBefore );

   if( uiPCount > 0 )
   {
      if( s_inKeyBlockBefore.type == HB_IT_BLOCK )
      {
         HB_ITEM_UNLOCK( &s_inKeyBlockBefore );
         hb_itemClear( &s_inKeyBlockBefore );
      }

      if( ISBLOCK(1) )
      {
         hb_itemCopy ( &s_inKeyBlockBefore , hb_param( 1, HB_IT_BLOCK ) );
         HB_ITEM_LOCK( &s_inKeyBlockBefore );
      }
   }
}

HB_FUNC( SETINKEYAFTERBLOCK )
{
   USHORT uiPCount = hb_pcount();

   hb_itemReturnCopy( &s_inKeyBlockAfter );

   if ( uiPCount > 0 )
   {
      if ( s_inKeyBlockAfter.type == HB_IT_BLOCK )
      {
         HB_ITEM_UNLOCK( &s_inKeyBlockAfter );
         hb_itemClear( &s_inKeyBlockAfter );
      }

      if ( ISBLOCK(1) )
      {
         hb_itemCopy( &s_inKeyBlockAfter, hb_param( 1, HB_IT_BLOCK ));
         HB_ITEM_LOCK( &s_inKeyBlockAfter );
      }
   }
}

HB_FUNC( __KEYBOARD )
{
   /* Clear the typeahead buffer without reallocating the keyboard buffer */
   hb_inkeyReset( FALSE );

   if( ISCHAR( 1 ) )
   {
      ULONG size = hb_parclen( 1 );

      if( size != 0 )
      {
         BYTE * fPtr = ( BYTE * ) hb_parcx( 1 );

         /* It might be just a request to clear the buffer */
         if( *fPtr )
         {
            BYTE * pString     = ( BYTE * ) hb_xgrab( size + 1 );
            PHB_inkeyKB pInkey = ( PHB_inkeyKB ) hb_xgrab( sizeof( HB_inkeyKB ) );
            PHB_inkeyKB pRoot;

            pString[ size ] = 0;
            pInkey->Pos    = size - 1;

            while( size-- )
            {
               if( * fPtr == 59 )
               {
                  pString[ size ] = 13; /* Convert ";" to CR, like Clipper does */
               }
               else
               {
                  pString[ size ] = * fPtr;
               }

               fPtr++;
            }

            pInkey->String = pString;
            pInkey->pNext  = NULL;

            // printf( "pInkey->Pos = %i, pInkey->String = %s", pInkey->Pos, pInkey->String );

            if( s_inkeyKB )
            {
               pRoot = s_inkeyKB;

               while( pRoot->pNext )
               {
                  pRoot = pRoot->pNext;
               }

               pRoot->pNext = pInkey;
            }
            else
            {
               s_inkeyKB = pInkey;
            }

            hb_inkeyPut( -99 );

            /*
            // Stuff the string
            if( size >= ( ULONG ) hb_set.HB_SET_TYPEAHEAD )
            {
               // Have to allow for a zero size typehead buffer
               if( hb_set.HB_SET_TYPEAHEAD )
               {
                  size = ( ULONG ) ( hb_set.HB_SET_TYPEAHEAD - 1 );
               }
               else
               {
                  size = 0;
               }
            }

            while( size-- )
            {
               int ch = *fPtr++;

               if( ch == 59 )
               {
                  ch = 13; // Convert ";" to CR, like Clipper does
               }

               hb_inkeyPut( ch );
            }
            */
         }
      }
   }
#if defined( HB_EXTENSION )
   else
   {
      if( ISNUM( 1 ) )
      {
         hb_inkeyPut( hb_parni(1) );
      }
   }
#endif
}

void HB_EXPORT hb_inkeyPut( int ch )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyPut(%d)", ch));

   if( ch )
   {
      if( hb_set.HB_SET_TYPEAHEAD )
      {
         /* Proper typeahead support is set */
         int head = s_inkeyHead;

         s_inkeyBuffer[ head++ ] = ch;

         if( head >= hb_set.HB_SET_TYPEAHEAD )
         {
            head = 0;
         }

         if( head != s_inkeyTail )
         {
            s_inkeyHead = head;
         }
         else
         {
            /* TODO: Add error sound */ ;
         }
      }
      else
      {
         s_inkeyForce = ch; /* Typeahead support is disabled */
      }
   }
}

#ifdef HB_EXTENSION

HB_FUNC( HB_KEYPUT )
{
   if( ISNUM( 1 ) )
      hb_inkeyPut( hb_parni( 1 ) );
}

#endif

HB_FUNC( NEXTKEY )
{
   hb_retni( hb_inkeyNext( ISNUM( 1 ) ? ( HB_inkey_enum ) hb_parni( 1 ) : hb_set.HB_SET_EVENTMASK ) );
}

HB_FUNC( LASTKEY )
{
   hb_retni( hb_inkeyTranslate( s_inkeyLast, ISNUM( 1 ) ? ( HB_inkey_enum ) hb_parni( 1 ) : hb_set.HB_SET_EVENTMASK ) );
}

HB_FUNC( SETLASTKEY )
{
  if( ISNUM(1) )
  {
    hb_setInkeyLast( hb_parni(1) );
  }
  hb_retc( "" );
}

int hb_inkeyTranslate( int key, HB_inkey_enum event_mask )
{
   // left for possible future use
   return key;
}

void hb_inkeyExit( void )
{
   if( s_inKeyBlockBefore.type == HB_IT_BLOCK )
   {
      HB_ITEM_UNLOCK( &s_inKeyBlockBefore );
      hb_itemClear( &s_inKeyBlockBefore );
   }

   if( s_inKeyBlockAfter.type == HB_IT_BLOCK )
   {
      HB_ITEM_UNLOCK( &s_inKeyBlockAfter );
      hb_itemClear( &s_inKeyBlockAfter );
   }
}
