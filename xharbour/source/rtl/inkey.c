/*
 * $Id: inkey.c,v 1.43 2005/09/30 23:44:05 druzus Exp $
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
#if defined( HB_OS_OS2 )
#  define INCL_DOSPROCESS
#  define INCL_NOPMAPI
#endif

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbapigt.h"
#include "hbvm.h"
#include "hbset.h"
#include "inkey.ch"

#include <time.h>
#if defined( HB_OS_UNIX )
  #include <sys/times.h>
  #include <unistd.h>
#endif

static int    s_defaultKeyBuffer[ HB_DEFAULT_INKEY_BUFSIZE + 1 ];

static int *  s_inkeyBuffer = s_defaultKeyBuffer;
static int    s_inkeyBufferSize = HB_DEFAULT_INKEY_BUFSIZE;
static int    s_inkeyHead = 0;
static int    s_inkeyTail = 0;

static BYTE * s_StrBuffer = NULL;
static ULONG  s_StrBufferSize;
static ULONG  s_StrBufferPos;

static int    s_inkeyLast = 0;

static PHB_ITEM s_inKeyBlockBefore = NULL;
static PHB_ITEM s_inKeyBlockAfter  = NULL;


int hb_inkeyTranslate( int iKey, HB_inkey_enum event_mask )
{
   /*
    * Why bitfield has been defined as enum type?
    * Bit operations on enum types are forbidden in
    * many C/C++ compilers
    */
   int iMask;

   switch( iKey )
   {
      case K_MOUSEMOVE:
      case K_MMLEFTDOWN:
      case K_MMRIGHTDOWN:
      case K_MMMIDDLEDOWN:
      case K_NCMOUSEMOVE:
         iMask = INKEY_MOVE;
         break;
      case K_LBUTTONDOWN:
      case K_LDBLCLK:
         iMask = INKEY_LDOWN;
         break;
      case K_LBUTTONUP:
         iMask = INKEY_LUP;
         break;
      case K_RBUTTONDOWN:
      case K_RDBLCLK:
         iMask = INKEY_RDOWN;
         break;
      case K_RBUTTONUP:
         iMask = INKEY_RUP;
         break;
      case K_MBUTTONDOWN:
      case K_MBUTTONUP:
      case K_MDBLCLK:
         iMask = INKEY_MMIDDLE;
         break;
      case K_MWFORWARD:
      case K_MWBACKWARD:
         iMask = INKEY_MWHEEL;
         break;
      default:
         iMask = INKEY_KEYBOARD;
         break;
   }

   if( ( iMask & ( int ) event_mask ) == 0 )
      return 0;

   return iKey;
}



/* Put the key into keyboard buffer */
void HB_EXPORT hb_inkeyPut( int iKey )
{
   int iHead = s_inkeyHead;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyPut(%d)", iKey));

   s_inkeyBuffer[ iHead++ ] = iKey;
   if( iHead >= s_inkeyBufferSize )
   {
      iHead = 0;
   }

   if( iHead != s_inkeyTail )
   {
      s_inkeyHead = iHead;
   }
}

/* drop the next key in keyboard buffer */
static void hb_inkeyPop( void )
{
   if( s_StrBuffer )
   {
      if( ++s_StrBufferPos >= s_StrBufferSize )
      {
         hb_xfree( s_StrBuffer );
         s_StrBuffer = NULL;
      }
   }
   else if( s_inkeyHead != s_inkeyTail )
   {
      if( ++s_inkeyTail >= s_inkeyBufferSize )
         s_inkeyTail = 0;
   }
}

static BOOL hb_inkeyNextCheck( HB_inkey_enum event_mask, int * iKey )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_inkeyNextCheck(%p)", iKey) );

   if( s_StrBuffer )
   {
      *iKey = s_StrBuffer[ s_StrBufferPos ];
   }
   else if( s_inkeyHead != s_inkeyTail )
   {
      *iKey = hb_inkeyTranslate( s_inkeyBuffer[ s_inkeyTail ], event_mask );
   }
   else
   {
      return FALSE;
   }

   if( *iKey == 0 )
   {
      hb_inkeyPop();
      return FALSE;
   }

   return TRUE;
}

static void hb_inkeyPollDo( void )
{
   int iKey;

   HB_TRACE( HB_TR_DEBUG, ("hb_inkeyPollDo()") );

   iKey = hb_gt_ReadKey( ( HB_inkey_enum ) INKEY_ALL );

   switch( iKey )
   {
      case HB_BREAK_FLAG:           /* Check for Ctrl+Break */
      case K_ALT_C:                 /* Check for normal Alt+C */
         if( hb_set.HB_SET_CANCEL )
         {
            hb_vmRequestCancel();   /* Request cancellation */
            return;
         }
         break;
      case K_ALT_D:                 /* Check for normal Alt+D */
         if( hb_set.HB_SET_DEBUG )
         {
            hb_vmRequestDebug();    /* Request the debugger */
            return;
         }
   }

   if( iKey )
   {
      hb_inkeyPut( iKey );
   }
}

/* Poll the console keyboard to stuff the Harbour buffer */
void hb_inkeyPoll( void )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_inkeyPoll()") );

   /*
    * Clipper 5.3 always poll events without respecting
    * hb_set.HB_SET_TYPEAHEAD when CL5.2 only when it's non zero.
    * IMHO keeping CL5.2 behavior will be more accurate for xharbour
    * because it allow to control it by user what some times could be
    * necessary due to different low level GT behavior on some platforms
    */
   if( hb_set.HB_SET_TYPEAHEAD )
   {
      hb_inkeyPollDo();
   }
}

/* Return the next key without extracting it */
int hb_inkeyNext( HB_inkey_enum event_mask )
{
   int iKey = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyNext(%d)", event_mask));

   hb_inkeyPoll();
   hb_inkeyNextCheck( event_mask, &iKey );

   return iKey;
}

int hb_inkey( BOOL fWait, double dSeconds, HB_inkey_enum event_mask )
{
   clock_t end_clock = 0;
   BOOL fPop;

#if defined( HB_OS_UNIX )
   /* NOTE: clock() returns a time used by a program - if it is suspended
    * then this time will be zero
    */
   struct tms tm;
   #define _HB_CUR_CLOCK() times( &tm )
   #define _HB_CLOCK_TICK  sysconf(_SC_CLK_TCK)
#else
   #define _HB_CUR_CLOCK() clock()
   #define _HB_CLOCK_TICK  CLOCKS_PER_SEC
#endif

   HB_TRACE(HB_TR_DEBUG, ("hb_inkey(%d, %f, %d)", (int) fWait, dSeconds, (int) event_mask));

   /* Wait forever ?, Use fixed value 100 for strict Clipper compatibility */
   if( fWait && dSeconds * 100 >= 1 )
   {
      end_clock = _HB_CUR_CLOCK() + ( clock_t ) ( dSeconds * _HB_CLOCK_TICK );
   }

   do
   {
      hb_inkeyPollDo();
      fPop = hb_inkeyNextCheck( event_mask, &s_inkeyLast );

      if( fPop )
      {
         break;
      }

      /* immediately break if a VM request is pending. */
      if( !fWait || hb_vmRequestQuery() != 0 )
      {
         return 0;
      }

      hb_idleState( TRUE );
   }
   while( end_clock == 0 || end_clock >  _HB_CUR_CLOCK() );

   hb_idleReset();

   if( fPop )
   {
      hb_inkeyPop();
      return s_inkeyLast;
   }

   return 0;
}

/* Return the value of the last key that was extracted */
int hb_inkeyLast( HB_inkey_enum event_mask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyLast(%d)", event_mask));

   hb_inkeyPoll();

   return hb_inkeyTranslate( s_inkeyLast, event_mask );
}

/* Force a value to s_inkeyLast and return previous value */
int hb_setInkeyLast( int iKey )
{
   int iLast = s_inkeyLast;

   HB_TRACE(HB_TR_DEBUG, ("hb_setInkeyLast(%d)", iKey));

   s_inkeyLast = iKey;

   return iLast;
}

/* Reset the keyboard buffer */
void hb_inkeyReset( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyReset()"));

   if( s_StrBuffer )
   {
      hb_xfree( s_StrBuffer );
      s_StrBuffer = NULL;
   }

   s_inkeyHead = 0;
   s_inkeyTail = 0;

   if( hb_set.HB_SET_TYPEAHEAD != s_inkeyBufferSize )
   {
      if( s_inkeyBufferSize > HB_DEFAULT_INKEY_BUFSIZE )
      {
         hb_xfree( s_inkeyBuffer );
      }
      if( hb_set.HB_SET_TYPEAHEAD > HB_DEFAULT_INKEY_BUFSIZE )
      {
         s_inkeyBufferSize = hb_set.HB_SET_TYPEAHEAD;
         s_inkeyBuffer = ( int * ) hb_xgrab( s_inkeyBufferSize * sizeof( int ) );
      }
      else
      {
         s_inkeyBufferSize = HB_DEFAULT_INKEY_BUFSIZE;
         s_inkeyBuffer = s_defaultKeyBuffer;
      }
   }
}

void hb_inkeyExit( void )
{
   if( s_inKeyBlockBefore )
   {
      hb_itemRelease( s_inKeyBlockBefore );
      s_inKeyBlockBefore = NULL;
   }
   if( s_inKeyBlockAfter )
   {
      hb_itemRelease( s_inKeyBlockAfter );
      s_inKeyBlockAfter = NULL;
   }

   if( s_StrBuffer )
   {
      hb_xfree( s_StrBuffer );
      s_StrBuffer = NULL;
   }
   if( s_inkeyBufferSize > HB_DEFAULT_INKEY_BUFSIZE )
   {
      hb_xfree( s_inkeyBuffer );
      s_inkeyBufferSize = HB_DEFAULT_INKEY_BUFSIZE;
      s_inkeyBuffer = s_defaultKeyBuffer;
   }
}

HB_FUNC( INKEY )
{
   USHORT uiPCount = hb_pcount();
   HB_ITEM Key;
   int iKey;

   if( s_inKeyBlockBefore )
   {
      hb_vmEvalBlock( s_inKeyBlockBefore );
   }

   do
   {
      iKey = hb_inkey( uiPCount == 1 || ( uiPCount > 1 && ISNUM( 1 ) ),
                       hb_parnd( 1 ),
                       ISNUM( 2 ) ? ( HB_inkey_enum ) hb_parni( 2 ) :
                                    hb_set.HB_SET_EVENTMASK );

      if( iKey == 0 || !s_inKeyBlockAfter )
         break;

      Key.type = HB_IT_NIL;
      hb_itemPutNI( &Key, iKey );
      iKey = hb_itemGetNI( hb_vmEvalBlockV( s_inKeyBlockAfter, 1, &Key ) );
      hb_setInkeyLast( iKey );
   }
   while( iKey == 0 );

   hb_retni( iKey );
}

HB_FUNC( SETINKEYBEFOREBLOCK )
{
   USHORT uiPCount = hb_pcount();

   if( s_inKeyBlockBefore )
   {
      hb_itemReturn( s_inKeyBlockBefore );
   }
   else
   {
      hb_ret();
   }

   if( uiPCount > 0 )
   {
      PHB_ITEM pBlock = hb_param( 1, HB_IT_BLOCK );

      if( s_inKeyBlockBefore )
      {
         hb_itemRelease( s_inKeyBlockBefore );
      }
      if( pBlock )
      {
         s_inKeyBlockBefore = hb_itemNew( pBlock );
      }
      else
      {
         s_inKeyBlockBefore = NULL;
      }
   }
}

HB_FUNC( SETINKEYAFTERBLOCK )
{
   USHORT uiPCount = hb_pcount();

   if( s_inKeyBlockAfter )
   {
      hb_itemReturn( s_inKeyBlockAfter );
   }
   else
   {
      hb_ret();
   }

   if( uiPCount > 0 )
   {
      PHB_ITEM pBlock = hb_param( 1, HB_IT_BLOCK );

      if( s_inKeyBlockAfter )
      {
         hb_itemRelease( s_inKeyBlockAfter );
      }
      if( pBlock )
      {
         s_inKeyBlockAfter = hb_itemNew( pBlock );
      }
      else
      {
         s_inKeyBlockAfter = NULL;
      }
   }
}

HB_FUNC( __KEYBOARD )
{
   /* Clear the typeahead buffer without reallocating the keyboard buffer */
   hb_inkeyReset();

   if( ISCHAR( 1 ) )
   {
      ULONG ulSize = hb_parclen( 1 );

      /* It might be just a request to clear the buffer */
      if( ulSize != 0 )
      {
         s_StrBuffer = ( BYTE * ) hb_xgrab( ulSize );
         memcpy( s_StrBuffer, hb_parc( 1 ), ulSize );
         s_StrBufferSize = ulSize;
         s_StrBufferPos = 0;
      }
   }
#if defined( HB_EXTENSION )
   else if( ISNUM( 1 ) )
   {
      hb_inkeyPut( hb_parni(1) );
   }
#endif
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
   hb_retni( hb_inkeyNext( ISNUM( 1 ) ? ( HB_inkey_enum ) hb_parni( 1 ) :
                                        hb_set.HB_SET_EVENTMASK ) );
}

HB_FUNC( LASTKEY )
{
   hb_retni( hb_inkeyLast( ISNUM( 1 ) ? ( HB_inkey_enum ) hb_parni( 1 ) :
                                        hb_set.HB_SET_EVENTMASK ) );
}

HB_FUNC( SETLASTKEY )
{
   if( ISNUM(1) )
   {
      hb_retni( hb_setInkeyLast( hb_parni(1) ) );
   }
   else
   {
      hb_ret();
   }
}
