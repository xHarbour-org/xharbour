/*
 * $Id: mousesln.c,v 1.1.1.1 2001/12/21 10:42:31 ronpinkas Exp $
 */

/*
 * Harbour Project source code:
 * Mouse subsystem for plain ANSI C stream IO (stub)
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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

/* NOTE: This file is a simple stub for those platforms which don't have
         any kind of mouse support. [vszakats] */

/* *********************************************************************** */

#include "gtsln.h"
#include <sys/time.h>

/* *********************************************************************** */

#define INKEY_UP ( INKEY_LUP | INKEY_RUP )

typedef struct _hbgtMouseEvent_
{
   unsigned int Btn;
   unsigned int Col;
   unsigned int Row;
   USHORT       Key;
   struct timeval Time;
} HB_MouseEvent;

static HB_MouseEvent s_LastMouseEvent = { 0, 0, 0, 0, { 0, 0 }  };

/* *********************************************************************** */

int hb_gt_MouseInkey( HB_inkey_enum eventmask )
{
    unsigned int Btn, Col, Row;
    
    /* mouse event consists of three chars under xterm */
    if( SLang_input_pending( 0 ) > 0 )
    {
	Btn = SLang_getkey();
        if( SLang_input_pending( 0 ) > 0 )
	{
	    Col = SLang_getkey();
            if( SLang_input_pending( 0 ) > 0 )
    	    {
	        struct timeval CurrTime, LastTime;
	        struct timezone TimeZone;
		
		Row = SLang_getkey();
		
		/* get the time of a mouse event */
		LastTime = s_LastMouseEvent.Time;
	        gettimeofday( &CurrTime, &TimeZone );
		s_LastMouseEvent.Time = CurrTime;
		
		s_LastMouseEvent.Btn = Btn;
		/* mouse event position */
		s_LastMouseEvent.Col = Col - 33;
		s_LastMouseEvent.Row = Row - 33;

		/* any button was released */
		/* we don't know which one */
	        if( ( Btn & 0x03 ) == 0x03 )
		{
		    switch ( s_LastMouseEvent.Key )
		    {
			case K_LBUTTONDOWN :
		    	    s_LastMouseEvent.Key = K_LBUTTONUP;
			    if( ( eventmask & INKEY_LUP ) != 0 )
				return( s_LastMouseEvent.Key );
			    break;
			case K_RBUTTONDOWN :
		    	    s_LastMouseEvent.Key = K_RBUTTONUP;
			    if( ( eventmask & INKEY_RUP ) != 0 )
				return( s_LastMouseEvent.Key );
			    break;
			default :
		    	    s_LastMouseEvent.Key = K_LBUTTONUP;
		    }
		}

                /* left button was pressed */
	        else if( ( Btn & 0x03 ) == 0x00 )
		{
		    USHORT LastKey = s_LastMouseEvent.Key;
	    	    s_LastMouseEvent.Key = K_LBUTTONDOWN;
		    
		    if( ( eventmask & INKEY_LDOWN ) != 0 )
		    {
			/* check the double click */
			if( LastKey == K_LBUTTONDOWN )
			{
			    long DiffTime = ( CurrTime.tv_sec - LastTime.tv_sec );
			    
			    if( DiffTime < 2 )
			    {
				DiffTime = DiffTime * 1000000 + 
				    ( CurrTime.tv_usec - LastTime.tv_usec );

    				if( DiffTime < 500000 )
				{
			    	    s_LastMouseEvent.Key = 0;
				    return( K_LDBLCLK );
				}
			    }
			}
			
			return( s_LastMouseEvent.Key );
		    }
		}

                /* right button was pressed */
	        else if( ( Btn & 0x03 ) == 0x02 )
		{
		    USHORT LastKey = s_LastMouseEvent.Key;
	    	    s_LastMouseEvent.Key = K_RBUTTONDOWN;
		    
		    if( ( eventmask & INKEY_RDOWN ) != 0 )
		    {
			/* check the double click */
			if( LastKey == K_RBUTTONDOWN )
			{
			    long DiffTime = ( CurrTime.tv_sec - LastTime.tv_sec );
			    
			    if( DiffTime < 2 )
			    {
				DiffTime = DiffTime * 1000000 + 
				    ( CurrTime.tv_usec - LastTime.tv_usec );

    				if( DiffTime < 500000 )
				{
			    	    s_LastMouseEvent.Key = 0;
				    return( K_RDBLCLK );
				}
			    }
			}
			
			return( s_LastMouseEvent.Key );
		    }
		}
    	    }
        }
    }

    return( 0 );
}

/* *********************************************************************** */

void hb_mouse_Init( void )
{
    if( hb_gt_UnderXTerm )
        /* force mouse usage under xterm */
	(void) SLtt_set_mouse_mode (1, 1);
}

/* *********************************************************************** */

void hb_mouse_Exit( void )
{
    ;
}

/* *********************************************************************** */

BOOL hb_mouse_IsPresent( void )
{
    if( hb_gt_UnderXTerm )
	return TRUE;
    else
	return FALSE;
}

/* *********************************************************************** */

void hb_mouse_Show( void )
{
   ;
}

/* *********************************************************************** */

void hb_mouse_Hide( void )
{
   ;
}

/* *********************************************************************** */

int hb_mouse_Col( void )
{
   return s_LastMouseEvent.Col;
}

/* *********************************************************************** */

int hb_mouse_Row( void )
{
   return s_LastMouseEvent.Row;
}

/* *********************************************************************** */

void hb_mouse_SetPos( int iRow, int iCol )
{
   /* it does really nothing */
   s_LastMouseEvent.Col = iCol;
   s_LastMouseEvent.Row = iRow;
}

/* *********************************************************************** */

BOOL hb_mouse_IsButtonPressed( int iButton )
{
   HB_SYMBOL_UNUSED( iButton );

   return FALSE;
}

/* *********************************************************************** */

int hb_mouse_CountButton( void )
{
    return( SLtt_tgetnum( "BT" ) );
/*
    int BtnsNum = SLtt_tgetnum( "BT" );
    return( ( BtnsNum == -1 ) ? 0 : BtnsNum );

    if( BtnsNum != -1 )
        return( BtnsNum );
    else
        return( 0 );
*/
}

/* *********************************************************************** */

void hb_mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight )
{
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( iBottom );
   HB_SYMBOL_UNUSED( iRight );
}

/* *********************************************************************** */

void hb_mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   HB_SYMBOL_UNUSED( piTop );
   HB_SYMBOL_UNUSED( piLeft );
   HB_SYMBOL_UNUSED( piBottom );
   HB_SYMBOL_UNUSED( piRight );
}

/* *********************************************************************** */
