/*
 * $Id: mousesln.c,v 1.3 2002/04/16 16:12:18 map Exp $
 */

/*
 * Harbour Project source code:
 * Mouse subsystem for gtsln 
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

/* *********************************************************************** */

#include "gtsln.h"
#include <sys/time.h>
#ifdef HAVE_GPM_H
    #include <sys/types.h>
    #include <gpm.h>
    Gpm_Connect Conn;
#endif
    
#define GET_TIME(tv)    (gettimeofday(&tv, (struct timezone *)NULL))
#define DIF_TIME(t1,t2) ((t2->tv_sec -t1->tv_sec) *1000+ \
          (t2->tv_usec-t1->tv_usec)/1000)
#define DIF_TIME1(t1,t2) ((t2.tv_sec -t1.tv_sec) *1000+ \
          (t2.tv_usec-t1.tv_usec)/1000)

/* *********************************************************************** */

typedef struct _hbgtMouseEvent_
{
    unsigned int   Col;
    unsigned int   Row;
    unsigned int   LastCol;
    unsigned int   LastRow;
    USHORT         Key;  /* to analize DBLCLK on xterm */
    struct timeval Time;
} HB_MouseEvent;

static HB_MouseEvent s_LastMouseEvent = { 0, 0, 0, 0, 0, { 0, 0 }  };
static BOOL s_bMousePresent = FALSE;
static int  s_iMouseButtons = -1;
static int  bFirstRelease = 1;

/* *********************************************************************** */

static BOOL GetXtermEvent( unsigned int *Btn, unsigned int *Col, unsigned int *Row )
{
    /* Xterm mouse event consists of three chars */
    if( SLang_input_pending( 0 ) > 0 )
    {
        *Btn = SLang_getkey();

        if( SLang_input_pending( 0 ) > 0 )
        {
            *Col = SLang_getkey();
            if( SLang_input_pending( 0 ) > 0 )
            {
                *Row =SLang_getkey();
                return( TRUE );
            }
        }
    }
    
    return( FALSE );
}

/* *********************************************************************** */

#ifdef HAVE_GPM_H
static BOOL GetGpmEvent( Gpm_Event *Evt )
{
    if( s_bMousePresent )
    {
        struct timeval tv = { 0, 0 };
        fd_set ReadFD;

        FD_ZERO( &ReadFD ); 
        FD_SET( gpm_fd, &ReadFD );

        if( select( gpm_fd+1, &ReadFD, NULL, NULL, &tv ) > 0 )
            if( FD_ISSET( gpm_fd, &ReadFD ) ) 
                return( Gpm_GetEvent( Evt ) > 0 );
    }

    return( FALSE );
}
#endif    

/* *********************************************************************** */

int hb_mouse_Inkey( HB_inkey_enum EventMask )
{
    struct timeval  CurrTime;
    struct timezone TimeZone;
    /* Force first click as single */
    static struct timeval tv1 = { 0, 0 }  ;
    static struct timeval tv2;
    int ch;

    if( hb_gt_UnderXterm )  
    {
        unsigned int Btn, Col, Row;
                
        if( GetXtermEvent( &Btn, &Col, &Row ) )
        {
            struct timeval LastTime;
            
            LastTime = s_LastMouseEvent.Time;
            s_LastMouseEvent.Time = tv1;
            
            s_LastMouseEvent.LastCol = s_LastMouseEvent.Col ;
            s_LastMouseEvent.LastRow = s_LastMouseEvent.Row ;          

            s_LastMouseEvent.Col = Col - 33 ;
            s_LastMouseEvent.Row = Row - 33 ;

            if( EventMask & INKEY_MOVE && ( s_LastMouseEvent.LastRow != s_LastMouseEvent.Row || s_LastMouseEvent.LastCol != s_LastMouseEvent.Col ) ) 
                ch = K_MOUSEMOVE;

            GET_TIME( tv2 );
            
            if( EventMask & INKEY_LDOWN && ( ( Btn & 0x03 ) == 0x00 ) )
            {
                if( DIF_TIME1( tv1, tv2 ) <= 250  )
                    if( bFirstRelease )
                        ch = K_LBUTTONDOWN;
                    else
                        ch = K_LDBLCLK;
                else
                    ch = K_LBUTTONDOWN;

                 bFirstRelease = 0;
                 s_LastMouseEvent.Key = K_LBUTTONDOWN;
            }
            else if( EventMask & INKEY_RDOWN && ( ( Btn & 0x03 ) == 0x02 ) )
            {
                if( DIF_TIME1( tv1, tv2 ) <= 250 && !bFirstRelease ) 
                    if( bFirstRelease )
                        ch = K_RBUTTONDOWN;
                    else
                        ch = K_RDBLCLK;
                else
                    ch = K_RBUTTONDOWN;
                    
                bFirstRelease = 0;
                s_LastMouseEvent.Key = K_RBUTTONDOWN;
            }
            else if( ( Btn & 0x03 ) == 0x03 )
            {
                if( EventMask & INKEY_LUP && s_LastMouseEvent.Key == K_LBUTTONDOWN )
                    ch = K_LBUTTONUP;
                else if( EventMask & INKEY_RUP && s_LastMouseEvent.Key == K_RBUTTONDOWN )
                    ch = K_RBUTTONUP;
                    
                GET_TIME( tv1 );
            }

            return ch;
        }
   }

#ifdef HAVE_GPM_H

#define CHECK_BUTTON_DOWN(Mask,GpmBtn,InkBtn,InkDbl) \
    if( ( EventMask & Mask ) && ( Evt.buttons & GpmBtn ) ) \
    {                                    \
        if( Evt.type & GPM_SINGLE )      \
            return( InkBtn );            \
        else if( Evt.type & GPM_DOUBLE ) \
            return( InkDbl );            \
    }

    else if( hb_gt_UnderLinuxConsole )
    {
        Gpm_Event Evt;

        if( GetGpmEvent( &Evt ) )
        {
            /* get the mouse event position */
            s_LastMouseEvent.Col = Evt.x;
            s_LastMouseEvent.Row = Evt.y;

            /* get the time of a mouse event */
            gettimeofday( &CurrTime, &TimeZone );
            s_LastMouseEvent.Time = CurrTime;

            if( ( Evt.type & GPM_MOVE ) && ( EventMask & INKEY_MOVE ) )
                return( K_MOUSEMOVE );

            else if( Evt.type & GPM_DOWN )
            {
                CHECK_BUTTON_DOWN(INKEY_LDOWN,GPM_B_LEFT,K_LBUTTONDOWN,K_LDBLCLK)
                else
                CHECK_BUTTON_DOWN(INKEY_RDOWN,GPM_B_RIGHT,K_RBUTTONDOWN,K_RDBLCLK)
            }

            else if( Evt.type & GPM_UP )
            {
                if( ( EventMask & INKEY_LUP ) && ( Evt.buttons & GPM_B_LEFT ) )
                    return( K_LBUTTONUP );
                else
                if( ( EventMask & INKEY_RUP ) && ( Evt.buttons & GPM_B_RIGHT ) )
                    return( K_RBUTTONUP );
            }
        }
    }
#endif

    return 0;
}

/* *********************************************************************** */

void hb_mouse_Init( void )
{
    if( hb_gt_UnderXterm )
    {
        char * SaveHilit = "\033[?1001s"; /* save old hilit tracking */
        char * EnabTrack = "\033[?1000h"; /* enable mouse tracking */
   
        /* force mouse usage under xterm */
        (void)SLtt_set_mouse_mode( 1, 1 );
   
        /* initial xterm settings */
        SLtt_write_string( SaveHilit );
        SLtt_write_string( EnabTrack );
        SLtt_flush_output();

        s_iMouseButtons = SLtt_tgetnum( "BT" );
        
        /* force two buttons mouse under xterm */
        if( s_iMouseButtons == -1 )
            s_iMouseButtons = 2;

        s_bMousePresent = TRUE;
    }

#ifdef HAVE_GPM_H
    else if( hb_gt_UnderLinuxConsole )
    {

        Conn.eventMask = GPM_MOVE | GPM_UP | GPM_DOWN | GPM_DRAG | GPM_DOUBLE;
        /* give me move events but handle them anyway */
        Conn.defaultMask= GPM_MOVE | GPM_HARD; 
        /* only pure mouse events, no Ctrl,Alt,Shft events */
        Conn.minMod = 0;    Conn.maxMod = 0;
        gpm_zerobased = 1;  gpm_visiblepointer = 1;

        if( Gpm_Open( &Conn, 0 ) >= 0 )
        {
            Gpm_Event Evt;

            s_bMousePresent = TRUE;
            
            while( GetGpmEvent( &Evt ) );
            {
                s_LastMouseEvent.Col = Evt.x;
                s_LastMouseEvent.Row = Evt.y;
            }

            s_iMouseButtons = Gpm_GetSnapshot( NULL );
            hb_mouse_FixTrash();
        }
    }
#endif
}

/* *********************************************************************** */

void hb_mouse_Exit( void )
{
    if( hb_gt_UnderXterm )
    {
        char * DisabTrack = "\033[?1000l"; /* disable mouse tracking */
        char * RestoHilit = "\033[?1001r"; /* restore old hilittracking */
   
        /* restore xterm settings */
        SLtt_write_string( DisabTrack );
        SLtt_write_string( RestoHilit );
        SLtt_flush_output();

        /* force mouse usage under xterm */
        (void)SLtt_set_mouse_mode( 0, 1 );
    }
    
#ifdef HAVE_GPM_H
    else if( hb_gt_UnderLinuxConsole )
        if( gpm_fd >= 0 ) 
            Gpm_Close();
#endif
}

/* *********************************************************************** */

BOOL hb_mouse_IsPresent( void )
{
    return s_bMousePresent;
}

/* *********************************************************************** */

void hb_mouse_Show( void )
{
#ifdef HAVE_GPM_H
    gpm_visiblepointer = 1;
    if( hb_gt_UnderLinuxConsole && s_bMousePresent )
        Gpm_DrawPointer( s_LastMouseEvent.Col, s_LastMouseEvent.Row, gpm_consolefd );
#endif
   ;
}

/* *********************************************************************** */

void hb_mouse_Hide( void )
{
#ifdef HAVE_GPM_H
    gpm_visiblepointer = 0;
#endif
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
#ifdef HAVE_GPM_H
    if( hb_gt_UnderLinuxConsole )
        if( s_bMousePresent && gpm_visiblepointer )
            Gpm_DrawPointer( iCol, iRow, gpm_consolefd );
#endif
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
    return( s_iMouseButtons );
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

void hb_mouse_FixTrash()
{
#ifdef HAVE_GPM_H
    if( hb_gt_UnderLinuxConsole )
        if( s_bMousePresent && gpm_visiblepointer )
            Gpm_DrawPointer( s_LastMouseEvent.Col, s_LastMouseEvent.Row, gpm_consolefd );
#endif
}

/* *********************************************************************** */
