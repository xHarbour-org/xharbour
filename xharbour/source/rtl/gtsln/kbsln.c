/*
 * $Id: kbsln.c,v 1.9 2003/05/16 19:52:11 druzus Exp $
 */

/*
 * Harbour Project source code:
 * Keyboard subsystem based on Slang screen library.
 *
 * Copyright 2000 Marek Paliwoda <paliwoda@inetia.pl>
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

/* NOTE: User programs should never call this layer directly! */

/* *********************************************************************** */

#include "gtsln.h"
#include <sys/ioctl.h>

#ifdef M_UNIX
   #include <sys/termio.h>
   #include <sys/vtkd.h>
#endif

/* we're assuming target has termios - should be better done */
#include <termios.h> 
#define HB_SLANG_ONE_ESC
/* *********************************************************************** */

/* keyboard states - these should be taken
   from system includes, not be hard coded */
#if defined(__linux__)
   #define SHIFT_PRESSED        1
   #define ALTR_PRESSED         2
   #define CONTROL_PRESSED      4
   #define ALTL_PRESSED         8
   #define ALT_PRESSED ALTL_PRESSED
#elif defined(M_UNIX)         /* SCO */
   #define SHIFT_PRESSED        1
   #define ALTR_PRESSED         8
   #define CONTROL_PRESSED      2
   #define ALTL_PRESSED         4
   #define ALT_PRESSED ALTL_PRESSED
#else /* we don't know how to do this */
   #define SHIFT_PRESSED        0
   #define ALTR_PRESSED         0
   #define CONTROL_PRESSED      0
   #define ALTL_PRESSED         0
   #define ALT_PRESSED ALTL_PRESSED
#endif
#define HB_GT_KBD_MODIF_MASK \
    ( ( SHIFT_PRESSED | CONTROL_PRESSED | ALTL_PRESSED ) << 16 )

#define MOUSE_ALL_EVENTS_MASK \
    ( INKEY_MOVE | INKEY_LDOWN | INKEY_LUP | INKEY_RDOWN | INKEY_RUP )
    
/* extra keysyms definitions */
#define SL_KEY_NUM_5      SL_KEY_B2       /* this is checked explicitly */
#define SL_KEY_MAX        ( ( unsigned int ) 0x2000 )
#define SL_KEY_ESC        ( SL_KEY_MAX + 1 )
#define SL_KEY_MOU        ( SL_KEY_ESC + 1 )
#define SL_KEY_ALT( ch )  ( SL_KEY_MAX + ( ( unsigned int ) ch ) )

/* we choose Ctrl+\ as an abort key on Unixes where it is a SIGQUIT key by default */
/* abort key is Ctrl+\ on Unix ( but Ctrl+@ on Linux console ) */
static int s_hb_gt_Abort_key = 28;

/* *********************************************************************** */

/* indicates that screen size has changed */
extern BOOL hb_gt_sln_bScreen_Size_Changed;

/* DeadKey definition's ENVVAR name. This EnvVar contains */
/* an ASCII value of a key, which serves as a DeadKey */
unsigned char *hb_DeadKeyEnvName = "HRBNATIONDEADKEY";

/* a table for Keys work with a Dead key. The first
   element contains a number of defined keys */
unsigned char s_convKDeadKeys[ 257 ];  /* it should be allocated by hb_xalloc() */

/* contains an integer value of a DeadKey or -1 */
int hb_DeadKey = -1;

static int HB_GT_FUNC(gt_try_get_Kbd_State());

/* key translations tables - notice problems with compilation after changes */
#include "keytrans.c"

/* *********************************************************************** */

static void HB_GT_FUNC(gt_Init_KeyTranslations())
{
    char ch, keyname[ SLANG_MAX_KEYMAP_KEY_SEQ + 1 ];
    int  keynum, i;
    char * keyseq;
 
    /* for defining ^[<Key> sequences - this simulates Alt+Keys */
    char AltChars[][ 2 ] =
    {
        { '0',   '9' },
        { 'A',   'Z' },
        { 'a',   'z' }
    };
 
    /* on Unix systems ESC is a special key so let
       assume ESC is a doble pressed ESC key    */
    SLkp_define_keysym( "^[^[", SL_KEY_ESC );
 
    /* try to define Shft-Fn and Ctrl-Fn keys.
       Because we assume terminal has only 10 Fkeys
       so F11-F30 is generated with Shift & Ctrl.
       This is not guaranteed to work in all cases */
    keynum = 11;
    keyname[ 0 ] = 'F';
    keyname[ 2 ] = 0;
 
    /* Shft & Ctrl FKeys definition takes place in two
       phases : from '1' to '9' and from 'A' to 'K' */
    for( i = 1; i <= 2; i++ )
    {
        for( ch = ( i == 1 ? '1' : 'A' ); ch <= ( i == 1 ? '9' : 'K' ); ch++ )
        {
            keyname[ 1 ] = ch;
            keyseq = SLtt_tgetstr( keyname );
            if( ( keyseq != NULL ) && ( keyseq[ 0 ] != 0 ) )
                SLkp_define_keysym( keyseq, SL_KEY_F( keynum ) );
            keynum++;
        }
    }
 
    /* We assume Esc key is a Meta key which is treated as an Alt key.
       Also pressing Alt+Key on linux console and linux xterm gives the
       same key sequences as with Meta key so we are happy */
 
    keyname[ 0 ] = 033;
    keyname[ 2 ] = 0;
 
    /* Alt+Letter & Alt+digit definition takes place in three phases :
       from '0' to '9', from 'A' to 'Z' and from 'a' to 'z' */
    for( i = 0; i < 3; i++ )
    {
        for( ch = AltChars[ i ][ 0 ]; ch <= AltChars[ i ][ 1 ]; ch++ )
        {
            /* fprintf( stderr, "%d %c\n", i, ch ); */
            keyname[ 1 ] = ch;
   
            /* QUESTION: why Slang reports error for defining Alt+O ???.
                         Have I any hidden error in key definitiions ??? */
            if( ch != 'O' )
                SLkp_define_keysym( keyname, SL_KEY_ALT( ch ) );
        }
    }
 
    /* mouse events under xterm */
    if( hb_gt_UnderXterm )
    {
        keyseq = SLtt_tgetstr( "Km" );
        if( ( keyseq != NULL ) && ( keyseq[ 0 ] != 0 ) )
        {
            //fprintf( stderr, "%s\r\n", keyseq );
            SLkp_define_keysym( keyseq, SL_KEY_MOU );
        }
    }
    
    /* five on numeric console */
    /* SLkp_define_keysym( "^[[G", SL_KEY_NUM_5 ); */
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_Init_Terminal( int phase ))
{
    struct termios newTTY;
    unsigned char * p;
    int ret = 0;
 
    /* first time init phase - we don't want this after
       return from system command ( see run.c )      */
    if( phase == 0 )
    {
        /* check if we run under linux console or under xterm */
        HB_GT_FUNC(gt_Init_TermType());
       
#ifdef __linux__
        /* for Linux console */
        if( hb_gt_UnderLinuxConsole )
            s_hb_gt_Abort_key = 0;
#endif
 
        /* get Dead key definition */
        p = hb_getenv( hb_DeadKeyEnvName );
 
        if( p && p[ 0 ] != '\0' )
        {
            int len = strlen( p );
            if( len > 0 )
                hb_DeadKey = ( int ) *p;
        }
        if( p )
            hb_xfree( ( void * ) p );
 
        /* number of keys dealing with a Dead key */
        s_convKDeadKeys[ 0 ] = 0;
    }
 
    /* Ctrl+\ to abort, no flow-control, no output processing */
    if( SLang_init_tty( s_hb_gt_Abort_key, 0, 0 ) != -1 )
    {
        /* do missing disable of start/stop processing */
        if( tcgetattr( SLang_TT_Read_FD, &newTTY ) == 0 )
        {
            newTTY.c_cc[ VSTOP ]  = 255;  /* disable ^S start/stop processing */
            newTTY.c_cc[ VSTART ] = 255;  /* disable ^Q start/stop processing */
            newTTY.c_cc[ VSUSP ]  = 255;  /* disable ^Z suspend processing */
            /* already done in Slang library */
            /* newTTY.c_cc[ VDSUSP ] = 255; */  /* disable ^Y delayed suspend processing */

            if( tcsetattr( SLang_TT_Read_FD, TCSADRAIN, &newTTY ) == 0 )
               /* everything looks ok so far */
               ret = 1;
        }
    }
 
    /* first time init phase - we don't want this after
       return from system command ( see run.c )      */
    if( ret && ( phase == 0 ) )
    {
        /* define keyboard translations */
        HB_GT_FUNC(gt_Init_KeyTranslations());
  
        /* for binary search of key translations */
        HB_GT_FUNC(gt_SortKeyTranslationTable());
    }
 
    return( ret );
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_ExtendedKeySupport())
{
    return( 0 );
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_ReadKey( HB_inkey_enum eventmask ))
{
    static int InDeadState = FALSE;
    unsigned int ch, tmp, kbdflags;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

    /* HB_SYMBOL_UNUSED( eventmask ); */

    /* user AbortKey break */
    if( SLKeyBoard_Quit == 1 )
        return( HB_BREAK_FLAG );

    /* has screen size changed ? */
    if( hb_gt_sln_bScreen_Size_Changed )
    {
        hb_gt_sln_bScreen_Size_Changed = FALSE;
        SLtt_get_screen_size();
#if SLANG_VERSION > 10202
        SLsmg_reinit_smg();
#endif
        /* TODO: we need here some kind of screen redrawing */
        /*SLsmg_refresh();*/
    }

    /* if no pending chars */
    if( 0 == SLang_input_pending( 0 ) )
        return( HB_GT_FUNC(mouse_Inkey( eventmask )) );

#if HB_GT_KBD_MODIF_MASK
    kbdflags = HB_GT_FUNC(gt_try_get_Kbd_State());
#else
    kbdflags = 0;
#endif

/* ------- one key ESC handling ----------------- */
    /* NOTE: This will probably not work on slow terminals
       or on very busy lines (i.e. modem lines ) */
    ch = SLang_getkey();
#ifdef HB_SLANG_ONE_ESC
    if( ch == 033 )   /* escape pressed - wait 0.6 sec for another key */
        if( 0 == SLang_input_pending( 0.3))
            return( 033 );

#else

    if( ch == 033 )   /* escape pressed - wait 0.6 sec for another key */
        if( 0 == SLang_input_pending( 6 ) )
            return( 0 );
#endif
    /* user AbortKey break */
    if( ch == s_hb_gt_Abort_key )
        return( HB_BREAK_FLAG );

    SLang_ungetkey( ch );
/* ------------------------------------------------- */

    ch = SLkp_getkey();

    /* unrecognized character */
    if( ch == SL_KEY_ERR )
        return( 0 );

    /* Dead key handling */
    if( InDeadState )
    {
        InDeadState = FALSE;
        if( ch == hb_DeadKey ) /* double press Dead key */
            return( ch );
        if( ch < 256 )  /* is this needed ??? */
        {
            int i;
            for( i=0; i < ( int ) s_convKDeadKeys[ 0 ]; i++ )
                if( ( int ) s_convKDeadKeys[ 2 * i + 1 ] == ch )
                    return( ( int ) s_convKDeadKeys[ 2 * i + 2 ] );
        }
        return( 0 );
    }
    else if( ch == hb_DeadKey )
    {
        /* entering Dead key state */
        InDeadState = TRUE;
        return( 0 );
    }

    /* any special key ? */
    if( ( tmp = ( ch | ( kbdflags << 16 ) ) ) > 256 )
    {
        if( ( tmp == SL_KEY_MOU ) )
        {
            if( ( eventmask & MOUSE_ALL_EVENTS_MASK ) != 0 )
                return( HB_GT_FUNC(mouse_Inkey( eventmask )) );
            else
            {
                /* mouse events are not requested so */
                /* clear a reported mouse position */
                /* NOTE : this will work only under Xterm */
                SLang_getkey(); SLang_getkey(); SLang_getkey();
                if( ( eventmask & INKEY_RAW ) == 0 )
                    return( 0 );
                else
                    return( tmp );
            }
        }

        if( ( eventmask & INKEY_RAW ) == 0 )
        {
            tmp = HB_GT_FUNC(gt_FindKeyTranslation( tmp ));

            /* TOFIX: this code is broken - needs a diffrent aproach */
            if( tmp == 0 )
            {
                tmp = HB_GT_FUNC(gt_FindKeyTranslation( ch ));
                if( tmp == 0 && ch < 256 ) tmp = ch;
            }
        }

        return( tmp );
    }

    /* standard ASCII key */
    return( ch );
}

/* *********************************************************************** */

static int HB_GT_FUNC(gt_try_get_Kbd_State())
{
#if defined(__linux__)
    unsigned char modifiers = 6;

    if( ioctl( 0, TIOCLINUX, &modifiers ) < 0 )
        return 0;
 
    return ( int ) modifiers;
 
#elif defined(M_UNIX)
 
    int modifiers = 0;
    int IOcommand = 0;
 
    if( ioctl( 0, TCGETSC, &IOcommand ) >= 0 )
    {
        /* if keyboard is not in SCANCODE mode */
        if( IOcommand == KB_XSCANCODE )
        {
            /* try to set it to SCANCODE mode */
            IOcommand = KB_ISSCANCODE;
            if( ioctl( 0, TCSETSC, &IOcommand ) >= 0 )
            {
                /* if SCANCODE mode is set corectly try get KBD state */
                if( ioctl( 0, KDGKBSTATE, &modifiers ) < 0 )
                    modifiers = 0;
    
                /* turn a keyboard to a normal mode ( translation mode ) */
                IOcommand = KB_XSCANCODE;
                ( void ) ioctl( 0, TCSETSC, &IOcommand )
            }
        }
        /* keyboard is already in SCANCODE mode */
        else if( ioctl( 0, KDGKBSTATE, &modifiers ) < 0 )
            modifiers = 0;
  
        return modifiers;
    }
#endif

    return 0;
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_Shft_Pressed())
{
    return ( HB_GT_FUNC(gt_try_get_Kbd_State()) & SHIFT_PRESSED ) != 0;
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_Ctrl_Pressed())
{
    return ( HB_GT_FUNC(gt_try_get_Kbd_State()) & CONTROL_PRESSED ) != 0;
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_Alt_Pressed())
{
    return ( HB_GT_FUNC(gt_try_get_Kbd_State()) & ALT_PRESSED ) != 0;
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_Kbd_State())
{
    return HB_GT_FUNC(gt_try_get_Kbd_State());
}

/* *********************************************************************** */
