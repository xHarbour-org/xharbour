/*
 * $Id: gtsln.c,v 1.22 2004/05/25 20:27:23 druzus Exp $
 */

/*
 * Harbour Project source code:
 * Video subsystem based on Slang screen library.
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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
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
 * not apply to the code that you add in this way.   To avoid misleading
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

static int s_iStdIn, s_iStdOut, s_iStdErr;

#ifndef HB_SLN_UTF8
/* does terminal works in Unicode (UTF-8) mode? */
int SLsmg_Is_Unicode = 0;
#endif

/* Slang color names */
static char * s_colorNames[] =
{
    "black"         ,
    "blue"          ,
    "green"         ,
    "cyan"          ,
    "red"           ,
    "magenta"       ,
    "brown"         ,
    "lightgray"     ,

    "gray"          ,
    "brightblue"    ,
    "brightgreen"   ,
    "brightcyan"    ,
    "brightred"     ,
    "brightmagenta" ,
    "yellow"        ,
    "white"
};

/* to convert Clipper colors into Slang ones */
static SLsmg_Char_Type s_colorTab[ 256 ];

/* to convert displayed characters */
static SLsmg_Char_Type s_outputTab[ 256 ];

/* to convert box displayed characters */
static SLsmg_Char_Type s_outboxTab[ 256 ];

/* currenlty used conversion table */
static SLsmg_Char_Type *s_currOutTab = s_outputTab;

/* to convert input characters */
unsigned char s_inputTab[ 256 ];
#ifndef HB_CDP_SUPPORT_OFF
PHB_CODEPAGE s_gtSln_cdpIN;
#endif

static USHORT s_uiDispCount = 0;
static SHORT s_sCursorStyle = SC_NORMAL;

/* clipboard support */
static char *s_clipboard = NULL;
static ULONG s_clipsize = 0;

/* indicate if we are currently running a command from system */
static BOOL s_bSuspended = FALSE;

/* delay for waiting on characters after ESC key */
extern int s_gtSLN_escDelay;

/* to convert DeadKey+letter to national character */
extern unsigned char s_convKDeadKeys[];
extern int HB_GT_FUNC(gt_Init_Terminal( int phase ));

/* the name of an environmet variable containig a definition of nation chars.*/
/* A definition is a list of pairs of chars. The first char in each pair is  */
/* an ASCII key, which should be pressed *after* a "DeadKey" was pressed to  */
/* get the nation char, a second in that pair is a corresponding nation char */
static unsigned char *hb_NationCharsEnvName = "HRBNATIONCHARS";

/* *********************************************************************** */

/* *********************************************************************** */

volatile BOOL hb_gt_sln_bScreen_Size_Changed = FALSE;

/* window's resize handler */
static void sigwinch_handler( int iSig )
{
   HB_SYMBOL_UNUSED( iSig );

   hb_gt_sln_bScreen_Size_Changed = TRUE;
   SLsignal( SIGWINCH, sigwinch_handler );
}

/* *********************************************************************** */

static void hb_sln_colorTrans( void )
{
   int i, clr, fg, bg;

   for( i = 0; i < 256; i++ )
   {
      fg = ( i & 0x0F );
      /*
       * bit 7 is a blinking attribute - not used when console is not in
       * UTF-8 mode because we are using it for changing into ACSC
       */
      bg = ( i >> 4 ) & ( SLsmg_Is_Unicode ? 0x0F : 0x07 );
      /*
       * in Clipper default color i 0x07 when in Slang 0x00,
       * we make a small trick with XOR 7 to make default colors
       * the same.
       */
      clr = ( i ^ 0x07 );
      SLtt_set_color( clr, ( char * ) NULL, s_colorNames[ fg ], s_colorNames[ bg ] );
      s_colorTab[ i ] = SLSMG_BUILD_CHAR( 0, clr );
   }
}

/* *********************************************************************** */

static void hb_sln_setSingleBox( void )
{
   /* convert all box chars into Clipper _B_SINBLE */
   s_outputTab[ 205 ] = s_outputTab[ 196 ];
   s_outputTab[ 186 ] = s_outputTab[ 179 ];
   s_outputTab[ 201 ] = s_outputTab[ 218 ];
   s_outputTab[ 214 ] = s_outputTab[ 218 ];
   s_outputTab[ 213 ] = s_outputTab[ 218 ];
   s_outputTab[ 187 ] = s_outputTab[ 191 ];
   s_outputTab[ 183 ] = s_outputTab[ 191 ];
   s_outputTab[ 184 ] = s_outputTab[ 191 ];
   s_outputTab[ 200 ] = s_outputTab[ 192 ];
   s_outputTab[ 211 ] = s_outputTab[ 192 ];
   s_outputTab[ 212 ] = s_outputTab[ 192 ];
   s_outputTab[ 188 ] = s_outputTab[ 217 ];
   s_outputTab[ 189 ] = s_outputTab[ 217 ];
   s_outputTab[ 190 ] = s_outputTab[ 217 ];
   s_outputTab[ 185 ] = s_outputTab[ 118 ];
   s_outputTab[ 204 ] = s_outputTab[ 195 ];
   s_outputTab[ 203 ] = s_outputTab[ 194 ];
   s_outputTab[ 202 ] = s_outputTab[ 193 ];
   s_outputTab[ 206 ] = s_outputTab[ 197 ];
}

/* *********************************************************************** */

static void hb_sln_setACSCtrans( void )
{
   unsigned char * p, ch;

   /* init an alternate chars table */
   if( ( p = SLtt_Graphics_Char_Pairs ) )
   {
      SLsmg_Char_Type SLch;
      int i, len = strlen( p );

      for( i = 0; i < len; i += 2 )
      {
         ch = *p++;
         SLch = ( ( SLsmg_Char_Type )( *p++ ) ) | HB_SLN_ACSC_ATTR;
         switch( ch )
         {
#ifdef HB_SLN_UTF8
            case SLSMG_HLINE_CHAR_TERM   :   s_outputTab[ 196 ] = SLch; break;
            case SLSMG_VLINE_CHAR_TERM   :   s_outputTab[ 179 ] = SLch; break;
            case SLSMG_ULCORN_CHAR_TERM  :   s_outputTab[ 218 ] = SLch; break;
            case SLSMG_URCORN_CHAR_TERM  :   s_outputTab[ 191 ] = SLch; break;
            case SLSMG_LLCORN_CHAR_TERM  :   s_outputTab[ 192 ] = SLch; break;
            case SLSMG_LRCORN_CHAR_TERM  :   s_outputTab[ 217 ] = SLch; break;
            case SLSMG_RTEE_CHAR_TERM    :   s_outputTab[ 180 ] = SLch; break;
            case SLSMG_LTEE_CHAR_TERM    :   s_outputTab[ 195 ] = SLch; break;
            case SLSMG_UTEE_CHAR_TERM    :   s_outputTab[ 194 ] = SLch; break;
            case SLSMG_DTEE_CHAR_TERM    :   s_outputTab[ 193 ] = SLch; break;
            case SLSMG_PLUS_CHAR_TERM    :   s_outputTab[ 197 ] = SLch; break;
/*
            case SLSMG_DEGREE_CHAR_TERM  :   s_outputTab[    ] = SLch; break;
            case SLSMG_PLMINUS_CHAR_TERM :   s_outputTab[    ] = SLch; break;
            case SLSMG_BULLET_CHAR_TERM  :   s_outputTab[    ] = SLch; break;
*/
            case SLSMG_DIAMOND_CHAR_TERM :   s_outputTab[ 04 ] = SLch; break;
            case SLSMG_LARROW_CHAR_TERM  :   s_outputTab[ 17 ] = 
                                             s_outputTab[ 27 ] = SLch; break;
            case SLSMG_RARROW_CHAR_TERM  :   s_outputTab[ 16 ] = 
                                             s_outputTab[ 26 ] = SLch; break;
            case SLSMG_DARROW_CHAR_TERM  :   s_outputTab[ 25 ] = 
                                             s_outputTab[ 31 ] = SLch; break;
            case SLSMG_UARROW_CHAR_TERM  :   s_outputTab[ 24 ] = 
                                             s_outputTab[ 30 ] = SLch; break;
            case SLSMG_BOARD_CHAR_TERM   :   s_outputTab[ 176 ] = SLch; break;
            case SLSMG_CKBRD_CHAR_TERM   :   s_outputTab[ 177 ] = SLch; break;
            case SLSMG_BLOCK_CHAR_TERM   :   s_outputTab[ 178 ] = 
                                             s_outputTab[ 219 ] = SLch; break;
#else
            case SLSMG_HLINE_CHAR   :   s_outputTab[ 196 ] = SLch; break;
            case SLSMG_VLINE_CHAR   :   s_outputTab[ 179 ] = SLch; break;
            case SLSMG_ULCORN_CHAR  :   s_outputTab[ 218 ] = SLch; break;
            case SLSMG_URCORN_CHAR  :   s_outputTab[ 191 ] = SLch; break;
            case SLSMG_LLCORN_CHAR  :   s_outputTab[ 192 ] = SLch; break;
            case SLSMG_LRCORN_CHAR  :   s_outputTab[ 217 ] = SLch; break;
            case SLSMG_RTEE_CHAR    :   s_outputTab[ 180 ] = SLch; break;
            case SLSMG_LTEE_CHAR    :   s_outputTab[ 195 ] = SLch; break;
            case SLSMG_UTEE_CHAR    :   s_outputTab[ 194 ] = SLch; break;
            case SLSMG_DTEE_CHAR    :   s_outputTab[ 193 ] = SLch; break;
            case SLSMG_PLUS_CHAR    :   s_outputTab[ 197 ] = SLch; break;
/*
            case SLSMG_DEGREE_CHAR; :   s_outputTab[    ] = SLch; break;
            case SLSMG_PLMINUS_CHAR :   s_outputTab[    ] = SLch; break;
            case SLSMG_BULLET_CHAR  :   s_outputTab[    ] = SLch; break;
*/
            case SLSMG_DIAMOND_CHAR :   s_outputTab[ 04 ] = SLch; break;
            case SLSMG_LARROW_CHAR  :   s_outputTab[ 17 ] = 
                                        s_outputTab[ 27 ] = SLch; break;
            case SLSMG_RARROW_CHAR  :   s_outputTab[ 16 ] = 
                                        s_outputTab[ 26 ] = SLch; break;
            case SLSMG_DARROW_CHAR  :   s_outputTab[ 25 ] = 
                                        s_outputTab[ 31 ] = SLch; break;
            case SLSMG_UARROW_CHAR  :   s_outputTab[ 24 ] = 
                                        s_outputTab[ 30 ] = SLch; break;
            case SLSMG_BOARD_CHAR   :   s_outputTab[ 176 ] = SLch; break;
            case SLSMG_CKBRD_CHAR   :   s_outputTab[ 177 ] = SLch; break;
            case SLSMG_BLOCK_CHAR   :   s_outputTab[ 178 ] = 
                                        s_outputTab[ 219 ] = SLch; break;
#endif
         }
      }
   }
}

/* *********************************************************************** */

static void hb_sln_setCharTrans( PHB_CODEPAGE cdpHost, PHB_CODEPAGE cdpTerm, BOOL fBox )
{
   int i, iSrc, iDst;

#ifdef HB_CDP_SUPPORT_OFF
   HB_SYMBOL_UNUSED( cdpHost );
   HB_SYMBOL_UNUSED( cdpTerm );
   HB_SYMBOL_UNUSED( fBox );
#endif

   /* build a conversion chars table */
   for( i = 0; i < 256; i++ )
   {
#ifndef HB_CDP_SUPPORT_OFF
      if ( SLsmg_Is_Unicode )
         iDst = hb_cdpGetU16( cdpHost, ( BYTE ) i );
      else
#endif
         iDst = i;

      if ( iDst < 32 )
         /* under Unix control-chars are not visible in a general meaning */
         s_outputTab[ i ] = SLSMG_BUILD_CHAR( '.', 0 );
      else if ( ! SLsmg_Is_Unicode && i >= 128 )
         s_outputTab[ i ] = SLSMG_BUILD_CHAR( iDst, 0 ) | HB_SLN_ACSC_ATTR;
      else
         s_outputTab[ i ] = SLSMG_BUILD_CHAR( iDst, 0 );
      s_outboxTab[ i ] = s_outputTab[ i ];
   }

   if ( ! SLsmg_Is_Unicode )
   {
      hb_sln_setACSCtrans();
      /* QUESTION: do we have double, single-double, ... frames under xterm ? */
      if( hb_gt_UnderXterm )
         hb_sln_setSingleBox();
      memcpy( s_outboxTab, s_outputTab, sizeof( s_outputTab ) );

#ifndef HB_CDP_SUPPORT_OFF
      if ( cdpHost && cdpHost->nChars )
      {
#ifdef HB_SLN_UTF8
         HB_SYMBOL_UNUSED( cdpTerm );
#else
         BOOL fTrans = cdpTerm && cdpTerm->nChars == cdpHost->nChars;
#endif

         for ( i = 0; i < cdpHost->nChars; i++ )
         {
            iSrc = ( unsigned char ) cdpHost->CharsUpper[ i ];
#ifdef HB_SLN_UTF8
            iDst = hb_cdpGetU16( cdpHost, ( BYTE ) iSrc );
#else
            iDst = fTrans ? ( unsigned char ) cdpTerm->CharsUpper[ i ] : iSrc;
#endif
            s_outputTab[ iSrc ] = SLSMG_BUILD_CHAR( iDst, 0 );
            if ( fBox )
               s_outboxTab[ iSrc ] = s_outputTab[ iSrc ];

            iSrc = ( unsigned char ) cdpHost->CharsLower[ i ];
#ifdef HB_SLN_UTF8
            iDst = hb_cdpGetU16( cdpHost, ( BYTE ) iSrc );
#else
            iDst = fTrans ? ( unsigned char ) cdpTerm->CharsLower[ i ] : iSrc;
#endif
            s_outputTab[ iSrc ] = SLSMG_BUILD_CHAR( iDst, 0 );
            if ( fBox )
               s_outboxTab[ iSrc ] = s_outputTab[ iSrc ];
         }
      }
#endif
   }
}

/* *********************************************************************** */
static void hb_sln_setKeyTrans( PHB_CODEPAGE cdpHost, PHB_CODEPAGE cdpTerm )
{
   char *p;
   int i;

   for ( i = 0; i < 256; i++ )
      s_inputTab[ i ] = ( unsigned char ) i;

#ifndef HB_CDP_SUPPORT_OFF
   if ( cdpHost && cdpTerm && cdpTerm->nChars == cdpHost->nChars )
   {
      int iSrc, iDst;

      for ( i = 0; i < cdpHost->nChars; i++ )
      {
         iSrc = ( unsigned char ) cdpTerm->CharsUpper[ i ];
         iDst = ( unsigned char ) cdpHost->CharsUpper[ i ];
         s_inputTab[ iSrc ] = iDst;

         iSrc = ( unsigned char ) cdpTerm->CharsLower[ i ];
         iDst = ( unsigned char ) cdpHost->CharsLower[ i ];
         s_inputTab[ iSrc ] = iDst;
      }
   }
   s_gtSln_cdpIN = cdpTerm ? cdpTerm : cdpHost;
#else
   HB_SYMBOL_UNUSED( cdpHost );
   HB_SYMBOL_UNUSED( cdpTerm );
#endif

   /* init national chars */
   p = hb_getenv( hb_NationCharsEnvName );
   if( p )
   {
      int len = strlen( p ) >> 1, ch;

      /* no more than 128 National chars are allowed */
      if( len > 128 ) len = 128;

      /* the first element contains a number of Dead keys defined in an ENVAR */
      s_convKDeadKeys[ 0 ] = ( unsigned char ) len;

      len <<= 1;
      for( i = 0; i < len; i += 2 )
      {
         ch = ( unsigned char ) p[ i + 1 ];
         s_convKDeadKeys[ i + 1 ] = ( unsigned char ) p[ i ];
         s_convKDeadKeys[ i + 2 ] = ch;
         s_inputTab[ ch ] = SLSMG_BUILD_CHAR( ch, 0 );
      }
      hb_xfree( ( void * ) p );
   }
}

/* *********************************************************************** */

/* I think this function should not be void. It should be BOOL */
void HB_GT_FUNC(gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr ))
{
    BOOL gt_Inited = FALSE;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

    /* stdin && stdout && stderr */
    s_iStdIn  = iFilenoStdin;
    s_iStdOut = iFilenoStdout;
    s_iStdErr = iFilenoStderr;

    /* Slang file descriptors */
    SLang_TT_Read_FD  = -1;
    SLang_TT_Write_FD = -1;

    s_uiDispCount = 0;

    /* read a terminal descripion from a terminfo database */
    SLtt_get_terminfo();

    /* initialize higher-level Slang routines */
    if( SLkp_init() != -1 )
    {
        /* initialize a terminal stuff and a Slang
            keyboard subsystem for the first time */
        if( HB_GT_FUNC(gt_Init_Terminal( 0 )) )
        {
            /* fix an OutStd()/OutErr() output */
            if( !isatty( iFilenoStdout ) )
                SLang_TT_Write_FD = SLang_TT_Read_FD;

#ifdef HB_SLN_UTF8
//            SLsmg_Setlocale = 0;
#endif
            /* initialize a screen handling subsytem */
            if( SLsmg_init_smg() != -1 )
            {
                /* install window resize handler */
                SLsignal( SIGWINCH, sigwinch_handler );

                /* do not indicate USER_BREAK in SLang_Error - ??? */
                SLang_Ignore_User_Abort = 1;

                /* no default abort procesing */
                SLang_set_abort_signal( NULL );

                /* NOTE: this is incompatible with CLIPPER
                   but under Unix we should assume cursor is
                   visible on startup because we cannot figure
                   out a current cursor state */
                /* turn on a cursor visibility */
                if( SLtt_set_cursor_visibility( 1 ) == -1 )
                    s_sCursorStyle = SC_UNAVAIL;

                /* NOTE: this driver is implemented in a way that it is
                   imposible to get intensity/blinking background mode.
                   The reason is the way Slang is written.
                   This is incompatible with Clipper.
                   But when the console is in UTF-8 mode we don't need
                   to switch into ACSC because we can display all supported
                   characters using it's UNICODE values so we can use
                   blink bit as in Clipper.
                 */
                if ( SLsmg_Is_Unicode )
                {
                    SLtt_Blink_Mode = 1;
                    SLtt_Use_Blink_For_ACS = 1;
                }
                else
                {
                    SLtt_Blink_Mode = 0;
                    SLtt_Use_Blink_For_ACS = 0;
                }
                SLsmg_Display_Eight_Bit = 128;
                SLsmg_Newline_Behavior = SLSMG_NEWLINE_SCROLLS;

                /* initialize conversion tables */
                hb_sln_colorTrans();
                hb_sln_setCharTrans( s_cdpage, NULL, TRUE );
                hb_sln_setKeyTrans( s_cdpage, NULL );
                /* HB_GT_FUNC(gt_build_conv_tabs()); */

                /* ensure we are in a normal chars set */
                SLtt_set_alt_char_set( 0 );

                /* set the normal Slang color */
                SLsmg_set_color( 0 );

                /* NOTE: due to a work of a Slang library which does not
                   prepare its internal screen buffer properly, a screen
                   must be cleared before normal work. This is not
                   compatible with Clipper */
                SLsmg_cls();
                SLsmg_gotorc( 0, 0 );
                SLsmg_refresh();

                gt_Inited = TRUE;
            }
        }
    }

    HB_GT_FUNC(mouse_Init());

    if( ! gt_Inited )
    {
        char *errmsg = '\r'+'\n'+"Internal error : screen driver initialization failure"+'\r'+'\n'+( char )0;

        /* something went wrong - restore default settings */
        SLang_reset_tty();

        /* TODO: a standard Harbour error should be generated here ! */
        write( iFilenoStderr, errmsg , strlen( errmsg ) );
        exit( 20 );
    }
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Exit( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

    /* restore a standard bell frequency and duration */
    if( hb_gt_UnderLinuxConsole )
    {
        char *escstr;

        escstr = "\033[10]";     SLtt_write_string( escstr );
        escstr = "\033[11]";     SLtt_write_string( escstr );
        SLtt_flush_output();
    }

    HB_GT_FUNC(mouse_Exit());

    /* NOTE: This is incompatible with Clipper - on exit leave a cursor visible */
    if( s_sCursorStyle != SC_UNAVAIL )
        HB_GT_FUNC(gt_SetCursorStyle( SC_NORMAL ));

   if ( s_clipboard != NULL )
   {
      hb_xfree( s_clipboard );
   }

    SLsmg_refresh();
    SLsmg_reset_smg();
    SLang_reset_tty();
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_GetScreenWidth( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

    return SLtt_Screen_Cols;
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_GetScreenHeight( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

    return SLtt_Screen_Rows;
}

/* *********************************************************************** */

SHORT HB_GT_FUNC(gt_Col( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

    return SLsmg_get_column();
}

/* *********************************************************************** */

SHORT HB_GT_FUNC(gt_Row( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

    return SLsmg_get_row();
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetPos( SHORT iRow, SHORT iCol, SHORT iMethod ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", iRow, iCol, iMethod));

    HB_SYMBOL_UNUSED( iMethod );

    SLsmg_gotorc( iRow, iCol );
    /* SLtt_goto_rc( iRow, iCol ); */

    if( s_uiDispCount == 0 )
    {
        SLsmg_refresh();
#ifdef HAVE_GPM_H
        HB_GT_FUNC(mouse_FixTrash());
#endif
    }
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_AdjustPos( BYTE * pStr, ULONG ulLen ))
{
#if 0
    ULONG ulCount;
    USHORT row = SLsmg_get_row();
    USHORT col = SLsmg_get_column();

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

    for( ulCount = 0; ulCount < ulLen; ulCount++ )
    {
        switch( *pStr++ )
        {
            case HB_CHAR_BEL:
                break;

            case HB_CHAR_BS:
                if( col )
                    col--;
                else
                {
                    col = SLtt_Screen_Cols - 1;
                    if( row )
                        row--;
                }
                break;

            case HB_CHAR_LF:
                col = 0;
                /* This is a hack. Out<xxx>() is done outside Slang and
                   it can't be tracked currently by Slang. This should
                   be changed in console.c */
                SLtt_write_string( "\r" );
                if( row < SLtt_Screen_Rows - 1 )
                    row++;
                break;

            case HB_CHAR_CR:
                col = 0;
                break;

            default:
                if( col < SLtt_Screen_Cols - 1 )
                    col++;
                else
                {
                    col = 0;
                    if( row < SLtt_Screen_Rows - 1 )
                        row++;
                }
        }
    }

    HB_GT_FUNC(gt_SetPos( row, col, HB_GT_SET_POS_AFTER ));
#else
    HB_SYMBOL_UNUSED( pStr );
    HB_SYMBOL_UNUSED( ulLen );
#endif
    return TRUE;
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_GetCursorStyle( void ))
{
    /* TODO: What shape is the cursor? */

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

    /* if we don't know a cursor state - assume visible */
    if( s_sCursorStyle == SC_UNAVAIL )
        return( SC_NORMAL );

    return( s_sCursorStyle );
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetCursorStyle( USHORT uiStyle ))
{
    /* keyseq to define cursor shape under linux console */
    static char cursDefseq[] = { 27, '[', '?', '1', 'c', 0 };

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", uiStyle));

    /* TODO: How to set the shape of the cursor on terminals ? */

    if( s_sCursorStyle == SC_UNAVAIL )
        return;

    if( ( s_sCursorStyle >= SC_NONE ) && ( s_sCursorStyle <= SC_SPECIAL2 ) )
    {
        s_sCursorStyle = uiStyle;
        SLtt_set_cursor_visibility( s_sCursorStyle != SC_NONE );

#ifdef __linux__
        /* NOTE: cursor apearence works only under linux console */
        if( hb_gt_UnderLinuxConsole )
        {
            switch( uiStyle )
            {
            case SC_NONE:
                cursDefseq[ 3 ] = '1';
                break;

            case SC_NORMAL:
                cursDefseq[ 3 ] = '2';
                break;

            case SC_INSERT:
                cursDefseq[ 3 ] = '4';
                break;

            case SC_SPECIAL1:
                cursDefseq[ 3 ] = '8';
                break;

            case SC_SPECIAL2:
                /* TODO: find a proper sequqnce to set a cursor
                   to SC_SPECIAL2 under Linux console  */
                cursDefseq[ 3 ] = '4';
                break;
            }

            SLtt_write_string( cursDefseq );
        }
#endif

        if( s_uiDispCount == 0 )
            /* SLsmg_refresh(); */
            SLtt_flush_output();
    }
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_IsColor( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

    return SLtt_Use_Ansi_Colors;
}

/* *********************************************************************** */

static void HB_GT_FUNC(gt_xPutch( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar ))
{
    SLsmg_Char_Type SLchar;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %i)", uiRow, uiCol, (int) byAttr, byChar));

    SLchar = HB_SLN_BUILD_CHAR( byChar, byAttr );

    SLsmg_gotorc( uiRow, uiCol );
    SLsmg_write_raw( &SLchar, 1 );
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Puts( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE * pbyStr, ULONG ulLen ))
{
    ULONG i;
    SLsmg_Char_Type *pScr;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", uiRow, uiCol, (int) byAttr, pbyStr, ulLen));

    pScr = ( SLsmg_Char_Type * ) hb_xgrab( ( ulLen + 1 ) * sizeof( SLsmg_Char_Type ) );
    for( i = 0; i < ulLen; i++ )
    {
        *( pScr + i ) = HB_SLN_BUILD_CHAR( *pbyStr++, byAttr );;
    }

    SLsmg_gotorc( uiRow, uiCol );
    if( ulLen > 0 )
        SLsmg_write_raw( pScr, ulLen );

    if( uiCol + ulLen >= (ULONG) SLtt_Screen_Cols )
        if( s_uiDispCount == 0 )
        {
            SLsmg_refresh();
#ifdef HAVE_GPM_H
            HB_GT_FUNC(mouse_FixTrash());
#endif
        }

    /* NOTE : enable this if problems with cursor positioning occur */
    /* HB_GT_FUNC(gt_SetPos( uiRow, uiCol + ulLen, HB_GT_SET_POS_AFTER )); */

    hb_xfree( ( BYTE * )pScr );
}

/* *********************************************************************** */

int HB_GT_FUNC(gt_RectSize( USHORT rows, USHORT cols ))
{
    return rows * cols * sizeof( SLsmg_Char_Type );
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_GetText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyDst ))
{
    int Cols;
    USHORT usSavRow = SLsmg_get_row();
    USHORT usSavCol = SLsmg_get_column();
    SLsmg_Char_Type * pBuf = ( SLsmg_Char_Type * ) pbyDst;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbyDst));

    Cols = uiRight - uiLeft + 1;
    while( uiTop <= uiBottom )
    {
        SLsmg_gotorc( uiTop, uiLeft );
        SLsmg_read_raw( pBuf, Cols );
        pBuf += Cols;
        ++uiTop;
    }
    SLsmg_gotorc( usSavRow, usSavCol );
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_PutText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbySrc ))
{
    int Cols;
    USHORT usSavRow = SLsmg_get_row();
    USHORT usSavCol = SLsmg_get_column();
    SLsmg_Char_Type * pBuf = ( SLsmg_Char_Type * ) pbySrc;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbySrc));

    Cols = uiRight - uiLeft + 1;
    while( uiTop <= uiBottom )
    {
        SLsmg_gotorc( uiTop, uiLeft );
        SLsmg_write_raw( pBuf, Cols );
        pBuf += Cols;
        ++uiTop;
    }
    HB_GT_FUNC(gt_SetPos( usSavRow, usSavCol, HB_GT_SET_POS_AFTER ));
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr ))
{
    int Rows, Cols;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

    Rows = uiBottom - uiTop + 1;
    Cols = uiRight - uiLeft + 1;

    /* note: we are clearing a high bit of color */
    SLsmg_set_color_in_region( ( byAttr - 7 ) & 0x7F, uiTop, uiLeft, Rows, Cols );

    if( s_uiDispCount == 0 )
    {
        SLsmg_refresh();
#ifdef HAVE_GPM_H
        HB_GT_FUNC(mouse_FixTrash());
#endif
    }
}

/* *********************************************************************** */

/* NOTE : hb_gt_Scroll is based on gtdos.c, but changed to get scroll
   worked well when scrolling horizontally. Clipper behaves
   strange here. */

void HB_GT_FUNC(gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE byAttr, SHORT iRows, SHORT iCols ))
{
    SHORT usSaveRow, usSaveCol;
    UINT uiSize;

    int iLength = ( usRight - usLeft ) + 1;
    int iCount, iColOld, iColNew, iColSize;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRight, (int) byAttr, iRows, iCols));

    if( hb_gtRectSize( usTop, usLeft, usBottom, usRight, &uiSize ) == 0 )
    {
        unsigned char * fpBlank = ( unsigned char * ) hb_xgrab( iLength );
        unsigned char * fpBuff = ( unsigned char * ) hb_xgrab( iLength * sizeof( SLsmg_Char_Type ) );

        memset( fpBlank, ' ', iLength );

        iColOld = iColNew = usLeft;
        if( iCols >= 0 )
        {
            iColOld += iCols;
            iColSize = ( int ) ( usRight - usLeft );
            iColSize -= iCols;
        }
        else
        {
            iColNew -= iCols;
            iColSize = ( int ) ( usRight - usLeft );
            iColSize += iCols;
        }

        /* this is probably not compatible with Clipper */
        HB_GT_FUNC(gt_DispBegin());

        hb_gtGetPos( &usSaveRow, &usSaveCol );

        for( iCount = ( iRows >= 0 ? usTop : usBottom );
              ( iRows >= 0 ? iCount <= usBottom : iCount >= usTop );
              ( iRows >= 0 ? iCount++ : iCount-- ) )
        {
            int iRowPos = iCount + iRows;

            /* Read the text to be scrolled into the current row */
            if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
                HB_GT_FUNC(gt_GetText( iRowPos, iColOld, iRowPos, iColOld + iColSize, fpBuff ));

            /* Blank the scroll region in the current row */
            HB_GT_FUNC(gt_Puts( iCount, usLeft, byAttr, fpBlank, iLength ));

            /* Write the scrolled text to the current row */
            if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
                HB_GT_FUNC(gt_PutText( iCount, iColNew, iCount, iColNew + iColSize, fpBuff ));
        }

        hb_xfree( fpBlank );
        hb_xfree( fpBuff );

        /* hb_gtSetPos( usSaveRow, usSaveCol ); */
        SLsmg_gotorc( usSaveRow, usSaveCol );

        /* this is probably not compatible with Clipper */
        HB_GT_FUNC(gt_DispEnd());
    }
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_DispBegin( void ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

    ++s_uiDispCount;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_DispEnd())
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

    /* is this compatible with Clipper ? */
    if( s_uiDispCount > 0 )
        --s_uiDispCount;

    if( s_uiDispCount == 0 )
    {
        SLsmg_refresh();
#ifdef HAVE_GPM_H
        HB_GT_FUNC(mouse_FixTrash());
#endif
    }
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_SetMode( USHORT uiRows, USHORT uiCols ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", uiRows, uiCols));

    HB_SYMBOL_UNUSED( uiRows );
    HB_SYMBOL_UNUSED( uiCols );

    /* TODO: How to change the size of the screen? */
    return FALSE;
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_GetBlink())
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

    /* TODO: current implementation disables blinking/intensity */
    return FALSE;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetBlink( BOOL bBlink ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

    /* TODO: current implementation disables blinking/intensity */
    HB_SYMBOL_UNUSED( bBlink );
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Tone( double dFrequency, double dDuration ))
{
    char escstr[ 64 ];
    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

    /* TODO: Implement this for other consoles than linux ? */

    dFrequency = HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 );
    /* dDuration = dDuration * 1000.0 / 18.2; */ /* clocks */

    if( hb_gt_UnderLinuxConsole )
    {
        snprintf( escstr, 63, "\033[10;%hd]", ( int )dFrequency );
        SLtt_write_string( escstr );
        snprintf( escstr, 63, "\033[11;%hd]", ( int )( dDuration * 1000.0 / 18.2 ) );
        SLtt_write_string( escstr );
        SLtt_flush_output();
    }

    SLtt_beep();

    if( hb_gt_UnderLinuxConsole )
    {
        /* The conversion from Clipper (DOS) timer tick units to
           milliseconds is * 1000.0 / 18.2. */
        dDuration /= 18.2;
        hb_idleSleep( dDuration );
    }
}

/* *********************************************************************** */

char * HB_GT_FUNC(gt_Version( void ))
{
    return "Harbour Terminal: Slang";
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_DispCount())
{
    return s_uiDispCount;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_Replicate( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar, ULONG ulLen ))
{
    ULONG i;
    SLsmg_Char_Type SLchar, * pScr;

    HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", uiRow, uiCol, byAttr, byChar, ulLen));

    pScr = ( SLsmg_Char_Type * ) hb_xgrab( ( ulLen + 1 ) * sizeof( SLsmg_Char_Type ) );

    SLchar = HB_SLN_BUILD_CHAR( byChar, byAttr );

    for( i = 0; i < ulLen; i++ )
        *( pScr + i ) = SLchar;

    SLsmg_gotorc( uiRow, uiCol );

    if( ulLen > 0 )
    {
        SLsmg_write_raw( pScr, ulLen );

        /* this should not be needed here. hb_gtRepChar() should set this for us */
        HB_GT_FUNC(gt_SetPos( uiRow, uiCol + ulLen, HB_GT_SET_POS_AFTER ));
    }

    hb_xfree( ( BYTE * ) pScr );
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_Box( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
                          BYTE * szBox, BYTE byAttr ))
{
    USHORT ret = 1;
    SHORT Row;
    SHORT Col;
    SHORT Height;
    SHORT Width;
    /* a box drawing hack */
    SLsmg_Char_Type *SaveOutTab = s_currOutTab;
    s_currOutTab = s_outboxTab;

    if( ( Left   >= 0 && Left   < HB_GT_FUNC(gt_GetScreenWidth())  )  ||
        ( Right  >= 0 && Right  < HB_GT_FUNC(gt_GetScreenWidth())  )  ||
        ( Top    >= 0 && Top    < HB_GT_FUNC(gt_GetScreenHeight()) )  ||
        ( Bottom >= 0 && Bottom < HB_GT_FUNC(gt_GetScreenHeight()) ) )
    {
        /* Ensure that box is drawn from top left to bottom right. */
        if( Top > Bottom )
        {
            SHORT tmp = Top;
            Top = Bottom;
            Bottom = tmp;
        }
        if( Left > Right )
        {
            SHORT tmp = Left;
            Left = Right;
            Right = tmp;
        }

        /* Draw the box or line as specified */
        Height = Bottom - Top + 1;
        Width  = Right - Left + 1;

        HB_GT_FUNC(gt_DispBegin());

        if( Height > 1 && Width > 1 && Top >= 0 && Top < HB_GT_FUNC(gt_GetScreenHeight()) && Left >= 0 && Left < HB_GT_FUNC(gt_GetScreenWidth()) )
            HB_GT_FUNC(gt_xPutch( Top, Left, byAttr, szBox[ 0 ] )); /* Upper left corner */

        Col = ( Height > 1 ? Left + 1 : Left );
        if(Col < 0 )
        {
            Width += Col;
            Col = 0;
        }
        if( Right >= HB_GT_FUNC(gt_GetScreenWidth()) )
        {
            Width -= Right - HB_GT_FUNC(gt_GetScreenWidth());
        }

        if( Col <= Right && Col < HB_GT_FUNC(gt_GetScreenWidth()) && Top >= 0 && Top < HB_GT_FUNC(gt_GetScreenHeight()) )
            HB_GT_FUNC(gt_Replicate( Top, Col, byAttr, szBox[ 1 ], Width + ( (Right - Left) > 1 ? -2 : 0 ) )); /* Top line */

        if( Height > 1 && (Right - Left) > 1 && Right < HB_GT_FUNC(gt_GetScreenWidth()) && Top >= 0 && Top < HB_GT_FUNC(gt_GetScreenHeight()) )
            HB_GT_FUNC(gt_xPutch( Top, Right, byAttr, szBox[ 2 ] )); /* Upper right corner */

        if( szBox[ 8 ] && Height > 2 && Width > 2 )
        {
            for( Row = Top + 1; Row < Bottom; Row++ )
            {
                if( Row >= 0 && Row < HB_GT_FUNC(gt_GetScreenHeight()) )
                {
                    Col = Left;
                    if( Col < 0 )
                        Col = 0; /* The width was corrected earlier. */
                    else
                        HB_GT_FUNC(gt_xPutch( Row, Col++, byAttr, szBox[ 7 ] )); /* Left side */
                    HB_GT_FUNC(gt_Replicate( Row, Col, byAttr, szBox[ 8 ], Width - 2 )); /* Fill */
                    if( Right < HB_GT_FUNC(gt_GetScreenWidth()) )
                        HB_GT_FUNC(gt_xPutch( Row, Right, byAttr, szBox[ 3 ] )); /* Right side */
                }
            }
        }
        else
        {
            for( Row = ( Width > 1 ? Top + 1 : Top ); Row < ( (Right - Left ) > 1 ? Bottom : Bottom + 1 ); Row++ )
            {
                if( Row >= 0 && Row < HB_GT_FUNC(gt_GetScreenHeight()) )
                {
                    if( Left >= 0 && Left < HB_GT_FUNC(gt_GetScreenWidth()) )
                        HB_GT_FUNC(gt_xPutch( Row, Left, byAttr, szBox[ 7 ] )); /* Left side */
                    if( ( Width > 1 || Left < 0 ) && Right < HB_GT_FUNC(gt_GetScreenWidth()) )
                        HB_GT_FUNC(gt_xPutch( Row, Right, byAttr, szBox[ 3 ] )); /* Right side */
                }
            }
        }

        if( Height > 1 && Width > 1 )
        {
            if( Left >= 0 && Bottom < HB_GT_FUNC(gt_GetScreenHeight()) )
                HB_GT_FUNC(gt_xPutch( Bottom, Left, byAttr, szBox[ 6 ] )); /* Bottom left corner */

            Col = Left + 1;
            if( Col < 0 )
                Col = 0; /* The width was corrected earlier. */

            if( Col <= Right && Bottom < HB_GT_FUNC(gt_GetScreenHeight()) )
                HB_GT_FUNC(gt_Replicate( Bottom, Col, byAttr, szBox[ 5 ], Width - 2 )); /* Bottom line */

            if( Right < HB_GT_FUNC(gt_GetScreenWidth()) && Bottom < HB_GT_FUNC(gt_GetScreenHeight()) )
                HB_GT_FUNC(gt_xPutch( Bottom, Right, byAttr, szBox[ 4 ] )); /* Bottom right corner */
        }

        /* NOTE : enable this if problems with cursor positioning occur */
        /* SLsmg_gotorc( uiTop + 1, uiLeft + 1 ); */
        HB_GT_FUNC(gt_DispEnd());

        ret = 0;
    }
    s_currOutTab = SaveOutTab;

    return ret;
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_BoxD( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr ))
{
    return HB_GT_FUNC(gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr ));
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_BoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr ))
{
    return HB_GT_FUNC(gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr ));
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_HorizLine( SHORT Row, SHORT Left, SHORT Right, BYTE byChar, BYTE byAttr ))
{
    USHORT ret = 1;
    /* a box drawing hack */
    SLsmg_Char_Type *SaveOutTab = s_currOutTab;
    s_currOutTab = s_outboxTab;

    if( Row >= 0 && Row < HB_GT_FUNC(gt_GetScreenHeight()) )
    {
        if( Left < 0 )
            Left = 0;
        else if( Left >= HB_GT_FUNC(gt_GetScreenWidth()) )
            Left = HB_GT_FUNC(gt_GetScreenWidth()) - 1;

        if( Right < 0 )
            Right = 0;
        else if( Right >= HB_GT_FUNC(gt_GetScreenWidth()) )
            Right = HB_GT_FUNC(gt_GetScreenWidth()) - 1;

        if( Left < Right )
            HB_GT_FUNC(gt_Replicate( Row, Left, byAttr, byChar, Right - Left + 1 ));
        else
            HB_GT_FUNC(gt_Replicate( Row, Right, byAttr, byChar, Left - Right + 1 ));

        ret = 0;
    }
    s_currOutTab = SaveOutTab;

    return ret;
}

/* *********************************************************************** */

USHORT HB_GT_FUNC(gt_VertLine( SHORT Col, SHORT Top, SHORT Bottom, BYTE byChar, BYTE byAttr ))
{
    USHORT ret = 1;
    USHORT uRow;
    /* a box drawing hack */
    SLsmg_Char_Type *SaveOutTab = s_currOutTab;
    s_currOutTab = s_outboxTab;

    if( Col >= 0 && Col < HB_GT_FUNC(gt_GetScreenWidth()) )
    {
        if( Top < 0 )
            Top = 0;
        else if( Top >= HB_GT_FUNC(gt_GetScreenHeight()) )
            Top = HB_GT_FUNC(gt_GetScreenHeight()) - 1;

        if( Bottom < 0 )
            Bottom = 0;
        else if( Bottom >= HB_GT_FUNC(gt_GetScreenHeight()) )
            Bottom = HB_GT_FUNC(gt_GetScreenHeight()) - 1;

        if( Top <= Bottom )
            uRow = Top;
        else
        {
            uRow = Bottom;
            Bottom = Top;
        }

        while( uRow <= Bottom )
            HB_GT_FUNC(gt_xPutch( uRow++, Col, byAttr, byChar ));

        HB_GT_FUNC(gt_SetPos( Bottom + 1, Col, HB_GT_SET_POS_AFTER ));
        ret = 0;
    }
    s_currOutTab = SaveOutTab;

    return ret;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_OutStd( BYTE * pbyStr, ULONG ulLen ))
{
    if( isatty( s_iStdOut ) )
    {
        //int SaveColor = SLsmg_get_color();
        //SLtt_set_alt_char_set( 1 );
        SLsmg_set_color( ( int )( ( unsigned char )( hb_gtCurrentColor() - 7 ) ) );
        SLsmg_write_nchars( pbyStr, ulLen );
        SLsmg_refresh();
        //SLtt_set_alt_char_set( 0 );
        //SLsmg_set_color( SaveColor );
    }
    else
        hb_fsWriteLarge( s_iStdOut, ( BYTE * ) pbyStr, ulLen );
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_OutErr( BYTE * pbyStr, ULONG ulLen ))
{
    if( isatty( s_iStdErr ) )
    {
        int Save_SLang_TT_Write_FD = SLang_TT_Write_FD;
        SLang_TT_Write_FD = s_iStdErr;
        //SLtt_set_alt_char_set( 1 );
        SLsmg_set_color( ( int )( ( unsigned char )( hb_gtCurrentColor() - 7 ) ) );
        SLsmg_write_nchars( pbyStr, ulLen );
        SLsmg_refresh();
        SLang_TT_Write_FD = Save_SLang_TT_Write_FD;
        //SLtt_set_alt_char_set( 0 );
    }
    else
        hb_fsWriteLarge( s_iStdErr, ( BYTE * ) pbyStr, ulLen );
}

/* *********************************************************************** */

/* NOTE: these two are for prepare Slang to temporary
   finish its work. They should be called from run.c. */

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_Suspend())
{
    if( ! s_bSuspended )
    {
        if( SLsmg_suspend_smg() != -1 )
        {
            SLang_reset_tty();
            s_bSuspended = TRUE;
        }
    }

    return s_bSuspended;
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_Resume())
{
    if( s_bSuspended &&
        SLsmg_resume_smg() != -1 &&
        HB_GT_FUNC(gt_Init_Terminal( 1 )) != -1
      )
    {
        SLsmg_refresh(); /* reinitialize a terminal */
#ifdef HAVE_GPM_H
        HB_GT_FUNC(mouse_FixTrash());
#endif
        s_bSuspended = FALSE;
    }

    return( !s_bSuspended );
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_PreExt())
{
    SLsmg_refresh();
#ifdef HAVE_GPM_H
    HB_GT_FUNC(mouse_FixTrash());
#endif
    return TRUE;
}

/* *********************************************************************** */

BOOL HB_GT_FUNC(gt_PostExt())
{
    return TRUE;
}

/* ************************** Clipboard support ********************************** */

void HB_GT_FUNC( gt_GetClipboard( char *szData, ULONG *pulMaxSize ) )
{
   if ( *pulMaxSize == 0 || s_clipsize < *pulMaxSize )
   {
      *pulMaxSize = s_clipsize;
   }

   if ( *pulMaxSize != 0 )
   {
      memcpy( szData, s_clipboard, *pulMaxSize );
   }

}

void HB_GT_FUNC( gt_SetClipboard( char *szData, ULONG ulSize ) )
{
   if ( s_clipboard != NULL )
   {
      hb_xfree( s_clipboard );
   }

   s_clipboard = (char *) hb_xgrab( ulSize +1 );
   memcpy( s_clipboard, szData, ulSize );
   s_clipboard[ ulSize ] = '\0';
   s_clipsize = ulSize;
}

ULONG HB_GT_FUNC( gt_GetClipboardSize( void ) )
{
   return s_clipsize;
}

/* *********************************************************************** */
/* extended GT functions */

int HB_GT_FUNC( gt_info(int iMsgType, BOOL bUpdate, int iParam, void *vpParam ) )
{
   int iRet = -1;
   HB_SYMBOL_UNUSED( vpParam );

   switch ( iMsgType )
   {
      case GTI_ISGRAPHIC:
         iRet = (int) FALSE;
         break;
      case GTI_INPUTFD:
         iRet = SLang_TT_Read_FD;
         break;
      case GTI_OUTPUTFD:
         iRet = SLang_TT_Write_FD;
         break;
      case GTI_ERRORFD:
         iRet = s_iStdErr;
         break;
      case GTI_ESCDELAY:
         iRet = s_gtSLN_escDelay;
         if ( bUpdate )
            s_gtSLN_escDelay = iParam;
         break;
   }
   return iRet;
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetDispCP( char * pszTermCDP, char * pszHostCDP, BOOL fBox ))
{
#ifndef HB_CDP_SUPPORT_OFF
   PHB_CODEPAGE cdpTerm = NULL, cdpHost = NULL;
   
   cdpHost = hb_cdpFind( pszHostCDP );
   if ( pszHostCDP && *pszHostCDP )
      cdpHost = hb_cdpFind( pszHostCDP );
   if ( ! cdpHost )
      cdpHost = s_cdpage;

   if ( pszTermCDP && *pszTermCDP )
      cdpTerm = hb_cdpFind( pszTermCDP );

   hb_sln_setCharTrans( cdpHost, cdpTerm, fBox );
#else
   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );
#endif
}

/* *********************************************************************** */

void HB_GT_FUNC(gt_SetKeyCP( char * pszTermCDP, char * pszHostCDP ))
{
#ifndef HB_CDP_SUPPORT_OFF
   PHB_CODEPAGE cdpTerm = NULL, cdpHost = NULL;

   cdpHost = hb_cdpFind( pszHostCDP );
   if ( pszHostCDP && *pszHostCDP )
      cdpHost = hb_cdpFind( pszHostCDP );
   if ( ! cdpHost )
      cdpHost = s_cdpage;

   if ( pszTermCDP && *pszTermCDP )
      cdpTerm = hb_cdpFind( pszTermCDP );

   hb_sln_setKeyTrans( cdpHost, cdpTerm );
#else
   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );
#endif
}

/* *********************************************************************** */

#ifdef HB_MULTI_GT

static void HB_GT_FUNC(gtFnInit( PHB_GT_FUNCS gt_funcs ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_gtFnInit(%p)", gt_funcs));

    gt_funcs->Init                  = HB_GT_FUNC( gt_Init );
    gt_funcs->Exit                  = HB_GT_FUNC( gt_Exit );
    gt_funcs->GetScreenWidth        = HB_GT_FUNC( gt_GetScreenWidth );
    gt_funcs->GetScreenHeight       = HB_GT_FUNC( gt_GetScreenHeight );
    gt_funcs->Col                   = HB_GT_FUNC( gt_Col );
    gt_funcs->Row                   = HB_GT_FUNC( gt_Row );
    gt_funcs->SetPos                = HB_GT_FUNC( gt_SetPos );
    gt_funcs->AdjustPos             = HB_GT_FUNC( gt_AdjustPos );
    gt_funcs->IsColor               = HB_GT_FUNC( gt_IsColor );
    gt_funcs->GetCursorStyle        = HB_GT_FUNC( gt_GetCursorStyle );
    gt_funcs->SetCursorStyle        = HB_GT_FUNC( gt_SetCursorStyle );
    gt_funcs->DispBegin             = HB_GT_FUNC( gt_DispBegin );
    gt_funcs->DispEnd               = HB_GT_FUNC( gt_DispEnd );
    gt_funcs->DispCount             = HB_GT_FUNC( gt_DispCount );
    gt_funcs->Puts                  = HB_GT_FUNC( gt_Puts );
    gt_funcs->Replicate             = HB_GT_FUNC( gt_Replicate );
    gt_funcs->RectSize              = HB_GT_FUNC( gt_RectSize );
    gt_funcs->GetText               = HB_GT_FUNC( gt_GetText );
    gt_funcs->PutText               = HB_GT_FUNC( gt_PutText );
    gt_funcs->SetAttribute          = HB_GT_FUNC( gt_SetAttribute );
    gt_funcs->Scroll                = HB_GT_FUNC( gt_Scroll );
    gt_funcs->SetMode               = HB_GT_FUNC( gt_SetMode );
    gt_funcs->GetBlink              = HB_GT_FUNC( gt_GetBlink );
    gt_funcs->SetBlink              = HB_GT_FUNC( gt_SetBlink );
    gt_funcs->Version               = HB_GT_FUNC( gt_Version );
    gt_funcs->Box                   = HB_GT_FUNC( gt_Box );
    gt_funcs->BoxD                  = HB_GT_FUNC( gt_BoxD );
    gt_funcs->BoxS                  = HB_GT_FUNC( gt_BoxS );
    gt_funcs->HorizLine             = HB_GT_FUNC( gt_HorizLine );
    gt_funcs->VertLine              = HB_GT_FUNC( gt_VertLine );
    gt_funcs->Suspend               = HB_GT_FUNC( gt_Suspend );
    gt_funcs->Resume                = HB_GT_FUNC( gt_Resume );
    gt_funcs->PreExt                = HB_GT_FUNC( gt_PreExt );
    gt_funcs->PostExt               = HB_GT_FUNC( gt_PostExt );
    gt_funcs->OutStd                = HB_GT_FUNC( gt_OutStd );
    gt_funcs->OutErr                = HB_GT_FUNC( gt_OutErr );
    gt_funcs->Tone                  = HB_GT_FUNC( gt_Tone );
    gt_funcs->ExtendedKeySupport    = HB_GT_FUNC( gt_ExtendedKeySupport );
    gt_funcs->ReadKey               = HB_GT_FUNC( gt_ReadKey );
    /* extended GT functions */
    gt_funcs->info                  = HB_GT_FUNC( gt_info );
    gt_funcs->SetDispCP             = HB_GT_FUNC( gt_SetDispCP );
    gt_funcs->SetKeyCP              = HB_GT_FUNC( gt_SetKeyCP );
    gt_funcs->SetClipboard          = HB_GT_FUNC( gt_SetClipboard );
    gt_funcs->GetClipboard          = HB_GT_FUNC( gt_GetClipboard );
    gt_funcs->GetClipboardSize      = HB_GT_FUNC( gt_GetClipboardSize );
}

/* ********************************************************************** */

static void HB_GT_FUNC(mouseFnInit( PHB_GT_FUNCS gt_funcs ))
{
    HB_TRACE(HB_TR_DEBUG, ("hb_mouseFnInit(%p)", gt_funcs));

    gt_funcs->mouse_Init            = HB_GT_FUNC( mouse_Init );
    gt_funcs->mouse_Exit            = HB_GT_FUNC( mouse_Exit );
    gt_funcs->mouse_IsPresent       = HB_GT_FUNC( mouse_IsPresent );
    gt_funcs->mouse_Show            = HB_GT_FUNC( mouse_Show );
    gt_funcs->mouse_Hide            = HB_GT_FUNC( mouse_Hide );
    gt_funcs->mouse_Col             = HB_GT_FUNC( mouse_Col );
    gt_funcs->mouse_Row             = HB_GT_FUNC( mouse_Row );
    gt_funcs->mouse_SetPos          = HB_GT_FUNC( mouse_SetPos );
    gt_funcs->mouse_IsButtonPressed = HB_GT_FUNC( mouse_IsButtonPressed );
    gt_funcs->mouse_CountButton     = HB_GT_FUNC( mouse_CountButton );
    gt_funcs->mouse_SetBounds       = HB_GT_FUNC( mouse_SetBounds );
    gt_funcs->mouse_GetBounds       = HB_GT_FUNC( mouse_GetBounds );
}


/* ********************************************************************** */

static HB_GT_INIT gtInit = { HB_GT_DRVNAME( HB_GT_NAME ),
                             HB_GT_FUNC(gtFnInit), HB_GT_FUNC(mouseFnInit) };

HB_GT_ANNOUNCE( HB_GT_NAME );

HB_CALL_ON_STARTUP_BEGIN( _hb_startup_gt_Init_ )
   hb_gtRegister( &gtInit );
HB_CALL_ON_STARTUP_END( _hb_startup_gt_Init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_startup_gt_Init_
#endif

#endif  /* HB_MULTI_GT */

/* ********************************************************************** */
