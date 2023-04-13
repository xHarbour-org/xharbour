/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compile help & info related functions
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

#include "hbcomp.h"

/*
 * Prints available options
 */
void hb_compPrintUsage( char * szSelf )
{
   static const char *  szOptions[] =
   {
      "\nOptions:  %ca               automatic memvar declaration",
      "\n          %cb               debug info",
      "\n          %cbuild           display detailed version info",
      "\n          %ccredits         display credits",
      "\n          %cd<id>[=<val>]   #define <id>",
      "\n          %ces[<level>]     set exit severity",
      "\n          %cex              create public function list (.xbx)",
      "\n          %cg<type>         output type generated is <type> (see below)",
      "\n          %cgc[<type>]      output type: C source (.c) (default)",
      "\n                           <type>: 0=compact 1=normal 2=verbose (default)",
      "\n                                   3=generate real C code",
      "\n          %cgo              output type: Platform dependant object module",
      "\n          %cgh              output type: Harbour Portable Object (.hrb)",
      "\n          %ci<path>         #include file search path",
      "\n          %cj[<file>]       output i18n support [to <file>] to .hil",
      "\n          %ck               compilation mode (type -k? for more data)",
      "\n          %cl               suppress line number information",
      "\n          %cm               compile module only",
      "\n          %cn[<type>]       no implicit starting procedure (default)",
      "\n                           <type>: 0=no implicit starting procedure",
      "\n                                   1=no starting procedure at all",
      "\n                                   2=force application starting procedure",
      "\n          %co<path>         object file drive and/or path",
      "\n          %cp[o<path>]      generate pre-processed output (.ppo) file in <path>",
      "\n          %cpt[o<path>]     generate pre-processor trace (.ppt) file in <path>",
      "\n          %cq               quiet",
      "\n          %cq0              quiet and don't display program header",
/* TODO:   "\n          %cr[<lib>]        request linker to search <lib> (or none)", */
      "\n          %cs               syntax check only",
/* TODO:   "\n          %ct<path>         path for temp file creation", */
      "\n          %cu[[+]<file>]    use command def set in <file> (or none)",
      "\n          %cundef:<id>      #undef <id>",
      "\n          %cv               variables are assumed M->",
      "\n          %cvd              external functions are assumed as dynamic functions",
#if defined( HB_AVOID_RESERVED_WORDS )
      "\n          %cvx              force using reserved word as variable name",
#endif
      "\n          %cw[<level>]      set warning level number (0..3, default 1)",
      "\n          %cx[<prefix>]     set symbol init function name prefix (for .c only)",
#ifdef YYDEBUG
      "\n          %cy               trace lex & yacc activity",
#endif
      "\n          %cz               suppress shortcutting (.and. & .or.)",
      "\n          @<file>          compile list of modules in <file>",
      "\n"
   };

   int                  iLine;

   printf( "\nSyntax:  %s <file[s][.prg]> [options]"
           "\n", szSelf );

   for( iLine = 0; iLine < ( int ) ( sizeof( szOptions ) / sizeof( char * ) ); iLine++ )
      printf( szOptions[ iLine ], HB_OS_OPT_DELIM_LIST[ 0 ] );
}

/*
 * List of compatibility/features modes
 */
void hb_compPrintModes( void )
{
   static const char *  szOptions[] =
   {
      "\nOptions:  c               clear all flags (strict Clipper mode)",
      "\n          h               Harbour mode (default)",
      "\n          i               enable support for HB_INLINE",
      "\n          r               runtime settings enabled",
      "\n          x               extended xbase mode",
      "\n          J               turn off jump optimization in pcode",
      "\n          ?               this info",
      "\n"
   };

   int                  iLine;

   printf( "\nCompatibility flags (lowercase/uppercase significant): -k[options]\n" );

   for( iLine = 0; iLine < ( int ) ( sizeof( szOptions ) / sizeof( char * ) ); iLine++ )
      printf( "%s", szOptions[ iLine ] );
}

/*
 * Prints credits
 */
void hb_compPrintCredits( void )
{

   char * szCredits = hb_credits();

   printf( "\n"
           "Credits: The xHarbour Team at www.xharbour.org\n"
           "         The Harbour Team at www.harbour-project.org\n"
           "\n" );
   printf( "%s", szCredits );
   printf( "\n" );
}

/*
 * Prints logo
 */
void hb_compPrintLogo( void )
{
   char *pszVersion = hb_verHarbour();
   printf( "%s\n", pszVersion );
   printf( "Copyright 1999-" _HB_CURR_YEAR ", http://www.xharbour.org http://www.harbour-project.org/\n" );
   hb_xfree( pszVersion );
}
