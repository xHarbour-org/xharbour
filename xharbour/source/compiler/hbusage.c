/*
 * $Id: hbusage.c,v 1.17 2004/03/31 19:29:46 likewolf Exp $
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
   static const char * szOptions [] =
   {
           "\nOptions:  %ca               automatic memvar declaration",
           "\n          %cb               debug info",
           "\n          %cbuild           display detailed version info",
           "\n          %ccredits         display credits",
           "\n          %cd<id>[=<val>]   #define <id>",
           "\n          %ces[<level>]     set exit severity",
           "\n          %cg<type>         output type generated is <type> (see below)",
           "\n          %cgc[<type>]      output type: C source (.c) (default)",
           "\n                           <type>: 0=compact 1=normal 2=verbose (default)",
           "\n                                   3=generate variable list (.var) file",
           "\n          %cgo              output type: Platform dependant object module",
           "\n          %cgw              output type: Windows/DOS OBJ32 (.obj)",
           "\n          %cgh              output type: Harbour Portable Object (.hrb)",
           "\n          %cgj              output type: Java source (.java)",
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
           "\n          %cp               generate pre-processed output (.ppo) file",
           "\n          %cq               quiet",
           "\n          %cq0              quiet and don't display program header",
/* TODO:   "\n          %cr[<lib>]        request linker to search <lib> (or none)", */
           "\n          %cs               syntax check only",
/* TODO:   "\n          %ct<path>         path for temp file creation", */
           "\n          %cu[<file>]       use command def set in <file> (or none)",
           "\n          %cundef:<id>      #undef <id>",
           "\n          %cv               variables are assumed M->",
           "\n          %cw[<level>]      set warning level number (0..3, default 1)",
           "\n          %cx[<prefix>]     set symbol init function name prefix (for .c only)",
#ifdef YYDEBUG
           "\n          %cy               trace lex & yacc activity",
#endif
           "\n          %cz               suppress shortcutting (.and. & .or.)",
/* TODO:   "\n           @<file>         compile list of modules in <file>", */
           "\n"
   };

   int iLine;

   printf( "\nSyntax:  %s <file[s][.prg]> [options]"
           "\n", szSelf );

   for( iLine = 0; iLine < ( int ) ( sizeof( szOptions ) / sizeof( char * ) ); iLine++ )
      printf( szOptions[ iLine ], OS_OPT_DELIMITER_LIST[ 0 ] );
}

/*
 * List of compatibility/features modes
 */
void hb_compPrintModes( void )
{
   static const char * szOptions [] =
   {
           "\nOptions:  c               clear all flags (strict Clipper mode)",
           "\n          h               Harbour mode (default)",
           "\n          i               enable support for HB_INLINE",
           "\n          r               runtime settings enabled",
           "\n          x               extended xbase mode",
           "\n          ?               this info",
           "\n"
   };

   int iLine;

   printf( "\nCompatibility flags (lowercase/uppercase significant): -k[options]\n" );

   for( iLine = 0; iLine < ( int ) ( sizeof( szOptions ) / sizeof( char * ) ); iLine++ )
      printf( szOptions[ iLine ] );
}

/*
 * Prints credits
 */
void hb_compPrintCredits( void )
{

   printf( "\n"
           "Credits: The xHarbour Team at www.xharbour.org\n"
           "         The Harbour Team at www.harbour-project.org\n"
           "\n"
           "Andi Jahja <xharbour@cbn.net.id>\n"
           "Antonio Carlos Pantaglione <toninho@fwi.com.br>\n"
           "Antonio Linares <alinares@fivetech.com>\n"
           "Alejandro de Garate <alex_degarate@hotmail.com>\n"
           "Alexander S. Kresin <alex@belacy.belgorod.su>\n"
           "April White <awhite@user.rose.com>\n"
           "Bil Simser <bsimser@home.com>\n"
           "Brian Hays <bhays@abacuslaw.com>\n"
           "Bruno Cantero <bruno@issnet.net>\n"
           "Charles Kwon <Charles@fivetech.net>\n"
           "Chen Kedem <niki@actcom.co.il>\n"
           "Dave Pearson <davep@davep.org>\n"
           "David G. Holm <dholm@jsd-llc.com>\n"
           "Davor Siklic <siki@msoft.cz>\n"
           "Eddie Runia <eddie@runia.com>\n"
           "Felipe G. Coury <fcoury@creation.com.br>\n"
           "Francesco Saverio Giudice <info@fsgiudice.com>\n"
           "Giancarlo Niccolai <gc@niccolai.ws>\n"
           "Gonzalo A. Diethelm <gonzalo.diethelm@iname.com>\n"
           "Horacio D. Roldan Kasimatis <harbour_ar@yahoo.com.ar>\n"
           "Ignacio Ortiz de Zuniga <ignacio@fivetech.com>\n"
           "Janica Lubos <janica@fornax.elf.stuba.sk>\n"
           "Jean-Francois Lefebvre (mafact) <jfl@mafact.com>\n"
           "Jose F. Gimenez <jfgimenez@wanadoo.es>\n"
           "Jose Lalin <dezac@corevia.com>\n"
           "Leslee Griffith <les.griffith@vantagesystems.ca>\n"
           "Lorenzo Fiorini <lorenzo_fiorini@tin.it>\n"
           "Luiz Rafael Culik <culik@sl.conex.net>\n"
           "Manuel Ruiz <mrt@joca.es>\n"
           "Marcelo Lombardo <lombardo@uol.com.br>\n"
           "Matteo Baccan <baccan@isanet.it>\n"
           "Matthew Hamilton <mhamilton@bunge.com.au>\n"
           "Mauricio Abre <maurifull@datafull.com>\n"
           "Maurilio Longo <maurilio.longo@libero.it>\n"
           "Mindaugas Kavaliauskas <dbtopas@dbtopas.lt>\n"
           "Nicolas del Pozo <niko@geroa.com>\n"
           "Patrick Mast <harbour@winfakt.com>\n"
           "Paul Tucker <ptucker@sympatico.ca>\n"
           "Peter Rees <peter@rees.co.nz>\n"
           "Peter Townsend <cephas@tpgi.com.au>\n"
           "Phil Barnett <philb@iag.net>\n"
           "Phil Krylov <phil@newstar.rinet.ru>\n"
           "Pritpal Bedi <pritpal@vouchcac.com>\n"
           "Przemyslaw Czerpak <druzus@polbox.com>\n"
           "Ron Pinkas <ron@profit-master.com>\n"
           "Ryszard Glab <rglab@imid.med.pl>\n"
           "Sylvain Robert <s.robert@videotron.ca>\n"
           "Tim Stone <timstone@mstrlink.com>\n"
           "Tommi Rouvali <tommi@rouvali.com>\n"
           "Vicente Guerra <vicente@guerra.com.mx>\n"
           "Viktor Szakats <viktor.szakats@syenar.hu>\n"
           "Vladimir Kazimirchik <v_kazimirchik@yahoo.com>\n"
           "Walter Negro <anegro@overnet.com.ar>\n"
           );
}

/*
 * Prints logo
 */
void hb_compPrintLogo( void )
{
   printf( "xHarbour Compiler build %d.%d.%d (%s)\n", HB_VER_MAJOR, HB_VER_MINOR, HB_VER_REVISION, HB_VER_LEX );
   printf( "Copyright 1999-2004, http://www.xharbour.org http://www.harbour-project.org/\n" );
}
