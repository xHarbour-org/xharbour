/*
 * $Id: xharbour.prg,v 1.1 2005/03/22 19:22:04 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Compiler main file - PRG Level
 *
 * Copyright 2005 Andi Jahja <xharbour@cbn.net.id>
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

//------------------------------------------------------------------------------
FUNCTION MAIN ( ... )

   LOCAL cCMDLine := ""
   LOCAL iResult

   AEval( hb_aParams(), { |e| cCMDLine += e + " " } )

   RETURN ( iResult := Compile( cCMDLine ) )

//------------------------------------------------------------------------------
#if defined( __WIN32__ )

#pragma begindump

#include "hbapi.h"
#include "hbdefs.h"
#include <windows.h>

#define MAX_ARGS 64

#if defined( __MINGW32__ )
   #undef  WINAPI
   #define WINAPI
#endif

HB_EXTERN_BEGIN
extern int HB_IMPORT WINAPI hrbmain( int, char ** );
HB_EXTERN_END

HB_FUNC_STATIC( COMPILE )
{
   int argc = 0;
   char *argv[ MAX_ARGS ];
   char *lpCmdLine = hb_parcx(1);
#ifdef HB_FM_WIN32_ALLOC
   LPSTR pArgs = ( LPSTR ) LocalAlloc( LMEM_FIXED, strlen( lpCmdLine ) + 1 );
#else
   LPSTR pArgs = ( LPSTR ) hb_xgrab(strlen( lpCmdLine ) + 1 );
#endif
   LPSTR pArg = pArgs;
   int iResult = EXIT_FAILURE;

   strcpy( pArgs, lpCmdLine );

   argv[ 0 ] = "hrbmain";

   if( pArgs && *pArg )
   {
      argv[ ++argc ] = pArg;

      while( *pArg != 0 )
      {
         if( *pArg == ' ' )
         {
            *pArg++ = 0;
            argc++;

            while( *pArg == ' ' )
            {
               pArg++;
            }

            if( *pArg != 0 )
            {
               argv[ argc ] = pArg++;
            }
            else
            {
               argc--;
            }
         }
         else
         {
            pArg++;
         }
      }

      argc++;

   }

   iResult = hrbmain( argc, argv );

#ifdef HB_FM_WIN32_ALLOC
   LocalFree( pArgs );
#else
   hb_xfree( pArgs );
#endif

   hb_retni( iResult );

}

#pragma enddump

#else

STATIC FUNCTION COMPILE()

   Alert("Function only run in Windows ....")

RETURN 0

#endif
