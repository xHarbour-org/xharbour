/*
 * $Id: mainwin.c,v 1.17 2004/05/06 23:42:02 peterrees Exp $
 */

/*
 * Harbour Project source code:
 * Windows applications entry point
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbvm.h"
#include "hbsetup.h"

#define MAX_ARGS 64

#if defined(HB_OS_WIN_32)
HB_EXTERN_BEGIN

int argc = 0;
char * argv[ MAX_ARGS ];  //23/12/2003 3:40p.m. change from 20 to 64

void HB_EXPORT hb_SetWinMainParameters( HANDLE hInstance, HANDLE hPrevInstance, int iCmdShow );

int WINAPI WinMain( HINSTANCE hInstance,      /* handle to current instance */
                    HINSTANCE hPrevInstance,  /* handle to previous instance */
                    LPSTR lpCmdLine,          /* pointer to command line */
                    int iCmdShow )            /* show state of window */
{
#ifdef HB_FM_WIN32_ALLOC
     LPSTR pArgs = ( LPSTR ) LocalAlloc( LMEM_FIXED, strlen( lpCmdLine ) + 1 );
#else
     LPSTR pArgs = ( LPSTR ) malloc(strlen( lpCmdLine ) + 1 );
#endif
   LPSTR pStart, pArg = pArgs;
   char szAppName[ 250 ];
   BOOL bInQuotedParam;
   int iResult ;
   HB_TRACE(HB_TR_DEBUG, ("WinMain(%p, %p, %s, %d)", hInstance, hPrevInstance, lpCmdLine, iCmdShow));

   HB_SYMBOL_UNUSED( iCmdShow );

   hb_SetWinMainParameters( hInstance, hPrevInstance, iCmdShow ) ;

   GetModuleFileName( hInstance, szAppName, 249 );
   argv[ argc++ ] = szAppName;

   while (*lpCmdLine && argc < MAX_ARGS )
   {
     while (*lpCmdLine== ' ')  // Skip over any white space
     {
       lpCmdLine++ ;
     }
     if (*lpCmdLine)
     {
       pStart= NULL ;
       bInQuotedParam= FALSE;
       while (*lpCmdLine)
       {
         if (*lpCmdLine == '"')
         {
           lpCmdLine++;
           if ( bInQuotedParam )
           {
             if (pStart == NULL)
             {
               pStart = pArg;
             }
             break ;
           }
           else
           {
             bInQuotedParam = TRUE ;
           }
         }
         else if (*lpCmdLine== ' ')
         {
           if ( bInQuotedParam )
           {
             *pArg = *lpCmdLine++ ;
             if (pStart == NULL)
             {
               pStart = pArg;
             }
             pArg++;
           }
           else
           {
             lpCmdLine++ ;
             break ;
           }
         }
         else
         {
           *pArg = *lpCmdLine++ ;
           if (pStart == NULL)
           {
             pStart = pArg;
           }
           pArg++;
         }
       }
       if (pStart)
       {
         *pArg++ = '\0';
         argv[ argc++ ] = pStart ;
       }
     }
   }

   hb_cmdargInit( argc, argv );

   hb_vmInit( TRUE );

   iResult = hb_vmQuit();
#ifdef HB_FM_WIN32_ALLOC
      LocalFree( pArgs );
#else
      free( pArgs );
#endif
   return iResult;
}

HB_EXTERN_END

#if ( defined(__WATCOMC__) || defined(__MINGW32__) ) && !defined(__EXPORT__)
void HB_EXPORT hb_forceLinkMain( void ) {}
#endif

#endif
