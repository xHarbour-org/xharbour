/*
 * $Id: hbver.c,v 1.27 2005/03/09 05:35:23 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Version detection functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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
 * Copyright 1999 Luiz Rafael Culik <culik@sl.conex.net>
 *    hb_verPlatform() (support for determining the windows version)
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    hb_verPlatform() (support for determining many windows flavours)
 *    hb_verCompiler() (support for determining some compiler version/revision)
 *
 * Copyright 2000-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_verPlatform() (support for detecting Windows NT on DOS)
 *    hb_verPlatform() (rearrangment and cleanup)
 *
 * See doc/license.txt for licensing terms.
 *
 */

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbver.h"
#include "hbcomp.h"
#include "hbmemory.ch"

#if defined(HB_OS_WIN_32)

   #include <ctype.h>
   #include "hbwbase.h"

#elif defined(HB_OS_UNIX)

   #include <sys/utsname.h>

#endif

char *hb_credits( void );
static void hb_conOutErr_ ( char * szText, ULONG ulLen, BOOL bOut );

#include <time.h>

/* NOTE: OS() function, as a primary goal will detect the version number
         of the target platform. As an extra it may also detect the host OS.
         The latter is mainly an issue in DOS, where the host OS can be OS/2
         WinNT/2K, Win3x, Win9x, DOSEMU, Desqview, etc. [vszakats] */

/* NOTE: The caller must free the returned buffer. [vszakats] */

char * hb_verPlatform( void )
{

   char *szPlatform = ( char * ) hb_xgrab( 256 );

   HB_TRACE(HB_TR_DEBUG, ("hb_verPlatform()"));

#if defined(HB_OS_DOS)

   {
      /* NOTE: Must be larger than 128, which is the maximum size of
               osVer.szCSDVersion (Win32). [vszakats] */
      char szName[128];
      union REGS regs;

      regs.h.ah = 0x30;
      HB_DOS_INT86( 0x21, &regs, &regs );

      sprintf( szPlatform, "DOS %d.%02d", regs.h.al, regs.h.ah );

      /* Host OS detection: Windows 2.x, 3.x, 95/98 */

      {
         regs.HB_XREGS.ax = 0x1600;
         HB_DOS_INT86( 0x2F, &regs, &regs );

         if( regs.h.al != 0x00 && regs.h.al != 0x80 )
         {
            if( regs.h.al == 0x01 || regs.h.al == 0xFF )
            {
               sprintf( szName, " (Windows 2.x)" );
            }
            else
            {
               sprintf( szName, " (Windows %d.%02d)", regs.h.al, regs.h.ah );
            }

            strcat( szPlatform, szName );
         }
      }

      /* Host OS detection: Windows NT/2000 */

      {
         regs.HB_XREGS.ax = 0x3306;
         HB_DOS_INT86( 0x21, &regs, &regs );

         if( regs.HB_XREGS.bx == 0x3205 )
         {
            strcat( szPlatform, " (Windows NT/2000)" );
         }
      }

      /* Host OS detection: OS/2 */

      {
         regs.h.ah = 0x30;
         HB_DOS_INT86( 0x21, &regs, &regs );

         if( regs.h.al >= 10 )
         {
            if( regs.h.al == 20 && regs.h.ah > 20 )
            {
               sprintf( szName, " (OS/2 %d.%02d)", regs.h.ah / 10, regs.h.ah % 10 );
            }
            else
            {
               sprintf( szName, " (OS/2 %d.%02d)", regs.h.al / 10, regs.h.ah );
            }

            strcat( szPlatform, szName );
         }
      }
   }

#elif defined(HB_OS_OS2)

   {
      ULONG aulQSV[ QSV_MAX ] = { 0 };
      APIRET rc;

      rc = DosQuerySysInfo( 1L, QSV_MAX, ( void * ) aulQSV, sizeof( ULONG ) * QSV_MAX );

      if( rc == 0 )
      {
         /* is this OS/2 2.x ? */
         if( aulQSV[ QSV_VERSION_MINOR - 1 ] < 30 )
         {
            sprintf( szPlatform, "OS/2 %ld.%02ld",
               aulQSV[ QSV_VERSION_MAJOR - 1 ] / 10,
               aulQSV[ QSV_VERSION_MINOR - 1 ] );
         }
         else
         {
            sprintf( szPlatform, "OS/2 %2.2f",
               ( float ) aulQSV[ QSV_VERSION_MINOR - 1 ] / 10 );
         }
      }
      else
      {
         sprintf( szPlatform, "OS/2" );
      }
   }

#elif defined(HB_OS_WIN_32)

   {
      /* NOTE: Must be larger than 128, which is the maximum size of
               osVer.szCSDVersion (Win32). [vszakats] */
      char szName[128];
      OSVERSIONINFO osVer;

      osVer.dwOSVersionInfoSize = sizeof( osVer );

      if( GetVersionEx( &osVer ) )
      {
         strcpy( szName, "Windows" );

         switch( osVer.dwPlatformId )
         {
            case VER_PLATFORM_WIN32_WINDOWS:

               if( osVer.dwMajorVersion == 4 && osVer.dwMinorVersion < 10 )
               {
                  strcat( szName, " 95" );
               }
               else if( osVer.dwMajorVersion == 4 && osVer.dwMinorVersion == 10 )
               {
                  strcat( szName, " 98" );
               }
               else
               {
                  strcat( szName, " ME" );
               }

               break;

            case VER_PLATFORM_WIN32_NT:

               if( osVer.dwMajorVersion == 5 && osVer.dwMinorVersion == 2 )
               {
                  strcat( szName, " Server 2003" );
               }
               else if( osVer.dwMajorVersion == 5 && osVer.dwMinorVersion == 1 )
               {
                  strcat( szName, " XP" );
               }
               else if( osVer.dwMajorVersion == 5 )
               {
                  strcat( szName, " 2000" );
               }
               else
               {
                  strcat( szName, " NT" );
               }

               /* test for specific product on Windows NT 4.0 SP6 and later */

               {
                  HBOSVERSIONINFOEX osVerEx;  /* NOTE */

                  osVerEx.dwOSVersionInfoSize = sizeof( osVerEx );

                                    /* Windows decl error? */
                  if( GetVersionEx( ( LPOSVERSIONINFOA ) &osVerEx ) )
                  {
                     /* workstation type */

                     if( osVerEx.wProductType == VER_NT_WORKSTATION )
                     {
                        if( osVerEx.dwMajorVersion == 4 )
                        {
                           strcat( szName, " Workstation 4.0" );
                        }
                        else if( osVerEx.wSuiteMask & VER_SUITE_PERSONAL )
                        {
                           strcat( szName, " Home Edition" );
                        }
                        else
                        {
                           strcat( szName, " Professional" );
                        }
                     }

                     /* server type */

                     else if( osVerEx.wProductType == VER_NT_SERVER )
                     {
                        if( osVerEx.dwMajorVersion == 5 && osVerEx.dwMinorVersion == 2 )
                        {
                           if( osVerEx.wSuiteMask & VER_SUITE_DATACENTER )
                           {
                              strcat( szName, " Datacenter Edition" );
                           }
                           else if( osVerEx.wSuiteMask & VER_SUITE_ENTERPRISE )
                           {
                              strcat( szName, " Enterprise Edition" );
                           }
                           else if( osVerEx.wSuiteMask == VER_SUITE_BLADE )
                           {
                              strcat( szName, " Web Edition" );
                           }
                           else
                           {
                              strcat( szName, " Standard Edition" );
                           }
                        }

                        else if( osVerEx.dwMajorVersion == 5 && osVerEx.dwMinorVersion == 0 )
                        {
                           if( osVerEx.wSuiteMask & VER_SUITE_DATACENTER )
                           {
                              strcat( szName, " Datacenter Server" );
                           }
                           else if( osVerEx.wSuiteMask & VER_SUITE_ENTERPRISE )
                           {
                              strcat( szName, " Advanced Server" );
                           }
                           else
                           {
                              strcat( szName, " Server" );
                           }
                        }

                        else
                        {
                           if( osVerEx.wSuiteMask & VER_SUITE_ENTERPRISE )
                           {
                              strcat( szName, " Server 4.0, Enterprise Edition" );
                           }
                           else
                           {
                              strcat( szName, " Server 4.0" );
                           }
                        }
                     }
                  }
               }

               break;

            case VER_PLATFORM_WIN32s:
               strcat( szName, " 32s" );
               break;

            case VER_PLATFORM_WIN32_CE:
               strcat( szName, " CE" );
               break;
         }

         sprintf( szPlatform, "%s %lu.%02lu.%04d", szName,
                              ( ULONG ) osVer.dwMajorVersion,
                              ( ULONG ) osVer.dwMinorVersion,
                              ( USHORT ) LOWORD( osVer.dwBuildNumber ) );

         /* Add service pack/other info */

         if( osVer.szCSDVersion )
         {
            int i;

            /* Skip the leading spaces (Win95B, Win98) */
            for( i = 0; osVer.szCSDVersion[ i ] != '\0' && isspace( ( BYTE ) osVer.szCSDVersion[ i ] ); i++ ) {}

            if( osVer.szCSDVersion[ i ] != '\0' )
            {
               strcat( szPlatform, " " );
               strcat( szPlatform, osVer.szCSDVersion + i );
            }
         }
      }
      else
      {
         sprintf( szPlatform, "Windows" );
      }
   }

#elif defined(HB_OS_UNIX)

   {
      struct utsname un;

      uname( &un );
      sprintf( szPlatform, "%s %s %s", un.sysname, un.release, un.machine );
   }

#elif defined(HB_OS_MAC)

   {
      strcpy( szPlatform, "MacOS compatible" );
   }

#else

   {
      strcpy( szPlatform, "(unknown)" );
   }

#endif

   return szPlatform;
}

BOOL hb_iswinnt(void)
{
#if defined(HB_OS_WIN_32)

  OSVERSIONINFO osvi ;
  osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  GetVersionEx (&osvi);
  return(osvi.dwPlatformId == VER_PLATFORM_WIN32_NT); // && osvi.dwMajorVersion >= 4);
#else
  return FALSE ;
#endif
}


/* NOTE: The caller must free the returned buffer. [vszakats] */

char * hb_verCompiler( void )
{
   char * pszCompiler;
   char * szName;
   int iVerMajor;
   int iVerMinor;
   int iVerPatch;

   HB_TRACE(HB_TR_DEBUG, ("hb_verCompiler()"));

   pszCompiler = ( char * ) hb_xgrab( 80 );

#if defined(__IBMC__) || defined(__IBMCPP__)

   #if defined(__IBMC__)
      iVerMajor = __IBMC__;
   #else
      iVerMajor = __IBMCPP__;
   #endif

   if( iVerMajor >= 300 )
      szName = "IBM Visual Age C++";
   else
      szName = "IBM C++";

   iVerMajor /= 100;
   iVerMinor = iVerMajor % 100;
   iVerPatch = 0;

#elif defined(__POCC__)

   szName = "Pelles ISO C Compiler";
   iVerMajor = __POCC__ / 100;
   iVerMinor = __POCC__ % 100;
   iVerPatch = 0;

#elif defined(__XCC__)

   szName = "Pelles ISO C Compiler";
   iVerMajor = __XCC__ / 100;
   iVerMinor = __XCC__ % 100;
   iVerPatch = 0;

#elif defined(__LCC__)

   szName = "Logiciels/Informatique lcc-win32";
   iVerMajor = 0;
   iVerMinor = 0;
   iVerPatch = 0;
//   iVerMajor = __LCC__ / 100;
//   iVerMinor = __LCC__ % 100;

#elif defined(__DMC__)

   szName = __DMC_VERSION_STRING__ ;
   iVerMajor = 0;
   iVerMinor = 0;
   iVerPatch = 0;

#elif defined(_MSC_VER)

   #if (_MSC_VER >= 800)
      szName = "Microsoft Visual C/C++";
   #else
      szName = "Microsoft C/C++";
   #endif

   iVerMajor = _MSC_VER / 100;
   iVerMinor = _MSC_VER % 100;

   #if defined(_MSC_FULL_VER)
      iVerPatch = _MSC_FULL_VER - ( _MSC_VER * 10000 );
   #endif

#elif defined(__BORLANDC__)

   szName = "Borland C++";
   #if (__BORLANDC__ == 1040) /* Version 3.1 */
      iVerMajor = 3;
      iVerMinor = 1;
   #elif (__BORLANDC__ >= 1280) /* Version 5.x */
      iVerMajor = ( __BORLANDC__ >> 8 ) & 0xF;
      iVerMinor = ( __BORLANDC__ >> 4 ) & 0xF;
      iVerPatch = __BORLANDC__ & 0xF;
   #else /* Version 4.x */
      iVerMajor = __BORLANDC__ >> 8;
      iVerMinor = ( __BORLANDC__ - 1 & 0xFF ) >> 4;
   #endif

#elif defined(__TURBOC__)

   szName = "Borland Turbo C";
   iVerMajor = __TURBOC__ >> 8;
   iVerMinor = __TURBOC__ & 0xFF;

#elif defined(__MPW__)

   szName = "MPW C";
   iVerMajor = __MPW__ / 100;
   iVerMinor = __MPW__ % 100;
   iVerPatch = 0;

#elif defined(__WATCOMC__)

   #if defined( __cplusplus )
      #if defined( HB_OS_WIN_32 )
         szName = "Open Watcom C++32";
      #else
         szName = "Open Watcom C++16";
      #endif
      iVerMajor = __WATCOM_CPLUSPLUS__ / 100;
      iVerMinor = __WATCOM_CPLUSPLUS__ % 100;
   #else
      #if defined( HB_OS_WIN_32 )
         szName = "Open Watcom C32";
      #else
         szName = "Open Watcom C16";
      #endif
      iVerMajor = __WATCOMC__ / 100;
      iVerMinor = __WATCOMC__ % 100;
   #endif

   #if defined( __WATCOM_REVISION__ )
      iVerPatch = __WATCOM_REVISION__;
   #else
      iVerPatch = 0;
   #endif

#elif defined(__GNUC__)

   #if defined(__DJGPP__)
      szName = "DJ Delorie's DJGPP";
   #elif defined(__CYGWIN__)
      szName = "Cygnus Cygwin GNU C";
   #elif defined(__MINGW32__)
      szName = "MinGW GNU C";
   #elif defined(__RSX32__)
      szName = "EMX/RSXNT/DOS GNU C";
   #elif defined(__RSXNT__)
      szName = "EMX/RSXNT/Win32 GNU C";
   #elif defined(__EMX__)
      szName = "EMX GNU C";
   #else
      szName = "GNU C";
   #endif

   iVerMajor = __GNUC__;
   iVerMinor = __GNUC_MINOR__;
   #if defined(__GNUC_PATCHLEVEL__)
      iVerPatch = __GNUC_PATCHLEVEL__;
   #else
      iVerPatch = 0;
   #endif
#else

   szName = ( char * ) NULL;
   iVerMajor = 0;
   iVerMinor = 0;
   iVerpatch = 0;

#endif

   if( szName )
      if( iVerPatch != 0 )
      {
         #if defined(_MSC_VER)
            sprintf( pszCompiler, "%s %hd.%02d.%hd", szName, iVerMajor, iVerMinor, iVerPatch );
         #else
            sprintf( pszCompiler, "%s %hd.%hd.%hd", szName, iVerMajor, iVerMinor, iVerPatch );
         #endif
      }
      else
      {
	 #if defined(__DMC__)
            sprintf( pszCompiler, "%s", szName );
	 #else
            sprintf( pszCompiler, "%s %hd.%hd", szName, iVerMajor, iVerMinor );
	 #endif
      }
   else
      strcpy( pszCompiler, "(unknown)" );

#if defined(__DJGPP__)

   {
      char szSub[ 32 ];
      sprintf( szSub, " (DJGPP %i.%02i)", ( int ) __DJGPP__, ( int ) __DJGPP_MINOR__ );
      strcat( pszCompiler, szSub );
   }

#elif defined(__BORLANDC__)

   {
      char szSub[ 32 ];
      /* QUESTION: Is there any better, safer, more official way to detect
                   the bit depth of the C compiler ? [vszakats] */

      sprintf( pszCompiler, "%s %hd.%hd.%hd", szName, iVerMajor, iVerMinor, iVerPatch );
      // sprintf( szSub, " (%i bit)", ( int ) ( sizeof( int ) * 8 ) );
      // strcat( pszCompiler, szSub );
   }

#endif

   return pszCompiler;
}

/* NOTE: The caller must free the returned buffer. [vszakats] */

char * hb_verHarbour( void )
{
   char * pszVersion;

   HB_TRACE(HB_TR_DEBUG, ("hb_verHarbour()"));

   pszVersion = ( char * ) hb_xgrab( 80 );

   sprintf( pszVersion, "xHarbour build %d.%d.%d Intl. (%s)", HB_VER_MAJOR, HB_VER_MINOR, HB_VER_REVISION, HB_VER_LEX );

   return pszVersion;
}

char * hb_verPCode( void )
{
   char * pszVersion;

   HB_TRACE(HB_TR_DEBUG, ("hb_verPCode()"));

   pszVersion = ( char * ) hb_xgrab( 32 );

   sprintf( pszVersion, "PCode Version: %d", HB_PCODE_VER );

   return pszVersion;

}

char *hb_verBuildInfo( BOOL bOut )
{
   char *szBuildInfo = (char*) hb_xgrab(1024);  // Should be enough IMO

   hb_xmemset( szBuildInfo, '\0', 1024 );

   hb_conOutErr_( "Harbour Build Info", 0, bOut );
   hb_conOutErr_( hb_conNewLine(), 0, bOut );
   hb_conOutErr_( "---------------------------", 0, bOut );
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   {
      char szLen[ 6 ];
      sprintf( szLen, "%d", HB_VER_MAJOR );
      hb_xstrcat( szBuildInfo, szLen, "(num)", "\t", NULL );
      sprintf( szLen, "%d", HB_VER_MINOR );
      hb_xstrcat( szBuildInfo, szLen, "(num)", "\t", NULL );
      sprintf( szLen, "%d", HB_VER_REVISION);
      hb_xstrcat( szBuildInfo, szLen, "(num)", "\t", NULL );
      hb_xstrcat( szBuildInfo, HB_VER_LEX, "\t", NULL );
   }

   {
      char * pszVersion = hb_verHarbour();
      hb_conOutErr_( "Version: ", 0, bOut );
      hb_conOutErr_( pszVersion, 0, bOut );
      hb_conOutErr_( hb_conNewLine(), 0, bOut );
      hb_xstrcat( szBuildInfo, pszVersion, "\t", NULL );
      hb_xfree( pszVersion );
   }

   {
      char * pszVersion = hb_verPCode();
      char szPCode[3];
      sprintf( szPCode, "%d", HB_PCODE_VER );
      hb_conOutErr_( pszVersion, 0, bOut );
      hb_conOutErr_( hb_conNewLine(), 0, bOut );
      hb_xstrcat( szBuildInfo, szPCode, "(num)", "\t", NULL );
      hb_xfree( pszVersion );
   }

   {
      char * pszVersion = hb_verCompiler();
      hb_conOutErr_( "Compiler: ", 0, bOut );
      hb_conOutErr_( pszVersion, 0, bOut );
      hb_conOutErr_( hb_conNewLine(), 0, bOut );
      hb_xstrcat( szBuildInfo, pszVersion, "\t", NULL );
      hb_xfree( pszVersion );
   }

   {
      char * pszVersion = hb_verPlatform();
      hb_conOutErr_( "Platform: ", 0, bOut );
      hb_conOutErr_( pszVersion, 0, bOut );
      hb_conOutErr_( hb_conNewLine(), 0, bOut );
      hb_xstrcat( szBuildInfo, pszVersion, "\t", NULL );
      hb_xfree( pszVersion );
   }

   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   hb_conOutErr_( "Built on: ", 0, bOut );
   hb_conOutErr_( __DATE__, 0, bOut );
   hb_xstrcat( szBuildInfo, __DATE__, "\t", NULL );
   hb_conOutErr_( " ", 0, bOut );
   hb_conOutErr_( __TIME__, 0, bOut );
   hb_xstrcat( szBuildInfo, __TIME__, "\t", NULL );
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   hb_conOutErr_( "Last ChangeLog entry: ", 0, bOut );
   hb_conOutErr_( HB_VER_LENTRY, 0, bOut );
   hb_xstrcat( szBuildInfo, HB_VER_LENTRY, "\t", NULL );
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   hb_conOutErr_( "ChangeLog CVS version: ", 0, bOut );
   hb_conOutErr_( HB_VER_CHLCVS, 0, bOut );
   hb_xstrcat( szBuildInfo, HB_VER_CHLCVS, "\t", NULL );
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   if( strlen( HB_VER_C_USR ) )
   {
      hb_conOutErr_( "Harbour compiler switches: ", 0, bOut );
      hb_conOutErr_( HB_VER_C_USR, 0, bOut );
      hb_conOutErr_( hb_conNewLine(), 0, bOut );
   }
   hb_xstrcat( szBuildInfo, HB_VER_C_USR, "\t", NULL );

   if( strlen( HB_VER_L_USR ) )
   {
      hb_conOutErr_( "C compiler switches: ", 0, bOut );
      hb_conOutErr_( HB_VER_L_USR, 0, bOut );
      hb_conOutErr_( hb_conNewLine(), 0, bOut );
   }
   hb_xstrcat( szBuildInfo, HB_VER_L_USR, "\t", NULL );

   if( strlen( HB_VER_PRG_USR ) )
   {
      hb_conOutErr_( "Linker switches: ", 0, bOut );
      hb_conOutErr_( HB_VER_PRG_USR, 0, bOut );
      hb_conOutErr_( hb_conNewLine(), 0, bOut );
   }
   hb_xstrcat( szBuildInfo, HB_VER_PRG_USR, "\t", NULL );

   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   hb_conOutErr_( "Harbour extensions: ", 0, bOut );
#if defined( HB_EXTENSION )
   hb_xstrcat( szBuildInfo, "yes", "\t", NULL );
   hb_conOutErr_( "Yes", 0, bOut );
#else
   hb_conOutErr_( "No", 0, bOut );
   hb_xstrcat( szBuildInfo, "no", "\t", NULL );
#endif
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   hb_conOutErr_( "CA-Clipper 5.2e undocumented: ", 0, bOut );
#if defined( HB_C52_UNDOC )
   hb_conOutErr_( "Yes", 0, bOut );
   hb_xstrcat( szBuildInfo, "yes", "\t", NULL );
#else
   hb_conOutErr_( "No", 0, bOut );
   hb_xstrcat( szBuildInfo, "no", "\t", NULL );
#endif
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   hb_conOutErr_( "CA-Clipper 5.2e strict compatibility: ", 0, bOut );
#if defined( HB_C52_STRICT )
   hb_conOutErr_( "Yes", 0, bOut );
   hb_xstrcat( szBuildInfo, "yes", "\t", NULL );
#else
   hb_conOutErr_( "No", 0, bOut );
   hb_xstrcat( szBuildInfo, "no", "\t", NULL );
#endif
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   hb_conOutErr_( "CA-Clipper 5.3x compatible extensions: ", 0, bOut );
#if defined( HB_COMPAT_C53 )
   hb_conOutErr_( "Yes", 0, bOut );
   hb_xstrcat( szBuildInfo, "yes", "\t", NULL );
#else
   hb_conOutErr_( "No", 0, bOut );
   hb_xstrcat( szBuildInfo, "no", "\t", NULL );
#endif
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   hb_conOutErr_( "Alaska Xbase++ compatible extensions: ", 0, bOut );
#if defined( HB_COMPAT_XPP )
   hb_conOutErr_( "Yes", 0, bOut );
   hb_xstrcat( szBuildInfo, "yes", "\t", NULL );
#else
   hb_conOutErr_( "No", 0, bOut );
   hb_xstrcat( szBuildInfo, "no", "\t", NULL );
#endif
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   hb_conOutErr_( "CA-Visual Objects compatible extensions: ", 0, bOut );
#if defined( HB_COMPAT_VO )
   hb_conOutErr_( "Yes", 0, bOut );
   hb_xstrcat( szBuildInfo, "yes", "\t", NULL );
#else
   hb_conOutErr_( "No", 0, bOut );
   hb_xstrcat( szBuildInfo, "no", "\t", NULL );
#endif
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   hb_conOutErr_( "Multisoft Flagship compatible extensions: ", 0, bOut );
#if defined( HB_COMPAT_FLAGSHIP )
   hb_conOutErr_( "Yes", 0, bOut );
   hb_xstrcat( szBuildInfo, "yes", "\t", NULL );
#else
   hb_conOutErr_( "No", 0, bOut );
   hb_xstrcat( szBuildInfo, "no", "\t", NULL );
#endif
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   hb_conOutErr_( "Microsoft FoxPro compatible extensions: ", 0, bOut );
#if defined( HB_COMPAT_FOXPRO )
   hb_conOutErr_( "Yes", 0, bOut );
   hb_xstrcat( szBuildInfo, "yes", "\t", NULL );
#else
   hb_conOutErr_( "No", 0, bOut );
   hb_xstrcat( szBuildInfo, "no", "\t", NULL );
#endif
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   hb_conOutErr_( "dBase compatible extensions: ", 0, bOut );
#if defined( HB_COMPAT_DBASE )
   hb_conOutErr_( "Yes", 0, bOut );
   hb_xstrcat( szBuildInfo, "yes", "\t", NULL );
#else
   hb_conOutErr_( "No", 0, bOut );
   hb_xstrcat( szBuildInfo, "no", "\t", NULL );
#endif
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   hb_conOutErr_( "Object file generation support: ", 0, bOut );
#if defined( HARBOUR_OBJ_GENERATION ) || defined( HB_BACK_END )
   hb_conOutErr_( "Yes", 0, bOut );
   hb_xstrcat( szBuildInfo, "yes", "\t", NULL );
#else
   hb_conOutErr_( "No", 0, bOut );
   hb_xstrcat( szBuildInfo, "no", "\t", NULL );
#endif
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   hb_conOutErr_( "ANSI C usage: ", 0, bOut );
#if defined( HARBOUR_STRICT_ANSI_C )
   hb_conOutErr_( "Strict", 0, bOut );
   hb_xstrcat( szBuildInfo, "yes", "\t", NULL );
#else
   hb_conOutErr_( "Non strict", 0, bOut );
   hb_xstrcat( szBuildInfo, "no", "\t", NULL );
#endif
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   hb_conOutErr_( "C++ mode: ", 0, bOut );
#if defined(__cplusplus)
   hb_conOutErr_( "On", 0, bOut );
   hb_xstrcat( szBuildInfo, "yes", "\t", NULL );
#else
   hb_conOutErr_( "Off", 0, bOut );
   hb_xstrcat( szBuildInfo, "no", "\t", NULL );
#endif
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   hb_conOutErr_( "Compiler YACC debug mode: ", 0, bOut );
#if defined( HARBOUR_YYDEBUG )
   hb_conOutErr_( "On", 0, bOut );
   hb_xstrcat( szBuildInfo, "yes", "\t", NULL );
#else
   hb_conOutErr_( "Off", 0, bOut );
   hb_xstrcat( szBuildInfo, "no", "\t", NULL );
#endif
#if 0
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   hb_conOutErr_( "Memory tracing and statistics: ", 0, bOut );
   hb_conOutErr_( hb_xquery( HB_MEM_USEDMAX ) != 0 ? "On" : "Off", 0, bOut );
#endif
/*
#if defined( HB_FM_STATISTICS )
   hb_conOutErr_( "On", 0, bOut );
#else
   hb_conOutErr_( "Off", 0, bOut );
#endif
*/
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   {
      char buffer[ 64 ];
      char szLen[ 3 ];
      sprintf( szLen, "%d", HB_SYMBOL_NAME_LEN );
      sprintf( buffer, "Maximum symbol name length: %i", HB_SYMBOL_NAME_LEN );
      hb_xstrcat( szBuildInfo, szLen, "(num)", NULL );
      hb_conOutErr_( buffer, 0, bOut );
      hb_conOutErr_( hb_conNewLine(), 0, bOut );
   }

   hb_conOutErr_( "---------------------------", 0, bOut );
   hb_conOutErr_( hb_conNewLine(), 0, bOut );

   return szBuildInfo;

}

char * hb_builddate()
{
   char* sz_Date = (char* ) hb_xgrab( 64 );
   hb_xmemset( sz_Date, '\0', 64 );
   hb_xstrcat( sz_Date,  __DATE__," ", __TIME__, NULL );

   return  sz_Date;
}

static void hb_conOutErr_ ( char * szText, ULONG ulLen, BOOL bOut )
{
   if( bOut )
   {
      hb_conOutErr( szText, ulLen );
   }
}

char *hb_credits()
{
   char *szCredits =
          "Alejandro de Garate <alex_degarate@hotmail.com>\n"
          "Alex Shashkov <alex_shashkov@users.sourceforge.net>\n"
          "Alex Strickland <sscc@mweb.co.za>\n"
          "Alexander S. Kresin <alex@belacy.belgorod.su>\n"
          "Andi Jahja <xharbour@cbn.net.id>\n"
          "Andy Wos <andrwos@bigpond.com>\n"
          "Antonio Carlos Pantaglione <toninho@fwi.com.br>\n"
          "Antonio Linares <alinares@fivetech.com>\n"
          "April White <awhite@user.rose.com>\n"
          "Ath <Ath@ath.nl>\n"
          "Augusto Infante <august@winbuilt.com>\n"
          "Bil Simser <bsimser@home.com>\n"
          "Brian Hays <bhays@abacuslaw.com>\n"
          "Bruno Cantero <bruno@issnet.net>\n"
          "Budyanto Djajapermana <budyanto@centrin.net.id>\n"
          "Charles Kwon <Charles@fivetech.net>\n"
          "Chen Kedem <niki@actcom.co.il>\n"
          "Dave Pearson <davep@davep.org>\n"
          "David G. Holm <dholm@jsd-llc.com>\n"
          "Davor Siklic <siki@msoft.cz>\n"
          "Dmitry V. Korzhov <dk@april26.spb.ru>\n"
          "Eddie Runia <eddie@runia.com>\n"
          "Eduardo Fernandes <modalsist@yahoo.com.br>\n"
          "Enrico Maria Giordano <e.m.giordano@emagsoftware.it>\n"
          "Felipe G. Coury <fcoury@creation.com.br>\n"
          "Francesco Saverio Giudice <info@fsgiudice.com>\n"
          "Giancarlo Niccolai <gc@niccolai.ws>\n"
          "Gonzalo A. Diethelm <gonzalo.diethelm@iname.com>\n"
          "Henryk Olkowski <oh1@op.pl>\n"
          "Horacio D. Roldan Kasimatis <harbour_ar@yahoo.com.ar>\n"
          "Ian Anderson <i.anderson@procon-online.de>\n"
          "Ignacio Ortiz de Zuniga <ignacio@fivetech.com>\n"
          "Jacek Potempa <Jacek.Potempa@otc.com.pl>\n"
          "Janica Lubos <janica@fornax.elf.stuba.sk>\n"
          "Jean-Francois Lefebvre (mafact) <jfl@mafact.com>\n"
          "Jose F. Gimenez <jfgimenez@wanadoo.es>\n"
          "Jose Lalin <dezac@corevia.com>\n"
          "Leslee Griffith <les.griffith@vantagesystems.ca>\n"
          "Lorenzo Fiorini <lorenzo_fiorini@teamwork.it>\n"
          "Luis Krause Mantilla <lkrausem@shaw.ca>\n"
          "Luiz Rafael Culik <culik@sl.conex.net>\n"
          "Manuel Ruiz <mrt@joca.es>\n"
          "Marcelo Lombardo <lombardo@uol.com.br>\n"
          "Marcos Antonio Gambeta <marcosgambeta@yahoo.com.br>\n"
          "Marek Paliwoda <paliwoda@inetia.pl>\n"
          "Martin Vogel <vogel@inttec.de>\n"
          "Matteo Baccan <baccan@isanet.it>\n"
          "Matthew Hamilton <mhamilton@bunge.com.au>\n"
          "Mauricio Abre <maurifull@datafull.com>\n"
          "Maurilio Longo <maurilio.longo@libero.it>\n"
          "Mighty-Siil <siil@usa.net>\n"
          "Mindaugas Kavaliauskas <dbtopas@dbtopas.lt>\n"
          "Nicolas del Pozo <niko@geroa.com>\n"
          "Patrick Mast <patrick@xHarbour.com>\n"
          "Paul Tucker <ptucker@sympatico.ca>\n"
          "Pavel Tsarenko <tpe2@mail.ru>\n"
          "Peter Rees <peter@rees.co.nz>\n"
          "Peter Townsend <cephas@tpgi.com.au>\n"
          "Phil Barnett <philb@iag.net>\n"
          "Phil Krylov <phil@newstar.rinet.ru>\n"
          "Philip Chee <philip@aleytys.pc.my>\n"
          "Pritpal Bedi <pritpal@vouchcac.com>\n"
          "Przemyslaw Czerpak <druzus@polbox.com>\n"
          "Rodrigo Moreno <rodrigo_moreno@users.sourceforge.net>\n"
          "Ron Pinkas <ron@profit-master.com>\n"
          "Ryszard Glab <rglab@imid.med.pl>\n"
          "Sylvain Robert <s.robert@videotron.ca>\n"
          "Tim Stone <timstone@mstrlink.com>\n"
          "Tommi Rouvali <tommi@rouvali.com>\n"
          "Tony Bretado <jabrecer@users.sourceforge.net>\n"
          "Vicente Guerra <vicente@guerra.com.mx>\n"
          "Viktor Szakats <viktor.szakats@syenar.hu>\n"
          "Vladimir Kazimirchik <v_kazimirchik@yahoo.com>\n"
          "Walter Negro <anegro@overnet.com.ar>";

  return szCredits;
}
