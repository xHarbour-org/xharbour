/*
 * $Id: hbwbase.c,v 1.1 2004/01/16 21:37:43 paultucker Exp $
 */

/*
 * Harbour Project source code:
 * Header file for those Win users without the Platform SDK installed.
 *
 * Copyright 2003
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

#ifndef _HB_WBASE
#define _HB_WBASE

   #ifndef VER_PLATFORM_WIN32_WINDOWS
      #define VER_PLATFORM_WIN32_WINDOWS 1
   #endif
   #ifndef VER_PLATFORM_WIN32_CE
      #define VER_PLATFORM_WIN32_CE 3
   #endif
   #ifndef VER_SUITE_PERSONAL
      #define VER_SUITE_PERSONAL 0x200
   #endif
   #ifndef VER_SUITE_ENTERPRISE
      #define VER_SUITE_ENTERPRISE 0x002
   #endif
   #ifndef VER_SUITE_DATACENTER
      #define VER_SUITE_DATACENTER 0x080
   #endif
   #ifndef VER_SUITE_BLADE
      #define VER_SUITE_BLADE 0x400
   #endif
   #ifndef VER_NT_WORKSTATION
      #define VER_NT_WORKSTATION       0x0000001
      #define VER_NT_DOMAIN_CONTROLLER 0x0000002
      #define VER_NT_SERVER            0x0000003
   #endif
   /* For those that don't have the MS Platform SDK installed */
   typedef struct _HBOSVERSIONINFOEX {
     DWORD dwOSVersionInfoSize;
     DWORD dwMajorVersion;
     DWORD dwMinorVersion;
     DWORD dwBuildNumber;
     DWORD dwPlatformId;
     #ifdef UNICODE
     WCHAR  szCSDVersion[128];
     #else
     CHAR  szCSDVersion[128];
     #endif
     WORD  wServicePackMajor;
     WORD  wServicePackMinor;
     WORD  wSuiteMask;
     BYTE  wProductType;
     BYTE  wReserved;
   } HBOSVERSIONINFOEX, *LPHBOSVERSIONINFOEX;

#endif
