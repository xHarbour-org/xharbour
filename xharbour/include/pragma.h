/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Andi Jahja <xharbour@telkom.net.id>
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

#ifndef _PRAGMA_H_INCLUDE
#define _PRAGMA_H_INCLUDE

#ifndef HB_NO_PRAGMA
   #if defined( _MSC_VER ) || defined( __WATCOMC__ ) || defined( __DMC__ )
      #if ( defined(_MSC_FULL_VER) && (_MSC_FULL_VER==13104035) )
         #pragma comment( lib, "bufferoverflowU" )
      #endif
      #pragma comment( lib, "uuid" )
      #pragma comment( lib, "psapi" )
      #pragma comment( lib, "iphlpapi" )
      #pragma comment( lib, "version" )
      #pragma comment( lib, "shell32" )
      #pragma comment( lib, "kernel32" )
      #pragma comment( lib, "user32" )
      #pragma comment( lib, "advapi32" )
      #pragma comment( lib, "gdi32" )
      #pragma comment( lib, "winspool" )
      #pragma comment( lib, "ole32" )
      #pragma comment( lib, "oleaut32" )
      #pragma comment( lib, "ws2_32" )
      #pragma comment( lib, "mpr" )
      #pragma comment( lib, "winmm" )
      #pragma comment( lib, "comctl32" )
      #pragma comment( lib, "comdlg32" )

      #ifdef HB_NEEDS_ACE
         #pragma comment( lib, "ace32" )
      #endif
   #endif /* _MSC_VER */

#endif /* _HB_NO_PRAGMA */
#endif /* _PRAGMA_H_INCLUDE */
