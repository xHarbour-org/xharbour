/*----------------------------------------------------------------------------
 MINIGUI - Harbour Win32 GUI library source code

 Copyright 2002 Roberto Lopez <roblez@ciudad.com.ar>
 http://www.geocities.com/harbour_minigui/

 This program is free software; you can redistribute it and/or modify it under 
 the terms of the GNU General Public License as published by the Free Software 
 Foundation; either version 2 of the License, or (at your option) any later 
 version. 

 This program is distributed in the hope that it will be useful, but WITHOUT 
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
 FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along with 
 this software; see the file COPYING. If not, write to the Free Software 
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or 
 visit the web site http://www.gnu.org/).

 As a special exception, you have permission for additional uses of the text 
 contained in this release of Harbour Minigui.

 The exception is that, if you link the Harbour Minigui library with other 
 files to produce an executable, this does not by itself cause the resulting 
 executable to be covered by the GNU General Public License.
 Your use of that executable is in no way restricted on account of linking the 
 Harbour-Minigui library code into it.

 Parts of this project are based upon:

	"Harbour GUI framework for Win32"
 	Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 	Copyright 2001 Antonio Linares <alinares@fivetech.com>
	www - http://www.harbour-project.org

	"Harbour Project"
	Copyright 1999-2003, http://www.harbour-project.org/
---------------------------------------------------------------------------*/

#define _WIN32_IE      0x0500
#define HB_OS_WIN_32_USED
#define _WIN32_WINNT   0x0400
#include <shlobj.h>

#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "winreg.h"
#include "tchar.h"

HB_FUNC ( INITIPADDRESS )
{
	HWND hWnd;
	HWND hIpAddress;
   int Style  ;

	INITCOMMONCONTROLSEX  i;
	i.dwSize = sizeof(INITCOMMONCONTROLSEX);
	i.dwICC = ICC_INTERNET_CLASSES;
	InitCommonControlsEx(&i);

	hWnd = (HWND) hb_parnl (1);

   Style = hb_parni(3) ;

	hIpAddress = CreateWindowEx(WS_EX_CLIENTEDGE,WC_IPADDRESS,"",
	Style ,
   hb_parni(4), hb_parni(5) ,hb_parni(6) ,hb_parni(7) ,
	hWnd,(HMENU)hb_parni(2) , GetModuleHandle(NULL) , NULL ) ;

	hb_retnl ( (LONG) hIpAddress );
}

HB_FUNC ( SETIPADDRESS )
{
	HWND hWnd;
	BYTE v1, v2, v3, v4;

	hWnd = (HWND) hb_parnl (1);

	v1 = (BYTE) hb_parni(2);
	v2 = (BYTE) hb_parni(3);
	v3 = (BYTE) hb_parni(4);
	v4 = (BYTE) hb_parni(5);

	SendMessage(hWnd, IPM_SETADDRESS, 0, MAKEIPADDRESS(v1,v2,v3,v4));
}

HB_FUNC ( GETIPADDRESS )
{
	HWND hWnd;
	DWORD pdwAddr;
	BYTE v1, v2, v3, v4;

	hWnd = (HWND) hb_parnl (1);

	SendMessage(hWnd, IPM_GETADDRESS, 0, (LPARAM)(LPDWORD)&pdwAddr);

	v1 = FIRST_IPADDRESS( pdwAddr );
	v2 = SECOND_IPADDRESS( pdwAddr );
	v3 = THIRD_IPADDRESS( pdwAddr );
	v4 = FOURTH_IPADDRESS( pdwAddr );

	hb_reta( 4 );
	hb_storni( (INT) v1, -1, 1 );
	hb_storni( (INT) v2, -1, 2 );
	hb_storni( (INT) v3, -1, 3 );
	hb_storni( (INT) v4, -1, 4 );
}

HB_FUNC ( CLEARIPADDRESS )
{
	HWND hWnd;

	hWnd = (HWND) hb_parnl (1);

	SendMessage(hWnd, IPM_CLEARADDRESS, 0, 0);
}


