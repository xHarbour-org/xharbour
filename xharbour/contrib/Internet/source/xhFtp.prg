/*
 * xHarbour Project source code:
 *
 * internet.lib xhFtp CLASS
 *
 * Copyright 2002 Pritpal Bedi [pritpal@vouchcac.com]
 * www - http://www.xharbour.org
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
 */

//---------------------------------------------------------------------//

#include    'HBClass.ch'
#include    'WinINet.ch'
#include    'cStruct.ch'
#include   'WinTypes.ch'
#include    'Winuser.ch'
#include     'Common.ch'

//---------------------------------------------------------------------//

pragma pack(4)

typedef struct { ;
    DWORD    dwLowDateTime;
    DWORD    dwHighDateTime;
} FILETIME

typedef struct { ;
    DWORD    dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD    nFileSizeHigh;
    DWORD    nFileSizeLow;
    DWORD    dwReserved0;
    DWORD    dwReserved1;
    char     cFileName[ MAX_PATH ];
    char     cAlternateFileName[ 14 ];
} FIND_DATA

typedef struct { ;
    WORD     wYear ;
    WORD     wMonth ;
    WORD     wDayOfWeek ;
    WORD     wDay ;
    WORD     wHour ;
    WORD     wMinute ;
    WORD     wSecond ;
    WORD     wMilliSeconds ;
} SYSTEMTIME

//---------------------------------------------------------------------//

CLASS xhFtp

VAR    hInternet
VAR    hFtp
VAR    cServer
VAR    cUserName
VAR    cPassword
VAR    nPort
VAR    aDebug

METHOD New               CONSTRUCTOR

METHOD Connect
METHOD DisConnect

METHOD PutFile
METHOD GetFile
METHOD DeleteFile
METHOD RenameFile

METHOD GetDirectory
METHOD CreateDirectory
METHOD RemoveDirectory

METHOD SetCurrentDirectory
METHOD GetCurrentDirectory

ENDCLASS

//---------------------------------------------------------------------//

METHOD New( cServer, cUserName, cPass, nPort, hInternet ) CLASS xhFtp
 
DEFAULT nPort TO 21

::cServer   := cServer
::cUserName := cUserName
::cPassword := cPass
::nPort     := nPort
::aDebug    := {}

if hInternet <> nil .and. hInternet <> 0
   ::hInternet := hInternet
else
   ::hInternet := InternetOpen()
endif	

return self 

//---------------------------------------------------------------------//

METHOD Connect() CLASS xhFtp
local lRet := .f.

if ::hInternet <> nil .and. ::hInternet <> 0
   ::hFtp := InternetConnect( ::hInternet, ::cServer, ;
                 ::nPort, ::cUserName, ::cPassword, INTERNET_SERVICE_FTP )
   if ::hFtp <> 0
      lRet := .t.
   endif 
endif

return lRet

//---------------------------------------------------------------------//

METHOD DisConnect() CLASS xhFtp
local lRet := .f.

if ::hFtp <> 0
   if InternetCloseHandle( ::hFtp )
      if InternetCloseHandle( ::hInternet )
         lRet := .t.
      endif
   endif
endif

return lRet

//---------------------------------------------------------------------//

METHOD GetFile( cRemoteFile, cLocalFile ) CLASS xhFtp
local lRet := .f.

lRet := FtpGetFile( ::hFtp, cRemoteFile, cLocalFile )

return lRet

//---------------------------------------------------------------------//

METHOD PutFile( cLocalFile, cRemoteFile ) CLASS xhFtp
local lRet := .f.

lRet := FtpPutFile( ::hFtp, cLocalFile, cRemoteFile )

return lRet

//---------------------------------------------------------------------//

METHOD DeleteFile( cRemoteFile ) CLASS xhFtp
local lRet

lRet := FtpDeleteFile( ::hFtp, cRemoteFile )

return lRet

//---------------------------------------------------------------------//

METHOD RenameFile( cExistingFile, cNewFile ) CLASS xhFtp
local lRet

lRet := FtpRenameFile( ::hFtp, cExistingFile, cNewFile )

return lRet

//---------------------------------------------------------------------//

METHOD CreateDirectory( cDirectory ) CLASS xhFtp
local lRet

lRet := FtpCreateDirectory( ::hFtp, cDirectory )

return lRet

//---------------------------------------------------------------------//

METHOD RemoveDirectory( cDirectory ) CLASS xhFtp
local lRet

lRet := FtpRemoveDirectory( ::hFtp, cDirectory )

return lRet

//---------------------------------------------------------------------//

METHOD GetCurrentDirectory() CLASS xhFtp
local cDirectory := space( 260 )

FtpGetCurrentDirectory( ::hFtp, @cDirectory )

return ( cDirectory )

//---------------------------------------------------------------------//

METHOD SetCurrentDirectory( cDirectory ) CLASS xhFtp
local lRet

lRet := FtpSetCurrentDirectory( ::hFtp, cDirectory )

return lRet

//---------------------------------------------------------------------//

METHOD GetDirectory( cFileSpec )
local d_       := {}
local aDir     := {}
local findData IS FIND_DATA
local sysTime  IS SYSTEMTIME
local hFind
local cSysTime
local cFileInfo

DEFAULT cFileSpec TO '*.*'

cFileInfo := findData:value
cSysTime  := sysTime:value

hFind := FtpFindFirstFile( ::hFtp, cFileSpec, @cFileInfo )

if hFind <> 0
   findData:Buffer( cFileInfo )
	
   d_:= array( 5 )
   d_[1] := findData:cFileName:value
   d_[2] := findData:dwFileAttributes
   d_[3] := findData:nFileSizeLow

   if FileTimeToSystemTime( findData:ftLastWriteTime:value, @cSysTime )
      sysTime:Buffer( cSysTime )
      d_[4] := ymd2date( sysTime:wYear, sysTime:wMOnth, sysTime:wDay )
      d_[5] := hms2time( sysTime:wHour, sysTime:wMinute, sysTime:wSecond )
   endif

   aadd( aDir, d_ )

   do while .t.
      if !InternetFindNextFile( hFind, @cFileInfo )
         exit
      endif

      findData:Buffer( cFileInfo )
      d_:= array( 5 )
      d_[1] := findData:cFileName:value
      d_[2] := findData:dwFileAttributes
      d_[3] := findData:nFileSizeLow

      if FileTimeToSystemTime( findData:ftLastWriteTime:value, @cSysTime )
         sysTime:Buffer( cSysTime )
         d_[4] := ymd2date( sysTime:wYear, sysTime:wMOnth, sysTime:wDay )
         d_[5] := hms2time( sysTime:wHour, sysTime:wMinute, sysTime:wSecond )
      endif

      aadd( aDir, d_ )

   enddo
endif

return aDir

//---------------------------------------------------------------------//

static function ymd2date( nYear, nMonth, nDay )
local sDate

sDate := ( strzero( nYear,4 ) + strzero( nMonth,2 ) + strzero( nDay,2 ) )

return sDate

//---------------------------------------------------------------------//

static function hms2time( nHour, nMinute, nSecond )
local cTime

cTime := strzero( nHour,2 ) + ':' + strzero( nMinute,2 ) + ':' + strzero( nSecond,2 )

return cTime

//---------------------------------------------------------------------//
//---------------------------------------------------------------------//
//---------------------------------------------------------------------//
//
//               Demonstration Program to Use xhFtp Class
//
//---------------------------------------------------------------------//
//---------------------------------------------------------------------//
//---------------------------------------------------------------------//

function xhTestFtp( cServer, cUserName, cPassword, nPort, cCmd, cSource, cTarget )
local oFtp, xRet

DEFAULT nPort TO 21

cCmd := alltrim( upper( cCmd ) )

oFtp := xhFtp():New( cServer, cUsername, cPassword, nPort, /* hInternet */ )

if oFtp:connect()
   do case
   
   case cCmd == 'MKDIR'
      xRet := oFtp:CreateDirectory( cSource )
   	
   case cCmd == 'RMDIR'
      xRet := oFtp:RemoveDirectory( cSource )
   	
   case cCmd == 'DIR'
      xRet := oFtp:GetDirectory( cSource )
   	
   case cCmd == 'CHDIR'
      xRet := oFtp:SetCurrentDirectory( cSource )
   	
   case cCmd == 'WKDIR'
      xRet := oFtp:GetCurrentDirectory()
	   
   case cCmd == 'RENAME'
      xRet := oFtp:RenameFile( cSource, cTarget )
   	
   case cCmd == 'DEL'
      xRet := oFtp:DeleteFile( cSource )
   	
   case cCmd == 'PUT'
      xRet := oFtp:PutFile( cSource, cTarget )
   	
   case cCmd == 'GET'
      xRet := oFtp:GetFile( cSource, cTarget )
   	
   endcase

   oFtp:Disconnect()
endif

return xRet

//---------------------------------------------------------------------//

