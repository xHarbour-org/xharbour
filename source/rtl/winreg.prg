/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 *     Copyright 2004 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 *
 * See doc/license.txt for licensing terms.
 *
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
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
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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
*/

/*

   Registry query and set for Win32

*/

#ifndef __PLATFORM__Windows

FUNCTION QueryRegistry()

   RETURN( .F. )

FUNCTION GetRegistry()

   RETURN( NIL )

FUNCTION SetRegistry()

   RETURN( .F. )

#else

//
//
// Predefined Value Types. from WINNT.H
//
#define KEY_QUERY_VALUE         1
#define KEY_SET_VALUE           2
#define KEY_CREATE_SUB_KEY      4
#define KEY_ENUMERATE_SUB_KEYS  8
#define KEY_NOTIFY              16
#define KEY_CREATE_LINK         32
#define REG_NONE                    ( 0 )   // No value type
#define REG_SZ                      ( 1 )   // Unicode nul terminated string
#define REG_EXPAND_SZ               ( 2 )   // Unicode nul terminated string
// (with environment variable references)
#define REG_BINARY                  ( 3 )   // Free form binary
#define REG_DWORD                   ( 4 )   // 32-bit number
#define REG_DWORD_LITTLE_ENDIAN     ( 4 )   // 32-bit number (same as REG_DWORD)
#define REG_DWORD_BIG_ENDIAN        ( 5 )   // 32-bit number
#define REG_LINK                    ( 6 )   // Symbolic Link (unicode)
#define REG_MULTI_SZ                ( 7 )   // Multiple Unicode strings
#define REG_RESOURCE_LIST           ( 8 )   // Resource list in the resource map
#define REG_FULL_RESOURCE_DESCRIPTOR ( 9 )  // Resource list in the hardware description
#define REG_RESOURCE_REQUIREMENTS_LIST ( 10 )

//---- QueryRegistry ------------------------

FUNCTION QueryRegistry( nHKEYHandle, cKeyName, cEntryName, xValue, lSetIt )

   LOCAL rVal, xKey := GetRegistry( nHKEYHandle, cKeyName, cEntryName )
   LOCAL cValType := ValType( xValue )

   IF lSetIt == NIL
      lSetIt := .F.
   ENDIF
   IF cValType == "L"
      xValue := iif( xValue, 1, 0 )
      cValType := ValType( xValue )
   ELSEIF cValType == "D"
      xValue := DToS( xValue )
      cValType := ValType( xValue )
   ENDIF
   rVal := ( xKey != NIL .AND. xValue != NIL .AND. cValType == ValType( xKey ) .AND. xValue == xKey )
   IF !rVal .AND. lSetIt
      rVal := SetRegistry( nHKEYHandle, cKeyName, cEntryName, xValue )
   ENDIF

   RETURN rVal

//---- GetRegistry ------------------------

FUNCTION GetRegistry( nHKEYHandle, cKeyName, cEntryName )

   LOCAL cName := NIL, nKeyHandle := 0, nValueType

   IF nHKeyHandle == NIL
      nHKeyHandle := 0
   ENDIF
   IF Empty( WinRegOpenKeyEx( nHKEYHandle, cKeyName,0, KEY_QUERY_VALUE, @nKeyHandle ) )
      nValueType  := 0
      // retrieve the length of the value
      IF WinRegQueryValueEx( nKeyHandle, cEntryName, 0, @nValueType, @cName ) > 0
         IF nValueType == REG_DWORD .OR. ;
               nValueType == REG_DWORD_LITTLE_ENDIAN .OR. ;
               nValueType == REG_DWORD_BIG_ENDIAN .OR. ;
               nValueType == REG_BINARY
            cName := BIN2U( cName )
         ELSE
            cName := StrTran( CSTR( cName ), Chr( 0 ) )
         ENDIF
      ENDIF
      WinRegCloseKey( nKeyHandle )
   ENDIF

   RETURN cName

//---- SetRegistry ------------------------

FUNCTION SetRegistry( nHKEYHandle, cKeyName, cEntryName, xValue )

   LOCAL cName := NIL, nKeyHandle, nValueType
   LOCAL rVal := .F.
   LOCAL nResult := 1

   IF nHKeyHandle == NIL
      nHKeyHandle := 0
   ENDIF
   nKeyHandle := 0
   IF WinRegCreateKeyEx( nHKEYHandle, cKeyName, 0, 0, 0, KEY_SET_VALUE, 0, @nKeyHandle, @nResult ) == 0
      SWITCH ValType( xValue )          // no support for Arrays, Codeblock ...
      CASE 'L'
         nValueType := REG_DWORD
         cName := iif( xValue, 1, 0 )
         EXIT
      CASE 'D'
         nValueType := REG_SZ
         cName := DToS( xValue )
         EXIT
      CASE 'N'
         nValueType := REG_DWORD
         cName := xValue
         EXIT
      CASE 'C'
      CASE 'M'
         nValueType := REG_SZ
         cName := xValue
         EXIT
      END SWITCH
      IF cName != NIL
         rVal := Empty( WinRegSetValueEx( nKeyHandle, cEntryName,0, nValueType, cName ) )
      ENDIF
      WinRegCloseKey( nKeyHandle )
   ENDIF

   RETURN( rVal )

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.h"
#include "hbapiitm.h"

static HKEY regkeykey( HB_PTRUINT nKey)
{
  HKEY Result ;
  if (nKey >= (HB_PTRUINT) HKEY_CLASSES_ROOT && nKey <= (HB_PTRUINT) HKEY_DYN_DATA )
     return (HKEY) nKey;
  switch ( nKey )
  {
  case 0 :
    Result = (HKEY) HKEY_LOCAL_MACHINE;
    break;
  case 1 :
    Result = (HKEY) HKEY_CLASSES_ROOT ;
    break;
  case 2 :
    Result = (HKEY) HKEY_CURRENT_USER ;
    break;
  case 3 :
    Result = (HKEY) HKEY_CURRENT_CONFIG ;
    break;
  case 4 :
    Result = (HKEY) HKEY_LOCAL_MACHINE;
    break;
  case 5 :
    Result = (HKEY) HKEY_USERS;
    break;
  default :
    Result = (HKEY) nKey ;
  }
  return  (HKEY) Result  ;
}

HB_FUNC_STATIC( WINREGCREATEKEYEX  )
{
  HKEY  hWnd ;
  ULONG rVal= ( ULONG ) -1, nresult = hb_parnl( 9 );

  if ( RegCreateKeyEx( regkeykey( ( HB_PTRUINT ) hb_parnint( 1 ) ), ( const char * ) hb_parc( 2 ), hb_parnl( 3 ), NULL, hb_parnl( 5 ), hb_parnl( 6 ), NULL, ( PHKEY ) &hWnd, &nresult ) == ERROR_SUCCESS )
  {
    rVal = ERROR_SUCCESS;

      // hb_stornl( ( ULONG ) hWnd, 8 );
      hb_storptr( hWnd, 8 );

    if ( ISBYREF( 9 ) )
    {
      hb_stornl( nresult, 9 );
    }
  }
  hb_retnl( rVal );
}

HB_FUNC_STATIC( WINREGOPENKEYEX )
{
  ULONG rVal= ( ULONG ) -1 ;
  void * hWnd ;
  if ( RegOpenKeyEx( regkeykey( ( HB_PTRUINT ) hb_parnint( 1 ) ), ( const char * ) hb_parc( 2 ), 0, hb_parnl( 4 ), ( PHKEY ) &hWnd ) == ERROR_SUCCESS )
  {
    rVal = ERROR_SUCCESS;
    hb_storptr( hWnd, 5 );

  }
  hb_retnl( rVal ) ;
}

HB_FUNC_STATIC( WINREGQUERYVALUEEX )
{
  BYTE *cValue;
  const char *cKey ;
  DWORD nSize = 0, nType ;

  cKey = ( const char *) hb_parc( 2 ) ;
  if ( RegQueryValueEx( ( HKEY ) hb_parptr( 1 ), cKey, 0, &nType, 0, &nSize ) == ERROR_SUCCESS )
  {
    if ( nSize > 0 )
    {
      cValue = ( BYTE *) hb_xgrab( nSize );
      if ( cValue )
      {
        if ( RegQueryValueEx( ( HKEY ) hb_parptr( 1 ), cKey, 0, &nType, ( BYTE *) cValue, &nSize ) == ERROR_SUCCESS )
        {
          if ( ISBYREF( 4 ) )
          {
            hb_stornl( nType, 4 );
          }
          if ( ISBYREF( 5 ) )
          {
            hb_storclen( ( char *) cValue, nSize, 5 );
          }
        }
        else
        {
          nSize = 0 ;
        }
        hb_xfree( cValue );
      }
      else
      {
        nSize = 0;
      }
    }
  }
  else
  {
    nSize = 0 ;
  }
  hb_retnl( nSize);
}

HB_FUNC_STATIC( WINREGSETVALUEEX )
{
  const char *cKey;
  BYTE *cValue;
  DWORD nType, nSpace;

  cKey = hb_parc( 2 );
  nType = hb_parnl( 4 );
  if ( nType != REG_DWORD )
  {
    cValue = ( BYTE *) hb_parc( 5 );
    hb_retni( RegSetValueEx( ( HKEY ) hb_parptr( 1 ), cKey, 0, nType, ( BYTE *) cValue, (DWORD) hb_parclen( 5 ) + 1 ) );
  }
  else
  {
    nSpace= hb_parnl( 5 );
    hb_retni( RegSetValueEx( ( HKEY ) hb_parptr( 1 ), cKey, 0, nType, (BYTE *) &nSpace, sizeof( REG_DWORD ) ) );
  }
}

HB_FUNC_STATIC( WINREGCLOSEKEY )
{
  hb_retnl( RegCloseKey( ( HKEY ) hb_parptr( 1 ) ) );
}

#pragma ENDDUMP

#endif
