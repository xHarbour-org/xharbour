/*
 * $Id: hbsxdef.ch 7743 2007-09-13 14:40:24Z druzus $
 */

/*
 * Harbour Project source code:
 *    SIx driver functions
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
 *
 * Copyright 2007 Miguel Angel Marchuet <miguelangel@marchuet.net>
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
#include "fileio.ch"
#include "dbinfo.ch"

static function Sx_SemName( xSem )
   local cPath, cName, cExt, cDrive
   if valtype( xSem ) == "C"
      cName := xSem
   else
      cName := ordName( xSem )
   endif
   if !empty( cName )
      hb_FNameSplit( cName, @cPath, @cName, @cExt, @cDrive )
      if empty( cExt )
         cExt := ".sem"
      endif
      return hb_FNameMerge( cPath, cName, cExt, cDrive )
   endif
return nil

static function Sx_SemOpen( cName, lNew )
   local hFile, cBuf := space( 2 ), i
   for i:=1 to 1000
      if file( cName )
         hFile := fopen( cName, FO_READWRITE + FO_EXCLUSIVE )
         lNew := .f.
      elseif pcount() > 1
         hFile := fcreate( cName )
         lNew := .t.
      endif
      if hFile != -1
         exit
      endif
   next
return hFile

function Sx_MakeSem( xSem )
   local nUsrs := -1, cName, hFile, cBuf := space( 2 ), lNew := .f.
   if !empty( cName := Sx_SemName( xSem ) )
      if ( hFile := Sx_SemOpen( cName, @lNew ) ) != -1
         if lNew
            fwrite( hFile, i2bin( nUsrs := 1 ), 2 )
         elseif fread( hFile, @cBuf, 2 ) == 2
            nUsrs := bin2i( cBuf ) + 1
            fseek( hFile, 0, FS_SET )
            fwrite( hFile, i2bin( nUsrs ), 2 )
         endif
         fclose( hFile )
      endif
   endif
return nUsrs

function Sx_KillSem( xSem )
   local nUsrs := -1, cName, hFile, cBuf := space( 2 )
   if !empty( cName := Sx_SemName( xSem ) )
      if ( hFile := Sx_SemOpen( cName ) ) != -1
         if fread( hFile, @cBuf, 2 ) == 2
            nUsrs := bin2i( cBuf ) - 1
            fseek( hFile, 0, FS_SET )
            fwrite( hFile, i2bin( nUsrs ), 2 )
         endif
         fclose( hFile )
         if nUsrs == 0
            ferase( cName )
         endif
      endif
   endif
return nUsrs

function Sx_IsSem( xSem )
   local cName
   if !empty( cName := Sx_SemName( xSem ) )
      return file( cName )
   endif
return .f.

function Sx_Freeze( xTag, nBag )
return dbOrderInfo( DBOI_CUSTOM, xTag, nBag, .T. )

function Sx_Warm( xTag, nBag )
return dbOrderInfo( DBOI_CUSTOM, xTag, nBag, .F. )

function Sx_Chill( xTag, nBag ) 
return dbOrderInfo( DBOI_CHGONLY, xTag, nBag, .T. )
