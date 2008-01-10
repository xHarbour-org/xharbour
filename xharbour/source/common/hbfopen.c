/*
 * $Id: hbfopen.c,v 1.1 2007/12/29 12:50:54 likewolf Exp $
 */

/*
 * Harbour Project source code:
 *
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapifs.h"
#include "hbset.h"

static int  s_iFileCase = HB_SET_CASE_MIXED;
static int  s_iDirCase  = HB_SET_CASE_MIXED;
static BOOL s_fFnTrim   = FALSE;
static char s_cDirSep   = OS_PATH_DELIMITER;

BYTE * hb_fsNameConv( BYTE * szFileName, BOOL * pfFree )
{
   if( s_fFnTrim || s_cDirSep != OS_PATH_DELIMITER ||
       s_iFileCase != HB_SET_CASE_MIXED || s_iDirCase != HB_SET_CASE_MIXED )
   {
      PHB_FNAME pFileName;
      ULONG ulLen;

      if( pfFree )
      {
         BYTE * szNew = ( BYTE * ) hb_xgrab( _POSIX_PATH_MAX + 1 );
         hb_strncpy( ( char * ) szNew, ( char * ) szFileName, _POSIX_PATH_MAX );
         szFileName = szNew;
         *pfFree = TRUE;
      }

      if( s_cDirSep != OS_PATH_DELIMITER )
      {
         BYTE *p = szFileName;
         while( *p )
         {
            if( *p == s_cDirSep )
               *p = OS_PATH_DELIMITER;
            p++;
         }
      }

      pFileName = hb_fsFNameSplit( ( char * ) szFileName );

      /* strip trailing and leading spaces */
      if( s_fFnTrim )
      {
         if( pFileName->szName )
         {
            ulLen = strlen( pFileName->szName );
            while( ulLen && pFileName->szName[ulLen - 1] == ' ' )
               --ulLen;
            while( ulLen && pFileName->szName[0] == ' ' )
            {
               ++pFileName->szName;
               --ulLen;
            }
            pFileName->szName[ulLen] = '\0';
         }
         if( pFileName->szExtension )
         {
            ulLen = strlen( pFileName->szExtension );
            while( ulLen && pFileName->szExtension[ulLen - 1] == ' ' )
               --ulLen;
            while( ulLen && pFileName->szExtension[0] == ' ' )
            {
               ++pFileName->szExtension;
               --ulLen;
            }
            pFileName->szExtension[ulLen] = '\0';
         }
      }

      /* FILECASE */
      if( s_iFileCase == HB_SET_CASE_LOWER )
      {
         if( pFileName->szName )
            hb_strlow( pFileName->szName );
         if( pFileName->szExtension )
            hb_strlow( pFileName->szExtension );
      }
      else if( s_iFileCase == HB_SET_CASE_UPPER )
      {
         if( pFileName->szName )
            hb_strupr( pFileName->szName );
         if( pFileName->szExtension )
            hb_strupr( pFileName->szExtension );
      }

      /* DIRCASE */
      if( pFileName->szPath )
      {
         if( s_iDirCase == HB_SET_CASE_LOWER )
            hb_strlow( pFileName->szPath );
         else if( s_iDirCase == HB_SET_CASE_UPPER )
            hb_strupr( pFileName->szPath );
      }

      hb_fsFNameMerge( ( char * ) szFileName, pFileName );
      hb_xfree( pFileName );
   }
   else if( pfFree )
      *pfFree = FALSE;

   return szFileName;
}

FILE * hb_fopen( const char *path, const char *mode )
{
   BOOL fFree;
   char * pszFile = ( char * ) hb_fsNameConv( ( BYTE * ) path, &fFree );
   FILE * file = fopen( pszFile, mode );

   if( fFree )
      hb_xfree( pszFile );

   return file;
}


