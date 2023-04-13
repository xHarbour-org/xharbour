/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Set functions
 *
 * Copyright 1999-2003 David G. Holm <dholm@jsd-llc.com>
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
 * Copyright 2008-2009 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_osEncodeCP()
 *    hb_osDecodeCP()
 *
 * See COPYING for licensing terms.
 *
 */

#include <ctype.h>

#define HB_OS_WIN_USED
#define _HB_SET_INTERNAL_

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbapigt.h"
#include "hbapilng.h"
#include "hbapicdp.h"
#include "hbapirdd.h"
#include "hbset.h"
#include "hbstack.h"

typedef struct HB_SET_LISTENER_
{
   int listener;
   HB_SET_LISTENER_CALLBACK * callback;
   struct HB_SET_LISTENER_ * next;
} HB_SET_LISTENER, * PHB_SET_LISTENER;

typedef struct
{
   PHB_SET_LISTENER first;
   PHB_SET_LISTENER last;
   int counter;
} HB_SET_LISTENER_LST, * PHB_SET_LISTENER_LST;

HB_EXTERN_BEGIN
#if defined( HB_OS_WIN ) && ( ! defined( __RSXNT__ ) ) && ( ! defined( __CYGWIN__ ) )
   extern BOOL hb_PrinterExists( LPTSTR pPrinterName );
   extern LONG hb_PrintFileRaw( const char * cPrinterName, const char * cFileName, const char * cDocName );
   extern BOOL hb_GetDefaultPrinter( LPTSTR pPrinterName, LPDWORD pdwBufferSize );
   extern BOOL hb_isLegacyDevice( LPTSTR pPrinterName );
   extern BOOL hb_set_SetPrinterStart( void );
   extern void hb_set_SetPrinterStop( void );
#endif
extern void * hb_stackId( void );
HB_EXTERN_END

static char s_PrintFileName[ HB_PATH_MAX ], s_PrinterName[ HB_PATH_MAX ];
static BOOL s_isDefaultPrinterDevice;   /* Printer is the default device */

static char set_char( PHB_ITEM pItem, char oldChar )
{
   char newChar = oldChar;

   HB_TRACE( HB_TR_DEBUG, ( "set_char(%p, %c)", pItem, oldChar ) );

   if( HB_IS_STRING( pItem ) )
   {
      /* Only replace if string has at least one character. */
      HB_SIZE ulLen = hb_itemGetCLen( pItem );

      if( ulLen > 0 )
         newChar = *hb_itemGetCPtr( pItem );
   }
   return newChar;
}

/*
 * Change the setting if the parameter is a logical value, or is
 * either "ON" or "OFF" (regardless of case)
 */
static BOOL set_logical( PHB_ITEM pItem, BOOL bDefault )
{
   BOOL bLogical = bDefault;

   HB_TRACE( HB_TR_DEBUG, ( "set_logical(%p)", pItem ) );

   if( HB_IS_LOGICAL( pItem ) )
      bLogical = hb_itemGetL( pItem );
   else if( HB_IS_STRING( pItem ) )
   {
      char *   szString = hb_itemGetCPtr( pItem );
      HB_SIZE  ulLen    = hb_itemGetCLen( pItem );

      if( ulLen >= 2
          && HB_TOUPPER( ( UCHAR ) szString[ 0 ] ) == 'O'
          && HB_TOUPPER( ( UCHAR ) szString[ 1 ] ) == 'N' )
         bLogical = TRUE;
      else if( ulLen >= 3
               && HB_TOUPPER( ( UCHAR ) szString[ 0 ] ) == 'O'
               && HB_TOUPPER( ( UCHAR ) szString[ 1 ] ) == 'F'
               && HB_TOUPPER( ( UCHAR ) szString[ 2 ] ) == 'F' )
         bLogical = FALSE;
   }

   return bLogical;
}

static int set_number( PHB_ITEM pItem, int iOldValue )
{
   HB_TRACE( HB_TR_DEBUG, ( "set_number(%p, %d)", pItem, iOldValue ) );

   return HB_IS_NUMERIC( pItem ) ? hb_itemGetNI( pItem ) : iOldValue;
}

static char * set_string( PHB_ITEM pItem, char * szOldString )
{
   char * szString;

   HB_TRACE( HB_TR_DEBUG, ( "set_string(%p, %s)", pItem, szOldString ) );

   if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
   {
      if( szOldString )
         hb_xfree( szOldString );
      /* Limit size of SET strings to 64K, truncating if source is longer */
      szString = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
   }
   else
      szString = szOldString;

   return szString;
}

static void close_binary( PHB_SET_STRUCT pSet, HB_FHANDLE handle )
{
   HB_TRACE( HB_TR_DEBUG, ( "close_binary(%p,%p)", pSet, ( void * ) ( HB_PTRDIFF ) handle ) );

   if( handle != FS_ERROR )
   {
      /* Close the file handle without disrupting the current
         user file error value */
      USHORT user_ferror = hb_fsError();
      hb_fsClose( handle );
      hb_fsSetError( user_ferror );
#if defined( HB_OS_WIN ) && ( ! defined( __RSXNT__ ) ) && ( ! defined( __CYGWIN__ ) )
      if( pSet->hb_set_winprinter && ( pSet->hb_set_printhan == handle ) && s_PrintFileName[ 0 ] )
      {
         if( hb_fsFSize( s_PrintFileName, FALSE ) > 0 )
            hb_PrintFileRaw( s_PrinterName, s_PrintFileName,
                             ( pSet->hb_set_printerjob ? pSet->hb_set_printerjob : s_PrintFileName ) );

         hb_fsDelete( s_PrintFileName );
      }
#else
      HB_SYMBOL_UNUSED( pSet );
#endif
   }
}

static void close_text( PHB_SET_STRUCT pSet, HB_FHANDLE handle )
{
   HB_TRACE( HB_TR_DEBUG, ( "close_text(%p,%p)", pSet, ( void * ) ( HB_PTRDIFF ) handle ) );

   if( handle != FS_ERROR )
   {
      /* Close the file handle without disrupting the current
         user file error value */
      USHORT user_ferror = hb_fsError();

      if( pSet->HB_SET_EOF )
         hb_fsWrite( handle, ( BYTE * ) "\x1A", 1 );

      hb_fsClose( handle );
      hb_fsSetError( user_ferror );
   }
}

static HB_FHANDLE open_handle( PHB_SET_STRUCT pSet, const char * file_name, BOOL bAppend, const char * def_ext, HB_set_enum set_specifier )
{
   USHORT         user_ferror;
   HB_FHANDLE     handle;
   char           path[ HB_PATH_MAX ];
   const char *   szPrnFile;
   BOOL           bPipe = FALSE, bTemp = FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "open_handle(%p, %s, %d, %s, %d)", pSet, file_name, ( int ) bAppend, def_ext, ( int ) set_specifier ) );

   user_ferror = hb_fsError(); /* Save the current user file error code */
   /* Create full filename */

   szPrnFile   = path;
#if defined( HB_OS_UNIX )
   bPipe       = set_specifier == HB_SET_PRINTFILE && file_name[ 0 ] == '|';
   if( bPipe )
   {
      szPrnFile = file_name;
      file_name++;
   }
#endif
   if( ! bPipe )
   {
      PHB_FNAME pFilename = hb_fsFNameSplit( file_name );

      if( pFilename->szPath == NULL && pSet->HB_SET_DEFAULT )
         pFilename->szPath = pSet->HB_SET_DEFAULT;

      if( pSet->HB_SET_DEFEXTENSIONS && pFilename->szExtension == NULL && def_ext )
         pFilename->szExtension = def_ext;

      hb_fsFNameMerge( path, pFilename );
      hb_xfree( pFilename );

      hb_xstrcpy( s_PrinterName, file_name, 0 );
#if defined( HB_OS_WIN ) && ( ! defined( __RSXNT__ ) ) && ( ! defined( __CYGWIN__ ) )
      if( set_specifier == HB_SET_PRINTFILE )
      {
         if( hb_stricmp( s_PrinterName, "prn" ) == 0 )
         {
            DWORD nSize = HB_PATH_MAX - 1;
            hb_GetDefaultPrinter( ( LPTSTR ) s_PrinterName, &nSize );
            if( ! s_PrinterName[ 0 ] )
               hb_xstrcpy( s_PrinterName, "lpt1", 0 );
         }
         pSet->hb_set_winprinter = hb_PrinterExists( s_PrinterName );
         if( pSet->hb_set_winprinter )
         {
            szPrnFile   = s_PrintFileName;
            bTemp       = TRUE;
         }
      }
#endif
   }

   /* Open the file either in append (bAppend) or truncate mode (!bAppend), but
      always use binary mode */

   /* QUESTION: What sharing mode does Clipper use ? [vszakats] */

   handle = FS_ERROR;
   while( handle == FS_ERROR )
   {
      BOOL bCreate = FALSE;

      if( bPipe )
         handle = hb_fsPOpen( file_name, "w" );
      else if( bTemp )
         handle = hb_fsCreateTemp( NULL, NULL, FC_NORMAL, s_PrintFileName );
      else
      {
         if( bAppend ) /* Append mode */
         {
            if( hb_fsFile( szPrnFile ) ) /* If the file already exists, open it (in read-write mode, in
                                            case of non-Unix and text modes). */
            {
               handle = hb_fsOpen( szPrnFile, FO_READWRITE );
               if( handle != FS_ERROR ) /* Position to EOF */
               {  /* Special binary vs. text file handling - even for UN*X, now
                     that there's an HB_SET_EOF flag. */
                  if( set_specifier == HB_SET_PRINTFILE ) /* PRINTFILE is always binary and needs no special handling. */
                     hb_fsSeek( handle, 0, FS_END );
                  else /* All other files are text files and may have an EOF
                          ('\x1A') character at the end (both UN*X and non-UN*X,
                          now that theres an HB_SET_EOF flag). */
                  {
                     char cEOF = '\0';

                     hb_fsSeek( handle, -1, FS_END );          /* Position to last char. */
                     hb_fsRead( handle, ( BYTE * ) &cEOF, 1 ); /* Read the last char. */

                     if( cEOF == '\x1A' )                      /* If it's an EOF, */
                        hb_fsSeek( handle, -1, FS_END );       /* Then write over it. */
                  }
               }
            }
            else
               bCreate = TRUE;  /* Otherwise create a new file. */
         }
         else
            bCreate = TRUE;  /* Always create a new file for overwrite mode. */

         if( bCreate )
         {
#if defined( HB_OS_WIN ) && ( ! defined( __RSXNT__ ) ) && ( ! defined( __CYGWIN__ ) )
            if( hb_isLegacyDevice( s_PrinterName ) ) /* according to the Win SDK devices should be opened not created */
               handle = hb_fsOpen( szPrnFile, FO_READWRITE );
            else
#endif
               handle = hb_fsCreateEx( szPrnFile, FC_NORMAL, FO_DENYNONE );
         }
      }

      if( handle == FS_ERROR )
      {
         USHORT uiAction;

         /* NOTE: using switch() here will result in a compiler warning.
                  [vszakats] */
         if( set_specifier == HB_SET_ALTFILE )
            uiAction = hb_errRT_TERM( EG_CREATE, 2013, NULL, szPrnFile, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY );
         else if( set_specifier == HB_SET_PRINTFILE )
            uiAction = hb_errRT_TERM( EG_CREATE, 2014, NULL, szPrnFile, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY );
         else if( set_specifier == HB_SET_EXTRAFILE )
            uiAction = hb_errRT_TERM( EG_CREATE, 2015, NULL, szPrnFile, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY );
         else
            uiAction = E_DEFAULT;

         if( uiAction == E_DEFAULT || uiAction == E_BREAK )
            break;
      }
   }
   hb_fsSetError( user_ferror ); /* Restore the current user file error code */
   return handle;
}

static void hb_set_OSCODEPAGE( PHB_SET_STRUCT pSet )
{
   int i;

   for( i = 0; i < 256; ++i )
   {
      pSet->hb_set_oscptransto[ i ]    = ( UCHAR ) i;
      pSet->hb_set_oscptransfrom[ i ]  = ( UCHAR ) i;
   }

#ifndef HB_CDP_SUPPORT_OFF
   {
      const char *   pszHostCDP  = hb_cdpID();
      char *         pszFileCDP  = pSet->HB_SET_OSCODEPAGE;

      if( pszFileCDP && pszFileCDP[ 0 ] && pszHostCDP )
      {
         PHB_CODEPAGE   cdpFile  = hb_cdpFind( pszFileCDP );
         PHB_CODEPAGE   cdpHost  = hb_cdpFind( pszHostCDP );

         if( cdpFile && cdpHost && cdpFile != cdpHost &&
             cdpFile->nChars && cdpFile->nChars == cdpHost->nChars )
         {
            for( i = 0; i < cdpHost->nChars; ++i )
            {
               pSet->hb_set_oscptransto[ ( UCHAR ) cdpHost->CharsUpper[ i ] ]    = ( UCHAR ) cdpFile->CharsUpper[ i ];
               pSet->hb_set_oscptransto[ ( UCHAR ) cdpHost->CharsLower[ i ] ]    = ( UCHAR ) cdpFile->CharsLower[ i ];
               pSet->hb_set_oscptransfrom[ ( UCHAR ) cdpFile->CharsUpper[ i ] ]  = ( UCHAR ) cdpHost->CharsUpper[ i ];
               pSet->hb_set_oscptransfrom[ ( UCHAR ) cdpFile->CharsLower[ i ] ]  = ( UCHAR ) cdpHost->CharsLower[ i ];
            }
         }
      }
   }
#endif
}

/* Sets default printer device */
static void hb_set_SetDefaultPrinter( PHB_SET_STRUCT pSet )
{
   if( pSet->HB_SET_PRINTFILE )
      hb_xfree( pSet->HB_SET_PRINTFILE );

#ifdef HB_OS_UNIX
   pSet->HB_SET_PRINTFILE     = hb_strdup( "|lpr" );
#else
   pSet->HB_SET_PRINTFILE     = hb_strdup( "PRN" );
#endif

   s_isDefaultPrinterDevice   = TRUE;
}

/* Initializes printer if needed */
BOOL hb_set_SetPrinterStart( void )
{
   HB_THREAD_STUB

   BOOL           bDone;
   PHB_SET_STRUCT pSet = hb_stackSetStruct();

   if( pSet->hb_set_printhan != FS_ERROR )
      /* It's already open */
      bDone = TRUE;
   else if( ! s_isDefaultPrinterDevice )
      /* It's not the default printer device... can't it be opened? */
      bDone = FALSE;
   else
   {
      /* Opens printer */
      pSet->hb_set_printhan   = open_handle( pSet, pSet->HB_SET_PRINTFILE, FALSE, ".prn", HB_SET_PRINTER );
      bDone                   = ( pSet->hb_set_printhan != FS_ERROR );
   }

   return bDone;
}

/* Closes default printer if needed */
void hb_set_SetPrinterStop( void )
{
   HB_THREAD_STUB

   PHB_SET_STRUCT pSet = hb_stackSetStruct();

   if( pSet->hb_set_printhan != FS_ERROR &&                 /* It's open */
       s_isDefaultPrinterDevice &&                          /* It's the default printer device */
       ! pSet->HB_SET_PRINTER &&                            /* SET PRINTER OFF */
       hb_stricmp( pSet->HB_SET_DEVICE, "PRINTER" ) != 0 )  /* SET DEVICE TO SCREEN */
   {
      /* Closes printer */
      close_binary( pSet, pSet->hb_set_printhan );
      pSet->hb_set_printhan = FS_ERROR;
   }
}

BOOL hb_setSetCentury( BOOL new_century_setting )
{
   HB_THREAD_STUB
   PHB_SET_STRUCT pSet                 = hb_stackSetStruct();
   BOOL           old_century_setting  = pSet->hb_set_century;

   pSet->hb_set_century = new_century_setting;
   /*
    * if the setting changed, adjust the current date format to use
    * the correct number of year digits.
    */
   if( old_century_setting != new_century_setting )
   {
      int      count, digit, size, y_size, y_start, y_stop;
      char *   szDateFormat, * szNewFormat;

      /* Convert to upper case and determine where year is */
      y_start        = y_stop = -1;
      szDateFormat   = pSet->HB_SET_DATEFORMAT;
      size           = ( int ) strlen( szDateFormat );
      for( count = 0; count < size; count++ )
      {
         digit = HB_TOUPPER( ( UCHAR ) szDateFormat[ count ] );
         if( digit == 'Y' )
         {
            if( y_start == -1 )
               y_start = count;
         }
         else if( y_start > -1 && y_stop == -1 )
            y_stop = count;
         szDateFormat[ count ] = ( char ) digit;
      }
      /* Determine size of year in current format */
      if( y_start < 0 )
      {
         y_start  = 0; /* There is no year in the current format */
         y_stop   = 0;
      }
      else if( y_stop < 0 )
         y_stop = size;  /* All digits are year digits */
      y_size   = y_stop - y_start;
      /* Calculate size of new format */
      size     -= y_size;
      if( new_century_setting )
         size += 4;
      else
         size += 2;

      /* Create the new date format */
      szNewFormat = ( char * ) hb_xgrab( size + 1 );

      {
         int format_len;
         if( y_start > 0 )
            HB_MEMCPY( szNewFormat, szDateFormat, y_start );
         szNewFormat[ y_start ] = '\0';
         hb_strncat( szNewFormat, "YY", size );
         if( new_century_setting )
            hb_strncat( szNewFormat, "YY", size );
         format_len = ( int ) strlen( szDateFormat );
         if( y_stop < format_len )
            hb_strncat( szNewFormat, szDateFormat + y_stop, size );
         /* DATE FORMAT is under direct control of SET, so notify when it
            it is changed indirectly via __SETCENTURY() */
         hb_setListenerNotify( HB_SET_DATEFORMAT, HB_SET_LISTENER_BEFORE );
         hb_xfree( szDateFormat );
         pSet->HB_SET_DATEFORMAT = szNewFormat;
         hb_setListenerNotify( HB_SET_DATEFORMAT, HB_SET_LISTENER_AFTER );
      }
   }

   /* Return the previous setting */
   return old_century_setting;
}

HB_FUNC( __SETCENTURY )
{
   BOOL     old_century_setting  = hb_setGetCentury();
   PHB_ITEM pNewVal              = hb_param( 1, HB_IT_ANY );

   if( pNewVal )
      hb_setSetCentury( set_logical( pNewVal, old_century_setting ) );

   hb_retl( old_century_setting );
}

HB_FUNC( SETCANCEL )
{
   hb_retl( hb_setGetCancel() );
   /* SETCANCEL() accepts only logical parameters */
   hb_setSetItem( HB_SET_CANCEL, hb_param( 1, HB_IT_LOGICAL ) );
}

HB_FUNC( SET )
{
   HB_THREAD_STUB
   PHB_SET_STRUCT pSet           = hb_stackSetStruct();
   int            args           = hb_pcount();
   BOOL           bFlag;

   HB_set_enum    set_specifier  = ( args > 0 ) ? ( HB_set_enum ) hb_parni( 1 ) : HB_SET_INVALID_;
   PHB_ITEM       pArg2          = ( args > 1 ) ? hb_param( 2, HB_IT_ANY ) : NULL;
   PHB_ITEM       pArg3          = ( args > 2 ) ? hb_param( 3, HB_IT_ANY ) : NULL;

   if( args > 1 )
      hb_setListenerNotify( set_specifier, HB_SET_LISTENER_BEFORE );

   switch( set_specifier )
   {
      case HB_SET_ALTERNATE:
         hb_retl( pSet->HB_SET_ALTERNATE );
         if( args > 1 )
            pSet->HB_SET_ALTERNATE = set_logical( pArg2, pSet->HB_SET_ALTERNATE );
         break;

      case HB_SET_ALTFILE:
         hb_retc( pSet->HB_SET_ALTFILE );
         if( args > 1 )
         {
            if( HB_IS_NIL( pArg2 ) )
            {
               if( pSet->HB_SET_ALTFILE )
               {
                  hb_xfree( pSet->HB_SET_ALTFILE );
                  pSet->HB_SET_ALTFILE = NULL;
               }
            }
            else
               pSet->HB_SET_ALTFILE = set_string( pArg2, pSet->HB_SET_ALTFILE );
         }

         if( args > 2 )
            bFlag = set_logical( pArg3, FALSE );
         else
            bFlag = FALSE;

         if( args > 1 )
         {
            close_text( pSet, pSet->hb_set_althan );
            pSet->hb_set_althan = FS_ERROR;
            if( pSet->HB_SET_ALTFILE && pSet->HB_SET_ALTFILE[ 0 ] != '\0' )
               pSet->hb_set_althan = open_handle( pSet, pSet->HB_SET_ALTFILE, bFlag, ".txt", HB_SET_ALTFILE );
         }
         break;

      case HB_SET_AUTOPEN:
         hb_retl( pSet->HB_SET_AUTOPEN );
         if( args > 1 )
            pSet->HB_SET_AUTOPEN = set_logical( pArg2, pSet->HB_SET_AUTOPEN );
         break;

      case HB_SET_AUTORDER:
         hb_retni( pSet->HB_SET_AUTORDER );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_AUTORDER ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            else
               pSet->HB_SET_AUTORDER = set_number( pArg2, pSet->HB_SET_AUTORDER );
         }
         break;

      case HB_SET_AUTOSHARE:
         hb_retni( pSet->HB_SET_AUTOSHARE );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_AUTOSHARE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            else
               pSet->HB_SET_AUTOSHARE = set_number( pArg2, pSet->HB_SET_AUTOSHARE );
         }
         break;

      case HB_SET_BELL:
         hb_retl( pSet->HB_SET_BELL );
         if( args > 1 )
            pSet->HB_SET_BELL = set_logical( pArg2, pSet->HB_SET_BELL );
         break;

      case HB_SET_CANCEL:
         hb_retl( pSet->HB_SET_CANCEL );
         if( args > 1 )
            pSet->HB_SET_CANCEL = set_logical( pArg2, pSet->HB_SET_CANCEL );
         break;

      case HB_SET_COLOR:
         hb_retc( hb_conSetColor( args >= 2 && HB_IS_STRING( pArg2 ) ? hb_itemGetCPtr( pArg2 ) : NULL ) );
         break;

      case HB_SET_CONFIRM:
         hb_retl( pSet->HB_SET_CONFIRM );
         if( args > 1 )
            pSet->HB_SET_CONFIRM = set_logical( pArg2, pSet->HB_SET_CONFIRM );
         break;

      case HB_SET_CONSOLE:
         hb_retl( pSet->HB_SET_CONSOLE );
         if( args > 1 )
            pSet->HB_SET_CONSOLE = set_logical( pArg2, pSet->HB_SET_CONSOLE );
         break;

      case HB_SET_CURSOR:
         if( args >= 2 && HB_IS_NUMERIC( pArg2 ) )
            hb_retni( hb_conSetCursor( TRUE, ( USHORT ) hb_itemGetNI( pArg2 ) ) );
         else
            hb_retni( hb_conSetCursor( FALSE, 0 ) );
         break;

      case HB_SET_DATEFORMAT:
         hb_retc( pSet->HB_SET_DATEFORMAT );
         if( args > 1 )
         {
            char *   value;
            int      year = 0;

            value = pSet->HB_SET_DATEFORMAT = set_string( pArg2, pSet->HB_SET_DATEFORMAT );
            while( *value )
            {
               if( *value == 'Y' || *value == 'y' )
                  year++;
               else if( year )   /* Only count the first set of consecutive "Y"s. */
                  break;
               ++value;
            }
            /* CENTURY is not controlled directly by SET, so there is no
               notification for changing it indirectly via DATE FORMAT. */
            pSet->hb_set_century = year >= 4;
         }
         break;

      case HB_SET_TIMEFORMAT:
         hb_retc( pSet->HB_SET_TIMEFORMAT );
         if( args > 1 )
            pSet->HB_SET_TIMEFORMAT = set_string( pArg2, pSet->HB_SET_TIMEFORMAT );
         break;

      case HB_SET_DEBUG:
         hb_retl( pSet->HB_SET_DEBUG );
         if( args > 1 )
            pSet->HB_SET_DEBUG = set_logical( pArg2, pSet->HB_SET_DEBUG );
         break;

      case HB_SET_DECIMALS:
         hb_retni( pSet->HB_SET_DECIMALS );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_DECIMALS ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            else
               pSet->HB_SET_DECIMALS = set_number( pArg2, pSet->HB_SET_DECIMALS );
         }
         break;

      case HB_SET_DEFAULT:
         hb_retc( pSet->HB_SET_DEFAULT );
         if( args > 1 )
            pSet->HB_SET_DEFAULT = set_string( pArg2, pSet->HB_SET_DEFAULT );
         break;

      case HB_SET_DELETED:
         hb_retl( pSet->HB_SET_DELETED );
         if( args > 1 )
            pSet->HB_SET_DELETED = set_logical( pArg2, pSet->HB_SET_DELETED );
         break;

      case HB_SET_DELIMCHARS:
         hb_retc( pSet->HB_SET_DELIMCHARS );
         if( args > 1 )
            pSet->HB_SET_DELIMCHARS = set_string( pArg2, pSet->HB_SET_DELIMCHARS );
         break;

      case HB_SET_DELIMITERS:
         hb_retl( pSet->HB_SET_DELIMITERS );
         if( args > 1 )
            pSet->HB_SET_DELIMITERS = set_logical( pArg2, pSet->HB_SET_DELIMITERS );
         break;

      case HB_SET_DEVICE:
         hb_retc( pSet->HB_SET_DEVICE );
         if( args > 1 && ! HB_IS_NIL( pArg2 ) )
         {
            /* If the print file is not already open, open it in overwrite mode. */
            pSet->HB_SET_DEVICE = set_string( pArg2, pSet->HB_SET_DEVICE );
            if( hb_stricmp( pSet->HB_SET_DEVICE, "PRINTER" ) == 0 )
               hb_set_SetPrinterStart();
            else
               hb_set_SetPrinterStop();
         }
         break;

      case HB_SET_EOF:
         hb_retl( pSet->HB_SET_EOF );
         if( args > 1 )
            pSet->HB_SET_EOF = set_logical( pArg2, pSet->HB_SET_EOF );
         break;

      case HB_SET_EPOCH:
         hb_retni( pSet->HB_SET_EPOCH );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_EPOCH ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            else
               pSet->HB_SET_EPOCH = set_number( pArg2, pSet->HB_SET_EPOCH );
         }
         break;

      case HB_SET_ESCAPE:
         hb_retl( pSet->HB_SET_ESCAPE );
         if( args > 1 )
            pSet->HB_SET_ESCAPE = set_logical( pArg2, pSet->HB_SET_ESCAPE );
         break;

      case HB_SET_EVENTMASK:
         hb_retni( pSet->HB_SET_EVENTMASK );
         if( args > 1 )
            pSet->HB_SET_EVENTMASK = set_number( pArg2, pSet->HB_SET_EVENTMASK );
         break;

      case HB_SET_EXACT:
         hb_retl( pSet->HB_SET_EXACT );
         if( args > 1 )
            pSet->HB_SET_EXACT = set_logical( pArg2, pSet->HB_SET_EXACT );
         break;

      case HB_SET_EXCLUSIVE:
         hb_retl( pSet->HB_SET_EXCLUSIVE );
         if( args > 1 )
            pSet->HB_SET_EXCLUSIVE = set_logical( pArg2, pSet->HB_SET_EXCLUSIVE );
         break;

      case HB_SET_EXIT:
         hb_retl( pSet->HB_SET_EXIT );
         if( args > 1 )
            pSet->HB_SET_EXIT = set_logical( pArg2, pSet->HB_SET_EXIT );
         break;

      case HB_SET_EXTRA:
         hb_retl( pSet->HB_SET_EXTRA );
         if( args > 1 )
            pSet->HB_SET_EXTRA = set_logical( pArg2, pSet->HB_SET_EXTRA );
         break;

      case HB_SET_EXTRAFILE:
         hb_retc( pSet->HB_SET_EXTRAFILE );
         if( args > 1 )
         {
            if( HB_IS_NIL( pArg2 ) )
            {
               if( pSet->HB_SET_EXTRAFILE )
               {
                  hb_xfree( pSet->HB_SET_EXTRAFILE );
                  pSet->HB_SET_EXTRAFILE = NULL;
               }
            }
            else
               pSet->HB_SET_EXTRAFILE = set_string( pArg2, pSet->HB_SET_EXTRAFILE );
         }

         if( args > 2 )
            bFlag = set_logical( pArg3, FALSE );
         else
            bFlag = FALSE;

         if( args > 1 && ! HB_IS_NIL( pArg2 ) )
         {
            close_text( pSet, pSet->hb_set_extrahan );
            pSet->hb_set_extrahan = FS_ERROR;
            if( pSet->HB_SET_EXTRAFILE && pSet->HB_SET_EXTRAFILE[ 0 ] != '\0' )
               pSet->hb_set_extrahan = open_handle( pSet, pSet->HB_SET_EXTRAFILE, bFlag, ".prn", HB_SET_EXTRAFILE );
         }
         break;

      case HB_SET_FIXED:
         hb_retl( pSet->HB_SET_FIXED );
         if( args > 1 )
            pSet->HB_SET_FIXED = set_logical( pArg2, pSet->HB_SET_FIXED );
         break;

      case HB_SET_INSERT:
         hb_retl( pSet->HB_SET_INSERT );
         if( args > 1 )
            pSet->HB_SET_INSERT = set_logical( pArg2, pSet->HB_SET_INSERT );
         break;

      case HB_SET_INTENSITY:
         hb_retl( pSet->HB_SET_INTENSITY );
         if( args > 1 )
            pSet->HB_SET_INTENSITY = set_logical( pArg2, pSet->HB_SET_INTENSITY );
         break;

      case HB_SET_MARGIN:
         hb_retni( pSet->HB_SET_MARGIN );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_MARGIN ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            else
               pSet->HB_SET_MARGIN = set_number( pArg2, pSet->HB_SET_MARGIN );
         }
         break;

      case HB_SET_MBLOCKSIZE:
         hb_retni( pSet->HB_SET_MBLOCKSIZE );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_MBLOCKSIZE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            else
               pSet->HB_SET_MBLOCKSIZE = set_number( pArg2, pSet->HB_SET_MBLOCKSIZE );
         }
         break;

      case HB_SET_MCENTER:
         hb_retl( pSet->HB_SET_MCENTER );
         if( args > 1 )
            pSet->HB_SET_MCENTER = set_logical( pArg2, pSet->HB_SET_MCENTER );
         break;

      case HB_SET_MESSAGE:
         hb_retni( pSet->HB_SET_MESSAGE );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_MESSAGE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            else
               pSet->HB_SET_MESSAGE = set_number( pArg2, pSet->HB_SET_MESSAGE );
         }
         break;

      case HB_SET_MFILEEXT:
         hb_retc( pSet->HB_SET_MFILEEXT );
         if( args > 1 )
            pSet->HB_SET_MFILEEXT = set_string( pArg2, pSet->HB_SET_MFILEEXT );
         break;

      case HB_SET_OPTIMIZE:
         hb_retl( pSet->HB_SET_OPTIMIZE );
         if( args > 1 )
            pSet->HB_SET_OPTIMIZE = set_logical( pArg2, pSet->HB_SET_OPTIMIZE );
         break;

      case HB_SET_FORCEOPT:
         hb_retl( pSet->HB_SET_FORCEOPT );
         if( args > 1 )
            pSet->HB_SET_FORCEOPT = set_logical( pArg2, pSet->HB_SET_FORCEOPT );
         break;

      case HB_SET_STRICTREAD:
         hb_retl( pSet->HB_SET_STRICTREAD );
         if( args > 1 )
            pSet->HB_SET_STRICTREAD = set_logical( pArg2, pSet->HB_SET_STRICTREAD );
         break;

      case HB_SET_HARDCOMMIT:
         hb_retl( pSet->HB_SET_HARDCOMMIT );
         if( args > 1 )
            pSet->HB_SET_HARDCOMMIT = set_logical( pArg2, pSet->HB_SET_HARDCOMMIT );
         break;

      case HB_SET_PATH:
         hb_retc( pSet->HB_SET_PATH );
         if( args > 1 )
         {
            pSet->HB_SET_PATH = set_string( pArg2, pSet->HB_SET_PATH );
            hb_fsFreeSearchPath( pSet->hb_set_path );
            pSet->hb_set_path = NULL;
            hb_fsAddSearchPath( pSet->HB_SET_PATH, &pSet->hb_set_path );
         }
         break;

      case HB_SET_PRINTER:
         hb_retl( pSet->HB_SET_PRINTER );
         if( args > 1 )
         {
            pSet->HB_SET_PRINTER = set_logical( pArg2, pSet->HB_SET_PRINTER );

            if( pSet->HB_SET_PRINTER )
               hb_set_SetPrinterStart();
            else
               hb_set_SetPrinterStop();
         }
         break;

      case HB_SET_PRINTFILE:
         hb_retc( pSet->HB_SET_PRINTFILE );
         if( args > 2 )
            bFlag = set_logical( pArg3, FALSE );
         else
            bFlag = FALSE;
         if( args > 1 && ! HB_IS_NIL( pArg2 ) )
         {
            pSet->HB_SET_PRINTFILE  = set_string( pArg2, pSet->HB_SET_PRINTFILE );

            close_binary( pSet, pSet->hb_set_printhan );
            pSet->hb_set_printhan   = FS_ERROR;
            if( pSet->HB_SET_PRINTFILE && pSet->HB_SET_PRINTFILE[ 0 ] != '\0' )
            {
               pSet->hb_set_printhan      = open_handle( pSet, pSet->HB_SET_PRINTFILE, bFlag, ".prn", HB_SET_PRINTFILE );
               s_isDefaultPrinterDevice   = FALSE;
            }
            else
               hb_set_SetDefaultPrinter( pSet );  /* Make sure there is a default print file name "PRN" */
         }
         break;

      case HB_SET_SCOREBOARD:
         hb_retl( pSet->HB_SET_SCOREBOARD );
         if( args > 1 )
            pSet->HB_SET_SCOREBOARD = set_logical( pArg2, pSet->HB_SET_SCOREBOARD );
         break;

      case HB_SET_SCROLLBREAK:
         hb_retl( pSet->HB_SET_SCROLLBREAK );
         if( args > 1 )
            pSet->HB_SET_SCROLLBREAK = set_logical( pArg2, pSet->HB_SET_SCROLLBREAK );
         break;

      case HB_SET_SOFTSEEK:
         hb_retl( pSet->HB_SET_SOFTSEEK );
         if( args > 1 )
            pSet->HB_SET_SOFTSEEK = set_logical( pArg2, pSet->HB_SET_SOFTSEEK );
         break;

      case HB_SET_TYPEAHEAD:
         hb_retni( pSet->HB_SET_TYPEAHEAD );
         if( args > 1 )
         {
            /* Set the value and limit the range */
            pSet->HB_SET_TYPEAHEAD = set_number( pArg2, pSet->HB_SET_TYPEAHEAD );
            if( pSet->HB_SET_TYPEAHEAD == 0 )
               /* Do nothing */;
            else if( pSet->HB_SET_TYPEAHEAD < 16 )
               pSet->HB_SET_TYPEAHEAD = 16;
            else if( pSet->HB_SET_TYPEAHEAD > 4096 )
               pSet->HB_SET_TYPEAHEAD = 4096;
            /* reset keyboard buffer */
            hb_inkeyReset();
         }
         break;

      case HB_SET_UNIQUE:
         hb_retl( pSet->HB_SET_UNIQUE );
         if( args > 1 )
            pSet->HB_SET_UNIQUE = set_logical( pArg2, pSet->HB_SET_UNIQUE );
         break;

      case HB_SET_VIDEOMODE:
         hb_retni( pSet->HB_SET_VIDEOMODE );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_VIDEOMODE ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            else
               pSet->HB_SET_VIDEOMODE = set_number( pArg2, pSet->HB_SET_VIDEOMODE );
         }
         break;

      case HB_SET_WRAP:
         hb_retl( pSet->HB_SET_WRAP );
         if( args > 1 )
            pSet->HB_SET_WRAP = set_logical( pArg2, pSet->HB_SET_WRAP );
         break;

      case HB_SET_LANGUAGE:
         hb_retc( hb_langID() );
         if( args > 1 && HB_IS_STRING( pArg2 ) )
            hb_langSelectID( hb_itemGetCPtr( pArg2 ) );
         break;

      case HB_SET_CODEPAGE:
         hb_retc( hb_cdpID() );
         if( args > 1 && HB_IS_STRING( pArg2 ) )
            hb_cdpSelectID( hb_itemGetCPtr( pArg2 ) );
         break;

      case HB_SET_IDLEREPEAT:
         hb_retl( pSet->HB_SET_IDLEREPEAT );
         if( args > 1 )
            pSet->HB_SET_IDLEREPEAT = set_logical( pArg2, pSet->HB_SET_IDLEREPEAT );
         break;

      case HB_SET_TRACE:
         hb_retl( pSet->HB_SET_TRACE );
         if( args > 1 )
            pSet->HB_SET_TRACE = set_logical( pArg2, pSet->HB_SET_TRACE );
         break;

      case HB_SET_TRACEFILE:
         hb_retc( ( char * ) ( pSet->HB_SET_TRACEFILE ) );

         if( args > 1 && HB_IS_STRING( pArg2 ) )
         {
            FILE *   fpTrace;
            BOOL     bAppend = FALSE;

            hb_xstrcpy( pSet->HB_SET_TRACEFILE, pArg2->item.asString.value, 0 );

            /* Create trace.log for tracing. */
            if( args > 2 && HB_IS_LOGICAL( pArg3 ) )
               bAppend = pArg3->item.asLogical.value;

            if( bAppend )
               fpTrace = hb_fopen( ( char * ) ( pSet->HB_SET_TRACEFILE ), "a" );
            else
               fpTrace = hb_fopen( ( char * ) ( pSet->HB_SET_TRACEFILE ), "w" );

            if( fpTrace )
               fclose( fpTrace );
            else
            {
               /* hb_errInternal( HB_EI_ERRUNRECOV, "Unable to create trace.log file", NULL, NULL ); */
            }
         }
         break;

      case HB_SET_TRACESTACK:
         hb_retni( pSet->HB_SET_TRACESTACK );
         if( args > 1 )
         {
            if( HB_IS_STRING( pArg2 ) )
            {
               if( ! hb_stricmp( pArg2->item.asString.value, "NONE" ) )
                  pSet->HB_SET_TRACESTACK = HB_SET_TRACESTACK_NONE;
               else if( ! hb_stricmp( pArg2->item.asString.value, "CURRENT" ) )
                  pSet->HB_SET_TRACESTACK = HB_SET_TRACESTACK_CURRENT;
               else if( ! hb_stricmp( pArg2->item.asString.value, "ALL" ) )
                  pSet->HB_SET_TRACESTACK = HB_SET_TRACESTACK_ALL;
               else
                  hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            }
            else if( HB_IS_NUMERIC( pArg2 ) )
            {
               if( set_number( pArg2, pSet->HB_SET_TRACESTACK ) < 0 )
                  hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
               else
                  pSet->HB_SET_TRACESTACK = ( char ) set_number( pArg2, pSet->HB_SET_TRACESTACK );
            }
            else
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
         }
         break;

      case HB_SET_PRINTERJOB:
         hb_retc( pSet->hb_set_printerjob );
         if( args > 1 && ISCHAR( 2 ) )
         {
            HB_SIZE ulLength = hb_parclen( 2 );
            if( pSet->hb_set_printerjob )
            {
               hb_xfree( pSet->hb_set_printerjob );
               pSet->hb_set_printerjob = NULL;
            }

            if( ulLength > 0 )
               pSet->hb_set_printerjob = hb_strdup( hb_parc( 2 ) );
         }
         break;

      case HB_SET_FILECASE:
         hb_retni( pSet->HB_SET_FILECASE );
         if( args > 1 )
         {
            if( HB_IS_STRING( pArg2 ) )
            {
               if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "LOWER" ) )
                  pSet->HB_SET_FILECASE = HB_SET_CASE_LOWER;
               else if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "UPPER" ) )
                  pSet->HB_SET_FILECASE = HB_SET_CASE_UPPER;
               else if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "MIXED" ) )
                  pSet->HB_SET_FILECASE = HB_SET_CASE_MIXED;
               else
                  hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            }
            else if( HB_IS_NUMERIC( pArg2 ) )
            {
               if( set_number( pArg2, pSet->HB_SET_FILECASE ) < 0 )
                  hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
               else
                  pSet->HB_SET_FILECASE = set_number( pArg2, pSet->HB_SET_FILECASE );
            }
            else
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
         }
         break;

      case HB_SET_DIRCASE:
         hb_retni( pSet->HB_SET_DIRCASE );
         if( args > 1 )
         {
            if( HB_IS_STRING( pArg2 ) )
            {
               if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "LOWER" ) )
                  pSet->HB_SET_DIRCASE = HB_SET_CASE_LOWER;
               else if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "UPPER" ) )
                  pSet->HB_SET_DIRCASE = HB_SET_CASE_UPPER;
               else if( ! hb_stricmp( hb_itemGetCPtr( pArg2 ), "MIXED" ) )
                  pSet->HB_SET_DIRCASE = HB_SET_CASE_MIXED;
               else
                  hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            }
            else if( HB_IS_NUMERIC( pArg2 ) )
            {
               if( set_number( pArg2, pSet->HB_SET_DIRCASE ) < 0 )
                  hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );

               else
                  pSet->HB_SET_DIRCASE = set_number( pArg2, pSet->HB_SET_DIRCASE );
            }
            else
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
         }
         break;

      case HB_SET_DIRSEPARATOR:
      {
         char szDirSep[ 2 ];
         szDirSep[ 0 ]  = ( char ) pSet->HB_SET_DIRSEPARATOR;
         szDirSep[ 1 ]  = '\0';
         hb_retc( szDirSep );
         if( args > 1 )
            pSet->HB_SET_DIRSEPARATOR = set_char( pArg2, ( char ) pSet->HB_SET_DIRSEPARATOR );
         break;
      }

      case HB_SET_ERRORLOOP:
         hb_retni( pSet->HB_SET_ERRORLOOP );
         if( args > 1 )
         {
            if( HB_IS_NUMERIC( pArg2 ) )
            {
               pSet->HB_SET_ERRORLOOP = hb_itemGetNI( pArg2 );

               if( pSet->HB_SET_ERRORLOOP < 0 )
                  hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
               else if( pSet->HB_SET_ERRORLOOP == 0 )
                  pSet->HB_SET_ERRORLOOP = 8;
            }
            else
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
         }
         break;

      case HB_SET_OUTPUTSAFETY:
         hb_retl( pSet->HB_SET_OUTPUTSAFETY );
         if( args > 1 )
            pSet->HB_SET_OUTPUTSAFETY = set_logical( pArg2, TRUE );
         break;

      case HB_SET_DBFLOCKSCHEME:
         hb_retni( pSet->HB_SET_DBFLOCKSCHEME );
         if( args > 1 )
         {
            if( set_number( pArg2, pSet->HB_SET_DBFLOCKSCHEME ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            else
               pSet->HB_SET_DBFLOCKSCHEME = set_number( pArg2, pSet->HB_SET_DBFLOCKSCHEME );
         }
         break;

      case HB_SET_DEFEXTENSIONS:
         hb_retl( pSet->HB_SET_DEFEXTENSIONS );
         if( args > 1 )
            pSet->HB_SET_DEFEXTENSIONS = set_logical( pArg2, pSet->HB_SET_DEFEXTENSIONS );
         break;

      case HB_SET_EOL:
         hb_retc( pSet->HB_SET_EOL );
         if( args > 1 )
            pSet->HB_SET_EOL = set_string( pArg2, pSet->HB_SET_EOL );
         break;

      case HB_SET_BACKGROUNDTASKS:
         hb_retl( pSet->HB_SET_BACKGROUNDTASKS );
         if( args > 1 )
            pSet->HB_SET_BACKGROUNDTASKS = set_logical( pArg2, pSet->HB_SET_BACKGROUNDTASKS );
         break;

      case HB_SET_TRIMFILENAME:
         hb_retl( pSet->HB_SET_TRIMFILENAME );
         if( args > 1 )
            pSet->HB_SET_TRIMFILENAME = set_logical( pArg2, pSet->HB_SET_TRIMFILENAME );
         break;

      case HB_SET_OSCODEPAGE:
         hb_retc( pSet->HB_SET_OSCODEPAGE );
         if( args > 1 )
         {
            pSet->HB_SET_OSCODEPAGE = set_string( pArg2, pSet->HB_SET_OSCODEPAGE );
            hb_set_OSCODEPAGE( pSet );
         }
         break;

      case HB_SET_BACKGROUNDTICK:
         hb_retnl( pSet->HB_SET_BACKGROUNDTICK );
         if( args > 1 )
         {
            int iNewVal = set_number( pArg2, pSet->HB_SET_BACKGROUNDTICK );

            if( iNewVal < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            else
               pSet->HB_SET_BACKGROUNDTICK = iNewVal == 0 ? 1000 : iNewVal;
         }
         break;

      case HB_SET_ERRORLOG:
         hb_reta( 2 );
         hb_storc( ( char * ) ( pSet->HB_SET_ERRORLOG ), -1, 1 );
         hb_storl( pSet->HB_SET_APPENDERROR, -1, 2 );
/*
 *       if( args > 1 && HB_IS_STRING( pArg2 ) )
 *       {
 *          strcpy( pSet->HB_SET_ERRORLOG, pArg2->item.asString.value );
 *
 *          if( args > 2 && HB_IS_LOGICAL( pArg3 ) )
 *          {
 *             pSet->HB_SET_APPENDERROR = pArg3->item.asLogical.value;
 *          }
 *       }
 */
         if( args > 1 )
         {
            if( HB_IS_STRING( pArg2 ) )
               hb_xstrcpy( pSet->HB_SET_ERRORLOG, pArg2->item.asString.value, 0 );

            if( pArg3 && HB_IS_LOGICAL( pArg3 ) )
               pSet->HB_SET_APPENDERROR = pArg3->item.asLogical.value;
         }
         break;

      case HB_SET_MACROBLOCKVARS:
         if( pArg3 && HB_IS_BLOCK( pArg3 ) )
         {
            if( ! ( pArg3->item.asBlock.value->uiFlags & CBF_DYNAMIC ) )
            {
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               break;
            }

            hb_retl( ( pArg3->item.asBlock.value->uiFlags & CBF_PRIVATE_VARS ) );

            if( set_logical( pArg2, ( pArg3->item.asBlock.value->uiFlags & CBF_PRIVATE_VARS ) ) )
               pArg3->item.asBlock.value->uiFlags |= CBF_PRIVATE_VARS;
         }
         else
         {
            hb_retl( pSet->HB_SET_MACROBLOCKVARS );

            if( args > 1 )
               pSet->HB_SET_MACROBLOCKVARS = set_logical( pArg2, pSet->HB_SET_MACROBLOCKVARS );
         }
         break;

      case HB_SET_WORKAREAS_SHARED:
         hb_retl( pSet->HB_SET_WORKAREAS_SHARED );
#ifdef HB_THREAD_SUPPORT
         if( args > 1 )
         {
            BOOL bNewVal = set_logical( pArg2, pSet->HB_SET_WORKAREAS_SHARED );

            if( hb_rddChangeSetWorkareasShared( pSet->HB_SET_WORKAREAS_SHARED, bNewVal ) )
               pSet->HB_SET_WORKAREAS_SHARED = bNewVal;
            else
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
         }
#endif
         break;

      case HB_SET_INVALID_:
         /* Return NIL if called with invalid SET specifier */
         break;

#if 0
      /*
       * intentionally removed default: clause to enable C compiler warning
       * when not all HB_SET_* cases are implemented. [druzus]
       */
      default:
         break;
#endif
   }

   if( args > 1 )
      hb_setListenerNotify( set_specifier, HB_SET_LISTENER_AFTER );
}

/* Listener test (1 of 2)
 * static void test_callback( HB_set_enum set, HB_set_listener_enum when )
 * {
 * printf("\ntest_callback( %d, %d )", set, when);
 * }
 * End listener test (1 of 2)
 */

void hb_setInitialize( PHB_SET_STRUCT pSet )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_setInitialize(%p)", pSet ) );

   pSet->HB_SET_ALTERNATE     = FALSE;
   pSet->HB_SET_ALTFILE       = NULL;
   pSet->hb_set_althan        = FS_ERROR;
   pSet->HB_SET_AUTOPEN       = TRUE;
   pSet->HB_SET_AUTORDER      = 0;
   pSet->HB_SET_AUTOSHARE     = 0;
   pSet->HB_SET_BELL          = FALSE;
   pSet->HB_SET_CANCEL        = TRUE;
   pSet->hb_set_century       = FALSE;
   pSet->HB_SET_COLOR         = ( char * ) hb_xgrab( HB_CLRSTR_LEN + 1 );
   hb_strncpy( pSet->HB_SET_COLOR, "W/N,N/W,N/N,N/N,N/W", HB_CLRSTR_LEN );
   pSet->HB_SET_CONFIRM       = FALSE;
   pSet->HB_SET_CONSOLE       = TRUE;
   pSet->HB_SET_DATEFORMAT    = hb_strdup( "mm/dd/yy" );
   pSet->HB_SET_TIMEFORMAT    = hb_strdup( "hh:mm:ss.ccc" );
   /*
    * Tests shows that Clipper has two different flags to control ALT+D
    * and ALTD() behavior and on startup these flags are not synchronized.
    * When application starts _SET_DEBUG is set to FALSE but debugger
    * can be activated by hitting K_ALT_D or calling ALTD() function without
    * parameter. It means that some other internal flag enables these
    * operations.
    * Because Harbour is using _SET_DEBUG flag only then we have to
    * initialize it to TRUE when debugger is linked to keep real Clipper
    * behavior or we will have to add second flag too and try to replicate
    * exactly unsynchronized behavior of these flags which exists in Clipper.
    * IMHO it's a bug in Clipper (side effect of some internal solutions) and
    * we should not try to emulate it [druzus].
    */
   /* pSet->HB_SET_DEBUG = FALSE; */
   pSet->HB_SET_DEBUG         = hb_dynsymFind( "__DBGENTRY" ) ? TRUE : FALSE;
   pSet->HB_SET_DECIMALS      = 2;
   pSet->HB_SET_DEFAULT       = hb_strdup( "" );
   pSet->HB_SET_DELETED       = FALSE;
   pSet->HB_SET_DELIMCHARS    = hb_strdup( "::" );
   pSet->HB_SET_DELIMITERS    = FALSE;
   pSet->HB_SET_DEVICE        = hb_strdup( "SCREEN" );
#if defined( HB_OS_UNIX )
   pSet->HB_SET_EOF           = FALSE;
#else
   pSet->HB_SET_EOF           = TRUE;
#endif
   pSet->HB_SET_EPOCH         = 1900;
   pSet->HB_SET_ESCAPE        = TRUE;
   pSet->HB_SET_EVENTMASK     = INKEY_KEYBOARD;
   pSet->HB_SET_EXACT         = FALSE;
   pSet->HB_SET_EXCLUSIVE     = TRUE;
   pSet->HB_SET_EXIT          = FALSE;
   pSet->HB_SET_EXTRA         = FALSE;
   pSet->HB_SET_EXTRAFILE     = NULL;
   pSet->hb_set_extrahan      = FS_ERROR;
   pSet->HB_SET_FIXED         = FALSE;
   pSet->HB_SET_FORCEOPT      = FALSE;
   pSet->HB_SET_HARDCOMMIT    = TRUE;
   pSet->HB_SET_IDLEREPEAT    = TRUE;
   pSet->HB_SET_INSERT        = FALSE;
   pSet->HB_SET_INTENSITY     = TRUE;
   pSet->HB_SET_MARGIN        = 0;
   pSet->HB_SET_MBLOCKSIZE    = 0;
   pSet->HB_SET_MCENTER       = FALSE;
   pSet->HB_SET_MESSAGE       = 0;
   pSet->HB_SET_MFILEEXT      = hb_strdup( "" );
   pSet->HB_SET_OPTIMIZE      = TRUE;
   pSet->HB_SET_PATH          = hb_strdup( "" );
   pSet->hb_set_path          = NULL;
   pSet->HB_SET_PRINTER       = FALSE;

   pSet->HB_SET_PRINTFILE     = NULL;
   hb_set_SetDefaultPrinter( pSet );

   pSet->hb_set_printhan      = FS_ERROR;
   pSet->hb_set_winhan        = FS_ERROR;
   pSet->HB_SET_SCOREBOARD    = TRUE;
   pSet->HB_SET_SCROLLBREAK   = TRUE;
   pSet->HB_SET_SOFTSEEK      = FALSE;
   pSet->HB_SET_STRICTREAD    = FALSE;
   pSet->HB_SET_TYPEAHEAD     = HB_DEFAULT_INKEY_BUFSIZE;
   pSet->HB_SET_UNIQUE        = FALSE;
   pSet->HB_SET_FILECASE      = HB_SET_CASE_MIXED;
   pSet->HB_SET_DIRCASE       = HB_SET_CASE_MIXED;
   pSet->HB_SET_DIRSEPARATOR  = HB_OS_PATH_DELIM_CHR;
   pSet->HB_SET_VIDEOMODE     = 0;
   pSet->HB_SET_WRAP          = FALSE;
   pSet->HB_SET_DBFLOCKSCHEME = 0;
   pSet->HB_SET_DEFEXTENSIONS = TRUE;
   pSet->HB_SET_EOL           = hb_strdup( hb_conNewLine() );
   pSet->HB_SET_TRIMFILENAME  = FALSE;
   pSet->HB_SET_OSCODEPAGE    = hb_strdup( "" );
   hb_set_OSCODEPAGE( pSet );

   pSet->hb_set_listener      = NULL;

   pSet->HB_SET_TRACE         = TRUE; /* Default Trace to ON */

   hb_xstrcpy( ( char * ) ( pSet->HB_SET_TRACEFILE ), "trace.log", 0 );

   hb_xstrcpy( ( char * ) ( pSet->HB_SET_ERRORLOG ), "error.log", 0 );
   pSet->HB_SET_APPENDERROR      = FALSE;

   pSet->HB_SET_TRACESTACK       = HB_SET_TRACESTACK_ALL;

   pSet->HB_SET_ERRORLOOP        = 8;

/* JC1: Set for output thread safety */
   pSet->HB_SET_OUTPUTSAFETY     = TRUE;

   pSet->HB_SET_BACKGROUNDTASKS  = FALSE;
   pSet->HB_SET_BACKGROUNDTICK   = 1000;

   pSet->hb_set_winprinter       = FALSE;
   pSet->hb_set_printerjob       = NULL;
   pSet->hb_set_winprinter       = FALSE;

   pSet->HB_SET_MACROBLOCKVARS   = FALSE;
   pSet->HB_SET_WORKAREAS_SHARED = TRUE;
}

void hb_setRelease( PHB_SET_STRUCT pSet )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_setRelease(%p)", pSet ) );

   close_text( pSet, pSet->hb_set_althan );
   close_text( pSet, pSet->hb_set_extrahan );
   close_binary( pSet, pSet->hb_set_printhan );

   if( pSet->HB_SET_ALTFILE )
   {
      hb_xfree( pSet->HB_SET_ALTFILE );
      pSet->HB_SET_ALTFILE = NULL;
   }

   if( pSet->HB_SET_DATEFORMAT )
   {
      hb_xfree( pSet->HB_SET_DATEFORMAT );
      pSet->HB_SET_DATEFORMAT = NULL;
   }

   if( pSet->HB_SET_TIMEFORMAT )
   {
      hb_xfree( pSet->HB_SET_TIMEFORMAT );
      pSet->HB_SET_TIMEFORMAT = NULL;
   }

   if( pSet->HB_SET_DEFAULT )
   {
      hb_xfree( pSet->HB_SET_DEFAULT );
      pSet->HB_SET_DEFAULT = NULL;
   }

   if( pSet->HB_SET_DELIMCHARS )
   {
      hb_xfree( pSet->HB_SET_DELIMCHARS );
      pSet->HB_SET_DELIMCHARS = NULL;
   }

   if( pSet->HB_SET_DEVICE )
   {
      hb_xfree( pSet->HB_SET_DEVICE );
      pSet->HB_SET_DEVICE = NULL;
   }

   if( pSet->HB_SET_EXTRAFILE )
   {
      hb_xfree( pSet->HB_SET_EXTRAFILE );
      pSet->HB_SET_EXTRAFILE = NULL;
   }

   if( pSet->HB_SET_MFILEEXT )
   {
      hb_xfree( pSet->HB_SET_MFILEEXT );
      pSet->HB_SET_MFILEEXT = NULL;
   }

   if( pSet->HB_SET_PATH )
   {
      hb_xfree( pSet->HB_SET_PATH );
      pSet->HB_SET_PATH = NULL;
   }

   if( pSet->HB_SET_PRINTFILE )
   {
      hb_xfree( pSet->HB_SET_PRINTFILE );
      pSet->HB_SET_PRINTFILE = NULL;
   }

   if( pSet->HB_SET_COLOR )
   {
      hb_xfree( pSet->HB_SET_COLOR );
      pSet->HB_SET_COLOR = NULL;
   }

   if( pSet->HB_SET_EOL )
   {
      hb_xfree( pSet->HB_SET_EOL );
      pSet->HB_SET_EOL = NULL;
   }

   if( pSet->HB_SET_OSCODEPAGE )
   {
      hb_xfree( pSet->HB_SET_OSCODEPAGE );
      pSet->HB_SET_OSCODEPAGE = NULL;
   }

   if( pSet->hb_set_printerjob )
   {
      hb_xfree( pSet->hb_set_printerjob );
      pSet->hb_set_printerjob = NULL;
   }

   pSet->HB_SET_TYPEAHEAD = 0;
   hb_inkeyReset(); /* reset keyboard buffer */

   hb_fsFreeSearchPath( pSet->hb_set_path );

   /* Free all set listeners */
   if( pSet->hb_set_listener )
   {
      PHB_SET_LISTENER pListener = ( ( PHB_SET_LISTENER_LST )
                                     pSet->hb_set_listener )->first;
      while( pListener )
      {
         PHB_SET_LISTENER pNext = pListener->next;
         hb_xfree( pListener );
         pListener = pNext;
      }
      hb_xfree( pSet->hb_set_listener );
   }
}

PHB_SET_STRUCT hb_setClone( PHB_SET_STRUCT pSrc )
{
   PHB_SET_STRUCT pSet;

   HB_TRACE( HB_TR_DEBUG, ( "hb_setClone(%p)", pSrc ) );

   pSet = ( PHB_SET_STRUCT ) hb_xgrab( sizeof( HB_SET_STRUCT ) );

   HB_MEMCPY( pSet, pSrc, sizeof( HB_SET_STRUCT ) );

   pSet->hb_set_althan     = pSet->hb_set_extrahan = pSet->hb_set_printhan = FS_ERROR;

   pSet->hb_set_listener   = NULL;

   pSet->HB_SET_TYPEAHEAD  = HB_DEFAULT_INKEY_BUFSIZE;

   pSet->HB_SET_COLOR      = ( char * ) hb_xgrab( HB_CLRSTR_LEN + 1 );
   hb_strncpy( pSet->HB_SET_COLOR, pSrc->HB_SET_COLOR, HB_CLRSTR_LEN );

   if( pSet->HB_SET_ALTFILE )
      pSet->HB_SET_ALTFILE = hb_strdup( pSet->HB_SET_ALTFILE );

   if( pSet->HB_SET_DATEFORMAT )
      pSet->HB_SET_DATEFORMAT = hb_strdup( pSet->HB_SET_DATEFORMAT );

   if( pSet->HB_SET_TIMEFORMAT )
      pSet->HB_SET_TIMEFORMAT = hb_strdup( pSet->HB_SET_TIMEFORMAT );

   if( pSet->HB_SET_DEFAULT )
      pSet->HB_SET_DEFAULT = hb_strdup( pSet->HB_SET_DEFAULT );

   if( pSet->HB_SET_DELIMCHARS )
      pSet->HB_SET_DELIMCHARS = hb_strdup( pSet->HB_SET_DELIMCHARS );

   if( pSet->HB_SET_DEVICE )
      pSet->HB_SET_DEVICE = hb_strdup( pSet->HB_SET_DEVICE );

   if( pSet->HB_SET_EXTRAFILE )
      pSet->HB_SET_EXTRAFILE = hb_strdup( pSet->HB_SET_EXTRAFILE );

   if( pSet->HB_SET_MFILEEXT )
      pSet->HB_SET_MFILEEXT = hb_strdup( pSet->HB_SET_MFILEEXT );

   if( pSet->HB_SET_PATH )
      pSet->HB_SET_PATH = hb_strdup( pSet->HB_SET_PATH );

   if( pSet->HB_SET_PRINTFILE )
      pSet->HB_SET_PRINTFILE = hb_strdup( pSet->HB_SET_PRINTFILE );

   if( pSet->HB_SET_EOL )
      pSet->HB_SET_EOL = hb_strdup( pSet->HB_SET_EOL );

   if( pSet->HB_SET_OSCODEPAGE )
      pSet->HB_SET_OSCODEPAGE = hb_strdup( pSet->HB_SET_OSCODEPAGE );

   if( pSet->hb_set_printerjob )
      pSet->hb_set_printerjob = hb_strdup( pSet->hb_set_printerjob );

   return pSet;
}

int hb_setListenerAdd( HB_SET_LISTENER_CALLBACK * callback )
{
   HB_THREAD_STUB
   PHB_SET_STRUCT       pSet  = hb_stackSetStruct();
   PHB_SET_LISTENER     p_sl  = ( PHB_SET_LISTENER ) hb_xgrab( sizeof( HB_SET_LISTENER ) );
   PHB_SET_LISTENER_LST pList;

   if( ! pSet->hb_set_listener )
   {
      pSet->hb_set_listener = hb_xgrab( sizeof( HB_SET_LISTENER_LST ) );
      memset( pSet->hb_set_listener, 0, sizeof( HB_SET_LISTENER_LST ) );
   }
   pList          = ( PHB_SET_LISTENER_LST ) pSet->hb_set_listener;

   p_sl->callback = callback;
   p_sl->listener = ++pList->counter;
   p_sl->next     = NULL;

   if( pList->last )
      pList->last->next = p_sl;
   else if( ! pList->first )
      pList->first = p_sl;

   pList->last = p_sl;

   return p_sl->listener;
}

void hb_setListenerNotify( HB_set_enum set, HB_set_listener_enum when )
{
   HB_THREAD_STUB
   PHB_SET_LISTENER_LST pList = ( PHB_SET_LISTENER_LST ) hb_stackSetStruct()->hb_set_listener;

   if( pList )
   {
      PHB_SET_LISTENER p_sl = pList->first;
      while( p_sl )
      {
         ( *p_sl->callback )( set, when );
         p_sl = p_sl->next;
      }
   }
}

int hb_setListenerRemove( int listener )
{
   HB_THREAD_STUB
   PHB_SET_LISTENER_LST pList = ( PHB_SET_LISTENER_LST ) hb_stackSetStruct()->hb_set_listener;

   if( pList )
   {
      PHB_SET_LISTENER  p_sl        = pList->first;
      PHB_SET_LISTENER  p_sl_prev   = NULL;
      while( p_sl )
      {
         if( listener == p_sl->listener )
         {
            listener = -listener;
            if( p_sl_prev )
               p_sl_prev->next = p_sl->next;
            else
               pList->first = p_sl->next;
            if( p_sl == pList->last )
               pList->last = p_sl_prev;
            hb_xfree( p_sl );
            break;
         }
         p_sl_prev   = p_sl;
         p_sl        = p_sl->next;
      }
   }
   return listener;
}

static BOOL hb_setSetFile( PHB_SET_STRUCT pSet, HB_set_enum set_specifier, const char * szFile, BOOL fAdditive )
{
   BOOL fResult = TRUE;

   switch( set_specifier )
   {
      case HB_SET_ALTFILE:
         if( pSet->HB_SET_ALTFILE )
            hb_xfree( pSet->HB_SET_ALTFILE );
         /* Limit size of SET strings to 64K, truncating if source is longer */
         pSet->HB_SET_ALTFILE = szFile ? hb_strndup( szFile, USHRT_MAX ) : NULL;
         close_text( pSet, pSet->hb_set_althan );
         pSet->hb_set_althan  = FS_ERROR;
         if( pSet->HB_SET_ALTFILE && pSet->HB_SET_ALTFILE[ 0 ] != '\0' )
            pSet->hb_set_althan = open_handle( pSet, pSet->HB_SET_ALTFILE,
                                               fAdditive, ".txt", HB_SET_ALTFILE );
         break;

      case HB_SET_EXTRAFILE:
         if( pSet->HB_SET_EXTRAFILE )
            hb_xfree( pSet->HB_SET_EXTRAFILE );
         /* Limit size of SET strings to 64K, truncating if source is longer */
         pSet->HB_SET_EXTRAFILE = szFile ? hb_strndup( szFile, USHRT_MAX ) : NULL;
         if( szFile )
         {
            close_text( pSet, pSet->hb_set_extrahan );
            pSet->hb_set_extrahan = FS_ERROR;
            if( pSet->HB_SET_EXTRAFILE && pSet->HB_SET_EXTRAFILE[ 0 ] != '\0' )
               pSet->hb_set_extrahan = open_handle( pSet, pSet->HB_SET_EXTRAFILE,
                                                    fAdditive, ".prn", HB_SET_EXTRAFILE );
         }
         break;

      case HB_SET_PRINTFILE:
         if( pSet->HB_SET_PRINTFILE )
            hb_xfree( pSet->HB_SET_PRINTFILE );
         /* Limit size of SET strings to 64K, truncating if source is longer */
         pSet->HB_SET_PRINTFILE = szFile ? hb_strndup( szFile, USHRT_MAX ) : NULL;
         if( szFile )
         {
            close_binary( pSet, pSet->hb_set_printhan );
            pSet->hb_set_printhan = FS_ERROR;
            if( pSet->HB_SET_PRINTFILE && pSet->HB_SET_PRINTFILE[ 0 ] != '\0' )
               pSet->hb_set_printhan = open_handle( pSet, pSet->HB_SET_PRINTFILE,
                                                    fAdditive, ".prn", HB_SET_PRINTFILE );
         }
         break;

      default:
         fResult = FALSE;
         break;
   }

   return fResult;
}

BOOL hb_setSetItem( HB_set_enum set_specifier, PHB_ITEM pItem )
{
   HB_THREAD_STUB
   PHB_SET_STRUCT pSet     = hb_stackSetStruct();
   BOOL           fResult  = FALSE;
   char *         szValue;
   int            iValue;

   if( pItem )
   {
      hb_setListenerNotify( set_specifier, HB_SET_LISTENER_BEFORE );

      switch( set_specifier )
      {
         case HB_SET_ALTFILE:
         case HB_SET_EXTRAFILE:
         case HB_SET_PRINTFILE:
            /* This sets needs 3-rd parameter to indicate additive mode
             * so they cannot be fully supported by this function
             */
            if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
            {
               fResult = hb_setSetFile( pSet, set_specifier, HB_IS_STRING( pItem ) ?
                                        hb_itemGetCPtr( pItem ) : NULL, FALSE );
            }
            break;

         case HB_SET_ALTERNATE:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_ALTERNATE  = hb_itemGetL( pItem );
               fResult                 = TRUE;
            }
            break;

         case HB_SET_AUTOPEN:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_AUTOPEN = hb_itemGetL( pItem );
               fResult              = TRUE;
            }
            break;

         case HB_SET_BACKGROUNDTASKS:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_BACKGROUNDTASKS  = hb_itemGetL( pItem );
               fResult                       = TRUE;
            }
            break;

         case HB_SET_BELL:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_BELL = hb_itemGetL( pItem );
               fResult           = TRUE;
            }
            break;

         case HB_SET_CANCEL:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_CANCEL  = hb_itemGetL( pItem );
               fResult              = TRUE;
            }
            break;

         case HB_SET_CONFIRM:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_CONFIRM = hb_itemGetL( pItem );
               fResult              = TRUE;
            }
            break;

         case HB_SET_CONSOLE:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_CONSOLE = hb_itemGetL( pItem );
               fResult              = TRUE;
            }
            break;

         case HB_SET_DEBUG:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_DEBUG   = hb_itemGetL( pItem );
               fResult              = TRUE;
            }
            break;

         case HB_SET_DELETED:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_DELETED = hb_itemGetL( pItem );
               fResult              = TRUE;
            }
            break;

         case HB_SET_DELIMITERS:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_DELIMITERS = hb_itemGetL( pItem );
               fResult                 = TRUE;
            }
            break;

         case HB_SET_EOF:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_EOF  = hb_itemGetL( pItem );
               fResult           = TRUE;
            }
            break;

         case HB_SET_ESCAPE:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_ESCAPE  = hb_itemGetL( pItem );
               fResult              = TRUE;
            }
            break;

         case HB_SET_EXACT:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_EXACT   = hb_itemGetL( pItem );
               fResult              = TRUE;
            }
            break;

         case HB_SET_EXCLUSIVE:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_EXCLUSIVE  = hb_itemGetL( pItem );
               fResult                 = TRUE;
            }
            break;

         case HB_SET_EXIT:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_EXIT = hb_itemGetL( pItem );
               fResult           = TRUE;
            }
            break;

         case HB_SET_EXTRA:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_EXTRA   = hb_itemGetL( pItem );
               fResult              = TRUE;
            }
            break;

         case HB_SET_FIXED:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_FIXED   = hb_itemGetL( pItem );
               fResult              = TRUE;
            }
            break;

         case HB_SET_IDLEREPEAT:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_IDLEREPEAT = hb_itemGetL( pItem );
               fResult                 = TRUE;
            }
            break;

         case HB_SET_INSERT:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_INSERT  = hb_itemGetL( pItem );
               fResult              = TRUE;
            }
            break;

         case HB_SET_INTENSITY:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_INTENSITY  = hb_itemGetL( pItem );
               fResult                 = TRUE;
            }
            break;

         case HB_SET_MCENTER:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_MCENTER = hb_itemGetL( pItem );
               fResult              = TRUE;
            }
            break;

         case HB_SET_OPTIMIZE:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_OPTIMIZE   = hb_itemGetL( pItem );
               fResult                 = TRUE;
            }
            break;

         case HB_SET_FORCEOPT:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_FORCEOPT   = hb_itemGetL( pItem );
               fResult                 = TRUE;
            }
            break;

         case HB_SET_OUTPUTSAFETY:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_OUTPUTSAFETY  = hb_itemGetL( pItem );
               fResult                    = TRUE;
            }
            break;

         case HB_SET_PRINTER:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_PRINTER = hb_itemGetL( pItem );
               fResult              = TRUE;
            }
            break;

         case HB_SET_SCOREBOARD:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_SCOREBOARD = hb_itemGetL( pItem );
               fResult                 = TRUE;
            }
            break;

         case HB_SET_SCROLLBREAK:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_SCROLLBREAK   = hb_itemGetL( pItem );
               fResult                    = TRUE;
            }
            break;

         case HB_SET_SOFTSEEK:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_SOFTSEEK   = hb_itemGetL( pItem );
               fResult                 = TRUE;
            }
            break;

         case HB_SET_STRICTREAD:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_STRICTREAD = hb_itemGetL( pItem );
               fResult                 = TRUE;
            }
            break;

         case HB_SET_UNIQUE:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_UNIQUE  = hb_itemGetL( pItem );
               fResult              = TRUE;
            }
            break;

         case HB_SET_WORKAREAS_SHARED:
#ifdef HB_THREAD_SUPPORT
            if( HB_IS_LOGICAL( pItem ) )
            {
               if( hb_rddChangeSetWorkareasShared( pSet->HB_SET_WORKAREAS_SHARED, hb_itemGetL( pItem ) ) )
               {
                  pSet->HB_SET_WORKAREAS_SHARED = hb_itemGetL( pItem );
                  fResult                       = TRUE;
               }
            }
#endif
            break;

         case HB_SET_WRAP:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_WRAP = hb_itemGetL( pItem );
               fResult           = TRUE;
            }
            break;

         case HB_SET_HARDCOMMIT:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_HARDCOMMIT = hb_itemGetL( pItem );
               fResult                 = TRUE;
            }
            break;

         case HB_SET_DEFEXTENSIONS:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_DEFEXTENSIONS = hb_itemGetL( pItem );
               fResult                    = TRUE;
            }
            break;

         case HB_SET_TRIMFILENAME:
            if( HB_IS_LOGICAL( pItem ) )
            {
               pSet->HB_SET_TRIMFILENAME  = hb_itemGetL( pItem );
               fResult                    = TRUE;
            }
            break;

         case HB_SET_BACKGROUNDTICK:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_BACKGROUNDTICK   = iValue ? iValue : 1000;
                  fResult                       = TRUE;
               }
            }
            break;

         case HB_SET_DECIMALS:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_DECIMALS   = iValue;
                  fResult                 = TRUE;
               }
            }
            break;

         case HB_SET_EPOCH:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_EPOCH   = iValue;
                  fResult              = TRUE;
               }
            }
            break;

         case HB_SET_ERRORLOOP:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_ERRORLOOP  = iValue ? iValue : 8;
                  fResult                 = TRUE;
               }
            }
            break;

         case HB_SET_MBLOCKSIZE:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_MBLOCKSIZE = iValue;
                  fResult                 = TRUE;
               }
            }
            break;

         case HB_SET_DBFLOCKSCHEME:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_DBFLOCKSCHEME = iValue;
                  fResult                    = TRUE;
               }
            }
            break;

         case HB_SET_AUTORDER:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_AUTORDER   = iValue;
                  fResult                 = TRUE;
               }
            }
            break;

         case HB_SET_AUTOSHARE:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_AUTOSHARE  = iValue;
                  fResult                 = TRUE;
               }
            }
            break;

         case HB_SET_CURSOR:
            if( HB_IS_NUMERIC( pItem ) )
            {
               hb_conSetCursor( TRUE, ( USHORT ) hb_itemGetNI( pItem ) );
               fResult = TRUE;
            }
            break;

         case HB_SET_EVENTMASK:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_EVENTMASK  = iValue;
                  fResult                 = TRUE;
               }
            }
            break;

         case HB_SET_MARGIN:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_MARGIN  = iValue;
                  fResult              = TRUE;
               }
            }
            break;

         case HB_SET_MESSAGE:
            if( HB_IS_NUMERIC( pItem ) )
            {
               iValue = hb_itemGetNI( pItem );
               if( iValue >= 0 )
               {
                  pSet->HB_SET_MESSAGE = iValue;
                  fResult              = TRUE;
               }
            }
            break;

         case HB_SET_TYPEAHEAD:
            if( HB_IS_NUMERIC( pItem ) )
            {
               /* Set the value and limit the range */
               pSet->HB_SET_TYPEAHEAD = hb_itemGetNI( pItem );
               if( pSet->HB_SET_TYPEAHEAD == 0 )
                  /* Do nothing */;
               else if( pSet->HB_SET_TYPEAHEAD < 16 )
                  pSet->HB_SET_TYPEAHEAD = 16;
               else if( pSet->HB_SET_TYPEAHEAD > 4096 )
                  pSet->HB_SET_TYPEAHEAD = 4096;
               /* reset keyboard buffer */
               hb_inkeyReset();
               fResult = TRUE;
            }
            break;

         case HB_SET_VIDEOMODE:
            if( HB_IS_NUMERIC( pItem ) )
            {
               pSet->HB_SET_VIDEOMODE  = hb_itemGetNI( pItem );
               fResult                 = TRUE;
            }
            break;

         case HB_SET_COLOR:
            if( HB_IS_STRING( pItem ) )
            {
               hb_conSetColor( hb_itemGetCPtr( pItem ) );
               fResult = TRUE;
            }
            break;

         case HB_SET_LANGUAGE:
            if( HB_IS_STRING( pItem ) )
            {
               hb_langSelectID( hb_itemGetCPtr( pItem ) );
               fResult = TRUE;
            }
            break;

         case HB_SET_CODEPAGE:
            if( HB_IS_STRING( pItem ) )
            {
               hb_cdpSelectID( hb_itemGetCPtr( pItem ) );
               fResult = TRUE;
            }
            break;

         case HB_SET_FILECASE:
         case HB_SET_DIRCASE:
            iValue = -1;
            if( HB_IS_STRING( pItem ) )
            {
               if( ! hb_stricmp( hb_itemGetCPtr( pItem ), "LOWER" ) )
                  iValue = HB_SET_CASE_LOWER;
               else if( ! hb_stricmp( hb_itemGetCPtr( pItem ), "UPPER" ) )
                  iValue = HB_SET_CASE_UPPER;
               else if( ! hb_stricmp( hb_itemGetCPtr( pItem ), "MIXED" ) )
                  iValue = HB_SET_CASE_MIXED;
            }
            else if( HB_IS_NUMERIC( pItem ) )
               iValue = hb_itemGetNI( pItem );

            if( iValue == HB_SET_CASE_LOWER ||
                iValue == HB_SET_CASE_UPPER ||
                iValue == HB_SET_CASE_MIXED )
            {
               if( set_specifier == HB_SET_FILECASE )
                  pSet->HB_SET_FILECASE = iValue;
               else
                  pSet->HB_SET_DIRCASE = iValue;
               fResult = TRUE;
            }
            break;

         case HB_SET_DATEFORMAT:
            if( HB_IS_STRING( pItem ) )
            {
               int iYear = 0;

               szValue                 = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_DATEFORMAT )
                  hb_xfree( pSet->HB_SET_DATEFORMAT );
               pSet->HB_SET_DATEFORMAT = szValue;
               while( *szValue )
               {
                  if( *szValue == 'Y' || *szValue == 'y' )
                     ++iYear;
                  else if( iYear )   /* Only count the first set of consecutive "Y"s. */
                     break;
                  ++szValue;
               }
               /* CENTURY is not controlled directly by SET, so there is no
                  notification for changing it indirectly via DATE FORMAT. */
               pSet->hb_set_century = iYear >= 4;
               fResult              = TRUE;
            }
            break;

         case HB_SET_TIMEFORMAT:
            if( HB_IS_STRING( pItem ) )
            {
               szValue                 = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_TIMEFORMAT )
                  hb_xfree( pSet->HB_SET_TIMEFORMAT );
               pSet->HB_SET_TIMEFORMAT = szValue;
               fResult                 = TRUE;
            }
            break;

         case HB_SET_DIRSEPARATOR:
            if( hb_itemGetCLen( pItem ) > 0 )
            {
               pSet->HB_SET_DIRSEPARATOR  = hb_itemGetCPtr( pItem )[ 0 ];
               fResult                    = TRUE;
            }
            break;

         case HB_SET_DEVICE:
            if( HB_IS_STRING( pItem ) )
            {
               szValue              = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_DEVICE )
                  hb_xfree( pSet->HB_SET_DEVICE );
               pSet->HB_SET_DEVICE  = szValue;

               /* If the print file is not already open, open it in overwrite mode. */
               if( hb_stricmp( szValue, "PRINTER" ) == 0 && pSet->hb_set_printhan == FS_ERROR &&
                   pSet->HB_SET_PRINTFILE && pSet->HB_SET_PRINTFILE[ 0 ] != '\0' )
                  pSet->hb_set_printhan = open_handle( pSet, pSet->HB_SET_PRINTFILE,
                                                       FALSE, ".prn", HB_SET_PRINTFILE );
               fResult = TRUE;
            }
            break;

         case HB_SET_MFILEEXT:
            if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
            {
               szValue                 = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_MFILEEXT )
                  hb_xfree( pSet->HB_SET_MFILEEXT );
               pSet->HB_SET_MFILEEXT   = szValue;
               fResult                 = TRUE;
            }
            break;

         case HB_SET_DEFAULT:
            if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
            {
               szValue              = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_DEFAULT )
                  hb_xfree( pSet->HB_SET_DEFAULT );
               pSet->HB_SET_DEFAULT = szValue;
               fResult              = TRUE;
            }
            break;

         case HB_SET_PATH:
            if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
            {
               szValue           = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_PATH )
                  hb_xfree( pSet->HB_SET_PATH );
               pSet->HB_SET_PATH = szValue;

               hb_fsFreeSearchPath( pSet->hb_set_path );
               pSet->hb_set_path = NULL;
               hb_fsAddSearchPath( pSet->HB_SET_PATH, &pSet->hb_set_path );

               fResult           = TRUE;
            }
            break;

         case HB_SET_DELIMCHARS:
            if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
            {
               szValue                 = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_DELIMCHARS )
                  hb_xfree( pSet->HB_SET_DELIMCHARS );
               pSet->HB_SET_DELIMCHARS = szValue;
               fResult                 = TRUE;
            }
            break;

         case HB_SET_EOL:
            if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
            {
               szValue           = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_EOL )
                  hb_xfree( pSet->HB_SET_EOL );
               pSet->HB_SET_EOL  = szValue;
               fResult           = TRUE;
            }
            break;

         case HB_SET_OSCODEPAGE:
            if( HB_IS_STRING( pItem ) || HB_IS_NIL( pItem ) )
            {
               szValue                 = hb_strndup( hb_itemGetCPtr( pItem ), USHRT_MAX );
               if( pSet->HB_SET_OSCODEPAGE )
                  hb_xfree( pSet->HB_SET_OSCODEPAGE );
               pSet->HB_SET_OSCODEPAGE = szValue;
               hb_set_OSCODEPAGE( pSet );
               fResult                 = TRUE;
            }
            break;

         case HB_SET_INVALID_:
         case HB_SET_ERRORLOG:
         case HB_SET_MACROBLOCKVARS:
         case HB_SET_PRINTERJOB:
         case HB_SET_TRACE:
         case HB_SET_TRACEFILE:
         case HB_SET_TRACESTACK:
            break;

#if 0
         /*
          * intentionally removed default: clause to enable C compiler warning
          * when not all HB_SET_* cases are implemented. [druzus]
          */
         default:
            break;
#endif
      }
      hb_setListenerNotify( set_specifier, HB_SET_LISTENER_AFTER );
   }

   return fResult;
}

BOOL hb_setSetItem2( HB_set_enum set_specifier, PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   BOOL fResult = FALSE;

   if( pItem1 )
   {
      switch( set_specifier )
      {
         case HB_SET_ALTFILE:
         case HB_SET_EXTRAFILE:
         case HB_SET_PRINTFILE:
            if( HB_IS_STRING( pItem1 ) || HB_IS_NIL( pItem1 ) )
            {
               HB_THREAD_STUB
               PHB_SET_STRUCT pSet = hb_stackSetStruct();

               hb_setListenerNotify( set_specifier, HB_SET_LISTENER_BEFORE );
               fResult = hb_setSetFile( pSet, set_specifier, HB_IS_STRING( pItem1 ) ?
                                        hb_itemGetCPtr( pItem1 ) : NULL,
                                        pItem2 && set_logical( pItem2, FALSE ) );
               hb_setListenerNotify( set_specifier, HB_SET_LISTENER_AFTER );
            }
            break;

         default:
            fResult = hb_setSetItem( set_specifier, pItem1 );
      }
   }
   return fResult;
}

BOOL hb_setGetL( HB_set_enum set_specifier )
{
   HB_THREAD_STUB
   PHB_SET_STRUCT pSet = hb_stackSetStruct();

   switch( set_specifier )
   {
      case HB_SET_ALTERNATE:
         return pSet->HB_SET_ALTERNATE;
      case HB_SET_AUTOPEN:
         return pSet->HB_SET_AUTOPEN;
      case HB_SET_BACKGROUNDTASKS:
         return pSet->HB_SET_BACKGROUNDTASKS;
      case HB_SET_BELL:
         return pSet->HB_SET_BELL;
      case HB_SET_CANCEL:
         return pSet->HB_SET_CANCEL;
      case HB_SET_CONFIRM:
         return pSet->HB_SET_CONFIRM;
      case HB_SET_CONSOLE:
         return pSet->HB_SET_CONSOLE;
      case HB_SET_DEBUG:
         return pSet->HB_SET_DEBUG;
      case HB_SET_DELETED:
         return pSet->HB_SET_DELETED;
      case HB_SET_DELIMITERS:
         return pSet->HB_SET_DELIMITERS;
      case HB_SET_EOF:
         return pSet->HB_SET_EOF;
      case HB_SET_ESCAPE:
         return pSet->HB_SET_ESCAPE;
      case HB_SET_EXACT:
         return pSet->HB_SET_EXACT;
      case HB_SET_EXCLUSIVE:
         return pSet->HB_SET_EXCLUSIVE;
      case HB_SET_EXIT:
         return pSet->HB_SET_EXIT;
      case HB_SET_EXTRA:
         return pSet->HB_SET_EXTRA;
      case HB_SET_FIXED:
         return pSet->HB_SET_FIXED;
      case HB_SET_IDLEREPEAT:
         return pSet->HB_SET_IDLEREPEAT;
      case HB_SET_INSERT:
         return pSet->HB_SET_INSERT;
      case HB_SET_INTENSITY:
         return pSet->HB_SET_INTENSITY;
      case HB_SET_MACROBLOCKVARS:
         return pSet->HB_SET_MACROBLOCKVARS;
      case HB_SET_MCENTER:
         return pSet->HB_SET_MCENTER;
      case HB_SET_OPTIMIZE:
         return pSet->HB_SET_OPTIMIZE;
      case HB_SET_OUTPUTSAFETY:
         return pSet->HB_SET_OUTPUTSAFETY;
      case HB_SET_FORCEOPT:
         return pSet->HB_SET_FORCEOPT;
      case HB_SET_PRINTER:
         return pSet->HB_SET_PRINTER;
      case HB_SET_SCOREBOARD:
         return pSet->HB_SET_SCOREBOARD;
      case HB_SET_SCROLLBREAK:
         return pSet->HB_SET_SCROLLBREAK;
      case HB_SET_SOFTSEEK:
         return pSet->HB_SET_SOFTSEEK;
      case HB_SET_STRICTREAD:
         return pSet->HB_SET_STRICTREAD;
      case HB_SET_TRACE:
         return pSet->HB_SET_TRACE;
      case HB_SET_UNIQUE:
         return pSet->HB_SET_UNIQUE;
      case HB_SET_WRAP:
         return pSet->HB_SET_WRAP;
      case HB_SET_HARDCOMMIT:
         return pSet->HB_SET_HARDCOMMIT;
      case HB_SET_DEFEXTENSIONS:
         return pSet->HB_SET_DEFEXTENSIONS;
      case HB_SET_TRIMFILENAME:
         return pSet->HB_SET_TRIMFILENAME;
      case HB_SET_WORKAREAS_SHARED:
         return pSet->HB_SET_WORKAREAS_SHARED;

      case HB_SET_ALTFILE:
      case HB_SET_AUTORDER:
      case HB_SET_AUTOSHARE:
      case HB_SET_BACKGROUNDTICK:
      case HB_SET_COLOR:
      case HB_SET_CURSOR:
      case HB_SET_DATEFORMAT:
      case HB_SET_DECIMALS:
      case HB_SET_DEFAULT:
      case HB_SET_DELIMCHARS:
      case HB_SET_DEVICE:
      case HB_SET_EPOCH:
      case HB_SET_ERRORLOG:
      case HB_SET_ERRORLOOP:
      case HB_SET_EVENTMASK:
      case HB_SET_EXTRAFILE:
      case HB_SET_MARGIN:
      case HB_SET_MBLOCKSIZE:
      case HB_SET_MESSAGE:
      case HB_SET_MFILEEXT:
      case HB_SET_PATH:
      case HB_SET_PRINTERJOB:
      case HB_SET_PRINTFILE:
      case HB_SET_TIMEFORMAT:
      case HB_SET_TRACEFILE:
      case HB_SET_TRACESTACK:
      case HB_SET_TYPEAHEAD:
      case HB_SET_VIDEOMODE:
      case HB_SET_LANGUAGE:
      case HB_SET_CODEPAGE:
      case HB_SET_FILECASE:
      case HB_SET_DIRCASE:
      case HB_SET_DIRSEPARATOR:
      case HB_SET_DBFLOCKSCHEME:
      case HB_SET_EOL:
      case HB_SET_OSCODEPAGE:
      case HB_SET_INVALID_:
         break;
#if 0
      /*
       * intentionally removed default: clause to enable C compiler warning
       * when not all HB_SET_* cases are implemented. [druzus]
       */
      default:
         break;
#endif
   }

   hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, 0 );
   return FALSE;
}

const char * hb_setGetCPtr( HB_set_enum set_specifier )
{
   HB_THREAD_STUB
   PHB_SET_STRUCT pSet = hb_stackSetStruct();

   switch( set_specifier )
   {
      case HB_SET_ALTFILE:
         return pSet->HB_SET_ALTFILE;
      case HB_SET_COLOR:
         return pSet->HB_SET_COLOR;
      case HB_SET_DATEFORMAT:
         return pSet->HB_SET_DATEFORMAT;
      case HB_SET_TIMEFORMAT:
         return pSet->HB_SET_TIMEFORMAT;
      case HB_SET_DEFAULT:
         return pSet->HB_SET_DEFAULT;
      case HB_SET_DELIMCHARS:
         return pSet->HB_SET_DELIMCHARS;
      case HB_SET_DEVICE:
         return pSet->HB_SET_DEVICE;
      case HB_SET_ERRORLOG:
         return pSet->HB_SET_ERRORLOG;
      case HB_SET_EXTRAFILE:
         return pSet->HB_SET_EXTRAFILE;
      case HB_SET_PATH:
         return pSet->HB_SET_PATH;
      case HB_SET_MFILEEXT:
         return pSet->HB_SET_MFILEEXT;
      case HB_SET_PRINTERJOB:
         return pSet->hb_set_printerjob;
      case HB_SET_PRINTFILE:
         return pSet->HB_SET_PRINTFILE;
      case HB_SET_TRACEFILE:
         return pSet->HB_SET_TRACEFILE;
      case HB_SET_EOL:
         return pSet->HB_SET_EOL;

      case HB_SET_OSCODEPAGE:
         return pSet->HB_SET_OSCODEPAGE;
      case HB_SET_LANGUAGE:
         return hb_langID();
      case HB_SET_CODEPAGE:
         return hb_cdpID();
      case HB_SET_ALTERNATE:
      case HB_SET_AUTOPEN:
      case HB_SET_AUTORDER:
      case HB_SET_AUTOSHARE:
      case HB_SET_BACKGROUNDTASKS:
      case HB_SET_BACKGROUNDTICK:
      case HB_SET_BELL:
      case HB_SET_CANCEL:
      case HB_SET_CONFIRM:
      case HB_SET_CONSOLE:
      case HB_SET_CURSOR:
      case HB_SET_DEBUG:
      case HB_SET_DECIMALS:
      case HB_SET_DELETED:
      case HB_SET_DELIMITERS:
      case HB_SET_EOF:
      case HB_SET_EPOCH:
      case HB_SET_ERRORLOOP:
      case HB_SET_ESCAPE:
      case HB_SET_EVENTMASK:
      case HB_SET_EXACT:
      case HB_SET_EXCLUSIVE:
      case HB_SET_EXIT:
      case HB_SET_EXTRA:
      case HB_SET_FIXED:
      case HB_SET_INSERT:
      case HB_SET_INTENSITY:
      case HB_SET_MACROBLOCKVARS:
      case HB_SET_MARGIN:
      case HB_SET_MBLOCKSIZE:
      case HB_SET_MCENTER:
      case HB_SET_MESSAGE:
      case HB_SET_OPTIMIZE:
      case HB_SET_OUTPUTSAFETY:
      case HB_SET_FORCEOPT:
      case HB_SET_STRICTREAD:
      case HB_SET_HARDCOMMIT:
      case HB_SET_PRINTER:
      case HB_SET_SCOREBOARD:
      case HB_SET_SCROLLBREAK:
      case HB_SET_SOFTSEEK:
      case HB_SET_TRACE:
      case HB_SET_TRACESTACK:
      case HB_SET_TYPEAHEAD:
      case HB_SET_UNIQUE:
      case HB_SET_VIDEOMODE:
      case HB_SET_WRAP:
      case HB_SET_IDLEREPEAT:
      case HB_SET_FILECASE:
      case HB_SET_DIRCASE:
      case HB_SET_DIRSEPARATOR:
      case HB_SET_DBFLOCKSCHEME:
      case HB_SET_DEFEXTENSIONS:
      case HB_SET_TRIMFILENAME:
      case HB_SET_WORKAREAS_SHARED:
      case HB_SET_INVALID_:
         break;
#if 0
      /*
       * intentionally removed default: clause to enable C compiler warning
       * when not all HB_SET_* cases are implemented. [druzus]
       */
      default:
         break;
#endif
   }

   hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, 0 );
   return FALSE;
}

int hb_setGetNI( HB_set_enum set_specifier )
{
   HB_THREAD_STUB
   PHB_SET_STRUCT pSet = hb_stackSetStruct();

   switch( set_specifier )
   {
      case HB_SET_AUTORDER:
         return pSet->HB_SET_AUTORDER;
      case HB_SET_AUTOSHARE:
         return pSet->HB_SET_AUTOSHARE;
      case HB_SET_BACKGROUNDTICK:
         return pSet->HB_SET_BACKGROUNDTICK;
      case HB_SET_DECIMALS:
         return pSet->HB_SET_DECIMALS;
      case HB_SET_EPOCH:
         return pSet->HB_SET_EPOCH;
      case HB_SET_ERRORLOOP:
         return pSet->HB_SET_ERRORLOOP;
      case HB_SET_EVENTMASK:
         return pSet->HB_SET_EVENTMASK;
      case HB_SET_MARGIN:
         return pSet->HB_SET_MARGIN;
      case HB_SET_MBLOCKSIZE:
         return pSet->HB_SET_MBLOCKSIZE;
      case HB_SET_MESSAGE:
         return pSet->HB_SET_MESSAGE;
      case HB_SET_TRACESTACK:
         return pSet->HB_SET_TRACESTACK;
      case HB_SET_TYPEAHEAD:
         return pSet->HB_SET_TYPEAHEAD;
      case HB_SET_FILECASE:
         return pSet->HB_SET_FILECASE;
      case HB_SET_DIRCASE:
         return pSet->HB_SET_DIRCASE;
      case HB_SET_DIRSEPARATOR:
         return pSet->HB_SET_DIRSEPARATOR;
      case HB_SET_VIDEOMODE:
         return pSet->HB_SET_VIDEOMODE;
      case HB_SET_DBFLOCKSCHEME:
         return pSet->HB_SET_DBFLOCKSCHEME;

      case HB_SET_ALTERNATE:
      case HB_SET_ALTFILE:
      case HB_SET_AUTOPEN:
      case HB_SET_BACKGROUNDTASKS:
      case HB_SET_BELL:
      case HB_SET_CANCEL:
      case HB_SET_COLOR:
      case HB_SET_CONFIRM:
      case HB_SET_CONSOLE:
      case HB_SET_CURSOR:
      case HB_SET_DATEFORMAT:
      case HB_SET_DEBUG:
      case HB_SET_DEFAULT:
      case HB_SET_DELETED:
      case HB_SET_DELIMCHARS:
      case HB_SET_DELIMITERS:
      case HB_SET_DEVICE:
      case HB_SET_EOF:
      case HB_SET_ERRORLOG:
      case HB_SET_ESCAPE:
      case HB_SET_EXACT:
      case HB_SET_EXCLUSIVE:
      case HB_SET_EXIT:
      case HB_SET_EXTRA:
      case HB_SET_EXTRAFILE:
      case HB_SET_FIXED:
      case HB_SET_INSERT:
      case HB_SET_INTENSITY:
      case HB_SET_MACROBLOCKVARS:
      case HB_SET_MCENTER:
      case HB_SET_MFILEEXT:
      case HB_SET_OPTIMIZE:
      case HB_SET_FORCEOPT:
      case HB_SET_STRICTREAD:
      case HB_SET_HARDCOMMIT:
      case HB_SET_OUTPUTSAFETY:
      case HB_SET_PATH:
      case HB_SET_PRINTER:
      case HB_SET_PRINTERJOB:
      case HB_SET_PRINTFILE:
      case HB_SET_SCOREBOARD:
      case HB_SET_SCROLLBREAK:
      case HB_SET_SOFTSEEK:
      case HB_SET_TIMEFORMAT:
      case HB_SET_TRACE:
      case HB_SET_TRACEFILE:
      case HB_SET_UNIQUE:
      case HB_SET_WRAP:
      case HB_SET_LANGUAGE:
      case HB_SET_CODEPAGE:
      case HB_SET_IDLEREPEAT:
      case HB_SET_EOL:
      case HB_SET_DEFEXTENSIONS:
      case HB_SET_TRIMFILENAME:
      case HB_SET_OSCODEPAGE:
      case HB_SET_WORKAREAS_SHARED:
      case HB_SET_INVALID_:
         break;
#if 0
      /*
       * intentionally removed default: clause to enable C compiler warning
       * when not all HB_SET_* cases are implemented. [druzus]
       */
      default:
         break;
#endif
   }

   hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, 0 );
   return FALSE;
}

long hb_setGetNL( HB_set_enum set_specifier )
{
   return hb_setGetNI( set_specifier );
}

HB_PATHNAMES * hb_setGetFirstSetPath( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->hb_set_path;
}

HB_FHANDLE hb_setGetAltHan( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->hb_set_althan;
}

BOOL hb_setGetCentury( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->hb_set_century;
}

HB_FHANDLE hb_setGetExtraHan( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->hb_set_extrahan;
}

HB_FHANDLE hb_setGetPrintHan( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->hb_set_printhan;
}

BOOL hb_setGetAlternate( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_ALTERNATE;
}

const char *  hb_setGetAltFile( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_ALTFILE;
}

BOOL hb_setGetAutOpen( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_AUTOPEN;
}

int hb_setGetAutOrder( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_AUTORDER;
}

int hb_setGetAutoShare( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_AUTOSHARE;
}

BOOL hb_setGetBell( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_BELL;
}

BOOL hb_setGetCancel( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_CANCEL;
}

char *  hb_setGetColor( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_COLOR;
}

BOOL hb_setGetConfirm( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_CONFIRM;
}

BOOL hb_setGetConsole( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_CONSOLE;
}

const char * hb_setGetDateFormat( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_DATEFORMAT;
}

const char * hb_setGetTimeFormat( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_TIMEFORMAT;
}

BOOL hb_setGetDebug( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_DEBUG;
}

int hb_setGetDecimals( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_DECIMALS;
}

const char *  hb_setGetDefault( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_DEFAULT;
}

BOOL hb_setGetDeleted( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_DELETED;
}

const char *  hb_setGetDelimChars( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_DELIMCHARS;
}

BOOL hb_setGetDelimiters( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_DELIMITERS;
}

const char *  hb_setGetDevice( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_DEVICE;
}

BOOL hb_setGetEOF( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_EOF;
}

int hb_setGetEpoch( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_EPOCH;
}

BOOL hb_setGetEscape( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_ESCAPE;
}

int hb_setGetEventMask( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_EVENTMASK;
}

BOOL hb_setGetExact( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_EXACT;
}

BOOL hb_setGetExclusive( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_EXCLUSIVE;
}

BOOL hb_setGetExit( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_EXIT;
}

BOOL hb_setGetExtra( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_EXTRA;
}

const char *  hb_setGetExtraFile( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_EXTRAFILE;
}

BOOL hb_setGetFixed( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_FIXED;
}

BOOL hb_setGetIdleRepeat( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_IDLEREPEAT;
}

BOOL hb_setGetInsert( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_INSERT;
}

BOOL hb_setGetIntensity( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_INTENSITY;
}

const char *  hb_setGetPath( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_PATH;
}

int hb_setGetMargin( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_MARGIN;
}

int hb_setGetMBlockSize( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_MBLOCKSIZE;
}

BOOL hb_setGetMCenter( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_MCENTER;
}

int hb_setGetMessage( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_MESSAGE;
}

const char *  hb_setGetMFileExt( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_MFILEEXT;
}

BOOL hb_setGetOptimize( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_OPTIMIZE;
}

BOOL hb_setGetPrinter( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_PRINTER;
}

const char *  hb_setGetPrintFile( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_PRINTFILE;
}

BOOL hb_setGetScoreBoard( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_SCOREBOARD;
}

BOOL hb_setGetScrollBreak( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_SCROLLBREAK;
}

BOOL hb_setGetSoftSeek( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_SOFTSEEK;
}

BOOL hb_setGetStrictRead( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_STRICTREAD;
}

int hb_setGetTypeAhead( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_TYPEAHEAD;
}

BOOL hb_setGetUnique( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_UNIQUE;
}

int hb_setGetFileCase( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_FILECASE;
}

int hb_setGetDirCase( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_DIRCASE;
}

int hb_setGetDirSeparator( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_DIRSEPARATOR;
}

int hb_setGetVideoMode( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_VIDEOMODE;
}

BOOL hb_setGetWrap( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_WRAP;
}

int hb_setGetDBFLockScheme( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_DBFLOCKSCHEME;
}

BOOL hb_setGetHardCommit( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_HARDCOMMIT;
}

BOOL hb_setGetForceOpt( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_FORCEOPT;
}

BOOL hb_setGetDefExtension( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_DEFEXTENSIONS;
}

const char * hb_setGetEOL( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_EOL;
}

BOOL hb_setGetTrimFileName( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_TRIMFILENAME;
}

const char * hb_setGetOSCODEPAGE( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_OSCODEPAGE;
}

const char * hb_osEncodeCP( const char * szName, char ** pszFree, HB_SIZE * pulSize )
{
   HB_THREAD_STUB

#if defined( HB_THREAD_SUPPORT )
   if( hb_stackId() )
#endif
   {
      PHB_CODEPAGE cdpOS = ( PHB_CODEPAGE ) hb_stackSetStruct()->HB_SET_OSCODEPAGE;
      if( cdpOS )
      {
         PHB_CODEPAGE cdpHost = hb_cdppage();
         if( cdpHost && cdpHost != cdpOS )
         {
            HB_SIZE  ulSize = 0;
            char *   pszBuf;

            if( pszFree == NULL )
            {
               pszFree  = ( char ** ) &szName;
               ulSize   = strlen( szName );
            }
            pszBuf = *pszFree;
            if( pulSize == NULL )
               pulSize = &ulSize;
            else if( *pulSize > 0 )
               ulSize = *pulSize - 1;

            szName = hb_cdpnDup3( szName, strlen( szName ),
                                  pszBuf, &ulSize, pszFree, pulSize,
                                  cdpHost, cdpOS );
         }
      }
   }

   return szName;
}

const char * hb_osDecodeCP( const char * szName, char ** pszFree, HB_SIZE * pulSize )
{
   HB_THREAD_STUB

#if defined( HB_THREAD_SUPPORT )
   if( hb_stackId() )
#endif
   {
      PHB_CODEPAGE cdpOS = ( PHB_CODEPAGE ) hb_stackSetStruct()->HB_SET_OSCODEPAGE;
      if( cdpOS )
      {
         PHB_CODEPAGE cdpHost = hb_cdppage();
         if( cdpHost && cdpHost != cdpOS )
         {
            HB_SIZE  ulSize = 0;
            char *   pszBuf;

            if( pszFree == NULL )
            {
               pszFree  = ( char ** ) &szName;
               ulSize   = strlen( szName );
            }
            pszBuf = *pszFree;
            if( pulSize == NULL )
               pulSize = &ulSize;
            else if( *pulSize > 0 )
               ulSize = *pulSize - 1;

            szName = hb_cdpnDup3( szName, strlen( szName ),
                                  pszBuf, &ulSize, pszFree, pulSize,
                                  cdpOS, cdpHost );
         }
      }
   }

   return szName;
}

BOOL hb_setGetAppendError( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_APPENDERROR;
}

char * hb_setGetErrorLog( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_ERRORLOG;
}

BOOL hb_setGetTrace( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_TRACE;
}

char * hb_setGetTraceFile( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_TRACEFILE;
}

char hb_setGetTraceStack( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_TRACESTACK;
}

int hb_setGetErrorLoop( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_ERRORLOOP;
}

BOOL hb_setGetOutputSafety( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_OUTPUTSAFETY;
}

BOOL hb_setGetBackgroundTasks( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_BACKGROUNDTASKS;
}

BOOL hb_setGetWinPrinter( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->hb_set_winprinter;
}

HB_FHANDLE hb_setGetWinHan( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->hb_set_winhan;
}

char * hb_setGetPrinterJob( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->hb_set_printerjob;
}

int hb_setGetBackGroundTick( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_BACKGROUNDTICK;
}

int hb_setGetWorkareasShared( void )
{
   HB_THREAD_STUB
   return hb_stackSetStruct()->HB_SET_WORKAREAS_SHARED;
}
