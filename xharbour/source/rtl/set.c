/*
 * $Id: set.c,v 1.84 2008/03/10 11:24:05 likewolf Exp $
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

#include <ctype.h>

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbapigt.h"
#include "hbapilng.h"
#include "hbset.h"

HB_SET_STRUCT hb_set;
HB_SET_STRUCT * hb_set_ptr = &hb_set;

typedef struct HB_SET_LISTENER_
{
   int listener;
   HB_SET_LISTENER_CALLBACK * callback;
   struct HB_SET_LISTENER_ * next;
} HB_SET_LISTENER, * PHB_SET_LISTENER;

static PHB_SET_LISTENER sp_sl_first;
static PHB_SET_LISTENER sp_sl_last;
static int s_next_listener;

static char hb_dirsep_string[2];
static HB_PATHNAMES * sp_set_path;

#if defined(HB_OS_WIN_32) && (!defined(__RSXNT__)) && (!defined(__CYGWIN__))
      extern BOOL hb_PrinterExists(LPTSTR pPrinterName) ;
      extern LONG hb_PrintFileRaw(UCHAR *cPrinterName,UCHAR *cFileName, UCHAR *cDocName) ;
      extern BOOL hb_GetDefaultPrinter(LPTSTR pPrinterName, LPDWORD pdwBufferSize);
      extern BOOL hb_isLegacyDevice( LPTSTR pPrinterName);
#endif
char s_PrintFileName[ _POSIX_PATH_MAX + 1 ], s_PrinterName[ _POSIX_PATH_MAX + 1 ];
BOOL s_isDefaultPrinterDevice;    // Printer is the default device
static void hb_setFreeSetPath( void )
{
   if( sp_set_path )
   {
      hb_fsFreeSearchPath( sp_set_path );
      sp_set_path = NULL;
   }
}

HB_EXPORT HB_SET_STRUCT *hb_GetSetStructPtr( void )
{
   return &hb_set;
}

static char set_char( PHB_ITEM pItem, char oldChar )
{
   char newChar = oldChar;

   HB_TRACE(HB_TR_DEBUG, ("set_char(%p, %c)", pItem, oldChar));

   if( HB_IS_STRING( pItem ) )
   {
      /* Only replace if string has at least one character. */
      ULONG ulLen = pItem->item.asString.length;
      if( ulLen > 0 )
      {
         newChar = *pItem->item.asString.value;
      }
   }
   return newChar;
}

static BOOL set_logical( PHB_ITEM pItem, BOOL bDefault )
{
   BOOL bLogical = bDefault;

   HB_TRACE(HB_TR_DEBUG, ("set_logical(%p)", pItem));

   if( HB_IS_LOGICAL( pItem ) )
      bLogical = hb_itemGetL( pItem );
   else if( HB_IS_STRING( pItem ) )
   {
      char * szString = pItem->item.asString.value;
      ULONG ulLen = pItem->item.asString.length;

      if( ulLen >= 2
       && toupper( szString[ 0 ] ) == 'O'
       && toupper( szString[ 1 ] ) == 'N' )
         bLogical = TRUE;
      else if( ulLen >= 3
       && toupper( szString[ 0 ] ) == 'O'
       && toupper( szString[ 1 ] ) == 'F'
       && toupper( szString[ 2 ] ) == 'F' )
         bLogical = FALSE;
   }

   return bLogical;
}

static int set_number( PHB_ITEM pItem, int iOldValue )
{
   HB_TRACE(HB_TR_DEBUG, ("set_number(%p, %d)", pItem, iOldValue));

   if( HB_IS_NUMERIC( pItem ) )
   {
      return hb_itemGetNI( pItem );
   }
   else
   {
      return iOldValue;
   }
}

static char * set_string( PHB_ITEM pItem, char * szOldString )
{
   char * szString;

   HB_TRACE(HB_TR_DEBUG, ("set_string(%p, %s)", pItem, szOldString));

   if( HB_IS_STRING( pItem ) )
   {
      /* Limit size of SET strings to 64K, truncating if source is longer */
      ULONG ulLen = pItem->item.asString.length;

      if( ulLen > USHRT_MAX )
      {
         ulLen = USHRT_MAX;
      }

      if( szOldString )
      {
         szString = ( char * ) hb_xrealloc( szOldString, ulLen + 1 );
      }
      else
      {
         szString = ( char * ) hb_xgrab( ulLen + 1 );
      }

      memcpy( szString, pItem->item.asString.value, ulLen );
      szString[ ulLen ] = '\0';
   }
   else if( HB_IS_NIL( pItem ) )
   {
      if( szOldString )
      {
         szString = ( char * ) hb_xrealloc( szOldString, 1 );
      }
      else
      {
         szString = ( char * ) hb_xgrab( 1 );
      }

      szString[ 0 ] = '\0';
   }
   else
   {
      szString = szOldString;
   }

   return szString;
}

static void close_binary( FHANDLE handle )
{
   HB_TRACE(HB_TR_DEBUG, ("close_binary(%p)", handle));

   if( handle != FS_ERROR )
   {
      /* Close the file handle without disrupting the current
         user file error value */
      USHORT user_ferror = hb_fsError();
      hb_fsClose( handle );
      hb_fsSetError( user_ferror );
#if defined(HB_OS_WIN_32) && (!defined(__RSXNT__)) && (!defined(__CYGWIN__))
      if ( hb_set.hb_set_winprinter && ( hb_set.hb_set_printhan == handle ) && s_PrintFileName[0] )
      {
         if ( hb_fsFSize( (BYTE *) s_PrintFileName, FALSE ) > 0 )
         {
            hb_PrintFileRaw( (BYTE *) s_PrinterName, (BYTE *) s_PrintFileName,
                            (BYTE *) ( hb_set.hb_set_printerjob ? hb_set.hb_set_printerjob : s_PrintFileName ) ) ;
         }
         hb_fsDelete( (BYTE *) s_PrintFileName );
      }
#endif
   }
}

static void close_text( FHANDLE handle )
{
   HB_TRACE(HB_TR_DEBUG, ("close_text(%p)", handle));

   if( handle != FS_ERROR )
   {
      /* Close the file handle without disrupting the current
         user file error value */
      USHORT user_ferror = hb_fsError();
      if( hb_set.HB_SET_EOF )
      {
         hb_fsWrite( handle, ( BYTE * ) "\x1A", 1 );
      }
      hb_fsClose( handle );
      hb_fsSetError( user_ferror );
   }
}

static FHANDLE open_handle( char * file_name, BOOL bAppend, char * def_ext, HB_set_enum set_specifier )
{
   USHORT user_ferror;
   FHANDLE handle;
   PHB_FNAME pFilename;
   char path[ _POSIX_PATH_MAX + 1 ], *szPrnFile ;
   BOOL bPipe = FALSE, bTemp = FALSE;
   HB_TRACE(HB_TR_DEBUG, ("open_handle(%s, %d, %s, %d)", file_name, (int) bAppend, def_ext, (int) set_specifier));

   user_ferror = hb_fsError(); /* Save the current user file error code */
   /* Create full filename */

   szPrnFile = path;
#if defined(OS_UNIX_COMPATIBLE)
   if( ( bPipe = ( set_specifier == HB_SET_PRINTFILE && (char) *file_name == '|' ) ) )
   {
      szPrnFile = file_name;
      file_name++;
   }
#endif
   if( ! bPipe )
   {
      pFilename = hb_fsFNameSplit( file_name );

      if( ! pFilename->szPath && hb_set.HB_SET_DEFAULT )
         pFilename->szPath = hb_set.HB_SET_DEFAULT;
      if( ! pFilename->szExtension && def_ext )
         pFilename->szExtension = def_ext;

      hb_fsFNameMerge( path, pFilename );
      hb_xfree( pFilename ) ;

      strcpy(s_PrinterName, file_name) ;
#if defined(HB_OS_WIN_32) && (!defined(__RSXNT__)) && (!defined(__CYGWIN__))
      if ( set_specifier == HB_SET_PRINTFILE )
      {
         if ( hb_stricmp( s_PrinterName, "prn" ) == 0 )
         {
            DWORD nSize= _POSIX_PATH_MAX ;
            hb_GetDefaultPrinter( (LPTSTR) s_PrinterName, &nSize );
            if ( !s_PrinterName[0] )
               strcpy( s_PrinterName, "lpt1" ) ;
         }
         hb_set.hb_set_winprinter = hb_PrinterExists( s_PrinterName );
         if ( hb_set.hb_set_winprinter )
         {
            szPrnFile = s_PrintFileName;
            bTemp = TRUE;
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

      if ( bPipe )
         handle = hb_fsPOpen( ( BYTE * ) file_name, ( BYTE * ) "w" );
      else if ( bTemp )
         handle = hb_fsCreateTemp( NULL, NULL, FC_NORMAL, ( BYTE * ) s_PrintFileName );
      else
      {
         if ( bAppend )
         {  /* Append mode */
            if( hb_fsFile( ( BYTE * ) szPrnFile ) )
            {  /* If the file already exists, open it (in read-write mode, in
                  case of non-Unix and text modes). */
               handle = hb_fsOpen( ( BYTE * ) szPrnFile, FO_READWRITE);
               if( handle != FS_ERROR )
               {  /* Position to EOF */
                  /* Special binary vs. text file handling - even for UN*X, now
                     that there's an HB_SET_EOF flag. */
                  if( set_specifier == HB_SET_PRINTFILE )
                  {  /* PRINTFILE is always binary and needs no special handling. */
                     hb_fsSeek( handle, 0, FS_END );
                  }
                  else
                  {  /* All other files are text files and may have an EOF
                        ('\x1A') character at the end (both UN*X and non-UN*X,
                        now that theres an HB_SET_EOF flag). */
                     char cEOF = '\0';
                     hb_fsSeek( handle, -1, FS_END ); /* Position to last char. */
                     hb_fsRead( handle, ( BYTE * ) &cEOF, 1 );   /* Read the last char. */
                     if( cEOF == '\x1A' )             /* If it's an EOF, */
                     {
                        hb_fsSeek( handle, -1, FS_END ); /* Then write over it. */
                     }
                  }
               }
            }
            else bCreate = TRUE; /* Otherwise create a new file. */
         }
         else bCreate = TRUE; /* Always create a new file for overwrite mode. */

         if( bCreate )
         {
#if defined(HB_OS_WIN_32) && (!defined(__RSXNT__)) && (!defined(__CYGWIN__))
           if ( hb_isLegacyDevice( s_PrinterName ) )
            {  // according to the Win SDK devices should be opened not created
               handle = hb_fsOpen( ( BYTE * ) szPrnFile, FO_READWRITE );
            }
            else
#endif
            {
               handle = hb_fsCreateEx( ( BYTE * ) szPrnFile, FC_NORMAL, FO_DENYNONE );
            }
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

// Sets default printer device
static void hb_set_SetDefaultPrinter( void )
{
   if( hb_set.HB_SET_PRINTFILE )
   {
      hb_xfree( hb_set.HB_SET_PRINTFILE );
   }

   #ifdef HB_OS_UNIX
      hb_set.HB_SET_PRINTFILE = ( char * ) hb_xgrab( 5 );
      memcpy( hb_set.HB_SET_PRINTFILE, "|lpr", 5 );
   #else
      hb_set.HB_SET_PRINTFILE = ( char * ) hb_xgrab( 4 );
      memcpy( hb_set.HB_SET_PRINTFILE, "PRN", 4 );
   #endif

   s_isDefaultPrinterDevice = TRUE;
}

// Initializes printer if needed
BOOL hb_set_SetPrinterStart( void )
{
   BOOL bDone;

   if( hb_set.hb_set_printhan != FS_ERROR )
   {
      // It's already open
      bDone = TRUE;
   }
   else if( ! s_isDefaultPrinterDevice )
   {
      // It's not the default printer device... can't it be opened?
      bDone = FALSE;
   }
   else
   {
      // Opens printer
      hb_set.hb_set_printhan = open_handle( hb_set.HB_SET_PRINTFILE, FALSE, ".prn", HB_SET_PRINTER );
      bDone = ( hb_set.hb_set_printhan != FS_ERROR );
   }

   return bDone;
}

// Closes default printer if needed
void hb_set_SetPrinterStop( void )
{
   if( hb_set.hb_set_printhan != FS_ERROR  &&  // It's open
       s_isDefaultPrinterDevice            &&  // It's the default printer device
       ! hb_set.HB_SET_PRINTER             &&  // SET PRINTER OFF
       hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) != 0 )   // SET DEVICE TO SCREEN
   {
      // Closes printer
      close_binary( hb_set.hb_set_printhan );
      hb_set.hb_set_printhan = FS_ERROR;
   }
}

HB_FUNC( SETCANCEL )
{
   hb_retl( hb_set.HB_SET_CANCEL );

   if( ISLOG( 1 ) )
   {
      hb_set.HB_SET_CANCEL = hb_parl( 1 );
   }
}

HB_FUNC( __SETCENTURY )
{
   BOOL old_century_setting = hb_set.hb_set_century;

   /*
    * Change the setting if the parameter is a logical value, or is
    * either "ON" or "OFF" (regardless of case)
    */
   if( ISLOG( 1 ) )
      hb_set.hb_set_century = hb_parl( 1 );
   else if( ISCHAR( 1 ) )
   {
      char * szString = hb_parcx( 1 );
      ULONG ulLen = hb_parclen( 1 );

      if( ulLen >= 2
       && toupper( szString[ 0 ] ) == 'O'
       && toupper( szString[ 1 ] ) == 'N' )
         hb_set.hb_set_century = TRUE;
      else if( ulLen >= 3
       && toupper( szString[ 0 ] ) == 'O'
       && toupper( szString[ 1 ] ) == 'F'
       && toupper( szString[ 2 ] ) == 'F' )
         hb_set.hb_set_century = FALSE;
   }

   /*
    * Finally, if the setting changed, adjust the current date format to use
    * the correct number of year digits.
    */
   if( old_century_setting != hb_set.hb_set_century )
   {
      int count, digit, size, y_size, y_start, y_stop;
      char * szDateFormat, * szNewFormat;

      /* Convert to upper case and determine where year is */
      y_start = y_stop = -1;
      szDateFormat = hb_set.HB_SET_DATEFORMAT;
      size = strlen( szDateFormat );
      for( count = 0; count < size; count++ )
      {
         digit = toupper( szDateFormat[ count ] );
         if( digit == 'Y' )
         {
            if( y_start == -1 ) y_start = count;
         }
         else if( y_start > -1 && y_stop == -1 ) y_stop = count;
         szDateFormat[ count ] = digit;
      }
      /* Determine size of year in current format */
      if( y_start < 0 )
      {
         y_start = 0; /* There is no year in the current format */
         y_stop = 0;
      }
      else if( y_stop < 0 ) y_stop = size; /* All digits are year digits */
      y_size = y_stop - y_start;
      /* Calculate size of new format */
      size -= y_size;
      if( hb_set.hb_set_century ) size += 4;
      else size += 2;

      /* Create the new date format */
      szNewFormat = ( char * ) hb_xgrab( size + 1 );

      {
         int format_len;
         if( y_start > 0 ) memcpy( szNewFormat, szDateFormat, y_start );
         szNewFormat[ y_start ] = '\0';
         strcat( szNewFormat, "YY" );
         if( hb_set.hb_set_century ) strcat( szNewFormat, "YY" );
         format_len = strlen( szDateFormat );
         if( y_stop < format_len ) strcat( szNewFormat, szDateFormat + y_stop );
         hb_xfree( szDateFormat );
         /* DATE FORMAT is under direct control of SET, so notify when it
            it is changed indirectly via __SETCENTURY() */
         hb_setListenerNotify( HB_SET_DATEFORMAT, HB_SET_LISTENER_BEFORE );
         hb_set.HB_SET_DATEFORMAT = szNewFormat;
         hb_setListenerNotify( HB_SET_DATEFORMAT, HB_SET_LISTENER_AFTER );
      }
   }

   /* Return the previous setting */
   hb_retl( old_century_setting );
}

HB_FUNC( SET )
{
   BOOL bFlag;
   int args = hb_pcount();

   HB_set_enum set_specifier = ( args > 0 ) ? ( HB_set_enum ) hb_parni( 1 ) : HB_SET_INVALID_;
   PHB_ITEM pArg2 = ( args > 1 ) ? hb_param( 2, HB_IT_ANY ) : NULL;
   PHB_ITEM pArg3 = ( args > 2 ) ? hb_param( 3, HB_IT_ANY ) : NULL;

   if( args > 1 ) hb_setListenerNotify( set_specifier, HB_SET_LISTENER_BEFORE );

   switch ( set_specifier )
   {
      case HB_SET_ALTERNATE  :
         hb_retl( hb_set.HB_SET_ALTERNATE );
         if( args > 1 )
         {
            hb_set.HB_SET_ALTERNATE = set_logical( pArg2, hb_set.HB_SET_ALTERNATE );
         }
         break;

      case HB_SET_ALTFILE    :
         if( hb_set.HB_SET_ALTFILE )
         {
            hb_retc( hb_set.HB_SET_ALTFILE );
         }
         else
         {
            hb_retc( NULL );
         }

         if( args > 1 )
         {
            if( HB_IS_NIL( pArg2 ) )
            {
               if( hb_set.HB_SET_ALTFILE )
               {
                  hb_xfree( hb_set.HB_SET_ALTFILE );
                  hb_set.HB_SET_ALTFILE = NULL;
               }
            }
            else
            {
               hb_set.HB_SET_ALTFILE = set_string( pArg2, hb_set.HB_SET_ALTFILE );
            }
         }

         if( args > 2 )
         {
            bFlag = set_logical( pArg3, FALSE );
         }
         else
         {
            bFlag = FALSE;
         }

         if( args > 1 )
         {
            close_text( hb_set.hb_set_althan );

            if( hb_set.HB_SET_ALTFILE && strlen( hb_set.HB_SET_ALTFILE ) > 0 )
            {
               hb_set.hb_set_althan = open_handle( hb_set.HB_SET_ALTFILE, bFlag, ".txt", HB_SET_ALTFILE );
            }
            else
            {
               hb_set.hb_set_althan = FS_ERROR;
            }
         }
         break;

      case HB_SET_AUTOPEN    :
         hb_retl( hb_set.HB_SET_AUTOPEN );
         if( args > 1 )
         {
            hb_set.HB_SET_AUTOPEN = set_logical( pArg2, hb_set.HB_SET_AUTOPEN );
         }
         break;

      case HB_SET_AUTORDER   :
         hb_retni( hb_set.HB_SET_AUTORDER );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_AUTORDER ) < 0 )
            {
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            }
            else
            {
               hb_set.HB_SET_AUTORDER = set_number( pArg2, hb_set.HB_SET_AUTORDER );
            }
         }
         break;

      case HB_SET_AUTOSHARE  :
         hb_retni( hb_set.HB_SET_AUTOSHARE );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_AUTOSHARE ) < 0 )
            {
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            }
            else
            {
               hb_set.HB_SET_AUTOSHARE = set_number( pArg2, hb_set.HB_SET_AUTOSHARE );
            }
         }
         break;

      case HB_SET_BELL       :
         hb_retl( hb_set.HB_SET_BELL );
         if( args > 1 )
         {
            hb_set.HB_SET_BELL = set_logical( pArg2, hb_set.HB_SET_BELL );
         }
         break;

      case HB_SET_CANCEL     :
         hb_retl( hb_set.HB_SET_CANCEL );
         if( args > 1 )
         {
            hb_set.HB_SET_CANCEL = set_logical( pArg2, hb_set.HB_SET_CANCEL );
         }
         break;

      case HB_SET_COLOR      :
         hb_retc( hb_conSetColor( args >= 2 && HB_IS_STRING( pArg2 ) ? pArg2->item.asString.value : ( char * ) NULL ) );
         break;

      case HB_SET_CONFIRM    :
         hb_retl( hb_set.HB_SET_CONFIRM );
         if( args > 1 )
         {
            hb_set.HB_SET_CONFIRM = set_logical( pArg2, hb_set.HB_SET_CONFIRM );
         }
         break;

      case HB_SET_CONSOLE    :
         hb_retl( hb_set.HB_SET_CONSOLE );
         if( args > 1 )
         {
            hb_set.HB_SET_CONSOLE = set_logical( pArg2, hb_set.HB_SET_CONSOLE );
         }
         break;

      case HB_SET_CURSOR     :
         if( args >= 2 && HB_IS_NUMERIC( pArg2 ) )
            hb_retni( hb_conSetCursor( TRUE, hb_itemGetNI( pArg2 ) ) );
         else
            hb_retni( hb_conSetCursor( FALSE, 0 ) );
         break;

      case HB_SET_DATEFORMAT :
         if( hb_set.HB_SET_DATEFORMAT )
         {
            hb_retc( hb_set.HB_SET_DATEFORMAT );
         }
         else
         {
            hb_retc( NULL );
         }

         if( args > 1 )
         {
            BOOL flag = FALSE;
            UINT i;
            int ch, year = 0;

            hb_set.HB_SET_DATEFORMAT = set_string( pArg2, hb_set.HB_SET_DATEFORMAT );

            for( i = 0; i < strlen( hb_set.HB_SET_DATEFORMAT ); i++ )
            {
               ch = hb_set.HB_SET_DATEFORMAT[i];

               if( !flag && ( ch == 'Y' || ch == 'y' ) )
               {
                  year++;   /* Only count the first set of consecutive "Y"s. */
               }
               else if( year )
               {
                  flag = TRUE; /* Indicate non-consecutive. */
               }
            }

            flag = ( year >= 4 );

            if( flag != hb_set.hb_set_century )
            {
               /* CENTURY is not controlled directly by SET, so there is no
                  notification for changing it indirectly via DATE FORMAT. */
               hb_set.hb_set_century = flag;
            }
         }
         break;

      case HB_SET_DEBUG      :
         hb_retl( hb_set.HB_SET_DEBUG );
         if( args > 1 )
         {
            hb_set.HB_SET_DEBUG = set_logical( pArg2, hb_set.HB_SET_DEBUG );
         }
         break;

      case HB_SET_DECIMALS   :
         hb_retni( hb_set.HB_SET_DECIMALS );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_DECIMALS ) < 0 )
            {
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            }
            else
            {
               hb_set.HB_SET_DECIMALS = set_number( pArg2, hb_set.HB_SET_DECIMALS );
            }
         }
         break;

      case HB_SET_DEFAULT    :
         if( hb_set.HB_SET_DEFAULT ) hb_retc( hb_set.HB_SET_DEFAULT );
         else hb_retc( NULL );
         if( args > 1 ) hb_set.HB_SET_DEFAULT = set_string( pArg2, hb_set.HB_SET_DEFAULT );
         break;

      case HB_SET_DELETED    :
         hb_retl( hb_set.HB_SET_DELETED );
         if( args > 1 )
         {
            hb_set.HB_SET_DELETED = set_logical( pArg2, hb_set.HB_SET_DELETED );
         }
         break;

      case HB_SET_DELIMCHARS :
         if( hb_set.HB_SET_DELIMCHARS ) hb_retc( hb_set.HB_SET_DELIMCHARS );
         else hb_retc( NULL );
         if( args > 1 ) hb_set.HB_SET_DELIMCHARS = set_string( pArg2, hb_set.HB_SET_DELIMCHARS );
         break;

      case HB_SET_DELIMITERS :
         hb_retl( hb_set.HB_SET_DELIMITERS );
         if( args > 1 )
         {
            hb_set.HB_SET_DELIMITERS = set_logical( pArg2, hb_set.HB_SET_DELIMITERS );
         }
         break;

      case HB_SET_DEVICE     :
         if( hb_set.HB_SET_DEVICE )
         {
            hb_retc( hb_set.HB_SET_DEVICE );
         }
         else
         {
            hb_retc( NULL );
         }

         if( args > 1 && ! HB_IS_NIL( pArg2 ) )
         {
            hb_set.HB_SET_DEVICE = set_string( pArg2, hb_set.HB_SET_DEVICE );

            /* If the print file is not already open, open it in overwrite mode. */
            if( hb_stricmp( hb_set.HB_SET_DEVICE, "PRINTER" ) == 0 )
            {
               hb_set_SetPrinterStart();
            }
            else
            {
               hb_set_SetPrinterStop();
            }

         }
         break;
      case HB_SET_EOF        :
         hb_retl( hb_set.HB_SET_EOF );
         if( args > 1 ) hb_set.HB_SET_EOF = set_logical( pArg2, hb_set.HB_SET_EOF );
         break;
      case HB_SET_EPOCH      :
         hb_retni( hb_set.HB_SET_EPOCH );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_EPOCH ) < 0 )
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            else
               hb_set.HB_SET_EPOCH = set_number( pArg2, hb_set.HB_SET_EPOCH );
         }
         break;

      case HB_SET_ESCAPE     :
         hb_retl( hb_set.HB_SET_ESCAPE );
         if( args > 1 )
         {
            hb_set.HB_SET_ESCAPE = set_logical( pArg2, hb_set.HB_SET_ESCAPE );
         }
         break;

      case HB_SET_EVENTMASK  :
         hb_retni( hb_set.HB_SET_EVENTMASK );
         if( args > 1 ) hb_set.HB_SET_EVENTMASK = set_number( pArg2, hb_set.HB_SET_EVENTMASK );
         break;

      case HB_SET_EXACT      :
         hb_retl( hb_set.HB_SET_EXACT );
         if( args > 1 )
         {
            hb_set.HB_SET_EXACT = set_logical( pArg2, hb_set.HB_SET_EXACT );
         }
         break;

      case HB_SET_EXCLUSIVE  :
         hb_retl( hb_set.HB_SET_EXCLUSIVE );
         if( args > 1 )
         {
            hb_set.HB_SET_EXCLUSIVE = set_logical( pArg2, hb_set.HB_SET_EXCLUSIVE );
         }
         break;

      case HB_SET_EXIT       :
         hb_retl( hb_set.HB_SET_EXIT );
         if( args > 1 )
         {
            hb_set.HB_SET_EXIT = set_logical( pArg2, hb_set.HB_SET_EXIT );
         }
         break;

      case HB_SET_EXTRA      :
         hb_retl( hb_set.HB_SET_EXTRA );
         if( args > 1 )
         {
            hb_set.HB_SET_EXTRA = set_logical( pArg2, hb_set.HB_SET_EXTRA );
         }
         break;

      case HB_SET_EXTRAFILE  :
         if( hb_set.HB_SET_EXTRAFILE ) hb_retc( hb_set.HB_SET_EXTRAFILE );
         else hb_retc( NULL );
         if( args > 1 )
         {
            if( HB_IS_NIL( pArg2 ) )
            {
               if( hb_set.HB_SET_EXTRAFILE )
               {
                  hb_xfree( hb_set.HB_SET_EXTRAFILE );
                  hb_set.HB_SET_EXTRAFILE = NULL;
               }
            }
            else
            {
               hb_set.HB_SET_EXTRAFILE = set_string( pArg2, hb_set.HB_SET_EXTRAFILE );
            }
         }

         if( args > 2 )
         {
            bFlag = set_logical( pArg3, FALSE );
         }
         else
         {
            bFlag = FALSE;
         }

         if( args > 1 && ! HB_IS_NIL( pArg2 ) )
         {
            close_text( hb_set.hb_set_extrahan );

            if( hb_set.HB_SET_EXTRAFILE && strlen( hb_set.HB_SET_EXTRAFILE ) > 0 )
            {
               hb_set.hb_set_extrahan = open_handle( hb_set.HB_SET_EXTRAFILE, bFlag, ".prn", HB_SET_EXTRAFILE );
            }
            else
            {
               hb_set.hb_set_extrahan = FS_ERROR;
            }
         }
         break;

      case HB_SET_FIXED      :
         hb_retl( hb_set.HB_SET_FIXED );
         if( args > 1 )
         {
            hb_set.HB_SET_FIXED = set_logical( pArg2, hb_set.HB_SET_FIXED );
         }
         break;

      case HB_SET_GTMODE      :
         hb_retni( hb_set.HB_SET_GTMODE );
         if ( args > 1 )
         {
            if ( HB_IS_STRING( pArg2 ) )
            {
               if ( hb_stricmp( pArg2->item.asString.value, "DETACHED" ) == 0 )
               {
                  hb_set.HB_SET_GTMODE = 1;
                  break;
               }
               else if ( hb_stricmp( pArg2->item.asString.value, "INLINE" ) == 0 )
               {
                  hb_set.HB_SET_GTMODE = 0;
                  break;
               }
            }
            hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
         }
         break;

      case HB_SET_INSERT     :
         hb_retl( hb_set.HB_SET_INSERT );
         if( args > 1 )
         {
            hb_set.HB_SET_INSERT = set_logical( pArg2, hb_set.HB_SET_INSERT );
         }
         break;

      case HB_SET_INTENSITY  :
         hb_retl( hb_set.HB_SET_INTENSITY );
         if( args > 1 )
         {
            hb_set.HB_SET_INTENSITY = set_logical( pArg2, hb_set.HB_SET_INTENSITY );
         }
         break;

      case HB_SET_MARGIN     :
         hb_retni( hb_set.HB_SET_MARGIN );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_MARGIN ) < 0 )
            {
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            }
            else
            {
               hb_set.HB_SET_MARGIN = set_number( pArg2, hb_set.HB_SET_MARGIN );
            }
         }
         break;

      case HB_SET_MBLOCKSIZE :
         hb_retni( hb_set.HB_SET_MBLOCKSIZE );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_MBLOCKSIZE ) < 0 )
            {
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            }
            else
            {
               hb_set.HB_SET_MBLOCKSIZE = set_number( pArg2, hb_set.HB_SET_MBLOCKSIZE );
            }
         }
         break;

      case HB_SET_MCENTER    :
         hb_retl( hb_set.HB_SET_MCENTER );
         if( args > 1 )
         {
            hb_set.HB_SET_MCENTER = set_logical( pArg2, hb_set.HB_SET_MCENTER );
         }
         break;

      case HB_SET_MESSAGE    :
         hb_retni( hb_set.HB_SET_MESSAGE );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_MESSAGE ) < 0 )
            {
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            }
            else
            {
               hb_set.HB_SET_MESSAGE = set_number( pArg2, hb_set.HB_SET_MESSAGE );
            }
         }
         break;

      case HB_SET_MFILEEXT   :
         if( hb_set.HB_SET_MFILEEXT )
         {
            hb_retc( hb_set.HB_SET_MFILEEXT );
         }
         else
         {
            hb_retc( NULL );
         }

         if( args > 1 )
         {
            hb_set.HB_SET_MFILEEXT = set_string( pArg2, hb_set.HB_SET_MFILEEXT );
         }
         break;

      case HB_SET_OPTIMIZE   :
         hb_retl( hb_set.HB_SET_OPTIMIZE );
         if( args > 1 )
         {
            hb_set.HB_SET_OPTIMIZE = set_logical( pArg2, hb_set.HB_SET_OPTIMIZE );
         }
         break;

      case HB_SET_FORCEOPT   :
         hb_retl( hb_set.HB_SET_FORCEOPT );
         if( args > 1 )
         {
            hb_set.HB_SET_FORCEOPT = set_logical( pArg2, hb_set.HB_SET_FORCEOPT );
         }
         break;

      case HB_SET_STRICTREAD :
         hb_retl( hb_set.HB_SET_STRICTREAD );
         if( args > 1 )
         {
            hb_set.HB_SET_STRICTREAD = set_logical( pArg2, hb_set.HB_SET_STRICTREAD );
         }
         break;

      case HB_SET_HARDCOMMIT :
         hb_retl( hb_set.HB_SET_HARDCOMMIT );
         if( args > 1 )
         {
            hb_set.HB_SET_HARDCOMMIT = set_logical( pArg2, hb_set.HB_SET_HARDCOMMIT );
         }
         break;

      case HB_SET_PATH       :
         if( hb_set.HB_SET_PATH )
         {
            hb_retc( hb_set.HB_SET_PATH );
         }
         else
         {
            hb_retc( NULL );
         }

         if( args > 1 )
         {
            hb_setFreeSetPath();
            hb_set.HB_SET_PATH = set_string( pArg2, hb_set.HB_SET_PATH );
            hb_fsAddSearchPath( hb_set.HB_SET_PATH, &sp_set_path );
         }
         break;

      case HB_SET_PRINTER    :
         hb_retl( hb_set.HB_SET_PRINTER );
         if( args > 1 )
         {
            hb_set.HB_SET_PRINTER = set_logical( pArg2, hb_set.HB_SET_PRINTER );

            if( hb_set.HB_SET_PRINTER )
            {
               hb_set_SetPrinterStart();
            }
            else
            {
               hb_set_SetPrinterStop();
            }
         }
         break;

      case HB_SET_PRINTFILE  :
      {
         if( hb_set.HB_SET_PRINTFILE )
         {
            hb_retc( hb_set.HB_SET_PRINTFILE );
         }
         else
         {
            hb_retc( NULL );
         }

         if( args > 2 )
         {
            bFlag = set_logical( pArg3, FALSE );
         }
         else
         {
            bFlag = FALSE;
         }

         if( args > 1 && ! HB_IS_NIL( pArg2 ) )
         {
            hb_set.HB_SET_PRINTFILE = set_string( pArg2, hb_set.HB_SET_PRINTFILE );

            close_binary( hb_set.hb_set_printhan );
            hb_set.hb_set_printhan = FS_ERROR;

            if( hb_set.HB_SET_PRINTFILE && strlen( hb_set.HB_SET_PRINTFILE ) > 0 )
            {
               hb_set.hb_set_printhan = open_handle( hb_set.HB_SET_PRINTFILE, bFlag, ".prn", HB_SET_PRINTFILE );
               s_isDefaultPrinterDevice = FALSE;
            }
            else
            {
               hb_set_SetDefaultPrinter(); // Make sure there is a default print file name "PRN"
            }
         }
      }
      break;

      case HB_SET_SCOREBOARD :
         hb_retl( hb_set.HB_SET_SCOREBOARD );
         if( args > 1 )
         {
            hb_set.HB_SET_SCOREBOARD = set_logical( pArg2, hb_set.HB_SET_SCOREBOARD );
         }
         break;

      case HB_SET_SCROLLBREAK:
         hb_retl( hb_set.HB_SET_SCROLLBREAK );
         if( args > 1 )
         {
            hb_set.HB_SET_SCROLLBREAK = set_logical( pArg2, hb_set.HB_SET_SCROLLBREAK );
         }
         break;

      case HB_SET_SOFTSEEK   :
         hb_retl( hb_set.HB_SET_SOFTSEEK );
         if( args > 1 )
         {
            hb_set.HB_SET_SOFTSEEK = set_logical( pArg2, hb_set.HB_SET_SOFTSEEK );
         }
         break;

      case HB_SET_TYPEAHEAD  :
         hb_retni( hb_set.HB_SET_TYPEAHEAD );

         if( args > 1 )
         {
            /* Set the value and limit the range */
            int old = hb_set.HB_SET_TYPEAHEAD;

            hb_set.HB_SET_TYPEAHEAD = set_number( pArg2, old );

            if( hb_set.HB_SET_TYPEAHEAD == 0 )
            {
                 /* Do nothing */ ;
            }
            else if( hb_set.HB_SET_TYPEAHEAD < 16 )
            {
                hb_set.HB_SET_TYPEAHEAD = 16;
            }
            else if( hb_set.HB_SET_TYPEAHEAD > 4096 )
            {
                hb_set.HB_SET_TYPEAHEAD = 4096;
            }
            /* reset keyboard buffer */
            hb_inkeyReset();
         }
         break;

      case HB_SET_UNIQUE     :
         hb_retl( hb_set.HB_SET_UNIQUE );
         if( args > 1 )
         {
            hb_set.HB_SET_UNIQUE = set_logical( pArg2, hb_set.HB_SET_UNIQUE );
         }
         break;

      case HB_SET_VIDEOMODE  :
         hb_retni( hb_set.HB_SET_VIDEOMODE );

         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_VIDEOMODE ) < 0 )
            {
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            }
            else
            {
               hb_set.HB_SET_VIDEOMODE = set_number( pArg2, hb_set.HB_SET_VIDEOMODE );
            }
         }
         break;

      case HB_SET_WRAP       :
         hb_retl( hb_set.HB_SET_WRAP );
         if( args > 1 )
         {
            hb_set.HB_SET_WRAP = set_logical( pArg2, hb_set.HB_SET_WRAP );
         }
         break;

      case HB_SET_LANGUAGE   :
         hb_retc( hb_langID() );
         if( args > 1 && ! HB_IS_NIL( pArg2 ) )
         {
            hb_langSelectID( pArg2->item.asString.value );
         }
         break;

      case HB_SET_IDLEREPEAT :
         hb_retl( hb_set.HB_SET_IDLEREPEAT );
         if( args > 1 )
         {
            hb_set.HB_SET_IDLEREPEAT = set_logical( pArg2, hb_set.HB_SET_IDLEREPEAT );
         }
         break;

       case HB_SET_TRACE :
          hb_retl( hb_set.HB_SET_TRACE );
          if( args > 1 )
          {
             hb_set.HB_SET_TRACE = set_logical( pArg2, hb_set.HB_SET_TRACE );
          }
          break;

       case HB_SET_TRACEFILE :
          hb_retc( (char *) ( hb_set.HB_SET_TRACEFILE ) );

          if( args > 1 && HB_IS_STRING( pArg2 ) )
          {
             FILE *fpTrace;
             BOOL bAppend = FALSE;

             strcpy( hb_set.HB_SET_TRACEFILE, pArg2->item.asString.value );

             /* Create trace.log for tracing. */
             if( args > 2 && HB_IS_LOGICAL( pArg3 ) )
             {
                bAppend = pArg3->item.asLogical.value;
             }

             if( bAppend )
             {
                fpTrace = hb_fopen( (char *) (hb_set.HB_SET_TRACEFILE), "a" );
             }
             else
             {
                fpTrace = hb_fopen( (char *) (hb_set.HB_SET_TRACEFILE), "w" );
             }

             if( fpTrace )
             {
                fclose( fpTrace );
             }
             else
             {
                //hb_errInternal( HB_EI_ERRUNRECOV, "Unable to create trace.log file", NULL, NULL );
             }
          }
          break;

      case HB_SET_TRACESTACK :
         hb_retni( hb_set.HB_SET_TRACESTACK );
         if( args > 1 )
         {
            if( HB_IS_STRING( pArg2 ) )
            {
               if( ! hb_stricmp( pArg2->item.asString.value, "NONE" ) )
               {
                  hb_set.HB_SET_TRACESTACK = HB_SET_TRACESTACK_NONE;
               }
               else if( ! hb_stricmp( pArg2->item.asString.value, "CURRENT" ) )
               {
                  hb_set.HB_SET_TRACESTACK = HB_SET_TRACESTACK_CURRENT;
               }
               else if( ! hb_stricmp( pArg2->item.asString.value, "ALL" ) )
               {
                  hb_set.HB_SET_TRACESTACK = HB_SET_TRACESTACK_ALL;
               }
               else
               {
                  hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
               }
            }
            else if( HB_IS_NUMERIC( pArg2 ) )
            {
               if( set_number( pArg2, hb_set.HB_SET_TRACESTACK ) < 0 )
               {
                  hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
               }
               else
               {
                  hb_set.HB_SET_TRACESTACK = set_number( pArg2, hb_set.HB_SET_TRACESTACK );
               }
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            }
         }
         break;
      case HB_SET_PRINTERJOB    :
         if ( hb_set.hb_set_printerjob )
         {
           hb_retc( hb_set.hb_set_printerjob );
         }
         else
         {
           hb_retc( "" ) ;
         }
         if ( args > 1 && ISCHAR( 2 ) )
         {
           ULONG ulLength = hb_parclen( 2 ) ;
           if ( hb_set.hb_set_printerjob )
           {
              hb_xfree( hb_set.hb_set_printerjob ) ;
              hb_set.hb_set_printerjob = NULL ;
           }

           if ( ulLength > 0 )
           {
             ulLength++ ;    // Add on space for '\0'
             hb_set.hb_set_printerjob = ( char *) hb_xgrab( ulLength ) ;
             memcpy( hb_set.hb_set_printerjob, hb_parc( 2 ), ulLength ) ;
           }
         }
         break;
      case HB_SET_FILECASE :
         hb_retni( hb_set.HB_SET_FILECASE );
         if( args > 1 )
         {
            if( HB_IS_STRING( pArg2 ) )
            {
               if( ! hb_stricmp( pArg2->item.asString.value, "LOWER" ) )
               {
                  hb_set.HB_SET_FILECASE = HB_SET_CASE_LOWER;
               }
               else if( ! hb_stricmp( pArg2->item.asString.value, "UPPER" ) )
               {
                  hb_set.HB_SET_FILECASE = HB_SET_CASE_UPPER;
               }
               else if( ! hb_stricmp( pArg2->item.asString.value, "MIXED" ) )
               {
                  hb_set.HB_SET_FILECASE = HB_SET_CASE_MIXED;
               }
               else
               {
                  hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
               }
            }
            else if( HB_IS_NUMERIC( pArg2 ) )
            {
               if( set_number( pArg2, hb_set.HB_SET_FILECASE ) < 0 )
               {
                  hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
               }
               else
               {
                  hb_set.HB_SET_FILECASE = set_number( pArg2, hb_set.HB_SET_FILECASE );
               }
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            }
         }
         break;

      case HB_SET_DIRCASE :
         hb_retni( hb_set.HB_SET_DIRCASE );
         if( args > 1 )
         {
            if( HB_IS_STRING( pArg2 ) )
            {
               if( ! hb_stricmp( pArg2->item.asString.value, "LOWER" ) )
               {
                  hb_set.HB_SET_DIRCASE = HB_SET_CASE_LOWER;
               }
               else if( ! hb_stricmp( pArg2->item.asString.value, "UPPER" ) )
               {
                  hb_set.HB_SET_DIRCASE = HB_SET_CASE_UPPER;
               }
               else if( ! hb_stricmp( pArg2->item.asString.value, "MIXED" ) )
               {
                  hb_set.HB_SET_DIRCASE = HB_SET_CASE_MIXED;
               }
               else
               {
                  hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
               }
            }
            else if( HB_IS_NUMERIC( pArg2 ) )
            {
               if( set_number( pArg2, hb_set.HB_SET_DIRCASE ) < 0 )
               {
                  hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
               }
               else
               {
                  hb_set.HB_SET_DIRCASE = set_number( pArg2, hb_set.HB_SET_DIRCASE );
               }
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            }
         }
         break;

      case HB_SET_DIRSEPARATOR :
         hb_dirsep_string[0] = hb_set.HB_SET_DIRSEPARATOR;
         hb_dirsep_string[1] = '\0';
         hb_retc( hb_dirsep_string );
         if( args > 1 )
         {
            hb_set.HB_SET_DIRSEPARATOR = set_char( pArg2, hb_set.HB_SET_DIRSEPARATOR );
         }
         break;

      case HB_SET_ERRORLOOP :
         hb_retni( hb_set.HB_SET_ERRORLOOP );
         if( args > 1 )
         {
            if( HB_IS_NUMERIC( pArg2 ) )
            {
               hb_set.HB_SET_ERRORLOOP = hb_itemGetNI( pArg2 );

               if( hb_set.HB_SET_ERRORLOOP < 0 )
               {
                  hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
               }
               else if( hb_set.HB_SET_ERRORLOOP == 0 )
               {
                  hb_set.HB_SET_ERRORLOOP = 8;
               }
            }
            else
            {
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            }
         }
         break;

      case HB_SET_OUTPUTSAFETY:
         hb_retl( hb_set.HB_SET_OUTPUTSAFETY );
         if ( args > 1 )
         {
            hb_set.HB_SET_OUTPUTSAFETY = set_logical(pArg2, TRUE );
         }
         break;

      case HB_SET_DBFLOCKSCHEME:
         hb_retni( hb_set.HB_SET_DBFLOCKSCHEME );
         if( args > 1 )
         {
            if( set_number( pArg2, hb_set.HB_SET_DBFLOCKSCHEME ) < 0 )
            {
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            }
            else
            {
               hb_set.HB_SET_DBFLOCKSCHEME = set_number( pArg2, hb_set.HB_SET_DBFLOCKSCHEME );
            }
         }
         break;

      case HB_SET_BACKGROUNDTASKS:
         hb_retl( hb_set.HB_SET_BACKGROUNDTASKS );
         if( args > 1 )
         {
            hb_set.HB_SET_BACKGROUNDTASKS = set_logical( pArg2, hb_set.HB_SET_BACKGROUNDTASKS );
         }
         break;

      case HB_SET_TRIMFILENAME:
         hb_retl( hb_set.HB_SET_TRIMFILENAME );
         if( args > 1 )
         {
            hb_set.HB_SET_TRIMFILENAME = set_logical( pArg2, hb_set.HB_SET_TRIMFILENAME );
         }
         break;

      case HB_SET_BACKGROUNDTICK:
         hb_retnl( hb_set.HB_SET_BACKGROUNDTICK );
         if( args > 1 )
         {
            int iNewVal = set_number( pArg2, hb_set.HB_SET_BACKGROUNDTICK );

            if( iNewVal < 0 )
            {
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
            }
            else
            {
               hb_set.HB_SET_BACKGROUNDTICK = iNewVal == 0 ? 1000 : iNewVal;
            }
         }
         break;

      case HB_SET_EOL:
         if( !hb_set.HB_SET_EOL )
         {
            hb_set.HB_SET_EOL = hb_itemPutC( NULL, hb_conNewLine() );
         }
         hb_itemReturn( hb_set.HB_SET_EOL );
         if( args > 1 )
         {
            hb_itemClear( hb_set.HB_SET_EOL );
            hb_itemCopy( hb_set.HB_SET_EOL, pArg2 );
         }
         break;

      case HB_SET_ERRORLOG    :
         hb_reta( 2 );
         hb_storc( (char *) ( hb_set.HB_SET_ERRORLOG ), -1, 1 );
         hb_storl( hb_set.HB_SET_APPENDERROR, -1, 2 );
/*
         if( args > 1 && HB_IS_STRING( pArg2 ) )
         {
            strcpy( hb_set.HB_SET_ERRORLOG, pArg2->item.asString.value );

            if( args > 2 && HB_IS_LOGICAL( pArg3 ) )
            {
               hb_set.HB_SET_APPENDERROR = pArg3->item.asLogical.value;
            }
         }
*/
         if( args > 1 )
         {
            if( HB_IS_STRING( pArg2 ) )
            {
               strcpy( hb_set.HB_SET_ERRORLOG, pArg2->item.asString.value );
            }

            if( pArg3 && HB_IS_LOGICAL( pArg3 ) )
            {
               hb_set.HB_SET_APPENDERROR = pArg3->item.asLogical.value;
            }
         }
         break;

      case HB_SET_TIMEFORMAT :
         if( hb_set.HB_SET_TIMEFORMAT )
         {
            hb_retc( hb_set.HB_SET_TIMEFORMAT );
         }
         else
         {
            hb_retc( NULL );
         }

         if( args > 1 )
         {
            hb_set.HB_SET_TIMEFORMAT = set_string( pArg2, hb_set.HB_SET_TIMEFORMAT );
         }
         break;

      case HB_SET_MACROBLOCKVARS:
         if( pArg3 && HB_IS_BLOCK( pArg3 ) )
         {
            if( !pArg3->item.asBlock.value->bDynamic )
            {
               hb_errRT_BASE( EG_ARG, 2020, NULL, "SET", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
               break;
            }

            hb_retl( pArg3->item.asBlock.value->bPrivVars );
            pArg3->item.asBlock.value->bPrivVars = set_logical( pArg2, pArg3->item.asBlock.value->bPrivVars );
         }
         else
         {
            hb_retl( hb_set.HB_SET_MACROBLOCKVARS );
            if( args > 1 )
            {
               hb_set.HB_SET_MACROBLOCKVARS = set_logical( pArg2, hb_set.HB_SET_MACROBLOCKVARS );
            }
         }
         break;

      default:
         /* Return NIL if called with invalid SET specifier */
         break;
   }

   if( args > 1 )
   {
      hb_setListenerNotify( set_specifier, HB_SET_LISTENER_AFTER );
   }
}

/* Listener test (1 of 2)
static void test_callback( HB_set_enum set, HB_set_listener_enum when )
{
   printf("\ntest_callback( %d, %d )", set, when);
}
End listener test (1 of 2) */

void hb_setInitialize( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_setInitialize()"));

   hb_set.HB_SET_ALTERNATE = FALSE;
   hb_set.HB_SET_ALTFILE = NULL;
   hb_set.hb_set_althan = FS_ERROR;
   hb_set.HB_SET_AUTOPEN = TRUE;
   hb_set.HB_SET_AUTORDER = 0;
   hb_set.HB_SET_AUTOSHARE = 0;
   hb_set.HB_SET_BELL = FALSE;
   hb_set.HB_SET_CANCEL = TRUE;
   hb_set.hb_set_century = FALSE;

   strncpy( hb_set.HB_SET_COLOR, "W/N,N/W,N/N,N/N,N/W", sizeof( hb_set.HB_SET_COLOR ) );

   hb_set.HB_SET_COLOR[ sizeof( hb_set.HB_SET_COLOR ) - 1 ] = '\0';
   hb_set.HB_SET_CONFIRM = FALSE;
   hb_set.HB_SET_CONSOLE = TRUE;
   hb_set.HB_SET_DATEFORMAT = ( char * ) hb_xgrab( 9 );

   memcpy( hb_set.HB_SET_DATEFORMAT, "mm/dd/yy", 9 );

   /* hb_set.HB_SET_DEBUG = TRUE; */
   hb_set.HB_SET_DEBUG = hb_dynsymFind( "__DBGENTRY" ) ? TRUE : FALSE;
   hb_set.HB_SET_DECIMALS = 2;
   hb_set.HB_SET_DEFAULT = ( char * ) hb_xgrab( 1 );
   hb_set.HB_SET_DEFAULT[ 0 ] = '\0';
   hb_set.HB_SET_DELETED = FALSE;
   hb_set.HB_SET_DELIMCHARS = ( char * ) hb_xgrab( 3 );

   memcpy( hb_set.HB_SET_DELIMCHARS, "::", 3 );

   hb_set.HB_SET_DELIMITERS = FALSE;
   hb_set.HB_SET_DEVICE = ( char * ) hb_xgrab( 7 );

   memcpy( hb_set.HB_SET_DEVICE, "SCREEN", 7 );

#if defined(OS_UNIX_COMPATIBLE)
   hb_set.HB_SET_EOF = FALSE;
#else
   hb_set.HB_SET_EOF = TRUE;
#endif
   hb_set.HB_SET_EPOCH = 1900;
   hb_set.HB_SET_ESCAPE = TRUE;
   hb_set.HB_SET_EVENTMASK = INKEY_KEYBOARD;
   hb_set.HB_SET_EXACT = FALSE;
   hb_set.HB_SET_EXCLUSIVE = TRUE;
   hb_set.HB_SET_EXIT = FALSE;
   hb_set.HB_SET_EXTRA = FALSE;
   hb_set.HB_SET_EXTRAFILE = NULL;
   hb_set.hb_set_extrahan = FS_ERROR;
   hb_set.HB_SET_FIXED = FALSE;
   hb_set.HB_SET_GTMODE = 1;
   hb_set.HB_SET_IDLEREPEAT = TRUE;
   hb_set.HB_SET_INSERT = FALSE;
   hb_set.HB_SET_INTENSITY = TRUE;
   hb_set.HB_SET_MARGIN = 0;
   hb_set.HB_SET_MBLOCKSIZE = 0;
   hb_set.HB_SET_MCENTER = FALSE;
   hb_set.HB_SET_MESSAGE = 0;
   hb_set.HB_SET_MFILEEXT = ( char * ) hb_xgrab( 1 );
   hb_set.HB_SET_MFILEEXT[ 0 ] = '\0';
   hb_set.HB_SET_PATH = ( char * ) hb_xgrab( 1 );
   hb_set.HB_SET_PATH[ 0 ] = '\0';
   hb_set.HB_SET_PRINTER = FALSE;

   hb_set.HB_SET_PRINTFILE = NULL;
   hb_set_SetDefaultPrinter();

   hb_set.hb_set_printhan = FS_ERROR;
   hb_set.hb_set_winhan = FS_ERROR;
   hb_set.HB_SET_SCOREBOARD = TRUE;
   hb_set.HB_SET_SCROLLBREAK = TRUE;
   hb_set.HB_SET_SOFTSEEK = FALSE;
   hb_set.HB_SET_OPTIMIZE = TRUE;
   hb_set.HB_SET_FORCEOPT = FALSE;
   hb_set.HB_SET_STRICTREAD = FALSE;
   hb_set.HB_SET_HARDCOMMIT = TRUE;
   hb_set.HB_SET_TRACE = TRUE; /* Default Trace to ON */

   strcpy( (char *) (hb_set.HB_SET_TRACEFILE), "trace.log" );

   strcpy( (char *) (hb_set.HB_SET_ERRORLOG), "error.log" );
   hb_set.HB_SET_APPENDERROR = FALSE;

   hb_set.HB_SET_TRACESTACK = HB_SET_TRACESTACK_ALL;
   hb_set.HB_SET_TYPEAHEAD = HB_DEFAULT_INKEY_BUFSIZE;
   hb_set.HB_SET_UNIQUE = FALSE;
   hb_set.HB_SET_VIDEOMODE = 0;

   hb_set.HB_SET_FILECASE = HB_SET_CASE_MIXED;
   hb_set.HB_SET_DIRCASE = HB_SET_CASE_MIXED;

#if defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE )
   hb_set.HB_SET_DIRSEPARATOR = '/';
#else
   hb_set.HB_SET_DIRSEPARATOR = '\\';
#endif

   hb_set.HB_SET_ERRORLOOP = 8;

/* JC1: Set for output thread safety */
   hb_set.HB_SET_OUTPUTSAFETY = TRUE;

   hb_set.HB_SET_DBFLOCKSCHEME = 0;
   hb_set.HB_SET_BACKGROUNDTASKS = FALSE;
   hb_set.HB_SET_BACKGROUNDTICK = 1000;
   hb_set.HB_SET_TRIMFILENAME = FALSE;

   hb_set.HB_SET_EOL = NULL;

   hb_set.HB_SET_WRAP = FALSE;
   hb_set.hb_set_winprinter=FALSE;
   hb_set.hb_set_printerjob=NULL;
   hb_set.hb_set_winprinter=FALSE;

   hb_set.HB_SET_TIMEFORMAT = ( char * ) hb_xgrab( 12 );

   memcpy( hb_set.HB_SET_TIMEFORMAT, "hh:mm:ss.cc", 12 );

   hb_set.HB_SET_MACROBLOCKVARS = FALSE;

   sp_sl_first = sp_sl_last = NULL;
   s_next_listener = 1;

   /* Listener test (2 of 2)
   {
      int temp = hb_setListenerAdd( test_callback );
      printf("\nSet listener test handle is %d", temp);
      {
         int temp2 = hb_setListenerAdd( test_callback );
         printf("\nSet listener test handle is %d", temp2);
         temp2 = hb_setListenerRemove( temp2 );
         printf("\nSet listener remove result is %d", temp2);
      }
   }
   End listener test (2 of 2) */
}

void hb_setRelease( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_setRelease()"));

   close_text( hb_set.hb_set_althan );
   close_text( hb_set.hb_set_extrahan );
   close_binary( hb_set.hb_set_printhan );

   if( hb_set.HB_SET_ALTFILE )    hb_xfree( hb_set.HB_SET_ALTFILE );
   if( hb_set.HB_SET_DATEFORMAT ) hb_xfree( hb_set.HB_SET_DATEFORMAT );
   if( hb_set.HB_SET_DEFAULT )    hb_xfree( hb_set.HB_SET_DEFAULT );
   if( hb_set.HB_SET_DELIMCHARS ) hb_xfree( hb_set.HB_SET_DELIMCHARS );
   if( hb_set.HB_SET_DEVICE )     hb_xfree( hb_set.HB_SET_DEVICE );
   if( hb_set.HB_SET_EXTRAFILE )  hb_xfree( hb_set.HB_SET_EXTRAFILE );
   if( hb_set.HB_SET_MFILEEXT  )  hb_xfree( hb_set.HB_SET_MFILEEXT );
   if( hb_set.HB_SET_PATH )       hb_xfree( hb_set.HB_SET_PATH );
   if( hb_set.HB_SET_PRINTFILE )  hb_xfree( hb_set.HB_SET_PRINTFILE );
   if( hb_set.hb_set_printerjob ) hb_xfree( hb_set.hb_set_printerjob );
   if( hb_set.HB_SET_TIMEFORMAT ) hb_xfree( hb_set.HB_SET_TIMEFORMAT );

   hb_set.HB_SET_TYPEAHEAD = 0;   hb_inkeyReset(); /* reset keyboard buffer */

   while( sp_sl_first )
   {
      /* Free all set listeners */
      sp_sl_last = sp_sl_first->next;
      hb_xfree( sp_sl_first );
      sp_sl_first = sp_sl_last;
   }

   hb_setFreeSetPath();
}

HB_EXPORT int hb_setListenerAdd( HB_SET_LISTENER_CALLBACK * callback )
{
   PHB_SET_LISTENER p_sl = (PHB_SET_LISTENER) hb_xgrab( sizeof( HB_SET_LISTENER ) );
   p_sl->callback = callback;
   p_sl->listener = s_next_listener++;
   p_sl->next = NULL;
   if( sp_sl_last ) sp_sl_last->next = p_sl;
   else if( ! sp_sl_first ) sp_sl_first = p_sl;
   sp_sl_last = p_sl;
   return p_sl->listener;
}

HB_EXPORT void hb_setListenerNotify( HB_set_enum set, HB_set_listener_enum when )
{
   PHB_SET_LISTENER p_sl = sp_sl_first;
   while( p_sl )
   {
      (* p_sl->callback)( set, when );
      p_sl = p_sl->next;
   }
}

HB_EXPORT int hb_setListenerRemove( int listener )
{
   PHB_SET_LISTENER p_sl = sp_sl_first;
   PHB_SET_LISTENER p_sl_prev = NULL;
   while( p_sl )
   {
      if( listener == p_sl->listener )
      {
         listener = -listener;
         if( p_sl_prev ) p_sl_prev->next = p_sl->next;
         else sp_sl_first = p_sl->next;
         if( p_sl == sp_sl_last ) sp_sl_last = p_sl_prev;
         hb_xfree( p_sl );
         p_sl = NULL;
      }
      if( p_sl )
      {
         p_sl_prev = p_sl;
         p_sl = p_sl->next;
      }
   }
   return listener;
}

HB_EXPORT HB_PATHNAMES * hb_setGetFirstSetPath( void )
{
   return sp_set_path;
}

HB_EXPORT FHANDLE hb_setAltHan( void )
{
   return hb_set.hb_set_althan;
}

HB_EXPORT BOOL hb_setCentury( void )
{
   return hb_set.hb_set_century;
}

HB_EXPORT FHANDLE hb_setExtraHan( void )
{
   return hb_set.hb_set_extrahan;
}

HB_EXPORT FHANDLE hb_setPrintHan( void )
{
   return hb_set.hb_set_printhan;
}

HB_EXPORT BOOL hb_setAlternate( void )
{
   return hb_set.HB_SET_ALTERNATE;
}

HB_EXPORT char * hb_setAltFile( void )
{
   return hb_set.HB_SET_ALTFILE;
}

HB_EXPORT BOOL hb_setAutOpen( void )
{
   return hb_set.HB_SET_AUTOPEN;
}

HB_EXPORT int hb_setAutOrder( void )
{
   return hb_set.HB_SET_AUTORDER;
}

HB_EXPORT int hb_setAutoShare( void )
{
   return hb_set.HB_SET_AUTOSHARE;
}

HB_EXPORT BOOL hb_setBell( void )
{
   return hb_set.HB_SET_BELL;
}

HB_EXPORT BOOL hb_setCancel( void )
{
   return hb_set.HB_SET_CANCEL;
}

HB_EXPORT char * hb_setColor( void )
{
   return hb_set.HB_SET_COLOR;
}

HB_EXPORT BOOL hb_setConfirm( void )
{
   return hb_set.HB_SET_CONFIRM;
}

HB_EXPORT BOOL hb_setConsole( void )
{
   return hb_set.HB_SET_CONSOLE;
}

HB_EXPORT char * hb_setDateFormat( void )
{
   return hb_set.HB_SET_DATEFORMAT;
}

HB_EXPORT BOOL hb_setDebug( void )
{
   return hb_set.HB_SET_DEBUG;
}

HB_EXPORT int hb_setDecimals( void )
{
   return hb_set.HB_SET_DECIMALS;
}

HB_EXPORT char * hb_setDefault( void )
{
   return hb_set.HB_SET_DEFAULT;
}

HB_EXPORT BOOL hb_setDeleted( void )
{
   return hb_set.HB_SET_DELETED;
}

HB_EXPORT char * hb_setDelimChars( void )
{
   return hb_set.HB_SET_DELIMCHARS;
}

HB_EXPORT BOOL hb_setDelimiters( void )
{
   return hb_set.HB_SET_DELIMITERS;
}

HB_EXPORT char * hb_setDevice( void )
{
   return hb_set.HB_SET_DEVICE;
}

HB_EXPORT BOOL hb_setEOF( void )
{
   return hb_set.HB_SET_EOF;
}

HB_EXPORT int hb_setEpoch( void )
{
   return hb_set.HB_SET_EPOCH;
}

HB_EXPORT BOOL hb_setEscape( void )
{
   return hb_set.HB_SET_ESCAPE;
}

HB_EXPORT int hb_setEventMask( void )
{
   return hb_set.HB_SET_EVENTMASK;
}

HB_EXPORT BOOL hb_setExact( void )
{
   return hb_set.HB_SET_EXACT;
}

HB_EXPORT BOOL hb_setExclusive( void )
{
   return hb_set.HB_SET_EXCLUSIVE;
}

HB_EXPORT BOOL hb_setExit( void )
{
   return hb_set.HB_SET_EXIT;
}

HB_EXPORT BOOL hb_setExtra( void )
{
   return hb_set.HB_SET_EXTRA;
}

HB_EXPORT char * hb_setExtraFile( void )
{
   return hb_set.HB_SET_EXTRAFILE;
}

HB_EXPORT BOOL hb_setFixed( void )
{
   return hb_set.HB_SET_FIXED;
}

HB_EXPORT BOOL hb_setIdleRepeat( void )
{
   return hb_set.HB_SET_IDLEREPEAT;
}

HB_EXPORT BOOL hb_setInsert( void )
{
   return hb_set.HB_SET_INSERT;
}

HB_EXPORT BOOL hb_setIntensity( void )
{
   return hb_set.HB_SET_INTENSITY;
}

HB_EXPORT char * hb_setPath( void )
{
   return hb_set.HB_SET_PATH;
}

HB_EXPORT int hb_setMargin( void )
{
   return hb_set.HB_SET_MARGIN;
}

HB_EXPORT int hb_setMBlockSize( void )
{
   return hb_set.HB_SET_MBLOCKSIZE;
}

HB_EXPORT BOOL hb_setMCenter( void )
{
   return hb_set.HB_SET_MCENTER;
}

HB_EXPORT int hb_setMessage( void )
{
   return hb_set.HB_SET_MESSAGE;
}

HB_EXPORT char * hb_setMFileExt( void )
{
   return hb_set.HB_SET_MFILEEXT;
}

HB_EXPORT BOOL hb_setOptimize( void )
{
   return hb_set.HB_SET_OPTIMIZE;
}

HB_EXPORT BOOL hb_setPrinter( void )
{
   return hb_set.HB_SET_PRINTER;
}

HB_EXPORT char * hb_setPrintFile( void )
{
   return hb_set.HB_SET_PRINTFILE;
}

HB_EXPORT BOOL hb_setScoreBoard( void )
{
   return hb_set.HB_SET_SCOREBOARD;
}

HB_EXPORT BOOL hb_setScrollBreak( void )
{
   return hb_set.HB_SET_SCROLLBREAK;
}

HB_EXPORT BOOL hb_setSoftSeek( void )
{
   return hb_set.HB_SET_SOFTSEEK;
}

HB_EXPORT BOOL hb_setStrictRead( void )
{
   return hb_set.HB_SET_STRICTREAD;
}

HB_EXPORT int hb_setTypeAhead( void )
{
   return hb_set.HB_SET_TYPEAHEAD;
}

HB_EXPORT BOOL hb_setUnique( void )
{
   return hb_set.HB_SET_UNIQUE;
}

HB_EXPORT int hb_setFileCase( void )
{
   return hb_set.HB_SET_FILECASE;
}

HB_EXPORT int hb_setDirCase( void )
{
   return hb_set.HB_SET_DIRCASE;
}

HB_EXPORT char hb_setDirSeparator( void )
{
   return hb_set.HB_SET_DIRSEPARATOR;
}

HB_EXPORT int hb_setVideoMode( void )
{
   return hb_set.HB_SET_VIDEOMODE;
}

HB_EXPORT BOOL hb_setWrap( void )
{
   return hb_set.HB_SET_WRAP;
}

HB_EXPORT int hb_setDBFLockScheme( void )
{
   return hb_set.HB_SET_DBFLOCKSCHEME;
}

HB_EXPORT BOOL hb_setTrace( void )
{
   return hb_set.HB_SET_TRACE;
}

HB_EXPORT char * hb_setTraceFile( void )
{
   return hb_set.HB_SET_TRACEFILE;
}

HB_EXPORT char hb_setTraceStack( void )
{
   return hb_set.HB_SET_TRACESTACK;
}

HB_EXPORT int hb_setErrorLoop( void )
{
   return hb_set.HB_SET_ERRORLOOP;
}

HB_EXPORT BOOL hb_setOutputSafety( void )
{
   return hb_set.HB_SET_OUTPUTSAFETY;
}

HB_EXPORT BOOL hb_setBackgroundTasks( void )
{
   return hb_set.HB_SET_BACKGROUNDTASKS;
}

HB_EXPORT BOOL hb_setTrimFileName( void )
{
   return hb_set.HB_SET_TRIMFILENAME;
}

HB_EXPORT BOOL hb_setWinPrinter( void )
{
   return hb_set.hb_set_winprinter;
}

HB_EXPORT FHANDLE hb_setWinHan( void )
{
   return hb_set.hb_set_winhan;
}

HB_EXPORT char * hb_setPrinterJob( void )
{
   return hb_set.hb_set_printerjob;
}

HB_EXPORT int hb_setGTMode( void )
{
   return hb_set.HB_SET_GTMODE;
}

HB_EXPORT int hb_setBackGroundTick( void )
{
   return hb_set.HB_SET_BACKGROUNDTICK;
}

HB_EXPORT BOOL hb_setHardCommit( void )
{
   return hb_set.HB_SET_HARDCOMMIT;
}

HB_EXPORT BOOL hb_setForceOpt( void )
{
   return hb_set.HB_SET_FORCEOPT;
}

HB_EXPORT PHB_ITEM hb_setEOL( void )
{
   return hb_set.HB_SET_EOL;
}

HB_EXPORT BOOL hb_setAppendError( void )
{
   return hb_set.HB_SET_APPENDERROR;
}

HB_EXPORT char * hb_setErrorLog( void )
{
   return hb_set.HB_SET_ERRORLOG;
}

HB_EXPORT char * hb_setTimeFormat( void )
{
   return hb_set.HB_SET_TIMEFORMAT;
}
