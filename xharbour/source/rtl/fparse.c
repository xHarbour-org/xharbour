/*
 * $Id: fparse.c,v 1.2 2004/02/18 10:50:45 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * FPARSE()
 *
 * Copyright 2004 Andi Jahja <xharbour@cbn.net.id>
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
FPARSE( cFile, cDelimiter ) -> array

   Purpose:
      Parse a delimited text file.

   Parameters:
      cFile - file to process
      cDelimiter - delimiter, default is comma

   Returns:
      Upon success -> Two dimensional array, of which each element contains
                      the results of parsing
      Upon error   -> An empty array
*/

#include "hbapi.h"
#include "hbfast.h"
/* adjustable, but this should be sufficient in normal situation */
#define MAX_READ 4096

//----------------------------------------------------------------------------//
static char ** hb_tokensplit ( char *string, BYTE delimiter, int iCharCount )
{
   char *buffer, *bufptr;
   char **token_list;
   char last_char = '\0';
   int word_count = 0, word_nbr;

   buffer = (char *) hb_xgrab ( iCharCount + 1 );

   bufptr = buffer;

   while ( *string )
   {
      if ( *string == delimiter )
      {
         while ( *string == delimiter )
         {
            string ++;
         }

         if (bufptr > buffer)
         {
            word_count ++;
            last_char = *bufptr++ = '\0';
         }
      }
      else
      {
         last_char = *bufptr++ = *string++;
      }
   }

   if (last_char > 0)
   {
      word_count++;
   }

   *bufptr = '\0';

   token_list = (char **) hb_xgrab (sizeof (char *) * (word_count + 2));
   token_list [0] = buffer;
   token_list++;

   bufptr = buffer;

   for (word_nbr = 0; word_nbr < word_count; word_nbr++)
   {
      token_list [word_nbr] = bufptr;
      bufptr += strlen (bufptr) + 1;
   }

   token_list [word_count] = NULL;

   return (token_list);
}

//----------------------------------------------------------------------------//
static BOOL file_read ( FILE *stream, char *string, int *iCharCount )
{
   int ch, cnbr = 0;

   memset (string, ' ', MAX_READ);

   for (;;)
   {
      ch = fgetc (stream);

      if ( (ch == '\n') ||  (ch == EOF) ||  (ch == 26) )
      {
         *iCharCount = cnbr;
         string [cnbr] = '\0';
         return (ch == '\n' || cnbr);
      }
      else
      {
         if ( cnbr < MAX_READ && ch != '\r' )
         {
            string [cnbr++] = (char) ch;
         }
      }

      if (cnbr >= MAX_READ)
      {
         *iCharCount = cnbr;
         string [MAX_READ] = '\0';
         return (TRUE);
      }
   }
}

//----------------------------------------------------------------------------//
HB_FUNC( FPARSE )
{
   FILE *inFile ;
   PHB_ITEM pSrc = hb_param(1, HB_IT_STRING);
   PHB_ITEM pDelim = hb_param(2, HB_IT_STRING);
   HB_ITEM pTemp, pArray, pItem;
   char *string ;
   char **tokens;
   int iToken, iCharCount = 0;
   BYTE nByte;

   /* file parameter correctly passed */
   if ( !pSrc )
   {
      hb_reta( 0 );
      return;
   }

   if ( pSrc->item.asString.length == 0 )
   {
      hb_reta( 0 );
      return;
   }

   /* open file for read */
   inFile = fopen( pSrc->item.asString.value, "r" );

   /* return empty array on failure */
   if ( !inFile )
   {
      hb_reta( 0 );
      return;
   }

   /* default delimiter to comma, chr(44) */
   nByte = pDelim ? (BYTE) pDelim->item.asString.value[0] : (BYTE) 44;

   /* the main array */
   pArray.type = HB_IT_NIL;
   hb_arrayNew( &pArray, 0 );

   /* book memory for line to read */
   string = (char*) hb_xgrab( MAX_READ + 1 );

   /* container for parsed line */
   pItem.type = HB_IT_NIL;

   /* holder for parsed text */
   pTemp.type = HB_IT_NIL;

   /* read the file until EOF */
   while ( file_read ( inFile, string, &iCharCount ) )
   {
      /* parse the read line */
      tokens = hb_tokensplit ( string, nByte, iCharCount ) ;

      /* prepare empty array */
      hb_arrayNew( &pItem, 0 );

      /* add parsed text to array */
      for (iToken = 0; tokens [iToken]; iToken++)
      {
         hb_arrayAddForward( &pItem, hb_itemPutC( &pTemp, tokens [iToken] ) );
      }

      /* add array containing parsed text to main array */
      hb_arrayAddForward( &pArray, &pItem );

      /* clean up */
      tokens--;
      hb_xfree( tokens [0] );
      hb_xfree( tokens );
   }

   /* return main array */
   hb_itemForwardValue( &(HB_VM_STACK).Return, &pArray );

   /* clean up */
   hb_xfree( string );
   fclose( inFile );
}
