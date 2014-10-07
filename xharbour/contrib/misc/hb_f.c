/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * File handling functions
 *
 * Copyright 1999 Andi Jahja <andij@aonlippo.co.id>
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

/* please run $(HARBOUR)\tests\testhbf.prg for testing */

#if defined( __WATCOMC__ )
   #pragma disable_message ( 124 )
#elif defined( __POCC__ )
   #pragma warn(push)
   #pragma warn(disable:2130)
#endif

#include "hbapi.h"
#include "hbapifs.h"

#define b_size     4096
#define c_size     4096

static ULONG hb_hbfskip( int recs );

static ULONG last_rec[10];
static ULONG recno[10];
static ULONG offset[10];
static int  handles[10];
static int  area = 0;
static BYTE *b;
static BYTE *c;
static ULONG last_off[10];
static ULONG lastbyte[10];
static int  isEof[10];

HB_FUNC( HB_FUSE )
{
   PHB_ITEM arg1_it = hb_param(1,HB_IT_STRING);
   PHB_ITEM arg2_it = hb_param(2,HB_IT_NUMERIC);
   int open_flags;

   if ( arg1_it )
   {
      open_flags     = arg2_it ? hb_parni(2) : 0;
      handles[area]  = (int) hb_fsOpen( hb_parcx(1), ( USHORT ) open_flags );
      offset[area]   = 0;
      recno[area]    = 1;
      b              = ( BYTE* ) hb_xgrab( b_size );
      c              = ( BYTE* ) hb_xgrab( c_size );
      lastbyte[area] = hb_fsSeek( handles[area], 0L, SEEK_END );
      isEof[area]    = (lastbyte[area] == 0);
      hb_retni( handles[area] );
   }
   else
   {
      hb_fsClose( handles[area] );
      hb_xfree( b )       ;
      hb_xfree( c )       ;
      hb_retni( 1 )       ;
      recno[area]    = 0L ;
      offset[area]   = 0L ;
      handles[area]  = 0  ;
      last_rec[area] = 0L ;
      last_off[area] = 0L ;
      lastbyte[area] = 0L ;
      isEof[area]    = 0  ;
   }
}

HB_FUNC( HB_FRECNO )
{
   hb_retnl( recno[area] );
}

HB_FUNC( HB_FSKIP )
{
   hb_hbfskip( hb_param(1,HB_IT_NUMERIC) ? hb_parni(1) : 1 );
}

static ULONG hb_hbfskip( int recs )
{
   LONG read_pos;
   LONG read_len;
   LONG x, y;
   int i;

   HB_TRACE(HB_TR_DEBUG, ("hb_hbskip(%d)", recs));

   if ( recs > 0 )
   {
      for (y = 0; y < recs; y++ )
      {
	 i = 0;
         hb_fsSeek( handles[area], offset[area], SEEK_SET );
         read_len = hb_fsRead( handles[area],  b, b_size );
         for (x = 0; x < read_len; x++ )
         {
	    BYTE cr1 = *(b + x);
	    BYTE cr2 = *(b + x + 1);

	    if ( ( cr1 == 13 || cr1 == 141 ) && cr2 == 10 )
	       i = 2;
	    else if ( cr1 == 10 )
	       i = 1;

	    if ( i )
            {
               break;
            }
         }
         if ( (offset[area] + x + i) < lastbyte[area] )
         {
            isEof[area] = FALSE;
            offset[area] += (x + i);
            recno[area] += 1;
         }
         else
            isEof[area] = TRUE;
      }
   }
   else
   {
      recs = -recs;
      isEof[area] = FALSE;

      if ( (recno[area] - recs) < 1 )
         return( 1 );

      for (y = recs; y > 0; y-- )
      {
         i = 0;

         if ( offset[area] < b_size )
         {
            read_pos = 0;
            read_len = (size_t)offset[area];
         }
         else
         {
            read_pos = (size_t)(offset[area] - b_size);
            read_len = b_size;
         }

         hb_fsSeek( handles[area], read_pos, SEEK_SET );
         read_len = hb_fsRead( handles[area], b, ( USHORT )read_len );

         for (x = read_len - 4; x >= 0; x-- )
         {
	    BYTE cr1 = *(b + x);
	    BYTE cr2 = *(b + x + 1);

	    if ( ( cr1 == 13 || cr1 == 141 ) && cr2 == 10 )
	       i = 2;
	    else if ( cr1 == 10 )
	       i = 1;

	    if ( i )
            {
               break;
            }
         }
         if ( x < 0 )
         {
            offset[area] = 0;
            recno[area] = 1;
         }
         else
         {
            offset[area] = read_pos + x + i;
            recno[area]--;
         }
      }
   }

   return ( recno[area] );
}

HB_FUNC( HB_FREADLN )
{
   ULONG x;
   ULONG read;
   BYTE cr1, cr2;

   hb_fsSeek( handles[area], offset[area], SEEK_SET );
   read = hb_fsRead( handles[area], b, b_size );

   for ( x = 0; x < b_size; x++ )
   {
      cr1 = *(b + x);
      cr2 = *(b + x + 1);

      if ( ( ( cr1 == 13 || cr1 == 141 ) && cr2 == 10 ) || ( cr1 == 10 ) || ( cr1 == 26 ) || ( x >= read ) )
      {
         break;
      }
   }

   hb_retclen( (char*) b, x );
}

HB_FUNC( HB_FEOF )
{
   hb_retl( isEof[area] );
}

HB_FUNC( HB_FGOTO )
{
   ULONG target;
   ULONG last;

   target = hb_parnl(1);

   if ( recno[area] > target )
   {
      while ( recno[area] != target )
      {
         last = recno[area];
         hb_hbfskip(-1);

         if ( recno[area] == last )
            break;
      }
   }
   else
   {
      while ( recno[area] != target )
      {
         last = recno[area];
         hb_hbfskip(1);

         if ( recno[area] == last )
            break;
      }
   }
}

HB_FUNC( HB_FGOBOTTOM )
{
    while ( !isEof[area] )
    {
        hb_hbfskip( 1 );
    }

    last_rec[area] = hb_hbfskip( 0 );
}
/*
HB_FUNC( HB_FGOBOTTOM )
{
   int x;
   int len;
   ULONG loc, last;
   BYTE cr1, cr2;

   if ( last_rec[area] != 0 )
   {
      recno[area] = last_rec[area];
      offset[area] = last_off[area];
   }
   else
   {
      loc = 0L;
      last = offset[area];

      do
      {
         hb_fsSeek( handles[area], offset[area], SEEK_SET );
         len = hb_fsRead(  handles[area], c, c_size );

         for ( x = 0; x < len; x++ )
         {
            cr1 = *(c + x);
            cr2 = *(c + x + 1);

            if ( ( ( cr1 == 13 || cr1 == 141 ) && cr2 == 10 ) || ( cr1 == 10 ) ) //|| ( x - loc > b_size ) )
            {
               last = offset[area] + loc;
               recno[area]++;
               x++;
               loc = x + 1;
            }
         }
         offset[area] += loc;

      } while ( len == c_size );

      last_rec[area] = --recno[area];
      last_off[area] = last;
   }
}
*/
HB_FUNC( HB_FGOTOP )
{
   offset[area] = 0L;
   recno[area] = 1L;
   isEof[area] = (lastbyte[area] == 0);
}

HB_FUNC( HB_FLASTREC )
{
   ULONG old_rec;
   ULONG old_offset;
   int  bIsEof;

   old_rec = recno[area];
   old_offset = offset[area];
   bIsEof  = isEof[area];

   HB_FUNCNAME( HB_FGOBOTTOM )();
   hb_retnl( last_rec[area] );

   recno[area]  = old_rec;
   offset[area] = old_offset;
   isEof[area]  = bIsEof  ;
}

HB_FUNC( HB_FSELECT )
{
   hb_retni( area + 1 );

   if ( ISNUM(1) )
      area = hb_parni(1) - 1;
}

HB_FUNC( HB_FINFO )                     /* used for debugging */
{
   hb_reta( 6 );
   hb_storni( area+1,         -1, 1);
   hb_storni( last_rec[area], -1, 2);
   hb_storni( recno[area],    -1, 3);
   hb_storni( offset[area],   -1, 4);
   hb_storni( lastbyte[area], -1, 5);
   hb_storl ( isEof[area],    -1, 6);
}

HB_FUNC( HB_FREADANDSKIP )
{
/* ------------------------------------------------
   Warning: This is a rogue function! It is a first shot at adding the logic
   to read .CSV records that respect CRLF embedded within quotes.
   It is very common, especially with Microsoft products, for
   comma-separated files to allow a field (usually an address field)
   to have hard returns within it. These records appear corrupted to any
   reader that presumes all hard returns are record separators.

   This function is useful right now to loop through a CSV file
   while !hb_feof(), but it does NOT recognize the same record count
   and positioning that the other functions in this file use.
   It does its own skip and read, so an entire file can be read
   sequentially with just this function.
   -BH
 --------------------------------------------------*/
   ULONG x =  0;
   ULONG read;
   BOOL bInField = 0, bHasCRLF = FALSE;
   BYTE cr1, cr2;
   int i = 0;

   hb_fsSeek( handles[area], offset[area], SEEK_SET );
   read = hb_fsRead( handles[area], b, b_size );

   while (  x < read )
   {
      i   = 0;
      cr1 = *(b + x);
      cr2 = *(b + x + 1);

      if ( cr1 == '"' )
      {
         bInField = !bInField ;
         x++;
         continue;
      }

      if ( bInField )
      {
         x++;
         continue;
      }

      if ( ( cr1 == 13 || cr1 == 141 ) && cr2 == 10 )
         i = 2;
      else if ( cr1 == 10 )
         i = 1;

      if ( i )
      {
         x += i;
         bHasCRLF = TRUE;
         break;
      }
      x++;
   }

   offset[area] = offset[area] + x;
   recno[area] += 1;

   // See if there's more to read
   if ( !isEof[area] )
      isEof[area] = (lastbyte[area] <= offset[area] + 1) ;

   hb_retclen( (char*) b, x - (bHasCRLF ? i : 0) );
}
