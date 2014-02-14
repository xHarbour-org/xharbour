/*
 * $Id$
 */

/*
 * File......: DISPC.C
 * Author....: Mike Taylor
 * CIS ID....: ?
 *
 * This is an original work by Mike Taylor and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.10  22 Apr 2004 15:32:00   David G. Holm <dholm@jsd-llc.com>
 * Corrected all hb_fsSeek calls to use FS_ defines instead of using
 * redefined SEEK_ ones that conflict with the C-level SEEK_ defines.
 *    Rev 1.9   ? ?
 * An unknown number of changes were made between Rev 1.8 and Rev 1.10.
 *
 *    Rev 1.8   24 May 2002 19:25:00   David G. Holm <dholm@jsd-llc.com>
 * Fixed some problems that caused C++ compiles to fail.
 *
 *    Rev 1.7   29 Mar 2002 17:00:00   Walter Negro <anegro@overnet.com.ar>
 * Ported to Harbour
 *
 *    Rev 1.6   01 Jan 1995 03:01:00   TED
 * Changed some prototypes to eliminate compiler warnings.
 *
 *    Rev 1.5   14 Feb 1994 16:58:42   GLENN
 * Steve Tyrakowski and Kevin Maher modified to be CPMI-compliant.
 *
 *    Rev 1.4   18 Nov 1991 02:20:20   GLENN
 * Mike fixed a bug in _ft_dfinit() related to allocating memory.  Some
 * users had been reporting problems, but everyone who tested this patch
 * reported success.
 *
 *    Rev 1.3   17 Aug 1991 15:25:46   GLENN
 * Don Caton fixed some spelling errors in the doc
 *
 *    Rev 1.2   15 Aug 1991 23:08:14   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:42   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:46   GLENN
 * Nanforum Toolkit
 *
 *
 */

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapigt.h"

#define K_STRING     0
#define K_LIST       ( ! K_STRING )

#define CR           ( ( char ) 13 )
#define LF           ( ( char ) 10 )

#define TABSET       8

typedef struct
{
   long     buffoffset;          /* offset into buffer of current line  */
   long     fsize;               /* file size in bytes                  */
   int      bufftop, buffbot;    /* first and last character in buffer  */
   int      wintop, winbot;      /* first and last character in window  */
   int      winrow, wincol;      /* row and column of window highlight  */
   int      sline, eline;        /* start and end line of window        */
   int      scol, ecol;          /* start and end col of window         */
   int      height, width;       /* height and width of window          */
   int      infile;              /* input file handle                   */
   int      maxlin;              /* line size                           */
   int      buffsize;            /* buffer size                         */
   int      hlight;              /* highlight attribute                 */
   int      norm;                /* normal attribute                    */
   int      kcount;              /* number of keys in terminate key list*/
   int      colinc;              /* col increment amount                */
   BOOL     brows;               /* browse flag                         */
   BOOL     refresh;             /* TRUE means refresh screen           */
   char     kstr[ 25 ];          /* terminate key string                */
   int      keylist[ 24 ];       /* terminate key list                  */
   int      keytype;             /* K_STRING or K_LIST                  */

   BOOL     bIsAllocated;        /* if buffers were allocated           */
   char *   buffer;              /* file buffer pointer                 */
   char *   lbuff;               /* line buffer pointer                 */
   char *   vseg;                /* video segment variable              */
} FT_DISPC, * PFT_DISPC;

static FT_DISPC DISPC;

/* prototypes */

static void          chattr( int x, int y, int len, int attr );
static long          getblock( long offset );
static void          buff_align( void );
static void          win_align( void );
static void          disp_update( int offset );
static void          windown( void );
static void          winup( void );
static void          linedown( void );
static void          lineup( void );
static void          filetop( void );
static void          filebot( void );

/*
 * chattr() replace the color attribute with a new one starting at
 * location x, y and going for length len.
 *
 */

static void chattr( int x, int y, int len, int attr )
{
   int    i;
   char * vmem;

   vmem = DISPC.vseg + ( y * ( DISPC.width + 1 ) * 2 ) + ( x * 2 ) + 1;

   /* calc the screen memory coord */

   for( i = 0; i <= len; i++, vmem += 2 )   /* write the new attribute value */
      *vmem = ( char ) attr;
}

/*
 * function getblock() reads the text file and returns the a block.
 *  the variables offset and buffsize tell it where to start reading and
 *  how many bytes to try to read.  if the block read in would not fill
 *  the buffer then the offset is adjusted so that the start or end of
 *  of the file is positioned at the head or tail of the buffer.
 *
 * it returns the offset into the file of the first byte of the buffer.
 *
 */

static long getblock( long offset )
{
   /*
       set the file pointer to the proper offset
       and if an error occured then check to see
       if a positive offset was requested, if so
       then set the pointer to the offset from
       the end of the file, otherwise set it from
       the beginning of the file.
    */

   hb_fsSeek( DISPC.infile, offset, FS_SET );

   /* read in the file and set the buffer bottom variable equal */
   /*  to the number of bytes actually read in.                 */

   DISPC.buffbot = (int) hb_fsReadLarge( DISPC.infile, ( BYTE * ) DISPC.buffer, DISPC.buffsize );

   /* if a full buffer's worth was not read in, make it full.   */

   if( ( DISPC.buffbot != DISPC.buffsize ) && ( DISPC.fsize > DISPC.buffsize ) )
   {
      if( offset > 0 )
         hb_fsSeek( DISPC.infile, ( long ) -DISPC.buffsize, FS_END );
      else
         hb_fsSeek( DISPC.infile, ( long ) DISPC.buffsize, FS_SET );

      DISPC.buffbot = (int) hb_fsReadLarge( DISPC.infile, ( BYTE * ) DISPC.buffer, DISPC.buffsize );
   }

   /* return the actual file position */

   return hb_fsSeek( DISPC.infile, 0L, FS_RELATIVE ) - DISPC.buffbot;
}

/*
 * buff_align makes sure the buffer top and bottom variables point
 * to actual complete lines of text.
 *
 */

static void buff_align( void )
{
   int i;

   DISPC.bufftop = 0;
   DISPC.buffbot = DISPC.buffsize;

   if( DISPC.buffoffset != 0L )        /* if the buffoffset is otherthan 0      */
   {
      i = DISPC.bufftop;               /* start at the top of the file and scan */

      /* forward until a CR is reached. */

      while( ( DISPC.buffer[ i ] != CR ) && ( i < DISPC.buffbot ) )
         i++;

      DISPC.bufftop = i + 2;
   }

   /* if the buffer offset is not a complete */
   /* buffer's length away from the file end */

   if( DISPC.buffoffset + ( ( long ) DISPC.buffbot ) != DISPC.fsize )
   {
      /*
         if the file position of the last byte
          of the buffer would end up past the
          end of the file, then the buffer does
          contain a complete buffer full and the
          buffer end pointer needs to be set to
          the last character of the file.
       */

      if( DISPC.buffoffset + ( ( long ) DISPC.buffbot ) > DISPC.fsize )
         DISPC.buffbot = ( int ) ( DISPC.fsize - DISPC.buffoffset );

      i = DISPC.buffbot;               /* point the end of the buffer to a valid */

      /* complete text line. */

      while( ( DISPC.buffer[ i ] != CR ) && ( i > DISPC.bufftop ) )
         i--;

      DISPC.buffbot = i + 2;
   }
}

/*
 * win_align takes the value for wintop and then figures out where
 * winbot would be.  if winbot would extend past the end of the
 * buffer, then the top of the window is adjusted to ensure that a full
 * screen of text will appear.  This simplifies the cursor routines.
 *
 */

static void win_align( void )
{
   int i;

   DISPC.winbot = DISPC.wintop;        /* find out if there is enough text for */
   i            = 0;                   /* full window.                         */

   while( ( DISPC.winbot < DISPC.buffbot ) && ( i < DISPC.height ) )
   {
      if( DISPC.buffer[ DISPC.winbot ] == CR )
         i++;
      DISPC.winbot++;
   }

   if( i < DISPC.height )              /* if there is not a full window,       */
   {
      /* then retrofit winbot to the end of a line */

      while( DISPC.buffer[ DISPC.winbot ] != LF && DISPC.winbot > DISPC.bufftop )
         DISPC.winbot--;

      DISPC.wintop = DISPC.winbot;
      i            = 0;                /* and setup wintop                     */

      while( ( DISPC.wintop > DISPC.bufftop ) && ( i <= DISPC.height ) )
      {
         if( DISPC.buffer[ DISPC.wintop ] == LF )
            i++;
         DISPC.wintop--;
      }

      if( DISPC.wintop != DISPC.bufftop )
         DISPC.wintop += 2;
   }
}

/*
 * this routine displays the actual text in the window.  This is done
 * by taking each line and placing it in a string.  the screen line
 * is then taken from the appropriate group of characters in the string.
 * this allows a window to page left-right across the buffer without
 * having to use any complex algorithm to calc the needed chars.
 *
 */

static void disp_update( int offset )
{
   int    line, col, pos, i;
   char * vmem;


   DISPC.refresh = FALSE;
   line          = 0;

   while( line < DISPC.height )
   {
      /*
         calculate the initial position, this save execution
         time because each column is considered as a offset
         from the line start
       */

      pos = ( line * ( DISPC.width + 1 ) * 2 );

      /* copy string to temp buffer */

      for( i = 0; DISPC.buffer[ offset ] != CR && offset <= DISPC.winbot; offset++ )
      {
         if( i <= DISPC.maxlin )
         {
            if( DISPC.buffer[ offset ] == '\t' )            /* check for a tab      */
            {
               DISPC.lbuff[ i++ ] = ' ';                    /* pad with spaces      */
               while( i % TABSET && i <= DISPC.maxlin )     /* until tab stop       */
                  DISPC.lbuff[ i++ ] = ' ';                 /* is reached or EOL    */
            }
            else
               DISPC.lbuff[ i++ ] = DISPC.buffer[ offset ];

         }
      }

      for( ; i <= DISPC.maxlin; i++ )                       /* fill out with spaces */
         DISPC.lbuff[ i ] = ' ';

      /* place the proper characters onto the screen */

      for( i = DISPC.wincol, col = 0; col <= DISPC.width; col++ )
      {
         vmem  = DISPC.vseg + pos + ( col * 2 );

         *vmem = DISPC.lbuff[ i++ ];
      }

      line   += 1;
      offset += 2;
   }
   hb_gtRest( ( USHORT ) DISPC.sline, ( USHORT ) DISPC.scol, ( USHORT ) DISPC.eline, ( USHORT ) DISPC.ecol, DISPC.vseg );
}

/*
 * move the window pointers so that a new window's worth of information
 * is visible.  it adjusts the pointers within the buffer and if necessary
 * it calls the getblock function to load in a new buffer
 *
 */

static void winup( void )
{
   int  k;
   long i, j;

   DISPC.refresh = TRUE;
   k             = DISPC.wintop - 3;

   while( ( DISPC.buffer[ k ] != CR ) && ( k > DISPC.bufftop ) )
      k--;

   if( k >= DISPC.bufftop )
   {
      if( DISPC.buffer[ k ] == CR )
         k += 2;

      DISPC.wintop = k;
      k            = DISPC.winbot - 3;

      while( DISPC.buffer[ k ] != CR )
         k--;

      DISPC.winbot = k + 2;
   }
   else
   if( ( ( long ) DISPC.bufftop ) + DISPC.buffoffset > 0 && DISPC.fsize > DISPC.buffsize )
   {
      i = DISPC.buffoffset + DISPC.wintop;
      j = DISPC.buffoffset - ( ( long ) ( DISPC.buffsize / 2 ) );

      if( j < 0 )
         j = 0;

      DISPC.buffoffset = getblock( j );
      DISPC.wintop     = ( ( int ) ( i - DISPC.buffoffset ) );

      buff_align();
      win_align();
   }
}

/*
 * move the window pointers so that a new window's worth of information
 * is visible.  it adjusts the pointers within the buffer and if necessary
 * it calls the getblock function to load in a new buffer
 *
 */

static void windown( void )
{
   int  k;
   long i, j;

   DISPC.refresh = TRUE;
   k             = DISPC.winbot;

   while( ( DISPC.buffer[ k ] != CR ) && ( k <= DISPC.buffbot ) )
      k++;
   k += 2;

   if( k <= DISPC.buffbot )
   {
      DISPC.winbot = k;
      k            = DISPC.wintop;

      while( DISPC.buffer[ k ] != CR )
         k++;
      DISPC.wintop = k + 2;
   }
   else if( ( ( ( long ) DISPC.buffbot ) + DISPC.buffoffset ) < DISPC.fsize && DISPC.fsize > DISPC.buffsize )
   {
      i = DISPC.buffoffset + DISPC.wintop;
      j = i;

      if( j > DISPC.fsize )
         j = DISPC.fsize - ( ( long ) DISPC.buffsize );

      DISPC.buffoffset = getblock( j );

      if( i < DISPC.buffoffset )
         DISPC.wintop = 0;
      else
         DISPC.wintop = ( ( int ) ( i - DISPC.buffoffset ) );

      buff_align();
      win_align();
   }
}

/* move the cursor one line down */

static void linedown( void )
{
   if( DISPC.winrow < DISPC.eline )    /* if cursor not at last line */
      ++DISPC.winrow;
   else                                /* otherwise adjust the window top variable */
      windown();
}

/* move the cursor one line up */

static void lineup( void )
{
   if( DISPC.winrow > DISPC.sline )
      --DISPC.winrow;
   else
      winup();
}

/* go to the top of the file */

static void filetop( void )
{
   if( DISPC.buffoffset != 0 )
   {
      DISPC.buffoffset = getblock( 0L );

      buff_align();
   }

   DISPC.refresh = TRUE;
   DISPC.wintop  = ( int ) DISPC.buffoffset;
   DISPC.winrow  = DISPC.sline;
   DISPC.wincol  = 0;

   win_align();
}

/* goto the bottom of the file */

static void filebot( void )
{
   if( ( ( ( long ) DISPC.buffbot ) + DISPC.buffoffset ) < DISPC.fsize && DISPC.fsize > DISPC.buffsize )
   {
      DISPC.buffoffset = getblock( DISPC.fsize + 1 );

      buff_align();
   }

   DISPC.refresh = TRUE;
   DISPC.wintop  = DISPC.buffbot - 3;
   DISPC.winrow  = DISPC.eline;
   DISPC.wincol  = 0;

   win_align();
}

HB_FUNC( _FT_DFINIT )
{
   int     rval, i, j;
   HB_SIZE ulSize;

   rval = 0;

   DISPC.sline  = hb_parni( 2 );                            /* top row of window     */
   DISPC.scol   = hb_parni( 3 );                            /* left col              */
   DISPC.eline  = hb_parni( 4 );                            /* bottom row            */
   DISPC.ecol   = hb_parni( 5 );                            /* right col             */

   DISPC.width  = DISPC.ecol  - DISPC.scol;                 /* calc width of window  */
   DISPC.height = DISPC.eline - DISPC.sline + 1;            /* calc height of window */

   hb_gtRectSize( DISPC.sline, DISPC.scol, DISPC.eline, DISPC.ecol, &ulSize );
   DISPC.vseg = ( char * ) hb_xalloc( ulSize );
   if( DISPC.vseg != NULL )
      hb_gtSave( ( USHORT ) DISPC.sline, ( USHORT ) DISPC.scol, ( USHORT ) DISPC.eline, ( USHORT ) DISPC.ecol, DISPC.vseg );

   DISPC.maxlin   = hb_parni( 12 );
   DISPC.buffsize = hb_parni( 13 );                            /* yes - load value */

   DISPC.buffer   = ( char * ) hb_xalloc( DISPC.buffsize );    /* allocate memory  */
   DISPC.lbuff    = ( char * ) hb_xalloc( DISPC.maxlin + 1 );  /*  for buffers     */

   DISPC.bIsAllocated = ! ( DISPC.buffer == NULL || DISPC.lbuff == NULL || DISPC.vseg == NULL );
   /* memory allocated? */
   if( ! DISPC.bIsAllocated )
   {
      rval = 8;                                 /* return error code 8 (memory) */
      if( DISPC.buffer != NULL )
         hb_xfree( DISPC.buffer );
      if( DISPC.lbuff != NULL )
         hb_xfree( DISPC.lbuff );
      if( DISPC.vseg != NULL )
         hb_xfree( DISPC.vseg );
   }
   else                                         /* get parameters            */
   {
      DISPC.infile = hb_parni( 1 );             /* file handle               */
      j            = hb_parni( 6 );             /* starting line value       */
      DISPC.norm   = hb_parni( 7 );             /* normal color attribute    */
      DISPC.hlight = hb_parni( 8 );             /* highlight color attribute */

      if( hb_parinfo( 9 ) & HB_IT_ARRAY )       /* if array */
      {
         DISPC.keytype = K_LIST;
         DISPC.kcount  = (int) hb_parinfa( 9, 0 );
         if( DISPC.kcount > 24 )
            DISPC.kcount = 24;
         for( i = 1; i <= DISPC.kcount; i++ )
            DISPC.keylist[ i - 1 ] = hb_parni( 9, i );  /* get exit key list */
      }
      else
      {
         DISPC.keytype = K_STRING;
         DISPC.kcount  = (int) hb_parclen( 9 );
         if( DISPC.kcount > 24 )
            DISPC.kcount = 24;
         hb_strncpy( DISPC.kstr, hb_parcx( 9 ), DISPC.kcount );  /* get exit key string */
      }

      DISPC.brows      = hb_parl( 10 ) ? TRUE: FALSE;   /* get browse flag   */
      DISPC.colinc     = hb_parni( 11 );                /* column skip value */

      DISPC.bufftop    = 0;                             /* init buffer top pointer      */
      DISPC.buffbot    = DISPC.buffsize;                /* init buffer bottom pointer   */
      DISPC.buffoffset = 0;                             /* curr line offset into buffer */
      DISPC.winrow     = DISPC.sline;                   /* init window row              */
      DISPC.wincol     = 0;                             /* init window col              */
      DISPC.wintop     = 0;                             /* init window top pointer      */
      DISPC.winbot     = 0;                             /* init window bottom pointer   */

      /* get file size */

      DISPC.fsize = hb_fsSeek( DISPC.infile, 0L, FS_END ) - 1;

      /* get the first block */

      hb_fsSeek( DISPC.infile, 0L, FS_SET );

      /* if block less than buffsize */

      if( DISPC.fsize < ( ( long ) DISPC.buffbot ) )
         DISPC.buffbot = ( int ) DISPC.fsize;           /* then set buffer bottom */

      /* set the current lines buffer offset pointer */

      DISPC.buffoffset = getblock( ( long ) DISPC.bufftop );

      /* align buffer and window pointer to valid values */

      buff_align();
      win_align();

      /* point line pointer to line passed by caller */

      for( i = 1; i < j; i++ )
         linedown();

      hb_gtRest( ( USHORT ) DISPC.sline, ( USHORT ) DISPC.scol, ( USHORT ) DISPC.eline, ( USHORT ) DISPC.ecol, DISPC.vseg );

   }

   hb_retni( rval );
}

HB_FUNC( _FT_DFCLOS )
{
   if( DISPC.bIsAllocated )
   {
      if( DISPC.buffer != NULL )
         hb_xfree( DISPC.buffer );                /* free up allocated buffer memory */
      if( DISPC.lbuff != NULL )
         hb_xfree( DISPC.lbuff );
      if( DISPC.vseg != NULL )
         hb_xfree( DISPC.vseg );
   }
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_DISPFILE()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Browse a text file
 *  $SYNTAX$
 *     FT_DISPFILE() -> cExitkey
 *  $ARGUMENTS$
 *     None
 *  $RETURNS$
 *     The ASCII keystroke that terminated FT_DISPFILE()
 *  $DESCRIPTION$
 *     This routine displays a text file within a defined window using as
 *     little memory as possible.  The text file to display has to be
 *     present or an error value of 0 is returned (as a character.)
 *
 *     Assumptions: The routine assumes that all lines are terminated
 *                  with a CR/LF sequence (0x0d and 0x0a).
 *
 *     Note:        Make sure you allocate a buffer large enough to hold
 *                  enough data for the number of lines that you have
 *                  in the window.  Use the following formula as a
 *                  guideline - buffer size = (# of line) + 1 * RMargin
 *                  this is the smallest you should make the buffer and
 *                  for normal use I recommend 4096 bytes.
 *
 *     Cursor Keys: Up, Down    - moves the highlight line
 *                  Left, Right - moves the window over nColSkip col's
 *                  Home        - moves the window to the far left
 *                  End         - moves the window to the nRMargin column
 *                  PgUp, PgDn  - moves the highlight one page
 *                  Ctrl-PgUp   - moves the highlight to the file top
 *                  Ctrl-PgDn   - moves the highlight to the file bottom
 *                  Ctrl-Right  - moves the window 16 col's to the right
 *                  Ctrl-Left   - moves the window 16 col's to the left
 *
 *                  Esc, Return - terminates the function
 *
 *                  All other keys are ignored unless they are specified
 *                  within cExitKeys parameter.  This list will tell the
 *                  routine what keys terminate the function.  Special
 *                  keys must be passed by a unique value and that value
 *                  can be found by looking in the keys.h file.
 *  $EXAMPLES$
 *     @ 4,9 TO 11,71
 *
 *     FT_DFSETUP("test.txt", 5, 10, 10, 70, 1, 7, 15,;
 *                 "AaBb" + Chr(143), .T., 5, 132, 4096)
 *
 *     cKey = FT_DISPFILE()
 *
 *     FT_DFCLOSE()
 *
 *     @ 20,0 SAY "Key that terminated FT_DISPFILE() was: " + '[' + cKey + ']'
 *  $SEEALSO$
 *     FT_DFSETUP() FT_DFCLOSE()
 *  $END$
 */

HB_FUNC( FT_DISPFILE )
{
   int  i, done;
   char rval[ 2 ];
   int  ch;

   /* make sure buffers were allocated and file was opened */

   if( DISPC.bIsAllocated && DISPC.infile > 0 )
   {
      done          = FALSE;
      DISPC.refresh = TRUE;

      /* draw inside of window with normal color attribute */

      for( i = 0; i < DISPC.height; i++ )
         chattr( 0, i, DISPC.width, DISPC.norm );

      hb_gtRest( ( USHORT ) DISPC.sline, ( USHORT ) DISPC.scol, ( USHORT ) DISPC.eline, ( USHORT ) DISPC.ecol, DISPC.vseg );

      /* main processing loop -- terminated by user key press */

      do
      {
         if( DISPC.refresh )                      /* redraw window contents? */
            disp_update( DISPC.wintop );

         hb_gtRest( ( USHORT ) DISPC.sline, ( USHORT ) DISPC.scol, ( USHORT ) DISPC.eline, ( USHORT ) DISPC.ecol, DISPC.vseg );

         /* if not browse, highlight the current line */

         if( DISPC.brows == FALSE )
            chattr( 0, DISPC.winrow - DISPC.sline, DISPC.width, DISPC.hlight );

         hb_gtRest( ( USHORT ) DISPC.sline, ( USHORT ) DISPC.scol, ( USHORT ) DISPC.eline, ( USHORT ) DISPC.ecol, DISPC.vseg );

         hb_gtSetPos( ( SHORT ) DISPC.winrow, ( SHORT ) DISPC.scol );

         ch = hb_inkey( TRUE, 0.0, INKEY_ALL );   /* get user key press */

         /* if not browse, then un-highlight current line */

         if( DISPC.brows == FALSE )
            chattr( 0, DISPC.winrow - DISPC.sline, DISPC.width, DISPC.norm );

         hb_gtRest( ( USHORT ) DISPC.sline, ( USHORT ) DISPC.scol, ( USHORT ) DISPC.eline, ( USHORT ) DISPC.ecol, DISPC.vseg );

         /* figure out what the user wants to do */

         switch( ch )
         {
            case K_DOWN:

               if( DISPC.brows )                                    /* if browse flag */
                  DISPC.winrow = DISPC.eline;                       /* is set, force  */

               /* active line to */
               linedown();                                          /* be last line   */
               break;

            case K_UP:

               if( DISPC.brows )                                    /* if browse flag */
                  DISPC.winrow = DISPC.sline;                       /* is set, force  */
                                                                    /* active line to */
               lineup();                                            /* be first line  */
               break;

            case K_LEFT:

               DISPC.wincol -= DISPC.colinc;                        /* move cursor    */
               DISPC.refresh = TRUE;                                /* to the left    */

               if( DISPC.wincol < 0 )
                  DISPC.wincol = 0;

               break;

            case K_RIGHT:

               DISPC.wincol += DISPC.colinc;                        /* move cursor  */
               DISPC.refresh = TRUE;                                /* to the right */

               if( DISPC.wincol > ( DISPC.maxlin - DISPC.width ) )
                  DISPC.wincol = DISPC.maxlin - DISPC.width;

               break;

            case K_HOME:

               DISPC.wincol  = 0;                                   /* move cursor  */
               DISPC.refresh = TRUE;                                /* to first col */

               break;

            /* move cursor to last col */

            case K_END:

               DISPC.wincol  = DISPC.maxlin - DISPC.width;
               DISPC.refresh = TRUE;

               break;

            case K_CTRL_LEFT:

               DISPC.wincol -= 16;                                  /* move cursor    */
               DISPC.refresh = TRUE;                                /* 16 col to left */

               if( DISPC.wincol < 0 )
                  DISPC.wincol = 0;

               break;

            case K_CTRL_RIGHT:

               DISPC.wincol += 16;                                  /* move cursor     */
               DISPC.refresh = TRUE;                                /* 16 col to right */

               if( DISPC.wincol > ( DISPC.maxlin - DISPC.width ) )
                  DISPC.wincol = DISPC.maxlin - DISPC.width;

               break;

            case K_PGUP:

               for( i = 0; i < DISPC.height; i++ )                  /* move window */
                  winup();                                          /* up one page */

               break;

            case K_PGDN:

               for( i = 0; i < DISPC.height; i++ )                  /* move window */
                  windown();                                        /* down 1 page */

               break;

            case K_CTRL_PGUP:

               filetop();                                           /* move cursor to */
               break;                                               /* to top of file */

            case K_CTRL_PGDN:

               filebot();                                           /* move cursor to */
               break;                                               /* to bot of file */

            case K_ENTER:

               done = TRUE;                                         /* carriage return */
               break;                                               /* terminates      */

            case K_ESC:

               done = TRUE;                                         /* escape key */
               break;                                               /* terminates */

            /* scan key list and see if key pressed is there */

            default:

               if( DISPC.keytype == K_STRING )
               {
                  for( i = 0; i <= DISPC.kcount; i++ )
                     if( ( ch > 0 ) && ( ch < 256 ) )
                        if( ( int ) DISPC.kstr[ i ] == ch )
                           done = TRUE;
               }
               else
               {
                  for( i = 0; i < DISPC.kcount; i++ )
                     if( DISPC.keylist[ i ] == ch )
                        done = TRUE;
               }

               break;
         }
      }
      while( done == FALSE );
   }
   else
      ch = 0;


   /* store the key pressed as a character to be returned */

   /* return key value to caller */

   if( DISPC.keytype == K_STRING )
   {
      rval[ 0 ] = ( char ) ch;
      rval[ 1 ] = '\0';
      hb_retc( rval );
   }
   else
      hb_retni( ch );
}
