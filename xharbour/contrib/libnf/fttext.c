/*
 * $Id: fttext.c,v 1.1 2005/04/26 10:06:11 andijahja Exp $
 */

/*
 * File......: TEXT.C
 * Author....: Brice de Ganahl and Steve Larsen
 * CIS ID....: 76370,1532
 *
 * xHarbour Conversion : Andi Jahja <andijahja@xharbour.com>
 *
 * This is an original work by Brice de Ganahl and Steve Larsen
 * and is placed in the public domain.
 *
 * Doc headers by Glenn Scott, Don Caton, and Steve Larsen
 *
 * Extensively revised by Steve Larsen
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.8   01 May 1995 04:36:22   TED
 * Major overhaul by Steve Larsen to fix several bugs/quirkiness,
 * add some requested features, clean up source for readability.
 *
 * -  Added ft_fError() test
 * -  Added ft_fBOF() test
 * -  Provided protected mode compatibility
 * -  Increased buffer to 4k, added logic to allow lines longer than
 *    the buffer size.
 * -  Revised seek logic
 * -  Changed undocumented calls to API functions wherever possible
 *
 *
 *    Rev 1.7   17 Oct 1992 16:25:16   GLENN
 * Leo cleaned up the documentation, including an errant SEEALSO
 * reference.
 *
 *    Rev 1.6   03 Oct 1992 02:07:38   GLENN
 * Minor adjustments to file header block.
 *
 *    Rev 1.5   03 Oct 1992 02:03:44   GLENN
 * Major modifications by Steve Larsen, as follows:
 *
 * Brice laid some wonderful groundwork with his initial release of
 * these functions, however I needed more capability.  With his per-
 * mission, I have made the following additions/changes to Rev. 1.4:
 *
 * -  Eliminated the problem of memory for buffers being re-allocated every
 *    time a file got used.
 * -  Further reduced memory impact by converting from extend system memory
 *    allocation techniques to virtual memory.  To accomplish this, we
 *    use the Clipper v5.01 r1.29 variants of the "_v" undocumented
 *    internal functions.  If these functions change in future releases, you
 *    will need to locate them herein and make the appropriate changes.
 *
 *    NOTE: these functions allocate and deallocate virtual memory on an
 *    "as-needed" basis.  If your application makes heavy and frequent use
 *    of those functions that perform a lot of buffering (ft_fInsert(),
 *    ft_fDelete() and ft_fWrite()), you might consider modifying the memory
 *    management scheme used herein, that is, allocate the required buffers
 *    only once upon the first call to these functions, then recycle them.
 * -  Added the ability to specify file open mode.
 * -  Added a function to write to a record, which through a switch can either
 *    over-write the current record, or insert a new one.
 * -  Added functions to insert, delete and append a specified number of lines.
 * -  Fixed the existing functions so that they properly handle "trailers",
 *    that is, a case where the last chars in a file are not CRLF delimited.
 * -  Provided checking for the possibility that the file might be terminated
 *    with ^Z (1Ah), if so, ignoring it (providing consistency with non-^Z
 *    terminated files).  This only occurs on the last record of a file.
 * -  Eliminated a potential problem if one were to issue an ft_fUse() prior
 *    actually opening any files.
 * -  Replaced the original C parsing logic to determine the end-of-line (CRLF)
 *    with an optimized assembler routine.  This bypassed a significant
 *    performance hit.
 * -  The original header (FTTEXT.h) file in now incorporated in this one file.
 *    This is not necessarily an enhancement, more like laziness.
 * -  Provided the (followup) author with his very first C experience!
 *
 *    Steve Larsen, Dec. 7, 1991   CIS 76370,1532
 *
 * -  Function changes/additions (refer to the individual doc headers for
 *    details):
 *
 *    FT_FSELECT( [ < nArea  > ] )                 -> nArea
 *    FT_FUSE(    [ < cFile  > ][, < nMode >   ] ) -> nHandle | NIL
 *    FT_FWRITELN(  < cData  >  [, < lInsert > ] ) -> NIL
 *    FT_FINSERT( [ < nLines > ] )                 -> NIL
 *    FT_FDELETE( [ < nLines > ] )                 -> NIL
 *    FT_FAPPEND( [ < nLines > ] )                 -> NIL
 *
 *    Internal Steve Larsen revisions:
 *
 *     12/07/91  Original rework
 *     02/13/92  Fixed _findeol(), FT_FREADLN() and FT_FGOBOT() to
 *               better handle files with CRLF, LF, ^Z or nothing
 *               at the EOF.  Previously, under some conditions the
 *               last record was chopped by a character, depending
 *               on the last character(s).
 *     05/02/92  Fixed buffering and VMM allocation problem with
 *               FT_FGOBOT().
 *     08/26/92  Correcting problem when appending blank lines to an
 *               empty file (ft_fAppend() and ft_fWriteLn()).
 *
 *
 *    Rev 1.4   17 Aug 1991 15:31:08   GLENN
 * Don Caton fixed some spelling errors in the doc
 *
 *    Rev 1.3   15 Aug 1991 23:08:36   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   29 Apr 1991 08:02:12   GLENN
 * Minor adjustments to documentation block
 *
 *    Rev 1.1   29 Apr 1991 08:00:26   GLENN
 * ft_flastrec() -- name was longer than 10 characters so linkers couldn't
 * find the symbol.  Just hacked off the last "c" so it is really
 * ft_flastre().  Sorry, folks.  -- Glenn
 *
 *    Rev 1.0   01 Apr 1991 01:02:48   GLENN
 * Nanforum Toolkit
 *
 */

/*  Notes:

     The Clipper internal functions used seem to be stable across
     versions but nothing is guaranteed.  These functions begin
     with _t, are used for file I/O, and are compatible with their
     ANSI counterparts (just strip the _t and you have the ANSI name).
     See text.h for the prototypes.

     This revision utilizes the in-line assembler feature found in MSC
     6.0.  If compiling with TurboC substitute "_asm" with "asm".

     I compile these functions with the following MicroSoft C parameters:

          cl  /c /AL /Od /Zl /Zi /FPa /Gs /W3 fttext.c

     Note that the /Od defeats optimization and is necessary only for
     compatibility with Blinker, Warplink, etc.  If you are not overlaying
     this code you may want to change this to /Oalt.  Likewise, the
     /Zi is for symbolic debugging info which you will want to omit in
     any final compiles.

     Some sample Clipper code which would use these functions is listed
     below.  It will print out the contents of this file.

              ft_fuse( "text.c" )
              do while !ft_feof()
                 ? ft_freadln()
                 ft_fskip()
              enddo
              ft_fuse()

*/
/* up this number if you need more than 10 text file areas */
#define TEXT_WORKAREAS 10
/* raise or lower this number for best performance on your system
   (larger for dealing with large files or long records, smaller for
    faster reads) */
#define BUFFSIZE  4096

#include "hbapi.h"
#include "hbapifs.h"

HB_FUNC( FT_FUSE );
HB_FUNC( FT_FAPPEND );
HB_FUNC( FT_FSELECT );
HB_FUNC( FT_FGOBOTTOM );
HB_FUNC( FT_FRECNO );
HB_FUNC( FT_FERROR );
HB_FUNC( FT_FGOTOP );
HB_FUNC( FT_FSKIP );
HB_FUNC( FT_FREADLN );
HB_FUNC( FT_FWRITELN );
HB_FUNC( FT_FLASTREC );
HB_FUNC( FT_FBOF );
HB_FUNC( FT_FEOF );
HB_FUNC( FT_FGOTO );
HB_FUNC( FT_FDELETE );

/* routines internal to this module */
static int _findeol( BYTE *buf, int buf_len );
// static int _findbol( char *buf, int buf_len );

static int _ins_buff( int bytes );
static int _del_buff( int bytes );
static int _writeLine( char * theData, int iDataLen );
static LONG _ft_skip( LONG recs );

/* arrays used by the text workareas */
static int  area = 0;
static LONG recno   [TEXT_WORKAREAS];
static LONG offset  [TEXT_WORKAREAS];
static int  handles [TEXT_WORKAREAS];
static LONG last_rec[TEXT_WORKAREAS];
static LONG last_off[TEXT_WORKAREAS];
static LONG lastbyte[TEXT_WORKAREAS];
static int  isBof   [TEXT_WORKAREAS];
static int  isEof   [TEXT_WORKAREAS];
static int  error   [TEXT_WORKAREAS];
static char *crlf   [TEXT_WORKAREAS];
static int icrlf    [TEXT_WORKAREAS];
static BYTE* ftBuff = NULL;

/* standard macros */
#define __max(a,b)  (((a) > (b)) ? (a) : (b))
#define __min(a,b)  (((a) < (b)) ? (a) : (b))

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FUSE()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Open or close a text file for use by the FT_F* functions
 *  $SYNTAX$
 *
 *     FT_FUSE( [ <cFile> ] [, <nMode> ] ) -> nHandle | 0
 *
 *  $ARGUMENTS$
 *
 *     ^b<cFile>^n is the text file you want to open.  If not specified,
 *     the file currently open, if any, will be closed.
 *
 *     ^b<nMode>^n is the open mode for the file.  Please refer to the
 *     discussion of open modes under FOPEN() in the Clipper manual
 *     and FILEIO.CH for a list of allowable open modes.  If not
 *     specified, the file will be opened with a mode of
 *     FO_READ + FO_SHARED (64).
 *
 *  $RETURNS$
 *
 *     If ^b<cFile>^n is passed and the file is opened successfully, an
 *     integer containing the text file's workarea.  If the file cannot be
 *     opened, -1 will be returned.  In this case, check the return value
 *     of ^bft_fError()^n for the cause of the error.
 *
 *     If FT_FUSE() is called without any arguments, it will close the
 *     text file in the current "text area" and return 0.
 *
 *     If a read error occurs ^ft_fError()^n will contain the error code.
 *
 *  $DESCRIPTION$
 *
 *     The FT_F*() file functions are for reading text files, that is,
 *     files where each line (record) is delimited by a CRLF pair.
 *
 *     Each file is opened in its own "workarea", similar to the concept
 *     use by dbf files.  As provided, a maximum of 10 files (in 10
 *     workareas) can be opened (assuming there are sufficient file
 *     handles available).  That number may be increased by modifying
 *     the #define TEXT_WORKAREAS in the C source code and recompiling.
 *
 *  $EXAMPLES$
 *
 *     #include "fileio.ch"
 *
 *     // open a text file for reading
 *     ft_fUse( "text.txt" )
 *
 *     // open a text file for reading and writing
 *     ft_fUse( "text.txt", FO_READWRITE + FO_SHARED )
 *
 *     // close file
 *     ft_fUse()
 *
 *  $SEEALSO$
 *     FT_FUSE() FT_FSELECT()
 *  $END$
 */

HB_FUNC( FT_FUSE )
{
   int attr = ISNUM( 2 ) ? hb_parni(2) : 0;
   int iArea = 0;
   BOOL bRelease = TRUE;

   error[area] = 0;

   if ( ISCHAR(1) )
   {
      handles[area] = hb_fsOpen( (BYTE*) hb_parcx(1), attr ) ;

      if( ISCHAR(3) )
      {
         crlf[area]  = hb_parcx(3);
         icrlf[area] = hb_parclen(3);
      }
      else
      {
         crlf[area]  = hb_conNewLine();
         icrlf[area] = strlen( crlf[area] );
      }

      if( handles[area] <= 0 )
      {
         error[area] = hb_fsError();
      }
      else
      {
         if ( ftBuff == NULL )
         {
            ftBuff = (BYTE*) hb_xgrab( BUFFSIZE );
         }
      }

      offset[area] = 0 ;
      recno[area] = 1;
      lastbyte[area] = hb_fsSeek( handles[area], 0L, SEEK_END );
      hb_retni( handles[area] );
   }
   else
   {
      if ( handles[area] != 0 )
      {
         hb_fsClose( handles[area] );
         hb_retni(0);
         recno[area]    = 0L;
         offset[area]   = 0L;
         handles[area]  = 0;
         last_rec[area] = 0L;
         last_off[area] = 0L;
         lastbyte[area] = 0L;
         isEof[area]    = 0;
      }

      for ( ; iArea < TEXT_WORKAREAS - 1; iArea++ )
      {
         if ( handles[ iArea] != 0 )
         {
            bRelease = FALSE;
            break;
         }
      }

      if ( bRelease && ftBuff != NULL )
      {
         hb_xfree( ftBuff );
      }
   }
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FSELECT()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Select a text file workarea
 *  $SYNTAX$
 *
 *     FT_FSELECT( [ <nNewArea> ] ) -> nPreviousArea
 *
 *  $ARGUMENTS$
 *
 *     ^b<nNewArea>^n is the text file workarea to select.
 *
 *  $RETURNS$
 *
 *     The current selected text file area.
 *
 *  $DESCRIPTION$
 *
 *     This function selects a text file "workarea" from 1 to 10.  A
 *     file may or may not be open in the selected area.
 *
 *     Passing 0 for ^b<nNewArea>^n selects the next available workarea,
 *     similar to Clipper's SELECT 0 command.  If no more workareas are
 *     available the current workarea is not changed.
 *
 *     Each file is opened in its own "workarea", similar to the concept
 *     used by dbf files.  As provided, a maximum of 10 files (in 10
 *     workareas) can be opened (assuming there are sufficient file
 *     handles available).  That number may be increased by modifying
 *     the #define TEXT_WORKAREAS in the C source code and recompiling.
 *
 *     All the FT_F*() file functions operate on the file in the currently
 *     selected text file workarea.
 *
 *     Text file workareas are separate from and independent of Clipper's
 *     database workareas.
 *
 *  $EXAMPLES$
 *
 *     FT_FSELECT(1)
 *
 *     nFile1 := FT_FUSE( "temp.c" )
 *
 *     ? FT_FLASTREC()                 // no. of lines in temp.c
 *
 *     FT_FSELECT(2)
 *
 *     nFile2 := FT_FUSE( "temp.h" )
 *
 *     ? FT_FLASTREC()                 // no. of lines in temp.h
 *
 *  $SEEALSO$
 *     FT_FUSE()
 *  $END$
 */

HB_FUNC( FT_FSELECT )
{
   int oldarea = area + 1;
   int newArea;

   if ( ISNUM(1) )
   {
      newArea = hb_parni(1);

      if( newArea <= TEXT_WORKAREAS )
      {
         if ( newArea == 0 )
         {
            for ( ; newArea < TEXT_WORKAREAS - 1; newArea++ )
            {
               if ( handles[ newArea] == 0 )
               {
                  area = newArea;
                  break;
               }
            }
         }
         else
         {
            area = newArea - 1;
         }
      }
   }
   hb_retni( oldarea );
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FGOTOP()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Go to the first record in a text file
 *  $SYNTAX$
 *
 *     FT_FGOTOP() -> NIL
 *
 *  $ARGUMENTS$
 *
 *     None
 *
 *  $RETURNS$
 *
 *     NIL
 *
 *  $DESCRIPTION$
 *
 *     This function moves the record pointer to the first record
 *     in the currently selected text file workarea.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     FT_FUSE( "text.c" )      // open text file
 *
 *     DO WHILE !FT_FEOF()
 *
 *        ? FT_FREADLN()        // read thru file
 *
 *        FT_FSKIP()
 *
 *     ENDDO
 *
 *     FT_FGOTOP()              // go back to top
 *
 *     ? FT_FRECNO()            // 1
 *
 *  $SEEALSO$
 *     FT_FSELECT() FT_FUSE() FT_FRECNO() FT_FGOBOTTOM()
 *  $END$
 */

HB_FUNC( FT_FGOTOP )
{
   error[area]  = 0;
   offset[area] = 0L;
   recno[area]  = 1L;
   isBof[area]  = FALSE;
   isEof[area]  = FALSE;
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FERROR()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Return the error code for a text file operation
 *  $SYNTAX$
 *
 *     FT_FERROR() -> nErrorNo
 *
 *  $ARGUMENTS$
 *
 *     None
 *
 *  $RETURNS$
 *
 *     The DOS error code if one occurred.  See a reference on DOS error
 *     codes for an explanation of what the code means.
 *
 *  $DESCRIPTION$
 *
 *     This function returns the DOS error code associated with a file
 *     operation on the currently selected text file.
 *
 *     Errors could stem from any open, create, read or write operation,
 *     among others.
 *
 *  $EXAMPLES$
 *
 *     if ft_fUse( "text.c" ) < 0     // open text file
 *        err := ft_fError();
 *        QOUT( 'Error opening file "Text.c", error code (' + ;
 *                  LTRIM( STR( err ) ) + ')' )
 *     endif
 *
 *  $SEEALSO$
 *
 *  $END$
 */

HB_FUNC( FT_FERROR )
{
   hb_retni( error[area] );
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FRECNO()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Return the current record number of a text file
 *  $SYNTAX$
 *
 *     FT_FRECNO() -> nRecNo
 *
 *  $ARGUMENTS$
 *
 *     None
 *
 *  $RETURNS$
 *
 *     The current record number of a text file or 0 if no file is open.
 *
 *  $DESCRIPTION$
 *
 *     This function returns the current record number of the file open
 *     in the currently selected text file workarea.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     FT_FUSE( "text.c" )      // open text file
 *
 *     DO WHILE !FT_FEOF()
 *
 *        ? FT_FREADLN()        // read thru file
 *
 *        FT_FSKIP()
 *
 *     ENDDO
 *
 *     FT_FGOTOP()              // go back to top
 *
 *     ? FT_FRECNO()            // 1
 *
 *  $SEEALSO$
 *      FT_FSELECT() FT_FUSE() FT_FGOTOP() FT_FGOBOTTOM()
 *  $END$
 */

HB_FUNC( FT_FRECNO )
{
   hb_retnl( recno[area] );
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FGOBOTTOM()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Go to the last record in a text file
 *  $SYNTAX$
 *
 *     FT_FGOBOTTOM() -> NIL
 *
 *  $ARGUMENTS$
 *
 *     None
 *
 *  $RETURNS$
 *
 *     NIL
 *
 *  $DESCRIPTION$
 *
 *     This function moves the record pointer to the last record of the
 *     file in the currently selected text file workarea.
 *
 *     If a read error occurs ^ft_fError()^n will contain the error code.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     // read last line
 *     FT_FUSE( "text.c" )
 *
 *     FT_FGOBOTTOM()
 *
 *     ? FT_FREADLN()
 *
 *  $SEEALSO$
 *     FT_FSELECT() FT_FUSE() FT_FGOTOP() FT_FRECNO() FT_FREADLN()
 *  $END$
 */

HB_FUNC( FT_FGOBOTTOM )
{
   int  x;
   int  len;
   LONG loc;

   if ( last_rec[area] != 0 )
   {
      recno[area] = last_rec[area];
      offset[area] = last_off[area];
   }
   else
   {
      loc = 0L;

      do
      {
         hb_fsSeek( handles[area], offset[area], SEEK_SET );
         len = hb_fsRead( handles[area], ftBuff, BUFFSIZE );
         for ( x = 0; x < len; x++ )
         {
            if ( ((*(ftBuff + x) == 13) && (*(ftBuff + x + 1) == 10)) ||
                 (*(ftBuff + x) == 10) || ( x - loc > BUFFSIZE ) )
            {
               recno[area]++;
               x++;
               loc = x + 1;
            }
         }
         offset[area] += loc;

      } while ( len == BUFFSIZE );

      last_rec[area] = --recno[area];
      last_off[area] = offset[area];
   }
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FSKIP()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Move the record pointer to a new position in a text file
 *  $SYNTAX$
 *
 *     FT_FSKIP( [ <nLines> ] ) -> nLinesSkipped
 *
 *  $ARGUMENTS$
 *
 *     <nLines> is the number of lines to skip.  Defaults to 1 if
 *     not specified.
 *
 *  $RETURNS$
 *
 *     The number of lines actually skipped.  If the file's EOF or
 *     BOF was encountered before ^b<nLines>^n could be skipped, the
 *     return value will be less than ^b<nLines>^n.
 *
 *  $DESCRIPTION$
 *
 *     This function moves the text file record pointer, similar to
 *     the CLIPPER SKIP command.
 *
 *     Use the return value to determine how many records were actually
 *     skipped, for example to write a custom skipper function for
 *     TBrowse'g text files.
 *
 *     If a read error occurs ^ft_fError()^n will contain the error code.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *   $EXAMPLES$
 *
 *     // display each record of a text file
 *
 *     FT_FUSE( "text.c" )
 *
 *     DO WHILE ! FT_FEOF()
 *
 *        ? FT_FREADLN()
 *
 *        FT_FSKIP()
 *
 *     ENDDO
 *
 *
 *  $SEEALSO$
 *     FT_FRECNO() FT_FGOTOP()
 *  $END$
 */

HB_FUNC( FT_FSKIP )
{
   if ( ISNUM(1) )
   {
      if( hb_parnl(1) )
      {
         hb_retnl( _ft_skip( hb_parnl(1) ) );
      }
      else
      {
         hb_retnl( 0L );
      }
  }
  else
  {
     hb_retnl( _ft_skip(1L) );
  }
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FREADLN()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Read a line from the currently selected text file
 *  $SYNTAX$
 *
 *     FT_FREADLN() -> cLine
 *
 *  $ARGUMENTS$
 *
 *     None
 *
 *  $RETURNS$
 *
 *     A string containing the current record in a text file.
 *
 *  $DESCRIPTION$
 *
 *     This function returns a line of text read from the file in the
 *     currently selected text file workarea.  Text lines are delimited
 *     with a CRLF pair.  The record pointer is not moved.
 *
 *     Currently the maximum record size is 4096 characters.  You may
 *     increase the maximum record size by changing the value of ^b#define
 *     ^bBUFFSIZE^n in the C source and recompiling, however you should
 *     consider the performance implications if you do (all read and writes
 *     use this buffer size, including ft_fSkip()'s and ft_fGoto()'s).
 *
 *     If a read error occurs ^ft_fError()^n will contain the error code.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     // display each record of a text file
 *
 *     FT_FUSE( "text.c" )
 *
 *     DO WHILE ! FT_FEOF()
 *
 *        ? FT_FREADLN()
 *
 *        FT_FSKIP()
 *
 *     ENDDO
 *
 *  $SEEALSO$
 *     FT_FUSE() FT_FWRITELN() FT_FRECNO() FT_FGOTOP()
 *  $END$
 */

HB_FUNC( FT_FREADLN )
{
   int  x;
   LONG read;

   hb_fsSeek( handles[area], offset[area], SEEK_SET );
   read = hb_fsRead( handles[area], ftBuff, BUFFSIZE );

   for ( x = 0; x < BUFFSIZE; x++ )
   {
      if ( ((*(ftBuff + x) == 13) && (*(ftBuff + x + 1) == 10)) ||
           (*(ftBuff + x) == 26) || (*(ftBuff + x) == 10) || ( x >= (int)read) )
      {
         break;
      }
   }

   hb_retclen( (char*) ftBuff, x );
   // hb_retclenAdoptRaw( (char*) ftBuff, x );
   // hb_xfree( ftBuff );
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FWRITELN()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Write a line to the currently selected text file
 *  $SYNTAX$
 *
 *     FT_FWRITELN( < cData >, [ < lInsert > ] ) -> lSuccess
 *
 *  $ARGUMENTS$
 *
 *     <cData> is a string of data to write to the file at the current
 *      record position.
 *
 *     <lInsert> is a logical indicating whether the contents
 *     of the current record are to be preserved, that is, if lInsert
 *     evaluates to .T., the a new record is inserted at the current
 *     position.  The current record then is pushed down to FT_FRECNO()+1.
 *
 *     If lInsert is .F. or omitted, the current record is replaced by
 *     cData.
 *
 *  $RETURNS$
 *
 *     TRUE if successful, otherwise check ^ft_fError()^n for error code.
 *
 *  $DESCRIPTION$
 *
 *     This function writes a line of text to the file in the currently
 *     selected text file workarea.  Text lines are delimited with a
 *     CRLF pair.  The record pointer is not moved.
 *
 *     The contents of the current record are updated to reflect the new
 *     new line written, unless the Insert option is selected.
 *
 *     Writing a null string has the effect of clearing the current line
 *     if in overstrike mode, else inserting a new line (same as
 *     FT_FINSERT()).
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     // write a line of text to a file
 *
 *     FT_FUSE( "config.sys" )
 *
 *     DO WHILE UPPER( FT_FREADLN() ) != "FILES=" .AND. !F_FEOF()
 *
 *        FT_FSKIP()
 *
 *     ENDDO
 *
 *     FT_FWRITELN( "FILES=30", FT_FEOF() )
 *
 *  $SEEALSO$
 *     FT_FREADLN() FT_FRECNO() FT_FINSERT() FT_FDELETE()
 *  $END$
 */

HB_FUNC( FT_FWRITELN )
{
   char *theData  = hb_parcx( 1 );
   int   iDataLen = hb_parclen( 1 );
   int   lInsert  = ( ISLOG( 2 ) ? hb_parl( 2 ) : 0 );
   int   err;
   int   iLineLen = 0;
   int   iRead, iEOL;

   /* position file pointer to insertion point */
   hb_fsSeek( handles[area], offset[area], SEEK_SET );

   if( lInsert )
   {
     /* insert mode, insert the length of new string + crlf */
     err = _ins_buff( iDataLen + 2 );

     if( !err )
     {
        hb_fsSeek( handles[area], offset[area], SEEK_SET );
        err = _writeLine( theData, iDataLen );
     }
  }
  else
  {
     /* overwrite mode, determine how many bytes over/under */
     /* find length of current line, loop if longer than ftBuff */
     do
     {
        iRead = hb_fsRead( handles[area], (BYTE*) ftBuff, BUFFSIZE );
        iEOL  = _findeol( ftBuff, iRead );

        if( iEOL == 0 )
        {
           iLineLen += iRead;
        }
        else
        {
           iLineLen += iEOL;
           break;
        }
      } while( iRead == BUFFSIZE );

      if( (iDataLen+2) <= iLineLen )
      {
         /* delete excess bytes from current record */
         _del_buff( iLineLen - iDataLen );

         /* write the new record's contents */
         hb_fsWrite( handles[area], (BYTE*) theData, iDataLen );
      }
      else
      {
         /* insert extra bytes into current record */
         _ins_buff( iDataLen - iLineLen );

         /* write the new record's contents */
         hb_fsWrite( handles[area], (BYTE*) theData, iDataLen );
      }

      error[area] = hb_fsError();
      err = (error[area]) ? 0 : 1;
   }
   hb_retl( err );
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FDELETE()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Deletes a line from the currently selected text file
 *  $SYNTAX$
 *
 *     FT_FDELETE( [ < nLines > ] ) -> lSuccess
 *
 *  $ARGUMENTS$
 *
 *     ^b<nLines>^n is the number of lines to be eliminated, beginning with
 *     the current record position.
 *
 *     If ^b<nLines>^n is omitted, the current record is deleted only.
 *
 *  $RETURNS$
 *
 *     TRUE if successful, otherwise check ^ft_fError()^n for error code.
 *
 *  $DESCRIPTION$
 *
 *     This function deletes one or several lines of text from the file
 *     in the currently selected text file workarea.  Text lines are
 *     delimited with a CRLF pair.  The record pointer is not moved,
 *     unless the deleted lines occur at the end of the file, in which
 *     case ^bft_fRecno()^n will equal ^bft_fLastRe()^n and ^bft_fEOF()^n
 *     will be set to TRUE.
 *
 *  $EXAMPLES$
 *
 *     // delete the next 4 lines from a file
 *     FT_FUSE( "test.txt" )
 *
 *     FT_FDELETE( 4 )
 *
 *  $SEEALSO$
 *     FT_FAPPEND() FT_FRECNO() FT_FINSERT()
 *  $END$
 */

HB_FUNC( FT_FDELETE )
{
   int  iBytesRead ;
   LONG srcPtr     ;
   LONG destPtr    ;
   LONG cur_rec = recno[area];
   LONG cur_off = offset[area];

   /* save address to current record ( first record to be deleted ) */
   destPtr = offset[area] ;

   /* skip over deleted records, point to first 'to be retained' record */
   _ft_skip( ( ISNUM( 1 ) ? hb_parni( 1 ) : 1 ) ) ;
   srcPtr = hb_fsSeek( handles[area], offset[area], SEEK_SET );

   /* buffer read retained data, write atop old data */
   do
   {
      hb_fsSeek( handles[area], srcPtr, SEEK_SET );
      iBytesRead  = hb_fsRead( handles[area], ftBuff , BUFFSIZE );   /* now read in a big glob */
      srcPtr  += iBytesRead;
      hb_fsSeek( handles[area], destPtr, SEEK_SET );
      destPtr += hb_fsWrite( handles[area], ftBuff, iBytesRead );
   }  while( iBytesRead > 0 );

   /* move DOS EOF marker */
   hb_fsSeek( handles[area],  srcPtr, SEEK_SET );
   hb_fsWrite( handles[area], ftBuff, 0  );

   error[area] = hb_fsError();

   /* restore pointers */
   recno[area] = cur_rec;
   offset[area]= cur_off;

   /* re_calc EOF */
   lastbyte[area] = hb_fsSeek( handles[area], 0L, SEEK_END );
   _ft_skip( 0 );

   /* restore pointers again */
   recno[area] = cur_rec;
   offset[area]= cur_off;

   /* if we've deleted to EOF, leave EOF flag set, otherwise clear it */
   if( recno[area] != last_rec[area] )
   {
      isEof[area]  = FALSE;
   }

   hb_retl( (error[area]) ? 0 : 1 );
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FAPPEND()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Appends a line to the currently selected text file
 *  $SYNTAX$
 *
 *     FT_FAPPEND( [ < nLines > ] ) -> NIL
 *
 *  $ARGUMENTS$
 *
 *     <nLines> is the number of lines that should be appended to the
 *     end of the currently selected text file.
 *
 *     If <nLines> is omitted, one record is appended.
 *
 *  $RETURNS$
 *
 *     lSuccess.  If FALSE, check ^bft_fError()^n for the error code.
 *
 *  $DESCRIPTION$
 *
 *     This function appends a line of text to the file in the currently
 *     selected text file workarea.  Text lines are delimited with a
 *     CRLF pair.  The record pointer is moved to the last appended
 *     record.
 *
 *     Multiple lines may be appended with one call to FT_FAPPEND().
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *     Each line appended with this function will be empty.
 *
 *     NOTE:  Occasionally a text file may contain a non-CRLF terminated
 *     line, at the end of the file ("stragglers").  This function assumes
 *     these stragglers to be the last line of the file, and begins
 *     appending the new lines after this line.  In other words, if the
 *     last line in the text file is not terminated with a CRLF pair prior
 *     to calling FT_FAPPEND(), the function will terminate that last line
 *     before appending any new lines.
 *
 *  $EXAMPLES$
 *
 *     // add a blank line of text to a file
 *     FT_FUSE( "test.txt" )
 *
 *     ?FT_FRECNO()           // displays 5
 *
 *     FT_FAPPEND()
 *
 *     ?FT_FRECNO()           // displays 6
 *
 *  $SEEALSO$
 *     FT_FRECNO() FT_FDELETE() FT_FINSERT() FT_FLASTRE()
 *  $END$
 */

HB_FUNC( FT_FAPPEND )
{
   int   no_lines = ( ISNUM( 1 ) ? hb_parni( 1 ) : 1 );
   int   iRead;
   int   iByteCount;

   error[area] = 0;

   /* go to end of file */

   HB_FUNCNAME( FT_FGOBOTTOM )();

   /* find end of record */

   hb_fsSeek( handles[area], offset[area], SEEK_SET );
   iRead = hb_fsRead( handles[area], ftBuff, BUFFSIZE );   /* now read in a big glob */

   /* determine if CRLF pair exists, if not, add one */

   /* get count of chars in this line */
   iByteCount = _findeol( ftBuff, iRead );

   if( iByteCount == 0 )
   {
      hb_fsSeek( handles[area], 0, SEEK_END );
   }
   else
   {
      offset[area] = hb_fsSeek( handles[area], offset[area] + iByteCount, SEEK_SET );
      recno[area]++;
      no_lines--;
   }

   while( no_lines-- )
   {
      if( hb_fsWrite( handles[area], (BYTE *) crlf[area], icrlf[area] ) != icrlf[area] )
      {
         error[area] = hb_fsError();
         break;
      }

      recno[area]++;
      offset[area] = hb_fsSeek( handles[area], 0, SEEK_CUR );
      no_lines--;
    }

   if( !error[area] )
   {
      /* move DOS eof marker */
      hb_fsWrite( handles[area], (BYTE*) crlf[area], 0 );
      error[area] = hb_fsError();
   }

   /* force recalc of last record/offset */
   last_rec[area] = 0;

   hb_retl( (error[area]) ? 0 : 1 );

}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FINSERT()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Inserts a line in the currently selected text file
 *  $SYNTAX$
 *
 *     FT_FINSERT( [ < nLines > ] ) -> lSuccess
 *
 *  $ARGUMENTS$
 *
 *     ^b<nLines>^n is the number of lines that should be inserted at the
 *     current record position.
 *
 *     If ^b<nLines>^n is omitted, one record is inserted.
 *
 *  $RETURNS$
 *
 *     ^blSuccess^n is TRUE if the insert succeeded, FALSE if not.  If
 *     false check the return value of ^bft_fError()^n for the reason.
 *
 *  $DESCRIPTION$
 *
 *     This function inserts a line of text in the file in the currently
 *     selected text file workarea.  Text lines are delimited with a
 *     CRLF pair.
 *
 *     The record pointer is not moved.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *     Each line inserted with this function will be empty.
 *
 *  $EXAMPLES$
 *
 *     // add a couple of blank lines of text to a file
 *     ft fUse( "test.txt" )
 *
 *     ft_fGoTo( 10 )
 *
 *     ft_fInsert( 5 )
 *
 *  $SEEALSO$
 *     FT_FAPPEND() FT_FRECNO() FT_FDELETE() FT_FLASTREC()
 *  $END$
 */

HB_FUNC( FT_FINSERT )
{
   int    no_lines = ( ISNUM( 1 ) ? hb_parni( 1 ) : 1 );
   int    no_bytes = no_lines * 2 ;
   int    err   = 1;

   if( _ins_buff( no_bytes ) )
   {
      err = 0;
   }
   else
   {
      while( no_lines-- )
      {
         if( hb_fsWrite( handles[area], (BYTE*) crlf[area], icrlf[area] ) != icrlf[area] )
         {
            error[area] = hb_fsError();
            err = 0;
            break;
         }
      }
    }

    hb_retl( err );
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FLASTREC()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Get the no. of records in the currently selected text file
 *  $SYNTAX$
 *
 *     FT_FLASTREC() -> nLastRecordNum
 *
 *  $ARGUMENTS$
 *
 *     None
 *
 *  $RETURNS$
 *
 *     An integer containing the number of records in the text file in
 *     the currently selected text file workarea, or zero if no file
 *     is currently open in the workarea.
 *
 *  $DESCRIPTION$
 *
 *     This function returns the number of the last record in a text file.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     FT_FUSE( "text.c" )
 *
 *     ? FT_FLASTREC()
 *
 *  $SEEALSO$
 *     FT_FUSE() FT_FRECNO()
 *  $END$
 */

HB_FUNC( FT_FLASTREC )
{
   LONG cur_rec    = recno[area];
   LONG cur_offset = offset[area];

   HB_FUNCNAME( FT_FGOBOTTOM )();

   hb_retnl( last_rec[area] );

   recno[area]  = cur_rec;
   offset[area] = cur_offset;
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FEOF()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Determine if end of text file has been encountered
 *  $SYNTAX$
 *
 *     FT_FEOF() -> lResult
 *
 *  $ARGUMENTS$
 *
 *     None
 *
 *  $RETURNS$
 *
 *     .T. if an attempt was made to skip past the last record of
 *     the currently selected text file, otherwise .F.
 *
 *  $DESCRIPTION$
 *
 *     This function is similar to the CLIPPER Eof() function.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     FT_FUSE( "FTTEXT.C" )
 *
 *     ? FT_FEOF()        // .F.
 *
 *     FT_FSKIP()
 *
 *     ? FT_FEOF()        // .T.
 *
 *  $SEEALSO$
 *     FT_FUSE() FT_FSKIP()
 *  $END$
 */

HB_FUNC( FT_FEOF )
{
   hb_retl( isEof[area] );
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FBOF()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Determine if attempt to skip past beginning of text file
 *  $SYNTAX$
 *
 *     FT_FBOF() -> lResult
 *
 *  $ARGUMENTS$
 *
 *     None
 *
 *  $RETURNS$
 *
 *     .T. if an attempt was made to skip past the first record of
 *     the currently selected text file, otherwise .F.
 *
 *  $DESCRIPTION$
 *
 *     This function is similar to the CLIPPER Bof() function.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     FT_FUSE( "FTTEXT.C" )
 *
 *     FT_FGOTOP()
 *
 *     ? FT_FBOF()        // .F.
 *
 *     FT_FSKIP(-1)
 *
 *     ? FT_FBOF()        // .T.
 *
 *  $SEEALSO$
 *     FT_FSKIP() FT_EOF() FT_GOTOP()
 *  $END$
 */

HB_FUNC( FT_FBOF )
{
   hb_retl( isBof[area] );
}

/*  $DOC$
 *  $FUNCNAME$
 *     FT_FGOTO()
 *  $CATEGORY$
 *     File I/O
 *  $ONELINER$
 *     Move record pointer to specific record in a text file
 *  $SYNTAX$
 *
 *     FT_FGOTO( nLine ) -> NIL
 *
 *  $ARGUMENTS$
 *
 *     <nLine> is the record number to go to.
 *
 *  $RETURNS$
 *
 *     NIL
 *
 *  $DESCRIPTION$
 *
 *     This function moves the record pointer to a specific record
 *     in the file in the currently selected text file workarea.  If
 *     the record number requested is greater than the number of records
 *     in the file, the record pointer will be positioned at the last
 *     record.
 *
 *     Internally, the function operates differently depending on how
 *     you invoke it.  Passing a value for ^b<nLine>^n results in what
 *     is effectively a skip operation, which is fairly quick.  However
 *     if you pass 0 for ^b<nLine>^n, e.g. ft_fGoTo( 0 ), the function
 *     internally goes to the top of the file, then skips down the
 *     required number of records.  Hence if your file is relatively
 *     large and the current record is a high number, you may see some
 *     delay as ft_fGoTo(0) skips through the file.
 *
 *     A text file "record" is a line of text terminated by a CRLF pair.
 *
 *  $EXAMPLES$
 *
 *     // read 5th line of text from file
 *
 *     ft_fUse( "FTTEXT.C" )
 *
 *     ft_fGoTo(5)
 *
 *     cText := ft_fReadLN()
 *
 *  $SEEALSO$
 *
 *    FT_FRECNO() FT_FGOTOP() FT_FREADLN()
 *  $END$
 */

HB_FUNC( FT_FGOTO )
{
   LONG target = hb_parnl(1);

   /* if a recno was passed, do a relative skip */
   if( target )
   {
      /* skip relative */
      target -= recno[area];

      if( target )
      {
        _ft_skip( target );
      }
   }
   else
   {
      /* goto 0 passed, go top then skip back */
      target = recno[area];

      offset[area] = 0L;
      recno[area]  = 1L;
      isBof[area]  = FALSE;
      isEof[area]  = FALSE;

      if( --target )
      {
         _ft_skip( target );
      }
   }
   error[area] = hb_fsError();
}

/*컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴*/
/* deletes xxx bytes from the current file, beginning at the current record */
static int _del_buff( int iLen )
{

  LONG  fpRead, fpWrite;
  int   WriteLen;
  int   SaveLen;

  /* initialize file pointers */
  fpWrite = offset[area];
  fpRead  = offset[area] + iLen;

  /* do initial load of buffer */
  hb_fsSeek( handles[area], fpRead, SEEK_SET );
  WriteLen = hb_fsRead( handles[area], (BYTE*) ftBuff, BUFFSIZE );
  fpRead += WriteLen;

  error[area] = 0;

  while( WriteLen > 0 )
  {
     /* position to beginning of write area */
     hb_fsSeek( handles[area], fpWrite, SEEK_SET );
     SaveLen = hb_fsWrite( handles[area], (BYTE*) ftBuff, WriteLen );

     /* move write pointer */
     fpWrite += SaveLen;

     if(  SaveLen != WriteLen )
     {
        /* error, fetch errcode and quit */
        error[area] = hb_fsError();
        break;
     }

     /* return to read area and read another buffer */
     hb_fsSeek( handles[area], fpRead, SEEK_SET );
     WriteLen = hb_fsRead( handles[area], (BYTE*) ftBuff, BUFFSIZE );
     fpRead  += WriteLen;
   }

   /* store length in bytes, set EOF marker for DOS */
   lastbyte[area] = hb_fsSeek( handles[area], fpWrite, SEEK_SET );
   hb_fsWrite( handles[area], (BYTE*) ftBuff, 0 );

   /* clear last_rec so next gobot will recount the records */
   last_rec[area] = 0L;
   hb_fsSeek( handles[area], offset[area], SEEK_SET );

   return error[area];
}
// _del_buff

/*컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴*/
/* writes a line of data to the file, including the terminating CRLF */
static int _writeLine( char * theData, int iDataLen )
{
   int err = 0;

   if( !( hb_fsWrite( handles[area], (BYTE*) theData, iDataLen ) == iDataLen ) )
   {
      err = 1;
      error[area] = hb_fsError();
   }
   else if( !( hb_fsWrite( handles[area], (BYTE*) crlf[area], icrlf[area] ) == icrlf[area] ) )
   {
      err = 1;
      error[area] = hb_fsError();
   }

   return err;
}
// _writeLine

/*  fttext.c  eof */

/*컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴*/
/* inserts xxx bytes into the current file, beginning at the current record */
/* the contents of the inserted bytes are indeterminate, i.e. you'll have to
     write to them before they mean anything */
static int _ins_buff( int iLen )
{
  BYTE *ReadBuff  = (BYTE *) hb_xgrab(BUFFSIZE);
  BYTE *WriteBuff = (BYTE *) hb_xgrab(BUFFSIZE);
  BYTE *SaveBuff;
  LONG  fpRead, fpWrite;
  int   WriteLen, ReadLen;
  int   SaveLen;
  int   iLenRemaining = iLen;

  /* set target move distance, this allows iLen to be greater than
     BUFFSIZE */
  iLen = __min( iLenRemaining, BUFFSIZE );
  iLenRemaining -= iLen;

  /* initialize file pointers */
  fpRead = offset[area];
  fpWrite= offset[area] + iLen;

  /* do initial load of both buffers */
  hb_fsSeek( handles[area], fpRead, SEEK_SET );
  WriteLen = hb_fsRead( handles[area], WriteBuff, BUFFSIZE );
  fpRead += WriteLen;

  ReadLen = hb_fsRead( handles[area], ReadBuff, BUFFSIZE );
  fpRead += ReadLen;

  error[area] = 0;

  while( !error[area] && iLen > 0 )
  {
    while( WriteLen > 0 )
    {
      /* position to beginning of write area */
      if( hb_fsSeek( handles[area], fpWrite, SEEK_SET ) != (ULONG) fpWrite )
      {
        error[area] = hb_fsError();
        break;
      }

      SaveLen = hb_fsWrite( handles[area], WriteBuff, WriteLen );

      if( !SaveLen )
      {
        error[area] = hb_fsError();
        break;
      }

      /* move write pointer */
      fpWrite += SaveLen;

      if(  SaveLen != WriteLen )
      {
        /* error, fetch errcode and quit */
        error[area] = hb_fsError();
        break;
      }
      // WriteLen = SaveLen;

      /* swap buffers */
      SaveBuff  = WriteBuff;
      WriteBuff = ReadBuff ;
      ReadBuff  = SaveBuff ;
      WriteLen  = ReadLen  ;

      /* return to read area and read another buffer */
      hb_fsSeek( handles[area], fpRead, SEEK_SET );
      ReadLen = hb_fsRead( handles[area], ReadBuff, BUFFSIZE );
      fpRead += ReadLen;
    }

    iLen = __min( iLenRemaining, BUFFSIZE );
    iLenRemaining -= iLen;
  }

   /* store length in bytes, set EOF marker for DOS */
   lastbyte[area] = hb_fsSeek( handles[area], fpWrite, SEEK_SET );
   hb_fsWrite( handles[area], WriteBuff, 0 );

   /* clear last_rec so next gobot will recount the records */
   last_rec[area] = 0L;
   hb_fsSeek( handles[area], offset[area], SEEK_SET );

   hb_xfree( ReadBuff  );
   hb_xfree( WriteBuff );

   return error[area];
}
// _ins_buff

/* internal routine to do buffer skips.  Passing a positive value performs
   a downward skip, a negative number does an upward skip.  Passing 0
   skips to the end of file.
   Returns a LONG indicating the number of records skipped */

static LONG _ft_skip( LONG recs )
{
   LONG read_pos;
   LONG read_len;
   LONG x, y;
   int i = 0;

   if ( recs > 0 )
   {
      for (y = 0; y < recs; y++ )
      {
         hb_fsSeek( handles[area], offset[area], SEEK_SET );
         read_len = hb_fsRead( handles[area], ftBuff, BUFFSIZE );
         for (x = 0; x < read_len; x++ )
         {
            if ((*(ftBuff + x) == 13) && (*(ftBuff + x + 1) == 10)) 
            {
               i = 0;
               break;
            }
            else if (*(ftBuff + x) == 10)
            {
               i = 1;
               break;
            }
         }

         if ( (offset[area] + x + icrlf[area] - i ) < lastbyte[area] )
         {
            isEof[area] = FALSE;
            offset[area] += ( x + icrlf[area] - i );
            recno[area] += 1;
         }
         else
         {
            isEof[area] = TRUE;
         }
      }
   }
   else
   {
      recs = -recs;
      isEof[area] = FALSE;

      if ( (recno[area] - recs) < 1 )
      {
         return( 1 );
      }

      for (y = recs; y > 0; y-- )
      {
         if ( offset[area] - BUFFSIZE < 0L )
         {
            read_pos = 0;
            read_len = (size_t)offset[area];
         }
         else
         {
            read_pos = (size_t)(offset[area] - BUFFSIZE);
            read_len = BUFFSIZE;
         }

         hb_fsSeek( handles[area], read_pos, SEEK_SET );
         read_len = hb_fsRead( handles[area], ftBuff, ( USHORT )read_len );

         for (x = read_len - 4; x >= 0; x-- )
         {
            if ( ((*(ftBuff + x) == 13) && (*(ftBuff + x + 1) == 10)) ||
                 (*(ftBuff + x) == 10) )
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
            offset[area] = read_pos + x + icrlf[area] - i;
            recno[area]--;
         }
      }
   }

   return ( recno[area] );
}

/*----------------------------------------------------------------------
_findeol()  -  In-line assembler routine to parse a buffer
               for a CRLF pair

               Returns count to first character _after_ next
               CRLF pair (beginning of next line).  Current line
               will contain the trailing CRLF.  1Ah and trailing
               LFs will be ignored (included in count).

               If no CRLF found return is zero.  (could mean EOF or
               line is longer than buffer end)
------------------------------------------------------------------------*/
static int _findeol( BYTE *buf, int buf_len )
{
   USHORT iByteCount = 0;
   USHORT ui;

   for( ui = 0; ui < buf_len; ui ++ )
   {
      if( buf[ ui ] == 13 || buf[ ui ] == 10 )
      {
         if( buf [ ui ] == 10 || buf [ ui + 1 ] == 10 )
         {
            break;
         }
      }
      else if ( buf[ ui ] == EOF )
      {
         break;
      }

      iByteCount ++;
   }

   return( iByteCount );
}     /* end _findeol() */

/*----------------------------------------------------------------------
_findbol()  -  In-line assembler routine to parse a buffer
               for a CRLF pair

               buf pointer points at beginning of search (end
               of the buffer), all searches are conducted
               backwards, returns No. of characters betw.
               initial position and first character _after_
               the preceding CRLF pair (beginning of line).
------------------------------------------------------------------------*/
//static int _findbol( char *buf, int buf_len )
//{
//   return( 0 );
//}     /* end _findbol() */
