/*
 * $Id$
 */

/*; File......: MKDIR.ASM
   ; Author....: Ted Means
   ; CIS ID....: 73067,3332
   ;
   ; This is an original work by Ted Means and is placed in the
   ; public domain.
   ;
   ; Modification history:
   ; ---------------------
   ;
   ;     Rev 1.2   15 Aug 1991 23:06:58   GLENN
   ;  Forest Belt proofread/edited/cleaned up doc
   ;
   ;     Rev 1.1   14 Jun 1991 19:54:44   GLENN
   ;  Minor edit to file header
   ;
   ;     Rev 1.0   01 Apr 1991 01:03:32   GLENN
   ;  Nanforum Toolkit
   ;
   ;
 */

/*  $DOC$
 *  $FUNCNAME$
 *     FT_MKDIR()
 *  $CATEGORY$
 *     DOS/BIOS
 *  $ONELINER$
 *     Create a subdirectory
 *  $SYNTAX$
 *     FT_MKDIR(  <cDirName> ) -> nResult
 *  $ARGUMENTS$
 *     <cDirName> is the name of the directory to create.
 *  $RETURNS$
 *      0  if successful
 *      3  if Path Not Found
 *      5  if Access Denied or directory already exists
 *     99  if invalid parameters passed
 *  $DESCRIPTION$
 *     Use this function to create the subdirectories needed by your
 *     application.  It might be especially useful in an installation
 *     program.
 *
 *     The source code is written to adhere to Turbo Assembler's IDEAL mode.
 *     To use another assembler, you will need to rearrange the PROC and
 *     SEGMENT directives, and also the ENDP and ENDS directives (a very
 *     minor task).
 *  $EXAMPLES$
 *     FT_MKDIR( "C:\CLIPPER" )
 *     FT_MKDIR( "\EXAMPLE" )
 *     FT_MKDIR( "..\SOURCE" )
 *  $END$
 */

/*This  is the Original FT_CHDIR() code
   IDEAL
   MODEL HUGE
   Public   _HB_FUN_FT_MKDIR

   Extrn    _hb_ftdir:Far

   Segment  _NanFor   Word      Public    "CODE"
         Assume    CS:_NanFor

   Proc     _HB_FUN_FT_MKDIR  Far

         Mov       AH,39h                    * DOS service--create directory
         Push      AX                        * Save on stack
         Call      _hb_ftdir                   * Call generic directory routine
         Add       SP,2                      * Realign stack
         Ret
   Endp     _HB_FUN_FT_MKDIR
   Ends     _NanFor
   End
 */

/* This is the New one Rewriten in C*/

#include "hbapi.h"
#include "hbapifs.h"
#if defined( HB_OS_DOS )
   #include <dos.h>
   #if defined( __DJGPP__ )
      #include <errno.h>
      #include <sys/stat.h>
   #endif
#elif defined( __WIN32__ )
   #include "hbapi.h"
   #include "hbapifs.h"
   #include <windows.h>
#endif

HB_FUNC( FT_MKDIR )
{
#if defined( HB_OS_DOS )
   #if defined( __DJGPP__ )
   if( mkdir( hb_parcx( 1 ), S_IWUSR ) )
   {
      hb_retni( ( errno == EINVAL ) ? 99 : ( ( errno == EACCES ) ? 5 : 3 ) );
   }
   else
   {
      hb_retni( 0 );
   }
   #else
   int            Status;
   char *         path = hb_parcx( 1 );
   union REGS     regs;
   struct SREGS   sregs;

   segread( &sregs );
   regs.h.ah         = 0x39;
   sregs.ds          = FP_SEG( path );
   regs.HB_XREGS.dx  = FP_OFF( path );
   HB_DOS_INT86X( 0x21, &regs, &regs, &sregs );
   Status            = regs.HB_XREGS.ax;
   hb_retni( Status );
   #endif
#elif defined( __WIN32__ )
   UINT iResult;

   hb_fsMkDir( ( const char * ) hb_parcx( 1 ) );

   iResult = ( UINT ) GetLastError();

   if( iResult == ERROR_ALREADY_EXISTS )
   {
      iResult = 5;
   }
   else if( iResult == ERROR_BAD_PATHNAME )
   {
      iResult = 99;
   }

   hb_retni( iResult );
#else
   BOOL  bResult;
   int   iRet = 0;
   if( ISCHAR( 1 ) )
   {
      bResult = hb_fsMkDir( ( const char * ) hb_parcx( 1 ) );
      if( ! bResult )
      {
         iRet = 5;
      }
   }
   else
   {
      iRet = 99;
   }
   hb_retni( iRet );
#endif
}
