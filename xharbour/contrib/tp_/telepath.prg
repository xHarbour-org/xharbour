/*
 * $Id: telepath.prg,v 1.3 2004/08/16 14:37:13 mauriliolongo Exp $
 */

/*
 * Harbour Project source code:
 * Telepathy emulation library
 *
 * Copyright 2000, 2001 Dan Levitt <dan@boba-fett.net>
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
   This is based upon a library originally made by Dan Levitt <dan@boba-fett.net>
   (see README and ChangeLog). The original files have been committed as v1.0,
   so you can always retrieve them (see CVS docs on how to)

   First xHarbour Port by Luiz Rafael Culik Guimaraes (culikr@brturbo.com)
*/


#pragma begindump
   #include "hbapi.h"
   #include <sys/ioctl.h>
   #include <unistd.h>
   #include <sys/stat.h>
   #include <fcntl.h>
#pragma enddump

#define TP_MAXPORTS  8

#define TPFP_NAME    1
#define TPFP_HANDLE  2
#define TPFP_BAUD    3
#define TPFP_DBITS   4
#define TPFP_PARITY  5
#define TPFP_SBITS   6
#define TPFP_OC      7
#define TPFP_INBUF   8

#xtranslate DEFAULT <a> to <b> => <a> := iif( valtype( <a> ) == "U", <b>, <a> )

static aPorts

function tp_inkey( nSecs )
   if valtype( nSecs ) == "U"
      return inkey()
   endif
return inkey( nSecs )

function tp_idle( lNewval )
   if lNewval == .t.
      return .t.
   endif
return .f.

function tp_delay( insecs )
    LOCAL nseconds := 0

   if valtype( insecs ) != "N"
      insecs := 0
   endif

   if insecs <= 0
      return nil
   endif

   nseconds := insecs
    HB_INLINE(nseconds)
    {
       double nseconds = hb_parnd(1);

       usleep( nseconds * 1000000 );
   }

return nil

function tp_close( nPort )
   if ! isopenport( nPort )
      return 1
   endif

   fclose( aPorts[ nPort, TPFP_HANDLE ])

   aPorts[ nPort, TPFP_NAME   ] := ""
   aPorts[ nPort, TPFP_HANDLE ] := -1
   aPorts[ nPort, TPFP_BAUD   ] := 9600
   aPorts[ nPort, TPFP_DBITS  ] := 8
   aPorts[ nPort, TPFP_PARITY ] := "N"
   aPorts[ nPort, TPFP_SBITS  ] := 1
   aPorts[ nPort, TPFP_OC     ] := "C"
   aPorts[ nPort, TPFP_INBUF  ] := ""
return 0

function tp_reopen( cPortname, nPort, nBaud, nData, cParity, nStop )
return tp_open( cPortname, nPort, nBaud, nData, cParity, nStop )

function tp_open( cPortname, nPort, nBaud, nData, cParity, nStop )
    LOCAL cnhandle
   local cCommand
   local lOldFile, lOldPath, lOldLower

   if empty( cPortname )
      tp_debug( 2, "You suck trying to open port num " + alltrim( str( nPort )) )
      return 1
   endif

   if valtype( cPortname ) != "C"
      tp_debug( 2, "You suck trying to open port name which isn't a type C" )
      return 1
   endif

   cPortname := alltrim( cPortname )

   if ! file( cPortname )
      tp_debug( 2, "You suck trying to open port name " + alltrim( cPortname ))
      return 1
   endif

   if ! isport( nPort )
      tp_debug( 2, "Hmmm... Port num " + alltrim( str( nPort )) + " ain't a port" )
      return 1
   endif

   if isopenport( nPort )
      tp_debug( 2, "You sucks... port num " + alltrim( str( nPort )) + " was already open" )
      tp_close( nPort )
   endif

   default nBaud to 9600
   default nData to 8
   default cParity to "N"
   default nStop to 1

   aPorts[ nPort, TPFP_NAME   ] := cPortname
   aPorts[ nPort, TPFP_BAUD   ] := nBaud
   aPorts[ nPort, TPFP_DBITS  ] := nData
   aPorts[ nPort, TPFP_PARITY ] := cParity
   aPorts[ nPort, TPFP_SBITS  ] := nStop
   aPorts[ nPort, TPFP_OC     ] := "O"
   aPorts[ nPort, TPFP_INBUF  ] := ""

   cCommand := "stty -echo raw " + alltrim( str( nBaud )) + " "

   /// 7/19/01 20:02:46 DDGL If the com port had previously been used with 7 bit data and is
   /// now used with 8 bits, it would not set it to 8.  it previously assumed that it
   /// was already 8.  d'oh.
   if nData == 7
      cCommand += "cs7 "
   else
      cCommand += "cs8 "
   endif

   /// 7/19/01 20:02:46 DDGL If the com port had previously been used with parity and then
   /// later used with no parity, it would not set it to none.  it previously assumed that it
   /// was already none.  d'oh.
   if cParity == "E"
      cCommand += "-parodd parenb "
   elseif cParity == "O"
      cCommand += "parodd parenb "
   else
      cCommand += "-parodd -parenb "
   endif

   /// 7/20/01 10:13:24 DDGL I suppose this was ok before.  But now if they pass something
   /// unexpected as nStop, they will get 1 which is probably what they want.
   if nStop == 2
      cCommand += "cstopb "
   else
      cCommand += "-cstopb "
   endif

   cCommand += " < " + cPortname

   lOldfile := FS_SET( "lowerfile", .f. )
   lOldpath := FS_SET( "lowerpath", .f. )
   lOldlower:= FS_SET( "lower",     .f. )

   cnHandle := fopen( cPortname, 2 )

   FS_SET( "lowerfile", lOldfile  )
   FS_SET( "lowerpath", lOldpath  )
   FS_SET( "lower",     lOldlower )

   if cnHandle > 0
      // success!
      tp_debug( 2, "I got port " + alltrim( str( nPort )) + " open as handle " + alltrim( str( cnHandle )) )
      aPorts[ nPort, TPFP_HANDLE ] := cnhandle

      tp_debug( 2, "Going to run: " + cCommand )
      run( cCommand )

      tp_recv( nPort )
      return 0
   endif

   if ! "/dev/ttyS" $ cPortname
      cnHandle := ferror()
   endif

   tp_debug( 2, "FAILed to open " + cPortname + " as port " + alltrim( str( nPort )) + ".  Error " + alltrim( str( ferror() )) )
   aPorts[ nPort, TPFP_NAME   ] := ""
   aPorts[ nPort, TPFP_HANDLE ] := -1
   aPorts[ nPort, TPFP_BAUD   ] := 9600
   aPorts[ nPort, TPFP_DBITS  ] := 8
   aPorts[ nPort, TPFP_PARITY ] := "N"
   aPorts[ nPort, TPFP_SBITS  ] := 1
   aPorts[ nPort, TPFP_OC     ] := "C"
   aPorts[ nPort, TPFP_INBUF  ] := ""
return 1

function tp_recv( nPort, nLength, nTimeout )
   local nDone
   local cRet := ""

   default nLength to 64000
   default nTimeout to 0

   _readfromporttobuffer( nPort )

   if ntimeout < 0
      nTimeout := 999999
   endif

   if nTimeout > 0
      nDone := _clock() + ntimeout
      while len( aPorts[ nPort, TPFP_INBUF ] ) < nLength .and. _clock() < nDone
         if tp_idle()
            exit
         endif

         _readfromporttobuffer( nPort )
      enddo
   endif

   if nLength > len( aPorts[ nPort, TPFP_INBUF ] )
      cRet := aPorts[ nPort, TPFP_INBUF ]
      aPorts[ nPort, TPFP_INBUF ] := ""
   else
      cRet := substr( aPorts[ nPort, TPFP_INBUF ], 1, nLength )
      aPorts[ nPort, TPFP_INBUF ] := substr( aPorts[ nPort, TPFP_INBUF ], nLength + 1 )
   endif
return cRet

function tp_send( nPort, cString, nTimeout )
   local x

   if ! isopenport( nPort )
      tp_debug( 2, "tp_send: unopen port " + alltrim( str( nPort )) + ". str " + cString )
      return 1
   endif

   // timeout is ignored... oh well

   if len( cString ) < 1
      tp_debug( 2, "tp_send: Cannot send nothing." )
      return 0
   endif
return fwrite( aPorts[ nPort, TPFP_HANDLE ], cString )

function tp_recvto( nPort, cDelim, nMaxlen, nTimeout )
   local x
   local cChar
   local nAt
   local nFirst := 100000
   local nDone

   if ! isopenport( nPort )
      return 1
   endif

   if valtype( cDelim ) != "C" .or. empty( cDelim )
      return ""
   endif

   default nMaxlen to 64000
   default nTimeout to 0

   if ntimeout < 0
      nDone := _clock() + 999999
   elseif ntimeout == 0
      nDone := 4
   else
      nDone := _clock() + nTimeout
   endif

   while ( nDone > _clock() .or. nFirst == 100000 ) .and. ! tp_idle()
      if nFirst == 100000
         nFirst := 99999
      endif

      _readfromporttobuffer( nPort )

      for x := 1 to len( cDelim )
         cChar := substr( cDelim, x, 1 )
         nAt := at( cChar, aPorts[ nPort, TPFP_INBUF ] )
         if nAt > 0 .and. nAt < nFirst
            nFirst := nAt
         endif
      next

      if nFirst < 64000
         exit
      endif
        HB_INLINE()
        {
         sched_yield();
        }
   enddo

   if nFirst < 64000
      return tp_recv( nPort, nFirst )
   endif
return ""

/// here's an improvement over original TP... you can "lookfor" a string
/// here rather than just a char.  yay me.
/// of course, if you're using clipper/tp code and you search for a single char it will work
/// the same.
function tp_lookfor( nPort, cLookfor )
   if ! isopenport( nPort )
      return 0
   endif
   _readfromporttobuffer( nPort )
return at( cLookfor, aPorts[ nPort, TPFP_INBUF ] )

function tp_inchrs( nPort )
   if ! isopenport( nPort )
      return 0
   endif
   _readfromporttobuffer( nPort )
return len( aPorts[ nPort, TPFP_INBUF ] )

function tp_clearin( nPort )
   tp_recv( nPort )
return 0

function tp_clrkbd
   clear typeahead

   // while inkey() != 0
   // enddo
return 0


/// sorry, no waitproc at this time.
function tp_waitfor( nPort, nTimeout, acList, lIgnorecase )
   local x
   local nAt
   local nFirst := 100000
   local nDone
   local nRet := 0

   if ! isopenport( nPort )
      return 0
   endif

   if valtype( acList ) != "A"
      return 0
   endif

   default nTimeout to -1
   default lIgnorecase to .f.

   if ntimeout < 0
      nDone := _clock() + 999999
   elseif ntimeout == 0
      nDone := 4
   else
      nDone := _clock() + nTimeout
   endif

   while ( nDone > _clock() .or. nFirst == 100000 ) .and. ! tp_idle()
      if nFirst == 100000
         nFirst := 99999
      endif

      _readfromporttobuffer( nPort )

      for x := 1 to len( acList )
         if lIgnorecase
            nAt := at( upper( acList[ x ] ), upper( aPorts[ nPort, TPFP_INBUF ] ))
         else
            nAt := at( acList[ x ] , aPorts[ nPort, TPFP_INBUF ] )
         endif
         if nAt > 0 .and. nAt < nFirst
            nFirst := nAt
            nRet := x
         endif
      next

      if nFirst < 64000
         exit
      endif
        hb_inline()
        {
         sched_yield();
        }
   enddo

   if nFirst < 64000
      tp_recv( nPort, nAt + len( acList[ nRet ] ))
      return nRet
   endif
return 0

// telepathy says...
// returns old rts value 0,1,2
// sets to 0 = rts off, 1 dtr on, 2 = dtr flow control autotoggle
// I don't support 2.  who uses dtr for flow control anyway...
function tp_ctrlrts( nPort, nParamNewval )
    LOCAL nph, nnewval, noldval


   if ! isopenport( nPort )
      return -1
   endif
   nph := aPorts[ nPort, TPFP_HANDLE ]
HB_INLINE( nph ,@nnewval, @noldval)
   {
        double nph = hb_parnd( 1 );
        double nnewval, noldval;
      unsigned int rtsresult = 0;
      ioctl( nph, TIOCMGET, &rtsresult );
      if ( rtsresult & TIOCM_RTS )
         noldval = 1;
      else
         noldval = 0;

      if ( noldval != nnewval )
      {
         if ( nnewval == 0 )
         {
            rtsresult &= ~TIOCM_RTS;
            ioctl( nph, TIOCMSET, &rtsresult );
         }
         else if ( nnewval == 1 )
         {
            rtsresult |= TIOCM_RTS;
            ioctl( nph, TIOCMSET, &rtsresult );
         }
         /// if newval == 2?  uhhhhhhh

       }
       hb_stornd(nnewval,2);
       hb_stornd(noldval,3);
   }

   if nNewval == 2
      run( "stty crtscts < " + aPorts[ nPort, TPFP_NAME ] )
   endif
return noldval

function tp_isri( nPort )
    LOCAL rinph, riretval

   if ! isopenport( nPort )
      return .f.
   endif
   rinph := aPorts[ nPort, TPFP_HANDLE ]

    HB_INLINE(rinph,@riretval)
   {
        double rinph= hb_parnd( 1 ) ;
        double nnewval, noldval;
      unsigned int riresult = 0;
      ioctl( rinph, TIOCMGET, &riresult );
        if ( riresult & TIOCM_RI )
        {
         riretval = 1;
        }
      else
        {
         riretval = 0;
        }
    hb_stornd(riretval,2);
   }

return ( riretval == 1 )

// telepathy says...
// returns old dtr value 0,1,2
// sets to 0 = dtr off, 1 dtr on, 2 = dtr flow control autotoggle
// I don't support 2.  who uses dtr for flow control anyway...
function tp_ctrldtr( nPort, nParamNewval )
    LOCAL nph, nnewval, noldval

   if ! isopenport( nPort )
      return -1
   endif
   nph := aPorts[ nPort, TPFP_HANDLE ]
   HB_INLINE(nph, @nnewval, @noldval)
   {
        double nph = hb_parnd(1);
        double nnewval, noldval;
      unsigned int result = 0;
      ioctl( nph, TIOCMGET, &result );
      if ( result & TIOCM_DTR )
         noldval = 1;
      else
         noldval = 0;

      if ( noldval != nnewval )
      {
         if ( nnewval == 0 )
            result &= ~TIOCM_DTR;
         else
            result |= TIOCM_DTR;

         ioctl( nph, TIOCMSET, &result );
      }
        hb_stornd(nnewval,2);
        hb_stornd(noldval,3);
   }

return noldval

function tp_isdcd( nPort )
    LOCAL nph, nretval

   if ! isopenport( nPort )
      return .f.
   endif
   nph := aPorts[ nPort, TPFP_HANDLE ]
    hB_inline(nph,@nretval)
   {
        double nph = hb_parnd( 1 ) ;
        double nretval;
      unsigned int result = 0;
      ioctl( nph, TIOCMGET, &result );
      if ( result & TIOCM_CD )
         nretval = 1;
      else
         nretval = 0;
    hb_stond(nretval,2);
   }

return ( nretval == 1 )

function tp_isdsr( nPort )
    LOCAL nph, nretval

   if ! isopenport( nPort )
      return .f.
   endif
   nph := aPorts[ nPort, TPFP_HANDLE ]
    hB_inline(nph,@nretval)
   {
        double nph = hb_parnd( 1 ) ;
        double nretval;
      unsigned int result = 0;
      ioctl( nph, TIOCMGET, &result );
      if ( result & TIOCM_DSR )
         nretval = 1;
      else
         nretval = 0;
    hb_stond(nretval,2);
   }
return ( nretval == 1 )


function tp_iscts( nPort )
    LOCAL nph, nretval

   if ! isopenport( nPort )
      return .f.
   endif
   nph := aPorts[ nPort, TPFP_HANDLE ]
    hB_inline(nph,@nretval)
   {
        double nph = hb_parnd( 1 ) ;
        double nretval;
      unsigned int result = 0;
      ioctl( nph, TIOCMGET, &result );
      if ( result & TIOCM_CTS )
         nretval = 1;
      else
         nretval = 0;
    hb_stond(nretval,2);
   }
return ( nretval == 1 )

/// sorry, but ctrldsr and ctrlcts will act like isdsr and iscts... if you want
/// flow control, talk to the system.
function tp_ctrldsr( nPort )
return tp_isdsr( nPort )

function tp_ctrlcts( nPort )
return tp_iscts( nPort )

/// you can't do these things.  try rc.serial
function tp_shared
return 0

function tp_setport
return 0





/// internal functions
static function isopenport( nPort )
   _tpinit()
   if ! isport( nPort )
      return .f.
   endif

   if aPorts[ nPort, TPFP_OC ] != "O"
      return .f.
   endif
return .t.

static function isport( nPort )
   _tpinit()
   if valtype( nPort ) != "N" .or. nPort < 1 .or. nPort > TP_MAXPORTS
      return .f.
   endif
return .t.

static function _readfromporttobuffer( nPort )
   local cChar
   if ! isopenport( nPort )
      return 0
   endif

   while .t.
      cChar := freadstr( aPorts[ nPort, TPFP_HANDLE ], 1 )
      if len( cChar ) == 0
         exit
      endif
      aPorts[ nPort, TPFP_INBUF ] += cChar
   enddo
return 0

static function _tpinit
    local x
   if aPorts == nil
      aPorts := array( TP_MAXPORTS )
      for x := 1 to len( aPorts )
         /// linux /dev port name, file handle, baud, data bits, parity, stop bits, O or C, input buffer
         aPorts[x] := { "", -1, 9600, 8, "N", 1, "C", "" }
      next
   endif
return nil

static function _clock
     LOCAL myvar
     myvar := 4
 //    #Cinline
  //      myvar = time( NULL );
  //   #endCinline
return myvar

/// you can uncomment the following section for compatability with TP code... I figured
/// you'd probably want them commented so it won't compile so that you would see where
/// you have potential incomplete port problems
///function tp_mstat
///return ""
///
///function tp_szmodem
///return 0
///
///function tp_noteoff
///return 0
///
///function tp_ontime
///return 0
///
///function tp_rzmodem
///return 0
///
///function tp_error
///return 0
///
///function tp_errmsg
///return ""
///
///function tp_fifo
///return 0
///
///function tp_flush
///return 0
///
///function tp_outchrs
///return 0
///
///function tp_keybd
///return 0
///
///function tp_outfree
///return 0

/// tp_debug is not a real TP function.  I included it so you can define your own debug
/// output function.
/// the point of the first parameter is a "debug level".  I keep a system variable for how
/// much debuggning output is wanted and if the tp_debug parameter is a LOWER number than
/// the global debug level I print the message.  Since I don't have your system globals,
/// I will ignore the first parameter and always print it.
/// I recommend you modify this function to suit your own debugging needs
function tp_debug( nDebugLevel, cString )
   ? cString
return nil

