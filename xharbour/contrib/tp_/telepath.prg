/*
 * $Id: telepath.prg,v 1.5 2004/08/25 08:49:06 mauriliolongo Exp $
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


#include "common.ch"
#include "fileio.ch"
#include "telepath.ch"



STATIC   aPorts               // Array with port info
STATIC   nErrorCode := 0      // Error code from last operation, 0 if no error




function tp_baud( nPort, nNewBaud )

   local nRes

   default nNewBaud to 0

   if ! isport( nPort ) .OR. Empty( aPorts[ nPort, TPFP_NAME ] )
      return TE_NOPORT
   endif

   if ! isopenport( nPort )
      return 0
   endif

   if nNewBaud > 0
      if ( nRes := p_InitPortSpeed( aPorts[ nPort, TPFP_HANDLE ] ,;
                                    nNewBaud,;
                                    aPorts[ nPort, TPFP_DBITS  ] ,;
                                    aPorts[ nPort, TPFP_PARITY ] ,;
                                    aPorts[ nPort, TPFP_SBITS  ] ) ) == 0

         aPorts[ nPort, TPFP_BAUD ] := nNewBaud


      else
         // set error code
      endif
   endif

return aPorts[ nPort, TPFP_BAUD ]



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



function tp_delay( nTime )

   default nTime to 0

   if nTime < 0
      return nil

   elseif nTime > 1800
      nTime := 1800

   endif

   ThreadSleep( nTime * 1000 )

return nil



function tp_close( nPort )

   /* Clipper returns 0 even if a port is not open */
   if ! isopenport( nPort )
      return 0
   endif

   fClose( aPorts[ nPort, TPFP_HANDLE ] )

   /* Port parameters should stay the same for the case the port
      gets reopened
   */
   aPorts[ nPort, TPFP_OC ] := .F.
   aPorts[ nPort, TPFP_INBUF ] := ""

return 0



function tp_reopen( nPort, nInSize, nOutSize )

   LOCAL nBaud, nData, cParity, nStop, cPortName

   default nInSize to 1536, nOutSize to 1536

   if ! isport( nPort ) .OR. Empty( aPorts[ nPort, TPFP_NAME ] )
      return TE_NOPORT
   endif

   cPortname   := aPorts[ nPort, TPFP_NAME ]
   nBaud       := aPorts[ nPort, TPFP_BAUD ]
   nData       := aPorts[ nPort, TPFP_DBITS ]
   cParity     := aPorts[ nPort, TPFP_PARITY ]
   nStop       := aPorts[ nPort, TPFP_SBITS  ]

return tp_open( nPort, nInSize, nOutSize, nBaud, nData, cParity, nStop, cPortName )



/*  linux
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
return 1*/



function tp_open( nPort, nInSize, nOutSize, nBaud, nData, cParity, nStop, cPortname )

   local nRes

   default nInSize to 1536, nOutSize to 1536
   default nBaud to 1200, nData to 8, cParity to "N", nStop to 1

   /* Serial ports name are made up of cPortName + nPort if nPort is not NIL */
   #ifdef __PLATFORM__Linux
   default cPortName to "/dev/ttyS"
   #else
   default cPortName to "COM"          // Ok for Win32 and OS/2
   #endif

   /* This way compatibility is retained for ports 1-4 on Win32 and Linux, but,
      should necessity arise, it is possible to simply pass a NIL on nPort and
      a full name on cPortName
   */
   #ifdef __PLATFORM_Linux
   cPortname := AllTrim( cPortname ) + iif( ValType( nPort ) == "N", AllTrim( Str( nPort - 1 ) ), "" )
   #else
   cPortname := AllTrim( cPortname ) + iif( ValType( nPort ) == "N", AllTrim( Str( nPort ) ), "" )
   #endif

   if ! File( cPortname )
      return TE_NOPORT
   endif

   if ! isport( nPort )
      return TE_NOPORT
   endif

   aPorts[ nPort, TPFP_NAME   ] := cPortname
   aPorts[ nPort, TPFP_BAUD   ] := nBaud
   aPorts[ nPort, TPFP_DBITS  ] := nData
   aPorts[ nPort, TPFP_PARITY ] := cParity
   aPorts[ nPort, TPFP_SBITS  ] := nStop
   aPorts[ nPort, TPFP_OC     ] := .F.
   aPorts[ nPort, TPFP_INBUF  ] := ""
   aPorts[ nPort, TPFP_INBUF_SIZE ] := nInSize

   aPorts[ nPort, TPFP_HANDLE ] := fOpen( cPortname, FO_READWRITE )

   if aPorts[ nPort, TPFP_HANDLE ] >= 0

      /* low level C functions are prefixed p_ (don't ask me why :)) */
      if ( nRes := p_InitPortSpeed( aPorts[ nPort, TPFP_HANDLE ] ,;
                                    aPorts[ nPort, TPFP_BAUD   ] ,;
                                    aPorts[ nPort, TPFP_DBITS  ] ,;
                                    aPorts[ nPort, TPFP_PARITY ] ,;
                                    aPorts[ nPort, TPFP_SBITS  ] ) ) == 0

         aPorts[ nPort, TPFP_OC ] := .T.

         return nRes

      else
         tp_Close( aPorts[ nPort, TPFP_HANDLE ] )
         return nRes

      endif

   endif

   // set error code to a static var to have tp_error() work as expected
   //cnHandle := ferror()

   aPorts[ nPort, TPFP_NAME   ] := ""
   aPorts[ nPort, TPFP_HANDLE ] := -1
   aPorts[ nPort, TPFP_BAUD   ] := 1200
   aPorts[ nPort, TPFP_DBITS  ] := 8
   aPorts[ nPort, TPFP_PARITY ] := "N"
   aPorts[ nPort, TPFP_SBITS  ] := 1
   aPorts[ nPort, TPFP_OC     ] := .F.
   aPorts[ nPort, TPFP_INBUF  ] := ""
   aPorts[ nPort, TPFP_INBUF_SIZE  ] := 0

return TE_CONFL   // maybe should return something different?



function tp_recv( nPort, nLength, nTimeout )

   local nDone
   local cRet := ""

   default nLength to aPorts[ nPort, TPFP_INBUF_SIZE  ]
   default nTimeout to 0

   FetchChars( nPort )

   nDone := Seconds() + iif( nTimeout >= 0, nTimeout, 0)

   while Len( aPorts[ nPort, TPFP_INBUF ] ) < nLength .AND.;
         ( nTimeout < 0 .OR. Seconds() < nDone )

      if FetchChars( nPort ) == 0
         if ! tp_idle()
            HB_IDLESTATE()
         else
            exit
         endif

      endif

   enddo

   if nLength > Len( aPorts[ nPort, TPFP_INBUF ] )
      cRet := aPorts[ nPort, TPFP_INBUF ]
      aPorts[ nPort, TPFP_INBUF ] := ""
   else
      cRet := SubStr( aPorts[ nPort, TPFP_INBUF ], 1, nLength )
      aPorts[ nPort, TPFP_INBUF ] := SubStr( aPorts[ nPort, TPFP_INBUF ], nLength + 1 )
   endif

return cRet



function tp_send( nPort, cString, nTimeout )

   local nWritten, nTotWritten, nDone
   local cLocString                 // a copy of cString to be able to change it

   default cString to "", nTimeout to 0

   if ! isopenport( nPort )
      return 0
   endif

   if Empty( cString )
      return 0
   else
      cLocString := cString
   endif

   nDone := Seconds() + iif( nTimeout >= 0, nTimeout+1, 0)
   nWritten := nTotWritten := 0

   while Len( cLocString ) > 0 .AND. nTotWritten < Len( cString ) .AND. ;
         ( nTimeout < 0 .OR. Seconds() < nDone )

      nWritten := p_WritePort( aPorts[ nPort, TPFP_HANDLE ], cLocString )

      if nWritten < Len( cString ) .AND. nWritten >= 0

         nTotWritten += nWritten

         if ! tp_idle()
            HB_IDLESTATE()
         else
            exit
         endif

         // Send remaining part of string
         cLocString := SubStr( cLocString, nWritten + 1)

      else     // nWritten < 0, error occurred
         exit
      endif

   enddo

return nTotWritten



function tp_sendsub( nPort, cString, nStart, nLength, nTimeout )

   default nStart to 1, nLength to Len( cString )

return tp_send( nPort, SubStr( cString, nStart, nLength ), nTimeout )



/* TODO: does not return if cDelim is not found but input buffer > than nMaxLen */
function tp_recvto( nPort, cDelim, nMaxlen, nTimeout )
   local x
   local cChar
   local nAt
   local nStartPos := 1, nFirst := 0
   local nDone

   /* wrong type of ret.code
   if ! isopenport( nPort )
      return 1
   endif
   */

   if ValType( cDelim ) <> "C" .OR. Empty( cDelim )
      return ""
   endif

   default nMaxlen to 64999      /* dos telepathy def. on xharbour could be higher */
   default nTimeout to 0


   FetchChars( nPort )

   /* Telepathy ng: [...] If nTimeout is omitted or zero, reads until finding the
                    delimiter or the input buffer is empty. */
   if nTimeout == 0 .AND. Empty( aPorts[ nPort, TPFP_INBUF ] )
      return ""
   endif

   nDone := Seconds() + iif( nTimeout >= 0, nTimeout, 0)

   while ( nTimeout < 0 .OR. Seconds() < nDone )

      FOR EACH cChar IN cDelim

         nAt := At( cChar, aPorts[ nPort, TPFP_INBUF ], nStartPos )

         if nAt > 0 .AND. iif(nFirst > 0, nAt < nFirst, .T.)
            nFirst := nAt
         endif

      NEXT

      // I've found it
      if nFirst > 0 .AND. nFirst < nMaxLen
         exit
      else
         // Next loop I don't need to search that part of the input buffer that
         // I've already just searched for
         nStartPos := Max( Len( aPorts[ nPort, TPFP_INBUF ] ), 1 )

         if ! tp_idle()
            if FetchChars( nPort ) == 0
               HB_IDLESTATE()
            endif
         else
            exit
         endif

      endif

   enddo

   if nFirst > 0 .AND. nFirst < nMaxLen
      return tp_recv( nPort, nFirst )
   endif

return ""



/*
    here's an improvement over original TP... you can "lookfor" a string
    here rather than just a char.  yay me.
    of course, if you're using clipper/tp code and you search for a single char it will work
    the same.
*/
function tp_lookfor( nPort, cLookfor )

   if ! isopenport( nPort )
      return 0
   endif

   FetchChars( nPort )

return At( cLookfor, aPorts[ nPort, TPFP_INBUF ] )



function tp_inchrs( nPort )

   if ! isopenport( nPort )
      return 0
   endif

   FetchChars( nPort )

return Len( aPorts[ nPort, TPFP_INBUF ] )



function tp_clearin( nPort )

   if isopenport( nPort )
      aPorts[ nPort, TPFP_INBUF ] := ""
   endif

return nil



function tp_clrkbd()

   clear typeahead

return nil



function tp_crc16( cString )

return p_CRC16(cString)


/*
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

      FetchChars( nPort )

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



*/

// internal (static) functions ---------------------------------------------------

static function isopenport( nPort )
   _tpinit()
   if ! isport( nPort )
      return .f.
   endif

   if ! aPorts[ nPort, TPFP_OC ]
      return .f.
   endif
return .t.



static function isport( nPort )
   _tpinit()
   if valtype( nPort ) != "N" .or. nPort < 1 .or. nPort > TP_MAXPORTS
      return .f.
   endif
return .t.



static function FetchChars( nPort )

   local cStr := ""

   if ! isopenport( nPort )
      return 0
   endif

   cStr := p_ReadPort( aPorts[ nPort, TPFP_HANDLE ] )

   if ! Empty( cStr )
      aPorts[ nPort, TPFP_INBUF ] += cStr
   endif

return Len( cStr )



static function _tpinit
   local x

   if aPorts == nil
      aPorts := array( TP_MAXPORTS )
      for x := 1 to len( aPorts )
         /// port name, file handle, baud, data bits, parity, stop bits, Open?, input buffer, input buff.size
         aPorts[ x ] := { "", -1, 1200, 8, "N", 1, .F., "", 0 }
      next
   endif

return nil




/*
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
*/



/* ----- platform neutral C code -------------------------------------- */

#pragma begindump

#define _CLIPDEFS_H     // Don't ridefine basic types
#include "hbapifs.h"
#include "extend.api"

/* crctab calculated by Mark G. Mendel, Network Systems Corporation */
static unsigned short crctab[ 256 ] = {
    0x0000,  0x1021,  0x2042,  0x3063,  0x4084,  0x50a5,  0x60c6,  0x70e7,
    0x8108,  0x9129,  0xa14a,  0xb16b,  0xc18c,  0xd1ad,  0xe1ce,  0xf1ef,
    0x1231,  0x0210,  0x3273,  0x2252,  0x52b5,  0x4294,  0x72f7,  0x62d6,
    0x9339,  0x8318,  0xb37b,  0xa35a,  0xd3bd,  0xc39c,  0xf3ff,  0xe3de,
    0x2462,  0x3443,  0x0420,  0x1401,  0x64e6,  0x74c7,  0x44a4,  0x5485,
    0xa56a,  0xb54b,  0x8528,  0x9509,  0xe5ee,  0xf5cf,  0xc5ac,  0xd58d,
    0x3653,  0x2672,  0x1611,  0x0630,  0x76d7,  0x66f6,  0x5695,  0x46b4,
    0xb75b,  0xa77a,  0x9719,  0x8738,  0xf7df,  0xe7fe,  0xd79d,  0xc7bc,
    0x48c4,  0x58e5,  0x6886,  0x78a7,  0x0840,  0x1861,  0x2802,  0x3823,
    0xc9cc,  0xd9ed,  0xe98e,  0xf9af,  0x8948,  0x9969,  0xa90a,  0xb92b,
    0x5af5,  0x4ad4,  0x7ab7,  0x6a96,  0x1a71,  0x0a50,  0x3a33,  0x2a12,
    0xdbfd,  0xcbdc,  0xfbbf,  0xeb9e,  0x9b79,  0x8b58,  0xbb3b,  0xab1a,
    0x6ca6,  0x7c87,  0x4ce4,  0x5cc5,  0x2c22,  0x3c03,  0x0c60,  0x1c41,
    0xedae,  0xfd8f,  0xcdec,  0xddcd,  0xad2a,  0xbd0b,  0x8d68,  0x9d49,
    0x7e97,  0x6eb6,  0x5ed5,  0x4ef4,  0x3e13,  0x2e32,  0x1e51,  0x0e70,
    0xff9f,  0xefbe,  0xdfdd,  0xcffc,  0xbf1b,  0xaf3a,  0x9f59,  0x8f78,
    0x9188,  0x81a9,  0xb1ca,  0xa1eb,  0xd10c,  0xc12d,  0xf14e,  0xe16f,
    0x1080,  0x00a1,  0x30c2,  0x20e3,  0x5004,  0x4025,  0x7046,  0x6067,
    0x83b9,  0x9398,  0xa3fb,  0xb3da,  0xc33d,  0xd31c,  0xe37f,  0xf35e,
    0x02b1,  0x1290,  0x22f3,  0x32d2,  0x4235,  0x5214,  0x6277,  0x7256,
    0xb5ea,  0xa5cb,  0x95a8,  0x8589,  0xf56e,  0xe54f,  0xd52c,  0xc50d,
    0x34e2,  0x24c3,  0x14a0,  0x0481,  0x7466,  0x6447,  0x5424,  0x4405,
    0xa7db,  0xb7fa,  0x8799,  0x97b8,  0xe75f,  0xf77e,  0xc71d,  0xd73c,
    0x26d3,  0x36f2,  0x0691,  0x16b0,  0x6657,  0x7676,  0x4615,  0x5634,
    0xd94c,  0xc96d,  0xf90e,  0xe92f,  0x99c8,  0x89e9,  0xb98a,  0xa9ab,
    0x5844,  0x4865,  0x7806,  0x6827,  0x18c0,  0x08e1,  0x3882,  0x28a3,
    0xcb7d,  0xdb5c,  0xeb3f,  0xfb1e,  0x8bf9,  0x9bd8,  0xabbb,  0xbb9a,
    0x4a75,  0x5a54,  0x6a37,  0x7a16,  0x0af1,  0x1ad0,  0x2ab3,  0x3a92,
    0xfd2e,  0xed0f,  0xdd6c,  0xcd4d,  0xbdaa,  0xad8b,  0x9de8,  0x8dc9,
    0x7c26,  0x6c07,  0x5c64,  0x4c45,  0x3ca2,  0x2c83,  0x1ce0,  0x0cc1,
    0xef1f,  0xff3e,  0xcf5d,  0xdf7c,  0xaf9b,  0xbfba,  0x8fd9,  0x9ff8,
    0x6e17,  0x7e36,  0x4e55,  0x5e74,  0x2e93,  0x3eb2,  0x0ed1,  0x1ef0
};


/* updcrc() macro by Pete Disdale */
#define updcrc(cp, crc)  ( ( crc << 8 ) ^ ( crctab[ ( ( crc >> 8 ) ^ cp ) ] ) )


HB_FUNC( P_CRC16 ) {

   char *ptr = _parc( 1 );
   int count = _parclen( 1 );

   register unsigned short crc = 0;

   while ( count-- > 0 ) {
      crc = updcrc( *ptr++, crc );
   }

   /* swap Hi and Lo byte */
   _retnl( ( crc >> 8 ) | ( ( crc << 8 ) & 0xFF00 ) );
}

#pragma enddump

