/*
 * $Id: net.c,v 1.3 2004/01/04 23:43:31 lculik Exp $
 *
 * xHarbour Project source code:
 * CT3 NET functions to PC-LAN/MS-NET.
 *
 * Copyright 2004 Eduardo Fernandes <eduardo@modalsistemas.com.br>
 * www - http://www.xharbour.org
 *
 *******
 *
 * CT3 NET Functions Comments:
 *
 * NETCANCEL( <cLocalDevice> ) -> lReleased
 * Return true if <cLocalDevice> was disconnected.
 *
 * NETDISK( cDrive ) -> lSuccess
 * Return true if <cDrive> is a network drive, otherwise return false if is a local drive.
 *
 * NETLOCNAME( cSahredDevice ) -> cLocalDevice
 * Not implemented yet.
 *
 * NETPRINTER() -> lSuccess
 * Return true if a current local printer seted by SET PRINTER TO was connected to a
 * network printer.
 *
 * NETREDIR( cLocalDevice, cSharedDevice [, cPassword ] ) -> lSuccess
 * Return true if <cLocalDevice> was connected to <cSharedDevice> with <cPassword>, if any.
 *
 * NETRMTNAME( cLocalDevice ) -> cSharedName
 * Return the shared resource name connected to a <cLocalDevice>.
 * The original parameter <nDevice> in CA-Clipper Tools was changed to <cLocalName> in
 * xHarbour because in Windows Network I didn´t find a number table like in MS-DOS. See
 * CA-Tools help for more details.
 *
 * NETWORK() -> lSuccess
 * Return true if a PC-LAN/MS-NET or Netware type is active.
 *
 * NNETWORK() -> lSuccess
 * Return true if a Netware type is active.
 *
 ******
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
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbset.h"

#if defined(HB_OS_WIN_32)

#include <windows.h>
#include <winnetwk.h>

#define HB_OS_WIN_32_USED


static BOOL hb_IsNetShared(LPSTR szLocalDevice )
{
   char szRemoteDevice[80] ;
   DWORD dwResult ;
   DWORD cchBuff = sizeof(szRemoteDevice) ;

   dwResult = WNetGetConnection( (LPSTR) szLocalDevice , (LPSTR) szRemoteDevice , &cchBuff) ;

   if ( dwResult == NO_ERROR )
   {
      return TRUE;
   }
   else
   {
      return FALSE;
   }
}

/*  $DOC$
 *  $FUNCNAME$
 *      NETCANCEL()
 *  $CATEGORY$
 *      CT3 net functions
 *  $ONELINER$
 *      Releases the redirection of a local device
 *  $SYNTAX$
 *      NETCANCEL(<cLocalDevice>) --> lReleased
 *  $ARGUMENTS$
 *      <cLocalDevice>  Designates the name for the local device, for which
 *      an existing redirection in the network is released.
 *  $RETURNS$
 *      NETCANCEL() returns .T. when a redirection existed for the designated
 *      device and it was released.
 *  $DESCRIPTION$
 *      NETWORK CANCEL REDIRECTION
 *      With NETCANCEL(), an existing redirection for a local device can be
 *      released.  You call the function by simply specifying the device name,
 *      d: or LPTn:.
 *
 *      If the return value is .F., the device did not exist or was not
 *      previously redirected.
 *  $EXAMPLES$
 *      Make LPT1 redirected printer devices local
 *
 *      NETCANCEL("LPT1:")
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This function is xHarbour libct contrib
 *  $PLATFORMS$
 *      WINDOWS
 *  $FILES$
 *      Source is net.c, library is libct.
 *  $SEEALSO$
 *      NETREDIR(),NETLOCNAME()
 *  $END$
 */

HB_FUNC ( NETCANCEL )
{
   DWORD dwResult;
   char *cDevice = (char *)hb_parc (1);

   dwResult = WNetCancelConnection( cDevice , TRUE ) ; // FALSE = fail if exist open files or print jobs.
                                                                                                       // TRUE = force cancel connection even if exist														//        open files or print jobs.
   hb_retl( dwResult == NO_ERROR ? TRUE : FALSE );
}

/*  $DOC$
 *  $FUNCNAME$
 *      NETPRINTER()
 *  $CATEGORY$
 *      CT3 net functions
 *  $ONELINER$
 *      Determines whether the current printer is a local or network printer.
 *  $SYNTAX$
 *      NETPRINTER() --> lServerPrinter
 *  $ARGUMENTS$
 *      None
 *  $RETURNS$
 *      NETPRINTER() returns .T. when the printer installed using the xHarbour
 *      SET PRINTER TO <LPTn> command has been redirected onto a network server.
 *  $DESCRIPTION$
 *      NETWORK PRINTER
 *      This function allows you to determine if the printer installed using the
 *      xHarbour SET PRINTER TO <LPTn> command is local or resides on a
 *      network server.
 *  $EXAMPLES$
 *      Determine whether LPT1 and LPT2 are local or remote:
 *
 *      SET PRINTER TO LPT1
 *      ? NETPRINTER()                        // .F., local printer
 *      SET PRINTER TO LPT2
 *      NETREDIR("LPT2:","\\COMPUTER\PRINTER")
 *      ? NETPRINTER()                        // .T., network printer
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This function is xHarbour libct contrib
 *  $PLATFORMS$
 *      WINDOWS
 *  $FILES$
 *      Source is net.c, library is libct.
 *  $SEEALSO$
 *      NETREDIR(),NETDISK()
 *  $END$
 */

HB_FUNC ( NETPRINTER )
{
   char *cPrn = hb_set.HB_SET_PRINTFILE ;  // query default local printer port.

   hb_retl( hb_IsNetShared( cPrn ) );
}

/*  $DOC$
 *  $FUNCNAME$
 *      NETDISK()
 *  $CATEGORY$
 *      CT3 net functions
 *  $ONELINER$
 *      Determines whether a drive is local or resident on the server
 *  $SYNTAX$
 *      NETDISK(<cDrive>) --> lServerDrive
 *  $ARGUMENTS$
 *      <cDrive>  Designates the drive that is tested in a range from A to Z.
 *  $RETURNS$
 *      NETDISK() returns .T. when the specified drive is a network drive.
 *  $DESCRIPTION$
 *      NETWORK DISK
 *      This function allows you to determine if a drive in the range of A: to
 *      Z: is local or remote.  Remote means that a drive is actually resident
 *      on a file server and that the local device address has been redirected
 *      through DOS.
 *  $EXAMPLES$
 *      Test all drives to determine if they are local or resident on the
 *      server:
 *
 *      FOR I = ASC("A") TO ASC("Z")
 *          ? NETDISK(CHR(I))
 *      NEXT I
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This function is xHarbour libct contrib
 *  $PLATFORMS$
 *      WINDOWS
 *  $FILES$
 *      Source is net.c, library is libct.
 *  $SEEALSO$
 *      NETREDIR(),NETPRINTER()
 *  $END$
 */

HB_FUNC ( NETDISK )
{
   char *cDrive = ( char *) hb_parcx( 1 );
   if ( strstr( cDrive, ":" ) == NULL )
   {
      strcat( cDrive, ":" ) ;
   }

   hb_retl( hb_IsNetShared( cDrive ) );
}

/*  $DOC$
 *  $FUNCNAME$
 *      NETREDIR()
 *  $CATEGORY$
 *      CT3 net functions
 *  $ONELINER$
 *      Redirects a local device to a server device
 *  $SYNTAX$
 *      NETREDIR(<cLocalDevice>, <cServerDevice>, [<cPassword>]) --> lRedirection
 *  $ARGUMENTS$
 *      <cLocalDevice>  Designates the name of the local device (LPTn:,
 *      PRN:, or d:).
 *
 *      <cServerDevice>  Designates the complete path for the server
 *      (\\<Servername>\<Devicename>).
 *
 *      <cPassword>  Can designate a password when it is required by the
 *      server.
 *  $RETURNS$
 *      NETREDIR() returns .T. when the specified local device has been
 *      redirected to the selected server device.
 *  $DESCRIPTION$
 *      NETWORK REDIRECTION
 *      Under PC LAN/MS-NET, server devices can be redirected to local devices
 *      using the NET USE <local> <remote> command.  Exactly the same thing can
 *      be accomplished using the NETREDIR() function from within an
 *      application. The function returns .T. when the redirection has been
 *      carried out. A return of .F. could occur for several reasons:
 *
 *      The device is not available on the server.  Use NET USE or NET SHARE
 *      on the selected server to review all the devices available there.
 *
 *      The server device is unavailable.
 *
 *      The server device name is wrong.
 *
 *      The password is incorrect or does not exist.
 *
 *      Too many devices have already been allocated.  In this instance you must
 *      change some of the flags with the NET START command. See PC LAN/MS-NET manual.
 *  $EXAMPLES$
 *      Create a new drive H:
 *
 *      ? NETREDIR("H:", "\\SERVER\DRIVEC")            // .T. if OK
 *
 *      In this example, the server device asks for a password:
 *
 *      ? NETREDIR("H:", "\\SERVER\DRIVEC", "SECRET")  // .T. if OK
 *
 *      Allocate a server printer to LPT2:
 *
 *      ? NETREDIR("LPT2:", "\\SERVER\PRINTER")        // .T. if OK
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This function is xHarbour libct contrib
 *  $PLATFORMS$
 *      WINDOWS
 *  $FILES$
 *      Source is net.c, library is libct.
 *  $SEEALSO$
 *      NETCANCEL(),NETLOCNAME(),NETRMTNAME()
 *  $END$
 */

HB_FUNC ( NETREDIR )
{

   DWORD dwResult;
   char *cLocalDev  = hb_parcx( 1 );
   char *cSharedRes = hb_parcx( 2 );
   char *cPassword  = hb_parcx( 3 );

   if ( strstr( cLocalDev, ":" ) == NULL )
   {
   strcat( cLocalDev, ":" );
   }

   if ( hb_pcount() == 3 && ISCHAR( 3 ) )
   {
      dwResult = WNetAddConnection( cSharedRes , cPassword , cLocalDev ) ;
   }
   else
   {
      dwResult = WNetAddConnection( cSharedRes , NULL , cLocalDev ) ;
   }

   hb_retl( dwResult == NO_ERROR ? TRUE : FALSE );

}

/*  $DOC$
 *  $FUNCNAME$
 *      NETRMTNAME()
 *  $CATEGORY$
 *      CT3 net functions
 *  $ONELINER$
 *      Determines the name of a server device for a local device
 *  $SYNTAX$
 *      NETRMTNAME(<cLocalDevice>) --> cServerDevice
 *  $ARGUMENTS$
 *      <cLocalDevice>  Designates the local devie name.
 *  $RETURNS$
 *      NETRMTNAME() returns the local device name for the designated location in the
 *      DOS redirection table (<cLocalDevice>), or a null string if no name is available.
 *  $DESCRIPTION$
 *      NETWORK REMOTE NAME
 *      Under PC LAN/MS-NET server devices can be redirected to local devices
 *      using the NET USE <local> <remote> command. In this way, NETRMTNAME()
 *      allows you to determine the server device that have been redirected to
 *      a local device.
 *
 *      Note
 *
 *      Using NETLOCNAME() allows you to determine the name of the
 *      corresponding local device.
 *  $EXAMPLES$
 *      Determine the server device used by the local device:
 *
 *      ?  NETRMTNAME( "F:" ) // Display server device
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This function is xHarbour libct contrib
 *  $PLATFORMS$
 *      WINDOWS
 *  $FILES$
 *      Source is net.c, library is libct.
 *  $SEEALSO$
 *      NETLOCNAME()
 *  $END$
 */

HB_FUNC ( NETRMTNAME )
{
   char szRemoteDevice[ 80 ];
   char *szLocalDevice = ( char * )hb_parc ( 1 ) ;
   DWORD dwResult;
   DWORD cchBuff = sizeof( szRemoteDevice );

   dwResult = WNetGetConnection( (LPSTR) szLocalDevice , (LPSTR) szRemoteDevice , &cchBuff);

   hb_retc( dwResult == NO_ERROR ? szRemoteDevice : "" ) ;
}

/*  $DOC$
 *  $FUNCNAME$
 *      NETWORK()
 *  $CATEGORY$
 *      CT3 net functions
 *  $ONELINER$
 *      Tests to see if a network is active
 *  $SYNTAX$
 *      NETWORK() --> lActiveNet
 *  $ARGUMENTS$
 *      None
 *  $RETURNS$
 *      NETWORK() returns .T. when a PC LAN/MS-NET or a Novell network is
 *      available.
 *  $DESCRIPTION$
 *      With NETWORK(), you can determine whether or not an IBM PC LAN/MS-NET or
 *      Novell network is available and active.  Using NNETWORK() allows you to
 *      further differentiate to see if you are dealing with a Novell network.
 *  $EXAMPLES$
 *      Test to see if one of the two networks is available:
 *
 *      IF NETWORK()
 *         IF !NNETWORK()
 *            // This is a PC LAN/MS-NET network.
 *         ELSE
 *            // This is a Novell network.
 *         ENDIF
 *      ENDIF
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This function is xHarbour libct contrib
 *  $PLATFORMS$
 *      WINDOWS
 *  $FILES$
 *      Source is net.c, library is libct.
 *  $SEEALSO$
 *      NNETWORK()
 *  $END$
 */

HB_FUNC ( NETWORK )
{
   DWORD dwResult;
   char szProviderName[80];
   DWORD cchBuff = sizeof(szProviderName);

   dwResult = WNetGetProviderName( WNNC_NET_MSNET , (LPSTR) szProviderName , &cchBuff);

   if ( dwResult != NO_ERROR )
   {
      dwResult = WNetGetProviderName( WNNC_NET_LANMAN , (LPSTR) szProviderName , &cchBuff);

      if ( dwResult != NO_ERROR )
      {
         dwResult = WNetGetProviderName( WNNC_NET_NETWARE , (LPSTR) szProviderName , &cchBuff);
      }
   }

   hb_retl( dwResult == NO_ERROR ? TRUE : FALSE );
}
/*  $DOC$
 *  $FUNCNAME$
 *      NNETWORK()
 *  $CATEGORY$
 *      CT3 net functions
 *  $ONELINER$
 *      Determines whether or not a Novell network is active
 *  $SYNTAX$
 *      NNETWORK() --> lNovellNetActive
 *
 *      Netware: 2.2 and 3.11
 *  $ARGUMENTS$
 *      None
 *  $RETURNS$
 *      NNETWORK() returns .T. when you are working in a Novell network.
 *  $DESCRIPTION$
 *      NOVELL NETWORK
 *      With NNETWORK(), you can determine if you are working with a Novell
 *      network.
 *
 *      Note
 *
 *      This function only tests to see if a Novell shell (requestor)
 *      has been loaded.
 *  $EXAMPLES$
 *      Determine if there a Novell network, version => 2.0/ELS I that is
 *      active:
 *
 *      IF NNETWORK()
 *         ...
 *      ENDIF
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This function is xHarbour libct contrib
 *  $PLATFORMS$
 *      WINDOWS
 *  $FILES$
 *      Source is net.c, library is libct.
 *  $SEEALSO$
 *      NETWORK()
 *  $END$
 */


HB_FUNC ( NNETWORK )
{
   DWORD dwResult;
   char szProviderName[80];
   DWORD cchBuff = sizeof(szProviderName);

   dwResult = WNetGetProviderName( WNNC_NET_NETWARE , (LPSTR) szProviderName , &cchBuff);

   hb_retl( dwResult == NO_ERROR ? TRUE : FALSE );
}

#endif

