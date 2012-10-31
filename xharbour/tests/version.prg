//
// $Id$
//

// Testing the VERSION function
/* Harbour Project source code
   http://www.Harbour-Project.org/
   Donated to the public domain by David G. Holm <dholm@jsd-llc.com>.
*/

#include "simpleio.ch"

function Main()

   local aOpt := {;
      "No Optimization",;
      "Optimized for GUI Applications",;
      "Optimized for Console Applications" }

   ? chr( 34 ) + version() + chr( 34 )
   ? chr( 34 ) + hb_compiler() + chr( 34 )
   ? chr( 34 ) + os() + chr( 34 )
   ? chr( 34 ) + "Multi Threading is " + if(hb_multithread(),"Enabled","Disabled") + chr( 34 )
   ? chr( 34 ) + aOpt[ hb_vmmode() + 1 ] + chr( 34 )
   ? "__XHARBOUR__ =", __XHARBOUR__
   ? "HB_VER_BUILDDATE =", HB_VER_BUILDDATE
   ? "__XHARBOUR__ == 0x0123", __XHARBOUR__ == 0x0123

   ?
   ? "Testing access from C program"
   XVER()
   return nil

#pragma begindump
#include "hbapi.h"
#include "hbver.h"
#include "hbverbld.h"

HB_FUNC( XVER )
{
   int iMajor     = ( __XHARBOUR__ >> 8 );
   int iMinor     = ( __XHARBOUR__ & 0xFF ) >> 4;
   int iRevision  = __XHARBOUR__ & 0xF;

   printf( "\n__XHARBOUR__=%u (as ulong)\n",__XHARBOUR__);
   printf( "__XHARBOUR__==0x0123 -> %s\n",(__XHARBOUR__==0x0123) ? "true" : "false" );
   printf( "HB_VER_BUILDDATE=%u (as ulong)\n", HB_VER_BUILDDATE) ;
   printf( "iMajor=%i (as int)\n",iMajor);
   printf( "iMinor=%i (as int)\n",iMinor);
   printf( "iRevision=%i (as int)\n",iRevision);
}
#pragma enddump
