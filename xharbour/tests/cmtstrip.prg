//
// $Id$
//
// Sample program to extract uncommented part of read buffer
//
// Andi Jahja <xharbour@cbn.net.id>
//

PROCEDURE MAIN

   OUTSTD(stripcomment(MEMOREAD("cmtstrip.txt")))
   RETURN

#PRAGMA BEGINDUMP
#include "hbapi.h"
HB_FUNC( STRIPCOMMENT )
{
   char *szOut = hb_stripOutComments( hb_parcx(1) );
   hb_retcAdopt( szOut );
}
#PRAGMA ENDDUMP
