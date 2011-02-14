//
// $Id$
//

// Testing the VERSION function
/* Harbour Project source code
   http://www.Harbour-Project.org/
   Donated to the public domain by David G. Holm <dholm@jsd-llc.com>.
*/

function Main()

   local aOpt := {;
      "No Optimization",;
      "Optimized for GUI Applications",;
      "Optimized for Console Applications" }

   outstd( chr( 34 ) + version() + chr( 34 ) + hb_osnewline() )
   outstd( chr( 34 ) + hb_compiler() + chr( 34 ) + hb_osnewline() )
   outstd( chr( 34 ) + os() + chr( 34 ) + hb_osnewline() )
   outstd( chr( 34 ) + "Multi Threading is " + if(hb_multithread(),"Enabled","Disabled") + chr( 34 ) + hb_osnewline() )
   outstd( chr( 34 ) + aOpt[ hb_vmmode() + 1 ] + chr( 34 ) + hb_osnewline() )

   return nil
