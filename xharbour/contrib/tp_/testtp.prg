/*
 * $Id$
 */

/// instructions - hook up a loopback device to "com1" of your computer
/// run this program.  It should echo Hi there bob.
/// If it gives you a file i/o error, try running as root.
/// also make sure that other programs (minicom?) can successfully talk to your
/// com port.

function main
   ?
   ?
   tp_open( "/dev/ttyS1", 1 )
   tp_send( 1, "Hi there bob" )
   tp_inkey( .5 )
   ? tp_recv( 1 )
   ?
return nil
