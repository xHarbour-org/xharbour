/* $Id$
   Test message directive
   Maybe useful for giving notice on compile time
   See below for example
*/

#message ""
#message "You can put any text after message directives"
#message "For example, we may tell user about what needed to create"
#message "application with this file, like libs required etc like:"
#message ""
#message "This program require FOOBAR.LIB"
#message "This program require BARFOO.LIB"
#message ""

PROCEDURE MAIN()

#if defined( __FOOBAR__)
   #message "__FOOBAR__ is defined"
#else
   #message "__FOOBAR__ is not defined"
#endif

   RETURN
