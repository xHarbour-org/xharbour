/*
 * $Id$
 */

/* test program for copyfile with codeblock
 * very useful for meter pourposes when copy large files
 *
 * Toninho
 */


procedure main()

   local nBytes := 0

   __copyfile( "testfcpy.exe", "c:\testfcpy.exe", {|n| ( nBytes += n, QOut("Bytes copied:",nBytes) ) } )

return

