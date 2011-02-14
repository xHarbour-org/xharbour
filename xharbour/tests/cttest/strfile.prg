/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 *   CT3 - Test STRFILE(), FILESTR(), SCREENFILE(), FILESCREEN()
 *
 * Copyright 2004 Pavel Tsarenko <tpe2@mail.ru>
 * www - http://www.xharbour.org
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


procedure main
local cVar

        ? 'STRFILE() test'

//  Add to the end of a file:

        ? STRFILE("ABCDEFGH", "TEST.TXT", .T.)             // Result: 8

//  A file with drive and path designations, result: 10:

        ? STRFILE("0123456789", "C:\TEXT\TEST.TXT", .T.)

//  Data in an existing file is overwritten from position 20 with
//  a designated string:

        ? STRFILE("NANTUCKET", "TEST.TXT", .T., 20)        // Result: 9

//  A 5-character string is written starting at position 10 in an
//  existing file 20-characters long.  Since the final parameter is
//  specified as .T. once, and specified as .F. once, you see different
//  results:

        ? STRFILE(REPLICATE("X", 20), "TEST.TXT")
        ? STRFILE("AAAAA", "TEST.TXT", .T., 10, .F.)  // "XXXXXXXXXXAAAAAXXXXX"
        ? STRFILE("AAAAA", "TEST.TXT", .T., 10, .T.)  // "XXXXXXXXXXAAAAA"

wait

        ? 'FILESTR() test'

//  Read in a file completely:

        ? FILESTR("C:\TEXT\TEST.TXT")            // Displays file text

//  Read in everything to the first Ctrl-Z:

        cVar  := FILESTR("C:\TEXT\TEST.TXT", .T.)

//  The file TEST.TXT contains "ABCDEFGHIJ".  Four characters,
//  beginning from position 3, are to be read:

        ? FILESTR("C:\TEXT\TEST.TXT", 4, 3)      // "CDEF"

//  Read the maximum that fits into the available working memory:

        cVar  := FILESTR("C:\TEXT\TEST.TXT", MEMORY(1) *1024 -100)

wait

        ? 'SCREENFILE() test'

//  An existing file is overwritten:

        SCREENFILE("screen.tst")                        // Result:  4000>
        SCREENFILE("\screen.tst")                       // Result:  4000>
        SCREENFILE("a:\screen.tst")                     // Result:  4000

//  The file is appended:

        SCREENFILE("screen.tst", .T. )                  // Result:  4000

//  Insert a screen from an offset of 4000 ( i.e., after the first
//  screen):

        SCREENFILE("screen.tst", .T., 4000)             // Result:  4000

//  Insert a screen after the third screen:

        SCREENFILE("screen.tst", .T., 12000)            // Result:  4000

//  Overwrite an existing file in the fourth screen and trim off
//  the rest of the file:

        SCREENFILE("screen.tst", .T., 12000, .T.)       // Result:  4000

//  Wildcards not permitted:

        SCREENFILE("screen.*")                          // Result:  0

wait

        ? 'FILESCREEN() test'

//  Read a screen from the beginning of a file:

        FILESCREEN("screen.tst")

//  Read the second screen.  Since the offset is specified as
//  4000, the first screen is skipped:

        FILESCREEN("screen.tst", 4000)

//  Drive and path are permitted:

        FILESCREEN("\screen.tst")
        FILESCREEN("a:screen.tst")

//  Wildcards are not permitted:

        FILESCREEN("screen.*")         // Result:  0

return
