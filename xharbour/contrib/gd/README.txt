/*
 * $Id$
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.xharbour.org http://www.harbour-project.org
 *
 * Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
 *    README file explaining howto compile GD
 *
 * See doc/license.txt for licensing terms.
 *
 */

GD Library is a porting to xHarbour of Thomas Boutell's famous GD library.

FOLDERS
=======

Folder structure:
  /
  +- doc            : help & license files
  +- env            : bcc32 build scripts
  +- include        : include files
  +- lib            : output library folder
  +- obj            : output object folder
  +- samples        : samples folder
  |  |
  |  +- counter     : a sample counter
  |
  +- source         : source files
  +- tests          : test files
     |
     +- images_in   : sample images
     +- images_out  : output of test images

COMPILING
=========

Actually platforms supported are:
- Win32 / BCC32
- GNU systems / GCC

to build library on Win32 with BCC use:
Download bgd.dll from this link:
   http://www.boutell.com/gd/http/gdwin32.zip
then
   make_b32.bat

on GNU system use:
   make install

   ** requirements: gd, gd-devel, libpng, libpng-devel, libjpeg, libjpeg-devel,
                    freetype, freetype-devel, zlib, zlib-devel

DOCUMENTATION
=============

Look at doc folder for help files.
Not yet finished gdlib.txt is the help file.
For full original help file look at gd/doc/index.html

SAMPLES
=======

For samples look at tests dir.
gdtest.prg      is an API test application
gdtestcls.prg   is a GDImage/GDChart Class test application
test_out.prg    is a sample of a cgi application for windows (I have to complete it).
antialiased.prg shows how apply anti-alias to lines.
animgif.prg     is a sample to create an animated gif.
bartest.prg     is a sample to create barcodes with GD Library.

in sample folder:
counter.prg     is sample applications of a graphic web counter.

to compile:
   in Windows/BCC : bldtest.bat <app_without_prg_ext> (for full static)
                    dll_b32.bat <app_without_prg_ext> (for dll version - needs harbour.dll)
   in Linux       : . bldtest.sh <app_without_prg_ext>

NOTES
=====


WARNING: if you are using Windows platform, copy bgd.dll in tests before use it.

At this time (24/10/2005 CET) it builds on Windows with last CVS.
Not recently tested on GNU system, but it have to run correctly.

