/*
 * $Id:  Exp $
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
make_b32.bat

on GNU system use:
make install

DOCUMENTATION
=============

Look at doc folder for help files.
Not yet finished gdlib.txt is the help file.
For full original help file look at gd/doc/index.html

SAMPLES
=======

For samples look at tests dir.
gdtest.prg is an API test application
gdtestcls.prg is a GDImage/GDChart Class test application
test_out.prg is a sample of a cgi application for windows (I have to complete it).

NOTES
=====

WARNING: if you are using Windows platform, copy bgd.dll in tests before use it.

At this time (24/10/2005 CET) it builds on Windows with last CVS.
Not recently testd on GNU system, but it have to run correctly.

