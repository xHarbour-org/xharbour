# xharbour
xHarbour is a portable implementation of the Clipper/xBase language 
(Compiler &amp; complete Run-time libraries). It's practically 100% backward 
compatible with CA-Clipper 5.2e and 5.3c, and offers many modern language extensions,
and extensive Run-time libraries.

<b>Welcome to xHarbour</b>

xHarbour is a fork from the [Harbour project](https://harbour.github.io/) -
a free software compiler for the xBase superset language often
referred to as Clipper (the language that is implemented by the compiler
CA-Clipper). The goal of the xHarbour project is to produce a cross platform
CA-Clipper compatible compiler, extended to support popular modern extensions..

The xHarbour web site is at [xHarbour.org](http://www.xharbour.org). If you
have any problems with this copy of xHarbour please visit our web site and
ensure that you are using the latest release.

If you have any questions about xHarbour please be sure to read the info on
[xHarbour.org](http://www.xharbour.org), and for live interaction you may use 

[comp.lang.xharbour newsgroup](news://comp.lang.xharbour)

Also, please be sure to read the
documentation that comes with xHarbour, you should find it in the ./doc/
directory. 

If you are reading this file as part of a source distribution of harbour you
probably want to start by reading dirstruc.txt because this is your map to
the harbour source directories.

How to get the latest xHarbour
------------------------------
1. git clone --recurse-submodules https://github.com/xHarbour-org/xharbour.git
2. cd xharbour
   
Requisites to build from sources
--------------------------------
Valid installation of a C/C++ compiler.

  Windows:
  --------
  Following are some FREE choices:
  
    - https://www.embarcadero.com/free-tools/ccompiler/free-download
    - http://www.smorgasbordet.com/pellesc/
    - https://visualstudio.microsoft.com/downloads/
    
  Linux:
  ------
  GCC or CLang

  MacOS:
  ------
  Apple's CLang
  
How to build from sources
-------------------------

  Windows:
  --------
  Depending on your installed C Compiler you will use one of the following batch
  files:
  
    - make_bc.bat (for Embarcadero / Borland C++)
    - make_pc.bat (for Pelles C)
    - make_vc.bat (for Visual Sttudio)

    There additional make_??.bat for more compilers but tthey may require some addittional settings.

  Linux:
  ------
  ./buildxbuilder.sh

  MacOS:
  ------
  ./buildxbuilder.sh
  
