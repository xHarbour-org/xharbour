# xharbour
xHarbour is a portable implementation of the Clipper/xBase language 
(Compiler &amp; complete Run-time libraries). It's practically 100% backward 
compatible with CA-Clipper 5.2e and 5.3c, and offers many modern language extensions,
and extensive Run-time libraries.

<center>Welcome to xHarbour</center>

xHarbour is a fork from the Harbour project <URL:http://harbour-project.org> -
a free software compiler for the xBase superset language often
referred to as Clipper (the language that is implemented by the compiler
CA-Clipper). The goal of the xHarbour project is to produce a cross platform
CA-Clipper compatible compiler, extended to support popular modern extensions..

The xHarbour web site is at <URL:http://www.xharbour.org>. If you
have any problems with this copy of Harbour please visit our web site and
ensure that you are using the latest release.

If you have any questions about xHarbour please be sure to read the FAQ
<URL:http://www.xharbour.org>, and for live interaction you may use 

news://comp.lang.xharbour <URL:https://groups.google.com/g/comp.lang.xharbour>

Also, please be sure to read the
documentation that comes with xHarbour, you should find it in the ./doc/
directory. 

If you are reading this file as part of a source distribution of harbour you
probably want to start by reading dirstruc.txt because this is your map to
the harbour source directories.

A note about building xHarbour from the source code: after the first

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;git clone

you need to perform

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;git submodule update --init --recursive

to obttain the xbScript sub module or building will fail.
