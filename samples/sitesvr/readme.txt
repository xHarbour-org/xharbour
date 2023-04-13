/*
 * $Id$
*/

xHarbour web server
-------------------

Just a demonstrative server that allows the creation of an web site
completely stored inside a DBF archive.

Instructions:
-------------

1) make (see WINDOWS USERS notice below)
2) from a directory where the SITE.DBF and related dbt and ntx files
   can be found, launch the program as
         "sitesrv <portnumber>"
   where <portnumber> is any valid TCP/IP port.

3) Open the web browser at the address http://localhost:<portnumber>
4) Navigate a little ;)
5) When you feel you want to change something, just browse the address
      http://localhost:<portnumber>/admin
   There you can add/remove/modify pages.
6) DBU or any DBF browser can be used to modify the archives while the
   server is running (although modification of the DBF files while the
   clients are connectiong might result in an index corruption, and
   this might require to rebuild the indexes).
7) To terminate the server enter the command "quit" at the server prompt.

Windows Users Notice
--------------------

Windows users must remove the "pthread" line from the makefile, if they
are not using MINGW for compilation with C.


Author
------

Giancarlo Niccolai <gian@niccolai.ws>


Contributors
------------

Waiting for you...



