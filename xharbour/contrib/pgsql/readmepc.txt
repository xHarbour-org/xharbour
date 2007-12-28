$Id: readmepc.txt,v 0.0 0000/00/00 00:00:00 modalsist Exp $

xHarbour Low Level API for Postgres RDBMS

IMPORTANT
---------
Read the README first.


Pelles C
---------
To use this library (libhbpg.lib) you will need build libpq.lib from
PostGreSql sources under Pelles C. 

To do it:

1) Go to the PostGreSql website and download the sources.
2) Instal the PostGreSql sources in a folder as C:\PostGreSql-x.x.x
3) Copy the pocc.mak file (give here) to the folder:
   C:\PostgreSql-x.x.x\src\interfaces\libpq
4) Go to the source folder above, f.e. cd c:\PostGreSql-x.x.x\scr\interfaces\libpq.
5) From this folder type: pomake /f pocc.mak to build the libpq.lib.
6) See at Release subfolder if the libpq.lib was created and so copy them to 
   the xharbour\lib folder.
7) Be sure that libhbpg.lib is in xharbour\lib togheter with libpq.
8) To build your application to manage PostGreSql server, you should link
   libhbpg.lib and libpq.lib into your compile scrip file.
   f.e. with hbmake you can select "Use external libs" and mark them to use.
9) See the TPostgres.prg source to learn about classes and methods to manage
   the PostGreSql server or follow the samples in contrib\tests folder.

-----
End
