$Id: readme.txt,v 1.1 2007/12/28 00:31:59 modalsist Exp $

How to build libpq.lib from postgresql sources with Pelles C compiler.
How to build libhbpg.lib from xharbour contrib source with Pelles C compiler.

1) Go to the www.postgresql.org and download the sources.

2) Instal the sources in a folder like c:\postgresql-x.x.x

3) Copy the apropriate poccxxx.mak file, in accordance with postgres version
   to the folder "c:\postgresql-x.x.x\src\interfaces\libpq"

4) Go to the source folder above.

5) Within this folder type "pomake /f poccxxx.mak /p > pocc.log" to build
   the libpq.lib.
   obs: you can force rebuild all the libpq.lib by call "pomake /f poccxxx.mak clean"

6) See at "Release" subfolder if the libpq.lib was created and copy them to
   the xharbour\lib folder. 

7) Change to the "xharbour contrib\pgsql" folder and run make_pc to build
   libhbpg.lib.

8) Be sure that libhbpg.lib and libpq.lib are into xharbour\lib before build
   an application to access postgresql server.

9) To build your application to manage PostGreSql server, you should link
   libhbpg.lib and libpq.lib into your compile scrip file.
   f.e. with hbmake you can select "Use external libs" and mark them to use.

10) See the TPostgres.prg source to learn about classes and methods to manage
    the PostGreSql server or follow the samples at "contrib\pgsql\tests"
    folder.

Notes about PostGre 8.3.x release:


1) For postgresql-8.3.x build you will need change the pg_config_os.h after
   it is copied. Run step 5 and after an error generated edit the file locate
   at c:\postgresql-x.x.x\src\include\pg_config_os.h
   Into this file there are 3 macros that are already defined by Pelles include
   files like: SIGABRT, S_ISDIR and S_ISREG. You must add these lines: 

   a) locate the SIGABRT define and copy the code below before them:

   #ifdef __POCC__
   #undef SIGABRT
   #endif

   b) locate the S_ISDIR and S_ISREG defines, replace them by the code below,
      or simply guard them under #ifndef / #endif.

   #ifndef __POCC__
   #define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
   #define S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
   #endif

   Run step 5 again to rebuild libpq.lib.

2) If you don't use hbmake to build your applications you will nedd add the
   schannel.lib from pellesc\lib\win folder to the pelles library list. 

That's all.

-----
End
