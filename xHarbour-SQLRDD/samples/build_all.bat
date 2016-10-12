IF EXIST ..\..\bin\xbuild.exe GOTO MAKE_XBUILD

GOTO MAKE_BCC_PLAIN

:MAKE_XBUILD

..\..\bin\xbuild chgStruct.prg    -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild codepage.prg     -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild constraint.prg   -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild createdsn.prg    -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild dbf2sql.prg      -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild demo01.prg       -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild filter.prg       -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild historic.prg     -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild info.prg         -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild memo.prg         -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild mlang.prg        -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild namespacesql.prg -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild oraclebi.prg     -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild parser1.prg      -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild query.prg        -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild relation.prg     -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild runscript.prg    -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild topconn.prg      -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj
..\..\bin\xbuild tstrdd.prg       -X..\..\ -i..\..\include\SQLRDD -L..\..\lib\sqlrdd odbccp32.lib -oobj

GOTO END

:MAKE_BCC_PLAIN

set HB_INSTALL=..\..

set INCLUDE_PATH=%HB_INSTALL%\include;%HB_INSTALL%\include\SQLRDD
set LIB_PATH=%HB_INSTALL%\lib;%HB_INSTALL%\lib\SQLRDD
set HB_LIBLIST= common.lib debug.lib vm.lib rtl.lib pcrepos.lib gtwin.lib lang.lib rdd.lib macro.lib pp.lib dbffpt.lib dbfntx.lib dbfcdx.lib hsx.lib hbsix.lib ct.lib tip.lib sql.lib oci.lib fbclient_bc.lib libpq.lib odbccp32.lib libmysql.lib odbc32.lib

%HB_INSTALL%\bin\harbour chgStruct.prg    -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour codepage.prg     -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour constraint.prg   -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour createdsn.prg    -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour dbf2sql.prg      -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour demo01.prg       -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour filter.prg       -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour historic.prg     -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour info.prg         -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour memo.prg         -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour mlang.prg        -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour namespacesql.prg -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour oraclebi.prg     -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour parser1.prg      -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour query.prg        -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour relation.prg     -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour runscript.prg    -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour topconn.prg      -n -q0 -gc -i%INCLUDE_PATH% -p
%HB_INSTALL%\bin\harbour tstrdd.prg       -n -q0 -gc -i%INCLUDE_PATH% -p

bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% chgStruct.c    %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% codepage.c     %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% constraint.c   %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% createdsn.c    %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% dbf2sql.c      %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% demo01.c       %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% filter.c       %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% historic.c     %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% info.c         %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% memo.c         %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% mlang.c        %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% namespacesql.c %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% oraclebi.c     %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% parser1.c      %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% query.c        %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% relation.c     %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% runscript.c    %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% topconn.c      %HB_LIBLIST%
bcc32  -v -y -O2 -lv -d -I%INCLUDE_PATH% -L%LIB_PATH% tstrdd.c       %HB_LIBLIST%

GOTO END

:END

del *.tds
del *.map
dir *.exe
