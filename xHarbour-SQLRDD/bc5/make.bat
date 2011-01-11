@echo off

set XHB_VERSION=

call build.bat chgStruct
call build.bat constraint
call build.bat createdsn
call build.bat dbf2sql
call build.bat demo01
call build.bat historic
call build.bat info
call build.bat memo
call build.bat mlang
call build.bat parser1
call build.bat query
call build.bat relation
call build.bat topconn
call build.bat tstrdd

call clean.bat

dir *.exe