@echo off
make -fhbodbc.b32
implib ..\..\lib\odbc32.lib odbc32.def
