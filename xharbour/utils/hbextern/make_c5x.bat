@echo off
rem
rem $Id: make_c5x.bat,v 1.1 2000/03/07 02:03:22 vszel Exp $
rem

clipper hbextern.prg /w /n /i..\..\include\
rtlink fi hbextern
del *.obj
