/*
 * $Id$
 */

To run samples:

(1) Make sure you have GTWVW.LIB in your xHarbour LIB directory
(2) Compile/link the .PRG using BLDWVW <prgname>
(3) WVWTEST9.BAT is a sample on how to compile/link several .PRG and .C
    source codes, and applying Windows XP style when appropriate.


BRIEF DESCRIPTIONS:

AS described in www.csacomputer.com/gtwvw :
prog0.prg    : to be compiled with clipper
prog1.prg    : 1st evolution with gtwvw
prog2.prg    : 2nd evolution with gtwvw

Supporting routines: (required by wvwtest9.prg only)
winapi.c     : some low level routines used in some samples
wvwmouse.prg : sample of how to handle graphic primitives

Supporting files:
test.dbf
some .gif
some .bmp
some .ico

Main programs:
wvwtest9.prg : demo of several gtwvw features.
maincoor.prg : to show differences of MainCoord and Standard Mode.
inpfocus.prg : demo of how to handle input on non topmost window.

Others:
wvt2wvw.ch   : header file to be included in your gtwvt program if you
               wish to link it with gtwvw.
               See gtwvw documentation (and description inside wvt2wvw.ch)
               about how to compile and link gtwvt program using gtwvw.