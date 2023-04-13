/*
 * $Id$
 */
/*
   test10.prg
   Setting SoftSeek ON/OFF
*/
#include "sixapi.ch"
#define EOL chr(10)
#command ? => outstd(EOL)
#command ? <xx,...> => outstd(<xx>, EOL)
 
PROCEDURE MAIN

   ? 'SYSTEM DEFAULT'
   ? 'sx_SetSoftSeek() =', sx_SetSoftSeek()
   ?
   SET SOFTSEEK ON
   ? 'SET SOFTSEEK ON'
   ? 'sx_SetSoftSeek() =', sx_SetSoftSeek()
   ?
   SET SOFTSEEK OFF
   ? 'SET SOFTSEEK OFF'
   ? 'sx_SetSoftSeek() =', sx_SetSoftSeek()
   ?
   ? 'Before :', sx_SetSoftSeek(), ', sx_SetSoftSeek(.T.) =', sx_SetSoftSeek( .T. ), ', Now :', sx_SetSoftSeek()
   ? 'Before :', sx_SetSoftSeek(), ', sx_SetSoftSeek(.F.) =', sx_SetSoftSeek( .F. ), ', Now :', sx_SetSoftSeek()
