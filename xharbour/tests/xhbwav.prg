**************************************************
* xhbwav.prg
* $Id$
* Test for wav player with gtalleg
*
* Andi Jahja
*

//----------------------------------------------------------------------------//
proc main( cWavFile, lLoop )

local nError

set color to N/W*
clear screen

if PCount() == 0 .and. file( "sample.wav" )
   cWavFile := "sample.wav"
endif

if lLoop == NIL
   lLoop := .T.
endif

if !empty( cWavFile )
   @ 10,15 say "xHarbour Meets Allegro"
   @ 11,15 say "Playing Wav File " + cWavFile + " ("  + ( if(lLoop, "Looping)", "No Looping)"))
   @ 13,15 say "Press any key to quit ..."
   if ( nError := hb_playwav( cWavFile, lLoop ) ) != 0
      if nError == 1
         alert( "Error initialising sound system" )
      elseif nError == 2
         alert( "Error reading WAV file " + cWavFile )
      endif
   else
      alert( "xHarbour Power + Allegro !",, "N/W*" )
   endif
endif

return

//----------------------------------------------------------------------------//
#PRAGMA BEGINDUMP
#include "hbapi.h"
#include "allegro.h"
HB_FUNC( HB_PLAYWAV )
{
   SAMPLE *the_wav;
   int pan = 128;
   int pitch = 1000;

   /* install a WAV sound driver */
   if ( install_sound(DIGI_AUTODETECT, MIDI_AUTODETECT, NULL) != 0)
      hb_retni(1);

   /* read in the WAV file */
   the_wav = load_sample(hb_parc(1));
   if (!the_wav) {
      hb_retni(2);
   }

   play_sample(the_wav, 255, pan, pitch, TRUE);

   /* wait for a keypress */
   readkey();

   /* destroy the WAV file */
   destroy_sample(the_wav);

   hb_retni(0);
}
#PRAGMA ENDDUMP
