**************************************************
* xhbwav.prg
* $Id: xhbwav.prg,v 1.1 2004/01/31 01:38:04 andijahja Exp $
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

#ifndef AL_MIDI_AUTODETECT
#define AL_MIDI_AUTODETECT MIDI_AUTODETECT
#define AL_SAMPLE SAMPLE
#define al_install_sound install_sound
#define al_load_sample load_sample
#define al_play_sample play_sample
#define al_read_key readkey
#define al_destroy_sample destroy_sample
#endif

HB_FUNC( HB_PLAYWAV )
{
   AL_SAMPLE *the_wav;
   int pan = 128;
   int pitch = 1000;

   /* install a WAV sound driver */
   if ( al_install_sound(DIGI_AUTODETECT, AL_MIDI_AUTODETECT, NULL) != 0)
      hb_retni(1);

   /* read in the WAV file */
   the_wav = al_load_sample(hb_parc(1));
   if (!the_wav) {
      hb_retni(2);
   }

   al_play_sample(the_wav, 255, pan, pitch, TRUE);

   /* wait for a keypress */
   al_read_key();

   /* destroy the WAV file */
   al_destroy_sample(the_wav);

   hb_retni(0);
}
#PRAGMA ENDDUMP
