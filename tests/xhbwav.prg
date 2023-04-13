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

setcursor(0)
set color to N/W*
clear screen
setwindowtitle("xHarbour WAV Player")

if PCount() == 0 .and. file( "sample.wav" )
   cWavFile := "sample.wav"
endif

if lLoop == NIL
   lLoop := .T.
endif

if !empty( cWavFile )
   /* install sound driver */
   if install_sound() == 0
      @ 10,15 say "xHarbour Meets Allegro"
      @ 11,15 say "Playing Wav File " + cWavFile + " ("  + ( if(lLoop, "Looping)", "No Looping)"))
      @ 13,15 say "Press any key to quit ..."
      if ( nError := hb_playwav( cWavFile, lLoop ) ) != 0
         if nError == 1
            alert( "Error reading WAV file " + cWavFile )
         endif
      else
         inkey(0)
         CloseWav()
         alert( "xHarbour Power + Allegro !",, "N/W*" )
      endif
   else
      alert( "Error initialising sound system" )
   endif
endif

return

//----------------------------------------------------------------------------//
#PRAGMA BEGINDUMP

#include "hbapi.h"
#include "allegro.h"

#ifndef AL_MIDI_AUTODETECT
#define AL_MIDI_AUTODETECT MIDI_AUTODETECT
#define AL_MIDI MIDI
#define al_install_sound install_sound
#define al_load_midi load_midi
#define al_play_midi play_midi
#define al_read_key readkey
#define al_destroy_midi destroy_midi
#endif

SAMPLE *the_wav;

HB_FUNC( HB_PLAYWAV )
{
   int pan = 128;
   int pitch = 1000;

   /* read in the WAV file */
   the_wav = load_sample(hb_parc(1));

   if (!the_wav)
   {
      hb_retni(1);
      return;
   }

   /* play the WAV file */
   play_sample(the_wav, 255, pan, pitch, TRUE);

   hb_retni(0);
}

HB_FUNC( SETWINDOWTITLE )
{
   set_window_title( hb_parc(1) );
}

HB_FUNC( CLOSEWAV )
{
   destroy_sample( the_wav );
}

HB_FUNC( INSTALL_SOUND )
{
   hb_retni(install_sound(DIGI_AUTODETECT, MIDI_AUTODETECT, hb_cmdargARGV()[0]));
}

#PRAGMA ENDDUMP
