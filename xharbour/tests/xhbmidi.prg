**************************************************
* xhbmidi.prg
* $Id: xhbmidi.prg,v 1.1 2004/01/31 00:45:50 andijahja Exp $
* Test for midi player with gtalleg
*
* Andi Jahja
*

//----------------------------------------------------------------------------//
proc main( cMidiFile, lLoop )

local nError

set color to N/W*
clear screen

if PCount() == 0 .and. file( "sample.mid" )
   cMidiFile := "sample.mid"
endif

if lLoop == NIL
   lLoop := .T.
endif

if !empty( cMidiFile )
   @ 10,15 say "xHarbour Meets Allegro"
   @ 11,15 say "Playing Midi File " + cMidiFile + " ("  + ( if(lLoop, "Looping)", "No Looping)"))
   @ 13,15 say "Press any key to quit ..."
   if ( nError := playmidi( cMidiFile, lLoop ) ) != 0
      if nError == 1
         alert( "Error initialising sound system" )
      elseif nError == 2
         alert( "Error reading MIDI file " + cMidiFile )
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
#define AL_MIDI MIDI
#define al_install_sound install_sound
#define al_load_midi load_midi
#define al_play_midi play_midi
#define al_read_key readkey
#define al_destroy_midi destroy_midi
#endif

HB_FUNC( PLAYMIDI )
{
   AL_MIDI *the_music;

   /* install a MIDI sound driver */
   if ( al_install_sound(DIGI_AUTODETECT, AL_MIDI_AUTODETECT, NULL) != 0)
      hb_retni(1);

   /* read in the MIDI file */
   the_music = al_load_midi(hb_parc(1));
   if (!the_music) {
      hb_retni(2);
   }

   al_play_midi(the_music, hb_parl(2));

   /* wait for a keypress */
   al_read_key();

   /* destroy the MIDI file */
   al_destroy_midi(the_music);

   hb_retni(0);
}
#PRAGMA ENDDUMP
