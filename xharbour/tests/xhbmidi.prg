**************************************************
* xhbmidi.prg
* $Id$
* Test for midi player with gtalleg
*
* Andi Jahja
*

//----------------------------------------------------------------------------//
proc main( cMidiFile, lLoop )

local nError

setcursor(0)
set color to N/W*
clear screen
setwindowtitle( "xHarbour MIDI Player" )

if PCount() == 0 .and. file( "sample.mid" )
   cMidiFile := "sample.mid"
endif

if lLoop == NIL
   lLoop := .T.
endif

if !empty( cMidiFile )
   /* install sound driver */
   if install_sound() == 0
      @ 10,15 say "xHarbour Meets Allegro"
      @ 11,15 say "Playing Midi File " + cMidiFile + " ("  + ( if(lLoop, "Looping)", "No Looping)"))
      @ 13,15 say "Press any key to quit ..."
      if ( nError := playmidi( cMidiFile, lLoop ) ) != 0
         if nError == 1
            alert( "Error reading MIDI file " + cMidiFile )
         endif
      else
         inkey(0)
         CloseMidi()
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

MIDI *the_music;

HB_FUNC( PLAYMIDI )
{
   /* read in the MIDI file */
   the_music = load_midi(hb_parc(1));

   if ( !the_music )
   {
      hb_retni(1);
      return;
   }

   /* play the MIDI file */
   play_midi(the_music, hb_parl(2));

   hb_retni(0);
}

HB_FUNC( SETWINDOWTITLE )
{
   set_window_title( hb_parc(1) );
}

HB_FUNC( CLOSEMIDI )
{
   destroy_midi( the_music );
}

HB_FUNC( INSTALL_SOUND )
{
   hb_retni(install_sound(DIGI_AUTODETECT, MIDI_AUTODETECT, hb_cmdargARGV()[0]));
}

#PRAGMA ENDDUMP
