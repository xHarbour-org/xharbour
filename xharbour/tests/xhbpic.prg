**************************************************
* xhbbit.prg
* $Id$
* Test for pcx,bmp,lbm,tga viewer with gtalleg
*
* Andi Jahja
*

//----------------------------------------------------------------------------//
proc main( cPcxFile ) // cPcxFile = filename.[bmp|lbm|pcx|tga]

local nError, lSound := .F.

setmode( 40, 85 )
setcursor(0)
clear screen
setwindowtitle( "xHarbour Picture Viewer" )

if PCount() == 0 .and. file( "sample.pcx" )
   cPcxFile := "sample.pcx"
endif

if !empty( cPcxFile )
   @ 02,03 say "xHarbour Picture Viewer" COLOR "B+"
   @ 03,03 say "File Name : " + cPcxFile
   @ 37,28 say "Support :  PCX,BMP,LBM,TGA" COLOR "GR+"
   @ 38,25 say "xHarbour Power ! Press Any Key ....."  COLOR "R+"
   if ( nError := hb_showPCX( cPcxFile ) ) != 0
      if nError == 1
         alert( "Error reading graphic file " + cPcxFile )
      endif
   else
      lSound := ( file( "sample.wav" ) .AND. install_sound() == 0 ) .AND.;
                ( nWav := hb_playwav( "sample.wav", .T. ) ) == 0
      inkey(0)
      if lSound
         CloseWav()
      endif
      alert( "xHarbour Power + Allegro !",, "N/W*" )
   endif
endif

return

//----------------------------------------------------------------------------//
#PRAGMA BEGINDUMP

#include "hbapi.h"
#include "allegro.h"

SAMPLE *the_wav;

HB_FUNC( HB_SHOWPCX )
{
   BITMAP *the_image;
   PALETTE the_palette;

   /* read in the bitmap file */
   the_image = load_bitmap( hb_parc(1), the_palette);

   if (!the_image)
   {
      hb_retni(1);
      return;
   }

   /* select the bitmap palette */
   set_palette(the_palette);

   /* blit the image onto the screen */
   blit(the_image, screen, 0, 0, (SCREEN_W-the_image->w)/2, (SCREEN_H-the_image->h)/2, the_image->w, the_image->h);

   /* destroy the bitmap */
   destroy_bitmap(the_image);

   hb_retni(0);

}

HB_FUNC( SETWINDOWTITLE )
{
   set_window_title( hb_parc(1) );
}

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

HB_FUNC( CLOSEWAV )
{
   destroy_sample( the_wav );
}

HB_FUNC( INSTALL_SOUND )
{
   hb_retni(install_sound(DIGI_AUTODETECT, MIDI_AUTODETECT, hb_cmdargARGV()[0]));
}
#PRAGMA ENDDUMP
