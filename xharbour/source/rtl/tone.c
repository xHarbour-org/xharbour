/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * TONE() function
 *
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbapi.h"
#include "hbapigt.h"

HB_FUNC( TONE )
{
   if( ISNUM( 1 ) )
      hb_gtTone( hb_parnd( 1 ), ( ISNUM( 2 ) ? hb_parnd( 2 ) : 1.0 ) );
}

#if defined( HB_OS_WIN )

#include "hbapi.h"
#include <windows.h>
#include <mmsystem.h>
#include <math.h>

HB_EXTERN_BEGIN
void _winTone( UINT uFrequency, double dwDuration, BOOL bSmoothing, UINT uVolume );
HB_EXTERN_END
#define SAMPLING_RATE 44100

HB_FUNC( HB_TONE )
{
   if ISNUM( 1 )
   {
      _winTone( hb_parni( 1 ),
                  ( ISNUM( 2 ) ? hb_parnd( 2 ) : 1.0 ) * 2100,
                  ISLOG( 3 ) ? hb_parl( 3 ) : TRUE,
                  ISNUM( 4 ) ? hb_parni( 4 ) : 127 );
   }
}

/* Keep it here for access from C programs */
void _winTone( UINT uFrequency, double dwDuration, BOOL bSmoothing, UINT uVolume )
{
   WAVEFORMATEX wfx;
   HWAVEOUT     hWaveOut = NULL;

   wfx.wFormatTag      = WAVE_FORMAT_PCM;
   wfx.nChannels       = 1;
   wfx.nSamplesPerSec  = SAMPLING_RATE;
   wfx.nAvgBytesPerSec = SAMPLING_RATE;
   wfx.nBlockAlign     = 1;
   wfx.wBitsPerSample  = 8;
   wfx.cbSize          = 0;

   /*
   WAVE_MAPPER is the most commonly used device
   CallBack is unnecessary since we only play simple tone
   */
   /* if( waveOutOpen( &hWaveOut, WAVE_MAPPER, &wfx, NULL, NULL, CALLBACK_NULL ) == MMSYSERR_NOERROR )
    */
   if( waveOutOpen( &hWaveOut, WAVE_MAPPER, &wfx, 0, 0, CALLBACK_NULL ) == MMSYSERR_NOERROR )
   {
      char            amp = ( char ) uVolume;
      int             i;
      unsigned char * buffer = (unsigned char*) hb_xgrab( (ULONG) dwDuration );
      double          dKoef  = uFrequency * 2 * 3.1416 / SAMPLING_RATE;
      WAVEHDR         wh;

      if( buffer )
      {
         wh.lpData = (LPSTR) buffer;
         wh.dwBufferLength = (DWORD) dwDuration;
         wh.dwFlags = WHDR_BEGINLOOP;
         wh.dwLoops = 1;

         if( waveOutPrepareHeader( hWaveOut, &wh, sizeof( wh ) ) == MMSYSERR_NOERROR )
         {
            wh.dwFlags = WHDR_BEGINLOOP | WHDR_ENDLOOP | WHDR_PREPARED;
            wh.dwLoops = 1;

            if( bSmoothing )
            {
               /*
                * Manipulating data to smooth sound from clicks at the start
                * and end (particularly noted when we call TONE() many times
                * in a row). This is a simulation of increasing volume gradually
                * before it reaches the peak, and decreasing volume gradually
                * before it reaches the end.
                */
               for( i = 0; i < dwDuration; i++ )
               {
                  if ( i < amp )
                  {
                     buffer[ i ] = (unsigned char) ( cos( i * dKoef ) * i + 127 );
                  }
                  else if ( dwDuration - i <= amp - 1  )
                  {
                     //amp = max( 0, --amp );
                     if( --amp < 0 )
		        amp = 0;
                     buffer[ i ] = (unsigned char) ( cos( i * dKoef ) * amp + 127 );
                  }
                  else
                  {
                     buffer[ i ] = (unsigned char) ( cos( i * dKoef ) * amp + 127 );
                  }
               }
            }
            else
            {
               /*
                * Raw sound, may cause annoying clicks when some tones are played
                * in a row.
                */
               for( i = 0; i < (int) dwDuration; i++ )
               {
                  buffer[ i ] = (unsigned char) ( cos( i * dKoef ) * amp + 127 );
               }
            }

            /*
             * Play the sound here
             */
            if( waveOutWrite( hWaveOut, &wh, sizeof(WAVEHDR) ) == MMSYSERR_NOERROR )
            {
               /*
                * Wait until the sound is finished
                */
               while (!(wh.dwFlags & WHDR_DONE))
               {
                  Sleep( 1 );
               }

               waveOutUnprepareHeader(hWaveOut, &wh, sizeof(WAVEHDR));
            }
         }

         hb_xfree( buffer );
      }
   }
}

#endif
