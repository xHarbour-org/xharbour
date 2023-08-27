
/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Crc32 checksum function
 *
 * Copyright 2003 Luiz Rafael Culik Guimaraes <culikr@uol.com.br>
 * www - http://www.xharbour.org
 * SEE ALSO COPYRIGHT NOTICE FOR CRC32 BELOW.
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

/* This file includes code slices from crc32.c for advanced CRC
 * Holder of copyright for this code is:
 *
 * Copyright (C) 1995-2002 Mark Adler
 *
 * ZLIB (containing crc32 code) can be found at:
 * http://www.gzip.org/zlib/
 */

#include "hbzlib.h"

#define		CRC_POLY_KERMIT		0x8408
#define		CRC_START_KERMIT	   0x0000
#define		CRC_START_CCITT_FFFF	0xFFFF
#define		CRC_POLY_CCITT		   0x1021
#define		CRC_START_MODBUS	   0xFFFF
#define		CRC_POLY_16	      	0xA001

static BOOL crc_tab_init		= FALSE;
static BOOL crc_tabccitt_init = FALSE;
static BOOL crc_tab16_init    = FALSE;

static uint16_t crc_tab[256];
static uint16_t crc_tabccitt[256];
static uint16_t crc_tab16[256];

HB_FUNC( HB_CRC32 )
{
   PHB_ITEM pString  = hb_param( 1, HB_IT_STRING );
   ULONG    ulSum    = 0;

   if( pString == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "Must be a string", 1, hb_param( 1, HB_IT_ANY ) );
      return;
   }

   if( ISNUM( 2 ) )
   {
      ulSum = ( ULONG ) hb_parnl( 2 );
   }

   hb_retnint( crc32( ulSum, ( const BYTE * ) hb_itemGetCPtr( pString ),
                      ( uInt ) hb_itemGetCLen( pString ) ) );
}

static void init_crc_tab( void ) 
{
   uint16_t i;
   uint16_t j;
   uint16_t crc;
   uint16_t c;

   for( i=0; i<256; i++ ) 
   {
      crc = 0;
      c   = i;

      for( j=0; j<8; j++ )
      {
         if( (crc ^ c) & 0x0001 ) crc = ( crc >> 1 ) ^ CRC_POLY_KERMIT;
         else                      crc =   crc >> 1;

         c = c >> 1;
      }

      crc_tab[i] = crc;
   }

   crc_tab_init = TRUE;
}

static void init_crcccitt_tab( void ) 
{
	uint16_t i;
	uint16_t j;
	uint16_t crc;
	uint16_t c;

	for( i=0; i<256; i++ )
   {
		crc = 0;
		c   = i << 8;

		for( j=0; j<8; j++ )
      {
			if( (crc ^ c) & 0x8000 ) crc = ( crc << 1 ) ^ CRC_POLY_CCITT;
			else                      crc =   crc << 1;

			c = c << 1;
		}

		crc_tabccitt[i] = crc;
	}

	crc_tabccitt_init = TRUE;
}

static void init_crc16_tab( void ) 
{
	uint16_t i;
	uint16_t j;
	uint16_t crc;
	uint16_t c;

	for( i=0; i<256; i++ ) 
   {
		crc = 0;
		c   = i;

		for( j=0; j<8; j++ ) 
      {
			if( (crc ^ c) & 0x0001 ) crc = ( crc >> 1 ) ^ CRC_POLY_16;
			else                      crc =   crc >> 1;

			c = c >> 1;
		}
		crc_tab16[i] = crc;
	}
	crc_tab16_init = TRUE;
}


static uint16_t crc_kermit( const unsigned char *input_str, size_t num_bytes ) 
{
   uint16_t crc;
   uint16_t low_byte;
   uint16_t high_byte;
   const unsigned char *ptr;
   size_t a;

   if( ! crc_tab_init ) init_crc_tab();

   crc = CRC_START_KERMIT;
   ptr = input_str;

   if( ptr != NULL )
   {
      for( a=0; a < num_bytes; a++ ) 
      {
         crc = (crc >> 8) ^ crc_tab[ (crc ^ (uint16_t) *ptr++) & 0x00FF ];
      }
   }

   low_byte  = (crc & 0xff00) >> 8;
   high_byte = (crc & 0x00ff) << 8;
   crc       = low_byte | high_byte;

   return crc;
}

static uint16_t crc_mcrf4xx( const uint8_t *data, size_t len ) 
{
   uint16_t crc;

   crc = CRC_START_CCITT_FFFF;

   if( !data || len == 0 )
        return crc;

   while( len-- ) 
   {
      int i;
      crc ^= *data++;
      
      for( i=0; i<8; i++ ) 
      {
         if (crc & 1)  crc = (crc >> 1) ^ 0x8408;
         else          crc = (crc >> 1);
      }
   }

   return crc;   
}

static uint16_t crc_ccitt_generic( const unsigned char *input_str, size_t num_bytes, uint16_t start_value ) 
{
   uint16_t crc;
   uint16_t tmp;
   uint16_t short_c;
   const unsigned char *ptr;
   size_t a;

   if( ! crc_tabccitt_init ) init_crcccitt_tab();

   crc = start_value;
   ptr = input_str;

   if( ptr != NULL ) 
   {
      for( a=0; a<num_bytes; a++ ) 
      {
         short_c = 0x00ff & (unsigned short) *ptr;
         tmp     = (crc >> 8) ^ short_c;
         crc     = (crc << 8) ^ crc_tabccitt[tmp];

         ptr++;
      }
   }
   return crc;
}

static uint16_t crc_modbus( const unsigned char *input_str, size_t num_bytes ) 
{
	uint16_t crc;
	uint16_t tmp;
	uint16_t short_c;
	const unsigned char *ptr;
	size_t a;

	if( ! crc_tab16_init ) init_crc16_tab();

	crc = CRC_START_MODBUS;
	ptr = input_str;

	if( ptr != NULL ) 
   {
      for( a=0; a<num_bytes; a++ ) 
      {

         short_c = 0x00ff & (uint16_t) *ptr;
         tmp     =  crc       ^ short_c;
         crc     = (crc >> 8) ^ crc_tab16[ tmp & 0xff ];

         ptr++;
      }
   }
	return crc;
}

static uint16_t crc_ccitt_ffff( const unsigned char *input_str, size_t num_bytes ) 
{
   return crc_ccitt_generic( input_str, num_bytes, CRC_START_CCITT_FFFF );
}

HB_FUNC( HB_CRC_KERMIT )
{
   PHB_ITEM pString  = hb_param( 1, HB_IT_STRING );

   if( pString == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "Must be a string", 1, hb_param( 1, HB_IT_ANY ) );
      return;
   }

   hb_retnl( crc_kermit( ( const unsigned char *  ) hb_parc( 1 ), hb_parclen( 1 ) ) );
}

HB_FUNC( HB_CRC_MCRF4XX )
{
   PHB_ITEM pString  = hb_param( 1, HB_IT_STRING );

   if( pString == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "Must be a string", 1, hb_param( 1, HB_IT_ANY ) );
      return;
   }

   hb_retnl( crc_mcrf4xx( ( const unsigned char *  ) hb_parc( 1 ), hb_parclen( 1 ) ) );
}

HB_FUNC( HB_CCITT_FFFF )
{
   PHB_ITEM pString  = hb_param( 1, HB_IT_STRING );

   if( pString == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "Must be a string", 1, hb_param( 1, HB_IT_ANY ) );
      return;
   }

   hb_retnl( crc_ccitt_ffff( ( const unsigned char *  ) hb_parc( 1 ), hb_parclen( 1 ) ) );
}

HB_FUNC( HB_CRC_MODBUS )
{
   PHB_ITEM pString  = hb_param( 1, HB_IT_STRING );

   if( pString == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "Must be a string", 1, hb_param( 1, HB_IT_ANY ) );
      return;
   }

   hb_retnl( crc_modbus( ( const unsigned char *  ) hb_parc( 1 ), hb_parclen( 1 ) ) );
}
