/*
 * $Id: msghr852.c,v 1.4 2003/06/30 17:07:29 ronpinkas Exp $
 */

/*
 * Harbour Project source code:
 * Language Support Module (HR852)
 *
 * Copyright 2000 Viktor Szakats <viktor.szakats@syenar.hu> (English, from msg_tpl.c)
 * Copyright 2000 Davor Siklic <siki@msoft.cz>
 * Copyright 2003 Vladimir Miholic <vmiholic@sk.hinet.hr>
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

/* Language name: CROATIAN */
/* ISO language code (2 chars): HR */
/* Codepage: 852 */

#include "hbapilng.h"

static HB_LANG s_lang =
{
   {
      /* Identification */

      "HR852",                     /* ID */
      "Croatian",                  /* Name (in English) */
      "Hrvatski",                  /* Name (in native language) */
      "HR",                        /* RFC ID */
      "852",                       /* Codepage */
      "$Revision: 1.4 $ $Date: 2003/06/30 17:07:29 $",         /* Version */

      /* Month names */

      "sijeŸanj",
      "veljaŸa",
      "o§ujak",
      "travanj",
      "svibanj",
      "lipanj",
      "srpanj",
      "kolovoz",
      "rujan",
      "listopad",
      "studeni",
      "prosinac",

      /* Day names */

      "nedjelja",
      "ponedjeljak",
      "utorak",
      "srijeda",
      "Ÿetvrtak",
      "petak",
      "subota",

       /* CA-Cl*pper compatible natmsg items */

       "Datot.baze podat. # Zapisi     Zadnja prom.    Vel.",
       "¦elite joç primjera?",
       "Str.Br.",
       "** Podzbroj **",
       "* Podpodzbroj *",
       "*** Zbroj ***",
       "Ins",
       "   ",
       "Pogreçan podatak",
       "Raspon: ",
       " - ",
       "D/N",
       "POGREæAN IZRAZ",

       /* Error description names */

       "Nepoznata greçka",
       "Pogreçan argument",
       "Pogreçna granica",
       "PrekoraŸenje niza",
       "PrekoraŸenje broja",
       "Dijeljenje s nulom",
       "BrojŸana greçka",
       "Sintaksna greçka",
       "Prekomplicirana operacija",
       "",
       "",
       "Nedostatak memorije",
       "Nedefinirana funkcija",
       "Nema eksportne metode",
       "Varijabla ne postoji",
       "Alijas ne postoji",
       "Nema izvozne varijable",
       "Nedopuçteni znak u aliasu",
       "Alias ve† u upotrebi",
       "",
       "Greçka kreiranja",
       "Greçka otvaranja",
       "Greçka zatvaranja",
       "Greçka Ÿitanja",
       "Greçka zapisivanja",
       "Greçka ispisa",
       "",
       "",
       "",
       "",
       "Operacija nije podr§ana",
       "PrekoraŸenje granice",
       "Otkriven kvar",
       "Tip podatka pogreçan",
       "Du§ina podatka pogreçna",
       "Radno podruŸje nije u upotrebi",
       "Radno podruŸje nije indeksirano",
       "potrebno iskljuŸiv",
       "Potrebno zakljuŸavanje",
       "Zapisanje nije dozvoljeno",
       "Izostalo zakljuŸavanje kod dodavanja",
       "Greçka zakljuŸavanja",
       "",
       "",
       "",
       "",
       "pristup matrici",
       "pridru§ivanje matrici",
       "dimenzija matrice",
       "nije matrica",
       "uvjetan",

       /* Internal error names */

       "Nepopravljiva greçka %lu: ",
       "Greçka obnavljanje neuspjeçno",
       "Nema ERRORBLOCK() za greçku",
       "Previçe povratnih poziva upravljaca greçaka",
       "RDD neispravan ili izostalo uŸitavanje",
       "Neispravan tip metode iz %s",
       "hb_xgrab ne mo§e dodijeliti memoriju",
       "hb_xrealloc pozvan s NULL pokazivaŸem",
       "hb_xrealloc pozvan s neispravnim pokazivaŸem",
       "hb_xrealloc ne mo§e realocirati memoriju",
       "hb_xfree pozvan s neispravnim pokazivaŸem",
       "hb_xfree pozvan s NULL pokazivaŸem",
       "Nije mogu†e prona†i poŸetnu proceduru: \'%s\'",
       "Nema poŸetne procedure",
       "Nepodr§an VM opcod",
       "Simbol element oŸekivan iz %s",
       "Neispravan simbol tip za sebe iz %s",
       "Kodeblok oŸekivan iz %s",
       "Nepravilan tip elementa na staku pokuçaj stavljanja iz %s",
       "PrekoraŸenje staka",
       "Element je bio kopiran u samog sebe iz %s",
       "Neispravan simbol elemenat dodan kao memorijska varijabla %s",
       "PrekoraŸenje memorijskog meÐuspremnika",
       "hb_xgrab zahtjev za dodjelom nul bajta",
       "hb_xrealloc zahtjev za proçirenjem na nul bajtove",
       "hb_xalloc zahtjev za dodjelom nul bajtova",

      /* Texts */

      "DD/MM/YYYY",
      "D",
      "N"
   }
};

HB_LANG_ANNOUNCE( HR852 );

HB_CALL_ON_STARTUP_BEGIN( hb_lang_Init_HR852 )
   hb_langRegister( &s_lang );
HB_CALL_ON_STARTUP_END( hb_lang_Init_HR852 )
#if defined(HB_STATIC_STARTUP) || ( (! defined(__GNUC__)) && (! defined(_MSC_VER)) )
   #pragma startup hb_lang_Init_HR852
#endif

