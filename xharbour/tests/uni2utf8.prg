/*
   $Id$
   Test program for Stand-Alone Unicode String Conversion
   WARNING: Must link HBCC.LIB
*/

#include "simpleio.ch"

#define HB_CSUTF8 4099

PROCEDURE MAIN()

   LOCAL hFile
   LOCAL aCP := { "CP852", "KOI8U", "CP1251", "CP866", "KOI8R", "CP862", "CP1253", "BGMIK" }
   LOCAL aChar := {;
        { "CP852",  "leden, £nor, býezen, duben, kvØten, Yerven, Yervenec, srpen, z ý¡, ý¡jen, listopad, prosinec" },;
        { "KOI8U",  "ó¦ÞÅÎØ, ìÀÔÉÊ, âÅÒÅÚÅÎØ, ë×¦ÔÅÎØ, ôÒÁ×ÅÎØ, þÅÒ×ÅÎØ, ìÉÐÅÎØ, óÅÒÐÅÎØ, ÷ÅÒÅÓÅÎØ, öÏ×ÔÅÎØ, ìÉÓÔÏÐÁÄ, çÒÕÄÅÎØ" },;
        { "KOI8R",  "ñÎ×ÁÒØ, æÅ×ÒÁÌØ, íÁÒÔ, áÐÒÅÌØ, íÁÊ, éÀÎØ, éÀÌØ, á×ÇÕÓÔ, óÅÎÔÑÂÒØ, ïËÔÑÂÒØ, îÏÑÂÒØ, äÅËÁÂÒØ" },;
        { "CP1251", "Ñ³÷åíü, Ëþòèé, Áåðåçåíü, Êâ³òåíü, Òðàâåíü, ×åðâåíü, Ëèïåíü, Ñåðïåíü, Âåðåñåíü, Æîâòåíü, Ëèñòîïàä" },;
        { "CP1251", "ßíóàðè, Ôåâðóàðè, Ìàðò, Àïðèë, Ìàé, Þíè, Þëè, Àâãóñò, Ñåïòåìâðè, Îêòîìâðè, Íîåìâðè, Äåêåìâðè" },;
        { "CP1253", "ÉáíïõÜñéïò, ÖåâñïõÜñéïò, ÌÜñôéïò, Áðñßëéïò, ÌÜúïò, Éïýíéïò, Éïýëéïò, Áýãïõóôïò, ÓåðôÝìâñéïò, Ïêôþâñéïò, ÍïÝìâñéïò, ÄåêÝìâñéïò" },;
        { "CP862",  "˜€…‰, ˜€…˜”, •˜Ž, Œ‰˜”€, ‰€Ž, ‰…‰, ‰Œ…‰, ˆ‘…‚…€, ˜Žˆ”‘, ˜…ˆ—…€, ˜Ž…, ˜Ž–ƒ" },;
        { "BGMIK",  "Ÿ­³ °¨, ”¥¢°³ °¨, Œ °², €¯°¨«, Œ ©, ž­¨, ž«¨, €¢£³±², ‘¥¯²¥¬¢°¨, Žª²®¬¢°¨, ®¥¬¢°¨, „¥ª¥¬¢°¨" },;
        { "CP866",  "‘âã¤§¥­ì, ‹îâë, ‘ ª ¢iª, Šà á ¢iª, Œ ©, —íà¢¥­ì, ‹i¯¥­ì, †­i¢¥­ì, ‚¥à á¥­ì, Š áâàëç­iª, ‹iáâ ¯ ¤, ‘­¥¦ ­ì" };
        }
   LOCAL aHandle := {}
   LOCAL cCP, i, n
   LOCAL cUTF8 := CHR(239) + CHR(187) + CHR(191)

   FOR EACH cCP IN aCP
      AADD( aHandle, { cCP, HB_CSReg( cCP ) } )
   NEXT

   FOR i := 1 TO LEN( aChar )
       IF ( n := AScan( aHandle, { |e| e[ 1 ] == aChar[ i ][ 1 ] } ) ) > 0
          cUTF8 += PadR( aChar[ i ][ 1 ], 8 ) + ": " + ;
             HB_CSToCS( aChar[ i ][ 2 ], aHandle[ n ][ 2 ], HB_CSUTF8 ) +;
             hb_osnewline()
       ENDIF
   NEXT

   hFile := FCreate( "hbccutf8.txt" )
   FWrite( hFile, cUTF8 )
   FClose( hFile )

RETURN
