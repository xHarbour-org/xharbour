/*
   $Id$
   Test program for Stand-Alone Unicode String Conversion
   WARNING: Must link HBCC.LIB
*/

REQUEST HB_GT_WIN_DEFAULT

PROCEDURE MAIN()

   LOCAL aCP := HB_UNIAVAILABLE()
   LOCAL i

   OutStd( "No", chr(9), "ID", chr(9), "Description", hb_osnewline() )
   FOR EACH i IN aCP
      OutStd( strzero(hb_enumindex(),2), chr(9), i[1], chr(9), i[2], hb_osNewLine() )
   NEXT
   OutStd( "CP Available =", ltrim(str(Len( aCP ))), hb_osNewLine() )
   OutStd( HB_UNI2STR(  "Invitaci\u00F3n","CP850"), hb_osNewLine() )
   OutStd( HB_UNI2STR(  "la ra\u00DDz del t\u00DArmino Espa\u00F1a","CP850"), hb_osNewLine() )
   OutStd( HB_STR2UNI(  "la raÌz del tÈrmino Espa§a", "CP850" ), hb_osNewLine() )
   OutStd( HB_STR2UNI(  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", "CP850" ), hb_osNewLine() )
   OutStd( HB_UNISTRLEN("Invitaci\u00F3n"), hb_osNewLine() )
   OutStd( HB_STR2UNI(  "AèBCïDEêFGHIJKLúMN•O£PQRSòTUVWXYZ†°", "PL-MAZ" ), hb_osNewLine() )
   OutStd( HB_STR2UNI(  "aÜbcçdeëfghijklímn§o¢pqrsûtuvwxyz¶ß", "PL-MAZ" ), hb_osNewLine() )
   OutStd( HB_UNI2STR(  "A\u0104BC\u0106DE\u0118FGHIJKL\u0141MN\u0143O\u00D3PQRS\u015ATUVWXYZ\u0179\u017B", "PL-MAZ"), hb_osNewLine() )
   OutStd( HB_UNI2STR(  "a\u0105bc\u0107de\u0119fghijkl\u0142mn\u0144o\u00F3pqrs\u015Btuvwxyz\u017A\u017C", "PL-MAZ"), hb_osNewLine() )

   RETURN
