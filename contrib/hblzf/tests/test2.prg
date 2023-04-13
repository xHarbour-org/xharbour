/*
 * $Id$
 */

#include "simpleio.ch"

#define _NREPL_     50

PROCEDURE Main()
   LOCAL cStr := Replicate( memoRead( ExeName() ), _NREPL_ )
   LOCAL aCompressedData := { NIL, NIL, NIL, NIL }
   LOCAL aFuncs := { ;
      {"GZIP" , {|c|hb_gzCompress(c)  }},;
      {"ZLIB" , {|c|hb_Compress(c)    }},;
      {"BZ2 " , {|c|hb_bz2_compress(c)}},;
      {"LZF " , {|c|hb_lzf_compress(c)}} ;
      }
   LOCAL aFuncs2 := { ;
      {"GZIP" , {|c|hb_zUncompress(c)   }},;
      {"ZLIB" , {|c|hb_zUncompress(c)   }},;
      {"BZ2 " , {|c|hb_bz2_uncompress(c)}},;
      {"LZF " , {|c|hb_lzf_decompress(c)}};
      }

   MakeTest( aFuncs , @aCompressedData, @cStr, .T. )
   MakeTest( aFuncs2, @aCompressedData, @cStr, .F. )

   RETURN

STATIC PROCEDURE MakeTest( aFuncs, aCompressedData, cStr, lComp )

   STATIC nStr
   LOCAL nStart, nEnd, nTime, i
   LOCAL cResult

   IF nStr == NIL
      nStr := LEN( cStr )
   ENDIF

   FOR i := 1 TO LEN( aFuncs )
      nStart := secondsCPU()
      IF lComp
         cResult := Eval( aFuncs[i][2], cStr )
         aCompressedData[i] := cResult
      ELSE
         cResult := Eval( aFuncs[i][2], aCompressedData[i] )
      ENDIF
      nEnd  := secondsCPU()
      nTime := nEnd - nStart
      IF lComp
         ? aFuncs[i][1] +":", ALLTRIM(STR(nStr)), "->", ALLTRIM(STR(Len( cResult ))), " Ratio:", PADL(LTRIM(STR((Len( cResult ) / nStr)*100)),6) + "% Time:", PADL(LTRIM(STR(nTime)),5)
      ELSE
         ? aFuncs[i][1] +":", ALLTRIM(STR( Len( aCompressedData[i]) )), "->", ALLTRIM(STR(Len( cResult ))), " Ratio:", PADL(LTRIM(STR((Len( cResult ) / Len(aCompressedData[i]))*100)),6) + "% Time:", PADL(LTRIM(STR(nTime)),5)
      ENDIF
   NEXT
   ?
