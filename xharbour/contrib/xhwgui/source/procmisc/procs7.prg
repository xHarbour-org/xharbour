/*
 * Common procedures
 *
 *
 * Author: Alexander S.Kresin <alex@belacy.belgorod.su>
 *         www - http://www.geocities.com/alkresin/
*/
*+膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊
*+
*+    Source Module => D:\MYAPPS\SOURCE\PROCS\PROCS7.PRG
*+
*+    Functions: Function RDSTR()
*+               Function getNextVar()
*+               Function FIND_Z()
*+
*+    Reformatted by Click! 2.00 on Jul-3-2001 at  2:59 pm
*+
*+膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊膊

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function RDSTR()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION RDSTR( han, strbuf, poz, buflen )

LOCAL stro := "", rez, oldpoz, poz1 := 1
   DO WHILE poz1 > 0
      oldpoz := poz
      poz    := AT( CHR( 10 ), SUBSTR( strbuf, poz ) )
      IF poz = 0
         IF han <> Nil
            stro += SUBSTR( strbuf, oldpoz )
            rez  := FREAD( han, @strbuf, buflen )
            IF rez = 0
               RETURN ""
            ELSEIF rez < buflen
               strbuf := SUBSTR( strbuf, 1, rez ) + CHR( 10 ) + CHR( 13 )
            ENDIF
            poz  := AT( CHR( 10 ), strbuf )
            stro += SUBSTR( strbuf, 1, poz )
         ELSE
            stro += Rtrim( SUBSTR( strbuf, oldpoz ) )
            poz  := oldpoz + Len( stro )
            IF Len( stro ) == 0
               RETURN ""
            ENDIF
         ENDIF
      ELSE
         stro += SUBSTR( strbuf, oldpoz, poz )
         poz  += oldpoz - 1
      ENDIF
      poz ++
      poz1 := AT( "&&", stro )
      IF poz1 <> 0
         stro := SUBSTR( stro, 1, poz1 - 1 )
      ENDIF
   ENDDO
   poz1 := LEN( stro )
   IF poz1 > 2 .AND. RIGHT( stro, 1 ) $ CHR( 13 ) + CHR( 10 )
      IF SUBSTR( stro, poz1 - 1, 1 ) $ CHR( 13 ) + CHR( 10 )
         poz1 --
      ENDIF
      stro := SUBSTR( stro, 1, poz1 - 1 )
   ENDIF
RETURN stro

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function getNextVar()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION getNextVar( stroka, varValue )

LOCAL varName, iPosEnd, iPos3
   IF EMPTY( stroka )
      RETURN ""
   ELSE
      IF ( iPosEnd := Find_Z( stroka ) ) == 0
         iPosEnd := IIF( RIGHT( stroka, 1 ) = ';', LEN( stroka ), LEN( stroka ) + 1 )
      ENDIF
      ipos3    := Find_Z( LEFT( stroka, iPosEnd - 1 ), ':' )
      varName  := Rtrim( LTRIM( LEFT( stroka, IIF( ipos3 = 0, iPosEnd, iPos3 ) - 1 ) ) )
      varValue := IIF( iPos3 <> 0, Ltrim( SUBSTR( stroka, iPos3 + 2, iPosEnd - iPos3 - 2 ) ), Nil )
      stroka   := SUBSTR( stroka, iPosEnd + 1 )
   ENDIF
RETURN varName

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function FIND_Z()
*+
*+    Called from ( procs7.prg   )   2 - function getnextvar()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION FIND_Z( stroka, symb )

LOCAL poz, poz1 := 1, i, j, ms1 := "(){}[]", ms2 := { 0, 0, 0, 0, 0, 0 }
   symb := IIF( symb = Nil, ",", symb )
   DO WHILE .T.
      poz := AT( symb, SUBSTR( stroka, poz1 ) )
      IF poz = 0
         EXIT
      ELSE
         poz := poz + poz1 - 1
      ENDIF
      FOR i := poz1 TO poz - 1
         IF ( j := AT( SUBSTR( stroka, i, 1 ), ms1 ) ) <> 0
            ms2[ j ] ++
         ENDIF
      NEXT
      IF ms2[ 1 ] = ms2[ 2 ] .AND. ms2[ 3 ] = ms2[ 4 ] .AND. ms2[ 5 ] = ms2[ 6 ]
         EXIT
      ELSE
         IF ( j := AT( SUBSTR( stroka, poz, 1 ), ms1 ) ) <> 0
            ms2[ j ] ++
         ENDIF
         poz1 := poz + 1
      ENDIF
   ENDDO
RETURN poz

#ifdef __WINDOWS__

FUNCTION Fchoice()
RETURN 1

#endif

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function CutExten()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION CutExten( fname )

LOCAL i
RETURN IIF( ( i := RAT( '.', fname ) ) = 0, fname, SUBSTR( fname, 1, i - 1 ) )

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function FilExten()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION FilExten( fname )

LOCAL i
RETURN IIF( ( i := RAT( '.', fname ) ) = 0, "", SUBSTR( fname, i + 1 ) )

FUNCTION FilePath( fname )
LOCAL i
RETURN IIF( ( i := RAT( '\', fname ) ) = 0, ;
           IIF( ( i := RAT( '/', fname ) ) = 0, "", LEFT( fname, i ) ), ;
           LEFT( fname, i ) )


FUNCTION CutPath( fname )
LOCAL i
RETURN IIF( ( i := RAT( '\', fname ) ) = 0, ;
           IIF( ( i := RAT( '/', fname ) ) = 0, fname, SUBSTR( fname, i+1 ) ), ;
           SUBSTR( fname, i+1 ) )

Function NextItem( stroka, lFirst, cSep )
Static nPos
Local i, oldPos

   IF ( lFirst!=Nil .AND. lFirst ) .OR. nPos==Nil
      nPos := 1
   ENDIF
   IF cSep == Nil; cSep := ";"; ENDIF
   IF nPos != 99999
      oldPos := nPos
      IF ( i := At( cSep,Substr( stroka,nPos ) ) ) == 0
         nPos := 99999
         Return Ltrim( Rtrim( Substr( stroka,oldPos ) ) )
      ELSE
         nPos += i
         Return Ltrim( Rtrim( Substr( stroka,oldPos,i-1 ) ) )
      ENDIF
   ENDIF
Return ""