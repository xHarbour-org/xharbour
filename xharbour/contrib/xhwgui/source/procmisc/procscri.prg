/*
 * Common procedures
 * Scripts
 *
 * Author: Alexander S.Kresin <alex@belacy.belgorod.su>
 *         www - http://www.geocities.com/alkresin/
*/
*+≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤
*+
*+    Source Module => D:\MYAPPS\SOURCE\PROCS\PROCSCRI.PRG
*+
*+    Functions: Function RdScript()
*+               Static Function Fou_If()
*+               Static Function Fou_Do()
*+               Function DoScript()
*+
*+    Reformatted by Click! 2.00 on Apr-12-2001 at  9:01 pm
*+
*+≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤

#include "fileio.ch"
Memvar iscr
STATIC aModules := {}
STATIC nLastError, numlin
#ifndef __WINDOWS__
STATIC y__size := 0, x__size := 0
#endif

FUNCTION RunScript( fname )
Local i, aScript

   IF ( i := Ascan( aModules, {|a|a[1]==fname} ) ) == 0
      aScript := RdScript( fname )
      IF aScript == Nil .OR. Empty( aScript )
         RETURN 1
      ELSE
         Aadd( aModules, { fname, aScript } )
         i := Len( aModules )
      ENDIF
   ENDIF
   DoScript( aModules[ i,2 ] )

RETURN 0

FUNCTION OpenScript( fname, scrkod )
LOCAL han, stroka, aScr, rejim := 0, i
LOCAL strbuf := Space(512), poz := 513
LOCAL aFormCode, aFormName

   scrkod = IIF( scrkod=Nil,"000",scrkod )
   han := FOPEN( fname, FO_READ + FO_SHARED )
   IF han <> - 1
      DO WHILE .T.
         stroka := RDSTR( han,@strbuf,@poz,512 )
         IF LEN( stroka ) = 0
            EXIT
         ELSEIF rejim == 0 .AND. LEFT( stroka, 8 ) == "#SCRIPT/" .AND. ;
                                 Substr( stroka,9,3 ) == scrkod
            aScr := RdScript( han, @strbuf, @poz )
            EXIT
         ELSEIF rejim == 0 .AND. LEFT( stroka, 7 ) == "#BLOCK/" .AND. ;
                                 Substr( stroka,8,3 ) == scrkod
            rejim     := - 1
            aFormCode := {}
            aFormName := {}
         ELSEIF rejim == -1 .AND. LEFT( stroka, 1 ) == "@"
            Aadd( aFormCode, SUBSTR( stroka, 2, 3 ) )
            Aadd( aFormName, SUBSTR( stroka, 6 ) )
         ELSEIF rejim == -1 .AND. LEFT( stroka, 9 ) == "#ENDBLOCK"
            i := WCHOICE( aFormName )
            IF i == 0
               FCLOSE( han )
               RETURN Nil
            ENDIF
            rejim  := 0
            scrkod := aFormCode[ i ]
         ENDIF
      ENDDO
      FCLOSE( han )
   ELSE
      ALERT( "ç•‚ ‰†©´† " + fname )
      RETURN Nil
   ENDIF
RETURN aScr

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function RdScript()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION RdScript( scrSource, strbuf, poz )

LOCAL han
LOCAL rezArray := {}, tmpArray := {}

   numlin := 1
   nLastError := 0
   IF scrSource == Nil
      han := Nil
      poz := 0
   ELSEIF VALTYPE( scrSource ) == "C"
      strbuf := SPACE( 512 )
      poz    := 513
      han    := FOPEN( scrSource, FO_READ + FO_SHARED )
   ELSE
      han := scrSource
   ENDIF
   IF han == Nil .OR. han <> - 1
      IF VALTYPE( scrSource ) == "C"
         WndOut( "Compiling ..." )
         WndOut( "" )
      ENDIF
      IF !CompileScr( han, @strbuf, @poz, rezArray, tmpArray, 0, scrSource )
         rezArray := Nil
      ENDIF
      IF scrSource != Nil .AND. VALTYPE( scrSource ) == "C"
         WndOut()
         FCLOSE( han )
      ENDIF
   ELSE
#ifdef __WINDOWS__
#ifdef ENGLISH
      MsgStop( "Can't open " + scrSource )
#else
      MsgStop( "ç• „§†´Æ·Ï Æ‚™‡Î‚Ï " + scrSource )
#endif
#else
#ifdef ENGLISH
      WndOut( "Can't open " + scrSource )
#else
      WndOut( "ç• „§†´Æ·Ï Æ‚™‡Î‚Ï " + scrSource )
#endif
      WAIT ""
      WndOut()
#endif
      nLastError := -1
      RETURN Nil
   ENDIF
RETURN rezArray

STATIC FUNCTION COMPILESCR( han, strbuf, poz, rezArray, tmpArray, level, scrSource )
LOCAL scom, poz1, stroka, bOldError, i
   DO WHILE .T.
      stroka := RDSTR( han, @strbuf, @poz, 512 )
      IF LEN( stroka ) = 0
         EXIT
      ENDIF
      numlin ++
      stroka := RTRIM( LTRIM( stroka ) )
      IF RIGHT( stroka, 1 ) = CHR( 26 )
         stroka := LEFT( stroka, LEN( stroka ) - 1 )
      ENDIF
      IF .NOT. EMPTY( stroka ) .AND. LEFT( stroka, 2 ) <> "//"

#ifdef __HARBOUR__
         IF Left( stroka,1 ) == "#"
            IF UPPER( Left( stroka,7 ) ) != "#ENDSCR"
               __ppAddRule( stroka )
               LOOP
            ENDIF
         ELSE
            stroka := __Preprocess( stroka )
         ENDIF
#endif

         poz1 := AT( " ", stroka )
         scom := UPPER( SUBSTR( stroka, 1, IIF( poz1 <> 0, poz1 - 1, 999 ) ) )
         DO CASE
         CASE scom = "PRIVATE"
            IF LEN( rezArray ) == 0 .OR. VALTYPE( ATAIL( rezArray ) ) == "C"
               AADD( rezArray, ALLTRIM( SUBSTR( stroka, 9 ) ) )
               AADD( tmpArray, "" )
            ELSE
               nLastError := 1
               RETURN .F.
            ENDIF
         CASE ( scom = "DO" .AND. UPPER( SUBSTR( stroka, 4, 5 ) ) = "WHILE" ) ;
                .OR. scom == "WHILE"
            AADD( tmpArray, stroka )
            AADD( rezArray, .F. )
         CASE scom = "ENDDO"
            IF .NOT. Fou_Do( rezArray, tmpArray )
               nLastError := 2
               RETURN .F.
            ENDIF
         CASE scom = "EXIT"
            AADD( tmpArray, "EXIT" )
            AADD( rezArray, .F. )
         CASE scom = "LOOP"
            AADD( tmpArray, "LOOP" )
            AADD( rezArray, .F. )
         CASE scom = "IF"
            AADD( tmpArray, stroka )
            AADD( rezArray, .F. )
         CASE scom = "ELSEIF"
            IF .NOT. Fou_If( rezArray, tmpArray, .T. )
               nLastError := 3
               RETURN .F.
            ENDIF
            AADD( tmpArray, SUBSTR( stroka, 5 ) )
            AADD( rezArray, .F. )
         CASE scom = "ELSE"
            IF .NOT. Fou_If( rezArray, tmpArray, .T. )
               nLastError := 1
               RETURN .F.
            ENDIF
            AADD( tmpArray, "IF .T." )
            AADD( rezArray, .F. )
         CASE scom = "ENDIF"
            IF .NOT. Fou_If( rezArray, tmpArray, .F. )
               nLastError := 1
               RETURN .F.
            ENDIF
         CASE scom = "RETURN"
            AADD( rezArray, {||EndScript()} )
            AADD( tmpArray, "" )
            IF level > 0
               FOR i := Len(tmpArray)-1  TO 1 STEP - 1
                  IF !EMPTY( tmpArray[ i ] ) .AND. ;
                     ( UPPER( LEFT( tmpArray[ i ], 2 ) ) = "IF" .OR. ;
                       UPPER( LEFT( tmpArray[ i ], 2 ) ) = "WHILE" .OR. ;
                       UPPER( LEFT( tmpArray[ i ], 2 ) ) = "DO WHILE" )
                     EXIT   
                  ENDIF
               NEXT
               IF i == 0
                  RETURN .T.
               ENDIF
            ENDIF
         CASE scom = "FUNCTION"
            stroka := Ltrim( Substr( stroka,poz1+1 ) )
            poz1 := At( "(",stroka )
            scom := UPPER( SUBSTR( stroka, 1, IIF( poz1 <> 0, poz1 - 1, 999 ) ) )
            AADD( rezArray, { scom,{} } )
            AADD( tmpArray, "" )
            IF !CompileScr( han, @strbuf, @poz, rezArray[Len(rezArray),2], {}, 0 )
               RETURN .F.
            ENDIF
         CASE scom = "#ENDSCRIPT"
            RETURN .T.
         OTHERWISE
            bOldError := ERRORBLOCK( { | e | MacroError(1,e,stroka) } )
            BEGIN SEQUENCE
               AADD( rezArray, &( "{||" + ALLTRIM( stroka ) + "}" ) )
            RECOVER
               IF scrSource != Nil .AND. VALTYPE( scrSource ) == "C"
                  WndOut()
                  FCLOSE( han )
               ENDIF
               ERRORBLOCK( bOldError )
               RETURN .F.
            END SEQUENCE
            ERRORBLOCK( bOldError )
            AADD( tmpArray, "" )
         ENDCASE
      ENDIF
   ENDDO
RETURN .T.

STATIC FUNCTION MacroError( nm, e, stroka )

#ifdef __WINDOWS__
   IF nm == 1
      MsgStop( ErrorMessage( e ) + Chr(10)+Chr(13) + "in" + Chr(10)+Chr(13) + ;
             AllTrim(stroka),"Script compiling error" )
   ELSEIF nm == 2
      MsgStop( ErrorMessage( e ),"Script variables error" )
   ELSEIF nm == 3
      MsgStop( ErrorMessage( e ),"Script execution error("+Trim(Str(stroka))+")" )
   ENDIF
#else
   IF nm == 1
      ALERT( "Error in;" + AllTrim(stroka) )
   ELSEIF nm == 2
      Alert( "Script variables error" )
   ELSEIF nm == 3
      Alert( "Script execution error("+Trim(Str(stroka))+")" )
   ENDIF
#endif
   BREAK
RETURN .T.

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Static Function Fou_If()
*+
*+    Called from ( procscri.prg )   3 - function rdscript()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
STATIC FUNCTION Fou_If( rezArray, tmpArray, prju )

LOCAL i, j, bOldError
   IF prju
      AADD( tmpArray, "JUMP" )
      AADD( rezArray, .F. )
   ENDIF
   j := LEN( rezArray )
   FOR i := j TO 1 STEP - 1
      IF .NOT. EMPTY( tmpArray[ i ] ) .AND. UPPER( LEFT( tmpArray[ i ], 2 ) ) = "IF"
         bOldError := ERRORBLOCK( { | e | MacroError(1,e,tmpArray[ i ]) } )
         BEGIN SEQUENCE
            rezArray[ i ] = &( "{||IIF(" + ALLTRIM( SUBSTR( tmpArray[ i ], 4 ) ) + ;
                 ",.T.,iscr:=" + LTRIM( STR( j, 5 ) ) + ")}" )
         RECOVER
            ERRORBLOCK( bOldError )
            RETURN .F.
         END SEQUENCE
         ERRORBLOCK( bOldError )
         tmpArray[ i ] = ""
         i --
         IF i > 0 .AND. .NOT. EMPTY( tmpArray[ i ] ) .AND. tmpArray[ i ] = "JUMP"
            rezArray[ i ] = &( "{||iscr:=" + LTRIM( STR( IIF( prju, j - 1, j ), 5 ) ) + "}" )
            tmpArray[ i ] = ""
         ENDIF
         RETURN .T.
      ENDIF
   NEXT
RETURN .F.

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Static Function Fou_Do()
*+
*+    Called from ( procscri.prg )   1 - function rdscript()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
STATIC FUNCTION Fou_Do( rezArray, tmpArray )

LOCAL i, j, iloop := 0, iPos, bOldError
   j := LEN( rezArray )
   FOR i := j TO 1 STEP - 1
      IF .NOT. EMPTY( tmpArray[ i ] ) .AND. LEFT( tmpArray[ i ], 4 ) = "EXIT"
         rezArray[ i ] = &( "{||iscr:=" + LTRIM( STR( j + 1, 5 ) ) + "}" )
         tmpArray[ i ] = ""
      ENDIF
      IF .NOT. EMPTY( tmpArray[ i ] ) .AND. LEFT( tmpArray[ i ], 4 ) = "LOOP"
         iloop := i
      ENDIF
      IF .NOT. EMPTY( tmpArray[ i ] ) .AND. ;
            ( UPPER( LEFT( tmpArray[ i ], 8 ) ) = "DO WHILE" .OR. ;
              UPPER( LEFT( tmpArray[ i ], 5 ) ) = "WHILE" )
         bOldError := ERRORBLOCK( { | e | MacroError(1,e,tmpArray[ i ] ) } )
         BEGIN SEQUENCE
            rezArray[ i ] = &( "{||IIF(" + ALLTRIM( SUBSTR( tmpArray[ i ], ;
                 IIF( UPPER( LEFT( tmpArray[ i ],1 ) ) == "D",10,7 ) ) ) + ;
                 ",.T.,iscr:=" + LTRIM( STR( j + 1, 5 ) ) + ")}" )
         RECOVER
            ERRORBLOCK( bOldError )
            RETURN .F.
         END SEQUENCE
         ERRORBLOCK( bOldError )
         tmpArray[ i ] = ""
         AADD( rezArray, &( "{||iscr:=" + LTRIM( STR( i - 1, 5 ) ) + "}" ) )
         AADD( tmpArray, "" )
         IF iloop > 0
            rezArray[ iloop ] = &( "{||iscr:=" + LTRIM( STR( i - 1, 5 ) ) + "}" )
            tmpArray[ iloop ] = ""
         ENDIF
         RETURN .T.
      ENDIF
   NEXT
RETURN .F.

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function DoScript()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION DoScript( aScript )

LOCAL arlen, stroka, varName, varValue
MEMVAR iscr, bOldError
PRIVATE iscr := 1, bOldError
   IF aScript == Nil .OR. ( arlen := Len( aScript ) ) == 0
      Return .T.
   ENDIF
   DO WHILE VALTYPE( aScript[ iscr ] ) != "B"
      IF VALTYPE( aScript[ iscr ] ) == "C"
         stroka := aScript[ iscr ]
         bOldError := ERRORBLOCK( { | e | MacroError(2,e) } )
         BEGIN SEQUENCE
         DO WHILE !EMPTY( varName := getNextVar( @stroka, @varValue ) )
            PRIVATE &varName
            IF varvalue != Nil
               &varName := &varValue
            ENDIF
         ENDDO
         RECOVER
            WndOut()
            ERRORBLOCK( bOldError )
            Return .F.
         END SEQUENCE
         ERRORBLOCK( bOldError )
      ENDIF
      iscr ++
   ENDDO
   bOldError := ERRORBLOCK( { | e | MacroError(3,e,iscr) } )
   BEGIN SEQUENCE
      DO WHILE iscr > 0 .AND. iscr <= arlen
         EVAL( aScript[ iscr ] )
         iscr ++
      ENDDO
   RECOVER
      WndOut()
      ERRORBLOCK( bOldError )
      Return .F.
   END SEQUENCE
   ERRORBLOCK( bOldError )
   WndOut()
RETURN .T.

FUNCTION EndScript
   iscr := -99
RETURN Nil

FUNCTION CompileErr( nLine )
   nLine := numlin
RETURN nLastError

FUNCTION Codeblock( string )
RETURN &("{||"+string+"}")

#ifdef __WINDOWS__

FUNCTION WndOut()
RETURN Nil

#else

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function WndOut()
*+
*+    Called from ( procscri.prg )   4 - function rdscript()
*+                                   1 - function doscript()
*+                                   1 - function wndget()
*+                                   1 - function wndopen()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION WndOut( sout, noscroll, prnew )
LOCAL y1, x1, y2, x2, oldc, ly__size := (y__size != 0)
STATIC w__buf
   IF sout == Nil .AND. !ly__size
      Return Nil
   ENDIF
   IF y__size == 0
      y__size := 5
      x__size := 30
      prnew   := .T.
   ELSEIF prnew == Nil
      prnew := .F.
   ENDIF
   y1 := 13 - INT( y__size / 2 )
   x1 := 41 - INT( x__size / 2 )
   y2 := y1 + y__size
   x2 := x1 + x__size
   IF sout == Nil 
      RESTSCREEN( y1, x1, y2, x2, w__buf )
      y__size := 0
   ELSE
      oldc := SETCOLOR( "N/W" )
      IF prnew
         w__buf := SAVESCREEN( y1, x1, y2, x2 )
         @ y1, x1, y2, x2 BOX "⁄ƒø≥Ÿƒ¿≥ "
      ELSEIF noscroll = Nil
         SCROLL( y1 + 1, x1 + 1, y2 - 1, x2 - 1, 1 )
      ENDIF
      @ y2 - 1, x1 + 2 SAY sout         
      SETCOLOR( oldc )
   ENDIF
RETURN Nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function WndGet()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION WndGet( sout, varget, spict )

LOCAL y1, x1, y2, x2, oldc
LOCAL GetList := {}
   WndOut( sout )
   y1   := 13 - INT( y__size / 2 )
   x1   := 41 - INT( x__size / 2 )
   y2   := y1 + y__size
   x2   := x1 + x__size
   oldc := SETCOLOR( "N/W" )
   IF LEN( sout ) + IIF( spict = "@D", 8, LEN( spict ) ) > x__size - 3
      SCROLL( y1 + 1, x1 + 1, y2 - 1, x2 - 1, 1 )
   ELSE
      x1 += LEN( sout ) + 1
   ENDIF
   @ y2 - 1, x1 + 2 GET varget PICTURE spict        
   READ
   SETCOLOR( oldc )
RETURN IIF( LASTKEY() = 27, Nil, varget )

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function WndOpen()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION WndOpen( ysize, xsize )

   y__size := ysize
   x__size := xsize
   WndOut( "",, .T. )
RETURN Nil
#endif
