*****************************************************
* Hash massive ( stress ) test
*
* Giancarlo Niccolai (C) 2003
*
* This test demonstrates limits and advantages of
* hashes.
*
* $Id$
*

PROCEDURE Main()
   LOCAL i
   LOCAL nSecs
   LOCAL nRow := 1
   LOCAL hHash

   //LOCAL aaArr := TAssociativeArray()

   Clear Screen
   @nRow++,0 SAY PadC(" --- HASH MASSIVE tests ---", MaxCol() )
   @nRow++,0 SAY PadC(" Giancarlo Niccolai ", MaxCol() )

   @nRow++,5 SAY "Insertion without Preallocation"
   hHash := {:}
   nSecs := Seconds()
   FOR i := 1 TO 10000
      IF I % 500 == 0
         @nRow,5 say Str(i)
      ENDIF
      HSet( hHash, RandStr(5), i )
   NEXT
   @nRow++,5 SAY "HASH Inserting " + Str( Seconds() - nSecs )

   * Doing the same but with preallocation
   @nRow++,5 SAY "Insertion With Preallocation"
   hHash := Hash()  // Alternative grammar
   HAllocate( hHash, 10000 )

   hHash := {:}
   nSecs := Seconds()
   FOR i := 1 TO 10000
      IF I % 500 == 0
         @nRow,5 say Str(i)
      ENDIF
      HSet( hHash, RandStr(5), i )
   NEXT
   @nRow++,5 SAY "HASH Prealloc Inserting " + Str( Seconds() - nSecs )

   hHash['k'] := "an element"
   @nRow++,5 SAY "Data retrival"
   nSecs := Seconds()
   FOR i := 1 TO 1000000
      IF I % 500 == 0
         @nRow,5 say Str(i)
      ENDIF
      IF HGet( hHash, 'k' ) != "an element"
         @nRow++,5 SAY "    ERROR!   "
         EXIT
      ENDIF
   NEXT
   @nRow++,5 SAY "HASH Retreiving " + Str( Seconds() - nSecs )

   @nRow+2,0

   /*
   @5,5 SAY "TAssociativeArray Test"
   aaArr["aaa"] := 'a'
   aaArr["yyy"] := "Elemento"
   aaArr["kkk"] := "Inserting"

   nSecs := Seconds()
   FOR i := 1 TO 10000
      IF I % 500 == 0
         @4,5 say Str(i)
      ENDIF
      aaArr[ RandStr(5) ]:= i
   NEXT
   @5,100 SAY "TASS Inserting " + Str( Seconds() - nSecs )

   nSecs := Seconds()
   FOR i := 1 TO 1000000
      IF I % 500 == 0
         @6,5 say Str(i)
      ENDIF
      IF aaArr[ "kkk" ] != "Inserting"
         //? "ERROR!"
         //EXIT
      ENDIF
   NEXT
   @7,5 SAY "TASS Retreiving " + Str( Seconds() - nSecs )
   */

RETURN

FUNCTION RandStr( nCount )
   LOCAL cVal := ""

   WHILE Len( cVal ) < nCount
      cVal += Chr( Asc('a') + HB_RandomInt( 0, 25 ) )
   ENDDO

RETURN cVal
