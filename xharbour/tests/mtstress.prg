************************************************************
* threadstress.prg
* $Id: mtstress.prg,v 1.8 2003/10/18 01:15:19 jonnymind Exp $
*
* Stresstest for thread programs
* Stress all those feature that are thread-critical:
* For each, With object, macro operations, codeblocks, Memvars GC
*
* (C) Giancarlo Niccolai
*

#include "hbclass.ch"
#include "hbmemory.ch"

GLOBAL bShow

PROCEDURE Main( cShow )
   REQUEST HB_Random
   LOCAL nStart;

   SET COLOR to w+/b
   SET OUTPUT SAFETY OFF

   CLEAR SCREEN
   @2,15 SAY "X H A R B O U R - Multithreading / Stress tests"

   IF cShow != NIL
      bShow := .F.
   ELSE
      bShow := .T.
   ENDIF

   nStart := Seconds()

   StartThread( @Collector(), 15 )
   StartThread( @Stress(), 1, 5 )
   StartThread( @Stress(), 2, 5 )
   StartThread( @Stress(), 3, 5 )
   StartThread( @Stress(), 4, 5 )
   StartThread( @Stress(), 5, 5 )
   StartThread( @Stress(), 6, 5 )
   StartThread( @Stress(), 7, 5 )
   StartThread( @Stress(), 8, 5 )
   WaitForThreads()

   @20, 10 SAY "Duration: " + Alltrim( Str( Seconds() - nStart ))
   @21, 10 SAY "Done - Press any key to stop"
   Inkey( 0 )
   @24,0

RETURN


PROCEDURE Stress( nId, nRow )
   LOCAL cData, aData
   LOCAL nCount
   LOCAL cRndVal
   LOCAL oTest
   PRIVATE cRnd, var1
   //PRIVATE cMemVal

   nRow += nId - 1
   m->cRnd := "ABCDEFGHILMNOPQRSTUVZ"
   cRndVal := "ABCDEFGHILMNOPQRSTUVZ"

   // Step 40: database test

   @nRow,5 SAY Space( 80 )
   IF File( "test.dbf" )
      Select &nId
      USE test SHARED Alias &( "Test" + Alltrim( Str(nId)) )
      aData := Array(Fcount())
      aFields(aData)
      GOTO Int( HB_Random( 1, Reccount() ) )
      FOR nCount := 1 TO 10000
         // this is to test if separate threads are able not to
         // change other areas or file pointers
         Select &nId
         IF bShow
            @nRow,5 SAY "Thread " + AllTrim( Str( nId ) ) +" DBF test " +;
            Alltrim( Str( nCount ) ) + ": Record "+Alltrim( Str( Recno( ) ))+;
               ":" +& ("FIELD->"+aData[1])
         ELSE
            cData := & ("FIELD->"+aData[1])
         ENDIF

         SKIP // this will create a linear ramp that can be checked
         IF Eof()
            GOTO 1
         ENDIF
      NEXT
      USE
   ENDIF

   //Step 1: foreach test

   @nRow,5 SAY Space( 80 )
   aData := Array( 10000 )
   FOR nCount := 1 TO 10000
      aData[ nCount ] := cRndVal[ Int( HB_Random(1, 21) ) ]
      @nRow,5 SAY "Thread " + AllTrim( Str( nId ) ) +" Foreach pre-test " +;
          Alltrim( Str( nCount ) )
   NEXT

   @nRow,5 SAY Space( 80 )
   nCount := 1
   FOR EACH cData IN aData
      IF bShow
      @nRow,5 SAY "Thread " + AllTrim( Str( nId ) ) +" Foreach test " +;
          Alltrim( Str( nCount ) ) + ": " + cData
      ELSE
         cData := cData + cData
      ENDIF
      nCount ++
   NEXT

   // generates garbage
   aData:= NIL

   // STEP 2: With object test.
   @nRow,5 SAY Space( 80 )

   oTest := TTest():New()
   WITH OBJECT oTest

      FOR nCount := 1 TO 10000

         cData := :cFirst[ Int( HB_Random(1, 21) ) ] + :cSecond[ Int( HB_Random(1, 21)) ] + ;
               :cThird[ Int( HB_Random(1, 21) ) ] + :cFuorth[ Int( HB_Random(1, 21) ) ]
         IF bShow
         @nRow,5 SAY "Thread " + AllTrim( Str( nId ) ) +" With Object test " +;
            Alltrim( Str( nCount ) ) + ": " + cData
         ENDIF
         nCount ++
      NEXT

   END

   // Step 3: private test

   @nRow,5 SAY Space( 80 )

   m->cMemVal := "XXX"
   FOR nCount := 1 TO 10000
      m->cMemVal := m->cRnd[ Int( HB_Random(1, 21) ) ]
      m->cMemVal += m->cRnd[ Int( HB_Random(1, 21) ) ]
      m->cMemVal += m->cRnd[ Int( HB_Random(1, 21) ) ]
      m->cMemVal += m->cRnd[ Int( HB_Random(1, 21) ) ]
      if bShow
         @nRow,5 SAY "Thread " + AllTrim( Str( nId ) ) +" Private test " +;
         Alltrim( Str( nCount ) ) + ": "+m->cMemVal
      ELSE
         cData := m->cMemVal
      ENDIF
   NEXT

   // Step 4: Public Memvar test

   @nRow,5 SAY Space( 80 )
   FOR nCount := 1 TO 10000
      m->var1 := m->cRnd[ Int( HB_Random(1, 21) ) ]
      m->var1 += m->cRnd[ Int( HB_Random(1, 21) ) ]
      m->var1 += m->cRnd[ Int( HB_Random(1, 21) ) ]
      m->var1 += m->cRnd[ Int( HB_Random(1, 21) ) ]
      IF bShow
         @nRow,5 SAY "Thread " + AllTrim( Str( nId ) ) +" Memvar test " +;
         Alltrim( Str( nCount ) ) + ": "+m->var1
      ELSE
         cData := m->var1
      ENDIF
   NEXT

   // Step 5: macro test

   @nRow,5 SAY Space( 80 )
   FOR nCount := 1 TO 10000
      cData := "cRndMem := cRnd[ Int( HB_Random(1, 21) ) ] + cRnd[ Int( HB_Random(1, 21) ) ] + cRnd[ Int( HB_Random(1, 21) ) ]"
      &cData
      IF bShow
         @nRow,5 SAY "Thread " + AllTrim( Str( nId ) ) +" Macro test " +;
         Alltrim( Str( nCount ) ) + ": "+ m->cRndMem
      ENDIF
   NEXT


RETURN


PROCEDURE Collector( nRow )
   LOCAL nCount
   // Exit when only main thread and this thread are left
   ThreadSleep( 250 )
   nCount := 1

   DO WHILE HB_ThreadCountStacks() > 2
      @nRow, 5 SAY "Collector loop :" + AllTrim( Str( nCount ) )
      HB_GCALL( .T. )
      ThreadSleep( 250 )
      nCount ++
   ENDDO

   @nRow, 5 SAY "Collector loop : DONE         "

RETURN


CLASS TTest
   DATA cFirst
   DATA cSecond
   DATA cThird
   DATA cFuorth

   METHOD New()
ENDCLASS

METHOD New()
   ::cFirst  := "ABCDEFGHILMNOPQRSTUVZ"
   ::cSecond := "ABCDEFGHILMNOPQRSTUVZ"
   ::cThird  := "ABCDEFGHILMNOPQRSTUVZ"
   ::cFuorth := "ABCDEFGHILMNOPQRSTUVZ"
RETURN Self
