/*
 * $Id: 1class.prg,v 1.3 2002/10/20 22:00:51 fsgiudice Exp $
 */

#include "hbclass.ch"
#include "classex.ch"
#include "debug.ch"


PROCEDURE Main()

   LOCAL DC := DemoClass()

   // Initial data displaying
   view dc:top, dc:data1, dc:changed, dc:coord, dc:title

   dc:title := "This is a title"
   view dc:title, dc:changed

   dc:top   := 45
   dc:data1 := 44

   // dc:coord := {1,1} // if uncomment this you gotta an error
   view dc:top, dc:data1, dc:changed, dc:coord

   dc:top   := "A"      // ERROR IN CLASS TYPE CHECKING
   dc:data1 := "B"
   view dc:top, dc:data1, dc:changed

RETURN

CLASS DemoClass
   DATA     data1   AS NUMERIC INIT 98
   DATA     Changed AS LOGICAL INIT .F. READONLY
   PROPERTY Title   AS STRING                  READ GetTitle WRITE SetTitle
   PROPERTY Top     AS NUMERIC INIT 99 INDEX 1 READ FTop     WRITE SetCoord
   PROPERTY Left    AS NUMERIC INIT 90 INDEX 2 READ FLeft    WRITE SetCoord
   PROPERTY Coord   AS ARRAY           READ GetCoord

   METHOD New()       CONSTRUCTOR
   METHOD GetTitle    INLINE IIF( ::FTitle == NIL, "No Title", ::FTitle )
   METHOD SetTitle(x) INLINE ::FTitle := x, ::Changed := .T.
   METHOD GetCoord    INLINE { ::FTop, ::FLeft }
   METHOD SetCoord
ENDCLASS

METHOD New()
RETURN Self

METHOD SetCoord( i, x )
  DO CASE
     CASE i == 1 // Top
       ::FTop  := x
     CASE i == 2 // Left
       ::FLeft := x
  ENDCASE
  view ::FTop, ::FLeft
  ::Changed := .T.
RETURN Self
