#include "set.ch"

/*
#command LIST [& <macro> [,] ] [<exp>[,]]                                  ;
         [<off:OFF>]                                                    ;
         [<toPrint: TO PRINTER>]                                        ;
         [TO FILE <(toFile)>]                                           ;
         [FOR <for>]                                                    ;
         [WHILE <while>]                                                ;
         [NEXT <next>]                                                  ;
         [RECORD <rec>]                                                 ;
         [<rest:REST>]                                                  ;
         [ALL]                                                          ;
                                                                        ;
      => __dbList(                                                      ;
                   <.off.>, { [<{exp}>,] [<(macro)>,] }, .t.,          ;
                   <{for}>, <{while}>, <next>, <rec>, <.rest.>,         ;
                   <.toPrint.>, <(toFile)>                              ;
                 )

 */

/*
#command A [<x>] => WasA( [<x>, <{x}>, #<x>, <(x)>, ] [<x>] )
A 1 2 3

#command A [ [B <b> ] <a> ] => AB( [(<a>, <b>),][(<a>,<b>)] )

A B 2 1

A 1 B 2 10

A 1 B 2 10 100

A 1 B 2 10 100 1000

A 1 B 2 10 B 20 100

A 1 B 2 10 B 20 100 1000
*/

extern dbSeek

/*
INIT Procedure InitProc()
    Alert( "Init" )
RETURN

EXIT Procedure ExitProc()
    Alert( "Exit" )
RETURN
*/

Procedure Main()

   memvar cFields2, cFields3, n, cFor, true, var

   field last, age

   LOCAL   cGoodInput := "FIELD->Age == 30"
   LOCAL   cBadInput  := "FIELD->Age -= 30"
   LOCAL   cFields    := "First, Last, Age"
   PRIVATE cFields2   := "First, Last, Ag"
   PRIVATE n          := 1
   PRIVATE cFields3   := "Test( 1, { 'hello[1,2]', 'There[[[[,]' }[n] ), 333"
   PRIVATE cFor       := "Age == 30"

   /*
   SELECT 1
   USE TEST ALIAS Current

   SELECT 2
   USE TEST2 ALIAS Other

   INDEX ON FIRST TO FIRST

   cMacro := "(other->(DBSEEK(current->first)),other->last)"
   LIST &cMacro
   */

   //A 1 2 3

   True := .T.
   IF True //.OR. 0
      IF &( ".T. .OR. 0" )
         Alert( "ShortCut" )
      ENDIF
   ENDIF

   USE TEST
   GO TOP

   //HB_SETMACRO( HB_SM_HARBOUR, .T. )
   Var:= ( &("n++") )
   ? Var

   LIST ALL &(cFields), &cFields3 FOR &cFor

   LIST ALL RecNo(), &cFields FOR &cGoodInput
   LIST ALL RecNo(), &cFields, RecNo() FOR &cGoodInput

   LIST ALL &(cFields2 + 'e') FOR &cGoodInput
   LIST ALL RecNo(), &cFields2.e FOR &cGoodInput
   LIST ALL RecNo(), &cFields2.e, RecNo() FOR &cGoodInput
   */
   WAIT

   GO TOP
   LIST ALL Last, Age FOR &cGoodInput
   WAIT

   //LIST ALL FOR &cBadInput

Return

Function Test( p1, p2 )

Return p2
