
#xtranslate DEFAULT( <p>, <v> ) => ( <p> := IIF( <p> == NIL, <v>, <p> ) )
#xtranslate SetNewValueReturnOld( <p>, <v> ) => LOCAL xOld, xOld := <p>, IIF( <v> <> NIL, <p> := <v>,  ), xOld

#command IF <lexpr> THEN <*statement*>  =>;
         IF (<lexpr>) ; <statement> ; END

#command IF <lexpr> THEN <statement1> ELSE <statement2> =>;
         IF (<lexpr>) ; <statement1> ; ELSE ; <statement2> ; END

#define STD_IN       0
#define STD_OUT      1

