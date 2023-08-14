#command LOCAL <x,...> => object <x>

#command IF <x> => if( <x> ) \{
#command ELSEIF <x> => \} else if( <x> ) \{
#command ELSE => \} else \{
#command END [<*x*>] => \}
#command ENDIF [<*x*>] => \}

#command DO WHILE <x> => while( <x> ) \{
#command WHILE <x> => while( <x> ) \{
#command ENDDO [<*x*>] => \}

#command DO CASE => if( *** ) \{
#command CASE <x> => \} else if( <x> ) \{
#command OTHERWISE => \} else \{
#command ENDCASE [<*x*>] => \}

#command FOR <!var!> = <init> TO <max> STEP <step> => for( <var> = init ; ( <step> \< 0 ? <var> \>= <max> : <var> \<= <max> ) ; var += <step> ) \;\{
#command FOR <!var!> = <init> TO <max> => for( <var> = init ; var \<= <max> ; var++ ) \;\{
#command NEXT [<*x*>] => \}

#command BEGIN SEQUENCE => try \{
#command RECOVER WITH <x> => \} catch <x> \{
#define  BREAK throw
#command END SEQUENCE [<*x*>] => \}

#define LOOP loop
#define EXIT break
#define RETURN return

#translate := <x> => = <x>

#translate .T. => true
#translate .F. => false
#translate .OR. => ||
//#translate .AND. => \&\&
#translate NIL => null

#translate IIF( <x>, <y>, <z> ) => ( <x> ? <y> : <z> )
#translate @ <!x!> => ref <x>
