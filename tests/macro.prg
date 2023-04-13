Procedure Main()

   MEMVAR Var

   LOCAL cMacro := "1, 2, 3"
   LOCAL cName := "new_var"

   PRIVATE Var

   ? 'Type( "undeclared_var" ) == "' + Type( "undeclared_var" ) + '" (expected "U")'   
   ? 'Type( "undeclared_var" ) == "' + Type( "undeclared_var" ) + '" (expected "U")'   
   ? 'Type( "undeclared_var := 5" ) == "' + Type( "undeclared_var := 5" ) + '" (expected "N")'
   ? 'Type( "undeclared_var" ) == "' + Type( "undeclared_var" ) + '" (expected "N")'
   ? '&( "undeclared_var := 5" ) == ', &( "undeclared_var := 5" ), ' (expected 5)'
   ? '&( "undeclared_var" ) == ', &( "undeclared_var" ), ' (expected 5)'
   ? 'Type( "undeclared_arr[1] := 5" ) == "' + Type( "undeclared_arr[1] := 5" ) + '" (expected "U")'

   ? 'PUBLIC &cName'
   PUBLIC &cName
   ?? ' (no error)'
   ? '&cName := 10'
   &cName := 10
   ? '&cName == ', &cName
   
   /*
   USE TEST
   INDEX ON FIRST TO FIRST

   DBSEEK( "John", "Mary", .t. )
   */

   Var := Test( &cMacro + 4 )
   ? Var
   
return

Function Test( p1, p2, p3 )

   ? p1, p2, p3

return p3

static function static_function(n)
return n+1
