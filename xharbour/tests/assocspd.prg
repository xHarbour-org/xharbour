PROCEDURE Main()

    LOCAL MyAssocArray1 := TAssociativeArray()
    LOCAL oErr := ErrorNew()
    LOCAL Counter, nStart := Seconds()

    // Dynamic property Array Syntax
    FOR Counter := 1 TO 100000
       MyAssocArray1[ "Name" ] := "John"
    NEXT
    ? Seconds() - nStart

    // Dynamic property OOP Syntax (about 40% faster)
    nStart := Seconds()
    FOR Counter := 1 TO 100000
       MyAssocArray1:Description := "John"
    NEXT
    ? Seconds() - nStart

    // Normal Object - Same speed as OOP Syntax of AA.
    nStart := Seconds()
    FOR Counter := 1 TO 100000
       oErr:Description := "John"
    NEXT
    ? Seconds() - nStart

RETURN
