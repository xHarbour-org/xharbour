PROCEDURE Main()

    LOCAL MyAssocArray1 := TAssociativeArray()
    LOCAL MyAssocArray2 := TAssociativeArray()
    LOCAL MyAssocArray3 := { "Name" => "Ron", "Age" => 41, "Street" => "2013 Roadrunner Ave." }
    LOCAL xProperty

    MyAssocArray1[ "Name" ] := "John"
    MyAssocArray1[ "Age" ]  := 33

    ? MyAssocArray1[ "Name" ]
    ? MyAssocArray1[ "Age" ]
    ?

    ? MyAssocArray1[1]
    ? MyAssocArray1[2]
    ?

    // OOP Syntax:
    MyAssocArray2:Street := "123 Some St."
    MyAssocArray2:City := "Los Angeles"

    TRY
       ? MyAssocArray2[ "Name" ] // Error! - Does not exist in THIS Array.
    END
    ? MyAssocArray2:Street
    ? MyAssocArray2:City
    ?

    FOR EACH xProperty IN MyAssocArray3
       ? xProperty
    NEXT
    ?

RETURN
