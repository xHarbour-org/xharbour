PROCEDURE Main()

    LOCAL MyAssocArray1 := TAssociativeArray()
    LOCAL MyAssocArray2 := TAssociativeArray()

    MyAssocArray1[ "Name" ] := "John"
    MyAssocArray1[ "Age" ]  := 33

    ? MyAssocArray1[ "Name" ]
    ? MyAssocArray1[ "Age" ]

    MyAssocArray2:Street := "123 Some St."
    MyAssocArray2:City := "Los Angeles"

    ? MyAssocArray2:Street
    ? MyAssocArray2:City

    ? MyAssocArray2[ "Name" ] // Error! - Does not exist in THIS Object.

RETURN
