#include "hbclass.ch"

PROCEDURE Main()

    LOCAL MyAssocArray := TAssociativeArray()

    MyAssocArray[ "Name" ] := "John"
    MyAssocArray[ "Age" ]  := 33

    ? MyAssocArray[ "Name" ]
    ? MyAssocArray[ "Age" ]

    MyAssocArray := TAssociativeArray()

    MyAssocArray:Street := "123 Some St."
    MyAssocArray:City := "Los Angeles"

    ? MyAssocArray:Street
    ? MyAssocArray:City

    ? MyAssocArray[ "NotDefinedYet" ]

RETURN
