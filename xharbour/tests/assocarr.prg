PROCEDURE Main()

    LOCAL aaContact  := TAssociativeArray()
    LOCAL aaSalary   := TAssociativeArray()
    LOCAL aaEmployee := TAssociativeArray()
    LOCAL xProperty, xSub, oErr

    // Array Syntax.
    aaSalary[ "Hourly" ] := 12.00
    aaSalary[ "Tax" ]  := 30.00

    // Litral Syntax.
    aaContact  := { "First" => "John", "Last" => "Doe", "Phone" => "555-1212" }

    // OOP Syntax:
    aaEmployee:Contact := aaContact
    aaEmployee:Salary  := aaSalary

    TRY
       ? aaContact[ "Hourly" ] // Error! - Does not exist in THIS Array.
    CATCH oErr
       ? "Caught:", oErr:Description, oErr:Operation
       ?
    END

    // Mixed Syntax.
    ? aaEmployee:Contact["First"]
    ? aaEmployee:Contact:Last
    ?

    FOR EACH xProperty IN aaEmployee:Keys

       IF aaEmployee[ xProperty ]:ClassName == "TASSOCIATIVEARRAY"
          ?
          ? xProperty
          ? "-------"

          FOR EACH xSub IN aaEmployee[ xProperty ]:Keys
             ? "     Key:", xSub
             ? "   Value:", aaEmployee[ xProperty][ xSub ]
          NEXT
       ELSE
          ? "  Key:", xProperty
          ? "Value:", aaEmployee[ xProperty ]
       ENDIF
    NEXT
    ?

RETURN
