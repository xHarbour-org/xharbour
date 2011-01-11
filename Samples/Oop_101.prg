#include "hbclass.ch"

PROCEDURE Main()

    LOCAL oItem
    LOCAL aDbf := {}

    aAdd( aDbf, { "ID",       "C",  5, 0 } )
    aAdd( aDbf, { "Name",     "C", 10, 0 } )
    aAdd( aDbf, { "Price",    "N", 10, 2 } )
    aAdd( aDbf, { "Group",    "C", 10, 0 } )
    aAdd( aDbf, { "Quantity", "N", 10, 2 } )
    aAdd( aDbf, { "Cost",     "N", 10, 2 } )

    TRY
       dbCreate( "Catalog", aDbf )
    CATCH
       Alert( "Sorry could not create the 'Catalog' database!" )
    END

    USE Catalog

    APPEND BLANK

    REPLACE ID       WITH "00001",;
            Name     WITH "TV",;
            Price    WITH 199.95,;
            Group    WITH "Electronics",;
            Quantity WITH 10,;
            Cost     WITH 140.00

    INDEX ON FIELD->ID TO ItemID

    // Note this is the class name, it will return an UN-initialized "array"
    // that has memebrs which I can access using the VAR NAME as an Index
    oItem := SalesItem()
    ? oItem:ID // NIL, no value yet.

    // Initialize using the New() method
    oItem:New( "00001" )
    ? oItem:ID, oItem:Name, oItem:Price, oItem:Quantity

    oItem:AddInventory( 100 )
    // the ::Quantity property will now have higher value.
    ? oItem:ID, oItem:Name, oItem:Price, oItem:Quantity

    oItem:Save()

    ? Catalog->ID, Catalog->Quantity

RETURN

CLASS SalesItem
   VAR ID
   VAR Name
   VAR Price
   VAR Group
   VAR Quantity
   VAR Cost

   METHOD New( ItemID )
   METHOD Save()
   METHOD AddInventory( Quantity )
ENDCLASS

METHOD New( ItemID ) CLASS SalesItem
   LOCAL nPresetArea := Select()

   SELECT Catalog
   SEEK ItemID

   IF Found()
      Self:ID       := Catalog->ID
      Self:Name     := Catalog->Name
      Self:Price    := Catalog->Price
      Self:Group    := Catalog->Group
      Self:Quantity := Catalog->Quantity
      Self:Cost     := Catalog->Cost
   ENDIF

   SELECT ( nPresetArea )
RETURN Self

METHOD Save() CLASS SalesItem
   LOCAL nPresetArea := Select()

   SELECT Catalog
   SEEK Self:ID

   IF Found()
      Catalog->Name     := Self:Name
      Catalog->Price    := Self:Price
      Catalog->Group    := Self:Group
      Catalog->Quantity := Self:Quantity
      Catalog->Cost     := Self:Cost

      COMMIT
   ENDIF

   SELECT ( nPresetArea )
RETURN Self

METHOD AddInventory( Quantity ) CLASS SalesItem
RETURN Self:Quantity += Quantity