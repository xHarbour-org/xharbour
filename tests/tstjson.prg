PROCEDURE Main( cJson )

   LOCAL hResult := { => }

   IF Empty( cJson )
      cJson := '{ "name": "John", "age": 31, "city": "New York" }'
   ENDIF

   ? "JSON string:", cJson

   hb_jsonDecode( cJson, @hResult )

   ? "Decoded:", ValToPrg( hResult )
   ? "Encoded:", hb_jsonEncode( hResult )

RETURN