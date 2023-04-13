PROCEDURE Main()

   LOCAL x := 8, y := 0, cMacro := "x"
   LOCAL cString, cEncrypted, cDecrypted

  
   ? 8 & 0
   ? 8 | 0
   ? 8 ^^ 0
   ? 256 >> 8
   ? 256 << 8

   ? x & y
   ? x | y
   ? x ^^ y

   x := 256
   y := 8

   ? x >> y
   ? x << y

   & ( "x" ) := "Macro OK"
   ? & ( "x" )
   ? &cMacro



   cString := "Hello World"
  
   // Simple XOR encryption
   cEncrypted := cString ^^ 0xF3
   cDecrypted := cEncrypted ^^ 0xF3
   ? cEncrypted
   ? cDecrypted
         
   // Password XOR encryption
   cEncrypted := cString ^^ "TopSecret"
   cDecrypted := cEncrypted ^^ "TopSecret"
   ? cEncrypted
   ? cDecrypted
  
   // String XOR checksum calculation
   ? 0 ^^ cString
  
   // Lowercase by setting 0x20 bit
   ? "MiXeD CaSe StRiNg" | 0x20   

RETURN
