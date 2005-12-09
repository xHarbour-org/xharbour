Function Main()

   local bar

	 // // Creating some Color (arguments are R, G, B)
   local black  := {0,0,0}
   local white  := {255,255,255}

   local blue   := {0,0,255}
   local yellon := {255,255,128}

   local red    := {255,0,0}

   local ccode13    := "789136043666"
   local ccode8     := "0421000"
   local ccode128   := "34191750090000160123640000020004335850000000190"//"LAVERSON"

   /* here is the list of arguments
   1- Barcode Type 13,8  and 128
   */

   bar := TCode():New(13)

   /* Here is the list of the arguments
   1 - Thickness
   2 - Color of bars
   3 - Color of spaces
   4 - Resolution
   5 - Text Font (0-5)
   */
   bar:Configure( 70 , black, white, 2 , 2 )

   /* Here is the list of the arguments
   1 - Width
   2 - Height
   3 - Filename (empty : display on screen)
   4 - Background color */
   bar:CreateBar( 205, 105,,white)

   // 1 - code bar
   bar:Draw13(ccode13)

   // Build image
   bar:Finish()

   // EAN8
   bar:= TCode():New(8)

   bar:Configure( 70 , black, white, 2 , 2 )

   bar:CreateBar( 154, 100,,white)

   bar:Draw8(ccode8)

   bar:Finish()

   bar:ResizeImage()

   // EAN128
   bar:= TCode():New(128)

   bar:Configure( 50 , black, white, 1 , 1 )

   bar:CreateBar( 559, 88,"Bar128",white)

   // 1- code bar
   // 2- barcode types A/B/C
   // A- Alphanumeric characters uppercase
   // B- Alphanumeric characters upper and lowercase
   // C- Numeric pairs of integer
   bar:Draw128(cCode128,"B")

   bar:Finish()

RETURN NIL
