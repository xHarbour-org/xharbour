/*
 * $Id: pickarry.prg,v 1.5 2004/05/03 12:56:57 lculik Exp $
 */

STATIC someitems
STATIC lAdd := .F.

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
#include "common.ch"
FUNCTION PICKARRY( T, L, b, r, IN_ARRAY, OUT_ARRAY, aDefault, lAllowAll )

LOCAL nChoice    := 1
LOCAL x
LOCAL NEW_ARRAY  := {}
LOCAL NUM_ELEMS  := Len( IN_ARRAY )
LOCAL PAD_LEN    := ( r - 1 ) - ( L + 1 )
LOCAL lIsChecked := .f.
LOCAL aitems     := IN_ARRAY
LOCAL aTemp
LOCAL cItem
LOCAL cItem1
LOCAL cTemp
LOCAL cOldColor  := Setcolor()
   DEFAULT lAllowAll TO .F.
   someitems := 0

   putscreen()
   Setcolor( 'gr+/rb,b+/w,w+/b,w/b+,w/b,w+/b' )
   @ T - 1, L - 1 CLEAR TO b + 1, r + 1
   @ T - 1, L - 1 TO b + 1, r + 1 double

   FOR x := 1 TO NUM_ELEMS
      IN_ARRAY[ X ] := Padr( '   ' + IN_ARRAY[ X ], PAD_LEN )

      OUT_ARRAY[ X ] := ' ' + OUT_ARRAY[ X ]
   NEXT

   //aTemp :=GetFiles(aitems)

   IF Len( ADefault ) > 0
      FOR EACH cItem IN aDefault

         x := Ascan( IN_ARRAY, { | a, y | Substr( a, 4, At( ' ', Alltrim( a ) ) - 1 ) == cItem } )
         IF x != 0

            IN_ARRAY[ x ] := Stuff( IN_ARRAY[ x ], 2, 1, If( lIsChecked, ' ', 'û' ) )

            OUT_ARRAY[ x ] := Stuff( OUT_ARRAY[ x ], 1, 1, If( lIsChecked, ' ', 'û' ) )
            SOMEITEMS ++
         ELSE
            cItem := Substr( cItem, Rat( '\', cItem ) - 1 )

            x := Ascan( aTemp, { | a, y | Substr( a, 4, At( ' ', a ) - 1 ) == cItem } )

            IF x != 0
               IN_ARRAY[ x ] := Stuff( IN_ARRAY[ x ], 2, 1, If( lIsChecked, ' ', 'û' ) )

               OUT_ARRAY[ x ] := Stuff( OUT_ARRAY[ x ], 1, 1, If( lIsChecked, ' ', 'û' ) )
               SOMEITEMS ++
            ENDIF
         ENDIF
      NEXT
   ENDIF

   DO WHILE nChoice != 0
      nChoice := Achoice( T, L, b, r, IN_ARRAY,, 'keys', nChoice, 1 )

      IF nChoice > 0
         if lAllowAll
            if lAdd
            For nChoice :=  1 to NUM_ELEMS
                 lIsChecked := Substr( IN_ARRAY[ nChoice ], 2, 1 ) == 'û'

                 IN_ARRAY[ nChoice ]  := Stuff( IN_ARRAY[ nChoice ], 2, 1, If( lIsChecked, ' ', 'û' ) )
                 OUT_ARRAY[ nChoice ] := Stuff( OUT_ARRAY[ nChoice ], 1, 1, If( lIsChecked, ' ', 'û' ) )

                 IF lIsChecked
                    SOMEITEMS --
                 ELSE
                    SOMEITEMS ++
                 ENDIF
        
            NEXT
            else
         lIsChecked := Substr( IN_ARRAY[ nChoice ], 2, 1 ) == 'û'

         IN_ARRAY[ nChoice ]  := Stuff( IN_ARRAY[ nChoice ], 2, 1, If( lIsChecked, ' ', 'û' ) )
         OUT_ARRAY[ nChoice ] := Stuff( OUT_ARRAY[ nChoice ], 1, 1, If( lIsChecked, ' ', 'û' ) )

         IF lIsChecked
            SOMEITEMS --
         ELSE
            SOMEITEMS ++
         ENDIF

         nChoice ++

            endif
         else
         lIsChecked := Substr( IN_ARRAY[ nChoice ], 2, 1 ) == 'û'

         IN_ARRAY[ nChoice ]  := Stuff( IN_ARRAY[ nChoice ], 2, 1, If( lIsChecked, ' ', 'û' ) )
         OUT_ARRAY[ nChoice ] := Stuff( OUT_ARRAY[ nChoice ], 1, 1, If( lIsChecked, ' ', 'û' ) )

         IF lIsChecked
            SOMEITEMS --
         ELSE
            SOMEITEMS ++
         ENDIF

         nChoice ++
         endif

      ENDIF

   ENDDO

   FOR x := 1 TO NUM_ELEMS
      IF Left( OUT_ARRAY[ X ], 1 ) == 'û'
         Aadd( NEW_ARRAY, Substr( OUT_ARRAY[ X ], 2 ) )
      ENDIF
      IN_ARRAY[ X ] := Substr( IN_ARRAY[ X ], 4 )

   NEXT

   Asize( OUT_ARRAY, Len( NEW_ARRAY ) )
   Acopy( NEW_ARRAY, OUT_ARRAY )

   getscreen()
   Setcolor( coldColor )
RETURN Len( NEW_ARRAY )

FUNCTION Keys( MODE )

LOCAL RETVAL := 2
LOCAL THEKEY := Lastkey()

   IF MODE = 1
      KEYBOARD Chr( 30 )
   ELSEIF MODE = 2
      KEYBOARD Chr( 31 )
   ELSEIF MODE = 3
      IF THEKEY = 32
         RETVAL := 1
      ELSEIF THEKEY == -4 //F5
         lAdd := !lAdd
         RETVAL := 1
      ELSEIF THEKEY = 27
         RETVAL := 0
      ELSEIF THEKEY = 13 .AND. SOMEITEMS < 1
         RETVAL := 1
         KEYBOARD Chr( 13 )
      ELSEIF THEKEY = 13
         KEYBOARD Chr( 24 )
         RETVAL := 0
      ENDIF
   ENDIF

RETURN ( RETVAL )

STATIC FUNCTION GetFiles( aIn )

LOCAL aRet  := {}
LOCAL cItem := ""

   FOR EACH cItem IN aIn

      cItem := Substr( cItem, 1, At( ' ', cItem ) - 1 )

      Aadd( aRet, Substr( cItem, 1, At( ' ', cItem ) ) )
   NEXT

RETURN aRet
