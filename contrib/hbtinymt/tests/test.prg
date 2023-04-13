* $Id$
*
* Test program for HBTINYMT Libraries
*
* Syntax (Initialization):
*  .L. := HB_TINYMT32_INIT( nVector1, nVector2, nVector3, [nSeed] )
*  .L. := HB_TINYMT32_INIT_BY_ARRAY( { nVector1, nVector2, nVector3 }, [nSeed], [nKeyLength] )
*
* Syntax (Random number generation, see below):
*   HB_TINYMT32_GENERATE_UINT32()
*   HB_TINYMT32_GENERATE_FLOAT()
*   HB_TINYMT32_GENERATE_FLOAT12()
*   HB_TINYMT32_GENERATE_FLOATOC()
*   HB_TINYMT32_GENERATE_FLOATOO()
*   HB_TINYMT32_GENERATE_32DOUBLE()
* Andi Jahja
*

PROC MAIN()

   LOCAL i , j

   SET DECIMAL TO 0

   IF HB_TINYMT32_INIT( 0x123, 0x234, 0x345 )
      OUTSTD( "32-bit unsigned integers r, where 0 <= r < 2^32" )
      OUTSTD( HB_OSNEWLINE() )
      FOR I := 1 TO 10
         FOR J := 1 TO 5
            OUTSTD( PADL( HB_TINYMT32_GENERATE_UINT32(), 11 ) )
         NEXT
         OUTSTD( HB_OSNEWLINE() )
      NEXT
   ENDIF
   OUTSTD( HB_OSNEWLINE() )

   SET DECIMAL TO 12

   IF HB_TINYMT32_INIT_BY_ARRAY( { 0x123, 0x234, 0x345 } )
      OUTSTD( "Float numbers r, where 0.0 <= r < 1.0" )
      OUTSTD( HB_OSNEWLINE() )
      FOR I := 1 TO 10
         FOR J := 1 TO 5
            OUTSTD( PADL( HB_TINYMT32_GENERATE_FLOAT(), 15 ) )
         NEXT
         OUTSTD( HB_OSNEWLINE() )
      NEXT
      OUTSTD( HB_OSNEWLINE() )
      OUTSTD( HB_OSNEWLINE() )

      OUTSTD( "Float numbers r, where 1.0 <= r < 2.0" )
      OUTSTD( HB_OSNEWLINE() )
      FOR I := 1 TO 10
         FOR J := 1 TO 5
            OUTSTD( PADL( HB_TINYMT32_GENERATE_FLOAT12(), 15 ) )
         NEXT
         OUTSTD( HB_OSNEWLINE() )
      NEXT
      OUTSTD( HB_OSNEWLINE() )
      OUTSTD( HB_OSNEWLINE() )

      OUTSTD( "Float numbers r, where 0.0 < r <= 1.0" )
      OUTSTD( HB_OSNEWLINE() )
      FOR I := 1 TO 10
         FOR J := 1 TO 5
            OUTSTD( PADL( HB_TINYMT32_GENERATE_FLOATOC(), 15 ) )
         NEXT
         OUTSTD( HB_OSNEWLINE() )
      NEXT
      OUTSTD( HB_OSNEWLINE() )
      OUTSTD( HB_OSNEWLINE() )

      OUTSTD("Float numbers r, where 0.0 < r < 1.0" )
      OUTSTD( HB_OSNEWLINE() )
      FOR I := 1 TO 10
         FOR J := 1 TO 5
            OUTSTD( PADL( HB_TINYMT32_GENERATE_FLOATOO(), 15 ) )
         NEXT
         OUTSTD( HB_OSNEWLINE() )
      NEXT
      OUTSTD( HB_OSNEWLINE() )
      OUTSTD( HB_OSNEWLINE() )

      OUTSTD( "32-bit precision double numbers r, where 0.0 <= r < 1.0" )
      OUTSTD( HB_OSNEWLINE() )
      FOR I := 1 TO 10
         FOR J := 1 TO 5
            OUTSTD( PADL( HB_TINYMT32_GENERATE_32DOUBLE(), 15 ) )
         NEXT
         OUTSTD( HB_OSNEWLINE() )
      NEXT
   ENDIF

