#include "cstruct.ch"

C STRUCTURE MyStructure
  Member sName    AS CTYPE_CHAR_PTR
  Member cAge     AS CTYPE_CHAR
  Member uiWeight AS CTYPE_UNSIGNED_INT
  Member dHeight  AS CTYPE_DOUBLE
END C STRUCTURE

Procedure Main()

  ClassSyntax()

  ArraySyntax()

Return

Procedure ClassSyntax()

   LOCAL oStructure := C STRUCTURE MyStructure
   LOCAL aFromStructure

   oStructure:sName := "Tom"
   oStructure:cAge  := 10
   oStructure:uiWeight := 35
   oStructure:dHeight  := 1.10

   PassToC( oStructure:Value )

   aFromStructure := oStructure:FromBuffer( GetFromC() )
   aEval( aFromStructure, {|Element| QOut( Element ) } )

Return

Procedure ArraySyntax()

   LOCAL aVar := { "Ron", 40, 78, 1.82 }
   LOCAL aDef := { CTYPE_CHAR_PTR, CTYPE_CHAR, CTYPE_UNSIGNED_INT, CTYPE_DOUBLE }
   LOCAL aFromStructure

   PassToC( ArrayToStructure( aVar, aDef ) )

   aFromStructure := StructureToArray( GetFromC(), aDef )
   aEval( aFromStructure, {|Element| QOut( Element ) } )

Return

#pragma BEGINDUMP

  #include "hbapi.h"

  typedef struct
  {
    char * sName;
    char   cAge;
    unsigned int uiWeight;
    double dHeight;
  } MY_STRUCTURE;

  HB_FUNC( PASSTOC )
  {
     MY_STRUCTURE *MyStructure = (MY_STRUCTURE * ) hb_param( 1, HB_IT_STRING )->item.asString.value;

     printf( " Name: %s Age: %i\ Weight: %u Height: %f\n", MyStructure->sName, MyStructure->cAge, MyStructure->uiWeight, MyStructure->dHeight );
  }

  HB_FUNC( GETFROMC )
  {
     MY_STRUCTURE MyStructure;

     MyStructure.sName = "From C";
     MyStructure.cAge  = 99;
     MyStructure.uiWeight = 77;
     MyStructure.dHeight = 3.14;

     hb_retclen( (char *) &MyStructure, sizeof( MY_STRUCTURE ) );
  }

#pragma ENDDUMP
