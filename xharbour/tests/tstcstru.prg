#include "cstruct.ch"

C STRUCTURE MyStructure Align 4
  Member sName    AS CTYPE_CHAR_PTR
  Member cAge     AS CTYPE_CHAR
  Member uiWeight AS CTYPE_UNSIGNED_INT
  Member dHeight  AS CTYPE_DOUBLE
  Member pNext    AS C STRUCTURE MyStructure
END C STRUCTURE

Procedure Main()

  ClassSyntax()

  // Array Syntax can NOT support nested structures!
  //ArraySyntax()

Return

Procedure ClassSyntax()

   LOCAL oStructure := C STRUCTURE MyStructure
   LOCAL aFromStructure

   oStructure:sName    := "Tom"
   oStructure:cAge     := 10
   oStructure:uiWeight := 35
   oStructure:dHeight  := 1.10
   oStructure:pNext    := C STRUCTURE MyStructure

   WITH OBJECT oStructure:pNext
       :sName    := "Nested"
       :cAge     := 100
       :uiWeight := 1000
       :dHeight  := 2.22
   END WITH

   PassToC( oStructure:Value )

   oStructure:Buffer( GetFromC() )

   aFromStructure := oStructure:Array()

   aEval( aFromStructure, {|Element| QOut( Element ) } )

   aEval( oStructure:pNext:Array, {|Element| QOut( Element ) } )

Return

Procedure ArraySyntax()

   LOCAL aVar := { "Ron", 40, 78, 1.82 }
   LOCAL aDef := { CTYPE_CHAR_PTR, CTYPE_CHAR, CTYPE_UNSIGNED_INT, CTYPE_DOUBLE }
   LOCAL aFromStructure

   PassToC( HB_ArrayToStructure( aVar, aDef ) )

   aFromStructure := HB_StructureToArray( GetFromC(), aDef )
   aEval( aFromStructure, {|Element| QOut( Element ) } )

Return

#pragma BEGINDUMP

  #include "hbapi.h"

  //#pragma pack(1)
  typedef struct _MYSTRUCTURE
  {
    char * sName;
    char   cAge;
    unsigned int uiWeight;
    double dHeight;
    struct _MYSTRUCTURE *pNext;
  } MY_STRUCTURE;
  #pragma pack()

  HB_FUNC( PASSTOC )
  {
     MY_STRUCTURE *MyStructure = (MY_STRUCTURE * ) hb_param( 1, HB_IT_STRING )->item.asString.value;

     printf( " Name: %s Age: %i\ Weight: %u Height: %f\n", MyStructure->sName, MyStructure->cAge, MyStructure->uiWeight, MyStructure->dHeight );

     MyStructure = MyStructure->pNext;

     printf( "(pNext) Name: %s Age: %i\ Weight: %u Height: %f\n", MyStructure->sName, MyStructure->cAge, MyStructure->uiWeight, MyStructure->dHeight );
  }

  HB_FUNC( GETFROMC )
  {
     MY_STRUCTURE *MyStructure = (MY_STRUCTURE *) hb_xgrab( sizeof( MY_STRUCTURE ) );
     MY_STRUCTURE *MyStructure2 = (MY_STRUCTURE* )hb_xgrab( sizeof( MY_STRUCTURE ) );

     MyStructure->sName = "From C";
     MyStructure->cAge  = 99;
     MyStructure->uiWeight = 77;
     MyStructure->dHeight = 3.14;
     MyStructure->pNext = MyStructure2;

     MyStructure2->sName = "Nested From C";
     MyStructure2->cAge  = 66;
     MyStructure2->uiWeight = 33;
     MyStructure2->dHeight = 1.11;
     MyStructure2->pNext = NULL;

     //printf( "\n\n\n\nMyStructure %p, MyStructure2: %p, MyStructure.pNext: %p\n", MyStructure, MyStructure2, MyStructure->pNext );
     //printf( "Sizeof: %i %i\n", sizeof( MY_STRUCTURE ), sizeof( MyStructure2.pNext ) );

     hb_retclenAdopt( (char *) MyStructure, sizeof( MY_STRUCTURE ) );
  }

#pragma ENDDUMP
