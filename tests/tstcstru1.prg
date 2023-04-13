#include "cstruct.ch"

#ifdef C_SYNTAX
   #include "wintypes.ch"

   pragma pack(1)
   typedef struct { ;
      char c1[2] ;
      LONG l2;
   } MyNestedStructure, *PMyNestedStructure

   pragma pack(4)
   typedef struct {;
      LPCSTR sName;
      char   cAge;
      UINT   uiWeight;
      DOUBLE dHeight;
      MyNestedStructure Nested;
      MyStructure *pNext;
   } MyStructure
   pragma pack()
#else
   // Explicit Alignment
   C STRUCTURE MyNestedStructure Align 1
     Member c1[2] IS CTYPE_CHAR
     Member l2    IS CTYPE_LONG
   END C STRUCTURE

   // Different Alignment
   C STRUCTURE MyStructure Align 4
     Member sName    IS CTYPE_CHAR_PTR
     Member cAge     IS CTYPE_CHAR
     Member uiWeight IS CTYPE_UNSIGNED_INT
     Member dHeight  IS CTYPE_DOUBLE

     // This is IN-PLACE - Note IS clause (may also use INPLACE clause).
     Member Nested   IS MyNestedStructure

     // This is DETACHED - Note AS clause.
     Member pNext    AS MyStructure
   END C STRUCTURE
#endif

Procedure Main()

   // Initialize from Nested Array.
   LOCAL oStructure IS MyStructure := { "Tom", 10, 35, 1.10, { { 65, 0 }, 10000 } }

   // Manualy initialize DETACHED Structure Member.
   oStructure:pNext IS MyStructure
   WITH OBJECT oStructure:pNext
       :sName    := "Nested"
       :cAge     := 100
       :uiWeight := 1000
       :dHeight  := 2.22

        // Automatic initialization of In-Place Structure Members.
       :Nested:c1[1] := 66
       :Nested:c1[2] := 0
       :Nested:l2 := 20000
   END WITH

   // Here we send it as a true C Structure to a C Function.
   DemoTransferToC( oStructure:GetPointer() )

   // Now display structure values using internal QOut()
   oStructure:SayMembers( "   " )

   wait
   ?

   // Change something at C level and return back
   Send_To_C_Change_Something_And_Return( @oStructure )

   // Now Display members (2nd paramenter) and value via QOut() locally (3rd parameter returns a string)
   ? oStructure:SayMembers( "   ", .t., .t. )

   wait
   ?

   // Here we receive a true C Structure from a C Function.
   oStructure:Buffer( DemoTransferFromC(), .T. ) // .T. (=lAdopt) it is needed to handle memory correctly
                                                  // because structure is created at C level and we have to
                                                  // lock memory at prg level

   // Show us the values in our PRG Structure.
   //oStructure:SayMembers( "   " )
   oStructure:SayMembers( "   ", .t. )

Return

#pragma BEGINDUMP
  #include "hbapi.h"
  #include "hbvm.h"
  
  #pragma pack(1)
  typedef struct _MYNESTEDSTRUCTURE
  {
     char c1[2];
     long l2;
  } MYNESTEDSTRUCTURE;

  #pragma pack(4)
  typedef struct _MYSTRUCTURE
  {
    char * sName;
    char   cAge;
    unsigned int uiWeight;
    double dHeight;
    struct _MYNESTEDSTRUCTURE Nested;
    struct _MYSTRUCTURE *pNext;
  } MY_STRUCTURE;
  #pragma pack()

  static PHB_DYNS pVALUE, pDEVALUE;
  
  static void TstCStru1_Init( void * cargo )
  {
     HB_SYMBOL_UNUSED( cargo );

     pVALUE         = hb_dynsymGetCase( "VALUE" );
     pDEVALUE       = hb_dynsymGetCase( "DEVALUE" );   
  }  
  
  HB_FUNC( DEMOTRANSFERTOC )
  {
     //MY_STRUCTURE *MyStructure = (MY_STRUCTURE * ) hb_param( 1, HB_IT_STRING )->item.asString.value;
     MY_STRUCTURE *MyStructure = hb_parptr( 1 );

     printf( "Name: %s Age: %i\ Weight: %u Height: %f Nested._1 %s Nested._2 %i\n",
             MyStructure->sName, MyStructure->cAge, MyStructure->uiWeight, MyStructure->dHeight,
             (char *) MyStructure->Nested.c1, MyStructure->Nested.l2 );

     MyStructure = MyStructure->pNext;

     printf( "(pNext) Name: %s Age: %i\ Weight: %u Height: %f Nested._1 %s Nested._2 %i\n",
             MyStructure->sName, MyStructure->cAge, MyStructure->uiWeight, MyStructure->dHeight,
             (char *) MyStructure->Nested.c1, MyStructure->Nested.l2 );
  }

  HB_FUNC( SEND_TO_C_CHANGE_SOMETHING_AND_RETURN )
  {
     PHB_ITEM pStructure = hb_param( 1, HB_IT_BYREF );

     if( pStructure && HB_IS_OBJECT( pStructure ) )
     {
        MY_STRUCTURE *MyStructure;

        hb_vmPushSymbol( pVALUE->pSymbol );
        hb_vmPush( pStructure );
        hb_vmSend(0);

        MyStructure = (MY_STRUCTURE *) hb_parc(-1);      
        MyStructure->cAge = 38;

        printf( "Name: %s Age: %i\ Weight: %u Height: %f Nested._1 %s Nested._2 %i\n",
                  MyStructure->sName, MyStructure->cAge, MyStructure->uiWeight, MyStructure->dHeight,
                  (char *) MyStructure->Nested.c1, MyStructure->Nested.l2 );
                
        //::DeValue()
        hb_vmPushSymbol( pDEVALUE->pSymbol );
        hb_vmPush( pStructure );
        hb_vmSend(0);
     }
  }

  HB_FUNC( DEMOTRANSFERFROMC )
  {
     // We NEED to release main structure at end
     MY_STRUCTURE *MyStructure = (MY_STRUCTURE *) hb_xgrab( sizeof( MY_STRUCTURE ) );
     MY_STRUCTURE *MyStructure2 = (MY_STRUCTURE*) hb_xgrab( sizeof( MY_STRUCTURE ) );

     MyStructure->sName = "From C";
     MyStructure->cAge  = 99;
     MyStructure->uiWeight = 77;
     MyStructure->dHeight = 3.14;

     MyStructure->Nested.c1[0] = 'a';
     MyStructure->Nested.c1[1] = 0;
     MyStructure->Nested.l2 = 2000;

     MyStructure->pNext = MyStructure2;

     MyStructure2->sName = "Nested From C";
     MyStructure2->cAge  = 66;
     MyStructure2->uiWeight = 33;
     MyStructure2->dHeight = 1.11;

     MyStructure2->Nested.c1[0] = 'b';
     MyStructure2->Nested.c1[1] = 0;
     MyStructure2->Nested.l2 = 20000;

     MyStructure2->pNext = NULL;

     hb_retclenAdoptRaw( (char *) MyStructure, sizeof( MY_STRUCTURE ) );
  }

  HB_CALL_ON_STARTUP_BEGIN( _tstcstru1_init_ )
    hb_vmAtInit( TstCStru1_Init, NULL );
  HB_CALL_ON_STARTUP_END( _tstcstru1_init_ )

  #if defined(HB_PRAGMA_STARTUP)
   #pragma startup _tstcstru1_init_
  #elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM tstcstru1_init = _tstcstru1_init_;
   #pragma data_seg()
  #endif
  
#pragma ENDDUMP
