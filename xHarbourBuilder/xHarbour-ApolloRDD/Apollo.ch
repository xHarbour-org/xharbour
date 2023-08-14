/*
 *   Apollo RDD for Harbour
 *   Copyright 2002 Patrick Mast
 *
 *   Written by Alexander S.Kresin <alex@belacy.belgorod.su>, December 2002
 *   some portions written by Patrick Mast <email@patrickmast.com>
 */

#include "set.ch"

/* EXTERN M6INIT M6Init() not yet finished */

#define SDENTX		1
#define SDEFOX		2
#define SDENSX		3

#define OPTIMIZE_NONE  0
#define OPTIMIZE_PART  1
#define OPTIMIZE_FULL  2

#define  TRIGGER_ENABLE     1
#define  TRIGGER_DISABLE    2
#define  TRIGGER_REMOVE	    3
#define  TRIGGER_INSTALL    4
#define  TRIGGER_PENDING    5

#define EVENT_PREUSE        1
#define EVENT_POSTUSE       2
#define EVENT_UPDATE	    3
#define EVENT_APPEND        4
#define EVENT_DELETE        5
#define EVENT_RECALL        6
#define EVENT_PACK          7
#define EVENT_ZAP           8
#define EVENT_PUT           9
#define EVENT_GET          10
#define EVENT_PRECLOSE     11
#define EVENT_POSTCLOSE    12

#command SET EPOCH TO <year>                                           ;
      => Set( _SET_EPOCH, <year> )                                     ;
         ;sx_SetEpoch( <year> )

#command SET CENTURY <x:ON,OFF,&>                                      ;
      => __SetCentury( <(x)> )                                         ;
         ;sx_SetCentury( <(x)> )

#command SET CENTURY (<x>)                                             ;
      => __SetCentury( <x> )                                           ;
         ;sx_SetCentury( <x> )

#command SET DATE [TO] AMERICAN                                        ;
      => Set( _SET_DATEFORMAT, if(sx_SetCentury(), "mm/dd/yyyy", "mm/dd/yy" ) ) ;
         ;sx_SetDateFormat( 0 )

#command SET DATE [TO] ANSI                                            ;
      => Set( _SET_DATEFORMAT, if(sx_SetCentury(), "yyyy.mm.dd", "yy.mm.dd" ) ) ;
         ;sx_SetDateFormat( 1 )

#command SET DATE [TO] BRITISH                                         ;
      => Set( _SET_DATEFORMAT, if(sx_SetCentury(), "dd/mm/yyyy", "dd/mm/yy" ) ) ;
         ;sx_SetDateFormat( 2 )

#command SET DATE [TO] FRENCH                                          ;
      => Set( _SET_DATEFORMAT, if(sx_SetCentury(), "dd/mm/yyyy", "dd/mm/yy" ) ) ;
         ;sx_SetDateFormat( 3 )

#command SET DATE [TO] GERMAN                                          ;
      => Set( _SET_DATEFORMAT, if(sx_SetCentury(), "dd.mm.yyyy", "dd.mm.yy" ) ) ;
         ;sx_SetDateFormat( 4 )

#command SET DATE [TO] ITALIAN                                         ;
      => Set( _SET_DATEFORMAT, if(sx_SetCentury(), "dd-mm-yyyy", "dd-mm-yy" ) ) ;
         ;sx_SetDateFormat( 5 )


#command SET FILETYPE TO <x:NTX,CDX,NSX>                               ;
      => sx_SetFileType( if( upper( <(x)> ) == "NTX", 1,               ;
                         if( upper( <(x)> ) == "CDX", 2, 3 ) ) )

#command SET DELETED <x:ON,OFF,&>                                      ;
      =>  Set( _SET_DELETED, <(x)> )                                   ;
          ;sx_SetDeleted( if( upper( <(x)> ) == "ON", .t., .f. ) )
#command SET DELETED (<x>)                                             ;
      =>  Set( _SET_DELETED, <x> ); sx_SetDeleted( <x> )

#command SET EXACT <x:ON,OFF,&>                                        ;
      =>  Set( _SET_EXACT, <(x)> )                                     ;
          ;sx_SetExact( if( upper( <(x)> ) == "ON", .t., .f. ) )
#command SET EXACT (<x>)                                               ;
      =>  Set( _SET_EXACT, <x> ); sx_SetExact( <x> )

#command SET MEMOBLOCK TO <x>                                          ;
      => Sx_SetMemoBlock(<x>)

#command SET SOFTSEEK <x:ON,OFF,&>                                     ;
      =>  Set( _SET_SOFTSEEK, <(x)> )                                  ;
          ;sx_SetSoftSeek( if( upper( <(x)> ) == "ON", .t., .f. ) )
#command SET SOFTSEEK (<x>)                                            ;
      =>  Set( _SET_SOFTSEEK, <x> ); sx_SetSoftSeek( <x> )

#command SET TURBOREAD <x:ON,OFF,&>                                    ;
      =>  sx_SetTurbo( if( upper( <(x)> ) == "ON", .t., .f. ) )

#command SET DIRTYREAD <x:ON,OFF,&>                                    ;
      =>  sx_SetTurbo( if( upper( <(x)> ) == "ON", .t., .f. ) )

#command SET MEMOBINARY <x:ON,OFF,&>                                   ;
      =>  SetMemoBinary( if( upper( <(x)> ) == "ON", .t., .f. ) )

#command USE <(db)>                                                    ;
             [VIA <rdd>]                                               ;
             [ALIAS <als>]                                             ;
             [<new: NEW>]                                              ;
             [<ex: EXCLUSIVE>]                                         ;
             [<sh: SHARED>]                                            ;
             [<ro: READONLY>]                                          ;
             [TRIGGER <trig>]                                          ;
             [INDEX <(index1)> [, <(indexn)>]]                         ;
                                                                       ;
      => sx_SetTrigger( TRIGGER_PENDING, <trig> )                      ;
         ;dbUseArea( <.new.>, <rdd>, <(db)>, <(als)>,                  ;
                    if(<.sh.> .or. <.ex.>, !<.ex.>, NIL), <.ro.>       ;
                  )                                                    ;
                                                                       ;
      [; dbSetIndex( <(index1)> )]                                     ;
      [; dbSetIndex( <(indexn)> )]

#xtranslate Sx_SetTagNo( [<Order>] )                                   ;
      => Sx_SetTag( [<Order>],                                         ;
                    iif(ordNumber()>0,ordBagName(),ordBagName(1)))

#xtranslate Sx_SetTagNo( <Order>, <indexname> )                        ;
      => Sx_SetTag( <Order>, <indexname> )

#xtranslate Sx_TagOrder()                                              ;
      => iif( empty(alias()), 0 ,OrdNumber() )

#xtranslate Sx_SetTagOrder( [<Order>] [,<indexname>] )                 ;
      => Sx_SetTag( [<Order>] [,<indexname>]  )

#xtranslate Sx_SetTagOrd( [<Order>] [,<indexname>] )                   ;
      => Sx_SetTag( [<Order>] [,<indexname>]  )

#command sx_SetTag( <(tag)>, <(indexname)> )                           ;
      => ordSetFocus( <(tag)>, <(indexname)> )

#xtranslate Sx_IndexFilter( [<Order>] )                                ;
      => OrdFor( [<Order>] )

/* Problem with tXBrowse's SetRDD
#xtranslate SetRDD( [<RDD>] )                                          ;
      => RDDSetDefault( [<RDD>] )
*/

#xtranslate Sx_SetDirty( [<x>] )                                       ;
      => Sx_SetTurbo( [<x>] )

#command PACK                                                          ;
      => __dbPack()                                                    ;
         ;Sx_MemoPack()

#xcommand WILDSEEK <x>                                                 ;
      => Sx_WildSeek( <x> )

#xcommand WILDSEEKNEXT <x>                                             ;
      => Sx_WildSeek( <x>, .T. )

#command SET FILTER TO <x>                                             ;
         [NOOPTIMIZE]                                                  ;
      => m6_SetFilter( <{x}>, <"x">, .T. )

#command SET FILTER TO <x:&>                                           ;
         [NOOPTIMIZE]                                                  ;
      => if ( Empty(<(x)>) )                                           ;
       ;    dbClearFilter()                                            ;
       ; else                                                          ;
       ;    m6_SetFilter( <{x}>, <(x)>, .T. )                          ;
       ; endif

#command SET FILTER TO <x>                                             ;
      => m6_SetFilter( <{x}>, <"x">, .F. )

#command SET FILTER TO <x:&>                                           ;
      => if ( Empty(<(x)>) )                                           ;
       ;    dbClearFilter()                                            ;
       ; else                                                          ;
       ;    m6_SetFilter( <{x}>, <(x)>, .F. )                          ;
       ; endif

#command REFRESH FILTER                                                ;
      => m6_RefreshFilter()

#command CLEAR SCOPE                                                   ;
      => Sx_ClrScope()

#xcommand SET SCOPETOP TO <value>                                      ;
      => Sx_SetScope(0, <value>)

#xcommand SET SCOPETOP TO                                              ;
      => Sx_ClrScope(0)

#xcommand SET SCOPEBOTTOM TO <value>                                   ;
      => Sx_SetScope(1, <value>)

#xcommand SET SCOPEBOTTOM TO                                           ;
      => Sx_ClrScope(1)

#command SET SCOPE TO                                                  ;
      => Sx_ClrScope()

#command SET SCOPE TO <value>                                          ;
      => Sx_SetScope(0, <value>)                                       ;
        ;Sx_SetScope(1, <value>)
