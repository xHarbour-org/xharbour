#translate ( ... ) => ()

#define HB_CONSTRUCTOR_NO_DIVERT

#include "hbclass.ch"

#define _AsName_(Name) Name

#XCOMMAND INLINE METHOD <!Method!>[()]             => s_oClass:AddMethod( <(Method)>, CLSMETH _CLASS_NAME_ <Method>(), nScope, .F. ); PP__INLINEMETHOD ; DECLARED METHOD _CLASS_NAME_ <Method>()
#XCOMMAND INLINE METHOD <!Method!>( <params,...> ) => s_oClass:AddMethod( <(Method)>, CLSMETH _CLASS_NAME_ <Method>(), nScope, .F. ); PP__INLINEMETHOD ; DECLARED METHOD _CLASS_NAME_ <Method>( <params> )
#COMMAND ENDMETHOD                                 => PP__ENDMETHOD

