#include "winuser.ch"
#include "wingdi.ch"
#include "common.ch"
#include "hbclass.ch"
#include "debug.ch"
#include "what32.ch"
#Include "commctrl.ch"
#include "wintypes.ch"
#include "cstruct.ch"
#Include "winstruc.ch"

/*
   Support for syntax:

      CONTROL <...> FROM <...>
        :<...> := <...>
      END CONTROL

   inside class declaration.
*/

#translate __STR( <x> ) => #<x>

#command CONTROL <!control!> FROM <!class!> =>  WITH OBJECT <class>() ;;
                                               #define __CONTROL__ <control>

#command END CONTROL => _HB_MEMBER { __CONTROL__ } ;;
                        s_oClass:AddMultiData( , HB_QWITH(), 1, { __STR( __CONTROL__ ) }, .F., .F. ) ;;
                        END WITH;;
                        #undef __CONTROL__

#command OBJECT <!object!> IS <!class!> => WITH OBJECT ( :<object> := <class>() )

#command DATA Caption INIT <x> => DATA FCaption PROTECTED INIT <x>
#command DATA Top     INIT <x> => DATA FTop     PROTECTED INIT <x>
#command DATA Left    INIT <x> => DATA FLeft    PROTECTED INIT <x>
#command DATA Height  INIT <x> => DATA FHeight  PROTECTED INIT <x>
#command DATA Width   INIT <x> => DATA FWidth   PROTECTED INIT <x>

//#command END OBJECT => END WITH
