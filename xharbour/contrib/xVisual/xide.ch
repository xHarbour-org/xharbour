#include "windows.ch"
#include "wingdi.ch"
#include "common.ch"
#include "hbclass.ch"
#include "debug.ch"
#include "what32.ch"
#Include "toolbar.ch"
#Include "winlview.ch"
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
