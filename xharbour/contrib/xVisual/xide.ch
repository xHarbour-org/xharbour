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
