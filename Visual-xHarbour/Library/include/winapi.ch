#include "winuser.ch"
#include "wintypes.ch"
#include "cstruct.ch"

#xcommand DEFAULT <v> TO <x> [, <vN> TO <xN>]         ;
       => IF <v> == nil ; <v> := <x> ; END            ;
          [; IF <vN> == nil ; <vN> := <xN> ; END]

#translate ISNIL( <v1> )       => ( (<v1>) == NIL )
#translate ISARRAY( <v1> )     => ( VALTYPE( <v1> ) == "A" )
#translate ISBLOCK( <v1> )     => ( VALTYPE( <v1> ) == "B" )
#translate ISCHARACTER( <v1> ) => ( VALTYPE( <v1> ) == "C" )
#translate ISCHAR( <v1> )      => ( VALTYPE( <v1> ) == "C" )
#translate ISSTRING( <v1> )    => ( VALTYPE( <v1> ) == "C" )
#translate ISDATE( <v1> )      => ( VALTYPE( <v1> ) == "D" )
#translate ISLOGICAL( <v1> )   => ( VALTYPE( <v1> ) == "L" )
#translate ISNUMBER( <v1> )    => ( VALTYPE( <v1> ) == "N" )
#translate ISNUMERIC( <v1> )   => ( VALTYPE( <v1> ) == "N" )
#translate ISOBJECT( <v1> )    => ( VALTYPE( <v1> ) == "O" )

#translate IFNIL( <v1>,<exp1>,<exp2> )       => IIF( (<v1>) == NIL,<exp1>,<exp2> )
#translate IFARRAY( <v1>,<exp1>,<exp2> )     => IIF( VALTYPE( <v1> ) == "A",<exp1>,<exp2> )
#translate IFBLOCK( <v1>,<exp1>,<exp2> )     => IIF( VALTYPE( <v1> ) == "B",<exp1>,<exp2> )
#translate IFCHARACTER( <v1>,<exp1>,<exp2> ) => IIF( VALTYPE( <v1> ) == "C",<exp1>,<exp2> )
#translate IFCHAR( <v1>,<exp1>,<exp2> )      => IIF( VALTYPE( <v1> ) == "C",<exp1>,<exp2> )
#translate IFSTRING( <v1>,<exp1>,<exp2> )    => IIF( VALTYPE( <v1> ) == "C",<exp1>,<exp2> )
#translate IFDATE( <v1>,<exp1>,<exp2> )      => IIF( VALTYPE( <v1> ) == "D",<exp1>,<exp2> )
#translate IFLOGICAL( <v1>,<exp1>,<exp2> )   => IIF( VALTYPE( <v1> ) == "L",<exp1>,<exp2> )
#translate IFNUMBER( <v1>,<exp1>,<exp2> )    => IIF( VALTYPE( <v1> ) == "N",<exp1>,<exp2> )
#translate IFNUMERIC( <v1>,<exp1>,<exp2> )   => IIF( VALTYPE( <v1> ) == "N",<exp1>,<exp2> )
#translate IFOBJECT( <v1>,<exp1>,<exp2> )    => IIF( VALTYPE( <v1> ) == "O",<exp1>,<exp2> )
#translate IFEMPTY( <v1>,<exp1>,<exp2> )     => IIF( EMPTY( <v1> ),<exp1>,<exp2> )


#ifndef _H_WINAPI

   #define _H_WINAPI

   #ifndef TRUE
      #define TRUE            .T.
   #endif

   #ifndef FALSE
      #define FALSE           .F.
   #endif

   #define CRLF               CHR(13) + CHR(10)

   #define GRID_LCLICK        2
   #define GRID_LDBLCLK       4
   #define GRID_CHAR          8

   #define GRID_F1           16
   #define GRID_F2           32
   #define GRID_F3           64
   #define GRID_F4          128
   #define GRID_F5          256
   #define GRID_F6          512
   #define GRID_F7         1024
   #define GRID_F8         2048
   #define GRID_F9         4096
   #define GRID_F10        8192
   #define GRID_F11       16384
   #define GRID_F12       32768

   #define GRID_UP        65536
   #define GRID_DOWN     131072
   #define GRID_LEFT     262144
   #define GRID_RIGHT    524288

   #define GRID_ENTER   1048576
   #define GRID_TAB     2097152
   #define GRID_SPACE   4194304

   //#define RGB( r, g, b ) ( (r) + ( (g) * 256 ) + ( (b) * 65536 ) )
   #define RGB( r, g, b )  ( (r) + ( (g) << 8 ) + ( (b) << 16 ) )
   #define MAKEARGB( a, r, g, b ) ( ( (a) << 24 ) + ( (b) << 16 ) + ( (g) << 8 ) + ( (r) << 0 ) )

   //#define MAKEWORD( a, b ) ( (a) + ( (b) * 256 ) )
   #define MAKEWORD( a, b ) ( ((a) & 0xFF ) | ( (b) << 8 ) )

   //#define MAKELONG( a, b ) ( (a) + ( (b) * 65536 ) )
   #define MAKELONG( a, b ) ( ((a) & 0xFFFF) | ( (b) << 16 ) )

   #define MAKELPARAM( a, b ) ( ((a) & 0xFFFF) | ( (b) << 16 ) )

   //#define LOWORD(l) ( (l) % 65536 )
   //#define HIWORD(l)  Int( (l) / 65536 )
   //#define LOWORD(l)  ( (l) & 0xFFFF )
   //#define HIWORD(l)  ( (l) >> 16 )

   #define GET_Y_LPARAM(l) ( HIWORD(l) )
   #define GET_X_LPARAM(l) ( LOWORD(l) )

   #define GET_KEYSTATE_WPARAM(wParam)     (LOWORD(wParam))

   //#define LOBYTE(w) ( (w) % 256 )
   //#define HIBYTE(w)  Int( (w) / 256 )
   #define LOBYTE(l)  ( (l) & 0xFF )
   #define HIBYTE(l)  ( (l) >> 8 )
   #define MAX_PATH 260

#endif
