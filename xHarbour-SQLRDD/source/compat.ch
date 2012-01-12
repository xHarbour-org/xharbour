/*
* SQLRDD PRG Header (general compatibility to old code)
* Copyright (c) 2003 - Marcelo Lombardo  <lombardo@uol.com.br>
* All Rights Reserved
*/

#xcommand DEFAULT <v1> := <x1> [, <vn> := <xn> ] => ;
                          IF <v1> == NIL ; <v1> := <x1> ; END ;
                          [; IF <vn> == NIL ; <vn> := <xn> ; END ]

#define CRLF      ( chr( 13 ) + chr( 10 ) )
#define CR_LF     ( chr( 13 ) + chr( 10 ) )


#include "hbcompat.ch"
