* ebget.ch
* sample of command translation to organize editboxes in @..GET alike

#IFNDEF __EBGET_CH

#DEFINE __EBGET_CH

#command @ <row>, <col> EBGET <var>                                       ;
                        [LABEL <label>]                                   ;
                        [<multiline: MULTILINE>]                        ;
                        [PICTURE <pic>]                                 ;
                                                                        ;
      => AddEBGet(aEBGets, <row>, <col>, @<var>, <"var">, {|x| <var> := x}, <label>, <.multiline.>, <pic>)

****************************
* constants to aEBGets member,
* according to EBReadGets() convention
* NOTE: a smarter way would be to use CLASS instead of arrays
****************************
#define __GET_LMULTILINE 1
#define __GET_CLABEL 2
#define __GET_NROW   3
#define __GET_NCOL   4
#define __GET_XINIT  5
#define __GET_CPICT  6
#define __GET_CVALTYPE  7
#define __GET_BTEXT   8
#define __GET_BASSIGN 9
#define __GET_NEBID  10
#define __GET_LFOCUSED  11

#ENDIF
