? 1; ? 2; ? 3

#command testme <x> => Passed(<x>)

test 7
testz 7

#translate MyList <x>, => WasList(<x>)

MyList A(7,8),

#command A <x> => WasArray(<x>)

A[7]

? 7\789\8

#command abc [bcd <x>] <y> => Test0( <y> )

abc bcd 7 8
abc 7

#command +. <x> => Test(<x>)
#command ++ <x> => Test2(<x>)
#command .T. <x> => Test3(<x>)
#command .not. <x> => Test4(<x>)

#command <> <x> => Test5(<x>)
#command <= <x> => Test6(<x>)
#command >= <x> => Test7(<x>)

#command \[\[ <x> => Test8(<x>)

#command 1.23 <x> => Test9(<x>)
#command 1.23 2.37 <x> => Test10(<x>)

#command .. <x> => Test11(<x>)

#command .23 <x> => Test12(<x>)

. 23 7
.23 7
.23 .7

1 . 23 77
1. 23 77
1.23 77

1.232.37 77
1.23 2.37 77

[ [ 7
[[ 7

< > 7
<> 7

< = 7
<= 7

> = 7
>= 7

. not . 7
.not . 7
.not. 7
! -7

. T . 7
.t. -7

++7
++ 7
+ + 7
+ + -7
++-7

+.7
+. 7
+ . 7
+ . -7

/*
do test .  prg

#XCOMMAND MyComm <x> => Test(<x>) ;;
                        #xcommand <x> \[(\[\<anyParams,...>])] => MyTest(\[\<anyParams>])
*/

/*
#xcommand Comm1 [OPT] [(<x,...>)] => Comm1Test([<x>])
Comm1
Comm1()
*/

//#xcommand Comm1 [([<anyParams,...>])] => Comm1Test([<anyParams>])

//MyComm TestComm

//TestComm( 7,8,9 )

//Comm1( 7,8,9 )

/*
INIT PROC SetError

   ErrorBlock( {|oErr| ErrHandler( oErr ) } )

RETURN

Procedure Main

   Local b

   BEGIN SEQUENCE

   b := &( "{|| ErrHandler() }" )

   ? b

   END SEQUENCE

RETURN

Function ErrHandler( oErr )

   BREAK

RETURN .F.
*/
