//
// $Id: statinit.prg,v 1.4 2001/05/15 13:02:07 vszakats Exp $
//

// ; Donated to the public domain by
//   Viktor Szakats <viktor.szakats@syenar.hu>

MEMVAR cMyPubVar

STATIC bBlock1 := {|| Hello() }
STATIC bBlock2 := {|| cMyPubVar }

FUNCTION Main()

   PUBLIC cMyPubVar := "Printed from a PUBLIC var from a codeblock assigned to a static variable."

   Eval( bBlock1 )
   ? Eval( bBlock2 )

   RETURN NIL

FUNCTION Hello()

   ? "Printed from a codeblock assigned to a static variable."

   RETURN NIL
