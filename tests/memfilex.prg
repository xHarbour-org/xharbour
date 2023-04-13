/*
 * $Id$
 */

// ; Donated to the public domain by
//   Viktor Szakats <viktor.szakats@syenar.hu>

MEMVAR mcLongerNameThen10Chars
#ifdef __HARBOUR__
MEMVAR mcLongerNa
#endif
MEMVAR mcString
MEMVAR mnDouble
MEMVAR mnDoubleH
MEMVAR mnInt
MEMVAR mnLong
MEMVAR mdDate
MEMVAR mlLogicalT
MEMVAR mlLogicalF
MEMVAR mxStayHere

FUNCTION Main()
   PRIVATE mcLongerNameThen10Chars := "Long String Name!"
   PRIVATE mcString   := "This is a" + Chr(0) + "string to save."
   PRIVATE mnDouble   := 100.0000
   PRIVATE mnDoubleH  := 5000000000
   PRIVATE mnInt      := 35
   PRIVATE mnLong     := 3000000
   PRIVATE mdDate     := Date()
   PRIVATE mlLogicalT := .F.
   PRIVATE mlLogicalF := .T.
   PRIVATE mcLongerNa := "Ten chars var name"

   SAVE TO memfile ALL EXTENDED
   SAVE TO memsome ALL LIKE "mnDouble*" EXTENDED

   CLEAR MEMORY

   SAVE TO memempt ALL EXTENDED

   PRIVATE mxStayHere := "CONST"

   RESTORE FROM memfile ADDITIVE EXTENDED

   ? mxStayHere
   ? mcLongerNameThen10Chars
   ? mcLongerNa // Can't restore the part of the name beyond the tenth char
                // Now restore-able with "EXTENDED" flag
   ? mcString
   ? mnDouble
   ? mnDoubleH
   ? mnInt
   ? mnLong
   ? mdDate
   ? mlLogicalF
   ? mlLogicalT

   RESTORE FROM memfile EXTENDED

// ? mxStayHere
   ? mcLongerNameThen10Chars
   ? mcLongerNa // Can't restore the part of the name beyond the tenth char
                // Now restore-able with "EXTENDED" flag
   ? mcString
   ? mnDouble
   ? mnDoubleH
   ? mnInt
   ? mnLong
   ? mdDate
   ? mlLogicalF
   ? mlLogicalT

   ? __MRestore( "memfile", .F., .T., "mndouble*", .T. )
   ? mnDouble
   ? mnDoubleH
   ? __mvScope("mnInt")

   RETURN NIL

#ifndef __HARBOUR__
FUNCTION __mvScope()
   RETURN -1
#endif
