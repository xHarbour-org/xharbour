***********************************************************
* clsctor.prg
* $Id: exec.prg,v 1.1 2003/02/18 23:54:57 jonnymind Exp $
*
* Test for class contructor
*
* Francesco Saverio Giudice <info@fsgiudice.com>
*

#include "hbclass.ch"

PROCEDURE MAIN()
   LOCAL oTest

   CLEAR SCREEN

   @3,15 SAY "X H A R B O U R - Call Class using constructor test"

   ? ""
   ? "1) Create Class without parameters create an empty instance"
   ? ""
   oTest := TClass()
   oTest:Show()

   ? ""
   ? "2) As above but with parameters automatically assign values to constructor"
   ? ""
   oTest := TClass( "My initialization parameters", 1, "B" )
   oTest:Show()

   ? ""
   ? "3) Create Class with explicit constructor call"
   ? ""
   oTest := TClass():New( "My initialization parameters", 1, "B" )
   oTest:Show()

   ? ""
   ? "Press any key to quit"
   Inkey(0)

RETURN

CLASS TClass
   DATA   cString
   DATA   nNumber
   DATA   cChar

   METHOD New     CONSTRUCTOR
   METHOD Show
ENDCLASS

METHOD New( cStr, nNum, cChar )
   //TraceLog( 'TClass:New', cStr, nNum, cChar )
   ::cString := cStr
   ::nNumber := nNum
   ::cChar   := cChar
RETURN Self

METHOD Show()
   ? ::cString, ::nNumber, ::cChar
RETURN NIL

