/*
 * $Id: inhprob.prg,v 1.2 2000/02/14 08:11:56 vszel Exp $
 */

#include "hbclass.ch"

function Main()

   local o := Three():New()

   o:CheckIt()

return nil

CLASS One

   METHOD New() INLINE Self

   METHOD Test() INLINE QOut( "One" )

   METHOD CheckIt() INLINE ::Test()

ENDCLASS

CLASS Two FROM One

   METHOD Test() INLINE Super:Test()

   METHOD CheckIt() INLINE Super:CheckIt()

ENDCLASS

CLASS Three FROM Two

   METHOD Test() INLINE QOut( "Three" )

ENDCLASS
