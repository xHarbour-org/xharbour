/* $Id$ */

#include "hbclass.ch"

#include "colors.ch"
#include "vxh.ch"

#include "vxhdebug.ch"


CLASS XHDebugList FROM ListBox
  METHOD Init( oParent ) CONSTRUCTOR
ENDCLASS


METHOD Init( oParent ) CLASS XHDebugList
  ::Super:Init( oParent )

  ::HorzScroll   := .T.
  ::VertScroll   := .T.
  ::ClientEdge   := .F.
  ::StaticEdge   := .T.
  ::ClipChildren := .F.
  ::ClipSiblings := .F.
  ::ExtendedSel  := .F.
  ::HasStrings   := .T.
  ::Border       := .F.
RETURN Self
