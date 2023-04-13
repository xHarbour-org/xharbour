/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id$

   TreeList - A flexible list of items or tree of items.

   XWT TreeList objects are capable to display a list, a
   table or a tree of items. A TreeList of one column
   is a list.

   The "xContent" member variable can be an array, or a
   TreeItem object. If the treelist has a list (only one
   column) , each element of xContent array must be:

   1) An xHarbour simple variable (including field/memvar);
      in this case a suitable string representation to be
      displayed.

   2) A TreeItem object. In this case a subtree (with its own
      xContent) is derived from this node.

   3) An object (which is considered ALWAYS to be an XWT widget):
      In this case the widget is painted at this point of the
      hyerarcy

   Else, if the TreeList has a table (more than one column),
   each element of the xContent array must be itself an array, or
   a TreeItem object.

   Eache array element of the xContent array MIGHT have the same
   number of colum of the table. But:

   1) if the number of elements is lesser than the column count,
      the remaining column are considered "void", and they are
      not displayed, nor selectable.
   2) If the number of elements is greater than the column count, the
      exceding items ignored.

   The xIntestation of a TreeItem can be an xHarbour simple variable,
   an object (in which case it is considered to be an XWT widget), or
   if the TreeList is a table (has more than one column), it can be
   an array.

   If the TreeItem widget text is not empty, then the widget text is
   used as a label, and xIntestation is ignored.

*/


#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTTreeItem FROM XWTWidget
   DATA xIntestation
   DATA xContent

   METHOD New( xIntestation, xContent )
ENDCLASS


METHOD New( xIntestation, xContent )
   ::Super:New()
   ::xIntestation := xIntestation
   ::xContent := xContent
   ::nWidgetType := XWT_TYPE_TREEITEM
RETURN Self
