
// WHOO.LIB

#include "hbclass.ch"
#include "windows.ch"
#include "debug.ch"

#Define RCF_DIALOG     0
#Define RCF_WINDOW     1
#Define RCF_MDIFRAME   2
#Define RCF_MDICHILD   4

*-----------------------------------------------------------------------------*

CLASS TForm FROM TWindow
   
   METHOD New()
   METHOD Add()
   METHOD Del()
   METHOD ChildFromHandle( hHandle )
   METHOD ChildFromId( hHandle )

ENDCLASS

*-----------------------------------------------------------------------------*

METHOD New( oParent ) CLASS TForm

   ::WndProc   := 'FormProc'
   ::Msgs      := -1
   ::FrameWnd  := .F.
   ::Style     := WS_OVERLAPPEDWINDOW
   ::FormType  := RCF_WINDOW
   ::lRegister := .T.
   ::lControl  := .F.
   ::ExStyle   := 0

   RETURN( super:New( oParent ) )

*-----------------------------------------------------------------------------*

METHOD Add( cName, oObj ) CLASS TForm

   __objAddData( self, cName )
   __ObjSetValueList( self, { { cName, oObj } } )
   oObj:Create()

   RETURN( oObj )

*-----------------------------------------------------------------------------*

METHOD Del( cName ) CLASS TForm

   local n
   local aList := __objGetValueList( self )

   if (n := aScan( aList, {|a| valtype( a[2] )=='O' .and.a[1] == UPPER(cName)} ) ) > 0
      aList[n][2]:Delete()
   endif
   
   RETURN( .t. )

*-----------------------------------------------------------------------------*

METHOD ChildFromHandle( hHandle ) CLASS TForm

   local n
   local aList := __objGetValueList( self )

   if (n := aScan( aList, {|a|valtype( a[2] )=='O' .and. a[2]:handle == hHandle} ) ) > 0
      return( aList[n][2] )
   endif

   RETURN(nil)

*-----------------------------------------------------------------------------*

METHOD ChildFromId( nId ) CLASS TForm

   local n
   local aList := __objGetValueList( self )

   if (n := aScan( aList, {|a|valtype( a[2] )=='O' .and. a[2]:Id == nId} ) ) > 0
      return( aList[n][2] )
   endif

   return(nil)

*-----------------------------------------------------------------------------*