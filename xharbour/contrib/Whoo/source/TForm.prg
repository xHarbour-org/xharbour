// Augusto Infante
// Whoo.lib

#include "hbclass.ch"
#include "windows.ch"
#include "debug.ch"
#include "what32.ch"

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
   METHOD GetObj()
   METHOD SetLink()
ENDCLASS

*-----------------------------------------------------------------------------*

METHOD New( oParent ) CLASS TForm
   
   ::WndProc   := IFNIL(::WndProc,'FormProc',::WndProc)
   ::Msgs      := IFNIL(::Msgs,-1,::Msgs)
   ::FrameWnd  := .F.
   ::Style     := IFNIL(::Style,WS_OVERLAPPEDWINDOW,::Style)
   ::FormType  := IFNIL(::FormType,RCF_WINDOW,::FormType)
   ::lRegister := IFNIL(::lRegister,.T.,::lRegister)
   ::lControl  := .F.
   ::ExStyle   := IFNIL(::ExStyle,0,::ExStyle)
   
   ::PropList  := { { 'Name',   ::Name    },;
                    { 'Caption',::Caption },;
                    { 'Left',   ::Left    },;
                    { 'Top',    ::Top     },;
                    { 'Width',  ::width   },;
                    { 'Height', ::height  },;
                    { 'Style',  ::Left    } }

   RETURN( super:New( oParent ) )

*-----------------------------------------------------------------------------*

METHOD Add( cName, oObj, lCreate ) CLASS TForm
   DEFAULT lCreate TO .T.
   oObj:propname := cName
   __objAddData( self, cName )
   __ObjSetValueList( self, { { cName, oObj } } )
   IF lCreate
      oObj:Create()
   endif
   AADD( ::Controls, oObj )
   RETURN( oObj )

*-----------------------------------------------------------------------------*

METHOD Del( cName ) CLASS TForm

   local n
   if (n := aScan( ::Controls, {|o| lower(o:name) == lower(cName)} ) ) > 0
      ::Controls[n]:Delete()
   endif
   RETURN( .t. )

*-----------------------------------------------------------------------------*

METHOD ChildFromHandle( hHandle ) CLASS TForm

   local n
   if (n := aScan( ::Controls, {|o| o:handle == hHandle} ) ) > 0
      return( ::Controls[n] )
   endif
   RETURN(nil)

*-----------------------------------------------------------------------------*

METHOD ChildFromId( nId ) CLASS TForm

   local n
   if (n := aScan( ::Controls, {|o| o:id == nId} ) ) > 0
      return( ::Controls[n] )
   endif
   return(nil)

*-----------------------------------------------------------------------------*

METHOD GetObj( cName ) CLASS TForm
   local n:= ASCAN( ::Controls,{|o|o:propname==cName} )
   if n>0
      return( ::Controls[n] )
   endif
return(nil)

*-----------------------------------------------------------------------------*

METHOD SetLink( cName, oObj ) CLASS TForm
   __objAddData( self, cName )
   __ObjSetValueList( self, { { cName, oObj } } )
return( oObj )
