#include "windows.ch"
#include "hbclass.ch"
#include "imglist.ch"
#include "debug.ch"
#include "TreeView.ch"


CLASS ObjTree FROM TForm
   VAR TreeRoot AS OBJECT
   METHOD New( oParent ) INLINE ::Caption := 'Object Tree',;
                                ::left    := 0,;
                                ::top     := 125,;
                                ::width   := 200,;
                                ::height  := 150,;
                                ::ExStyle := WS_EX_TOOLWINDOW ,;
                                super:new( oParent )
   METHOD OnCloseQuery() INLINE 0
   METHOD OnCreate()
   METHOD OnSize(n,x,y)  INLINE  ::Tree:Move(,,x,y,.t.),nil
ENDCLASS

METHOD OnCreate() CLASS ObjTree
   local o,hImg,hBmp

   hImg := ImageList_Create( 16, 16, ILC_COLORDDB+ILC_MASK )
   hBmp := LoadImage( hInstance(), "OBJTREE", IMAGE_BITMAP, 0, 0, LR_LOADTRANSPARENT )
   ImageList_AddMasked( hImg, hBmp, RGB( 0, 255, 255 ) )
   DeleteObject(hBmp)

   ::Add('tree', TreeObj():New( self, 100,  0,  0, 100, 100) )
   TVSetImageList(::Tree:handle, hImg, 0 )
RETURN(nil)

CLASS TreeObj FROM TTreeView
   METHOD New() CONSTRUCTOR
   METHOD Add()
ENDCLASS

METHOD New( oObj, nId, nL, nT, nW, nH ) CLASS TreeObj
   ::Style := WS_CLIPSIBLINGS
   ::ExStyle := WS_EX_ACCEPTFILES
return( super:New( oObj, nId, nL, nT, nW, nH ) )

METHOD Add( cText, nImg ) CLASS TreeObj
   if empty( ::Parent:TreeRoot )
      ::Parent:TreeRoot:=super:Add( cText, nImg )
     else
      ::Parent:TreeRoot:Add( cText, nImg )
   endif
return(nil)
