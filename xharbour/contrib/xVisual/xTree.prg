#include "windows.ch"
#include "hbclass.ch"
#include "imglist.ch"


CLASS ObjTree FROM TForm
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
   ::Add('tree', TTreeView():New( self, 100,  0,  0, 100, 100) )
   TVSetImageList(::Tree:handle, hImg, 0 )

   o:=::tree:Add('testing1',0)
      o:=o:Add('testing tree 1',1)
         o:=o:Add('testing tree sub 1',2)
   ::tree:Add('testing2',1)
   ::tree:Add('testing3',1)
   ::tree:Add('testing4',1)
RETURN(nil)
