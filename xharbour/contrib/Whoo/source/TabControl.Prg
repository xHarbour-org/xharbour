// Augusto Infante
// Whoo.lib

#Include "windows.ch"
#include "hbclass.ch"
#Include 'debug.ch'
#Include "tabctrl.ch"
#Include "wintypes.ch"
#Include "cstruct.ch"
#Include "winGdi.ch"
#Include "what32.ch"

// move that structure to WinStruc.ch

typedef struct {;
    HWND hwndFrom; 
    UINT idFrom; 
    UINT code; 
} NMHDR

*-----------------------------------------------------------------------------*

CLASS TTabControl FROM TControl

   DATA Controls AS  ARRAY
   DATA nCurSel
   DATA Cargo
   DATA ntProc
   DATA SendTo AS OBJECT
   METHOD New() CONSTRUCTOR
   METHOD AddTab()
   METHOD InsertTab()
   METHOD DeleteTab()
   METHOD Configure()
   METHOD AdjustRect()
   METHOD DeleteAllTabs()
   METHOD DeselectAll()
   METHOD GetCurFocus()
   METHOD GetCurSel()
   METHOD GetExtendedStyle()
   METHOD GetImageList()
   METHOD GetItem()
   METHOD GetItemCount()
   METHOD GetItemRect()
   METHOD GetRowCount()
   METHOD GetToolTips()
   METHOD GetUnicodeFormat()
   METHOD HighlightItem()
   METHOD HitTest()
   METHOD RemoveImage()
   METHOD SetCurFocus()
   METHOD SetCurSel()
   METHOD SetExtendedStyle()
   METHOD SetImageList()
   METHOD SetItem()
   METHOD SetItemExtra()
   METHOD SetItemSize()
   METHOD SetMinTabWidth()
   METHOD SetPadding()
   METHOD SetToolTips()
   METHOD SetUnicodeFormat()
   METHOD TabNotify()
   METHOD OnCreate()
   METHOD OnSize()
   METHOD GetObj()
ENDCLASS

*-----------------------------------------------------------------------------*

METHOD New( oParent, nId, nL, nT, nW, nH, nSel ) CLASS TTabControl

   ::Id      := nId
   ::ExStyle := 0
   ::Style   := WS_CHILD + WS_VISIBLE + WS_TABSTOP
   
   ::name    := WC_TABCONTROL
   ::Caption := ""
   
   ::Left    := nL
   ::Top     := nT
   ::width   := IFNIL(nW,289,nW)
   ::height  := IFNIL(nH,193,nH)
   
   ::lRegister := .F.
   ::lControl  := .F.
   ::WndProc   := 'FormProc'
   
   ::Controls:= {}
   ::nCurSel := IF(nSel==NIL,1,nSel)

   ::Msgs    := {WM_NOTIFY,WM_SIZE}
RETURN( super:new( oParent ))

*-----------------------------------------------------------------------------*

METHOD OnCreate() CLASS TTabControl
   ::ntProc := SetProcedure( ::Parent:handle,;
               {|hWnd,nMsg,nwParam,nlParam|::TabNotify( nMsg,nwParam, nlParam )},{WM_NOTIFY} )
return( super:OnCreate() )

*-----------------------------------------------------------------------------*

METHOD TabNotify( nMsg,nwParam, nlParam ) CLASS TTabControl
   LOCAL nSel
   nSel:=TabCtrl_GetCurSel( ::handle )+1
   IF ::nCursel <> nSel
      ShowWindow(::Controls[::nCurSel]:handle, SW_HIDE)
      ::nCurSel:=nSel
      ShowWindow(::Controls[::nCurSel]:handle, SW_SHOW)
   endif
   return(CallWindowProc( ::ntProc, ::parent:handle,nMsg,nwParam, nlParam ))

*-----------------------------------------------------------------------------*

METHOD AddTab( cName, oPage, nImgPos ) CLASS TTabControl
   LOCAL hTab, cRes
   if VALTYPE( oPage ) == "C"
      cRes := oPage
      oPage:= NIL
   endif
   DEFAULT oPage TO TabPage():New( self, cName )
   if cRes != NIL
      oPage:resname := cRes
      oPage:ImageIndex := nImgPos
   endif
   oPage:propname := cName
   if (hTab:=TabCtrl_AddItem(::handle,oPage:Caption,oPage:ImageIndex)) > -1
      AADD(::Controls,oPage )
      __objAddData( self, cName )
      __ObjSetValueList( self, { { cName, oPage } } )
   ENDIF
   RETURN(hTab)

*-----------------------------------------------------------------------------*

METHOD InsertTab(nPos,cText,cRes,bProc) CLASS TTabControl

   if TabCtrl_InsertItem(::handle,cText,nPos) > -1
      aIns(::Controls,nPos,NIL,.T.)
      RETURN(.T.)
   ENDIF

   return(.F.)

*-----------------------------------------------------------------------------*

METHOD DeleteTab(nPos) CLASS TTabControl

   Local nCount:=LEN(::Controls)
   if nPos > 0 .and. nPos <= nCount
      IF nPos <= ::nCurSel     // verify !!!!!
        ::nCurSel--
      ENDIF
      TabCtrl_DeleteItem(nPos-1)
      if isWindow(::Controls[nPos]:handle)
         DestroyWindow(::Controls[nPos]:handle)
      endif
      ADel(::Controls,nPos,.t.)
      return(.T.)
   Endif

   RETURN(.F.)

*-----------------------------------------------------------------------------*

METHOD Configure(lCreate) CLASS TTabControl

   LOCAL aTab :=GetClientRect(::handle)
   local acRect:={0,0,0,0}
   LOCAL aTemp
   LOCAL aWnd:={}
   LOCAL hCtrl
   LOCAL i
   LOCAL aPt
   LOCAL bBlock
   DEFAULT lCreate TO .T.
   IF LEN(::Controls) > 0
      acRect:=TabCtrl_GetItemRect(::handle,0)
      FOR i:=1 TO LEN(::Controls)-1
         aTemp:=TabCtrl_GetItemRect(::handle,i)
         acRect[1]:=MIN(acRect[1],aTemp[1])
         acRect[2]:=MIN(acRect[2],aTemp[2])
         acRect[3]:=MAX(acRect[3],aTemp[3])
         acRect[4]:=MAX(acRect[4],aTemp[4])
      NEXT
   ENDIF

   aPt:={acRect[1],acRect[2]}
   acRect[1]:=aPt[1]
   acRect[2]:=aPt[2]

   FOR i:=1 TO LEN(::Controls)
      IF ::Controls[i]!=NIL
         IF lCreate
            ::Controls[i]:Create()
         endif
         ::Controls[i]:Move( acRect[1],;
                         acRect[2]+acRect[4],;
                         aTab[3]-6,;
                         aTab[4]-acRect[2]-acRect[4]-4,;
                         .T.)
         if lCreate
            IF i<>::nCurSel
               ShowWindow(::Controls[i]:handle,SW_HIDE)
            endif
         endif
      endif
   next

   RETURN(self)

*-----------------------------------------------------------------------------*
METHOD AdjustRect(lDisplay,aRect) CLASS TTabControl

   TabCtrl_AdjustRect(::handle,lDisplay,@aRect)

   RETURN(aRect)

*-----------------------------------------------------------------------------*
METHOD DeleteAllTabs() CLASS TTabControl

  Local lRet:=TabCtrl_DeleteAllItems(::handle)

  AEVAL(::Controls,{|hWnd| IF(isWindow(hWnd),DestroyWindow(hWnd),)})
  ::Controls:={}
  ::aDlg:={}
  ::nCurSel:=0

  RETURN(lRet)

*-----------------------------------------------------------------------------*
METHOD DeselectAll(lExcludeFocus) CLASS TTabControl

   TabCtrl_DeselectAll(::handle,lExcludeFocus)

   RETURN(NIL)

*-----------------------------------------------------------------------------*
METHOD GetCurFocus() CLASS TTabControl

  RETURN TabCtrl_GetCurFocus(::handle )+1

*-----------------------------------------------------------------------------*
METHOD GetCurSel() CLASS TTabControl

   RETURN TabCtrl_GetCurSel(::handle)+1

*-----------------------------------------------------------------------------*
METHOD GetExtendedStyle() CLASS TTabControl

   RETURN TabCtrl_GetExtendedStyle(::handle)

*-----------------------------------------------------------------------------*
METHOD GetImageList() CLASS TTabControl

   RETURN NIL //TabCtrl_GetImageList(::handle)

*-----------------------------------------------------------------------------*
METHOD GetItem(nItem,ptrItem) CLASS TTabControl

   RETURN TabCtrl_GetItem(::handle,nItem-1,@ptrItem)

*-----------------------------------------------------------------------------*
METHOD GetItemCount() CLASS TTabControl

   RETURN TabCtrl_GetItemCount(::handle)

*-----------------------------------------------------------------------------*
METHOD GetItemRect(nItem) CLASS TTabControl

   RETURN TabCtrl_GetItemRect(::handle,nItem-1)

*-----------------------------------------------------------------------------*
METHOD GetRowCount() CLASS TTabControl

   RETURN TabCtrl_GetRowCount(::handle)

*-----------------------------------------------------------------------------*
METHOD GetToolTips() CLASS TTabControl

   RETURN TabCtrl_GetToolTips(::handle)

*-----------------------------------------------------------------------------*
METHOD GetUnicodeFormat() CLASS TTabControl

   RETURN TabCtrl_GetUnicodeFormat(::handle)

*-----------------------------------------------------------------------------*
METHOD HighlightItem(nItem,nHighlight) CLASS TTabControl

   RETURN TabCtrl_HighlightItem(::handle,nItem-1,nHighlight)

*-----------------------------------------------------------------------------*
METHOD HitTest(nPtrHitTestInfo) CLASS TTabControl

   RETURN TabCtrl_HitTest(::handle,nPtrHitTestInfo) + 1

*-----------------------------------------------------------------------------*
METHOD RemoveImage(nImageIndex) CLASS TTabControl

   RETURN TabCtrl_RemoveImage(::handle, nImageIndex-1)

*-----------------------------------------------------------------------------*
METHOD SetCurFocus(nItem) CLASS TTabControl

   TabCtrl_SetCurFocus(::handle, nItem-1)

   RETURN(NIL)

*-----------------------------------------------------------------------------*
METHOD SetCurSel(nItem) CLASS TTabControl

   RETURN TabCtrl_SetCurSel(::handle, nItem-1) + 1

*-----------------------------------------------------------------------------*
METHOD SetExtendedStyle(nExStyle) CLASS TTabControl

   RETURN TabCtrl_SetExtendedStyle(::handle,nExStyle)

*-----------------------------------------------------------------------------*
METHOD SetImageList(hImageList) CLASS TTabControl

   RETURN TabCtrl_SetImageList(::handle, hImageList)

*-----------------------------------------------------------------------------*
METHOD SetItem(nItem, cText) CLASS TTabControl

   RETURN TabCtrl_SetItem(::handle, nItem-1, cText )

*-----------------------------------------------------------------------------*
METHOD SetItemExtra(nBytes) CLASS TTabControl

   RETURN TabCtrl_SetItemExtra(::handle, nBytes)

*-----------------------------------------------------------------------------*
METHOD SetItemSize(x,y) CLASS TTabControl

   RETURN TabCtrl_SetItemSize(::handle, x, y )

*-----------------------------------------------------------------------------*
METHOD SetMinTabWidth(dx) CLASS TTabControl

   RETURN TabCtrl_SetMinTabWidth( ::handle, dx )

*-----------------------------------------------------------------------------*
METHOD SetPadding( cx, cy ) CLASS TTabControl

   TabCtrl_SetPadding( ::handle, cx, cy )

   RETURN(NIL)

*-----------------------------------------------------------------------------*
METHOD SetToolTips( hToolTips ) CLASS TTabControl

   TabCtrl_SetToolTips( ::handle, hToolTips )

   RETURN(NIL)

*-----------------------------------------------------------------------------*
METHOD SetUnicodeFormat( lUnicode ) CLASS TTabControl

   RETURN TabCtrl_SetUnicodeFormat( ::handle, lUnicode )

*-----------------------------------------------------------------------------*

METHOD OnSize( nwParam,x , y ) CLASS TTabControl
   local n, aRect
   for n:=1 to LEN( ::Controls )
       if ::Controls[n]!=nil
          ::Configure(.F.)
       endif
   next
return( super:OnSize() )

*-----------------------------------------------------------------------------*

METHOD GetObj( cName ) CLASS TTabControl
   local n:= ASCAN( ::Controls,{|o|o:propname==cName} )
   if n>0
      return( ::Controls[n] )
   endif
return(nil)

*-----------------------------------------------------------------------------*

CLASS TabPage FROM TPanel
   DATA ImageIndex
   METHOD New() CONSTRUCTOR
   METHOD OnCommand()
ENDCLASS

METHOD New( oParent, cCaption, cRes, nImgPos ) CLASS TabPage
   super:new( oParent )
   ::width   := 10
   ::height  := 10
   ::style   := WS_CHILD + WS_VISIBLE
   ::modal   := .F.
   ::resname := cRes
   ::Caption := cCaption
   ::ImageIndex := nImgPos
return( self )

METHOD OnCommand(nwParam,nlParam) CLASS TabPage
   IF ::Parent:SendTo != NIL
      RETURN(::Parent:SendTo:SendMessage( WM_COMMAND, nwParam, nlParam ))
   endif
return(0)

