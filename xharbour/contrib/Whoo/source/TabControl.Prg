// Augusto Infante
// Whoo.lib

#Include "windows.ch"
#include "hbclass.ch"
#Include 'debug.ch'
#Include "tabctrl.ch"
#Include "wintypes.ch"
#Include "cstruct.ch"
#Include "winGdi.ch"

// move that structure to WinStruc.ch

typedef struct {;
    HWND hwndFrom; 
    UINT idFrom; 
    UINT code; 
} NMHDR

*-----------------------------------------------------------------------------*

CLASS TTabControl FROM TControl

   DATA Tabs AS  ARRAY HIDDEN
   DATA Dlgs AS  ARRAY HIDDEN
   DATA Procs AS ARRAY HIDDEN
   DATA nCurSel
   DATA Cargo

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
   METHOD OnNotify( nwParam, nlParam ) INLINE ::TabNotify( nwParam, nlParam )
   METHOD TabNotify()
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
   ::width   := nW
   ::height  := nH
   
   ::lRegister := .F.
   ::lControl  := .F.
   ::WndProc   := 'FormProc'
   
   ::Tabs    := {}
   ::Dlgs    := {}
   ::Procs   := {}
   ::nCurSel := IF(nSel==NIL,1,nSel)

   ::Msgs    := {WM_NOTIFY,WM_CTLCOLORDLG}
   
RETURN( super:new( oParent ))

*-----------------------------------------------------------------------------*

FUNCTION _TempPageProc(nMsg)
   IF nMsg==WM_CTLCOLORDLG
      return(GetStockObject(NULL_BRUSH))
   END

   RETURN(0)

*-----------------------------------------------------------------------------*

METHOD TabNotify( nwParam, nlParam ) CLASS TTabControl

   LOCAL tnhdr

   LOCAL n,nSel
   LOCAL lVisible
   LOCAL lEnabled
   LOCAL nLen

   nSel:=TabCtrl_GetCurSel( ::handle )+1
   IF ::nCursel <> nSel
      ShowWindow(::Tabs[::nCurSel], SW_HIDE)
      ::nCurSel:=nSel
      ShowWindow(::Tabs[::nCurSel], SW_SHOW)
   endif

   Return( super:OnNotify() )

*-----------------------------------------------------------------------------*

METHOD AddTab(cText,cRes,bProc,nImgPos) CLASS TTabControl
LOCAL hTab
   if (hTab:=TabCtrl_AddItem(::handle,cText,nImgPos)) > -1
      AADD(::Dlgs,cRes)
      AADD(::Tabs,NIL )
      AADD(::Procs,bProc)
   ENDIF

   RETURN(hTab)

*-----------------------------------------------------------------------------*

METHOD InsertTab(nPos,cText,cRes,bProc) CLASS TTabControl

   if TabCtrl_InsertItem(::handle,cText,nPos) > -1
      AINS(::Dlgs,nPos,cRes,.T.)
      aIns(::Tabs,nPos,NIL,.T.)
      AINS(::Procs,nPos,bProc,.T.)
      RETURN(.T.)
   ENDIF

   return(.F.)

*-----------------------------------------------------------------------------*

METHOD DeleteTab(nPos) CLASS TTabControl

   Local nCount:=LEN(::Tabs)
   if nPos > 0 .and. nPos <= nCount
      IF nPos <= ::nCurSel     // verify !!!!!
        ::nCurSel--
      ENDIF
      TabCtrl_DeleteItem(nPos-1)
      ADel(::Dlgs,nPos,.t.)
      if isWindow(::Tabs[nPos])
         DestroyWindow(::Tabs[nPos])
      endif
      ADel(::Tabs,nPos,.t.)
      ADEL(::Procs,nPos,.T.)
      return(.T.)
   Endif

   RETURN(.F.)

*-----------------------------------------------------------------------------*

METHOD Configure() CLASS TTabControl

   LOCAL aTab :=GetClientRect(::handle)
   local acRect:={0,0,0,0}
   LOCAL aTemp
   LOCAL aWnd:={}
   LOCAL hCtrl
   LOCAL i
   LOCAL aPt
   LOCAL bBlock
   view aTab
   IF LEN(::Tabs) > 0
      acRect:=TabCtrl_GetItemRect(::handle,0)
      FOR i:=1 TO LEN(::Tabs)-1
         aTemp:=TabCtrl_GetItemRect(::handle,i)
         acRect[1]:=MIN(acRect[1],aTemp[1])
         acRect[2]:=MIN(acRect[2],aTemp[2])
         acRect[3]:=MAX(acRect[3],aTemp[3])
         acRect[4]:=MAX(acRect[4],aTemp[4])
      NEXT
   ENDIF

   aPt:={acRect[1],acRect[2]}
   ClientToScreen(::handle   ,aPt)
   ScreenToClient(::Parent:handle,aPt)
   acRect[1]:=aPt[1]
   acRect[2]:=aPt[2]

   FOR i:=1 TO LEN(::Dlgs)
      IF ::Dlgs[i] != NIL .AND. EMPTY(::Tabs[i])

       //  bBlock:=IF(valtype(::Procs[i])== "B", ::Procs[i], {|| _TempPageProc()} )

         hCtrl:=CreatePage(::Dlgs[i],::Parent:handle,::Procs, i )
         ::Tabs[i]:=hCtrl
         MoveWindow(hCtrl,acRect[1]+4,acRect[2]+acRect[4]+4,aTab[3]-8,aTab[4]-acRect[2]-acRect[4]-1,.F.)
         IF i<>::nCurSel
            ShowWindow(hCtrl,SW_HIDE)
         endif
      endif
   next

   RETURN(self)

*-----------------------------------------------------------------------------*
Static Function CreatePage(acRes,hParent,aProcs, i)
   Local bBlock:=IF(valtype( aProcs[i])== "B", aProcs[i], {|nMsg| _TempPageProc(nMsg)} )
   RETURN CreateDialog( , acRes, hParent, bBlock )

*-----------------------------------------------------------------------------*
METHOD AdjustRect(lDisplay,aRect) CLASS TTabControl

   TabCtrl_AdjustRect(::handle,lDisplay,@aRect)

   RETURN(aRect)

*-----------------------------------------------------------------------------*
METHOD DeleteAllTabs() CLASS TTabControl

  Local lRet:=TabCtrl_DeleteAllItems(::handle)

  AEVAL(::Tabs,{|hWnd| IF(isWindow(hWnd),DestroyWindow(hWnd),)})
  ::Tabs:={}
  ::aDlg:={}
  ::Procs:={}
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
