#Include "..\include\windows.ch"
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

CLASS TabControl

   DATA hParent
   DATA hTab AS  NUMERIC
   DATA Tabs AS  ARRAY HIDDEN
   DATA Dlgs AS  ARRAY HIDDEN
   DATA Procs AS ARRAY HIDDEN
   DATA nCurSel
   DATA nProc
   DATA nId
   DATA Cargo

   METHOD New() CONSTRUCTOR
   METHOD TabProc()
   METHOD Add()
   METHOD Insert()
   METHOD Delete()
   METHOD Configure()
   METHOD AdjustRect()
   METHOD DeleteAll()
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
   METHOD TabSubClass()
   METHOD OnInit() VIRTUAL
ENDCLASS

*-----------------------------------------------------------------------------*

METHOD New( hDlg, nL, nT, nW, nH, nStyle, nSel,nId ) CLASS TabControl
   ::hParent:=hDlg
   ::Tabs:={}
   ::Dlgs:={}
   ::Procs:={}
   ::nId:=nId
   ::nCurSel:=IF(nSel==NIL,1,nSel)
   ::hTab:=TabCtrl_Create( hDlg, nL, nT, nW, nH, nStyle,nId)
   ::nProc:=SetProcedure( hDlg, {|hDlg,nMsg,nwParam,nlParam|;
            ::TabProc(hDlg,nMsg,nwParam,nlParam)} , {WM_NOTIFY} )
RETURN Self

*-----------------------------------------------------------------------------*

FUNCTION _TempPageProc(nMsg)
   IF nMsg==WM_CTLCOLORDLG
      return(GetStockObject(NULL_BRUSH))
   END

   RETURN(0)

*-----------------------------------------------------------------------------*

METHOD TabProc(hDlg, nMsg, nwParam, nlParam) CLASS TabControl

   LOCAL tnhdr

   LOCAL n,nSel
   LOCAL lVisible
   LOCAL lEnabled
   LOCAL nLen
   IF nMsg==WM_NOTIFY

    //  tnhdr IS NMHDR
    //  tnhdr:Buffer( peek(nlParam,tnhdr:sizeof() ) )

      IF  .T. // tnhdr:code==TCN_SELCHANGE

        nSel:=TabCtrl_GetCurSel( ::hTab )+1
        IF ::nCursel <> nSel
           ShowWindow(::Tabs[::nCurSel], SW_HIDE)
           ::nCurSel:=nSel
           ShowWindow(::Tabs[::nCurSel], SW_SHOW)
           /*
           IF ::nCurSel > 0

              //nLen:=len(::Tabs[::nCurSel])
              nLen:=if(EMPTY(::Tabs[::nCurSel]),0,len(::Tabs[::nCurSel]))
              FOR n:=1 TO nLen
                 ::Tabs[::nCurSel,n,2]:=isWindowEnabled(::Tabs[::nCurSel,n,1])
                 ::Tabs[::nCurSel,n,3]:=isWindowVisible(::Tabs[::nCurSel,n,1])
                 ShowWindow(::Tabs[::nCurSel,n,1],SW_HIDE)
                 EnableWindow(::Tabs[::nCurSel,n,1],.F.)
              NEXT
           ENDIF

           ::nCurSel:=nSel
           nLen:=if(EMPTY(::Tabs[::nCurSel]),0,len(::Tabs[::nCurSel]))

           FOR n:=1 TO nLen
              IF ::Tabs[::nCurSel,n,2]
                 EnableWindow(::Tabs[::nCurSel,n,1],.T.)
              ENDIF
              IF ::Tabs[::nCurSel,n,3]
                 ShowWindow(::Tabs[::nCurSel,n,1],SW_SHOW)
              ENDIF
           NEXT
           */

        ENDIF

      ENDIF

   ENDIF

   Return( CallWindowProc(::nProc, hDlg, nMsg, nwParam, nlParam) )

*-----------------------------------------------------------------------------*

METHOD Add(cText,cRes,bProc,nImgPos) CLASS TabControl
LOCAL hTab
   if (hTab:=TabCtrl_AddItem(::hTab,cText,nImgPos)) > -1
      AADD(::Dlgs,cRes)
      AADD(::Tabs,NIL )
      AADD(::Procs,bProc)
   ENDIF

   RETURN(hTab)

*-----------------------------------------------------------------------------*

METHOD Insert(nPos,cText,cRes,bProc) CLASS TabControl

   if TabCtrl_InsertItem(::hTab,cText,nPos) > -1
      AINS(::Dlgs,nPos,cRes,.T.)
      aIns(::Tabs,nPos,NIL,.T.)
      AINS(::Procs,nPos,bProc,.T.)
      RETURN(.T.)
   ENDIF

   return(.F.)

*-----------------------------------------------------------------------------*

METHOD Delete(nPos) CLASS TabControl

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

METHOD Configure() CLASS TabControl

   LOCAL aTab :=GetClientRect(::hTab)
   local acRect:={0,0,0,0}
   LOCAL aTemp
   LOCAL aWnd:={}
   LOCAL hCtrl
   LOCAL i
   LOCAL aPt
   LOCAL bBlock
   view aTab
   IF LEN(::Tabs) > 0
      acRect:=TabCtrl_GetItemRect(::hTab,0)
      FOR i:=1 TO LEN(::Tabs)-1
         aTemp:=TabCtrl_GetItemRect(::hTab,i)
         acRect[1]:=MIN(acRect[1],aTemp[1])
         acRect[2]:=MIN(acRect[2],aTemp[2])
         acRect[3]:=MAX(acRect[3],aTemp[3])
         acRect[4]:=MAX(acRect[4],aTemp[4])
      NEXT
   ENDIF

   aPt:={acRect[1],acRect[2]}
   ClientToScreen(::hTab   ,aPt)
   ScreenToClient(::hParent,aPt)
   acRect[1]:=aPt[1]
   acRect[2]:=aPt[2]

   FOR i:=1 TO LEN(::Dlgs)
      IF ::Dlgs[i] != NIL .AND. EMPTY(::Tabs[i])

       //  bBlock:=IF(valtype(::Procs[i])== "B", ::Procs[i], {|| _TempPageProc()} )
         hCtrl:=CreatePage(::Dlgs[i],::hParent,::Procs, i )
         ::Tabs[i]:=hCtrl
         MoveWindow(hCtrl,acRect[1]+4,acRect[2]+acRect[4]+4,aTab[3]-8,aTab[4]-acRect[2]-acRect[4]-1,.F.)
         IF i<>::nCurSel
            ShowWindow(hCtrl,SW_HIDE)
         ENDIF
      ENDIF
   NEXT

   RETURN(self)

*-----------------------------------------------------------------------------*
Static Function CreatePage(acRes,hParent,aProcs, i)
   Local bBlock:=IF(valtype( aProcs[i])== "B", aProcs[i], {|nMsg| _TempPageProc(nMsg)} )
   RETURN CreateDialog( , acRes, hParent, bBlock )

*-----------------------------------------------------------------------------*
METHOD AdjustRect(lDisplay,aRect) CLASS TabControl

   TabCtrl_AdjustRect(::hTab,lDisplay,@aRect)

   RETURN(aRect)

*-----------------------------------------------------------------------------*
METHOD DeleteAll() CLASS TabControl

  Local lRet:=TabCtrl_DeleteAllItems(::hTab)

  AEVAL(::Tabs,{|hWnd| IF(isWindow(hWnd),DestroyWindow(hWnd),)})
  ::Tabs:={}
  ::aDlg:={}
  ::Procs:={}
  ::nCurSel:=0

  RETURN(lRet)

*-----------------------------------------------------------------------------*
METHOD DeselectAll(lExcludeFocus) CLASS TabControl

   TabCtrl_DeselectAll(::hTab,lExcludeFocus)

   RETURN(NIL)

*-----------------------------------------------------------------------------*
METHOD GetCurFocus() CLASS TabControl

  RETURN TabCtrl_GetCurFocus(::hTab )+1

*-----------------------------------------------------------------------------*
METHOD GetCurSel() CLASS TabControl

   RETURN TabCtrl_GetCurSel(::hTab)+1

*-----------------------------------------------------------------------------*
METHOD GetExtendedStyle() CLASS TabControl

   RETURN TabCtrl_GetExtendedStyle(::hTab)

*-----------------------------------------------------------------------------*
METHOD GetImageList() CLASS TabControl

   RETURN NIL //TabCtrl_GetImageList(::hTab)

*-----------------------------------------------------------------------------*
METHOD GetItem(nItem,ptrItem) CLASS TabControl

   RETURN TabCtrl_GetItem(::hTab,nItem-1,@ptrItem)

*-----------------------------------------------------------------------------*
METHOD GetItemCount() CLASS TabControl

   RETURN TabCtrl_GetItemCount(::hTab)

*-----------------------------------------------------------------------------*
METHOD GetItemRect(nItem) CLASS TabControl

   RETURN TabCtrl_GetItemRect(::hTab,nItem-1)

*-----------------------------------------------------------------------------*
METHOD GetRowCount() CLASS TabControl

   RETURN TabCtrl_GetRowCount(::hTab)

*-----------------------------------------------------------------------------*
METHOD GetToolTips() CLASS TabControl

   RETURN TabCtrl_GetToolTips(::hTab)

*-----------------------------------------------------------------------------*
METHOD GetUnicodeFormat() CLASS TabControl

   RETURN TabCtrl_GetUnicodeFormat(::hTab)

*-----------------------------------------------------------------------------*
METHOD HighlightItem(nItem,nHighlight) CLASS TabControl

   RETURN TabCtrl_HighlightItem(::hTab,nItem-1,nHighlight)

*-----------------------------------------------------------------------------*
METHOD HitTest(nPtrHitTestInfo) CLASS TabControl

   RETURN TabCtrl_HitTest(::hTab,nPtrHitTestInfo) + 1

*-----------------------------------------------------------------------------*
METHOD RemoveImage(nImageIndex) CLASS TabControl

   RETURN TabCtrl_RemoveImage(::hTab, nImageIndex-1)

*-----------------------------------------------------------------------------*
METHOD SetCurFocus(nItem) CLASS TabControl

   TabCtrl_SetCurFocus(::hTab, nItem-1)

   RETURN(NIL)

*-----------------------------------------------------------------------------*
METHOD SetCurSel(nItem) CLASS TabControl

   RETURN TabCtrl_SetCurSel(::hTab, nItem-1) + 1

*-----------------------------------------------------------------------------*
METHOD SetExtendedStyle(nExStyle) CLASS TabControl

   RETURN TabCtrl_SetExtendedStyle(::hTab,nExStyle)

*-----------------------------------------------------------------------------*
METHOD SetImageList(hImageList) CLASS TabControl

   RETURN TabCtrl_SetImageList(::hTab, hImageList)

*-----------------------------------------------------------------------------*
METHOD SetItem(nItem, cText) CLASS TabControl

   RETURN TabCtrl_SetItem(::hTab, nItem-1, cText )

*-----------------------------------------------------------------------------*
METHOD SetItemExtra(nBytes) CLASS TabControl

   RETURN TabCtrl_SetItemExtra(::hTab, nBytes)

*-----------------------------------------------------------------------------*
METHOD SetItemSize(x,y) CLASS TabControl

   RETURN TabCtrl_SetItemSize(::hTab, x, y )

*-----------------------------------------------------------------------------*
METHOD SetMinTabWidth(dx) CLASS TabControl

   RETURN TabCtrl_SetMinTabWidth( ::hTab, dx )

*-----------------------------------------------------------------------------*
METHOD SetPadding( cx, cy ) CLASS TabControl

   TabCtrl_SetPadding( ::hTab, cx, cy )

   RETURN(NIL)

*-----------------------------------------------------------------------------*
METHOD SetToolTips( hToolTips ) CLASS TabControl

   TabCtrl_SetToolTips( ::hTab, hToolTips )

   RETURN(NIL)

*-----------------------------------------------------------------------------*
METHOD SetUnicodeFormat( lUnicode ) CLASS TabControl

   RETURN TabCtrl_SetUnicodeFormat( ::hTab, lUnicode )

*-----------------------------------------------------------------------------*




/*
*-----------------------------------------------------------------------------*

METHOD Configure()


   LOCAL aTab :=GetClientRect(::hTab)
   local acRect:={0,0,0,0}
   LOCAL aTemp
   LOCAL aWnd:={}
   LOCAL hCtrl
   LOCAL cRes
   LOCAL i

   IF LEN(::Tabs) > 0
      acRect:=TabCtrl_GetItemRect(::hTab,1)
      FOR i:=2 TO LEN(::Tabs)
         aTemp:=TabCtrl_GetItemRect(::hTab,i)
         acRect[1]:=MIN(acRect[1],aTemp[1])
         acRect[2]:=MIN(acRect[2],aTemp[2])
         acRect[3]:=MAX(acRect[3],aTemp[3])
         acRect[4]:=MAX(acRect[4],aTemp[4])
      NEXT
   ENDIF

   FOR i:=1 TO LEN(::Dlgs)
      aWnd:={}
      IF (cRes:=::Dlgs[i]) != NIL .AND. EMPTY(::Tabs[i])
         hCtrl :=CreateDialog(,cRes, ::hTab,{|| _TempPageProc()})
         MoveWindow(hCtrl,4,acRect[2]+acRect[4]+4,aTab[3]-8,aTab[4]-acRect[2]-acRect[4]-6,.F.)
         aWnd:=TransferChildren(::hParent,hCtrl,i==::nCurSel)
         DestroyWindow(hCtrl)
         ::Tabs[i]:=ACLONE(aWnd)
      ENDIF
   NEXT

   RETURN(self)

*-----------------------------------------------------------------------------*

Function TransferChildren(hDlg,hPage,lShow)

   LOCAL aChildren:={}
   LOCAL aRect
   LOCAL aPt
   LOCAL lVisible
   LOCAL lEnabled
   LOCAL cClass
   LOCAL cText
   LOCAL nStyle
   LOCAL nExStyle
   LOCAL nId
   LOCAL hNewWnd
   LOCAL hWnd:=GetWindow(hPage,GW_CHILD)

   DO WHILE !EMPTY(hWnd)
      aRect:=GetWindowRect(hWnd)
      aPt:={aRect[1],aRect[2]}
      ScreenToClient(hDlg,aPt)
      aRect[3]-=aRect[1]
      aRect[4]-=aRect[2]
      aRect[1]:=aPt[1]
      aRect[2]:=aPt[2]
      cClass:=GetClassName(hWnd)
      cText:=GetWindowText(hWnd)
      nStyle:=GetWindowLong(hWnd,GWL_STYLE)
      nExStyle:=GetWindowLong(hWnd,GWL_EXSTYLE)
      nId:=GetWindowLong(hWnd,GWL_ID)
      hNewWnd:=CreateWindowEx(nExStyle,cClass,cText,nStyle,;
               aRect[1],aRect[2],aRect[3],aRect[4],hDlg,nId)
      SendMEssage(hNewWnd,WM_SETFONT,SendMessage(hWnd,WM_GETFONT,0,0), 0 )
      lVisible:=AND(nStyle,WS_VISIBLE)==WS_VISIBLE
      lEnabled:=isWindowEnabled(hWnd)
      AADD(aChildren,{hNewWnd,lEnabled,lVisible})
      IF !lShow
         ShowWindow(hNewWnd,SW_HIDE)
         EnableWindow(hNewWnd,.F.)
      Endif
      hWnd:=GetWindow(hWnd,GW_HWNDNEXT)
   ENDDO

   RETURN(aChildren)

*-----------------------------------------------------------------------------*

*/

METHOD TabSubClass(hDlg,nMsg,nwParam,nlParam)
DO CASE
   CASE nMsg==WM_INITDIALOG
        ::OnInit(hDlg)
END        
RETURN(self)
