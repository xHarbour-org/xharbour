#include "windows.ch"
#include "wingdi.ch"
#include "hbclass.ch"
#include "what32.ch"
#include "wintypes.ch"
#include "cstruct.ch"
#include "debug.ch"

GLOBAL EXTERNAL oApp


typedef struct {;
    UINT    mask;
    int     cxy;
    LPTSTR  pszText;
    HBITMAP hbm;
    int     cchTextMax;
    int     fmt;
    LPARAM  lParam;
    int     iImage;
    int     iOrder;
} HDITEM, FAR * LPHDITEM

CLASS ObjInspect FROM TForm

   VAR Browser  AS OBJECT
   VAR Objects  AS ARRAY INIT {}
   VAR CurObject AS OBJECT
   METHOD New( oParent ) INLINE ::Caption := 'Object Inspector',;
                                ::left    := 0,;
                                ::top     := 275,;
                                ::width   := 200,;
                                ::height  := 297,;
                                ::ExStyle := WS_EX_TOOLWINDOW ,;
                                super:new( oParent )
   // disallow window from being closed
   METHOD OnCloseQuery() INLINE 0
   METHOD OnCreate()
   METHOD OnSize(n,x,y)  INLINE  ::GetObj("InspCombo"):Move(,,x,21,.t.),;
                                 ::GetObj("InspTabs"):Move(,25,x,y-25,.t.),;
                                 MoveWindow(::browser:hWnd, 0, 0,;
                                                      ::InspTabs:Properties:ClientRect()[3]+1,;
                                                      ::InspTabs:Properties:ClientRect()[4],.t.),;
                                 nil
   METHOD SetBrowserData()
   METHOD SaveVar()
ENDCLASS

//-------------------------------------------------------------------------------------------------
METHOD SetBrowserData( oObj ) CLASS ObjInspect

   ::CurObject := oObj

   ::Browser:source := __ObjGetValueList( oObj, NIL, HB_OO_CLSTP_EXPORTED )

   aSort( ::Browser:Source,,, {|x,y| x[1] < y[1] } )
   aEval( ::Browser:Source, {|a|a[1] := Proper( a[1] )} )

   ::Browser:RefreshAll()

   IF oObj:ClassName == "TFORMEDIT"
      oObj:XFMRoot()
   ELSE
      oObj:Parent:XFMRoot()
   ENDIF

RETURN Self

METHOD OnCreate() CLASS ObjInspect
  local oCol1, oCol2
  local aProp := {{"",""}}
  local aRect := ::ClientRect()
  local oCombo:= ComboInsp():New(  self, 100, 0, 0, aRect[3], 100 )
  oCombo:Style:= WS_CHILD + WS_VISIBLE + WS_BORDER + WS_TABSTOP + CBS_DROPDOWNLIST + WS_VSCROLL + CBS_HASSTRINGS + CBS_OWNERDRAWFIXED

  ::Add( 'InspCombo', oCombo )
  ::InspCombo:SetItemHeight( -1, 15 )

  ::Add( 'InspTabs', TTabControl():New( self, 101,  0,  25, aRect[3], aRect[4]-25) )
  ::InspTabs:AddTab( "Properties")
  ::InspTabs:AddTab( "Events", TabPage():New( ::InspTabs, "Events") )
  ::InspTabs:Configure()

  ::Browser:=TBrowser():Init( aProp )
  ::Browser:Create( ::InspTabs:Properties:handle,  0, 0,;
                                                 ::InspTabs:Properties:ClientRect()[3],;
                                                 ::InspTabs:Properties:ClientRect()[4],;
                                                 WS_CHILD+WS_VISIBLE+WS_VSCROLL,;
                                                 WS_EX_CLIENTEDGE )
  ::Browser:wantHScroll:=.F.
  oCol1:=whColumn():Init( 'Property', {|oCol,oB,n| asString(oB:source[n,1]) } ,DT_LEFT,85)
  oCol1:VertAlign   :=TA_CENTER
  oCol1:fgColor:= GetSysColor(COLOR_WINDOWTEXT)
  oCol1:Style  := 0

  oCol2:=whColumn():INIT( 'Value'   , {|oCol,oB,n| asString(oB:source[n,2]) } ,DT_LEFT,81)
  oCol2:VertAlign  :=TA_CENTER
  oCol2:fgColor    := RGB(0,0,128)
  oCol2:bSaveBlock := {|cText|::SaveVar(cText)}

  ::Browser:addColumn(oCol1)
  ::Browser:addColumn(oCol2)
  ::Browser:HeadFont      :=GetMessageFont()
  ::Browser:Font          :=GetMessageFont()
  ::Browser:HeadHeight    :=0
  ::Browser:BgColor       := GetSysColor(COLOR_BTNFACE)
  ::Browser:HiliteNoFocus := GetSysColor(COLOR_BTNFACE)

  ::Browser:bKillBlock:={|| DeleteObject(::Browser:Font),DeleteObject(::Browser:HeadFont)}
  ::Browser:configure()

return( super:OnCreate() )

//----------------------------------------------------------------------------------------------

METHOD SaveVar(cText) CLASS ObjInspect
   local cType, cVar
   cVar := ::Browser:source[::Browser:RecPos][1]
   cType:= valtype( __objSendMsg( ::CurObject, cVar ) )
   view ::Browser:RecPos
   do case
      case cType == 'N'
           cText:=VAL(cText)
      case cType == 'U'
           cText:=NIL
      case cType == 'L'
           cText:= IIF( cText == ".T.",.T.,.F.)
   endcase
   __objSendMsg( ::CurObject, "_"+cVar, cText )
   ::Browser:source[::Browser:RecPos][2]:= cText
   ::Browser:RefreshCurrent()
   ::CurObject:Update()
   ::CurObject:SetFocus()
   SetFocus( ::Browser:hWnd)
return(self)

//----------------------------------------------------------------------------------------------

CLASS ComboInsp FROM TComboBox
   METHOD DrawItem()
   METHOD OnClick()
   METHOD AddString()
   METHOD SetCurSel()
ENDCLASS

//---------------------------------------------------------------------------------

METHOD OnClick(nwParam,nlParam) CLASS ComboInsp
   local oObj
   if hiword(nwParam)==CBN_SELCHANGE
      oObj := ::Parent:Objects[::GetCurSel()+1]
      ::Parent:SetBrowserData( oObj )
   endif
return(0)

//---------------------------------------------------------------------------------

METHOD AddString(cText,oObj) CLASS ComboInsp
   aadd(::Parent:Objects,oObj)
return(super:AddString(cText))

//---------------------------------------------------------------------------------

METHOD SetCurSel(n) CLASS ComboInsp
   ::Parent:SetBrowserData( ::Parent:Objects[n+1] )
return(super:SetCurSel(n))

//---------------------------------------------------------------------------------

METHOD DrawItem( dis ) CLASS ComboInsp
   LOCAL lselected
   LOCAL aclip, aRect
   LOCAL itemTxt, cText
   LOCAL nLen, n
   lselected := And( dis:itemState, ODS_SELECTED ) > 0
   aclip := { dis:rcItem:Left , dis:rcItem:Top  , ;
              dis:rcItem:Right  , dis:rcItem:Bottom  }
   IF And( dis:itemAction, ODA_DRAWENTIRE ) > 0 .OR. And( dis:itemAction, ODA_SELECT ) > 0
      SetTextColor( dis:hDC  , GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_WINDOWTEXT )) )
      SetBkColor( dis:hDC  , GetSysColor(IF( lselected,COLOR_HIGHLIGHT,COLOR_WINDOW )) )
      nLen := SendMessage( dis:hwndItem, CB_GETLBTEXTLEN, dis:itemID, 0 )
      itemTxt := Space( nLen + 1 )
      SendMessage( dis:hwndItem, CB_GETLBTEXT, dis:itemID, @itemTxt )
      itemTxt:=left(itemTxt,nLen)
      cText := ""
      aRect := ACLONE(aClip)
      for n:=1 to nLen+1
          if substr(itemTxt,n,1)==chr(9).or.n==nLen+1
             if n==nLen+1
                SetTextColor( dis:hDC, GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_BTNSHADOW )) )
             endif
             exttextout( dis:hDC , dis:rcItem:Left + aRect[1]+2, dis:rcItem:Top , ;
                                 ETO_OPAQUE + ETO_CLIPPED, aRect, cText )
             cText:=""
             aRect[1]+=80
             loop
          endif
          cText+=substr(itemTxt,n,1)
      next
   endif
   if And( dis:itemState, ODS_FOCUS ) > 0 .OR. And( dis:itemAction, ODA_FOCUS ) > 0
      drawfocusrect( dis:hDC  , aclip )
   endif
return(1)

//---------------------------------------------------------------------------------

CLASS TBrowser FROM whBrowse
   METHOD New() CONSTRUCTOR
ENDCLASS

//---------------------------------------------------------------------------------

METHOD New( oSource ) CLASS TBrowser
   super:Init( oSource )
return(self)

//---------------------------------------------------------------------------------

FUNCTION ZeroInit(ostr)
  ostr:buffer(replicate(chr( 0 ),ostr : sizeof()))
  RETURN(NIL)

//---------------------------------------------------------------------------------
