#include "windows.ch"
#include "wingdi.ch"
#include "hbclass.ch"
#include "what32.ch"
#include "wintypes.ch"
#include "cstruct.ch"

GLOBAL oApp

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
                                                      ::InspTabs:Properties:ClientRect()[3],;
                                                      ::InspTabs:Properties:ClientRect()[4],.t.),;
                                 nil
ENDCLASS

//-------------------------------------------------------------------------------------------------

METHOD OnCreate() CLASS ObjInspect
  local oBrowse, oCol1, oCol2
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

  oBrowse:=whBrowse():Init( aProp )
  oBrowse:Create( ::InspTabs:Properties:handle,  0, 0,;
                                                 ::InspTabs:Properties:ClientRect()[3],;
                                                 ::InspTabs:Properties:ClientRect()[4],,WS_EX_CLIENTEDGE )
  oBrowse:wantHScroll:=.F.
  oCol1:=whColumn():Init( 'Property', {|oCol,oB,n| asString(oB:source[n,1]) } ,DT_LEFT,94)
  oCol2:=whColumn():INIT( 'Value'   , {|oCol,oB,n| asString(oB:source[n,2]) } ,DT_LEFT,70)
  oCol1:VertAlign   :=TA_CENTER
  oCol2:VertAlign   :=TA_CENTER
  oBrowse:addColumn(oCol1)
  oBrowse:addColumn(oCol2)
  oBrowse:HeadFont  :=GetMessageFont()
  oBrowse:Font      :=GetMessageFont()
  oBrowse:HeadHeight:=20
  oBrowse:bKillBlock:={|| DeleteObject(oBrowse:Font),DeleteObject(oBrowse:HeadFont)}
  oBrowse:configure()

  ::SetLink( 'browser', oBrowse )

return( super:OnCreate() )

//----------------------------------------------------------------------------------------------

CLASS ComboInsp FROM TComboBox
   METHOD DrawItem()
ENDCLASS

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
             exttextout( dis:hDC , dis:rcItem:Left + aRect[1]+2, dis:rcItem:Top , ;
                                 ETO_OPAQUE + ETO_CLIPPED, aRect, cText )
             cText:=""
             aRect[1]+=70
             loop
          endif
          cText+=substr(itemTxt,n,1)
      next
   endif
   if And( dis:itemState, ODS_FOCUS ) > 0 .OR. And( dis:itemAction, ODA_FOCUS ) > 0
      drawfocusrect( dis:hDC  , aclip )
   endif
return(1)

FUNCTION ZeroInit(ostr)
  ostr:buffer(replicate(chr( 0 ),ostr : sizeof()))
  RETURN(NIL)
