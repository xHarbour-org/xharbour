/*
 * $Id: xInspect.prg,v 1.36 2002/10/23 06:31:58 what32 Exp $
 */

/*
 * xHarbour Project source code:
 *
 * xIDE Object Inspector
 *
 * Copyright 2002 Augusto Infante [systems@quesoro.com] Andy Wos [andrwos@aust1.net] Ron Pinkas [ron@ronpinkas.com]
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 */

#include "windows.ch"
#include "wingdi.ch"
#include "hbclass.ch"
#include "what32.ch"
#include "wintypes.ch"
#include "cstruct.ch"
#include "debug.ch"
#include "accel.ch"

GLOBAL EXTERNAL oApp

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
                                 ::browser:width :=::InspTabs:Properties:ClientRect()[3],;
                                 ::browser:height:=::InspTabs:Properties:ClientRect()[4],;
                                 nil
   METHOD SetBrowserData()
   METHOD SaveVar()
ENDCLASS

//-------------------------------------------------------------------------------------------------

METHOD SetBrowserData( oObj, nState ) CLASS ObjInspect
   DEFAULT nState TO 0
   IF nState == 0
      ::CurObject := oObj
   ENDIF
   IF oObj:handle == ::CurObject:handle .OR. nState == 0
      ::Browser:source := __ObjGetValueList( oObj, NIL, HB_OO_CLSTP_EXPORTED )
      aSort( ::Browser:Source,,, {|x,y| x[1] < y[1] } )
      aEval( ::Browser:Source, {|a|a[1] := Proper( a[1] )} )
      ::Browser:RefreshAll()
   endif

   IF oObj:ClassName == "TFORMEDIT"
      oObj:XFMRoot()
   ELSE
      oObj:Parent:XFMControl( , oObj, .F. )
   ENDIF

RETURN Self

//-------------------------------------------------------------------------------------------------

METHOD OnCreate() CLASS ObjInspect

   local aRect := ::ClientRect()
   local oCombo:= ComboInsp():New(  self, 100, 0, 0, aRect[3], 100 )
   oCombo:Style:= WS_CHILD + WS_VISIBLE + WS_BORDER + WS_TABSTOP + CBS_DROPDOWNLIST + WS_VSCROLL + CBS_HASSTRINGS + CBS_OWNERDRAWFIXED

   ::Add( 'InspCombo', oCombo )
   ::InspCombo:SetItemHeight( -1, 15 )

   ::Add( 'InspTabs', TTabControl():New( self, 101,  0,  25, aRect[3], aRect[4]-25) )
   ::InspTabs:AddTab( "Properties")
   ::InspTabs:AddTab( "Events", TabPage():New( ::InspTabs, "Events") )
   ::InspTabs:Configure()

   ::Browser:=InspectBrowser():New( ::InspTabs:Properties )
   ::Browser:Create()

return( super:OnCreate() )

//----------------------------------------------------------------------------------------------

METHOD SaveVar(cText,nKey) CLASS ObjInspect
   local cType, cVar, oObj
   cVar := ::Browser:source[::Browser:RecPos][1]
   cType:= valtype( __objSendMsg( ::CurObject, cVar ) )
   do case
      case cType == 'N'
           cText:=VAL(cText)
      case cType == 'U'
           cText:=NIL
      case cType == 'L'
           cText:= IIF( cText == ".T.",.T.,.F.)
   endcase

   if __objSendMsg( ::CurObject, cVar ) != cText
      __objSendMsg( ::CurObject, "_"+cVar, cText )
      
      ::Browser:source[::Browser:RecPos][2]:= cText
      ::Browser:RefreshCurrent()


//------------------------------------- XFM UPDATE ---------------------------------------
      IF ::CurObject:ClassName == "TFORMEDIT"
         ::CurObject:XFMRoot()
      ELSE
         ::CurObject:Parent:XFMControl( , ::CurObject, .F. )
         ::CurObject:Parent:oMask:Refresh()
      ENDIF
//----------------------------------------------------------------------------------------
      SetFocus(::Browser:handle)
   endif

   IF nKey==VK_UP .OR. nKey==VK_DOWN
      PostMessage( ::Browser:handle, WM_KEYDOWN, nKey, 0 )
      PostMessage( ::Browser:handle, WM_LBUTTONDBLCLK, 0, 0 )
      ::Browser:RefreshAll()
   ENDIF
return(self)

//----------------------------------------------------------------------------------------------

CLASS ComboInsp FROM TComboBox
   METHOD DrawItem()
   METHOD OnClick()
   METHOD AddString()
   METHOD SetCurSel()
   METHOD DelObject()
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
   IF n<0
      ::Parent:Browser:source:={"",""}
      ::Parent:Browser:RefreshAll()
     else
      ::Parent:SetBrowserData( ::Parent:Objects[n+1] )
      ::Parent:CurObject:SetFocus()
   endif
return(super:SetCurSel(n))

//---------------------------------------------------------------------------------

METHOD DelObject( oObj ) CLASS ComboInsp
   local n,x,y
   IF ( n:= aScan( ::Parent:Objects, {|o|o:handle == oObj:handle} ))>0
      for x:=1 to len(::Parent:Parent:ObjTree:Tree:Items)
          if( y:=aScan( ::Parent:Parent:ObjTree:Tree:Items[x]:Items,{|o|o:cargo == oObj:handle} ))>0
             ::Parent:Parent:ObjTree:Tree:Items[x]:Items[y]:Delete()
             exit
          endif
      next
      aDel( ::Parent:Objects, n, .T. )
      ::DeleteString( n-1 )
      ::SetCurSel( n-2 )
   ENDIF
return(nil)

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


CLASS InspectBrowser FROM TWBrowse
   DATA oCtrl
   METHOD New() CONSTRUCTOR
   METHOD SetColControl()
   METHOD OnCommand()
ENDCLASS

METHOD New( oParent ) CLASS InspectBrowser
   local oCol1,oCol2, aProp := { {"",""} }
   super:New( oParent, 150, 0, 0, 100, 100, aProp )
   ::BgColor      := GetSysColor(COLOR_BTNFACE)
   ::HiliteNoFocus:= GetSysColor(COLOR_BTNFACE)
   ::wantHScroll  :=.F.
   ::HeadHeight   := 20

   oCol1:= whColumn():Init( "Property",{|oCol,oB,n| asString(oB:source[n,1]) },DT_LEFT, 84 )
   oCol1:VertAlign  := TA_CENTER
   oCol1:Style      := TBC_MOVE + TBC_SIZE

   oCol2:= whColumn():Init( "Value",   {|oCol,oB,n| asString(oB:source[n,2]) },DT_LEFT, 80 )
   oCol2:VertAlign  := TA_CENTER
   oCol2:Style      := TBC_MOVE + TBC_SIZE
   oCol2:bSaveBlock := {|cText,o,nKey|oApp:MainFrame:ObjInsp:SaveVar(cText,nKey)}

   ::AddColumn( oCol1 )
   ::AddColumn( oCol2 )

   ::bOnDblClick   := {|o,x,y|::SetColControl(x,y)}
   ::Font          := oParent:Parent:font
RETURN(self)

METHOD SetColControl(x,y) CLASS InspectBrowser
   local cType, cVar, aRect
   if y==2
      IF ::oCtrl!=NIL
         ::oCtrl:Destroy()
         ::oCtrl:=NIL
      ENDIF
      cVar := ::source[::RecPos][1]
      cType:= valtype( __objSendMsg( ::Parent:Parent:Parent:CurObject, cVar ) )
      DO CASE
         CASE cType == "L"
              aRect:=::GetItemRect()
              ::oCtrl:=TComboBox():New(self,222,aRect[1]-1,aRect[2]-1,aRect[3]-aRect[1]+1,100) //,aRect[4]-aRect[2]+1)
              ::oCtrl:Create()
              ::oCtrl:SetItemHeight(-1,aRect[4]-(aRect[2]+5))
              ::oCtrl:AddString("TRUE")
              ::oCtrl:AddString("FALSE")
         CASE cType == "C"
              ::EditCell(,::Columns[::ColPos]:bSaveBlock,,,,)
         CASE cType == "O"
              aRect:=::GetItemRect()
              view aRect
              ::oCtrl:=TButton():New( self, "...", 333, aRect[3]-(aRect[4]-aRect[2]+1), aRect[2]-1, (aRect[4]-aRect[2]+1), aRect[4]-aRect[2]+1)
              ::oCtrl:Create()
              ::oCtrl:SetFocus()
      ENDCASE
   endif
RETURN(self)

METHOD OnCommand(nwParam,nlParam) CLASS InspectBrowser
   LOCAL oList
   IF ::oCtrl!=NIL.AND.nlParam==::oCtrl:handle
      DO CASE
         CASE ::oCtrl:ClassName()=="TCOMBOBOX"
              IF HIWORD(nwParam)==CBN_KILLFOCUS
                 ::oCtrl:Destroy()
                 ::oCtrl:=NIL
              ENDIF
         CASE ::oCtrl:ClassName()=="TBUTTON"
              DO CASE
                 CASE HIWORD(nwParam)==BN_KILLFOCUS
                      ::oCtrl:Destroy()
                      ::oCtrl:=NIL
                 CASE HIWORD(nwParam)==BN_CLICKED
                      ::oCtrl:Destroy()
                      ::oCtrl:=NIL
                      oList:=StringList():New(self)
                      oList:Create()
              ENDCASE
      ENDCASE
   ENDIF
RETURN(nil)


//------------------------------------------------------------------------------------------

CLASS StringList FROM TPanel
   VAR nEProc PROTECTED
   METHOD New(oParent) INLINE ::resname := "StringList",;
                              ::Modal   := .T.,;
                              Super:New( oParent )
   METHOD OnCommand()
   METHOD OnCreate()
ENDCLASS

METHOD OnCreate() CLASS StringList
   local n,nLines
   local cText:=""
   FOR n:= 1 TO LEN(oApp:MainFrame:ObjInsp:CurObject:Items:Text)
       cText+=oApp:MainFrame:ObjInsp:CurObject:Items:Text[n]+CRLF
   NEXT
   cText:=LEFT( cText, LEN( cText)-2)
   SetDlgItemText( ::handle, 103, cText)
   nLines:=SendDlgItemMessage( ::handle, 103, EM_GETLINECOUNT, 0, 0 )
   SetDlgItemText( ::handle, 101, alltrim(str(nLines))+" Lines" )
RETURN(self)

METHOD OnCommand( nwParam ) CLASS StringList
   local n, cText, nPtr
   static nLines
   DO CASE
      CASE HIWORD(nwParam) == EN_CHANGE
           n:=SendDlgItemMessage( ::handle, 103, EM_GETLINECOUNT, 0, 0 )
           IF nLines != n
              nLines := n
              SetDlgItemText( ::handle, 101, alltrim(str(nLines))+" Lines" )
           ENDIF
      CASE nwParam == IDOK
           IF nLines==NIL
              nLines:=SendDlgItemMessage( ::handle, 103, EM_GETLINECOUNT, 0, 0 )
           ENDIF
           oApp:MainFrame:ObjInsp:CurObject:Items:Text:={}
           FOR n:=1 TO nLines
               cText := i2bin(100)+space(200)
               SendDlgItemMessage( ::handle, 103, EM_GETLINE, n-1, cText )
               cText:=STRTRAN(cText,CHR(10))
               cText:=STRTRAN(cText,CHR(13))
               oApp:MainFrame:ObjInsp:CurObject:Items:Add(ALLTRIM(cText))
               oApp:MainFrame:ObjInsp:CurObject:Parent:XFMControl( , oApp:MainFrame:ObjInsp:CurObject, .F. )
           NEXT
           nLines := NIL
           EndDialog( ::handle, IDOK )
      CASE nwParam == IDCANCEL
           nLines := NIL
           EndDialog( ::handle, IDCANCEL )
   ENDCASE
return(nil)
