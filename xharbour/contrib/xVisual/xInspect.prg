/*
 * $Id: xInspect.prg,v 1.54 2002/10/29 23:50:11 what32 Exp $
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
GLOBAL EXTERNAL FormEdit

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

   METHOD OnCloseQuery() INLINE 0
   METHOD OnCreate()
   METHOD OnSize(n,x,y)  INLINE  ::ComboBox1:Move(,,x,21,.t.),;
                                 ::InspTabs:Move(,25,x,y-25,.t.),;
                                 ::browser:width := ::InspTabs:Properties:ClientRect()[3],;
                                 ::browser:height:= ::InspTabs:Properties:ClientRect()[4],;
                                 nil
   METHOD SetBrowserData()
   METHOD SaveVar()
ENDCLASS

//-------------------------------------------------------------------------------------------------

METHOD SetBrowserData( oObj, bCurrent ) CLASS ObjInspect

   LOCAL aProp

   IF ::CurObject == oObj
      IF bCurrent
         // Do nothing - already selected.
         RETURN Self
      ELSE
         FOR EACH aProp IN ::Browser:source
            aProp[2] := __objSendMsg( ::CurObject, aProp[1] )
         NEXT

         ::Browser:RefreshAll()
      ENDIF
   ELSE
      ::Browser:source := __ObjGetValueList( oObj, NIL, HB_OO_CLSTP_EXPORTED )
      aSort( ::Browser:Source,,, {|x,y| x[1] < y[1] } )
      aEval( ::Browser:Source, {|a|a[1] := Proper( a[1] ) } )
      ::Browser:RefreshAll()

      ::CurObject := oObj
   ENDIF

   IF oObj:ClassName == "TFORMEDIT"
      oObj:XFMRoot()
   ELSE
      oObj:Parent:XFMControl( , oObj, .F. )
   ENDIF

RETURN Self

//-------------------------------------------------------------------------------------------------

METHOD OnCreate() CLASS ObjInspect

   LOCAL oTabs

   local aRect := ::ClientRect()
   local oCombo:= ComboInsp():New(  self, 100, 0, 0, aRect[3], 100 )
   oCombo:Style:= WS_CHILD + WS_VISIBLE + WS_BORDER + WS_TABSTOP + CBS_DROPDOWNLIST + WS_VSCROLL + CBS_HASSTRINGS + CBS_OWNERDRAWFIXED

   ::Add( oCombo )
   ::ComboBox1:SetItemHeight( -1, 15 )

   oTabs := TTabControl():New( self, 101,  0,  25, aRect[3], aRect[4]-25)
   oTabs:Name := "InspTabs"
   ::Add( oTabs )

   ::InspTabs:AddTab( "Properties")
   ::InspTabs:AddTab( "Events", TabPage():New( ::InspTabs, "Events") )
   ::InspTabs:Configure()

   ::Browser:=InspectBrowser():New( ::InspTabs:Properties )
   ::Browser:Create()

return( super:OnCreate() )

//----------------------------------------------------------------------------------------------

METHOD SaveVar(cText,nKey) CLASS ObjInspect

   LOCAL cType, cVar, oObj

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
//      SetFocus(::Browser:handle)
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
   LOCAL oObj

   IF HiWord( nwParam ) == CBN_SELCHANGE
      oObj := ::Parent:Objects[::GetCurSel()+1]

      IF ! ( ::Parent:CurObject == oObj )
         ::Parent:SetBrowserData( oObj, .T. )
      ENDIF

      IF ( ! FormEdit:oMask:IsFocused( ::Parent:CurObject:handle ) ) .AND. ( ! FormEdit:oMask:Creating ) ;
         .AND. ( ! FormEdit:oMask:mousedown ) .AND. ( ! FormEdit:oMask:moving ) .AND. ( ! FormEdit:oMask:sizing ) ;
         .AND. ( ! FormEdit:oMask:selecting )

         FormEdit:oMask:OnLButtonDown( , ::Parent:CurObject:Left + 4, ::Parent:CurObject:Top + 4 )
         FormEdit:oMask:OnLButtonUp( , ::Parent:CurObject:Left + 4, ::Parent:CurObject:Top + 4 )
      ENDIF

   ENDIF

RETURN 0

//---------------------------------------------------------------------------------

METHOD AddString(cText,oObj) CLASS ComboInsp
   aadd(::Parent:Objects,oObj)
return(super:AddString(cText))

//---------------------------------------------------------------------------------

METHOD SetCurSel(n) CLASS ComboInsp

   LOCAL oObj

   IF n < 0
      ::Parent:Browser:source := { "", "" }
      ::Parent:Browser:RefreshAll()
   ELSE
      oObj := ::Parent:Objects[ n + 1 ]

      IF ! ( ::Parent:CurObject == oObj )
         ::Parent:SetBrowserData( oObj, .T. )
      ENDIF

      IF FormEdit != NIL
      
         IF ( ! FormEdit:oMask:IsFocused( ::Parent:CurObject:handle ) ) .AND. ( ! FormEdit:oMask:Creating ) ;
            .AND. ( ! FormEdit:oMask:mousedown ) .AND. ( ! FormEdit:oMask:moving ) .AND. ( ! FormEdit:oMask:sizing ) ;
            .AND. ( ! FormEdit:oMask:selecting )

            FormEdit:oMask:OnLButtonDown( , ::Parent:CurObject:Left + 4, ::Parent:CurObject:Top + 4 )
            FormEdit:oMask:OnLButtonUp( , ::Parent:CurObject:Left + 4, ::Parent:CurObject:Top + 4 )
         ENDIF
         
      ENDIF
      
   ENDIF

RETURN Super:SetCurSel( n )

//---------------------------------------------------------------------------------

METHOD DelObject( oObj ) CLASS ComboInsp
   LOCAL n,x,y
   IF ( n:= aScan( ::Parent:Objects, {|o|o:handle == oObj:handle} ))>0
   
      FOR x:=1 to len(::Parent:Parent:ObjTree:TreeView1:Items)
      
          IF( y:=aScan( ::Parent:Parent:ObjTree:TreeView1:Items[x]:Items,{|o|o:cargo == oObj:handle} ))>0
             ::Parent:Parent:ObjTree:TreeView1:Items[x]:Items[y]:Delete()
             EXIT
          ENDIF
          
      NEXT
      
      aDel( ::Parent:Objects, n, .T. )
      ::DeleteString( n-1 )
      ::SetCurSel( n-2 )
   ENDIF
   
RETURN nil

//---------------------------------------------------------------------------------

METHOD DrawItem( dis ) CLASS ComboInsp

   LOCAL lSelected
   LOCAL aClip, aRect
   LOCAL itemTxt, cText
   LOCAL nLen, n
   
   lSelected := And( dis:itemState, ODS_SELECTED ) > 0
   
   aClip := { dis:rcItem:Left,  dis:rcItem:Top, ;
              dis:rcItem:Right, dis:rcItem:Bottom  }
              
   IF And( dis:itemAction, ODA_DRAWENTIRE ) > 0 .OR. And( dis:itemAction, ODA_SELECT ) > 0
   
      SetTextColor( dis:hDC  , GetSysColor(IF( lselected,COLOR_HIGHLIGHTTEXT,COLOR_WINDOWTEXT )) )
      SetBkColor( dis:hDC  , GetSysColor(IF( lselected,COLOR_HIGHLIGHT,COLOR_WINDOW )) )
      
      nLen    := SendMessage( dis:hwndItem, CB_GETLBTEXTLEN, dis:itemID, 0 )
      itemTxt := Space( nLen + 1 )
      SendMessage( dis:hwndItem, CB_GETLBTEXT, dis:itemID, @itemTxt )
      
      itemTxt :=left(itemTxt,nLen)
      cText   := ""
      aRect   := ACLONE(aClip)
      FOR n := 1 to nLen + 1
      
          IF SubStr( itemTxt, n, 1) == chr(9) .or. n == nLen + 1
             IF n == nLen + 1
                SetTextColor( dis:hDC, GetSysColor( IF( lselected, COLOR_HIGHLIGHTTEXT,;
                                                                   COLOR_BTNSHADOW ) ) )
             ENDIF
             ExtTextOut( dis:hDC, dis:rcItem:Left + aRect[1]+2, dis:rcItem:Top , ;
                                 ETO_OPAQUE + ETO_CLIPPED, aRect, cText )
             cText := ""
             aRect[1] += 80
             LOOP
          ENDIF
          
          cText += SubStr( itemTxt, n, 1 )
      NEXT
      
   ENDIF
   IF And( dis:itemState, ODS_FOCUS ) > 0 .OR. And( dis:itemAction, ODA_FOCUS ) > 0
      DrawfocusRect( dis:hDC, aclip )
   ENDIF

return( 1 )

//---------------------------------------------------------------------------------


CLASS InspectBrowser FROM TWBrowse

   DATA oCtrl
   METHOD New() CONSTRUCTOR
   METHOD SetColControl()
   METHOD OnCommand()

ENDCLASS

//-------------------------------------------------------------------------------------------

METHOD New( oParent ) CLASS InspectBrowser

   LOCAL oCol1,oCol2, aProp := { {"",""} }

   super:New( oParent, 150, 0, 0, 100, 100, aProp )

   ::BgColor      := GetSysColor( COLOR_BTNFACE )
   ::HiliteNoFocus:= GetSysColor( COLOR_BTNFACE )
   ::wantHScroll  :=.F.
   ::HeadHeight   := 20

   oCol1 := whColumn():Init( "Property",{|oCol,oB,n| asString(oB:source[n,1]) }, DT_LEFT, 84 )
   oCol1:VertAlign  := TA_CENTER
   oCol1:Style      := TBC_MOVE + TBC_SIZE
   ::AddColumn( oCol1 )

   oCol2 := whColumn():Init( "Value",   {|oCol,oB,n| asString(oB:source[n,2]) }, DT_LEFT, 80 )
   oCol2:VertAlign  := TA_CENTER
   oCol2:Style      := TBC_MOVE + TBC_SIZE
   oCol2:bSaveBlock := {| cText, o, nKey| oApp:MainFrame:ObjInspect:SaveVar( cText, nKey ) }
   ::AddColumn( oCol2 )

   ::bOnDblClick   := {|o,x,y|::SetColControl(x,y)}
   ::Font          := oParent:Parent:font

RETURN self

//-------------------------------------------------------------------------------------------

METHOD SetColControl(x,y) CLASS InspectBrowser

   LOCAL cType, cVar, aRect
   
   if y == 2

      IF ::oCtrl != NIL
         ::oCtrl:Destroy()
         ::oCtrl := NIL
      ENDIF

      cVar := ::source[::RecPos][1]
      cType:= valtype( __objSendMsg( ::Parent:Parent:Parent:CurObject, cVar ) )
      DO CASE
         CASE cType == "L"
              aRect   := ::GetItemRect()
              ::oCtrl := TComboBox():New( self, 222, aRect[1]-1, aRect[2]-1,;
                                                     aRect[3]-aRect[1]+1, 100)
              ::oCtrl:Create()
              ::oCtrl:SetItemHeight( -1, aRect[4]-(aRect[2]+5) )
              ::oCtrl:AddString( "TRUE" )
              ::oCtrl:AddString( "FALSE" )

         CASE cType == "C"
              ::EditCell(,::Columns[::ColPos]:bSaveBlock,,,,)

         CASE cType == "O"
              aRect:=::GetItemRect()
              view aRect
              ::oCtrl:=TButton():New( self, "...", 333, aRect[3]-(aRect[4]-aRect[2]+1), aRect[2]-1, (aRect[4]-aRect[2]+1), aRect[4]-aRect[2]+1)
              ::oCtrl:Create()
              ::oCtrl:SetFocus()
      ENDCASE
   ENDIF

RETURN self

//-------------------------------------------------------------------------------------------

METHOD OnCommand(nwParam,nlParam) CLASS InspectBrowser
   LOCAL oList

   IF ::oCtrl != NIL .AND. nlParam == ::oCtrl:handle

      DO CASE
         CASE ::oCtrl:ClassName() == "TCOMBOBOX"

              IF HiWord(nwParam) == CBN_KILLFOCUS
                 ::oCtrl:Destroy()
                 ::oCtrl := NIL
              ENDIF

         CASE ::oCtrl:ClassName() == "TBUTTON"
              DO CASE
                 CASE HiWord(nwParam) == BN_KILLFOCUS
                      ::oCtrl:Destroy()
                      ::oCtrl := NIL

                 CASE HiWord(nwParam)==BN_CLICKED
                      ::oCtrl:Destroy()
                      ::oCtrl := NIL

                      oList := StringList():New(self)
                      oList:Create()
              ENDCASE
      ENDCASE
      
   ENDIF
   
RETURN nil

//------------------------------------------------------------------------------------------

CLASS StringList FROM TPanel

   VAR nEProc PROTECTED
   METHOD New(oParent) INLINE ::resname := "StringList",;
                              ::Modal   := .T.,;
                              Super:New( oParent )
   METHOD OnCommand()
   METHOD OnCreate()

ENDCLASS

//-------------------------------------------------------------------------------------------

METHOD OnCreate() CLASS StringList

   LOCAL cText := "", cItem

   FOR EACH cItem IN oApp:MainFrame:ObjInspect:CurObject:Items:Text
      cText += ( cItem + CRLF )
   NEXT

   cText:= Left( cText, Len( cText ) -2 )

   PostMessage( GetDlgItem( ::handle, 103 ), EM_SETSEL, 0, 0)
   SetDlgItemText( ::handle, 103, cText )
   SetDlgItemText( ::handle, 101, AllTrim( Str( Len( oApp:MainFrame:ObjInspect:CurObject:Items:Text ) ) ) + " Lines" )

RETURN Self

//-------------------------------------------------------------------------------------------

METHOD OnCommand( nwParam ) CLASS StringList
   local n, cText, nPtr
   static nLines

   DO CASE
      CASE HIWORD(nwParam) == EN_CHANGE
           n := SendDlgItemMessage( ::handle, 103, EM_GETLINECOUNT, 0, 0 )
           IF nLines != n
              nLines := n
              SetDlgItemText( ::handle, 101, AllTrim( Str(nLines) ) + " Lines" )
           ENDIF

      CASE nwParam == IDOK
           IF nLines == NIL
              nLines := SendDlgItemMessage( ::handle, 103, EM_GETLINECOUNT, 0, 0 )
           ENDIF

           oApp:MainFrame:ObjInspect:CurObject:Items:Text := {}
           FOR n := 1 TO nLines
               cText := I2Bin( 100 ) + Space( 200 )
               SendDlgItemMessage( ::handle, 103, EM_GETLINE, n - 1, @cText )
               cText := StrTran( cText, Chr(0) , '' )
               cText := StrTran( cText, Chr(10), '' )
               cText := StrTran( cText, Chr(13), '' )
               view cText
               oApp:MainFrame:ObjInspect:CurObject:Items:Add( AllTrim( cText ) )
           NEXT

           oApp:MainFrame:ObjInspect:CurObject:Parent:XFMControl( , oApp:MainFrame:ObjInspect:CurObject, .F. )
           nLines := NIL
           EndDialog( ::handle, IDOK )

      CASE nwParam == IDCANCEL
           nLines := NIL
           EndDialog( ::handle, IDCANCEL )
   ENDCASE

RETURN NIL
