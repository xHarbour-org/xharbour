/*
 * $Id: xInspect.prg,v 1.74 2003/01/29 10:27:32 what32 Exp $
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

#include "winuser.ch"
#include "wingdi.ch"
#include "hbclass.ch"
#include "what32.ch"
#include "wintypes.ch"
#include "cstruct.ch"
#include "debug.ch"

GLOBAL EXTERNAL Application
GLOBAL EXTERNAL FormEdit
GLOBAL EXTERNAL MainForm
GLOBAL EXTERNAL ObjInspect
GLOBAL EXTERNAL ObjTree
GLOBAL InspTabs
GLOBAL InspCombo
GLOBAL InspBrowse

CLASS ObjInspect FROM TForm

   DATA Browser   AS OBJECT PROTECTED

   DATA Objects   AS ARRAY INIT {}
   DATA CurObject AS OBJECT

   METHOD Create()

   METHOD WMSize(n,x,y)  INLINE  IIF( ! InspCombo == NIL, ( InspCombo:Width := x,;
                                                          InspTabs:Move( , 25, x, y-25, .T. ),;
                                                          InspBrowse:FWidth := InspTabs:Properties:ClientRect()[3],;
                                                          InspBrowse:FHeight:= InspTabs:Properties:ClientRect()[4],;
                                                          InspBrowse:Move( , , , , .T. ) ), ),;
                                                          NIL

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
         FOR EACH aProp IN InspBrowse:source
            aProp[2] := __objSendMsg( ::CurObject, aProp[1] )
         NEXT

         InspBrowse:RefreshAll()
      ENDIF

   ELSE

      InspBrowse:source := __ObjGetValueList( oObj, NIL, HB_OO_CLSTP_EXPORTED )
   
      aSort( InspBrowse:Source,,, {|x,y| x[1] < y[1] } )
      aEval( InspBrowse:Source, {|a|a[1] := Proper( a[1] ) } )

      InspBrowse:RefreshAll()
    
      ::CurObject := oObj

   ENDIF

/*
   IF oObj:ClassName == "TFORMEDIT"
      FormEdit:XFMRoot()
   ELSE
      FormEdit:XFMControl( , oObj, .F. )
   ENDIF
*/
RETURN Self

//-------------------------------------------------------------------------------------------------

METHOD Create( oParent ) CLASS ObjInspect
   
   LOCAL oPage
   
   // Object Inspector Window

   ::Super:Create( oParent )

   ::Objects  := {}

   ::FCaption := "Object Inspector"
   ::Name     := "ObjInspect"
   ::FLeft    := 0
   ::FTop     := 275
   ::FWidth   := 200
   ::FHeight  := 297
   ::ExStyle  := WS_EX_TOOLWINDOW

   ::SetParent( MainForm )

   // ComboBox   
   InspCombo := ComboInsp():Create( self )
   InspCombo:FWidth := ::FWidth - 8
   InspCombo:FHeight:= 200
   InspCombo:Style  := WS_CHILD + WS_VISIBLE + WS_BORDER + WS_TABSTOP + CBS_DROPDOWNLIST + WS_VSCROLL + CBS_HASSTRINGS + CBS_OWNERDRAWFIXED
   InspCombo:SetParent( Self )
   InspCombo:SetItemHeight( -1, 15 )


   // TabControls
   InspTabs := TTabControl():Create( self )

   InspTabs:FTop   := 25
   InspTabs:FWidth := ::FWidth - 8
   InspTabs:FHeight:= ::FHeight - 50

   InspTabs:SetParent( Self )
   
   oPage := TabPage():Create( InspTabs )
   oPage:Name    := "Properties"
   oPage:Caption := "Properties"
   oPage:SetParent( InspTabs )

   oPage := TabPage():Create( InspTabs )
   oPage:Name    := "Events"
   oPage:Caption := "Events"
   oPage:SetParent( InspTabs )

   InspBrowse:=InspectBrowser():Create( InspTabs:Properties )
   
   OBjInspect := Self
return( Self )

//----------------------------------------------------------------------------------------------

METHOD SaveVar(cText,nKey) CLASS ObjInspect

   LOCAL cType, cVar, oObj

   cVar := InspBrowse:source[InspBrowse:RecPos][1]
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

      InspBrowse:source[InspBrowse:RecPos][2]:= cText
      InspBrowse:RefreshCurrent()


//------------------------------------- XFM UPDATE ---------------------------------------
      IF ::CurObject:ClassName == "TFORMEDIT"
         FormEdit:XFMRoot()
      ELSE
         FormEdit:XFMControl( , ::CurObject, .F. )
         FormEdit:oMask:Refresh()
      ENDIF
//----------------------------------------------------------------------------------------
//      SetFocus(InspBrowse:handle)
   endif

   IF nKey==VK_UP .OR. nKey==VK_DOWN
      PostMessage( InspBrowse:handle, WM_KEYDOWN, nKey, 0 )
      PostMessage( InspBrowse:handle, WM_LBUTTONDBLCLK, 0, 0 )
      InspBrowse:RefreshAll()
   ENDIF
return(self)

//----------------------------------------------------------------------------------------------

CLASS ComboInsp FROM TComboBox

   METHOD DrawItem()
   METHOD AddString()
   METHOD SetCurSel()
   METHOD DelObject()
   METHOD MyOnClick()
   METHOD Create()

ENDCLASS

//---------------------------------------------------------------------------------

METHOD Create( oParent ) CLASS ComboInsp

   ::Super:Create( oParent )
   ::OnClick := HB_ObjMsgPtr( Self, "MyOnClick" )

RETURN Self

//---------------------------------------------------------------------------------
METHOD MyOnClick(nwParam,nlParam) CLASS ComboInsp
   LOCAL oObj

   IF HiWord( nwParam ) == CBN_SELCHANGE
      oObj := ObjInspect:Objects[::GetCurSel()+1]

      IF ! ( ObjInspect:CurObject == oObj )
         ObjInspect:SetBrowserData( oObj, .T. )
      ENDIF

      IF ( ! FormEdit:oMask:IsFocused( ObjInspect:CurObject:FHandle ) ) .AND. ( ! FormEdit:oMask:Creating ) ;
         .AND. ( ! FormEdit:oMask:mousedown ) .AND. ( ! FormEdit:oMask:moving ) .AND. ( ! FormEdit:oMask:sizing ) ;
         .AND. ( ! FormEdit:oMask:selecting )

         FormEdit:oMask:WMLButtonDown( , ObjInspect:CurObject:Left + 4, ObjInspect:CurObject:Top + 4 )
         FormEdit:oMask:WMLButtonUp( , ObjInspect:CurObject:Left + 4, ObjInspect:CurObject:Top + 4 )
      ENDIF

   ENDIF

RETURN 0

//---------------------------------------------------------------------------------

METHOD AddString( cText, oObj ) CLASS ComboInsp

   aAdd( ObjInspect:Objects, oObj )

RETURN Super:AddString( cText )

//---------------------------------------------------------------------------------

METHOD SetCurSel(n) CLASS ComboInsp

   LOCAL oObj

   IF n < 0
      InspBrowse:source := { "", "" }
      InspBrowse:RefreshAll()
   ELSE
      oObj := ObjInspect:Objects[ n + 1 ]

      IF ! ( ObjInspect:CurObject == oObj )
         ObjInspect:SetBrowserData( oObj, .T. )
      ENDIF

      IF FormEdit != NIL

         IF ( ! FormEdit:oMask:IsFocused( ObjInspect:CurObject:handle ) ) .AND. ( ! FormEdit:oMask:Creating ) ;
            .AND. ( ! FormEdit:oMask:mousedown ) .AND. ( ! FormEdit:oMask:moving ) .AND. ( ! FormEdit:oMask:sizing ) ;
            .AND. ( ! FormEdit:oMask:selecting )

            FormEdit:oMask:WMLButtonDown( , ObjInspect:CurObject:Left + 4, ObjInspect:CurObject:Top + 4 )
            FormEdit:oMask:WMLButtonUp( , ObjInspect:CurObject:Left + 4, ObjInspect:CurObject:Top + 4 )
         ENDIF

      ENDIF

   ENDIF

RETURN Super:SetCurSel( n )

//---------------------------------------------------------------------------------

METHOD DelObject( oObj ) CLASS ComboInsp

   LOCAL n,x,y

   IF ( n:= aScan( ObjInspect:Objects, {|o|o:FHandle == oObj:FHandle} ))>0

      FOR x:=1 to Len( ObjTree:TreeView1:Items)

          IF( y := aScan( ObjTree:TreeView1:Items[x]:Items, {|o| o:cargo == oObj:FHandle } ) ) > 0
             ObjTree:TreeView1:Items[x]:Items[y]:Delete()
             EXIT
          ENDIF

      NEXT

      aDel( ObjInspect:Objects, n, .T. )
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
   METHOD Create() CONSTRUCTOR
   METHOD SetColControl()
   METHOD WMCommand()

ENDCLASS

//-------------------------------------------------------------------------------------------

METHOD Create( oParent ) CLASS InspectBrowser

   LOCAL oCol1,oCol2, aProp := { {"",""} }

   ::Source := aProp
   
   ::Super:Create( oParent )

   ::FLeft  := 0
   ::FTop   := 0
   ::FWidth := oParent:FWidth
   ::FHeight:= oParent:FHeight

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
   oCol2:bSaveBlock := {| cText, o, nKey| ObjInspect:SaveVar( cText, nKey ) }
   ::AddColumn( oCol2 )

   ::bOnDblClick   := {|o,x,y|::SetColControl(x,y)}
   
   ::SetParent( oParent )
RETURN self

//-------------------------------------------------------------------------------------------

METHOD SetColControl(x,y) CLASS InspectBrowser

   LOCAL cType, cVar, aRect

   if y == 2

      IF ::oCtrl != NIL
         ::oCtrl:DestroyWindowHandle()
         ::oCtrl := NIL
      ENDIF

      cVar := ::source[::RecPos][1]
      cType:= valtype( __objSendMsg( ObjInspect:CurObject, cVar ) )
      DO CASE
         CASE cType == "L"
              aRect   := ::GetItemRect()
              
              ::oCtrl := TComboBox():Create( self )
              ::oCtrl:FLeft   := aRect[1]-1
              ::oCtrl:FTop    := aRect[2]-1
              ::oCtrl:FWidth  := aRect[3]-aRect[1]+1
              ::oCtrl:FHeight := 100
              
              ::oCtrl:SetParent( Self )

              ::oCtrl:SetItemHeight( -1, aRect[4]-(aRect[2]+5) )
              ::oCtrl:AddString( "TRUE" )
              ::oCtrl:AddString( "FALSE" )

         CASE cType == "C"
              ::EditCell(,::Columns[::ColPos]:bSaveBlock,,,,)
         CASE cType == "N"
              ::EditCell(,::Columns[::ColPos]:bSaveBlock,,,,)

         CASE cType == "O"
              aRect:=::GetItemRect()
              
              ::oCtrl := TButton():Create( Self )
              ::oCtrl:FLeft   := aRect[3]-(aRect[4]-aRect[2]+1)
              ::oCtrl:FTop    := aRect[2]-1
              ::oCtrl:FWidth  := aRect[4]-aRect[2]+1
              ::oCtrl:FHeight := aRect[4]-aRect[2]+1
              ::oCtrl:FCaption:= "..."
              ::oCtrl:Id      := 333
              
              ::oCtrl:SetParent( Self )
              ::oCtrl:SetFocus()
      ENDCASE
   ENDIF

RETURN self

//-------------------------------------------------------------------------------------------

METHOD WMCommand(nwParam,nlParam) CLASS InspectBrowser
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
                      ::oCtrl:DestroyWindowHandle()
                      ::oCtrl := NIL

                 CASE HiWord(nwParam)==BN_CLICKED
                      ::oCtrl:DestroyWindowHandle()
                      ::oCtrl := NIL

                      oList := StringList():Create(self)
                      oList:SetParent( self )
              ENDCASE
      ENDCASE

   ENDIF

RETURN nil

//------------------------------------------------------------------------------------------

CLASS StringList FROM TPanel

   VAR nEProc PROTECTED
   METHOD Create(oParent) INLINE ::resname := "StringList",;
                              ::Modal   := .T.,;
                              Super:Create( oParent )
   METHOD WMCommand()
   METHOD WMInitDialog()

ENDCLASS

//-------------------------------------------------------------------------------------------

METHOD WMInitDialog() CLASS StringList
   LOCAL cText := "", cItem

   FOR EACH cItem IN ObjInspect:CurObject:Items:Strings
      cText += ( cItem + CRLF )
   NEXT

   cText:= Left( cText, Len( cText ) -2 )

   PostMessage( GetDlgItem( ::handle, 103 ), EM_SETSEL, 0, 0)
   SetDlgItemText( ::handle, 103, cText )
   SetDlgItemText( ::handle, 101, AllTrim( Str( Len( ObjInspect:CurObject:Items:Strings ) ) ) + " Lines" )

RETURN Self

//-------------------------------------------------------------------------------------------

METHOD WMCommand( nwParam ) CLASS StringList
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

           ObjInspect:CurObject:Items:Strings := {}
           ObjInspect:CurObject:Items:Clear()
           
           FOR n := 1 TO nLines
               cText := I2Bin( 100 ) + Space( 200 )
               SendDlgItemMessage( ::handle, 103, EM_GETLINE, n - 1, @cText )
               cText := StrTran( cText, Chr(0) , '' )
               cText := StrTran( cText, Chr(10), '' )
               cText := StrTran( cText, Chr(13), '' )
               ObjInspect:CurObject:Items:Add( AllTrim( cText ) )
           NEXT

           FormEdit:XFMControl( , ObjInspect:CurObject, .F. )
           nLines := NIL
           EndDialog( ::handle, IDOK )

      CASE nwParam == IDCANCEL
           nLines := NIL
           EndDialog( ::handle, IDCANCEL )
   ENDCASE

RETURN NIL
