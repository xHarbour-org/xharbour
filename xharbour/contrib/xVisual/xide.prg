/*
 * $Id: xide.prg,v 1.76 2002/10/06 23:54:06 what32 Exp $
 */

/*
 * xHarbour Project source code:
 *
 * xIDE Main Module
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
#include "common.ch"
#include "hbclass.ch"
#include "debug.ch"
#include "what32.ch"
#Include "toolbar.ch"
#Include "winlview.ch"
#include "wintypes.ch"
#include "cstruct.ch"

GLOBAL oApp

//-------------------------------------------------------------------------------------------

FUNCTION Main
   local oSplash

   oApp := Application():Initialize()

   // splash screen
   oSplash := TSplash():New( oApp, "visual_xharbour.bmp", 5000 )

   WITH OBJECT oApp
      WITH OBJECT :CreateForm( 'MainFrame', MainFrame() )
         :SetStyle( WS_THICKFRAME, .F. )
         :SetStyle( WS_MAXIMIZEBOX, .F. )
         :MainMenu()
         :MainToolBar()
         :MainStatusBar()

         // add the object windows
         :Add( 'ObjTree', ObjTree():New( oApp:MainFrame ) )
         :Add( 'ObjInsp', ObjInspect():New( oApp:MainFrame ) )
         :Add( 'ObjEdit', ObjEdit():New( oApp:MainFrame ) )

         // focus to main Frame
         :SetFocus()
      END
      :Run()
  END
RETURN( nil)

//----------------------------------------------------------------------------------------------

CLASS MainFrame FROM TFrame
   METHOD New( oParent ) INLINE ::Caption := 'xHarbour xIde',;
                                ::left    := 0,;
                                ::top     := 0,;
                                ::width   := GetWindowRect(GetDesktopWindow())[3],;
                                ::height  := 125,;
                                ::Icon    := LoadIcon(hInstance(),'0IDE'),;
                                super:new( oParent )

   METHOD OnCloseQuery() INLINE if( ::MsgBox( 'Quitting xIDE ?','Exit', MB_YESNO ) == IDYES,;
                                    NIL, 0 )
   METHOD MainMenu()
   METHOD MainToolBar()
   METHOD MainStatusBar()
ENDCLASS

//----------------------------------------------------------------------------------------------

METHOD MainMenu() CLASS MainFrame
   ::WindowMenu := TMenu():New()
   With Object ::WindowMenu
      :AddPopup('&Test')
      With Object :Popup
         :AddItem( 'Editor', 101, {||oApp:CreateForm( 'Form1', TFormEdit(), oApp:MainFrame ) } )
         :AddSeparator()
         :AddItem( 'Exit'  , 200, {||oApp:MainFrame:PostMessage(WM_SYSCOMMAND,SC_CLOSE)} )
      end
   end
   ::SetWindowMenu()
return(self)

//----------------------------------------------------------------------------------------------

METHOD MainToolBar() CLASS MainFrame
   local n, oTool, oSplash
   LOCAL hImg1,hImg2,hBmp,aStdTab
   With Object ::Add('Rebar', TRebar():New( oApp:MainFrame ) )
      // add the xmake toolbar
      With Object :Add( 'Tools', TToolBar():New( oApp:MainFrame:GetObj("Rebar"), 444, 15, , , 26, 26, 20, 20, 14 ))
         :AddButton( "NewProj",      ToolButton():New( 0,,"New Project",                    100 ) )
         :AddButton( "OpenProj",     ToolButton():New( 1,,"Open Project",                   101 ) )
         :AddButton( "Properties",   ToolButton():New( 2,,"Properties",                     102 ) )
         :AddButton( "Build",        ToolButton():New( 3,,"Build Application",              103 ) )
         :AddButton( "BldLunch",     ToolButton():New( 4,,"Build and Launch Application",   104 ) )
         :AddButton( "ReBldLunch",   ToolButton():New( 5,,'Re-Build Application',           105 ) )
         :AddButton( "ReBldLunchApp",ToolButton():New( 6,,'Re-Build and Launch Application',106 ) )
         :AddButton( "LunchApp",     ToolButton():New( 7,,'Launch Application',             107 ) )
         :AddButton( "SingSource",   ToolButton():New( 8,,'Compile Single Source',          108 ) )
         :AddButton( "AllSources",   ToolButton():New( 9,,'Compile All Sources',            109 ) )
         :AddButton( "LinkOnly",     ToolButton():New(10,,'Link Only',                      110 ) )
         :AddButton( "CompPPO",      ToolButton():New(11,,'Compile to PPO',                 111 ) )
         :AddButton( "View",         ToolButton():New(12,,'View',                           112 ) )
         :AddButton( "Files",        ToolButton():New(13,,'Files',                          113 ) )

         SendMessage( :handle, TB_SETROWS, 2 )
         // ----------------------------------------------------   set imagelist
         hImg1:= ImageList_Create( 20, 20, ILC_COLORDDB+ILC_MASK )
         hBmp := LoadImage( hInstance(), "XMAKE", IMAGE_BITMAP, 0, 0, LR_LOADTRANSPARENT )
         ImageList_AddMasked( hImg1, hBmp, RGB( 0, 255, 255 ) )
         DeleteObject(hBmp)
         SendMessage( :handle, TB_SETIMAGELIST, 0, hImg1 )
         //---------------------------------------------------------------------
      End
      :AddBand( NIL, RBBS_GRIPPERALWAYS + RBBS_NOVERT , :GetObj("Tools"):handle, 200, 52, 200, "", NIL )

      // add the TabControl on the Rebarband
      With Object :Add( 'Tabs', TTabControl():New( oApp:MainFrame:GetObj("Rebar"), 445,  0,  0,  0,  0) )
         :AddTab( "StdTab", TabPage():New( oApp:MainFrame:GetObj("Rebar"):GetObj("Tabs"), "Standard" ) )
         :AddTab( "Aditional" )
         :AddTab( "Win32" )
         :AddTab( "System" )
         :AddTab( "Internet" )
         :AddTab( "Dialogs" )
         :AddTab( "Win 3.1" )
         :AddTab( "Samples" )
         :AddTab( "Activex" )
      End
      :AddBand( NIL, RBBS_GRIPPERALWAYS + RBBS_NOVERT , :GetObj("Tabs"):handle, 550, 56, , "", NIL )
      :GetObj("Tabs"):Configure()

      // sets the controls toolbar on the TabControl
      With Object :Tabs:StdTab
         With Object :Add( 'TabBand', TRebar():New( oApp:MainFrame:Rebar:Tabs:StdTab ) )
            :SetStyle( WS_BORDER, .F. )
            With Object :Add( 'StdTools', TToolBar():New( oApp:MainFrame:Rebar:Tabs:StdTab:TabBand, 444, 14, , , 28, 28, 20, 20 ) )
               :SetStyle( TBSTYLE_CHECKGROUP )

               aStdTab := { '', 'Frames', 'MainMenu', 'PopupMenu', 'Label', 'Edit', 'Memo', 'Button', ;
                                'CheckBox', 'RadioButton', 'ListBox', 'ComboBox', 'ScrollBar', 'GroupBox', ;
                                'RadioGroup', 'Panel', 'ActionList' }
               for n:=0 to 16
                   oTool := ToolButton():New( n,,aStdTab[n+1], 150+n )
                   oTool:Action := {|oItem| oApp:Form1:OnMenuCommand(oItem) }
                   oTool:Style  := TBSTYLE_BUTTON + TBSTYLE_CHECKGROUP
                   :AddButton( if(n==0,'arrow',aStdTab[n+1] ), oTool )
               next

               // ----------------------------------------------------   set imagelist
               hImg2:= ImageList_Create( 24, 24, ILC_COLORDDB+ILC_MASK )
               hBmp := LoadImage( hInstance(), "STDTAB", IMAGE_BITMAP, 0, 0, LR_LOADTRANSPARENT )
               ImageList_AddMasked( hImg2, hBmp, RGB( 0, 255, 255 ) )
               DeleteObject(hBmp)
               SendMessage( :handle, TB_SETIMAGELIST, 0, hImg2 )
               //---------------------------------------------------------------------
            End
            :AddBand( NIL, RBBS_NOVERT, :GetObj("StdTools"):handle, 100, 30,  , "", NIL )
            :GetObj("StdTools"):DisableAll()

            //--------- sets a QUICK access to the control
            oApp:MainFrame:SetLink( 'StdBar', :GetObj("StdTools") )
         End
      End
   End
return(self)

//----------------------------------------------------------------------------------------------

METHOD MainStatusBar() CLASS MainFrame
   ::Add('Status',  TStatusBar():New( oApp:MainFrame, 'StatusBar', 1001 ) )
   ::Status:SetPanels( { 150,380,480,580,-1 } )
   ::Status:SetPanelText( 0, "Visual xHarbour" )
return(self)

//----------------------------------------------------------------------------------------------

CLASS ObjEdit FROM TForm
   METHOD New( oParent ) INLINE ::Caption := 'xIde Quick Editor',;
                                ::left    := 100,;
                                ::top     := 100,;
                                ::width   := 300,;
                                ::height  := 300,;
                                super:new( oParent )
   METHOD OnCreate()
   METHOD OnSize(n,x,y)  INLINE ::QuickEdit:Move(,,x,y,.t.),nil
ENDCLASS

METHOD OnCreate() CLASS ObjEdit
   local aRect := ::ClientRect()
   ::Add('QuickEdit', TEdit():New( self, "", 101,  0,0, aRect[3], aRect[4] ),.F. )
   ::QuickEdit:Style := WS_CHILD+WS_CLIPSIBLINGS+WS_VISIBLE+ES_MULTILINE+WS_BORDER+WS_VSCROLL+WS_HSCROLL+ES_AUTOVSCROLL+ES_AUTOHSCROLL+WS_TABSTOP
   ::QuickEdit:create()
return(nil)

