#include "windows.ch"
#include "wingdi.ch"
#include "common.ch"
#include "hbclass.ch"
#include "debug.ch"
#include "what32.ch"
#Include "toolbar.ch"
#Include "winlview.ch"

GLOBAL oApp

//-------------------------------------------------------------------------------------------

FUNCTION Main
   local oSplash

   oApp := Application():Initialize()

   // splash screen
   oSplash := TSplash():New( oApp, "visual_xharbour.bmp", 5000 )

   WITH OBJECT oApp
      WITH OBJECT :CreateFrame( 'MainFrame', MainFrame() )
         :SetStyle( WS_THICKFRAME, .F. )
         :SetStyle( WS_MAXIMIZEBOX, .F. )
         :MainMenu()
         :MainToolBar()
         :MainStatusBar()
         
         // add the object windows
         :Add( 'ObjTree', ObjTree():New( oApp:MainFrame ) )
         :Add( 'ObjInsp', ObjInspect():New( oApp:MainFrame ) )
         
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
                                super:new( oParent )

   METHOD OnCloseQuery() INLINE if( ::MsgBox( 'Quitting xIDE ?','Exit', MB_YESNO ) == IDYES,;
                                    PostQuitMessage(0), 0 )
   METHOD MainMenu()
   METHOD MainToolBar()
   METHOD MainStatusBar()
ENDCLASS

METHOD MainMenu() CLASS MainFrame
   ::WindowMenu := TMenu():New()
   With Object ::WindowMenu
      :AddPopup('&Test')
      With Object :Popup
         :AddItem( 'Editor', 101, {||oApp:CreateForm( 'SubForm', TFormEdit(),oApp:MainFrame ) } )
         :AddSeparator()
         :AddItem( 'Exit'  , 200, {||oApp:MainFrame:PostMessage(WM_SYSCOMMAND,SC_CLOSE)} )
      end
   end
   ::SetWindowMenu()
return(self)

METHOD MainToolBar() CLASS MainFrame
   local n, oTool, oSplash
   LOCAL hImg1,hImg2,hBmp,aStdTab
   With Object ::Add('Rebar', TRebar():New( oApp:MainFrame ) )
      // add the xmake toolbar
      With Object :Add( 'Tools', TToolBar():New( oApp:MainFrame:GetObj("Rebar"), 444, 15, , , 26, 26, 20, 20 ))
         :AddButton( "NewProj",      ToolButton():New( 0,,"New Project",10, {|o|MessageBox(,o:hint)} ) )
         :AddButton( "OpenProj",     ToolButton():New( 1,,"Open Project",11 ) )
         :AddButton( "Properties",   ToolButton():New( 2,,"Properties",12 ) )
         :AddButton( "Build",        ToolButton():New( 3,,"Build Application",13 ) )
         :AddButton( "BldLunch",     ToolButton():New( 4,,"Build and Launch Application",14 ) )
         :AddButton( "ReBldLunch",   ToolButton():New( 5,,'Re-Build Application',15 ) )
         :AddButton( "ReBldLunchApp",ToolButton():New( 6,,'Re-Build and Launch Application',16 ) )
         :AddButton( "LunchApp",     ToolButton():New( 7,,'Launch Application',17 ) )
         :AddButton( "SingSource",   ToolButton():New( 8,,'Compile Single Source',18 ) )
         :AddButton( "AllSources",   ToolButton():New( 9,,'Compile All Sources',19 ) )
         :AddButton( "LinkOnly",     ToolButton():New(10,,'Link Only',20 ) )
         :AddButton( "CompPPO",      ToolButton():New(11,,'Compile to PPO',21 ) )
         :AddButton( "View",         ToolButton():New(12,,'View',22 ) )
         :AddButton( "Files",        ToolButton():New(13,,'Files',23) )
               
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
      With Object :GetObj("Tabs"):GetObj("StdTab") //:Tabs:StdTab
         With Object :Add( 'TabBand', TRebar():New( oApp:MainFrame:GetObj("Rebar"):GetObj("Tabs"):GetObj("StdTab") ) )
            :SetStyle( WS_BORDER, .F. )
            With Object :Add( 'StdTools', TToolBar():New( oApp:MainFrame:GetObj("Rebar"):GetObj("Tabs"):GetObj("StdTab"):GetObj("TabBand"), 444, 14, , , 28, 28, 20, 20 ) )
               :SetStyle( TBSTYLE_CHECKGROUP )
               aStdTab := { '', 'Frames', 'MainMenu', 'PopupMenu', 'Label', 'Edit', 'Memo', 'Button', ;
                                'CheckBox', 'RadioButton', 'ListBox', 'ComboBox', 'ScrollBar', 'GroupBox', ;
                                'RadioGroup', 'Panel', 'ActionList' }
               for n:=0 to 16
                   oTool := ToolButton():New( n,,aStdTab[n+1], n+100 )
                   oTool:Action := {|oItem| oApp:SubForm:OnMenuCommand(oItem) }
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

METHOD MainStatusBar() CLASS MainFrame
   ::Add('Status',  TStatusBar():New( oApp:MainFrame, 'StatusBar', 1001 ) ) 
   ::Status:SetPanels( { 150,380,480,580,-1 } )
   ::Status:SetPanelText( 0, "Visual xHarbour" )
return(self)

//----------------------------------------------------------------------------------------------

CLASS ObjTree FROM TForm
   METHOD New( oParent ) INLINE ::Caption := 'Object Tree',;
                                ::left    := 0,;
                                ::top     := 125,;
                                ::width   := 200,;
                                ::height  := 150,;
                                ::ExStyle := /*WS_EX_CLIENTEDGE + */WS_EX_TOOLWINDOW ,;
                                super:new( oParent )
   // disallow window from being closed
   METHOD OnCloseQuery() INLINE 0
ENDCLASS

//----------------------------------------------------------------------------------------------

CLASS ObjInspect FROM TForm
   METHOD New( oParent ) INLINE ::Caption := 'Object Inspector',;
                                ::left    := 0,;
                                ::top     := 275,;
                                ::width   := 200,;
                                ::height  := 250,;
                                ::ExStyle := /*WS_EX_CLIENTEDGE + */WS_EX_TOOLWINDOW ,;
                                super:new( oParent )
   // disallow window from being closed
   METHOD OnCloseQuery() INLINE 0
   METHOD OnCreate()
   METHOD OnSize(n,x,y)  INLINE  ::GetObj("InspCombo"):Move(,,x,21,.t.),;
                                 ::GetObj("InspTabs"):Move(,25,x,y-25,.t.),nil
ENDCLASS

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
