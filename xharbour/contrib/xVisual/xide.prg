#include "windows.ch"
#include "wingdi.ch"
#include "common.ch"
#include "hbclass.ch"
#include "debug.ch"
#include "what32.ch"
#Include "toolbar.ch"
#Include "winlview.ch"

static oApp

//-------------------------------------------------------------------------------------------

FUNCTION Main
   local n, oTool, oSplash
   LOCAL hImg1,hImg2,hBmp,aStdTab

   oApp := Application():Initialize()

   // splash screen
   oSplash := TSplash():New( oApp, "visual_xharbour.bmp", 5000 )

   WITH OBJECT oApp
      WITH OBJECT :CreateFrame( 'MainFrame', MainFrame() )
         :WindowMenu := TMenu():New()
         :SetStyle( WS_THICKFRAME, .F. )
         :SetStyle( WS_MAXIMIZEBOX, .F. )
         // add the menu
         WITH OBJECT :WindowMenu
            :AddPopup('&Test')
            WITH OBJECT :Popup
               :AddItem( 'Editor'     , 101, {||oApp:CreateForm( 'SubForm', TFormEdit(),oApp:MainFrame ) } )
               :AddSeparator()
               :AddItem( 'Exit'       , 200, {||oApp:MainFrame:PostMessage(WM_SYSCOMMAND,SC_CLOSE)} )
            END
         END
         :SetWindowMenu()
         // add the rebar
         WITH OBJECT :Add('Rebar', TRebar():New( oApp:MainFrame ) )
            // add the xmake toolbar
            WITH OBJECT :Add( 'Tools', TToolBar():New( oApp:MainFrame:Rebar, 444, 15, , , 26, 26, 20, 20 ))
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
            END
            :AddBand( NIL, RBBS_GRIPPERALWAYS + RBBS_NOVERT , :Tools:handle, 200, 52, 200, "", NIL )

            // add the TabControl on the Rebarband
            WITH OBJECT :Add( 'Tabs', TTabControl():New( oApp:MainFrame:Rebar, 445,  0,  0,  0,  0) )
               :AddTab( "Standard" )
               :AddTab( "Aditional" )
               :AddTab( "Win32" )
               :AddTab( "System" )
               :AddTab( "Internet" )
               :AddTab( "Dialogs" )
               :AddTab( "Win 3.1" )
               :AddTab( "Samples" )
               :AddTab( "Activex" )
            END
            :AddBand( NIL, RBBS_GRIPPERALWAYS + RBBS_NOVERT , :Tabs:handle, 550, 56, , "", NIL )
            :Tabs:Configure()
            
            // sets the controls toolbar on the TabControl

            WITH OBJECT :Tabs:Tabs[1]
               WITH OBJECT :Add( 'TabBand', TRebar():New( oApp:MainFrame:Rebar:Tabs:Tabs[1] ) )
                  :SetStyle( WS_BORDER, .F. )
                  WITH OBJECT :Add( 'StdTools', TToolBar():New( oApp:MainFrame:Rebar:Tabs:Tabs[1]:TabBand, 444, 14, , , 28, 28, 20, 20 ) )
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
                  END

                  :AddBand( NIL, RBBS_NOVERT, :StdTools:handle, 100, 30,  , "", NIL )
                  :StdTools:DisableAll()
                  
                  //--------- sets a QUICK access to the control
                  oApp:MainFrame:SetLink( 'StdBar', :StdTools )
               END
            END

         END // rebar
         // add the main status bar
         WITH OBJECT :Add('Status',  TStatusBar():New( oApp:MainFrame, 'StatusBar', 1001 ) ) 
            :SetPanels( { 150,380,480,580,-1 } )
            :SetPanelText( 0, "What32 API StatusBar" )
            :SetPanelText( 2, "Enjoy" )
         END
         
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
ENDCLASS

//----------------------------------------------------------------------------------------------

CLASS ObjTree FROM TForm
   METHOD New( oParent ) INLINE ::Caption := 'Object Tree',;
                                ::left    := 0,;
                                ::top     := 125,;
                                ::width   := 200,;
                                ::height  := 150,;
                                ::ExStyle := WS_EX_CLIENTEDGE + WS_EX_TOOLWINDOW ,;
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
                                ::ExStyle := WS_EX_CLIENTEDGE + WS_EX_TOOLWINDOW ,;
                                super:new( oParent )
   // disallow window from being closed
   METHOD OnCloseQuery() INLINE 0
   METHOD OnCreate()
   METHOD OnSize(n,x,y)  INLINE ::InspTabs:Move(,,x,y,.t.),nil
ENDCLASS

METHOD OnCreate() CLASS ObjInspect
  local aRect := ::ClientRect()
  ::Add( 'InspTabs', TTabControl():New( self, 555,  0,  0, aRect[3], aRect[4]) )
  ::InspTabs:AddTab( "Properties",'Test' )
  ::InspTabs:AddTab( "Events" )
  ::InspTabs:Configure()
return( super:OnCreate() )

//----------------------------------------------------------------------------------------------

