

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
   local oTool, oRebar, n
   LOCAL hImg1,hImg2,hBmp,aStdTab
   
   oApp := Application():Initialize()
   WITH OBJECT oApp

      WITH OBJECT :CreateFrame( 'MainFrame', MainFrame() )
         :WindowMenu := TMenu():New()
         :SetStyle( WS_THICKFRAME, .F. )
         :SetStyle( WS_MAXIMIZEBOX, .F. )
         WITH OBJECT :WindowMenu
            :AddPopup('&Test')

            WITH OBJECT :Popup
               :AddItem( 'Editor'     , 101, {||oApp:CreateForm( 'SubForm', TFormEdit(),oApp:MainFrame ) } )
               :AddSeparator()
               :AddItem( 'Button'     , 102, {|oItem| oApp:SubForm:OnMenuCommand(oItem) } )
               :AddItem( 'Edit'       , 103, {|oItem| oApp:SubForm:OnMenuCommand(oItem) } )
               :AddItem( 'ComboBox'   , 104, {|oItem| oApp:SubForm:OnMenuCommand(oItem) } )
               :AddItem( 'Label'      , 105, {|oItem| oApp:SubForm:OnMenuCommand(oItem) } )
               :AddItem( 'RadioButton', 106, {|oItem| oApp:SubForm:OnMenuCommand(oItem) } )
               :AddItem( 'CheckBox'   , 107, {|oItem| oApp:SubForm:OnMenuCommand(oItem) } )
               :AddItem( 'ListBox'    , 108, {|oItem| oApp:SubForm:OnMenuCommand(oItem) } )
               :AddItem( 'StatusBar'  , 109, {|oItem| oApp:SubForm:OnMenuCommand(oItem) } )
               :AddItem( 'TabControl' , 110, {|oItem| oApp:SubForm:OnMenuCommand(oItem) } )
               
            END
         END
         :SetWindowMenu()
         :WindowMenu:Gray( 102 )
         :WindowMenu:Gray( 103 )
         :WindowMenu:Gray( 104 )
         :WindowMenu:Gray( 105 )
         :WindowMenu:Gray( 106 )
         :WindowMenu:Gray( 107 )
         :WindowMenu:Gray( 108 )
         :WindowMenu:Gray( 109 )
         :WindowMenu:Gray( 110 )

         WITH OBJECT :Add('Rebar', TRebar():New( oApp:MainFrame ) )
            WITH OBJECT :Add( 'Tools', TToolBar():New( oApp:MainFrame:Rebar, 444, 15, , , 26, 26, 20, 20 ) )
               :AddButton( 0, 10,,,,  ,,'New Project' )
               :AddButton( 1, 11,,,,  ,,'Open Project' )
               :AddButton( 2, 12,,,,  ,,'Properties' )
               :AddButton( 3, 13,,,,  ,,'Build Application' )
               :AddButton( 4, 14,,,,  ,,'Build and Launch Application' )
               :AddButton( 5, 15,,,,  ,,'Re-Build Application' )
               :AddButton( 6, 16,,,,  ,,'Re-Build and Launch Application' )
               :AddButton( 7, 17,,,,  ,,'Launch Application' )
               :AddButton( 8, 18,,,,  ,,'Compile Single Source' )
               :AddButton( 9, 19,,,,  ,,'Compile All Sources' )
               :AddButton(10, 20,,,,  ,,'Link Only' )
               :AddButton(11, 21,,,,  ,,'Compile to PPO' )
               :AddButton(12, 22,,,,  ,,'View' )
               :AddButton(14, 23,,,,  ,,'Files')

               // ----------------------------------------------------   set imagelist
               hImg1:= ImageList_Create( 20, 20, ILC_COLORDDB+ILC_MASK )
               hBmp := LoadImage( hInstance(), "XMAKE", IMAGE_BITMAP, 0, 0, LR_LOADTRANSPARENT )
               ImageList_AddMasked( hImg1, hBmp, RGB( 0, 255, 255 ) )
               DeleteObject(hBmp)
               SendMessage( :handle, TB_SETIMAGELIST, 0, hImg1 )
               //---------------------------------------------------------------------
            END
            :AddBand( NIL, RBBS_GRIPPERALWAYS + RBBS_NOVERT + RBBS_BREAK , :Tools:handle, 110, 26, 150 , "", NIL )

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
            :AddBand( NIL, RBBS_GRIPPERALWAYS + RBBS_NOVERT + RBBS_BREAK, :Tabs:handle, 650, 60, , "", NIL )
            :Tabs:Configure()
            
            WITH OBJECT :Tabs:Tabs[1]
               WITH OBJECT :Add( 'TabBand', TRebar():New( oApp:MainFrame:Rebar:Tabs:Tabs[1] ) )
                  :SetStyle( WS_BORDER, .F. )
                  WITH OBJECT :Add( 'TabTools', TToolBar():New( oApp:MainFrame:Rebar:Tabs:Tabs[1]:TabBand, 444, 14, , , 26, 26, 20, 20 ) )

                     aStdTab := { '', 'Frames', 'MainMenu', 'PopupMenu', 'Label', 'Edit', 'Memo', 'Button', ;
                                  'CheckBox', 'RadioButton', 'Listbox', 'ComboBox', 'ScrollBar', 'GroupBox', ;
                                  'RadioGroup', 'Panel', 'ActionList' }
                     for n:=0 to 16
                         :AddButton(n,100+n,,TBSTYLE_BUTTON + TBSTYLE_CHECKGROUP,,,,aStdTab[n+1] )
                     next
                     
                     // ----------------------------------------------------   set imagelist
                     hImg2:= ImageList_Create( 28, 28, ILC_COLORDDB+ILC_MASK )
                     hBmp := LoadImage( hInstance(), "STDTAB", IMAGE_BITMAP, 0, 0, LR_LOADTRANSPARENT )
                     ImageList_AddMasked( hImg2, hBmp, RGB( 0, 255, 255 ) )
                     DeleteObject(hBmp)
                     SendMessage( :handle, TB_SETIMAGELIST, 0, hImg2 )
                     //---------------------------------------------------------------------
                  END
                  :AddBand( NIL, RBBS_NOVERT, :TabTools:handle, 100, 34,  , "", NIL )
               END
            END
         END
         
         WITH OBJECT :Add('Status',  TStatusBar():New( oApp:MainFrame, 'StatusBar', 1001 ) ) 
            :SetPanels( { 150,380,480,580,-1 } )
            :SetPanelText( 0, "What32 API StatusBar" )
            :SetPanelText( 2, "Enjoy" )
         END
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
                                ::height  := 160,;
                                super:new( oParent )

   METHOD OnCloseQuery() INLINE if( ::MsgBox( 'Quitting xIDE ?', 'OnCloseQuery', MB_YESNO ) == IDYES,;
                                    PostQuitMessage(0), 0 )

ENDCLASS

//----------------------------------------------------------------------------------------------


