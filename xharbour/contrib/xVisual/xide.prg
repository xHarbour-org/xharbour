

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
   local oTool, oRebar
   LOCAL hImg,hBmp
   
   oApp := Application():Initialize()
   WITH OBJECT oApp

      WITH OBJECT :CreateFrame( 'MainFrame', MainFrame() )
         :WindowMenu := TMenu():New()
         :SetStyle( WS_THICKFRAME, .F. )
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

//----------------------------------------------------------------------------------------------
//   IMAGELIST CLASS UNDER CONSTRUCTION
//----------------------------------------------------------------------------------------------

         hImg := ImageList_Create( 20, 20, ILC_COLORDDB+ILC_MASK )
         hBmp := LoadImage( hInstance(), "XMAKE", IMAGE_BITMAP, 0, 0, LR_LOADTRANSPARENT )
         ImageList_AddMasked( hImg, hBmp, RGB( 0, 255, 255 ) )

         WITH OBJECT :Add('Rebar', TRebar():New( oApp:MainFrame ) )
            WITH OBJECT :Add( 'Tools', TToolBar():New( oApp:MainFrame:Rebar, 444, 14, , , 26, 26, 20, 20 ) )
               :AddButton( 0, 500,,,,  ,,'New Project' )
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
               SendMessage( :handle, TB_SETIMAGELIST, 0, hImg )
            END
            :AddBand( NIL, RBBS_GRIPPERALWAYS + RBBS_NOVERT + RBBS_BREAK , :Tools:handle, 110, 26, 150 , "", NIL )

            WITH OBJECT :Add( 'Tabs', TTabControl():New( oApp:MainFrame:Rebar, 445,  0,  0, 0,  0 ) )
               :AddTab( "Standard")
               :AddTab( "Aditional")
               :AddTab( "Win32")
               :AddTab( "System")
               :AddTab( "Internet")
               :AddTab( "Dialogs")
               :AddTab( "Win 3.1")
               :AddTab( "Samples")
               :AddTab( "Activex")
               :Configure()
            END
            :AddBand( NIL, RBBS_GRIPPERALWAYS + RBBS_NOVERT + RBBS_BREAK, :Tabs:handle, 650, 60, , "", NIL )

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


