

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
            :AddPopup('File')

            WITH OBJECT :Popup
               :AddItem( 'Editor'  , 101, {||oApp:CreateForm( 'SubForm', TFormEdit(),oApp:MainFrame ) } )

               :AddItem( 'Button'     , 102, {|oItem| oApp:SubForm:OnMenuCommand(oItem) } )
               :AddItem( 'Edit'       , 103, {|oItem| oApp:SubForm:OnMenuCommand(oItem) } )
               :AddItem( 'ComboBox'   , 104, {|oItem| oApp:SubForm:OnMenuCommand(oItem) } )
               :AddItem( 'Label'      , 105, {|oItem| oApp:SubForm:OnMenuCommand(oItem) } )
               :AddItem( 'RadioButton', 106, {|oItem| oApp:SubForm:OnMenuCommand(oItem) } )
               :AddItem( 'CheckBox'   , 107, {|oItem| oApp:SubForm:OnMenuCommand(oItem) } )
               :AddItem( 'ListBox'    , 108, {|oItem| oApp:SubForm:OnMenuCommand(oItem) } )
               :AddItem( 'StatusBar'  , 109, {|oItem| oApp:SubForm:OnMenuCommand(oItem) } )

            END
         END

         :SetWindowMenu()
      END

//----------------------------------------------------------------------------------------------
//   UNDER CONSTRUCTION
//----------------------------------------------------------------------------------------------

      hImg := ImageList_Create( 20, 20, ILC_COLORDDB+ILC_MASK )
      hBmp := LoadImage( hInstance(), "XMAKE", IMAGE_BITMAP, 0, 0, LR_LOADTRANSPARENT )
      ImageList_AddMasked( hImg, hBmp, RGB( 0, 255, 255 ) )

      :MainFrame:Add('Rebar', TRebar():New( :MainFrame ) )
      
      
      :MainFrame:Rebar:Add( 'Tools', TToolBar():New( :MainFrame:Rebar, 444, 14, , , 32, 32, 24, 24 ) )
      :MainFrame:Rebar:Tools:AddButton( 0, 10,,,,  ,,'New Project' )

      SendMessage( :MainFrame:Rebar:Tools:handle, TB_SETIMAGELIST, 0, hImg )
      SendMessage( :MainFrame:Rebar:Tools:handle, TB_SETBUTTONSIZE, 0, MAKELONG( 26, 26 ) )

      :MainFrame:Rebar:AddBand( NIL, RBBS_GRIPPERALWAYS + RBBS_NOVERT + RBBS_BREAK , :MainFrame:Rebar:Tools:handle, 110, 26, 150 , "", NIL )

//------------------------------------

      :MainFrame:Add('Status',  TStatusBar():New( :MainFrame, 'StatusBar', 1001 ) ) 
      :MainFrame:Status:SetPanels( { 150,380,480,580,-1 } )
      :MainFrame:Status:SetPanelText( 0, "What32 API StatusBar" )
      :MainFrame:Status:SetPanelText( 2, "Enjoy" )

      :Run()
  END

RETURN( nil)

//----------------------------------------------------------------------------------------------

CLASS MainFrame FROM TFrame
   
   METHOD New( oParent ) INLINE ::Caption := 'xHarbour xIde',;
                                ::left    := 0,;
                                ::top     := 0,;
                                ::width   := GetWindowRect(GetDesktopWindow())[3],;
                                ::height  := 100,;
                                super:new( oParent )

   METHOD OnCloseQuery() INLINE if( ::MsgBox( 'Quitting xIDE ?', 'OnCloseQuery', MB_YESNO ) == IDYES,;
                                    PostQuitMessage(0), 0 )

ENDCLASS

//----------------------------------------------------------------------------------------------

CLASS SubForm1 FROM TPanel
   
   METHOD New( oParent )     INLINE ::Caption := 'SubForm1 from TForm', ;
                                    ::left    := 0,;
                                    ::top     := 150,;
                                    ::width   := 500,;
                                    ::height  := 200,;
                                    super:New( oParent )

   METHOD OnCreate()         INLINE ::CreateSub()

   METHOD OnCommand(nwParam) INLINE ::SubCommands( nwParam )

   METHOD CreateSub()
   METHOD SubCommands()
ENDCLASS

//----------------------------------------------------------------------------------------------

METHOD SubCommands( nwParam ) CLASS SubForm1
   if nwParam == 500 
      ::MsgBox( 'HI FROM TBUTTON')
   endif
return(nil)

//----------------------------------------------------------------------------------------------

METHOD CreateSub() CLASS SubForm1

   local oBtn
   local oMask
   local xRet

   ::WindowMenu := TMenu():New()

   ::WindowMenu:AddPopup( 'popup 1' )
   
      ::WindowMenu:Popup:AddItem( 'item 100', 100, {|| ::MsgBox( 'HI FROM SUBFORM1')})
      ::WindowMenu:Popup:AddItem( 'item 101', 101)
      
   ::SetWindowMenu()


   ::Add('Status',   TStatusBar():New( self, 'StatusBar', 1000 ) ) 

   ::Add('TestButton',  TButton():New( self, 'This is a BUTTON',           500,   0,  0, 200, 100 ) ) 
   ::Add('TestEdit',      TEdit():New( self, 'This is an edit control',    501, 210,  0, 200,  20 ) )
   ::Add('TestCombo', TComboBox():New( self, 502, 210, 30, 200, 100 ) )
   ::Add('TestText',    TStatic():New( self, 'This is a Static control',   503, 210, 55, 200,  20 ) )
   ::Add('TestRadio',    TRadio():New( self, 'This is a Radio Button',     504, 210, 80, 200,  20 ) )
   ::Add('TestCheck',    TCheck():New( self, 'This is a Check Button',     505, 210,105, 200,  20 ) )

   ::TestButton:SetFocus()


return( super:OnCreate() )

//----------------------------------------------------------------------------------------------


