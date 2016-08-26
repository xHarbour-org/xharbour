/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// CommonDialogs.prg                                                                                    *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "vxh.ch"
#include "debug.ch"
#include "commdlg.ch"

#define BIF_NONEWFOLDERBUTTON 0x0200
#define LPUNKNOWN        -4
#define LPPRINTPAGERANGE -4
#define HPROPSHEETPAGE   -4
#define OFN_EX_NOPLACESBAR         0x00000001


CLASS CommonDialogs INHERIT Component
   DATA Style EXPORTED
   METHOD SetStyle()
ENDCLASS

METHOD SetStyle( nStyle, lAdd ) CLASS CommonDialogs
   DEFAULT lAdd TO .T.
   IF lAdd
      ::Style := (::Style | nStyle)
    ELSE
      ::Style := (::Style & NOT( nStyle ))
   ENDIF
RETURN self

//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------

CLASS ColorDialog INHERIT CommonDialogs
   PROPERTY FullOpen        SET ::SetStyle( CC_FULLOPEN, v )        DEFAULT .T.
   PROPERTY AnyColor        SET ::SetStyle( CC_ANYCOLOR, v )        DEFAULT .T.
   PROPERTY SolidColor      SET ::SetStyle( CC_SOLIDCOLOR, v )      DEFAULT .T.
   PROPERTY PreventFullOpen SET ::SetStyle( CC_PREVENTFULLOPEN, v ) DEFAULT .F.
   PROPERTY ShowHelp        SET ::SetStyle( CC_SHOWHELP, v )        DEFAULT .F.
   PROPERTY Color           DEFAULT RGB(0,0,0)

   DATA Colors          EXPORTED INIT {}
   DATA SysDefault      EXPORTED

   METHOD Init() CONSTRUCTOR
   METHOD Show()
ENDCLASS

METHOD Init( oParent ) CLASS ColorDialog
   ::Style := (CC_ANYCOLOR | CC_RGBINIT | CC_SOLIDCOLOR | CC_FULLOPEN ) //| CC_ENABLEHOOK
   ::__xCtrlName := "ColorDialog"
   ::ClsName     := "ColorDialog"
   ::ComponentType := "CommonDialog"
   Super:Init( oParent )
RETURN Self

METHOD Show() CLASS ColorDialog
   LOCAL lRet
   lRet := _ChooseColor( ::Owner:hWnd, @::Color, ::Application:CustomColors, ::Style )//, "VXHCOLORDIALOGPROC" )
RETURN lRet

//FUNCTION VXHCOLORDIALOGPROC( hWnd, nMsg, nwParam, nlParam )
//RETURN 1

//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------

CLASS FolderBrowserDialog INHERIT CommonDialogs
   PROPERTY Description                                                                  DEFAULT ""
   PROPERTY RootFolder                                                                   DEFAULT __GetSystem():RootFolders:Desktop
   PROPERTY SelectedPath                                                                 DEFAULT ""
   PROPERTY ShowNewFolderButton SET ::SetShowNewFolderButton( BIF_NONEWFOLDERBUTTON, v ) DEFAULT .T.

   DATA SelectedId      INIT 0

   METHOD Init() CONSTRUCTOR
   METHOD Show()
   METHOD BrowseForFolderCallBack()
   METHOD SetShowNewFolderButton()
ENDCLASS

METHOD Init( oParent ) CLASS FolderBrowserDialog
   ::Style := (BIF_NEWDIALOGSTYLE | BIF_BROWSEINCLUDEURLS)
   ::__xCtrlName := "FolderBrowserDialog"
   ::ClsName     := "FolderBrowserDialog"
   ::ComponentType := "CommonDialog"
   Super:Init( oParent )
RETURN Self

METHOD Show() CLASS FolderBrowserDialog
   LOCAL pCallBack := WinCallBackPointer( HB_ObjMsgPtr( Self, "BrowseForFolderCallBack" ), Self )
   ::SelectedPath := SHBrowseForFolder( ::Owner:hWnd, ::Description, ::Style, ::RootFolder, pCallBack, @::SelectedId )
   VXH_FreeCallBackPointer( pCallBack )
RETURN ::SelectedPath

METHOD BrowseForFolderCallBack( hWnd, nMsg, lp ) CLASS FolderBrowserDialog
   LOCAL cBuffer
   SWITCH nMsg
      CASE BFFM_INITIALIZED
         SendMessage( hWnd, BFFM_SETSELECTION, 1, ::SelectedPath )
         EXIT

      CASE BFFM_SELCHANGED
         cBuffer := SHGetPathFromIDList( lp )
         SendMessage( hWnd, BFFM_SETSTATUSTEXT, 0, cBuffer )
   END
RETURN 0

METHOD SetShowNewFolderButton( nStyle, lAdd ) CLASS FolderBrowserDialog
   DEFAULT lAdd TO .T.
   IF !lAdd
      ::Style := (::Style | nStyle)
    ELSE
      ::Style := (::Style & NOT( nStyle ))
   ENDIF
RETURN self

//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------

CLASS OpenFileDialog INHERIT CommonDialogs
   PROPERTY CheckFileExists  SET ::SetStyle( OFN_FILEMUSTEXIST, v )           DEFAULT .T.
   PROPERTY CheckPathExists  SET ::SetStyle( OFN_PATHMUSTEXIST, v )           DEFAULT .T.
   PROPERTY MultiSelect      SET ::SetStyle( OFN_ALLOWMULTISELECT, v )        DEFAULT .T.
   PROPERTY DeferenceLinks   SET ::__SetInvStyle( OFN_NODEREFERENCELINKS, v ) DEFAULT .T.
   PROPERTY ReadOnlyChecked  SET ::SetStyle( OFN_READONLY, v )                DEFAULT .F.
   PROPERTY RestoreDirectory SET ::SetStyle( OFN_NOCHANGEDIR, v )             DEFAULT .F.
   PROPERTY ShowHelp         SET ::SetStyle( OFN_SHOWHELP, v )                DEFAULT .F.
   PROPERTY ShowReadOnly     SET ::__SetInvStyle( OFN_HIDEREADONLY, v )       DEFAULT .F.

   PROPERTY AddExtension     DEFAULT .T.
   PROPERTY CheckFileExists  DEFAULT .T.
   PROPERTY FileName         DEFAULT ""
   PROPERTY Filter           DEFAULT ""
   PROPERTY DefaultExt
   PROPERTY FilterIndex      DEFAULT 1
   PROPERTY InitialDirectory
   PROPERTY Title
   PROPERTY ShowPlacesBar    DEFAULT .T.

   DATA __PrevName    PROTECTED
   DATA __PrevFilter  PROTECTED
   METHOD Init() CONSTRUCTOR
   METHOD Show()
   METHOD __SetInvStyle( n, l ) INLINE ::SetStyle( n, !l )
ENDCLASS

METHOD Init( oParent ) CLASS OpenFileDialog
   ::Style := (OFN_EXPLORER | OFN_ALLOWMULTISELECT | OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY)
   ::__xCtrlName := "OpenFileDialog"
   ::ClsName     := "OpenFileDialog"
   ::ComponentType := "CommonDialog"
   Super:Init( oParent )
   IF ::DesignMode
      ::FileName := ::Name
   ENDIF
RETURN Self

METHOD Show() CLASS OpenFileDialog
   LOCAL n, cFilter
   LOCAL ofn := (struct OPENFILENAME)

   IF VALTYPE( ::FileName ) == "A"
      ::FileName := ::__PrevName
   ENDIF
   IF ::__PrevFilter != NIL
      ::FilterIndex := ::__PrevFilter
   ENDIF

   ::__PrevName   := ::FileName
   ::__PrevFilter := ::FilterIndex

   DEFAULT ::Title TO ::Owner:Caption

   ofn:hInstance       := ::AppInstance
   ofn:nMaxFile        := 8192 + 1
   ofn:hwndOwner       := ::Owner:hWnd
   ofn:lpstrInitialDir := ::InitialDirectory
   ofn:lpstrFile       := PadR( ::FileName, 8192 )
   ofn:Flags           := ::Style
   ofn:nFilterIndex    := ::FilterIndex
   ofn:lpstrDefExt     := ::DefaultExt
   ofn:lpstrTitle      := ::Title

   cFilter := ::Filter
   IF VALTYPE( cFilter ) == "A"
      cFilter := ""
      FOR n := 1 TO LEN( ::Filter )
          cFilter += ::Filter[n][1] + chr( 0 ) + ::Filter[n][2] + chr( 0 )
      NEXT
    ELSE
      IF cFilter[-1] != "|"
         cFilter += "|"
      ENDIF
      cFilter := STRTRAN( cFilter, "|", chr( 0 ) )
   ENDIF

   ofn:lpstrFilter := cFilter
   ofn:FlagsEx     := 0
   IF !::ShowPlacesBar
      ofn:FlagsEx  := OFN_EX_NOPLACESBAR
      ofn:Flags := (ofn:Flags | OFN_ENABLEHOOK)
   ENDIF
   IF GetOpenFileName( @ofn )
      ::FilterIndex := ofn:nFilterIndex
      IF ::MultiSelect
         ::FileName := hb_aTokens( ofn:lpstrFile, CHR(0) )
         ADEL( ::FileName, LEN(::FileName), .T. )
         ADEL( ::FileName, LEN(::FileName), .T. )
         IF LEN( ::FileName ) == 1
            AADD( ::FileName, SubStr( ::FileName[1], RAT( "\", ::FileName[1] ) + 1 ) )
            ::FileName[1] := SubStr( ::FileName[1], 1, RAT( "\", ::FileName[1] ) - 1 )
         ENDIF
       ELSE
         ::FileName := Left( ofn:lpstrFile, At( Chr(0), ofn:lpstrFile ) - 1 )
      ENDIF
      RETURN .T.
   ENDIF
RETURN .F.

//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------

CLASS SaveFileDialog INHERIT CommonDialogs
   PROPERTY AddExtension                                                      DEFAULT .T.
   PROPERTY DefaultExt
   PROPERTY FileName                                                          DEFAULT ""
   PROPERTY Filter                                                            DEFAULT ""
   PROPERTY FilterIndex                                                       DEFAULT 1
   PROPERTY InitialDirectory
   PROPERTY Title
   PROPERTY ShowPlacesBar                                                     DEFAULT .T.
   PROPERTY OverwritePrompt  SET ::SetStyle( OFN_OVERWRITEPROMPT, v )         DEFAULT .T.
   PROPERTY CheckPathExists  SET ::SetStyle( OFN_PATHMUSTEXIST, v )           DEFAULT .T.
   PROPERTY CheckFileExists  SET ::SetStyle( OFN_FILEMUSTEXIST, v )           DEFAULT .F.
   PROPERTY CreatePrompt     SET ::SetStyle( OFN_CREATEPROMPT, v )            DEFAULT .F.
   PROPERTY DeferenceLinks   SET ::__SetInvStyle( OFN_NODEREFERENCELINKS, v ) DEFAULT .T.
   PROPERTY RestoreDirectory SET ::SetStyle( OFN_NOCHANGEDIR, v )             DEFAULT .F.
   PROPERTY ShowHelp         SET ::SetStyle( OFN_SHOWHELP, v )                DEFAULT .F.
   PROPERTY PathMustExist    SET ::SetStyle( OFN_PATHMUSTEXIST, v )           DEFAULT .T.
   PROPERTY HideReadOnly     SET ::SetStyle( OFN_HIDEREADONLY, v )            DEFAULT .T.


   DATA __PrevName    PROTECTED
   DATA __PrevFilter  PROTECTED
   DATA FileExtension EXPORTED

   METHOD Init() CONSTRUCTOR
   METHOD Show()
   METHOD __SetInvStyle( n, l ) INLINE ::SetStyle( n, !l )
ENDCLASS

METHOD Init( oParent ) CLASS SaveFileDialog
   ::Style := (OFN_EXPLORER | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_NODEREFERENCELINKS | OFN_OVERWRITEPROMPT)
   ::__xCtrlName := "SaveFileDialog"
   ::ClsName     := "SaveFileDialog"
   ::ComponentType := "CommonDialog"
   Super:Init( oParent )
   IF ::DesignMode
      ::FileName := ::Name
   ENDIF
RETURN Self

METHOD Show() CLASS SaveFileDialog
   LOCAL n, cFilter, aFilter, cExt
   LOCAL ofn := (struct OPENFILENAME)

   IF VALTYPE( ::FileName ) == "A"
      ::FileName := ::__PrevName
   ENDIF

   IF ::__PrevFilter != NIL
      ::FilterIndex := ::__PrevFilter
   ENDIF

   ::__PrevName   := ::FileName
   ::__PrevFilter := ::FilterIndex

   DEFAULT ::Title TO ::Owner:Caption

   ofn:hInstance       := ::AppInstance
   ofn:nMaxFile        := 8192 + 1
   ofn:hwndOwner       := ::Owner:hWnd
   ofn:lpstrInitialDir := ::InitialDirectory
   ofn:lpstrFile       := PadR( ::FileName, 8192 )
   ofn:Flags           := ::Style
   ofn:nFilterIndex    := ::FilterIndex
   ofn:lpstrDefExt     := ::DefaultExt
   ofn:lpstrTitle      := ::Title

   IF ::Filter[-1] != "|"
      ::Filter += "|"
   ENDIF

   cFilter := STRTRAN( ::Filter, "|", chr( 0 ) )

   ofn:lpstrFilter := cFilter

   IF !::ShowPlacesBar
      ofn:FlagsEx  := OFN_EX_NOPLACESBAR
      ofn:Flags := (ofn:Flags | OFN_ENABLEHOOK)
   ENDIF

   IF GetSaveFileName( @ofn )
      ::FileName := Left( ofn:lpstrFile, At( Chr(0), ofn:lpstrFile ) - 1 )
      ::FilterIndex := ofn:nFilterIndex

      IF ::AddExtension .AND. ofn:nFileExtension == 0
         aFilter := __str2a( ::Filter, "|" )
         IF LEN( aFilter ) > 0 .AND. ::FilterIndex > 0
            cExt := SUBSTR( aFilter[ ::FilterIndex*2 ], 3 )
            n := AT( ";", cExt )
            ::FileExtension := LOWER( SUBSTR( cExt, 1, IIF( n > 0, n-1, LEN( cExt ) ) ) )
            ::FileName += ( "." + ::FileExtension )
         ENDIF
       ELSEIF ofn:nFileExtension > 0
         ::FileExtension := LOWER( SUBSTR( ::FileName, ofn:nFileExtension ) )
      ENDIF
      RETURN .T.
   ENDIF
RETURN .F.

//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------

CLASS PrintDialog INHERIT CommonDialogs
   PROPERTY AllowCurrentPage SET ::__SetInvStyle( PD_NOCURRENTPAGE, v )      DEFAULT .F.
   PROPERTY AllowPrintFile   SET ::__SetInvStyle( PD_DISABLEPRINTTOFILE, v ) DEFAULT .F.
   PROPERTY AllowSelection   SET ::__SetInvStyle( PD_NOSELECTION, v )        DEFAULT .F.
   PROPERTY AllowSomePages   SET ::__SetInvStyle( PD_NOPAGENUMS, v )         DEFAULT .F.
   PROPERTY FromPage                                                         DEFAULT 0
   PROPERTY ToPage                                                           DEFAULT 0
   PROPERTY Copies                                                           DEFAULT 1

   DATA PrinterName EXPORTED
   DATA Default     EXPORTED INIT .F.
   DATA DriverName  EXPORTED
   DATA Port        EXPORTED

   METHOD Init() CONSTRUCTOR
   METHOD Show()
   METHOD __SetInvStyle( n, l ) INLINE ::SetStyle( n, !l )
ENDCLASS

METHOD Init( oParent ) CLASS PrintDialog
   ::Style := (PD_NOCURRENTPAGE | PD_DISABLEPRINTTOFILE | PD_NOSELECTION | PD_NOPAGENUMS)
   ::__xCtrlName := "PrintDialog"
   ::ClsName     := "PrintDialog"
   ::ComponentType := "CommonDialog"
   Super:Init( oParent )
RETURN Self

METHOD Show() CLASS PrintDialog
   LOCAL pd, nPtr, advnm, dn

   pd := (struct PRINTDLG)
   pd:hwndOwner           := ::Owner:hWnd
   pd:hInstance           := NIL
   pd:Flags               := ::Style
   pd:nFromPage           := ::FromPage
   pd:nToPage             := ::ToPage

   pd:nMinPage            := 0
   pd:nMaxPage            := 1000
   pd:nCopies             := 1

   IF PrintDlg( @pd )
      IF ! EMPTY( pd:hDevNames )
         IF ( nPtr  := GlobalLock( pd:hDevNames ) ) <> 0
            dn := (struct DEVNAMES)
            dn:Buffer( Peek( nPtr, dn:sizeof ) )
            advnm := Array( 4 )
            ::DriverName  := Peek( nPtr + dn:wDriverOffset )
            ::PrinterName := Peek( nPtr + dn:wDeviceOffset )
            ::Port        := Peek( nPtr + dn:wOutputOffset )
            ::Default     := dn:wDefault == 1
            GlobalUnlock( pd:hDevNames )
         ENDIF
         GlobalFree( pd:hDevNames )
      ENDIF

      ::FromPage  := pd:nFromPage
      ::ToPage    := pd:nToPage
      ::Copies    := pd:nCopies
      RETURN .T.
   ENDIF
RETURN .F.

//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------

CLASS FontDialog INHERIT CommonDialogs
   PROPERTY Font
   DATA ForeColor EXPORTED

   ACCESS Color INLINE ::ForeColor

   METHOD Init() CONSTRUCTOR
   METHOD Show()
ENDCLASS

METHOD Init( oParent ) CLASS FontDialog
   ::Style          := (CF_SCREENFONTS | CF_INITTOLOGFONTSTRUCT | CF_EFFECTS)
   ::__xCtrlName    := "FontDialog"
   ::ClsName        := "FontDialog"
   ::ComponentType  := "CommonDialog"
   Super:Init( oParent )

   ::Font := Font( Self )
   IF ::DesignMode
      ::Font:Create()
   ENDIF
RETURN Self

METHOD Show( oParent ) CLASS FontDialog
   LOCAL lRet
   ::Font:Create()
   lRet := ::Font:Choose( IIF( oParent != NIL, oParent, ::Owner ),, ::Style ) != NIL
   ::Font:Delete()
RETURN lRet

//------------------------------------------------------------------------------------------------
CLASS FontDialogFont
   PROPERTY Name
   DATA Owner       EXPORTED
   DATA ClsName     EXPORTED INIT "FontDialogFont"
   ACCESS Parent INLINE ::Owner
   ACCESS DesignMode INLINE IIF( ::Owner != NIL, ::Owner:DesignMode, .F. )

   METHOD Init() CONSTRUCTOR
   METHOD Choose() INLINE ::Owner:Show()
ENDCLASS

METHOD Init( oOwner ) CLASS FontDialogFont
   ::Owner     := oOwner
   __SetInitialValues( Self )
RETURN Self

//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------

CLASS PageSetup INHERIT CommonDialogs
   PROPERTY ReturnDefault  SET ::SetStyle( PSD_RETURNDEFAULT, v )  DEFAULT .F.
   PROPERTY DisableMargins SET ::SetStyle( PSD_DISABLEMARGINS, v ) DEFAULT .T. PROTECTED
   PROPERTY PaperSize                                              DEFAULT DMPAPER_LETTER
   PROPERTY Orientation                                            DEFAULT DMORIENT_PORTRAIT

   DATA EnumPaperSize   EXPORTED
   DATA EnumOrientation EXPORTED INIT { { "Portrait", "Landscape" }, {__GetSystem():PageSetup:Portrait, __GetSystem():PageSetup:Landscape} }

   DATA PrinterName  EXPORTED
   DATA Default      EXPORTED INIT .F.
   DATA DriverName   EXPORTED
   DATA Port         EXPORTED

   DATA PageWidth    EXPORTED
   DATA PageHeight   EXPORTED

   DATA LeftMargin   EXPORTED INIT 1000
   DATA TopMargin    EXPORTED INIT 1000
   DATA RightMargin  EXPORTED INIT 1000
   DATA BottomMargin EXPORTED INIT 1000

   DATA psd          EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD Show()
   METHOD __SetInvStyle( n, l ) INLINE ::SetStyle( n, !l )
ENDCLASS

METHOD Init( oParent ) CLASS PageSetup
   LOCAL n
   ::Style := (PSD_DEFAULTMINMARGINS | PSD_MARGINS)
   ::__xCtrlName := "PageSetup"
   ::ClsName     := "PageSetup"
   ::ComponentType := "CommonDialog"
   ::EnumPaperSize := {{},{}}
   ::psd := (struct PAGESETUPDLG)

   FOR n := 1 TO LEN( ::System:PaperSize )
       AADD( ::EnumPaperSize[1], ::System:PaperSize[n][1] )
       AADD( ::EnumPaperSize[2], ::System:PaperSize[n][2] )
   NEXT

   Super:Init( oParent )
RETURN Self

METHOD Show() CLASS PageSetup
   LOCAL nPtr, advnm, dn, nOrientation, nPaperSize, nWidth, nLenght
   nOrientation := ::Orientation
   nPaperSize   := ::PaperSize
   nLenght      := ::PageHeight
   nWidth       := ::PageWidth

   ::psd:hwndOwner       := ::Owner:hWnd
   ::psd:Flags           := ::Style
   ::psd:ptPaperSize:x   := ::PageWidth
   ::psd:ptPaperSize:y   := ::PageHeight

   ::psd:rtMargin:Left   := ::LeftMargin
   ::psd:rtMargin:Top    := ::TopMargin
   ::psd:rtMargin:Right  := ::RightMargin
   ::psd:rtMargin:Bottom := ::BottomMargin

   IF PageSetupDlg( @::psd, @nOrientation, @nPaperSize, @nWidth, @nLenght )
      IF ! EMPTY( ::psd:hDevNames )
         IF ( nPtr  := GlobalLock( ::psd:hDevNames ) ) <> 0
            dn := (struct DEVNAMES)
            dn:Buffer( Peek( nPtr, dn:sizeof ) )
            advnm := Array( 4 )
            ::DriverName  := Peek( nPtr + dn:wDriverOffset )
            ::PrinterName := Peek( nPtr + dn:wDeviceOffset )
            ::Port        := Peek( nPtr + dn:wOutputOffset )
            ::Default     := dn:wDefault == 1
            GlobalUnlock( ::psd:hDevNames )
         ENDIF
         GlobalFree( ::psd:hDevNames )
      ENDIF
      ::Orientation  := nOrientation
      ::PaperSize    := nPaperSize

      ::LeftMargin   := ::psd:rtMargin:Left
      ::TopMargin    := ::psd:rtMargin:Top
      ::RightMargin  := ::psd:rtMargin:Right
      ::BottomMargin := ::psd:rtMargin:Bottom

      ::PageWidth    := nWidth
      ::PageHeight   := nLenght
      RETURN .T.
   ENDIF
RETURN .F.

//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------

#define TDF_ENABLE_HYPERLINKS               0x0001
#define TDF_USE_HICON_MAIN                  0x0002
#define TDF_USE_HICON_FOOTER                0x0004
#define TDF_ALLOW_DIALOG_CANCELLATION       0x0008
#define TDF_USE_COMMAND_LINKS               0x0010
#define TDF_USE_COMMAND_LINKS_NO_ICON       0x0020
#define TDF_EXPAND_FOOTER_AREA              0x0040

#define TDF_EXPANDED_BY_DEFAULT             0x0080
#define TDF_VERIFICATION_FLAG_CHECKED       0x0100
#define TDF_SHOW_PROGRESS_BAR               0x0200
#define TDF_SHOW_MARQUEE_PROGRESS_BAR       0x0400
#define TDF_CALLBACK_TIMER                  0x0800
#define TDF_POSITION_RELATIVE_TO_WINDOW     0x1000
#define TDF_RTL_LAYOUT                      0x2000
#define TDF_NO_DEFAULT_RADIO_BUTTON         0x4000
#define TDF_CAN_BE_MINIMIZED                0x8000

#define TDF_SIZE_TO_CONTENT                 0x1000000

#define TDCBF_OK_BUTTON                     0x0001
#define TDCBF_YES_BUTTON                    0x0002
#define TDCBF_NO_BUTTON                     0x0004
#define TDCBF_CANCEL_BUTTON                 0x0008
#define TDCBF_RETRY_BUTTON                  0x0010
#define TDCBF_CLOSE_BUTTON                  0x0020


CLASS TaskDialog INHERIT CommonDialogs
   DATA __Flags         PROTECTED INIT TDF_SIZE_TO_CONTENT
   DATA __ComBttns      PROTECTED INIT 0

   DATA ButtonPressed           EXPORTED
   DATA RadioButton             EXPORTED
   DATA VerificationFlagChecked EXPORTED

   PROPERTY Width                                                                           DEFAULT 0
   PROPERTY Buttons                                                                         DEFAULT ""
   PROPERTY MainInstruction                                                                 DEFAULT ""
   PROPERTY Content                                                                         DEFAULT ""
   PROPERTY WindowTitle                                                                     DEFAULT ""
   PROPERTY Footer                                                                          DEFAULT ""
   PROPERTY EnableHyperlinks         SET ::__SetFlags( TDF_ENABLE_HYPERLINKS, v )           DEFAULT .F.
   PROPERTY UseIconMain              SET ::__SetFlags( TDF_USE_HICON_MAIN, v )              DEFAULT .F.
   PROPERTY UseIconFooter            SET ::__SetFlags( TDF_USE_HICON_FOOTER, v )            DEFAULT .F.
   PROPERTY AllowDialogCancellation  SET ::__SetFlags( TDF_ALLOW_DIALOG_CANCELLATION, v )   DEFAULT .F.
   PROPERTY UseCommandLinks          SET ::__SetFlags( TDF_USE_COMMAND_LINKS, v )           DEFAULT .F.
   PROPERTY UseCommandLinksNoIcon    SET ::__SetFlags( TDF_USE_COMMAND_LINKS_NO_ICON, v )   DEFAULT .F.
   PROPERTY ExpandFooterArea         SET ::__SetFlags( TDF_EXPAND_FOOTER_AREA, v )          DEFAULT .F.
   PROPERTY ExpandedByDefault        SET ::__SetFlags( TDF_EXPANDED_BY_DEFAULT, v )         DEFAULT .F.
   PROPERTY VerificationFlagChecked  SET ::__SetFlags( TDF_VERIFICATION_FLAG_CHECKED, v )   DEFAULT .F.
   PROPERTY ShowProgressBar          SET ::__SetFlags( TDF_SHOW_PROGRESS_BAR, v )           DEFAULT .F.
   PROPERTY ShowMarqueeProgressBar   SET ::__SetFlags( TDF_SHOW_MARQUEE_PROGRESS_BAR, v )   DEFAULT .F.
   PROPERTY CallbackTimer            SET ::__SetFlags( TDF_CALLBACK_TIMER, v )              DEFAULT .F.
   PROPERTY PositionRelativeToWindow SET ::__SetFlags( TDF_POSITION_RELATIVE_TO_WINDOW, v ) DEFAULT .F.
   PROPERTY RTLLayout                SET ::__SetFlags( TDF_RTL_LAYOUT, v )                  DEFAULT .F.
   PROPERTY NoDefaultRadioButton     SET ::__SetFlags( TDF_NO_DEFAULT_RADIO_BUTTON, v )     DEFAULT .F.
   PROPERTY CanBeMinimized           SET ::__SetFlags( TDF_CAN_BE_MINIMIZED, v )            DEFAULT .F.
   PROPERTY OK_Button                SET ::__SetBttns( TDCBF_OK_BUTTON, v )                 DEFAULT .F.
   PROPERTY YES_Button               SET ::__SetBttns( TDCBF_YES_BUTTON, v )                DEFAULT .F.
   PROPERTY NO_Button                SET ::__SetBttns( TDCBF_NO_BUTTON, v )                 DEFAULT .F.
   PROPERTY CANCEL_Button            SET ::__SetBttns( TDCBF_CANCEL_BUTTON, v )             DEFAULT .F.
   PROPERTY RETRY_Button             SET ::__SetBttns( TDCBF_RETRY_BUTTON, v )              DEFAULT .F.
   PROPERTY CLOSE_Button             SET ::__SetBttns( TDCBF_CLOSE_BUTTON, v )              DEFAULT .F.

   METHOD Init() CONSTRUCTOR
   METHOD Show()
   METHOD __SetFlags()
   METHOD __SetBttns()
ENDCLASS

METHOD Init( oParent ) CLASS TaskDialog
   ::__xCtrlName   := "TaskDialog"
   ::ClsName       := "TaskDialog"
   ::ComponentType := "CommonDialog"
   Super:Init( oParent )
RETURN Self

METHOD Show() CLASS TaskDialog
   LOCAL n, nRet, nButton := 0, nRadio := 0, lChecked := .F.
   IF VALTYPE( ::Buttons ) == "C"
      ::Buttons := hb_aTokens( ::Buttons, "|" )
      FOR n := 1 TO LEN( ::Buttons )
          ::Buttons[n] := hb_aTokens( ::Buttons[n], "," )
          ::Buttons[n][1] := VAL(::Buttons[n][1])
      NEXT
   ENDIF
   nRet := TaskDialogIndirect( ::Form:hWnd,;
                               ::Form:AppInstance,;
                               ::Buttons,;
                               ::MainInstruction,;
                               ::Content,;
                               ::__Flags,;
                               ::__ComBttns,;
                               ::WindowTitle,;
                               ::Footer,;
                               ::Width,;
                               @nButton, @nRadio, @lChecked )
   ::ButtonPressed := nButton
   ::RadioButton   := nRadio
   ::VerificationFlagChecked := lChecked
RETURN nButton

METHOD __SetFlags( nFlags, lAdd ) CLASS TaskDialog
   DEFAULT lAdd TO .T.
   IF lAdd
      ::__Flags := (::__Flags | nFlags)
    ELSE
      ::__Flags := (::__Flags & NOT( nFlags ))
   ENDIF
RETURN self

METHOD __SetBttns( nButton, lAdd ) CLASS TaskDialog
   DEFAULT lAdd TO .T.
   IF lAdd
      ::__ComBttns := (::__ComBttns | nButton)
    ELSE
      ::__ComBttns := (::__ComBttns & NOT( nButton ))
   ENDIF
RETURN self

//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------

CLASS ReplaceTextDialog INHERIT CommonDialogs
   PROPERTY HideUpDown    SET ::SetStyle( FR_HIDEUPDOWN, v )    DEFAULT .F.
   PROPERTY HideMatchCase SET ::SetStyle( FR_HIDEMATCHCASE, v ) DEFAULT .F.
   PROPERTY HideWholeWord SET ::SetStyle( FR_HIDEWHOLEWORD, v ) DEFAULT .F.

   DATA FindWhat      EXPORTED  INIT ""
   DATA ReplaceWith   EXPORTED  INIT ""
   DATA MultipleFiles EXPORTED  INIT .F.
   DATA WholeWord     EXPORTED  INIT .F.
   DATA MatchCase     EXPORTED  INIT .F.
   DATA Direction     EXPORTED  INIT 0
   DATA Events        EXPORTED  INIT {;
                                       {"General", { { "OnFindNext",   "", "" },;
                                                     { "OnReplace",    "", "" },;
                                                     { "OnReplaceAll", "", "" } } };
                                     }

   CLASSDATA hDlg      PROTECTED
   DATA __pCallBackPtr PROTECTED
   DATA __nProc        PROTECTED
   DATA cInit          PROTECTED

   METHOD Init() CONSTRUCTOR
   METHOD Show()
   METHOD __WndProc()
   METHOD ExecuteEvent()
   METHOD Close()    INLINE IIF( IsWindow( ::hDlg ), DestroyWindow( ::hDlg ), )
   METHOD SetFocus() INLINE SetFocus( GetDlgItem( ::hDlg, 1152 ) )
ENDCLASS

METHOD Init( oOwner ) CLASS ReplaceTextDialog
   DEFAULT ::__xCtrlName   TO "ReplaceTextDialog"
   DEFAULT ::ClsName       TO "ReplaceTextDialog"
   DEFAULT ::ComponentType TO "CommonDialog"
   ::Style          := FR_ENABLEHOOK
   Super:Init( oOwner )
RETURN Self

METHOD Show( oOwner, cInit ) CLASS ReplaceTextDialog
   LOCAL pfr := (struct FINDREPLACE)
   DEFAULT oOwner TO ::Owner
   DEFAULT cInit  TO ""
   ::cInit := cInit
   IF IsWindow( ::hDlg )
      SetActiveWindow( ::hDlg )
      SetDlgItemText( ::hDlg, 1152, ::cInit )
   ELSE
      ::__pCallBackPtr := WinCallBackPointer( HB_ObjMsgPtr( Self, "__WndProc" ), Self )
      IF ::ClsName == "ReplaceTextDialog"
         ::hDlg := ReplaceText( oOwner:hWnd, ::Style, ::__pCallBackPtr )
       ELSE
         ::hDlg := FindText( oOwner:hWnd, ::Style, ::__pCallBackPtr )
      ENDIF
   ENDIF
RETURN NIL

METHOD __WndProc( hWnd, nMsg, nwParam, nlParam ) CLASS ReplaceTextDialog
   LOCAL pfr
   (hWnd, nlParam)
   DO CASE
      CASE nMsg == WM_INITDIALOG
           pfr = (struct FINDREPLACE*) nlParam
           ShowWindow( hWnd, SW_SHOWNORMAL )
           SetDlgItemText( hWnd, 1152, ::cInit )
           UpdateWindow( hWnd )
           SendMessage( GetDlgItem(hWnd,1056), BM_SETCHECK, BST_UNCHECKED, 0 )
           SendMessage( GetDlgItem(hWnd,1057), BM_SETCHECK, BST_CHECKED, 0 )
           SendMessage( GetDlgItem(hWnd,1152), EM_SETSEL, 0, -1 )
           ::Direction := 1

      CASE nMsg == WM_COMMAND
           DO CASE
              CASE nwParam == IDOK
                   ::ExecuteEvent( "OnFindNext", hWnd )

              CASE nwParam == 1024
                   ::ExecuteEvent( "OnReplace", hWnd )

              CASE nwParam == 1025
                   ::ExecuteEvent( "OnReplaceAll", hWnd )

              CASE nwParam == 1040
                   ::WholeWord := ( SendMessage( GetDlgItem( hWnd, nwParam ), BM_GETSTATE, 0, 0 ) & BST_CHECKED ) != 0

              CASE nwParam == 1041
                   ::MatchCase := ( SendMessage( GetDlgItem( hWnd, nwParam ), BM_GETSTATE, 0, 0 ) & BST_CHECKED ) != 0

              CASE nwParam == 1056
                   ::Direction := 0

              CASE nwParam == 1057
                   ::Direction := 1

              CASE nwParam == IDCANCEL
                   //VXH_FreeCallBackPointer( ::__pCallBackPtr )
                   ::Application:MainForm:PostMessage( WM_VXH_FREECALLBACK, ::__pCallBackPtr )
                   ::ExecuteEvent( "OnFindClose", hWnd )
           ENDCASE
   ENDCASE
RETURN 0

METHOD ExecuteEvent( cEvent, hWnd ) CLASS ReplaceTextDialog
   ::FindWhat    := GetDlgItemText( hWnd, 1152 )
   ::ReplaceWith := GetDlgItemText( hWnd, 1153 )

   IF HGetPos( ::Owner:EventHandler, cEvent ) != 0
      cEvent := ::Owner:EventHandler[ cEvent ]
      IF __objHasMsg( ::Owner:Form, cEvent )
         ::Owner:Form:&cEvent( Self )
      ELSEIF __objHasMsg( ::Owner, cEvent )
         ::Owner:&cEvent( Self )
      ENDIF
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------

CLASS FindTextDialog INHERIT ReplaceTextDialog
   METHOD Init() CONSTRUCTOR
ENDCLASS

METHOD Init( oOwner ) CLASS FindTextDialog
   ::__xCtrlName    := "FindTextDialog"
   ::ClsName        := "FindTextDialog"
   ::ComponentType  := "CommonDialog"
   ::Style          := FR_ENABLEHOOK
   Super:Init( oOwner )
RETURN Self
