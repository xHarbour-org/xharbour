/*
 * $Id: TCustomForm.prg,v 1.6 2002/11/13 00:57:52 ronpinkas Exp $
 */

/*
 * xHarbour Project source code:
 *
 * Whoo.lib TCustomForm CLASS
 *
 * Copyright 2002 Francesco Saverio Giudice [info@fsgiudice.com]
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
#include "hbclass.ch"
#include "what32.ch"
#include "classex.ch"
#include "types.ch"


CLASS TCustomForm FROM TScrollingWinControl
/*
  PROTECTED:

    DATA     FActionLists              //: TList;
    DATA     FFormState                //: TFormState;
    METHOD   Activate                virtual  //; dynamic;
    METHOD   ActiveChanged           virtual  //; dynamic;
    METHOD   AlignControls           virtual  //; override;
    METHOD   BeginAutoDrag           virtual  //; override;
    METHOD   ChangeScale             virtual  //; override;
    METHOD   CreateParams            virtual  //; override;
    METHOD   CreateWindowHandle      virtual  //; override;
    METHOD   CreateWnd               virtual  //; override;
    METHOD   Deactivate              virtual  //; dynamic;
    METHOD   DefineProperties        virtual  //; override;
    METHOD   DestroyWindowHandle     virtual  //; override;
    METHOD   DoClose                 virtual  //; dynamic;
    METHOD   DoCreate                VIRTUAL
    METHOD   DoDestroy               VIRTUAL
    METHOD   DoHide                  virtual  //; dynamic;
    METHOD   DoShow                  virtual  //; dynamic;
    METHOD   GetClientRect           virtual   //; override;
    METHOD   GetChildren             virtual  //; override;
    METHOD   GetFloating             virtual   //: Boolean; override;
    METHOD   HandleCreateException   virtual   //: Boolean; dynamic;
    METHOD   Loaded                  virtual  //; override;
    METHOD   Notification            virtual  //; override;
    METHOD   Paint                   virtual  //; dynamic;
    METHOD   PaintWindow             virtual  //; override;
    METHOD   PaletteChanged          virtual   //(Foreground: Boolean): Boolean; override;
    METHOD   QueryInterface          virtual   //(const IID: TGUID; out Obj): HResult; override;
    METHOD   ReadState               virtual  //(Reader: TReader); override;
    METHOD   RequestAlign            virtual  //; override;
    METHOD   SetChildOrder           virtual  //(Child: TComponent; Order: Integer); override;
    METHOD   SetParentBiDiMode       virtual  //(Value: Boolean); override;
    METHOD   DoDock                  virtual  //(NewDockSite: TWinControl; var ARect: TRect); override;
    METHOD   SetParent               virtual  //(AParent: TWinControl); override;
    METHOD   UpdateActions           VIRTUAL
    METHOD   UpdateWindowState       virtual
    METHOD   ValidateRename          virtual  //(AComponent: TComponent; const CurName, NewName: string); override;
    METHOD   VisibleChanging         virtual  //; override;
    METHOD   WndProc                 virtual  //(var Message: TMessage); override;
    METHOD   Resizing                virtual  //(State: TWindowState); override;
*/

    PROPERTY ActiveMDIChild             //: TForm;
    PROPERTY AlphaBlend                 AS LOGICAL
    PROPERTY AlphaBlendValue            AS NUMERIC   //: Byte;
    PROPERTY BorderIcons                TYPE TBorderIcons DEFAULT {biSystemMenu, biMinimize, biMaximize}
    PROPERTY AutoScroll                 //stored IsAutoScrollStored;
    PROPERTY ClientHandle               AS NUMERIC //: HWND;
    PROPERTY ClientHeight               WRITE SetClientHeight //stored IsClientSizeStored;
    PROPERTY ClientWidth                WRITE SetClientWidth  //stored IsClientSizeStored;
    PROPERTY TransparentColor           AS LOGICAL
    PROPERTY TransparentColorValue      //: TColor
    PROPERTY Ctl3D                      DEFAULT TRUE
    PROPERTY DefaultMonitor             TYPE TDefaultMonitor DEFAULT dmActiveForm
    PROPERTY FormStyle                  TYPE TFormStyle      DEFAULT fsNormal
    PROPERTY Height                     //stored IsFormSizeStored;
    PROPERTY HorzScrollBar              //stored IsForm;
    PROPERTY Icon                       //: TIcon
    PROPERTY MDIChildCount              AS NUMERIC
    PROPERTY MDIChildren                AS ARRAY //[I: Integer]: TForm;
    PROPERTY OldCreateOrder             AS LOGICAL
    PROPERTY ObjectMenuItem             //: TMenuItem;
    PROPERTY PixelsPerInch              AS NUMERIC //: Integer;
    PROPERTY ParentFont                 DEFAULT FALSE
    PROPERTY PopupMenu                  //stored IsForm;
    PROPERTY Position                   TYPE TPosition       DEFAULT poDesigned
    PROPERTY PrintScale                 TYPE TPrintScale     DEFAULT poProportional
    PROPERTY Scaled                     AS LOGICAL           DEFAULT TRUE
    PROPERTY TileMode                   TYPE TTileMode       DEFAULT tbHorizontal
    PROPERTY VertScrollBar              //stored IsForm;
    PROPERTY Visible                    WRITE SetVisible     DEFAULT FALSE
    PROPERTY Width                      //stored IsFormSizeStored;

    PROPERTY WindowMenu                 READ WindowMenu WRITE SetWindowMenu //: TMenuItem;

/*
  PUBLIC:
    METHOD   Create                     CONSTRUCTOR //(AOwner: TComponent); override;
    METHOD   CreateNew                  CONSTRUCTOR //VIRTUAL     //(AOwner: TComponent; Dummy: Integer = 0); virtual;
    METHOD   Destroy                    virtual //destructor ; override;
    METHOD   AfterConstruction          virtual //; override;
    METHOD   BeforeDestruction          virtual //; override;
    METHOD   Close
    METHOD   CloseQuery                 VIRTUAL //: Boolean; virtual;
    METHOD   DefaultHandler             virtual //(var Message); override;
    METHOD   DefocusControl             virtual //(Control: TWinControl; Removing: Boolean);
    METHOD   Dock                       virtual //(NewDockSite: TWinControl; ARect: TRect); override;
    METHOD   FocusControl               virtual //(Control: TWinControl);
    METHOD   GetFormImage               virtual //: TBitmap;
    METHOD   Hide                       virtual
    METHOD   IsShortCut                 virtual //(var Message: TWMKey): Boolean; dynamic;
    METHOD   MakeFullyVisible           virtual //(AMonitor: TMonitor = nil);
    METHOD   MouseWheelHandler          virtual //(var Message: TMessage); override;
    METHOD   Print                      virtual
    METHOD   Release                    virtual
    METHOD   SendCancelMode             virtual //(Sender: TControl);
    METHOD   SetFocus                   virtual //; override;
    METHOD   SetFocusedControl          VIRTUAL //(Control: TWinControl): Boolean; virtual;
    METHOD   Show                       virtual
    METHOD   ShowModal                  VIRTUAL //: Integer; virtual;
    METHOD   WantChildKey               virtual //(Child: TControl; var Message: TMessage): Boolean; virtual;

    PROPERTY Active                     AS LOGICAL  //: Boolean;
    PROPERTY ActiveControl                      //: TWinControl;
    PROPERTY Action
    PROPERTY ActiveOleControl                   //: TWinControl;
    PROPERTY BorderStyle                TYPE TFormBorderStyle DEFAULT bsSizeable
    PROPERTY Canvas                     //: TCanvas;
    PROPERTY Caption                    //stored IsForm;
    PROPERTY Color                      //nodefault;
    PROPERTY Designer                   //: IDesignerHook;
    PROPERTY DropTarget                 AS LOGICAL //: Boolean;
    PROPERTY Font
    PROPERTY FormState                  //: TFormState;
    PROPERTY HelpFile                   AS STRING
    PROPERTY KeyPreview                 AS LOGICAL DEFAULT FALSE
    PROPERTY Menu                       //: TMainMenu;
    PROPERTY ModalResult                //: TModalResult;
    PROPERTY Monitor                    //: TMonitor;
    PROPERTY OleFormObject              //: IOleForm;
    PROPERTY WindowState                TYPE TWindowState DEFAULT wsNormal
*/

    METHOD SetWindowMenu( oMenu )
    METHOD RefreshMDIMenu()
    METHOD SetParent( oParent )

ENDCLASS

METHOD SetWindowMenu( Value ) CLASS TCustomForm

   IF ::FWindowMenu != Value
      ::FWindowMenu := Value

      IF Value != NIL
         Value:FreeNotification(Self)
      ENDIF

      ::RefreshMDIMenu()
   ENDIF

Return NIL

METHOD RefreshMDIMenu() CLASS TCustomForm

   LOCAL MenuHandle, WindowMenuHandle
   LOCAL Redraw

   IF ( ::FormStyle == fsMDIForm ) .AND. ( ::ClientHandle <> 0 )
      MenuHandle := 0
      IF ::Menu != NIL
         MenuHandle := ::Menu:Handle
      ENDIF

      WindowMenuHandle := 0
      IF ::WindowMenu != NIL
         WindowMenuHandle := ::WindowMenu:Handle
      ENDIF

      Redraw := GetMenu( ::Handle ) <> MenuHandle

      SendMessage( ::ClientHandle, WM_MDISETMENU, MenuHandle, WindowMenuHandle )

      IF Redraw
         DrawMenuBar( ::Handle )
      ENDIF
   ENDIF

Return NIL

METHOD SetParent( oParent ) CLASS TCustomForm

   IF ( ! ::Parent == oParent) .AND. ( ! oParent == Self )
      IF ::Parent == NIL
         ::DestroyHandle()
      ENDIF

      ::Super:SetParent( oParent )

      IF ::Parent == NIL
         ::UpdateControlState()
      ENDIF
   ENDIF

RETURN NIL

