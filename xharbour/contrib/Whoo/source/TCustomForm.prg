/*
 * $Id: TCustomForm.prg,v 1.10 2002/11/18 06:38:05 what32 Exp $
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

  PROTECTED:

    DATA     FActionLists              //: TList;
    DATA     FFormState                //: TFormState;


    METHOD   Activate                VIRTUAL  //; dynamic;
    METHOD   ActiveChanged           VIRTUAL  //; dynamic;

    METHOD   Deactivate              VIRTUAL  //; dynamic;

    METHOD   DoClose                 VIRTUAL  //; dynamic;
    METHOD   DoCreate                VIRTUAL
    METHOD   DoDestroy               VIRTUAL
    METHOD   DoHide                  VIRTUAL  //; dynamic;
    METHOD   DoShow                  VIRTUAL  //; dynamic;


    METHOD   HandleCreateException   VIRTUAL   //: Boolean; dynamic;
    METHOD   Paint                   VIRTUAL  //; dynamic;
    METHOD   UpdateActions           VIRTUAL
    METHOD   UpdateWindowState       VIRTUAL



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
//    PROPERTY Height                     //stored IsFormSizeStored;
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
//    PROPERTY Width                      //stored IsFormSizeStored;

    PROPERTY WindowMenu                 READ FWindowMenu WRITE SetWindowMenu //: TMenuItem;




  PUBLIC:
//    METHOD   Create                     VIRTUAL //(AOwner: TComponent); override;
//    METHOD   CloseQuery                 VIRTUAL //: Boolean; virtual;
//    METHOD   Close                      VIRTUAL
//    METHOD   Show                       VIRTUAL
    METHOD   CreateNew                  VIRTUAL //VIRTUAL     //(AOwner: TComponent; Dummy: Integer = 0); virtual;
    METHOD   DefocusControl             VIRTUAL //(Control: TWinControl; Removing: Boolean);
    METHOD   FocusControl               VIRTUAL //(Control: TWinControl);
    METHOD   GetFormImage               VIRTUAL //: TBitmap;
    METHOD   Hide                       VIRTUAL
    METHOD   IsShortCut                 VIRTUAL //(var Message: TWMKey): Boolean; dynamic;
    METHOD   MakeFullyVisible           VIRTUAL //(AMonitor: TMonitor = nil);
    METHOD   Print                      VIRTUAL
    METHOD   Release                    VIRTUAL
    METHOD   SendCancelMode             VIRTUAL //(Sender: TControl);
    METHOD   SetFocusedControl          VIRTUAL //(Control: TWinControl): Boolean; virtual;
    METHOD   ShowModal                  VIRTUAL //: Integer; virtual;
    METHOD   WantChildKey               VIRTUAL //(Child: TControl; var Message: TMessage): Boolean; virtual;
/*
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
//    METHOD SetParent( oParent )

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
/*
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

*/