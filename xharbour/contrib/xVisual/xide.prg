/*
 * $Id: xide.prg,v 1.98 2002/10/27 01:29:26 what32 Exp $
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
GLOBAL MainFrame
GLOBAL FormEdit

//-------------------------------------------------------------------------------------------

FUNCTION Main
   local oSplash

   oApp := Application():Initialize()


   // splash screen
//   oSplash := TSplash():New( oApp, "visualxharbour.bmp", 3000 )

   WITH OBJECT oApp
      WITH OBJECT :CreateForm( @MainFrame, MainFrame() )
         :SetStyle( WS_THICKFRAME, .F. )
         :SetStyle( WS_MAXIMIZEBOX, .F. )
         :MainMenu()
         :MainToolBar()
         :MainStatusBar()

         // add the object windows
         :Add( /*'ObjTree',*/ ObjTree():New( MainFrame ) )
         :Add( /*'ObjInsp',*/ ObjInspect():New( MainFrame ) )
         :Add( /*'ObjEdit',*/ ObjEdit():New( MainFrame ) )

         // focus to main Frame
         :SetFocus()
      END
      :Run()
  END
RETURN( nil)

//----------------------------------------------------------------------------------------------

CLASS MainFrame FROM TForm
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
         :AddItem( 'Editor', 101, {||oApp:CreateForm( @FormEdit, TFormEdit(), MainFrame ) } )
         :AddItem( 'Open', 102, {|| XFMOpen( "Form1.prg" ) } )
         :AddSeparator()
         :AddItem( 'Exit'  , 200, {||MainFrame:PostMessage(WM_SYSCOMMAND,SC_CLOSE)} )
      end
   end
   ::SetWindowMenu()
return(self)

//----------------------------------------------------------------------------------------------

METHOD MainToolBar() CLASS MainFrame
   local n, oTool, oSplash
   LOCAL hImg1,hImg2,hImg3,hBmp,aStdTab
   ::Add( /*"Rebar",*/ TRebar():New( MainFrame ) )

    // add the xmake toolbar
   With Object ::Add( /*"Tools",*/ TToolBar():New( MainFrame, 444, 15, , , 26, 26, 20, 20, 14 ))
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
   ::Rebar:AddBand( NIL, RBBS_GRIPPERALWAYS + RBBS_NOVERT , ::ToolBar:handle, 200, 52, 200, "", NIL )

   // add the TabControl on the Rebarband
   With Object ::Add( /*"Tabs",*/ TTabControl():New( MainFrame, 445,  0,  0,  0,  0) )
      :AddTab( "StdTab", TabPage():New( MainFrame:TabControl, "Standard" ) )
      :AddTab( "Additional" )
      :AddTab( "Win32", TabPage():New( MainFrame:TabControl, "Win32" ) )
      :AddTab( "System" )
      :AddTab( "Internet" )
      :AddTab( "Dialogs" )
      :AddTab( "Win 3.1" )
      :AddTab( "Samples" )
      :AddTab( "Activex" )
      :Configure()
   End
   ::Rebar:AddBand( NIL, RBBS_GRIPPERALWAYS + RBBS_NOVERT , ::TabControl:handle, 550, 56, , "", NIL )

   // sets the controls toolbar on the TabControl
   With Object ::TabControl:StdTab
      :Add( /*'TabBand',*/ TRebar():New( MainFrame:TabControl:StdTab ) )
      :Rebar:SetStyle( WS_BORDER, .F. )
      With Object :Add( /*'StdTools',*/ TToolBar():New( MainFrame:TabControl:StdTab, 444, 14, , , 28, 28, 20, 20 ) )
         :SetStyle( TBSTYLE_CHECKGROUP )
         aStdTab := { '', 'Frames', 'MainMenu', 'PopupMenu', 'Label', 'Edit', 'Memo', 'Button', ;
                          'CheckBox', 'RadioButton', 'ListBox', 'ComboBox', 'ScrollBar', 'GroupBox', ;
                          'RadioGroup', 'Panel', 'ActionList' }
         for n:=0 to 16
             oTool := ToolButton():New( n,,aStdTab[n+1], 150+n )
             oTool:Action := {|oItem| FormEdit:OnMenuCommand(oItem) }
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
      :Rebar:AddBand( NIL, RBBS_NOVERT, :ToolBar:handle, 100, 30,  , "", NIL )
      :ToolBar:DisableAll()
   End

//----------------------------------------------------------------------------------------------
   With Object ::TabControl:Win32
      :Add( /*'TabWin32',*/ TRebar():New( MainFrame:TabControl:Win32 ) )
      :Rebar:SetStyle( WS_BORDER, .F. )
      With Object :Add( /*'Win32Tools',*/ TToolBar():New( MainFrame:TabControl:Win32, 445, 14, , , 28, 28, 20, 20 ) )
         :SetStyle( TBSTYLE_CHECKGROUP )
         aStdTab := { '', 'TabControl', 'TreeView', '', 'StatusBar', 'ProgressBar', 'ToolBar', 'Rebar', ;
                      '', '', '', '', '', '', ;
                      '', '', '' }
         for n:=0 to 16
             oTool := ToolButton():New( n,,aStdTab[n+1], 250+n )
             oTool:Action := {|oItem| FormEdit:OnMenuCommand(oItem) }
             oTool:Style  := TBSTYLE_BUTTON + TBSTYLE_CHECKGROUP
             :AddButton( if(n==0,'arrow',aStdTab[n+1] ), oTool )
         next

         // ----------------------------------------------------   set imagelist
         hImg2:= ImageList_Create( 24, 24, ILC_COLORDDB+ILC_MASK )
         hBmp := LoadImage( hInstance(), "WIN32", IMAGE_BITMAP, 0, 0, LR_LOADTRANSPARENT )
         ImageList_AddMasked( hImg2, hBmp, RGB( 0, 255, 255 ) )
         DeleteObject(hBmp)
         SendMessage( :handle, TB_SETIMAGELIST, 0, hImg2 )
         //---------------------------------------------------------------------

      End
      :Rebar:AddBand( NIL, RBBS_NOVERT, :ToolBar:handle, 100, 30,  , "", NIL )
      :ToolBar:DisableAll()
   End
   //--------- sets a QUICK access to the control
   ::SetLink( 'StdBar',   ::TabControl:StdTab:ToolBar)
   ::SetLink( 'Win32Bar', ::TabControl:Win32:ToolBar )
return(self)

//----------------------------------------------------------------------------------------------

METHOD MainStatusBar() CLASS MainFrame
   ::Add( /*'Status',*/  TStatusBar():New( MainFrame, 'StatusBar', 1001 ) )
   ::StatusBar:SetPanels( { 150,380,480,580,-1 } )
   ::StatusBar:SetPanelText( 0, "Visual xHarbour" )
return(self)

//----------------------------------------------------------------------------------------------
#pragma BEGINDUMP

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include <windows.h>

#include "hbapi.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbfast.h"

#define SKIP_SPACE() { while( *sText == ' ' || *sText == '\t' ) sText++; }
#define SKIP_EOL() { while( *sText == '\n' || *sText == '\r' ) sText++; }

int XFMParse( char *sText );

HB_FUNC( XFMOPEN )
{
   PHB_ITEM pXFM = hb_param( 1, HB_IT_STRING );
   FILE *fh;
   long lSize ;
   char *sXFM;
   int  iErr;

   //printf( "File: %s Handle: %p\n", argv[1], fh );

   if( pXFM )
   {
      fh = fopen( pXFM->item.asString.value, "r" );
   }
   else
   {
      fh = NULL;
   }

   if( fh )
   {
      fseek( fh, 0, SEEK_END );
      if( ( iErr = ferror( fh ) ) != 0 )
      {
         OutputDebugString( "I/O Error\n" );
         hb_retl( 0 );
         fclose( fh );
         return;
      }

      lSize = ftell( fh );

      if( lSize > 0 )
      {
         sXFM = malloc( lSize + 1 );

         fseek( fh, 0, SEEK_SET );
         if( ( iErr = ferror( fh ) ) != 0 )
         {
            OutputDebugString( "I/O Error\n" );
            hb_retl( 0 );
            fclose( fh );
            return;
         }

         fread( sXFM, 1, lSize, fh );
         if( ( iErr = ferror( fh ) ) != 0 )
         {
            OutputDebugString( "I/O Error\n" );
            hb_retl( 0 );
            fclose( fh );
            return;
         }

         //printf( "%s", sXFM );
         hb_retl( ! XFMParse( sXFM ) );
         fclose( fh );
      }
   }

   hb_retl( 1 );
}

int XFMParse( char *sText )
{
   char sClass[64], sFromClass[64], sVar[64], sExp[64], *pTemp, *pEnd[16], sAssign[64], sTemp[256];
   int i, iEnd = 0;
   static PHB_DYNS pCreateForm = NULL;
   static PHB_DYNS pTFormEdit = NULL;
   HB_ITEM Exp;

   if( pCreateForm == NULL )
   {
      pCreateForm = hb_dynsymFind( "CREATEFORM" );
   }

   if( pTFormEdit == NULL )
   {
      pTFormEdit = hb_dynsymFind( "TFORMEDIT" );
   }

   //OutputDebugString( sText );

   sText = strstr( sText, "// ! AUTO_GENERATED !" );
   if( ! sText )
   {
      return 0;
   }
   sText += 21;

   sText = strstr( sText, "CLASS" );
   if( ! *sText )
   {
      return 0;
   }
   sText += 5;

   SKIP_SPACE();

   i = 0;
   while( isalnum( *sText ) )
   {
     sClass[i++] = *sText++;
   }
   sClass[i] = '\0';

   OutputDebugString( "Class: " );
   OutputDebugString( (char *) sClass );

   SKIP_SPACE();

   if( strncmp( sText, "FROM", 4 ) )
   {
      return 0;
   }
   sText += 4;

   SKIP_SPACE();

   i = 0;
   while( isalnum( *sText ) )
   {
     sFromClass[i++] = *sText++;
   }
   sFromClass[i] = '\0';

   OutputDebugString( " From: " );
   OutputDebugString( sFromClass );
   OutputDebugString( "\n" );

   //TFormEdit()
   hb_vmPushSymbol( pTFormEdit->pSymbol );
   hb_vmPushNil();
   hb_vmDo( 0 );

   //oApp:CreateForm( @FormEdit, TFormEdit(), MainFrame )
   hb_vmPushSymbol( pCreateForm->pSymbol );
   hb_vmPush( &OAPP );
   // See below alternative to pushing REF.
   //memcpy( ( * hb_stack.pPos ), &FORMEDIT, sizeof( HB_ITEM ) );
   //hb_stackPush();
   hb_vmPushNil();
   hb_vmPush( &hb_stack.Return );
   hb_itemClear( &hb_stack.Return );
   hb_vmPush( &MAINFRAME );
   hb_vmSend( 3 );

   // Instead of pushing @FormEdit
   hb_itemForwardValue( &FORMEDIT, &hb_stack.Return );

   pEnd[ iEnd ] = strstr( sText, "END CLASS" );
   if( pEnd[ iEnd ] == NULL )
   {
      return 0;
   }
   pEnd[ iEnd++ ][0] = '\0';

   SKIP_SPACE();

   SKIP_EOL();

   SKIP_SPACE();

 Vars:

   if( strncmp( sText, "DATA", 4 ) )
   {
      goto Controls;
   }
   sText += 4;

   SKIP_SPACE();

   i = 0;
   while( isalnum( *sText ) )
   {
     sVar[i++] = *sText++;
   }
   sVar[i] = '\0';

   OutputDebugString( "Var: " );
   OutputDebugString( (char *) sVar );

   SKIP_SPACE()

   if( strncmp( sText, "INIT", 4 ) )
   {
      return 0;
   }
   sText += 4;

   SKIP_SPACE();

   i = 0;
   while( i < 64 && *sText && *sText != '\n' )
   {
     sExp[i++] = *sText++;
   }
   sExp[i] = '\0';

   if( *sText != '\n' )
   {
      return 0;
   }

   OutputDebugString( " = " );
   OutputDebugString( (char *) sExp );
   OutputDebugString( "\n" );

   sAssign[0] = '_';
   sAssign[1] = '\0';
   strcat( (char *) sAssign, (char *) sVar );

   switch( sExp[0] )
   {
      case '.' :
        Exp.type = HB_IT_LOGICAL;
        Exp.item.asLogical.value = sExp[1] == 'T' ? TRUE : FALSE;
        break;

      case '"' :
        Exp.type = HB_IT_STRING;
        sExp[ strlen( sVar ) - 1] = '\0';
        Exp.item.asString.value = (char *) sExp + 1;
        Exp.item.asString.bStatic = TRUE;
        break;

      default :
        Exp.type = HB_IT_LONG;
        Exp.item.asLong.value = atol( sExp );
        break;
   }

   OutputDebugString( "Assign: " );
   OutputDebugString( sAssign );
   OutputDebugString( " Type: " );

   sprintf( (char *) sTemp, "%i\n", Exp.type );
   OutputDebugString( sTemp );

   hb_objSendMsg( &FORMEDIT, sAssign, 1, &Exp );
   hb_itemClear( &hb_stack.Return );

   SKIP_EOL();

   SKIP_SPACE();

   goto Vars;

 Controls:

   if( strncmp( sText, "CONTROL", 6 ) )
   {
      return 1;
   }
   sText += 7;

   SKIP_SPACE();

   i = 0;
   while( isalnum( *sText ) )
   {
     sClass[i++] = *sText++;
   }
   sClass[i] = '\0';

   OutputDebugString( "Control: " );
   OutputDebugString( (char *) sClass );

   SKIP_SPACE();

   if( strncmp( sText, "FROM", 4 ) )
   {
      return 0;
   }
   sText += 4;

   SKIP_SPACE();

   i = 0;
   while( isalnum( *sText ) )
   {
     sFromClass[i++] = *sText++;
   }
   sFromClass[i] = '\0';

   OutputDebugString( " From: " );
   OutputDebugString( (char *) sFromClass );
   OutputDebugString( "\n" );

   pEnd[ iEnd ] = strstr( sText, "END CONTROL" );
   if( pEnd[ iEnd ] == NULL )
   {
      return 0;
   }
   pEnd[ iEnd++ ][0] = '\0';

   SKIP_SPACE();

   SKIP_EOL();

   SKIP_SPACE();

 Properties:

   if( *sText != ':' )
   {
      if( strncmp( sText, "OBJECT", 6 ) )
      {
         if( *sText == '\0' )
         {
            OutputDebugString( "END\n" );

            sText = pEnd[ --iEnd ] + 10;

            sText = strchr( sText, '\n' );
            if( sText == NULL )
            {
               return 0;
            }

            SKIP_EOL();

            SKIP_SPACE();
         }

         if( *sText == ':' )
         {
            goto Properties;
         }

         goto Vars;
      }

      sText += 6;

      SKIP_SPACE();

      i = 0;
      while( isalnum( *sText ) )
      {
        sClass[i++] = *sText++;
      }
      sClass[i] = '\0';

      OutputDebugString( "Object: " );
      OutputDebugString( (char *) sClass );

      SKIP_SPACE();

      if( strncmp( sText, "IS", 2 ) )
      {
         return 0;
      }
      sText += 2;

      SKIP_SPACE();

      i = 0;
      while( isalnum( *sText ) )
      {
        sFromClass[i++] = *sText++;
      }
      sFromClass[i] = '\0';

      OutputDebugString( "IS: " );
      OutputDebugString( (char *) sFromClass );
      OutputDebugString( "\n" );

      pEnd[ iEnd ] = strstr( sText, "END OBJECT" );
      if( pEnd[ iEnd ] == NULL )
      {
         return 0;
      }


      pEnd[ iEnd++ ][0] = '\0';

      SKIP_SPACE();

      SKIP_EOL();

      SKIP_SPACE();

      goto Properties;
   }

   sText += 1;

   SKIP_SPACE();

   i = 0;
   while( isalnum( *sText ) )
   {
     sVar[i++] = *sText++;
   }
   sVar[i] = '\0';

   SKIP_SPACE();

   if( strncmp( sText, ":=", 2 ) )
   {
      if( strncmp( sVar, "SetMethod", 9 ) == 0 && *sText == '(' )
      {
         sText++;

         SKIP_SPACE();

         if( *sText == '"' )
         {
            sText++;

            i = 0;
            while( isalnum( *sText ) )
            {
              sVar[i++] = *sText++;
            }
            sVar[i] = '\0';

            sText++;

            OutputDebugString( "Event: " );
            OutputDebugString( (char *) sVar );

            SKIP_SPACE();

            if( *sText != ',' )
            {
               return 0;
            }
            sText++;

            SKIP_SPACE()

            if( strncmp( sText, "{ ||", 4 ) )
            {
               return 0;
            }
            sText += 4;

            SKIP_SPACE();

            i = 0;
            while( isalnum( *sText ) || *sText == '_' )
            {
              sExp[i++] = *sText++;
            }
            sExp[i] = '\0';

            OutputDebugString( " = " );
            OutputDebugString( (char *) sExp );
            OutputDebugString( "\n" );

            sText = strchr( sText, '\n' );
            if( sText == NULL )
            {
               return 0;
            }

            SKIP_EOL();

            SKIP_SPACE();

            goto Properties;
         }
      }
      return 0;
   }

   OutputDebugString( "Var: " );
   OutputDebugString( (char *) sVar );

   sText += 2;

   SKIP_SPACE()

   i = 0;
   while( i < 64 && *sText && *sText != '\n' )
   {
     sExp[i++] = *sText++;
   }
   sExp[i] = '\0';

   if( *sText != '\n' )
   {
      return 0;
   }

   OutputDebugString( " = " );
   OutputDebugString( (char *) sExp );
   OutputDebugString( "\n" );

   SKIP_EOL();

   SKIP_SPACE();

   goto Properties;
}

#pragma ENDDUMP
