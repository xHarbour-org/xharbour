/*
  (c) copyright xHarbour.com Inc. http://www.xHarbour.com
  Author: Ron Pinkas Ron@xHarbour.com

  This source file is an intellectual property of xHarbour.com Inc.
  You may NOT forward or share this file under any conditions!
*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef CINTERFACE
   #define CINTERFACE 1
#endif

#define NONAMELESSUNION

#include "windows.h"
#include "commctrl.h"
#include "shlwapi.h"
#include "shlobj.h"

#include "hbvm.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbfast.h"
#include "hbstack.h"

#define _BUILD_ "4.5"

#define FORCE_VALID 0
#define ANIMATE

#if 1
#undef  OutputDebugString
  //#define OutputDebugString( s ) TraceLog( "debug.log", s )
  #define OutputDebugString( s )
#endif

#define ID_Open     11
#define ID_Save     12
#define ID_Build    13

#define ID_Root    101
#define ID_Target  102
#define ID_Type    103
#define ID_Output  104
#define ID_Clean   105
#define ID_Link    106
#define ID_MT      107
#define ID_GUI     108
#define ID_XBP     109
#define ID_UseDLL  110

#define ID_Main     201
#define ID_Libs     202
#define ID_Includes 203
#define ID_Defines  204

#define ID_OpenSource 301
#define ID_Add        302
#define ID_Remove     303
#define ID_Properties 304

#define ID_xHB_Root         411
#define ID_xHB_Lib          412
#define ID_xHB_Flags        413
#define ID_xHB_Debug        414
#define ID_xHB_ClassicDebug 415

#define ID_C_Compiler  421
#define ID_C_Root      422
#define ID_C_Flags     423
#define ID_C_Debug     424

#define ID_FWH_Root 431
#define ID_FWH_Lib  432

#define ID_RUN_Arguments 441
#define ID_RUN_StartIn   442
#define ID_RUN_Auto      443
#define ID_RUN_Dont      444
#define ID_RUN_Ask       445

#define ID_Return    5001
#define ID_Close     5002
#define ID_Run       5003
#define ID_Arguments 5004
#define ID_StartIn   5005
#define ID_Memorize  5006

#define ID_ErrorLog  9999

#define IDM_MODULES_CONTEXT_OPEN        10101
#define IDM_MODULES_CONTEXT_MOVEUP      10102
#define IDM_MODULES_CONTEXT_MOVEDOWN    10103
#define IDM_MODULES_CONTEXT_MOVETOP     10104
#define IDM_MODULES_CONTEXT_MOVEBOTTOM  10105
#define IDM_MODULES_CONTEXT_PROPERTIES  10106

#define IDM_OPEN_CONTEXT_RELOAD         10201

#define IDM_BUILD_CONTEXT_OPENLOG       10301
#define IDM_BUILD_CONTEXT_VIEWERRORS    10302


HB_EXTERN_BEGIN
   int _getdrive( void );
HB_EXTERN_END

void Process_SetContinueFlag( BOOL bWaitContinue );

BOOL Process_SetContinue( BOOL );

#define Alert( sMessage ) MessageBox( s_hWnd, sMessage, "xBuild Wizard", MB_ICONINFORMATION )

LRESULT CALLBACK MainWindowProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );

LRESULT CALLBACK MyDefaultWindowProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );

LRESULT CALLBACK EditSubclassProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
WNDPROC wpOrigEditProc;

BOOL CALLBACK StepPagesProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
BOOL CALLBACK SettingsProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );

BOOL CALLBACK ErrorLogProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
BOOL CALLBACK SuccessProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );

int CALLBACK ListViewSorter( LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort );

void SelectStepPage( int iPage, BOOL bPaint );
void AcceptDropedFiles( HDROP wParam, BOOL bClean );

void SelectTypeTab( int iTab, BOOL bNewPage );
void SelectSettingTab( int iTab, BOOL bNewPage );

void AddModule( char *sFile );

void AddModules( void );
void DeleteModules( void );
void SetModulesProperties( void );
void OpenModules( void );

void FilesContextMenu( void );
void MoveModules( int iDirection );

BOOL Validate( int iControl, HWND hDlg, HWND hControl, BOOL bForce );
BOOL ValidateTarget( BOOL bForce );
BOOL ValidateMain( BOOL bForce  );
BOOL ValidateFolders( HWND hDlg, int iControlID, char *sRoot, BOOL bForce  );
BOOL ValidateRoot( BOOL bForce  );
BOOL ValidateOutput( void );
BOOL ValidateFWH_Root( BOOL bForce  );
BOOL ValidateFWH_Lib( BOOL bForce  );

void InitFWH( void );

void Init_C_Compiler( void );

void SetProject( void );

HFONT Stretch( HWND hWnd, int iNewWidth, int iNewHeight, int iOldWidth, int iOldHeight, BOOL bRepaint, int iLeft, int iTop, char *sDefaultFont, BOOL bMonoFont );
BOOL CALLBACK EnumChildProc( HWND hwndChild, LPARAM lParam );

HRGN BitmapToRegion( HBITMAP hBmp, COLORREF cTransparentColor );
int PaintBitmap( HWND hWnd, HBITMAP SrcBmp, int LeftDestination, int TopDestination, int Width, int Height, int LeftSource, int TopSource );
int PaintBitmapDC( HWND hWnd, HDC hDC, HBITMAP SrcBmp, int LeftDestination, int TopDestination, int Width, int Height, int LeftSource, int TopSource );

#define MOVE_UP     -1
#define MOVE_DOWN    1
#define MOVE_TOP   -10
#define MOVE_BOTTOM 10

#define BUTTONS  4
#define BUTTON_RADIOUS  21

#define BUTTON_OFFSET   65
#define BUTTON_MARGIN   21
#define BUTTON_TOP       9
#define BUTTON_WIDTH    65
#define BUTTON_HEIGHT   52

#define STATE_OFF   0
#define STATE_ON    1
#define STATE_HOVER 2

static HBITMAP   s_hBitmap;
static HDC       s_MainDC, s_CompatibleDC;
static HWND      s_Tooltip;
static POINT     s_aButtonCenters[BUTTONS] = { { 53, 35 }, { 118, 35 }, { 183, 35 }, { 248, 35 } };

static int       s_iStep = -1;

static HWND      s_hWnd = 0;
static HWND      s_ahStepPages[ BUTTONS ] = { 0, 0, 0, 0 };
static HFONT     s_ahStepFonts[ BUTTONS ];

static HWND      s_hTypeTabControl;
static HWND      s_hSettingTabControl;

#define FILE_TYPES 7

char *s_asTypes[] = { ".prg", ".c", ".rc", ".y", ".obj", ".res", ".lib" };


static HWND      s_ahTypeTabs[ FILE_TYPES ];
static HFONT     s_ahTypeFonts[ FILE_TYPES ];
static int       s_iTypeTab = -1;

char *s_asSettings[] = { "xHarbour", "C Compiler", "FiveWin", "Run" };
static HWND      s_ahSettingTabs[ 4 ];
static HFONT     s_ahSettingFonts[ 4 ];
static int       s_iSettingTab = -1;

static HBRUSH    s_hFormsBrush = NULL;
static HBITMAP   s_ahButtonStates[BUTTONS][3];

static HBITMAP s_ahClose[ 3 ];
static HBITMAP s_ahMinimize[ 3 ];
static HBITMAP s_ahHelp[ 3 ];

static HBITMAP s_ahOpen[ 3 ];
static HBITMAP s_ahSave[ 3 ];
static HBITMAP s_ahFinish[ 3 ];

static HBITMAP s_hCancel;

static HBITMAP s_ahFolder[ 3];

static int s_FolderState_1 = STATE_OFF, s_FolderState_2 = STATE_OFF, s_FolderState_3 = STATE_OFF;

static HMENU s_hPopupContextMenus = NULL;

static HMENU s_hModules_ContextMenu = NULL;
static HMENU s_hOpen_ContextMenu    = NULL;
static HMENU s_hBuild_ContextMenu   = NULL;

char s_sArguments[ 256 ]    = "";
char s_sStartIn[ _MAX_DIR ] = "";

typedef struct tagHover
{
   HBITMAP OFF;
   long Left;
   long Top;
   long Width;
   long Height;
} HOVER, *PHOVER ;

typedef struct tagStretchInfo
{
   float fXFactor;
   float fYFactor;
   char  *sDefaultFont;
   HFONT hFont;
   BOOL  bPaint;
} STRETCHINFO, *PSTRETCHINFO ;

static HOVER s_Hover;
static char s_sCurrentDir[ _MAX_DIR ];

HINSTANCE s_hInstance;

static LPITEMIDLIST s_xHB_RootID = NULL, s_FWH_RootID = NULL, s_StartIn = NULL;

static LPMALLOC s_pSH_Alloc;

static int s_iValidating = 0, s_iSkipValidation = 0;

static char s_C_Compiler[10];

static char s_xHB_Exe[12];

static PHB_ITEM s_pProject, s_pProgress, s_pErrorHandler, s_pOnFWH;

static HWND s_hLog = 0;
static BOOL s_bError = FALSE;

HWND s_hAnimation;

static HFONT s_hFont;

static BOOL s_bAddResourceAsk;

static BOOL s_bAdded = FALSE;
static BOOL s_bDeleted = FALSE;

static HWND s_hResults = 0;

#ifdef DEMO
   void xHbStartDemo( void )
   {
      OutputDebugString( "xBuild Demo Startup\n" );
      //MessageBox( 0, (LPCSTR) "This demo copy of xBuild Wizard was built with xHarbour, distributed by http://www.xHarbour.com", (LPCSTR) "http://www.xHarbour.com", MB_SYSTEMMODAL );
   }
#endif

void WizardEvents( void )
{
   if( s_hWnd )
   {
      MSG msg;

      if( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
      {
         if( ! IsDialogMessage( GetActiveWindow(), &msg ) )
         {
            TranslateMessage( &msg );
            DispatchMessage( &msg );
         }
      }
   }
}

HB_FUNC( XBUILD_GUI_RESULTSWINDOW )
{
   ShowWindow( s_hResults, SW_SHOW );

   hb_retnl( (long) s_hResults );
}

HB_FUNC( GUI_WIZARDWINDOW )
{
   hb_retnl( (long) s_hWnd );
}

HB_FUNC( GUI_LOGWINDOW )
{
   hb_retnl( (long) s_hLog );
}

HB_FUNC( GUI_PROGRESS )
{
   if( s_hWnd )
   {
      char sProgress[ MAX_PATH * 2 ];
      RECT Area;

      strcpy( sProgress, hb_parc( 1 ) );

      Area.left  = 40;
      Area.top   = 83;
      Area.right = 400;
      Area.bottom = 100;

      InvalidateRect( s_hWnd, &Area, FALSE );
      SendMessage( s_hWnd, WM_PAINT, 0, 0 );
      SetTextColor( s_MainDC, RGB( 0, 0, 255 ) );
      SelectObject( s_MainDC, s_hFont );
      TextOut( s_MainDC, 40, 83, sProgress, strlen( sProgress ) );
   }
}

HB_FUNC( XBUILD_GUI_SETERROR )
{
    s_bError = hb_parl(1);
}

HB_FUNC( XBUILD_GUI_ONERROR )
{
   PHB_ITEM pError = hb_param( 1, HB_IT_OBJECT );
   char sError[1024];

   OutputDebugString( "Error: " );
   OutputDebugString( hb_parc(2) );
   OutputDebugString( "\n" );

   s_bError = TRUE;

   ShowWindow( s_hResults, SW_HIDE );

   if( hb_parc(2 ) == NULL || hb_parc(2)[0] == '\0' )
   {
      if( s_hAnimation )
      {
         #ifdef ANIMATE
            Animate_Stop( s_hAnimation );
            Animate_Close( s_hAnimation );
            DestroyWindow( s_hAnimation );
            s_hAnimation = NULL;
         #endif
      }

      if( pError )
      {
         hb_objSendMsg( pError, "Operation", 0 );
         sprintf( sError, "Sorry, could not build your project!\n\nOperation: '%s'\n", hb_stackReturnItem()->item.asString.value );
         hb_objSendMsg( pError, "Description", 0 );
         sprintf( sError + strlen( sError ), "Description: '%s'\n", hb_stackReturnItem()->item.asString.value );

         hb_objSendMsg( pError, "SubSystem", 0 );

         // NON Production Error.
         if( strcmp( hb_stackReturnItem()->item.asString.value, "xBuild" ) )
         {
            hb_objSendMsg( pError, "ModuleName", 0 );
            sprintf( sError + strlen( sError ), "\nModule: '%s'\n", hb_stackReturnItem()->item.asString.value );

            hb_objSendMsg( pError, "ProcName", 0 );
            sprintf( sError + strlen( sError ), "Procedure: '%s'\n", hb_stackReturnItem()->item.asString.value );

            hb_objSendMsg( pError, "ProcLine", 0 );
            sprintf( sError + strlen( sError ), "Line: '%li'\n", hb_stackReturnItem()->item.asLong.value );
         }

         Alert( sError );
      }

      s_iStep = -1;
      return;
   }

   if( s_hWnd )
   {
      if( s_hLog == 0 )
      {
         s_hLog = CreateDialog( GetModuleHandle( NULL ), "xBuild_ErrorLog", 0, ErrorLogProc );
      }
      else
      {
         SetDlgItemText( s_hLog, ID_ErrorLog, hb_parc( 2 ) );
      }

      ShowWindow( s_hLog, SW_SHOW );

      s_iStep = -1;

      //SetForegroundWindow( s_hWnd );
   }
   else
   {
      DialogBox( GetModuleHandle( NULL ), "xBuild_ErrorLog", 0, ErrorLogProc );
   }
}

BOOL CALLBACK ErrorLogProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   // char sMessage[256];

   // sprintf( sMessage, "Message: %i, wParam: %i, lParam %i\n", message, wParam, lParam );
   // OutputDebugString( sMessage );

   switch( message )
   {
      case WM_GETDLGCODE:
      {
         return DLGC_WANTMESSAGE;
      }

      case WM_INITDIALOG:
      {
         SetWindowText( hWnd, "xBuild - Error Log:" );
         SetDlgItemText( hWnd, ID_ErrorLog, hb_parc( 2 ) );

         SetWindowLong( hWnd, GWL_EXSTYLE, WS_EX_CONTROLPARENT );
         break;//return TRUE;
      }

      case WM_SIZE :
      {
         RECT ClientArea;

         GetClientRect( hWnd, &ClientArea );
         MoveWindow( GetDlgItem( hWnd, ID_ErrorLog ), 0, 0, ClientArea.right, ClientArea.bottom, FALSE );

         break;
      }

      case WM_CLOSE:
      {
         DestroyWindow( hWnd );
         s_hLog = 0;
         break;
      }
   }

   return FALSE;
}

BOOL CALLBACK SuccessProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   // char sMessage[256];

   // sprintf( sMessage, "Message: %i, wParam: %i, lParam %i\n", message, wParam, lParam );
   // OutputDebugString( sMessage );
   static BOOL s_bOK = FALSE;

   switch( message )
   {
      case WM_COMMAND:
      {
         // Menus.
         if( lParam == 0 )
         {
            OutputDebugString( "Menu\n" );
         }
         else
         {
            switch( HIWORD( wParam ) )
            {
               case BN_CLICKED:
               {
                  switch( LOWORD( wParam ) )
                  {
                     case IDOK :
                     {
                        s_bOK = TRUE;
                        SendMessage( hWnd, WM_CLOSE, 0, 0 );
                        break;
                     }
                  }

                  break;
               }

               case EN_KILLFOCUS :
               {
                  OutputDebugString( "Edit Losing Focus\n" );

                  // Modal!!!
                  if( IsChild( /*s_*/hWnd, GetFocus() ) )
                  {
                     //OutputDebugString( "CHILD\n" );

                     Validate( LOWORD( wParam ), hWnd, (HWND) lParam, TRUE );
                  }

                  break;
               }

               case EN_SETFOCUS :
               {
                  // OutputDebugString( "Edit Getting Focus\n" );
                  s_iValidating = LOWORD( wParam );
                  s_iSkipValidation = 0;
                  break;
               }
            }
         }

         break;
      }

      case WM_INITDIALOG:
      {
         s_bOK = FALSE;

         CheckRadioButton( hWnd, ID_Return, ID_Close, ID_Return );
         CheckDlgButton( hWnd, ID_Run, BST_CHECKED );
         SetDlgItemText( hWnd, ID_Arguments, s_sArguments );
         SetDlgItemText( hWnd, ID_StartIn, s_sStartIn );
         SetFocus( GetDlgItem( hWnd, ID_Arguments ) );

         SetWindowLong( hWnd, GWL_EXSTYLE, WS_EX_CONTROLPARENT );
         break;//return TRUE;
      }

      case WM_CLOSE:
      {
         int iResult = 0;

         if( s_bOK == FALSE )
         {
            EndDialog( hWnd, 0 + iResult );
            break;
         }

         if( IsDlgButtonChecked( hWnd, ID_Memorize ) )
         {
            PHB_DYNS pSym;

            GetDlgItemText( hWnd, ID_Arguments, s_sArguments, 256 );
            SetDlgItemText( s_ahSettingTabs[3], ID_RUN_Arguments, s_sArguments );

            GetDlgItemText( hWnd, ID_StartIn, s_sStartIn, _MAX_DIR );
            SetDlgItemText( s_ahSettingTabs[3], ID_RUN_StartIn, s_sStartIn );

            if( IsDlgButtonChecked( hWnd, ID_Run ) )
            {
               CheckRadioButton( s_ahSettingTabs[3], ID_RUN_Auto, ID_RUN_Ask, ID_RUN_Auto );
            }
            else
            {
               CheckRadioButton( s_ahSettingTabs[3], ID_RUN_Auto, ID_RUN_Ask, ID_RUN_Dont );
            }

            pSym = hb_dynsymFindName( "GenerateProjectFile" );

            // OutputDebugString( "Memorize - Save Project.\n" );

            if( pSym )
            {
               SetProject();

               hb_vmPushSymbol( pSym->pSymbol );
               hb_vmPushNil();
               hb_vmPush( s_pProject );
               hb_vmDo( 1 );

               // Safety!
               hb_vmRequestReset();
            }
            else
            {
               Alert( "Sorry, cound't locate: 'GenerateProjectFile()'!" );
            }
         }

         if( IsDlgButtonChecked( hWnd, ID_Run ) )
         {
            GetDlgItemText( hWnd, ID_Arguments, s_sArguments, 256 );
            SetDlgItemText( s_ahSettingTabs[3], ID_RUN_Arguments, s_sArguments );

            GetDlgItemText( hWnd, ID_StartIn, s_sStartIn, _MAX_DIR );
            SetDlgItemText( s_ahSettingTabs[3], ID_RUN_StartIn, s_sStartIn );

            iResult = 10;
         }

         if( IsDlgButtonChecked( hWnd, ID_Close ) )
         {
            EndDialog( hWnd, 1 + iResult );
         }
         else
         {
            EndDialog( hWnd, 0 + iResult );
         }

         break;
      }
   }

   return FALSE;
}

void xBuildWizardGUI( HINSTANCE hInstance, int iCmdShow, PHB_ITEM pProject, PHB_ITEM pProgress, PHB_ITEM pErrorHandler, PHB_ITEM pOnFWH )
{
   char    sMessage[256];
   MSG     msg;
   HRGN    Region;
   INITCOMMONCONTROLSEX Controls;
   BITMAP  bm;
   LONG    lRet;

   #ifdef DEMO
    MessageBox( 0, (LPCSTR) "Thank you for evaluating xHarbour xBuilder. \n Copyright (c) 2008 xHarbour.com Inc. \n \n http://www.xHarbour.com" , (LPCSTR) "xHarbour xBuilder Demo", MB_OK | MB_ICONINFORMATION ) ;
   #endif

   Controls.dwSize = sizeof( INITCOMMONCONTROLSEX );
   Controls.dwICC = ICC_ANIMATE_CLASS | ICC_LISTVIEW_CLASSES | ICC_TAB_CLASSES;

   InitCommonControlsEx( &Controls );
   CoInitialize( NULL );

   if( pProject )
   {
      s_pProject = hb_itemNew( pProject );
   }
   else
   {
      s_pProject = hb_itemNew( NULL);
   }

   if( pProgress )
   {
      s_pProgress = hb_itemNew( pProgress );
   }
   else
   {
      s_pProgress = hb_itemNew( NULL );
   }

   if( pErrorHandler )
   {
      s_pErrorHandler = hb_itemNew( pErrorHandler );
   }
   else
   {
      s_pErrorHandler = hb_itemNew( NULL );
   }

   if( pOnFWH )
   {
      s_pOnFWH = hb_itemNew( pOnFWH );
   }
   else
   {
      s_pOnFWH = hb_itemNew( NULL );
   }

   SHGetMalloc( &s_pSH_Alloc );

   GetCurrentDirectory( _MAX_DIR, s_sCurrentDir );

   s_Hover.OFF    = NULL;

   s_ahButtonStates[0][ STATE_OFF   ] = (HBITMAP) LoadImage( hInstance, "Button1_Off"  , IMAGE_BITMAP, 0, 0, LR_SHARED );
   s_ahButtonStates[0][ STATE_ON    ] = (HBITMAP) LoadImage( hInstance, "Button1_On"   , IMAGE_BITMAP, 0, 0, LR_SHARED );
   s_ahButtonStates[0][ STATE_HOVER ] = (HBITMAP) LoadImage( hInstance, "Button1_Hover", IMAGE_BITMAP, 0, 0, LR_SHARED );

   s_ahButtonStates[1][ STATE_OFF   ] = (HBITMAP) LoadImage( hInstance, "Button2_Off"  , IMAGE_BITMAP, 0, 0, LR_SHARED );
   s_ahButtonStates[1][ STATE_ON    ] = (HBITMAP) LoadImage( hInstance, "Button2_On"   , IMAGE_BITMAP, 0, 0, LR_SHARED );
   s_ahButtonStates[1][ STATE_HOVER ] = (HBITMAP) LoadImage( hInstance, "Button2_Hover", IMAGE_BITMAP, 0, 0, LR_SHARED );

   s_ahButtonStates[2][ STATE_OFF   ] = (HBITMAP) LoadImage( hInstance, "Button3_Off"  , IMAGE_BITMAP, 0, 0, LR_SHARED );
   s_ahButtonStates[2][ STATE_ON    ] = (HBITMAP) LoadImage( hInstance, "Button3_On"   , IMAGE_BITMAP, 0, 0, LR_SHARED );
   s_ahButtonStates[2][ STATE_HOVER ] = (HBITMAP) LoadImage( hInstance, "Button3_Hover", IMAGE_BITMAP, 0, 0, LR_SHARED );

   s_ahButtonStates[3][ STATE_OFF   ] = (HBITMAP) LoadImage( hInstance, "Button4_Off"  , IMAGE_BITMAP, 0, 0, LR_SHARED );
   s_ahButtonStates[3][ STATE_ON    ] = (HBITMAP) LoadImage( hInstance, "Button4_On"   , IMAGE_BITMAP, 0, 0, LR_SHARED );
   s_ahButtonStates[3][ STATE_HOVER ] = (HBITMAP) LoadImage( hInstance, "Button4_Hover", IMAGE_BITMAP, 0, 0, LR_SHARED );

   s_ahClose[ STATE_OFF  ]    = (HBITMAP) LoadImage( hInstance, "Close"         , IMAGE_BITMAP, 0, 0, LR_SHARED );
   s_ahClose[ STATE_HOVER]    = (HBITMAP) LoadImage( hInstance, "Close_Hover"   , IMAGE_BITMAP, 0, 0, LR_SHARED );

   s_ahMinimize[ STATE_OFF  ] = (HBITMAP) LoadImage( hInstance, "Minimize"      , IMAGE_BITMAP, 0, 0, LR_SHARED );
   s_ahMinimize[ STATE_HOVER] = (HBITMAP) LoadImage( hInstance, "Minimize_Hover", IMAGE_BITMAP, 0, 0, LR_SHARED );

   s_ahHelp[ STATE_OFF  ]     = (HBITMAP) LoadImage( hInstance, "Help_"         , IMAGE_BITMAP, 0, 0, LR_SHARED );
   s_ahHelp[ STATE_HOVER]     = (HBITMAP) LoadImage( hInstance, "Help_Hover"    , IMAGE_BITMAP, 0, 0, LR_SHARED );

   s_ahOpen[ STATE_OFF  ]     = (HBITMAP) LoadImage( hInstance, "Open"          , IMAGE_BITMAP, 0, 0, LR_SHARED );
   s_ahOpen[ STATE_HOVER]     = (HBITMAP) LoadImage( hInstance, "Open_Hover"    , IMAGE_BITMAP, 0, 0, LR_SHARED );

   s_ahSave[ STATE_OFF  ]     = (HBITMAP) LoadImage( hInstance, "Save"           , IMAGE_BITMAP, 0, 0, LR_SHARED );
   s_ahSave[ STATE_HOVER]     = (HBITMAP) LoadImage( hInstance, "Save_Hover"     , IMAGE_BITMAP, 0, 0, LR_SHARED );

   s_ahFinish[ STATE_OFF  ]   = (HBITMAP) LoadImage( hInstance, "Finish"         , IMAGE_BITMAP, 0, 0, LR_SHARED );
   s_ahFinish[ STATE_HOVER]   = (HBITMAP) LoadImage( hInstance, "Finish_Hover"   , IMAGE_BITMAP, 0, 0, LR_SHARED );

   s_ahFolder[ STATE_OFF  ]   = (HBITMAP) LoadImage( hInstance, "Folder"         , IMAGE_BITMAP, 0, 0, LR_SHARED );
   s_ahFolder[ STATE_HOVER]   = (HBITMAP) LoadImage( hInstance, "Folder_Hover"   , IMAGE_BITMAP, 0, 0, LR_SHARED );

   s_hCancel = (HBITMAP) LoadImage( hInstance, "Cancel_Build"   , IMAGE_BITMAP, 0, 0, LR_SHARED );

   /*
   ZeroMemory( &bm, sizeof( BITMAP ) );
   GetObject( s_ahFinish[ STATE_HOVER], sizeof( BITMAP ), &bm );
   sprintf( sMessage, "Width: %i, Height: %i\n", bm.bmWidth, bm.bmHeight );
   OutputDebugString( sMessage );
   */

   s_hPopupContextMenus = LoadMenu( hInstance, "PopupContextMenues" );

   s_hModules_ContextMenu = GetSubMenu( s_hPopupContextMenus, 0 );
   s_hOpen_ContextMenu    = GetSubMenu( s_hPopupContextMenus, 1 );
   s_hBuild_ContextMenu   = GetSubMenu( s_hPopupContextMenus, 2 );

   s_hBitmap = (HBITMAP) LoadImage( hInstance, "xBuildWizard", IMAGE_BITMAP, 0, 0, LR_SHARED );

   s_hFont = CreateFont( -12,                  // height of font
                         0,                   // average character width
                         0,                   // angle of escapement
                         0,                   // base-line orientation angle
                         0,                   // font weight
                         FALSE,               // italic attribute option
                         FALSE,               // underline attribute option
                         FALSE,               // strikeout attribute option
                         DEFAULT_CHARSET,     // character set identifier
                         OUT_DEFAULT_PRECIS,  // output precision
                         CLIP_DEFAULT_PRECIS, // clipping precision
                         DEFAULT_QUALITY,     // output quality
                         FF_DONTCARE,         // pitch and family
                         "MS Sans Serif"      // typeface name
                       );

   sprintf( sMessage, "Font: %p\n", s_hFont );
   OutputDebugString( sMessage );

   if( s_hBitmap )
   {
      WNDCLASS wc;

      ZeroMemory( &wc, sizeof(wc) );

      wc.style         = CS_OWNDC;
      wc.lpszClassName = "xBuild_BitmapClippedWindow";
      wc.lpfnWndProc   = (WNDPROC) MainWindowProc;
      wc.hInstance     = hInstance;
      wc.hCursor       = LoadCursor( NULL, IDC_ARROW );
      wc.hIcon         = NULL;

      if( RegisterClass( &wc ) )
      {
         ZeroMemory( &bm, sizeof( BITMAP ) );
         GetObject( s_hBitmap, sizeof( BITMAP ), &bm );

         s_hWnd = CreateWindowEx( WS_EX_ACCEPTFILES,
                                  "xBuild_BitmapClippedWindow", "xBuild",
                                  WS_POPUP | WS_CLIPSIBLINGS,
                                  100, 100,
                                  bm.bmWidth, bm.bmHeight,
                                  NULL, NULL, hInstance, NULL );

         if( s_hWnd )
         {
            TOOLINFO ti;

            SendMessage( s_hWnd, WM_SETICON, (WPARAM)ICON_BIG, (LPARAM) LoadIcon( hInstance, "Application" ) );
            SendMessage( s_hWnd, WM_SETICON, (WPARAM)ICON_SMALL, (LPARAM) LoadIcon( hInstance, "Application" ) );

            //SetWindowLong( s_hWnd, GWL_EXSTYLE, GetWindowLong( s_hWnd, GWL_EXSTYLE) & ~WS_EX_TOOLWINDOW );
            SetWindowLong( s_hWnd, GWL_STYLE, GetWindowLong( s_hWnd, GWL_STYLE) | WS_SYSMENU );

            s_MainDC = GetDC( s_hWnd );
            s_CompatibleDC = CreateCompatibleDC( s_MainDC );

            Region = BitmapToRegion( s_hBitmap, 0x0000ff00 );

            SetWindowRgn( s_hWnd, Region, TRUE );
            ShowWindow( s_hWnd, iCmdShow );

            s_Tooltip = CreateWindowEx( 0, TOOLTIPS_CLASS, NULL,
                                        WS_POPUP | TTS_BALLOON | TTS_NOPREFIX | TTS_ALWAYSTIP,
                                        CW_USEDEFAULT, CW_USEDEFAULT,
                                        CW_USEDEFAULT, CW_USEDEFAULT,
                                        s_hWnd, NULL, s_hInstance, NULL );

            SendMessage( s_Tooltip, (UINT) TTM_SETMAXTIPWIDTH, (WPARAM) 0, (LPARAM) 300 );
            SendMessage( s_Tooltip, (UINT) TTM_SETDELAYTIME, (WPARAM) TTDT_INITIAL, (LPARAM) MAKELONG( 1000, 0 ) );
            SendMessage( s_Tooltip, (UINT) TTM_SETDELAYTIME, (WPARAM) TTDT_AUTOPOP, (LPARAM) MAKELONG( 30000, 0 ) );
            SendMessage( s_Tooltip, (UINT) TTM_SETDELAYTIME, (WPARAM) TTDT_RESHOW, (LPARAM) MAKELONG( 1000, 0 ) );

            ZeroMemory( &ti, sizeof( TOOLINFO ) );
            ti.cbSize = sizeof( TOOLINFO );
            ti.uFlags   = TTF_SUBCLASS;
            ti.hwnd     = s_hWnd;
            ti.uId      = ID_Save;
            ti.hinst    = s_hInstance;
            ti.lpszText = "Nothing to save yet.";

            ti.rect.left   = 415;
            ti.rect.top    = 172;
            ti.rect.right  = 415 + 77;
            ti.rect.bottom = 172 + 60;

            SendMessage( s_Tooltip, TTM_ADDTOOL, 0, (LPARAM) &ti );

            wc.style         = 0;
            wc.lpszClassName = "DefaultWindow";
            wc.lpfnWndProc   = (WNDPROC) MyDefaultWindowProc;
            wc.hInstance     = hInstance;
            wc.hCursor       = LoadCursor( NULL, IDC_ARROW );
            wc.hIcon         = NULL;

            if( RegisterClass( &wc ) )
            {
               s_hResults = CreateWindowEx( WS_EX_CLIENTEDGE, "DefaultWindow", "Errors found", WS_CHILD, 26, 104, 386 , 216, s_hWnd, NULL, s_hInstance, NULL );
            }
            else
            {
               Alert( "Failed to register 'DefaultWindow' class" );
            }

            while( ( lRet = GetMessage( &msg, NULL, 0, 0 ) ) != 0 )
            {
               if( lRet == -1 )
               {
                  Alert( "Error in main loop!" );
                  break;
               }

               /*
               if( s_iStep >= 0 )
               {
                  // MUST be FIRST!!!
                  if( s_iStep == 3 && s_iSettingTab >= 0 && IsDialogMessage( s_ahSettingTabs[ s_iSettingTab ], &msg ) )
                  {
                      continue;
                  }

                  if( IsDialogMessage( s_ahStepPages[s_iStep], &msg ) )
                  {
                      continue;
                  }
               }
               */

               if( ! IsDialogMessage( GetActiveWindow(), &msg ) )
               {
                  TranslateMessage(&msg);
                  DispatchMessage(&msg);
               }
            }

            OutputDebugString( "After LOOP." );

            if( s_pSH_Alloc )
            {
                if( s_xHB_RootID )
                {
                   s_pSH_Alloc->lpVtbl->Free( s_pSH_Alloc, s_xHB_RootID );
                   s_xHB_RootID = NULL;
                }

                if( s_FWH_RootID )
                {
                   s_pSH_Alloc->lpVtbl->Free( s_pSH_Alloc, s_FWH_RootID );
                   s_FWH_RootID = NULL;
                }

                if( s_StartIn )
                {
                   s_pSH_Alloc->lpVtbl->Free( s_pSH_Alloc, s_StartIn );
                   s_StartIn = NULL;
                }

                s_pSH_Alloc->lpVtbl->Release( s_pSH_Alloc );
            }
         }
      }
   }

   CoUninitialize();

   hb_itemRelease( s_pProject );
   hb_itemRelease( s_pProgress );
   hb_itemRelease( s_pErrorHandler );
   hb_itemRelease( s_pOnFWH );

   OutputDebugString( "Finished." );
}

BOOL CreateForms( void )
{
   TCITEM TabItem;
   RECT Area;

   char sMessage[256];
   int iTab;
   LVCOLUMN LVColumn;

   //-------------------------------------- Step Pages ----------------------------------//
   s_iStep = 0;
   s_ahStepPages[0] = CreateDialog( s_hInstance, "xBuild_1", s_hWnd, StepPagesProc );
   s_iStep = 1;
   s_ahStepPages[1] = CreateDialog( s_hInstance, "xBuild_2", s_hWnd, StepPagesProc );
   s_iStep = 2;
   s_ahStepPages[2] = CreateDialog( s_hInstance, "xBuild_3", s_hWnd, StepPagesProc );
   s_iStep = 3;
   s_ahStepPages[3] = CreateDialog( s_hInstance, "xBuild_4", s_hWnd, StepPagesProc );
   s_iStep = -1;

   ShowWindow( s_ahStepPages[1], SW_HIDE );
   ShowWindow( s_ahStepPages[2], SW_HIDE );
   ShowWindow( s_ahStepPages[3], SW_HIDE );

   //sprintf( sMessage, "Pages: %i, %i, %i, %i\n", s_ahStepPages[0], s_ahStepPages[1], s_ahStepPages[2], s_ahStepPages[3] );
   //OutputDebugString( sMessage );

   //MoveWindow( s_ahStepPages[0], 26, 104, 386 , 216, FALSE );
   s_ahStepFonts[0] = Stretch( s_ahStepPages[0], 386, 216, -1, -1, FALSE, 26, 104, NULL, TRUE );

   //MoveWindow( s_ahStepPages[1], 26, 104, 386 , 216, FALSE );
   s_ahStepFonts[1] = Stretch( s_ahStepPages[1], 386, 216, -1, -1, FALSE, 26, 104, NULL, TRUE );

   //MoveWindow( s_ahStepPages[2], 26, 104, 386 , 216, FALSE );
   s_ahStepFonts[2] = Stretch( s_ahStepPages[2], 386, 216, -1, -1, FALSE, 26, 104, NULL, TRUE );

   //MoveWindow( s_ahStepPages[3], 26, 104, 386 , 216, FALSE );
   s_ahStepFonts[3] = Stretch( s_ahStepPages[3], 386, 216, -1, -1, FALSE, 26, 104, NULL, TRUE );


   wpOrigEditProc = (WNDPROC) SetWindowLong( GetDlgItem( s_ahStepPages[1], ID_Main ), GWL_WNDPROC, (LONG) EditSubclassProc );

   //--------------------------------- Types Tabs ----------------------------------//
   TabItem.mask = TCIF_TEXT ;
   TabItem.iImage = -1;
   TabItem.pszText = sMessage;

   s_hTypeTabControl = CreateWindow( WC_TABCONTROL, "", WS_CHILD | WS_CLIPSIBLINGS | WS_VISIBLE | TCS_FIXEDWIDTH | TCS_FOCUSNEVER,
                                6, 6, 377, 180, s_ahStepPages[2], NULL, s_hInstance, NULL );

   SendMessage( s_hTypeTabControl, WM_SETFONT, (WPARAM) s_hFont, 0 );

   for( iTab = 0; iTab < FILE_TYPES; iTab++ )
   {
      strcpy( TabItem.pszText, s_asTypes[iTab] );
      TabCtrl_InsertItem( s_hTypeTabControl, iTab, &TabItem );
   }

   TabCtrl_SetItemSize( s_hTypeTabControl, 45, 20 );

   Area.left = 0; Area.top = 0; Area.right = 0, Area.bottom = 0;
   TabCtrl_AdjustRect( s_hTypeTabControl, FALSE, &Area );

   // sprintf( sMessage, "Area: %i, %i, %i, %i\n", Area.left, Area.top, Area.right, Area.bottom );
   // OutputDebugString( sMessage );

   // Create ListView Controls 1 for each Tab.
   ZeroMemory( &LVColumn, sizeof( LVCOLUMN ) );

   LVColumn.mask = LVCF_TEXT | LVCF_WIDTH;
   LVColumn.cx = 300;
   LVColumn.pszText = "Name";

   for( iTab = 0; iTab < FILE_TYPES; iTab++ )
   {
      s_ahTypeTabs[ iTab ] = CreateWindowEx( 0, WC_LISTVIEW , "", WS_CHILD | WS_CLIPSIBLINGS | LVS_REPORT | LVS_SHOWSELALWAYS,
                                          0, 0, 377, 186, s_ahStepPages[2], NULL, s_hInstance, NULL );

       SendMessage( s_ahTypeTabs[ iTab ], LVM_SETEXTENDEDLISTVIEWSTYLE, 0,
                    SendMessage( s_ahTypeTabs[ iTab ], LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0 ) |
                    LVS_EX_GRIDLINES | LVS_EX_ONECLICKACTIVATE | LVS_NOSORTHEADER | LVS_EX_FULLROWSELECT );

      // sprintf( sMessage, "ListView: #%i, %p\n", iTab, s_ahTypeTabs[iTab] );
      // OutputDebugString( sMessage );

      //SendMessage( s_ahTypeTabs[ iTab ], WM_SETFONT, SendMessage( s_ahStepPages[2], WM_GETFONT, 0, 0 ), 0 );
      SendMessage( s_ahTypeTabs[ iTab ], WM_SETFONT, (WPARAM) s_hFont, 0 );

      if( SendMessage( s_ahTypeTabs[ iTab ], VIEW_DETAILS, 0, 0 ) == -1 )
      {
         OutputDebugString( "Failed to display ListView Item!\n" );
      }

      if( ListView_InsertColumn( s_ahTypeTabs[ iTab ], iTab, &LVColumn ) == -1 )
      {
         OutputDebugString( "Failed to add column!\n" );
      }

      //MoveWindow( s_ahTypeTabs[ iTab ], 9, 7 + Area.top, 375 - Area.left, 179 - 3 - Area.top, FALSE );
      s_ahTypeFonts[iTab] = Stretch( s_ahTypeTabs[iTab], 375 - Area.left, 179 - 3 - Area.top, -1, -1, FALSE, 9, 7 + Area.top, NULL, TRUE );
   }


   //-------------------------------------- Settings Pages ----------------------------------//
   s_hSettingTabControl = CreateWindow( WC_TABCONTROL, "", WS_CHILD | WS_CLIPSIBLINGS | WS_VISIBLE | TCS_FIXEDWIDTH | TCS_FOCUSNEVER,
                                         6, 6, 377, 207, s_ahStepPages[3], NULL, s_hInstance, NULL );

   SendMessage( s_hSettingTabControl, WM_SETFONT, (WPARAM) s_hFont, 0 );

   for( iTab = 0; iTab < 4; iTab++ )
   {
      strcpy( TabItem.pszText, s_asSettings[ iTab ] );
      TabCtrl_InsertItem( s_hSettingTabControl, iTab, &TabItem );
   }

   TabCtrl_SetItemSize( s_hSettingTabControl, 80, 20 );

   /*
   TabCtrl_GetItemRect( s_hSettingTab, 0, &Area );

   sprintf( sMessage, "Area: %i, %i, %i, %i\n", Area.left, Area.top, Area.right, Area.bottom );
   OutputDebugString( sMessage );
   */

   //sprintf( sMessage, "Settings: %i, %i, %i\n", s_ahSettingTabs[0], s_ahSettingTabs[1], s_ahSettingTabs[2] );
   //OutputDebugString( sMessage );

   // Sub pages of Step 4.
   s_ahSettingTabs[0] = CreateDialog( s_hInstance, "xBuild_41", s_ahStepPages[3], SettingsProc );
   s_ahSettingTabs[1] = CreateDialog( s_hInstance, "xBuild_42", s_ahStepPages[3], SettingsProc );
   s_ahSettingTabs[2] = CreateDialog( s_hInstance, "xBuild_43", s_ahStepPages[3], SettingsProc );
   s_ahSettingTabs[3] = CreateDialog( s_hInstance, "xBuild_44", s_ahStepPages[3], SettingsProc );

   // Coordinates if child of Page
   //MoveWindow( s_ahSettingTabs[0], 8, Area.top + 6, 372, 179, FALSE );
   s_ahSettingFonts[0] = Stretch( s_ahSettingTabs[0], 372, 180, -1, -1, FALSE, 8, Area.top + 6, NULL, TRUE );

   //MoveWindow( s_ahSettingTabs[1], 8, Area.top + 6, 372, 179, FALSE );
   s_ahSettingFonts[1] = Stretch( s_ahSettingTabs[1], 372, 180, -1, -1, FALSE, 8, Area.top + 6, NULL, TRUE );

   //MoveWindow( s_ahSettingTabs[2], 8, Area.top + 6, 372, 179, FALSE );
   s_ahSettingFonts[2] = Stretch( s_ahSettingTabs[2], 372, 180, -1, -1, FALSE, 8, Area.top + 6, NULL, TRUE );

   //MoveWindow( s_ahSettingTabs[2], 8, Area.top + 6, 372, 179, FALSE );
   s_ahSettingFonts[3] = Stretch( s_ahSettingTabs[3], 372, 180, -1, -1, FALSE, 8, Area.top + 6, NULL, TRUE );

   ShowWindow( s_ahSettingTabs[0], SW_HIDE );
   ShowWindow( s_ahSettingTabs[1], SW_HIDE );
   ShowWindow( s_ahSettingTabs[2], SW_HIDE );

   SelectStepPage( 0, TRUE );

   return TRUE;
}

void SetProject( void )
{
   char sMessage[256];
   char sFile[ MAX_PATH ], sDefines[ 2048 ], sMultiFiles[ MAX_PATH * 16 ], sFlags[ 256 ], sLib[ MAX_PATH ];
   #ifdef LINK_SUPPORTED
      char sLinkFlags[256];
   #endif
   //char sRunArguments[256];
   HB_ITEM NoAutoFWH, LoadedProperties, Item, Flags, Lib, Exe, LinkFlags;
   int iTab, iIndex, iLen;
   PHB_DYNS pSym = hb_dynsymFindName( "TMakeProject" );

   NoAutoFWH.type        = HB_IT_NIL;
   LoadedProperties.type = HB_IT_NIL;
   Item.type             = HB_IT_NIL;
   Flags.type            = HB_IT_NIL;
   Lib.type              = HB_IT_NIL;
   Exe.type              = HB_IT_NIL;
   LinkFlags.type        = HB_IT_NIL;

   // Save the lNoAutoFWH flag
   hb_objSendMsg( s_pProject, "lNoAutoFWH", 0 );
   hb_itemForwardValue( &NoAutoFWH, hb_stackReturnItem() );

   // Save the Project's existing Loaded Properties.
   hb_objSendMsg( s_pProject, "aLoadedProperties", 0 );
   hb_itemForwardValue( &LoadedProperties, hb_stackReturnItem() );

   // OutputDebugString( "Creating New Project.\n" );
   if( pSym )
   {
      hb_vmPushSymbol( pSym->pSymbol );
      hb_vmPushNil();
      hb_vmDo( 0 );

      // Safety!
      hb_vmRequestReset();

      hb_itemForwardValue( s_pProject, hb_stackReturnItem() );
   }
   else
   {
      Alert( "Sorry, cound't locate: 'TMakeProject()'!" );
   }

   // Restore lNoAutoFWH
   hb_objSendMsg( s_pProject, "_lNoAutoFWH", 1, &NoAutoFWH );

   //-------------------- Target --------------------------
   GetDlgItemText( s_ahStepPages[0], ID_Target, sFile, MAX_PATH );

   // sprintf( sMessage, "Target: '%s'\n", sFile );
   // OutputDebugString( sMessage );

   hb_itemPutC( &Item, sFile );
   hb_objSendMsg( s_pProject, "New", 1, &Item );
   //-----------------------------------------------------

   //--------------------- Set_xHB() ------------------------------
   GetDlgItemText( s_ahSettingTabs[0], ID_xHB_Lib, sLib, 256 );
   iLen = strlen( sLib );
   if( sLib[ iLen - 1 ] == '\\' )
   {
      iLen--;
      sLib[ iLen ] = '\0';
   }
   hb_itemPutCL( &Lib, sLib, iLen );

   hb_itemPutC( &Exe, s_xHB_Exe );

   GetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, sFile, MAX_PATH );
   hb_itemPutC( &Item, sFile );

   hb_objSendMsg( s_pProject, "Set_xHB", 4, &Item, &Flags, &Lib, &Exe );
   //-------------------------------------------------------------------

   //--------------------------- Set C Compiler ------------------------
   GetDlgItemText( s_ahSettingTabs[1], ID_C_Root, sFile, MAX_PATH );
   hb_itemPutC( &Item, sFile );

   #ifdef LINK_SUPPORTED
      GetDlgItemText( s_ahSettingTabs[1], ID_Link_Flags, sLinkFlags, 256 );
      if( sFlags[0] != '<' )
      {
         hb_itemPutC( &LinkFlags, sLinkFlags );
      }
   #endif

   switch( s_C_Compiler[0] )
   {
      case 'x' :
      {
         hb_objSendMsg( s_pProject, "Set_XCC", 3, &Item, &Flags, &LinkFlags );
         break;
      }

      case 'b' :
      {
         hb_objSendMsg( s_pProject, "Set_BCC", 3, &Item, &Flags, &LinkFlags );
         break;
      }

      case 'c' :
      {
         hb_objSendMsg( s_pProject, "Set_VC", 3, &Item, &Flags, &LinkFlags );
         break;
      }

      case 'g' :
      {
         hb_objSendMsg( s_pProject, "Set_MingW", 3, &Item, &Flags, &LinkFlags );
         break;
      }

      case 'p' :
      {
         hb_objSendMsg( s_pProject, "Set_POCC", 3, &Item, &Flags, &LinkFlags );
         break;
      }
   }

   //--------------------- Set_FWH() ------------------------------
   GetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, sFile, MAX_PATH );

   if( sFile[0] )
   {
      hb_itemPutC( &Item, sFile );

      GetDlgItemText( s_ahSettingTabs[2], ID_FWH_Lib, sLib, 256 );
      hb_itemPutC( &Lib, sLib );

      hb_objSendMsg( s_pProject, "Set_FWH", 2, &Item, &Lib );
   }

   //------------------- OutputFolder --------------------
   GetDlgItemText( s_ahStepPages[0], ID_Output, sFile, MAX_PATH );

   // sprintf( sMessage, "Target: '%s'\n", sFile );
   // OutputDebugString( sMessage );

   hb_itemPutC( &Item, sFile );
   hb_objSendMsg( s_pProject, "_OutputFolder", 1, &Item );
   //-----------------------------------------------------

   hb_itemPutL( &Item, IsDlgButtonChecked( s_ahStepPages[0], ID_Clean ) );
   hb_objSendMsg( s_pProject, "_lClean", 1, &Item );

   hb_itemPutL( &Item, IsDlgButtonChecked( s_ahStepPages[0], ID_Link ) );
   hb_objSendMsg( s_pProject, "_lLink", 1, &Item );

   #if defined( DEMO ) || defined( PERSONAL )
      Item.item.asLogical.value = FALSE;
   #else
      Item.item.asLogical.value = IsDlgButtonChecked( s_ahSettingTabs[1], ID_C_Debug );
   #endif
   hb_objSendMsg( s_pProject, "_lDebug", 1, &Item );

   #if defined( DEMO ) || defined( PERSONAL )
      Item.item.asLogical.value = FALSE;
   #else
      Item.item.asLogical.value = IsDlgButtonChecked( s_ahStepPages[0], ID_MT );
   #endif
   hb_objSendMsg( s_pProject, "_lMT", 1, &Item );

   Item.item.asLogical.value = IsDlgButtonChecked( s_ahStepPages[0], ID_GUI );
   hb_objSendMsg( s_pProject, "_lGUI", 1, &Item );

   Item.item.asLogical.value = IsDlgButtonChecked( s_ahStepPages[0], ID_XBP );
   hb_objSendMsg( s_pProject, "_lXBP", 1, &Item );

   #if defined( DEMO ) || defined( PERSONAL )
      Item.item.asLogical.value = FALSE;
   #else
      Item.item.asLogical.value = IsDlgButtonChecked( s_ahStepPages[0], ID_UseDLL );
   #endif
   hb_objSendMsg( s_pProject, "_lUseDLL", 1, &Item );

   //---------------- Main -------------------------------
   if( GetDlgItemText( s_ahStepPages[1], ID_Main, sFile, MAX_PATH ) )
   {
      sprintf( sMessage, "Main: '%s'\n", sFile );
      OutputDebugString( sMessage );

      hb_itemPutC( &Item, sFile );
      hb_objSendMsg( s_pProject, "AddFiles", 1, &Item );
   }
   //-----------------------------------------------------

   //---------------- Libs -------------------------------
   GetDlgItemText( s_ahStepPages[1], ID_Libs, sMultiFiles, MAX_PATH * 16 );
   hb_itemPutC( &Item, sMultiFiles );
   hb_objSendMsg( s_pProject, "_LibFolders", 1, &Item );
   //-----------------------------------------------------

   //---------------- Includes -------------------------------
   GetDlgItemText( s_ahStepPages[1], ID_Includes, sMultiFiles, MAX_PATH * 16 );
   hb_itemPutC( &Item, sMultiFiles );
   hb_objSendMsg( s_pProject, "SetIncludeFolders", 1, &Item );
   //-----------------------------------------------------

   //---------------- Defines -------------------------------
   GetDlgItemText( s_ahStepPages[1], ID_Defines, sDefines, 2048 );
   hb_itemPutC( &Item, sDefines );
   hb_objSendMsg( s_pProject, "SetDefines", 1, &Item );
   //-----------------------------------------------------

   //---------------- Modules ----------------------------
   for( iTab = 0; iTab < FILE_TYPES; iTab++ )
   {
      /*
      for( iIndex = 0; iIndex < ListView_GetItemCount( s_ahTypeTabs[ iTab ] ); iIndex++ )
      {
         sFile[0] = '\0';
         ListView_GetItemText( s_ahTypeTabs[ iTab ], iIndex, 0, sFile, MAX_PATH );

         if( sFile[0] == '\0' )
         {
            break;
         }

         hb_itemPutC( &Item, sFile );
         hb_objSendMsg( s_pProject, "AddFiles", 1, &Item );
      }
      */

      //OutputDebugString( "Modules\n" );

      iIndex = -1;//ListView_GetTopIndex( s_ahTypeTabs[ iTab ] );

      while( ( iIndex = ListView_GetNextItem( s_ahTypeTabs[ iTab ], iIndex, LVNI_ALL ) ) >= 0 )
      {
         sFile[0] = '\0';
         ListView_GetItemText( s_ahTypeTabs[ iTab ], iIndex, 0, sFile, MAX_PATH );

         OutputDebugString( sFile );
         OutputDebugString( "\n" );

         // May point to different buffer!!!
         hb_itemPutC( &Item, sFile );
         hb_objSendMsg( s_pProject, "AddFiles", 1, &Item );
      }

   }
   //-----------------------------------------------------

   GetDlgItemText( s_ahSettingTabs[0], ID_xHB_Flags, sFlags, 256 );
   if( sFlags[0] != '<' )
   {
      hb_itemPutC( &Flags, sFlags );
      hb_objSendMsg( s_pProject, "_MyPRG_Flags", 1, &Flags );
   }

   hb_itemPutL( &Item, IsDlgButtonChecked( s_ahSettingTabs[0], ID_xHB_Debug ) );
   hb_objSendMsg( s_pProject, "_lPRG_Debug", 1, &Item );

   hb_itemPutL( &Item, IsDlgButtonChecked( s_ahSettingTabs[0], ID_xHB_ClassicDebug ) );
   hb_objSendMsg( s_pProject, "_lPRG_ClassicDebug", 1, &Item );

   GetDlgItemText( s_ahSettingTabs[1], ID_C_Flags, sFlags, 256 );
   if( sFlags[0] != '<' )
   {
      hb_itemPutC( &Flags, sFlags );
      hb_objSendMsg( s_pProject, "_MyC_Flags", 1, &Flags );
   }

   GetDlgItemText( s_ahSettingTabs[3], ID_RUN_Arguments, s_sArguments, 256 );
   hb_itemPutC( &Item, s_sArguments );
   hb_objSendMsg( s_pProject, "_RunArguments", 1, &Item );

   GetDlgItemText( s_ahSettingTabs[3], ID_RUN_StartIn, s_sStartIn, _MAX_DIR );
   hb_itemPutC( &Item, s_sStartIn );
   hb_objSendMsg( s_pProject, "_StartIn", 1, &Item );

   if( ! IsDlgButtonChecked( s_ahSettingTabs[3], ID_RUN_Ask ) )
   {
      hb_itemPutL( &Item, IsDlgButtonChecked( s_ahSettingTabs[3], ID_RUN_Auto ) );
      hb_objSendMsg( s_pProject, "_lAutoRun", 1, &Item );
   }

   // Reset lAddedDependencies to avoid unneeded refreshing of the project file.
   hb_itemPutL( &Item, ( s_bAdded || s_bDeleted ) );
   s_bAdded = FALSE; s_bDeleted = FALSE;
   hb_objSendMsg( s_pProject, "_lAddedDependencies", 1, &Item );

   // Restore the Projects's Loaded Properties.
   hb_objSendMsg( s_pProject, "_aLoadedProperties", 1, &LoadedProperties );

   hb_itemClear( &Item );
   hb_itemClear( &Flags );
   hb_itemClear( &Lib );
   hb_itemClear( &Exe );
   hb_itemClear( &LoadedProperties );
   hb_itemClear( &LinkFlags );
}

void InitControls( void )
{
   //char sMessage[256];
   char sFile[ MAX_PATH ], sDefines[ 2048 ], *pTemp;
   HB_ITEM Item, LiteralDependancies;
   ULONG ulIndex;
   char xHB_xHB[]          = "C:\\xhb\\bin\\xhb.exe";
   char xHarbour_xHB[]     = "C:\\xharbour\\bin\\xhb.exe";
   char xHarbour_Harbour[] = "C:\\xharbour\\bin\\harbour.exe";
   char cDrive;
   int iTab;

   RECT rcClient;  // Client area of parent window
   int cyVScroll;  // Height of scroll bar arrow
   HWND hwndPB;

   OutputDebugString( "Initializing.\n" );

   Item.type                = HB_IT_NIL;
   LiteralDependancies.type = HB_IT_NIL;

   s_bAddResourceAsk = TRUE;

   if( s_pProject && HB_IS_OBJECT( s_pProject ) )
   {
      OutputDebugString( "Setting.\n" );

      //-------------------- Target --------------------------
      hb_objSendMsg( s_pProject, "TargetFolder", 0 );
      strcpy( sFile, hb_stackReturnItem()->item.asString.value );

      if( sFile[0] )
      {
         strcat( sFile, "\\" );
      }

      hb_objSendMsg( s_pProject, "cFile", 0 );
      strcat( sFile, hb_stackReturnItem()->item.asString.value );

      // sprintf( sMessage, "Target: '%s'\n", sFile );
      // OutputDebugString( sMessage );

      SetDlgItemText( s_ahStepPages[0], ID_Target, sFile );
      ValidateTarget( FALSE );
      //-----------------------------------------------------

      //------------------ OutputFolder ---------------------
      hb_objSendMsg( s_pProject, "OutputFolder", 0 );

      // sprintf( sMessage, "OutputFolder: '%s'\n", hb_stackReturnItem()->item.asString.value );
      // OutputDebugString( sMessage );

      SetDlgItemText( s_ahStepPages[0], ID_Output, hb_stackReturnItem()->item.asString.value );
      //-----------------------------------------------------

      hb_objSendMsg( s_pProject, "lClean", 0 );
      if( hb_stackReturnItem()->item.asLogical.value )
      {
         CheckDlgButton( s_ahStepPages[0], ID_Clean, BST_CHECKED );
      }
      else
      {
         CheckDlgButton( s_ahStepPages[0], ID_Clean, BST_UNCHECKED );
      }

      hb_objSendMsg( s_pProject, "lDebug", 0 );
      if( hb_stackReturnItem()->item.asLogical.value )
      {
         CheckDlgButton( s_ahSettingTabs[1], ID_C_Debug, BST_CHECKED );
      }
      else
      {
         CheckDlgButton( s_ahSettingTabs[1], ID_C_Debug, BST_UNCHECKED );
      }


      hb_objSendMsg( s_pProject, "lMT", 0 );
      if( hb_stackReturnItem()->item.asLogical.value )
      {
         CheckDlgButton( s_ahStepPages[0], ID_MT, BST_CHECKED );
      }
      else
      if( hb_stackReturnItem()->item.asLogical.value )
      {
         CheckDlgButton( s_ahStepPages[0], ID_MT, BST_UNCHECKED );
      }

      hb_objSendMsg( s_pProject, "lGUI", 0 );
      if( hb_stackReturnItem()->item.asLogical.value )
      {
         CheckDlgButton( s_ahStepPages[0], ID_GUI, BST_CHECKED );
      }
      else
      {
         CheckDlgButton( s_ahStepPages[0], ID_GUI, BST_UNCHECKED );
      }

      hb_objSendMsg( s_pProject, "lXbp", 0 );
      if( hb_stackReturnItem()->item.asLogical.value )
      {
         CheckDlgButton( s_ahStepPages[0], ID_XBP, BST_CHECKED );
      }
      else
      {
         CheckDlgButton( s_ahStepPages[0], ID_XBP, BST_UNCHECKED );
      }

      hb_objSendMsg( s_pProject, "lUseDLL", 0 );
      if( hb_stackReturnItem()->item.asLogical.value )
      {
         CheckDlgButton( s_ahStepPages[0], ID_UseDLL, BST_CHECKED );
      }
      else
      {
         CheckDlgButton( s_ahStepPages[0], ID_UseDLL, BST_UNCHECKED );
      }

      //---------------- Libs -------------------------------
      hb_objSendMsg( s_pProject, "LibFolders", 0 );
      SetDlgItemText( s_ahStepPages[1], ID_Libs, hb_stackReturnItem()->item.asString.value );
      //-----------------------------------------------------

      //---------------- Includes -------------------------------
      hb_objSendMsg( s_pProject, "IncludeFolders", 0 );
      SetDlgItemText( s_ahStepPages[1], ID_Includes, hb_stackReturnItem()->item.asString.value );
      //-----------------------------------------------------

      //---------------- Defines -------------------------------
      hb_objSendMsg( s_pProject, "Defines", 0 );
      strcpy( sDefines, hb_stackReturnItem()->item.asString.value );

      if( sDefines[0]  )
      {
         // sprintf( sMessage, "Defines: '%s'\n", sDefines );
         // OutputDebugString( sMessage );

         pTemp = sDefines;

         while( ( pTemp = strstr( pTemp, "-D" ) ) != NULL )
         {
            pTemp[0] = ';';
            pTemp[1] = ' ';
         }

         SetDlgItemText( s_ahStepPages[1], ID_Defines, sDefines + 2 );
      }
      //-----------------------------------------------------

      // ------------------ Dependancies --------------------
      hb_objSendMsg( s_pProject, "aLiteralDependancies", 0 );
      hb_itemForwardValue( &LiteralDependancies, hb_stackReturnItem() );
      //-----------------------------------------------------

      //---------------- Main -------------------------------
      if( hb_stricmp( sFile + strlen( sFile ) - 3, "exe" ) == 0 )
      {
         if( LiteralDependancies.item.asArray.value->ulLen )
         {
            hb_arrayGet( &LiteralDependancies, 1, &Item );

            // sprintf( sMessage, "Main: '%s'\n", Item.item.asString.value );
            // OutputDebugString( sMessage );

            if( HB_IS_STRING( &Item ) )
            {
               SetDlgItemText( s_ahStepPages[1], ID_Main, Item.item.asString.value );
            }
         }

         ulIndex = 2;
      }
      else
      {
         SetDlgItemText( s_ahStepPages[1], ID_Main, "" );
         ulIndex = 1;
      }
      //-----------------------------------------------------

      //---------------- Modules ----------------------------
      for( iTab = 0; iTab < FILE_TYPES; iTab++ )
      {
         ListView_DeleteAllItems( s_ahTypeTabs[ iTab ] );
      }

      for( /* ulIndex initialized above */; ulIndex <= LiteralDependancies.item.asArray.value->ulLen; ulIndex++ )
      {
         hb_arrayGet( &LiteralDependancies, ulIndex, &Item );

         //sprintf( sMessage, "Module #%i: '%s'\n", ulIndex, Item.item.asString.value );
         //OutputDebugString( sMessage );

         AddModule( Item.item.asString.value );
      }
      //-----------------------------------------------------
   }
   else
   {
      PHB_DYNS pSym = hb_dynsymFindName( "TMakeProject" );

      OutputDebugString( "Creating New Project.\n" );

      if( pSym )
      {
         hb_vmPushSymbol( pSym->pSymbol );
         hb_vmPushNil();
         hb_vmDo( 0 );

         // Safety!
         hb_vmRequestReset();

         hb_itemForwardValue( s_pProject, hb_stackReturnItem() );

         pSym = hb_dynsymFindName( "LoadIni" );

         if( pSym )
         {
            hb_vmPushSymbol( pSym->pSymbol );
            hb_vmPushNil();
            hb_vmPush( s_pProject );
            hb_vmDo( 1 );

            // Safety!
            hb_vmRequestReset();
         }
      }
      else
      {
         Alert( "Sorry, cound't locate: 'TMakeProject()'!" );
      }

      CheckDlgButton( s_ahStepPages[0], ID_XBP, BST_CHECKED );
   }

   //---------------- xHarbour ---------------------------
   hb_objSendMsg( s_pProject, "xHB_Root", 0 );
   if( HB_IS_STRING( hb_stackReturnItem() ) && hb_stackReturnItem()->item.asString.value[0] )
   {
      SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, hb_stackReturnItem()->item.asString.value );
      strcpy( sFile, hb_stackReturnItem()->item.asString.value );
      strcat( sFile, "bin\\" );
   }
   else
   {
      SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, "" );
   }

   hb_objSendMsg( s_pProject, "xHB_Executable", 0 );
   if( HB_IS_STRING( hb_stackReturnItem() ) && hb_stackReturnItem()->item.asString.value[0] )
   {
      strcpy( s_xHB_Exe, hb_stackReturnItem()->item.asString.value );
      strcat( sFile, s_xHB_Exe );
   }
   else
   {
      sFile[0] = '\0';
      s_xHB_Exe[0] = '\0';
   }

   hb_objSendMsg( s_pProject, "MyPRG_Flags", 0 );
   if( HB_IS_STRING( hb_stackReturnItem() ) && hb_stackReturnItem()->item.asString.length > 0 && hb_stackReturnItem()->item.asString.value[0] != ' ' )
   {
      SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Flags, hb_stackReturnItem()->item.asString.value );
   }
   else
   {
      SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Flags, "<default>" );
   }

   hb_objSendMsg( s_pProject, "lPRG_Debug", 0 );
   if( hb_stackReturnItem()->item.asLogical.value )
   {
      CheckDlgButton( s_ahSettingTabs[0], ID_xHB_Debug, BST_CHECKED );
   }
   else
   {
      CheckDlgButton( s_ahSettingTabs[0], ID_xHB_Debug, BST_UNCHECKED );
   }

   hb_objSendMsg( s_pProject, "lPRG_ClassicDebug", 0 );
   if( hb_stackReturnItem()->item.asLogical.value )
   {
      CheckDlgButton( s_ahSettingTabs[0], ID_xHB_ClassicDebug, BST_CHECKED );
   }
   else
   {
      CheckDlgButton( s_ahSettingTabs[0], ID_xHB_ClassicDebug, BST_UNCHECKED );
   }

   if( sFile[0] && PathFileExists( sFile ) )
   {
      char sRelative[ MAX_PATH ];

      GetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, sFile, MAX_PATH );

      hb_objSendMsg( s_pProject, "xHB_LibFolder", 0 );

      if( PathRelativePathTo( sRelative,
                          sFile,
                          FILE_ATTRIBUTE_DIRECTORY,
                          hb_stackReturnItem()->item.asString.value,
                          FILE_ATTRIBUTE_DIRECTORY ) )
      {
         if( sRelative[0] == '\\' )
         {
            SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Lib, sRelative + 1 );
         }
         else if( sRelative[0] == '.' && sRelative[1] == '\\' )
         {
            SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Lib, sRelative + 2 );
         }
         else
         {
            SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Lib, sRelative );
         }
      }
      else
      {
         Alert( "Lib folder must be a sub folder of the 'Root folder'!" );
      }

      goto Set_C;
   }

   //TODO: FIX!!!
   // Assume default.
   SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Lib, "lib" );

   GetModuleFileName( NULL, sFile, MAX_PATH );
   pTemp = strrchr( sFile, '\\' );

   if( pTemp )
   {
      // Intentionally 1 and not 0!
      pTemp[1] = '\0';
      strcat( sFile, "xhb.exe" );

      if( PathFileExists( sFile ) )
      {
         pTemp[0] = '\0';
         pTemp = strrchr( sFile, '\\' );

         if( pTemp )
         {
            pTemp[0] = '\0';
            SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, sFile );

            // sprintf( sMessage, "Found: '%s'\n", sFile );
            // OutputDebugString( sMessage );
            goto Set_C;
         }
      }
   }

   GetClientRect( s_ahSettingTabs[0], &rcClient );

   cyVScroll = GetSystemMetrics( SM_CYVSCROLL );

   hwndPB = CreateWindowEx( 0, PROGRESS_CLASS,
                           (LPSTR) NULL, WS_CHILD | WS_VISIBLE,
                           0, rcClient.bottom - cyVScroll,
                           rcClient.right, cyVScroll,
                           s_ahSettingTabs[0], (HMENU) 0, s_hInstance, NULL );

   SendMessage( hwndPB, PBM_SETRANGE, 0, MAKELPARAM( 0, 3 + 23 * 3 - 1 ) );
   SendMessage( hwndPB, PBM_SETSTEP, (WPARAM) 1, 0 );

   if( PathFileExists( xHB_xHB + 2 ) )
   {
      SendMessage( hwndPB, PBM_STEPIT, 0, 0 );
      WizardEvents();

      xHB_xHB[0] = _getdrive() + 'A' - 1;
      xHB_xHB[6] = '\0';

      SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, xHB_xHB );

      // sprintf( sMessage, "Found: '%s'\n", xHB_xHB );
      // OutputDebugString( sMessage );
      goto Set_C;
   }

   for( cDrive = 'C'; cDrive <= 'Z'; cDrive++ )
   {
      SendMessage( hwndPB, PBM_STEPIT, 0, 0 );
      //WizardEvents();

      xHB_xHB[0] = cDrive;

      if( PathFileExists( xHB_xHB ) )
      {
         xHB_xHB[6] = '\0';
         SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, xHB_xHB );

         // sprintf( sMessage, "Found: '%s'\n", xHB_xHB );
         // OutputDebugString( sMessage );
         goto Set_C;
      }
   }

   if( PathFileExists( xHarbour_xHB + 2 ) )
   {
      SendMessage( hwndPB, PBM_STEPIT, 0, 0 );
      WizardEvents();

      xHarbour_xHB[0] = _getdrive() + 'A' - 1;
      xHarbour_xHB[11] = '\0';

      SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, xHarbour_xHB );

      // sprintf( sMessage, "Found: '%s'\n", xHarbour_xHB );
      // OutputDebugString( sMessage );
      goto Set_C;
   }

   for( cDrive = 'C'; cDrive <= 'Z'; cDrive++ )
   {
      SendMessage( hwndPB, PBM_STEPIT, 0, 0 );
      //WizardEvents();

      xHarbour_xHB[0] = cDrive;

      if( PathFileExists( xHarbour_xHB ) )
      {
         xHarbour_xHB[11] = '\0';

         SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, xHarbour_xHB );

         // sprintf( sMessage, "Found: '%s'\n", xHarbour_xHB );
         // OutputDebugString( sMessage );
         goto Set_C;
      }
   }

   if( PathFileExists( xHarbour_Harbour + 2 ) )
   {
      SendMessage( hwndPB, PBM_STEPIT, 0, 0 );
      WizardEvents();

      xHarbour_Harbour[0] = _getdrive() + 'A' - 1;
      xHarbour_Harbour[11] = '\0';

      SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, xHarbour_Harbour );

      strcpy( s_xHB_Exe, "harbour.exe" );

      // sprintf( sMessage, "Found: '%s'\n", xHarbour_Harbour );
      // OutputDebugString( sMessage );
      goto Set_C;
   }

   for( cDrive = 'C'; cDrive <= 'Z'; cDrive++ )
   {
      SendMessage( hwndPB, PBM_STEPIT, 0, 0 );
      //WizardEvents();

      xHarbour_Harbour[0] = cDrive;

      if( PathFileExists( xHarbour_Harbour ) )
      {
         xHarbour_Harbour[11] = '\0';
         SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, xHarbour_Harbour );

         strcpy( s_xHB_Exe, "harbour.exe" );

         // sprintf( sMessage, "Found: '%s'\n", xHarbour_Harbour );
         // OutputDebugString( sMessage );
         goto Set_C;
      }
   }

   Set_C :

      hb_objSendMsg( s_pProject, "C_Root", 0 );
      if( HB_IS_STRING( hb_stackReturnItem() ) )
      {
         SetDlgItemText( s_ahSettingTabs[1], ID_C_Root, hb_stackReturnItem()->item.asString.value );
      }
      else
      {
         SetDlgItemText( s_ahSettingTabs[1], ID_C_Root, "" );
      }

      hb_objSendMsg( s_pProject, "C_Executable", 0 );
      if( HB_IS_STRING( hb_stackReturnItem() ) )
      {
         strcpy( s_C_Compiler, hb_stackReturnItem()->item.asString.value );

         switch( s_C_Compiler[0] )
         {
            case 'b' :
            {
               SendMessage( GetDlgItem( s_ahSettingTabs[1], ID_C_Compiler ), CB_SETCURSEL, 0, 0 );
               break;
            }

            case 'g' :
            {
               SendMessage( GetDlgItem( s_ahSettingTabs[1], ID_C_Compiler ), CB_SETCURSEL, 1, 0 );
               break;
            }

            case 'c' :
            {
               SendMessage( GetDlgItem( s_ahSettingTabs[1], ID_C_Compiler ), CB_SETCURSEL, 2, 0 );
               break;
            }

            case 'p' :
            {
               SendMessage( GetDlgItem( s_ahSettingTabs[1], ID_C_Compiler ), CB_SETCURSEL, 3, 0 );
               break;
            }

            case 'x' :
            {
               SendMessage( GetDlgItem( s_ahSettingTabs[1], ID_C_Compiler ), CB_SETCURSEL, 4, 0 );
               break;
            }

            default :
            {
               Alert( "Unexpected C Compiler!" );
               s_C_Compiler[0] = '\0';
            }
         }
      }
      else
      {
         s_C_Compiler[0] = '\0';
      }

      #if 0
         if( hb_stricmp( s_xHB_Exe, "xhb.exe" ) == 0 )
         {
            strcpy( s_C_Compiler, "xcc.exe" );
            SendMessage( GetDlgItem( s_ahSettingTabs[1], ID_C_Compiler ), CB_SETCURSEL, 4, 0 );

            GetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, sFile, MAX_PATH );
            SetDlgItemText( s_ahSettingTabs[1], ID_C_Root, sFile );
         }
      #endif

      hb_objSendMsg( s_pProject, "MyC_Flags", 0 );
      if( HB_IS_STRING( hb_stackReturnItem() ) && hb_stackReturnItem()->item.asString.length > 0 && hb_stackReturnItem()->item.asString.value[0] != ' ' )
      {
         SetDlgItemText( s_ahSettingTabs[1], ID_C_Flags, hb_stackReturnItem()->item.asString.value );
      }
      else
      {
         SetDlgItemText( s_ahSettingTabs[1], ID_C_Flags, "<default>" );
      }

      hb_objSendMsg( s_pProject, "RunArguments", 0 );
      if( HB_IS_STRING( hb_stackReturnItem() ) )
      {
         strncpy( s_sArguments, hb_stackReturnItem()->item.asString.value, hb_stackReturnItem()->item.asString.length );
         SetDlgItemText( s_ahSettingTabs[3], ID_RUN_Arguments, s_sArguments );
      }

      hb_objSendMsg( s_pProject, "StartIn", 0 );
      if( HB_IS_STRING( hb_stackReturnItem() ) )
      {
         strncpy( s_sStartIn, hb_stackReturnItem()->item.asString.value, hb_stackReturnItem()->item.asString.length );
         SetDlgItemText( s_ahSettingTabs[3], ID_RUN_StartIn, s_sStartIn );
      }

      hb_objSendMsg( s_pProject, "lAutoRun", 0 );
      if( HB_IS_LOGICAL( hb_stackReturnItem() ) )
      {
         if( hb_stackReturnItem()->item.asLogical.value )
         {
            CheckRadioButton( s_ahSettingTabs[3], ID_RUN_Auto, ID_RUN_Ask, ID_RUN_Auto );
         }
         else
         {
            CheckRadioButton( s_ahSettingTabs[3], ID_RUN_Auto, ID_RUN_Ask, ID_RUN_Dont );
         }
      }
      else
      {
         CheckRadioButton( s_ahSettingTabs[3], ID_RUN_Auto, ID_RUN_Ask, ID_RUN_Ask );
      }

      DestroyWindow( hwndPB );

      s_bAdded = FALSE;
      s_bDeleted = FALSE;
}

BOOL ValidateFWH_Root( BOOL bForce )
{
   char sFile[ MAX_PATH ], sErr[ MAX_PATH + 64 ];
   WIN32_FIND_DATA FindFileData;
   HANDLE hFind;

   GetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, sFile, MAX_PATH );

   if( sFile[0] == '\0' )
   {
      return TRUE;
   }

   if( sFile[ strlen( sFile ) - 1 ] == '\\' )
   {
      strcat( sFile, "include\\fivewin.ch" );
   }
   else
   {
      strcat( sFile, "\\include\\fivewin.ch" );
   }

   hFind = FindFirstFile( sFile, &FindFileData );

   if( hFind == INVALID_HANDLE_VALUE )
   {
      sprintf( sErr, "Sorry, couldn't locate FWH header at: '%s'!", sFile );
      Alert( sErr );
      return FALSE;
   }
   else
   {
      FindClose( hFind );
   }

   return TRUE;
}

BOOL ValidateFWH_Lib( BOOL bForce )
{
   char sFile[ MAX_PATH ], sLib[ MAX_PATH ], sErr[ MAX_PATH + 64 ];
   WIN32_FIND_DATA FindFileData;
   HANDLE hFind;

   GetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, sFile, MAX_PATH );

   if( sFile[0] == '\0' )
   {
      if( bForce )
      {
         //Alert( "Sorry, FWH Root must be specified before you can select the LIB sub folder!" );
         //return FALSE;
      }

      return TRUE;
   }

   if( sFile[ strlen( sFile ) - 1 ] != '\\' )
   {
      strcat( sFile, "\\" );
   }

   GetDlgItemText( s_ahSettingTabs[2], ID_FWH_Lib, sLib, MAX_PATH );

   strcat( sFile, sLib );

   if( sFile[ strlen( sFile ) - 1 ] != '\\' )
   {
      strcat( sFile, "\\" );
   }

   if( strcmp( s_C_Compiler, "xcc.exe" ) == 0 || strcmp( s_C_Compiler, "cl.exe" ) == 0 )
   {
      strcat( sFile, "FiveHMX.lib" );
   }
   else
   {
      strcat( sFile, "FiveHX.lib" );
   }

   ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
   hFind = FindFirstFile( sFile, &FindFileData );

   if( hFind == INVALID_HANDLE_VALUE )
   {
      sprintf( sErr, "Sorry, couldn't locate FWH R/T support at: '%s'!", sFile );
      Alert( sErr );
      return FALSE;
   }
   else
   {
      FindClose( hFind );
   }

   return TRUE;
}

void InitFWH( void )
{
   //char sMessage[256];
   char sFile[ MAX_PATH ], sLib[ MAX_PATH ];
   char cDrive;
   char FWH_FWH[]      = "C:\\fwh\\lib\\fivehcm.lib";
   char FW24_FWH[]     = "C:\\fw24\\lib\\fivehcm.lib";
   char FIVEWIN_FWH[]  = "C:\\fivewin\\lib\\fivehcm.lib";
   char FWH_XFWH[]      = "C:\\fwh\\libx\\fivehcm.lib";
   char FW24_XFWH[]     = "C:\\fw24\\libx\\fivehcm.lib";
   char FIVEWIN_XFWH[]  = "C:\\fivewin\\libx\\fivehcm.lib";
   HWND hFWHRoot = GetDlgItem( s_ahSettingTabs[2], ID_FWH_Root );
   HB_ITEM Lib, Flags, Item;
   RECT rcClient;  // Client area of parent window
   int cyVScroll;  // Height of scroll bar arrow
   HWND hwndPB;

   static BOOL bRunning = FALSE;

   if( bRunning )
   {
      return;
   }
   else
   {
      bRunning = TRUE;
   }

   Lib.type   = HB_IT_NIL;
   Flags.type = HB_IT_NIL;
   Item.type  = HB_IT_NIL;

   #ifdef DRIVES
      ULONG uDriveMask = _getdrives(), uWorkMask;
      int iDrives = 0;
   #endif

   SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Lib, "lib" );
   hb_objSendMsg( s_pProject, "FWH_Root", 0 );

   strcpy( sFile, hb_stackReturnItem()->item.asString.value );

   if( sFile[ strlen( sFile ) - 1 ] == '\\' )
   {
      strcat( sFile, "include\\fivewin.ch" );
   }
   else
   {
      strcat( sFile, "\\include\\fivewin.ch" );
   }

   if( PathFileExists( sFile ) )
   {
      SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, hb_stackReturnItem()->item.asString.value );
      bRunning = FALSE;

      hb_itemClear( &Lib );
      hb_itemClear( &Flags );
      hb_itemClear( &Item );

      return;
   }

   // Progress...
   GetClientRect( s_ahSettingTabs[2], &rcClient );

   cyVScroll = GetSystemMetrics( SM_CYVSCROLL );

   hwndPB = CreateWindowEx( 0, PROGRESS_CLASS,
                           (LPSTR) NULL, WS_CHILD | WS_VISIBLE,
                           0, rcClient.bottom - cyVScroll,
                           rcClient.right, cyVScroll,
                           s_ahSettingTabs[2], (HMENU) 0, s_hInstance, NULL );

   #ifdef DRIVES
      while( uDriveMask )
      {
          if( uDriveMask & 1 )
          {
             iDrives++;
          }

          ++cDrive;
          uDriveMask >>= 1;
      }

      SendMessage( hwndPB, PBM_SETRANGE, 0, MAKELPARAM( 0, 6 + iDrives * 3 - 1 ) );
      SendMessage( hwndPB, PBM_SETSTEP, (WPARAM) 1, 0 );
   #else
      SendMessage( hwndPB, PBM_SETRANGE, 0, MAKELPARAM( 0, 6 + 23 * 3 - 1 ) );
      SendMessage( hwndPB, PBM_SETSTEP, (WPARAM) 1, 0 );
   #endif

   // Search ...
   if( PathFileExists( FWH_FWH + 2 ) )
   {
      SendMessage( hwndPB, PBM_STEPIT, 0, 0 );
      WizardEvents();

      FWH_FWH[0] = _getdrive() + 'A' - 1;
      FWH_FWH[7] = '\0';

      SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, FWH_FWH );

      //sprintf( sMessage, "Found: '%s'\n", FWH_FWH );
      //OutputDebugString( sMessage );
      goto FWH_Done;
   }

   if( PathFileExists( FWH_XFWH + 2 ) )
   {
      SendMessage( hwndPB, PBM_STEPIT, 0, 0 );
      WizardEvents();

      FWH_XFWH[0] = _getdrive() + 'A' - 1;
      FWH_XFWH[7] = '\0';

      SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, FWH_XFWH );
      SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Lib, "libx" );

      goto FWH_Done;
   }

   #ifdef DRIVES
      // Reset.
      uWorkMask = uDriveMask;

      // Skip A.
      uWorkMask >>= 1;
      // Skip B.
      uWorkMask >>= 1;

      cDrive = 'C';

      while( uWorkMask )
      {
         SendMessage( hwndPB, PBM_STEPIT, 0, 0 );
         WizardEvents();

         if( uWorkMask & 1 )
         {
            FWH_FWH[0] = cDrive;

            if( PathFileExists( FWH_FWH ) )
            {
               FWH_FWH[6] = '\0';
               SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, FWH_FWH );

               //sprintf( sMessage, "Found: '%s'\n", FWH_FWH );
               //OutputDebugString( sMessage );
               goto FWH_Done;
            }

            if( PathFileExists( FWH_XFWH ) )
            {
               FWH_XFWH[6] = '\0';
               SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, FWH_XFWH );
               SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Lib, "libx" );

               //sprintf( sMessage, "Found: '%s'\n", FWH_XFWH );
               //OutputDebugString( sMessage );
               goto FWH_Done;
            }
         }

         ++cDrive;
         uWorkMask >>= 1;
      }
   #else
      for( cDrive = 'C'; cDrive <= 'Z'; cDrive++ )
      {
         if( GetFocus() != hFWHRoot )
         {
            DestroyWindow( hwndPB );
            bRunning = FALSE;

            hb_itemClear( &Lib );
            hb_itemClear( &Flags );
            hb_itemClear( &Item );

            return;
         }

         SendMessage( hwndPB, PBM_STEPIT, 0, 0 );
         //WizardEvents();

         //sprintf( sMessage, "Searching: '%s'\n", FWH_FWH );
         //OutputDebugString( sMessage );

         FWH_FWH[0] = cDrive;

         if( PathFileExists( FWH_FWH ) )
         {
            FWH_FWH[6] = '\0';
            SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, FWH_FWH );

            //sprintf( sMessage, "Found: '%s'\n", FWH_FWH );
            //OutputDebugString( sMessage );
            goto FWH_Done;
         }

         if( PathFileExists( FWH_XFWH ) )
         {
            FWH_XFWH[6] = '\0';
            SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, FWH_XFWH );
            SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Lib, "libx" );

            //sprintf( sMessage, "Found: '%s'\n", FWH_XFWH );
            //OutputDebugString( sMessage );
            goto FWH_Done;
         }
      }
   #endif

   if( PathFileExists( FW24_FWH + 2 ) )
   {
      SendMessage( hwndPB, PBM_STEPIT, 0, 0 );
      WizardEvents();

      FW24_FWH[0] = _getdrive() + 'A' - 1;
      FW24_FWH[7] = '\0';

      SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, FW24_FWH );
      goto FWH_Done;
   }

   if( PathFileExists( FW24_XFWH + 2 ) )
   {
      SendMessage( hwndPB, PBM_STEPIT, 0, 0 );
      WizardEvents();

      FW24_XFWH[0] = _getdrive() + 'A' - 1;
      FW24_XFWH[7] = '\0';

      SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, FW24_XFWH );
      SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Lib, "libx" );
      goto FWH_Done;
   }

   for( cDrive = 'C'; cDrive <= 'Z'; cDrive++ )
   {
      if( GetFocus() != hFWHRoot )
      {
         DestroyWindow( hwndPB );
         bRunning = FALSE;

         hb_itemClear( &Lib );
         hb_itemClear( &Flags );
         hb_itemClear( &Item );

         return;
      }

      SendMessage( hwndPB, PBM_STEPIT, 0, 0 );
      //WizardEvents();

      FW24_FWH[0] = cDrive;

      //sprintf( sMessage, "Searching: '%s'\n", FW24_FWH );
      //OutputDebugString( sMessage );

      if( PathFileExists( FW24_FWH ) )
      {
         FW24_FWH[7] = '\0';
         SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, FW24_FWH );
         goto FWH_Done;
      }

      if( PathFileExists( FW24_XFWH ) )
      {
         FW24_XFWH[7] = '\0';
         SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, FW24_XFWH );
         SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Lib, "libx" );
         goto FWH_Done;
      }
   }

   if( PathFileExists( FIVEWIN_FWH + 2 ) )
   {
      SendMessage( hwndPB, PBM_STEPIT, 0, 0 );
      WizardEvents();

      FIVEWIN_FWH[0] = _getdrive() + 'A' - 1;
      FIVEWIN_FWH[10] = '\0';

      SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, FIVEWIN_FWH );
      goto FWH_Done;
   }

   if( PathFileExists( FIVEWIN_XFWH + 2 ) )
   {
      SendMessage( hwndPB, PBM_STEPIT, 0, 0 );
      WizardEvents();

      FIVEWIN_XFWH[0] = _getdrive() + 'A' - 1;
      FIVEWIN_XFWH[10] = '\0';

      SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, FIVEWIN_XFWH );
      SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Lib, "libx" );
      goto FWH_Done;
   }

   for( cDrive = 'C'; cDrive <= 'Z'; cDrive++ )
   {
      if( GetFocus() != hFWHRoot )
      {
         DestroyWindow( hwndPB );
         bRunning = FALSE;

         hb_itemClear( &Lib );
         hb_itemClear( &Flags );
         hb_itemClear( &Item );

         return;
      }

      SendMessage( hwndPB, PBM_STEPIT, 0, 0 );
      //WizardEvents();

      FIVEWIN_FWH[0] = cDrive;

      //sprintf( sMessage, "Searching: '%s'\n", FIVEWIN_FWH );
      //OutputDebugString( sMessage );

      if( PathFileExists( FIVEWIN_FWH ) )
      {
         FIVEWIN_FWH[10] = '\0';
         SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, FIVEWIN_FWH );
         goto FWH_Done;
      }

      if( PathFileExists( FIVEWIN_XFWH ) )
      {
         FIVEWIN_XFWH[10] = '\0';
         SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, FIVEWIN_XFWH );
         SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Lib, "libx" );
         goto FWH_Done;
      }
   }

   FWH_Done:

     //--------------------- Set_FWH() ------------------------------
     GetDlgItemText( s_ahSettingTabs[2], ID_FWH_Lib, sLib, 256 );
     hb_itemPutC( &Lib, sLib );

     GetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, sFile, MAX_PATH );
     hb_itemPutC( &Item, sFile );

     hb_itemClear( &Flags );
     hb_objSendMsg( s_pProject, "Set_FWH", 2, &Item, &Lib );
     //----------------------------------------------------------------

     bRunning = FALSE;
     DestroyWindow( hwndPB );

     hb_itemClear( &Lib );
     hb_itemClear( &Flags );
     hb_itemClear( &Item );
}

int CALLBACK ListViewSorter( LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort )
{
   // char sMessage[256];

   // sprintf( sMessage, "Sorting %i and %i\n", lParam1, lParam2 );
   // OutputDebugString( sMessage );

   return lParam1 < lParam2 ? 1 : -1;
}

LRESULT CALLBACK MainWindowProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   POINTS  CursorPos;

   switch( message )
   {
      case WM_SETFOCUS:
      {
         // OutputDebugString( "MainWindow Got Focus.\n" );

         SelectStepPage( s_iStep, FALSE );

         return 0;
      }

      case WM_CAPTURECHANGED:
      {
         // OutputDebugString( "CaptureChanged\n" );
         ReleaseCapture();
         break;
      }

      case WM_RBUTTONDOWN:
      {
         if( s_iStep >= 0 || s_iStep == -1 )
         {
            CursorPos = MAKEPOINTS( lParam );

            if( CursorPos.x >= 420 && CursorPos.x <= 487 )
            {
               // Open Project
               if(  CursorPos.y >= 106 && CursorPos.y <= 164 )
               {
                  POINT CursorPosition;

                  GetCursorPos( &CursorPosition );
                  //TrackPopupMenu( s_hOpen_ContextMenu, TPM_RIGHTBUTTON, CursorPosition.x, CursorPosition.y, 0, hWnd, NULL );
               }
               // Save Project
               else if(  CursorPos.y >= 172 && CursorPos.y <= 230 )
               {
               }
               // Build now
               else if(  CursorPos.y >= 246 && CursorPos.y <= 315 )
               {
                  POINT CursorPosition;
                  BOOL bLoaded;

                  //char sMessage[256];

                  //sprintf( sMessage, "Type: %i\n", s_pProject->type );
                  //OutputDebugString( sMessage );

                  hb_objSendMsg( s_pProject, "aLiteralDependancies", 0 );

                  // Safety!
                  hb_vmRequestReset();

                  // Type checked in API.
                  bLoaded = hb_arrayLen( hb_stackReturnItem() );

                  if( bLoaded && IsWindowVisible( s_hResults ) == FALSE && ( s_hLog == 0 || IsWindowVisible( s_hLog ) == FALSE ) )
                  {
                     EnableMenuItem( s_hBuild_ContextMenu, IDM_BUILD_CONTEXT_OPENLOG, MF_ENABLED | MF_BYCOMMAND );
                  }
                  else
                  {
                     EnableMenuItem( s_hBuild_ContextMenu, IDM_BUILD_CONTEXT_OPENLOG, MF_GRAYED | MF_BYCOMMAND );
                  }

                  if( bLoaded && IsWindowVisible( s_hResults ) == FALSE )
                  {
                     EnableMenuItem( s_hBuild_ContextMenu, IDM_BUILD_CONTEXT_VIEWERRORS, MF_ENABLED | MF_BYCOMMAND );
                  }
                  else
                  {
                     EnableMenuItem( s_hBuild_ContextMenu, IDM_BUILD_CONTEXT_VIEWERRORS, MF_GRAYED | MF_BYCOMMAND );
                  }

                  GetCursorPos( &CursorPosition );
                  TrackPopupMenu( s_hBuild_ContextMenu, TPM_RIGHTBUTTON, CursorPosition.x, CursorPosition.y, 0, hWnd, NULL );
               }
            }
         }

         break;
      }

      case WM_LBUTTONDOWN:
      {
         SetCapture( hWnd );

         if( s_bError && IsWindowVisible( s_hResults ) )
         {
            s_bError = FALSE;
            DestroyWindow( GetWindow( s_hResults, GW_CHILD ) );
            ShowWindow( s_hResults, SW_HIDE );

            if( s_iStep == 0 )
            {
               s_iStep = -1;
               SelectStepPage( 0, TRUE );
            }
            else
            {
               SelectStepPage( -s_iStep, TRUE );
            }
         }

         //if( s_iStep >= 0 || s_iStep == -1 )
         {
            //char sMessage[256];
            int iButton, iRadious;

            CursorPos = MAKEPOINTS( lParam );

            //sprintf( sMessage, "Main Down at X: %i Y: %i\n", CursorPos.x, CursorPos.y );
            //OutputDebugString( sMessage );

            if(  CursorPos.y >= 6 && CursorPos.y <= 24 )
            {
               if( s_iStep >= 0 || s_iStep == -1 )
               {
                  if( CursorPos.x >= 463 && CursorPos.x <= 481 )
                  {
                     ShowWindow( hWnd, SW_MINIMIZE );
                     break;
                  }
                  else if( CursorPos.x >= 487 && CursorPos.x <= 505 )
                  {
                     s_iSkipValidation = -1;
                     SendMessage( s_hWnd, WM_CLOSE, 0, 0 );
                     break;
                  }
                  else if( CursorPos.x >= 439 && CursorPos.x <= 457 )
                  {
                     MessageBox( 0, "xBuild Wizard Version: " _BUILD_ " Dated " __DATE__" " __TIME__, "About", MB_SETFOREGROUND );
                     break;
                  }
               }
            }
            else if( CursorPos.x >= 420 && CursorPos.x <= 487 )
            {
               // Open Project
               if(  CursorPos.y >= 106 && CursorPos.y <= 164 )
               {
                  if( s_iStep >= 0 || s_iStep == -1 )
                  {
                     OPENFILENAME ofn;
                     char sFile[ MAX_PATH ];

                     if( s_iValidating )
                     {
                        HWND hDlg;

                        if( s_iStep == 3 )
                        {
                           hDlg = s_ahSettingTabs[ s_iSettingTab ];
                        }
                        else
                        {
                           hDlg = s_ahStepPages[ s_iStep ];
                        }

                        if( ! Validate( s_iValidating, hDlg, GetDlgItem( hDlg, s_iValidating ), FALSE ) )
                        {
                           return 0;
                        }
                     }

                     GetDlgItemText( s_ahStepPages[0], ID_Target, sFile, MAX_PATH );

                     if( sFile[0] )
                     {
                        if( MessageBox( hWnd, "Save current project, before loading new project?", "xBuild Wizard", MB_YESNO | MB_ICONQUESTION ) == IDYES )
                        {
                           PHB_DYNS pSym = hb_dynsymFindName( "GenerateProjectFile" );

                           // OutputDebugString( "Creating New Project.\n" );

                           if( pSym )
                           {
                              SetProject();

                              hb_vmPushSymbol( pSym->pSymbol );
                              hb_vmPushNil();
                              hb_vmPush( s_pProject );
                              hb_vmDo( 1 );

                              // Safety!
                              hb_vmRequestReset();
                           }
                           else
                           {
                              Alert( "Sorry, cound't locate: 'GenerateProjectFile()'!" );
                           }
                        }
                     }

                     s_iSkipValidation = ID_Main;

                     sFile[0] = '\0';

                     ZeroMemory( &ofn, sizeof( OPENFILENAME ) );

                     ofn.hInstance       = s_hInstance;
                     ofn.lStructSize     = sizeof( OPENFILENAME );
                     ofn.hwndOwner       = hWnd;
                     ofn.lpstrTitle      = "xBuild Wizard";
                     ofn.lpstrFilter     = "Project files (*.xbp)\0"
                                             "*.exe.xbp;*.lib.xbp;*.dll.xbp\0"
                                           "Executables Project Files (*.exe.xbp)\0"
                                             "*.exe.xbp\0"
                                           "Librarry Project Files (*.lib.xbp)\0"
                                             "*.lib.xbp\0"
                                           "Dll Project Files (*.exe.xbp)\0"
                                             "*.dll.xbp\0";

                     ofn.Flags           = OFN_EXPLORER | OFN_ENABLESIZING | OFN_PATHMUSTEXIST| OFN_FILEMUSTEXIST;
                     ofn.lpstrInitialDir = s_sCurrentDir;
                     ofn.lpstrDefExt     = NULL;
                     ofn.nFilterIndex    = 1;
                     ofn.lpstrFile       = sFile;
                     ofn.nMaxFile        = MAX_PATH;

                     if( GetOpenFileName( &ofn ) )
                     {
                        PHB_DYNS pSym;

                        GetCurrentDirectory( _MAX_DIR, s_sCurrentDir );

                        OutputDebugString( "Loading: " );
                        OutputDebugString( sFile );
                        OutputDebugString( "\n" );

                        pSym = hb_dynsymFindName( "LoadProject" );

                        if( pSym )
                        {
                           char sMessage[256];

                           hb_vmPushSymbol( pSym->pSymbol );
                           hb_vmPushNil();
                           hb_vmPushString( sFile + strlen( s_sCurrentDir ) + 1, strlen( sFile + strlen( s_sCurrentDir ) + 1 ) );
                           hb_vmDo( 1 );

                           //sprintf( sMessage, "pSequence: %p\n", HB_VM_STACK.pSequence );
                           //OutputDebugString( sMessage );

                           //sprintf( sMessage, "Class: %s\n", hb_objGetClsName( hb_stackItem( HB_VM_STACK.pSequence->lBase - 1 ) ) );
                           //OutputDebugString( sMessage );

                           /* TODO!
                           if( hb_vmRequestQuery() & HB_BREAK_REQUESTED && HB_VM_STACK.pSequence && strcmp( hb_objGetClsName( hb_stackItem( HB_VM_STACK.pSequence->lBase - 1 ) ), "ERROR" ) == 0 )
                           {
                           }
                           */

                           // Safety!
                           hb_vmRequestReset();

                           hb_itemForwardValue( s_pProject, hb_stackReturnItem() );

                           if( ! HB_IS_OBJECT( s_pProject ) )
                           {
                              Alert( "Sorry, 'LoadProject()' failed!" );
                              return 0;
                           }

                           OutputDebugString( "InitControls\n" );

                           InitControls();

                           if( ofn.Flags & OFN_READONLY )
                           {
                              CheckDlgButton( s_ahStepPages[0], ID_XBP, BST_UNCHECKED );
                           }
                        }
                        else
                        {
                           Alert( "Sorry, cound't locate: 'LoadProject()'!" );
                        }

                        // OutputDebugString( sFile );
                        // OutputDebugString( "\n" );
                     }

                     SetDlgItemText( s_ahStepPages[0], ID_Root, s_sCurrentDir );
                     SetFocus( GetDlgItem( s_ahStepPages[0], ID_Root ) );
                  }
               }
               // Save Project
               else if(  CursorPos.y >= 172 && CursorPos.y <= 230 )
               {
                  if( s_iStep >= 0 || s_iStep == -1 )
                  {
                     PHB_DYNS pSym;

                     if( s_iValidating )
                     {
                        HWND hDlg;

                        if( s_iStep == 3 )
                        {
                           hDlg = s_ahSettingTabs[ s_iSettingTab ];
                        }
                        else
                        {
                           hDlg = s_ahStepPages[ s_iStep ];
                        }

                        if( ! Validate( s_iValidating, hDlg, GetDlgItem( hDlg, s_iValidating ), TRUE ) )
                        {
                           return 0;
                        }
                     }

                     if( ! ValidateTarget( TRUE ) )
                     {
                        if( s_iStep )
                        {
                           SelectStepPage( 0, TRUE );
                        }

                        SetFocus( GetDlgItem( s_ahStepPages[0], ID_Target ) );
                        return 0;
                     }
                     else if(  ! ValidateMain( TRUE ) )
                     {
                        if( s_iStep != 1 )
                        {
                           SelectStepPage( 1, TRUE );
                        }

                        SetFocus( GetDlgItem( s_ahStepPages[1], ID_Main ) );
                        return 0;
                     }

                     pSym = hb_dynsymFindName( "GenerateProjectFile" );

                     // OutputDebugString( "Creating New Project.\n" );

                     if( pSym )
                     {
                        SetProject();

                        hb_vmPushSymbol( pSym->pSymbol );
                        hb_vmPushNil();
                        hb_vmPush( s_pProject );
                        hb_vmDo( 1 );

                        // Safety!
                        hb_vmRequestReset();
                     }
                     else
                     {
                        Alert( "Sorry, cound't locate: 'GenerateProjectFile()'!" );
                     }
                  }
               }
               // Build now
               else if(  CursorPos.y >= 246 && CursorPos.y <= 315 )
               {
                  // Building already!
                  if( s_iStep == -10 )
                  {
                     //Alert( "Cancel Request!" );
                     hb_vmRequestBreak( NULL );
                     Process_SetContinueFlag( FALSE );
                     s_bError = TRUE;

                     PaintBitmap( hWnd, s_ahFinish[ STATE_OFF ]  , 415, 241, 77, 79, 0, 0 );
                  }
                  else
                  {
                     int iStep;

                     if( s_iValidating )
                     {
                        HWND hDlg;

                        if( s_iStep == 3 )
                        {
                           hDlg = s_ahSettingTabs[ s_iSettingTab ];
                        }
                        else
                        {
                           hDlg = s_ahStepPages[ s_iStep ];
                        }

                        if( ! Validate( s_iValidating, hDlg, GetDlgItem( hDlg, s_iValidating ), TRUE ) )
                        {
                           return 0;
                        }
                     }

                     if( ! ValidateTarget( TRUE ) )
                     {
                        if( s_iStep )
                        {
                           SelectStepPage( 0, TRUE );
                        }

                        SetFocus( GetDlgItem( s_ahStepPages[0], ID_Target ) );
                        return 0;
                     }
                     else if(  ! ValidateMain( TRUE ) )
                     {
                        if( s_iStep != 1 )
                        {
                           SelectStepPage( 1, TRUE );
                        }

                        SetFocus( GetDlgItem( s_ahStepPages[1], ID_Main ) );
                        return 0;
                     }

                     iStep = s_iStep;
                     if( s_iStep >= 0 )
                     {
                        SelectStepPage( -10, FALSE );
                     }

                     s_bError = FALSE;

                     #ifdef ANIMATE
                        s_hAnimation = Animate_Create( s_hWnd, 1001, WS_CHILD | ACS_CENTER | ACS_TRANSPARENT, s_hInstance );

                        // sprintf( sMessage, "Animation: %i\n", s_hAnimation );
                        // OutputDebugString( sMessage );

                        MoveWindow( s_hAnimation, 26, 104, 386 , 216, FALSE );

                        ShowWindow( s_hAnimation, SW_SHOW );

                        if( Animate_Open( s_hAnimation, "WorkAvi" ) == 0 )
                        {
                           OutputDebugString( "*** AVI Open Failed! ***\n" );
                        }
                        else if( Animate_Play( s_hAnimation, 0, -1, -1 ) == 0 )
                        {
                           OutputDebugString( "*** Play Failed! ***\n" );
                        }
                     #endif

                     SetProject();

                     OutputDebugString( "Making...\n" );

                     PaintBitmap( hWnd, s_hCancel  , 415, 241, 77, 79, 0, 0 );

                     hb_objSendMsg( s_pProject, "_OnFWH", 1, s_pOnFWH );

                     hb_objSendMsg( s_pProject, "Make", 2, s_pErrorHandler, s_pProgress );

                     // Safety!
                     hb_vmRequestReset();

                     OutputDebugString( "Done.\n" );

                     // Might have been killed by a NON production Error.
                     if( s_hAnimation )
                     {
                        #ifdef ANIMATE
                           Animate_Stop( s_hAnimation );
                           Animate_Close( s_hAnimation );
                           DestroyWindow( s_hAnimation );
                           s_hAnimation = NULL;
                        #endif
                     }

                     if( s_bError )
                     {
                        OutputDebugString( "Error.\n" );

                        if( IsWindowVisible( s_hResults ) == FALSE )
                        {
                           if( s_hLog == 0 || IsWindowVisible( s_hLog ) == FALSE )
                           {
                              SelectStepPage( iStep, TRUE );
                           }
                           else
                           {
                              SelectStepPage( iStep, TRUE );
                              SetForegroundWindow( s_hLog );
                           }
                        }
                        else
                        {
                           s_iStep = -iStep;
                           SetForegroundWindow( s_hResults );
                        }
                     }
                     else
                     {
                        long lType = SendMessage( GetDlgItem( s_ahStepPages[0], ID_Type ), CB_GETCURSEL, 0, 0 );
                        char sMessage[256];

                        sprintf( sMessage, "Type: %i.\n", lType );
                        OutputDebugString( sMessage );

                        if( lType == 0 )
                        {
                           int iResult;

                           //s_sArguments[0] = '\0';
                           //s_sStartIn[0] = '\0';

                           if( IsDlgButtonChecked( s_ahSettingTabs[3], ID_RUN_Ask ) )
                           {
                              iResult = DialogBox( s_hInstance, "XBUILD_Success", s_hWnd, SuccessProc );
                           }
                           else if( IsDlgButtonChecked( s_ahSettingTabs[3], ID_RUN_Auto ) )
                           {
                              iResult = 10;
                           }
                           else
                           {
                              iResult = 0;
                           }

                           if( iResult >= 10 )
                           {
                              char sCommand[ MAX_PATH + MAX_PATH + MAX_PATH + MAX_PATH + 1 ];
                              STARTUPINFO si;
                              PROCESS_INFORMATION pi;

                              OutputDebugString( "Running:" );

                              // si
                              ZeroMemory( &si, sizeof(si) );
                              si.cb = sizeof(si);

                              // pi
                              ZeroMemory( &pi, sizeof(pi) );

                              hb_objSendMsg( s_pProject, "lPRG_Debug", 0 );

                              if( hb_parl( -1 ) )
                              {
                                 hb_objSendMsg( s_pProject, "lPRG_ClassicDebug", 0 );

                                 if( ! hb_parl( -1 ) )
                                 {
                                    hb_objSendMsg( s_pProject, "xHB_Root", 0 );

                                    strcpy( sCommand, hb_parcx( -1 ) );
                                    strcat( sCommand, "bin\\xDebugW.exe " );

                                    if( PathFileExists( sCommand ) )
                                    {
                                       strcat( sCommand, "-xbp:\"");

                                       hb_objSendMsg( s_pProject, "XBPFullPath", 0 );

                                       strcat( sCommand, hb_parcx( -1 ) );
                                       strcat( sCommand, "\" " );
                                    }
                                    else
                                    {
                                       OutputDebugString( "Missing: " );
									   OutputDebugString( sCommand );
                                       sCommand[0] = '\0';
                                    }
                                 }
                                 else
                                 {
                                    OutputDebugString( "CLASSIC Debugging!\n" );
                                    sCommand[0] = '\0';
                                 }
                              }
                              else
                              {
                                 OutputDebugString( "NO PRG Debugging!\n" );
                                 sCommand[0] = '\0';
                              }

                              GetDlgItemText( s_ahStepPages[0], ID_Target, sCommand + strlen( sCommand ), MAX_PATH );

                              strcat( sCommand, " " );
                              strcat( sCommand, s_sArguments );

                              OutputDebugString( sCommand );
                              OutputDebugString( "\n" );

                              if( CreateProcess( NULL,                              //App name.
                                                 sCommand,                          // Command line.
                                                 NULL /*&sa*/,                      // Process handle not inheritable.
                                                 NULL /*&sa*/,                      // Thread handle not inheritable.
                                                 FALSE,                             // Set handle inheritance .
                                                 CREATE_NEW_CONSOLE,                // No creation flags.
                                                 NULL,                              // Use parent's environment block.
                                                 s_sStartIn[0] ? s_sStartIn : NULL, // Use parent's starting directory.
                                                 &si,                               // Pointer to STARTUPINFO structure.
                                                 &pi )                              // Pointer to PROCESS_INFORMATION structure.
                                )
                              {
                                 CloseHandle( pi.hProcess );
                                 CloseHandle( pi.hThread );
                              }
                              else
                              {
                                 Alert( "Sorry, could not run this executable!" );
                              }
                           }

                           if( iResult == 1 || iResult == 11 )
                           {
                              DestroyWindow( s_hWnd );
                           }
                           else
                           {
                              SelectStepPage( iStep, TRUE );
                           }
                        }
                        else
                        {
                           if( MessageBox( hWnd, "Project built successfully, close xBuild Wizard?", "xBuild Wizard", MB_YESNO | MB_ICONQUESTION ) == IDYES )
                           {
                              DestroyWindow( s_hWnd );
                           }
                           else
                           {
                              SelectStepPage( iStep, TRUE );
                           }
                        }
                     }
                  }
                  break;
               }
            }

            if( s_iStep >= 0 || s_iStep == -1 )
            {
               for( iButton = 0; iButton < BUTTONS; iButton++ )
               {
                  int i_X_Offset = CursorPos.x - s_aButtonCenters[iButton].x;
                  int i_Y_Offset = CursorPos.y - s_aButtonCenters[iButton].y;

                  iRadious = (int) sqrt( (float)( ( i_X_Offset * i_X_Offset ) + ( i_Y_Offset * i_Y_Offset ) ) );

                  // sprintf( sMessage, "Radial distance %i from Button %i\n", iRadious, iButton );
                  // OutputDebugString( sMessage );

                  if( iRadious <= BUTTON_RADIOUS )
                  {
                     // sprintf( sMessage, "On Button: %i\n", iButton );
                     // OutputDebugString( sMessage );

                     SelectStepPage( iButton, TRUE );
                     break;
                  }
               }
            }
         }

         ReleaseCapture();
         SendMessage( hWnd, WM_SYSCOMMAND, SC_MOVE | HTCLIENT, 0 );
         break;
      }

      case WM_LBUTTONUP:
      {
         // OutputDebugString( "Up\n" );

         ReleaseCapture();

         // CursorPos = MAKEPOINTS(lParam);

         break;
      }

      case WM_MOUSEMOVE:
      {
         if( s_iStep >= 0 || s_iStep == -1 )
         {
            //MSG     msg;
            int iButton, iRadious;
            BOOL bHit   = FALSE;

            CursorPos   = MAKEPOINTS( lParam );

            // sprintf( sMessage, "Down at X: %i Y: %i\n", CursorPos.x, CursorPos.y );
            // OutputDebugString( sMessage );

            if( CursorPos.y >= 6 && CursorPos.y <= 24 )
            {
               if( CursorPos.x >= 463 && CursorPos.x <= 481 )
               {
                  bHit = TRUE;

                  if( s_Hover.OFF != s_ahMinimize[ STATE_OFF ] )
                  {
                     PaintBitmap( hWnd, s_Hover.OFF, s_Hover.Left, s_Hover.Top, s_Hover.Width, s_Hover.Height, 0, 0 );

                     s_Hover.OFF = s_ahMinimize[ STATE_OFF ];
                     s_Hover.Left = 463;
                     s_Hover.Top = 6;
                     s_Hover.Width = 18;
                     s_Hover.Height = 18;

                     PaintBitmap( hWnd, s_ahMinimize[ STATE_HOVER ], 463, 6, 18, 18, 0, 0 );
                  }
                  break;
               }
               else if( CursorPos.x >= 487 && CursorPos.x <= 505 )
               {
                  bHit = TRUE;

                  if( s_Hover.OFF != s_ahClose[ STATE_OFF ] )
                  {
                     PaintBitmap( hWnd, s_Hover.OFF, s_Hover.Left, s_Hover.Top, s_Hover.Width, s_Hover.Height, 0, 0 );

                     s_Hover.OFF = s_ahClose[ STATE_OFF ];
                     s_Hover.Left = 487;
                     s_Hover.Top = 6;
                     s_Hover.Width = 18;
                     s_Hover.Height = 18;

                     PaintBitmap( hWnd, s_ahClose[ STATE_HOVER ]   , 487, 6, 18, 18, 0, 0 );
                  }
                  break;
               }
               else if( CursorPos.x >= 439 && CursorPos.x <= 457 )
               {
                  bHit = TRUE;

                  if( s_Hover.OFF != s_ahClose[ STATE_OFF ] )
                  {
                     PaintBitmap( hWnd, s_Hover.OFF, s_Hover.Left, s_Hover.Top, s_Hover.Width, s_Hover.Height, 0, 0 );

                     s_Hover.OFF = s_ahHelp[ STATE_OFF ];
                     s_Hover.Left = 439;
                     s_Hover.Top = 6;
                     s_Hover.Width = 18;
                     s_Hover.Height = 18;

                     PaintBitmap( hWnd, s_ahHelp[ STATE_HOVER ]   , 439, 6, 18, 18, 0, 0 );
                  }
                  break;
               }
            }
            else if( CursorPos.x >= 417 && CursorPos.x <= 487 )
            {
               if(  CursorPos.y >= 106 && CursorPos.y <= 164 )
               {
                  bHit = TRUE;

                  if( s_Hover.OFF != s_ahOpen[ STATE_OFF ] )
                  {
                     PaintBitmap( hWnd, s_Hover.OFF, s_Hover.Left, s_Hover.Top, s_Hover.Width, s_Hover.Height, 0, 0 );

                     s_Hover.OFF    = s_ahOpen[ STATE_OFF ];
                     s_Hover.Left   = 415;
                     s_Hover.Top    = 106;
                     s_Hover.Width  = 77;
                     s_Hover.Height = 60;

                     PaintBitmap( hWnd, s_ahOpen[ STATE_HOVER ]  , 415, 106, 77, 60, 0, 0 );
                  }
                  break;
               }
               else if(  CursorPos.y >= 172 && CursorPos.y <= 230 )
               {
                  bHit = TRUE;

                  if( s_Hover.OFF != s_ahSave[ STATE_OFF ] )
                  {
                     PaintBitmap( hWnd, s_Hover.OFF, s_Hover.Left, s_Hover.Top, s_Hover.Width, s_Hover.Height, 0, 0 );

                     s_Hover.OFF    = s_ahSave[ STATE_OFF ];
                     s_Hover.Left   = 415;
                     s_Hover.Top    = 172;
                     s_Hover.Width  = 77;
                     s_Hover.Height = 60;

                     PaintBitmap( hWnd, s_ahSave[ STATE_HOVER ]  , 415, 172, 77, 60, 0, 0 );
                  }
                  break;
               }
               else if(  CursorPos.y >= 246 && CursorPos.y <= 315 )
               {
                  bHit = TRUE;

                  if( s_Hover.OFF != s_ahFinish[ STATE_OFF ] )
                  {
                     PaintBitmap( hWnd, s_Hover.OFF, s_Hover.Left, s_Hover.Top, s_Hover.Width, s_Hover.Height, 0, 0 );

                     s_Hover.OFF    = s_ahFinish[ STATE_OFF ];
                     s_Hover.Left   = 415;
                     s_Hover.Top    = 241;
                     s_Hover.Width  = 77;
                     s_Hover.Height = 79;

                     PaintBitmap( hWnd, s_ahFinish[ STATE_HOVER ]  , 415, 241, 77, 79, 0, 0 );
                  }
                  break;
               }
            }

            for( iButton = 0; iButton < BUTTONS; iButton++ )
            {
               int i_X_Offset = CursorPos.x - s_aButtonCenters[iButton].x;
               int i_Y_Offset = CursorPos.y - s_aButtonCenters[iButton].y;

               iRadious = (int) sqrt( (float) ( ( i_X_Offset * i_X_Offset ) + ( i_Y_Offset * i_Y_Offset ) ) );

               // sprintf( sMessage, "Radial distance %i from Button %i\n", iRadious, iButton );
               // OutputDebugString( sMessage );

               if( iButton == s_iStep )
               {
                  continue;
               }

               if( iRadious <= BUTTON_RADIOUS )
               {
                  bHit = TRUE;

                  // sprintf( sMessage, "On Button: %i\n", iButton );
                  // OutputDebugString( sMessage );

                  if( s_Hover.OFF != s_ahButtonStates[iButton][ STATE_OFF ] )
                  {
                     int iLeft = ( iButton * BUTTON_OFFSET ) + BUTTON_MARGIN;

                     PaintBitmap( hWnd, s_Hover.OFF, s_Hover.Left, s_Hover.Top, s_Hover.Width, s_Hover.Height, 0, 0 );

                     s_Hover.OFF    = s_ahButtonStates[iButton][ STATE_OFF ];
                     s_Hover.Left   = iLeft;
                     s_Hover.Top    = BUTTON_TOP;
                     s_Hover.Width  = BUTTON_WIDTH;
                     s_Hover.Height = BUTTON_HEIGHT;

                     PaintBitmap( hWnd, s_ahButtonStates[iButton][ STATE_HOVER ]  , iLeft, BUTTON_TOP, BUTTON_WIDTH, BUTTON_HEIGHT, 0, 0 );
                  }

                  break;
               }
            }

            if( ( ! bHit ) && s_Hover.OFF )
            {
               PaintBitmap( hWnd, s_Hover.OFF, s_Hover.Left, s_Hover.Top, s_Hover.Width, s_Hover.Height, 0, 0 );
               s_Hover.OFF = NULL;
            }
         }

         break;
      }

      case WM_CANCELMODE:
      {
         // OutputDebugString( "CancelMode\n" );
         ReleaseCapture();
         break;
      }

      case WM_PAINT:
      {
         PAINTSTRUCT ps;
         HDC hDC = BeginPaint( hWnd, &ps );
         int iButton;
         char sStepMessage[ MAX_PATH + 32 ], sTarget[ MAX_PATH ];

         // OutputDebugString( "Painting MainWindow\n" );

         PaintBitmap( hWnd, s_hBitmap, ps.rcPaint.left, ps.rcPaint.top, ps.rcPaint.right, ps.rcPaint.bottom, ps.rcPaint.left, ps.rcPaint.top );

         for( iButton = 0; iButton < BUTTONS; iButton++ )
         {
            int iLeft = ( iButton * BUTTON_OFFSET ) + BUTTON_MARGIN;
            //char sMessage[256];

            // sprintf( sMessage, "Painting Button: %i, Left: %i\n", iButton, iLeft );
            // OutputDebugString( sMessage );

            PaintBitmap( hWnd, s_ahButtonStates[iButton][ iButton == s_iStep || ( iButton == 0 && s_iStep == -1 ) ], iLeft, BUTTON_TOP, BUTTON_WIDTH, BUTTON_HEIGHT, 0, 0 );
         }

         PaintBitmap( hWnd, s_ahClose[ STATE_OFF ]   , 487, 6, 18, 18, 0, 0 );
         PaintBitmap( hWnd, s_ahMinimize[ STATE_OFF ], 463, 6, 18, 18, 0, 0 );
         PaintBitmap( hWnd, s_ahHelp[ STATE_OFF ]    , 439, 6, 18, 18, 0, 0 );

         PaintBitmap( hWnd, s_ahOpen[ STATE_OFF ]    , 415, 106, 77, 60, 0, 0 );
         PaintBitmap( hWnd, s_ahSave[ STATE_OFF ]    , 415, 172, 77, 60, 0, 0 );

         if( s_iStep == -10 )
         {
            PaintBitmap( hWnd, s_hCancel  , 415, 241, 77, 79, 0, 0 );
         }
         else
         {
            PaintBitmap( hWnd, s_ahFinish[ STATE_OFF ]  , 415, 241, 77, 79, 0, 0 );
         }

         if( s_ahStepPages[0] == 0 )
         {
            //OutputDebugString( "CreateForms\n" );
            CreateForms();
            //OutputDebugString( "InitControls\n" );
            InitControls();
         }

         if( s_iStep >= 0 )
         {
            GetDlgItemText( s_ahStepPages[0], ID_Target, sTarget, MAX_PATH );

            if( sTarget[0] == '\0' )
            {
               sprintf( sStepMessage, "Step %i of 4", s_iStep + 1, sTarget );
            }
            else
            {
               sprintf( sStepMessage, "Step %i of 4 [%s]", s_iStep + 1, sTarget );
            }

            SetBkMode( hDC, TRANSPARENT );
            SetTextColor( hDC, RGB( 0, 0, 255 ) );
            SelectObject( hDC, s_hFont );
            TextOut( hDC, 40, 83, sStepMessage, strlen( sStepMessage ) );
         }

         EndPaint( hWnd, &ps );

         SelectStepPage( s_iStep, FALSE );

         break;
      }

      case WM_COMMAND:
      {
         //OutputDebugString( "Command.\n" );

         // Menus.
         if( lParam == 0 )
         {
            switch( wParam )
            {
               case IDM_OPEN_CONTEXT_RELOAD :
               {
                  Alert( "Reload" );
                  break;
               }

               case IDM_BUILD_CONTEXT_OPENLOG :
               {
                  PHB_DYNS pSym = hb_dynsymFindName( "GUI_OpenLog" );

                  s_bError = TRUE;

                  if( pSym )
                  {
                     int iStep = s_iStep;

                     if( s_iStep >= 0 )
                     {
                        SelectStepPage( -10, FALSE );
                     }

                     hb_vmPushSymbol( pSym->pSymbol );
                     hb_vmPushNil();
                     hb_vmPush( s_pProject );

                     hb_vmDo( 1 );

                     // Safety!
                     hb_vmRequestReset();

                     if( s_hLog == 0 || IsWindowVisible( s_hLog ) == FALSE )
                     {
                        SelectStepPage( iStep, TRUE );
                     }
                     else
                     {
                        SelectStepPage( iStep, TRUE );
                        SetForegroundWindow( s_hLog );
                     }
                  }

                  break;
               }

               case IDM_BUILD_CONTEXT_VIEWERRORS :
               {
                  PHB_DYNS pSym = hb_dynsymFindName( "GUI_ViewErrors" );

                  s_bError = TRUE;

                  if( pSym )
                  {
                     int iStep = s_iStep;

                     if( s_iStep >= 0 )
                     {
                        SelectStepPage( -10, FALSE );
                     }

                     hb_vmPushSymbol( pSym->pSymbol );
                     hb_vmPushNil();
                     hb_vmPush( s_pProject );

                     hb_vmDo( 1 );

                     // Safety!
                     hb_vmRequestReset();

                     if( IsWindowVisible( s_hResults ) == FALSE )
                     {
                        if( s_hLog == 0 || IsWindowVisible( s_hLog ) == FALSE )
                        {
                           SelectStepPage( iStep, TRUE );
                        }
                        else
                        {
                           SelectStepPage( iStep, TRUE );
                           SetForegroundWindow( s_hLog );
                        }
                     }
                     else
                     {
                        s_iStep = -iStep;
                        SetForegroundWindow( s_hResults );
                     }
                  }

                  break;
               }

               default:
               {
                 if( wParam > IDM_OPEN_CONTEXT_RELOAD && wParam < IDM_BUILD_CONTEXT_OPENLOG )
                 {
                 }
               }
            }
         }

         break;
      }

      case WM_CTLCOLORSTATIC:
      {
         return (INT_PTR) s_hFormsBrush;
         //break;
      }

      case WM_DROPFILES:
      {
         AcceptDropedFiles( (HDROP) wParam, GetAsyncKeyState( VK_CONTROL ) & 0x8000 );
         break;
      }

      case WM_CREATE:
      {
         //OutputDebugString( "Creating\n" );
         //SendMessage( hWnd, WM_SETFONT, (WPARAM) s_hFont, 0 );
         break;
      }

      case WM_CLOSE :
      {
         if( 1 || MessageBox( hWnd, "Close xBuild Wizard?", "xBuild Wizard", MB_YESNO | MB_ICONQUESTION ) == IDYES )
         {
            //PostQuitMessage(0);
            return DefWindowProc( hWnd, message, wParam, lParam );
         }

         if( s_iStep >= 0 )
         {
            if( s_iStep == 3 )
            {
               SetFocus( s_ahSettingTabs[ s_iSettingTab ] );
            }
            else
            {
               SetFocus( s_ahStepPages[ s_iStep ] );
            }
         }
         return 0;
      }

      case WM_DESTROY:
      {
         int iStep, iType, iSetting;

         s_iStep = -1;

         DeleteObject( s_hBitmap );
         DeleteObject( s_hFormsBrush );

         DeleteObject( s_ahButtonStates[0][ STATE_OFF   ] );
         DeleteObject( s_ahButtonStates[0][ STATE_ON    ] );
         DeleteObject( s_ahButtonStates[0][ STATE_HOVER ] );

         DeleteObject( s_ahButtonStates[1][ STATE_OFF   ] );
         DeleteObject( s_ahButtonStates[1][ STATE_ON    ] );
         DeleteObject( s_ahButtonStates[1][ STATE_HOVER ] );

         DeleteObject( s_ahButtonStates[2][ STATE_OFF   ] );
         DeleteObject( s_ahButtonStates[2][ STATE_ON    ] );
         DeleteObject( s_ahButtonStates[2][ STATE_HOVER ] );

         DeleteObject( s_ahButtonStates[3][ STATE_OFF   ] );
         DeleteObject( s_ahButtonStates[3][ STATE_ON    ] );
         DeleteObject( s_ahButtonStates[3][ STATE_HOVER ] );

         DeleteObject( s_ahClose[ STATE_OFF  ] );
         DeleteObject( s_ahClose[ STATE_HOVER] );

         DeleteObject( s_ahMinimize[ STATE_OFF  ] );
         DeleteObject( s_ahMinimize[ STATE_HOVER] );

         DeleteObject( s_ahHelp[ STATE_OFF  ] );
         DeleteObject( s_ahHelp[ STATE_HOVER] );

         DeleteObject( s_ahOpen[ STATE_OFF  ] );
         DeleteObject( s_ahOpen[ STATE_HOVER] );

         DeleteObject( s_ahSave[ STATE_OFF  ] );
         DeleteObject( s_ahSave[ STATE_HOVER] );

         DeleteObject( s_ahFinish[ STATE_OFF  ] );
         DeleteObject( s_ahFinish[ STATE_HOVER] );

         DeleteObject( s_ahFolder[ STATE_OFF  ] );
         DeleteObject( s_ahFolder[ STATE_HOVER] );

         DeleteDC( s_CompatibleDC );

         ReleaseDC( hWnd, s_MainDC );

         DestroyMenu( s_hPopupContextMenus );

         DeleteObject( s_hFont );

         for( iStep = 0; iStep < BUTTONS; iStep++ )
         {
            DeleteObject( s_ahStepFonts[ iStep ] );
         }

         for( iType = 0; iType < FILE_TYPES; iType++ )
         {
            DeleteObject( s_ahTypeFonts[ iType ] );
         }

         for( iSetting = 0; iSetting < 3; iSetting++ )
         {
            DeleteObject( s_ahSettingFonts[ iSetting ] );
         }

         OutputDebugString( "Posting Quit.\n" );
         PostQuitMessage( 0 );
         OutputDebugString( "Posted.\n" );

         return 0;
      }

      case WM_QUIT:
      {
         OutputDebugString( "WM_QUIT.\n" );
      }
   }

   return DefWindowProc( hWnd, message, wParam, lParam );
}

LRESULT CALLBACK MyDefaultWindowProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   switch( message )
   {
      case WM_NOTIFY :
      {
         LPNMHDR NotifyHeader = (LPNMHDR) lParam;
         //char sMessage[256];

         //sprintf( sMessage, "Notify( %i ): %i, Id: %i\n", NotifyHeader->code, NotifyHeader->hwndFrom, NotifyHeader->idFrom );
         //OutputDebugString( sMessage );

         switch( NotifyHeader->code )
         {
            case NM_DBLCLK:
            {
               if( s_bError && s_hResults )
               {
                  HWND hLV = GetWindow( s_hResults, GW_CHILD );

                  if( ListView_GetSelectedCount( hLV ) )
                  {
                     int iFile;
                     char sFile[ MAX_PATH ], sLine[20];

                     iFile = ListView_GetNextItem( hLV, -1, LVNI_SELECTED );

                     sFile[0] = '\0';
                     ListView_GetItemText( hLV, iFile, 0, sFile, MAX_PATH );

                     if( strchr( sFile, '*' ) || strchr( sFile, '?' ) )
                     {
                        break;
                     }

                     sLine[0] = '\0';
                     ListView_GetItemText( hLV, iFile, 1, sLine, 20 );

                     if( sFile[0] && atoi( sLine ) )
                     {
                        PHB_DYNS pSym = hb_dynsymFindName( "PopupEditor" );

                        if( pSym )
                        {
                           hb_vmPushSymbol( pSym->pSymbol );
                           hb_vmPushNil();
                           hb_vmPushLong( (long) s_hWnd );
                           hb_vmPushInteger( 10 );
                           hb_vmPushInteger( 10 );
                           hb_vmPushInteger( 780 );
                           hb_vmPushInteger( 580 );
                           hb_vmPushString( sFile, strlen( sFile ) );
                           hb_vmPushInteger( atoi( sLine ) );
                           hb_vmDo( 7 );

                           // Safety!
                           hb_vmRequestReset();
                        }
                     }
                  }
               }

               break;
            }
         }

         break;
      }
   }

   return DefWindowProc( hWnd, message, wParam, lParam );
}

LRESULT CALLBACK EditSubclassProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   switch( message )
   {
      case WM_LBUTTONDBLCLK :
      {
         //OutputDebugString( "DBLCLK\n" );

         if( s_iStep == 1 )
         {
            char sFile[ MAX_PATH ];
            PHB_DYNS pSym = hb_dynsymFindName( "PopupEditor" );

            sFile[0] = '\0';
            GetWindowText( hWnd, sFile, 256 );

            if( sFile[0] == '\0' )
            {
               break;
            }

            OutputDebugString( "Open Main Module: " );
            OutputDebugString( sFile );
            OutputDebugString( "\n" );

            if( pSym )
            {
               hb_vmPushSymbol( pSym->pSymbol );
               hb_vmPushNil();
               hb_vmPushLong( (long) s_hWnd );
               hb_vmPushInteger( 10 );
               hb_vmPushInteger( 10 );
               hb_vmPushInteger( 780 );
               hb_vmPushInteger( 580 );
               hb_vmPushString( sFile, strlen( sFile ) );
               hb_vmDo( 6 );

               // Safety!
               hb_vmRequestReset();
            }
         }

         break;
      }
   }

   return CallWindowProc( wpOrigEditProc, hWnd, message, wParam, lParam);
}

void SelectStepPage( int iPage, BOOL bPaint )
{
   // char sMessage[256];

   if( iPage == s_iStep )
   {
      return;
   }

   if( s_iValidating )
   {
      HWND hDlg;

      if( s_iStep == 3 )
      {
         hDlg = s_ahSettingTabs[ s_iSettingTab ];
      }
      else
      {
         hDlg = s_ahStepPages[ s_iStep ];
      }

      if( ! Validate( s_iValidating, hDlg, GetDlgItem( hDlg, s_iValidating ), FORCE_VALID ) )
      {
         #if FORCE_VALID
            iPage = s_iStep;
         #endif
      }
   }

   if( iPage > 0 && s_iStep == 0 )
   {
      if ( ! ValidateTarget( FORCE_VALID ) )
      {
         #if FORCE_VALID
            iPage = 0;
         #endif
      }
   }

   if( iPage > 1 && s_iStep <= 1 )
   {
      if ( ! ValidateMain( FORCE_VALID ) )
      {
         #if FORCE_VALID
            iPage = 1;
         #endif
      }
   }

   if( s_iStep >= 0 )
   {
      // sprintf( sMessage, "Closing Page: #%i\n", s_iStep );
      // OutputDebugString( sMessage );

      ShowWindow( s_ahStepPages[s_iStep], SW_HIDE );

      if( s_iStep == 2 )
      {
         SelectTypeTab( -1, FALSE );
      }
      else if( s_iStep == 3 )
      {
         SelectSettingTab( -1, FALSE );
      }
   }

   s_iStep = iPage;

   // sprintf( sMessage, "Showing Page: #%i\n", s_iStep );
   // OutputDebugString( sMessage );

   if( s_iStep >= 0 )
   {
      ShowWindow( s_ahStepPages[s_iStep], SW_SHOW );

      if( s_iStep == 2 )
      {
         int iTab = s_iTypeTab == -1 ? 0 : s_iTypeTab;

         s_iTypeTab = -1;
         SelectTypeTab( iTab, TRUE );
      }
      else if( s_iStep == 3 )
      {
         int iTab = s_iSettingTab == -1 ? 0 : s_iSettingTab;

         s_iSettingTab = -1;
         SelectSettingTab( iTab, TRUE );
      }

      SetFocus( s_ahStepPages[ s_iStep ] );
   }

   if( bPaint )
   {
      InvalidateRect( s_hWnd, NULL, FALSE );
   }
}

BOOL CALLBACK StepPagesProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   //char sMessage[256];
   POINTS CursorPos;

   // sprintf( sMessage, "Message: %i, wParam: %i, lParam %i\n", message, wParam, lParam );
   // OutputDebugString( sMessage );

   switch( message )
   {
      case WM_COMMAND:
      {
         //OutputDebugString( "Command.\n" );

         // Menus.
         if( lParam == 0 )
         {
            OutputDebugString( "Menu\n" );

            switch( wParam )
            {
               case IDM_MODULES_CONTEXT_OPEN :
               {
                  OutputDebugString( "Context - Open" );
                  OpenModules();
                  break;
               }

               case IDM_MODULES_CONTEXT_MOVEUP :
               {
                  OutputDebugString( "Context - Up" );
                  MoveModules( MOVE_UP );
                  break;
               }

               case IDM_MODULES_CONTEXT_MOVEDOWN :
               {
                  OutputDebugString( "Context - Down" );
                  MoveModules( MOVE_DOWN );
                  break;
               }

               case IDM_MODULES_CONTEXT_MOVETOP :
               {
                  OutputDebugString( "Context - Top" );
                  MoveModules( MOVE_TOP );
                  break;
               }

               case IDM_MODULES_CONTEXT_MOVEBOTTOM :
               {
                  OutputDebugString( "Context - Bottom" );
                  MoveModules( MOVE_BOTTOM );
                  break;
               }

               case IDM_MODULES_CONTEXT_PROPERTIES :
               {
                  OutputDebugString( "Context - Properties" );
                  SetModulesProperties();
                  break;
               }

               default :
               {
                  OutputDebugString( "OOPS - Unexpected Menu ID!" );
               }
            }
         }
         else
         {
            switch( HIWORD( wParam ) )
            {
               case EN_KILLFOCUS :
               {
                  OutputDebugString( "Edit Losing Focus\n" );

                  if( IsChild( s_hWnd, GetFocus() ) )
                  {
                     Validate( LOWORD( wParam ), hWnd, (HWND) lParam, TRUE );
                  }

                  break;
               }

               case EN_SETFOCUS :
               {
                  // OutputDebugString( "Edit Getting Focus\n" );
                  s_iValidating = LOWORD( wParam );
                  s_iSkipValidation = 0;
                  break;
               }

               case CBN_SELCHANGE :
               {
                  char sTarget[ MAX_PATH ];

                  GetDlgItemText( s_ahStepPages[0], ID_Target, sTarget, MAX_PATH );

                  if( sTarget[0] )
                  {
                     long lType = SendMessage( GetDlgItem( hWnd, ID_Type ), CB_GETCURSEL, 0, 0 );

                     sTarget[ strlen( sTarget ) - 3 ] = '\0';

                     switch( lType )
                     {
                        case 0 :
                        {
                           strcat( sTarget, "exe" );
                           break;
                        }

                        case 1 :
                        {
                           strcat( sTarget, "lib" );
                           break;
                        }

                        case 2 :
                        {
                           strcat( sTarget, "dll" );
                           break;
                        }
                     }

                     SetDlgItemText( s_ahStepPages[0], ID_Target, sTarget );

                     ValidateTarget( TRUE );
                  }

                  break;
               }

               case BN_CLICKED:
               {
                  OutputDebugString( "Button Clicked.\n" );

                  switch( LOWORD( wParam ) )
                  {
                     case ID_OpenSource :
                     {
                        OutputDebugString( "Open" );
                        OpenModules();
                        break;
                     }

                     case ID_Add :
                     {
                        OutputDebugString( "Add" );
                        AddModules();
                        break;
                     }

                     case ID_Remove :
                     {
                        OutputDebugString( "Remove" );
                        DeleteModules();
                        break;
                     }

                     case ID_Properties :
                     {
                        OutputDebugString( "Properties" );
                        SetModulesProperties();
                        break;
                     }

                     default:
                     {
                        OutputDebugString( "OOPS, unexpected Button ID!!!" );
                     }
                  }
               }
            }
         }

         break;
      }

      case WM_SETFOCUS:
      {
         // OutputDebugString( "Page Got Focus.\n" );

         if( s_iStep == 2 )
         {
            SelectTypeTab( s_iTypeTab == -1 ? 0 : s_iTypeTab, TRUE );

            if( ListView_GetSelectedCount( s_ahTypeTabs[ s_iTypeTab ] ) )
            {
               EnableWindow( GetDlgItem( hWnd, ID_OpenSource ), TRUE );
               EnableWindow( GetDlgItem( hWnd, ID_Properties ), TRUE );
            }
            else
            {
               EnableWindow( GetDlgItem( hWnd, ID_OpenSource ), FALSE );
               EnableWindow( GetDlgItem( hWnd, ID_Properties ), FALSE );
            }
         }
         else if( s_iStep == 3 )
         {
            SelectTypeTab( s_iSettingTab == -1 ? 0 : s_iSettingTab, TRUE );
         }

         return FALSE;
      }

      case WM_LBUTTONDOWN :
      {
         CursorPos = MAKEPOINTS( lParam );

         // sprintf( sMessage, "Step Down at X: %i Y: %i\n", CursorPos.x, CursorPos.y );
         // OutputDebugString( sMessage );

         break;
      }

      case WM_LBUTTONUP:
      {
         CursorPos = MAKEPOINTS( lParam );

         switch ( s_iStep )
         {
            case 0 :
            {
               if( CursorPos.x >= 348 && CursorPos.x <= 374 )
               {
                  if( CursorPos.y >= 17 && CursorPos.y <= 42 )
                  {
                     BROWSEINFO BrowseInfo;
                     LPITEMIDLIST ShellItemID;
                     char szFolder[ _MAX_DIR ], szDisplayName[ _MAX_DIR ];

                     BrowseInfo.hwndOwner = hWnd;
                     BrowseInfo.pidlRoot = NULL;
                     BrowseInfo.pszDisplayName = szDisplayName;
                     BrowseInfo.lpszTitle = "Please select location of the Project's ROOT Folder.";
                     BrowseInfo.ulFlags = BIF_NEWDIALOGSTYLE | BIF_RETURNONLYFSDIRS | BIF_RETURNFSANCESTORS /*| BIF_SHAREABLE*/;
                     BrowseInfo.lpfn = NULL;
                     BrowseInfo.lParam = 0;
                     BrowseInfo.iImage = 0;

                     ShellItemID = SHBrowseForFolder( &BrowseInfo );

                     if( ShellItemID )
                     {
                        SHGetPathFromIDList( ShellItemID, szFolder );

                        s_pSH_Alloc->lpVtbl->Free( s_pSH_Alloc, ShellItemID );
                        ShellItemID = NULL;

                        if( ! SetCurrentDirectory( szFolder ) )
                        {
                           Alert( "Sorry, couldn't select the specified ROOT Folder!" );
                        }

                        strcpy( s_sCurrentDir, szFolder );
                        SetDlgItemText( hWnd, ID_Root, szFolder );
                     }

                     SetFocus( GetDlgItem( hWnd, ID_Root ) );
                  }
               }

               break;
            }

            case 1 :
            {
               if( CursorPos.x >= 348 && CursorPos.x <= 374 )
               {
                  if( CursorPos.y >= 28 && CursorPos.y <= 53 )
                  {
                     OPENFILENAME ofn;
                     char sFile[ MAX_PATH ];

                     s_iSkipValidation = ID_Main;

                     sFile[0] = '\0';

                     ZeroMemory( &ofn, sizeof( OPENFILENAME ) );

                     ofn.hInstance       = s_hInstance;
                     ofn.lStructSize     = sizeof( OPENFILENAME );
                     ofn.hwndOwner       = hWnd;
                     ofn.lpstrTitle      = "xBuild Wizard";
                     ofn.lpstrFilter     = "All Source modules (*.prg;*.c)\0"
                                             "*.prg;*.c;\0"
                                           "xHarbour sources (*.prg)\0"
                                             "*.prg\0"
                                           "C sources (*.c)\0"
                                              "*.c\0";

                     ofn.Flags           = OFN_EXPLORER | OFN_ENABLESIZING | OFN_PATHMUSTEXIST| OFN_FILEMUSTEXIST | OFN_NOCHANGEDIR;
                     ofn.lpstrInitialDir = NULL;
                     ofn.lpstrDefExt     = NULL;
                     ofn.nFilterIndex    = 1;
                     ofn.lpstrFile       = sFile;
                     ofn.nMaxFile        = MAX_PATH;

                     if( GetOpenFileName( &ofn ) )
                     {
                        int iOffset;

                        if( hb_strnicmp( sFile, s_sCurrentDir, ( iOffset = strlen( s_sCurrentDir ) ) ) )
                        {
                           iOffset = -1;
                        }

                        // OutputDebugString( sFile );
                        // OutputDebugString( "\n" );

                        SetDlgItemText( hWnd, ID_Main, sFile + iOffset + 1 );
                     }

                     SetFocus( GetDlgItem( hWnd, ID_Main ) );
                  }
                  else if( CursorPos.y >= 77 && CursorPos.y <= 103 )
                  {
                     BROWSEINFO BrowseInfo;
                     LPITEMIDLIST ShellItemID;
                     char szFolder[ _MAX_DIR ], szDisplayName[ _MAX_DIR ], szLibFolders[ _MAX_DIR * 16];

                     BrowseInfo.hwndOwner = hWnd;
                     BrowseInfo.pidlRoot = NULL;
                     BrowseInfo.pszDisplayName = szDisplayName;
                     BrowseInfo.lpszTitle = "Please select location of additional default search folder for Library files.";
                     BrowseInfo.ulFlags = BIF_NEWDIALOGSTYLE | BIF_RETURNONLYFSDIRS | BIF_RETURNFSANCESTORS /*| BIF_SHAREABLE*/;
                     BrowseInfo.lpfn = NULL;
                     BrowseInfo.lParam = 0;
                     BrowseInfo.iImage = 0;

                     ShellItemID = SHBrowseForFolder( &BrowseInfo );

                     if( ShellItemID )
                     {
                        SHGetPathFromIDList( ShellItemID, szFolder );

                        s_pSH_Alloc->lpVtbl->Free( s_pSH_Alloc, ShellItemID );
                        ShellItemID = NULL;

                        strcat( szFolder, ";" );

                        GetDlgItemText( hWnd, ID_Libs, szLibFolders, _MAX_DIR * 16 );
                        strcat( szLibFolders, szFolder );
                        SetDlgItemText( hWnd, ID_Libs, szLibFolders );
                     }

                     SetFocus( GetDlgItem( hWnd, ID_Libs ) );
                  }
                  else if( CursorPos.y >= 126 && CursorPos.y <= 152 )
                  {
                     BROWSEINFO BrowseInfo;
                     LPITEMIDLIST ShellItemID;
                     char szFolder[ _MAX_DIR ], szDisplayName[ _MAX_DIR ], szIncludeFolders[ _MAX_DIR * 16 ];

                     BrowseInfo.hwndOwner = hWnd;
                     BrowseInfo.pidlRoot = NULL;
                     BrowseInfo.pszDisplayName = szDisplayName;
                     BrowseInfo.lpszTitle = "Please select location of additional default search folder for Include files.";
                     BrowseInfo.ulFlags = BIF_NEWDIALOGSTYLE | BIF_RETURNONLYFSDIRS | BIF_RETURNFSANCESTORS /*| BIF_SHAREABLE*/;
                     BrowseInfo.lpfn = NULL;
                     BrowseInfo.lParam = 0;
                     BrowseInfo.iImage = 0;

                     ShellItemID = SHBrowseForFolder( &BrowseInfo );

                     if( ShellItemID )
                     {
                        SHGetPathFromIDList( ShellItemID, szFolder );

                        s_pSH_Alloc->lpVtbl->Free( s_pSH_Alloc, ShellItemID );
                        ShellItemID = NULL;

                        strcat( szFolder, ";" );

                        GetDlgItemText( hWnd, ID_Includes, szIncludeFolders, _MAX_DIR * 16 );
                        strcat( szIncludeFolders, szFolder );
                        SetDlgItemText( hWnd, ID_Includes, szIncludeFolders );
                     }

                     SetFocus( GetDlgItem( hWnd, ID_Includes ) );
                  }
               }

               break;
            }
         }
      }

      case WM_MOUSEMOVE :
      {
         CursorPos = MAKEPOINTS( lParam );

         switch ( s_iStep )
         {
            case 0 :
            {
               BOOL bHit_1 = FALSE, bHit_2 = FALSE;
               HDC hDC = GetDC( hWnd );

               if( CursorPos.x >= 348 && CursorPos.x <= 374 )
               {
                  if( CursorPos.y >= 17 && CursorPos.y <= 42 )
                  {
                     bHit_1 = TRUE;

                     if( s_FolderState_1 == STATE_OFF )
                     {
                        PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_HOVER ], 348,  17, 26, 26, 0, 0 );
                        s_FolderState_1 = STATE_HOVER;
                     }
                  }
               }

               if( bHit_1 == FALSE && s_FolderState_1 == STATE_HOVER )
               {
                  PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 348,  17, 26, 26, 0, 0 );
                  s_FolderState_1 = STATE_OFF;
               }

               ReleaseDC( hWnd, hDC );

               break;
            }

            case 1 :
            {
               BOOL bHit_1 = FALSE, bHit_2 = FALSE, bHit_3;
               HDC hDC = GetDC( hWnd );

               if( CursorPos.x >= 348 && CursorPos.x <= 374 )
               {
                  if( CursorPos.y >= 28 && CursorPos.y <= 53 )
                  {
                     bHit_1 = TRUE;

                     if( s_FolderState_1 == STATE_OFF )
                     {
                        PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_HOVER ], 348,  28, 26, 26, 0, 0 );
                        s_FolderState_1 = STATE_HOVER;
                     }
                  }
                  else if( CursorPos.y >= 77 && CursorPos.y <= 103 )
                  {
                     bHit_2 = TRUE;

                     if( s_FolderState_2 == STATE_OFF )
                     {
                        PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_HOVER ], 348,  77, 26, 26, 0, 0 );
                        s_FolderState_2 = STATE_HOVER;
                     }
                  }
                  else if( CursorPos.y >= 126 && CursorPos.y <= 152 )
                  {
                     bHit_3 = TRUE;

                     if( s_FolderState_3 == STATE_OFF )
                     {
                        PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_HOVER ], 348, 126, 26, 26, 0, 0 );
                        s_FolderState_3 = STATE_HOVER;
                     }
                  }
               }

               if( bHit_1 == FALSE && s_FolderState_1 == STATE_HOVER )
               {
                  PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 348,  28, 26, 26, 0, 0 );
                  s_FolderState_1 = STATE_OFF;
               }

               if( bHit_2 == FALSE && s_FolderState_2 == STATE_HOVER )
               {
                  PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 348,  77, 26, 26, 0, 0 );
                  s_FolderState_2 = STATE_OFF;
               }

               if( bHit_3 == FALSE && s_FolderState_3 == STATE_HOVER )
               {
                  PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 348, 126, 26, 26, 0, 0 );
                  s_FolderState_3 = STATE_OFF;
               }

               ReleaseDC( hWnd, hDC );

               break;
            }

            /*
            case 2:
            {
               if( ListView_GetSelectedCount( s_ahTypeTabs[ s_iTypeTab ] ) )
               {
                  EnableWindow( GetDlgItem( hWnd, ID_OpenSource ), TRUE );
                  EnableWindow( GetDlgItem( hWnd, ID_Properties ), TRUE );
               }
               else
               {
                  EnableWindow( GetDlgItem( hWnd, ID_OpenSource ), FALSE );
                  EnableWindow( GetDlgItem( hWnd, ID_Properties ), FALSE );
               }

               break;
            }
            */
         }

         break;
      }

      case WM_PAINT:
      {
         PAINTSTRUCT ps;
         HDC hDC = BeginPaint( hWnd, &ps );

         SetBkMode( hDC, TRANSPARENT );

         switch ( s_iStep )
         {
            case 0 :
            {
               // OutputDebugString( "Paint Step1\n" );

               s_FolderState_1 = STATE_OFF, s_FolderState_2 = STATE_OFF, s_FolderState_3 = STATE_OFF;

               PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 348,  17, 26, 26, 0, 0 );

               EndPaint( hWnd, &ps );

               // OutputDebugString( "Painted\n" );

               return FALSE;
               //break;
            }

            case 1 :
            {
               // OutputDebugString( "Paint Step1\n" );

               s_FolderState_1 = STATE_OFF, s_FolderState_2 = STATE_OFF, s_FolderState_3 = STATE_OFF;

               PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 348,  28, 26, 26, 0, 0 );
               PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 348,  77, 26, 26, 0, 0 );
               PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 348, 126, 26, 26, 0, 0 );

               EndPaint( hWnd, &ps );

               // OutputDebugString( "Painted\n" );

               return FALSE;
               //break;
            }
         }

         break;
      }

      case WM_CTLCOLORDLG:
      {
         // Question: is there a way to set new background default?
         #if 0
            HDC hDC;
            hDC = GetDC( hWnd );
            SetBkMode( hDC, TRANSPARENT );
            ReleaseDC( hWnd, &ps );
         #endif

         #if 0
            // Question: Why not working?
            SetBkMode( (HDC) wParam, TRANSPARENT );
         #endif

         return (INT_PTR) s_hFormsBrush;
         //break;
      }

      case WM_CTLCOLORSTATIC:
      {
         //SetTextColor( (HDC) wParam, RGB( 0, 32, 255 ) );
         return (INT_PTR) s_hFormsBrush;
         //break;
      }

      case WM_NOTIFY:
      {
         LPNMHDR NotifyHeader = (LPNMHDR) lParam;

         //sprintf( sMessage, "Notify( %i ): %i, Id: %i\n", NotifyHeader->code, NotifyHeader->hwndFrom, NotifyHeader->idFrom );
         //OutputDebugString( sMessage );

         switch( NotifyHeader->code )
         {
            case NM_CLICK:
            {
               if( s_iStep == 2 )
               {
                  SelectTypeTab( s_iTypeTab == -1 ? 0 : s_iTypeTab, TRUE );

                  if( ListView_GetSelectedCount( s_ahTypeTabs[ s_iTypeTab ] ) )
                  {
                     EnableWindow( GetDlgItem( hWnd, ID_OpenSource ), TRUE );
                     EnableWindow( GetDlgItem( hWnd, ID_Properties ), TRUE );
                  }
                  else
                  {
                     EnableWindow( GetDlgItem( hWnd, ID_OpenSource ), FALSE );
                     EnableWindow( GetDlgItem( hWnd, ID_Properties ), FALSE );
                  }
               }

               break;
            }

            case NM_DBLCLK:
            {
               if( s_iStep == 2 )
               {
                  OpenModules();
               }

               break;
            }

            case NM_RCLICK:
            {
               // sprintf( sMessage, "RightClick hWnd: %i, Id: %i\n", NotifyHeader->hwndFrom, NotifyHeader->idFrom );
               // OutputDebugString( sMessage );

               if( NotifyHeader->hwndFrom == s_ahTypeTabs[ s_iTypeTab ] )
               {
                  FilesContextMenu();
               }

               break;
            }

            case TCN_SELCHANGE:
            {
               int iTab = TabCtrl_GetCurSel( NotifyHeader->hwndFrom );

               // OutputDebugString( "Change\n" );

               if( s_iStep == 2 )
               {
                  SelectTypeTab( iTab, TRUE );
               }
               else
               {
                  SelectSettingTab( iTab, TRUE );
               }

               break;
            }
         }

         break;
      }

      case WM_DROPFILES:
      {
         AcceptDropedFiles( (HDROP) wParam, GetAsyncKeyState( VK_CONTROL ) & 0x8000 );
         break;
      }

      case WM_INITDIALOG:
      {
         TOOLINFO ti;

         // OutputDebugString( "INIT StePagesProc\n" );

         ZeroMemory( &ti, sizeof( TOOLINFO ) );

         switch ( s_iStep )
         {
            case 0 :
            {
                HWND hCombo = GetDlgItem( hWnd, ID_Type );

                if( hCombo )
                {
                   // OutputDebugString( "Adding\n" );

                   SendMessage( hCombo, CB_ADDSTRING, 0, (LPARAM) "Exe" );
                   SendMessage( hCombo, CB_ADDSTRING, 0, (LPARAM) "Lib" );
                   SendMessage( hCombo, CB_ADDSTRING, 0, (LPARAM) "Dll" );

                   SendMessage( hCombo, CB_SETCURSEL, 0, 0 );
                }
                else
                {
                   TraceLog( "error.log", "Couldn't locate ID_Type!" );
                }

                if( s_hFormsBrush == NULL )
                {
                   s_hFormsBrush = CreateSolidBrush( GetSysColor( COLOR_WINDOW ) );
                   // SetClassLong( hWnd, GCL_HBRBACKGROUND, (LONG) s_hFormsBrush );
                }

                SetDlgItemText( hWnd, ID_Root, s_sCurrentDir );

                #if defined( DEMO ) || defined( PERSONAL )
                   EnableWindow( GetDlgItem( s_ahSettingTabs[1], ID_C_Debug ), FALSE );
                   EnableWindow( GetDlgItem( hWnd, ID_MT ), FALSE );
                   EnableWindow( GetDlgItem( hWnd, ID_UseDLL ), FALSE );
                #endif

                ti.cbSize = sizeof( TOOLINFO );
                ti.uFlags = TTF_SUBCLASS | TTF_IDISHWND;
                ti.hwnd   = hWnd;
                ti.uId    = (UINT) GetDlgItem( hWnd, ID_Root );
                ti.hinst  = s_hInstance;
                ti.lpszText = "The ROOT Folder is where the Project File will be saved "
                              "as well as the ROOT point from which the Target Folder and "
                              "Compiler's Output folder Target are created.";
                ti.rect.left = ti.rect.top = ti.rect.bottom = ti.rect.right = 0;

                SendMessage( s_Tooltip, TTM_ADDTOOL, 0, (LPARAM) &ti );

                ti.uFlags = TTF_SUBCLASS | TTF_IDISHWND;
                ti.hwnd   = hWnd;
                ti.uId    = (UINT) GetDlgItem( hWnd, ID_Target );
                ti.hinst  = s_hInstance;
                ti.lpszText = "Target may optionally include a SUB Folder (will be auto created if not existing).\n\n"
                              "It may optionally specify extension <.exe|.lib|.dll>, or you may select the type using the 'Target Type' Combobox.";
                ti.rect.left = ti.rect.top = ti.rect.bottom = ti.rect.right = 0;

                SendMessage( s_Tooltip, TTM_ADDTOOL, 0, (LPARAM) &ti );

                ti.uFlags = TTF_SUBCLASS | TTF_IDISHWND;
                ti.hwnd   = hWnd;
                ti.uId    = (UINT) GetDlgItem( hWnd, ID_Output );
                ti.hinst  = s_hInstance;
                ti.lpszText = "xBuild will direct all compiler's output (object files, etc.) to this SUB folder, "
                              "which will be auto created if not already existing.";
                ti.rect.left = ti.rect.top = ti.rect.bottom = ti.rect.right = 0;

                SendMessage( s_Tooltip, TTM_ADDTOOL, 0, (LPARAM) &ti );

                ti.uFlags = TTF_SUBCLASS | TTF_IDISHWND;
                ti.hwnd   = hWnd;
                ti.uId    = (UINT) GetDlgItem( hWnd, ID_Link );
                ti.hinst  = s_hInstance;
                ti.lpszText = "You may force xBuild to re-link your application, even if none of your sources has changed."
                              "This may be needed when linking against newer system or compiler libraries, etc..";
                ti.rect.left = ti.rect.top = ti.rect.bottom = ti.rect.right = 0;

                SendMessage( s_Tooltip, TTM_ADDTOOL, 0, (LPARAM) &ti );

                ti.uFlags = TTF_SUBCLASS | TTF_IDISHWND;
                ti.hwnd   = hWnd;
                ti.uId    = (UINT) GetDlgItem( hWnd, ID_GUI );
                ti.hinst  = s_hInstance;
                ti.lpszText = "What32 and FWH applications will be auto detected - you need to set this option ONLY for other types of GUI applications.";
                ti.rect.left = ti.rect.top = ti.rect.bottom = ti.rect.right = 0;

                SendMessage( s_Tooltip, TTM_ADDTOOL, 0, (LPARAM) &ti );

                ti.uFlags = TTF_SUBCLASS | TTF_IDISHWND;
                ti.hwnd   = hWnd;
                ti.uId    = (UINT) GetDlgItem( hWnd, ID_XBP );
                ti.hinst  = s_hInstance;
                ti.lpszText = "By default xBuild will save any changes to your project each time that you build it. "
                              "On occasions [for example when you have some commented lines in the project file] you might want "
                              "to retain the original project UNMODIFIED, in such cases please UN-Check this setting.";
                ti.rect.left = ti.rect.top = ti.rect.bottom = ti.rect.right = 0;

                SendMessage( s_Tooltip, TTM_ADDTOOL, 0, (LPARAM) &ti );

                break;
            }

            case 1 :
            {
                ti.cbSize = sizeof( TOOLINFO );
                ti.uFlags = TTF_SUBCLASS | TTF_IDISHWND;
                ti.hwnd   = hWnd;
                ti.uId    = (UINT) GetDlgItem( hWnd, ID_Libs );
                ti.hinst  = s_hInstance;
                ti.lpszText = "You do NOT need to worry about the Compilers' Lib folders, as they will be auto added to your project.\n\n"
                              "You may specify additional folder[s] (separated by ';') to search for library files.";
                ti.rect.left = ti.rect.top = ti.rect.bottom = ti.rect.right = 0;

                SendMessage( s_Tooltip, TTM_ADDTOOL, 0, (LPARAM) &ti );

                ti.uFlags = TTF_SUBCLASS | TTF_IDISHWND;
                ti.hwnd   = hWnd;
                ti.uId    = (UINT) GetDlgItem( hWnd, ID_Includes );
                ti.hinst  = s_hInstance;
                ti.lpszText = "You do NOT need to worry about the Compilers' Include folders, as they will be auto added to your project.\n\n"
                              "You may specify additional folder[s] (separated by ';') to search for #include files.";
                ti.rect.left = ti.rect.top = ti.rect.bottom = ti.rect.right = 0;

                SendMessage( s_Tooltip, TTM_ADDTOOL, 0, (LPARAM) &ti );

                ti.uFlags = TTF_SUBCLASS | TTF_IDISHWND;
                ti.hwnd   = hWnd;
                ti.uId    = (UINT) GetDlgItem( hWnd, ID_Defines );
                ti.hinst  = s_hInstance;
                ti.lpszText = "Multiple #defines[s] maybe specified separated by ';'";
                ti.rect.left = ti.rect.top = ti.rect.bottom = ti.rect.right = 0;

                SendMessage( s_Tooltip, TTM_ADDTOOL, 0, (LPARAM) &ti );

                break;
            }
         }

         SetWindowLong( hWnd, GWL_EXSTYLE, WS_EX_CONTROLPARENT );
         break;//return TRUE;
      }
   }

   return FALSE;
}

BOOL CALLBACK SettingsProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   //char sMessage[256];
   POINTS CursorPos;

   switch( message )
   {
      /*
      case WM_SETFOCUS:
      {
         // OutputDebugString( "Tab Settings Got Focus.\n" );
         break;
      }
      */

      case WM_COMMAND :
      {
         if( lParam )
         {
            switch HIWORD( wParam )
            {
               case EN_KILLFOCUS :
               {
                  // OutputDebugString( "Setting Edit Losing Focus\n" );

                  if( IsChild( s_hWnd, GetFocus() ) )
                  {
                     Validate( LOWORD( wParam ), s_ahSettingTabs[ s_iSettingTab ], (HWND) lParam, TRUE );
                  }

                  break;
               }

               case EN_SETFOCUS :
               {
                  char sMessage[256];

                  s_iValidating = LOWORD( wParam );
                  s_iSkipValidation = 0;

                  sprintf( sMessage, "SettingTab: %i, Control: %i\n", s_iSettingTab, s_iValidating );
                  OutputDebugString( sMessage );

                  switch ( s_iSettingTab )
                  {
                     case 2 :
                     {
                         switch ( s_iValidating )
                         {
                             case ID_FWH_Root :
                             {
                                char sRoot[ MAX_PATH ];

                                GetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, sRoot, MAX_PATH );

                                if( sRoot[0] == '\0' )
                                {
                                   //OutputDebugString( "Setting FWH\n" );
                                   InitFWH();
                                }

                                break;
                             }
                         }

                         break;
                     }
                  }

                  break;
               }

               case CBN_SELCHANGE :
               {
                  switch( SendMessage( (HWND) lParam, CB_GETCURSEL, 0, 0 ) )
                  {
                     case 0 :
                     {
                        #ifdef DEMO
                           Alert( "Sorry, this DEMO Version is restricted to using the DEMO Version of xHarbour from http://www.xHarbour.com" );
                           SendMessage( (HWND) lParam, CB_SETCURSEL, 4, 0 );
                        #else
                           strcpy( s_C_Compiler, "bcc32.exe" );

                           if( hb_stricmp( s_xHB_Exe, "xhb.exe" ) == 0 )
                           {
                              Alert( "Borland C Compiler is not compatible with object files generated by xHB.exe! "
                                     "Please make sure to change the xHarbour compiler Root Folder to a location containing Harbour.exe!" );

                              SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, "" );
                              s_xHB_Exe[0] = '\0';

                           }
                        #endif
                        break;
                     }

                     case 1 :
                     {
                        #ifdef DEMO
                           Alert( "Sorry, this DEMO Version is restricted to using the DEMO Version of xHarbour from http://www.xHarbour.com" );
                           SendMessage( (HWND) lParam, CB_SETCURSEL, 4, 0 );
                        #else
                           strcpy( s_C_Compiler, "gcc.exe" );

                           if( hb_stricmp( s_xHB_Exe, "xhb.exe" ) == 0 )
                           {
                              Alert( "GCC C Compiler is not compatible with object files generated by xHB.exe! "
                                     "Please make sure to change the xHarbour compiler Root Folder to a location containg Harbour.exe!" );

                              SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, "" );
                              s_xHB_Exe[0] = '\0';
                           }
                        #endif
                        break;
                     }

                     case 2 :
                     {
                        #ifdef DEMO
                           Alert( "Sorry, this DEMO Version is restricted to using the DEMO Version of xHarbour from http://www.xHarbour.com" );
                           SendMessage( (HWND) lParam, CB_SETCURSEL, 4, 0 );
                        #else
                           strcpy( s_C_Compiler, "cl.exe" );
                        #endif
                        break;
                     }

                     case 3 :
                     {
                        #ifdef DEMO
                           Alert( "Sorry, this DEMO Version is restricted to using the DEMO Version of xHarbour from http://www.xHarbour.com" );
                           SendMessage( (HWND) lParam, CB_SETCURSEL, 4, 0 );
                        #else
                           strcpy( s_C_Compiler, "pocc.exe" );
                        #endif
                        break;
                     }

                     case 4 :
                     {
                        strcpy( s_C_Compiler, "xcc.exe" );
                        break;
                     }
                  }

                  Init_C_Compiler();
                  break;
               }
            }
         }

         break;
      }

      case WM_LBUTTONUP :
      {
         CursorPos = MAKEPOINTS( lParam );

         switch ( s_iSettingTab )
         {
            case 0 :
            {
               if( CursorPos.x >= 335 && CursorPos.x <= 361 )
               {
                  if( CursorPos.y >= 22 && CursorPos.y <= 48 )
                  {
                     BROWSEINFO BrowseInfo;
                     char szFolder[ _MAX_DIR ], szDisplayName[ _MAX_DIR ];

                     BrowseInfo.hwndOwner = hWnd;
                     BrowseInfo.pidlRoot = NULL;
                     BrowseInfo.pszDisplayName = szDisplayName;
                     BrowseInfo.lpszTitle = "Please select the Root Folder of your xHarbour installation.";
                     BrowseInfo.ulFlags = BIF_NEWDIALOGSTYLE | BIF_RETURNONLYFSDIRS | BIF_RETURNFSANCESTORS /*| BIF_SHAREABLE*/;
                     BrowseInfo.lpfn = NULL;
                     BrowseInfo.lParam = 0;
                     BrowseInfo.iImage = 0;

                     if( s_xHB_RootID )
                     {
                        s_pSH_Alloc->lpVtbl->Free( s_pSH_Alloc, s_xHB_RootID );
                        s_xHB_RootID = NULL;
                     }

                     s_xHB_RootID = SHBrowseForFolder( &BrowseInfo );

                     if( s_xHB_RootID )
                     {
                        SHGetPathFromIDList( s_xHB_RootID, szFolder );

                        SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, szFolder );
                     }

                     SetFocus( GetDlgItem( s_ahSettingTabs[0], ID_xHB_Root ) );
                  }
                  else if( CursorPos.y >= 69 && CursorPos.y <= 95 )
                  {
                     BROWSEINFO BrowseInfo;
                     LPITEMIDLIST ShellItemID;
                     char szFolder[ _MAX_DIR ], szDisplayName[ _MAX_DIR ];
                     char xHB_Root[ _MAX_DIR ];

                     GetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, xHB_Root, _MAX_DIR );

                     if( xHB_Root[0] == '\0' )
                     {
                        Alert( "Sorry, xHarbour Root must be specified before you can select the LIB sub folder!" );
                        SetFocus( GetDlgItem( s_ahSettingTabs[0], ID_xHB_Root ) );
                        break;
                     }
                     else if( xHB_Root[1] != ':' )
                     {
                        int iLen;

                        memmove( xHB_Root + 2, xHB_Root, ( iLen = strlen( xHB_Root ) ) + 1 );
                        xHB_Root[0] = _getdrive() - 1 + 'A';
                        xHB_Root[1] = ':';

                        if( xHB_Root[ iLen + 1 ] == '\\' )
                        {
                           xHB_Root[ iLen + 1 ] = '\0';
                        }
                     }

                     BrowseInfo.hwndOwner = hWnd;
                     BrowseInfo.pidlRoot = s_xHB_RootID;
                     BrowseInfo.pszDisplayName = szDisplayName;
                     BrowseInfo.lpszTitle = "Please select the LIB Folder of your xHarbour installation.";
                     BrowseInfo.ulFlags = BIF_NEWDIALOGSTYLE | BIF_RETURNONLYFSDIRS | BIF_RETURNFSANCESTORS /*| BIF_SHAREABLE*/;
                     BrowseInfo.lpfn = NULL;
                     BrowseInfo.lParam = 0;
                     BrowseInfo.iImage = 0;

                     ShellItemID = SHBrowseForFolder( &BrowseInfo );

                     if( ShellItemID )
                     {
                        char sRelative[ MAX_PATH ];

                        SHGetPathFromIDList( ShellItemID, szFolder );

                        s_pSH_Alloc->lpVtbl->Free( s_pSH_Alloc, ShellItemID );
                        ShellItemID = NULL;

                        if( PathRelativePathTo( sRelative,
                                                xHB_Root,
                                                FILE_ATTRIBUTE_DIRECTORY,
                                                szFolder,
                                                FILE_ATTRIBUTE_DIRECTORY ) && sRelative[0] == '.' && sRelative[1] == '\\' )
                        {
                            SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Lib, sRelative + 2 );
                        }
                        else
                        {
                           Alert( "Lib folder must be a sub folder of the 'Root folder'!" );
                        }
                     }

                     SetFocus( GetDlgItem( s_ahSettingTabs[0], ID_xHB_Lib ) );
                  }
               }

               break;
            }

            case 1 :
            {
               if( CursorPos.x >= 335 && CursorPos.x <= 361 )
               {
                  if( CursorPos.y >= 70 && CursorPos.y <= 96 )
                  {
                     BROWSEINFO BrowseInfo;
                     LPITEMIDLIST ShellItemID;
                     char szFolder[ _MAX_DIR ], szDisplayName[ _MAX_DIR ];

                     BrowseInfo.hwndOwner = hWnd;
                     BrowseInfo.pidlRoot = NULL;
                     BrowseInfo.pszDisplayName = szDisplayName;
                     BrowseInfo.lpszTitle = "Please select the Root Folder of your C Compiler installation.";
                     BrowseInfo.ulFlags = BIF_NEWDIALOGSTYLE | BIF_RETURNONLYFSDIRS | BIF_RETURNFSANCESTORS /*| BIF_SHAREABLE*/;
                     BrowseInfo.lpfn = NULL;
                     BrowseInfo.lParam = 0;
                     BrowseInfo.iImage = 0;

                     ShellItemID = SHBrowseForFolder( &BrowseInfo );

                     if( ShellItemID )
                     {
                        SHGetPathFromIDList( ShellItemID, szFolder );

                        s_pSH_Alloc->lpVtbl->Free( s_pSH_Alloc, ShellItemID );
                        ShellItemID = NULL;

                        SetDlgItemText( s_ahSettingTabs[1], ID_C_Root, szFolder );
                     }

                     SetFocus( GetDlgItem( s_ahSettingTabs[0], ID_C_Root ) );
                  }
               }

               break;
            }

            case 2 :
            {
               if( CursorPos.x >= 335 && CursorPos.x <= 361 )
               {
                  if( CursorPos.y >= 77 && CursorPos.y <= 103 )
                  {
                     BROWSEINFO BrowseInfo;
                     char szFolder[ _MAX_DIR ], szDisplayName[ _MAX_DIR ];

                     BrowseInfo.hwndOwner = hWnd;
                     BrowseInfo.pidlRoot = NULL;
                     BrowseInfo.pszDisplayName = szDisplayName;
                     BrowseInfo.lpszTitle = "Please select the Root Folder of your FWH installation.";
                     BrowseInfo.ulFlags = BIF_NEWDIALOGSTYLE | BIF_RETURNONLYFSDIRS | BIF_RETURNFSANCESTORS /*| BIF_SHAREABLE*/;
                     BrowseInfo.lpfn = NULL;
                     BrowseInfo.lParam = 0;
                     BrowseInfo.iImage = 0;

                     if( s_FWH_RootID )
                     {
                        s_pSH_Alloc->lpVtbl->Free( s_pSH_Alloc, s_FWH_RootID );
                        s_FWH_RootID = NULL;
                     }

                     s_FWH_RootID = SHBrowseForFolder( &BrowseInfo );

                     if( s_FWH_RootID )
                     {
                        SHGetPathFromIDList( s_FWH_RootID, szFolder );

                        SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, szFolder );
                     }

                     SetFocus( GetDlgItem( s_ahSettingTabs[2], ID_FWH_Root ) );
                  }
                  else if( CursorPos.y >= 127 && CursorPos.y <= 153 )
                  {
                     BROWSEINFO BrowseInfo;
                     LPITEMIDLIST ShellItemID;
                     char szFolder[ _MAX_DIR ], szDisplayName[ _MAX_DIR ];
                     char FWH_Root[ _MAX_DIR ];

                     GetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, FWH_Root, _MAX_DIR );

                     if( FWH_Root[0] == '\0' )
                     {
                        //Alert( "Sorry, FWH Root must be specified before you can select the LIB sub folder!" );
                        //SetFocus( GetDlgItem( s_ahSettingTabs[2], ID_FWH_Root ) );
                        //break;
                     }

                     BrowseInfo.hwndOwner = hWnd;
                     BrowseInfo.pidlRoot = s_FWH_RootID;
                     BrowseInfo.pszDisplayName = szDisplayName;
                     BrowseInfo.lpszTitle = "Please select the desired LIB Folder of your FWH installation.";
                     BrowseInfo.ulFlags = BIF_NEWDIALOGSTYLE | BIF_RETURNONLYFSDIRS | BIF_RETURNFSANCESTORS /*| BIF_SHAREABLE*/;
                     BrowseInfo.lpfn = NULL;
                     BrowseInfo.lParam = 0;
                     BrowseInfo.iImage = 0;

                     ShellItemID = SHBrowseForFolder( &BrowseInfo );

                     if( ShellItemID )
                     {
                        SHGetPathFromIDList( ShellItemID, szFolder );

                        s_pSH_Alloc->lpVtbl->Free( s_pSH_Alloc, ShellItemID );
                        ShellItemID = NULL;

                        SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Lib, szFolder + strlen( FWH_Root ) + 1 );
                     }

                     SetFocus( GetDlgItem( s_ahSettingTabs[0], ID_FWH_Lib ) );
                  }
               }

               break;

            case 3 :
            {
               if( CursorPos.x >= 335 && CursorPos.x <= 361 )
               {
                  if( CursorPos.y >= 69 && CursorPos.y <= 95 )
                  {
                     BROWSEINFO BrowseInfo;
                     char szFolder[ _MAX_DIR ], szDisplayName[ _MAX_DIR ];

                     BrowseInfo.hwndOwner = hWnd;
                     BrowseInfo.pidlRoot = NULL;
                     BrowseInfo.pszDisplayName = szDisplayName;
                     BrowseInfo.lpszTitle = "Please select the Start Folder when running your executable.";
                     BrowseInfo.ulFlags = BIF_NEWDIALOGSTYLE | BIF_RETURNONLYFSDIRS | BIF_RETURNFSANCESTORS /*| BIF_SHAREABLE*/;
                     BrowseInfo.lpfn = NULL;
                     BrowseInfo.lParam = 0;
                     BrowseInfo.iImage = 0;

                     if( s_StartIn )
                     {
                        s_pSH_Alloc->lpVtbl->Free( s_pSH_Alloc, s_StartIn );
                        s_StartIn = NULL;
                     }

                     s_StartIn = SHBrowseForFolder( &BrowseInfo );

                     if( s_StartIn )
                     {
                        SHGetPathFromIDList( s_StartIn, szFolder );

                        SetDlgItemText( s_ahSettingTabs[3], ID_RUN_StartIn, szFolder );
                     }

                     SetFocus( GetDlgItem( s_ahSettingTabs[3], ID_RUN_StartIn ) );
                  }
               }

               break;
            }
            }
         }

         break;
      }

      case WM_MOUSEMOVE :
      {
         CursorPos = MAKEPOINTS( lParam );

         switch ( s_iSettingTab )
         {
            case 0 :
            {
               BOOL bHit_1 = FALSE, bHit_2 = FALSE;
               HDC hDC = GetDC( hWnd );

               if( CursorPos.x >= 335 && CursorPos.x <= 361 )
               {
                  if( CursorPos.y >= 22 && CursorPos.y <= 48 )
                  {
                     bHit_1 = TRUE;

                     if( s_FolderState_1 == STATE_OFF )
                     {
                        PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_HOVER ], 335, 22, 26, 26, 0, 0 );
                        s_FolderState_1 = STATE_HOVER;
                     }
                  }
                  else if( CursorPos.y >= 69 && CursorPos.y <= 95 )
                  {
                     bHit_2 = TRUE;

                     if( s_FolderState_2 == STATE_OFF )
                     {
                        PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_HOVER ], 335, 69, 26, 26, 0, 0 );
                        s_FolderState_2 = STATE_HOVER;
                     }
                  }
               }

               if( bHit_1 == FALSE && s_FolderState_1 == STATE_HOVER )
               {
                  PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 335,  22, 26, 26, 0, 0 );
                  s_FolderState_1 = STATE_OFF;
               }

               if( bHit_2 == FALSE && s_FolderState_2 == STATE_HOVER )
               {
                  PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 335, 69, 26, 26, 0, 0 );
                  s_FolderState_2 = STATE_OFF;
               }

               ReleaseDC( hWnd, hDC );

               break;
            }

            case 1 :
            {
               BOOL bHit_1 = FALSE;
               HDC hDC = GetDC( hWnd );

               if( CursorPos.x >= 335 && CursorPos.x <= 361 )
               {
                  if( CursorPos.y >= 70 && CursorPos.y <= 96 )
                  {
                     bHit_1 = TRUE;

                     if( s_FolderState_1 == STATE_OFF )
                     {
                        PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_HOVER ], 335, 70, 26, 26, 0, 0 );
                        s_FolderState_1 = STATE_HOVER;
                     }
                  }
               }

               if( bHit_1 == FALSE && s_FolderState_1 == STATE_HOVER )
               {
                  PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 335, 70, 26, 26, 0, 0 );
                  s_FolderState_1 = STATE_OFF;
               }

               ReleaseDC( hWnd, hDC );

               break;
            }

            case 2 :
            {
               BOOL bHit_1 = FALSE, bHit_2 = FALSE;
               HDC hDC = GetDC( hWnd );

               if( CursorPos.x >= 335 && CursorPos.x <= 361 )
               {
                  if( CursorPos.y >= 77 && CursorPos.y <= 103 )
                  {
                     bHit_1 = TRUE;

                     if( s_FolderState_1 == STATE_OFF )
                     {
                        PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_HOVER ], 335, 77, 26, 26, 0, 0 );
                        s_FolderState_1 = STATE_HOVER;
                     }
                  }
                  else if( CursorPos.y >= 127 && CursorPos.y <= 153 )
                  {
                     bHit_2 = TRUE;

                     if( s_FolderState_2 == STATE_OFF )
                     {
                        PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_HOVER ], 335, 127, 26, 26, 0, 0 );
                        s_FolderState_2 = STATE_HOVER;
                     }
                  }
               }

               if( bHit_1 == FALSE && s_FolderState_1 == STATE_HOVER )
               {
                  PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 335,  77, 26, 26, 0, 0 );
                  s_FolderState_1 = STATE_OFF;
               }

               if( bHit_2 == FALSE && s_FolderState_2 == STATE_HOVER )
               {
                  PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 335, 127, 26, 26, 0, 0 );
                  s_FolderState_2 = STATE_OFF;
               }

               ReleaseDC( hWnd, hDC );

               break;
            }

            case 3 :
            {
               BOOL bHit_1 = FALSE;
               HDC hDC = GetDC( hWnd );

               if( CursorPos.x >= 335 && CursorPos.x <= 361 )
               {
                  if( CursorPos.y >= 69 && CursorPos.y <= 95 )
                  {
                     bHit_1 = TRUE;

                     if( s_FolderState_1 == STATE_OFF )
                     {
                        PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_HOVER ], 335, 69, 26, 26, 0, 0 );
                        s_FolderState_1 = STATE_HOVER;
                     }
                  }
               }

               if( bHit_1 == FALSE && s_FolderState_1 == STATE_HOVER )
               {
                  PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 335, 69, 26, 26, 0, 0 );
                  s_FolderState_1 = STATE_OFF;
               }

               ReleaseDC( hWnd, hDC );

               break;
            }
         }

         break;
      }

      case WM_PAINT:
      {
         PAINTSTRUCT ps;
         HDC hDC = BeginPaint( hWnd, &ps );

         s_FolderState_1 = STATE_OFF, s_FolderState_2 = STATE_OFF, s_FolderState_3 = STATE_OFF;

         switch ( s_iSettingTab )
         {
            case 0 :
            {
               PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 335, 22, 26, 26, 0, 0 );
               PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 335, 69, 26, 26, 0, 0 );

               break;
            }

            case 1 :
            {
               PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 335, 70, 26, 26, 0, 0 );

               break;
            }

            case 2 :
            {
               PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 335,  77, 26, 26, 0, 0 );
               PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 335, 127, 26, 26, 0, 0 );

               break;
            }

            case 3 :
            {
               PaintBitmapDC( hWnd, hDC, s_ahFolder[ STATE_OFF ], 335, 69, 26, 26, 0, 0 );

               break;
            }
         }

         EndPaint( hWnd, &ps );

         break;
      }

      case WM_CTLCOLORDLG:
      {
         return (INT_PTR) s_hFormsBrush;
         //break;
      }

      case WM_CTLCOLORSTATIC:
      {
         //SetTextColor( (HDC) wParam, RGB( 0, 32, 255 ) );
         return (INT_PTR) s_hFormsBrush;
         //break;
      }

      /*
      case WM_NOTIFY:
      {
         LPNMHDR NotifyHeader = (LPNMHDR) lParam;

         // OutputDebugString( "Settings Notify\n" );

         switch( NotifyHeader->code )
         {
         }
      }
      */

      case WM_INITDIALOG:
      {
         HWND hCombo = GetDlgItem( hWnd, ID_C_Compiler );

         // OutputDebugString( "INIT Settings.\n" );

         if( hCombo )
         {
            SendMessage( hCombo, CB_ADDSTRING, 0, (LPARAM) "BCC" );
            SendMessage( hCombo, CB_ADDSTRING, 0, (LPARAM) "MingW" );
            SendMessage( hCombo, CB_ADDSTRING, 0, (LPARAM) "MSVC" );
            SendMessage( hCombo, CB_ADDSTRING, 0, (LPARAM) "PellesC" );
            SendMessage( hCombo, CB_ADDSTRING, 0, (LPARAM) "xCC" );

            SendMessage( hCombo, CB_SETCURSEL, 4, 0 );
         }

         //CheckDlgButton( hWnd, ID_RUN_Ask, BST_CHECKED );

         SetWindowLong( hWnd, GWL_EXSTYLE, WS_EX_CONTROLPARENT );
         break;//return TRUE;
      }
   }

   return FALSE;
}

void MoveModules( int iDirection )
{
   // char sMessage[256];
   UINT uiSelectedModules = ListView_GetSelectedCount( s_ahTypeTabs[ s_iTypeTab ] );
   UINT uiModules;
   LVITEM LVItem;
   int iStart, iBase, iIndex;

   // sprintf( sMessage, "Move %i files, Direction %i\n",  uiSelectedModules, iDirection );
   // OutputDebugString( sMessage );

   if( uiSelectedModules == 0 )
   {
      OutputDebugString( "No Selected files." );
      return;
   }

   uiModules = ListView_GetItemCount( s_ahTypeTabs[ s_iTypeTab ] );

   if( iDirection > 0 )
   {
      iStart = -1;

      // Find LAST selected file.
      while( ( iStart = ListView_GetNextItem( s_ahTypeTabs[ s_iTypeTab ], iStart, LVNI_SELECTED ) ) >= 0 )
      {
         iBase = iStart;
      }

      // File just BELOW the selected group.
      iBase++;
   }
   else
   {
      // Find FIRST select file.
      iBase = ListView_GetNextItem( s_ahTypeTabs[ s_iTypeTab ], -1, LVNI_SELECTED );

      // File just ABOVE the selected group.
      iBase--;
   }

   if( iBase >= 0 && (UINT) iBase < uiModules )
   {
      memset( &LVItem, 0, sizeof( LVITEM ) );

      LVItem.mask    = LVIF_PARAM;
      LVItem.iItem   = iBase;

      if( iDirection == MOVE_UP )
      {
         // Move NUMBER of SELECTED files REVERESED of the Group Direction.
         LVItem.lParam = (LPARAM) iBase + uiSelectedModules;

         // sprintf( sMessage, "Moving Base from: %i To: %i\n", iBase, LVItem.lParam );
         // OutputDebugString( sMessage );

         ListView_SetItem( s_ahTypeTabs[ s_iTypeTab ], &LVItem );
      }
      else if( iDirection == MOVE_DOWN )
      {
         // Move NUMBER of SELECTED files REVERESED of the Group Direction.
         LVItem.lParam = (LPARAM) iBase - uiSelectedModules;

         // sprintf( sMessage, "Moving Base from: %i To: %i\n", iBase, LVItem.lParam );
         // OutputDebugString( sMessage );

         ListView_SetItem( s_ahTypeTabs[ s_iTypeTab ], &LVItem );
      }
      if( iDirection == MOVE_TOP )
      {
         for( iIndex = 0; iIndex <= iBase; iIndex++ )
         {
            LVItem.mask    = LVIF_PARAM;
            LVItem.iItem   = iIndex;

            // Move NUMBER of SELECTED files REVERESED of the Group Direction.
            LVItem.lParam = iIndex + (LPARAM) uiSelectedModules;

            // sprintf( sMessage, "Moving intermediate from: %i To: %i\n", iIndex, LVItem.lParam );
            // OutputDebugString( sMessage );

            ListView_SetItem( s_ahTypeTabs[ s_iTypeTab ], &LVItem );
         }

         iDirection = -iIndex;
      }
      else if( iDirection == MOVE_BOTTOM )
      {
         for( iIndex = iBase; (UINT) iIndex < uiModules; iIndex++ )
         {
            LVItem.mask    = LVIF_PARAM;
            LVItem.iItem   = iIndex;

            // Move NUMBER of SELECTED files REVERESED of the Group Direction.
            LVItem.lParam = iIndex - (LPARAM) uiSelectedModules;

            // sprintf( sMessage, "Moving intermediate from: %i To: %i\n", iIndex, LVItem.lParam );
            // OutputDebugString( sMessage );

            ListView_SetItem( s_ahTypeTabs[ s_iTypeTab ], &LVItem );
         }

         iDirection = uiModules - iBase;
      }

      iStart = -1;
      while( ( iStart = ListView_GetNextItem( s_ahTypeTabs[ s_iTypeTab ], iStart, LVNI_SELECTED ) ) >= 0 )
      {
         LVItem.mask   = LVIF_PARAM;
         LVItem.iItem  = iStart;

         // Move in Group Direction.
         LVItem.lParam = (LPARAM) iStart + iDirection;

         // sprintf( sMessage, "Moving: %i To: %i\n", iStart, LVItem.lParam );
         // OutputDebugString( sMessage );

         ListView_SetItem( s_ahTypeTabs[ s_iTypeTab ], &LVItem );
      }

      ListView_SortItems( s_ahTypeTabs[ s_iTypeTab ], ListViewSorter, NULL  );
   }
}

void AddModules( void )
{
   OPENFILENAME ofn;
   char szFilesList[32768], sFile[ MAX_PATH ], szPath[ _MAX_DIR ], *pTemp;

   // OutputDebugString( "AddModules.\n" );

   szFilesList[0] = '\0';

   ZeroMemory( &ofn, sizeof( OPENFILENAME ) );

   ofn.hInstance       = s_hInstance;
   ofn.lStructSize     = sizeof( OPENFILENAME );
   ofn.hwndOwner       = s_ahStepPages[2];
   ofn.lpstrTitle      = "xBuild Wizard";
   ofn.lpstrFilter     = "All modules (*.prg, *.c, *.rc, *.y, *.obj, *.res, *.lib, *.xbp)\0"
                           "*.prg;*.c;*.rc;*.y;*.sly;*.obj;*.res;*.lib;*.xbp\0"
                         "xHarbour sources (*.prg)\0"
                           "*.prg\0"
                         "C sources (*.c)\0"
                            "*.c\0"
                         "Resouce files (*.rc)\0"
                            "*.rc\0"
                         "Yacc files (*.y)\0"
                            "*.y;*.sly\0"
                         "Object files (*.obj)\0"
                            "*.obj\0"
                         "Compiled resouces (*.res)\0"
                            "*.res\0"
                         "Library files (*.lib)\0"
                            "*.lib\0"
                         "xBuild Project files (*.xbp)\0"
                            "*.exe.xbp;*.lib.xbp;*.dll.xbp\0";

   ofn.Flags           = OFN_EXPLORER | OFN_ALLOWMULTISELECT | OFN_ENABLESIZING | OFN_PATHMUSTEXIST| OFN_FILEMUSTEXIST | OFN_NOCHANGEDIR;
   ofn.lpstrInitialDir = NULL;
   ofn.lpstrDefExt     = NULL;
   ofn.nFilterIndex    = 1;
   ofn.lpstrFile       = szFilesList;
   ofn.nMaxFile        = 32768;

   if( GetOpenFileName( &ofn ) )
   {
      // Single file selection.
      if( strrchr( szFilesList, '.' ) )
      {
         int iOffset;

         // OutputDebugString( "Single File\n" );

         if( hb_strnicmp( szFilesList, s_sCurrentDir, ( iOffset = strlen( s_sCurrentDir ) ) ) )
         {
            iOffset = -1;
         }

         AddModule( szFilesList + iOffset + 1 );

         return;
      }

      strcpy( szPath, szFilesList );

      if( hb_stricmp( szPath, s_sCurrentDir ) == 0 )
      {
         szPath[0] = '\0';
      }
      else
      {
         strcat( szPath, "\\" );
      }

      pTemp = szFilesList + ofn.nFileOffset;

      while( pTemp[0] )
      {
         strcpy( sFile, szPath );
         strcat( sFile, pTemp );
         pTemp += ( strlen( pTemp ) + 1 );

         // PathRelativePathTo( szRelative, s_sCurrentDir, FILE_ATTRIBUTE_DIRECTORY, sFile, FILE_ATTRIBUTE_NORMAL );
         // OutputDebugString( szRelative );

         // OutputDebugString( sFile );
         AddModule( sFile );
      }
   }
   else
   {
      OutputDebugString( "Failed to get files\n" );
   }
}

void OpenModules( void )
{
   UINT uiSelectedModules = ListView_GetSelectedCount( s_ahTypeTabs[ s_iTypeTab ] );
   int iStart = -1;
   char sFile[ MAX_PATH ];
   PHB_DYNS pSym = hb_dynsymFindName( "PopupEditor" );

   // char sMessage[256];

   // OutputDebugString( "DeleteModules.\n" );

   while( ( iStart = ListView_GetNextItem( s_ahTypeTabs[ s_iTypeTab ], iStart, LVNI_SELECTED ) ) >= 0 )
   {
      sFile[0] = '\0';
      ListView_GetItemText( s_ahTypeTabs[ s_iTypeTab ], iStart, 0, sFile, MAX_PATH );

      if( sFile[0] == '\0' )
      {
         continue;
      }

      OutputDebugString( "Open Module: " );
      OutputDebugString( sFile );
      OutputDebugString( "\n" );

      if( pSym )
      {
         hb_vmPushSymbol( pSym->pSymbol );
         hb_vmPushNil();
         hb_vmPushLong( (long) s_hWnd );
         hb_vmPushInteger( 10 );
         hb_vmPushInteger( 10 );
         hb_vmPushInteger( 780 );
         hb_vmPushInteger( 580 );
         hb_vmPushString( sFile, strlen( sFile ) );
         hb_vmDo( 6 );

         // Safety!
         hb_vmRequestReset();
      }
   }
}

void DeleteModules( void )
{
   UINT uiSelectedModules = ListView_GetSelectedCount( s_ahTypeTabs[ s_iTypeTab ] );
   int iStart = -1;
   // char sMessage[256];

   // OutputDebugString( "DeleteModules.\n" );

   while( ( iStart = ListView_GetNextItem( s_ahTypeTabs[ s_iTypeTab ], iStart, LVNI_SELECTED ) ) >= 0 )
   {
      // sprintf( sMessage, "Deleting: %i\n", iStart );
      // OutputDebugString( sMessage );

      s_bDeleted = TRUE;

      ListView_DeleteItem( s_ahTypeTabs[ s_iTypeTab ], iStart );
      iStart--;
   }
}

void FilesContextMenu( void )
{
   UINT uiSelectedModules = ListView_GetSelectedCount( s_ahTypeTabs[ s_iTypeTab ] );
   POINT CursorPosition;

   GetCursorPos( &CursorPosition );

   if( uiSelectedModules > 0 )
   {
      UINT uiModules = ListView_GetItemCount( s_ahTypeTabs[ s_iTypeTab ] );
      int iStart, iBase;

      EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_MOVEUP    , MF_ENABLED | MF_BYCOMMAND );
      EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_MOVEDOWN  , MF_ENABLED | MF_BYCOMMAND );
      EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_MOVETOP   , MF_ENABLED | MF_BYCOMMAND );
      EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_MOVEBOTTOM, MF_ENABLED | MF_BYCOMMAND );
      EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_OPEN      , MF_ENABLED | MF_BYCOMMAND );
      EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_PROPERTIES, MF_ENABLED | MF_BYCOMMAND );

      SelectTypeTab( s_iTypeTab == -1 ? 0 : s_iTypeTab, TRUE );

      if( ListView_GetSelectedCount( s_ahTypeTabs[ s_iTypeTab ] ) == 0 )
      {
         EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_OPEN      , MF_GRAYED | MF_BYCOMMAND );
         EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_PROPERTIES, MF_GRAYED | MF_BYCOMMAND );
      }

      iStart = -1;
      iBase  = -1;

      // Scan all selected files.
      while( ( iStart = ListView_GetNextItem( s_ahTypeTabs[ s_iTypeTab ], iStart, LVNI_SELECTED ) ) >= 0 )
      {
         // Disable UP and TOP.
         if( iStart == 0 )
         {
            EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_MOVEUP , MF_GRAYED | MF_BYPOSITION );
            EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_MOVETOP, MF_GRAYED | MF_BYPOSITION );
         }

         // Disable ALL movements.
         if( iBase != -1 && ( iStart - iBase ) > 1 )
         {
            EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_MOVEUP    , MF_GRAYED | MF_BYPOSITION );
            EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_MOVEDOWN  , MF_GRAYED | MF_BYPOSITION );
            EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_MOVETOP   , MF_GRAYED | MF_BYPOSITION );
            EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_MOVEBOTTOM, MF_GRAYED | MF_BYPOSITION );

            goto Done;
         }

         iBase = iStart;
      }

      // Disable DOWN, and BOTTOM
      if( (UINT) iBase == uiModules )
      {
         EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_MOVEDOWN  , MF_GRAYED | MF_BYPOSITION );
         EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_MOVEBOTTOM, MF_GRAYED | MF_BYPOSITION );
      }

    Done:

      // No properties for NON Source modules.
      /*
      if( s_iTypeTab > 2 )
      {
         EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_PROPERTIES, MF_GRAYED | MF_BYPOSITION );
      }
      else
      {
         EnableMenuItem( s_hModules_ContextMenu, IDM_MODULES_CONTEXT_PROPERTIES, MF_ENABLED | MF_BYPOSITION );
      }
      */

      TrackPopupMenu( s_hModules_ContextMenu, TPM_RIGHTBUTTON, CursorPosition.x, CursorPosition.y, 0, s_ahStepPages[2], NULL );
   }
}

void SetModulesProperties( void )
{
   //UINT uiSelectedModules = ListView_GetSelectedCount( s_ahTypeTabs[ s_iTypeTab ] );

   OutputDebugString( "Properties.\n" );
}

void SelectTypeTab( int iTab, BOOL bNewPage )
{
   // char sMessage[256];

   if( iTab != s_iTypeTab )
   {
      // sprintf( sMessage, "Selecting Type: %i\n", iTab );
      // OutputDebugString( sMessage );

      if( s_iTypeTab >= 0 )
      {
         ShowWindow( s_ahTypeTabs[ s_iTypeTab ], SW_HIDE );
      }

      if( iTab >= 0 )
      {
         TabCtrl_SetCurSel( s_hTypeTabControl, iTab );

         ShowWindow( s_ahTypeTabs[ iTab ], SW_SHOW );
         BringWindowToTop( s_ahTypeTabs[ iTab ] );
         SetForegroundWindow( s_ahTypeTabs[ iTab ] );

         SetFocus( s_ahTypeTabs[ iTab ] );
      }

      if( bNewPage )
      {
         s_iTypeTab = iTab;
      }
   }
}

BOOL Validate( int iControl, HWND hDlg, HWND hControl, BOOL bForce )
{
   //char sMessage[256];

   if( s_iSkipValidation == -1 || s_iValidating == 0 || s_iValidating == s_iSkipValidation )
   {
      return TRUE;
   }

   s_iValidating = 0;

   switch( iControl )
   {
      case ID_Root :
      {
         if( ValidateRoot( bForce ) )
         {
            return TRUE;
         }

         break;
      }

      case ID_Target :
      {
         if( ValidateTarget( bForce ) )
         {
            return TRUE;
         }

         break;
      }

      case ID_Main :
      {
         if( ValidateMain( bForce ) )
         {
            return TRUE;
         }

         break;
      }

      case ID_Output :
      {
         if( ValidateOutput() )
         {
            return TRUE;
         }

         break;
      }

      case ID_Libs :
      {
         if( ValidateFolders( hDlg, ID_Libs, NULL, bForce ) )
         {
            return TRUE;
         }

         break;
      }

      case ID_Includes :
      {
         if( ValidateFolders( hDlg, ID_Includes, NULL, bForce ) )
         {
            return TRUE;
         }

         break;
      }

      case ID_xHB_Root :
      {
         // OutputDebugString( "Validating xHB_Root.\n" );

         if( ValidateFolders( hDlg, ID_xHB_Root, NULL, bForce ) )
         {
            if( strcmp( s_xHB_Exe, "xhb.exe" ) == 0 )
            {
               char sRoot[ MAX_PATH ];
               int iCompiler;

               GetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, sRoot, MAX_PATH );

               iCompiler = SendMessage( GetDlgItem( s_ahSettingTabs[1], ID_C_Compiler ), CB_GETCURSEL, 0, 0 );

               if( iCompiler != 4 && iCompiler != 3  && iCompiler != 2 )
               {
                  Alert( "Previously selected C Compiler is not compatible with object files generated by xHB.exe! "
                         "I'll now reset the configured C compiler to xcc.exe." );

                  // Must reset C Compiler too.
                  SendMessage( GetDlgItem( s_ahSettingTabs[1], ID_C_Compiler ), CB_SETCURSEL, 4, 0 );
                  SetDlgItemText( s_ahSettingTabs[1], ID_C_Root, sRoot );
               }
            }

            return TRUE;
         }

         break;
      }

      case ID_xHB_Lib :
      {
         char xHB_Lib[ _MAX_DIR ], xHB_Root[ _MAX_DIR ];

         GetDlgItemText( s_ahSettingTabs[0], ID_xHB_Lib, xHB_Lib, _MAX_DIR );

         if( xHB_Lib[0] == '\0' )
         {
            return TRUE;
         }

         GetDlgItemText( s_ahSettingTabs[0], ID_xHB_Root, xHB_Root, _MAX_DIR );

         if( xHB_Root[0] == '\0' )
         {
            //Alert( "Sorry, xHarbour Root must be specified before you can select the LIB sub folder!" );
            //SetDlgItemText( s_ahSettingTabs[0], ID_xHB_Lib, "" );
            //hControl = GetDlgItem( s_ahSettingTabs[0], ID_xHB_Root );
            return TRUE;
         }

         if( ValidateFolders( s_ahSettingTabs[0], ID_xHB_Lib, xHB_Root, bForce ) )
         {
            return TRUE;
         }

         break;
      }

      case ID_C_Root :
      {
         // OutputDebugString( "Validating C_Root.\n" );

         if( ValidateFolders( s_ahSettingTabs[1], ID_C_Root, NULL, bForce ) )
         {
            return TRUE;
         }

         break;
      }

      case ID_FWH_Root :
      {
         // OutputDebugString( "Validating FWH_Root.\n" );

         if( ValidateFWH_Root( bForce ) )
         {
            char sFile[ MAX_PATH ];

            GetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, sFile, MAX_PATH );

            if( sFile[0] == '\0' )
            {
               SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Lib, "lib" );
            }

            return TRUE;
         }

         break;
      }

      case ID_FWH_Lib :
      {
         char FWH_Lib[ _MAX_DIR ], FWH_Root[ _MAX_DIR ];

         GetDlgItemText( s_ahSettingTabs[2], ID_FWH_Lib, FWH_Lib, _MAX_DIR );

         if( FWH_Lib[0] == '\0' )
         {
            SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Lib, "lib" );
            return TRUE;
         }

         GetDlgItemText( s_ahSettingTabs[2], ID_FWH_Root, FWH_Root, _MAX_DIR );

         if( FWH_Root[0] == '\0' )
         {
            //Alert( "Sorry, FWH Root, must be specified before you can select the LIB sub folder!" );
            //SetDlgItemText( s_ahSettingTabs[2], ID_FWH_Lib, "lib" );
            return TRUE;
         }

         if( ValidateFWH_Lib( bForce ) )
         {
            return TRUE;
         }

         break;
      }

      case ID_RUN_StartIn :
      {
         // OutputDebugString( "Validating RUN_StartIn.\n" );

         if( ValidateFolders( s_ahSettingTabs[3], ID_RUN_StartIn, NULL, bForce ) )
         {
            return TRUE;
         }

         break;
      }

      case ID_StartIn :
      {
         // OutputDebugString( "Validating StartIn.\n" );

         if( ValidateFolders( hDlg, ID_StartIn, NULL, bForce ) )
         {
            return TRUE;
         }

         break;
      }

      default :
      {
         return TRUE;
      }
   }

   if( bForce )
   {
      SetFocus( hDlg );
      SetFocus( hControl );
   }

   return FALSE;
}

void SelectSettingTab( int iTab, BOOL bNewPage )
{
   char sMessage[256];

   if( s_iValidating )
   {
      int iControl = s_iValidating;

      if( ! Validate( s_iValidating, s_ahSettingTabs[ s_iSettingTab ], GetDlgItem( s_ahSettingTabs[ s_iSettingTab ], s_iValidating ), TRUE ) )
      {
         TabCtrl_SetCurSel( s_hSettingTabControl, s_iSettingTab );
         SetFocus( GetDlgItem( s_ahSettingTabs[ s_iSettingTab ], iControl ) );

         return;
      }
   }

   if( iTab != s_iSettingTab )
   {
      sprintf( sMessage, "Selecting Setting: %i (New: %i)\n", iTab, bNewPage );
      OutputDebugString( sMessage );

      if( s_iSettingTab >= 0 )
      {
         ShowWindow( s_ahSettingTabs[ s_iSettingTab ], SW_HIDE );
      }

      if( iTab >= 0 )
      {
         if( bNewPage )
         {
            s_iSettingTab = iTab;
         }

         TabCtrl_SetCurSel( s_hSettingTabControl, iTab );

         ShowWindow( s_ahSettingTabs[ iTab ], SW_SHOW );
         //BringWindowToTop( s_ahTypeTabs[ iTab ] );
         //SetForegroundWindow( s_ahTypeTabs[ iTab ] );
         SetFocus( s_ahSettingTabs[ iTab ] );
      }
   }
}

void AcceptDropedFiles( HDROP hDrop, BOOL bClean )
{
   // char sMessage[256]
   char sFile[ MAX_PATH ];
   UINT uiNewFiles = DragQueryFile( hDrop, 0xFFFFFFFF, NULL, 0 ), uiIndex;
   int iOffset;

   if( uiNewFiles == 0xFFFFFFFF )
   {
      return;
   }

   if( bClean )
   {
      int iTab;

      for( iTab = 0; iTab < FILE_TYPES; iTab++ )
      {
         ListView_DeleteAllItems( s_ahTypeTabs[ iTab ] );
      }
   }

   // sprintf( sMessage, "Files: %i\n", uiNewFiles );
   // OutputDebugString( sMessage );

   SelectStepPage( 2, TRUE );

   // OutputDebugString( s_sCurrentDir );
   // OutputDebugString( "\n" );

   DragQueryFile( hDrop, 0, sFile, MAX_PATH );

   if( hb_strnicmp( s_sCurrentDir, sFile, ( iOffset = strlen( s_sCurrentDir ) ) ) )
   {
      iOffset = -1;
   }

   for( uiIndex = 0; uiIndex < uiNewFiles; uiIndex++ )
   {
      DragQueryFile( hDrop, uiIndex, sFile, MAX_PATH );

      AddModule( sFile + iOffset + 1 );
   }

   DragFinish( hDrop );
}

void AddModule( char *sFile )
{
   char   sExt[5];
   int    iLen, iType;
   LVITEM LVItem;

   //char sMessage[512];

   //sprintf( sMessage, "Len: %i File: '%s'\n", iLen, sFile );
   //OutputDebugString( sMessage );

  iLen = strlen( sFile );
  sExt[0] = '.';

  GetExtension:

   if( iLen > 4 && sFile[iLen - 4] == '.' )
   {
      sExt[ 1 ] = tolower( sFile[ iLen - 3 ] );
      sExt[ 2 ] = tolower( sFile[ iLen - 2 ] );
      sExt[ 3 ] = tolower( sFile[ iLen - 1 ] );
      sExt[ 4 ] = '\0';
   }
   else if( iLen > 3 && sFile[iLen - 3] == '.' )
   {
      sExt[ 1 ] = tolower( sFile[ iLen - 2 ] );
      sExt[ 2 ] = tolower( sFile[ iLen - 1 ] );
      sExt[ 3 ] = '\0';
   }
   else if( iLen > 2 && sFile[iLen - 2] == '.' )
   {
      sExt[ 1 ] = tolower( sFile[ iLen - 1 ] );
      sExt[ 2 ] = '\0';
   }
   else
   {
      OutputDebugString( "Unsupported Extension!\n" );
      return;
   }

   //sprintf( sMessage, "Extension: '%s'\n", sExt );
   //OutputDebugString( sMessage );

   if( sExt[1] == 'x' && sExt[2] == 'b' && sExt[3] == 'p' )
   {
      iLen -= 4;
      goto GetExtension;
   }
   else if( sExt[1] == 'd' && sExt[2] == 'l' && sExt[3] == 'l' )
   {
      sExt[ 1 ] = 'l';
      sExt[ 2 ] = 'i';
      sExt[ 3 ] = 'b';
      sExt[ 4 ] = '\0';
   }

   //sprintf( sMessage, ">Extension: '%s'\n", sExt );
   //OutputDebugString( sMessage );

   ZeroMemory( &LVItem, sizeof( LVITEM ) );

   for( iType = 0; iType < FILE_TYPES; iType++ )
   {
      if( strcmp( sExt, s_asTypes[iType] ) == 0 )
      {
         int iIndex;
         char sExists[ MAX_PATH ];

         for( iIndex = 0; iIndex < ListView_GetItemCount( s_ahTypeTabs[ iType ] ); iIndex++ )
         {
            sExists[0] = '\0';
            ListView_GetItemText( s_ahTypeTabs[ iType ], iIndex, 0, sExists, MAX_PATH );

            if( hb_stricmp( sFile, sExists ) == 0 )
            {
               return;
            }
         }

         s_bAdded = TRUE;

         SelectTypeTab( iType, TRUE );

         LVItem.mask    = LVIF_TEXT;// | LVIF_PARAM;
         LVItem.iItem   = iIndex;
         LVItem.pszText = sFile;
         LVItem.lParam  = (LPARAM) iIndex;

         //sprintf( sMessage, "Pos: %i File: '%s'\n", iIndex, sFile );
         //OutputDebugString( sMessage );

         iIndex = ListView_InsertItem( s_ahTypeTabs[ iType ], &LVItem );

         //sprintf( sMessage, ">Pos: %i File: '%s'\n", iIndex, sFile );
         //OutputDebugString( sMessage );

         break;
      }
   }

   if( iType == FILE_TYPES )
   {
      OutputDebugString( "Unsupported Type!\n" );
   }
   else
   {
      ListView_SortItems( s_ahTypeTabs[ iType ], ListViewSorter, NULL  );
   }
}

BOOL ValidateFolders( HWND hDlg, int iControlID, char *sRoot, BOOL bForce )
{
   //char sMessage[256];
   char sFolder[ MAX_PATH ], sFolders[ _MAX_DIR * 16 ], sErr[ MAX_PATH + 64 ], *pToken;
   WIN32_FIND_DATA FindFileData;
   HANDLE hFind;
   int iLen;

   if( bForce == FALSE )
   {
      return TRUE;
   }

   sFolder[0] = '\0';

   GetDlgItemText( hDlg, iControlID, sFolders, _MAX_DIR * 16 );

   pToken = strtok( sFolders, ";" );

   if( sFolders[0] == '\0' || ( iLen = strlen( pToken ) ) == 0 )
   {
      pToken = sFolder;
      goto Validate_Executable;
   }

   if( pToken[ --iLen ] == '\\' )
   {
      pToken[iLen] = '\0';
   }

   // sprintf( sMessage, "Validating folder: '%s'\n", pToken );
   // OutputDebugString( sMessage );

   if( sRoot )
   {
      strcpy( sFolder, sRoot );

      if( sRoot[ strlen( sRoot ) -1 ] != '\\' )
      {
         strcat( sFolder, "\\" );
      }

      strcat( sFolder, pToken );

      pToken = sFolder;
   }

   ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
   hFind = FindFirstFile( pToken, &FindFileData );

   if( hFind == INVALID_HANDLE_VALUE )
   {
      sprintf( sErr, "Sorry, folder not found: '%s'!", pToken );
      Alert( sErr );
      return FALSE;
   }
   else
   {
      if( ! ( FindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY ) )
      {
         sprintf( sErr, "Sorry, not a folder: '%s'!", pToken );
         Alert( sErr );
         FindClose( hFind );
         return FALSE;
      }

      FindClose( hFind );
   }

 Validate_Executable :

   switch( iControlID )
   {
      case ID_xHB_Root :
      {
         strcat( pToken, "\\bin\\" );
         iLen = strlen( pToken );
         strcat( pToken, s_xHB_Exe );

         ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
         hFind = FindFirstFile( pToken, &FindFileData );

         if( hFind == INVALID_HANDLE_VALUE )
         {
            pToken[ iLen ] = '\0';
            strcat( pToken, "xhb.exe" );

            ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
            hFind = FindFirstFile( pToken, &FindFileData );

            if( hFind == INVALID_HANDLE_VALUE )
            {
               pToken[ iLen ] = '\0';
               strcat( pToken, "harbour.exe" );

               ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
               hFind = FindFirstFile( pToken, &FindFileData );

               if( hFind == INVALID_HANDLE_VALUE )
               {
                  pToken[ iLen ] = '\0';
                  sprintf( sErr, "Sorry, couldn't locate xHarbour compiler at: '%s'!", pToken );
                  Alert( sErr );
                  return FALSE;
               }
               else
               {
                  FindClose( hFind );

                  strcpy( s_xHB_Exe, "harbour.exe" );
                  return TRUE;
               }
            }
            else
            {
               FindClose( hFind );

               strcpy( s_xHB_Exe, "xhb.exe" );
               return TRUE;
            }
         }
         else
         {
            FindClose( hFind );
            return TRUE;
         }

         break;
      }

      case ID_xHB_Lib :
      {
         strcat( pToken, "\\" );
         iLen = strlen( pToken );
         strcat( pToken, "xhb.lib" );

         ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
         hFind = FindFirstFile( pToken, &FindFileData );

         if( hFind == INVALID_HANDLE_VALUE )
         {
            pToken[ iLen ] = '\0';
            strcat( pToken, "rtl.lib" );

            ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
            hFind = FindFirstFile( pToken, &FindFileData );

            if( hFind == INVALID_HANDLE_VALUE )
            {
               pToken[ iLen ] = '\0';
               sprintf( sErr, "Sorry, couldn't locate xHarbour R/T support at: '%s'!", pToken );
               Alert( sErr );
               return TRUE;
            }
            else
            {
               FindClose( hFind );
               return TRUE;
            }
         }
         else
         {
            FindClose( hFind );
            return TRUE;
         }

         break;
      }

      case ID_C_Root :
      {
         strcat( pToken, "\\bin\\" );
         strcat( pToken, s_C_Compiler );

         ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
         hFind = FindFirstFile( pToken, &FindFileData );

         if( hFind == INVALID_HANDLE_VALUE )
         {
            sprintf( sErr, "Sorry, couldn't locate C Compiler at: '%s'!", pToken );
            Alert( sErr );
            return FALSE;
         }
         else
         {
            FindClose( hFind );
            return TRUE;
         }

         break;
      }

      case ID_FWH_Root :
      {
         if( pToken[0] )
         {
            strcat( pToken, "\\include\\fivewin.ch" );

            ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
            hFind = FindFirstFile( pToken, &FindFileData );

            if( hFind == INVALID_HANDLE_VALUE )
            {
               sprintf( sErr, "Sorry, couldn't locate FWH header at: '%s'!", pToken );
               Alert( sErr );
               return FALSE;
            }
            else
            {
               FindClose( hFind );
               return TRUE;
            }
         }

         break;
      }

      case ID_FWH_Lib :
      {
         if( pToken[0] )
         {
            if( strcmp( s_C_Compiler, "xcc.exe" ) == 0 || strcmp( s_C_Compiler, "cl.exe" ) == 0 )
            {
               strcat( pToken, "\\FiveHMX.lib" );
            }
            else
            {
               strcat( pToken, "\\FiveHX.lib" );
            }

            ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
            hFind = FindFirstFile( pToken, &FindFileData );

            if( hFind == INVALID_HANDLE_VALUE )
            {
               sprintf( sErr, "Sorry, couldn't locate FWH R/T support at: '%s'!", pToken );
               Alert( sErr );
               return FALSE;
            }
            else
            {
               FindClose( hFind );
               return TRUE;
            }
         }

         break;
      }
   }

   while( ( pToken = strtok( NULL, "; " ) ) != NULL )
   {
      ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
      hFind = FindFirstFile( pToken, &FindFileData );

      if( hFind == INVALID_HANDLE_VALUE )
      {
         sprintf( sErr, "Sorry, folder nt found: '%s'!", pToken );
         Alert( sErr );

         return FALSE;
      }
      else
      {
         if( FindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY )
         {
            FindClose( hFind );
         }
         else
         {
            sprintf( sErr, "Sorry, not a directory: '%s'!", pToken );
            Alert( sErr );
            FindClose( hFind );
            return FALSE;
         }
      }
   }

   return TRUE;
}

BOOL ValidateRoot( BOOL bForce )
{
   char sMessage[ MAX_PATH + 64 ];
   char sFile[ MAX_PATH ];

   GetDlgItemText( s_ahStepPages[0], ID_Root, sFile, MAX_PATH );

   if( sFile[0] == '\0' )
   {
      SetDlgItemText( s_ahStepPages[0], ID_Root, s_sCurrentDir );
      return TRUE;
   }

   if( SetCurrentDirectory( sFile ) )
   {
      strcpy( s_sCurrentDir, sFile );

      return TRUE;
   }
   else
   {
      sprintf( sMessage, "Sorry, could not change folder to: '%s'!", sFile );
      Alert( sMessage );
   }

   return FALSE;
}

BOOL ValidateMain( BOOL bForce )
{
   //char sMessage[256];
   char sMain[ MAX_PATH ];
   char *pTemp;
   WIN32_FIND_DATA FindFileData;
   HANDLE hFind;
   long lType = SendMessage( GetDlgItem( s_ahStepPages[ 0 ], ID_Type ), CB_GETCURSEL, 0, 0 );

   GetDlgItemText( s_ahStepPages[1], ID_Main, sMain, MAX_PATH );

   if( lType > 0 )
   {
      // sprintf( sMessage, "Type: %i %i\n", lType, lType > 0 );
      // OutputDebugString( sMessage );

      if( sMain[0] )
      {
         HWND hFocus = GetFocus();
         char sMessage[ 256 + MAX_PATH ];

         sprintf( sMessage, "Non Executable Target should have NO Main Module - Would you like me to move '%s' to modules list?\n\n[if you choose 'No' I'll simply clear the Main Module!]", sMain );

         //Alert( "Warning, NON Executable target, there should be no main module\n\nMoved main source to modules list.!" );
         if( MessageBox( s_hWnd, sMessage, "xBuild Wizard", MB_YESNO | MB_ICONQUESTION ) == IDYES )
         {
            AddModule( sMain );
         }

         SetFocus( hFocus );
         SetDlgItemText( s_ahStepPages[1], ID_Main, "" );
      }

      return TRUE;
   }

   if( sMain[0] == '\0' )
   {
      if( bForce )
      {
         Alert( "Sorry, Main module must be specified!" );
      }

      return FALSE;
   }

   if( ( pTemp = strrchr( sMain, '.' ) ) != NULL )
   {
      if( hb_stricmp( pTemp, ".prg" ) == 0  )
      {
      }
      else if( hb_stricmp( pTemp, ".c" ) == 0  )
      {
      }
      else
      {
         Alert( "Please correct, Main module must be <.prg> or <.c> extension." );
         return FALSE;
      }
   }
   else
   {
      strcat( sMain, ".prg" );
      SetDlgItemText( s_ahStepPages[1], ID_Main, sMain );
   }

   if( sMain[0] == '.' )
   {
      Alert( "Invalid name format." );
      return FALSE;
   }

   ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
   hFind = FindFirstFile( sMain, &FindFileData );

   if( hFind == INVALID_HANDLE_VALUE )
   {
      if( bForce )
      {
         Alert( "Warning, couldn't find the specified main module." );
      }

      return FALSE;
   }
   else
   {
      int iLen = strlen( sMain ) - 3;
      FindClose( hFind );

      sMain[ iLen ]     = 'r';
      sMain[ iLen + 1 ] = 'c';
      sMain[ iLen + 2 ] = '\0';

      ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
      hFind = FindFirstFile( sMain, &FindFileData );

      if( s_bAddResourceAsk && hFind != INVALID_HANDLE_VALUE && IsDlgButtonChecked( s_ahStepPages[0], ID_GUI ) )
      {
         int iIndex;
         char sExists[ MAX_PATH ];

         s_bAddResourceAsk = FALSE;

         for( iIndex = 0; iIndex < ListView_GetItemCount( s_ahTypeTabs[ 2 ] ); iIndex++ )
         {
            sExists[0] = '\0';
            ListView_GetItemText( s_ahTypeTabs[ 2 ], iIndex, 0, sExists, MAX_PATH );

            if( hb_stricmp( sMain, sExists ) == 0 )
            {
               FindClose( hFind );
               return TRUE;
            }
         }

         if( MessageBox( s_hWnd, "Found a resource file matching your main prg file, would you like me to add it to this project?", "xBuild Wizard", MB_YESNO | MB_ICONQUESTION ) == IDYES )
         {
            HWND hFocus = GetFocus();

            AddModule( sMain );

            SetFocus( hFocus );
         }

         FindClose( hFind );
      }
   }

   return TRUE;
}

BOOL ValidateTarget( BOOL bForce )
{
   char sTarget[ MAX_PATH + 64 ], sMain[ MAX_PATH ], sTooltip[ MAX_PATH + 64 ];
   char *pTemp;
   FILE *hFile;
   WIN32_FIND_DATA FindFileData;
   HANDLE hFind;
   RECT Area;
   TOOLINFO ti;
   long lType;

   GetDlgItemText( s_ahStepPages[0], ID_Target, sTarget, MAX_PATH );

   if( sTarget[0] == '\0' )
   {
      if( bForce )
      {
         Alert( "Sorry, Target name must be specified!" );
      }

      ZeroMemory( &ti, sizeof( TOOLINFO ) );
      ti.cbSize = sizeof( TOOLINFO );
      ti.hwnd     = s_hWnd;
      ti.uId      = ID_Save;
      ti.hinst    = s_hInstance;
      ti.lpszText = "Nothing to save yet.";

      SendMessage( s_Tooltip, (UINT) TTM_UPDATETIPTEXT, (WPARAM) 0, (LPARAM) &ti );

      return FALSE;
   }

   if( ( pTemp = strrchr( sTarget, '.' ) ) != NULL )
   {
      if( hb_stricmp( pTemp, ".exe" ) == 0  )
      {
         // already default.
         SendMessage( GetDlgItem( s_ahStepPages[0], ID_Type ), CB_SETCURSEL, 0, 0 );
         lType = 0;
      }
      else if( hb_stricmp( pTemp, ".lib" ) == 0  )
      {
         SendMessage( GetDlgItem( s_ahStepPages[0], ID_Type ), CB_SETCURSEL, 1, 0 );
         lType = 1;
      }
      else if( hb_stricmp( pTemp, ".dll" ) == 0 )
      {
         SendMessage( GetDlgItem( s_ahStepPages[0], ID_Type ), CB_SETCURSEL, 2, 0 );
         lType = 2;
      }
      else
      {
         Alert( "Please correct - Invalid Target extension." );

         ZeroMemory( &ti, sizeof( TOOLINFO ) );
         ti.cbSize = sizeof( TOOLINFO );
         ti.hwnd     = s_hWnd;
         ti.uId      = ID_Save;
         ti.hinst    = s_hInstance;
         ti.lpszText = "Nothing to save yet.";

         SendMessage( s_Tooltip, (UINT) TTM_UPDATETIPTEXT, (WPARAM) 0, (LPARAM) &ti );

         return FALSE;
      }
   }
   else
   {
      lType = SendMessage( GetDlgItem( s_ahStepPages[ 0 ], ID_Type ), CB_GETCURSEL, 0, 0 );

      switch( lType )
      {
         case 0 :
         {
            strcat( sTarget, ".exe" );
            break;
         }

         case 1 :
         {
            strcat( sTarget, ".lib" );
            break;
         }

         case 2 :
         {
            strcat( sTarget, ".dll" );
            break;
         }
      }

      SetDlgItemText( s_ahStepPages[0], ID_Target, sTarget );
   }

   if( sTarget[0] == '.' )
   {
      Alert( "Invalid Target format." );

      ZeroMemory( &ti, sizeof( TOOLINFO ) );
      ti.cbSize = sizeof( TOOLINFO );
      ti.hwnd     = s_hWnd;
      ti.uId      = ID_Save;
      ti.hinst    = s_hInstance;
      ti.lpszText = "Nothing to save yet.";

      SendMessage( s_Tooltip, (UINT) TTM_UPDATETIPTEXT, (WPARAM) 0, (LPARAM) &ti );

      return FALSE;
   }

   ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
   hFind = FindFirstFile( sTarget, &FindFileData );

   if( hFind == INVALID_HANDLE_VALUE )
   {
      pTemp = strrchr( sTarget, '\\' );

      if( pTemp )
      {
         pTemp[0] = '\0';

         if( ! PathFileExists( sTarget ) )
         {
            PHB_DYNS pSym = hb_dynsymFindName( "MakeNestedDir" );

            if( pSym )
            {
               hb_vmPushSymbol( pSym->pSymbol );
               hb_vmPushNil();
               hb_vmPushString( sTarget, pTemp - sTarget );
               hb_vmDo( 1 );

               // Safety!
               hb_vmRequestReset();
            }
            else
            {
               Alert( "Sorry, cound't locate: 'MakeNestedDir()'!" );
            }
         }

         pTemp[0] = '\\';
      }

      hFile = fopen( sTarget, "w" );

      if( hFile == NULL )
      {
         //if( bForce )
         {
            if( pTemp )
            {
               Alert( "Warning, Target is not creatable!\n\nPlease note that the path prefix must be RELATIVE to the Project's ROOT folder!" );
            }
            else
            {
               Alert( "Warning, Target is not creatable!" );
            }

            ZeroMemory( &ti, sizeof( TOOLINFO ) );
            ti.cbSize = sizeof( TOOLINFO );
            ti.hwnd     = s_hWnd;
            ti.uId      = ID_Save;
            ti.hinst    = s_hInstance;
            ti.lpszText = "Nothing to save yet.";

            SendMessage( s_Tooltip, (UINT) TTM_UPDATETIPTEXT, (WPARAM) 0, (LPARAM) &ti );

            return FALSE;
         }
      }
      else
      {
         fclose( hFile );
         remove( sTarget );
      }
   }
   else
   {
      FindClose( hFind );
   }

   Area.left  = 40;
   Area.top   = 83;
   Area.right = 400;
   Area.bottom = 100;

   InvalidateRect( s_hWnd, &Area, FALSE );
   SendMessage( s_hWnd, WM_PAINT, 0, 0 );

   // ----------------------- Set Main ----------------------------
   if( lType == 0 )
   {
      GetDlgItemText( s_ahStepPages[1], ID_Main, sMain, MAX_PATH );

      if( sMain[0] == '\0' )
      {
         int iLen = strlen( sTarget ) - 3;

         strncpy( sMain, sTarget, iLen );
         sMain[ iLen ]     = 'p';
         sMain[ iLen + 1 ] = 'r';
         sMain[ iLen + 2 ] = 'g';
         sMain[ iLen + 3 ] = '\0';

         ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
         hFind = FindFirstFile( sMain, &FindFileData );

         if( hFind == INVALID_HANDLE_VALUE )
         {
            sMain[ iLen ]     = 'c';
            sMain[ iLen + 1 ] = '\0';

            ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
            hFind = FindFirstFile( sMain, &FindFileData );

            if( hFind != INVALID_HANDLE_VALUE )
            {
               SetDlgItemText( s_ahStepPages[1], ID_Main, sMain );
               FindClose( hFind );
            }
         }
         else
         {
            FindClose( hFind );

            SetDlgItemText( s_ahStepPages[1], ID_Main, sMain );

            sMain[ iLen ]     = 'r';
            sMain[ iLen + 1 ] = 'c';
            sMain[ iLen + 2 ] = '\0';

            ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
            hFind = FindFirstFile( sMain, &FindFileData );

            if( hFind != INVALID_HANDLE_VALUE )
            {
               HWND hFocus = GetFocus();

               AddModule( sMain );
               SetFocus( hFocus );

               FindClose( hFind );
            }
         }
      }
   }

   sprintf( sTooltip, "%s\\%s.xbp", s_sCurrentDir, sTarget );

   ZeroMemory( &ti, sizeof( TOOLINFO ) );
   ti.cbSize = sizeof( TOOLINFO );
   ti.hwnd     = s_hWnd;
   ti.uId      = ID_Save;
   ti.hinst    = s_hInstance;
   ti.lpszText = sTooltip;

   SendMessage( s_Tooltip, (UINT) TTM_UPDATETIPTEXT, (WPARAM) 0, (LPARAM) &ti );

   sprintf( sTarget, "xBuild - %s", sTooltip );
   SendMessage( s_hWnd, WM_SETTEXT, (WPARAM) 0, (LPARAM) sTarget );

   return TRUE;
}

BOOL ValidateOutput()
{
   char sOutput[ MAX_PATH ], sErr[ MAX_PATH + 64 ], *pTemp;
   WIN32_FIND_DATA FindFileData;
   HANDLE hFind;

   GetDlgItemText( s_ahStepPages[0], ID_Output, sOutput, MAX_PATH );

   if( sOutput[0] == '\0' )
   {
      return TRUE;
   }

   strcpy( sOutput, s_sCurrentDir );
   strcat( sOutput, "\\" );
   pTemp = sOutput + strlen( sOutput );

   GetDlgItemText( s_ahStepPages[0], ID_Output, pTemp, MAX_PATH );

   ZeroMemory( &FindFileData, sizeof( WIN32_FIND_DATA ) );
   hFind = FindFirstFile( sOutput, &FindFileData );

   if( hFind == INVALID_HANDLE_VALUE )
   {
      PHB_DYNS pSym = hb_dynsymFindName( "MakeNestedDir" );

      if( pSym )
      {
         hb_vmPushSymbol( pSym->pSymbol );
         hb_vmPushNil();
         hb_vmPushString( pTemp, strlen( pTemp ) );
         hb_vmDo( 1 );

         // Safety!
         hb_vmRequestReset();

         hFind = FindFirstFile( sOutput, &FindFileData );

         if( hFind == INVALID_HANDLE_VALUE )
         {
            Alert( "Sorry, couldn't create the specified output folder\n\nPlease note that the path must be RELATIVE to the Project's ROOT folder!" );
            return FALSE;
         }
         else
         {
            // Must be a folder, because didn't exist until the above call to MakeNestedDir().
            FindClose( hFind );
         }
      }
      else
      {
         Alert( "Sorry, cound't locate: 'MakeNestedDir()'!" );
         return FALSE;
      }
   }
   else
   {
      if( FindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY )
      {
         FindClose( hFind );
      }
      else
      {
         sprintf( sErr, "Sorry, file '%s' is in the way - pleas remove!", pTemp );
         Alert( sErr );
         FindClose( hFind );
         return FALSE;
      }
   }

   return TRUE;
}

void Init_C_Compiler( void )
{
   char sFile[ MAX_PATH ];
   char sFolder[ MAX_PATH ];
   char *sProgramFolder;

   GetDlgItemText( s_ahSettingTabs[1], ID_C_Root, sFile, MAX_PATH );

   if( sFile[ strlen( sFile ) - 1 ] != '\\' )
   {
      strcat( sFile, "\\" );
   }

   strcat( sFile, s_C_Compiler );

   if( PathFileExists( sFile ) )
   {
      return;
   }


   sProgramFolder = getenv( "ProgramFiles" );

   if( sProgramFolder == NULL )
   {
       sProgramFolder = "C:\\Programs Files";
   }

   switch( s_C_Compiler[0] )
   {
      case 'x' :
      {
         if( PathFileExists( "\\xhb\\bin\\xcc.exe" ) )
         {
            SetDlgItemText( s_ahSettingTabs[1], ID_C_Root, "\\xhb" );
         }
         else if( PathFileExists( "c:\\xhb\\bin\\xcc.exe" ) )
         {
            SetDlgItemText( s_ahSettingTabs[1], ID_C_Root, "c:\\xhb" );
         }

         break;
      }

      case 'b' :
      {
         char *Folders[] = { "Borland\\BDS\\4.0" };
         int i;

         for( i = 0; i < ( sizeof( Folders ) / sizeof( char * ) ); i++ )
         {
            sprintf( sFolder, "%s\\%s", sProgramFolder, Folders[i] );
            sprintf( sFile, "%s\\bin\\bcc32.exe", sFolder );

            if( PathFileExists( sFile ) )
            {
               SetDlgItemText( s_ahSettingTabs[1], ID_C_Root, sFolder );
               break;
            }
         }

         if( i < ( sizeof( Folders ) / sizeof( char * ) ) )
         {
            break;
         }
         else if( PathFileExists( "\\bcc55\\bin\\bcc32.exe" ) )
         {
            SetDlgItemText( s_ahSettingTabs[1], ID_C_Root, "\\bcc55" );
         }
         else if( PathFileExists( "c:\\bcc55\\bin\\bcc32.exe" ) )
         {
            SetDlgItemText( s_ahSettingTabs[1], ID_C_Root, "c:\\bcc55" );
         }
         else if( PathFileExists( "\\borland\\bcc55\\bin\\bcc32.exe" ) )
         {
            SetDlgItemText( s_ahSettingTabs[1], ID_C_Root, "\\borland\\bcc55" );
         }
         else if( PathFileExists( "c:\\borland\\bcc55\\bin\\bcc32.exe" ) )
         {
            SetDlgItemText( s_ahSettingTabs[1], ID_C_Root, "c:\\borland\\bcc55" );
         }

         break;
      }

      case 'c' :
      {
         char *Folders[] = { "Microsoft Visual Studio 9.0\\VC",
                             "Microsoft Visual Studio 8\\VC",
                             "Microsoft Visual Studio .NET 2003\\VC7",
                             "Microsoft Visual Studio\\VC98" };
         int i;

         for( i = 0; i < ( sizeof( Folders ) / sizeof( char * ) ); i++ )
         {
            sprintf( sFolder, "%s\\%s", sProgramFolder, Folders[i] );
            sprintf( sFile, "%s\\bin\\cl.exe", sFolder );

            if( PathFileExists( sFile ) )
            {
               SetDlgItemText( s_ahSettingTabs[1], ID_C_Root, sFolder );
               break;
            }
         }

         break;
      }

      case 'p' :
      {
         char *Folders[] = { "PellesC" };
         int i;

         for( i = 0; i < ( sizeof( Folders ) / sizeof( char * ) ); i++ )
         {
            sprintf( sFolder, "%s\\%s", sProgramFolder, Folders[i] );
            sprintf( sFile, "%s\\bin\\pocc.exe", sFolder );

            if( PathFileExists( sFile ) )
            {
               SetDlgItemText( s_ahSettingTabs[1], ID_C_Root, sFolder );
               break;
            }
         }
         break;
      }

      case 'g' :
      {
         // TODO!
         break;
      }
   }
}

HFONT Stretch( HWND hWnd, int iNewWidth, int iNewHeight, int iOldWidth, int iOldHeight, BOOL bPaint, int iLeft, int iTop, char *sDefaultFont, BOOL bMonoFont )
{
   //char sMessage[ 256 ];
   STRETCHINFO StretchInfo;
   char sFont[ 64 ];
   HDC hDC = GetDC( hWnd );
   HFONT hFont = NULL;
   RECT rct;
   TEXTMETRIC tm;
   POINT TopLeft;

   if( hWnd == 0 )
   {
      Alert( "Invalid hWnd passed to Stretch()" );
      return NULL;
   }

   GetWindowRect( hWnd, &rct );

   if( iOldWidth == -1 )
   {
      iOldWidth = rct.right - rct.left + 1;
   }

   if( iOldHeight == -1 )
   {
      iOldHeight = rct.bottom - rct.top + 1;
   }

   if( iNewWidth == -1 )
   {
      if( iNewHeight == -1 )
      {
         Alert( "Invalid new size!" );
      }
      else
      {
         StretchInfo.fYFactor = (float) iNewHeight / (float) iOldHeight;
         StretchInfo.fXFactor = StretchInfo.fYFactor;
      }
   }
   else
   {
      StretchInfo.fXFactor = (float) iNewWidth / (float) iOldWidth;
   }

   if( iNewHeight == -1 )
   {
      StretchInfo.fYFactor  = StretchInfo.fXFactor;
   }
   else
   {
      StretchInfo.fYFactor = (float) iNewHeight / (float) iOldHeight;
   }

   #ifdef FIXED_FACTOR
      if( StretchInfo.fYFactor > StretchInfo.fXFactor )
      {
         StretchInfo.fYFactor = StretchInfo.fXFactor;
      }
      else
      {
         StretchInfo.fXFactor = StretchInfo.fYFactor;
      }
   #endif

   if( iNewHeight == -1 )
   {
      iNewHeight = (int) floor ( StretchInfo.fYFactor * (float) iOldHeight );
   }

   if( iNewWidth == -1 )
   {
      iNewWidth = (int) floor ( StretchInfo.fXFactor * (float) iOldWidth );
   }


   SelectObject( hDC, (HFONT) SendMessage( hWnd, WM_GETFONT, 0, 0 ) );
   GetTextMetrics( hDC, &tm );
   GetTextFace( hDC, sizeof( sFont ), sFont );
   ReleaseDC( hWnd, hDC );

   if( sDefaultFont == NULL )
   {
      hFont = CreateFont( (int) -floor( (float) tm.tmHeight * StretchInfo.fYFactor ), // height of font
                           0,                   // average character width
                           0,                   // angle of escapement
                           0,                   // base-line orientation angle
                           tm.tmWeight,         // font weight
                           tm.tmItalic,         // italic attribute option
                           tm.tmUnderlined,     // underline attribute option
                           tm.tmStruckOut,      // strikeout attribute option
                           tm.tmCharSet,        // character set identifier
                           OUT_DEFAULT_PRECIS,  // output precision
                           CLIP_DEFAULT_PRECIS, // clipping precision
                           DEFAULT_QUALITY,     // output quality
                           tm.tmPitchAndFamily, // pitch and family
                           sFont                // typeface name
                        );
   }

   if( hFont == NULL && sDefaultFont )
   {
      hFont = CreateFont( (int) -floor( (float) tm.tmHeight * StretchInfo.fYFactor ), // height of font
                           0,                   // average character width
                           0,                   // angle of escapement
                           0,                   // base-line orientation angle
                           tm.tmWeight,         // font weight
                           tm.tmItalic,         // italic attribute option
                           tm.tmUnderlined,     // underline attribute option
                           tm.tmStruckOut,      // strikeout attribute option
                           tm.tmCharSet,        // character set identifier
                           OUT_DEFAULT_PRECIS,  // output precision
                           CLIP_DEFAULT_PRECIS, // clipping precision
                           DEFAULT_QUALITY,     // output quality
                           FF_DONTCARE,         // pitch and family
                           sDefaultFont         // typeface name
                        );
   }

   SendMessage( hWnd, WM_SETFONT, (WPARAM) hFont, (LPARAM) 0 );

   if( iLeft == -1 || iTop == -1 )
   {
      TopLeft.x = rct.left;
      TopLeft.y = rct.top;

      ScreenToClient( GetParent( hWnd ), &TopLeft );

      if( iLeft == -1 )
      {
         iLeft = TopLeft.x;
      }

      if( iTop == -1 )
      {
         iTop = TopLeft.y;
      }
   }

   //sprintf( sMessage, "Sizing, from: %i, %i, to: %i, %i Factor: %f, %f\n", iOldWidth, iOldHeight, iNewWidth, iNewHeight, StretchInfo.fXFactor, StretchInfo.fYFactor );
   //OutputDebugString( sMessage );

   MoveWindow( hWnd, iLeft, iTop, iNewWidth, iNewHeight, bPaint );

   StretchInfo.bPaint = bPaint;
   StretchInfo.sDefaultFont = sDefaultFont;

   if( bMonoFont )
   {
      StretchInfo.hFont = hFont;
   }
   else
   {
      StretchInfo.hFont = NULL;
   }

   EnumChildWindows( hWnd, EnumChildProc, (LPARAM) &StretchInfo );

   return hFont;
}

BOOL CALLBACK EnumChildProc( HWND hChild, LPARAM lParam )
{
   //char sMessage[ 256 ];
   PSTRETCHINFO pStretchInfo = (PSTRETCHINFO) lParam;
   char sFont[ 64 ];
   HDC hDC = GetDC( hChild );
   TEXTMETRIC tm;
   HFONT hFont;
   RECT rct;
   POINT TopLeft;

   SelectObject( hDC, (HFONT) SendMessage( hChild, WM_GETFONT, 0, 0 ) );
   GetTextMetrics( hDC, &tm );
   GetTextFace( hDC, sizeof( sFont ), sFont );
   ReleaseDC( hChild, hDC );

   hFont = pStretchInfo->hFont;

   if( hFont == NULL && pStretchInfo->sDefaultFont == NULL )
   {
      hFont = CreateFont( (int) -floor( (float) tm.tmHeight * pStretchInfo->fYFactor ), // height of font
                           0,                   // average character width
                           0,                   // angle of escapement
                           0,                   // base-line orientation angle
                           tm.tmWeight,         // font weight
                           tm.tmItalic,         // italic attribute option
                           tm.tmUnderlined,     // underline attribute option
                           tm.tmStruckOut,      // strikeout attribute option
                           tm.tmCharSet,        // character set identifier
                           OUT_DEFAULT_PRECIS,  // output precision
                           CLIP_DEFAULT_PRECIS, // clipping precision
                           DEFAULT_QUALITY,     // output quality
                           tm.tmPitchAndFamily, // pitch and family
                           sFont                // typeface name
                        );

      //sprintf( sMessage, "Font: %p\n", hFont );
      //OutputDebugString( sMessage );
   }

   if( hFont == NULL && pStretchInfo->sDefaultFont )
   {
      hFont = CreateFont( (int) -floor( (float) tm.tmHeight * pStretchInfo->fYFactor ), // height of font
                           0,                   // average character width
                           0,                   // angle of escapement
                           0,                   // base-line orientation angle
                           tm.tmWeight,         // font weight
                           tm.tmItalic,         // italic attribute option
                           tm.tmUnderlined,     // underline attribute option
                           tm.tmStruckOut,      // strikeout attribute option
                           tm.tmCharSet,        // character set identifier
                           OUT_DEFAULT_PRECIS,  // output precision
                           CLIP_DEFAULT_PRECIS, // clipping precision
                           DEFAULT_QUALITY,     // output quality
                           FF_DONTCARE,         // pitch and family
                           pStretchInfo->sDefaultFont // typeface name
                        );

      //sprintf( sMessage, "*** Font: %p\n", hFont );
      //OutputDebugString( sMessage );
   }

   SendMessage( hChild, WM_SETFONT, (WPARAM) hFont, (LPARAM) 0 );

   GetWindowRect( hChild, &rct );

   TopLeft.x = rct.left;
   TopLeft.y = rct.top;

   ScreenToClient( GetParent( hChild ), &TopLeft );

   //sprintf( sMessage, "Old Pos: %i, %i New Pos: %i, %i\n", TopLeft.x, TopLeft.y, (int) floor( (float) TopLeft.x * pStretchInfo->fXFactor ), (int) floor( (float) TopLeft.y * pStretchInfo->fYFactor ) );
   //OutputDebugString( sMessage );

    // Size and position the child window.
   MoveWindow( hChild,
               (int) floor( (float) TopLeft.x * pStretchInfo->fXFactor ),
               (int) floor( (float) TopLeft.y * pStretchInfo->fYFactor ),
               (int) floor( (float) ( rct.right - rct.left + 1 ) * pStretchInfo->fXFactor ),
               (int) floor( (float) ( rct.bottom - rct.top + 1 ) * pStretchInfo->fYFactor ),
               pStretchInfo->bPaint );

    return TRUE;
}

HRGN BitmapToRegion( HBITMAP hBmp, COLORREF cTransparentColor )
{
   HRGN hRgn = NULL;
   DWORD* pBitmapBits;
   DWORD* pBitmapCursor;
   BITMAP bitmap;
   RGNDATA* pRGNData = NULL;
   int iLastRectIDX;
   int iRGNDataSize_Rects;
   int iRowIDX, iColIDX;
   DWORD dwTransMasked;
   BOOL bDetectedTransparentPixel;

   // Get the size of the source
   GetObject( hBmp, sizeof( bitmap ), &bitmap );
   pBitmapBits = (DWORD*) malloc( sizeof( DWORD ) * bitmap.bmWidth * bitmap.bmHeight );

   // Extract the bits of the bitmap
   {
      BITMAPINFO bmi;
      HDC dc;

      memset(&bmi, 0, sizeof(bmi));
      bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
      bmi.bmiHeader.biPlanes = 1;
      bmi.bmiHeader.biBitCount = 32;
      bmi.bmiHeader.biCompression = BI_RGB;
      bmi.bmiHeader.biWidth = bitmap.bmWidth;
      bmi.bmiHeader.biHeight = -bitmap.bmHeight;

      dc = CreateCompatibleDC( NULL );
      GetDIBits(dc, hBmp, 0, bitmap.bmHeight, pBitmapBits, &bmi, DIB_RGB_COLORS);
      DeleteDC(dc);
   }

   // Step through bitmap row by row - building rects for the rows that arn't the transparent color.
   dwTransMasked = cTransparentColor & 0x00FFFFFF;
   pBitmapCursor = pBitmapBits;
   iLastRectIDX = 0;
   iRGNDataSize_Rects = 0;
   bDetectedTransparentPixel = FALSE;

   for( iRowIDX = 0; iRowIDX < bitmap.bmHeight; iRowIDX++ )
   {
      BOOL bInStrip = FALSE;

      for( iColIDX = 0; iColIDX < bitmap.bmWidth; iColIDX++, pBitmapCursor++ )
      {
         // Is the current pixel transparent?
         if( (((*pBitmapCursor)&0x00FFFFFF)^dwTransMasked) == 0L)
         {
            bDetectedTransparentPixel = TRUE;

            // If we are in a strip - close it
            if( bInStrip )
            {
               bInStrip = FALSE;
               ((RECT*)pRGNData->Buffer)[iLastRectIDX].right = iColIDX;
               iLastRectIDX++;
            }
         }
         else
         {
            // Open a new strip if we need to
            if( ! bInStrip )
            {
               bInStrip = TRUE;

               // Ensure that we have enough memory allocated
               if( iLastRectIDX == iRGNDataSize_Rects )
               {
                  iRGNDataSize_Rects += 4096;
                  pRGNData = (RGNDATA*) realloc( (void *) pRGNData, sizeof( RGNDATAHEADER ) + ( iRGNDataSize_Rects * sizeof( RECT ) ) );
               }

               ( (RECT*) pRGNData->Buffer)[iLastRectIDX].left   = iColIDX;
               ( (RECT*) pRGNData->Buffer)[iLastRectIDX].top    = iRowIDX;
               ( (RECT*) pRGNData->Buffer)[iLastRectIDX].bottom = iRowIDX+1;
            }
         }
      }

      // Close any open rects
      if( bInStrip )
      {
          ((RECT*)pRGNData->Buffer)[iLastRectIDX].right = bitmap.bmWidth;
          iLastRectIDX++;
      }
   }

   free( pBitmapBits );

   // If there are some rects in this region - create the GDI object
   if( bDetectedTransparentPixel )
   {
      pRGNData->rdh.dwSize = sizeof(RGNDATAHEADER);
      pRGNData->rdh.iType = RDH_RECTANGLES;
      pRGNData->rdh.nCount = iLastRectIDX;
      pRGNData->rdh.nRgnSize = sizeof(RGNDATAHEADER) + (iLastRectIDX * sizeof(RECT));
      pRGNData->rdh.rcBound.left = 0;
      pRGNData->rdh.rcBound.top = 0;
      pRGNData->rdh.rcBound.right = bitmap.bmWidth;
      pRGNData->rdh.rcBound.bottom = bitmap.bmHeight;
      hRgn = ExtCreateRegion(NULL, pRGNData->rdh.nRgnSize, pRGNData);
   }

   // Cleanup
   if( pRGNData )
   {
      free(pRGNData);
   }

   return hRgn;
}

int PaintBitmap( HWND hWnd, HBITMAP SrcBmp, int LeftDestination, int TopDestination, int Width, int Height, int LeftSource, int TopSource )
{
   if( Width && Height )
   {
      HBITMAP hBitmap = (HBITMAP) SelectObject( s_CompatibleDC, SrcBmp );
      int retval = BitBlt( s_MainDC, LeftDestination, TopDestination, Width, Height, s_CompatibleDC, LeftSource, TopSource, SRCCOPY );

      SelectObject( s_CompatibleDC, hBitmap );

      return retval;
   }

   return FALSE;
}

int PaintBitmapDC( HWND hWnd, HDC hDC, HBITMAP SrcBmp, int LeftDestination, int TopDestination, int Width, int Height, int LeftSource, int TopSource )
{
   if( Width && Height )
   {
      HDC hCompatibleDC = CreateCompatibleDC( hDC );

      HBITMAP hBitmap = (HBITMAP) SelectObject( hCompatibleDC, SrcBmp );
      int retval = BitBlt( hDC, LeftDestination, TopDestination, Width, Height, hCompatibleDC, LeftSource, TopSource, SRCCOPY );

      SelectObject( hCompatibleDC, hBitmap );

      return retval;
   }

   return FALSE;
}

HB_FUNC( MESSAGEBOX )
{
   hb_retni( MessageBox( ( HWND ) hb_parnl( 1 ), hb_parcx( 2 ), hb_parcx( 3 ), hb_parni( 4 ) ) );
}
