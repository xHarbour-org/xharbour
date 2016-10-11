/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// FolderTree.prg                                                                                           *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#include "vxh.ch"

#define SFGAO_CANRENAME       0x00000010L
#define SFGAO_CANDELETE       0x00000020L
#define SFGAO_HASPROPSHEET    0x00000040L
#define SFGAO_DROPTARGET      0x00000100L
#define SFGAO_CAPABILITYMASK  0x00000177L
#define SFGAO_LINK            0x00010000L
#define SFGAO_SHARE           0x00020000L
#define SFGAO_READONLY        0x00040000L
#define SFGAO_GHOSTED         0x00080000L
#define SFGAO_HIDDEN          0x00080000L
#define SFGAO_DISPLAYATTRMASK 0x000F0000L
#define SFGAO_FILESYSANCESTOR 0x10000000L
#define SFGAO_FOLDER          0x20000000L
#define SFGAO_FILESYSTEM      0x40000000L
#define SFGAO_HASSUBFOLDER    0x80000000L
#define SFGAO_CONTENTSMASK    0x80000000L
#define SFGAO_VALIDATE        0x01000000L
#define SFGAO_REMOVABLE       0x02000000L
#define SFGAO_COMPRESSED      0x04000000L
#define SFGAO_BROWSABLE       0x08000000L
#define SFGAO_NONENUMERATED   0x00100000L
#define SFGAO_NEWCONTENT      0x00200000L
#define SFGAO_CANMONIKER      0x00400000L

#define SHCNE_RENAMEITEM      0x00000001L
#define SHCNE_CREATE          0x00000002L
#define SHCNE_DELETE          0x00000004L
#define SHCNE_MKDIR           0x00000008L
#define SHCNE_RMDIR           0x00000010L
#define SHCNE_MEDIAINSERTED   0x00000020L
#define SHCNE_MEDIAREMOVED    0x00000040L
#define SHCNE_DRIVEREMOVED    0x00000080L
#define SHCNE_DRIVEADD        0x00000100L
#define SHCNE_NETSHARE        0x00000200L
#define SHCNE_NETUNSHARE      0x00000400L
#define SHCNE_ATTRIBUTES      0x00000800L
#define SHCNE_UPDATEDIR       0x00001000L
#define SHCNE_UPDATEITEM      0x00002000L
#define SHCNE_SERVERDISCONNECT 0x00004000L
#define SHCNE_UPDATEIMAGE     0x00008000L
#define SHCNE_DRIVEADDGUI     0x00010000L
#define SHCNE_RENAMEFOLDER    0x00020000L
#define SHCNE_FREESPACE       0x00040000L
#define SHCNE_EXTENDED_EVENT  0x04000000L
#define SHCNE_ASSOCCHANGED    0x08000000L
#define SHCNE_DISKEVENTS      0x0002381FL
#define SHCNE_GLOBALEVENTS    0x0C0581E0L
#define SHCNE_ALLEVENTS       0x7FFFFFFFL
#define SHCNE_INTERRUPT       0x80000000L

//-----------------------------------------------------------------------------------------------

CLASS FolderTree INHERIT TreeView
   DATA RootFolder, InitialPath

   PROPERTY AllowShellMenu                      DEFAULT .T.
   PROPERTY SysFolder      SET ::__SetFolder(v) DEFAULT __GetSystem():Folders:Desktop
   PROPERTY Folder         SET ::__SetFolder(v)
   PROPERTY Explorer       ROOT "Appearance" SET ::__SetExpTheme(v) DEFAULT .T.

   PROPERTY FolderView     ROOT "Behavior" GET __ChkComponent( Self, @::xFolderView )

   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD OnParentNotify()
   METHOD Update()
   METHOD GetPath()                 INLINE FolderTreeGetPath( ::hWnd )
   METHOD GetSysFolder()
   METHOD __SetExpTheme()
   METHOD __SetFolder( cPath ) INLINE IIF( ::hWnd != NIL, ( ::ResetContent(), IIF( cPath == ::SysFolder, cPath := NIL, ), FolderTreeInit( ::hWnd, ::SysFolder, cPath ) ), )
ENDCLASS

METHOD GetSysFolder() CLASS FolderTree
   LOCAL tvi, cFolder
   tvi := (struct TVITEM)
   tvi:hItem := SendMessage( ::hWnd, TVM_GETNEXTITEM, TVGN_CARET, 0 )
   IF !EMPTY( tvi:hItem )

      tvi:mask       := TVIF_TEXT
      tvi:pszText    := Space( MAX_PATH + 1 )
      tvi:cchTextMax := MAX_PATH

      SendMessage( ::hWnd, TVM_GETITEM, 0, @tvi )

      cFolder := Left( tvi:pszText, At( Chr(0), tvi:pszText ) -1 )
      cFolder := StrTran( cFolder, " " )
   ENDIF
RETURN cFolder

METHOD Init( oParent ) CLASS FolderTree
   ::__xCtrlName := "FolderTree"
   ::Super:Init( oParent )
RETURN Self

METHOD Create() CLASS FolderTree
   Super:Create()
   ::__SetExpTheme()
   RedrawWindow( ::hWnd, , , (RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW) )
   ::SetWindowPos(,0,0,0,0, (SWP_FRAMECHANGED|SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER) )

   FolderTreeInit( ::hWnd, ::SysFolder, IIF( EMPTY( ::Folder ), NIL, ::Folder ) )
RETURN Self

METHOD __SetExpTheme( lSet ) CLASS FolderTree
   DEFAULT lSet TO ::xExplorer
   ::HasLines := ! lSet
   IF lSet
      ::HasButtons := .T.
   ENDIF
   SetWindowTheme( ::hWnd, IIF( lSet, "explorer", "treeview" ) )
RETURN Self

METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS FolderTree
   Super:OnParentNotify( nwParam, nlParam, hdr )
   IF hdr:hwndFrom == ::hWnd

      IF hdr:code == TVN_ITEMEXPANDING
         FolderTreePopulate( ::hWnd, nlParam )

       ELSEIF hdr:code == TVN_SELCHANGED
         IF ::FolderView != NIL
            ::FolderView:PopulateFromTree( nlParam )
         ENDIF

       ELSEIF hdr:code == NM_RCLICK .AND. ::AllowShellMenu
         FolderTreeShowStdMenu( ::hWnd, nlParam, .T. )
      ENDIF
   ENDIF
RETURN NIL

METHOD Update() CLASS FolderTree
   LOCAL oItem, hItem
   hItem := ::GetRoot()
   WHILE hItem != 0
      oItem := FindTreeItem( ::Items, hItem )
      IF oItem != NIL
         //nState := ::GetItemState( hItem, TVIF_STATE )
      ENDIF
      hItem := ::GetNextItem( hItem )
   ENDDO
RETURN Self


//----------------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------------------

CLASS FolderList INHERIT ListView
   DATA RootFolder, InitialPath

   PROPERTY AllowNavigation                      DEFAULT .T.
   PROPERTY ShowFolders                          DEFAULT .T.
   PROPERTY AllowShellMenu                       DEFAULT .T.
   PROPERTY SysFolder   SET ::__SetFolder(v,.t.) DEFAULT __GetSystem():Folders:Desktop
   PROPERTY Folder      SET ::__SetFolder(v)

   DATA SelFile         EXPORTED  INIT ""
   DATA SelFolder       EXPORTED  INIT ""

   DATA __aColumns      PROTECTED
   DATA __nCurFolderID  PROTECTED
   DATA __LastRegEntry  PROTECTED

   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD OnParentNotify()
   METHOD Populate()
   METHOD __SetFolder()
   METHOD OnUserMsg()
   METHOD OnGetDlgCode()     INLINE (DLGC_WANTMESSAGE | DLGC_WANTALLKEYS)
   METHOD PopulateByID()
   METHOD PopulateFromTree()
ENDCLASS

METHOD Init( oParent ) CLASS FolderList
   ::__xCtrlName := "FolderList"
   ::Super:Init( oParent )
   IF ::DesignMode
      AINS( ::Events, 1, {"Navigation", {;
                                          { "OnReturn",  "", "" } } }, .T. )
   ENDIF
RETURN Self

METHOD Create() CLASS FolderList
   ::Style := (::Style | LVS_SHAREIMAGELISTS)
   Super:Create()
   RedrawWindow( ::hWnd, , , (RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW) )
   ::SetWindowPos(,0,0,0,0, (SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER) )
   ::__aColumns := {}
   ListViewBrowseInit( ::hWnd, ::SysFolder, IIF( EMPTY( ::Folder ), NIL, ::Folder ), ::ShowFolders )
   IF !EMPTY( ::Folder )
      ::__SetFolder( ::Folder )
    ELSE
      ::__SetFolder( ::SysFolder )
   ENDIF
RETURN Self

METHOD OnUserMsg( hWnd, nMsg, nwParam, nlParam ) CLASS FolderList
   (hWnd, nwParam, nlParam)
   IF nMsg == WM_USER + 15 .AND. ::__nCurFolderID != NIL
      FolderListPopulateByID( ::hWnd, ::__nCurFolderID  )
      RETURN 0
   ENDIF
RETURN NIL


METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS FolderList
   LOCAL nSubItem

   IF hdr:code == LVN_GETDISPINFO
      ListBrowseSetDisplayInfo( nlParam )

    ELSEIF hdr:code == NM_RCLICK .AND. ::AllowShellMenu
      ListViewShowStdMenu( ::hWnd, nlParam )

    ELSEIF hdr:code == NM_DBLCLK
      ::Populate( nlParam )

    ELSEIF hdr:code == NM_RETURN
      ExecuteEvent( "OnReturn", Self )

    ELSEIF hdr:code == LVN_COLUMNCLICK
      nSubItem := __ListViewGetSubItem( nlParam )

      ASIZE( ::__aColumns, nSubItem+1 )
      IF ::__aColumns[ nSubItem+1 ] == NIL
         ::__aColumns[ nSubItem+1 ] := .T.
       ELSE
         ::__aColumns[ nSubItem+1 ] := !::__aColumns[ nSubItem+1 ]
      ENDIF
      ::SortItems( nSubItem, ::__aColumns[ nSubItem+1 ] )
      RETURN 0
    ELSEIF hdr:code == LVN_ITEMCHANGED
      IF ( __ListViewGetNewState( nlParam ) & LVIS_FOCUSED ) == LVIS_FOCUSED
      ENDIF
    ELSE
      Super:OnParentNotify( nwParam, nlParam )
   ENDIF
RETURN 0

METHOD __SetFolder( cFolder ) CLASS FolderList
   LOCAL nSpecialFolder
   IF VALTYPE( cFolder ) == "N"
      nSpecialFolder := cFolder
      cFolder := NIL
   ENDIF
   ::PopulateByID( FolderIDFromFolderName( cFolder, nSpecialFolder ) )
RETURN Self

METHOD PopulateByID( nID ) CLASS FolderList
   LOCAL lRoot, cFolder
   IF nID != NIL
      cFolder := FolderFromIDList( nID )
      lRoot := ! Empty( cFolder ) .AND. Upper( Right( cFolder, 7 ) ) == "DESKTOP"

      IF ::__LastRegEntry != NIL
         UnregisterNotify( ::__LastRegEntry )
      ENDIF
      FolderListPopulateByID( ::hWnd, nID, lRoot )
      ::__nCurFolderID := nID
      ::__LastRegEntry := RegisterNotifyFolderID( ::hWnd, nID, WM_USER + 15 )
   ENDIF
RETURN Self

METHOD Populate( nlParam ) CLASS FolderList
//   ::PopulateByID( FolderTreeIDList( nlParam ) )
   LOCAL nID
   IF ::__LastRegEntry != NIL
      UnregisterNotify( ::__LastRegEntry )
   ENDIF
   nID := FolderBrowsePopulate( ::hWnd, nlParam )
   ::__nCurFolderID := nID
   ::__LastRegEntry := RegisterNotifyFolderID( ::hWnd, nID, WM_USER + 15 )
RETURN Self

METHOD PopulateFromTree( nlParam ) CLASS FolderList
   LOCAL nID
   IF ::__LastRegEntry != NIL
      UnregisterNotify( ::__LastRegEntry )
   ENDIF
   nID := ListViewBrowsePopulateFromTree( ::hWnd, nlParam )
   ::__nCurFolderID := nID
   ::__LastRegEntry := RegisterNotifyFolderID( ::hWnd, nID, WM_USER + 15 )
RETURN Self
