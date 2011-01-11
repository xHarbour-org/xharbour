#include "hbapi.h"

#define DUMMYUNIONNAME

#include <windows.h>
#include <shlobj.h>
#include <winuser.h>

BOOL GetName (LPSHELLFOLDER lpsf, LPITEMIDLIST lpi, DWORD dwFlags, LPSTR lpFriendlyName);
LPITEMIDLIST CopyItemID(LPMALLOC g_pMalloc, LPITEMIDLIST pidl);
LPITEMIDLIST Concatenate(LPMALLOC lpMalloc, LPCITEMIDLIST pidl1, LPCITEMIDLIST pidl2);
void GetNormalAndSelectedIcons(LPITEMIDLIST lpifq, LPTV_ITEM lptvitem);

#define IDR_VIEW_POPUP                  203

typedef struct {
   BOOL bRoot;
   LPSHELLFOLDER lpsfParent;
   LPITEMIDLIST  lpi;
   LPITEMIDLIST  lpifq;
   ULONG         ulAttribs;
   LPSHELLFOLDER pRoot;
}LPTVITEMDATA;


//-------------------------------------------------------------------------------------------------------------------------------------------------------
UINT FolderTreeDeleteChildren(HTREEITEM hItem, HWND hTreeView)
{
   UINT nCount = 0;
   HTREEITEM hChild = TreeView_GetChild(hTreeView, hItem);
   while (hChild != NULL)
   {
        HTREEITEM hNextItem = TreeView_GetNextSibling(hTreeView, hChild);
        TreeView_DeleteItem (hTreeView, hChild);
        hChild = hNextItem;
        nCount++;
   }
   return nCount;
}

//-------------------------------------------------------------------------------------------------------------------------------------------------------
BOOL FolderTreeInsertItem(BOOL bRoot, TVINSERTSTRUCT* tvins, char szBuff[MAX_PATH], HTREEITEM hParent, HTREEITEM hPrev, LPSHELLFOLDER lpsf, LPITEMIDLIST lpifq, LPITEMIDLIST lpi, BOOL bChildValid, LPSHELLFOLDER pRoot )
{
   TV_ITEM tvi; // tree view item
   HRESULT hr;
   ULONG ulAttrs;
   LPTVITEMDATA* lptvid;
   LPMALLOC pMalloc;

   CoInitialize( NULL );
   hr = SHGetMalloc(&pMalloc);

   if(FAILED(hr))
      return FALSE;

   ulAttrs = SFGAO_HASSUBFOLDER | SFGAO_FOLDER | SFGAO_FILESYSTEM | SFGAO_GHOSTED | SFGAO_LINK | SFGAO_SHARE;
   lpsf->lpVtbl->GetAttributesOf(lpsf, 1, (const struct _ITEMIDLIST **)&lpi, &ulAttrs);

   tvi.mask = TVIF_TEXT | TVIF_IMAGE | TVIF_SELECTEDIMAGE | TVIF_PARAM;

   if (ulAttrs & SFGAO_HASSUBFOLDER)
   {
      if(bChildValid)
      {
         tvi.cChildren = 1;
         tvi.mask |= TVIF_CHILDREN;
      }
   }
   
   if (ulAttrs & SFGAO_GHOSTED)
   {
      tvi.mask |= LVIF_STATE;
      tvi.stateMask = LVIS_CUT;
      tvi.state = LVIS_CUT;
   }
   
   if (ulAttrs & SFGAO_LINK)
   {
      tvi.mask |= LVIF_STATE;
      tvi.stateMask = LVIS_OVERLAYMASK;
      tvi.state = INDEXTOOVERLAYMASK(2);
   }
   
   if (ulAttrs & SFGAO_SHARE)
   {
      tvi.mask |= LVIF_STATE;
      tvi.stateMask = LVIS_OVERLAYMASK;
      tvi.state = INDEXTOOVERLAYMASK(1);
   }

   lptvid = NULL;
   lptvid = (LPTVITEMDATA*) pMalloc->lpVtbl->Alloc (pMalloc, sizeof (LPTVITEMDATA));

   if (! lptvid)
   {
      pMalloc->lpVtbl->Release(pMalloc);
      CoUninitialize();
      return FALSE;
   }

   GetName(lpsf, lpi, SHGDN_NORMAL, szBuff);

   tvi.pszText = szBuff;
   tvi.cchTextMax = MAX_PATH;

   lptvid->lpi = CopyItemID (pMalloc, lpi);

   if( pRoot != NULL )
   {
      lptvid->lpsfParent = pRoot;
   }
   else
   {
      lptvid->lpsfParent = lpsf;
   }

   lpsf->lpVtbl->AddRef(lpsf);

   lptvid->bRoot = bRoot;
   lptvid->lpifq = Concatenate(pMalloc, lpifq, lpi);
   GetNormalAndSelectedIcons(lptvid->lpifq, &tvi);
   tvi.lParam = (LPARAM)lptvid;

   tvins->item = tvi;
   tvins->hInsertAfter = hPrev;
   tvins->hParent = hParent;

   pMalloc->lpVtbl->Release(pMalloc);
   CoUninitialize();
   return TRUE;
}

//-------------------------------------------------------------------------------------------------------------------------------------------------------
HTREEITEM FolderTreeInsertRoot(LPSHELLFOLDER lpsf, HWND hTreeView, int iFolder, const char *cInitialPath )
{
   LPITEMIDLIST lpi = NULL;
   HTREEITEM hParent = NULL;
   char szBuff [MAX_PATH];
   TVINSERTSTRUCT tvins;
   HRESULT hr;
   LPSHELLFOLDER pRoot = NULL;

   if( cInitialPath != NULL )
   {
      OLECHAR olePath[MAX_PATH];
      ULONG chEaten;
      ULONG dwAttributes;
      MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, cInitialPath, -1, olePath, MAX_PATH);
      hr = lpsf->lpVtbl->ParseDisplayName(lpsf,NULL,NULL,olePath,&chEaten,&lpi,&dwAttributes);
      hr = lpsf->lpVtbl->BindToObject( lpsf, lpi, NULL, &IID_IShellFolder, (LPVOID *) &pRoot);
   }
   else
   {
      SHGetSpecialFolderLocation(NULL, iFolder, &lpi);
   }

   if( pRoot == NULL && iFolder != CSIDL_DESKTOP )
   {
      hr = lpsf->lpVtbl->BindToObject( lpsf, lpi, NULL, &IID_IShellFolder, (LPVOID *) &pRoot);
   }

   FolderTreeInsertItem( TRUE, &tvins, szBuff, NULL, NULL , lpsf, NULL, lpi , TRUE, pRoot);
   hParent = TreeView_InsertItem(hTreeView, &tvins);
   TreeView_Expand(hTreeView, hParent, TVE_EXPAND);

   if( pRoot != NULL )
   {
      pRoot->lpVtbl->Release(pRoot);
   }
   return hParent;
}


//-------------------------------------------------------------------------------------------------------------------------------------------------------
HB_FUNC( FOLDERTREEINIT )
{
   HWND          hTreeView = (HWND) hb_parnl(1);
   LPSHELLFOLDER lpsf = NULL;
   HRESULT       hr;
   HTREEITEM     hParent = NULL;
   SHFILEINFO    sfi;
   HIMAGELIST    himlSmall;
   HIMAGELIST    himlLarge;

   himlSmall = (HIMAGELIST)SHGetFileInfo( TEXT("C:\\"), 0, &sfi, sizeof(SHFILEINFO), SHGFI_SYSICONINDEX | SHGFI_SMALLICON);
   himlLarge = (HIMAGELIST)SHGetFileInfo( TEXT("C:\\"), 0, &sfi, sizeof(SHFILEINFO), SHGFI_SYSICONINDEX | SHGFI_LARGEICON);
   if (himlSmall && himlLarge)
   {
      SendMessage( hTreeView, TVM_SETIMAGELIST, (WPARAM)LVSIL_NORMAL, (LPARAM)himlSmall);
      SendMessage( hTreeView, TVM_SETIMAGELIST, (WPARAM)LVSIL_STATE, (LPARAM)himlSmall);
   }

   hr = SHGetDesktopFolder (&lpsf);
   if (SUCCEEDED (hr))
   {
      hParent = FolderTreeInsertRoot(lpsf, hTreeView, hb_parni(2), hb_parc(3) );
      lpsf->lpVtbl->Release (lpsf);
   }
}

//-------------------------------------------------------------------------------------------------------------------------------------------------------
void FolderTreePopulate( HWND hTreeView, TVITEM *tvi, UINT action )
{
   LPTVITEMDATA* lptvid = NULL;
   HTREEITEM hItem = NULL;
   HTREEITEM hPrev;
   HRESULT hr;
   ULONG celtFetched;
   LPITEMIDLIST pidlItems = NULL;
   LPENUMIDLIST ppenum = NULL;
   IShellFolder *psfProgFiles = NULL;
   LPMALLOC pMalloc;

   hr = SHGetMalloc(&pMalloc);

   if(FAILED(hr))
      return;

   lptvid = (LPTVITEMDATA*) pMalloc->lpVtbl->Alloc (pMalloc, sizeof (LPTVITEMDATA));
   if (! lptvid)
   {
      return;
   }

   lptvid = (LPTVITEMDATA*)tvi->lParam;
   if(lptvid == NULL)
      return;

   if(lptvid->bRoot)
   {
      psfProgFiles = lptvid->lpsfParent;
   }
   else
   {
      hr = lptvid->lpsfParent->lpVtbl->BindToObject(lptvid->lpsfParent , lptvid->lpi, NULL, &IID_IShellFolder, (LPVOID *) &psfProgFiles);
      if(FAILED(hr))
         return;
   }

   hr = psfProgFiles->lpVtbl->EnumObjects(psfProgFiles, NULL,SHCONTF_FOLDERS | SHCONTF_NONFOLDERS | SHCONTF_INCLUDEHIDDEN, &ppenum);
   if(FAILED(hr))
      return;

   SetCursor(LoadCursor(NULL,IDC_WAIT));
   SendMessage(hTreeView, WM_SETREDRAW, FALSE, 0L);
//   if( action == TVE_COLLAPSE )
      FolderTreeDeleteChildren(tvi->hItem, hTreeView);

   while( ( hr = ppenum->lpVtbl->Next(ppenum, 1,&pidlItems, &celtFetched) ) == S_OK && celtFetched == 1 )
   {
      char szBuff[MAX_PATH];
      TVINSERTSTRUCT tvins;
      ULONG uAttr = SFGAO_FOLDER | SFGAO_GHOSTED | SFGAO_READONLY;
      psfProgFiles->lpVtbl->GetAttributesOf(psfProgFiles, 1, (LPCITEMIDLIST *) &pidlItems, &uAttr);

      if(uAttr & SFGAO_FOLDER )
      {
         if( FolderTreeInsertItem(FALSE, &tvins, szBuff, tvi->hItem, NULL , psfProgFiles , lptvid->lpifq, pidlItems , TRUE, NULL) )
         {
            hPrev = TreeView_InsertItem(hTreeView, &tvins);
         }
      }
   }

   pMalloc->lpVtbl->Release(pMalloc);
   SetCursor(LoadCursor(NULL,IDC_ARROW));
   SendMessage(hTreeView, WM_SETREDRAW, TRUE, 0L);
   InvalidateRect(hTreeView, NULL, TRUE);
}

//-------------------------------------------------------------------------------------------------------------------------------------------------------
void FolderTreeUpdate( HWND hTreeView, HTREEITEM hItem )
{
   TVITEM tvItem;
   HTREEITEM hChild;
   char szBuff[MAX_PATH];

   tvItem.mask = TVIF_STATE | TVIF_TEXT | TVIF_PARAM;
   tvItem.hItem = hItem;
   tvItem.pszText = szBuff;
   tvItem.cchTextMax = MAX_PATH;

   TreeView_GetItem( hTreeView, &tvItem );

   hChild = TreeView_GetChild( hTreeView, hItem);
   while (hChild != NULL)
   {
      FolderTreeUpdate(hTreeView, hChild);
      hChild = (HTREEITEM) SendMessage( hTreeView, TVM_GETNEXTITEM, TVGN_NEXT, (LPARAM) hChild );
   }
}

HB_FUNC( FOLDERTREEUPDATE )
{
   FolderTreeUpdate( (HWND) hb_parnl(1), (HTREEITEM) (HWND) hb_parnl(2) );
}

//-------------------------------------------------------------------------------------------------------------------------------------------------------
HB_FUNC( FOLDERTREEPOPULATETREE )
{
   LPARAM lParam = (LPARAM) hb_parnl(2);
   TVITEM tvi = ((NM_TREEVIEW*)lParam)->itemNew;
   FolderTreePopulate( (HWND) hb_parnl(1), &tvi, ((NM_TREEVIEW*)lParam)->action );
}

//-------------------------------------------------------------------------------------------------------------------------------------------------------
HB_FUNC( FOLDERTREESHOWSTDMENU )
{
   HWND hWnd = (HWND) hb_parnl(1);
   LPARAM lParam = (LPARAM) hb_parnl(2);
   BOOL bShowMenu = hb_parl(3);

   TVITEM tvi;
   
   HRESULT hr;
   HMENU hMenuPopup;
   IContextMenu *icm;
   LPMALLOC m_pMalloc;
   LPTVITEMDATA* lptvid = NULL;
   UINT  id;
   static POINT pt;
   CMINVOKECOMMANDINFO  ici;
   
   ZeroMemory(&tvi, sizeof(tvi));
   tvi.mask = TVIF_PARAM | TVIF_HANDLE;
   tvi.hItem = TreeView_GetSelection( hWnd );
   TreeView_GetItem( hWnd, &tvi );

   if( ! tvi.lParam )
   {
      return;
   }
   hr = SHGetMalloc(&m_pMalloc);

   if(FAILED(hr))
   {
      goto Done;
   }

   lptvid = (LPTVITEMDATA*) m_pMalloc->lpVtbl->Alloc (m_pMalloc, sizeof (LPTVITEMDATA));
   if (! lptvid)
   {
      goto Done;
   }

   lptvid = (LPTVITEMDATA*)tvi.lParam;
   if(lptvid == NULL)
   {
      goto Done;
   }
   hr = lptvid->lpsfParent->lpVtbl->GetUIObjectOf( lptvid->lpsfParent, hWnd, 1, (LPCITEMIDLIST*)&lptvid->lpi, &IID_IContextMenu, NULL, (LPVOID*)&icm);
   hMenuPopup = CreatePopupMenu();
   if(!hMenuPopup || FAILED(hr))
   {
      goto Done;
   }

   hr = icm->lpVtbl->QueryContextMenu(icm , hMenuPopup, 0, 1, 0x7fff, CMF_NORMAL | CMF_EXPLORE);
   if(FAILED(hr))
   {
      goto Done;
   }
   GetCursorPos(&pt);
   if(bShowMenu)
   {
      id = TrackPopupMenu(hMenuPopup, TPM_LEFTALIGN | TPM_RETURNCMD | TPM_RIGHTBUTTON, pt.x, pt.y, 0, hWnd,NULL);
   }
   else
   {
      id = hr;
   }
   if(!id)
   {
      goto Done;
   }
   ici.cbSize = sizeof(CMINVOKECOMMANDINFO);
   ici.fMask = CMIC_MASK_FLAG_NO_UI;
   ici.hwnd = hWnd;
   ici.lpVerb = (LPCSTR)(INT_PTR)(id - 1);
   ici.lpParameters = NULL;
   ici.lpDirectory = NULL;
   ici.nShow = SW_SHOWNORMAL;
   ici.dwHotKey = 0;
   ici.hIcon = NULL;
   hr = icm->lpVtbl->InvokeCommand(icm , &ici);

   FolderTreeUpdate( hWnd, (HTREEITEM) SendMessage( hWnd, TVM_GETNEXTITEM, TVGN_ROOT, 0 ) );
Done:
   m_pMalloc->lpVtbl->Release(m_pMalloc);
}

HB_FUNC( FOLDERTREEGETPATH )
{
   HWND hWnd = (HWND) hb_parnl(1);
   char lpBuffer[ MAX_PATH + 1 ];

   TVITEM tvi;
   tvi.mask = TVIF_PARAM | TVIF_HANDLE;
   tvi.hItem = TreeView_GetSelection( hWnd );
   TreeView_GetItem( hWnd, &tvi );

   SHGetPathFromIDList( ((LPTVITEMDATA*)tvi.lParam)->lpifq, lpBuffer);
   hb_retc( lpBuffer );
}

HB_FUNC( FOLDERTREEGETIDLIST )
{
   HWND hWnd = (HWND) hb_parnl(1);

   TVITEM tvi;
   tvi.mask = TVIF_PARAM | TVIF_HANDLE;
   tvi.hItem = TreeView_GetSelection( hWnd );
   TreeView_GetItem( hWnd, &tvi );

   hb_retnl( (LONG) (((LPTVITEMDATA*)tvi.lParam)->lpifq) );
}

