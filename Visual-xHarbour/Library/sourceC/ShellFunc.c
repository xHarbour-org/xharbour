#include "hbapi.h"

#define DUMMYUNIONNAME

#include <windows.h>
#include <stdio.h>
#include <shlobj.h>

void GetNormalAndSelectedIcons(LPITEMIDLIST lpifq, LPTV_ITEM lptvitem);
int GetNormalIcon(LPITEMIDLIST lpifq);
int GetIcon (LPITEMIDLIST lpi, UINT uFlags);
LPITEMIDLIST CopyItemID(LPMALLOC g_pMalloc, LPITEMIDLIST pidl);
LPITEMIDLIST Concatenate(LPMALLOC lpMalloc, LPCITEMIDLIST pidl1, LPCITEMIDLIST pidl2);
UINT GetSize(LPCITEMIDLIST pidl);
LPITEMIDLIST GetNextItem(LPCITEMIDLIST pidl);
LPITEMIDLIST Copy(LPMALLOC lpMalloc, LPCITEMIDLIST pidlSource);

BOOL GetName( LPSHELLFOLDER lpsf, LPITEMIDLIST lpi, DWORD dwFlags, LPSTR lpFriendlyName )
{
   BOOL bSuccess = TRUE;
   STRRET str;
   if ( NOERROR == lpsf->lpVtbl->GetDisplayNameOf( lpsf, lpi, dwFlags, &str ) )
   {
      switch (str.uType)
      {
         case STRRET_WSTR:
              WideCharToMultiByte (CP_ACP, 0, str.pOleStr, -1, lpFriendlyName, sizeof(str), NULL, NULL );
              break;

         case STRRET_OFFSET:
              lstrcpy (lpFriendlyName, (LPSTR)lpi + str.uOffset);
              break;

         case STRRET_CSTR:
              lstrcpy (lpFriendlyName, (LPSTR) str.cStr);
              break;

         default:
              bSuccess = FALSE;
              break;
      }
   }
   else
   {
      bSuccess = FALSE;
   }
   return bSuccess;
}

//--------------------------------------------------------------------------------------------------------------
void GetNormalAndSelectedIcons( LPITEMIDLIST lpifq, LPTV_ITEM lptvitem )
{
   lptvitem->iImage = GetIcon (lpifq, SHGFI_PIDL | CSIDL_DESKTOP | SHGFI_ICON | SHGFI_SYSICONINDEX | SHGFI_SMALLICON) ;
   lptvitem->iSelectedImage = GetIcon (lpifq, SHGFI_PIDL | SHGFI_SYSICONINDEX | SHGFI_SMALLICON | SHGFI_OPENICON );
}

//--------------------------------------------------------------------------------------------------------------
int GetIcon( LPITEMIDLIST lpi, UINT uFlags )
{
   SHFILEINFO sfi;
   SHGetFileInfo( (LPCSTR)lpi, 0, &sfi, sizeof (SHFILEINFO), uFlags );
   return sfi.iIcon;
}

//--------------------------------------------------------------------------------------------------------------
LPITEMIDLIST CopyItemID( LPMALLOC pMalloc, LPITEMIDLIST pidl )
{
   int cb = pidl->mkid.cb;
   LPITEMIDLIST pidlNew = (LPITEMIDLIST)
   pMalloc->lpVtbl->Alloc(pMalloc, cb + sizeof(USHORT));
   if (pidlNew == NULL)
   {
      return NULL;
   }
   CopyMemory(pidlNew, pidl, cb);
   *((USHORT *) (((LPBYTE) pidlNew) + cb)) = 0;
   return pidlNew;
}

//--------------------------------------------------------------------------------------------------------------
LPITEMIDLIST Concatenate( LPMALLOC pMalloc, LPCITEMIDLIST pidl1, LPCITEMIDLIST pidl2 )
{
   LPITEMIDLIST   pidlNew;
   UINT           cb1, cb2;
   
   if(!pidl1 && !pidl2)
   {
      return NULL;
   }
   
   if(!pidl1)
   {
      pidlNew = Copy(pMalloc , pidl2);
      return pidlNew;
   }
   
   if(!pidl2)
   {
      pidlNew = Copy(pMalloc, pidl1);
      return pidlNew;
   }
   
   cb1 = GetSize(pidl1) - sizeof(ITEMIDLIST);
   cb2 = GetSize(pidl2);
   pidlNew = (LPITEMIDLIST)pMalloc->lpVtbl->Alloc(pMalloc, cb1 + cb2);
   
   if(pidlNew)
   {
      CopyMemory(pidlNew, pidl1, cb1);
      CopyMemory(((LPBYTE)pidlNew) + cb1, pidl2, cb2);
   }
   
   return pidlNew;
}

//--------------------------------------------------------------------------------------------------------------
UINT GetSize( LPCITEMIDLIST pidl )
{
   UINT cbTotal = 0;
   LPITEMIDLIST pidlTemp = (LPITEMIDLIST) pidl;
   if( pidlTemp )
   {
      while( pidlTemp->mkid.cb )
      {
         cbTotal += pidlTemp->mkid.cb;
         pidlTemp = GetNextItem( pidlTemp );
      }
      cbTotal += sizeof(ITEMIDLIST);
    }
   return (cbTotal);
}

//--------------------------------------------------------------------------------------------------------------
LPITEMIDLIST GetNextItem(LPCITEMIDLIST pidl)
{
   if(pidl)
   {
      return (LPITEMIDLIST)(LPBYTE)(((LPBYTE)pidl) + pidl->mkid.cb);
   }
   else
   {
      return (NULL);
   }
}

//--------------------------------------------------------------------------------------------------------------
LPITEMIDLIST Copy(LPMALLOC pMalloc , LPCITEMIDLIST pidlSource)
{
   LPITEMIDLIST pidlTarget;
   UINT cbSource;
   
   if( NULL == pidlSource )
   {
      return (NULL);
   }
   
   cbSource = GetSize(pidlSource);
   pidlTarget = (LPITEMIDLIST)pMalloc->lpVtbl->Alloc(pMalloc, cbSource);
   
   if(!pidlTarget)
   {
      return (NULL);
   }
   
   CopyMemory(pidlTarget, pidlSource, cbSource);
   return pidlTarget;
}

//--------------------------------------------------------------------------------------------------------------
int GetNormalIcon(LPITEMIDLIST lpifq)
{
   int nIconIndex;
   nIconIndex = GetIcon(lpifq, SHGFI_PIDL | CSIDL_DESKTOP | SHGFI_ICON | SHGFI_SYSICONINDEX | SHGFI_SMALLICON );
   return nIconIndex;
}
