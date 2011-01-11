/******************************************************************************
*
*  File: NamedItem.h
*
*  Author:  Joel Alley
*
*  Date: October 26, 1998
*
*  Description:   This file contains the declaration of the NamedItem class,
*                 which helps the engine map names added to it's namespace via
*                 IActiveScript::AddNamedItem to IDispatch pointers retrieved
*                 via IActiveScriptSite::GetItemInfo.
*
*  Modifications:
******************************************************************************/

#ifndef _h_NamedItem
#define _h_NamedItem

class NamedItem{
protected:
   static IActiveScriptSite* m_pScriptSite;

   OLECHAR m_Name[256];
   DWORD m_dwFlags;
   IDispatch* m_pDispatch;

public:
   //constructor
   NamedItem( LPCOLESTR name, DWORD dwFlags, IDispatch* pDispatch = NULL );
   //Destructor
   ~NamedItem();

   static void SetScriptSite( IActiveScriptSite* pScriptSite );
   LPCOLESTR GetName();
   DWORD GetFlags();
   STDMETHODIMP GetDispatch( IDispatch** ppDispatch );
   STDMETHODIMP Disconnect();

protected:
   STDMETHODIMP Reconcile();
};

#endif
