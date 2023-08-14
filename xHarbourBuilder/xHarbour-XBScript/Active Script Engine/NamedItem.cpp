/******************************************************************************
*
*  File: NamedItem.cpp
*
*  Author:  Joel Alley
*
*  Date: October 26, 1998
*
*  Description:   This file contains the definition of the NamedItem class,
*                 which helps the engine map names added to it's namespace via
*                 IActiveScript::AddNamedItem to IDispatch pointers retrieved
*                 via IActiveScriptSite::GetItemInfo.
*
*  Modifications:
******************************************************************************/
#define NAMEDITEM

#include "activeDepends.h"

IActiveScriptSite* NamedItem::m_pScriptSite;

//constructor
NamedItem::NamedItem( LPCOLESTR name, DWORD dwFlags, IDispatch* pDispatch )
{
   wcscpy( (LPOLESTR) m_Name, name );
   m_dwFlags = dwFlags;
   m_pDispatch = pDispatch;
}

//Destructor
NamedItem::~NamedItem()
{
   NAMEDITEMTRACE("~~~NamedItem::NamedItem\n");

   if( m_pDispatch != NULL )
   {
      m_pDispatch->Release();
      m_pDispatch = NULL;
   }

   NAMEDITEMTRACE("DONE ~~~NamedItem::NamedItem\n");
}

/******************************************************************************
*  SetScriptSite -- This method sets the static IActiveScriptSite reference 
*                   that all NamedItem's use to reconcile their names to 
*                   IDispatch pointers.
*  Parameters: pScriptSite -- a reference to the Active Script Host
*  Returns: none
******************************************************************************/
void NamedItem::SetScriptSite( IActiveScriptSite* pScriptSite )
{
   //tracing purposes only
   NAMEDITEMTRACE("NamedItem::SetScriptSite\n");

   m_pScriptSite = pScriptSite;
}

/******************************************************************************
*  GetName -- This method returns the name that was added to the engine via
*             IActiveScript::AddNamedItem.
*  Parameters: none
*  Returns: the LPCOLESTR that contains this NamedItem's name
******************************************************************************/
LPCOLESTR NamedItem::GetName()
{
   //tracing purposes only
   NAMEDITEMTRACE("NamedItem::GetName\n");

   return m_Name;
}

/******************************************************************************
*  GetFlags -- This method returns the flags that were set when this NamedItem
*              was added to the engine.
*  Parameters: none
*  Returns: a DWORD containing the flags that were set for this NamedItem
******************************************************************************/
DWORD NamedItem::GetFlags()
{
   //tracing purposes only
   NAMEDITEMTRACE("NamedItem::GetFlags\n");

   return m_dwFlags;
}

/******************************************************************************
*  GetDispatch -- This method retrieves the IDispatch pointer that this
*                 NamedItem coincides with.
*  Parameters: ppDispatch -- address of the IDispatch pointer to receive the
*                            requested IDispatch
*  Returns: S_OK
*           E_FAIL
*           DISP_E_UNKNOWNNAME
******************************************************************************/
STDMETHODIMP NamedItem::GetDispatch( IDispatch** ppDispatch )
{
   //tracing purposes only
   NAMEDITEMTRACE("NamedItem::GetDispatch\n");

   HRESULT hr = S_OK;

   //If the m_pDispatch pointer is null, then try to reconcile the NamedItem.
   if( m_pDispatch == NULL )
   {
      hr = Reconcile();
   }

   //If we succeeded, or if we didn't need to reconcile, then set the pointer
   //into ppDispatch and AddRef
   if( SUCCEEDED(hr) )
   {
      *ppDispatch = m_pDispatch;
       m_pDispatch->AddRef();
   }
   else
   {
      if( m_pDispatch )
      {
         NAMEDITEMTRACE("*** OOPS NamedItem::GetDispatch\n");
	      m_pDispatch = NULL;
	   }
   }

   //report our success or failure
   return hr;
}

/******************************************************************************
*  Reconcile -- This method reconciles a name in the engine's namespace with an
*               IDispatch pointer provided by the host.
*  Parameters: none
*  Returns: S_OK
*           E_FAIL
*           DISP_E_UNKNOWNNAME;
******************************************************************************/
STDMETHODIMP NamedItem::Reconcile()
{
   //tracing purposes only
   NAMEDITEMTRACE("NamedItem::Reconcile\n");

   IUnknown* pUnknown = NULL;
   HRESULT hr = S_OK;

   //If the NamedItem hasn't been reconciled before, then do so now.  If it has
   //then do nothing.
   if( m_pDispatch == NULL )
   {
      //try getting an IUnknown pointer from the Script Host.
      hr = m_pScriptSite->GetItemInfo( m_Name, SCRIPTINFO_IUNKNOWN, &pUnknown, NULL );
      
      //If that was successful, then QI for IDispatch, and release the IUnknown
      //pointer
      if( SUCCEEDED(hr) )
	   {
         hr = pUnknown->QueryInterface( IID_IDispatch, (void**)&m_pDispatch );
         pUnknown->Release();
         pUnknown = NULL;
		   OutputDebugString( "Conciled Dispatch\n" );
      }
	   else
	   {
         NAMEDITEMTRACE("FAILED NamedItem::Reconcile\n");

	      if( m_pDispatch )
		   {
            NAMEDITEMTRACE("*** OOPS NamedItem::Reconcile\n");
	        m_pDispatch = NULL;
		   }
	   }
   }
   
   //report our success or failure
   return hr;
}

/******************************************************************************
*  Disconnect -- This method releases the IDispatch pointer this named item is
*  attached to.  This is used when transitioning into the 
*  SCRIPTSTATE_UNINITIALIZED state.
*  Parameters: none
*  Returns: S_OK
******************************************************************************/
STDMETHODIMP NamedItem::Disconnect()
{
   //tracing purposes only
   NAMEDITEMTRACE("NamedItem::Disconnect\n");

   if( m_pDispatch != NULL )
   {
      m_pDispatch->Release();
      m_pDispatch = NULL;
   }

   return S_OK;
}