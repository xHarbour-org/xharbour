/******************************************************************************
*
*  File: SampleClassFactory.cpp
*
*  Author:  Joel Alley
*
*  Date: November 9, 1998
*
*  Description:   This file contains the definition of the Class Factory for
*                 the sample script engine.  All the methods are fairly generic,
*                 and nothing specific to scripting occurs here.
*  Modifications:
******************************************************************************/
#define CLASSFACTORY
#include "activeDepends.h"

XBScriptFactory::XBScriptFactory()
{
   //tracing purposes only
   SAMPLESCRIPTFACTORYTRACE("XBScriptFactory\n");

	m_refCount = 1;
}

XBScriptFactory::~XBScriptFactory()
{
   _ASSERT( m_refCount == 0 );

   //tracing purposes only
   SAMPLESCRIPTFACTORYTRACE("~~~XBScriptFactory\n");
}

/******************************************************************************
*   IUnknown Interfaces -- All COM objects must implement, either directly or
*   indirectly, the IUnknown interface.
******************************************************************************/

/******************************************************************************
*   QueryInterface -- Determines if this component supports the requested
*   interface, places a pointer to that interface in ppvObj if it's available,
*   and returns S_OK.  If not, sets ppvObj to NULL and returns E_NOINTERFACE.
******************************************************************************/
STDMETHODIMP XBScriptFactory::QueryInterface(REFIID riid, void ** ppvObj)
{
   //tracing purposes only
   SAMPLESCRIPTFACTORYTRACE("XBScriptFactory::QueryInterface->");

   if (riid == IID_IUnknown)
   {
      SAMPLESCRIPTFACTORYTRACE("IUnknown\n");
      *ppvObj = static_cast<IClassFactory*>(this);
   }
   else if (riid == IID_IClassFactory)
   {
      SAMPLESCRIPTFACTORYTRACE("IDispatch\n");
      *ppvObj = static_cast<IClassFactory*>(this);
   }
   else
   {
      SAMPLESCRIPTFACTORYTRACE("XBScriptFactory::Unsupported Interface\n");
      *ppvObj = NULL;
      return E_NOINTERFACE;
   }

   static_cast<IUnknown*>(*ppvObj)->AddRef();

   return S_OK;
}

/******************************************************************************
*   AddRef() -- In order to allow an object to delete itself when it is no
*   longer needed, it is necessary to maintain a count of all references to
*   this object.  When a new reference is created, this function increments
*   the count.
******************************************************************************/
STDMETHODIMP_(ULONG) XBScriptFactory::AddRef()
{
   //tracing purposes only
   SAMPLESCRIPTFACTORYTRACE("XBScriptFactory::AddRef\n");

   return ++m_refCount;
}

/******************************************************************************
*   Release() -- When a reference to this object is removed, this function
*   decrements the reference count.  If the reference count is 0, then this
*   function deletes this object and returns 0;
******************************************************************************/
STDMETHODIMP_(ULONG) XBScriptFactory::Release()
{
   //tracing purposes only
   SAMPLESCRIPTFACTORYTRACE("XBScriptFactory::Release\n");

   --m_refCount;

   _ASSERTE( m_refCount >= 0 );

   if( m_refCount == 0 )
   {
      delete this;
      return 0;
   }

   return m_refCount;
}


/******* IClassFactory Methods *******/
/******************************************************************************
*	CreateInstance() -- This method attempts to create an instance of XBScript
*	and returns it to the caller.  It maintains a count of the number of
*	created objects.
******************************************************************************/
STDMETHODIMP XBScriptFactory::CreateInstance( LPUNKNOWN pUnkOuter, REFIID riid, LPVOID *ppvObj )
{
   //tracing purposes only
   SAMPLESCRIPTFACTORYTRACE("XBScriptFactory::CreateInstance\n");

	HRESULT hr;
	XBScript* pObj;

	*ppvObj = NULL;
	hr = E_OUTOFMEMORY;

	//XBScript does not support aggregation, so be sure no one asked
	if (pUnkOuter != NULL)
	{
		return CLASS_E_NOAGGREGATION;
	}

	//Create a new instance of XBScript
	pObj = new XBScript();

	if( pObj == NULL )
	{
		return hr;
	}

	hr = pObj->QueryInterface( riid, ppvObj );

	if( FAILED(hr) )
	{
		delete pObj;
	}

	return hr;
}

/******************************************************************************
*	LockServer() -- This method maintains a count of the current locks on this
*	DLL.  The count is used to determine if the DLL can be unloaded, or if
*	clients are still using it.
******************************************************************************/
STDMETHODIMP XBScriptFactory::LockServer(BOOL fLock)
{
    //tracing purposes only
    SAMPLESCRIPTFACTORYTRACE("XBScriptFactory::LockServer\n");

    OutputDebugValues( "XBScriptFactory::LockServer(%i) (%i)", fLock, g_cLock );

	if( fLock )
	{
		g_cLock++;
	}
	else
	{
		g_cLock--;
	}

	return NOERROR;
}

