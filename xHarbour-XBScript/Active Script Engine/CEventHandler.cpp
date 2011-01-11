/******************************************************************************
*
*  File: CEventHandler.cpp
*
*  Author:  Joel Alley
*
*  Date: August 26, 1998
*
*  Description:   This file contains the definition of classes necessary for
*                 the script engine to sink events from objects added with 
*                 IActiveScript::AddNamedItem.  Essentially this involves 
*                 determining what events are sunk in the script, connecting to
*                 the event source through connection points, and mapping
*                 between the dispids of the event interfaces and the dispids
*                 of events in the script engine.
*
*  Modifications:
******************************************************************************/
#define EVENTHANDLER
#include "activeDepends.h"

/******************************************************************************
*  CEventHandler -- CEventHandler encapsulates the connection point the engine
*  needs to sink events from other COM objects.  It maintains a COM pointer to
*  the event source and to the CASInterpreter which can sink the events.  This 
*  class also maintains a list which maps between the dispid's of the 
*  dispinterface it sinks and the dispid's of the methods in the CASInterpreter.
******************************************************************************/
CEventHandler::CEventHandler( LPCOLESTR objectName )
{
   EVENTTRACE("CEventHandler\n");

   m_refCount = 1;

   m_objectName = objectName;
   m_ConnectedInterface = IID_NULL;
   m_pSourceDispatch = NULL;
   m_pSinkDispatch = NULL;
   m_pConnectionPoint = NULL;
   m_isInitialized = false;
   m_isConnected = false;
}

CEventHandler::~CEventHandler()
{
   EVENTTRACE("~CEventHandler\n");

   _ASSERTE( m_refCount == 0 );

   Reset();
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
STDMETHODIMP CEventHandler::QueryInterface(REFIID riid, void ** ppvObj)
{
   //tracing purposes only
   EVENTTRACE("CEventHandler::QueryInterface->");
   
   if (riid == IID_IUnknown)
   {
      EVENTTRACE("IUnknown\n");
      *ppvObj = static_cast<IDispatch*>(this);
   }   
   else if (riid == IID_IDispatch)
   {
      EVENTTRACE("IDispatch\n");
      *ppvObj = static_cast<IDispatch*>(this);
   }
   else if (riid == m_ConnectedInterface)
   {
      EVENTTRACE("Event Interface\n");
      *ppvObj = this;
   }
   else
   {
      EVENTTRACE("Unsupported Interface\n");
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
STDMETHODIMP_(ULONG) CEventHandler::AddRef()
{
   //tracing purposes only
   EVENTTRACE("CEventHandler::AddRef\n");
   
   return ++m_refCount;
}

/******************************************************************************
*   Release() -- When a reference to this object is removed, this function 
*   decrements the reference count.  If the reference count is 0, then this 
*   function deletes this object and returns 0;
******************************************************************************/
STDMETHODIMP_(ULONG) CEventHandler::Release()
{
   //tracing purposes only
   EVENTTRACE("CEventHandler::Release\n");
   
   --m_refCount;

   _ASSERTE( m_refCount >= 0 );
   
   if( m_refCount == 0 )
   {
      delete this;
      return 0;
   }

   return m_refCount;
}

/******************************************************************************
*   IDispatch Interface -- This interface allows this class to be used as an
*   automation server, allowing its functions to be called by other COM
*   objects
******************************************************************************/

/******************************************************************************
*   GetTypeInfoCount -- This function determines if the class supports type 
*   information interfaces or not.  It places 1 in iTInfo if the class supports
*   type information and 0 if it doesn't.
******************************************************************************/
STDMETHODIMP CEventHandler::GetTypeInfoCount(UINT *iTInfo)
{
   //tracing purposes only
   EVENTTRACE("CEventHandler::GetTypeInfoCount\n");
   
   //We don't support type information
   *iTInfo = 0;
   return S_OK;
}

/******************************************************************************
*   GetTypeInfo -- Returns the type information for the class.  For classes 
*   that don't support type information, this function returns E_NOTIMPL;
******************************************************************************/
STDMETHODIMP CEventHandler::GetTypeInfo(UINT iTInfo, LCID lcid, 
                                       ITypeInfo **ppTInfo)
{
   //tracing purposes only
   EVENTTRACE("CEventHandler::GetTypeInfo\n");
   
   //We don't support type information
   *ppTInfo = NULL;
   return DISP_E_BADINDEX;
}

/******************************************************************************
*   GetIDsOfNames -- Takes an array of strings and returns an array of DISPID's
*   which corespond to the methods or properties indicated.  If the name is not 
*   recognized, returns DISP_E_UNKNOWNNAME.
******************************************************************************/
STDMETHODIMP CEventHandler::GetIDsOfNames(REFIID riid,  
                                         OLECHAR **rgszNames, 
                                         UINT cNames,  LCID lcid,
                                         DISPID *rgDispId)
{
   //tracing purposes only
   EVENTTRACE("CEventHandler::GetIDsOfNames -- UNEXPECTED\n");
   
   return E_NOTIMPL;
}

/******************************************************************************
*   Invoke -- Takes a dispid and uses it to call another of this class's 
*   methods.  Returns S_OK if the call was successful.
******************************************************************************/
STDMETHODIMP CEventHandler::Invoke(DISPID dispIdMember, REFIID riid, LCID lcid,
                                  WORD wFlags, DISPPARAMS* pDispParams,
                                  VARIANT* pVarResult, EXCEPINFO* pExcepInfo,
                                  UINT* puArgErr)
{
   //tracing purposes only
   EVENTTRACE("CEventHandler::Invoke\n");
   
   //Validate arguments
   if ((riid != IID_NULL))
      return E_INVALIDARG;
   
   HRESULT hr = S_OK;
   
   //if we are not connected, just clean up the return value
   if (!m_isConnected){
      if (pVarResult)
         VariantInit( pVarResult );
   }
   else
   {
      //Run through the event set and try to find one with the right dispid.
      CEvent* pEvent = NULL;

      FOREACH( m_EventList, 
         pEvent = m_EventList.Retrieve();

         if( pEvent->m_eventDispid == dispIdMember )
         {
            break;
         }
      );

      //if we found one, check to see if it's implemented.  If so, then call
      //the source IDispatch's Invoke with the new dispid, and the other 
      //parameters.
      if ((pEvent) && (pEvent->m_isImplemented))
	   {
         hr = m_pSinkDispatch->Invoke( pEvent->m_engineDispid, riid, lcid, wFlags, pDispParams, pVarResult, pExcepInfo, puArgErr );
      }
   }

   //All done...
   return hr;
}

/******************************************************************************
*  Initialize -- This method takes an IDispatch* for the source and sink 
*  and uses them to construct the list of CEvents which maps between the 
*  dispids of the outgoing interface and the dispids of the sinking interface.
******************************************************************************/
STDMETHODIMP CEventHandler::Initialize( IDispatch* pSourceDispatch, IDispatch* pSinkDispatch )
{
   //tracing purposes only
   EVENTTRACE("CEventHandler::Initialize\n");
   
   HRESULT hr = S_OK;

   //If we've already initialized this event handler, we don't need to do it
   //again.
   if( ! m_isInitialized )
   {   
      //The first thing to do is save away the IDispatch pointers so we can
      //use them later.  Don't forget to AddRef them.
      m_pSourceDispatch = pSourceDispatch;
      m_pSourceDispatch->AddRef();
      m_pSinkDispatch = pSinkDispatch;
      m_pSinkDispatch->AddRef();
      
      //The next thing to do is find the event source interface on the 
      //pSourceDispatch object.
      ITypeInfo* ptinfoSource = NULL;

      if( GetEventSourceTypeInfo( &ptinfoSource ) )
      {         
         //Use the type info to get the type attributes
         TYPEATTR* pTypeAttributes = NULL;
         ptinfoSource->GetTypeAttr( &pTypeAttributes );
         
         //We need the GUID of the interface we're going to sink, so copy it.
         memcpy( &m_ConnectedInterface, &pTypeAttributes->guid, sizeof(GUID) );
         
         //How many events are there in this interface?
         int numEvents = pTypeAttributes->cFuncs;
         
         //make sure we have some events to sink
         if( numEvents == 0 )
         {
            //Clean up...
            ptinfoSource->ReleaseTypeAttr( pTypeAttributes );
            pTypeAttributes = NULL;

            ptinfoSource->Release();
            ptinfoSource = NULL;

            return E_NOINTERFACE;
         }         
		   else
         {
			   //Create the list of events
			   FUNCDESC* pfdesc = NULL;
			   BSTR bstrName = NULL;
			   UINT cNames;

			   for (int i = 0; i < numEvents; i++)
            {
				   //Get the function description
				   ptinfoSource->GetFuncDesc( i, &pfdesc );
				 
				   //Get the name of the event
				   ptinfoSource->GetNames( pfdesc->memid, &bstrName, 1, &cNames );
				 
				   //Create the CEvent object
				   CEvent* pEvent = new CEvent( bstrName, pfdesc->memid );
				 
				   //Add the event to the list
				   m_EventList.InsertAfter( pEvent, bstrName );
				 
				   //Release the function description
				   ptinfoSource->ReleaseFuncDesc( pfdesc );
               pfdesc = NULL;
			   }
			 
			   ptinfoSource->ReleaseTypeAttr( pTypeAttributes );
            pTypeAttributes = NULL;

            ptinfoSource->Release();
            ptinfoSource = NULL;

            m_isInitialized = true;
		   }
      }
      else 
      {
         hr = E_FAIL;
      }
   }
   else
   {
      hr = E_FAIL;
   }

   //When we've completed the list, we're done.
   return hr;
}

/******************************************************************************
*  MapEvents -- This method runs through the list of events sunk by this 
*  handler and maps their dispids to the dispids of the interpreter that sinks
*  those events.  It provides a means of adding new methods.
******************************************************************************/
void CEventHandler::MapEvents()
{
   //tracing purposes only
   EVENTTRACE("CEventHandler::MapEvents\n");
   
   HRESULT hr = S_OK;

   //If we don't have a sink IDispatch yet, do nothing.
   if (m_pSinkDispatch){

      CEvent* pEvent = NULL;

      FOREACH( m_EventList, 
         //Get each event.
         pEvent = m_EventList.Retrieve();
      
         //Try and get a dispid for it from the sink
         hr = m_pSinkDispatch->GetIDsOfNames( IID_NULL, (LPOLESTR *) (&(pEvent->m_eventName)), 1, 0, &(pEvent->m_engineDispid) );
      
         if (SUCCEEDED(hr))
         {
            //mark the event as implemented
            pEvent->m_isImplemented = true;
         }
      );
   }
}

/******************************************************************************
*  AliasEvent -- This method maps the name of an event to a different name in
*  the IDispatch that serves as the event sink.
*  Parameters: lpzExternalName -- The event name used by the event source
*              lpzInternalName -- The event name used by the event sink
*  Returns: none
******************************************************************************/
void CEventHandler::AliasEvent( LPCOLESTR lpzExternalName, 
                               LPCOLESTR lpzInternalName )
{
   //tracing purposes only
   EVENTTRACE("CEventHandler::AliasEvent\n");

   HRESULT hr = S_OK;

   //If we don't have a sink IDispatch yet, do nothing.
   if (m_pSinkDispatch){
      
      CEvent* pEvent = NULL;

      if( m_EventList.FindByName( lpzExternalName ) )
      {
         //Get the CEvent
         pEvent = m_EventList.Retrieve();
         
         //Try and get a dispid for it from the sink
         hr = m_pSinkDispatch->GetIDsOfNames( IID_NULL, (LPOLESTR *) &lpzInternalName, 1, 0, &(pEvent->m_engineDispid) );
         
         if (SUCCEEDED(hr))
         {
            //mark the event as implemented
            pEvent->m_isImplemented = true;
         }
      }
   }
}

/******************************************************************************
*  ConnectEvents -- If this handler has not already connected, this method
*  finds the connection point for the interface this handler sinks, and calls
*  IConnectionPoint::Advise() on it.
******************************************************************************/
STDMETHODIMP CEventHandler::ConnectEvents()
{
   //tracing puposes only
   EVENTTRACE("CEventHandler::ConnectEvents\n");

   HRESULT hr = E_FAIL;

   //Make sure we have both a source IDispatch and a sink IDispatch...
   if ((m_pSourceDispatch) && (m_pSinkDispatch)){
      //First, map the event dispids to their sink counterparts.
      MapEvents();

      //Now, get the IConnectionPointContainer of the source object.
      IConnectionPointContainer* pConnectionPointContainer = NULL;
      hr = m_pSourceDispatch->QueryInterface( IID_IConnectionPointContainer, (void**)&pConnectionPointContainer );

      if( FAILED(hr) )
      {
         return hr;
      }

      //Use the interface to get the connection point we want
      hr = pConnectionPointContainer->FindConnectionPoint( m_ConnectedInterface, &m_pConnectionPoint );
      pConnectionPointContainer->Release();
      pConnectionPointContainer = NULL;

      if( SUCCEEDED(hr) )
      {
         //Use the connection point to hook up to the event source
         hr = m_pConnectionPoint->Advise( (IUnknown*)this, &m_Cookie );
      }

      //Finally, set m_isConnected to true.
      m_isConnected = true;
   }

   return hr;
}

/******************************************************************************
*  DisconnectEvents -- If this handler has connected to an event source, this
*  method calls IConnectionPoint::Unadvise() on it.
******************************************************************************/
STDMETHODIMP CEventHandler::DisconnectEvents()
{
   //tracing purposes only
   EVENTTRACE("CEventHandler::DisconnectEvents\n");

   HRESULT hr = S_OK;

   //First, inform the connection point we are no longer interested in events.
   if (m_pConnectionPoint){
      hr = m_pConnectionPoint->Unadvise( m_Cookie );
      
      //Release the connection point interface
      m_pConnectionPoint->Release();
      m_pConnectionPoint = NULL;
   }
   
   //Set m_isConnected to false;
   m_isConnected = false;

   return S_OK;
}

/******************************************************************************
*  PseudoDisconnect -- There may be times when we wish to quit responding to
*  events without actually disconnecting from the source.  This method sets the
*  m_isConnected flag to false so the handler won't respond to events.
******************************************************************************/
STDMETHODIMP CEventHandler::PseudoDisconnect()
{
   //tracing purposes only
   EVENTTRACE("CEventHandler::PseudoDisconnect\n");

   m_isConnected = false;
   return S_OK;
}

/******************************************************************************
*  PseudoConnect -- If we PseudoDisconnect from events, we'll need a way to
*  restart event handling without having to call Advise again.  This method
*  sets the m_isConnected flag to true so the handler will begin responding to
*  events again.
******************************************************************************/
STDMETHODIMP CEventHandler::PseudoConnect()
{
   //tracing purposes only
   EVENTTRACE("CEventHandler::PsuedoConnect\n");

   m_isConnected = true;
   return S_OK;
}

/******************************************************************************
*  GetEventSourceTypeInfo -- This method searches the type information provided
*  by the m_pSourceDispatch, and finds the interface which is marked default,
*  source, and is not marked restricted.  If no such interface is available,
*  this method sets the pSourceInfo pointer to NULL.
******************************************************************************/
BOOL CEventHandler::GetEventSourceTypeInfo( ITypeInfo** ppSourceInfo )
{
   HRESULT hr;
   IProvideClassInfo* pClassInfo = NULL;
   TYPEATTR* pTypeAttributes = NULL;
   ITypeInfo* pCoClassTypeInfo = NULL;
   int numInterfaces;
   int interfaceFlags;

   //make sure we have an source IDispatch to work with.
   if( m_pSourceDispatch )
   {
      //Get the TypeInfo from the source IDispatch
      hr = m_pSourceDispatch->QueryInterface( IID_IProvideClassInfo, (void**)&pClassInfo);

      if( FAILED(hr) )
      {
         return false;
      }

      hr = pClassInfo->GetClassInfo( &pCoClassTypeInfo );
      pClassInfo->Release();
      pClassInfo = NULL;

      if( FAILED(hr) )
      {
         return false;
      }

      //Check the type attributes of the ITypeInfo to make sure it's a coclass
      hr = pCoClassTypeInfo->GetTypeAttr( &pTypeAttributes );

      if( SUCCEEDED(hr) )
      {
         if( pTypeAttributes->typekind != TKIND_COCLASS )
         {
            return false;
         }
      }
      else
      {
         return false;
      }

      //How many interfaces does this coclass implement?
      numInterfaces = pTypeAttributes->cImplTypes;

      //Release the type attributes
      pCoClassTypeInfo->ReleaseTypeAttr( pTypeAttributes );
      pTypeAttributes = NULL;

      //Now, search through the set of interfaces looking for one that is marked
      //default, source, and not restricted.
      for( int i = 0; i < numInterfaces; i++ )
      {
         hr = pCoClassTypeInfo->GetImplTypeFlags( i, &interfaceFlags );

         if( FAILED(hr) )
         {
            return false;
         }

         if( ( interfaceFlags & (IMPLTYPEFLAG_FDEFAULT | IMPLTYPEFLAG_FSOURCE | IMPLTYPEFLAG_FRESTRICTED) ) == (IMPLTYPEFLAG_FDEFAULT | IMPLTYPEFLAG_FSOURCE) )
         {
            //This is the interface we need, so get the RefType of it.
            HREFTYPE refType;

            hr = pCoClassTypeInfo->GetRefTypeOfImplType( i, &refType );

            if (SUCCEEDED(hr))
            {
               hr = pCoClassTypeInfo->GetRefTypeInfo( refType, ppSourceInfo );
               return true;
            }
         }
      }
   }

   return false;
}

/******************************************************************************
*  Reset -- This method resets the event handler
*  Parameters: none
*  Returns: none
******************************************************************************/
void CEventHandler::Reset()
{
   //tracing purposes only
   EVENTTRACE("CEventHandler::Reset\n");

   if( m_pSinkDispatch != NULL )
   {
      m_pSinkDispatch->Release();
      m_pSinkDispatch = NULL;
   }

   if( m_pConnectionPoint != NULL )
   {
      m_pConnectionPoint->Release();
      m_pConnectionPoint = NULL;
   }

   if( m_pSourceDispatch != NULL )
   {
      m_pSourceDispatch->Release();
      m_pSourceDispatch = NULL;
   }

   m_EventList.RemoveAll();
}