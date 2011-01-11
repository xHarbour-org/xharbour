/******************************************************************************
*
*  File: CEventHandler.h
*
*  Author:  Joel Alley
*
*  Date: August 26, 1998
*
*  Description:   This file contains the declaration of classes necessary for
*                 the script engine to sink events from objects added with 
*                 IActiveScript::AddNamedItem.  Essentially this involves 
*                 determining what events are sunk in the script, connecting to
*                 the event source through connection points, and mapping
*                 between the dispids of the event interfaces and the dispids
*                 of events in the script engine.
*
*  Modifications:
******************************************************************************/

#ifndef _h_CEventHandler
#define _h_CEventHandler

class CEvent {
public:
   //Data members
   LPCOLESTR m_eventName;
   DISPID m_eventDispid;
   DISPID m_engineDispid;
   BOOL m_isImplemented;

   //Constructor
   CEvent( LPCOLESTR eventName, DISPID eventDispid ) 
      : m_eventName( eventName), m_eventDispid( eventDispid ), 
      m_engineDispid(0), m_isImplemented( false ) {;}
   //Destructor
   ~CEvent() {;}
};

class CEventHandler : public IDispatch {
private:
   //Data members
   int m_refCount;
   LPCOLESTR m_objectName;
   IID m_ConnectedInterface;
   TList<CEvent*> m_EventList;
   IDispatch* m_pSourceDispatch;
   IDispatch* m_pSinkDispatch;
   IConnectionPoint* m_pConnectionPoint;
   DWORD m_Cookie;
   BOOL m_isInitialized;
   BOOL m_isConnected;

public:
   //Constructor
   CEventHandler( LPCOLESTR objectName );
   //Destructor
   ~CEventHandler();

      /***** IUnknown Methods *****/
   STDMETHODIMP QueryInterface(REFIID riid, void**ppvObj);
   STDMETHODIMP_(ULONG) AddRef();
   STDMETHODIMP_(ULONG) Release();
 
   /***** IDispatch Methods *****/
   STDMETHODIMP GetTypeInfoCount(UINT* iTInfo);
   STDMETHODIMP GetTypeInfo(UINT iTInfo, LCID lcid, ITypeInfo** ppTInfo);
   STDMETHODIMP GetIDsOfNames(REFIID riid, OLECHAR** rgszNames,
      UINT cNames, LCID lcid, DISPID* rgDispId);
   STDMETHODIMP Invoke(DISPID dispIdMember, REFIID riid, LCID lcid,  
      WORD wFlags, DISPPARAMS* pDispParams,  VARIANT* pVarResult,
      EXCEPINFO* pExcepInfo,  UINT* puArgErr);

   /***** Non-COM methods *****/
   BOOL IsInitialized() {return m_isInitialized; }
   BOOL IsConnected() {return m_isConnected; }
   STDMETHODIMP Initialize( IDispatch* pSourceDispatch, 
      IDispatch* pSinkDispatch );
   void MapEvents();
   void AliasEvent( LPCOLESTR lpzExternalName, LPCOLESTR lpzInternalName );
   STDMETHODIMP ConnectEvents();
   STDMETHODIMP DisconnectEvents();
   STDMETHODIMP PseudoDisconnect();
   STDMETHODIMP PseudoConnect();
   void Reset();

private:
   BOOL GetEventSourceTypeInfo( ITypeInfo** pSourceInfo );
};
#endif
