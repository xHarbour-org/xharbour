/******************************************************************************
*
*  File: XBScript.h
*
*  Author:  Joel Alley
*
*  Date: October 3, 1998
*
*  Description:   This file contains the declaration of the XBScript class,
*                 which serves as the primary object for the sample script
*                 engine.  This object implements the IActiveScript and
*                 IActiveScriptParse interfaces.
*
*  Modifications:
******************************************************************************/
//The global reference count on this DLL.

#ifndef _h_XBScript
#define _h_XBScript

//#include <ActiveScp.h>

#define OLESCRIPT_E_SYNTAX 0x80020101

extern int g_cLock;

class XBScript : public IActiveScript,
                 public IActiveScriptParse,
                 public IActiveScriptParseProcedure,
                 public IHostInfoUpdate,
                 public IObjectSafety
{
protected:
   static int s_m_ID;
   int m_ID;

   int m_refCount;
   SCRIPTSTATE m_ScriptState;

   TList<NamedItem*>      *m_pNamedItems;
   TList<CASInterpreter*> *m_pASInterpreters;
   TList<CEventHandler*>  *m_pEventHandlers;

public:
   IActiveScriptSite* m_pScriptSite;
   CASInterpreter  *m_pGlobalInterpreter;
   CASErrorHandler *m_pErrorHandler;

   //Constructor
   XBScript();
   //Destructor
   ~XBScript();

   /***** IUnknown Methods *****/
   STDMETHODIMP QueryInterface(REFIID riid, void**ppvObj);
   STDMETHODIMP_(ULONG) AddRef();
   STDMETHODIMP_(ULONG) Release();

   /***** IActiveScript Methods *****/
   STDMETHODIMP AddNamedItem( LPCOLESTR pstrName, DWORD dwFlags );
   STDMETHODIMP AddTypeLib( REFGUID guidTypeLib, DWORD dwMaj, DWORD dwMin, DWORD dwFlags );
   STDMETHODIMP Clone( IActiveScript **ppscript );
   STDMETHODIMP Close(void);
   STDMETHODIMP GetCurrentScriptThreadID( SCRIPTTHREADID *pstidThread );
   STDMETHODIMP GetScriptDispatch( LPCOLESTR pstrItemName, IDispatch **ppdisp );
   STDMETHODIMP GetScriptSite( REFIID iid, void **ppvSiteObject );
   STDMETHODIMP GetScriptState( SCRIPTSTATE *pss );
   STDMETHODIMP GetScriptThreadID( DWORD dwWin32ThreadID, SCRIPTTHREADID *pstidThread );
   STDMETHODIMP GetScriptThreadState( SCRIPTTHREADID stidThread, SCRIPTTHREADSTATE *pstsState );
   STDMETHODIMP InterruptScriptThread( SCRIPTTHREADID stidThread, const EXCEPINFO *pexcepinfo, DWORD dwFlags );
   STDMETHODIMP SetScriptSite( IActiveScriptSite *pScriptSite );
   STDMETHODIMP SetScriptState( tagSCRIPTSTATE ss );

   /***** IActiveScriptParse Methods *****/
   STDMETHODIMP InitNew(void);
   STDMETHODIMP AddScriptlet( LPCOLESTR pstrDefaultName, LPCOLESTR pstrCode, LPCOLESTR pstrItemName, LPCOLESTR pstrSubItemName, LPCOLESTR pstrEventName, 
                                      LPCOLESTR pstrDelimiter, DWORD dwSourceContextCookie, ULONG ulStartingLineNumber, DWORD dwFlags, BSTR *pbstrName, EXCEPINFO *pexcepinfo );
   STDMETHODIMP ParseScriptText( LPCOLESTR pstrCode, LPCOLESTR pstrItemName, IUnknown *punkContext, LPCOLESTR pstrDelimiter, DWORD dwSourceContextCookie,
                                         ULONG ulStartingLineNumber, DWORD dwFlags, VARIANT *pvarResult, EXCEPINFO *pexcepinfo );

   /***** IActiveScriptParseProcedure Methods *****/
   STDMETHODIMP ParseProcedureText( LPCOLESTR pstrCode, LPCOLESTR pstrFormalParams, LPCOLESTR pstrProcedureName, LPCOLESTR pstrItemName, IUnknown *punkContext,
                                            LPCOLESTR pstrDelimiter, DWORD dwSourceContextCookie, ULONG ulStartingLineNumber, DWORD dwFlags, IDispatch **ppdisp);

   /***** IHostInfoUpdate Methods *****/
   STDMETHODIMP UpdateInfo(hostinfo hostinfoNew);

   /***** IObjectSafety Methods *****/
   STDMETHODIMP GetInterfaceSafetyOptions( REFIID riid, DWORD *pdwSupportedOptions, DWORD *pdwEnabledOptions);
   STDMETHODIMP SetInterfaceSafetyOptions( REFIID riid, DWORD dwOptionSetMask, DWORD dwEnabledOptions);

   /***** Utility Methods *****/
   CASInterpreter* CreateInterpreter( LPCOLESTR pstrName );
   void DisconnectEvents();
   void ConnectEvents();
   HRESULT ExecuteImmediateScripts();
   STDMETHODIMP GetNamedItemIDispatch( LPCOLESTR pstrItemName, LPCOLESTR pstrSubItemName, IDispatch** ppDispatch );
   STDMETHODIMP Reset( BOOL bReleaseScriptSite );
};

#endif
