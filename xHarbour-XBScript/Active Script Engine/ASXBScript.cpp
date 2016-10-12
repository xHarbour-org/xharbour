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
#define XBSCRIPT
#include "activeDepends.h"

static OLECHAR s_wID[32];
static int s_iID_Index = 0;

BOOL g_bErrors = FALSE;

int XBScript::s_m_ID = 0;

//Constructor
XBScript::XBScript()
{
   //tracing purposes only
   OutputDebugValues( "!!!(%i) XBScript::XBScript\n", s_m_ID );

   m_ID = s_m_ID++;

   m_refCount = 1;

   m_pNamedItems     = new TList<NamedItem*>;
   m_pASInterpreters = new TList<CASInterpreter*>;
   m_pEventHandlers  = new TList<CEventHandler*>;
   
   m_pErrorHandler   = new CASErrorHandler;

   m_pScriptSite = NULL;
   m_pGlobalInterpreter = NULL;
   
   m_ScriptState = SCRIPTSTATE_UNINITIALIZED;

   // Block GC execpet when explcitly allowed - in CASInterpreter::Reset()
   hb_gcSetCollecting( TRUE );
}

//Destructor
XBScript::~XBScript()
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("~~~XBScript::XBScript\n");

   _ASSERTE( m_refCount == 0 );

   if( m_pScriptSite )
   {
      m_pScriptSite->Release();
      m_pScriptSite = NULL;
   }

   Close();

   delete m_pNamedItems;
   delete m_pASInterpreters;
   delete m_pEventHandlers;

   _ASSERT( m_pErrorHandler->Release() == 0 );
   m_pErrorHandler = NULL;

   SAMPLESCRIPTTRACE("DONE ~~~XBScript::XBScript\n");
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
STDMETHODIMP XBScript::QueryInterface(REFIID riid, void ** ppvObj)
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::QueryInterface->");

   if (riid == IID_IUnknown)
   {
      SAMPLESCRIPTTRACE("IUnknown\n");
      *ppvObj = static_cast<IActiveScript*>(this);
   }
   else if (riid == IID_IActiveScript)
   {
      SAMPLESCRIPTTRACE("IActiveScript\n");
      *ppvObj = static_cast<IActiveScript*>(this);
   }
   else if (riid == IID_IActiveScriptParse)
   {
      SAMPLESCRIPTTRACE("IActiveScriptParse\n");
      *ppvObj = static_cast<IActiveScriptParse*>(this);
   }
   else if (riid == IID_IActiveScriptParseProcedure)
   {
      SAMPLESCRIPTTRACE("IActiveScriptParseProcedure\n");
      *ppvObj = static_cast<IActiveScriptParseProcedure*>(this);
   }
   else if (riid == IID_IHostInfoUpdate)
   {
      SAMPLESCRIPTTRACE("IHostInfoUpdate\n");
      *ppvObj = static_cast<IHostInfoUpdate*>(this);
   }
   else if (riid == IID_IObjectSafety)
   {
      SAMPLESCRIPTTRACE("IObjectSafety\n");
      *ppvObj = static_cast<IObjectSafety*>(this);
   }
   else
   {
      SAMPLESCRIPTTRACE("Unsupported Interface\n");
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
STDMETHODIMP_(ULONG) XBScript::AddRef()
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::AddRef\n");

   g_cLock++;

   m_refCount++;

   OutputDebugValues( "Ref: %i, Global: %i\n", m_refCount, g_cLock );

   return m_refCount;
}

/******************************************************************************
*   Release() -- When a reference to this object is removed, this function
*   decrements the reference count.  If the reference count is 0, then this
*   function deletes this object and returns 0;
******************************************************************************/
STDMETHODIMP_(ULONG) XBScript::Release()
{
   //tracing purposes only
   OutputDebugValues( ">>>(%i, %i)XBScript::Release\n", m_refCount, g_cLock );

   g_cLock--;
   m_refCount--;

   _ASSERTE( m_refCount >= 0 );

   if( m_refCount == 0 )
   {
      delete this;
      OutputDebugValues( "<<<DELETED!\n" );
      return 0;
   }
   else
   {
      OutputDebugValues( "<<<RELEASE Ref: %i, Global: %i\n", m_refCount, g_cLock );
   }

   return m_refCount;
}

/******************************************************************************
*  IActiveScript interface -- The IActiveScript interface is the primary
*  interface for an ActiveX Script Engine.  It controls the object model the
*  engine can use in its scripts.  IActiveScript also give the host control
*  over the running of scripts, from starting scripts to controlling the script
*  thread.
******************************************************************************/

/******************************************************************************
*  AddNamedItem -- This method allows the host to add a named item to the
*  script engine's global namespace.  This allows the engine to automate
*  objects through the GetItemInfo() method of the host.  Code added to the
*  engine with AddScriptlet or ParseScriptText where pstrItemName is not NULL
*  must use an Item name previously added with AddNamedItem.
*
*  Parameters: pstrName -- The name to be added to the engine's namespace
*              dwFlags -- SCRIPTITEM_CODEONLY
*                         SCRIPTITEM_GLOBALMEMBERS
*                         SCRIPTITEM_ISPERSISTENT
*                         SCRIPTITEM_ISSOURCE
*                         SCRIPTITEM_ISVISIBLE
*                         SCRIPTITEM_NOCODE
*                         SCRIPTITEM_ALL_FLAGS
*  Returns: S_OK
*           E_INVALIDARG
*           E_POINTER
*           E_UNEXPECTED
******************************************************************************/
STDMETHODIMP XBScript::AddNamedItem( LPCOLESTR pstrName, DWORD dwFlags )
{
   char sGlobal[256];

	if( pstrName )
	{
       unsigned int uLen = wcslen( pstrName ) + 1;

       WideCharToMultiByte( CP_ACP, WC_COMPOSITECHECK, pstrName, uLen, (char *) sGlobal, uLen, NULL, NULL);

  	   OutputDebugValues( "XBScript::AddNamedItem('%s', %08lx)\n", sGlobal, dwFlags );
	}
	else
	{
	   OutputDebugValues( "!!!NO NAME!!!\n" );
	}

	typedef struct
	{
		DWORD f;
		char *name;
	} Flags;

	const Flags flags[] =
	{
		{SCRIPTITEM_CODEONLY, "SCRIPTITEM_CODEONLY"},
		{SCRIPTITEM_GLOBALMEMBERS, "SCRIPTITEM_GLOBALMEMBERS"},
		{SCRIPTITEM_ISPERSISTENT, "SCRIPTITEM_ISPERSISTENT"},
		{SCRIPTITEM_ISSOURCE, "SCRIPTITEM_ISSOURCE"},
		{SCRIPTITEM_ISVISIBLE, "SCRIPTITEM_ISVISIBLE"},
		{SCRIPTITEM_NOCODE, "SCRIPTITEM_NOCODE"},
	};

	for (unsigned i = 0; i < sizeof(flags)/sizeof(flags[0]); i++)
	{
		if (dwFlags & flags[i].f)
		{
			OutputDebugValues( "\t%s\n", flags[i].name );
		}
	}

   OutputDebugValues( "Adding: '%s'\n", sGlobal );

#if 0
   //First, check the flags to make sure we don't have conflicts.  We don't
   //support SCRIPTITEM_ALL_FLAGS
   if( ( dwFlags & SCRIPTITEM_CODEONLY ) && ( dwFlags & ( SCRIPTITEM_GLOBALMEMBERS | SCRIPTITEM_ISSOURCE | SCRIPTITEM_ISVISIBLE | SCRIPTITEM_NOCODE) ) )
   {
      //CODEONLY conflicts with everything but ISPERSISTENT
      return E_INVALIDARG;
   }

   if( ( dwFlags & SCRIPTITEM_ISSOURCE ) && ( dwFlags & SCRIPTITEM_NOCODE ) )
   {
      //ISSOURCE conflicts with NOCODE
      return E_INVALIDARG;
   }
#endif

   //If the named item is NOCODE or GLOBALMEMBERS, we don't create a CASInterpreter
   //for it.
   if( dwFlags & SCRIPTITEM_NOCODE )
   {
      //We don't want to create a NamedItem here because the item may not be
      //visible.  Although NOCODE without ISVISIBLE doesn't make much sense,
      //its not actually a conflict.
   }
   else if( dwFlags & ( SCRIPTITEM_GLOBALMEMBERS | SCRIPTITEM_ISVISIBLE ) )
   {
      //For a GLOBALMEMBERS and ISVISIBLE, we need to create a NamedItem
      //structure and add it to the list of named items.
	   if( m_pNamedItems->FindByName( pstrName ) )
	   {
		   OutputDebugValues( "*** Already VISIBALE '%s'\n", sGlobal );
	   }
	   else
	   {
         NamedItem* pNamedItem = new NamedItem( pstrName, dwFlags, NULL );
	      IDispatch *pDispatch = NULL;

         //Add it to the NamedItems list
         m_pNamedItems->InsertAfter( pNamedItem, pstrName );
	   }

	   if( m_pGlobalInterpreter->m_bAddedGlobals )
		{
 		    OutputDebugValues( "*** Already have Globals - CONCILE '%s'\n", sGlobal );
           	
			 NamedItem* pNamedItem = NULL;
          pNamedItem = m_pNamedItems->Retrieve();

			 m_pGlobalInterpreter->ConcileDispatch( pNamedItem );
		}
   }

   //TODO!!! Review the need for that || part.
   if( (dwFlags & SCRIPTITEM_ISSOURCE) /* || ! (dwFlags & SCRIPTITEM_NOCODE) */ )
   {
      //Create a new CASInterpreter for the script associated with this
      //named item, and add it to the list of CASInterpreters.
      CASInterpreter* pASInterpreter = CreateInterpreter( pstrName );
      m_pASInterpreters->InsertAfter( pASInterpreter, pstrName );
   }

   // If dwFlags contains SCRIPTITEM_ISSOURCE, then we need to create an
   // event handler for this named item.
   if( dwFlags & SCRIPTITEM_ISSOURCE )
   {
      CEventHandler* pEventHandler = new CEventHandler( pstrName );
      m_pEventHandlers->InsertAfter( pEventHandler, pstrName );
   }

   return S_OK;
}

/******************************************************************************
*  AddTypeLib -- This method allows the host to provide a typelib to the engine
*  for constants, named item type information, etc.
*  Parmeters:  guidTypeLib -- the CLSID of the type library
*              dwMaj -- the major version of the type library
*              dwMin -- the minor version of the type library
*              dwFlags -- SCRIPTTYPELIB_ISCONTROL
*                         SCRIPTTYPELIB_ISPERSISTENT
*                         SCRIPTTYPELIB_ALL_FLAGS
*  Returns: S_OK
*           E_INVALIDARG
*           E_UNEXPECTED
*           TYPE_E_CANTLOADLIBRARY
******************************************************************************/
STDMETHODIMP XBScript::AddTypeLib( REFGUID guidTypeLib, DWORD dwMaj, DWORD dwMin,
                                 DWORD dwFlags )
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::AddTypeLib\n");

   //currently, we don't support AddTypeLib in this engine.
   return E_NOTIMPL;
}

/******************************************************************************
*  Clone -- This method clones this script engine, returning a new script
*  engine which is identical to this one, if it were transitioned back to the
*  initialized state.  All named items added with SCRIPTITEM_ISPERSISTENT, all
*  typelibs added with SCRIPTTYPELIB_ISPERSISTENT, and all script added with
*  SCRIPTTEXT_ISPERSISTENT are carried over to the new engine.  However,
*  the new engine must have a host added with SetScriptSite before execution
*  can begin.
*
*  Parameters: ppscript -- Address which receives a pointer to the new script
*                          engine.
*
*  Returns: S_OK
*           E_NOTIMPL
*           E_POINTER
*           E_UNEXPECTED
******************************************************************************/
STDMETHODIMP XBScript::Clone( IActiveScript **ppscript )
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("-------------  XBScript::Clone --------------\n");

   //XBScript *Clone = new XBScript;
   
   //Clone->

   if( ppscript && *ppscript )
   {
      *ppscript = NULL;
   }

   //currently, wd don't support Cloning this script engine.
   return E_NOTIMPL;
}

/******************************************************************************
*  GetScriptSite -- This method returns the site object associated with this
*  engine.  It returns the result of a QueryInterface on m_pScriptSite with
*  the specified iid.
******************************************************************************/
STDMETHODIMP XBScript::GetScriptSite( REFIID iid, void **ppvSiteObject )
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::GetScriptSite\n");

   return m_pScriptSite->QueryInterface( iid, ppvSiteObject );
}

/******************************************************************************
*  GetScriptState -- This method gets the current state of the engine.
*
*  Parameters: pss -- address of the variable containing the current state of
*                     the engine.
*
*  Returns: S_OK
*           E_POINTER
******************************************************************************/
STDMETHODIMP XBScript::GetScriptState( SCRIPTSTATE *pss )
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::GetScriptState\n");

   *pss = m_ScriptState;

   return S_OK;
}


/******************************************************************************
*  SetScriptSite -- This method informs the engine of the IActiveScriptSite
*  provided by the host.  It must be called before any other IActiveScript
*  method may be used.
*  Returns: S_OK
*           E_FAIL
*           E_INVALIDARG
*           E_POINTER
*           E_UNEXPECTED
*******************************************************************************/
STDMETHODIMP XBScript::SetScriptSite( IActiveScriptSite *pScriptSite )
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::SetScriptSite\n");

   _ASSERT( m_pScriptSite == NULL );

   if( m_pScriptSite )
   {
      OutputDebugValues( "*** ALREADY HAS Site: %p!!!\n", m_pScriptSite );
      m_pScriptSite->Release();
      m_pScriptSite = NULL;
   }

   m_pScriptSite = pScriptSite;
   m_pScriptSite->AddRef();

   //Set the ScriptSite for the NamedItems
   NamedItem::SetScriptSite( pScriptSite );

   //Set the ScriptSite for the error handler
   m_pErrorHandler->SetScriptSite( pScriptSite );

   //Get the LCID to use for Error messages
   LCID theLCID;
   m_pScriptSite->GetLCID( &theLCID );

   //Set the error handler's LCID
   m_pErrorHandler->SetLocale( theLCID );

   //Set the state to Initialized
   SetScriptState( SCRIPTSTATE_INITIALIZED );

   return S_OK;
}

/******************************************************************************
*  SetScriptState -- This method moves the engine to the given state.  If
*  there are intervening states between the current engine state and the
*  requested state, the engine transitions through those states.
*
*  Parameter:  ss -- SCRIPTSTATE_UNINITIALIZED
*                    SCRIPTSTATE_INITIALIZED
*                    SCRIPTSTATE_STARTED
*                    SCRIPTSTATE_CONNECTED
*                    SCRIPTSTATE_DISCONNECTED
*                    SCRIPTSTATE_CLOSED
*  Returns: S_OK
*           E_FAIL
*           E_UNEXPECTED
*           OLESCRIPT_S_PENDING
*           S_FALSE
******************************************************************************/
HRESULT XBScript::SetScriptState( tagSCRIPTSTATE ss )
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::SetScriptState\n");

   /*
     enum tagSCRIPTSTATE
     { 
          SCRIPTSTATE_UNINITIALIZED = 0,
	       SCRIPTSTATE_INITIALIZED   = 5,
	       SCRIPTSTATE_STARTED       = 1,
	       SCRIPTSTATE_CONNECTED     = 2,
	       SCRIPTSTATE_DISCONNECTED  = 3,
	       SCRIPTSTATE_CLOSED        = 4
     }SCRIPTSTATE;
   */

   OutputDebugValues( "::SetScriptState(%08lx) %08lx %p\n", (DWORD) ss, (DWORD) m_ScriptState, m_pScriptSite );

   //If we are already in the requested state, do nothing and return S_OK;
   if( ss == m_ScriptState )
   {
      return S_OK;
   }

   //If we are in a Closed state, then this method shouldn't have been called
   //so return E_UNEXPECTED.
   if ( m_ScriptState == SCRIPTSTATE_CLOSED )
   {
      return E_UNEXPECTED;
   }

   HRESULT hr = S_OK;

   //Otherwise, transition from the current state to the requested state.
   //Switch statements allow us to fall through intermediate states to the
   //desired state.
   switch(ss) 
   {
      case SCRIPTSTATE_UNINITIALIZED:
         switch(m_ScriptState)
         {
            case SCRIPTSTATE_CONNECTED:
               //Disconnect from any event sinks
               DisconnectEvents();
            case SCRIPTSTATE_DISCONNECTED:
            case SCRIPTSTATE_STARTED:
            case SCRIPTSTATE_INITIALIZED:
               //Reset the engine
               Reset( TRUE );
               m_ScriptState = SCRIPTSTATE_UNINITIALIZED;
               hr = S_OK;
               break;
         }
         break;

      case SCRIPTSTATE_INITIALIZED:
         switch(m_ScriptState)
         {
            case SCRIPTSTATE_CONNECTED:
               //Disconnect from any event sinks
               DisconnectEvents();
            case SCRIPTSTATE_DISCONNECTED:
            case SCRIPTSTATE_STARTED:
               //Reset the engine
               Reset( FALSE );
            case SCRIPTSTATE_UNINITIALIZED:
               if( this->m_pScriptSite == NULL )
               {
                  return E_UNEXPECTED;
               }

               m_ScriptState = SCRIPTSTATE_INITIALIZED;
               m_pScriptSite->OnStateChange( m_ScriptState );
               hr = S_OK;
               break;
         }
         break;

      case SCRIPTSTATE_STARTED:
         switch(m_ScriptState)
         {
            case SCRIPTSTATE_UNINITIALIZED:
               SetScriptState( SCRIPTSTATE_INITIALIZED );

            case SCRIPTSTATE_CONNECTED:
               //Disconnect from any event sinks
               DisconnectEvents();

            case SCRIPTSTATE_DISCONNECTED:
               //Release any interface pointers

            case SCRIPTSTATE_INITIALIZED:
               //Connect interface pointers

               //Execute immediate scripts
               m_ScriptState = SCRIPTSTATE_STARTED;
               m_pScriptSite->OnStateChange( m_ScriptState );
               hr = ExecuteImmediateScripts();
               break;
         }
         break;

      case SCRIPTSTATE_CONNECTED:
         hr = S_OK;
         
         switch(m_ScriptState) 
         {
            case SCRIPTSTATE_UNINITIALIZED:
            case SCRIPTSTATE_INITIALIZED:
               //Execute immediate scripts
               m_ScriptState = SCRIPTSTATE_STARTED;
               m_pScriptSite->OnStateChange( m_ScriptState );
               hr = ExecuteImmediateScripts();

            case SCRIPTSTATE_STARTED:
               //Connect interface pointers
            case SCRIPTSTATE_DISCONNECTED:
               //Connect any event sinks
               ConnectEvents();
               m_ScriptState = SCRIPTSTATE_CONNECTED;
               m_pScriptSite->OnStateChange( m_ScriptState );
               break;
         }
         break;

      case SCRIPTSTATE_DISCONNECTED:
         switch(m_ScriptState) 
         {
            case SCRIPTSTATE_CONNECTED:
               //Disconnect from any event sinks
               DisconnectEvents();
            case SCRIPTSTATE_STARTED:
            case SCRIPTSTATE_INITIALIZED:
            case SCRIPTSTATE_UNINITIALIZED:
               m_ScriptState = SCRIPTSTATE_DISCONNECTED;
               m_pScriptSite->OnStateChange( m_ScriptState );
               hr = S_OK;
               break;
         }
         break;

      case SCRIPTSTATE_CLOSED:
         switch(m_ScriptState) 
         {
            case SCRIPTSTATE_CONNECTED:
               //Disconnect from any event sinks
               DisconnectEvents();
            case SCRIPTSTATE_DISCONNECTED:
            case SCRIPTSTATE_STARTED:
               //Release any interface pointers
            case SCRIPTSTATE_INITIALIZED:
            case SCRIPTSTATE_UNINITIALIZED:
               Reset( TRUE );  
               m_ScriptState = SCRIPTSTATE_CLOSED;
               //m_pScriptSite->OnStateChange( m_ScriptState );
               hr = S_OK;
               break;
         }
         break;

      default:
        break;
   }

   OutputDebugValues( "SetScriptState returns: %p\n", hr );

   return hr;
}

/******************************************************************************
*  GetScriptDispatch -- This method allows the host to access dynamically
*  created variables and members found in the script.  These members are
*  wrapped in an IDispatchEx* interface.
*
*  Parameters: pstrItemName -- the name of the item the host needs an IDispatch
*                              interface for.
*              ppDisp -- address of the pointer to the IDispatch interface.
*
*  Returns: S_OK
*           E_INVALIDARG
*           E_POINTER
*           E_UNEXPECTED
*           S_FALSE
******************************************************************************/
STDMETHODIMP XBScript::GetScriptDispatch( LPCOLESTR pstrItemName, IDispatch **ppdisp )
{
   if( pstrItemName )
   {
      unsigned int uLen = wcslen( pstrItemName ) + 1;

      char sName[256];

      WideCharToMultiByte( CP_ACP, WC_COMPOSITECHECK, pstrItemName, uLen, (char *) sName, uLen, NULL, NULL);

      OutputDebugValues( "XBScript::GetScriptDispatch(%s)\n", (char *) sName );
   }
   else
   {
      OutputDebugValues( "XBScript::GetScriptDispatch(null)\n" );
   }

   //Our CASInterpreters implement IDispatch and IDispatchEx so we can use them
   //to make calls from outside the engine.  Find the CASInterpreter that the
   //caller wants and return it.
   HRESULT hr = S_FALSE;
   CASInterpreter* pASInterpreter = NULL;

   //If the pstrItemName is NULL, then return the global code block.
   //Otherwise, find the code block with the specified name and return that.
   if( pstrItemName == NULL )
   {
      hr = m_pGlobalInterpreter->QueryInterface( IID_IDispatch, (void**)ppdisp );
   }
   else if (m_pASInterpreters->FindByName( pstrItemName ))
   {
      pASInterpreter = m_pASInterpreters->Retrieve();
      hr = pASInterpreter->QueryInterface( IID_IDispatch, (void**)ppdisp );
   }
   else
   {
      OutputDebugValues( "*** FAILED XBScript::GetScriptDispatch()\n" );
      *ppdisp = NULL;
   }

   //return the result of this call
   return hr;
}

/******************************************************************************
*  Close -- This method causes the engine to complete any immediate script
*  event sinks, and macro invocations already in progress, release any
*  interface pointers it may have, and abandon any script it may have, thus
*  entering a closed state.  This method must be called before releasing the
*  engine to avoid reference counting problems.
*
*  Parameters: none
*
*  Returns: S_OK
*           E_UNEXPECTED
*           OLESCRIPT_S_PENDING
*           S_FALSE
******************************************************************************/
STDMETHODIMP XBScript::Close(void)
{
   //tracing purposes only
   SAMPLESCRIPTTRACE(">>>XBScript::Close\n");

   //Set the scriptstate to closed.  This gives the engine a chance to clean
   //itself up, no matter what state it's currently in.
   
   SetScriptState( SCRIPTSTATE_CLOSED );

   SAMPLESCRIPTTRACE("XBScript::Close->AFTER State\n");

   m_pNamedItems->RemoveAll();

   SAMPLESCRIPTTRACE("XBScript::Close->AFTER NamedItems\n");

   m_pEventHandlers->ReleaseAll();

   SAMPLESCRIPTTRACE("XBScript::Close->AFTER Events\n");

   m_pASInterpreters->ReleaseAll();

   SAMPLESCRIPTTRACE("<<<XBScript::Close->AFTER Intrepreters\n");

   //Release the IActiveScriptSite pointer
   if (m_pScriptSite != NULL)
   {
      m_pScriptSite->Release();
      m_pScriptSite = NULL;
   }

   SAMPLESCRIPTTRACE("XBScript::Close->AFTER Site->Release\n");

   return S_OK;
}

/******************************************************************************
*  GetCurrentScriptThreadID -- This method gets an engine defined identifier
*  that the host can use to refer to the current script thread.
*
*  Parameters: pstidThread -- address of a SCRIPTRHREADID which refers to
*                             the current script thread.
*
*  Returns: S_OK
*           E_POINTER
******************************************************************************/
STDMETHODIMP XBScript::GetCurrentScriptThreadID( SCRIPTTHREADID *pstidThread )
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::GetCurrentScriptThreadID\n");

   *pstidThread = GetCurrentThreadId();
   return S_OK;
}

/******************************************************************************
*  GetScriptThreadID -- This method gets the engine-defined identifier
*  associated with given Win32 thread ID.  In this engine, simply returns a
*  call to GetCurrentScriptThreadID.
*
*  Parameters: dwWin32ThreadID -- id of a running Win32 thread in the current
*                                 process.
*              pstidThread -- address of the variable which receives the engine
*                             defined thread id.
*
*  Returns: S_OK
*           E_POINTER
*           E_UNEXPECTED
******************************************************************************/
STDMETHODIMP XBScript::GetScriptThreadID( DWORD dwWin32ThreadID,
                                        SCRIPTTHREADID *pstidThread )
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::GetScriptThreadID\n");

   //just pass on the call to GetCurrentScriptThreadID
   return GetCurrentScriptThreadID( pstidThread );
}

/******************************************************************************
*  GetScriptThreadState -- This method returns the state of the given script
*  thread.
*
*  Parameters: stidThread -- SCRIPTTHREADID_BASE
*                            SCRIPTTHREADID_CURRENT
*              pstsState -- address of the variable which receives the state
*                           of the given script thread.
*  Returns: S_OK
*           E_POINTER
*           E_UNEXPECTED
******************************************************************************/
STDMETHODIMP XBScript::GetScriptThreadState( SCRIPTTHREADID stidThread,
                                           SCRIPTTHREADSTATE *pstsState )
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::GetScriptThreadState\n");

   //TODO!!!
   *pstsState = SCRIPTTHREADSTATE_NOTINSCRIPT;
   return S_OK;
}

/******************************************************************************
*  InterruptScriptThread -- Interrupts the execution of a running script thread
*  (an event sink, an immediate execution, or a macro invocation). This method
*  can be used to terminate a script that is stuck (in an infinite loop,
*  for example). It can be called from non-base threads without resulting in a
*  non-base callout to host objects or to the IActiveScriptSite method.
*
*  Parameter:  stidThread -- SCRIPTTHREADID_ALL
*                            SCRIPTTHREADID_BASE
*                            SCRIPTTHREADID_CURRENT
*              pexcepinfo -- Address of an EXCEPINFO structure that receives
*                            error info.
*              dwFlags -- SCRIPTINTERRUPT_DEBUG
*                         SCRIPTINTERRUPT_RAISEEXCEPTION
*
*  Returns: S_OK
*           E_INVALIDARG
*           E_POINTER
*           E_UNEXPECTED
******************************************************************************/
STDMETHODIMP XBScript::InterruptScriptThread( SCRIPTTHREADID   stidThread,
                                            const EXCEPINFO *pexcepinfo,
                                            DWORD dwFlags )
{
/*
   #define SCRIPTTHREADID_CURRENT  ((SCRIPTTHREADID)-1)
   #define SCRIPTTHREADID_BASE     ((SCRIPTTHREADID)-2)
   #define SCRIPTTHREADID_ALL      ((SCRIPTTHREADID)-3)

   #define SCRIPTINTERRUPT_DEBUG           0x00000001
   #define SCRIPTINTERRUPT_RAISEEXCEPTION  0x00000002
   #define SCRIPTINTERRUPT_ALL_FLAGS       (SCRIPTINTERRUPT_DEBUG | \
                                         SCRIPTINTERRUPT_RAISEEXCEPTION)

*/
   OutputDebugValues( "XBScript::InterruptScriptThread(%i, %p, %i)\n", stidThread, pexcepinfo, dwFlags );

   if( stidThread == SCRIPTTHREADID_ALL || stidThread == SCRIPTTHREADID_BASE )
   {
      //g_bErrors = TRUE;
      hb_vmRequestBreak( NULL );
   }
   else if( stidThread == SCRIPTTHREADID_CURRENT )
   {
	   //g_ActiveInterpreter->Stop();
	   hb_vmRequestBreak( NULL );
   }

   return S_OK;
}

/******************************************************************************
*  IActiveScriptParse interface -- This interface controls the initialization
*  and addition of script to a script engine.
******************************************************************************/

/******************************************************************************
*  InitNew -- Initializes the script engine.  This must be called before any
*  script is added with AddScriptlet or ParseScriptText.
*  Returns: S_OK
*           E_FAIL
******************************************************************************/
STDMETHODIMP XBScript::InitNew(void)
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::InitNew\n");

   //Create a global CASInterpreter to contain global named-items and script.
   CASInterpreter* pASInterpreter = CreateInterpreter( L"" );
   m_pASInterpreters->InsertAfter( pASInterpreter, L"" );

   m_pGlobalInterpreter = pASInterpreter;

   return S_OK;
}

/******************************************************************************
*  ParseScriptText -- This method parses the given script text, adding names
*  to the engine's namespace and evaluating code as appropriate.
*
*  Parameters: pstrCode -- Address of the script code to evaluate
*              pstrItemName -- Address of the named item that gives this script
*                              its context.
*              punkContext -- Address of the context object.  This item is
*                             reserved for the debugger.
*              pstrDelimiter -- Address of the delimiter the host used to detect
*                               the end of the scriptlet
*              dwSourceContextCookie -- Application defined value for debugging
*              ulStartingLineNumber -- zero-based number defining where parsing
*                                      began.
*              dwFlags -- SCRIPTTEXT_DELAYEXECUTION
*                         SCRIPTTEXT_ISVISIBLE
*                         SCRIPTTEXT_ISEXPRESSION
*                         SCRIPTTEXT_ISPERSISTENT
*                         SCRIPTTEXT_HOSTMANAGESSOURCE
*                         SCRIPTTEXT_ALL_FLAGS
*              pvarResult -- Address of the buffer that receives the result
*                            of scriptlet processiing.
*              pexcepinfo -- Address of the structure that receives error
*                            information.
*  Returns: S_OK
*           DISP_E_EXCEPTION
*           E_INVALIDARG
*           E_POINTER
*           E_NOTIMPL
*           E_UNEXPECTED
*           OLESCRIPT_E_SYNTAX
******************************************************************************/
STDMETHODIMP XBScript::ParseScriptText( LPCOLESTR pstrCode, LPCOLESTR pstrItemName,
                                      IUnknown *punkContext, LPCOLESTR pstrDelimiter,
                                      DWORD dwSourceContextCookie,
                                      ULONG ulStartingLineNumber, DWORD dwFlags,
                                      VARIANT *pvarResult, EXCEPINFO *pexcepinfo )
{
   //tracing purposes only
   OutputDebugValues( "XBScript::ParseScriptText()\n" );

   if( g_bErrors )
   {
      return E_FAIL;
   }

   //If the script engine is currently connected to events, temporarily
   //disconnect.
   BOOL fWasConnected = false;

   if( m_ScriptState == SCRIPTSTATE_CONNECTED )
   {
      SetScriptState( SCRIPTSTATE_DISCONNECTED );
      fWasConnected = true;
   }

   CASInterpreter* pAssocASInterpreter = NULL;
   CEventHandler* pEventHandler = NULL;
   HRESULT hr = S_OK;

   //Retrieve the CASInterpreter associated with this code.  If we have a
   //pstrItemName, find that CASInterpreter.
   if( pstrItemName )
   {
      if( ! m_pASInterpreters->FindByName( pstrItemName ) )
	   {
         OutputDebugValues( "*** FAILED FindByName()\n" );
         return E_INVALIDARG;
	   }

      pAssocASInterpreter = m_pASInterpreters->Retrieve();
      
	   OutputDebugValues( "Interpreter: %s\n", pAssocASInterpreter->m_sName );
	   
      //At this point, we may be adding methods that can sink events, so
      //initialize the relevant event handler.
      if( m_pEventHandlers->FindByName( pstrItemName ) )
	   {
         pEventHandler = m_pEventHandlers->Retrieve();

         //Initialize the event handler, but don't try to initialize the
         //event handler more than once.
         if( ! pEventHandler->IsInitialized() )
		   {
            //Get the CASInterpreter's IDispatch pointer
            IDispatch* pSinkDispatch = NULL;
            pAssocASInterpreter->QueryInterface( IID_IDispatch, (void**)&pSinkDispatch );

            //There is an event handler, so get the two IDispatch pointers we need.
            IDispatch* pSourceDispatch = NULL;

            hr = GetNamedItemIDispatch( pstrItemName, NULL, &pSourceDispatch );

            //Make sure we have two good pointers, and send them to the event
            //handler
            if ((pSourceDispatch) && (pSinkDispatch))
			   {
               hr = pEventHandler->Initialize( pSourceDispatch, pSinkDispatch );
            }

            //The event handler should AddRef the pointers if it caches them, so release these references.
            if( pSourceDispatch )
            {
               pSourceDispatch->Release();
               pSourceDispatch = NULL;
            }

            if( pSinkDispatch )
            {
               pSinkDispatch->Release();
               pSinkDispatch = NULL;
            }
         }
      }
   }
   else
   {
      if( ! m_pGlobalInterpreter )
	   {
         OutputDebugValues( "*** MISSING Global Interpreter!!!\n" );
         return E_FAIL;
	   }

      pAssocASInterpreter = m_pGlobalInterpreter;
   }

   if( ( dwFlags & SCRIPTTEXT_ISEXPRESSION ) != SCRIPTTEXT_ISEXPRESSION )
   {
      //Parse the script text inside the relevant interpreter
	   hr = pAssocASInterpreter->ParseText( pstrCode, ulStartingLineNumber + 1, dwSourceContextCookie );
   }

   //If the script engine was connected to events before this call, then
   //reconnect it to those events
   if( fWasConnected )
   {
      SetScriptState( SCRIPTSTATE_CONNECTED );
   }

   if( hr == DISP_E_EXCEPTION )
	{
	   OutputDebugValues( "DISP_E_EXCEPTION\n" );

		if( pexcepinfo )
		{
		   OutputDebugValues( "Filling Exception info.\n" );
         
         memset( pexcepinfo, 0, sizeof(EXCEPINFO) );

         pexcepinfo->bstrSource       = m_pErrorHandler->m_Operation;
         m_pErrorHandler->m_Operation = NULL;
         pexcepinfo->bstrDescription  = m_pErrorHandler->m_Desc;
         m_pErrorHandler->m_Desc      = NULL;
			pexcepinfo->scode = OLESCRIPT_E_SYNTAX;

         return hr;
	   }
	}

	if( FAILED( hr ) )
	{
	   OutputDebugValues( "FAILED Parse - NO Exception.\n" );
      return hr;
	}

   //If the engine has been started, then execute any immediate code in the code block
   if( (m_ScriptState == SCRIPTSTATE_STARTED) || (m_ScriptState == SCRIPTSTATE_CONNECTED) )
   {	  
	   if( SUCCEEDED( hr ) )
	   {
	      if( SUCCEEDED( hr ) )
	      {
		      if( dwFlags & SCRIPTTEXT_ISEXPRESSION )
			   {
               hr = pAssocASInterpreter->Eval( pstrCode, 0, NULL, pvarResult, FALSE );
			      OutputDebugValues( "EXPRESSION returned type: %i\n", hb_stackReturnItem()->type );
			   }
			   else
			   {
               hr = pAssocASInterpreter->EvaluateImmediate();
			   }
		   }
	   }
   }

   return hr;
}

/******************************************************************************
*  AddScriptlet -- Adds a scriptlet to the engine.  This method is primarily
*  used in HTML based scripting to add event handlers to the engine.  In
*  essense, it's a call to AddNamedItem and ParseScriptText rolled into one.
*
*  Parameters: pstrDefaultName -- Address of the default name of the scriptlet
*              pstrCode -- Address of the scriptlet text
*              pstrItemName -- Address of the name associated with the script
*              pstrSubItemName -- Address of the sub-name associated with the
*                                 script.  These designate the object whose
*                                 event this scriptlet sinks.
*              pstrEventName -- Address of the name of the event this scriptlet
*                               sinks.
*              pstrDelimiter -- Address of the delimeter the host used to detect
*                               the end of the scriptlet
*              dwSourceContextCookie -- Application defined value for debugging
*              ulStartingLineNumber -- zero-based number defining where parsing
*                                      began.
*              dwFlags -- SCRIPTTEXT_ISVISIBLE
*                         SCRIPTTEXT_ISPERSISTENT
*              pbstrName -- named used to identify the scriptlet
*              pexcepinfo -- Address of an EXCEPINFO structure which is filled
*                            with error information.
*  Returns: S_OK
*           DISP_E_EXCEPTION
*           E_INVALIDARG
*           E_NOTIMPL
*           E_POINTER
*           E_UNEXPECTED
*           OLESCRIPT_E_INVALIDNAME
*           OLESCRIPT_E_SYNTAX
******************************************************************************/
STDMETHODIMP XBScript::AddScriptlet( LPCOLESTR pstrDefaultName, LPCOLESTR pstrCode,
                                   LPCOLESTR pstrItemName, LPCOLESTR pstrSubItemName,
                                   LPCOLESTR pstrEventName, LPCOLESTR pstrDelimiter,
                                   DWORD dwSourceContextCookie,
                                   ULONG ulStartingLineNumber, DWORD dwFlags,
                                   BSTR *pbstrName, EXCEPINFO *pexcepinfo )
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::AddScriptlet\n");

   if( pstrDefaultName )
   {
	   OutputDebugValues( "HAVE DEFAULTNAME!\n" );
   }

   if( pstrCode == NULL || pstrItemName == NULL || pstrEventName || *pbstrName )
   {
	  OutputDebugValues( "!!! Invalid Arguments !!!\n" );
      return E_INVALIDARG;
   }

   HRESULT hr;

   //If the script engine is currently connected to events, temporarily disconnect.
   BOOL fWasConnected = false;

   if( m_ScriptState == SCRIPTSTATE_CONNECTED )
   {
      SetScriptState( SCRIPTSTATE_DISCONNECTED );
      fWasConnected = true;
   }

   CASInterpreter* pAssocASInterpreter = NULL;
   CEventHandler* pAssocEventHandler = NULL;

   //Search for an existing CASInterpreter for this scriptlet.  If one isn't
   //available, then return E_INVALIDARG.
   if( ! m_pASInterpreters->FindByName( pstrItemName ) )
   {
      OutputDebugValues( "*** FAILED FindByName()\n" );
      hr = E_INVALIDARG;
   }
   else
   {
      pAssocASInterpreter = m_pASInterpreters->Retrieve();

      OutputDebugValues( "Interpreter: %s\n", pAssocASInterpreter->m_sName );

      //Set up the pstrHandlerName
      LPOLESTR pstrHandlerName = NULL;

      if( pstrSubItemName != NULL )
	  {
         int handlerLength = wcslen(pstrItemName) + wcslen(pstrSubItemName) + 2;
         pstrHandlerName = new WCHAR[handlerLength];

         wcscpy( pstrHandlerName, pstrItemName );
         wcscat( pstrHandlerName, L"_" );
         wcscat( pstrHandlerName, pstrSubItemName );
      }
      else
	   {
         int handlerLength = wcslen(pstrItemName) + 1;
         pstrHandlerName = new WCHAR[handlerLength];

         wcscpy( pstrHandlerName, pstrItemName );
      }

      //Search for an existing CEventHandler for this scriptliet.  If one
      //isn't available then create one.
      if( m_pEventHandlers->FindByName( pstrHandlerName ) )
	   {
         pAssocEventHandler = m_pEventHandlers->Retrieve();
	   }
      else
	   {
         pAssocEventHandler = new CEventHandler( pstrHandlerName );
         m_pEventHandlers->InsertAfter( pAssocEventHandler, pstrHandlerName );
      }

      //Get the CASInterpreter's IDispatch pointer
      IDispatch* pSinkDispatch = NULL;
      pAssocASInterpreter->QueryInterface( IID_IDispatch, (void**)&pSinkDispatch );

      //Make sure the event handler has been initialized, but don't try to
      //initialize the event handler more than once.
      if( ! pAssocEventHandler->IsInitialized() )
	   {
         //There is an event handler, so get the two IDispatch pointers we need.
         HRESULT hr;
         IDispatch* pSourceDispatch = NULL;

         GetNamedItemIDispatch( pstrItemName, pstrSubItemName, &pSourceDispatch );

         //Make sure we have two good pointers, and send them to the event handler
         if ((pSourceDispatch) && (pSinkDispatch))
		   {
            hr = pAssocEventHandler->Initialize( pSourceDispatch, pSinkDispatch );

            //The event handler should AddRef the pointers if it caches them,
            //so release these references.
            pSourceDispatch->Release();
            pSourceDispatch = NULL;

            pSinkDispatch->Release();
            pSinkDispatch = NULL;
         }
      }

      //Create a name for the new event
      int nameLength = wcslen(pstrHandlerName) + wcslen( pstrEventName) + 2;
      LPOLESTR pszEventName = new WCHAR[ nameLength ];

      wcscpy( pszEventName, pstrHandlerName );
      wcscat( pszEventName, L"_" );
      wcscat( pszEventName, pstrEventName );

      //Create a wrapper function
      int eventLength = (wcslen( pszEventName) + wcslen(pstrCode) + 32 );
      LPOLESTR pszEventMethod = new WCHAR[ eventLength ];
         
	   // Note + 32 above, we actually use 21 bytes so far.
      wcscpy( pszEventMethod, L"Procedure " );
      wcscat( pszEventMethod, pszEventName );
      wcscat( pszEventMethod, L"()\n" );
      wcscat( pszEventMethod, pstrCode );
      wcscat( pszEventMethod, L"\nReturn" );

      //Parse the script inside the appropriate interpreter
      hr = pAssocASInterpreter->ParseText( pszEventMethod, ulStartingLineNumber + 1, dwSourceContextCookie);

      //If the engine has been started, then execute any immediate code in the code block
      if( (m_ScriptState == SCRIPTSTATE_STARTED) || (m_ScriptState == SCRIPTSTATE_CONNECTED) )
	   {
		   if( SUCCEEDED( hr ) )
		   {
            hr = pAssocASInterpreter->EvaluateImmediate();
		   }
      }

      //We need to alias the event we just created.  The source object will
      //have an event like "onclick", but internally, we've got an event
      //like pstrItemName_(pstrSubItemName_)pstrEventName.
      pAssocEventHandler->AliasEvent( pstrEventName, pszEventName );

      //Write the name of the event sink into pbstrName
      if( pbstrName )
	   {
         *pbstrName = SysAllocString( pstrEventName );
	   }

      //Clean up all these buffers.
      delete [] pstrHandlerName;
      delete [] pszEventName;
      delete [] pszEventMethod;
   }

   //If the script engine was connected to events before this call, then reconnect it to those events
   if( fWasConnected )
   {
      SetScriptState( SCRIPTSTATE_CONNECTED );
   }
 
   return hr;
}

/******************************************************************************
*  IActiveScriptParseProcedure interface -- This interface allows an Active
*  Script Host to use IDispatch-style function pointers to fire methods instead
*  of using the more difficult method of Connection Points.
******************************************************************************/

/******************************************************************************
*  ParseProcedureText -- This method allows an Active Script Host to use
*  IDispatch-style function pointers to fire methods instead of using the more
*  difficult method of Connection Points.  It parses a scriplet and wraps it in
*  an anonymous IDispatch interface, which the host can use in lieu of
*  Connection Points to handle events.
*
*  Parameters: pstrCode -- Address of the script code to evaluate
*              pstrFormalParams -- Address of any formal parameters to the
*                                  scriptlet. (ignored)
*              pstrProcedureName -- Name of the event
*              pstrItemName -- Address of the named item that gives this
*                              scriptlet its context.
*              punkContext -- Address of the context object.  This item is
*                             reserved for the debugger.
*              pstrDelimiter -- Address of the delimiter the host used to detect
*                               the end of the scriptlet.
*              dwSourceContextCookie -- Application defined value for debugging
*              ulStartingLineNumber -- zero-based number defining where parsing
*                                      began.
*              dwFlags -- SCRIPTPROC_HOSTMANAGESSOURCE
*                         SCRIPTPROC_IMPLICIT_THIS
*                         SCRIPTPROC_IMPLICIT_PARENTS
*                         SCRIPTPROC_ALL_FLAGS
*              ppdisp -- Address of the pointer that receives the IDispatch
*                        pointer the host uses to call this event.
*  Returns: S_OK
*           DISP_E_EXCEPTION
*           E_INVALIDARG
*           E_POINTER
*           E_NOTIMPL
*           E_UNEXPECTED
*           OLESCRIPT_E_SYNTAX
******************************************************************************/
STDMETHODIMP XBScript::ParseProcedureText( LPCOLESTR pstrCode,
      LPCOLESTR pstrFormalParams, LPCOLESTR pstrProcedureName,
      LPCOLESTR pstrItemName, IUnknown *punkContext,
      LPCOLESTR pstrDelimiter, DWORD dwSourceContextCookie,
      ULONG ulStartingLineNumber, DWORD dwFlags, IDispatch **ppdisp)
{
   //tracing purposes only
   OutputDebugValues( "XBScript::ParseProcedureText( <code>, Params: '%s', ProcName: '%s', Item: '%s', ... ) \n", pstrFormalParams, pstrProcedureName, pstrItemName );

   if( pstrCode == NULL || *ppdisp )
   {
      OutputDebugValues( "*** Invalid Arguments!\n" );
	   return E_INVALIDARG;
   }

   HRESULT hr;

   //If the script engine is currently connected to events, temporarily disconnect.
   BOOL fWasConnected = false;
   CASInterpreter* pAssocASInterpreter;

   if( m_ScriptState == SCRIPTSTATE_CONNECTED )
   {
      SetScriptState( SCRIPTSTATE_DISCONNECTED );
      fWasConnected = true;
   }

   if( m_pASInterpreters->FindByName( pstrCode ) )
   {
       pAssocASInterpreter = m_pASInterpreters->Retrieve();
       hr = S_OK;
   }
   else
   {
      //Create an anonymous CASInterpreter to serve as the function pointer
      pAssocASInterpreter = CreateInterpreter( pstrCode );
      m_pASInterpreters->InsertAfter( pAssocASInterpreter, pstrCode );

      //Create a wrapper function for the script text, with the name in
      //pstrEventName.  pstrProcedureName may be empty, so if it is, create
      //a name for the method.
      LPCOLESTR pstrEventName = NULL;

      if( wcscmp( pstrProcedureName, L"" ) == 0 )
      {
         pstrEventName = new WCHAR[64];

         swprintf( (wchar_t *)pstrEventName, 63, L"%s%i", L"defaultEvent", m_pASInterpreters->Size() );
      }
      else
      {
         pstrEventName = new WCHAR[ wcslen( pstrProcedureName ) + 1 ];
         wcscpy( (wchar_t *)pstrEventName, pstrProcedureName );
      }

      // void pstrProcedureName(){ pstrCode }
      LPCOLESTR pszEventMethod = new WCHAR[ wcslen( pstrEventName ) + wcslen( pstrCode ) + ( pstrFormalParams ? wcslen( pstrFormalParams ) : 0 ) + 64 ];

      // Note + 64 above so far using 21 bytes.
      if( dwFlags & SCRIPTPROC_ISEXPRESSION )
      {
         wcscpy( (wchar_t *)pszEventMethod, L"FUNCTION " );
      }
      else
      {
         wcscpy( (wchar_t *)pszEventMethod, L"PROCEDURE " );
      }

      wcscat( (wchar_t *)pszEventMethod, pstrEventName );

      wcscat( (wchar_t *)pszEventMethod, L"(" );
      if( pstrFormalParams )
      {
         wcscat( (wchar_t *)pszEventMethod, pstrFormalParams );
      }
      wcscat( (wchar_t *)pszEventMethod, L")\n" );

      if( dwFlags & SCRIPTPROC_ISEXPRESSION )
      {
         wcscat( (wchar_t *)pszEventMethod, L"RETURN " );
      }
      wcscat( (wchar_t *)pszEventMethod, pstrCode );
      wcscat( (wchar_t *)pszEventMethod, L"\n" );

      //Parse the script inside the interpreter
      hr = pAssocASInterpreter->ParseText( pszEventMethod, ulStartingLineNumber /* + 1 - The added PROCEDURE/FUNCTION wrapper*/, dwSourceContextCookie);

      #if 0
         //In order for this event to be set up as the default property of
         //pAssocASInterpreter, we need to reference it once.  This is just a quirk
         //of CASInterpreter, and not important to scripting.
         DISPID dispid;
      
         hr = pAssocASInterpreter->GetIDsOfNames( IID_NULL, (unsigned short**)&pstrEventName, 1, 0, &dispid );
      #endif

      delete [] pstrEventName;
      delete [] pszEventMethod;
   }

   if( SUCCEEDED( hr ) )
   {
      //QI the interpreter into ppdisp.
      hr = pAssocASInterpreter->QueryInterface( IID_IDispatch, (void**)ppdisp );
   }

   //If the script engine was connected to events before this call, then
   //reconnect it to those events
   if( fWasConnected )
   {
      SetScriptState( SCRIPTSTATE_CONNECTED );
   }

   return hr;
}

/******************************************************************************
*  IHostInfoUpdate -- This interface is included as a concession to IE4 and
*  4.01.  It allows IE to change the LCID's used for messages and error
*  reporting to the user.
******************************************************************************/

/******************************************************************************
*  UpdateInfo -- This method allows a host to request that the engine use
*  the LCID specified by the host's IHostInfoProvider method.  It should
*  QI the host for IHostInfoProvider and call IHostInfoProvider::GetHostInfo.
*
*  Parameters: hostinfoNew -- hostinfoLocale
*                             hostinfoCodePage
*                             hostinfoErrorLocale
*  Returns: S_OK
*           E_INVALIDARG
*           E_UNEXPECTED
*           E_FAIL
******************************************************************************/
STDMETHODIMP XBScript::UpdateInfo( hostinfo hostinfoNew )
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::UpdateInfo\n");

   HRESULT hr = S_OK;

   //Make sure we have an IActiveScriptSite pointer
   if( m_pScriptSite == NULL )
   {
      hr = E_UNEXPECTED;
   }
   else
   {
      void* pvInfo = NULL;

      //QI the host for IHostInfoProvider
      IHostInfoProvider* pHostInfoProvider = NULL;
      hr = m_pScriptSite->QueryInterface( IID_IHostInfoProvider, (void**)&pHostInfoProvider );

      //If we got the inteface, then ask the host for it's information
      if (SUCCEEDED(hr))
	   {
         hr = pHostInfoProvider->GetHostInfo( hostinfoNew, &pvInfo );

         //Release the pHostInfoProvider interface;
         pHostInfoProvider->Release();
         pHostInfoProvider = NULL;
      }

      //If we got the information we needed, then fill in the right values
      if (SUCCEEDED(hr))
	  {
         //Turn the raw data into an LCID
         LCID lcid = *((LCID *)pvInfo);

         switch (hostinfoNew)
		 {
         case hostinfoLocale:
            //Set the LCID for both normal messages and error messages.  Since
            //We don't have any normal messages, so just fall through to set
            //the error messages LCID.

         case hostinfoErrorLocale:
            //Set the LCID for error messages

            //CErrorHandler::SetLocale( lcid );
            break;

         case hostinfoCodePage:
            //We don't understand hostinfoCodePage, so return E_INVALIDARG
            hr = E_INVALIDARG;
            break;

         default:
            hr = E_INVALIDARG;
         }

         //Free up the memory that IHostInfoProvider allocated
         CoTaskMemFree(pvInfo);
      }
   }

   //Return the status of the operation
   return hr;
}

/******************************************************************************
*  IObjectSafety interface -- This interface is included so scripts can be run
*  in Internet Explorer.  In order to maintain the security model of IE, an
*  object must support this interface.
******************************************************************************/

/******************************************************************************
*  GetInterfaceSafetyOptions -- This method returns the safety options
*  supported by the script engine.  Since some supported options may not be
*  currently enabled, this method also returns the currently enabled options.
******************************************************************************/
STDMETHODIMP XBScript::GetInterfaceSafetyOptions( REFIID riid,
     DWORD *pdwSupportedOptions, DWORD *pdwEnabledOptions)
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::GetInterfaceSafetyOptions\n");

   //For now, we only return information about the engine itself, so we can
   //ignore the riid.

   //Reset the values of the flags to make sure we don't OR with garbage.
   *pdwSupportedOptions = 0;
   *pdwEnabledOptions = 0;

   //OR in the options that the engine supports and has enabled.
   *pdwSupportedOptions = *pdwEnabledOptions |=
      (INTERFACESAFE_FOR_UNTRUSTED_CALLER |
      INTERFACESAFE_FOR_UNTRUSTED_DATA |
      INTERFACE_USES_DISPEX |
      INTERFACE_USES_SECURITY_MANAGER );

   //All done...
   return S_OK;
}

/******************************************************************************
*  SetInterfaceSafetyOptions -- This method sets the safety options that the
*  host wants enabled on the script engine.  Objects that support IObjectSafety
*  should check the dwOptionSetMask to make sure they support the options
*  requested.
******************************************************************************/
STDMETHODIMP XBScript::SetInterfaceSafetyOptions( REFIID riid,
     DWORD dwOptionSetMask, DWORD dwEnabledOptions)
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::SetInterfaceSafetyOptions\n");

   //For now, we only set information about the engine itself, so we can
   //ignore the riid.

   //Check dwOptionSetMask to be sure we support the requested options.
   //This is somewhat redundant since we support all options, but it's good
   //anyway.  The Exclusive OR finds differences between the flags we support
   //and dwOptionSetMask, and the AND makes sure that we support more flags
   //that dwOptionSetMask, not less.
   if(dwOptionSetMask & (dwOptionSetMask ^ (INTERFACESAFE_FOR_UNTRUSTED_CALLER
      | INTERFACESAFE_FOR_UNTRUSTED_DATA | INTERFACE_USES_DISPEX |
      INTERFACE_USES_SECURITY_MANAGER ) ) )
   {
      OutputDebugValues( "*** UNSPPORTED Safety\n" );      
	   //Somehow, we got a request for a flag we don't support
      return E_FAIL;
   }

   //Here we would set the enabled options, but since our options are
   //unchangeable, we just return S_OK;
   return S_OK;
}

/******************************************************************************
*  Utility Methods -- These methods iterate through all the CASInterpreters in
*  the engine and tell them to perform various actions.  They are implemented
*  generally the same way.
******************************************************************************/

/******************************************************************************
*  CreateInterpreter -- This method creates a new CASInterpreter and intializes
*  it with the given name.  This method serves primarily to ease the extension
*  of the CASIntepreter class for debugging.
*  Parameters: pstrName -- LPCOLESTR to initialize the interpreter with
*  Returns: none
******************************************************************************/
CASInterpreter* XBScript::CreateInterpreter( LPCOLESTR pstrName )
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::CreateInterpreter\n");

   if( pstrName == NULL )
   {
      pstrName = _itow( s_iID_Index, s_wID, 10 );
	   s_iID_Index++;	  
   }

   CASInterpreter* pInterpreter = new CASInterpreter( pstrName, this );

   return pInterpreter;
}

/******************************************************************************
*  DisconnectEvents() -- This method iterates through all the CEventHandlers
*  and tells them to attach to their events.
******************************************************************************/
void XBScript::DisconnectEvents()
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::DisconnectEvents\n");

   //Iterate through the list of event handlers and tell them to disconnect.
   CEventHandler* pEventHandler = NULL;

   PFOREACH( m_pEventHandlers,
      pEventHandler = m_pEventHandlers->Retrieve();
      pEventHandler->DisconnectEvents();
   );
}

/******************************************************************************
*  ConnectEvents() -- This method tells the CASInterpreters to iterate through
*  their symbols and connect any event sinks to their sources.
******************************************************************************/
void XBScript::ConnectEvents()
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::ConnectEvents\n");

   m_pGlobalInterpreter->ConcileNamedItems( m_pNamedItems );

   //Iterate through the list of event handlers and tell them to connect.
   CEventHandler* pEventHandler = NULL;

   PFOREACH( m_pEventHandlers,
      pEventHandler = m_pEventHandlers->Retrieve();
      pEventHandler->ConnectEvents();
   );
}

/******************************************************************************
*  ExecuteImmediateScripts() -- This method iterates through the CASInterpreters
*  and Execute any pending code.
******************************************************************************/
HRESULT XBScript::ExecuteImmediateScripts()
{
   //tracing purposes only
   OutputDebugValues( "(%i) XBScript::ExecuteImmediateScripts\n", m_ID );

   CASInterpreter* pCurrentASInterpreter = NULL;

   if( m_pGlobalInterpreter == NULL )
	{
       OutputDebugValues( "*** MISSING Global Interpreter!!!\n" );
       return E_FAIL;
	}

   m_pGlobalInterpreter->ConcileNamedItems( m_pNamedItems );
   return m_pGlobalInterpreter->EvaluateImmediate();
}

/******************************************************************************
*  GetNamedItemIDispatch -- This method retrieves the IDispatch pointer
*  associated with a name added IActiveScript::AddNamedItem.
*  Parameters: pstrItemName -- The name added with AddNamedItem
*              pstrSubItemName -- The name of a subitem within pstrItemName. If
*                                 specified, the IDispatch of the subitem will
*                                 be returned.
*              ppDispatch -- Address of the IDispatch pointer to return
*  Returns: S_OK
*           E_INVALIDARG
*           E_POINTER
*           TYPE_E_ELEMENTNOTFOUND
******************************************************************************/
STDMETHODIMP XBScript::GetNamedItemIDispatch( LPCOLESTR pstrItemName,
                                         LPCOLESTR pstrSubItemName,
                                         IDispatch** ppDispatch )
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::GetNamedItemIDispatch\n");

   HRESULT hr = E_FAIL;

   //Check the arguments to this method first.
   if( pstrItemName == NULL )
   {
      hr = E_INVALIDARG;
   }
   else if( *ppDispatch != NULL )
   {
      hr = E_POINTER;
   }
   else
   {
      IUnknown* punk = NULL;
      IDispatch* pDisp = NULL;

      //Use IActiveScriptSite::GetItemInfo to retrieve the IDispatch of named
      //item
      hr = m_pScriptSite->GetItemInfo( pstrItemName, SCRIPTINFO_IUNKNOWN, &punk, NULL );

      //QI for IDispatch
      if( SUCCEEDED(hr) )
	   {
         hr = punk->QueryInterface( IID_IDispatch, (void**)&pDisp );
         punk->Release();
         punk = NULL;

         if( SUCCEEDED(hr) )
         {
            //If we got the IDispatch pointer and pstrSubItemName isn't NULL,
            //then use GetIDsOfNames/ Invoke to get the SubItem.
            if( pstrSubItemName != NULL )
			   {
               //First, get the DISPID of the name
               DISPID l_dispid;
               hr = pDisp->GetIDsOfNames(IID_NULL, (LPOLESTR *)&pstrSubItemName, 1, 0, &l_dispid );

               //If we found the name, then try to call Invoke
               if (SUCCEEDED(hr))
			      {
                  DISPPARAMS l_dispParams;
                  EXCEPINFO l_exceptions;
                  UINT l_error = 0;

                  VARIANT* pReturnVariant = new VARIANT;
                  VariantInit( pReturnVariant );

                  l_dispParams.rgvarg            = NULL;
                  l_dispParams.rgdispidNamedArgs = NULL;
                  l_dispParams.cArgs             = 0;
                  l_dispParams.cNamedArgs        = 0;

                  hr = pDisp->Invoke( l_dispid, IID_NULL, 0, DISPATCH_PROPERTYGET, &l_dispParams, pReturnVariant, &l_exceptions, &l_error);

                  if (SUCCEEDED(hr) && (pReturnVariant->vt == VT_DISPATCH))
				      {
                     pDisp->Release();
                     pDisp = pReturnVariant->pdispVal;
                  }
               }
            }

            //Copy the IDispatch pointer into ppDispatch.
            pDisp->QueryInterface( IID_IDispatch, (void**)ppDispatch );
            pDisp->Release();
            pDisp = NULL;
         }
      }
   }

   return hr;
}

/******************************************************************************
*  Reset -- This method resets the engine to an initialized state.  Immediate
*  scripts are reset, and all non-persistent named items are removed.
*  Parameters: none
*  Returns: S_OK
******************************************************************************/
STDMETHODIMP XBScript::Reset( BOOL bReleaseScriptSite )
{
   //tracing purposes only
   SAMPLESCRIPTTRACE("XBScript::Reset\n");

   OutputDebugValues( "********* RESET ERRORS\n" );
   g_bErrors = FALSE;

   s_iID_Index = 0;

   //Reset the Global CASInterpreter
   if( m_pGlobalInterpreter )
   {
      m_pGlobalInterpreter->Reset( FALSE );
   }

   if( m_pNamedItems->Size() )
   {
      //scan the list of NamedItems.  If the NamedItem is persistent, then reset
      //it and it's associated CEventHandler and CASInterpreter.  If not, delete them
      NamedItem* pNamedItem = NULL;
      LPCOLESTR pstrItemName = NULL;
      DWORD dwFlags = 0;
      CASInterpreter* pAssocASInterpreter = NULL;
      CEventHandler* pEventHandler = NULL;

      m_pNamedItems->Reset();

      do
      {
         pNamedItem = m_pNamedItems->Retrieve();

         //Get the name of the item and it's flags
         pstrItemName = pNamedItem->GetName();
         dwFlags = pNamedItem->GetFlags();

         //If the NamedItem is persistent...
         if( dwFlags & SCRIPTITEM_ISPERSISTENT )
	      {
            //Release the IDispatch associated with the NamedItem
            pNamedItem->Disconnect();

            //Reset the CASInterpreter
            if( m_pASInterpreters->FindByName( pstrItemName ))
		      {
               pAssocASInterpreter = m_pASInterpreters->Retrieve();
               pAssocASInterpreter->Reset( FALSE );
            }

            //At this point, we may Reset the event handlers.  Release and then
            //re-aquire any event handlers
            if( m_pEventHandlers->FindByName( pstrItemName ))
		      {
               pEventHandler = m_pEventHandlers->Retrieve();

               if( bReleaseScriptSite )
               {
                  _ASSERT( pEventHandler->Release() == 0 );
                  m_pEventHandlers->Remove();
               }
               else
               {
                  //Get the CASInterpreter's IDispatch pointer
                  IDispatch* pSinkDispatch = NULL;
                  pAssocASInterpreter->QueryInterface( IID_IDispatch, (void**)&pSinkDispatch );

                  //There is an event handler, so get the two IDispatch pointers we need.
                  IDispatch* pSourceDispatch = NULL;

                  GetNamedItemIDispatch( pstrItemName, NULL, &pSourceDispatch );

                  //Make sure we have two good pointers, and send them to the event handler
                  if( pSourceDispatch && pSinkDispatch )
			         {
                     pEventHandler->Initialize( pSourceDispatch, pSinkDispatch );

                     //The event handler should AddRef the pointers if it caches
                     //them, so release these references.
                     pSourceDispatch->Release();
                     pSourceDispatch = NULL;

                     pSinkDispatch->Release();
                     pSinkDispatch = NULL;
                  }
               }
            }

		      // Next.
		      ( *m_pNamedItems )++;
         }
         else
	      {
            //Non persitent name.
		 
		      //Reset the CASInterpreter
            if( m_pASInterpreters->FindByName( pstrItemName ) )
		      {
               pAssocASInterpreter = m_pASInterpreters->Retrieve();

               //pAssocASInterpreter->Reset( TRUE );
               pAssocASInterpreter->Release();
               pAssocASInterpreter = NULL;

               m_pASInterpreters->Remove();
            }

		      // Will get Next.
		      m_pNamedItems->Remove();
         }
      }
      while( m_pNamedItems->Size() && pNamedItem != m_pNamedItems->GetLast() );
   }

   //Release the IActiveScriptSite pointer   
   if( bReleaseScriptSite && m_pScriptSite )
   {
      OutputDebugValues( "Releasing Site!!!\n" );
      m_pScriptSite->Release();
      m_pScriptSite = NULL;
      OutputDebugValues( "DONE Releasing Site!!!\n" );
   }   

   // REVIEW!!!
#if 0
   ResetVM();
#endif

   return S_OK;
}

//----------------------------------------------------------------------------//
HB_FUNC( OUTPUTDEBUGSTRING )
{
   #ifdef _DEBUG
      OutputDebugString( hb_parcx(1) );
      OutputDebugString( "\n" );
   #endif
}

//----------------------------------------------------------------------------//
HB_FUNC( GTSYS )
{
}

HB_FUNC( SETKEY )
{
}

HB_FUNC( HB_SETKEYARRAY )
{
}

HB_FUNC( HB_SETKEYGET )
{
}

HB_FUNC( HB_SETKEYSAVE )
{
}

HB_FUNC( HB_SETKEYCHECK )
{
}

HB_FUNC( INKEY )
{
}

HB_FUNC( SETINKEYBEFOREBLOCK )
{
}

HB_FUNC( __KEYBOARD )
{
}

HB_FUNC( HB_KEYPUT )
{
}

HB_FUNC( NEXTKEY )
{
}

HB_FUNC( LASTKEY )
{
}

void HB_EXPORT hb_vmRequestQuit( void )
{
   hb_vmRequestBreak( NULL );
}

void HB_EXPORT hb_errInternal( ULONG ulIntCode, const char * szText, const char * szPar1, const char * szPar2 )
{
   char title[64], buffer[ 256 ];
   FILE *fpError;
   BOOL bLang;

   HB_TRACE(HB_TR_DEBUG, ("hb_errInternal(%lu, %s, %s, %s)", ulIntCode, szText, szPar1, szPar2));

   bLang = ( hb_langID() != NULL );

   if( szText )
   {
      fpError = fopen( "error.log", "w" );

      if( fpError )
      {
         fclose( fpError );
         TraceLog( "error.log", szText, szPar1, szPar2 );
      }
   }

   hb_conOutErr( hb_conNewLine(), 0 );

   sprintf( title, bLang ?
                      ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_ERRINTR ) :
                      "Unrecoverable error %lu: ", ulIntCode );

   hb_conOutErr( title, 0 );

   if( szText != NULL )
   {
      sprintf( buffer, szText, szPar1, szPar2 );
   }
   else if (bLang)
   {
      sprintf( buffer, ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_ERRINTR + ulIntCode - 9000 ), szPar1, szPar2 );
   }

   hb_conOutErr( buffer, 0 );
   hb_conOutErr( hb_conNewLine(), 0 );
   hb_stackDispCall();

   //#ifdef HB_OS_WIN_32
   //   MessageBox( NULL, buffer, title, MB_ICONSTOP );
   //#endif

   /* release console settings */
   //hb_conRelease();

   //if( hb_cmdargCheck( "ERRGPF" ) )
   //{
   //    int *pGPF = NULL;
   //    *pGPF = 0;
   //    *(--pGPF) = 0;
   //}

   #if 0 && defined( HB_THREAD_SUPPORT ) && defined( HB_OS_OS2 )
      /* Post all threads waiting on an indefinite wait */
      DosPostEventSem(hb_hevWakeUpAll);
      /* Let's give them some time to wake up */
      DosSleep(5000);
      /* Stop VM, I cannot call exit() here or I end up with a zombie process */
      hb_vmQuit();
   #endif

   //exit( EXIT_FAILURE );
   g_bErrors = TRUE;
   return;
}

// TODO!
char ** hb_cmdargARGV( void ) /* retrieve command line argument buffer pointer */
{
   static char* argv[1];

   argv[0] = "";

   return argv;
}

HB_FUNC( HB_CMDARGARGV )
{
   hb_retc( hb_cmdargARGV()[0] );
}

void xbScript_atexit( void )
{
   OutputDebugString( "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! EXIT CALLED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" );
   //_ASSERT( 0 );
   hb_stackDispCall();
   g_bErrors = TRUE;
}

#if 0
void hb_conInit( void )
{
}

void hb_conRelease( void )
{
}
#endif

void HB_EXPORT hb_conOutErr( const char * pStr, ULONG ulLen )
{
   OutputDebugString( pStr );
}

void TraceLog( char *sFile, const char *sFormat, ... )
{
    char sBuffer[ 65536 ];
    va_list ap;

    va_start( ap, sFormat );

    _vsnprintf( sBuffer, 65535, sFormat, ap );

    OutputDebugString( sBuffer );

    va_end( ap );
}
