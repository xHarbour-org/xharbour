/******************************************************************************
*
*  File: CASInterpreter.cpp
*
*  Author:  Joel Alley
*
*  Date: October 3, 1998
*
*  Description:   This file contains the definition of the CASInterpreter
*                 class, which serves as the link between a normal script
*                 interpreter and an Active Script Engine.  It adds the
*                 IDispatch and IDispatchEx interfaces, as well as COM
*                 functionality, so other CASInterpreters and external code
*                 can call methods and variables in this interpreter.
*
*  Modifications:
******************************************************************************/
#define CASINTERPRETER
#include "activeDepends.h"

CASInterpreter *g_pInterpreter;

extern BOOL g_bErrors;

static void InitSymbols( void );

//Initialize the static dispid counter so we guarantee our Dispids are unique
int CASInterpreter::sm_CurrentDispid = 0;

static BOOL     s_bInit               = TRUE;

static PHB_DYNS s_pSym_TInterpreter   = NULL ;

static PHB_DYNS s_pSym_TOleAuto       = NULL ;
static PHB_DYNS s_pSym_hObj           = NULL ;
static PHB_DYNS s_pSym_New            = NULL ;

static PHB_DYNS s_pSym_SetScript      = NULL ;
static PHB_DYNS s_pSym_AddText        = NULL ;
static PHB_DYNS s_pSym_Compile        = NULL ;
static PHB_DYNS s_pSym_Run            = NULL ;
static PHB_DYNS s_pSym_EvalExpression = NULL;

static PHB_DYNS s_pSym_ScriptSiteAddGlobal      = NULL;
//static PHB_DYNS s_pSym_ScriptSiteResetGlobals   = NULL;
//static PHB_DYNS s_pSym_IsProcedure              = NULL;

static PHB_DYNS s_pSym_GetLine        = NULL;
static PHB_DYNS s_pSym_SubCode        = NULL;
static PHB_DYNS s_pSym_Operation      = NULL;

#ifdef RESET_VM
   HB_EXPORT PSYMBOLS * hb_vmSymbols( void );
   HB_EXPORT void hb_vmDoExitFunctions( void );
#else
   static PHB_DYNS s_pSym___MVClear      = NULL;
   static PHB_DYNS s_pSym_dbCloseAll     = NULL;
   static PHB_DYNS s_pSym_Reset          = NULL;
#endif

CASInterpreter::CASInterpreter( LPCOLESTR name, XBScript *pEngine )
{
   CASINTERPRETERTRACE( "!!!CASInterpreter::CASInterpreter\n" );

   unsigned int uLen = wcslen( name ) + 1;

   wcscpy( m_Name, name );

   WideCharToMultiByte( CP_ACP, WC_COMPOSITECHECK, name, uLen, (char *) m_sName, uLen, NULL, NULL );

	m_refCount = 1;

	m_pNamedItems = NULL;

	m_pInterpreter = NULL;
   m_pOleWrapDispatch = OleWrap( NULL );
   m_pEngine = pEngine;

   m_sCode = (char *) malloc( MAX_SCRIPT_SIZE );
	m_sCode[0] = 0;

	m_bExecuted     = FALSE;
   m_bAppendText   = FALSE;
	m_bAddedGlobals = FALSE;

	if( s_bInit )
	{
      s_bInit = FALSE;
	   InitSymbols();
	}

   GetPRGInterpreter();
   _ASSERT( m_pInterpreter );

	CASINTERPRETERTRACE("Created Interpreter\n");
}

CASInterpreter::~CASInterpreter()
{
   OutputDebugValues( "(%s)~~~CASInterpreter::CASInterpreter\n", (char *) m_sName );

   _ASSERTE( m_refCount == 0 );

   free( (void *) m_sCode );

   if( m_pInterpreter && HB_IS_OBJECT( m_pInterpreter ) )
   {
      OutputDebugValues( "BEFORE hb_itemClear() - Object Counter: %i\n", m_pInterpreter->item.asArray.value->ulHolders );

      hb_itemRelease( m_pInterpreter );
	   m_pInterpreter = NULL;
   }

   if( m_pOleWrapDispatch )
   {
      m_pOleWrapDispatch->Release();
      m_pOleWrapDispatch = NULL;
   }

   hb_gcSetCollecting( FALSE );
   hb_gcCollectAll( FALSE );
   hb_gcSetCollecting( TRUE );
}

STDMETHODIMP CASInterpreter::GetPRGInterpreter()
{
    OutputDebugValues( "Creating PRG Interpreter (%s)\n", m_sName );

	if( m_pInterpreter && m_pInterpreter->type )
	{
	   OutputDebugValues( "*** ALREADY HAVE INTERPRETER ***\n" );
	   hb_itemRelease( m_pInterpreter );
	}

    if( g_bErrors )
	{
      OutputDebugValues( "!!! PRIOR ERROR !!!" );
		return E_FAIL;
	}

	hb_vmRequestReset();

	// TInterpreter() -> TInterpreter Class object.
	hb_vmPushSymbol( s_pSym_TInterpreter->pSymbol );
	hb_vmPushNil();
   hb_vmPushString( m_sName, strlen( m_sName ) );
	hb_vmDo( 1 );

	m_pInterpreter = hb_itemNew( hb_stackReturnItem() );
     
	if( ! HB_IS_OBJECT( m_pInterpreter ) )
	{
      OutputDebugValues( "ERROR! Couldn't create Interpreter, got Type:%i\n", m_pInterpreter->type );
      hb_itemRelease( m_pInterpreter );
      m_pInterpreter = NULL;
	   return E_FAIL;
	}

	if( m_sCode[0] )
	{
		return SetScript();
	}

    return S_OK;
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
STDMETHODIMP CASInterpreter::QueryInterface(REFIID riid, void ** ppvObj)
{
   //tracing purposes only
   CASINTERPRETERTRACE("CASInterpreter::QueryInterface->");

   if (riid == IID_IUnknown)
   {
      CASINTERPRETERTRACE("IUnknown\n");
      *ppvObj = static_cast<IDispatch*>(this);
   }
   else if (riid == IID_IDispatch)
   {
      CASINTERPRETERTRACE("IDispatch\n");
      *ppvObj = static_cast<IDispatch*>(this);
   }
   else
   {
      CASINTERPRETERTRACE("Unsupported Interface\n");
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
STDMETHODIMP_(ULONG) CASInterpreter::AddRef()
{
   OutputDebugValues( "(%s) CASInterpreter::AddRef(%i)\n", (char *) m_sName, m_refCount + 1 );

   return ++m_refCount;
}

/******************************************************************************
*   Release() -- When a reference to this object is removed, this function
*   decrements the reference count.  If the reference count is 0, then this
*   function deletes this object and returns 0;
******************************************************************************/
STDMETHODIMP_(ULONG) CASInterpreter::Release()
{
   OutputDebugValues( "(%s) CASInterpreter::Release(%i)\n", (char *) m_sName, m_refCount - 1 );

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
STDMETHODIMP CASInterpreter::GetTypeInfoCount(UINT *iTInfo)
{
   //tracing purposes only
   CASINTERPRETERTRACE("CASInterpreter::GetTypeInfoCount\n");

   //This object doesn't support type information
   *iTInfo = 0;

   return E_FAIL;
}

/******************************************************************************
*   GetTypeInfo -- Returns the type information for the class.  For classes
*   that don't support type information, this function returns E_NOTIMPL;
******************************************************************************/
STDMETHODIMP CASInterpreter::GetTypeInfo( UINT iTInfo, LCID lcid, ITypeInfo **ppTInfo )
{
   //tracing purposes only
   CASINTERPRETERTRACE("CASInterpreter::GetTypeInfo\n");

   //This object doesn't support type information
   *ppTInfo = NULL;
   return DISP_E_BADINDEX;
}

/******************************************************************************
*   GetIDsOfNames -- Takes an array of strings and returns an array of DISPID's
*   which corespond to the methods or properties indicated.  If the name is not
*   recognized, returns DISP_E_UNKNOWNNAME.
******************************************************************************/
STDMETHODIMP CASInterpreter::GetIDsOfNames( REFIID riid,
                                            OLECHAR **rgszNames,
                                            UINT cNames,  LCID lcid,
                                            DISPID *rgDispId)
{
   if( cNames == 1 && wcscmp( rgszNames[0], L"defaultEvent" ) == 0 )
   {
      rgDispId[0] = 0;

	   OutputDebugValues( "Returned: defaultEvent as ID #0\n" );

	   return S_OK;
   }

   return m_pOleWrapDispatch->GetIDsOfNames( riid, rgszNames, cNames, lcid, rgDispId );
}

/******************************************************************************
*  Invoke -- Takes a dispid and uses it to call a method or property defined
*  in the script code in the context of this CASInterpreter.
******************************************************************************/
STDMETHODIMP CASInterpreter::Invoke(DISPID dispIdMember, REFIID riid, LCID lcid,
                                    WORD wFlags, DISPPARAMS* pDispParams,
                                    VARIANT* pVarResult, EXCEPINFO* pExcepInfo,
                                    UINT* puArgErr)
{
   HRESULT hr = S_OK;

   //We support a default property for IActiveScriptParseProcedure.
   if( dispIdMember == 0 )
   {
      hr = EvaluateImmediate();

      // This can execute multiple times.
      m_bExecuted = FALSE;

      return hr;
   }

   return m_pOleWrapDispatch->Invoke( dispIdMember, riid, lcid, wFlags, pDispParams, pVarResult, pExcepInfo, puArgErr );
}

STDMETHODIMP CASInterpreter::ConcileNamedItems( TList<NamedItem*>* pNamedItems )
{
   OutputDebugValues( "(%s) CONCILE Globals\n", (char *) m_sName );

  	if( m_bAddedGlobals )
	{
		OutputDebugValues( "*** ALREADY ADDED GLOBALS ***\n" );
		return E_FAIL;
	}

 	m_pNamedItems = pNamedItems;
   m_bAddedGlobals = TRUE;

 	if( m_pNamedItems == NULL || m_pNamedItems->Size() == 0 )
	{
      OutputDebugValues( "No Globals to process\n" );
	   return E_FAIL;
	}

   NamedItem* pNamedItem = NULL;

   char sGlobal[256];

	PFOREACH( m_pNamedItems,
      //Get the next named item
      pNamedItem = m_pNamedItems->Retrieve();

   	unsigned int uLen = wcslen( pNamedItem->GetName() ) + 1;

      WideCharToMultiByte( CP_ACP, WC_COMPOSITECHECK, pNamedItem->GetName(), uLen, (char *) sGlobal, __min( uLen, 256 ), NULL, NULL );

      OutputDebugValues( "NAME: '%s' Flags: %i\n", (char *) sGlobal, pNamedItem->GetFlags() );

		if( pNamedItem->GetFlags() && ( SCRIPTITEM_GLOBALMEMBERS | SCRIPTITEM_ISVISIBLE ) )
		{
			ConcileDispatch( pNamedItem );
    	}
	)

    m_bAddedGlobals = TRUE;

   return S_OK;
}

HRESULT CASInterpreter::ParseText( LPCOLESTR scriptText, ULONG startingLineNumber, DWORD dwSourceContext )
{
   OutputDebugValues( "(%s) CASInterpreter::ParseText!!!\n", (char *) m_sName );

   m_ulStartLine = startingLineNumber;

   if( m_sCode[0] )
   {
      m_bAppendText = TRUE;
   }

   m_bExecuted = FALSE;

   if( WideCharToMultiByte( CP_ACP, WC_COMPOSITECHECK, scriptText, -1, (char *) m_sCode, MAX_SCRIPT_SIZE, NULL, NULL ) )
   {
      OutputDebugValues( "(%s) Start Line: %i Pending code: %s\n", (char *) m_sName, m_ulStartLine, m_sCode );
   }
   else
   {
      g_bErrors = TRUE;
      OutputDebugValues( "(%s) Failed to process SysString!!!\n", (char *) m_sName );
      return E_FAIL;
   }

   return SetScript();
}

/******************************************************************************
*  EvaluateImmediate -- This method executes the immediate instructions
*  in the instruction list.  This consists of any DATA tags and any code at the
*  "main" label.
******************************************************************************/
HRESULT CASInterpreter::EvaluateImmediate()
{
   OutputDebugValues( "Interpreter: '%s' ::EvalImmediate\n", (char *) m_sName );
   
	if( m_bExecuted )
	{
      OutputDebugValues( "(%s) Already Exceuted!\n", (char *) m_sName );
	   return S_OK;
	}

   //OutputDebugValues( "Interpreter: '%s' Code: %s\n", (char *) m_sName, (char *) m_sCode );

   HRESULT hr = Run();
	m_bExecuted = TRUE;

   return hr;
}

void CASInterpreter::Reset( BOOL bReleaseCode )
{
   OutputDebugValues( "('%s', %i)CASInterpreter::Reset(%i)\n", (char *) m_sName, m_refCount, bReleaseCode );

   CASInterpreter::sm_CurrentDispid = 0;

   g_bErrors   = FALSE;

   //Reset the NamedItems pointer
   m_pNamedItems = NULL;

   m_bExecuted     = FALSE;
   m_bAppendText   = FALSE;
   m_bAddedGlobals = FALSE;

   // TODO REVIEW.
   if( bReleaseCode )
   {
      m_sCode[0] = 0;

      if( m_pOleWrapDispatch )
      {
         m_pOleWrapDispatch->Release();
         m_pOleWrapDispatch = NULL;
      }
   }

   if( s_bInit == FALSE )
   {
      #ifdef RESET_VM
        s_bInit = TRUE;
      #else
  		   OutputDebugValues( "(%s) Reset Interpreter\n", (char *) m_sName );

			if( m_pInterpreter )
			{
		      hb_vmPushSymbol( s_pSym_Reset->pSymbol );
         	hb_vmPush( m_pInterpreter );
		      hb_vmSend(0);
			}

         hb_gcSetCollecting( FALSE );
         hb_gcCollectAll( FALSE );
         hb_gcSetCollecting( TRUE );

      #endif         

	   OutputDebugValues( "Done\n" );
   }
}

/******************************************************************************
*  AutomationHelper -- This method simply wraps up Ole Automation calls
*  so that they don't have to further clutter up the code in the interpreter.
*  statements.  It returns the HRESULT returned by Invoke.
******************************************************************************/
HRESULT CASInterpreter::AutomationHelper( IDispatch* pDispatch,
                                         LPCOLESTR lpzName,
                                         VARIANT *pArgs,
                                         VARIANT *pReturnValue,
                                         WORD dwFlags, unsigned int cArgs )
{
   HRESULT hr;
   DISPID l_dispid;

   //tracing purposes only
   OutputDebugValues( "(%s)CASInterpreter::AutomationHelper\n", (char *) m_sName );

   //First, get the DISPID of the name
   hr = pDispatch->GetIDsOfNames( IID_NULL, (LPOLESTR *) &lpzName, 1, 0, &l_dispid );

   OutputDebugValues( "GetIDsOfNames ID: %i Result: %i\n", l_dispid, hr );

   //If we found the name, then try to call Invoke
   if( SUCCEEDED( hr ) )
   {
      DISPPARAMS l_dispParams;
      DISPID dispidNamed = DISPID_PROPERTYPUT;
      UINT l_error = 0;

	   if( pReturnValue )
	   {
         VariantInit( pReturnValue );
	   }

      l_dispParams.rgvarg            = pArgs;
      l_dispParams.rgdispidNamedArgs = NULL;
      l_dispParams.cArgs             = cArgs;
      l_dispParams.cNamedArgs        = 0;

      //Special stuff for PROPERTYPUT's
      if( dwFlags & DISPATCH_PROPERTYPUT )
	   {
         OutputDebugValues( "PROPERTYPUT\n" );

         l_dispParams.cNamedArgs = 1;
         l_dispParams.rgdispidNamedArgs = &dispidNamed;
      }
	  
      hr = pDispatch->Invoke( l_dispid, IID_NULL, 0, dwFlags, &l_dispParams, pReturnValue, /*&l_exceptions*/ NULL, &l_error );
	   {
         char Method[256];
         unsigned int uLen = wcslen( lpzName ) + 1;

         WideCharToMultiByte( CP_ACP, WC_COMPOSITECHECK, lpzName, uLen, (char *) Method, uLen, NULL, NULL );

		   OutputDebugValues( "(%s)Invoked: '%s' Flags: %i, Dispatch %p Args: %i, Arg1: %i, Result: %i\n", (char *) m_sName, (char *) Method, dwFlags, pDispatch, cArgs, (cArgs ? pArgs[0].vt : 0 ), hr );		
	   }
   }

   return hr;
}

STDMETHODIMP CASInterpreter::SetScript()
{
	//OutputDebugValues( "(%s)::SetScript(%s)\n", m_sName, m_sCode );
   OutputDebugValues( "(%s) StartLine: %i in ::SetScript()\n", m_sName, m_ulStartLine );

	if( m_sCode[0] == 0 )
	{
		OutputDebugValues( "!!! EmptyScript !!!" );
		return S_OK;
	}

   if( g_bErrors )
	{
		OutputDebugValues( "!!! PRIOR ERROR !!!" );
		return E_FAIL;
	}

	if( m_bAppendText )
	{
	   hb_vmPushSymbol( s_pSym_AddText->pSymbol );
	}
	else
	{
	   hb_vmPushSymbol( s_pSym_SetScript->pSymbol );
	}

	hb_vmRequestReset();

	hb_vmPush( m_pInterpreter );
	hb_vmPushString( (char *) m_sCode, strlen( (char *) m_sCode ) );
	hb_vmPushLong( m_ulStartLine );

   hb_vmSend(2);
 
   m_bAppendText = FALSE;

	return Compile();
}

STDMETHODIMP CASInterpreter::ConcileDispatch( NamedItem* pNamedItem )
{
	char sGlobal[256];
	unsigned int uLen = wcslen( pNamedItem->GetName() ) + 1;

   WideCharToMultiByte( CP_ACP, WC_COMPOSITECHECK, pNamedItem->GetName(), uLen, (char *) sGlobal, __min( uLen, 256 ), NULL, NULL );

	IDispatch *pDispatch = NULL;

	pNamedItem->GetDispatch( &pDispatch );

	OutputDebugValues( "CONCILE '%s', Dispatch: %p\n", sGlobal, pDispatch );

	if( pDispatch )
	{
		if( g_bErrors )
		{
			OutputDebugValues( "!!! PRIOR ERROR !!!" );
			return E_FAIL;
		}

		hb_vmRequestReset();

		hb_vmPushSymbol( s_pSym_ScriptSiteAddGlobal->pSymbol );
		hb_vmPush( m_pInterpreter );
      hb_vmPushString( (char *) sGlobal, __min( uLen - 1, 256 ) );
		hb_vmPushLong( (long) pDispatch );

		hb_vmSend(2);

		OutputDebugValues( "Added Global '%s' Flags: %i Dispatch %p\n", (char *) sGlobal, pNamedItem->GetFlags(), pDispatch );

		return S_OK;
	}
	else
	{
		OutputDebugValues( "CONCILE Failed! for '%s'\n", (char *) sGlobal );
	}

	return E_FAIL;
}

STDMETHODIMP CASInterpreter::Compile()
{
   HRESULT hr;

   OutputDebugValues( "(%s) Compile()\n", m_sName );   
   //OutputDebugValues( "(%s) Compile(%s)\n", m_sName, m_sCode );

	if( m_sCode[0] == 0 )
	{
      OutputDebugValues( "(%s)!!! NOTHING to Compile !!!\n", m_sName );
		return S_OK;
	}

   if( g_bErrors )
	{
		OutputDebugValues( "!!! PRIOR ERROR !!!" );
		return E_FAIL;
	}

	hb_vmRequestReset();

	hb_vmPushSymbol( s_pSym_Compile->pSymbol );
	hb_vmPush( m_pInterpreter );

   if( m_pEngine && m_pEngine->m_pGlobalInterpreter && m_pEngine->m_pGlobalInterpreter->m_pInterpreter )
   {
      OutputDebugValues( "--->>>(%s)!!! Pushed Global(%s)\n", m_sName, m_pEngine->m_pGlobalInterpreter->m_sName );
      hb_vmPush( m_pEngine->m_pGlobalInterpreter->m_pInterpreter );
   }
   else
   {
      hb_vmPushNil();
   }

	hb_vmSend(1);

	if( strcmp( hb_objGetClsName( hb_stackReturnItem() ), "ERROR" ) == 0 )
	{
      g_bErrors = TRUE;

		OutputDebugValues( "FAILED!!!\n" );

		char sError[512];
      HB_ITEM Error;

    	HB_ITEM ModuleName;
    	HB_ITEM ProcName;
		HB_ITEM ProcLine;
		HB_ITEM SubSystem;
		HB_ITEM Operation;
		HB_ITEM Description;

		Error.type = HB_IT_NIL;
		hb_itemForwardValue( &Error, hb_stackReturnItem() );

		hb_objSendMsg( &Error, "ModuleName", 0 );
		ModuleName.type = HB_IT_NIL;
		hb_itemForwardValue( &ModuleName, hb_stackReturnItem() );

		hb_objSendMsg( &Error, "ProcName", 0 );
		ProcName.type = HB_IT_NIL;
		hb_itemForwardValue( &ProcName, hb_stackReturnItem() );

		hb_objSendMsg( &Error, "ProcLine", 0 );
		ProcLine.type = HB_IT_NIL;
		hb_itemForwardValue( &ProcLine, hb_stackReturnItem() );

		hb_objSendMsg( &Error, "SubSystem", 0 );
		SubSystem.type = HB_IT_NIL;
		hb_itemForwardValue( &SubSystem, hb_stackReturnItem() );

		hb_objSendMsg( &Error, "Operation", 0 );
		Operation.type = HB_IT_NIL;
		hb_itemForwardValue( &Operation, hb_stackReturnItem() );

		hb_objSendMsg( &Error, "Description", 0 );
		Description.type = HB_IT_NIL;
		hb_itemForwardValue( &Description, hb_stackReturnItem() );

		sError[0] = '\0';

		if( HB_IS_STRING( &ModuleName ) )
		{
		   OutputDebugValues( "ModuleName: %s\n", ModuleName.item.asString.value );
		   strcat( sError, ModuleName.item.asString.value );
		   strcat( sError, "/" );
		}

		if( HB_IS_STRING( &ProcName ) )
		{
		   OutputDebugValues( "ProcName: %s\n", ProcName.item.asString.value );
		   strcat( sError, ProcName.item.asString.value );
		   strcat( sError, "/" );
		}

		if( HB_IS_NUMERIC( &ProcLine ) )
		{
		   OutputDebugValues( "ProcLine: %i\n", hb_itemGetNL( &ProcLine ) );
		}

		if( HB_IS_STRING( &SubSystem ) )
		{
		   OutputDebugValues( "SubSystem: %s\n", SubSystem.item.asString.value );
		   strcat( sError, SubSystem.item.asString.value );
		   strcat( sError, "/" );
		}

    	if( HB_IS_STRING( &Operation ) )
		{
		   OutputDebugValues( "Operation: %s\n", Operation.item.asString.value );
		   strcat( sError, Operation.item.asString.value );
		   strcat( sError, ":" );
		}

		if( HB_IS_STRING( &Description ) )
		{
		   OutputDebugValues( "Description: %s\n", Description.item.asString.value );
		   strcat( sError, Description.item.asString.value );
		}

      PHB_ITEM pErrorMessage = hb_itemDoC( "PP_ErrorMessage", 1, &Error );

      hr = m_pEngine->m_pErrorHandler->HandleCompileError( pErrorMessage->item.asString.value, hb_itemGetNL( &ProcLine ), m_pEngine->m_pGlobalInterpreter->GetSourceLineText( hb_itemGetNL( &ProcLine ) ) );
      
      hb_itemRelease( pErrorMessage );

	   hb_itemClear( &Error );

	   hb_itemClear( &ModuleName );
	   hb_itemClear( &ProcName );
	   hb_itemClear( &ProcLine );
	   hb_itemClear( &SubSystem );
	   hb_itemClear( &Operation );
	   hb_itemClear( &Description );
	}
   else
   {
      hr = S_OK;
   }

	return hr;
}

STDMETHODIMP CASInterpreter::HandleReturnedError( void )
{
   g_bErrors = TRUE;

	OutputDebugValues( "RUN FAILED!!!\n" );

	HB_ITEM Error;

	HB_ITEM ModuleName;
  	HB_ITEM ProcName;
 	HB_ITEM ProcLine;
	HB_ITEM SubSystem;
	HB_ITEM Operation;
	HB_ITEM Description;

	Error.type = HB_IT_NIL;
	hb_itemForwardValue( &Error, hb_stackReturnItem() );

	hb_objSendMsg( &Error, "ModuleName", 0 );
	ModuleName.type = HB_IT_NIL;
 	hb_itemForwardValue( &ModuleName, hb_stackReturnItem() );

   hb_objSendMsg( &Error, "ProcName", 0 );
   ProcName.type = HB_IT_NIL;
   hb_itemForwardValue( &ProcName, hb_stackReturnItem() );

	hb_objSendMsg( &Error, "ProcLine", 0 );
 	ProcLine.type = HB_IT_NIL;
   hb_itemForwardValue( &ProcLine, hb_stackReturnItem() );

	hb_objSendMsg( &Error, "SubSystem", 0 );
	SubSystem.type = HB_IT_NIL;
	hb_itemForwardValue( &SubSystem, hb_stackReturnItem() );

	hb_objSendMsg( &Error, "Operation", 0 );
	Operation.type = HB_IT_NIL;
	hb_itemForwardValue( &Operation, hb_stackReturnItem() );

	hb_objSendMsg( &Error, "Description", 0 );
	Description.type = HB_IT_NIL;
	hb_itemForwardValue( &Description, hb_stackReturnItem() );

	if( HB_IS_STRING( &ModuleName ) )
	{
	   OutputDebugValues( "ModuleName: %s\n", ModuleName.item.asString.value );
	}

	if( HB_IS_STRING( &ProcName ) )
	{
	   OutputDebugValues( "ProcName: %s\n", ProcName.item.asString.value );
	}

	if( HB_IS_NUMERIC( &ProcLine ) )
	{
	   OutputDebugValues( "ProcLine: %i\n", hb_itemGetNI( &ProcLine ) );
	}

	if( HB_IS_STRING( &SubSystem ) )
	{
	   OutputDebugValues( "SubSystem: %s\n", SubSystem.item.asString.value );
	}

	if( HB_IS_STRING( &Operation ) )
	{
	   OutputDebugValues( "Operation: %s\n", Operation.item.asString.value );
	}

	if( HB_IS_STRING( &Description ) )
	{
	   OutputDebugValues( "Description: %s\n", Description.item.asString.value );
	}

   PHB_ITEM pErrorMessage = hb_itemDoC( "PP_ErrorMessage", 1, &Error );
   HRESULT hr = m_pEngine->m_pErrorHandler->HandleRuntimeError( hb_itemGetCPtr( pErrorMessage ), hb_itemGetCPtr( &Operation ), hb_itemGetNI( &ProcLine ) - m_ulStartLine, m_pEngine->m_pGlobalInterpreter->GetSourceLineText( hb_itemGetNI( &ProcLine ) - m_ulStartLine ), NULL );
   hb_itemRelease( pErrorMessage );

   hb_itemClear( &Error );
   hb_itemClear( &ModuleName );
   hb_itemClear( &ProcName );
   hb_itemClear( &ProcLine );
   hb_itemClear( &SubSystem );
   hb_itemClear( &Operation );
   hb_itemClear( &Description );

   m_pEngine->m_pScriptSite->OnLeaveScript();

   return hr;
}

STDMETHODIMP CASInterpreter::Run()
{
   OutputDebugValues( "(%s)Run()->\n%s\n", (char *) m_sName, (char *) m_sCode );

	if( m_sCode[0] == 0 )
	{
	   OutputDebugValues( "!!! NOTHING to run !!!\n" );
	}
   else
	{
		if( g_bErrors )
		{
		   OutputDebugValues( "!!! PRIOR ERROR !!!" );
		   return E_FAIL;
		}

      g_pInterpreter = this;

      //We have to notify the host that we're going to execute code
      m_pEngine->m_pScriptSite->OnEnterScript();

		hb_vmRequestReset();

      __try
      {
         hb_vmPushSymbol( s_pSym_Run->pSymbol );
         hb_vmPush( m_pInterpreter );
         hb_vmSend(0);
      }
      __except( EXCEPTION_EXECUTE_HANDLER )
      {
         OutputDebugString( "EXCEPTION Caught in: CASInterpreter::Run()\n" );
		   m_pEngine->m_pScriptSite->OnLeaveScript();
         g_bErrors = TRUE;
         return DISP_E_EXCEPTION;
      }

		if( strcmp( hb_objGetClsName( hb_stackReturnItem() ), "ERROR" ) == 0 )
		{
         OutputDebugValues( "::Run() FAILED!!!\n" );
         return HandleReturnedError();
		}
		else
		{
		   m_pEngine->m_pScriptSite->OnLeaveScript();

			OutputDebugValues( "SUCCESS.\n" );
		}
	}

   return S_OK;
}

STDMETHODIMP CASInterpreter::Eval( LPCOLESTR wExp, unsigned int cArgs, VARIANT *pArgs, VARIANT *pRetValue, BOOL bMethod )
{
   unsigned int uLen = wcslen( (const wchar_t *) wExp ) + 1;
   char *sExp = ( char * ) hb_xgrab( uLen );

   WideCharToMultiByte( CP_ACP, WC_COMPOSITECHECK, wExp, uLen, sExp, uLen, NULL, NULL );

   OutputDebugValues( "(%s)Evaluating <%s> (%i params)\n", (char *) m_sName, sExp, cArgs );
   //OutputDebugValues( "Code Context: %s\n", m_sCode );

   if( g_bErrors )
   {
	  OutputDebugValues( "!!! PRIOR ERROR !!!" );
      return E_FAIL;
   }

   hb_vmRequestReset();

   HRESULT hr;

   g_pInterpreter = this;

   //We have to notify the host that we're going to execute code
   m_pEngine->m_pScriptSite->OnEnterScript();

   hb_vmRequestReset();

   __try
   {
      HB_ITEM aArgs, Exp;

      aArgs.type = HB_IT_NIL;
      if( cArgs || bMethod )
      {
         hb_arrayNew( &aArgs, cArgs );

	      for( unsigned int i = 0; i < cArgs; i++ )
	      {
	         VariantToItem( aArgs.item.asArray.value->pItems + i, &pArgs[i] );
	      }
      }

      Exp.type = HB_IT_NIL; 
      hb_itemPutCPtr( &Exp, sExp, uLen - 1 );

      hb_vmPushSymbol( s_pSym_EvalExpression->pSymbol );
      hb_vmPush( m_pInterpreter );
      hb_itemPushForward( &Exp );   // cExp
      hb_itemPushForward( &aArgs ); // aParams
      hb_vmPushLong( 0 );           //nLine
      hb_vmPushLogical( bMethod );  //bScriptProc

      hb_vmSend(4);
   }
   __except( EXCEPTION_EXECUTE_HANDLER )
   {
      OutputDebugString( "EXCEPTION Caught in: CASInterpreter::Eval()\n" );
	   m_pEngine->m_pScriptSite->OnLeaveScript();
      g_bErrors = TRUE;
      return DISP_E_EXCEPTION;
   }


   //We have to notify the host that we're  done executing code
   m_pEngine->m_pScriptSite->OnLeaveScript();

   if( strcmp( hb_objGetClsName( hb_stackReturnItem() ), "ERROR" ) == 0 )
   {
	   OutputDebugValues( "::Eval() FAILED!!!\n" );
      return HandleReturnedError();
   }
   else
   {
      if( pRetValue )
	  {
         hr = ItemToVariant( pRetValue, hb_stackReturnItem() );
	  }
	  else
	  {
		  hr = S_OK;
	  }

      hb_itemClear( hb_stackReturnItem() );
   }

   return hr;
}

STDMETHODIMP CASInterpreter::ResolveSiteGlobals( LPCOLESTR method, VARIANT *pArgs, VARIANT *pRetValue, unsigned int cArgs, WORD dwFlags )
{
   OutputDebugValues( "(%s)CASInterpreter::ResolveSiteGlobals\n", m_sName );

   if( ! ( this == m_pEngine->m_pGlobalInterpreter ) )
   {
      OutputDebugValues( "REDIRECT to GLOBAL(%s)!!!\n", m_pEngine->m_pGlobalInterpreter->m_sName );
      return m_pEngine->m_pGlobalInterpreter->ResolveSiteGlobals( method, pArgs, pRetValue, cArgs, dwFlags );
   }

   if( g_bErrors )
   {
      hb_vmRequestBreak( NULL );
      OutputDebugValues( "!!! PRIOR ERROR !!!\n" );
      return E_FAIL;
   }

   if( m_pNamedItems == NULL )
   {
      OutputDebugValues( "!!! NO GLOBALS !!!\n" );
      return E_FAIL;
   }

   HRESULT hr = E_FAIL;

   NamedItem* pNamedItem = NULL;

   char sGlobal[256];

   PFOREACH( m_pNamedItems,
      //Get the nexted named item
      pNamedItem = m_pNamedItems->Retrieve();

      //Only search it if it's a global member.
      if( pNamedItem->GetFlags() && ( SCRIPTITEM_GLOBALMEMBERS | SCRIPTITEM_ISVISIBLE ) )
	   {
         unsigned int uLen = wcslen( pNamedItem->GetName() ) + 1;

         WideCharToMultiByte( CP_ACP, WC_COMPOSITECHECK, pNamedItem->GetName(), uLen, (char *) sGlobal, __min( uLen, 256 ), NULL, NULL );

         OutputDebugValues( "TRY: '%s' Flags: %i Args: %i\n", (char *) sGlobal, pNamedItem->GetFlags(), cArgs );

         //Try to get the NamedItem's IDispatch pointer
         IDispatch* pDispatch = NULL;
         hr = pNamedItem->GetDispatch( &pDispatch );

         //if successful, use the AutomationHelper to try for the
         //property, and then release the IDispatch pointer
         if( SUCCEEDED(hr) )
		   {
            OutputDebugValues( "USE Helper: '%s' Flags: %i\n", (char *) sGlobal, pNamedItem->GetFlags() );

            hr = AutomationHelper( pDispatch, method, pArgs, pRetValue, dwFlags, cArgs );
            pDispatch->Release();

            //If we found it, then break out of the for loop.
            if( SUCCEEDED(hr) )
            {
               OutputDebugValues( "SUCCEDED GLOBAL Resolution\n" );
               break;
            }
         }
      }
   );

   if( FAILED( hr ) )
   {
      OutputDebugValues( "FAILED GLOBAL Resolution\n" );
   }

   return hr;
}

const char * CASInterpreter::GetSourceLineText( ULONG ulRequestedLine )
{
   ULONG ulLine = 1;

   OutputDebugValues( "(%s) CASInterpreter::GetSourceLineText() Line: #%i\n", m_sName, ulRequestedLine );

   /*
   if( m_sCode[0] == 0 )
   {
	   return E_FAIL;
   }
   */

   hb_vmRequestReset();

   hb_vmPushSymbol( s_pSym_GetLine->pSymbol );
   hb_vmPush( m_pInterpreter );
   hb_vmPushLong( ulRequestedLine );

   hb_vmSend(1);

   OutputDebugValues( "Got Line: '%s'\n", hb_parcx( -1 ) );

   return hb_parcx( -1 );
}

HB_ERRCODE hb_memvarGet( PHB_ITEM pItem, PHB_SYMB pMemvarSymb )
{
   HB_THREAD_STUB
   PHB_DYNS pDyn;
   HB_ERRCODE bSuccess = FAILURE;

   OutputDebugValues( "MEMVAR GET: %s %p\n", pMemvarSymb->szName, pMemvarSymb->pDynSym );

   HB_VALUE_PTR s_globalTable = *hb_memvarValueBaseAddress();

   pDyn = pMemvarSymb->pDynSym;

   if( pDyn )
   {
      OutputDebugValues( "Memvar item (%i)(%s) queried\n", pDyn->hMemvar, pMemvarSymb->szName );

      if( pDyn->hMemvar )
      {
         /* value is already created
          */
         HB_ITEM_PTR pGetItem = s_globalTable[ pDyn->hMemvar ].pVarItem;

         if( HB_IS_BYREF( pGetItem ) )
         {
            hb_itemCopy( pItem, hb_itemUnRef( pGetItem ) );
         }
         else
         {
            hb_itemCopy( pItem, pGetItem );
         }

         bSuccess = SUCCESS;
      }
	   else
	   {
 	      OutputDebugValues( "RESOLVE GET: %s\n", pMemvarSymb->szName );

         VARIANT RetValue;

  	      unsigned int uLen = strlen( pMemvarSymb->szName ) + 1;

         WCHAR method[256];

         MultiByteToWideChar( CP_ACP, 0, pMemvarSymb->szName, uLen, (LPOLESTR) method, uLen );

		   VariantInit( &RetValue );

         HRESULT hr = g_pInterpreter->ResolveSiteGlobals( method, NULL, &RetValue, 0, DISPATCH_PROPERTYGET );

		   if( SUCCEEDED(hr) )
		   {
            VariantToItem( pItem, &RetValue );
            bSuccess = SUCCESS;
            OutputDebugValues( "GET GLOBAL Resolved(%s)\n", pMemvarSymb->szName );
		   }
		   else
		   {
           OutputDebugValues( "GLOBAL FAILED!!!(%s)\n", pMemvarSymb->szName );
		   }

		   VariantClear( &RetValue );
	   }
   }
   else
   {
      hb_errInternal( HB_EI_MVBADSYMBOL, NULL, pMemvarSymb->szName, NULL );
   }

   return bSuccess;
}

/*
 * This functions copies passed item value into the memvar pointed
 * by symbol
 *
 * pMemvar - symbol associated with a variable
 * pItem   - value to store in memvar
 *
 */
void hb_memvarSetValue( PHB_SYMB pMemvarSymb, PHB_ITEM pItem )
{
   HB_THREAD_STUB
   PHB_DYNS pDyn;

   OutputDebugValues( "hb_memvarSetValue(%s)\n", pMemvarSymb->szName );

   HB_VALUE_PTR s_globalTable = *hb_memvarValueBaseAddress();

   pDyn = ( PHB_DYNS ) pMemvarSymb->pDynSym;

   if( pDyn )
   {
      HB_TRACE(HB_TR_INFO, ("Memvar item (%i)(%s) assigned", pDyn->hMemvar , pMemvarSymb->szName));

      if( pDyn->hMemvar )
      {
         /* value is already created */
         HB_ITEM_PTR pSetItem;

         pSetItem = s_globalTable[ pDyn->hMemvar ].pVarItem;

         // JC1: the variable we have now can't be destroyed in the meanwhile.
         // It could be changed, but this is a race condition that must be
         // prevented at prg level.
         if( HB_IS_BYREF( pSetItem ) )
         {
            pSetItem = hb_itemUnRef( pSetItem );
         }

         hb_itemCopy( pSetItem, pItem );
      }
      else
      {


      //VBScript assums Script Variable on UNqualified assignment
      //NOTE !!!
      #if 0
         // Objects except for TOLEAUTO can NOT be passed as OLE argument!
         if( (! HB_IS_ARRAY( pItem ) )  || strcmp( hb_objGetClsName( pItem ), "TOLEAUTO" ) == 0 )
		 {
			OutputDebugValues( "RESOLVE SET: %s\n", pMemvarSymb->szName );

			VARIANT Value;

			VariantInit( &Value );

   			unsigned int uLen = strlen( pMemvarSymb->szName ) + 1;

			WCHAR method[256];

			MultiByteToWideChar( CP_ACP, 0, pMemvarSymb->szName, uLen, (LPOLESTR) method, uLen );

			ItemToVariant( &Value, pItem );

			HRESULT hr = g_ActiveInterpreter->ResolveSiteGlobals( method, &Value, NULL, 1, DISPATCH_PROPERTYPUT );

			if( SUCCEEDED(hr) )
			{
                OutputDebugValues( "SET GLOBAL Resolved(%s)\n", pMemvarSymb->szName );
				return;
			}
            else
		    {
               OutputDebugValues( "GLOBAL FAILED - Assuming new PRIVATE(%s)\n", pMemvarSymb->szName );
		    }
         }
      #endif

         /* assignment to undeclared memvar - PRIVATE is assumed
          */
         hb_memvarNewParameter( pMemvarSymb, pItem );
      }

      /* Remove MEMOFLAG if exists (assignment from field). */
      s_globalTable[ pDyn->hMemvar ].pVarItem->type &= ~HB_IT_MEMOFLAG;
   }
   else
   {
      OutputDebugValues( "hb_memvarSetValue(%s) RAISING ERROR\n", pMemvarSymb->szName );
      hb_errInternal( HB_EI_MVBADSYMBOL, NULL, pMemvarSymb->szName, NULL );
   }
}

void hb_memvarGetValue( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb )
{
   OutputDebugValues( "hb_memvarGetValue(%s)\n", pMemvarSymb->szName );

   if( hb_memvarGet( pItem, pMemvarSymb ) == FAILURE )
   {
	  HB_ERROR_INFO_PTR pHandler = hb_errorHandler( NULL );

      if( pHandler )
	  {
         OutputDebugValues( "*** MACRO REsolution failed for (%s)\n", pMemvarSymb->szName );
         hb_errorHandler( pHandler );
		 return;
	  }

      /* Generate an error with retry possibility
       * (user created error handler can create this variable)
       */
      USHORT uiAction = E_RETRY;
      HB_ITEM_PTR pError;

      OutputDebugValues( "*** RAISING Error for (%s)\n", pMemvarSymb->szName );

      pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003, NULL, pMemvarSymb->szName, 0, EF_CANRETRY );

      while( uiAction == E_RETRY )
      {
         uiAction = hb_errLaunch( pError );

         if( uiAction == E_RETRY )
         {
            if( hb_memvarGet( pItem, pMemvarSymb ) == SUCCESS )
            {
               uiAction = E_DEFAULT;
            }
         }
      }

      if( uiAction != E_DEFAULT )
	  {
	     g_bErrors = TRUE;
         OutputDebugValues( "!!! GLOBAL ERROR hb_memvarGetValue '%s' !!!", pMemvarSymb->szName );

		 //hb_vmRequestBreak( pError );
		 hb_itemForwardValue( hb_stackReturnItem(), pError );
	  }

      hb_itemRelease( pError );
   }
}

HB_FUNC( RESOLVESITEGLOBALS )
{
    PHB_ITEM pError = hb_param( 1, HB_IT_BYREF );

	OutputDebugValues( "RESOLVE SiteGlobals\n" );

	if( pError == NULL || ! HB_IS_OBJECT( pError ) )
	{
		g_bErrors = TRUE;

		OutputDebugValues( "Invalid paramaters to ResolveSiteGlobals()\n" );
        hb_vmRequestBreak( pError );
		return;
	}

	if( g_bErrors )
	{
        OutputDebugValues( "!!! PRIOR ERROR !!!" );
		return;
	}

	hb_vmRequestReset();

	hb_vmPushSymbol( s_pSym_SubCode->pSymbol );
	hb_vmPush( pError );
	hb_vmSend( 0 );

	if( hb_stackReturnItem()->type == HB_IT_INTEGER )
	{
      long lErrorCode = (long) hb_stackReturnItem()->item.asInteger.value;

	   switch( lErrorCode )
	   {
		   case 1001:
			   break;

	      case 1003:
  		      OutputDebugValues( "DONT RETRY - Memvar already processed by hb_memvar*()\n" );
	         hb_retl( 0 );
		      return;

		  default:
			  OutputDebugValues( "Error: %i\n", lErrorCode );
           hb_vmRequestBreak( pError );
 		     return;
	   }
	}
	else
	{
      OutputDebugValues( "Type: %i\n", hb_stackReturnItem()->type );

      hb_vmRequestBreak( pError );
	   return;
	}

	hb_vmPushSymbol( s_pSym_Operation->pSymbol );
	hb_vmPush( pError );
	hb_vmSend( 0 );

	if( ! HB_IS_STRING( hb_stackReturnItem() ) )
	{
        OutputDebugValues( "!!!Operation Type: %i\n", hb_stackReturnItem()->type );

        hb_vmRequestBreak( pError );
		return;
	}

	OutputDebugValues( "Resolve Method: >%s<\n", hb_stackReturnItem()->item.asString.value );

	unsigned int uLen = strlen( hb_stackReturnItem()->item.asString.value ) + 1;
   WCHAR method[256];

   MultiByteToWideChar( CP_ACP, 0, hb_stackReturnItem()->item.asString.value, uLen, (LPOLESTR) method, uLen );

   VARIANT *aArgs;
   unsigned int cArgs;

	PHB_ITEM pArgs = hb_param( 2, HB_IT_ARRAY );

   if( pArgs == NULL || pArgs->item.asArray.value->ulLen == 0 )
	{
		cArgs = 0;
		aArgs = NULL;
	}
	else
	{
      cArgs = pArgs->item.asArray.value->ulLen;
      aArgs = (VARIANT *) malloc( sizeof( VARIANT ) * cArgs );//new VARIANT[cArgs];
	}

	unsigned int i = 0;
	while( i < cArgs )
	{
      OutputDebugValues( "Param #%i, Type: %i\n", i, pArgs->item.asArray.value->pItems[i].type );
      VariantInit( &aArgs[i] );
	   ItemToVariant( &aArgs[i], pArgs->item.asArray.value->pItems + cArgs - i - 1 );
	   i++;
	}

	VARIANT RetValue;
   VariantInit( &RetValue );

	HRESULT hr = g_pInterpreter->ResolveSiteGlobals( method, aArgs, &RetValue, cArgs, DISPATCH_METHOD );

	while( cArgs )
	{
	   cArgs--;
      VariantClear( &aArgs[cArgs] );
	}

	if( aArgs )
	{
       free( (void *) aArgs );
	}

   if( SUCCEEDED( hr) )
	{
	   VariantToItem( hb_stackReturnItem(), &RetValue );
	   VariantClear( &RetValue );
	}
	else
	{
       OutputDebugValues( "FAILED to resolve! Return: %i\n", pError->type );
       
	   //hb_itemForwardValue( hb_stackReturnItem(), pError );
       
	   // New Error!!!
	   if( g_bErrors )
	   {
		   OutputDebugValues( "!!! PRIOR ERROR !!!" );
		   hb_itemForwardValue( pError, hb_stackReturnItem() );
	   }

      hb_vmRequestBreak( pError );
	}
}

#ifdef VM_RESET
static void ProcessSymbols( void )
{
   PSYMBOLS pLastSymbols = *hb_vmSymbols();

   OutputDebugValues( "ProcessSymbols()\n" );

   do
   {
      USHORT ui;

      for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
      {

         PHB_SYMB pSymbol = pLastSymbols->pModuleSymbols + ui;
         HB_SYMBOLSCOPE hSymScope;

         hSymScope = pSymbol->cScope;

         //OutputDebugValues( "Module: '%s' Sym: '%s' Scope: %i\n", pLastSymbols->szModuleName, pSymbol->szName, hSymScope );

         if( hSymScope & ( HB_FS_PUBLIC | HB_FS_MESSAGE | HB_FS_MEMVAR | HB_FS_FIRST ) )
		 {
            hb_dynsymNew( pSymbol, pLastSymbols );
		 }
      }

      pLastSymbols = pLastSymbols->pNext;

   } while( pLastSymbols );
}
#endif

static void InitSymbols( void )
{
	OutputDebugValues( "InitSymbols()\n" );

	#ifdef VM_RESET
		// Simulate static intializers.
		if( *hb_dynsymCount() == 0 )
		{
			ProcessSymbols();
		}

		hb_vmInit( FALSE );
	#endif

    s_pSym_GetLine       = hb_dynsymGet( "GETLINE" );
	 s_pSym_SubCode       = hb_dynsymFind( "SUBCODE" );
	 s_pSym_Operation     = hb_dynsymFind( "OPERATION" );

    s_pSym_TInterpreter  = hb_dynsymFind( "XBSCRIPTAX" ); // "TINTERPRETER" );

    s_pSym_TOleAuto      = hb_dynsymFind( "TOLEAUTO" );
    s_pSym_New           = hb_dynsymFind( "NEW" );
    s_pSym_hObj          = hb_dynsymFind( "HOBJ" );

	s_pSym_Compile        = hb_dynsymFind( "COMPILE" );

	s_pSym_SetScript      = hb_dynsymGet( "SETSCRIPT" );
	s_pSym_AddText        = hb_dynsymGet( "ADDTEXT" );
	s_pSym_Run            = hb_dynsymGet( "RUN" );
	s_pSym_EvalExpression = hb_dynsymGet( "EVALEXPRESSION" );

	s_pSym_ScriptSiteAddGlobal    = hb_dynsymGet( "SCRIPTSITEADDGLOBAL" );
//	s_pSym_ScriptSiteResetGlobals = hb_dynsymGet( "SCRIPTSITERESETGLOBALS" );
//	s_pSym_IsProcedure            = hb_dynsymGet( "ISPROCEDURE" );

	#ifndef VM_RESET
		s_pSym___MVClear      = hb_dynsymGet( "__MVCLEAR" );
		s_pSym_dbCloseAll     = hb_dynsymGet( "DBCLOSEALL" );
		s_pSym_Reset          = hb_dynsymFind( "RESET" );
	#endif

	/*
	OutputDebugValues( "-------------Initialized:\n %p, %p, %p, %p %p, %p, %p, %p, %p, %p %p %p %p %p %p\n",
                              s_pSym_GetLine 
                              s_pSym_SubCode,
                              s_pSym_Operation,

		                      s_pSym_TInterpreter,

							  s_pSym_TOleAuto,
							  s_pSym_New,
							  s_pSym_hObj,

							  s_pSym_Compile,

							  s_pSym_SetScript,
							  s_pSym_AddText,
							  s_pSym_Run,
							  s_pSym_EvalExpression,

							  s_pSym_ScriptSiteAddGlobal,
							  s_pSym_ScriptSiteResetGlobals,
							  s_pSym_IsProcedure );
   */

  OutputDebugValues( "DONE InitSymbols()\n" );
}

void ResetVM( void )
{
   OutputDebugString( "*** *** ResetVM() *** ***\n" );

   #ifdef RESET_VM
      OutputDebugValues( "Quiting VM...\n" );

  	   // Save.
		PSYMBOLS *ppSymbols = hb_vmSymbols();
		PSYMBOLS pSymbols = *ppSymbols;

		// Calling EXPLICTLY because Local Symbols will be NULL in hb_vmQuit();
		hb_vmDoExitFunctions();

		//  Set to NULL to avoide Release.
		*ppSymbols = NULL;

		hb_vmQuit();

		// Restore.
		*ppSymbols = pSymbols;

		s_pSym_TInterpreter   = NULL ;

		s_pSym_TOleAuto       = NULL ;
		s_pSym_hObj           = NULL ;
		s_pSym_New            = NULL ;

		s_pSym_SetScript      = NULL ;
		s_pSym_AddText        = NULL ;
		s_pSym_Compile        = NULL ;
		s_pSym_Run            = NULL ;
		s_pSym_EvalExpression = NULL;

		s_pSym_ScriptSiteAddGlobal    = NULL;
		//s_pSym_ScriptSiteResetGlobals = NULL;
		//s_pSym_IsProcedure            = NULL;

		s_pSym_GetLine        = NULL;
		s_pSym_SubCode        = NULL;
		s_pSym_Operation      = NULL;

		//s_pSym___MVClear      = NULL;
		//s_pSym_dbCloseAll     = NULL;

		#if 0
		   do
		   {
		      OutputDebugValues( "Module: %s Scope %ui\n", pSymbols->szModuleName, pSymbols->hScope );

				if( pSymbols->hScope & HB_FS_INIT && ! ( pSymbols->hScope & HB_FS_EXIT ) )
				{
				   OutputDebugValues( "INIT\n" );
				}

				pSymbols = pSymbols->pNext;
			}
         while( pSymbols );
		#endif

   #else          

      #if 1
         OutputDebugValues( "Clear Memory\n" );

	      hb_vmRequestReset();

		   hb_vmPushSymbol( s_pSym___MVClear->pSymbol );
		   hb_vmPushNil();
		   hb_vmDo(0);

		   OutputDebugValues( "Close ALL\n" );

		   hb_vmPushSymbol( s_pSym_dbCloseAll->pSymbol );
		   hb_vmPushNil();
		   hb_vmDo(0);
      #endif

   #endif
}