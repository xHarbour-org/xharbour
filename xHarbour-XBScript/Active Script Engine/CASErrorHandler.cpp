/******************************************************************************
*
*  File:CASErrorHandler.cpp
*
*  Author:  Joel Alley
*
*  Date: November 22, 1998
*
*  Description:   This file contains the definition of the CASErrorHandler 
*                 class, which extends the functionality of the CErrorHandler
*                 class to make use of the Active Scripting facilities when
*                 reporting errors.
*
*  Modifications:
******************************************************************************/
#define CASERRORHANDLER
#include "activeDepends.h"

#define SCRIPT_E_REPORTED 0x80020101

extern CASInterpreter *g_ActiveInterpreter;

CASErrorHandler::CASErrorHandler()
{
	OutputDebugValues( "CASErrorHandler::CASErrorHandler()\n" );
	
   m_refCount         = 1;

   m_Desc             = NULL;
   m_ulLine           = 0;
   m_Operation        = NULL;
   m_Source           = NULL;

   m_pScriptSite      = NULL;
   m_pScriptSiteDebug = NULL;
}

CASErrorHandler::~CASErrorHandler()
{
	OutputDebugValues( "~~~CASErrorHandler::CASErrorHandler()\n" );

   _ASSERTE( m_refCount == 0 );

   if( m_Desc )
   {
      SysFreeString( m_Desc );
      m_Desc = NULL;
   }

   if( m_Operation )
   {
      SysFreeString( m_Operation );
      m_Operation = NULL;
   }

   if( m_Source )
   {
      SysFreeString( m_Source );
      m_Source = NULL;
   }

   // No reference counting here - using the ScriptEngine refernce!!!
    m_pScriptSite = NULL;

   if( m_pScriptSiteDebug )
   {
      m_pScriptSiteDebug->Release();
      m_pScriptSiteDebug = NULL;
   }
}

/******************************************************************************
*  SetScriptSite -- This method caches away the IActiveScriptSite interface for
*  use in reporting errors.
*  Parameters: pScriptSite -- The IActiveScriptSite interface pointer to cache
*  Returns: none
******************************************************************************/
void CASErrorHandler::SetScriptSite( IActiveScriptSite* pScriptSite )
{   
   //tracing purposes only
   CASERRORHANDLERTRACE("CASErrorHandler::SetScriptSite\n");

   // NOTE: Not using AddRef()!!!
   m_pScriptSite = pScriptSite;

   HRESULT hr = m_pScriptSite->QueryInterface( __uuidof(IActiveScriptSiteDebug), (void **) &m_pScriptSiteDebug );
   OutputDebugValues( "Result: %i, ScriptSiteDebug: %p\n", hr, m_pScriptSiteDebug );
}

/******************************************************************************
*  HandleCompileError -- This method handles reporting compile errors.
*  Parameters: pToken -- The token where the compile error occurred.
*
*              errorVal -- The enumerated error value that occurred, indicating
*                          which string to display.
*  Returns: none
******************************************************************************/
STDMETHODIMP CASErrorHandler::HandleCompileError( char *sDesc, ULONG ulLine, const char *sSource )
{
   HRESULT hr = E_FAIL;

   //tracing purposes only
   CASERRORHANDLERTRACE("CASErrorHandler::HandleCompileError\n");

   if( m_Desc )
   {
      SysFreeString( m_Desc );
      m_Desc = NULL;
   }

   if( m_Operation )
   {
      SysFreeString( m_Operation );
      m_Operation = NULL;
   }

   if( m_Source )
   {
      SysFreeString( m_Source );
      m_Source = NULL;
   }

   m_ulLine = ulLine;
   m_Desc   = hb_oleAnsiToSysString( sDesc );   
   m_Source = hb_oleAnsiToSysString( sSource );
   
   //Get the error handlers IActiveScriptError interface
   IActiveScriptError* pErrorInf = NULL;
   QueryInterface( IID_IActiveScriptError, (void**)&pErrorInf );

   //Call IActiveScriptSite::OnScriptError
   hr = m_pScriptSite->OnScriptError( pErrorInf );

   pErrorInf->Release();
   pErrorInf = NULL;

   if( SUCCEEDED( hr ) )
   {
	   OutputDebugValues( "COMPILATION Exception Handled.\n" );
	   return SCRIPT_E_REPORTED;//DISP_E_EXCEPTION; //SCRIPT_E_REPORTED;
   }

   return DISP_E_EXCEPTION;//hr;
}

/******************************************************************************
*  HandleRuntimeError -- This method handles reporting run-time errors
*  Parameters: pInstruction -- The instruction where the run-time error 
*                              occurred.
*              errorVal -- The enumerated error value that occurred, indicating
*                          which string to display.
*              pInterpreter -- The interpreter in which the runtime error 
*                              occurred.
*  Returns: none
******************************************************************************/
STDMETHODIMP CASErrorHandler::HandleRuntimeError( char *sDesc, char *sOperation, ULONG ulLine, const char *sSource, DebugResume* pInterpreter )
{
   HRESULT hr = E_FAIL;
   BOOL bCallOnScriptError, bEnterDebugger = FALSE;

   //tracing purposes only
   CASERRORHANDLERTRACE("CASErrorHandler::HandleRuntimeError\n");

   if( m_Desc )
   {
      SysFreeString( m_Desc );
      m_Desc = NULL;
   }

   if( m_Operation )
   {
      SysFreeString( m_Operation );
      m_Operation = NULL;
   }

   if( m_Source )
   {
      SysFreeString( m_Source );
      m_Source = NULL;
   }

   m_ulLine    = ulLine - 1;

   m_Desc      = hb_oleAnsiToSysString( sDesc );
   m_Operation = hb_oleAnsiToSysString( sOperation );
   m_Source    = hb_oleAnsiToSysString( sSource );

   //return DISP_E_EXCEPTION;

   __try
   {
      //Call IActiveScriptSite::OnScriptError
      if( m_pScriptSiteDebug )
      {
         bCallOnScriptError = FALSE;

         OutputDebugString( ">>> call: OnScriptErrorDebug()!!!\n" );
         hr = m_pScriptSiteDebug->OnScriptErrorDebug( static_cast<IActiveScriptErrorDebug*>(this), &bEnterDebugger, &bCallOnScriptError );
         OutputDebugValues( "Result: %p EnterDebugger: %i CallOnScriptError: %i\n", hr, bEnterDebugger, bCallOnScriptError );
      }
      else
      {
         bCallOnScriptError = TRUE;
      }
   }
   __except( EXCEPTION_EXECUTE_HANDLER )
   {
      OutputDebugString( "EXCEPTION Caught for OnScriptErrorDebug!\n" );
      hr = DISP_E_EXCEPTION;
   }

   if( FAILED(hr) || bCallOnScriptError )
   {
      //Get the error handlers IActiveScriptError interface
      IActiveScriptError* pErrorInf = NULL;

      __try
      {
         QueryInterface( IID_IActiveScriptError, (void**) &pErrorInf );
         hr = m_pScriptSite->OnScriptError( pErrorInf );
      }
      __except( EXCEPTION_EXECUTE_HANDLER )
      {
         OutputDebugString( "EXCEPTION Caught for OnScriptError!\n" );
         hr = DISP_E_EXCEPTION;
      }

      pErrorInf->Release();
      pErrorInf = NULL;

      if( SUCCEEDED( hr ) )
      {
	      OutputDebugValues( "RUN-TIME Error reported using OnScriptError.\n" );
	      return SCRIPT_E_REPORTED;//DISP_E_EXCEPTION; //SCRIPT_E_REPORTED;
      }
   }
   else
   {
	   OutputDebugValues( "RUN-TIME Error reported using OnScriptErrorDebug.\n" );
	   return SCRIPT_E_REPORTED;//DISP_E_EXCEPTION; //SCRIPT_E_REPORTED;
   }

   OutputDebugString( "Run-time error returned as DISP_E_EXCEPTION!!!\n" );

   return DISP_E_EXCEPTION;//hr;
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
STDMETHODIMP CASErrorHandler::QueryInterface(REFIID riid, void ** ppvObj)
{
   //tracing purposes only
   CASERRORHANDLERTRACE("CASErrorHandler::QueryInterface->");

   if (riid == IID_IUnknown)
   {
      CASERRORHANDLERTRACE("IUnknown\n");
      *ppvObj = static_cast<IActiveScriptError*>(this);
   }
   else if( riid == IID_IActiveScriptError )
   {
      CASERRORHANDLERTRACE("IActiveScriptError\n");
      *ppvObj = static_cast<IActiveScriptError*>(this);
   }
   else
   {
      CASERRORHANDLERTRACE("Unsupported Interface\n");
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
STDMETHODIMP_(ULONG) CASErrorHandler::AddRef()
{
   //tracing purposes only
   CASERRORHANDLERTRACE("CASErrorHandler::AddRef\n");

   return ++m_refCount;
}

/******************************************************************************
*   Release() -- When a reference to this object is removed, this function 
*   decrements the reference count.  If the reference count is 0, then this 
*   function deletes this object and returns 0;
******************************************************************************/
STDMETHODIMP_(ULONG) CASErrorHandler::Release()
{
   //tracing purposes only
   CASERRORHANDLERTRACE("CASErrorHandler::Release\n");

   --m_refCount;

   _ASSERTE( m_refCount >= 0 );

   if( m_refCount == 0 )
   {
      delete this;
      return 0;
   }

   return m_refCount;
}

/*******************************************************************************
*  IActiveScriptError interface -- This interface is used to provide info
*  about an error that has occurred in a running script engine.
******************************************************************************/

/******************************************************************************
*  GetExceptionInfo -- This method fills an EXCEPINFO structure with info
*  about an error that has occurred.
*  Parameters: pexcepinfo -- The address of the EXCEPINFO structure to fill
*  Returns: S_OK
*           E_FAIL
******************************************************************************/
STDMETHODIMP CASErrorHandler::GetExceptionInfo( EXCEPINFO *pexcepinfo )
{
   //tracing purposes only
   CASERRORHANDLERTRACE("CASErrorHandler::GetExceptionInfo\n");

   //check the argument to this method
   if( pexcepinfo == NULL )
   {
      return E_INVALIDARG;
   }

   memset( pexcepinfo, 0, sizeof( EXCEPINFO ) );

   //Fill in information on the error.  Most of the fields can be left empty.
   pexcepinfo->bstrSource = m_Operation;
   m_Operation = NULL;

   pexcepinfo->bstrDescription = m_Desc;
   m_Desc = NULL;

   pexcepinfo->scode = E_FAIL;
   
   return S_OK;
}

/******************************************************************************
*  GetSourcePosition -- This method retrieves information on the source context,
*  line number, and character position of the error.
*  Parameters: pdwSourceContext -- Address to fill with the source context of 
*                                  the error.
*              pulLineNumber -- Address to fill with the line number of the 
*                               error.
*              pichCharPosition -- Address to fill with the character position
*                                  of the error.
*  Returns: S_OK
*           E_FAIL
*******************************************************************************/
STDMETHODIMP CASErrorHandler::GetSourcePosition( DWORD *pdwSourceContext, ULONG *pulLineNumber, LONG *pichCharPosition )
{
   //tracing purposes only
   CASERRORHANDLERTRACE( "CASErrorHandler::GetSourcePosition\n" );

   //check the arguments to this method
   if( (pdwSourceContext == NULL) || (pulLineNumber == NULL) || (pichCharPosition == NULL) )
   {
	   OutputDebugString( "Invalid Arguments!\n" );
      return E_INVALIDARG;
   }
   
   if( m_Source == NULL || m_Source[0] == 0 )
   {
	   OutputDebugString( "Source not found!!\n" );
      return E_FAIL;
   }

   OutputDebugValues( "Returning Line: %i\n", m_ulLine );

   *pdwSourceContext = 0;
   *pulLineNumber = m_ulLine;
   *pichCharPosition = 0;

   return S_OK;
}

STDMETHODIMP CASErrorHandler::GetSourceLineText( BSTR *pbstrSourceLine )
{
   CASERRORHANDLERTRACE( "CASErrorHandler::GetSourceLineText\n" );

   if( pbstrSourceLine == NULL )
   {
	   OutputDebugString( "Invalid Arguments!\n" );
      return E_INVALIDARG;
   }

   if( m_Source && m_Source[0] )
   {
      OutputDebugValues( "Returning SourceLineText: '%S'\n", m_Source );
      *pbstrSourceLine = m_Source;
      m_Source = NULL;
      return S_OK;
   }

   OutputDebugString( "Source not found!!\n" );

   return E_FAIL;
}

/******************************************************************************
*  SetLocale -- This method sets, or resets, the locale used for displaying
*  error messages.  This method must be called once before any errors are 
*  reported.
*  Parameters:
*  Returns: none
******************************************************************************/
void CASErrorHandler::SetLocale( LCID newLCID )
{
}

/***** IActiveScriptErrorDebug Methods *****/
STDMETHODIMP CASErrorHandler::GetDocumentContext( IDebugDocumentContext** ppssc )
{
   OutputDebugString( "CASErrorHandler::GetDocumentContext()\n" );

   *ppssc = NULL;
   return E_NOTIMPL;
}

STDMETHODIMP CASErrorHandler::GetStackFrame( IDebugStackFrame**  ppdsf )
{
   OutputDebugString( "CASErrorHandler::GetStackFrame()\n" );

   *ppdsf = NULL;
   return E_NOTIMPL;
}
