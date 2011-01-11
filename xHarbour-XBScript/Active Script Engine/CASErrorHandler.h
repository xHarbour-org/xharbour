/******************************************************************************
*
*  File:CASErrorHandler.h
*
*  Author:  Joel Alley
*
*  Date: November 22, 1998
*
*  Description:   This file contains the declaration of the CASErrorHandler
*                 class, which extends the functionality of the CErrorHandler
*                 class to make use of the Active Scripting facilities when
*                 reporting errors.
*
*  Modifications:
******************************************************************************/

#ifndef _h_CASErrorHandler
#define _h_CASErrorHandler

#include "activdbg.h"

HB_EXTERN_BEGIN
   extern HB_EXPORT LPSTR hb_oleWideToAnsi( BSTR wString );
   extern HB_EXPORT BSTR hb_oleAnsiToSysString( const char *cString );
HB_EXTERN_END

class CASErrorHandler : public IActiveScriptErrorDebug
{
protected:
   IActiveScriptSite *m_pScriptSite;
   IActiveScriptSiteDebug *m_pScriptSiteDebug;

   int m_refCount;

public:
   BSTR m_Desc;
   BSTR m_Operation;
   int m_ulLine;
   BSTR m_Source;

   //Constructor
   CASErrorHandler();

   //Destructor
   ~CASErrorHandler();

   void SetScriptSite( IActiveScriptSite* pScriptSite );
   STDMETHODIMP HandleCompileError( char *sDesc, ULONG ulLine, const char *sSource );
   STDMETHODIMP HandleRuntimeError( char *sDesc, char * sOperation, ULONG ulLine, const char *sSource, DebugResume* pInterpreter );
   void SetLocale( LCID newLCID );

   /***** IUnknown Methods *****/
   STDMETHODIMP QueryInterface(REFIID riid, void**ppvObj);
   STDMETHODIMP_(ULONG) AddRef();
   STDMETHODIMP_(ULONG) Release();

   /***** IActiveScriptError Methods *****/
   STDMETHODIMP GetExceptionInfo( EXCEPINFO *pexcepinfo );
   STDMETHODIMP GetSourcePosition( DWORD *pdwSourceContext, ULONG *pulLineNumber, LONG *pichCharPosition );
   STDMETHODIMP GetSourceLineText( BSTR *pbstrSourceLine );

   /***** IActiveScriptErrorDebug Methods *****/
   STDMETHODIMP GetDocumentContext( IDebugDocumentContext** ppssc );
   STDMETHODIMP GetStackFrame( IDebugStackFrame**  ppdsf );
};

#endif
