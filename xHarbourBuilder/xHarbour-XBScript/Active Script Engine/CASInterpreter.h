/******************************************************************************
*
*  File: CASInterpreter.h
*
*  Author:  Joel Alley
*
*  Date: October 3, 1998
*
*  Description:   This file contains the declaration of the CASInterpreter
*                 class, which serves as the link between a normal script 
*                 interpreter and an Active Script Engine by deriving from the 
*                 CInterpreter class.  It adds the IDispatch and IDispatchEx 
*                 interfaces, as well as COM functionality, so other 
*                 CASInterpreters and external code can call methods and 
*                 variables in this interpreter.
*
*  Modifications:
******************************************************************************/

#ifndef _h_CASInterpreter
#define _h_CASInterpreter

#include "..\..\xHarbour-OleServer\source\oleserver.h"

extern HB_EXPORT BSTR hb_oleAnsiToSysString( const char *cString );

void ResetVM( void );

#define MAX_SCRIPT_SIZE 262144

class XBScript;

//Maps a DISPID to an LPCOLESTR for IDispatch methods
struct MemberInfo
{
   LPCOLESTR m_name;
   DISPID m_dispid;
   BOOL m_isMethod;
};

class CASInterpreter: public IDispatch 
                      /*,public CInterpreter*/
{

protected:
   int m_refCount;
   WCHAR m_Name[256];
   TList<NamedItem*>  *m_pNamedItems;

   static int sm_CurrentDispid;

   ULONG m_ulStartLine;
   char  *m_sCode;
   BOOL  m_bAppendText;

public:
   BOOL  m_bAddedGlobals;
   char  m_sName[256];
   PHB_ITEM m_pInterpreter;
   BOOL m_bExecuted;
   IDispatch *m_pOleWrapDispatch;
   XBScript *m_pEngine;

public:
   //Constructor
   CASInterpreter( LPCOLESTR name, XBScript *pEngine );

   //Destructor
   ~CASInterpreter();


   /***** IUnknown Methods *****/
   STDMETHODIMP QueryInterface(REFIID riid, void**ppvObj);
   STDMETHODIMP_(ULONG) AddRef();
   STDMETHODIMP_(ULONG) Release();

   /***** IDispatch Methods *****/
   STDMETHODIMP GetTypeInfoCount(UINT* iTInfo);
   STDMETHODIMP GetTypeInfo(UINT iTInfo, LCID lcid, ITypeInfo** ppTInfo);
   STDMETHODIMP GetIDsOfNames(REFIID riid, OLECHAR** rgszNames,UINT cNames, LCID lcid, DISPID* rgDispId);
   STDMETHODIMP Invoke(DISPID dispIdMember, REFIID riid, LCID lcid, WORD wFlags, DISPPARAMS* pDispParams,  VARIANT* pVarResult, EXCEPINFO* pExcepInfo,  UINT* puArgErr);


   /***** CASInterpreter Methods *****/
   LPCOLESTR GetName() { return m_Name; }
   HRESULT ParseText( LPCOLESTR scriptText, ULONG startingLineNumber, DWORD dwSourceContext );
   
   HRESULT EvaluateImmediate();
   void Reset( BOOL bReleaseCode );
   
   HRESULT AutomationHelper( IDispatch* pDispatch, LPCOLESTR lpzName, VARIANT *pArgs, VARIANT *pReturnValue, WORD dwFlags, unsigned int cArgs );

   STDMETHODIMP ResolveSiteGlobals( LPCOLESTR method, VARIANT *pArgs, VARIANT *pRetValue, unsigned cArgs, WORD dwFlags );

   STDMETHODIMP CASInterpreter::ConcileNamedItems( TList<NamedItem*>* pNamedItems );
   STDMETHODIMP CASInterpreter::ConcileDispatch( NamedItem* pNamedItem );

   const char * CASInterpreter::GetSourceLineText( ULONG iLine );

   // Wrappers to PP Methods.
   STDMETHODIMP SetScript();
   STDMETHODIMP Compile();
   STDMETHODIMP HandleReturnedError();
   STDMETHODIMP Run();
   STDMETHODIMP Eval( LPCOLESTR wExp, unsigned int cArgs, VARIANT *pArgs, VARIANT *pRetValue, BOOL bMethod );
   STDMETHODIMP GetPRGInterpreter();
};



  #ifndef XBSCRIPT
    #define _UNDEF_XBSCRIPT
    #define XBSCRIPT
  #endif

  #include "activeDepends.h"
   
  #ifdef _UNDEF_XBSCRIPT
    #undef _UNDEF_XBSCRIPT
    #undef XBSCRIPT
  #endif
#endif