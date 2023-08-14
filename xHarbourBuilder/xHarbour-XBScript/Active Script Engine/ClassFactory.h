/******************************************************************************
*
*  File: SampleClassFactory.h
*
*  Author:  Joel Alley
*
*  Date: November 9, 1998
*
*  Description:   This file contains the declaration of the Class Factory for
*                 the sample script engine.  All the methods are fairly generic,
*                 and nothing specific to scripting occurs here.
*  Modifications:
******************************************************************************/

#ifndef _h_ClassFactory
#define _h_ClassFactory

extern int g_cLock;

class XBScriptFactory : public IClassFactory
{
protected:
	ULONG m_refCount;	//reference count

public:
	XBScriptFactory();
	~XBScriptFactory();

	/******* IUnknown Methods *******/
	STDMETHODIMP QueryInterface(REFIID riid, LPVOID* ppvObj);
	STDMETHODIMP_(ULONG) AddRef();
	STDMETHODIMP_(ULONG) Release();

	/******* IClassFactory Methods *******/
	STDMETHODIMP CreateInstance(LPUNKNOWN, REFIID, LPVOID *);
	STDMETHODIMP LockServer(BOOL);
};
#endif

