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

//GUID for this script engine  {ADB21CAC-  9B03-  4D64-   90   97-  83   B0   57   41   FD   AF}
const CLSID CLSID_XBScript = {0xADB21CAC,0x9B03,0x4D64,{0x90,0x97,0x83,0xB0,0x57,0x41,0xFD,0xAF}};

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

