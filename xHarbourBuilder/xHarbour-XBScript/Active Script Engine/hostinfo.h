// This file comes from Microsoft's "SamScrpt" script sample engine 
// source code: 
//   SamScrpt.exe 
//   (http://support.microsoft.com/download/support/mslfiles/SamScrpt.exe) 

/******************************************************************************
*
*  File: HostInfo.h
*
*  Author:  Joel Alley
*
*  Date: June 4, 1998
*
*  Description:   This file contains the declaration of two interfaces
*                 necessary for an ActiveX Script Host to change the LCID that
*                 VBScript.dll uses for error messages.
*
*  Modifications:
******************************************************************************/
#ifndef HOST_INFO_H_INCLUDED 
#define HOST_INFO_H_INCLUDED 

extern "C"
{

#if __DMC__ 
	extern const IID IID_IHostInfoUpdate;
#else 
	// {1D044690-8923-11d0-ABD2-00A0C911E8B2} 
	const GUID IID_IHostInfoUpdate =
	{ 0x1d044690, 0x8923, 0x11d0,{ 0xab, 0xd2, 0x0, 0xa0, 0xc9, 0x11, 0xe8, 0xb2 } };
#endif 

	enum hostinfo
	{
		hostinfoLocale = 0,
		hostinfoCodePage = 1,
		hostinfoErrorLocale = 2
	};

	class IHostInfoUpdate : public IUnknown
	{
	public:
		// *** IUnknown Methods *** 
		STDMETHOD(QueryInterface)(REFIID riid, void **ppvObj) = 0;
		STDMETHOD_(ULONG, AddRef)(void) = 0;
		STDMETHOD_(ULONG, Release)(void) = 0;

		// *** IHostInfoUpdate Methods *** 
		STDMETHOD(UpdateInfo)(hostinfo hostinfoNew) = 0;
	};

#if __DMC__ 
	extern const IID IID_IHostInfoProvider;
#else 
	// {F8418AE0-9A5D-11d0-ABD4-00A0C911E8B2} 
	const GUID IID_IHostInfoProvider =
	{ 0xf8418ae0, 0x9a5d, 0x11d0,{ 0xab, 0xd4, 0x0, 0xa0, 0xc9, 0x11, 0xe8, 0xb2 } };
#endif 

	class IHostInfoProvider : public IUnknown
	{

	public:

		// *** IUnknown Methods *** 
		STDMETHOD(QueryInterface)(REFIID riid, void **ppvObj) = 0;
		STDMETHOD_(ULONG, AddRef)(void) = 0;
		STDMETHOD_(ULONG, Release)(void) = 0;

		// *** IHostInfoProvider Methods *** 
		STDMETHOD(GetHostInfo)(hostinfo hostinfoRequest, void * * ppvInfo) = 0;
	};

}

#endif // HOST_INFO_H_INCLUDED