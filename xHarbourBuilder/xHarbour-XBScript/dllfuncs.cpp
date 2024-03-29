/******************************************************************************
*
*  File: DLLFunctions.cpp
*
*  Author:  Joel Alley
*
*  Date: November 9, 1998
*
*  Description:   This file contains the definition of the standard, exported
*                 DLL functions.  It has two sets of registry values, one of
*                 which is commented out.  They correspond to the builds of
*                 the Active Script Engine with and without debugging support.
*
*  Modifications:
******************************************************************************/
#define CLASSFACTORY
#define DLLFUNCTIONS
#include "activeDepends.h"

//{ADB21CAC-9B03-4D64-9097-83B05741FDAF}
//GUID for this script engine  {ADB21CAC-  9B03-  4D64-   90   97-  83   B0   57   41   FD   AF}
const IID & CLSID_XBScript = { 0xADB21CAC,0x9B03,0x4D64,{ 0x90,0x97,0x83,0xB0,0x57,0x41,0xFD,0xAF } };

#ifndef VM_RESET
   static BOOL s_bInit = TRUE;
#endif

HINSTANCE g_hInstance;

extern void xbScript_atexit( void );

/******* Exported DLL functions *******/

/******************************************************************************
*  g_RegTable -- This N*3 array contains the keys, value names, and values that
*  are associated with this dll in the registry.
******************************************************************************/
char const * const g_RegTable [][3] = 
{
   //format is {key, value name, value }
   {"XBScript", 0, "XB Script Language"},
   {"XBScript\\CLSID", 0, "{ADB21CAC-9B03-4D64-9097-83B05741FDAF}"},
   {"XBScript\\OLEScript", 0, NULL},

   {"CLSID\\{ADB21CAC-9B03-4D64-9097-83B05741FDAF}", 0, "XB Script Language"},
   {"CLSID\\{ADB21CAC-9B03-4D64-9097-83B05741FDAF}\\Implemented Categories", 0, NULL },
   {"CLSID\\{ADB21CAC-9B03-4D64-9097-83B05741FDAF}\\Implemented Categories\\{F0B7A1A1-9847-11CF-8F20-00805F2CD064}", 0, NULL},
   {"CLSID\\{ADB21CAC-9B03-4D64-9097-83B05741FDAF}\\Implemented Categories\\{F0B7A1A2-9847-11CF-8F20-00805F2CD064}", 0, NULL},

   {"CLSID\\{ADB21CAC-9B03-4D64-9097-83B05741FDAF}\\InprocServer32", 0, (const char*)-1},
   {"CLSID\\{ADB21CAC-9B03-4D64-9097-83B05741FDAF}\\InprocServer32", "ThreadingModel", "Apartment" },

   {"CLSID\\{ADB21CAC-9B03-4D64-9097-83B05741FDAF}\\OLEScript", 0, NULL },
   {"CLSID\\{ADB21CAC-9B03-4D64-9097-83B05741FDAF}\\ProgId", 0, "XBScript"},
   //{"CLSID\\{ADB21CAC-9B03-4D64-9097-83B05741FDAF}\\VersionIndependentProgId", 0, "XBScript"},

      //Ron begin
   {".XBS", 0, "XBSFile" }, 
   {".XBS\\CLSID", 0, "{ADB21CAC-9B03-4D64-9097-83B05741FDAF}" },
       
   {"PROTOCOLS\\Handler\\xbscript", "CLSID", "{3050F3B2-98B5-11CF-BB82-00AA00BDCE0B}" },

   {"XBSFile", 0, "XBScript Script File"},
   {"XBSFile\\DefaultIcon", 0, "WScript.exe,3" },
   {"XBSFile\\ScriptEngine", 0, "XBScript" },
   {"XBSFile\\Shell", 0, NULL },
   {"XBSFile\\Shell\\Open", 0, "&Open" },
   {"XBSFile\\Shell\\Open\\Command", 0, "WScript.exe \"%1\" %*" },
   {"XBSFile\\Shell\\Open2", 0, "&Open &with command prompt" },
   {"XBSFile\\Shell\\Open2\\Command", 0, "CScript.exe \"%1\" %*" },
   //Ron end.

#if 0
   //format is {key, value name, value }
   {"DebugScript", 0, "DebugScript"},
   {"DebugScript\\CLSID", 0, "{51DBD0C1-77D7-11d2-94A2-006008939020}"},
   {"DebugScript\\OLEScript", 0, ""},

   {"DebugScript.1", 0, "DebugScript.1"},
   {"DebugScript.1\\CLSID", 0, "{51DBD0C1-77D7-11d2-94A2-006008939020}"},
   {"DebugScript.1\\OLEScript", 0, ""},

   {"CLSID\\{51DBD0C1-77D7-11d2-94A2-006008939020}", 0, "DebugScript"},
   {"CLSID\\{51DBD0C1-77D7-11d2-94A2-006008939020}\\Implemented Categories\\{F0B7A1A1-9847-11CF-8F20-00805F2CD064}", 0, ""},
   {"CLSID\\{51DBD0C1-77D7-11d2-94A2-006008939020}\\Implemented Categories\\{F0B7A1A2-9847-11CF-8F20-00805F2CD064}", 0, ""},


   //DebugScript lives in a DLL, so register it as InprocServer32
   //Server path is a rogue value to indicate DllRegisterServer should use the
   //current file path
   {"CLSID\\{51DBD0C1-77D7-11d2-94A2-006008939020}\\InprocServer32", 0, 
      (const char*)-1},
   {"CLSID\\{51DBD0C1-77D7-11d2-94A2-006008939020}\\InprocServer32", 
   "ThreadingModel", "Apartment" },
   {"CLSID\\{51DBD0C1-77D7-11d2-94A2-006008939020}\\OLEScript", 0, "" },
   {"CLSID\\{51DBD0C1-77D7-11d2-94A2-006008939020}\\ProgId", 0, "DebugScript.1"},
   {"CLSID\\{51DBD0C1-77D7-11d2-94A2-006008939020}\\VersionIndependentProgId", 0, "DebugScript"},
#endif
};

/******************************************************************************
*  DLLRegisterServer -- This method is the exported method that is used by
*  COM to self-register this component.  It removes the need for a .reg file.
*  ( Taken from Don Box's _Essential COM_ pg. 110-112)
******************************************************************************/
STDAPI DllRegisterServer(void)
{
	HRESULT hr = S_OK;

	//look up server's file name
	char szFileName[256];
	//HMODULE dllModule = GetModuleHandle( "XBScript.dll" );

	GetModuleFileName(g_hInstance, szFileName, 255);

	OutputDebugValues("Registring: %s\n", szFileName);

	//register entries from the table
	int nEntries = sizeof(g_RegTable) / sizeof(*g_RegTable);
	long err;

	for (int i = 0; i < nEntries; i++)
	{
		const char *pszName = g_RegTable[i][0];
		const char *pszValueName = g_RegTable[i][1];
		const char *pszValue = g_RegTable[i][2];

		//Map rogue values to module file name
		if (pszValue == (const char*)-1)
		{
			pszValue = szFileName;
		}

		//Create the key
		HKEY hkey;
		err = RegCreateKeyA(HKEY_CLASSES_ROOT, pszName, &hkey);

		//Set the value
		if (err == ERROR_SUCCESS)
		{
			OutputDebugValues("Created Key %s!!!\n", pszName);

			if (pszValue == NULL)
			{
				OutputDebugValues("Skipping SetValue: %s\n", pszName);
			}
			else
			{
				err = RegSetValueExA(hkey, pszValueName, 0, REG_SZ, (const BYTE*)pszValue, (pszValue == NULL ? 0 : (strlen(pszValue) + 1)));
				
				if (err != ERROR_SUCCESS)
				{
					OutputDebugValues("FAILED to Set Property %s in Key: %s\n", pszValueName, pszName);
					hr = SELFREG_E_CLASS;
				}
			}

			RegCloseKey(hkey);
		}
		else
		{
			OutputDebugValues("FAILED to Create Key %s!!!\n", pszName );
			hr = SELFREG_E_CLASS;
		}
	}

	//if cannot add key or value, back out and fail
	if (hr != S_OK)
	{
		DllUnregisterServer();
	}

    return hr;
}

/******************************************************************************
*  DllUnregisterServer -- This method is the exported method that is used by 
*  COM to remove the keys added to the registry by DllRegisterServer.  It
*  is essentially for housekeeping.
*  (Taken from Don Box, _Essential COM_ pg 112)
******************************************************************************/
STDAPI DllUnregisterServer(void)
{
   HRESULT hr = S_OK;

   int nEntries = sizeof(g_RegTable)/sizeof(*g_RegTable);

   for (int i = nEntries - 1; i >= 0; i--)
   {
      const char * pszKeyName = g_RegTable[i][0];

      long err = RegDeleteKeyA(HKEY_CLASSES_ROOT, pszKeyName);

	  if (err == ERROR_SUCCESS)
	  {
		  OutputDebugValues("Deleted Key: %s\n", pszKeyName);
	  }
	  else if (err == 2)
	  {
		  OutputDebugValues("Missing Key: %s\n", pszKeyName);
	  }
	  else
	  {
		  OutputDebugValues("FAILED!!! DeleteKey: %s\n", pszKeyName);
          hr = SELFREG_E_CLASS;
	  }
   }

   return hr;
}

/******************************************************************************
*	DllGetClassObject() -- This method is the exported method that clients use
*	to create objects in the DLL.  It uses class factories to generate the
*	desired object and returns it to the caller.  The caller must call Release()
*	on the object when they're through with it.
******************************************************************************/
STDAPI DllGetClassObject(REFCLSID rclsid, REFIID riid, LPVOID FAR * ppvObj)
{
	OutputDebugValues("DllGetClassObject(%p)\n", rclsid);

    //Make sure the requested class is supported by this server
   if( IsEqualCLSID( rclsid, CLSID_XBScript )  )
   {
      //Make sure the requested interface is supported
      if(( !IsEqualCLSID(riid, IID_IUnknown ) ) && ( !IsEqualCLSID(riid, IID_IClassFactory ) ) )
	  {
         return E_NOINTERFACE;
	  }

#ifndef VM_RESET
   	  if( s_bInit )
	  {
         s_bInit = FALSE;
		 OutputDebugValues( "Init VM...\n" );
		 
		 //Sleep( 10000 );
		 atexit( xbScript_atexit );
         
		 hb_vmInit( FALSE );
		 OutputDebugValues( "Done.\n" );
	  }
#endif
      //Create the class factory
      *ppvObj = (LPVOID) new XBScriptFactory();
   }
   else
   {
	  OutputDebugValues( "!!! WRONG Class ID!!!\n" );
      return E_FAIL;
   }
   
	//error checking
	if( *ppvObj == NULL )
	{
		return E_OUTOFMEMORY;
	}

	//Addref the Class Factory
	((LPUNKNOWN)*ppvObj)->AddRef();

	return NOERROR;
}

/******************************************************************************
*	DllCanUnloadNow() -- This method checks to see if it's alright to unload 
*	the dll by determining if there are currently any locks on the dll.
******************************************************************************/
STDAPI DllCanUnloadNow()
{
	if( g_cLock = 0 )
	{
		#ifndef VM_RESET
		   OutputDebugValues( "(%i) DllCanUnloadNow() -> Quiting VM...\n", g_cLock );

           #if 0 // WHY?
		      hb_vmPushSymbol( hb_dynsymGet( "__MVCLEAR" )->pSymbol );
              hb_vmPushNil();
		      hb_vmDo(0);
           #endif

           hb_gcSetCollecting( FALSE );

		   hb_vmQuit();

		   OutputDebugValues( "Done\n" );
		#endif

		return S_OK;
	}
	else
	{
		return S_FALSE;
	}
}

#if 1
BOOL WINAPI DllMain( HINSTANCE hInstance, DWORD fdwReason, LPVOID lpvReserved )
{
   OutputDebugString( "DllMain\n" );
    
   switch( fdwReason )
   {
      case DLL_PROCESS_ATTACH:
      {
         OutputDebugString( "DllMain() -> Attach\n" );

         DisableThreadLibraryCalls(hInstance);

		 g_hInstance = hInstance;
         break;
      }

      case DLL_PROCESS_DETACH:
         OutputDebugString( "DllMain() -> Detach\n" );
         break;

      default:
         OutputDebugString( "DllMain() -> Default\n" );
   }

   return TRUE;
}
#endif
