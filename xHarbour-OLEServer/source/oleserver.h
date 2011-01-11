#ifndef _h_oleserver
  #define _h_oleserver

  typedef struct tagIDispatchObject
  {
    struct IDispatchVtbl *lpVtbl;
    IUnknown             *pUnkOuter;
    PHB_ITEM     m_pxObject;
    unsigned int m_uiMembers;
    PHB_ITEM     m_pxHashMembers;
    IID          m_InterfaceIID;
    PHB_ITEM     m_pxHashHandlers;
    ULONG        m_ulGenericPos;
    int m_iRef;
  } IDispatchObject;

HB_EXTERN_BEGIN

  extern HB_EXPORT LPSTR hb_oleWideToAnsi( BSTR wString );
  extern HB_EXPORT BSTR hb_oleAnsiToSysString( const char *cString );


#ifdef _DEBUG
  void OutputDebugValues( const char *sFormat, ... );
#endif

  IDispatch *OleWrap( PHB_ITEM pObject );

  HRESULT ItemToVariant( VARIANT *pVariant, PHB_ITEM pItem );
  HRESULT VariantToItem( PHB_ITEM pItem, VARIANT *pVariant );
  HRESULT STDMETHODCALLTYPE ErrorToException( PHB_ITEM pError, EXCEPINFO *pException, SCODE scode, const char *sSource );

  static HRESULT STDMETHODCALLTYPE ClassFactoryQueryInterface( IClassFactory *This, REFIID riid, void **ppvObj );
  static ULONG   STDMETHODCALLTYPE ClassFactoryAddRef( IClassFactory *This );
  static ULONG   STDMETHODCALLTYPE ClassFactoryRelease( IClassFactory *This );
  static HRESULT STDMETHODCALLTYPE ClassFactoryCreateInstance( IClassFactory *This, IUnknown * pUnkOuter, REFIID riid,  void ** ppvObject );
  static HRESULT STDMETHODCALLTYPE ClassFactoryLockServer( IClassFactory *This, BOOL fLock );

  static HRESULT STDMETHODCALLTYPE DispatchObjectQueryInterface( IDispatch *This, REFIID riid, void **ppvObj );
  static ULONG   STDMETHODCALLTYPE DispatchObjectAddRef( IDispatch *This );
  static ULONG   STDMETHODCALLTYPE DispatchObjectRelease( IDispatch *This );
  static HRESULT STDMETHODCALLTYPE DispatchObjectGetTypeInfoCount( IDispatch *This, UINT *iTInfo );
  static HRESULT STDMETHODCALLTYPE DispatchObjectGetTypeInfo( IDispatch *This, UINT iTInfo, LCID lcid, ITypeInfo **ppTInfo );
  static HRESULT STDMETHODCALLTYPE DispatchObjectGetIDsOfNames( IDispatch *This, REFIID riid, LPOLESTR *pNames, UINT iNames, LCID lcid, DISPID *pDispID );
  static HRESULT STDMETHODCALLTYPE DispatchObjectInvoke( IDispatch *This, DISPID DispID, REFIID riid, LCID lcid, WORD wFlags, DISPPARAMS *pParams, VARIANT *pResult, EXCEPINFO *pExecpInfo, UINT *puArgErr );

HB_EXTERN_END

#endif
