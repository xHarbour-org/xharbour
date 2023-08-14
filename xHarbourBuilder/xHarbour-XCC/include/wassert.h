#ifdef __POCC__

#include <assert.h>

#ifdef _DEBUG
#define _ASSERT_(exp)  assert(exp)
#else
#define _ASSERT_(exp)
#endif

#ifdef _DEBUG
#define _VERIFY_(exp)  assert(exp)
#else
#define _VERIFY_(exp)  (exp)
#endif

#define _SET_ASSERT_VERIFY_TRACE_MODE_
#define _TRACE0_(msg)
#define _TRACE1_(msg, arg1)
#define _TRACE2_(msg, arg1, arg2)
#define _TRACE3_(msg, arg1, arg2, arg3)
#define _TRACE4_(msg, arg1, arg2, arg3, arg4)
#define _TRACE5_(msg, arg1, arg2, arg3, arg4, arg5)
#define _TRACE6_(msg, arg1, arg2, arg3, arg4, arg5, arg6)

#else /* __POCC */

#ifdef _DEBUG
#define _ASSERT_(exp)  _ASSERT(exp)
#else
#define _ASSERT_(exp)
#endif

#ifdef _DEBUG
#define _VERIFY_(exp)  _ASSERT(exp)
#else
#define _VERIFY_(exp)  (exp)
#endif

#ifdef _DEBUG
#include <crtdbg.h>

#define _SET_ASSERT_VERIFY_TRACE_MODE_ \
{ \
    _CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_DEBUG | _CRTDBG_MODE_WNDW); \
    _CrtSetReportMode(_CRT_WARN, _CRTDBG_MODE_DEBUG); \
}

#define _TRACE0_(msg) \
    (void)_CrtDbgReport(_CRT_WARN, __FILE__, __LINE__, "", "%s", msg)

#define _TRACE1_(msg, arg1) \
    (void)_CrtDbgReport(_CRT_WARN, __FILE__, __LINE__, "", msg, arg1)

#define _TRACE2_(msg, arg1, arg2) \
    (void)_CrtDbgReport(_CRT_WARN, __FILE__, __LINE__, "", msg, arg1, arg2)

#define _TRACE3_(msg, arg1, arg2, arg3) \
    (void)_CrtDbgReport(_CRT_WARN, __FILE__, __LINE__, "", msg, arg1, arg2, arg3)

#define _TRACE4_(msg, arg1, arg2, arg3, arg4) \
    (void)_CrtDbgReport(_CRT_WARN, __FILE__, __LINE__, "", msg, arg1, arg2, arg3, arg4)

#define _TRACE5_(msg, arg1, arg2, arg3, arg4, arg5) \
    (void)_CrtDbgReport(_CRT_WARN, __FILE__, __LINE__, "", msg, arg1, arg2, arg3, arg4, arg5)

#define _TRACE6_(msg, arg1, arg2, arg3, arg4, arg5, arg6) \
    (void)_CrtDbgReport(_CRT_WARN, __FILE__, __LINE__, "", msg, arg1, arg2, arg3, arg4, arg5, arg6)

#else /* _DEBUG */
#define _SET_ASSERT_VERIFY_TRACE_MODE_
#define _TRACE0_(msg)
#define _TRACE1_(msg, arg1)
#define _TRACE2_(msg, arg1, arg2)
#define _TRACE3_(msg, arg1, arg2, arg3)
#define _TRACE4_(msg, arg1, arg2, arg3, arg4)
#define _TRACE5_(msg, arg1, arg2, arg3, arg4, arg5)
#define _TRACE6_(msg, arg1, arg2, arg3, arg4, arg5, arg6)
#endif /* _DEBUG */

#endif /* __POCC__ */
