#include "hbapi.h"

/*
 We should undef macro calls and access the functions from DLL
*/
#undef hb_pcount
#undef hb_ret
#undef hb_reta
#undef hb_retc
#undef hb_retclen
#undef hb_retcAdopt
#undef hb_retclenAdopt
#undef hb_retcAdoptStatic
#undef hb_retclenAdoptStatic
#undef hb_retclenAdoptRaw
#undef hb_retclenAdoptRawStatic
#undef hb_retds
#undef hb_retd
#undef hb_retdl
#undef hb_retl
#undef hb_retnd
#undef hb_retni
#undef hb_retnl
#undef hb_retnlen
#undef hb_retndlen
#undef hb_retnilen
#undef hb_retnllen
#undef hb_retptr
#undef hb_retnll
#undef hb_retnlllen
#undef hb_stackItemFromTop
#undef hb_stackItemFromBase
#undef hb_stackTopOffset
#undef hb_stackBaseOffset
#undef hb_stackTopItem
#undef hb_stackBaseItem
#undef hb_stackSelfItem
#undef hb_stackItem
