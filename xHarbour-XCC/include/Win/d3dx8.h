#ifndef _D3DX8_H
#define _D3DX8_H

/* D3DX utility library */

#include "d3d8.h"
#include <limits.h>

#ifndef D3DXINLINE
#ifdef __cplusplus
#define D3DXINLINE inline
#else
#define D3DXINLINE
#endif
#endif /* D3DXINLINE */

#define D3DX_DEFAULT  ULONG_MAX
#define D3DX_DEFAULT_FLOAT  FLT_MAX

#include "d3dx8math.h"
#include "d3dx8core.h"
#include "d3dx8tex.h"
#include "d3dx8mesh.h"
#include "d3dx8shape.h"
#include "d3dx8effect.h"

#endif /* _D3DX8_H */

