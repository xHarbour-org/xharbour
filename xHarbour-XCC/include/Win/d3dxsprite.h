#ifndef _D3DXSPRITE_H
#define _D3DXSPRITE_H

/* D3DX sprite helper functions */

#include <d3d.h>
#include <limits.h>
#include "d3dxerr.h"

#ifndef __PODXAPI
#if __POCC__ >= 274
#define __PODXAPI  __declspec(dllimport)
#else
#define __PODXAPI
#endif
#endif /* __PODXAPI */

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __cplusplus
HRESULT WINAPI D3DXPrepareDeviceForSprite(LPDIRECT3DDEVICE7,BOOL ZEnable = FALSE);
#else
__PODXAPI HRESULT WINAPI D3DXPrepareDeviceForSprite(LPDIRECT3DDEVICE7,BOOL);
#endif

#ifdef __cplusplus
HRESULT WINAPI D3DXDrawSpriteSimple(LPDIRECTDRAWSURFACE7,LPDIRECT3DDEVICE7,const D3DXVECTOR3*,float alpha = 1.0f,float scale = 1.0f,float angleRad = 0.0f,const D3DXVECTOR2 *pOffset = NULL,const RECT *pSourceRect = NULL);
#else
__PODXAPI HRESULT WINAPI D3DXDrawSpriteSimple(LPDIRECTDRAWSURFACE7,LPDIRECT3DDEVICE7,D3DXVECTOR3*,float,float,float,D3DXVECTOR2*,RECT*);
#endif

#ifdef __cplusplus
HRESULT WINAPI D3DXDrawSpriteTransform(LPDIRECTDRAWSURFACE7,LPDIRECT3DDEVICE7,const D3DXMATRIX*,float alpha = 1.0f,const RECT *pSourceRect  = NULL);
#else
__PODXAPI HRESULT WINAPI D3DXDrawSpriteTransform(LPDIRECTDRAWSURFACE7,LPDIRECT3DDEVICE7,D3DXMATRIX*,float,RECT*);
#endif

#ifdef __cplusplus
void WINAPI D3DXBuildSpriteTransform(D3DXMATRIX*,const RECT*,float angleRad = 0.0f,const D3DXVECTOR2 *pOffset = NULL);
#else
__PODXAPI void WINAPI D3DXBuildSpriteTransform(D3DXMATRIX*,RECT*,float,D3DXVECTOR2*);
#endif

#ifdef __cplusplus
HRESULT WINAPI D3DXDrawSprite3D(LPDIRECTDRAWSURFACE7,LPDIRECT3DDEVICE7,const D3DXVECTOR4 [4],float alpha = 1.0f,const RECT *pSourceRect = NULL);
#else
__PODXAPI HRESULT WINAPI D3DXDrawSprite3D(LPDIRECTDRAWSURFACE7,LPDIRECT3DDEVICE7,D3DXVECTOR4 [4],float,RECT*);
#endif

#ifdef __cplusplus
}
#endif

#endif /* _D3DXSPRITE_H */
