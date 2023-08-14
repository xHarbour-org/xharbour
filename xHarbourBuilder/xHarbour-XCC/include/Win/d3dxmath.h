#ifndef _D3DXMATH_H
#define _D3DXMATH_H

/* D3DX math types and functions */

#include <d3d.h>
#include <math.h>
#include <limits.h>
#include "d3dxerr.h"

#ifndef D3DXINLINE
#ifdef __cplusplus
#define D3DXINLINE inline
#else
#define D3DXINLINE _inline
#endif
#endif

#ifndef __PODXAPI
#if __POCC__ >= 274
#define __PODXAPI  __declspec(dllimport)
#else
#define __PODXAPI
#endif
#endif /* __PODXAPI */

typedef struct ID3DXMatrixStack *LPD3DXMATRIXSTACK;

DEFINE_GUID( IID_ID3DXMatrixStack,0xe3357330,0xcc5e,0x11d2,0xa4,0x34,0x0,0xa0,0xc9,0x6,0x29,0xa8);

#define D3DX_PI  ((float)3.141592654f)
#define D3DX_1BYPI  ((float)0.318309886f)

#define D3DXToRadian(degree)  ((degree) * (D3DX_PI / 180.0f))
#define D3DXToDegree(radian)  ((radian) * (180.0f / D3DX_PI))

typedef struct D3DXVECTOR2 {
#ifdef __cplusplus
public:
    D3DXVECTOR2() {};
    D3DXVECTOR2(const float *);
    D3DXVECTOR2(float, float);
    operator float* ();
    operator const float* () const;
    D3DXVECTOR2& operator += (const D3DXVECTOR2&);
    D3DXVECTOR2& operator -= (const D3DXVECTOR2&);
    D3DXVECTOR2& operator *= (float);
    D3DXVECTOR2& operator /= (float);
    D3DXVECTOR2 operator + () const;
    D3DXVECTOR2 operator - () const;
    D3DXVECTOR2 operator + (const D3DXVECTOR2&) const;
    D3DXVECTOR2 operator - (const D3DXVECTOR2&) const;
    D3DXVECTOR2 operator * (float) const;
    D3DXVECTOR2 operator / (float) const;
    friend D3DXVECTOR2 operator * (float, const D3DXVECTOR2&);
    BOOL operator == (const D3DXVECTOR2&) const;
    BOOL operator != (const D3DXVECTOR2&) const;

public:
#endif 
    float x, y;
} D3DXVECTOR2, *LPD3DXVECTOR2;

typedef struct D3DXVECTOR3 {
#ifdef __cplusplus
public:
    D3DXVECTOR3() {};
    D3DXVECTOR3(const float *);
    D3DXVECTOR3(const D3DVECTOR&);
    D3DXVECTOR3(float, float, float);
    operator float* ();
    operator const float* () const;
    operator D3DVECTOR* ();
    operator const D3DVECTOR* () const;
    operator D3DVECTOR& ();
    operator const D3DVECTOR& () const;
    D3DXVECTOR3& operator += (const D3DXVECTOR3&);
    D3DXVECTOR3& operator -= (const D3DXVECTOR3&);
    D3DXVECTOR3& operator *= (float);
    D3DXVECTOR3& operator /= (float);
    D3DXVECTOR3 operator + () const;
    D3DXVECTOR3 operator - () const;
    D3DXVECTOR3 operator + (const D3DXVECTOR3&) const;
    D3DXVECTOR3 operator - (const D3DXVECTOR3&) const;
    D3DXVECTOR3 operator * (float) const;
    D3DXVECTOR3 operator / (float) const;
    friend D3DXVECTOR3 operator * (float, const struct D3DXVECTOR3&);
    BOOL operator == (const D3DXVECTOR3&) const;
    BOOL operator != (const D3DXVECTOR3&) const;

public:
#endif 
    float x, y, z;
} D3DXVECTOR3, *LPD3DXVECTOR3;

typedef struct D3DXVECTOR4 {
#ifdef __cplusplus
public:
    D3DXVECTOR4() {};
    D3DXVECTOR4(const float*);
    D3DXVECTOR4(float, float, float, float);
    operator float* ();
    operator const float* () const;
    D3DXVECTOR4& operator += (const D3DXVECTOR4&);
    D3DXVECTOR4& operator -= (const D3DXVECTOR4&);
    D3DXVECTOR4& operator *= (float);
    D3DXVECTOR4& operator /= (float);
    D3DXVECTOR4 operator + () const;
    D3DXVECTOR4 operator - () const;
    D3DXVECTOR4 operator + (const D3DXVECTOR4&) const;
    D3DXVECTOR4 operator - (const D3DXVECTOR4&) const;
    D3DXVECTOR4 operator * (float) const;
    D3DXVECTOR4 operator / (float) const;
    friend D3DXVECTOR4 operator * (float, const D3DXVECTOR4&);
    BOOL operator == (const D3DXVECTOR4&) const;
    BOOL operator != (const D3DXVECTOR4&) const;

public:
#endif 
    float x, y, z, w;
} D3DXVECTOR4, *LPD3DXVECTOR4;

typedef struct D3DXMATRIX {
#ifdef __cplusplus
public:
    D3DXMATRIX() {};
    D3DXMATRIX(const float *);
    D3DXMATRIX(const D3DMATRIX&);
    D3DXMATRIX(float, float, float, float, float, float, float, float, float, float, float, float, float, float, float, float);
    float& operator () (UINT, UINT);
    float  operator () (UINT, UINT) const;
    operator float* ();
    operator const float* () const;
    operator D3DMATRIX* ();
    operator const D3DMATRIX* () const;
    operator D3DMATRIX& ();
    operator const D3DMATRIX& () const;
    D3DXMATRIX& operator *= (const D3DXMATRIX&);
    D3DXMATRIX& operator += (const D3DXMATRIX&);
    D3DXMATRIX& operator -= (const D3DXMATRIX&);
    D3DXMATRIX& operator *= (float);
    D3DXMATRIX& operator /= (float);
    D3DXMATRIX operator + () const;
    D3DXMATRIX operator - () const;
    D3DXMATRIX operator * (const D3DXMATRIX&) const;
    D3DXMATRIX operator + (const D3DXMATRIX&) const;
    D3DXMATRIX operator - (const D3DXMATRIX&) const;
    D3DXMATRIX operator * (float) const;
    D3DXMATRIX operator / (float) const;
    friend D3DXMATRIX operator * (float, const D3DXMATRIX&);
    BOOL operator == (const D3DXMATRIX&) const;
    BOOL operator != (const D3DXMATRIX&) const;
#endif 

    union {
        float m[4][4];
#ifdef __cplusplus
        struct {
            float m00, m01, m02, m03;
            float m10, m11, m12, m13;
            float m20, m21, m22, m23;
            float m30, m31, m32, m33;
        };
#endif 
    };
} D3DXMATRIX, *LPD3DXMATRIX;

typedef struct D3DXQUATERNION {
#ifdef __cplusplus
public:
    D3DXQUATERNION() {}
    D3DXQUATERNION(const float *);
    D3DXQUATERNION(float, float, float, float);
    operator float* ();
    operator const float* () const;
    D3DXQUATERNION& operator += (const D3DXQUATERNION&);
    D3DXQUATERNION& operator -= (const D3DXQUATERNION&);
    D3DXQUATERNION& operator *= (const D3DXQUATERNION&);
    D3DXQUATERNION& operator *= (float);
    D3DXQUATERNION& operator /= (float);
    D3DXQUATERNION  operator + () const;
    D3DXQUATERNION  operator - () const;
    D3DXQUATERNION operator + (const D3DXQUATERNION&) const;
    D3DXQUATERNION operator - (const D3DXQUATERNION&) const;
    D3DXQUATERNION operator * (const D3DXQUATERNION&) const;
    D3DXQUATERNION operator * (float) const;
    D3DXQUATERNION operator / (float) const;
    friend D3DXQUATERNION operator * (float, const D3DXQUATERNION&);
    BOOL operator == (const D3DXQUATERNION&) const;
    BOOL operator != (const D3DXQUATERNION&) const;
#endif 
    float x, y, z, w;
} D3DXQUATERNION, *LPD3DXQUATERNION;

typedef struct D3DXPLANE {
#ifdef __cplusplus
public:
    D3DXPLANE() {}
    D3DXPLANE(const float*);
    D3DXPLANE(float, float, float, float);
    operator float* ();
    operator const float* () const;
    D3DXPLANE operator + () const;
    D3DXPLANE operator - () const;
    BOOL operator == (const D3DXPLANE&) const;
    BOOL operator != (const D3DXPLANE&) const;
#endif 
    float a, b, c, d;
} D3DXPLANE, *LPD3DXPLANE;

typedef struct D3DXCOLOR {
#ifdef __cplusplus
public:
    D3DXCOLOR() {}
    D3DXCOLOR(DWORD argb);
    D3DXCOLOR(const float *);
    D3DXCOLOR(const D3DCOLORVALUE&);
    D3DXCOLOR(float, float, float, float);
    operator DWORD () const;
    operator float* ();
    operator const float* () const;
    operator D3DCOLORVALUE* ();
    operator const D3DCOLORVALUE* () const;
    operator D3DCOLORVALUE& ();
    operator const D3DCOLORVALUE& () const;
    D3DXCOLOR& operator += (const D3DXCOLOR&);
    D3DXCOLOR& operator -= (const D3DXCOLOR&);
    D3DXCOLOR& operator *= (float);
    D3DXCOLOR& operator /= (float);
    D3DXCOLOR operator + () const;
    D3DXCOLOR operator - () const;
    D3DXCOLOR operator + (const D3DXCOLOR&) const;
    D3DXCOLOR operator - (const D3DXCOLOR&) const;
    D3DXCOLOR operator * (float) const;
    D3DXCOLOR operator / (float) const;
    friend D3DXCOLOR operator * (float, const D3DXCOLOR&);
    BOOL operator == (const D3DXCOLOR&) const;
    BOOL operator != (const D3DXCOLOR&) const;
#endif 
    FLOAT r, g, b, a;
} D3DXCOLOR, *LPD3DXCOLOR;

float D3DXVec2Length(const D3DXVECTOR2*);
float D3DXVec2LengthSq(const D3DXVECTOR2*);
float D3DXVec2Dot(const D3DXVECTOR2*,const D3DXVECTOR2*);
float D3DXVec2CCW(const D3DXVECTOR2*,const D3DXVECTOR2*);
D3DXVECTOR2* D3DXVec2Add(D3DXVECTOR2*,const D3DXVECTOR2*,const D3DXVECTOR2*);
D3DXVECTOR2* D3DXVec2Subtract(D3DXVECTOR2*,const D3DXVECTOR2*,const D3DXVECTOR2*);
D3DXVECTOR2* D3DXVec2Minimize(D3DXVECTOR2*,const D3DXVECTOR2*,const D3DXVECTOR2*);
D3DXVECTOR2* D3DXVec2Maximize(D3DXVECTOR2*,const D3DXVECTOR2*,const D3DXVECTOR2*);
D3DXVECTOR2* D3DXVec2Scale(D3DXVECTOR2*,const D3DXVECTOR2*,float);
D3DXVECTOR2* D3DXVec2Lerp(D3DXVECTOR2*,const D3DXVECTOR2*,const D3DXVECTOR2*,float);

#ifdef __cplusplus
extern "C" {
#endif

__PODXAPI D3DXVECTOR2* WINAPI D3DXVec2Normalize(D3DXVECTOR2*,const D3DXVECTOR2*);
__PODXAPI D3DXVECTOR2* WINAPI D3DXVec2Hermite(D3DXVECTOR2*,const D3DXVECTOR2*,const D3DXVECTOR2*,const D3DXVECTOR2*,const D3DXVECTOR2*,float);
__PODXAPI D3DXVECTOR2* WINAPI D3DXVec2BaryCentric(D3DXVECTOR2*,const D3DXVECTOR2*,const D3DXVECTOR2*,D3DXVECTOR2*,float,float);
__PODXAPI D3DXVECTOR4* WINAPI D3DXVec2Transform(D3DXVECTOR4*,const D3DXVECTOR2*,const D3DXMATRIX*);
__PODXAPI D3DXVECTOR2* WINAPI D3DXVec2TransformCoord(D3DXVECTOR2*,const D3DXVECTOR2*,const D3DXMATRIX*);
__PODXAPI D3DXVECTOR2* WINAPI D3DXVec2TransformNormal(D3DXVECTOR2*,const D3DXVECTOR2*,const D3DXMATRIX*);

#ifdef __cplusplus
}
#endif

float D3DXVec3Length(const D3DXVECTOR3*);
float D3DXVec3LengthSq(const D3DXVECTOR3*);
float D3DXVec3Dot(const D3DXVECTOR3*,const D3DXVECTOR3*);
D3DXVECTOR3* D3DXVec3Cross(D3DXVECTOR3*,const D3DXVECTOR3*,const D3DXVECTOR3*);
D3DXVECTOR3* D3DXVec3Add(D3DXVECTOR3*,const D3DXVECTOR3*,const D3DXVECTOR3*);
D3DXVECTOR3* D3DXVec3Subtract(D3DXVECTOR3*,const D3DXVECTOR3*,const D3DXVECTOR3*);
D3DXVECTOR3* D3DXVec3Minimize(D3DXVECTOR3*,const D3DXVECTOR3*,const D3DXVECTOR3*);
D3DXVECTOR3* D3DXVec3Maximize(D3DXVECTOR3*,const D3DXVECTOR3*,const D3DXVECTOR3*);
D3DXVECTOR3* D3DXVec3Scale(D3DXVECTOR3*,const D3DXVECTOR3*,float);
D3DXVECTOR3* D3DXVec3Lerp(D3DXVECTOR3*,const D3DXVECTOR3*,const D3DXVECTOR3*,float);

#ifdef __cplusplus
extern "C" {
#endif

__PODXAPI D3DXVECTOR3* WINAPI D3DXVec3Normalize(D3DXVECTOR3*,const D3DXVECTOR3*);
__PODXAPI D3DXVECTOR3* WINAPI D3DXVec3Hermite(D3DXVECTOR3*,const D3DXVECTOR3*,const D3DXVECTOR3*,const D3DXVECTOR3*,const D3DXVECTOR3*,float);
__PODXAPI D3DXVECTOR3* WINAPI D3DXVec3BaryCentric(D3DXVECTOR3*,const D3DXVECTOR3*,const D3DXVECTOR3*,const D3DXVECTOR3*,float,float);
__PODXAPI D3DXVECTOR4* WINAPI D3DXVec3Transform(D3DXVECTOR4*,const D3DXVECTOR3*,const D3DXMATRIX*);
__PODXAPI D3DXVECTOR3* WINAPI D3DXVec3TransformCoord(D3DXVECTOR3*,const D3DXVECTOR3*,const D3DXMATRIX*);
__PODXAPI D3DXVECTOR3* WINAPI D3DXVec3TransformNormal(D3DXVECTOR3*,const D3DXVECTOR3*,const D3DXMATRIX*);

#ifdef __cplusplus
}
#endif

float D3DXVec4Length(const D3DXVECTOR4*);
float D3DXVec4LengthSq(const D3DXVECTOR4 *);
float D3DXVec4Dot(const D3DXVECTOR4*,const D3DXVECTOR4*);
D3DXVECTOR4* D3DXVec4Add(D3DXVECTOR4*,const D3DXVECTOR4*,const D3DXVECTOR4*);
D3DXVECTOR4* D3DXVec4Subtract(D3DXVECTOR4*,const D3DXVECTOR4*,const D3DXVECTOR4*);
D3DXVECTOR4* D3DXVec4Minimize(D3DXVECTOR4*,const D3DXVECTOR4*,const D3DXVECTOR4*);
D3DXVECTOR4* D3DXVec4Maximize( D3DXVECTOR4*, const D3DXVECTOR4*,const D3DXVECTOR4*);
D3DXVECTOR4* D3DXVec4Scale(D3DXVECTOR4*,const D3DXVECTOR4*,float);
D3DXVECTOR4* D3DXVec4Lerp(D3DXVECTOR4*,const D3DXVECTOR4*,const D3DXVECTOR4*,float);

#ifdef __cplusplus
extern "C" {
#endif

__PODXAPI D3DXVECTOR4* WINAPI D3DXVec4Cross(D3DXVECTOR4*,const D3DXVECTOR4*,const D3DXVECTOR4*,const D3DXVECTOR4*);
__PODXAPI D3DXVECTOR4* WINAPI D3DXVec4Normalize(D3DXVECTOR4*,const D3DXVECTOR4*);
__PODXAPI D3DXVECTOR4* WINAPI D3DXVec4Hermite(D3DXVECTOR4*,const D3DXVECTOR4*,const D3DXVECTOR4*,const D3DXVECTOR4*,const D3DXVECTOR4*,float);
__PODXAPI D3DXVECTOR4* WINAPI D3DXVec4BaryCentric(D3DXVECTOR4*,const D3DXVECTOR4*,const D3DXVECTOR4*,const D3DXVECTOR4*,float,float);
__PODXAPI D3DXVECTOR4* WINAPI D3DXVec4Transform(D3DXVECTOR4*,const D3DXVECTOR4*,const D3DXMATRIX*);

#ifdef __cplusplus
}
#endif

D3DXMATRIX* D3DXMatrixIdentity(D3DXMATRIX*);
BOOL D3DXMatrixIsIdentity(const D3DXMATRIX *);

#ifdef __cplusplus
extern "C" {
#endif

__PODXAPI float WINAPI D3DXMatrixfDeterminant(const D3DXMATRIX*);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixMultiply(D3DXMATRIX*,const D3DXMATRIX*,const D3DXMATRIX*);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixTranspose(D3DXMATRIX*,const D3DXMATRIX*);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixInverse(D3DXMATRIX*,float*,const D3DXMATRIX*);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixScaling(D3DXMATRIX*,float,float,float);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixTranslation(D3DXMATRIX*,float,float,float);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixRotationX(D3DXMATRIX*,float);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixRotationY(D3DXMATRIX*,float);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixRotationZ(D3DXMATRIX*,float);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixRotationAxis(D3DXMATRIX*,const D3DXVECTOR3*,float);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixRotationQuaternion(D3DXMATRIX*,const D3DXQUATERNION*);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixRotationYawPitchRoll(D3DXMATRIX*,float,float,float);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixTransformation(D3DXMATRIX*,const D3DXVECTOR3*,const D3DXQUATERNION*,const D3DXVECTOR3*,const D3DXVECTOR3*,const D3DXQUATERNION*,const D3DXVECTOR3*);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixAffineTransformation(D3DXMATRIX*,float,const D3DXVECTOR3*,const D3DXQUATERNION*,const D3DXVECTOR3*);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixLookAt(D3DXMATRIX*,const D3DXVECTOR3*,const D3DXVECTOR3*,const D3DXVECTOR3*);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixLookAtLH(D3DXMATRIX*,const D3DXVECTOR3*,const D3DXVECTOR3*,const D3DXVECTOR3*);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixPerspective(D3DXMATRIX*,float,float,float,float);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixPerspectiveLH(D3DXMATRIX*,float,float,float,float);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixPerspectiveFov(D3DXMATRIX*,float,float,float,float);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixPerspectiveFovLH(D3DXMATRIX*,float,float,float,float);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixPerspectiveOffCenter(D3DXMATRIX*,float,float,float,float,float,float);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixPerspectiveOffCenterLH(D3DXMATRIX*,float,float,float,float,float,float);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixOrtho(D3DXMATRIX*,float,float,float,float);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixOrthoLH(D3DXMATRIX*,float,float,float,float);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixOrthoOffCenter(D3DXMATRIX*,float,float,float,float,float,float);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixOrthoOffCenterLH(D3DXMATRIX*,float,float,float,float,float,float);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixShadow(D3DXMATRIX*,const D3DXVECTOR4*,const D3DXPLANE*);
__PODXAPI D3DXMATRIX* WINAPI D3DXMatrixReflect(D3DXMATRIX*,const D3DXPLANE*);

#ifdef __cplusplus
}
#endif

float D3DXQuaternionLength(const D3DXQUATERNION*);
float D3DXQuaternionLengthSq(const D3DXQUATERNION *);
float D3DXQuaternionDot(const D3DXQUATERNION*,const D3DXQUATERNION*);
D3DXQUATERNION* D3DXQuaternionIdentity(D3DXQUATERNION*);
BOOL D3DXQuaternionIsIdentity(const D3DXQUATERNION *);
D3DXQUATERNION* D3DXQuaternionConjugate(D3DXQUATERNION*,const D3DXQUATERNION*);

#ifdef __cplusplus
extern "C" {
#endif

__PODXAPI void WINAPI D3DXQuaternionToAxisAngle(const D3DXQUATERNION*,D3DXVECTOR3*,float*);
__PODXAPI D3DXQUATERNION* WINAPI D3DXQuaternionRotationMatrix(D3DXQUATERNION*,const D3DXMATRIX*);
__PODXAPI D3DXQUATERNION* WINAPI D3DXQuaternionRotationAxis(D3DXQUATERNION*,const D3DXVECTOR3*,float);
__PODXAPI D3DXQUATERNION* WINAPI D3DXQuaternionRotationYawPitchRoll(D3DXQUATERNION*,float,float,float);
__PODXAPI D3DXQUATERNION* WINAPI D3DXQuaternionMultiply(D3DXQUATERNION*,const D3DXQUATERNION*,const D3DXQUATERNION*);
__PODXAPI D3DXQUATERNION* WINAPI D3DXQuaternionNormalize(D3DXQUATERNION*,const D3DXQUATERNION*);
__PODXAPI D3DXQUATERNION* WINAPI D3DXQuaternionInverse(D3DXQUATERNION*,const D3DXQUATERNION*);
__PODXAPI D3DXQUATERNION* WINAPI D3DXQuaternionLn( D3DXQUATERNION*,const D3DXQUATERNION*);
__PODXAPI D3DXQUATERNION* WINAPI D3DXQuaternionExp( D3DXQUATERNION*,const D3DXQUATERNION*);
__PODXAPI D3DXQUATERNION* WINAPI D3DXQuaternionSlerp(D3DXQUATERNION*,const D3DXQUATERNION*,const D3DXQUATERNION*,float);
__PODXAPI D3DXQUATERNION* WINAPI D3DXQuaternionSquad(D3DXQUATERNION*,const D3DXQUATERNION*,const D3DXQUATERNION*,const D3DXQUATERNION*,const D3DXQUATERNION*,float);
__PODXAPI D3DXQUATERNION* WINAPI D3DXQuaternionBaryCentric(D3DXQUATERNION*,const D3DXQUATERNION*,const D3DXQUATERNION*,const D3DXQUATERNION*,float,float);

#ifdef __cplusplus
}
#endif

float D3DXPlaneDot(const D3DXPLANE*,const D3DXVECTOR4*);
float D3DXPlaneDotCoord(const D3DXPLANE*,const D3DXVECTOR3*);
float D3DXPlaneDotNormal(const D3DXPLANE*,const D3DXVECTOR3*);

#ifdef __cplusplus
extern "C" {
#endif

__PODXAPI D3DXPLANE* WINAPI D3DXPlaneNormalize(D3DXPLANE*,const D3DXPLANE*);
__PODXAPI D3DXVECTOR3* WINAPI D3DXPlaneIntersectLine(D3DXVECTOR3*,const D3DXPLANE*,const D3DXVECTOR3*,const D3DXVECTOR3*);
__PODXAPI D3DXPLANE* WINAPI D3DXPlaneFromPointNormal(D3DXPLANE *,const D3DXVECTOR3*,const D3DXVECTOR3*);
__PODXAPI D3DXPLANE* WINAPI D3DXPlaneFromPoints(D3DXPLANE*,const D3DXVECTOR3*,const D3DXVECTOR3*,const D3DXVECTOR3*);
__PODXAPI D3DXPLANE* WINAPI D3DXPlaneTransform(D3DXPLANE*,const D3DXPLANE*,const D3DXMATRIX*);

#ifdef __cplusplus
}
#endif

D3DXCOLOR* D3DXColorNegative(D3DXCOLOR*,const D3DXCOLOR*);
D3DXCOLOR* D3DXColorAdd(D3DXCOLOR*,const D3DXCOLOR*,const D3DXCOLOR*);
D3DXCOLOR* D3DXColorSubtract(D3DXCOLOR*,const D3DXCOLOR*,const D3DXCOLOR*);
D3DXCOLOR* D3DXColorScale(D3DXCOLOR*,const D3DXCOLOR*,float);
D3DXCOLOR* D3DXColorModulate(D3DXCOLOR*,const D3DXCOLOR*,const D3DXCOLOR*);
D3DXCOLOR* D3DXColorLerp(D3DXCOLOR*,const D3DXCOLOR*,const D3DXCOLOR*,float);

#ifdef __cplusplus
extern "C" {
#endif

__PODXAPI D3DXCOLOR* WINAPI D3DXColorAdjustSaturation(D3DXCOLOR*,const D3DXCOLOR*,float);
__PODXAPI D3DXCOLOR* WINAPI D3DXColorAdjustContrast(D3DXCOLOR*,const D3DXCOLOR*,float);

#ifdef __cplusplus
}
#endif

DECLARE_INTERFACE_(ID3DXMatrixStack, IUnknown)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,LPVOID*) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(Pop)(THIS) PURE;
    STDMETHOD(Push)(THIS) PURE;
    STDMETHOD(LoadIdentity)(THIS) PURE;
    STDMETHOD(LoadMatrix)(THIS_ const D3DXMATRIX*) PURE;
    STDMETHOD(MultMatrix)(THIS_ const D3DXMATRIX*) PURE;
    STDMETHOD(MultMatrixLocal)(THIS_ const D3DXMATRIX*) PURE;
    STDMETHOD(RotateAxis)(THIS_ const D3DXVECTOR3*,float) PURE;
    STDMETHOD(RotateAxisLocal)(THIS_ const D3DXVECTOR3*,float) PURE;
    STDMETHOD(RotateYawPitchRoll)(THIS_ float,float,float) PURE;
    STDMETHOD(RotateYawPitchRollLocal)(THIS_ float,float,float) PURE;
    STDMETHOD(Scale)(THIS_ float,float,float) PURE;
    STDMETHOD(ScaleLocal)(THIS_ float,float,float) PURE;
    STDMETHOD(Translate)(THIS_ float,float,float) PURE;
    STDMETHOD(TranslateLocal)(THIS_ float,float,float) PURE;
    STDMETHOD_(D3DXMATRIX*, GetTop)(THIS) PURE;
};

#ifdef __cplusplus
extern "C" {
#endif

__PODXAPI HRESULT WINAPI D3DXCreateMatrixStack(DWORD,LPD3DXMATRIXSTACK*);

#ifdef __cplusplus
}
#endif

#endif /* _D3DXMATH_H */
