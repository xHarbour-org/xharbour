#ifndef _DMORT_H
#define _DMORT_H

/* DirectShow Media Object definitions */

STDAPI MoInitMediaType(DMO_MEDIA_TYPE*,DWORD);
STDAPI MoFreeMediaType(DMO_MEDIA_TYPE*);
STDAPI MoCopyMediaType(DMO_MEDIA_TYPE*, const DMO_MEDIA_TYPE*);
STDAPI MoCreateMediaType(DMO_MEDIA_TYPE**,DWORD);
STDAPI MoDeleteMediaType(DMO_MEDIA_TYPE*);
STDAPI MoDuplicateMediaType(DMO_MEDIA_TYPE**, const DMO_MEDIA_TYPE*);

#endif /* _DMORT_H */
