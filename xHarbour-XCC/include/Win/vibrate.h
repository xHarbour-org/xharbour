#ifndef _VIBRATE_H
#define _VIBRATE_H

/* Vibrator API definitions (Windows CE - Smartphone) */

typedef struct {
    WORD wDuration;
    BYTE bAmplitude;
    BYTE bFrequency;
} VIBRATENOTE; 

typedef enum {
    VDC_AMPLITUDE,
    VDC_FREQUENCY,
    VDC_LAST
} VIBRATEDEVICECAPS;

HRESULT Vibrate(DWORD, const VIBRATENOTE*,BOOL,DWORD);
HRESULT VibrateStop(void);
int VibrateGetDeviceCaps(VIBRATEDEVICECAPS);

#endif /* _VIBRATE_H */
