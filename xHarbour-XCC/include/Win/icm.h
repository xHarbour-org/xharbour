#ifndef _ICM_H
#define _ICM_H

/* Image Color Management definitions */

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_COLOR_CHANNELS  8

#define CLASS_MONITOR  'mntr'
#define CLASS_PRINTER  'prtr'
#define CLASS_SCANNER  'scnr'
#define CLASS_LINK  'link'
#define CLASS_ABSTRACT  'abst'
#define CLASS_COLORSPACE  'spac'
#define CLASS_NAMED  'nmcl'

#define SPACE_XYZ  'XYZ '
#define SPACE_Lab  'Lab '
#define SPACE_Luv  'Luv '
#define SPACE_YCbCr  'YCbr'
#define SPACE_Yxy  'Yxy '
#define SPACE_RGB  'RGB '
#define SPACE_GRAY  'GRAY'
#define SPACE_HSV  'HSV '
#define SPACE_HLS  'HLS '
#define SPACE_CMYK  'CMYK'
#define SPACE_CMY  'CMY '
#define SPACE_2_CHANNEL  '2CLR'
#define SPACE_3_CHANNEL  '3CLR'
#define SPACE_4_CHANNEL  '4CLR'
#define SPACE_5_CHANNEL  '5CLR'
#define SPACE_6_CHANNEL  '6CLR'
#define SPACE_7_CHANNEL  '7CLR'
#define SPACE_8_CHANNEL  '8CLR'

#define FLAG_EMBEDDEDPROFILE  0x00000001
#define FLAG_DEPENDENTONDATA  0x00000002

#define ATTRIB_TRANSPARENCY  0x00000001
#define ATTRIB_MATTE  0x00000002

#define INTENT_PERCEPTUAL  0
#define INTENT_RELATIVE_COLORIMETRIC  1
#define INTENT_SATURATION  2
#define INTENT_ABSOLUTE_COLORIMETRIC  3

#define PROFILE_FILENAME  1
#define PROFILE_MEMBUFFER  2

#define PROFILE_READ  1
#define PROFILE_READWRITE  2

#define INDEX_DONT_CARE  0

#define CMM_FROM_PROFILE  INDEX_DONT_CARE
#define CMM_WINDOWS_DEFAULT 'Win '

#define ENUM_TYPE_VERSION  0x0300

#define ET_DEVICENAME  0x00000001
#define ET_MEDIATYPE  0x00000002
#define ET_DITHERMODE  0x00000004
#define ET_RESOLUTION  0x00000008
#define ET_CMMTYPE  0x00000010
#define ET_CLASS  0x00000020
#define ET_DATACOLORSPACE  0x00000040
#define ET_CONNECTIONSPACE  0x00000080
#define ET_SIGNATURE  0x00000100
#define ET_PLATFORM  0x00000200
#define ET_PROFILEFLAGS  0x00000400
#define ET_MANUFACTURER  0x00000800
#define ET_MODEL  0x00001000
#define ET_ATTRIBUTES  0x00002000
#define ET_RENDERINGINTENT  0x00004000
#define ET_CREATOR  0x00008000
#define ET_DEVICECLASS  0x00010000

#define PROOF_MODE  0x00000001
#define NORMAL_MODE  0x00000002
#define BEST_MODE  0x00000003
#define ENABLE_GAMUT_CHECKING  0x00010000
#define USE_RELATIVE_COLORIMETRIC  0x00020000
#define FAST_TRANSLATE  0x00040000
#define RESERVED  0x80000000

#define CSA_A  1
#define CSA_ABC  2
#define CSA_DEF  3
#define CSA_DEFG  4
#define CSA_GRAY  5
#define CSA_RGB  6
#define CSA_CMYK  7
#define CSA_Lab  8

#define CMM_WIN_VERSION  0
#define CMM_IDENT  1
#define CMM_DRIVER_VERSION  2
#define CMM_DLL_VERSION  3
#define CMM_VERSION  4
#define CMM_DESCRIPTION  5
#define CMM_LOGOICON  6

#define CMS_FORWARD  0
#define CMS_BACKWARD  1

#define COLOR_MATCH_VERSION  0x0200

#define CMS_DISABLEICM  1
#define CMS_ENABLEPROOFING  2

#define CMS_SETRENDERINTENT  4
#define CMS_SETPROOFINTENT  8
#define CMS_SETMONITORPROFILE  0x10
#define CMS_SETPRINTERPROFILE  0x20
#define CMS_SETTARGETPROFILE  0x40

#define CMS_USEHOOK  0x80
#define CMS_USEAPPLYCALLBACK  0x100
#define CMS_USEDESCRIPTION  0x200

#define CMS_DISABLEINTENT  0x400
#define CMS_DISABLERENDERINTENT 0x800

#define CMS_MONITOROVERFLOW  0x80000000L
#define CMS_PRINTEROVERFLOW  0x40000000L
#define CMS_TARGETOVERFLOW  0x20000000L

typedef char COLOR_NAME[32];
typedef COLOR_NAME *PCOLOR_NAME, *LPCOLOR_NAME;

typedef struct tagNAMED_PROFILE_INFO {
    DWORD dwFlags;
    DWORD dwCount;
    DWORD dwCountDevCoordinates;
    COLOR_NAME szPrefix;
    COLOR_NAME szSuffix;
} NAMED_PROFILE_INFO;
typedef NAMED_PROFILE_INFO *PNAMED_PROFILE_INFO, *LPNAMED_PROFILE_INFO;

struct GRAYCOLOR {
    WORD gray;
};

struct RGBCOLOR {
    WORD red;
    WORD green;
    WORD blue;
};

struct CMYKCOLOR {
    WORD cyan;
    WORD magenta;
    WORD yellow;
    WORD black;
};

struct XYZCOLOR {
    WORD X;
    WORD Y;
    WORD Z;
};

struct YxyCOLOR {
    WORD Y;
    WORD x;
    WORD y;
};

struct LabCOLOR {
    WORD L;
    WORD a;
    WORD b;
};

struct GENERIC3CHANNEL {
    WORD ch1;
    WORD ch2;
    WORD ch3;
};

struct NAMEDCOLOR {
    DWORD dwIndex;
};

struct HiFiCOLOR {
    BYTE channel[MAX_COLOR_CHANNELS];
};

typedef union tagCOLOR {
    struct GRAYCOLOR gray;
    struct RGBCOLOR rgb;
    struct CMYKCOLOR cmyk;
    struct XYZCOLOR XYZ;
    struct YxyCOLOR Yxy;
    struct LabCOLOR Lab;
    struct GENERIC3CHANNEL gen3ch;
    struct NAMEDCOLOR named;
    struct HiFiCOLOR hifi;
} COLOR, *PCOLOR, *LPCOLOR;

typedef enum {
    COLOR_GRAY = 1,
    COLOR_RGB,
    COLOR_XYZ,
    COLOR_Yxy,
    COLOR_Lab,
    COLOR_3_CHANNEL,
    COLOR_CMYK,
    COLOR_5_CHANNEL,
    COLOR_6_CHANNEL,
    COLOR_7_CHANNEL,
    COLOR_8_CHANNEL,
    COLOR_NAMED,
} COLORTYPE, *PCOLORTYPE, *LPCOLORTYPE;

typedef enum {
    BM_x555RGB = 0x0000,
    BM_x555XYZ = 0x0101,
    BM_x555Yxy,
    BM_x555Lab,
    BM_x555G3CH,
    BM_RGBTRIPLETS = 0x0002,
    BM_BGRTRIPLETS = 0x0004,
    BM_XYZTRIPLETS = 0x0201,
    BM_YxyTRIPLETS,
    BM_LabTRIPLETS,
    BM_G3CHTRIPLETS,
    BM_5CHANNEL,
    BM_6CHANNEL,
    BM_7CHANNEL,
    BM_8CHANNEL,
    BM_GRAY,
    BM_xRGBQUADS = 0x0008,
    BM_xBGRQUADS = 0x0010,
    BM_xG3CHQUADS = 0x0304,
    BM_KYMCQUADS,
    BM_CMYKQUADS = 0x0020,
    BM_10b_RGB = 0x0009,
    BM_10b_XYZ = 0x0401,
    BM_10b_Yxy,
    BM_10b_Lab,
    BM_10b_G3CH,
    BM_NAMED_INDEX,
    BM_16b_RGB = 0x000A,
    BM_16b_XYZ = 0x0501,
    BM_16b_Yxy,
    BM_16b_Lab,
    BM_16b_G3CH,
    BM_16b_GRAY,
    BM_565RGB = 0x0001,
} BMFORMAT, *PBMFORMAT, *LPBMFORMAT;

typedef BOOL(WINAPI *PBMCALLBACKFN)(ULONG,ULONG,LPARAM);
typedef PBMCALLBACKFN LPBMCALLBACKFN;

typedef struct tagPROFILEHEADER {
    DWORD phSize;
    DWORD phCMMType;
    DWORD phVersion;
    DWORD phClass;
    DWORD phDataColorSpace;
    DWORD phConnectionSpace;
    DWORD phDateTime[3];
    DWORD phSignature;
    DWORD phPlatform;
    DWORD phProfileFlags;
    DWORD phManufacturer;
    DWORD phModel;
    DWORD phAttributes[2];
    DWORD phRenderingIntent;
    CIEXYZ phIlluminant;
    DWORD phCreator;
    BYTE phReserved[44];
} PROFILEHEADER;
typedef PROFILEHEADER *PPROFILEHEADER, *LPPROFILEHEADER;

typedef struct tagPROFILE {
    DWORD dwType;
    PVOID pProfileData;
    DWORD cbDataSize;
} PROFILE, *PPROFILE, *LPPROFILE;

typedef HANDLE HPROFILE;
typedef HPROFILE *PHPROFILE;
typedef HANDLE HTRANSFORM;

typedef DWORD TAGTYPE;
typedef TAGTYPE *PTAGTYPE, *LPTAGTYPE;

typedef struct tagENUMTYPEA {
    DWORD dwSize;
    DWORD dwVersion;
    DWORD dwFields;
    PCSTR pDeviceName;
    DWORD dwMediaType;
    DWORD dwDitheringMode;
    DWORD dwResolution[2];
    DWORD dwCMMType;
    DWORD dwClass;
    DWORD dwDataColorSpace;
    DWORD dwConnectionSpace;
    DWORD dwSignature;
    DWORD dwPlatform;
    DWORD dwProfileFlags;
    DWORD dwManufacturer;
    DWORD dwModel;
    DWORD dwAttributes[2];
    DWORD dwRenderingIntent;
    DWORD dwCreator;
    DWORD dwDeviceClass;
} ENUMTYPEA, *PENUMTYPEA, *LPENUMTYPEA;

typedef struct tagENUMTYPEW {
    DWORD dwSize;
    DWORD dwVersion;
    DWORD dwFields;
    PCWSTR pDeviceName;
    DWORD dwMediaType;
    DWORD dwDitheringMode;
    DWORD dwResolution[2];
    DWORD dwCMMType;
    DWORD dwClass;
    DWORD dwDataColorSpace;
    DWORD dwConnectionSpace;
    DWORD dwSignature;
    DWORD dwPlatform;
    DWORD dwProfileFlags;
    DWORD dwManufacturer;
    DWORD dwModel;
    DWORD dwAttributes[2];
    DWORD dwRenderingIntent;
    DWORD dwCreator;
    DWORD dwDeviceClass;
} ENUMTYPEW, *PENUMTYPEW, *LPENUMTYPEW;

struct _tagCOLORMATCHSETUPW;
struct _tagCOLORMATCHSETUPA;

typedef BOOL(WINAPI *PCMSCALLBACKW)(struct _tagCOLORMATCHSETUPW*,LPARAM);
typedef BOOL(WINAPI *PCMSCALLBACKA)(struct _tagCOLORMATCHSETUPA*,LPARAM);

typedef struct _tagCOLORMATCHSETUPW {
    DWORD dwSize;
    DWORD dwVersion;
    DWORD dwFlags;
    HWND hwndOwner;
    PCWSTR pSourceName;
    PCWSTR pDisplayName;
    PCWSTR pPrinterName;
    DWORD dwRenderIntent;
    DWORD dwProofingIntent;
    PWSTR pMonitorProfile;
    DWORD ccMonitorProfile;
    PWSTR pPrinterProfile;
    DWORD ccPrinterProfile;
    PWSTR pTargetProfile;
    DWORD ccTargetProfile;
    DLGPROC lpfnHook;
    LPARAM lParam;
    PCMSCALLBACKW lpfnApplyCallback;
    LPARAM lParamApplyCallback;
} COLORMATCHSETUPW, *PCOLORMATCHSETUPW, *LPCOLORMATCHSETUPW;

typedef struct _tagCOLORMATCHSETUPA {
    DWORD dwSize;
    DWORD dwVersion;
    DWORD dwFlags;
    HWND hwndOwner;
    PCSTR pSourceName;
    PCSTR pDisplayName;
    PCSTR pPrinterName;
    DWORD dwRenderIntent;
    DWORD dwProofingIntent;
    PSTR pMonitorProfile;
    DWORD ccMonitorProfile;
    PSTR pPrinterProfile;
    DWORD ccPrinterProfile;
    PSTR pTargetProfile;
    DWORD ccTargetProfile;
    DLGPROC lpfnHook;
    LPARAM lParam;
    PCMSCALLBACKA lpfnApplyCallback;
    LPARAM lParamApplyCallback;
} COLORMATCHSETUPA, *PCOLORMATCHSETUPA, *LPCOLORMATCHSETUPA;

typedef HANDLE HCMTRANSFORM;
typedef PVOID LPDEVCHARACTER;

HPROFILE WINAPI OpenColorProfileA(PPROFILE,DWORD,DWORD,DWORD);
HPROFILE WINAPI OpenColorProfileW(PPROFILE,DWORD,DWORD,DWORD);
BOOL WINAPI CloseColorProfile(HPROFILE);
BOOL WINAPI GetColorProfileFromHandle(HPROFILE,PBYTE,PDWORD);
BOOL WINAPI IsColorProfileValid(HPROFILE,PBOOL);
BOOL WINAPI CreateProfileFromLogColorSpaceA(LPLOGCOLORSPACEA,PBYTE *);
BOOL WINAPI CreateProfileFromLogColorSpaceW(LPLOGCOLORSPACEW,PBYTE *);
BOOL WINAPI GetCountColorProfileElements(HPROFILE,PDWORD);
BOOL WINAPI GetColorProfileHeader(HPROFILE,PPROFILEHEADER);
BOOL WINAPI GetColorProfileElementTag(HPROFILE,DWORD,PTAGTYPE);
BOOL WINAPI IsColorProfileTagPresent(HPROFILE,TAGTYPE,PBOOL);
BOOL WINAPI GetColorProfileElement(HPROFILE,TAGTYPE,DWORD,PDWORD,PVOID,PBOOL);
BOOL WINAPI SetColorProfileHeader(HPROFILE,PPROFILEHEADER);
BOOL WINAPI SetColorProfileElementSize(HPROFILE,TAGTYPE,DWORD);
BOOL WINAPI SetColorProfileElement(HPROFILE,TAGTYPE,DWORD,PDWORD,PVOID);
BOOL WINAPI SetColorProfileElementReference(HPROFILE,TAGTYPE,TAGTYPE);
BOOL WINAPI GetPS2ColorSpaceArray(HPROFILE,DWORD,DWORD,PBYTE,PDWORD,PBOOL);
BOOL WINAPI GetPS2ColorRenderingIntent(HPROFILE,DWORD,PBYTE,PDWORD);
BOOL WINAPI GetPS2ColorRenderingDictionary(HPROFILE,DWORD,PBYTE,PDWORD,PBOOL);
BOOL WINAPI GetNamedProfileInfo(HPROFILE,PNAMED_PROFILE_INFO);
BOOL WINAPI ConvertColorNameToIndex(HPROFILE,PCOLOR_NAME,PDWORD,DWORD);
BOOL WINAPI ConvertIndexToColorName(HPROFILE,PDWORD,PCOLOR_NAME,DWORD);
BOOL WINAPI CreateDeviceLinkProfile(PHPROFILE,DWORD,PDWORD,DWORD,DWORD,PBYTE *,DWORD);
HTRANSFORM WINAPI CreateColorTransformA(LPLOGCOLORSPACEA,HPROFILE,HPROFILE,DWORD);
HTRANSFORM WINAPI CreateColorTransformW(LPLOGCOLORSPACEW,HPROFILE,HPROFILE,DWORD);
HTRANSFORM WINAPI CreateMultiProfileTransform(PHPROFILE,DWORD,PDWORD,DWORD,DWORD,DWORD);
BOOL WINAPI DeleteColorTransform(HTRANSFORM);
BOOL WINAPI TranslateBitmapBits(HTRANSFORM,PVOID,BMFORMAT,DWORD,DWORD,DWORD,PVOID,BMFORMAT,DWORD,PBMCALLBACKFN,LPARAM);
BOOL WINAPI CheckBitmapBits(HTRANSFORM,PVOID,BMFORMAT,DWORD,DWORD,DWORD,PBYTE,PBMCALLBACKFN,LPARAM);
BOOL WINAPI TranslateColors(HTRANSFORM,PCOLOR,DWORD,COLORTYPE,PCOLOR,COLORTYPE);
BOOL WINAPI CheckColors(HTRANSFORM,PCOLOR,DWORD,COLORTYPE,PBYTE);
DWORD WINAPI GetCMMInfo(HTRANSFORM,DWORD);
BOOL WINAPI RegisterCMMA(PCSTR,DWORD,PCSTR);
BOOL WINAPI RegisterCMMW(PCWSTR,DWORD,PCWSTR);
BOOL WINAPI UnregisterCMMA(PCSTR,DWORD);
BOOL WINAPI UnregisterCMMW(PCWSTR,DWORD);
BOOL WINAPI SelectCMM(DWORD);
BOOL WINAPI GetColorDirectoryA(PCSTR,PSTR,PDWORD);
BOOL WINAPI GetColorDirectoryW(PCWSTR,PWSTR,PDWORD);
BOOL WINAPI InstallColorProfileA(PCSTR,PCSTR);
BOOL WINAPI InstallColorProfileW(PCWSTR,PCWSTR);
BOOL WINAPI UninstallColorProfileA(PCSTR,PCSTR,BOOL);
BOOL WINAPI UninstallColorProfileW(PCWSTR,PCWSTR,BOOL);
BOOL WINAPI EnumColorProfilesA(PCSTR,PENUMTYPEA,PBYTE,PDWORD,PDWORD);
BOOL WINAPI EnumColorProfilesW(PCWSTR,PENUMTYPEW,PBYTE,PDWORD,PDWORD);
BOOL WINAPI SetStandardColorSpaceProfileA(PCSTR,DWORD,PCSTR);
BOOL WINAPI SetStandardColorSpaceProfileW(PCWSTR,DWORD,PCWSTR);
BOOL WINAPI GetStandardColorSpaceProfileA(PCSTR,DWORD,PSTR,PDWORD);
BOOL WINAPI GetStandardColorSpaceProfileW(PCWSTR,DWORD,PWSTR,PDWORD);
BOOL WINAPI AssociateColorProfileWithDeviceA(PCSTR,PCSTR,PCSTR);
BOOL WINAPI AssociateColorProfileWithDeviceW(PCWSTR,PCWSTR,PCWSTR);
BOOL WINAPI DisassociateColorProfileFromDeviceA(PCSTR,PCSTR,PCSTR);
BOOL WINAPI DisassociateColorProfileFromDeviceW(PCWSTR,PCWSTR,PCWSTR);
BOOL WINAPI SetupColorMatchingW(PCOLORMATCHSETUPW pcms);
BOOL WINAPI SetupColorMatchingA(PCOLORMATCHSETUPA pcms);

BOOL WINAPI CMCheckColors(HCMTRANSFORM,LPCOLOR,DWORD,COLORTYPE,LPBYTE);
BOOL WINAPI CMCheckColorsInGamut(HCMTRANSFORM,RGBTRIPLE*,LPBYTE,UINT);
BOOL WINAPI CMCheckRGBs(HCMTRANSFORM,LPVOID,BMFORMAT,DWORD,DWORD,DWORD,LPBYTE,PBMCALLBACKFN,LPARAM);
BOOL WINAPI CMConvertColorNameToIndex(HPROFILE,PCOLOR_NAME,PDWORD,DWORD);
BOOL WINAPI CMConvertIndexToColorName(HPROFILE,PDWORD,PCOLOR_NAME,DWORD);
BOOL WINAPI CMCreateDeviceLinkProfile(PHPROFILE,DWORD,PDWORD,DWORD,DWORD,LPBYTE*);
HCMTRANSFORM WINAPI CMCreateMultiProfileTransform(PHPROFILE,DWORD,PDWORD,DWORD,DWORD);
BOOL WINAPI CMCreateProfile(LPLOGCOLORSPACEA,LPDEVCHARACTER*);
BOOL WINAPI CMCreateProfileW(LPLOGCOLORSPACEW,LPDEVCHARACTER*);
HCMTRANSFORM WINAPI CMCreateTransform(LPLOGCOLORSPACEA,LPDEVCHARACTER,LPDEVCHARACTER);
HCMTRANSFORM WINAPI CMCreateTransformW(LPLOGCOLORSPACEW,LPDEVCHARACTER,LPDEVCHARACTER);
HCMTRANSFORM WINAPI CMCreateTransformExt(LPLOGCOLORSPACEA,LPDEVCHARACTER,LPDEVCHARACTER,DWORD);
HCMTRANSFORM WINAPI CMCreateTransformExtW(LPLOGCOLORSPACEW,LPDEVCHARACTER,LPDEVCHARACTER,DWORD);
BOOL WINAPI CMDeleteTransform(HCMTRANSFORM);
DWORD WINAPI CMGetInfo(DWORD);
BOOL WINAPI CMGetNamedProfileInfo(HPROFILE,PNAMED_PROFILE_INFO);
BOOL WINAPI CMGetPS2ColorRenderingDictionary(HPROFILE,DWORD,LPBYTE,LPDWORD,LPBOOL);
BOOL WINAPI CMGetPS2ColorRenderingIntent(HPROFILE,DWORD,LPBYTE,LPDWORD);
BOOL WINAPI CMGetPS2ColorSpaceArray(HPROFILE,DWORD,DWORD,LPBYTE,LPDWORD,LPBOOL);
BOOL WINAPI CMIsProfileValid(HPROFILE,LPBOOL);
BOOL WINAPI CMTranslateColors(HCMTRANSFORM,LPCOLOR,DWORD,COLORTYPE,LPCOLOR,COLORTYPE);
BOOL WINAPI CMTranslateRGB(HCMTRANSFORM,COLORREF,LPCOLORREF,DWORD);
BOOL WINAPI CMTranslateRGBs(HCMTRANSFORM,LPVOID,BMFORMAT,DWORD,DWORD,DWORD,LPVOID,BMFORMAT,DWORD);
BOOL WINAPI CMTranslateRGBsExt(HCMTRANSFORM,LPVOID,BMFORMAT,DWORD,DWORD,DWORD,LPVOID,BMFORMAT,DWORD,LPBMCALLBACKFN,LPARAM);

#ifdef UNICODE
#define ENUMTYPE  ENUMTYPEW
#define PENUMTYPE  PENUMTYPEW
#define COLORMATCHSETUP  COLORMATCHSETUPW
#define PCOLORMATCHSETUP  PCOLORMATCHSETUPW
#define LPCOLORMATCHSETUP  LPCOLORMATCHSETUPW
#define PCMSCALLBACK  PCMSCALLBACKW
#define CreateColorTransform  CreateColorTransformW
#define OpenColorProfile  OpenColorProfileW
#define CreateProfileFromLogColorSpace  CreateProfileFromLogColorSpaceW
#define RegisterCMM  RegisterCMMW
#define UnregisterCMM  UnregisterCMMW
#define GetColorDirectory  GetColorDirectoryW
#define InstallColorProfile  InstallColorProfileW
#define UninstallColorProfile  UninstallColorProfileW
#define AssociateColorProfileWithDevice  AssociateColorProfileWithDeviceW
#define DisassociateColorProfileFromDevice  DisassociateColorProfileFromDeviceW
#define EnumColorProfiles  EnumColorProfilesW
#define SetStandardColorSpaceProfile  SetStandardColorSpaceProfileW
#define GetStandardColorSpaceProfile  GetStandardColorSpaceProfileW
#define SetupColorMatching  SetupColorMatchingW
#else
#define ENUMTYPE  ENUMTYPEA
#define PENUMTYPE  PENUMTYPEA
#define COLORMATCHSETUP  COLORMATCHSETUPA
#define PCOLORMATCHSETUP  PCOLORMATCHSETUPA
#define LPCOLORMATCHSETUP  LPCOLORMATCHSETUPA
#define PCMSCALLBACK  PCMSCALLBACKA
#define CreateColorTransform  CreateColorTransformA
#define OpenColorProfile  OpenColorProfileA
#define CreateProfileFromLogColorSpace  CreateProfileFromLogColorSpaceA
#define RegisterCMM  RegisterCMMA
#define UnregisterCMM  UnregisterCMMA
#define GetColorDirectory  GetColorDirectoryA
#define InstallColorProfile  InstallColorProfileA
#define UninstallColorProfile  UninstallColorProfileA
#define AssociateColorProfileWithDevice  AssociateColorProfileWithDeviceA
#define DisassociateColorProfileFromDevice  DisassociateColorProfileFromDeviceA
#define EnumColorProfiles  EnumColorProfilesA
#define SetStandardColorSpaceProfile  SetStandardColorSpaceProfileA
#define GetStandardColorSpaceProfile  GetStandardColorSpaceProfileA
#define SetupColorMatching  SetupColorMatchingA
#endif /* UNICODE */

#ifdef __cplusplus
}
#endif

#endif /* _ICM_H */
