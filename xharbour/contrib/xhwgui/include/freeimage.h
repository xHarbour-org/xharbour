// ==========================================================
// FreeImage 3
//
// Design and implementation by
// - Floris van den Berg (flvdberg@wxs.nl)
// - Hervé Drolon (drolon@infonie.fr)
//
// Contributors:
// - Adam Gates (radad@xoasis.com)
// - Alex Kwak
// - Alexander Dymerets (sashad@te.net.ua)
// - Detlev Vendt (detlev.vendt@brillit.de)
// - Jan L. Nauta (jln@magentammt.com)
// - Jani Kajala (janik@remedy.fi)
// - Juergen Riecker (j.riecker@gmx.de)
// - Karl-Heinz Bussian (khbussian@moss.de)
// - Laurent Rocher (rocherl@club-internet.fr)
// - Luca Piergentili (l.pierge@terra.es)
// - Machiel ten Brinke (brinkem@uni-one.nl)
// - Markus Loibl (markus.loibl@epost.de)
// - Martin Weber (martweb@gmx.net)
// - Matthias Wandel (mwandel@rim.net)
//
// This file is part of FreeImage 3
//
// COVERED CODE IS PROVIDED UNDER THIS LICENSE ON AN "AS IS" BASIS, WITHOUT WARRANTY
// OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTIES
// THAT THE COVERED CODE IS FREE OF DEFECTS, MERCHANTABLE, FIT FOR A PARTICULAR PURPOSE
// OR NON-INFRINGING. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE COVERED
// CODE IS WITH YOU. SHOULD ANY COVERED CODE PROVE DEFECTIVE IN ANY RESPECT, YOU (NOT
// THE INITIAL DEVELOPER OR ANY OTHER CONTRIBUTOR) ASSUME THE COST OF ANY NECESSARY
// SERVICING, REPAIR OR CORRECTION. THIS DISCLAIMER OF WARRANTY CONSTITUTES AN ESSENTIAL
// PART OF THIS LICENSE. NO USE OF ANY COVERED CODE IS AUTHORIZED HEREUNDER EXCEPT UNDER
// THIS DISCLAIMER.
//
// Use at your own risk!
// ==========================================================

#ifndef FREEIMAGE_H
#define FREEIMAGE_H

// Compiler options ---------------------------------------------------------

#if defined(FREEIMAGE_LIB) || !defined(WIN32)
#define DLL_API
#define DLL_CALLCONV
#else
#define WIN32_LEAN_AND_MEAN
#define DLL_CALLCONV __stdcall
// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the FREEIMAGE_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// DLL_API functions as being imported from a DLL, wheras this DLL sees symbols
// defined with this macro as being exported.
#ifdef FREEIMAGE_EXPORTS
#define DLL_API __declspec(dllexport)
#else
#define DLL_API __declspec(dllimport)
#endif // FREEIMAGE_EXPORTS
#endif // FREEIMAGE_LIB || !WIN32

// For C compatility --------------------------------------------------------

#ifdef __cplusplus
#define FI_DEFAULT(x)	= x
#define FI_ENUM(x)      enum x
#define FI_STRUCT(x)	struct x
#else
#define FI_DEFAULT(x)
#define FI_ENUM(x)      typedef int x; enum x
#define FI_STRUCT(x)	typedef struct x x; struct x
#endif

// Bitmap types -------------------------------------------------------------

FI_STRUCT (FIBITMAP) { void *data; };
FI_STRUCT (FIMULTIBITMAP) { void *data; };

// Types used in the library (directly copied from Windows) -----------------

#ifndef _WINDOWS_
#define _WINDOWS_

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif
#ifndef NULL
#define NULL 0
#endif

#ifndef SEEK_SET
#define SEEK_SET  0
#define SEEK_CUR  1
#define SEEK_END  2
#endif

#ifndef __GNUC__
typedef long BOOL;
typedef unsigned char BYTE;
typedef unsigned short WORD;
typedef unsigned long DWORD;
typedef long LONG;

typedef struct tagRGBQUAD {
  BYTE rgbBlue; 
  BYTE rgbGreen; 
  BYTE rgbRed; 
  BYTE rgbReserved; 
} RGBQUAD; 

typedef struct tagBITMAPINFOHEADER{
  DWORD biSize;
  LONG  biWidth; 
  LONG  biHeight; 
  WORD  biPlanes; 
  WORD  biBitCount;
  DWORD biCompression; 
  DWORD biSizeImage; 
  LONG  biXPelsPerMeter; 
  LONG  biYPelsPerMeter; 
  DWORD biClrUsed; 
  DWORD biClrImportant;
} BITMAPINFOHEADER, *PBITMAPINFOHEADER; 

typedef struct tagBITMAPINFO { 
  BITMAPINFOHEADER bmiHeader; 
  RGBQUAD          bmiColors[1];
} BITMAPINFO, *PBITMAPINFO;

#endif // __GNUC__
#endif // _WINDOWS_

// ICC profile support ------------------------------------------------------

#define FIICC_DEFAULT			0x00
#define FIICC_COLOR_IS_CMYK		0x01

FI_STRUCT (FIICCPROFILE) { 
	WORD    flags;	// info flag
	DWORD	size;	// profile's size measured in bytes
	void   *data;	// points to a block of contiguous memory containing the profile
};

// Important enums ----------------------------------------------------------

/** I/O image format identifiers.
*/
FI_ENUM(FREE_IMAGE_FORMAT) {
	FIF_UNKNOWN = -1,
	FIF_BMP = 0,
	FIF_ICO,
	FIF_JPEG,
	FIF_JNG,
	FIF_KOALA,
	FIF_LBM,
	FIF_MNG,
	FIF_PBM,
	FIF_PBMRAW,
	FIF_PCD,
	FIF_PCX,
	FIF_PGM,
	FIF_PGMRAW,
	FIF_PNG,
	FIF_PPM,
	FIF_PPMRAW,
	FIF_RAS,
	FIF_TARGA,
	FIF_TIFF,
	FIF_WBMP,
	FIF_PSD,
	FIF_CUT,
	FIF_IFF = FIF_LBM,
	FIF_XBM,
	FIF_XPM
};

/** Image color type used in FreeImage.
*/
FI_ENUM(FREE_IMAGE_COLOR_TYPE) {
	FIC_MINISWHITE = 0,		// min value is white
    FIC_MINISBLACK = 1,		// min value is black
    FIC_RGB        = 2,		// RGB color model
    FIC_PALETTE    = 3,		// color map indexed
	FIC_RGBALPHA   = 4,		// RGB color model with alpha channel
	FIC_CMYK       = 5		// CMYK color model
};

/** Color quantization algorithms.
Constants used in FreeImage_ColorQuantize.
*/
FI_ENUM(FREE_IMAGE_QUANTIZE) {
    FIQ_WUQUANT = 0,		// Xiaolin Wu color quantization algorithm
    FIQ_NNQUANT = 1			// NeuQuant neural-net quantization algorithm by Anthony Dekker
};

/** Dithering algorithms.
Constants used FreeImage_Dither.
*/
FI_ENUM(FREE_IMAGE_DITHER) {
    FID_FS			= 0,	// Floyd & Steinberg error diffusion
	FID_BAYER4x4	= 1,	// Bayer ordered dispersed dot dithering (order 2 dithering matrix)
	FID_BAYER8x8	= 2,	// Bayer ordered dispersed dot dithering (order 3 dithering matrix)
	FID_CLUSTER6x6	= 3,	// Ordered clustered dot dithering (order 3 - 6x6 matrix)
	FID_CLUSTER8x8	= 4,	// Ordered clustered dot dithering (order 4 - 8x8 matrix)
	FID_CLUSTER16x16= 5		// Ordered clustered dot dithering (order 8 - 16x16 matrix)
};

/** Upsampling / downsampling filters. 
Constants used in FreeImage_Rescale.
*/
FI_ENUM(FREE_IMAGE_FILTER) {
	FILTER_BOX		  = 0,	// Box, pulse, Fourier window, 1st order (constant) b-spline
	FILTER_BICUBIC	  = 1,	// Mitchell & Netravali's two-param cubic filter
	FILTER_BILINEAR   = 2,	// Bilinear filter
	FILTER_BSPLINE	  = 3,	// 4th order (cubic) b-spline
	FILTER_CATMULLROM = 4,	// Catmull-Rom spline, Overhauser spline
	FILTER_LANCZOS3	  = 5	// Lanczos3 filter
};

/** Color channels.
Constants used in color manipulation routines.
*/
FI_ENUM(FREE_IMAGE_COLOR_CHANNEL) {
	FICC_RGB	= 0,	// Use red, green and blue channels
	FICC_RED	= 1,	// Use red channel
	FICC_GREEN	= 2,	// Use green channel
	FICC_BLUE	= 3,	// Use blue channel
	FICC_ALPHA	= 4,	// Use alpha channel
	FICC_BLACK	= 5		// Use black channel
};

// File IO routines ---------------------------------------------------------

#ifndef FREEIMAGE_IO
#define FREEIMAGE_IO

typedef void* fi_handle;
typedef unsigned (DLL_CALLCONV *FI_ReadProc) (void *buffer, unsigned size, unsigned count, fi_handle handle);
typedef unsigned (DLL_CALLCONV *FI_WriteProc) (void *buffer, unsigned size, unsigned count, fi_handle handle);
typedef int (DLL_CALLCONV *FI_SeekProc) (fi_handle handle, long offset, int origin);
typedef long (DLL_CALLCONV *FI_TellProc) (fi_handle handle);

#ifdef WIN32
#pragma pack(push, 1)
#else
#pragma pack(1)
#endif // WIN32

FI_STRUCT(FreeImageIO) {
	FI_ReadProc  read_proc;     // pointer to the function used to read data
    FI_WriteProc write_proc;    // pointer to the function used to write data
    FI_SeekProc  seek_proc;     // pointer to the function used to seek
    FI_TellProc  tell_proc;     // pointer to the function used to aquire the current position
};

#ifdef WIN32
#pragma pack(pop)
#else
#pragma pack(4)
#endif // WIN32

#endif // FREEIMAGE_IO

// Plugin routines ----------------------------------------------------------
/*
#ifndef PLUGINS
#define PLUGINS

FI_STRUCT (Plugin);

typedef void (DLL_CALLCONV *FI_InitProc)(Plugin *plugin, int format_id);

typedef const char *(DLL_CALLCONV *FI_FormatProc) ();
typedef const char *(DLL_CALLCONV *FI_DescriptionProc) ();
typedef const char *(DLL_CALLCONV *FI_ExtensionListProc) ();
typedef const char *(DLL_CALLCONV *FI_RegExprProc) ();
typedef void *(DLL_CALLCONV *FI_OpenProc)(FreeImageIO *io, fi_handle handle, BOOL read);
typedef void (DLL_CALLCONV *FI_CloseProc)(FreeImageIO *io, fi_handle handle, void *data);
typedef int (DLL_CALLCONV *FI_PageCountProc)(FreeImageIO *io, fi_handle handle, void *data);
typedef int (DLL_CALLCONV *FI_PageCapabilityProc)(FreeImageIO *io, fi_handle handle, void *data);
typedef FIBITMAP *(DLL_CALLCONV *FI_LoadProc)(FreeImageIO *io, fi_handle handle, int page, int flags, void *data);
typedef BOOL (DLL_CALLCONV *FI_SaveProc)(FreeImageIO *io, FIBITMAP *dib, fi_handle handle, int page, int flags, void *data);
typedef BOOL (DLL_CALLCONV *FI_ValidateProc)(FreeImageIO *io, fi_handle handle);
typedef const char *(DLL_CALLCONV *FI_MimeProc) ();
typedef BOOL (DLL_CALLCONV *FI_SupportsExportBPPProc)(int bpp);
typedef BOOL (DLL_CALLCONV *FI_SupportsICCProfilesProc)();

FI_STRUCT (Plugin) {
	FI_FormatProc format_proc;
	FI_DescriptionProc description_proc;
	FI_ExtensionListProc extension_proc;
	FI_RegExprProc regexpr_proc;
	FI_OpenProc open_proc;
	FI_CloseProc close_proc;
	FI_PageCountProc pagecount_proc;
	FI_PageCapabilityProc pagecapability_proc;
	FI_LoadProc load_proc;
	FI_SaveProc save_proc;
	FI_ValidateProc validate_proc;
	FI_MimeProc mime_proc;
	FI_SupportsExportBPPProc supports_export_bpp_proc;
	FI_SupportsICCProfilesProc supports_icc_profiles_proc;
};

#endif // PLUGINS
*/

// Load / Save flag constants -----------------------------------------------

#define BMP_DEFAULT         0
#define BMP_SAVE_RLE        1
#define CUT_DEFAULT         0
#define ICO_DEFAULT         0
#define IFF_DEFAULT         0
#define JPEG_DEFAULT        0
#define JPEG_FAST           1
#define JPEG_ACCURATE       2
#define JPEG_QUALITYSUPERB  0x80
#define JPEG_QUALITYGOOD    0x100
#define JPEG_QUALITYNORMAL  0x200
#define JPEG_QUALITYAVERAGE 0x400
#define JPEG_QUALITYBAD     0x800
#define KOALA_DEFAULT       0
#define LBM_DEFAULT         0
#define MNG_DEFAULT         0
#define PCD_DEFAULT         0
#define PCD_BASE            1		// load the bitmap sized 768 x 512
#define PCD_BASEDIV4        2		// load the bitmap sized 384 x 256
#define PCD_BASEDIV16       3		// load the bitmap sized 192 x 128
#define PCX_DEFAULT         0
#define PNG_DEFAULT         0
#define PNG_IGNOREGAMMA		1		// avoid gamma correction
#define PNM_DEFAULT         0
#define PNM_SAVE_RAW        0       // If set the writer saves in RAW format (i.e. P4, P5 or P6)
#define PNM_SAVE_ASCII      1       // If set the writer saves in ASCII format (i.e. P1, P2 or P3)
#define PSD_DEFAULT         0
#define RAS_DEFAULT         0
#define TARGA_DEFAULT       0
#define TARGA_LOAD_RGB888   1       // If set the loader converts RGB555 and ARGB8888 -> RGB888.
#define TIFF_DEFAULT        0
#define TIFF_CMYK			0x0001	// reads/stores tags for separated CMYK (use | to combine with compression flags)
#define TIFF_PACKBITS       0x0100  // save using PACKBITS compression
#define TIFF_DEFLATE        0x0200  // save using DEFLATE compression
#define TIFF_ADOBE_DEFLATE  0x0400  // save using ADOBE DEFLATE compression
#define TIFF_NONE           0x0800  // save without any compression
#define WBMP_DEFAULT        0
#define XBM_DEFAULT			0
#define XPM_DEFAULT			0


#ifdef __cplusplus
extern "C" {
#endif

// Init / Error routines ----------------------------------------------------

DLL_API void DLL_CALLCONV FreeImage_Initialise(BOOL load_local_plugins_only FI_DEFAULT(FALSE));
DLL_API void DLL_CALLCONV FreeImage_DeInitialise();

// Version routines ---------------------------------------------------------

DLL_API const char *DLL_CALLCONV FreeImage_GetVersion();
DLL_API const char *DLL_CALLCONV FreeImage_GetCopyrightMessage();

// Message output functions -------------------------------------------------

DLL_API void DLL_CALLCONV FreeImage_OutputMessageProc(int fif, const char *fmt, ...);

typedef void (*FreeImage_OutputMessageFunction)(FREE_IMAGE_FORMAT fif, const char *msg);
DLL_API void DLL_CALLCONV FreeImage_SetOutputMessage(FreeImage_OutputMessageFunction omf);

// Allocate / Clone / Unload routines ---------------------------------------

DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Allocate(int width, int height, int bpp, unsigned red_mask FI_DEFAULT(0), unsigned green_mask FI_DEFAULT(0), unsigned blue_mask FI_DEFAULT(0));
DLL_API FIBITMAP * DLL_CALLCONV FreeImage_Clone(FIBITMAP *dib);
DLL_API void DLL_CALLCONV FreeImage_Unload(FIBITMAP *dib);

// Load / Save routines -----------------------------------------------------

DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Load(FREE_IMAGE_FORMAT fif, const char *filename, int flags FI_DEFAULT(0));
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadFromHandle(FREE_IMAGE_FORMAT fif, FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(0));
DLL_API BOOL DLL_CALLCONV FreeImage_Save(FREE_IMAGE_FORMAT fif, FIBITMAP *dib, const char *filename, int flags FI_DEFAULT(0));
DLL_API BOOL DLL_CALLCONV FreeImage_SaveToHandle(FREE_IMAGE_FORMAT fif, FIBITMAP *dib, FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(0));

// Plugin Interface ---------------------------------------------------------
/*
DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_RegisterLocalPlugin(FI_InitProc proc_address, const char *format FI_DEFAULT(0), const char *description FI_DEFAULT(0), const char *extension FI_DEFAULT(0), const char *regexpr FI_DEFAULT(0));
DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_RegisterExternalPlugin(const char *path, const char *format FI_DEFAULT(0), const char *description FI_DEFAULT(0), const char *extension FI_DEFAULT(0), const char *regexpr FI_DEFAULT(0));
DLL_API int DLL_CALLCONV FreeImage_GetFIFCount();
DLL_API int DLL_CALLCONV FreeImage_SetPluginEnabled(FREE_IMAGE_FORMAT fif, BOOL enable);
DLL_API int DLL_CALLCONV FreeImage_IsPluginEnabled(FREE_IMAGE_FORMAT fif);
DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFIFFromFormat(const char *format);
DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFIFFromMime(const char *mime);
*/
DLL_API const char *DLL_CALLCONV FreeImage_GetFormatFromFIF(FREE_IMAGE_FORMAT fif);
/*
DLL_API const char *DLL_CALLCONV FreeImage_GetFIFExtensionList(FREE_IMAGE_FORMAT fif);
DLL_API const char *DLL_CALLCONV FreeImage_GetFIFDescription(FREE_IMAGE_FORMAT fif);
DLL_API const char * DLL_CALLCONV FreeImage_GetFIFRegExpr(FREE_IMAGE_FORMAT fif);
DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFIFFromFilename(const char *filename);
DLL_API BOOL DLL_CALLCONV FreeImage_FIFSupportsReading(FREE_IMAGE_FORMAT fif);
DLL_API BOOL DLL_CALLCONV FreeImage_FIFSupportsWriting(FREE_IMAGE_FORMAT fif);
DLL_API BOOL DLL_CALLCONV FreeImage_FIFSupportsExportBPP(FREE_IMAGE_FORMAT fif, int bpp);
DLL_API BOOL DLL_CALLCONV FreeImage_FIFSupportsICCProfiles(FREE_IMAGE_FORMAT fif);
*/
// Multipaging interface ----------------------------------------------------

DLL_API FIMULTIBITMAP * DLL_CALLCONV FreeImage_OpenMultiBitmap(FREE_IMAGE_FORMAT fif, const char *filename, BOOL create_new, BOOL read_only, BOOL keep_cache_in_memory FI_DEFAULT(FALSE));
DLL_API BOOL DLL_CALLCONV FreeImage_CloseMultiBitmap(FIMULTIBITMAP *bitmap, int flags FI_DEFAULT(0));
DLL_API int DLL_CALLCONV FreeImage_GetPageCount(FIMULTIBITMAP *bitmap);
DLL_API void DLL_CALLCONV FreeImage_AppendPage(FIMULTIBITMAP *bitmap, FIBITMAP *data);
DLL_API void DLL_CALLCONV FreeImage_InsertPage(FIMULTIBITMAP *bitmap, int page, FIBITMAP *data);
DLL_API void DLL_CALLCONV FreeImage_DeletePage(FIMULTIBITMAP *bitmap, int page);
DLL_API FIBITMAP * DLL_CALLCONV FreeImage_LockPage(FIMULTIBITMAP *bitmap, int page);
DLL_API void DLL_CALLCONV FreeImage_UnlockPage(FIMULTIBITMAP *bitmap, FIBITMAP *page, BOOL changed);
DLL_API BOOL DLL_CALLCONV FreeImage_MovePage(FIMULTIBITMAP *bitmap, int target, int source);
DLL_API BOOL DLL_CALLCONV FreeImage_GetLockedPageNumbers(FIMULTIBITMAP *bitmap, int *pages, int *count);

// Filetype request routines ------------------------------------------------

DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFileType(const char *filename, int size FI_DEFAULT(0));
DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFileTypeFromHandle(FreeImageIO *io, fi_handle handle, int size FI_DEFAULT(0));

// FreeImage info routines --------------------------------------------------

DLL_API unsigned DLL_CALLCONV FreeImage_GetRedMask(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetGreenMask(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetBlueMask(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetTransparencyCount(FIBITMAP *dib);
DLL_API BYTE * DLL_CALLCONV FreeImage_GetTransparencyTable(FIBITMAP *dib);
DLL_API void DLL_CALLCONV FreeImage_SetTransparent(FIBITMAP *dib, BOOL enabled);
DLL_API void DLL_CALLCONV FreeImage_SetTransparencyTable(FIBITMAP *dib, BYTE *table, int count);
DLL_API BOOL DLL_CALLCONV FreeImage_IsTransparent(FIBITMAP *dib);

// DIB info routines --------------------------------------------------------

DLL_API unsigned DLL_CALLCONV FreeImage_GetColorsUsed(FIBITMAP *dib);
DLL_API BYTE *DLL_CALLCONV FreeImage_GetBits(FIBITMAP *dib);
DLL_API BYTE *DLL_CALLCONV FreeImage_GetScanLine(FIBITMAP *dib, int scanline);
DLL_API unsigned DLL_CALLCONV FreeImage_GetBPP(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetWidth(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetHeight(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetLine(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetPitch(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetDIBSize(FIBITMAP *dib);
DLL_API RGBQUAD *DLL_CALLCONV FreeImage_GetPalette(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetDotsPerMeterX(FIBITMAP *dib);
DLL_API unsigned DLL_CALLCONV FreeImage_GetDotsPerMeterY(FIBITMAP *dib);
DLL_API BITMAPINFOHEADER *DLL_CALLCONV FreeImage_GetInfoHeader(FIBITMAP *dib);
DLL_API BITMAPINFO *DLL_CALLCONV FreeImage_GetInfo(FIBITMAP *dib);
DLL_API FREE_IMAGE_COLOR_TYPE DLL_CALLCONV FreeImage_GetColorType(FIBITMAP *dib);

// ICC profile routines -----------------------------------------------------

DLL_API FIICCPROFILE *DLL_CALLCONV FreeImage_GetICCProfile(FIBITMAP *dib);
DLL_API FIICCPROFILE *DLL_CALLCONV FreeImage_CreateICCProfile(FIBITMAP *dib, void *data, long size);
DLL_API void DLL_CALLCONV FreeImage_DestroyICCProfile(FIBITMAP *dib);

// Line conversion routines -------------------------------------------------

DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To8(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To8(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To8_555(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To8_565(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To8(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To8(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To16_555(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To16_555(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To16_555(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine16_565_To16_555(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To16_555(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To16_555(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To16_565(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To16_565(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To16_565(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine16_555_To16_565(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To16_565(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To16_565(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To24(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To24(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To24(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To24_555(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To24_565(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To24(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To32(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To32(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To32(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To32_555(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To32_565(BYTE *target, BYTE *source, int width_in_pixels);
DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To32(BYTE *target, BYTE *source, int width_in_pixels);

// Smart conversion routines ------------------------------------------------

DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertTo8Bits(FIBITMAP *dib);
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertTo16Bits555(FIBITMAP *dib);
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertTo16Bits565(FIBITMAP *dib);
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertTo24Bits(FIBITMAP *dib);
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertTo32Bits(FIBITMAP *dib);
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ColorQuantize(FIBITMAP *dib, FREE_IMAGE_QUANTIZE quantize);
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Threshold(FIBITMAP *dib, BYTE T);
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Dither(FIBITMAP *dib, FREE_IMAGE_DITHER algorithm);

DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertFromRawBits(BYTE *bits, int width, int height, int pitch, unsigned bpp, unsigned red_mask, unsigned green_mask, unsigned blue_mask, BOOL topdown FI_DEFAULT(FALSE));
DLL_API void DLL_CALLCONV FreeImage_ConvertToRawBits(BYTE *bits, FIBITMAP *dib, int pitch, unsigned bpp, unsigned red_mask, unsigned green_mask, unsigned blue_mask, BOOL topdown FI_DEFAULT(FALSE));

// ZLib interface -----------------------------------------------------------

DLL_API DWORD DLL_CALLCONV FreeImage_ZLibCompress(BYTE *target, DWORD target_size, BYTE *source, DWORD source_size);
DLL_API DWORD DLL_CALLCONV FreeImage_ZLibUncompress(BYTE *target, DWORD target_size, BYTE *source, DWORD source_size);

// --------------------------------------------------------------------------
// Image manipulation toolkit -----------------------------------------------
// --------------------------------------------------------------------------

// rotation and flipping
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_RotateClassic(FIBITMAP *dib, double angle);
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_RotateEx(FIBITMAP *dib, double angle, double x_shift, double y_shift, double x_origin, double y_origin, BOOL use_mask);
DLL_API BOOL DLL_CALLCONV FreeImage_FlipHorizontal(FIBITMAP *dib);
DLL_API BOOL DLL_CALLCONV FreeImage_FlipVertical(FIBITMAP *dib);

// upsampling / downsampling
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Rescale(FIBITMAP *dib, int dst_width, int dst_height, FREE_IMAGE_FILTER filter);

// color manipulation routines (point operations)
DLL_API BOOL DLL_CALLCONV FreeImage_AdjustCurve(FIBITMAP *dib, BYTE *LUT, FREE_IMAGE_COLOR_CHANNEL channel);
DLL_API BOOL DLL_CALLCONV FreeImage_AdjustGamma(FIBITMAP *dib, double gamma);
DLL_API BOOL DLL_CALLCONV FreeImage_AdjustBrightness(FIBITMAP *dib, double percentage);
DLL_API BOOL DLL_CALLCONV FreeImage_AdjustContrast(FIBITMAP *dib, double percentage);
DLL_API BOOL DLL_CALLCONV FreeImage_Invert(FIBITMAP *dib);
DLL_API BOOL DLL_CALLCONV FreeImage_GetHistogram(FIBITMAP *dib, DWORD *histo, FREE_IMAGE_COLOR_CHANNEL channel FI_DEFAULT(FICC_BLACK));

// channel processing routines
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_GetChannel(FIBITMAP *dib, FREE_IMAGE_COLOR_CHANNEL channel);
DLL_API BOOL DLL_CALLCONV FreeImage_SetChannel(FIBITMAP *dib, FIBITMAP *dib8, FREE_IMAGE_COLOR_CHANNEL channel);

// copy / paste routines
DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Copy(FIBITMAP *dib, int left, int top, int right, int bottom);
DLL_API BOOL DLL_CALLCONV FreeImage_Paste(FIBITMAP *dst, FIBITMAP *src, int left, int top, int alpha);


#ifdef __cplusplus
}
#endif

#endif // FREEIMAGE_H
