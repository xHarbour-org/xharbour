/*
 * $Id: hbapigt.h,v 1.31 2004/04/02 22:01:57 andijahja Exp $
 */

/*
 * Harbour Project source code:
 * Header file for the Terminal API
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    Keyboard related declarations
 *    Cursor declarations
 * See above for licensing terms.
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    Mouse related declarations
 *    Undocumented GT API declarations
 * See doc/license.txt for licensing terms.
 *
 */

#ifndef HB_APIGT_H_
#define HB_APIGT_H_

#include "hbapi.h"
#include "gtinfo.ch"

HB_EXTERN_BEGIN

/* NOTE: The declaration of hb_gtSetPos(), hb_gtGetPos(), hb_gtWrite(),
         hb_gtWriteAt(), hb_gtRepChar(), hb_gtBox(), hb_gtBoxS(), hb_gtBoxD()
         hb_gtInit() differs in parameter types from the original CA-Cl*pper
         versions. [vszakats] */

/* maximum length of color string */
#define CLR_STRLEN              64

/* attributes for color strings, these are the same as the ones in color.ch
   but prefixed with HB_ to avoid collision. */
#define HB_CLR_STANDARD         0
#define HB_CLR_ENHANCED         1
#define HB_CLR_BORDER           2
#define HB_CLR_BACKGROUND       3
#define HB_CLR_UNSELECTED       4
#define HB_CLR_MAX_             HB_CLR_UNSELECTED

/* strings for borders (same as box.ch, but defined for use by C) */

/* Note. This part will never be used, but is being kept in the source,
         so that if you use code page 437, you can see what the line
         draw characters are supposed to look like.
                                01234567
#define _B_SINGLE              "ÚÄ¿³ÙÄÀ³"
#define _B_DOUBLE              "ÉÍ»º¼ÍÈº"
#define _B_SINGLE_DOUBLE       "ÖÄ·º½ÄÓº"
#define _B_DOUBLE_SINGLE       "ÕÍ¸³¾ÍÔ³"
#define HB_B_SINGLE_V          '³'
#define HB_B_SINGLE_H          'Ä'
#define HB_B_DOUBLE_V          'º'
#define HB_B_DOUBLE_H          'Í'
*/
#define _B_SINGLE              "\xDA\xC4\xBF\xB3\xD9\xC4\xC0\xB3"
#define _B_DOUBLE              "\xC9\xCD\xBB\xBA\xBC\xCD\xC8\xBA"
#define _B_SINGLE_DOUBLE       "\xD6\xC4\xB7\xBA\xBD\xC4\xD3\xBA"
#define _B_DOUBLE_SINGLE       "\xD5\xCD\xB8\xB3\xBE\xCD\xD4\xB3"
#define HB_B_SINGLE_V          '\xB3'
#define HB_B_SINGLE_H          '\xC4'
#define HB_B_DOUBLE_V          '\xBA'
#define HB_B_DOUBLE_H          '\xCD'

/* Used to tell hb_gt_SetPos() when the cursor position
   is being set. Before or after text is or was displayed.
*/
#define HB_GT_SET_POS_AFTER     1
#define HB_GT_SET_POS_BEFORE    0

/* Keyboard filters */

typedef enum
{
   INKEY_MOVE           = 1,    /* Mouse Events */
   INKEY_LDOWN          = 2,    /* Mouse Left Click Down */
   INKEY_LUP            = 4,    /* Mouse Left Click Up */
   INKEY_RDOWN          = 8,    /* Mouse Right Click Down */
   INKEY_RUP            = 16,   /* Mouse Right Click Up */
   INKEY_KEYBOARD       = 128,  /* Keyboard Events */
   INKEY_ALL            = 159,  /* All Mouse and Keyboard Events */
   INKEY_RAW            = 256   /* Minimally Decoded Keyboard Events */
} HB_inkey_enum;

/* KEYBOARD struct */

typedef struct _HB_inkeyKB
{
   int    Pos;
   BYTE * String;
   struct _HB_inkeyKB * pNext;
} HB_inkeyKB, * PHB_inkeyKB;


/* Cursor style constants */

typedef enum
{
   SC_NONE              = 0,    /* None */
   SC_NORMAL            = 1,    /* Underline */
   SC_INSERT            = 2,    /* Lower half block */
   SC_SPECIAL1          = 3,    /* Full block */
   SC_SPECIAL2          = 4     /* Upper half block */
} HB_cursor_enum;


typedef struct _tag_HB_GT_GCOLOR
{
   USHORT usAlpha;
   USHORT usRed;
   USHORT usGreen;
   USHORT usBlue;
} HB_GT_GCOLOR;

typedef struct _tag_HB_GT_COLDEF
{
   char *name;
   HB_GT_GCOLOR color;
} HB_GT_COLDEF;

/* GT Graphical object system */
typedef struct _tag_gt_gobject
{
   int type;
   struct _tag_gt_gobject *next;
   struct _tag_gt_gobject *prev;
   HB_GT_GCOLOR color;
   int x;
   int y;
   /* Note: this data should be added on an "eredity" cast, but as many compilers
   we support handle this differently, we just add here all the needed data.
   Objects may ignore or use this fields for other things, i.e. strings will store
   options in "width".
   */
   int width;
   int height;
   char *data;
   USHORT data_len;
} HB_GT_GOBJECT;

typedef enum
{
   GTO_POINT      = 0,
   GTO_LINE       = 1,
   GTO_SQUARE     = 3,
   GTO_RECTANGLE  = 4,
   GTO_CIRCLE     = 5,
   GTO_DISK       = 7,
   /* TODO: add other types */
   GTO_TEXT       = 100,
} HB_gt_object_enum;

/* Event subsystem */

typedef enum
{
   GTEVENT_RESIZE   = 0,
   GTEVENT_CLOSE    = 1,
   GTEVENT_ICONIZE  = 2,
   GTEVENT_MAXH     = 3,
   GTEVENT_MAXV     = 4,
   GTEVENT_MAXIMIZE = 5,
   GTEVENT_DEICONIZE= 6,
   GTEVENT_SHUTDOWN = 7
} HB_gt_event_enum;


/* This pointers holds the list of items that the GT module should draw */
extern HB_GT_GOBJECT *hb_gt_gobjects;
extern HB_GT_GOBJECT *hb_gt_gobjects_end;

/* This is the list of colors */
#define HB_GT_COLDEF_COUNT 16
extern HB_GT_COLDEF hb_gt_gcoldefs[ HB_GT_COLDEF_COUNT ];

/* Public interface. These should never change, only be added to. */

extern void   hb_gtInit( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr );
extern void   hb_gtExit( void );
extern void   HB_EXPORT hb_gtAdjustPos( int iHandle, char * pStr, ULONG ulLen );
extern USHORT HB_EXPORT hb_gtBox( SHORT uiTop, SHORT uiLeft, SHORT uiBottom, SHORT uiRight, BYTE * pbyFrame );
extern USHORT HB_EXPORT hb_gtBoxD( SHORT uiTop, SHORT uiLeft, SHORT uiBottom, SHORT uiRight );
extern USHORT HB_EXPORT hb_gtBoxS( SHORT uiTop, SHORT uiLeft, SHORT uiBottom, SHORT uiRight );
extern USHORT HB_EXPORT hb_gtColorSelect( USHORT uiColorIndex );
extern USHORT HB_EXPORT hb_gtColorToN( char * szColorString );
extern USHORT HB_EXPORT hb_gtDispBegin( void );
extern USHORT HB_EXPORT hb_gtDispCount( void );
extern USHORT HB_EXPORT hb_gtDispEnd( void );
extern USHORT HB_EXPORT hb_gtDrawShadow( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr );
extern USHORT HB_EXPORT hb_gtGetBlink( BOOL * pbBlink );
extern USHORT HB_EXPORT hb_gtGetColorStr( char * pszColorString );
extern USHORT HB_EXPORT hb_gtGetCursor( USHORT * puiCursorShape );
extern USHORT HB_EXPORT hb_gtGetPos( SHORT * piRow, SHORT * piCol );
extern BOOL   HB_EXPORT hb_gtIsColor( void );
extern USHORT HB_EXPORT hb_gtMaxCol( void );
extern USHORT HB_EXPORT hb_gtMaxRow( void );
extern USHORT HB_EXPORT hb_gtPostExt( void );
extern USHORT HB_EXPORT hb_gtPreExt( void );
extern USHORT HB_EXPORT hb_gtSuspend( void ); /* prepare the reminal for shell output */
extern USHORT HB_EXPORT hb_gtResume( void ); /* resume the terminal after the shell output */
extern int    HB_EXPORT hb_gtExtendedKeySupport( void );
extern int    HB_EXPORT hb_gtReadKey( HB_inkey_enum eventmask );
extern USHORT HB_EXPORT hb_gtRectSize( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, UINT * puiBuffSize );
extern USHORT HB_EXPORT hb_gtRepChar( USHORT uiRow, USHORT uiCol, BYTE byChar, USHORT uiCount );
extern USHORT HB_EXPORT hb_gtRest( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, void * pScrBuff );
extern USHORT HB_EXPORT hb_gtSave( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, void * pScrBuff );
extern USHORT HB_EXPORT hb_gtScrDim( USHORT * puiHeight, USHORT * puiWidth );
extern USHORT HB_EXPORT hb_gtScroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, SHORT iRows, SHORT iCols );
extern USHORT HB_EXPORT hb_gtSetBlink( BOOL bBlink );
extern USHORT HB_EXPORT hb_gtSetColorStr( char * pszColorString );
extern USHORT HB_EXPORT hb_gtSetCursor( USHORT uiCursorShape );
extern USHORT HB_EXPORT hb_gtSetMode( USHORT uiRows, USHORT uiCols );
extern USHORT HB_EXPORT hb_gtSetPos( SHORT iRow, SHORT iCol );
extern USHORT HB_EXPORT hb_gtSetPosContext( SHORT iRow, SHORT iCol, SHORT iMode );
extern USHORT HB_EXPORT hb_gtSetSnowFlag( BOOL bNoSnow );
extern void   HB_EXPORT hb_gtTone( double dFrequency, double dDuration );
extern USHORT HB_EXPORT hb_gtWrite( BYTE * pbyStr, ULONG ulLen );
extern USHORT HB_EXPORT hb_gtWriteAt( USHORT uiRow, USHORT uiCol, BYTE * pbyStr, ULONG ulLen );
extern USHORT HB_EXPORT hb_gtWriteCon( BYTE * pbyStr, ULONG ulLen );
extern int    HB_EXPORT hb_gtCurrentColor( void );
extern int    HB_EXPORT hb_gtIndexedColor( int idx );
extern char   HB_EXPORT * hb_gtVersion( void );
#define hb_gtOutStd( pbyStr, ulLen ) hb_gt_OutStd( pbyStr, ulLen )
#define hb_gtOutErr( pbyStr, ulLen ) hb_gt_OutErr( pbyStr, ulLen )

/* GT Directed close request handler */
extern void HB_EXPORT hb_gtHandleClose(void);
extern BOOL HB_EXPORT hb_gtHandleShutdown(void);
extern BOOL HB_EXPORT hb_gtSetCloseHandler(PHB_ITEM handler);
extern PHB_ITEM HB_EXPORT hb_gtGetCloseHandler(void);

extern int  HB_EXPORT   hb_gtGetCloseEvent( void );
extern int  HB_EXPORT   hb_gtGetShutdownEvent( void );
extern void HB_EXPORT   hb_gtSetCloseEvent( int iEvent );
extern void HB_EXPORT   hb_gtSetShutdownEvent( int iEvent );

extern void HB_EXPORT hb_gtSetResizeEvent( int iEvent );
extern int  HB_EXPORT hb_gtGetResizeEvent( void );
extern void HB_EXPORT hb_gtHandleResize( void );

/* Undocumented CA-Clipper 5.x GT API calls */

#define HB_GT_RECT void
#define HB_GT_WND void
#define HB_GT_RGB void
#define HB_GT_SLR void

extern void   hb_gtWCreate( HB_GT_RECT * rect, HB_GT_WND ** wnd );
extern void   hb_gtWDestroy( HB_GT_WND * wnd );
extern BOOL   hb_gtWFlash( void );
extern void   hb_gtWApp( HB_GT_WND ** wnd );
extern void   hb_gtWCurrent( HB_GT_WND * wnd );
extern void   hb_gtWPos( HB_GT_WND * wnd, HB_GT_RECT * rect );
extern BOOL   hb_gtWVis( HB_GT_WND * wnd, USHORT uiStatus );

extern USHORT HB_EXPORT hb_gtSLR( HB_GT_SLR * pSLR ); /* System Level Request */
extern USHORT HB_EXPORT hb_gtModalRead( void * );
extern USHORT HB_EXPORT hb_gtBeginWrite( void );
extern USHORT HB_EXPORT hb_gtEndWrite( void );
extern USHORT HB_EXPORT hb_gtFlushCursor( void );
extern USHORT HB_EXPORT hb_gtSetColor( HB_GT_RGB * color );
extern USHORT HB_EXPORT hb_gtGetColor( HB_GT_RGB * color );
extern USHORT HB_EXPORT hb_gtSetBorder( HB_GT_RGB * color );

/* Private interface listed below. these are common to all platforms */

extern void   hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr );
extern void   hb_gt_Exit( void );
extern BOOL   hb_gt_AdjustPos( BYTE * pStr, ULONG ulLen );
extern USHORT hb_gt_Box( SHORT uiTop, SHORT uiLeft, SHORT uiBottom, SHORT uiRight, BYTE * pbyFrame, BYTE byAttr );
extern USHORT hb_gt_BoxD( SHORT uiTop, SHORT uiLeft, SHORT uiBottom, SHORT uiRight, BYTE * pbyFrame, BYTE byAttr );
extern USHORT hb_gt_BoxS( SHORT uiTop, SHORT uiLeft, SHORT uiBottom, SHORT uiRight, BYTE * pbyFrame, BYTE byAttr );
extern SHORT  hb_gt_Col( void );
extern void   hb_gt_DispBegin( void );
extern USHORT hb_gt_DispCount( void );
extern void   hb_gt_DispEnd( void );
extern BOOL   hb_gt_GetBlink( void );
extern USHORT hb_gt_GetCursorStyle( void );
extern USHORT hb_gt_GetScreenHeight( void );
extern USHORT hb_gt_GetScreenWidth( void );
extern void   hb_gt_GetText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyDst );
extern USHORT hb_gt_HorizLine( SHORT uiRow, SHORT uiLeft, SHORT uiRight, BYTE byChar, BYTE byAttr );
extern BOOL   hb_gt_IsColor( void );
extern BOOL   hb_gt_PreExt( void );
extern BOOL   hb_gt_PostExt( void );
extern BOOL   hb_gt_Suspend( void ); /* suspend the terminal before the shell call */
extern BOOL   hb_gt_Resume( void ); /* resume the terminal after the shell call */
extern void   hb_gt_Puts( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE * pbyStr, ULONG ulLen );
extern void   hb_gt_PutText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbySrc );
extern int    hb_gt_ExtendedKeySupport( void );
extern int    hb_gt_ReadKey( HB_inkey_enum eventmask );
extern int    hb_gt_RectSize( USHORT rows, USHORT cols );
extern void   hb_gt_Replicate( USHORT uiTop, USHORT uiLeft, BYTE byAttr, BYTE byChar, ULONG ulLen );
extern SHORT  hb_gt_Row( void );
extern void   hb_gt_Scroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr, SHORT iRows, SHORT iCols );
extern void   hb_gt_SetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr );
extern void   hb_gt_SetBlink( BOOL bBlink );
extern void   hb_gt_SetCursorStyle( USHORT uiCursorShape );
extern BOOL   hb_gt_SetMode( USHORT uiRows, USHORT uiCols );
extern void   hb_gt_SetPos( SHORT iRow, SHORT iCol, SHORT iMethod );
extern void   hb_gt_Tone( double dFrequency, double dDuration );
extern char * hb_gt_Version( void );
extern USHORT hb_gt_VertLine( SHORT uiCol, SHORT uiTop, SHORT uiBottom, BYTE byChar, BYTE byAttr );

extern void   hb_gt_OutStd( BYTE * pbyStr, ULONG ulLen );
extern void   hb_gt_OutErr( BYTE * pbyStr, ULONG ulLen );

/* Private interface for extended GT functions listed below */
extern void   hb_gt_SetDispCP( char * pszTermCDP, char * pszHostCDP, BOOL bBox );
extern void   hb_gt_SetKeyCP( char * pszTermCDP, char * pszHostCDP );

/* Clipboard support */
extern void HB_EXPORT hb_gt_GetClipboard( char *szData, ULONG *pulMaxSize );
extern void HB_EXPORT hb_gt_SetClipboard( char *szData, ULONG ulSize );
extern ULONG HB_EXPORT hb_gt_GetClipboardSize( void );

/* Keyboard related declarations */

#define HB_BREAK_FLAG 256 /* 256, because that's what DJGPP returns Ctrl+Break as.
                             Clipper has no key code 256, so it may as well be
                             used for all the Harbour builds that need it */

/* Harbour keyboard support functions */
extern int    hb_inkey( BOOL bWait, double dSeconds, HB_inkey_enum event_mask ); /* Wait for keyboard input */
extern int    hb_inkeyGet( HB_inkey_enum event_mask );            /* Extract the next key from the Harbour keyboard buffer */
extern HB_EXPORT void hb_inkeyPut( int ch );          /* Inserts an inkey code into the keyboard buffer */
extern HB_EXPORT int  hb_inkeyLast( HB_inkey_enum event_mask );           /* Return the value of the last key that was extracted */
extern HB_EXPORT int  hb_setInkeyLast( int ch );      /* Force a value to LASTKEY and return the previous value */
extern HB_EXPORT int  hb_inkeyNext( HB_inkey_enum event_mask );           /* Return the next key without extracting it */
extern HB_EXPORT void hb_inkeyPoll( void );           /* Poll the console keyboard to stuff the Harbour buffer */
extern HB_EXPORT void hb_inkeyReset( BOOL allocate ); /* Reset the Harbour keyboard buffer */
extern HB_EXPORT int  hb_inkeyTranslate( int key, HB_inkey_enum event_make ); /* Translation extended codes to normal codes, if needed */
extern void hb_inkeyExit( void );

/* Mouse related declarations */

/* Public interface. These should never change, only be added to. */

extern BOOL   HB_EXPORT hb_mouseIsPresent( void );
extern BOOL   HB_EXPORT hb_mouseGetCursor( void );
extern void   HB_EXPORT hb_mouseSetCursor( BOOL bVisible );
extern int    HB_EXPORT hb_mouseCol( void );
extern int    HB_EXPORT hb_mouseRow( void );
extern void   HB_EXPORT hb_mouseSetPos( int iRow, int iCol );
extern BOOL   HB_EXPORT hb_mouseIsButtonPressed( int iButton );
extern int    HB_EXPORT hb_mouseCountButton( void );
extern void   HB_EXPORT hb_mouseSetBounds( int iTop, int iLeft, int iBottom, int iRight );
extern void   HB_EXPORT hb_mouseGetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight );

/* Private interface listed below. these are common to all platforms */

extern void   hb_mouse_Init( void );
extern void   hb_mouse_Exit( void );
extern BOOL   hb_mouse_IsPresent( void );
extern void   hb_mouse_Show( void );
extern void   hb_mouse_Hide( void );
extern int    hb_mouse_Col( void );
extern int    hb_mouse_Row( void );
extern void   hb_mouse_SetPos( int iRow, int iCol );
extern BOOL   hb_mouse_IsButtonPressed( int iButton );
extern int    hb_mouse_CountButton( void );
extern void   hb_mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight );
extern void   hb_mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight );

/*
   GT DRIVER request to api to update the status: USER made something on the
   window that request immediate attention of the GT system.
*/
extern HB_EXPORT void hb_gt_hasChanged( int status );

/* Gt to driver communication */
/*
   GT API request to driver to update its status: PRG level made
   something important to change.
*/
extern void hb_gt_update( int status );

/* Support for HB_GT_GOBJECT system */
extern void HB_EXPORT hb_gtAddGobject( HB_GT_GOBJECT *gobject );
extern void HB_EXPORT hb_gtDestroyGobject( HB_GT_GOBJECT *gobject );
extern void HB_EXPORT hb_gtClearGobjects( void );
extern HB_EXPORT HB_GT_COLDEF * hb_gt_gcolorFromString( char *color_name );
extern BOOL HB_EXPORT hb_gtGobjectInside( HB_GT_GOBJECT *gobject, int x1, int y1, int x2, int y2 );

/* Support for clipboard system */
extern void HB_EXPORT hb_gtGetClipboard( char *szData, ULONG *pulMaxSize );
extern void HB_EXPORT hb_gtSetClipboard( char *szData, ULONG ulSize );
extern ULONG HB_EXPORT hb_gtGetClipboardSize( void );
extern void HB_EXPORT hb_gtPasteFromClipboard( ULONG ulSize );

/*
   GT information query or update. msgType determines the kind of information
   to be queried or changed, the parameters are provided to pass status changes;
   different messages may use different parameters.
*/
extern int  hb_gt_info(int iMsgType, BOOL bUpdate, int iParm, void *vpParam );

/* for compilation with multi GT drivers
   User can choose GT on runtime by //GT:<gtname> switch [druzus] */

#if ! defined(HB_GT_NAME)
#  undef HB_MULTI_GT
#endif

#ifdef HB_MULTI_GT

#include "hbinit.h"

#if defined(__WATCOMC__)
    #undef _DOS
    #undef _WIN
#endif
/* These hacks are needed to force preprocessing if id/x is also a macro */
#define _HB_GT_PREF_( id )      _HB_GT_PREF__( id )
#define _HB_GT_PREF__( id )     _##id

#define HB_GT_FUNC( x )         HB_GT_FUNC_( x, _HB_GT_PREF_( HB_GT_NAME ) )
#define HB_GT_FUNC_( x, id )    HB_GT_FUNC__( x, id )
#define HB_GT_FUNC__( x, id )   hb##id##_##x

#define HB_GT_DRVNAME( id )     HB_GT_DRVNAME_( _HB_GT_PREF_( id ) )
#define HB_GT_DRVNAME_( id )    HB_GT_DRVNAME__( id )
#define HB_GT_DRVNAME__( id )   HB_GT_DRV##id

#define HB_GT_REQUEST( id )     HB_GT_REQUEST_( _HB_GT_PREF_( id ) )
#define HB_GT_REQUEST_( id )    HB_GT_REQUEST__( id )
#define HB_GT_REQUEST__( id )   HB_FUNC_EXTERN( HB_GT##id ); \
                                void hb_gt_ForceLink##id( void ) \
                                { \
                                   HB_FUNCNAME( HB_GT##id )(); \
                                }

#define HB_GT_ANNOUNCE( id )    HB_GT_ANNOUNCE_( _HB_GT_PREF_( id ) )
#define HB_GT_ANNOUNCE_( id )   HB_GT_ANNOUNCE__( id )
#define HB_GT_ANNOUNCE__( id )  HB_FUNC( HB_GT##id ) {}

/* convert lower case suffixes to upper */
#define _nul   _NUL
#define _std   _STD
#define _cgi   _CGI
#define _pca   _PCA
#define _crs   _CRS
#define _sln   _SLN
#define _win   _WIN
#define _wvt   _WVT
#define _dos   _DOS
#define _os2   _OS2
#define _tpl   _TPL
#define _QTc   _QTC
#define _xvt   _XVT
#define _alleg _ALLEG

/* names of GT */
#define HB_GT_DRV_NUL   "nul"
#define HB_GT_DRV_STD   "std"
#define HB_GT_DRV_CGI   "cgi"
#define HB_GT_DRV_PCA   "pca"
#define HB_GT_DRV_CRS   "crs"
#define HB_GT_DRV_SLN   "sln"
#define HB_GT_DRV_WIN   "win"
#define HB_GT_DRV_WVT   "wvt"
#define HB_GT_DRV_DOS   "dos"
#define HB_GT_DRV_OS2   "os2"
#define HB_GT_DRV_TPL   "tpl"
#define HB_GT_DRV_QTC   "QTc"
#define HB_GT_DRV_XVT   "xvt"
#define HB_GT_DRV_ALLEG "alleg"


typedef struct _HB_GT_FUNCS
{
    void    (* Init) ( int, int, int );
    void    (* Exit) ( void );
    USHORT  (* GetScreenWidth) ( void );
    USHORT  (* GetScreenHeight) ( void );
    SHORT   (* Col) ( void );
    SHORT   (* Row) ( void );
    void    (* SetPos) ( SHORT, SHORT, SHORT );
    BOOL    (* AdjustPos) ( BYTE *, ULONG );
    BOOL    (* IsColor) ( void );
    USHORT  (* GetCursorStyle) ( void );
    void    (* SetCursorStyle) ( USHORT );
    void    (* DispBegin) ( void );
    void    (* DispEnd) ( void );
    USHORT  (* DispCount) ( void );
    void    (* Puts) ( USHORT, USHORT, BYTE, BYTE *, ULONG );
    void    (* Replicate) ( USHORT, USHORT, BYTE, BYTE, ULONG );
    int     (* RectSize) ( USHORT, USHORT );
    void    (* GetText) ( USHORT, USHORT, USHORT, USHORT, BYTE * );
    void    (* PutText) ( USHORT, USHORT, USHORT, USHORT, BYTE * );
    void    (* SetAttribute) ( USHORT, USHORT, USHORT, USHORT, BYTE );
    void    (* Scroll) ( USHORT, USHORT, USHORT, USHORT, BYTE, SHORT, SHORT );
    BOOL    (* SetMode) ( USHORT, USHORT );
    BOOL    (* GetBlink) ( void );
    void    (* SetBlink) ( BOOL );
    char *  (* Version) ( void );
    USHORT  (* Box) ( SHORT, SHORT, SHORT, SHORT, BYTE *, BYTE );
    USHORT  (* BoxD) ( SHORT, SHORT, SHORT, SHORT, BYTE *, BYTE );
    USHORT  (* BoxS) ( SHORT, SHORT, SHORT, SHORT, BYTE *, BYTE );
    USHORT  (* HorizLine) ( SHORT, SHORT, SHORT, BYTE, BYTE );
    USHORT  (* VertLine) ( SHORT, SHORT, SHORT, BYTE, BYTE );
    BOOL    (* Suspend) ( void );
    BOOL    (* Resume) ( void );
    BOOL    (* PreExt) ( void );
    BOOL    (* PostExt) ( void );
    void    (* OutStd) ( BYTE *, ULONG );
    void    (* OutErr) ( BYTE *, ULONG );
    void    (* Tone) ( double, double );
    /* keyboard */
    int     (* ExtendedKeySupport) ( void );
    int     (* ReadKey) ( HB_inkey_enum );
    /* mouse */
    void    (* mouse_Init) ( void );
    void    (* mouse_Exit) ( void );
    BOOL    (* mouse_IsPresent) ( void );
    void    (* mouse_Show) ( void );
    void    (* mouse_Hide) ( void );
    int     (* mouse_Col) ( void );
    int     (* mouse_Row) ( void );
    void    (* mouse_SetPos) ( int, int );
    BOOL    (* mouse_IsButtonPressed) ( int );
    int     (* mouse_CountButton) ( void );
    void    (* mouse_SetBounds) ( int, int, int, int );
    void    (* mouse_GetBounds) ( int *, int *, int *, int * );
    /* extended GT functions */
    void    (* GetClipboard) ( char *, ULONG * );
    void    (* SetClipboard) ( char *, ULONG );
    ULONG   (* GetClipboardSize) ( void );

    /* GT CLIPBOARD functions */
    void    (* SetDispCP) ( char *, char *, BOOL );
    void    (* SetKeyCP) ( char *, char * );

    /* GT to DRIVER communication functions */
    void    (* update ) ( int );
    int     (* info ) (int, BOOL , int , void * );
} HB_GT_FUNCS, * PHB_GT_FUNCS;

typedef struct _HB_GT_INIT
{
    char    * id;
    void    (* gtInit) ( PHB_GT_FUNCS );
    void    (* mouseInit) ( PHB_GT_FUNCS );
} HB_GT_INIT, * PHB_GT_INIT;

extern BOOL   HB_EXPORT hb_gtRegister( PHB_GT_INIT gtInit );

/* Private interface in multi GT version, common to all platforms */

extern void   HB_GT_FUNC( gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr ) );
extern void   HB_GT_FUNC( gt_Exit( void ) );
extern BOOL   HB_GT_FUNC( gt_AdjustPos( BYTE * pStr, ULONG ulLen ) );
extern USHORT HB_GT_FUNC( gt_Box( SHORT uiTop, SHORT uiLeft, SHORT uiBottom, SHORT uiRight, BYTE * pbyFrame, BYTE byAttr ) );
extern USHORT HB_GT_FUNC( gt_BoxD( SHORT uiTop, SHORT uiLeft, SHORT uiBottom, SHORT uiRight, BYTE * pbyFrame, BYTE byAttr ) );
extern USHORT HB_GT_FUNC( gt_BoxS( SHORT uiTop, SHORT uiLeft, SHORT uiBottom, SHORT uiRight, BYTE * pbyFrame, BYTE byAttr ) );
extern SHORT  HB_GT_FUNC( gt_Col( void ) );
extern void   HB_GT_FUNC( gt_DispBegin( void ) );
extern USHORT HB_GT_FUNC( gt_DispCount( void ) );
extern void   HB_GT_FUNC( gt_DispEnd( void ) );
extern BOOL   HB_GT_FUNC( gt_GetBlink( void ) );
extern USHORT HB_GT_FUNC( gt_GetCursorStyle( void ) );
extern USHORT HB_GT_FUNC( gt_GetScreenHeight( void ) );
extern USHORT HB_GT_FUNC( gt_GetScreenWidth( void ) );
extern void   HB_GT_FUNC( gt_GetText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyDst ) );
extern USHORT HB_GT_FUNC( gt_HorizLine( SHORT uiRow, SHORT uiLeft, SHORT uiRight, BYTE byChar, BYTE byAttr ) );
extern BOOL   HB_GT_FUNC( gt_IsColor( void ) );
extern BOOL   HB_GT_FUNC( gt_PreExt( void ) );
extern BOOL   HB_GT_FUNC( gt_PostExt( void ) );
extern BOOL   HB_GT_FUNC( gt_Suspend( void ) ); /* suspend the terminal before the shell call */
extern BOOL   HB_GT_FUNC( gt_Resume( void ) ); /* resume the terminal after the shell call */
extern void   HB_GT_FUNC( gt_Puts( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE * pbyStr, ULONG ulLen ) );
extern void   HB_GT_FUNC( gt_PutText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbySrc ) );
extern int    HB_GT_FUNC( gt_ExtendedKeySupport( void ) );
extern int    HB_GT_FUNC( gt_ReadKey( HB_inkey_enum eventmask ) );
extern int    HB_GT_FUNC( gt_RectSize( USHORT rows, USHORT cols ) );
extern void   HB_GT_FUNC( gt_Replicate( USHORT uiTop, USHORT uiLeft, BYTE byAttr, BYTE byChar, ULONG ulLen ) );
extern SHORT  HB_GT_FUNC( gt_Row( void ) );
extern void   HB_GT_FUNC( gt_Scroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr, SHORT iRows, SHORT iCols ) );
extern void   HB_GT_FUNC( gt_SetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr ) );
extern void   HB_GT_FUNC( gt_SetBlink( BOOL bBlink ) );
extern void   HB_GT_FUNC( gt_SetCursorStyle( USHORT uiCursorShape ) );
extern BOOL   HB_GT_FUNC( gt_SetMode( USHORT uiRows, USHORT uiCols ) );
extern void   HB_GT_FUNC( gt_SetPos( SHORT iRow, SHORT iCol, SHORT iMethod ) );
extern void   HB_GT_FUNC( gt_Tone( double dFrequency, double dDuration ) );
extern char * HB_GT_FUNC( gt_Version( void ) );
extern USHORT HB_GT_FUNC( gt_VertLine( SHORT uiCol, SHORT uiTop, SHORT uiBottom, BYTE byChar, BYTE byAttr ) );

extern void   HB_GT_FUNC( gt_OutStd( BYTE * pbyStr, ULONG ulLen ) );
extern void   HB_GT_FUNC( gt_OutErr( BYTE * pbyStr, ULONG ulLen ) );

extern void   HB_GT_FUNC( gt_SetDispCP( char * pszTermCDP, char * pszHostCDP, BOOL bBox ) );
extern void   HB_GT_FUNC( gt_SetKeyCP( char * pszTermCDP, char * pszHostCDP ) );

extern void   HB_GT_FUNC( mouse_Init( void ) );
extern void   HB_GT_FUNC( mouse_Exit( void ) ) ;
extern BOOL   HB_GT_FUNC( mouse_IsPresent( void ) );
extern void   HB_GT_FUNC( mouse_Show( void ) );
extern void   HB_GT_FUNC( mouse_Hide( void ) );
extern int    HB_GT_FUNC( mouse_Col( void ) );
extern int    HB_GT_FUNC( mouse_Row( void ) );
extern void   HB_GT_FUNC( mouse_SetPos( int iRow, int iCol ) );
extern BOOL   HB_GT_FUNC( mouse_IsButtonPressed( int iButton ) );
extern int    HB_GT_FUNC( mouse_CountButton( void ) );
extern void   HB_GT_FUNC( mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight ) );
extern void   HB_GT_FUNC( mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight ) );

/* Gt clipboard functions */
extern void HB_EXPORT HB_GT_FUNC( gt_GetClipboard( char *szData, ULONG *pulMaxSize ) );
extern void HB_EXPORT HB_GT_FUNC( gt_SetClipboard( char *szData, ULONG ulSize ) );
extern ULONG HB_EXPORT HB_GT_FUNC( gt_GetClipboardSize( void ) );

/* Gt to driver communication */
/*
   GT API request to driver to update its status: PRG level made
   something important to change.
*/
extern void HB_GT_FUNC( gt_update( int status ) );


/*
   GT information query or update. msgType determines the kind of information
   to be queried or changed, the parameters are provided to pass status changes;
   different messages may use different parameters.
*/
extern int  HB_GT_FUNC( gt_info(int iMsgType, BOOL bUpdate, int iParm, void *vpParam ));

#else
#  define HB_GT_FUNC(x)   HB_GT_FUNC_(x)
#  define HB_GT_FUNC_(x)  hb_##x
#endif  /* HB_MULTI_GT */


/* SetKey related declarations */

/* Public interface. These should never change, only be added to. */

extern void   hb_setkeyInit( void );
extern void   hb_setkeyExit( void );

/* Private interface listed below. these are common to all platforms */

/* none as of yet */

/* JC1: Supporting Screen Output lock also from other modules */
#ifdef HB_THREAD_SUPPORT

void hb_consoleLock();
void hb_consoleUnlock();

#endif

HB_EXTERN_END

#endif /* HB_APIGT_H_ */
