/*
 * $Id$
 */

/*
 * [x]Harbour Project source code:
 * Header file for Win32prn Class
 *     Copyright 2004 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 *
 * See doc/license.txt for licensing terms.
 *
 * www - http://www.harbour-project.org - http://www.xharbour.org
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


#ifndef _WIN32PRN_CH
#define _WIN32PRN_CH

// Cut from wingdi.h

#define MM_TEXT             1
#define MM_LOMETRIC         2
#define MM_HIMETRIC         3
#define MM_LOENGLISH        4
#define MM_HIENGLISH        5

// Device Parameters for GetDeviceCaps()

#define HORZSIZE      4     // Horizontal size in millimeters
#define VERTSIZE      6     // Vertical size in millimeters
#define HORZRES       8     // Horizontal width in pixels
#define VERTRES       10    // Vertical height in pixels
#define NUMBRUSHES    16    // Number of brushes the device has
#define NUMPENS       18    // Number of pens the device has
#define NUMFONTS      22    // Number of fonts the device has
#define NUMCOLORS     24    // Number of colors the device supports
#define RASTERCAPS    38    // Bitblt capabilities

#define LOGPIXELSX    88    // Logical pixels/inch in X
#define LOGPIXELSY    90    // Logical pixels/inch in Y

#define PHYSICALWIDTH   110 // Physical Width in device units
#define PHYSICALHEIGHT  111 // Physical Height in device units
#define PHYSICALOFFSETX 112 // Physical Printable Area x margin
#define PHYSICALOFFSETY 113 // Physical Printable Area y margin
#define SCALINGFACTORX  114 // Scaling factor x
#define SCALINGFACTORY  115 // Scaling factor y

/* Device mode: bin selections */
#define DMBIN_FIRST         DMBIN_UPPER
#define DMBIN_UPPER         1
#define DMBIN_ONLYONE       1
#define DMBIN_LOWER         2
#define DMBIN_MIDDLE        3
#define DMBIN_MANUAL        4
#define DMBIN_ENVELOPE      5
#define DMBIN_ENVMANUAL     6
#define DMBIN_AUTO          7
#define DMBIN_TRACTOR       8
#define DMBIN_SMALLFMT      9
#define DMBIN_LARGEFMT      10
#define DMBIN_LARGECAPACITY 11
#define DMBIN_CASSETTE      14
#define DMBIN_FORMSOURCE    15
#define DMBIN_LAST          DMBIN_FORMSOURCE

/* Device mode: print qualities */
#define DMRES_DRAFT         (-1)
#define DMRES_LOW           (-2)
#define DMRES_MEDIUM        (-3)
#define DMRES_HIGH          (-4)

/* Device mode: duplex enable */
#define DMDUP_SIMPLEX    1
#define DMDUP_VERTICAL   2
#define DMDUP_HORIZONTAL 3

/* Text Alignment Options */
#define TA_NOUPDATECP                0
#define TA_UPDATECP                  1

#define TA_LEFT                      0
#define TA_RIGHT                     2
#define TA_CENTER                    6

#define TA_TOP                       0
#define TA_BOTTOM                    8
#define TA_BASELINE                  24

#define MM_TO_INCH 25.4

/* Char Set */
#define ANSI_CHARSET            0
#define DEFAULT_CHARSET         1
#define SYMBOL_CHARSET          2
#define MAC_CHARSET             77
#define SHIFTJIS_CHARSET        128
#define HANGEUL_CHARSET         129
#define HANGUL_CHARSET          129
#define JOHAB_CHARSET           130
#define GB2312_CHARSET          134
#define CHINESEBIG5_CHARSET     136
#define GREEK_CHARSET           161
#define TURKISH_CHARSET         162
#define VIETNAMESE_CHARSET      163
#define HEBREW_CHARSET          177
#define ARABIC_CHARSET          178
#define BALTIC_CHARSET          186
#define RUSSIAN_CHARSET         204
#define THAI_CHARSET            222
#define EASTEUROPE_CHARSET      238
#define OEM_CHARSET             255

/* Background mode */
#define TRANSPARENT  1
#define OPAQUE  2

/* Font Weight */
#define FW_DONTCARE    0
#define FW_THIN        100
#define FW_EXTRALIGHT  200
#define FW_LIGHT       300
#define FW_NORMAL      400
#define FW_MEDIUM      500
#define FW_SEMIBOLD    600
#define FW_BOLD        700
#define FW_EXTRABOLD   800
#define FW_HEAVY       900

/* Device mode: Paper Size */
#define DMPAPER_FIRST                          DMPAPER_LETTER
#define DMPAPER_LETTER                         1  
#define DMPAPER_LETTERSMALL                    2  
#define DMPAPER_TABLOID                        3  
#define DMPAPER_LEDGER                         4  
#define DMPAPER_LEGAL                          5  
#define DMPAPER_STATEMENT                      6  
#define DMPAPER_EXECUTIVE                      7  
#define DMPAPER_A3                             8  
#define DMPAPER_A4                             9  
#define DMPAPER_A4SMALL                        10  
#define DMPAPER_A5                             11  
#define DMPAPER_B4                             12  
#define DMPAPER_B5                             13
#define DMPAPER_FOLIO                          14  
#define DMPAPER_QUARTO                         15  
#define DMPAPER_10X14                          16
#define DMPAPER_11X17                          17  
#define DMPAPER_NOTE                           18  
#define DMPAPER_ENV_9                          19  
#define DMPAPER_ENV_10                         20
#define DMPAPER_ENV_11                         21  
#define DMPAPER_ENV_12                         22  
#define DMPAPER_ENV_14                         23  
#define DMPAPER_CSHEET                         24  
#define DMPAPER_DSHEET                         25  
#define DMPAPER_ESHEET                         26  
#define DMPAPER_ENV_DL                         27  
#define DMPAPER_ENV_C5                         28  
#define DMPAPER_ENV_C3                         29  
#define DMPAPER_ENV_C4                         30  
#define DMPAPER_ENV_C6                         31  
#define DMPAPER_ENV_C65                        32  
#define DMPAPER_ENV_B4                         33  
#define DMPAPER_ENV_B5                         34  
#define DMPAPER_ENV_B6                         35  
#define DMPAPER_ENV_ITALY                      36  
#define DMPAPER_ENV_MONARCH                    37  
#define DMPAPER_ENV_PERSONAL                   38  
#define DMPAPER_FANFOLD_US                     39
#define DMPAPER_FANFOLD_STD_GERMAN             40  
#define DMPAPER_FANFOLD_LGL_GERMAN             41  
#define DMPAPER_ISO_B4                         42  
#define DMPAPER_JAPANESE_POSTCARD              43  
#define DMPAPER_9X11                           44  
#define DMPAPER_10X11                          45  
#define DMPAPER_15X11                          46  
#define DMPAPER_ENV_INVITE                     47  
#define DMPAPER_RESERVED_48                    48  
#define DMPAPER_RESERVED_49                    49  
#define DMPAPER_LETTER_EXTRA                   50  
#define DMPAPER_LEGAL_EXTRA                    51  
#define DMPAPER_TABLOID_EXTRA                  52  
#define DMPAPER_A4_EXTRA                       53  
#define DMPAPER_LETTER_TRANSVERSE              54  
#define DMPAPER_A4_TRANSVERSE                  55  
#define DMPAPER_LETTER_EXTRA_TRANSVERSE        56 
#define DMPAPER_A_PLUS                         57  
#define DMPAPER_B_PLUS                         58  
#define DMPAPER_LETTER_PLUS                    59
#define DMPAPER_A4_PLUS                        60  
#define DMPAPER_A5_TRANSVERSE                  61  
#define DMPAPER_B5_TRANSVERSE                  62
#define DMPAPER_A3_EXTRA                       63  
#define DMPAPER_A5_EXTRA                       64  
#define DMPAPER_B5_EXTRA                       65  
#define DMPAPER_A2                             66
#define DMPAPER_A3_TRANSVERSE                  67  
#define DMPAPER_A3_EXTRA_TRANSVERSE            68  
#define DMPAPER_DBL_JAPANESE_POSTCARD          69 
#define DMPAPER_A6                             70  
#define DMPAPER_JENV_KAKU2                     71  
#define DMPAPER_JENV_KAKU3                     72  
#define DMPAPER_JENV_CHOU3                     73
#define DMPAPER_JENV_CHOU4                     74  
#define DMPAPER_LETTER_ROTATED                 75  
#define DMPAPER_A3_ROTATED                     76  
#define DMPAPER_A4_ROTATED                     77  
#define DMPAPER_A5_ROTATED                     78  
#define DMPAPER_B4_JIS_ROTATED                 79  
#define DMPAPER_B5_JIS_ROTATED                 80  
#define DMPAPER_JAPANESE_POSTCARD_ROTATED      81 
#define DMPAPER_DBL_JAPANESE_POSTCARD_ROTATED  82 
#define DMPAPER_A6_ROTATED                     83  
#define DMPAPER_JENV_KAKU2_ROTATED             84  
#define DMPAPER_JENV_KAKU3_ROTATED             85
#define DMPAPER_JENV_CHOU3_ROTATED             86  
#define DMPAPER_JENV_CHOU4_ROTATED             87  
#define DMPAPER_B6_JIS                         88  
#define DMPAPER_B6_JIS_ROTATED                 89  
#define DMPAPER_12X11                          90  
#define DMPAPER_JENV_YOU4                      91  
#define DMPAPER_JENV_YOU4_ROTATED              92  
#define DMPAPER_P16K                           93  
#define DMPAPER_P32K                           94  
#define DMPAPER_P32KBIG                        95  
#define DMPAPER_PENV_1                         96  
#define DMPAPER_PENV_2                         97  
#define DMPAPER_PENV_3                         98  
#define DMPAPER_PENV_4                         99  
#define DMPAPER_PENV_5                         100 
#define DMPAPER_PENV_6                         101 
#define DMPAPER_PENV_7                         102 
#define DMPAPER_PENV_8                         103 
#define DMPAPER_PENV_9                         104 
#define DMPAPER_PENV_10                        105
#define DMPAPER_P16K_ROTATED                   106 
#define DMPAPER_P32K_ROTATED                   107 
#define DMPAPER_P32KBIG_ROTATED                108
#define DMPAPER_PENV_1_ROTATED                 109 
#define DMPAPER_PENV_2_ROTATED                 110 
#define DMPAPER_PENV_3_ROTATED                 111 
#define DMPAPER_PENV_4_ROTATED                 112
#define DMPAPER_PENV_5_ROTATED                 113 
#define DMPAPER_PENV_6_ROTATED                 114 
#define DMPAPER_PENV_7_ROTATED                 115 
#define DMPAPER_PENV_8_ROTATED                 116 
#define DMPAPER_PENV_9_ROTATED                 117 
#define DMPAPER_PENV_10_ROTATED                118 

#define DMPAPER_USER                           256

/* Brush Styles */
#define BS_SOLID            0
#define BS_NULL             1
#define BS_HOLLOW           BS_NULL
#define BS_HATCHED          2
#define BS_PATTERN          3
#define BS_INDEXED          4
#define BS_DIBPATTERN       5
#define BS_DIBPATTERNPT     6
#define BS_PATTERN8X8       7
#define BS_DIBPATTERN8X8    8
#define BS_MONOPATTERN      9

/* Hatch Styles */
#define HS_HORIZONTAL       0
#define HS_VERTICAL         1
#define HS_FDIAGONAL        2
#define HS_BDIAGONAL        3
#define HS_CROSS            4
#define HS_DIAGCROSS        5

/* Pen Styles */
#define PS_SOLID            0
#define PS_DASH             1
#define PS_DOT              2
#define PS_DASHDOT          3
#define PS_DASHDOTDOT       4
#define PS_NULL             5
#define PS_INSIDEFRAME      6
#define PS_USERSTYLE        7
#define PS_ALTERNATE        8
#define PS_STYLE_MASK       0x0000000F

/* Stock Logical Objects */
#define WHITE_BRUSH         0
#define LTGRAY_BRUSH        1
#define GRAY_BRUSH          2
#define DKGRAY_BRUSH        3
#define BLACK_BRUSH         4
#define NULL_BRUSH          5
#define HOLLOW_BRUSH        NULL_BRUSH

#define WHITE_PEN           6
#define BLACK_PEN           7
#define NULL_PEN            8

#define OEM_FIXED_FONT      10
#define ANSI_FIXED_FONT     11
#define ANSI_VAR_FONT       12
#define SYSTEM_FONT         13
#define DEVICE_DEFAULT_FONT 14
#define DEFAULT_PALETTE     15
#define SYSTEM_FIXED_FONT   16
#ifndef _WINUSER_

/* Draw Text Styles */
#define DT_TOP                      0x00000000
#define DT_LEFT                     0x00000000
#define DT_CENTER                   0x00000001
#define DT_RIGHT                    0x00000002
#define DT_VCENTER                  0x00000004
#define DT_BOTTOM                   0x00000008
#define DT_WORDBREAK                0x00000010
#define DT_SINGLELINE               0x00000020
#define DT_EXPANDTABS               0x00000040
#define DT_TABSTOP                  0x00000080
#define DT_NOCLIP                   0x00000100
#define DT_EXTERNALLEADING          0x00000200
#define DT_CALCRECT                 0x00000400
#define DT_NOPREFIX                 0x00000800
#define DT_INTERNAL                 0x00001000

#define DT_EDITCONTROL              0x00002000
#define DT_PATH_ELLIPSIS            0x00004000
#define DT_END_ELLIPSIS             0x00008000
#define DT_MODIFYSTRING             0x00010000
#define DT_RTLREADING               0x00020000
#define DT_WORD_ELLIPSIS            0x00040000
#define DT_NOFULLWIDTHCHARBREAK     0x00080000
#define DT_HIDEPREFIX               0x00100000
#define DT_PREFIXONLY               0x00200000
#endif // !_WINUSER_

/* Color */
#define BLACK     0x00000000
#define BLUE      0x00FF0000
#define BROWN     0x000A0A80
#define CYAN      0x00FFFF00
#define GRAY      0x00808080
#define GREEN     0x0000FF00
#define MAGENTA   0x00FF00FF
#define ORANGE    0X000A80FF
#define PINK  	  0x00CBC0FF
#define PURPLE	  0x00800080
#define RED       0x000000FF
#define WHITE     0x00FFFFFF
#define YELLOW    0x0000FFFF

#define DARKGRAY  0x00A9A9A9
#define LIGHTGRAY 0x00D3D3D3

#endif
