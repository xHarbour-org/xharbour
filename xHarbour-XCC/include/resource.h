/****************************************************************************
 *                                                                          *
 * File    : resource.h                                                     *
 *                                                                          *
 * Purpose : Win32 resource definitions.                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-11-27  Created by Pelle Orinius.                            *
 *           01-05-20  Added resource type RSRC_T_MANIFEST (XP).            *
 *           01-11-20  Added extended dialog definitions.                   *
 *           02-07-09  Added menubar definitions.                           *
 *                                                                          *
 ****************************************************************************/

#ifndef _WINRES_H
#define _WINRES_H

#ifdef __cplusplus
extern "C" {
#endif

#include <potypes.h>

#ifndef _WCHAR_T_DEFINED
#include <wchar.h>
#endif

#ifndef NOWIN32

/* Predefined resource types */
#ifndef RSRC_T_CURSOR
#define RSRC_T_NEWRESOURCE      0x2000
#define RSRC_T_ERROR            0x7FFF
#define RSRC_T_CURSOR           1
#define RSRC_T_BITMAP           2
#define RSRC_T_ICON             3
#define RSRC_T_MENU             4
#define RSRC_T_DIALOG           5
#define RSRC_T_STRING           6
#define RSRC_T_FONTDIR          7
#define RSRC_T_FONT             8
#define RSRC_T_ACCELERATOR      9
#define RSRC_T_RCDATA           10
#define RSRC_T_MESSAGETABLE     11
#define RSRC_T_GROUP_CURSOR     12
#define RSRC_T_GROUP_ICON       14
#define RSRC_T_VERSION          16
#define RSRC_T_DLGINCLUDE       17
#define RSRC_T_PLUGPLAY         19
#define RSRC_T_VXD              20
#define RSRC_T_ANICURSOR        21
#define RSRC_T_ANIICON          22
#define RSRC_T_HTML             23
#define RSRC_T_MANIFEST         24
#define RSRC_T_NEWBITMAP        (RSRC_T_BITMAP|RSRC_T_NEWRESOURCE)
#define RSRC_T_NEWMENU          (RSRC_T_MENU|RSRC_T_NEWRESOURCE)
#define RSRC_T_NEWDIALOG        (RSRC_T_DIALOG|RSRC_T_NEWRESOURCE)
#endif

/* Control class ordinals */
#define RSRC_C_BUTTON       0x80
#define RSRC_C_EDIT         0x81
#define RSRC_C_STATIC       0x82
#define RSRC_C_LISTBOX      0x83
#define RSRC_C_SCROLLBAR    0x84
#define RSRC_C_COMBOBOX     0x85

#include <pshpack2.h>

struct _RSRC_hdr_head {
    long ressize;               /* size of data without header */
    long hdrsize;               /* length of the additional header */
};

#define RSRC_HDR_HEAD  struct _RSRC_hdr_head

struct _RSRC_hdr_tail {
    long version_data;          /* predefined resource data version */
    ushort_t memflags;          /* resource memory flags */
    ushort_t language;          /* Unicode support for NLS */
    long version;               /* version of the resource data */
    ulong_t characteristics;    /* characteristics of the data */
};

#define RSRC_HDR_TAIL  struct _RSRC_hdr_tail

/* values for memflags */
#define RSRC_F_MOVEABLE     0x0010
#define RSRC_F_FIXED        ~RSRC_F_MOVEABLE
#define RSRC_F_PURE         0x0020
#define RSRC_F_IMPURE       ~RSRC_F_PURE
#define RSRC_F_PRELOAD      0x0040
#define RSRC_F_LOADONCALL   ~RSRC_F_PRELOAD
#define RSRC_F_DISCARDABLE  0x1000

/*
 * Win32 binary resource header.
 */
struct _RSRC_hdr {
    RSRC_HDR_HEAD head;         /* common header head */
    wchar_t type[2];            /* ordinal (or unicode string) */
    wchar_t name[2];            /* ordinal (or unicode string) */
    RSRC_HDR_TAIL tail;         /* common header tail */
};

#define RSRC_HDR  struct _RSRC_hdr

/*
 * Win32 menu header.
 */
struct _RSRC_menuhdr {
    short version;              /* version (0) */
    ushort_t hdrsize;           /* additional header size */
};

#define RSRC_MENUHDR  struct _RSRC_menuhdr

/*
 * Win32 menu popup header.
 */
struct _RSRC_menupopup {
    ushort_t flags;             /* flags */
    wchar_t text[1];            /* text for popup item */
};

#define RSRC_MENUPOPUP  struct _RSRC_menupopup

/*
 * Win32 menu item header.
 */
struct _RSRC_menuitem {
    ushort_t flags;             /* flags */
    short id;                   /* id of menu item */
    wchar_t text[1];            /* text for menu item */
};

#define RSRC_MENUITEM  struct _RSRC_menuitem

/* flags values */
#define RSRC_M_GRAYED           0x0001
#define RSRC_M_INACTIVE         0x0002
#define RSRC_M_BITMAP           0x0004
#define RSRC_M_CHECKED          0x0008
#define RSRC_M_POPUP            0x0010
#define RSRC_M_MENUBARBREAK     0x0020
#define RSRC_M_MENUBREAK        0x0040
#define RSRC_M_ENDMENU          0x0080
#define RSRC_M_OWNERDRAW        0x0100
#define RSRC_M_SEPARATOR        0x1000
#define RSRC_M_HELP             0x4000

/*
 * Win32 extended menu header.
 */
struct _RSRC_menuexhdr {
    short version;              /* version (1) */
    ushort_t hdrsize;           /* additional header size */
    long helpid;                /* help id */
};

#define RSRC_MENUEXHDR  struct _RSRC_menuexhdr

/*
 * Win32 extended menu item.
 */
struct _RSRC_menuexitem {
    ulong_t type;               /* menu item type flags */
    ulong_t state;              /* menu item state flags */
    long id;                    /* id of menu item */
    ushort_t flags;             /* flags */
    wchar_t text[1];            /* text for menu item */
};

#define RSRC_MENUEXITEM  struct _RSRC_menuexitem

/* flags values */
#define RSRC_XM_POPUP   0x0001
#define RSRC_XM_END     0x0080

/*
 * WinCE menubar item.
 */
struct _RSRC_menubaritem {
    short image;                /* image index */
    short id;                   /* id of menu item */
    ushort_t state;             /* menu item state flags */
    ushort_t style;             /* menu item style flags */
    short strid;                /* string id (text for menu item) */
    short unknown1;
    short unknown2;
};

#define RSRC_MENUBARITEM  struct _RSRC_menubaritem

/* state values */
#define RSRC_S_CHECKED          0x0001
#define RSRC_S_PRESSED          0x0002
#define RSRC_S_ENABLED          0x0004
#define RSRC_S_HIDDEN           0x0008
#define RSRC_S_INDETERMINATE    0x0010
#define RSRC_S_WRAP             0x0020
#define RSRC_S_ELLIPSES         0x0040
#define RSRC_S_HIGHLIGHTED      0x0080

/* style values */
#define RSRC_Y_BUTTON           0x0000
#define RSRC_Y_SEPARATOR        0x0001
#define RSRC_Y_CHECK            0x0002
#define RSRC_Y_GROUP            0x0004
#define RSRC_Y_DROPDOWN         0x0008
#define RSRC_Y_AUTOSIZE         0x0010
#define RSRC_Y_WRAPABLE         0x0200

/*
 * Win32 dialog template.
 */
struct _RSRC_dlghdr {
    ulong_t style;              /* style */
    ulong_t exstyle;            /* extended style */
    ushort_t nitems;            /* number of dialog items */
    short x;
    short y;
    short width;
    short height;
};

#define RSRC_DLGHDR  struct _RSRC_dlghdr

/*
 * Win32 dialog item template.
 */
struct _RSRC_dlgent {
    ulong_t style;              /* style */
    ulong_t exstyle;            /* extended style */
    short x;
    short y;
    short width;
    short height;
    short id;
};

#define RSRC_DLGENT  struct _RSRC_dlgent

/*
 * Win32 extended dialog template.
 */
struct _RSRC_dlgexhdr {
    short version;              /* version */
    short signature;            /* always 0xFFFF */
    long helpid;                /* help id */
    ulong_t exstyle;            /* extended style */
    ulong_t style;              /* style */
    ushort_t nitems;            /* number of dialog items */
    short x;
    short y;
    short width;
    short height;
    /* sz_or_ord menu */
    /* sz_or_ord class */
    /* wchar_t title[] */
    /* short pointsize (if DS_SETFONT) */
    /* short weight (if DS_SETFONT) */
    /* char italic (if DS_SETFONT) */
    /* char charset (if DS_SETFONT) */
    /* wchar_t font[] (if DS_SETFONT) */
};

#define RSRC_DLGEXHDR  struct _RSRC_dlgexhdr

/*
 * Win32 extended dialog item template.
 */
struct _RSRC_dlgexent {
    long helpid;                /* help id */
    ulong_t exstyle;            /* extended style */
    ulong_t style;              /* style */
    short x;
    short y;
    short width;
    short height;
    short id;
    short padding;
    /* sz_or_ord class */
    /* sz_or_ord title */
    /* short extra_size */
};

#define RSRC_DLGEXENT  struct _RSRC_dlgexent

/*
 * Win32 accelerator table entry.
 */
struct _RSRC_accent
{
    ushort_t flags;             /* flags */
    short key;                  /* ANSI character value or virtual-key code */
    short id;                   /* accelerator id */
    short pad;                  /* padding to assure alignment */
};

#define RSRC_ACCENT  struct _RSRC_accent

/* flags values */
#define RSRC_A_VIRTKEY  0x0001
#define RSRC_A_NOINVERT 0x0002
#define RSRC_A_SHIFT    0x0004
#define RSRC_A_CONTROL  0x0008
#define RSRC_A_ALT      0x0010
#define RSRC_A_LAST     0x0080

/* Win32 stringtable */
#define RSRC_STRINGS_PER_BLOCK  16

/*
 * Win32 message table entry.
 */
struct _RSRC_msgent {
    ushort_t size;              /* size */
    ushort_t flags;             /* flags */
    union {
        char atext[1];
        wchar_t wtext[1];
    } u;
};

#define RSRC_MSGENT  struct _RSRC_msgent

/* flags values */
#define RSRC_M_UNICODE  0x0001

/*
 * Win32 message table block.
 */
struct _RSRC_msgblk {
    long lowid;
    long highid;
    long offset;
};

#define RSRC_MSGBLK  struct _RSRC_msgblk

/*
 * Win32 message table.
 */
struct _RSRC_msgtab {
    long nblocks;               /* number of blocks */
    RSRC_MSGBLK blocks[1];      /* message blocks (* nblocks) */
};

#define RSRC_MSGTAB  struct _RSRC_msgtab

/*
 * Win32 icon and cursor resources.
 */
struct _RSRC_newhdr
{
    short reserved;
    short type;
    short nentries;
};

#define RSRC_NEWHDR  struct _RSRC_newhdr

struct _RSRC_icocur {
    union {
        struct {
            char width;         /* width of icon image */
            char height;        /* height of icon image */
            short ncolors;      /* number of colors */
        } ico;
        struct {
            short width;        /* width of cursor image */
            short height;       /* height of cursor image */
        } cur;
    } u;
    short nplanes;              /* number of planes */
    short nbitspixel;           /* number of bits per pixel */
    long ressize;               /* size of image */
    short id;
};

#define RSRC_ICOCUR  struct _RSRC_icocur

struct _RSRC_hotspot {
    short x;
    short y;
};

#define RSRC_HOTSPOT  struct _RSRC_hotspot

/*
 * Win32 fixed version info.
 */
struct _RSRC_verfix {
    long signature;             /* e.g. 0xfeef04bd (RSRC_VF_SIGNATURE) */
    long structver;             /* e.g. 0x00010000 = "1.00" (RSRC_VF_STRUCTVER) */
    short file_hi_minver;       /* e.g. 0x0075 = ".75" */
    short file_hi_majver;       /* e.g. 0x0003 = "3" */
    short file_lo_minver;       /* e.g. 0x0031 = ".31" */
    short file_lo_majver;       /* e.g. 0x0000 = "0" = 3.75.0.31 */
    short prod_hi_minver;       /* e.g. 0x0010 = ".10" */
    short prod_hi_majver;       /* e.g. 0x0003 = "3" */
    short prod_lo_minver;       /* e.g. 0x0031 = ".31" */
    short prod_lo_majver;       /* e.g. 0x0000 = "0" = 3.10.0.31 */
    ulong_t flagsmask;
    ulong_t flags;
    ulong_t os;
    ulong_t type;
    ulong_t subtype;
    long date_hi;               /* e.g. 0 */
    long date_lo;               /* e.g. 0 */
};

#define RSRC_VERFIX  struct _RSRC_verfix

/* Values for sigature and structver */
#define RSRC_VF_SIGNATURE   0xFEEF04BD
#define RSRC_VF_STRUCTVER   0x00010000

/*
 * Win32 variable version info header.
 */
struct _RSRC_verhdr {
    ushort_t size;
    ushort_t versize;
    short type;
};

#define RSRC_VERHDR  struct _RSRC_verhdr

/* type values */
#define RSRC_V_TEXT     1
#define RSRC_V_BINARY   0

#include "poppack.h"    /* back to default */

#endif /* NOWIN32 */

#endif /* _WINRES_H */
