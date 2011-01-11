/****************************************************************************
 *                                                                          *
 * File    : cpp.c                                                          *
 *                                                                          *
 * Purpose : Win32 Resource Compiler; preprocessor; main.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "rc.h"

/* Globals */
char outbuf[16384];
char *outpp = outbuf;
SOURCE *cursource;
char *curtime;
int incdepth;
TOKENROW maintr = {0};

/* Locals */
static int ifdepth = 0;
static int ifsatisfied[NIF];
static bool_t skipping = FALSE;

/* Static function prototypes */
static void control(TOKENROW *);

/****************************************************************************
 *                                                                          *
 * Function: pp_init                                                        *
 *                                                                          *
 * Purpose : Initialize the preprocessor.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *           03-07-22  Added __PORC__ symbol.                               *
 *           03-08-03  Added relevant symbols from "windows.h" (Borland).   *
 *                                                                          *
 ****************************************************************************/

void pp_init(void)
{
    setup_kwtab();
    setup_include();
    setup_lexfsm();

    /* Define the predefined symbols */
    pp_define("__PORC__=260");
    pp_define("RC_INVOKED");
    pp_define("_WIN32");

    /* Define generic window styles */
    pp_define("WS_OVERLAPPED=0x00000000L");
    pp_define("WS_POPUP=0x80000000L");
    pp_define("WS_CHILD=0x40000000L");
    pp_define("WS_MINIMIZE=0x20000000L");
    pp_define("WS_VISIBLE=0x10000000L");
    pp_define("WS_DISABLED=0x08000000L");
    pp_define("WS_CLIPSIBLINGS=0x04000000L");
    pp_define("WS_CLIPCHILDREN=0x02000000L");
    pp_define("WS_MAXIMIZE=0x01000000L");
    pp_define("WS_CAPTION=0x00C00000L");
    pp_define("WS_BORDER=0x00800000L");
    pp_define("WS_DLGFRAME=0x00400000L");
    pp_define("WS_VSCROLL=0x00200000L");
    pp_define("WS_HSCROLL=0x00100000L");
    pp_define("WS_SYSMENU=0x00080000L");
    pp_define("WS_THICKFRAME=0x00040000L");
    pp_define("WS_GROUP=0x00020000L");
    pp_define("WS_TABSTOP=0x00010000L");
    pp_define("WS_MINIMIZEBOX=0x00020000L");
    pp_define("WS_MAXIMIZEBOX=0x00010000L");
    pp_define("WS_TILED=WS_OVERLAPPED");
    pp_define("WS_ICONIC=WS_MINIMIZE");
    pp_define("WS_SIZEBOX=WS_THICKFRAME");
    pp_define("WS_TILEDWINDOW=WS_OVERLAPPEDWINDOW");
    pp_define("WS_OVERLAPPEDWINDOW=WS_OVERLAPPED|WS_CAPTION|WS_SYSMENU|WS_THICKFRAME|WS_MINIMIZEBOX|WS_MAXIMIZEBOX");
    pp_define("WS_POPUPWINDOW=WS_POPUP|WS_BORDER|WS_SYSMENU");
    pp_define("WS_CHILDWINDOW=WS_CHILD");

    /* Define generic extended window styles */
    pp_define("WS_EX_DLGMODALFRAME=0x00000001L");
    pp_define("WS_EX_NOPARENTNOTIFY=0x00000004L");
    pp_define("WS_EX_TOPMOST=0x00000008L");
    pp_define("WS_EX_ACCEPTFILES=0x00000010L");
    pp_define("WS_EX_TRANSPARENT=0x00000020L");
    pp_define("WS_EX_MDICHILD=0x00000040L");
    pp_define("WS_EX_TOOLWINDOW=0x00000080L");
    pp_define("WS_EX_WINDOWEDGE=0x00000100L");
    pp_define("WS_EX_CLIENTEDGE=0x00000200L");
    pp_define("WS_EX_CONTEXTHELP=0x00000400L");
    pp_define("WS_EX_RIGHT=0x00001000L");
    pp_define("WS_EX_LEFT=0x00000000L");
    pp_define("WS_EX_RTLREADING=0x00002000L");
    pp_define("WS_EX_LTRREADING=0x00000000L");
    pp_define("WS_EX_LEFTSCROLLBAR=0x00004000L");
    pp_define("WS_EX_RIGHTSCROLLBAR=0x00000000L");
    pp_define("WS_EX_CONTROLPARENT=0x00010000L");
    pp_define("WS_EX_STATICEDGE=0x00020000L");
    pp_define("WS_EX_APPWINDOW=0x00040000L");
    pp_define("WS_EX_OVERLAPPEDWINDOW=WS_EX_WINDOWEDGE|WS_EX_CLIENTEDGE");
    pp_define("WS_EX_PALETTEWINDOW=WS_EX_WINDOWEDGE|WS_EX_TOOLWINDOW|WS_EX_TOPMOST");
    pp_define("WS_EX_NOINHERITLAYOUT=0x00100000L");
    pp_define("WS_EX_LAYOUTRTL=0x00400000L");
    pp_define("WS_EX_LAYERED=0x00080000");
    pp_define("WS_EX_NOACTIVATE=0x08000000L");

    /* Define dialog styles */
    pp_define("DS_ABSALIGN=0x01L");
    pp_define("DS_SYSMODAL=0x02L");
    pp_define("DS_LOCALEDIT=0x20L");
    pp_define("DS_SETFONT=0x40L");
    pp_define("DS_MODALFRAME=0x80L");
    pp_define("DS_NOIDLEMSG=0x100L");
    pp_define("DS_SETFOREGROUND=0x200L");
    pp_define("DS_3DLOOK=0x0004L");
    pp_define("DS_FIXEDSYS=0x0008L");
    pp_define("DS_NOFAILCREATE=0x0010L");
    pp_define("DS_CONTROL=0x0400L");
    pp_define("DS_CENTER=0x0800L");
    pp_define("DS_CENTERMOUSE=0x1000L");
    pp_define("DS_CONTEXTHELP=0x2000L");
    pp_define("DS_SHELLFONT=DS_SETFONT|DS_FIXEDSYS");

    /* Define editbox styles */
    pp_define("ES_LEFT=0x0000L");
    pp_define("ES_CENTER=0x0001L");
    pp_define("ES_RIGHT=0x0002L");
    pp_define("ES_MULTILINE=0x0004L");
    pp_define("ES_UPPERCASE=0x0008L");
    pp_define("ES_LOWERCASE=0x0010L");
    pp_define("ES_PASSWORD=0x0020L");
    pp_define("ES_AUTOVSCROLL=0x0040L");
    pp_define("ES_AUTOHSCROLL=0x0080L");
    pp_define("ES_NOHIDESEL=0x0100L");
    pp_define("ES_OEMCONVERT=0x0400L");
    pp_define("ES_READONLY=0x0800L");
    pp_define("ES_WANTRETURN=0x1000L");
    pp_define("ES_NUMBER=0x2000L");

    /* Define button styles */
    pp_define("BS_PUSHBUTTON=0x00000000L");
    pp_define("BS_DEFPUSHBUTTON=0x00000001L");
    pp_define("BS_CHECKBOX=0x00000002L");
    pp_define("BS_AUTOCHECKBOX=0x00000003L");
    pp_define("BS_RADIOBUTTON=0x00000004L");
    pp_define("BS_3STATE=0x00000005L");
    pp_define("BS_AUTO3STATE=0x00000006L");
    pp_define("BS_GROUPBOX=0x00000007L");
    pp_define("BS_USERBUTTON=0x00000008L");
    pp_define("BS_AUTORADIOBUTTON=0x00000009L");
    pp_define("BS_OWNERDRAW=0x0000000BL");
    pp_define("BS_LEFTTEXT=0x00000020L");
    pp_define("BS_TEXT=0x00000000L");
    pp_define("BS_ICON=0x00000040L");
    pp_define("BS_BITMAP=0x00000080L");
    pp_define("BS_LEFT=0x00000100L");
    pp_define("BS_RIGHT=0x00000200L");
    pp_define("BS_CENTER=0x00000300L");
    pp_define("BS_TOP=0x00000400L");
    pp_define("BS_BOTTOM=0x00000800L");
    pp_define("BS_VCENTER=0x00000C00L");
    pp_define("BS_PUSHLIKE=0x00001000L");
    pp_define("BS_MULTILINE=0x00002000L");
    pp_define("BS_NOTIFY=0x00004000L");
    pp_define("BS_FLAT=0x00008000L");
    pp_define("BS_RIGHTBUTTON=BS_LEFTTEXT");

    /* Define static styles */
    pp_define("SS_LEFT=0x00000000L");
    pp_define("SS_CENTER=0x00000001L");
    pp_define("SS_RIGHT=0x00000002L");
    pp_define("SS_ICON=0x00000003L");
    pp_define("SS_BLACKRECT=0x00000004L");
    pp_define("SS_GRAYRECT=0x00000005L");
    pp_define("SS_WHITERECT=0x00000006L");
    pp_define("SS_BLACKFRAME=0x00000007L");
    pp_define("SS_GRAYFRAME=0x00000008L");
    pp_define("SS_WHITEFRAME=0x00000009L");
    pp_define("SS_USERITEM=0x0000000AL");
    pp_define("SS_SIMPLE=0x0000000BL");
    pp_define("SS_LEFTNOWORDWRAP=0x0000000CL");
    pp_define("SS_OWNERDRAW=0x0000000DL");
    pp_define("SS_BITMAP=0x0000000EL");
    pp_define("SS_ENHMETAFILE=0x0000000FL");
    pp_define("SS_ETCHEDHORZ=0x00000010L");
    pp_define("SS_ETCHEDVERT=0x00000011L");
    pp_define("SS_ETCHEDFRAME=0x00000012L");
    pp_define("SS_TYPEMASK=0x0000001FL");
    pp_define("SS_NOPREFIX=0x00000080L");
    pp_define("SS_NOTIFY=0x00000100L");
    pp_define("SS_CENTERIMAGE=0x00000200L");
    pp_define("SS_RIGHTJUST=0x00000400L");
    pp_define("SS_REALSIZEIMAGE=0x00000800L");
    pp_define("SS_SUNKEN=0x00001000L");
    pp_define("SS_ENDELLIPSIS=0x00004000L");
    pp_define("SS_PATHELLIPSIS=0x00008000L");
    pp_define("SS_WORDELLIPSIS=0x0000C000L");
    pp_define("SS_ELLIPSISMASK=0x0000C000L");

    /* Define listbox styles */
    pp_define("LBS_NOTIFY=0x0001L");
    pp_define("LBS_SORT=0x0002L");
    pp_define("LBS_NOREDRAW=0x0004L");
    pp_define("LBS_MULTIPLESEL=0x0008L");
    pp_define("LBS_OWNERDRAWFIXED=0x0010L");
    pp_define("LBS_OWNERDRAWVARIABLE=0x0020L");
    pp_define("LBS_HASSTRINGS=0x0040L");
    pp_define("LBS_USETABSTOPS=0x0080L");
    pp_define("LBS_NOINTEGRALHEIGHT=0x0100L");
    pp_define("LBS_MULTICOLUMN=0x0200L");
    pp_define("LBS_WANTKEYBOARDINPUT=0x0400L");
    pp_define("LBS_EXTENDEDSEL=0x0800L");
    pp_define("LBS_DISABLENOSCROLL=0x1000L");
    pp_define("LBS_NODATA=0x2000L");
    pp_define("LBS_NOSEL=0x4000L");
    pp_define("LBS_STANDARD=LBS_NOTIFY|LBS_SORT|WS_VSCROLL|WS_BORDER");

    /* Define combobox styles */
    pp_define("CBS_SIMPLE=0x0001L");
    pp_define("CBS_DROPDOWN=0x0002L");
    pp_define("CBS_DROPDOWNLIST=0x0003L");
    pp_define("CBS_OWNERDRAWFIXED=0x0010L");
    pp_define("CBS_OWNERDRAWVARIABLE=0x0020L");
    pp_define("CBS_AUTOHSCROLL=0x0040L");
    pp_define("CBS_OEMCONVERT=0x0080L");
    pp_define("CBS_SORT=0x0100L");
    pp_define("CBS_HASSTRINGS=0x0200L");
    pp_define("CBS_NOINTEGRALHEIGHT=0x0400L");
    pp_define("CBS_DISABLENOSCROLL=0x0800L");
    pp_define("CBS_UPPERCASE=0x2000L");
    pp_define("CBS_LOWERCASE=0x4000L");

    /* Define scrollbar styles */
    pp_define("SBS_HORZ=0x0000L");
    pp_define("SBS_VERT=0x0001L");
    pp_define("SBS_TOPALIGN=0x0002L");
    pp_define("SBS_LEFTALIGN=0x0002L");
    pp_define("SBS_BOTTOMALIGN=0x0004L");
    pp_define("SBS_RIGHTALIGN=0x0004L");
    pp_define("SBS_SIZEBOXTOPLEFTALIGN=0x0002L");
    pp_define("SBS_SIZEBOXBOTTOMRIGHTALIGN=0x0004L");
    pp_define("SBS_SIZEBOX=0x0008L");
    pp_define("SBS_SIZEGRIP=0x0010L");

    /* Define generic common control styles */
    pp_define("CCS_TOP=0x00000001L");
    pp_define("CCS_NOMOVEY=0x00000002L");
    pp_define("CCS_BOTTOM=0x00000003L");
    pp_define("CCS_NORESIZE=0x00000004L");
    pp_define("CCS_NOPARENTALIGN=0x00000008L");
    pp_define("CCS_ADJUSTABLE=0x00000020L");
    pp_define("CCS_NODIVIDER=0x00000040L");
    pp_define("CCS_VERT=0x00000080L");
    pp_define("CCS_LEFT=CCS_VERT|CCS_TOP");
    pp_define("CCS_RIGHT=CCS_VERT|CCS_BOTTOM");
    pp_define("CCS_NOMOVEX=CCS_VERT|CCS_NOMOVEY");

    /* Define header styles */
    pp_define("HDS_HORZ=0x0000");
    pp_define("HDS_BUTTONS=0x0002");
    pp_define("HDS_HOTTRACK=0x0004");
    pp_define("HDS_HIDDEN=0x0008");
    pp_define("HDS_DRAGDROP=0x0040");
    pp_define("HDS_FULLDRAG=0x0080");
    pp_define("HDS_FILTERBAR=0x0100");
    pp_define("HDS_FLAT=0x0200");

    /* Define toolbar styles */
    pp_define("TBSTYLE_BUTTON=0x0000");
    pp_define("TBSTYLE_SEP=0x0001");
    pp_define("TBSTYLE_CHECK=0x0002");
    pp_define("TBSTYLE_GROUP=0x0004");
    pp_define("TBSTYLE_CHECKGROUP=TBSTYLE_GROUP|TBSTYLE_CHECK");
    pp_define("TBSTYLE_DROPDOWN=0x0008");
    pp_define("TBSTYLE_AUTOSIZE=0x0010");
    pp_define("TBSTYLE_NOPREFIX=0x0020");
    pp_define("TBSTYLE_TOOLTIPS=0x0100");
    pp_define("TBSTYLE_WRAPABLE=0x0200");
    pp_define("TBSTYLE_ALTDRAG=0x0400");
    pp_define("TBSTYLE_FLAT=0x0800");
    pp_define("TBSTYLE_LIST=0x1000");
    pp_define("TBSTYLE_CUSTOMERASE=0x2000");
    pp_define("TBSTYLE_REGISTERDROP=0x4000");
    pp_define("TBSTYLE_TRANSPARENT=0x8000");

    /* Define rebar styles */
    pp_define("RBS_TOOLTIPS=0x0100");
    pp_define("RBS_VARHEIGHT=0x0200");
    pp_define("RBS_BANDBORDERS=0x0400");
    pp_define("RBS_FIXEDORDER=0x0800");
    pp_define("RBS_REGISTERDROP=0x1000");
    pp_define("RBS_AUTOSIZE=0x2000");
    pp_define("RBS_VERTICALGRIPPER=0x4000");
    pp_define("RBS_DBLCLKTOGGLE=0x8000");

    /* Define tooltip styles */
    pp_define("TTS_ALWAYSTIP=0x01");
    pp_define("TTS_NOPREFIX=0x02");
    pp_define("TTS_NOANIMATE=0x10");
    pp_define("TTS_NOFADE=0x20");
    pp_define("TTS_BALLOON=0x40");

    /* Define statusbar styles */
    pp_define("SBARS_SIZEGRIP=0x0100");
    pp_define("SBARS_TOOLTIPS=0x0800");
    pp_define("SBT_TOOLTIPS=0x0800");

    /* Define trackbar styles */
    pp_define("TBS_AUTOTICKS=0x0001");
    pp_define("TBS_VERT=0x0002");
    pp_define("TBS_HORZ=0x0000");
    pp_define("TBS_TOP=0x0004");
    pp_define("TBS_BOTTOM=0x0000");
    pp_define("TBS_LEFT=0x0004");
    pp_define("TBS_RIGHT=0x0000");
    pp_define("TBS_BOTH=0x0008");
    pp_define("TBS_NOTICKS=0x0010");
    pp_define("TBS_ENABLESELRANGE=0x0020");
    pp_define("TBS_FIXEDLENGTH=0x0040");
    pp_define("TBS_NOTHUMB=0x0080");
    pp_define("TBS_TOOLTIPS=0x0100");
    pp_define("TBS_REVERSED=0x0200");
    pp_define("TBS_DOWNISLEFT=0x0400");

    /* Define updown styles */
    pp_define("UDS_WRAP=0x0001");
    pp_define("UDS_SETBUDDYINT=0x0002");
    pp_define("UDS_ALIGNRIGHT=0x0004");
    pp_define("UDS_ALIGNLEFT=0x0008");
    pp_define("UDS_AUTOBUDDY=0x0010");
    pp_define("UDS_ARROWKEYS=0x0020");
    pp_define("UDS_HORZ=0x0040");
    pp_define("UDS_NOTHOUSANDS=0x0080");
    pp_define("UDS_HOTTRACK=0x0100");

    /* Define progress styles */
    pp_define("PBS_SMOOTH=0x01");
    pp_define("PBS_VERTICAL=0x04");
    pp_define("PBS_MARQUEE=0x08");

    /* Define listview styles */
    pp_define("LVS_ICON=0x0000");
    pp_define("LVS_REPORT=0x0001");
    pp_define("LVS_SMALLICON=0x0002");
    pp_define("LVS_LIST=0x0003");
    pp_define("LVS_TYPEMASK=0x0003");
    pp_define("LVS_SINGLESEL=0x0004");
    pp_define("LVS_SHOWSELALWAYS=0x0008");
    pp_define("LVS_SORTASCENDING=0x0010");
    pp_define("LVS_SORTDESCENDING=0x0020");
    pp_define("LVS_SHAREIMAGELISTS=0x0040");
    pp_define("LVS_NOLABELWRAP=0x0080");
    pp_define("LVS_AUTOARRANGE=0x0100");
    pp_define("LVS_EDITLABELS=0x0200");
    pp_define("LVS_OWNERDATA=0x1000");
    pp_define("LVS_NOSCROLL=0x2000");
    pp_define("LVS_ALIGNTOP=0x0000");
    pp_define("LVS_ALIGNLEFT=0x0800");
    pp_define("LVS_OWNERDRAWFIXED=0x0400");
    pp_define("LVS_NOCOLUMNHEADER=0x4000");
    pp_define("LVS_NOSORTHEADER=0x8000");

    /* Define treeview styles */
    pp_define("TVS_HASBUTTONS=0x0001");
    pp_define("TVS_HASLINES=0x0002");
    pp_define("TVS_LINESATROOT=0x0004");
    pp_define("TVS_EDITLABELS=0x0008");
    pp_define("TVS_DISABLEDRAGDROP=0x0010");
    pp_define("TVS_SHOWSELALWAYS=0x0020");
    pp_define("TVS_RTLREADING=0x0040");
    pp_define("TVS_NOTOOLTIPS=0x0080");
    pp_define("TVS_CHECKBOXES=0x0100");
    pp_define("TVS_TRACKSELECT=0x0200");
    pp_define("TVS_SINGLEEXPAND=0x0400");
    pp_define("TVS_INFOTIP=0x0800");
    pp_define("TVS_FULLROWSELECT=0x1000");
    pp_define("TVS_NOSCROLL=0x2000");
    pp_define("TVS_NONEVENHEIGHT=0x4000");
    pp_define("TVS_NOHSCROLL=0x8000");

    /* Define tab control styles */
    pp_define("TCS_SCROLLOPPOSITE=0x0001");
    pp_define("TCS_BOTTOM=0x0002");
    pp_define("TCS_RIGHT=0x0002");
    pp_define("TCS_MULTISELECT=0x0004");
    pp_define("TCS_FLATBUTTONS=0x0008");
    pp_define("TCS_FORCEICONLEFT=0x0010");
    pp_define("TCS_FORCELABELLEFT=0x0020");
    pp_define("TCS_HOTTRACK=0x0040");
    pp_define("TCS_VERTICAL=0x0080");
    pp_define("TCS_TABS=0x0000");
    pp_define("TCS_BUTTONS=0x0100");
    pp_define("TCS_SINGLELINE=0x0000");
    pp_define("TCS_MULTILINE=0x0200");
    pp_define("TCS_RIGHTJUSTIFY=0x0000");
    pp_define("TCS_FIXEDWIDTH=0x0400");
    pp_define("TCS_RAGGEDRIGHT=0x0800");
    pp_define("TCS_FOCUSONBUTTONDOWN=0x1000");
    pp_define("TCS_OWNERDRAWFIXED=0x2000");
    pp_define("TCS_TOOLTIPS=0x4000");
    pp_define("TCS_FOCUSNEVER=0x8000");

    /* Define animate styles */
    pp_define("ACS_CENTER=0x0001");
    pp_define("ACS_TRANSPARENT=0x0002");
    pp_define("ACS_AUTOPLAY=0x0004");
    pp_define("ACS_TIMER=0x0008");

    /* Define monthcal styles */
    pp_define("MCS_DAYSTATE=0x0001");
    pp_define("MCS_MULTISELECT=0x0002");
    pp_define("MCS_WEEKNUMBERS=0x0004");
    pp_define("MCS_NOTODAYCIRCLE=0x0008");
    pp_define("MCS_NOTODAY=0x0010");

    /* Define datetime styles */
    pp_define("DTS_UPDOWN=0x0001");
    pp_define("DTS_SHOWNONE=0x0002");
    pp_define("DTS_SHORTDATEFORMAT=0x0000");
    pp_define("DTS_LONGDATEFORMAT=0x0004");
    pp_define("DTS_SHORTDATECENTURYFORMAT=0x000C");
    pp_define("DTS_TIMEFORMAT=0x0009");
    pp_define("DTS_APPCANPARSE=0x0010");
    pp_define("DTS_RIGHTALIGN=0x0020");

    /* Define pager control styles */
    pp_define("PGS_VERT=0x00000000");
    pp_define("PGS_HORZ=0x00000001");
    pp_define("PGS_AUTOSCROLL=0x00000002");
    pp_define("PGS_DRAGNDROP=0x00000004");

    /* Define menu flags */
    pp_define("MF_INSERT=0x00000000L");
    pp_define("MF_CHANGE=0x00000080L");
    pp_define("MF_APPEND=0x00000100L");
    pp_define("MF_DELETE=0x00000200L");
    pp_define("MF_REMOVE=0x00001000L");
    pp_define("MF_BYCOMMAND=0x00000000L");
    pp_define("MF_BYPOSITION=0x00000400L");
    pp_define("MF_SEPARATOR=0x00000800L");
    pp_define("MF_ENABLED=0x00000000L");
    pp_define("MF_GRAYED=0x00000001L");
    pp_define("MF_DISABLED=0x00000002L");
    pp_define("MF_UNCHECKED=0x00000000L");
    pp_define("MF_CHECKED=0x00000008L");
    pp_define("MF_USECHECKBITMAPS=0x00000200L");
    pp_define("MF_STRING=0x00000000L");
    pp_define("MF_BITMAP=0x00000004L");
    pp_define("MF_OWNERDRAW=0x00000100L");
    pp_define("MF_POPUP=0x00000010L");
    pp_define("MF_MENUBARBREAK=0x00000020L");
    pp_define("MF_MENUBREAK=0x00000040L");
    pp_define("MF_UNHILITE=0x00000000L");
    pp_define("MF_HILITE=0x00000080L");
    pp_define("MF_DEFAULT=0x00001000L");
    pp_define("MF_SYSMENU=0x00002000L");
    pp_define("MF_HELP=0x00004000L");
    pp_define("MF_RIGHTJUSTIFY=0x00004000L");
    pp_define("MF_MOUSESELECT=0x00008000L");
    pp_define("MF_END=0x00000080L");

    /* Define menu types */
    pp_define("MFT_STRING=MF_STRING");
    pp_define("MFT_BITMAP=MF_BITMAP");
    pp_define("MFT_MENUBARBREAK=MF_MENUBARBREAK");
    pp_define("MFT_MENUBREAK=MF_MENUBREAK");
    pp_define("MFT_OWNERDRAW=MF_OWNERDRAW");
    pp_define("MFT_RADIOCHECK=0x00000200L");
    pp_define("MFT_SEPARATOR=MF_SEPARATOR");
    pp_define("MFT_RIGHTORDER=0x00002000L");
    pp_define("MFT_RIGHTJUSTIFY=MF_RIGHTJUSTIFY");

    /* Define menu states */
    pp_define("MFS_GRAYED=0x00000003L");
    pp_define("MFS_DISABLED=MFS_GRAYED");
    pp_define("MFS_CHECKED=MF_CHECKED");
    pp_define("MFS_HILITE=MF_HILITE");
    pp_define("MFS_ENABLED=MF_ENABLED");
    pp_define("MFS_UNCHECKED=MF_UNCHECKED");
    pp_define("MFS_UNHILITE=MF_UNHILITE");
    pp_define("MFS_DEFAULT=MF_DEFAULT");

    /* Define version resource flags */
    pp_define("VS_VERSION_INFO=1");
    pp_define("VS_FF_DEBUG=0x00000001");
    pp_define("VS_FF_PRERELEASE=0x00000002");
    pp_define("VS_FF_PATCHED=0x00000004");
    pp_define("VS_FF_PRIVATEBUILD=0x00000008");
    pp_define("VS_FF_INFOINFERRED=0x00000010");
    pp_define("VS_FF_SPECIALBUILD=0x00000020");
    pp_define("VOS_UNKNOWN=0x00000000");
    pp_define("VOS_DOS=0x00010000");
    pp_define("VOS_NT=0x00040000");
    pp_define("VOS__WINDOWS16=0x00000001");
    pp_define("VOS__WINDOWS32=0x00000004");
    pp_define("VOS_DOS_WINDOWS16=0x00010001");
    pp_define("VOS_DOS_WINDOWS32=0x00010004");
    pp_define("VOS_NT_WINDOWS32=0x00040004");
    pp_define("VFT_UNKNOWN=0");
    pp_define("VFT_APP=1");
    pp_define("VFT_DLL=2");
    pp_define("VFT_DRV=3");
    pp_define("VFT_FONT=4");
    pp_define("VFT_VXD=5");
    pp_define("VFT_STATIC_LIB=7");
    pp_define("VFT2_UNKNOWN=0");
    pp_define("VFT2_DRV_PRINTER=1");
    pp_define("VFT2_DRV_KEYBOARD=2");
    pp_define("VFT2_DRV_LANGUAGE=3");
    pp_define("VFT2_DRV_DISPLAY=4");
    pp_define("VFT2_DRV_MOUSE=5");
    pp_define("VFT2_DRV_NETWORK=6");
    pp_define("VFT2_DRV_SYSTEM=7");
    pp_define("VFT2_DRV_INSTALLABLE=8");
    pp_define("VFT2_DRV_SOUND=9");
    pp_define("VFT2_DRV_COMM=10");
    pp_define("VFT2_DRV_INPUTMETHOD=11");
    pp_define("VFT2_FONT_RASTER=1");
    pp_define("VFT2_FONT_VECTOR=2");
    pp_define("VFT2_FONT_TRUETYPE=3");

    /* Define ID's */
    pp_define("IDOK=1");
    pp_define("IDCANCEL=2");
    pp_define("IDABORT=3");
    pp_define("IDRETRY=4");
    pp_define("IDIGNORE=5");
    pp_define("IDYES=6");
    pp_define("IDNO=7");
    pp_define("IDCLOSE=8");
    pp_define("IDHELP=9");

    /* Languages from winnt.h */
    pp_define("LANG_NEUTRAL=0x00");

    pp_define("LANG_AFRIKAANS=0x36");
    pp_define("LANG_ALBANIAN=0x1c");
    pp_define("LANG_ARABIC=0x01");
    pp_define("LANG_ARMENIAN=0x2b");
    pp_define("LANG_ASSAMESE=0x4d");
    pp_define("LANG_AZERI=0x2c");
    pp_define("LANG_BASQUE=0x2d");
    pp_define("LANG_BELARUSIAN=0x23");
    pp_define("LANG_BENGALI=0x45");
    pp_define("LANG_BULGARIAN=0x02");
    pp_define("LANG_CATALAN=0x03");
    pp_define("LANG_CHINESE=0x04");
    pp_define("LANG_CROATIAN=0x1a");
    pp_define("LANG_CZECH=0x05");
    pp_define("LANG_DANISH=0x06");
    pp_define("LANG_DUTCH=0x13");
    pp_define("LANG_ENGLISH=0x09");
    pp_define("LANG_ESTONIAN=0x25");
    pp_define("LANG_FAEROESE=0x38");
    pp_define("LANG_FARSI=0x29");
    pp_define("LANG_FINNISH=0x0b");
    pp_define("LANG_FRENCH=0x0c");
    pp_define("LANG_GEORGIAN=0x37");
    pp_define("LANG_GERMAN=0x07");
    pp_define("LANG_GREEK=0x08");
    pp_define("LANG_GUJARATI=0x47");
    pp_define("LANG_HEBREW=0x0d");
    pp_define("LANG_HINDI=0x39");
    pp_define("LANG_HUNGARIAN=0x0e");
    pp_define("LANG_ICELANDIC=0x0f");
    pp_define("LANG_INDONESIAN=0x21");
    pp_define("LANG_ITALIAN=0x10");
    pp_define("LANG_JAPANESE=0x11");
    pp_define("LANG_KANNADA=0x4b");
    pp_define("LANG_KASHMIRI=0x60");
    pp_define("LANG_KAZAK=0x3f");
    pp_define("LANG_KONKANI=0x57");
    pp_define("LANG_KOREAN=0x12");
    pp_define("LANG_LATVIAN=0x26");
    pp_define("LANG_LITHUANIAN=0x27");
    pp_define("LANG_MACEDONIAN=0x2f");
    pp_define("LANG_MALAY=0x3e");
    pp_define("LANG_MALAYALAM=0x4c");
    pp_define("LANG_MANIPURI=0x58");
    pp_define("LANG_MARATHI=0x4e");
    pp_define("LANG_NEPALI=0x61");
    pp_define("LANG_NORWEGIAN=0x14");
    pp_define("LANG_ORIYA=0x48");
    pp_define("LANG_POLISH=0x15");
    pp_define("LANG_PORTUGUESE=0x16");
    pp_define("LANG_PUNJABI=0x46");
    pp_define("LANG_ROMANIAN=0x18");
    pp_define("LANG_RUSSIAN=0x19");
    pp_define("LANG_SANSKRIT=0x4f");
    pp_define("LANG_SERBIAN=0x1a");
    pp_define("LANG_SINDHI=0x59");
    pp_define("LANG_SLOVAK=0x1b");
    pp_define("LANG_SLOVENIAN=0x24");
    pp_define("LANG_SPANISH=0x0a");
    pp_define("LANG_SWAHILI=0x41");
    pp_define("LANG_SWEDISH=0x1d");
    pp_define("LANG_TAMIL=0x49");
    pp_define("LANG_TATAR=0x44");
    pp_define("LANG_TELUGU=0x4a");
    pp_define("LANG_THAI=0x1e");
    pp_define("LANG_TURKISH=0x1f");
    pp_define("LANG_UKRAINIAN=0x22");
    pp_define("LANG_URDU=0x20");
    pp_define("LANG_UZBEK=0x43");
    pp_define("LANG_VIETNAMESE=0x2a");

    pp_define("SUBLANG_NEUTRAL=0x00");
    pp_define("SUBLANG_DEFAULT=0x01");
    pp_define("SUBLANG_SYS_DEFAULT=0x02");

    pp_define("SUBLANG_ARABIC_SAUDI_ARABIA=0x01");
    pp_define("SUBLANG_ARABIC_IRAQ=0x02");
    pp_define("SUBLANG_ARABIC_EGYPT=0x03");
    pp_define("SUBLANG_ARABIC_LIBYA=0x04");
    pp_define("SUBLANG_ARABIC_ALGERIA=0x05");
    pp_define("SUBLANG_ARABIC_MOROCCO=0x06");
    pp_define("SUBLANG_ARABIC_TUNISIA=0x07");
    pp_define("SUBLANG_ARABIC_OMAN=0x08");
    pp_define("SUBLANG_ARABIC_YEMEN=0x09");
    pp_define("SUBLANG_ARABIC_SYRIA=0x0a");
    pp_define("SUBLANG_ARABIC_JORDAN=0x0b");
    pp_define("SUBLANG_ARABIC_LEBANON=0x0c");
    pp_define("SUBLANG_ARABIC_KUWAIT=0x0d");
    pp_define("SUBLANG_ARABIC_UAE=0x0e");
    pp_define("SUBLANG_ARABIC_BAHRAIN=0x0f");
    pp_define("SUBLANG_ARABIC_QATAR=0x10");
    pp_define("SUBLANG_AZERI_LATIN=0x01");
    pp_define("SUBLANG_AZERI_CYRILLIC=0x02");
    pp_define("SUBLANG_CHINESE_TRADITIONAL=0x01");
    pp_define("SUBLANG_CHINESE_SIMPLIFIED=0x02");
    pp_define("SUBLANG_CHINESE_HONGKONG=0x03");
    pp_define("SUBLANG_CHINESE_SINGAPORE=0x04");
    pp_define("SUBLANG_CHINESE_MACAU=0x05");
    pp_define("SUBLANG_DUTCH=0x01");
    pp_define("SUBLANG_DUTCH_BELGIAN=0x02");
    pp_define("SUBLANG_ENGLISH_US=0x01");
    pp_define("SUBLANG_ENGLISH_UK=0x02");
    pp_define("SUBLANG_ENGLISH_AUS=0x03");
    pp_define("SUBLANG_ENGLISH_CAN=0x04");
    pp_define("SUBLANG_ENGLISH_NZ=0x05");
    pp_define("SUBLANG_ENGLISH_EIRE=0x06");
    pp_define("SUBLANG_ENGLISH_SOUTH_AFRICA=0x07");
    pp_define("SUBLANG_ENGLISH_JAMAICA=0x08");
    pp_define("SUBLANG_ENGLISH_CARIBBEAN=0x09");
    pp_define("SUBLANG_ENGLISH_BELIZE=0x0a");
    pp_define("SUBLANG_ENGLISH_TRINIDAD=0x0b");
    pp_define("SUBLANG_ENGLISH_ZIMBABWE=0x0c");
    pp_define("SUBLANG_ENGLISH_PHILIPPINES=0x0d");
    pp_define("SUBLANG_FRENCH=0x01");
    pp_define("SUBLANG_FRENCH_BELGIAN=0x02");
    pp_define("SUBLANG_FRENCH_CANADIAN=0x03");
    pp_define("SUBLANG_FRENCH_SWISS=0x04");
    pp_define("SUBLANG_FRENCH_LUXEMBOURG=0x05");
    pp_define("SUBLANG_FRENCH_MONACO=0x06");
    pp_define("SUBLANG_GERMAN=0x01");
    pp_define("SUBLANG_GERMAN_SWISS=0x02");
    pp_define("SUBLANG_GERMAN_AUSTRIAN=0x03");
    pp_define("SUBLANG_GERMAN_LUXEMBOURG=0x04");
    pp_define("SUBLANG_GERMAN_LIECHTENSTEIN=0x05");
    pp_define("SUBLANG_ITALIAN=0x01");
    pp_define("SUBLANG_ITALIAN_SWISS=0x02");
    pp_define("SUBLANG_KASHMIRI_INDIA=0x02");
    pp_define("SUBLANG_KOREAN=0x01");
    pp_define("SUBLANG_LITHUANIAN=0x01");
    pp_define("SUBLANG_MALAY_MALAYSIA=0x01");
    pp_define("SUBLANG_MALAY_BRUNEI_DARUSSALAM=0x02");
    pp_define("SUBLANG_NEPALI_INDIA=0x02");
    pp_define("SUBLANG_NORWEGIAN_BOKMAL=0x01");
    pp_define("SUBLANG_NORWEGIAN_NYNORSK=0x02");
    pp_define("SUBLANG_PORTUGUESE=0x02");
    pp_define("SUBLANG_PORTUGUESE_BRAZILIAN=0x01");
    pp_define("SUBLANG_SERBIAN_LATIN=0x02");
    pp_define("SUBLANG_SERBIAN_CYRILLIC=0x03");
    pp_define("SUBLANG_SPANISH=0x01");
    pp_define("SUBLANG_SPANISH_MEXICAN=0x02");
    pp_define("SUBLANG_SPANISH_MODERN=0x03");
    pp_define("SUBLANG_SPANISH_GUATEMALA=0x04");
    pp_define("SUBLANG_SPANISH_COSTA_RICA=0x05");
    pp_define("SUBLANG_SPANISH_PANAMA=0x06");
    pp_define("SUBLANG_SPANISH_DOMINICAN_REPUBLIC=0x07");
    pp_define("SUBLANG_SPANISH_VENEZUELA=0x08");
    pp_define("SUBLANG_SPANISH_COLOMBIA=0x09");
    pp_define("SUBLANG_SPANISH_PERU=0x0a");
    pp_define("SUBLANG_SPANISH_ARGENTINA=0x0b");
    pp_define("SUBLANG_SPANISH_ECUADOR=0x0c");
    pp_define("SUBLANG_SPANISH_CHILE=0x0d");
    pp_define("SUBLANG_SPANISH_URUGUAY=0x0e");
    pp_define("SUBLANG_SPANISH_PARAGUAY=0x0f");
    pp_define("SUBLANG_SPANISH_BOLIVIA=0x10");
    pp_define("SUBLANG_SPANISH_EL_SALVADOR=0x11");
    pp_define("SUBLANG_SPANISH_HONDURAS=0x12");
    pp_define("SUBLANG_SPANISH_NICARAGUA=0x13");
    pp_define("SUBLANG_SPANISH_PUERTO_RICO=0x14");
    pp_define("SUBLANG_SWEDISH=0x01");
    pp_define("SUBLANG_SWEDISH_FINLAND=0x02");
    pp_define("SUBLANG_URDU_PAKISTAN=0x01");
    pp_define("SUBLANG_URDU_INDIA=0x02");
    pp_define("SUBLANG_UZBEK_LATIN=0x01");
    pp_define("SUBLANG_UZBEK_CYRILLIC=0x02");

    pp_define("SORT_DEFAULT=0x0");

    pp_define("SORT_JAPANESE_XJIS=0x0");
    pp_define("SORT_JAPANESE_UNICODE=0x1");

    pp_define("SORT_CHINESE_BIG5=0x0");
    pp_define("SORT_CHINESE_PRCP=0x0");
    pp_define("SORT_CHINESE_UNICODE=0x1");
    pp_define("SORT_CHINESE_PRC=0x2");
    pp_define("SORT_CHINESE_BOPOMOFO=0x3");

    pp_define("SORT_KOREAN_KSC=0x0");
    pp_define("SORT_KOREAN_UNICODE=0x1");

    pp_define("SORT_GERMAN_PHONE_BOOK=0x1");

    pp_define("SORT_HUNGARIAN_DEFAULT=0x0");
    pp_define("SORT_HUNGARIAN_TECHNICAL=0x1");

    pp_define("SORT_GEORGIAN_TRADITIONAL=0x0");
    pp_define("SORT_GEORGIAN_MODERN=0x1");
}

/****************************************************************************
 *                                                                          *
 * Function: pp_define                                                      *
 *                                                                          *
 * Purpose : Define a preprocessor symbol (predefined / command line).      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

void pp_define(const char *s)
{
    TOKENROW tr;

    set_source("<cmdarg>", NULL, s);
    make_tokenrow(3, &tr);
    get_tokens(&tr, TRUE);
    doadefine(&tr);
    unset_source();
}

/****************************************************************************
 *                                                                          *
 * Function: pp_start                                                       *
 *                                                                          *
 * Purpose : Start processing preprocessor input.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

void pp_start(const char *filename)
{
    time_t tm;
    HANDLE hf;
    WINERR err;

    tm = time(NULL);
    curtime = ctime(&tm);

    make_tokenrow(3, &maintr);

    err = my_openfile(filename, &hf);
    if (err) pp_error(RCFATAL(MYOPENERROR(err)), filename);

    set_source((char *)newstring((uchar_t *)filename, strlen(filename), 0), hf, NULL);

    setup_hideset();

    genline();  /* generate #line */
}

/****************************************************************************
 *                                                                          *
 * Function: process                                                        *
 *                                                                          *
 * Purpose : Process the preprocessor input.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t pp_process(TOKENROW *trp)
{
    static int anymacros = 0;
    bool_t readmore = TRUE;
    bool_t eof = FALSE;

    while (readmore)
    {
        if (trp->tp >= trp->lp)
        {
            trp->tp = trp->lp = trp->bp;
            outpp = outbuf;
            anymacros |= get_tokens(trp, TRUE);
            trp->tp = trp->bp;
        }

        if (trp->tp->type == END)
        {
            if (--incdepth >= 0)
            {
                if (cursource->ifdepth != 0)
                {
                    pp_error(RCWARNING2(ERROR_UNTERM_INCLUDE_COND));
                    do
                        if (--ifdepth < skipping) skipping = 0;
                    while (--cursource->ifdepth);
                }

                unset_source();
                cursource->line += cursource->lineinc;
                trp->tp = trp->lp;
                genline();
                continue;
            }

            if (ifdepth != 0)
                pp_error(RCERROR(ERROR_UNTERM_IF_DIRECT));

            eof = TRUE;
            break;
        }

        if (trp->tp->type == SHARP)
        {
            trp->tp += 1;
            control(trp);
        }
        else if (!skipping && anymacros)
            expandrow(trp, NULL);

        if (skipping)
            emptyrow(trp);

        readmore = put_tokens(trp);
        anymacros = 0;

        cursource->line += cursource->lineinc;
        if (cursource->lineinc > 1)
            readmore = genline();
    }

    return eof;
}

/****************************************************************************
 *                                                                          *
 * Function: control                                                        *
 *                                                                          *
 * Purpose : Handle preprocessor directives.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void control(TOKENROW *trp)
{
    NLIST *np;
    TOKEN *tp;

    tp = trp->tp;
    if (tp->type != NAME)
    {
        if (tp->type == NUMBER)
            goto kline;

        if (tp->type != NL)
            pp_error(RCERROR(ERROR_INVALID_CONTROL_LINE));

        return;  /* else empty line */
    }

    np = pp_lookup(tp, FALSE);
    if (np == NULL || (np->flag & ISKW) == 0 && !skipping)
    {
        pp_error(RCWARNING1(ERROR_UNKNOWN_PP_CONTROL), tp);
        return;
    }

    if (skipping)
    {
        switch (np->val)
        {
            case KENDIF:
                if (--ifdepth < skipping)
                    skipping = 0;
                --cursource->ifdepth;
                emptyrow(trp);
                return;

            case KIFDEF:
            case KIFNDEF:
            case KIF:
                if (++ifdepth >= NIF)
                    pp_error(RCFATAL(ERROR_IF_TOO_DEEPLY_NESTED));
                ++cursource->ifdepth;
                return;

            case KELIF:
            case KELSE:
                if (ifdepth <= skipping)
                    break;
                return;

            default:
                return;
        }
    }

    switch (np->val)
    {
        case KDEFINE:
            define(trp);
            break;

        case KUNDEF:
            tp += 1;
            if (tp->type != NAME || trp->lp - trp->bp != 4)
            {
                pp_error(RCERROR(ERROR_UNDEF_SYNTAX_ERROR));
                break;
            }
            if ((np = pp_lookup(tp, FALSE)) != NULL)
                np->flag &= ~ISDEFINED;
            break;

        case KPRAGMA:
            /* return; */
            break;

        case KIFDEF:
        case KIFNDEF:
        case KIF:
            if (++ifdepth >= NIF)
                pp_error(RCFATAL(ERROR_IF_TOO_DEEPLY_NESTED));

            ++cursource->ifdepth;
            ifsatisfied[ifdepth] = 0;

            if (eval(trp, np->val))
                ifsatisfied[ifdepth] = 1;
            else
                skipping = ifdepth;
            break;

        case KELIF:
            if (ifdepth == 0)
            {
                pp_error(RCERROR(ERROR_ELIF_WITHOUT_IF));
                return;
            }

            if (ifsatisfied[ifdepth] == 2)
                pp_error(RCERROR(ERROR_ELIF_AFTER_ELSE));

            if (eval(trp, np->val))
            {
                if (ifsatisfied[ifdepth])
                    skipping = ifdepth;
                else
                {
                    skipping = 0;
                    ifsatisfied[ifdepth] = 1;
                }
            }
            else
            {
                skipping = ifdepth;
            }
            break;

        case KELSE:
            if (ifdepth == 0 || cursource->ifdepth == 0)
            {
                pp_error(RCERROR(ERROR_ELSE_WITHOUT_IF));
                return;
            }

            if (ifsatisfied[ifdepth] == 2)
                pp_error(RCERROR(ERROR_ELSE_AFTER_ELSE));

            if (trp->lp - trp->bp != 3)
                pp_error(RCERROR(ERROR_ELSE_SYNTAX_ERROR));

            skipping = ifsatisfied[ifdepth] ? ifdepth : 0;
            ifsatisfied[ifdepth] = 2;
            break;

        case KENDIF:
            if (ifdepth == 0 || cursource->ifdepth == 0)
            {
                pp_error(RCERROR(ERROR_ENDIF_WITHOUT_IF));
                return;
            }

            --ifdepth;
            --cursource->ifdepth;

            if (trp->lp - trp->bp != 3)
                pp_error(RCWARNING1(ERROR_ENDIF_SYNTAX_ERROR));
            break;

        case KERROR:
            trp->tp = tp+1;
            pp_error(RCERROR(ERROR_ERROR_DIRECT), trp);
            break;

        case KLINE:
            trp->tp = tp+1;
            expandrow(trp, "<line>");
            tp = trp->bp+2;
kline:
            if (tp+1 >= trp->lp || tp->type != NUMBER || tp+3 < trp->lp ||
               (tp+3 == trp->lp && ((tp+1)->type != STRING) ||
                *(tp+1)->t == 'L'))
            {
                pp_error(RCERROR(ERROR_LINE_SYNTAX_ERROR));
                return;
            }

            cursource->line = atol((char *)tp->t)-1;
            if (cursource->line < 0 || cursource->line > 32767)
                pp_error(RCWARNING1(ERROR_LINE_NUMBER_OUT_OF_RANGE));

            tp = tp+1;
            if (tp+1 < trp->lp)
                cursource->filename = (char *)newstring(tp->t+1, tp->len-2, 0);
            return;

        case KDEFINED:
            pp_error(RCERROR(ERROR_CONTROL_SYNTAX_ERROR));
            break;

        case KINCLUDE:
            include(trp);
            trp->lp = trp->bp;
            return;

        case KEVAL:
            eval(trp, np->val);
            break;

        default:
            pp_error(RCFATAL(ERROR_CONTROL_NOT_IMPLEMENTED), tp);
            break;
    }

    emptyrow(trp);
    return;
}

/****************************************************************************
 *                                                                          *
 * Function: pp_error                                                       *
 *                                                                          *
 * Purpose : Display preprocessor error message.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-09-27  Created                                              *
 *           01-11-09  Rewritten when compiler were converted to DLL.       *
 *           02-05-09  Bugfix: must terminate the string (for %t and %r).   *
 *           02-06-17  Rewritten to support resource messages.              *
 *                                                                          *
 ****************************************************************************/

void pp_error(ulong_t err, ...)
{
    if (error_callback != 0)
    {
        enum errclass class = 0;
        char tmp[256];
        char buf[512], *bp;
        va_list ap;
        char *cp;
        const char *ep;
        TOKEN *tp;
        TOKENROW *trp;
        SOURCE *s;
        int i;

        if (ISFATAL(err)) class = ERRCLASS_FATAL;
        if (ISERROR(err)) class = ERRCLASS_ERROR;
        if (ISWARNING(err)) class = ERRCLASS_WARNING;

        if (!FormatMessage(FORMAT_MESSAGE_FROM_HMODULE|FORMAT_MESSAGE_IGNORE_INSERTS, hmod,
            err & ~(ERROR_SEVERITY_FATAL|ERROR_SEVERITY_ERROR|ERROR_SEVERITY_WARNING),
            0, tmp, NELEMS(tmp), NULL))
        {
            sprintf(tmp, "*** No message for error 0x%X ***", err);
        }

        va_start(ap, err);
        bp = buf;
        for (ep = tmp; *ep; ep++)
        {
            if (*ep == '%')
            {
                if (ep[1] == '%')
                    ep++;

                switch (*++ep)
                {
                    case 'c':
                        i = va_arg(ap, char);
                        bp += sprintf(bp, "%c", i);
                        break;

                    case '1':  /* MESSAGETABLE specifier */
                    case 's':
                        cp = va_arg(ap, char *);
                        bp += sprintf(bp, "%s", cp);
                        break;

                    case 'd':
                        i = va_arg(ap, int);
                        bp += sprintf(bp, "%d", i);
                        break;

                    case 't':
                        tp = va_arg(ap, TOKEN *);
                        bp += sprintf(bp, "%.*s", tp->len, tp->t);
                        break;

                    case 'r':
                        trp = va_arg(ap, TOKENROW *);
                        for (tp = trp->tp; tp < trp->lp && tp->type != NL; tp++)
                        {
                            if (tp > trp->tp && tp->wslen)
                                *bp++ = ' ';
                            bp += sprintf(bp, "%.*s", tp->len, tp->t);
                        }
                        break;

                    default:
                        *bp++ = *ep;
                        break;
                }
            }
            else
            {
                *bp++ = *ep;
            }
        }
        *bp = '\0';
        va_end(ap);

        /* Find the most recent "real" file name */
        for (s = cursource; s && s->hf == NULL; s = s->next)
            ;

        error_callback(class, (s) ? s->filename : NULL, (s) ? s->line : 0, buf);

        if (ISFATAL(err)) longjmp(jumpbuf, 1);
        if (ISERROR(err)) nerrs++;
    }
}

