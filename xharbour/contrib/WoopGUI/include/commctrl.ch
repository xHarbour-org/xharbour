#define CLR_NONE                0xFFFFFFFFL
#define CLR_DEFAULT             0xFF000000L

//#define CCM_FIRST               8192               // 0x2000      // Common control shared messages
//#define CCM_LAST                (CCM_FIRST + 512) // 0x200)
//#define CCM_SETBKCOLOR          (CCM_FIRST + 1) // lParam is bkColor


/*                                                                             
 * ToolTips Flags                                                          
 */                                                                            
#define TTS_ALWAYSTIP              1 // 0x01
#define TTS_NOPREFIX               2 // 0x02
#define TTS_NOANIMATE             16 // 0x10
#define TTS_NOFADE                32 // 0x20
#define TTS_BALLOON               64 // 0x40

//#define WM_USER                 1024 // 0x0400


//====== STATUS BAR CONTROL ===================================================

//#define SBARS_SIZEGRIP           256 // 0x0100
//#define SBT_TOOLTIPS            2048 // 0x0800
//#define SBARS_TOOLTIPS          SBT_TOOLTIPS
//
//#define STATUSCLASSNAME         "msctls_statusbar32"
//
///*                                                                             
// * StatusBar Flags                                                          
// */                                                                            
//
//#define SB_SETPARTS             (WM_USER+4)
//#define SB_GETPARTS             (WM_USER+6)
//#define SB_GETBORDERS           (WM_USER+7)
//#define SB_SETMINHEIGHT         (WM_USER+8)
//#define SB_SIMPLE               (WM_USER+9)
//#define SB_GETRECT              (WM_USER+10)
//#define SB_ISSIMPLE             (WM_USER+14)
//#define SB_SETICON              (WM_USER+15)
//#define SB_SETTIPTEXTA          (WM_USER+16)
//#define SB_SETTIPTEXTW          (WM_USER+17)
//#define SB_GETTIPTEXTA          (WM_USER+18)
//#define SB_GETTIPTEXTW          (WM_USER+19)
//#define SB_GETICON              (WM_USER+20)
//#define SBT_OWNERDRAW                 4096     // 0x1000
//#define SBT_NOBORDERS                  256     // 0x0100
//#define SBT_POPOUT                     512     // 0x0200
//#define SBT_RTLREADING                1024     // 0x0400
//#define SB_SETBKCOLOR           CCM_SETBKCOLOR      // lParam = bkColor

//====== STATUS BAR CONTROL ===================================================

// begin_r_commctrl

#define SBARS_SIZEGRIP          0x0100
#define SBARS_TOOLTIPS          0x0800
// this is a status bar flag, preference to SBARS_TOOLTIPS
#define SBT_TOOLTIPS            0x0800

// end_r_commctrl

#define STATUSCLASSNAME         "msctls_statusbar32"

#define SB_SETTEXTA             (WM_USER+1)
#define SB_SETTEXTW             (WM_USER+11)
#define SB_GETTEXTA             (WM_USER+2)
#define SB_GETTEXTW             (WM_USER+13)
#define SB_GETTEXTLENGTHA       (WM_USER+3)
#define SB_GETTEXTLENGTHW       (WM_USER+12)

#define SB_GETTEXT              SB_GETTEXTA
#define SB_SETTEXT              SB_SETTEXTA
#define SB_GETTEXTLENGTH        SB_GETTEXTLENGTHA
#define SB_SETTIPTEXT           SB_SETTIPTEXTA
#define SB_GETTIPTEXT           SB_GETTIPTEXTA


#define SB_SETPARTS             (WM_USER+4)
#define SB_GETPARTS             (WM_USER+6)
#define SB_GETBORDERS           (WM_USER+7)
#define SB_SETMINHEIGHT         (WM_USER+8)
#define SB_SIMPLE               (WM_USER+9)
#define SB_GETRECT              (WM_USER+10)
#define SB_ISSIMPLE             (WM_USER+14)
#define SB_SETICON              (WM_USER+15)
#define SB_SETTIPTEXTA          (WM_USER+16)
#define SB_SETTIPTEXTW          (WM_USER+17)
#define SB_GETTIPTEXTA          (WM_USER+18)
#define SB_GETTIPTEXTW          (WM_USER+19)
#define SB_GETICON              (WM_USER+20)
#define SB_SETUNICODEFORMAT     CCM_SETUNICODEFORMAT
#define SB_GETUNICODEFORMAT     CCM_GETUNICODEFORMAT

#define SBT_OWNERDRAW            0x1000
#define SBT_NOBORDERS            0x0100
#define SBT_POPOUT               0x0200
#define SBT_RTLREADING           0x0400
#define SBT_NOTABPARSING         0x0800

#define SB_SETBKCOLOR           CCM_SETBKCOLOR      // lParam = bkColor

/// status bar notifications
#define SBN_SIMPLEMODECHANGE    (SBN_FIRST - 0)

// refers to the data saved for simple mode
#define SB_SIMPLEID  0x00ff


//------------------------------------------------------------------------------------

#define ICC_LISTVIEW_CLASSES 0x00000001 // listview, header
#define ICC_TREEVIEW_CLASSES 0x00000002 // treeview, tooltips
#define ICC_BAR_CLASSES      0x00000004 // toolbar, statusbar, trackbar, tooltips
#define ICC_TAB_CLASSES      0x00000008 // tab, tooltips
#define ICC_UPDOWN_CLASS     0x00000010 // updown
#define ICC_PROGRESS_CLASS   0x00000020 // progress
#define ICC_HOTKEY_CLASS     0x00000040 // hotkey
#define ICC_ANIMATE_CLASS    0x00000080 // animate
#define ICC_WIN95_CLASSES    0x000000FF
#define ICC_DATE_CLASSES     0x00000100 // month picker, date picker, time picker, updown
#define ICC_USEREX_CLASSES   0x00000200 // comboex
#define ICC_COOL_CLASSES     0x00000400 // rebar (coolbar) control
//#if (_WIN32_IE >= 0x0400)
#define ICC_INTERNET_CLASSES 0x00000800
#define ICC_PAGESCROLLER_CLASS 0x00001000   // page scroller
#define ICC_NATIVEFNTCTL_CLASS 0x00002000   // native font control
//#endif

#define ODT_HEADER              100
#define ODT_TAB                 101
#define ODT_LISTVIEW            102


//====== Ranges for control message IDs =======================================

#define LVM_FIRST               0x1000      // ListView messages
#define TV_FIRST                0x1100      // TreeView messages
#define HDM_FIRST               0x1200      // Header messages
#define TCM_FIRST               0x1300      // Tab control messages

//#if (_WIN32_IE >= 0x0400)
#define PGM_FIRST               0x1400      // Pager control messages
#define CCM_FIRST               0x2000      // Common control shared messages
#define CCM_LAST                (CCM_FIRST + 0x200)


#define CCM_SETBKCOLOR          (CCM_FIRST + 1) // lParam is bkColor

#define CCM_SETCOLORSCHEME      (CCM_FIRST + 2) // lParam is color scheme
#define CCM_GETCOLORSCHEME      (CCM_FIRST + 3) // fills in COLORSCHEME pointed to by lParam
#define CCM_GETDROPTARGET       (CCM_FIRST + 4)
#define CCM_SETUNICODEFORMAT    (CCM_FIRST + 5)
#define CCM_GETUNICODEFORMAT    (CCM_FIRST + 6)

//#if (_WIN32_IE >= 0x0500)
#define COMCTL32_VERSION  5
#define CCM_SETVERSION          (CCM_FIRST + 0x7)
#define CCM_GETVERSION          (CCM_FIRST + 0x8)
#define CCM_SETNOTIFYWINDOW     (CCM_FIRST + 0x9) // wParam == hwndParent.
//#endif // (_WIN32_IE >= 0x0500)

//#endif // (_WIN32_IE >= 0x0400)

//#if (_WIN32_IE >= 0x0400)
// for tooltips
#define INFOTIPSIZE 1024
//#endif

//====== WM_NOTIFY Macros =====================================================

//#define HANDLE_WM_NOTIFY(hwnd, wParam, lParam, fn) \
//    (fn)((hwnd), (int)(wParam), (NMHDR FAR*)(lParam))
//#define FORWARD_WM_NOTIFY(hwnd, idFrom, pnmhdr, fn) \
//    (LRESULT)(fn)((hwnd), WM_NOTIFY, (WPARAM)(int)(idFrom), (LPARAM)(NMHDR FAR*)(pnmhdr))


//====== Generic WM_NOTIFY notification codes =================================

#define NM_OUTOFMEMORY          (NM_FIRST-1)
#define NM_CLICK                (NM_FIRST-2)    // uses NMCLICK struct
#define NM_DBLCLK               (NM_FIRST-3)
#define NM_RETURN               (NM_FIRST-4)
#define NM_RCLICK               (NM_FIRST-5)    // uses NMCLICK struct
#define NM_RDBLCLK              (NM_FIRST-6)
#define NM_SETFOCUS             (NM_FIRST-7)
#define NM_KILLFOCUS            (NM_FIRST-8)
//#if (_WIN32_IE >= 0x0300)
#define NM_CUSTOMDRAW           (NM_FIRST-12)
#define NM_HOVER                (NM_FIRST-13)
//#endif
//#if (_WIN32_IE >= 0x0400)
#define NM_NCHITTEST            (NM_FIRST-14)   // uses NMMOUSE struct
#define NM_KEYDOWN              (NM_FIRST-15)   // uses NMKEY struct
#define NM_RELEASEDCAPTURE      (NM_FIRST-16)
#define NM_SETCURSOR            (NM_FIRST-17)   // uses NMMOUSE struct
#define NM_CHAR                 (NM_FIRST-18)   // uses NMCHAR struct
//#endif
//#if (_WIN32_IE >= 0x0401)
#define NM_TOOLTIPSCREATED      (NM_FIRST-19)   // notify of when the tooltips window is create
//#endif
//#if (_WIN32_IE >= 0x0500)
#define NM_LDOWN                (NM_FIRST-20)
#define NM_RDOWN                (NM_FIRST-21)
//#endif

//#ifndef CCSIZEOF_STRUCT
//#define CCSIZEOF_STRUCT(structname, member)  (((int)((LPBYTE)(&((structname*)0)->member) - ((LPBYTE)((structname*)0)))) + sizeof(((structname*)0)->member))
//#endif

//====== WM_NOTIFY codes (NMHDR.code values) ==================================

#define NM_FIRST                (0U-  0U)       // generic to all controls
#define NM_LAST                 (0U- 99U)

#define LVN_FIRST               (0U-100U)       // listview
#define LVN_LAST                (0U-199U)

// Property sheet reserved      (0U-200U) -  (0U-299U) - see prsht.h

#define HDN_FIRST               (0U-300U)       // header
#define HDN_LAST                (0U-399U)

#define TVN_FIRST               (0U-400U)       // treeview
#define TVN_LAST                (0U-499U)

#define TTN_FIRST               (0U-520U)       // tooltips
#define TTN_LAST                (0U-549U)

#define TCN_FIRST               (0U-550U)       // tab control
#define TCN_LAST                (0U-580U)

// Shell reserved               (0U-580U) -  (0U-589U)

#define CDN_FIRST               (0U-601U)       // common dialog (new)
#define CDN_LAST                (0U-699U)

#define TBN_FIRST               (0U-700U)       // toolbar
#define TBN_LAST                (0U-720U)

#define UDN_FIRST               (0U-721)        // updown
#define UDN_LAST                (0U-740)
//#if (_WIN32_IE >= 0x0300)
#define MCN_FIRST               (0U-750U)       // monthcal
#define MCN_LAST                (0U-759U)

#define DTN_FIRST               (0U-760U)       // datetimepick
#define DTN_LAST                (0U-799U)

#define CBEN_FIRST              (0U-800U)       // combo box ex
#define CBEN_LAST               (0U-830U)

#define RBN_FIRST               (0U-831U)       // rebar
#define RBN_LAST                (0U-859U)
//#endif

//#if (_WIN32_IE >= 0x0400)
#define IPN_FIRST               (0U-860U)       // internet address
#define IPN_LAST                (0U-879U)       // internet address

#define SBN_FIRST               (0U-880U)       // status bar
#define SBN_LAST                (0U-899U)

#define PGN_FIRST               (0U-900U)       // Pager Control
#define PGN_LAST                (0U-950U)

//#endif

//#if (_WIN32_IE >= 0x0500)
#ifndef WMN_FIRST
#define WMN_FIRST               (0U-1000U)
#define WMN_LAST                (0U-1200U)
//#endif
//#endif

#define MSGF_COMMCTRL_BEGINDRAG     0x4200
#define MSGF_COMMCTRL_SIZEHEADER    0x4201
#define MSGF_COMMCTRL_DRAGSELECT    0x4202
#define MSGF_COMMCTRL_TOOLBARCUST   0x4203
