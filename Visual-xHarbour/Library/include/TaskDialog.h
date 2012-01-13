typedef struct tagTASKDIALOG_BUTTON {
  int    nButtonID;
  PCWSTR pszButtonText;
} TASKDIALOG_BUTTON;

enum _TASKDIALOG_FLAGS
{
  TDF_ENABLE_HYPERLINKS               = 0x0001,
  TDF_USE_HICON_MAIN                  = 0x0002,
  TDF_USE_HICON_FOOTER                = 0x0004,
  TDF_ALLOW_DIALOG_CANCELLATION       = 0x0008,
  TDF_USE_COMMAND_LINKS               = 0x0010,
  TDF_USE_COMMAND_LINKS_NO_ICON       = 0x0020,
  TDF_EXPAND_FOOTER_AREA              = 0x0040,
  TDF_EXPANDED_BY_DEFAULT             = 0x0080,
  TDF_VERIFICATION_FLAG_CHECKED       = 0x0100,
  TDF_SHOW_PROGRESS_BAR               = 0x0200,
  TDF_SHOW_MARQUEE_PROGRESS_BAR       = 0x0400,
  TDF_CALLBACK_TIMER                  = 0x0800,
  TDF_POSITION_RELATIVE_TO_WINDOW     = 0x1000,
  TDF_RTL_LAYOUT                      = 0x2000,
  TDF_NO_DEFAULT_RADIO_BUTTON         = 0x4000,
  TDF_CAN_BE_MINIMIZED                = 0x8000
};
typedef int TASKDIALOG_FLAGS;

enum _TASKDIALOG_COMMON_BUTTON_FLAGS
{
  TDCBF_OK_BUTTON            = 0x0001, // selected control return value IDOK
  TDCBF_YES_BUTTON           = 0x0002, // selected control return value IDYES
  TDCBF_NO_BUTTON            = 0x0004, // selected control return value IDNO
  TDCBF_CANCEL_BUTTON        = 0x0008, // selected control return value IDCANCEL
  TDCBF_RETRY_BUTTON         = 0x0010, // selected control return value IDRETRY
  TDCBF_CLOSE_BUTTON         = 0x0020  // selected control return value IDCLOSE
};
typedef int TASKDIALOG_COMMON_BUTTON_FLAGS;
typedef HRESULT (CALLBACK* PFTASKDIALOGCALLBACK)(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam, LONG_PTR lpRefData);
#define TD_WARNING_ICON MAKEINTRESOURCEW(-1)

typedef struct _TASKDIALOGCONFIG {
  UINT                           cbSize;
  HWND                           hwndParent;
  HINSTANCE                      hInstance;
  TASKDIALOG_FLAGS               dwFlags;
  TASKDIALOG_COMMON_BUTTON_FLAGS dwCommonButtons;
  PCWSTR                         pszWindowTitle;
  union {
    HICON  hMainIcon;
    PCWSTR pszMainIcon;
  };
  PCWSTR                         pszMainInstruction;
  PCWSTR                         pszContent;
  UINT                           cButtons;
  const TASKDIALOG_BUTTON        *pButtons;
  int                            nDefaultButton;
  UINT                           cRadioButtons;
  const TASKDIALOG_BUTTON        *pRadioButtons;
  int                            nDefaultRadioButton;
  PCWSTR                         pszVerificationText;
  PCWSTR                         pszExpandedInformation;
  PCWSTR                         pszExpandedControlText;
  PCWSTR                         pszCollapsedControlText;
  union {
    HICON  hFooterIcon;
    PCWSTR pszFooterIcon;
  };
  PCWSTR                         pszFooter;
  PFTASKDIALOGCALLBACK           pfCallback;
  LONG_PTR                       lpCallbackData;
  UINT                           cxWidth;
} TASKDIALOGCONFIG;
