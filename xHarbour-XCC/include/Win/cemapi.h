#ifndef _CEMAPI_H
#define _CEMAPI_H

/* Messaging Applications Programming Interface (MAPI) definitions (Windows CE) */

#include <mapidefs.h>
#include <mapicode.h>
#include <mapitags.h>
#include <mapispi.h>
#include <mapix.h>

#ifndef _PURE
#define _PURE =0
#endif // _PURE

#define SYNCPROGRESSITEM_STATUSTEXT  0x0001
#define SYNCPROGRESSITEM_STATUSTYPE  0x0002
#define SYNCPROGRESSITEM_PROGVALUE  0x0004
#define SYNCPROGRESSITEM_MAXVALUE  0x0008
#define SYNCPROGRESSITEM_DISCONNECTED  0x0010
#define SYNCPROGRESSITEM_TOTAL_NEW_MAIL  0x0020
#define SYNCPROGRESSITEM_NEW_MESSAGE  0x0040

#define SYNC_NORMAL  0
#define SYNC_HIERARCHY  1
#define SYNC_CREATE_FOLDER  2
#define SYNC_RENAME_FOLDER  3
#define SYNC_DELETE_FOLDER  4
#define SYNC_RESETHIERARCHY  5
#define SYNC_FOLDER  6

#define MESSAGEFIELDS_FLAGS_DELETEONCLOSE  0x00000001
#define MESSAGEFIELDS_FLAGS_SMSSERVICE  0x00000002

#define DELACTION_IMMEDIATELY  0x00000000
#define DELACTION_MANUALLY  0x00000001

#define FOLDER_HIDE  0x00010000
#define FOLDER_SPECIAL  0x00020000
#define FOLDER_INBOX  0x00040000
#define FOLDER_DELETED_ITEMS  0x00080000
#define FOLDER_SENT_ITEMS  0x00100000
#define FOLDER_OUTBOX  0x00200000
#define FOLDER_DRAFTS  0x00400000
#define FOLDER_IPM  0x00800000

#define SHUTDOWNFLAG_LOSTCONNECTION  0x0001

#define MAX_ONESTOP_TYPE_NAME  32

#define PR_CE_IMAP_UID  PROP_TAG(PT_LONG,0x8100)
#define PR_CE_MIME_TEXT  PROP_TAG(PT_BINARY,0x8103)
#define PR_CE_IPM_DRAFTS_ENTRYID  PROP_TAG(PT_BINARY,0x8104)
#define PR_CE_IPM_INBOX_ENTRYID  PROP_TAG(PT_BINARY,0x8106)
#define PR_CONTENT_LENGTH_EX  PROP_TAG(PT_LONG,0x8108)
#define PR_CE_XPRT_MSG_STATUS  PROP_TAG(PT_LONG,0x8102)
#define PR_CE_XPRT_MESSAGE_FLAGS  PROP_TAG(PT_LONG,0x810b)
#define PR_CE_ITEM_GENERATION  PROP_TAG(PT_LONG,0x810c)
#define PR_CE_ITEM_INDEX  PROP_TAG(PT_LONG,0x810d)
#define PR_CE_XHEADERS  PROP_TAG(PT_UNICODE,0x8110)
#ifndef PR_MESSAGE_SIZE_EX 
#define PR_MESSAGE_SIZE_EX  PROP_TAG(PT_LONG,0x8111)
#endif 

#define MSGSTATUS_RECTYPE_SMTP  0x00020000
#define MSGSTATUS_RECTYPE_SMS  0x00040000
#define MSGSTATUS_SMSSTATUS_SUCCESS  0x00020000
#define MSGSTATUS_RECTYPE_ALLTYPES  (MSGSTATUS_RECTYPE_SMTP|MSGSTATUS_RECTYPE_SMS)
#define MSGSTATUS_HEADERONLY  0x00010000
#define MSGSTATUS_PARTIAL_DOWNLOAD  0x00080000
#define MSGSTATUS_PARTIAL  0x00100000
#define MSGSTATUS_REMOTE_DOWNLOAD_ATTACH  0x00200000
#define MSGSTATUS_REMOTE_DOWNLOAD_HEADER  0x00400000
#define MSGSTATUS_HAS_TNEF  0x00800000
#define MSGSTATUS_REMOTE_DOWNLOAD_TNEF  0x01000000
#define MSGSTATUS_GHOSTED_ATTACH  0x02000000
#define MSGSTATUS_PENDING_ATTACHMENTS  0x04000000
#define MSGSTATUS_HAS_PR_CE_MIME_TEXT  0x08000000
#define MSGSTATUS_HAS_PR_BODY  0x10000000

#define CEMAPI_E_PLAINTEXT_NOTSUPPORTED  MAKE_MAPI_E(0x901)
#define CEMAPI_E_PROTOCOL_ERROR  MAKE_MAPI_E(0x902)
#define CEMAPI_E_NO_ACCOUNT_INFO  MAKE_MAPI_E(0x903)
#define CEMAPI_E_NEED_TO_CONNECT  MAKE_MAPI_E(0x904)
#define CEMAPI_E_MUST_SYNC_ONCE  MAKE_MAPI_E(0x905)

typedef struct _SYNCPROGRESSITEM {
    ULONG cbSize;
    DWORD mask;
    LPCWSTR pwszStatusText;
    DWORD dwStatusType;
    ULONG ulProgValue;
    ULONG ulMaxValue;
    ULONG ulTotalNewMail;
    ULONG cbIdNewMsg;
    LPENTRYID pidNewMsg;
} SYNCPROGRESSITEM;

typedef struct _MAILSYNCREQUEST {
    DWORD cbSize;
    DWORD cbBufSize;
    DWORD cbCookie;
    LPBYTE pbCookie;
    ULONG ffFlags;
    ULONG objType;
    LPENTRYID pid;
    DWORD cbId;
    LPSPropValue pval;
} MAILSYNCREQUEST;

typedef struct _SYNCCREDENTIALS {
    DWORD cbSize;
    DWORD cbBufSize;
    LPTSTR pszUsername;
    LPTSTR pszDomain;
    LPTSTR pszPassword;
} SYNCCREDENTIALS;

typedef struct _TRANSPORTEVENT {
    LPCWSTR pszSourceDLL;
    LPCWSTR pszSourceProfile;
    HRESULT hr;
    DWORD dwSeverity;
    DWORD cbData;
    LPBYTE pbData;
} TRANSPORTEVENT;

typedef struct _MESSAGEFIELDS {
    LPCWSTR pszwTo;
    LPCWSTR pszwCc;
    LPCWSTR pszwBcc;
    LPCWSTR pszwSubject;
    LPCWSTR pszwBody;
    DWORD flags;
} MESSAGEFIELDS;

const DWORD kdwOptionValueNone = 0;
const DWORD kdwOptionValueAll = 0xFFFFFFFF;

const WCHAR kszCapAmountToFetch[] = L"FetchSize";  
const WCHAR kszCapAttachAmount[] = L"AttachFetchSize";
const WCHAR kszCapAgeFilter[] = L"AgeFilter";
const WCHAR kszCapSaveSent[] = L"SaveSent";
const WCHAR kszCapSMSUnicode[] = L"SMSUnicode";
const WCHAR kszCapSMTPAuthenticate[] = L"SMTPAuthenticate";
const WCHAR kszCapIncludeOriginal[] = L"IncludeOriginal";
const WCHAR kszCapIndentBody[] = L"IndentBody";
const WCHAR kszCapAddLeading[] = L"AddLeading";
const WCHAR kszCapLeadChar[] = L"LeadChar";
const WCHAR kszCapFolders[] = L"Folders";
const WCHAR kszCapFolderNotifications[] = L"FolderNotifications";
const WCHAR kszCapSMSRequestStatus[] = L"SMSRequestStatus";
const WCHAR kszCapSyncMsgClass[] = L"SyncMsgClass";
const WCHAR kszCapPropDialog[] = L"PropDialog";
const WCHAR kszCapSMTPAllowAuthLogin[] = L"SMTPAllowAuthLogin";
const WCHAR kszCapSyncHierarchy[] = L"SyncHierarchy";
const WCHAR kszCapDeleteAction[] = L"DeleteAction";
const WCHAR kszCapMoveToTrash[] = L"TrashMoves";

const WCHAR kszCapReturnAddress[] = L"ReturnAddress";
const WCHAR kszCapCreateSpecialFldrs[] = L"CreateSpecialFolders";

class IMailSyncCallBack : public IUnknown
{
    public:
    MAPIMETHOD(RequestSync)(LPCWSTR,DWORD,LPBYTE) _PURE;
    MAPIMETHOD(Progress)(LPCWSTR,SYNCPROGRESSITEM*) _PURE;
    MAPIMETHOD(GetGlobalSetting)(LPCWSTR,LPSPropValue) _PURE;
    MAPIMETHOD_(UINT, DisplayMessageBox)(LPCWSTR,LPCWSTR,LPCWSTR,UINT) _PURE;
    MAPIMETHOD(RequestCredentials)(LPCWSTR,SYNCCREDENTIALS*,SYNCCREDENTIALS**) _PURE;
    MAPIMETHOD(LogEvent)(TRANSPORTEVENT*) _PURE;
    MAPIMETHOD(AllocateMem)(DWORD,LPBYTE*) _PURE;
    MAPIMETHOD(FreeMem)(LPVOID) _PURE;
};

typedef struct _FOLDERNODE {    
    _FOLDERNODE *pSibbling;
    _FOLDERNODE *pChild;
    _FOLDERNODE *pParent;
    DWORD ffFlags;
    LPWSTR szName;
} FOLDERNODE;


class IMailSyncHandler : public IUnknown
{
    public:
    MAPIMETHOD(Initialize)(IMailSyncCallBack*,LPCWSTR,IMsgStore*) _PURE;
    MAPIMETHOD(ShutDown)(DWORD) _PURE;
    MAPIMETHOD(Synchronize)(MAILSYNCREQUEST*) _PURE;
    MAPIMETHOD(DoProperties)(HWND) _PURE;
    MAPIMETHOD(GetCapability)(LPCWSTR,LPSPropValue) _PURE;
    MAPIMETHOD(SetCapability)(LPCWSTR,LPSPropValue) _PURE;
    MAPIMETHOD(Install)(LPCWSTR,LPCWSTR,LPCWSTR,GUID*) _PURE;
    MAPIMETHOD(UnInstall)() _PURE;
    MAPIMETHOD(DecodeEvent)(TRANSPORTEVENT*,LPWSTR*) _PURE;
    MAPIMETHOD(GetFolderHierarchy)(FOLDERNODE**) _PURE;
    enum FOLDEROPTIONS {
        koptNA = 0,
        koptDownload,
        koptQueryDownload,
        koptNotifications,
        koptQueryNotifications,
        koptGetRemotePath,
        koptCreate,
        koptQueryCreate,
        koptSetAge,
        koptSetBodyFetchSize,
        koptSetAttachFetchSize,
        koptGetAge,
        koptGetBodyFetchSize,
        koptGetAttachFetchSize,
    }; 
    MAPIMETHOD(SetFolderOptions)(IMAPIFolder*,FOLDERNODE*,FOLDEROPTIONS,LPSPropValue) _PURE;
    MAPIMETHOD(Connect)(DWORD,SYNCCREDENTIALS*) _PURE;
    MAPIMETHOD(Disconnect)(DWORD) _PURE;
};

typedef HRESULT (WINAPI *ONESTOPFACTORYFUNC)(LPCWSTR, IMailSyncHandler**);

#define MAPI_ICEMAPISESSION_METHODS(IPURE) \
    MAPIMETHOD(CreateMsgStore)(THIS_ LPCWSTR,IMsgStore**) IPURE; \
    MAPIMETHOD(DeleteMsgStore)(THIS_ ULONG,LPENTRYID) IPURE;

#undef INTERFACE
#define INTERFACE ICEMAPISession
DECLARE_MAPI_INTERFACE_(ICEMAPISession, IMAPISession)
{
    public:
    BEGIN_INTERFACE 
    MAPI_IUNKNOWN_METHODS(PURE)
    MAPI_IMAPISESSION_METHODS(PURE)
    MAPI_ICEMAPISESSION_METHODS(PURE)
};

typedef struct _CEENTRYID {
    DWORD abFlags;
    DWORD Data1;
    DWORD Data2;
    DWORD Data3;
} CEENTRYID;

class IMessageForm;

class IMessageFormHost
{
    public:
    virtual HRESULT GetParentHWND(HWND*) _PURE;
    enum eMsgStatus {
        eNotAvailable = 0,
        eSent = 1,
        ePostponed = 2,
        eDelete = 3,
        eDeletePermanently = 4
    };
    virtual HRESULT FormClosing(IMessageForm*,IMessage*,eMsgStatus) _PURE;
    MAPIMETHOD(GetGlobalSetting)(LPCWSTR, LPSPropValue) _PURE;
    MAPIMETHOD(SetGlobalSetting)(LPCWSTR, LPSPropValue) _PURE;
    enum actions { 
        actNull = 0,
        actFirst = 0,
        actReplyTo = 1,
        actReplyToAll = 2,
        actForward = 3,
        actDelete = 4,
        actClose = 5,
        actMoveToFolder = 6,
        actGotoNextMessage = 7,
        actGotoPrevMessage = 8,
        actComposeNew = 9,
        actGetFullMessage = 10,
        actViewAttachment = 11,
        actReplyWith = 12,
        actShow = 13,
        actLast
    };
    virtual HRESULT DoAction(IMessageForm*, IMessage*,actions,MESSAGEFIELDS*) _PURE;
    virtual HRESULT CreateNewMessage(IMessage**) _PURE;
};

class IMessageForm : public IUnknown
{
    public:
    MAPIMETHOD(Activate)() _PURE;
    MAPIMETHOD(ShowForm)(ULONG) _PURE;
    MAPIMETHOD(SetMessage)(WORD,LPMESSAGE) _PURE;
    MAPIMETHOD(CloseForm)(BOOL fSave=FALSE) _PURE;
    MAPIMETHOD(PreTranslateMsg)(MSG*,BOOL*) _PURE;
    MAPIMETHOD(GetActiveMessage)(LPMESSAGE*) _PURE;
    MAPIMETHOD(OnMessageModified)(ULONG,LPENTRYID,LPSPropTagArray) _PURE;
    MAPIMETHOD(SetCaptionText)(LPCWSTR) _PURE;
};

class IFormProvider : public IUnknown
{
    public:
    enum FORMTYPE {
        kfNewMsg = 0,
        kfReply = 1,
        kfReplyToAll = 2,
        kfForward = 3,
        kfLoad = 4,
        kfReplyWith = 5
    };
    MAPIMETHOD(GetMsgStatusIconIndex)(LPMAPIFOLDER,LPENTRYID,ULONG,ULONG,ULONG,LPCWSTR,ULONG,ULONG*) _PURE;
    MAPIMETHOD(ComposeMessageForm)(IMessageFormHost*,LPMESSAGE,IMessageForm**,FORMTYPE,LPMESSAGE,MESSAGEFIELDS*) _PURE;
    MAPIMETHOD(CreateReadForm)(IMessageFormHost*,IMessageForm**,LPMESSAGE) _PURE;
};

extern "C" {
HRESULT WINAPI FormFactory(LPCWSTR, IFormProvider**);
}

typedef HRESULT (WINAPI *FORMFACTORYFUNC)(LPCWSTR, IFormProvider**);

typedef struct _INBOXGLYPHINFO {
    ULONG cbSize;
    ULONG ResIdColor;
    ULONG ResIdMono;
    ULONG iUnRead;
    ULONG iRead;
    ULONG iRepliedTo;
    ULONG iForwarded;
    ULONG iAttachment;
} INBOXGLYPHINFO;

#endif /* _CEMAPI_H */
