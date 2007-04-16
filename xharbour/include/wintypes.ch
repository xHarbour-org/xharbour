#ifndef __WINTYPES_CH

   #define __WINTYPES_CH

   //#include "cstruct.ch"

   #define VOID                 CTYPE_VOID
   #define UINT                 CTYPE_UNSIGNED_INT
   #define int                  CTYPE_INT
   #pragma push -w0
   #define int(x)    Int(x) // Fixes conflict with Int() function
   #pragma pop -w
   #define HANDLE               CTYPE_UNSIGNED_LONG
   #define HICON                CTYPE_UNSIGNED_LONG
   #define HBITMAP              CTYPE_UNSIGNED_LONG
   #define HCURSOR              CTYPE_UNSIGNED_LONG
   #define HBRUSH               CTYPE_UNSIGNED_LONG
   #define LPCSTR               CTYPE_CHAR_PTR
   #define WNDPROC              CTYPE_UNSIGNED_LONG
   #define BOOL                 CTYPE_LONG
   #define LPVOID               CTYPE_VOID_PTR
   #define DWORD                CTYPE_UNSIGNED_LONG
   #define WORD                 CTYPE_UNSIGNED_SHORT
   #define LPCTSTR              CTYPE_CHAR_PTR
   #define COLORREF             CTYPE_UNSIGNED_LONG
   #define BYTE                 CTYPE_CHAR
   #define TCHAR                CTYPE_UNSIGNED_CHAR
   #define HINSTANCE            CTYPE_UNSIGNED_LONG
   #define HMENU                CTYPE_UNSIGNED_LONG
   #define HTREEITEM            CTYPE_UNSIGNED_LONG
   #define INT                  CTYPE_INT
   #pragma push -w0
   #define INT(x)    Int(x) // Fixes conflict with Int() function
   #pragma pop -w
   #define HWND                 CTYPE_UNSIGNED_LONG
   #define LPARAM               CTYPE_LONG
   #define HGLOBAL              CTYPE_UNSIGNED_LONG
   #define WPARAM               CTYPE_INT
   #define HKEY                 CTYPE_UNSIGNED_LONG
   #define char                 CTYPE_CHAR
   #define LONG                 CTYPE_LONG
   #define BCHAR                CTYPE_UNSIGNED_CHAR
   #define WCHAR                CTYPE_UNSIGNED_SHORT
   #define DOUBLE               CTYPE_DOUBLE
   #define LPTSTR               CTYPE_CHAR_PTR
   #define LPSTR                CTYPE_CHAR_PTR
   #define ULONG                CTYPE_UNSIGNED_LONG
   #define UCHAR                CTYPE_UNSIGNED_CHAR
   #define SHORT                CTYPE_SHORT
   #define USHORT               CTYPE_UNSIGNED_SHORT
   #define PVOID                CTYPE_VOID_PTR
   #define ULONG_PTR            CTYPE_UNSIGNED_LONG_PTR

   #define LPOFNHOOKPROC        CTYPE_UNSIGNED_LONG
   #define LPCFHOOKPROC         CTYPE_UNSIGNED_LONG
   #define LPFRHOOKPROC         CTYPE_UNSIGNED_LONG
   #define LPPAGESETUPHOOK      CTYPE_UNSIGNED_LONG
   #define LPPAGEPAINTHOOK      CTYPE_UNSIGNED_LONG
   #define LPPRINTHOOKPROC      CTYPE_UNSIGNED_LONG
   #define LPSETUPHOOKPROC      CTYPE_UNSIGNED_LONG

   #define BFFCALLBACK          CTYPE_UNSIGNED_LONG

   #define HDC                  CTYPE_UNSIGNED_LONG
   #define HIMAGELIST           CTYPE_UNSIGNED_LONG

#endif

