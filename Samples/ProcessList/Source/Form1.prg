#include "vxh.ch"
#include "Form1.xfm"

#include "cstruct.ch"
#include "hbdll.ch"

#define TH32CS_SNAPPROCESS 0x00000002
import CreateToolhelp32Snapshot( p1, p2 ) from Kernel32.dll
import Process32First( p1, p2 ) from Kernel32.dll
import Process32Next( p1, p2 ) from Kernel32.dll

C structure PROCESSENTRY32 align 4
   member dwSize is CTYPE_UNSIGNED_LONG
   member cntUsage is CTYPE_UNSIGNED_LONG
   member th32ProcessID is CTYPE_UNSIGNED_LONG
   member th32DefaultHeapID is CTYPE_UNSIGNED_LONG_PTR
   member th32ModuleID is CTYPE_UNSIGNED_LONG
   member cntThreads is CTYPE_UNSIGNED_LONG
   member th32ParentProcessID is CTYPE_UNSIGNED_LONG
   member pcPriClassBase is CTYPE_LONG
   member dwFlags is CTYPE_UNSIGNED_LONG
   member szExeFile[MAX_PATH] is CTYPE_CHAR
end C structure

//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Button1_OnClick( Sender ) CLASS Form1
   if !::GetSnapShot()
      ::MessageBox( "Error "+str( GetLastError() )+" !", "Alert", MB_ICONEXCLAMATION|MB_OK )
   endif   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnLoad( Sender ) CLASS Form1
   if !::GetSnapShot()
      ::MessageBox( "Error "+str( GetLastError() )+"  -- please try a 'Refresh'!", "Alert", MB_ICONEXCLAMATION|MB_OK )
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GetOut( oStru ) CLASS Form1
   local i, y:="", aText:=oStru:szExeFile:value()
   for i:=1 to MAX_PATH
      if aText[i]==chr(0)
         exit
      else
         y+=aText[i]
      endif
   next   
   ::MemoryTable1:Append()
   with object ::MemoryTable1:Fields
      :TASK := y
      :ID := oStru:th32ProcessID
      :THREADS := oStru:cntThreads
   end
RETURN NIL
//----------------------------------------------------------------------------------------------------//
METHOD GetSnapShot() CLASS Form1
   
     local cBuf, nLen
     local oStru:=(STRUCT PROCESSENTRY32)
     local res, hand:=CreateToolhelp32Snapshot( TH32CS_SNAPPROCESS, NIL )
     
     if hand == NIL
        return .F.
     endif
     
     nLen:=oStru:SizeOf()
     cBuf:=space(nLen)     
     res:=Process32First( hand, @cBuf )
     if res == 0
        CloseHandle( hand )
        return .F.
     else
        ::MemoryTable1:Zap()
     endif
     oStru:Buffer( cBuf )
     ::GetOut( oStru )
     
     do while .t.
        cBuf:=space(nLen)
        res:=Process32Next( hand, @cBuf )
        if res == 0
           exit
        endif
        oStru:Buffer( cBuf )
        ::GetOut( oStru )
     end
     
     CloseHandle( hand )
     ::MemoryTable1:GoTop()
     ::DataGrid1:Update()

RETURN .t.