#include "vxh.ch"
#include "FormInfo.xfm"
//---------------------------------------- End of system code ----------------------------------------//

#include "cstruct.ch"
#include "hbdll.ch"

#define PRINTER_ENUM_LOCAL  0x00000002
#define PRINTER_ENUM_CONNECTIONS  0x00000004

IMPORT EnumPrintersA( par1, par2, par3, par4, par5, par6, par7 ) FROM winspool.drv

// the content of this structure is being read fast ( retrieved from the local registry )
// use this one whenever possible
   C structure PRINTER_INFO_4 align 4
      member pPrinterName is CTYPE_CHAR_PTR
      member pServerName  is CTYPE_CHAR_PTR
      member Attributes   is CTYPE_UNSIGNED_LONG
   end C structure

// the content of this structure is read slowly and it might generate error messages,
// because each available printer is "opened" and "closed"
// use this one when there is no better solution
   C structure PRINTER_INFO_2 align 4
      member pServerName         is CTYPE_CHAR_PTR
      member pPrinterName        is CTYPE_CHAR_PTR
      member pShareName          is CTYPE_CHAR_PTR
      member pPortName           is CTYPE_CHAR_PTR
      member pDriverName         is CTYPE_CHAR_PTR
      member pComment            is CTYPE_CHAR_PTR
      member pLocation           is CTYPE_CHAR_PTR
      member pDevMode            is CTYPE_VOID_PTR
      member pSepFile            is CTYPE_CHAR_PTR
      member pPrintProcessor     is CTYPE_CHAR_PTR
      member pDataType           is CTYPE_CHAR_PTR
      member pParameters         is CTYPE_CHAR_PTR
      member pSecurityDescriptor is CTYPE_CHAR_PTR
      member Attributes          is CTYPE_UNSIGNED_LONG
      member Priority            is CTYPE_UNSIGNED_LONG
      member DefaultPriority     is CTYPE_UNSIGNED_LONG
      member StartTime           is CTYPE_UNSIGNED_LONG
      member UntilTime           is CTYPE_UNSIGNED_LONG
      member Status              is CTYPE_UNSIGNED_LONG
      member cJobs               is CTYPE_UNSIGNED_LONG
      member AveragePPM          is CTYPE_UNSIGNED_LONG
   end C structure



function ListPrinters( oWindow, nLevel, nFlags, cPrn, aStru )
   local nNeeded:=u2bin(0), nCrt:=u2bin(0)
   local cBuf:=NIL, nLenBuf:=0, nStru, nRet
   local i, nLenStru
   local bSt, aRoot:=NIL
   if nLevel<>2 .and. nLevel<>4
      oWindow:MessageBox( "Wrong LEVEL value "+str(nLevel), ;
                  "Error", MB_ICONHAND )
      return .f.
   endif

   if nLevel == 2
       bSt:=(STRUCT PRINTER_INFO_2)
   else
       bSt:=(STRUCT PRINTER_INFO_4)
   endif
   nLenStru:=bSt:SizeOf()
   
   nRet:=EnumPrintersA( nFlags, cPrn, nLevel, ;
                       @cBuf, nLenBuf, @nNeeded, @nCrt )
   nLenBuf:=bin2u( nNeeded )
   if nLenBuf < nLenStru
      oWindow:MessageBox( "GetPrintersA buffer error "+str(nRet), ;
                  "Error "+str( GetLastError()), MB_ICONHAND )
      return .f.
   endif
   
   cBuf:=replicate( chr(0), nLenBuf )
   nRet:=EnumPrintersA( nFlags, cPrn, nLevel, ;
                       @cBuf, nLenBuf, @nNeeded, @nCrt )
   nLenBuf:=bin2u( nNeeded )
   nStru:=bin2u( nCrt )
   if ( nRet < 1 ) .or. ( nLenBuf < nLenStru ) .or. ( nStru < 1 )
      oWindow:MessageBox( "GetPrintersA error "+str(nRet), ;
                  "Error "+str( GetLastError()), MB_ICONHAND )
      return .f.
   endif  
   
   aRoot:=array( nStru )
   aStru:=array( nStru )
   
   for i:=1 to nStru
      aRoot[i]:=substr( cBuf, (i-1)*nLenStru+1, nLenStru )
      if nLevel == 2
         aStru[i]:=(STRUCT PRINTER_INFO_2)
      else
         aStru[i]:=(STRUCT PRINTER_INFO_4)
      endif
      aStru[i]:Buffer( aRoot[i], .T. )
   next      
return .t.



//----------------------------------------------------------------------------------------------------//
METHOD RadioBrief_OnClick( Sender ) CLASS FormInfo
   ::DoRefresh( 4 ) 
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD RadioDetailed_OnClick( Sender ) CLASS FormInfo
   ::DoRefresh( 2 )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormInfo_OnLoad( Sender ) CLASS FormInfo
   ::RadioBrief:SetState( BST_CHECKED )
   ::RadioDetailed:SetState( BST_UNCHECKED )
   ::DoRefresh( 4 )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD DoRefresh( nLevel ) CLASS FormInfo
   local i, cPrn:=NIL, aStru:=NIL
   local nFlags:=PRINTER_ENUM_LOCAL|PRINTER_ENUM_CONNECTIONS
   
   if ListPrinters( ::this, nLevel, nFlags, cPrn, @aStru )
      with object ::dg:DataSource
         :Zap()
         for i:=1 to len( aStru )
            :Append()
            :Fields:SERVER := aStru[i]:pServerName
            :Fields:NAME   := aStru[i]:pPrinterName
            if nLevel == 2
               :Fields:PORT := aStru[i]:pPortName
            endif
         next         
         :GoTop()
      end
     ::dg:Update()
   endif   
   
RETURN Self