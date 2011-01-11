GLOBAL cBuffer:=NIL

#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnLoad( Sender ) CLASS Form1
   local nVal, cFile:=""
   ::nBlock:=10240
   ::SpecChar:=chr(0)
   
   if len( ::Application:Params ) < 1
      ::OpenFileDialog1:FileName:=""
      ::OpenFileDialog1:Show()
      cFile=::OpenFileDialog1:FileName
   else
      cFile:=::Application:Params[1]
   endif
   
   if empty( cFile )
      ::Close()
   else
      ::nTot:=HB_FSize( cFile )
      if ::nTot>0 .and. ::nTot<=1000*::nBlock
         cBuffer:=MemoRead( cFile )
         if empty( cBuffer )
            ::MessageBox( "Input file empty or not available", "BitViewer", MB_ICONINFORMATION  )
            ::Close()
         endif
         if ::nTot > ::nBlock
            nVal:=floor(::nTot/::nBlock)
            if nVal*::nBlock == ::nTot
               nVal--
            endif
         else
            nVal:=0
         endif
         ::LabelTotal:Caption:=ltrim(str(nVal))
         ::myDisplay()
      else
         if ::nTot > 0
            ::MessageBox( "The input file exceeds 1000 blocks of " + ltrim(str(::nBlock)) + " bytes", "BitViewer", MB_ICONINFORMATION  )
         else
            ::MessageBox( "Input file empty or not available", "BitViewer", MB_ICONINFORMATION  )
         endif
         ::Close()
      endif
   endif
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnSysCommand( Sender ) CLASS Form1
   if ::wParam == SC_CLOSE
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myEstimate( Sender ) CLASS Form1
   local nMax:=::mt:RecCount(), nPos:=::mt:RecNo()
   local c2, c4, c8, ni, nr
   
   ::Lposition:Caption:=ltrim(str(nPos))
   
   if nPos == nMax
      ::Lbin2w:Caption:=""
      ::Lbin2i:Caption:=""
      ::Lbin2u:Caption:=""
      ::Lbin2l:Caption:=""
      return Self
   endif
   
   c2:=substr( ::Subject, nPos, 2 )
   ni:=Bin2W( c2 )
   ::Lbin2w:Caption:=ltrim(str(ni))
   ni:=Bin2I( c2 )
   ::Lbin2i:Caption:=ltrim(str(ni))
   
   if nPos+2 >= nMax
      ::Lbin2u:Caption:=""
      ::Lbin2l:Caption:=""
      return Self
   endif
   
   c4:=substr( ::Subject, nPos, 4 )
   ni:=Bin2U( c4 )
   ::Lbin2u:Caption:=ltrim(str(ni))
   ni:=Bin2L( c4 )
   ::Lbin2l:Caption:=ltrim(str(ni))  
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonColor_OnClick( Sender ) CLASS Form1
   local n:=Int( val( alltrim(::EditAsc:Caption) ) )
   
   if n<0 .or. n>255
      ::MessageBox( "The ASCII code needs to be between 0 and 255", "BitViewer", MB_ICONEXCLAMATION )
      return Self
   endif
   
   ::EditAsc:Caption:=ltrim(str(n))
   ::SpecChar:=chr(n)
   ::dg:Update()
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridColumn1_OnQueryBackColor( Sender ) CLASS Form1
   if ::dg:DataSource:Fields:CHR == ::SpecChar
      return 8442111
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonReplace_OnClick( Sender ) CLASS Form1
   local nPos:=Int( val( alltrim(::EditBlock:Caption) ) )
   if nPos<0 .or. nPos*::nBlock>::nTot
      ::MessageBox( "Required block number is out of range", "BitViewer", MB_ICONEXCLAMATION )
      return Self
   endif
   ::MyDisplay()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyDisplay() CLASS Form1
   local i, nVal, nLen
   local nPos:=Int( val( alltrim(::EditBlock:Caption) ) )

//   ::Disable()

   if (nPos+1)*::nBlock > ::nTot
      nLen:=::nTot - nPos*::nBlock
   else
      nLen:=::nBlock
   endif
   ::mt:Zap()
   ::Subject:=substr( cBuffer, 1+nPos*::nBlock, nLen )
   
   with object ::mt
      for i:=1 to nLen
         :Append()
         nVal:=asc(::Subject[i])
         :Fields:POS:=ltrim( str(i) )
         :Fields:CHR:=::Subject[i]
         :Fields:ASC:=ltrim( str(nVal) )
         :Fields:HEX:=NumToHex( nVal )
         :Fields:BIN:=NtoC( nVal, 2 )
      next
      :GoTop()
   end
   ::dg:Update()
   
   ::Enable()
   
RETURN Self