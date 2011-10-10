#include "vxh.ch"
#include "Form1.xfm"

#include "hbxml.ch"
#include "fileio.ch"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD ButtonStart_OnClick( Sender ) CLASS Form1
   local nStep:=16
   if !( ::oTest == NIL )
      ::oTest:Close()
   endif
   ::MyRoot( nStep, .F. )   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyConvert( nStep, nChunk, lTest ) CLASS Form1
   local cHeader := '<?xml version="1.0" encoding="utf-8" ?>'+;
         chr(13)+chr(10)+'<table>'+chr(13)+chr(10)
   local cFooter := "</table>" + chr(13) + chr(10)   
   local nHeader := len(cHeader), nEconomy:=floor( nChunk/2 )
   local cOut:=cHeader+space(nChunk-nHeader), cNext
   local nCurrLen:=nHeader, nNextLen, nDiff
   local lFaulty:=.F., lError:=.F.
   local nTotal := ::MyTable:RecCount()
   local i, j, nCycles
   
   if nStep<1 .or. nTotal<1
      return .F.
   endif
   
   nCycles := if( lTest, 1, ceiling( nTotal/nStep ) )
   
   for i:=1 to nCycles

      ::Application:Yield()
      if ::lCanceled
         exit
      endif
      if !( ::oProggy == NIL )
         ::oProggy:Proggy:StepIt()
      endif
      
      cNext := ::Cycle( i, nStep, @lError )
      hb_gcall(.T.)  // cleaning after TXML*
      
      if empty( cNext )
         exit
      endif
      
      nNextLen := len( cNext )
      nDiff := nChunk - nCurrLen - nNextLen
      
      if nDiff < 0  // the new sring doesn't fit the chunk
         
         if !::OutWrite( @cOut, nCurrLen )    // write out existing content
            lFaulty := .T.
            exit
         endif
         nCurrLen:=0
         
         if nNextLen > nEconomy  // the new string is "big", so it's written out directly
            if !::OutWrite( @cNext, nNextLen )
               lFaulty := .T.
               exit
            endif
         else      // the new string is added in the chunk, at the beginning
            for j:=1 to nNextLen
               cOut[j] := cNext[j]
            next
            nCurrLen := nNextLen
         endif
         
      else  // the new string is added into the chunk, after the existing content
         
         for j:=1 to nNextLen
            cOut[nCurrLen+j] := cNext[j]
         next
         nCurrLen := nCurrLen + nNextLen
         
      endif
   
   next
   
   if ::lCanceled
      return .F.
   endif
   
   if !lFaulty .and. !lError
      if nCurrLen > 0
         if !::OutWrite( @cOut, nCurrLen ) 
            lFaulty := .T.
         endif
      endif
      if !lFaulty
         if !::OutWrite( @cFooter, len(cFooter) )
            lFaulty := .T.
         endif
      endif
   endif
      
RETURN if( lFaulty.or.lError, .F., .T. )
//----------------------------------------------------------------------------------------------------//
METHOD Cycle( nCounter, nStep, lError ) CLASS Form1
   local i, j, cRes := ""
   local nPos:=len(::Pos), cVal, cTag
   local nCurr, nDig, nDec
   local cSeq
   local oDoc, oSeq:=NIL, oNode
   local oRow := ::MyTable:Fields
   
   oDoc := TXMLDocument():New()
   
   ::MyTable:GoTo( (nCounter-1)*nStep +1 )
   
   for i:=1 to nStep
      if ::MyTable:Eof()
         exit
      endif

      cSeq := ltrim( str(::MyTable:RecNo()) )
      oNode := TXMLNode():New( HBXML_TYPE_TAG, "row", { "sequence"=>cSeq } )
      if oSeq == NIL
         oSeq := oDoc:oRoot:addBelow( oNode )
      else
         oSeq:insertAfter( oNode )
         oSeq := oNode
      endif
         
      for j:=1 to nPos
            
         nCurr :=  ::Pos[j]
         cTag := lower( ::Stru[ nCurr, 1 ] )
            
         switch ( ::Stru[ nCurr, 2 ] )
         case "C"
         case "M"
            cVal := hb_strtoutf8( rtrim( oRow:FieldGet(nCurr) ) )
            exit
         case "N"
            nDig := ::Stru[ nCurr, 3 ]
            nDec := ::Stru[ nCurr, 4 ]
            if nDec < 1
               cVal := ltrim( str( oRow:FieldGet(nCurr), nDig ) )
            else
               cVal := ltrim( str( oRow:FieldGet(nCurr), nDig, nDec ) )
            endif
            exit
         case "D"
            cVal := dtos( oRow:FieldGet(nCurr) )
            exit
         case "L"
            cVal := if( oRow:FieldGet(nCurr), "Y", "N" )
            exit
         end   

         oNode := TXMLNode():New( HBXML_TYPE_TAG, cTag, , cVal )
         oSeq:addBelow( oNode )
            
      next
               
      ::MyTable:Skip()
         
   next
   
   if !( oSeq == NIL )
      cRes := oDoc:toString( HBXML_STYLE_INDENT )
   endif
   
   oNode := NIL
   oSeq := NIL
   oDoc := NIL
RETURN cRes
//----------------------------------------------------------------------------------------------------//
METHOD ButtonBrow_OnClick( Sender ) CLASS Form1
   with object ::MyOpenFile
      :DefaultExt := "dbf"
      :Filter := "DBase III or IV file (*.dbf)|*.dbf|All files ( *.* )|*.*"
      :Title := "Select the input DBF file:"
      :FileName := ""
      if :Show() .and. !empty( :FileName )
         ::EditIn:Caption := :FileName
      endif
   end
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD OpenTable() CLASS Form1
   local lOk:=.t.
   local i, nr, nLang

   nLang := ::ComboLang:GetCurSel()
   
   if nLang == 1
      hb_SetCodePage( "IT850" )
   elseif nLang == 2
      hb_SetCodePage( "HU852" )
   elseif nLang == 3
      hb_SetCodePage( "ITISO" )
   elseif nLang == 4
      hb_SetCodePage( "HUISO" )
   elseif nLang == 5
      hb_SetCodePage( "HUWIN" )
   elseif nLang == 6
      hb_SetCodePage( "ITWIN" )
   endif

   with object ::MyTable
      :FileName := ::cFileIn
      try
         :Open()
      catch
         lOk:=.f.
      end
   end
   if !lOk
      ::MessageBox( "Input file open error", "dbf2xml", MB_ICONHAND )
      return .f.
   endif
   
   ::Stru := ::MyTable:Struct()
   nr := len(::Stru)
   ::Pos := array( nr )
   for i:=1 to nr
      ::Pos[i] := i
   next   
RETURN lOk
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnLoad( Sender ) CLASS Form1
   ::lTerminate := .F.
   ::lCanceled := .F.
   ::hOut := NIL
   ::EditIn:Caption := ""
   ::EditOut:Caption := ""
   with object ::ComboLang
      :AddItem( "850" )
      :AddItem( "852" )
      :AddItem( "ISO-8859-1" )
      :AddItem( "ISO-8859-2" )
      :AddItem( "Windows-1250" )
      :AddItem( "Windows-1252" )
      :SetCurSel(1)
   end
   with object ::ComboPos
      :AddItem( "10" )
      :AddItem( "20" )
      :AddItem( "40" )
      :AddItem( "80" )
      :SetCurSel(1)
   end
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonTest_OnClick( Sender ) CLASS Form1
   local nStep:=val(::ComboPos:GetSelString())
   if !( ::oTest == NIL )
      ::oTest:Close()
   endif
   ::MyRoot( nStep, .T. )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonDoc_OnClick( Sender ) CLASS Form1
   with object ::MySaveFile
      :DefaultExt := "xml"
      :Filter := "XML file (*.xml)|*.xml"
      :Title := "Select the output XML file:"
      :FileName := ""
      if :Show() .and. !empty( :FileName )
         ::EditOut:Caption := :FileName
      endif
   end   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD TestUI( lTest ) CLASS Form1
   local lOk:=.t., cFile := alltrim( ::EditIn:Caption )
   
   if empty( cFile )
      ::MessageBox( "Please select an input file", "dbf2xml", MB_ICONEXCLAMATION )
      lOk := .f.
   else
      if !file( cFile )
         ::MessageBox( "Input file does not exist", "dbf2xml", MB_ICONEXCLAMATION )
         lOk := .f.
      else
         ::cFileIn := cFile
      endif      
   endif
   
   cFile := alltrim( ::EditOut:Caption )
   if empty( cFile )
      ::MessageBox( "Please select an output file", "dbf2xml", MB_ICONEXCLAMATION )
      lOk := .f.
   else
      if lTest .and. file( cFile )
         ::MessageBox( "The output file already exists", "dbf2xml", MB_ICONEXCLAMATION )
         lOk := .f.
      else
         ::cFileOut := cFile
      endif      
   endif
RETURN lOk
//----------------------------------------------------------------------------------------------------//
METHOD OutOpen() CLASS Form1
   local lOk:=.t.
   ::hOut := fcreate( ::cFileOut, FC_NORMAL )
   if ::hOut < 1
      lOk := .f.
      ::hOut := NIL
      ::MessageBox( "Output file creation error "+ltrim(str( ferror() )), "dbf2xml", MB_ICONHAND )
   endif
RETURN lOk
//----------------------------------------------------------------------------------------------------//
METHOD OutWrite( cString, nLen ) CLASS Form1
   local lOk:=.t., nWritten:=-1
   if ::hOut == NIL
      return .t.
   endif
   nWritten := fwrite( ::hOut, cString, nLen )
   if nWritten < nLen
      lOk := .f.
      ::MessageBox( "Output file writing error "+ltrim(str( ferror() )), "dbf2xml", MB_ICONHAND )
   endif
RETURN lOk
//----------------------------------------------------------------------------------------------------//
METHOD OutClose() CLASS Form1
   local lOk:=.t.
   if ::hOut == NIL
      return .t.
   endif
   if !fclose( ::hOut )
      lOk := .f.
      ::MessageBox( "Output file closing error "+ltrim(str( ferror() )), "dbf2xml", MB_ICONHAND )
   endif
   ::hOut := NIL
RETURN lOk
//----------------------------------------------------------------------------------------------------//
METHOD MyRoot( nStep, lTest ) CLASS Form1
   local nChunk:=64*1024
   local lOk, lCloseIn, lCloseOut, nTime

   if !::TestUI(lTest) .or. !::OpenTable()
      return Self
   endif
   if !::OutOpen()
      return Self
   endif

   FormProggy( ::this )
   with object ::oProggy:Proggy
      :MaxRange := if( lTest, nStep, ::MyTable:RecCount() )
      :Step := nStep
   end
   
   nTime := seconds()
   lOk := ::MyConvert( nStep, nChunk, lTest )
   nTime := seconds() - nTime
   
   if !( ::oProggy == NIL )
      ::oProggy:Close()
   endif
   
   lCloseOut := if( ::OutClose(), .T., .F. )   
   lCloseIn := .F.
   try
      ::MyTable:Close()
      lCloseIn := .T.
   catch
      ::MessageBox( "Error while closing the input file", "dbf2xml", MB_ICONHAND )
   end     
   
   ::Application:Yield()
   if !lTest .and. lOk
      ::MessageBox( "Elapsed time:  "+ ltrim(str(nTime,10,2)) + "  seconds", "dbf2xml", MB_ICONINFORMATION )
   endif
   if lTest .and. lOk
      Form2(::this)
   endif      
   
   if lCloseIn .and. lCloseOut .and. ::lCanceled
      ::lCanceled := .F.
      return Self
   endif
   
   if !lOk .or. !lCloseIn .or. !lCloseOut
      ::lTerminate := .T.
      ::Close()
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnSysCommand( Sender ) CLASS Form1
   if ::wParam==SC_CLOSE .and. !::lTerminate
      ::MessageBox( "Please use the 'Close' button for leaving the application!", "dbf2xml", MB_ICONEXCLAMATION )
      return 0
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonTerminate_OnClick( Sender ) CLASS Form1
   ::lTerminate := .T.
   ::Close()
RETURN Self