#include "vxh.ch"
#include "FormXml2Dbf.xfm"
#include "fileio.ch"
#include "hbxml.ch" 
//---------------------------------------- End of system code ----------------------------------------//

function CnvUtf8Char( cOld, cNew )
// cOld should be an UTF-8 byte sequence representing a single character   
   local nLen := len( cOld )
   if nLen>1 .and. nLen<4  // utf8 byte sequences of 4 bytes are not yet implemented
      cNew := hb_utf8tostr( cOld )
      if cNew == NIL
         cNew := "Failed to convert UTF-8 sequence: " + cOld
         return .f.
      endif
   else
      cNew := "Invalid UTF-8 sequence found: " + cOld
      return .f.
   endif
return .t.

//----------------------------------------------------------------------------------------------------//
METHOD FormXml2Dbf_OnCreate( Sender ) CLASS FormXml2Dbf
   ::FileMax := 1024*1024*8  // test mode
   ::FootLen := 1024*32  // sampling buffer for the closing tags
// sampling buffer - it should be at least 32, and it should not exceed ::FootLen
   ::HeadLen := 1024*32
   ::ChunkStruLen := 1024*64  // chunk length for structure exploring
   ::ChunkDataLen := 1024*32  // chunk length for data extraction
   ::KeyLen := 64  // it can be changed together with MtXml and MtDbf structure only
   ::SampleCaps := array(16) // caption texts for the sample grid
   ::SampleCols := 16 // number of columns in the sample grid - it should be modified together with ::MtSample
   ::SampleRows := 20 // maximum number of rows in the sample grid
   ::RexAttrAll := hb_regexcomp( "(?s)<[A-Za-z][-_:A-Za-z0-9]*[^>]*" )
   ::RexAttrTag := hb_regexcomp( "<[A-Za-z][-_:A-Za-z0-9]*\s" )
   ::RexAttrAttr := hb_regexcomp( "(?s)[\s][A-Za-z][-_:A-Za-z0-9]*=" )   
   ::RexBoolean := hb_regexcomp( "(?s)^[\W\s]*[TYJSOAIDtyjsoaid]" )
   ::RexDateAnsi := hb_regexcomp( "(?s)[\d]{8}[\D\s]*" )
   ::RexDateGuess := hb_regexcomp( "(?s)[\d]+[\D\s]" )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormXml2Dbf_OnLoad( Sender ) CLASS FormXml2Dbf
   ::GridDbf:Enabled := .f.
   ::ProggyBar:Visible := .f.
   ::EditXml:Caption := ""
   ::EditDbf:Caption := ""  
   ::FileXml := NIL
   ::FileDbf := NIL
   ::Status := "1"
   ::Error := ""
   ::RowTag := "."   
   ::Table := .f.  // TRUE when we have "table-alike" XML tree structure
   ::Cols := {=>}  // for holding the XML ROW structure
   ::OrigCols := NIL  // for holding a copy of the original and complete ::Cols
   ::MyInitData()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonProceed_OnClick( Sender ) CLASS FormXml2Dbf
// this is the READ button of the "Table Selection" group
   local lOk:=.t., i, j, cTag, cAttr, hAttr
   local nLen, nCols
   if ::Status > "2"
      return Self
   endif
   if ::FileXml==NIL
      ::MessageBox( "Please select an XML file for being loaded!", "dbf2xml", MB_OK|MB_ICONEXCLAMATION )
      return Self
   endif
   if ::Status > "1"
      ::MessageBox( "XML file already loaded!", "dbf2xml", MB_OK|MB_ICONEXCLAMATION )
      return Self
   endif

   ::Status := "r"
   lOk := ::MyWorkUp()   
   if !lOk
      ::Status := "1"
      alert( ::Error )
      return Self
   endif
   ::Status := "2"
 
   with object ::MtXml
   
      :Append()
// the first row is left empty - needed for UNSELECT DBF field      
      :Fields:TAG := ""
      :Fields:ATTR := ""
      :Fields:SELECTION := ""
      nCols := len( ::Cols )
      for i:=1 to nCols
         cTag := hgetkeyat( ::Cols, i ) 
         hAttr := hgetvalueat( ::Cols, i )
         nLen := len( hAttr )
         for j:=1 to nLen
            cAttr :=  hgetkeyat( hAttr, j )
            :Append()
            :Fields:TAG := cTag
            :Fields:ATTR := cAttr
            if cAttr == "__tdata"
               :Fields:SELECTION := cTag
            else
               :Fields:SELECTION := left( cTag + "  " + cAttr, ::KeyLen )
            endif
         next
      next
      
   end
   
   ::LabelTime:Caption := str(::Seconds)  // for tests only
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MySlimStru() CLASS FormXml2Dbf
// prepares the xml items selected as dbf columns   
   local i, j, nTot
   local cTag, cAttr, hAttr, nPos, cType, nLen, nDec
   
   with object ::MtDbf   // adding DBF structure info to ::Cols
      :GoTop()
      nTot := :RecCount()
      for i:=1 to nTot
         if !empty( :Fields:SELECTION )
            cTag  := :Fields:TAG
            cAttr := :Fields:ATTR           
            nPos := :Recno()
            cType := :Fields:TYPE
            nLen := :Fields:LEN
            nDec := :Fields:DEC
            ::Cols[cTag][cAttr] := { nPos, cType, nLen, nDec }
         endif
         :Skip()
      next
   end

   nTot := len( ::Cols )  // ::Cols will store only attributes requested by the user
   for i:=1 to nTot
      cTag := hgetkeyat( ::Cols, i )
      hAttr := hgetvalueat( ::Cols, i )
      nLen := len( hAttr )
      for j:=nLen to 1 step -1
         cAttr := hgetvalueat( hAttr, j )
         if cAttr == NIL
            hdelat( hAttr, j )
         endif
      next
      if len( hAttr ) < 1
         ::Cols[cTag] := NIL  
      endif
   next
   
   for i:= nTot to 1 step -1   // ::Cols will store only tags requested by the user
      cTag := hgetvalueat( ::Cols, i )
      if cTag == NIL
         hdelat( ::Cols, i )
      endif
   next
// at this point the ::Cols structure is slimmed down,
// interactive selections for DBF fields will be possible only after doing the import
RETURN NIL
//----------------------------------------------------------------------------------------------------//
METHOD MySampling() CLASS FormXml2Dbf
   
   local i, j, hand:=-1, nTot, nBuf, nRes, nStart, nLen
   local cBuf, cSample, cStart, cRegexp, cRes, cKey
   local aTags:={}

// this heuristic algorithm analyzes the footer zone of the XML file;
// it's not ready to identify complete row data inside <rowtag attr1="...", ... />

// collecting the proper sample string - cBuf
   ::RowTag := "."
   ::Table := .F.
   nTot := HB_FSize( ::FileXml )
   if nTot < 1
      ::Error := "file empty"
      return .f.
   endif
   
   if nTot > ::FileMax
      ::Error := "file too big"
      return .f.
   endif
   
   if nTot <= ::FootLen
      
      cBuf := memoread( ::FileXml )
      nBuf := len( cBuf )
      if nBuf < nTot
         ::Error := "memoread error"
         return .f.
      endif
      
      if nBuf <= ::HeadLen
         cStart := cBuf
      else
         cStart := substr( cBuf, 1, ::HeadLen )
      endif
      
   else
      
      hand := fopen( ::FileXml )
      if hand < 1
         ::Error := "fopen error"
         return .f.
      endif
      
      nBuf := ::HeadLen
      cStart := space( nBuf )
      nRes := fread( hand, @cStart, nBuf )
      if nRes < nBuf
         ::Error := "fread error - header zone"
         fclose( hand )
         return .f.
      endif

      nBuf := ::FootLen
      nRes := fseek( hand, nTot-nBuf, FS_SET )
      if nRes < nTot - nBuf
         ::Error := "fseek error"
         fclose( hand )
         return .f.
      endif
      
      cBuf := space( nBuf )
      nRes := fread( hand, @cBuf, nBuf )
      if nRes < nBuf
         ::Error := "fread error - footer zone"
         fclose( hand )
         return .f.
      endif
      
      fclose( hand )
      
   endif

// 
   if !::MyCheckDoc( @cStart )      
      return .f.
   endif

// generating the inverted sample string - cSample

   cSample := space( nBuf )
   for i:=1 to nBuf
      cSample[nBuf-i+1] := cBuf[i]
   next
   
// using cSample - collecting the names of closing tags of form </sometag> in array aTags;
// the algorithm stops when it finds the second occurence of one of the already collected tag names

// ::RowTag is set with the tag name having a second occurence,
// otherwise no table row can be delimited
   
   cRegexp := hb_RegexComp( "(?s)>[-_:A-Za-z0-9]*[A-Za-z]/<" )

   nStart := 0
   nLen := 0

   do while .t.
      
      cRes := hb_atx( cRegexp, @cSample, .t., @nStart, @nLen )
      
      if cRes==NIL
         exit
      else
         cKey := ""
         for i:=len(cRes)-2 to 2 step -1
            cKey+=cRes[i]
         next
         nRes := 0
         for i:=1 to len( aTags )
            if cKey==aTags[i] .and. len(cKey)==len(aTags[i])
               nRes := i
               exit
            endif
         next
         if nRes > 0
            ::RowTag := cKey
            asize( aTags, nRes )
            exit
         else
            aadd( aTags, cKey )
         endif
         nStart := nStart + nLen
         nLen := 0
      endif
      
   enddo

// in case ::RowTag is set, the rightmost tag in aTags occurs repeatedly in the XML;
// we are going to find the biggest repetitive subtree;
// this algorithm supposes THAT tag to be the ROW delimiter tag of our table

// when no repeated tag is found in the sample buffer,
// the first tag found after the root tag is set as ::RowTag

   if ::RowTag <> "."

      if len( aTags ) < 2
          ::Error := "Multipe root tag"
          return .f.         
      endif   
         
      nRes:=0
      for i:=1 to len(aTags)   // - 1
         cRegexp := "(?s)</" + aTags[i] + ">[\s]*<" +aTags[i]
         nStart := 0
         nLen := 0
         cRes := hb_atx( cRegexp, @cBuf, .t., @nStart, @nLen )
         if cRes <> NIL
            nRes := i
            exit
         endif
      next
      
      if nRes > 0     // we have something like ...</myrow>   <myrow>...
         if nRes < 2
            ::Error := "Multiple root tag"
            return .f.
         endif
         ::RowTag := aTags[ nRes ]
         ::Table := .t.   // indicator set as "table-alike" file structure
      else
         ::RowTag := aTags[2]            
      endif
      
   else // no repeated tag in sample buffer

      if len( aTags ) < 2
          ::Error := "Insufficient data in file"
          return .f.         
      endif   
      ::RowTag := aTags[2]      
      
   endif

RETURN .t.
//----------------------------------------------------------------------------------------------------//
METHOD MyWorkup() CLASS FormXml2Dbf
   local i, j, lOk, nCols, nAttr, nMax, nKey, cTag, cAttr, hAttr
   local cFile := ::FileXml + ".txt", cOut := "", cEnd := chr(13) + chr(10)
   
   ::MyInitData()
   if !::MySampling()
      return .f.
   endif
// the buffer for this regex SHOULD contain at least a compele ROW   
   ::RexChunk := hb_RegexComp( "(?s)<(" + ::RowTag + ")[\s>].*(</\1>)" )
   ::LabelRow:Caption := ::RowTag  // for tests only

   if !::Table
      ::Error := "no table-alike structure"
      return .f.
   endif

   ::GroupBox1:Enabled := .f.
   ::GroupBox2:Enabled := .f.
   ::Seconds := Seconds()
   with object ::ProggyBar
      :Position := 0
      :Visible := .t.
      lOk := ::MyChunks( .f. )  // gathering candidates for DBF columns
      :Visible :=.f.
      :Position := 0
   end
   ::Seconds := Seconds() - ::Seconds()
   ::GroupBox1:Enabled := .t.
   ::GroupBox2:Enabled := .t.
   if !lOk
      return .f.
   endif

// checking tag and attribute lengths
   nMax := ::KeyLen
   nKey := ::KeyLen
   nCols := len( ::Cols )
   for i:=1 to nCols
      cTag := hgetkeyat( ::Cols, i ) 
      if len(cTag)>nKey .and. len(cTag)>nMax
         nMax := len(cTag)
      endif
      hAttr := hgetvalueat( ::Cols, i )
      nAttr := len( hAttr )
      for j:=1 to nAttr
         cAttr := hgetkeyat( hAttr, j ) 
         cOut := cOut + cTag + "  " + cAttr + cEnd
         if len(cAttr)>nKey .and. len(cAttr)>nMax
            nMax := len(cAttr)
         endif
      next
   next

   memowrit( cFile, cOut )  // write structure - for test purposes
   if nMax > ::KeyLen
      ::Error := "TAG or ATTRIBUTE name too long:"+str( nMax )
      return .f.
   endif
   
RETURN .t.
//----------------------------------------------------------------------------------------------------//
METHOD MyInitStru( lAll ) CLASS FormXml2Dbf
   local i, nLen
   if ::Status > "1"
      if lAll
         ::MtXml:Zap()
         ::MtSample:Zap()
         ::Cols := {=>}
         ::OrigCols := NIL
      else
         ::Cols := hclone( ::OrigCols )
      endif
      with object ::MtDbf
         nLen := :Reccount()
         :GoTop()
         for i:=1 to nLen
            :Fields:TAG := NIL
            :Fields:ATTR := NIL 
            :Fields:SELECTION := ""
            :Skip()
         next
      end
      ::GridDbf:Update()
   endif
   
   if lAll
      if !( ::FileXml == NIL )
         ::EditXml:Caption := ""
         ::FileXml := NIL
      endif   
   endif
RETURN .t.
//----------------------------------------------------------------------------------------------------//
METHOD MyWriteInDbf( cBuf ) CLASS FormXml2Dbf
// each set of rows coming from ::MyChunks is transformed in a TXmlDocument,
// from where the data is extracted to the DBF file
   local lOk:=.t., i, j, nLen, nIndex
   local oDoc, oCurrent, oNext
   local cSepa := chr(13) + chr(10), cTag, cAttr, cData
   local aTag, aAttr, hAttr

   cBuf := '<?xml version="1.0" ?>' + cSepa + '<x' + ::RowTag + '>' + cSepa + ;
           cBuf + '</x' + ::RowTag + '>' + cSepa

   oDoc := TXmlDocument( cBuf ) //HBXML_STYLE_NOESCAPE )
   if !( oDoc:nError==HBXML_ERROR_NONE ) 
      ::DocErrorInspect( oDoc )
      return .f. 
   endif 

// this algorithm handles only default tags ( no CDATA, no comments )

   oCurrent := oDoc:oRoot:NextInTree() // positioning on ROOT tag
   
   do while .t.
      
      oNext := oCurrent:NextInTree()
      if oNext == NIL // no more ROW data
         exit
      endif
      
      cTag := oNext:cName
      if cTag==::RowTag .and. len(cTag)==len(::RowTag) // read new ROW
         try
            dbappend()
         catch
            lOk := .f.
         end
         if !lOk
            ::Error := "Append error in DBF"
            exit
         endif
      endif
      
      oCurrent := oNext
      
      nIndex := hgetpos( ::Cols, cTag )
      if nIndex < 1  // tag not needed by the user
         loop
      endif
      
      cData := oCurrent:cData
      if valtype( cData ) == "C" // we have PCDATA
         nIndex := hgetpos( ::Cols[cTag], "__tdata" )
         if nIndex > 0   // data needed by the user
            aTag := hgetvalueat( ::Cols[cTag], nIndex ) 
            ::ProcessField( @cData, @aTag )            
         endif
      endif
      
      hAttr := oCurrent:aAttributes      
      nLen := len( hAttr )
      for i:=1 to nLen
         cAttr := hgetkeyat( hAttr, i )
         nIndex := hgetpos( ::Cols[cTag], cAttr )
         if nIndex < 1   // attribute not needed by the user
            loop
         endif
         aTag := hgetvalueat( ::Cols[cTag], nIndex )
         cData := hgetvalueat( hAttr, i ) // we have "attribute" data
         ::ProcessField( @cData, @aTag )
      next
      
   enddo

RETURN lOk
//----------------------------------------------------------------------------------------------------//
METHOD MyChunks( lGetData ) CLASS FormXml2Dbf

   local lStart:=.T., lNext:=.T., lOk:=.t.
   local hand, i, nBuf, nRes, nStart, nLen, nChunk
   local cBuf, cCoda:=NIL
   local cHeader:="", cFooter:="" // document header and footer zone - not yet used
   local cNext:=NIL, cBody:="", cChunk:="", cBufSamp:=NIL
   local cType := ::EncType

   if lGetData  // managing conversion errors
      if cType=="2" .and. ::EncError>0  // UTF-8 with conversion errors
          if ::EncError>::EncOk  // a number of erroneous utf8 byte sequences were found
             cType := "1"  // because the encoding declarationn is wrong, or the http setup was not thought for such files
          endif
      endif
   endif

   ::ProggyBar:StepIt()
   hand := fopen( ::FileXml )
   if hand < 1
      ::Error := "fopen error"
      return .f.
   endif

   nBuf := if( lGetData, ::ChunkDataLen, ::ChunkStruLen )
   cBuf := space( nBuf )

   i:= 0 // counter for chunks - ROW data is read in chunks
// this algorithm works correctly ONLY IF each chunk includes at least a ROW

   do while lNext

      ::Application:Yield()
      ::ProggyBar:StepIt()
      
      nRes := fread( hand, @cBuf, nBuf )
      if ferror() > 0
         ::Error := "fread error" + str( ferror() )
         fclose( hand )
         lOk := .f.
         exit
      endif
      
      if nRes < nBuf
         lNext := .F.  // end of XML file
         fclose( hand )
         nBuf := nRes
         if nBuf < 1
            exit
         endif
         cBuf := substr( cBuf, 1, nRes )
      endif
      
      switch ( cType )  // decoding is done here - if needed
         
      case "1"  // ANSI or OEM
         cChunk := cBuf
         if ::EncFrom > ::EncTo
            lOk := ::CnvAnsiOem( @cChunk, .t. )
         elseif ::EncFrom < ::EncTo
            lOk := ::CnvAnsiOem( @cChunk, .f. )
         endif
         exit
         
      case "2"  // UTF-8
         if cCoda == NIL
            cChunk := cBuf
         else   // previous buffer ended with an utf8 sequence
            cChunk := cCoda + cBuf
            cCoda := NIL
         endif
         lOk := ::CnvUtf8String( @cChunk, @cCoda, lNext, !lGetData )
         exit
         
      case "3"  // UCS-2LE
         exit
         
      case "4"  // UCS-2 BE
         exit
      
      end
      
      if !lOk  // conversion process not ok
         exit
      endif
      
      if !(cNext == NIL )           // adjusting chunk
         cChunk := cNext + cChunk
      endif
      
      nChunk := len( cChunk )
      nStart := 0
      nLen := 0
      cBody := hb_atx( ::RexChunk, @cChunk, .t., @nStart, @nLen )
      
      if cBody == NIL  // no match 
         if lStart
            cHeader := cChunk
         else
            cFooter := cChunk
         endif
         ::Error := "ROW not found - buffer too small"
         lok := .f.
         exit
      endif
      
      if lStart .and. nStart>1  // header zone
         cHeader := left( cChunk, nStart-1 ) 
      endif
      
      if nStart + nLen < nChunk   // we have data to be passed to the next cChunk
         cNext := substr( cChunk, nStart+nLen )   
      else
         cNext := NIL
      endif
      

// each set of rows is sent for further processing
      if lGetData   // extracting the data
         if !::MyWriteInDbf( @cBody )
            lOk := .f.
            exit
         endif
      else          // exploring the row structure
         if !::MyExplore( @cBody )
            lOk := .f.
            exit
         endif
      endif

      if lStart
         cBufSamp := cBody  // data for ::MtSample
         lStart := .f.
      endif
      i++
      
   enddo
   
   if !lOk
      return .f.
   endif
   lOk := ::FillMtSample( @cBufSamp )    // adding data to the sample grid

  if lOk .and. !lGetData   // creating a copy of the original, complete ::Cols
     ::OrigCols := hclone( ::Cols )
  endif

RETURN lOk
//----------------------------------------------------------------------------------------------------//
METHOD ButtonXml_OnClick( Sender ) CLASS FormXml2Dbf
// select XML file
   if ::Status > "2"
      return Self
   endif
   if ::Status>"1"
      if ::MessageBox( "Do you want to abandon the current XML ", "xml2dbf", MB_ICONQUESTION|MB_YESNO ) == IDNO
         return Self
      endif
   endif
   ::MyInitStru( .t. )
   ::Status := "1"
   
   with object ::MyOpenFile
      :DefaultExt := "xml"
      :InitialDirectory := curdrive() + ":\" + curdir()
      :Filter := "xml files (*.xml)|*.xml|All files (*.*)|*.*"
      :Title := "Select Input Table..."
      :FileName := ""
      :Show()
      if !empty( :FileName )
         ::EditXml:Caption := :FileName
         ::FileXml := :FileName
      endif
   end
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonDbf_OnClick( Sender ) CLASS FormXml2Dbf
// select DBF file   
   local lOk :=.t., i, nLen, aStru
   if ::Status > "2"
      return Self
   endif
   if !( ::FileDbf == NIL )
      ::EditDbf:Caption := ""
      ::FileDbf := NIL
      ::MtDbf:Zap()
      ::GridDbf:Update()
   endif
   
   with object ::MyOpenFile
      :DefaultExt := "dbf"
      :InitialDirectory := curdrive() + ":\" + curdir()
      :Filter := "dbf files (*.dbf)|*.dbf|All files (*.*)|*.*"
      :Title := "Select Output Table..."
      :FileName := ""
      :Show()
      if !empty( :FileName )
         ::EditDbf:Caption := :FileName
         ::FileDbf := :FileName
      endif
   end
   if ::FileDbf == NIL
      return Self
   endif
   
   try
      dbusearea( .f., , ::FileDbf, , .t., .t. )
   catch
      lOk := .f.
   end
   if !lOk
      alert( "DBF file not available" )
      ::EditDbf:Caption := ""
      ::FileDbf := NIL
      return Self
   endif

   aStru := dbstruct()
   nLen := len( aStru )
   for i:=1 to nLen
      with object ::MtDbf
         :Append()
         :Fields:NAME := aStru[i, 1]
         :Fields:TYPE := aStru[i, 2]
         :Fields:LEN := aStru[i, 3]
         :Fields:DEC := aStru[i, 4]
         :Fields:TAG := NIL
         :Fields:ATTR := NIL 
         :Fields:SELECTION := ""
      end
   next
   ::GridDbf:Update()
   
   dbclosearea()

RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonImport_OnClick( Sender ) CLASS FormXml2Dbf
// this is the "Import" button of the "Column Selection" group
   local i, j, cTag, cAttr, hAttr
   local lOk:=.t.
   if ::Status < "3"
      return Self
   endif

   if !::MyCheckSel()
      alert( ::Error )
      return Self
   endif
   
   try
      dbusearea( .f., , ::FileDbf, , .t., .f. )
   catch
      lOk := .f.
   end
   if !lOk
      alert( "DBF file not available" )
      return Self
   endif
   
   ::GridDbf:Enabled := .f.
   ::GridDbf:Refresh()
   ::MySlimStru() // if this fails there is a big problem, RTE is preferable for tests

   ::Status := "w"        // starting data import in .dbf
   ::GroupBox1:Enabled := .f.
   ::GroupBox2:Enabled := .f.
   ::Seconds := Seconds()
   with object ::Proggybar
      :Position := 0
      :Visible := .t.
      lOk := ::MyChunks( .t. ) // reading XML file and extracting data in DBF 
      :Visible := .f.
      :Position := 0
   end
   ::Seconds := Seconds() - ::Seconds
   ::Status := "4"
   dbclosearea()
   ::LabelTime:Caption := str(::Seconds)  // for tests only
   ::GroupBox1:Enabled := .t.
   ::GroupBox2:Enabled := .t.
   
   if !lOk  // we have a big problem
      alert( ::Error + " - Inconsistent data in DBF"  )
      ::Close()
   endif
   
   ::MyInitStru( .f. )
   ::Status := "2"
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyCheckSel() CLASS FormXml2Dbf
// prepares the xml items selected as dbf columns   
   local i, nCount:=0, nTot
   
   with object ::MtDbf  // count selected fields
      :GoTop()
      nTot := :RecCount()
      for i:=1 to nTot
         if !empty( :Fields:SELECTION )
            nCount++
         endif
         :Skip()
      next
   end
   if nCount < 1
      ::Error := "At least an XML item needs to be selected for starting the IMPORT"
      return .f.
   endif   
RETURN .T.
//----------------------------------------------------------------------------------------------------//
METHOD FormXml2Dbf_OnSysCommand( Sender ) CLASS FormXml2Dbf
   local cMess
   if ::wParam == SC_CLOSE
      if ::Status == "r"
         cMess := "Are you sure you want to stop reading the XML file?"
      elseif ::Status == "w"
         cMess := "Are you sure you want to stop writing data in the DBF file?"
      else
         cMess := "Are you sure you want to close the program?"
      endif
      if ::Status > "1"
         if ::MessageBox( cMess, "xml2dbf", MB_ICONQUESTION|MB_YESNO ) == IDNO
            return 0
         endif
      endif
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridDbf_OnClick( Sender ) CLASS FormXml2Dbf
   local nCol:=Sender:ColPos
   if nCol < 5
      return Self
   endif
   FormXmlStru( ::this )   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ProcessField( cData, aTag ) CLASS FormXml2Dbf
   local xData
   switch aTag[2]
   case "C"
      fieldput( aTag[1], cData )   
      exit
   case "N"
      fieldput( aTag[1], val(cData) )   
      exit
   case "D"
      xData := ::ProcessDate( @cData )
      if xData <> NIL
         fieldput( aTag[1], xData )            
      endif
      exit
   case "L"
      fieldput( aTag[1], if(  hb_atx(::RexBoolean, cData)==NIL, .f., .t. ) )
      exit
   end   
return NIL
//----------------------------------------------------------------------------------------------------//
METHOD ProcessDate( cData ) CLASS FormXml2Dbf

   local n1, n2, n3, nLen, cRes
   local aRes
   if empty( cData )
      return NIL
   endif

   cRes := hb_atx( ::RexDateAnsi, cData )
   if !( cRes == NIL )
      return ( stod( substr( cRes, 1, 8 ) ) )
   endif

   aRes := hb_regexall( ::RexDateGuess, cData+".", .t., .f., 0, 1, .t. )
   if aRes==NIL .or. len(aRes)<3
      return NIL
   endif

   nLen := len( aRes[1] ) - 1
   n1 := val( substr( aRes[1], 1, nlen ) )
   nLen := len( aRes[2] ) - 1
   n2 := val( substr( aRes[2], 1, nlen ) )
   nLen := len( aRes[3] ) - 1
   n3 := val( substr( aRes[3], 1, nlen ) )

   switch ::DateStyle
   case "A"  // American - mdy
      if n1>12 .or. n2>31 .or. n3>3000
         return NIL
      endif
      if n3 < 100
         n3+=2000
      endif
      return ( stod( strzero(n3,4) + strzero(n1,2 ) + strzero(n2,2) ) )
      exit
   case "G"   // German - dmy
      if n1>31 .or. n2>12 .or. n3>3000
         return NIL
      endif
      if n3 < 100
         n3+=2000
      endif
      return ( stod( strzero(n3,4) + strzero(n2,2 ) + strzero(n1,2) ) )
      exit
   case "J"       // Japanese & Hungarian - ymd
      if n1>3000 .or. n2>12 .or. n3>31
         return NIL
      endif
      if n1 < 100
         n1+=2000
      endif
      return ( stod( strzero(n1,4) + strzero(n2,2 ) + strzero(n3,2) ) )
      exit
   end

RETURN NIL
//----------------------------------------------------------------------------------------------------//
METHOD MyInitData() CLASS FormXml2Dbf
// encoding type "1"=ansi/oem, "2"=UTF-8, "3"=UCS-2LE, "4"=UCS-2BE   
   ::EncType := "1"   // default encoding type supposed to be ansi/oem
   ::EncFrom := 1252  // codepage of the XML file - when EncType = "1"
   ::EncTo := 850     // codepage of the DBF
   ::EncOk := 0
   ::EncError := 0
// "A"=American mdy; "G"=German dmy; "J"=Japanese ymd; - order of day/month/year
   ::DateStyle := "J" // used by MySQL and MS SQL server and others
// document types: "1"; "9" Office
   ::DocType := "1"
RETURN NIL
//----------------------------------------------------------------------------------------------------//
METHOD MyExplore( cDoc ) CLASS FormXml2Dbf
// cDoc is coming from ::MyChunks
   local cBuf, cTag, cAttr, cKey
   local i, j, nKey, nPos, nRes, nAttr
   local aRes, aAttr

   aRes := hb_regexall( ::RexAttrAll, @cDoc, .t., .f., 0, 1, .t. )
   if aRes == NIL  // no tags found
      return .t.
   endif
   
   nRes := len(aRes)
   for i:=1 to nRes
      
      cBuf := aRes[i]
      nPos := len( cBuf )
      if cBuf[nPos] == "/"  // some tags could have this format <TAG.../>
         cBuf := substr( cBuf, 1, nPos-1 )
      endif
      cTag := hb_atx( ::RexAttrTag, @cBuf, .t. )
      if cTag == NIL  // no attribute found
         cKey := substr( cBuf, 2 )
         nPos := hgetpos( ::Cols, cKey )
         if nPos < 1  // new tag found
            ::Cols[cKey] := { "__tdata"=>NIL }
         endif
         loop
      endif     
      
      nPos:=len(cTag)-2
      cTag := substr( cTag, 2, nPos )      
      nPos := hgetpos( ::Cols, cTag )
      if nPos < 1  // new tag found
         ::Cols[cTag] := { "__tdata"=>NIL }
      endif
      
      aAttr := hb_regexall( ::RexAttrAttr, @cBuf, .t., .f., 0, 1, .t. )
      if aAttr == NIL  // no valid attribute found
         loop
      endif
      
      nAttr := len( aAttr )
      for j:=1 to nAttr
         
         nPos := len( aAttr[j] ) - 2
         cAttr := substr( aAttr[j], 2, nPos )
         nPos := hgetpos( ::Cols[cTag], cAttr )
         if nPos < 1    // new attribute found
            ::Cols[cTag][cAttr] := NIL
         endif

      next
      
   next

RETURN .t.
//----------------------------------------------------------------------------------------------------//
METHOD MyCheckDoc( cBuf ) CLASS FormXml2Dbf
   local cBomutf8 := chr(239) + chr(187) + chr(191)
   local cBombe := chr(254) + chr(255)
   local cBomle := chr(255) + chr(254)
   local i, j, nBuf, nSampling:=32, nLen, n1, n2
   local cSample, cRes, cString, cType:="0" // unidentified encoding
   local cRexXml := "(?is)<?xml[\s]+[^>]*"
   local cRexEnc := '(?is)encoding="[^"]+'
   
   nBuf := len( cBuf )
   if nBuf < nSampling
      ::Error := "Header zone too short"
      return .f.
   endif
   
   if substr( cBuf, 1, 3 ) == cBomutf8
      cType := "2"
      cSample := substr( cBuf, 4, 512 )
   elseif substr( cBuf, 1, 2 ) == cBomle
      cType := "3"
      cSample := substr( cBuf, 3, 512 )
   elseif substr( cBuf, 1, 2 ) == cBombe
      cType := "4"
      cSample := substr( cBuf, 512 )
   else   // trying to identify the document encoding
      cSample := substr( cBuf, 1, nSampling )
      nLen := nSampling/2
      n1 := 0
      n2 := 0
      for i:=1 to nLen
         if cSample[2*i-1] == chr(0)
            n1++
         endif
         if cSample[2*i] == chr(0)
            n2++
         endif
      next
      if n1==nLen .and. n2==nLen
         ::Error := "File starts with NULL characters - unknown encoding"
         return .f.
      endif
      if n1==nLen .or. n2==nLen
         cType := if( n1==nLen, "3", "4" )   // we have widechars
      endif
      cSample := substr( cBuf, 1, 512 )
   endif
// now if cType is still "0", then it might be UTF-8 or ansi/oem or malformed widechar
   if cType > "2"
      ::Error := "UCS-2 or UTF-16 not yet supported"
      return .f.
   endif
   
   cSample := lower( cSample ) // for better search results
   cRes := hb_atx( cRexXml, cSample, .f. )
   
   if !( cRes == NIL )  // we have an XML tag, reading the "encoding" attribute
      
      cString := cRes
      cRes := hb_atx( cRexEnc, cString, .f. )
      
      if !( cRes == NIL )  // encoding attribute value exists
         cRes := substr( cRes, 11 )
         // widechar      
         if at("utf-16", cRes)>0 .or. at("utf-32", cRes)>0 .or. ;
            at("ucs-2", cRes)>0 .or. at("ucs-4", cRes)>0
            ::Error := "Encoding - " + cRes + " - not yet supported"
            return .f.
         endif
         // UTF-8
         if at("utf-8", cRes)>0 .or. at("utf_8", cRes)>0 .or. at("utf8", cRes)>0
            cType := if( cType=="0", "2", cType )
         endif
         // now cType should be "0" or "2"      
         // Western
         if at("1252", cRes)>0 .or. at("8859-1", cRes)>0 .or. at("latin1", cRes)>0
            cType := if( cType=="0", "1", "?" )
         endif
         // Central European  
         if at("1250", cRes)>0 .or. at("8859-2", cRes)>0
            ::EncFrom := 1250
            ::EncTo := 852
            cType := if( cType=="0", "1", "?" )
         endif
         // Russian      
         if at("1251", cRes)>0
            ::EncFrom := 1251
            ::EncTo := 866
            cType := if( cType=="0", "1", "?" )
         endif
         
      else  // encoding attribute not found
         
         ::EncType := "2"  // supposing to be UTF-8
         
      endif
   
      if cType == "?"
         ::Error := "The document has UTF-8 BOM and/or wrong ENCODING attribute" + cRes
         return .f.
      endif
   
      if cType == "2" // UTF-8
         ::EncType := "2"
      endif
      
   else // xml tag not found - now cType might be "0" or "2"
      
      ::EncType := "2" // supposing to be UTF-8

   endif  // end of analyzing the xml tag
   
// now we have ansi or utf8 data, and trying to find some specific patterns, which are ascii,
// so the encoding for these searches shouldn't matter, while it's not widechar

   if at("<?mso-application", cSample) > 0  // Microsoft Office XML uses WordML or SpreadsheetML
      ::DocType := "9"
      ::Error := "Microsoft Office is not supported"
      return .f.
   endif

RETURN .t.
//----------------------------------------------------------------------------------------------------//
METHOD ButtonCancel_OnClick( Sender ) CLASS FormXml2Dbf
// Go back to Files
   if ::Status < "3"
      return Self
   endif
   if ::Status >= "3"
      ::GridDbf:Enabled := .f.
      ::GridDbf:Refresh()
      ::Status := "2"
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD CnvUtf8String( cOld, cCoda, lCoda, lRead ) CLASS FormXml2Dbf
// utf8 byte sequence : 1 cStarter byte  and 1-2 cFollower bytes
   local cFollower:=chr(128), cStarter:=chr(192)
   local cNew, cCurr:=" ", cUtf, cText:=" "
   local cEncDummy:=chr(32) // symbol added instead of invalid utf8 byte sequences
   local lUtf:=.f.    // initially there is no pending utf8 byte sequence
   local lOk:=.t.     // conversion process ok
   local lIgnore:=.t. // conversion errors are ignored
   local i, nOld, nNew:=0, n1:=0, n2:=0, nEncOk:=0, nEncError:=0
   
   nOld := len( cOld )
   cNew := replicate( chr(0), nOld )
   
   for i:=1 to nOld
      
      if cOld[i] < cFollower  // plain ascii
         
         if lUtf // we have a pending ut8 byte sequence
            if CnvUtf8Char( substr( cOld, n1, n2 ), @cText )
               if lRead
                  nEncOk++
               endif
               nNew++
               cNew[nNew] := cText  // adding the converted character
               lUtf := .f.
            else
               if lIgnore
                  if lRead
                     nEncError++
                  endif
                  nNew++
                  cNew[nNew] := cEncDummy
                  lUtf:=.f.
               else
                  ::Error := cText  // error message
                  lOk :=.f.
                  exit
               endif
            endif
         endif
         nNew++
         cNew[nNew] := cOld[i]  // adding the plain ascii character
         
      elseif cOld[i] < cStarter // utf8 byte sequence is continuing
         
         if lUtf
            n2++
         else
            if lIgnore
               if lRead
                  nEncError++
               endif
               nNew++
               cNew[nNew] := cEncDummy
            else
               ::Error := "Invalid UTF-8 sequence found: " + cOld[i]
               lOk := .f.
               exit
            endif
         endif
         
      else // utf8 byte sequence is starting

         if lUtf  // we have a pending utf8 byte sequence
            if CnvUtf8Char( substr( cOld, n1, n2 ), @cText )
               if lRead
                  nEncOk++
               endif
               nNew++
               cNew[nNew] := cText  // adding the converted character
               n1 := i   // mark new pending utf8 byte sequence
               n2 := 1
            else
               if lIgnore
                  if lRead
                     nEncError++
                  endif
                  nNew++
                  cNew[nNew] := cEncDummy
                  n1 := i
                  n2 := 1
               else
                  ::Error := cText
                  lOk :=.f.
                  exit
               endif
            endif
         else
            n1 := i      // mark new pending utf8 byte sequence
            n2 := 1
            lUtf := .t.
         endif
         
      endif
      
   next
   
   if !lOk
      return .f.
   endif

   if lUtf // we have a pending utf8 byte sequence
      if lCoda  // send it back - maybe it continues in the next chunk
         cCoda := substr( cOld, n1, n2 )
      else      // convert it - we have no more chunks
         if CnvUtf8Char( substr( cOld, n1, n2 ), @cText  )
            if lRead
               nEncOk++
            endif
            nNew++
            cNew[nNew] := cText   // adding the converted character
         else
            if lIgnore
               if lRead
                  nEncError++
               endif
               nNew++
               cNew[nNew] := cEncDummy
            else
               ::Error := cText
               return .f.
            endif
         endif
      endif
   endif
   
   cOld := substr( cNew, 1, nNew )  // send back the converted chunk
   if lRead
      ::EncOk += nEncOk
      ::EncError += nEncError
   endif

RETURN .t.
//----------------------------------------------------------------------------------------------------//
METHOD CnvAnsiOem( cBuf, lToOem ) CLASS FormXml2Dbf
// now this conversion considers the desktop's default OEM and ANSI codepages
// this can be changed by using hb_translate(...)
   local i, cBase:=chr(127), nTot:=len(cBuf)
   for i:=1 to nTot
      if cBuf[i] > cBase
         cBuf[i] := if( lToOem, hb_ansitooem(cBuf[i]), hb_oemtoansi(cBuf[i]) )
      endif
   next
RETURN .t.
//----------------------------------------------------------------------------------------------------//
METHOD ButtonShow_OnClick( Sender ) CLASS FormXml2Dbf
// Show XML data   
   if ::Status < "2"
      ::MessageBox( "An XML file needs to be selected and loaded!", "xml2dbf", MB_OK|MB_ICONEXCLAMATION )
      return Self
   endif
   FormXmlData( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonNext_OnClick( Sender ) CLASS FormXml2Dbf
// go to Select Columns   
   if ::Status > "2"
      return Self
   endif
   if ::FileXml==NIL .or. ::FileDbf==NIL
      ::MessageBox( "Please select both files!", "xml2dbf", MB_OK|MB_ICONEXCLAMATION )
      return Self
   endif
   if ::Status < "2"
      ::MessageBox( "Please load the XML file!", "xml2dbf", MB_OK|MB_ICONEXCLAMATION )
      return Self
   endif
   ::GridDbf:Enabled := .t.
   ::GridDbf:Refresh()
   ::Status := "3"
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FillMtSample( cBuf ) CLASS FormXml2Dbf
// extracts sample XML data from cBuf - coming from MyChunks
   local nCols := ::SampleCols
   local nWidth := 20 // column width in characters
   local nRows := ::SampleRows
   local lOk:=.t., i, j, nLen, nIndex, nTot, nPos
   local oDoc, oCurrent, oNext
   local cSepa := chr(13) + chr(10), cTag, cAttr, cData
   local aAttr, hAttr, hList:={=>}
   
   for i:=1 to nCols
      ::SampleCaps[i] := "."
   next
   
   nLen:=len( ::Cols )
   nIndex:=0
   for i:=1 to nLen
      cTag := hgetkeyat( ::Cols, i )
      hAttr := hgetvalueat( ::Cols, i )
      hList[cTag] := {=>}
      nTot := len( hAttr )
      for j:=1 to nTot
         nIndex++
         if nIndex > nCols
            exit
         endif
         cAttr := hgetkeyat( hAttr, j )
         hList[cTag][cAttr]:=nIndex
         ::SampleCaps[nIndex] := left( cTag+if( cAttr=="__tdata", "", " "+cAttr ) , 20 )
      next
      if nIndex > nCols
         exit
      endif
   next
   nCols := if( nIndex>nCols, nCols, nIndex ) // current number of columns

   cBuf := '<?xml version="1.0" ?>' + cSepa + '<x' + ::RowTag + '>' + cSepa + ;
           cBuf + '</x' + ::RowTag + '>' + cSepa

   oDoc := TXmlDocument( cBuf )
   if !( oDoc:nError==HBXML_ERROR_NONE ) 
//memowrit( "xy.txt", cBuf ) 
      ::DocErrorInspect( oDoc )
      return .f. 
   endif 

   oCurrent := oDoc:oRoot:NextInTree() // positioning on ROOT tag
   nTot := 0
   ::ProggyBar:StepIt()   
   
   do while .t.
      
      oNext := oCurrent:NextInTree()
      if oNext == NIL // no more ROW data
         exit
      endif
      
      cTag := oNext:cName
      if cTag==::RowTag .and. len(cTag)==len(::RowTag) // read new ROW
         nTot++
         if nTot > nRows
            exit
         endif
         ::MtSample:Append()
      endif
      
      oCurrent := oNext
      
      nIndex := hgetpos( hList, cTag )
      if nIndex < 1  // tag not needed here
         loop
      endif
      
      cData := oCurrent:cData
      if valtype( cData ) == "C" // we have PCDATA
         nIndex := hgetpos( hList[cTag], "__tdata" )
         if nIndex > 0   // data needed by the user
            nPos := hgetvalueat( hList[cTag], nIndex ) 
            ::MtSample:Table[nTot, nPos] := left( cData, nWidth )
         endif
      endif
      
      hAttr := oCurrent:aAttributes      
      nLen := len( hAttr )
      for i:=1 to nLen
         cAttr := hgetkeyat( hAttr, i )
         nIndex := hgetpos( hList[cTag], cAttr )
         if nIndex < 1   // attribute not needed by the user
            loop
         endif
         nPos := hgetvalueat( hList[cTag], nIndex )
         cData := hgetvalueat( hAttr, i ) // we have "attribute" data
         ::MtSample:Table[nTot, nPos] := left( cData, nWidth )         
      next
      
   enddo

   ::MtSample:GoTop()
   ::ProggyBar:StepIt()   
   
RETURN .t.
//----------------------------------------------------------------------------------------------------//
METHOD DocErrorInspect( oDoc ) CLASS FormXml2Dbf
   local cMess := hb_XmlErrorDesc( oDoc:nError )
   local cName, cLine
   with object oDoc:oErrorNode
      cName := " -- tag name: " + :cName
      cLine := " at line:" + str( :nBeginLine )
   end
   ::Error := "XML parser error " + cMess + cName + cLine
RETURN NIL
