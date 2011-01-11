GLOBAL nBack, nFore
#include "vxh.ch"
#include "Form1.xfm"
#include "hbxml.ch" 
#define HKEY_CURRENT_USER 2
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnLoad( Sender ) CLASS Form1

   local cPath := "Software\Google\Google Desktop\API"
   local cKey := "search_url"
   ::cQueryUrl := GetRegistry( HKEY_CURRENT_USER, cPath, cKey )
   if ::cQueryUrl == NIL
      ::PanelTop:Enabled := .f.
      alert( "Google Desktop is not installed, or 'search_url' registry key not found" )
   endif
   nBack := RGB( 212, 255, 212 )
   nFore := RGB( 160, 32, 96 )
   ::lWords := NIL   
   ::EditKeys:Caption := ""
   ::CheckRun:Check()
   ::MyCleanUp()

RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonWord_OnClick( Sender ) CLASS Form1
   ::ButtonWord:Font:Bold := .t.
   ::ButtonPhrase:Font:Bold := .f.
   ::lWords := .t.
   ::MyCleanup()
   ::MySearch()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonPhrase_OnClick( Sender ) CLASS Form1
   ::ButtonWord:Font:Bold := .f.
   ::ButtonPhrase:Font:Bold := .t.
   ::lWords := .f.
   ::MyCleanup()
   ::MySearch()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonPrev_OnClick( Sender ) CLASS Form1
   if ::nFiles < 1
      return Self
   endif
   if ::nPage > 1
      ::nPage--
      ::MySearch()
   endif   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonNext_OnClick( Sender ) CLASS Form1
   if ::nFiles < 1
      return Self
   endif
   if ::nPage < ::nLimit
      ::nPage++
      ::MySearch()
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ComboGoto_OnCBNSelEndOk( Sender ) CLASS Form1
   local nNew := val( Sender:GetSelString() )
   if ::nFiles < 1
      return Self
   endif
   if nNew <> ::nPage
      ::nPage := nNew
      ::MySearch()
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD GridRes_OnLButtonUp( Sender ) CLASS Form1
   local nInd, nRow
   
   nRow:=::MemTabRes:Recno()
   if Sender:ColPos <> 2 // click over other than second column
      return Self
   endif
   if nRow%3 <> 1  // didn't click over the file's name
      return Self
   endif
   nInd := Ceiling( nRow/3 )
   
   if ::CheckRun:Checked() == .T.
      if ::MessageBox( "Do you want to open: "+::aResult[nInd, 1], "Googling...", MB_YESNO|MB_ICONQUESTION ) <> IDYES
         return Self
      endif
   endif
   
   ShellExecute( , "open", ::aResult[nInd ,1], , , SW_SHOW )
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyWebBrowser_DocumentComplete( Sender, pDisp, URL ) CLASS Form1
   ::Cursor := IDC_ARROW
   try
      ::cContent := Sender:Document:Body:InnerText
      ::MyWorkup()
      if !empty( ::cError )
         alert( ::cError )
         ::cError := ""
      endif
   catch
   end
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyCleanup() CLASS Form1
   
   ::cError := ""
   ::aResult := {}     // column1 = file name ; column2 = snippet
   ::cContent := ""    // xml formatted content
   ::nLimit := 5       // maximum number of pages
   ::nFiles := 0       // total number of result files found
   ::nPages := 0       // total number of result pages found
   ::nPage := 1        // current page number
   
   ::LabelTotal:Caption := "0"
   
   with object ::ComboPagelen
      if :GetCount() < 1
         :AddItem( "10" )
         :AddItem( "20" )
         :AddItem( "30" )
         :SetCurSel( 1 )
      endif
      ::nPageLen := val( :GetSelString() )  // number of files per page
   end
   
   ::ComboGoto():ResetContent()
   
   if len( ::MemTabRes:Table ) > 0
      ::MemTabRes:Zap()
      ::GridRes:Update()
   endif
   
RETURN NIL
//----------------------------------------------------------------------------------------------------//
METHOD MyWorkup() CLASS Form1
   
   local i, nCounter:=0, nVal, cCounter, cString
   local cCategory, cUrl, cSnippet, cTag
   local oDoc, oNode, oIterator, oCurrent 

   ::Cursor := IDC_WAIT
   ::aResult := {}
   ::ComboGoto:ResetContent()
   ::MemTabRes:Zap()
   ::GridRes:Update()
   if empty( ::cContent )
      ::Cursor := IDC_ARROW
      return NIL
   endif
   cString := StrTran( ::cContent, "<b>" )
   cString := StrTran( cString, "</b>" )

   oDoc:=TXmlDocument( cString, HBXML_STYLE_NOESCAPE ) 
   if !( oDoc:nError==HBXML_ERROR_NONE ) 
      ::cError := "XML file parsing error " + str(oDoc:nError)
      ::Cursor := IDC_ARROW
      return NIL 
   endif 

   oNode:=oDoc:findfirst( "results" ) 
   if oNode == NIL 
      ::cError := "XML file error - no RESULTS tag found"
      ::Cursor := IDC_ARROW
      return NIL 
   endif
   
   cCounter := oNode:getAttribute( "count" )
   nCounter := val( cCounter )
   ::nFiles := nCounter
   ::LabelTotal:Caption := ltrim( str( nCounter, 10 ) )
   if nCounter < 1
      ::nPages := 0
      ::nPage := 1
      ::Cursor := IDC_ARROW
      return NIL
   endif
   
   ::nPages := min( ::nLimit, Ceiling( ::nFiles/::nPagelen ) )
   with object ::ComboGoTo
      for i:=1 to ::nPages
         :AddItem( str(i, 3) )
      next
      if ::nPage <= ::nPages
         :SetCurSel( ::nPage )
      else
         :SetCurSel( ::nPages )
      endif
   end

   oNode:=oDoc:findfirst( "result" ) 

   do while oNode <> NIL

      cCategory := "" 
      cUrl := ""
      cSnippet := ""
      oIterator:=TXmlIterator():New( oNode ) 
      oCurrent:=oIterator:Next() 

      do while oCurrent <> NIL
         cTag := oCurrent:cName 
         if cTag == "category" 
            cCategory := oCurrent:cData
         elseif cTag == "url"   
            cUrl := alltrim( oCurrent:cData )
         elseif cTag == "snippet" 
            cSnippet := alltrim( oCurrent:cData )
         endif 
         oCurrent:=oIterator:Next() 
      enddo 

      if cCategory == "file"
         aadd( ::aResult, { cUrl, cSnippet } )
      endif
      oNode:=oDoc:findNext() 

   enddo
   
   nVal := ::nPagelen*( ::nPage - 1 )
   with object ::MemTabRes
      for i:=1 to len( ::aResult )
         :Append()
         :Fields:CRT := str( nVal + i, 5 )
         :Fields:FILE := ::aResult[i, 1]
         :Append()
         :Fields:CRT := ""
         :Fields:FILE := left( ::aResult[i, 2], 80 )
         :Append()
         :Fields:CRT := ""
         :Fields:FILE := substr( ::aResult[i, 2], 81, 80 )
      next
      :GoTop()
   end
   ::GridRes:Update()
   ::Cursor := IDC_ARROW
   
RETURN NIL
//----------------------------------------------------------------------------------------------------//
METHOD MySearch() CLASS Form1
   
   local cText := alltrim( ::EditKeys:Caption )
   local cOpt, nStart:=0
   
   if empty( cText )
      return NIL
   endif
   cText := StrTran( cText, " ", "+" )
   if !::lWords
      cText := "%22" + cText + "%22"
   endif
   if ::nFiles > 0   // an initial search has been made previously
      nStart := ::nPagelen*( ::nPage - 1 )
   endif
   cOpt := "&num=" + ltrim(str(::nPageLen,3)) + ;
            if( nStart>0, "&start=" + ltrim(str(nStart,5)), "" ) + ;
            "&format=xml"

   ::Cursor := IDC_WAIT
   ::MyWebBrowser:Url := ::cQueryUrl + cText + cOpt
   
RETURN NIL
//----------------------------------------------------------------------------------------------------//
METHOD GridColumn2_OnQueryBackColor( Sender ) CLASS Form1
   if ::MemTabRes:Recno()%3 == 1
      return nBack
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ComboPagelen_OnCBNSelEndOk( Sender ) CLASS Form1
   local nVal := val( Sender:GetSelString() )
   if ::nFiles < 1
      return Self
   endif
   if nVal <> ::nPagelen
      ::nPagelen := nVal
      ::nPage := 1
      ::MySearch()
   endif
RETURN Self