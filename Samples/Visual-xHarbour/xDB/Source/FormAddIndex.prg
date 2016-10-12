GLOBAL EXTERNAL oApp, oChild, oErr, oTable
GLOBAL EXTERNAL cIniPath, cIndexName
GLOBAL EXTERNAL lShowd, lOrderActive
GLOBAL EXTERNAL lShared, lStatistics
GLOBAL EXTERNAL cDriver, hDriver
GLOBAL EXTERNAL aIndex
GLOBAL EXTERNAL cFontFace, cFontSize
GLOBAL EXTERNAL cBackColor, cForeColor, cHighlightColor, cHighlightTextColor
GLOBAL EXTERNAL hFontFace, hFontSize, hBack, hFore, hHighlight, hHighlightText


#include "vxh.ch"
#include "FormAddIndex.xfm"
#include "dbinfo.ch"
//---------------------------------------- End of system code ----------------------------------------//

function RemakeAIndex()
   
   local i, nL, nPos
   local cBagName, cKeyType, cKeyName:="", cUnique
   local cKeyExpr, cKeyForExpr, cWholeBagName
   local nOrders:=DBorderInfo( DBOI_ORDERCOUNT )

   ASize( aIndex, 1)
   if nOrders < 1
      return 1
   endif
   
   for i:=1 to nOrders
      cWholeBagName:=DBOrderInfo( DBOI_FULLPATH, , i )
      nL:=len( cWholeBagName )
      nPos:=RAt( "\", cWholeBagName )
      cBagName:=if( nPos>0, Right( cWholeBagName, nL-nPos ), cWholeBagName )
      cKeyName:=DBOrderInfo( DBOI_NAME, , i  )
      cUnique:=if( DBOrderInfo( DBOI_UNIQUE, , i), "Yes", "No")
      cKeyExpr:=DBOrderInfo( DBOI_EXPRESSION, , i )
      cKeyType:=DBOrderInfo( DBOI_KEYTYPE, , i )
      cKeyForExpr:=DBOrderInfo( DBOI_CONDITION, , i )
      AADD( aIndex, { cBagName, cKeyType, cKeyName, cUnique, cKeyExpr, cKeyForExpr, cWholeBagName } )
   next
      
return (nOrders+1)


//----------------------------------------------------------------------------------------------------//
METHOD FormAddIndex_OnLoad( Sender ) CLASS FormAddIndex
   local npos, nl
   if aIndex <> NIL        // set up grid with indexes
      ::MTable:Table:=aIndex
      ::MTable:GoTop()
      with object ::Grid
         :Font:FaceName:=HFontFace[cFontFace]
         :Font:PointSize:=hFontSize[cFontSize]
         :BackColor:=hBack[cBackColor]
         :ForeColor:=hFore[cForeColor]
         :HighlightColor:=hHighlight[cHighlightColor]      
         :HighlightTextColor:=hHighlightText[cHighlightTextColor]      
         :Update()
      end
   endif   
   if lShared
      ::RadioReindex:Enabled:=.f.
      ::CheckPack:Enabled:=.f.
   endif
   if len(aIndex) <= 1
      ::RadioAdd:Enabled:=.f.
   endif
   ::PBar:SetRange({1, 50})
   ::PBar:SetStep(1)
   ::PBar:Visible:=.f.
   nl:=len(oTable:FileName)
   npos:=RAt( "\", oTable:FileName )
   cIniPath:=if( npos>0, right( oTable:FileName, nl-npos ), oTable:FileName )
   cIndexName:=""
   ::myCleanEdits(Self)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonNew_OnClick( Sender ) CLASS FormAddIndex
// get index file name
   local npos, cRDD:=right(hDriver[cDriver],3)
   local cExt:=if(cRDD=="ADT", "ADI", cRDD)
   local cFilter:=cDriver+" (*."+cExt+")|*."+cExt+"|All files ( *.* )|*.*"
   with object oApp:SaveFile
      :DefaultExt:=cExt
      :Filter:=cFilter
      :InitialDirectory:=cIniPath
      :Title:="New index location and name"
      :FileName:=""
      :Show()
      cIndexName:=:FileName
   end
   if empty(cIndexName)
      return Self
   endif
   
   if file(cIndexName)
      ::MessageBox("Index file already exists", "Warning", MB_ICONEXCLAMATION)
      cIndexName:=""
      return Self
   endif
   
   npos:=RAt("\", cIndexName)
   cIniPath:=if(nPos>0, Left(cIndexName, npos-1), "")   
   ::EditFile:Caption:=cIndexName

RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonOK_OnClick( Sender ) CLASS FormAddIndex
   
   local i, cKey, lOK, cType, cIndex, cUnique, cFor, cTypeFor
   local cName, nAddPos
   local nSeconds, nl, npos, cRDD:=right(hDriver[cDriver],3)
   local lNew:=if( ::RadioCreate:GetState()==BST_CHECKED, .t., .f.)
   local lAdd:=if( ::RadioAdd:GetState()==BST_CHECKED, .t., .f.)

   if lNew .or. lAdd   // create new index file or add index key

      if lNew .and. empty(alltrim(cIndexName))
            ::MessageBox( "Please select a location and file name for the new index", "Warning", MB_ICONEXCLAMATION)
            return Self
      endif
      
      cName:=alltrim(::EditKeyName:Caption)
      if !myCheckName(cName, 10)
         ::MessageBox( "Please complete the Key Name field with a valid name - the maximum length is 10 characters", "Key name not valid", MB_ICONEXCLAMATION)
         return Self
      endif

      if lAdd
         
         nl:=0
         nAddPos:=0
         for i:=2 to len(aIndex)
            if upper(aIndex[i,7])<>upper(cIndexName)
               loop
            endif
            if empty(alltrim(aIndex[i,3]))  
               nl:=i
               exit
            endif
            if upper(cName)==upper(aIndex[i,3])
               nAddPos:=i
               exit
            endif
         next
         
         if nl > 0
            ::MessageBox( "The index file doesn't support multiple keys", "Warning", MB_ICONEXCLAMATION)
            return Self
         endif
         if nAddPos > 0
            nl:=::MessageBox( "Are you sure you want to overwrite key '"+cName+"' ?", "Key Name already exists", MB_ICONQUESTION|MB_YESNO)
            if nl <> 6
               return Self
            endif
         endif
         
      endif

      cKey:=alltrim(::EditKey:Caption)
      if empty(cKey)
         ::MessageBox( "Please specify the key expression for the new index", "Warning", MB_ICONEXCLAMATION)
         return Self
      endif
      if len(cKey) > 255
         ::MessageBox( "The current key expression has "+ltrim(str(len(cKey)))+" characters - the maximum is 255", "Key too long", MB_ICONEXCLAMATION)
         return Self
      endif
      lOK:=.f.
      try
         cType:=valtype(&cKey)
         lOK:=.t.
      catch oErr
         myError(oApp, oErr)
      end
      if !lOK
         ::MessageBox( "Please specify a valid key expression for the new index", "Warning", MB_ICONEXCLAMATION)
         return Self
      endif
      
      cFor=alltrim(::EditFor:Caption)
      if !empty(cFor)
         if len(cFor) > 255
            ::MessageBox( "The current FOR expression has "+ltrim(str(len(cFor)))+" characters - the maximum is 255", "Expression too long", MB_ICONEXCLAMATION)
            return Self
         endif
         lOK:=.f.
         try
            cTypeFor=valtype(&cFor)
            lOK:=.t.
         catch oErr
            myError(oApp, oErr)
         end
         if !lOK .or. cTypeFor<>"L"
            ::MessageBox( "Please specify a valid FOR expression for the new index", "Warning", MB_ICONEXCLAMATION)
            return Self
         endif
      endif

      ::Disable()
                                  // proceed - create index
      nl:=len(cIndexName)
      npos:=RAt("\", cIndexName)
      cIndex:=if( npos>0, right(cIndexName, nl-npos), cIndexName)
      
      ::PBar:Visible:=.t.
      ::Timer1:Start()
      nSeconds:=Seconds()
      lOK:=.f.
      
      try
         oTable:CreateOrder( cIndexName, cName, cKey, cFor, , , , , , , , , ::CheckUnique:Checked())
         lOK:=.t.
         if cRDD=="ADT" .and. AdsGetLastError() > 0
            lOK:=.f.
         endif         
      catch oErr
         myError(oApp, oErr)
      end
      
      nSeconds:=mySeconds(nSeconds, Seconds())
      ::Timer1:Stop()
      ::PBar:Visible:=.f.
      
      if !lOK            // index creation failure
         ::Enable()
         return Self
      endif
              
      nl:=RemakeAIndex()
      oApp:BoxIndex:ResetContent()
      for i:=1 to nl   // refresh index list on the main form
         oApp:BoxIndex:AddString(aIndex[i,5])
      next
      oApp:BoxIndex:SetCurSel(nl)
      if nl > 1
         OrdSetFocus(nl-1)
      endif
      lOrderActive:=if( nl>1, .t., .f. )
      ::MTable:Table:=aIndex
      ::MTable:GoBottom()
      ::Grid:Update()
      oChild:Grid:Update()
      if nl > 1
         ::RadioAdd:Enabled:=.t.
      endif
      if lStatistics
         ::MessageBox( "Index creation has been terminated in "+ltrim(str(nSeconds))+"  seconds","", MB_ICONINFORMATION)
      endif
      ::Enable()
   
   elseif ::RadioReindex:GetState() == BST_CHECKED  // reinex opened indexes
      
      ::Disable()
                                  // proceed - reindex   
      ::PBar:Visible:=.t.
      ::Timer1:Start()
      nSeconds:=Seconds()
      lOK:=.f.
      
      try
         if ::CheckPack:Checked()
            pack
         else
            oTable:Reindex()
         endif
         lOK:=.t.
         if cRDD=="ADT" .and. AdsGetLastError() > 0
            lOK:=.f.
         endif         
      catch oErr
         myError(oApp, oErr)
      end
      
      nSeconds:=mySeconds( nSeconds, Seconds())
      ::Timer1:Stop()
      oChild:Grid:Update()      
      ::PBar:Visible:=.f.
      if lOK .and. lStatistics
         ::MessageBox( "Reindexing has been terminated in  "+ltrim(str(nSeconds))+"  seconds", "", MB_ICONINFORMATION)
      endif
      
      ::Enable()
      
   endif
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonCancel_OnClick( Sender ) CLASS FormAddIndex
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Timer1_OnTimeOut( Sender ) CLASS FormAddIndex
   ::PBar:StepIt()
   Sender:Start()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Button1_OnClick( Sender ) CLASS FormAddIndex
   FormShowStructure( ::this )
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD RadioAdd_OnClick( Sender ) CLASS FormAddIndex
   ::GroupBox1:Enabled:=.T.
   ::myCleanEdits(Self)
   ::EditFile:Enabled:=.f.
   ::ButtonNew:Enabled:=.f.
   ::CheckPack:Enabled:=.f.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD RadioReindex_OnClick( Sender ) CLASS FormAddIndex
   ::myCleanEdits(Self)
   ::EditFile:Enabled:=.T.
   ::ButtonNew:Enabled:=.T.   
   ::GroupBox1:Enabled:=.f.
   ::CheckPack:Enabled:=.t.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD RadioCreate_OnClick( Sender ) CLASS FormAddIndex
   ::myCleanEdits(Self)
   ::GroupBox1:Enabled:=.T.
   ::EditFile:Enabled:=.T.
   ::ButtonNew:Enabled:=.T.   
   ::CheckPack:Enabled:=.f.
   ::CheckPack:UnCheck()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myCleanEdits( Sender ) CLASS FormAddIndex
   ::EditFile:Caption:=""
   ::EditKeyName:Caption:=""
   ::EditKey:Caption:=""
   ::EditFor:Caption=""
   ::CheckUnique:SetState(BST_UNCHECKED)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Grid_OnLButtonUp( Sender ) CLASS FormAddIndex
   local i:=::Grid:DataSource:Recno()
   i:=if(i<2, 2, i)
   if ::RadioAdd:GetState() == BST_CHECKED
      cIndexName:=alltrim(aIndex[i,7])   
      ::EditFile:Caption:=cIndexName
   endif
RETURN Self