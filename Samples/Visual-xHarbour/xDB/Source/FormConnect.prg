GLOBAL EXTERNAL oApp, oErr, oIni
GLOBAL EXTERNAL nCursorCalc, nCursorNormal
GLOBAL EXTERNAL cEngine, cSchema, hConnection, oSQL
GLOBAL EXTERNAL cDefaultEngine, lKeepDeleted
GLOBAL EXTERNAL aConString
GLOBAL EXTERNAL nConStringMax, nConStringSel

#include "vxh.ch"
#include "FormConnect.xfm"
#include "sqlrdd.ch"
#include "mysql.ch"
#include "firebird.ch"
//---------------------------------------- End of system code ----------------------------------------//



function myConnect(nTrials, nPause, nServerType, cConnectString)   
// custom connection function; nTrials and nPause can be set
// depending on the database engine's environment on the hosting server

   local i:=0, nResp:=0, nHand:=0 
   
   do while .t.      
      
      for i=1 to nTrials
         #ifdef XDB_SQLRDD
            try
               nHand:=SR_AddConnection(nServerType, cConnectString)
            catch oErr
               nHand:=-1
            end
         #endif
         if nHand <= 0
            millisec(nPause)
         else
            exit
         endif
      next
      
      if nHand <= 0
         nResp:=messagebox(,"Connection failure","",;
                             MB_ICONEXCLAMATION|MB_RETRYCANCEL)
         if nResp <> 4
            exit
         endif
      else
         exit
      endif
      
   enddo
      
return (nHand)



//----------------------------------------------------------------------------------------------------//
METHOD FormConnect_OnLoad( Sender ) CLASS FormConnect
   ::BoxConString:Visible:=.f.
   ::LabelCon:Visible:=.f.
   ::RadioMySQL:SetState(if( cDefaultEngine=="MYSQL", BST_CHECKED, BST_UNCHECKED))
   ::RadioFB:SetState(if( cDefaultEngine=="FB", BST_CHECKED, BST_UNCHECKED))
   ::RadioMS:SetState(if( cDefaultEngine=="MS", BST_CHECKED, BST_UNCHECKED))
   ::CheckKeep:SetState(if(lKeepDeleted, BST_CHECKED, BST_UNCHECKED))
   ::myReadConString(Self)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonCancel_OnClick( Sender ) CLASS FormConnect
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonConnect_OnClick( Sender ) CLASS FormConnect

   local lOK:=.f., cString:=""
   local cHST:=alltrim(::EditHST:Caption)
   local cUID:=alltrim(::EditUID:Caption)
   local cPWD:=alltrim(::EditPWD:Caption)
   local cDTB:=alltrim(::EditDTB:Caption)
   local cPRT:=alltrim(::EditPRT:Caption)
   local cCHARSET:=alltrim(::EditCHARSET:Caption)

   ::Disable()
   cSchema:=cDTB
   hConnection:=NIL
   ::Cursor:=nCursorCalc
   
   if ::RadioMySQL:GetState() == BST_CHECKED   // MySQL
      cString:="HST="+cHST+";"+;
               "UID="+cUID+";"+;
               "PWD="+cPWD+";"+;
               "DTB="+cDTB+";"+;
               if( empty(cPRT), "", "PRT="+cPRT+";")
      #ifdef XDB_SQLRDD
         hConnection:=myConnect( 2, 200, CONNECT_MYSQL, cString)
      #endif   
   elseif ::RadioFB:GetState() == BST_CHECKED      // FireBird
      cString:="FIREBIRD="+cHST+";"+;
               "UID="+cUID+";"+;
               "PWD="+cPWD+";"+;
               if( empty(cCHARSET), "", "CHARSET="+cCHARSET+";")
      #ifdef XDB_SQLRDD
         hConnection:=myConnect( 2, 200, CONNECT_FIREBIRD, cString)
      #endif      
   elseif ::RadioMS:GetState() == BST_CHECKED      // MS SQL Server
      cString:="DSN="+cHST+";"+;
               "UID="+cUID+";"+;
               "PWD="+cPWD+";"+;
               "DTB="+cDTB+";"
      #ifdef XDB_SQLRDD
         hConnection:=myConnect( 2, 200, CONNECT_ODBC, cString)
      #endif   
   endif

   if hConnection > 0     // if connection OK
      #ifdef XDB_SQLRDD      
         oSQL:=SR_GetConnection(hConnection)   // get SQL object  
         SR_UseDeleteds(lKeepDeleted)     // set "set deleted" behaviour
      #endif
      ::myWriteConString(Self)  // register connection string in .ini file
   else
      cEngine:="NO"
   endif
   
   ::Enable()
   ::Cursor:=nCursorNormal
   
   if hConnection > 0
      ::Close()
   endif

RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD RadioMySQL_OnClick( Sender ) CLASS FormConnect
   ::Disable()
   ::myReadConString(Self)
   ::Enable()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD RadioFB_OnClick( Sender ) CLASS FormConnect
   ::Disable()
   ::myReadConString(Self)
   ::Enable()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myRefreshEdits( Sender ) CLASS FormConnect
// spreads connection string parameters in different EDIT fields   
   local i, aPar, cString
   nConStringSel:=Min(nConStringSel, nConStringMax)
   if nConStringSel<1           // no current connection string
      cString:=";;;;;"
   else
      cString:=alltrim(aConString[nConStringSel])
      if empty(cString)        // current connection string empty
         cString:=";;;;;"
      endif
   endif
   aPar:=HB_ATokens( cString, ";")     // spread it
   for i:=len(aPar)+1 to 5
      AADD( aPar, "")
   next
   ::EditHST:Caption:=aPar[1]
   ::EditUID:Caption:=aPar[2]
   ::EditPWD:Caption:=""
   if ::RadioMySQL:GetState() == BST_CHECKED
      ::LabelHost:Caption:="Host"
      ::EditDTB:Caption:=aPar[4]
      ::EditPRT:Caption:=aPar[5]
      ::EditCHARSET:Caption:=""
      ::EditDTB:Enabled:=.t.
      ::EditPRT:Enabled:=.t.
      ::EditCHARSET:Enabled:=.f.
   elseif ::RadioFB:GetState() == BST_CHECKED
      ::LabelHost:Caption:="Host"
      ::EditDTB:Caption:=""
      ::EditPRT:Caption:=""
      ::EditCHARSET:Caption:=aPar[4]
      ::EditDTB:Enabled:=.f.
      ::EditPRT:Enabled:=.f.
      ::EditCHARSET:Enabled:=.t.   
   elseif ::RadioMS:GetState() == BST_CHECKED
      ::LabelHost:Caption:="DSN"
      ::EditDTB:Caption:=aPar[4]
      ::EditPRT:Caption:=""
      ::EditCHARSET:Caption:=""
      ::EditDTB:Enabled:=.t.
      ::EditPRT:Enabled:=.F.
      ::EditCHARSET:Enabled:=.f.
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myRefreshBoxConString( Sender ) CLASS FormConnect
// refreshes the current conection string selection
   local i, nMax:=0
   for i:=nConStringMax to 1 step -1     // looks for last not empty string in list
      if !empty(alltrim(aConString[i]))
         nMax:=i
         exit
      endif
   next
   ::BoxConString:ResetContent()    // refills ComboBox with connectionstings
   for i:=1 to nMax
      ::BoxConString:AddString(aConString[i])
   next
   if nConStringSel > 0        // selects last current connection string, if applicable
      ::BoxConString:SetCurSel(nConStringSel)
   endif
// if it doesn't exist a completed connection string, it doesn't display the combobox   
   ::BoxConString:Visible:=if( nConStringSel>0, .t., .f.)
   ::LabelCon:Visible:=if( nConStringSel>0, .t., .f.)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myReadConString( Sender ) CLASS FormConnect
   local i, nMax, cServer   
   ::Disable()
   if ::RadioMySQL:GetState() == BST_CHECKED
      cServer:="MySQL"
   elseif ::RadioFB:GetState() == BST_CHECKED
      cServer:="FireBird"
   elseif ::RadioMS:GetState() == BST_CHECKED
      cServer:="MS"
   endif
// read last selected connection string from .ini file
   nConStringSel:=oIni:ReadNumber( cServer, "ConStringSel", 0)
   if nConStringSel > nConStringMax  // if error
      nConStringSel:=0
   endif
   for i:=1 to nConStringMax    // read all strings from .ini file
      aConString[i]:=alltrim(oIni:ReadString( cServer, "ConString"+strzero(i,2), "" ))
   next
   nMax:=0
   for i:=nConStringMax to 1 step -1   // look for last not empty string
      if !empty(aConString[i])
         nMax:=i
         exit
      endif
   next
   if nConStringSel == 0        // select a current string
      if nMax > 0
         nConStringSel:=1
      endif
   else
      if nMax < nConStringSel
         if nMax == 0
            nConStringSel:=0
         else
            nConStringSel:=1
         endif
      endif
   endif
// update the form
   ::myRefreshBoxConString(Self)
   ::myRefreshEdits(Self)
   ::Enable()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD myWriteConString( Sender ) CLASS FormConnect
   local i, nPos, cServer, cString
   
   ::Disable()   
// read last selected connection string from the form
   if ::RadioMySQL:GetState() == BST_CHECKED
      cServer:="MySQL"
      cEngine:="MYSQL"
      cString:=alltrim(::EditHST:Caption)+";"+;
               alltrim(::EditUID:Caption)+";;"+;
               alltrim(::EditDTB:Caption)+";"+;
               alltrim(::EditPRT:Caption)+";"
   elseif ::RadioFB:GetState() == BST_CHECKED
      cServer:="FireBird"
      cEngine:="FB"
      cString:=alltrim(::EditHST:Caption)+";"+;
               alltrim(::EditUID:Caption)+";;"+;
               alltrim(::EditCHARSET:Caption)+";;"
   elseif ::RadioMS:GetState() == BST_CHECKED
      cServer:="MS"
      cEngine:="MS"
      cString:=alltrim(::EditHST:Caption)+";"+;
               alltrim(::EditUID:Caption)+";;"+;
               alltrim(::EditDTB:Caption)+";;"
   endif
   
   if len(cString)==5 .and. cString==";;;;;"   // if empty, return
      ::Enable()
      return Self
   endif
   
   nPos:=0
   for i:=1 to nConStringMax  // check if it's already present in the combobox
      if len(cString)==len(aConString[i]) .and. cString==aConString[i]
         nPos:=i
         exit
      endif
   next
   if nPos==0      // if not, append to the combobox and select it as current string
      for i:=nConStringMax to 2 step -1
         aConString[i]:=aConString[i-1]
      next
      aConString[1]:=cString
      nConStringSel:=1
      ::myRefreshBoxConString(Self)
   endif
// write the last current selection in the .ini file
   oIni:WriteNumber( cServer, "ConStringSel", nConStringSel)
// if something has been changed, write all connection string info in the .ini file   
   if nPos==0 .or. cEngine<>cDefaultEngine .or. lKeepDeleted<>::CheckKeep:Checked()
      cDefaultEngine:=cEngine
      lKeepDeleted:=::CheckKeep:Checked()
      oIni:WriteString("Data", "DefaultEngine", cDefaultEngine)
      oIni:WriteLogical("Data", "KeepDeleted", lKeepDeleted)
      for i:=1 to nConStringMax
         oIni:WriteString( cServer, "ConString"+strzero(i,2), aConString[i])
      next
   endif
   
   ::Enable()   

RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD BoxConString_OnCBNSelEndOk( Sender ) CLASS FormConnect
   local cString:=alltrim(aConString[Sender:GetCurSel()])
   if empty(cString)
      ::MessageBox( "Please select a valid connection string", "Empty string", MB_ICONEXCLAMATION)
      return Self
   endif
   ::Disable()
   nConStringSel:=Sender:GetCurSel()
   ::myRefreshEdits(Self)
   ::Enable()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD RadioMS_OnClick( Sender ) CLASS FormConnect
   ::Disable()
   ::myReadConString(Self)
   ::Enable()      
RETURN Self