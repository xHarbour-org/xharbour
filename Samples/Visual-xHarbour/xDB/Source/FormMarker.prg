GLOBAL EXTERNAL oApp, oChild, oErr, oTable
GLOBAL EXTERNAL lMarker, lMarkerSec
GLOBAL EXTERNAL cMarkerCondition, cMarkerConditionSec
GLOBAL EXTERNAL aCols, aMarker

#include "vxh.ch"
#include "FormMarker.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD FormMarker_OnLoad( Sender ) CLASS FormMarker
   local i
   ::Listme:AddItem("n/a")
   for i:=1 to len(aCols)
      ::Listme:AddItem(aCols[i])
   next
   for i:=1 to len(aCols)
      ::Listme:SetSel(i, aMarker[i])
   next
   ::EditPri:Caption:=cMarkerCondition
   ::EditSec:Caption:=cMarkerConditionSec
   ::CheckMarker:SetState(if( lMarker, BST_CHECKED, BST_UNCHECKED))
   ::CheckMarkerSec:SetState(if( lMarkerSec, BST_CHECKED, BST_UNCHECKED))
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonOK_OnClick( Sender ) CLASS FormMarker
   local i, lOK, cVal, cValSec

   if lMarker .or. lMarkerSec  // checked at least one marker condition
      
      lOK:=.f.
      for i:=1 to len(aCols)  // is there any column selected for colorizing ?
         if ::Listme:GetSel(i) > 0
            lOK:=.t.
         endif
      next
      if !lOK
         ::MessageBox("Please select at least one column, or unckeck the marker activating boxes","Warning", MB_ICONEXCLAMATION)
         return Self
      endif
// memorize and check marker conditions found
      cMarkerCondition:=alltrim(::EditPri:Caption)
      cMarkerConditionSec:=alltrim(::EditSec:Caption)
      if (lMarker.and.empty(cMarkerCondition)) .or. (lMarkerSec.and.empty(cMarkerConditionSec))
         ::MessageBox( "Activated condition(s) cannot be empty", "Warning", MB_ICONEXCLAMATION)
         return Self
      endif
      if !lMarker .and. lMarkerSec
         ::MessageBox( "Please use 'Marker 1' when you need a single marker", "Warning", MB_ICONEXCLAMATION)
         return Self
      endif
      if len(cMarkerCondition)>255 .or. len(cMarkerConditionSec)>255
         ::MessageBox( "The current expression(s) have "+;
                      ltrim(str(len(cMarkerCondition)))+" and "+;
                      ltrim(str(len(cMarkerConditionSec)))+;
                      " characters - the maximum is 255", ;
                      "Expression too long", MB_ICONEXCLAMATION)
         return Self
      endif
      lOK:=.f.
      try
         if lMarker
            cVal:=valtype(&cMarkerCondition)
         endif
         if lMarkerSec
            cValSec:=valtype(&cMarkerConditionSec)
         endif
         lOK:=.t.
      catch oErr
         ::MessageBox("Please type in valid expression(s)", "Warning", MB_ICONEXCLAMATION)      
      end
      if !lOK
         return Self
      endif
      if (lMarker.and.cVal<>"L") .or. (lMarkerSec.and.cValSec<>"L")
         ::MessageBox("Please type in a valid expression(s) which evaluate to logical value", "Warning", MB_ICONEXCLAMATION)      
         return Self
      endif
      
      ::Disable()
// attach the corresponding colorizing event handler methods
// to columns selected for colorizing and detach from the other columns
      for i:=1 to len(aCols)
         cVal:="GridColumn"+ltrim(str(i))
         if aMarker[i]
            oChild:&cVal:EventHandler["OnQueryBackColor"]:=""
            oChild:&cVal:EventHandler["OnQueryForeColor"]:=""
         endif
         aMarker[i]:=if(::Listme:GetSel(i)>0, .t., .f.)
         if aMarker[i]
            oChild:&cVal:EventHandler["OnQueryBackColor"]:="myMarkerBack"
            oChild:&cVal:EventHandler["OnQueryForeColor"]:="myMarkerFore"
         endif
      next

   endif
   
   ::Disable()
   
   oChild:Grid:Update()

   ::Enable()

   ::Close()
   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD CheckMarker_OnClick( Sender ) CLASS FormMarker
   lMarker:=if(Sender:Checked(), .t., .f.)
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonCancel_OnClick( Sender ) CLASS FormMarker
   ::Close()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD CheckMarkerSec_OnClick( Sender ) CLASS FormMarker
   lMarkerSec:=if(Sender:Checked(), .t., .f.)   
RETURN Self