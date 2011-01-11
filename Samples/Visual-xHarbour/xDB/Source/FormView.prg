GLOBAL EXTERNAL oApp, oErr
GLOBAL EXTERNAL lView, lCols, lTop, lBottom, lShared
GLOBAL EXTERNAL aCols, aView, aMarker
GLOBAL EXTERNAL cTopCondition, cBottomCondition
GLOBAL EXTERNAL cDriver, hDriver

#include "vxh.ch"
#include "FormView.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD FormView_OnLoad( Sender ) CLASS FormView   
   local i, cRDD:=right( hDriver[cDriver], 3)
// list all columns of the current free table   
   ::Listme:AddItem("n/a")
   for i:=1 to len(aCols)
      ::Listme:AddItem(aCols[i])
   next
   for i:=1 to len(aCols)
      ::Listme:SetSel(i, if(aView[i], .t., .f.))
   next
// display conditions - if already present   
   ::EditTop:Caption:=cTopCondition
   ::EditBottom:Caption:=cBottomCondition
   ::CheckColumn:SetState(if( lCols, BST_CHECKED, BST_UNCHECKED))   
   ::CheckTop:SetState(if( lTop, BST_CHECKED, BST_UNCHECKED))   
   ::CheckBottom:SetState(if( lBottom, BST_CHECKED, BST_UNCHECKED))
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonApply_OnClick( Sender ) CLASS FormView
   local i, cType, cCond, lOK
// checking values found on the form   
   lView:=.f.
   
   if lCols  // column selection is active
      lOK:=.f.
      for i:=1 to len(aCols)  // memorize selected columns
         aView[i]:=if( ::Listme:GetSel(i)>0, .t., .f.)
         if aView[i]
            lOK:=.t.
         endif
      next      
      if !lOK
         ::MessageBox("Please select at least one column, or unckeck the 'Activate Column Selection' option","Warning", MB_ICONEXCLAMATION)
         return Self
      endif
   endif
   
   if lTop   // active ORDER - key expression
      lOK:=.f.
      cCond:=alltrim(::EditTop:Caption)
      if empty(cCond)
         ::MessageBox("ORDER key expression is empty", "Warning", MB_ICONEXCLAMATION)
         return Self
      endif
      if len(cCond) > 255
         ::MessageBox("The current key expression has "+ltrim(str(len(cCond)))+" characters - the maximum is 255", "Expression too long", MB_ICONEXCLAMATION)
         return Self
      endif
      try
         cType:=valtype(&cCond)
         lOK:=.t.
      catch oErr
         ::MessageBox("Please type in a valid key expression (type C, N or D), or uncheck the 'Activate ordering' option","Warning",MB_ICONEXCLAMATION)
      end
      if !lOK
         return Self
      endif
      if !cType$"CND"
         ::MessageBox("Please type in a valid key expression (type C, N or D), or uncheck the 'Activate ordering' option","Warning",MB_ICONEXCLAMATION)         
         return Self
      endif
      cTopCondition:=cCond
   endif
      
   if lBottom   // active FOR - filter expression
      lOK:=.f.
      cCond:=alltrim(::EditBottom:Caption)
      if empty(cCond)
         ::MessageBox("FOR filter expression is empty", "Warning", MB_ICONEXCLAMATION)
         return Self
      endif
      if len(cCond) > 255
         ::MessageBox("The current filter expression has "+ltrim(str(len(cCond)))+" characters - the maximum is 255", "Expression too long", MB_ICONEXCLAMATION)
         return Self
      endif
      try
         cType:=valtype(&cCond)
         lOK:=.t.
      catch oErr
         ::MessageBox("Please type in a valid boolean expression as filter expression, or uncheck the 'Activate filtering' option","Warning",MB_ICONEXCLAMATION)
      end
      if !lOK
         return Self
      endif
      if cType <> "L"
         ::MessageBox("Please type in a valid boolean expression as filter expression, or uncheck the 'Activate filtering' option","Warning",MB_ICONEXCLAMATION)         
         return Self
      endif
      cBottomCondition:=cCond
   endif

   if lCols .or. lTop .or. lBottom  // at least one selection is active
      lView:=.t.                  // so VIEW is possible to create
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonClose_OnClick( Sender ) CLASS FormView
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD CheckColumn_OnClick( Sender ) CLASS FormView
   lCols:=if(Sender:Checked(), .t., .f.)   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD CheckTop_OnClick( Sender ) CLASS FormView
   lTop:=if(Sender:Checked(), .t., .f.)      
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonView_OnClick( Sender ) CLASS FormView
   ::ButtonApply_OnClick(Self) // check values and refresh conditions
   if lView                        // create proper VIEW
      FormCurrentView( ::this )
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD CheckBottom_OnClick( Sender ) CLASS FormView
   lBottom:=if(Sender:Checked(), .t., .f.)         
RETURN Self