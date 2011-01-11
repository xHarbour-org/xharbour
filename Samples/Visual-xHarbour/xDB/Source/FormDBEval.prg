GLOBAL EXTERNAL oApp, oChild, oErr
GLOBAL EXTERNAL lCount, lStatistics, nCountNext, cCountFor, cCountWhile
GLOBAL EXTERNAL nCalcInit1, nCalcInit2, nCalcInit3
GLOBAL EXTERNAL nCalcNext, cCalcExpr, cCalcFor, cCalcWhile

#include "vxh.ch"
#include "FormDBEval.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD ButtonCalc_OnClick( Sender ) CLASS FormDBEval

   local lOK, nSeconds
   local cExpr:=alltrim(::EditExpr:Caption)
   local cFor:=alltrim(::EditFor:Caption), cWhile:=alltrim(::EditWhile:Caption)
   local lAll:=if(::RadioAll:GetState()==BST_CHECKED, .t., .f.)
   local lRest:=if(::RadioRest:GetState()==BST_CHECKED, .t., .f.)
   local lNext:=if(::RadioNext:GetState()==BST_CHECKED, .t., .f.)
   local nNext:=::MaskEditNext:Caption
   local cType, cBlocExpr, cBlocFor, cBlocWhile, cBlocTest
   memvar _calc1, _calc2, _calc3
   public _calc1, _calc2, _calc3   // helper values for calculations
                          // checking front-end values
   if lNext .and. nNext<1
      ::MessageBox("The minimum number of records in the NEXT condition is 1 - the current record", "", MB_ICONEXCLAMATION)
      return Self
   endif
   
   lOK:=.f.
   try
      _calc1:=val(alltrim(::EditInit1:Caption))
      _calc2:=val(alltrim(::EditInit2:Caption))
      _calc3:=val(alltrim(::EditInit3:Caption))
      lOK:=.t.
   catch oErr
      myError(oApp, oErr)
   end
   if !lOK
      ::MessageBox("The initial values of variable _CALCx have to be numeric", "", MB_ICONEXCLAMATION)
      return Self
   endif
   
   if empty(cExpr)
      ::MessageBox("Please complete the expression", "", MB_ICONEXCLAMATION)
      return Self
   endif
   if len(cExpr) > 510
      ::MessageBox("The actual length of the expression is"+str(len(cExpr))+" - the maximum permitted is 510", "Expression too long", MB_ICONEXCLAMATION)
      return Self
   endif
   lOK:=.f.
   try
      Eval({|| &cExpr })
      lOK:=.t.
   catch oErr
      myError(oApp, oErr)
   end
   if !lOK
      ::MessageBox("Please type in a valid expression", "", MB_ICONEXCLAMATION)
      return Self
   endif
      
   lOK:=.T.
   try
      if !empty(cFor)
         cType:=valtype(&cFor)
         if cType<>"L"
            lOK:=.F.
         endif
      endif
      if !empty(cWhile)
         cType:=valtype(&cWhile)
         if cType<>"L"
            lOK:=.F.
         endif
      endif
   catch oErr
      myError(oApp, oErr)
   end
   if !lOK
      ::MessageBox("The FOR, respectively WHILE conditions must be valid logical expressions or empty", "", MB_ICONEXCLAMATION)
      return Self
   endif
// redo initializations lost with test evaluations
   _calc1:=val(alltrim(::EditInit1:Caption))
   _calc2:=val(alltrim(::EditInit2:Caption))
   _calc3:=val(alltrim(::EditInit3:Caption))
                               // construct code blocks
   cBlocExpr:="{||"+cExpr+"}"
   cBlocFor:=if( empty(cFor), NIL, "{||"+cFor+"}")
   cBlocWhile:=if( empty(cWhile), NIL, "{||"+cWhile+"}")

   ::Disable()   
   
   ::PBar:Visible:=.t.
   ::Timer1:Start()
                               // proper DBEval
   lOK:=.f.
   nSeconds:=Seconds()
   try
      DBEval( &cBlocExpr, ;
              if( cBlocFor==NIL, NIL, &cBlocFor), ;
              if( cBlocWhile==NIL, NIL, &cBlocWhile), ;
              if( lNext, nNext, NIL), ;
              NIL, ;
              if( lRest, .t., NIL) )
      lOK:=.t.
   catch oErr
      myError(oApp, oErr)
   end
   nSeconds:=mySeconds( nSeconds, Seconds())
   oChild:Grid:Update()

   ::Timer1:Stop()
   ::PBar:Visible:=.f.
   
   if lOK
      ::EditResult1:Caption:=if(lCount, str(_calc1,20), str(_calc1,20,2))
      ::EditResult2:Caption:=if(lCount, str(_calc2,20), str(_calc2,20,2))
      ::EditResult3:Caption:=if(lCount, str(_calc3,20), str(_calc3,20,2))
   endif
   if lStatistics
      ::MessageBox( "Calculation has been terminated in "+ltrim(str(nSeconds))+" seconds", "", MB_ICONINFORMATION)
   endif
   release _calc1, _calc2, _calc3
   ::Enable()

RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD FormDBEval_OnLoad( Sender ) CLASS FormDBEval
   Sender:PBar:Visible:=.f.
   Sender:PBar:SetRange({1, 50})
   Sender:PBar:SetStep(1)
   if lCount
      with object Sender
         :Caption:="Count"
         :EditInit1:Caption:="0"
         :EditInit1:Enabled:=.f.
         :EditInit2:Caption:="0"
         :EditInit2:Visible:=.f.
         :TInit2:Visible:=.f.
         :EditInit3:Caption:="0"
         :EditInit3:Visible:=.f.
         :TInit3:Visible:=.f.
         :EditResult2:Visible:=.f.
         :TResult2:Visible:=.f.
         :EditResult3:Visible:=.f.
         :TResult3:Visible:=.f.
         :EditExpr:Caption:="_CALC1++"
         :EditExpr:Enabled:=.f.
         :EditFor:Caption:=cCountFor
         :EditWhile:Caption:=cCountWhile
         :MaskEditNext:Caption:=nCountNext
      end
   else
      with object Sender
         :Caption:="DBEval"
         :EditInit1:Caption:=ltrim(str(nCalcInit1,20,2))
         :EditInit2:Caption:=ltrim(str(nCalcInit2,20,2))
         :EditInit3:Caption:=ltrim(str(nCalcInit3,20,2))
         :EditExpr:Caption:=cCalcExpr
         :EditFor:Caption:=cCalcFor
         :EditWhile:Caption:=cCalcWhile
         :MaskEditNext:Caption:=nCalcNext
      end
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Timer1_OnTimeOut( Sender ) CLASS FormDBEval
   ::PBar:StepIt()
   Sender:Start()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonClose_OnClick( Sender ) CLASS FormDBEval
   if lCount
      nCountNext:=::MaskEditNext:Caption
      cCountFor:=alltrim(::EditFor:Caption)
      cCountWhile:=alltrim(::EditWhile:Caption)
   else
      nCalcInit1:=val(alltrim(::EditInit1:Caption))
      nCalcInit2:=val(alltrim(::EditInit2:Caption))
      nCalcInit3:=val(alltrim(::EditInit3:Caption))
      nCalcNext:=::MaskEditNext:Caption
      cCalcExpr:=alltrim(::EditExpr:Caption)
      cCalcFor:=alltrim(::EditFor:Caption)
      cCalcWhile:=alltrim(::EditWhile:Caption)
   endif
   ::Close()
RETURN Self