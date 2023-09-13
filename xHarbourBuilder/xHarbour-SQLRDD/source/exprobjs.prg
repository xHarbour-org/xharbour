#include "compat.ch"
#include "hbclass.ch"

**************************************************
class Operator
   HIDDEN:
   data _cPattern

   EXPORTED:
   data cName
   data aSymbols

   EXPORTED:
   access cPattern

   EXPORTED:
   method new(pName, pSymbols)
endclass

method new(pName, pSymbols)
   ::cName := pName
   ::aSymbols := pSymbols
return self

method cPattern
    if (::_cPattern == nil)
      ::_cPattern = cJoin(::aSymbols, "|")
      ::_cPattern = cPattern(::_cPattern)
    endif
return ::_cPattern

**************************************************
class ComparisonOperator from Operator
   EXPORTED:
   method new(pName, pSymbols)
endclass

method new(pName, pSymbols)
   ::super:new(pName, pSymbols)
return self

**************************************************
class SerialOperator from Operator
   PROTECTED:
   data _nPriority

   EXPORTED:
   access nPriority inline ::_nPriority

   EXPORTED:
   method new(pName, pSymbols)
endclass

method new(pName, pSymbols)
   ::super:new(pName, pSymbols)
return self

**************************************************
class LogicalOperator from SerialOperator
   EXPORTED:
   method new(pName, pSymbols)
endclass

method new(pName, pSymbols)
   ::super:new(pName, pSymbols)
    if (pName == "and")
        ::_nPriority = 1
    elseif (pName == "or")
        ::_nPriority = 0
      endif
return self

**************************************************
class ArithmeticOperator from SerialOperator
   EXPORTED:
   method new(pName, pSymbols)
endclass

method new(pName, pSymbols)
   ::super:new(pName, pSymbols)
    if (pName == "exponent")
        ::_nPriority = 2
    elseif (pName == "multiplied" .or. pName == "divided")
        ::_nPriority = 1
    elseif (pName == "plus" .or. pName == "minus")
        ::_nPriority = 0
    endif
return self

**************************************************
class AlgebraSet
   EXPORTED:
   data oOperator readonly

   EXPORTED:
   data cType readonly

   EXPORTED:
   data cIdentityElement readonly

   EXPORTED:
   data cAbsorbentElement readonly

   EXPORTED:
   method new(pOperator, pType)
endclass

method new(pOperator, pType)
   ::oOperator = pOperator
   ::cType = pType

   do case
      case (::oOperator:cName in {"plus", "minus"})
         if(::cType == "C")
            ::cIdentityElement = "''"
         elseif(::cType == "N")
            ::cIdentityElement = "0"
         elseif(::cType == "D")
            ::cIdentityElement = "0"
         endif
      case (::oOperator:cName in {"multiplied", "divided", "exponent"})
         ::cIdentityElement = "1"
         ::cAbsorbentElement = "0"
         ::cType = "N"
      case (::oOperator:cName == "and")
         ::cIdentityElement = ".T."
         ::cAbsorbentElement = ".F."
         ::cType = "L"
      case (::oOperator:cName == "or")
         ::cIdentityElement = ".F."
         ::cAbsorbentElement = ".T."
         ::cType = "L"
   endcase
return self

**************************************************
class ISerialComposition //just a dummy class that is used as interface
endclass

**************************************************
class ExpressionBase
   HIDDEN:
   data _oWorkArea

   EXPORTED:
   data lSimplified init .F.

   EXPORTED:
   data lAssessable

   EXPORTED:
   data lIsSimple init .F.

   EXPORTED:
   data oClipperExpression

   EXPORTED:
   data cContext

   EXPORTED:
   method GetType() virtual

   EXPORTED:
   access oWorkArea

   EXPORTED:
   method new(pContext, pClipperString)
endclass

method new(pContext, pClipperString)
   ::oClipperExpression = ClipperExpression():new(pContext, pClipperString)
   ::cContext = upper(pContext)
return self

method oWorkArea() class ExpressionBase
   if(::_oWorkArea == nil)
      ::_oWorkArea = oGetWorkarea(::cContext)
   endif
return ::_oWorkArea

**************************************************
class ConditionBase from ExpressionBase
   PROTECTED:
   data lDenied_ init .F. //bug with _lDenied, so I use lDenied_

   EXPORTED:
   access lDenied
   assign lDenied(value) inline ::lDenied(value)

   EXPORTED:
   method GetType() inline "L"

   EXPORTED:
   method new(pContext, pClipperString) inline ::super:new(pContext, pClipperString)

   EXPORTED:
   method new2(pContext, pClipperString, pDenied)
endclass

method new2(pContext, pClipperString, pDenied)
   ::lDenied_ = pDenied
return ::super:new(pContext, pClipperString)

method lDenied(value) class ConditionBase
   if(value != nil .and. value != ::lDenied_)
      ::lDenied_ = value
      ::oClipperExpression = ClipperExpression():new(::cContext, "!(" + ::oClipperExpression:cValue + ")")
   endif
return ::lDenied_

**************************************************
class BooleanExpression from ConditionBase
   EXPORTED:
   data oExpression

   EXPORTED:
   access Value inline iif(::lDenied_, iif(upper(::oExpression:Value) == ".T.", ".F.", ".T."), ::oExpression:Value)

   EXPORTED:
   access lDenied
   assign lDenied(value) inline ::lDenied(value)

   EXPORTED:
   method new(pContext, pClipperString, pExpr)

   EXPORTED:
   method new2(pContext, pClipperString, pDenied, pExpr)
endclass

method new2(pContext, pClipperString, pDenied, pExpr)
   ::lDenied_ = pDenied
return ::new(pContext, pClipperString, pExpr)

method new(pContext, pClipperString, pExpr)
   ::super:new(pContext, pClipperString)
   ::oExpression := pExpr
   ::lIsSimple = pExpr:lIsSimple
   ::lSimplified = pExpr:lSimplified
return self

//not very usefull, but cleaner
method lDenied(value) class BooleanExpression
   if(value != nil .and. value != ::lDenied_ .and. ::lIsSimple .and. ::oExpression:ValueType = "value")
      ::lDenied_ = value
      ::oClipperExpression = ClipperExpression():new(::cContext, ::Value)
      return ::lDenied_
   endif
return ::super:lDenied(value)

**************************************************
class ComposedConditionBase from ConditionBase
   EXPORTED:
   data oOperand1
   data oOperand2
   data oOperator

   EXPORTED:
   method new(pContext, pClipperString, pOperand1, pOperator, pOperand2)

   EXPORTED:
   method new2(pContext, pClipperString, pDenied, pOperand1, pOperator, pOperand2)
endclass

method new2(pContext, pClipperString, pDenied, pOperand1, pOperator, pOperand2) CLASS ComposedConditionBase
   ::lDenied_ = pDenied
return ::new(pContext, pClipperString, /*pExpr,*/ pOperand1, pOperator, pOperand2)

method new(pContext, pClipperString, pOperand1, pOperator, pOperand2) CLASS ComposedConditionBase
   ::super:new(pContext, pClipperString)
   ::oOperand1 := pOperand1
   ::oOperand2 := pOperand2
   ::oOperator := pOperator
return self

**************************************************
class Comparison from ComposedConditionBase
endclass

**************************************************
class ComposedCondition from ComposedConditionBase
endclass

**************************************************
class Expression from ExpressionBase
   EXPORTED:
   method GetType()

   EXPORTED:
   method new(pContext, pClipperString) inline ::super:new(pContext, pClipperString)
endclass

method GetType() class Expression
return ::oClipperExpression:cType

**************************************************
class ValueExpression from Expression
   EXPORTED:
   data Value

   EXPORTED:
   data ValueType

   HIDDEN:
   data cType

   EXPORTED:
   method GetType()

   EXPORTED:
   access oExpression inline self //usefull to implement the same interface than ValueExpression

   EXPORTED:
   method new(pContext, pValue)
endclass

method new(pContext, pValue)
   ::super:new(pContext, alltrim(pValue))

   if(aScan(::oWorkArea:aNames, {|x| x == upper(pValue)} ) > 0)
      ::ValueType := "field"
   elseif((pValue like "\w+") .and. (!pValue like "\d+") .and. !lower(pValue) == "nil")
      ::ValueType := "variable"
   else
      ::ValueType := "value"
      ::lSimplified = .T.
   endif

   ::Value := ::oClipperExpression:cValue
   ::lIsSimple = .T.
return self

//method redefined because it's faster than evaluate the expression.
method GetType() class ValueExpression
   if ::cType == nil
      if ::ValueType == "field"
         ::cType = ::oWorkArea:GetFieldByName(::Value):cType
      elseif ::ValueType == "variable"
         ::cType = ::super:GetType()
      elseif ::ValueType == "value"
         if (::Value like "\d+")
            ::cType = "N"
         elseif (::Value like "'.*'")
            ::cType = "C"
         elseif (upper(::Value) in {".T.", ".F."})
            ::cType = "L"
         endif
      else
         ::cType = "U"
      endif
   endif
return ::cType

**************************************************
class FunctionExpression from Expression
   EXPORTED:
   data cFunctionName
   data aParameters

   EXPORTED:
   method new(pContext, pClipperString, pFunctionName, aParameters)
endclass

method new(pContext, pClipperString, pFunctionName, aParameters)
   ::super:new(pContext, pClipperString)
   ::cFunctionName := lower(pFunctionName)
   ::aParameters := aParameters
return self

**************************************************
class Parameter
   EXPORTED:
   data lIsByRef
   data oExpression

   EXPORTED:
   method new(pExpression, pIsByRef)
endclass

method new(pExpression, pIsByRef)
   ::oExpression := pExpression
   ::lIsByRef := pIsByRef
return self

**************************************************
class ComposedExpression from Expression
   EXPORTED:
   data oOperand1
   data oOperand2
   data oOperator

   HIDDEN:
   data cType

   EXPORTED:
   method GetType()

   EXPORTED:
   method new(pContext, pClipperString, pOperand1, pOperator, pOperand2)
endclass

method new(pContext, pClipperString, pOperand1, pOperator, pOperand2)
   ::super:new(pContext, pClipperString)
   ::oOperand1 := pOperand1
   ::oOperand2 := pOperand2
   ::oOperator := pOperator
return self

method GetType()
   local cOperand1Type
   if(::cType == nil)
      cOperand1Type := ::oOperand1:GetType()
      if(::oOperator:cName in {"plus", "minus"} .and. cOperand1Type == "N") //date + numeric
         ::cType = ::oOperand2:GetType()
      else
         ::cType = cOperand1Type
      endif
   endif
return ::cType

**************************************************
procedure Visualize(oExpression) //for debuging
   local i
   alert(oExpression:className() + " - workarea: " + oExpression:cContext)
   if(oExpression:isKindOf("ConditionBase"))
      if(oExpression:lDenied)
         alert("not")
      endif
   endif
   if (oExpression:isKindOf("BooleanExpression"))
      Visualize(oExpression:oExpression)
   elseif(oExpression:isKindOf("Comparison") .or. oExpression:isKindOf("ComposedCondition") .or. oExpression:isKindOf("ComposedExpression"))
      Visualize(oExpression:oOperand1)
      alert(oExpression:oOperator:cName)
      Visualize(oExpression:oOperand2)
   elseif(oExpression:isKindOf("ValueExpression"))
      alert(oExpression:Value)
   elseif(oExpression:isKindOf("FunctionExpression"))
      alert(oExpression:cFunctionName)
      alert(cstr(len(oExpression:aParameters)) + " parameter(s) :")
      for i:=1 to len(oExpression:aParameters)
         Visualize(oExpression:aParameters[i]:oExpression)
      next i
   endif
return


**************************************************

function CollectAliases(oExpression, aAliases)
   local i
   aAddDistinct(aAliases, oExpression:cContext, {|x| lower(x)})
   if(oExpression:isKindOf("BooleanExpression"))
      CollectAliases(oExpression:oExpression, aAliases)
   elseif(oExpression:isKindOf("Comparison") .or. oExpression:isKindOf("ComposedCondition") .or. oExpression:isKindOf("ComposedExpression"))
      CollectAliases(oExpression:oOperand1, aAliases)
      CollectAliases(oExpression:oOperand2, aAliases)
   elseif(oExpression:isKindOf("FunctionExpression"))
      for i:=1 to len(oExpression:aParameters)
         CollectAliases(oExpression:aParameters[i]:oExpression, aAliases)
      next i
   endif
return aAliases

**************************************************
function ConvertToCondition(oExpression)
   if(!oExpression:isKindOf("ComposedExpression") .and. oExpression:GetType() == "L")
      return BooleanExpression():new(oExpression:cContext, oExpression:oClipperExpression:cValue, oExpression)
   endif
return nil
