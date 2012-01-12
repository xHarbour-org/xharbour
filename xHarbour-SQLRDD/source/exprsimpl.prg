#include "compat.ch"
#include "hbclass.ch"

**************************************************
class ExpressionSimplifierBase
	EXPORTED:
	data cContext
	
	EXPORTED:
	data lFixVariables init .F.

	EXPORTED:
	data lIgnoreRelations init .F.

	EXPORTED:
	METHOD Simplify(oExpression) virtual	
	
	EXPORTED:
	METHOD Assessable(oExpression) virtual
	
	PROTECTED:
	METHOD NewSimpleExpression(cContext, cClipperString) virtual
	
	PROTECTED:
	METHOD NewComposedExpression(cAlias, cExpression, oOperand1, oConnector, oOperand2) virtual
	
	PROTECTED:
	METHOD SimplifyComposition(oExpression)	
	
	PROTECTED:
	METHOD CompositionAssessable(oExpression)

	EXPORTED:
	METHOD new(pFixVariables, pIgnoreRelations, pContext)
endclass

METHOD new(pFixVariables, pIgnoreRelations, pContext)
	if(pFixVariables!=nil)
		::lFixVariables = pFixVariables
	endif
	if(pIgnoreRelations!=nil)
		::lIgnoreRelations = pIgnoreRelations
	endif
	::cContext = upper(pContext)	
return self

METHOD SimplifyComposition(oExpression)	class ExpressionSimplifierBase
   LOCAL oAlgebraSet, newClipperString
	local newOperands := {oExpression:oOperand1, oExpression:oOperand2}, oSimpleExpression, i
	for i:=1 to 2	
		newOperands[i] = ::Simplify(newOperands[i]) 
		if(newOperands[i]:lIsSimple)
			oSimpleExpression = newOperands[i]:oExpression
			oAlgebraSet = AlgebraSet():new(oExpression:oOperator, oExpression:GetType())
			if (oAlgebraSet:cIdentityElement == upper(newOperands[i]:Value))
				return iif(i==1, ::Simplify(newOperands[2]), newOperands[1])
			elseif(oAlgebraSet:cAbsorbentElement != nil .and. oAlgebraSet:cAbsorbentElement == upper(newOperands[i]:Value))
				return ::NewSimpleExpression(oExpression:cContext, oAlgebraSet:cAbsorbentElement)
			endif
		endif
	next i
		
	if(!newOperands[1]==oExpression:oOperand1 .or. !newOperands[2]==oExpression:oOperand2)
		newClipperString = newOperands[1]:oClipperExpression:cValue + " " + oExpression:oOperator:aSymbols[1] + " " + newOperands[2]:oClipperExpression:cValue
		return ::NewComposedExpression(oExpression:cContext, newClipperString, newOperands[1], oExpression:oOperator, newOperands[2])
	endif	
return oExpression

METHOD CompositionAssessable(oExpression) class ExpressionSimplifierBase
return (oExpression:oOperand1:lSimplified .or. ::Assessable(oExpression:oOperand1)) .and.;
	(oExpression:oOperand2:lSimplified .or. ::Assessable(oExpression:oOperand2)); 

**************************************************
class ExpressionSimplifier from ExpressionSimplifierBase
	HIDDEN:
	data _oConditionSimplifier
	
	HIDDEN:
	access oConditionSimplifier inline iif(::_oConditionSimplifier == nil, (::_oConditionSimplifier := ConditionSimplifier():new(::lFixVariables, ::lIgnoreRelations, ::cContext)), ::_oConditionSimplifier)

	EXPORTED:
	METHOD Simplify(oExpression)	
	
	EXPORTED:
	METHOD Assessable(oExpression)
	
	HIDDEN:
	METHOD ValueAssessable(oExpression)
	
	HIDDEN:
	METHOD FunctionAssessable(oExpression)
	
	PROTECTED:
	METHOD NewSimpleExpression(cContext, cClipperString) inline ValueExpression():new(cContext, cClipperString)
	
	PROTECTED:
	METHOD NewComposedExpression(cAlias, cExpression, oOperand1, oConnector, oOperand2) inline ComposedExpression():new(cAlias, cExpression, oOperand1, oConnector, oOperand2)
	
	EXPORTED:
	METHOD new(pFixVariables, pIgnoreRelations, pContext, pConditionSimplifier)
endclass

METHOD new(pFixVariables, pIgnoreRelations, pContext, pConditionSimplifier)
	::_oConditionSimplifier = pConditionSimplifier
return ::super:new(pFixVariables, pIgnoreRelations, pContext)

METHOD Simplify(oExpression) class ExpressionSimplifier
	LOCAL newValue, i, newParams, lAtLeastOneParamSimplified, oParameter, oSimplifiedExpression, result, simplifier, newClipperString
   LOCAL lEvaluated := .F.
   
	if(oExpression:lSimplified)
		return oExpression
	elseif(::Assessable(oExpression))
		try
			newValue = oExpression:oClipperExpression:Evaluate()
			lEvaluated = .T.
		catch
		end
		if(lEvaluated)
			do case
				case (valtype(newValue)=="C")
					newValue = "'"+newValue+"'"
					result = ValueExpression():new(oExpression:cContext, newValue)
				case (valtype(newValue)=="N" .or. valtype(newValue)=="L" .or. valtype(newValue)=="U")
					newValue = cstr(newValue)
					result = ValueExpression():new(oExpression:cContext, newValue)
				case (valtype(newValue)=="D")
					newValue = "'"+dtoc(newValue)+"'"
					result = FunctionExpression():new(oExpression:cContext, "ctod("+newValue+")", "ctod", {Parameter():new(ValueExpression():new(oExpression:cContext, newValue), .F.)})
			endcase		
		endif
	endif
	if(!lEvaluated)
		if(oExpression:isKindOf("ComposedExpression"))
			result = ::SimplifyComposition(oExpression)
		elseif(oExpression:isKindOf("FunctionExpression"))
			newParams = {}
			lAtLeastOneParamSimplified = .F.
			for i:=1 to len(oExpression:aParameters)
				oParameter = oExpression:aParameters[i]
				if(oParameter:oExpression:isKindOf("ConditionBase"))
					simplifier = ::oConditionSimplifier
				else
					simplifier = self
				endif
				if(oParameter:lIsByRef .or. (oSimplifiedExpression := simplifier:Simplify(oParameter:oExpression)) == oParameter:oExpression)
					aadd(newParams, oParameter)
				else
					aadd(newParams, Parameter():new(oSimplifiedExpression, .F.))
					lAtLeastOneParamSimplified = .T.
				endif
			next i
			if(lAtLeastOneParamSimplified)
				newClipperString = oExpression:cFunctionName + "("
				for i:=1 to len(newParams)
					newClipperString += newParams[i]:oExpression:oClipperExpression:cValue + iif(i==len(newParams), ")", ",")
				next i
		
				result = FunctionExpression():new(oExpression:cContext, newClipperString, oExpression:cFunctionName, newParams)
			endif
		endif
	endif
	if(result == nil)
		result = oExpression
	endif
	result:lSimplified = .T.
return result

METHOD Assessable(oExpression) class ExpressionSimplifier
	local result
	if(oExpression:lAssessable != nil)
		return oExpression:lAssessable
	endif

	do case
		case (oExpression:isKindOf("ValueExpression"))
			result = ::ValueAssessable(oExpression)
		case (oExpression:isKindOf("FunctionExpression"))
			result = ::FunctionAssessable(oExpression)
		case (oExpression:isKindOf("ComposedExpression"))
			result = ::CompositionAssessable(oExpression)
	endcase
	oExpression:lAssessable = result
return result

METHOD ValueAssessable(oExpression) class ExpressionSimplifier
   LOCAL lRet
	do case
		case (oExpression:ValueType == "value")
			lRet := .T.
		case (oExpression:ValueType == "variable")
			lRet := ::lFixVariables
		case (oExpression:ValueType == "field")
			lRet := (::lIgnoreRelations .or. !::cContext == oExpression:cContext .and. len(RelationManager():new():GetRelations(::cContext, oExpression:cContext)) == 0) .and. ::lFixVariables 			
	endcase
   
RETURN lRet
   
METHOD FunctionAssessable(oExpression) class ExpressionSimplifier
	local i, simplifier
	for i:=1 to len(oExpression:aParameters)
		if(oExpression:aParameters[i]:oExpression:isKindOf("ConditionBase"))
			simplifier = ::oConditionSimplifier
		else
			simplifier = self
		endif
		if(!simplifier:Assessable(oExpression:aParameters[i]:oExpression))
			Return .F.
		endif
	next i
return __DynsIsFun(__DynsGetIndex(oExpression:cFunctionName)) .and. !oExpression:cFunctionName == "deleted" .and. !oExpression:cFunctionName == "recno"	

**************************************************
class ConditionSimplifier from ExpressionSimplifierBase
	HIDDEN:
	data _oExpressionSimplifier
	
	EXPORTED:
	METHOD Simplify(oCondition)	
	
	EXPORTED:
	METHOD Assessable(oCondition)
	
	HIDDEN:
	METHOD BooleanExprAssessable(oCondition)
	
	HIDDEN:
	METHOD ComparisonAssessable(oCondition)
	
	PROTECTED:
	METHOD NewSimpleExpression(cContext, cClipperString) inline BooleanExpression():new(cContext, cClipperString, ValueExpression():new(cContext, cClipperString))
	
	PROTECTED:
	METHOD NewComposedExpression(cAlias, cExpression, oOperand1, oConnector, oOperand2) inline ComposedCondition():new(cAlias, cExpression, oOperand1, oConnector, oOperand2)
	
	EXPORTED:
	METHOD new(pFixVariables, pIgnoreRelations, pContext) 
endclass

METHOD new(pFixVariables, pIgnoreRelations, pContext)
	::_oExpressionSimplifier = ExpressionSimplifier():new(pFixVariables, pIgnoreRelations, pContext, self)
return ::super:new(pFixVariables, pIgnoreRelations, pContext)

METHOD Simplify(oCondition) class ConditionSimplifier
	local newValue, newOperand1, newOperand2, newExpression, result, newClipperString
	if(oCondition:lSimplified)
		return oCondition
	elseif(oCondition:isKindOf("BooleanExpression"))
		newExpression = ::_oExpressionSimplifier:Simplify(oCondition:oExpression)
		if(!newExpression == oCondition:oExpression)
			result = ConvertToCondition(newExpression)
		endif
	elseif(oCondition:isKindOf("Comparison"))
		newOperand1 = ::_oExpressionSimplifier:Simplify(oCondition:oOperand1) 
		newOperand2 = ::_oExpressionSimplifier:Simplify(oCondition:oOperand2)
		newClipperString = newOperand1:oClipperExpression:cValue + " " + oCondition:oOperator:aSymbols[1] + " " + newOperand2:oClipperExpression:cValue
		if(newOperand1:isKindOf("ValueExpression") .and. newOperand1:ValueType == "value" .and. newOperand2:isKindOf("ValueExpression") .and. newOperand2:ValueType == "value")
			newValue = cstr(&newClipperString)
			result = BooleanExpression():new(oCondition:cContext, newValue, ValueExpression():new(oCondition:cContext, newValue))
		elseif(!newOperand1==oCondition:oOperand1 .or. !newOperand2==oCondition:oOperand2)
			result = Comparison():new(oCondition:cContext, newClipperString, newOperand1, oCondition:oOperator, newOperand2)
		endif	  
	elseif(oCondition:isKindOf("ComposedCondition"))
		result = ::SimplifyComposition(oCondition)
	endif
	if(result == nil)
		result = oCondition
	endif 
	result:lDenied = oCondition:lDenied
	oCondition:lSimplified = .T.
return result

METHOD Assessable(oCondition) class ConditionSimplifier
	local result
	if(oCondition:lAssessable != nil)
		return oCondition:lAssessable
	endif
	do case
		case (oCondition:isKindOf("BooleanExpression"))
			result = ::BooleanExprAssessable(oCondition)
		case (oCondition:isKindOf("Comparison"))
			result = ::ComparisonAssessable(oCondition)
		case (oCondition:isKindOf("ComposedCondition"))
			result = ::CompositionAssessable(oCondition)
	endcase
	oCondition:lAssessable = result
return result
	
METHOD BooleanExprAssessable(oCondition) class ConditionSimplifier
return ::_oExpressionSimplifier:Assessable(oCondition:oExpression) 

METHOD ComparisonAssessable(oCondition) class ConditionSimplifier
return ::_oExpressionSimplifier:Assessable(oCondition:oOperand1) .and. ::_oExpressionSimplifier:Assessable(oCondition:oOperand2)
