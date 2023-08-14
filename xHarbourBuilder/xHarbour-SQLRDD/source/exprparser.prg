#include "compat.ch"
#include "hbclass.ch"
		
**************************************************
class ParserBase
	HIDDEN:
	data _SortedOperators

	PROTECTED:
	data _Operators
	
	PROTECTED:
	data _cDefaultContext

	EXPORTED:
	method Parse(cExpression)
	
	PROTECTED:
	method InternParse(cExpression)	
	
	PROTECTED:
	method GetComposedExpression(cAlias, cExpression, oOperand1, oConnector, oOperand2) virtual	
	
	PROTECTED:
	method GetOperators() virtual
	
	PROTECTED:
	access SortedOperators

	PROTECTED:
	method GetOperands(cExpression, cAlias, oOperand1, oConnector, oOperand2)
	
	PROTECTED:
	method RestoreParenthesis(cExpression)	
	
	PROTECTED:
	method ResolveParenthesis(cExpression)
	
	PROTECTED:
	method ExtractAlias1(cExpression)	
	
	PROTECTED:
	method ExtractAlias2(cExpression)	
	
	PROTECTED:
	method ExtractAlias3(cExpression)	
	
	HIDDEN:
	method ExtractAlias(cExpression, cRegex)
	
	EXPORTED:
	method new(pWorkarea)		
endclass

method new(pWorkarea)
	::_cDefaultContext = pWorkarea
return self

method SortedOperators class ParserBase
	if(::_SortedOperators == nil)
		::_SortedOperators = asort(::GetOperators(),,, {|x,y| x:nPriority < y:nPriority})
	endif
return ::_SortedOperators

method Parse(cExpression) class ParserBase
return ::InternParse("?"+AtRepl('"', cExpression, "'")+"?", ::_cDefaultContext)

method InternParse(cExpression, cAlias) class ParserBase
	local oOperand1, oOperand2, oConnector
	
	::GetOperands(@cExpression, @cAlias, @oOperand1, @oConnector, @oOperand2)
	
	if (oOperand2 != nil)
	    return ::GetComposedExpression(cAlias, cExpression, oOperand1, oConnector, oOperand2)
	endif
return oOperand1

method GetOperands(cExpression, cAlias, oOperand1, oConnector, oOperand2) class ParserBase
	local o, aGroups, i, cNewAlias, cRegO
	cExpression = alltrim(cExpression)
	
	cAlias = iif((cNewAlias := ::ExtractAlias1(@cExpression)) != nil, cNewAlias, cAlias) 
		
	do while (cExpression like "^\?(?:[^\'\?]*?(?:\'[^\']*\'))*[^\'\?]*\?$")
	    cExpression = alltrim(substr(cExpression, 2, len(cExpression) - 2))
	    cAlias = iif((cNewAlias := ::ExtractAlias2(@cExpression)) != nil, cNewAlias, cAlias) 
		::ResolveParenthesis(@cExpression)	
	enddo
	
	for i:=1 to len(::SortedOperators)
		o := ::SortedOperators[i]
	    cRegO = "^((?:[^\'\?]*(?:\'[^\']*\'|\?(?:[^\'\?]*(?:\'[^\']*\'))*[^\'\?]*\?))*?[^\'\?]*?)(" + o:cPattern + ")(\s*[^>].*)$"
	    if (HB_RegExMatch(cRegO, cExpression, .F.))
	    	aGroups = HB_RegExAtX(cRegO, cExpression)
	        oOperand1 = ::InternParse(aGroups[2,1], cAlias)
	        oConnector = o
	        oOperand2 = ::InternParse(aGroups[4,1], cAlias)
			exit
		endif
	next i

	cAlias = iif((cNewAlias := ::ExtractAlias3(@cExpression)) != nil, cNewAlias, cAlias)
return  Nil

METHOD ResolveParenthesis(cExpression) class ParserBase
   LOCAL i
	LOCAL nParenthesisDeep := 0 
   
	FOR i := 1 to len(cExpression)
		do case
			case (cExpression[i] == "'")
			    do while (cExpression[++i] != "'")
			    enddo
			case (cExpression[i] == "(")
                if (nParenthesisDeep == 0)
                    cExpression[i] = "?"
                endif
                nParenthesisDeep++
			case (cExpression[i] == ")")
                nParenthesisDeep--
                if (nParenthesisDeep == 0)
                    cExpression[i] = "?"
                endif
		endcase
	next i
RETURN Nil
		
method RestoreParenthesis(cExpression) class ParserBase
    local cParenthesis := "(", i
    for i:=1 to len(cExpression)
    	if(cExpression[i] == "'")
		    do while (cExpression[++i] != "'")
		    enddo
    	elseif(cExpression[i] == "?")
    		cExpression[i] = cParenthesis
    		cParenthesis = iif(cParenthesis == "(", ")", "(")
    	endif
    next i

return cExpression

method ExtractAlias1(cExpression) class ParserBase
	static regex := "^(\w+)\s*->\s*(\?.+\?)$"
return ::ExtractAlias(@cExpression, regex);

method ExtractAlias2(cExpression) class ParserBase
	static regex := "^(\w+)\s*->\s*(\(.+\))$"
return ::ExtractAlias(@cExpression, regex);

method ExtractAlias3(cExpression) class ParserBase
	static regex := "^(\w+)\s*->\s*(\w+)$"
return ::ExtractAlias(@cExpression, regex);

method ExtractAlias(cExpression, cRegex) class ParserBase
   LOCAL aGroups
   
   IF HB_RegExMatch(cRegex, cExpression, .F.)
    	aGroups = HB_RegExAtX(cRegex, cExpression)
        cExpression = aGroups[3,1]
        return aGroups[2,1]
    endif
return nil

/**************************************************/
class ExpressionParser from ParserBase		
	PROTECTED:
	method GetOperators()
	
	PROTECTED:
	method GetComposedExpression(cAlias, cExpression, oOperand1, oConnector, oOperand2)	
		
	PROTECTED:
	method GetOperands(cExpression, cAlias, oOperand1, oConnector, oOperand2)
	
	PROTECTED:
	method GetParameter(cExpression, cAlias)
	
	EXPORTED:
	method new(pWorkarea) inline ::super:new(pWorkarea)
endclass

method GetOperators() class ExpressionParser
	if(::_Operators == nil)	
		::_Operators = 	{                                                               ;
						    ArithmeticOperator():new("plus", {"+"}),                    ;
						    ArithmeticOperator():new("minus", {"-"}),                   ;
						    ArithmeticOperator():new("multiplied", {"*"}),              ;
						    ArithmeticOperator():new("divided", {"/"}),                 ;
						    ArithmeticOperator():new("exponent", {"^"})                 ;
						}  
	endif
return ::_Operators

method GetComposedExpression(cAlias, cExpression, oOperand1, oConnector, oOperand2) class ExpressionParser
return ComposedExpression():new(cAlias, ::RestoreParenthesis(cExpression), oOperand1, oConnector, oOperand2)

method GetOperands(cExpression, cAlias, oOperand1, oConnector, oOperand2) class ExpressionParser
	LOCAL aGroups, aParamGroups, cFunctionName, cParameters, aParameters := {}
   
	STATIC cRegFunction := "^(\w+)\s*\?\s*(.*?)\s*\?$"
   STATIC cRegParams := "^((?:[^\'\?,]*?(?:\'[^\']*\'|\?(?:[^\'\?]*?(?:\'[^\']*\'))*[^\'\?]*?\?))*[^\'\?,]*?),(.*)$"
   STATIC cRegMacro := "^&\s*(\w+)$"
   
	::super:GetOperands( @cExpression, @cAlias, @oOperand1, @oConnector, @oOperand2 )

    if (oOperand1 == nil)
        if (HB_RegExMatch(cRegFunction, cExpression, .F.))
	    	aGroups = HB_RegExAtX(cRegFunction, cExpression)
            cFunctionName = aGroups[2,1]
            cParameters = aGroups[3,1]
            ::ResolveParenthesis(@cParameters)
            
            do while (HB_RegExMatch(cRegParams, cParameters, .F.))
	    		aParamGroups = HB_RegExAtX(cRegParams, cParameters)
                aadd(aParameters, ::GetParameter(aParamGroups[2,1], cAlias))
                cParameters = aParamGroups[3,1]
            enddo
            
            if(!cParameters == "")
            	aadd(aParameters, ::GetParameter(cParameters, cAlias))
            endif
             
            oOperand1 = FunctionExpression():new(cAlias, ::RestoreParenthesis(cExpression), cFunctionName, aParameters)
        elseif(HB_RegExMatch(cRegMacro, cExpression, .F.))
        	oOperand1 = ::InternParse(&(HB_RegExAtX(cRegMacro, cExpression)[2,1]))
        else
            oOperand1 = ValueExpression():new(cAlias, ::RestoreParenthesis(cExpression), ::RestoreParenthesis(cExpression))
        endif
	endif
return Nil

method GetParameter(cExpression, cAlias) class ExpressionParser
	static cRegParam := "^(@?)(.*)$"
	local aGroups, lByRef, oExpression
	if (cExpression like "^\s*$")
		lByRef = .F.
		oExpression = ValueExpression():new(cAlias, "nil")  
	else
		aGroups := HB_RegExAtX(cRegParam, cExpression)
		lByRef := aGroups[2,1] != ""
		oExpression := GetConditionOrExpression(::RestoreParenthesis(aGroups[3,1]), cAlias)
	endif
return Parameter():new(oExpression, lByRef)
	
**************************************************
class ConditionParser from ParserBase	
	HIDDEN:
	data _cRegOperator
	data _cRegNegative1
	data _cRegNegative2
	data _cNegativesPattern init "!|\.not\."
	
	EXPORTED:
	data aClipperComparisonOperators	
	
	PROTECTED:
	method GetOperators()
	
	PROTECTED:
	method GetComposedExpression(cAlias, cExpression, oOperand1, oConnector, oOperand2)	
		
	PROTECTED:
	method GetOperands(cExpression, cAlias, oOperand1, oConnector, oOperand2)
		
	EXPORTED:
	method new(pWorkarea) 
endclass

method new(pWorkarea)
   LOCAL cOperatorsChoice, cOperatorsChars
   
	::super:new(pWorkarea)
	::aClipperComparisonOperators :=  						        ;
	{																;
	    ComparisonOperator():new("equal", {"="}),             		;
	    ComparisonOperator():new("equalEqual", {"=="}),            	;
	    ComparisonOperator():new("different", {"!=", "<>", "#"}),   ;
	    ComparisonOperator():new("lower", {"<"}),                   ;
	    ComparisonOperator():new("higher", {">"}),                  ;
	    ComparisonOperator():new("lowerOrEqual", {"<="}),           ;
	    ComparisonOperator():new("higherOrEqual", {">="}),          ;
	    ComparisonOperator():new("included", {"$"})                 ;
	}	
	
	cOperatorsChoice := cJoin(xSelect(::aClipperComparisonOperators, {|x| x:cPattern}), "|")
	cOperatorsChars := cPattern(CharList(cJoin(xSelectMany(::aClipperComparisonOperators, {|x| x:aSymbols}), "")))

    ::_cRegOperator = HB_RegExComp("^((?:[^\'\?]*?(?:\'[^\']*\'|\?(?:[^\'\?]*?(?:\'[^\']*\'))*[^\'\?]*?\?))*(?:[^\'\?]*?[^-"+cOperatorsChars+"\s])?\s*)("+cOperatorsChoice+")([^"+cOperatorsChars+"].*)$", .F.)
	::_cRegNegative1 = HB_RegExComp("^("+::_cNegativesPattern+")\s*(\?.*\?)$", .F.)
	::_cRegNegative2 = HB_RegExComp("^("+::_cNegativesPattern+")\s*(.+)$", .F.)
return self

method GetOperators() class ConditionParser
	if(::_Operators == nil)	
		::_Operators = 	{                                                               ;
						    LogicalOperator():new("and", {".and."}),                    ;
						    LogicalOperator():new("or", {".or."})                       ;
						}  
	endif
return ::_Operators

method GetComposedExpression(cAlias, cExpression, oOperand1, oConnector, oOperand2) class ConditionParser
return ComposedCondition():new(cAlias, ::RestoreParenthesis(cExpression), oOperand1, oConnector, oOperand2)

method GetOperands(cExpression, cAlias, oOperand1, oConnector, oOperand2) class ConditionParser
	local aGroups, oComparisonOperator, oExpressionParser, lDenied, cNewAlias, cExpression2
	::super:GetOperands(@cExpression, @cAlias, @oOperand1, @oConnector, @oOperand2)

    if (oOperand1 == nil)
        if (HB_RegExMatch(::_cRegNegative1, cExpression, .F.))
	    	aGroups = HB_RegExAtX(::_cRegNegative1, cExpression)
            oOperand1 = ::InternParse(aGroups[3,1], cAlias)
            oOperand1:lDenied = .T.
        else
			cAlias = iif((cNewAlias := ::ExtractAlias3(@cExpression)) != nil, cNewAlias, cAlias)

			oExpressionParser := ExpressionParser():new(cAlias)
			
            if (HB_RegExMatch(::_cRegOperator, cExpression, .F.))
		    	aGroups = HB_RegExAtX(::_cRegOperator, cExpression)
		    	
                oComparisonOperator = xFirst(::aClipperComparisonOperators, {|y| aGroups[3,1] $ y:aSymbols})
				
                oOperand1 = Comparison():new(cAlias, ::RestoreParenthesis(cExpression), oExpressionParser:Parse(::RestoreParenthesis(aGroups[2,1])),;
                                            oComparisonOperator,;
                                            oExpressionParser:Parse(::RestoreParenthesis(aGroups[4,1])))
            else
            	lDenied = HB_RegExMatch(::_cRegNegative2, cExpression, .F.)
            	cExpression2 = cExpression
		        if (lDenied)
			    	aGroups = HB_RegExAtX(::_cRegNegative2, cExpression)
			    	cExpression2 = aGroups[3,1]
            	endif
                oOperand1 = BooleanExpression():new2(cAlias, ::RestoreParenthesis(cExpression), lDenied, oExpressionParser:Parse(::RestoreParenthesis(cExpression2)))
            endif
        endif
	endif
return Nil
	
**************************************************
static function GetConditionOrExpression(cExpression, cAlias)
   LOCAL cContext
   local oParser := ConditionParser():new(cAlias)
   local oResult := oParser:Parse(cExpression)
   
   if (oResult:isKindOf("BooleanExpression") .and. oResult:oExpression:GetType() != "L")
    	cContext := oResult:cContext
      oResult := oResult:oExpression
      oResult:cContext = cContext
   endif
return oResult

**************************************************
FUNCTION cPattern(cString)
   LOCAL i
	LOCAL aSpecialChars := ".+-*/^$()#"
   
   FOR i:=1 to Len( aSpecialChars )
    	cString = StrTran( cString, aSpecialChars[i], "\" + aSpecialChars[i] ) 
   NEXT i
RETURN cString

