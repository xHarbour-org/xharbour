#include "compat.ch"
#include "dbinfo.ch"
#include "hbclass.ch"
#include "sqlrdd.ch"
#include "inkey.ch"
//#define DEBUG

**************************************************
CLASS ExpressionTranslator
   HIDDEN:
   data _oExpressionSimplifier

   HIDDEN:
   data _oConditionSimplifier

   HIDDEN:
   data _aComparisonOperators

   HIDDEN:
   data _aLogicalOperators

   HIDDEN:
   data _aArithmeticOperators

   PROTECTED:
   data _oDefaultContext

   PROTECTED:
   data cAs init "as"

   PROTECTED:
   data cTrue init "true"

   PROTECTED:
   data cFalse init "false"

   PROTECTED:
   data cNull init "null"

   EXPORTED:
   data aUDF init  {}

   EXPORTED:
   data lFixVariables init .F.

   EXPORTED:
   data lSimplifyCondition init .T.

   EXPORTED:
   data lIndexExpression init .T.

   EXPORTED:
   data lFetchJoin init .T.

   EXPORTED:
   data aRelations init {}

   EXPORTED:
   METHOD TranslateRelationExpression(oDirectRelation)

   EXPORTED:
   METHOD GetTranslation(oCondition)

   EXPORTED:
   METHOD Translate(oExpression)

   PROTECTED:
   METHOD InternalTranslate(oExpression)

   PROTECTED:
   METHOD TranslateCondition(oCondition)

   PROTECTED:
   METHOD TranslateComparison(oComparison)

   PROTECTED:
   METHOD TranslateBooleanExpression(oBooleanExpression)

   PROTECTED:
   METHOD TranslateExpression(oExpression)

   PROTECTED:
   METHOD TranslateFunctionExpression(oFunctionExpression)

   PROTECTED:
   METHOD TranslateValueExpression(oValueExpression)

   PROTECTED:
   METHOD TranslateComposition(oExpression)

   PROTECTED:
   METHOD TranslateOperand(oOperand, oOperator)

   PROTECTED:
   METHOD new(pWorkarea, pFixVariables)

   PROTECTED:
   METHOD GetSQLOperator(oOperator)

   PROTECTED:
   METHOD GetComparisonOperators() virtual

   PROTECTED:
   METHOD GetLogicalOperators() virtual

   PROTECTED:
   METHOD GetArithmeticOperators() virtual

   PROTECTED:
   METHOD GetComparisonOperatorSymbol(cName) inline ::GetOperatorSymbol(::GetComparisonOperators(), cName)

   PROTECTED:
   METHOD GetLogicalOperatorSymbol(cName) inline ::GetOperatorSymbol(::GetLogicalOperators(), cName)

   PROTECTED:
   METHOD GetArithmeticOperatorSymbol(cName) inline ::GetOperatorSymbol(::GetArithmeticOperators(), cName)

   HIDDEN:
   METHOD GetOperatorSymbol(aOperators, cName)

   PROTECTED:
   METHOD GetFunctionName(oFunctionExpression)  virtual

   PROTECTED:
   METHOD Deny(cCondition) virtual

   PROTECTED:
   METHOD GetNewTranslator(pFixVariables, pSimplifyCondition, pIndexExpression) virtual

   PROTECTED:
   METHOD GetSQLAlias(oWorkArea)

   PROTECTED:
   METHOD FormatField(oWorkArea, CFieldName)
/*
   PROTECTED:
   METHOD CheckParams(oFunctionExpression)    */

   PROTECTED:
   METHOD AaddRelations(aRelations)inline aAddRangeDistinct(::aRelations,  xSelectMany(aRelations, {|y| iif(y:isKindOf("DirectRelation"), {y}, y:aDirectRelations)}), {|x| x:oWorkArea2:cAlias})

   EXPORTED:
   METHOD new(pWorkarea, pFixVariables, pSimplifyCondition, pIndexExpression)
endclass

METHOD new(pWorkarea, pFixVariables, pSimplifyCondition, pIndexExpression)
   if(valtype(pWorkarea) == "C")
      ::_oDefaultContext = oGetWorkarea(pWorkarea)
   else
      ::_oDefaultContext = pWorkarea
   endif
   ::lSimplifyCondition = pSimplifyCondition == nil .or. pSimplifyCondition
   ::lFixVariables = pFixVariables != nil .and. pFixVariables
   ::lIndexExpression = pIndexExpression ==nil .or. pIndexExpression
   ::_oExpressionSimplifier = ExpressionSimplifier():new(::lFixVariables, .F., ::_oDefaultContext:cAlias)
   if(::lSimplifyCondition)
      ::_oConditionSimplifier = ConditionSimplifier():new(::lFixVariables, .F., ::_oDefaultContext:cAlias)
   endif
RETURN self

METHOD GetTranslation(oCondition) CLASS ExpressionTranslator
   LOCAL translation, i, x
   LOCAL oResult := TranslationResult():new(), aSQLConditions := {}, aClipperConditions := {}
   LOCAL aConditions := SplitCondition( oCondition, {} )

   FOR i := 1 TO len(aConditions)
      ::aRelations := {}
      x = nil
      translation = ::Translate(aConditions[i], @x)
      if(translation == nil)
         translation = aConditions[i]:oClipperExpression:cValue
         if(aConditions[i]:isKindOf("ComposedCondition"))
            translation = "(" + translation + ")"
         endif
         aadd(aClipperConditions, translation)
      else
         if(x == .F.) //x can be null
            aClipperConditions = {}
            aSQLConditions = {translation}
            exit
         elseif(x == .T.)
            loop
         endif
         if(aConditions[i]:isKindOf("ComposedCondition"))
            translation = "(" + translation + ")"
         endif
         aadd(aSQLConditions, translation)
      endif
   next i
   if(len(aClipperConditions) == 0)
      aClipperConditions = {".T."}
   endif
   if(len(aSQLConditions) == 0)
      aSQLConditions = {"1 = 1"}
   endif
   oResult:cClipperCondition := cJoin( aClipperConditions, " .and. " )
   oResult:cSQLCondition     := cJoin( aSQLConditions, " " + ::GetLogicalOperatorSymbol("and") + " " )
RETURN oResult

METHOD Translate( oExpression, x ) CLASS ExpressionTranslator
   LOCAL result, i, oRelation, resultHeader, cOperatorAnd, cFilterCondition, oFilterCondition, oParser, oErr
   LOCAL aInitRelations, aSortedRelations, addedAliases, lProgress
   LOCAL resultFooter := ""
   LOCAL aFilters := {}

   TRY
      result = iif(pcount() == 2, ::InternalTranslate(oExpression, @x), ::InternalTranslate(oExpression))

      if(oExpression:isKindOf("ConditionBase"))
         if(len(::aRelations)>0)
            resultHeader = "exists (select 0 from " + ::_oDefaultContext:cFileName + " " + ::cAs + " " + "A"

            //we have to look for the filter aplying on the workarea in relation. This action could eventually modify ::aRelations
            for i:=1 to len(::aRelations)
               oRelation = ::aRelations[i]
               cFilterCondition = oRelation:oWorkArea2:cFilterExpression

               if (cFilterCondition != nil .and. !cFilterCondition == "")
                  oParser := ConditionParser():new(oRelation:oWorkArea2:cAlias)
                  oFilterCondition = oParser:Parse(cFilterCondition)
                  aadd(aFilters, ::InternalTranslate(oFilterCondition))
               endif

               oRelation:cSQLJoin := ::TranslateRelationExpression(oRelation)
            next i

            cOperatorAnd = " " + ::GetLogicalOperatorSymbol("and") + " "

            if(::lFetchJoin)
               addedAliases = {lower(::_oDefaultContext:cAlias)}
               aInitRelations = aclone(::aRelations)
               aSortedRelations = {}
               do while (len(aInitRelations)>0)
                  lProgress = .F.
                  for i:=1 to len(aInitRelations)
                     if(len(aWhere(aInitRelations[i]:aDependingContexts, {|x| ! lower(x) $ addedAliases })) == 1)
                        aadd(addedAliases, lower(aInitRelations[i]:oWorkArea2:cAlias))
                        aadd(aSortedRelations, aInitRelations[i])
                        adel(aInitRelations, i, .T.)
                        lProgress = .T.
                        i--
                     endif
                  next i
                  if(!lProgress)
                     Throw(ErrorNew(,,,, "Circular dependency in the relations. Pass the parameter lFetchJoin to .F. to avoid this problem."))
                  endif
               enddo

               for i:=1 to len(aSortedRelations)
                  oRelation = aSortedRelations[i]
                  resultHeader += " inner join " + oRelation:oWorkArea2:cFileName + " " + ::cAs + " " +  upper(oRelation:oWorkArea2:cAlias) + " on " + oRelation:cSQLJoin
               next i
            else
               for i:=1 to len(::aRelations)
                  oRelation = ::aRelations[i]
                  resultHeader += ", " + oRelation:oWorkArea2:cFileName + " " + ::cAs + " " + upper(oRelation:oWorkArea2:cAlias)
                  resultFooter += cOperatorAnd + oRelation:cSQLJoin
               next i
            endif
            resultFooter += iif(len(aFilters)>0, cOperatorAnd + cJoin(aFilters, cOperatorAnd), "") + ")"
            result = resultHeader + " where " + result + resultFooter
         endif
      endif
   catch oErr
      #ifdef DEBUG
         throw(oErr)
      #endif
      result = nil
   end
RETURN result

METHOD InternalTranslate(oExpression, x) CLASS ExpressionTranslator
   local result
   if(oExpression:isKindOf("ConditionBase"))
      if(::lSimplifyCondition)
         oExpression = ::_oConditionSimplifier:Simplify(oExpression)
      endif
      if(pcount() == 2 .and. oExpression:lIsSimple .and. oExpression:oExpression:ValueType == "value")
         x = upper(oExpression:Value) == ".T." //Value take denied into account.
      endif
      result = ::TranslateCondition(oExpression)
   elseif(oExpression:isKindOf("Expression"))
      result = ::TranslateExpression(oExpression)
   endif
RETURN result

METHOD TranslateCondition(oCondition) CLASS ExpressionTranslator
   local result
   if(oCondition:isKindOf("Comparison"))
      result = ::TranslateComparison(oCondition)
   elseif(oCondition:isKindOf("BooleanExpression"))
      result = ::TranslateBooleanExpression(oCondition)
   elseif(oCondition:isKindOf("ComposedCondition"))
      result = ::TranslateComposition(oCondition)
   endif
   if(oCondition:lDenied)
      result = ::Deny(result)
   endif
RETURN result

METHOD TranslateComposition(oSerialComposition) CLASS ExpressionTranslator
    local cOperand1 := ::TranslateOperand(oSerialComposition:oOperand1, oSerialComposition:oOperator)
    local cOperand2 := ::TranslateOperand(oSerialComposition:oOperand2, oSerialComposition:oOperator)
    local cOperator := ::GetSQLOperator(oSerialComposition:oOperator):aSymbols[1]
RETURN cOperand1 + " " + cOperator + " " + cOperand2

METHOD TranslateOperand(oOperand, oOperator) CLASS ExpressionTranslator
   local result := ::InternalTranslate(oOperand)
   if((oOperand:isKindOf("ComposedCondition") .or. oOperand:isKindOf("ComposedExpression")) .and. oOperand:oOperator:nPriority < oOperator:nPriority) //oOperand:isKindOf("ISerialComposition") problem with multipleinheritance
      result = "(" + result + ")"
   endif
RETURN result

METHOD TranslateComparison(oComparison) CLASS ExpressionTranslator
RETURN ::TranslateExpression(oComparison:oOperand1) + " " + ::GetSQLOperator(oComparison:oOperator):aSymbols[1] + " " + ::TranslateExpression(oComparison:oOperand2)

METHOD TranslateBooleanExpression(oBooleanExpression) CLASS ExpressionTranslator
RETURN ::TranslateExpression(oBooleanExpression:oExpression)

METHOD TranslateExpression(oExpression) CLASS ExpressionTranslator
   local result

   if(!oExpression:lSimplified)
      oExpression = ::_oExpressionSimplifier:Simplify(oExpression)
   endif
   if(oExpression:isKindOf("ComposedExpression"))
      result = ::TranslateComposition(oExpression)
   elseif(oExpression:isKindOf("FunctionExpression"))
      result = ::TranslateFunctionExpression(oExpression)
   elseif(oExpression:isKindOf("ValueExpression"))
      result = ::TranslateValueExpression(oExpression)
   endif
RETURN result

METHOD TranslateFunctionExpression(oFunctionExpression) CLASS ExpressionTranslator
   LOCAL aParameters, cSQLFunctionName
   LOCAL cFunctionName := oFunctionExpression:cFunctionName
   do case
      case (cFunctionName == "deleted")
         RETURN ::FormatField(oFunctionExpression:oWorkArea, "SR_DELETED")
      case(oFunctionExpression:cFunctionName == "recno")
         RETURN "SR_RECNO"
   endcase
   cSQLFunctionName := ::GetFunctionName(oFunctionExpression)
   aParameters = xSelect(oFunctionExpression:aParameters, {|x| ::InternalTranslate(x:oExpression)})
RETURN cSQLFunctionName + "(" + cJoin(aParameters, ",") + ")"
/*
METHOD CheckParams(oFunctionExpression) CLASS ExpressionTranslator
   if(ascan(oFunctionExpression:aParameters, {|x| x:lIsByRef})>0)
      Throw(ErrorNew(,,,,"The expression cannot be translated because " + oFunctionExpression:cFunctionName + " contains a parameter passed by reference"))
   endif
RETURN */

METHOD TranslateValueExpression(oValueExpression) CLASS ExpressionTranslator
   LOCAL result, aRelations, upperValue

   do case
      case (oValueExpression:ValueType = "field")
         if (upper(::_oDefaultContext:cAlias) != oValueExpression:cContext)
            aRelations = RelationManager():new():GetRelations(::_oDefaultContext:cAlias, oValueExpression:cContext)
            if (len(aRelations) > 1)
               Throw(ErrorNew(,,,,"There is several relations between " + ::_oDefaultContext:cAlias + " and " + oValueExpression:cContext + ". Translation impossible."))
            elseif (len(aRelations) == 1)
               ::AaddRelations(aRelations)
            endif
         endif
         result = ::FormatField(oValueExpression:oWorkArea, oValueExpression:Value)
      case (oValueExpression:ValueType = "variable" .and. !::lFixVariables)
         Throw(ErrorNew(,,,, "The variable " + oValueExpression:Value + " isn't SQL valid"))
      case (oValueExpression:ValueType = "value")
         upperValue = upper(oValueExpression:Value)
         if(upperValue == ".T.")
            result = ::cTrue
         elseif(upperValue == ".F.")
            result = ::cFalse
         elseif(upperValue == "NIL")
            result = ::cNull
         else
            result = oValueExpression:Value
         endif
   endcase
RETURN result

METHOD GetSQLAlias(oWorkArea) CLASS ExpressionTranslator
RETURN iif(oWorkArea == ::_oDefaultContext, "A", upper(oWorkArea:cAlias))

METHOD FormatField(oWorkArea, cFieldName) CLASS ExpressionTranslator
RETURN ::GetSQLAlias(oWorkArea) + "." + upper(alltrim(cFieldName)) //SR_DBQUALIFY

METHOD GetSQLOperator(oOperator) CLASS ExpressionTranslator
   local aSQLOperators
   do case
      case (oOperator:isKindOf("LogicalOperator"))
         aSQLOperators = ::GetLogicalOperators()
      case (oOperator:isKindOf("ArithmeticOperator"))
         aSQLOperators = ::GetArithmeticOperators()
      case (oOperator:isKindOf("ComparisonOperator"))
         aSQLOperators = ::GetComparisonOperators()
   endcase
RETURN aSQLOperators[ascan( aSQLOperators, {|x| x:cName == oOperator:cName} )]

METHOD GetOperatorSymbol(aOperators, cName) CLASS ExpressionTranslator
RETURN xFirst(aOperators, {|x| x:cName == cName}):aSymbols[1]

METHOD TranslateRelationExpression(oDirectRelation) CLASS ExpressionTranslator
   LOCAL aFields1, aFields2, aEqualityFields, i, cRelationExpr, cIndexExpr
   LOCAL oTranslator := ::GetNewTranslator()

   if (!::lIndexExpression .and. !oDirectRelation:lSameLength)
      Throw(ErrorNew(,,,, "Joint between " + oDirectRelation:oWorkArea1:cAlias + " and " + oDirectRelation:oWorkArea2:cAlias + " hasn't be made because it required complex expressions that can be slow to evaluate on the server side. To force the joint, pass the property 'lIndexExpression' of the translator to .T."))
   endif

   oDirectRelation:SimplifyExpression(::_oExpressionSimplifier)
   oDirectRelation:SimplifyIndexExpression(::_oExpressionSimplifier)

   if(oDirectRelation:lSameLength)//we try to make the joint on equality on each field whereas on the translated expressions because is it much faster on the database side : no conversion and no concatenation
      aFields1 := {}
      aFields2 := {}
      if(GetJointsFields(oDirectRelation:oExpression, oDirectRelation:oIndexExpression, oDirectRelation:oWorkArea1, oDirectRelation:oWorkArea2, @aFields1, @aFields2))
         aEqualityFields := {}
         for i:=1 to len(aFields1)
            aadd(aEqualityFields, ::FormatField(oDirectRelation:oWorkArea1, aFields1[i]) + " " + ::GetComparisonOperatorSymbol("equalEqual") + " " + ::FormatField(oDirectRelation:oWorkArea2, aFields2[i]))
         next i
         RETURN cJoin(aEqualityFields, " " + ::GetLogicalOperatorSymbol("and") + " ")
      endif
   endif

   cRelationExpr := oTranslator:Translate(oDirectRelation:oExpression)

   if(cRelationExpr == nil)
      Throw(ErrorNew(,,,, "The translation of the relation expression on " + oDirectRelation:oWorkArea1:cAlias + " into " + oDirectRelation:oWorkArea2:cAlias + " has failed"))
   endif

   ::AaddRelations(oTranslator:aRelations) //There can be a field of a workearea in relation in the relation expression ?

   cIndexExpr = oTranslator:Translate(oDirectRelation:oIndexExpression)

   if(cIndexExpr == nil)
      Throw(ErrorNew(,,,, "The translation of the index expression of " + oDirectRelation:oWorkArea2:cAlias + "   has failed"))
   endif

RETURN cRelationExpr + " "+ ::GetComparisonOperatorSymbol("equal") + " " + cIndexExpr

**************************************************
class MSSQLExpressionTranslator from ExpressionTranslator
   PROTECTED:
   data cAs init ""

   PROTECTED:
   data cTrue init "1"

   PROTECTED:
   data cFalse init "0"

   EXPORTED:
   METHOD new(pWorkarea, pFixVariables)

   PROTECTED:
   METHOD GetFunctionName(oFunctionExpression)

   PROTECTED:
   METHOD GetComparisonOperators()

   PROTECTED:
   METHOD GetLogicalOperators()

   PROTECTED:
   METHOD GetArithmeticOperators()

   PROTECTED:
   METHOD GetNewTranslator(pFixVariables, pSimplifyCondition, pIndexExpression)

   PROTECTED:
   METHOD TranslateComparison(oComparison)

   PROTECTED:
   METHOD TranslateFunctionExpression(oFunctionExpression)

   PROTECTED:
   METHOD TranslateBooleanExpression(oBooleanExpression)

   PROTECTED:
   METHOD Deny(cCondition) inline "not("+cCondition+")"

   EXPORTED:
   METHOD new(pWorkarea, pFixVariables, pSimplifyCondition, pIndexExpression)
endclass

METHOD new(pWorkarea, pFixVariables, pSimplifyCondition, pIndexExpression) CLASS MSSQLExpressionTranslator
   ::aUDF = {"padl", "padr", "padc", "valtype", "transform", "at", "rat", "strtran", "min", "max"}
RETURN ::super:new(pWorkarea, pFixVariables, pSimplifyCondition, pIndexExpression)

METHOD TranslateComparison(oComparison) CLASS MSSQLExpressionTranslator
   LOCAL bLike := {|x| iif((x like "^\'.*\'$"), " like '%" + substr(x, 2, len(x)-2) + "%'", " like '%'+" + x + "+'") }

   if(oComparison:oOperator:cName == "included")
      RETURN ::TranslateExpression(oComparison:oOperand2) + eval(bLike, ::TranslateExpression(oComparison:oOperand1))
   elseif(!set(_SET_EXACT) .and. oComparison:oOperator:cName == "equal" .and. oComparison:oOperand2:GetType()=="C")
      RETURN ::TranslateExpression(oComparison:oOperand1) + eval(bLike, ::TranslateExpression(oComparison:oOperand2))
   elseif(oComparison:oOperand2:isKindOf("ValueExpression") .and. upper(oComparison:oOperand2:Value) == "NIL")
      if(oComparison:oOperator:cName == "equal" .or. oComparison:oOperator:cName == "equalEqual")
         RETURN ::TranslateExpression(oComparison:oOperand1) + " IS NULL"
      elseif(oComparison:oOperator:cName == "different")
         RETURN ::TranslateExpression(oComparison:oOperand1) + " IS NOT NULL"
      else
         throw(ErrorNew(,,,, "null value cannot be compared with the operator " + oComparison:oOperator:cName))
      endif
   else
      RETURN ::super:TranslateComparison(oComparison)
   endif

RETURN Nil

METHOD TranslateFunctionExpression(oFunctionExpression) CLASS MSSQLExpressionTranslator
   LOCAL result, cSavedFormat, dDate
   LOCAL cFunctionName := oFunctionExpression:cFunctionName, firstParam, secondParam, thirdParam, aParamExprs

   //::CheckParams(oFunctionExpression)
   aParamExprs := xSelect(oFunctionExpression:aParameters, {|x| x:oExpression})
   do case
      case (cFunctionName == "substr")
         thirdParam = iif(len(aParamExprs) == 3, ::InternalTranslate(aParamExprs[3]), "999999")
         RETURN "substring("+::InternalTranslate(aParamExprs[1])+", " + ::InternalTranslate(aParamExprs[2]) + "," + thirdParam + ")"
      case (cFunctionName == "cstr")
         RETURN "convert(char, "+::InternalTranslate(aParamExprs[1])+")"
      case (cFunctionName == "val")
         if(set(_SET_FIXED))
            RETURN "convert(decimal(38," + alltrim(str(set(_SET_DECIMALS))) + "), "+::InternalTranslate(aParamExprs[1])+")"
         endif
         RETURN "convert(float, "+::InternalTranslate(aParamExprs[1])+")"
      case (cFunctionName == "int")
         RETURN "round("+::InternalTranslate(aParamExprs[1])+", 0)"
      case (cFunctionName == "alltrim")
         RETURN "ltrim(rtrim("+::InternalTranslate(aParamExprs[1])+"))"
      case (cFunctionName == "dow") //cdow not implemented as it is used to format date values in a textual way.
         RETURN "datepart(weekday, "+::InternalTranslate(aParamExprs[1])+")"
      case (cFunctionName == "iif" .or. cFunctionName == "if")
         if(aParamExprs[2]:isKindOf("ConditionBase"))
            if(aParamExprs[2]:isKindOf("BooleanExpression") .and. aParamExprs[3]:isKindOf("BooleanExpression"))
               secondParam = ::super:TranslateBooleanExpression(aParamExprs[2])
               thirdParam = ::super:TranslateBooleanExpression(aParamExprs[3])
            else
               Throw(ErrorNew(,,,,"TSQL doesn't support condition as the second or third parameter of the 'CASE WHEN ELSE END' structure"))
            endif
         else
            secondParam = ::InternalTranslate(aParamExprs[2])
            thirdParam = ::InternalTranslate(aParamExprs[3])
         endif
         RETURN "CASE WHEN " + ::InternalTranslate(aParamExprs[1]) + " THEN " + secondParam + " ELSE " + thirdParam + " END"
      case (cFunctionName == "at")
         if(len(aParamExprs) <= 3)
            RETURN "charindex(" + ::InternalTranslate(aParamExprs[1]) + ", " + ::InternalTranslate(aParamExprs[2]) + iif(len(aParamExprs) == 3, ", " + ::InternalTranslate(aParamExprs[3]), "") + ")"
         endif
      case (cFunctionName == "islower")   //http://www.simple-talk.com/sql/t-sql-programming/sql-string-user-function-workbench-part-1/
         RETURN ::InternalTranslate(aParamExprs[1]) + " like '[A-Z]%' COLLATE Latin1_General_CS_AI"
      case (cFunctionName == "isupper")
         RETURN ::InternalTranslate(aParamExprs[1]) + " like '[a-z]%' COLLATE Latin1_General_CS_AI"
      case (cFunctionName == "isalpha")
         RETURN ::InternalTranslate(aParamExprs[1]) + " like '[A-Z]%'"
      case (cFunctionName == "isdigit")
         RETURN ::InternalTranslate(aParamExprs[1]) + " like '[0-9]%'"
      case (cFunctionName == "dtos")
         RETURN "convert(char, "+::InternalTranslate(aParamExprs[1])+", 112)"
      case (cFunctionName == "ctod")
         firstParam = ::InternalTranslate(aParamExprs[1])
         if (firstParam like "\'.*\'")
            if (firstParam like "\'\s*\'")
               RETURN ::cNull
            endif
            cSavedFormat = set(_SET_DATEFORMAT)
            dDate = oFunctionExpression:oClipperExpression:Evaluate()
            set date AMERICAN
            result = "'" + dtoc(dDate) + "'"
            set date format cSavedFormat
            RETURN result
         endif
         RETURN firstParam
      case (cFunctionName == "strtran")
         if (len(aParamExprs) < 3)
            RETURN "replace(" + ::InternalTranslate(aParamExprs[1]) + ", " + ::InternalTranslate(aParamExprs[2]) + iif(len(aParamExprs) == 3, ", " + ::InternalTranslate(aParamExprs[3]), ", ''") + ")"
         endif
   endcase
RETURN ::super:TranslateFunctionExpression(oFunctionExpression)

METHOD TranslateBooleanExpression(oBooleanExpression) CLASS MSSQLExpressionTranslator
RETURN ::super:TranslateBooleanExpression(oBooleanExpression) + " = 1"

METHOD GetFunctionName(oFunctionExpression)  class MSSQLExpressionTranslator
   local cFunctionName := oFunctionExpression:cFunctionName
   do case
      case (cFunctionName in {"abs", "left", "right", "replicate", "space", "str", "stuff", "upper", "lower", "ltrim", "rtrim", "year", "month", "day", "len", "exp", "log", "round", "sqrt"} )
         RETURN cFunctionName
      case (cFunctionName in ::aUDF)
         RETURN "xhb." + cFunctionName
      case (cFunctionName == "trim")
         RETURN "rtrim"
      case (cFunctionName == "date")
         RETURN "getdate"
      otherwise
         Throw(ErrorNew(,,,,"No SQL function corresponding to " + cFunctionName + " has been defined!")) //all functions translation should be specified. We could RETURN oFunctionExpression:cFunctionName, but we would have no way to check if the SQL is valid before testing it.
   endcase

RETURN Nil

METHOD GetNewTranslator(pFixVariables, pSimplifyCondition, pIndexExpression)
RETURN MSSQLExpressionTranslator():new(::_oDefaultContext, pFixVariables, pSimplifyCondition, pIndexExpression)

METHOD GetComparisonOperators() CLASS MSSQLExpressionTranslator
   if(::_aComparisonOperators == nil)
      ::_aComparisonOperators :=                               ;
         {                                               ;
             ComparisonOperator():new("equal", {"="}),                  ;
             ComparisonOperator():new("equalEqual", {"="}),             ;
             ComparisonOperator():new("different", {"!="}),          ;
             ComparisonOperator():new("lower", {"<"}),                   ;
             ComparisonOperator():new("higher", {">"}),                  ;
             ComparisonOperator():new("lowerOrEqual", {"<="}),           ;
             ComparisonOperator():new("higherOrEqual", {">="}),          ;
         }
   endif
RETURN ::_aComparisonOperators

METHOD GetLogicalOperators() CLASS MSSQLExpressionTranslator
   if(::_aLogicalOperators == nil)
      ::_aLogicalOperators :=                                     ;
         {                                               ;
             LogicalOperator():new("and", {"and"}),                     ;
             LogicalOperator():new("or", {"or"})                        ;
         }
   endif
RETURN ::_aLogicalOperators

METHOD GetArithmeticOperators() CLASS MSSQLExpressionTranslator
   if(::_aArithmeticOperators == nil)
      ::_aArithmeticOperators :=                               ;
         {                                               ;
             ArithmeticOperator():new("plus", {"+"}),                    ;
             ArithmeticOperator():new("minus", {"-"}),                   ;
             ArithmeticOperator():new("multiplied", {"*"}),              ;
             ArithmeticOperator():new("divided", {"/"}),                 ;
             ArithmeticOperator():new("exponent", {"^"})                 ;
         }
   endif
RETURN ::_aArithmeticOperators

**************************************************
class TranslationResult
   EXPORTED:
   data cSQLCondition

   EXPORTED:
   data cClipperCondition

   EXPORTED:
   METHOD lIsFullSQL inline ::cClipperCondition == nil .or. alltrim(::cClipperCondition) == ""
endclass

**************************************************
class EnchancedDirectRelation from DirectRelation
   HIDDEN:
   data _oExpression

   EXPORTED:
   access oExpression

   EXPORTED:
   METHOD SimplifyExpression(oSimplifier) inline ::_oExpression := oSimplifier:Simplify(::oExpression)

   HIDDEN:
   data _oIndexExpression

   EXPORTED:
   access oIndexExpression

   EXPORTED:
   METHOD SimplifyIndexExpression(oSimplifier) inline ::_oIndexExpression := oSimplifier:Simplify(::oIndexExpression)

   EXPORTED:
   data oSeekIndex readonly

   HIDDEN:
   data _aDependingContexts

   EXPORTED:
   access aDependingContexts

   EXPORTED:
   data nMaxLength readonly

   EXPORTED:
   data lSameLength readonly

   EXPORTED:
   data cSQLJoin

   EXPORTED:
   METHOD new(pWorkarea1, pWorkarea2, pExpression)
endclass

METHOD new(pWorkarea1, pWorkarea2, pExpression) CLASS EnchancedDirectRelation
   LOCAL indexLength

   ::super:new( pWorkarea1, pWorkarea2, pExpression )
   ::oSeekIndex = ::oWorkArea2:GetControllingIndex()
   indexLength = iif(::oSeekIndex == nil, 15, ::oSeekIndex:nLength)
   ::lSameLength = ::oClipperExpression:nLength == indexLength
   ::nMaxLength = min(::oClipperExpression:nLength, indexLength)

RETURN self

METHOD oExpression(xValue) CLASS EnchancedDirectRelation
   LOCAL cRelationExpr

   (xValue)

   if ::_oExpression == nil
      cRelationExpr = ::oClipperExpression:cValue
      if(::oClipperExpression:nLength > ::nMaxLength)
         cRelationExpr = "left(" + cRelationExpr + ", " + str(::nMaxLength) + ")"
      endif
      ::_oExpression = ExpressionParser():new(::oWorkarea1:cAlias):Parse(cRelationExpr)
   endif
RETURN ::_oExpression

METHOD oIndexExpression(xValue) CLASS EnchancedDirectRelation
   LOCAL cIndexExpr

(xValue)

   if ::_oIndexExpression == nil
      cIndexExpr = iif(::oSeekIndex:lIsSynthetic, ::oSeekIndex:aDbFields[1]:cName, ::oSeekIndex:oClipperExpression:cValue)
      if (::oSeekIndex:nLength > ::nMaxLength)
         cIndexExpr = "left(" + cIndexExpr + ", " + str(::nMaxLength) + ")"
      endif
      ::_oIndexExpression = ExpressionParser():new(::oWorkarea2:cAlias):Parse(cIndexExpr)
   endif
RETURN ::_oIndexExpression

METHOD aDependingContexts() CLASS EnchancedDirectRelation
   if ::_aDependingContexts == nil
      ::_aDependingContexts = CollectAliases(::oExpression, CollectAliases(::oIndexExpression, {}))
   endif
RETURN ::_aDependingContexts

**************************************************
class EnchancedRelationFactory from RelationFactory
   EXPORTED:
   METHOD NewDirectRelation(pWorkarea1, pWorkarea2, pExpression) inline EnchancedDirectRelation():new(pWorkarea1, pWorkarea2, pExpression)

   EXPORTED:
   METHOD new()
endclass

METHOD new()
   static instance
   if(instance == nil)
      instance = self
   endif
RETURN instance

**************************************************
function SplitCondition(oCondition, aConditions)
   do while (oCondition:isKindOf("ComposedCondition") .and. oCondition:oOperator:cName == "and")
      SplitCondition(oCondition:oOperand1, aConditions)
      oCondition = oCondition:oOperand2
   enddo
   aadd(aConditions, oCondition)
RETURN aConditions

**************************************************

function GetJointsFields(oRelationExpr, oIndexExpr, oWorkArea1, oWorkArea2, aFields1, aFields2)
   local oField1, oField2
   if(oRelationExpr:isKindOf(oIndexExpr))
      if(oRelationExpr:isKindOf("ComposedExpression"))
         RETURN GetJointsFields(oRelationExpr:oOperand1, oIndexExpr:oOperand1, oWorkArea1, oWorkArea2, @aFields1, @aFields2) .and.;
            GetJointsFields(oRelationExpr:oOperand2, oIndexExpr:oOperand2, oWorkArea1, oWorkArea2, @aFields1, @aFields2)
      elseif(oRelationExpr:isKindOf("FunctionExpression") .and. oRelationExpr:cFunctionName == oIndexExpr:cFunctionName)
         RETURN GetJointsFields(oRelationExpr:aParameters[1]:oExpression, oIndexExpr:aParameters[1]:oExpression, oWorkArea1, oWorkArea2, @aFields1, @aFields2)
      elseif(oRelationExpr:isKindOf("ValueExpression"))
         oField1 = oWorkArea1:GetFieldByName(oRelationExpr:Value) //TODO : we could apply the same strategy with field of other workarea but we have to look for the relations with this workarea and to do this we need to translate an expression. => I don't deal with this case, it's very rare.
         oField2 = oWorkArea2:GetFieldByName(oIndexExpr:Value)
         if(!oField1 == nil .and. !oField2 == nil .and. oField1:cType == oField2:cType .and. oField1:nLength == oField2:nLength)
            aadd(aFields1, oField1:cName)
            aadd(aFields2, oField2:cName)
            RETURN .T.
         endif
      endif
   endif
RETURN .F.
