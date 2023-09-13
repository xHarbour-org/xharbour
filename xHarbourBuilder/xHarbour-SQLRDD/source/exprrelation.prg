#include "compat.ch"
#include "sqlrdd.ch"
#include "hbclass.ch"
#ifndef __XHARBOUR__
   #include "xhbcls.ch"
#endif

**************************************************
Function NewDbSetRelation(cAlias, bRelation, cRelation, lScoped)
   DbSetRelation(cAlias, bRelation, cRelation, lScoped)
   RelationManager():new():AddRelation(EnchancedRelationFactory():new(), alias(), cAlias, cRelation)
return Nil

**************************************************
Function NewdbClearRelation()
   dbClearRelation()
   RelationManager():new():Clear(alias())
return Nil

**************************************************
Function Newdbclearfilter()
   dbclearfilter()
   oGetWorkarea(alias()):cFilterExpression = ""
return Nil

**************************************************
Function oGetWorkarea(cAlias)
   local result, oerr
   try
      result := &cAlias->(dbInfo( DBI_INTERNAL_OBJECT ))
   catch oErr
      oErr:Description += " (cAlias: " + cstr(cAlias) + ")"
      throw(oErr)
   end
return result

**************************************************
procedure SelectFirstAreaNotInUse()
   LOCAL nArea
   for nArea := 1 TO 65534
      if Empty( alias(nArea) )
         select &nArea
         exit
      endif
   next
RETURN

**************************************************
class RelationBase
   EXPORTED:
   data oWorkarea1

   EXPORTED:
   data oWorkarea2
endclass

**************************************************
class IndirectRelation from RelationBase
   EXPORTED:
   data aDirectRelations init {}

   EXPORTED:
   access oWorkarea1 inline ::aDirectRelations[1]:oWorkarea1

   EXPORTED:
   access oWorkarea2 inline atail(::aDirectRelations):oWorkarea2
endclass

**************************************************
class DirectRelation from RelationBase
   EXPORTED:
   data oClipperExpression readonly

   EXPORTED:
   method new(pWorkarea1, pWorkarea2, pExpression)
endclass

method new(pWorkarea1, pWorkarea2, pExpression)
   if(valtype(pWorkarea1) == "C")
      ::oWorkarea1 = oGetWorkarea(pWorkarea1)
   else
      ::oWorkarea1 = pWorkarea1
   endif
   if(valtype(pWorkarea2) == "C")
      ::oWorkarea2 = oGetWorkarea(pWorkarea2)
   else
      ::oWorkarea2 = pWorkarea2
   endif

   ::oClipperExpression = ClipperExpression():new(::oWorkarea1:cAlias, pExpression, .T.)
return self

**************************************************
class RelationFactory
   EXPORTED:
   method NewDirectRelation(pWorkarea1, pWorkarea2, pExpression) inline DirectRelation():new(pWorkarea1, pWorkarea2, pExpression)

   EXPORTED:
   method new()
endclass

method new()
   static instance
   if(instance == nil)
      instance = self
   endif
return instance

**************************************************
class RelationManager
   HIDDEN:
   data oInternDictionary init Dictionary():new()

   HIDDEN:
   data aDirectRelations init {}

   EXPORTED:
   method AddRelation(oFactory, pAlias1, pAlias2, pExpression)

   EXPORTED:
   method GetRelations(cAlias1, cAlias2)

   EXPORTED:
   method Clear(cAlias)

   HIDDEN:
   method BuildRelations(oIndirectRelation, cAlias1, cAlias2)

   EXPORTED:
   method new()
endclass

method new()
   static instance
   if(instance == nil)
      instance = self
   endif
return instance

method Clear(cAlias) class RelationManager
   ::oInternDictionary:Clear()
   RemoveAll(::aDirectRelations, {|y| lower(y:oWorkarea1:cAlias) == lower(cAlias)})
return Nil

method AddRelation(oFactory, pAlias1, pAlias2, pExpression) class RelationManager
   local cAlias1 := upper(pAlias1)
   local cAlias2 := upper(pAlias2)
   local n := ascan(::aDirectRelations, {|x| upper(x:oWorkarea1:cAlias) == cAlias1 .and. upper(x:oWorkarea2:cAlias) == cAlias2})
   local oNewRelation := oFactory:NewDirectRelation(cAlias1, cAlias2, pExpression)
   if(n > 0)
      ::aDirectRelations[n] = oNewRelation
   else
      aadd(::aDirectRelations, oNewRelation)
   endif
   ::oInternDictionary:Clear()
return Nil

method GetRelations(cAlias1, cAlias2) class RelationManager
   local result := {}, r, i, oDirectRelation
   local dico2
   cAlias1 = upper(cAlias1)
   cAlias2 = upper(cAlias2)

   if(::oInternDictionary:lContainsKey(cAlias1) .and. (dico2 := ::oInternDictionary:xValue(cAlias1)):lContainsKey(cAlias2))
      result = ::oInternDictionary:xValue(cAlias1):xValue(cAlias2)
   else
      for i:=1 to len(::aDirectRelations)
         oDirectRelation = ::aDirectRelations[i]
         if(cAlias1 == upper(oDirectRelation:oWorkarea1:cAlias))
            if(cAlias2 == upper(oDirectRelation:oWorkarea2:cAlias))
               aadd(result, oDirectRelation)
            else
               r = IndirectRelation():new()
               aadd(r:aDirectRelations, oDirectRelation)
               aAddRange(result, ::BuildRelations(r, oDirectRelation:oWorkarea2:cAlias, cAlias2))
            endif
         endif
      next i
      if(dico2 == nil)
         dico2 = Dictionary():new()
         dico2:aadd(cAlias2, result)
         ::oInternDictionary:aadd(cAlias1, dico2 , 3)
      else
         dico2:aadd(cAlias2, result)
      endif
   endif
return result

METHOD BuildRelations(oIndirectRelation, cAlias1, cAlias2) class RelationManager
   LOCAL result := {}, r, i, j, oDirectRelation

   cAlias1 = upper(cAlias1)
   cAlias2 = upper(cAlias2)

   for i:=1 to len(::aDirectRelations)
      oDirectRelation = ::aDirectRelations[i]
      if(cAlias1 == upper(oDirectRelation:oWorkarea1:cAlias))
         oDirectRelation = ::aDirectRelations[i]
         r := IndirectRelation():new()
         for j:=1 to len(oIndirectRelation:aDirectRelations)
            aadd(r:aDirectRelations, oIndirectRelation:aDirectRelations[j])
         next j

         aadd(r:aDirectRelations, oDirectRelation)

         if(oDirectRelation:oWorkarea2:cAlias == cAlias2)
            aadd(result, r)
         else
            aAddRange(result, ::BuildRelations(r, oDirectRelation:oWorkarea2:cAlias, cAlias2))
         endif
      endif
   next i
return result

**************************************************
class DbIndex
   HIDDEN:
   data _aInfos

   HIDDEN:
   data _cName

   EXPORTED:
   access cName inline ::_cName

   HIDDEN:
   data _lIsSynthetic

   EXPORTED:
   access lIsSynthetic

   HIDDEN:
   data _aDbFields

   EXPORTED:
   access aDbFields

   EXPORTED:
   data oClipperExpression readonly

   HIDDEN:
   data _nLength

   EXPORTED:
   access nLength

   EXPORTED:
   data oWorkarea

   EXPORTED:
   method new(pWorkarea, pName)
endclass

method new(pWorkarea, pName)
   if(valtype(pWorkarea) == "C")
      ::oWorkarea = oGetWorkarea(pWorkarea)
   else
      ::oWorkarea = pWorkarea
   endif
   ::_cName = upper(pName)
   ::_aInfos = aWhere(pWorkarea:aIndex, {|x| x[10] == ::_cName})[1]
   ::oClipperExpression = ClipperExpression():new(::oWorkarea:cAlias, ::_aInfos[4], ::lIsSynthetic)
return self

method lIsSynthetic() class DbIndex
   if(::_lIsSynthetic == nil)
      ::_lIsSynthetic = (::_aInfos[9] == "")
   endif
return ::_lIsSynthetic

method aDbFields() class DbIndex
   local i
   if(::_aDbFields == nil)
      ::_aDbFields = {}
      if(::lIsSynthetic())
         //::oClipperExpression:nLength will evaluate the index expression which is a bit slow. It would be nice to have access to the legnth of a synthetic index.
         aadd(::_aDbFields, DbField():new(HB_RegExAtX(".*\[(.*?)\]", ::_aInfos[1], .F.)[2,1], "C", ::oClipperExpression:nLength)) //the way to get the name of the field that contains the synthetic index isn't very clean... We also suppose that the synthtic index has a fix length
      else
         for i:=1 to len(::_aInfos[3]) - 1 //not SR_RECNO
            aadd(::_aDbFields, ::oWorkarea:GetFieldByName(::_aInfos[3][i][1]))
         next i
      endif
   endif
return ::_aDbFields

method nLength() class DbIndex
   local i
   if(::_nLength == nil)
      ::_nLength = 0
      for i:=1 to len(::aDbFields)
         ::_nLength += ::aDbFields[i]:nLength
      next i
   endif
return ::_nLength

**************************************************
class DbField
   EXPORTED:
   data cName readonly

   EXPORTED:
   data cType readonly

   EXPORTED:
   data nLength readonly

   EXPORTED:
   method new(pName, pType, pLength)
endclass

method new(pName, pType, pLength)
   ::cName := pName
   ::cType := pType
   ::nLength := pLength
return self

**************************************************
class ClipperExpression
   EXPORTED:
   data lIgnoreRelations

   EXPORTED:
   data cContext readonly

   EXPORTED:
   data cValue readonly

   HIDDEN:
   data _cEvaluation

   HIDDEN:
   method cEvaluation

   HIDDEN:
   data _cType

   EXPORTED:
   access cType

   HIDDEN:
   data _nLength

   EXPORTED:
   access nLength

   EXPORTED:
   method Evaluate()

   EXPORTED:
   method new(pContext, pValue)
endclass

method new(pContext, pValue, pIgnoreRelations) CLASS ClipperExpression
   ::cContext = pContext
   ::cValue = pValue
   ::lIgnoreRelations = pcount() == 3 .and. pIgnoreRelations
return self

method cEvaluation() CLASS ClipperExpression
   if(::_cEvaluation == nil)
      ::_cEvaluation = cstr(::Evaluate(::lIgnoreRelations))
   endif
return Nil

method Evaluate(lIgnoreRelations) CLASS ClipperExpression
   LOCAL nSeconds, save_slct, Result, oErr

   //can be very slow with relations...
   nseconds := seconds()

   try
      if(pcount() == 1 .and. lIgnoreRelations)

         save_slct := select()

         SelectFirstAreaNotInUse()

         use &(oGetWorkarea(::cContext):cFileName) via "SQLRDD" alias "AliasWithoutRelation"

         result = &(::cValue)

         close ("AliasWithoutRelation")

         select(save_slct)
      else
         result = &(::cContext)->(&(::cValue))
      endif
   catch oErr
      oErr:description += ";The value unseccessfully evaluated was : " + ::cValue   + ";"
      throw(oErr)
   end

return result

method cType() CLASS ClipperExpression
   if(::_cType == nil)
      ::_cType = valtype(::cEvaluation())
   endif
return ::_cType

method nLength() CLASS ClipperExpression
   if(::_nLength == nil)
      ::_nLength = len(::cEvaluation())
   endif
return ::_nLength

**************************************************

function ExtendWorkarea()
   extend class SR_WORKAREA with data aIndexes
   extend class SR_WORKAREA with method GetIndexes
   extend class SR_WORKAREA with method GetControllingIndex

   extend class SR_WORKAREA with data aDbFields
   extend class SR_WORKAREA with method GetFields
   extend class SR_WORKAREA with method GetFieldByName

   extend class SR_WORKAREA with data cFilterExpression

   override method ParseForClause in class SR_WORKAREA with NewParseForClause
return Nil

function GetIndexes(lOrdered)
   local self := HB_QSelf(), i
   lOrdered = lOrdered == nil .or. lOrdered
   if ::aIndexes == nil
      ::aIndexes = {}
      for i:=1 to len(::aIndex)
         if (::aIndex[i,10] like "^\w+$")
            aadd(::aIndexes, DbIndex():new(self, ::aIndex[i,10]))
         endif
      next i
   endif
   if lOrdered //order can change with set order to => we could also redefine DbSetOrder() to sort aIndexes each time the order change.
      asort(::aIndexes, {|x, y| &(::cAlias)->(OrdNumber(x:cName)) < &(::cAlias)->(OrdNumber(y:cName))})
   endif
return ::aIndexes

function GetControllingIndex()
   local self := HB_QSelf()
   local aIndexes := ::GetIndexes(.F.)
   local nIndex := &(::cAlias)->(OrdNumber())
   if(nIndex == 0)
      return nil
   endif
return aIndexes[nIndex]

function GetFields()
   local self := HB_QSelf(), i, save_slct, nCount, _aTypes, _aNames, _aLengths
   if (::aDbFields == nil)

      save_slct := select()
      select(::cAlias)
      nCount = fcount()

      _aTypes := Array(nCount)
      _aNames := Array(nCount)
      _aLengths := Array(nCount)
      ::aDbFields := Array(nCount)
      AFields(_aNames, _aTypes, _aLengths)
      for i:=1 to nCount
         ::aDbFields[i] = DbField():new(_aNames[i], _aTypes[i], _aLengths[i])
      next i
      select(save_slct)
   endif
return ::aDbFields

function GetFieldByName(cName)
   local self := HB_QSelf()
return xFirst(::GetFields(), {|x| lower(x:cName) == lower(cName)})

//should be implemented : GetTranslations() and lFixVariables
function NewParseForClause(cFor, lFixVariables)
   local self := HB_QSelf(), oParser, otranslator, oCondition
   ::cFilterExpression = cFor

   oParser := ConditionParser():new(::cAlias)
      otranslator := MSSQLExpressionTranslator():new(::cAlias, lFixVariables, .T.)

   oCondition = oParser:Parse(cFor)
return otranslator:Translate(oCondition)

