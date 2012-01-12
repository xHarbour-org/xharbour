#include "compat.ch"
#include "hbclass.ch"

**************************************************
function cJoin(aArray, cString)
	local result := "", i
	if (len(aArray)>0)
		for i:=1 to len(aArray) - 1 
			result += aArray[i]	+ cString
		next i
		result += aArray[len(aArray)]	
	endif
return result

**************************************************
function xSelect(aArray, bSelector)
	local newArray := array(len(aArray))
	aeval(aArray, {|x,n| newArray[n] := eval(bSelector, x)})
return newArray
	
**************************************************
function xSelectMany(aArray, bSelector)
	local newArray := {}
	aeval(aArray, {|x| aAddRange(newArray, eval(bSelector, x))})
return newArray	
	
**************************************************
function aWhere(aArray, bPredicate)	
	local newArray := {}, i
	for i:=1 to len(aArray)
		if(eval(bPredicate, aArray[i]))
			aadd(newArray, aArray[i])
		endif
	next i
return newArray	
	
**************************************************
function xFirst(aArray, bPredicate)
	local i := ascan(aArray, bPredicate)
	if(i==0)
		return nil
	endif
return aArray[i]
	
**************************************************
function xFirstOrDefault(aArray)
	if(len(aArray) == 0)
		return nil
	endif
return aArray[1]	
		
**************************************************
function aDistinct(aArray, bSelector)	
	local newArray := {}, ids := {}, i, id
	for i:=1 to len(aArray)
		if(!(id := eval(bSelector, aArray[i])) in ids)
			aadd(ids, id)
			aadd(newArray, aArray[i])
		endif
	next i
return newArray	
	
**************************************************
PROCEDURE aAddRange(aArray1, aArray2)
   LOCAL i
	for i:=1 to len(aArray2)
		aadd(aArray1, aArray2[i]) 
	next i
RETURN

**************************************************
PROCEDURE aAddDistinct(aArray1, xValue, bSelector)
	local id
	if(bSelector == nil)
		bSelector = {|x| x}
	endif
	id := eval(bSelector, xValue)
	if(ascan(aArray1, {|x| id == eval(bSelector, x)})==0)
		aadd(aArray1, xValue)
	endif
return
	
**************************************************
PROCEDURE aAddRangeDistinct(aArray1, aArray2, bSelector)
   LOCAL i
	for i:=1 to len(aArray2)
		aAddDistinct(aArray1, aArray2[i], bSelector) 
	next i
return	

**************************************************
PROCEDURE RemoveAll(aArray, bPredicate)
	local i
	for i:=1 to len(aArray)
		if(eval(bPredicate, aArray[i]))
			 adel(aArray, i, .T.)
			 i--
		endif
	next i		
return

**************************************************
function aReplaceNilBy(aArray, xValue)
return aeval(aArray, {|x,n| if(x == nil, aArray[n] := xValue, )})	

**************************************************
class Dictionary
	HIDDEN:
	data aInternArray init {}
	
	EXPORTED:
	method aAdd(xKey, xValue, nMode)

	EXPORTED:
	method xValue(xKey)

	EXPORTED:
	method Remove(xKey)
		
	EXPORTED:
	method GetKeyValuePair(xKey)
			
	EXPORTED:
	method At(nIndex)
		
	EXPORTED:
	access nLength inline len(::aInternArray)  
	
	EXPORTED:
	method SetValue(xKey, xValue)

	EXPORTED:
	method nIndexOfKey(xKey)
	
	EXPORTED:
	method Clear()
	
	EXPORTED:
	method lContainsKey()	
endclass

//nMode = 1 : If the key exist, an exception is thrown
//nMode = 2 : If the key exist, the method does nothing
//nMode = 3 : If the key exist, the value is replaced
method aAdd(xKey, xValue, nMode) class Dictionary
	local lContainsKey := ::lContainsKey(xKey)
	if(!nMode in {1,2,3})
		nMode = 1
	endif
	do case
		case (!lContainsKey)
			aadd(::aInternArray, KeyValuePair():new(xKey, xValue))
		case (nMode == 1 .and. lContainsKey)
			Throw(ErrorNew(,,,,"The given key already exists in the dictionary"))
		case (nMode == 3 .and. lContainsKey)
			::SetValue(xKey, xValue)
	endcase
return Nil

method GetKeyValuePair(xKey)
	local result := xFirst(::aInternArray, {|y| y:xKey == xKey})
	if(result == nil)
		Throw(ErrorNew(,,,,"The key " + cstr(xKey) + " was not found."))
	endif		
return result

method At(nIndex)		
return ::aInternArray[nIndex]

method xValue(xKey) class Dictionary
return ::GetKeyValuePair(xKey):xValue

method SetValue(xKey, xValue)
	::GetKeyValuePair(xKey):xValue = xValue
return Nil	
	
method nIndexOfKey(xKey)
return ascan(::aInternArray, {|x| x:xKey == xKey})

method Remove(xKey) class Dictionary
	local nIndex := ::nIndexOfKey(xKey)
	if(nIndex == 0)
		Throw(ErrorNew(,,,,"The key " + cstr(xKey) + " was not found."))
	endif
return adel(::aInternArray, nIndex, .T.)

method Clear() class Dictionary
	::aInternArray = {}
return Nil

method lContainsKey(xKey) class Dictionary
return ::nIndexOfKey(xKey) > 0

**************************************************
class KeyValuePair
	EXPORTED:
	data xKey readonly
	
	data xValue
	
	method new(pKey, pValue)
endclass

method new(pKey, pValue)
	::xKey = pKey
	::xValue = pValue
return self

**************************************************
function ToDictionary(aArray, bKeySelector)
	local result := Dictionary():new(), i
	for i:=1 to len(aArray)
		result:aadd(eval(bKeySelector, aArray[i]), aArray[i])
	next i
return result

**************************************************
function GetFileName(cPath)
   LOCAL aGroups
	local cRegEx := "^(?:(\w:(?:\\|/)?)((?:.+?(?:\\|/))*))?(\w+?)(\.\w+)?$"
    if (HB_RegExMatch(cRegEx, cPath, .F.)) 
    	aGroups = HB_RegExAtX(cRegEx, cPath)
		return aGroups[4,1]
	else
		Throw(ErrorNew(,,,, cPath + " is not a valid path"))		
    endif	
RETURN Nil    
