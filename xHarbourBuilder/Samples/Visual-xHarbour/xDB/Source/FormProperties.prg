GLOBAL EXTERNAL oApp, oErr, oTable
GLOBAL EXTERNAL lShowd, lOpenIndex
GLOBAL EXTERNAL lShared, lReadOnly, lSoftSeek
GLOBAL EXTERNAL cDriver, hDriver
GLOBAL EXTERNAL aIndex
GLOBAL EXTERNAL cFontFace, cFontSize
GLOBAL EXTERNAL cBackColor, cForeColor, cHighlightColor, cHighlightTextColor
GLOBAL EXTERNAL hFontFace, hFontSize, hBack, hFore, hHighlight, hHighlightText

#include "vxh.ch"
#include "FormProperties.xfm"
#include "dbinfo.ch"
//---------------------------------------- End of system code ----------------------------------------//
//----------------------------------------------------------------------------------------------------//
METHOD FormProperties_OnLoad( Sender ) CLASS FormProperties
   local dLastUpdate
   local nColNr, nRowNr, nRowLen, nFileSize, nHeaderLen, nMemoBlock
// current free table's properties   
   select(oTable:Alias)
   dLastUpdate:=DBInfo(DBI_LASTUPDATE)
   nColNr:=DBInfo(DBI_FCOUNT)
   nRowLen:=DBInfo(DBI_GETRECSIZE)
   nHeaderLen:=DBInfo(DBI_GETHEADERSIZE)
   nMemoBlock:=DBInfo(DBI_MEMOBLOCKSIZE)
   nRowNr:=oTable:RecCount()
   nFileSize:=nHeaderLen+nRowNr*nRowLen
   ::EditName:Caption:=oTable:Path+"\"+oTable:FileName
   ::LabelLastUpdate:Caption:=dtoc(dLastUpdate)
   ::LabelColNr:Caption:=ltrim(str(nColNr))
   ::LabelRowLen:Caption:=ltrim(str(nRowLen))
   ::LabelFileSize:Caption:=ltrim(str(nFileSize))
   ::LabelMemoBlock:Caption:=if( valtype(nMemoBlock)=="N", ltrim(str(nMemoBlock)), "N./A.")
   ::LabelRowNr:Caption:=ltrim(str(nRowNr))
   if aIndex <> NIL  // setup grid appearance (about indexes)
      ::MTable:Table:=aIndex
      ::MTable:GoTop()
      with object ::IGrid
         :Font:FaceName:=HFontFace[cFontFace]
         :Font:PointSize:=hFontSize[cFontSize]
         :BackColor:=hBack[cBackColor]
         :ForeColor:=hFore[cForeColor]
         :HighlightColor:=hHighlight[cHighlightColor]      
         :HighlightTextColor:=hHighlightText[cHighlightTextColor]      
         :Update()
      end
   endif
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonClose_OnClick( Sender ) CLASS FormProperties
    ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonStructure_OnClick( Sender ) CLASS FormProperties
   FormShowStructure( ::this )
RETURN Self