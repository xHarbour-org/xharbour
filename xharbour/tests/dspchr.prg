#define POL_MAZ   "èïêú•£ò†°Üçëí§¢û¶ß"
#define POL_ISO   "°∆ £—”¶¨Ø±ÊÍ≥ÒÛ∂ºø"
#define POL_852   "§è®ù„‡óçΩ•Ü©à‰¢ò´æ"
#define POL_1250  "•∆ £—”åèØπÊÍ≥ÒÛúüø"

REQUEST HB_CODEPAGE_PLMAZ
REQUEST HB_CODEPAGE_PLISO
REQUEST HB_CODEPAGE_PL852
REQUEST HB_CODEPAGE_PLWIN

function main( cTermCP, cHostCP, lBoxChar )
local i, j

if empty( cTermCP )
    cTermCP := "PLISO"
else
    cTermCP := upper( cTermCP )
endif
if empty( cHostCP )
    cHostCP := "PLMAZ"
else
    cHostCP := upper( cHostCP )
endif
lBoxChar := !empty( lBoxChar )

//HB_SETCODEPAGE( cHostCP )
HB_SETDISPCP( cTermCP, cHostCP, lBoxChar )

? "Host codpage: " + cHostCP + ", terminal codepage: " + cTermCP
? HB_GT_VERSION()
?
for i := 0 to 15
    for j := 0 to 15
	?? "  " + chr( i * 16 + j )
    next
    ?
next
?
? "(say) ISO-8859-2:", POL_ISO
? "(say)    Mazovia:", POL_MAZ
? "(say)    CP-1250:", POL_1250
? "(say)     CP-852:", POL_852
?
dspboxch( "(box) ISO-8859-2: " + POL_ISO )
dspboxch( "(box)    Mazovia: " + POL_MAZ )
dspboxch( "(box)    CP-1250: " + POL_1250 )
dspboxch( "(box)     CP-852: " + POL_852 )

inkey(0)
?
return nil

function dspboxch( cStr )
local i, r, c
?
for i := 1 to len( cStr )
    r:=row(); c:=col()
    dispbox( r, c, r, c, substr( cStr, i, 1 ) )
    setpos(r,c+1)
next
return nil
