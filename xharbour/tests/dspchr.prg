#define POL_MAZ   "èïêú•£ò†°Üçëí§¢û¶ß"
#define POL_ISO   "°∆ £—”¶¨Ø±ÊÍ≥ÒÛ∂ºø"
#define POL_852   "§è®ù„‡óçΩ•Ü©à‰¢ò´æ"
#define POL_WIN   "•∆ £—”åèØπÊÍ≥ÒÛúüø"

REQUEST HB_CODEPAGE_PLMAZ
REQUEST HB_CODEPAGE_PLISO
REQUEST HB_CODEPAGE_PL852
REQUEST HB_CODEPAGE_PLWIN

function main( cTermCP, cHostCP, lBoxChar )
local i, j, x

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
//HB_SETDISPCP( cTermCP, cHostCP, lBoxChar )
//HB_SETKEYCP( cTermCP, cHostCP )
HB_SETTERMCP( cTermCP, cHostCP, lBoxChar )

? "Host codpage: " + cHostCP + ", terminal codepage: " + cTermCP
? HB_GT_VERSION()
?
for i := 0 to 15
    for j := 0 to 15
        x := i * 16 + j
        ?? "  " + chr( iif( x<32, 42, x ) )
    next
    ?
next
? "ISO-8859-2: say[ " + POL_ISO + " ]"; dspboxch( ", box[ " + POL_ISO + " ]" )
? "   Mazovia: say[ " + POL_MAZ + " ]"; dspboxch( ", box[ " + POL_MAZ + " ]" )
? "   CP-1250: say[ " + POL_WIN + " ]"; dspboxch( ", box[ " + POL_WIN + " ]" )
? "    CP-852: say[ " + POL_852 + " ]"; dspboxch( ", box[ " + POL_852 + " ]" )
?

while (x:=inkey(0)) != 13 // K_ENTER
    ? "Keycode = " + ltrim(str(x)) + ", char = " + chr(x)
enddo
?
return nil

function dspboxch( cStr )
local i, r, c
for i := 1 to len( cStr )
    r:=row(); c:=col()
    dispbox( r, c, r, c, substr( cStr, i, 1 ) )
    setpos(r,c+1)
next
return nil
