function main(x)
local bg, fg, n

setblink(empty(x))
scroll()
for bg := 0 to 15
    for fg := 0 to 15
        n := bg * 16 + fg
        @ 5 + bg, 5 + fg * 4 say "["+NUM2HEX(n)+"]" color NTOCOLOR( n )
    next
next
?
?
return nil

static function NTOCOLOR(nClr)
return ltrim( str( int( nClr % 16 ), 2 ) ) + "/" + ;
       ltrim( str( int( nClr / 16 ), 2 ) )

static function NUM2HEX(nVal)
local cHex := "", i, n
for i := 1 to 2
    n := nVal % 16
    cHex := chr( n + iif( n > 9, 55, 48 ) ) + cHex
    nVal := int( nVal / 16 )
next
return cHex
