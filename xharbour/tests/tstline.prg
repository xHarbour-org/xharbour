//345678901234567890123456789012345678901234567890
/* Don't touch these initial lines! */
Function Main()
   local buff, nH
// lin1234
// ##1234567890123456789012345678901234567890qwertyuiop
   nH := fOpen( "tstline.prg" )
   HB_FREADLINE( nH, @buff )
   ? rtrim(buff), len( buff )
   HB_FREADLINE( nH, @buff )
   ? rtrim(buff), len( buff )
   HB_FREADLINE( nH, @buff )
   ? rtrim(buff), len( buff )
   HB_FREADLINE( nH, @buff )
   ? rtrim(buff), len( buff )
   HB_FREADLINE( nH, @buff, { chr(13)+chr(10), chr(10) } )
   ? rtrim(buff), len( buff )
   HB_FREADLINE( nH, @buff )
   ? rtrim(buff), len( buff )
   HB_FREADLINE( nH, @buff )
   ? rtrim(buff), len( buff )

return

