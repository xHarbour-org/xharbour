Function Main()
	local nH, cRead := space(120)
	nH := fOpen( "rline1.prg" )
   fReadLine( nH, @cRead, 120, { chr(10) } )
   ? cRead
   ? len(cRead)
   fReadLine( nH, @cRead, 120, { chr(13) } )
   ? cRead
   ? len(cRead)
   fReadLine( nH, @cRead, 120, { chr(13)+chr(10) } )
   ? cRead
   ? len(cRead)
   fReadLine( nH, @cRead, 120, chr(13)+chr(10) )
   ? cRead
   ? len(cRead)

	fClose( nH )

return
