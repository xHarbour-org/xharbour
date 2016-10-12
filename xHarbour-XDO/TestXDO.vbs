OPTION EXPLICIT
 
DIM XDOServer, XDO

DIM Table 
DIM Field  
DIM StartWithB
 
Table      = "Customer" 
Field      = "Last" 
StartWithB = "B"
 
SET XDOServer = CreateObject( "xHarbour.DataObjects" )

SET XDO = XDOServer.Open( Table ) 

XDO.GoTop

' Fields can be accessed as direct properties.
MsgBox XDO.First

XDO.CreateIndex( Field ) 

XDO.Seek( StartWithB ) 


' Field can also be accessed indirectly by name.
MsgBox XDO.Field( "Last" )

XDO.Locate( "State = ""CA""" )
MsgBox XDO.RecNo

XDO.Goto( 1 ) 
MsgBox XDO.RecNo

XDO.Skip( XDO.RecCount + 1 ) 
MsgBox XDO.Bof
MsgBox XDO.Eof

XDO.Previous

XDO.Delete
MsgBox XDO.Deleted

XDO.Recall
MsgBox XDO.Deleted

XDO.Pack
MsgBox XDO.RecCount
XDO.ReIndex
XDO.Close