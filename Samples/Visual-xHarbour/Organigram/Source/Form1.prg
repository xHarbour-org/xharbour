#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnLoad( Sender ) CLASS Form1
   local i, j, k, oi, oj, aSub, hSub
   local hPers:={ "Sales Department"=>{=>}, "Administration Department"=>{=>}, "Purchases Department"=>{=>} }
   hPers[ "Sales Department" ][ "Exports" ]:={}
   hPers[ "Sales Department" ][ "Home Sales" ]:={ "Travellers", "Agents", "Salesman" }
   hPers[ "Administration Department" ][ "Personnel" ]:={}
   hPers[ "Administration Department" ][ "Accounts" ]:={ "Cashier", "Ledgers" }
   hPers[ "Purchases Department" ][ "Partners" ]:={}
   hPers[ "Purchases Department" ][ "Stock Control" ]:={}                   
   for i:=1 to len( hPers )      
       oi:=::TreeView1:AddItem( hGetKeyAt( hPers, i ), 1 )
       hSub:=hGetValueAt( hPers, i )
       for j:=1 to len( hSub )
          oj:=oi:AddItem( hGetKeyAt( hSub, j ), 2 )
          aSub:=hGetValueAt( hSub, j )
          for k:=1 to len( aSub )
             oj:AddItem( aSub[k], 3 )
          next
       next      
   next
   ::TreeView1:ExPandAll()
   ::Label1:Caption:=""
   ::oEmployees:=NIL
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD TreeView1_AfterSelect( Sender ) CLASS Form1
   ::Label1:Caption:=Sender:GetSelText()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Label1_OnSize( Sender ) CLASS Form1
   Sender:Refresh()   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD PictureBox1_OnLButtonUp( Sender ) CLASS Form1
     //FormEmployees( NIL )
RETURN Self