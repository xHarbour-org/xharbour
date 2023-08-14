#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//


//----------------------------------------------------------------------------------------------------
METHOD Form1_OnLoad( Sender ) CLASS Form1
   ::Label1:Caption := ""
   ::Label2:Caption := ""
RETURN Self


//----------------------------------------------------------------------------------------------------
METHOD Button1_OnClick( Sender ) CLASS Form1
   STATIC nItem := 0
   nItem++
   
   IF ::TreeView1:Selecteditem == NIL
      ::TreeView1:AddItem("Item "+LTRIM(STR(nItem)), nItem)
   ELSE
      ::TreeView1:Selecteditem:AddItem("Item "+LTRIM(STR(nItem)), nItem)
   ENDIF
   
   ::TreeView1:Expandall()
RETURN Self

//----------------------------------------------------------------------------------------------------
METHOD Button2_OnClick( Sender ) CLASS Form1
   IF ::TreeView1:Selecteditem == NIL
      ALERT("Please select an item / node to delete.")
   ELSE
      ::TreeView1:Selecteditem:Delete()
   ENDIF
RETURN Self


//----------------------------------------------------------------------------------------------------
METHOD TreeView1_AfterSelect( Sender ) CLASS Form1
   ::Label1:Caption := "Current : "+::TreeView1:Selecteditem:Caption + " (Level : "+LTRIM(STR(::TreeView1:Selecteditem:Level))+")"
   IF ::TreeView1:Previousitem == NIL
      ::Label2:Caption := ""
   ELSE
      ::Label2:Caption := "Previous : "+::TreeView1:PreviousItem:Caption
   ENDIF
RETURN Self
