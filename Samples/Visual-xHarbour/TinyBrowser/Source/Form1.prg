#include "vxh.ch"
#include "Form1.xfm"

#define OLECMDID_PAGESETUP 8
#define OLECMDID_PRINTPREVIEW 7
#define OLECMDEXECOPT_PROMPTUSER 1
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD ToolButton1_OnClick( Sender ) CLASS Form1
   ::WebBrowser1:Url:="http://www.xharbour.com/xhdn/referenceguide"   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolButton2_OnClick( Sender ) CLASS Form1
   ::WebBrowser1:ExecWB( OLECMDID_PAGESETUP, ;
                         OLECMDEXECOPT_PROMPTUSER, ;
                         NIL, ;
                         NIL )   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ToolButton3_OnClick( Sender ) CLASS Form1
   ::WebBrowser1:ExecWB( OLECMDID_PRINTPREVIEW, ;
                         OLECMDEXECOPT_PROMPTUSER, ;
                         NIL, ;
                         NIL )      
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD WebBrowser1_BeforeNavigate2( Sender, pDisp, URL, Flags, TargetFrameName, PostData, Headers, Cancel ) CLASS Form1
   ::Disable()
   ::Cursor:=IDC_WAIT   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD WebBrowser1_DocumentComplete( Sender, pDisp, URL ) CLASS Form1
   ::Enable()
   ::Cursor:=IDC_ARROW   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD WebBrowser1_NavigateError( Sender, pDisp, URL, Frame, StatusCode, Cancel ) CLASS Form1
   ::Enable()
   ::Cursor:=IDC_ARROW   
RETURN Self