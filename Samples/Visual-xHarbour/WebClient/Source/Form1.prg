#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//

/*
This sample project is intended to demonstrate the communication
of a VxH client program with a server-side script.
The setup is typical to shared hosting, and in situations like traveling,
when persistent connection to a server is not possible.
*/


function my_GenUploadForm( cUrl )
   local cForm:=""
   cForm += '<form name="MyUploadForm" '
   cForm += 'method="post" '
   cForm += 'action="' + cUrl + '" '
   cForm += 'enctype="multipart/form-data">' + hb_osnewline()
   cForm += '<input type="file" name="myfile" value="" />' + hb_osnewline()
   cForm += '<input type="submit" name="mysubmit" value="Upload" />' + hb_osnewline()
   cForm += '</form>'
return cForm



function my_hexpack( cIn, lPack )
// data sent via HTTP request needs to be "url-encoded";
// here the hexa encoding is used, because it's flawless, language-neutral,
//and handy for sending several kilobytes of string data at once ( numbers and text )
   local i, n:=len(cIn), cOut:=''
   if n < 1
      return ""
   endif
   if lPack
      for i:=1 to n
         cOut+=numtohex( cIn[i], 2 )
      next
   else
      for i:=1 to n step 2
         cOut+=chr( hextonum( cIn[i]+cIn[i+1] ) )
      next
   endif
return cOut



function my_putstring( hVals )
// encoding and aggregating individual data fields for http GET
   local i, n:=len( hVals )
   local key, val, cGet:="", a
   if n < 1
      return ""
   endif
   for i:=1 to n
      a:=hgetpairat( hVals, i )
      key:=xstr( a[1] )
      val:=my_hexpack( xstr( a[2] ), .t. )
      if i > 1
         cGet+="&"
      endif
      cGet += key+"="+val
   next
return cGet



function my_GenSendForm( cMethod, cUrl, hVals )
// this function can be used for sending data both with GET and POST methods;
// the function generates a HTML form  filled with the encoded input data;
// the "input type" can be "text" - here the "hidden" type is used
   local i, n:=len( hVals )
   local key, val, a, cForm:=""
   
   cForm += '<html><body>'
   cForm += '<form name="MySendForm" method="' + cMethod + '" action="' + cUrl +'">' + hb_osnewline()
   for i:=1 to n
      a:=hgetpairat( hVals, i )
      key:=xstr( a[1] )
      val:=my_hexpack( xstr( a[2] ), .t. )
      cForm += '<input type="hidden" name="' + key + '" value="' + val + '" />' + hb_osnewline()
   next
   cForm += '<input type="submit" name="button" value="go" />' + hb_osnewline()
   cForm += '</form></body></html>'
   
return cForm



function my_getstring( cbuf )
// the HTTP response is decoded and placed in the "output data" groupbox
   local i, nstart:=1, nend:=0, hVals:=hash()
   local nrows:=0, crow
   if valtype( cBuf ) <> "C"
      return hVals
   endif
   if len(cbuf) < 1
      return hVals
   endif
   
   nend:=at( chr(44), cbuf, nstart )
   if nend < 1
      return hVals
   endif
   
   nstart:=nend+1
   nrows:=val( left( cbuf, nEnd-1 ) )
   if nrows<1 .or. nrows>20
      return hvals
   endif
   
   for i:=1 to nrows
      nend:=at( chr(44), cbuf, nstart )
      if nend < 1
         exit
      endif
      crow:=substr( cbuf, nstart, nend-nstart )
      nstart:=nend+1
      hVals[i] := my_hexpack( crow, .f. )
   next
   
return hVals



//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnLoad( Sender ) CLASS Form1
// the WebBrowser is used for "background" operations,
// except when uploading a file via http POST
   ::UploadPanel:Visible:=.f.
   ::Progressbar1:Visible:=.f.
   ::Edit1:Caption:=""
   ::EDit2:Caption:=""
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD WebBrowser1_DownloadBegin( Sender ) CLASS Form1
   ::ProgressBar1:Visible:=.t.
   ::Cursor:=IDC_WAIT
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD WebBrowser1_DownloadComplete( Sender ) CLASS Form1
   ::ProgressBar1:Visible:=.f.   
   ::UploadPanel:Visible:=.f.
   ::Cursor:=IDC_ARROW
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD WebBrowser1_ProgressChange( Sender, Progress, ProgressMax ) CLASS Form1
   ::ProgressBar1:StepIt()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD WebBrowser1_DocumentComplete( Sender, pDisp, URL ) CLASS Form1
// the HTTP response is extracted from the received document
   local i, hGet, aget
   if ::WebPage == "blank"
      return Self
   endif
   
   if ::WebPage == "error"
      alert( "Request error" )
      return Self
   endif

   if ::WebPage == "upload"
      ::MessageBox( ::WebBrowser1:Document:Body:InnerText, "", MB_ICONINFORMATION )
      return Self
   endif

   try  // response to GET or POST
      
      hGet:=my_getstring( @::WebBrowser1:Document:Body:InnerText )
      
      for i:=1 to len(hGet)
         aget:=hgetpairat( hget, i )
         if i == 1
            ::Edit3:Caption:=aget[2]
         elseif i == 2
            ::Edit4:Caption:=aget[2]
         else
            alert( str(aget[1]) + '  ' + aget[2] )
         endif
      next
      
   catch
      alert('Data decoding error')
   end
      
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD WebBrowser1_NavigateError( Sender, pDisp, URL, Frame, StatusCode, Cancel ) CLASS Form1
   ::WebPage:="error"
   Cancel:=.t.
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyGet_OnClick( Sender ) CLASS Form1
// the "input data" is collected, prepared and sent via HTTP request - GET method;
// GET is suitable when sending less then one kilobyte of data at once - length before encoding;
// Note: in the 1990's servers supported maximum 255 bytes in the GET header
   local cGet, hGet:=hash()
   local navNoHistory:=2, navAllowAutoSearch:=16
   local cUrl:="http://www.MyDomain.com/MySubDomain/aaabbb.xyz"
   ::WebPage:="get"
   hGet['v1']:=::Edit1:Caption
   hGet['v2']:=::Edit2:Caption
   cGet:=my_putstring( hGet )
   ::WebBrowser1:Url := cUrl + "?" + cGet
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyPost_OnClick( Sender ) CLASS Form1
// the "input data" is collected, prepared and sent via HTTP request - POST method;
// POST is necessary when sending more then one kilobyte of data at once - length before encoding;
   local cForm, hData:=hash(), xx
   local cFile:=::Application:Path + "\tmp.html"
   local cUrl:="http://www.MyDomain.com/MySubDomain/aaabbb.xyz"
   ::WebPage:="post"
   hData['v1']:=::Edit1:Caption
   hData['v2']:=::Edit2:Caption
   cForm:=my_GenSendForm( "post", cUrl, hData )
   
   with object ::WebBrowser1
      :Document:Write( cForm )
      //:Refresh()
      :Document:MySendForm:submit()
   end   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD MyUpload_OnClick( Sender ) CLASS Form1
   local cForm
   local cUrl:="http://www.MyDomain.com/MySubDomain/aaabbb.xyz"
   cForm := my_GenUploadForm( cUrl )
   ::UploadPanel:Visible:=.t.
   ::WebPage:="upload"
   with object ::WebBrowser1
      :Document:Write( cForm )
      //:Refresh()
   end   
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnCreate( Sender ) CLASS Form1
   Sender:WebPage:="blank"
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD ButtonCancel_OnClick( Sender ) CLASS Form1
   ::UploadPanel:Visible:=.f.   
   ::WebBrowser1:Url := "about:blank"
   ::WebPage:="blank"
RETURN Self