What is XBScript?
-----------------

XBScript is a full featured scripting language, compatible with any ActitveScript Host, like:

   - WSH (WScript.exe and CScript.exe)

   - Internet Explorer (IE)

   - Internet Information Server (IIS).


You may embed XBScript source in:

   1. .xbs file, to be executed directly from Console, 
      or by clicking it from Explorer.

   2. .html file to be executed in client browser, same as with
      JScript, VBScript, etc, simply use:

         <SCRIPT LANGUAGE="XBScript">

      See test.htm sample. 

   3. .asp file to be executed on Server, same as with
      JScript, VBScript, etc, simply use:

         <%@ Language=XBScript %>

      See test.asp sample.

   4. to be executed in any language supporting OLE, 
      by means of:

         oSE = CreateObject( "ScriptControl" )
         oSE.AllowUI := .T.

         // Now execute some XBScript
         oSE.Language = "XBScript"
         oSE.executeStatement( "Alert( \"XBScript\" )" )

   5. to be executed by any Script Host compliant with 
      the MS ActiveScript specifications.


How to install?
---------------

1. Extract XBScript.zip to any folder on your system.

2. Open a DOS console in the target folder.

3. Type: RegSvr32 XBScript.dll


How to Test?
------------

1. Open: test.xbs from your explorer

2. Or, from DOS prompt by simply typing:

      test.xbs [Enter]

3. Or open testpage.htm by either of the above methods.

4. Or, if you have an IIS Server:
   
   - Copy ServerVariables.asp to root of any site.

   - Make sure that ASP is enabled, and security settings allow for scripting.

   - From any HTML Client type:

        http://www.yourserver.com/ServerVariables.asp

     If receive a response indicating an error
     correct as per steps above.
 

FAQ:
----

Q. What functions are included in the language?

A. The complete xHarbour R/T libraries, including DBFNTX and DBFCDX. The
   Professional Version adds full ZIp file support, and the Enterprise
   Versions further adds xHarbour.com's unique SQLRDD, which allows full
   SQL Server database access, using the familiar xBase syntax.

--
Q. Can I use UI components, like SAY, GETS, Browse, etc.?

A. No, just like other script engines, script in general should rely
   on the Host methods for UI, as appropriate for the given host.

--
Q. Does XBScript support Host Events, like Button1.OnClick, etc?

A. Yes, XBScript offers full support for all Host Events.

--
Q. Can I use all of Clipper's flow controls, including BEGIN SEQUENCE, etc?

A. Yes, you may use all documented Clipper Flow Controls, PLUS xHarbour extensions
   like TRY/CATCH/FINALLY/END,FOR EACH, and WITH OBJECT.

--
Q. Can I use all of xHarbour syntax extensions?

A. Yes all of xHarbour syntax as of the xHarbour CVS dated Nov-29-2005 are 
   supported. The only features NOT supported are:

      - Module wide STATIC variables
      - Extended Codeblocks (Extended INLINE Methods are supported using 
        other approach)
      - LOCAL, and STATIC variables are simulated using PRIVATE memvars.
      - MEMVARS, and FIELDS declarations are ignored.
      - Detached LOCALs in Codeblocks, as we don't have real LOCALs.

--
Q. Can I use Macros, and Codeblocks?

A. Yes, you may use both.

--
Q. Can I use OOP, and can I define new classes in my scripts?

A. Yes, you have full access to xHarbour OOP, and you can define new 
   classes in Scripts.

--
Q. How about the Clipper pre-processor, can I use #define, #undef #ifdef, 
    #[x]translate and #command directives?

A. Yes, you may use all Clipper #directives, as well as new xHarbour 
   #directives like:  #[x]untranslate, [x]uncommand, Dispose Markers
   (<-x->) etc.

--
Q. Can I access OLE Servers?

A. Yes, full support for OLE is included, check ole.xbs and cdo_email.xbs
   samples.

--
Q. What is the cost of XBScript?

A. XBScript is free of cost, anyone can download it free of cost from:
   http://www.xHarbour.com. Commercial versions (Professional and Enterprise)
   are also available.

--
Q. Can I distribute it freely?

A. No, you are not allowed to distribute xbScript itself it in any form. You may
   freely distribute scripts of of any form, including html and asp pages.

--
Q. How can I make it simple for users to get XBScript so it can be
   used in my HTML pages?

A. XBScript may be downloaded and installed "on-demand" transparently 
   much like Flash, Acrobat Reader, etc., just include the following 
   at the top of your main page:

      <OBJECT ID="XBScript" WIDTH=32 HEIGHT=32
         CLASSID="CLSID:ADB21CAC-9B03-4D64-9097-83B05741FDAF"
         CODEBASE="http://www.xharbour.com/ondemand/xbscript.cab">
      </OBJECT>


For more complete licensing information please read the license.txt file.

Thanks for taking the time to review xbScript, I'm confident you'll like
what you see.

Please don't hesitate to forward your comments to xbScript@xHarbour.com

Best regards,

Ron Pinkas

CEO
http://www.xHarbour.com