#define CRLF HB_OsNewLine()

FUNCTION Build_xHB_Setup(cEdition, cOFw, cRevision)

   LOCAL cBuild:="",cAIP,cAI,lDemo:=.F.
   LOCAL cPackageName,cPackageFolder,cProductName

   IF Empty(cOFw)
      cOFw:="C"
   ENDIF

   IF Empty(cRevision)
      cRevision:=CMonth(Date())+"-"+NtoC(Year(Date()))+"-Build"+NtoC(Month(Date()))+NtoC(Day(Date()))
      ? cRevision
   ENDIF

   DO CASE
      CASE IsDirectory("C:\Program Files (x86)\Caphyon\Advanced Installer 10.6\")
                  cAI:="C:\Program Files (x86)\Caphyon\Advanced Installer 10.6\bin\x86\"
      OTHER
          ?     "Geen Advanced installer gevonden"
          Alert("Geen Advanced installer gevonden")
          RETURN
   ENDCASE


   IF Empty(cEdition)
      cEdition:="ENT"
   ELSE
      cEdition:=Upper(cEdition)
   ENDIF

   DO CASE
      CASE cEdition="ENT"   ; cEdition_Full:="Enterprise"
      CASE cEdition="PROF"  ; cEdition_Full:="Professional"
      CASE cEdition="PERS"  ; cEdition_Full:="Personal"
      CASE cEdition="DEMO"  ; cEdition_Full:="Demo"
      OTHER                 ; Alert("Unknown edition") ; Alert(cEdition)
   ENDCASE

   cAIP:="D:\xHB.aip"

   IF cEdition="DEMO"
      cBuild+='COPY \xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-Demo.aip '+cAIP+' /Y'+CRLF
   ELSE
      cBuild+='COPY \xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB.aip '+cAIP+' /Y'+CRLF
   ENDIF
   cBuild+='MD D:\Resources'+CRLF
   cBuild+='COPY \xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\*.jpg D:\Resources\*.jpg /Y'+CRLF
   cBuild+='COPY \xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\*.rtf D:\Resources\*.rtf /Y'+CRLF
   cBuild+='COPY \xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\*.ini D:\xHB-Files\Bin\*.ini /Y'+CRLF
   cBuild+='COPY \xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\*.vbs D:\xHB-Files\*.vbs /Y'+CRLF

   //----------------------------------------------------------------------------------------------------//

   // Shortcuts in SHORTCUTDIR
   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "xPrompt"'+;
            ' -desc "xPrompt"'+;
            ' -target "APPDIR\bin\xPrompt.exe"'+;
			' -icon C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\xPrompt2.ico'+;
            ' -dir SHORTCUTDIR'+;
            ' -wkdir APPDIR'+;
            CRLF+CRLF

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "xEdit"'+;
            ' -desc "xEdit"'+;
            ' -target "APPDIR\bin\xEditW.exe"'+;
            ' -dir SHORTCUTDIR'+;
            ' -wkdir APPDIR\bin'+;
            CRLF+CRLF

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "xBuild Project Builder"'+;
            ' -desc "xBuild Project Builder"'+;
            ' -target "APPDIR\bin\xBuildW.exe"'+;
            ' -dir SHORTCUTDIR'+;
            ' -wkdir APPDIR\bin'+;
            CRLF+CRLF

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "Visual xHarbour"'+;
            ' -desc "Visual xHarbour"'+;
            ' -target "APPDIR\bin\VXH.exe"'+;
            ' -dir SHORTCUTDIR'+;
            ' -wkdir APPDIR\bin'+;
            CRLF+CRLF

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "Visual xDebugger"'+;
            ' -desc "Visual xDebugger"'+;
            ' -target "APPDIR\bin\xDebugW.exe"'+;
			' -icon C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\bug.ico'+;
            ' -dir SHORTCUTDIR'+;
            ' -wkdir APPDIR\bin'+;
            CRLF+CRLF

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "Samples folder"'+;
            ' -desc "Samples folder"'+;
            ' -target "[Samples_DIR]"'+;
            ' -dir SHORTCUTDIR'+;
            CRLF+CRLF

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "Documentation"'+;
            ' -desc "Documentation"'+;
            ' -target "[Doc_DIR]"'+;
            ' -dir SHORTCUTDIR'+;
            CRLF+CRLF

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "SQLRDD Documentation"'+;
            ' -desc "SQLRDD Documentation"'+;
            ' -target "[SQLRDD_1_DIR]"'+;
            ' -dir SHORTCUTDIR'+;
            CRLF+CRLF

   // Shortcut in /xHarbour on the Web
   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "Visual xHarbour"'+;
            ' -desc "Visual xHarbour"'+;
            ' -target "APPDIR\Visual xHarbour.url"'+;
			' -icon C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\url.ico'+;
            ' -dir SHORTCUTDIR\"xHarbour On The Web"'+;
            CRLF+CRLF

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "xHarbour Builder Buttons"'+;
            ' -desc "xHarbour Builder Buttons"'+;
            ' -target "APPDIR\xHarbour Builder Buttons.url"'+;
            ' -icon C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\url.ico'+;
            ' -dir SHORTCUTDIR\"xHarbour On The Web"'+;
            CRLF+CRLF

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "xHarbour documentation online"'+;
            ' -desc "xHarbour documentation online"'+;
            ' -target "APPDIR\xHarbour.doc.url"'+;
			' -icon C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\url.ico'+;
            ' -dir SHORTCUTDIR\"xHarbour On The Web"'+;
            CRLF+CRLF

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "xHarbour.com website"'+;
            ' -desc "xHarbour.com website"'+;
            ' -target "APPDIR\xHarbour.com.url"'+;
            ' -icon C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\url.ico'+;
            ' -dir SHORTCUTDIR\"xHarbour On The Web"'+;
            CRLF+CRLF

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "xHarbour.org website"'+;
            ' -desc "xHarbour.org website"'+;
            ' -target "APPDIR\xHarbour.org.url"'+;
            ' -icon C:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\url.ico'+;
            ' -dir SHORTCUTDIR\"xHarbour On The Web"'+;
            CRLF+CRLF


   // Shortcuts on desktop
   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "Visual xHarbour"'+;
            ' -desc "VXH"'+;
            ' -target "APPDIR\bin\VXH.exe"'+;
            ' -dir DesktopFolder'+;
            ' -wkdir APPDIR\bin'+;
            CRLF+CRLF


   //----------------------------------------------------------------------------------------------------//

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+;
            ' /SetProperty wf_Edition="'+cEdition_Full+'"'+CRLF

   //----------------------------------------------------------------------------------------------------//

   cPackageFolder:='C:\odrive\Gdrive (xHarbour Builder)\xHB-SetupFiles\xHB\xHB Win-Installers\xHB-'+cRevision+'\'
   cProductName  :='xHarbour Builder '+cEdition_Full
   cPackageName  :=cProductName+'-'+cRevision+'.exe'

// cBuild+='RD D:\xHB /Q /S'+CRLF

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /SetPackageName "'+cPackageFolder+cPackageName+'"'+CRLF
   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /SetProperty ProductName="'+cProductName+'"'+CRLF

   //----------------------------------------------------------------------------------------------------//

   cBuild+='"'+cAI+'AdvancedInstaller.com" /build '+cAIP+CRLF+CRLF

   cBuild+='COPY \xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Serial.txt "'+cPackageFolder+'\serial.txt" /Y'+CRLF

   cBuild+='RD D:\xHB-cache /Q /S'+CRLF
   cBuild+='RD D:\Resources /Q /S'+CRLF
   cBuild+='RD D:\Setups /Q /S'+CRLF
   cBuild+='DEL '+cAIP+' /Q'+CRLF
   cBuild+='ATTRIB +H D:\xHB-Files'+CRLF
	
//	cBuild+='COPY D:\'+cPackageName+' 

// cBuild+='pause'+CRLF+CRLF

   MemoWrit("D:\Build.bat",cBuild,.F.)
   __Run("D:\Build.bat")
   DELETE FILE ("D:\Build.bat")

RETURN NIL


//----------------------------------------------------------------------------------------------------//


