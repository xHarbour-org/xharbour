#define CRLF HB_OsNewLine()

FUNCTION Build_xHB_VXH_Setup(cEdition, cOFw)

   LOCAL cBuild:="",cAIP,cAI,lDemo:=.F.,cTmp,nTmp1,nTmp2
   LOCAL cPackageName,cPackageFolder,cProductName

   IF Empty(cOFw)
      cOFw:="C"
   ENDIF

   cTmp:=MemoRead("W:\xHarbour.com\Visual-xHarbour\IDE\Source\VXH1.PRG")
   nTmp1:=At("#define VXH_Version",cTmp)
//   cVersion:=SubStr(cTmp,nTmp1+26,3)
//   ? ">"+cVersion+"<"
   nTmp2:=At("#define VXH_BuildVersion",cTmp)
   cRevision:="Build "+CMonth(Date())+"-"+NtoC(Day(Date()))+"-"+NtoC(Year(Date()))+" v"+SubStr(cTmp,nTmp1+26,3)+"."+SubStr(cTmp,nTmp2+26,3)
   ? ">"+cRevision+"<"

   DO CASE
      CASE IsDirectory("C:\Program Files\Caphyon\Advanced Installer 8.7")
                  cAI:="C:\Program Files\Caphyon\Advanced Installer 8.7\bin\x86\"
      CASE IsDirectory("W:\Program Files\Caphyon\Advanced Installer 8.7")
                  cAI:="W:\Program Files\Caphyon\Advanced Installer 8.7\bin\x86\"
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

   cAIP:="C:\xHB-SetupFiles\xHB.aip"

   IF cEdition="DEMO"
      cBuild+='COPY W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-VXH-Demo.aip '+cAIP+' /Y'+CRLF
   ELSE
      cBuild+='COPY W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-VXH.aip '+cAIP+' /Y'+CRLF
   ENDIF
   cBuild+='MD C:\xHB-SetupFiles\Resources'+CRLF
   cBuild+='COPY W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\*.jpg C:\xHB-SetupFiles\Resources\*.jpg /Y'+CRLF
   cBuild+='COPY W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\*.rtf C:\xHB-SetupFiles\Resources\*.rtf /Y'+CRLF
   cBuild+='COPY W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\*.ini C:\xHB-SetupFiles\xHB-Files\Bin\*.ini /Y'+CRLF
   cBuild+='COPY W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\*.vbs C:\xHB-SetupFiles\xHB-Files\*.vbs /Y'+CRLF

   //----------------------------------------------------------------------------------------------------//

   // Shortcuts in SHORTCUTDIR

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "Visual xHarbour"'+;
            ' -desc "Visual xHarbour"'+;
            ' -target "APPDIR\bin\VXH.exe"'+;
            ' -dir SHORTCUTDIR'+;
            ' -wkdir APPDIR\bin'+;
            CRLF+CRLF

   // Shortcut in /xHarbour on the Web
   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "Visual xHarbour"'+;
            ' -desc "Visual xHarbour"'+;
            ' -target "APPDIR\Visual xHarbour.url"'+;
			   ' -icon w:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\url.ico'+;
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

   cPackageFolder:='C:\xHB-SetupFiles\vxh\windows\'+cRevision+'\'
   cProductName  :='Visual xHarbour '+cEdition_Full
   cPackageName  :=cProductName+'-'+cRevision+'.exe'

// cBuild+='RD C:\xHB-SetupFiles\vxh /Q /S'+CRLF

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /SetPackageName "'+cPackageFolder+cPackageName+'"'+CRLF
   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /SetProperty ProductName="'+cProductName+'"'+CRLF

   //----------------------------------------------------------------------------------------------------//

   cBuild+='"'+cAI+'AdvancedInstaller.com" /build '+cAIP+CRLF+CRLF

   cBuild+='COPY W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Serial.txt "'+cPackageFolder+'\serial.txt" /Y'+CRLF

   cBuild+='RD C:\xHB-SetupFiles\xHB-cache /Q /S'+CRLF
   cBuild+='RD C:\xHB-SetupFiles\Resources /Q /S'+CRLF
   cBuild+='RD C:\xHB-SetupFiles\Setups /Q /S'+CRLF
   cBuild+='DEL '+cAIP+' /Q'+CRLF
   cBuild+='ATTRIB +H C:\xHB-SetupFiles\xHB-Files'+CRLF

// cBuild+='pause'+CRLF+CRLF

   MemoWrit("C:\xHB-SetupFiles\Build.bat",cBuild,.F.)
   __Run("C:\xHB-SetupFiles\Build.bat")
   DELETE FILE ("C:\xHB-SetupFiles\Build.bat")

RETURN NIL


//----------------------------------------------------------------------------------------------------//


