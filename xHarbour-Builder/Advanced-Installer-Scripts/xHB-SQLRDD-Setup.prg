#define CRLF HB_OsNewLine()

FUNCTION Build_xHB_Setup(cOFw,cVersion)

   LOCAL cBuild:="",cAIP,cSource,cTarget,cAI
   LOCAL cPackageName,cPackageFolder,cProductName, cOriginalAIP

   IF Empty(cOFw)
      cOFw:="C"
   ENDIF

   IF Empty(cVersion)
      cVersion:=""
   ENDIF
   cVersion:=Upper(cVersion)

   cRevision:=CMonth(Date())+"-"+NtoC(Year(Date()))+"-Build"+NtoC(Month(Date()))+NtoC(Day(Date()))
   ? ">"+cRevision+"<"

   DO CASE
      CASE IsDirectory("C:\Program Files\Caphyon\Advanced Installer 8.7")
                  cAI:="C:\Program Files\Caphyon\Advanced Installer 8.7\bin\x86\"
      CASE IsDirectory("W:\Program Files\Caphyon\Advanced Installer 8.7")
                  cAI:="W:\Program Files\Caphyon\Advanced Installer 8.7\bin\x86\"

   ENDCASE

   cAIP:="C:\xHB-SetupFiles\xHB.aip"

   IF "DEMO"$cVersion
      cOriginalAIP:= "W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-SQLRDD-Demo.aip"
   ELSE
      cOriginalAIP:= "W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\xHB-SQLRDD.aip"
   ENDIF

   cBuild+='COPY '+cOriginalAIP+' '+cAIP+' /Y'+CRLF
   cBuild+='MD C:\xHB-SetupFiles\Resources'+CRLF
   cBuild+='COPY W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\*.jpg C:\xHB-SetupFiles\Resources\*.jpg /Y'+CRLF
   cBuild+='COPY W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\*.rtf C:\xHB-SetupFiles\Resources\*.rtf /Y'+CRLF
   cBuild+='COPY W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Resources\*.vbs C:\xHB-SetupFiles\xHB-Files\*.vbs /Y'+CRLF

   //----------------------------------------------------------------------------------------------------//

   // Shortcuts in SHORTCUTDIR

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "xHarbour Builder SQLRDD Samples"'+;
            ' -desc "xHarbour Builder SQLRDD Samples"'+;
            ' -target "[sqlrdd_DIR]"'+;
            ' -dir SHORTCUTDIR'+;
            CRLF+CRLF

   // Shortcut in /xHarbour on the Web
   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "xHarbour Builder SQLRDD Manual"'+;
            ' -desc "xHarbour Builder SQLRDD Manual"'+;
            ' -target "APPDIR\doc\SQLRDD Manual.pdf"'+;
            ' -dir SHORTCUTDIR\"Documentation"'+;
            CRLF+CRLF

   // Shortcut in /xHarbour on the Web
   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /NewShortcut'+;
            ' -name "xHarbour Builder SQLRDD Reference"'+;
            ' -desc "xHarbour Builder SQLRDD Reference"'+;
            ' -target "APPDIR\doc\SQLRDD Reference.pdf"'+;
            ' -dir SHORTCUTDIR\"Documentation"'+;
            CRLF+CRLF


   //----------------------------------------------------------------------------------------------------//

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+;
            ' /SetProperty wf_Edition="SQLRDD"'+CRLF

   //----------------------------------------------------------------------------------------------------//

   DO CASE
      CASE cVersion=="XHB-RELEASE"
           cProductName  :='xHarbour Builder SQLRDD'
           cPackageFolder:='C:\xHB-SetupFiles\sqlrdd\windows\'+cRevision+'\xHarbour.com\'
      CASE cVersion=="XHB-DEMO"
           cProductName  :='xHarbour Builder SQLRDD Demo'
           cPackageFolder:='C:\xHB-SetupFiles\sqlrdd\windows\'+cRevision+'\xHarbour.com\'
      CASE cVersion=="BCC-RELEASE"
           cProductName  :='xHarbour Builder SQLRDD for BCC'
           cPackageFolder:='C:\xHB-SetupFiles\sqlrdd\windows\'+cRevision+'\xHarbour.org\'
      CASE cVersion=="BCC-DEMO"
           cProductName  :='xHarbour Builder SQLRDD Demo for BCC'
           cPackageFolder:='C:\xHB-SetupFiles\sqlrdd\windows\'+cRevision+'\xHarbour.org\'
   ENDCASE
   cPackageName  :=cProductName+'-'+cRevision+'.exe'

   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /SetPackageName "'+cPackageFolder+cPackageName+'"'+CRLF
   cBuild+='"'+cAI+'AdvancedInstaller.com" /edit '+cAIP+' /SetProperty ProductName="'+cProductName+'"'+CRLF

   //----------------------------------------------------------------------------------------------------//

   cBuild+='"'+cAI+'AdvancedInstaller.com" /build '+cAIP+CRLF+CRLF

   cBuild+='COPY W:\xHarbour.com\xHarbour-Builder\Advanced-Installer-Scripts\Serial.txt "'+cPackageFolder+'\serial.txt" /Y'+CRLF

   cBuild+='RD C:\xHB-SetupFiles\xHB-cache /Q /S'+CRLF
   cBuild+='RD C:\xHB-SetupFiles\Resources /Q /S'+CRLF
   cBuild+='DEL '+cAIP+' /Q'+CRLF
   cBuild+='ATTRIB +H C:\xHB-SetupFiles\xHB-Files'+CRLF

// cBuild+='pause'+CRLF+CRLF

   MemoWrit("C:\xHB-SetupFiles\Build.bat",cBuild,.F.)
   __Run("C:\xHB-SetupFiles\Build.bat")
   DELETE FILE ("C:\xHB-SetupFiles\Build.bat")

RETURN NIL


//----------------------------------------------------------------------------------------------------//


