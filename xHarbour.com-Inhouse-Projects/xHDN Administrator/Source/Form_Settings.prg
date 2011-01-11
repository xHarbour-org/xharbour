GLOBAL EXTERNAL System
GLOBAL EXTERNAL Application

GLOBAL EXTERNAL AppName, AppVersion, AppCaption
GLOBAL EXTERNAL oIni

GLOBAL aGMT

#translate xCRLF => CHR(13) + CHR(10)

#include "vxh.ch"
#include "Form_Settings.xfm"
//---------------------------------------- End of system code ----------------------------------------//

//----------------------------------------------------------------------------------------------------//
METHOD Form_Settings_OnLoad( Sender ) CLASS Form_Settings

   aGMT := { ;
   "GMT-12:00", "GMT-11:00", "GMT-10:00", "GMT-09:00", "GMT-08:00", "GMT-07:00", "GMT-07:00", "GMT-07:00", "GMT-06:00", "GMT-06:00", ;
   "GMT-06:00", "GMT-06:00", "GMT-05:00", "GMT-05:00", "GMT-05:00", "GMT-04:00", "GMT-04:00", "GMT-04:00", "GMT-03:30", "GMT-03:00", ;
   "GMT-03:00", "GMT-03:00", "GMT-02:00", "GMT-01:00", "GMT-01:00", "GMT", "GMT", "GMT+01:00", "GMT+01:00", "GMT+01:00"}

   // TAB SETTINGS
   ::Edit_DefaultPath:Caption := oIni:ReadString("FILES", "LOCATION")
   IF oIni:ReadLogical("FILES", "SAVEDEFAULT") == .T.
      ::Check_SaveDefault:Check()
   END

   IF oIni:ReadLogical("DATABASE", "USECONNSTRING") == .T.
      ::Check_ConnectionString:Check()
      ::Edit_DBServer:Enabled := .F.
      ::Edit_DBName:Enabled := .F.
      ::Edit_DBUsername:Enabled := .F.
      ::Edit_DBPassword:Enabled := .F.
   ELSE
      ::CHeck_ConnectionString:UnCheck()
   END
   ::Edit_ConnectionString:Caption := oIni:ReadString("DATABASE", "CONNSTRING")

   IF oIni:ReadLogical("DATABASE", "USEDATABASECRED") == .T.
      ::Check_Credentials:Check()
      ::Edit_ConnectionString:Enabled := .F.
   ELSE
      ::CHeck_Credentials:UnCheck()
   END
   ::Edit_DBServer:Caption := oIni:ReadString("DATABASE", "SERVER")
   ::Edit_DBName:Caption := oIni:ReadString("DATABASE", "DATABASE")
   ::Edit_DBUsername:Caption := oIni:ReadString("DATABASE", "USER")
   ::Edit_DBPassword:Caption := oIni:ReadString("DATABASE", "PASSWORD")

   // TAB PREFERENCES
   IF oIni:ReadLogical("PREFERENCES", "OPENRECURSIVE") == .T.
      ::Check_OpenRecursive:Check()
   END
   IF oIni:ReadLogical("PREFERENCES", "FILTERFILES") == .T.
      ::Check_FilterFiles:Check()
   END
   ::Check_PersonalizedStamp:Check()
   IF oIni:ReadLogical("PREFERENCES", "EXPANDTREE") == .T.
      ::Check_ExpandTree:Check()
   END
   IF oIni:ReadLogical("PREFERENCES", "CONFIRMEXIT") == .T.
      ::Check_ConfirmExit:Check()
   END
   IF oIni:ReadLogical("PREFERENCES", "SAVEPOSITIONS")
      ::Check_SavePositions:Check()
   END
   ::Edit_Name:Caption := oIni:ReadString("GENERAL", "USERNAME")
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Combo_Timezone_OnCreate( Sender ) CLASS Form_Settings
   Sender:AddString("GMT -12:00 International Date Line West")
   Sender:AddString("GMT -11:00 Midway Island, Samoa")
   Sender:AddString("GMT -10:00 Hawaii")
   Sender:AddString("GMT -09:00 Alaska")
   Sender:AddString("GMT -08:00 Pacific Time (US & Canada); Tijuana")
   Sender:AddString("GMT -07:00 Arizona")
   Sender:AddString("GMT -07:00 Chihuahua, La Paz, Mazatlan")
   Sender:AddString("GMT -07:00 Mountain Time (US & Canada)")
   Sender:AddString("GMT -06:00 Central America")
   Sender:AddString("GMT -06:00 Central Time (US & Canada)")
   Sender:AddString("GMT -06:00 Guadalajara, Mexico City, Monterrey")
   Sender:AddString("GMT -06:00 Saskatchewan")
   Sender:AddString("GMT -05:00 Bogota, Lima, Quito")
   Sender:AddString("GMT -05:00 Eastern Time (US & Canada)")
   Sender:AddString("GMT -05:00 Indiana (East)")
   Sender:AddString("GMT -04:00 Atlantic Time (Canada)")
   Sender:AddString("GMT -04:00 Cacacas, La Paz")
   Sender:AddString("GMT -04:00 Santiago")
   Sender:AddString("GMT -03:30 Newfoundland")
   Sender:AddString("GMT -03:00 Brasilia")
   Sender:AddString("GMT -03:00 Buenos Aires, Georgetown")
   Sender:AddString("GMT -03:00 Greenland")
   Sender:AddString("GMT -02:00 Mid-Atlantic")
   Sender:AddString("GMT -01:00 Azores")
   Sender:AddString("GMT -01:00 Cape Verde Is.")
   Sender:AddString("GMT Casablanca, Monrovia")
   Sender:AddString("GMT Greenwich Mean Time: Dublin, Edinburgh, Lisbon, London")
   Sender:AddString("GMT +01:00 Amsterdam, Berlin, Bern, Rome, Stockholm, Vienna")
   Sender:AddString("GMT +01:00 Belgrade, Bratislava, Budapest, Ljubljana, Prague")
   Sender:AddString("GMT +01:00 Brussels, Copenhagen, Madrid, Paris")

   Sender:SetCurSel(oIni:ReadNumber("GENERAL", "TIMEZONE"))
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Button_Browse_OnClick( Sender ) CLASS Form_Settings
   WITH OBJECT ::MyFolderBrowserDialog
      :Show()

      IF LEN(:SelectedPath) > 0
         ::Edit_DefaultPath:Caption := :SelectedPath
      END
   END
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Check_ConnectionString_OnClick( Sender ) CLASS Form_Settings
   IF Sender:Checked()
      ::Check_Credentials:UnCheck()

      ::Edit_ConnectionString:Enabled := .T.
      ::Edit_DBServer:Enabled := .F.
      ::Edit_DBName:Enabled := .F.
      ::Edit_DBUsername:Enabled := .F.
      ::Edit_DBPassword:Enabled := .F.
   ELSE
      ::Check_Credentials:Check()

      ::Edit_ConnectionString:Enabled := .F.
      ::Edit_DBServer:Enabled := .T.
      ::Edit_DBName:Enabled := .T.
      ::Edit_DBUsername:Enabled := .T.
      ::Edit_DBPassword:Enabled := .T.
   END
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Check_Credentials_OnClick( Sender ) CLASS Form_Settings
   IF Sender:Checked()
      ::Check_ConnectionString:UnCheck()

      ::Edit_ConnectionString:Enabled := .F.
      ::Edit_DBServer:Enabled := .T.
      ::Edit_DBName:Enabled := .T.
      ::Edit_DBUsername:Enabled := .T.
      ::Edit_DBPassword:Enabled := .T.
   ELSE
      ::Check_ConnectionString:Check()

      ::Edit_ConnectionString:Enabled := .T.
      ::Edit_DBServer:Enabled := .F.
      ::Edit_DBName:Enabled := .F.
      ::Edit_DBUsername:Enabled := .F.
      ::Edit_DBPassword:Enabled := .F.
   END
RETURN Self

//----------------------------------------------------------------------------------------------------//
METHOD Button_OK_OnClick( Sender ) CLASS Form_Settings

   // TAB SETTINGS
   IF ::Edit_DefaultPath:Caption == ""
      MessageBox(Self:Handle, "Please specify the folder where the source files are located.", AppCaption, MB_OK + MB_ICONASTERISK)
      ::Edit_DefaultPath:SetFocus()
      RETURN NIL
   END

   IF ::Check_ConnectionString:Checked()
      IF ::Edit_ConnectionString:Caption == ""
         MessageBox(Self:Handle, "Please provide the connectionstring for the online xHDN database." + xCRLF + xCRLF + "More information on connectionstrings: www.connectionstring.com", AppCaption, MB_OK + MB_ICONASTERISK)
         ::Edit_ConnectionString:SetFocus()
         RETURN NIL
      END
   ELSE
      IF ::Edit_DBServer:Caption == ""
         MessageBox(Self:Handle, "Please specify the name or IP address of the database server.", AppCaption, MB_OK + MB_ICONASTERISK)
         ::Edit_DBServer:SetFocus()
         RETURN NIL
      END
      IF ::Edit_DBName:Caption == ""
         MessageBox(Self:Handle, "Please specify the name of the database to connect to.", AppCaption, MB_OK + MB_ICONASTERISK)
         ::Edit_DBName:SetFocus()
         RETURN NIL
      END
      IF ::Edit_DBName:Caption == ""
         MessageBox(Self:Handle, "Please specify the login name for the database.", AppCaption, MB_OK + MB_ICONASTERISK)
         ::Edit_DBName:SetFocus()
         RETURN NIL
      END
      IF ::Edit_DBPassword:Caption == ""
         MessageBox(Self:Handle, "Please specify the password for the database.", AppCaption, MB_OK + MB_ICONASTERISK)
         ::Edit_DBPassword:SetFocus()
         RETURN NIL
      END
   END

   // TAB PREFERENCES
   

   // TAB OTHER
   IF ::Edit_Name:Caption == ""
      MessageBox(Self:Handle, "Please provide your name for future reference.", AppCaption, MB_OK + MB_ICONASTERISK)
      ::Edit_Name:SetFocus()
      RETURN NIL
   END

   IF ::Combo_Timezone:GetCurSel() <= 0
      MessageBox(Self:Handle, "Please select a timezone from the listbox.", AppCaption, MB_OK + MB_ICONASTERISK)
      ::Combo_Timezone:SetFocus()
      RETURN NIL
   END

   oIni:WriteString("FILES", "LOCATION", ::Edit_DefaultPath:Caption)
   oIni:WriteLogical("FILES", "SAVEDEFAULT", ::Check_SaveDefault:Checked())

   oIni:WriteLogical("DATABASE", "USECONNSTRING", ::Check_ConnectionString:Checked())
   oIni:WriteString("DATABASE", "CONNSTRING", ::Edit_ConnectionString:Caption)
   oIni:WriteLogical("DATABASE", "USEDATABASECRED", ::Check_Credentials:Checked())
   oIni:WriteString("DATABASE", "SERVER", ::Edit_DBServer:Caption)
   oIni:WriteString("DATABASE", "DATABASE", ::Edit_DBName:Caption)
   oIni:WriteString("DATABASE", "USER", ::Edit_DBUsername:Caption)
   oIni:WriteString("DATABASE", "PASSWORD", ::Edit_DBPassword:Caption)

   oIni:WriteLogical("PREFERENCES", "OPENRECURSIVE", ::Check_OpenRecursive:Checked())
   oIni:WriteLogical("PREFERENCES", "FILTERFILES", ::Check_FilterFiles:Checked())
   oIni:WriteLogical("PREFERENCES", "EXPANDTREE", ::Check_ExpandTree:Checked())
   oIni:WriteLogical("PREFERENCES", "CONFIRMEXIT", ::Check_ConfirmExit:Checked())
   oIni:WriteLogical("PREFERENCES", "SAVEPOSITIONS", ::Check_SavePositions:Checked())

   oIni:WriteString("GENERAL", "USERNAME", ::Edit_Name:Caption)
   oIni:WriteNumber("GENERAL", "TIMEZONE", ::Combo_Timezone:GetCurSel())

   ::Close()
RETURN Self

METHOD Button_Cancel_OnClick( Sender ) CLASS Form_Settings
   ::Close()
RETURN Self

METHOD Edit_Name_OnEn_Update( Sender ) CLASS Form_Settings
   ::Edit_PersonalizedStamp:Caption := "!CHANGED BY: " + Sender:Caption + " on " + CreateTimestamp() + " " + aGMT[::Combo_Timezone:GetCurSel()] + " (<FILE CRC CHECKSUM>)"
RETURN Self

METHOD Combo_Timezone_OnCBNSelEndOk( Sender ) CLASS Form_Settings
   ::Edit_PersonalizedStamp:Caption := "!CHANGED BY: " + ::Edit_Name:Caption + " on " + CreateTimestamp() + " " + aGMT[::Combo_Timezone:GetCurSel()] + " (<FILE CRC CHECKSUM>)"
RETURN Self

METHOD Form_Settings_OnShowWindow( Sender ) CLASS Form_Settings
   ::Edit_PersonalizedStamp:Caption := "!CHANGED BY: " + oIni:ReadString("GENERAL", "USERNAME") + " on " + CreateTimestamp() + " " + aGMT[oIni:ReadNumber("GENERAL", "TIMEZONE")] + " (<FILE CRC CHECKSUM>)"
RETURN Self