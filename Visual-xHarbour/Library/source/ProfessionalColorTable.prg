#include "vxh.ch"
#include "debug.ch"

CLASS ProfessionalColorTable
   DATA ButtonCheckedGradientBegin           EXPORTED
   DATA ButtonCheckedGradientEnd             EXPORTED
   DATA ButtonPressedGradientBegin           EXPORTED
   DATA ButtonPressedGradientEnd             EXPORTED
   DATA ButtonSelectedBorder                 EXPORTED
   DATA ButtonPressedBorder                  EXPORTED
   DATA ButtonSelectedGradientBegin          EXPORTED
   DATA ButtonSelectedGradientEnd            EXPORTED
   DATA GripDark                             EXPORTED
   DATA GripLight                            EXPORTED
   DATA MenuBorder                           EXPORTED
   DATA MenuItemDisabled                     EXPORTED INIT RGB(240, 240, 240)
   DATA MenuItemDisabledBorder               EXPORTED INIT RGB(174, 174, 174)
   DATA MenuBackground                       EXPORTED
   DATA MenuItemBorder                       EXPORTED
   DATA MenuItemPressedGradientBegin         EXPORTED
   DATA MenuItemPressedGradientEnd           EXPORTED
   DATA MenuItemSelected                     EXPORTED
   DATA MenuItemSelectedGradientBegin        EXPORTED
   DATA MenuItemSelectedGradientEnd          EXPORTED
   DATA OverflowButtonGradientBegin          EXPORTED
   DATA OverflowButtonGradientEnd            EXPORTED
   DATA OverflowButtonGradientMiddle         EXPORTED
   DATA SeparatorDark                        EXPORTED
   DATA SeparatorLight                       EXPORTED
   DATA ToolStripBorder                      EXPORTED
   DATA ToolStripGradientBegin               EXPORTED
   DATA ToolStripGradientEnd                 EXPORTED
   DATA ToolStripGradientMiddle              EXPORTED
   DATA ToolStripPanelGradientBegin          EXPORTED
   DATA ToolStripPanelGradientEnd            EXPORTED
   DATA MenuItemShadow                       EXPORTED INIT .F.

   DATA ColorScheme
   DATA UseSystemColors                      EXPORTED INIT .F.
   DATA MenuStripeVisible                    EXPORTED INIT .T.
   DATA MenuCheckRectangle                   EXPORTED INIT .T.
   DATA MenuBarSeparated                     EXPORTED INIT .F.
   DATA AutoScheme                           EXPORTED INIT .T.
   DATA Theme                                EXPORTED INIT ""
   DATA cScheme                              EXPORTED

   DATA TitleBackColorActive                 EXPORTED INIT RGB(70,130,180)
   DATA TitleBackColorInactive               EXPORTED INIT RGB(70,130,180)

   DATA Pen                                  EXPORTED INIT {=>}
   DATA Brush                                EXPORTED INIT {=>}

   METHOD GetCurrentStyle()
   METHOD Load()
   METHOD Unload()
   METHOD Clean()
ENDCLASS

//-----------------------------------------------------------------------------------------------------------------------------
METHOD Unload() CLASS ProfessionalColorTable
   IF ::Pen != NIL
      HEVAL( ::Pen,   {|a,hPen| (a), DeleteObject( hPen ) } )
      HEVAL( ::Brush, {|a,hBrush| (a), DeleteObject( hBrush ) } )
   ENDIF
   ::Pen   := NIL
   ::Brush := NIL
RETURN Self

//-----------------------------------------------------------------------------------------------------------------------------
METHOD Load( cScheme ) CLASS ProfessionalColorTable

   IF ::AutoScheme .AND. cScheme == NIL
      ::GetCurrentStyle()
   ENDIF
   DEFAULT cScheme TO ::ColorScheme

   DO CASE

      CASE cScheme == "Classic"
           ::ButtonCheckedGradientBegin             := GetSysColor( COLOR_INACTIVECAPTION )
           ::ButtonCheckedGradientEnd               := GetSysColor( COLOR_INACTIVECAPTION )
           ::ButtonPressedGradientBegin             := RGB(133, 146, 181)
           ::ButtonPressedGradientEnd               := RGB(133, 146, 181)
           ::ButtonPressedBorder                    := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonSelectedBorder                   := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonSelectedGradientBegin            := RGB(182, 189, 210)
           ::ButtonSelectedGradientEnd              := RGB(182, 189, 210)
           ::GripDark                               := RGB(160, 160, 160)
           ::GripLight                              := GetSysColor( COLOR_WINDOW )
           ::MenuBorder                             := RGB(102, 102, 102)
           ::MenuBackground                         := GetSysColor( COLOR_WINDOW )
           ::MenuItemBorder                         := GetSysColor( COLOR_HIGHLIGHT )
           ::MenuItemPressedGradientBegin           := RGB(245, 244, 242)
           ::MenuItemPressedGradientEnd             := RGB(234, 232, 228)
           ::MenuItemSelected                       := GetSysColor( COLOR_WINDOW )
           ::MenuItemSelectedGradientBegin          := RGB(182, 189, 210)
           ::MenuItemSelectedGradientEnd            := RGB(182, 189, 210)

           ::OverflowButtonGradientBegin            := RGB(225, 222, 217)
           ::OverflowButtonGradientEnd              := GetSysColor( COLOR_BTNSHADOW )
           ::OverflowButtonGradientMiddle           := RGB(216, 213, 206)
           ::SeparatorDark                          := RGB(166, 166, 166)
           ::SeparatorLight                         := GetSysColor( COLOR_BTNHIGHLIGHT )
           ::ToolStripBorder                        := RGB(219, 216, 209)
           ::ToolStripGradientBegin                 := RGB(245, 244, 242)
           ::ToolStripGradientEnd                   := GetSysColor( COLOR_BTNFACE )
           ::ToolStripGradientMiddle                := LightenColor( GetSysColor( COLOR_BTNFACE ), 100 )//RGB(234, 232, 228)
           ::ToolStripPanelGradientBegin            := GetSysColor( COLOR_BTNFACE )
           ::ToolStripPanelGradientEnd              := RGB(246, 245, 244)

      CASE cScheme == "NormalColor"
           ::ButtonCheckedGradientBegin             := IIF( ::UseSystemColors, NIL, RGB(255, 223, 154) )
           ::ButtonCheckedGradientEnd               := IIF( ::UseSystemColors, NIL, RGB(255, 166, 76) )
           ::ButtonPressedGradientBegin             := IIF( ::UseSystemColors, RGB(152, 181, 226), RGB(254, 128, 62) )
           ::ButtonPressedGradientEnd               := IIF( ::UseSystemColors, RGB(152, 181, 226), RGB(255, 223, 154) )
           ::ButtonPressedBorder                    := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(0, 0, 128) )
           ::ButtonSelectedBorder                   := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(0, 0, 128) )
           ::ButtonSelectedGradientBegin            := IIF( ::UseSystemColors, RGB(193, 210, 238), RGB(255, 255, 222) )
           ::ButtonSelectedGradientEnd              := IIF( ::UseSystemColors, RGB(193, 210, 238), RGB(255, 203, 136) )
           ::GripDark                               := IIF( ::UseSystemColors, RGB(193, 190, 179), RGB(39, 65, 118) )
           ::GripLight                              := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 255, 255) )
           ::MenuBorder                             := IIF( ::UseSystemColors, RGB(138, 134, 122), RGB(0, 45, 150) )
           ::MenuBackground                         := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 255, 255) )
           ::MenuItemBorder                         := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(0, 0, 128) )
           ::MenuItemPressedGradientBegin           := IIF( ::UseSystemColors, RGB(251, 250, 246), RGB(227, 239, 255) )
           ::MenuItemPressedGradientEnd             := IIF( ::UseSystemColors, RGB(246, 244, 236), RGB(123, 164, 224) )
           ::MenuItemSelected                       := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 238, 194) )
           ::MenuItemSelectedGradientBegin          := IIF( ::UseSystemColors, RGB(193, 210, 238), RGB(255, 255, 222) )
           ::MenuItemSelectedGradientEnd            := IIF( ::UseSystemColors, RGB(193, 210, 238), RGB(255, 203, 136) )

           ::OverflowButtonGradientBegin            := IIF( ::UseSystemColors, RGB(242, 240, 228), RGB(127, 177, 250) )
           ::OverflowButtonGradientEnd              := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNSHADOW ), RGB(0, 53, 145) )
           ::OverflowButtonGradientMiddle           := IIF( ::UseSystemColors, RGB(238, 235, 220), RGB(82, 127, 208) )
           ::SeparatorDark                          := IIF( ::UseSystemColors, RGB(197, 194, 184), RGB(106, 140, 203) )
           ::SeparatorLight                         := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNHIGHLIGHT ), RGB(241, 249, 255) )
           ::ToolStripBorder                        := IIF( ::UseSystemColors, RGB(239, 237, 222), RGB(59, 97, 156) )
           ::ToolStripGradientBegin                 := IIF( ::UseSystemColors, RGB(251, 250, 246), RGB(227, 239, 255) )
           ::ToolStripGradientEnd                   := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(123, 164, 224) )
           ::ToolStripGradientMiddle                := IIF( ::UseSystemColors, RGB(246, 244, 236), RGB(203, 225, 252) )
           ::ToolStripPanelGradientBegin            := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(158, 190, 245) )
           ::ToolStripPanelGradientEnd              := IIF( ::UseSystemColors, RGB(251, 250, 247), RGB(196, 218, 250) )

      CASE cScheme == "HomeStead"
           ::ButtonCheckedGradientBegin             := IIF( ::UseSystemColors, NIL, RGB(255, 223, 154) )
           ::ButtonCheckedGradientEnd               := IIF( ::UseSystemColors, NIL, RGB(255, 166, 76) )
           ::ButtonPressedGradientBegin             := IIF( ::UseSystemColors, RGB(201, 208, 184), RGB(254, 128, 62) )
           ::ButtonPressedGradientEnd               := IIF( ::UseSystemColors, RGB(201, 208, 184), RGB(255, 223, 154) )
           ::ButtonPressedBorder                    := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(63, 93, 56) )
           ::ButtonSelectedBorder                   := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(63, 93, 56) )
           ::ButtonSelectedGradientBegin            := IIF( ::UseSystemColors, RGB(223, 227, 212), RGB(255, 255, 222) )
           ::ButtonSelectedGradientEnd              := IIF( ::UseSystemColors, RGB(223, 227, 212), RGB(255, 203, 136) )
           ::GripDark                               := IIF( ::UseSystemColors, RGB(193, 190, 179), RGB(81, 94, 51) )
           ::GripLight                              := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 255, 255) )
           ::MenuBorder                             := IIF( ::UseSystemColors, RGB(138, 134, 122), RGB(117, 141, 94) )
           ::MenuBackground                         := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 255, 255) )
           ::MenuItemBorder                         := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(63, 93, 56) )
           ::MenuItemPressedGradientBegin           := IIF( ::UseSystemColors, RGB(251, 250, 246), RGB(237, 240, 214) )
           ::MenuItemPressedGradientEnd             := IIF( ::UseSystemColors, RGB(246, 244, 236), RGB(181, 196, 143) )
           ::MenuItemSelected                       := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 238, 194) )
           ::MenuItemSelectedGradientBegin          := IIF( ::UseSystemColors, RGB(223, 227, 212), RGB(255, 255, 222) )
           ::MenuItemSelectedGradientEnd            := IIF( ::UseSystemColors, RGB(223, 227, 212), RGB(255, 203, 136) )

           ::OverflowButtonGradientBegin            := IIF( ::UseSystemColors, RGB(242, 240, 228), RGB(186, 204, 150) )
           ::OverflowButtonGradientEnd              := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNSHADOW ), RGB(96, 119, 107) )
           ::OverflowButtonGradientMiddle           := IIF( ::UseSystemColors, RGB(238, 235, 220), RGB(141, 160, 107) )
           ::SeparatorDark                          := IIF( ::UseSystemColors, RGB(197, 194, 184), RGB(96, 128, 88) )
           ::SeparatorLight                         := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNHIGHLIGHT ), RGB(244, 247, 222) )
           ::ToolStripBorder                        := IIF( ::UseSystemColors, RGB(239, 237, 222), RGB(96, 128, 88) )
           ::ToolStripGradientBegin                 := IIF( ::UseSystemColors, RGB(251, 250, 246), RGB(255, 255, 237) )
           ::ToolStripGradientEnd                   := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(181, 196, 143) )
           ::ToolStripGradientMiddle                := IIF( ::UseSystemColors, RGB(246, 244, 236), RGB(206, 220, 167) )
           ::ToolStripPanelGradientBegin            := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(217, 217, 167) )
           ::ToolStripPanelGradientEnd              := IIF( ::UseSystemColors, RGB(251, 250, 247), RGB(242, 241, 228) )

      CASE cScheme == "Metallic"
           ::ButtonCheckedGradientBegin             := IIF( ::UseSystemColors, NIL, RGB(255, 223, 154) )
           ::ButtonCheckedGradientEnd               := IIF( ::UseSystemColors, NIL, RGB(255, 166, 76) )
           ::ButtonPressedGradientBegin             := IIF( ::UseSystemColors, RGB(217, 218, 223), RGB(254, 128, 62) )
           ::ButtonPressedGradientEnd               := IIF( ::UseSystemColors, RGB(217, 218, 223), RGB(255, 223, 154) )
           ::ButtonPressedBorder                    := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(75, 75, 111) )
           ::ButtonSelectedBorder                   := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(75, 75, 111) )
           ::ButtonSelectedGradientBegin            := IIF( ::UseSystemColors, RGB(232, 233, 236), RGB(255, 255, 222) )
           ::ButtonSelectedGradientEnd              := IIF( ::UseSystemColors, RGB(232, 233, 236), RGB(255, 203, 136) )
           ::GripDark                               := IIF( ::UseSystemColors, RGB(182, 182, 185), RGB(84, 84, 117) )
           ::GripLight                              := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 255, 255) )
           ::MenuBorder                             := IIF( ::UseSystemColors, RGB(126, 126, 129), RGB(124, 124, 148) )
           ::MenuBackground                         := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 255, 255) )
           ::MenuItemBorder                         := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(75, 75, 111) )
           ::MenuItemPressedGradientBegin           := IIF( ::UseSystemColors, RGB(248, 248, 249), RGB(232, 233, 242) )
           ::MenuItemPressedGradientEnd             := IIF( ::UseSystemColors, RGB(240, 239, 241), RGB(172, 170, 194) )
           ::MenuItemSelected                       := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 238, 194) )
           ::MenuItemSelectedGradientBegin          := IIF( ::UseSystemColors, RGB(232, 233, 236), RGB(255, 255, 222) )
           ::MenuItemSelectedGradientEnd            := IIF( ::UseSystemColors, RGB(232, 233, 236), RGB(255, 203, 136) )

           ::OverflowButtonGradientBegin            := IIF( ::UseSystemColors, RGB(233, 233, 235), RGB(186, 185, 206) )
           ::OverflowButtonGradientEnd              := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNSHADOW ), RGB(118, 116, 146) )
           ::OverflowButtonGradientMiddle           := IIF( ::UseSystemColors, RGB(227, 226, 230), RGB(156, 155, 180) )
           ::SeparatorDark                          := IIF( ::UseSystemColors, RGB(186, 186, 189), RGB(110, 109, 143) )
           ::SeparatorLight                         := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNHIGHLIGHT ), RGB(255, 255, 255) )
           ::ToolStripBorder                        := IIF( ::UseSystemColors, RGB(229, 228, 232), RGB(124, 124, 148) )
           ::ToolStripGradientBegin                 := IIF( ::UseSystemColors, RGB(248, 248, 249), RGB(249, 249, 255) )
           ::ToolStripGradientEnd                   := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(147, 145, 176) )
           ::ToolStripGradientMiddle                := IIF( ::UseSystemColors, RGB(240, 239, 241), RGB(225, 226, 236) )
           ::ToolStripPanelGradientBegin            := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(215, 215, 229) )
           ::ToolStripPanelGradientEnd              := IIF( ::UseSystemColors, RGB(249, 248, 249), RGB(243, 243, 247) )

           ::TitleBackColorActive                   := RGB(70,130,180)
           ::TitleBackColorInactive                 := RGB(70,130,180)

      CASE cScheme == "MediaCenter"
           ::ButtonCheckedGradientBegin             := IIF( ::UseSystemColors, NIL, RGB(226, 229, 238) )
           ::ButtonCheckedGradientEnd               := IIF( ::UseSystemColors, NIL, RGB(226, 229, 238) )
           ::ButtonPressedGradientBegin             := IIF( ::UseSystemColors, RGB(153, 175, 212), RGB(153, 175, 212) )
           ::ButtonPressedGradientEnd               := IIF( ::UseSystemColors, RGB(153, 175, 212), RGB(153, 175, 212) )
           ::ButtonPressedBorder                    := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(51, 94, 168) )
           ::ButtonSelectedBorder                   := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(51, 94, 168) )
           ::ButtonSelectedGradientBegin            := IIF( ::UseSystemColors, RGB(194, 207, 229), RGB(194, 207, 229) )
           ::ButtonSelectedGradientEnd              := IIF( ::UseSystemColors, RGB(194, 207, 229), RGB(194, 207, 229) )
           ::GripDark                               := IIF( ::UseSystemColors, RGB(189, 188, 191), RGB(189, 188, 191) )
           ::GripLight                              := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 255, 255) )
           ::MenuBorder                             := IIF( ::UseSystemColors, RGB(134, 133, 136), RGB(134, 133, 136) )
           ::MenuBackground                         := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 255, 255) )
           ::MenuItemBorder                         := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(51, 94, 168) )
           ::MenuItemPressedGradientBegin           := IIF( ::UseSystemColors, RGB(250, 250, 251), RGB(252, 252, 252) )
           ::MenuItemPressedGradientEnd             := IIF( ::UseSystemColors, RGB(245, 244, 246), RGB(245, 244, 246) )
           ::MenuItemSelected                       := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(194, 207, 229) )
           ::MenuItemSelectedGradientBegin          := IIF( ::UseSystemColors, RGB(194, 207, 229), RGB(194, 207, 229) )
           ::MenuItemSelectedGradientEnd            := IIF( ::UseSystemColors, RGB(194, 207, 229), RGB(194, 207, 229) )

           ::OverflowButtonGradientBegin            := IIF( ::UseSystemColors, RGB(241, 240, 242), RGB(242, 242, 242) )
           ::OverflowButtonGradientEnd              := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNSHADOW ), RGB(167, 166, 170) )
           ::OverflowButtonGradientMiddle           := IIF( ::UseSystemColors, RGB(237, 235, 239), RGB(224, 224, 225) )
           ::SeparatorDark                          := IIF( ::UseSystemColors, RGB(193, 193, 196), RGB(193, 193, 196) )
           ::SeparatorLight                         := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNHIGHLIGHT ), RGB(255, 255, 255) )
           ::ToolStripBorder                        := IIF( ::UseSystemColors, RGB(238, 237, 240), RGB(238, 237, 240) )
           ::ToolStripGradientBegin                 := IIF( ::UseSystemColors, RGB(250, 250, 251), RGB(252, 252, 252) )
           ::ToolStripGradientEnd                   := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(235, 233, 237) )
           ::ToolStripGradientMiddle                := IIF( ::UseSystemColors, RGB(245, 244, 246), RGB(245, 244, 246) )
           ::ToolStripPanelGradientBegin            := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(235, 233, 237) )
           ::ToolStripPanelGradientEnd              := IIF( ::UseSystemColors, RGB(251, 250, 251), RGB(251, 250, 251) )

      CASE cScheme == "Aero"
           ::ButtonCheckedGradientBegin             := RGB(194, 224, 255)//GetSysColor( COLOR_INACTIVECAPTION )
           ::ButtonCheckedGradientEnd               := RGB(194, 224, 255)//GetSysColor( COLOR_INACTIVECAPTION )
           ::ButtonPressedGradientBegin             := RGB(153, 204, 255)
           ::ButtonPressedGradientEnd               := RGB(153, 204, 255)
           ::ButtonPressedBorder                    := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonSelectedBorder                   := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(51, 94, 168) )
           ::ButtonSelectedGradientBegin            := RGB(194, 224, 255)
           ::ButtonSelectedGradientEnd              := RGB(194, 224, 255)
           ::GripDark                               := RGB(184, 184, 184)
           ::GripLight                              := GetSysColor( COLOR_WINDOW )
           ::MenuBorder                             := RGB(128, 128, 128)
           ::MenuBackground                         := GetSysColor( COLOR_WINDOW )
           ::MenuItemBorder                         := GetSysColor( COLOR_HIGHLIGHT )
           ::MenuItemPressedGradientBegin           := RGB(252, 252, 252)
           ::MenuItemPressedGradientEnd             := RGB(250, 250, 250)
           ::MenuItemSelected                       := RGB(194, 224, 255)
           ::MenuItemSelectedGradientBegin          := RGB(194, 224, 255)
           ::MenuItemSelectedGradientEnd            := RGB(194, 224, 255)

           ::OverflowButtonGradientBegin            := RGB(247, 247, 247)
           ::OverflowButtonGradientEnd              := GetSysColor( COLOR_BTNSHADOW )
           ::OverflowButtonGradientMiddle           := GetSysColor( COLOR_BTNSHADOW ) //RGB(245, 245, 245)
           ::SeparatorDark                          := RGB(189, 189, 189)
           ::SeparatorLight                         := GetSysColor( COLOR_BTNHIGHLIGHT )
           ::ToolStripBorder                        := RGB(246, 246, 246)
           ::ToolStripGradientBegin                 := RGB(252, 252, 252)
           ::ToolStripGradientEnd                   := GetSysColor( COLOR_BTNFACE )
           ::ToolStripGradientMiddle                := RGB(250, 250, 250)
           ::ToolStripPanelGradientBegin            := GetSysColor( COLOR_BTNFACE )
           ::ToolStripPanelGradientEnd              := RGB(253, 253, 253)
   ENDCASE

   ::Clean()
RETURN Self

METHOD Clean() CLASS ProfessionalColorTable
   LOCAL nColor, cProp,  aKeys := { "ToolStripBorder",;
                                    "ToolStripGradientBegin",;
                                    "SeparatorDark",;
                                    "SeparatorLight",;
                                    "ButtonSelectedBorder",;
                                    "ButtonPressedBorder",;
                                    "ButtonPressedGradientBegin",;
                                    "ButtonCheckedGradientBegin",;
                                    "ButtonCheckedGradientEnd",;
                                    "ToolStripPanelGradientEnd",;
                                    "MenuItemSelected",;
                                    "MenuItemDisabledBorder",;
                                    "MenuItemDisabled",;
                                    "MenuItemSelectedGradientEnd",;
                                    "MenuItemPressedGradientEnd",;
                                    "MenuItemBorder",;
                                    "MenuBorder",;
                                    "MenuBackground",;
                                    "TitleBackColorInactive",;
                                    "TitleBackColorActive" }
   HSetCaseMatch( ::Pen, .F. )
   HSetCaseMatch( ::Brush, .F. )

   FOR EACH cProp IN aKeys
       IF HHasKey( ::Pen, cProp )
          DeleteObject( ::Pen[ cProp ] )
          DeleteObject( ::Brush[ cProp ] )
       ENDIF
       nColor := __objSendMsg( Self, UPPER( cProp ) )
       ::Pen[ cProp ]   := CreatePen( PS_SOLID, 1, nColor )
       ::Brush[ cProp ] := CreateSolidBrush( nColor )
   NEXT
RETURN Self

METHOD GetCurrentStyle() CLASS ProfessionalColorTable
   LOCAL cTheme, cColor, cSize
   GetCurrentThemeName( @cTheme, @cColor, @cSize )
   DEFAULT cTheme TO ""
   ::Theme := cTheme
   ::ColorScheme := cColor
   IF cColor == NIL
      ::ColorScheme := "Classic"
    ELSEIF "Aero" $ cTheme
      ::ColorScheme := "Metallic"
    ELSEIF cColor == "Media Center Style"
      ::ColorScheme := "MediaCenter"
   ENDIF
   DEFAULT ::ColorScheme TO "NormalColor"
RETURN NIL



CLASS FlatGrayColorTable INHERIT ProfessionalColorTable
   METHOD Load()
ENDCLASS

//-----------------------------------------------------------------------------------------------------------------------------
METHOD Load() CLASS FlatGrayColorTable
   ::UseSystemColors := .T.

   ::ButtonCheckedGradientBegin             := RGB( 197, 222, 245 )
   ::ButtonCheckedGradientEnd               := RGB( 197, 222, 245 )
   ::ButtonPressedGradientBegin             := RGB( 201, 224, 247 )
   ::ButtonPressedGradientEnd               := RGB( 201, 224, 247 )
   ::ButtonPressedBorder                    := RGB(  86, 157, 229 )
   ::ButtonSelectedBorder                   := RGB( 126, 180, 234 )
   ::ButtonSelectedGradientBegin            := RGB( 232, 242, 252 )
   ::ButtonSelectedGradientEnd              := RGB( 232, 242, 252 )
   ::GripDark                               := IIF( ::UseSystemColors, RGB(182, 182, 185), RGB(84, 84, 117) )
   ::GripLight                              := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 255, 255) )
   ::MenuBorder                             := IIF( ::UseSystemColors, RGB(126, 126, 129), RGB(124, 124, 148) )
   ::MenuItemBorder                         := RGB( 120, 174, 229 )
   ::MenuItemPressedGradientBegin           := RGB( 201, 224, 247 )
   ::MenuItemPressedGradientEnd             := RGB( 201, 224, 247 )
   ::MenuItemSelected                       := RGB( 209, 226, 242 )

   ::MenuItemSelectedGradientBegin          := IIF( ::UseSystemColors, RGB(232, 233, 236), RGB(255, 255, 222) )
   ::MenuItemSelectedGradientEnd            := IIF( ::UseSystemColors, RGB(232, 233, 236), RGB(255, 203, 136) )

   ::OverflowButtonGradientBegin            := IIF( ::UseSystemColors, RGB(233, 233, 235), RGB(186, 185, 206) )
   ::OverflowButtonGradientEnd              := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNSHADOW ), RGB(118, 116, 146) )
   ::OverflowButtonGradientMiddle           := IIF( ::UseSystemColors, RGB(227, 226, 230), RGB(156, 155, 180) )
   ::SeparatorDark                          := IIF( ::UseSystemColors, RGB(186, 186, 189), RGB(110, 109, 143) )
   ::SeparatorLight                         := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNHIGHLIGHT ), RGB(255, 255, 255) )
   ::MenuBackground                         := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB( 255, 255, 255 ) )
   ::ToolStripBorder                        := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB( 255, 255, 255 ) )
   ::ToolStripGradientBegin                 := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB( 255, 255, 255 ) )
   ::ToolStripGradientEnd                   := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB( 255, 255, 255 ) )
   ::ToolStripGradientMiddle                := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB( 255, 255, 255 ) )
   ::ToolStripPanelGradientBegin            := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB( 255, 255, 255 ) )
   ::ToolStripPanelGradientEnd              := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB( 255, 255, 255 ) )

   ::MenuItemShadow                         := .F.
   ::MenuBarSeparated                       := .T.
   ::Clean()
RETURN Self

CLASS VSColorTable INHERIT ProfessionalColorTable
   METHOD Load()
ENDCLASS

//-----------------------------------------------------------------------------------------------------------------------------
METHOD Load() CLASS VSColorTable
   ::UseSystemColors := .T.

   ::ButtonCheckedGradientBegin             := RGB( 197, 222, 245 )
   ::ButtonCheckedGradientEnd               := RGB( 197, 222, 245 )
   ::ButtonPressedGradientBegin             := RGB( 201, 224, 247 )
   ::ButtonPressedGradientEnd               := RGB( 201, 224, 247 )
   ::ButtonPressedBorder                    := RGB(  86, 157, 229 )
   ::ButtonSelectedBorder                   := RGB( 126, 180, 234 )
   ::ButtonSelectedGradientBegin            := RGB( 232, 242, 252 )
   ::ButtonSelectedGradientEnd              := RGB( 232, 242, 252 )
   ::GripDark                               := IIF( ::UseSystemColors, RGB(182, 182, 185), RGB(84, 84, 117) )
   ::GripLight                              := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 255, 255) )
   ::MenuBorder                             := IIF( ::UseSystemColors, RGB(126, 126, 129), RGB(124, 124, 148) )
   ::MenuItemBorder                         := RGB( 120, 174, 229 )
   ::MenuItemPressedGradientBegin           := RGB( 201, 224, 247 )
   ::MenuItemPressedGradientEnd             := RGB( 201, 224, 247 )
   ::MenuItemSelected                       := RGB( 209, 226, 242 )

   ::MenuItemSelectedGradientBegin          := IIF( ::UseSystemColors, RGB(232, 233, 236), RGB(255, 255, 222) )
   ::MenuItemSelectedGradientEnd            := IIF( ::UseSystemColors, RGB(232, 233, 236), RGB(255, 203, 136) )

   ::OverflowButtonGradientBegin            := IIF( ::UseSystemColors, RGB(233, 233, 235), RGB(186, 185, 206) )
   ::OverflowButtonGradientEnd              := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNSHADOW ), RGB(118, 116, 146) )
   ::OverflowButtonGradientMiddle           := IIF( ::UseSystemColors, RGB(227, 226, 230), RGB(156, 155, 180) )
   ::SeparatorDark                          := IIF( ::UseSystemColors, RGB(186, 186, 189), RGB(110, 109, 143) )
   ::SeparatorLight                         := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNHIGHLIGHT ), RGB(255, 255, 255) )
   ::MenuBackground                         := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB( 255, 255, 255 ) )
   ::ToolStripBorder                        := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB( 255, 255, 255 ) )
   ::ToolStripGradientBegin                 := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB( 255, 255, 255 ) )
   ::ToolStripGradientEnd                   := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB( 255, 255, 255 ) )
   ::ToolStripGradientMiddle                := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB( 255, 255, 255 ) )
   ::ToolStripPanelGradientBegin            := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB( 255, 255, 255 ) )
   ::ToolStripPanelGradientEnd              := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB( 255, 255, 255 ) )

   ::MenuItemShadow                         := .F.
   ::MenuBarSeparated                       := .T.
   ::Clean()
RETURN Self
