#include "vxh.ch"
#include "debug.ch"

static __aSystemPen := {}
static __aSystemBrush := {}

EXIT PROCEDURE __CleanProfessionalColorTableResources
   AEVAL( __aSystemPen, {|a| DeleteObject( a[2] ) } )
   AEVAL( __aSystemBrush, {|a| DeleteObject( a[2] ) } )
   __aSystemPen := NIL
   __aSystemBrush := NIL
RETURN

CLASS ProfessionalColorTable
   DATA ButtonCheckedGradientBegin           EXPORTED
   DATA ButtonCheckedGradientEnd             EXPORTED
   DATA ButtonCheckedGradientMiddle          EXPORTED
   DATA ButtonCheckedHighlight               EXPORTED
   DATA ButtonCheckedHighlightBorder         EXPORTED
   DATA ButtonPressedBorder                  EXPORTED
   DATA ButtonPressedGradientBegin           EXPORTED
   DATA ButtonPressedGradientEnd             EXPORTED
   DATA ButtonPressedGradientMiddle          EXPORTED
   DATA ButtonPressedHighlight               EXPORTED
   DATA ButtonPressedHighlightBorder         EXPORTED
   DATA ButtonSelectedBorder                 EXPORTED
   DATA ButtonSelectedGradientBegin          EXPORTED
   DATA ButtonSelectedGradientEnd            EXPORTED
   DATA ButtonSelectedGradientMiddle         EXPORTED
   DATA ButtonSelectedHighlight              EXPORTED
   DATA ButtonSelectedHighlightBorder        EXPORTED
   DATA CheckBackground                      EXPORTED
   DATA CheckPressedBackground               EXPORTED
   DATA CheckSelectedBackground              EXPORTED
   DATA GripDark                             EXPORTED
   DATA GripLight                            EXPORTED
   DATA ImageMarginGradientBegin             EXPORTED
   DATA ImageMarginGradientEnd               EXPORTED
   DATA ImageMarginGradientMiddle            EXPORTED
   DATA ImageMarginRevealedGradientBegin     EXPORTED
   DATA ImageMarginRevealedGradientEnd       EXPORTED
   DATA ImageMarginRevealedGradientMiddle    EXPORTED
   DATA MenuBorder                           EXPORTED
   DATA MenuItemBorder                       EXPORTED
   DATA MenuItemPressedGradientBegin         EXPORTED
   DATA MenuItemPressedGradientEnd           EXPORTED
   DATA MenuItemPressedGradientMiddle        EXPORTED
   DATA MenuItemSelected                     EXPORTED
   DATA MenuItemSelectedGradientBegin        EXPORTED
   DATA MenuItemSelectedGradientEnd          EXPORTED
   DATA MenuStripGradientBegin               EXPORTED
   DATA MenuStripGradientEnd                 EXPORTED
   DATA OverflowButtonGradientBegin          EXPORTED
   DATA OverflowButtonGradientEnd            EXPORTED
   DATA OverflowButtonGradientMiddle         EXPORTED
   DATA RaftingContainerGradientBegin        EXPORTED
   DATA RaftingContainerGradientEnd          EXPORTED
   DATA SeparatorDark                        EXPORTED
   DATA SeparatorLight                       EXPORTED
   DATA StatusStripGradientBegin             EXPORTED
   DATA StatusStripGradientEnd               EXPORTED
   DATA ToolStripBorder                      EXPORTED
   DATA ToolStripContentPanelGradientBegin   EXPORTED
   DATA ToolStripContentPanelGradientEnd     EXPORTED
   DATA ToolStripDropDownBackground          EXPORTED
   DATA ToolStripGradientBegin               EXPORTED
   DATA ToolStripGradientEnd                 EXPORTED
   DATA ToolStripGradientMiddle              EXPORTED
   DATA ToolStripPanelGradientBegin          EXPORTED
   DATA ToolStripPanelGradientEnd            EXPORTED

   DATA ColorScheme
   DATA UseSystemColors                      EXPORTED INIT .F.
   
   DATA Pen                                  EXPORTED
   DATA Brush                                EXPORTED

   DATA AutoScheme                           EXPORTED INIT .T.
   
   DATA Theme                                EXPORTED

   METHOD Init() CONSTRUCTOR
   METHOD GetCurrentStyle()
   METHOD Load()
ENDCLASS

//-----------------------------------------------------------------------------------------------------------------------------
METHOD Init() CLASS ProfessionalColorTable
   ::Pen   := ProfessionalColorTablePen( Self )
   ::Brush := ProfessionalColorTableBrush( Self )
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
           ::ButtonCheckedGradientMiddle            := NIL
           ::ButtonCheckedHighlight                 := RGB(184, 191, 211)
           ::ButtonCheckedHighlightBorder           := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonPressedBorder                    := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonPressedGradientBegin             := RGB(133, 146, 181)
           ::ButtonPressedGradientEnd               := RGB(133, 146, 181)
           ::ButtonPressedGradientMiddle            := RGB(133, 146, 181)
           ::ButtonPressedHighlight                 := RGB(131, 144, 179)
           ::ButtonPressedHighlightBorder           := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonSelectedBorder                   := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonSelectedGradientBegin            := RGB(182, 189, 210)
           ::ButtonSelectedGradientEnd              := RGB(182, 189, 210)
           ::ButtonSelectedGradientMiddle           := RGB(182, 189, 210)
           ::ButtonSelectedHighlight                := RGB(184, 191, 211)
           ::ButtonSelectedHighlightBorder          := GetSysColor( COLOR_HIGHLIGHT )
           ::CheckBackground                        := GetSysColor( COLOR_HIGHLIGHT )
           ::CheckPressedBackground                 := RGB(133, 146, 181)
           ::CheckSelectedBackground                := RGB(133, 146, 181)
           ::GripDark                               := RGB(160, 160, 160)
           ::GripLight                              := GetSysColor( COLOR_WINDOW )
           ::ImageMarginGradientBegin               := RGB(245, 244, 242)
           ::ImageMarginGradientEnd                 := GetSysColor( COLOR_3DFACE )
           ::ImageMarginGradientMiddle              := RGB(234, 232, 228)
           ::ImageMarginRevealedGradientBegin       := RGB(238, 236, 233)
           ::ImageMarginRevealedGradientEnd         := RGB(216, 213, 206)
           ::ImageMarginRevealedGradientMiddle      := RGB(225, 222, 217)
           ::MenuBorder                             := RGB(102, 102, 102)
           ::MenuItemBorder                         := GetSysColor( COLOR_HIGHLIGHT )
           ::MenuItemPressedGradientBegin           := RGB(245, 244, 242)
           ::MenuItemPressedGradientEnd             := RGB(234, 232, 228)
           ::MenuItemPressedGradientMiddle          := RGB(225, 222, 217)
           ::MenuItemSelected                       := GetSysColor( COLOR_WINDOW )
           ::MenuItemSelectedGradientBegin          := RGB(182, 189, 210)
           ::MenuItemSelectedGradientEnd            := RGB(182, 189, 210)
           ::MenuStripGradientBegin                 := GetSysColor( COLOR_BTNFACE )
           ::MenuStripGradientEnd                   := RGB(246, 245, 244)
           ::OverflowButtonGradientBegin            := RGB(225, 222, 217)
           ::OverflowButtonGradientEnd              := GetSysColor( COLOR_BTNSHADOW )
           ::OverflowButtonGradientMiddle           := RGB(216, 213, 206)
           ::RaftingContainerGradientBegin          := GetSysColor( COLOR_BTNFACE )
           ::RaftingContainerGradientEnd            := RGB(246, 245, 244)
           ::SeparatorDark                          := RGB(166, 166, 166)
           ::SeparatorLight                         := GetSysColor( COLOR_BTNHIGHLIGHT )
           ::StatusStripGradientBegin               := GetSysColor( COLOR_BTNFACE )
           ::StatusStripGradientEnd                 := RGB(246, 245, 244)
           ::ToolStripBorder                        := RGB(219, 216, 209)
           ::ToolStripContentPanelGradientBegin     := GetSysColor( COLOR_BTNFACE )
           ::ToolStripContentPanelGradientEnd       := RGB(246, 245, 244)
           ::ToolStripDropDownBackground            := RGB(249, 248, 247)
           ::ToolStripGradientBegin                 := RGB(245, 244, 242)
           ::ToolStripGradientEnd                   := GetSysColor( COLOR_BTNFACE )
           ::ToolStripGradientMiddle                := LightenColor( GetSysColor( COLOR_BTNFACE ), 100 )//RGB(234, 232, 228)
           ::ToolStripPanelGradientBegin            := GetSysColor( COLOR_BTNFACE )
           ::ToolStripPanelGradientEnd              := RGB(246, 245, 244)

      CASE cScheme == "NormalColor"
           ::ButtonCheckedGradientBegin             := IIF( ::UseSystemColors, NIL, RGB(255, 223, 154) )
           ::ButtonCheckedGradientEnd               := IIF( ::UseSystemColors, NIL, RGB(255, 166, 76) )
           ::ButtonCheckedGradientMiddle            := IIF( ::UseSystemColors, NIL, RGB(255, 195, 116) )
           ::ButtonCheckedHighlight                 := RGB(195, 211, 237)
           ::ButtonCheckedHighlightBorder           := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonPressedBorder                    := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(0, 0, 128) )
           ::ButtonPressedGradientBegin             := IIF( ::UseSystemColors, RGB(152, 181, 226), RGB(254, 128, 62) )
           ::ButtonPressedGradientEnd               := IIF( ::UseSystemColors, RGB(152, 181, 226), RGB(255, 223, 154) )
           ::ButtonPressedGradientMiddle            := IIF( ::UseSystemColors, RGB(152, 181, 226), RGB(255, 177, 109) )
           ::ButtonPressedHighlight                 := IIF( ::UseSystemColors, RGB(150, 179, 225), RGB(150, 179, 225) )
           ::ButtonPressedHighlightBorder           := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonSelectedBorder                   := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(0, 0, 128) )
           ::ButtonSelectedGradientBegin            := IIF( ::UseSystemColors, RGB(193, 210, 238), RGB(255, 255, 222) )
           ::ButtonSelectedGradientEnd              := IIF( ::UseSystemColors, RGB(193, 210, 238), RGB(255, 203, 136) )
           ::ButtonSelectedGradientMiddle           := IIF( ::UseSystemColors, RGB(193, 210, 238), RGB(255, 225, 172) )
           ::ButtonSelectedHighlight                := IIF( ::UseSystemColors, RGB(195, 211, 237), RGB(195, 211, 237) )
           ::ButtonSelectedHighlightBorder          := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(0, 0, 128) )
           ::CheckBackground                        := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(255, 192, 111) )
           ::CheckPressedBackground                 := IIF( ::UseSystemColors, RGB(152, 181, 226), RGB(254, 128, 62) )
           ::CheckSelectedBackground                := IIF( ::UseSystemColors, RGB(152, 181, 226), RGB(254, 128, 62) )
           ::GripDark                               := IIF( ::UseSystemColors, RGB(193, 190, 179), RGB(39, 65, 118) )
           ::GripLight                              := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 255, 255) )
           ::ImageMarginGradientBegin               := IIF( ::UseSystemColors, RGB(251, 250, 246), RGB(227, 239, 255) )
           ::ImageMarginGradientEnd                 := IIF( ::UseSystemColors, GetSysColor( COLOR_3DFACE ), RGB(123, 164, 224) )
           ::ImageMarginGradientMiddle              := IIF( ::UseSystemColors, RGB(246, 244, 236), RGB(203, 225, 252) )
           ::ImageMarginRevealedGradientBegin       := IIF( ::UseSystemColors, RGB(247, 246, 239), RGB(203, 221, 246) )
           ::ImageMarginRevealedGradientEnd         := IIF( ::UseSystemColors, RGB(238, 235, 220), RGB(114, 155, 215) )
           ::ImageMarginRevealedGradientMiddle      := IIF( ::UseSystemColors, RGB(242, 240, 228), RGB(161, 197, 249) )
           ::MenuBorder                             := IIF( ::UseSystemColors, RGB(138, 134, 122), RGB(0, 45, 150) )
           ::MenuItemBorder                         := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(0, 0, 128) )
           ::MenuItemPressedGradientBegin           := IIF( ::UseSystemColors, RGB(251, 250, 246), RGB(227, 239, 255) )
           ::MenuItemPressedGradientEnd             := IIF( ::UseSystemColors, RGB(246, 244, 236), RGB(123, 164, 224) )
           ::MenuItemPressedGradientMiddle          := IIF( ::UseSystemColors, RGB(242, 240, 228), RGB(161, 197, 249) )
           ::MenuItemSelected                       := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 238, 194) )
           ::MenuItemSelectedGradientBegin          := IIF( ::UseSystemColors, RGB(193, 210, 238), RGB(255, 255, 222) )
           ::MenuItemSelectedGradientEnd            := IIF( ::UseSystemColors, RGB(193, 210, 238), RGB(255, 203, 136) )
           ::MenuStripGradientBegin                 := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(158, 190, 245) )
           ::MenuStripGradientEnd                   := IIF( ::UseSystemColors, RGB(251, 250, 247), RGB(196, 218, 250) )
           ::OverflowButtonGradientBegin            := IIF( ::UseSystemColors, RGB(242, 240, 228), RGB(127, 177, 250) )
           ::OverflowButtonGradientEnd              := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNSHADOW ), RGB(0, 53, 145) )
           ::OverflowButtonGradientMiddle           := IIF( ::UseSystemColors, RGB(238, 235, 220), RGB(82, 127, 208) )
           ::RaftingContainerGradientBegin          := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(158, 190, 245) )
           ::RaftingContainerGradientEnd            := IIF( ::UseSystemColors, RGB(251, 250, 247), RGB(196, 218, 250) )
           ::SeparatorDark                          := IIF( ::UseSystemColors, RGB(197, 194, 184), RGB(106, 140, 203) )
           ::SeparatorLight                         := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNHIGHLIGHT ), RGB(241, 249, 255) )
           ::StatusStripGradientBegin               := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(158, 190, 245) )
           ::StatusStripGradientEnd                 := IIF( ::UseSystemColors, RGB(251, 250, 247), RGB(196, 218, 250) )
           ::ToolStripBorder                        := IIF( ::UseSystemColors, RGB(239, 237, 222), RGB(59, 97, 156) )
           ::ToolStripContentPanelGradientBegin     := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(158, 190, 245) )
           ::ToolStripContentPanelGradientEnd       := IIF( ::UseSystemColors, RGB(251, 250, 247), RGB(196, 218, 250) )
           ::ToolStripDropDownBackground            := IIF( ::UseSystemColors, RGB(252, 252, 249), RGB(246, 246, 246) )
           ::ToolStripGradientBegin                 := IIF( ::UseSystemColors, RGB(251, 250, 246), RGB(227, 239, 255) )
           ::ToolStripGradientEnd                   := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(123, 164, 224) )
           ::ToolStripGradientMiddle                := IIF( ::UseSystemColors, RGB(246, 244, 236), RGB(203, 225, 252) )
           ::ToolStripPanelGradientBegin            := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(158, 190, 245) )
           ::ToolStripPanelGradientEnd              := IIF( ::UseSystemColors, RGB(251, 250, 247), RGB(196, 218, 250) )

      CASE cScheme == "HomeStead"
           ::ButtonCheckedGradientBegin             := IIF( ::UseSystemColors, NIL, RGB(255, 223, 154) )
           ::ButtonCheckedGradientEnd               := IIF( ::UseSystemColors, NIL, RGB(255, 166, 76) )
           ::ButtonCheckedGradientMiddle            := IIF( ::UseSystemColors, NIL, RGB(255, 195, 116) )
           ::ButtonCheckedHighlight                 := RGB(223, 227, 213)
           ::ButtonCheckedHighlightBorder           := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonPressedBorder                    := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(63, 93, 56) )
           ::ButtonPressedGradientBegin             := IIF( ::UseSystemColors, RGB(201, 208, 184), RGB(254, 128, 62) )
           ::ButtonPressedGradientEnd               := IIF( ::UseSystemColors, RGB(201, 208, 184), RGB(255, 223, 154) )
           ::ButtonPressedGradientMiddle            := IIF( ::UseSystemColors, RGB(201, 208, 184), RGB(255, 177, 109) )
           ::ButtonPressedHighlight                 := IIF( ::UseSystemColors, RGB(200, 206, 182), RGB(200, 206, 182) )
           ::ButtonPressedHighlightBorder           := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonSelectedBorder                   := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(63, 93, 56) )
           ::ButtonSelectedGradientBegin            := IIF( ::UseSystemColors, RGB(223, 227, 212), RGB(255, 255, 222) )
           ::ButtonSelectedGradientEnd              := IIF( ::UseSystemColors, RGB(223, 227, 212), RGB(255, 203, 136) )
           ::ButtonSelectedGradientMiddle           := IIF( ::UseSystemColors, RGB(223, 227, 212), RGB(255, 225, 172) )
           ::ButtonSelectedHighlight                := IIF( ::UseSystemColors, RGB(223, 227, 213), RGB(223, 227, 213) )
           ::ButtonSelectedHighlightBorder          := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(63, 93, 56) )
           ::CheckBackground                        := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(255, 192, 111) )
           ::CheckPressedBackground                 := IIF( ::UseSystemColors, RGB(201, 208, 184), RGB(254, 128, 62) )
           ::CheckSelectedBackground                := IIF( ::UseSystemColors, RGB(201, 208, 184), RGB(254, 128, 62) )
           ::GripDark                               := IIF( ::UseSystemColors, RGB(193, 190, 179), RGB(81, 94, 51) )
           ::GripLight                              := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 255, 255) )
           ::ImageMarginGradientBegin               := IIF( ::UseSystemColors, RGB(251, 250, 246), RGB(255, 255, 237) )
           ::ImageMarginGradientEnd                 := IIF( ::UseSystemColors, GetSysColor( COLOR_3DFACE ), RGB(181, 196, 143) )
           ::ImageMarginGradientMiddle              := IIF( ::UseSystemColors, RGB(246, 244, 236), RGB(206, 220, 167) )
           ::ImageMarginRevealedGradientBegin       := IIF( ::UseSystemColors, RGB(247, 246, 239), RGB(230, 230, 209) )
           ::ImageMarginRevealedGradientEnd         := IIF( ::UseSystemColors, RGB(238, 235, 220), RGB(160, 177, 116) )
           ::ImageMarginRevealedGradientMiddle      := IIF( ::UseSystemColors, RGB(242, 240, 228), RGB(186, 201, 143) )
           ::MenuBorder                             := IIF( ::UseSystemColors, RGB(138, 134, 122), RGB(117, 141, 94) )
           ::MenuItemBorder                         := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(63, 93, 56) )
           ::MenuItemPressedGradientBegin           := IIF( ::UseSystemColors, RGB(251, 250, 246), RGB(237, 240, 214) )
           ::MenuItemPressedGradientEnd             := IIF( ::UseSystemColors, RGB(246, 244, 236), RGB(181, 196, 143) )
           ::MenuItemPressedGradientMiddle          := IIF( ::UseSystemColors, RGB(242, 240, 228), RGB(186, 201, 143) )
           ::MenuItemSelected                       := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 238, 194) )
           ::MenuItemSelectedGradientBegin          := IIF( ::UseSystemColors, RGB(223, 227, 212), RGB(255, 255, 222) )
           ::MenuItemSelectedGradientEnd            := IIF( ::UseSystemColors, RGB(223, 227, 212), RGB(255, 203, 136) )
           ::MenuStripGradientBegin                 := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(217, 217, 167) )
           ::MenuStripGradientEnd                   := IIF( ::UseSystemColors, RGB(251, 250, 247), RGB(242, 241, 228) )
           ::OverflowButtonGradientBegin            := IIF( ::UseSystemColors, RGB(242, 240, 228), RGB(186, 204, 150) )
           ::OverflowButtonGradientEnd              := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNSHADOW ), RGB(96, 119, 107) )
           ::OverflowButtonGradientMiddle           := IIF( ::UseSystemColors, RGB(238, 235, 220), RGB(141, 160, 107) )
           ::RaftingContainerGradientBegin          := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(217, 217, 167) )
           ::RaftingContainerGradientEnd            := IIF( ::UseSystemColors, RGB(251, 250, 247), RGB(242, 241, 228) )
           ::SeparatorDark                          := IIF( ::UseSystemColors, RGB(197, 194, 184), RGB(96, 128, 88) )
           ::SeparatorLight                         := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNHIGHLIGHT ), RGB(244, 247, 222) )
           ::StatusStripGradientBegin               := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(217, 217, 167) )
           ::StatusStripGradientEnd                 := IIF( ::UseSystemColors, RGB(251, 250, 247), RGB(242, 241, 228) )
           ::ToolStripBorder                        := IIF( ::UseSystemColors, RGB(239, 237, 222), RGB(96, 128, 88) )
           ::ToolStripContentPanelGradientBegin     := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(217, 217, 167) )
           ::ToolStripContentPanelGradientEnd       := IIF( ::UseSystemColors, RGB(251, 250, 247), RGB(242, 241, 228) )
           ::ToolStripDropDownBackground            := IIF( ::UseSystemColors, RGB(252, 252, 249), RGB(244, 244, 238) )
           ::ToolStripGradientBegin                 := IIF( ::UseSystemColors, RGB(251, 250, 246), RGB(255, 255, 237) )
           ::ToolStripGradientEnd                   := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(181, 196, 143) )
           ::ToolStripGradientMiddle                := IIF( ::UseSystemColors, RGB(246, 244, 236), RGB(206, 220, 167) )
           ::ToolStripPanelGradientBegin            := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(217, 217, 167) )
           ::ToolStripPanelGradientEnd              := IIF( ::UseSystemColors, RGB(251, 250, 247), RGB(242, 241, 228) )

      CASE cScheme == "Metallic"
           ::ButtonCheckedGradientBegin             := IIF( ::UseSystemColors, NIL, RGB(255, 223, 154) )
           ::ButtonCheckedGradientEnd               := IIF( ::UseSystemColors, NIL, RGB(255, 166, 76) )
           ::ButtonCheckedGradientMiddle            := IIF( ::UseSystemColors, NIL, RGB(255, 195, 116) )
           ::ButtonCheckedHighlight                 := RGB(231, 232, 235)
           ::ButtonCheckedHighlightBorder           := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonPressedBorder                    := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(75, 75, 111) )
           ::ButtonPressedGradientBegin             := IIF( ::UseSystemColors, RGB(217, 218, 223), RGB(254, 128, 62) )
           ::ButtonPressedGradientEnd               := IIF( ::UseSystemColors, RGB(217, 218, 223), RGB(255, 223, 154) )
           ::ButtonPressedGradientMiddle            := IIF( ::UseSystemColors, RGB(217, 218, 223), RGB(255, 177, 109) )
           ::ButtonPressedHighlight                 := IIF( ::UseSystemColors, RGB(215, 216, 222), RGB(215, 216, 222) )
           ::ButtonPressedHighlightBorder           := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonSelectedBorder                   := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(75, 75, 111) )
           ::ButtonSelectedGradientBegin            := IIF( ::UseSystemColors, RGB(232, 233, 236), RGB(255, 255, 222) )
           ::ButtonSelectedGradientEnd              := IIF( ::UseSystemColors, RGB(232, 233, 236), RGB(255, 203, 136) )
           ::ButtonSelectedGradientMiddle           := IIF( ::UseSystemColors, RGB(232, 233, 236), RGB(255, 225, 172) )
           ::ButtonSelectedHighlight                := IIF( ::UseSystemColors, RGB(231, 232, 235), RGB(231, 232, 235) )
           ::ButtonSelectedHighlightBorder          := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(75, 75, 111) )
           ::CheckBackground                        := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(255, 192, 111) )
           ::CheckPressedBackground                 := IIF( ::UseSystemColors, RGB(217, 218, 223), RGB(254, 128, 62) )
           ::CheckSelectedBackground                := IIF( ::UseSystemColors, RGB(217, 218, 223), RGB(254, 128, 62) )
           ::GripDark                               := IIF( ::UseSystemColors, RGB(182, 182, 185), RGB(84, 84, 117) )
           ::GripLight                              := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 255, 255) )
           ::ImageMarginGradientBegin               := IIF( ::UseSystemColors, RGB(248, 248, 249), RGB(249, 249, 255) )
           ::ImageMarginGradientEnd                 := IIF( ::UseSystemColors, GetSysColor( COLOR_3DFACE ), RGB(147, 145, 176) )
           ::ImageMarginGradientMiddle              := IIF( ::UseSystemColors, RGB(240, 239, 241), RGB(225, 226, 236) )
           ::ImageMarginRevealedGradientBegin       := IIF( ::UseSystemColors, RGB(243, 242, 244), RGB(215, 215, 226) )
           ::ImageMarginRevealedGradientEnd         := IIF( ::UseSystemColors, RGB(227, 226, 230), RGB(118, 116, 151) )
           ::ImageMarginRevealedGradientMiddle      := IIF( ::UseSystemColors, RGB(233, 233, 235), RGB(184, 185, 202) )
           ::MenuBorder                             := IIF( ::UseSystemColors, RGB(126, 126, 129), RGB(124, 124, 148) )
           ::MenuItemBorder                         := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(75, 75, 111) )
           ::MenuItemPressedGradientBegin           := IIF( ::UseSystemColors, RGB(248, 248, 249), RGB(232, 233, 242) )
           ::MenuItemPressedGradientEnd             := IIF( ::UseSystemColors, RGB(240, 239, 241), RGB(172, 170, 194) )
           ::MenuItemPressedGradientMiddle          := IIF( ::UseSystemColors, RGB(233, 233, 235), RGB(184, 185, 202) )
           ::MenuItemSelected                       := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 238, 194) )
           ::MenuItemSelectedGradientBegin          := IIF( ::UseSystemColors, RGB(232, 233, 236), RGB(255, 255, 222) )
           ::MenuItemSelectedGradientEnd            := IIF( ::UseSystemColors, RGB(232, 233, 236), RGB(255, 203, 136) )
           ::MenuStripGradientBegin                 := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(215, 215, 229) )
           ::MenuStripGradientEnd                   := IIF( ::UseSystemColors, RGB(249, 248, 249), RGB(243, 243, 247) )
           ::OverflowButtonGradientBegin            := IIF( ::UseSystemColors, RGB(233, 233, 235), RGB(186, 185, 206) )
           ::OverflowButtonGradientEnd              := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNSHADOW ), RGB(118, 116, 146) )
           ::OverflowButtonGradientMiddle           := IIF( ::UseSystemColors, RGB(227, 226, 230), RGB(156, 155, 180) )
           ::RaftingContainerGradientBegin          := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(215, 215, 229) )
           ::RaftingContainerGradientEnd            := IIF( ::UseSystemColors, RGB(249, 248, 249), RGB(243, 243, 247) )
           ::SeparatorDark                          := IIF( ::UseSystemColors, RGB(186, 186, 189), RGB(110, 109, 143) )
           ::SeparatorLight                         := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNHIGHLIGHT ), RGB(255, 255, 255) )
           ::StatusStripGradientBegin               := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(215, 215, 229) )
           ::StatusStripGradientEnd                 := IIF( ::UseSystemColors, RGB(249, 248, 249), RGB(243, 243, 247) )
           ::ToolStripBorder                        := IIF( ::UseSystemColors, RGB(229, 228, 232), RGB(124, 124, 148) )
           ::ToolStripContentPanelGradientBegin     := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(215, 215, 229) )
           ::ToolStripContentPanelGradientEnd       := IIF( ::UseSystemColors, RGB(249, 248, 249), RGB(243, 243, 247) )
           ::ToolStripDropDownBackground            := IIF( ::UseSystemColors, RGB(251, 250, 251), RGB(253, 250, 255) )
           ::ToolStripGradientBegin                 := IIF( ::UseSystemColors, RGB(248, 248, 249), RGB(249, 249, 255) )
           ::ToolStripGradientEnd                   := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(147, 145, 176) )
           ::ToolStripGradientMiddle                := IIF( ::UseSystemColors, RGB(240, 239, 241), RGB(225, 226, 236) )
           ::ToolStripPanelGradientBegin            := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(215, 215, 229) )
           ::ToolStripPanelGradientEnd              := IIF( ::UseSystemColors, RGB(249, 248, 249), RGB(243, 243, 247) )

      CASE cScheme == "MediaCenter"
           ::ButtonCheckedGradientBegin             := IIF( ::UseSystemColors, NIL, RGB(226, 229, 238) )
           ::ButtonCheckedGradientEnd               := IIF( ::UseSystemColors, NIL, RGB(226, 229, 238) )
           ::ButtonCheckedGradientMiddle            := IIF( ::UseSystemColors, NIL, RGB(226, 229, 238) )
           ::ButtonCheckedHighlight                 := RGB(196, 208, 229)
           ::ButtonCheckedHighlightBorder           := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonPressedBorder                    := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(51, 94, 168) )
           ::ButtonPressedGradientBegin             := IIF( ::UseSystemColors, RGB(153, 175, 212), RGB(153, 175, 212) )
           ::ButtonPressedGradientEnd               := IIF( ::UseSystemColors, RGB(153, 175, 212), RGB(153, 175, 212) )
           ::ButtonPressedGradientMiddle            := IIF( ::UseSystemColors, RGB(153, 175, 212), RGB(153, 175, 212) )
           ::ButtonPressedHighlight                 := IIF( ::UseSystemColors, RGB(152, 173, 210), RGB(152, 173, 210) )
           ::ButtonPressedHighlightBorder           := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonSelectedBorder                   := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(51, 94, 168) )
           ::ButtonSelectedGradientBegin            := IIF( ::UseSystemColors, RGB(194, 207, 229), RGB(194, 207, 229) )
           ::ButtonSelectedGradientEnd              := IIF( ::UseSystemColors, RGB(194, 207, 229), RGB(194, 207, 229) )
           ::ButtonSelectedGradientMiddle           := IIF( ::UseSystemColors, RGB(194, 207, 229), RGB(194, 207, 229) )
           ::ButtonSelectedHighlight                := IIF( ::UseSystemColors, RGB(196, 208, 229), RGB(196, 208, 229) )
           ::ButtonSelectedHighlightBorder          := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(51, 94, 168) )
           ::CheckBackground                        := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(226, 229, 238) )
           ::CheckPressedBackground                 := IIF( ::UseSystemColors, RGB(153, 175, 212), RGB(51, 94, 168) )
           ::CheckSelectedBackground                := IIF( ::UseSystemColors, RGB(153, 175, 212), RGB(51, 94, 168) )
           ::GripDark                               := IIF( ::UseSystemColors, RGB(189, 188, 191), RGB(189, 188, 191) )
           ::GripLight                              := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(255, 255, 255) )
           ::ImageMarginGradientBegin               := IIF( ::UseSystemColors, RGB(250, 250, 251), RGB(252, 252, 252) )
           ::ImageMarginGradientEnd                 := IIF( ::UseSystemColors, GetSysColor( COLOR_3DFACE ), RGB(235, 233, 237) )
           ::ImageMarginGradientMiddle              := IIF( ::UseSystemColors, RGB(245, 244, 246), RGB(245, 244, 246) )
           ::ImageMarginRevealedGradientBegin       := IIF( ::UseSystemColors, RGB(247, 246, 248), RGB(247, 246, 248) )
           ::ImageMarginRevealedGradientEnd         := IIF( ::UseSystemColors, RGB(237, 235, 239), RGB(228, 226, 230) )
           ::ImageMarginRevealedGradientMiddle      := IIF( ::UseSystemColors, RGB(241, 240, 242), RGB(241, 240, 242) )
           ::MenuBorder                             := IIF( ::UseSystemColors, RGB(134, 133, 136), RGB(134, 133, 136) )
           ::MenuItemBorder                         := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(51, 94, 168) )
           ::MenuItemPressedGradientBegin           := IIF( ::UseSystemColors, RGB(250, 250, 251), RGB(252, 252, 252) )
           ::MenuItemPressedGradientEnd             := IIF( ::UseSystemColors, RGB(245, 244, 246), RGB(245, 244, 246) )
           ::MenuItemPressedGradientMiddle          := IIF( ::UseSystemColors, RGB(241, 240, 242), RGB(241, 240, 242) )
           ::MenuItemSelected                       := IIF( ::UseSystemColors, GetSysColor( COLOR_WINDOW ), RGB(194, 207, 229) )
           ::MenuItemSelectedGradientBegin          := IIF( ::UseSystemColors, RGB(194, 207, 229), RGB(194, 207, 229) )
           ::MenuItemSelectedGradientEnd            := IIF( ::UseSystemColors, RGB(194, 207, 229), RGB(194, 207, 229) )
           ::MenuStripGradientBegin                 := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(235, 233, 237) )
           ::MenuStripGradientEnd                   := IIF( ::UseSystemColors, RGB(251, 250, 251), RGB(251, 250, 251) )
           ::OverflowButtonGradientBegin            := IIF( ::UseSystemColors, RGB(241, 240, 242), RGB(242, 242, 242) )
           ::OverflowButtonGradientEnd              := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNSHADOW ), RGB(167, 166, 170) )
           ::OverflowButtonGradientMiddle           := IIF( ::UseSystemColors, RGB(237, 235, 239), RGB(224, 224, 225) )
           ::RaftingContainerGradientBegin          := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(235, 233, 237) )
           ::RaftingContainerGradientEnd            := IIF( ::UseSystemColors, RGB(251, 250, 251), RGB(251, 250, 251) )
           ::SeparatorDark                          := IIF( ::UseSystemColors, RGB(193, 193, 196), RGB(193, 193, 196) )
           ::SeparatorLight                         := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNHIGHLIGHT ), RGB(255, 255, 255) )
           ::StatusStripGradientBegin               := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(235, 233, 237) )
           ::StatusStripGradientEnd                 := IIF( ::UseSystemColors, RGB(251, 250, 251), RGB(251, 250, 251) )
           ::ToolStripBorder                        := IIF( ::UseSystemColors, RGB(238, 237, 240), RGB(238, 237, 240) )
           ::ToolStripContentPanelGradientBegin     := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(235, 233, 237) )
           ::ToolStripContentPanelGradientEnd       := IIF( ::UseSystemColors, RGB(251, 250, 251), RGB(251, 250, 251) )
           ::ToolStripDropDownBackground            := IIF( ::UseSystemColors, RGB(252, 252, 252), RGB(252, 252, 252) )
           ::ToolStripGradientBegin                 := IIF( ::UseSystemColors, RGB(250, 250, 251), RGB(252, 252, 252) )
           ::ToolStripGradientEnd                   := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(235, 233, 237) )
           ::ToolStripGradientMiddle                := IIF( ::UseSystemColors, RGB(245, 244, 246), RGB(245, 244, 246) )
           ::ToolStripPanelGradientBegin            := IIF( ::UseSystemColors, GetSysColor( COLOR_BTNFACE ), RGB(235, 233, 237) )
           ::ToolStripPanelGradientEnd              := IIF( ::UseSystemColors, RGB(251, 250, 251), RGB(251, 250, 251) )

      CASE cScheme == "Aero"
           ::ButtonCheckedGradientBegin             := RGB(194, 224, 255)//GetSysColor( COLOR_INACTIVECAPTION )
           ::ButtonCheckedGradientEnd               := RGB(194, 224, 255)//GetSysColor( COLOR_INACTIVECAPTION )
           ::ButtonCheckedGradientMiddle            := NIL
           ::ButtonCheckedHighlight                 := RGB(196, 225, 255)
           ::ButtonCheckedHighlightBorder           := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonPressedBorder                    := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonPressedGradientBegin             := RGB(153, 204, 255)
           ::ButtonPressedGradientEnd               := RGB(153, 204, 255)
           ::ButtonPressedGradientMiddle            := RGB(153, 204, 255)
           ::ButtonPressedHighlight                 := RGB(152, 203, 255)
           ::ButtonPressedHighlightBorder           := GetSysColor( COLOR_HIGHLIGHT )
           ::ButtonSelectedBorder                   := IIF( ::UseSystemColors, GetSysColor( COLOR_HIGHLIGHT ), RGB(51, 94, 168) )
           ::ButtonSelectedGradientBegin            := RGB(194, 224, 255)
           ::ButtonSelectedGradientEnd              := RGB(194, 224, 255)
           ::ButtonSelectedGradientMiddle           := RGB(194, 224, 255)
           ::ButtonSelectedHighlight                := RGB(196, 225, 255)
           ::ButtonSelectedHighlightBorder          := GetSysColor( COLOR_HIGHLIGHT )
           ::CheckBackground                        := GetSysColor( COLOR_HIGHLIGHT )
           ::CheckPressedBackground                 := RGB(153, 204, 255)
           ::CheckSelectedBackground                := RGB(153, 204, 255)
           ::GripDark                               := RGB(184, 184, 184)
           ::GripLight                              := GetSysColor( COLOR_WINDOW )
           ::ImageMarginGradientBegin               := RGB(252, 252, 252)
           ::ImageMarginGradientEnd                 := GetSysColor( COLOR_3DFACE )
           ::ImageMarginGradientMiddle              := RGB(250, 250, 250)
           ::ImageMarginRevealedGradientBegin       := RGB(251, 251, 251)
           ::ImageMarginRevealedGradientEnd         := RGB(245, 245, 245)
           ::ImageMarginRevealedGradientMiddle      := RGB(247, 247, 247)
           ::MenuBorder                             := RGB(128, 128, 128)
           ::MenuItemBorder                         := GetSysColor( COLOR_HIGHLIGHT )
           ::MenuItemPressedGradientBegin           := RGB(252, 252, 252)
           ::MenuItemPressedGradientEnd             := RGB(250, 250, 250)
           ::MenuItemPressedGradientMiddle          := RGB(247, 247, 247)
           ::MenuItemSelected                       := RGB(194, 224, 255)
           ::MenuItemSelectedGradientBegin          := RGB(194, 224, 255)
           ::MenuItemSelectedGradientEnd            := RGB(194, 224, 255)
           ::MenuStripGradientBegin                 := GetSysColor( COLOR_BTNFACE )
           ::MenuStripGradientEnd                   := RGB(253, 253, 253)
           
           ::OverflowButtonGradientBegin            := RGB(247, 247, 247)
           ::OverflowButtonGradientEnd              := GetSysColor( COLOR_BTNSHADOW )
           ::OverflowButtonGradientMiddle           := GetSysColor( COLOR_BTNSHADOW ) //RGB(245, 245, 245)
           
           ::RaftingContainerGradientBegin          := GetSysColor( COLOR_BTNFACE )
           ::RaftingContainerGradientEnd            := RGB(253, 253, 253)
           ::SeparatorDark                          := RGB(189, 189, 189)
           ::SeparatorLight                         := GetSysColor( COLOR_BTNHIGHLIGHT )
           ::StatusStripGradientBegin               := GetSysColor( COLOR_BTNFACE )
           ::StatusStripGradientEnd                 := RGB(253, 253, 253)
           ::ToolStripBorder                        := RGB(246, 246, 246)
           ::ToolStripContentPanelGradientBegin     := GetSysColor( COLOR_BTNFACE )
           ::ToolStripContentPanelGradientEnd       := RGB(253, 253, 253)
           ::ToolStripDropDownBackground            := RGB(253, 253, 253)
           ::ToolStripGradientBegin                 := RGB(252, 252, 252)
           ::ToolStripGradientEnd                   := GetSysColor( COLOR_BTNFACE )
           ::ToolStripGradientMiddle                := RGB(250, 250, 250)
           ::ToolStripPanelGradientBegin            := GetSysColor( COLOR_BTNFACE )
           ::ToolStripPanelGradientEnd              := RGB(253, 253, 253)
   ENDCASE
   IF !EMPTY( __aSystemPen )
      IF ::Brush != NIL
         ::Brush:Clean()
      ENDIF
      IF ::Pen != NIL
         ::Pen:Clean()
      ENDIF
   ENDIF
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

//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------
CLASS ProfessionalColorTablePen
   DATA Parent
   METHOD Init() CONSTRUCTOR
   METHOD Clean()
   error HANDLER OnError()
ENDCLASS

METHOD Init( oParent ) CLASS ProfessionalColorTablePen
   ::Parent := oParent
RETURN Self

METHOD Clean() CLASS ProfessionalColorTablePen
   LOCAL aProp, n, nColor, hPen
   FOR n := 1 TO LEN( __aSystemPen )
       DeleteObject( __aSystemPen[n][2] )
       nColor := __objSendMsg( ::Parent, UPPER( __aSystemPen[n][1] ) )
       hPen := CreatePen( PS_SOLID, 1, nColor )
       __objSendMsg( Self, __aSystemPen[n][1], hPen )
      __aSystemPen[n][2] := hPen
   NEXT
RETURN Self

METHOD OnError( xValue ) CLASS ProfessionalColorTablePen
   LOCAL nColor, n, hPen, cMsg := __GetMessage()
   IF PCount() == 0 .AND. xValue == NIL
      nColor := __objSendMsg( ::Parent, UPPER( cMsg ) )
      hPen := CreatePen( PS_SOLID, 1, nColor )

      __objAddData( Self, cMsg, HB_OO_CLSTP_EXPORTED )
      __objSendMsg( Self, cMsg, hPen )

      AADD( __aSystemPen, { cMsg, hPen } )
      RETURN hPen
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------
CLASS ProfessionalColorTableBrush
   DATA Parent
   METHOD Init() CONSTRUCTOR
   METHOD Clean()
   error HANDLER OnError()
ENDCLASS

METHOD Init( oParent ) CLASS ProfessionalColorTableBrush
   ::Parent := oParent
RETURN Self

METHOD Clean() CLASS ProfessionalColorTableBrush
   LOCAL aProp, n, nColor, hBrush
   FOR n := 1 TO LEN( __aSystemBrush )
       DeleteObject( __aSystemBrush[n][2] )
       nColor := __objSendMsg( ::Parent, UPPER( __aSystemBrush[n][1] ) )
       hBrush := CreateSolidBrush( nColor )
       __objSendMsg( Self, __aSystemBrush[n][1], hBrush )
      __aSystemBrush[n][2] := hBrush
   NEXT
RETURN Self

METHOD OnError( xValue ) CLASS ProfessionalColorTableBrush
   LOCAL nColor, n, hBrush, cMsg := __GetMessage()
   IF PCount() == 0 .AND. xValue == NIL
      nColor := __objSendMsg( ::Parent, UPPER( cMsg ) )
      hBrush := CreateSolidBrush( nColor )

      __objAddData( Self, cMsg, HB_OO_CLSTP_EXPORTED )
      __objSendMsg( Self, cMsg, hBrush )

      AADD( __aSystemBrush, { cMsg, hBrush } )
      RETURN hBrush
   ENDIF
RETURN NIL

