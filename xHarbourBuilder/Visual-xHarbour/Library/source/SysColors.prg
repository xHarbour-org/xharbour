/*
 * $Id$
 */

// SystemColors class - holds system colours


#include "winuser.ch"
#include "hbclass.ch"

//--------------------------------------------------------------------------------------------------

CLASS __SystemColors

   ACCESS BtnFace                  INLINE GetSysColor( COLOR_BTNFACE                 )
   ACCESS ScrollBars               INLINE GetSysColor( COLOR_SCROLLBAR               )
   ACCESS BackGround               INLINE GetSysColor( COLOR_BACKGROUND              )
   ACCESS ActiveCaption            INLINE GetSysColor( COLOR_ACTIVECAPTION           )
   ACCESS InactiveCaption          INLINE GetSysColor( COLOR_INACTIVECAPTION         )
   ACCESS Menu                     INLINE GetSysColor( COLOR_MENU                    )
   ACCESS Window                   INLINE GetSysColor( COLOR_WINDOW                  )
   ACCESS WindowFrame              INLINE GetSysColor( COLOR_WINDOWFRAME             )
   ACCESS MenuText                 INLINE GetSysColor( COLOR_MENUTEXT                )
   ACCESS WindowText               INLINE GetSysColor( COLOR_WINDOWTEXT              )
   ACCESS CaptionText              INLINE GetSysColor( COLOR_CAPTIONTEXT             )
   ACCESS ActiveBorder             INLINE GetSysColor( COLOR_ACTIVEBORDER            )
   ACCESS InactiveBorder           INLINE GetSysColor( COLOR_INACTIVEBORDER          )
   ACCESS AppWorkSpace             INLINE GetSysColor( COLOR_APPWORKSPACE            )
   ACCESS Highlight                INLINE GetSysColor( COLOR_HIGHLIGHT               )
   ACCESS HighlightText            INLINE GetSysColor( COLOR_HIGHLIGHTTEXT           )
   ACCESS BtnHighlight             INLINE GetSysColor( COLOR_BTNHIGHLIGHT            )
   ACCESS BtnShadow                INLINE GetSysColor( COLOR_BTNSHADOW               )
   ACCESS GrayText                 INLINE GetSysColor( COLOR_GRAYTEXT                )
   ACCESS BtnText                  INLINE GetSysColor( COLOR_BTNTEXT                 )
   ACCESS InactiveCaptionText      INLINE GetSysColor( COLOR_INACTIVECAPTIONTEXT     )
   ACCESS DkShadow3d               INLINE GetSysColor( COLOR_3DDKSHADOW              )
   ACCESS Light3d                  INLINE GetSysColor( COLOR_3DLIGHT                 )
   ACCESS InfoText                 INLINE GetSysColor( COLOR_INFOTEXT                )
   ACCESS InfoBk                   INLINE GetSysColor( COLOR_INFOBK                  )
   ACCESS HotLight                 INLINE GetSysColor( COLOR_HOTLIGHT                )
   ACCESS GradientActiveCaption    INLINE GetSysColor( COLOR_GRADIENTACTIVECAPTION   )
   ACCESS GradientInactiveCaption  INLINE GetSysColor( COLOR_GRADIENTINACTIVECAPTION )

   ACCESS DeskTop                  INLINE ::BackGround
   ACCESS Face3D                   INLINE ::BtnFace
   ACCESS Shadow3D                 INLINE ::BtnShadow
   ACCESS Highlight3D              INLINE ::BtnHighlight
   ACCESS HiLight3D                INLINE ::BtnHighlight
   ACCESS BtnHiLight               INLINE ::BtnHighlight

ENDCLASS

