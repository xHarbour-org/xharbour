/*
 * HWGUI demo de tela Maximizada
 * 
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
 *
 * Demo montado por Sandro <sandrorrfreire@yahoo.com.br>
*/

#include "windows.ch"
#include "guilib.ch"
 
* --------------------------------------------
function Main
* --------------------------------------------
Private oMain, temp1
Private oDlg
Private oFont := Nil
 
   INIT WINDOW oMain MAIN TITLE "Demo Maximize"  MAXIMIZE

          
    MENU OF oMain
      MENU TITLE "&Arquivo"

         MENUITEM "&Maximize   " ACTION oMain:Maximize()
         MENUITEM "&Minimize   " ACTION oMain:Minimize()
         MENUITEM "&Sair" ACTION EndWindow()

      ENDMENU
    ENDMENU

    ACTIVATE WINDOW oMain 
 
return nil
 
