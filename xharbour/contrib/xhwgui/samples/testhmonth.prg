//================================================================//
// Programa......: Controle MonthCalendar - Exemplo
// Programador...: Marcos Antonio Gambeta
// Contato.......: marcos_gambeta@hotmail.com
// Website.......: http://geocities.yahoo.com.br/marcosgambeta/
//================================================================//
// Linguagem.....: Harbour/xHarbour + HWGUI
// Plataforma....: Windows
// Criado em ....: 17/2/2004 21:41:05
// Atualizado em : 17/2/2004 22:39:19
//================================================================//
// Este programa demonstra o uso do controle MonthCalendar da
// biblioteca HWGUI (Classe HMonthCalendar).
//================================================================//

#include "windows.ch"
#include "guilib.ch"

//================================================================//

Function Main

   Local oWnd

   SET DATE BRITISH
   SET CENTURY ON

   SetTooltipBalloon(.t.)

   INIT WINDOW oWnd MAIN TITLE "Controle MonthCalendar" ;
      AT 100,100 SIZE 640,480

   MENU OF oWnd
      MENUITEM "&Calendário 1" ACTION Dlg1()
      MENUITEM "&Calendário 2" ACTION Dlg2()
      MENUITEM "&Sair"         ACTION EndWindow()
   ENDMENU

   ACTIVATE WINDOW oWnd

   Return Nil

//================================================================//

Function Dlg1

   Local oDlg
   Local oMC
   Local oFont

   INIT DIALOG oDlg TITLE "Calendário - Exemplo 1" ;
      AT 20,20 SIZE 500,300

   PREPARE FONT oFont NAME "Arial" WIDTH 0 HEIGHT -12

   @ 20,20 MONTHCALENDAR oMC ;
      SIZE 250,250 ;
      INIT ctod("01/01/2004") ;
      ON INIT {||MsgInfo("Evento On Init","MonthCalendar")} ;
      ON CHANGE {||MsgInfo("Evento On Change","MonthCalendar")} ;
      NOTODAY NOTODAYCIRCLE WEEKNUMBERS ;
      FONT oFont ;
      TOOLTIP "MonthCalendar - NoToday - NoTodayCircle - WeekNumbers"

   @ 300,20 BUTTON "Get Date" ON CLICK {||MsgInfo(dtoc(oMC:GetValue()))} SIZE 100,40
   @ 300,60 BUTTON "Set Date" ON CLICK {||oMC:SetValue(Date())} SIZE 100,40

   ACTIVATE DIALOG oDlg

   Return Nil

//================================================================//

Function Dlg2

   Local oDlg
   Local oMC
   Local oFont

   INIT DIALOG oDlg TITLE "Calendário - Exemplo 2" ;
      AT 20,20 SIZE 500,300

   PREPARE FONT oFont NAME "Courier New" WIDTH 0 HEIGHT -12

   @ 20,20 MONTHCALENDAR oMC ;
      SIZE 250,250 ;
      INIT Date() ;
      FONT oFont

   @ 300,20 BUTTON "Get Date" ON CLICK {||MsgInfo(dtoc(oMC:GetValue()))} SIZE 100,40
   @ 300,60 BUTTON "Set Date" ON CLICK {||oMC:SetValue(Date())} SIZE 100,40

   ACTIVATE DIALOG oDlg

   Return Nil

//================================================================//

