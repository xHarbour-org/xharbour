/*
 * $Id: TApplication.prg,v 1.44 2002/11/09 03:18:16 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib TApplication CLASS module
 *
 * Copyright 2002 Augusto Infante [augusto@2vias.com.ar]
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 */



#include "hbclass.ch"
#include "what32.ch"
#include "windows.ch"
#include "debug.ch"
#include "wingdi.ch"
#include "tabctrl.ch"
#include "classex.ch"
#include "WinTypes.ch" 
#include "cstruct.ch"

GLOBAL Application
GLOBAL EXTERNAL lPrevInstance

*------------------------------------------------------------------------------*

CLASS Application

   PROPERTY OnIdle READ FOnIdle WRITE FOnIdle 
   
   DATA Msg INIT HB_CStructure( "MSG" )
   DATA Instance
   DATA InstMsg
   
   PROPERTY Handle READ FHandle
   DATA FTerminate INIT .F.
   
   DATA nFormCount            INIT 0
   DATA FrameCreated AS LOGIC INIT .F.
   DATA MultiInstance         INIT .F.
   DATA InstMsg               INIT NIL
   DATA aForms                INIT {}
   DATA AppForms              INIT {}

   PROPERTY MainForm READ FMainForm

   METHOD Initialize() CONSTRUCTOR
   METHOD Run()
   METHOD CreateForm()
   METHOD RemoveForm()
   METHOD CreateFrame()
   METHOD Terminate()                            INLINE PostQuitMessage(0)
   METHOD MessageBox( cText, cCaption, nFlags )  INLINE MessageBox( GetActiveWindow(), cText, cCaption, nFlags )
   METHOD ProcessMessages EXTERN TApplication_ProcessMessages() // Called by user code to yield.   
   METHOD MainLoop        EXTERN TApplication_MainLoop() // Called ONLY by ::Run()!!!

ENDCLASS

*------------------------------------------------------------------------------*

METHOD Initialize() CLASS Application

   LOCAL nId, cMsg

   IF !::MultiInstance
      ::InstMsg := RegisterWindowMessage( GetModuleFileName() )
      AllowSetForegroundWindow( -1 )
      IF lPrevInstance
         SendMessage( HWND_BROADCAST, ::InstMsg, 0, 0)
         PostQuitMessage(0)
         QUIT
         return(0)
      ENDIF
   ENDIF
   ::Instance := hInstance()

   InitCommonControls()
   InitCommonControlsEx()
   InitCommonControlsEx(ICC_COOL_CLASSES)

   Application := Self

   RETURN(self)

*------------------------------------------------------------------------------*

METHOD Run() CLASS Application

   ::MainLoop()
   
Return 0

*------------------------------------------------------------------------------*
METHOD CreateForm( oForm, oTarget ) CLASS Application

   LOCAL aVars, aVar

   //TraceLog( cForm, oForm, oForm:PropName )

   oForm:Create( Self )

   //oForm:Name := oForm:ClassName() //ControlName + AllTrim( Str( Len( ::AppForms ) + 1 ) )
   //__objAddData( Self, oForm:Name )
   //__ObjSetValueList( self, { { oForm:Name, oForm } } )

   //TraceLog( :Caption, :Top, :Left, :Height, :Width )

   aVars := __objGetValueList( oForm, NIL, HB_OO_CLSTP_EXPORTED )
   FOR EACH aVar IN aVars
      IF ValType( aVar[2] ) == 'O'
         aVar[2]:Create( oForm )
      ENDIF
   NEXT

   IF ::FMainForm == NIL
      oForm:GetHandle()
      ::FMainForm := oForm
   ENDIF

   oTarget := oForm

RETURN oForm

*------------------------------------------------------------------------------*
METHOD RemoveForm( oForm ) CLASS Application

   LOCAL nRet, n
   
   IF oForm == ::MainForm
      nRet := 1
   ENDIF
   
return nRet

*------------------------------------------------------------------------------*
METHOD CreateFrame( cName, oFrame ) CLASS Application

   LOCAL n

   IF ::FrameCreated
      MessageBox( 0, 'Frame is already created', MB_ICONEXCLAMATION )
      RETURN( NIL )
   ENDIF
   
   ::FrameCreated := .T.
   __objAddData( self, cName )
   
   oFrame := if( oFrame != NIL, oFrame:New( self ), TFrame():New( self ) )
   __ObjSetValueList( self, { { cName, oFrame } } )
   
   oFrame:propname := cName
   oFrame:Create()

RETURN( oFrame )

*------------------------------------------------------------------------------*
#pragma BEGINDUMP

#define _WIN32_WINNT   0x0400

#include <windows.h>

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbfast.h"

void HandleMessage( void );
BOOL ProcessMessage( MSG *pMsg );
BOOL IsMDIMsg( MSG *pMsg );
void Idle( MSG *pMsg );

// Called by users when need to yield.
HB_FUNC( TAPPLICATION_PROCESSMESSAGES )
{
   MSG Msg;

   while( ProcessMessage( &Msg ) );
}

HB_FUNC( TAPPLICATION_MAINLOOP )
{
   BOOL bTerminate;
      
   do
   {
      HandleMessage();
   
      hb_objSendMsg( &APPLICATION, "FTerminate", 0 );
      
      bTerminate = hb_stack.Return.item.asLogical.value;
      
   }while( ! bTerminate );

   hb_stack.Return.type = HB_IT_NIL;
}

void HandleMessage( void )
{
   MSG Msg;
  
   if( ! ProcessMessage( &Msg ) )
   {
      Idle( &Msg );
   }
}

BOOL ProcessMessage( MSG *pMsg )
{
  BOOL Handled = FALSE;

  PHB_ITEM HB_pSelf = hb_stackSelfItem();
  PHB_ITEM HB_pMsg = NULL;
  HB_ITEM HB_Pointer, HB_Handled;
  char sTemp[256];
    
  if( PeekMessage( pMsg, 0, 0, 0, PM_REMOVE ) )
  {
    sprintf( (char *) sTemp, "%i\n", (int) pMsg->message );
    
    if( pMsg->message == WM_QUIT )
    {
       // Usng the Handled item as temp.
       HB_Handled.type = HB_IT_LOGICAL;
       HB_Handled.item.asLogical.value = TRUE;
         
       hb_objSendMsg( &APPLICATION, "_FTerminate", 1, &HB_Handled );
    }
    else
    {
      if( hb_objHasMsg( HB_pSelf, "FOnMessage" ) && hb_objHasMsg( HB_pSelf, "Msg" ) )
      {
         hb_objSendMsg( HB_pSelf, "Msg", 0 );
     
         HB_pMsg->type = HB_IT_NIL;
         hb_itemForwardValue( HB_pMsg, &hb_stack.Return );
  
         HB_Pointer.type = HB_IT_LONG;
         HB_Pointer.item.asLong.value = (LONG) pMsg;
         
         hb_objSendMsg( HB_pMsg, "Pointer", 1, &HB_Pointer );
         hb_stack.Return.type = HB_IT_NIL; // Return is a LONG.
         
         HB_Handled.type = HB_IT_LOGICAL;
         HB_Handled.item.asLogical.value = FALSE;
         
         hb_objSendMsg( HB_pSelf, "FOnMessage", 2, HB_pMsg, &HB_Handled );
         
         Handled = hb_stack.Return.item.asLogical.value;
      }
                 
      if( ! Handled && ! IsDialogMessage( NULL, pMsg ) && ! IsMDIMsg( pMsg )
          /* && ! IsHintMessage( Msg ) && ! IsKeyMessage( Msg ) */ )
      {
        TranslateMessage( pMsg );
        DispatchMessage( pMsg );
      }            
    }
    
    return TRUE;
  } 
    
  return FALSE;  
}

BOOL IsMDIMsg( MSG *pMsg )
{   
  /*
  if( MainForm != NULL ) && ( MainForm.FormStyle == fsMDIForm ) &&
    ( Screen.ActiveForm != NULL ) &&
    ( Screen.ActiveForm.FormStyle == fsMDIChild ) )
  {   
     return TranslateMDISysAccel( MainForm.ClientHandle, pMsg );
  }
  */
  
  return FALSE;
}

void Idle( MSG *pMsg )
{   
   static PHB_DYNS pSymExec = NULL;

   BOOL Done = TRUE;
   
   HB_ITEM HB_Done;
   HB_ITEM HB_OnIdle;

   hb_gcCollectAll();
   
   if( pSymExec == NULL )
   {
      hb_dynsymFind( "HB_EXEC" );
   }
      
   //PHB_ITEM Control;
   
   //Control := DoMouseIdle();
   
   //if( FShowHint && ( FMouseControl == NULL )
   //{
   //   CancelHint();
   //}
   
   //Application.Hint := GetLongHint(GetHint(Control));
         
   HB_Done.type = HB_IT_LOGICAL;
   HB_Done.item.asLogical.value = TRUE;
            
   hb_objSendMsg( &APPLICATION, "FOnIdle", 1, &HB_Done );
      
   hb_itemForwardValue( &HB_OnIdle, &hb_stack.Return );
   
   switch( HB_OnIdle.type )
   {
      case HB_IT_BLOCK:
      {
         hb_vmPushSymbol( &hb_symEval );
         hb_itemPushForward( &HB_OnIdle );                  
         hb_vmSend( 0 );
         break;
      }
      
      case HB_IT_LONG:
      {
         hb_vmPushSymbol( pSymExec );
         hb_vmPushNil();
         hb_itemPushForward( &HB_OnIdle );                  
         hb_vmDo( 0 );
         break;
      }
   }
   
   //if( Done && IsIdleMessage( pMsg ) )
   //{
   //  DoActionIdle();
   //}
   
   if( Done )
   {
      WaitMessage();
   }
}
#pragma ENDDUMP
