#pragma BEGINDUMP
#ifdef __XCC__
  //#pragma comment( lib, "ole.lib" )
  #pragma comment( lib, "oleserver.lib" )
#endif
#pragma ENDDUMP

#include "inkey.ch"

// Elements
#define HASH_ID_ATTACHEDGET       0
#define HASH_ID_ELEMENT_BEHAVIORS 1

// Forms
#define HASH_ID_FORM              0
#define HASH_ID_BROWSER           1
#define HASH_ID_EVENTS            2
#define HASH_ID_FORM_BEHAVIORS    3


GLOBAL __IEForms, __IEBehaviors

INIT PROCEDURE IEGuiInit

   __IEForms     := Hash()
   __IEBehaviors := Hash()

   ErrorBlock( {|e| TraceLog( e:Operation, e:Description, e:ProcName, e:ProcLine ), Break(e) } )

RETURN

FUNCTION IE_Browser( lAddr, lTool, lStatus, lTheater )

   LOCAL oBrowser := CreateObject( "InternetExplorer.Application" )

   oBrowser:Navigate( "about:blank" )

   oBrowser:AddressBar := lAddr
   oBrowser:Toolbar := lTool
   oBrowser:Statusbar := lStatus
   oBrowser:TheaterMode := lTheater

RETURN oBrowser

FUNCTION IE_Form( cFormID, oBrowser, cBody, cUrl, cTitle, hEvents, ahBehaviors )

   STATIC cExe
   LOCAL lDel := .F.
   LOCAL oDocument := oBrowser:Document
   
   IF cExe == NIL
      cExe := HB_ArgV(0)

      IF ! '\' IN cExe
         cExe := CurDrive() + ":\" + CurDir() + "\" + cExe
      ENDIF
   ENDIF

   IF ! File( cExe + ".htm" )
      lDel := .T.
      MemoWrit( cExe + ".htm", "" )
   ENDIF
   oBrowser:Navigate( cExe + ".htm" )

   WHILE oDocument:Body == NIL
      DoEvents()
   ENDDO

   IF Empty( cUrl )
      oDocument:Body:innerHTML := cBody
   ELSE
      oDocument:Open( cUrl )
   ENDIF

   IF lDel
      Erase( cExe + ".htm" )
   ENDIF

   IF ! Empty( cTitle )
      oDocument:Title := cTitle
   ENDIF

   HDelAt( hEvents, 1 )
   HSetCaseMatch( hEvents, .F. )

   __IEForms[ cFormID ] := Hash()

   __IEForms[ cFormID ][ HASH_ID_FORM ]           := WrapTOleAuto( oDocument:Forms[ cFormID ], "HTMLFormElement" )
   __IEForms[ cFormID ][ HASH_ID_BROWSER ]        := oBrowser
   __IEForms[ cFormID ][ HASH_ID_EVENTS ]         := hEvents
   __IEForms[ cFormID ][ HASH_ID_FORM_BEHAVIORS ] := ahBehaviors

RETURN __IEForms[ cFormID ][ HASH_ID_FORM ]

FUNCTION IE_DefineElement( oForm, cID, xBefore, nTop, nLeft, nSize, cElement, hEvents, ahBehaviors )

   LOCAL oElement, oBefore, cAbsolute

   IF ValType( nTop ) == 'N' .OR. ValType( nLeft ) == 'N'
      cAbsolute := 'style="position:absolute; '

      IF ValType( nTop ) == 'N'
         cAbsolute += "top:" + LTrim( Str( nTop ) ) + 'px; '
      ENDIF
      IF ValType( nLeft ) == 'N'
         cAbsolute += "left:" + LTrim( Str( nLeft ) ) + 'px;"'
      ENDIF

      cElement += cAbsolute
   ENDIF

   IF ValType( nSize ) == 'N'
      cElement += "SIZE=" + LTrim( Str( nSize ) ) + ' '
   ENDIF
   cElement += '>'

   //TraceLog( cElement )

   oElement := oForm:Document:createElement( cElement )

   IF ValType( xBefore ) == 'C'
      oBefore := oForm:Elements[ xBefore ]
      oBefore:parentNode:insertBefore( oElement, oBefore )
   ELSEIF ValType( xBefore ) == 'O'
      oBefore := xBefore
      oBefore:parentNode:insertBefore( oElement, oBefore )
   ELSE
      oForm:appendChild( oElement )
   ENDIF

   HDelAt( hEvents, 1 )
   HSetCaseMatch( hEvents, .F. )

   hEvents[ HASH_ID_ATTACHEDGET ]       := NIL
   hEvents[ HASH_ID_ELEMENT_BEHAVIORS ] := ahBehaviors

   __IEForms[ oForm:id ][ cID ] := hEvents

RETURN oElement

FUNCTION IE_DefineGet( oForm, cID, xBefore, nTop, nLeft, nSize, GetList, oGet, hEvents, ahBehaviors )

   LOCAL oElement, cElement

   cElement := "<INPUT TYPE='TEXT' ID='" + cID + "' "

   IF Empty( nSize )
      nSize := Len( oGet:VarGet() )
   ENDIF

   oElement := IE_DefineElement( oForm, cID, xBefore, nTop, nLeft, nSize, cElement, hEvents )

   AAdd( GetList, oGet )

   hEvents[ HASH_ID_ATTACHEDGET ]       := oGet
   hEvents[ HASH_ID_ELEMENT_BEHAVIORS ] := ahBehaviors

   __IEForms[ oForm:id ][ cID ] := hEvents

RETURN oElement

FUNCTION IE_DefineButton( oForm, cID, xBefore, nTop, nLeft, nSize, hEvents, ahBehaviors )

   LOCAL cElement := "<INPUT TYPE='BUTTON' ID='" + cID + "' "
   LOCAL oButton := IE_DefineElement( oForm, cID, xBefore, nTop, nLeft, nSize, cElement, hEvents )

   HDelAt( hEvents, 1 )
   HSetCaseMatch( hEvents, .F. )

   hEvents[ HASH_ID_ATTACHEDGET ]       := NIL
   hEvents[ HASH_ID_ELEMENT_BEHAVIORS ] := ahBehaviors

   __IEForms[ oForm:id ][cID] := hEvents

RETURN oButton

FUNCTION IE_RedefineElement( oForm, cID, hEvents, ahBehaviors )

   LOCAL oElement := oForm[ cID ]

   HDelAt( hEvents, 1 )
   HSetCaseMatch( hEvents, .F. )

   hEvents[ HASH_ID_ATTACHEDGET ]       := NIL
   hEvents[ HASH_ID_ELEMENT_BEHAVIORS ] := ahBehaviors

   __IEForms[ oForm:id ][ cID ] := hEvents

RETURN oElement

FUNCTION IE_RedefineGet( oForm, cID, GetList, oGet, hEvents, ahBehaviors )

   LOCAL oElement := oForm[ cID ]

   AAdd( GetList, oGet )

   HDelAt( hEvents, 1 )
   HSetCaseMatch( hEvents, .F. )

   hEvents[ HASH_ID_ATTACHEDGET ]       := oGet
   hEvents[ HASH_ID_ELEMENT_BEHAVIORS ] := ahBehaviors

   __IEForms[ oForm:id ][ cID ] := hEvents

RETURN oElement

FUNCTION IE_DefineBehavior( cBehavior, hEvents )

   HDelAt( hEvents, 1 )
   HSetCaseMatch( hEvents, .F. )

RETURN __IEBehaviors[ cBehavior ] := hEvents

STATIC FUNCTION __HtmlFormElement_GenericHandler( pEvtObj )

   LOCAL Self, oDocument
   LOCAL pSource
   LOCAL cEvent
   LOCAL oGet, lWhen, lValid
   LOCAL cBehavior
   LOCAL oGetList
   LOCAL oErr
   LOCAL nKey
   LOCAL oWindow
   LOCAL cChar

   TRY
      Self      := HB_QSelf()
      oDocument := ::document

      IF pEvtObj != NIL
         pSource := oDocument:getElementById( pEvtObj:srcElement:id )
      ENDIF

      cEvent    := __GetMessage()
      oGetList  := __GetListActive()
      oWindow   := ::document:parentWindow

      //Tracelog( cEvent, ::id, pEvtObj, pSource )
   CATCH oErr
      TraceLog( oErr:Operation, oErr:Description, oErr:ProcName, oErr:ProcLine, cEvent )
      RETURN NIL
   END

   //TraceLog( cEvent, pSource, ::id )
   
   TRY
      Eval( __IEForms[ ::id ][ pSource:id ][ cEvent ], pSource )
   CATCH
   END

   FOR EACH cBehavior IN __IEForms[ ::id ][ HASH_ID_FORM_BEHAVIORS ]
      TRY
         //TraceLog( cBehavior, cEvent )
         Eval( __IEBehaviors[ cBehavior ][ cEvent ], pSource )
      CATCH
      END
   NEXT

   FOR EACH cBehavior IN __IEForms[ ::id ][ pSource:id ][ HASH_ID_ELEMENT_BEHAVIORS ]
      TRY
         Eval( __IEBehaviors[ cBehavior ][ cEvent ], pSource )
      CATCH
      END
   NEXT

   //TRY
      DO CASE
         CASE cEvent == "onkeypress"
            TRY
               oGet := __IEForms[ ::id ][ pSource:id ][ HASH_ID_ATTACHEDGET ]
            CATCH
            END

            IF ! oGet == NIL
               nKey := pEvtObj:keyCode

               //TraceLog( oGet:name, nKey, oGet:buffer )

               oGetList:GetApplyKey( nKey )
               pSource:value := oGet:buffer
               IE_SetCaretPos( pSource, oGet:pos )

               RETURN .F.
            ENDIF

         CASE cEvent == "onkeydown"
            IF pEvtObj:keyCode == 46
               TRY
                  oGet := __IEForms[ ::id ][ pSource:id ][ HASH_ID_ATTACHEDGET ]
               CATCH
               END

               IF ! oGet == NIL
                  IF oDocument:selection:type == "Text"
                     oGet:pos := IE_GetSelectionStart( pSource )
                     oGet:right()
                     oGet:left()

                     FOR EACH cChar IN oDocument:selection:createRange:text
                        oGetList:GetApplyKey( K_DEL )
                     NEXT
                  ELSE
                     oGetList:GetApplyKey( K_DEL )
                  ENDIF

                  pSource:value := oGet:buffer
                  IE_SetCaretPos( pSource, oGet:pos )
                  RETURN .F.
               ENDIF
            ENDIF

         CASE cEvent == "onkeyup"
            TRY
               oGet := __IEForms[ ::id ][ pSource:id ][ HASH_ID_ATTACHEDGET ]
            CATCH
            END

            IF ! oGet == NIL
               nKey := pEvtObj:keyCode

               SWITCH nKey
                  CASE 35//K_END
                     nKey := K_END
                     EXIT

                  CASE 36//K_HOME
                     nKey := K_HOME
                     EXIT

                  CASE 37//K_LEFT
                     nKey := K_LEFT
                     EXIT

                  CASE 39//K_RIGHT
                     nKey := K_RIGHT
                     EXIT

                  CASE 45//K_INS
                     nKey := K_INS
                     EXIT

                  CASE K_BS
                     EXIT

                  DEFAULT
                     TraceLog( oGet:name, nKey, oGet:buffer )
                     nKey := 0
               END

               IF nKey > 0
                  oGetList:GetApplyKey( nKey )
                  pSource:value := oGet:buffer
                  IE_SetCaretPos( pSource, oGet:pos )
               ENDIF

               RETURN .F.
            ENDIF

         CASE cEvent == "onpaste"
            TRY
               oGet := __IEForms[ ::id ][ pSource:id ][ HASH_ID_ATTACHEDGET ]
            CATCH
            END

            IF ! oGet == NIL
               FOR EACH cChar IN oWindow:clipboardData:getData( "Text" )
                  oGetList:GetApplyKey( Asc( cChar ) )
               NEXT

               pSource:value := oGet:buffer
               IE_SetCaretPos( pSource, oGet:pos )
               RETURN .F.
            ENDIF

         CASE cEvent == "oncut"
            TRY
               oGet := __IEForms[ ::id ][ pSource:id ][ HASH_ID_ATTACHEDGET ]
            CATCH
            END

            IF ! oGet == NIL
               IF oDocument:selection:type == "Text"
                  //TraceLog( oDocument:selection:createRange:text )
                  //TraceLog( IE_GetSelectionStart( pSource ) )
                  //TraceLog( IE_GetSelectionEnd( pSource ) )

                  oGet:pos := IE_GetSelectionStart( pSource )
                  oGet:right()
                  oGet:left()

                  //TraceLog( oDocument:selection:createRange:text )

                  FOR EACH cChar IN oDocument:selection:createRange:text
                     //TraceLog( HB_EnumIndex(), cChar )
                     oGetList:GetApplyKey( K_DEL )
                  NEXT

                  pSource:value := oGet:buffer
               ENDIF

               RETURN .F.
            ENDIF

         CASE cEvent == "onbeforeactivate"
            TRY
               oGet := __IEForms[ ::id ][ pSource:id ][ HASH_ID_ATTACHEDGET ]
            CATCH
            END
            //TraceLog( oGet )

            IF oGet == NIL
               oGetList:nPos := 0
               oGetList:oGet := NIL
               GetActive( NIL )
            ELSE
               oGetList:nPos := aScan( oGetList:aGetList, {|_1| _1 == oGet } )
               oGetList:oGet := oGet
               oGetList:PostActiveGet()

               IF oGet:PreBlock != NIL
                 lWhen := Eval( oGet:preblock, oGet )
               ELSE
                 lWhen := .T.
               ENDIF

               IF lWhen
                  oGet:setFocus()
                  pSource:value := oGet:buffer
               ELSE
                  pSource:value := oGet:VarGet()
               ENDIF

               //pEvtObj:returnValue := lWhen
               RETURN lWhen
            ENDIF

         CASE cEvent == "onactivate"
            TRY
               oGet := __IEForms[ ::id ][ pSource:id ][ HASH_ID_ATTACHEDGET ]
            CATCH
            END
            //TraceLog( oGet )

            IF oGet != NIL
               IE_SetCaretPos( pSource, oGet:pos )
            ENDIF

         CASE cEvent == "onbeforedeactivate"
            TRY
               oGet := __IEForms[ ::id ][ pSource:id ][ HASH_ID_ATTACHEDGET ]
            CATCH
            END
            //TraceLog( oGet )

            IF oGet != NIL
               //oGet:VarPut( pSource:value )
               //IF oGet:Changed
                  oGet:Assign()
                  oGetList:lUpdated := .t.
               //ENDIF

               IF oGet:postblock != NIL
                  lValid := Eval( oGet:postblock, oGet )
               ELSE
                  lValid := .T.
               ENDIF

               IF lValid
                  pSource:value := oGet:VarGet()
               ELSE
                  pSource:value := oGet:buffer
               ENDIF

               //pEvtObj:returnValue := lValid
               RETURN lValid
            ENDIF
      ENDCASE
   //CATCH oErr
   //    TraceLog( oErr:Operation, oErr:Description, oErr:ProcName, oErr:ProcLine )
   //END

RETURN NIL

/*
PROCEDURE IE_Activate( oBrowser )

   oBrowser:Visible := .T.

RETURN
*/

FUNCTION IE_ModalForm( xForm, oBrowser, GetList )

   LOCAL oForm
   LOCAL oGetList, oPresetGetList
   LOCAL oErr

   IF ValType( xForm ) == 'C'
      oForm := WrapTOleAuto( oBrowser:Document:Forms[ xForm ], "HTMLFormElement" )
   ELSE
      oForm := xForm
   ENDIF

   // GetListSupport
   //oForm:EventHandler[ "onactivate" ]   := ( @__HtmlFormElement_OnActivate() )
   //oForm:EventHandler[ "ondeactivate" ] := ( @__HtmlFormElement_OnDeactivate() )

   oForm:EventHandler[ "*" ] := ( @__HtmlFormElement_GenericHandler() )

   oForm:ConnectEvents( , { InterfaceByName( oForm:TypeLib, "HTMLFormElementEvents2" ) } )
   oForm:SetActive()

   oGetList := HBGetList():New( GetList )

   oGetList:cReadProcName := ProcName( 1 )
   oGetList:nReadProcLine := ProcLine( 1 )

   oPresetGetList := __GetListActive( )

   __GetListSetActive( oGetList )
   __GetListLast( oGetList )

   IF Empty( oBrowser )
      oBrowser := __IEForms[ oForm:id ][ HASH_ID_BROWSER ]
   ENDIF

   TRY
      oBrowser:Visible := .T.
      oForm:Item(0):focus()

      WHILE oBrowser:Visible // Seems to support events in itself!
         DoEvents()
      END

      IE_CloseForm( oForm )
   CATCH oErr
      TraceLog( oErr:Operation, oErr:Description, oErr:ProcName, oErr:ProcLine )
   END

   GetList := {}

   __GetListSetActive( oPresetGetList )

RETURN oForm

PROCEDURE IE_CloseForm( oForm )

   LOCAL oErr, oStoredForm

   //TraceLog( oForm )
   
   #if 1
   TRY
      IF ! Empty( oForm:ConnectedInterfaces )
         oForm:DisconnectEvents()
      ENDIF
   CATCH oErr
      TraceLog( oErr:Operation, oErr:Description, oErr:ProcName, oErr:ProcLine )
   END
   #endif

   //TRY
   //   HDel( __IEForms, oForm:id )
   //   TraceLog( oForm:id )
   //CATCH oErr
      //TraceLog( oErr:Operation, oErr:Description, oErr:ProcName, oErr:ProcLine )
      FOR EACH oStoredForm IN __IEForms:Values
         IF HB_ArrayID( oStoredForm[ HASH_ID_FORM ] ) == HB_ArrayID( oForm )
            TraceLog( __IEForms:Keys[ HB_EnumIndex() ] )
            HDel( __IEForms, __IEForms:Keys[ HB_EnumIndex() ] )
            EXIT
         ENDIF
      NEXT
   //END

   oForm := NIL

RETURN

PROCEDURE IE_CloseBrowser( oBrowser )

   __IEForms     := NIL
   __IEBehaviors := NIL

   oBrowser := NIL

RETURN

PROCEDURE IE_SetCaretPos( oElement, nPos )

   LOCAL oRange, oErr

   //oElement:Focus()

   TRY
      oRange := oElement:createTextRange()

      oRange:moveStart( "character", Max( 0, nPos - 1 ) )
      oRange:collapse()
      oRange:select()
   CATCH oErr
      TraceLog( oErr:Operation, oErr:Description, oErr:ProcName, oErr:ProcLine, ValToPrg( oErr:aaStack ) )
   END

RETURN

FUNCTION IE_GetSelectionStart( oElement )

   LOCAL oDocument := oElement:document
   LOCAL oRange, oRange2

   IF oDocument:selection:type == "Text"
      oRange := oDocument:selection:createRange()

      oRange2 := oElement:createTextRange()
      oRange2:setEndPoint( "EndToStart", oRange )

      //TraceLog( oRange:text, oRange2:text )

      RETURN Len( oRange2:text ) + 1
   ENDIF

RETURN 0

FUNCTION IE_GetSelectionEnd( oElement )

   LOCAL oDocument := oElement:document
   LOCAL oRange, oRange2

   IF oDocument:selection:type == "Text"
      oRange := oDocument:selection:createRange()

      oRange2 := oElement:createTextRange()
      oRange2:setEndPoint( "EndToEnd", oRange )

      //TraceLog( oRange:text, oRange2:text )

      RETURN Len( oRange2:text )
   ENDIF

RETURN 0

/*
// Get the start position of the caret in the object
function getCaretStart(obj){
  if(typeof obj.selectionStart != "undefined"){
    return obj.selectionStart;
  }else if(document.selection&&document.selection.createRange){
    var M=document.selection.createRange();
    try{
      var Lp = M.duplicate();
      Lp.moveToElementText(obj);
    }catch(e){
      var Lp=obj.createTextRange();
    }
    Lp.setEndPoint("EndToStart",M);
    var rb=Lp.text.length;
    if(rb>obj.value.length){
      return -1;
    }
    return rb;
  }
}
*/

/*
// sets the caret selection from s to e in the object
function setSelection(obj,s,e){
  obj.focus();
  if (obj.setSelectionRange){
    obj.setSelectionRange(s,e);
  }else if(obj.createTextRange){
    m = obj.createTextRange();
    m.moveStart('character',s);
    m.moveEnd('character',e);
    m.select();
  }
}
*/

// Research
#if 0

var myObject = document.createElement('object');
DivID.appendChild(myObject);
myObject.width = "200";
myObject.height = "100";
myObject.classid= "clsid:6BF52A52-394A-11d3-B153-00C04F79FAA6";
myObject.URL = "example.wmv";
myObject.uiMode = "none" ;

#endif