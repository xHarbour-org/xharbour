/*
 * $Id$
 */
#ifdef WIN
   #include "winapi.ch"
#endif

PROCEDURE Main( cFile )

   LOCAL oEditor

   IF Empty( cFile )

   ELSEIF ! File( cFile )
      IF cFile HAS "\?|\*"
         IF Len( Directory( cFile ) ) == 0
            #ifdef WIN
               MessageBox( 0, "No file matches: " + cFile, MB_OK | MB_ICONINFORMATION )
            #else
               Alert( "No file matches: " + cFile )
            #endif

            cFile := NIL
         ENDIF
      ELSE
        #ifdef WIN
          IF MessageBox( 0, "File does not exist: " + cFile + CRLF + CRLF + "Create?", "xEdit", MB_YESNO )== IDYES
        #else
          IF Alert( "File does not exist: " + cFile + ";;Create?", { "Yes", "No" } ) == 1
        #endif
             IF ! MemoWrit( cFile, "" )
                Throw( ErrorNew( "xEdit", 0, 1001, "Create error: " + cFile, "I/O Error (" + Str( FError(), 2 ) + ")", HB_aParams() ) )
              ENDIF
          ELSE
             RETURN
          ENDIF
      ENDIF
   ENDIF

   #ifndef WIN
      SAVE SCREEN
   #endif

   #ifdef OLE
      oServer := CreateObject( "xEditOleServer" )
      oEditor := oServer:Editor()
      oEditor:New( , , , , cFile )

      //oServer:PopupEditor( hParent, 10, 10, 580, 380, cFile )
   #else
      oEditor := Editor()
      oEditor:New( , , , , cFile )
   #endif

   oEditor:Edit()

   #ifndef WIN
      RESTORE SCREEN
   #endif

RETURN
