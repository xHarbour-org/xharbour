/*
 * $Id$
 */
//3456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A
/*
  (c) copyright xHarbour.com Inc. http://www.xHarbour.com
  Author: Ron Pinkas Ron@xHarbour.com

  This source file is an intellectual property of xHarbour.com Inc.
  You may NOT forward or share this file under any conditions!
*/

//#define HB_GCAll()

#ifdef __DEBUG__
   #xcommand ASSERT( <x> ) => IIF( <x>, , Alert( "Assertion failed!;" + #<x> + ";" + ProcName() + "(" + Str( ProcLine(), 5 ) + ")" ) )
#else
   #xcommand ASSERT( <x> ) =>
#endif

#ifdef VXH
   #xtranslate Application => __GetApplication()
#endif


#ifdef OLESERVER
    #define DLL

    #pragma BEGINDUMP
       #define CLS_Name "xEditOleServer"
       #define CLS_ID   "{023CE211-F98A-428f-A8D2-A5FEE342DCF2}"

       #include "OleServer.h"
    #pragma ENDDUMP
#endif

STATIC s_FakeLine

STATIC s_PRG_KeyWord, s_PRG_Word, s_PRG_Operator, s_PRG_Literal
STATIC s_ErrorLine, s_WarningLine, s_TraceLog, s_MissingExternal
STATIC s_Func, s_Method, s_Method2, s_Class, s_ClassVar, s_InlineComment
STATIC s_SysFunc
STATIC s_Colors, s_CustomColors, s_PrgColors, s_LogColors, s_DefaultColors
STATIC s_nShow, s_nLeft, s_nTop, s_nBottom, s_nRight, s_cFont, s_nFontSize, s_nFontWeight, s_nCharSet, s_lAutoSave, s_lFocusLine, s_nTabSpaces
STATIC s_lForceEOL, s_cWorkspaceFilename := "", s_lESCClosesWindow
STATIC s_lResetOnSave
STATIC s_aEditors := {}, s_aPopups := {}

#if defined( XEDIT_DYN_ACCELRATOR )
   STATIC s_nKeyAccelerator
#else
   #define s_nKeyAccelerator 3
#endif

STATIC s_oInterpreter

STATIC s_aFind := {}, s_aReplace := {}
STATIC s_aFiles := {}, s_aWorkspaces := {}

#ifdef WIN
   #command SAVE SCREEN =>
   #command RESTORE SCREEN =>

   //#translate QOut( <x,...> ) => TraceLog( <x> )
   //#translate QQOut( <x,...> ) => TraceLog( <x> )

   #command @ <row>, <col> SAY <s> => Error TextOut( hDC, LEFT_PAD + (<col>) * ::TextMetric:tmAveCharWidth, (<row>) * ::TextMetric:tmHeight, <s>, Len( <s> ) )

   #xtranslate SetPos( <row>, <col> ) => ( /*TraceLog( <row>, <col> ),*/ SetCaretPos( LEFT_PAD + (<col>) * ::TextMetric:tmAveCharWidth, (<row>) * ::TextMetric:tmHeight ) )

   STATIC s_hFont
   STATIC s_wc
   STATIC s_hInstance
   STATIC s_hActiveDialog
   STATIC s_DesktopRect
   STATIC s_hDll, s_hMenu
   STATIC s_hPlus, s_hMinus, s_hBookmark/*, s_hModified */
   STATIC s_hFindFont

   //STATIC s_hShrink

   //STATIC s_hOpen, s_hNew, s_hSave, s_hClose, s_hPrint, s_hExit, s_hCopy, s_hCut, s_hPaste, s_hSelect, s_hSelectAll

    STATIC s_oxHDN

    //PROCEDURE HB_GT_WIN()
    //RETURN
#else
   // TODO
   FUNCTION GetSaveFileName()
   RETURN .F.
#endif

#include "hbclass.ch"
#include "xedit.ch"
#include "debug.ch"

#ifdef OLESERVER

    #define DLL_PROCESS_ATTACH  1
    #define DLL_PROCESS_DETACH  0

    PROCEDURE DllMain( hInstance, nReason )

       SWITCH nReason
          CASE DLL_PROCESS_ATTACH
             TraceLog( "xEdit OlseServer Dll Loaded." )
             s_hInstance := hInstance
             InitEditor()
             EXIT

          CASE DLL_PROCESS_DETACH
             TraceLog( "xEdit OleServer Dll UNloaded." )
             EXIT

          DEFAULT
             TraceLog( "UNEXPECTED nReason in DllMain!" )
       END

    RETURN

    //FUNCTION CreateInstance

    //RETURN Editor()
#endif

#ifdef OLESERVER
     PROCEDURE InitEditor()
#else
INIT PROCEDURE InitEditor()
#endif

   LOCAL cPath, nAt, aIni, aEntries, sSection, cExt, cKey, Color, aRGB, oError, ExtensionColors, aExtensions
   LOCAL aScripts := {}, aInitializers := {}, aColorExtensions := {}

   #ifdef SCRIPTS
      LOCAL sScriptFile, sProcedure, xRet
   #endif

   s_PRG_KeyWord  := HB_RegExComp( "(?i)^(IF|ELSEIF|ELSE|ENDIF|DO +WHILE|DO +CASE|DO|WHILE|LOOP|EXIT +(FUNCTION|PROCEDURE)|EXIT|ENDCASE|ENDDO|CASE|OTHERWISE|SWITCH|DEFAILT|ENDCLASS|FOR +EACH|FOR|NEXT|BEGIN +SEQUENCE|BREAK|RECOVER( +USING)?|ENDSEQUENCE|END +SEQUENCE|LOCAL|STATIC +(FUNCTION|PROCEDURE)|STATIC|METHOD|PRIVATE|PUBLIC|MEMVAR|DECALRE|FIELD|GLOBAL +EXTERNAL|GLOBAL|EXTERNAL|EXTERN|ANNOUNCE|REQUEST|PROCEDURE|FUNCTION|TRY|CATCH|INIT +(FUNCTION|PROCEDURE)|EXIT|RETURN|SAVE|RESTORE|CLASS|VAR|DATA|METHOD|ACCESS|ASSIGN|PROTECTED|READONLY|EXPORTED|PUBLISHED|HIDDEN|END|READ|WITH +OBJECT|CLEAR +SCREEN|DEFAULT)( +|$)" )
   s_PRG_Word     := HB_RegExComp( "(?i)(IIF|WHILE|IN|STEP|TO|EXTERNAL|PROTECTED|READONLY|EXPORTED|PUBLISHED|INLINE|PRIVATE|HIDDEN|VIRTUAL|GET|SAY|GET|CLEAR|\.T\.|\.F\.) *" )
   s_PRG_Literal  := HB_RegExComp( E"(\".*\"|'.*'|E\".*\") *" )
   s_PRG_Operator := HB_RegExComp( "(?i)(\+\+|--|\*\*|:=|<>|>=|<=|==|!=|\+=|-=|\*=|/=|HAS|LIKE|\.AND\.|\.OR\.|\.NOT\.|[!@#$^&*(){}[\]|/,><-=]|\+) *" )

   s_Func         := HB_RegExComp( "(?i)^ *(INIT|EXIT|STATIC)? *(FUNCTION|PROCEDURE) +([_a-z|0-9]+)" )
   s_Method       := HB_RegExComp( "(?i)^ *METHOD +([_a-z|0-9]+) *(\([^\)]*\))? *CLASS +([_a-z|0-9]+)" )
   s_Method2      := HB_RegExComp( "(?i)^ *METHOD +([_a-z|0-9]+) *: *([_a-z|0-9]+) *(\([^\)]*\))?" )
   s_Class        := HB_RegExComp( "(?i)^ *CLASS +([_a-z|0-9]+)" )
   s_ClassVar     := HB_RegExComp( "(?i)^ *CLASS *(VAR|DATA)" )

   s_InlineComment := HB_RegExComp( "(^\*)|//|&&" )

   #ifdef WIN
      InitCommonControls()

      #if defined( SCRIPTS ) || defined( VXH )
         s_SysFunc := WinFunctions()
      #else
         s_SysFunc := ""
      #endif
   #else
      s_SysFunc := ConFunctions()
   #endif

   #ifdef SCRIPTS
      s_oInterpreter := TInterpreter()
   #endif

   //t1.prg(7) Error E0018  LOOP statement with no loop in sight
   //t1.prg(16): error: Syntax error; found 'error' expecting ';'.
   s_ErrorLine := HB_RegExComp( "(?i)(?:[0-9]+00\r+)*(.+)\(([0-9]+)?\):? *(error:|Error [EF][0-9]+) (.+)" )

   //xLINK: error: Unresolved external symbol '_HB_FUN_MISSING'.
   s_MissingExternal := HB_RegExComp( "(?i)(xLink)(:) *(error:|Error [EF][0-9]+) (.+)" )

   //hbsetup.ch(84) Warning I0001  Redefinition or duplicate definition of #define SOME_DEF
   //\xhb\include\gfx.ch(106): warning: Unknown preprocessor control: 'translate'.
   s_WarningLine := HB_RegExComp( "(?i)(?:[0-9]+00\r+)*(.+)\(([0-9]+)\):? *(warning:|Warning [A-Z][0-9]+) (.+)" )

   //Type: C >>>couldn't build: t1.obj<<<
   s_TraceLog := HB_RegExComp( "Type: . >>>(.*)(<<<)?" )

   s_Colors := Hash()
   HSetCaseMatch( s_Colors, .F. )

   s_DefaultColors := Hash()
   HSetCaseMatch( s_DefaultColors, .F. )
   s_DefaultColors[ "Name" ] := "*.*"
   s_DefaultColors[ "hExtensionTree" ] := NIL
   s_DefaultColors[ "Colorizer" ] := NIL
   s_DefaultColors[ "AfterKey" ]  := NIL

   s_PrgColors := Hash()
   HSetCaseMatch( s_PrgColors, .F. )
   s_PrgColors[ "Name" ] := "*.prg; *.xbs; *.ch; *.xfm"
   s_PrgColors[ "hExtensionTree" ] := NIL

   s_LogColors := Hash()
   HSetCaseMatch( s_LogColors, .F. )
   s_LogColors[ "Name" ] := "*.log"
   s_LogColors[ "hExtensionTree" ] := NIL

   s_CustomColors := Hash()
   HSetCaseMatch( s_CustomColors, .F. )

   s_CustomColors[ "prg" ] := s_PrgColors
   s_CustomColors[ "xbs" ] := s_PrgColors
   s_CustomColors[ "ch"  ] := s_PrgColors
   s_CustomColors[ "xfm" ] := s_PrgColors

   s_CustomColors[ "log" ] := s_LogColors

   s_PrgColors[ "Colorizer" ] := HB_FuncPtr( "TextOutColorizePRGTokens" )
   s_PrgColors[ "AfterKey" ]  := HB_FuncPtr( "PRGAfterKey" )

   s_LogColors[ "Colorizer" ] := HB_FuncPtr( "TextOutColorizeLOGTokens" )

   #ifdef WIN
      s_Colors[ "Red"        ] := RGB( 255, 000, 000 )
      s_Colors[ "Green"      ] := RGB( 000, 255, 000 )
      s_Colors[ "Blue"       ] := RGB( 000, 000, 255 )
      s_Colors[ "White"      ] := RGB( 255, 255, 255 )
      s_Colors[ "Black"      ] := RGB( 000, 000, 000 )
      s_Colors[ "Gray"       ] := RGB( 128, 128, 128 )
      s_Colors[ "Magenta"    ] := RGB( 255, 000, 255 )
      s_Colors[ "Cyan"       ] := RGB( 000, 255, 255 )
      s_Colors[ "Orange"     ] := RGB( 255, 128, 000 )
      s_Colors[ "Yellow"     ] := RGB( 255, 255, 000 )
      s_Colors[ "Dark Red"   ] := RGB( 128, 000, 000 )
      s_Colors[ "Dark Green" ] := RGB( 000, 128, 000 )
      s_Colors[ "Dark Blue"  ] := RGB( 000, 000, 128 )

      s_DefaultColors[ "Background" ] := s_Colors[ "White"  ]
      s_DefaultColors[ "Text"       ] := s_Colors[ "Black"  ]
      s_DefaultColors[ "Highlight"  ] := s_Colors[ "Yellow" ]

      s_PrgColors[ "Background"    ] := s_Colors[ "White"      ]
      s_PrgColors[ "Text"          ] := s_Colors[ "Black"      ]
      s_PrgColors[ "Comments"      ] := s_Colors[ "Dark Green" ]
      s_PrgColors[ "Directives"    ] := s_Colors[ "Orange"     ]
      s_PrgColors[ "C Code"        ] := s_Colors[ "Gray"       ]
      s_PrgColors[ "Keywords"      ] := s_Colors[ "Blue"       ]
      s_PrgColors[ "Reserved"      ] := s_Colors[ "Dark Blue"  ]
      s_PrgColors[ "Operators"     ] := s_Colors[ "Magenta"    ]
      s_PrgColors[ "Literals"      ] := s_Colors[ "Red"        ]
      s_PrgColors[ "Open Literals" ] := s_Colors[ "Dark Red"   ]
      s_PrgColors[ "API"           ] := s_Colors[ "Cyan"       ]
      s_PrgColors[ "Highlight"     ] := s_Colors[ "Yellow"     ]

      s_LogColors[ "Background"  ] := s_Colors[ "White"  ]
      s_LogColors[ "Text"        ] := s_Colors[ "Black"  ]
      s_LogColors[ "Couldn't"    ] := s_Colors[ "Red"    ]
      s_LogColors[ "Module"      ] := s_Colors[ "Blue"   ]
      s_LogColors[ "Line"        ] := s_Colors[ "Red"    ]
      s_LogColors[ "Error"       ] := s_Colors[ "Gray"   ]
      s_LogColors[ "Description" ] := s_Colors[ "Orange" ]
      s_LogColors[ "Highlight"   ] := s_Colors[ "Yellow" ]
   #else
      s_Colors[ "Red"        ] := "R+"
      s_Colors[ "Green"      ] := "G+"
      s_Colors[ "Blue"       ] := "B+"
      s_Colors[ "White"      ] := "W"
      s_Colors[ "Black"      ] := "N"
      s_Colors[ "Gray"       ] := "N+"
      s_Colors[ "Magenta"    ] := "RB"
      s_Colors[ "Cyan"       ] := "BG"
      s_Colors[ "Orange"     ] := "GR"
      s_Colors[ "Yellow"     ] := "GR+"
      s_Colors[ "Dark Red"   ] := "R"
      s_Colors[ "Dark Green" ] := "G"
      s_Colors[ "Dark Blue"  ] := "B"

      s_DefaultColors[ "Background" ] := s_Colors[ "White"   ]
      s_DefaultColors[ "Text"       ] := s_Colors[ "Black"   ] + "/" + s_DefaultColors[ "Background" ]
      s_DefaultColors[ "Highlight"  ] := s_Colors[ "Yellow"   ]

      s_PrgColors[ "Background" ] := s_Colors[ "White"   ]
      s_PrgColors[ "Highlight" ] := s_Colors[ "Yellow"  ]

      s_PrgColors[ "Text"          ] := s_Colors[ "Black"   ] + "/" + s_PrgColors[ "Background" ]
      s_PrgColors[ "Comments"      ] := s_Colors[ "Green"   ] + "/" + s_PrgColors[ "Background" ]
      s_PrgColors[ "Directives"    ] := s_Colors[ "Orange"  ] + "/" + s_PrgColors[ "Background" ]
      s_PrgColors[ "C Code"        ] := s_Colors[ "Gray"    ] + "/" + s_PrgColors[ "Background" ]
      s_PrgColors[ "Keywords"      ] := s_Colors[ "Blue"    ] + "/" + s_PrgColors[ "Background" ]
      s_PrgColors[ "Reserved"      ] := s_Colors[ "Blue"    ] + "/" + s_PrgColors[ "Background" ]
      s_PrgColors[ "Operators"     ] := s_Colors[ "Magenta" ] + "/" + s_PrgColors[ "Background" ]
      s_PrgColors[ "Literals"      ] := s_Colors[ "Red"     ] + "/" + s_PrgColors[ "Background" ]
      s_PrgColors[ "Open Literals" ] := s_Colors[ "Yellow"  ] + "/" + s_PrgColors[ "Background" ]
      s_PrgColors[ "API"           ] := s_Colors[ "Cyan"    ] + "/" + s_PrgColors[ "Background" ]

      s_LogColors[ "Background"  ] := s_Colors[ "White"  ]
      s_LogColors[ "Highlight"   ] := s_Colors[ "Yellow"  ]

      s_LogColors[ "Text"        ] := s_Colors[ "Black"  ] + "/" + s_LogColors[ "Background" ]
      s_LogColors[ "Couldn't"    ] := s_Colors[ "Red"    ] + "/" + s_LogColors[ "Background" ]
      s_LogColors[ "Module"      ] := s_Colors[ "Blue"   ] + "/" + s_LogColors[ "Background" ]
      s_LogColors[ "Line"        ] := s_Colors[ "Red"    ] + "/" + s_LogColors[ "Background" ]
      s_LogColors[ "Error"       ] := s_Colors[ "Gray"   ] + "/" + s_LogColors[ "Background" ]
      s_LogColors[ "Description" ] := s_Colors[ "Orange" ] + "/" + s_LogColors[ "Background" ]
   #endif

   // TODO!!! Review - HB_REadIni() should be fixed. This is an UGLY hack!
   HB_SetIniComment( "", "\" )

   cPath := HB_Argv(0)
   nAt := RAt( DIR_SEPARATOR, cPath )
   cPath := Left( cPath, nAt )

   IF File( cPath + "xedit.ini" )
      TRY
         aIni := HB_ReadIni( cPath + "xedit.ini", .T., "=" )
      CATCH
         aIni := NIL
      END
   ENDIF

   s_nShow            := NIL
   s_nLeft            := NIL
   s_nTop             := NIL
   s_nRight           := NIL
   s_nBottom          := NIL
   s_cFont            := ""
   s_nFontSize        := -12
   #ifdef WIN
      s_nFontWeight      := FW_DONTCARE
      s_nCharSet         := DEFAULT_CHARSET
   #endif
   s_lAutoSave        := .T.
   s_lFocusLine       := .T.
   s_nTabSpaces       := 3
   s_lForceEOL        := .F.

   #if defined( XEDIT_DYN_ACCELRATOR )
      s_nKeyAccelerator  := 2
   #endif

   s_lESCClosesWindow := .F.
   s_lResetOnSave     := .T.

   IF ! Empty( aIni )

   #ifndef VXH
      TRY
         s_nShow            := Val( aIni:Main[ "Show"                     ] )
      CATCH oError
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      END

      TRY
         s_nLeft            := Val( aIni:Main[ "Left"                     ] )
      CATCH oError
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      END
      TRY
         s_nTop             := Val( aIni:Main[ "Top"                      ] )
      CATCH oError
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      END
      TRY
         s_nRight           := Val( aIni:Main[ "Right"                    ] )
      CATCH oError
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      END
      TRY
         s_nBottom          := Val( aIni:Main[ "Bottom"                   ] )
      CATCH oError
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      END
      TRY
         s_lAutoSave        := &(   aIni:Main[ "AutoSave"                 ] )
      CATCH oError
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      END
      TRY
         s_lESCClosesWindow := &(   aIni:Main[ "ESC Closes active window" ] )
      CATCH oError
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      END
    #else
      s_lAutoSave           := .F. // not for VXH
   #endif

      TRY
         s_cFont            := aIni:Main[ "Font"                          ]
      CATCH oError
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      END
      TRY
         s_nFontSize        := Val( aIni:Main[ "FontSize"                 ] )
      CATCH oError
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      END
      TRY
         s_nFontWeight      := Val( aIni:Main[ "FontWeight"               ] )
      CATCH oError
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      END
      TRY
         s_nCharSet         := Val( aIni:Main[ "CharSet"                  ] )
      CATCH oError
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      END
      TRY
         s_lFocusLine       := &(   aIni:Main[ "FocusLine"                ] )
      CATCH oError
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      END
      TRY
         s_nTabSpaces       := Int( Val( aIni:Main[ "Tab"                 ] ) )
      CATCH oError
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      END
      TRY
         s_lForceEOL        := &(   aIni:Main[ "Force EOL"                ] )
      CATCH oError
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      END

   #if defined( XEDIT_DYN_ACCELRATOR )
      TRY
         s_nKeyAccelerator  := Val( aIni:Main[ "Key Accelerator"          ] )
      CATCH oError
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      END
   #endif

      TRY
         s_lResetOnSave    := &(   aIni:Main[ "Reset Modified on Save"   ] )
      CATCH oError
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      END

      TRY
         FOR EACH sSection IN aIni:Keys
            //TraceLog( sSection )

            IF Lower( sSection ) == "find"
               aEntries := aIni[ sSection ]

               FOR EACH cKey IN aEntries:Keys
                  IF cKey[1] == '#'
                     LOOP
                  ELSE
                     aAdd( s_aFind, aEntries[ cKey ] )
                  ENDIF
               NEXT
            ELSEIF Lower( sSection ) == "replace"
               aEntries := aIni[ sSection ]

               FOR EACH cKey IN aEntries:Keys
                  IF cKey[1] == '#'
                     LOOP
                  ELSE
                     aAdd( s_aReplace, aEntries[ cKey ] )
                  ENDIF
               NEXT
            ELSEIF Lower( sSection ) == "recent files"
               aEntries := aIni[ sSection ]

               FOR EACH cKey IN aEntries:Keys
                  IF cKey[1] == '#'
                     LOOP
                  ELSE
                     #ifdef __PLATFORM__Windows
                        IF aScan( s_aFiles, Lower( aEntries[ cKey ] ), , .T. ) == 0
                           aAdd( s_aFiles, Lower( aEntries[ cKey ] ) )
                        ENDIF
                     #else
                        IF aScan( s_aFiles, aEntries[ cKey ], , .T. == 0
                           aAdd( s_aFiles, aEntries[ cKey ] )
                        ENDIF
                     #endif
                  ENDIF
               NEXT
            ELSEIF Lower( sSection ) == "recent workspaces"
               aEntries := aIni[ sSection ]

               FOR EACH cKey IN aEntries:Keys
                  IF cKey[1] == '#'
                     LOOP
                  ELSE
                     #ifdef __PLATFORM__Windows
                        IF aScan( s_aWorkspaces, Lower( aEntries[ cKey ] ), , .T. ) == 0
                           aAdd( s_aWorkspaces, Lower( aEntries[ cKey ] ) )
                        ENDIF
                     #else
                        IF aScan( s_aWorkspaces, aEntries[ cKey ], , .T. ) == 0
                           aAdd( s_aWorkspaces, aEntries[ cKey ] )
                        ENDIF
                     #endif
                  ENDIF
               NEXT
            ELSEIF sSection[1] == '.'
               aExtensions := HB_aTokens( SubStr( sSection, 2 ), '.' )
               cExt := aExtensions[1]
               aDel( aExtensions, 1, .T. )

               aAdd( aColorExtensions, cExt )

               TRY
                  ExtensionColors := s_CustomColors[ cExt ]
               CATCH
                  s_CustomColors[ cExt ] := Hash()
                  ExtensionColors := s_CustomColors[ cExt ]
               END
               ExtensionColors[ "hExtensionTree" ] := NIL

               // Default!!!
               ExtensionColors[ "Background"    ] := s_Colors[ "White"      ]
               ExtensionColors[ "Text"          ] := s_Colors[ "Black"      ]
               ExtensionColors[ "Comments"      ] := s_Colors[ "Dark Green" ]
               ExtensionColors[ "Directives"    ] := s_Colors[ "Orange"     ]
               ExtensionColors[ "Keywords"      ] := s_Colors[ "Blue"       ]
               ExtensionColors[ "Reserved"      ] := s_Colors[ "Dark Blue"  ]
               ExtensionColors[ "Operators"     ] := s_Colors[ "Magenta"    ]
               ExtensionColors[ "Literals"      ] := s_Colors[ "Red"        ]
               ExtensionColors[ "Open Literals" ] := s_Colors[ "Dark Red"   ]
               ExtensionColors[ "API"           ] := s_Colors[ "Cyan"       ]
               ExtensionColors[ "Highlight"     ] := s_Colors[ "Yellow"     ]

               // Aliases
               ExtensionColors[ "Name" ] := "*." + cExt
               FOR EACH cExt IN aExtensions
                  ExtensionColors[ "Name" ] += "; *." + cExt
                  s_CustomColors[ cExt ] := ExtensionColors
               NEXT

               aEntries := aIni[ sSection ]

               //TraceLog( ValToPrg( aEntries ) )

               FOR EACH cKey IN aEntries:Keys
                  //TraceLog( cKey )

                  IF cKey[1] == '#'
                     // Preserve Comments.
                     ExtensionColors[ cKey ] := aEntries[ cKey ]
                     LOOP
                  ELSEIF Lower( cKey ) == "wincolorizer"
                     ExtensionColors[ "WinColorizer" ] := aEntries[ cKey ]

                     #ifdef WIN
                     #else
                        LOOP
                     #endif
                  ELSEIF Lower( cKey ) == "concolorizer"
                     ExtensionColors[ "ConColorizer" ] := aEntries[ cKey ]

                     #ifdef WIN
                        LOOP
                     #else
                     #endif
                  ELSEIF Lower( cKey ) == "initializer"
                     ExtensionColors[ "Initializer" ] := aEntries[ cKey ]
                  ENDIF

                #ifdef WIN
                  IF Lower( cKey ) == "wincolorizer"
                #else
                  IF Lower( cKey ) == "concolorizer"
                #endif
                     #ifdef SCRIPTS
                        //TraceLog( aEntries[ cKey ] )
                        nAt := At( "->", aEntries[ cKey ] )

                        IF nAt > 0
                           sScriptFile := Lower( Left( aEntries[ cKey ], nAt - 1 ) )
                           sProcedure  := SubStr( aEntries[ cKey ], nAt + 2 )

                           nAt := At( "(", sProcedure )
                           IF nAt > 0
                              sProcedure := Left( sProcedure, nAt - 1 )
                           ENDIF

                           //TraceLog( sScriptFile, sProcedure )

                           IF aScan( aScripts, sScriptFile, , , .T. ) == 0
                              s_oInterpreter:AddText( MemoRead( cPath + sScriptFile ) )

                              xRet := s_oInterpreter:Compile()
                              //TraceLog( xRet )

                              IF xRet:ClassName == "ERROR"
                                 TraceLog( xRet:ModuleName, xRet:ProcName, xRet:ProcLine, ValToPrg( xRet:Args ) )
                                 Alert( "Script: " + cPath + sScriptFile + ";" + xRet:SubSystem + ";" + xRet:Operation + ";" + xRet:Description )
                                 LOOP
                              ENDIF

                              aAdd( aScripts, sScriptFile )
                           ENDIF

                           //TraceLog( HB_FuncPtr( sProcedure ) )
                           //ExtensionColors[ "ColorizerScriptFile" ]      := sScriptFile
                           ExtensionColors[ "ColorizerScriptProcedure" ] := sProcedure
                        ENDIF
                     #endif

                     LOOP
                  ELSEIF Lower( cKey ) == "initializer"
                     #ifdef SCRIPTS
                        //TraceLog( aEntries[ cKey ] )
                        nAt := At( "->", aEntries[ cKey ] )

                        IF nAt > 0
                           sScriptFile := Lower( Left( aEntries[ cKey ], nAt - 1 ) )
                           sProcedure  := SubStr( aEntries[ cKey ], nAt + 2 )

                           nAt := At( "(", sProcedure )
                           IF nAt > 0
                              sProcedure := Left( sProcedure, nAt - 1 )
                           ENDIF

                           //TraceLog( sScriptFile, sProcedure, HB_FuncPtr( sProcedure ) )

                           IF aScan( aScripts, sScriptFile, , , .T. ) == 0
                              //Alert( "Compiling: " + sProcedure )
                              s_oInterpreter:AddText( MemoRead( cPath + sScriptFile ) )

                              xRet := s_oInterpreter:Compile()
                              //TraceLog( xRet )

                              IF xRet:ClassName == "ERROR"
                                 TraceLog( xRet:ModuleName, xRet:ProcName, xRet:ProcLine, ValToPrg( xRet:Args ) )
                                 Alert( "Script: " + cPath + sScriptFile + ";" + xRet:SubSystem + ";" + xRet:Operation + ";" + xRet:Description )
                                 LOOP
                              ENDIF

                              aAdd( aInitializers, sProcedure )
                              aAdd( aScripts, sScriptFile )
                           ENDIF
                        ENDIF
                     #endif

                     LOOP
                  ELSEIF Lower( cKey ) == "afterkey"
                     #ifdef SCRIPTS
                        //TraceLog( aEntries[ cKey ] )
                        nAt := At( "->", aEntries[ cKey ] )

                        IF nAt > 0
                           sScriptFile := Lower( Left( aEntries[ cKey ], nAt - 1 ) )
                           sProcedure  := SubStr( aEntries[ cKey ], nAt + 2 )

                           nAt := At( "(", sProcedure )
                           IF nAt > 0
                              sProcedure := Left( sProcedure, nAt - 1 )
                           ENDIF

                           IF aScan( aScripts, sScriptFile, , , .T. ) == 0
                              //TraceLog( cPath, sScriptFile, sProcedure, MemoRead( cPath + sScriptFile ) )
                              s_oInterpreter:AddText( MemoRead( cPath + sScriptFile ) )

                              xRet := s_oInterpreter:Compile()
                              //TraceLog( xRet, sScriptFile, sProcedure )

                              IF xRet:ClassName == "ERROR"
                                 TraceLog( xRet:ModuleName, xRet:ProcName, xRet:ProcLine, ValToPrg( xRet:Args ) )
                                 Alert( "Script: " + cPath + sScriptFile + ";" + xRet:SubSystem + ";" + xRet:Operation + ";" + xRet:Description )
                                 LOOP
                              ENDIF

                              aAdd( aScripts, sScriptFile )
                           ENDIF

                           ExtensionColors[ "AfterKeyScriptFile" ]      := sScriptFile
                           ExtensionColors[ "AfterKeyScriptProcedure" ] := sProcedure
                        ENDIF
                     #endif

                     LOOP
                  ENDIF

                  #ifdef WIN
                     Color := aEntries[ cKey ]

                     IF Color[1] == '('
                        Color := SubStr( Color, 2, Len( Color ) - 2 )
                        aRGB := HB_aTokens( Color, ',' )
                        ExtensionColors[ cKey ] := RGB( Val(aRGB[1] ), Val( aRGB[2] ), Val( aRGB[3] ) )
                     ELSE
                        TRY
                           ExtensionColors[ cKey ] := s_Colors[ Color ]
                        CATCH
                           TraceLog(cKey,  Color )
                        END
                     ENDIF
                  #else
                     IF Lower( cKey ) == "background"
                        LOOP
                     ENDIF

                     TRY
                        ExtensionColors[ cKey ] := s_Colors[ Color ] + "/" + s_PrgColors[ "Background" ]
                     CATCH
                        TraceLog( cKey, Color )
                     END
                  #endif
               NEXT
            ENDIF
         NEXT

         //TraceLog( s_oInterpreter:aCompiledProcs, aInitializers )

         #ifdef SCRIPTS
            IF Len( s_oInterpreter:aCompiledProcs ) > 0
               s_oInterpreter:SetStaticProcedures()
               //TraceLog( "Procedures:", s_oInterpreter:aCompiledProcs )
               PP_GenDynProcedures( s_oInterpreter:aCompiledProcs )

               FOR EACH sProcedure IN aInitializers
                   //TraceLog( "Init: " + sProcedure )

                   TRY
                      HB_Exec( HB_FuncPtr( sProcedure ) )
                   CATCH oError
                      TraceLog( oError:ProcLine, sProcedure, oError:Description + ";" + oError:Operation )
                   END
               NEXT

               FOR EACH cExt IN aColorExtensions
                  //TraceLog( cExt )
                  TRY
                     S_CustomColors[ cExt ][ "Colorizer" ] := HB_FuncPtr( S_CustomColors[ cExt ][ "ColorizerScriptProcedure" ] )
                  CATCH oError
                     TraceLog( oError:ProcLine, cExt, oError:Description + ";" + oError:Operation )
                  END

                  TRY
                     S_CustomColors[ cExt ][ "AfterKey" ] := HB_FuncPtr( S_CustomColors[ cExt ][ "AfterKeyScriptProcedure" ] )
                     //TraceLog( cExt, S_CustomColors[ cExt ][ "AfterKeyScriptProcedure" ], S_CustomColors[ cExt ][ "AfterKey" ] )
                  CATCH oError
                     TraceLog( oError:ProcLine, cExt, oError:Description + ";" + oError:Operation )
                  END
               NEXT
            ENDIF
         #endif

      CATCH oError
         TraceLog( oError:ProcLine, cKey, oError:Description + ";" + oError:Operation )
      END
   ENDIF

   #ifdef WIN
      #ifndef DLL
         s_hInstance := GetModuleHandle()
      #endif

      s_wc := (struct WNDCLASS)

      s_wc:style         := CS_OWNDC | CS_DBLCLKS | CS_SAVEBITS
      s_wc:lpszClassName := "xEdit"
      s_wc:lpfnWndProc   := WinCallbackPointer( @xEditControlWindowProc() )
      s_wc:hInstance     := s_hInstance
      //s_wc:hbrBackground := GetStockObject( WHITE_BRUSH )
      //s_wc:hCursor       := LoadCursor( NIL, IDC_IBEAM )

      RegisterClass( s_wc )

      WITH OBJECT (struct WNDCLASS)
         :style         := 0
         //:lpfnWndProc   := WinCallbackPointer( HB_ObjMsgPtr( <oDisplay>, "xEditFinderWindowProc" ), <oDisplay> )
         :lpfnWndProc   := DefWindowProcPointer()
         :hInstance     := s_hInstance
         :hbrBackground := COLOR_BTNFACE + 1
         :hCursor       := 0
         :lpszClassName := "xEditFinder"

         RegisterClass( HB_QWith() )
      END

      WITH OBJECT (struct WNDCLASS)
         :style         := CS_OWNDC | CS_DBLCLKS
         :lpfnWndProc   := WinCallbackPointer( @xEditPopupWindowProc() )
         :hInstance     := s_hInstance
         //:hbrBackground := GetStockObject( WHITE_BRUSH )
         //:hCursor       := LoadCursor( NIL, IDC_IBEAM )
         :hIcon         := LoadIcon( s_hInstance, "Application" )
         //:lpszMenuName  := "XEDITCONTAINER"
         :lpszClassName := "xEditPopup"

         RegisterClass( HB_QWith() )
      END

      WITH OBJECT (struct WNDCLASS)
         :style         := CS_OWNDC | CS_DBLCLKS
         :lpfnWndProc   := WinCallbackPointer( @xHDNWindowProc() )
         :hInstance     := s_hInstance
         //:hbrBackground := GetStockObject( WHITE_BRUSH )
         //:hCursor       := LoadCursor( NIL, IDC_IBEAM )
         :hIcon         := LoadIcon( s_hInstance, "Application" )
         //:lpszMenuName  := "XEDITCONTAINER"
         :lpszClassName := "xHDNWindow"

         RegisterClass( HB_QWith() )
      END

      s_hFont := CreateFont( s_nFontSize, 0, 0, 0, s_nFontWeight, .F., .F., .F., s_nCharSet, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, FIXED_PITCH | FF_DONTCARE, s_cFont )
      //TraceLog( s_hFont )

      GetClientRect( GetDesktopWindow(), @s_DesktopRect )

      s_hPlus     := LoadIcon( s_hInstance, "PLUS" )
      s_hMinus    := LoadIcon( s_hInstance, "MINUS" )
      s_hBookmark := LoadIcon( s_hInstance, "BOOKMARK" )
      //s_hModified := LoadIcon( s_hInstance, "MODIFIED" )

      //s_hShrink := LoadIcon( s_hInstance, "SHRINK" )

      //TraceLog( s_hPlus, s_hMinus )
   #endif

RETURN

EXIT PROCEDURE ExitEditor
   s_oInterpreter := NIL
RETURN

STATIC FUNCTION UnRef( x )
   //TraceLog( x )
RETURN x

FUNCTION PRG_KeyWord()
RETURN s_PRG_KeyWord

FUNCTION PRG_Word()
RETURN s_PRG_Word

FUNCTION PRG_Literal()
RETURN s_PRG_Literal

FUNCTION PRG_Operator()
RETURN s_PRG_Operator

FUNCTION PrgColors()
RETURN s_PrgColors

FUNCTION CountLines( Line )

   LOCAL nLines := 1

   IF Line == NIL
      RETURN 0
   ENDIF

   WHILE Line[ ED_NEXTLINE ] != NIL
      Line := Line[ ED_NEXTLINE ]
      nLines++
   END

RETURN nLines

#ifdef WIN
   FUNCTION GetLogErrors( sText )

      LOCAL nID := 0, aaErrors := {}, sLine, aMatch, sFile

      WHILE NextLine( @sText, @sLine )
         IF Empty( sLine )
            LOOP
         ENDIF

         sLine := LTrim( sLine )

         aMatch := HB_Regex( s_TraceLog, sLine )

         IF Empty( aMatch )
            aMatch := HB_Regex( s_ErrorLine, sLine )
            //TraceLog( ValToPrg( aMatch ) )

            IF Empty( aMatch )
               aMatch := HB_Regex( s_WarningLine, sLine )
            ENDIF

            IF Empty( aMatch )
               aMatch := HB_Regex( s_MissingExternal, sLine )
            ENDIF

            IF ! Empty( aMatch )
               //aMatch[1] := Str( ++nID, 3 )
               aAdd( aaErrors, aDel( aMatch, 1, .T. )  )
            ENDIF
         ELSE
            IF aMatch[2] = "Couldn't"
               sFile := HB_aTokens( sLine, " " )[-1]
               sFile := Left( sFile, Len( sFile ) - 3 )
               aAdd( aaErrors, { /*Str( ++nID, 3 ),*/ sFile, "", "Error", "Couldn't build" } )
            ELSEIF aMatch[2] = "[In use?]"
               sFile := HB_aTokens( sLine, " " )[-1]
               sFile := Left( sFile, Len( sFile ) - 3 )
               aAdd( aaErrors, { /*Str( ++nID, 3 ),*/ sFile, "", "Error", "[In use?] couldn't erase" } )
            ENDIF
         ENDIF
      END

   RETURN aaErrors

   FUNCTION xEditListView( hWnd, nLeft, nTop, nWidth, nHeight, aHeaders, aLines )

      LOCAL LVItem, LVColumn
      LOCAL sHeader, nRow, aLine, sText

      OutputDebugString( "Listview Parent:" + Str( hWnd ) + EOL )

      hWnd := CreateWindowEx( WS_EX_CLIENTEDGE, "SysListView32", "", WS_CHILD | WS_CLIPSIBLINGS |;
                                 LVS_REPORT | LVS_SINGLESEL | LVS_NOSORTHEADER,;
                                 nLeft, nTop, nWidth, nHeight, hWnd, 0, s_hInstance )

       SendMessage( hWnd, LVM_SETEXTENDEDLISTVIEWSTYLE, 0,;
                    SendMessage( hwnd, LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0 ) |;
                    LVS_EX_GRIDLINES | LVS_EX_TWOCLICKACTIVATE | LVS_NOSORTHEADER | LVS_EX_FULLROWSELECT );

      OutputDebugString( "Listview:" + Str( hWnd ) + EOL )

      SendMessage( hWnd, WM_SETFONT, s_hFont, 0 )

      IF SendMessage( hWnd, VIEW_DETAILS, 0, 0 ) == -1
        OutputDebugString( "Failed to display ListView Item!" + EOL )
      ENDIF

      LVColumn := (struct LVCOLUMN)
      LVColumn:mask := LVCF_TEXT | LVCF_WIDTH

      FOR EACH sHeader IN aHeaders
         LVColumn:cx := Max( Len( sHeader ), Len( aLines[1][ HB_EnumIndex() ] ) ) * 15
         LVColumn:pszText := sHeader

         IF SendMessage( hWnd, LVM_INSERTCOLUMN, HB_EnumIndex() - 1, LVColumn ) == -1
            OutputDebugString( "Failed to add column: " + LVColumn:pszText + EOL )
         //ELSE
         //   OutputDebugString( "Added column #" + Str( HB_EnumIndex() - 1 ) + " " + LVColumn:pszText + EOL )
         ENDIF
      NEXT


      LVItem := (struct LVITEMA)
      LVItem:mask := LVIF_TEXT | LVIF_PARAM

      FOR EACH aLine IN aLines
         nRow            := HB_EnumIndex() - 1

         LVItem:iItem    := nRow
         LVItem:iSubItem := 0
         LVItem:pszText  := aLine[1]
         LVItem:lParam   := 0//Some cargo

         IF SendMessage( hWnd, LVM_INSERTITEM, nRow, LVItem ) == -1
            OutputDebugString( "Failed to insert line:" + Str( nRow ) + " item: " + LVItem:pszText + EOL )
            LOOP
         //ELSE
         //   OutputDebugString( "Inserted line:" + Str( nRow ) + " item: " + LVItem:pszText + EOL )
         ENDIF

         FOR EACH sText IN aLine
            LVItem:pszText := sText
            LVItem:iSubItem := HB_EnumIndex() - 1

            IF SendMessage( hWnd, LVM_SETITEMTEXT, nRow, LVItem ) == -1
               OutputDebugString( "Failed to set sub item:" + Str( LVItem:iSubItem ) + " item: " + LVItem:pszText + EOL )
            //ELSE
            //   OutputDebugString( "Set sub item:" + Str( LVItem:iSubItem ) + " item: " + LVItem:pszText + EOL )
            ENDIF
         NEXT
      NEXT

      OutputDebugString( "xEditListView() Done." + EOL )

   RETURN hWnd

   FUNCTION xHDNWindowProc( hWnd, nMsg, nwParam, nlParam )

      SWITCH nMsg
         CASE WM_SIZE
            IF s_oxHDN != NIL
               s_oxHDN:__OnSize()
            ENDIF
            RETURN 0

         CASE WM_CREATE
            //s_oxHDN := CreateActiveX( hWnd, "Shell.Explorer", , OLEIVERB_INPLACEACTIVATE )
            s_oxHDN := TActiveX():New( hWnd, "Shell.Explorer", , OLEIVERB_INPLACEACTIVATE )
            RETURN 0

         CASE WM_DESTROY
            s_oxHDN := NIL
            EXIT
      END

   RETURN DefWindowProc( hWnd, nMsg, nwParam, nlParam )

   FUNCTION xEditPopupWindowProc( hWnd, nMsg, nwParam, nlParam )

       LOCAL Rect, hEdit := GetWindow( hWnd, GW_CHILD )
       LOCAL nEditor

       //TraceLog( hWnd, nMsg )
      //TraceLog( hEdit )

       IF ! Empty( hEdit )
          SWITCH nMsg
             CASE WM_SETFOCUS
                SetFocus( hEdit )
                RETURN 0

             CASE WM_WINDOWPOSCHANGED
                ValidateRect( hWnd )

                GetClientRect( hWnd, @Rect )

                // NOT on Minimize
                IF Rect:right == 0 .OR. Rect:bottom == 0
                   RETURN 0
                ENDIF

                //TraceLog( Rect:right, Rect:bottom, ::nStatusHeight )
                MoveWindow( hEdit, 0, 0, Rect:right, Rect:bottom, .T. )
                RETURN 0

            CASE WM_QUERYENDSESSION
            CASE WM_CLOSE
               nEditor := aScan( s_aPopups, {|o| o:oDisplay:hWnd == hEdit } )
               IF nEditor > 0
                  IF ! s_aPopups[ nEditor ]:oDisplay:CloseWindow( .F. )
                     RETURN 0
                  ENDIF
               ENDIF

               IF nMsg == WM_QUERYENDSESSION
                  RETURN 1
               ENDIF

               DestroyWindow( hWnd )
               RETURN 0

            CASE WM_DESTROY
               SetFocus( GetWindow( hWnd, GW_OWNER ) )
          END
       ENDIF

   RETURN DefWindowProc( hWnd, nMsg, nwParam, nlParam )

   FUNCTION PopupEditor( hParent, nLeft, nTop, nWidth, nHeight, cFile, nLine, sText )

      LOCAL Rect, oEditor, nPointer, oError, hPopup, hWnd

      IF nWidth > 0 .AND. nHEight > 0
         hPopup := CreateWindowEx( 0, "xEditPopup", cFile, WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN | WS_CLIPSIBLINGS, nLeft, nTop, nWidth, nHeight, hParent, 0, s_hInstance )
      ELSE
        hPopup := hParent
      ENDIF

      //TraceLog( hWnd, cFile )

      GetClientRect( hPopup, @Rect )

      hWnd := CreateWindowEx( WS_EX_CLIENTEDGE, "xEdit", NIL, WS_CHILD | WS_VISIBLE | WS_VSCROLL | WS_HSCROLL, 0, 0, Rect:right, Rect:bottom, hPopup, 0, s_hInstance )
      //TraceLog( hWnd )

      TRY
         nPointer := SendMessage( hWnd, EN_GETEDITOR )

         IF nPointer == 0
            Throw( ErrorNew( "xEdit", 0, 1002, "EN_GETEDITOR failure.", HB_aParams() ) )
         ENDIF

         oEditor := ArrayFromPointer( nPointer, .T. )

         IF oEditor == NIL
            Throw( ErrorNew( "xEdit", 0, 1003, "EN_GETEDITOR failure.", HB_aParams() ) )
         ENDIF

         IF Empty( cFile ) .AND. sText == NIL
            Throw( ErrorNew( "xEdit", 0, 1002, "Invalid file.", HB_aParams() ) )
         ENDIF

         oEditor:Load( cFile, sText, .F. )

         //TraceLog( cFile, oEditor:cPath, oEditor:cFile )

         IF ! Empty( nLine )
            oEditor:GoLine( nLine )
         ENDIF

         ShowWindow( hPopup, SW_SHOW )
         oEditor:oDisplay:Display()
         SetFocus( hWnd )

         aAdd( s_aPopups, oEditor )
      CATCH oError
         DestroyWindow( hPopup )

         MessageBox( hParent, "Sorry, I have encountered an Error:" + ;
                         CRLF + CRLF + ;
                         oError:Description + CRLF + oError:Operation + CRLF + ;
                         oError:ModuleName + "->" + oError:ProcName + "(" + Str( oError:ProcLine, 5 ) + ")", + ;
                         "xEdit", MB_ICONERROR )
      END

      //TraceLog( 2, oEditor:cPath, oEditor:cFile )

   RETURN oEditor

   PROCEDURE AutoOpenSource( nRow, nCol, oEditor )

      LOCAL sLine := oEditor:CurrentLine[ ED_BUFFER ]
      LOCAL aMatch := HB_Regex( s_ErrorLine, sLine )

      (nRow)
      (nCol)

      IF Empty( aMatch )
         aMatch := HB_Regex( s_WarningLine, sLine )
      ENDIF

      IF ! Empty( aMatch )
         TRY
            PopupEditor( oEditor:oDisplay:hWnd, 10, 10, 780, 580, aMatch[2], Val( aMatch[3] ) )
         CATCH
         END
      ENDIF

   RETURN

   FUNCTION GetNiceFont( nWeight )

      LOCAL ncm

      SysNonClientMetrics( @ncm )

      IF nWeight != NIL
         ncm:lfMessageFont:lfWeight := nWeight
      ENDIF

   RETURN CreateFontIndirect( ncm:lfMessageFont )

   FUNCTION xEditControlWindowProc( hWnd, nMsg, nwParam, nlParam )

      LOCAl CreateStructure, pDisplay, oDisplay, hDC, Rect, TabItem
      LOCAL sFind, sReplace, nLeft, nTop
      #ifdef VXH
         LOCAL rc
      #endif

      //TraceLog( nMsg )

      SWITCH nMsg
         CASE WM_GETDLGCODE
            RETURN DLGC_WANTMESSAGE

         CASE WM_CREATE
            CreateStructure := (struct CREATESTRUCT)
            CreateStructure:Pointer( nlParam )

            //TraceLog( CreateStructure:lpszClass, CreateStructure:lpszName, CreateStructure:lpCreateParams, CreateStructure:x, CreateStructure:y, CreateStructure:cx, CreateStructure:cy )

            pDisplay := LongFromPointer( CreateStructure:lpCreateParams )

            IF Empty( pDisplay )
               oDisplay := EditorGUIDisplay()
            ELSE
               oDisplay := ArrayFromPointer( pDisplay, .T. )
            ENDIF

            WITH OBJECT oDisplay
               :hWnd := hWnd
               :lEscape := .F.

               hDC  := GetDC( hWnd )
               SelectObject( hDC, s_hFont )
               GetTextMetrics( hDC, @HB_QWith():TextMetric )
               ReleaseDC( hWnd, hDC )

               :TextMetric:tmHeight += 2

               GetClientRect( hWnd, @Rect )

               Rect:left   := :TextMetric:tmHeight//LEFT_PAD

               :nRows    := Int( Rect:bottom / :TextMetric:tmHeight )
               :nColumns := Int( ( Rect:right - Rect:left ) / :TextMetric:tmHeight ) - 1 // LEFT_PAD == ::TextMetric:tmHeight

               //TraceLog( :nRows, :nColumns, :TextMetric:tmHeight )

               IF :oEditor == NIL//! ProcName(2) == "EDITORGUIDISPLAY:NEW"
                  :oEditor := Editor()
               ENDIF
               :oEditor:SetDisplay( HB_QWith(), .F. )

               IF GetWindowLong( hWnd, GWL_STYLE ) & ES_READONLY != 0
                  :oEditor:lReadOnly := .T.
                  :oEditor:SetExtension( "log" )
                  :oEditor:pHitTest := HB_FuncPtr( "AutoOpenSource" )
               ENDIF

               :DefWindowProc := SetWindowLong( hWnd, GWL_WNDPROC, WinCallbackPointer( HB_ObjMsgPtr( HB_QWith(), "xEditWindowProc" ), HB_QWith() ) )

               nLeft := 100
               nTop  := 100

               #ifdef VXH
                  // Create the dialog based on the parent's position
                  rc := (struct RECT)
                  GetWindowRect( Application:MainForm:hWnd, @rc )
                  nLeft := rc:left + 20
                  nTop  := rc:top  + 20
               #endif

               :hFind := CreateWindowEx( 0, "xEditFinder", "Find and Replace", WS_POPUP | WS_CAPTION | WS_BORDER | WS_SYSMENU , nLeft, nTop, FIND_DIALOG_WIDTH, FIND_DIALOG_HEIGHT, hWnd, 0, s_hInstance, NIL )


               //TraceLog( :hFind )
               SetWindowLong( :hFind, GWL_WNDPROC, WinCallbackPointer( HB_ObjMsgPtr( HB_QWith(), "xEditFinderWindowProc" ), HB_QWith() ) )

               GetClientRect( :hFind, @Rect )

               TabItem := (struct TCITEM)

               TabItem:mask = TCIF_TEXT
               TabItem:iImage := -1

               :hFindTabControl := CreateWindowEx( 0, "SysTabControl32", "", WS_CHILD | WS_CLIPSIBLINGS | WS_VISIBLE | WS_TABSTOP | TCS_FIXEDWIDTH | TCS_FOCUSNEVER, 5, 5, Rect:right - 10, Rect:bottom - 10, :hFind, 0, s_hInstance, NIL )
               //TraceLog( :hFind, :hFindTabControl )

               IF Empty( s_hFindFont )
                  s_hFindFont := GetNiceFont()
               ENDIF
               SendMessage( :hFindTabControl, WM_SETFONT, s_hFindFont, 0 )

               TabItem:pszText := "&Find"
               SendMessage( :hFindTabControl, TCM_INSERTITEM, 0, TabItem )

               TabItem:pszText := "&Replace"
               SendMessage( :hFindTabControl, TCM_INSERTITEM, 1, TabItem )

               TabItem:pszText := "&Goto"
               SendMessage( :hFindTabControl, TCM_INSERTITEM, 2, TabItem )

               SendMessage( :hFindTabControl, TCM_SETITEMSIZE, 0, MAKELPARAM( 80, 25 ) )

               //Rect:left := 0; Rect:top := 0; Rect:right := 0; Rect:bottom := 0
               //TabCtrl_AdjustRect( s_hTypeTabControl, FALSE, &Rect );
               GetClientRect( :hFindTabControl, @Rect )
               //TraceLog( Rect:top, Rect:left, Rect:right, Rect:bottom )
               SendMessage( :hFindTabControl, TCM_ADJUSTRECT, .F., @Rect )

               //TraceLog( Rect:top, Rect:left, Rect:right, Rect:bottom )

               :hFindDialog := CreateDialog( IIF( Empty( s_hDll ), s_hInstance, s_hDll ), "FIND_DIALOG", :hFind, WinCallbackPointer( HB_ObjMsgPtr( HB_QWith(), "xEditFindDialogProc" ), HB_QWith() ) )

               CheckRadioButton( :hFindDialog, ID_Top, ID_Previous, ID_Next )

               ShowWindow( GetDlgItem( :hFindDialog, ID_RepLabel ), SW_HIDE )
               ShowWindow( GetDlgItem( :hFindDialog, ID_Replace  ), SW_HIDE )
               ShowWindow( GetDlgItem( :hFindDialog, ID_Once     ), SW_HIDE )
               ShowWindow( GetDlgItem( :hFindDialog, ID_All      ), SW_HIDE )

               s_hActiveDialog := :hFindDialog

               ShowWindow( s_hActiveDialog, SW_SHOW )

               //TraceLog( :hFindDialog )
               Stretch( :hFindDialog, Rect:right - Rect:left + 2, Rect:bottom - Rect:top + 2, -1, -1, FALSE, Rect:left + 3, Rect:top + 4, /*"MS Sans Serif"*/, .T. );
               //MoveWindow( :hFindDialog, Rect:left + 3, Rect:top + 4, Rect:right - Rect:left + 2, Rect:bottom - Rect:top + 2, .F. );

               FOR EACH sFind IN s_aFind
                  SendMessage( GetDlgItem( :hFindDialog, ID_Find ), CB_ADDSTRING, 0, UnRef( sFind ) )
               NEXT

               FOR EACH sReplace IN s_aReplace
                  SendMessage( GetDlgItem( :hFindDialog, ID_Replace ), CB_ADDSTRING, 0, UnRef( sReplace ) )
               NEXT

               :hGotoDialog := CreateDialog( IIF( Empty( s_hDll ), s_hInstance, s_hDll ), "GOTO_DIALOG"   , :hFind, WinCallbackPointer( HB_ObjMsgPtr( HB_QWith(), "xEditGotoDialogProc" ), HB_QWith() ) )
               Stretch( :hGotoDialog, Rect:right - Rect:left + 2, Rect:bottom - Rect:top + 2, -1, -1, FALSE, Rect:left + 3, Rect:top + 4, /*"MS Sans Serif"*/, .T. );
               //MoveWindow( :hFindDialog, Rect:left + 3, Rect:top + 4, Rect:right - Rect:left + 2, Rect:bottom - Rect:top + 2, .F. );
            END
            RETURN 0
      END

      //TraceLog( "OOPS", nMsg )

   RETURN DefWindowProc( hWnd, nMsg, nwParam, nlParam )
#endif

CLASS EditorDisplay

   VAR oEditor              READONLY

   VAR nTop                 READONLY
   VAR nLeft                READONLY
   VAR nBottom              READONLY
   VAR nRight               READONLY

   VAR nRows         INIT 0 READONLY
   VAR nColumns      INIT 0 READONLY

   VAR BaseLine             READONLY

   VAR nBaseLine     INIT 0 READONLY
   VAR nBaseColumn   INIT 0 READONLY
   VAR nRow          INIT 0 READONLY
   VAR nColumn       INIT 0 READONLY

   VAR nFocus        INIT -1 READONLY

   VAR lEscape       INIT .T.

   VAR nDeferDisplay INIT 0

   METHOD Synch( lDisplay )

   METHOD Display( nLine )  VIRTUAL
   METHOD ShowSelection( nLineFrom, nColumnFrom, nLineTo, nColumnTo, nLine ) VIRTUAL

   /*
   METHOD ScrollUp          VIRTUAL
   METHOD ScrollDown        VIRTUAL
   METHOD ScrollLeft        VIRTUAL
   METHOD ScrollRight       VIRTUAL
   */

   METHOD GoRow( nRow )
   METHOD GoColumn( nColumn )
   METHOD Status()          VIRTUAL
   METHOD CloseWindow( lSetDisplay )
   METHOD OpenWorkSpace( cFile )
   METHOD OpenXBP( cFile )

ENDCLASS

METHOD Synch( lDisplay ) CLASS EditorDisplay

   LOCAL nBaseLine, nLine

   WITH OBJECT ::oEditor
      nLine := :nLine

      nBaseLine := nLine - ( ::nRows >> 1 )
      nBaseLine := Max( 0, nBaseLine )

      ::BaseLine := :CurrentLine

      //TraceLog( nLine, nBaseLine, :nLine, ::nRows, :CurrentLine[ ED_BUFFER ] )

      WHILE --nLine > nBaseLine
         ::BaseLine := ::BaseLine[ ED_PREVLINE ]
      END

      ::nBaseLine := nBaseLine
      ::nRow := :nLine - nBaseLine - 1

      IF :nColumn <= ::nColumns
         ::nBaseColumn := 0
         ::nColumn := :nColumn - 1
      ELSE
         ::nBaseColumn := :nColumn - ::nColumns
         ::nColumn := :nColumn - ::nBaseColumn - 1
      ENDIF

      //TraceLog( :nLine, :nColumn, ::nRow, ::nColumn, ::nBaseLine, ::nBaseColumn, ::nRows, ::nColumns, ::nDeferDisplay )

      IF lDisplay
         #ifdef DEMO
            Alert(  "Thank you for reviewing this DEMO Version of xEdit from http://www.xHarbour.com" )
         #endif

         //TraceLog( :pColorizer, ::BaseLine[ ED_BUFFER ] )

         ::Display()
      //ELSE
      //   ::Status()
      ENDIF
   END

RETURn Self

METHOD GoRow( nRow, hDC ) CLASS EditorDisplay

   //TraceLog( nRow, ::nRow, ::nColumn )

   IF ::nRow < 0 //.OR. ::nColumn < 0 //.OR. nRow == ::nRow
      RETURN Self
   ENDIF

   WITH OBJECT ::oEditor
      //TraceLog( nRow, ::nRow, ::nBaseLine, ::nColumn, ::nRows, :nLine, :nLines, :CurrentLine[ ED_BUFFER ] )

      nRow := Min( nRow, :nLines - ::nBaseLine - 1 )
      ::nRow := nRow

      :nLine := ::nBaseLine + nRow + 1
      :CurrentLine := ::BaseLine

      WHILE nRow > 0
         :CurrentLine := :CurrentLine[ ED_NEXTLINE ]
         nRow--
      ENDDO

      IF ::nColumn > 0
         IF :lSquare .AND. :nColumnFrom > 0
            ::nColumn := :nColumnTo - ::nBaseColumn
         ELSEIF s_lForceEOL
            ::nColumn := Min( ::nColumn, Len( :CurrentLine[ ED_BUFFER ] ) - ::nBaseColumn )
         ENDIF
         ::nColumn := Max( 0, ::nColumn )

         :nColumn  := ::nBaseColumn + ::nColumn + 1
      ENDIF
   END

   ::Status( hDC )

   //TraceLog( nRow, ::nRow, ::nBaseLine, ::nColumn, ::nRows, ::oEditor:nLine, ::oEditor:nLines, ::oEditor:CurrentLine[ ED_BUFFER ] )

RETURN Self

METHOD GoColumn( nColumn ) CLASS EditorDisplay

   //TraceLog( nColumn, Len( ::oEditor:CurrentLine[ ED_BUFFER ] ) )
   IF ::oEditor:lSquare .AND. ::oEditor:lShift
   ELSEIF s_lForceEOL
      nColumn := Min( nColumn, Len( ::oEditor:CurrentLine[ ED_BUFFER ] ) - ::nBaseColumn )
   ENDIF

   ::nColumn := Max( 0, ::nColumn )

   ::nColumn := nColumn
   ::oEditor:nColumn := ::nBaseColumn + nColumn + 1

   ::Status()

RETURN Self

METHOD CloseWindow( lSetDisplay ) CLASS EditorDisplay

   LOCAL ofn, cFile, nRet
   #ifdef WIN
      LOCAL hTEmp
   #endif

   //TraceLog( :lModified, :cFile )

   WITH OBJECT ::oEditor
      IF :lModified
         IF Empty( :cFile )
            #ifdef WIN
               nRet := MessageBox( 0, "Do you want to save changes to <Not named yet>?", "xEdit", MB_TASKMODAL | MB_ICONQUESTION | MB_YESNOCANCEL )
            #else
                nRet := Alert( "Do you want to save changes to <Not named yet>?", { "Yes", "No", "Cancel" } )
                SWITCH nRet
                   CASE 1
                      nRet := IDYES
                      EXIT

                   CASE 2
                      nRet := IDNO
                      EXIT

                   CASE 3
                      nRet := IDCANCEL
                      EXIT
                END
            #endif

            IF nRet == IDCANCEL
               RETURN .F.
            ELSEIF nRet == IDYES
               #ifdef WIN
                  ofn := (struct OPENFILENAME)

                  ofn:lStructSize     := 76
                  ofn :hwndOwner      := ::hContainer
                  ofn:hInstance       := s_hInstance
                  ofn:nMaxFile        := MAX_PATH + 1
                  //ofn:lpstrInitialDir :=
                  ofn:lpstrFile       := Pad( "*.prg", MAX_PATH )
                  ofn:lpstrDefExt     := "prg"
                  ofn:lpstrFilter     := "xHarbour source files" + Chr(0) + "*.prg;*.ch;*.xbs;*.xfm" + Chr(0) +;
                                         "C sources"             + Chr(0) + "*.c;*.cpp;*.h"          + Chr(0) +;
                                         "Resource source file"  + Chr(0) + "*.rc"                   + Chr(0) +;
                                         "xBuild project files"  + Chr(0) + "*.xbp;*.inc"            + Chr(0) +;
                                         "Log files"             + Chr(0) + "*.log"                  + Chr(0) +;
                                         "Text files"            + Chr(0) + "*.txt"                  + Chr(0) +;
                                         "All files"             + Chr(0) + "*.*"                    + Chr(0)
                  ofn:lpstrTitle      := "xEdit - Save document as"
                  ofn:Flags           := OFN_NOREADONLYRETURN | OFN_OVERWRITEPROMPT | OFN_PATHMUSTEXIST
               #else
                  //TODO
               #endif

               IF GetSaveFileName( @ofn )
                  cFile := Left( ofn:lpstrFile, At( Chr(0), ofn:lpstrFile ) - 1 )
                  :Save( cFile )
               ELSE
                  RETURN .F.
               ENDIF
            ENDIF
         ELSE
            #ifdef WIN
               nRet := MessageBox( 0, "Do you want to save changes to " + :cPath + :cFile +"?", "xEdit", MB_TASKMODAL | MB_ICONQUESTION | MB_YESNOCANCEL )
            #else
                nRet := Alert( "Do you want to save changes to " + :cPath + :cFile +"?", { "Yes", "No", "Cancel" } )
                SWITCH nRet
                   CASE 1
                      nRet := IDYES
                      EXIT

                   CASE 2
                      nRet := IDNO
                      EXIT

                   CASE 3
                      nRet := IDCANCEL
                      EXIT
                END
            #endif

            IF nRet == IDCANCEL
               RETURN .F.
            ELSEIF nRet == IDYES
               :Save()
            ENDIF
         ENDIF
      ENDIF

      :Close()

      #ifdef WIN
         IF :hFileItem != NIL .AND. ::hFilesTree != NIL
            hTemp := :hFileItem
            :hFileItem := NIL
            //TraceLog( "Delete", hTemp )
            SendMessage( ::hFilesTree, TVM_DELETEITEM, 0, hTemp )
         ENDIF
      #endif
   END

   IF lSetDisplay
      IF Len( s_aEditors ) > 0
         //Alert( "Selecting" )
         s_aEditors[-1]:SetDisplay( Self, .T. )

         #ifdef WIN
            //SendMessage( ::hContainer, WM_SETTEXT, , "xEdit - " + ::oEditor:cPath + ::oEditor:cFile )
         #endif
      ELSE
         Editor():New( 0, 0, 0, 0, , Self )
         ::Display()
      ENDIF
   ELSE
      IF Len( s_aEditors ) > 0
         ::oEditor := s_aEditors[-1]
      ENDIF
   ENDIF

RETURN .T.

METHOD OpenWorkSpace( sWorkspace ) CLASS EditorDisplay

   LOCAL aIni, oError, aLineColumn

   #ifdef WIN
      LOCAL hRecent, sRecentFile
      LOCAL hCursor := SetCursor( LoadCursor( NIL, IDC_WAIT ) )
   #endif

   LOCAL sFile

   //TraceLog( s_aEditors )

   TRY
      WITH OBJECT ::oEditor
         WHILE Len( s_aEditors ) > 0
            IF ! ::CloseWindow( .F. )
               ::Display()

               #ifdef WIN
                  SendMessage( ::hContainer, WM_SETTEXT, , "xEdit - " + :cPath + :cFile )
               #endif

               BREAK
            ENDIF
         ENDDO

         IF ! DIR_SEPARATOR IN sWorkspace
            sWorkspace := DiskName() + ':' + DIR_SEPARATOR + CurDir() + DIR_SEPARATOR + sWorkspace
         ENDIF

         s_cWorkspaceFilename := sWorkspace

         aIni := HB_ReadIni( sWorkspace, .T., "=" )

         ::nDeferDisplay++

         FOR EACH sFile IN aIni:Main:Keys
            IF File( sFile )
               TRY
                  aLineColumn := HB_ATokens( aIni:Main[ sFile ],  ',' )

                  Editor():New( 0, 0, 0, 0, sFile, Self )

                  :GoLine( Val( aLineColumn[1] ) )
                  :nColumn := Val( aLineColumn[2] )
               CATCH oError
                  TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
               END
            ENDIF
         NEXT

         ::nDeferDisplay--
         ASSERT( ::nDeferDisplay >= 0 )

         TRY
            aLineColumn := HB_ATokens( aIni[ "Default"][ aIni[ "Default" ]:Keys[1] ],  ',' )

            WITH OBJECT Editor():New( 0, 0, 0, 0, aIni[ "Default" ]:Keys[1], Self )
               :GoLine( Val( aLineColumn[1] ) )
               :nColumn := Val( aLineColumn[2] )
            END
         CATCH oError
            TraceLog( oError:Operation, oError:Description, oError:ModuleName, oError:ProcName, oError:ProcLine )
         END

         ::Synch( .T. )

         #ifdef WIN
            IF aScan( s_aWorkspaces, sWorkspace, , , .T. ) == 0
               aIns( s_aWorkspaces, 1, sWorkspace, .T. )
               aSize( s_aWorkspaces, Min( 30, Len( s_aWorkspaces ) ) )

               hRecent := CreateMenu()

               FOR EACH sRecentFile IN s_aWorkspaces
                   AppendMenu( hRecent, MF_STRING, 5100 + HB_EnumIndex(), sRecentFile )
               NEXT

               DeleteMenu( GetSubMenu( s_hMenu, 0 ), 15, MF_BYPOSITION )
               InsertMenu( GetSubMenu( s_hMenu, 0 ), 15, MF_POPUP | MF_BYPOSITION, hRecent, "Recent Workspaces" )
            ENDIF
         #endif
      END
   CATCH oError
      IF oError != NIL
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      ENDIF
   END

   //TraceLog( s_aEditors )

   #ifdef WIN
      //SendMessage( ::hContainer, WM_SETTEXT, , "xEdit - " + ::oEditor:cPath + ::oEditor:cFile )

      SetCursor( hCursor )
   #endif

RETURN Self

METHOD OpenXBP( sXBP, lClean ) CLASS EditorDisplay

   LOCAL aLines, sLine, sFolder, sFile, sExt, oError

   #ifdef WIN
      LOCAL hCursor := SetCursor( LoadCursor( NIL, IDC_WAIT ) )
   #endif

   //TraceLog( s_aEditors )

   IF lClean == NIL
      lClean := .F.
   ENDIF

   TRY
      WITH OBJECT ::oEditor
         IF lClean
            WHILE Len( s_aEditors ) > 0
               IF ! ::CloseWindow( .F. )
                  ::Display()

                  #ifdef WIN
                     SendMessage( ::hContainer, WM_SETTEXT, , "xEdit - " + :cPath + :cFile )
                  #endif

                  BREAK
               ENDIF
            ENDDO
         ENDIF

         IF DIR_SEPARATOR IN sXBP
            sFolder := Left( sXBP, RAt( sXBP, DIR_SEPARATOR ) )
         ELSE
            sFolder := DiskName() + ':' + DIR_SEPARATOR + CurDir() + DIR_SEPARATOR
            sXBP := sFolder + sXBP
         ENDIF

         aLines := {}
         ProcessTextFile( sXBP, {|_1| aAdd( aLines, _1 ) }, @Eval() )

         ::nDeferDisplay++

         FOR EACH sLine IN aLines
            sLine := AllTrim( sLine )
            IF sLine[1] == '[' .AND. sLine[-1] == ']'
               sFile := AllTrim( SubStr( sLine, 2, Len( sLine ) - 2 ) )
               IF ! DIR_SEPARATOR IN sFile
                  sFile := sFolder + sFile
               ENDIF

               sExt := Lower( Right( sFile, 4 ) )

               IF sExt == ".xbp"
                  ::OpenXBP( sFile, .F. )
                  LOOP
               ELSEIF sExt == ".obj"
                  LOOP
               ELSEIF sExt == ".res"
                  LOOP
               ELSEIF sExt == ".lib"
                  LOOP
               ENDIF
            ELSE
               LOOP
            ENDIF

            IF File( sFile )
               TRY
                  Editor():New( 0, 0, 0, 0, sFile, Self )
               CATCH oError
                  TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
               END
            ENDIF
         NEXT

         ::nDeferDisplay--
         ASSERT( ::nDeferDisplay >= 0 )

         ::Synch( .T. )

         #if 0//def WIN
            IF aScan( s_aWorkspaces, sWorkspace, , , .T. ) == 0
               aIns( s_aWorkspaces, 1, sWorkspace, .T. )
               aSize( s_aWorkspaces, Min( 30, Len( s_aWorkspaces ) ) )

               hRecent := CreateMenu()

               FOR EACH sRecentFile IN s_aWorkspaces
                   AppendMenu( hRecent, MF_STRING, 5100 + HB_EnumIndex(), sRecentFile )
               NEXT

               DeleteMenu( GetSubMenu( s_hMenu, 0 ), 15, MF_BYPOSITION )
               InsertMenu( GetSubMenu( s_hMenu, 0 ), 15, MF_POPUP | MF_BYPOSITION, hRecent, "Recent Workspaces" )
            ENDIF
         #endif
      END
   CATCH oError
      IF oError != NIL
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      ENDIF
   END

   //TraceLog( s_aEditors )

   #ifdef WIN
      //SendMessage( ::hContainer, WM_SETTEXT, , "xEdit - " + ::oEditor:cPath + ::oEditor:cFile )

      SetCursor( hCursor )
   #endif

RETURN Self

#ifndef WIN

CLASS EditorConsoleDisplay FROM EditorDisplay

   METHOD New( oEditor, nRow, nCol, nRows, nColumns ) CONSTRUCTOR

   METHOD Display( nLine )

   METHOD Status()            INLINE SetPos( ::nBottom + 1, ::nLeft + 2 ), ;
                                     QQOut( ;
                                            Str( ::oEditor:nLine, 5 ) + ':' + Str( ::oEditor:nColumn, 5 ) ;
                                            , Pad( ::oEditor:CurrentLine[ ED_BUFFER ], ::nColumns - 15 ) ;
                                          ), ;
                                     SetPos( ::nTop + ::nRow, ::nLeft + ::nColumn )

ENDCLASS

METHOD New( oEditor, nTop, nLeft, nRows, nColumns ) CLASS EditorConsoleDisplay

   IF nTop == NIL
      nTop := 0
      nLeft := 0
      nRows := MaxRow() - 1
      nColumns := MaxCol() - 1
   ENDIF

   //TraceLog( nRows, nColumns )

   ::oEditor := oEditor

   ::nTop    := nTop + 1
   ::nLeft   := nLeft + 1
   ::nBottom := nTop + nRows
   ::nRight  := nLeft + nColumns

   ::nRows    := nRows
   ::nColumns := nColumns

   SetColor( "N/W" )

   @ nTop, nLeft TO nTop + nRows + 1, nLeft + nColumns + 1 DOUBLE
   @ nTop + 1, nLeft + 1 CLEAR TO nTop + nRows, nLeft + nColumns

   IF oEditor != NIL
      oEditor:SetDisplay( Self, .F. )
   ENDIF

RETURN Self

METHOD Display( nLine ) CLASS EditorConsoleDisplay

   LOCAL Line, nRow := 0, nRows := ::nRows, nBaseColumn, sLine
   LOCAL nEditorLine, nOpen := 0, aComment, nDump := 0, aDump
   LOCAL nBaseLine
   LOCAL aFunc
   LOCAL nX, nY
   LOCAL nColumns := ::nColumns
   LOCAL nTop := ::nTop, nLeft := ::nLeft
   LOCAL HighlightedLine

   //TraceLog( nRows, ::nBaseLine, tmHeight )

   IF ::nDeferDisplay > 0
      //::nDeferDisplay++
      ::GoRow( ::nRow )
      RETURN Self
   ENDIF

   WITH OBJECT ::oEditor
      HighlightedLine := :HighlightedLine

      IF ::nRow >= 0 .AND. ::nColumn >= 0
         SetCursor( .F. )
      ENDIF

      Line := ::BaseLine
      nEditorLine := ::nBaseLine + 1

      nBaseColumn := ::nBaseColumn

      WHILE nRow < nRows .AND. Line != NIL
         nX := ::nLeft
         nY := nTop + nRow

         /*
         IF Line == NIL
            TextOut( nX, nY, Space( nColumns ) )
            nRow++
            LOOP
         ENDIF
         */

         IF Line == HighlightedLine

         ENDIF

         IF nOpen == 0 .AND. nDump == 0
            FOR EACH aComment IN :aComments
               IF aComment[1] == 0
                  LOOP
               ENDIF

               IF aComment[1] <= nEditorLine .AND. nEditorLine <= aComment[2]
                  nOpen := HB_EnumIndex()
                  SetColor( s_PrgColors[ "Comments" ] )
                  EXIT
               ENDIF
            NEXT

            IF nOpen == 0
               FOR EACH aDump IN :aDumps
                  IF aDump[1] > nEditorLine
                     EXIT
                  ELSEIF aDump[1] <= nEditorLine .AND. nEditorLine <= aDump[2]
                     nDump := HB_EnumIndex()
                     SetColor( s_PrgColors[ "C Code" ] )
                     EXIT
                  ENDIF
               NEXT
            ENDIF
         ENDIF

         IF nOpen > 0
            IF nLine == NIL .OR. nLine == nRow
               TextOut( nX, nY, SubStr( Line[ ED_BUFFER ], nBaseColumn + 1, nColumns ) )
               @ nY, Col() CLEAR TO nY, nLeft + nColumns - 1
            ENDIF

            IF :aComments[nOpen][2] == nEditorLine
               nOpen := 0
               SetColor( s_PrgColors[ "Comments" ] )
            ENDIF
         ELSEIF nDump > 0
            IF nLine == NIL .OR. nLine == nRow
               TextOut( nX, nY, SubStr( Line[ ED_BUFFER ], nBaseColumn + 1, nColumns ) )
               @ nY, Col() CLEAR TO nY, nLeft + nColumns - 1
            ENDIF

            IF :aDumps[nDump][2] == nEditorLine
               nDump := 0
               SetColor( s_PrgColors[ "Text" ] )
            ENDIF
         ELSEIF :pColorizer != NIL
            IF nLine == NIL .OR. nLine == nRow
               HB_Exec( :pColorizer, , nX, nY, Line[ ED_BUFFER ], nBaseColumn, nColumns, :ExtensionColors, HB_QWith() )
               @ nY, Col() CLEAR TO nY, nLeft + nColumns - 1
            ENDIF
         ELSE
            IF nLine == NIL .OR. nLine == nRow
               SetColor( s_DefaultColors[ "Text" ] )
               TextOut( nX, nY, SubStr( Line[ ED_BUFFER ], nBaseColumn, nColumns ) )
               @ nY, Col() CLEAR TO nY, nLeft + nColumns - 1
            ENDIF
         ENDIF

         Line := Line[ ED_NEXTLINE ]
         nEditorLine++
         nRow++
      END

      ::ShowSelection( , , , , nLine )

      IF ::nRow >= 0 .AND. ::nColumn >= 0
         ::GoRow( ::nRow )

         SetCursor( .T. )
      ENDIF
   END

RETURN Self

PROCEDURE TextOutColorizePRGTokens(  nX, nY, sLine, nOffset, nMaxLen )

   LOCAL cToken, nIndex := 1, nLen, nLens := 0

   //TraceLog( sLine, nOffset )

   IF Empty( sLine ) .OR. nOffset >= Len( sLine )
      TextOut( nX, nY, Space( nMaxLen ) )
      RETURN
   ENDIF

   WHILE sLine[nIndex] == ' '
      nIndex++
   END
   nIndex--

   IF nIndex > nOffset
      nIndex -= nOffset
      nOffset := 0
      TextOut( nX, nY, Space( nIndex ) )
      nX += nIndex
      nLens += nIndex
   ELSE
      nOffset -= nIndex
   ENDIF

   sLine := LTrim( sLine )

   cToken := HB_AtX( s_PRG_KeyWord, sLine, , , @nLen )

   IF nLen > 0
      SetColor( s_PrgColors[ "Keywords" ] )
      sLine := SubStr( sLine, nLen + 1 )
   ELSE
      cToken := NextToken( @sLine )
      nLen := Len( cToken )

      IF cToken == '*'
         SetColor( s_PrgColors[ "Comments" ] )
         TextOut( nX, nY, SubStr( cToken + sLine, nOffset + 1, nMaxLen - nLens ) )
         RETURN
      ELSEIF cToken = "//" .OR. cToken == "&&"
         SetColor( s_PrgColors[ "Comments" ] )
         TextOut( nX, nY, SubStr( cToken + sLine, nOffset + 1, nMaxLen - nLens ) )
         RETURN
      ELSEIF cToken == '#'
         SetColor( s_PrgColors[ "Directives" ] )
         TextOut( nX, nY, SubStr( cToken + sLine, nOffset + 1, nMaxLen - nLens ) )
         RETURN
      ELSEIF cToken LIKE s_PRG_Operator
         SetColor( s_PrgColors[ "Operators" ] )
      ELSEIF sLine[1] == '(' .AND. '|' + Lower( cToken ) + '|' IN s_SysFunc
         SetColor( s_PrgColors[ "API" ] )
      ELSEIF cToken = "/*"
         SetColor( s_PrgColors[ "Comments" ] )
      ELSE
         SetColor( s_PrgColors[ "Text" ] )
      ENDIF
   ENDIF

   //TraceLog( cToken, sLine )

   IF nLens >= nOffset
      TextOut( nX, nY, Left( cToken, nMaxLen - nLens ) )
      nX += nLen
      nLens += nLen
   ELSEIF nLens + nLen > nOffset
      nLen := nLens + nLen - nOffset
      cToken := Right( cToken, nLen )
      TextOut( nX, nY, Left( cToken, nMaxLen - nLens ) )
      nX += nLen
      nLens := nOffset + nLen
   ELSE
      nLens += nLen
   ENDIF

   WHILE ! Empty( cToken := NextToken( @sLine ) )
      //TraceLog( cToken, sLine )
      nLen := Len( cToken )

      IF cToken LIKE s_PRG_Word
         SetColor( s_PrgColors[ "Reserved" ] )
      ELSEIF cToken LIKE s_PRG_Literal
         SetColor( s_PrgColors[ "Literals" ] )
      ELSEIF cToken LIKE s_PRG_Operator
         SetColor( s_PrgColors[ "Operators" ] )
      ELSEIF sLine[1] == '(' .AND. '|' + Lower( cToken ) + '|' IN s_SysFunc
         SetColor( s_PrgColors[ "API" ] )
      ELSEIF cToken = "/*" .OR. cToken = "//" .OR. cToken = "&&"
         SetColor( s_PrgColors[ "Comments" ] )
      ELSE
         SetColor( s_PrgColors[ "Text" ] )
      ENDIF

      IF nLens >= nOffset
         TextOut( nX, nY, Left( cToken, nMaxLen - nLens - nOffset ) )
         nX += nLen
         nLens += nLen
      ELSEIF nLens + nLen > nOffset
         nLen := nLens + nLen - nOffset
         cToken := Right( cToken, nLen )
         TextOut( nX, nY, Left( cToken, nMaxLen - nOffset ) )
         nX += nLen
         nLens := nOffset + nLen
      ELSE
         nLens += nLen
      ENDIF
   END

RETURN

PROCEDURE TextOutColorizeLOGTokens( nX, nY, sLine, nCharWidth, nOffset )
RETURN

#else

CLASS EditorGUIDisplay FROM EditorDisplay

   CLASS VAR wc

   VAR hContainer           READONLY
   VAR hWnd                 READONLY
   VAR hFilesTree           READONLY
   VAR hTreeRoot            READONLY
   VAR hStatusBar           //READONLY
   VAR hSplitter            READONLY
   VAR hFind                READONLY

   VAR hFindTabControl      READONLY
   VAR hFindDialog          READONLY
   VAR hGotoDialog          READONLY

   //VAR hShrinkTree          READONLY

   VAR hBrush               READONLY
   VAR hPadBrush            READONLY INIT COLOR_BTNFACE + 1

   VAR nStatusHeight INIT 0 READONLY
   VAR TextMetric           READONLY INIT (struct TEXTMETRIC)
   VAR DefWindowProc                 INIT DefWindowProcPointer()

   //VAR sFiller              READONLY INIT ""
   //VAR nFillerLength        READONLY INIT 0

   VAR lRepeat              READONLY INIT .F.
   VAR nPreviousKey         READONLY INIT 0

   VAR hHighlightBrush      READONLY

   METHOD New( oEditor, nTop, nLeft, nRows, nColumns, lPixel ) CONSTRUCTOR

   METHOD Display( hDC, nLine )
   METHOD Status( hDC, cText )

   METHOD xEditWindowProc( hWnd, message, wParam, lParam )
   METHOD xEditContainerWindowProc( hWnd, message, wParam, lParam )
   METHOD xEditSplitterWindowProc( hWnd, message, wParam, lParam )
   METHOD xEditFinderWindowProc( hWnd, message, wParam, lParam )
   METHOD xEditFindDialogProc( hWnd, message, wParam, lParam )
   METHOD xEditGotoDialogProc( hWnd, message, wParam, lParam )

   METHOD ShowSelection( nLineFrom, nColumnFrom, nLineTo, nColumnTo, nLine, hDC )

   METHOD NextWindow()
   METHOD PreviousWindow()

   METHOD RowColToScreenPoint( nRow, nCol )

   METHOD ChooseFont()
ENDCLASS

METHOD New( oEditor, nTop, nLeft, nRows, nColumns, lPixel ) CLASS EditorGUIDisplay

   LOCAL WindowRect, ClientRect, nExtraWidth, nExtraHeight, hDC, nStyle, hParent, cCaption, wndpl, tvins
   //LOCAL hMenu

   #ifdef WIN
      LOCAL hRecent, sRecentFile
   #endif

   //TraceLog( nRows, nColumns )

   ::oEditor := oEditor

   /*
   ::nTop    := 0
   ::nLeft   := 0
   ::nBottom := nTop + nRows
   ::nRight  := nLeft + nColumns
   */

   //TraceLog( s_nLeft, s_nTop, s_nWidth, s_nHeight )
   IF s_nShow == NIL .OR. s_nLeft == NIL .OR. s_nTop == NIL .OR. s_nRight == NIL .OR. s_nBottom == NIL
      s_nShow    := SW_SHOWNORMAL

      s_nTop     := 20
      s_nLeft    := 20
      s_nRight   := 780
      s_nBottom  := 580
   ENDIF

   IF Empty( nRows ) .OR. Empty( nColumns )
      nTop     := s_nTop
      nLeft    := s_nLeft
      nColumns := s_nRight - s_nLeft
      nRows    := s_nBottom - s_nTop
   ELSE
      s_nTop     := nTop
      s_nLeft    := nLeft
      s_nRight   := s_nLeft + nColumns
      s_nBottom  := s_nTop + nRows
   ENDIF

   ::nTop    := s_nTop
   ::nLeft   := s_nLeft
   ::nBottom := s_nRight
   ::nRight  := s_nBottom

   ::nRow        := 0
   ::nColumn     := 0
   ::nBaseLine   := 0
   ::nBaseColumn := 0

   nStyle := WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   hParent := 0

   IF oEditor == NIL
      cCaption := "xEdit"
   ELSE
      cCaption := "xEdit - " + oEditor:cPath + oEditor:cFile
   ENDIF

   IF Empty( s_hDll )
      s_hDll  := LoadLibrary( "xEdit" )
   ENDIF

   IF Empty( s_hMenu )
      IF Empty( s_hDll )
         s_hMenu := LoadMenu( s_hInstance, "XEDITCONTAINER" )
      ELSE
         s_hMenu := LoadMenu( s_hDll, "XEDITCONTAINER" )
      ENDIF
   ENDIF

   //TraceLog( s_hDll, s_hMenu )
   /*
   s_hOpen := LoadBitmap( s_hInstance, "IDB_FILE_OPEN" )
   s_hNew  := LoadBitmap( s_hInstance, "IDB_FILE_NEW" )
   s_hSave := LoadBitmap( s_hInstance, "IDB_FILE_SAVE" )
   s_hExit := LoadBitmap( s_hInstance, "IDB_FILE_EXIT" )

   hMenu := GetSubMenu( s_hMenu, 0 )
   SetMenuItemBitmaps( hMenu, IDM_FILE_OPEN, MF_BYCOMMAND, s_hOpen )
   SetMenuItemBitmaps( hMenu, IDM_FILE_NEW,  MF_BYCOMMAND, s_hNew )
   SetMenuItemBitmaps( hMenu, IDM_FILE_SAVE, MF_BYCOMMAND, s_hSave )
   SetMenuItemBitmaps( hMenu, IDM_FILE_EXIT, MF_BYCOMMAND, s_hExit )
   */

   hRecent := CreateMenu()

   FOR EACH sRecentFile IN s_aFiles
       AppendMenu( hRecent, MF_STRING, 5000 + HB_EnumIndex(), sRecentFile )
   NEXT

   DeleteMenu( GetSubMenu( s_hMenu, 0 ), 14, MF_BYPOSITION )
   InsertMenu( GetSubMenu( s_hMenu, 0 ), 14, MF_POPUP | MF_BYPOSITION, hRecent, "Recent Files" )

   hRecent := CreateMenu()

   FOR EACH sRecentFile IN s_aWorkspaces
       AppendMenu( hRecent, MF_STRING, 5100 + HB_EnumIndex(), sRecentFile )
   NEXT

   DeleteMenu( GetSubMenu( s_hMenu, 0 ), 15, MF_BYPOSITION )
   InsertMenu( GetSubMenu( s_hMenu, 0 ), 15, MF_POPUP | MF_BYPOSITION, hRecent, "Recent Workspaces" )

   IF ::wc == NIL
      ::wc := (struct WNDCLASS)

      WITH OBJECT ::wc
         :style         := CS_OWNDC | CS_DBLCLKS
         :lpfnWndProc   := DefWindowProcPointer()
         :hInstance     := s_hInstance
         //:hbrBackground := GetStockObject( WHITE_BRUSH )
         :hCursor       := LoadCursor( NIL, IDC_ARROW )
         :hIcon         := LoadIcon( s_hInstance, "Application" )
         :lpszMenuName  := "XEDITCONTAINER"
         :lpszClassName := "xEditContainer"

         RegisterClass( HB_QWith() )
      END

      WITH OBJECT ::wc
         :style         := 0
         //:lpfnWndProc   := DefWindowProcPointer()
         //:hInstance     := s_hInstance
         //:hbrBackground := GetStockObject( BLACK_BRUSH )
         :hCursor       := LoadCursor( NIL, IDC_SIZEWE )
         :lpszMenuName  := NIL
         :lpszClassName := "xEditSplitter"

         RegisterClass( HB_QWith() )
      END
   ENDIF

   //TraceLog( nTop, nLeft, nRows, nColumns )

   ::hContainer := CreateWindowEx( WS_EX_ACCEPTFILES, "xEditContainer", cCaption, nStyle, s_nLeft, s_nTop, 0, 0, hParent, 0, s_hInstance, NIL )

   IF ! Empty( s_hMenu )
      SetMenu( ::hContainer, s_hMenu )
   ENDIF

   //TraceLog( s_hInstance, ::hContainer )

   wndpl := (struct WINDOWPLACEMENT)

   wndpl:length                  := wndpl:SizeOf()
   wndpl:flags                   := 0
   wndpl:showCmd                 := s_nShow//SW_RESTORE
   //wndpl:ptMinPosition
   //wndpl:ptMaxPosition
   wndpl:rcNormalPosition:left   := s_nLeft
   wndpl:rcNormalPosition:top    := s_nTop
   wndpl:rcNormalPosition:right  := s_nRight
   wndpl:rcNormalPosition:bottom := s_nBottom

   SetWindowPlacement( ::hContainer, wndpl )

   SetWindowLong( ::hContainer, GWL_WNDPROC, WinCallbackPointer( HB_ObjMsgPtr( Self, "xEditContainerWindowProc" ), Self ) )

   ::hStatusBar := CreateWindowEx( 0, "msctls_statusbar32", "StatusBar", WS_CHILD | WS_CLIPSIBLINGS | WS_CLIPCHILDREN | WS_VISIBLE, 0, 0, s_nRight - s_nLeft, 20, ::hContainer, 0, s_hInstance, NIL )
   GetClientRect( ::hStatusBar, @ClientRect )
   ::nStatusHeight := ClientRect:bottom - ClientRect:top + 1
   //TraceLog( ::hStatusBar, ::nStatusHeight )

   GetClientRect( ::hContainer, @ClientRect )
   //TraceLog( ::hContainer, nTop, nLeft, nRows, nColumns, ClientRect:top, ClientRect:left, ClientRect:bottom, ClientRect:right )

   ::hSplitter := CreateWindowEx( 0, "xEditSplitter", NIL, WS_CHILD | WS_VISIBLE, 148, 0, 4, ClientRect:bottom - ::nStatusHeight + 1, ::hContainer, 0, s_hInstance, NIL )
   SetWindowLong( ::hSplitter, GWL_WNDPROC, WinCallbackPointer( HB_ObjMsgPtr( Self, "xEditSplitterWindowProc" ), Self ) )

   ::hFilesTree := CreateWindowEx( WS_EX_CLIENTEDGE, "SysTreeView32", "Files", TVS_DISABLEDRAGDROP | TVS_SHOWSELALWAYS | TVS_HASLINES /*| TVS_LINESATROOT */ | TVS_HASBUTTONS | WS_CHILD | WS_VISIBLE | WS_BORDER | WS_VSCROLL | WS_HSCROLL, 0, 0, 150, ClientRect:bottom - ::nStatusHeight + 1, ::hContainer, 9001, s_hInstance, NIL )

   IF ::hFilesTree != NIL
      tvins := (struct TVINSERTSTRUCT)

      tvins:hParent      := TVI_ROOT
      tvins:hInsertAfter := TVI_SORT

      tvins:item:mask        := TVIF_TEXT
      tvins:item:pszText    := Pad( "Opened Files" + Chr(0), MAX_PATH )
      tvins:item:cchTextMax := MAX_PATH + 1

      ::hTreeRoot := SendMessage( ::hFilesTree, TVM_INSERTITEM, 0, tvins )

      //::hShrinkTree := CreateWindowEx( 0, "button", "", BS_PUSHBUTTON | BS_FLAT | BS_ICON | WS_CHILD | WS_VISIBLE, 120, 5, 20, 20, ::hContainer, ID_Shrink, s_hInstance, NIL )
      //SendMessage( ::hShrinkTree, BM_SETIMAGE, IMAGE_ICON, s_hShrink )
   ENDIF

   ::nDeferDisplay++

   IF oEditor != NIL
      ::BaseLine := oEditor:CurrentLine
      ::nBaseLine := oEditor:nLine - 1
      //TraceLog( ::oEditor:cFile, oEditor:cFile )
   ENDIF

   // 150 could not haved changed since creation above!!!
   ::hWnd := CreateWindowEx( WS_EX_CLIENTEDGE, "xEdit", NIL, WS_CHILD | WS_VISIBLE | WS_BORDER | WS_TABSTOP | WS_VSCROLL | WS_HSCROLL, 150, 0, ClientRect:right - 150 + 1, ClientRect:bottom - ::nStatusHeight + 1, ::hContainer, 0, s_hInstance, Self )
   //TraceLog( ::hWnd )

   IF ::hWnd == 0
      TraceLog( "Failed!" )
      Alert( 'Sorry, failed to create an "xEdit" window!' )
      BREAK
   ENDIF

   ::lEscape := .T.

   SetWindowLong( ::hWnd, GWL_WNDPROC, WinCallbackPointer( HB_ObjMsgPtr( Self, "xEditWindowProc" ), Self ) )

   hDC := GetDC( ::hWnd )
   SelectObject( hDC, s_hFont )
   GetTextMetrics( hDC, @::TextMetric )
   ReleaseDC( ::hWnd, hDC )

   ::TextMetric:tmHeight += 2

   IF lPixel
      GetClientRect( ::hWnd, @ClientRect )

      ::nRows   :=  Int( ClientRect:bottom / ::TextMetric:tmHeight )
      ::nColumns := Int( ( ClientRect:right - ClientRect:left - LEFT_PAD ) / ::TextMetric:tmAveCharWidth )

      //TraceLog( ::nColumns, ::nRows, ClientRect:right, ClientRect:bottom )
   ELSE
      GetWindowRect( ::hContainer, @WindowRect )
      //TraceLog( WindowREct:left, WindowRect:right, WindowRect:top, WindowRect:bottom )

      GetClientRect( ::hContainer, @ClientRect )
      //TraceLog( ClientRect:right, ClientRect:bottom )

      nExtraWidth  := ( WindowRect:right - WindowRect:left ) - ClientRect:right
      nExtraWidth += LEFT_PAD + 1

      nExtraHeight := ( WindowRect:bottom - WindowRect:top ) - ClientRect:bottom

      //TraceLog( ::nRows, ::nColumns, nExtraWidth, nExtraHeight )
      MoveWindow( ::hContainer, nLeft, nTop, 150 + ::nColumns * ::TextMetric:tmAveCharWidth + nExtraWidth, ::nRows * ::TextMetric:tmHeight + nExtraHeight + ::nStatusHeight, .F. )
      //TraceLog( ::nRows, ::nColumns )
   ENDIF

   //::nFillerLength := nColumns + 1
   //::sFiller := Space( ::nFillerLength )

   //TraceLog( ::nRows, ::nColumns )

   ::nDeferDisplay--
   ASSERT( ::nDeferDisplay >= 0 )

   /* Done in ControlProcedure when receiving oEditor
   IF oEditor != NIL
      oEditor:SetDisplay( Self, .F. )
   ENDIF
   */

RETURN Self

METHOD Status( hDC, cStatus ) CLASS EditorGUIDisplay

   LOCAl sStatus := "Line:", Rect
   LOCAL bReleaseDC := .F.

   //TraceLog( ::nDeferDisplay, ::nRow )

   IF ::nDeferDisplay > 0
      RETURN Self
   ENDIF

   IF ! ( cStatus == NIL )
      SetWindowText( ::hStatusBar, cStatus )
      RETURN Self
   ENDIF

   IF GetFocus() == ::hWnd
      SetPos( ::nRow, ::nColumn )
   ENDIF

   WITH OBJECT ::oEditor
      sStatus += Str( :nLine, 5 )
      sStatus += ' Chr:'
      sStatus += Str( :nColumn, 5 )

      //sStatus += " Base Row:" + Str( ::nBaseLine, 5 ) + " Base Col:" + Str( ::nBaseColumn, 3 )

      //sStatus += " Row:"
      //sStatus += Str( ::nRow, 3 )
      //sStatus += " Col:"
      //sStatus += Str( ::nColumn, 3 )
      //sStatus += ""

      //sStatus += :CurrentLine[ ED_BUFFER ]

      //sStatus += " Undo:" + Str( Len( :aUnDo ), 3 ) + " Redo:" + Str( Len( :aRedo ) )

      SetWindowText( ::hStatusBar, sStatus )

      #ifdef VXH
         Application:SetEditorPos( :nLine, :nColumn )
      #endif
      //TraceLog( ::nRow,  ::nFocus )//, :nRecentUndo, Len( :aUnDo ), :nRecentRedo, Len( :aReDo ) )

      IF ::nRow != ::nFocus //.AND. ::nFocus >= 0
         IF hDC == NIL
            bReleaseDC := .T.
            hDC := GetDC( ::hWnd )
         ENDIF

         /*
         IF :RecentLine != NIL //.AND. ;
            //( aScan( :aUndo, {|aAction| aAction[1] & 0xFF < ED_NOIMPACT }, :nRecentUndo + 1 ) > 0 ) .OR. ;
            //( Len( :aUndo ) < :nRecentUndo .AND. aScan( :aRedo, {|aAction| aAction[1] & 0xFF < ED_NOIMPACT }, :nRecentRedo + 1 ) > 0 )
         ENDIF
         */

         :RecentLine  := :CurrentLine
         //:nRecentUndo := Len( :aUnDo )
         //:nRecentRedo := Len( :aReDo )

         // DRAWFCOUS begin.
         IF s_lFocusLine
            GetClientRect( ::hWnd, @Rect )

            IF ::nFocus >= 0
               //TraceLog( "Remove" )

               // REMOVE the previous FOCUS
               Rect:left   := LEFT_PAD - 1
               Rect:top    := ::nFocus * ::TextMetric:tmHeight
               Rect:bottom :=  Rect:top + ::TextMetric:tmHeight

               // UN-PAINT
               FrameRect( hDC, Rect, ::hBrush )
            ENDIF

            //TraceLog( ::nRow, :nLine, ::nFocus )
            ::nFocus := :nLine - ::nBaseLine - 1

            IF ::nFocus >= 0
               Rect:left   := LEFT_PAD - 1
               Rect:top    := ::nFocus * ::TextMetric:tmHeight
               Rect:bottom :=  Rect:top + ::TextMetric:tmHeight

               // PAINT
               SetTextColor( hDC, :ExtensionColors[ "Text" ] )
               //TraceLog( "DRAWFOCUS" )
               DrawFocusRect( hDC, Rect )
            ENDIF

            IF bReleaseDC
               ReleaseDC( ::hWnd, hDC )
            ENDIF
         ENDIF // DRAWFCOUS end.
      ENDIF
   END

RETURN Self

PROCEDURE TextOutColorizeLOGTokens( hDC, nX, nY, sLine, nCharWidth, nOffset )

   LOCAL aMatch, cToken, nIndex := 1, nLen, nLens := 0
   LOCAl oError

   IF Empty( sLine ) .OR. nOffset >= Len( sLine )
      //TextOut( hDC, nX, nY, sFiller, nFillerLength )
      RETURN
   ENDIF

   /*
   IF sLine = "<<<"
      //TextOut( hDC, nX, nY, sFiller, nFillerLength )
      RETURN
   ENDIF
   */

   BEGIN SEQUENCE
      WHILE sLine[nIndex] == ' '
         nIndex++
      END
      nIndex--

      IF nIndex > nOffset
         nIndex -= nOffset
         nOffset := 0
         TextOut( hDC, nX, nY, Space( nIndex ), nIndex )
         nX += ( nIndex * nCharWidth )
         nLens += nIndex
      ELSE
         nOffset -= nIndex
      ENDIF

      sLine := LTrim( sLine )

      aMatch := HB_Regex( s_TraceLog, sLine )

      IF Empty( aMatch )
         aMatch := HB_Regex( s_ErrorLine, sLine )

         IF Empty( aMatch )
            aMatch := HB_Regex( s_WarningLine, sLine )
         ENDIF
      ELSE
         cToken := aMatch[2]
         nLen := Len( cToken )

         IF cToken = "Couldn't"
            SetTextColor( hDC, s_LogColors[ "Couldn't" ] )
         ELSEIF cToken = "[In use?]"
            SetTextColor( hDC, s_LogColors[ "Couldn't" ] )
         ELSE
            //BREAK
            SetTextColor( hDC, s_LogColors[ "Text" ] )
         ENDIF

         IF nLens >= nOffset
            TextOut( hDC, nX, nY, cToken, nLen )
         ELSEIF nLens + nLen > nOffset
            nLen := nLens + nLen - nOffset
            cToken := Right( cToken, nLen )
            TextOut( hDC, nX, nY, cToken, nLen )
         ENDIF

         BREAK
      ENDIF

      IF Empty( aMatch )
         SetTextColor( hDC, s_LogColors[ "Text" ] )

         cToken := sLine
         nLen := Len( sLine )

         IF nLens >= nOffset
            TextOut( hDC, nX, nY, cToken, nLen )
         ELSEIF nLens + nLen > nOffset
            nLen := nLens + nLen - nOffset
            cToken := Right( cToken, nLen )
            TextOut( hDC, nX, nY, cToken, nLen )
         ENDIF

         BREAK
      ELSE
         IF aMatch[4] = "Error" .OR. aMatch[4] = "error:"
            SetTextColor( hDC, s_LogColors[ "Couldn't" ] )

            cToken := "! "
            nLen := 2

            IF nLens >= nOffset
               TextOut( hDC, nX, nY, cToken, nLen )
               nX += ( nLen * nCharWidth )
            ELSEIF nLens + nLen > nOffset
               nLen := nLens + nLen - nOffset
               cToken := Right( cToken, nLen )
               TextOut( hDC, nX, nY, cToken, nLen )
               nX += ( nLen * nCharWidth )
               nLens := nOffset + nLen
            ELSE
               nLens += nLen
            ENDIF
         ENDIF

         SetTextColor( hDC, s_LogColors[ "Module" ] )

         cToken := aMatch[2] + " "
         nLen   := Len( cToken )

         IF nLens >= nOffset
            TextOut( hDC, nX, nY, cToken, nLen )
            nX += ( nLen * nCharWidth )
         ELSEIF nLens + nLen > nOffset
            nLen := nLens + nLen - nOffset
            cToken := Right( cToken, nLen )
            TextOut( hDC, nX, nY, cToken, nLen )
            nX += ( nLen * nCharWidth )
            nLens := nOffset + nLen
         ELSE
            nLens += nLen
         ENDIF

         SetTextColor( hDC, s_LogColors[ "Line" ] )

         cToken := aMatch[3] + " "
         nLen   := Len( cToken )

         IF nLens >= nOffset
            TextOut( hDC, nX, nY, cToken, nLen )
            nX += ( nLen * nCharWidth )
         ELSEIF nLens + nLen > nOffset
            nLen := nLens + nLen - nOffset
            cToken := Right( cToken, nLen )
            TextOut( hDC, nX, nY, cToken, nLen )
            nX += ( nLen * nCharWidth )
            nLens := nOffset + nLen
         ELSE
            nLens += nLen
         ENDIF

         SetTextColor( hDC, s_LogColors[ "Error" ] )

         cToken := aMatch[4] + " "
         nLen   := Len( cToken )

         IF nLens >= nOffset
            TextOut( hDC, nX, nY, cToken, nLen )
            nX += ( nLen * nCharWidth )
         ELSEIF nLens + nLen > nOffset
            nLen := nLens + nLen - nOffset
            cToken := Right( cToken, nLen )
            TextOut( hDC, nX, nY, cToken, nLen )
            nX += ( nLen * nCharWidth )
            nLens := nOffset + nLen
         ELSE
            nLens += nLen
         ENDIF

         SetTextColor( hDC, s_LogColors[ "Description" ] )

         cToken := aMatch[5] + " "
         nLen   := Len( cToken )

         IF nLens >= nOffset
            TextOut( hDC, nX, nY, cToken, nLen )
            nX += ( nLen * nCharWidth )
         ELSEIF nLens + nLen > nOffset
            nLen := nLens + nLen - nOffset
            cToken := Right( cToken, nLen )
            TextOut( hDC, nX, nY, cToken, nLen )
            nX += ( nLen * nCharWidth )
            nLens := nOffset + nLen
         ELSE
            nLens += nLen
         ENDIF

      ENDIF

   RECOVER USING oError
      //TraceLog( ValToPrg( oError ) )
      nX += ( nLen * nCharWidth )
   END SEQUENCE

   //TextOut( hDC, nX, nY, sFiller, nFillerLength )

RETURN

PROCEDURE TextOutColorizePRGTokens( hDC, nX, nY, sLine, nCharWidth, nOffset, CustomColors, oEditor )

   LOCAL cToken, nIndex := 1, nLen, nLens := 0
   LOCAL nComment := 0
   //LOCAL oError

   //TraceLog( sLine, nOffset )

   IF Empty( sLine ) .OR. nOffset >= Len( sLine )
      RETURN
   ENDIF

   WITH OBJECT oEditor
      BEGIN SEQUENCE
         WHILE sLine[nIndex] == ' '
            nIndex++
         END
         nIndex--

         IF nIndex > nOffset
            nIndex -= nOffset
            nOffset := 0
            TextOut( hDC, nX, nY, Space( nIndex ), nIndex )
            nX += ( nIndex * nCharWidth )
            nLens += nIndex
         ELSE
            nOffset -= nIndex
         ENDIF

         sLine := LTrim( sLine )

         nLen := At( "*/", sLine )
         IF nLen > 0 .AND. :nPhysicalLine - 1 == :nLastCloser
            SetTextColor( hDC, CustomColors[ "Comments" ] )
            nLen += 2

            WHILE sLine[nLen] == ' '
               nLen++
            END

            nLen--

            cToken := Left( sLine, nLen )
            sLine := SubStr( sLine, nLen + 1 )
            //TraceLog( cToken, sLine )
         ELSE
            cToken := HB_AtX( s_PRG_KeyWord, sLine, , , @nLen )

            IF nLen > 0
               SetTextColor( hDC, CustomColors[ "Keywords" ] )
               sLine := SubStr( sLine, nLen + 1 )
            ELSE
               cToken := NextToken( @sLine )
               nLen := Len( cToken )

               IF cToken[1] == '*'
                  SetTextColor( hDC, CustomColors[ "Comments" ] )
                  nLen := Len( cToken ) + Len( sLine ) - nOffset
                  TextOut( hDC, nX, nY, SubStr( cToken + sLine, nOffset + 1 ), nLen )
                  BREAK
               ELSEIF cToken = "/*" .OR. cToken = '//' .OR. cToken = '&&'
                  SetTextColor( hDC, CustomColors[ "Comments" ] )
                  nLen := Len( cToken ) + Len( sLine ) - nOffset
                  TextOut( hDC, nX, nY, SubStr( cToken + sLine, nOffset + 1 ), nLen )
                  BREAK
               ELSEIF cToken[1] == '#'
                  SetTextColor( hDC, CustomColors[ "Directives" ] )
                  nLen := Len( cToken ) + Len( sLine ) - nOffset
                  TextOut( hDC, nX, nY, SubStr( cToken + sLine, nOffset + 1 ), nLen )
                  BREAK
               ENDIF

               IF HB_RegExMatch( s_PRG_Operator, cToken, .T. )
                  SetTextColor( hDC, CustomColors[ "Operators" ] )
               ELSEIF HB_RegExMatch( s_PRG_Literal, cToken, .T. )
                  SetTextColor( hDC, CustomColors[ "Literals" ] )
               ELSEIF cToken[1] $ ["'] // fake quote closing some editors don't change colors until quotes are closed"
                  cToken += sLine
                  sLine := ""
                  nLen := Len( cToken )
                  SetTextColor( hDC, CustomColors[ "Open Literals" ] )
               ELSEIF sLine[1] == '(' .AND. '|' + Lower( cToken ) + '|' IN s_SysFunc
                  SetTextColor( hDC, CustomColors[ "API" ] )
               ELSEIF cToken = "/*"
                  SetTextColor( hDC, CustomColors[ "Comments" ] )
               ELSE
                  SetTextColor( hDC, CustomColors[ "Text" ] )
               ENDIF
            ENDIF
         ENDIF

         //TraceLog( nLen, nLens, nOffset, cToken )

         IF nLens >= nOffset
            TextOut( hDC, nX, nY, cToken, nLen )
            nX += ( nLen * nCharWidth )
         ELSEIF nLens + nLen > nOffset
            nLen := nLens + nLen - nOffset
            cToken := Right( cToken, nLen )
            TextOut( hDC, nX, nY, cToken, nLen )
            nX += ( nLen * nCharWidth )
            nLens := nOffset + nLen
         ELSE
            nLens += nLen
         ENDIF

         WHILE ! Empty( cToken := NextToken( @sLine ) )
            nLen := Len( cToken )

            IF HB_RegExMatch( s_PRG_Word, cToken, .T. )
               SetTextColor( hDC, CustomColors[ "Reserved" ] )
            ELSEIF HB_RegExMatch( s_PRG_Literal, cToken, .T. )
               SetTextColor( hDC, CustomColors[ "Literals" ] )
            ELSEIF cToken[1] $ ["'] //"
               cToken += sLine
               sLine := ""
               nLen := Len( cToken )
               SetTextColor( hDC, CustomColors[ "Open Literals" ] )
            ELSEIF HB_RegExMatch( s_PRG_Operator, cToken, .T. )
               SetTextColor( hDC, CustomColors[ "Operators" ] )
            ELSEIF sLine[1] == '(' .AND. '|' + Lower( cToken ) + '|' IN s_SysFunc
               SetTextColor( hDC, CustomColors[ "API" ] )
            ELSEIF cToken = "/*" .OR. cToken = '//' .OR. cToken = '&&'
               SetTextColor( hDC, CustomColors[ "Comments" ] )
            ELSE
               SetTextColor( hDC, CustomColors[ "Text" ] )
            ENDIF

            IF nLens >= nOffset
               TextOut( hDC, nX, nY, cToken, nLen )
               nX += ( nLen * nCharWidth )
               //nLens += nLen // No longer needed here
            ELSEIF nLens + nLen > nOffset
               nLen := nLens + nLen - nOffset
               cToken := Right( cToken, nLen )
               TextOut( hDC, nX, nY, cToken, nLen )
               nX += ( nLen * nCharWidth )
               nLens := nOffset //+ nLen No longer needed
            ELSE
               nLens += nLen
            ENDIF
         END
      RECOVER //USING oError
         nX += ( nLen * nCharWidth )
         //TraceLog( ValToPrg( oError ) )
      END SEQUENCE
   END

RETURN

METHOD Display( hDC, nLine ) CLASS EditorGUIDisplay

   LOCAL Line, nRow, nRows, tmHeight, tmAveCharWidth, nBaseColumn
   LOCAL nEditorLine, nOpen := 0, aComment, nDump := 0, aDump
   LOCAL bReleaseDC := .F.
   LOCAL si
   LOCAL sLine, nLen, nX, nY
   //LOCAL sFiller := ::sFiller, nFillerLength := ::nFillerLength
   LOCAL Rect
   LOCAL nFunc, nHiddenLines, aFunc, nDumpLine
   LOCAL IconRect := (struct RECT)
   LOCAL nAt, nOpenAt, nCloseAt
   //LOCAL nPhysicalLine
   LOCAL nLastOpener := 0, nLastCloser := 0
   LOCAL hDirectDC, hBitmap, hWnd := ::hWnd
   LOCAL HighlightedLine

   IF ::nDeferDisplay > 0
      //::nDeferDisplay++
      //TraceLog( ::oEditor:CurrentLine[ ED_BUFFER ], ::oEditor:nLine, nRow, ::nBaseLine, ::BaseLine )
      ::GoRow( ::nRow )
      //TraceLog( ::oEditor:CurrentLine[ ED_BUFFER ] )
      RETURN Self
   ENDIF

   nRow := 0
   tmHeight := ::TextMetric:tmHeight
   tmAveCharWidth := ::TextMetric:tmAveCharWidth

   //TraceLog( ::nDeferDisplay, ::nRows, ::nBaseLine, tmHeight, ::nRow, nLine )

   IF GetFocus() == hWnd .AND. ::nRow >= 0 .AND. ::nColumn >= 0
      //TraceLog( "Hid" )
      HideCaret( hWnd )
   ENDIF

   WITH OBJECT ::oEditor
      HighlightedLine := :HighlightedLine

      IF hDC == NIL
         bReleaseDC := .T.
         hDC := GetDC( hWnd )
      ENDIF

      hDirectDC := hDC

      IF nLine == NIL
         GetClientRect( hWnd, @Rect )

         hDC := CreateCompatibleDC( hDirectDC )
         hBitmap := CreateCompatibleBitmap( hDirectDC, Rect:right, Rect:bottom )
         SelectObject( hDC, hBitmap )
         SelectObject( hDC, s_hFont )

         nRows   := Int( ( Rect:bottom /*- ::nStatusHeight*/ ) / ::TextMetric:tmHeight )
         ::nRows := nRows

         //Residual at bottom of Edit Rect
         Rect:left    := LEFT_PAD - 1
         Rect:top     := ::nRows * tmHeight
         FillRect( hDC, Rect, ::hBrush )

         // Left PAD Rect
         Rect:left    := 0
         Rect:top     := 0
         Rect:right   := LEFT_PAD - 1

         FillRect( hDC, Rect, ::hPadBrush )
      ELSE
         nRows := ::nRows
      ENDIF

      SetBKColor( hDC, :hBKColor )

      Line := ::BaseLine
      nEditorLine := ::nBaseLine + 1

      nBaseColumn := ::nBaseColumn

      GetClientRect( hWnd, @Rect )
      Rect:left := LEFT_PAD - 1
      //Rect:right--

      :nLastOpener := 0
      :nLastCloser := 0

      WHILE nRow < nRows
         //TraceLog( nRow, nRows, Line, nEditorLine )

         nX := LEFT_PAD
         nY := nRow * tmHeight + 1

         IF nLine == NIL
            Rect:top  := nY - 1
            Rect:bottom := Rect:top + tmHeight

            FillRect( hDC, Rect, ::hBrush )
         ELSEIF nLine == nRow
            Rect:left := LEFT_PAD
            Rect:top  := nY
            Rect:right--
            Rect:bottom := Rect:top + tmHeight - 2

            FillRect( hDC, Rect, ::hBrush )
         ENDIF

         IF Line == NIL
            nRow++
            LOOP
         ENDIF

         IF nLine == NIL .OR. nLine == nRow
            IconRect:left   := 0
            IconRect:top    := nY - 1
            IconRect:right  := LEFT_PAD - 1
            IconRect:bottom := IconRect:top + tmHeight

            IF Line[ ED_MODIFICATIONS ] == 0
               FillRect( hDC, IconRect, ::hPadBrush )
            ELSE
               FillRect( hDC, IconRect, ::hHighlightBrush )
            ENDIF

            //TraceLog( "IconRect" )
         ENDIF

         nFunc        := 0
         nHiddenLines := 0

         FOR EACH aFunc IN :aFunctions
            IF aFunc[3] == Line
               nFunc := HB_EnumIndex()
               EXIT
            ENDIF

            IF aFunc[1] > nEditorLine + nHiddenLines
               EXIT
            ENDIF

            IF Len( aFunc[3] ) > ED_BASE_LEN
               nHiddenLines += aFunc[3][ ED_COLLAPSED_LINES ]
            ENDIF
         NEXT

         :nPhysicalLine := nEditorLine + nHiddenLines

         //TraceLog( nOpen, nDump, :pColorizer, :aComments )

         IF nOpen == 0 .AND. nDump == 0 .AND. :pColorizer != NIL
            FOR EACH aComment IN :aComments
               //TraceLog( aComment[1], aComment[2] )

               IF aComment[1] == 0
                  LOOP
               ENDIF

               IF aComment[1] > :nPhysicalLine
                  EXIT
               ENDIF

               IF aComment[1] == nLastOpener .OR. aComment[1] < nLastCloser
                  //TraceLog( "Skip", aComment[1], aComment[2], :nLastOpener, :nLastCloser )
                  LOOP
               ENDIF

               //Tracelog( "Testing", :nPhysicalLine, nEditorLine, aComment[1], aComment[2], :nLastOpener, :nLastCloser )

               IF aComment[1] == :nPhysicalLine .OR. ( aComment[1] < :nPhysicalLine .AND. ( :nPhysicalLine <= aComment[2] .OR. aComment[2] == 0 ) )
                  //TraceLog( "Comment", HB_EnumIndex(), nEditorLine, :nPhysicalLine, aComment[1], aComment[2], :nLastOpener, :nLastCloser )

                  nOpen := HB_EnumIndex()

                  IF aComment[2] == 0 .OR. aComment[2] >= aComment[1]
                     SetTextColor( hDC, s_PrgColors[ "Comments" ] )
                  ENDIF

                  :nLastOpener  := aComment[1]
                  :nLastCloser  := aComment[2]

                  EXIT
               ENDIF

               nLastOpener  := aComment[1]
               nLastCloser  := aComment[2]
            NEXT

            IF nOpen == 0
               FOR EACH aDump IN :aDumps
                  nDumpLine := aDump[1]

                  IF nDumpLine > :nPhysicalLine
                     EXIT
                  ELSEIF nDumpLine <= :nPhysicalLine .AND. ( :nPhysicalLine <= aDump[2] .OR. aDump[2] == 0 )
                     nDump := HB_EnumIndex()
                     SetTextColor( hDC, s_PrgColors[ "C Code" ] )
                     EXIT
                  ENDIF
               NEXT
            ENDIF
         ENDIF

         IF Line == HighlightedLine .AND. ( nLine == NIL .OR.  nLine == nRow )
            Rect:left := LEFT_PAD
            Rect:top  := nY
            Rect:bottom := Rect:top + tmHeight - 2

            FillRect( hDC, Rect, ::hHighlightBrush )
            SetBKColor( hDC, :hHighlightColor )

            Rect:left := LEFT_PAD - 1
         ENDIF

         IF nOpen > 0
            IF nLine == NIL .OR. nLine == nRow
               sLine := SubStr( Line[ ED_BUFFER ], nBaseColumn + 1 )
               nLen := Len( sLine )
               TextOut( hDC, nX, nY, sLine, nLen )
               //nX += ( nLen * tmAveCharWidth )
               //TextOut( hDC, nX, nY, sFiller, nFillerLength )
            ENDIF

            IF :nLastCloser > 0 .AND. :nLastCloser <= :nPhysicalLine
               nOpen := 0
               SetTextColor( hDC, s_PrgColors[ "Text" ] )
            //ELSE
            //   TraceLog( :nPhysicalLine, nEditorLine, nOpen, :aComments[nOpen][1], :aComments[nOpen][2] )
            ENDIF
         ELSEIF nDump > 0
            IF nLine == NIL .OR. nLine == nRow
               sLine := SubStr( Line[ ED_BUFFER ], nBaseColumn + 1 )
               nLen := Len( sLine )
               TextOut( hDC, nX, nY, sLine, nLen )
               //nX += ( nLen * tmAveCharWidth )
               //TextOut( hDC, nX, nY, sFiller, nFillerLength )
            ENDIF

            IF :aDumps[nDump][2] == :nPhysicalLine
               nDump := 0
               SetTextColor( hDC, s_PrgColors[ "Text" ] )
            ENDIF
         ELSEIF :pColorizer != NIL
            IF nLine == NIL .OR. nLine == nRow
               //TraceLog( nEditorLine, Line[ ED_BUFFER ] )
               HB_Exec( :pColorizer, , hDC, nX, nY, Line[ ED_BUFFER ], tmAveCharWidth, nBaseColumn, :ExtensionColors, HB_QWith() )

               IF nLine == NIL
                  IF nFunc > 0
                     IF Len( Line ) == ED_BASE_LEN
                        DrawIconEx( hDC, 2, nY + 1, s_hMinus, ( LEFT_PAD >> 1 ) + 1, ( LEFT_PAD >> 1 ) + 1, NIL, NIL, DI_NORMAL )
                     ELSE
                        DrawIconEx( hDC, 2, nY + 1, s_hPlus, ( LEFT_PAD >> 1 ) + 1, ( LEFT_PAD >> 1 ) + 1, NIL, NIL, DI_NORMAL )
                     ENDIF
                  ENDIF
               ELSE
                  IF nFunc == 0
                     IF Line[ ED_BUFFER ] HAS s_Func .OR. Line[ ED_BUFFER ] HAS s_Method .OR. Line[ ED_BUFFER ] HAS s_Method2 .OR. ( Line[ ED_BUFFER ] HAS s_Class .AND. ! Line[ ED_BUFFER ] HAS s_ClassVar )
                        aAdd( :aFunctions, { :nLine, /*TODO*/, Line } )
                        aSort( :aFunctions, , , {|_1, _2| _1[1] < _2[1] } )

                        DrawIconEx( hDC, 2, nY + 1, s_hMinus, ( LEFT_PAD >> 1 ) + 1, ( LEFT_PAD >> 1 ) + 1, NIL, NIL, DI_NORMAL )
                        //TraceLog( "Like" )
                     ENDIF
                  ELSE
                     IF Line[ ED_BUFFER ] HAS s_Func .OR. Line[ ED_BUFFER ] HAS s_Method .OR. Line[ ED_BUFFER ] HAS s_Method2 .OR. ( Line[ ED_BUFFER ] HAS s_Class .AND. ! Line[ ED_BUFFER ] HAS s_ClassVar )
                        IF Len( Line ) == ED_BASE_LEN
                           DrawIconEx( hDC, 2, nY + 1, s_hMinus, ( LEFT_PAD >> 1 ) + 1, ( LEFT_PAD >> 1 ) + 1, NIL, NIL, DI_NORMAL )
                        ELSE
                           DrawIconEx( hDC, 2, nY + 1, s_hPlus, ( LEFT_PAD >> 1 ) + 1, ( LEFT_PAD >> 1 ) + 1, NIL, NIL, DI_NORMAL )
                        ENDIF
                        //TraceLog( "Like" )
                     ELSE
                        aDel( :aFunctions, nFunc, .T. )

                        IconRect:left   := 2
                        IconRect:top    := nY + 1
                        IconRect:right  := IconRect:left + ( LEFT_PAD >> 1 ) + 1
                        IconRect:bottom := IconRect:top  + ( LEFT_PAD >> 1 ) + 1

                        IF Line[ ED_MODIFICATIONS ] == 0
                           FillRect( hDC, IconRect, ::hPadBrush )
                        ELSE
                           FillRect( hDC, IconRect, ::hHighlightBrush )
                        ENDIF

                        //TraceLog( "Deleted" )
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            IF nLine == NIL .OR. nLine == nRow
               sLine := SubStr( Line[ ED_BUFFER ], nBaseColumn + 1 )
               nLen := Len( sLine )
               SetTextColor( hDC, s_DefaultColors[ "Text" ] )
               TextOut( hDC, nX, nY, sLine, nLen )
               //nX += ( nLen * tmAveCharWidth )
               //TextOut( hDC, nX, nY, sFiller, nFillerLength )
            ENDIF
         ENDIF

         IF aScan( :aBookmarks, {|aBookmark| aBookmark[2] == Line } ) > 0
            //SetTextColor( hDC, s_Colors[ "Red" ] )
            //TextOut( hDC, 0, nY, "#", 1 )
            DrawIconEx( hDC, 2, nY + 1, s_hBookmark, ( LEFT_PAD ) - 2, ( LEFT_PAD ) - 3, NIL, NIL, DI_NORMAL )
         ENDIF

         IF nLine == nRow
            //TraceLog( nLine )

            nAt     := 0
            nOpenAt := 0

            WHILE .T.
               nAt  := AtSkipStrings( "/*", Line[ ED_BUFFER ], nAt + 1 )

               IF nAt > 0
                  nOpenAt := nAt
               ELSE
                  EXIT
               ENDIF
            END

            nAt      := 0
            nCloseAt := 0

            WHILE .T.
               nAt  := At( "*/", Line[ ED_BUFFER ], nAt + 1 )

               IF nAt > 0
                  nCloseAt := nAt
               ELSE
                  EXIT
               ENDIF
            END

            HB_AtX( s_InlineComment, Line[ ED_BUFFER ], , @nAt )
            IF nAt > 0 .AND. nAt < nOpenAt
               nOpenAt := 0
            ENDIF

            //TraceLog( Line[ED_BUFFER], nAt )

            IF nOpenAt > 0 .AND. ( nCloseAt == 0 .OR. nOpenAt > nCloseAt + 1 )
               :AddCommentOpener( :nPhysicalLine, hDC )

               // Left most opener
               //nOpenAt  := AtSkipStrings( "/*", Line[ ED_BUFFER ] )
            ELSE
               :DelCommentOpener( :nPhysicalLine, hDC )
            ENDIF

            nCloseAt := At( "*/", Line[ ED_BUFFER ] )

            //TraceLog( :nPhysicalLine, Line[ ED_BUFFER ], nOpenAt, nCloseAt )

            IF nCloseAt > 0 //.AND. ( nOpenAt == 0 .OR. nCloseAt < nOpenAt )
               :AddCommentCloser( :nPhysicalLine, hDC )
            ELSE
               :DelCommentCloser( :nPhysicalLine, hDC )
            ENDIF
         ENDIF

         IF Line == HighlightedLine
            SetBKColor( hDC, :hBKColor )
         ENDIF

         Line := Line[ ED_NEXTLINE ]
         nEditorLine++
         nRow++
      END

      IF bReleaseDC
         si IS SCROLLINFO

         WITH OBJECT si
            :cbSize := :SizeOf
            :fMask := SIF_ALL
            :nMin := 0
            :nMax := Max( ::nBaseLine + ::nRows - 1, ::oEditor:nLines - 1 )
            :nPage := nRows
            :nPos := ::nBaseLine
            :nTrackPos := 0
         END
         //TraceLog( si:nPos, si:nPage, nRows, :nLines, si:nMax )

         SetScrollInfo( hWnd, SB_VERT, si, .T. )

         WITH OBJECT si
            //:cbSize := :SizeOf
            //:fMask := SIF_ALL
            //:nMin := 0
            :nMax  := ::oEditor:nLongestLine - 1
            :nPage := ::nColumns
            :nPos  := ::nBaseColumn
            :nTrackPos := 0
         END
         //TraceLog( si:nPos, si:nPage, nRows, :nLines, si:nMax )

         SetScrollInfo( hWnd, SB_HORZ, si, .T. )
      ENDIF

      //TraceLog( nLine, ::nRow, ::nColumn, bReleaseDC )

      IF nLine == NIL
         ::ShowSelection( , , , , , hDC )

         IF ::nColumn == -1 .AND. :nColumn > ::nBaseColumn .AND. :nColumn <= ::nBaseColumn + ::nColumns
            ::nColumn := :nColumn - ::nBaseColumn - 1
         ENDIF

         IF ::nRow == -1 .AND. :nLine > ::nBaseLine .AND. :nLine <= ::nBaseLine + ::nRows
            ::nRow := :nLine - ::nBaseLine - 1
         ENDIF

         IF ::nRow >= 0 //.AND. ::nColumn >= 0
            // Always force DrawFocus after full paint.
            IF bReleaseDC
               ::nFocus := -1
               ::GoRow( ::nRow, hDC )
            ELSE
               IF s_lFocusLine
                  Rect:left   := LEFT_PAD - 1
                  Rect:top    := ::nRow * ::TextMetric:tmHeight
                  Rect:bottom := Rect:top + ::TextMetric:tmHeight

                  // PAINT
                  SetTextColor( hDC, :ExtensionColors[ "Text" ] )
                  //TraceLog( "DRAWFOCUS" )
                  DrawFocusRect( hDC, Rect )
               ENDIF

               ::nFocus    := ::nRow
               :RecentLine := :CurrentLine
            ENDIF
         ENDIF

         GetClientRect( hWnd, @Rect )
         //TraceLog( hDirectDc, hDC, Rect:right, Rect:bottom )
         BitBlt( hDirectDC, 0, 0, Rect:right, Rect:bottom, hDC, 0, 0, SRCCOPY )
         DeleteDC( hDC )
         DeleteObject( hBitmap )
      ELSE
         ::ShowSelection( , , , , ::nBaseLine + nLine + 1, hDC )
      ENDIF
   END

   IF GetFocus() == hWnd .AND. ::nRow >= 0 .AND. ::nColumn >= 0
      //TraceLog( "Show" )
      ShowCaret( hWnd )
   ENDIF

   IF bReleaseDC
      ReleaseDC( hWnd, hDirectDC )
   ENDIF

RETURN Self

METHOD xEditSplitterWindowProc( hWnd, nMsg, nwParam, nlParam ) CLASS EditorGUIDisplay

   LOCAL Point, Rect

   //TraceLog( nMsg )

   SWITCH nMsg
      CASE WM_LBUTTONDOWN
         SetCapture( hWnd )
         RETURN 0
         //EXIT

      CASE WM_LBUTTONUP
         ReleaseCapture()
         RETURN 0
         //EXIT

      CASE WM_MOUSEMOVE
         IF nwParam & MK_LBUTTON != 0
            Point := (struct POINT)

            Point:y := HIWORD( nlParam )
            Point:x := LOWORD( nlParam )
            IF Point:x > 32768
               Point:x -= 65535
            ENDIF

            MapWindowPoints( hWnd, ::hContainer, @Point, 1 )

            GetClientRect( ::hContainer, @Rect )
            IF Point:x < 25 .OR. Point:x > Rect:right - 50
               RETURN 0
            ENDIF

            IF ::hSplitter != NIL
               MoveWindow( ::hSplitter, Point:x - 2, 0, 4, Rect:bottom - ::nStatusHeight + 1, .F. )
            ENDIF
            IF ::hFilesTree != NIL
               MoveWindow( ::hFilesTree, 0, 0, Point:X, Rect:bottom - ::nStatusHeight + 1, .T. )
            ENDIF
            IF ::hWnd != NIL
               MoveWindow( ::hWnd, Point:x, 0, Rect:right - Point:x + 1, Rect:bottom - ::nStatusHeight + 1, .T. )
            ENDIF

            ::Display()
         ENDIF
         RETURN 0
         //EXIT
   END

RETURN DefWindowProc( hWnd, nMsg, nwParam, nlParam )

METHOD xEditFinderWindowProc( hWnd, nMsg, nwParam, nlParam ) CLASS EditorGUIDisplay

   LOCAL Notification
   //TraceLog( hWnd, nMsg, s_hActiveDialog )

   SWITCH nMsg
      //CASE WM_CREATE
      //   NEVER gets here, because Windw Procedure is set after creation!!!
      //   RETURN 0

      //CASE WM_PAINT
      //   RETURN 0

      CASE WM_SETFOCUS
         SetFocus( s_hActiveDialog )
         RETURN 0

      CASE WM_COMMAND
         SendMessage( s_hActiveDialog, nMsg, nwParam, nlParam )
         RETURN 0

      CASE WM_NOTIFY
         Notification := (struct NMHDR)
         Notification:Pointer( nlParam )

         //TraceLog( Notification:Code )

         SWITCH Notification:Code
            CASE TCN_SELCHANGE
               SWITCH SendMessage( ::hFindTabControl, TCM_GETCURSEL, 0, 0 )
                  CASE 0
                     ShowWindow( ::hGotoDialog, SW_HIDE)
                     s_hActiveDialog := ::hFindDialog
                     ShowWindow( ::hFindDialog, SW_SHOW )

                     ShowWindow( GetDlgItem( ::hFindDialog, ID_RepLabel, SW_HIDE ) )
                     ShowWindow( GetDlgItem( ::hFindDialog, ID_Replace , SW_HIDE ) )
                     ShowWindow( GetDlgItem( ::hFindDialog, ID_Once    , SW_HIDE ) )
                     ShowWindow( GetDlgItem( ::hFindDialog, ID_All     , SW_HIDE ) )
                     RETURN 0

                  CASE 1
                     ShowWindow( ::hGotoDialog, SW_HIDE)
                     s_hActiveDialog := ::hFindDialog
                     ShowWindow( ::hFindDialog, SW_SHOW )

                     ShowWindow( GetDlgItem( ::hFindDialog, ID_RepLabel ), SW_SHOW )
                     ShowWindow( GetDlgItem( ::hFindDialog, ID_Replace  ), SW_SHOW )
                     ShowWindow( GetDlgItem( ::hFindDialog, ID_Once     ), SW_SHOW )
                     ShowWindow( GetDlgItem( ::hFindDialog, ID_All      ), SW_SHOW )
                     RETURN 0

                  CASE 2
                     ShowWindow( ::hFindDialog, SW_HIDE )
                     s_hActiveDialog := ::hGotoDialog
                     ShowWindow( ::hGotoDialog, SW_SHOW )
                     SetFocus( ::hGotoDialog )
                     RETURN 0

                  DEFAULT
                     TraceLog( "Oops! Unexcpected case." )
               END
         END
         EXIT

      CASE WM_CLOSE
         ShowWindow( hWnd, SW_HIDE )
         RETURN 0
   END

RETURN DefWindowProc( hWnd, nMsg, nwParam, nlParam )

METHOD xEditFindDialogProc( hWnd, nMsg, nwParam, nlParam ) CLASS EditorGUIDisplay

   STATIC oGlobalStart

   LOCAL sFind, nFind, RegEx, oError, aFound
   LOCAL lReplace, sReplace, lMore
   LOCAL nLineFrom, nColumnFrom, nLineTo, nColumnTo, lSquare, nDirection
   LOCAL aActions := {}, nFound := 0., nPos
   LOCAL lGlobal := IsDlgButtonChecked( hWnd, ID_Global ) & BST_CHECKED != 0
   #ifndef VXH
    LOCAL Point, nX, nY
   #endif


   (nlParam)

   SWITCH nMsg
      //CASE WM_PAINT
      //   RETURN 0

      CASE WM_SETFOCUS
         IF Len( s_aEditors ) > 1
            EnableWindow( GetDlgItem( hWnd, ID_Global ), .T. )
         ELSE
            CheckDlgButton( hWnd, ID_Global, BST_UNCHECKED )
            EnableWindow( GetDlgItem( hWnd, ID_Global ), .F. )
         ENDIF

         IF ::oEditor:nLineFrom == 0
            CheckDlgButton( hWnd, ID_Selected, BST_UNCHECKED )
            EnableWindow( GetDlgItem( hWnd, ID_Selected ), .F. )
         ELSE
            EnableWindow( GetDlgItem( hWnd, ID_Selected ), .T. )

            IF ::oEditor:nLineFrom > 0 .AND. ::oEditor:nLineFrom == ::oEditor:nLineTo
               SetDlgItemText( hWnd, ID_Find, ::oEditor:SelectedText() )
               CheckDlgButton( hWnd, ID_Selected, BST_UNCHECKED )
            ELSE
               CheckDlgButton( hWnd, ID_Selected, BST_CHECKED )
            ENDIF
         ENDIF
         RETURN 0

      CASE WM_COMMAND
         IF HIWORD( nwParam ) == BN_CLICKED
            SWITCH LOWORD( nwParam )
                CASE ID_MatchCase
                   IF IsDlgButtonChecked( hWnd, ID_MatchCase ) & BST_CHECKED != 0
                      CheckDlgButton( hWnd, ID_RegEx, BST_UNCHECKED )
                   ENDIF
                   RETURN 0

                CASE ID_WholeWord
                   IF IsDlgButtonChecked( hWnd, ID_WholeWord ) & BST_CHECKED != 0
                      CheckDlgButton( hWnd, ID_RegEx, BST_UNCHECKED )
                   ENDIF
                   RETURN 0

                CASE ID_RegEx
                   IF IsDlgButtonChecked( hWnd, ID_RegEx ) & BST_CHECKED != 0
                      CheckDlgButton( hWnd, ID_MatchCase, BST_UNCHECKED )
                      CheckDlgButton( hWnd, ID_WholeWord, BST_UNCHECKED )
                   ENDIF
                   RETURN 0

                CASE ID_Top
                   IF IsDlgButtonChecked( hWnd, ID_Top ) & BST_CHECKED != 0
                      SetWindowText( GetDlgItem( hWnd, IDOK ), "Find First" )
                   ENDIF
                   RETURN 0

                CASE ID_Next
                   IF IsDlgButtonChecked( hWnd, ID_Next ) & BST_CHECKED != 0
                      SetWindowText( GetDlgItem( hWnd, IDOK ), "Find Next" )
                   ENDIF
                   RETURN 0

                CASE ID_Previous
                   IF IsDlgButtonChecked( hWnd, ID_Previous ) & BST_CHECKED != 0
                      SetWindowText( GetDlgItem( hWnd, IDOK ), "Find Previous" )
                   ENDIF
                   RETURN 0

                CASE ID_Selected
                   IF IsDlgButtonChecked( hWnd, ID_Selected ) & BST_CHECKED != 0
                      CheckRadioButton( ::hFindDialog, ID_Top, ID_Previous, ID_Next )

                      CheckDlgButton( hWnd, ID_Global, BST_UNCHECKED )
                      EnableWindow( GetDlgItem( hWnd, ID_Global ), .F. )
                   ELSE
                      IF Len( s_aEditors ) > 1
                         EnableWindow( GetDlgItem( hWnd, ID_Global ), .T. )
                      ELSE
                         CheckDlgButton( hWnd, ID_Global, BST_UNCHECKED )
                         EnableWindow( GetDlgItem( hWnd, ID_Global ), .F. )
                      ENDIF
                   ENDIF
                   RETURN 0

                CASE ID_Global
                   IF IsDlgButtonChecked( hWnd, ID_Global ) & BST_CHECKED != 0
                      CheckDlgButton( hWnd, ID_Selected, BST_UNCHECKED )
                      EnableWindow( GetDlgItem( hWnd, ID_Selected ), .F. )

                      lGlobal := .T.
                      oGlobalStart := ::oEditor
                   ELSE
                      IF ::oEditor:nLineFrom == 0
                         EnableWindow( GetDlgItem( hWnd, ID_Selected ), .F. )
                      ELSE
                         EnableWindow( GetDlgItem( hWnd, ID_Selected ), .T. )
                      ENDIF

                      lGlobal := .F.
                   ENDIF
                   RETURN 0

                CASE ID_Once
                CASE ID_All
                CASE IDOK
                   GetDlgItemText( hWnd, ID_Find, @sFind, 255 )

                   IF SendMessage( ::hFindTabControl, TCM_GETCURSEL, 0, 0 ) == 1 .AND. LOWORD( nwParam ) != IDOK
                      lReplace := .T.
                      GetDlgItemText( hWnd, ID_Replace, @sReplace, 255 )
                   ELSE
                      lReplace := .F.
                   ENDIF

                   IF Len( sFind ) == 0
                      ShowWindow( ::hFind, SW_HIDE )
                      RETURN 0
                   ENDIF

                   WITH OBJECT ::oEditor
                      IF IsDlgButtonChecked( hWnd, ID_Next ) & BST_CHECKED != 0
                         nFind := FR_DOWN
                      ELSEIF IsDlgButtonChecked( hWnd, ID_Top ) & BST_CHECKED != 0
                         IF lReplace

                           IF :nLineFrom > 0
                               ::nColumn := :nColumnFrom - 1
                               :nColumn := 1
                               :GoLine( :nLineFrom )
                            ELSE
                               ::nColumn := 0
                               :nColumn := 1
                               :GoLine(1)
                           ENDIF

                            nFind := FR_DOWN
                         ELSE
                            nFind := FR_FROMTOP
                         ENDIF
                      ELSE
                         nFind := 0
                      ENDIF

                      IF IsDlgButtonChecked( hWnd, ID_Selected ) & BST_CHECKED != 0
                         nFind := nFind | FR_SELECTED
                      ENDIF

                      IF IsDlgButtonChecked( hWnd, ID_RegEx ) & BST_CHECKED == 0
                         IF IsDlgButtonChecked( hWnd, ID_MatchCase ) & BST_CHECKED != 0
                            nFind := nFind | FR_MATCHCASE
                         ENDIF
                         IF IsDlgButtonChecked( hWnd, ID_WholeWord ) & BST_CHECKED != 0
                            nFind := nFind | FR_WHOLEWORD
                         ENDIF
                      ELSE
                         TRY
                            RegEx := HB_RegExComp( sFind )
                         CATCH oError
                            Alert( "Invalid Regular Expression! Please correct and try again." )
                            SetFocus( hWnd )
                            RETURN 0
                         END
                         sFind := NIL
                      ENDIF

                      IF LOWORD( nwParam ) == ID_All
                         :oDisplay:nDeferDisplay++
                      ENDIF

                      lMore := .T.
                      WHILE lMore
                         lMore := .F.

                         //TraceLog( :cFile, :nLine, :nColumn, sFind, nFind, RegEx )
                         aFound := :Find( sFind, nFind, RegEx )
                         //TraceLog( aFound[1], aFound[2], aFound[3] )

                         IF aFound[1] == 0
                            IF LOWORD( nwParam ) == ID_All
                               IF nFound > 0 .AND. Len( aActions ) > 0
                                  aActions[ -1 ][1] := ( Len( aActions ) << 8 ) | aActions[ -1 ][1]
                                  :Action( aActions, :aUnDo )
                               ENDIF

                               :oDisplay:nDeferDisplay--
                               ASSERT( :oDisplay:nDeferDisplay >= 0 )

                               IF nFound > 0
                                  :oDisplay:Display()

                                  IF aScan( s_aFind, sFind, , , .T. ) == 0
                                     aIns( s_aFind, 1, sFind, .T. )
                                     aSize( s_aFind, Min( 10, Len( s_aFind ) ) )
                                     SendMessage( GetDlgItem( hWnd, ID_Find ), CB_INSERTSTRING, 0, sFind )
                                  ENDIF

                                  IF aScan( s_aReplace, sReplace, , , .T. ) == 0
                                     aIns( s_aReplace, 1, sReplace, .T. )
                                     aSize( s_aReplace, Min( 10, Len( s_aReplace ) ) )
                                     SendMessage( GetDlgItem( hWnd, ID_Replace ), CB_INSERTSTRING, 0, sReplace )
                                  ENDIF
                               ENDIF
                            ENDIF

                            IF lGlobal
                               ::NextWindow()

                               HB_ResetWith( ::oEditor )

                               IF ! ::oEditor == oGlobalStart
                                  lMore := .T.

                                  ::nColumn := 1
                                  :GoLine( 1 )

                                  LOOP
                               ENDIF
                            ENDIF

                            IF LOWORD( nwParam ) == ID_All
                               IF nFound > 0
                                  MessageBox( hwnd, "Finished searching, " + LTrim( Str( nFound ) ) + " entries replaced.", "xEdit", MB_OK | MB_ICONINFORMATION )
                                  nFound := 0
                               ELSE
                                  MessageBox( hwnd, "Finished searching, no matches found.", "xEdit", MB_OK | MB_ICONINFORMATION )
                               ENDIF
                            ELSE
                               MessageBox( hwnd, "Finished searching, no match found.", "xEdit", MB_OK | MB_ICONINFORMATION )
                            ENDIF
                         ELSE
                            //TraceLog( aFound[1], aFound[2], aFound[3] )

                            nFound++
                            nPos   := Len( aActions ) + 1

                            IF nFind & FR_SELECTED == 0 .AND. :nLineFrom > 0
                               // Normalize.
                               :Action( { { ED_UNSELECT, :nLineFrom, :nColumnFrom, :nLineTo, :nColumnTo, :lSquare, :nDirection } }, :aUnDo )
                            ENDIF

                            IF aFound[3] <= ::nBaseColumn + ::nColumns
                               ::nColumn := aFound[2] - 1
                            ELSE
                               ::nBaseColumn := aFound[3] - ::nColumns
                               ::nColumn := aFound[2] - ::nBaseColumn - 1
                            ENDIF
                            :GoLine( aFound[1] )

                            IF nFind & FR_SELECTED == 0
                               IF lReplace
                                  aAdd( aActions, { ED_PASTE, aFound[1], aFound[2], sReplace } )
                                  aAdd( aActions, { ED_DELETE, aFound[1], aFound[2], aFound[3] - aFound[2] + 1 } )
                               ENDIF

                               aAdd( aActions, { ED_SELECT, aFound[1], aFound[2], aFound[1], aFound[3], .F., IIF( nFind & FR_DOWN != 0, 1, -1 ) } )
                            ELSE
                               IF lReplace
                                  lSquare     := :lSquare
                                  nLineFrom   := :nLineFrom
                                  nColumnFrom := :nColumnFrom
                                  nLineTo     := :nLineTo
                                  nColumnTo   := :nColumnTo
                                  nDirection  := :nDirection

                                  aAdd( aActions, { ED_SELECT  , nLineFrom, nColumnFrom, nLineTo, nColumnTo, lSquare, nDirection } )
                                  IF Len( sReplace ) > 0
                                     aAdd( aActions, { ED_PASTE   , aFound[1], aFound[2], sReplace } )
                                  ENDIF
                                  aAdd( aActions, { ED_DELETE  , aFound[1], aFound[2], aFound[3] - aFound[2] + 1 } )
                                  aAdd( aActions, { ED_UNSELECT, nLineFrom, nColumnFrom, nLineTo, nColumnTo, lSquare, nDirection } )

                                  IF nFind & FR_DOWN == 0
                                     aIns( aActions, nPos, { ED_GOTO, aFound[1], aFound[2] }, .T. )
                                  ELSE
                                     aIns( aActions, nPos, { ED_GOTO, aFound[1], aFound[2] + Len( sReplace ) }, .T. )
                                  ENDIF
                               ELSE
                                  IF nFind & FR_DOWN == 0
                                     aAdd( aActions, { ED_GOTO, aFound[1], aFound[2] } )
                                  ELSE
                                     aAdd( aActions, { ED_GOTO, aFound[1], aFound[3] } )
                                  ENDIF
                               ENDIF
                            ENDIF

                            IF lReplace
                               IF LOWORD( nwParam ) == ID_All
                                  lMore := .T.

                                  IF nFind & FR_DOWN == 0
                                     :Action( { { ED_GOTO, aFound[1], aFound[2] } }, :aUnDo )
                                  ELSE
                                     :Action( { { ED_GOTO, aFound[1], aFound[2] + Max( 1, Len( sReplace ) ) } }, :aUnDo )
                                  ENDIF
                               ENDIF
                            ELSE
                               //PostMessage( hWnd, WM_COMMAND, MAKELPARAM( IDOK, BN_CLICKED ) )
                            ENDIF
                         ENDIF

                         IF LOWORD( nwParam ) == ID_All
                            ShowWindow( ::hFind, SW_HIDE )
                            SetFocus( ::hWnd )
                         ELSE

                            #ifndef VXH

                               Point := ::RowColToScreenPoint( ::nRow, ::nColumn )

                               IF Point:x < s_DesktopRect:right / 2
                                  nX := Point:x//s_DesktopRect:right - 20 - FIND_DIALOG_WIDTH
                               ELSE
                                  nX := Point:x - FIND_DIALOG_WIDTH// 20
                               ENDIF
                               IF Point:y < s_DesktopRect:bottom / 2
                                  nY := Point:y + ::TextMetric:tmHeight//s_DesktopRect:bottom - 20 - FIND_DIALOG_HEIGHT
                               ELSE
                                  nY := Point:y - FIND_DIALOG_HEIGHT//20
                               ENDIF

                               MoveWindow( ::hFind, nX, nY, FIND_DIALOG_WIDTH, FIND_DIALOG_HEIGHT )
                            #endif
                         ENDIF
                      ENDDO

                      IF nFound > 0
                         aActions[ -1 ][1] := ( Len( aActions ) << 8 ) | aActions[ -1 ][1]
                         :Action( aActions, :aUnDo )

                         IF aScan( s_aFind, sFind, , , .T. ) == 0
                            aIns( s_aFind, 1, sFind, .T. )
                            aSize( s_aFind, Min( 10, Len( s_aFind ) ) )
                            SendMessage( GetDlgItem( hWnd, ID_Find ), CB_INSERTSTRING, 0, sFind )
                         ENDIF

                         IF lReplace .AND. aScan( s_aReplace, sReplace, , , .T. ) == 0
                            aIns( s_aReplace, 1, sReplace, .T. )
                            aSize( s_aReplace, Min( 10, Len( s_aReplace ) ) )
                            SendMessage( GetDlgItem( hWnd, ID_Replace ), CB_INSERTSTRING, 0, sReplace )
                         ENDIF
                      ENDIF
                   END

                   RETURN 0

                CASE IDCANCEL
                   ShowWindow( ::hFind, SW_HIDE )
                   SetFocus( ::hWnd )

                   RETURN 0
            END
         ENDIF
         RETURN 0

      CASE WM_INITDIALOG
         SetWindowLong( hWnd, GWL_EXSTYLE, WS_EX_CONTROLPARENT )
         RETURN 1
   END

RETURN 0

METHOD xEditGotoDialogProc( hWnd, nMsg, nwParam, nlParam ) CLASS EditorGUIDisplay

   LOCAL aFunc, aMatch, sEntity, sText, nLine
   LOCAL aBookmark, nBookmark
   LOCAL nOffset, aFunc2

   (nlParam)

   WITH OBJECT ::oEditor
      SWITCH nMsg
         CASE WM_INITDIALOG
            SetWindowLong( hWnd, GWL_EXSTYLE, WS_EX_CONTROLPARENT )
            RETURN 1

         //CASE WM_PAINT
         //   RETURN 0

         CASE WM_SETFOCUS
            //TraceLog( :aFunctions )

            SendMessage( GetDlgItem( hWnd, ID_Entities ), CB_RESETCONTENT, 0, 0 )
            FOR EACH aFunc IN :aFunctions
               IF :IsCommentedFunction( HB_EnumIndex() )
                  LOOP
               ENDIF

               aMatch := HB_Regex( s_Func, aFunc[3][ ED_BUFFER ] )

               IF Empty( aMatch )
                  aMatch := HB_Regex( s_Method, aFunc[3][ ED_BUFFER ] )

                  IF Empty( aMatch )
                     aMatch := HB_Regex( s_Method2, aFunc[3][ ED_BUFFER ] )

                     IF ! Empty( aMatch )
                        SendMessage( GetDlgItem( hWnd, ID_Entities ), CB_ADDSTRING, 0, aMatch[2] + " : " + aMatch[3] )
                     ENDIF
                  ELSE
                     SendMessage( GetDlgItem( hWnd, ID_Entities ), CB_ADDSTRING, 0, aMatch[4] + " : " + aMatch[2] )
                  ENDIF
               ELSE
                  SendMessage( GetDlgItem( hWnd, ID_Entities ), CB_ADDSTRING, 0, aMatch[4] )
               ENDIF
            NEXT

            SendMessage( GetDlgItem( hWnd, ID_Bookmarks ), CB_RESETCONTENT, 0, 0 )
            FOR EACH aBookmark IN :aBookmarks
               SendMessage( GetDlgItem( hWnd, ID_Bookmarks ), CB_ADDSTRING, 0, Left( LTrim( aBookmark[2][ ED_BUFFER ] ), 40 ) )
            NEXT

            RETURN 0

         /*
         CASE WM_NOTIFY
            Notification := (struct NMHDR)
            Notification:Pointer( nlParam )

            //TraceLog( Notification:Code )

            SWITCH Notification:Code
            END
            RETURN 0
         */

         CASE WM_COMMAND
            SWITCH HIWORD( nwParam )
               CASE EN_KILLFOCUS
                  GetDlgItemText( hWnd, LOWORD( nwParam ), @sText, 255 )

                  IF ! Empty( sText )
                      SendMessage( GetDlgItem( hWnd, ID_Bookmarks ), CB_SETCURSEL, -1, 0 )
                      SendMessage( GetDlgItem( hWnd, ID_Entities ), CB_SETCURSEL, -1, 0 )
                  ENDIF
                  RETURN 0

               CASE CBN_SELCHANGE
                  GetDlgItemText( hWnd, LOWORD( nwParam ), @sEntity, 255 )

                  IF ! Empty( sEntity )
                     IF LOWORD( nwParam ) == ID_Entities
                        SendMessage( GetDlgItem( hWnd, ID_Bookmarks ), CB_SETCURSEL, -1, 0 )
                     ELSE
                        SendMessage( GetDlgItem( hWnd, ID_Entities ), CB_SETCURSEL, -1, 0 )
                     END

                     SetDlgItemText( hWnd, ID_Line, NIL )
                     SetDlgItemText( hWnd, ID_Column, NIL )
                  ENDIF
                  RETURN 0

               CASE BN_CLICKED
                  SWITCH LOWORD( nwParam )
                      CASE IDCANCEL
                         ShowWindow( ::hFind, SW_HIDE )
                         SetFocus( ::hWnd )
                         RETURN 0

                      CASE IDOK
                         GetDlgItemText( hWnd, ID_Line, @sText, 255 )
                         nLine := Val( sText )
                         IF nLine > 0
                            :GoLine( nLine )

                            GetDlgItemText( hWnd, ID_Column, @sText, 255 )
                            ::GoColumn( Val( sText ) )

                            ShowWindow( ::hFind, SW_HIDE )
                            SetFocus( ::hWnd )
                            RETURN 0
                         ENDIF

                         GetDlgItemText( hWnd, ID_Entities, @sEntity, 255 )
                         IF ! Empty( sEntity )
                            //TraceLog( "Searching", sEntity )

                            FOR EACH aFunc IN :aFunctions
                               IF :IsCommentedFunction( HB_EnumIndex() )
                                  LOOP
                               ENDIF

                               aMatch := HB_Regex( s_Func, aFunc[3][ ED_BUFFER ] )

                               IF Empty( aMatch )
                                  aMatch := HB_Regex( s_Method, aFunc[3][ ED_BUFFER ] )

                                  IF Empty( aMatch )
                                     aMatch := HB_Regex( s_Method2, aFunc[3][ ED_BUFFER ] )

                                     IF ! Empty( aMatch )
                                        IF aMatch[2] + " : " + aMatch[3] == sEntity
                                           nOffset := 0

                                           FOR EACH aFunc2 IN :aFunctions
                                              IF aFunc2 == aFunc
                                                 EXIT
                                              ENDIF

                                              IF Len( aFunc2[3] ) > ED_BASE_LEN
                                                 nOffset += aFunc2[3][ ED_COLLAPSED_LINES ]
                                              ENDIF
                                           NEXT

                                           :GoLine( aFunc[1] - nOffset )
                                           ShowWindow( ::hFind, SW_HIDE )
                                           SetFocus( ::hWnd )
                                           RETURN 0
                                        ENDIF
                                     ENDIF
                                  ELSE
                                     IF aMatch[4] + " : " + aMatch[2] == sEntity
                                        nOffset := 0

                                        FOR EACH aFunc2 IN :aFunctions
                                           IF aFunc2 == aFunc
                                              EXIT
                                           ENDIF

                                           IF Len( aFunc2[3] ) > ED_BASE_LEN
                                              nOffset += aFunc2[3][ ED_COLLAPSED_LINES ]
                                           ENDIF
                                        NEXT

                                        :GoLine( aFunc[1] - nOffset )
                                        ShowWindow( ::hFind, SW_HIDE )
                                        SetFocus( ::hWnd )
                                        RETURN 0
                                     ENDIF
                                  ENDIF
                               ELSE
                                  IF aMatch[4] == sEntity
                                     nOffset := 0

                                     FOR EACH aFunc2 IN :aFunctions
                                        IF aFunc2 == aFunc
                                           EXIT
                                        ENDIF

                                        IF Len( aFunc2[3] ) > ED_BASE_LEN
                                           nOffset += aFunc2[3][ ED_COLLAPSED_LINES ]
                                        ENDIF
                                     NEXT

                                     :GoLine( aFunc[1] - nOffset )
                                     ShowWindow( ::hFind, SW_HIDE )
                                     SetFocus( ::hWnd )
                                     RETURN 0
                                  ENDIF
                               ENDIF
                            NEXT
                         ENDIF

                         nBookmark := SendMessage( GetDlgItem( hWnd, ID_Bookmarks ), CB_GETCURSEL, 0, 0 )
                         IF nBookmark != CB_ERR
                            nBookmark++
                            nOffset := 0

                            FOR EACH aFunc IN :aFunctions
                               IF aFunc[1] == :aBookmarks[ nBookmark ][1]
                                  EXIT
                               ELSEIF aFunc[1] > :aBookmarks[ nBookmark ][1]
                                  IF HB_EnumIndex() > 1 .AND. Len( :aFunctions[ HB_EnumIndex() - 1 ][3] ) > ED_BASE_LEN
                                     nOffset -= :aFunctions[ HB_EnumIndex() - 1 ][3][ ED_COLLAPSED_LINES ]
                                     :Expand( :aFunctions[ HB_EnumIndex() - 1 ][3] )
                                  ENDIF

                                  EXIT
                               ENDIF

                               IF Len( aFunc[3] ) > ED_BASE_LEN
                                  nOffset += aFunc[3][ ED_COLLAPSED_LINES ]
                               ENDIF
                            NEXT

                            :GoLine( :aBookmarks[ nBookmark ][1] - nOffset )
                            ShowWindow( ::hFind, SW_HIDE )
                            SetFocus( ::hWnd )
                         ENDIF
                         RETURN 0
                  END
            END
            RETURN 0
      END
   END

RETURN 0

METHOD xEditContainerWindowProc( hWnd, nMsg, nwParam, nlParam ) CLASS EditorGUIDisplay

   LOCAL TreeviewRect
   LOCAL nEnable, ofn, cFile, oEditor, Notification, tvi, nEditorID
   LOCAL aIni, oError
   LOCAL nFile, nFiles
   LOCAL hMenu, aFunc, aMatch, aMenu, aBookmark
   LOCAL nOffset
   LOCAL Rect
   LOCAL aFiles
   LOCAL hTemp

   //TraceLog( NumToHex( nMsg ) )

   WITH OBJECT ::oEditor
      SWITCH nMsg
         CASE WM_SETFOCUS
            SetFocus( ::hWnd )
            RETURN 0

         CASE WM_WINDOWPOSCHANGED
         //CASE WM_SIZE
            IF Empty( ::hFilesTree )
               RETURN 0
            ENDIF

            ValidateRect( hWnd )

            GetClientRect( hWnd, @Rect )

            // NOT on Minimize
            IF Rect:right == 0 .OR. Rect:bottom == 0
               RETURN 0
            ENDIF

            //TraceLog( Rect:right, Rect:bottom, ::nStatusHeight )

            MoveWindow( ::hStatusBar, 0, Rect:bottom - ::nStatusHeight, Rect:right, ::nStatusHeight, .F. )

            GetWindowRect( ::hFilesTree, @TreeviewRect )

            MoveWindow( ::hFilesTree, 0, 0, Max( 25, Min( Rect:right - 50, ( TreeviewRect:right - TreeviewRect:left ) ) ), Rect:bottom - ::nStatusHeight + 1, .T. )

            GetWindowRect( ::hFilesTree, @TreeviewRect )

            MoveWindow( ::hSplitter, ( TreeviewRect:right - TreeviewRect:left ) - 2, 0, 4, Rect:bottom - ::nStatusHeight + 1, .F. )

            // Will be painted again in WM_WINDOWPOSCHANGED of ::xEditWindowProcedure() resulting from MoveWindow()
            ::nDeferDisplay++
            MoveWindow( ::hWnd, ( TreeviewRect:right - TreeviewRect:left ), 0, Rect:right - ( TreeviewRect:right - TreeviewRect:left ) + 1, Rect:bottom - ::nStatusHeight + 1, .T. )
            ::nDeferDisplay--
            ASSERT( ::nDeferDisplay >= 0 )
            RETURN 0

         CASE WM_INITMENUPOPUP
            //TraceLog( LOWORD( nlParam ) )
            SWITCH LOWORD( nlParam )
               CASE 0
                  EnableMenuItem(  nwParam, IDM_FILE_SAVE,    MF_BYCOMMAND | IIF( Empty( :cFile ) .OR. ! :lModified, MF_GRAYED, MF_ENABLED ) )
                  EnableMenuItem(  nwParam, IDM_FILE_DISCARD, MF_BYCOMMAND | IIF( Empty( :cFile ) .OR. ! :lModified, MF_GRAYED , MF_ENABLED ) )

                  EnableMenuItem(  nwParam, IDM_FILE_SAVE_ALL, MF_BYCOMMAND | MF_GRAYED )
                  FOR EACH oEditor IN s_aEditors
                     IF oEditor:lModified .AND. ! Empty( oEditor:cFile )
                        EnableMenuItem(  nwParam, IDM_FILE_SAVE_ALL, MF_BYCOMMAND | MF_ENABLED )
                        EXIT
                     ENDIF
                  NEXT
                  RETURN 0

               CASE 1
                  EnableMenuItem(  nwParam, IDM_EDIT_UNDO,     MF_BYCOMMAND | IIF( Len( :aUnDo ) > 0 ,MF_ENABLED, MF_GRAYED ) )
                  EnableMenuItem(  nwParam, IDM_EDIT_REDO,     MF_BYCOMMAND | IIF( Len( :aReDo ) > 0 ,MF_ENABLED, MF_GRAYED ) )
                  EnableMenuItem(  nwParam, IDM_EDIT_UNDOFAST, MF_BYCOMMAND | IIF( aScan( :aUndo, {|aAction| aAction[1] & 0xFF < ED_NOIMPACT } ) > 0, MF_ENABLED, MF_GRAYED ) )
                  EnableMenuItem(  nwParam, IDM_EDIT_REDOFAST, MF_BYCOMMAND | IIF( aScan( :aRedo, {|aAction| aAction[1] & 0xFF < ED_NOIMPACT } ) > 0, MF_ENABLED, MF_GRAYED ) )

                  EnableMenuItem(  nwParam, IDM_EDIT_PASTE, MF_BYCOMMAND | IIF( IsClipboardFormatAvailable( CF_TEXT ), MF_ENABLED, MF_GRAYED ) )

                  IF :nLineFrom == 0
                     nEnable = MF_GRAYED
                  ELSE
                     nEnable = MF_ENABLED
                  ENDIF

                  EnableMenuItem ( nwParam, IDM_EDIT_CUT,   nEnable)
                  EnableMenuItem ( nwParam, IDM_EDIT_COPY,  nEnable)
                  EnableMenuItem ( nwParam, IDM_EDIT_CLEAR, nEnable)
                  RETURN 0

               CASE 3
                  DeleteMenu( nwParam, 0, MF_BYPOSITION )

                  IF Len( :aFunctions ) == 0
                     InsertMenu( nwParam, 0, MF_BYPOSITION | MF_STRING, IDM_VIEW_ENTITIES, "Entities" )
                     EnableMenuItem ( nwParam, IDM_VIEW_ENTITIES, MF_BYCOMMAND | MF_GRAYED )
                  ELSE
                     hMenu := CreateMenu()

                     aMenu := {}
                     FOR EACH aFunc IN :aFunctions
                        IF :IsCommentedFunction( HB_EnumIndex() )
                           LOOP
                        ENDIF

                        aMatch := HB_Regex( s_Func, aFunc[3][ ED_BUFFER ] )

                        IF Empty( aMatch )
                           aMatch := HB_Regex( s_Method, aFunc[3][ ED_BUFFER ] )

                           IF Empty( aMatch )
                              aMatch := HB_Regex( s_Method2, aFunc[3][ ED_BUFFER ] )

                              IF ! Empty( aMatch )
                                 aAdd( aMenu, { 10000 + HB_EnumIndex(), aMatch[2] + " : " + aMatch[3] } )
                              ENDIF
                           ELSE
                              aAdd( aMenu, { 10000 + HB_EnumIndex(), aMatch[4] + " : " + aMatch[2] } )
                           ENDIF
                        ELSE
                           aAdd( aMenu, { 10000 + HB_EnumIndex(), aMatch[4] } )
                        ENDIF
                     NEXT

                     aSort( aMenu, , , {|_1, _2| _1[2] < _2[2] } )

                     FOR EACH aFunc IN aMenu
                        AppendMenu( hMenu, MF_STRING, aFunc[1], aFunc[2] )
                     NEXT

                     InsertMenu( nwParam, 0, MF_BYPOSITION | MF_POPUP, hMenu, "Entities" )
                  ENDIF

                  DeleteMenu( nwParam, 1, MF_BYPOSITION )
                  IF Len( :aBookmarks ) == 0
                     InsertMenu( nwParam, 1, MF_BYPOSITION | MF_STRING, IDM_VIEW_BOOKMARKS, "Bookmarks" )
                     EnableMenuItem ( nwParam, IDM_VIEW_BOOKMARKS, MF_BYCOMMAND | MF_GRAYED )
                  ELSE
                     hMenu := CreateMenu()

                     FOR EACH aBookmark IN :aBookmarks
                        AppendMenu( hMenu, MF_STRING, 20000 + HB_EnumIndex(), Left( LTrim( aBookmark[2][ ED_BUFFER ] ), 40 ) )
                     NEXT

                     InsertMenu( nwParam, 1, MF_BYPOSITION | MF_POPUP, hMenu, "Bookmarks" )
                  ENDIF
                  RETURN 0

               CASE 4
                  WHILE GetMenuItemCount( nwParam ) > 3
                     IF ! DeleteMenu( nwParam, 3, MF_BYPOSITION )
                        Throw( ErrorNew( "xEdit", 0, 1002, "Menu deletion failure.", HB_aParams() ) )
                     ENDIF
                  END

                  FOR EACH oEditor IN s_aEditors
                     IF HB_EnumIndex() < 10
                        AppendMenu( nwParam, MF_STRING, 6000 + HB_EnumIndex(), " &" + Str( HB_EnumIndex(), 1 ) + " " + oEditor:cPath + oEditor:cFile )
                     ELSE
                        AppendMenu( nwParam, MF_STRING, 6000 + HB_EnumIndex(), Str( HB_EnumIndex(), 2 ) + " " + oEditor:cPath + oEditor:cFile )
                     ENDIF

                     IF oEditor == HB_QWith()
                        CheckMenuItem( nwParam, 6000 + HB_EnumIndex(), MF_CHECKED | MF_BYCOMMAND )
                     ENDIF
                  NEXT
                  RETURN 0
            END
            EXIT

         CASE WM_COMMAND
            //TraceLog( nwParam, nlParam, LOWORD( nwParam ), HIWORD( nwParam ) )

            IF nlParam == 0
               IF LOWORD( nwParam ) > 5000 .AND. LOWORD( nwParam ) < 5100
                  FOR EACH oEditor IN s_aEditors
                     //TraceLog( oEditor:cPath + oEditor:cFile, cFile )

                     IF Lower( oEditor:cPath + oEditor:cFile ) == s_aFiles[ LOWORD( nwParam ) - 5000 ]
                        IF MessageBox( 0, "<" + s_aFiles[ LOWORD( nwParam ) - 5000 ] + "> already loaded, load again?", "xEdit", MB_TASKMODAL | MB_ICONQUESTION | MB_YESNO ) == IDNO
                           RETURN 0
                        ENDIF

                        EXIT
                     ENDIF
                  NEXT

                  TRY
                     IF File( s_aFiles[ LOWORD( nwParam ) - 5000 ] )
                        Editor():New( 0, 0, 0, 0, s_aFiles[ LOWORD( nwParam ) - 5000 ], Self )
                        ::Display()
                        //SendMessage( hwnd, WM_SETTEXT, , "xEdit - " + s_aFiles[ LOWORD( nwParam ) - 5000 ] )
                     ELSE
                        MessageBox( hwnd, "File: <" + s_aFiles[ LOWORD( nwParam ) - 5000 ]  + "> is no longer present.", "xEdit", MB_OK | MB_ICONINFORMATION )
                     ENDIF
                  CATCH oError
                     TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine, ValToPrg( oError:Args ) )
                  END

                  RETURN 0
               ELSEIF LOWORD( nwParam ) > 5100 .AND. LOWORD( nwParam ) < 5200
                  ::OpenWorkspace( s_aWorkspaces[ LOWORD( nwParam ) - 5100 ] )

                  RETURN 0
               ELSEIF LOWORD( nwParam ) > 6000 .AND. LOWORD( nwParam ) < 7000
                  s_aEditors[ LOWORD( nwParam ) - 6000 ]:SetDisplay( Self, .T. )

                  // Do NOT use :cPath and :cFile as the WITH OBJECT is NO LONGER the desired Editor.
                  //SendMessage( hWnd, WM_SETTEXT, , "xEdit - " + s_aEditors[ LOWORD( nwParam ) - 6000 ]:cPath + s_aEditors[ LOWORD( nwParam ) - 6000 ]:cFile )
                  RETURN 0
               ELSEIF LOWORD( nwParam ) > 10000 .AND. LOWORD( nwParam ) < 20000
                  nOffset := 0

                  FOR EACH aFunc IN :aFunctions
                     IF aFunc == :aFunctions[ LOWORD( nwParam ) - 10000 ]
                        EXIT
                     ENDIF

                     IF Len( aFunc[3] ) > ED_BASE_LEN
                        nOffset += aFunc[3][ ED_COLLAPSED_LINES ]
                     ENDIF
                  NEXT

                  :GoLine( :aFunctions[ LOWORD( nwParam ) - 10000 ][1] - nOffset )
                  RETURN 0
               ELSEIF LOWORD( nwParam ) > 20000 .AND. LOWORD( nwParam ) < 30000
                  nOffset := 0

                  FOR EACH aFunc IN :aFunctions
                     IF aFunc[1] == :aBookmarks[ LOWORD( nwParam ) - 20000 ][1]
                        EXIT
                     ELSEIF aFunc[1] > :aBookmarks[ LOWORD( nwParam ) - 20000 ][1]
                        IF HB_EnumIndex() > 1 .AND. Len( :aFunctions[ HB_EnumIndex() - 1 ][3] ) > ED_BASE_LEN
                           nOffset -= :aFunctions[ HB_EnumIndex() - 1 ][3][ ED_COLLAPSED_LINES ]
                           :Expand( :aFunctions[ HB_EnumIndex() - 1 ][3] )
                        ENDIF

                        EXIT
                     ENDIF

                     IF Len( aFunc[3] ) > ED_BASE_LEN
                        nOffset += aFunc[3][ ED_COLLAPSED_LINES ]
                     ENDIF
                  NEXT

                  :GoLine( :aBookmarks[ LOWORD( nwParam ) - 20000 ][1] - nOffset )
                  RETURN 0
               ENDIF

               SWITCH LOWORD( nwParam )
                  CASE IDM_FILE_NEW
                     Editor():New( 0, 0, 0, 0, , Self )
                     ::Display()

                     //SendMessage( hwnd, WM_SETTEXT, , "xEdit" )
                     RETURN 0

                  CASE IDM_FILE_OPEN
                     ofn IS OPENFILENAME

                     ofn:lStructSize     := 76
                     ofn:hwndOwner       := hWnd
                     ofn:hInstance       := s_hInstance
                     ofn:nMaxFile        := MAX_PATH + 1
                     //ofn:lpstrInitialDir :=
                     ofn:lpstrFile       := Space( MAX_PATH )
                     ofn:lpstrFilter     := "xHarbour source files" + Chr(0) + "*.prg;*.ch;*.xbs;*.xfm" + Chr(0) +;
                                            "C sources"             + Chr(0) + "*.c;*.cpp;*.h"          + Chr(0) +;
                                            "Resource source file"  + Chr(0) + "*.rc"                   + Chr(0) +;
                                            "xBuild project files"  + Chr(0) + "*.xbp;*.inc"            + Chr(0) +;
                                            "Log files"             + Chr(0) + "*.log"                  + Chr(0) +;
                                            "Text files"            + Chr(0) + "*.txt"                  + Chr(0) +;
                                            "All files"             + Chr(0) + "*.*"                    + Chr(0)
                     ofn:lpstrTitle      := "xEdit - Open File"
                     ofn:Flags           := OFN_FILEMUSTEXIST

                     IF ! GetOpenFileName( @ofn )
                        RETURN 0
                     ENDIF

                     cFile := Left( ofn:lpstrFile, At( Chr(0), ofn:lpstrFile ) - 1 )

                     IF ! Empty( cFile )
                        FOR EACH oEditor IN s_aEditors
                           //TraceLog( oEditor:cPath + oEditor:cFile, cFile )

                          #ifdef WIN
                           IF Lower( oEditor:cPath + oEditor:cFile ) == Lower( cFile )
                           #else
                           IF oEditor:cPath + oEditor:cFile == cFile
                          #endif
                              IF MessageBox( 0, "<" + cFile + "> already loaded, load again?", "xEdit", MB_TASKMODAL | MB_ICONQUESTION | MB_YESNO ) == IDNO
                                 RETURN 0
                              ENDIF

                              EXIT
                           ENDIF
                        NEXT

                        TRY
                           Editor():New( 0, 0, 0, 0, cFile, Self )
                           ::Display()
                           //SendMessage( hwnd, WM_SETTEXT, , "xEdit - " + cFile )
                        CATCH
                        END
                     ENDIF
                     RETURN 0

                  CASE IDM_FILE_OPEN_WORKSPACE
                     ofn IS OPENFILENAME

                     ofn:lStructSize     := 76
                     ofn :hwndOwner      := hWnd
                     ofn:hInstance       := s_hInstance
                     ofn:nMaxFile        := MAX_PATH + 1
                     //ofn:lpstrInitialDir :=
                     ofn:lpstrFile       := Space( MAX_PATH )
                     ofn:lpstrFilter     := "xEdit Workspace files" + Chr(0) + "*.xws" + Chr(0)
                     ofn:lpstrTitle      := "xEdit - Open Workspace"
                     ofn:Flags           := OFN_FILEMUSTEXIST

                     IF ! GetOpenFileName( @ofn )
                        RETURN 0
                     ENDIF

                     cFile := Left( ofn:lpstrFile, At( Chr(0), ofn:lpstrFile ) - 1 )

                     IF ! Empty( cFile )
                        ::OpenWorkSpace( cFile )
                     ENDIF
                     RETURN 0

                  CASE IDM_FILE_CLOSE
                     ::CloseWindow( .T. )
                     RETURN 0

                  CASE IDM_FILE_CLOSE_ALL
                     WHILE Len( s_aEditors ) > 0 .AND. ::CloseWindow( .F. )
                     ENDDO

                     ::Synch( .T.)

                     SendMessage( hwnd, WM_SETTEXT, , "xEdit" )
                     RETURN 0

                  CASE IDM_FILE_DISCARD
                     :Load( :cPath + :cFile, , .T., .T. )

                     tvi := (struct TVITEM)

                     tvi:hItem     := :hFileItem
                     tvi:mask      := TVIF_STATE
                     tvi:stateMask := TVIS_BOLD
                     tvi:state     := 0

                     SendMessage( ::hFilesTree, TVM_SETITEM, 0, @tvi )
                     RETURN 0

                  CASE IDM_FILE_SAVE
                     :Save()
                     RETURN 0
#ifndef VXH
                  CASE IDM_FILE_SAVE_ALL
                     FOR EACH oEditor IN s_aEditors
                        IF oEditor:lModified
                           oEditor:Save()
                        ENDIF
                     NEXT
                     RETURN 0
#endif
                  CASE IDM_FILE_SAVE_AS
                     ofn := (struct OPENFILENAME)

                     ofn:lStructSize     := 76
                     ofn :hwndOwner      := hWnd
                     ofn:hInstance       := s_hInstance
                     ofn:nMaxFile        := MAX_PATH + 1
                     //ofn:lpstrInitialDir :=
                     ofn:lpstrFile       := Pad( "*.prg", MAX_PATH )
                     ofn:lpstrDefExt     := "prg"
                     ofn:lpstrFilter     := "xHarbour source files" + Chr(0) + "*.prg;*.ch;*.xbs;*.xfm" + Chr(0) +;
                                            "C sources"             + Chr(0) + "*.c;*.cpp;*.h"          + Chr(0) +;
                                            "Resource source file"  + Chr(0) + "*.rc"                   + Chr(0) +;
                                            "xBuild project files"  + Chr(0) + "*.xbp;*.inc"            + Chr(0) +;
                                            "Log files"             + Chr(0) + "*.log"                  + Chr(0) +;
                                            "Text files"            + Chr(0) + "*.txt"                  + Chr(0) +;
                                            "All files"             + Chr(0) + "*.*"                    + Chr(0)
                     ofn:lpstrTitle      := "xEdit - Save document as"
                     ofn:Flags           := OFN_NOREADONLYRETURN | OFN_OVERWRITEPROMPT | OFN_PATHMUSTEXIST

                     IF ! GetSaveFileName( @ofn )
                        RETURN 0
                     ENDIF

                     cFile := Left( ofn:lpstrFile, At( Chr(0), ofn:lpstrFile ) - 1 )

                     IF ! Empty( cFile )
                        IF :hFileItem != NIL .AND. ::hFilesTree != NIL
                           hTemp := TVM_DELETEITEM
                           :hFileItem := NIL
                           SendMessage( ::hFilesTree, TVM_DELETEITEM, 0, hTemp )
                        ENDIF

                        :Save( cFile )
                        :SetDisplay( Self, .T. )

                        //SendMessage( hwnd, WM_SETTEXT, , "xEdit - " + :cPath + :cFile )
                     ENDIF
                     RETURN 0

                  CASE IDM_FILE_SAVE_WORKSPACE
                     ofn := (struct OPENFILENAME)

                     ofn:lStructSize     := 76
                     ofn:hwndOwner       := hWnd
                     ofn:hInstance       := s_hInstance
                     ofn:nMaxFile        := MAX_PATH + 1
                     //ofn:lpstrInitialDir :=
                     ofn:lpstrFile       := s_cWorkspaceFilename + Space( MAX_PATH - Len( s_cWorkspaceFilename ) )
                     ofn:lpstrFilter     := "xEdit Workspace files" + Chr(0) + "*.xws" + Chr(0)
                     ofn:lpstrDefExt     := "xws"
                     ofn:lpstrTitle      := "xEdit - Save Workspace"
                     ofn:Flags           := OFN_NOREADONLYRETURN | OFN_OVERWRITEPROMPT | OFN_PATHMUSTEXIST

                     IF ! GetSaveFileName( @ofn )
                        RETURN 0
                     ENDIF

                     s_cWorkspaceFilename := Left( ofn:lpstrFile, At( Chr(0), ofn:lpstrFile ) - 1 )

                     IF ! '.' IN s_cWorkspaceFilename
                        s_cWorkspaceFilename += ".xws"
                     ENDIF

                     IF ! Empty( s_cWorkspaceFilename )
                        aIni := Hash()
                        aIni[ "MAIN" ] := Hash()

                        FOR EACH oEditor IN s_aEditors
                           IF oEditor == ::oEditor
                              LOOP
                           ENDIF

                           aIni[ "MAIN" ][ oEditor:cPath + oEditor:cFile ] := Str( oEditor:nLine ) + "," + Str( oEditor:nColumn )
                        NEXT

                        aIni[ "Default" ] := Hash()
                        aIni[ "Default" ][ :cPath + :cFile ] := Str( :nLine, 9 ) + "," + Str( :nColumn, 9 )

                        HB_WriteIni( s_cWorkspaceFilename, aIni )
                     ENDIF
                     RETURN 0

                  CASE IDM_FILE_PRINT
                     Alert( "Print" )
                     RETURN 0

                  CASE IDM_FILE_EXIT
                     SendMessage( hWnd, WM_CLOSE, 0, 0 )
                     RETURN 0

                  CASE IDM_EDIT_UNDOFAST
                     :lShift := .T.
                  CASE IDM_EDIT_UNDO
                     :OnKey( K_CTRL_Z, 1 )
                     :lShift := .F.
                     RETURN 0

                  CASE IDM_EDIT_REDOFAST
                     :lShift := .T.
                  CASE IDM_EDIT_REDO
                     :OnKey( K_CTRL_Y, 1 )
                     :lShift := .F.
                     RETURN 0

                  CASE IDM_EDIT_CUT
                     :OnKey( K_CTRL_X, 1 )
                     RETURN 0

                  CASE IDM_EDIT_COPY
                     :OnKey( K_CTRL_C, 1 )
                     RETURN 0

                  CASE IDM_EDIT_PASTE
                     :OnKey( K_CTRL_V, 1 )
                     RETURN 0

                  CASE IDM_EDIT_CLEAR
                     :OnKey( K_DEL, 1 )
                     RETURN 0

                  CASE IDM_EDIT_SELECT_ALL
                     :Home()
                     :lShift := .T.
                     :End()
                     :lShift := .F.
                     RETURN 0

                  CASE IDM_EDIT_FIND
                     :OnKey( K_CTRL_F, 1 )
                     RETURN 0

                  CASE IDM_EDIT_REPLACE
                     :OnKey( K_CTRL_H, 1 )
                     RETURN 0

                  CASE IDM_EDIT_GOTO
                     :OnKey( K_CTRL_G, 1 )
                     RETURN 0

                  CASE IDM_FORMAT_FONT
                     ::ChooseFont()
                     RETURN 0

                  CASE IDM_VIEW_NORMAL
                     :Action( { { ED_EXPANDALL, :nLine } }, :aUnDo )
                     RETURN 0

                  CASE IDM_VIEW_COLLAPSED
                     :Action( { { ED_COLLAPSEALL, :nLine } }, :aUnDo )
                     RETURN 0

                  CASE IDM_WINDOW_NEXT
                     ::NextWindow()
                     RETURN 0

                  CASE IDM_WINDOW_PREVIOUS
                     ::PreviousWindow()
                     RETURN 0

                  CASE IDM_HELP_HELP
                     MessageBox( hwnd, "Sorry, Help not yet implemented.", "xEdit", MB_OK | MB_ICONEXCLAMATION )
                     RETURN 0

                  CASE IDM_HELP_ABOUT
                     MessageBox( hwnd, "xEdit (c) " + __DATE__ + " " + __TIME__ + " xHarbour.com Inc., 2004-2006", "xEdit", MB_OK | MB_ICONINFORMATION )
                     RETURN 0
               END
            //ELSE
            //   IF nwParam == ID_Shrink
            //      SetFocus( ::hWnd )
            //   ENDIF
            ENDIF
            EXIT

         CASE WM_NOTIFY
            Notification := (struct NMHDR)
            Notification:Pointer( nlParam )

            //TraceLog( Notification:Code )

            SWITCH Notification:Code
               CASE NM_SETFOCUS
                  SetFocus( ::hWnd )
                  EXIT

               CASE TVN_SELCHANGED
                  //TraceLog( "TVN_SELCHANGED" )

                  IF ::hFilesTree == NIL
                     EXIT
                  ENDIF

                  //pnmtv := (struct NMTREEVIEW)
                  //pnmtv:Pointer( nlParam )

                  tvi := (struct TVITEM)

                  tvi:hItem := SendMessage( ::hFilesTree, TVM_GETNEXTITEM, TVGN_CARET, 0 )

                  tvi:mask       := TVIF_TEXT
                  tvi:pszText    := Space( MAX_PATH + 1 )
                  tvi:cchTextMax := MAX_PATH

                  SendMessage( ::hFilesTree, TVM_GETITEM, 0, @tvi )

                  cFile := Left( tvi:pszText, At( Chr(0), tvi:pszText ) -1 )

                  IF cFile[1] == '*' .OR. cFile == "Opened Files"
                     IF :hFileItem != NIL //.AND. ::hFilesTree != NIL
                        //TraceLog( "Oops", cFile, :hFileItem )
                        SendMessage( ::hFilesTree, TVM_SELECTITEM, TVGN_CARET, :hFileItem )
                     ENDIF
                  ELSE
                     nEditorID := aScan( s_aEditors, {|_1| _1:hFileItem == tvi:hItem } )
                     IF nEditorID > 0 .AND. ( ! ( s_aEditors[ nEditorID ] == ::oEditor ) )
                        s_aEditors[ nEditorID ]:SetDisplay( Self, .T. )

                        // Do NOT use :cPath and :cFile as the WITH OBJECT is NO LONGER the desired Editor.
                        //SendMessage( hWnd, WM_SETTEXT, , "xEdit - " + s_aEditors[ nEditorID ]:cPath + s_aEditors[ nEditorID ]:cFile )
                     ENDIF
                  ENDIF

                  SetFocus( ::hWnd )
                  EXIT
            END
            EXIT

        CASE WM_DROPFILES
            nFiles := DragQueryFile( nwParam, 0xFFFFFFFF ) - 1
            FOR nFile := 0 TO nFiles
               DragQueryFile( nwParam, nFile, @cFile, MAX_PATH )

               Editor():New( 0, 0, 0, 0, cFile, Self )
            NEXT

            IF nFile > 0
               ::Display()
               //SendMessage( hwnd, WM_SETTEXT, , "xEdit - " + cFile )
            ENDIF
            RETURN 0

         CASE WM_ACTIVATEAPP
            IF nwParam == 0
               IF s_lAutoSave .AND. :lModified .AND. (! ProcName(1) == "DESTROYWINDOW" ) .AND. ( ! Empty( :cFile ) )
                  //TraceLog( "SAVE" )
                  :Save( , .T. )
               ENDIF
            ELSE
               FOR EACH oEditor IN s_aEditors
                  WITH OBJECT oEditor
                     //TraceLog( :cPath, :cFile, Directory( :cPath + :cFile ) )

                     IF ! Empty( :cFile )
                        aFiles := Directory( :cPath + :cFile )

                        IF Len( aFiles ) == 0
                           IF MessageBox( 0, "<" + :cPath + :cFile + "> has been deleted. Would you like to restore the file from memory?", "xEdit", MB_TASKMODAL | MB_ICONQUESTION | MB_YESNO ) == IDYES
                              :Save( , .T. )
                           ENDIF
                        ELSE
                           IF aFiles[1][ F_DATE ] > :Date .OR. ( aFiles[1][ F_DATE ] == :Date .AND. aFiles[1][ F_TIME ] > :Time )
                              IF aScan( :aUndo, {|aAction| aAction[1] & 0xFF < ED_NOIMPACT } ) > 0
                                 IF MessageBox( 0, "<" + :cPath + :cFile + "> has been modified by other program. Would you like to reload the file and LOSE your changes?", "xEdit", MB_TASKMODAL | MB_ICONQUESTION | MB_YESNO ) == IDYES
                                    :Load( :cPath + :cFile, , .T., .T. )
                                  ENDIF
                              ELSE
                                 IF MessageBox( 0, "<" + :cPath + :cFile + "> has been modified by other program. Would you like to reload the file?", "xEdit", MB_TASKMODAL | MB_ICONQUESTION | MB_YESNO ) == IDYES
                                   :Load( :cPath + :cFile, , .T., .T. )
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                  END
               NEXT
            ENDIF
            RETURN 0

         CASE WM_QUERYENDSESSION
         CASE WM_CLOSE
            //TraceLog( "Close!" )
            WHILE Len( s_aEditors ) > 0
               IF ! ::CloseWindow( .F. )
                  RETURN 0
               ENDIF
            ENDDO

            IF nMsg == WM_QUERYENDSESSION
               RETURN 1
            ENDIF

            DestroyWindow( hWnd )
            RETURN 0

         CASE WM_DESTROY
            //TraceLog( "Destroy!" )

            /* Windows' Menu is auto destroyed as per MSDN
            IF ! Empty( s_hMenu )
               //TraceLog( "DestroyMenu", s_hMenu )
               DestroyMenu( s_hMenu )
               s_hMenu := NIL
            ENDIF
            */

            IF ! Empty( s_hFont )
               //TraceLog( "DeleteObject - s_hFont", s_hFont )
               DeleteObject( s_hFont )
               s_hFont := NIL
            ENDIF

            IF ! Empty( s_hFindFont )
               //TraceLog( "DeleteObject - s_hFindFont", s_hFindFont )
               DeleteObject( s_hFindFont )
               s_hFindFont := NIL
            ENDIF

            IF ! Empty( s_hPlus )
               //TraceLog( "DestroyIcon - s_hPlus", s_hPlus )
               DestroyIcon( s_hPlus )
               s_hPlus := NIL
            ENDIF
            IF ! Empty( s_hMinus )
               //TraceLog( "DestroyIcon - s_hMinus", s_hMinus )
               DestroyIcon( s_hMinus )
               s_hMinus := NIL
            ENDIF
            IF ! Empty( s_hBookmark )
               //TraceLog( "DestroyIcon - s_hBookmark", s_hBookmark )
               DestroyIcon( s_hBookmark )
               s_hBookmark := NIL
            ENDIF

            // Last
            IF ! Empty( s_hDll )
               //TraceLog( "FreeLibrary", s_hDll )
               FreeLibrary( s_hDll )
               s_hDll := NIL
            ENDIF

            //TODO: Free all others!!!
            FreeCallBackPointer( s_wc:lpfnWndProc )
            s_wc:lpfnWndProc := NIL

            // Detach!
            SetWindowLong( hWnd, GWL_WNDPROC, DefWindowProcPointer() )
            //TraceLog( "Deatched!" )

            PostQuitMessage( 0 )
            RETURN 0
      END
   END

   //TraceLog( nMsg )

RETURN DefWindowProc( hWnd, nMsg, nwParam, nlParam )

METHOD xEditWindowProc( hWnd, nMsg, nwParam, nlParam ) CLASS EditorGUIDisplay

   STATIC K_Ctrl := { K_CTRL_A, K_CTRL_B, K_CTRL_C, K_CTRL_D, K_CTRL_E, K_CTRL_F, K_CTRL_G, K_CTRL_H, ;
                      K_CTRL_I, K_CTRL_J, K_CTRL_K, K_CTRL_L, K_CTRL_M, K_CTRL_N, K_CTRL_O, K_CTRL_P, ;
                      K_CTRL_Q, K_CTRL_R, K_CTRL_S, K_CTRL_T, K_CTRL_U, K_CTRL_V, K_CTRL_W, K_CTRL_X, ;
                      K_CTRL_Y, K_CTRL_Z }

   LOCAL nKey
   LOCAL nCount
   LOCAL Rect, hDC, nRow, nColumn
   LOCAL nRows, nColumns
   LOCAL si
   LOCAL ps
   LOCAl nLine
   LOCAL nX, nY
   LOCAL hPopup, hContext, Point, nEnable
   LOCAL nAt, cPath, aIni, sFind, sReplace, wndpl
   //LOCAL WindowPos
   LOCAl nLineFrom, nColumnFrom, nLineTo, nColumnTo
   LOCAL sFile
   LOCAL ExtensionColors, cKey, cExtension, cColor, sGroup
   LOCAL nNewBase
   LOCAL nDeferDisplay
   LOCAL oErr
   LOCAL nLinesPerDelta
   LOCAL hxHDN

   //TraceLog( hWnd, nMsg, ::oEditor:cFile, StringPointer( ::oEditor:cFile ) )

   WITH OBJECT ::oEditor
      SWITCH nMsg
         CASE WM_PAINT
            //TraceLog()
            hDC = BeginPaint( hWnd, @ps )
            //TraceLog( ps:rcPaint:right, ps:rcPaint:left )
            ::Display( hDC )
            EndPaint( hWnd, ps )
            RETURN 0

         CASE WM_KEYUP
            ::lRepeat := .F.
            EXIT

         CASE WM_KEYDOWN
            nCount := LOWORD( nlParam )

            //TraceLog( nwParam, nlParam )

            :lAlt   := .F.
            :lCtrl  := .F.
            :lShift := .F.
            IF GetKeyState( VK_MENU ) & 0x8000 != 0
               :lAlt   := .T.
            ENDIF
            IF GetKeyState( VK_CONTROL ) & 0x8000 != 0
               :lCtrl  := .T.
            ENDIF
            IF GetKeyState( VK_SHIFT ) & 0x8000 != 0
               :lShift := .T.
            ENDIF

            //TraceLog( :lAlt, :lCtrl, :lShift )

            SWITCH nwParam
               CASE VK_UP
                  IF :lAlt
                     nKey := K_ALT_UP
                  ELSEIF :lCtrl
                     nKey := K_CTRL_UP
                  ELSEIF :lShift
                     nKey := K_SH_UP
                  ELSE
                     nKey := K_UP
                  ENDIF

                  IF ::lRepeat .AND. ::nPreviousKey == K_UP
                     nCount := Min( s_nKeyAccelerator, :nLine - 1 )
                  ENDIF
                  EXIT

               CASE VK_DOWN
                  IF :lAlt
                     nKey := K_ALT_DOWN
                  ELSEIF :lCtrl
                     nKey := K_CTRL_DOWN
                  ELSEIF :lShift
                     nKey := K_SH_DOWN
                  ELSE
                     nKey := K_DOWN
                  ENDIF

                  IF ::lRepeat .AND. ::nPreviousKey == K_DOWN
                     nCount := Min( s_nKeyAccelerator, :nLines - :nLine )
                  ENDIF
                  EXIT

               CASE VK_LEFT
                  IF :lAlt
                     nKey := K_ALT_LEFT
                  ELSEIF :lCtrl
                     nKey := K_CTRL_LEFT
                  ELSEIF :lShift
                     nKey := K_SH_LEFT
                  ELSE
                     nKey := K_LEFT
                  ENDIF

                  IF ::lRepeat .AND. ::nPreviousKey == K_LEFT
                     nCount := Min( s_nKeyAccelerator, :nColumn - 1 )
                  ENDIF
                  EXIT

               CASE VK_RIGHT
                  IF :lAlt
                     nKey := K_ALT_RIGHT
                  ELSEIF :lCtrl
                     nKey := K_CTRL_RIGHT
                  ELSEIF :lShift
                     nKey := K_SH_RIGHT
                  ELSE
                     nKey := K_RIGHT
                  ENDIF

                  IF ::lRepeat  .AND. ::nPreviousKey == K_RIGHT
                     nCount := s_nKeyAccelerator
                  ENDIF
                  EXIT

               CASE VK_HOME
                  IF :lAlt
                     nKey := K_ALT_HOME
                  ELSEIF :lCtrl
                     nKey := K_CTRL_HOME
                  ELSEIF :lShift
                     nKey := K_SH_HOME
                  ELSE
                     nKey := K_HOME
                  ENDIF

                  IF ::lRepeat .AND. ::nPreviousKey == K_HOME
                     nKey := K_CTRL_HOME
                  ENDIF
                  EXIT

               CASE VK_END
                  IF :lAlt
                     nKey := K_ALT_END
                  ELSEIF :lCtrl
                     nKey := K_CTRL_END
                  ELSEIF :lShift
                     nKey := K_SH_END
                  ELSE
                     nKey := K_END
                  ENDIF

                  IF ::lRepeat .AND. ::nPreviousKey == K_END
                     nKey := K_CTRL_END
                  ENDIF

                  EXIT

               CASE VK_PRIOR
                  IF :lAlt
                     nKey := K_ALT_PGUP
                  ELSEIF :lCtrl
                     nKey := K_CTRL_PGUP
                  ELSEIF :lShift
                     nKey := K_SH_PGUP
                  ELSE
                     nKey := K_PGUP
                  ENDIF
                  EXIT

               CASE VK_NEXT
                  IF :lAlt
                     nKey := K_ALT_PGDN
                  ELSEIF :lCtrl
                     nKey := K_CTRL_PGDN
                  ELSEIF :lShift
                     nKey := K_SH_PGDN
                  ELSE
                     nKey := K_PGDN
                  ENDIF
                  EXIT

               CASE VK_DELETE
                  IF :lAlt
                     nKey := K_ALT_DEL
                  ELSEIF :lCtrl
                     nKey := K_CTRL_DEL
                  ELSEIF :lShift
                     nKey := K_SH_DEL
                  ELSE
                     nKey := K_DEL
                  ENDIF
                  EXIT

               CASE VK_INSERT
                  IF :lAlt
                     nKey := K_ALT_INS
                  ELSEIF :lCtrl
                     nKey := K_CTRL_INS
                  ELSEIF :lShift
                     nKey := K_SH_INS
                  ELSE
                     nKey := K_INS
                  ENDIF
                  EXIT

               CASE VK_F1
                  TRY
                     :SelectWord()

                     IF :nColumnTo > 0
                        IF s_oxHDN == NIL
                           hxHDN := CreateWindowEx( 0, "xHDNWindow", "http://www.xHarbour.com/xHDN", WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN | WS_CLIPSIBLINGS, 10, 10, 600, 400, 0, 0, s_hInstance )
                        ENDIF

                        IF s_oxHDN == NIL
                           DestroyWindow( hxHDN )
                        ELSE
                           s_oxHDN:Navigate( "http://www.xHarbour.com/xHDN/referenceguide/find.asp?q=" + :SelectedText() )
                           ShowWindow( hxHDN, SW_RESTORE )
                        ENDIF
                     ENDIF
                  CATCH oErr
                     DestroyWindow( hxHDN )
                     TraceLog( oErr:Operation, oErr:Description, oErr:ProcName, oErr:ProcLine )
                  END
                  RETURN 0

               CASE VK_F3
                  IF s_hActiveDialog == ::hFindDialog
                     GetDlgItemText( ::hFindDialog, ID_Find, @sFind, 255 )

                     IF ! Empty( sFind )
                        SendMessage( ::hFindDialog, WM_COMMAND, MAKELPARAM( IDOK, BN_CLICKED ) )
                     ENDIF

                     SetFocus( ::hWnd )
                  ENDIF
                  RETURN 0

               CASE VK_F6
                  IF :lShift
                     ::PreviousWindow()
                  ELSE
                     ::NextWindow()
                  ENDIF
                  RETURN 0

               CASE VK_F7
                  ::ChooseFont()
                  RETURN 0

               CASE VK_F8
                  IF :lShift .AND. :lCtrl
                     IF :nLineFrom > 0
                        :Action( { { ED_UNSELECT, :nLineFrom, :nColumnFrom, :nLineTo, :nColumnTo, :lSquare, :nDirection } }, :aUnDo )
                     ENDIF
                     :lSquare := .T.
                  ENDIF
                  RETURN 0

               CASE VK_F9
                  :Highlight()
                  RETURN 0

               DEFAULT
                  RETURN 0
            END

            //TraceLog( LOWORD( nlParam ), HIWORD( nlParam ) )
            :OnKey( nKey, nCount )

            :lAlt   := .F.
            :lCtrl  := .F.
            :lShift := .F.

            ::nPreviousKey := nKey
            ::lRepeat := .T.
            RETURN 0

         CASE WM_CHAR
            :lAlt   := .F.
            :lCtrl  := .F.
            :lShift := .F.
            IF GetKeyState( VK_MENU ) & 0x8000 != 0
               :lAlt   := .T.
            ENDIF
            IF GetKeyState( VK_CONTROL ) & 0x8000 != 0
               :lCtrl  := .T.
            ENDIF
            IF GetKeyState( VK_SHIFT ) & 0x8000 != 0
               :lShift := .T.
            ENDIF

            //TraceLog( nwParam, :lAlt, :lCtrl, :lShift )

            SWITCH nwParam
              CASE 127
                 IF :lAlt
                    nKey := K_ALT_BS
                 ELSEIF :lCtrl
                    nKey := K_CTRL_BS
                 ELSEIF :lShift
                    nKey := K_BS
                 ELSE
                    nKey := 127
                 ENDIF
                 EXIT

               CASE VK_BACK
                 nKey := K_BS
                 EXIT

               CASE VK_TAB
                 IF :lAlt
                    nKey := K_ALT_TAB
                 ELSEIF :lCtrl
                    nKey := K_CTRL_TAB
                 ELSEIF :lShift
                    nKey := K_SH_TAB
                 ELSE
                    nKey := K_TAB
                 ENDIF
                 EXIT

               CASE VK_RETURN
                 IF :lAlt
                    nKey := K_ALT_RETURN
                 ELSEIF :lCtrl
                    nKey := K_CTRL_RETURN
                 ELSEIF :lShift
                    nKey := K_SH_RETURN
                 ELSE
                    nKey := K_RETURN
                 ENDIF
                 EXIT

               CASE VK_ESCAPE
                  nKey := K_ESC

                  IF ! s_lESCClosesWindow
                     RETURN 0
                  ELSE
                     IF ::lEscape .AND. ::CloseWindow( .T. )
                        RETURN 0
                     ENDIF
                  ENDIF
                  EXIT

               DEFAULT
                 nKey := nwParam
            END

            //TraceLog( nKey, nwParam, nlParam, :lAlt, :lCtrl, :lShift )

            IF :lCtrl .AND. nwParam >= 1 .AND. nwParam <= 26  // K_CTRL_A - Z
               :OnKey( K_Ctrl[ nwParam ],  LOWORD( nlParam ) )
            ELSE
               :OnKey( nKey,  LOWORD( nlParam ) )
            ENDIF

            :lAlt   := .F.
            :lCtrl  := .F.
            :lShift := .F.

            RETURN 0

         CASE WM_SYSCHAR
            /*
            :lAlt   := .T.

            IF GetKeyState( VK_CONTROL ) & 0x8000 != 0
               :lCtrl  := .T.
            ENDIF
            IF GetKeyState( VK_SHIFT ) & 0x8000 != 0
               :lShift := .T.
            ENDIF

            SWITCH HIWORD( nlParam ) & 0xFF // Scan Code
             CASE  2
               nKey := K_ALT_1
               EXIT
             CASE  3
               nKey := K_ALT_2
               EXIT
             CASE  4
               nKey := K_ALT_3
               EXIT
             CASE  5
               nKey := K_ALT_4
               EXIT
             CASE  6
               nKey := K_ALT_5
               EXIT
             CASE  7
               nKey := K_ALT_6
               EXIT
             CASE  8
               nKey := K_ALT_7
               EXIT
             CASE  9
               nKey := K_ALT_8
               EXIT
             CASE 10
               nKey := K_ALT_9
               EXIT
             CASE 11
               nKey := K_ALT_0
               EXIT
             CASE 13
               nKey := K_ALT_EQUALS
               EXIT
             CASE 14
               nKey := K_ALT_BS
               EXIT
             CASE 16
               nKey := K_ALT_Q
               EXIT
             CASE 17
               nKey := K_ALT_W
               EXIT
             CASE 18
               nKey := K_ALT_E
               EXIT
             CASE 19
               nKey := K_ALT_R
               EXIT
             CASE 20
               nKey := K_ALT_T
               EXIT
             CASE 21
               nKey := K_ALT_Y
               EXIT
             CASE 22
               nKey := K_ALT_U
               EXIT
             CASE 23
               nKey := K_ALT_I
               EXIT
             CASE 24
               nKey := K_ALT_O
               EXIT
             CASE 25
               nKey := K_ALT_P
               EXIT
             CASE 30
               nKey := K_ALT_A
               EXIT
             CASE 31
               nKey := K_ALT_S
               EXIT
             CASE 32
               nKey := K_ALT_D
               EXIT
             CASE 33
               nKey := K_ALT_F
               EXIT
             CASE 34
               nKey := K_ALT_G
               EXIT
             CASE 35
               nKey := K_ALT_H
               EXIT
             CASE 36
               nKey := K_ALT_J
               EXIT
             CASE 37
               nKey := K_ALT_K
               EXIT
             CASE 38
               nKey := K_ALT_L
               EXIT
             CASE 44
               nKey := K_ALT_Z
               EXIT
             CASE 45
               nKey := K_ALT_X
               EXIT
             CASE 46
               nKey := K_ALT_C
               EXIT
             CASE 47
               nKey := K_ALT_V
               EXIT
             CASE 48
               nKey := K_ALT_B
               EXIT
             CASE 49
               nKey := K_ALT_N
               EXIT
             CASE 50
               nKey := K_ALT_M
               EXIT

             DEFAULT
               nKey := nwParam
               EXIT
           END

           :OnKey( nKey,  LOWORD( nlParam ) )
           RETURN 0
           */

           //TraceLog( nwParam, HIWORD( nlParam ) & 0xFF )

           SWITCH nwParam
              CASE VK_BACK
                 :OnKey( K_ALT_BS,  LOWORD( nlParam ) )
                 EXIT

              DEFAULT
                 SendMessage( ::hContainer, WM_SYSCOMMAND, SC_KEYMENU, nwParam )
           END

           RETURN 0

         CASE WM_LBUTTONDBLCLK
            nRow    := Int( HIWORD( nlParam ) / ::TextMetric:tmHeight )
            nColumn := Round( ( LOWORD( nlParam ) - LEFT_PAD ) / ::TextMetric:tmAveCharWidth, 0 )
            nColumn := Max( 0, nColumn )

            /*
            IF :nLineFrom > 0
               // Normalize.
               :Action( { { ED_GOTO, :nLine, :nColumn }, { ( 2 << 8 ) | ED_UNSELECT, :nLineFrom, :nColumnFrom, :nLineTo, :nColumnTo, :lSquare, :nDirection } }, :aUnDo )
            ENDIF
            */

            ::GoRow( nRow )
            ::GoColumn( nColumn )

            IF ! Empty( :pHitTest )
               HB_Exec( :pHitTest, , nRow, nColumn, HB_QWith() )
            ENDIF

            :SelectWord()
            RETURN 0

         CASE WM_LBUTTONDOWN
            IF GetFocus() != hWnd
               SetFocus( hWnd )
            ENDIF

            nX := LOWORD( nlParam )
            nY := HIWORD( nlParam )

            nRow := Int( nY / ::TextMetric:tmHeight )

            IF nX < LEFT_PAD
               IF ::nRow == -1 .OR. ::nColumn == -1
                  ShowCaret( hWnd )

                  ::nRow       := 0
                  ::nColumn    := 0

                  :CurrentLine := ::BaseLine
                  :nLine       := ::nBaseLine
                  :nColumn     := 1
               ENDIF

               ::nColumn := 0
               ::GoRow( nRow )

               :OnKey( K_CTRL_E, 1 )
               RETURN 0
            ENDIF

            nColumn := Round( ( nX - LEFT_PAD ) / ::TextMetric:tmAveCharWidth, 0 )
            nColumn := Max( 0, nColumn )

            IF nwParam & MK_SHIFT != 0
               IF :nLineFrom == 0
                  :nDirection  := 0
                  :nLineFrom   := :nLine
                  :nColumnFrom := :nColumn
               ENDIF

               SendMessage( hWnd, WM_MOUSEMOVE, MK_LBUTTON, MAKELPARAM( ( nColumn + 1 ) * ::TextMetric:tmAveCharWidth, ( nRow ) * ::TextMetric:tmHeight ) )
            ELSE
               IF :nLineFrom > 0
                  // Normalize.
                  :Action( { { ED_GOTO, :nLine, :nColumn }, { ( 2 << 8 ) | ED_UNSELECT, :nLineFrom, :nColumnFrom, :nLineTo, :nColumnTo, :lSquare, :nDirection } }, :aUnDo )
               ENDIF

               //TraceLog( ::nRow, ::nColumn, :nLine, ::nBaseLine )

               IF ::nRow == -1 .OR. ::nColumn == -1
                  ShowCaret( hWnd )

                  ::nRow       := 0
                  ::nColumn    := 0

                  :CurrentLine := ::BaseLine
                  :nLine       := ::nBaseLine
                  :nColumn     := 1
               ENDIF

               ::GoRow( nRow )
               ::GoColumn( nColumn )

               :lSquare     := GetKeyState( VK_MENU ) & 0x8000 != 0
               :nDirection  := 0

               :nLineFrom   := :nLine
               :nColumnFrom := :nColumn
               :nLineTo     := 0
               :nColumnTo   := 0
            ENDIF

            :nLastX      := nX
            :nLastY      := nY
            :nLastLine   := ::nBaseLine + nRow + 1
            :nLastColumn := ::nBaseColumn + nColumn + 1

            SetCapture( hWnd )
            RETURN 0

         CASE WM_MOUSEMOVE
            nX := LOWORD( nlParam )

            IF nwParam & MK_LBUTTON == 0
               IF nX < LEFT_PAD
                  SetCursor( LoadCursor( NIL, IDC_ARROW ) )
               ELSE
                  SetCursor( LoadCursor( NIL, IDC_IBEAM ) )
               ENDIF
            ELSE
               IF nX >= 32768
                  nX -= 65535
               ENDIF

               nY := HIWORD( nlParam )
               IF nY >= 32768
                  nY -= 65535
               ENDIF

               IF :nLineFrom == 0 //.OR. :nColumnFrom == 0
                  RETURN 0
               ENDIF

               //TraceLog( nY, nX, :nLineFrom, :nColumnFrom, :nLineTo, :nColumnTo )

               nLine := ::nBaseLine + Int( nY / ::TextMetric:tmHeight ) + 1
               nLine := Max( 1, nLine )

               IF nX > 0
                  nColumn := ::nBaseColumn + Round( ( nX - LEFT_PAD ) / ::TextMetric:tmAveCharWidth, 0 ) + 1
               ELSE
                  nColumn := ::nBaseColumn - Round( Abs( nX + LEFT_PAD ) / ::TextMetric:tmAveCharWidth, 0 )
               ENDIF

               //TraceLog( :nDirection, nColumn, :nLineFrom, :nColumnFrom )

               IF :lSquare .AND. :nColumnTo > 0
               ELSE
                  nColumn := Min( Len( :GetLine( nLine )[ ED_BUFFER ] ) + 1, nColumn )
               ENDIF
               nColumn := Max( 0, nColumn )

               IF nLine > :nLines
                  nColumn := Len( :GetLine( nLine )[ ED_BUFFER ] ) + 1
                  nLine := :nLines
               ENDIF

               //TraceLog( nY, nX, nLine, nColumn, :nLastLine, :nLastColumn, :nLineFrom, :nColumnFrom, :nLineTo, :nColumnTo, ::nBaseLine, ::nBaseColumn, :nLines )

               IF nLine == :nLastLine .AND. nColumn == :nLastColumn
                  //TraceLog( "!!!" )
                  RETURN 0
               ENDIF

               //TraceLog( :nLastLine, :nLastColumn, nLine, nColumn, ::nBaseLine, ::nBaseColumn, :nLineFrom, :nColumnFrom, :nLineTo, :nColumnTo )

               IF nLine > ::nBaseLine + ::nRows
                  IF nY > :nLastY
                     IF :nDirection == 0
                        :nDirection := 1
                     ENDIF
                     :nLineTo    := nLine
                     :nColumnTo  := nColumn
                     :GoBaseLine( nLine - ::nRows )
                  ENDIF
               ELSEIF nLine <= ::nBaseLine
                  IF nY < :nLastY
                     IF :nDirection == 0
                        :nDirection  := -1
                        :nLineTo   := :nLineFrom
                        :nColumnTo := :nColumnFrom
                     ENDIF
                     :nLineFrom   := nLine
                     :nColumnFrom := nColumn
                     :GoBaseLine( nLine - 1 )
                     //TraceLog( nLine, ::nBaseLine )
                  ENDIF
               ELSEIF nColumn > ::nBaseColumn + ::nColumns + 1
                  //TraceLog( :nDirection, nColumn, ::nBaseColumn, ::nColumns )
                  IF nX > :nLastX
                     IF :nDirection == 0
                        :nDirection  := 1
                        :nLineTo     := nLine
                        :nColumnTo   := nColumn - 1
                     ELSEIF :nDirection == 1
                        :nLineTo     := nLine
                        :nColumnTo   := nColumn - 1
                     ELSE
                        :nLineFrom   := nLine
                        :nColumnFrom := nColumn
                     ENDIF

                     ::nBaseColumn := nColumn - ::nColumns - 1
                     ::nColumn := ::nColumns
                     ::Display()
                  ENDIF
               ELSEIF nColumn > 0 .AND. nColumn <= ::nBaseColumn
                  //TraceLog( "***", nX, :nLastX, :nDirection, ::nBaseColumn, nColumn )
                  IF nX < :nLastX
                     IF :nDirection == 0
                        :nDirection  := -1
                        :nLineTo     := :nLineFrom
                        :nColumnTo   := :nColumnFrom - 1
                        :nLineFrom   := nLine
                        :nColumnFrom := nColumn
                     ELSEIF :nDirection == 1
                        :nLineTo     := nLine
                        :nColumnTo   := nColumn - 1
                     ELSE
                        :nLineFrom   := nLine
                        :nColumnFrom := nColumn
                     ENDIF

                     ::nBaseColumn := nColumn - 1
                     ::nColumn := 0
                     ::Display()
                  ENDIF
               ELSE
                  SWITCH :nDirection
                     CASE 0
                        //TraceLog( nLine, nColumn, :nLastLine, :nLastColumn, :nLineFrom, :nColumnFrom )

                        // nLineTo does NOT yet have any value!
                        IF nLine > :nLineFrom .OR. ( nLine == :nLineFrom .AND. nColumn > :nColumnFrom )
                           :nDirection  := 1
                           :nLineTo     := nLine
                           :nColumnTo   := nColumn - 1
                        ELSEIF nLine < :nLineFrom .OR. ( nLine == :nLineFrom .AND. nColumn < :nColumnFrom )
                           :nDirection  := -1
                           :nLineTo     := :nLineFrom
                           :nColumnTo   := :nColumnFrom - 1
                           :nLineFrom   := nLine
                           :nColumnFrom := nColumn
                        ELSE
                           //TraceLog( "Oops" )
                           EXIT
                        ENDIF


                        //TraceLog( :nDirection, :nLineFrom, :nColumnFrom, :nLineTo, :nColumnTo, :nLines )

                        // Initial
                        //::ShowSelection()
                        nLineFrom   := :nLineFrom
                        nColumnFrom := :nColumnFrom
                        nLineTo     := :nLineTo
                        nColumnTo   := :nColumnTo

                        :nLineFrom   := 0
                        :nColumnFrom := 0
                        :nLineTo     := 0
                        :nColumnTo   := 0

                        :Action( { { ED_SELECT, nLineFrom, nColumnFrom, nLineTo, nColumnTo, :lSquare, :nDirection } }, :aUnDo )
                        EXIT

                     CASE 1
                        IF :lSquare
                           // Normalize.
                           ::ShowSelection()

                           IF nLine > :nLineFrom
                              :nLineTo := nLine
                           ELSE
                              :nLineFrom  := nLine
                           ENDIF

                           IF nColumn > :nColumnFrom
                              :nColumnTo := nColumn - 1
                           ELSE
                              :nDirection  := -1
                              :nColumnTo   := :nColumnFrom
                              :nColumnFrom := nColumn
                           ENDIF

                           ::ShowSelection()
                        ELSEIF nLine == :nLineFrom .AND. nColumn == :nColumnFrom
                           // Cancel
                           ::ShowSelection()

                           :nDirection  := 0
                           :nLineTo     := 0
                           :nColumnTo   := 0
                        ELSEIF nLine < :nLineFrom .OR. ( nLine == :nLineFrom .AND. nColumn < :nColumnFrom )
                           // Cancel
                           ::ShowSelection()

                           // Reverse
                           :nDirection  := -1
                           :nLineTo     := :nLineFrom
                           :nColumnTo   := :nColumnFrom - 1
                           :nLineFrom   := nLine
                           :nColumnFrom := nColumn

                           // New Start
                           ::ShowSelection()
                        ELSE
                           IF nLine > :nLineTo .OR. ( nLine == :nLineTo .AND. nColumn > :nColumnTo + 1 )
                              // Growing
                              ::ShowSelection( :nLineTo, :nColumnTo + 1, nLine, nColumn - 1 )
                           ELSE
                              // Shrinking
                              ::ShowSelection( nLine, nColumn, :nLineTo, :nColumnTo )
                           ENDIF

                           :nLineTo     := nLine
                           :nColumnTo   := nColumn - 1
                        ENDIF
                        EXIT

                     CASE -1
                        IF :lSquare
                           // Normalize.
                           ::ShowSelection()

                           IF nLine < :nLineTo
                              :nLineFrom := nLine
                           ELSE
                              :nLineTo := nLine
                           ENDIF

                           IF nColumn < :nColumnTo
                              :nColumnFrom := nColumn
                           ELSE
                              :nDirection  := 1
                              :nColumnFrom := :nColumnTo
                              :nColumnTo   := nColumn
                           ENDIF

                           ::ShowSelection()
                        ELSEIF nLine == :nLineTo .AND. nColumn == :nColumnTo + 1
                           //TraceLog( "Cancel", nLine, nColumn )
                           // Cancel
                           ::ShowSelection()

                           :nDirection  := 0
                           :nLineFrom   := nLine
                           :nColumnFrom := nColumn
                           :nLineTo     := 0
                           :nColumnTo   := 0

                           //nColumn--
                        ELSEIF nLine > :nLineTo .OR. ( nLine == :nLineTo .AND. nColumn > :nColumnTo + 1 )
                           //TraceLog( "Reverse" )
                           // Cancel
                           ::ShowSelection()

                           // Reverse
                           :nDirection  := 1
                           :nLineFrom   := :nLineTo
                           :nColumnFrom := :nColumnTo + 1
                           :nLineTo     := nLine
                           :nColumnTo   := nColumn - 1

                           // New Start
                           ::ShowSelection()
                        ELSE
                           IF nLine < :nLineFrom .OR. ( nLine == :nLineFrom .AND. nColumn < :nColumnFrom )
                               // Growing
                              ::ShowSelection( nLine, nColumn, :nLineFrom, :nColumnFrom - 1 )
                           ELSE
                              // Shrinking
                              ::ShowSelection( :nLineFrom, :nColumnFrom, nLine, nColumn - 1 )
                           ENDIF

                           :nLineFrom   := nLine
                           :nColumnFrom := nColumn
                        ENDIF
                        EXIT
                  END
               ENDIF

               //TraceLog( nLine, nColumn, ::nBaseLine, ::nBaseColumn, ::nRow, ::nColumn )

               // Reset
               IF ::nRow == -1 .OR. ::nColumn == -1
                  ShowCaret( hWnd )
                  ::nRow    := 0
                  ::nColumn := 0
               ENDIF

               ::GoRow( Max( 0, nLine - ::nBaseLine - 1 ) )
               ::GoColumn( Max( 0, ( nColumn - ::nBaseColumn - 1 ) ) )

               :nLastX      := nX
               :nLastY      := nY
               :nLastLine   := nLine
               :nLastColumn := nColumn
            ENDIF
            RETURN 0

         CASE WM_LBUTTONUP
            IF :nLineTo == 0
               // Normalize.
               ::ShowSelection()

               :nDirection  := 0
               :nLineFrom   := 0
               :nColumnFrom := 0
               :nLineTo     := 0
               :nColumnTo   := 0
            ELSE
               IF Len( :aUnDo ) > 0 .AND. :aUndo[-1][1] == ED_UNSELECT
                  :aUnDo[-1][2] := :nLineFrom
                  :aUnDo[-1][3] := :nColumnFrom
                  :aUnDo[-1][4] := :nLineTo
                  :aUnDo[-1][5] := :nColumnTo
                  :aUnDo[-1][6] := :lSquare
                  :aUnDo[-1][7] := :nDirection
               ELSE
                  aAdd( :aUndo, { ED_UNSELECT, :nLineFrom, :nColumnFrom, :nLineTo, :nColumnTo, :lSquare, :nDirection } )
               ENDIF
            ENDIF

            // Reset
            IF ::nRow == -1 .OR. ::nColumn == -1
               nRow    := :nLastLine - ::nBaseLine - 1
               nColumn := :nLastColumn - ::nBaseColumn - 1

               ::nRow    := 0
               ::nColumn := Max( 0, nColumn )

               ShowCaret( hWnd )
               ::GoRow( nRow )
            ENDIF

            ReleaseCapture()

            #ifdef VXH
               Application:Project:EditReset(0)
            #endif
            RETURN 0

         CASE WM_RBUTTONUP
            hPopup := LoadMenu( s_hInstance, "EDITCONTEXT" )
            hContext := GetSubMenu( hPopup, 0 )

            EnableMenuItem(  hContext, IDM_EDIT_UNDO,     MF_BYCOMMAND | IIF( Len( :aUnDo ) > 0 ,MF_ENABLED, MF_GRAYED ) )
            EnableMenuItem(  hContext, IDM_EDIT_REDO,     MF_BYCOMMAND | IIF( Len( :aReDo ) > 0 ,MF_ENABLED, MF_GRAYED ) )
            EnableMenuItem(  hContext, IDM_EDIT_UNDOFAST, MF_BYCOMMAND | IIF( aScan( :aUndo, {|aAction| aAction[1] & 0xFF < ED_NOIMPACT } ) > 0, MF_ENABLED, MF_GRAYED ) )
            EnableMenuItem(  hContext, IDM_EDIT_REDOFAST, MF_BYCOMMAND | IIF( aScan( :aRedo, {|aAction| aAction[1] & 0xFF < ED_NOIMPACT } ) > 0, MF_ENABLED, MF_GRAYED ) )

            EnableMenuItem(  hContext, IDM_EDIT_PASTE,    MF_BYCOMMAND | IIF( IsClipboardFormatAvailable( CF_TEXT ), MF_ENABLED, MF_GRAYED ) )

            IF :nLineFrom == 0
               nEnable = MF_BYCOMMAND | MF_GRAYED
            ELSE
               nEnable = MF_BYCOMMAND | MF_ENABLED
            ENDIF

            EnableMenuItem ( hContext, IDM_EDIT_CUT,   nEnable)
            EnableMenuItem ( hContext, IDM_EDIT_COPY,  nEnable)
            EnableMenuItem ( hContext, IDM_EDIT_CLEAR, nEnable)

            Point := (struct POINT)
            Point:x := LOWORD( nlParam )
            Point:y := HIWORD( nlParam )

            ClientToScreen( hWnd, @Point )

            TrackPopupMenu( hContext, TPM_RIGHTBUTTON, Point:X, Point:y, 0, hWnd, NIL )

            DestroyMenu( hPopup )
            RETURN 0

         CASE WM_SETFOCUS
            IF CreateCaret( hWnd, NIL, IIF( :lInsert, 2, ::TextMetric:tmAveCharWidth ), ::TextMetric:tmHeight )
               //TraceLog( ::TextMetric:tmAveCharWidth, ::nColumn, ::TextMetric:tmHeight, ::nRow )
               SetPos( ::nRow, ::nColumn )

               IF ::nRow >= 0 .AND. ::nColumn >= 0
                  ShowCaret( hWnd )
               ENDIF
            ENDIF
            RETURN 0//EXIT

         CASE WM_KILLFOCUS
            //OutputDebugString("KILLED")
            DestroyCaret()
            RETURN 0//EXIT

         CASE WM_ERASEBKGND
            RETURN 1

         CASE WM_MOUSEWHEEL
            SystemParametersInfo( SPI_GETWHEELSCROLLLINES, 0, @nLinesPerDelta, 0)
            nNewBase := Max( ::nBaseLine - Int( GET_WHEEL_DELTA_WPARAM( nwParam ) / WHEEL_DELTA * nLinesPerDelta ), 0 )

            IF ::nRow >= 0
               IF :nLine > nNewBase .AND. :nLine <= nNewBase + ::nRows
                  ::nRow := :nLine - nNewBase - 1
               ELSE
                  ::nRow := -1

                  IF ::nColumn >= 0
                     //TraceLog( "Hid" )
                     HideCaret( hWnd )
                  ENDIF
               ENDIF
            ENDIF

            :GoBaseLine( nNewBase )
            RETURN 0

         CASE WM_VSCROLL
            SWITCH Int( LOWORD( nwParam ) )
               /*
               // user clicked the HOME keyboard key
               CASE SB_TOP
                 IF ::nRow >= 0
                    ::nRow := -1
                    IF ::nColumn >= 0
                       HideCaret( ::hWnd )
                    ENDIF
                 ENDIF

                 :Home()
                 RETURN 0

               // user clicked the END keyboard key
               CASE SB_BOTTOM
                 IF ::nRow >= 0
                    ::nRow := -1
                    IF ::nColumn >= 0
                       HideCaret( ::hWnd )
                    ENDIF
                 ENDIF

                 :End()
                 RETURN 0
               */

               // user clicked the top arrow
               CASE SB_LINEUP
                 //TraceLog( "UP", :nLine, ::nBaseLine, ::nRow )
                 IF ::nRow >= 0
                    ::nRow := -1
                    IF ::nColumn >= 0
                       HideCaret( ::hWnd )
                    ENDIF
                 ENDIF

                 :GoBaseLine( ::nBaseLine - 1 )
                 RETURN 0

               // user clicked the bottom arrow
               CASE SB_LINEDOWN
                 //TraceLog( "DOWN", :nLine, ::nBaseLine, ::nRow )
                 IF ::nRow >= 0
                    ::nRow := -1
                    IF ::nColumn >= 0
                       HideCaret( ::hWnd )
                       //Alert( "Hid" )
                    ENDIF
                 ENDIF

                 :GoBaseLine( ::nBaseLine + 1, .T. )
                 RETURN 0

               // user clicked the scroll bar shaft above the scroll box
               CASE SB_PAGEUP
                 IF ::nRow >= 0
                    ::nRow := -1
                    IF ::nColumn >= 0
                       HideCaret( ::hWnd )
                    ENDIF
                 ENDIF

                 :GoBaseLine( ::nBaseLine - ::nRows )
                 RETURN 0

               // user clicked the scroll bar shaft below the scroll box
               CASE SB_PAGEDOWN
                 IF ::nRow >= 0
                    ::nRow := -1
                    IF ::nColumn >= 0
                       HideCaret( ::hWnd )
                    ENDIF
                 ENDIF

                 :GoBaseLine( ::nBaseLine + ::nRows )
                 RETURN 0

               // user dragged the scroll box
               CASE SB_THUMBTRACK
                 si IS SCROLLINFO
                 si:cbSize := si:SizeOf
                 si:fMask  = SIF_TRACKPOS
                 GetScrollInfo( hwnd, SB_VERT, @si )

                 IF ::nRow >= 0
                    ::nRow := -1
                    IF ::nColumn >= 0
                       HideCaret( ::hWnd )
                    ENDIF
                 ENDIF

                 :GoBaseLine( si:nTrackPos, .T. )
                 RETURN 0
            END
            EXIT

         CASE WM_HSCROLL
            SWITCH Int( LOWORD( nwParam ) )
               // user clicked the top arrow
               CASE SB_LINELEFT
                 IF ::nBaseColumn > 0
                    ::nBaseColumn--
                 ELSE
                    RETURN 0
                 ENDIF
                 EXIT

               // user clicked the bottom arrow
               CASE SB_LINERIGHT
                 IF ::nBaseColumn < :nLongestLine - ::nColumns
                    ::nBaseColumn++
                 ELSE
                    RETURN 0
                 ENDIF
                 EXIT

               // user clicked the scroll bar shaft above the scroll box
               CASE SB_PAGELEFT
                 IF ::nBaseColumn > ::nColumns
                    ::nBaseColumn -= ::nColumns
                 ELSEIF ::nBaseColumn > 0
                    ::nBaseColumn := 0
                 ELSE
                    RETURN 0
                 ENDIF
                 EXIT

               // user clicked the scroll bar shaft below the scroll box
               CASE SB_PAGERIGHT
                 IF ::nBaseColumn < :nLongestLine - ::nColumns
                    ::nBaseColumn += ::nColumns
                 ELSEIF ::nBaseColumn < :nLongestLine
                    ::nBaseColumn := :nLongestLine - ::nColumns
                 ELSE
                    RETURN 0
                 ENDIF
                 EXIT

               // user dragged the scroll box
               CASE SB_THUMBTRACK
                 si IS SCROLLINFO
                 si:cbSize := si:SizeOf
                 si:fMask  = SIF_TRACKPOS
                 GetScrollInfo( hwnd, SB_HORZ, @si )

                 IF ::nColumn >= 0
                    ::nColumn := -1
                    IF ::nRow >= 0
                       HideCaret( ::hWnd )
                    ENDIF
                 ENDIF

                 ::nBaseColumn := si:nTrackPos

                 ::Display()
                 RETURN 0

               DEFAULT
                 RETURN 0
            END

            :nLine := ::nBaseLine + ::nRow + 1
            :nColumn := ::nBaseColumn + ::nColumn + 1
            ::Display()
            RETURN 0

         CASE WM_WINDOWPOSCHANGED
         //CASE WM_SIZE

            //nRows   := Int( ( HIWORD( nlParam ) /*- ::nStatusHeight*/ ) / ::TextMetric:tmHeight )
            //nColumns := Int( ( LOWORD( nlParam ) - LEFT_PAD - 1 ) / ::TextMetric:tmAveCharWidth )

            //WindowPos := (struct WINDOWPOS)
            //WindowPos:Pointer( nlParam )
            //nRows   := Int( ( WindowPos:cY - ::nStatusHeight ) / ::TextMetric:tmHeight )
            //nColumns := Int( ( WindowPos:cX - LEFT_PAD - 1 ) / ::TextMetric:tmAveCharWidth )

            GetClientRect( hWnd, @Rect )

            IF Rect:right <= 0 .OR. Rect:bottom <= 0
               // This fixes a problem minimizing the host window, it will set
               // ::nRow to -1 and the caret will never show again when restoring
               RETURN 0
            ENDIF

            nDeferDisplay := ::nDeferDisplay
            ::nDeferDisplay := 0

            nRows   := Int( ( Rect:bottom /*- ::nStatusHeight*/ ) / ::TextMetric:tmHeight )
            nColumns := Int( ( Rect:right - LEFT_PAD - 1 ) / ::TextMetric:tmAveCharWidth )

            //TraceLog( nRows, nColumns )

            //::nFillerLength := nColumns + 1
            //::sFiller := Space( ::nFillerLength )

            IF ProcName(3) != "SETSCROLLINFO"
               //TraceLog( "+++" )
               si IS SCROLLINFO

               WITH OBJECT si
                  :cbSize    := :SizeOf
                  :fMask     := SIF_ALL
                  :nMin      := 0
                  :nMax      := Max( ::nBaseLine + ::nRows - 1, ::oEditor:nLines - 1 )
                  :nPage     := nRows
                  :nPos      := ::nBaseLine
                  :nTrackPos := 0
               END
               SetScrollInfo( hWnd, SB_VERT, si, .T. )

               WITH OBJECT si
                  //:cbSize  := :SizeOf
                  //:fMask   := SIF_ALL
                  //:nMin    := 0
                  :nMax      := ::oEditor:nLongestLine - 1
                  :nPage     := ::nColumns
                  :nPos      := ::nBaseColumn
                  :nTrackPos := 0
               END

               SetScrollInfo( hWnd, SB_HORZ, si, .T. )
            ENDIF

            //TraceLog( nRows, ::nRows, nColumns, ::nColumns, :CurrentLine[ ED_BUFFER ] )

            InvalidateRect( hWnd )

            ::nRows := nRows
            ::nRow := Min( ::nRow, nRows - 1 )

            ::nColumns := nColumns
            ::nColumn := Min( ::nColumn, nColumns - 1 )

            ::nDeferDisplay := nDeferDisplay

            //TraceLog( ::nRows, ::nColumns, ::TextMetric:tmHeight )
            RETURN 0

         CASE EN_GETEDITOR
            RETURN ArrayPointer( ::oEditor )

         CASE EN_SETSTATUSWINDOW
            //TraceLog( nwParam, nlParam )
            ::hStatusBar := nlParam
            ::Display()
            RETURN 0

         CASE WM_SETTEXT
            :Load( NIL, StringFromPointer( nlParam ), .F. )
            ::Display()
            RETURN 0

         CASE WM_GETTEXT
            CopyStringToPointer( nlParam, :GetBuffer, nwParam )
            RETURN 0

         CASE WM_COMMAND
            SWITCH LOWORD( nwParam )
               CASE IDM_EDIT_UNDOFAST
                  :lShift := .T.
               CASE IDM_EDIT_UNDO
                  :OnKey( K_CTRL_Z, 1 )
                  :lShift := .F.
                  RETURN 0

               CASE IDM_EDIT_REDOFAST
                  :lShift := .T.
               CASE IDM_EDIT_REDO
                  :OnKey( K_CTRL_Y, 1 )
                  :lShift := .F.
                  RETURN 0

               CASE IDM_EDIT_CUT
                  :OnKey( K_CTRL_X, 1 )
                  RETURN 0

               CASE IDM_EDIT_COPY
                  :OnKey( K_CTRL_C, 1 )
                  RETURN 0

               CASE IDM_EDIT_PASTE
                  :OnKey( K_CTRL_V, 1 )
                  RETURN 0

               CASE IDM_EDIT_CLEAR
                  :OnKey( K_DEL, 1 )
                  RETURN 0

               CASE IDM_EDIT_SELECT_ALL
                  :Home()
                  :lShift := .T.
                  :End()
                  :lShift := .F.
                  RETURN 0

               CASE IDM_EDIT_FIND
                  :OnKey( K_CTRL_F, 1 )
                  RETURN 0

               CASE IDM_EDIT_REPLACE
                  :OnKey( K_CTRL_H, 1 )
                  RETURN 0

               CASE IDM_EDIT_GOTO
                  :OnKey( K_CTRL_G, 1 )
                  RETURN 0
            END
            RETURN 0

         CASE WM_GETDLGCODE
            RETURN DLGC_WANTMESSAGE

         //CASE WM_NCPAINT
         //   RETURN 0

         CASE WM_CLOSE
            DestroyWindow( hWnd )
            // FALL thriough!!!

         CASE WM_DESTROY
            //TraceLog( "Destroy" )

            cPath := HB_Argv(0)
            nAt := RAt( DIR_SEPARATOR, cPath )
            cPath := Left( cPath, nAt )

            TRY
               aIni := HB_ReadIni( cPath + "xedit.ini", .T., "=" )
            CATCH
            END

            IF Empty( aIni )
               aIni := Hash()
               aIni[ "MAIN" ] := Hash()
            ENDIF

            IF ! Empty( ::hContainer )
               GetWindowPlacement( ::hContainer, @wndpl )

               IF wndpl:showCmd == SW_SHOWMINIMIZED
                  aIni[ "MAIN" ][ "Show" ] := SW_SHOWNORMAL
               ELSE
                  aIni[ "MAIN" ][ "Show" ] := wndpl:showCmd
               ENDIF
               aIni[ "MAIN" ][ "Left"       ] := wndpl:rcNormalPosition:left
               aIni[ "MAIN" ][ "Top"        ] := wndpl:rcNormalPosition:top
               aIni[ "MAIN" ][ "Right"      ] := wndpl:rcNormalPosition:right
               aIni[ "MAIN" ][ "Bottom"     ] := wndpl:rcNormalPosition:bottom
            ENDIF

            aIni[ "MAIN" ][ "Font"                     ] := s_cFont
            aIni[ "MAIN" ][ "FontSize"                 ] := s_nFontSize
            aIni[ "MAIN" ][ "FontWeight"               ] := s_nFontWeight
            aIni[ "MAIN" ][ "CharSet"                  ] := s_nCharSet
            aIni[ "MAIN" ][ "FocusLine"                ] := s_lFocusLine
            aIni[ "MAIN" ][ "AutoSave"                 ] := s_lAutoSave
            aIni[ "MAIN" ][ "Tab"                      ] := s_nTabSpaces
            aIni[ "MAIN" ][ "Force EOL"                ] := s_lForceEOL
          #if defined( XEDIT_DYN_ACCELRATOR )
            aIni[ "MAIN" ][ "Key Accelerator"          ] := s_nKeyAccelerator
          #endif
            aIni[ "MAIN" ][ "ESC Closes active window" ] := s_lESCClosesWindow
            aIni[ "MAIN" ][ "Reset Modified on Save"   ] := s_lResetOnSave

            aIni[ "Find" ] := Hash()
            FOR EACH sFind IN s_aFind
               aIni["Find"][ Right( '0' + LTrim( Str( HB_EnumIndex() , 2 ) ), 2 ) ] := sFind
            NEXT

            aIni[ "Replace" ] := Hash()
            FOR EACH sReplace IN s_aReplace
               aIni["Replace"][ Right( '0' + LTrim( Str( HB_EnumIndex() , 2 ) ), 2 ) ] := sReplace
            NEXT

            aIni[ "Recent Files" ] := Hash()
            FOR EACH sFile IN s_aFiles
               aIni["Recent Files"][ Right( '0' + LTrim( Str( HB_EnumIndex() , 2 ) ), 2 ) ] := sFile
            NEXT

            aIni[ "Recent Workspaces" ] := Hash()
            FOR EACH sFile IN s_aWorkspaces
               aIni["Recent Workspaces"][ Right( '0' + LTrim( Str( HB_EnumIndex() , 2 ) ), 2 ) ] := sFile
            NEXT

            FOR EACH cExtension IN s_CustomColors:Keys
               ExtensionColors := s_CustomColors[ cExtension ]

               // Ignoring aliases
               IF ! ExtensionColors[ "Name" ] = "*." + cExtension
                  LOOP
               ENDIF

               sGroup :=  ExtensionColors[ "Name" ]
               sGroup := StrTran( sGroup, "*", "" )
               sGroup := StrTran( sGroup, ";", "" )
               sGroup := StrTran( sGroup, " ", "" )

               //TraceLog( ExtensionColors[ "Name" ] )
               aIni[ sGroup ] := Hash()

               TRY
                  //TraceLog( "Initializer", ExtensionColors[ "Initializer" ] )
                  aIni[ sGroup ][ "Initializer" ] := ExtensionColors[ "Initializer" ]
               CATCH
               #if 0
                  //TraceLog( "Initializer", "" )
                  aIni[ sGroup ][ "Initializer" ] := ""
               #endif
               END

               TRY
                  //TraceLog( "AfterKey", ExtensionColors[ "AfterKeyScriptFile" ] + "->" + ExtensionColors[ "AfterKeyScriptProcedure" ] )
                  aIni[ sGroup ][ "AfterKey" ] := ExtensionColors[ "AfterKeyScriptFile" ] + "->" + ExtensionColors[ "AfterKeyScriptProcedure" ]
               CATCH
               #if 0
                  //TraceLog( "AfterKey", "" )
                  aIni[ sGroup ][ "AfterKey" ] := ""
               #endif

               END

               TRY
                  //TraceLog( "WinColorizer", ExtensionColors[ "WinColorizer" ] )
                  aIni[ sGroup ][ "WinColorizer" ] := ExtensionColors[ "WinColorizer" ]
               CATCH
               #if 0
                  //TraceLog( "WinColorizer", "" )
                  aIni[ sGroup ][ "WinColorizer" ] := ""
               #endif
               END

               TRY
                  //TraceLog( "ConColorizer", ExtensionColors[ "ConColorizer" ] )
                  aIni[ sGroup ][ "ConColorizer" ] := ExtensionColors[ "ConColorizer" ]
               CATCH
               #if 0
                  //TraceLog( "ConColorizer", "" )
                   aIni[ sGroup ][ "ConColorizer" ] := ""
               #endif
               END

               FOR EACH cKey IN ExtensionColors:Keys
                  DO CASE
                     CASE cKey[1] == '#'
                        // Preserve Comments.
                        aIni[ sGroup ][ cKey ] := ExtensionColors[ cKey ]
                        LOOP

                     CASE cKey == "Name"
                        LOOP

                     CASE cKey == "hExtensionTree"
                        LOOP

                     CASE cKey == "Initializer"
                        LOOP

                     CASE cKey == "WinColorizer"
                        LOOP

                     CASE cKey == "ConColorizer"
                        LOOP

                        // INTENTIONALLY = instead of == !!!
                     CASE cKey = "Colorizer"
                        LOOP

                        // INTENTIONALLY = instead of == !!!
                     CASE cKey = "AfterKey"
                       LOOP
                  ENDCASE

                  BEGIN SEQUENCE
                     FOR EACH cColor IN s_Colors:Keys
                        IF s_Colors[ cColor ] == ExtensionColors[ cKey ]
                           //TraceLog( cKey, cColor )
                           aIni[ sGroup ][ cKey ] := cColor
                           BREAK
                        ENDIF
                     NEXT

                     //TraceLog( cKey, ExtensionColors[ cKey ] )
                     aIni[ sGroup ][ cKey ] := "(" + Str( ExtensionColors[ cKey ] & 0xFF, 3 ) + ", " + Str( ( ExtensionColors[ cKey ] >> 8 ) & 0xFF, 3 ) + ", " + Str( ( ExtensionColors[ cKey ] >> 16 ) & 0xFF, 3 ) + ")"
                  END SEQUENCE
               NEXT
            NEXT

            HB_WriteIni( cPath + "xedit.ini", aIni )

            IF ::hBrush != NIL
               DeleteObject( ::hBrush )
               ::hBrush := NIL
            ENDIF

            IF ::hHighlightBrush != NIL
               DeleteObject( ::hHighlightBrush )
               ::hHighlightBrush := NIL
            ENDIF

            RETURN 0

         //CASE WM_CREATE
            // NEVER get's here, because Window Procedure is set afeter creation!!!
      END
   END

   //TraceLog( nMsg )

RETURN DefWindowProc( hWnd, nMsg, nwParam, nlParam )//CallWindowProc( ::DefWindowProc, hWnd, nMsg, nwParam, nlParam )

METHOD ShowSelection( nLineFrom, nColumnFrom, nLineTo, nColumnTo, nOnlyLine, hDC ) CLASS EditorGUIDisplay

   LOCAL Rect IS RECT
   LOCAL nLine
   LOCAL sLine
   LOCAL bReleaseDC := .F.
   LOCAL nFirstLine, nLastLine

   IF ::nDeferDisplay > 0
      RETURN Self
   ENDIF

   WITH OBJECT ::oEditor
      IF nLineFrom == NIL
         nLineFrom   := :nLineFrom
         nColumnFrom := :nColumnFrom
         nLineTo     := :nLineTo
         nColumnTo   := :nColumnTo
      ENDIF

      //TraceLog( nLineFrom, nColumnFrom, nLineTo, nColumnTo, nOnlyLine, :nLines )

      IF nLineFrom == 0 //.OR. nLineTo == 0 .OR. nColumnFrom == 0 .OR. nColumnTo == 0
         //TraceLog( "***" )
         RETURN Self
      ENDIF

      IF nLineFrom <= ::nBaseLine + ::nRows .AND. nLineTo > ::nBaseLine
         IF hDC == NIL
            bReleaseDC := .T.
            hDC := GetDC( ::hWnd )
         ENDIF

         nLine := nLineFrom

         nFirstLine := ::nBaseLine + 1
         nLastLine  := nFirstLine + ::nRows - 1

         IF nLine >= nFirstLine .AND. nLineFrom <= nLastLine .AND. ( nOnlyLine == NIL .OR. nOnlyLine == nLine )
            sLine := :GetLine( nLine )[ED_BUFFER]
            IF :lSquare .OR. nColumnFrom <= Len( sLine ) + 1
               Rect:top    := ( nLine - ::nBaseLine - 1 ) * ::TextMetric:tmHeight + 1

               IF :lSquare
                  Rect:left  := LEFT_PAD + ( nColumnFrom - ::nBaseColumn - 1 ) * ::TextMetric:tmAveCharWidth
                  Rect:right  := LEFT_PAD + ( nColumnTo - ::nBaseColumn ) * ::TextMetric:tmAveCharWidth
               ELSE
                  Rect:left   := LEFT_PAD + Max( 0, ( nColumnFrom - ::nBaseColumn - 1 ) ) * ::TextMetric:tmAveCharWidth
                  IF nLineTo > nLineFrom
                     Rect:right  := LEFT_PAD + ( Len( sLine ) + 1 - ::nBaseColumn ) * ::TextMetric:tmAveCharWidth
                  ELSE
                     Rect:right  := LEFT_PAD + ( Min( nColumnTo, Len( sLine ) + 1 ) - ::nBaseColumn ) * ::TextMetric:tmAveCharWidth
                  ENDIF
               ENDIF
               Rect:bottom := ( nLine - ::nBaseLine ) * ::TextMetric:tmHeight - 1

               //TraceLog( LEFT_PAD, Rect:left, Rect:top, Rect:right, Rect:bottom, ::TextMetric:tmAveCharWidth, nColumnFrom, nColumnTo )
               InvertRect( hDC, Rect )
            ENDIF
         ENDIF

         WHILE ++nLine < nLineTo .AND. nLine <= ::nBaseLine + ::nRows
            IF nLine >= nFirstLine .AND. nLineFrom <= nLastLine .AND. ( nOnlyLine == NIL .OR. nOnlyLine == nLine )
               sLine := :GetLine( nLine )[ED_BUFFER]

               Rect:top    := ( nLine - ::nBaseLine - 1 ) * ::TextMetric:tmHeight + 1
               IF :lSquare
                  Rect:left  := LEFT_PAD + ( nColumnFrom - ::nBaseColumn - 1 ) * ::TextMetric:tmAveCharWidth
                  Rect:right  := LEFT_PAD + ( nColumnTo - ::nBaseColumn ) * ::TextMetric:tmAveCharWidth
               ELSE
                  Rect:left   := LEFT_PAD
                  Rect:right  := LEFT_PAD + ( Len( sLine ) + 1 - ::nBaseColumn ) * ::TextMetric:tmAveCharWidth
               ENDIF
               Rect:bottom := ( nLine - ::nBaseLine ) * ::TextMetric:tmHeight - 1

               InvertRect( hDC, Rect )
            ENDIF
         END

         IF nLine == nLineTo .AND. nLine <= nLastLine .AND. ( nOnlyLine == NIL .OR. nOnlyLine == nLine )
            sLine := :GetLine( nLine )[ED_BUFFER]

            Rect:top    := ( nLine - ::nBaseLine - 1 ) * ::TextMetric:tmHeight + 1
            IF :lSquare
               Rect:left  := LEFT_PAD + ( nColumnFrom - ::nBaseColumn - 1 ) * ::TextMetric:tmAveCharWidth
               Rect:right  := LEFT_PAD + ( nColumnTo - ::nBaseColumn ) * ::TextMetric:tmAveCharWidth
            ELSE
               Rect:left   := LEFT_PAD
               nColumnTo := Min( nColumnTo, Len( sLine ) + 1 )
               Rect:right  := LEFT_PAD + Max( 0, ( Min( Len( sLine ) + 1, nColumnTo ) - ::nBaseColumn ) ) * ::TextMetric:tmAveCharWidth
            ENDIF
            Rect:bottom := ( nLine - ::nBaseLine ) * ::TextMetric:tmHeight - 1

            InvertRect( hDC, Rect )
         ENDIF

         IF bReleaseDC
            ReleaseDC( ::hWnd, hDC )
         ENDIF
      ENDIF
   END
RETURN Self

METHOD NextWindow() CLASS EditorGUIDisplay

   LOCAL nEditorID

   IF Len( s_aEditors ) > 1
      nEditorID := aScan( s_aEditors, ::oEditor, , , .T. )

      nEditorID++
      IF nEditorID > Len( s_aEditors )
         nEditorID := 1
      ENDIF

      s_aEditors[ nEditorID ]:SetDisplay( Self, .T. )
      //SendMessage( ::hContainer, WM_SETTEXT, , "xEdit - " + ::oEditor:cPath + ::oEditor:cFile )

      #ifdef VXH
         Application:Project:SourceTabChanged(, nEditorID )
         Application:SourceTabs:SetCurSel( nEditorID )
      #endif

   ENDIF

RETURN 0

METHOD PreviousWindow() CLASS EditorGUIDisplay

   LOCAL nEditorID

   IF Len( s_aEditors ) > 1
      nEditorID := aScan( s_aEditors, ::oEditor, , , .T. )

      nEditorID--
      IF nEditorID == 0
         nEditorID := Len( s_aEditors )
      ENDIF

      s_aEditors[ nEditorID ]:SetDisplay( Self, .T. )
      //SendMessage( ::hContainer, WM_SETTEXT, , "xEdit - " + ::oEditor:cPath + ::oEditor:cFile )

      #ifdef VXH
         Application:Project:SourceTabChanged(, nEditorID )
         Application:SourceTabs:SetCurSel( nEditorID )
      #endif
   ENDIF

RETURN 0

METHOD RowColToScreenPoint( nRow, nColumn ) CLASS EditorGUIDisplay

   LOCAL Point := (struct POINT )

   Point:X := nColumn * ::TextMetric:tmAveCharWidth
   Point:Y := nRow * ::TextMetric:tmHeight

   ClientToScreen( ::hWnd, @Point )

RETURN Point

PROCEDURE ChooseFont() CLASS EditorGUIDisplay

   LOCAL lf := (struct LOGFONT), cf := (struct CHOOSEFONT), hDC := GetDC( ::hWnd )

   /*
   lf:lfHeight         := s_nFontSize
   lf:lfWidth          := 0
   lf:lfEscapement     := 0
   lf:lfOrientation    := 0
   lf:lfWeight         := s_nFontWeight
   lf:lfItalic         := 0
   lf:lfUnderline      := 0
   lf:lfStrikeOut      := 0
   lf:lfCharSet        := s_nCharSet
   lf:lfOutPrecision   := OUT_DEFAULT_PRECIS
   lf:lfClipPrecision  := CLIP_DEFAULT_PRECIS
   lf:lfQuality        := DEFAULT_QUALITY
   lf:lfPitchAndFamily := FF_DONTCARE

   FOR EACH cChar IN s_cFont
      lf:lfFaceName[HB_EnumIndex()] := cChar
   NEXT
   lf:lfFaceName[ Len( s_cFont ) + 1] := 0
   */

   GetObject( s_hFont, lf:SizeOf(), @lf )
   //TraceLog( lf:lfCharSet, lf:lfPitchAndFamily, lf:lfFaceName:AsString() )


   cf:lStructSize    := cf:SizeOf()
   cf:hwndOwner      := ::hWnd
   cf:hDC            := hDC
   cf:lpLogFont      := lf//(struct LOGFONT)
   cf:iPointSize     := NIL
   cf:Flags          := CF_FIXEDPITCHONLY | CF_SCREENFONTS | CF_FORCEFONTEXIST | CF_INITTOLOGFONTSTRUCT
   cf:rgbColors      := NIL
   cf:lCustData      := NIL
   cf:lpfnHook       := NIL
   cf:lpTemplateName := NIL
   cf:hInstance      := s_hInstance
   cf:lpszStyle      := NIL
   cf:nFontType      := NIL
   cf:nSizeMin       := NIL
   cf:nSizeMax       := NIL

   //TraceLog( cf:lpLogFont:lfCharSet, cf:lpLogFont:lfPitchAndFamily, cf:lpLogFont:lfFaceName:AsString() )

   IF ChooseFont( @cf )
      //TraceLog( cf:lpLogFont:lfCharSet, cf:lpLogFont:lfPitchAndFamily, cf:lpLogFont:lfFaceName:AsString() )

      s_cFont       := cf:lpLogFont:lfFaceName:AsString()
      s_nFontSize   := cf:lpLogFont:lfHeight
      s_nFontWeight := cf:lpLogFont:lfWeight
      s_nCharSet    := cf:lpLogFont:lfCharSet

      s_hFont := CreateFontIndirect( cf:lpLogFont )

      SelectObject( hDC, s_hFont )
      GetTextMetrics( hDC, @::TextMetric )
      ::TextMetric:tmHeight += 2
      ReleaseDC( ::hWnd, hDC )

      InvalidateRect( ::hWnd, NIL, .F. )

      SendMessage( ::hWnd, WM_KILLFOCUS, 0, 0 )
      SendMessage( ::hWnd, WM_SETFOCUS, 0, 0 )
   ENDIF

RETURN

#endif

CLASS Editor

   VAR nTop, nLeft, nRows, nColumns      READONLY

   VAR oDisplay                          READONLY
   VAR cPath       INIT ""               READONLY
   VAR cFile       INIT ""               READONLY
   VAR Date
   VAR Time

   VAR lInsert     INIT .T.
   VAR lReadOnly   INIT .F.

   #ifndef VXH
      VAR lModified   INIT .F.
   #endif

   VAR FirstLine                         READONLY
   VAR LastLine                          READONLY
   VAR CurrentLine                       READONLY
   VAR RecentLine                        READONLY
   VAR BaseColumn                        READONLY

   VAR nLine       INIT 1                READONLY
   VAR nLines      INIT 1                READONLY

   VAR nColumn     INIT 1                READONLY

   VAR aBookmarks  INIT {}
   VAR aComments   INIT {}
   VAR aDumps      INIT {}
   VAR aFunctions  INIT {}

   VAR aActions
   VAR nActions
   VAR aUndo       INIT {}
   VAr aRedo       INIT {}
   VAR nLastUndo   INIT 0
   VAR nLastRedo   INIT 0

   //VAR nRecentUndo INIT 0
   //VAR nRecentRedo INIT 0

   VAR nLongestLine  INIT 1

   VAR nDirection    INIT 0
   VAR nLineFrom     INIT 0
   VAR nLineTo       INIT 0
   VAR nColumnFrom   INIT 0
   VAR nColumnTo     INIT 0
   VAR lSquare       INIT .F.
   VAR cClipBoard    INIT ""
   VAR lCLipSquare   INIT .F.
   VAR lLine         INIT .F.

   VAR nNavigation         INIT 0
   VAR nPendingColumnFrom  INIT 0

   VAR nLastX       INIT 0
   VAR nLastY       INIT 0
   VAR nLastLine    INIT 0
   VAR nLastColumn  INIT 0

   VAR lAlt         INIT .F.
   VAR lCtrl        INIT .F.
   VAR lShift       INIT .F.

   VAR nPreferedColumn INIT 0

   VAR hBKColor     INIT s_DefaultColors[ "Background" ]
   VAR pColorizer
   VAR ExtensionColors

   VAR pHitTest

   VAR pAfterKey

   VAR hFileItem

   VAR nPhysicalLine
   VAR nLastOpener
   VAR nLastCloser

   VAR hHighlightColor  INIT s_DefaultColors[ "Highlight" ]
   VAR HighlightedLine

   #ifdef VXH
      DATA xModified INIT .F.
      ACCESS lModified INLINE ::xModified
      ASSIGN lModified( lMod ) INLINE ::NotifyVXH( lMod ), ::xModified := lMod
      METHOD NotifyVXH( lMod )
   #endif

   METHOD New( nTop, nLeft, nLines, nColumns, cFile, oDisplay ) CONSTRUCTOR
   METHOD Close()

   METHOD Load( cFile, sText, lResetDisplay, lDisplay )
   METHOD Save( cFile, bAuto )
   METHOD GetBuffer()

   METHOD AddLine( Line, lDisplay )
   METHOD LoadAddLine( sLine )

   METHOD DeleteLine()
   METHOD InsertLine( Line, lDisplay )
   METHOD GoLine( nLine )
   METHOD GoBaseLine( nLine )
   METHOD GetLine( nLine )

   METHOD SetDisplay( oDisplay, lDisplay )
   METHOD SetExtension( cExt )

   METHOD PageUp()
   METHOD PageDown()

   METHOD Home()
   METHOD LineHome()
   METHOD End()
   METHOD LineEnd()

   METHOD Up()
   METHOD Down()
   METHOD Left()
   METHOD Right()

   METHOD Find( sText, nFrom, RegEx )

   METHOD Edit()

   METHOD OnKey( nKey, nCount )

   METHOD Undo()                       INLINE ::Action( ::aUndo, ::aRedo )

   INLINE METHOD UndoFast()
                                       WHILE ! Empty( ::aUnDo )
                                          IF ::Action( ::aUndo, ::aRedo )
                                             EXIT
                                          ENDIF
                                       END
         ENDMETHOD

   METHOD ReDo()                       INLINE ::Action( ::aRedo, ::aUnDo )

   INLINE METHOD RedoFast()
                                       WHILE ! Empty( ::aReDo )
                                          IF ::Action( ::aRedo, ::aUndo )
                                             EXIT
                                          ENDIF
                                       END
         ENDMETHOD

   METHOD Action( aActions, aReverse )

   METHOD SelectedText()
   METHOD Position( nLine, nColumn )
   METHOD LineColumn( nPosition )
   METHOD SetSel( nStart, nEnd )

   // Scripts simplification
   METHOD Display( p1, p2 )            INLINE ::oDisplay:Display( p1, p2 )

   ACCESS Row                          INLINE ::oDisplay:nRow
   ASSIGN Row( n )                     INLINE ::oDisplay:nRow := n

   ACCESS Column                       INLINE ::oDisplay:nColumn
   ASSIGN Column( n )                  INLINE ::oDisplay:nColumn := n

   METHOD GoRow( n )                   INLINE ::oDisplay:GoRow( n )
   METHOD GoColumn( n )                INLINE ::oDisplay:GoColumn( n - 1 )

   METHOD IsCommentedFunction( nFunc )
   METHOD Collapse( Line )
   METHOD Expand( Line )

   METHOD CollapseAll()
   METHOD ExpandAll()

   METHOD AddCommentOpener( nLine )
   METHOD DelCommentOpener( nLine )
   METHOD AddCommentCloser( nLine )
   METHOD DelCommentCloser( nLine )
   METHOD ToggleBookmark()

   METHOD SelectLine()                 INLINE ::Action( { { ED_GOTO, ::nLine, ::nColumn }, { ( 2 << 8 ) | ED_SELECT, ::nLine, 1, ::nLine + 1, 0, .F., 1 } }, ::aUnDo )

   METHOD SetPosSilent( nPos )

   METHOD Highlight()                  INLINE ( ::HighlightedLine := ::CurrentLine, ::oDisplay:Display() )
   METHOD SelectWord()
ENDCLASS

METHOD SetPosSilent( nPos ) CLASS Editor

   LOCAL aPos := ::LineColumn( nPos )

   IF aPos[1] == 0
      RETURN Self
   ENDIF

   ::CurrentLine := ::GetLine( aPos[1] )

   ::nLine   := aPos[1]
   ::nColumn := aPos[2]

RETURN Self

METHOD ToggleBookmark() CLASS Editor

   LOCAL nBookmark, aFunc, nOffset
   LOCAL Line := ::CurrentLine

   #ifdef WIN
      LOCAL hDC := GetDC( ::oDisplay:hWnd )
      LOCAL Rect
   #endif

   IF ( nBookmark := aScan( ::aBookmarks, {|_1| _1[2] == Line } ) ) == 0
      nOffset := 0

      FOR EACH aFunc IN ::aFunctions
         IF aFunc[1] >= ::nLine
            EXIT
         ENDIF

         IF Len( aFunc[3] ) > ED_BASE_LEN
            nOffset += aFunc[3][ ED_COLLAPSED_LINES ]
         ENDIF
      NEXT

      aAdd( ::aBookmarks, { ::nLine + nOffset, Line } )

      #ifdef WIN
         //SetTextColor( hDC, s_Colors[ "Red" ] )
         //TextOut( hDC, 0, ::oDisplay:nRow * ::oDisplay:TextMetric:tmHeight + 1, "#", 1 )
         DrawIconEx( hDC, 2, ::oDisplay:nRow * ::oDisplay:TextMetric:tmHeight + 2, s_hBookmark, ( ::oDisplay:TextMetric:tmHeight ) - 3, ( ::oDisplay:TextMetric:tmHeight ) - 2, 0, NIL, DI_NORMAL )
      #endif
   ELSE
      aDel( ::aBookmarks, nBookmark, .T. )

      #ifdef WIN
         Rect IS RECT

         Rect:left   := 0
         Rect:top    := ::oDisplay:nRow * ::oDisplay:TextMetric:tmHeight
         Rect:right  := ::oDisplay:TextMetric:tmHeight - 1
         Rect:bottom := Rect:top + ::oDisplay:TextMetric:tmHeight

         IF Line[ ED_MODIFICATIONS ] == 0
            FillRect( hDC, Rect, ::oDisplay:hPadBrush )
         ELSE
            FillRect( hDC, Rect, ::oDisplay:hHighlightBrush )
         ENDIF

         IF aScan( ::aFunctions, {|_1| _1[3] == Line } )  > 0
            IF Len( Line ) == ED_BASE_LEN
               DrawIconEx( hDC, 2, ::oDisplay:nRow * ::oDisplay:TextMetric:tmHeight + 2, s_hMinus, ( ::oDisplay:TextMetric:tmHeight >> 1 ) + 1, ( ::oDisplay:TextMetric:tmHeight >> 1 ) + 1, NIL, NIL, DI_NORMAL )
            ELSE
               DrawIconEx( hDC, 2, ::oDisplay:nRow * ::oDisplay:TextMetric:tmHeight + 2, s_hPlus, ( ::oDisplay:TextMetric:tmHeight >> 1 ) + 1, ( ::oDisplay:TextMetric:tmHeight >> 1 ) + 1, NIL, NIL, DI_NORMAL )
            ENDIF
         ENDIF
      #endif
   ENDIF

   #ifdef WIN
      ReleaseDC( ::oDisplay:hWnd, hDC )
   #endif

RETURN nBookMark > 0

METHOD New( nTop, nLeft, nRows, nColumns, cFile, oDisplay ) CLASS Editor

   LOCAL aFiles, nFiles, aFile
   LOCAL nAt, cPath

   //TraceLog( nTop, nLeft, nRows, nColumns, cFile, oDisplay )

   #ifdef DEMO
      IF Len( s_aEditors ) >= 5
         Alert( "This DEMO Version is restricted to 5 buffers." )
         BREAK
      ENDIF
   #endif

   aAdd( s_aEditors, Self )
   //TraceLog( s_aEditors )

   ::nTop     := nTop
   ::nLeft    := nLeft
   ::nRows    := nRows
   ::nColumns := nColumns

   //TraceLog( ::CurrentLine[ ED_BUFFER], ::FirstLine[ ED_BUFFER ], ::nLines, ::nLine )

   IF oDisplay == NIL
      #ifdef WIN
         oDisplay := EditorGUIDisplay():New( Self, nTop, nLeft, nRows, nColumns, .T. )
         /*
          Already called by XEDITCONTROLWINDOWPROC()
          ::SetDisplay( oDisplay, .T. )
          */
      #else
         oDisplay := EditorConsoleDisplay():New( Self, nTop, nLeft, nRows, nColumns )
         ::SetDisplay( oDisplay, .T. )
      #endif
   ELSE
      // *** JUST for property access in ::Load() NOT YET set!!!
      ::oDisplay := oDisplay
   ENDIF

   IF ValType( cFile ) == 'C'
      IF cFile HAS "\?|\*"
         aFiles := Directory( cFile )
         nFiles := Len( aFiles )

         IF nFiles == 0
            Throw( ErrorNew( "xEdit", 0, 1001, "Load error: " + cFile, "Missing file", HB_aParams() ) )
         ELSE
            nAt := RAt( DIR_SEPARATOR, cFile )
            cPath := Left( cFile, nAt )

            //::Load( cPath + aFiles[1][1], NIL, .T., .T. )
            //aDel( aFiles, 1, .T. )
            oDisplay:CloseWindow( .F. )

            FOR EACH aFile IN aFiles
               Editor():New( nTop, nLeft, nRows, nColumns, cPath + aFiles[ HB_EnumIndex() ][ 1 ], oDisplay )
            NEXT
         ENDIF
      ELSE
         IF ProcName(1) == "MAIN"
            IF Lower( Right( cFile, 4 ) ) == ".xws"
               oDisplay:OpenWorkSpace( cFile )
            ELSEIF Lower( Right( cFile, 4 ) ) == ".xbp"
               oDisplay:OpenXBP( cFile )
            ELSE
               oDisplay:CloseWindow( .F. )
               Editor():New( nTop, nLeft, nRows, nColumns, cFile, oDisplay )
            ENDIF
         ELSE
            ::Load( cFile, NIL, .T., .T. )
         ENDIF
      ENDIF
   ELSE
      ::Load( , "", .T., .T. )
   ENDIF

RETURN Self

METHOD SetDisplay( oDisplay, lDisplay ) CLASS Editor

   LOCAL nAt, cExt, cFile

   #ifdef WIN
      LOCAL tvins
   #endif

   IF lDisplay == NIL
      lDisplay := .T.
   ENDIF

   ::oDisplay := oDisplay

   IF ::pColorizer == NIL
      nAt := RAt( '.', ::cFile )
      IF nAt > 0
         cExt := SubStr( ::cFile, nAt + 1 )
      ELSE
         cExt := ""
      ENDIF

      //TraceLog( cExt )

      // Default
      ::ExtensionColors := s_DefaultColors
      ::pColorizer      := s_DefaultColors[ "Colorizer" ]
      ::pAfterKey       := s_DefaultColors[ "AfterKey" ]
      ::hBKColor        := s_DefaultColors[ "Background" ]
      ::hHighlightColor := s_DefaultColors[ "Highlight" ]

      TRY
         ::ExtensionColors := s_CustomColors[ cExt ]

         TRY
            ::pColorizer := s_CustomColors[ cExt ][ "Colorizer" ]
         CATCH
         END

         TRY
            ::pAfterKey := s_CustomColors[ cExt ][ "AfterKey" ]
         CATCH
         END

         #ifdef WIN
            TRY
               ::hBKColor := s_CustomColors[ cExt ][ "Background" ]
            CATCH
            END
            TRY
               ::hHighlightColor := s_CustomColors[ cExt ][ "Highlight" ]
            CATCH
            END
         #endif
      CATCH
      END
   ENDIF

   //TraceLog( ::ExtensionColors[ "Name" ] )

   WITH OBJECT oDisplay
      :oEditor := Self

      IF ::FirstLine == NIL
         s_FakeLine    := { "", NIL, NIL, 0 }

         ::FirstLine   := s_FakeLine
         ::LastLine    := s_FakeLine
         ::CurrentLine := s_FakeLine
      ENDIF

      :BaseLine := ::CurrentLine
      :nBaseLine := ::nLine - 1

      #ifdef WIN
          IF :hBrush != NIL
             DeleteObject( :hBrush )
          ENDIF
          :hBrush := CreateSolidBrush( ::hBKColor )
          IF :hHighlightBrush != NIL
             DeleteObject( :hHighlightBrush )
          ENDIF
          :hHighlightBrush := CreateSolidBrush( ::hHighlightColor )

          IF :hContainer != NIL .AND. :hFilesTree != NIL .AND. ( lDisplay .OR. ( ! Empty( ::cFile ) ) )
             IF ::ExtensionColors[ "hExtensionTree" ] == NIL
                tvins := (struct TVINSERTSTRUCT)

                tvins:hParent      := :hTreeRoot
                tvins:hInsertAfter := TVI_SORT

                tvins:item:mask       := TVIF_TEXT
                tvins:item:pszText    := Pad( ::ExtensionColors[ "Name" ] + Chr(0), MAX_PATH )
                tvins:item:cchTextMax := MAX_PATH + 1

                //TraceLog( :hFilesTree, TVM_INSERTITEM, 0, tvins )
                ::ExtensionColors[ "hExtensionTree" ] := SendMessage( :hFilesTree, TVM_INSERTITEM, 0, tvins )
             ENDIF

             IF  :hFilesTree != NIL
                IF ::hFileItem == NIL
                   IF Empty( ::cFile )
                      cFile := "Not named yet"
                   ELSE
                      cFile := Pad( ::cFile + " (" + ::cPath + ")" + Chr(0), MAX_PATH )
                   ENDIF

                   tvins := (struct TVINSERTSTRUCT)
                   tvins:hParent         := ::ExtensionColors[ "hExtensionTree" ]
                   tvins:hInsertAfter    := TVI_SORT
                   tvins:item:mask       := TVIF_TEXT
                   tvins:item:pszText    := cFile
                   tvins:item:cchTextMax := MAX_PATH + 1

                   //TraceLog( ::cFile, ::cPath, tvins:item:pszText )

                   // Add
                   //TraceLog( :hFilesTree, TVM_INSERTITEM, 0, tvins )
                   ::hFileItem := SendMessage( :hFilesTree, TVM_INSERTITEM, 0, tvins )
                ENDIF

                IF ::hFileItem != NIL
                   // Select
                   //TraceLog( :hFilesTree, TVM_SELECTITEM, TVGN_CARET, ::hFileItem, ::cPath, ::cFile )
                   SendMessage( :hFilesTree, TVM_SELECTITEM, TVGN_CARET, ::hFileItem )
                ENDIF
             ENDIF

             //TraceLog( :hContainer, WM_SETTEXT, ::cPath, ::cFile )
             SendMessage( :hContainer, WM_SETTEXT, , "xEdit - " + ::cPath + ::cFile )

             SetFocus( :hWnd )
          ENDIF
      #endif

      :Synch( lDisplay )
   END

RETURN Self

METHOD SetExtension( cExt ) CLASS Editor

   LOCAL oError

   // Default
   ::ExtensionColors := s_DefaultColors
   ::pColorizer      := NIL
   ::pAfterKey       := NIL
   ::hBKColor        := s_DefaultColors[ "Background" ]
   ::hHighlightColor := s_DefaultColors[ "Highlight" ]

   TRY
      ::ExtensionColors := s_CustomColors[ cExt ]

      TRY
         ::pColorizer   := s_CustomColors[ cExt ][ "Colorizer" ]
      CATCH
      END

      TRY
         ::pAfterKey := s_CustomColors[ cExt ][ "AfterKey" ]
      CATCH
      END

      #ifdef WIN
         TRY
            ::hBKColor := s_CustomColors[ cExt ][ "Background" ]
         CATCH
         END
         TRY
            ::hHighlightColor := s_CustomColors[ cExt ][ "Highlight" ]
         CATCH
         END

         WITH OBJECT ::oDisplay
            IF :hBrush != NIL
               DeleteObject( :hBrush )
            ENDIF
            :hBrush := CreateSolidBrush( ::hBKColor )

            IF :hHighlightBrush != NIL
               DeleteObject( :hHighlightBrush )
            ENDIF
            :hHighlightBrush := CreateSolidBrush( ::hHighlightColor )
         END
      #endif
   CATCH oError
      TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine, ValToPrg( oError:Args ) )
   END

RETURN Self

METHOD Close() CLASS Editor

   LOCAL oEditor

   //::Load( , "" )
   s_FakeLine    := { "", NIL, NIL, 0 }

   ::cFile       := ""
   ::cPath       := ""
   ::FirstLine   := s_FakeLine
   ::LastLine    := s_FakeLine
   ::CurrentLine := s_FakeLine
   ::nLine       := 1
   ::nLines      := 1

   FOR EACH oEditor IN s_aEditors
      IF oEditor == Self
         aDel( s_aEditors, HB_EnumIndex(), .T. )
         EXIT
      ENDIF
   NEXT

   #ifndef VXH
      HB_GCAll()
   #endif
   //TraceLog( s_aEditors )

RETURN Self

METHOD DeleteLine() CLASS Editor

   LOCAL aBookmark, aComment, aDump, aFunction, nPos := 0
   LOCAL Delete
   LOCAL nLine

   WITH OBJECT ::oDisplay
      //TraceLog( "DELETE", ::nLine, :nRow, ::CurrentLine[ ED_BUFFER ] )

      IF ::nLines == 1
         ::FirstLine[ ED_BUFFER ] := ""

         ::aBookmarks := {}
         ::aComments  := {}
         ::aDumps     := {}
         ::aFunctions := {}

         :Display()
         RETURN Self
      ENDIF

      ::nLine := Min( ::nLine, ::nLines )
      :nRow := Min( :nRow, ::nLines - :nBaseLine - 1 )
      :nColumn := 0

      //TraceLog( ::nLine, ::nLines )

      IF ::CurrentLine == :BaseLine
         :BaseLine := :BaseLine[ ED_NEXTLINE ]
      ENDIF

      Delete := ::CurrentLine

      IF ::nLine == 1
         ::FirstLine := ::FirstLine[ ED_NEXTLINE ]
         ::FirstLine[ ED_PREVLINE ] := NIL

         ::CurrentLine := ::FirstLine
      ELSEIF ::nLine == ::nLines
         ::LastLine := ::LastLine[ ED_PREVLINE ]
         ::LastLine[ ED_NEXTLINE ] := NIL

         ::CurrentLine := ::LastLine
      ELSE
         // Link Previous line to the Next line.
         ::CurrentLine[ ED_PREVLINE ][ ED_NEXTLINE ] := ::CurrentLine[ ED_NEXTLINE ]

         // Link Next line to the Prior line.
         ::CurrentLine[ ED_NEXTLINE ][ ED_PREVLINE ] := ::CurrentLine[ ED_PREVLINE ]
         ::CurrentLine := ::CurrentLine[ ED_NEXTLINE ]
      ENDIF

      // Explicit release
      aSize( Delete, 0 )

      ::nLines--

      nLine := ::nLine

      FOR nPos := 1 TO  Len( ::aFunctions )
         aFunction := ::aFunctions[ nPos ]

         IF aFunction[1] == nLine
            aDel( ::aFunctions, nPos, .T. )
            nPos--
            LOOP
         ELSEIF aFunction[1] > nLine
            aFunction[1]--
         ELSE
            IF Len( aFunction ) > ED_BASE_LEN
               nLine += aFunction[3][ ED_COLLAPSED_LINES ]
            ENDIF
         ENDIF
      NEXT

      FOR nPos := 1 TO Len( ::aBookmarks )
         aBookmark := ::aBookmarks[ nPos ]

         IF aBookmark[1] == nLine
            aDel( ::aBookmarks, nPos, .T. )
            nPos--
            LOOP
         ELSEIF aBookmark[1] > nLine
            aBookmark[1]--
         ENDIF
      NEXT

      ::DelCommentOpener( nLine - 1 )
      ::DelCommentCloser( nLine - 1 )

      FOR EACH aComment IN ::aComments
         IF aComment[1] >= nLine
            aComment[1]--
            //TraceLog( nLine, HB_EnumIndex(), aComment[1], aComment[2] )
         ENDIF
         IF aComment[2] >= nLine
            aComment[2]--
            //TraceLog( nLine, HB_EnumIndex(), aComment[1], aComment[2] )
         ENDIF
      NEXT

      FOR nPos := 1 TO Len(  ::aDumps )
         aDump := ::aDumps[ nPos ]

         IF aDump[1] - 1 == nLine
            IF aDump[2] == 0
               aDel( ::aDumps, nPos, .T. )
               nPos--
               LOOP
            ELSE
               aDump[1] := 0
            ENDIF
         ELSEIF aDump[1] > nLine
            aDump[1]--
         ENDIF

         IF aDump[2] + 1 == nLine
            IF aDump[1] == 0
               aDel( ::aDumps, nPos, .T. )
               nPos--
               LOOP
            ELSE
               aDump[2] := 0
            ENDIF
         ELSEIF aDump[2] > nLine
            aDump[2]--
         ENDIF
      NEXT

      :Display()
   END

RETURN Self

PROCEDURE InsertLine( Line, lDisplay ) CLASS Editor

   LOCAL aBookmark, aComment, aDump, aFunction
   LOCAL nAt, nOpenAt := 0, nCloseAt := 0
   LOCAL sLine := Line[ ED_BUFFER ], nLine
   LOCAL sTemp

   //TraceLog( sLine )
   //TraceLog( Len( Line ) )

   IF ::nLine == 1
      //TraceLog( ::FirstLine[ ED_BUFFER ], ::CurrentLine[ ED_BUFFER ], ::LastLine[ ED_BUFFER ] , ::oDisplay:BaseLine[ ED_BUFFER ] )

      // INIT of class uses aCopy, so properties are NOT linked to same array refernce yet.
      // NO longer using INIT s_FakeLine!!!
      //::FirstLine := ::CurrentLine
      //::LastLine  := ::CurrentLine

      ::FirstLine[ ED_PREVLINE ] := Line

      Line[ ED_PREVLINE ] := NIL
      Line[ ED_NEXTLINE ] := ::FirstLine

      ::FirstLine := Line
      ::oDisplay:BaseLine := Line
   ELSE
      // Link Previous line to the new line.
      ::CurrentLine[ ED_PREVLINE ][ ED_NEXTLINE ] := Line

      Line[ ED_PREVLINE ] := ::CurrentLine[ ED_PREVLINE ]
      Line[ ED_NEXTLINE ] := ::CurrentLine

      // Link Current previous pointer to the new line.
      ::CurrentLine[ ED_PREVLINE ] := Line
   ENDIF

   ::CurrentLine := Line

   ::nLines++
   ::nLongestLine := Max( ::nLongestLine, Len( sLine ) )

   nLine := ::nLine

   FOR EACH aFunction IN ::aFunctions
      IF aFunction[1] >= nLine
         aFunction[1]++
      ELSE
         IF Len( aFunction[3] ) > ED_BASE_LEN
            nLine += aFunction[3][ ED_COLLAPSED_LINES ]
         ENDIF
      ENDIF
   NEXT

   // NOTE: nLine is already ADJUSTED to include nHiddenLines!!!

   FOR EACH aBookmark IN ::aBookmarks
      IF aBookmark[1] >= nLine
         aBookmark[1]++
      ENDIF
   NEXT

   FOR EACH aComment IN ::aComments
      IF aComment[1] - 1 >= nLine
         aComment[1]++
         //TraceLog( nLine, HB_EnumIndex(), aComment[1], aComment[2] )
      ENDIF
      IF aComment[2] + 1 >= nLine
         aComment[2]++
         //TraceLog( nLine, HB_EnumIndex(), aComment[1], aComment[2] )
      ENDIF
   NEXT

   FOR EACH aDump IN ::aDumps
      IF aDump[1] >= nLine
         aDump[1]++
      ENDIF
      IF aDump[2] >= nLine
         aDump[2]++
      ENDIF
   NEXT

   HB_AtX( s_InlineComment, sLine, , @nAt )
   nOpenAt  := AtSkipStrings( "/*", sLine )
   nCloseAt := At/*SkipStrings*/( "*/", sLine )

   IF nAt > 0
      // Line comment supercedes
      IF nAt < nOpenAt
         nOpenAt := 0
      ENDIF

      // Line comment supercedes
      IF nAt < nCloseAt
         nCloseAt := 0
      ENDIF
   ENDIF

   // Opener processed below at loop, don't change!

   IF nCloseAt > 0
      IF nOpenAt == 0 .OR. nOpenAt > nCloseAt
         ::AddCommentCloser( nLine )
         nOpenAt := 0
      ENDIF
      //TraceLog( "Closer", nLine, nOpenAt, nCloseAt, sLine, ::aComments[-1][1], ::aComments[-1][2] )
   ENDIF

   IF nOpenAt > 0
      // Start with the known opener
      nAt := nOpenAt - 2

      WHILE .T.
         nAt := AtSkipStrings( "/*", sLine, nAt + 2 )

         IF nAt > 0
            nAt := At/*SkipStrings*/( "*/", sLine, nAt + 2 )

            IF nAt == 0
               ::AddCommentOpener( nLine )
               EXIT
            ELSE
               // Continue searching new opener just beyond this closer.
               LOOP
            ENDIF
         ELSE
             nAt := At/*SkipStrings*/( "*/", sLine, nCloseAt + 2 )

             IF nCloseAt > 0
                ::AddCommentCloser( nLine )
             ENDIF

            EXIT
         ENDIF
      END
   ENDIF

   sTemp := Upper( Left( LTrim( sLine ), 17 ) )

   IF sLine HAS s_Func .OR. sLine HAS s_Method .OR. sLine HAS s_Method2 .OR. ( sLine HAS s_Class .AND. ! sLine HAS s_ClassVar )
      aAdd( ::aFunctions, { nLine, /*TODO*/, Line } )
   ELSEIF sTemp = "#PRAGMA BEGINDUMP"
      aAdd( ::aDumps, { nLine, 0 } )
   ELSEIF sTemp = "#PRAGMA ENDDUMP"
      nAt := aScan( ::aDumps, {|_1| _1[2] == 0 /*.AND. _1[1] < nLine*/ } )

      IF nAt == 0
         aAdd( ::aDumps, { 0 , nLine - 1  } )
      ELSE
         ::aDumps[ nAt ][2] := nLine - 1
      ENDIF
   ENDIF

   WITH OBJECT ::oDisplay
      :nBaseColumn := 0
      :nColumn := 0

      IF lDisplay
         :Display()
      ENDIF
   END

RETURN

PROCEDURE AddLine( Line, lDisplay ) CLASS Editor

   LOCAL aFunction
   LOCAL nAt, nOpenAt := 0, nCloseAt := 0
   LOCAL sLine := Line[ ED_BUFFER ], nLine
   LOCAL sTemp

   //TraceLog( ::nLines, sLine )

   IF ::nLines >= 1
      Line[ ED_PREVLINE ] := ::LastLine
      ::LastLine[ ED_NEXTLINE ] := Line
   ELSEIF ::nLines == 0
      ::FirstLine   := Line
      ::CurrentLine := Line
   ENDIF

   ::LastLine := Line
   ::nLongestLine := Max( ::nLongestLine, Len( sLine ) )
   ::nLines++

   nLine := ::nLines

   IF lDisplay != NIL
      FOR EACH aFunction IN ::aFunctions
         IF Len( aFunction ) > ED_BASE_LEN
            nLine += aFunction[3][ ED_COLLAPSED_LINES ]
         ENDIF
      NEXT
   ENDIF

   HB_AtX( s_InlineComment, sLine, , @nAt )
   nOpenAt  := AtSkipStrings( "/*", sLine )
   nCloseAt := At/*SkipStrings*/( "*/", sLine )

   IF nAt > 0
      // Line comment supercedes
      IF nAt < nOpenAt
         nOpenAt := 0
      ENDIF

      // Line comment supercedes
      IF nAt < nCloseAt
         nCloseAt := 0
      ENDIF
   ENDIF

   // Opener processed below at loop, don't change!

   IF nCloseAt > 0
      IF nOpenAt == 0 .OR. nOpenAt > nCloseAt
         ::AddCommentCloser( nLine )
         nOpenAt := 0
      ENDIF
      //TraceLog( "Closer", nLine, nOpenAt, nCloseAt, sLine, ::aComments[-1][1], ::aComments[-1][2] )
   ENDIF

   IF nOpenAt > 0
      // Start with the known opener
      nAt := nOpenAt - 2

      WHILE .T.
         nAt := AtSkipStrings( "/*", sLine, nAt + 2 )

         IF nAt > 0
            nAt := At/*SkipStrings*/( "*/", sLine, nAt + 2 )

            IF nAt == 0
               ::AddCommentOpener( nLine )
               EXIT
            ELSE
               // Continue searching new opener just beyond this closer.
               LOOP
            ENDIF
         ELSE
             nAt := At/*SkipStrings*/( "*/", sLine, nCloseAt + 2 )

             IF nCloseAt > 0
                ::AddCommentCloser( nLine )
             ENDIF

            EXIT
         ENDIF
      END
   ENDIF

   sTemp := Upper( Left( LTrim( sLine ), 17 ) )

   IF sLine HAS s_Func .OR. sLine HAS s_Method .OR. sLine HAS s_Method2 .OR. ( sLine HAS s_Class .AND. ! sLine HAS s_ClassVar )
      aAdd( ::aFunctions, { nLine, /*TODO*/, Line } )
   ELSEIF sTemp = "#PRAGMA BEGINDUMP"
      aAdd( ::aDumps, { nLine, 0 } )
   ELSEIF sTemp = "#PRAGMA ENDDUMP"
      nAt := aScan( ::aDumps, {|_1| _1[2] == 0 /*.AND. _1[1] < nLine*/ } )

      IF nAt == 0
         aAdd( ::aDumps, { 0 , nLine - 1  } )
      ELSE
         ::aDumps[ nAt ][2] := nLine - 1
      ENDIF
   ENDIF

   IF lDisplay != NIL
      WITH OBJECT ::oDisplay
         //IF HB_QWith() != NIL
            :nBaseColumn := 0
            :nColumn := 0

            :Display()
         //ENDIF
      END
   ENDIF

RETURN //Self

PROCEDURE LoadAddLine( sLine ) CLASS Editor

   LOCAL nAt := 0, nOpenAt := 0, nCloseAt := 0
   LOCAL nLine
   LOCAL Line
   LOCAL sTemp

   sLine := StrTran( sLine, Chr(9), Space( s_nTabSpaces ) )
   //sLine := StrTran( sLine, Chr(13), "" )
   //sLine := StrTran( sLine, Chr(26), "" )

   Line := { sLine, NIL, NIL, 0 }

   //TraceLog( ::nLines, sLine )

   IF ::nLines > 0
      Line[ ED_PREVLINE ] := ::LastLine
      ::LastLine[ ED_NEXTLINE ] := Line
   ELSE
      ::FirstLine   := Line
      ::CurrentLine := Line
   ENDIF

   ::LastLine := Line
   ::nLongestLine := Max( ::nLongestLine, Len( sLine ) )
   ::nLines++

   nLine := ::nLines

   HB_AtX( s_InlineComment, sLine, , @nAt )
   nOpenAt  := AtSkipStrings( "/*", sLine )
   nCloseAt := At/*SkipStrings*/( "*/", sLine )

   IF nAt > 0
      // Line comment supercedes
      IF nAt < nOpenAt
         nOpenAt := 0
      ENDIF

      // Line comment supercedes
      IF nAt < nCloseAt
         nCloseAt := 0
      ENDIF
   ENDIF

   // Opener processed below at loop, don't change!

   IF nCloseAt > 0
      IF nOpenAt == 0 .OR. nOpenAt > nCloseAt
         ::AddCommentCloser( nLine )
         nOpenAt := 0
      ENDIF
      //TraceLog( "Closer", nLine, nOpenAt, nCloseAt, sLine, ::aComments[-1][1], ::aComments[-1][2] )
   ENDIF

   IF nOpenAt > 0
      // Start with the known opener
      nAt := nOpenAt - 2

      WHILE .T.
         nAt := AtSkipStrings( "/*", sLine, nAt + 2 )

         IF nAt > 0
            nAt := At/*SkipStrings*/( "*/", sLine, nAt + 2 )

            IF nAt == 0
               ::AddCommentOpener( nLine )
               EXIT
            ELSE
               // Continue searching new opener just beyond this closer.
               LOOP
            ENDIF
         ELSE
             nAt := At/*SkipStrings*/( "*/", sLine, nCloseAt + 2 )

             IF nCloseAt > 0
                ::AddCommentCloser( nLine )
             ENDIF

            EXIT
         ENDIF
      END
   ENDIF

   sTemp := Upper( Left( LTrim( sLine ), 17 ) )

   IF sLine HAS s_Func .OR. sLine HAS s_Method .OR. sLine HAS s_Method2 .OR. ( sLine HAS s_Class .AND. ! sLine HAS s_ClassVar )
      aAdd( ::aFunctions, { nLine, /*TODO*/, Line } )
   ELSEIF sTemp = "#PRAGMA BEGINDUMP"
      aAdd( ::aDumps, { nLine, 0 } )
   ELSEIF sTemp = "#PRAGMA ENDDUMP"
      nAt := aScan( ::aDumps, {|_1| _1[2] == 0 /*.AND. _1[1] < nLine*/ } )

      IF nAt == 0
         aAdd( ::aDumps, { 0 , nLine - 1  } )
      ELSE
         ::aDumps[ nAt ][2] := nLine - 1
      ENDIF
   ENDIF

   IF nLine % 1000 == 0
      ::oDisplay:Status( NIL, "Loading: " + Str( nLine ) )
   ENDIF

RETURN //Self

METHOD Up( nCount ) CLASS Editor

   NAVIGATION_BEGIN

   #ifdef VXH
      nCount := 1
   #endif

   IF ::nLine == 1
      ::Home()
      BREAK
   ENDIF

   IF nCount > ::nLine
      nCount := ::nLine - 1
   ENDIF

   WITH OBJECT ::oDisplay
      //TraceLog( ::nLine, :nBaseLine, :nRow, nCount )

      IF :nRow == -1 .OR. :nColumn == -1
         ShowCaret( :hWnd )

         :nColumn := Max( 0, :nColumn )

         IF :nRow == -1
            ::GoLine( ::nLine )
         ENDIF
      ENDIF

      IF s_lForceEOL .AND. ::nPreferedColumn > Len( ::CurrentLine[ ED_BUFFER ] )
         :nColumn := ::nPreferedColumn - :nBaseColumn - 1
      ENDIF

      IF :nRow >= nCount
         :GoRow( :nRow - nCount )
      ELSE
         nCount -= :nRow
         :nRow := 0
         ::GoBaseLine( :nBaseLine - nCount )
      ENDIF
   END

   NAVIGATION_END

RETURN Self

METHOD Down( nCount ) CLASS Editor

   NAVIGATION_BEGIN

   #ifdef VXH
      nCount := 1
   #endif

   IF ::nLine >= ::nLines
      ::End()
      BREAK
   ENDIF

   WITH OBJECT ::oDisplay
      //TraceLog( ::nLine, :nRow, :nColumn )

      IF :nRow == -1 .OR. :nColumn == -1
         ShowCaret( :hWnd )

         :nColumn := Max( 0, :nColumn )

         IF :nRow == -1
            ::GoLine( ::nLine )
         ENDIF
      ENDIF

      nCount := Min( nCount, ::nLines - ::nLine )

      IF ::nPreferedColumn > Len( ::CurrentLine[ ED_BUFFER ] )
         :nColumn := ::nPreferedColumn - :nBaseColumn - 1
      ENDIF

      IF :nRow + nCount < :nRows
         :GoRow( :nRow + nCount )
      ELSE
         nCount -= :nRows - ( :nRow + 1 )
         :nRow := :nRows - 1
         ::GoBaseLine( :nBaseLine + nCount )
      ENDIF
   END

   NAVIGATION_END

RETURN Self

METHOD Left( nCount ) CLASS Editor

   NAVIGATION_BEGIN

   WITH OBJECT ::oDisplay

      IF :nRow == -1 .OR. :nColumn == -1
         ShowCaret( :hWnd )

         :nColumn := Max( 0, :nColumn )

         IF :nRow == -1
           ::GoLine( ::nLine )
         ENDIF
      ENDIF

      IF ::nColumn == 1
         ::Up(1)
         ::LineEnd()
         BREAK
      ENDIF

      IF ::lCtrl
         ::nColumn--
         WHILE ::nColumn > 0 .AND. ::CurrentLine[ ED_BUFFER ][::nColumn ] == ' '
            ::nColumn--
         END

         IF IsDigit( ::CurrentLine[ ED_BUFFER ][::nColumn ] )
            WHILE ::nColumn > 0 .AND. IsAlNum( ::CurrentLine[ ED_BUFFER ][ ::nColumn ] )
               ::nColumn--
            END
         ELSEIF IsAlNum( ::CurrentLine[ ED_BUFFER ][::nColumn ] )
            WHILE ::nColumn > 0 .AND. IsAlNum( ::CurrentLine[ ED_BUFFER ][ ::nColumn ] )
               ::nColumn--
            END
         ELSE
            WHILE ::nColumn > 0 .AND. ::CurrentLine[ ED_BUFFER ][ ::nColumn ] != ' ' .AND. ( ! IsAlNum( ::CurrentLine[ ED_BUFFER ][ ::nColumn ] ) )
               ::nColumn--
            END
         ENDIF
         ::nColumn++

         IF ::nColumn > :nBaseColumn
            :GoColumn( ::nColumn - :nBaseColumn - 1 )
         ELSE
            :nBaseColumn := ::nColumn - 1
            :nColumn := 0
            :Display()
         ENDIF

         BREAK
      ENDIF

      IF :nColumn >= nCount
         :GoColumn( :nColumn - nCount )
         BREAK
      ENDIF

      nCount -= :nColumn
      :nColumn := 0
      :nBaseColumn := Max( 0, :nBaseColumn - nCount )

      :Display()
   END

   NAVIGATION_END

   ::nPreferedColumn := ::nColumn;

RETURN Self

METHOD Right( nCount ) CLASS Editor

   LOCAL nLen

   NAVIGATION_BEGIN

   WITH OBJECT ::oDisplay
      IF :nRow == -1 .OR. :nColumn == -1
         ShowCaret( :hWnd )

         :nColumn := Max( 0, :nColumn )

         IF :nRow == -1
            ::GoLine( ::nLine )
         ENDIF
      ENDIF

      nLen := Len( ::CurrentLine[ ED_BUFFER ] )

      IF ::lSquare .AND. ::lShift
         IF :nColumn < :nColumns - 1
            :GoColumn( :nColumn + 1 )
         ELSE
            :nBaseColumn++
            :Display()
         ENDIF

         BREAK
      ENDIF

      IF ( s_lForceEOL  .OR. ::nLineFrom > 0 ).AND. ::nColumn > nLen
         ::Down(1)
         ::LineHome()
         BREAK
      ENDIF

      IF ::lCtrl
         IF IsDigit( ::CurrentLine[ ED_BUFFER ][::nColumn ] )
            WHILE ::nColumn <= nLen .AND. IsAlNum( ::CurrentLine[ ED_BUFFER ][ ::nColumn ] )
               ::nColumn++
            END
         ELSEIF IsAlNum( ::CurrentLine[ ED_BUFFER ][::nColumn ] )
            WHILE ::nColumn <= nLen .AND. IsAlNum( ::CurrentLine[ ED_BUFFER ][ ::nColumn ] )
               ::nColumn++
            END
         ELSE
            WHILE ::nColumn <= nLen .AND. ::CurrentLine[ ED_BUFFER ][ ::nColumn ] != ' ' .AND. ( ! IsAlNum( ::CurrentLine[ ED_BUFFER ][ ::nColumn ] ) )
               ::nColumn++
            END
         ENDIF

         WHILE ::nColumn <= nLen .AND. ::CurrentLine[ ED_BUFFER ][::nColumn ] == ' '
            ::nColumn++
         END

         IF ::nColumn <= :nBaseColumn + :nColumns
            :GoColumn( ::nColumn - :nBaseColumn - 1 )
         ELSE
            :nBaseColumn := ::nColumn - 1
            :nColumn := 0
            :Display()
         ENDIF

         BREAK
      ENDIF

      IF :nColumn + nCount <= :nColumns
         :GoColumn( :nColumn + nCount )
         BREAK
      ENDIF

      nCount -= :nColumns - :nColumn
      :nColumn := :nColumns
      :nBaseColumn := Max( 0, :nBaseColumn + nCount )

      :Display()
   END

   NAVIGATION_END

   ::nPreferedColumn := ::nColumn;

RETURN Self

METHOD PageUp() CLASS Editor

   NAVIGATION_BEGIN

   WITH OBJECT ::oDisplay
      IF :nRow == -1 .OR. :nColumn == -1
         ShowCaret( :hWnd )

         IF :nRow == -1
            ::GoLine( ::nLine )
         ENDIF
      ENDIF

      IF :nBaseLine >= :nRows
         ::GoBaseLine( :nBaseLine - :nRows )
      ELSE
         ::Home()
      ENDIF
   END

   NAVIGATION_END

RETURN Self

METHOD PageDown() CLASS Editor

   NAVIGATION_BEGIN
   WITH OBJECT ::oDisplay
      IF :nRow == -1 .OR. :nColumn == -1
         ShowCaret( :hWnd )

         IF :nRow == -1
            ::GoLine( ::nLine )
         ENDIF
      ENDIF

      IF :nBaseLine + :nRows + :nRows < ::nLines
         ::GoBaseLine( :nBaseLine + :nRows )
      ELSE
        ::End()
      ENDIF
   END

   NAVIGATION_END

RETURN Self

METHOD Home() CLASS Editor

   LOCAL nBaseLine
   LOCAL nBaseColumn

   NAVIGATION_BEGIN

   WITH OBJECT ::oDisplay
      IF :nRow == -1 .OR. :nColumn == -1
         ShowCaret( :hWnd )

         :nColumn := Max( 0, :nColumn )

         IF :nRow == -1
           ::GoLine( ::nLine )
         ENDIF
      ENDIF

      nBaseLine    := :nBaseLine
      nBaseColumn  := :nBaseColumn

      :nBaseLine   := 0
      :nBaseColumn := 0
      :nRow        := 0
      :nColumn     := 0

      :BaseLine := ::FirstLine

      IF nBaseLine <> 0 .OR. nBaseColumn <> 0
         :Display()
      ELSE
         :GoRow( 0 )
      ENDIF

      :GoColumn( 0 )
   END

   NAVIGATION_END

   ::nPreferedColumn := ::nColumn;

   //TraceLog( ::nLine, ::nColumn, ::nLineFrom, ::nColumnFrom, ::nLineTo, ::nColumnTo )

RETURN Self

METHOD LineHome() CLASS Editor

   LOCAL c, nSpaces

   NAVIGATION_BEGIN

   WITH OBJECT ::oDisplay
      IF :nRow == -1 .OR. :nColumn == -1
         ShowCaret( :hWnd )

         :nColumn := Max( 0, :nColumn )

         IF :nRow == -1
           ::GoLine( ::nLine )
         ENDIF
      ENDIF

      nSpaces := 0

      FOR EACH c IN ::CurrentLine[ ED_BUFFER ]
          IF c != ' '
             EXIT
          ENDIF

          nSpaces++
      NEXT

      IF ::nColumn > nSpaces + 1 .OR. ::nColumn == 1
         ::nColumn := nSpaces + 1
      ELSE
         ::nColumn := 1
      ENDIF

      IF ::nColumn <= :nBaseColumn .OR. ::nColumn > :nBaseColumn + :nColumns
         IF ::nColumn > :nColumns
            :nBaseColumn := ::nColumn - 1
         ELSE
            :nBaseColumn := 0
         ENDIF

         :nColumn := 0
         :Display()
         BREAK
      ENDIF

     :GoColumn( ::nColumn - :nBaseColumn - 1 )
   END

   NAVIGATION_END

   ::nPreferedColumn := ::nColumn;

RETURN Self

METHOD End() CLASS Editor

   LOCAL nBaseLine
   LOCAL nBaseColumn
   LOCAL nNewBase

   NAVIGATION_BEGIN

   WITH OBJECT ::oDisplay
      IF :nRow == -1 .OR. :nColumn == -1
         ShowCaret( :hWnd )

         :nColumn := Max( 0, :nColumn )

         IF :nRow == -1
           ::GoLine( ::nLine )
         ENDIF
      ENDIF

      nBaseLine    := :nBaseLine
      nBaseColumn  := :nBaseLine

      nNewBase     := Max( 0, ::nLines - :nRows )
      :nBaseColumn := 0
      :nRow        := ::nLines - nNewBase - 1
      :nColumn     := 0

      IF nNewBase != nBaseLine .OR. :nBaseColumn != nBaseColumn
         ::GoBaseLine( nNewBase )
         //TraceLog( nNewBase, ::nLines, :nRow, :nRows, ::nLine, :nBaseLine, :BaseLine == ::CurrentLine )
      ELSE
         :GoRow( :nRow )
      ENDIF
   END

   ::LineEnd()

   NAVIGATION_END

RETURN Self

METHOD LineEnd() CLASS Editor

   LOCAL nLen

   NAVIGATION_BEGIN

   WITH OBJECT ::oDisplay
      IF :nRow == -1 .OR. :nColumn == -1
         ShowCaret( :hWnd )

         :nColumn := Max( 0, :nColumn )

         IF :nRow == -1
           ::GoLine( ::nLine )
         ENDIF
      ENDIF

      nLen := Len( ::CurrentLine[ ED_BUFFER ] )

      IF nLen >= :nBaseColumn + :nColumns
         :nBaseColumn := nLen - :nColumns + 1
         :nColumn := :nColumns - 1
         :Display()
      ELSE
         :GoColumn( nLen - :nBaseColumn )
      ENDIF
   END

   NAVIGATION_END

   ::nPreferedColumn := ::nColumn;

RETURN Self

METHOD Find( sText, nFrom, RegEx ) CLASS Editor

   LOCAl Line, nLine, nColumn, nFoundAt, nLast := 0, sFound, sLast, cChar, sEscaped, nLineTo, nColumnTo, nLen, nLastLen

   IF RegEx == NIL
      IF nFrom & FR_MATCHCASE != 0
         RegEx := ""
      ELSE
         RegEx := "(?i)"
      ENDIF

      sEscaped := ""
      FOR EACH cChar IN sText
         IF cChar IN "\.*+()[]^{}$|?"
            sEscaped += '\'
         ENDIF

         sEscaped += cChar
      NEXT
      sText := sEscaped

      IF nFrom & FR_WHOLEWORD != 0
         RegEx += "\b" + sText + "\b"
      ELSE
         RegEx += sText
      ENDIF

      //TraceLog( RegEx )
      RegEx := HB_RegExComp( RegEx )
   ENDIF

   WITH OBJECT ::oDisplay
      IF :nRow == -1 .OR. :nColumn == -1
         ShowCaret( :hWnd )

         :nColumn := Max( 0, :nColumn )

         IF :nRow == -1
           ::GoLine( ::nLine )
         ENDIF
      ENDIF

      IF nFrom & FR_FROMTOP != 0
         // Set
         Line        := ::FirstLine
         nLine       := 1
         nColumn     := 1

         nFrom := nFrom | FR_DOWN
      ELSE
         Line        := ::CurrentLine
         nLine       := ::nLine
         nColumn     := ::nColumn
      ENDIF

      // DOWN Search
      IF nFrom & FR_DOWN != 0

         IF nFrom & FR_SELECTED == 0
            nLineTo   := ::nLines
            nColumnTo := ::nLongestLine
         ELSE
            //nLine := ::nLineFrom
            //nColumn := ::nColumnFrom

            IF ::nDirection == 1
               //TraceLog( "F", nLine, nColumn, ::nLineFrom, ::nColumnFrom, ::nLineTo, ::nColumnTo )

               IF nLine == ::nLineTo .AND. nColumn == ::nColumnTo + 1
                  nLine := ::nLineFrom
                  nColumn := ::nColumnFrom
               ENDIF
            ELSE
               //TraceLog( "B", nLine, nColumn, ::nLineFrom, ::nColumnFrom, ::nLineTo, ::nColumnTo )
            ENDIF

            nLineTo   := ::nLineTo
            nColumnTo := ::nColumnTo

            ::GoLine( nLine )
            Line := ::CurrentLine
         ENDIF

         //TraceLog( "Down", nLine, nColumn, nLineTo, nColumnTo, Line[ ED_BUFFER ] )

         IF nColumn <= Len( Line[ ED_BUFFER ] )
            nFoundAt := nColumn
            nLen     := 0
            sFound   := HB_AtX( RegEx, Line[ ED_BUFFER ], , @nFoundAt, @nLen )
            //TraceLog( sFound, nFoundAt, nLine, nLineTo )

            IF nFoundAt > 0 .AND. ( nLine < nLineTo .OR. nFoundAt + nLen - 1 <= nColumnTo )
               RETURN { nLine, nFoundAt, nFoundAt + nLen - 1 }
            ENDIF
         ENDIF

         nFoundAt := 0
         WHILE nLine < nLineTo//::nLines
            Line := Line[ ED_NEXTLINE ]
            nLine++

            nLen := 0
            sFound := HB_AtX( RegEx, Line[ ED_BUFFER ], , @nFoundAt, @nLen )
            //TraceLog( sFound, nFoundAt, nLine, nLineTo )

            IF nFoundAt > 0 .AND. ( nLine < nLineTo .OR. nFoundAt + nLen - 1 <= nColumnTo )
               RETURN { nLine, nFoundAt, nFoundAt + nLen - 1 }
            ENDIF
         END

      ELSE // UP Search

         IF nFrom & FR_SELECTED == 0
            nLineTo   := 1
            nColumnTo := 1
            nColumn--
         ELSE
            //nLine   := ::nLineTo
            //nColumn := ::nColumnTo

            IF ::nDirection == -1
               //TraceLog( "B", nLine, nColumn, ::nLineFrom, ::nColumnFrom, ::nLineTo, ::nColumnTo )

               IF nLine == ::nLineFrom .AND. nColumn == ::nColumnFrom
                  nLine := ::nLineTo
                  nColumn := ::nColumnTo
               ENDIF
            ELSE
               //TraceLog( "F", nLine, nColumn, ::nLineFrom, ::nColumnFrom, ::nLineTo, ::nColumnTo )
            ENDIF

            nLineTo   := ::nLineFrom
            nColumnTo := ::nColumnFrom

            ::GoLine( nLine )
            Line := ::CurrentLine
         ENDIF

         //TraceLog( "UP", nLine, nColumn, nLineTo, nColumnTo, Line[ ED_BUFFER ] )

         nFoundAt := 0
         nLen     := 0
         sFound   := HB_AtX( RegEx, Line[ ED_BUFFER ], , @nFoundAt, @nLen )

         WHILE nFoundAt > 0 .AND. nFoundAt + nLen - 1 < nColumn
            nLast    := nFoundAt
            nLastLen := nLen
            sLast    := sFound
            nFoundAt += nLen

            nLen   := 0
            sFound := HB_AtX( RegEx, Line[ ED_BUFFER ], , @nFoundAt, @nLen )
            //TraceLog( sFound, nFoundAt, nLine, nLineTo )
         END

         //TraceLog( nLast, nLastLen, nColumn )
         IF nLast > 0 .AND. ( nLine > nLineTo .OR. nLast >= nColumnTo ) .AND. nLast + nLastLen - 1 <= nColumn
            //TraceLog( nLine, nLast, nLast + nLastLen - 1 )
            RETURN { nLine, nLast, nLast + nLastLen - 1 }
         ENDIF

         nFoundAt := 0
         WHILE nLine > nLineTo
            Line := Line[ ED_PREVLINE ]
            nLine--

            sFound := HB_AtX( RegEx, Line[ ED_BUFFER ], , @nFoundAt,  )

            WHILE nFoundAt > 0
               nLast := nFoundAt
               sLast := sFound
               nFoundAt += nLen

               nLen   := 0
               sFound := HB_AtX( RegEx, Line[ ED_BUFFER ], , @nFoundAt, @nLen)
               //TraceLog( sFound, nFoundAt, Line[ ED_BUFFER ] )
            END

            IF nLast > 0 .AND. ( nLine > nLineTo .OR. nLast >= nColumnTo )
               RETURN { nLine, nLast, nLast + Len( sLast ) - 1 }
            ENDIF
         END
      ENDIF
   END

RETURN { 0, 0, 0 }

METHOD GoLine( nLine ) CLASS Editor

    LOCAL nNewBase, nFromCurrent, nFromBottom

    IF nLine > ::nLines
       nLine := ::nLines
    ELSEIF nLine < 1
       nLine := 1
    ENDIF

    WITH OBJECT ::oDisplay
       IF nLine > :nBaseLine .AND. nLine <= :nBaseLine + :nRows
          :GoRow( nLine - :nBaseLine - 1 )
          RETURN Self
       ENDIF

       nNewBase := nLine - Int( :nRows >> 1 ) - 1
       IF nNewBase + :nRows > ::nLines
          nNewBase := ::nLines - :nRows
       ENDIF

       nFromCurrent := Abs( nNewBase - :nBaseLine )
       nFromBottom := ::nLines - nNewBase

       //TraceLog( ::oDisplay:nBaseLine, ::nLine, nLine, nNewBase, nFromCurrent, nFromBottom )

       IF nFromCurrent < nFromBottom .AND. nFromCurrent < nNewBase
          IF :nBaseLine > nNewBase
             WHILE :nBaseLine > nNewBase
                :BaseLine := :BaseLine[ ED_PREVLINE ]
                :nBaseLine--
             ENDDO
          ELSE
             WHILE :nBaseLine < nNewBase
                :BaseLine := :BaseLine[ ED_NEXTLINE ]
                :nBaseLine++
             ENDDO
          ENDIF
       ELSEIF nFromBottom < nFromCurrent .AND. nFromBottom < nNewBase
          :BaseLine := ::LastLine
          :nBaseLine := ::nLines - 1

          WHILE :nBaseLine >= nNewBase
             :BaseLine := :BaseLine[ ED_PREVLINE ]
             :nBaseLine--
          ENDDO
       ELSE
          :BaseLine := ::FirstLine
          :nBaseLine := 0

          WHILE :nBaseLine < nNewBase
             :BaseLine := :BaseLine[ ED_NEXTLINE ]
             :nBaseLine++
          ENDDO
       ENDIF

       //TraceLog( nLine, nNewBase, :nBaseLine, :BaseLine[ ED_BUFFER ] )

       :nRow := nLine - :nBaseLine - 1
       :Display()
    END

RETURN Self

METHOD GoBaseLine( nLine ) CLASS Editor

    LOCAL nNewBase, nFromCurrent, nFromBottom

    //TraceLog( ::nLine, ::oDisplay:nBaseLine, nLine )

    IF nLine > ::nLines
       nLine := ::nLines - 1
    ELSEIF nLine < 0
       nLine := 0
    ENDIF

    nNewBase := nLine

    WITH OBJECT ::oDisplay
       //nNewBase := Min( nNewBase, ::nLines - :nRows + 1 )

       IF nNewBase == :nBaseLine
          RETURN Self
       ENDIF

       nFromCurrent := Abs( nNewBase - :nBaseLine )
       nFromBottom := ::nLines - nNewBase

       IF nFromCurrent < nFromBottom .AND. nFromCurrent < nNewBase
          IF :nBaseLine > nNewBase
             WHILE :nBaseLine > nNewBase
                :BaseLine := :BaseLine[ ED_PREVLINE ]
                :nBaseLine--
             ENDDO
          ELSE
             WHILE :nBaseLine < nNewBase
                :BaseLine := :BaseLine[ ED_NEXTLINE ]
                :nBaseLine++
             ENDDO
          ENDIF
       ELSEIF nFromBottom < nFromCurrent .AND. nFromBottom < nNewBase
          :BaseLine := ::LastLine
          :nBaseLine := ::nLines - 1

          WHILE :nBaseLine > nNewBase
             :BaseLine := :BaseLine[ ED_PREVLINE ]
             :nBaseLine--
          ENDDO
       ELSE
          :BaseLine := ::FirstLine
          :nBaseLine := 0

          WHILE :nBaseLine < nNewBase
             :BaseLine := :BaseLine[ ED_NEXTLINE ]
             :nBaseLine++
          ENDDO
       ENDIF

       //TraceLog( ::nLines, ::nLine, :nRows, nLine, :nRow, nNewBase, :nBaseLine, :BaseLine[ ED_BUFFER ] )

       :Display()
    END

RETURN Self

METHOD GetLine( nLine ) CLASS Editor

   LOCAL nFromCurrent, nFromBottom, nBaseLine, BaseLine

   //TraceLog( ::nLine, ::oDisplay:nBaseLine, nLine, ::oDisplay:BaseLine )
   nLine--

   IF nLine > ::nLines
      nLine := ::nLines - 1
   ELSEIF nLine < 0
      nLine := 0
   ENDIF

   IF nLine == ::nLine - 1
      RETURN ::CurrentLine
   ENDIF

   nBaseLine := ::nLine - 1
   BaseLine := ::CurrentLine

   nFromCurrent := Abs( nLine - nBaseLine )
   nFromBottom := ::nLines - nLine

   IF nFromCurrent < nFromBottom .AND. nFromCurrent < nLine
      IF nBaseLine > nLine
         WHILE nBaseLine > nLine
            BaseLine := BaseLine[ ED_PREVLINE ]
            nBaseLine--
         ENDDO
      ELSE
         WHILE nBaseLine < nLine
            BaseLine := BaseLine[ ED_NEXTLINE ]
            nBaseLine++
         ENDDO
      ENDIF
   ELSEIF nFromBottom < nFromCurrent .AND. nFromBottom < nLine
      BaseLine := ::LastLine
      nBaseLine := ::nLines - 1

      WHILE nBaseLine > nLine
         BaseLine := BaseLine[ ED_PREVLINE ]
         nBaseLine--
      ENDDO
   ELSE
      BaseLine := ::FirstLine
      nBaseLine := 0

      WHILE nBaseLine < nLine
         BaseLine := BaseLine[ ED_NEXTLINE ]
         nBaseLine++
      ENDDO
   ENDIF

   //TraceLog( nLine, nBaseLine, BaseLine[ ED_BUFFER ] )

RETURN BaseLine

METHOD Load( cFile, sText, lResetDisplay, lDisplay ) CLASS Editor

   LOCAL hFile
   LOCAL sLine
   LOCAL nAt
   LOCAL cExt
   LOCAL aEol := { Chr(13) + Chr(10), Chr(10) }

   #ifdef WIN
    LOCAL hCursor := SetCursor( LoadCursor( NIL, IDC_WAIT ) )
   #endif
   #ifndef VXH
    #ifdef WIN
     LOCAL oError
     LOCAL hRecent, sRecentFile
    #endif
   #endif

   //TraceLog( cFile, sText, lResetDisplay )

   ::nDirection  := 0
   ::nLineFrom   := 0
   ::nColumnFrom := 0
   ::nLineTo     := 0
   ::nColumnTo   := 0

   ::aFunctions := {}; ::aComments := {}; ::aDumps := {}; ::aBookmarks := {}; ::aUnDo := {}; ::aReDo := {}; ::lModified := .F.

   ::nLine   := 0
   ::nColumn := 0
   ::nLines  := 0

   s_FakeLine    := { "", NIL, NIL, 0 }

   ::FirstLine   := s_FakeLine
   ::LastLine    := s_FakeLine
   ::CurrentLine := s_FakeLine

   #ifndef VXH
      HB_GCAll()
   #endif
   WITH OBJECT ::oDisplay
      IF cFile == NIL
         ::cFile := ""
         ::cPath := ""

         sText := StrTran( sText, Chr(13), "" )

         WHILE NextLine( @sText, @sLine )
            ::LoadAddLine( sLine )
         END
      ELSE
         hFile := FOpen( cFile, FO_READ )

         IF hFile == -1
            TraceLog( "Open error: " + cFile, "I/O Error (" + Str( FError(), 2 ) + ")", "Path: " + CurDir(), HB_aParams() )
            Throw( ErrorNew( "xEdit", 0, 1001, "Open error: " + cFile, "I/O Error (" + Str( FError(), 2 ) + ")", HB_aParams() ) )
         ENDIF

         nAt := RAt( DIR_SEPARATOR, cFile )
         IF nAt > 0
            ::cPath := Left( cFile, nAt )

            IF ::cPath[2] == ':'
               IF ::cPath[3] != DIR_SEPARATOR
                  ::cPath := Left( ::cPath, 2 ) + DIR_SEPARATOR + CurDir( Left( ::cPath, 2 ) ) + DIR_SEPARATOR + SubStr( ::cPath, 3 )
               ENDIF
            ELSE
               IF ::cPath[1] == DIR_SEPARATOR
                  IF ::cPath[2] == DIR_SEPARATOR // UNC path - no disk drive
                  ELSE
                     ::cPath := DiskName() + ':' + ::cPath
                  ENDIF
               ELSE
                  ::cPath := DiskName() + ':' + DIR_SEPARATOR + CurDir() + DIR_SEPARATOR + ::cPath
               ENDIF
            ENDIF

            ::cFile := SubStr( cFile, nAt + 1 )
         ELSE
            ::cPath := DiskName() + ':' + DIR_SEPARATOR + CurDir() + DIR_SEPARATOR
            ::cFile := cFile
         ENDIF

         nAt := RAt( '.', ::cFile )
         IF nAt > 0
            cExt := SubStr( ::cFile, nAt + 1 )
         ELSE
            cExt := ""
         ENDIF

         //TraceLog( "Start", Seconds() )

         //TraceLog( ::cFile, ::cPath )

         //TraceLog( s_PrgColors[ "Background" ], s_PrgColors[ "Text" ] )

      #if 0
         WHILE HB_FReadLine( hFile, @sLine, aEol ) == 0
            ::LoadAddLine( sLine )
         END

         IF Len( sLine ) > 0
            ::LoadAddLine( sLine )
         ENDIF

         FClose( hFile )
      #else
         FClose( hFile )
         ProcessTextFile( cFile, Self, HB_ObjMsgPtr( Self, "LoadAddLine" ) )
      #endif

         //TraceLog( "Done", Seconds() )

         ::Date := Directory( cFile )[1][ F_DATE ]
         ::Time := Directory( cFile )[1][ F_TIME ]

         //TraceLog( ::oDisplay, HB_QWith(), ProcName(2) )

         #ifndef VXH
            #ifdef WIN
               IF ( ! HB_QWith() == NIL ) .AND. :hContainer != 0 .AND. ( ! ProcName(2) == "EDITORGUIDISPLAY:OPENWORKSPACE" )
                  #ifdef WIN
                     nAt := aScan( s_aFiles, Lower( ::cPath + ::cFile ), , , .T. )
                  #else
                     nAt := aScan( s_aFiles, ::cPath + ::cFile, , , .T. )
                  #endif

                  //TraceLog( nAt )

                  TRY
                     IF nAt == 1
                        BREAK
                     ELSEIF nAt > 1
                        aDel( s_aFiles, nAt, .T. )
                        #ifdef WIN
                           aIns( s_aFiles, 1, Lower( ::cPath + ::cFile ), .T. )
                        #else
                           aIns( s_aFiles, 1, ::cPath + ::cFile, .T. )
                        #endif
                     ELSE
                        #ifdef WIN
                           aIns( s_aFiles, 1, Lower( ::cPath + ::cFile ), .T. )
                        #else
                           aIns( s_aFiles, 1, ::cPath + ::cFile, .T. )
                        #endif

                        aSize( s_aFiles, Min( 30, Len( s_aFiles ) ) )
                     ENDIF

                     hRecent := CreateMenu()

                     FOR EACH sRecentFile IN s_aFiles
                         AppendMenu( hRecent, MF_STRING, 5000 + HB_EnumIndex(), sRecentFile )
                     NEXT

                     DeleteMenu( GetSubMenu( s_hMenu, 0 ), 14, MF_BYPOSITION )
                     InsertMenu( GetSubMenu( s_hMenu, 0 ), 14, MF_POPUP | MF_BYPOSITION, hRecent, "Recent Files" )

                  CATCH oError
                     IF ! Empty( oError )
                        TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
                     ENDIF
                  END
               ENDIF
            #endif
         #endif
      ENDIF

      IF ::nLines == 0
         s_FakeLine  := { "", NIL, NIL, 0 }

         ::nLines    := 1
         ::FirstLine := s_FakeLine
         ::LastLine  := s_FakeLine
      ELSE
         IF ::LastLine[ ED_BUFFER ][-1] == Chr( 26 )
            ::LastLine[ ED_BUFFER ][-1] := 32
         ENDIF
      ENDIF

      ::nLine       := 1
      ::nColumn     := 1
      ::CurrentLine := ::FirstLine

      //TraceLog( ::nLines, ::LastLine[ ED_BUFFER ], ::nLine, ::CurrentLine[ ED_BUFFER], ::FirstLine[ ED_BUFFER ] )

      //TraceLog( ValToPrg( ::aComments ) )

      IF HB_QWith() != NIL
         IF lResetDisplay
            ::SetDisplay( ::oDisplay, lDisplay )
         ELSE
            IF ! Empty( cExt )
               ::SetExtension( cExt )
            ENDIF

            :BaseLine  := ::FirstLine
            :nBaseLine := 0
            :nRow      := 0
            :nColumn   := 0
         ENDIF
      ENDIF
   END

   #ifdef WIN
      SetCursor( hCursor )
   #endif

RETURN Self

METHOD IsCommentedFunction( nFunc ) CLASS Editor

   LOCAL aComment, aFunc := ::aFunctions[ nFunc ]
   LOCAL nLastOpener := 0, nLastCloser := 0

   FOR EACH aComment IN ::aComments
      //TraceLog( aComment[1], aComment[2], aFunc[1] )

      IF aComment[1] == 0
         LOOP
      ENDIF

      IF aComment[1] == nLastOpener .OR. aComment[1] < nLastCloser
         LOOP
      ENDIF

      IF aComment[1] > aFunc[1]
         EXIT
      ENDIF

      IF aComment[1] <= aFunc[1] .AND. aComment[2] >= aFunc[1]
         //TraceLog( "COMMENTED" )
         RETURN .T.
      ENDIF

      nLastOpener  := aComment[1]
      nLastCloser  := aComment[2]
  NEXT

RETURN .F.

METHOD Collapse( Line )  CLASS Editor

   LOCAL aFunc, nFunc := aScan( ::aFunctions, {|_1| _1[3] == Line } )

   //TraceLog( nFunc, ::aFunctions[nFunc][1], ::nLines )

   IF nFunc == 0
      TraceLog( "Oops!" )
      RETURN Self
   ENDIF

   aFunc := ::aFunctions[ nFunc ]

   IF ::IsCommentedFunction( nFunc )
      IF nFunc > 1 .AND. Len( ::aFunctions[ nFunc - 1 ][3] ) > ED_BASE_LEN
         //TraceLog( nFunc, Len( ::aFunctions ) )

         // Relink to end of prior collapsed
         Line[ ED_PREVLINE ] := ::aFunctions[ nFunc - 1 ][3][ ED_COLLAPSED_END ]

         IF nFunc < Len( ::aFunctions )
            ::aFunctions[ nFunc - 1 ][3][ ED_NEXTLINE ] := ::aFunctions[ nFunc + 1 ][3]
            ::aFunctions[ nFunc - 1 ][3][ ED_COLLAPSED_END ] := ::aFunctions[ nFunc + 1 ][3][ ED_PREVLINE ]
            ::aFunctions[ nFunc - 1 ][3][ ED_COLLAPSED_LINES ] += ::aFunctions[ nFunc + 1 ][1] - aFunc[1] - 1
         ELSE
            ::aFunctions[ nFunc - 1 ][3][ ED_NEXTLINE ] := NIL
            ::aFunctions[ nFunc - 1 ][3][ ED_COLLAPSED_END ] := ::LastLine
            ::aFunctions[ nFunc - 1 ][3][ ED_COLLAPSED_LINES ] += CountLines( Line )
         ENDIF
      ENDIF

      RETURN Self
   ENDIF

   IF Len( Line ) > ED_BASE_LEN
      TraceLog( "Allready collapsed!" )
      RETURN Self
   ENDIF

   IF nFunc < Len( ::aFunctions )
      IF aFunc[1] + 1 == ::aFunctions[ nFunc + 1 ][1]
         RETURN Self
      ENDIF
   ELSE
      IF aFunc[1] == ::nLines
         RETURN Self
      ENDIF
   ENDIF

   aSize( Line, ED_FULL_LEN )

   // Save pointer to First Hidden Line.
   Line[ ED_COLLAPSED_START ] := Line[ ED_NEXTLINE ]

   IF nFunc < Len( ::aFunctions )
      // Hide until Next Entity
      Line[ ED_NEXTLINE ] := ::aFunctions[ nFunc + 1 ][3]

      // Save pointer to Last Hidden Line.
      Line[ ED_COLLAPSED_END ] := Line[ ED_NEXTLINE ][ ED_PREVLINE ]

      // Hide until Previous Entity
      Line[ ED_NEXTLINE ][ ED_PREVLINE ] := Line
   ELSE
      // Hide until EOF
      Line[ ED_NEXTLINE ] := NIL

      // Save pointer to Last Hidden Line.
      Line[ ED_COLLAPSED_END ] := ::LastLine

      // Hide backward from EOF
      ::LastLine := Line
   ENDIF

   // Attach any subsequent commented function[s].
   WHILE nFunc < Len( ::aFunctions )
      IF ::IsCommentedFunction( ++nFunc )
         IF nFunc < Len( ::aFunctions )
            // Hide until Next Entity
            Line[ ED_NEXTLINE ] := ::aFunctions[ nFunc + 1 ][3]

            // Save pointer to Last Hidden Line.
            Line[ ED_COLLAPSED_END ] := Line[ ED_NEXTLINE ][ ED_PREVLINE ]

            // Hide until Previous Entity
            Line[ ED_NEXTLINE ][ ED_PREVLINE ] := Line
         ELSE
            // Hide until EOF
            Line[ ED_NEXTLINE ] := NIL

            // Save pointer to Last Hidden Line.
            Line[ ED_COLLAPSED_END ] := ::LastLine

            // Hide backward from EOF
            ::LastLine := Line
         ENDIF
      ELSE
         EXIT
      ENDIF
   END

   // Save number of hidden lines.
   Line[ ED_COLLAPSED_END ][ ED_NEXTLINE ] := NIL
   Line[ ED_COLLAPSED_LINES ] := CountLines( Line[ ED_COLLAPSED_START ] )
   Line[ ED_COLLAPSED_END ][ ED_NEXTLINE ] := Line[ ED_NEXTLINE ]
   ::nLines -= Line[ ED_COLLAPSED_LINES ]

   ::oDisplay:Display()

RETURN Self

METHOD Expand( Line )  CLASS Editor

   LOCAL aFunc, nFunc := aScan( ::aFunctions, {|_1| _1[3] == Line } )

   //TraceLog( nFunc, ::aFunctions[nFunc][1] )

   IF nFunc == 0
      TraceLog( "Oops!" )
      RETURN Self
   ENDIF

   aFunc := ::aFunctions[ nFunc ]

   IF Len( Line ) == ED_BASE_LEN
      TraceLog( "Already expanded!" )
      RETURN Self
   ENDIF

   //TraceLog( nFunc, Len( ::aFunctions ), Line[ ED_BUFFER ] )

   IF nFunc < Len( ::aFunctions )
      // Restore backward link.
      Line[ ED_NEXTLINE ][ ED_PREVLINE ] := Line[ ED_COLLAPSED_END ]
   ELSE
     // Restore true EOF.
     ::LastLine := Line[ ED_COLLAPSED_END ]
   ENDIF

   // Unhide
   Line[ ED_NEXTLINE ] := Line[ ED_COLLAPSED_START ]

   ::nLines += Line[ ED_COLLAPSED_LINES ]

   aSize( Line, ED_BASE_LEN )

   ::oDisplay:Display()

RETURN Self

METHOD CollapseAll()

   LOCAL aFunc

   WITH OBJECT ::oDisplay
      :nDeferDisplay++

      FOR EACH aFunc IN ::aFunctions
         IF Len( aFunc[3] ) > ED_BASE_LEN
            LOOP
         ENDIF

         ::Collapse( aFunc[3] )
      NEXT

      :nBaseLine   := 0
      :nBaseColumn := 0
      :nRow        := 0
      :nColumn     := 0

      :BaseLine := ::FirstLine

      :nDeferDisplay--
      ASSERT( ::nDeferDisplay >= 0 )
      :Display()
   END

RETURN Self

METHOD ExpandAll()

   LOCAL aFunc

   ::oDisplay:nDeferDisplay++

   FOR EACH aFunc IN ::aFunctions
      IF Len( aFunc[3] ) == ED_BASE_LEN
         LOOP
      ENDIF

      ::Expand( aFunc[3] )
   NEXT

   ::oDisplay:nDeferDisplay--
   ASSERT( ::nDeferDisplay >= 0 )
   ::oDisplay:Display()

RETURN Self

METHOD AddCommentOpener( nLine, hDC )

   LOCAL nBegin := nLine + 1
   LOCAL nAt := aScan( ::aComments, {|_1| _1[1] == nBegin } ), nNearest, lUsed := .F.

   //TraceLog( nLine, nAt )

   IF nAt > 0
      // Already exists.
      //TraceLog( "Already added" )
      RETURN .F.
   ELSE
      // Mark all Closers to whom this new Opener is the new valid opener.

      // First we have to find if the new opener is free standing, or nested within an existing comment.
      nAt := aScan( ::aComments, {|_1| _1[1] < nBegin .AND. ( _1[2] == 0 .OR. _1[2] >= nBegin ) }, nAt + 1 )

      // Nested - do NOT manipulate existing blocks - simply add it along with it's closest closer
      IF nAt == 0
          WHILE .T.
             // If we are here it must be a valid new Free standing Opener!

             // New Opener is outer (or first available).
             nAt := aScan( ::aComments, {|_1| ( _1[1] == 0 .OR. _1[1] > nBegin ) .AND. _1[2] >= nBegin }, nAt + 1 )

             IF nAt == 0
                EXIT
             ENDIF

             //TraceLog( nAt )

             IF ::aComments[ nAt ][1] == 0
                ::aComments[ nAt ][1] := nBegin
                //TraceLog( "Opened", nLine, ::aComments[nAt][1], ::aComments[nAt][2] )
             ELSE
                IF aScan( ::aComments, {|_1| _1[1] == ::aComments[ nAt ][1] .AND. ! _1 == ::aComments[ nAt ] } ) > 0
                   // There are more Blocks using this opener so we can safely recycle this block.
                   ::aComments[ nAt ][1] := nBegin
                ELSEIF aScan( ::aComments, {|_1| _1[1] == nBegin .AND. _1[2] == ::aComments[ nAt ][2] } ) > 0
                   // Already has such!
                ELSE
                   aAdd( ::aComments, { nBegin, ::aComments[ nAt ][2] } )
                ENDIF
             ENDIF

             lUsed := .T.
          END
      ENDIF

      IF ! lUsed
         // Find the nearest Closer.
         nAt      := 0
         nNearest := 0

         WHILE .T.
            nAt := aScan( ::aComments, {|_1| _1[2] >= nLine }, nAt + 1 )

            IF nAt == 0
               EXIT
            ENDIF

            //TraceLog( nAt )

            IF nNearest == 0
               nNearest := ::aComments[ nAt ][2]
            ELSE
               nNearest := Min( ::aComments[ nAt ][2], nNearest )
            ENDIF
         END

         IF nNearest > 0
            // "Adopt" nearest Closer.
            aAdd( ::aComments, { nBegin, nNearest } )
         ELSE
            // No available Closer yet.
            aAdd( ::aComments, { nBegin, 0 } )
         ENDIF
      ENDIF

      aSort( ::aComments, , , {|_1, _2| IIF(_1[1] == _2[1], _1[2] < _2[2] , _1[1] < _2[1] ) } )

      IF hDC != NIL
         //TraceLog( nLine, ValToPrg( ::aComments ) )
         ::oDisplay:Display( hDC )
      ENDIF
   ENDIF

RETURN .T.

METHOD DelCommentOpener( nLine, hDC )

   LOCAL nAt := 0, nOpener, nNearest, lFound := .F.
   LOCAL nBegin := nLine + 1

   WHILE .T.
      nAt := aScan( ::aComments, {|_1| _1[1] == nBegin }, nAt + 1 )

      IF nAt == 0
         EXIT
      ENDIF

      //TraceLog( "Process", nLine, nAt, ::aComments[nAt][1], ::aComments[nAt][2] )
      lFound := .T.

      IF ::aComments[nAt][2] == 0 .OR. aScan( ::aComments, {|_1| _1[2] == ::aComments[nAt][2] .AND. ! _1 == ::aComments[nAt] } ) > 0
         aDel( ::aComments, nAt, .T. )
         //TraceLog( "Deleted", nAt )
      ELSE
         // Find nearest Opener.
         nOpener  := 0
         nNearest := 0

         WHILE .T.
            nOpener := aScan( ::aComments, {|_1| _1[1] <= nLine }, nOpener + 1 )

            IF nOpener == 0
               EXIT
            ENDIF

            // Openers are pre-sorted, so latest found will be higher then previous.
            nNearest := ::aComments[ nOpener ][1]
         END

         //TraceLog( "Nearest", nNearest )

         IF nNearest > 0
            // "Adopt" the nearest Opener.
            ::aComments[nAt][1] := nNearest
         ELSE
            // No availabale Opener yet.
            ::aComments[nAt][1] := 0
         ENDIF
      ENDIF
   END

   IF lFound
      aSort( ::aComments, , , {|_1, _2| IIF(_1[1] == _2[1], _1[2] < _2[2] , _1[1] < _2[1] ) } )

      IF hDC != NIL
         //TraceLog( nLine, ValToPrg( ::aComments ) )
         ::oDisplay:Display( hDC )
      ENDIF
   ENDIF

RETURN lFound

METHOD AddCommentCloser( nLine, hDC )

   LOCAL nEnd := nLine - 1
   LOCAL nAt := aScan( ::aComments, {|_1| _1[2] == nEnd } ), nNearest, lUsed := .F.

   //TraceLog( nLine, nAt )

   IF nAt > 0
      // Already exists.
      RETURN .F.
   ELSE
      // Mark all Openers to whom this new Closer is nearest.
      nAt := 0
      WHILE .T.
         // Opener is valid.
         nAt := aScan( ::aComments, {|_1| _1[1] <= nLine .AND. ( _1[2] == 0 .OR. _1[2] > nEnd ) }, nAt + 1 )

         IF nAt == 0
            EXIT
         ENDIF

         lUsed := .T.

         // New Closer is nearer.

         IF ::aComments[ nAt ][2] == 0
            //TraceLog( "Closed", nAt, nLine, ::aComments[nAt][1], ::aComments[nAt][2] )
            ::aComments[ nAt ][2] := nEnd
         ELSE
            IF aScan( ::aComments, {|_1| _1[2] == ::aComments[ nAt ][2] .AND. ! _1 == ::aComments[ nAt ] } ) > 0
               // Closer may be overriden, duplicates available.
               ::aComments[ nAt ][2] := nEnd
            ELSEIF aScan( ::aComments, {|_1| _1[1] == ::aComments[ nAt ][1] .AND. _1[2] == nEnd } ) > 0
               // Already has such!
            ELSE
               aAdd( ::aComments, { ::aComments[ nAt ][1], nEnd } )
            ENDIF
         ENDIF
      END

      IF ! lUsed
         // Find the nearest Opener.
         nAt      := 0
         nNearest := 0

         WHILE .T.
            nAt := aScan( ::aComments, {|_1| _1[1] <= nLine }, nAt + 1 )

            IF nAt == 0
               EXIT
            ENDIF

            // Openers are pre-sorted, so latest found will be higher then previous.
            nNearest := ::aComments[ nAt ][1]
         END

         IF nNearest > 0
            // "Adopt" nearest Opener.
            aAdd( ::aComments, { nNearest, nEnd } )
         ELSE
            // No available Opener yet.
            aAdd( ::aComments, { 0, nEnd } )
         ENDIF
      ENDIF

      aSort( ::aComments, , , {|_1, _2| IIF(_1[1] == _2[1], _1[2] < _2[2] , _1[1] < _2[1] ) } )

      IF hDC != NIL
         //TraceLog( nLine, ValToPrg( ::aComments ) )
         ::oDisplay:Display( hDC )
      ENDIF
   ENDIF

RETURN .T.

METHOD DelCommentCloser( nLine, hDC )

   LOCAL nAt := 0, nCloser, nNearest, lFound := .F.
   LOCAL nEnd := nLine - 1

   WHILE .T.
      nAt := aScan( ::aComments, {|_1| _1[2] == nEnd }, nAt + 1 )

      IF nAt == 0
         EXIT
      ENDIF

      //TraceLog( "Process", nLine, nAt, ::aComments[nAt][1], ::aComments[nAt][2] )

      lFound := .T.

      IF ::aComments[nAt][1] == 0 .OR. aScan( ::aComments, {|_1| _1[1] == ::aComments[nAt][1] .AND. ! _1 == ::aComments[nAt] } ) > 0
         aDel( ::aComments, nAt, .T. )
         //TraceLog( "Deleted", nAt )
      ELSE
         // Find nearest Closer.
         nCloser  := 0
         nNearest := 0

         WHILE .T.
            nCloser := aScan( ::aComments, {|_1| _1[2] > nEnd }, nCloser + 1 )

            IF nCloser == 0
               EXIT
            ENDIF

            IF nNearest == 0
               nNearest := ::aComments[ nCloser ][2]
            ELSE
               nNearest := Min( ::aComments[ nCloser ][2], nNearest )
            ENDIF
         END

         //TraceLog( nLine, "Nearest", nNearest )

         IF nNearest > 0
            // "Adopt" the nearest Closer.
            ::aComments[nAt][2] := nNearest
            //TraceLog( "Nearest", nLine, nAt, nNearest )
         ELSE
            // No availabale Closer yet.
            ::aComments[nAt][2] := 0
            //TraceLog( "No Closer" )
         ENDIF
      ENDIF
   END

   IF lFound
      aSort( ::aComments, , , {|_1, _2| IIF(_1[1] == _2[1], _1[2] < _2[2] , _1[1] < _2[1] ) } )

      IF hDC != NIL
         //TraceLog( nLine, ValToPrg( ::aComments ) )
         ::oDisplay:Display( hDC )
      ENDIF
   ENDIF

RETURN lFound

METHOD GetBuffer()  CLASS Editor

   LOCAL sText := "", Line

   Line := ::FirstLine

   WHILE Line != NIL
      sText += Line[ ED_BUFFER ] + EOL

      IF Len( Line ) == ED_BASE_LEN
         Line := Line[ ED_NEXTLINE ]
      ELSE
         Line := Line[ ED_COLLAPSED_START ]
      ENDIF
   ENDDO

RETURN sText

METHOD Save( cFile, bAuto )  CLASS Editor

   LOCAL nAt
   LOCAL hFile, Line

   #ifndef VXH
    #ifdef WIN
       LOCAL tvi
    #endif
   #endif

   #ifdef WIN
     LOCAL hCursor
   #endif

   IF bAuto == NIL
      bAuto := .F.
   ENDIF

   //TraceLog( cFile, ::cPath, ::cFile, bAuto )

   IF cFile == NIL
      cFile := ::cPath + ::cFile
   ELSE
      IF DIR_SEPARATOR IN cFile
         nAt := RAt( DIR_SEPARATOR, cFile )
         ::cPath := Left( cFile, nAt )
         ::cFile := SubStr( cFile, nAt + 1 )
      ELSE
         ::cPath := DiskName() + ":" + DIR_SEPARATOR + CurDir() + DIR_SEPARATOR
         ::cFile := cFile
      ENDIF
   ENDIF

   IF ! Empty( cFile )
      #ifdef WIN
         hCursor := SetCursor( LoadCursor( NIL, IDC_WAIT ) )
      #endif

      //TraceLog( 1, cFile )

      IF ( ! bAuto ) .AND. File( cFile )
         __CopyFile( cFile, cFile + ".bak" )
      ENDIF

      //TraceLog( 2, cFile )

      TRY
         IF File( cFile )
           hFile := FOpen( cFile, FO_READWRITE )
         ELSE
           hFile := FCreate( cFile )
         ENDIF

         IF hFile == -1
            //Alert( "(1) Save error: <" + cFile + ">;;I/O Error (" + Str( FError(), 2 ) + ")" )
            BREAK
         ENDIF

         Line := ::FirstLine
         WHILE Line != NIL
            //TraceLog( Line[ ED_BUFFER ] )
            FWrite( hFile, Line[ ED_BUFFER ] + EOL )

            IF s_lResetOnSave
               Line[ ED_MODIFICATIONS ] := 0
            ENDIF

            IF FError() != 0
               BREAK
            ENDIF

            IF Len( Line ) == ED_BASE_LEN
               Line := Line[ ED_NEXTLINE ]
            ELSE
               Line := Line[ ED_COLLAPSED_START ]
            ENDIF
         ENDDO
         // Truncate
         FSeek( hFile, - Len( EOL ), FS_RELATIVE )
         FWrite( hFile, "", 0 )
         FClose( hFile)

         IF s_lResetOnSave
            ::oDisplay:Display()
         ENDIF

         ::Date := Directory( cFile )[1][ F_DATE ]
         ::Time := Directory( cFile )[1][ F_TIME ]

         ::nLastUnDo := Len( ::aUnDo )
         ::nLastReDo := Len( ::aReDo )
         ::lModified := .F.

         #ifndef VXH
            #ifdef WIN
               IF ::hFileItem != NIL .AND. ::oDisplay:hFilesTree != NIL
                   tvi := (struct TVITEM)

                   tvi:hItem     := ::hFileItem
                   tvi:mask      := TVIF_STATE
                   tvi:stateMask := TVIS_BOLD
                   tvi:state     := IIF( ::lModified, TVIS_BOLD, 0 )

                   SendMessage( ::oDisplay:hFilesTree, TVM_SETITEM, 0, @tvi )
               ENDIF
            #endif
         #endif
         #ifdef WIN
            SetCursor( hCursor )
         #endif
      CATCH
         TraceLog( cFile, FError() )
         Alert( "Save error: <" + cFile + ">;;I/O Error (" + Str( FError(), 2 ) + ")" )
      END
   ENDIF

RETURN Self

METHOD Edit() CLASS Editor

   LOCAL oError

   #ifdef WIN
      LOCAL msg
   #endif

   TRY
      #ifdef WIN
         WITH OBJECT ::oDisplay
            ShowWindow( :hContainer, SW_SHOW )
            SetFocus( :hContainer )

            WHILE GetMessage( @msg, 0, 0, 0 )
               IF ! IsDialogMessage( GetActiveWindow(), msg )
                  TranslateMessage( msg )
                  DispatchMessage( msg )
               ENDIF
            END
         END
      #else
         ::oDisplay:Display()
         WHILE .T.
            ::OnKey( Inkey( 0 ), 1 )
         END
      #endif

      //TraceLog( "NO Error", ::aUnDo, ::cFile )
   CATCH oError
      IF ! Empty( oError )
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      ENDIF
   END

   IF oError != NIL
      #ifdef WIN
         EnableWindow( ::oDisplay:hWnd, .F. )
      #endif

      IF ! oError:ProcName == "EDITOR:SAVE"
          IF Len( ::aUndo ) > 0 .AND. ! Empty( ::cFile )
             #ifdef WIN
                IF MessageBox( 0, "Sorry, I have encountered an Error:" + ;
                                  CRLF + CRLF + ;
                                  oError:Description + CRLF + oError:Operation + CRLF + ;
                                  oError:ModuleName + "->" + oError:ProcName + "(" + Str( oError:ProcLine, 5 ) + ")" + ;
                                  CRLF + CRLF + ;
                                  "Save changes to file: " + ::cPath + ::cFile, ;
                                  "xEdit (Error Recovery)", MB_ICONERROR | MB_YESNO )== IDYES
             #else
                IF Alert( "Sorry, I have encountered an Error:;;" + ;
                          oError:Description + ";" + oError:Operation + ;
                          oError:ModuleName + "->" + oError:ProcName + "(" + Str( oError:ProcLine, 5 ) + ")" + ;
                          ";;" + ;
                          "Save changes to file: " + ::cPath + ::cFile, { "Yes", "No" } ) == 1
             #endif
                   ::Save()
                ENDIF
          ENDIF
      ENDIF

      Throw( oError )
   ENDIF

RETURN Self

METHOD OnKey( nKey, nCount ) CLASS Editor

   LOCAL sSelected, aActions := {}, Counter
   LOCAL sText
   LOCAL nColumn
   LOCAL oEditor
   LOCAL oError
   LOCAL nLines

   //TraceLog( nKey, nCount )

   IF nKey >= 32 .AND. nKey <= 255 .AND. nKey != K_CTRL_BS
      TRY
          WITH OBJECT ::oDisplay
             IF ::lInsert
                ::Action( { { ED_INSERT, ::nLine, ::nColumn, Replicate( Chr( nKey ), nCount ) } }, ::aUnDo )
             ELSE
                ::Action( { { ED_OVERRIDE, ::nLine, ::nColumn, Replicate( Chr( nKey ), nCount ) } }, ::aUnDo )
             ENDIF
          END

          IF ! Empty( ::pAfterKey )
             HB_Exec( ::pAfterKey, , Self, nKey, nCount )
          ENDIF
      CATCH oError
         TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
      END

      RETURN Self
   ENDIF

   TRY
      WITH OBJECT ::oDisplay
         SWITCH nKey
            CASE K_CTRL_TAB
            CASE K_SH_TAB
            CASE K_TAB
               IF ::nLineFrom > 0
                  //TraceLog( ::nLineFrom, ::nColumnFrom, ::nLineTo, ::nColumnTo )

                  IF nKey == K_TAB
                     nLines := ::nLineTo - ::nLineFrom
                     IF ::nColumnTo > 0
                        nLines++
                     ENDIF

                     sSelected := ""
                     FOR Counter := 1 TO nlines
                        sSelected += Space( s_nTabSpaces ) + EOL
                     NEXT

                     aAdd( aActions, { ED_GOTO, ::nLine, ::nColumn } )
                     aAdd( aActions, { ED_SELECT, ::nLineFrom, ::nColumnFrom, ::nLineTo, ::nColumnTo, ::lSquare, ::nDirection } )
                     aAdd( aActions, { ED_PASTE, ::nLineFrom, ::nColumnFrom, sSelected, .T. } )
                     aAdd( aActions, { ( 4 << 8 ) | ED_UNSELECT, ::nLineFrom, ::nColumnFrom, ::nLineTo, ::nColumnTo, ::lSquare, ::nDirection } )

                     ::Action( aActions, ::aUnDo )
                  ELSEIF nKey == K_SH_TAB
                     // INTENTIONAL not using + 1!!!
                     nLines := ::nLineTo - ::nLineFrom
                     IF ::nColumnTo == 0
                        nLines--
                     ENDIF

                     aAdd( aActions, { ED_GOTO, ::nLine, ::nColumn } )
                     aAdd( aActions, { ED_SELECT, ::nLineFrom, ::nColumnFrom, ::nLineTo, ::nColumnTo, ::lSquare, ::nDirection } )

                     // INTENTIONAL using 0!!!
                     FOR Counter := 0 TO nlines
                        //TraceLog( Counter, ::nLineFrom + Counter )
                        aAdd( aActions, { ED_DELETE, ::nLineFrom + Counter, ::nColumnFrom, s_nTabSpaces } )
                     NEXT

                     // nLines ALREADY INCREMENTED at *NEXT* for actual number of line + 3 for GOTO, SELECT,  and DELETE.
                     aAdd( aActions, { ( ( nLines + 3 ) << 8 ) | ED_UNSELECT, ::nLineFrom, ::nColumnFrom, ::nLineTo, ::nColumnTo, ::lSquare, ::nDirection } )

                     ::Action( aActions, ::aUnDo )
                  ENDIF
               ELSE
                  ::OnKey( ' ', nCount * s_nTabSpaces )
               ENDIF
               EXIT

            CASE K_ALT_BS
               ::UnDo()
               EXIT

            CASE K_CTRL_BS
            CASE K_SH_BS
            CASE K_BS
               FOR Counter := 1 TO nCount
                  IF ::nLineFrom > 0
                     ::Action( { { ED_CUT, ::nLine, ::nColumn } }, ::aUnDo )
                     EXIT
                  ENDIF

                  IF ::nLine == 1 .AND. ::nColumn == 1
                     EXIT
                  ENDIF

                  IF nKey == K_CTRL_BS
                     nColumn := ::nColumn - 1
                     WHILE nColumn > 1 .AND. ::CurrentLine[ ED_BUFFER ][ nColumn ] == ' '
                        nColumn--
                     END
                     nColumn := RAt( " ", Left( ::CurrentLine[ ED_BUFFER ], nColumn ) ) + 1

                     ::Action( { { ED_DELETE, ::nLine, nColumn, ::nColumn - nColumn + 1 } }, ::aUnDo )
                  ELSEIF ::nColumn > 1
                     ::Action( { { ED_BACKSPACE, ::nLine, ::nColumn } }, ::aUnDo )
                  ELSE
                     sSelected := ::CurrentLine[ ED_BUFFER ]
                     aAdd( aActions, { ED_GOTO, ::nLine - 1, Len( ::CurrentLine[ ED_PREVLINE ][ ED_BUFFER ] ) + 1 } )
                     aAdd( aActions, { ED_INSERT, ::nLine - 1, Len( ::CurrentLine[ ED_PREVLINE ][ ED_BUFFER ] ) + 1, sSelected } )
                     aAdd( aActions, { ( 3 << 8 ) | ED_DELETELINE, ::nLine } )
                     ::Action( aActions, ::aUnDo )
                  ENDIF
               NEXT
               EXIT

            CASE K_INS
               aAdd( aActions, { ED_TOGGLEINSERT, ::nLine, ::nColumn } )
               ::Action( aActions, ::aUnDo )
               EXIT

            CASE K_DEL
               //TraceLog( ::nColumn )
               //aAdd( aActions, { ED_GOTO, ::nLine, ::nColumn } )
               aAdd( aActions, { ED_DELETE, ::nLine, ::nColumn, nCount } )
               ::Action( aActions, ::aUnDo )
               EXIT

            CASE K_RETURN
               IF ::lInsert
                  ::Action( { { ED_BREAKLINE, ::nLine, ::nColumn } }, ::aUnDo )
               ELSE
                  IF ::nLine == ::nLines
                     aAdd( aActions, { ED_ADDLINE, ::nLine, "" } )
                     ::Action( aActions, ::aUnDo )
                  ELSE
                     ::Down(1)
                     ::LineHome()
                  ENDIF
               ENDIF
               EXIT

            CASE K_SH_HOME
            CASE K_HOME
               ::LineHome()
               EXIT

            CASE K_CTRL_HOME
               ::Home()
               EXIT

            CASE K_SH_END
            CASE K_END
               ::LineEnd()
               EXIT

            CASE K_CTRL_END
               ::End()
               EXIT

            CASE K_SH_UP
            CASE K_UP
               ::Up( nCount )
               EXIT

            CASE K_SH_DOWN
            CASE K_DOWN
               ::Down( nCount )
               EXIT

            CASE K_SH_LEFT
            CASE K_CTRL_LEFT
            CASE K_LEFT
               ::Left( nCount )
               EXIT

            CASE K_SH_RIGHT
            CASE K_CTRL_RIGHT
            CASE K_RIGHT
               ::Right( nCount )
               EXIT

            CASE K_SH_PGUP
            CASE K_PGUP
               ::PageUp( nCount )
               EXIT

            CASE K_SH_PGDN
            CASE K_PGDN
               ::PageDown( nCount )
               EXIT

            CASE K_CTRL_A
               ::Home()
               ::lShift := .T.
               ::End()
               ::lShift := .F.
               EXIT

            CASE K_CTRL_B
               ::ToggleBookmark()
               EXIT

            CASE K_CTRL_INS
            CASE K_CTRL_C
               IF ::nLineFrom == 0
                  ::SelectLine()
               ENDIF

               IF ::nColumnFrom == 1 .AND. ::nColumnTo == 0
                  ::lLine := .T.
               ELSE
                  ::lLine := .F.
               ENDIF

               ::cClipBoard  := ::SelectedText()
               ::lClipSquare := ::lSquare

               #ifdef WIN
                  IF OpenClipboard( ::oDisplay:hWnd )
                     EmptyClipboard()
                     SetClipboardData( CF_TEXT, GlobalString( ::cClipBoard ) )
                     CloseClipboard()
                  ENDIF
               #endif
               EXIT

            CASE K_CTRL_F
               ::lCtrl := .F.
               #ifdef WIN
                  SendMessage( ::oDisplay:hFindTabControl, TCM_SETCURFOCUS, 0, 0 )
                  ShowWindow( ::oDisplay:hFind, SW_SHOW )
               #endif
               EXIT

            CASE K_CTRL_G
               ::lCtrl := .F.
               #ifdef WIN
                  SendMessage( ::oDisplay:hFindTabControl, TCM_SETCURFOCUS, 2, 0 )
                  ShowWindow( ::oDisplay:hFind, SW_SHOW )
               #endif
               EXIT

            CASE K_CTRL_H
               ::lCtrl := .F.
               #ifdef WIN
                  SendMessage( ::oDisplay:hFindTabControl, TCM_SETCURFOCUS, 1, 0 )
                  ShowWindow( ::oDisplay:hFind, SW_SHOW )

                  IF ::nLineFrom > 0 .AND. ::nLineFrom == ::nLineTo
                     SetDlgItemText( :hFindDialog, ID_Find, ::SelectedText() )
                  ENDIF
               #endif
               EXIT

            CASE K_CTRL_L
               FOR EACH oEditor IN s_aEditors
                  IF oEditor:lModified
                     oEditor:Save()
                  ENDIF
               NEXT
               EXIT

            CASE K_CTRL_O
               #ifdef WIN
               IF ! Empty( :hContainer )
                  SendMessage( :hContainer, WM_COMMAND, MAKELONG( IDM_FILE_OPEN, 0 ), 0 )
               ENDIF
               #endif
               EXIT

            CASE K_CTRL_S
               ::Save()
               EXIT

            CASE K_SH_INS
               ::lShift := .F.
            CASE K_CTRL_V
               #ifdef WIN
                  IF IsClipboardFormatAvailable( CF_TEXT )
                     IF OpenClipboard( ::oDisplay:hWnd )
                        sText := GetClipboardData( CF_TEXT )
                        CloseClipboard()

                        IF sText[-1] == 0
                           sText := Left( sText, Len( sText ) - 1 )
                        ENDIF
                     ENDIF
                  ENDIF
               #else
                   sText := ::cClipBoard
               #endif

               IF ::nLineFrom == 0
                  IF ::lLine
                     #ifdef VXH
                        // workaround pasting after copy/cut the whole row
                        ::Action( { { ED_PASTE, ::nLine, ::nColumn, sText } }, ::aUndo )
                     #else
                        ::Action( { { ED_PASTE, ::nLine, 1, sText } }, ::aUndo )
                     #endif
                  ELSE
                     ::Action( { { ED_PASTE, ::nLine, ::nColumn, sText } }, ::aUndo )
                  ENDIF
               ELSE
                  ::Action( { { ED_PASTE, ::nLineFrom, ::nColumnFrom, sText } }, ::aUndo )
               ENDIF
               EXIT

            CASE K_SH_DEL
               ::lShift := .F.
            CASE K_CTRL_X
               IF ::nLineFrom == 0
                  ::SelectLine()
               ENDIF

               IF ::nColumnFrom == 1 .AND. ::nColumnTo == 0
                  ::lLine := .T.
               ELSE
                  ::lLine := .F.
               ENDIF

               ::cClipBoard  := ::SelectedText()
               ::lClipSquare := ::lSquare

               ::Action( { { ED_CUT, ::nLine, ::nColumn } }, ::aUndo )

               #ifdef WIN
                  IF OpenClipboard( ::oDisplay:hWnd )
                     EmptyClipboard()
                     SetClipboardData( CF_TEXT, GlobalString( ::aUndo[-1][4] ) )
                     CloseClipboard()
                  ENDIF
               #endif
               EXIT

            CASE K_CTRL_Y
               IF ::lShift
                  ::ReDo()
               ELSE
                  ::ReDoFast()
               ENDIF
               EXIT

            CASE K_CTRL_Z
               IF ::lShift
                  ::UnDo()
               ELSE
                  ::UnDoFast()
               ENDIF
               EXIT

            CASE K_ESC
               IF ::oDisplay:lEscape
                  BREAK
               ENDIF
               EXIT

            CASE K_CTRL_E
               //TraceLog( ::lAlt, ::lCtrl, ::lShift )

               IF Empty( ::aFunctions )
                  EXIT
               ENDIF

               IF ::lShift
                  ::lShift := .F.

                  IF Len( ::aFunctions[1][3] ) > ED_BASE_LEN
                     ::Action( {{ ED_EXPANDALL, ::nLine } }, ::aUnDo )
                  ELSE
                     ::Action( {{ ED_COLLAPSEALL, ::nLine } }, ::aUnDo )
                  ENDIF
               ELSE
                  IF Len( ::CurrentLine ) == ED_BASE_LEN
                     ::Action( { { ED_COLLAPSE, ::nLine } }, ::aUndo )
                  ELSE
                     ::Action( { { ED_EXPAND, ::nLine } }, ::aUndo )
                  ENDIF
               ENDIF
               EXIT
         END
      END

      IF ! Empty( ::pAfterKey )
         HB_Exec( ::pAfterKey, , Self, nKey, nCount )
      ENDIF

   CATCH oError
      TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine )
   END

RETURN Self

METHOD Action( aActions, aReverse ) CLASS Editor

   LOCAL aAction, sSelected, nAction, nLine := ::nLine, nColumn := ::nColumn, nLen, nExtraActions := 0
   LOCAL aBatch, nLines, sLine
   LOCAL nLineFrom, nColumnFrom, nLineTo, nColumnTo
   LOCAL lOneMore
   LOCAL oError
   LOCAL lSquare
   LOCAL nFunc
   LOCAL nDirection
   LOCAL nPad
   LOCAL nCursorLine
   LOCAL nDeferDisplay := 0
   LOCAL lImpacted := .F.

   #ifndef VXH
      #ifdef WIN
         LOCAL tvi
      #endif
   #endif

   //TraceLog( aActions, aReverse )

   IF Empty( aActions )
      RETURN lImpacted
   ENDIF

   IF ( ! aActions == ::aRedo ) .AND. aReverse == ::aUnDo
      aSize( ::aRedo, 0 )
   ENDIF

   ::aActions := aActions

   aAction := aActions[ -1 ]
   aSize( aActions, Len( aActions ) - 1 )

   TRY
      WITH OBJECT ::oDisplay

         IF aAction[1] > 256
            ::nActions := aAction[1] >> 8
            aAction[1] := aAction[1] & 0xFF

            #if 1
               IF aActions == ::aUndo .OR. aActions == ::aRedo
                  nDeferDisplay++
                  :nDeferDisplay++
               ENDIF
            #endif
         ELSE
            ::nActions := 1
         ENDIF

         FOR nAction := 1 TO ::nActions
            //Alert( Str( nAction ) + " of: " + Str( ::nActions ) )
            //TraceLog( nAction, ::nActions, aAction[1], aAction[2], IIF( Len( aAction ) > 2, aAction[3], NIL ), IIF( Len( aAction ) > 3, aAction[4], NIL ), IIF( Len( aAction ) > 4, aAction[5], NIL ) )

            IF :nRow == -1  .OR. :nColumn == -1
               ShowCaret( :hWnd )

               IF :nRow == -1
                  :nRow := 0
               ENDIF
               IF :nColumn == -1
                  :nColumn := 0
               ENDIF
            ENDIF

            IF aAction[1] < ED_NOIMPACT
               lImpacted := .T.
            ENDIF

            //TraceLog( nAction, aAction[2], :nBaseLine, ::nLines )
            IF aAction[1] == ED_UNSELECT
               IF aAction[7] == 1
                  nCursorLine := aAction[4]
               ELSE
                  nCursorLine := aAction[2]
               ENDIF

               IF nCursorLine <= ::nLines
                  IF ::nLine == nCursorLine
                     // Already there.
                  ELSEIF nCursorLine <= :nBaseLine .OR. nCursorLine >= :nBaseLine + :nRows
                     ::GoLine( nCursorLine )
                  ELSE
                     :GoRow( nCursorLine - :nBaseLine - 1 )
                  ENDIF
               ELSE
                  ::GoLine( ::nLines )
               ENDIF
            ELSE
               IF aAction[2] <= ::nLines
                  IF ::nLine == aAction[2]
                     // Already there.
                  ELSEIF aAction[2] <= :nBaseLine .OR. aAction[2] >= :nBaseLine + :nRows
                     ::GoLine( aAction[2] )
                  ELSE
                     :GoRow( aAction[2] - :nBaseLine - 1 )
                  ENDIF
               ELSE
                  ::GoLine( ::nLines )
               ENDIF
            ENDIF
            //:GoColumn( :nColumn )

            //TraceLog( ::CurrentLine[ ED_BUFFER ] )

            IF aAction[1] < ED_NOIMPACT
               IF ::lReadOnly
                  nExtraActions--

                  IF nAction < ::nActions
                     aAction := aActions[ -1 ]
                     aSize( aActions, Len( aActions ) - 1 )
                  ENDIF

                  LOOP
               ENDIF

               IF Len( ::CurrentLine ) > ED_BASE_LEN
                  aAdd( aActions, aAction )
                  ::nActions++

                  IF ::nLine > 1 .AND. Len( ::CurrentLine[ ED_PREVLINE ] ) > ED_BASE_LEN
                     aAction[2] += ::CurrentLine[ ED_PREVLINE ][ ED_COLLAPSED_LINES ]
                     aAdd( aActions, { ED_EXPAND, ::nLine - 1 } )
                     ::nActions++
                  ENDIF

                  aAction := { ED_EXPAND, ::nLine }
               ENDIF
            ENDIF

            SWITCH aAction[ 1 ]
               CASE ED_GOTO
                  IF Len( aAction ) == 5
                     aAdd( aReverse, { ED_GOTO, aAction[4], aAction[5], aAction[2], aAction[3] } )
                  ELSE
                     aAdd( aReverse, { ED_GOTO, nLine, nColumn } )
                  ENDIF

                  ::nColumn := aAction[3]
                  EXIT

               CASE ED_BACKSPACE
                  ::nPreferedColumn := 0

                  IF ::nLineFrom > 0
                     aAdd( aActions, { ED_CUT, aAction[2], aAction[3] } )
                     nAction--
                     EXIT
                  ENDIF

                  aAdd( aReverse, { ED_INSERT, aAction[2], aAction[3] - 1, ::CurrentLine[ ED_BUFFER ][ aAction[3] - 1 ] } )
                  //TraceLog( Len( aReverse ), aAction[3], ::CurrentLine[ ED_BUFFER ][ aAction[3] - 1 ] )

                  ::CurrentLine[ ED_BUFFER ] := Left( ::CurrentLine[ ED_BUFFER ], aAction[3] - 2 ) + SubStr( ::CurrentLine[ ED_BUFFER ], aAction[3] )

                  IF aActions == ::aUnDo
                     ::CurrentLine[ ED_MODIFICATIONS ]--
                  ELSE
                     ::CurrentLine[ ED_MODIFICATIONS ]++
                  ENDIF

                  ::nColumn := aAction[3] - 1
                  //TraceLog( ::nColumn, :nColumn, aAction[3] )
                  EXIT

               CASE ED_DELETE
                  ::nPreferedColumn := 0

                  IF ::nLineFrom > 0
                     aAdd( aActions, { ED_CUT, aAction[2], aAction[3] } )
                     nAction--
                     EXIT
                  ENDIF

                  IF aAction[2] < ::nLines .AND. aAction[3] > Len( ::CurrentLine[ ED_BUFFER ] )
                     sSelected := ::CurrentLine[ ED_NEXTLINE][ ED_BUFFER ]

                     aAdd( aReverse, { ED_DELETE, aAction[2], aAction[3], Len( sSelected ) } )

                     ::CurrentLine[ ED_BUFFER ] += sSelected
                     IF aActions == ::aUnDo
                        ::CurrentLine[ ED_MODIFICATIONS ]--
                     ELSE
                        ::CurrentLine[ ED_MODIFICATIONS ]++
                     ENDIF

                     ::Down(1)

                     IF ::nLine < ::nLines
                        aAdd( aReverse, { ED_INSERTLINE, ::nLine, sSelected } )
                     ELSE
                        aAdd( aReverse, { ED_ADDLINE, ::nLine, sSelected } )
                     ENDIF
                     nExtraActions++

                     ::DeleteLine()
                     ::Up(1)
                     ::nColumn := aAction[3]
                     EXIT
                  ENDIF

                  IF Len( ::CurrentLine[ ED_BUFFER ] ) > 0
                     sSelected := SubStr( ::CurrentLine[ ED_BUFFER ], aAction[3], aAction[4] )
                     aAdd( aReverse, { ED_INSERT, aAction[2], aAction[3], sSelected } )

                     ::CurrentLine[ ED_BUFFER ] := Left( ::CurrentLine[ ED_BUFFER ], aAction[3] - 1 ) + SubStr( ::CurrentLine[ ED_BUFFER ], aAction[3] + aAction[4] )

                     IF aActions == ::aUnDo
                        ::CurrentLine[ ED_MODIFICATIONS ]--
                     ELSE
                        ::CurrentLine[ ED_MODIFICATIONS ]++
                     ENDIF

                     ::nColumn := aAction[3]
                  ELSE
                     IF aAction[2] >= ::nLines
                        aAdd( aReverse, { ED_ADDLINE, aAction[2], ::CurrentLine[ ED_BUFFER ] } )
                     ELSE
                        aAdd( aReverse, { ED_INSERTLINE, aAction[2], ::CurrentLine[ ED_BUFFER ] } )
                     ENDIF
                     ::DeleteLine()
                  ENDIF
                  EXIT

               CASE ED_OVERRIDE
                  IF aAction[3] - 1 > Len( ::CurrentLine[ ED_BUFFER ] )
                     aAction[1] := ED_INSERT

                     //aAdd( aActions, aAction )
                     nAction--
                     //EXIT
                     LOOP
                  ENDIF

                  ::nPreferedColumn := 0

                  IF ::nLineFrom > 0
                     aAction[2] := ::nLineFrom
                     aAction[3] := ::nColumnFrom

                     aAdd( aActions, aAction )
                     nAction--

                     aAdd( aActions, { ED_CUT, aAction[2], aAction[3] } )
                     ::nActions++
                     EXIT
                  ENDIF

                  IF Len( aReverse ) >= 1 .AND. aReverse[-1][1] == ED_GOTO .and. aReverse[-1][2] == aAction[2] .AND. aReverse[-1][3] == aAction[3]
                     aAdd( aReverse, { ED_OVERRIDE, aAction[2], aAction[3], ::CurrentLine[ ED_BUFFER ][ aAction[3] ] } )
                  ELSE
                     aAdd( aReverse, { ED_GOTO, aAction[2], aAction[3] } )

                     aAdd( aReverse, { ED_OVERRIDE, aAction[2], aAction[3], ::CurrentLine[ ED_BUFFER ][ aAction[3] ] } )
                     nExtraActions++
                  ENDIF

                  ::CurrentLine[ ED_BUFFER ] := Left( ::CurrentLine[ ED_BUFFER ], aAction[3] - 1 ) + aAction[4] + SubStr( ::CurrentLine[ ED_BUFFER ], aAction[3] + 1 )

                  IF aActions == ::aUnDo
                     ::CurrentLine[ ED_MODIFICATIONS ]--
                  ELSE
                     ::CurrentLine[ ED_MODIFICATIONS ]++
                  ENDIF

                  ::nColumn := aAction[3] + 1
                  EXIT

               CASE ED_INSERT
                  //TraceLog( aAction[3], aAction[4] )

                  ::nPreferedColumn := 0

                  IF ::nLineFrom > 0
                     //TraceLog( ::nLineFrom )
                     aAction[2] := ::nLineFrom
                     aAction[3] := Max( ::nColumnFrom, 1 )

                     aAdd( aActions, aAction )
                     nAction--

                     aAdd( aActions, { ED_CUT, ::nLineFrom, ::nColumnFrom } )
                     ::nActions++
                     EXIT
                  ENDIF

                  IF aAction[3] - 1 > Len( ::CurrentLine[ ED_BUFFER ] )
                     nPad := aAction[3] - 1 - Len( ::CurrentLine[ ED_BUFFER ] )
                     aAction[4] := Space( nPad ) + aAction[4]
                  ELSE
                     nPad := 0
                  ENDIF

                  nLen := Len( aAction[4] )
                  IF nLen == 0
                     EXIT
                  ELSEIF nLen == 1
                     aAdd( aReverse, { ED_BACKSPACE, aAction[2], aAction[3] + 1 } )
                  ELSE
                     IF nPad == 0
                        aAdd( aReverse, { ED_DELETE, aAction[2], aAction[3], nLen } )
                     ELSE
                        aAdd( aReverse, { ED_GOTO, aAction[2], aAction[3] } )
                        aAdd( aReverse, { ED_DELETE, aAction[2], aAction[3] - nPad, nLen } )
                        nExtraActions++
                     ENDIF
                  ENDIF

                  ::CurrentLine[ ED_BUFFER ] := Left( ::CurrentLine[ ED_BUFFER ], aAction[3] - 1 ) + aAction[4] + SubStr( ::CurrentLine[ ED_BUFFER ], aAction[3] )

                  IF aActions == ::aUnDo
                     ::CurrentLine[ ED_MODIFICATIONS ]--
                  ELSE
                     ::CurrentLine[ ED_MODIFICATIONS ]++
                  ENDIF

                  ::nLongestLine := Max( ::nLongestLine, Len( ::CurrentLine[ ED_BUFFER ] ) )
                  ::nColumn := aAction[3] + nLen - nPad
                  //TraceLog( ::nColumn, :nColumn, aAction[3] )
                  EXIT

               CASE ED_DELETELINE
                  ::nPreferedColumn := 0

                  //TraceLog( "DELETELINE" )

                  IF aAction[2] >= ::nLines
                     aAdd( aReverse, { ED_ADDLINE, aAction[2], ::CurrentLine[ ED_BUFFER ] } )
                  ELSE
                     aAdd( aReverse, { ED_INSERTLINE, aAction[2], ::CurrentLine[ ED_BUFFER ] } )
                  ENDIF
                  ::DeleteLine()
                  EXIT

               CASE ED_BREAKLINE
                  //TraceLog( "BREAKLINE", ::nLine, aAction[3] )

                  ::nPreferedColumn := 0

                  IF aAction[3] == 1
                     aAdd( aActions, { ED_GOTO, aAction[2] + 1, 1 } )
                     nAction--

                     aAdd( aActions, { ED_INSERTLINE, aAction[2], "" } )
                     ::nActions++
                     EXIT
                  ENDIF

                  aAdd( aReverse, { ED_GOTO, ::nLine, ::nColumn } )

                  IF aAction[3] <= Len( ::CurrentLine[ ED_BUFFER ] )
                     sSelected := SubStr( ::CurrentLine[ ED_BUFFER ], aAction[3] )

                     aAdd( aReverse, { ED_INSERT, aAction[2], aAction[3], sSelected } )
                     nExtraActions++

                     ::CurrentLine[ ED_BUFFER ] := Left( ::CurrentLine[ ED_BUFFER ], aAction[3] - 1 )

                     IF aActions == ::aUnDo
                        ::CurrentLine[ ED_MODIFICATIONS ]--
                     ELSE
                        ::CurrentLine[ ED_MODIFICATIONS ]++
                     ENDIF
                  ELSE
                     sSelected := ""
                  ENDIF

                  //Alert( sSelected )

                  aAdd( aActions, { ED_GOTO, aAction[2] + 1, 1 } )
                  ::nActions++

                  IF ::nLine == ::nLines
                     aAdd( aActions, { ED_ADDLINE, aAction[2] + 1, sSelected } )
                  ELSE
                     aAdd( aActions, { ED_INSERTLINE, aAction[2] + 1, sSelected } )
                  ENDIF
                  ::nActions++
                  EXIT

               CASE ED_INSERTLINE
                  ::nPreferedColumn := 0

                  //TraceLog( "INSERTLINE", ::nLine, aAction[3] )

                  aAdd( aReverse, { ED_DELETELINE, aAction[2] } )
                  ::InsertLine( { aAction[3], NIL, NIL, 0 }, .T. )

                  IF ! aActions == ::aUnDo
                     ::CurrentLine[ ED_MODIFICATIONS ]++
                  ENDIF
                  EXIT

               CASE ED_ADDLINE
                  ::nPreferedColumn := 0

                  //TraceLog( "ADDLINE", ::nLine, aAction[3] )

                  IF Len( ::LastLine ) == ED_BASE_LEN
                     aAdd( aReverse, { ED_DELETELINE, ::nLines + 1 } )
                     ::AddLine( { aAction[3], NIL, NIL, 0 }, .T. )
                     ::Down(1)
                  ELSE
                     IF Len( aActions ) > 0 .AND. aActions[-1][1] == ED_GOTO
                        aActions[-1][2] += ::LastLine[ ED_COLLAPSED_LINES ] - 1
                     ENDIF

                     aAction[2] += ::LastLine[ ED_COLLAPSED_LINES ]
                     aAdd( aActions, aAction )
                     nAction--

                     aAdd( aActions, { ED_EXPAND, ::nLines } )
                     ::nActions++
                  ENDIF

                  IF aActions == ::aUnDo
                     ::CurrentLine[ ED_MODIFICATIONS ]--
                  ELSE
                     ::CurrentLine[ ED_MODIFICATIONS ]++
                  ENDIF
                  EXIT

               CASE ED_CUT
                  ::nPreferedColumn := 0

                  aAdd( aReverse, { ED_GOTO, aAction[2], aAction[3] } )

                  IF ::nLineFrom > 0
                     nLineFrom   := ::nLineFrom
                     nColumnFrom := ::nColumnFrom
                     nLineTo     := ::nLineTo
                     nColumnTo   := ::nColumnTo
                     nDirection  := ::nDirection

                     IF Len( aAction ) == 3
                        lSquare := ::lSquare
                     ELSE
                        lSquare := aAction[4]
                     ENDIF

                     ::lSquare     := .F.
                     ::nDirection  := 0
                     ::nLineFrom   := 0
                     ::nColumnFrom := 0
                     ::nLineTo     := 0
                     ::nColumnTo   := 0

                     aAdd( aReverse, { ED_SELECT, nLineFrom, nColumnFrom, nLineTo, nColumnTo, lSquare, nDirection } )
                     nExtraActions++

                     ::GoLine( nLineFrom )

                     //TraceLog( ::nLine, ::CurrentLine[ ED_BUFFER ], nLineFrom, nColumnFrom, nLineTo, nColumnTo )

                     IF nLineTo == nLineFrom
                        sSelected := SubStr( ::CurrentLine[ ED_BUFFER ], nColumnFrom, ( nColumnTo - nColumnFrom ) + 1 )
                        ::CurrentLine[ ED_BUFFER] := Left( ::CurrentLine[ ED_BUFFER ], nColumnFrom - 1 ) + SubStr( ::CurrentLine[ ED_BUFFER ], nColumnTo + 1 )

                        IF aActions == ::aUnDo
                           ::CurrentLine[ ED_MODIFICATIONS ]--
                        ELSE
                           ::CurrentLine[ ED_MODIFICATIONS ]++
                        ENDIF
                     ELSE
                        nDeferDisplay++
                        :nDeferDisplay++

                        IF lSquare
                           sSelected := SubStr( ::CurrentLine[ ED_BUFFER ], nColumnFrom, nColumnTo - nColumnFrom + 1 ) + EOL
                           ::CurrentLine[ ED_BUFFER] := Left( ::CurrentLine[ ED_BUFFER ], nColumnFrom - 1 ) + SubStr( ::CurrentLine[ ED_BUFFER ], nColumnTo + 1 )

                           IF aActions == ::aUnDo
                              ::CurrentLine[ ED_MODIFICATIONS ]--
                           ELSE
                              ::CurrentLine[ ED_MODIFICATIONS ]++
                           ENDIF

                           :Display( NIL, ::nLine - :nBaseLine - 1 )
                        ELSE
                           IF nColumnFrom > 1
                              sSelected := SubStr( ::CurrentLine[ ED_BUFFER ], nColumnFrom )
                              ::CurrentLine[ ED_BUFFER] := Left( ::CurrentLine[ ED_BUFFER ], nColumnFrom - 1 )
                           ELSE
                              sSelected := ::CurrentLine[ ED_BUFFER ]
                              ::CurrentLine[ ED_BUFFER] := ""
                           ENDIF
                        ENDIF

                        //::GoLine( nLineFrom + 1 )
                        ::Down( 1 )
                        //TraceLog( ::CurrentLine[ ED_BUFFER ], ::nLine, ::nLines )

                        IF nLineTo > ::nLines
                           nLineTo := ::nLines
                           nColumnTo := Len( ::LastLine[ ED_BUFFER ] )
                        ENDIF

                        WHILE ::nLine < nLineTo
                           IF lSquare
                              sSelected += SubStr( ::CurrentLine[ ED_BUFFER ], nColumnFrom, nColumnTo - nColumnFrom + 1 ) + EOL
                              ::CurrentLine[ ED_BUFFER] := Left( ::CurrentLine[ ED_BUFFER ], nColumnFrom - 1 ) + SubStr( ::CurrentLine[ ED_BUFFER ], nColumnTo + 1 )

                              IF aActions == ::aUnDo
                                 ::CurrentLine[ ED_MODIFICATIONS ]--
                              ELSE
                                 ::CurrentLine[ ED_MODIFICATIONS ]++
                              ENDIF

                              IF aActions == ::aUnDo
                                 ::CurrentLine[ ED_MODIFICATIONS ]--
                              ELSE
                                 ::CurrentLine[ ED_MODIFICATIONS ]++
                              ENDIF

                              :Display( NIL, ::nLine - :nBaseLine - 1 )
                              ::Down(1)
                           ELSE
                              sSelected += EOL + ::CurrentLine[ ED_BUFFER ]
                              ::DeleteLine()
                              nLineTo--
                           ENDIF

                           //TraceLog( ::nLine, ::nLines, nLineTo, ::CurrentLine[ ED_BUFFER ] )
                        END

                        IF lSquare
                           sSelected += SubStr( ::CurrentLine[ ED_BUFFER ], nColumnFrom, nColumnTo - nColumnFrom + 1 ) + EOL
                           ::CurrentLine[ ED_BUFFER] := Left( ::CurrentLine[ ED_BUFFER ], nColumnFrom - 1 ) + SubStr( ::CurrentLine[ ED_BUFFER ], nColumnTo + 1 )

                           IF aActions == ::aUnDo
                              ::CurrentLine[ ED_MODIFICATIONS ]--
                           ELSE
                              ::CurrentLine[ ED_MODIFICATIONS ]++
                           ENDIF

                           :Display( NIL, ::nLine - :nBaseLine - 1 )
                        ELSE
                           sSelected += EOL

                           IF nColumnTo > 0
                              sSelected += Left( ::CurrentLine[ ED_BUFFER ], nColumnTo )

                              IF nColumnTo < Len( ::CurrentLine[ ED_BUFFER ] )
                                 ::CurrentLine[ ED_BUFFER ] := SubStr( ::CurrentLine[ ED_BUFFER ], nColumnTo + 1 )

                                 IF aActions == ::aUnDo
                                    ::CurrentLine[ ED_MODIFICATIONS ]--
                                 ELSE
                                    ::CurrentLine[ ED_MODIFICATIONS ]++
                                 ENDIF
                              ELSE
                                 ::CurrentLine[ ED_BUFFER ] := ""
                              ENDIF
                           ENDIF

                           ::CurrentLine[ ED_PREVLINE][ ED_BUFFER ] += ::CurrentLine[ ED_BUFFER ]

                           IF nColumnTo > 0
                              IF aActions == ::aUnDo
                                 ::CurrentLine[ ED_PREVLINE][ ED_MODIFICATIONS ]--
                              ELSE
                                 ::CurrentLine[ ED_PREVLINE][ ED_MODIFICATIONS ]++
                              ENDIF
                           ENDIF

                           ::DeleteLine()
                        ENDIF
                        //TraceLog( sSelected )

                        //TraceLog( nLineFrom, ::nLines )
                        ::GoLine( nLineFrom )
                     ENDIF

                     //TraceLog( nDirection, nColumnFrom, nColumnTo )

                     WITH OBJECT ::oDisplay
                        :Display( NIL, nLineFrom - :nBaseLine - 1 )
                        :GoColumn( Max( nColumnFrom - :nBaseColumn - 1, 0 ) )
                     END

                     //aAdd( aReverse, { ED_GOTO, nLineFrom, nColumnFrom } )
                     aAdd( aReverse, { ED_PASTE, nLineFrom, nColumnFrom, sSelected, lSquare } )
                     nExtraActions++
                  ENDIF

                  #ifndef VXH
                     HB_GCAll()
                  #endif
                  EXIT

               CASE ED_PASTE
                  IF ::nLineFrom > 0
                     //TraceLog( ::nLineFrom )
                     aAction[2] := ::nLineFrom
                     aAction[3] := ::nColumnFrom

                     aAdd( aActions, aAction )
                     nAction--

                     aAdd( aActions, { ED_CUT, ::nLineFrom, ::nColumnFrom } )
                     ::nActions++
                     EXIT
                  ENDIF

                  ::nPreferedColumn := 0

                  ::nColumn := Max( aAction[3], 1 )
                  sSelected := aAction[4]
                  nLines := 0
                  aBatch := {}

                  IF Len( aAction ) == 4
                     IF sSelected == ::cClipBoard
                        lSquare := ::lClipSquare
                     ELSE
                        lSquare := .F.
                     ENDIF
                  ELSE
                     lSquare := aAction[5]
                  ENDIF

                  //TraceLog( lSquare, sSelected )

                  IF (! lSquare) .AND. Right( sSelected, Len( EOL ) ) == EOL
                     lOneMore := .T.
                  ELSE
                     lOneMore := .F.
                  ENDIF

                  // Text to the right will first be pushed down before inserting pasted text
                  IF (! lSquare ) .AND. EOL IN sSelected //.AND. ( aAction[3] <= Len( ::CurrentLine[ ED_BUFFER ] ) )
                     aAction[2]++
                  ENDIF

                  WHILE NextLine( @sSelected, @sLine )
                     nLines++
                     sLine := StrTran( sLine, Chr(9), "   " )
                     sLine := StrTran( sLine, Chr(13), "" )

                     //TraceLog( nLines, sLine, Asc( sLine[-1] ) )

                     IF lSquare
                        aAdd( aBatch, { ED_INSERT, aAction[2]++, aAction[3], sLine } )
                     ELSE
                        aAdd( aBatch, { ED_INSERTLINE, aAction[2], sLine } )
                     ENDIF
                  END

                  IF lOneMore
                     nLines++
                     aAdd( aBatch, { ED_INSERTLINE, aAction[2] + 1, "" } )
                  ENDIF

                  IF nLines >= 1 .AND. (! lSquare )
                     aBatch[1][1] := ED_INSERT
                     aBatch[1][2] := aAction[2]
                     aIns( aBatch[1], 3, ::nColumn, .T. )

                     //TraceLog( aBatch[1][1], aBatch[1][2], aBatch[1][3] )

                     IF nLines > 1
                        aBatch[-1][1] := ED_INSERT
                        aIns( aBatch[-1], 3, 1, .T. )

                        // Move 1 to top
                        aBatch[1][2] := ::nLine
                        aAdd( aBatch, aBatch[1] )

                        //aDel( aBatch, 1, .T. )
                        //TraceLog( ::nLine, nLines )
                        aBatch[1] := { ED_GOTO, ::nLine + nLines - 1, IIF( lOneMore, 1, Len( sLine ) + 1 ) }

                        IF ::nColumn <= 1
                           aAdd( aBatch, { ED_INSERTLINE, ::nLine, "" } )
                        ELSEIF ::nColumn <= Len( ::CurrentLine[ ED_BUFFER ] )
                           aAdd( aBatch, { ED_BREAKLINE, ::nLine, ::nColumn } )
                        ENDIF

                        nDeferDisplay++
                        :nDeferDisplay++
                     ENDIF
                  ENDIF

                  // TODO:UGLY - Fix above!!!
                  FOR nLine := 1 TO Len( aBatch )
                     aAction := aBatch[ nLine ]

                     IF aAction[1] == ED_INSERT
                        IF aAction[4] == ""
                           aDel( aBatch, nLine, .T. )
                           nLine--
                        ELSEIF nLine < Len( aBatch ) .AND. aBatch[ nLine + 1 ][1] == ED_INSERTLINE
                           IF aBatch[ nLine + 1 ][2] == aAction[2] .AND. aBatch[ nLine + 1 ][3] == ""
                              aBatch[ nLine + 1 ][3] := aAction[4]
                              aDel( aBatch, nLine, .T. )
                           ENDIF
                        ENDIF
                     ENDIF
                  NEXT

                  // Merge remaining actions
                  aEval( aBatch, {|_1| aAdd( aActions, _1 ) } )

                  // Adjust
                  ::nActions += Len( aBatch ) - 1
                  nAction--
                  EXIT

               CASE ED_SELECT
                  ::nPreferedColumn := 0

                  aAdd( aReverse, { ED_UNSELECT, aAction[2], aAction[3], aAction[4], aAction[5], aAction[6], aAction[7] } )
                  //TraceLog( aAction[2], aAction[3], aAction[4], aAction[5] )

                  IF ::nLineFrom > 0
                     :ShowSelection()
                  ENDIF

                  ::nDirection  := 0
                  ::nLineFrom   := 0
                  ::nColumnFrom := 0
                  ::nLineTo     := 0
                  ::nColumnTo   := 0

                  IF aAction[4] > :nBaseLine + :nRows
                     ::GoLine( aAction[4] )
                  ELSE
                     :GoRow( aAction[4] - :nBaseLine - 1 )
                  ENDIF
                  ::nColumn     := Max( aAction[5] + 1, 1 )

                  ::nLineFrom   := aAction[2]
                  ::nColumnFrom := aAction[3]
                  ::nLineTo     := aAction[4]
                  ::nColumnTo   := aAction[5]

                  ::lSquare     := aAction[6]
                  ::nDirection  := aAction[7]

                  :ShowSelection()
                  EXIT

               CASE ED_UNSELECT
                  ::nPreferedColumn := 0

                  aAdd( aReverse, { ED_SELECT, aAction[2], aAction[3], aAction[4], aAction[5], aAction[6], aAction[7] } )

                  // Normalize.
                  :ShowSelection()

                  ::nLastLine   := 0
                  ::nLastColumn := 0

                  ::nDirection  := 0
                  ::nLineFrom   := 0
                  ::nColumnFrom := 0
                  ::nLineTo     := 0
                  ::nColumnTo   := 0
                  ::lSquare     := .F.
                  EXIT

               CASE ED_COLLAPSE
                  ::nPreferedColumn := 0

                  nFunc := aScan( ::aFunctions, {|_1| _1[3] == ::CurrentLine } )
                  IF nFunc > 0
                     aAdd( aReverse, { ED_EXPAND, aAction[2] } )
                     ::Collapse( ::CurrentLine )
                  ENDIF
                  EXIT

               CASE ED_EXPAND
                  ::nPreferedColumn := 0

                  nFunc := aScan( ::aFunctions, {|_1| _1[3] == ::CurrentLine } )
                  IF nFunc > 0
                     aAdd( aReverse, { ED_COLLAPSE, aAction[2] } )
                     ::Expand( ::CurrentLine )
                  ENDIF
                  EXIT

               CASE ED_COLLAPSEALL
                  ::nPreferedColumn := 0

                  aAdd( aReverse, { ED_EXPANDALL, aAction[2] } )
                  ::CollapseAll()
                  EXIT

               CASE ED_EXPANDALL
                  ::nPreferedColumn := 0

                  aAdd( aReverse, { ED_COLLAPSEALL, aAction[2] } )
                  ::ExpandAll()
                  EXIT

               CASE ED_TOGGLEINSERT
                  aAdd( aReverse, aAction )

                  IF ::lInsert
                     ::lInsert := .F.

                     #ifdef WIN
                        DestroyCaret()
                        IF CreateCaret( ::oDisplay:hWnd, NIL, ::oDisplay:TextMetric:tmAveCharWidth, ::oDisplay:TextMetric:tmHeight )
                           //SetPos( ::oDisplay:nRow, ::oDisplay:nColumn )

                           IF ::oDisplay:nRow >= 0 .AND. ::oDisplay:nColumn >= 0
                              ShowCaret( ::oDisplay:hWnd )
                           ENDIF
                        ENDIF
                     #endif
                  ELSE
                     ::lInsert := .T.

                     #ifdef WIN
                        DestroyCaret()
                        IF CreateCaret( ::oDisplay:hWnd, NIL, 2, ::oDisplay:TextMetric:tmHeight )
                           //SetPos( ::oDisplay:nRow, ::oDisplay:nColumn )

                           IF ::oDisplay:nRow >= 0 .AND. ::oDisplay:nColumn >= 0
                              ShowCaret( ::oDisplay:hWnd )
                           ENDIF
                        ENDIF
                     #endif
                  ENDIF
            END

            IF nAction == ::nActions
               ::nColumn := Max( ::nColumn, 1 )

               IF ::nColumn <= :nBaseColumn
                  IF ::nColumn < :nColumns
                     :nBaseColumn := 0
                     :nColumn := ::nColumn - 1
                  ELSE
                     :nBaseColumn := ::nColumn - :nColumns
                     :nColumn := :nColumns - 1
                     :nBaseColumn := ::nColumn
                  ENDIF
                  :Display()
               ELSEIF ::nColumn > :nColumns
                  IF :nBaseColumn < ::nColumn - :nColumns
                     :nBaseColumn := ::nColumn - :nColumns
                     :nColumn := :nColumns - 1
                     :Display()
                  ELSE
                     :nColumn := ::nColumn - :nBaseColumn - 1
                  ENDIF
               ELSE
                  :nColumn := ::nColumn - :nBaseColumn - 1
               ENDIF
            ENDIF

            //TraceLog( nAction, aAction[1], :nRow, ::nLine )
            #ifdef WIN
               :Display( NIL, :nRow )
            #else
               :Display( :nRow )
            #endif
            //TraceLog( nAction, aAction[1], :nRow, ::nLine )

            //TraceLog( ::nColumn, :nColumn )
            IF nAction < ::nActions
               aAction := aActions[ -1 ]
               aSize( aActions, Len( aActions ) - 1 )
            ENDIF
         NEXT

         IF ::nActions > 1 .OR. nExtraActions != 0
            aReverse[-1][1] := aReverse[-1][1] | ( ::nActions + nExtraActions << 8 )
         ENDIF

         // TODO OPTIMIZE!!!
         IF nDeferDisplay > 0
            :nDeferDisplay -= nDeferDisplay
            ASSERT( :nDeferDisplay >= 0 )
            :Display()
         ELSE
            :Status()
         ENDIF

         //TraceLog( ::nActions, ::aUnDo, ::nLastUndo, aAction[1] )

         IF aScan( ::aUnDo, {|aAction| /*TraceLog( aAction[1]  & 0xFF ),*/ aAction[1] & 0xFF < ED_NOIMPACT }, ::nLastUndo + 1 ) > 0
            ::lModified := .T.
            //TraceLog( "YES" )
         ELSEIF Len( ::aUndo ) < ::nLastUndo .AND. aScan( ::aRedo, {|aAction| aAction[1] & 0xFF < ED_NOIMPACT }, ::nLastReDo + 1 ) > 0
            ::lModified := .T.
            //TraceLog( "YES" )
         ELSE
            ::lModified := .F.
            //TraceLog( "NO" )
         ENDIF

         #ifndef VXH
            #ifdef WIN
               IF ::hFileItem != NIL .AND. :hFilesTree != NIL
                   tvi := (struct TVITEM)

                   tvi:hItem     := ::hFileItem
                   tvi:mask      := TVIF_STATE
                   tvi:stateMask := TVIS_BOLD
                   tvi:state     := IIF( ::lModified, TVIS_BOLD, 0 )

                   SendMessage( :hFilesTree, TVM_SETITEM, 0, @tvi )
               ENDIF
            #endif
         #endif
      END

   CATCH oError
      //Alert( oError:Operation )
      TraceLog( oError:Operation, oError:Description, oError:ProcName, oError:ProcLine, ValToPrg( oError:Args ), ValToPrg( aAction ), nAction )
   END

   #ifdef VXH
      Application:Project:EditReset(0)
   #endif

RETURN lImpacted

#ifdef VXH
   METHOD NotifyVXH( lMod )
      LOCAL nEditorID, cText

      IF lMod != ::lModified
         IF lMod
            Application:Project:Modified := .T.
         ENDIF

         IF Len( s_aEditors ) > 0
            nEditorID := aScan( s_aEditors, Self, , , .T. )
            cText     := Application:SourceTabs:GetItemText( nEditorID )
            cText     := ALLTRIM( STRTRAN( cText, "*" ) )
            Application:SourceTabs:SetItemText( nEditorID, IIF( lMod, " ", "" ) + cText + IIF( lMod, " * ", "" ), lMod )
            Application:Props[ "EditUndoItem" ]:Enabled := Application:Props[ "EditUndoBttn" ]:Enabled := Len( ::aUnDo ) > 0
            Application:Props[ "EditRedoItem" ]:Enabled := Application:Props[ "EditRedoBttn" ]:Enabled := Len( ::aReDo ) > 0
         ENDIF
      ENDIF

   RETURN Self
#endif

METHOD SelectedText() CLASS Editor

   LOCAL nLineFrom, nColumnFrom, nLineTo, nColumnTo
   LOCAL Line, nLine, sText

   IF ::nLineFrom > 0
      nLineFrom   := ::nLineFrom
      nColumnFrom := ::nColumnFrom
      nLineTo     := ::nLineTo
      nColumnTo   := ::nColumnTo

      Line := ::GetLine( nLineFrom )

      //TraceLog( Line[ ED_BUFFER ], nLineFrom, nColumnFrom, nLineTo, nColumnTo )

      IF nLineTo == nLineFrom
         IF ::lSquare
            sText := PadR( SubStr( Line[ ED_BUFFER ], nColumnFrom, ( nColumnTo - nColumnFrom ) + 1 ), nColumnTo - nColumnFrom + 1 )
         ELSE
            sText := SubStr( Line[ ED_BUFFER ], nColumnFrom, ( nColumnTo - nColumnFrom ) + 1 )
         ENDIF
      ELSE
         IF ::lSquare
            sText := PadR( SubStr( Line[ ED_BUFFER ], nColumnFrom, nColumnTo - nColumnFrom + 1 ), nColumnTo - nColumnFrom + 1 ) + EOL
         ELSE
            sText := SubStr( Line[ ED_BUFFER ], nColumnFrom )
         ENDIF

         nLine := nLineFrom

         Line := Line[ ED_NEXTLINE ]

         WHILE ++nLine < nLineTo
            IF ::lSquare
               sText += PadR( SubStr( Line[ ED_BUFFER ], nColumnFrom, nColumnTo - nColumnFrom + 1 ), nColumnTo - nColumnFrom + 1 ) + EOL
            ELSE
               sText += EOL + Line[ ED_BUFFER ]
            ENDIF

            Line := Line[ ED_NEXTLINE ]
         END

         IF nLineTo <= ::nLines
            IF ::lSquare
               sText += PadR( SubStr( Line[ ED_BUFFER ], nColumnFrom, nColumnTo - nColumnFrom + 1 ), nColumnTo - nColumnFrom + 1 ) + EOL
            ELSE
               sText += EOL + Left( Line[ ED_BUFFER ], nColumnTo )
            ENDIF
         ENDIF
      ENDIF

      IF ::lSquare
      ELSE
         IF nLineTo > ::nLines .OR. nColumnTo > Len( Line[ ED_BUFFER ] )
            sText += EOL
         ENDIF
      ENDIF

      //TraceLog( sText )
   ENDIF

RETURN sText

METHOD Position( nLine, nColumn ) CLASS Editor

   LOCAL Line := ::FirstLine, nLineIndex := 1, nIndex := 0, nEolLen := Len( EOL )

   WHILE nLineIndex < nLine
      nIndex += Len( Line[ ED_BUFFER ] ) + nEolLen
      Line := Line[ ED_NEXTLINE ]
      nLineIndex++
   END

RETURN nIndex + nColumn

METHOD LineColumn( nPosition ) CLASS Editor

   LOCAL Line := ::FirstLine, nLine := 1, nColumn, nIndex := 0, nEolLen := Len( EOL )

   IF nPosition == 0
      RETURN { 1, 1 }
   ENDIF

   WHILE Line != NIL
      nIndex += Len( Line[ ED_BUFFER ] ) + nEolLen

      IF nIndex >= nPosition
         EXIT
      ENDIF

      Line := Line[ ED_NEXTLINE ]
      nLine++
   END

   //TraceLog( nLine, ::nLines, nIndex, nPosition )

   IF nIndex >= nPosition
      nColumn := nPosition - ( nIndex - ( Len( Line[ ED_BUFFER ] ) + nEolLen ) )
   ELSE
      nLine   := 0
      nColumn := 0
   ENDIF

RETURN { nLine, nColumn }

METHOD SetSel( nStart, nEnd ) CLASS Editor

   LOCAL aPos := ::LineColumn( nStart )

   IF aPos[1] > 0
      ::GoLine( aPos[1] )

      WITH OBJECT ::oDisplay
         IF aPos[2] <= :nBaseColumn
            IF aPos[2] < :nColumns
               :nBaseColumn := 0
               :nColumn := aPos[2] - 1
            ELSE
               :nBaseColumn := aPos[2] - :nColumns
               :nColumn := :nColumns - 1
            ENDIF
            :Display()
         ELSEIF aPos[2] >= :nColumns
            :nBaseColumn := aPos[2] - :nColumns
            :nColumn := :nColumns - 1
            :Display()
         ELSE
            :GoColumn( aPos[2] - :nBaseColumn - 1 )
         ENDIF

         IF nEnd > nStart
            ::nDirection  := 1
            ::nLineFrom   := ::nLine
            ::nColumnFrom := ::nColumn

            aPos := ::LineColumn( nEnd )

            ::nLineTo   := aPos[1]
            ::nColumnTo := aPos[2]

            :ShowSelection()
         ENDIF
      END
   ENDIF
RETURN 0

METHOD SelectWord() CLASS Editor

   LOCAL cLine := ::CurrentLine[ ED_BUFFER ]
   LOCAL nColumn := ::nColumn
   LOCAL nLen := Len( cLine )
   LOCAL nColumnFrom

   IF nLen == 0 .OR. cLine[ nColumn ] == ' '
      RETURN 0
   ENDIF

   IF IsAlNum( cLine[ nColumn ] ) .OR. cLine[ nColumn ] == '_'
      WHILE nColumn > 0 .AND. ( IsAlNum( cLine[ nColumn ] ) .OR. cLine[ nColumn ] == '_' )
         nColumn--
      END

      nColumnFrom := nColumn + 1

      nColumn := ::nColumn + 1
      WHILE nColumn <= nLen .AND. IsAlNum( cLine[ nColumn ] )
         nColumn++
      END
   ELSE
      WHILE nColumn > 0 .AND. cLine[ nColumn ] != ' ' .AND. ( ! IsAlNum( cLine[ nColumn ] ) )
         nColumn--
      END

      nColumnFrom := nColumn + 1

      nColumn := ::nColumn + 1
      WHILE nColumn <= nLen .AND. cLine[ nColumn ] != ' ' .AND. ( ! IsAlNum( cLine[ nColumn ] ) ) .AND. cLine[ nColumn ] != '_'
         nColumn++
      END
   ENDIF

   ::Action( { { ED_SELECT, ::nLine, nColumnFrom, ::nLine, nColumn - 1, .F., 1 } }, ::aUnDo )

RETURN Self

PROCEDURE PRGAfterKey( oEditor, nKey, nCount )

   LOCAL nNewPad := 0
   LOCAL nPad    := 0
   LOCAL aActions := {}

   (nCount)

   WITH OBJECT oEditor
      IF :nLine > 1
         IF Len( :CurrentLine[ ED_PREVLINE ] ) > ED_BASE_LEN
            // Collapsed mode.
            RETURN
         ENDIF

         HB_AtX( "^ *", :CurrentLine[ ED_PREVLINE ][ ED_BUFFER ], , , @nNewPad )

         IF ! Empty( HB_AtX( "(?i)^ *(INIT +(FUNCTION|PROCEDURE)|EXIT +(FUNCTION|PROCEDURE)|STATIC +(FUNCTION|PROCEDURE)|FUNCTION|PROCEDURE|CLASS|METHOD|IF|ELSEIF|ELSE|DO|WHILE|SWITCH|FOR|BEGIN|TRY|CATCH|WITH)", :CurrentLine[ ED_PREVLINE ][ ED_BUFFER ] ) )
            nNewPad += 3
         ENDIF
      ENDIF

      IF nKey == K_RETURN
         IF :nLine > 1
            HB_AtX( "^ *", :CurrentLine[ ED_BUFFER ], , , @nPad )

            :LineHome()
            //TraceLog( :CurrentLine[ ED_BUFFER ], nNewPad, nPad, :nLine, :nColumn )

            IF nPad < nNewPad
               :Action( { { ED_PASTE, :nLine, :nColumn, Space( nNewPad - nPad ) } }, :aUndo )
            ELSEIF nPad > nNewPad
               //TraceLog( :Column, nPad, nNewPad )
               :OnKey( K_BS, nPad - nNewPad )
            ENDIF
         ENDIF
      ELSEIF nKey == ' '
         HB_AtX( "^ *", :CurrentLine[ ED_BUFFER ], , , @nPad )

         IF :nColumn == nPad + 3
            IF :CurrentLine[ ED_BUFFER][ :nColumn - 2 ] == 'I'
               aAdd( aActions, { ED_GOTO, :nLine, :nColumn + 1 } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, Space( nPad ) + "ENDIF" } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, "" } )
               aAdd( aActions, { ED_PASTE, :nLine, :nColumn - 1, "F" } )
            ELSEIF :CurrentLine[ ED_BUFFER][ :nColumn - 2 ] == 'F'
               aAdd( aActions, { ED_GOTO, :nLine, :nColumn + 2 } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, Space( nPad ) + "NEXT" } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, "" } )
               aAdd( aActions, { ED_PASTE, :nLine, :nColumn - 1, "OR" } )
            ELSEIF :CurrentLine[ ED_BUFFER][ :nColumn - 2 ] == 'W'
               aAdd( aActions, { ED_GOTO, :nLine, :nColumn + 4 } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, Space( nPad ) + "ENDDO" } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, "" } )
               aAdd( aActions, { ED_PASTE, :nLine, :nColumn - 1, "HILE" } )
            ELSEIF :CurrentLine[ ED_BUFFER][ :nColumn - 2 ] == 'S'
               aAdd( aActions, { ED_GOTO, :nLine, :nColumn + 5 } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, Space( nPad + 3 ) + "CASE " } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, Space( nPad ) + "END" } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, "" } )
               aAdd( aActions, { ED_PASTE, :nLine, :nColumn - 1, "WITCH" } )
            ELSEIF :CurrentLine[ ED_BUFFER][ :nColumn - 2 ] == 'D'
               aAdd( aActions, { ED_GOTO, :nLine + 1, nPad + 3 + 6 } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, Space( nPad + 3 ) + "CASE " } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, Space( nPad ) + "ENDCASE" } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, "" } )
               aAdd( aActions, { ED_PASTE, :nLine, :nColumn - 1, "O CASE" } )
            ELSEIF :CurrentLine[ ED_BUFFER][ :nColumn - 2 ] == 'B'
               aAdd( aActions, { ED_GOTO, :nLine + 1, nPad + 3 + 15 } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, Space( nPad + 3 ) + "RECOVER USING " } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, Space( nPad ) + "ENDSEQUENCE" } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, "" } )
               aAdd( aActions, { ED_PASTE, :nLine, :nColumn - 1, "EGIN SEQUENCE" } )
            ELSEIF :CurrentLine[ ED_BUFFER][ :nColumn - 2 ] == 'T'
               aAdd( aActions, { ED_GOTO, :nLine + 1, nPad + 3 + 7 } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, Space( nPad + 3 ) + "CATCH " } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, Space( nPad ) + "END" } )
               aAdd( aActions, { ED_INSERTLINE, :nLine + 1, "" } )
               aAdd( aActions, { ED_PASTE, :nLine, :nColumn - 1, "RY" } )
            ENDIF
         ENDIF

         IF ! Empty( aActions )
            aActions[-1][1] += 256 * Len( aActions )
            :Action( aActions, :aUnDo )
         ENDIF
      /*
      ELSEIF :nColumn == 2 .AND. Len( :CurrentLine[ ED_BUFFER ] ) == 1 .AND. ( ( nKey >= '0' .AND. nKey <= '9' ) .OR. ( nKey >= 'A' .OR. nKey <= 'Z' ) .OR. ( nKey >= 'a' .OR. nKey <= 'z' ) .OR. nKey == '_' )
         :LineHome()
         :Action( { { ED_PASTE, :nLine, :nColumn, Space( nNewPad ) } }, ::aUndo )
         :LineEnd()
      */
      ENDIF
   END

RETURN

FUNCTION xEdit_GetEditors()
RETURN s_aEditors

#ifdef DLL
   #pragma BEGINDUMP
      #define HB_NO_DEFAULT_API_MACROS
      #define HB_NO_DEFAULT_STACK_MACROS
   #pragma ENDDUMP
#endif

#pragma BEGINDUMP
   #include <ctype.h>

   #include "hbapi.h"
   #include "hbapiitm.h"
   #include "hbfast.h"
   #include "hbstack.h"
   #include "hbvm.h"

   static BOOL s_bArrayPrefix = FALSE;

   //----------------------------------------------------------------------------//
   HB_FUNC( NEXTTOKEN )
   {
      PHB_ITEM pLine       = hb_param( 1, HB_IT_STRING );
      PHB_ITEM pDontRecord = hb_param( 2, HB_IT_LOGICAL );
      char *sLine, *pTmp;
      char sReturn[2048];
      char s2[3];
      BOOL lDontRecord;
      size_t Counter, nLen;

      #ifdef DEBUG_TOKEN
         char sProc[64];
         USHORT uiLine;
      #endif

      if( pLine == NULL || pLine->item.asString.length == 0 )
      {
         hb_ret();
         return;
      }

      sLine = pLine->item.asString.value;
      nLen = pLine->item.asString.length;

      #ifdef DEBUG_TOKEN
         hb_procinfo( 1, (char *) &sProc, &uiLine );
         printf( "%s[%i] Processing: '%s'\n", (char *) sProc, uiLine, sLine );
      #endif

      if( pDontRecord == NULL )
      {
         lDontRecord = FALSE;
      }
      else
      {
         lDontRecord = pDontRecord->item.asLogical.value;
      }

      // *** To be removed after final testing !!!
      while( sLine[0] == ' ' )
      {
         sLine++; nLen--;
      }

      sReturn[0] = '\0';
      s2[2]      = '\0';

      if( nLen >= 2 )
      {
         s2[0] = sLine[0];
         s2[1] = sLine[1];

         if( strstr( "++\\--\\->\\:=\\==\\!=\\<>\\>=\\<=\\+=\\-=\\*=\\^=\\**\\/=\\%=", (char*) s2 ) )
         {
            sReturn[0] = s2[0];
            sReturn[1] = s2[1];
            sReturn[2] = '\0';

            goto Done;
         }
         else if( s2[0] == '/' && s2[1] == '*' )
         {
            pTmp = strstr( sLine + 2, "*/" );
            if( pTmp == NULL )
            {
               strcpy( sReturn, sLine );
            }
            else
            {
               strncpy( sReturn, sLine, ( pTmp - sLine ) + 2 );
               sReturn[( pTmp - sLine ) + 2] = '\0';
            }

            goto Done;
         }
         else if( s2[0] == '/' && s2[1] == '/' )
         {
            strcpy( sReturn, sLine );
            goto Done;
         }
         else if( s2[0] == '&' && s2[1] == '&' )
         {
            strcpy( sReturn, sLine );
            goto Done;
         }

         if( nLen >= 3 )
         {
            if( sLine[0] == 'E' && sLine[1] == '"' )
            {
               pTmp = sLine + 2;

               do
               {
                  pTmp = strchr( pTmp, '"' );

                  if( pTmp == NULL )
                  {
                     sReturn[0] = sLine[0];
                     sReturn[1] = '\0';
                     break;
                  }
                  else
                  {
                     if( pTmp[-1] != '\\' || ( pTmp[-2] == '\\' && pTmp[-1] == '\\' ) )
                     {
                        strncpy( (char *) sReturn, sLine, ( pTmp - sLine ) + 1 );
                        sReturn[ pTmp - sLine + 1 ] = '\0';
                        break;
                     }

                     pTmp++; // skip current "
                  }
               }
               while( TRUE );

               goto Done;
            }
         }
      }

      if( isalpha( sLine[0] ) || sLine[0] == '_' )
      {
         sReturn[0] = sLine[0];
         Counter = 1;

         // Why did I have the '\\' is NOT clear - document if and when reinstating!!!
         // Possibly for file name support.
         while( isalnum( sLine[Counter] ) || sLine[Counter] == '_' || sLine[Counter] == '\\' )
         {
            sReturn[Counter] = sLine[Counter];
            Counter++;
         }

         sReturn[Counter] = '\0';
         goto Done;
      }
      else if( isdigit( sLine[0] ) )
      {
         sReturn[0] = sLine[0];
         Counter = 1;
         while( isdigit( sLine[Counter] ) || sLine[Counter] == '\\' )
         {
            sReturn[Counter] = sLine[Counter];
            Counter++;
         }

         // Consume the point (and subsequent digits) only if digits follow...
         if( sLine[Counter] == '.' && isdigit( sLine[Counter + 1] ) )
         {
            sReturn[Counter] = '.';
            Counter++;
            sReturn[Counter] = sLine[Counter];
            Counter++;
            while( isdigit( sLine[Counter] ) || sLine[Counter] == '\\' )
            {
               sReturn[Counter] = sLine[Counter];
               Counter++;
            }
         }

         // Either way we are done.
         sReturn[Counter] = '\0';
         goto Done;
      }
      else if( sLine[0] == '.' && isdigit( sLine[1] ) )
      {
         sReturn[0] = '.';
         sReturn[1] = sLine[1];
         Counter = 2;
         while( isdigit( sLine[Counter] ) )
         {
            sReturn[Counter] = sLine[Counter];
            Counter++;
         }

         sReturn[Counter] = '\0';
         goto Done;
      }
      else if( sLine[0] == '.' )
      {
         if( nLen >= 5 && sLine[4] == '.' )
         {
            if( toupper( sLine[1] ) == 'A' && toupper( sLine[2] ) == 'N' && toupper( sLine[3] ) == 'D' )
            {
               sReturn[0] = '.';
               sReturn[1] = sLine[1];
               sReturn[2] = sLine[2];
               sReturn[3] = sLine[3];
               sReturn[4] = '.';
               sReturn[5] = '\0';

               goto Done;
            }
            else if( toupper( sLine[1] ) == 'N' && toupper( sLine[2] ) == 'O' && toupper( sLine[3] ) == 'T' )
            {
               sReturn[0] = '.';
               sReturn[1] = sLine[1];
               sReturn[2] = sLine[2];
               sReturn[3] = sLine[3];
               sReturn[4] = '.';
               sReturn[5] = '\0';

               goto Done;
            }
         }

         if( nLen >= 4 && sLine[3] == '.' && toupper( sLine[1] ) == 'O' && toupper( sLine[2] ) == 'R' )
         {
            sReturn[0] = '.';
            sReturn[1] = sLine[1];
            sReturn[2] = sLine[2];
            sReturn[3] = '.';
            sReturn[4] = '\0';

            goto Done;
         }

         if( nLen >= 3 && sLine[2] == '.' )
         {
            if( toupper( sLine[1] ) == 'T' )
            {
               sReturn[0] = '.';
               sReturn[1] = sLine[1];
               sReturn[2] = '.';
               sReturn[3] = '\0';

               goto Done;
            }
            else if( toupper( sLine[1] ) == 'F' )
            {
               sReturn[0] = '.';
               sReturn[1] = sLine[1];
               sReturn[2] = '.';
               sReturn[3] = '\0';

               goto Done;
            }
         }

         sReturn[0] = '.';
         sReturn[1] = '\0';

         goto Done;
      }
      else if( sLine[0] == '"' )
      {
         pTmp = strchr( sLine + 1, '"' );
         if( pTmp == NULL )
         {
            sReturn[0] = '"';
            sReturn[1] = '\0';
         }
         else
         {
            strncpy( sReturn, sLine, ( pTmp - sLine ) + 1 );
            sReturn[( pTmp - sLine ) + 1] = '\0';
         }

         goto Done;
      }
      else if( sLine[0] == '\'' )
      {
         pTmp = strchr( sLine + 1, '\'' );
         if( pTmp == NULL )
         {
            sReturn[0] = '\'';
            sReturn[1] = '\0';
         }
         else
         {
            strncpy( sReturn, sLine, ( pTmp - sLine ) + 1 );
            sReturn[( pTmp - sLine ) + 1] = '\0';
         }

         goto Done;
      }
      else if( sLine[0] == '[' )
      {
         if( s_bArrayPrefix )
         {
            sReturn[0] = '[';
            sReturn[1] = '\0';
         }
         else
         {
            pTmp = strchr( sLine + 1, ']' );
            if( pTmp == NULL )
            {
               sReturn[0] = '[';
               sReturn[1] = '\0';
            }
            else
            {
               strncpy( sReturn, sLine, ( pTmp - sLine ) + 1 );
               sReturn[( pTmp - sLine ) + 1] = '\0';
            }
         }

         goto Done;
      }
      else if( sLine[0] == '\\' )
      {
         sReturn[0] = '\\';
         sReturn[1] = sLine[1];
         sReturn[2] = '\0';

         goto Done;
      }
      else if ( strchr( "+-*/:=^!&()[]{}@,|<>#%?$~", sLine[0] ) )
      {
         sReturn[0] = sLine[0];
         sReturn[1] = '\0';

         goto Done;
      }
      else
      {
         // Todo Generic Error.
         //printf( "\nUnexpected case: %s\n", sLine );
         //getchar();
         sReturn[0] = sLine[0];
         sReturn[1] = '\0';
      }

    Done:

      sLine += ( nLen = strlen( sReturn ) );

      if( ! lDontRecord )
      {
         if( sReturn[0] == '.' && nLen > 1 && sReturn[nLen - 1] == '.' )
         {
            s_bArrayPrefix = FALSE;
         }
         else
         {
            s_bArrayPrefix = ( isalnum( sReturn[0] ) || strchr( "])}._", sReturn[0] ) );

            if( nLen < 7 && toupper( sReturn[0] ) == 'R' && toupper( sReturn[1] ) == 'E' &&
                toupper( sReturn[2] ) == 'T' && toupper( sReturn[3] ) == 'U'  )
            {
               if( sReturn[4] == '\0' )
               {
                  s_bArrayPrefix = FALSE;
               }
               else if( toupper( sReturn[4] ) == 'R' )
               {
                  if( sReturn[5] == '\0' )
                  {
                     s_bArrayPrefix = FALSE;
                  }
                  else if( toupper( sReturn[5] ) == 'N' && sReturn[6] == '\0' )
                  {
                     s_bArrayPrefix = FALSE;
                  }
               }
            }
         }
      }

      while( sLine[0] == ' ' )
      {
         sReturn[nLen] = sLine[0];
         sLine++; nLen++;
      }
      sReturn[nLen] = '\0';

      if( ISBYREF( 1 ) )
      {
         if( sLine[0] == '\0' )
         {
            hb_itemPutC( pLine, NULL );
         }
         else
         {
            hb_itemPutCPtr( pLine, hb_strdup( sLine ), strlen( sLine ) );
         }
         //printf( "\nToken: '%s' value: '%s'\n", sReturn, pLine->item.asString.value );
      }
      else
      {
         //printf( "\nToken: '%s' ***value: '%s'\n", sReturn, pLine->item.asString.value );
      }

      //#define DEBUG_TOKEN
      #ifdef DEBUG_TOKEN
         TraceLog( NULL, "Token: '%s'\n", sReturn );
      #endif

      hb_retclen( sReturn, nLen );
   }

   HB_FUNC_STATIC( NEXTLINE )
   {
      PHB_ITEM pBuffer = hb_param( 1, HB_IT_BYREF );
      PHB_ITEM pLine = hb_param( 2, HB_IT_BYREF );
      unsigned long ulPos = 0;

      if( pBuffer && pLine && HB_IS_STRING( pBuffer ) && pBuffer->item.asString.length )
      {
         //TraceLog( NULL, "Scan: %i >%s<\n", pBuffer->item.asString.length, pBuffer->item.asString.value );

         hb_retl( TRUE );

         while( ulPos < pBuffer->item.asString.length && ( pBuffer->item.asString.value[ ulPos ] != 10 ) )
         {
            ulPos++;
         }

         //TraceLog( NULL, "Len: %i Pos: %i >%.*s<\n", pBuffer->item.asString.length, ulPos, ulPos + 1, pBuffer->item.asString.value );

         if( ulPos == pBuffer->item.asString.length )
         {
            //TraceLog( NULL, "ALL\n" );
            if( pBuffer->item.asString.value[ ulPos - 1 ] == 13 )
            {
               hb_itemPutCL( pLine, pBuffer->item.asString.value, ulPos - 1 );
            }
            else
            {
               hb_itemForwardValue( pLine, pBuffer );
            }

            hb_storc( NULL, 1 );
            return;
         }
         else if( ulPos )
         {
            if( pBuffer->item.asString.value[ ulPos - 1 ] == 13 )
            {
               hb_itemPutCL( pLine, pBuffer->item.asString.value, ulPos - 1 );
            }
            else
            {
               hb_itemPutCL( pLine, pBuffer->item.asString.value, ulPos );
            }

            //TraceLog( NULL, "Line: %i >%s<\n", pLine->item.asString.length, pLine->item.asString.value );
         }
         else
         {
            //TraceLog( NULL, "EMPTY\n" );
            hb_storc( NULL, 2 );
         }

         hb_itemPutCRaw( pBuffer, hb_strdup( pBuffer->item.asString.value + ulPos + 1 ), pBuffer->item.asString.length - ( ulPos + 1 ) );
         //TraceLog( NULL, "Next: %i >%s<\n", pBuffer->item.asString.length, pBuffer->item.asString.value );
      }
      else
      {
         hb_storc( NULL, 1 );
         hb_retl( FALSE );
      }
   }

   HB_FUNC_STATIC( PROCESSTEXTFILE )
   {
      #define READSIZE 4096

      const char *sFile = hb_parcx( 1 );
      PHB_ITEM pSelf = hb_param( 2, HB_IT_OBJECT | HB_IT_BLOCK );
      PHB_ITEM pMethod = hb_param( 3, HB_IT_POINTER );
      PHB_SYMB pSym = (PHB_SYMB) hb_itemGetPtr( pMethod );

      FILE *hFile;
      HB_ITEM_NEW( Line );

      int iRead = 0, iLines = 0, iLineLen;
      char buffer[ READSIZE + 1 ];
      char *pNewLine, *pLastLine;

      //#define SHOW_REWIND

      #ifdef SHOW_REWIND
         int bShowRewind = 0;
      #endif

      hFile = fopen( sFile, "rb" );
      if( hFile == NULL )
      {
         return;
      }

      buffer[ READSIZE ] = '\0';

      while( ( iRead = fread( buffer, 1, READSIZE, hFile ) ) == READSIZE )
      {
         buffer[ iRead ] = 0;

         pNewLine = (char *) buffer;

         // Optimize and avoid GPF trap when the first char is '\n' (pNewLines[-1] is invalid)
         while( *pNewLine == '\n' )
         {
            hb_vmPushSymbol( pSym );
            hb_vmPush( pSelf );
            hb_vmPush( hb_itemPutCL( &Line, "", 0 ) );
            hb_vmSend( 1 );

            iLines++;
            pNewLine++;
         }

         pLastLine = pNewLine;

         while( ( pNewLine = strchr( pNewLine, '\n' ) ) != NULL )
         {
            if( *( pNewLine - 1 ) == '\r' )
            {
               *( pNewLine - 1 ) = '\0';
               iLineLen = ( pNewLine - 1 ) - pLastLine;
            }
            else
            {
               *pNewLine = '\0';
               iLineLen = pNewLine - pLastLine;
            }

            #ifdef SHOW_REWIND
            if( bShowRewind )
            {
               bShowRewind = 0;
               printf( "%i(%i)>%s\n", iLines, iLineLen, pLastLine );
            }
            #endif

            while( *pLastLine == '\r' )
            {
               pLastLine++;
               iLineLen--;
            }

            //printf( "%i> %s\n", iLines, pLastLine );
            hb_vmPushSymbol( pSym );
            hb_vmPush( pSelf );
            hb_vmPush( hb_itemPutCL( &Line, pLastLine, iLineLen ) );
            hb_vmSend( 1 );

            iLines++;
            pNewLine++; // The string terminator should provide safety.
            pLastLine = pNewLine;

            //printf( "Lines: %i\r", iLines, pNewLine );
         }

         // Rewind to pLastLine position!
         fseek( hFile, pLastLine - ( buffer + READSIZE ), SEEK_CUR );

         #ifdef SHOW_REWIND
            bShowRewind = 1;
         #endif
      }

      if( iRead > 0 && iRead < READSIZE )
      {
         buffer[ iRead ] = 0;

         pNewLine = (char *) buffer;

         // Optimize and avoid GPF trap when the first char is '\n' (pNewLines[-1] is invalid)
         while( *pNewLine == '\n' )
         {
            hb_vmPushSymbol( pSym );
            hb_vmPush( pSelf );
            hb_vmPush( hb_itemPutCL( &Line, "", 0 ) );
            hb_vmSend( 1 );

            iLines++;
            pNewLine++;
         }

         pLastLine = pNewLine;

         while( ( pNewLine = strchr( pNewLine, '\n' ) ) != NULL )
         {
            if( *( pNewLine - 1 ) == '\r' )
            {
               *( pNewLine - 1 ) = '\0';
               iLineLen = ( pNewLine - 1 ) - pLastLine;
            }
            else
            {
               *pNewLine = '\0';
               iLineLen = pNewLine - pLastLine;
            }

            while( *pLastLine == '\r' )
            {
               pLastLine++;
               iLineLen--;
            }

            //printf( "%i> %s\n", iLines, pLastLine );
            hb_vmPushSymbol( pSym );
            hb_vmPush( pSelf );
            hb_vmPush( hb_itemPutCL( &Line, pLastLine, iLineLen ) );
            hb_vmSend( 1 );

            iLines++;
            pNewLine++; // The string terminator should provide safety.
            pLastLine = pNewLine;

            //printf( "Lines: %i\r", iLines, pNewLine );
         }

         if( pLastLine && *pLastLine )
         {
            while( *pLastLine == '\r' )
            {
               pLastLine++;
            }

            iLineLen = strlen( pLastLine );

            //printf( "%i> %s\n", iLines, pLastLine );
            hb_vmPushSymbol( pSym );
            hb_vmPush( pSelf );
            hb_vmPush( hb_itemPutCL( &Line, pLastLine, iLineLen ) );
            hb_vmSend( 1 );

            iLines++;
         }
      }

      hb_itemClear( &Line );

      //printf( "Lines: %i\n", iLines, pNewLine );
      fclose( hFile );
   }

#pragma ENDDUMP

#ifdef WIN

  #pragma BEGINDUMP

    #include <math.h>

    #include <windows.h>

    #include "hbapigt.h"

    typedef struct tagStretchInfo
    {
       float fXFactor;
       float fYFactor;
       const char  *sDefaultFont;
       HFONT hFont;
       BOOL  bPaint;
    } STRETCHINFO, *PSTRETCHINFO ;

    BOOL CALLBACK EnumChildProc( HWND hChild, LPARAM lParam )
    {
       //char sMessage[ 256 ];
       PSTRETCHINFO pStretchInfo = (PSTRETCHINFO) lParam;
       char sFont[ 64 ];
       HDC hDC = GetDC( hChild );
       TEXTMETRIC tm;
       HFONT hFont;
       RECT rct;
       POINT TopLeft;
       HANDLE hParent = GetParent( hChild );

       if( hParent )
       {
          char sClass[256];
          GetClassName( (HWND) hParent, sClass, 256 );

          if( strcmp( sClass, "ComboBox" ) == 0 )
          {
             return TRUE;
          }
          //TraceLog( NULL, "Class >%s<\n", sClass );
       }

       SelectObject( hDC, (HFONT) SendMessage( hChild, WM_GETFONT, 0, 0 ) );
       GetTextMetrics( hDC, &tm );
       GetTextFace( hDC, sizeof( sFont ), sFont );
       ReleaseDC( hChild, hDC );

       hFont = pStretchInfo->hFont;

       if( hFont == NULL && pStretchInfo->sDefaultFont == NULL )
       {
          hFont = CreateFont( (int) -floor( (float) tm.tmHeight * pStretchInfo->fYFactor ), // height of font
                               0,                   // average character width
                               0,                   // angle of escapement
                               0,                   // base-line orientation angle
                               tm.tmWeight,         // font weight
                               tm.tmItalic,         // italic attribute option
                               tm.tmUnderlined,     // underline attribute option
                               tm.tmStruckOut,      // strikeout attribute option
                               tm.tmCharSet,        // character set identifier
                               OUT_DEFAULT_PRECIS,  // output precision
                               CLIP_DEFAULT_PRECIS, // clipping precision
                               DEFAULT_QUALITY,     // output quality
                               tm.tmPitchAndFamily, // pitch and family
                               sFont                // typeface name
                            );

          //sprintf( sMessage, "Font: %p\n", hFont );
          //OutputDebugString( sMessage );
       }

       if( hFont == NULL && pStretchInfo->sDefaultFont )
       {
          hFont = CreateFont( (int) -floor( (float) tm.tmHeight * pStretchInfo->fYFactor ), // height of font
                               0,                   // average character width
                               0,                   // angle of escapement
                               0,                   // base-line orientation angle
                               tm.tmWeight,         // font weight
                               tm.tmItalic,         // italic attribute option
                               tm.tmUnderlined,     // underline attribute option
                               tm.tmStruckOut,      // strikeout attribute option
                               tm.tmCharSet,        // character set identifier
                               OUT_DEFAULT_PRECIS,  // output precision
                               CLIP_DEFAULT_PRECIS, // clipping precision
                               DEFAULT_QUALITY,     // output quality
                               FF_DONTCARE,         // pitch and family
                               pStretchInfo->sDefaultFont // typeface name
                            );

          //sprintf( sMessage, "*** Font: %p\n", hFont );
          //OutputDebugString( sMessage );
       }

       SendMessage( hChild, WM_SETFONT, (WPARAM) hFont, (LPARAM) 0 );

       GetWindowRect( hChild, &rct );

       TopLeft.x = rct.left;
       TopLeft.y = rct.top;

       ScreenToClient( GetParent( hChild ), &TopLeft );

       //sprintf( sMessage, "Old Pos: %i, %i New Pos: %i, %i\n", TopLeft.x, TopLeft.y, (int) floor( (float) TopLeft.x * pStretchInfo->fXFactor ), (int) floor( (float) TopLeft.y * pStretchInfo->fYFactor ) );
       //OutputDebugString( sMessage );

        // Size and position the child window.
       MoveWindow( hChild,
                   (int) floor( (float) TopLeft.x * pStretchInfo->fXFactor ),
                   (int) floor( (float) TopLeft.y * pStretchInfo->fYFactor ),
                   (int) floor( (float) ( rct.right - rct.left + 1 ) * pStretchInfo->fXFactor ),
                   (int) floor( (float) ( rct.bottom - rct.top + 1 ) * pStretchInfo->fYFactor ),
                   pStretchInfo->bPaint );

        return TRUE;
    }

    HFONT Stretch( HWND hWnd, int iNewWidth, int iNewHeight, int iOldWidth, int iOldHeight, BOOL bPaint, int iLeft, int iTop, const char *sDefaultFont, BOOL bMonoFont )
    {
       //char sMessage[ 256 ];
       STRETCHINFO StretchInfo;
       char sFont[ 64 ];
       HDC hDC = GetDC( hWnd );
       HFONT hFont = NULL;
       RECT rct;
       TEXTMETRIC tm;
       POINT TopLeft;

       if( hWnd == 0 )
       {
          MessageBox( 0, "Stretch() - Invalid hWnd argument!", "xEdit", MB_TASKMODAL | MB_ICONERROR );
          return NULL;
       }

       GetWindowRect( hWnd, &rct );

       if( iOldWidth == -1 )
       {
          iOldWidth = rct.right - rct.left + 1;
       }

       if( iOldHeight == -1 )
       {
          iOldHeight = rct.bottom - rct.top + 1;
       }

       if( iNewWidth == -1 )
       {
          if( iNewHeight == -1 )
          {
             MessageBox( 0, "Stretch() - Invalid new size argument!", "xEdit", MB_TASKMODAL | MB_ICONERROR );
          }
          else
          {
             StretchInfo.fYFactor = (float) iNewHeight / (float) iOldHeight;
             StretchInfo.fXFactor = StretchInfo.fYFactor;
          }
       }
       else
       {
          StretchInfo.fXFactor = (float) iNewWidth / (float) iOldWidth;
       }

       if( iNewHeight == -1 )
       {
          StretchInfo.fYFactor  = StretchInfo.fXFactor;
       }
       else
       {
          StretchInfo.fYFactor = (float) iNewHeight / (float) iOldHeight;
       }

       #ifdef FIXED_FACTOR
          if( StretchInfo.fYFactor > StretchInfo.fXFactor )
          {
             StretchInfo.fYFactor = StretchInfo.fXFactor;
          }
          else
          {
             StretchInfo.fXFactor = StretchInfo.fYFactor;
          }
       #endif

       if( iNewHeight == -1 )
       {
          iNewHeight = (int) floor ( StretchInfo.fYFactor * (float) iOldHeight );
       }

       if( iNewWidth == -1 )
       {
          iNewWidth = (int) floor ( StretchInfo.fXFactor * (float) iOldWidth );
       }


       SelectObject( hDC, (HFONT) SendMessage( hWnd, WM_GETFONT, 0, 0 ) );
       GetTextMetrics( hDC, &tm );
       GetTextFace( hDC, sizeof( sFont ), sFont );
       ReleaseDC( hWnd, hDC );

       if( sDefaultFont == NULL )
       {
          hFont = CreateFont( (int) -floor( (float) tm.tmHeight * StretchInfo.fYFactor ), // height of font
                               0,                   // average character width
                               0,                   // angle of escapement
                               0,                   // base-line orientation angle
                               tm.tmWeight,         // font weight
                               tm.tmItalic,         // italic attribute option
                               tm.tmUnderlined,     // underline attribute option
                               tm.tmStruckOut,      // strikeout attribute option
                               tm.tmCharSet,        // character set identifier
                               OUT_DEFAULT_PRECIS,  // output precision
                               CLIP_DEFAULT_PRECIS, // clipping precision
                               DEFAULT_QUALITY,     // output quality
                               tm.tmPitchAndFamily, // pitch and family
                               sFont                // typeface name
                            );
       }

       if( hFont == NULL && sDefaultFont )
       {
          hFont = CreateFont( (int) -floor( (float) tm.tmHeight * StretchInfo.fYFactor ), // height of font
                               0,                   // average character width
                               0,                   // angle of escapement
                               0,                   // base-line orientation angle
                               tm.tmWeight,         // font weight
                               tm.tmItalic,         // italic attribute option
                               tm.tmUnderlined,     // underline attribute option
                               tm.tmStruckOut,      // strikeout attribute option
                               tm.tmCharSet,        // character set identifier
                               OUT_DEFAULT_PRECIS,  // output precision
                               CLIP_DEFAULT_PRECIS, // clipping precision
                               DEFAULT_QUALITY,     // output quality
                               FF_DONTCARE,         // pitch and family
                               sDefaultFont         // typeface name
                            );
       }

       SendMessage( hWnd, WM_SETFONT, (WPARAM) hFont, (LPARAM) 0 );

       if( iLeft == -1 || iTop == -1 )
       {
          TopLeft.x = rct.left;
          TopLeft.y = rct.top;

          ScreenToClient( GetParent( hWnd ), &TopLeft );

          if( iLeft == -1 )
          {
             iLeft = TopLeft.x;
          }

          if( iTop == -1 )
          {
             iTop = TopLeft.y;
          }
       }

       //sprintf( sMessage, "Sizing, from: %i, %i, to: %i, %i Factor: %f, %f\n", iOldWidth, iOldHeight, iNewWidth, iNewHeight, StretchInfo.fXFactor, StretchInfo.fYFactor );
       //OutputDebugString( sMessage );

       MoveWindow( hWnd, iLeft, iTop, iNewWidth, iNewHeight, bPaint );

       StretchInfo.bPaint = bPaint;
       StretchInfo.sDefaultFont = sDefaultFont;

       if( bMonoFont )
       {
          StretchInfo.hFont = hFont;
       }
       else
       {
          StretchInfo.hFont = NULL;
       }

       EnumChildWindows( hWnd, EnumChildProc, (LPARAM) &StretchInfo );

       return hFont;
    }

    HB_FUNC_STATIC( STRETCH )
    {
       hb_retnl( (long) Stretch( (HWND) hb_parnl(1), hb_parni(2), hb_parni(3), hb_parni(4), hb_parni(5), hb_parl(6), hb_parni(7), hb_parni(8), hb_parc(9), hb_parl(10) ) );
    }

    HB_FUNC_STATIC( STRINGFROMPOINTER )
    {
       hb_retc( (char *) hb_parnl( 1 ) );
    }

    HB_FUNC_STATIC( COPYSTRINGTOPOINTER )
    {
       strncpy( (char *) hb_parnl(1), hb_parc(2), hb_parnl(3) );
    }

  #pragma ENDDUMP

#endif
