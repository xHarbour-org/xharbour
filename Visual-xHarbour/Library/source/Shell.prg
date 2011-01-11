/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Shell.prg                                                                                            *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "commdlg.ch"
#include "debug.ch"
#include "vxh.ch"
#include "fileio.ch"
//-----------------------------------------------------------------------------------------------

CLASS CFile
   DATA Name             EXPORTED INIT ""
   DATA Path             EXPORTED INIT ""
   DATA ofn              EXPORTED
   DATA Filter           EXPORTED INIT {}
   DATA Result           EXPORTED INIT IDCANCEL
   DATA FileBuffer       EXPORTED
   DATA Handle           EXPORTED
   DATA Lines            EXPORTED INIT 0
   DATA AllowMultiSelect EXPORTED INIT .F.
   DATA Modified         EXPORTED
   DATA Flags            EXPORTED
   DATA FilterIndex      EXPORTED
   DATA DefaultExtention EXPORTED
   DATA FileExtension    EXPORTED

   ACCESS Exists   INLINE FILE( ::Path + "\" + ::Name )
   METHOD Init()  CONSTRUCTOR
   METHOD SaveDialog()
   METHOD OpenDialog()
   METHOD AddFilter( cDesc, cFilter ) INLINE AADD( ::Filter, { cDesc, cFilter } )
   METHOD Load()
   METHOD Save()
   METHOD Delete()    INLINE FERASE( ::Path + "\" + ::Name )
   METHOD Line(n)     INLINE MEMOLINE( ::FileBuffer, 254, n, , .F. )
   METHOD Close()
ENDCLASS

METHOD Init( cName ) CLASS CFile
   ::Name := cName
   ::Modified := .F.
RETURN Self

METHOD SaveDialog( oParent, cTitle ) CLASS CFile
   LOCAL cFile, n, cFilter := ""

   DEFAULT oParent TO GetActiveWindow()

   ::ofn                 := (struct OPENFILENAME)
   ::ofn:hInstance       := __GetApplication():Instance
   ::ofn:nMaxFile        := MAX_PATH + 1
   ::ofn:hwndOwner       := IIF( VALTYPE( oParent ) == "O", oParent:hWnd, oParent )
   ::ofn:lStructSize     := 76
   ::ofn:lpstrInitialDir := ::Path
   ::ofn:lpstrFile       := PadR( ::Name, MAX_PATH )
   ::ofn:lpstrDefExt     := ::DefaultExtention
   ::ofn:lpstrTitle      := cTitle

   IF Empty( ::Flags )
      ::ofn:Flags := OFN_EXPLORER
   ELSE
      ::ofn:Flags := ::Flags
   ENDIF

   FOR n := 1 TO LEN( ::Filter )
       cFilter += ::Filter[n][1] + chr( 0 ) + ::Filter[n][2] + chr( 0 )
   NEXT
   ::ofn:lpstrFilter := cFilter

   IF GetSaveFileName( @::ofn )// + "."+ ::Filter[::oFn:nFileExtension + 1][2]
      ::Result := IDOK
      ::FilterIndex   := ::ofn:nFilterIndex
      ::FileExtension := ::ofn:nFileExtension

      cFile := Left( ::ofn:lpstrFile, At( Chr(0), ::ofn:lpstrFile ) - 1 )

      IF ( n := RAT( "\", cFile ) ) == 0
         ::Path   := ""
         ::Name   := cFile
      ELSE
         ::Path   := LEFT( cFile, n-1 )
         ::Name   := SUBSTR( cFile, n+1, LEN( cFile )-n )
      ENDIF
   ELSE
      ::Result := IDCANCEL
   ENDIF

RETURN Self

METHOD OpenDialog( oParent ) CLASS CFile

   LOCAL aFiles, cFile, n, x, cFilter := ""

   DEFAULT oParent TO GetActiveWindow()

   ::ofn                 := (struct OPENFILENAME)
   ::ofn:lStructSize     := 76
   ::ofn:hInstance       := __GetApplication():Instance
   ::ofn:nMaxFile        := 1024 + 1
   ::ofn:hwndOwner       := IIF( VALTYPE( oParent ) == "O", oParent:hWnd, oParent )
   ::ofn:lpstrInitialDir := ::Path
   ::ofn:lpstrFile       := PadR( ::Name, 1024 )

   IF Empty( ::Flags )
      ::ofn:Flags := OFN_EXPLORER
   ELSE
      ::ofn:Flags := ::Flags
   ENDIF

   IF ::AllowMultiSelect
      ::ofn:Flags := ::ofn:Flags | OFN_ALLOWMULTISELECT
   ENDIF

   FOR n := 1 TO LEN( ::Filter )
       cFilter += ::Filter[n][1] + chr( 0 ) + ::Filter[n][2] + chr( 0 )
   NEXT
   ::ofn:lpstrFilter := cFilter

   IF GetOpenFileName( @::ofn )
      ::Result := IDOK
      IF ::AllowMultiSelect
         ::Name := hb_ATokens( ::ofn:lpstrFile, CHR(0) )
         ::Path := ::Name[1]
         IF ::Path[-1] == "\"
            ::Path := SUBSTR( ::Path, 1, LEN( ::Path )-1 )
         ENDIF
         ADEL( ::Name, 1, .T. )
         ADEL( ::Name, LEN(::Name), .T. )
         ADEL( ::Name, LEN(::Name), .T. )

         IF LEN( ::Name ) == 0
            IF ( n := RAT( "\", ::Path ) ) == 0
               ::Name   := ::Path
               ::Path   := ""
             ELSE
               ::Name   := { SUBSTR( ::Path, n+1 ) }
               ::Path   := SUBSTR( ::Path, 1, n-1 )
            ENDIF
         ENDIF

       ELSE
         cFile := Left( ::ofn:lpstrFile, At( Chr(0), ::ofn:lpstrFile ) - 1 )
         IF ( n := RAT( "\", cFile ) ) == 0
            ::Path   := ""
            ::Name   := cFile
         ELSE
            ::Path   := LEFT( cFile, n-1 )
            ::Name   := SUBSTR( cFile, n+1, LEN( cFile )-n )
         ENDIF
      ENDIF
   ELSE
      ::Result := IDCANCEL
   ENDIF

RETURN Self

METHOD Close() CLASS CFile
   IF ::Modified
      IF MessageBox( 0, ::Name+" has been modified, Do you want to save the changes ?", "File Save", MB_ICONQUESTION | MB_YESNO ) == IDYES
         ::Save()
      ENDIF
   ENDIF
RETURN Self

METHOD Save( cPath ) CLASS CFile
   LOCAL hFile, cFile
   DEFAULT cPath TO ::Path
   
   IF ( hFile := fCreate( cPath + IIF( !Empty( cPath ), "\", "" ) + ::Name ) ) <> -1
      fWrite( hFile, ::FileBuffer, Len( ::FileBuffer ) )
      fClose( hFile )
   ENDIF
//   MEMOWRIT( cPath + "\" + ::Name, ::FileBuffer )
RETURN .T.

METHOD Load() CLASS CFile
   LOCAL cFile := ::Name
   IF !EMPTY( ::Path )
      cFile := ::Path + "\" + ::Name
   ENDIF
   ::FileBuffer := MEMOREAD( cFile )
   ::Lines := MLCOUNT( ::FileBuffer )
RETURN Self


FUNCTION SplitFile( cFile )
   LOCAL n := RAT( "\", cFile )
RETURN { SUBSTR( cFile, 1, n-1 ), SUBSTR( cFile, n+1 ) }
