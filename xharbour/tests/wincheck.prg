
PROCEDURE MAIN()
   LOCAL nResult:= 0, a, x
   CLS
   ? 'Checking Windows Version & Platform Details'
   ? '-------------------------------------------'
   ?
   ?? 'Platform is: '
   IF OS_ISWINNT()
      ?? "VER_PLATFORM_WIN32_NT. i.e. NT, 2000, XP or 2003 Server"
   ELSEIF OS_ISWIN9X()
      ?? "VER_PLATFORM_WIN32_WINDOWS. i.e. 95, 98 or ME"
   ELSE
      ?? "UNKNOWN?????????"
   ENDIF
   ?
   ? 'Release Version of Windows is: '
   DO CASE
   CASE OS_ISWIN95()
      ?? "95"
   CASE OS_ISWIN98()
      ?? "98"
   CASE OS_ISWINME()
      ?? "ME"
   CASE OS_ISWINNT351()
      ?? "NT3.51"
   CASE OS_ISWINNT4()
      ?? "NT4.00"
   CASE OS_ISWIN2000()
      ?? "2000"
   CASE OS_ISWINXP()
      ?? "XP"
   CASE OS_ISWIN2003()
      ?? "2003 Server"
   OTHERWISE
      ?? "UNKNOWN"
   ENDCASE

   ?
   ? 'Details of version'
   a:= os_versioninfo()
   IF !EMPTY( a )
      ? "   MajorVersion: ", a[ 1 ]
      ? "   MinorVersion: ", a[ 2 ]
      ? "    BuildNumber: ", a[ 3 ]
      ? "     PlatformId: ", a[ 4 ]
      ? "      ExtraData: ", a[ 5 ]
   ENDIF
   ?
   IF OS_ISWTSCLIENT()
      ? "Running as a Windows Terminal Server Client"
   ELSE
      ? "NO Windows Terminal Server Client detected"
   ENDIF

   ?
   ? 'Checking and setting network for Windows'
   ? '----------------------------------------'
   ?
   IF OS_ISWTSCLIENT() .AND. !OS_NETREGOK() // Note: If Windows Terminal Server client DONOT
                                            //       attempt to set registry.
     ? 'Registry on WTS server is not set correctly for networking.'
     ? 'Please check with System Administrator to set the registry correctly.'
   ELSEIF OS_NETREGOK( .T. )
     ? 'Registry set OK for networking'
   ELSE
     ? 'Failed to set registry - May need "Administrator" rights'
   ENDIF
   ?

   IF !OS_NETVREDIROK( @nResult )
      ? 'Invalid RVREDIR.VXD file installed'
      IF nResult = 950
         ? 'You need file VREDRUPD.EXE if your vredir.vxd is dated "09:50:00"'
      ELSEIF nResult == 1111
         ? 'You need file VRDRUPD.EXE if your vredir.vxd is dated "11:11:10"'
      ENDIF
   ENDIF

   WAIT

RETURN
