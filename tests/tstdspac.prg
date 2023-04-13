/*

 $Id$

 tstdspac

 This program demonstrates that the 4 diskspace related functions work
 correctly for disks of any size.

 Certain os's may allow limits to the amount of disk space available to
 a user.  If that is the case, you should see a difference between
 the return value of DiskSpace() and DiskFree().

 Currently, Disk quota's are only implimented for NT.

 NOTE: Unlike Clipper, these functions return a floating point number!

 Original sample written by Paul Tucker {ptucker@sympatico.ca>
 www - http://www.harbour-project.org
 
 2007-08-23 : Current sample written by Patrick Mast

 This test program placed in the public domain
*/

#include "fileio.ch"

#define HB_DISK_MB      1048576
#define HB_DISK_GB      1073741824

#define HB_DISK_DRIVE_C 3
#define HB_DISK_DRIVE_D 4

PROCEDURE Main()

LOCAL cDrive

? "   DiskSpace() | Total disk space on current drive ", Str(   DiskSpace()/HB_DISK_MB,10,2) + " Mb"
? "HB_DiskSpace() | Total disk space on current drive ", Str(HB_DiskSpace()/HB_DISK_MB,10,2) + " Mb"

WAIT


? "//-------------------------------------------------------------//"
TRY
 ? "With HB_DiskSpace() for drive C:"
 ? "Free disk space available: ", Str(HB_DiskSpace("C:",HB_DISK_AVAIL)/HB_DISK_MB,10,2) + " Mb"
 ? "    Total free disk space: ", Str(HB_DiskSpace("C:",HB_DISK_FREE )/HB_DISK_MB,10,2) + " Mb"
 ? "          Used disk space: ", Str(HB_DiskSpace("C:",HB_DISK_USED )/HB_DISK_MB,10,2) + " Mb"
 ? "         Total disk space: ", Str(HB_DiskSpace("C:",HB_DISK_TOTAL)/HB_DISK_GB,10,2) + " Gb"
CATCH
 ? "Drive C: not availabe or not ready"
END
?

TRY
 ? "With DiskSpace() for drive C:"
 ? "Free disk space available: ", Str(DiskSpace( HB_DISK_DRIVE_C ) / HB_DISK_MB,10,2) + " Mb"
CATCH
 ? "Drive C: not availabe or not ready"
END
?
? "//-------------------------------------------------------------//"
?

TRY
 ? "With HB_DiskSpace() for drive D:"
 ? "Free disk space available: ", Str(HB_DiskSpace("D:",HB_DISK_AVAIL)/HB_DISK_MB,10,2) + " Mb"
 ? "    Total free disk space: ", Str(HB_DiskSpace("D:",HB_DISK_FREE )/HB_DISK_MB,10,2) + " Mb"
 ? "          Used disk space: ", Str(HB_DiskSpace("D:",HB_DISK_USED )/HB_DISK_MB,10,2) + " Mb"
 ? "         Total disk space: ", Str(HB_DiskSpace("D:",HB_DISK_TOTAL)/HB_DISK_GB,10,2) + " Gb"
CATCH
 ? "Drive D: not availabe or not ready"
END
?

TRY
 ? "With DiskSpace() for drive D:"
 ? "Free disk space available: ", Str(DiskSpace( HB_DISK_DRIVE_D ) / HB_DISK_MB,10,2) + " Mb"
CATCH
 ? "Drive D: not availabe or not ready"
END
?
? "//-------------------------------------------------------------//"
?

WAIT

FOR f=1 TO 26

    cDrive := Chr(64+f)+":"
    
    TRY
     ? "Free disk space available on drive " + cDrive + ": " + ;
        Str(HB_DiskSpace(cDrive,HB_DISK_AVAIL)/HB_DISK_MB,10,2) + " Mb"
    CATCH
    END 

NEXT

WAIT

RETURN