

// AJ Wos

// Note: This is a special purpose source code file.
// Because of the fact that the structure definition in xHarbour creates an INIT PROCEDURE
// for each defined structure, each structure must be defined once, and once only.
// You can group all structure definitions in one header CH file ( see winstruc.ch )
// and have them created automatically in this program. If you do that, do not include
// that file (eg.winstruc.ch) anywhere else, otheriwsethe structure definition will be
// corrupted if it is created twice in the same or even different sources.

// if you add new structure definitions in another file, make sure that the definitionsa
// are not duplicated, anywhere in all of your project sources.
// This is why I ave decided to place all structure definitions in one separate PRG !
// This one.

// Important:
// Please review the PPO file after every new structure added, and make sure that all
// parameter types and return values are resolved. If necessary add new types to WinTypes.ch.
// All the above applies to xBase syntax structure definitions as well.
// If a member is a structure, or a structure pointer, it must also be definied in a file.


#Include "windows.ch"
#Include "wintypes.ch"
#Include "cstruct.ch"
#Include "winstruc.ch"



