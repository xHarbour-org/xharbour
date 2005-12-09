@rem ========================================================
@rem  This is used to import bgd.dll when it will use MSVC
@rem ========================================================
@echo Creating bgd.dll
lib /machine:x86 /def:bgd.def
@if exist bgd.lib ren bgd.lib libbgd.lib
@echo Done. You can now link to the bgd.lib import library from MSVC.

