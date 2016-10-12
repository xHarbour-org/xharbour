Set objShell = CreateObject("Wscript.Shell")
strPath = "explorer.exe /e," & Wscript.Arguments(0)
objShell.Run strPath