Set oWS = WScript.CreateObject("WScript.Shell")
sLinkFile = oWS.SpecialFolders("Desktop") + "\\"  + WScript.Arguments.Item(0)    + ".LNK"
Set oLink = oWS.CreateShortcut(sLinkFile)
oLink.TargetPath = "Rscript.exe"
script = "source(file.path(paste0(.libPaths()[1]," + chr(39) + "/PipeFish/scripts/" + WScript.Arguments.Item(1) + chr(39)+ ")))"
oLink.Arguments = " -e "+ chr(34) + script + chr(34)
oLink.Save
