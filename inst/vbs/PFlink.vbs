Set oWS = WScript.CreateObject("WScript.Shell")
sLinkFile = oWS.SpecialFolders("Desktop") + "\PipeFish.LNK"
Set oLink = oWS.CreateShortcut(sLinkFile)
    oLink.TargetPath = WScript.Arguments.Item(0)
 '  oLink.Arguments = ""
   oLink.Description = "PipeFish data tool"   
   oLink.HotKey = "ALT+CTRL+P"
 '  oLink.IconLocation = "C:\Program Files\R\R-3.2.2\library\PipeFish\gui\PipeFish.exe, 2"
 '  oLink.WindowStyle = "1"   
 '  oLink.WorkingDirectory = ""C:\Program Files\R\R-3.2.2\library\PipeFish\gui\"
oLink.Save