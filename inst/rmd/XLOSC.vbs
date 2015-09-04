dim fso,Dir,fls

	Set WshShell = WScript.CreateObject("WScript.Shell")
    set fso= Wscript.CreateObject("Scripting.FileSystemObject")
    set Dir = fso.GetFolder(WScript.Arguments.Item(0))
    set fls = Dir.Files

    Dim XL
    Set XL = CreateObject("excel.application") 

    For each x in fls
        if (x.type = "Microsoft Office Excel 97-2003 Worksheet" Or x.type = "Microsoft Excel Worksheet") then
            Dim XLWkbk
            Set XLWkbk = XL.Workbooks.Open(x)
            XLWkbk.Save
            XLWkbk.Close
            Set XLWkbk = Nothing
        end if
    Next
    XL.Quit
    Set XL = Nothing
