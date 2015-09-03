Dim objXLApp, objXLWb, objXLWs
Set objXLApp = CreateObject("Excel.Application")
Set objXLWb = objXLApp.Workbooks.Open(WScript.Arguments.Item(0))
objXLWb.Save
objXLWb.Close