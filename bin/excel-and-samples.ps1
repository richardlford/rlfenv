[IO.FileInfo]$fi = New-Object -TypeName "System.IO.FileInfo" `
    "c:\throughput\"
$exists = $fi.Exists

[double]$x=9
[double]$y=[Math]::Sqrt($x)
echo "y=$y"
Get-ChildItem -Recurse -Filter
Trace-Command


$objExcel = New-Object -comobject Excel.Application
$objExcel.Visible = $True
$objWorkbook = $objExcel.Workbooks.Add()
$objWorksheet = $objWorkbook.Worksheets.Item(1)
$objWorksheet.Cells.Item(1,1) = "A value in cell A1."
$objWorkbook.SaveAs("C:ScriptsTest2.xlsx")
$objExcel.Quit()
