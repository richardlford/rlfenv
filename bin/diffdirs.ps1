#
# diffdirs.ps1
#
[CmdletBinding()]
Param(
   [Parameter(Mandatory=$true)]
   [string]$reldir
)
$dir1 = "C:\ProjectK\src\QA\CLR\testsrc"
$dir2 = "d:\bm\x6\coreclr\tests\src"
$path1 = Join-Path $dir1 $reldir
# $path1 = Join-Path $path1 "Desktop"
$path2 = Join-Path $dir2 $reldir
$scriptBlock = [ScriptBlock]::Create("windiff $path1 $path2")
Start-Job -ScriptBlock $scriptBlock
