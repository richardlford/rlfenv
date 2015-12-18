#
# diffother.ps1
#
[CmdletBinding()]
Param(
   [Parameter(Mandatory=$true)]
   [string]$reldir,
   [Parameter(Mandatory=$true)]
   [string]$item
)
$dir1 = "C:\ProjectK\src\QA\CLR\testsrc"
$dir2 = "d:\bm\x6\coreclr\tests\src"
$relpath = Join-Path $reldir $item
$path1 = Join-Path $dir1 $relpath
$path2 = Join-Path $dir2 $relpath
$scriptBlock = [ScriptBlock]::Create("windiff $path1 $path2")
Start-Job -ScriptBlock $scriptBlock
